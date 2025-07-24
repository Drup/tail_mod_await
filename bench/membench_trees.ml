open Bench_tree_map_common
open Picos

module Memmeasure = struct

  type t = {
    peak_words : int;
    current_stack: int;
    peak_main_stack: int;
    suspended_stacks : int;
    peak_stacks : int;
  }

  external _caml_used_stack_size: ('a,'b) continuation -> int  =
    "caml_used_stack_size" [@@noalloc]

  external _caml_total_stack_size: ('a,'b) continuation -> int  =
    "caml_total_stack_size" [@@noalloc]
      
  let suspended_stack_size k = _caml_used_stack_size k

  type _ Effect.t += Stack_size: int Effect.t
  (* let run_with_stack_observer f = match f () with *)
  (*   | x -> x *)
  (*   | effect Stack_size, k -> *)
  (*     let n = suspended_stack_size k in *)
  (*     Effect.Deep.continue k n *)

  let empty_measure = {
    peak_words = 0;
    current_stack = 0;
    peak_main_stack = 0;
    suspended_stacks = 0;
    peak_stacks = 0;
  }
  
  let m = ref empty_measure
      
  let reset_monitor () = m := empty_measure
  
  let update_memory () =
    let stat = Gc.quick_stat () in
    m := {!m with
          peak_words = max !m.peak_words stat.heap_words;
         }
  
  let update_current_stack () =
    let current_stack = Effect.perform Stack_size in
    let peak_main_stack = max !m.peak_main_stack current_stack in
    let peak_stacks = max !m.peak_stacks (current_stack + !m.suspended_stacks) in
    m := {!m with
          current_stack = current_stack;
          peak_main_stack = peak_main_stack;
          peak_stacks = peak_stacks;
         }
    
  let suspend_stack k =
    let current_stack = 0 in
    let suspended_stacks = !m.suspended_stacks + _caml_used_stack_size k in
    let peak_stacks = max !m.peak_stacks suspended_stacks in
    m := {!m with
          current_stack = current_stack;
          suspended_stacks = suspended_stacks;
          peak_stacks = peak_stacks;
         }

  let resume_stack k =
    let l = _caml_used_stack_size k in
    let current_stack = l in
    let peak_main_stack = max !m.peak_main_stack current_stack in
    let suspended_stacks = !m.suspended_stacks - l in
    let peak_stacks = max !m.peak_stacks (current_stack + suspended_stacks) in
    m := {!m with
          current_stack = current_stack;
          peak_main_stack = peak_main_stack;
          suspended_stacks = suspended_stacks;
          peak_stacks = peak_stacks;
         }

  let discontinue_stack k =
    let l = _caml_used_stack_size k in
    let suspended_stacks = !m.suspended_stacks - l in
    let peak_stacks = max !m.peak_stacks (!m.current_stack + suspended_stacks) in
    m := {!m with
          suspended_stacks = suspended_stacks;
          peak_stacks = peak_stacks;
         }
  

  (* Forked from Picos_mux_fifo. *)
  module Scheduler = struct
    open Picos

    module Select = struct
      let cancel_after _ ~seconds:_ _ _ =
        raise (Sys_error "Computation: cancel_after unavailable; install picos_io")

      let check_configured = Fun.id

    end

    let[@inline never] quota_non_positive _ = invalid_arg "quota must be positive"

    type ready =
      | Spawn of Fiber.t * (Fiber.t -> unit)
      | Continue of Fiber.t * (unit, unit) Effect.Deep.continuation
      | Resume of
          Fiber.t
          * ((exn * Printexc.raw_backtrace) option, unit) Effect.Deep.continuation
      | Return of Fiber.t * (unit, unit) Effect.Deep.continuation

    module Mpscq = Picos_aux_mpscq

    type t = {
      ready : ready Mpscq.t;
      needs_wakeup : bool Atomic.t;
      mutex : Mutex.t;
      condition : Condition.t;
      mutable resume :
        Trigger.t ->
        Fiber.t ->
        ((exn * Printexc.raw_backtrace) option, unit) Effect.Deep.continuation ->
        unit;
      mutable current : ((Fiber.t, unit) Effect.Deep.continuation -> unit) option;
      mutable yield : ((unit, unit) Effect.Deep.continuation -> unit) option;
      mutable return : ((unit, unit) Effect.Deep.continuation -> unit) option;
      mutable discontinue : ((unit, unit) Effect.Deep.continuation -> unit) option;
      mutable handler : (unit, unit) Effect.Deep.handler;
      quota : int;
      mutable fiber : Fiber.Maybe.t;
      mutable remaining_quota : int;
      mutable num_alive_fibers : int;
    }

    let rec next t =
      match Mpscq.pop_exn t.ready with
      | ready -> begin
          t.remaining_quota <- t.quota;
          let fiber =
            match ready with
            | Spawn (fiber, _)
            | Continue (fiber, _)
            | Resume (fiber, _)
            | Return (fiber, _) ->
              fiber
          in
          t.fiber <- Fiber.Maybe.of_fiber fiber;
          match ready with
          | Spawn (_, main) -> Effect.Deep.match_with main fiber t.handler
          | Return (_, k) ->
            resume_stack k;
            Effect.Deep.continue k ()
          | Continue (_, k) ->
            resume_stack k;
            Fiber.continue fiber k ()
          | Resume (_, k) ->
            resume_stack k;
            Fiber.resume fiber k
        end
      | exception Mpscq.Empty ->
        t.fiber <- Fiber.Maybe.nothing;
        if t.num_alive_fibers <> 0 then begin
          if Atomic.get t.needs_wakeup then begin
            Mutex.lock t.mutex;
            match
              if Atomic.get t.needs_wakeup then Condition.wait t.condition t.mutex
            with
            | () -> Mutex.unlock t.mutex
            | exception exn ->
              Mutex.unlock t.mutex;
              raise exn
          end
          else Atomic.set t.needs_wakeup true;
          next t
        end

    let run_fiber ?quota ?fatal_exn_handler fiber main =
      Select.check_configured ();
      let t =
        let quota =
          match quota with
          | None -> Int.max_int
          | Some quota -> if quota <= 0 then quota_non_positive quota else quota
        in
        {
          ready = Mpscq.create ~padded:true ();
          needs_wakeup = Atomic.make false |> Multicore_magic.copy_as_padded;
          mutex = Mutex.create ();
          condition = Condition.create ();
          resume = Obj.magic ();
          current = None;
          yield = None;
          return = None;
          discontinue = None;
          handler = Obj.magic ();
          quota;
          fiber = Fiber.Maybe.of_fiber fiber;
          remaining_quota = quota;
          num_alive_fibers = 1;
        }
      in
      t.handler <-
        {
          exnc = (match fatal_exn_handler with None -> raise | Some exnc -> exnc);
          effc =
            (fun (type a) (e : a Effect.t) ->
               match e with
               | Fiber.Current ->
                 (t.current : ((a, _) Effect.Deep.continuation -> _) option)
               | Fiber.Spawn r ->
                 let fiber = Fiber.Maybe.to_fiber t.fiber in
                 if Fiber.is_canceled fiber then t.discontinue
                 else begin
                   t.num_alive_fibers <- t.num_alive_fibers + 1;
                   Mpscq.push t.ready (Spawn (r.fiber, r.main));
                   t.return
                 end
               | Fiber.Yield -> t.yield
               | Computation.Cancel_after r -> begin
                   let fiber = Fiber.Maybe.to_fiber t.fiber in
                   if Fiber.is_canceled fiber then t.discontinue
                   else
                     match
                       Select.cancel_after r.computation ~seconds:r.seconds r.exn
                         r.bt
                     with
                     | () -> t.return
                     | exception exn ->
                       let bt = Printexc.get_raw_backtrace () in
                       Some
                         (fun k ->
                            discontinue_stack k;
                            Effect.Deep.discontinue_with_backtrace k exn bt)
                 end
               | Trigger.Await trigger ->
                 Some
                   (fun k ->
                      suspend_stack k;
                      let fiber = Fiber.Maybe.to_fiber t.fiber in
                      if Fiber.try_suspend fiber trigger fiber k t.resume then
                        next t
                      else
                        let remaining_quota = t.remaining_quota - 1 in
                        if 0 < remaining_quota then begin
                          t.remaining_quota <- remaining_quota;
                          resume_stack k;
                          Fiber.resume fiber k
                        end
                        else begin
                          Mpscq.push t.ready (Resume (fiber, k));
                          next t
                        end)
                   
               | Stack_size ->
                 Some
                   (fun k ->
                      let n = suspended_stack_size k in
                      Effect.Deep.continue k n
                   )
               | _ -> None);
          retc =
            (fun () ->
               t.num_alive_fibers <- t.num_alive_fibers - 1;
               next t);
        };
      t.resume <-
        (fun trigger fiber k ->
           let resume = Resume (fiber, k) in
           if Fiber.unsuspend fiber trigger then Mpscq.push t.ready resume
           else Mpscq.push_head t.ready resume;
           if
             Atomic.get t.needs_wakeup
             && Atomic.compare_and_set t.needs_wakeup true false
           then begin
             begin
               match Mutex.lock t.mutex with
               | () -> Mutex.unlock t.mutex
               | exception Sys_error _ -> ()
             end;
             Condition.broadcast t.condition
           end);
      t.current <-
        Some
          (fun k ->
             let fiber = Fiber.Maybe.to_fiber t.fiber in
             Effect.Deep.continue k fiber);
      t.yield <-
        Some
          (fun k ->
             suspend_stack k;
             let fiber = Fiber.Maybe.to_fiber t.fiber in
             Mpscq.push t.ready (Continue (fiber, k));
             next t);
      t.return <-
        Some
          (fun k ->
             suspend_stack k;
             let remaining_quota = t.remaining_quota - 1 in
             if 0 < remaining_quota then begin
               t.remaining_quota <- remaining_quota;
               resume_stack k;
               Effect.Deep.continue k ()
             end
             else begin
               Mpscq.push t.ready (Return (Fiber.Maybe.to_fiber t.fiber, k));
               next t
             end);
      t.discontinue <-
        Some
          (fun k ->
             let fiber = Fiber.Maybe.to_fiber t.fiber in
             Fiber.continue fiber k ());
      Effect.Deep.match_with main fiber t.handler

    let[@inline never] run ?quota ?fatal_exn_handler fiber main computation =
      run_fiber ?quota ?fatal_exn_handler fiber main;
      Computation.peek_exn computation

    let run ?quota ?fatal_exn_handler ?forbid main =
      let forbid = match forbid with None -> false | Some forbid -> forbid in
      let computation = Computation.create ~mode:`LIFO () in
      let fiber = Fiber.create ~forbid computation in
      let main _ = Computation.capture computation main () in
      run ?quota ?fatal_exn_handler fiber main computation

  end

  
  let run title f =
    let _alarm = Gc.create_alarm update_memory in
    let total_allocated () =
      let s0 = Gc.quick_stat () in
      s0.minor_words +. s0.major_words -. s0.promoted_words
    in
    reset_monitor ();
    Format.printf "Running %s@.%!" title;
    Gc.full_major ();
    let init_allocated = total_allocated () in
    let x = Scheduler.run f in
    Gc.full_major ();
    Gc.delete_alarm _alarm;
    Format.printf "Peak Memory: %iKB@.%!" (!m.peak_words/(1024));
    Format.printf "Peak Main Stack: %i@.%!" (!m.peak_main_stack);
    Format.printf "Peak Stack: %i@.%!" (!m.peak_stacks);
    Format.printf "Peak Total: %iMB@.%!" ((!m.peak_stacks + !m.peak_words)/(1024 * 1024));
    Format.printf "Allocated Memory: %fKB@.%!"
      ((total_allocated () -. init_allocated) /. (1024.));
    x

end
  
let main () =
  let f x =
    if Random.int 100 <= 2 then Memmeasure.update_current_stack ();
    let r = ref 0 in
    for _ = 0 to int_of_float (x *. 100000.) do incr r done;
    !r
  in
  let size = 100000 in
  let t = gen_tree size in
  Format.printf "Size: %i, Height: %i@.%!"
    (cardinal t) (height t);
  Format.printf "Data size: %iMB@.%!"
    (Obj.reachable_words (Obj.repr t) / (1024 * 1024));
  let _ =
    Memmeasure.run "map" @@ fun () ->
    map f t
  in
  let _ =
    Memmeasure.run "map_tmc" @@ fun () ->
    map_tmc f t
  in
  let _ =
    Memmeasure.run "map_async" @@ fun () ->
    Computation.await @@ map_async f t
  in
  let _ =
    Memmeasure.run "map_async_tmca" @@ fun () ->
    Computation.await @@ map_async_tmca f t
  in
  let _ =
    Memmeasure.run "map_asyncpar_width" @@ fun () ->
    Computation.await @@ map_asyncpar_width f t
  in
  let _ =
    Memmeasure.run "map_asyncpar_width_tmca" @@ fun () ->
    Computation.await @@ map_asyncpar_width_tmca f t
  in
  let _ =
    Memmeasure.run "map_asyncpar_full" @@ fun () ->
    Computation.await @@ map_asyncpar_full f t
  in
  let _ =
    Memmeasure.run "map_asyncpar_full_tmca" @@ fun () ->
    Computation.await @@ map_asyncpar_full_tmca f t
  in
  ()

let () = main ()
