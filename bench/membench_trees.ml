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
    module Htbl = Picos_aux_htbl

    module Collection = struct
      type 'a t = (int, 'a) Htbl.t

      module Key = struct
        type t = int

        let equal = Int.equal
        let hash = Fun.id
      end

      let create () = Htbl.create ~hashed_type:(module Key) ()

      let rec push t value =
        let key = Random.bits () in
        if not (Htbl.try_add t key value) then push t value

      let rec pop_exn t =
        let key = Htbl.find_random_exn t in
        try Htbl.remove_exn t key with Not_found -> pop_exn t

      let is_empty t =
        match Htbl.find_random_exn t with _ -> false | exception Not_found -> true
    end

    type ready =
      | Spawn of Fiber.t * (Fiber.t -> unit)
      | Current of Fiber.t * (Fiber.t, unit) Effect.Deep.continuation
      | Continue of Fiber.t * (unit, unit) Effect.Deep.continuation
      | Resume of
          Fiber.t
          * ((exn * Printexc.raw_backtrace) option, unit) Effect.Deep.continuation
      | Raise of
          Fiber.t
          * (unit, unit) Effect.Deep.continuation
          * exn
          * Printexc.raw_backtrace
      | Return of Fiber.t * (unit, unit) Effect.Deep.continuation

    type t = {
      ready : ready Collection.t;
      mutable num_waiters_non_zero : bool;
      num_alive_fibers : int Atomic.t;
      mutable resume :
        Trigger.t ->
        Fiber.t ->
        ((exn * Printexc.raw_backtrace) option, unit) Effect.Deep.continuation ->
        unit;
      num_waiters : int ref;
      condition : Condition.t;
      mutex : Mutex.t;
      mutable current : ((Fiber.t, unit) Effect.Deep.continuation -> unit) option;
      mutable yield : ((unit, unit) Effect.Deep.continuation -> unit) option;
      mutable return : ((unit, unit) Effect.Deep.continuation -> unit) option;
      mutable handler : (unit, unit) Effect.Deep.handler;
      mutable run : bool;
    }

    let fiber_key : Fiber.Maybe.t ref Picos_thread.TLS.t =
      Picos_thread.TLS.create ()

    let get () =
      match Picos_thread.TLS.get_exn fiber_key with
      | p -> p
      | exception Picos_thread.TLS.Not_set ->
        let p = ref Fiber.Maybe.nothing in
        Picos_thread.TLS.set fiber_key p;
        p

    let[@inline] relaxed_wakeup t ~known_not_empty =
      if
        t.num_waiters_non_zero
        && (known_not_empty || not (Collection.is_empty t.ready))
      then begin
        Mutex.lock t.mutex;
        Mutex.unlock t.mutex;
        Condition.signal t.condition
      end

    let exec p t ready =
      begin
        p :=
          match ready with
          | Spawn (fiber, _)
          | Raise (fiber, _, _, _)
          | Return (fiber, _)
          | Current (fiber, _)
          | Continue (fiber, _)
          | Resume (fiber, _) ->
            Fiber.Maybe.of_fiber fiber
      end;
      match ready with
      | Spawn (fiber, main) -> Effect.Deep.match_with main fiber t.handler
      | Raise (_, k, exn, bt) -> discontinue_stack k; Effect.Deep.discontinue_with_backtrace k exn bt
      | Return (_, k) -> resume_stack k; Effect.Deep.continue k ()
      | Current (fiber, k) -> resume_stack k; Effect.Deep.continue k fiber
      | Continue (fiber, k) -> resume_stack k; Fiber.continue fiber k ()
      | Resume (fiber, k) -> resume_stack k; Fiber.resume fiber k

    let rec next p t =
      match Collection.pop_exn t.ready with
      | ready ->
        relaxed_wakeup t ~known_not_empty:false;
        exec p t ready
      | exception Not_found ->
        p := Fiber.Maybe.nothing;
        if Atomic.get t.num_alive_fibers <> 0 then begin
          Mutex.lock t.mutex;
          let n = !(t.num_waiters) + 1 in
          t.num_waiters := n;
          if n = 1 then t.num_waiters_non_zero <- true;
          if Collection.is_empty t.ready && Atomic.get t.num_alive_fibers <> 0
          then begin
            match Condition.wait t.condition t.mutex with
            | () ->
              let n = !(t.num_waiters) - 1 in
              t.num_waiters := n;
              if n = 0 then t.num_waiters_non_zero <- false;
              Mutex.unlock t.mutex;
              next p t
            | exception async_exn ->
              let n = !(t.num_waiters) - 1 in
              t.num_waiters := n;
              if n = 0 then t.num_waiters_non_zero <- false;
              Mutex.unlock t.mutex;
              raise async_exn
          end
          else begin
            let n = !(t.num_waiters) - 1 in
            t.num_waiters := n;
            if n = 0 then t.num_waiters_non_zero <- false;
            Mutex.unlock t.mutex;
            next p t
          end
        end
        else begin
          Mutex.lock t.mutex;
          Mutex.unlock t.mutex;
          Condition.broadcast t.condition
        end

    let default_fatal_exn_handler exn =
      prerr_string "Fatal error: exception ";
      prerr_string (Printexc.to_string exn);
      prerr_char '\n';
      Printexc.print_backtrace stderr;
      flush stderr;
      exit 2

    let context ?fatal_exn_handler () =
      Select.check_configured ();
      let exnc =
        match fatal_exn_handler with
        | None -> default_fatal_exn_handler
        | Some handler ->
          fun exn ->
            handler exn;
            raise exn
      in
      let t =
        {
          ready = Collection.create ();
          num_waiters_non_zero = false;
          num_alive_fibers = Atomic.make 1 |> Multicore_magic.copy_as_padded;
          resume = Obj.magic ();
          num_waiters = ref 0 |> Multicore_magic.copy_as_padded;
          condition = Condition.create ();
          mutex = Mutex.create ();
          current = None;
          yield = None;
          return = None;
          handler = Obj.magic ();
          run = false;
        }
      in
      t.resume <-
        (fun trigger fiber k ->
           suspend_stack k;
           let resume = Resume (fiber, k) in
           Fiber.unsuspend fiber trigger |> ignore;
           Collection.push t.ready resume;
           let non_zero =
             match Mutex.lock t.mutex with
             | () ->
               let non_zero = t.num_waiters_non_zero in
               Mutex.unlock t.mutex;
               non_zero
             | exception Sys_error _ -> false
           in
           if non_zero then Condition.signal t.condition);
      t.current <-
        Some
          (fun k ->
             suspend_stack k;
             let p = Picos_thread.TLS.get_exn fiber_key in
             let fiber = Fiber.Maybe.to_fiber !p in
             Collection.push t.ready (Current (fiber, k));
             next p t);
      t.yield <-
        Some
          (fun k ->
             suspend_stack k;
             let p = Picos_thread.TLS.get_exn fiber_key in
             let fiber = Fiber.Maybe.to_fiber !p in
             Collection.push t.ready (Continue (fiber, k));
             next p t);
      t.return <-
        Some
          (fun k ->
             suspend_stack k;
             let p = Picos_thread.TLS.get_exn fiber_key in
             let fiber = Fiber.Maybe.to_fiber !p in
             Collection.push t.ready (Return (fiber, k));
             next p t);
      t.handler <-
        {
          retc =
            (fun () ->
               Atomic.decr t.num_alive_fibers;
               let p = Picos_thread.TLS.get_exn fiber_key in
               next p t);
          exnc;
          effc =
            (fun (type a) (e : a Effect.t) ->
               match e with
               | Fiber.Current ->
                 (t.current : ((a, _) Effect.Deep.continuation -> _) option)
               | Fiber.Spawn r ->
                 let p = Picos_thread.TLS.get_exn fiber_key in
                 let fiber = Fiber.Maybe.to_fiber !p in
                 if Fiber.is_canceled fiber then t.yield
                 else begin
                   Atomic.incr t.num_alive_fibers;
                   Collection.push t.ready (Spawn (r.fiber, r.main));
                   relaxed_wakeup t ~known_not_empty:true;
                   t.return
                 end
               | Fiber.Yield -> t.yield
               | Computation.Cancel_after r -> begin
                   let p = Picos_thread.TLS.get_exn fiber_key in
                   let fiber = Fiber.Maybe.to_fiber !p in
                   if Fiber.is_canceled fiber then t.yield
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
                            Collection.push t.ready (Raise (fiber, k, exn, bt));
                            next p t)
                 end
               | Trigger.Await trigger ->
                 Some
                   (fun k ->
                      suspend_stack k;
                      let p = Picos_thread.TLS.get_exn fiber_key in
                      let fiber = Fiber.Maybe.to_fiber !p in
                      if Fiber.try_suspend fiber trigger fiber k t.resume then
                        next p t
                      else begin
                        Collection.push t.ready (Resume (fiber, k));
                        next p t
                      end)
               | Stack_size ->
                 Some
                   (fun k ->
                      let n = suspended_stack_size k in
                      Effect.Deep.continue k n
                   )
               | _ -> None);
        };
      t

    let runner_on_this_thread t =
      Select.check_configured ();
      next (get ()) t

    let rec await t =
      if t.num_waiters_non_zero then begin
        match Condition.wait t.condition t.mutex with
        | () -> await t
        | exception async_exn ->
          Mutex.unlock t.mutex;
          raise async_exn
      end
      else Mutex.unlock t.mutex

    let run_fiber ?context:t_opt fiber main =
      let t =
        match t_opt with
        | Some t ->
          Select.check_configured ();
          t
        | None -> context ()
      in
      Mutex.lock t.mutex;
      if t.run then begin
        Mutex.unlock t.mutex;
        invalid_arg "already run"
      end
      else begin
        t.run <- true;
        Mutex.unlock t.mutex;
        get () := Fiber.Maybe.of_fiber fiber;
        Effect.Deep.match_with main fiber t.handler;
        Mutex.lock t.mutex;
        await t
      end

    let[@inline never] run ?context fiber main computation =
      run_fiber ?context fiber main;
      Computation.peek_exn computation

    let run ?context ?forbid main =
      let forbid = match forbid with None -> false | Some forbid -> forbid in
      let computation = Computation.create ~mode:`LIFO () in
      let fiber = Fiber.create ~forbid computation in
      let main _ = Computation.capture computation main () in
      run ?context fiber main computation
        
    let rec run_fiber_on n fiber main runner_main context =
      if n <= 1 then run_fiber ~context fiber main
      else
        let runner =
          try Domain.spawn runner_main
          with exn ->
            let bt = Printexc.get_raw_backtrace () in
            run ~context Fun.id;
            Printexc.raise_with_backtrace exn bt
        in
        match run_fiber_on (n - 1) fiber main runner_main context with
        | result ->
          begin
            match Domain.join runner with
            | None -> ()
            | Some (exn, bt) -> Printexc.raise_with_backtrace exn bt
          end;
          result
        | exception exn ->
          let bt = Printexc.get_raw_backtrace () in
          begin
            match Domain.join runner with
            | None -> ()
            | Some (exn, bt) -> Printexc.raise_with_backtrace exn bt
          end;
          Printexc.raise_with_backtrace exn bt

    let run_fiber_on ?fatal_exn_handler ~n_domains fiber main =
      if n_domains < 1 then invalid_arg "n_domains must be positive";
      let context = context ?fatal_exn_handler () in
      let runner_main =
        if n_domains = 1 then fun () -> None
        else
          let bt_status = Printexc.backtrace_status () in
          fun () ->
            Printexc.record_backtrace bt_status;
            match runner_on_this_thread context with
            | () -> None
            | exception exn ->
              let bt = Printexc.get_raw_backtrace () in
              Some (exn, bt)
      in
      run_fiber_on n_domains fiber main runner_main context

    let[@inline never] run_on ?fatal_exn_handler ~n_domains fiber main computation =
      run_fiber_on ?fatal_exn_handler ~n_domains fiber main;
      Computation.peek_exn computation

    let run_on ?fatal_exn_handler ~n_domains ?forbid main =
      let forbid = match forbid with None -> false | Some forbid -> forbid in
      let computation = Computation.create ~mode:`LIFO () in
      let fiber = Fiber.create ~forbid computation in
      let main _ = Computation.capture computation main () in
      run_on ?fatal_exn_handler ~n_domains fiber main computation

  end

  
  let run title f =
    let _alarm = Gc.create_alarm update_memory in
    Format.printf "Running %s@.%!" title;
    Gc.compact ();
    reset_monitor ();
    let measure () =
      let s0 = Gc.quick_stat () in
      s0.live_words, 
      s0.minor_words +. s0.major_words -. s0.promoted_words
    in
    let _init_live, init_allocated = measure () in
    let init_stack, x = Scheduler.run_on ~n_domains:4 @@ fun () ->
      let n = Effect.perform Stack_size in
      let x = f () in
      n, x
    in
    Gc.full_major ();
    Gc.delete_alarm _alarm;
    let _final_live, final_allocated = measure () in
    Format.printf "Peak Memory: %.1f MB@.%!"
      ((float !m.peak_words -. float _init_live)/.1024./.1024.);
    Format.printf "Allocated Memory: %.1f MB@.%!"
      ((final_allocated -. init_allocated) /.1024./.1024.);
    Format.printf "Peak Main Stack: %f MB@.%!"
      ((float !m.peak_main_stack -. float init_stack)/.1024./.1024.);
    Format.printf "Peak Stack: %f MB@.%!"
      ((float !m.peak_stacks -. float init_stack)/.1024./.1024.);
    Format.printf "Peak Total: %.1f MB@.@.%!"
      ((float !m.peak_stacks +. float !m.peak_words)/.1024./.1024.);
    x

end

 
let main () =
  let iterations = 100000. in
  let f x =
    if Random.int 100 <= 2 then Memmeasure.update_current_stack ();
    let r = ref 0 in
    for _ = 0 to int_of_float (x *. iterations) do incr r done;
    !r
  in
  let size = 100000 in
  let t = gen_tree size in
  Format.printf "Tree Size: %i, Tree Height: %i@.%!"
    (cardinal t) (height t);
  Format.printf "Data size: %.1f MB@.%!"
    (float (Obj.reachable_words (Obj.repr t)) /.1024./.1024.);
  Format.printf "Task Size: %.0f average iterations@.%!"
    iterations;
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
