// Provided by @octachron
// Additional runtime functions to measure the given stack.

#define CAML_INTERNALS

#include <caml/mlvalues.h>
#include <caml/fiber.h>

value caml_used_stack_size(value cont) {
  value vstack = Field(cont, 0);
  struct stack_info* stack = Ptr_val(vstack);
  uint s = Stack_high(stack) - stack-> sp;
  return Val_int(s);
}
value caml_total_stack_size(value cont) {
  value vstack = Field(cont, 0);
  struct stack_info* stack = Ptr_val(vstack);
  uint s = Stack_high(stack) - Stack_base(stack);
  return Val_int(s);
}
