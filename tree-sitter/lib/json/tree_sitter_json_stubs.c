#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#include "tree_sitter/api.h"

const TSLanguage* tree_sitter_json(void);

CAMLprim value caml_tree_sitter_json_language(value unit) {
  CAMLparam1(unit);
  const TSLanguage* lang = tree_sitter_json();
  CAMLreturn(caml_copy_nativeint((intnat)lang));
}
