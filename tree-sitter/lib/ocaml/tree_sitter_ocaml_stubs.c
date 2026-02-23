#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#include "tree_sitter/api.h"

const TSLanguage* tree_sitter_ocaml(void);
const TSLanguage* tree_sitter_ocaml_interface(void);
const TSLanguage* tree_sitter_ocaml_type(void);

CAMLprim value caml_tree_sitter_ocaml_language(value unit) {
  CAMLparam1(unit);
  const TSLanguage* lang = tree_sitter_ocaml();
  CAMLreturn(caml_copy_nativeint((intnat)lang));
}

CAMLprim value caml_tree_sitter_ocaml_interface_language(value unit) {
  CAMLparam1(unit);
  const TSLanguage* lang = tree_sitter_ocaml_interface();
  CAMLreturn(caml_copy_nativeint((intnat)lang));
}

CAMLprim value caml_tree_sitter_ocaml_type_language(value unit) {
  CAMLparam1(unit);
  const TSLanguage* lang = tree_sitter_ocaml_type();
  CAMLreturn(caml_copy_nativeint((intnat)lang));
}
