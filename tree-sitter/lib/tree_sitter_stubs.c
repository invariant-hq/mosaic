#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "alloc.h"
#include "tree_sitter/api.h"

/* Handle types */

struct ts_language_handle {
  const TSLanguage* language;
};

struct ts_parser_handle {
  TSParser* parser;
};

struct ts_tree_handle {
  TSTree* tree;
};

struct ts_node_handle {
  TSNode node;
};

struct ts_query_handle {
  TSQuery* query;
};

struct ts_query_cursor_handle {
  TSQueryCursor* cursor;
};

#define Language_val(v) ((struct ts_language_handle*)Data_custom_val(v))
#define Parser_val(v) ((struct ts_parser_handle*)Data_custom_val(v))
#define Tree_val(v) ((struct ts_tree_handle*)Data_custom_val(v))
#define Node_val(v) ((struct ts_node_handle*)Data_custom_val(v))
#define Query_val(v) ((struct ts_query_handle*)Data_custom_val(v))
#define QueryCursor_val(v) ((struct ts_query_cursor_handle*)Data_custom_val(v))

/* Finalizers */

static void finalize_language(value v) {
  struct ts_language_handle* handle = Language_val(v);
  handle->language = NULL;
}

static void finalize_parser(value v) {
  struct ts_parser_handle* handle = Parser_val(v);
  if (handle->parser != NULL) {
    ts_parser_delete(handle->parser);
    handle->parser = NULL;
  }
}

static void finalize_tree(value v) {
  struct ts_tree_handle* handle = Tree_val(v);
  if (handle->tree != NULL) {
    ts_tree_delete(handle->tree);
    handle->tree = NULL;
  }
}

static void finalize_query(value v) {
  struct ts_query_handle* handle = Query_val(v);
  if (handle->query != NULL) {
    ts_query_delete(handle->query);
    handle->query = NULL;
  }
}

static void finalize_query_cursor(value v) {
  struct ts_query_cursor_handle* handle = QueryCursor_val(v);
  if (handle->cursor != NULL) {
    ts_query_cursor_delete(handle->cursor);
    handle->cursor = NULL;
  }
}

/* Custom operations */

static struct custom_operations language_ops = {
    .identifier = "tree_sitter.language",
    .finalize = finalize_language,
    .compare = custom_compare_default,
    .compare_ext = custom_compare_ext_default,
    .hash = custom_hash_default,
    .serialize = custom_serialize_default,
    .deserialize = custom_deserialize_default,
    .fixed_length = false,
};

static struct custom_operations parser_ops = {
    .identifier = "tree_sitter.parser",
    .finalize = finalize_parser,
    .compare = custom_compare_default,
    .compare_ext = custom_compare_ext_default,
    .hash = custom_hash_default,
    .serialize = custom_serialize_default,
    .deserialize = custom_deserialize_default,
    .fixed_length = false,
};

static struct custom_operations tree_ops = {
    .identifier = "tree_sitter.tree",
    .finalize = finalize_tree,
    .compare = custom_compare_default,
    .compare_ext = custom_compare_ext_default,
    .hash = custom_hash_default,
    .serialize = custom_serialize_default,
    .deserialize = custom_deserialize_default,
    .fixed_length = false,
};

static struct custom_operations node_ops = {
    .identifier = "tree_sitter.node",
    .finalize = NULL,
    .compare = custom_compare_default,
    .compare_ext = custom_compare_ext_default,
    .hash = custom_hash_default,
    .serialize = custom_serialize_default,
    .deserialize = custom_deserialize_default,
    .fixed_length = false,
};

static struct custom_operations query_ops = {
    .identifier = "tree_sitter.query",
    .finalize = finalize_query,
    .compare = custom_compare_default,
    .compare_ext = custom_compare_ext_default,
    .hash = custom_hash_default,
    .serialize = custom_serialize_default,
    .deserialize = custom_deserialize_default,
    .fixed_length = false,
};

static struct custom_operations query_cursor_ops = {
    .identifier = "tree_sitter.query_cursor",
    .finalize = finalize_query_cursor,
    .compare = custom_compare_default,
    .compare_ext = custom_compare_ext_default,
    .hash = custom_hash_default,
    .serialize = custom_serialize_default,
    .deserialize = custom_deserialize_default,
    .fixed_length = false,
};

/* Allocators */

static value alloc_language(const TSLanguage* language) {
  value v = caml_alloc_custom_mem(&language_ops,
                                  sizeof(struct ts_language_handle), 0);
  struct ts_language_handle* handle = Language_val(v);
  handle->language = language;
  return v;
}

static value alloc_parser(TSParser* parser) {
  value v =
      caml_alloc_custom_mem(&parser_ops, sizeof(struct ts_parser_handle), 0);
  struct ts_parser_handle* handle = Parser_val(v);
  handle->parser = parser;
  return v;
}

static value alloc_tree(TSTree* tree) {
  value v = caml_alloc_custom_mem(&tree_ops, sizeof(struct ts_tree_handle), 0);
  struct ts_tree_handle* handle = Tree_val(v);
  handle->tree = tree;
  return v;
}

static value alloc_node(TSNode node) {
  value v = caml_alloc_custom_mem(&node_ops, sizeof(struct ts_node_handle), 0);
  struct ts_node_handle* handle = Node_val(v);
  handle->node = node;
  return v;
}

static value alloc_query(TSQuery* query) {
  value v =
      caml_alloc_custom_mem(&query_ops, sizeof(struct ts_query_handle), 0);
  struct ts_query_handle* handle = Query_val(v);
  handle->query = query;
  return v;
}

static value alloc_query_cursor(TSQueryCursor* cursor) {
  value v = caml_alloc_custom_mem(&query_cursor_ops,
                                  sizeof(struct ts_query_cursor_handle), 0);
  struct ts_query_cursor_handle* handle = QueryCursor_val(v);
  handle->cursor = cursor;
  return v;
}

/* Helpers */

static value caml_ts_optional_node(TSNode node) {
  CAMLparam0();
  CAMLlocal2(result, node_value);
  if (ts_node_is_null(node)) {
    CAMLreturn(Val_int(0));
  }
  node_value = alloc_node(node);
  result = caml_alloc(1, 0);
  Store_field(result, 0, node_value);
  CAMLreturn(result);
}

static value caml_ts_copy_point(TSPoint point) {
  CAMLparam0();
  CAMLlocal1(res);
  res = caml_alloc_tuple(2);
  Store_field(res, 0, Val_int(point.row));
  Store_field(res, 1, Val_int(point.column));
  CAMLreturn(res);
}

static TSPoint caml_ts_point_of_value(value v) {
  TSPoint point = {0, 0};
  if (Is_block(v) && Wosize_val(v) == 2) {
    point.row = (uint32_t)Int_val(Field(v, 0));
    point.column = (uint32_t)Int_val(Field(v, 1));
  }
  return point;
}

static value caml_ts_range_to_value(const TSRange* range) {
  CAMLparam0();
  CAMLlocal1(record);
  record = caml_alloc(4, 0);
  Store_field(record, 0, Val_int(range->start_byte));
  Store_field(record, 1, Val_int(range->end_byte));
  Store_field(record, 2, caml_ts_copy_point(range->start_point));
  Store_field(record, 3, caml_ts_copy_point(range->end_point));
  CAMLreturn(record);
}

/* Language */

CAMLprim value caml_ts_language_of_address(value addr) {
  CAMLparam1(addr);
  const TSLanguage* language =
      (const TSLanguage*)(uintptr_t)Nativeint_val(addr);
  if (language == NULL) {
    caml_failwith("Tree_sitter.Language.of_address: null pointer");
  }
  CAMLreturn(alloc_language(language));
}

CAMLprim value caml_ts_language_version(value language_v) {
  CAMLparam1(language_v);
  const TSLanguage* language = Language_val(language_v)->language;
  CAMLreturn(Val_int(ts_language_version(language)));
}

CAMLprim value caml_ts_language_name(value language_v) {
  CAMLparam1(language_v);
  const TSLanguage* language = Language_val(language_v)->language;
  const char* name = ts_language_name(language);
  CAMLreturn(caml_copy_string(name == NULL ? "" : name));
}

CAMLprim value caml_ts_language_symbol_count(value language_v) {
  CAMLparam1(language_v);
  const TSLanguage* language = Language_val(language_v)->language;
  CAMLreturn(Val_int(ts_language_symbol_count(language)));
}

CAMLprim value caml_ts_language_symbol_name(value language_v, value symbol_v) {
  CAMLparam2(language_v, symbol_v);
  const TSLanguage* language = Language_val(language_v)->language;
  TSSymbol symbol = (TSSymbol)Int_val(symbol_v);
  const char* name = ts_language_symbol_name(language, symbol);
  CAMLreturn(caml_copy_string(name == NULL ? "" : name));
}

CAMLprim value caml_ts_language_field_count(value language_v) {
  CAMLparam1(language_v);
  const TSLanguage* language = Language_val(language_v)->language;
  CAMLreturn(Val_int(ts_language_field_count(language)));
}

CAMLprim value caml_ts_language_field_name_for_id(value language_v,
                                                  value id_v) {
  CAMLparam2(language_v, id_v);
  CAMLlocal2(result, some);
  const TSLanguage* language = Language_val(language_v)->language;
  TSFieldId field_id = (TSFieldId)Int_val(id_v);
  const char* name = ts_language_field_name_for_id(language, field_id);
  value ret;
  if (name == NULL) {
    ret = Val_int(0);
  } else {
    result = caml_copy_string(name);
    some = caml_alloc(1, 0);
    Store_field(some, 0, result);
    ret = some;
  }
  CAMLreturn(ret);
}

CAMLprim value caml_ts_language_field_id_for_name(value language_v,
                                                  value name_v) {
  CAMLparam2(language_v, name_v);
  CAMLlocal1(some);
  const TSLanguage* language = Language_val(language_v)->language;
  const char* name = String_val(name_v);
  uint32_t length = (uint32_t)caml_string_length(name_v);
  TSFieldId field_id = ts_language_field_id_for_name(language, name, length);
  value ret;
  if (field_id == 0) {
    ret = Val_int(0);
  } else {
    some = caml_alloc(1, 0);
    Store_field(some, 0, Val_int(field_id));
    ret = some;
  }
  CAMLreturn(ret);
}

CAMLprim value caml_ts_language_symbol_for_name(value language_v, value name_v,
                                                value is_named_v) {
  CAMLparam3(language_v, name_v, is_named_v);
  CAMLlocal1(some);
  const TSLanguage* language = Language_val(language_v)->language;
  const char* name = String_val(name_v);
  uint32_t length = (uint32_t)caml_string_length(name_v);
  bool is_named = Bool_val(is_named_v);
  TSSymbol symbol =
      ts_language_symbol_for_name(language, name, length, is_named);
  if (symbol == 0) {
    CAMLreturn(Val_int(0));
  }
  some = caml_alloc(1, 0);
  Store_field(some, 0, Val_int(symbol));
  CAMLreturn(some);
}

CAMLprim value caml_ts_language_symbol_type(value language_v, value symbol_v) {
  CAMLparam2(language_v, symbol_v);
  const TSLanguage* language = Language_val(language_v)->language;
  TSSymbol symbol = (TSSymbol)Int_val(symbol_v);
  TSSymbolType type = ts_language_symbol_type(language, symbol);
  CAMLreturn(Val_int((int)type));
}

/* Parser */

CAMLprim value caml_ts_parser_create_with_language(value language_v) {
  CAMLparam1(language_v);
  const TSLanguage* language = Language_val(language_v)->language;
  if (language == NULL) {
    caml_failwith("Tree_sitter.Parser.create: null language");
  }
  TSParser* parser = ts_parser_new();
  if (parser == NULL) {
    caml_failwith("Tree_sitter.Parser.create: ts_parser_new returned NULL");
  }
  if (!ts_parser_set_language(parser, language)) {
    ts_parser_delete(parser);
    caml_failwith("Tree_sitter.Parser.create: incompatible language version");
  }
  CAMLreturn(alloc_parser(parser));
}

CAMLprim value caml_ts_parser_language(value parser_v) {
  CAMLparam1(parser_v);
  struct ts_parser_handle* parser = Parser_val(parser_v);
  if (parser->parser == NULL) {
    caml_failwith("Tree_sitter.Parser.language: parser is closed");
  }
  const TSLanguage* language = ts_parser_language(parser->parser);
  if (language == NULL) {
    caml_failwith("Tree_sitter.Parser.language: no language set");
  }
  CAMLreturn(alloc_language(language));
}

CAMLprim value caml_ts_parser_set_language(value parser_v, value language_v) {
  CAMLparam2(parser_v, language_v);
  struct ts_parser_handle* parser = Parser_val(parser_v);
  const TSLanguage* language = Language_val(language_v)->language;
  if (parser->parser == NULL || language == NULL) {
    caml_failwith(
        "Tree_sitter.Parser.set_language: invalid parser or language");
  }
  if (!ts_parser_set_language(parser->parser, language)) {
    caml_failwith("Tree_sitter.Parser.set_language: incompatible version");
  }
  CAMLreturn(Val_unit);
}

CAMLprim value caml_ts_parser_parse_string(value parser_v, value source_v) {
  CAMLparam2(parser_v, source_v);
  struct ts_parser_handle* parser = Parser_val(parser_v);
  if (parser->parser == NULL) {
    caml_failwith("Tree_sitter.Parser.parse_string: parser is closed");
  }
  const char* source = String_val(source_v);
  size_t length = caml_string_length(source_v);
  TSTree* tree =
      ts_parser_parse_string(parser->parser, NULL, source, (uint32_t)length);
  if (tree == NULL) {
    caml_failwith("Tree_sitter.Parser.parse_string: parse returned NULL");
  }
  CAMLreturn(alloc_tree(tree));
}

CAMLprim value caml_ts_parser_parse_string_old(value parser_v,
                                               value old_tree_opt,
                                               value source_v) {
  CAMLparam3(parser_v, old_tree_opt, source_v);
  struct ts_parser_handle* parser = Parser_val(parser_v);
  if (parser->parser == NULL) {
    caml_failwith("Tree_sitter.Parser.parse_string: parser is closed");
  }
  const TSTree* old_tree = NULL;
  if (old_tree_opt != Val_int(0)) {
    struct ts_tree_handle* old_handle = Tree_val(Field(old_tree_opt, 0));
    old_tree = old_handle->tree;
  }
  const char* source = String_val(source_v);
  size_t length = caml_string_length(source_v);
  TSTree* tree = ts_parser_parse_string(parser->parser, old_tree, source,
                                        (uint32_t)length);
  if (tree == NULL) {
    caml_failwith("Tree_sitter.Parser.parse_string: parse returned NULL");
  }
  CAMLreturn(alloc_tree(tree));
}

CAMLprim value caml_ts_parser_reset(value parser_v) {
  CAMLparam1(parser_v);
  struct ts_parser_handle* parser = Parser_val(parser_v);
  if (parser->parser == NULL) {
    caml_failwith("Tree_sitter.Parser.reset: parser is closed");
  }
  ts_parser_reset(parser->parser);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_ts_parser_set_included_ranges(value parser_v,
                                                  value ranges_v) {
  CAMLparam2(parser_v, ranges_v);
  struct ts_parser_handle* parser = Parser_val(parser_v);
  if (parser->parser == NULL) {
    caml_failwith("Tree_sitter.Parser.set_included_ranges: parser is closed");
  }
  size_t count = Wosize_val(ranges_v);
  if (count == 0) {
    ts_parser_set_included_ranges(parser->parser, NULL, 0);
    CAMLreturn(Val_unit);
  }
  TSRange* ranges = (TSRange*)malloc(count * sizeof(TSRange));
  if (ranges == NULL) caml_raise_out_of_memory();
  for (size_t i = 0; i < count; ++i) {
    value r = Field(ranges_v, i);
    ranges[i].start_byte = (uint32_t)Int_val(Field(r, 0));
    ranges[i].end_byte = (uint32_t)Int_val(Field(r, 1));
    ranges[i].start_point = caml_ts_point_of_value(Field(r, 2));
    ranges[i].end_point = caml_ts_point_of_value(Field(r, 3));
  }
  bool ok =
      ts_parser_set_included_ranges(parser->parser, ranges, (uint32_t)count);
  free(ranges);
  if (!ok) {
    caml_failwith("Tree_sitter.Parser.set_included_ranges: invalid ranges");
  }
  CAMLreturn(Val_unit);
}

/* Node */

CAMLprim value caml_ts_node_type(value node_v) {
  CAMLparam1(node_v);
  TSNode node = Node_val(node_v)->node;
  const char* type = ts_node_type(node);
  CAMLreturn(caml_copy_string(type == NULL ? "" : type));
}

CAMLprim value caml_ts_node_symbol(value node_v) {
  CAMLparam1(node_v);
  TSNode node = Node_val(node_v)->node;
  CAMLreturn(Val_int(ts_node_symbol(node)));
}

CAMLprim value caml_ts_node_start_byte(value node_v) {
  CAMLparam1(node_v);
  TSNode node = Node_val(node_v)->node;
  CAMLreturn(Val_int(ts_node_start_byte(node)));
}

CAMLprim value caml_ts_node_end_byte(value node_v) {
  CAMLparam1(node_v);
  TSNode node = Node_val(node_v)->node;
  CAMLreturn(Val_int(ts_node_end_byte(node)));
}

CAMLprim value caml_ts_node_start_point(value node_v) {
  CAMLparam1(node_v);
  TSNode node = Node_val(node_v)->node;
  TSPoint point = ts_node_start_point(node);
  CAMLreturn(caml_ts_copy_point(point));
}

CAMLprim value caml_ts_node_end_point(value node_v) {
  CAMLparam1(node_v);
  TSNode node = Node_val(node_v)->node;
  TSPoint point = ts_node_end_point(node);
  CAMLreturn(caml_ts_copy_point(point));
}

CAMLprim value caml_ts_node_child_count(value node_v) {
  CAMLparam1(node_v);
  TSNode node = Node_val(node_v)->node;
  CAMLreturn(Val_int(ts_node_child_count(node)));
}

CAMLprim value caml_ts_node_named_child_count(value node_v) {
  CAMLparam1(node_v);
  TSNode node = Node_val(node_v)->node;
  CAMLreturn(Val_int(ts_node_named_child_count(node)));
}

CAMLprim value caml_ts_node_child(value node_v, value index_v) {
  CAMLparam2(node_v, index_v);
  TSNode node = Node_val(node_v)->node;
  uint32_t index = (uint32_t)Unsigned_long_val(index_v);
  TSNode child = ts_node_child(node, index);
  CAMLreturn(caml_ts_optional_node(child));
}

CAMLprim value caml_ts_node_named_child(value node_v, value index_v) {
  CAMLparam2(node_v, index_v);
  TSNode node = Node_val(node_v)->node;
  uint32_t index = (uint32_t)Unsigned_long_val(index_v);
  TSNode child = ts_node_named_child(node, index);
  CAMLreturn(caml_ts_optional_node(child));
}

CAMLprim value caml_ts_node_child_by_field_name(value node_v, value name_v) {
  CAMLparam2(node_v, name_v);
  TSNode node = Node_val(node_v)->node;
  const char* name = String_val(name_v);
  uint32_t length = (uint32_t)caml_string_length(name_v);
  TSNode child = ts_node_child_by_field_name(node, name, length);
  CAMLreturn(caml_ts_optional_node(child));
}

CAMLprim value caml_ts_node_parent(value node_v) {
  CAMLparam1(node_v);
  TSNode node = Node_val(node_v)->node;
  TSNode parent = ts_node_parent(node);
  CAMLreturn(caml_ts_optional_node(parent));
}

CAMLprim value caml_ts_node_next_sibling(value node_v) {
  CAMLparam1(node_v);
  TSNode node = Node_val(node_v)->node;
  TSNode sibling = ts_node_next_sibling(node);
  CAMLreturn(caml_ts_optional_node(sibling));
}

CAMLprim value caml_ts_node_prev_sibling(value node_v) {
  CAMLparam1(node_v);
  TSNode node = Node_val(node_v)->node;
  TSNode sibling = ts_node_prev_sibling(node);
  CAMLreturn(caml_ts_optional_node(sibling));
}

CAMLprim value caml_ts_node_next_named_sibling(value node_v) {
  CAMLparam1(node_v);
  TSNode node = Node_val(node_v)->node;
  TSNode sibling = ts_node_next_named_sibling(node);
  CAMLreturn(caml_ts_optional_node(sibling));
}

CAMLprim value caml_ts_node_prev_named_sibling(value node_v) {
  CAMLparam1(node_v);
  TSNode node = Node_val(node_v)->node;
  TSNode sibling = ts_node_prev_named_sibling(node);
  CAMLreturn(caml_ts_optional_node(sibling));
}

CAMLprim value caml_ts_node_descendant_for_byte_range(value node_v,
                                                      value start_v,
                                                      value end_v) {
  CAMLparam3(node_v, start_v, end_v);
  TSNode node = Node_val(node_v)->node;
  uint32_t start_byte = (uint32_t)Unsigned_long_val(start_v);
  uint32_t end_byte = (uint32_t)Unsigned_long_val(end_v);
  TSNode descendant =
      ts_node_descendant_for_byte_range(node, start_byte, end_byte);
  CAMLreturn(caml_ts_optional_node(descendant));
}

CAMLprim value caml_ts_node_descendant_for_point_range(value node_v,
                                                       value start_point_v,
                                                       value end_point_v) {
  CAMLparam3(node_v, start_point_v, end_point_v);
  TSNode node = Node_val(node_v)->node;
  TSPoint start_point = caml_ts_point_of_value(start_point_v);
  TSPoint end_point = caml_ts_point_of_value(end_point_v);
  TSNode descendant =
      ts_node_descendant_for_point_range(node, start_point, end_point);
  CAMLreturn(caml_ts_optional_node(descendant));
}

CAMLprim value caml_ts_node_named_descendant_for_byte_range(value node_v,
                                                            value start_v,
                                                            value end_v) {
  CAMLparam3(node_v, start_v, end_v);
  TSNode node = Node_val(node_v)->node;
  uint32_t start_byte = (uint32_t)Unsigned_long_val(start_v);
  uint32_t end_byte = (uint32_t)Unsigned_long_val(end_v);
  TSNode descendant =
      ts_node_named_descendant_for_byte_range(node, start_byte, end_byte);
  CAMLreturn(caml_ts_optional_node(descendant));
}

CAMLprim value caml_ts_node_named_descendant_for_point_range(
    value node_v, value start_point_v, value end_point_v) {
  CAMLparam3(node_v, start_point_v, end_point_v);
  TSNode node = Node_val(node_v)->node;
  TSPoint start_point = caml_ts_point_of_value(start_point_v);
  TSPoint end_point = caml_ts_point_of_value(end_point_v);
  TSNode descendant =
      ts_node_named_descendant_for_point_range(node, start_point, end_point);
  CAMLreturn(caml_ts_optional_node(descendant));
}

CAMLprim value caml_ts_node_is_named(value node_v) {
  CAMLparam1(node_v);
  TSNode node = Node_val(node_v)->node;
  CAMLreturn(Val_bool(ts_node_is_named(node)));
}

CAMLprim value caml_ts_node_is_missing(value node_v) {
  CAMLparam1(node_v);
  TSNode node = Node_val(node_v)->node;
  CAMLreturn(Val_bool(ts_node_is_missing(node)));
}

CAMLprim value caml_ts_node_is_extra(value node_v) {
  CAMLparam1(node_v);
  TSNode node = Node_val(node_v)->node;
  CAMLreturn(Val_bool(ts_node_is_extra(node)));
}

CAMLprim value caml_ts_node_is_error(value node_v) {
  CAMLparam1(node_v);
  TSNode node = Node_val(node_v)->node;
  CAMLreturn(Val_bool(ts_node_is_error(node)));
}

CAMLprim value caml_ts_node_has_error(value node_v) {
  CAMLparam1(node_v);
  TSNode node = Node_val(node_v)->node;
  CAMLreturn(Val_bool(ts_node_has_error(node)));
}

CAMLprim value caml_ts_node_has_changes(value node_v) {
  CAMLparam1(node_v);
  TSNode node = Node_val(node_v)->node;
  CAMLreturn(Val_bool(ts_node_has_changes(node)));
}

CAMLprim value caml_ts_node_to_sexp(value node_v) {
  CAMLparam1(node_v);
  CAMLlocal1(result);
  TSNode node = Node_val(node_v)->node;
  char* sexp = ts_node_string(node);
  if (sexp == NULL) {
    caml_failwith("Tree_sitter.Node.to_sexp: ts_node_string returned NULL");
  }
  result = caml_copy_string(sexp);
  free(sexp);
  CAMLreturn(result);
}

CAMLprim value caml_ts_node_eq(value lhs_v, value rhs_v) {
  CAMLparam2(lhs_v, rhs_v);
  TSNode lhs = Node_val(lhs_v)->node;
  TSNode rhs = Node_val(rhs_v)->node;
  CAMLreturn(Val_bool(ts_node_eq(lhs, rhs)));
}

/* Tree */

CAMLprim value caml_ts_tree_root_node(value tree_v) {
  CAMLparam1(tree_v);
  struct ts_tree_handle* tree = Tree_val(tree_v);
  if (tree->tree == NULL) {
    caml_failwith("Tree_sitter.Tree.root_node: tree is closed");
  }
  TSNode root = ts_tree_root_node(tree->tree);
  CAMLreturn(alloc_node(root));
}

CAMLprim value caml_ts_tree_root_sexp(value tree_v) {
  CAMLparam1(tree_v);
  CAMLlocal1(result);
  struct ts_tree_handle* tree = Tree_val(tree_v);
  if (tree->tree == NULL) {
    caml_failwith("Tree_sitter.Tree.root_sexp: tree is closed");
  }
  TSNode root = ts_tree_root_node(tree->tree);
  char* sexp = ts_node_string(root);
  if (sexp == NULL) {
    caml_failwith("Tree_sitter.Tree.root_sexp: ts_node_string returned NULL");
  }
  result = caml_copy_string(sexp);
  free(sexp);
  CAMLreturn(result);
}

CAMLprim value caml_ts_tree_copy(value tree_v) {
  CAMLparam1(tree_v);
  struct ts_tree_handle* tree = Tree_val(tree_v);
  if (tree->tree == NULL) {
    caml_failwith("Tree_sitter.Tree.copy: tree is closed");
  }
  TSTree* copy = ts_tree_copy(tree->tree);
  if (copy == NULL) {
    caml_failwith("Tree_sitter.Tree.copy: ts_tree_copy returned NULL");
  }
  CAMLreturn(alloc_tree(copy));
}

CAMLprim value caml_ts_tree_language(value tree_v) {
  CAMLparam1(tree_v);
  struct ts_tree_handle* tree = Tree_val(tree_v);
  if (tree->tree == NULL) {
    caml_failwith("Tree_sitter.Tree.language: tree is closed");
  }
  const TSLanguage* language = ts_tree_language(tree->tree);
  if (language == NULL) {
    caml_failwith("Tree_sitter.Tree.language: returned NULL");
  }
  CAMLreturn(alloc_language(language));
}

CAMLprim value caml_ts_tree_edit_native(value tree_v, value start_byte_v,
                                        value old_end_byte_v,
                                        value new_end_byte_v,
                                        value start_point_v,
                                        value old_end_point_v,
                                        value new_end_point_v) {
  CAMLparam5(tree_v, start_byte_v, old_end_byte_v, new_end_byte_v,
             start_point_v);
  CAMLxparam2(old_end_point_v, new_end_point_v);
  struct ts_tree_handle* tree = Tree_val(tree_v);
  if (tree->tree == NULL) {
    caml_failwith("Tree_sitter.Tree.edit: tree is closed");
  }
  TSInputEdit edit = {
      .start_byte = Unsigned_long_val(start_byte_v),
      .old_end_byte = Unsigned_long_val(old_end_byte_v),
      .new_end_byte = Unsigned_long_val(new_end_byte_v),
      .start_point = caml_ts_point_of_value(start_point_v),
      .old_end_point = caml_ts_point_of_value(old_end_point_v),
      .new_end_point = caml_ts_point_of_value(new_end_point_v),
  };
  ts_tree_edit(tree->tree, &edit);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_ts_tree_edit_bytecode(value* argv, int argn) {
  (void)argn;
  return caml_ts_tree_edit_native(argv[0], argv[1], argv[2], argv[3], argv[4],
                                  argv[5], argv[6]);
}

CAMLprim value caml_ts_tree_get_changed_ranges(value old_tree_v,
                                               value new_tree_v) {
  CAMLparam2(old_tree_v, new_tree_v);
  CAMLlocal1(array);
  struct ts_tree_handle* old_tree = Tree_val(old_tree_v);
  struct ts_tree_handle* new_tree = Tree_val(new_tree_v);
  if (old_tree->tree == NULL || new_tree->tree == NULL) {
    caml_failwith("Tree_sitter.Tree.changed_ranges: tree is closed");
  }
  uint32_t length = 0;
  TSRange* ranges =
      ts_tree_get_changed_ranges(old_tree->tree, new_tree->tree, &length);
  if (ranges == NULL) {
    array = caml_alloc(0, 0);
    CAMLreturn(array);
  }
  array = caml_alloc(length, 0);
  for (uint32_t i = 0; i < length; ++i) {
    Store_field(array, i, caml_ts_range_to_value(&ranges[i]));
  }
  ts_free(ranges);
  CAMLreturn(array);
}

CAMLprim value caml_ts_tree_included_ranges(value tree_v) {
  CAMLparam1(tree_v);
  CAMLlocal1(array);
  struct ts_tree_handle* tree = Tree_val(tree_v);
  if (tree->tree == NULL) {
    caml_failwith("Tree_sitter.Tree.included_ranges: tree is closed");
  }
  uint32_t length = 0;
  TSRange* ranges = ts_tree_included_ranges(tree->tree, &length);
  if (ranges == NULL) {
    array = caml_alloc(0, 0);
    CAMLreturn(array);
  }
  array = caml_alloc(length, 0);
  for (uint32_t i = 0; i < length; ++i) {
    Store_field(array, i, caml_ts_range_to_value(&ranges[i]));
  }
  ts_free(ranges);
  CAMLreturn(array);
}

/* Query */

static const char* ts_query_error_to_string(TSQueryError error_type) {
  switch (error_type) {
    case TSQueryErrorNone:
      return "none";
    case TSQueryErrorSyntax:
      return "syntax";
    case TSQueryErrorNodeType:
      return "node_type";
    case TSQueryErrorField:
      return "field";
    case TSQueryErrorCapture:
      return "capture";
    case TSQueryErrorStructure:
      return "structure";
    case TSQueryErrorLanguage:
      return "language";
    default:
      return "unknown";
  }
}

CAMLprim value caml_ts_query_new(value language_v, value source_v) {
  CAMLparam2(language_v, source_v);
  const TSLanguage* language = Language_val(language_v)->language;
  const char* source = String_val(source_v);
  uint32_t length = (uint32_t)caml_string_length(source_v);
  uint32_t error_offset = 0;
  TSQueryError error_type = TSQueryErrorNone;
  TSQuery* query =
      ts_query_new(language, source, length, &error_offset, &error_type);
  if (query == NULL) {
    char buffer[128];
    snprintf(buffer, sizeof(buffer),
             "Tree_sitter.Query.create: %s error at offset %u",
             ts_query_error_to_string(error_type), error_offset);
    caml_failwith(buffer);
  }
  CAMLreturn(alloc_query(query));
}

CAMLprim value caml_ts_query_capture_count(value query_v) {
  CAMLparam1(query_v);
  struct ts_query_handle* handle = Query_val(query_v);
  if (handle->query == NULL) {
    caml_failwith("Tree_sitter.Query.capture_count: query disposed");
  }
  CAMLreturn(Val_int(ts_query_capture_count(handle->query)));
}

CAMLprim value caml_ts_query_pattern_count(value query_v) {
  CAMLparam1(query_v);
  struct ts_query_handle* handle = Query_val(query_v);
  if (handle->query == NULL) {
    caml_failwith("Tree_sitter.Query.pattern_count: query disposed");
  }
  CAMLreturn(Val_int(ts_query_pattern_count(handle->query)));
}

CAMLprim value caml_ts_query_capture_name_for_id(value query_v, value id_v) {
  CAMLparam2(query_v, id_v);
  CAMLlocal2(result, some);
  struct ts_query_handle* handle = Query_val(query_v);
  if (handle->query == NULL) {
    caml_failwith("Tree_sitter.Query.capture_name_for_id: query disposed");
  }
  uint32_t length = 0;
  uint32_t id = (uint32_t)Int_val(id_v);
  const char* name = ts_query_capture_name_for_id(handle->query, id, &length);
  if (name == NULL) {
    CAMLreturn(Val_int(0));
  }
  result = caml_alloc_initialized_string(length, name);
  some = caml_alloc(1, 0);
  Store_field(some, 0, result);
  CAMLreturn(some);
}

CAMLprim value caml_ts_query_capture_index_for_name(value query_v,
                                                    value name_v) {
  CAMLparam2(query_v, name_v);
  CAMLlocal1(some);
  struct ts_query_handle* handle = Query_val(query_v);
  if (handle->query == NULL) {
    caml_failwith("Tree_sitter.Query.capture_index_for_name: query disposed");
  }
  size_t length = caml_string_length(name_v);
  const char* name = String_val(name_v);
  uint32_t capture_count = ts_query_capture_count(handle->query);
  for (uint32_t i = 0; i < capture_count; ++i) {
    uint32_t len = 0;
    const char* current = ts_query_capture_name_for_id(handle->query, i, &len);
    if (current != NULL && len == length &&
        strncmp(current, name, length) == 0) {
      some = caml_alloc(1, 0);
      Store_field(some, 0, Val_int(i));
      CAMLreturn(some);
    }
  }
  CAMLreturn(Val_int(0));
}

CAMLprim value caml_ts_query_disable_capture(value query_v, value name_v) {
  CAMLparam2(query_v, name_v);
  struct ts_query_handle* handle = Query_val(query_v);
  if (handle->query == NULL) {
    caml_failwith("Tree_sitter.Query.disable_capture: query disposed");
  }
  const char* name = String_val(name_v);
  uint32_t length = (uint32_t)caml_string_length(name_v);
  ts_query_disable_capture(handle->query, name, length);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_ts_query_disable_pattern(value query_v, value pattern_v) {
  CAMLparam2(query_v, pattern_v);
  struct ts_query_handle* handle = Query_val(query_v);
  if (handle->query == NULL) {
    caml_failwith("Tree_sitter.Query.disable_pattern: query disposed");
  }
  uint32_t pattern_index = (uint32_t)Int_val(pattern_v);
  ts_query_disable_pattern(handle->query, pattern_index);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_ts_query_string_value_for_id(value query_v, value id_v) {
  CAMLparam2(query_v, id_v);
  CAMLlocal2(result, some);
  struct ts_query_handle* handle = Query_val(query_v);
  if (handle->query == NULL) {
    caml_failwith("Tree_sitter.Query.string_value_for_id: query disposed");
  }
  uint32_t length = 0;
  uint32_t id = (uint32_t)Int_val(id_v);
  const char* str = ts_query_string_value_for_id(handle->query, id, &length);
  if (str == NULL) {
    CAMLreturn(Val_int(0));
  }
  result = caml_alloc_initialized_string(length, str);
  some = caml_alloc(1, 0);
  Store_field(some, 0, result);
  CAMLreturn(some);
}

CAMLprim value caml_ts_query_predicates_for_pattern(value query_v,
                                                    value pattern_v) {
  CAMLparam2(query_v, pattern_v);
  CAMLlocal2(array, step);
  struct ts_query_handle* handle = Query_val(query_v);
  if (handle->query == NULL) {
    caml_failwith("Tree_sitter.Query.predicates_for_pattern: query disposed");
  }
  uint32_t pattern_index = (uint32_t)Int_val(pattern_v);
  uint32_t step_count = 0;
  const TSQueryPredicateStep* steps = ts_query_predicates_for_pattern(
      handle->query, pattern_index, &step_count);
  array = caml_alloc(step_count, 0);
  for (uint32_t i = 0; i < step_count; ++i) {
    /* Each step is (type : int, value_id : int) encoded as a tuple */
    step = caml_alloc_tuple(2);
    Store_field(step, 0, Val_int((int)steps[i].type));
    Store_field(step, 1, Val_int(steps[i].value_id));
    Store_field(array, i, step);
  }
  CAMLreturn(array);
}

/* Query_cursor */

CAMLprim value caml_ts_query_cursor_new(value unit) {
  CAMLparam1(unit);
  TSQueryCursor* cursor = ts_query_cursor_new();
  if (cursor == NULL) {
    caml_failwith("Tree_sitter.Query_cursor.create: returned NULL");
  }
  CAMLreturn(alloc_query_cursor(cursor));
}

CAMLprim value caml_ts_query_cursor_exec(value cursor_v, value query_v,
                                         value node_v) {
  CAMLparam3(cursor_v, query_v, node_v);
  struct ts_query_cursor_handle* cursor = QueryCursor_val(cursor_v);
  struct ts_query_handle* query = Query_val(query_v);
  TSNode node = Node_val(node_v)->node;
  if (cursor->cursor == NULL || query->query == NULL) {
    caml_failwith("Tree_sitter.Query_cursor.exec: disposed cursor or query");
  }
  ts_query_cursor_exec(cursor->cursor, query->query, node);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_ts_query_cursor_set_byte_range(value cursor_v,
                                                   value start_v, value end_v) {
  CAMLparam3(cursor_v, start_v, end_v);
  struct ts_query_cursor_handle* cursor = QueryCursor_val(cursor_v);
  if (cursor->cursor == NULL) {
    caml_failwith("Tree_sitter.Query_cursor.set_byte_range: cursor disposed");
  }
  uint32_t start_byte = (uint32_t)Unsigned_long_val(start_v);
  uint32_t end_byte = (uint32_t)Unsigned_long_val(end_v);
  ts_query_cursor_set_byte_range(cursor->cursor, start_byte, end_byte);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_ts_query_cursor_set_point_range(value cursor_v,
                                                    value start_point_v,
                                                    value end_point_v) {
  CAMLparam3(cursor_v, start_point_v, end_point_v);
  struct ts_query_cursor_handle* cursor = QueryCursor_val(cursor_v);
  if (cursor->cursor == NULL) {
    caml_failwith("Tree_sitter.Query_cursor.set_point_range: cursor disposed");
  }
  TSPoint start_point = caml_ts_point_of_value(start_point_v);
  TSPoint end_point = caml_ts_point_of_value(end_point_v);
  ts_query_cursor_set_point_range(cursor->cursor, start_point, end_point);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_ts_query_cursor_next_match(value cursor_v) {
  CAMLparam1(cursor_v);
  CAMLlocal4(result, ocaml_match, captures, capture_tuple);
  struct ts_query_cursor_handle* cursor = QueryCursor_val(cursor_v);
  if (cursor->cursor == NULL) {
    caml_failwith("Tree_sitter.Query_cursor.next_match: cursor disposed");
  }
  TSQueryMatch match;
  if (!ts_query_cursor_next_match(cursor->cursor, &match)) {
    CAMLreturn(Val_int(0));
  }
  captures = caml_alloc(match.capture_count, 0);
  for (uint32_t i = 0; i < match.capture_count; ++i) {
    capture_tuple = caml_alloc_tuple(2);
    Store_field(capture_tuple, 0, Val_int(match.captures[i].index));
    Store_field(capture_tuple, 1, alloc_node(match.captures[i].node));
    Store_field(captures, i, capture_tuple);
  }
  ocaml_match = caml_alloc_tuple(2);
  Store_field(ocaml_match, 0, Val_int(match.pattern_index));
  Store_field(ocaml_match, 1, captures);
  result = caml_alloc(1, 0);
  Store_field(result, 0, ocaml_match);
  CAMLreturn(result);
}

CAMLprim value caml_ts_query_cursor_next_capture(value cursor_v,
                                                 value query_v) {
  CAMLparam2(cursor_v, query_v);
  CAMLlocal3(result, tup, node_value);
  struct ts_query_cursor_handle* cursor = QueryCursor_val(cursor_v);
  struct ts_query_handle* query = Query_val(query_v);
  if (cursor->cursor == NULL || query->query == NULL) {
    caml_failwith(
        "Tree_sitter.Query_cursor.next_capture: disposed cursor or query");
  }
  TSQueryMatch match;
  uint32_t capture_index = 0;
  if (!ts_query_cursor_next_capture(cursor->cursor, &match, &capture_index)) {
    CAMLreturn(Val_int(0));
  }
  if (capture_index >= match.capture_count) {
    CAMLreturn(Val_int(0));
  }
  TSQueryCapture capture = match.captures[capture_index];
  node_value = alloc_node(capture.node);
  tup = caml_alloc_tuple(3);
  Store_field(tup, 0, Val_int(capture.index));
  Store_field(tup, 1, Val_int(match.pattern_index));
  Store_field(tup, 2, node_value);
  result = caml_alloc(1, 0);
  Store_field(result, 0, tup);
  CAMLreturn(result);
}

/* Tree_cursor */

struct ts_tree_cursor_handle {
  TSTreeCursor cursor;
  bool valid;
};

#define TreeCursor_val(v) ((struct ts_tree_cursor_handle*)Data_custom_val(v))

static void finalize_tree_cursor(value v) {
  struct ts_tree_cursor_handle* handle = TreeCursor_val(v);
  if (handle->valid) {
    ts_tree_cursor_delete(&handle->cursor);
    handle->valid = false;
  }
}

static struct custom_operations tree_cursor_ops = {
    .identifier = "tree_sitter.tree_cursor",
    .finalize = finalize_tree_cursor,
    .compare = custom_compare_default,
    .compare_ext = custom_compare_ext_default,
    .hash = custom_hash_default,
    .serialize = custom_serialize_default,
    .deserialize = custom_deserialize_default,
    .fixed_length = false,
};

static value alloc_tree_cursor(TSTreeCursor cursor) {
  value v = caml_alloc_custom_mem(&tree_cursor_ops,
                                  sizeof(struct ts_tree_cursor_handle), 0);
  struct ts_tree_cursor_handle* handle = TreeCursor_val(v);
  handle->cursor = cursor;
  handle->valid = true;
  return v;
}

CAMLprim value caml_ts_tree_cursor_new(value node_v) {
  CAMLparam1(node_v);
  TSNode node = Node_val(node_v)->node;
  TSTreeCursor cursor = ts_tree_cursor_new(node);
  CAMLreturn(alloc_tree_cursor(cursor));
}

CAMLprim value caml_ts_tree_cursor_delete(value cursor_v) {
  CAMLparam1(cursor_v);
  struct ts_tree_cursor_handle* handle = TreeCursor_val(cursor_v);
  if (handle->valid) {
    ts_tree_cursor_delete(&handle->cursor);
    handle->valid = false;
  }
  CAMLreturn(Val_unit);
}

CAMLprim value caml_ts_tree_cursor_reset(value cursor_v, value node_v) {
  CAMLparam2(cursor_v, node_v);
  struct ts_tree_cursor_handle* handle = TreeCursor_val(cursor_v);
  if (!handle->valid) {
    caml_failwith("Tree_sitter.Tree_cursor.reset: cursor is closed");
  }
  TSNode node = Node_val(node_v)->node;
  ts_tree_cursor_reset(&handle->cursor, node);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_ts_tree_cursor_current_node(value cursor_v) {
  CAMLparam1(cursor_v);
  struct ts_tree_cursor_handle* handle = TreeCursor_val(cursor_v);
  if (!handle->valid) {
    caml_failwith("Tree_sitter.Tree_cursor.current_node: cursor is closed");
  }
  TSNode node = ts_tree_cursor_current_node(&handle->cursor);
  CAMLreturn(alloc_node(node));
}

CAMLprim value caml_ts_tree_cursor_current_field_name(value cursor_v) {
  CAMLparam1(cursor_v);
  CAMLlocal2(result, some);
  struct ts_tree_cursor_handle* handle = TreeCursor_val(cursor_v);
  if (!handle->valid) {
    caml_failwith(
        "Tree_sitter.Tree_cursor.current_field_name: cursor is closed");
  }
  const char* name = ts_tree_cursor_current_field_name(&handle->cursor);
  if (name == NULL) {
    CAMLreturn(Val_int(0));
  }
  result = caml_copy_string(name);
  some = caml_alloc(1, 0);
  Store_field(some, 0, result);
  CAMLreturn(some);
}

CAMLprim value caml_ts_tree_cursor_current_field_id(value cursor_v) {
  CAMLparam1(cursor_v);
  struct ts_tree_cursor_handle* handle = TreeCursor_val(cursor_v);
  if (!handle->valid) {
    caml_failwith("Tree_sitter.Tree_cursor.current_field_id: cursor is closed");
  }
  TSFieldId id = ts_tree_cursor_current_field_id(&handle->cursor);
  CAMLreturn(Val_int(id));
}

CAMLprim value caml_ts_tree_cursor_current_depth(value cursor_v) {
  CAMLparam1(cursor_v);
  struct ts_tree_cursor_handle* handle = TreeCursor_val(cursor_v);
  if (!handle->valid) {
    caml_failwith("Tree_sitter.Tree_cursor.current_depth: cursor is closed");
  }
  uint32_t depth = ts_tree_cursor_current_depth(&handle->cursor);
  CAMLreturn(Val_int(depth));
}

CAMLprim value caml_ts_tree_cursor_goto_parent(value cursor_v) {
  CAMLparam1(cursor_v);
  struct ts_tree_cursor_handle* handle = TreeCursor_val(cursor_v);
  if (!handle->valid) {
    caml_failwith("Tree_sitter.Tree_cursor.goto_parent: cursor is closed");
  }
  bool ok = ts_tree_cursor_goto_parent(&handle->cursor);
  CAMLreturn(Val_bool(ok));
}

CAMLprim value caml_ts_tree_cursor_goto_first_child(value cursor_v) {
  CAMLparam1(cursor_v);
  struct ts_tree_cursor_handle* handle = TreeCursor_val(cursor_v);
  if (!handle->valid) {
    caml_failwith("Tree_sitter.Tree_cursor.goto_first_child: cursor is closed");
  }
  bool ok = ts_tree_cursor_goto_first_child(&handle->cursor);
  CAMLreturn(Val_bool(ok));
}

CAMLprim value caml_ts_tree_cursor_goto_last_child(value cursor_v) {
  CAMLparam1(cursor_v);
  struct ts_tree_cursor_handle* handle = TreeCursor_val(cursor_v);
  if (!handle->valid) {
    caml_failwith("Tree_sitter.Tree_cursor.goto_last_child: cursor is closed");
  }
  bool ok = ts_tree_cursor_goto_last_child(&handle->cursor);
  CAMLreturn(Val_bool(ok));
}

CAMLprim value caml_ts_tree_cursor_goto_next_sibling(value cursor_v) {
  CAMLparam1(cursor_v);
  struct ts_tree_cursor_handle* handle = TreeCursor_val(cursor_v);
  if (!handle->valid) {
    caml_failwith(
        "Tree_sitter.Tree_cursor.goto_next_sibling: cursor is closed");
  }
  bool ok = ts_tree_cursor_goto_next_sibling(&handle->cursor);
  CAMLreturn(Val_bool(ok));
}

CAMLprim value caml_ts_tree_cursor_goto_previous_sibling(value cursor_v) {
  CAMLparam1(cursor_v);
  struct ts_tree_cursor_handle* handle = TreeCursor_val(cursor_v);
  if (!handle->valid) {
    caml_failwith(
        "Tree_sitter.Tree_cursor.goto_previous_sibling: cursor is closed");
  }
  bool ok = ts_tree_cursor_goto_previous_sibling(&handle->cursor);
  CAMLreturn(Val_bool(ok));
}

CAMLprim value caml_ts_tree_cursor_goto_first_child_for_byte(value cursor_v,
                                                             value byte_v) {
  CAMLparam2(cursor_v, byte_v);
  struct ts_tree_cursor_handle* handle = TreeCursor_val(cursor_v);
  if (!handle->valid) {
    caml_failwith(
        "Tree_sitter.Tree_cursor.goto_first_child_for_byte: cursor is closed");
  }
  uint32_t byte = (uint32_t)Int_val(byte_v);
  int64_t index =
      ts_tree_cursor_goto_first_child_for_byte(&handle->cursor, byte);
  CAMLreturn(Val_int((int)index));
}
