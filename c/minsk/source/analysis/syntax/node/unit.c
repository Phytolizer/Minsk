#include "minsk/analysis/syntax/node/unit.h"
#include "minsk/analysis/syntax/node.h"
#include "minsk/analysis/syntax/token.h"

void compilation_unit_syntax_init(
    compilation_unit_syntax_t* unit,
    expression_syntax_t* root,
    syntax_token_t end_of_file_token
) {
  unit->base.kind = syntax_kind_compilation_unit;
  unit->root = root;
  unit->end_of_file_token = end_of_file_token;
}

void compilation_unit_syntax_free(compilation_unit_syntax_t* unit) {
  syntax_node_free((syntax_node_t*)unit->root);
  token_free(&unit->end_of_file_token);
}
