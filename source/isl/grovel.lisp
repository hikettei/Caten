(in-package :caten/isl)

(include "isl/arg.h")

(cenum #.(swig-lispify "isl_arg_type" 'enumname)
       ((:arg-end "isl_arg_end"))
       ((:arg-alias "isl_arg_alias"))
       ((:arg-arg "isl_arg_arg"))
       ((:arg-bool "isl_arg_bool"))
       ((:arg-child "isl_arg_child"))
       ((:arg-choice "isl_arg_choice"))
       ((:arg-flags "isl_arg_flags"))
       ((:arg-footer "isl_arg_footer"))
       ((:arg-int "isl_arg_int"))
       ((:arg-user "isl_arg_user"))
       ((:arg-long "isl_arg_long"))
       ((:arg-ulong "isl_arg_ulong"))
       ((:arg-str "isl_arg_str"))
       ((:arg-str-list "isl_arg_str_list"))
       ((:arg-version "isl_arg_version")))

(include "isl/ast_type.h")

(cenum #.(swig-lispify "isl_ast_expr_op_type" 'enumname)
       ((:ast-expr-op-error "isl_ast_expr_op_error"))
       ((:ast-expr-op-and "isl_ast_expr_op_and"))
       ((:ast-expr-op-and-then "isl_ast_expr_op_and_then"))
       ((:ast-expr-op-or "isl_ast_expr_op_or"))
       ((:ast-expr-op-or-else "isl_ast_expr_op_or_else"))
       ((:ast-expr-op-max "isl_ast_expr_op_max"))
       ((:ast-expr-op-min "isl_ast_expr_op_min"))
       ((:ast-expr-op-minus "isl_ast_expr_op_minus"))
       ((:ast-expr-op-add "isl_ast_expr_op_add"))
       ((:ast-expr-op-sub "isl_ast_expr_op_sub"))
       ((:ast-expr-op-mul "isl_ast_expr_op_mul"))
       ((:ast-expr-op-div "isl_ast_expr_op_div"))
       ((:ast-expr-op-fdiv-q "isl_ast_expr_op_fdiv_q"))
       ((:ast-expr-op-pdiv-q "isl_ast_expr_op_pdiv_q"))
       ((:ast-expr-op-pdiv-r "isl_ast_expr_op_pdiv_r"))
       ((:ast-expr-op-zdiv-r "isl_ast_expr_op_zdiv_r"))
       ((:ast-expr-op-cond "isl_ast_expr_op_cond"))
       ((:ast-expr-op-select "isl_ast_expr_op_select"))
       ((:ast-expr-op-eq "isl_ast_expr_op_eq"))
       ((:ast-expr-op-le "isl_ast_expr_op_le"))
       ((:ast-expr-op-lt "isl_ast_expr_op_lt"))
       ((:ast-expr-op-ge "isl_ast_expr_op_ge"))
       ((:ast-expr-op-gt "isl_ast_expr_op_gt"))
       ((:ast-expr-op-call "isl_ast_expr_op_call"))
       ((:ast-expr-op-access "isl_ast_expr_op_access"))
       ((:ast-expr-op-member "isl_ast_expr_op_member"))
       ((:ast-expr-op-address-of "isl_ast_expr_op_address_of")))

(cenum #.(swig-lispify "isl_ast_expr_type" 'enumname)
       ((:ast-expr-error "isl_ast_expr_error"))
       ((:ast-expr-op "isl_ast_expr_op"))
       ((:ast-expr-id "isl_ast_expr_id"))
       ((:ast-expr-int "isl_ast_expr_int")))

(cenum #.(swig-lispify "isl_ast_node_type" 'enumname)
       ((:ast-node-error "isl_ast_node_error"))
       ((:ast-node-for "isl_ast_node_for"))
       ((:ast-node-if "isl_ast_node_if"))
       ((:ast-node-block "isl_ast_node_block"))
       ((:ast-node-mark "isl_ast_node_mark"))
       ((:ast-node-user "isl_ast_node_user")))

(cenum #.(swig-lispify "isl_ast_loop_type" 'enumname)
       ((:ast-loop-error "isl_ast_loop_error"))
       ((:ast-loop-default "isl_ast_loop_default"))
       ((:ast-loop-atomic "isl_ast_loop_atomic"))
       ((:ast-loop-unroll "isl_ast_loop_unroll"))
       ((:ast-loop-separate "isl_ast_loop_separate")))

(include "isl/ctx.h")

(cenum #.(swig-lispify "isl_error" 'enumname)
       ((:error-none "isl_error_none"))
       ((:error-abort "isl_error_abort"))
       ((:error-alloc "isl_error_alloc"))
       ((:error-unknown "isl_error_unknown"))
       ((:error-internal "isl_error_internal"))
       ((:error-invalid "isl_error_invalid"))
       ((:error-quota "isl_error_quota"))
       ((:error-unsupported "isl_error_unsupported")))

(cenum #.(swig-lispify "isl_stat" 'enumname)
       ((:stat-error "isl_stat_error"))
       ((:stat-ok "isl_stat_ok")))

(cenum #.(swig-lispify "isl_bool" 'enumname)
       ((:bool-error "isl_bool_error"))
       ((:bool-false "isl_bool_false"))
       ((:bool-true "isl_bool_true")))

(constant (#.(swig-lispify "isl_size_error" 'constant) "isl_size_error"))

(include "isl/lp.h")

(cenum #.(swig-lispify "isl_lp_result" 'enumname)
       ((:lp-error "isl_lp_error"))
       ((:lp-ok "isl_lp_ok"))
       ((:lp-unbounded "isl_lp_unbounded"))
       ((:lp-empty "isl_lp_empty")))

(include "isl/polynomial_type.h")

(cenum #.(swig-lispify "isl_fold" 'enumname)
       ((:fold-error "isl_fold_error"))
       ((:fold-min "isl_fold_min"))
       ((:fold-max "isl_fold_max"))
       ((:fold-list "isl_fold_list")))

(include "isl/schedule_type.h")

(cenum #.(swig-lispify "isl_schedule_node_type" 'enumname)
       ((:schedule-node-error "isl_schedule_node_error"))
       ((:schedule-node-band "isl_schedule_node_band"))
       ((:schedule-node-context "isl_schedule_node_context"))
       ((:schedule-node-domain "isl_schedule_node_domain"))
       ((:schedule-node-expansion "isl_schedule_node_expansion"))
       ((:schedule-node-extension "isl_schedule_node_extension"))
       ((:schedule-node-filter "isl_schedule_node_filter"))
       ((:schedule-node-leaf "isl_schedule_node_leaf"))
       ((:schedule-node-guard "isl_schedule_node_guard"))
       ((:schedule-node-mark "isl_schedule_node_mark"))
       ((:schedule-node-sequence "isl_schedule_node_sequence"))
       ((:schedule-node-set "isl_schedule_node_set")))

(include "isl/space_type.h")

(cenum #.(swig-lispify "isl_dim_type" 'enumname)
       ((:dim-cst "isl_dim_cst"))
       ((:dim-param "isl_dim_param"))
       ((:dim-in "isl_dim_in"))
       ((:dim-out "isl_dim_out"))
       ((:dim-set "isl_dim_set"))
       ((:dim-div "isl_dim_div"))
       ((:dim-all "isl_dim_all")))

(include "isl/stream.h")

(cenum #.(swig-lispify "isl_token_type" 'enumname)
       ((:token-error "ISL_TOKEN_ERROR"))
       ((:token-unknown "ISL_TOKEN_UNKNOWN"))
       ((:token-value "ISL_TOKEN_VALUE"))
       ((:token-ident "ISL_TOKEN_IDENT"))
       ((:token-ge "ISL_TOKEN_GE"))
       ((:token-le "ISL_TOKEN_LE"))
       ((:token-gt "ISL_TOKEN_GT"))
       ((:token-lt "ISL_TOKEN_LT"))
       ((:token-ne "ISL_TOKEN_NE"))
       ((:token-eq-eq "ISL_TOKEN_EQ_EQ"))
       ((:token-lex-ge "ISL_TOKEN_LEX_GE"))
       ((:token-lex-le "ISL_TOKEN_LEX_LE"))
       ((:token-lex-gt "ISL_TOKEN_LEX_GT"))
       ((:token-lex-lt "ISL_TOKEN_LEX_LT"))
       ((:token-to "ISL_TOKEN_TO"))
       ((:token-and "ISL_TOKEN_AND"))
       ((:token-or "ISL_TOKEN_OR"))
       ((:token-exists "ISL_TOKEN_EXISTS"))
       ((:token-not "ISL_TOKEN_NOT"))
       ((:token-def "ISL_TOKEN_DEF"))
       ((:token-infty "ISL_TOKEN_INFTY"))
       ((:token-nan "ISL_TOKEN_NAN"))
       ((:token-min "ISL_TOKEN_MIN"))
       ((:token-max "ISL_TOKEN_MAX"))
       ((:token-rat "ISL_TOKEN_RAT"))
       ((:token-true "ISL_TOKEN_TRUE"))
       ((:token-false "ISL_TOKEN_FALSE"))
       ((:token-ceild "ISL_TOKEN_CEILD"))
       ((:token-floord "ISL_TOKEN_FLOORD"))
       ((:token-mod "ISL_TOKEN_MOD"))
       ((:token-string "ISL_TOKEN_STRING"))
       ((:token-map "ISL_TOKEN_MAP"))
       ((:token-aff "ISL_TOKEN_AFF"))
       ((:token-ceil "ISL_TOKEN_CEIL"))
       ((:token-floor "ISL_TOKEN_FLOOR"))
       ((:token-implies "ISL_TOKEN_IMPLIES"))
       ((:token-last "ISL_TOKEN_LAST")))

(include "isl/options.h")

(constant (#.(swig-lispify "ISL_ON_ERROR_WARN" 'constant) "ISL_ON_ERROR_WARN"))
(constant (#.(swig-lispify "ISL_ON_ERROR_CONTINUE" 'constant) "ISL_ON_ERROR_CONTINUE"))
(constant (#.(swig-lispify "ISL_ON_ERROR_ABORT" 'constant) "ISL_ON_ERROR_ABORT"))
