(in-package :caten/isl)
;; Common Lisp version of https://github.com/inducer/isl/blob/master/codegen.c
(include "isl/ast.h")
(include "isl/ast_build.h")
(include "isl/options.h")
(include "isl/space.h")
(include "isl/set.h")
(include "isl/union_set.h")
(include "isl/union_map.h")
(include "isl/stream.h")
(include "isl/schedule.h")
(include "isl/schedule_node.h")


(c "
isl_schedule_node *node_set_options(isl_schedule_node *node, void *user);
isl_schedule_node *node_set_options(isl_schedule_node *node, void *user)
{
	enum isl_ast_loop_type *type = user;
	int i;
	isl_size n;

	if (isl_schedule_node_get_type(node) != isl_schedule_node_band)
		return node;

	n = isl_schedule_node_band_n_member(node);
	if (n < 0)
		return isl_schedule_node_free(node);
	for (i = 0; i < n; ++i)
		node = isl_schedule_node_band_member_set_ast_loop_type(node, i, *type);
	return node;
}

__isl_give isl_schedule *isl_schedule_map_schedule_node_bottom_up(
	__isl_take isl_schedule *schedule,
	__isl_give isl_schedule_node *(*fn)(
		__isl_take isl_schedule_node *node, void *user), void *user);
__isl_give isl_schedule *isl_schedule_map_schedule_node_bottom_up(
	__isl_take isl_schedule *schedule,
	__isl_give isl_schedule_node *(*fn)(
		__isl_take isl_schedule_node *node, void *user), void *user)
{
	isl_schedule_node *node;

	node = isl_schedule_get_root(schedule);
	isl_schedule_free(schedule);

	node = isl_schedule_node_map_descendant_bottom_up(node, fn, user);
	schedule = isl_schedule_node_get_schedule(node);
	isl_schedule_node_free(node);

	return schedule;
}

/*
 separate-or-atomic = 0 -> atomic
 separate-or-atomic = 1 -> separate
*/
__isl_give isl_schedule *schedule_set_options(__isl_take isl_schedule *schedule, int option);
__isl_give isl_schedule *schedule_set_options(
	__isl_take isl_schedule *schedule, int option)
{
	if (!(option == 0) && !(option == 1)) return schedule;
        enum isl_ast_loop_type type;
	type = option == 1 ? isl_ast_loop_separate : isl_ast_loop_atomic;
	schedule = isl_schedule_map_schedule_node_bottom_up(schedule, &node_set_options, &type);
	return schedule;
}
")
