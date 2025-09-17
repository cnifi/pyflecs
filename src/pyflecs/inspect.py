from ctypes import Structure
from typing import Callable

from .cflecs import (
    struct_ecs_iter_t,
    struct_ecs_query_desc_t,
    struct_ecs_system_desc_t,
    struct_ecs_table_record_t,
    struct_ecs_table_t,
    struct_ecs_term_ref_t,
    struct_ecs_term_t,
    struct_ecs_type_info_t,
)

SPACER = " " * 2

type ReprFunc = Callable[[type[Structure], int, dict], None]


def stringify_ecs_type_info(value: struct_ecs_type_info_t, depth=1):
    prefix = SPACER * depth
    return "\n".join(
        [
            f"{prefix}size={value.size}",
            f"{prefix}alignment={value.alignment}",
            f"{prefix}hooks={value.hooks}",
            f"{prefix}component={value.component}",
            f"{prefix}name={value.name}",
        ]
    )


def stringify_ecs_table_record_t(value: struct_ecs_table_record_t, depth=1):
    prefix = SPACER * depth
    return "\n".join(
        [
            f"{prefix}index={value.index}",
            f"{prefix}count={value.count}",
            f"{prefix}column={value.column}",
        ]
    )


def stringify_ecs_table_t(value: struct_ecs_table_t, depth=1):
    prefix = SPACER * depth
    return "\n".join(
        [
            f"{prefix}id={value.id}",
            f"{prefix}flags={value.flags}",
            f"{prefix}column_count={value.column_count}",
            f"{prefix}version={value.version}",
            f"{prefix}bloom_filter={value.bloom_filter}",
            f"{prefix}type={value.type}",
            f"{prefix}data={value.data}",
            f"{prefix}node={value.node}",
            f"{prefix}component_map={value.component_map}",
            f"{prefix}dirty_state={value.dirty_state}",
            f"{prefix}column_map={value.component_map}",
        ]
    )


def stringify_ecs_iter_t(value: struct_ecs_iter_t, depth=0):
    prefix = SPACER * depth
    return "\n".join(
        [
            f"{prefix}offset={value.offset}",
            f"{prefix}count={value.count}",
            f"{prefix}entities={value.entities}",
            f"{prefix}ptrs={value.ptrs}",
            f"{prefix}trs={value.trs}",
            f"{prefix}sizes={value.sizes}",
            f"{prefix}table={value.table}",
            f"{prefix}other_table={value.other_table}",
            f"{prefix}ids={value.ids}",
            f"{prefix}sources={value.sources}",
            f"{prefix}constrained_vars={value.constrained_vars}",
            f"{prefix}set_fields={value.set_fields}",
            f"{prefix}ref_fields={value.ref_fields}",
            f"{prefix}up_fields={value.up_fields}",
            f"{prefix}field_count={value.field_count}",
            f"{prefix}term_index={value.term_index}",
            f"{prefix}query={value.query}",
            f"{prefix}param={value.param}",
            f"{prefix}ctx={value.ctx}",
            f"{prefix}binding_ctx={value.binding_ctx}",
            f"{prefix}callback_ctx={value.callback_ctx}",
            f"{prefix}run_ctx={value.run_ctx}",
            f"{prefix}delta_time={value.delta_time}",
            f"{prefix}delta_system_time={value.delta_system_time}",
            f"{prefix}frame_offset={value.frame_offset}",
            f"{prefix}flags={value.flags}",
            f"{prefix}interrupted_by={value.interrupted_by}",
            f"{prefix}next={value.next}",
            f"{prefix}callback={value.callback}",
            f"{prefix}fini={value.fini}",
            f"{prefix}chain_it={value.chain_it}",
        ]
    )


def stringify_ecs_term_ref_t(value: struct_ecs_term_ref_t, depth=1):
    prefix = SPACER * depth
    return "\n".join(
        [
            f"{prefix}id={value.id}",
            f"{prefix}name={value.name if value.name else None}",
        ]
    )


def stringify_ecs_term_t(value: struct_ecs_term_t, depth=1):
    prefix = SPACER * depth
    return "\n".join(
        [
            f"{prefix}id={value.id}",
            f"{prefix}src=TermRef:\n{stringify_ecs_term_ref_t(value.src, depth + 1)}",
            f"{prefix}first=TermRef:\n{stringify_ecs_term_ref_t(value.first, depth + 1)}",
            f"{prefix}second=TermRef:\n{stringify_ecs_term_ref_t(value.second, depth + 1)}",
            f"{prefix}trav={value.trav}",
            f"{prefix}inout={value.inout}",
            f"{prefix}oper={value.oper}",
            f"{prefix}field_index={value.field_index}",
            # f"{prefix}flags_={value.flags}",
        ]
    )


def stringify_ecs_query_desc_t(
    value: struct_ecs_query_desc_t, depth=1, reprs={"ecs_term_t": stringify_ecs_term_t}
):
    prefix = SPACER * depth

    terms = "\n".join(
        [
            f"{prefix}Term:\n{reprs['ecs_term_t'](t, depth + 2)}"
            for t in filter(lambda t: t.id > 0, value.terms)
        ]
    )

    return "\n".join([f"{prefix}terms=[\n{terms}{prefix}]"])


def stringify_ecs_system_desc_t(
    value: struct_ecs_system_desc_t,
    depth=1,
    reprs: dict[str, Callable] = {"ecs_query_desc_t": stringify_ecs_query_desc_t},
):
    prefix = SPACER * depth
    return "\n".join(
        [
            f"{prefix}callback={value.callback}",
            f"{prefix}query=QueryDescription:\n{reprs['ecs_query_desc_t'](value.query, depth + 1)}",
            f"{prefix}run={value.run}",
            f"{prefix}ctx={value.ctx}",
            f"{prefix}ctx_free={value.ctx_free}",
            f"{prefix}callback_ctx={value.callback_ctx}",
            f"{prefix}callback_ctx_free={value.callback_ctx_free}",
            f"{prefix}run_ctx={value.run_ctx}",
            f"{prefix}run_ctx_free={value.run_ctx_free}",
            f"{prefix}interval={value.interval}",
            f"{prefix}rate={value.rate}",
            f"{prefix}tick_source={value.tick_source}",
            f"{prefix}multi_threaded={value.multi_threaded}",
            f"{prefix}immediate={value.immediate}",
        ]
    )
