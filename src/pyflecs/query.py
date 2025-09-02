from __future__ import annotations  # Required for runtime typing

from ctypes import (
    CFUNCTYPE,
    POINTER,
    _Pointer,
    byref,
    c_uint64,
    c_void_p,
    pointer,
    py_object,
    sizeof,
)
from ctypes import (
    cast as ccast,
)
from enum import Enum
from typing import Callable, Optional

# from .world import World
from .adaptor import BoxedQueryDesc, BoxedTerm, BoxedTermRef
from .cflecs import (  # EcsQueryCacheNone, ecs_field_w_size, ecs_id_t, ecs_query_init,
    FLECS_TERM_COUNT_MAX,
    EcsAcyclic,
    EcsAlias,
    EcsAny,
    EcsCanToggle,
    EcsChildOf,
    EcsDelete,
    EcsDependsOn,
    EcsDisabled,
    EcsDontFragment,
    EcsDontInherit,
    EcsEmpty,
    EcsExclusive,
    EcsFinal,
    EcsFlecs,
    EcsFlecsCore,
    EcsInherit,
    EcsInheritable,
    EcsIsA,
    EcsModule,
    EcsMonitor,
    EcsName,
    EcsNotQueryable,
    EcsOnAdd,
    EcsOnDelete,
    EcsOnDeleteTarget,
    EcsOneOf,
    EcsOnInstantiate,
    EcsOnRemove,
    EcsOnSet,
    EcsOnStart,
    EcsOnTableCreate,
    EcsOnTableDelete,
    EcsOrderedChildren,
    EcsOverride,
    EcsPairIsTag,
    EcsPanic,
    EcsPredEq,
    EcsPredLookup,
    EcsPredMatch,
    EcsPrefab,
    EcsPreFrame,
    EcsPrivate,
    # EcsQuery,
    EcsQueryCacheAll,
    EcsQueryCacheAuto,
    EcsQueryCacheDefault,
    EcsQueryCacheNone,
    EcsReflexive,
    EcsRelationship,
    EcsRemove,
    EcsScopeClose,
    EcsScopeOpen,
    EcsSlotOf,
    EcsSparse,
    EcsSymbol,
    EcsSymmetric,
    EcsTarget,
    EcsThis,
    EcsTrait,
    EcsTransitive,
    EcsTraversable,
    EcsVariable,
    EcsWildcard,
    EcsWith,
    EcsWorld,
    ecs_field_w_size,
    ecs_id_t,
    ecs_query_init,
    ecs_query_iter,
    ecs_query_next,
    ecs_term_t,
    struct_ecs_iter_t,
    struct_ecs_query_t,
    struct_ecs_table_t,
    struct_ecs_world_t,
)
from .component import ComponentType, is_component
from .inspect import (
    stringify_ecs_iter_t,
    stringify_ecs_query_desc_t,
    stringify_ecs_term_t,
)
from .types import ContextFreeAction, EntityId, Int8, Int32

UINT64_MAX_INT = 18446744073709551615


class TermRefBuilder:
    """Builds a pyflecs.TermRef"""

    def __init__(self, value: Optional[int | str | ComponentType] = None):
        self._value = value
        self._refs = dict[str, int]()

    @property
    def value(self):
        return self._value

    @value.setter
    def value(self, value):
        self._value = value
        if value is ComponentType:
            self._refs["value"] = id(value)
        return self

    def build(self):
        tr = BoxedTermRef()
        if type(self._value) is int:
            tr.id = ecs_id_t(self._value)
        elif type(self._value) is str:
            tr.name = self._value
        return TermRef(tr, self._refs)


class TermRef:
    """One ref in a pyflecs.QueryTerm"""

    @classmethod
    def builder(cls):
        return TermRefBuilder()

    @classmethod
    def kwargs(cls, **kwargs):
        return TermRefBuilder(**kwargs)

    @classmethod
    def tuple(cls, tup: tuple):
        return TermRefBuilder(*tup)

    def __init__(self, term_ref: BoxedTermRef, refs=dict[str, int]()):
        self._value = term_ref
        self._refs = refs

    def _resolve_id(self, world):
        return world.idof(self._refs["id"])

    def _resolve(self, world):
        self._value.id = self._resolve_id(world)

    @property
    def id(self):
        if hasattr(self._refs, "id"):
            return self._refs["id"]
        else:
            return int(self._value.id)

    @property
    def name(self):
        return self._value.name if self._value.name else None


class TermBuilder:
    """Builds a pyflecs.Term"""

    def __init__(
        self,
        id: Optional[int | ComponentType] = None,
        src: Optional[str | ComponentType] = None,
        first: Optional[str | ComponentType] = None,
        second: Optional[str | ComponentType] = None,
    ):
        self._refs = dict[str, int]()
        self._src = TermRefBuilder()
        self._first = TermRefBuilder()
        self._second = TermRefBuilder()
        self.id = id if id is not callable(id) else None  # :-<
        self.src = src
        self.first = first
        self.second = second

    @property
    def id(self):
        return self._id

    @id.setter
    def id(self, id_: Optional[int | ComponentType] = None):
        self._id = id_
        if is_component(id_):
            self._refs["id"] = id(id_)
        return self

    @property
    def src(self):
        return self._src

    @src.setter
    def src(self, src: Optional[int | str | ComponentType]):
        self.src.value = src
        return self

    @property
    def first(self):
        return self._first

    @first.setter
    def first(self, first: Optional[int | str | ComponentType]):
        self.first.value = first
        return self

    @property
    def second(self):
        return self._second

    @second.setter
    def second(self, second: Optional[str | ComponentType]):
        self.second.value = second
        return self

    def build(self):
        t = BoxedTerm()
        if self._id and type(self._id) is int:
            t.id = self._id
        if self._src:
            t.src = self._src.build()._value
        if self._first:
            t.first = self._first.build()._value
        if self._second:
            t.second = self._second.build()._value
        return Term(t, self._refs)


class Term:
    """One term in a pyflecs.Query"""

    @classmethod
    def builder(cls, world):
        return TermBuilder(world)

    @classmethod
    def kwargs(cls, **kwargs):
        return TermBuilder(**kwargs).build()

    @classmethod
    def tuple(cls, terms: tuple):
        return TermBuilder(*terms).build()

    def __init__(self, term: BoxedTerm, refs=dict[str, int]()):
        self._value = term
        self._refs = refs

    def _resolve(self, world):
        for k, v in self._refs.items():
            self._value.__setattr__(k, world.fidof(v))

    @property
    def id(self):
        if hasattr(self._refs, "id"):
            return self._refs.id
        else:
            return int(self._value.id)

    @property
    def src(self):
        return TermRef(self._value.src)

    @property
    def first(self):
        return TermRef(self._value.first)

    @property
    def second(self):
        return TermRef(self._value.second)

    @property
    def trav(self):
        return int(self._value.trav)

    @property
    def inout(self):
        return int(self._value.inout)

    @property
    def oper(self):
        return int(self._value.oper)

    @property
    def field_index(self):
        return int(self._value.field_index)

    def __repr__(self):
        return "\n".join(
            [
                "Term:",
                stringify_ecs_term_t(self._value),
                f"  refs={self._refs}",
            ]
        )


class CacheKind(Enum):
    DEFAULT = EcsQueryCacheDefault
    AUTO = EcsQueryCacheAuto
    ALL = EcsQueryCacheAll
    NONE = EcsQueryCacheNone


class QueryFlags(Enum):
    FLECS = EcsFlecs
    FLECS_CORE = EcsFlecsCore
    WORLD = EcsWorld
    WILDCARD = EcsWildcard
    ANY = EcsAny
    THIS = EcsThis
    VARIABLE = EcsVariable
    TRANSITIVE = EcsTransitive
    REFLEXIVE = EcsReflexive
    FINAL = EcsFinal
    INHERITABLE = EcsInheritable
    ON_INSTANTIATE = EcsOnInstantiate
    OVERRIDE = EcsOverride
    INHERIT = EcsInherit
    DONT_INHERIT = EcsDontInherit
    SYMMETRIC = EcsSymmetric
    EXCLUSIVE = EcsExclusive
    ACYCLIC = EcsAcyclic
    TRAVERSABLE = EcsTraversable
    WITH = EcsWith
    ONE_OF = EcsOneOf
    CAN_TOGGLE = EcsCanToggle
    TRAIT = EcsTrait
    RELATIONSHIP = EcsRelationship
    TARGET = EcsTarget
    PAIR_IS_TAG = EcsPairIsTag
    NAME = EcsName
    SYMBOL = EcsSymbol
    ALIAS = EcsAlias
    CHILD_OF = EcsChildOf
    IS_A = EcsIsA
    DEPENDS_ON = EcsDependsOn
    SLOT_OF = EcsSlotOf
    ORDERED_CHILDREN = EcsOrderedChildren
    MODULE = EcsModule
    PRIVATE = EcsPrivate
    PREFAB = EcsPrefab
    DISABLED = EcsDisabled
    NOT_QUERYABLE = EcsNotQueryable
    ON_ADD = EcsOnAdd
    ON_REMOVE = EcsOnRemove
    ON_SET = EcsOnSet
    MONITOR = EcsMonitor
    ON_TABLE_CREATE = EcsOnTableCreate
    ON_TABLE_DELETE = EcsOnTableDelete
    ON_DELETE = EcsOnDelete
    ON_DELETE_TARGET = EcsOnDeleteTarget
    REMOVE = EcsRemove
    DELETE = EcsDelete
    PANIC = EcsPanic
    SPARSE = EcsSparse
    DONT_FRAGMENT = EcsDontFragment
    PRED_EQ = EcsPredEq
    PRED_MATCH = EcsPredMatch
    PRED_LOOKUP = EcsPredLookup
    SCOPE_OPEN = EcsScopeOpen
    SCOPE_CLOSE = EcsScopeClose
    EMPTY = EcsEmpty
    ON_START = EcsOnStart
    PRE_FRAME = EcsPreFrame
    # WILDCARD = cflecs.EcsWildcard
    # QUERY = EcsQuery


# ecs_world_t *world, ecs_table_t *table, ecs_id_t group_id, void *ctx
GroupBy = Callable[[object, object, EntityId], EntityId]


def group_by(func: GroupBy):
    """Decorator for a groupby callback function."""

    _cfunctype = CFUNCTYPE(
        ecs_id_t,
        POINTER(struct_ecs_world_t),
        POINTER(struct_ecs_table_t),
        ecs_id_t,
        c_void_p,
    )

    @_cfunctype
    def wrapper(
        world: _Pointer[struct_ecs_world_t],
        table: _Pointer[struct_ecs_table_t],
        id: c_uint64,
        ctx: c_void_p,
    ):
        # TODO
        # return func(World._world(int(world)), table, id)
        return None

    return wrapper


# ecs_entity_t e1, const void *ptr1, ecs_entity_t e2, const void *ptr2
OrderBy = Callable[[EntityId, c_void_p, EntityId, c_void_p], int]


def order_by(func: OrderBy):
    """Decorator for a orderby callback function."""

    _cfunctype = CFUNCTYPE(Int32, EntityId, c_void_p, EntityId, c_void_p)

    @_cfunctype
    def wrapper(e1: EntityId, d1: c_void_p, e2: EntityId, d2: c_void_p):
        # TODO
        return func(e1, d1, e2, d2)


# ecs_world_t *world, uint64_t group_id, void *group_by_ctx
OnGroupCreate = Callable[[object, EntityId, c_void_p], c_void_p]


def on_group_create(func: OnGroupCreate):
    """Decorator for group create action."""

    _cfunctype = CFUNCTYPE(c_void_p, _Pointer[py_object], EntityId, c_void_p)

    @_cfunctype
    def wrapper(world, group_id: EntityId, group_by_ctx: c_void_p):
        return func(world, group_id, group_by_ctx)

    return func


# ecs_world_t *world, uint64_t group_id, void *group_ctx, void *group_by_ctx
OnGroupDelete = Callable[[object, EntityId, c_void_p, c_void_p], c_void_p]

# TODO
# void *ctx
# ContextFreeCallback = Callable[[c_void_p], None]


class QueryDescriptionBuilder:
    """Builds a pyflecs.QueryDescription"""

    def __init__(
        self,
        terms: list[Term] = [],
        cache_kind: Optional[CacheKind] = None,
        flags: Optional[QueryFlags] = None,
        entity: Optional[int] = None,
        order_by: Optional[int] = None,
        group_by: Optional[int] = None,
        order_by_callback: Optional[OrderBy] = None,
        group_by_callback: Optional[GroupBy] = None,
        on_group_create: Optional[OnGroupCreate] = None,
        on_group_delete: Optional[OnGroupDelete] = None,
        group_by_ctx: Optional[c_void_p] = None,
        group_by_ctx_free: Optional[ContextFreeAction] = None,
        ctx=None,  # TODO
        ctx_free=None,  # TODO
    ):
        self._terms = terms
        self._cache_kind = cache_kind
        self._entity = entity
        self._flags = flags
        self._order_by = order_by
        self._group_by = group_by
        self._order_by_callback = order_by_callback
        self._group_by_callback = group_by_callback
        self._on_group_create = on_group_create
        self._on_group_delete = on_group_delete
        self._group_by_ctx = group_by_ctx
        self._group_by_ctx_free = group_by_ctx_free
        self._ctx = ctx
        self._ctx_free = ctx_free

    @property
    def terms(self):
        return self._terms

    @terms.setter
    def terms(self, terms: list[Term]):
        self._terms = terms

    @property
    def cache_kind(self):
        return self._cache_kind

    @cache_kind.setter
    def cache_kind(self, cache_kind: CacheKind):
        self._cache_kind = cache_kind
        return self

    @property
    def entity(self):
        return self._entity

    @entity.setter
    def entity(self, entity: EntityId):
        self._entity = entity
        return self

    @property
    def order_by(self):
        return self._order_by

    @order_by.setter
    def order_by(self, order_by: EntityId):
        self._order_by = order_by
        return self

    @property
    def group_by(self):
        return self._group_by

    @group_by.setter
    def group_by(self, group_by: EntityId):
        self._group_by = group_by
        return self

    @property
    def order_by_callback(self):
        return self._order_by_callback

    @order_by_callback.setter
    def order_by_callback(self, order_by_callback: OrderBy):
        self._order_by_callback = order_by_callback
        return self

    @property
    def group_by_callback(self):
        return self._group_by_callback

    @group_by_callback.setter
    def group_by_callback(self, group_by_callback: GroupBy):
        self._group_by_callback = group_by_callback
        return self

    @property
    def on_group_create(self):
        return self._on_group_create

    @on_group_create.setter
    def on_group_create(self, on_group_create: OnGroupCreate):
        self._on_group_create = on_group_create
        return self

    @property
    def on_group_delete(self):
        return self._on_group_delete

    @on_group_delete.setter
    def on_group_delete(self, on_group_delete: OnGroupDelete):
        self._on_group_delete = on_group_delete
        return self

    @property
    def group_by_ctx(self):
        return self._group_by_ctx

    @group_by_ctx.setter
    def group_by_ctx(self, group_by_ctx: c_void_p):
        self._group_by_ctx = group_by_ctx

    @property
    def group_by_ctx_free(self):
        return self._group_by_ctx_free

    # TODO
    @group_by_ctx_free.setter
    def group_by_ctx_free(self, group_by_ctx_free):
        self._group_by_ctx_free = group_by_ctx_free
        return self

    @property
    def ctx(self):
        return self._ctx

    @ctx.setter
    def ctx(self, ctx: c_void_p):
        self._ctx = ctx
        return self

    @property
    def ctx_free(self):
        return self._ctx_free

    # TODO
    @ctx_free.setter
    def ctx_free(self, _ctx_free):
        self._ctx_free = _ctx_free
        return self

    def build(self):
        d = BoxedQueryDesc()

        if not self._terms:
            raise Exception("query_desc is required to construct a QueryDescription")

        # TODO: It appears we have to allocate the full capacity here...
        d.terms = (ecs_term_t * FLECS_TERM_COUNT_MAX)(*[t._value for t in self._terms])
        # d.terms = (ecs_term_t * len(self._terms))(*[t._value for t in self._terms])

        if self._cache_kind is not None:
            # TODO: Will this work
            d.cache_kind = self._cache_kind.value
        if self._entity is not None:
            d.entity = self._entity
        if self._flags is not None:
            d.flags = self._flags
        # TODO
        # descriptor.expr = const char* (optional)
        if self._order_by_callback is not None:
            d.order_by_callback = self._order_by_callback
        if self._order_by is not None:
            d.order_by = self._order_by
        if self._group_by is not None:
            d.group_by = self._group_by
        if self._group_by_callback is not None:
            d.group_by_callback = self._group_by_callback
        # TODO
        # descriptor.order_by_table_callback = ecs_sort_table_action_t
        if self._on_group_create is not None:
            d.on_group_create = self._on_group_create
        if self._on_group_delete is not None:
            d.on_group_delete = self._on_group_delete
        if self._group_by_ctx is not None:
            d.group_by_ctx = self._group_by_ctx
        if self._group_by_ctx_free is not None:
            d.group_by_ctx_free = self._group_by_ctx_free
        if self._ctx is not None:
            d.ctx = self._ctx
        if self._ctx_free is not None:
            d.ctx_free = self._ctx_free

        qd = QueryDescription(d, self._terms)

        # Return reference for python wrapper object
        d.binding_ctx = ccast(pointer(py_object(qd)), c_void_p)
        # TODO
        # query_desc.binding_ctx_free = lambda: query_desc.release()

        return qd


class QueryDescription:
    """A template for generating pyflecs.Query objects"""

    @classmethod
    def tuple(cls, tup: list[tuple]):
        return QueryDescriptionBuilder(terms=[Term.tuple(t) for t in tup]).build()

    @classmethod
    def kwargs(cls, **kwargs):
        return QueryDescriptionBuilder(**kwargs).build()

    def __init__(self, desc: BoxedQueryDesc, terms: list[Term] = []):
        self._value = desc
        self._terms = terms

    def _resolve(self, world):
        for i, t in enumerate(self._terms):
            t._resolve(world)
            self._value.terms[i].id = t.id
            if t.src is not None:
                if t.src.id is not None:
                    self._value.terms[i].src.id = t.src.id
                if t.src.name is not None:
                    self._value.terms[i].src.name = t.src.name
            if t.first is not None:
                if t.first.id is not None:
                    self._value.terms[i].first.id = t.first.id
                if t.first.name is not None:
                    self._value.terms[i].first.name = t.first.name
            if t.second is not None:
                if t.second.id is not None:
                    self._value.terms[i].second.id = t.second.id
                if t.second.name is not None:
                    self._value.terms[i].second.name = t.second.name

    @property
    def terms(self):
        return [Term(t) for t in filter(lambda t: t.id > 0, self._value.terms)]

    @property
    def cache_kind(self):
        # TODO: Will this work?
        return CacheKind(self._value.cache_kind)

    def __repr__(self):
        return f"QueryDescription: terms=\n{stringify_ecs_query_desc_t(self._value)}"


class QueryExecutor:
    """Python wrapper around struct_ecs_query_iter. Provides iterative access to query results."""

    def __init__(self, iter: struct_ecs_iter_t):
        self._iter = iter

    def __str__(self):
        return stringify_ecs_iter_t(self._iter)

    def __iter__(self):
        return self

    def __next__(self):
        if not ecs_query_next(byref(self._iter)):
            raise StopIteration()
        return QueryResult(self._iter, self._iter.frame_offset)


class QueryResult:
    """Represents one query result from a pyflecs.Query"""

    def __init__(self, iterator: struct_ecs_iter_t, index: int):
        self._value = iterator
        self._index = index

    def entity(self):
        if not self._value.entities:
            raise Exception("May not access entity on this query result")
        return self._value.entities[self._index]

    def component[T: ComponentType](self, index: int, cls: T) -> T:
        return ccast(
            ecs_field_w_size(byref(self._value), sizeof(cls), Int8(index)),
            POINTER(cls),  # type: ignore
        ).contents

    def __repr__(self):
        return stringify_ecs_iter_t(self._value)


class Query:
    """Python wrapper around struct_ecs_query_t."""

    @classmethod
    def kwargs(cls, world, **kwargs):
        return Query(world, QueryDescriptionBuilder(**kwargs).build())

    @classmethod
    def tuple(cls, world, terms: tuple):
        return Query(world, QueryDescription.tuple(*terms))

    def __init__(self, world, desc: QueryDescription):
        self._world = world

        desc._resolve(world)  # Resolve pyflecs refs

        self._value: _Pointer[struct_ecs_query_t] = ecs_query_init(
            world._value, byref(desc._value)
        )

    @property
    def terms(self):
        return self._value.contents.terms

    @property
    def expr(self):
        return self._value.contents.expr

    @property
    def cache_kind(self):
        return self._value.contents.cache_kind

    @property
    def flags(self):
        return self._value.contents.flags

    @property
    def order_by_callback(self):
        return self._value.contents.order_by_callback

    def executor(self):
        return QueryExecutor(ecs_query_iter(self._world._value, self._value))

    def __iter__(self):
        return self.executor()
