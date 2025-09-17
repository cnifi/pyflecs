from __future__ import annotations

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
from typing import Callable

from .adaptor import BoxedQueryDesc, BoxedTerm, BoxedTermRef
from .cflecs import (
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
from .component import Component
from .inspect import (
    SPACER,
    stringify_ecs_iter_t,
    stringify_ecs_query_desc_t,
    stringify_ecs_term_ref_t,
    stringify_ecs_term_t,
)
from .types import ContextFreeAction, EntityId, Int8, Int32, UInt32

idof = id


class Ref:
    def resolve(self, world):
        raise NotImplementedError()


class TermRefBuilder:
    """Builds a pyflecs.TermRef"""

    def __init__(
        self,
        id: int | type[Component] = 0,
        name: str | None = None,
        refs=dict[str, type[Component]](),
    ):
        self._refs = refs
        self.id(id)
        self.name(name)

    def id(self, id: int | type[Component]):
        self._id = id
        return self

    def name(self, name: str | None):
        self._name = name
        return self

    def build(self):
        if self._id == 0 and self._name is None:
            raise Exception("One of [id, name] must be set to build TermRef")
        tr = BoxedTermRef()
        tr.id = self._id
        tr.name = self._name
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

    @classmethod
    def value(cls, value: int | type[Component] | str | None):
        match value:
            case int() | Component():
                return TermRefBuilder(id=value).build()
            case str():
                return TermRefBuilder(name=value).build()
            case _:
                return None

    @classmethod
    def copy(cls, other: TermRef):
        return TermRefBuilder(id=other.id, name=other.name).build()

    def __init__(self, value: BoxedTermRef, refs=dict[str, type[Component]]()):
        self._value = value
        self._refs = refs

    def resolve(self, world):
        """Ensures this object's unbound component refs are given ids."""

        if "id" in self._refs:
            self._value.id = world.fidof(self._refs["id"])

    # def resolved(self):
    #     return len(self._refs) < 0

    @property
    def id(self):
        if hasattr(self._refs, "id"):
            return self._refs["id"]
        else:
            return int(self._value.id)

    @property
    def name(self):
        return self._value.name if self._value.name else None

    def repr(self, depth=0):
        prefix = SPACER * depth
        return "\n".join(
            [
                f"{prefix}id={self._value.id}",
                f"{prefix}name={self._value.name if self._value.name else None}",
                f"{prefix}refs={self._refs}",
            ]
        )

    def __repr__(self):
        return "\n".join(["TermRef:", self.repr(depth=1)])


class TermBuilder:
    """Builds a pyflecs.Term"""

    def __init__(
        self,
        id: int | type[Component] = 0,
        src: TermRef | None = None,
        first: TermRef | None = None,
        second: TermRef | None = None,
        refs=dict[int, type[Component]](),
    ):
        self._refs = refs
        self.id(id)
        self.src(src)
        self.first(first)
        self.second(second)

    def id(self, id: int | type[Component] = 0):
        match id:
            case int():
                self._id = id
            case type() if issubclass(id, Component):
                self._id = 0
                self._refs["id"] = id
        return self

    def src(self, src: int | str | type[Component] | TermRef | None):
        self._src = src if isinstance(src, TermRef) else TermRef.value(src)
        return self

    def first(self, first: int | str | type[Component] | TermRef | None):
        self._first = first if isinstance(first, TermRef) else TermRef.value(first)
        return self

    def second(self, second: int | str | type[Component] | TermRef | None):
        self._second = second if isinstance(second, TermRef) else TermRef.value(second)
        return self

    def build(self):
        t = BoxedTerm()
        t.id = self._id
        if self._src is not None:
            t.src = self._src._value
        if self._first is not None:
            t.first = self._first._value
        if self._second is not None:
            t.second = self._second._value
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
    def copy(cls, other):
        return TermBuilder(
            id=other.id, src=other.src, first=other.first, second=other.second
        ).build()

    @classmethod
    def tuple(cls, terms: tuple):
        return TermBuilder(*terms).build()

    def __init__(self, term: BoxedTerm, refs=dict[str, type[Component]]()):
        self._value = term
        self._refs = refs

    def resolve(self, world):
        self._value
        if "id" in self._refs:
            self._value.id = world.fidof(self._refs["id"])
        self.src.resolve(world)
        self.first.resolve(world)
        self.second.resolve(world)

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

    def repr(self, depth=1):
        prefix = SPACER * depth
        return "\n".join(
            [
                f"{prefix}id={self._value.id}",
                f"{prefix}src=TermRef:\n{TermRef(self._value.src).repr(depth + 1)}",
                f"{prefix}first=TermRef:\n{TermRef(self._value.first).repr(depth + 1)}",
                f"{prefix}second=TermRef:\n{TermRef(self._value.second).repr(depth + 1)}",
                f"{prefix}trav={self._value.trav}",
                f"{prefix}inout={self._value.inout}",
                f"{prefix}oper={self._value.oper}",
                f"{prefix}field_index={self._value.field_index}",
                f"{prefix}refs={self._refs}",
            ]
        )

    def __repr__(self):
        return "\n".join(["Term:", self.repr()])


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
        cache_kind: CacheKind | None = None,
        flags=0,
        entity=0,
        order_by=0,
        group_by=0,
        order_by_callback: OrderBy | None = None,
        group_by_callback: GroupBy | None = None,
        on_group_create: OnGroupCreate | None = None,
        on_group_delete: OnGroupDelete | None = None,
        group_by_ctx: c_void_p | None = None,
        group_by_ctx_free: ContextFreeAction | None = None,
        ctx=None,
        ctx_free=None,
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

    def terms(self, terms: list[Term]):
        self._terms = terms

    def cache_kind(self, cache_kind: CacheKind):
        self._cache_kind = cache_kind
        return self

    def entity(self, entity: EntityId):
        self._entity = entity
        return self

    def flags(self, flags: int):
        self._flags = flags
        return self

    def order_by(self, order_by: EntityId):
        self._order_by = order_by
        return self

    def group_by(self, group_by: EntityId):
        self._group_by = group_by
        return self

    def order_by_callback(self, order_by_callback: OrderBy):
        self._order_by_callback = order_by_callback
        return self

    def group_by_callback(self, group_by_callback: GroupBy):
        self._group_by_callback = group_by_callback
        return self

    def on_group_create(self, on_group_create: OnGroupCreate):
        self._on_group_create = on_group_create
        return self

    def on_group_delete(self, on_group_delete: OnGroupDelete):
        self._on_group_delete = on_group_delete
        return self

    def group_by_ctx(self, group_by_ctx: c_void_p):
        self._group_by_ctx = group_by_ctx

    def group_by_ctx_free(self, group_by_ctx_free):
        self._group_by_ctx_free = group_by_ctx_free
        return self

    def ctx(self, ctx: c_void_p):
        self._ctx = ctx
        return self

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
            d.flags = UInt32(self._flags)
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

    @classmethod
    def copy(cls, other):
        return QueryDescriptionBuilder(
            terms=[Term.copy(t) for t in other.terms],
            cache_kind=other.cache_kind,
            flags=other.flags,
            entity=other.entity,
            order_by=other.order_by,
            group_by=other.group_by,
            order_by_callback=other.order_by_callback,
            group_by_callback=other.group_by_callback,
            on_group_create=other.on_group_create,
            on_group_delete=other.on_group_delete,
            group_by_ctx=other.group_by_ctx,
            group_by_ctx_free=other.group_by_ctx_free,
            ctx=other.ctx,
            ctx_free=other.ctx_free,
        ).build()

    def __init__(self, desc: BoxedQueryDesc, terms: list[Term] = []):
        self._value = desc
        self._terms = terms

    def resolve(self, world):
        for i, t in enumerate(self._terms):
            t.resolve(world)

            self._value.terms[i] = Term.copy(t)._value

    @property
    def terms(self):
        return [Term(t) for t in filter(lambda t: t.id > 0, self._value.terms)]

    @property
    def cache_kind(self):
        # TODO: Will this work?
        return CacheKind(self._value.cache_kind)

    @property
    def flags(self):
        return int(self._value.flags)

    @property
    def entity(self):
        return int(self._value.entity)

    @property
    def order_by(self):
        return int(self._value.order_by)

    @property
    def group_by(self):
        return int(self._value.group_by)

    @property
    def order_by_callback(self):
        return self._value.order_by_callback

    @property
    def group_by_callback(self):
        return self._value.group_by_callback

    @property
    def on_group_create(self):
        return self._value.on_group_create

    @property
    def on_group_delete(self):
        return self._value.on_group_delete

    @property
    def group_by_ctx(self):
        return self._value.group_by_ctx

    @property
    def group_by_ctx_free(self):
        return self._value.group_by_ctx_free

    @property
    def ctx(self):
        return self._value.ctx

    @property
    def ctx_free(self):
        return self._value.ctx_free

    def repr(
        self,
        depth=0,
    ):
        prefix = SPACER * depth
        return "\n".join(
            [
                f"{prefix}terms=[",
                *[t.repr(depth=depth + 1) for t in self._terms],
                f"{prefix}]\n",
            ]
        )

    def __repr__(self):
        return "\n".join(
            [
                "QueryDescription:",
                self.repr(depth=1),
            ]
        )


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

    def component[T: type[Component]](self, index: int, cls: T) -> T:
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

        desc.resolve(world)  # Resolve pyflecs refs

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
