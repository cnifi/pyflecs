from ctypes import c_void_p
from functools import wraps
from typing import Callable, Optional, final

from .adaptor import BoxedSystemDescription
from .cflecs import (
    # EcsOnUpdate,
    ecs_run_action_t,
    struct_ecs_system_desc_t,
)
from .inspect import stringify_ecs_system_desc_t

# from .pipeline import Phase
from .query import QueryDescription, QueryExecutor, QueryResult
from .types import ContextFreeAction, EntityId, Int32, Time

type IterateAction = Callable[[QueryExecutor], None]


def _iterate_action(func: IterateAction):
    """Decorator for a system action which is executed once per query match."""

    @ecs_run_action_t
    def wrapper(iterator):
        return func(QueryResult(iterator.contents, 0))

    return wrapper


type RunAction = Callable[[QueryExecutor], None]


def _run_action(func: RunAction):
    """Decorator for a system action which is executed once per tick."""

    @ecs_run_action_t
    def wrapper(iterator):
        return func(QueryExecutor(iterator.contents))

    return wrapper


class SystemDescriptionBuilder:
    def __init__(
        self,
        entity: EntityId | None = None,
        query: QueryDescription | None = None,
        callback: IterateAction | None = None,
        run: RunAction | None = None,
        ctx: Optional[c_void_p] = None,  # TODO
        ctx_free: Optional[ContextFreeAction] = None,
        callback_ctx: Optional[c_void_p] = None,  # TODO
        callback_ctx_free: Optional[ContextFreeAction] = None,
        run_ctx: Optional[c_void_p] = None,  # TODO
        run_ctx_free: Optional[ContextFreeAction] = None,
        interval: Optional[Time] = None,
        rate: Optional[Int32] = None,
        tick_source: Optional[EntityId] = None,
        multi_threaded: Optional[bool] = None,
        immediate: Optional[bool] = None,
    ):
        self._entity = entity
        self._query = query
        self._callback = callback
        self._run = run
        self._ctx = ctx
        self._ctx_free = ctx_free
        self._callback_ctx = callback_ctx
        self._callback_ctx_free = callback_ctx_free
        self._run_ctx = run_ctx
        self._run_ctx_free = run_ctx_free
        self._interval = interval
        self._rate = rate
        self._tick_source = tick_source
        self._multi_threaded = multi_threaded
        self._immediate = immediate

    def entity(self, entity: EntityId | None):
        self._entity = entity
        return self

    def query(self, query: QueryDescription | None):
        self._query = query
        return self

    def callback(self, callback: IterateAction | None):
        self._callback = callback
        return self

    def run(self, run: RunAction | None):
        self._run = run
        return self

    def ctx(self, ctx: c_void_p | None):
        self._ctx = ctx
        return self

    def ctx_free(self, ctx_free: ContextFreeAction | None):
        self._ctx_free = ctx_free
        return self

    def callback_ctx(self, callback_ctx: c_void_p | None):
        self._callback_ctx = callback_ctx
        return self

    def callback_ctx_free(self, callback_ctx_free: ContextFreeAction | None):
        self._callback_ctx_free = callback_ctx_free
        return self

    def run_ctx(self, run_ctx: c_void_p | None):
        self._run_ctx = run_ctx
        return self

    def run_ctx_free(self, run_ctx_free: ContextFreeAction | None):
        self._run_ctx_free = run_ctx_free
        return self

    def interval(self, interval: Int32 | None):
        self._interval = interval
        return self

    def rate(self, rate: Int32 | None):
        self._rate = rate
        return self

    def tick_source(self, tick_source: EntityId | None):
        self._tick_source = tick_source
        return self

    def multi_threaded(self, multi_threaded: bool | None):
        self._multi_threaded = multi_threaded
        return self

    def immediate(self, immediate: bool | None):
        self._immediate = immediate
        return self

    def build(self):
        d = struct_ecs_system_desc_t()
        # This happens automatically
        # system_desc._canary = 0
        if self._run is None and self._callback is None:
            raise Exception("SystemDescription must define one of [run, callback]")
        if self._entity is not None:
            d.entity = self._entity
        if self._query is not None:
            d.query = self._query._value
        if self._multi_threaded is not None:
            d.multi_threaded = self._multi_threaded
        if self._callback is not None:
            d.callback = _iterate_action(self._callback)
        if self._run is not None:
            d.run = _run_action(self._run)
        if self._ctx is not None:
            d.ctx = self._ctx
        if self._ctx_free is not None:
            d.ctx_free = self._ctx_free
        if self._callback_ctx is not None:
            d.callback_ctx = self._callback_ctx
        if self._callback_ctx_free is not None:
            d.callback_ctx_free = self._callback_ctx_free
        if self._run_ctx is not None:
            d.run_ctx = self._run_ctx
        if self._run_ctx_free is not None:
            d.run_ctx_free = self._run_ctx_free
        if self._interval is not None:
            d.interval = self._interval
        if self._rate is not None:
            d.rate = self._rate
        if self._tick_source is not None:
            d.tick_source = self._tick_source
        if self._multi_threaded is not None:
            d.multi_threaded = self._multi_threaded
        if self._immediate is not None:
            d.immediate = self._immediate
        return SystemDescription(d, self._query)


class SystemDescription:
    """Python wrapper around struct_system_desc_t."""

    @classmethod
    def builder(cls):
        return SystemDescriptionBuilder()

    @classmethod
    def kwargs(cls, **kwargs):
        return SystemDescriptionBuilder(**kwargs).build()

    def __init__(
        self,
        system_desc: BoxedSystemDescription,
        query: Optional[QueryDescription] = None,
    ):
        self._value = system_desc
        self._query = query

    def _resolve(self, world):
        if self._query is not None:
            self._query._resolve(world)
            # self._value.query.terms[0].id = self._query._value.terms[0].id
            self._value.query = self._query._value

    @property
    def callback(self):
        return self._value.callback

    @property
    def query(self) -> QueryDescription:
        return QueryDescription(self._value.query)

    @property
    def run(self):
        return self._value.run

    @property
    def ctx(self) -> c_void_p:
        return self._value.ctx

    @property
    def ctx_free(self) -> ContextFreeAction:
        return self._value.ctx_free

    @property
    def callback_ctx(self) -> c_void_p:
        return self._value.callback_ctx

    @property
    def callback_ctx_free(self) -> ContextFreeAction:
        return self._value.callback_ctx_free

    @property
    def run_ctx(self) -> c_void_p:
        return self._value.run_ctx

    @property
    def run_ctx_free(self) -> ContextFreeAction:
        return self._value.run_ctx_free

    @property
    def interval(self) -> Time:
        return self._value.interval

    @property
    def rate(self) -> int:
        return self._value.rate

    @property
    def tick_source(self) -> EntityId:
        return self._value.tick_source

    @property
    def multi_threaded(self) -> bool:
        return self._value.multi_threaded

    @property
    def immediate(self) -> bool:
        return self._value.immediate

    def __repr__(self):
        return "\n".join(
            ["SystemDescription:", stringify_ecs_system_desc_t(self._value)]
        )


class System:
    """Base class for all systems."""

    def __init__(self, world, description: SystemDescription):
        """System.__init__ should not be called by consumers. Call a World.system* factory method"""

        super().__init__()

        self._id = EntityId(0)
        self._world = world
        self.description = description

    @property
    def id(self):
        return self._id

    def each(self, executor):
        raise NotImplementedError()

    def run(self, executor):
        raise NotImplementedError()

    def __int__(self):
        return self._id


SystemType = type[System]


def system(
    entity: EntityId | None = None,
    query: QueryDescription | None = None,
    # ctx: Optional[c_void_p] = None,  # TODO
    # ctx_free: Optional[ContextFreeAction] = None,
    # callback_ctx: Optional[c_void_p] = None,  # TODO
    # callback_ctx_free: Optional[ContextFreeAction] = None,
    # run_ctx: Optional[c_void_p] = None,  # TODO
    # run_ctx_free: Optional[ContextFreeAction] = None,
    interval: Optional[Time] = None,
    rate: Optional[Int32] = None,
    tick_source: Optional[EntityId] = None,
    multi_threaded: Optional[bool] = None,
    immediate: Optional[bool] = True,
):
    @wraps(System)
    def decorator(cls: type):
        class _WrappedSystem(cls, System):
            def __init__(self, world):
                sdb = (
                    SystemDescriptionBuilder()
                    .entity(entity)
                    .query(query)
                    .interval(interval)
                    .rate(rate)
                    .tick_source(tick_source)
                    .multi_threaded(multi_threaded)
                    .immediate(immediate)
                )

                if hasattr(cls, "run"):
                    sdb.run(lambda *args: cls.run(self, *args))

                if hasattr(cls, "each"):
                    sdb.callback(lambda *args: cls.each(self, *args))

                sd = sdb.build()

                System.__init__(self, world, sd)
                cls.__init__(self)

        return _WrappedSystem

    return decorator
