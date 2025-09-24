from ctypes import c_void_p
from typing import Callable

from .adaptor import BoxedSystemDescription
from .cflecs import (
    # EcsOnUpdate,
    ecs_run_action_t,
    struct_ecs_system_desc_t,
)
from .entity import EntityId
from .inspect import SPACER, stringify_ecs_system_desc_t

# from .pipeline import Phase
from .query import QueryDescription, QueryExecutor, QueryResult
from .types import ContextFreeAction, Int32, Time

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
        entity=EntityId(0),
        query: QueryDescription | None = None,
        callback: IterateAction | None = None,
        run: RunAction | None = None,
        ctx: c_void_p | None = None,
        ctx_free: ContextFreeAction | None = None,
        callback_ctx: c_void_p | None = None,
        callback_ctx_free: ContextFreeAction | None = None,
        run_ctx: c_void_p | None = None,
        run_ctx_free: ContextFreeAction | None = None,
        interval: Time | None = None,
        rate=0,
        tick_source=0,
        multi_threaded=False,
        immediate=False,
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

    def entity(self, entity: EntityId):
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

    def interval(self, interval: Time):
        self._interval = interval
        return self

    def rate(self, rate: int):
        self._rate = rate
        return self

    def tick_source(self, tick_source: EntityId):
        self._tick_source = tick_source
        return self

    def multi_threaded(self, multi_threaded: bool):
        self._multi_threaded = multi_threaded
        return self

    def immediate(self, immediate: bool):
        self._immediate = immediate
        return self

    def build(self):
        d = struct_ecs_system_desc_t()

        # This happens automatically
        # system_desc._canary = 0
        if self._run is None and self._callback is None:
            raise Exception("SystemDescription must define one of [run, callback]")
        d.entity = self._entity
        if self._query is not None:
            d.query = self._query._value
        d.multi_threaded = self._multi_threaded
        if self._callback is not None:
            d.callback = _iterate_action(self._callback)
        if self._run is not None:
            d.run = _run_action(self._run)
        d.ctx = self._ctx
        if self._ctx_free is not None:
            d.ctx_free = self._ctx_free
        d.callback_ctx = self._callback_ctx
        if self._callback_ctx_free is not None:
            d.callback_ctx_free = self._callback_ctx_free
        d.run_ctx = self._run_ctx
        if self._run_ctx_free is not None:
            d.run_ctx_free = self._run_ctx_free
        #     d.interval = self._interval
        d.rate = self._rate
        d.tick_source = self._tick_source
        d.multi_threaded = self._multi_threaded
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
        query: QueryDescription | None = None,
    ):
        self._value = system_desc
        self._query = query

    def resolve(self, world):
        if self._query is not None:
            self._query.resolve(world)
            self._value.query = QueryDescription.copy(self._query)._value

    @property
    def callback(self) -> IterateAction:
        return self._value.callback

    @property
    def query(self) -> QueryDescription:
        return QueryDescription(self._value.query)

    @property
    def run(self) -> RunAction:
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

    def repr(self, depth=0):
        prefix = SPACER * depth
        return "\n".join(
            [
                f"{prefix}callback={self._value.callback}",
                f"{prefix}query=QueryDescription:\n{self._query.repr(depth + 1) if self._query is not None else None}"
                f"{prefix}run={self._value.run}",
                f"{prefix}ctx={self._value.ctx}",
                f"{prefix}ctx_free={self._value.ctx_free}",
                f"{prefix}callback_ctx={self._value.callback_ctx}",
                f"{prefix}callback_ctx_free={self._value.callback_ctx_free}",
                f"{prefix}run_ctx={self._value.run_ctx}",
                f"{prefix}run_ctx_free={self._value.run_ctx_free}",
                f"{prefix}interval={self._value.interval}",
                f"{prefix}rate={self._value.rate}",
                f"{prefix}tick_source={self._value.tick_source}",
                f"{prefix}multi_threaded={self._value.multi_threaded}",
                f"{prefix}immediate={self._value.immediate}",
            ]
        )

    def __repr__(self):
        return "\n".join(["SystemDescription:", self.repr(depth=1)])


class System:
    """Base class for all systems."""

    def __init__(self, world, description: SystemDescription):
        """System.__init__ should not be called by consumers. Call a World.system* factory method"""

        super().__init__()

        self._world = world
        self.description = description

    def each(self, executor):
        raise NotImplementedError()

    def run(self, executor):
        raise NotImplementedError()


SystemType = type[System]


def system(
    entity=EntityId(0),
    query: QueryDescription | None = None,
    ctx: c_void_p | None = None,
    ctx_free: ContextFreeAction | None = None,
    callback_ctx: c_void_p | None = None,
    callback_ctx_free: ContextFreeAction | None = None,
    run_ctx: c_void_p | None = None,
    run_ctx_free: ContextFreeAction | None = None,
    interval: Time | None = None,
    rate=0,
    tick_source=EntityId(0),
    multi_threaded=False,
    immediate=True,
):
    def decorator(cls: type):
        class WrappedSystem(cls, System):
            def __init__(self, world):
                sdb = (
                    SystemDescriptionBuilder()
                    .entity(entity)
                    .query(query)
                    .ctx(ctx)
                    .ctx_free(ctx_free)
                    .callback_ctx(callback_ctx)
                    .callback_ctx_free(callback_ctx_free)
                    .run_ctx(run_ctx)
                    .run_ctx_free(run_ctx_free)
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

        return WrappedSystem

    return decorator
