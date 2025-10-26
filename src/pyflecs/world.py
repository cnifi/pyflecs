from ctypes import POINTER, _Pointer, byref, cast, sizeof

from bidict import bidict

from .cflecs import (
    String,
    ecs_add_id,
    ecs_add_pair,
    ecs_clear,
    ecs_component_init,
    ecs_delete,
    ecs_entity_init,
    ecs_get_id,
    ecs_get_pipeline,
    ecs_init,
    ecs_pipeline_init,
    ecs_progress,
    ecs_run,
    ecs_set_id,
    ecs_size_t,
    ecs_system_init,
    struct_ecs_component_desc_t,
    struct_ecs_entity_desc_t,
    struct_ecs_world_t,
)
from .component import Component
from .entity import EntityIdPair
from .patch import patch
from .pipeline import Pipeline
from .query import Query, QueryDescription
from .system import EachAction, OnceAction, SystemDescription
from .types import EntityId

idof = id

patch()  # TODO: Move to init


class World:
    def __init__(self, pipeline: Pipeline | None = None):
        self._value: _Pointer[struct_ecs_world_t] = ecs_init()
        self._systemdescs = bidict[EntityId, SystemDescription]()

    @property
    def pipeline(self):
        """Return the pipeline that this world is using."""
        return Pipeline(ecs_get_pipeline(self._value))

    @pipeline.setter
    def pipeline(self, pipeline: Pipeline):
        """Set the pipeline that this world is using."""
        ecs_pipeline_init(self._value, pipeline.description)

    def component(self, cls: type[Component]):
        """Create a new component in this world."""

        desc = struct_ecs_component_desc_t()

        entity_desc = struct_ecs_entity_desc_t()
        entity_desc.id = 0

        byte_name = String(cls._wrapped_.__qualname__.encode("utf-8"))

        entity_desc.name = byte_name
        entity_desc.symbol = byte_name
        entity_desc.use_low_id = True

        entity_id: EntityId = ecs_entity_init(self._value, byref(entity_desc))

        desc.entity = entity_id
        desc.type.size = ecs_size_t(sizeof(cls))
        desc.type.alignment = ecs_size_t(cls._align_)
        # desc.type.name = Box(String(component.__name__.encode("utf-8"))).value()

        # TODO: hooks

        return ecs_component_init(self._value, byref(desc))

    def entity(self):
        """Create a new entity in this world."""

        desc = struct_ecs_entity_desc_t()
        desc.use_low_id = False

        entity_id: EntityId = ecs_entity_init(self._value, byref(desc))

        return entity_id

    def add(self, e: EntityId, i: EntityId | EntityIdPair):
        """Add a component to the specified entity."""

        match i:
            case tuple():
                return self.add_pair(e, i)
            case type() if issubclass(i, Component):
                return ecs_add_id(self._value, e, self.fidof(i))
            case int():
                return ecs_add_id(self._value, e, i)

    def add_pair(
        self, entity_id: int, pair: tuple[int | type[Component], int | type[Component]]
    ):
        """Add a pair to the specified entity."""

        ecs_add_pair(
            self._value,
            entity_id,
            EntityId(pair[0]) if pair[0] is int else self._cmptypes.inverse[pair[0]],  # type: ignore
            EntityId(pair[1]) if pair[1] is int else self._cmptypes.inverse[pair[1]],  # type: ignore
        )

    def set[T: Component](
        self, entity_id: EntityId, component_id: EntityId, component: T
    ):
        """Set the value(s) of the component on the specified entity."""

        ecs_set_id(self._value, entity_id, component_id, sizeof(T), byref(component))  # type: ignore

    def get[T: Component](
        self, entity_id: EntityId, component_id: EntityId
    ) -> T | None:
        """Get the value of a component by its component id."""

        component_void_ptr = ecs_get_id(self._value, entity_id, component_id)

        if not component_void_ptr:
            return None

        component_ptr = cast(component_void_ptr, POINTER(T))  # type: ignore
        component = component_ptr.contents

        return component  # type: ignore

    def clear(self, entity_id: EntityId):
        """Clear the specified entity of all components."""

        ecs_clear(self._value, entity_id)

    def delete(self, entity_id: EntityId):
        """Delete the specified entity."""

        del self._systemdescs[entity_id]

        ecs_delete(self._value, entity_id)

    def query(self, d: QueryDescription):
        """Create a query from a query description object."""

        return Query(self, d)

    def query_kwargs(self, **kwargs):
        """Create a query from a query description built implicitly using the provided kwargs."""

        return Query.kwargs(self, **kwargs)

    def query_terms(self, terms: list[tuple]):
        """Create a simple query, with default configuration, based only on a list of terms."""

        return Query(self, QueryDescription.tuple(terms))

    def system(self, system_desc: SystemDescription) -> EntityId:
        """Create a new system."""

        system_id = ecs_system_init(self._value, byref(system_desc._value))

        # Pyflecs must keep a reference so the object is not garbage collected while flecs is using it
        self._systemdescs[system_id] = system_desc

        return system_id

    def system_kwargs(self, **kwargs):
        """Create a new system from a system description built implicitly using the provided kwargs."""

        return self.system(SystemDescription.kwargs(**kwargs))

    def system_once(self, action: OnceAction):
        """Create a new system, using defaults, with a run action."""

        return self.system(SystemDescription.once(action))

    def system_each(self, query: QueryDescription, action: EachAction):
        """Create a new system, using defaults, with a query and each action."""

        return self.system(SystemDescription.each(query, action))

    def run(self, system_id: EntityId, delta=0.0):
        """Run the specified system once."""

        ecs_run(self._value, system_id, delta, None)

    def progress(self):
        """Move time forward on this world, triggering pipeline processing on the delta time."""

        return ecs_progress(self._value, 0)
