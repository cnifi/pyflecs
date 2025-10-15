from ctypes import _Pointer, byref, c_uint64, sizeof

from bidict import bidict

from .adaptor import BoxedComponentDesc, BoxedEntityDesc
from .cflecs import (
    String,
    ecs_add_id,
    ecs_add_pair,
    ecs_clear,
    ecs_component_init,
    ecs_delete,
    ecs_entity_init,
    ecs_get_id,
    ecs_init,
    ecs_progress,
    ecs_run,
    ecs_set_id,
    ecs_size_t,
    ecs_system_init,
    struct_ecs_world_t,
)
from .component import Component
from .patch import patch
from .query import Query, QueryDescription
from .system import EachAction, OnceAction, System, SystemDescription
from .types import EntityId

patch()  # TODO: Move to init


class World:
    """Many things happen here."""

    def __init__(self):
        self._value: _Pointer[struct_ecs_world_t] = ecs_init()
        self._systems = bidict[EntityId, System]()
        self._systypes = bidict[EntityId, type[System]]()
        self._cmptypes = bidict[EntityId, type[Component]]()

    def component(self, cls: type[Component]):
        """Create a new component in this world."""

        desc = BoxedComponentDesc()

        entity_desc = BoxedEntityDesc()
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

        cid: EntityId = ecs_component_init(self._value, byref(desc))

        self._cmptypes[cid] = cls

        return int(cid)

    def entity(self):
        """Create a new entity in this world."""

        d = BoxedEntityDesc()

        d.use_low_id = False

        return ecs_entity_init(self._value, byref(d))

    def add(self, e: int, i: int | tuple[int, int]):
        """Add a component to the specified entity."""

        match i:
            case tuple():
                return self.add_pair(e, i)
            case type() if issubclass(i, Component):
                return ecs_add_id(self._value, e, self.fidof(i))
            case int():
                return ecs_add_id(self._value, e, i)

    def add_pair(self, e: int, p: tuple[int | type[Component], int | type[Component]]):
        """Add a pair to the specified entity."""

        ecs_add_pair(
            self._value,
            EntityId(e),
            EntityId(p[0]) if p[0] is int else self._cmptypes.inverse[p[0]],  # type: ignore
            EntityId(p[1]) if p[1] is int else self._cmptypes.inverse[p[1]],  # type: ignore
        )

    def set(self, e: int, c: Component):
        """Set the value(s) of the component on the specified entity."""

        ecs_set_id(self._value, e, self._cmptypes.inverse[type(c)], sizeof(c), byref(c))

    def get(self, e: int, c: type[Component]):
        """Get a component of the specified entity."""

        return ecs_get_id(self._value, e, self._cmptypes.inverse[c])

    def clear(self, e: EntityId):
        """Clear the specified entity of all components."""

        ecs_clear(self._value, e)

    def delete_entity(self, e: EntityId):
        ecs_delete(self._value, e)

    def delete_system_id(self, s: EntityId):
        self.delete_entity(s)

        del self._systypes[s]
        del self._systems[s]

    def delete_system_type(self, systype: type[System]):
        """Delete the system given by the system type."""

        self.delete_system_id(self._systypes.inverse[systype])

    def delete_system(self, sys: System):
        """Delete the system given by the system instance."""

        self.delete_system_id(self._systems.inverse[sys])

    def delete_id(self, i: EntityId):
        """Delete the entity with the specified id."""

        if self._systypes[i] is not None:
            self.delete_system_type(self._systypes[i])
        elif self._systems[i] is not None:
            self.delete_system(self._systems[i])
        else:
            self.delete_entity(i)

    def delete(self, e: EntityId | System | type[System]):
        """Delete the entity with the specified id, object, or type."""

        match e:
            case EntityId():
                self.delete_id(c_uint64(e))
            case System():
                self.delete_system(e)
            case type() if issubclass(e, System):
                self.delete_system_type(e)

    def query(self, d: QueryDescription):
        """Create a query from a query description object."""

        return Query(self, d)

    def query_kwargs(self, **kwargs):
        """Create a query from a query description built implicitly using the provided kwargs."""

        return Query.kwargs(self, **kwargs)

    def query_terms(self, terms: list[tuple]):
        """Create a simple query, with default configuration, based only on a list of terms."""

        return Query(self, QueryDescription.tuple(terms))

    def system(self, sord: SystemDescription | type[System]):
        """Create a new system."""

        w = self
        wv = self._value

        if isinstance(sord, SystemDescription):
            sd: SystemDescription = sord
            s = System(w, sd)
        else:
            s: System = sord(w)  # type: ignore
            sd = s.description

        sd.resolve(w)

        sid = ecs_system_init(wv, byref(sd._value))
        s._world = w

        self._systypes[sid] = type(s)
        self._systems[sid] = s

        return int(sid), s

    def system_kwargs(self, **kwargs):
        """Create a new system from a system description built implicitly using the provided kwargs."""

        return self.system(SystemDescription.kwargs(**kwargs))

    def system_once(self, action: OnceAction):
        """Create a new system, using defaults, with a run action."""

        return self.system(SystemDescription.kwargs(run=action))

    def system_each(self, query: QueryDescription, action: EachAction):
        """Create a new system, using defaults, with a query and each action."""

        return self.system(SystemDescription.kwargs(query=query, callback=action))

    def run_system_id(self, sid: int, delta=0.0):
        """Run the specified system once."""

        ecs_run(self._value, EntityId(sid), delta, None)

    def run_system_type(self, systype: type[System], delta=0.0):
        """Run the specified system once."""

        ecs_run(self._value, self._systypes.inverse[systype], delta, None)

    def run_system(self, sys: System, delta=0.0):
        """Run the specified system once."""

        ecs_run(self._value, self._systems.inverse[sys], delta, None)

    def run(self, sysid_or_systype: int | type[System], delta=0.0):
        match sysid_or_systype:
            case int():
                return self.run_system_id(sysid_or_systype, delta)
            case type() if issubclass(sysid_or_systype, System):
                return self.run_system_type(sysid_or_systype, delta)
            case System():
                return self.run_system(sysid_or_systype, delta)

    def progress(self):
        """Move time forward on this world, triggering pipeline processing on the delta time."""

        return ecs_progress(self._value, 0)
