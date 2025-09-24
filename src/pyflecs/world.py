from ctypes import _Pointer, byref, c_uint64, sizeof
from typing import Dict

from .adaptor import BoxedComponentDesc, BoxedEntityDesc
from .cflecs import (
    String,
    ecs_add_id,
    ecs_add_pair,
    ecs_component_init,
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
from .component import Component, ComponentId, ComponentIdPair
from .entity import EntityId, EntityIdPair
from .patch import *
from .query import Query, QueryDescription
from .system import System, SystemDescription, SystemType
from .types import IdType


class WorldException(Exception):
    pass


type PyflecsObject = type[Component] | System

EntityOrComponentId = ComponentId

EntityOrComponentIdPair = tuple[EntityOrComponentId, EntityOrComponentId]


class World:
    """Many things happen here"""

    _addr_to_world: Dict[int, object] = dict()

    @classmethod
    def _world(cls, id: int):
        return cls._addr_to_world[id]

    def __init__(self):
        # This class acts as a wrapper around struct_ecs_world_t
        self._value: _Pointer[struct_ecs_world_t] = ecs_init()

        # Mapping between ids and their wrapping python instance
        self._pid2pob = dict[int, PyflecsObject]()
        self._pid2fid = dict[int, IdType]()
        self._fid2pid = dict[IdType, int]()

        # TODO: Remove
        self._addr_to_world[id(self)] = self

    def _putob(self, fid: IdType, pob: PyflecsObject):
        pid = id(pob)
        self._pid2pob[pid] = pob
        self._pid2fid[pid] = fid
        self._fid2pid[fid] = pid

    def _delob(self, id: int | IdType):
        if type(id) is int:
            pid: int = id
            fid: IdType = self._pid2fid[pid]
        else:
            pid: int = id  # type: ignore
            fid: IdType = self._pid2fid[id]  # type: ignore

        del self._pid2pob[pid]
        del self._pid2fid[pid]
        del self._fid2pid[fid]

    def fidof(self, pid: int | PyflecsObject) -> IdType:
        """Given a python object or python object id, return the associated flecs entity id registered to this world."""

        return self._pid2fid[pid if type(pid) is int else id(pid)]  # type: ignore

    def pidof(self, fid: IdType):
        """Given the flecs id registered to this world, return the associated python object id."""

        return self._fid2pid[fid]

    def pobof(self, id: int | IdType):
        """Given the python object id or flecs id registered to this world, return the associated python object."""

        return self._pid2pob[id if type(id) is int else self._fid2pid[id]]  # type: ignore

    def component(self, cls: type[Component]):
        """Register a component in this world."""

        desc = BoxedComponentDesc()

        entity_desc = BoxedEntityDesc()
        entity_desc.id = 0

        byte_name = String(cls._wrapped_.__qualname__.encode("utf-8"))

        entity_desc.name = byte_name
        entity_desc.symbol = byte_name
        entity_desc.use_low_id = True

        entity_id: c_uint64 = ecs_entity_init(self._value, byref(entity_desc))

        desc.entity = entity_id
        desc.type.size = ecs_size_t(sizeof(cls))
        desc.type.alignment = ecs_size_t(cls._align_)
        # desc.type.name = Box(String(component.__name__.encode("utf-8"))).value()

        # TODO: hooks

        cid = ecs_component_init(self._value, byref(desc))

        self._putob(cid, cls)

        return int(cid)

    def entity(self):
        """Create a new entity in this world."""

        d = BoxedEntityDesc()

        d.use_low_id = False

        return int(ecs_entity_init(self._value, byref(d)))

    def add(
        self,
        e: EntityId,
        i: EntityOrComponentId | EntityOrComponentIdPair,
    ):
        """Add a component to the specified entity."""

        match i:
            case tuple():
                return self.add_pair(e, i)
            case type() if issubclass(i, Component):
                return ecs_add_id(self._value, e, self.fidof(i))
            case int():
                return ecs_add_id(self._value, e, i)

    def add_pair(self, e: EntityId, p: EntityIdPair | ComponentIdPair):
        """Add a pair to the specified entity."""

        p0 = IdType(p[0]) if p[0] is int else self.fidof(p[0])
        p1 = IdType(p[1]) if p[1] is int else self.fidof(p[1])

        ecs_add_pair(self._value, e, p0, p1)

    def set(self, e: EntityId, c: Component):
        """Set the value(s) of the component on the specified entity."""

        ecs_set_id(self._value, e, self.fidof(type(c)), sizeof(c), byref(c))

    def get(self, e: EntityId, c: type[Component]):
        """Get a component of the specified entity."""

        return ecs_get_id(self._value, e, self.fidof(c))

    def query(self, d: QueryDescription):
        """Create a query from a query description object."""

        return Query(self, d)

    def query_kwargs(self, **kwargs):
        """Create a query from a query description built implicitly using the provided kwargs."""

        return Query.kwargs(self, **kwargs)

    def query_terms(self, terms: list[tuple]):
        """Create a simple query, with default configuration, based only on a list of terms."""

        print(terms)
        return Query(self, QueryDescription.tuple(terms))

    def system(self, sord: SystemType | SystemDescription):
        """Create a system instance in this world."""

        w = self
        wv = self._value

        if isinstance(sord, SystemDescription):
            sd: SystemDescription = sord
            s = System(w, sd)
        else:
            s: System = sord(w)  # type: ignore
            sd = s.description

        sd.resolve(w)

        # Initialize the system and get the system id
        sid = ecs_system_init(wv, byref(sd._value))
        s._world = w

        # Register the flecs id to the python reference
        self._putob(sid, s)

        return s

    # def system_lambda(self, action: Callable[[]]):
    #     pass

    def system_kwargs(self, **kwargs):
        """Create a system from a system description built implicitly using the provided kwargs."""

        return self.system(SystemDescription.kwargs(**kwargs))

    # def system_each(self, **kwargs):
    #     pass

    def progress(self):
        return ecs_progress(self._value, 0)

    def run(self, s: EntityId | System):
        if type(s) is int:
            ecs_run(self._value, s, 0.0, None)
        elif isinstance(s, System):
            ecs_run(self._value, self.fidof(s), 0.0, None)
