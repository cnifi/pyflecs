from ctypes import Structure, addressof

from .cflecs import (
    c_char_p,
    struct_ecs_component_desc_t,
    struct_ecs_entity_desc_t,
    struct_ecs_query_desc_t,
    struct_ecs_system_desc_t,
    struct_ecs_term_ref_t,
    struct_ecs_term_t,
    struct_ecs_type_hooks_t,
    struct_ecs_type_info_t,
)

type CType = c_char_p | Structure


class ReferenceManager:
    """ReferenceManager is responsible for holding onto references to python-allocated C data so that python will not garbage collect it while it is still in use inside C flecs."""

    def __init__(self):
        self._references = {}

    def hold(self, value: CType):
        """Hold a reference to the provided value."""
        self._references[addressof(value)] = value
        return value

    def get(self, value: CType):
        return self._references[addressof(value)]

    def holding(self, value: CType):
        return self._references[addressof(value)] is not None

    def release(self, value: CType):
        del self._references[addressof(value)]

    def release_all(self):
        for r in self._references.values():
            self.release(r)


class ReferenceBox(Structure):
    """A reference box is used as a decorator for C struct types to add tracking behavior by a ReferenceManager."""

    _refmgr = ReferenceManager()

    def __init__(self):
        ReferenceBox._refmgr.hold(self)

    @classmethod
    def get(cls, value):
        return cls._refmgr.get(value)

    @classmethod
    def release_all(cls):
        cls._refmgr.release_all()

    # def __hash__(self):
    #   return addressof(self)

    def release(self):
        self._refmgr.release(self)


class BoxedTypeHooks(struct_ecs_type_hooks_t, ReferenceBox):
    def __init__(self):
        struct_ecs_type_hooks_t.__init__(self)
        ReferenceBox.__init__(self)


class BoxedTypeInfo(struct_ecs_type_info_t, ReferenceBox):
    def __init__(self):
        struct_ecs_type_info_t.__init__(self)
        ReferenceBox.__init__(self)


class BoxedComponentDesc(struct_ecs_component_desc_t, ReferenceBox):
    def __init__(self):
        struct_ecs_component_desc_t.__init__(self)
        ReferenceBox.__init__(self)


class BoxedEntityDesc(struct_ecs_entity_desc_t, ReferenceBox):
    def __init__(self):
        struct_ecs_entity_desc_t.__init__(self)
        ReferenceBox.__init__(self)


class BoxedQueryDesc(struct_ecs_query_desc_t, ReferenceBox):
    def __init__(self):
        struct_ecs_query_desc_t.__init__(self)
        ReferenceBox.__init__(self)


class BoxedTerm(struct_ecs_term_t, ReferenceBox):
    def __init__(self):
        struct_ecs_term_t.__init__(self)
        ReferenceBox.__init__(self)


class BoxedTermRef(struct_ecs_term_ref_t, ReferenceBox):
    def __init__(self):
        struct_ecs_term_ref_t.__init__(self)
        ReferenceBox.__init__(self)


class BoxedSystemDescription(struct_ecs_system_desc_t, ReferenceBox):
    def __init__(self):
        struct_ecs_system_desc_t.__init__(self)
        ReferenceBox.__init__(self)
