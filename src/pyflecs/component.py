from ctypes import Structure
from inspect import get_annotations, isclass
from typing import Optional


class Component(Structure):
    """Base class for all component classes."""

    __wrappedname__: Optional[str] = None


ComponentType = type[Component]


def is_component(c):
    return isclass(c) and issubclass(c, Component)


def component(cls: type):
    """Decorator for flecs component classes."""

    def _compute_fields(cls):
        return list(get_annotations(cls).items())

    def _compute_align(cls):
        # field_size = sizeof(item[1])
        # if _Component._align_ < field_size:
        #   _Component._align_ = field_size
        return 8

    class _Component(Component):
        # The wrapped class
        _wrapped = cls

        # Component name is taken from the wrapped class
        __wrappedname__ = cls.__name__

        # The ctypes field definitions
        _fields_ = _compute_fields(cls)

        # The ctypes struct alignment
        _align_ = _compute_align(cls)

        # The ctypes struct pack
        # _pack_ = _compute_pack(cls)

    return _Component
