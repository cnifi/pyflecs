from ctypes import Structure, sizeof
from inspect import get_annotations, isclass
from typing import Optional, Self

from .types import Boolean, Double, Int64, String


class Component(Structure):
    """Base class for all component classes."""

    _wrappedclass_: Self

    _wrappedname_: Optional[str] = None


ComponentType = type[Component]


def is_component(c):
    return isclass(c) and issubclass(c, Component)


def component(cls):
    """Decorator for flecs component classes."""

    def configure_field(k: str, t: type):
        if t is int:
            return (
                f"_{k}",
                Int64,
                lambda value: Int64(value),
                lambda value: int(value),
            )
        if t is float:
            return (
                f"_{k}",
                Double,
                lambda value: Double(value),
                lambda value: float(value),
            )
        if t is bool:
            return (
                f"_{k}",
                Boolean,
                lambda value: Boolean(value),
                lambda value: bool(value),
            )
        if t is str:
            return (
                f"_{k}",
                String,
                lambda value: value.encode("utf-8") if value is not None else None,
                lambda value: value.decode("utf-8") if value is not None else None,
            )
        return k, t, None, None

    fields = list(get_annotations(cls).items())

    field_configs = [configure_field(k, t) for k, t in fields]

    def compute_align():
        highest = 0

        for _, typ, _, _ in field_configs:
            size = sizeof(typ)
            if size > highest:
                highest = size

        return highest

    def map_init_arg(i, v):
        config = field_configs[i]

        if len(config) < 3:
            return v

        return config[2](v)  # type: ignore

    def map_init_args(*args):
        return [map_init_arg(i, v) for i, v in enumerate(args)]

    class WrappedComponent(Component):
        # The wrapped class
        _wrappedclass_ = cls

        # Component name is taken from the wrapped class
        _wrappedname_ = cls.__name__

        # The ctypes field definitions
        _fields_ = [(tup[0], tup[1]) for tup in field_configs]

        # The ctypes struct alignment
        _align_ = compute_align()

        # The ctypes struct pack
        # _pack_ = _compute_pack(cls)        # The ctypes struct pack

        def __init__(self, *args, **kwargs):
            super().__init__(*map_init_args(*args))

    for key, _, to, frm in field_configs:
        if to is not None and frm is not None:

            def getter(self, key=key, to=to, frm=frm):
                return frm(getattr(self, key))

            def setter(self, value, key=key, to=to, frm=frm):
                setattr(self, key, to(value))

            prop = property(getter, setter)

            setattr(WrappedComponent, key[1:], prop)

    return WrappedComponent
