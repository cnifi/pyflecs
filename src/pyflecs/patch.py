# https://stackoverflow.com/questions/15570610/ctypes-reimplementation-of-rshift-for-c-ulong

from ctypes import c_ulong, c_uint


def _rshift(self, other):
    if hasattr(other, "value"):
        other = other.value
    return c_ulong(self.value >> other)


def _lshift(self, other):
    if hasattr(other, "value"):
        other = other.value
    return c_ulong(self.value << other)


def _add(self, other):
    if hasattr(other, "value"):
        other = other.value
    return c_ulong(self.value + other)


def _or(self, other):
    if hasattr(other, "value"):
        other = other.value
    return c_ulong(self.value | other)


def _coerce(self, other):
    try:
        return self, self.__class__(other)
    except TypeError:
        return NotImplemented


c_ulong.__lshift__ = _lshift
c_ulong.__rshift__ = _rshift
c_ulong.__or__ = _or
c_ulong.__add__ = _add
c_ulong.__coerce__ = _coerce
