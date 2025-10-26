from ctypes import (
    CFUNCTYPE,
    c_bool,
    c_byte,
    c_char,
    c_char_p,
    c_double,
    c_float,
    c_int8,
    c_int16,
    c_int32,
    c_int64,
    c_long,
    c_longdouble,
    c_longlong,
    c_short,
    c_size_t,
    c_ubyte,
    c_uint8,
    c_uint16,
    c_uint32,
    c_uint64,
    c_ulong,
    c_ulonglong,
    c_ushort,
    c_void_p,
)
from typing import Callable

from .cflecs import struct_ecs_time_t

Double = c_double

Float = c_float

Short = c_short

UShort = c_ushort

Int8 = c_int8

Int16 = c_int16

Int32 = c_int32

Int64 = c_int64

UInt8 = c_uint8

UInt16 = c_uint16

UInt32 = c_uint32

UInt64 = c_uint64

Long = c_long

LongDouble = c_longdouble

LongLong = c_longlong

ULong = c_ulong

ULongLong = c_ulonglong

Byte = c_byte

UByte = c_ubyte

Character = c_char

String = c_char_p

Boolean = c_bool

type EntityId = c_uint64

Size = c_size_t


class Time:
    def __init__(self, value: struct_ecs_time_t):
        self._value = value


ContextFreeAction = Callable[[c_void_p], c_void_p]


def context_free_action(func: ContextFreeAction):
    _cfunctype = CFUNCTYPE(c_void_p, c_void_p)

    @_cfunctype
    def wrapper(ctx: c_void_p):
        return func(ctx)

    return wrapper
