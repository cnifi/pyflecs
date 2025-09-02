r"""Wrapper for flecs.h

Generated with:
.venv/bin/ctypesgen -llibflecs.dylib ./flecs.h -o cflecs.py

Do not modify this file.
"""

__docformat__ = "restructuredtext"

# Begin preamble for Python

import ctypes
import sys
from ctypes import *  # noqa: F401, F403

_int_types = (ctypes.c_int16, ctypes.c_int32)
if hasattr(ctypes, "c_int64"):
    # Some builds of ctypes apparently do not have ctypes.c_int64
    # defined; it's a pretty good bet that these builds do not
    # have 64-bit pointers.
    _int_types += (ctypes.c_int64,)
for t in _int_types:
    if ctypes.sizeof(t) == ctypes.sizeof(ctypes.c_size_t):
        c_ptrdiff_t = t
del t
del _int_types


class UserString:
    def __init__(self, seq):
        if isinstance(seq, bytes):
            self.data = seq
        elif isinstance(seq, UserString):
            self.data = seq.data[:]
        else:
            self.data = str(seq).encode()

    def __bytes__(self):
        return self.data

    def __str__(self):
        return self.data.decode()

    def __repr__(self):
        return repr(self.data)

    def __int__(self):
        return int(self.data.decode())

    def __long__(self):
        return int(self.data.decode())

    def __float__(self):
        return float(self.data.decode())

    def __complex__(self):
        return complex(self.data.decode())

    def __hash__(self):
        return hash(self.data)

    def __le__(self, string):
        if isinstance(string, UserString):
            return self.data <= string.data
        else:
            return self.data <= string

    def __lt__(self, string):
        if isinstance(string, UserString):
            return self.data < string.data
        else:
            return self.data < string

    def __ge__(self, string):
        if isinstance(string, UserString):
            return self.data >= string.data
        else:
            return self.data >= string

    def __gt__(self, string):
        if isinstance(string, UserString):
            return self.data > string.data
        else:
            return self.data > string

    def __eq__(self, string):
        if isinstance(string, UserString):
            return self.data == string.data
        else:
            return self.data == string

    def __ne__(self, string):
        if isinstance(string, UserString):
            return self.data != string.data
        else:
            return self.data != string

    def __contains__(self, char):
        return char in self.data

    def __len__(self):
        return len(self.data)

    def __getitem__(self, index):
        return self.__class__(self.data[index])

    def __getslice__(self, start, end):
        start = max(start, 0)
        end = max(end, 0)
        return self.__class__(self.data[start:end])

    def __add__(self, other):
        if isinstance(other, UserString):
            return self.__class__(self.data + other.data)
        elif isinstance(other, bytes):
            return self.__class__(self.data + other)
        else:
            return self.__class__(self.data + str(other).encode())

    def __radd__(self, other):
        if isinstance(other, bytes):
            return self.__class__(other + self.data)
        else:
            return self.__class__(str(other).encode() + self.data)

    def __mul__(self, n):
        return self.__class__(self.data * n)

    __rmul__ = __mul__

    def __mod__(self, args):
        return self.__class__(self.data % args)

    # the following methods are defined in alphabetical order:
    def capitalize(self):
        return self.__class__(self.data.capitalize())

    def center(self, width, *args):
        return self.__class__(self.data.center(width, *args))

    def count(self, sub, start=0, end=sys.maxsize):
        return self.data.count(sub, start, end)

    def decode(self, encoding=None, errors=None):  # XXX improve this?
        if encoding:
            if errors:
                return self.__class__(self.data.decode(encoding, errors))
            else:
                return self.__class__(self.data.decode(encoding))
        else:
            return self.__class__(self.data.decode())

    def encode(self, encoding=None, errors=None):  # XXX improve this?
        if encoding:
            if errors:
                return self.__class__(self.data.encode(encoding, errors))
            else:
                return self.__class__(self.data.encode(encoding))
        else:
            return self.__class__(self.data.encode())

    def endswith(self, suffix, start=0, end=sys.maxsize):
        return self.data.endswith(suffix, start, end)

    def expandtabs(self, tabsize=8):
        return self.__class__(self.data.expandtabs(tabsize))

    def find(self, sub, start=0, end=sys.maxsize):
        return self.data.find(sub, start, end)

    def index(self, sub, start=0, end=sys.maxsize):
        return self.data.index(sub, start, end)

    def isalpha(self):
        return self.data.isalpha()

    def isalnum(self):
        return self.data.isalnum()

    def isdecimal(self):
        return self.data.isdecimal()

    def isdigit(self):
        return self.data.isdigit()

    def islower(self):
        return self.data.islower()

    def isnumeric(self):
        return self.data.isnumeric()

    def isspace(self):
        return self.data.isspace()

    def istitle(self):
        return self.data.istitle()

    def isupper(self):
        return self.data.isupper()

    def join(self, seq):
        return self.data.join(seq)

    def ljust(self, width, *args):
        return self.__class__(self.data.ljust(width, *args))

    def lower(self):
        return self.__class__(self.data.lower())

    def lstrip(self, chars=None):
        return self.__class__(self.data.lstrip(chars))

    def partition(self, sep):
        return self.data.partition(sep)

    def replace(self, old, new, maxsplit=-1):
        return self.__class__(self.data.replace(old, new, maxsplit))

    def rfind(self, sub, start=0, end=sys.maxsize):
        return self.data.rfind(sub, start, end)

    def rindex(self, sub, start=0, end=sys.maxsize):
        return self.data.rindex(sub, start, end)

    def rjust(self, width, *args):
        return self.__class__(self.data.rjust(width, *args))

    def rpartition(self, sep):
        return self.data.rpartition(sep)

    def rstrip(self, chars=None):
        return self.__class__(self.data.rstrip(chars))

    def split(self, sep=None, maxsplit=-1):
        return self.data.split(sep, maxsplit)

    def rsplit(self, sep=None, maxsplit=-1):
        return self.data.rsplit(sep, maxsplit)

    def splitlines(self, keepends=0):
        return self.data.splitlines(keepends)

    def startswith(self, prefix, start=0, end=sys.maxsize):
        return self.data.startswith(prefix, start, end)

    def strip(self, chars=None):
        return self.__class__(self.data.strip(chars))

    def swapcase(self):
        return self.__class__(self.data.swapcase())

    def title(self):
        return self.__class__(self.data.title())

    def translate(self, *args):
        return self.__class__(self.data.translate(*args))

    def upper(self):
        return self.__class__(self.data.upper())

    def zfill(self, width):
        return self.__class__(self.data.zfill(width))


class MutableString(UserString):
    """mutable string objects

    Python strings are immutable objects.  This has the advantage, that
    strings may be used as dictionary keys.  If this property isn't needed
    and you insist on changing string values in place instead, you may cheat
    and use MutableString.

    But the purpose of this class is an educational one: to prevent
    people from inventing their own mutable string class derived
    from UserString and than forget thereby to remove (override) the
    __hash__ method inherited from UserString.  This would lead to
    errors that would be very hard to track down.

    A faster and better solution is to rewrite your program using lists."""

    def __init__(self, string=""):
        self.data = string

    def __hash__(self):
        raise TypeError("unhashable type (it is mutable)")

    def __setitem__(self, index, sub):
        if index < 0:
            index += len(self.data)
        if index < 0 or index >= len(self.data):
            raise IndexError
        self.data = self.data[:index] + sub + self.data[index + 1 :]

    def __delitem__(self, index):
        if index < 0:
            index += len(self.data)
        if index < 0 or index >= len(self.data):
            raise IndexError
        self.data = self.data[:index] + self.data[index + 1 :]

    def __setslice__(self, start, end, sub):
        start = max(start, 0)
        end = max(end, 0)
        if isinstance(sub, UserString):
            self.data = self.data[:start] + sub.data + self.data[end:]
        elif isinstance(sub, bytes):
            self.data = self.data[:start] + sub + self.data[end:]
        else:
            self.data = self.data[:start] + str(sub).encode() + self.data[end:]

    def __delslice__(self, start, end):
        start = max(start, 0)
        end = max(end, 0)
        self.data = self.data[:start] + self.data[end:]

    def immutable(self):
        return UserString(self.data)

    def __iadd__(self, other):
        if isinstance(other, UserString):
            self.data += other.data
        elif isinstance(other, bytes):
            self.data += other
        else:
            self.data += str(other).encode()
        return self

    def __imul__(self, n):
        self.data *= n
        return self


class String(MutableString, ctypes.Union):
    _fields_ = [("raw", ctypes.POINTER(ctypes.c_char)), ("data", ctypes.c_char_p)]

    def __init__(self, obj=b""):
        if isinstance(obj, (bytes, UserString)):
            self.data = bytes(obj)
        else:
            self.raw = obj

    def __len__(self):
        return self.data and len(self.data) or 0

    def from_param(cls, obj):
        # Convert None or 0
        if obj is None or obj == 0:
            return cls(ctypes.POINTER(ctypes.c_char)())

        # Convert from String
        elif isinstance(obj, String):
            return obj

        # Convert from bytes
        elif isinstance(obj, bytes):
            return cls(obj)

        # Convert from str
        elif isinstance(obj, str):
            return cls(obj.encode())

        # Convert from c_char_p
        elif isinstance(obj, ctypes.c_char_p):
            return obj

        # Convert from POINTER(ctypes.c_char)
        elif isinstance(obj, ctypes.POINTER(ctypes.c_char)):
            return obj

        # Convert from raw pointer
        elif isinstance(obj, int):
            return cls(ctypes.cast(obj, ctypes.POINTER(ctypes.c_char)))

        # Convert from ctypes.c_char array
        elif isinstance(obj, ctypes.c_char * len(obj)):
            return obj

        # Convert from object
        else:
            return String.from_param(obj._as_parameter_)

    from_param = classmethod(from_param)


def ReturnString(obj, func=None, arguments=None):
    return String.from_param(obj)


# As of ctypes 1.0, ctypes does not support custom error-checking
# functions on callbacks, nor does it support custom datatypes on
# callbacks, so we must ensure that all callbacks return
# primitive datatypes.
#
# Non-primitive return values wrapped with UNCHECKED won't be
# typechecked, and will be converted to ctypes.c_void_p.
def UNCHECKED(type):
    if hasattr(type, "_type_") and isinstance(type._type_, str) and type._type_ != "P":
        return type
    else:
        return ctypes.c_void_p


# ctypes doesn't have direct support for variadic functions, so we have to write
# our own wrapper class
class _variadic_function(object):
    def __init__(self, func, restype, argtypes, errcheck):
        self.func = func
        self.func.restype = restype
        self.argtypes = argtypes
        if errcheck:
            self.func.errcheck = errcheck

    def _as_parameter_(self):
        # So we can pass this variadic function as a function pointer
        return self.func

    def __call__(self, *args):
        fixed_args = []
        i = 0
        for argtype in self.argtypes:
            # Typecheck what we can
            fixed_args.append(argtype.from_param(args[i]))
            i += 1
        return self.func(*fixed_args + list(args[i:]))


def ord_if_char(value):
    """
    Simple helper used for casts to simple builtin types:  if the argument is a
    string type, it will be converted to it's ordinal value.

    This function will raise an exception if the argument is string with more
    than one characters.
    """
    return ord(value) if (isinstance(value, bytes) or isinstance(value, str)) else value


# End preamble

_libs = {}
_libdirs = []

# Begin loader

"""
Load libraries - appropriately for all our supported platforms
"""
# ----------------------------------------------------------------------------
# Copyright (c) 2008 David James
# Copyright (c) 2006-2008 Alex Holkner
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
#
#  * Redistributions of source code must retain the above copyright
#    notice, this list of conditions and the following disclaimer.
#  * Redistributions in binary form must reproduce the above copyright
#    notice, this list of conditions and the following disclaimer in
#    the documentation and/or other materials provided with the
#    distribution.
#  * Neither the name of pyglet nor the names of its
#    contributors may be used to endorse or promote products
#    derived from this software without specific prior written
#    permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
# FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
# COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
# BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
# ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
# ----------------------------------------------------------------------------

import ctypes
import ctypes.util
import glob
import os.path
import platform
import re
import sys


def _environ_path(name):
    """Split an environment variable into a path-like list elements"""
    if name in os.environ:
        return os.environ[name].split(":")
    return []


class LibraryLoader:
    """
    A base class For loading of libraries ;-)
    Subclasses load libraries for specific platforms.
    """

    # library names formatted specifically for platforms
    name_formats = ["%s"]

    class Lookup:
        """Looking up calling conventions for a platform"""

        mode = ctypes.DEFAULT_MODE

        def __init__(self, path):
            super(LibraryLoader.Lookup, self).__init__()
            self.access = dict(cdecl=ctypes.CDLL(path, self.mode))

        def get(self, name, calling_convention="cdecl"):
            """Return the given name according to the selected calling convention"""
            if calling_convention not in self.access:
                raise LookupError(
                    "Unknown calling convention '{}' for function '{}'".format(
                        calling_convention, name
                    )
                )
            return getattr(self.access[calling_convention], name)

        def has(self, name, calling_convention="cdecl"):
            """Return True if this given calling convention finds the given 'name'"""
            if calling_convention not in self.access:
                return False
            return hasattr(self.access[calling_convention], name)

        def __getattr__(self, name):
            return getattr(self.access["cdecl"], name)

    def __init__(self):
        self.other_dirs = []

    def __call__(self, libname):
        """Given the name of a library, load it."""
        paths = self.getpaths(libname)

        for path in paths:
            # noinspection PyBroadException
            try:
                return self.Lookup(path)
            except Exception:  # pylint: disable=broad-except
                pass

        raise ImportError("Could not load %s." % libname)

    def getpaths(self, libname):
        """Return a list of paths where the library might be found."""
        if os.path.isabs(libname):
            yield libname
        else:
            # search through a prioritized series of locations for the library

            # we first search any specific directories identified by user
            for dir_i in self.other_dirs:
                for fmt in self.name_formats:
                    # dir_i should be absolute already
                    yield os.path.join(dir_i, fmt % libname)

            # check if this code is even stored in a physical file
            try:
                this_file = __file__
            except NameError:
                this_file = None

            # then we search the directory where the generated python interface is stored
            if this_file is not None:
                for fmt in self.name_formats:
                    yield os.path.abspath(
                        os.path.join(os.path.dirname(__file__), fmt % libname)
                    )

            # now, use the ctypes tools to try to find the library
            for fmt in self.name_formats:
                path = ctypes.util.find_library(fmt % libname)
                if path:
                    yield path

            # then we search all paths identified as platform-specific lib paths
            for path in self.getplatformpaths(libname):
                yield path

            # Finally, we'll try the users current working directory
            for fmt in self.name_formats:
                yield os.path.abspath(os.path.join(os.path.curdir, fmt % libname))

    def getplatformpaths(self, _libname):  # pylint: disable=no-self-use
        """Return all the library paths available in this platform"""
        return []


# Darwin (Mac OS X)


class DarwinLibraryLoader(LibraryLoader):
    """Library loader for MacOS"""

    name_formats = [
        "lib%s.dylib",
        "lib%s.so",
        "lib%s.bundle",
        "%s.dylib",
        "%s.so",
        "%s.bundle",
        "%s",
    ]

    class Lookup(LibraryLoader.Lookup):
        """
        Looking up library files for this platform (Darwin aka MacOS)
        """

        # Darwin requires dlopen to be called with mode RTLD_GLOBAL instead
        # of the default RTLD_LOCAL.  Without this, you end up with
        # libraries not being loadable, resulting in "Symbol not found"
        # errors
        mode = ctypes.RTLD_GLOBAL

    def getplatformpaths(self, libname):
        if os.path.pathsep in libname:
            names = [libname]
        else:
            names = [fmt % libname for fmt in self.name_formats]

        for directory in self.getdirs(libname):
            for name in names:
                yield os.path.join(directory, name)

    @staticmethod
    def getdirs(libname):
        """Implements the dylib search as specified in Apple documentation:

        http://developer.apple.com/documentation/DeveloperTools/Conceptual/
            DynamicLibraries/Articles/DynamicLibraryUsageGuidelines.html

        Before commencing the standard search, the method first checks
        the bundle's ``Frameworks`` directory if the application is running
        within a bundle (OS X .app).
        """

        dyld_fallback_library_path = _environ_path("DYLD_FALLBACK_LIBRARY_PATH")
        if not dyld_fallback_library_path:
            dyld_fallback_library_path = [
                os.path.expanduser("~/lib"),
                "/usr/local/lib",
                "/usr/lib",
            ]

        dirs = []

        if "/" in libname:
            dirs.extend(_environ_path("DYLD_LIBRARY_PATH"))
        else:
            dirs.extend(_environ_path("LD_LIBRARY_PATH"))
            dirs.extend(_environ_path("DYLD_LIBRARY_PATH"))
            dirs.extend(_environ_path("LD_RUN_PATH"))

        if hasattr(sys, "frozen") and getattr(sys, "frozen") == "macosx_app":
            dirs.append(os.path.join(os.environ["RESOURCEPATH"], "..", "Frameworks"))

        dirs.extend(dyld_fallback_library_path)

        return dirs


# Posix


class PosixLibraryLoader(LibraryLoader):
    """Library loader for POSIX-like systems (including Linux)"""

    _ld_so_cache = None

    _include = re.compile(r"^\s*include\s+(?P<pattern>.*)")

    name_formats = ["lib%s.so", "%s.so", "%s"]

    class _Directories(dict):
        """Deal with directories"""

        def __init__(self):
            dict.__init__(self)
            self.order = 0

        def add(self, directory):
            """Add a directory to our current set of directories"""
            if len(directory) > 1:
                directory = directory.rstrip(os.path.sep)
            # only adds and updates order if exists and not already in set
            if not os.path.exists(directory):
                return
            order = self.setdefault(directory, self.order)
            if order == self.order:
                self.order += 1

        def extend(self, directories):
            """Add a list of directories to our set"""
            for a_dir in directories:
                self.add(a_dir)

        def ordered(self):
            """Sort the list of directories"""
            return (i[0] for i in sorted(self.items(), key=lambda d: d[1]))

    def _get_ld_so_conf_dirs(self, conf, dirs):
        """
        Recursive function to help parse all ld.so.conf files, including proper
        handling of the `include` directive.
        """

        try:
            with open(conf) as fileobj:
                for dirname in fileobj:
                    dirname = dirname.strip()
                    if not dirname:
                        continue

                    match = self._include.match(dirname)
                    if not match:
                        dirs.add(dirname)
                    else:
                        for dir2 in glob.glob(match.group("pattern")):
                            self._get_ld_so_conf_dirs(dir2, dirs)
        except IOError:
            pass

    def _create_ld_so_cache(self):
        # Recreate search path followed by ld.so.  This is going to be
        # slow to build, and incorrect (ld.so uses ld.so.cache, which may
        # not be up-to-date).  Used only as fallback for distros without
        # /sbin/ldconfig.
        #
        # We assume the DT_RPATH and DT_RUNPATH binary sections are omitted.

        directories = self._Directories()
        for name in (
            "LD_LIBRARY_PATH",
            "SHLIB_PATH",  # HP-UX
            "LIBPATH",  # OS/2, AIX
            "LIBRARY_PATH",  # BE/OS
        ):
            if name in os.environ:
                directories.extend(os.environ[name].split(os.pathsep))

        self._get_ld_so_conf_dirs("/etc/ld.so.conf", directories)

        bitage = platform.architecture()[0]

        unix_lib_dirs_list = []
        if bitage.startswith("64"):
            # prefer 64 bit if that is our arch
            unix_lib_dirs_list += ["/lib64", "/usr/lib64"]

        # must include standard libs, since those paths are also used by 64 bit
        # installs
        unix_lib_dirs_list += ["/lib", "/usr/lib"]
        if sys.platform.startswith("linux"):
            # Try and support multiarch work in Ubuntu
            # https://wiki.ubuntu.com/MultiarchSpec
            if bitage.startswith("32"):
                # Assume Intel/AMD x86 compat
                unix_lib_dirs_list += ["/lib/i386-linux-gnu", "/usr/lib/i386-linux-gnu"]
            elif bitage.startswith("64"):
                # Assume Intel/AMD x86 compatible
                unix_lib_dirs_list += [
                    "/lib/x86_64-linux-gnu",
                    "/usr/lib/x86_64-linux-gnu",
                ]
            else:
                # guess...
                unix_lib_dirs_list += glob.glob("/lib/*linux-gnu")
        directories.extend(unix_lib_dirs_list)

        cache = {}
        lib_re = re.compile(r"lib(.*)\.s[ol]")
        # ext_re = re.compile(r"\.s[ol]$")
        for our_dir in directories.ordered():
            try:
                for path in glob.glob("%s/*.s[ol]*" % our_dir):
                    file = os.path.basename(path)

                    # Index by filename
                    cache_i = cache.setdefault(file, set())
                    cache_i.add(path)

                    # Index by library name
                    match = lib_re.match(file)
                    if match:
                        library = match.group(1)
                        cache_i = cache.setdefault(library, set())
                        cache_i.add(path)
            except OSError:
                pass

        self._ld_so_cache = cache

    def getplatformpaths(self, libname):
        if self._ld_so_cache is None:
            self._create_ld_so_cache()

        result = self._ld_so_cache.get(libname, set())
        for i in result:
            # we iterate through all found paths for library, since we may have
            # actually found multiple architectures or other library types that
            # may not load
            yield i


# Windows


class WindowsLibraryLoader(LibraryLoader):
    """Library loader for Microsoft Windows"""

    name_formats = ["%s.dll", "lib%s.dll", "%slib.dll", "%s"]

    class Lookup(LibraryLoader.Lookup):
        """Lookup class for Windows libraries..."""

        def __init__(self, path):
            super(WindowsLibraryLoader.Lookup, self).__init__(path)
            self.access["stdcall"] = ctypes.windll.LoadLibrary(path)


# Platform switching

# If your value of sys.platform does not appear in this dict, please contact
# the Ctypesgen maintainers.

loaderclass = {
    "darwin": DarwinLibraryLoader,
    "cygwin": WindowsLibraryLoader,
    "win32": WindowsLibraryLoader,
    "msys": WindowsLibraryLoader,
}

load_library = loaderclass.get(sys.platform, PosixLibraryLoader)()


def add_library_search_dirs(other_dirs):
    """
    Add libraries to search paths.
    If library paths are relative, convert them to absolute with respect to this
    file's directory
    """
    for path in other_dirs:
        if not os.path.isabs(path):
            path = os.path.abspath(path)
        load_library.other_dirs.append(path)


del loaderclass

# End loader

add_library_search_dirs([])

# Begin libraries
_libs["libflecs.dylib"] = load_library("libflecs.dylib")

# 1 libraries
# End libraries

# No modules

NULL = None  # <built-in>

__darwin_intptr_t = c_long  # /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/arm/_types.h: 27

# /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/string.h: 71
if _libs["libflecs.dylib"].has("memcmp", "cdecl"):
    memcmp = _libs["libflecs.dylib"].get("memcmp", "cdecl")
    memcmp.argtypes = [POINTER(None), POINTER(None), c_size_t]
    memcmp.restype = c_int

# /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/string.h: 72
if _libs["libflecs.dylib"].has("memcpy", "cdecl"):
    memcpy = _libs["libflecs.dylib"].get("memcpy", "cdecl")
    memcpy.argtypes = [POINTER(None), POINTER(None), c_size_t]
    memcpy.restype = POINTER(c_ubyte)
    memcpy.errcheck = lambda v, *a: cast(v, c_void_p)

# /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/string.h: 73
if _libs["libflecs.dylib"].has("memmove", "cdecl"):
    memmove = _libs["libflecs.dylib"].get("memmove", "cdecl")
    memmove.argtypes = [POINTER(None), POINTER(None), c_size_t]
    memmove.restype = POINTER(c_ubyte)
    memmove.errcheck = lambda v, *a: cast(v, c_void_p)

# /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/string.h: 74
if _libs["libflecs.dylib"].has("memset", "cdecl"):
    memset = _libs["libflecs.dylib"].get("memset", "cdecl")
    memset.argtypes = [POINTER(None), c_int, c_size_t]
    memset.restype = POINTER(c_ubyte)
    memset.errcheck = lambda v, *a: cast(v, c_void_p)

# /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/string.h: 75
if _libs["libflecs.dylib"].has("strcat", "cdecl"):
    strcat = _libs["libflecs.dylib"].get("strcat", "cdecl")
    strcat.argtypes = [String, String]
    if sizeof(c_int) == sizeof(c_void_p):
        strcat.restype = ReturnString
    else:
        strcat.restype = String
        strcat.errcheck = ReturnString

# /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/string.h: 77
if _libs["libflecs.dylib"].has("strcmp", "cdecl"):
    strcmp = _libs["libflecs.dylib"].get("strcmp", "cdecl")
    strcmp.argtypes = [String, String]
    strcmp.restype = c_int

# /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/string.h: 79
if _libs["libflecs.dylib"].has("strcpy", "cdecl"):
    strcpy = _libs["libflecs.dylib"].get("strcpy", "cdecl")
    strcpy.argtypes = [String, String]
    if sizeof(c_int) == sizeof(c_void_p):
        strcpy.restype = ReturnString
    else:
        strcpy.restype = String
        strcpy.errcheck = ReturnString

# /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/string.h: 82
if _libs["libflecs.dylib"].has("strlen", "cdecl"):
    strlen = _libs["libflecs.dylib"].get("strlen", "cdecl")
    strlen.argtypes = [String]
    strlen.restype = c_size_t

# /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/string.h: 84
if _libs["libflecs.dylib"].has("strncmp", "cdecl"):
    strncmp = _libs["libflecs.dylib"].get("strncmp", "cdecl")
    strncmp.argtypes = [String, String, c_size_t]
    strncmp.restype = c_int

# /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/string.h: 85
if _libs["libflecs.dylib"].has("strncpy", "cdecl"):
    strncpy = _libs["libflecs.dylib"].get("strncpy", "cdecl")
    strncpy.argtypes = [String, String, c_size_t]
    if sizeof(c_int) == sizeof(c_void_p):
        strncpy.restype = ReturnString
    else:
        strncpy.restype = String
        strncpy.errcheck = ReturnString

intptr_t = __darwin_intptr_t  # /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_intptr_t.h: 32

uintptr_t = c_ulong  # /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uintptr_t.h: 34

uint8_t = c_ubyte  # /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/_types/_uint8_t.h: 31

uint16_t = c_ushort  # /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/_types/_uint16_t.h: 31

uint32_t = c_uint  # /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/_types/_uint32_t.h: 31

uint64_t = c_ulonglong  # /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/_types/_uint64_t.h: 31

# /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/alloca.h: 32
for _lib in _libs.values():
    if not _lib.has("alloca", "cdecl"):
        continue
    alloca = _lib.get("alloca", "cdecl")
    alloca.argtypes = [c_size_t]
    alloca.restype = POINTER(c_ubyte)
    alloca.errcheck = lambda v, *a: cast(v, c_void_p)
    break

ecs_flags8_t = uint8_t  # /Users/cnifi/git/pyflecs/flecs.h: 879

ecs_flags16_t = uint16_t  # /Users/cnifi/git/pyflecs/flecs.h: 880

ecs_flags32_t = uint32_t  # /Users/cnifi/git/pyflecs/flecs.h: 881

ecs_flags64_t = uint64_t  # /Users/cnifi/git/pyflecs/flecs.h: 882

ecs_size_t = c_int32  # /Users/cnifi/git/pyflecs/flecs.h: 899


# /Users/cnifi/git/pyflecs/flecs.h: 2008
class struct_ecs_allocator_t(Structure):
    pass


ecs_allocator_t = struct_ecs_allocator_t  # /Users/cnifi/git/pyflecs/flecs.h: 902


# /Users/cnifi/git/pyflecs/flecs.h: 1166
class struct_ecs_vec_t(Structure):
    pass


struct_ecs_vec_t.__slots__ = [
    "array",
    "count",
    "size",
]
struct_ecs_vec_t._fields_ = [
    ("array", POINTER(None)),
    ("count", c_int32),
    ("size", c_int32),
]

ecs_vec_t = struct_ecs_vec_t  # /Users/cnifi/git/pyflecs/flecs.h: 1166

# /Users/cnifi/git/pyflecs/flecs.h: 1169
if _libs["libflecs.dylib"].has("ecs_vec_init", "cdecl"):
    ecs_vec_init = _libs["libflecs.dylib"].get("ecs_vec_init", "cdecl")
    ecs_vec_init.argtypes = [
        POINTER(struct_ecs_allocator_t),
        POINTER(ecs_vec_t),
        ecs_size_t,
        c_int32,
    ]
    ecs_vec_init.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 1176
if _libs["libflecs.dylib"].has("ecs_vec_init_w_dbg_info", "cdecl"):
    ecs_vec_init_w_dbg_info = _libs["libflecs.dylib"].get(
        "ecs_vec_init_w_dbg_info", "cdecl"
    )
    ecs_vec_init_w_dbg_info.argtypes = [
        POINTER(struct_ecs_allocator_t),
        POINTER(ecs_vec_t),
        ecs_size_t,
        c_int32,
        String,
    ]
    ecs_vec_init_w_dbg_info.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 1187
if _libs["libflecs.dylib"].has("ecs_vec_init_if", "cdecl"):
    ecs_vec_init_if = _libs["libflecs.dylib"].get("ecs_vec_init_if", "cdecl")
    ecs_vec_init_if.argtypes = [POINTER(ecs_vec_t), ecs_size_t]
    ecs_vec_init_if.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 1195
if _libs["libflecs.dylib"].has("ecs_vec_fini", "cdecl"):
    ecs_vec_fini = _libs["libflecs.dylib"].get("ecs_vec_fini", "cdecl")
    ecs_vec_fini.argtypes = [
        POINTER(struct_ecs_allocator_t),
        POINTER(ecs_vec_t),
        ecs_size_t,
    ]
    ecs_vec_fini.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 1204
if _libs["libflecs.dylib"].has("ecs_vec_reset", "cdecl"):
    ecs_vec_reset = _libs["libflecs.dylib"].get("ecs_vec_reset", "cdecl")
    ecs_vec_reset.argtypes = [
        POINTER(struct_ecs_allocator_t),
        POINTER(ecs_vec_t),
        ecs_size_t,
    ]
    ecs_vec_reset.restype = POINTER(ecs_vec_t)

# /Users/cnifi/git/pyflecs/flecs.h: 1213
if _libs["libflecs.dylib"].has("ecs_vec_clear", "cdecl"):
    ecs_vec_clear = _libs["libflecs.dylib"].get("ecs_vec_clear", "cdecl")
    ecs_vec_clear.argtypes = [POINTER(ecs_vec_t)]
    ecs_vec_clear.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 1217
if _libs["libflecs.dylib"].has("ecs_vec_append", "cdecl"):
    ecs_vec_append = _libs["libflecs.dylib"].get("ecs_vec_append", "cdecl")
    ecs_vec_append.argtypes = [
        POINTER(struct_ecs_allocator_t),
        POINTER(ecs_vec_t),
        ecs_size_t,
    ]
    ecs_vec_append.restype = POINTER(c_ubyte)
    ecs_vec_append.errcheck = lambda v, *a: cast(v, c_void_p)

# /Users/cnifi/git/pyflecs/flecs.h: 1226
if _libs["libflecs.dylib"].has("ecs_vec_remove", "cdecl"):
    ecs_vec_remove = _libs["libflecs.dylib"].get("ecs_vec_remove", "cdecl")
    ecs_vec_remove.argtypes = [POINTER(ecs_vec_t), ecs_size_t, c_int32]
    ecs_vec_remove.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 1235
if _libs["libflecs.dylib"].has("ecs_vec_remove_ordered", "cdecl"):
    ecs_vec_remove_ordered = _libs["libflecs.dylib"].get(
        "ecs_vec_remove_ordered", "cdecl"
    )
    ecs_vec_remove_ordered.argtypes = [POINTER(ecs_vec_t), ecs_size_t, c_int32]
    ecs_vec_remove_ordered.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 1244
if _libs["libflecs.dylib"].has("ecs_vec_remove_last", "cdecl"):
    ecs_vec_remove_last = _libs["libflecs.dylib"].get("ecs_vec_remove_last", "cdecl")
    ecs_vec_remove_last.argtypes = [POINTER(ecs_vec_t)]
    ecs_vec_remove_last.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 1248
if _libs["libflecs.dylib"].has("ecs_vec_copy", "cdecl"):
    ecs_vec_copy = _libs["libflecs.dylib"].get("ecs_vec_copy", "cdecl")
    ecs_vec_copy.argtypes = [
        POINTER(struct_ecs_allocator_t),
        POINTER(ecs_vec_t),
        ecs_size_t,
    ]
    ecs_vec_copy.restype = ecs_vec_t

# /Users/cnifi/git/pyflecs/flecs.h: 1257
if _libs["libflecs.dylib"].has("ecs_vec_copy_shrink", "cdecl"):
    ecs_vec_copy_shrink = _libs["libflecs.dylib"].get("ecs_vec_copy_shrink", "cdecl")
    ecs_vec_copy_shrink.argtypes = [
        POINTER(struct_ecs_allocator_t),
        POINTER(ecs_vec_t),
        ecs_size_t,
    ]
    ecs_vec_copy_shrink.restype = ecs_vec_t

# /Users/cnifi/git/pyflecs/flecs.h: 1266
if _libs["libflecs.dylib"].has("ecs_vec_reclaim", "cdecl"):
    ecs_vec_reclaim = _libs["libflecs.dylib"].get("ecs_vec_reclaim", "cdecl")
    ecs_vec_reclaim.argtypes = [
        POINTER(struct_ecs_allocator_t),
        POINTER(ecs_vec_t),
        ecs_size_t,
    ]
    ecs_vec_reclaim.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 1275
if _libs["libflecs.dylib"].has("ecs_vec_set_size", "cdecl"):
    ecs_vec_set_size = _libs["libflecs.dylib"].get("ecs_vec_set_size", "cdecl")
    ecs_vec_set_size.argtypes = [
        POINTER(struct_ecs_allocator_t),
        POINTER(ecs_vec_t),
        ecs_size_t,
        c_int32,
    ]
    ecs_vec_set_size.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 1285
if _libs["libflecs.dylib"].has("ecs_vec_set_min_size", "cdecl"):
    ecs_vec_set_min_size = _libs["libflecs.dylib"].get("ecs_vec_set_min_size", "cdecl")
    ecs_vec_set_min_size.argtypes = [
        POINTER(struct_ecs_allocator_t),
        POINTER(ecs_vec_t),
        ecs_size_t,
        c_int32,
    ]
    ecs_vec_set_min_size.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 1295
if _libs["libflecs.dylib"].has("ecs_vec_set_min_count", "cdecl"):
    ecs_vec_set_min_count = _libs["libflecs.dylib"].get(
        "ecs_vec_set_min_count", "cdecl"
    )
    ecs_vec_set_min_count.argtypes = [
        POINTER(struct_ecs_allocator_t),
        POINTER(ecs_vec_t),
        ecs_size_t,
        c_int32,
    ]
    ecs_vec_set_min_count.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 1305
if _libs["libflecs.dylib"].has("ecs_vec_set_min_count_zeromem", "cdecl"):
    ecs_vec_set_min_count_zeromem = _libs["libflecs.dylib"].get(
        "ecs_vec_set_min_count_zeromem", "cdecl"
    )
    ecs_vec_set_min_count_zeromem.argtypes = [
        POINTER(struct_ecs_allocator_t),
        POINTER(ecs_vec_t),
        ecs_size_t,
        c_int32,
    ]
    ecs_vec_set_min_count_zeromem.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 1315
if _libs["libflecs.dylib"].has("ecs_vec_set_count", "cdecl"):
    ecs_vec_set_count = _libs["libflecs.dylib"].get("ecs_vec_set_count", "cdecl")
    ecs_vec_set_count.argtypes = [
        POINTER(struct_ecs_allocator_t),
        POINTER(ecs_vec_t),
        ecs_size_t,
        c_int32,
    ]
    ecs_vec_set_count.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 1325
if _libs["libflecs.dylib"].has("ecs_vec_grow", "cdecl"):
    ecs_vec_grow = _libs["libflecs.dylib"].get("ecs_vec_grow", "cdecl")
    ecs_vec_grow.argtypes = [
        POINTER(struct_ecs_allocator_t),
        POINTER(ecs_vec_t),
        ecs_size_t,
        c_int32,
    ]
    ecs_vec_grow.restype = POINTER(c_ubyte)
    ecs_vec_grow.errcheck = lambda v, *a: cast(v, c_void_p)

# /Users/cnifi/git/pyflecs/flecs.h: 1335
if _libs["libflecs.dylib"].has("ecs_vec_count", "cdecl"):
    ecs_vec_count = _libs["libflecs.dylib"].get("ecs_vec_count", "cdecl")
    ecs_vec_count.argtypes = [POINTER(ecs_vec_t)]
    ecs_vec_count.restype = c_int32

# /Users/cnifi/git/pyflecs/flecs.h: 1339
if _libs["libflecs.dylib"].has("ecs_vec_size", "cdecl"):
    ecs_vec_size = _libs["libflecs.dylib"].get("ecs_vec_size", "cdecl")
    ecs_vec_size.argtypes = [POINTER(ecs_vec_t)]
    ecs_vec_size.restype = c_int32

# /Users/cnifi/git/pyflecs/flecs.h: 1343
if _libs["libflecs.dylib"].has("ecs_vec_get", "cdecl"):
    ecs_vec_get = _libs["libflecs.dylib"].get("ecs_vec_get", "cdecl")
    ecs_vec_get.argtypes = [POINTER(ecs_vec_t), ecs_size_t, c_int32]
    ecs_vec_get.restype = POINTER(c_ubyte)
    ecs_vec_get.errcheck = lambda v, *a: cast(v, c_void_p)

# /Users/cnifi/git/pyflecs/flecs.h: 1352
if _libs["libflecs.dylib"].has("ecs_vec_first", "cdecl"):
    ecs_vec_first = _libs["libflecs.dylib"].get("ecs_vec_first", "cdecl")
    ecs_vec_first.argtypes = [POINTER(ecs_vec_t)]
    ecs_vec_first.restype = POINTER(c_ubyte)
    ecs_vec_first.errcheck = lambda v, *a: cast(v, c_void_p)

# /Users/cnifi/git/pyflecs/flecs.h: 1359
if _libs["libflecs.dylib"].has("ecs_vec_last", "cdecl"):
    ecs_vec_last = _libs["libflecs.dylib"].get("ecs_vec_last", "cdecl")
    ecs_vec_last.argtypes = [POINTER(ecs_vec_t), ecs_size_t]
    ecs_vec_last.restype = POINTER(c_ubyte)
    ecs_vec_last.errcheck = lambda v, *a: cast(v, c_void_p)


# /Users/cnifi/git/pyflecs/flecs.h: 1615
class struct_ecs_block_allocator_t(Structure):
    pass


# /Users/cnifi/git/pyflecs/flecs.h: 1406
class struct_ecs_sparse_t(Structure):
    pass


struct_ecs_sparse_t.__slots__ = [
    "dense",
    "pages",
    "size",
    "count",
    "max_id",
    "allocator",
    "page_allocator",
]
struct_ecs_sparse_t._fields_ = [
    ("dense", ecs_vec_t),
    ("pages", ecs_vec_t),
    ("size", ecs_size_t),
    ("count", c_int32),
    ("max_id", uint64_t),
    ("allocator", POINTER(struct_ecs_allocator_t)),
    ("page_allocator", POINTER(struct_ecs_block_allocator_t)),
]

ecs_sparse_t = struct_ecs_sparse_t  # /Users/cnifi/git/pyflecs/flecs.h: 1406

# /Users/cnifi/git/pyflecs/flecs.h: 1410
if _libs["libflecs.dylib"].has("flecs_sparse_init", "cdecl"):
    flecs_sparse_init = _libs["libflecs.dylib"].get("flecs_sparse_init", "cdecl")
    flecs_sparse_init.argtypes = [
        POINTER(ecs_sparse_t),
        POINTER(struct_ecs_allocator_t),
        POINTER(struct_ecs_block_allocator_t),
        ecs_size_t,
    ]
    flecs_sparse_init.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 1420
if _libs["libflecs.dylib"].has("flecs_sparse_fini", "cdecl"):
    flecs_sparse_fini = _libs["libflecs.dylib"].get("flecs_sparse_fini", "cdecl")
    flecs_sparse_fini.argtypes = [POINTER(ecs_sparse_t)]
    flecs_sparse_fini.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 1425
if _libs["libflecs.dylib"].has("flecs_sparse_clear", "cdecl"):
    flecs_sparse_clear = _libs["libflecs.dylib"].get("flecs_sparse_clear", "cdecl")
    flecs_sparse_clear.argtypes = [POINTER(ecs_sparse_t)]
    flecs_sparse_clear.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 1430
if _libs["libflecs.dylib"].has("flecs_sparse_add", "cdecl"):
    flecs_sparse_add = _libs["libflecs.dylib"].get("flecs_sparse_add", "cdecl")
    flecs_sparse_add.argtypes = [POINTER(ecs_sparse_t), ecs_size_t]
    flecs_sparse_add.restype = POINTER(c_ubyte)
    flecs_sparse_add.errcheck = lambda v, *a: cast(v, c_void_p)

# /Users/cnifi/git/pyflecs/flecs.h: 1439
if _libs["libflecs.dylib"].has("flecs_sparse_last_id", "cdecl"):
    flecs_sparse_last_id = _libs["libflecs.dylib"].get("flecs_sparse_last_id", "cdecl")
    flecs_sparse_last_id.argtypes = [POINTER(ecs_sparse_t)]
    flecs_sparse_last_id.restype = uint64_t

# /Users/cnifi/git/pyflecs/flecs.h: 1444
if _libs["libflecs.dylib"].has("flecs_sparse_new_id", "cdecl"):
    flecs_sparse_new_id = _libs["libflecs.dylib"].get("flecs_sparse_new_id", "cdecl")
    flecs_sparse_new_id.argtypes = [POINTER(ecs_sparse_t)]
    flecs_sparse_new_id.restype = uint64_t

# /Users/cnifi/git/pyflecs/flecs.h: 1449
if _libs["libflecs.dylib"].has("flecs_sparse_remove", "cdecl"):
    flecs_sparse_remove = _libs["libflecs.dylib"].get("flecs_sparse_remove", "cdecl")
    flecs_sparse_remove.argtypes = [POINTER(ecs_sparse_t), ecs_size_t, uint64_t]
    flecs_sparse_remove.restype = c_bool

# /Users/cnifi/git/pyflecs/flecs.h: 1458
if _libs["libflecs.dylib"].has("flecs_sparse_remove_w_gen", "cdecl"):
    flecs_sparse_remove_w_gen = _libs["libflecs.dylib"].get(
        "flecs_sparse_remove_w_gen", "cdecl"
    )
    flecs_sparse_remove_w_gen.argtypes = [POINTER(ecs_sparse_t), ecs_size_t, uint64_t]
    flecs_sparse_remove_w_gen.restype = c_bool

# /Users/cnifi/git/pyflecs/flecs.h: 1468
if _libs["libflecs.dylib"].has("flecs_sparse_is_alive", "cdecl"):
    flecs_sparse_is_alive = _libs["libflecs.dylib"].get(
        "flecs_sparse_is_alive", "cdecl"
    )
    flecs_sparse_is_alive.argtypes = [POINTER(ecs_sparse_t), uint64_t]
    flecs_sparse_is_alive.restype = c_bool

# /Users/cnifi/git/pyflecs/flecs.h: 1475
if _libs["libflecs.dylib"].has("flecs_sparse_get_dense", "cdecl"):
    flecs_sparse_get_dense = _libs["libflecs.dylib"].get(
        "flecs_sparse_get_dense", "cdecl"
    )
    flecs_sparse_get_dense.argtypes = [POINTER(ecs_sparse_t), ecs_size_t, c_int32]
    flecs_sparse_get_dense.restype = POINTER(c_ubyte)
    flecs_sparse_get_dense.errcheck = lambda v, *a: cast(v, c_void_p)

# /Users/cnifi/git/pyflecs/flecs.h: 1485
if _libs["libflecs.dylib"].has("flecs_sparse_count", "cdecl"):
    flecs_sparse_count = _libs["libflecs.dylib"].get("flecs_sparse_count", "cdecl")
    flecs_sparse_count.argtypes = [POINTER(ecs_sparse_t)]
    flecs_sparse_count.restype = c_int32

# /Users/cnifi/git/pyflecs/flecs.h: 1489
if _libs["libflecs.dylib"].has("flecs_sparse_has", "cdecl"):
    flecs_sparse_has = _libs["libflecs.dylib"].get("flecs_sparse_has", "cdecl")
    flecs_sparse_has.argtypes = [POINTER(ecs_sparse_t), uint64_t]
    flecs_sparse_has.restype = c_bool

# /Users/cnifi/git/pyflecs/flecs.h: 1495
if _libs["libflecs.dylib"].has("flecs_sparse_get", "cdecl"):
    flecs_sparse_get = _libs["libflecs.dylib"].get("flecs_sparse_get", "cdecl")
    flecs_sparse_get.argtypes = [POINTER(ecs_sparse_t), ecs_size_t, uint64_t]
    flecs_sparse_get.restype = POINTER(c_ubyte)
    flecs_sparse_get.errcheck = lambda v, *a: cast(v, c_void_p)

# /Users/cnifi/git/pyflecs/flecs.h: 1505
if _libs["libflecs.dylib"].has("flecs_sparse_insert", "cdecl"):
    flecs_sparse_insert = _libs["libflecs.dylib"].get("flecs_sparse_insert", "cdecl")
    flecs_sparse_insert.argtypes = [POINTER(ecs_sparse_t), ecs_size_t, uint64_t]
    flecs_sparse_insert.restype = POINTER(c_ubyte)
    flecs_sparse_insert.errcheck = lambda v, *a: cast(v, c_void_p)

# /Users/cnifi/git/pyflecs/flecs.h: 1515
if _libs["libflecs.dylib"].has("flecs_sparse_ensure", "cdecl"):
    flecs_sparse_ensure = _libs["libflecs.dylib"].get("flecs_sparse_ensure", "cdecl")
    flecs_sparse_ensure.argtypes = [
        POINTER(ecs_sparse_t),
        ecs_size_t,
        uint64_t,
        POINTER(c_bool),
    ]
    flecs_sparse_ensure.restype = POINTER(c_ubyte)
    flecs_sparse_ensure.errcheck = lambda v, *a: cast(v, c_void_p)

# /Users/cnifi/git/pyflecs/flecs.h: 1526
if _libs["libflecs.dylib"].has("flecs_sparse_ensure_fast", "cdecl"):
    flecs_sparse_ensure_fast = _libs["libflecs.dylib"].get(
        "flecs_sparse_ensure_fast", "cdecl"
    )
    flecs_sparse_ensure_fast.argtypes = [POINTER(ecs_sparse_t), ecs_size_t, uint64_t]
    flecs_sparse_ensure_fast.restype = POINTER(c_ubyte)
    flecs_sparse_ensure_fast.errcheck = lambda v, *a: cast(v, c_void_p)

# /Users/cnifi/git/pyflecs/flecs.h: 1536
if _libs["libflecs.dylib"].has("flecs_sparse_ids", "cdecl"):
    flecs_sparse_ids = _libs["libflecs.dylib"].get("flecs_sparse_ids", "cdecl")
    flecs_sparse_ids.argtypes = [POINTER(ecs_sparse_t)]
    flecs_sparse_ids.restype = POINTER(uint64_t)

# /Users/cnifi/git/pyflecs/flecs.h: 1540
if _libs["libflecs.dylib"].has("flecs_sparse_shrink", "cdecl"):
    flecs_sparse_shrink = _libs["libflecs.dylib"].get("flecs_sparse_shrink", "cdecl")
    flecs_sparse_shrink.argtypes = [POINTER(ecs_sparse_t)]
    flecs_sparse_shrink.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 1548
if _libs["libflecs.dylib"].has("ecs_sparse_init", "cdecl"):
    ecs_sparse_init = _libs["libflecs.dylib"].get("ecs_sparse_init", "cdecl")
    ecs_sparse_init.argtypes = [POINTER(ecs_sparse_t), ecs_size_t]
    ecs_sparse_init.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 1556
if _libs["libflecs.dylib"].has("ecs_sparse_add", "cdecl"):
    ecs_sparse_add = _libs["libflecs.dylib"].get("ecs_sparse_add", "cdecl")
    ecs_sparse_add.argtypes = [POINTER(ecs_sparse_t), ecs_size_t]
    ecs_sparse_add.restype = POINTER(c_ubyte)
    ecs_sparse_add.errcheck = lambda v, *a: cast(v, c_void_p)

# /Users/cnifi/git/pyflecs/flecs.h: 1564
if _libs["libflecs.dylib"].has("ecs_sparse_last_id", "cdecl"):
    ecs_sparse_last_id = _libs["libflecs.dylib"].get("ecs_sparse_last_id", "cdecl")
    ecs_sparse_last_id.argtypes = [POINTER(ecs_sparse_t)]
    ecs_sparse_last_id.restype = uint64_t

# /Users/cnifi/git/pyflecs/flecs.h: 1568
if _libs["libflecs.dylib"].has("ecs_sparse_count", "cdecl"):
    ecs_sparse_count = _libs["libflecs.dylib"].get("ecs_sparse_count", "cdecl")
    ecs_sparse_count.argtypes = [POINTER(ecs_sparse_t)]
    ecs_sparse_count.restype = c_int32

# /Users/cnifi/git/pyflecs/flecs.h: 1572
if _libs["libflecs.dylib"].has("ecs_sparse_get_dense", "cdecl"):
    ecs_sparse_get_dense = _libs["libflecs.dylib"].get("ecs_sparse_get_dense", "cdecl")
    ecs_sparse_get_dense.argtypes = [POINTER(ecs_sparse_t), ecs_size_t, c_int32]
    ecs_sparse_get_dense.restype = POINTER(c_ubyte)
    ecs_sparse_get_dense.errcheck = lambda v, *a: cast(v, c_void_p)

# /Users/cnifi/git/pyflecs/flecs.h: 1581
if _libs["libflecs.dylib"].has("ecs_sparse_get", "cdecl"):
    ecs_sparse_get = _libs["libflecs.dylib"].get("ecs_sparse_get", "cdecl")
    ecs_sparse_get.argtypes = [POINTER(ecs_sparse_t), ecs_size_t, uint64_t]
    ecs_sparse_get.restype = POINTER(c_ubyte)
    ecs_sparse_get.errcheck = lambda v, *a: cast(v, c_void_p)


# /Users/cnifi/git/pyflecs/flecs.h: 1830
class struct_ecs_map_t(Structure):
    pass


ecs_map_t = struct_ecs_map_t  # /Users/cnifi/git/pyflecs/flecs.h: 1604


# /Users/cnifi/git/pyflecs/flecs.h: 1606
class struct_ecs_block_allocator_block_t(Structure):
    pass


struct_ecs_block_allocator_block_t.__slots__ = [
    "memory",
    "next",
]
struct_ecs_block_allocator_block_t._fields_ = [
    ("memory", POINTER(None)),
    ("next", POINTER(struct_ecs_block_allocator_block_t)),
]

ecs_block_allocator_block_t = (
    struct_ecs_block_allocator_block_t  # /Users/cnifi/git/pyflecs/flecs.h: 1609
)


# /Users/cnifi/git/pyflecs/flecs.h: 1611
class struct_ecs_block_allocator_chunk_header_t(Structure):
    pass


struct_ecs_block_allocator_chunk_header_t.__slots__ = [
    "next",
]
struct_ecs_block_allocator_chunk_header_t._fields_ = [
    ("next", POINTER(struct_ecs_block_allocator_chunk_header_t)),
]

ecs_block_allocator_chunk_header_t = (
    struct_ecs_block_allocator_chunk_header_t  # /Users/cnifi/git/pyflecs/flecs.h: 1613
)

struct_ecs_block_allocator_t.__slots__ = [
    "data_size",
    "chunk_size",
    "chunks_per_block",
    "block_size",
    "head",
    "block_head",
]
struct_ecs_block_allocator_t._fields_ = [
    ("data_size", c_int32),
    ("chunk_size", c_int32),
    ("chunks_per_block", c_int32),
    ("block_size", c_int32),
    ("head", POINTER(ecs_block_allocator_chunk_header_t)),
    ("block_head", POINTER(ecs_block_allocator_block_t)),
]

ecs_block_allocator_t = (
    struct_ecs_block_allocator_t  # /Users/cnifi/git/pyflecs/flecs.h: 1628
)

# /Users/cnifi/git/pyflecs/flecs.h: 1631
if _libs["libflecs.dylib"].has("flecs_ballocator_init", "cdecl"):
    flecs_ballocator_init = _libs["libflecs.dylib"].get(
        "flecs_ballocator_init", "cdecl"
    )
    flecs_ballocator_init.argtypes = [POINTER(ecs_block_allocator_t), ecs_size_t]
    flecs_ballocator_init.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 1641
if _libs["libflecs.dylib"].has("flecs_ballocator_new", "cdecl"):
    flecs_ballocator_new = _libs["libflecs.dylib"].get("flecs_ballocator_new", "cdecl")
    flecs_ballocator_new.argtypes = [ecs_size_t]
    flecs_ballocator_new.restype = POINTER(ecs_block_allocator_t)

# /Users/cnifi/git/pyflecs/flecs.h: 1650
if _libs["libflecs.dylib"].has("flecs_ballocator_fini", "cdecl"):
    flecs_ballocator_fini = _libs["libflecs.dylib"].get(
        "flecs_ballocator_fini", "cdecl"
    )
    flecs_ballocator_fini.argtypes = [POINTER(ecs_block_allocator_t)]
    flecs_ballocator_fini.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 1654
if _libs["libflecs.dylib"].has("flecs_ballocator_free", "cdecl"):
    flecs_ballocator_free = _libs["libflecs.dylib"].get(
        "flecs_ballocator_free", "cdecl"
    )
    flecs_ballocator_free.argtypes = [POINTER(ecs_block_allocator_t)]
    flecs_ballocator_free.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 1658
if _libs["libflecs.dylib"].has("flecs_balloc", "cdecl"):
    flecs_balloc = _libs["libflecs.dylib"].get("flecs_balloc", "cdecl")
    flecs_balloc.argtypes = [POINTER(ecs_block_allocator_t)]
    flecs_balloc.restype = POINTER(c_ubyte)
    flecs_balloc.errcheck = lambda v, *a: cast(v, c_void_p)

# /Users/cnifi/git/pyflecs/flecs.h: 1662
if _libs["libflecs.dylib"].has("flecs_balloc_w_dbg_info", "cdecl"):
    flecs_balloc_w_dbg_info = _libs["libflecs.dylib"].get(
        "flecs_balloc_w_dbg_info", "cdecl"
    )
    flecs_balloc_w_dbg_info.argtypes = [POINTER(ecs_block_allocator_t), String]
    flecs_balloc_w_dbg_info.restype = POINTER(c_ubyte)
    flecs_balloc_w_dbg_info.errcheck = lambda v, *a: cast(v, c_void_p)

# /Users/cnifi/git/pyflecs/flecs.h: 1667
if _libs["libflecs.dylib"].has("flecs_bcalloc", "cdecl"):
    flecs_bcalloc = _libs["libflecs.dylib"].get("flecs_bcalloc", "cdecl")
    flecs_bcalloc.argtypes = [POINTER(ecs_block_allocator_t)]
    flecs_bcalloc.restype = POINTER(c_ubyte)
    flecs_bcalloc.errcheck = lambda v, *a: cast(v, c_void_p)

# /Users/cnifi/git/pyflecs/flecs.h: 1671
if _libs["libflecs.dylib"].has("flecs_bcalloc_w_dbg_info", "cdecl"):
    flecs_bcalloc_w_dbg_info = _libs["libflecs.dylib"].get(
        "flecs_bcalloc_w_dbg_info", "cdecl"
    )
    flecs_bcalloc_w_dbg_info.argtypes = [POINTER(ecs_block_allocator_t), String]
    flecs_bcalloc_w_dbg_info.restype = POINTER(c_ubyte)
    flecs_bcalloc_w_dbg_info.errcheck = lambda v, *a: cast(v, c_void_p)

# /Users/cnifi/git/pyflecs/flecs.h: 1676
if _libs["libflecs.dylib"].has("flecs_bfree", "cdecl"):
    flecs_bfree = _libs["libflecs.dylib"].get("flecs_bfree", "cdecl")
    flecs_bfree.argtypes = [POINTER(ecs_block_allocator_t), POINTER(None)]
    flecs_bfree.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 1681
if _libs["libflecs.dylib"].has("flecs_bfree_w_dbg_info", "cdecl"):
    flecs_bfree_w_dbg_info = _libs["libflecs.dylib"].get(
        "flecs_bfree_w_dbg_info", "cdecl"
    )
    flecs_bfree_w_dbg_info.argtypes = [
        POINTER(ecs_block_allocator_t),
        POINTER(None),
        String,
    ]
    flecs_bfree_w_dbg_info.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 1687
if _libs["libflecs.dylib"].has("flecs_brealloc", "cdecl"):
    flecs_brealloc = _libs["libflecs.dylib"].get("flecs_brealloc", "cdecl")
    flecs_brealloc.argtypes = [
        POINTER(ecs_block_allocator_t),
        POINTER(ecs_block_allocator_t),
        POINTER(None),
    ]
    flecs_brealloc.restype = POINTER(c_ubyte)
    flecs_brealloc.errcheck = lambda v, *a: cast(v, c_void_p)

# /Users/cnifi/git/pyflecs/flecs.h: 1693
if _libs["libflecs.dylib"].has("flecs_brealloc_w_dbg_info", "cdecl"):
    flecs_brealloc_w_dbg_info = _libs["libflecs.dylib"].get(
        "flecs_brealloc_w_dbg_info", "cdecl"
    )
    flecs_brealloc_w_dbg_info.argtypes = [
        POINTER(ecs_block_allocator_t),
        POINTER(ecs_block_allocator_t),
        POINTER(None),
        String,
    ]
    flecs_brealloc_w_dbg_info.restype = POINTER(c_ubyte)
    flecs_brealloc_w_dbg_info.errcheck = lambda v, *a: cast(v, c_void_p)

# /Users/cnifi/git/pyflecs/flecs.h: 1700
if _libs["libflecs.dylib"].has("flecs_bdup", "cdecl"):
    flecs_bdup = _libs["libflecs.dylib"].get("flecs_bdup", "cdecl")
    flecs_bdup.argtypes = [POINTER(ecs_block_allocator_t), POINTER(None)]
    flecs_bdup.restype = POINTER(c_ubyte)
    flecs_bdup.errcheck = lambda v, *a: cast(v, c_void_p)


# /Users/cnifi/git/pyflecs/flecs.h: 1716
class struct_ecs_stack_page_t(Structure):
    pass


struct_ecs_stack_page_t.__slots__ = [
    "data",
    "next",
    "sp",
    "id",
]
struct_ecs_stack_page_t._fields_ = [
    ("data", POINTER(None)),
    ("next", POINTER(struct_ecs_stack_page_t)),
    ("sp", c_int16),
    ("id", uint32_t),
]

ecs_stack_page_t = struct_ecs_stack_page_t  # /Users/cnifi/git/pyflecs/flecs.h: 1721


# /Users/cnifi/git/pyflecs/flecs.h: 1723
class struct_ecs_stack_cursor_t(Structure):
    pass


# /Users/cnifi/git/pyflecs/flecs.h: 1733
class struct_ecs_stack_t(Structure):
    pass


struct_ecs_stack_cursor_t.__slots__ = [
    "prev",
    "page",
    "sp",
    "is_free",
    "owner",
]
struct_ecs_stack_cursor_t._fields_ = [
    ("prev", POINTER(struct_ecs_stack_cursor_t)),
    ("page", POINTER(struct_ecs_stack_page_t)),
    ("sp", c_int16),
    ("is_free", c_bool),
    ("owner", POINTER(struct_ecs_stack_t)),
]

ecs_stack_cursor_t = struct_ecs_stack_cursor_t  # /Users/cnifi/git/pyflecs/flecs.h: 1731

struct_ecs_stack_t.__slots__ = [
    "first",
    "tail_page",
    "tail_cursor",
    "cursor_count",
]
struct_ecs_stack_t._fields_ = [
    ("first", POINTER(ecs_stack_page_t)),
    ("tail_page", POINTER(ecs_stack_page_t)),
    ("tail_cursor", POINTER(ecs_stack_cursor_t)),
    ("cursor_count", c_int32),
]

ecs_stack_t = struct_ecs_stack_t  # /Users/cnifi/git/pyflecs/flecs.h: 1740

# /Users/cnifi/git/pyflecs/flecs.h: 1746
if _libs["libflecs.dylib"].has("flecs_stack_init", "cdecl"):
    flecs_stack_init = _libs["libflecs.dylib"].get("flecs_stack_init", "cdecl")
    flecs_stack_init.argtypes = [POINTER(ecs_stack_t)]
    flecs_stack_init.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 1750
if _libs["libflecs.dylib"].has("flecs_stack_fini", "cdecl"):
    flecs_stack_fini = _libs["libflecs.dylib"].get("flecs_stack_fini", "cdecl")
    flecs_stack_fini.argtypes = [POINTER(ecs_stack_t)]
    flecs_stack_fini.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 1754
if _libs["libflecs.dylib"].has("flecs_stack_alloc", "cdecl"):
    flecs_stack_alloc = _libs["libflecs.dylib"].get("flecs_stack_alloc", "cdecl")
    flecs_stack_alloc.argtypes = [POINTER(ecs_stack_t), ecs_size_t, ecs_size_t]
    flecs_stack_alloc.restype = POINTER(c_ubyte)
    flecs_stack_alloc.errcheck = lambda v, *a: cast(v, c_void_p)

# /Users/cnifi/git/pyflecs/flecs.h: 1766
if _libs["libflecs.dylib"].has("flecs_stack_calloc", "cdecl"):
    flecs_stack_calloc = _libs["libflecs.dylib"].get("flecs_stack_calloc", "cdecl")
    flecs_stack_calloc.argtypes = [POINTER(ecs_stack_t), ecs_size_t, ecs_size_t]
    flecs_stack_calloc.restype = POINTER(c_ubyte)
    flecs_stack_calloc.errcheck = lambda v, *a: cast(v, c_void_p)

# /Users/cnifi/git/pyflecs/flecs.h: 1778
if _libs["libflecs.dylib"].has("flecs_stack_free", "cdecl"):
    flecs_stack_free = _libs["libflecs.dylib"].get("flecs_stack_free", "cdecl")
    flecs_stack_free.argtypes = [POINTER(None), ecs_size_t]
    flecs_stack_free.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 1788
if _libs["libflecs.dylib"].has("flecs_stack_reset", "cdecl"):
    flecs_stack_reset = _libs["libflecs.dylib"].get("flecs_stack_reset", "cdecl")
    flecs_stack_reset.argtypes = [POINTER(ecs_stack_t)]
    flecs_stack_reset.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 1792
if _libs["libflecs.dylib"].has("flecs_stack_get_cursor", "cdecl"):
    flecs_stack_get_cursor = _libs["libflecs.dylib"].get(
        "flecs_stack_get_cursor", "cdecl"
    )
    flecs_stack_get_cursor.argtypes = [POINTER(ecs_stack_t)]
    flecs_stack_get_cursor.restype = POINTER(ecs_stack_cursor_t)

# /Users/cnifi/git/pyflecs/flecs.h: 1796
if _libs["libflecs.dylib"].has("flecs_stack_restore_cursor", "cdecl"):
    flecs_stack_restore_cursor = _libs["libflecs.dylib"].get(
        "flecs_stack_restore_cursor", "cdecl"
    )
    flecs_stack_restore_cursor.argtypes = [
        POINTER(ecs_stack_t),
        POINTER(ecs_stack_cursor_t),
    ]
    flecs_stack_restore_cursor.restype = None

ecs_map_data_t = uint64_t  # /Users/cnifi/git/pyflecs/flecs.h: 1815

ecs_map_key_t = ecs_map_data_t  # /Users/cnifi/git/pyflecs/flecs.h: 1816

ecs_map_val_t = ecs_map_data_t  # /Users/cnifi/git/pyflecs/flecs.h: 1817


# /Users/cnifi/git/pyflecs/flecs.h: 1820
class struct_ecs_bucket_entry_t(Structure):
    pass


struct_ecs_bucket_entry_t.__slots__ = [
    "key",
    "value",
    "next",
]
struct_ecs_bucket_entry_t._fields_ = [
    ("key", ecs_map_key_t),
    ("value", ecs_map_val_t),
    ("next", POINTER(struct_ecs_bucket_entry_t)),
]

ecs_bucket_entry_t = struct_ecs_bucket_entry_t  # /Users/cnifi/git/pyflecs/flecs.h: 1824


# /Users/cnifi/git/pyflecs/flecs.h: 1828
class struct_ecs_bucket_t(Structure):
    pass


struct_ecs_bucket_t.__slots__ = [
    "first",
]
struct_ecs_bucket_t._fields_ = [
    ("first", POINTER(ecs_bucket_entry_t)),
]

ecs_bucket_t = struct_ecs_bucket_t  # /Users/cnifi/git/pyflecs/flecs.h: 1828

struct_ecs_map_t.__slots__ = [
    "buckets",
    "bucket_count",
    "count",
    "bucket_shift",
    "allocator",
]
struct_ecs_map_t._fields_ = [
    ("buckets", POINTER(ecs_bucket_t)),
    ("bucket_count", c_int32),
    ("count", c_uint, 26),
    ("bucket_shift", c_uint, 6),
    ("allocator", POINTER(struct_ecs_allocator_t)),
]


# /Users/cnifi/git/pyflecs/flecs.h: 1843
class struct_ecs_map_iter_t(Structure):
    pass


struct_ecs_map_iter_t.__slots__ = [
    "map",
    "bucket",
    "entry",
    "res",
]
struct_ecs_map_iter_t._fields_ = [
    ("map", POINTER(ecs_map_t)),
    ("bucket", POINTER(ecs_bucket_t)),
    ("entry", POINTER(ecs_bucket_entry_t)),
    ("res", POINTER(ecs_map_data_t)),
]

ecs_map_iter_t = struct_ecs_map_iter_t  # /Users/cnifi/git/pyflecs/flecs.h: 1843


# /Users/cnifi/git/pyflecs/flecs.h: 1848
class struct_ecs_map_params_t(Structure):
    pass


struct_ecs_map_params_t.__slots__ = [
    "allocator",
    "entry_allocator",
]
struct_ecs_map_params_t._fields_ = [
    ("allocator", POINTER(struct_ecs_allocator_t)),
    ("entry_allocator", struct_ecs_block_allocator_t),
]

ecs_map_params_t = struct_ecs_map_params_t  # /Users/cnifi/git/pyflecs/flecs.h: 1848

# /Users/cnifi/git/pyflecs/flecs.h: 1859
if _libs["libflecs.dylib"].has("ecs_map_params_init", "cdecl"):
    ecs_map_params_init = _libs["libflecs.dylib"].get("ecs_map_params_init", "cdecl")
    ecs_map_params_init.argtypes = [
        POINTER(ecs_map_params_t),
        POINTER(struct_ecs_allocator_t),
    ]
    ecs_map_params_init.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 1864
if _libs["libflecs.dylib"].has("ecs_map_params_fini", "cdecl"):
    ecs_map_params_fini = _libs["libflecs.dylib"].get("ecs_map_params_fini", "cdecl")
    ecs_map_params_fini.argtypes = [POINTER(ecs_map_params_t)]
    ecs_map_params_fini.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 1869
if _libs["libflecs.dylib"].has("ecs_map_init", "cdecl"):
    ecs_map_init = _libs["libflecs.dylib"].get("ecs_map_init", "cdecl")
    ecs_map_init.argtypes = [POINTER(ecs_map_t), POINTER(struct_ecs_allocator_t)]
    ecs_map_init.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 1875
if _libs["libflecs.dylib"].has("ecs_map_init_w_params", "cdecl"):
    ecs_map_init_w_params = _libs["libflecs.dylib"].get(
        "ecs_map_init_w_params", "cdecl"
    )
    ecs_map_init_w_params.argtypes = [POINTER(ecs_map_t), POINTER(ecs_map_params_t)]
    ecs_map_init_w_params.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 1881
if _libs["libflecs.dylib"].has("ecs_map_init_if", "cdecl"):
    ecs_map_init_if = _libs["libflecs.dylib"].get("ecs_map_init_if", "cdecl")
    ecs_map_init_if.argtypes = [POINTER(ecs_map_t), POINTER(struct_ecs_allocator_t)]
    ecs_map_init_if.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 1886
if _libs["libflecs.dylib"].has("ecs_map_init_w_params_if", "cdecl"):
    ecs_map_init_w_params_if = _libs["libflecs.dylib"].get(
        "ecs_map_init_w_params_if", "cdecl"
    )
    ecs_map_init_w_params_if.argtypes = [POINTER(ecs_map_t), POINTER(ecs_map_params_t)]
    ecs_map_init_w_params_if.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 1892
if _libs["libflecs.dylib"].has("ecs_map_fini", "cdecl"):
    ecs_map_fini = _libs["libflecs.dylib"].get("ecs_map_fini", "cdecl")
    ecs_map_fini.argtypes = [POINTER(ecs_map_t)]
    ecs_map_fini.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 1897
if _libs["libflecs.dylib"].has("ecs_map_get", "cdecl"):
    ecs_map_get = _libs["libflecs.dylib"].get("ecs_map_get", "cdecl")
    ecs_map_get.argtypes = [POINTER(ecs_map_t), ecs_map_key_t]
    ecs_map_get.restype = POINTER(ecs_map_val_t)

# /Users/cnifi/git/pyflecs/flecs.h: 1903
if _libs["libflecs.dylib"].has("ecs_map_get_deref_", "cdecl"):
    ecs_map_get_deref_ = _libs["libflecs.dylib"].get("ecs_map_get_deref_", "cdecl")
    ecs_map_get_deref_.argtypes = [POINTER(ecs_map_t), ecs_map_key_t]
    ecs_map_get_deref_.restype = POINTER(c_ubyte)
    ecs_map_get_deref_.errcheck = lambda v, *a: cast(v, c_void_p)

# /Users/cnifi/git/pyflecs/flecs.h: 1909
if _libs["libflecs.dylib"].has("ecs_map_ensure", "cdecl"):
    ecs_map_ensure = _libs["libflecs.dylib"].get("ecs_map_ensure", "cdecl")
    ecs_map_ensure.argtypes = [POINTER(ecs_map_t), ecs_map_key_t]
    ecs_map_ensure.restype = POINTER(ecs_map_val_t)

# /Users/cnifi/git/pyflecs/flecs.h: 1915
if _libs["libflecs.dylib"].has("ecs_map_ensure_alloc", "cdecl"):
    ecs_map_ensure_alloc = _libs["libflecs.dylib"].get("ecs_map_ensure_alloc", "cdecl")
    ecs_map_ensure_alloc.argtypes = [POINTER(ecs_map_t), ecs_size_t, ecs_map_key_t]
    ecs_map_ensure_alloc.restype = POINTER(c_ubyte)
    ecs_map_ensure_alloc.errcheck = lambda v, *a: cast(v, c_void_p)

# /Users/cnifi/git/pyflecs/flecs.h: 1922
if _libs["libflecs.dylib"].has("ecs_map_insert", "cdecl"):
    ecs_map_insert = _libs["libflecs.dylib"].get("ecs_map_insert", "cdecl")
    ecs_map_insert.argtypes = [POINTER(ecs_map_t), ecs_map_key_t, ecs_map_val_t]
    ecs_map_insert.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 1929
if _libs["libflecs.dylib"].has("ecs_map_insert_alloc", "cdecl"):
    ecs_map_insert_alloc = _libs["libflecs.dylib"].get("ecs_map_insert_alloc", "cdecl")
    ecs_map_insert_alloc.argtypes = [POINTER(ecs_map_t), ecs_size_t, ecs_map_key_t]
    ecs_map_insert_alloc.restype = POINTER(c_ubyte)
    ecs_map_insert_alloc.errcheck = lambda v, *a: cast(v, c_void_p)

# /Users/cnifi/git/pyflecs/flecs.h: 1936
if _libs["libflecs.dylib"].has("ecs_map_remove", "cdecl"):
    ecs_map_remove = _libs["libflecs.dylib"].get("ecs_map_remove", "cdecl")
    ecs_map_remove.argtypes = [POINTER(ecs_map_t), ecs_map_key_t]
    ecs_map_remove.restype = ecs_map_val_t

# /Users/cnifi/git/pyflecs/flecs.h: 1942
if _libs["libflecs.dylib"].has("ecs_map_remove_free", "cdecl"):
    ecs_map_remove_free = _libs["libflecs.dylib"].get("ecs_map_remove_free", "cdecl")
    ecs_map_remove_free.argtypes = [POINTER(ecs_map_t), ecs_map_key_t]
    ecs_map_remove_free.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 1948
if _libs["libflecs.dylib"].has("ecs_map_clear", "cdecl"):
    ecs_map_clear = _libs["libflecs.dylib"].get("ecs_map_clear", "cdecl")
    ecs_map_clear.argtypes = [POINTER(ecs_map_t)]
    ecs_map_clear.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 1959
if _libs["libflecs.dylib"].has("ecs_map_iter", "cdecl"):
    ecs_map_iter = _libs["libflecs.dylib"].get("ecs_map_iter", "cdecl")
    ecs_map_iter.argtypes = [POINTER(ecs_map_t)]
    ecs_map_iter.restype = ecs_map_iter_t

# /Users/cnifi/git/pyflecs/flecs.h: 1964
if _libs["libflecs.dylib"].has("ecs_map_next", "cdecl"):
    ecs_map_next = _libs["libflecs.dylib"].get("ecs_map_next", "cdecl")
    ecs_map_next.argtypes = [POINTER(ecs_map_iter_t)]
    ecs_map_next.restype = c_bool

# /Users/cnifi/git/pyflecs/flecs.h: 1969
if _libs["libflecs.dylib"].has("ecs_map_copy", "cdecl"):
    ecs_map_copy = _libs["libflecs.dylib"].get("ecs_map_copy", "cdecl")
    ecs_map_copy.argtypes = [POINTER(ecs_map_t), POINTER(ecs_map_t)]
    ecs_map_copy.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 2003
try:
    ecs_block_allocator_alloc_count = (c_int64).in_dll(
        _libs["libflecs.dylib"], "ecs_block_allocator_alloc_count"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 2004
try:
    ecs_block_allocator_free_count = (c_int64).in_dll(
        _libs["libflecs.dylib"], "ecs_block_allocator_free_count"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 2005
try:
    ecs_stack_allocator_alloc_count = (c_int64).in_dll(
        _libs["libflecs.dylib"], "ecs_stack_allocator_alloc_count"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 2006
try:
    ecs_stack_allocator_free_count = (c_int64).in_dll(
        _libs["libflecs.dylib"], "ecs_stack_allocator_free_count"
    )
except:
    pass

struct_ecs_allocator_t.__slots__ = [
    "chunks",
    "sizes",
]
struct_ecs_allocator_t._fields_ = [
    ("chunks", ecs_block_allocator_t),
    ("sizes", struct_ecs_sparse_t),
]

# /Users/cnifi/git/pyflecs/flecs.h: 2018
if _libs["libflecs.dylib"].has("flecs_allocator_init", "cdecl"):
    flecs_allocator_init = _libs["libflecs.dylib"].get("flecs_allocator_init", "cdecl")
    flecs_allocator_init.argtypes = [POINTER(ecs_allocator_t)]
    flecs_allocator_init.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 2022
if _libs["libflecs.dylib"].has("flecs_allocator_fini", "cdecl"):
    flecs_allocator_fini = _libs["libflecs.dylib"].get("flecs_allocator_fini", "cdecl")
    flecs_allocator_fini.argtypes = [POINTER(ecs_allocator_t)]
    flecs_allocator_fini.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 2026
if _libs["libflecs.dylib"].has("flecs_allocator_get", "cdecl"):
    flecs_allocator_get = _libs["libflecs.dylib"].get("flecs_allocator_get", "cdecl")
    flecs_allocator_get.argtypes = [POINTER(ecs_allocator_t), ecs_size_t]
    flecs_allocator_get.restype = POINTER(ecs_block_allocator_t)

# /Users/cnifi/git/pyflecs/flecs.h: 2031
if _libs["libflecs.dylib"].has("flecs_strdup", "cdecl"):
    flecs_strdup = _libs["libflecs.dylib"].get("flecs_strdup", "cdecl")
    flecs_strdup.argtypes = [POINTER(ecs_allocator_t), String]
    if sizeof(c_int) == sizeof(c_void_p):
        flecs_strdup.restype = ReturnString
    else:
        flecs_strdup.restype = String
        flecs_strdup.errcheck = ReturnString

# /Users/cnifi/git/pyflecs/flecs.h: 2036
if _libs["libflecs.dylib"].has("flecs_strfree", "cdecl"):
    flecs_strfree = _libs["libflecs.dylib"].get("flecs_strfree", "cdecl")
    flecs_strfree.argtypes = [POINTER(ecs_allocator_t), String]
    flecs_strfree.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 2041
if _libs["libflecs.dylib"].has("flecs_dup", "cdecl"):
    flecs_dup = _libs["libflecs.dylib"].get("flecs_dup", "cdecl")
    flecs_dup.argtypes = [POINTER(ecs_allocator_t), ecs_size_t, POINTER(None)]
    flecs_dup.restype = POINTER(c_ubyte)
    flecs_dup.errcheck = lambda v, *a: cast(v, c_void_p)


# /Users/cnifi/git/pyflecs/flecs.h: 2151
class struct_ecs_strbuf_list_elem(Structure):
    pass


struct_ecs_strbuf_list_elem.__slots__ = [
    "count",
    "separator",
]
struct_ecs_strbuf_list_elem._fields_ = [
    ("count", c_int32),
    ("separator", String),
]

ecs_strbuf_list_elem = (
    struct_ecs_strbuf_list_elem  # /Users/cnifi/git/pyflecs/flecs.h: 2151
)


# /Users/cnifi/git/pyflecs/flecs.h: 2162
class struct_ecs_strbuf_t(Structure):
    pass


struct_ecs_strbuf_t.__slots__ = [
    "content",
    "length",
    "size",
    "list_stack",
    "list_sp",
    "small_string",
]
struct_ecs_strbuf_t._fields_ = [
    ("content", String),
    ("length", ecs_size_t),
    ("size", ecs_size_t),
    ("list_stack", ecs_strbuf_list_elem * int(32)),
    ("list_sp", c_int32),
    ("small_string", c_char * int(512)),
]

ecs_strbuf_t = struct_ecs_strbuf_t  # /Users/cnifi/git/pyflecs/flecs.h: 2162

# /Users/cnifi/git/pyflecs/flecs.h: 2167
if _libs["libflecs.dylib"].has("ecs_strbuf_append", "cdecl"):
    _func = _libs["libflecs.dylib"].get("ecs_strbuf_append", "cdecl")
    _restype = None
    _errcheck = None
    _argtypes = [POINTER(ecs_strbuf_t), String]
    ecs_strbuf_append = _variadic_function(_func, _restype, _argtypes, _errcheck)

# /Users/cnifi/git/pyflecs/flecs.h: 2175
if _libs["libflecs.dylib"].has("ecs_strbuf_vappend", "cdecl"):
    ecs_strbuf_vappend = _libs["libflecs.dylib"].get("ecs_strbuf_vappend", "cdecl")
    ecs_strbuf_vappend.argtypes = [POINTER(ecs_strbuf_t), String, c_void_p]
    ecs_strbuf_vappend.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 2183
if _libs["libflecs.dylib"].has("ecs_strbuf_appendstr", "cdecl"):
    ecs_strbuf_appendstr = _libs["libflecs.dylib"].get("ecs_strbuf_appendstr", "cdecl")
    ecs_strbuf_appendstr.argtypes = [POINTER(ecs_strbuf_t), String]
    ecs_strbuf_appendstr.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 2190
if _libs["libflecs.dylib"].has("ecs_strbuf_appendch", "cdecl"):
    ecs_strbuf_appendch = _libs["libflecs.dylib"].get("ecs_strbuf_appendch", "cdecl")
    ecs_strbuf_appendch.argtypes = [POINTER(ecs_strbuf_t), c_char]
    ecs_strbuf_appendch.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 2197
if _libs["libflecs.dylib"].has("ecs_strbuf_appendint", "cdecl"):
    ecs_strbuf_appendint = _libs["libflecs.dylib"].get("ecs_strbuf_appendint", "cdecl")
    ecs_strbuf_appendint.argtypes = [POINTER(ecs_strbuf_t), c_int64]
    ecs_strbuf_appendint.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 2204
if _libs["libflecs.dylib"].has("ecs_strbuf_appendflt", "cdecl"):
    ecs_strbuf_appendflt = _libs["libflecs.dylib"].get("ecs_strbuf_appendflt", "cdecl")
    ecs_strbuf_appendflt.argtypes = [POINTER(ecs_strbuf_t), c_double, c_char]
    ecs_strbuf_appendflt.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 2212
if _libs["libflecs.dylib"].has("ecs_strbuf_appendbool", "cdecl"):
    ecs_strbuf_appendbool = _libs["libflecs.dylib"].get(
        "ecs_strbuf_appendbool", "cdecl"
    )
    ecs_strbuf_appendbool.argtypes = [POINTER(ecs_strbuf_t), c_bool]
    ecs_strbuf_appendbool.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 2219
if _libs["libflecs.dylib"].has("ecs_strbuf_mergebuff", "cdecl"):
    ecs_strbuf_mergebuff = _libs["libflecs.dylib"].get("ecs_strbuf_mergebuff", "cdecl")
    ecs_strbuf_mergebuff.argtypes = [POINTER(ecs_strbuf_t), POINTER(ecs_strbuf_t)]
    ecs_strbuf_mergebuff.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 2226
if _libs["libflecs.dylib"].has("ecs_strbuf_appendstrn", "cdecl"):
    ecs_strbuf_appendstrn = _libs["libflecs.dylib"].get(
        "ecs_strbuf_appendstrn", "cdecl"
    )
    ecs_strbuf_appendstrn.argtypes = [POINTER(ecs_strbuf_t), String, c_int32]
    ecs_strbuf_appendstrn.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 2233
if _libs["libflecs.dylib"].has("ecs_strbuf_get", "cdecl"):
    ecs_strbuf_get = _libs["libflecs.dylib"].get("ecs_strbuf_get", "cdecl")
    ecs_strbuf_get.argtypes = [POINTER(ecs_strbuf_t)]
    if sizeof(c_int) == sizeof(c_void_p):
        ecs_strbuf_get.restype = ReturnString
    else:
        ecs_strbuf_get.restype = String
        ecs_strbuf_get.errcheck = ReturnString

# /Users/cnifi/git/pyflecs/flecs.h: 2238
if _libs["libflecs.dylib"].has("ecs_strbuf_get_small", "cdecl"):
    ecs_strbuf_get_small = _libs["libflecs.dylib"].get("ecs_strbuf_get_small", "cdecl")
    ecs_strbuf_get_small.argtypes = [POINTER(ecs_strbuf_t)]
    if sizeof(c_int) == sizeof(c_void_p):
        ecs_strbuf_get_small.restype = ReturnString
    else:
        ecs_strbuf_get_small.restype = String
        ecs_strbuf_get_small.errcheck = ReturnString

# /Users/cnifi/git/pyflecs/flecs.h: 2243
if _libs["libflecs.dylib"].has("ecs_strbuf_reset", "cdecl"):
    ecs_strbuf_reset = _libs["libflecs.dylib"].get("ecs_strbuf_reset", "cdecl")
    ecs_strbuf_reset.argtypes = [POINTER(ecs_strbuf_t)]
    ecs_strbuf_reset.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 2248
if _libs["libflecs.dylib"].has("ecs_strbuf_list_push", "cdecl"):
    ecs_strbuf_list_push = _libs["libflecs.dylib"].get("ecs_strbuf_list_push", "cdecl")
    ecs_strbuf_list_push.argtypes = [POINTER(ecs_strbuf_t), String, String]
    ecs_strbuf_list_push.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 2255
if _libs["libflecs.dylib"].has("ecs_strbuf_list_pop", "cdecl"):
    ecs_strbuf_list_pop = _libs["libflecs.dylib"].get("ecs_strbuf_list_pop", "cdecl")
    ecs_strbuf_list_pop.argtypes = [POINTER(ecs_strbuf_t), String]
    ecs_strbuf_list_pop.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 2261
if _libs["libflecs.dylib"].has("ecs_strbuf_list_next", "cdecl"):
    ecs_strbuf_list_next = _libs["libflecs.dylib"].get("ecs_strbuf_list_next", "cdecl")
    ecs_strbuf_list_next.argtypes = [POINTER(ecs_strbuf_t)]
    ecs_strbuf_list_next.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 2266
if _libs["libflecs.dylib"].has("ecs_strbuf_list_appendch", "cdecl"):
    ecs_strbuf_list_appendch = _libs["libflecs.dylib"].get(
        "ecs_strbuf_list_appendch", "cdecl"
    )
    ecs_strbuf_list_appendch.argtypes = [POINTER(ecs_strbuf_t), c_char]
    ecs_strbuf_list_appendch.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 2272
if _libs["libflecs.dylib"].has("ecs_strbuf_list_append", "cdecl"):
    _func = _libs["libflecs.dylib"].get("ecs_strbuf_list_append", "cdecl")
    _restype = None
    _errcheck = None
    _argtypes = [POINTER(ecs_strbuf_t), String]
    ecs_strbuf_list_append = _variadic_function(_func, _restype, _argtypes, _errcheck)

# /Users/cnifi/git/pyflecs/flecs.h: 2279
if _libs["libflecs.dylib"].has("ecs_strbuf_list_appendstr", "cdecl"):
    ecs_strbuf_list_appendstr = _libs["libflecs.dylib"].get(
        "ecs_strbuf_list_appendstr", "cdecl"
    )
    ecs_strbuf_list_appendstr.argtypes = [POINTER(ecs_strbuf_t), String]
    ecs_strbuf_list_appendstr.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 2285
if _libs["libflecs.dylib"].has("ecs_strbuf_list_appendstrn", "cdecl"):
    ecs_strbuf_list_appendstrn = _libs["libflecs.dylib"].get(
        "ecs_strbuf_list_appendstrn", "cdecl"
    )
    ecs_strbuf_list_appendstrn.argtypes = [POINTER(ecs_strbuf_t), String, c_int32]
    ecs_strbuf_list_appendstrn.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 2291
if _libs["libflecs.dylib"].has("ecs_strbuf_written", "cdecl"):
    ecs_strbuf_written = _libs["libflecs.dylib"].get("ecs_strbuf_written", "cdecl")
    ecs_strbuf_written.argtypes = [POINTER(ecs_strbuf_t)]
    ecs_strbuf_written.restype = c_int32


# /Users/cnifi/git/pyflecs/flecs.h: 2345
class struct_ecs_time_t(Structure):
    pass


struct_ecs_time_t.__slots__ = [
    "sec",
    "nanosec",
]
struct_ecs_time_t._fields_ = [
    ("sec", uint32_t),
    ("nanosec", uint32_t),
]

ecs_time_t = struct_ecs_time_t  # /Users/cnifi/git/pyflecs/flecs.h: 2345

# /Users/cnifi/git/pyflecs/flecs.h: 2348
try:
    ecs_os_api_malloc_count = (c_int64).in_dll(
        _libs["libflecs.dylib"], "ecs_os_api_malloc_count"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 2349
try:
    ecs_os_api_realloc_count = (c_int64).in_dll(
        _libs["libflecs.dylib"], "ecs_os_api_realloc_count"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 2350
try:
    ecs_os_api_calloc_count = (c_int64).in_dll(
        _libs["libflecs.dylib"], "ecs_os_api_calloc_count"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 2351
try:
    ecs_os_api_free_count = (c_int64).in_dll(
        _libs["libflecs.dylib"], "ecs_os_api_free_count"
    )
except:
    pass

ecs_os_thread_t = uintptr_t  # /Users/cnifi/git/pyflecs/flecs.h: 2354

ecs_os_cond_t = uintptr_t  # /Users/cnifi/git/pyflecs/flecs.h: 2355

ecs_os_mutex_t = uintptr_t  # /Users/cnifi/git/pyflecs/flecs.h: 2356

ecs_os_dl_t = uintptr_t  # /Users/cnifi/git/pyflecs/flecs.h: 2357

ecs_os_sock_t = uintptr_t  # /Users/cnifi/git/pyflecs/flecs.h: 2358

ecs_os_thread_id_t = uint64_t  # /Users/cnifi/git/pyflecs/flecs.h: 2361

ecs_os_proc_t = CFUNCTYPE(
    UNCHECKED(None),
)  # /Users/cnifi/git/pyflecs/flecs.h: 2364

ecs_os_api_init_t = CFUNCTYPE(
    UNCHECKED(None),
)  # /Users/cnifi/git/pyflecs/flecs.h: 2368

ecs_os_api_fini_t = CFUNCTYPE(
    UNCHECKED(None),
)  # /Users/cnifi/git/pyflecs/flecs.h: 2372

ecs_os_api_malloc_t = CFUNCTYPE(
    UNCHECKED(POINTER(c_ubyte)), ecs_size_t
)  # /Users/cnifi/git/pyflecs/flecs.h: 2376

ecs_os_api_free_t = CFUNCTYPE(
    UNCHECKED(None), POINTER(None)
)  # /Users/cnifi/git/pyflecs/flecs.h: 2381

ecs_os_api_realloc_t = CFUNCTYPE(
    UNCHECKED(POINTER(c_ubyte)), POINTER(None), ecs_size_t
)  # /Users/cnifi/git/pyflecs/flecs.h: 2386

ecs_os_api_calloc_t = CFUNCTYPE(
    UNCHECKED(POINTER(c_ubyte)), ecs_size_t
)  # /Users/cnifi/git/pyflecs/flecs.h: 2392

ecs_os_api_strdup_t = CFUNCTYPE(
    UNCHECKED(String), String
)  # /Users/cnifi/git/pyflecs/flecs.h: 2397

ecs_os_thread_callback_t = CFUNCTYPE(
    UNCHECKED(POINTER(c_ubyte)), POINTER(None)
)  # /Users/cnifi/git/pyflecs/flecs.h: 2402

ecs_os_api_thread_new_t = CFUNCTYPE(
    UNCHECKED(ecs_os_thread_t), ecs_os_thread_callback_t, POINTER(None)
)  # /Users/cnifi/git/pyflecs/flecs.h: 2407

ecs_os_api_thread_join_t = CFUNCTYPE(
    UNCHECKED(POINTER(c_ubyte)), ecs_os_thread_t
)  # /Users/cnifi/git/pyflecs/flecs.h: 2413

ecs_os_api_thread_self_t = CFUNCTYPE(
    UNCHECKED(ecs_os_thread_id_t),
)  # /Users/cnifi/git/pyflecs/flecs.h: 2418

ecs_os_api_task_new_t = CFUNCTYPE(
    UNCHECKED(ecs_os_thread_t), ecs_os_thread_callback_t, POINTER(None)
)  # /Users/cnifi/git/pyflecs/flecs.h: 2422

ecs_os_api_task_join_t = CFUNCTYPE(
    UNCHECKED(POINTER(c_ubyte)), ecs_os_thread_t
)  # /Users/cnifi/git/pyflecs/flecs.h: 2428

ecs_os_api_ainc_t = CFUNCTYPE(
    UNCHECKED(c_int32), POINTER(c_int32)
)  # /Users/cnifi/git/pyflecs/flecs.h: 2434

ecs_os_api_lainc_t = CFUNCTYPE(
    UNCHECKED(c_int64), POINTER(c_int64)
)  # /Users/cnifi/git/pyflecs/flecs.h: 2439

ecs_os_api_mutex_new_t = CFUNCTYPE(
    UNCHECKED(ecs_os_mutex_t),
)  # /Users/cnifi/git/pyflecs/flecs.h: 2445

ecs_os_api_mutex_lock_t = CFUNCTYPE(
    UNCHECKED(None), ecs_os_mutex_t
)  # /Users/cnifi/git/pyflecs/flecs.h: 2450

ecs_os_api_mutex_unlock_t = CFUNCTYPE(
    UNCHECKED(None), ecs_os_mutex_t
)  # /Users/cnifi/git/pyflecs/flecs.h: 2455

ecs_os_api_mutex_free_t = CFUNCTYPE(
    UNCHECKED(None), ecs_os_mutex_t
)  # /Users/cnifi/git/pyflecs/flecs.h: 2460

ecs_os_api_cond_new_t = CFUNCTYPE(
    UNCHECKED(ecs_os_cond_t),
)  # /Users/cnifi/git/pyflecs/flecs.h: 2466

ecs_os_api_cond_free_t = CFUNCTYPE(
    UNCHECKED(None), ecs_os_cond_t
)  # /Users/cnifi/git/pyflecs/flecs.h: 2471

ecs_os_api_cond_signal_t = CFUNCTYPE(
    UNCHECKED(None), ecs_os_cond_t
)  # /Users/cnifi/git/pyflecs/flecs.h: 2476

ecs_os_api_cond_broadcast_t = CFUNCTYPE(
    UNCHECKED(None), ecs_os_cond_t
)  # /Users/cnifi/git/pyflecs/flecs.h: 2481

ecs_os_api_cond_wait_t = CFUNCTYPE(
    UNCHECKED(None), ecs_os_cond_t, ecs_os_mutex_t
)  # /Users/cnifi/git/pyflecs/flecs.h: 2486

ecs_os_api_sleep_t = CFUNCTYPE(
    UNCHECKED(None), c_int32, c_int32
)  # /Users/cnifi/git/pyflecs/flecs.h: 2492

ecs_os_api_enable_high_timer_resolution_t = CFUNCTYPE(
    UNCHECKED(None), c_bool
)  # /Users/cnifi/git/pyflecs/flecs.h: 2498

ecs_os_api_get_time_t = CFUNCTYPE(
    UNCHECKED(None), POINTER(ecs_time_t)
)  # /Users/cnifi/git/pyflecs/flecs.h: 2503

ecs_os_api_now_t = CFUNCTYPE(
    UNCHECKED(uint64_t),
)  # /Users/cnifi/git/pyflecs/flecs.h: 2508

ecs_os_api_log_t = CFUNCTYPE(
    UNCHECKED(None), c_int32, String, c_int32, String
)  # /Users/cnifi/git/pyflecs/flecs.h: 2512

ecs_os_api_abort_t = CFUNCTYPE(
    UNCHECKED(None),
)  # /Users/cnifi/git/pyflecs/flecs.h: 2520

ecs_os_api_dlopen_t = CFUNCTYPE(
    UNCHECKED(ecs_os_dl_t), String
)  # /Users/cnifi/git/pyflecs/flecs.h: 2525

ecs_os_api_dlproc_t = CFUNCTYPE(
    UNCHECKED(ecs_os_proc_t), ecs_os_dl_t, String
)  # /Users/cnifi/git/pyflecs/flecs.h: 2530

ecs_os_api_dlclose_t = CFUNCTYPE(
    UNCHECKED(None), ecs_os_dl_t
)  # /Users/cnifi/git/pyflecs/flecs.h: 2536

ecs_os_api_module_to_path_t = CFUNCTYPE(
    UNCHECKED(String), String
)  # /Users/cnifi/git/pyflecs/flecs.h: 2541

ecs_os_api_perf_trace_t = CFUNCTYPE(
    UNCHECKED(None), String, c_size_t, String
)  # /Users/cnifi/git/pyflecs/flecs.h: 2545


# /Users/cnifi/git/pyflecs/flecs.h: 2641
class struct_ecs_os_api_t(Structure):
    pass


struct_ecs_os_api_t.__slots__ = [
    "init_",
    "fini_",
    "malloc_",
    "realloc_",
    "calloc_",
    "free_",
    "strdup_",
    "thread_new_",
    "thread_join_",
    "thread_self_",
    "task_new_",
    "task_join_",
    "ainc_",
    "adec_",
    "lainc_",
    "ladec_",
    "mutex_new_",
    "mutex_free_",
    "mutex_lock_",
    "mutex_unlock_",
    "cond_new_",
    "cond_free_",
    "cond_signal_",
    "cond_broadcast_",
    "cond_wait_",
    "sleep_",
    "now_",
    "get_time_",
    "log_",
    "abort_",
    "dlopen_",
    "dlproc_",
    "dlclose_",
    "module_to_dl_",
    "module_to_etc_",
    "perf_trace_push_",
    "perf_trace_pop_",
    "log_level_",
    "log_indent_",
    "log_last_error_",
    "log_last_timestamp_",
    "flags_",
    "log_out_",
]
struct_ecs_os_api_t._fields_ = [
    ("init_", ecs_os_api_init_t),
    ("fini_", ecs_os_api_fini_t),
    ("malloc_", ecs_os_api_malloc_t),
    ("realloc_", ecs_os_api_realloc_t),
    ("calloc_", ecs_os_api_calloc_t),
    ("free_", ecs_os_api_free_t),
    ("strdup_", ecs_os_api_strdup_t),
    ("thread_new_", ecs_os_api_thread_new_t),
    ("thread_join_", ecs_os_api_thread_join_t),
    ("thread_self_", ecs_os_api_thread_self_t),
    ("task_new_", ecs_os_api_thread_new_t),
    ("task_join_", ecs_os_api_thread_join_t),
    ("ainc_", ecs_os_api_ainc_t),
    ("adec_", ecs_os_api_ainc_t),
    ("lainc_", ecs_os_api_lainc_t),
    ("ladec_", ecs_os_api_lainc_t),
    ("mutex_new_", ecs_os_api_mutex_new_t),
    ("mutex_free_", ecs_os_api_mutex_free_t),
    ("mutex_lock_", ecs_os_api_mutex_lock_t),
    ("mutex_unlock_", ecs_os_api_mutex_lock_t),
    ("cond_new_", ecs_os_api_cond_new_t),
    ("cond_free_", ecs_os_api_cond_free_t),
    ("cond_signal_", ecs_os_api_cond_signal_t),
    ("cond_broadcast_", ecs_os_api_cond_broadcast_t),
    ("cond_wait_", ecs_os_api_cond_wait_t),
    ("sleep_", ecs_os_api_sleep_t),
    ("now_", ecs_os_api_now_t),
    ("get_time_", ecs_os_api_get_time_t),
    ("log_", ecs_os_api_log_t),
    ("abort_", ecs_os_api_abort_t),
    ("dlopen_", ecs_os_api_dlopen_t),
    ("dlproc_", ecs_os_api_dlproc_t),
    ("dlclose_", ecs_os_api_dlclose_t),
    ("module_to_dl_", ecs_os_api_module_to_path_t),
    ("module_to_etc_", ecs_os_api_module_to_path_t),
    ("perf_trace_push_", ecs_os_api_perf_trace_t),
    ("perf_trace_pop_", ecs_os_api_perf_trace_t),
    ("log_level_", c_int32),
    ("log_indent_", c_int32),
    ("log_last_error_", c_int32),
    ("log_last_timestamp_", c_int64),
    ("flags_", ecs_flags32_t),
    ("log_out_", POINTER(None)),
]

ecs_os_api_t = struct_ecs_os_api_t  # /Users/cnifi/git/pyflecs/flecs.h: 2641

# /Users/cnifi/git/pyflecs/flecs.h: 2645
try:
    ecs_os_api = (ecs_os_api_t).in_dll(_libs["libflecs.dylib"], "ecs_os_api")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 2659
if _libs["libflecs.dylib"].has("ecs_os_init", "cdecl"):
    ecs_os_init = _libs["libflecs.dylib"].get("ecs_os_init", "cdecl")
    ecs_os_init.argtypes = []
    ecs_os_init.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 2665
if _libs["libflecs.dylib"].has("ecs_os_fini", "cdecl"):
    ecs_os_fini = _libs["libflecs.dylib"].get("ecs_os_fini", "cdecl")
    ecs_os_fini.argtypes = []
    ecs_os_fini.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 2674
if _libs["libflecs.dylib"].has("ecs_os_set_api", "cdecl"):
    ecs_os_set_api = _libs["libflecs.dylib"].get("ecs_os_set_api", "cdecl")
    ecs_os_set_api.argtypes = [POINTER(ecs_os_api_t)]
    ecs_os_set_api.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 2683
if _libs["libflecs.dylib"].has("ecs_os_get_api", "cdecl"):
    ecs_os_get_api = _libs["libflecs.dylib"].get("ecs_os_get_api", "cdecl")
    ecs_os_get_api.argtypes = []
    ecs_os_get_api.restype = ecs_os_api_t

# /Users/cnifi/git/pyflecs/flecs.h: 2692
if _libs["libflecs.dylib"].has("ecs_os_set_api_defaults", "cdecl"):
    ecs_os_set_api_defaults = _libs["libflecs.dylib"].get(
        "ecs_os_set_api_defaults", "cdecl"
    )
    ecs_os_set_api_defaults.argtypes = []
    ecs_os_set_api_defaults.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 2877
if _libs["libflecs.dylib"].has("ecs_os_dbg", "cdecl"):
    ecs_os_dbg = _libs["libflecs.dylib"].get("ecs_os_dbg", "cdecl")
    ecs_os_dbg.argtypes = [String, c_int32, String]
    ecs_os_dbg.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 2889
if _libs["libflecs.dylib"].has("ecs_os_trace", "cdecl"):
    ecs_os_trace = _libs["libflecs.dylib"].get("ecs_os_trace", "cdecl")
    ecs_os_trace.argtypes = [String, c_int32, String]
    ecs_os_trace.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 2901
if _libs["libflecs.dylib"].has("ecs_os_warn", "cdecl"):
    ecs_os_warn = _libs["libflecs.dylib"].get("ecs_os_warn", "cdecl")
    ecs_os_warn.argtypes = [String, c_int32, String]
    ecs_os_warn.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 2913
if _libs["libflecs.dylib"].has("ecs_os_err", "cdecl"):
    ecs_os_err = _libs["libflecs.dylib"].get("ecs_os_err", "cdecl")
    ecs_os_err.argtypes = [String, c_int32, String]
    ecs_os_err.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 2925
if _libs["libflecs.dylib"].has("ecs_os_fatal", "cdecl"):
    ecs_os_fatal = _libs["libflecs.dylib"].get("ecs_os_fatal", "cdecl")
    ecs_os_fatal.argtypes = [String, c_int32, String]
    ecs_os_fatal.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 2936
if _libs["libflecs.dylib"].has("ecs_os_strerror", "cdecl"):
    ecs_os_strerror = _libs["libflecs.dylib"].get("ecs_os_strerror", "cdecl")
    ecs_os_strerror.argtypes = [c_int]
    ecs_os_strerror.restype = c_char_p

# /Users/cnifi/git/pyflecs/flecs.h: 2946
if _libs["libflecs.dylib"].has("ecs_os_strset", "cdecl"):
    ecs_os_strset = _libs["libflecs.dylib"].get("ecs_os_strset", "cdecl")
    ecs_os_strset.argtypes = [POINTER(POINTER(c_char)), String]
    ecs_os_strset.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 2959
if _libs["libflecs.dylib"].has("ecs_os_perf_trace_push_", "cdecl"):
    ecs_os_perf_trace_push_ = _libs["libflecs.dylib"].get(
        "ecs_os_perf_trace_push_", "cdecl"
    )
    ecs_os_perf_trace_push_.argtypes = [String, c_size_t, String]
    ecs_os_perf_trace_push_.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 2964
if _libs["libflecs.dylib"].has("ecs_os_perf_trace_pop_", "cdecl"):
    ecs_os_perf_trace_pop_ = _libs["libflecs.dylib"].get(
        "ecs_os_perf_trace_pop_", "cdecl"
    )
    ecs_os_perf_trace_pop_.argtypes = [String, c_size_t, String]
    ecs_os_perf_trace_pop_.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 2974
if _libs["libflecs.dylib"].has("ecs_sleepf", "cdecl"):
    ecs_sleepf = _libs["libflecs.dylib"].get("ecs_sleepf", "cdecl")
    ecs_sleepf.argtypes = [c_double]
    ecs_sleepf.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 2993
if _libs["libflecs.dylib"].has("ecs_time_measure", "cdecl"):
    ecs_time_measure = _libs["libflecs.dylib"].get("ecs_time_measure", "cdecl")
    ecs_time_measure.argtypes = [POINTER(ecs_time_t)]
    ecs_time_measure.restype = c_double

# /Users/cnifi/git/pyflecs/flecs.h: 3003
if _libs["libflecs.dylib"].has("ecs_time_sub", "cdecl"):
    ecs_time_sub = _libs["libflecs.dylib"].get("ecs_time_sub", "cdecl")
    ecs_time_sub.argtypes = [ecs_time_t, ecs_time_t]
    ecs_time_sub.restype = ecs_time_t

# /Users/cnifi/git/pyflecs/flecs.h: 3013
if _libs["libflecs.dylib"].has("ecs_time_to_double", "cdecl"):
    ecs_time_to_double = _libs["libflecs.dylib"].get("ecs_time_to_double", "cdecl")
    ecs_time_to_double.argtypes = [ecs_time_t]
    ecs_time_to_double.restype = c_double

# /Users/cnifi/git/pyflecs/flecs.h: 3023
if _libs["libflecs.dylib"].has("ecs_os_memdup", "cdecl"):
    ecs_os_memdup = _libs["libflecs.dylib"].get("ecs_os_memdup", "cdecl")
    ecs_os_memdup.argtypes = [POINTER(None), ecs_size_t]
    ecs_os_memdup.restype = POINTER(c_ubyte)
    ecs_os_memdup.errcheck = lambda v, *a: cast(v, c_void_p)

# /Users/cnifi/git/pyflecs/flecs.h: 3029
if _libs["libflecs.dylib"].has("ecs_os_has_heap", "cdecl"):
    ecs_os_has_heap = _libs["libflecs.dylib"].get("ecs_os_has_heap", "cdecl")
    ecs_os_has_heap.argtypes = []
    ecs_os_has_heap.restype = c_bool

# /Users/cnifi/git/pyflecs/flecs.h: 3033
if _libs["libflecs.dylib"].has("ecs_os_has_threading", "cdecl"):
    ecs_os_has_threading = _libs["libflecs.dylib"].get("ecs_os_has_threading", "cdecl")
    ecs_os_has_threading.argtypes = []
    ecs_os_has_threading.restype = c_bool

# /Users/cnifi/git/pyflecs/flecs.h: 3037
if _libs["libflecs.dylib"].has("ecs_os_has_task_support", "cdecl"):
    ecs_os_has_task_support = _libs["libflecs.dylib"].get(
        "ecs_os_has_task_support", "cdecl"
    )
    ecs_os_has_task_support.argtypes = []
    ecs_os_has_task_support.restype = c_bool

# /Users/cnifi/git/pyflecs/flecs.h: 3041
if _libs["libflecs.dylib"].has("ecs_os_has_time", "cdecl"):
    ecs_os_has_time = _libs["libflecs.dylib"].get("ecs_os_has_time", "cdecl")
    ecs_os_has_time.argtypes = []
    ecs_os_has_time.restype = c_bool

# /Users/cnifi/git/pyflecs/flecs.h: 3045
if _libs["libflecs.dylib"].has("ecs_os_has_logging", "cdecl"):
    ecs_os_has_logging = _libs["libflecs.dylib"].get("ecs_os_has_logging", "cdecl")
    ecs_os_has_logging.argtypes = []
    ecs_os_has_logging.restype = c_bool

# /Users/cnifi/git/pyflecs/flecs.h: 3049
if _libs["libflecs.dylib"].has("ecs_os_has_dl", "cdecl"):
    ecs_os_has_dl = _libs["libflecs.dylib"].get("ecs_os_has_dl", "cdecl")
    ecs_os_has_dl.argtypes = []
    ecs_os_has_dl.restype = c_bool

# /Users/cnifi/git/pyflecs/flecs.h: 3053
if _libs["libflecs.dylib"].has("ecs_os_has_modules", "cdecl"):
    ecs_os_has_modules = _libs["libflecs.dylib"].get("ecs_os_has_modules", "cdecl")
    ecs_os_has_modules.argtypes = []
    ecs_os_has_modules.restype = c_bool

ecs_id_t = uint64_t  # /Users/cnifi/git/pyflecs/flecs.h: 3084

ecs_entity_t = ecs_id_t  # /Users/cnifi/git/pyflecs/flecs.h: 3091


# /Users/cnifi/git/pyflecs/flecs.h: 3111
class struct_anon_8(Structure):
    pass


struct_anon_8.__slots__ = [
    "array",
    "count",
]
struct_anon_8._fields_ = [
    ("array", POINTER(ecs_id_t)),
    ("count", c_int32),
]

ecs_type_t = struct_anon_8  # /Users/cnifi/git/pyflecs/flecs.h: 3111


# /Users/cnifi/git/pyflecs/flecs.h: 3135
class struct_ecs_world_t(Structure):
    pass


ecs_world_t = struct_ecs_world_t  # /Users/cnifi/git/pyflecs/flecs.h: 3135


# /Users/cnifi/git/pyflecs/flecs.h: 3138
class struct_ecs_stage_t(Structure):
    pass


ecs_stage_t = struct_ecs_stage_t  # /Users/cnifi/git/pyflecs/flecs.h: 3138


# /Users/cnifi/git/pyflecs/flecs.h: 3141
class struct_ecs_table_t(Structure):
    pass


ecs_table_t = struct_ecs_table_t  # /Users/cnifi/git/pyflecs/flecs.h: 3141


# /Users/cnifi/git/pyflecs/flecs.h: 3498
class struct_ecs_term_t(Structure):
    pass


ecs_term_t = struct_ecs_term_t  # /Users/cnifi/git/pyflecs/flecs.h: 3144


# /Users/cnifi/git/pyflecs/flecs.h: 3522
class struct_ecs_query_t(Structure):
    pass


ecs_query_t = struct_ecs_query_t  # /Users/cnifi/git/pyflecs/flecs.h: 3147


# /Users/cnifi/git/pyflecs/flecs.h: 3563
class struct_ecs_observer_t(Structure):
    pass


ecs_observer_t = struct_ecs_observer_t  # /Users/cnifi/git/pyflecs/flecs.h: 3166


# /Users/cnifi/git/pyflecs/flecs.h: 3756
class struct_ecs_observable_t(Structure):
    pass


ecs_observable_t = struct_ecs_observable_t  # /Users/cnifi/git/pyflecs/flecs.h: 3171


# /Users/cnifi/git/pyflecs/flecs.h: 4870
class struct_ecs_iter_t(Structure):
    pass


ecs_iter_t = struct_ecs_iter_t  # /Users/cnifi/git/pyflecs/flecs.h: 3177


# /Users/cnifi/git/pyflecs/flecs.h: 3784
class struct_ecs_ref_t(Structure):
    pass


ecs_ref_t = struct_ecs_ref_t  # /Users/cnifi/git/pyflecs/flecs.h: 3187


# /Users/cnifi/git/pyflecs/flecs.h: 3635
class struct_ecs_type_hooks_t(Structure):
    pass


ecs_type_hooks_t = struct_ecs_type_hooks_t  # /Users/cnifi/git/pyflecs/flecs.h: 3192


# /Users/cnifi/git/pyflecs/flecs.h: 3705
class struct_ecs_type_info_t(Structure):
    pass


ecs_type_info_t = struct_ecs_type_info_t  # /Users/cnifi/git/pyflecs/flecs.h: 3197


# /Users/cnifi/git/pyflecs/flecs.h: 4411
class struct_ecs_record_t(Structure):
    pass


ecs_record_t = struct_ecs_record_t  # /Users/cnifi/git/pyflecs/flecs.h: 3200


# /Users/cnifi/git/pyflecs/flecs.h: 3203
class struct_ecs_component_record_t(Structure):
    pass


ecs_component_record_t = (
    struct_ecs_component_record_t  # /Users/cnifi/git/pyflecs/flecs.h: 3203
)

ecs_poly_t = None  # /Users/cnifi/git/pyflecs/flecs.h: 3224


# /Users/cnifi/git/pyflecs/flecs.h: 3227
class struct_ecs_mixins_t(Structure):
    pass


ecs_mixins_t = struct_ecs_mixins_t  # /Users/cnifi/git/pyflecs/flecs.h: 3227


# /Users/cnifi/git/pyflecs/flecs.h: 3234
class struct_ecs_header_t(Structure):
    pass


struct_ecs_header_t.__slots__ = [
    "type",
    "refcount",
    "mixins",
]
struct_ecs_header_t._fields_ = [
    ("type", c_int32),
    ("refcount", c_int32),
    ("mixins", POINTER(ecs_mixins_t)),
]

ecs_header_t = struct_ecs_header_t  # /Users/cnifi/git/pyflecs/flecs.h: 3234


# /Users/cnifi/git/pyflecs/flecs.h: 4428
class struct_ecs_table_record_t(Structure):
    pass


ecs_table_record_t = struct_ecs_table_record_t  # /Users/cnifi/git/pyflecs/flecs.h: 3236

ecs_run_action_t = CFUNCTYPE(
    UNCHECKED(None), POINTER(ecs_iter_t)
)  # /Users/cnifi/git/pyflecs/flecs.h: 3256

ecs_iter_action_t = CFUNCTYPE(
    UNCHECKED(None), POINTER(ecs_iter_t)
)  # /Users/cnifi/git/pyflecs/flecs.h: 3265

ecs_iter_next_action_t = CFUNCTYPE(
    UNCHECKED(c_bool), POINTER(ecs_iter_t)
)  # /Users/cnifi/git/pyflecs/flecs.h: 3275

ecs_iter_fini_action_t = CFUNCTYPE(
    UNCHECKED(None), POINTER(ecs_iter_t)
)  # /Users/cnifi/git/pyflecs/flecs.h: 3283

ecs_order_by_action_t = CFUNCTYPE(
    UNCHECKED(c_int), ecs_entity_t, POINTER(None), ecs_entity_t, POINTER(None)
)  # /Users/cnifi/git/pyflecs/flecs.h: 3287

ecs_sort_table_action_t = CFUNCTYPE(
    UNCHECKED(None),
    POINTER(ecs_world_t),
    POINTER(ecs_table_t),
    POINTER(ecs_entity_t),
    POINTER(None),
    c_int32,
    c_int32,
    c_int32,
    ecs_order_by_action_t,
)  # /Users/cnifi/git/pyflecs/flecs.h: 3294

ecs_group_by_action_t = CFUNCTYPE(
    UNCHECKED(uint64_t),
    POINTER(ecs_world_t),
    POINTER(ecs_table_t),
    ecs_id_t,
    POINTER(None),
)  # /Users/cnifi/git/pyflecs/flecs.h: 3305

ecs_group_create_action_t = CFUNCTYPE(
    UNCHECKED(POINTER(c_ubyte)), POINTER(ecs_world_t), uint64_t, POINTER(None)
)  # /Users/cnifi/git/pyflecs/flecs.h: 3312

ecs_group_delete_action_t = CFUNCTYPE(
    UNCHECKED(None), POINTER(ecs_world_t), uint64_t, POINTER(None), POINTER(None)
)  # /Users/cnifi/git/pyflecs/flecs.h: 3318

ecs_module_action_t = CFUNCTYPE(
    UNCHECKED(None), POINTER(ecs_world_t)
)  # /Users/cnifi/git/pyflecs/flecs.h: 3325

ecs_fini_action_t = CFUNCTYPE(
    UNCHECKED(None), POINTER(ecs_world_t), POINTER(None)
)  # /Users/cnifi/git/pyflecs/flecs.h: 3329

ecs_ctx_free_t = CFUNCTYPE(
    UNCHECKED(None), POINTER(None)
)  # /Users/cnifi/git/pyflecs/flecs.h: 3334

ecs_compare_action_t = CFUNCTYPE(
    UNCHECKED(c_int), POINTER(None), POINTER(None)
)  # /Users/cnifi/git/pyflecs/flecs.h: 3338

ecs_hash_value_action_t = CFUNCTYPE(
    UNCHECKED(uint64_t), POINTER(None)
)  # /Users/cnifi/git/pyflecs/flecs.h: 3343

ecs_xtor_t = CFUNCTYPE(
    UNCHECKED(None), POINTER(None), c_int32, POINTER(ecs_type_info_t)
)  # /Users/cnifi/git/pyflecs/flecs.h: 3347

ecs_copy_t = CFUNCTYPE(
    UNCHECKED(None), POINTER(None), POINTER(None), c_int32, POINTER(ecs_type_info_t)
)  # /Users/cnifi/git/pyflecs/flecs.h: 3353

ecs_move_t = CFUNCTYPE(
    UNCHECKED(None), POINTER(None), POINTER(None), c_int32, POINTER(ecs_type_info_t)
)  # /Users/cnifi/git/pyflecs/flecs.h: 3360

ecs_cmp_t = CFUNCTYPE(
    UNCHECKED(c_int), POINTER(None), POINTER(None), POINTER(ecs_type_info_t)
)  # /Users/cnifi/git/pyflecs/flecs.h: 3367

ecs_equals_t = CFUNCTYPE(
    UNCHECKED(c_bool), POINTER(None), POINTER(None), POINTER(ecs_type_info_t)
)  # /Users/cnifi/git/pyflecs/flecs.h: 3373

flecs_poly_dtor_t = CFUNCTYPE(
    UNCHECKED(None), POINTER(ecs_poly_t)
)  # /Users/cnifi/git/pyflecs/flecs.h: 3379

enum_ecs_inout_kind_t = c_int  # /Users/cnifi/git/pyflecs/flecs.h: 3399

EcsInOutDefault = 0  # /Users/cnifi/git/pyflecs/flecs.h: 3399

EcsInOutNone = EcsInOutDefault + 1  # /Users/cnifi/git/pyflecs/flecs.h: 3399

EcsInOutFilter = EcsInOutNone + 1  # /Users/cnifi/git/pyflecs/flecs.h: 3399

EcsInOut = EcsInOutFilter + 1  # /Users/cnifi/git/pyflecs/flecs.h: 3399

EcsIn = EcsInOut + 1  # /Users/cnifi/git/pyflecs/flecs.h: 3399

EcsOut = EcsIn + 1  # /Users/cnifi/git/pyflecs/flecs.h: 3399

ecs_inout_kind_t = enum_ecs_inout_kind_t  # /Users/cnifi/git/pyflecs/flecs.h: 3399

enum_ecs_oper_kind_t = c_int  # /Users/cnifi/git/pyflecs/flecs.h: 3410

EcsAnd = 0  # /Users/cnifi/git/pyflecs/flecs.h: 3410

EcsOr = EcsAnd + 1  # /Users/cnifi/git/pyflecs/flecs.h: 3410

EcsNot = EcsOr + 1  # /Users/cnifi/git/pyflecs/flecs.h: 3410

EcsOptional = EcsNot + 1  # /Users/cnifi/git/pyflecs/flecs.h: 3410

EcsAndFrom = EcsOptional + 1  # /Users/cnifi/git/pyflecs/flecs.h: 3410

EcsOrFrom = EcsAndFrom + 1  # /Users/cnifi/git/pyflecs/flecs.h: 3410

EcsNotFrom = EcsOrFrom + 1  # /Users/cnifi/git/pyflecs/flecs.h: 3410

ecs_oper_kind_t = enum_ecs_oper_kind_t  # /Users/cnifi/git/pyflecs/flecs.h: 3410

enum_ecs_query_cache_kind_t = c_int  # /Users/cnifi/git/pyflecs/flecs.h: 3418

EcsQueryCacheDefault = 0  # /Users/cnifi/git/pyflecs/flecs.h: 3418

EcsQueryCacheAuto = EcsQueryCacheDefault + 1  # /Users/cnifi/git/pyflecs/flecs.h: 3418

EcsQueryCacheAll = EcsQueryCacheAuto + 1  # /Users/cnifi/git/pyflecs/flecs.h: 3418

EcsQueryCacheNone = EcsQueryCacheAll + 1  # /Users/cnifi/git/pyflecs/flecs.h: 3418

ecs_query_cache_kind_t = (
    enum_ecs_query_cache_kind_t  # /Users/cnifi/git/pyflecs/flecs.h: 3418
)


# /Users/cnifi/git/pyflecs/flecs.h: 3495
class struct_ecs_term_ref_t(Structure):
    pass


struct_ecs_term_ref_t.__slots__ = [
    "id",
    "name",
]
struct_ecs_term_ref_t._fields_ = [
    ("id", ecs_entity_t),
    ("name", String),
]

ecs_term_ref_t = struct_ecs_term_ref_t  # /Users/cnifi/git/pyflecs/flecs.h: 3495

struct_ecs_term_t.__slots__ = [
    "id",
    "src",
    "first",
    "second",
    "trav",
    "inout",
    "oper",
    "field_index",
    "flags_",
]
struct_ecs_term_t._fields_ = [
    ("id", ecs_id_t),
    ("src", ecs_term_ref_t),
    ("first", ecs_term_ref_t),
    ("second", ecs_term_ref_t),
    ("trav", ecs_entity_t),
    ("inout", c_int16),
    ("oper", c_int16),
    ("field_index", c_int8),
    ("flags_", ecs_flags16_t),
]

struct_ecs_query_t.__slots__ = [
    "hdr",
    "terms",
    "sizes",
    "ids",
    "bloom_filter",
    "flags",
    "var_count",
    "term_count",
    "field_count",
    "fixed_fields",
    "var_fields",
    "static_id_fields",
    "data_fields",
    "write_fields",
    "read_fields",
    "row_fields",
    "shared_readonly_fields",
    "set_fields",
    "cache_kind",
    "vars",
    "ctx",
    "binding_ctx",
    "entity",
    "real_world",
    "world",
    "eval_count",
]
struct_ecs_query_t._fields_ = [
    ("hdr", ecs_header_t),
    ("terms", POINTER(ecs_term_t)),
    ("sizes", POINTER(c_int32)),
    ("ids", POINTER(ecs_id_t)),
    ("bloom_filter", uint64_t),
    ("flags", ecs_flags32_t),
    ("var_count", c_int8),
    ("term_count", c_int8),
    ("field_count", c_int8),
    ("fixed_fields", ecs_flags32_t),
    ("var_fields", ecs_flags32_t),
    ("static_id_fields", ecs_flags32_t),
    ("data_fields", ecs_flags32_t),
    ("write_fields", ecs_flags32_t),
    ("read_fields", ecs_flags32_t),
    ("row_fields", ecs_flags32_t),
    ("shared_readonly_fields", ecs_flags32_t),
    ("set_fields", ecs_flags32_t),
    ("cache_kind", ecs_query_cache_kind_t),
    ("vars", POINTER(POINTER(c_char))),
    ("ctx", POINTER(None)),
    ("binding_ctx", POINTER(None)),
    ("entity", ecs_entity_t),
    ("real_world", POINTER(ecs_world_t)),
    ("world", POINTER(ecs_world_t)),
    ("eval_count", c_int32),
]

struct_ecs_observer_t.__slots__ = [
    "hdr",
    "query",
    "events",
    "event_count",
    "callback",
    "run",
    "ctx",
    "callback_ctx",
    "run_ctx",
    "ctx_free",
    "callback_ctx_free",
    "run_ctx_free",
    "observable",
    "world",
    "entity",
]
struct_ecs_observer_t._fields_ = [
    ("hdr", ecs_header_t),
    ("query", POINTER(ecs_query_t)),
    ("events", ecs_entity_t * int(8)),
    ("event_count", c_int32),
    ("callback", ecs_iter_action_t),
    ("run", ecs_run_action_t),
    ("ctx", POINTER(None)),
    ("callback_ctx", POINTER(None)),
    ("run_ctx", POINTER(None)),
    ("ctx_free", ecs_ctx_free_t),
    ("callback_ctx_free", ecs_ctx_free_t),
    ("run_ctx_free", ecs_ctx_free_t),
    ("observable", POINTER(ecs_observable_t)),
    ("world", POINTER(ecs_world_t)),
    ("entity", ecs_entity_t),
]

struct_ecs_type_hooks_t.__slots__ = [
    "ctor",
    "dtor",
    "copy",
    "move",
    "copy_ctor",
    "move_ctor",
    "ctor_move_dtor",
    "move_dtor",
    "cmp",
    "equals",
    "flags",
    "on_add",
    "on_set",
    "on_remove",
    "on_replace",
    "ctx",
    "binding_ctx",
    "lifecycle_ctx",
    "ctx_free",
    "binding_ctx_free",
    "lifecycle_ctx_free",
]
struct_ecs_type_hooks_t._fields_ = [
    ("ctor", ecs_xtor_t),
    ("dtor", ecs_xtor_t),
    ("copy", ecs_copy_t),
    ("move", ecs_move_t),
    ("copy_ctor", ecs_copy_t),
    ("move_ctor", ecs_move_t),
    ("ctor_move_dtor", ecs_move_t),
    ("move_dtor", ecs_move_t),
    ("cmp", ecs_cmp_t),
    ("equals", ecs_equals_t),
    ("flags", ecs_flags32_t),
    ("on_add", ecs_iter_action_t),
    ("on_set", ecs_iter_action_t),
    ("on_remove", ecs_iter_action_t),
    ("on_replace", ecs_iter_action_t),
    ("ctx", POINTER(None)),
    ("binding_ctx", POINTER(None)),
    ("lifecycle_ctx", POINTER(None)),
    ("ctx_free", ecs_ctx_free_t),
    ("binding_ctx_free", ecs_ctx_free_t),
    ("lifecycle_ctx_free", ecs_ctx_free_t),
]

struct_ecs_type_info_t.__slots__ = [
    "size",
    "alignment",
    "hooks",
    "component",
    "name",
]
struct_ecs_type_info_t._fields_ = [
    ("size", ecs_size_t),
    ("alignment", ecs_size_t),
    ("hooks", ecs_type_hooks_t),
    ("component", ecs_entity_t),
    ("name", String),
]


# /Users/cnifi/git/pyflecs/flecs.h: 3735
class struct_ecs_data_t(Structure):
    pass


ecs_data_t = struct_ecs_data_t  # /Users/cnifi/git/pyflecs/flecs.h: 3735


# /Users/cnifi/git/pyflecs/flecs.h: 3738
class struct_ecs_query_cache_match_t(Structure):
    pass


ecs_query_cache_match_t = (
    struct_ecs_query_cache_match_t  # /Users/cnifi/git/pyflecs/flecs.h: 3738
)


# /Users/cnifi/git/pyflecs/flecs.h: 3741
class struct_ecs_query_cache_group_t(Structure):
    pass


ecs_query_cache_group_t = (
    struct_ecs_query_cache_group_t  # /Users/cnifi/git/pyflecs/flecs.h: 3741
)


# /Users/cnifi/git/pyflecs/flecs.h: 3749
class struct_ecs_event_id_record_t(Structure):
    pass


# /Users/cnifi/git/pyflecs/flecs.h: 3754
class struct_ecs_event_record_t(Structure):
    pass


struct_ecs_event_record_t.__slots__ = [
    "any",
    "wildcard",
    "wildcard_pair",
    "event_ids",
    "event",
]
struct_ecs_event_record_t._fields_ = [
    ("any", POINTER(struct_ecs_event_id_record_t)),
    ("wildcard", POINTER(struct_ecs_event_id_record_t)),
    ("wildcard_pair", POINTER(struct_ecs_event_id_record_t)),
    ("event_ids", ecs_map_t),
    ("event", ecs_entity_t),
]

ecs_event_record_t = struct_ecs_event_record_t  # /Users/cnifi/git/pyflecs/flecs.h: 3754

struct_ecs_observable_t.__slots__ = [
    "on_add",
    "on_remove",
    "on_set",
    "on_wildcard",
    "events",
    "last_observer_id",
]
struct_ecs_observable_t._fields_ = [
    ("on_add", ecs_event_record_t),
    ("on_remove", ecs_event_record_t),
    ("on_set", ecs_event_record_t),
    ("on_wildcard", ecs_event_record_t),
    ("events", ecs_sparse_t),
    ("last_observer_id", uint64_t),
]


# /Users/cnifi/git/pyflecs/flecs.h: 3770
class struct_ecs_table_range_t(Structure):
    pass


struct_ecs_table_range_t.__slots__ = [
    "table",
    "offset",
    "count",
]
struct_ecs_table_range_t._fields_ = [
    ("table", POINTER(ecs_table_t)),
    ("offset", c_int32),
    ("count", c_int32),
]

ecs_table_range_t = struct_ecs_table_range_t  # /Users/cnifi/git/pyflecs/flecs.h: 3770


# /Users/cnifi/git/pyflecs/flecs.h: 3781
class struct_ecs_var_t(Structure):
    pass


struct_ecs_var_t.__slots__ = [
    "range",
    "entity",
]
struct_ecs_var_t._fields_ = [
    ("range", ecs_table_range_t),
    ("entity", ecs_entity_t),
]

ecs_var_t = struct_ecs_var_t  # /Users/cnifi/git/pyflecs/flecs.h: 3781

struct_ecs_ref_t.__slots__ = [
    "entity",
    "id",
    "table_id",
    "table_version_fast",
    "table_version",
    "record",
    "ptr",
]
struct_ecs_ref_t._fields_ = [
    ("entity", ecs_entity_t),
    ("id", ecs_entity_t),
    ("table_id", uint64_t),
    ("table_version_fast", uint32_t),
    ("table_version", uint16_t),
    ("record", POINTER(ecs_record_t)),
    ("ptr", POINTER(None)),
]


# /Users/cnifi/git/pyflecs/flecs.h: 3799
class struct_ecs_page_iter_t(Structure):
    pass


struct_ecs_page_iter_t.__slots__ = [
    "offset",
    "limit",
    "remaining",
]
struct_ecs_page_iter_t._fields_ = [
    ("offset", c_int32),
    ("limit", c_int32),
    ("remaining", c_int32),
]

ecs_page_iter_t = struct_ecs_page_iter_t  # /Users/cnifi/git/pyflecs/flecs.h: 3799


# /Users/cnifi/git/pyflecs/flecs.h: 3805
class struct_ecs_worker_iter_t(Structure):
    pass


struct_ecs_worker_iter_t.__slots__ = [
    "index",
    "count",
]
struct_ecs_worker_iter_t._fields_ = [
    ("index", c_int32),
    ("count", c_int32),
]

ecs_worker_iter_t = struct_ecs_worker_iter_t  # /Users/cnifi/git/pyflecs/flecs.h: 3805


# /Users/cnifi/git/pyflecs/flecs.h: 4419
class struct_ecs_table_cache_hdr_t(Structure):
    pass


# /Users/cnifi/git/pyflecs/flecs.h: 3812
class struct_ecs_table_cache_iter_t(Structure):
    pass


struct_ecs_table_cache_iter_t.__slots__ = [
    "cur",
    "next",
    "iter_fill",
    "iter_empty",
]
struct_ecs_table_cache_iter_t._fields_ = [
    ("cur", POINTER(struct_ecs_table_cache_hdr_t)),
    ("next", POINTER(struct_ecs_table_cache_hdr_t)),
    ("iter_fill", c_bool),
    ("iter_empty", c_bool),
]

ecs_table_cache_iter_t = (
    struct_ecs_table_cache_iter_t  # /Users/cnifi/git/pyflecs/flecs.h: 3812
)


# /Users/cnifi/git/pyflecs/flecs.h: 3824
class struct_ecs_each_iter_t(Structure):
    pass


struct_ecs_each_iter_t.__slots__ = [
    "it",
    "ids",
    "sources",
    "sizes",
    "columns",
    "trs",
]
struct_ecs_each_iter_t._fields_ = [
    ("it", ecs_table_cache_iter_t),
    ("ids", ecs_id_t),
    ("sources", ecs_entity_t),
    ("sizes", ecs_size_t),
    ("columns", c_int32),
    ("trs", POINTER(ecs_table_record_t)),
]

ecs_each_iter_t = struct_ecs_each_iter_t  # /Users/cnifi/git/pyflecs/flecs.h: 3824


# /Users/cnifi/git/pyflecs/flecs.h: 3828
class struct_ecs_query_op_profile_t(Structure):
    pass


struct_ecs_query_op_profile_t.__slots__ = [
    "count",
]
struct_ecs_query_op_profile_t._fields_ = [
    ("count", c_int32 * int(2)),
]

ecs_query_op_profile_t = (
    struct_ecs_query_op_profile_t  # /Users/cnifi/git/pyflecs/flecs.h: 3828
)


# /Users/cnifi/git/pyflecs/flecs.h: 3833
class struct_ecs_query_var_t(Structure):
    pass


# /Users/cnifi/git/pyflecs/flecs.h: 3834
class struct_ecs_query_op_t(Structure):
    pass


# /Users/cnifi/git/pyflecs/flecs.h: 3835
class struct_ecs_query_op_ctx_t(Structure):
    pass


# /Users/cnifi/git/pyflecs/flecs.h: 3849
class struct_ecs_query_iter_t(Structure):
    pass


struct_ecs_query_iter_t.__slots__ = [
    "vars",
    "query_vars",
    "ops",
    "op_ctx",
    "written",
    "group",
    "tables",
    "all_tables",
    "elem",
    "cur",
    "all_cur",
    "profile",
    "op",
    "iter_single_group",
]
struct_ecs_query_iter_t._fields_ = [
    ("vars", POINTER(struct_ecs_var_t)),
    ("query_vars", POINTER(struct_ecs_query_var_t)),
    ("ops", POINTER(struct_ecs_query_op_t)),
    ("op_ctx", POINTER(struct_ecs_query_op_ctx_t)),
    ("written", POINTER(uint64_t)),
    ("group", POINTER(ecs_query_cache_group_t)),
    ("tables", POINTER(ecs_vec_t)),
    ("all_tables", POINTER(ecs_vec_t)),
    ("elem", POINTER(ecs_query_cache_match_t)),
    ("cur", c_int32),
    ("all_cur", c_int32),
    ("profile", POINTER(ecs_query_op_profile_t)),
    ("op", c_int16),
    ("iter_single_group", c_bool),
]

ecs_query_iter_t = struct_ecs_query_iter_t  # /Users/cnifi/git/pyflecs/flecs.h: 3849


# /Users/cnifi/git/pyflecs/flecs.h: 3854
class union_anon_9(Union):
    pass


union_anon_9.__slots__ = [
    "query",
    "page",
    "worker",
    "each",
]
union_anon_9._fields_ = [
    ("query", ecs_query_iter_t),
    ("page", ecs_page_iter_t),
    ("worker", ecs_worker_iter_t),
    ("each", ecs_each_iter_t),
]


# /Users/cnifi/git/pyflecs/flecs.h: 3863
class struct_ecs_iter_private_t(Structure):
    pass


struct_ecs_iter_private_t.__slots__ = [
    "iter",
    "entity_iter",
    "stack_cursor",
]
struct_ecs_iter_private_t._fields_ = [
    ("iter", union_anon_9),
    ("entity_iter", POINTER(None)),
    ("stack_cursor", POINTER(ecs_stack_cursor_t)),
]

ecs_iter_private_t = struct_ecs_iter_private_t  # /Users/cnifi/git/pyflecs/flecs.h: 3863


# /Users/cnifi/git/pyflecs/flecs.h: 3870
class struct_ecs_commands_t(Structure):
    pass


struct_ecs_commands_t.__slots__ = [
    "queue",
    "stack",
    "entries",
]
struct_ecs_commands_t._fields_ = [
    ("queue", ecs_vec_t),
    ("stack", ecs_stack_t),
    ("entries", ecs_sparse_t),
]

ecs_commands_t = struct_ecs_commands_t  # /Users/cnifi/git/pyflecs/flecs.h: 3870

# /Users/cnifi/git/pyflecs/flecs.h: 3916
if _libs["libflecs.dylib"].has("flecs_module_path_from_c", "cdecl"):
    flecs_module_path_from_c = _libs["libflecs.dylib"].get(
        "flecs_module_path_from_c", "cdecl"
    )
    flecs_module_path_from_c.argtypes = [String]
    if sizeof(c_int) == sizeof(c_void_p):
        flecs_module_path_from_c.restype = ReturnString
    else:
        flecs_module_path_from_c.restype = String
        flecs_module_path_from_c.errcheck = ReturnString

# /Users/cnifi/git/pyflecs/flecs.h: 3926
if _libs["libflecs.dylib"].has("flecs_default_ctor", "cdecl"):
    flecs_default_ctor = _libs["libflecs.dylib"].get("flecs_default_ctor", "cdecl")
    flecs_default_ctor.argtypes = [POINTER(None), c_int32, POINTER(ecs_type_info_t)]
    flecs_default_ctor.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 3938
if _libs["libflecs.dylib"].has("flecs_vasprintf", "cdecl"):
    flecs_vasprintf = _libs["libflecs.dylib"].get("flecs_vasprintf", "cdecl")
    flecs_vasprintf.argtypes = [String, c_void_p]
    if sizeof(c_int) == sizeof(c_void_p):
        flecs_vasprintf.restype = ReturnString
    else:
        flecs_vasprintf.restype = String
        flecs_vasprintf.errcheck = ReturnString

# /Users/cnifi/git/pyflecs/flecs.h: 3948
if _libs["libflecs.dylib"].has("flecs_asprintf", "cdecl"):
    _func = _libs["libflecs.dylib"].get("flecs_asprintf", "cdecl")
    _restype = String
    _errcheck = None
    _argtypes = [String]
    flecs_asprintf = _variadic_function(_func, _restype, _argtypes, _errcheck)

# /Users/cnifi/git/pyflecs/flecs.h: 3961
if _libs["libflecs.dylib"].has("flecs_chresc", "cdecl"):
    flecs_chresc = _libs["libflecs.dylib"].get("flecs_chresc", "cdecl")
    flecs_chresc.argtypes = [String, c_char, c_char]
    if sizeof(c_int) == sizeof(c_void_p):
        flecs_chresc.restype = ReturnString
    else:
        flecs_chresc.restype = String
        flecs_chresc.errcheck = ReturnString

# /Users/cnifi/git/pyflecs/flecs.h: 3973
if _libs["libflecs.dylib"].has("flecs_chrparse", "cdecl"):
    flecs_chrparse = _libs["libflecs.dylib"].get("flecs_chrparse", "cdecl")
    flecs_chrparse.argtypes = [String, String]
    flecs_chrparse.restype = c_char_p

# /Users/cnifi/git/pyflecs/flecs.h: 3990
if _libs["libflecs.dylib"].has("flecs_stresc", "cdecl"):
    flecs_stresc = _libs["libflecs.dylib"].get("flecs_stresc", "cdecl")
    flecs_stresc.argtypes = [String, ecs_size_t, c_char, String]
    flecs_stresc.restype = ecs_size_t

# /Users/cnifi/git/pyflecs/flecs.h: 4005
if _libs["libflecs.dylib"].has("flecs_astresc", "cdecl"):
    flecs_astresc = _libs["libflecs.dylib"].get("flecs_astresc", "cdecl")
    flecs_astresc.argtypes = [c_char, String]
    if sizeof(c_int) == sizeof(c_void_p):
        flecs_astresc.restype = ReturnString
    else:
        flecs_astresc.restype = String
        flecs_astresc.errcheck = ReturnString

# /Users/cnifi/git/pyflecs/flecs.h: 4016
if _libs["libflecs.dylib"].has("flecs_parse_ws_eol", "cdecl"):
    flecs_parse_ws_eol = _libs["libflecs.dylib"].get("flecs_parse_ws_eol", "cdecl")
    flecs_parse_ws_eol.argtypes = [String]
    flecs_parse_ws_eol.restype = c_char_p

# /Users/cnifi/git/pyflecs/flecs.h: 4028
if _libs["libflecs.dylib"].has("flecs_parse_digit", "cdecl"):
    flecs_parse_digit = _libs["libflecs.dylib"].get("flecs_parse_digit", "cdecl")
    flecs_parse_digit.argtypes = [String, String]
    flecs_parse_digit.restype = c_char_p

# /Users/cnifi/git/pyflecs/flecs.h: 4034
if _libs["libflecs.dylib"].has("flecs_to_snake_case", "cdecl"):
    flecs_to_snake_case = _libs["libflecs.dylib"].get("flecs_to_snake_case", "cdecl")
    flecs_to_snake_case.argtypes = [String]
    if sizeof(c_int) == sizeof(c_void_p):
        flecs_to_snake_case.restype = ReturnString
    else:
        flecs_to_snake_case.restype = String
        flecs_to_snake_case.errcheck = ReturnString


# /Users/cnifi/git/pyflecs/flecs.h: 4061
class struct_ecs_suspend_readonly_state_t(Structure):
    pass


struct_ecs_suspend_readonly_state_t.__slots__ = [
    "is_readonly",
    "is_deferred",
    "cmd_flushing",
    "defer_count",
    "scope",
    "with",
    "cmd_stack",
    "cmd",
    "stage",
]
struct_ecs_suspend_readonly_state_t._fields_ = [
    ("is_readonly", c_bool),
    ("is_deferred", c_bool),
    ("cmd_flushing", c_bool),
    ("defer_count", c_int32),
    ("scope", ecs_entity_t),
    ("with", ecs_entity_t),
    ("cmd_stack", ecs_commands_t * int(2)),
    ("cmd", POINTER(ecs_commands_t)),
    ("stage", POINTER(ecs_stage_t)),
]

ecs_suspend_readonly_state_t = (
    struct_ecs_suspend_readonly_state_t  # /Users/cnifi/git/pyflecs/flecs.h: 4061
)

# /Users/cnifi/git/pyflecs/flecs.h: 4064
if _libs["libflecs.dylib"].has("flecs_suspend_readonly", "cdecl"):
    flecs_suspend_readonly = _libs["libflecs.dylib"].get(
        "flecs_suspend_readonly", "cdecl"
    )
    flecs_suspend_readonly.argtypes = [
        POINTER(ecs_world_t),
        POINTER(ecs_suspend_readonly_state_t),
    ]
    flecs_suspend_readonly.restype = POINTER(ecs_world_t)

# /Users/cnifi/git/pyflecs/flecs.h: 4069
if _libs["libflecs.dylib"].has("flecs_resume_readonly", "cdecl"):
    flecs_resume_readonly = _libs["libflecs.dylib"].get(
        "flecs_resume_readonly", "cdecl"
    )
    flecs_resume_readonly.argtypes = [
        POINTER(ecs_world_t),
        POINTER(ecs_suspend_readonly_state_t),
    ]
    flecs_resume_readonly.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 4079
if _libs["libflecs.dylib"].has("flecs_table_observed_count", "cdecl"):
    flecs_table_observed_count = _libs["libflecs.dylib"].get(
        "flecs_table_observed_count", "cdecl"
    )
    flecs_table_observed_count.argtypes = [POINTER(ecs_table_t)]
    flecs_table_observed_count.restype = c_int32

# /Users/cnifi/git/pyflecs/flecs.h: 4087
if _libs["libflecs.dylib"].has("flecs_dump_backtrace", "cdecl"):
    flecs_dump_backtrace = _libs["libflecs.dylib"].get("flecs_dump_backtrace", "cdecl")
    flecs_dump_backtrace.argtypes = [POINTER(None)]
    flecs_dump_backtrace.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 4096
if _libs["libflecs.dylib"].has("flecs_poly_claim_", "cdecl"):
    flecs_poly_claim_ = _libs["libflecs.dylib"].get("flecs_poly_claim_", "cdecl")
    flecs_poly_claim_.argtypes = [POINTER(ecs_poly_t)]
    flecs_poly_claim_.restype = c_int32

# /Users/cnifi/git/pyflecs/flecs.h: 4105
if _libs["libflecs.dylib"].has("flecs_poly_release_", "cdecl"):
    flecs_poly_release_ = _libs["libflecs.dylib"].get("flecs_poly_release_", "cdecl")
    flecs_poly_release_.argtypes = [POINTER(ecs_poly_t)]
    flecs_poly_release_.restype = c_int32

# /Users/cnifi/git/pyflecs/flecs.h: 4120
if _libs["libflecs.dylib"].has("flecs_poly_refcount", "cdecl"):
    flecs_poly_refcount = _libs["libflecs.dylib"].get("flecs_poly_refcount", "cdecl")
    flecs_poly_refcount.argtypes = [POINTER(ecs_poly_t)]
    flecs_poly_refcount.restype = c_int32

# /Users/cnifi/git/pyflecs/flecs.h: 4130
if _libs["libflecs.dylib"].has("flecs_component_ids_index_get", "cdecl"):
    flecs_component_ids_index_get = _libs["libflecs.dylib"].get(
        "flecs_component_ids_index_get", "cdecl"
    )
    flecs_component_ids_index_get.argtypes = []
    flecs_component_ids_index_get.restype = c_int32

# /Users/cnifi/git/pyflecs/flecs.h: 4139
if _libs["libflecs.dylib"].has("flecs_component_ids_get", "cdecl"):
    flecs_component_ids_get = _libs["libflecs.dylib"].get(
        "flecs_component_ids_get", "cdecl"
    )
    flecs_component_ids_get.argtypes = [POINTER(ecs_world_t), c_int32]
    flecs_component_ids_get.restype = ecs_entity_t

# /Users/cnifi/git/pyflecs/flecs.h: 4152
if _libs["libflecs.dylib"].has("flecs_component_ids_get_alive", "cdecl"):
    flecs_component_ids_get_alive = _libs["libflecs.dylib"].get(
        "flecs_component_ids_get_alive", "cdecl"
    )
    flecs_component_ids_get_alive.argtypes = [POINTER(ecs_world_t), c_int32]
    flecs_component_ids_get_alive.restype = ecs_entity_t

# /Users/cnifi/git/pyflecs/flecs.h: 4163
if _libs["libflecs.dylib"].has("flecs_component_ids_set", "cdecl"):
    flecs_component_ids_set = _libs["libflecs.dylib"].get(
        "flecs_component_ids_set", "cdecl"
    )
    flecs_component_ids_set.argtypes = [POINTER(ecs_world_t), c_int32, ecs_entity_t]
    flecs_component_ids_set.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 4176
if _libs["libflecs.dylib"].has("flecs_query_trivial_cached_next", "cdecl"):
    flecs_query_trivial_cached_next = _libs["libflecs.dylib"].get(
        "flecs_query_trivial_cached_next", "cdecl"
    )
    flecs_query_trivial_cached_next.argtypes = [POINTER(ecs_iter_t)]
    flecs_query_trivial_cached_next.restype = c_bool

# /Users/cnifi/git/pyflecs/flecs.h: 4195
if _libs["libflecs.dylib"].has("flecs_check_exclusive_world_access_write", "cdecl"):
    flecs_check_exclusive_world_access_write = _libs["libflecs.dylib"].get(
        "flecs_check_exclusive_world_access_write", "cdecl"
    )
    flecs_check_exclusive_world_access_write.argtypes = [POINTER(ecs_world_t)]
    flecs_check_exclusive_world_access_write.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 4203
if _libs["libflecs.dylib"].has("flecs_check_exclusive_world_access_read", "cdecl"):
    flecs_check_exclusive_world_access_read = _libs["libflecs.dylib"].get(
        "flecs_check_exclusive_world_access_read", "cdecl"
    )
    flecs_check_exclusive_world_access_read.argtypes = [POINTER(ecs_world_t)]
    flecs_check_exclusive_world_access_read.restype = None


# /Users/cnifi/git/pyflecs/flecs.h: 4263
class struct_anon_10(Structure):
    pass


struct_anon_10.__slots__ = [
    "keys",
    "values",
]
struct_anon_10._fields_ = [
    ("keys", ecs_vec_t),
    ("values", ecs_vec_t),
]

ecs_hm_bucket_t = struct_anon_10  # /Users/cnifi/git/pyflecs/flecs.h: 4263


# /Users/cnifi/git/pyflecs/flecs.h: 4273
class struct_anon_11(Structure):
    pass


struct_anon_11.__slots__ = [
    "hash",
    "compare",
    "key_size",
    "value_size",
    "hashmap_allocator",
    "bucket_allocator",
    "impl",
]
struct_anon_11._fields_ = [
    ("hash", ecs_hash_value_action_t),
    ("compare", ecs_compare_action_t),
    ("key_size", ecs_size_t),
    ("value_size", ecs_size_t),
    ("hashmap_allocator", POINTER(ecs_block_allocator_t)),
    ("bucket_allocator", ecs_block_allocator_t),
    ("impl", ecs_map_t),
]

ecs_hashmap_t = struct_anon_11  # /Users/cnifi/git/pyflecs/flecs.h: 4273


# /Users/cnifi/git/pyflecs/flecs.h: 4279
class struct_anon_12(Structure):
    pass


struct_anon_12.__slots__ = [
    "it",
    "bucket",
    "index",
]
struct_anon_12._fields_ = [
    ("it", ecs_map_iter_t),
    ("bucket", POINTER(ecs_hm_bucket_t)),
    ("index", c_int32),
]

flecs_hashmap_iter_t = struct_anon_12  # /Users/cnifi/git/pyflecs/flecs.h: 4279


# /Users/cnifi/git/pyflecs/flecs.h: 4285
class struct_anon_13(Structure):
    pass


struct_anon_13.__slots__ = [
    "key",
    "value",
    "hash",
]
struct_anon_13._fields_ = [
    ("key", POINTER(None)),
    ("value", POINTER(None)),
    ("hash", uint64_t),
]

flecs_hashmap_result_t = struct_anon_13  # /Users/cnifi/git/pyflecs/flecs.h: 4285

# /Users/cnifi/git/pyflecs/flecs.h: 4288
if _libs["libflecs.dylib"].has("flecs_hashmap_init_", "cdecl"):
    flecs_hashmap_init_ = _libs["libflecs.dylib"].get("flecs_hashmap_init_", "cdecl")
    flecs_hashmap_init_.argtypes = [
        POINTER(ecs_hashmap_t),
        ecs_size_t,
        ecs_size_t,
        ecs_hash_value_action_t,
        ecs_compare_action_t,
        POINTER(ecs_allocator_t),
    ]
    flecs_hashmap_init_.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 4300
if _libs["libflecs.dylib"].has("flecs_hashmap_fini", "cdecl"):
    flecs_hashmap_fini = _libs["libflecs.dylib"].get("flecs_hashmap_fini", "cdecl")
    flecs_hashmap_fini.argtypes = [POINTER(ecs_hashmap_t)]
    flecs_hashmap_fini.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 4304
if _libs["libflecs.dylib"].has("flecs_hashmap_get_", "cdecl"):
    flecs_hashmap_get_ = _libs["libflecs.dylib"].get("flecs_hashmap_get_", "cdecl")
    flecs_hashmap_get_.argtypes = [
        POINTER(ecs_hashmap_t),
        ecs_size_t,
        POINTER(None),
        ecs_size_t,
    ]
    flecs_hashmap_get_.restype = POINTER(c_ubyte)
    flecs_hashmap_get_.errcheck = lambda v, *a: cast(v, c_void_p)

# /Users/cnifi/git/pyflecs/flecs.h: 4314
if _libs["libflecs.dylib"].has("flecs_hashmap_ensure_", "cdecl"):
    flecs_hashmap_ensure_ = _libs["libflecs.dylib"].get(
        "flecs_hashmap_ensure_", "cdecl"
    )
    flecs_hashmap_ensure_.argtypes = [
        POINTER(ecs_hashmap_t),
        ecs_size_t,
        POINTER(None),
        ecs_size_t,
    ]
    flecs_hashmap_ensure_.restype = flecs_hashmap_result_t

# /Users/cnifi/git/pyflecs/flecs.h: 4324
if _libs["libflecs.dylib"].has("flecs_hashmap_set_", "cdecl"):
    flecs_hashmap_set_ = _libs["libflecs.dylib"].get("flecs_hashmap_set_", "cdecl")
    flecs_hashmap_set_.argtypes = [
        POINTER(ecs_hashmap_t),
        ecs_size_t,
        POINTER(None),
        ecs_size_t,
        POINTER(None),
    ]
    flecs_hashmap_set_.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 4335
if _libs["libflecs.dylib"].has("flecs_hashmap_remove_", "cdecl"):
    flecs_hashmap_remove_ = _libs["libflecs.dylib"].get(
        "flecs_hashmap_remove_", "cdecl"
    )
    flecs_hashmap_remove_.argtypes = [
        POINTER(ecs_hashmap_t),
        ecs_size_t,
        POINTER(None),
        ecs_size_t,
    ]
    flecs_hashmap_remove_.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 4345
if _libs["libflecs.dylib"].has("flecs_hashmap_remove_w_hash_", "cdecl"):
    flecs_hashmap_remove_w_hash_ = _libs["libflecs.dylib"].get(
        "flecs_hashmap_remove_w_hash_", "cdecl"
    )
    flecs_hashmap_remove_w_hash_.argtypes = [
        POINTER(ecs_hashmap_t),
        ecs_size_t,
        POINTER(None),
        ecs_size_t,
        uint64_t,
    ]
    flecs_hashmap_remove_w_hash_.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 4356
if _libs["libflecs.dylib"].has("flecs_hashmap_get_bucket", "cdecl"):
    flecs_hashmap_get_bucket = _libs["libflecs.dylib"].get(
        "flecs_hashmap_get_bucket", "cdecl"
    )
    flecs_hashmap_get_bucket.argtypes = [POINTER(ecs_hashmap_t), uint64_t]
    flecs_hashmap_get_bucket.restype = POINTER(ecs_hm_bucket_t)

# /Users/cnifi/git/pyflecs/flecs.h: 4361
if _libs["libflecs.dylib"].has("flecs_hm_bucket_remove", "cdecl"):
    flecs_hm_bucket_remove = _libs["libflecs.dylib"].get(
        "flecs_hm_bucket_remove", "cdecl"
    )
    flecs_hm_bucket_remove.argtypes = [
        POINTER(ecs_hashmap_t),
        POINTER(ecs_hm_bucket_t),
        uint64_t,
        c_int32,
    ]
    flecs_hm_bucket_remove.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 4368
if _libs["libflecs.dylib"].has("flecs_hashmap_copy", "cdecl"):
    flecs_hashmap_copy = _libs["libflecs.dylib"].get("flecs_hashmap_copy", "cdecl")
    flecs_hashmap_copy.argtypes = [POINTER(ecs_hashmap_t), POINTER(ecs_hashmap_t)]
    flecs_hashmap_copy.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 4373
if _libs["libflecs.dylib"].has("flecs_hashmap_iter", "cdecl"):
    flecs_hashmap_iter = _libs["libflecs.dylib"].get("flecs_hashmap_iter", "cdecl")
    flecs_hashmap_iter.argtypes = [POINTER(ecs_hashmap_t)]
    flecs_hashmap_iter.restype = flecs_hashmap_iter_t

# /Users/cnifi/git/pyflecs/flecs.h: 4377
if _libs["libflecs.dylib"].has("flecs_hashmap_next_", "cdecl"):
    flecs_hashmap_next_ = _libs["libflecs.dylib"].get("flecs_hashmap_next_", "cdecl")
    flecs_hashmap_next_.argtypes = [
        POINTER(flecs_hashmap_iter_t),
        ecs_size_t,
        POINTER(None),
        ecs_size_t,
    ]
    flecs_hashmap_next_.restype = POINTER(c_ubyte)
    flecs_hashmap_next_.errcheck = lambda v, *a: cast(v, c_void_p)

struct_ecs_record_t.__slots__ = [
    "cr",
    "table",
    "row",
    "dense",
]
struct_ecs_record_t._fields_ = [
    ("cr", POINTER(ecs_component_record_t)),
    ("table", POINTER(ecs_table_t)),
    ("row", uint32_t),
    ("dense", c_int32),
]

struct_ecs_table_cache_hdr_t.__slots__ = [
    "cr",
    "table",
    "prev",
    "next",
]
struct_ecs_table_cache_hdr_t._fields_ = [
    ("cr", POINTER(struct_ecs_component_record_t)),
    ("table", POINTER(ecs_table_t)),
    ("prev", POINTER(struct_ecs_table_cache_hdr_t)),
    ("next", POINTER(struct_ecs_table_cache_hdr_t)),
]

ecs_table_cache_hdr_t = (
    struct_ecs_table_cache_hdr_t  # /Users/cnifi/git/pyflecs/flecs.h: 4423
)

struct_ecs_table_record_t.__slots__ = [
    "hdr",
    "index",
    "count",
    "column",
]
struct_ecs_table_record_t._fields_ = [
    ("hdr", ecs_table_cache_hdr_t),
    ("index", c_int16),
    ("count", c_int16),
    ("column", c_int16),
]


# /Users/cnifi/git/pyflecs/flecs.h: 4442
class struct_ecs_table_diff_t(Structure):
    pass


struct_ecs_table_diff_t.__slots__ = [
    "added",
    "removed",
    "added_flags",
    "removed_flags",
]
struct_ecs_table_diff_t._fields_ = [
    ("added", ecs_type_t),
    ("removed", ecs_type_t),
    ("added_flags", ecs_flags32_t),
    ("removed_flags", ecs_flags32_t),
]

ecs_table_diff_t = struct_ecs_table_diff_t  # /Users/cnifi/git/pyflecs/flecs.h: 4442

# /Users/cnifi/git/pyflecs/flecs.h: 4458
if _libs["libflecs.dylib"].has("ecs_record_find", "cdecl"):
    ecs_record_find = _libs["libflecs.dylib"].get("ecs_record_find", "cdecl")
    ecs_record_find.argtypes = [POINTER(ecs_world_t), ecs_entity_t]
    ecs_record_find.restype = POINTER(ecs_record_t)

# /Users/cnifi/git/pyflecs/flecs.h: 4469
if _libs["libflecs.dylib"].has("ecs_record_get_entity", "cdecl"):
    ecs_record_get_entity = _libs["libflecs.dylib"].get(
        "ecs_record_get_entity", "cdecl"
    )
    ecs_record_get_entity.argtypes = [POINTER(ecs_record_t)]
    ecs_record_get_entity.restype = ecs_entity_t

# /Users/cnifi/git/pyflecs/flecs.h: 4491
if _libs["libflecs.dylib"].has("ecs_write_begin", "cdecl"):
    ecs_write_begin = _libs["libflecs.dylib"].get("ecs_write_begin", "cdecl")
    ecs_write_begin.argtypes = [POINTER(ecs_world_t), ecs_entity_t]
    ecs_write_begin.restype = POINTER(ecs_record_t)

# /Users/cnifi/git/pyflecs/flecs.h: 4502
if _libs["libflecs.dylib"].has("ecs_write_end", "cdecl"):
    ecs_write_end = _libs["libflecs.dylib"].get("ecs_write_end", "cdecl")
    ecs_write_end.argtypes = [POINTER(ecs_record_t)]
    ecs_write_end.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 4525
if _libs["libflecs.dylib"].has("ecs_read_begin", "cdecl"):
    ecs_read_begin = _libs["libflecs.dylib"].get("ecs_read_begin", "cdecl")
    ecs_read_begin.argtypes = [POINTER(ecs_world_t), ecs_entity_t]
    ecs_read_begin.restype = POINTER(ecs_record_t)

# /Users/cnifi/git/pyflecs/flecs.h: 4535
if _libs["libflecs.dylib"].has("ecs_read_end", "cdecl"):
    ecs_read_end = _libs["libflecs.dylib"].get("ecs_read_end", "cdecl")
    ecs_read_end.argtypes = [POINTER(ecs_record_t)]
    ecs_read_end.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 4554
if _libs["libflecs.dylib"].has("ecs_record_get_id", "cdecl"):
    ecs_record_get_id = _libs["libflecs.dylib"].get("ecs_record_get_id", "cdecl")
    ecs_record_get_id.argtypes = [POINTER(ecs_world_t), POINTER(ecs_record_t), ecs_id_t]
    ecs_record_get_id.restype = POINTER(c_ubyte)
    ecs_record_get_id.errcheck = lambda v, *a: cast(v, c_void_p)

# /Users/cnifi/git/pyflecs/flecs.h: 4568
if _libs["libflecs.dylib"].has("ecs_record_ensure_id", "cdecl"):
    ecs_record_ensure_id = _libs["libflecs.dylib"].get("ecs_record_ensure_id", "cdecl")
    ecs_record_ensure_id.argtypes = [
        POINTER(ecs_world_t),
        POINTER(ecs_record_t),
        ecs_id_t,
    ]
    ecs_record_ensure_id.restype = POINTER(c_ubyte)
    ecs_record_ensure_id.errcheck = lambda v, *a: cast(v, c_void_p)

# /Users/cnifi/git/pyflecs/flecs.h: 4581
if _libs["libflecs.dylib"].has("ecs_record_has_id", "cdecl"):
    ecs_record_has_id = _libs["libflecs.dylib"].get("ecs_record_has_id", "cdecl")
    ecs_record_has_id.argtypes = [POINTER(ecs_world_t), POINTER(ecs_record_t), ecs_id_t]
    ecs_record_has_id.restype = c_bool

# /Users/cnifi/git/pyflecs/flecs.h: 4603
if _libs["libflecs.dylib"].has("ecs_record_get_by_column", "cdecl"):
    ecs_record_get_by_column = _libs["libflecs.dylib"].get(
        "ecs_record_get_by_column", "cdecl"
    )
    ecs_record_get_by_column.argtypes = [POINTER(ecs_record_t), c_int32, c_size_t]
    ecs_record_get_by_column.restype = POINTER(c_ubyte)
    ecs_record_get_by_column.errcheck = lambda v, *a: cast(v, c_void_p)

# /Users/cnifi/git/pyflecs/flecs.h: 4615
if _libs["libflecs.dylib"].has("flecs_components_get", "cdecl"):
    flecs_components_get = _libs["libflecs.dylib"].get("flecs_components_get", "cdecl")
    flecs_components_get.argtypes = [POINTER(ecs_world_t), ecs_id_t]
    flecs_components_get.restype = POINTER(ecs_component_record_t)

# /Users/cnifi/git/pyflecs/flecs.h: 4625
if _libs["libflecs.dylib"].has("flecs_component_get_id", "cdecl"):
    flecs_component_get_id = _libs["libflecs.dylib"].get(
        "flecs_component_get_id", "cdecl"
    )
    flecs_component_get_id.argtypes = [POINTER(ecs_component_record_t)]
    flecs_component_get_id.restype = ecs_id_t

# /Users/cnifi/git/pyflecs/flecs.h: 4637
if _libs["libflecs.dylib"].has("flecs_component_get_table", "cdecl"):
    flecs_component_get_table = _libs["libflecs.dylib"].get(
        "flecs_component_get_table", "cdecl"
    )
    flecs_component_get_table.argtypes = [
        POINTER(ecs_component_record_t),
        POINTER(ecs_table_t),
    ]
    flecs_component_get_table.restype = POINTER(ecs_table_record_t)

# /Users/cnifi/git/pyflecs/flecs.h: 4663
if _libs["libflecs.dylib"].has("flecs_component_iter", "cdecl"):
    flecs_component_iter = _libs["libflecs.dylib"].get("flecs_component_iter", "cdecl")
    flecs_component_iter.argtypes = [
        POINTER(ecs_component_record_t),
        POINTER(ecs_table_cache_iter_t),
    ]
    flecs_component_iter.restype = c_bool

# /Users/cnifi/git/pyflecs/flecs.h: 4674
if _libs["libflecs.dylib"].has("flecs_component_next", "cdecl"):
    flecs_component_next = _libs["libflecs.dylib"].get("flecs_component_next", "cdecl")
    flecs_component_next.argtypes = [POINTER(ecs_table_cache_iter_t)]
    flecs_component_next.restype = POINTER(ecs_table_record_t)


# /Users/cnifi/git/pyflecs/flecs.h: 4681
class struct_ecs_table_records_t(Structure):
    pass


struct_ecs_table_records_t.__slots__ = [
    "array",
    "count",
]
struct_ecs_table_records_t._fields_ = [
    ("array", POINTER(ecs_table_record_t)),
    ("count", c_int32),
]

ecs_table_records_t = (
    struct_ecs_table_records_t  # /Users/cnifi/git/pyflecs/flecs.h: 4681
)

# /Users/cnifi/git/pyflecs/flecs.h: 4690
if _libs["libflecs.dylib"].has("flecs_table_records", "cdecl"):
    flecs_table_records = _libs["libflecs.dylib"].get("flecs_table_records", "cdecl")
    flecs_table_records.argtypes = [POINTER(ecs_table_t)]
    flecs_table_records.restype = ecs_table_records_t

# /Users/cnifi/git/pyflecs/flecs.h: 4699
if _libs["libflecs.dylib"].has("flecs_table_record_get_component", "cdecl"):
    flecs_table_record_get_component = _libs["libflecs.dylib"].get(
        "flecs_table_record_get_component", "cdecl"
    )
    flecs_table_record_get_component.argtypes = [POINTER(ecs_table_record_t)]
    flecs_table_record_get_component.restype = POINTER(ecs_component_record_t)

# /Users/cnifi/git/pyflecs/flecs.h: 4709
if _libs["libflecs.dylib"].has("flecs_table_id", "cdecl"):
    flecs_table_id = _libs["libflecs.dylib"].get("flecs_table_id", "cdecl")
    flecs_table_id.argtypes = [POINTER(ecs_table_t)]
    flecs_table_id.restype = uint64_t

# /Users/cnifi/git/pyflecs/flecs.h: 4723
if _libs["libflecs.dylib"].has("flecs_table_traverse_add", "cdecl"):
    flecs_table_traverse_add = _libs["libflecs.dylib"].get(
        "flecs_table_traverse_add", "cdecl"
    )
    flecs_table_traverse_add.argtypes = [
        POINTER(ecs_world_t),
        POINTER(ecs_table_t),
        POINTER(ecs_id_t),
        POINTER(ecs_table_diff_t),
    ]
    flecs_table_traverse_add.restype = POINTER(ecs_table_t)


# /Users/cnifi/git/pyflecs/flecs.h: 4740
class struct_ecs_value_t(Structure):
    pass


struct_ecs_value_t.__slots__ = [
    "type",
    "ptr",
]
struct_ecs_value_t._fields_ = [
    ("type", ecs_entity_t),
    ("ptr", POINTER(None)),
]

ecs_value_t = struct_ecs_value_t  # /Users/cnifi/git/pyflecs/flecs.h: 4740


# /Users/cnifi/git/pyflecs/flecs.h: 4786
class struct_ecs_entity_desc_t(Structure):
    pass


struct_ecs_entity_desc_t.__slots__ = [
    "_canary",
    "id",
    "parent",
    "name",
    "sep",
    "root_sep",
    "symbol",
    "use_low_id",
    "add",
    "set",
    "add_expr",
]

struct_ecs_entity_desc_t._fields_ = [
    ("_canary", c_int32),
    ("id", ecs_entity_t),
    ("parent", ecs_entity_t),
    ("name", String),
    ("sep", String),
    ("root_sep", String),
    ("symbol", String),
    ("use_low_id", c_bool),
    ("add", POINTER(ecs_id_t)),
    ("set", POINTER(ecs_value_t)),
    ("add_expr", String),
]

ecs_entity_desc_t = struct_ecs_entity_desc_t  # /Users/cnifi/git/pyflecs/flecs.h: 4786


# /Users/cnifi/git/pyflecs/flecs.h: 4816
class struct_ecs_bulk_desc_t(Structure):
    pass


struct_ecs_bulk_desc_t.__slots__ = [
    "_canary",
    "entities",
    "count",
    "ids",
    "data",
    "table",
]
struct_ecs_bulk_desc_t._fields_ = [
    ("_canary", c_int32),
    ("entities", POINTER(ecs_entity_t)),
    ("count", c_int32),
    ("ids", ecs_id_t * int(32)),
    ("data", POINTER(POINTER(None))),
    ("table", POINTER(ecs_table_t)),
]

ecs_bulk_desc_t = struct_ecs_bulk_desc_t  # /Users/cnifi/git/pyflecs/flecs.h: 4816


# /Users/cnifi/git/pyflecs/flecs.h: 4830
class struct_ecs_component_desc_t(Structure):
    pass


struct_ecs_component_desc_t.__slots__ = [
    "_canary",
    "entity",
    "type",
]
struct_ecs_component_desc_t._fields_ = [
    ("_canary", c_int32),
    ("entity", ecs_entity_t),
    ("type", ecs_type_info_t),
]

ecs_component_desc_t = (
    struct_ecs_component_desc_t  # /Users/cnifi/git/pyflecs/flecs.h: 4830
)

struct_ecs_iter_t.__slots__ = [
    "world",
    "real_world",
    "offset",
    "count",
    "entities",
    "ptrs",
    "trs",
    "sizes",
    "table",
    "other_table",
    "ids",
    "sources",
    "constrained_vars",
    "set_fields",
    "ref_fields",
    "row_fields",
    "up_fields",
    "system",
    "event",
    "event_id",
    "event_cur",
    "field_count",
    "term_index",
    "query",
    "param",
    "ctx",
    "binding_ctx",
    "callback_ctx",
    "run_ctx",
    "delta_time",
    "delta_system_time",
    "frame_offset",
    "flags",
    "interrupted_by",
    "priv_",
    "next",
    "callback",
    "fini",
    "chain_it",
]
struct_ecs_iter_t._fields_ = [
    ("world", POINTER(ecs_world_t)),
    ("real_world", POINTER(ecs_world_t)),
    ("offset", c_int32),
    ("count", c_int32),
    ("entities", POINTER(ecs_entity_t)),
    ("ptrs", POINTER(POINTER(None))),
    ("trs", POINTER(POINTER(ecs_table_record_t))),
    ("sizes", POINTER(ecs_size_t)),
    ("table", POINTER(ecs_table_t)),
    ("other_table", POINTER(ecs_table_t)),
    ("ids", POINTER(ecs_id_t)),
    ("sources", POINTER(ecs_entity_t)),
    ("constrained_vars", ecs_flags64_t),
    ("set_fields", ecs_flags32_t),
    ("ref_fields", ecs_flags32_t),
    ("row_fields", ecs_flags32_t),
    ("up_fields", ecs_flags32_t),
    ("system", ecs_entity_t),
    ("event", ecs_entity_t),
    ("event_id", ecs_id_t),
    ("event_cur", c_int32),
    ("field_count", c_int8),
    ("term_index", c_int8),
    ("query", POINTER(ecs_query_t)),
    ("param", POINTER(None)),
    ("ctx", POINTER(None)),
    ("binding_ctx", POINTER(None)),
    ("callback_ctx", POINTER(None)),
    ("run_ctx", POINTER(None)),
    ("delta_time", c_float),
    ("delta_system_time", c_float),
    ("frame_offset", c_int32),
    ("flags", ecs_flags32_t),
    ("interrupted_by", ecs_entity_t),
    ("priv_", ecs_iter_private_t),
    ("next", ecs_iter_next_action_t),
    ("callback", ecs_iter_action_t),
    ("fini", ecs_iter_fini_action_t),
    ("chain_it", POINTER(ecs_iter_t)),
]


# /Users/cnifi/git/pyflecs/flecs.h: 5047
class struct_ecs_query_desc_t(Structure):
    pass


struct_ecs_query_desc_t.__slots__ = [
    "_canary",
    "terms",
    "expr",
    "cache_kind",
    "flags",
    "order_by_callback",
    "order_by_table_callback",
    "order_by",
    "group_by",
    "group_by_callback",
    "on_group_create",
    "on_group_delete",
    "group_by_ctx",
    "group_by_ctx_free",
    "ctx",
    "binding_ctx",
    "ctx_free",
    "binding_ctx_free",
    "entity",
]
struct_ecs_query_desc_t._fields_ = [
    ("_canary", c_int32),
    ("terms", ecs_term_t * int(32)),
    ("expr", String),
    ("cache_kind", ecs_query_cache_kind_t),
    ("flags", ecs_flags32_t),
    ("order_by_callback", ecs_order_by_action_t),
    ("order_by_table_callback", ecs_sort_table_action_t),
    ("order_by", ecs_entity_t),
    ("group_by", ecs_id_t),
    ("group_by_callback", ecs_group_by_action_t),
    ("on_group_create", ecs_group_create_action_t),
    ("on_group_delete", ecs_group_delete_action_t),
    ("group_by_ctx", POINTER(None)),
    ("group_by_ctx_free", ecs_ctx_free_t),
    ("ctx", POINTER(None)),
    ("binding_ctx", POINTER(None)),
    ("ctx_free", ecs_ctx_free_t),
    ("binding_ctx_free", ecs_ctx_free_t),
    ("entity", ecs_entity_t),
]

ecs_query_desc_t = struct_ecs_query_desc_t  # /Users/cnifi/git/pyflecs/flecs.h: 5047


# /Users/cnifi/git/pyflecs/flecs.h: 5109
class struct_ecs_observer_desc_t(Structure):
    pass


struct_ecs_observer_desc_t.__slots__ = [
    "_canary",
    "entity",
    "query",
    "events",
    "yield_existing",
    "callback",
    "run",
    "ctx",
    "ctx_free",
    "callback_ctx",
    "callback_ctx_free",
    "run_ctx",
    "run_ctx_free",
    "observable",
    "last_event_id",
    "term_index_",
    "flags_",
]
struct_ecs_observer_desc_t._fields_ = [
    ("_canary", c_int32),
    ("entity", ecs_entity_t),
    ("query", ecs_query_desc_t),
    ("events", ecs_entity_t * int(8)),
    ("yield_existing", c_bool),
    ("callback", ecs_iter_action_t),
    ("run", ecs_run_action_t),
    ("ctx", POINTER(None)),
    ("ctx_free", ecs_ctx_free_t),
    ("callback_ctx", POINTER(None)),
    ("callback_ctx_free", ecs_ctx_free_t),
    ("run_ctx", POINTER(None)),
    ("run_ctx_free", ecs_ctx_free_t),
    ("observable", POINTER(ecs_poly_t)),
    ("last_event_id", POINTER(c_int32)),
    ("term_index_", c_int8),
    ("flags_", ecs_flags32_t),
]

ecs_observer_desc_t = (
    struct_ecs_observer_desc_t  # /Users/cnifi/git/pyflecs/flecs.h: 5109
)


# /Users/cnifi/git/pyflecs/flecs.h: 5158
class struct_ecs_event_desc_t(Structure):
    pass


struct_ecs_event_desc_t.__slots__ = [
    "event",
    "ids",
    "table",
    "other_table",
    "offset",
    "count",
    "entity",
    "param",
    "const_param",
    "observable",
    "flags",
]
struct_ecs_event_desc_t._fields_ = [
    ("event", ecs_entity_t),
    ("ids", POINTER(ecs_type_t)),
    ("table", POINTER(ecs_table_t)),
    ("other_table", POINTER(ecs_table_t)),
    ("offset", c_int32),
    ("count", c_int32),
    ("entity", ecs_entity_t),
    ("param", POINTER(None)),
    ("const_param", POINTER(None)),
    ("observable", POINTER(ecs_poly_t)),
    ("flags", ecs_flags32_t),
]

ecs_event_desc_t = struct_ecs_event_desc_t  # /Users/cnifi/git/pyflecs/flecs.h: 5158


# /Users/cnifi/git/pyflecs/flecs.h: 5179
class struct_ecs_build_info_t(Structure):
    pass


struct_ecs_build_info_t.__slots__ = [
    "compiler",
    "addons",
    "version",
    "version_major",
    "version_minor",
    "version_patch",
    "debug",
    "sanitize",
    "perf_trace",
]
struct_ecs_build_info_t._fields_ = [
    ("compiler", String),
    ("addons", POINTER(POINTER(c_char))),
    ("version", String),
    ("version_major", c_int16),
    ("version_minor", c_int16),
    ("version_patch", c_int16),
    ("debug", c_bool),
    ("sanitize", c_bool),
    ("perf_trace", c_bool),
]

ecs_build_info_t = struct_ecs_build_info_t  # /Users/cnifi/git/pyflecs/flecs.h: 5179


# /Users/cnifi/git/pyflecs/flecs.h: 5219
class struct_anon_14(Structure):
    pass


struct_anon_14.__slots__ = [
    "add_count",
    "remove_count",
    "delete_count",
    "clear_count",
    "set_count",
    "ensure_count",
    "modified_count",
    "discard_count",
    "event_count",
    "other_count",
    "batched_entity_count",
    "batched_command_count",
]
struct_anon_14._fields_ = [
    ("add_count", c_int64),
    ("remove_count", c_int64),
    ("delete_count", c_int64),
    ("clear_count", c_int64),
    ("set_count", c_int64),
    ("ensure_count", c_int64),
    ("modified_count", c_int64),
    ("discard_count", c_int64),
    ("event_count", c_int64),
    ("other_count", c_int64),
    ("batched_entity_count", c_int64),
    ("batched_command_count", c_int64),
]


# /Users/cnifi/git/pyflecs/flecs.h: 5238
class struct_ecs_world_info_t(Structure):
    pass


struct_ecs_world_info_t.__slots__ = [
    "last_component_id",
    "min_id",
    "max_id",
    "delta_time_raw",
    "delta_time",
    "time_scale",
    "target_fps",
    "frame_time_total",
    "system_time_total",
    "emit_time_total",
    "merge_time_total",
    "rematch_time_total",
    "world_time_total",
    "world_time_total_raw",
    "frame_count_total",
    "merge_count_total",
    "eval_comp_monitors_total",
    "rematch_count_total",
    "id_create_total",
    "id_delete_total",
    "table_create_total",
    "table_delete_total",
    "pipeline_build_count_total",
    "systems_ran_frame",
    "observers_ran_frame",
    "tag_id_count",
    "component_id_count",
    "pair_id_count",
    "table_count",
    "cmd",
    "name_prefix",
]
struct_ecs_world_info_t._fields_ = [
    ("last_component_id", ecs_entity_t),
    ("min_id", ecs_entity_t),
    ("max_id", ecs_entity_t),
    ("delta_time_raw", c_float),
    ("delta_time", c_float),
    ("time_scale", c_float),
    ("target_fps", c_float),
    ("frame_time_total", c_float),
    ("system_time_total", c_float),
    ("emit_time_total", c_float),
    ("merge_time_total", c_float),
    ("rematch_time_total", c_float),
    ("world_time_total", c_double),
    ("world_time_total_raw", c_double),
    ("frame_count_total", c_int64),
    ("merge_count_total", c_int64),
    ("eval_comp_monitors_total", c_int64),
    ("rematch_count_total", c_int64),
    ("id_create_total", c_int64),
    ("id_delete_total", c_int64),
    ("table_create_total", c_int64),
    ("table_delete_total", c_int64),
    ("pipeline_build_count_total", c_int64),
    ("systems_ran_frame", c_int64),
    ("observers_ran_frame", c_int64),
    ("tag_id_count", c_int32),
    ("component_id_count", c_int32),
    ("pair_id_count", c_int32),
    ("table_count", c_int32),
    ("cmd", struct_anon_14),
    ("name_prefix", String),
]

ecs_world_info_t = struct_ecs_world_info_t  # /Users/cnifi/git/pyflecs/flecs.h: 5238


# /Users/cnifi/git/pyflecs/flecs.h: 5246
class struct_ecs_query_group_info_t(Structure):
    pass


struct_ecs_query_group_info_t.__slots__ = [
    "id",
    "match_count",
    "table_count",
    "ctx",
]
struct_ecs_query_group_info_t._fields_ = [
    ("id", uint64_t),
    ("match_count", c_int32),
    ("table_count", c_int32),
    ("ctx", POINTER(None)),
]

ecs_query_group_info_t = (
    struct_ecs_query_group_info_t  # /Users/cnifi/git/pyflecs/flecs.h: 5246
)


# /Users/cnifi/git/pyflecs/flecs.h: 5264
class struct_EcsIdentifier(Structure):
    pass


struct_EcsIdentifier.__slots__ = [
    "value",
    "length",
    "hash",
    "index_hash",
    "index",
]
struct_EcsIdentifier._fields_ = [
    ("value", String),
    ("length", ecs_size_t),
    ("hash", uint64_t),
    ("index_hash", uint64_t),
    ("index", POINTER(ecs_hashmap_t)),
]

EcsIdentifier = struct_EcsIdentifier  # /Users/cnifi/git/pyflecs/flecs.h: 5264


# /Users/cnifi/git/pyflecs/flecs.h: 5270
class struct_EcsComponent(Structure):
    pass


struct_EcsComponent.__slots__ = [
    "size",
    "alignment",
]
struct_EcsComponent._fields_ = [
    ("size", ecs_size_t),
    ("alignment", ecs_size_t),
]

EcsComponent = struct_EcsComponent  # /Users/cnifi/git/pyflecs/flecs.h: 5270


# /Users/cnifi/git/pyflecs/flecs.h: 5275
class struct_EcsPoly(Structure):
    pass


struct_EcsPoly.__slots__ = [
    "poly",
]
struct_EcsPoly._fields_ = [
    ("poly", POINTER(ecs_poly_t)),
]

EcsPoly = struct_EcsPoly  # /Users/cnifi/git/pyflecs/flecs.h: 5275


# /Users/cnifi/git/pyflecs/flecs.h: 5284
class struct_EcsDefaultChildComponent(Structure):
    pass


struct_EcsDefaultChildComponent.__slots__ = [
    "component",
]
struct_EcsDefaultChildComponent._fields_ = [
    ("component", ecs_id_t),
]

EcsDefaultChildComponent = (
    struct_EcsDefaultChildComponent  # /Users/cnifi/git/pyflecs/flecs.h: 5284
)

# /Users/cnifi/git/pyflecs/flecs.h: 5330
try:
    ECS_PAIR = (ecs_id_t).in_dll(_libs["libflecs.dylib"], "ECS_PAIR")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5333
try:
    ECS_AUTO_OVERRIDE = (ecs_id_t).in_dll(_libs["libflecs.dylib"], "ECS_AUTO_OVERRIDE")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5336
try:
    ECS_TOGGLE = (ecs_id_t).in_dll(_libs["libflecs.dylib"], "ECS_TOGGLE")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5348
try:
    FLECS_IDEcsComponentID_ = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "FLECS_IDEcsComponentID_"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5351
try:
    FLECS_IDEcsIdentifierID_ = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "FLECS_IDEcsIdentifierID_"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5354
try:
    FLECS_IDEcsPolyID_ = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "FLECS_IDEcsPolyID_"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5357
try:
    FLECS_IDEcsDefaultChildComponentID_ = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "FLECS_IDEcsDefaultChildComponentID_"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5360
try:
    EcsQuery = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsQuery")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5363
try:
    EcsObserver = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsObserver")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5366
try:
    EcsSystem = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsSystem")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5369
try:
    FLECS_IDEcsTickSourceID_ = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "FLECS_IDEcsTickSourceID_"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5372
for _lib in _libs.values():
    try:
        FLECS_IDEcsPipelineQueryID_ = (ecs_entity_t).in_dll(
            _lib, "FLECS_IDEcsPipelineQueryID_"
        )
        break
    except:
        pass

# /Users/cnifi/git/pyflecs/flecs.h: 5375
try:
    FLECS_IDEcsTimerID_ = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "FLECS_IDEcsTimerID_"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5378
try:
    FLECS_IDEcsRateFilterID_ = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "FLECS_IDEcsRateFilterID_"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5381
try:
    EcsFlecs = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsFlecs")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5384
try:
    EcsFlecsCore = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsFlecsCore")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5387
try:
    EcsWorld = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsWorld")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5390
try:
    EcsWildcard = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsWildcard")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5393
try:
    EcsAny = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsAny")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5396
try:
    EcsThis = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsThis")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5399
try:
    EcsVariable = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsVariable")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5411
try:
    EcsTransitive = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsTransitive")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5420
try:
    EcsReflexive = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsReflexive")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5432
try:
    EcsFinal = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsFinal")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5439
try:
    EcsInheritable = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsInheritable")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5442
try:
    EcsOnInstantiate = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "EcsOnInstantiate"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5447
try:
    EcsOverride = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsOverride")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5452
try:
    EcsInherit = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsInherit")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5458
try:
    EcsDontInherit = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsDontInherit")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5467
try:
    EcsSymmetric = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsSymmetric")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5478
try:
    EcsExclusive = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsExclusive")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5481
try:
    EcsAcyclic = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsAcyclic")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5485
try:
    EcsTraversable = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsTraversable")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5496
try:
    EcsWith = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsWith")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5507
try:
    EcsOneOf = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsOneOf")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5510
try:
    EcsCanToggle = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsCanToggle")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5515
try:
    EcsTrait = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsTrait")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5526
try:
    EcsRelationship = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsRelationship")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5537
try:
    EcsTarget = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsTarget")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5541
try:
    EcsPairIsTag = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsPairIsTag")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5544
try:
    EcsName = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsName")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5547
try:
    EcsSymbol = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsSymbol")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5550
try:
    EcsAlias = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsAlias")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5553
try:
    EcsChildOf = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsChildOf")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5556
try:
    EcsIsA = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsIsA")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5559
try:
    EcsDependsOn = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsDependsOn")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5562
try:
    EcsSlotOf = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsSlotOf")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5565
try:
    EcsOrderedChildren = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "EcsOrderedChildren"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5568
try:
    EcsModule = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsModule")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5571
try:
    EcsPrivate = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsPrivate")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5575
try:
    EcsPrefab = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsPrefab")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5579
try:
    EcsDisabled = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsDisabled")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5584
try:
    EcsNotQueryable = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsNotQueryable")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5587
try:
    EcsOnAdd = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsOnAdd")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5590
try:
    EcsOnRemove = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsOnRemove")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5593
try:
    EcsOnSet = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsOnSet")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5596
try:
    EcsMonitor = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsMonitor")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5599
try:
    EcsOnTableCreate = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "EcsOnTableCreate"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5602
try:
    EcsOnTableDelete = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "EcsOnTableDelete"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5605
try:
    EcsOnDelete = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsOnDelete")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5609
try:
    EcsOnDeleteTarget = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "EcsOnDeleteTarget"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5613
try:
    EcsRemove = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsRemove")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5617
try:
    EcsDelete = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsDelete")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5621
try:
    EcsPanic = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsPanic")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5624
try:
    EcsSparse = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsSparse")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5627
try:
    EcsDontFragment = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsDontFragment")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5630
try:
    EcsPredEq = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsPredEq")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5633
try:
    EcsPredMatch = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsPredMatch")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5636
try:
    EcsPredLookup = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsPredLookup")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5639
try:
    EcsScopeOpen = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsScopeOpen")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5642
try:
    EcsScopeClose = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsScopeClose")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5648
try:
    EcsEmpty = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsEmpty")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5650
try:
    FLECS_IDEcsPipelineID_ = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "FLECS_IDEcsPipelineID_"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5651
try:
    EcsOnStart = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsOnStart")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5652
try:
    EcsPreFrame = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsPreFrame")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5653
try:
    EcsOnLoad = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsOnLoad")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5654
try:
    EcsPostLoad = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsPostLoad")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5655
try:
    EcsPreUpdate = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsPreUpdate")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5656
try:
    EcsOnUpdate = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsOnUpdate")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5657
try:
    EcsOnValidate = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsOnValidate")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5658
try:
    EcsPostUpdate = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsPostUpdate")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5659
try:
    EcsPreStore = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsPreStore")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5660
try:
    EcsOnStore = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsOnStore")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5661
try:
    EcsPostFrame = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsPostFrame")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5662
try:
    EcsPhase = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsPhase")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5664
try:
    EcsConstant = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsConstant")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5706
if _libs["libflecs.dylib"].has("ecs_init", "cdecl"):
    ecs_init = _libs["libflecs.dylib"].get("ecs_init", "cdecl")
    ecs_init.argtypes = []
    ecs_init.restype = POINTER(ecs_world_t)

# /Users/cnifi/git/pyflecs/flecs.h: 5715
if _libs["libflecs.dylib"].has("ecs_mini", "cdecl"):
    ecs_mini = _libs["libflecs.dylib"].get("ecs_mini", "cdecl")
    ecs_mini.argtypes = []
    ecs_mini.restype = POINTER(ecs_world_t)

# /Users/cnifi/git/pyflecs/flecs.h: 5725
if _libs["libflecs.dylib"].has("ecs_init_w_args", "cdecl"):
    ecs_init_w_args = _libs["libflecs.dylib"].get("ecs_init_w_args", "cdecl")
    ecs_init_w_args.argtypes = [c_int, POINTER(POINTER(c_char))]
    ecs_init_w_args.restype = POINTER(ecs_world_t)

# /Users/cnifi/git/pyflecs/flecs.h: 5736
if _libs["libflecs.dylib"].has("ecs_fini", "cdecl"):
    ecs_fini = _libs["libflecs.dylib"].get("ecs_fini", "cdecl")
    ecs_fini.argtypes = [POINTER(ecs_world_t)]
    ecs_fini.restype = c_int

# /Users/cnifi/git/pyflecs/flecs.h: 5747
if _libs["libflecs.dylib"].has("ecs_is_fini", "cdecl"):
    ecs_is_fini = _libs["libflecs.dylib"].get("ecs_is_fini", "cdecl")
    ecs_is_fini.argtypes = [POINTER(ecs_world_t)]
    ecs_is_fini.restype = c_bool

# /Users/cnifi/git/pyflecs/flecs.h: 5758
if _libs["libflecs.dylib"].has("ecs_atfini", "cdecl"):
    ecs_atfini = _libs["libflecs.dylib"].get("ecs_atfini", "cdecl")
    ecs_atfini.argtypes = [POINTER(ecs_world_t), ecs_fini_action_t, POINTER(None)]
    ecs_atfini.restype = None


# /Users/cnifi/git/pyflecs/flecs.h: 5768
class struct_ecs_entities_t(Structure):
    pass


struct_ecs_entities_t.__slots__ = [
    "ids",
    "count",
    "alive_count",
]
struct_ecs_entities_t._fields_ = [
    ("ids", POINTER(ecs_entity_t)),
    ("count", c_int32),
    ("alive_count", c_int32),
]

ecs_entities_t = struct_ecs_entities_t  # /Users/cnifi/git/pyflecs/flecs.h: 5768

# /Users/cnifi/git/pyflecs/flecs.h: 5797
if _libs["libflecs.dylib"].has("ecs_get_entities", "cdecl"):
    ecs_get_entities = _libs["libflecs.dylib"].get("ecs_get_entities", "cdecl")
    ecs_get_entities.argtypes = [POINTER(ecs_world_t)]
    ecs_get_entities.restype = ecs_entities_t

# /Users/cnifi/git/pyflecs/flecs.h: 5808
if _libs["libflecs.dylib"].has("ecs_world_get_flags", "cdecl"):
    ecs_world_get_flags = _libs["libflecs.dylib"].get("ecs_world_get_flags", "cdecl")
    ecs_world_get_flags.argtypes = [POINTER(ecs_world_t)]
    ecs_world_get_flags.restype = ecs_flags32_t

# /Users/cnifi/git/pyflecs/flecs.h: 5837
if _libs["libflecs.dylib"].has("ecs_frame_begin", "cdecl"):
    ecs_frame_begin = _libs["libflecs.dylib"].get("ecs_frame_begin", "cdecl")
    ecs_frame_begin.argtypes = [POINTER(ecs_world_t), c_float]
    ecs_frame_begin.restype = c_float

# /Users/cnifi/git/pyflecs/flecs.h: 5848
if _libs["libflecs.dylib"].has("ecs_frame_end", "cdecl"):
    ecs_frame_end = _libs["libflecs.dylib"].get("ecs_frame_end", "cdecl")
    ecs_frame_end.argtypes = [POINTER(ecs_world_t)]
    ecs_frame_end.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 5859
if _libs["libflecs.dylib"].has("ecs_run_post_frame", "cdecl"):
    ecs_run_post_frame = _libs["libflecs.dylib"].get("ecs_run_post_frame", "cdecl")
    ecs_run_post_frame.argtypes = [
        POINTER(ecs_world_t),
        ecs_fini_action_t,
        POINTER(None),
    ]
    ecs_run_post_frame.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 5871
if _libs["libflecs.dylib"].has("ecs_quit", "cdecl"):
    ecs_quit = _libs["libflecs.dylib"].get("ecs_quit", "cdecl")
    ecs_quit.argtypes = [POINTER(ecs_world_t)]
    ecs_quit.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 5881
if _libs["libflecs.dylib"].has("ecs_should_quit", "cdecl"):
    ecs_should_quit = _libs["libflecs.dylib"].get("ecs_should_quit", "cdecl")
    ecs_should_quit.argtypes = [POINTER(ecs_world_t)]
    ecs_should_quit.restype = c_bool

# /Users/cnifi/git/pyflecs/flecs.h: 5895
if _libs["libflecs.dylib"].has("ecs_measure_frame_time", "cdecl"):
    ecs_measure_frame_time = _libs["libflecs.dylib"].get(
        "ecs_measure_frame_time", "cdecl"
    )
    ecs_measure_frame_time.argtypes = [POINTER(ecs_world_t), c_bool]
    ecs_measure_frame_time.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 5909
if _libs["libflecs.dylib"].has("ecs_measure_system_time", "cdecl"):
    ecs_measure_system_time = _libs["libflecs.dylib"].get(
        "ecs_measure_system_time", "cdecl"
    )
    ecs_measure_system_time.argtypes = [POINTER(ecs_world_t), c_bool]
    ecs_measure_system_time.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 5929
if _libs["libflecs.dylib"].has("ecs_set_target_fps", "cdecl"):
    ecs_set_target_fps = _libs["libflecs.dylib"].get("ecs_set_target_fps", "cdecl")
    ecs_set_target_fps.argtypes = [POINTER(ecs_world_t), c_float]
    ecs_set_target_fps.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 5947
if _libs["libflecs.dylib"].has("ecs_set_default_query_flags", "cdecl"):
    ecs_set_default_query_flags = _libs["libflecs.dylib"].get(
        "ecs_set_default_query_flags", "cdecl"
    )
    ecs_set_default_query_flags.argtypes = [POINTER(ecs_world_t), ecs_flags32_t]
    ecs_set_default_query_flags.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 6028
if _libs["libflecs.dylib"].has("ecs_readonly_begin", "cdecl"):
    ecs_readonly_begin = _libs["libflecs.dylib"].get("ecs_readonly_begin", "cdecl")
    ecs_readonly_begin.argtypes = [POINTER(ecs_world_t), c_bool]
    ecs_readonly_begin.restype = c_bool

# /Users/cnifi/git/pyflecs/flecs.h: 6040
if _libs["libflecs.dylib"].has("ecs_readonly_end", "cdecl"):
    ecs_readonly_end = _libs["libflecs.dylib"].get("ecs_readonly_end", "cdecl")
    ecs_readonly_end.argtypes = [POINTER(ecs_world_t)]
    ecs_readonly_end.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 6054
if _libs["libflecs.dylib"].has("ecs_merge", "cdecl"):
    ecs_merge = _libs["libflecs.dylib"].get("ecs_merge", "cdecl")
    ecs_merge.argtypes = [POINTER(ecs_world_t)]
    ecs_merge.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 6073
if _libs["libflecs.dylib"].has("ecs_defer_begin", "cdecl"):
    ecs_defer_begin = _libs["libflecs.dylib"].get("ecs_defer_begin", "cdecl")
    ecs_defer_begin.argtypes = [POINTER(ecs_world_t)]
    ecs_defer_begin.restype = c_bool

# /Users/cnifi/git/pyflecs/flecs.h: 6087
if _libs["libflecs.dylib"].has("ecs_is_deferred", "cdecl"):
    ecs_is_deferred = _libs["libflecs.dylib"].get("ecs_is_deferred", "cdecl")
    ecs_is_deferred.argtypes = [POINTER(ecs_world_t)]
    ecs_is_deferred.restype = c_bool

# /Users/cnifi/git/pyflecs/flecs.h: 6104
if _libs["libflecs.dylib"].has("ecs_defer_end", "cdecl"):
    ecs_defer_end = _libs["libflecs.dylib"].get("ecs_defer_end", "cdecl")
    ecs_defer_end.argtypes = [POINTER(ecs_world_t)]
    ecs_defer_end.restype = c_bool

# /Users/cnifi/git/pyflecs/flecs.h: 6122
if _libs["libflecs.dylib"].has("ecs_defer_suspend", "cdecl"):
    ecs_defer_suspend = _libs["libflecs.dylib"].get("ecs_defer_suspend", "cdecl")
    ecs_defer_suspend.argtypes = [POINTER(ecs_world_t)]
    ecs_defer_suspend.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 6136
if _libs["libflecs.dylib"].has("ecs_defer_resume", "cdecl"):
    ecs_defer_resume = _libs["libflecs.dylib"].get("ecs_defer_resume", "cdecl")
    ecs_defer_resume.argtypes = [POINTER(ecs_world_t)]
    ecs_defer_resume.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 6153
if _libs["libflecs.dylib"].has("ecs_set_stage_count", "cdecl"):
    ecs_set_stage_count = _libs["libflecs.dylib"].get("ecs_set_stage_count", "cdecl")
    ecs_set_stage_count.argtypes = [POINTER(ecs_world_t), c_int32]
    ecs_set_stage_count.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 6164
if _libs["libflecs.dylib"].has("ecs_get_stage_count", "cdecl"):
    ecs_get_stage_count = _libs["libflecs.dylib"].get("ecs_get_stage_count", "cdecl")
    ecs_get_stage_count.argtypes = [POINTER(ecs_world_t)]
    ecs_get_stage_count.restype = c_int32

# /Users/cnifi/git/pyflecs/flecs.h: 6183
if _libs["libflecs.dylib"].has("ecs_get_stage", "cdecl"):
    ecs_get_stage = _libs["libflecs.dylib"].get("ecs_get_stage", "cdecl")
    ecs_get_stage.argtypes = [POINTER(ecs_world_t), c_int32]
    ecs_get_stage.restype = POINTER(ecs_world_t)

# /Users/cnifi/git/pyflecs/flecs.h: 6195
if _libs["libflecs.dylib"].has("ecs_stage_is_readonly", "cdecl"):
    ecs_stage_is_readonly = _libs["libflecs.dylib"].get(
        "ecs_stage_is_readonly", "cdecl"
    )
    ecs_stage_is_readonly.argtypes = [POINTER(ecs_world_t)]
    ecs_stage_is_readonly.restype = c_bool

# /Users/cnifi/git/pyflecs/flecs.h: 6206
if _libs["libflecs.dylib"].has("ecs_stage_new", "cdecl"):
    ecs_stage_new = _libs["libflecs.dylib"].get("ecs_stage_new", "cdecl")
    ecs_stage_new.argtypes = [POINTER(ecs_world_t)]
    ecs_stage_new.restype = POINTER(ecs_world_t)

# /Users/cnifi/git/pyflecs/flecs.h: 6214
if _libs["libflecs.dylib"].has("ecs_stage_free", "cdecl"):
    ecs_stage_free = _libs["libflecs.dylib"].get("ecs_stage_free", "cdecl")
    ecs_stage_free.argtypes = [POINTER(ecs_world_t)]
    ecs_stage_free.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 6225
if _libs["libflecs.dylib"].has("ecs_stage_get_id", "cdecl"):
    ecs_stage_get_id = _libs["libflecs.dylib"].get("ecs_stage_get_id", "cdecl")
    ecs_stage_get_id.argtypes = [POINTER(ecs_world_t)]
    ecs_stage_get_id.restype = c_int32

# /Users/cnifi/git/pyflecs/flecs.h: 6244
if _libs["libflecs.dylib"].has("ecs_set_ctx", "cdecl"):
    ecs_set_ctx = _libs["libflecs.dylib"].get("ecs_set_ctx", "cdecl")
    ecs_set_ctx.argtypes = [POINTER(ecs_world_t), POINTER(None), ecs_ctx_free_t]
    ecs_set_ctx.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 6258
if _libs["libflecs.dylib"].has("ecs_set_binding_ctx", "cdecl"):
    ecs_set_binding_ctx = _libs["libflecs.dylib"].get("ecs_set_binding_ctx", "cdecl")
    ecs_set_binding_ctx.argtypes = [POINTER(ecs_world_t), POINTER(None), ecs_ctx_free_t]
    ecs_set_binding_ctx.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 6271
if _libs["libflecs.dylib"].has("ecs_get_ctx", "cdecl"):
    ecs_get_ctx = _libs["libflecs.dylib"].get("ecs_get_ctx", "cdecl")
    ecs_get_ctx.argtypes = [POINTER(ecs_world_t)]
    ecs_get_ctx.restype = POINTER(c_ubyte)
    ecs_get_ctx.errcheck = lambda v, *a: cast(v, c_void_p)

# /Users/cnifi/git/pyflecs/flecs.h: 6282
if _libs["libflecs.dylib"].has("ecs_get_binding_ctx", "cdecl"):
    ecs_get_binding_ctx = _libs["libflecs.dylib"].get("ecs_get_binding_ctx", "cdecl")
    ecs_get_binding_ctx.argtypes = [POINTER(ecs_world_t)]
    ecs_get_binding_ctx.restype = POINTER(c_ubyte)
    ecs_get_binding_ctx.errcheck = lambda v, *a: cast(v, c_void_p)

# /Users/cnifi/git/pyflecs/flecs.h: 6291
if _libs["libflecs.dylib"].has("ecs_get_build_info", "cdecl"):
    ecs_get_build_info = _libs["libflecs.dylib"].get("ecs_get_build_info", "cdecl")
    ecs_get_build_info.argtypes = []
    ecs_get_build_info.restype = POINTER(ecs_build_info_t)

# /Users/cnifi/git/pyflecs/flecs.h: 6299
if _libs["libflecs.dylib"].has("ecs_get_world_info", "cdecl"):
    ecs_get_world_info = _libs["libflecs.dylib"].get("ecs_get_world_info", "cdecl")
    ecs_get_world_info.argtypes = [POINTER(ecs_world_t)]
    ecs_get_world_info.restype = POINTER(ecs_world_info_t)

# /Users/cnifi/git/pyflecs/flecs.h: 6311
if _libs["libflecs.dylib"].has("ecs_dim", "cdecl"):
    ecs_dim = _libs["libflecs.dylib"].get("ecs_dim", "cdecl")
    ecs_dim.argtypes = [POINTER(ecs_world_t), c_int32]
    ecs_dim.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 6330
if _libs["libflecs.dylib"].has("ecs_shrink", "cdecl"):
    ecs_shrink = _libs["libflecs.dylib"].get("ecs_shrink", "cdecl")
    ecs_shrink.argtypes = [POINTER(ecs_world_t)]
    ecs_shrink.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 6349
if _libs["libflecs.dylib"].has("ecs_set_entity_range", "cdecl"):
    ecs_set_entity_range = _libs["libflecs.dylib"].get("ecs_set_entity_range", "cdecl")
    ecs_set_entity_range.argtypes = [POINTER(ecs_world_t), ecs_entity_t, ecs_entity_t]
    ecs_set_entity_range.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 6365
if _libs["libflecs.dylib"].has("ecs_enable_range_check", "cdecl"):
    ecs_enable_range_check = _libs["libflecs.dylib"].get(
        "ecs_enable_range_check", "cdecl"
    )
    ecs_enable_range_check.argtypes = [POINTER(ecs_world_t), c_bool]
    ecs_enable_range_check.restype = c_bool

# /Users/cnifi/git/pyflecs/flecs.h: 6375
if _libs["libflecs.dylib"].has("ecs_get_max_id", "cdecl"):
    ecs_get_max_id = _libs["libflecs.dylib"].get("ecs_get_max_id", "cdecl")
    ecs_get_max_id.argtypes = [POINTER(ecs_world_t)]
    ecs_get_max_id.restype = ecs_entity_t

# /Users/cnifi/git/pyflecs/flecs.h: 6392
if _libs["libflecs.dylib"].has("ecs_run_aperiodic", "cdecl"):
    ecs_run_aperiodic = _libs["libflecs.dylib"].get("ecs_run_aperiodic", "cdecl")
    ecs_run_aperiodic.argtypes = [POINTER(ecs_world_t), ecs_flags32_t]
    ecs_run_aperiodic.restype = None


# /Users/cnifi/git/pyflecs/flecs.h: 6406
class struct_ecs_delete_empty_tables_desc_t(Structure):
    pass


struct_ecs_delete_empty_tables_desc_t.__slots__ = [
    "clear_generation",
    "delete_generation",
    "time_budget_seconds",
]
struct_ecs_delete_empty_tables_desc_t._fields_ = [
    ("clear_generation", uint16_t),
    ("delete_generation", uint16_t),
    ("time_budget_seconds", c_double),
]

ecs_delete_empty_tables_desc_t = (
    struct_ecs_delete_empty_tables_desc_t  # /Users/cnifi/git/pyflecs/flecs.h: 6406
)

# /Users/cnifi/git/pyflecs/flecs.h: 6438
if _libs["libflecs.dylib"].has("ecs_delete_empty_tables", "cdecl"):
    ecs_delete_empty_tables = _libs["libflecs.dylib"].get(
        "ecs_delete_empty_tables", "cdecl"
    )
    ecs_delete_empty_tables.argtypes = [
        POINTER(ecs_world_t),
        POINTER(ecs_delete_empty_tables_desc_t),
    ]
    ecs_delete_empty_tables.restype = c_int32

# /Users/cnifi/git/pyflecs/flecs.h: 6448
if _libs["libflecs.dylib"].has("ecs_get_world", "cdecl"):
    ecs_get_world = _libs["libflecs.dylib"].get("ecs_get_world", "cdecl")
    ecs_get_world.argtypes = [POINTER(ecs_poly_t)]
    ecs_get_world.restype = POINTER(ecs_world_t)

# /Users/cnifi/git/pyflecs/flecs.h: 6457
if _libs["libflecs.dylib"].has("ecs_get_entity", "cdecl"):
    ecs_get_entity = _libs["libflecs.dylib"].get("ecs_get_entity", "cdecl")
    ecs_get_entity.argtypes = [POINTER(ecs_poly_t)]
    ecs_get_entity.restype = ecs_entity_t

# /Users/cnifi/git/pyflecs/flecs.h: 6474
if _libs["libflecs.dylib"].has("flecs_poly_is_", "cdecl"):
    flecs_poly_is_ = _libs["libflecs.dylib"].get("flecs_poly_is_", "cdecl")
    flecs_poly_is_.argtypes = [POINTER(ecs_poly_t), c_int32]
    flecs_poly_is_.restype = c_bool

# /Users/cnifi/git/pyflecs/flecs.h: 6493
if _libs["libflecs.dylib"].has("ecs_make_pair", "cdecl"):
    ecs_make_pair = _libs["libflecs.dylib"].get("ecs_make_pair", "cdecl")
    ecs_make_pair.argtypes = [ecs_entity_t, ecs_entity_t]
    ecs_make_pair.restype = ecs_id_t

# /Users/cnifi/git/pyflecs/flecs.h: 6520
if _libs["libflecs.dylib"].has("ecs_exclusive_access_begin", "cdecl"):
    ecs_exclusive_access_begin = _libs["libflecs.dylib"].get(
        "ecs_exclusive_access_begin", "cdecl"
    )
    ecs_exclusive_access_begin.argtypes = [POINTER(ecs_world_t), String]
    ecs_exclusive_access_begin.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 6547
if _libs["libflecs.dylib"].has("ecs_exclusive_access_end", "cdecl"):
    ecs_exclusive_access_end = _libs["libflecs.dylib"].get(
        "ecs_exclusive_access_end", "cdecl"
    )
    ecs_exclusive_access_end.argtypes = [POINTER(ecs_world_t), c_bool]
    ecs_exclusive_access_end.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 6578
if _libs["libflecs.dylib"].has("ecs_new", "cdecl"):
    ecs_new = _libs["libflecs.dylib"].get("ecs_new", "cdecl")
    ecs_new.argtypes = [POINTER(ecs_world_t)]
    ecs_new.restype = ecs_entity_t

# /Users/cnifi/git/pyflecs/flecs.h: 6599
if _libs["libflecs.dylib"].has("ecs_new_low_id", "cdecl"):
    ecs_new_low_id = _libs["libflecs.dylib"].get("ecs_new_low_id", "cdecl")
    ecs_new_low_id.argtypes = [POINTER(ecs_world_t)]
    ecs_new_low_id.restype = ecs_entity_t

# /Users/cnifi/git/pyflecs/flecs.h: 6611
if _libs["libflecs.dylib"].has("ecs_new_w_id", "cdecl"):
    ecs_new_w_id = _libs["libflecs.dylib"].get("ecs_new_w_id", "cdecl")
    ecs_new_w_id.argtypes = [POINTER(ecs_world_t), ecs_id_t]
    ecs_new_w_id.restype = ecs_entity_t

# /Users/cnifi/git/pyflecs/flecs.h: 6623
if _libs["libflecs.dylib"].has("ecs_new_w_table", "cdecl"):
    ecs_new_w_table = _libs["libflecs.dylib"].get("ecs_new_w_table", "cdecl")
    ecs_new_w_table.argtypes = [POINTER(ecs_world_t), POINTER(ecs_table_t)]
    ecs_new_w_table.restype = ecs_entity_t

# /Users/cnifi/git/pyflecs/flecs.h: 6646
if _libs["libflecs.dylib"].has("ecs_entity_init", "cdecl"):
    ecs_entity_init = _libs["libflecs.dylib"].get("ecs_entity_init", "cdecl")
    ecs_entity_init.argtypes = [POINTER(ecs_world_t), POINTER(ecs_entity_desc_t)]
    ecs_entity_init.restype = ecs_entity_t

# /Users/cnifi/git/pyflecs/flecs.h: 6677
if _libs["libflecs.dylib"].has("ecs_bulk_init", "cdecl"):
    ecs_bulk_init = _libs["libflecs.dylib"].get("ecs_bulk_init", "cdecl")
    ecs_bulk_init.argtypes = [POINTER(ecs_world_t), POINTER(ecs_bulk_desc_t)]
    ecs_bulk_init.restype = POINTER(ecs_entity_t)

# /Users/cnifi/git/pyflecs/flecs.h: 6691
if _libs["libflecs.dylib"].has("ecs_bulk_new_w_id", "cdecl"):
    ecs_bulk_new_w_id = _libs["libflecs.dylib"].get("ecs_bulk_new_w_id", "cdecl")
    ecs_bulk_new_w_id.argtypes = [POINTER(ecs_world_t), ecs_id_t, c_int32]
    ecs_bulk_new_w_id.restype = POINTER(ecs_entity_t)

# /Users/cnifi/git/pyflecs/flecs.h: 6712
if _libs["libflecs.dylib"].has("ecs_clone", "cdecl"):
    ecs_clone = _libs["libflecs.dylib"].get("ecs_clone", "cdecl")
    ecs_clone.argtypes = [POINTER(ecs_world_t), ecs_entity_t, ecs_entity_t, c_bool]
    ecs_clone.restype = ecs_entity_t

# /Users/cnifi/git/pyflecs/flecs.h: 6727
if _libs["libflecs.dylib"].has("ecs_delete", "cdecl"):
    ecs_delete = _libs["libflecs.dylib"].get("ecs_delete", "cdecl")
    ecs_delete.argtypes = [POINTER(ecs_world_t), ecs_entity_t]
    ecs_delete.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 6739
if _libs["libflecs.dylib"].has("ecs_delete_with", "cdecl"):
    ecs_delete_with = _libs["libflecs.dylib"].get("ecs_delete_with", "cdecl")
    ecs_delete_with.argtypes = [POINTER(ecs_world_t), ecs_id_t]
    ecs_delete_with.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 6761
if _libs["libflecs.dylib"].has("ecs_set_child_order", "cdecl"):
    ecs_set_child_order = _libs["libflecs.dylib"].get("ecs_set_child_order", "cdecl")
    ecs_set_child_order.argtypes = [
        POINTER(ecs_world_t),
        ecs_entity_t,
        POINTER(ecs_entity_t),
        c_int32,
    ]
    ecs_set_child_order.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 6777
if _libs["libflecs.dylib"].has("ecs_get_ordered_children", "cdecl"):
    ecs_get_ordered_children = _libs["libflecs.dylib"].get(
        "ecs_get_ordered_children", "cdecl"
    )
    ecs_get_ordered_children.argtypes = [POINTER(ecs_world_t), ecs_entity_t]
    ecs_get_ordered_children.restype = ecs_entities_t

# /Users/cnifi/git/pyflecs/flecs.h: 6799
if _libs["libflecs.dylib"].has("ecs_add_id", "cdecl"):
    ecs_add_id = _libs["libflecs.dylib"].get("ecs_add_id", "cdecl")
    ecs_add_id.argtypes = [POINTER(ecs_world_t), ecs_entity_t, ecs_id_t]
    ecs_add_id.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 6813
if _libs["libflecs.dylib"].has("ecs_remove_id", "cdecl"):
    ecs_remove_id = _libs["libflecs.dylib"].get("ecs_remove_id", "cdecl")
    ecs_remove_id.argtypes = [POINTER(ecs_world_t), ecs_entity_t, ecs_id_t]
    ecs_remove_id.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 6870
if _libs["libflecs.dylib"].has("ecs_auto_override_id", "cdecl"):
    ecs_auto_override_id = _libs["libflecs.dylib"].get("ecs_auto_override_id", "cdecl")
    ecs_auto_override_id.argtypes = [POINTER(ecs_world_t), ecs_entity_t, ecs_id_t]
    ecs_auto_override_id.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 6882
if _libs["libflecs.dylib"].has("ecs_clear", "cdecl"):
    ecs_clear = _libs["libflecs.dylib"].get("ecs_clear", "cdecl")
    ecs_clear.argtypes = [POINTER(ecs_world_t), ecs_entity_t]
    ecs_clear.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 6894
if _libs["libflecs.dylib"].has("ecs_remove_all", "cdecl"):
    ecs_remove_all = _libs["libflecs.dylib"].get("ecs_remove_all", "cdecl")
    ecs_remove_all.argtypes = [POINTER(ecs_world_t), ecs_id_t]
    ecs_remove_all.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 6906
if _libs["libflecs.dylib"].has("ecs_set_with", "cdecl"):
    ecs_set_with = _libs["libflecs.dylib"].get("ecs_set_with", "cdecl")
    ecs_set_with.argtypes = [POINTER(ecs_world_t), ecs_id_t]
    ecs_set_with.restype = ecs_entity_t

# /Users/cnifi/git/pyflecs/flecs.h: 6917
if _libs["libflecs.dylib"].has("ecs_get_with", "cdecl"):
    ecs_get_with = _libs["libflecs.dylib"].get("ecs_get_with", "cdecl")
    ecs_get_with.argtypes = [POINTER(ecs_world_t)]
    ecs_get_with.restype = ecs_id_t

# /Users/cnifi/git/pyflecs/flecs.h: 6939
if _libs["libflecs.dylib"].has("ecs_enable", "cdecl"):
    ecs_enable = _libs["libflecs.dylib"].get("ecs_enable", "cdecl")
    ecs_enable.argtypes = [POINTER(ecs_world_t), ecs_entity_t, c_bool]
    ecs_enable.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 6958
if _libs["libflecs.dylib"].has("ecs_enable_id", "cdecl"):
    ecs_enable_id = _libs["libflecs.dylib"].get("ecs_enable_id", "cdecl")
    ecs_enable_id.argtypes = [POINTER(ecs_world_t), ecs_entity_t, ecs_id_t, c_bool]
    ecs_enable_id.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 6975
if _libs["libflecs.dylib"].has("ecs_is_enabled_id", "cdecl"):
    ecs_is_enabled_id = _libs["libflecs.dylib"].get("ecs_is_enabled_id", "cdecl")
    ecs_is_enabled_id.argtypes = [POINTER(ecs_world_t), ecs_entity_t, ecs_id_t]
    ecs_is_enabled_id.restype = c_bool

# /Users/cnifi/git/pyflecs/flecs.h: 7004
if _libs["libflecs.dylib"].has("ecs_get_id", "cdecl"):
    ecs_get_id = _libs["libflecs.dylib"].get("ecs_get_id", "cdecl")
    ecs_get_id.argtypes = [POINTER(ecs_world_t), ecs_entity_t, ecs_id_t]
    ecs_get_id.restype = POINTER(c_ubyte)
    ecs_get_id.errcheck = lambda v, *a: cast(v, c_void_p)

# /Users/cnifi/git/pyflecs/flecs.h: 7021
if _libs["libflecs.dylib"].has("ecs_get_mut_id", "cdecl"):
    ecs_get_mut_id = _libs["libflecs.dylib"].get("ecs_get_mut_id", "cdecl")
    ecs_get_mut_id.argtypes = [POINTER(ecs_world_t), ecs_entity_t, ecs_id_t]
    ecs_get_mut_id.restype = POINTER(c_ubyte)
    ecs_get_mut_id.errcheck = lambda v, *a: cast(v, c_void_p)

# /Users/cnifi/git/pyflecs/flecs.h: 7043
if _libs["libflecs.dylib"].has("ecs_ensure_id", "cdecl"):
    ecs_ensure_id = _libs["libflecs.dylib"].get("ecs_ensure_id", "cdecl")
    ecs_ensure_id.argtypes = [POINTER(ecs_world_t), ecs_entity_t, ecs_id_t, c_size_t]
    ecs_ensure_id.restype = POINTER(c_ubyte)
    ecs_ensure_id.errcheck = lambda v, *a: cast(v, c_void_p)

# /Users/cnifi/git/pyflecs/flecs.h: 7060
if _libs["libflecs.dylib"].has("ecs_ref_init_id", "cdecl"):
    ecs_ref_init_id = _libs["libflecs.dylib"].get("ecs_ref_init_id", "cdecl")
    ecs_ref_init_id.argtypes = [POINTER(ecs_world_t), ecs_entity_t, ecs_id_t]
    ecs_ref_init_id.restype = ecs_ref_t

# /Users/cnifi/git/pyflecs/flecs.h: 7074
if _libs["libflecs.dylib"].has("ecs_ref_get_id", "cdecl"):
    ecs_ref_get_id = _libs["libflecs.dylib"].get("ecs_ref_get_id", "cdecl")
    ecs_ref_get_id.argtypes = [POINTER(ecs_world_t), POINTER(ecs_ref_t), ecs_id_t]
    ecs_ref_get_id.restype = POINTER(c_ubyte)
    ecs_ref_get_id.errcheck = lambda v, *a: cast(v, c_void_p)

# /Users/cnifi/git/pyflecs/flecs.h: 7087
if _libs["libflecs.dylib"].has("ecs_ref_update", "cdecl"):
    ecs_ref_update = _libs["libflecs.dylib"].get("ecs_ref_update", "cdecl")
    ecs_ref_update.argtypes = [POINTER(ecs_world_t), POINTER(ecs_ref_t)]
    ecs_ref_update.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 7111
if _libs["libflecs.dylib"].has("ecs_emplace_id", "cdecl"):
    ecs_emplace_id = _libs["libflecs.dylib"].get("ecs_emplace_id", "cdecl")
    ecs_emplace_id.argtypes = [
        POINTER(ecs_world_t),
        ecs_entity_t,
        ecs_id_t,
        POINTER(c_bool),
    ]
    ecs_emplace_id.restype = POINTER(c_ubyte)
    ecs_emplace_id.errcheck = lambda v, *a: cast(v, c_void_p)

# /Users/cnifi/git/pyflecs/flecs.h: 7127
if _libs["libflecs.dylib"].has("ecs_modified_id", "cdecl"):
    ecs_modified_id = _libs["libflecs.dylib"].get("ecs_modified_id", "cdecl")
    ecs_modified_id.argtypes = [POINTER(ecs_world_t), ecs_entity_t, ecs_id_t]
    ecs_modified_id.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 7148
if _libs["libflecs.dylib"].has("ecs_set_id", "cdecl"):
    ecs_set_id = _libs["libflecs.dylib"].get("ecs_set_id", "cdecl")
    ecs_set_id.argtypes = [
        POINTER(ecs_world_t),
        ecs_entity_t,
        ecs_id_t,
        c_size_t,
        POINTER(None),
    ]
    ecs_set_id.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 7183
if _libs["libflecs.dylib"].has("ecs_is_valid", "cdecl"):
    ecs_is_valid = _libs["libflecs.dylib"].get("ecs_is_valid", "cdecl")
    ecs_is_valid.argtypes = [POINTER(ecs_world_t), ecs_entity_t]
    ecs_is_valid.restype = c_bool

# /Users/cnifi/git/pyflecs/flecs.h: 7213
if _libs["libflecs.dylib"].has("ecs_is_alive", "cdecl"):
    ecs_is_alive = _libs["libflecs.dylib"].get("ecs_is_alive", "cdecl")
    ecs_is_alive.argtypes = [POINTER(ecs_world_t), ecs_entity_t]
    ecs_is_alive.restype = c_bool

# /Users/cnifi/git/pyflecs/flecs.h: 7223
if _libs["libflecs.dylib"].has("ecs_strip_generation", "cdecl"):
    ecs_strip_generation = _libs["libflecs.dylib"].get("ecs_strip_generation", "cdecl")
    ecs_strip_generation.argtypes = [ecs_entity_t]
    ecs_strip_generation.restype = ecs_id_t

# /Users/cnifi/git/pyflecs/flecs.h: 7243
if _libs["libflecs.dylib"].has("ecs_get_alive", "cdecl"):
    ecs_get_alive = _libs["libflecs.dylib"].get("ecs_get_alive", "cdecl")
    ecs_get_alive.argtypes = [POINTER(ecs_world_t), ecs_entity_t]
    ecs_get_alive.restype = ecs_entity_t

# /Users/cnifi/git/pyflecs/flecs.h: 7270
if _libs["libflecs.dylib"].has("ecs_make_alive", "cdecl"):
    ecs_make_alive = _libs["libflecs.dylib"].get("ecs_make_alive", "cdecl")
    ecs_make_alive.argtypes = [POINTER(ecs_world_t), ecs_entity_t]
    ecs_make_alive.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 7293
if _libs["libflecs.dylib"].has("ecs_make_alive_id", "cdecl"):
    ecs_make_alive_id = _libs["libflecs.dylib"].get("ecs_make_alive_id", "cdecl")
    ecs_make_alive_id.argtypes = [POINTER(ecs_world_t), ecs_id_t]
    ecs_make_alive_id.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 7305
if _libs["libflecs.dylib"].has("ecs_exists", "cdecl"):
    ecs_exists = _libs["libflecs.dylib"].get("ecs_exists", "cdecl")
    ecs_exists.argtypes = [POINTER(ecs_world_t), ecs_entity_t]
    ecs_exists.restype = c_bool

# /Users/cnifi/git/pyflecs/flecs.h: 7324
if _libs["libflecs.dylib"].has("ecs_set_version", "cdecl"):
    ecs_set_version = _libs["libflecs.dylib"].get("ecs_set_version", "cdecl")
    ecs_set_version.argtypes = [POINTER(ecs_world_t), ecs_entity_t]
    ecs_set_version.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 7344
if _libs["libflecs.dylib"].has("ecs_get_type", "cdecl"):
    ecs_get_type = _libs["libflecs.dylib"].get("ecs_get_type", "cdecl")
    ecs_get_type.argtypes = [POINTER(ecs_world_t), ecs_entity_t]
    ecs_get_type.restype = POINTER(ecs_type_t)

# /Users/cnifi/git/pyflecs/flecs.h: 7355
if _libs["libflecs.dylib"].has("ecs_get_table", "cdecl"):
    ecs_get_table = _libs["libflecs.dylib"].get("ecs_get_table", "cdecl")
    ecs_get_table.argtypes = [POINTER(ecs_world_t), ecs_entity_t]
    ecs_get_table.restype = POINTER(ecs_table_t)

# /Users/cnifi/git/pyflecs/flecs.h: 7367
if _libs["libflecs.dylib"].has("ecs_type_str", "cdecl"):
    ecs_type_str = _libs["libflecs.dylib"].get("ecs_type_str", "cdecl")
    ecs_type_str.argtypes = [POINTER(ecs_world_t), POINTER(ecs_type_t)]
    if sizeof(c_int) == sizeof(c_void_p):
        ecs_type_str.restype = ReturnString
    else:
        ecs_type_str.restype = String
        ecs_type_str.errcheck = ReturnString

# /Users/cnifi/git/pyflecs/flecs.h: 7383
if _libs["libflecs.dylib"].has("ecs_table_str", "cdecl"):
    ecs_table_str = _libs["libflecs.dylib"].get("ecs_table_str", "cdecl")
    ecs_table_str.argtypes = [POINTER(ecs_world_t), POINTER(ecs_table_t)]
    if sizeof(c_int) == sizeof(c_void_p):
        ecs_table_str.restype = ReturnString
    else:
        ecs_table_str.restype = String
        ecs_table_str.errcheck = ReturnString

# /Users/cnifi/git/pyflecs/flecs.h: 7402
if _libs["libflecs.dylib"].has("ecs_entity_str", "cdecl"):
    ecs_entity_str = _libs["libflecs.dylib"].get("ecs_entity_str", "cdecl")
    ecs_entity_str.argtypes = [POINTER(ecs_world_t), ecs_entity_t]
    if sizeof(c_int) == sizeof(c_void_p):
        ecs_entity_str.restype = ReturnString
    else:
        ecs_entity_str.restype = String
        ecs_entity_str.errcheck = ReturnString

# /Users/cnifi/git/pyflecs/flecs.h: 7417
if _libs["libflecs.dylib"].has("ecs_has_id", "cdecl"):
    ecs_has_id = _libs["libflecs.dylib"].get("ecs_has_id", "cdecl")
    ecs_has_id.argtypes = [POINTER(ecs_world_t), ecs_entity_t, ecs_id_t]
    ecs_has_id.restype = c_bool

# /Users/cnifi/git/pyflecs/flecs.h: 7433
if _libs["libflecs.dylib"].has("ecs_owns_id", "cdecl"):
    ecs_owns_id = _libs["libflecs.dylib"].get("ecs_owns_id", "cdecl")
    ecs_owns_id.argtypes = [POINTER(ecs_world_t), ecs_entity_t, ecs_id_t]
    ecs_owns_id.restype = c_bool

# /Users/cnifi/git/pyflecs/flecs.h: 7453
if _libs["libflecs.dylib"].has("ecs_get_target", "cdecl"):
    ecs_get_target = _libs["libflecs.dylib"].get("ecs_get_target", "cdecl")
    ecs_get_target.argtypes = [
        POINTER(ecs_world_t),
        ecs_entity_t,
        ecs_entity_t,
        c_int32,
    ]
    ecs_get_target.restype = ecs_entity_t

# /Users/cnifi/git/pyflecs/flecs.h: 7473
if _libs["libflecs.dylib"].has("ecs_get_parent", "cdecl"):
    ecs_get_parent = _libs["libflecs.dylib"].get("ecs_get_parent", "cdecl")
    ecs_get_parent.argtypes = [POINTER(ecs_world_t), ecs_entity_t]
    ecs_get_parent.restype = ecs_entity_t

# /Users/cnifi/git/pyflecs/flecs.h: 7498
if _libs["libflecs.dylib"].has("ecs_get_target_for_id", "cdecl"):
    ecs_get_target_for_id = _libs["libflecs.dylib"].get(
        "ecs_get_target_for_id", "cdecl"
    )
    ecs_get_target_for_id.argtypes = [
        POINTER(ecs_world_t),
        ecs_entity_t,
        ecs_entity_t,
        ecs_id_t,
    ]
    ecs_get_target_for_id.restype = ecs_entity_t

# /Users/cnifi/git/pyflecs/flecs.h: 7515
if _libs["libflecs.dylib"].has("ecs_get_depth", "cdecl"):
    ecs_get_depth = _libs["libflecs.dylib"].get("ecs_get_depth", "cdecl")
    ecs_get_depth.argtypes = [POINTER(ecs_world_t), ecs_entity_t, ecs_entity_t]
    ecs_get_depth.restype = c_int32

# /Users/cnifi/git/pyflecs/flecs.h: 7528
if _libs["libflecs.dylib"].has("ecs_count_id", "cdecl"):
    ecs_count_id = _libs["libflecs.dylib"].get("ecs_count_id", "cdecl")
    ecs_count_id.argtypes = [POINTER(ecs_world_t), ecs_id_t]
    ecs_count_id.restype = c_int32

# /Users/cnifi/git/pyflecs/flecs.h: 7552
if _libs["libflecs.dylib"].has("ecs_get_name", "cdecl"):
    ecs_get_name = _libs["libflecs.dylib"].get("ecs_get_name", "cdecl")
    ecs_get_name.argtypes = [POINTER(ecs_world_t), ecs_entity_t]
    ecs_get_name.restype = c_char_p

# /Users/cnifi/git/pyflecs/flecs.h: 7566
if _libs["libflecs.dylib"].has("ecs_get_symbol", "cdecl"):
    ecs_get_symbol = _libs["libflecs.dylib"].get("ecs_get_symbol", "cdecl")
    ecs_get_symbol.argtypes = [POINTER(ecs_world_t), ecs_entity_t]
    ecs_get_symbol.restype = c_char_p

# /Users/cnifi/git/pyflecs/flecs.h: 7584
if _libs["libflecs.dylib"].has("ecs_set_name", "cdecl"):
    ecs_set_name = _libs["libflecs.dylib"].get("ecs_set_name", "cdecl")
    ecs_set_name.argtypes = [POINTER(ecs_world_t), ecs_entity_t, String]
    ecs_set_name.restype = ecs_entity_t

# /Users/cnifi/git/pyflecs/flecs.h: 7603
if _libs["libflecs.dylib"].has("ecs_set_symbol", "cdecl"):
    ecs_set_symbol = _libs["libflecs.dylib"].get("ecs_set_symbol", "cdecl")
    ecs_set_symbol.argtypes = [POINTER(ecs_world_t), ecs_entity_t, String]
    ecs_set_symbol.restype = ecs_entity_t

# /Users/cnifi/git/pyflecs/flecs.h: 7620
if _libs["libflecs.dylib"].has("ecs_set_alias", "cdecl"):
    ecs_set_alias = _libs["libflecs.dylib"].get("ecs_set_alias", "cdecl")
    ecs_set_alias.argtypes = [POINTER(ecs_world_t), ecs_entity_t, String]
    ecs_set_alias.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 7641
if _libs["libflecs.dylib"].has("ecs_lookup", "cdecl"):
    ecs_lookup = _libs["libflecs.dylib"].get("ecs_lookup", "cdecl")
    ecs_lookup.argtypes = [POINTER(ecs_world_t), String]
    ecs_lookup.restype = ecs_entity_t

# /Users/cnifi/git/pyflecs/flecs.h: 7660
if _libs["libflecs.dylib"].has("ecs_lookup_child", "cdecl"):
    ecs_lookup_child = _libs["libflecs.dylib"].get("ecs_lookup_child", "cdecl")
    ecs_lookup_child.argtypes = [POINTER(ecs_world_t), ecs_entity_t, String]
    ecs_lookup_child.restype = ecs_entity_t

# /Users/cnifi/git/pyflecs/flecs.h: 7688
if _libs["libflecs.dylib"].has("ecs_lookup_path_w_sep", "cdecl"):
    ecs_lookup_path_w_sep = _libs["libflecs.dylib"].get(
        "ecs_lookup_path_w_sep", "cdecl"
    )
    ecs_lookup_path_w_sep.argtypes = [
        POINTER(ecs_world_t),
        ecs_entity_t,
        String,
        String,
        String,
        c_bool,
    ]
    ecs_lookup_path_w_sep.restype = ecs_entity_t

# /Users/cnifi/git/pyflecs/flecs.h: 7714
if _libs["libflecs.dylib"].has("ecs_lookup_symbol", "cdecl"):
    ecs_lookup_symbol = _libs["libflecs.dylib"].get("ecs_lookup_symbol", "cdecl")
    ecs_lookup_symbol.argtypes = [POINTER(ecs_world_t), String, c_bool, c_bool]
    ecs_lookup_symbol.restype = ecs_entity_t

# /Users/cnifi/git/pyflecs/flecs.h: 7742
if _libs["libflecs.dylib"].has("ecs_get_path_w_sep", "cdecl"):
    ecs_get_path_w_sep = _libs["libflecs.dylib"].get("ecs_get_path_w_sep", "cdecl")
    ecs_get_path_w_sep.argtypes = [
        POINTER(ecs_world_t),
        ecs_entity_t,
        ecs_entity_t,
        String,
        String,
    ]
    if sizeof(c_int) == sizeof(c_void_p):
        ecs_get_path_w_sep.restype = ReturnString
    else:
        ecs_get_path_w_sep.restype = String
        ecs_get_path_w_sep.errcheck = ReturnString

# /Users/cnifi/git/pyflecs/flecs.h: 7762
if _libs["libflecs.dylib"].has("ecs_get_path_w_sep_buf", "cdecl"):
    ecs_get_path_w_sep_buf = _libs["libflecs.dylib"].get(
        "ecs_get_path_w_sep_buf", "cdecl"
    )
    ecs_get_path_w_sep_buf.argtypes = [
        POINTER(ecs_world_t),
        ecs_entity_t,
        ecs_entity_t,
        String,
        String,
        POINTER(ecs_strbuf_t),
        c_bool,
    ]
    ecs_get_path_w_sep_buf.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 7787
if _libs["libflecs.dylib"].has("ecs_new_from_path_w_sep", "cdecl"):
    ecs_new_from_path_w_sep = _libs["libflecs.dylib"].get(
        "ecs_new_from_path_w_sep", "cdecl"
    )
    ecs_new_from_path_w_sep.argtypes = [
        POINTER(ecs_world_t),
        ecs_entity_t,
        String,
        String,
        String,
    ]
    ecs_new_from_path_w_sep.restype = ecs_entity_t

# /Users/cnifi/git/pyflecs/flecs.h: 7809
if _libs["libflecs.dylib"].has("ecs_add_path_w_sep", "cdecl"):
    ecs_add_path_w_sep = _libs["libflecs.dylib"].get("ecs_add_path_w_sep", "cdecl")
    ecs_add_path_w_sep.argtypes = [
        POINTER(ecs_world_t),
        ecs_entity_t,
        ecs_entity_t,
        String,
        String,
        String,
    ]
    ecs_add_path_w_sep.restype = ecs_entity_t

# /Users/cnifi/git/pyflecs/flecs.h: 7831
if _libs["libflecs.dylib"].has("ecs_set_scope", "cdecl"):
    ecs_set_scope = _libs["libflecs.dylib"].get("ecs_set_scope", "cdecl")
    ecs_set_scope.argtypes = [POINTER(ecs_world_t), ecs_entity_t]
    ecs_set_scope.restype = ecs_entity_t

# /Users/cnifi/git/pyflecs/flecs.h: 7843
if _libs["libflecs.dylib"].has("ecs_get_scope", "cdecl"):
    ecs_get_scope = _libs["libflecs.dylib"].get("ecs_get_scope", "cdecl")
    ecs_get_scope.argtypes = [POINTER(ecs_world_t)]
    ecs_get_scope.restype = ecs_entity_t

# /Users/cnifi/git/pyflecs/flecs.h: 7856
if _libs["libflecs.dylib"].has("ecs_set_name_prefix", "cdecl"):
    ecs_set_name_prefix = _libs["libflecs.dylib"].get("ecs_set_name_prefix", "cdecl")
    ecs_set_name_prefix.argtypes = [POINTER(ecs_world_t), String]
    ecs_set_name_prefix.restype = c_char_p

# /Users/cnifi/git/pyflecs/flecs.h: 7887
if _libs["libflecs.dylib"].has("ecs_set_lookup_path", "cdecl"):
    ecs_set_lookup_path = _libs["libflecs.dylib"].get("ecs_set_lookup_path", "cdecl")
    ecs_set_lookup_path.argtypes = [POINTER(ecs_world_t), POINTER(ecs_entity_t)]
    ecs_set_lookup_path.restype = POINTER(ecs_entity_t)

# /Users/cnifi/git/pyflecs/flecs.h: 7898
if _libs["libflecs.dylib"].has("ecs_get_lookup_path", "cdecl"):
    ecs_get_lookup_path = _libs["libflecs.dylib"].get("ecs_get_lookup_path", "cdecl")
    ecs_get_lookup_path.argtypes = [POINTER(ecs_world_t)]
    ecs_get_lookup_path.restype = POINTER(ecs_entity_t)

# /Users/cnifi/git/pyflecs/flecs.h: 7926
if _libs["libflecs.dylib"].has("ecs_component_init", "cdecl"):
    ecs_component_init = _libs["libflecs.dylib"].get("ecs_component_init", "cdecl")
    ecs_component_init.argtypes = [POINTER(ecs_world_t), POINTER(ecs_component_desc_t)]
    ecs_component_init.restype = ecs_entity_t

# /Users/cnifi/git/pyflecs/flecs.h: 7940
if _libs["libflecs.dylib"].has("ecs_get_type_info", "cdecl"):
    ecs_get_type_info = _libs["libflecs.dylib"].get("ecs_get_type_info", "cdecl")
    ecs_get_type_info.argtypes = [POINTER(ecs_world_t), ecs_id_t]
    ecs_get_type_info.restype = POINTER(ecs_type_info_t)

# /Users/cnifi/git/pyflecs/flecs.h: 7956
if _libs["libflecs.dylib"].has("ecs_set_hooks_id", "cdecl"):
    ecs_set_hooks_id = _libs["libflecs.dylib"].get("ecs_set_hooks_id", "cdecl")
    ecs_set_hooks_id.argtypes = [
        POINTER(ecs_world_t),
        ecs_entity_t,
        POINTER(ecs_type_hooks_t),
    ]
    ecs_set_hooks_id.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 7968
if _libs["libflecs.dylib"].has("ecs_get_hooks_id", "cdecl"):
    ecs_get_hooks_id = _libs["libflecs.dylib"].get("ecs_get_hooks_id", "cdecl")
    ecs_get_hooks_id.argtypes = [POINTER(ecs_world_t), ecs_entity_t]
    ecs_get_hooks_id.restype = POINTER(ecs_type_hooks_t)

# /Users/cnifi/git/pyflecs/flecs.h: 7996
if _libs["libflecs.dylib"].has("ecs_id_is_tag", "cdecl"):
    ecs_id_is_tag = _libs["libflecs.dylib"].get("ecs_id_is_tag", "cdecl")
    ecs_id_is_tag.argtypes = [POINTER(ecs_world_t), ecs_id_t]
    ecs_id_is_tag.restype = c_bool

# /Users/cnifi/git/pyflecs/flecs.h: 8009
if _libs["libflecs.dylib"].has("ecs_id_in_use", "cdecl"):
    ecs_id_in_use = _libs["libflecs.dylib"].get("ecs_id_in_use", "cdecl")
    ecs_id_in_use.argtypes = [POINTER(ecs_world_t), ecs_id_t]
    ecs_id_in_use.restype = c_bool

# /Users/cnifi/git/pyflecs/flecs.h: 8033
if _libs["libflecs.dylib"].has("ecs_get_typeid", "cdecl"):
    ecs_get_typeid = _libs["libflecs.dylib"].get("ecs_get_typeid", "cdecl")
    ecs_get_typeid.argtypes = [POINTER(ecs_world_t), ecs_id_t]
    ecs_get_typeid.restype = ecs_entity_t

# /Users/cnifi/git/pyflecs/flecs.h: 8046
if _libs["libflecs.dylib"].has("ecs_id_match", "cdecl"):
    ecs_id_match = _libs["libflecs.dylib"].get("ecs_id_match", "cdecl")
    ecs_id_match.argtypes = [ecs_id_t, ecs_id_t]
    ecs_id_match.restype = c_bool

# /Users/cnifi/git/pyflecs/flecs.h: 8056
if _libs["libflecs.dylib"].has("ecs_id_is_pair", "cdecl"):
    ecs_id_is_pair = _libs["libflecs.dylib"].get("ecs_id_is_pair", "cdecl")
    ecs_id_is_pair.argtypes = [ecs_id_t]
    ecs_id_is_pair.restype = c_bool

# /Users/cnifi/git/pyflecs/flecs.h: 8065
if _libs["libflecs.dylib"].has("ecs_id_is_wildcard", "cdecl"):
    ecs_id_is_wildcard = _libs["libflecs.dylib"].get("ecs_id_is_wildcard", "cdecl")
    ecs_id_is_wildcard.argtypes = [ecs_id_t]
    ecs_id_is_wildcard.restype = c_bool

# /Users/cnifi/git/pyflecs/flecs.h: 8073
if _libs["libflecs.dylib"].has("ecs_id_is_any", "cdecl"):
    ecs_id_is_any = _libs["libflecs.dylib"].get("ecs_id_is_any", "cdecl")
    ecs_id_is_any.argtypes = [ecs_id_t]
    ecs_id_is_any.restype = c_bool

# /Users/cnifi/git/pyflecs/flecs.h: 8090
if _libs["libflecs.dylib"].has("ecs_id_is_valid", "cdecl"):
    ecs_id_is_valid = _libs["libflecs.dylib"].get("ecs_id_is_valid", "cdecl")
    ecs_id_is_valid.argtypes = [POINTER(ecs_world_t), ecs_id_t]
    ecs_id_is_valid.restype = c_bool

# /Users/cnifi/git/pyflecs/flecs.h: 8103
if _libs["libflecs.dylib"].has("ecs_id_get_flags", "cdecl"):
    ecs_id_get_flags = _libs["libflecs.dylib"].get("ecs_id_get_flags", "cdecl")
    ecs_id_get_flags.argtypes = [POINTER(ecs_world_t), ecs_id_t]
    ecs_id_get_flags.restype = ecs_flags32_t

# /Users/cnifi/git/pyflecs/flecs.h: 8114
if _libs["libflecs.dylib"].has("ecs_id_flag_str", "cdecl"):
    ecs_id_flag_str = _libs["libflecs.dylib"].get("ecs_id_flag_str", "cdecl")
    ecs_id_flag_str.argtypes = [ecs_id_t]
    ecs_id_flag_str.restype = c_char_p

# /Users/cnifi/git/pyflecs/flecs.h: 8125
if _libs["libflecs.dylib"].has("ecs_id_str", "cdecl"):
    ecs_id_str = _libs["libflecs.dylib"].get("ecs_id_str", "cdecl")
    ecs_id_str.argtypes = [POINTER(ecs_world_t), ecs_id_t]
    if sizeof(c_int) == sizeof(c_void_p):
        ecs_id_str.restype = ReturnString
    else:
        ecs_id_str.restype = String
        ecs_id_str.errcheck = ReturnString

# /Users/cnifi/git/pyflecs/flecs.h: 8137
if _libs["libflecs.dylib"].has("ecs_id_str_buf", "cdecl"):
    ecs_id_str_buf = _libs["libflecs.dylib"].get("ecs_id_str_buf", "cdecl")
    ecs_id_str_buf.argtypes = [POINTER(ecs_world_t), ecs_id_t, POINTER(ecs_strbuf_t)]
    ecs_id_str_buf.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 8150
if _libs["libflecs.dylib"].has("ecs_id_from_str", "cdecl"):
    ecs_id_from_str = _libs["libflecs.dylib"].get("ecs_id_from_str", "cdecl")
    ecs_id_from_str.argtypes = [POINTER(ecs_world_t), String]
    ecs_id_from_str.restype = ecs_id_t

# /Users/cnifi/git/pyflecs/flecs.h: 8168
if _libs["libflecs.dylib"].has("ecs_term_ref_is_set", "cdecl"):
    ecs_term_ref_is_set = _libs["libflecs.dylib"].get("ecs_term_ref_is_set", "cdecl")
    ecs_term_ref_is_set.argtypes = [POINTER(ecs_term_ref_t)]
    ecs_term_ref_is_set.restype = c_bool

# /Users/cnifi/git/pyflecs/flecs.h: 8183
if _libs["libflecs.dylib"].has("ecs_term_is_initialized", "cdecl"):
    ecs_term_is_initialized = _libs["libflecs.dylib"].get(
        "ecs_term_is_initialized", "cdecl"
    )
    ecs_term_is_initialized.argtypes = [POINTER(ecs_term_t)]
    ecs_term_is_initialized.restype = c_bool

# /Users/cnifi/git/pyflecs/flecs.h: 8201
if _libs["libflecs.dylib"].has("ecs_term_match_this", "cdecl"):
    ecs_term_match_this = _libs["libflecs.dylib"].get("ecs_term_match_this", "cdecl")
    ecs_term_match_this.argtypes = [POINTER(ecs_term_t)]
    ecs_term_match_this.restype = c_bool

# /Users/cnifi/git/pyflecs/flecs.h: 8217
if _libs["libflecs.dylib"].has("ecs_term_match_0", "cdecl"):
    ecs_term_match_0 = _libs["libflecs.dylib"].get("ecs_term_match_0", "cdecl")
    ecs_term_match_0.argtypes = [POINTER(ecs_term_t)]
    ecs_term_match_0.restype = c_bool

# /Users/cnifi/git/pyflecs/flecs.h: 8229
if _libs["libflecs.dylib"].has("ecs_term_str", "cdecl"):
    ecs_term_str = _libs["libflecs.dylib"].get("ecs_term_str", "cdecl")
    ecs_term_str.argtypes = [POINTER(ecs_world_t), POINTER(ecs_term_t)]
    if sizeof(c_int) == sizeof(c_void_p):
        ecs_term_str.restype = ReturnString
    else:
        ecs_term_str.restype = String
        ecs_term_str.errcheck = ReturnString

# /Users/cnifi/git/pyflecs/flecs.h: 8241
if _libs["libflecs.dylib"].has("ecs_query_str", "cdecl"):
    ecs_query_str = _libs["libflecs.dylib"].get("ecs_query_str", "cdecl")
    ecs_query_str.argtypes = [POINTER(ecs_query_t)]
    if sizeof(c_int) == sizeof(c_void_p):
        ecs_query_str.restype = ReturnString
    else:
        ecs_query_str.restype = String
        ecs_query_str.errcheck = ReturnString

# /Users/cnifi/git/pyflecs/flecs.h: 8285
if _libs["libflecs.dylib"].has("ecs_each_id", "cdecl"):
    ecs_each_id = _libs["libflecs.dylib"].get("ecs_each_id", "cdecl")
    ecs_each_id.argtypes = [POINTER(ecs_world_t), ecs_id_t]
    ecs_each_id.restype = ecs_iter_t

# /Users/cnifi/git/pyflecs/flecs.h: 8295
if _libs["libflecs.dylib"].has("ecs_each_next", "cdecl"):
    ecs_each_next = _libs["libflecs.dylib"].get("ecs_each_next", "cdecl")
    ecs_each_next.argtypes = [POINTER(ecs_iter_t)]
    ecs_each_next.restype = c_bool

# /Users/cnifi/git/pyflecs/flecs.h: 8315
if _libs["libflecs.dylib"].has("ecs_children", "cdecl"):
    ecs_children = _libs["libflecs.dylib"].get("ecs_children", "cdecl")
    ecs_children.argtypes = [POINTER(ecs_world_t), ecs_entity_t]
    ecs_children.restype = ecs_iter_t

# /Users/cnifi/git/pyflecs/flecs.h: 8325
if _libs["libflecs.dylib"].has("ecs_children_next", "cdecl"):
    ecs_children_next = _libs["libflecs.dylib"].get("ecs_children_next", "cdecl")
    ecs_children_next.argtypes = [POINTER(ecs_iter_t)]
    ecs_children_next.restype = c_bool

# /Users/cnifi/git/pyflecs/flecs.h: 8344
if _libs["libflecs.dylib"].has("ecs_query_init", "cdecl"):
    ecs_query_init = _libs["libflecs.dylib"].get("ecs_query_init", "cdecl")
    ecs_query_init.argtypes = [POINTER(ecs_world_t), POINTER(ecs_query_desc_t)]
    ecs_query_init.restype = POINTER(ecs_query_t)

# /Users/cnifi/git/pyflecs/flecs.h: 8353
if _libs["libflecs.dylib"].has("ecs_query_fini", "cdecl"):
    ecs_query_fini = _libs["libflecs.dylib"].get("ecs_query_fini", "cdecl")
    ecs_query_fini.argtypes = [POINTER(ecs_query_t)]
    ecs_query_fini.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 8365
if _libs["libflecs.dylib"].has("ecs_query_find_var", "cdecl"):
    ecs_query_find_var = _libs["libflecs.dylib"].get("ecs_query_find_var", "cdecl")
    ecs_query_find_var.argtypes = [POINTER(ecs_query_t), String]
    ecs_query_find_var.restype = c_int32

# /Users/cnifi/git/pyflecs/flecs.h: 8377
if _libs["libflecs.dylib"].has("ecs_query_var_name", "cdecl"):
    ecs_query_var_name = _libs["libflecs.dylib"].get("ecs_query_var_name", "cdecl")
    ecs_query_var_name.argtypes = [POINTER(ecs_query_t), c_int32]
    ecs_query_var_name.restype = c_char_p

# /Users/cnifi/git/pyflecs/flecs.h: 8392
if _libs["libflecs.dylib"].has("ecs_query_var_is_entity", "cdecl"):
    ecs_query_var_is_entity = _libs["libflecs.dylib"].get(
        "ecs_query_var_is_entity", "cdecl"
    )
    ecs_query_var_is_entity.argtypes = [POINTER(ecs_query_t), c_int32]
    ecs_query_var_is_entity.restype = c_bool

# /Users/cnifi/git/pyflecs/flecs.h: 8465
if _libs["libflecs.dylib"].has("ecs_query_iter", "cdecl"):
    ecs_query_iter = _libs["libflecs.dylib"].get("ecs_query_iter", "cdecl")
    ecs_query_iter.argtypes = [POINTER(ecs_world_t), POINTER(ecs_query_t)]
    ecs_query_iter.restype = ecs_iter_t

# /Users/cnifi/git/pyflecs/flecs.h: 8477
if _libs["libflecs.dylib"].has("ecs_query_next", "cdecl"):
    ecs_query_next = _libs["libflecs.dylib"].get("ecs_query_next", "cdecl")
    ecs_query_next.argtypes = [POINTER(ecs_iter_t)]
    ecs_query_next.restype = c_bool

# /Users/cnifi/git/pyflecs/flecs.h: 8499
if _libs["libflecs.dylib"].has("ecs_query_has", "cdecl"):
    ecs_query_has = _libs["libflecs.dylib"].get("ecs_query_has", "cdecl")
    ecs_query_has.argtypes = [POINTER(ecs_query_t), ecs_entity_t, POINTER(ecs_iter_t)]
    ecs_query_has.restype = c_bool

# /Users/cnifi/git/pyflecs/flecs.h: 8523
if _libs["libflecs.dylib"].has("ecs_query_has_table", "cdecl"):
    ecs_query_has_table = _libs["libflecs.dylib"].get("ecs_query_has_table", "cdecl")
    ecs_query_has_table.argtypes = [
        POINTER(ecs_query_t),
        POINTER(ecs_table_t),
        POINTER(ecs_iter_t),
    ]
    ecs_query_has_table.restype = c_bool

# /Users/cnifi/git/pyflecs/flecs.h: 8555
if _libs["libflecs.dylib"].has("ecs_query_has_range", "cdecl"):
    ecs_query_has_range = _libs["libflecs.dylib"].get("ecs_query_has_range", "cdecl")
    ecs_query_has_range.argtypes = [
        POINTER(ecs_query_t),
        POINTER(ecs_table_range_t),
        POINTER(ecs_iter_t),
    ]
    ecs_query_has_range.restype = c_bool

# /Users/cnifi/git/pyflecs/flecs.h: 8568
if _libs["libflecs.dylib"].has("ecs_query_match_count", "cdecl"):
    ecs_query_match_count = _libs["libflecs.dylib"].get(
        "ecs_query_match_count", "cdecl"
    )
    ecs_query_match_count.argtypes = [POINTER(ecs_query_t)]
    ecs_query_match_count.restype = c_int32

# /Users/cnifi/git/pyflecs/flecs.h: 8581
if _libs["libflecs.dylib"].has("ecs_query_plan", "cdecl"):
    ecs_query_plan = _libs["libflecs.dylib"].get("ecs_query_plan", "cdecl")
    ecs_query_plan.argtypes = [POINTER(ecs_query_t)]
    if sizeof(c_int) == sizeof(c_void_p):
        ecs_query_plan.restype = ReturnString
    else:
        ecs_query_plan.restype = String
        ecs_query_plan.errcheck = ReturnString

# /Users/cnifi/git/pyflecs/flecs.h: 8599
if _libs["libflecs.dylib"].has("ecs_query_plan_w_profile", "cdecl"):
    ecs_query_plan_w_profile = _libs["libflecs.dylib"].get(
        "ecs_query_plan_w_profile", "cdecl"
    )
    ecs_query_plan_w_profile.argtypes = [POINTER(ecs_query_t), POINTER(ecs_iter_t)]
    if sizeof(c_int) == sizeof(c_void_p):
        ecs_query_plan_w_profile.restype = ReturnString
    else:
        ecs_query_plan_w_profile.restype = String
        ecs_query_plan_w_profile.errcheck = ReturnString

# /Users/cnifi/git/pyflecs/flecs.h: 8621
if _libs["libflecs.dylib"].has("ecs_query_args_parse", "cdecl"):
    ecs_query_args_parse = _libs["libflecs.dylib"].get("ecs_query_args_parse", "cdecl")
    ecs_query_args_parse.argtypes = [POINTER(ecs_query_t), POINTER(ecs_iter_t), String]
    ecs_query_args_parse.restype = c_char_p

# /Users/cnifi/git/pyflecs/flecs.h: 8654
if _libs["libflecs.dylib"].has("ecs_query_changed", "cdecl"):
    ecs_query_changed = _libs["libflecs.dylib"].get("ecs_query_changed", "cdecl")
    ecs_query_changed.argtypes = [POINTER(ecs_query_t)]
    ecs_query_changed.restype = c_bool

# /Users/cnifi/git/pyflecs/flecs.h: 8666
if _libs["libflecs.dylib"].has("ecs_query_get", "cdecl"):
    ecs_query_get = _libs["libflecs.dylib"].get("ecs_query_get", "cdecl")
    ecs_query_get.argtypes = [POINTER(ecs_world_t), ecs_entity_t]
    ecs_query_get.restype = POINTER(ecs_query_t)

# /Users/cnifi/git/pyflecs/flecs.h: 8681
if _libs["libflecs.dylib"].has("ecs_iter_skip", "cdecl"):
    ecs_iter_skip = _libs["libflecs.dylib"].get("ecs_iter_skip", "cdecl")
    ecs_iter_skip.argtypes = [POINTER(ecs_iter_t)]
    ecs_iter_skip.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 8707
if _libs["libflecs.dylib"].has("ecs_iter_set_group", "cdecl"):
    ecs_iter_set_group = _libs["libflecs.dylib"].get("ecs_iter_set_group", "cdecl")
    ecs_iter_set_group.argtypes = [POINTER(ecs_iter_t), uint64_t]
    ecs_iter_set_group.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 8720
if _libs["libflecs.dylib"].has("ecs_query_get_group_ctx", "cdecl"):
    ecs_query_get_group_ctx = _libs["libflecs.dylib"].get(
        "ecs_query_get_group_ctx", "cdecl"
    )
    ecs_query_get_group_ctx.argtypes = [POINTER(ecs_query_t), uint64_t]
    ecs_query_get_group_ctx.restype = POINTER(c_ubyte)
    ecs_query_get_group_ctx.errcheck = lambda v, *a: cast(v, c_void_p)

# /Users/cnifi/git/pyflecs/flecs.h: 8733
if _libs["libflecs.dylib"].has("ecs_query_get_group_info", "cdecl"):
    ecs_query_get_group_info = _libs["libflecs.dylib"].get(
        "ecs_query_get_group_info", "cdecl"
    )
    ecs_query_get_group_info.argtypes = [POINTER(ecs_query_t), uint64_t]
    ecs_query_get_group_info.restype = POINTER(ecs_query_group_info_t)


# /Users/cnifi/git/pyflecs/flecs.h: 8744
class struct_ecs_query_count_t(Structure):
    pass


struct_ecs_query_count_t.__slots__ = [
    "results",
    "entities",
    "tables",
]
struct_ecs_query_count_t._fields_ = [
    ("results", c_int32),
    ("entities", c_int32),
    ("tables", c_int32),
]

ecs_query_count_t = struct_ecs_query_count_t  # /Users/cnifi/git/pyflecs/flecs.h: 8744

# /Users/cnifi/git/pyflecs/flecs.h: 8753
if _libs["libflecs.dylib"].has("ecs_query_count", "cdecl"):
    ecs_query_count = _libs["libflecs.dylib"].get("ecs_query_count", "cdecl")
    ecs_query_count.argtypes = [POINTER(ecs_query_t)]
    ecs_query_count.restype = ecs_query_count_t

# /Users/cnifi/git/pyflecs/flecs.h: 8762
if _libs["libflecs.dylib"].has("ecs_query_is_true", "cdecl"):
    ecs_query_is_true = _libs["libflecs.dylib"].get("ecs_query_is_true", "cdecl")
    ecs_query_is_true.argtypes = [POINTER(ecs_query_t)]
    ecs_query_is_true.restype = c_bool

# /Users/cnifi/git/pyflecs/flecs.h: 8774
if _libs["libflecs.dylib"].has("ecs_query_get_cache_query", "cdecl"):
    ecs_query_get_cache_query = _libs["libflecs.dylib"].get(
        "ecs_query_get_cache_query", "cdecl"
    )
    ecs_query_get_cache_query.argtypes = [POINTER(ecs_query_t)]
    ecs_query_get_cache_query.restype = POINTER(ecs_query_t)

# /Users/cnifi/git/pyflecs/flecs.h: 8806
if _libs["libflecs.dylib"].has("ecs_emit", "cdecl"):
    ecs_emit = _libs["libflecs.dylib"].get("ecs_emit", "cdecl")
    ecs_emit.argtypes = [POINTER(ecs_world_t), POINTER(ecs_event_desc_t)]
    ecs_emit.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 8821
if _libs["libflecs.dylib"].has("ecs_enqueue", "cdecl"):
    ecs_enqueue = _libs["libflecs.dylib"].get("ecs_enqueue", "cdecl")
    ecs_enqueue.argtypes = [POINTER(ecs_world_t), POINTER(ecs_event_desc_t)]
    ecs_enqueue.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 8836
if _libs["libflecs.dylib"].has("ecs_observer_init", "cdecl"):
    ecs_observer_init = _libs["libflecs.dylib"].get("ecs_observer_init", "cdecl")
    ecs_observer_init.argtypes = [POINTER(ecs_world_t), POINTER(ecs_observer_desc_t)]
    ecs_observer_init.restype = ecs_entity_t

# /Users/cnifi/git/pyflecs/flecs.h: 8849
if _libs["libflecs.dylib"].has("ecs_observer_get", "cdecl"):
    ecs_observer_get = _libs["libflecs.dylib"].get("ecs_observer_get", "cdecl")
    ecs_observer_get.argtypes = [POINTER(ecs_world_t), ecs_entity_t]
    ecs_observer_get.restype = POINTER(ecs_observer_t)

# /Users/cnifi/git/pyflecs/flecs.h: 8876
if _libs["libflecs.dylib"].has("ecs_iter_next", "cdecl"):
    ecs_iter_next = _libs["libflecs.dylib"].get("ecs_iter_next", "cdecl")
    ecs_iter_next.argtypes = [POINTER(ecs_iter_t)]
    ecs_iter_next.restype = c_bool

# /Users/cnifi/git/pyflecs/flecs.h: 8889
if _libs["libflecs.dylib"].has("ecs_iter_fini", "cdecl"):
    ecs_iter_fini = _libs["libflecs.dylib"].get("ecs_iter_fini", "cdecl")
    ecs_iter_fini.argtypes = [POINTER(ecs_iter_t)]
    ecs_iter_fini.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 8904
if _libs["libflecs.dylib"].has("ecs_iter_count", "cdecl"):
    ecs_iter_count = _libs["libflecs.dylib"].get("ecs_iter_count", "cdecl")
    ecs_iter_count.argtypes = [POINTER(ecs_iter_t)]
    ecs_iter_count.restype = c_int32

# /Users/cnifi/git/pyflecs/flecs.h: 8920
if _libs["libflecs.dylib"].has("ecs_iter_is_true", "cdecl"):
    ecs_iter_is_true = _libs["libflecs.dylib"].get("ecs_iter_is_true", "cdecl")
    ecs_iter_is_true.argtypes = [POINTER(ecs_iter_t)]
    ecs_iter_is_true.restype = c_bool

# /Users/cnifi/git/pyflecs/flecs.h: 8931
if _libs["libflecs.dylib"].has("ecs_iter_first", "cdecl"):
    ecs_iter_first = _libs["libflecs.dylib"].get("ecs_iter_first", "cdecl")
    ecs_iter_first.argtypes = [POINTER(ecs_iter_t)]
    ecs_iter_first.restype = ecs_entity_t

# /Users/cnifi/git/pyflecs/flecs.h: 8973
if _libs["libflecs.dylib"].has("ecs_iter_set_var", "cdecl"):
    ecs_iter_set_var = _libs["libflecs.dylib"].get("ecs_iter_set_var", "cdecl")
    ecs_iter_set_var.argtypes = [POINTER(ecs_iter_t), c_int32, ecs_entity_t]
    ecs_iter_set_var.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 8989
if _libs["libflecs.dylib"].has("ecs_iter_set_var_as_table", "cdecl"):
    ecs_iter_set_var_as_table = _libs["libflecs.dylib"].get(
        "ecs_iter_set_var_as_table", "cdecl"
    )
    ecs_iter_set_var_as_table.argtypes = [
        POINTER(ecs_iter_t),
        c_int32,
        POINTER(ecs_table_t),
    ]
    ecs_iter_set_var_as_table.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 9005
if _libs["libflecs.dylib"].has("ecs_iter_set_var_as_range", "cdecl"):
    ecs_iter_set_var_as_range = _libs["libflecs.dylib"].get(
        "ecs_iter_set_var_as_range", "cdecl"
    )
    ecs_iter_set_var_as_range.argtypes = [
        POINTER(ecs_iter_t),
        c_int32,
        POINTER(ecs_table_range_t),
    ]
    ecs_iter_set_var_as_range.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 9023
if _libs["libflecs.dylib"].has("ecs_iter_get_var", "cdecl"):
    ecs_iter_get_var = _libs["libflecs.dylib"].get("ecs_iter_get_var", "cdecl")
    ecs_iter_get_var.argtypes = [POINTER(ecs_iter_t), c_int32]
    ecs_iter_get_var.restype = ecs_entity_t

# /Users/cnifi/git/pyflecs/flecs.h: 9034
if _libs["libflecs.dylib"].has("ecs_iter_get_var_name", "cdecl"):
    ecs_iter_get_var_name = _libs["libflecs.dylib"].get(
        "ecs_iter_get_var_name", "cdecl"
    )
    ecs_iter_get_var_name.argtypes = [POINTER(ecs_iter_t), c_int32]
    ecs_iter_get_var_name.restype = c_char_p

# /Users/cnifi/git/pyflecs/flecs.h: 9044
if _libs["libflecs.dylib"].has("ecs_iter_get_var_count", "cdecl"):
    ecs_iter_get_var_count = _libs["libflecs.dylib"].get(
        "ecs_iter_get_var_count", "cdecl"
    )
    ecs_iter_get_var_count.argtypes = [POINTER(ecs_iter_t)]
    ecs_iter_get_var_count.restype = c_int32

# /Users/cnifi/git/pyflecs/flecs.h: 9053
if _libs["libflecs.dylib"].has("ecs_iter_get_vars", "cdecl"):
    ecs_iter_get_vars = _libs["libflecs.dylib"].get("ecs_iter_get_vars", "cdecl")
    ecs_iter_get_vars.argtypes = [POINTER(ecs_iter_t)]
    ecs_iter_get_vars.restype = POINTER(ecs_var_t)

# /Users/cnifi/git/pyflecs/flecs.h: 9070
if _libs["libflecs.dylib"].has("ecs_iter_get_var_as_table", "cdecl"):
    ecs_iter_get_var_as_table = _libs["libflecs.dylib"].get(
        "ecs_iter_get_var_as_table", "cdecl"
    )
    ecs_iter_get_var_as_table.argtypes = [POINTER(ecs_iter_t), c_int32]
    ecs_iter_get_var_as_table.restype = POINTER(ecs_table_t)

# /Users/cnifi/git/pyflecs/flecs.h: 9088
if _libs["libflecs.dylib"].has("ecs_iter_get_var_as_range", "cdecl"):
    ecs_iter_get_var_as_range = _libs["libflecs.dylib"].get(
        "ecs_iter_get_var_as_range", "cdecl"
    )
    ecs_iter_get_var_as_range.argtypes = [POINTER(ecs_iter_t), c_int32]
    ecs_iter_get_var_as_range.restype = ecs_table_range_t

# /Users/cnifi/git/pyflecs/flecs.h: 9104
if _libs["libflecs.dylib"].has("ecs_iter_var_is_constrained", "cdecl"):
    ecs_iter_var_is_constrained = _libs["libflecs.dylib"].get(
        "ecs_iter_var_is_constrained", "cdecl"
    )
    ecs_iter_var_is_constrained.argtypes = [POINTER(ecs_iter_t), c_int32]
    ecs_iter_var_is_constrained.restype = c_bool

# /Users/cnifi/git/pyflecs/flecs.h: 9120
if _libs["libflecs.dylib"].has("ecs_iter_get_group", "cdecl"):
    ecs_iter_get_group = _libs["libflecs.dylib"].get("ecs_iter_get_group", "cdecl")
    ecs_iter_get_group.argtypes = [POINTER(ecs_iter_t)]
    ecs_iter_get_group.restype = uint64_t

# /Users/cnifi/git/pyflecs/flecs.h: 9135
if _libs["libflecs.dylib"].has("ecs_iter_changed", "cdecl"):
    ecs_iter_changed = _libs["libflecs.dylib"].get("ecs_iter_changed", "cdecl")
    ecs_iter_changed.argtypes = [POINTER(ecs_iter_t)]
    ecs_iter_changed.restype = c_bool

# /Users/cnifi/git/pyflecs/flecs.h: 9150
if _libs["libflecs.dylib"].has("ecs_iter_str", "cdecl"):
    ecs_iter_str = _libs["libflecs.dylib"].get("ecs_iter_str", "cdecl")
    ecs_iter_str.argtypes = [POINTER(ecs_iter_t)]
    if sizeof(c_int) == sizeof(c_void_p):
        ecs_iter_str.restype = ReturnString
    else:
        ecs_iter_str.restype = String
        ecs_iter_str.errcheck = ReturnString

# /Users/cnifi/git/pyflecs/flecs.h: 9169
if _libs["libflecs.dylib"].has("ecs_page_iter", "cdecl"):
    ecs_page_iter = _libs["libflecs.dylib"].get("ecs_page_iter", "cdecl")
    ecs_page_iter.argtypes = [POINTER(ecs_iter_t), c_int32, c_int32]
    ecs_page_iter.restype = ecs_iter_t

# /Users/cnifi/git/pyflecs/flecs.h: 9181
if _libs["libflecs.dylib"].has("ecs_page_next", "cdecl"):
    ecs_page_next = _libs["libflecs.dylib"].get("ecs_page_next", "cdecl")
    ecs_page_next.argtypes = [POINTER(ecs_iter_t)]
    ecs_page_next.restype = c_bool

# /Users/cnifi/git/pyflecs/flecs.h: 9205
if _libs["libflecs.dylib"].has("ecs_worker_iter", "cdecl"):
    ecs_worker_iter = _libs["libflecs.dylib"].get("ecs_worker_iter", "cdecl")
    ecs_worker_iter.argtypes = [POINTER(ecs_iter_t), c_int32, c_int32]
    ecs_worker_iter.restype = ecs_iter_t

# /Users/cnifi/git/pyflecs/flecs.h: 9217
if _libs["libflecs.dylib"].has("ecs_worker_next", "cdecl"):
    ecs_worker_next = _libs["libflecs.dylib"].get("ecs_worker_next", "cdecl")
    ecs_worker_next.argtypes = [POINTER(ecs_iter_t)]
    ecs_worker_next.restype = c_bool

# /Users/cnifi/git/pyflecs/flecs.h: 9262
if _libs["libflecs.dylib"].has("ecs_field_w_size", "cdecl"):
    ecs_field_w_size = _libs["libflecs.dylib"].get("ecs_field_w_size", "cdecl")
    ecs_field_w_size.argtypes = [POINTER(ecs_iter_t), c_size_t, c_int8]
    ecs_field_w_size.restype = POINTER(c_ubyte)
    ecs_field_w_size.errcheck = lambda v, *a: cast(v, c_void_p)

# /Users/cnifi/git/pyflecs/flecs.h: 9289
if _libs["libflecs.dylib"].has("ecs_field_at_w_size", "cdecl"):
    ecs_field_at_w_size = _libs["libflecs.dylib"].get("ecs_field_at_w_size", "cdecl")
    ecs_field_at_w_size.argtypes = [POINTER(ecs_iter_t), c_size_t, c_int8, c_int32]
    ecs_field_at_w_size.restype = POINTER(c_ubyte)
    ecs_field_at_w_size.errcheck = lambda v, *a: cast(v, c_void_p)

# /Users/cnifi/git/pyflecs/flecs.h: 9304
if _libs["libflecs.dylib"].has("ecs_field_is_readonly", "cdecl"):
    ecs_field_is_readonly = _libs["libflecs.dylib"].get(
        "ecs_field_is_readonly", "cdecl"
    )
    ecs_field_is_readonly.argtypes = [POINTER(ecs_iter_t), c_int8]
    ecs_field_is_readonly.restype = c_bool

# /Users/cnifi/git/pyflecs/flecs.h: 9319
if _libs["libflecs.dylib"].has("ecs_field_is_writeonly", "cdecl"):
    ecs_field_is_writeonly = _libs["libflecs.dylib"].get(
        "ecs_field_is_writeonly", "cdecl"
    )
    ecs_field_is_writeonly.argtypes = [POINTER(ecs_iter_t), c_int8]
    ecs_field_is_writeonly.restype = c_bool

# /Users/cnifi/git/pyflecs/flecs.h: 9330
if _libs["libflecs.dylib"].has("ecs_field_is_set", "cdecl"):
    ecs_field_is_set = _libs["libflecs.dylib"].get("ecs_field_is_set", "cdecl")
    ecs_field_is_set.argtypes = [POINTER(ecs_iter_t), c_int8]
    ecs_field_is_set.restype = c_bool

# /Users/cnifi/git/pyflecs/flecs.h: 9341
if _libs["libflecs.dylib"].has("ecs_field_id", "cdecl"):
    ecs_field_id = _libs["libflecs.dylib"].get("ecs_field_id", "cdecl")
    ecs_field_id.argtypes = [POINTER(ecs_iter_t), c_int8]
    ecs_field_id.restype = ecs_id_t

# /Users/cnifi/git/pyflecs/flecs.h: 9354
if _libs["libflecs.dylib"].has("ecs_field_column", "cdecl"):
    ecs_field_column = _libs["libflecs.dylib"].get("ecs_field_column", "cdecl")
    ecs_field_column.argtypes = [POINTER(ecs_iter_t), c_int8]
    ecs_field_column.restype = c_int32

# /Users/cnifi/git/pyflecs/flecs.h: 9366
if _libs["libflecs.dylib"].has("ecs_field_src", "cdecl"):
    ecs_field_src = _libs["libflecs.dylib"].get("ecs_field_src", "cdecl")
    ecs_field_src.argtypes = [POINTER(ecs_iter_t), c_int8]
    ecs_field_src.restype = ecs_entity_t

# /Users/cnifi/git/pyflecs/flecs.h: 9378
if _libs["libflecs.dylib"].has("ecs_field_size", "cdecl"):
    ecs_field_size = _libs["libflecs.dylib"].get("ecs_field_size", "cdecl")
    ecs_field_size.argtypes = [POINTER(ecs_iter_t), c_int8]
    ecs_field_size.restype = c_size_t

# /Users/cnifi/git/pyflecs/flecs.h: 9396
if _libs["libflecs.dylib"].has("ecs_field_is_self", "cdecl"):
    ecs_field_is_self = _libs["libflecs.dylib"].get("ecs_field_is_self", "cdecl")
    ecs_field_is_self.argtypes = [POINTER(ecs_iter_t), c_int8]
    ecs_field_is_self.restype = c_bool

# /Users/cnifi/git/pyflecs/flecs.h: 9416
if _libs["libflecs.dylib"].has("ecs_table_get_type", "cdecl"):
    ecs_table_get_type = _libs["libflecs.dylib"].get("ecs_table_get_type", "cdecl")
    ecs_table_get_type.argtypes = [POINTER(ecs_table_t)]
    ecs_table_get_type.restype = POINTER(ecs_type_t)

# /Users/cnifi/git/pyflecs/flecs.h: 9430
if _libs["libflecs.dylib"].has("ecs_table_get_type_index", "cdecl"):
    ecs_table_get_type_index = _libs["libflecs.dylib"].get(
        "ecs_table_get_type_index", "cdecl"
    )
    ecs_table_get_type_index.argtypes = [
        POINTER(ecs_world_t),
        POINTER(ecs_table_t),
        ecs_id_t,
    ]
    ecs_table_get_type_index.restype = c_int32

# /Users/cnifi/git/pyflecs/flecs.h: 9445
if _libs["libflecs.dylib"].has("ecs_table_get_column_index", "cdecl"):
    ecs_table_get_column_index = _libs["libflecs.dylib"].get(
        "ecs_table_get_column_index", "cdecl"
    )
    ecs_table_get_column_index.argtypes = [
        POINTER(ecs_world_t),
        POINTER(ecs_table_t),
        ecs_id_t,
    ]
    ecs_table_get_column_index.restype = c_int32

# /Users/cnifi/git/pyflecs/flecs.h: 9458
if _libs["libflecs.dylib"].has("ecs_table_column_count", "cdecl"):
    ecs_table_column_count = _libs["libflecs.dylib"].get(
        "ecs_table_column_count", "cdecl"
    )
    ecs_table_column_count.argtypes = [POINTER(ecs_table_t)]
    ecs_table_column_count.restype = c_int32

# /Users/cnifi/git/pyflecs/flecs.h: 9475
if _libs["libflecs.dylib"].has("ecs_table_type_to_column_index", "cdecl"):
    ecs_table_type_to_column_index = _libs["libflecs.dylib"].get(
        "ecs_table_type_to_column_index", "cdecl"
    )
    ecs_table_type_to_column_index.argtypes = [POINTER(ecs_table_t), c_int32]
    ecs_table_type_to_column_index.restype = c_int32

# /Users/cnifi/git/pyflecs/flecs.h: 9488
if _libs["libflecs.dylib"].has("ecs_table_column_to_type_index", "cdecl"):
    ecs_table_column_to_type_index = _libs["libflecs.dylib"].get(
        "ecs_table_column_to_type_index", "cdecl"
    )
    ecs_table_column_to_type_index.argtypes = [POINTER(ecs_table_t), c_int32]
    ecs_table_column_to_type_index.restype = c_int32

# /Users/cnifi/git/pyflecs/flecs.h: 9501
if _libs["libflecs.dylib"].has("ecs_table_get_column", "cdecl"):
    ecs_table_get_column = _libs["libflecs.dylib"].get("ecs_table_get_column", "cdecl")
    ecs_table_get_column.argtypes = [POINTER(ecs_table_t), c_int32, c_int32]
    ecs_table_get_column.restype = POINTER(c_ubyte)
    ecs_table_get_column.errcheck = lambda v, *a: cast(v, c_void_p)

# /Users/cnifi/git/pyflecs/flecs.h: 9516
if _libs["libflecs.dylib"].has("ecs_table_get_id", "cdecl"):
    ecs_table_get_id = _libs["libflecs.dylib"].get("ecs_table_get_id", "cdecl")
    ecs_table_get_id.argtypes = [
        POINTER(ecs_world_t),
        POINTER(ecs_table_t),
        ecs_id_t,
        c_int32,
    ]
    ecs_table_get_id.restype = POINTER(c_ubyte)
    ecs_table_get_id.errcheck = lambda v, *a: cast(v, c_void_p)

# /Users/cnifi/git/pyflecs/flecs.h: 9530
if _libs["libflecs.dylib"].has("ecs_table_get_column_size", "cdecl"):
    ecs_table_get_column_size = _libs["libflecs.dylib"].get(
        "ecs_table_get_column_size", "cdecl"
    )
    ecs_table_get_column_size.argtypes = [POINTER(ecs_table_t), c_int32]
    ecs_table_get_column_size.restype = c_size_t

# /Users/cnifi/git/pyflecs/flecs.h: 9541
if _libs["libflecs.dylib"].has("ecs_table_count", "cdecl"):
    ecs_table_count = _libs["libflecs.dylib"].get("ecs_table_count", "cdecl")
    ecs_table_count.argtypes = [POINTER(ecs_table_t)]
    ecs_table_count.restype = c_int32

# /Users/cnifi/git/pyflecs/flecs.h: 9552
if _libs["libflecs.dylib"].has("ecs_table_size", "cdecl"):
    ecs_table_size = _libs["libflecs.dylib"].get("ecs_table_size", "cdecl")
    ecs_table_size.argtypes = [POINTER(ecs_table_t)]
    ecs_table_size.restype = c_int32

# /Users/cnifi/git/pyflecs/flecs.h: 9562
if _libs["libflecs.dylib"].has("ecs_table_entities", "cdecl"):
    ecs_table_entities = _libs["libflecs.dylib"].get("ecs_table_entities", "cdecl")
    ecs_table_entities.argtypes = [POINTER(ecs_table_t)]
    ecs_table_entities.restype = POINTER(ecs_entity_t)

# /Users/cnifi/git/pyflecs/flecs.h: 9576
if _libs["libflecs.dylib"].has("ecs_table_has_id", "cdecl"):
    ecs_table_has_id = _libs["libflecs.dylib"].get("ecs_table_has_id", "cdecl")
    ecs_table_has_id.argtypes = [POINTER(ecs_world_t), POINTER(ecs_table_t), ecs_id_t]
    ecs_table_has_id.restype = c_bool

# /Users/cnifi/git/pyflecs/flecs.h: 9592
if _libs["libflecs.dylib"].has("ecs_table_get_depth", "cdecl"):
    ecs_table_get_depth = _libs["libflecs.dylib"].get("ecs_table_get_depth", "cdecl")
    ecs_table_get_depth.argtypes = [
        POINTER(ecs_world_t),
        POINTER(ecs_table_t),
        ecs_entity_t,
    ]
    ecs_table_get_depth.restype = c_int32

# /Users/cnifi/git/pyflecs/flecs.h: 9607
if _libs["libflecs.dylib"].has("ecs_table_add_id", "cdecl"):
    ecs_table_add_id = _libs["libflecs.dylib"].get("ecs_table_add_id", "cdecl")
    ecs_table_add_id.argtypes = [POINTER(ecs_world_t), POINTER(ecs_table_t), ecs_id_t]
    ecs_table_add_id.restype = POINTER(ecs_table_t)

# /Users/cnifi/git/pyflecs/flecs.h: 9623
if _libs["libflecs.dylib"].has("ecs_table_find", "cdecl"):
    ecs_table_find = _libs["libflecs.dylib"].get("ecs_table_find", "cdecl")
    ecs_table_find.argtypes = [POINTER(ecs_world_t), POINTER(ecs_id_t), c_int32]
    ecs_table_find.restype = POINTER(ecs_table_t)

# /Users/cnifi/git/pyflecs/flecs.h: 9638
if _libs["libflecs.dylib"].has("ecs_table_remove_id", "cdecl"):
    ecs_table_remove_id = _libs["libflecs.dylib"].get("ecs_table_remove_id", "cdecl")
    ecs_table_remove_id.argtypes = [
        POINTER(ecs_world_t),
        POINTER(ecs_table_t),
        ecs_id_t,
    ]
    ecs_table_remove_id.restype = POINTER(ecs_table_t)

# /Users/cnifi/git/pyflecs/flecs.h: 9659
if _libs["libflecs.dylib"].has("ecs_table_lock", "cdecl"):
    ecs_table_lock = _libs["libflecs.dylib"].get("ecs_table_lock", "cdecl")
    ecs_table_lock.argtypes = [POINTER(ecs_world_t), POINTER(ecs_table_t)]
    ecs_table_lock.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 9670
if _libs["libflecs.dylib"].has("ecs_table_unlock", "cdecl"):
    ecs_table_unlock = _libs["libflecs.dylib"].get("ecs_table_unlock", "cdecl")
    ecs_table_unlock.argtypes = [POINTER(ecs_world_t), POINTER(ecs_table_t)]
    ecs_table_unlock.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 9684
if _libs["libflecs.dylib"].has("ecs_table_has_flags", "cdecl"):
    ecs_table_has_flags = _libs["libflecs.dylib"].get("ecs_table_has_flags", "cdecl")
    ecs_table_has_flags.argtypes = [POINTER(ecs_table_t), ecs_flags32_t]
    ecs_table_has_flags.restype = c_bool

# /Users/cnifi/git/pyflecs/flecs.h: 9696
if _libs["libflecs.dylib"].has("ecs_table_has_traversable", "cdecl"):
    ecs_table_has_traversable = _libs["libflecs.dylib"].get(
        "ecs_table_has_traversable", "cdecl"
    )
    ecs_table_has_traversable.argtypes = [POINTER(ecs_table_t)]
    ecs_table_has_traversable.restype = c_bool

# /Users/cnifi/git/pyflecs/flecs.h: 9707
if _libs["libflecs.dylib"].has("ecs_table_swap_rows", "cdecl"):
    ecs_table_swap_rows = _libs["libflecs.dylib"].get("ecs_table_swap_rows", "cdecl")
    ecs_table_swap_rows.argtypes = [
        POINTER(ecs_world_t),
        POINTER(ecs_table_t),
        c_int32,
        c_int32,
    ]
    ecs_table_swap_rows.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 9736
if _libs["libflecs.dylib"].has("ecs_commit", "cdecl"):
    ecs_commit = _libs["libflecs.dylib"].get("ecs_commit", "cdecl")
    ecs_commit.argtypes = [
        POINTER(ecs_world_t),
        ecs_entity_t,
        POINTER(ecs_record_t),
        POINTER(ecs_table_t),
        POINTER(ecs_type_t),
        POINTER(ecs_type_t),
    ]
    ecs_commit.restype = c_bool

# /Users/cnifi/git/pyflecs/flecs.h: 9764
if _libs["libflecs.dylib"].has("ecs_search", "cdecl"):
    ecs_search = _libs["libflecs.dylib"].get("ecs_search", "cdecl")
    ecs_search.argtypes = [
        POINTER(ecs_world_t),
        POINTER(ecs_table_t),
        ecs_id_t,
        POINTER(ecs_id_t),
    ]
    ecs_search.restype = c_int32

# /Users/cnifi/git/pyflecs/flecs.h: 9806
if _libs["libflecs.dylib"].has("ecs_search_offset", "cdecl"):
    ecs_search_offset = _libs["libflecs.dylib"].get("ecs_search_offset", "cdecl")
    ecs_search_offset.argtypes = [
        POINTER(ecs_world_t),
        POINTER(ecs_table_t),
        c_int32,
        ecs_id_t,
        POINTER(ecs_id_t),
    ]
    ecs_search_offset.restype = c_int32

# /Users/cnifi/git/pyflecs/flecs.h: 9855
if _libs["libflecs.dylib"].has("ecs_search_relation", "cdecl"):
    ecs_search_relation = _libs["libflecs.dylib"].get("ecs_search_relation", "cdecl")
    ecs_search_relation.argtypes = [
        POINTER(ecs_world_t),
        POINTER(ecs_table_t),
        c_int32,
        ecs_id_t,
        ecs_entity_t,
        ecs_flags64_t,
        POINTER(ecs_entity_t),
        POINTER(ecs_id_t),
        POINTER(POINTER(struct_ecs_table_record_t)),
    ]
    ecs_search_relation.restype = c_int32

# /Users/cnifi/git/pyflecs/flecs.h: 9874
if _libs["libflecs.dylib"].has("ecs_table_clear_entities", "cdecl"):
    ecs_table_clear_entities = _libs["libflecs.dylib"].get(
        "ecs_table_clear_entities", "cdecl"
    )
    ecs_table_clear_entities.argtypes = [POINTER(ecs_world_t), POINTER(ecs_table_t)]
    ecs_table_clear_entities.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 9895
if _libs["libflecs.dylib"].has("ecs_value_init", "cdecl"):
    ecs_value_init = _libs["libflecs.dylib"].get("ecs_value_init", "cdecl")
    ecs_value_init.argtypes = [POINTER(ecs_world_t), ecs_entity_t, POINTER(None)]
    ecs_value_init.restype = c_int

# /Users/cnifi/git/pyflecs/flecs.h: 9908
if _libs["libflecs.dylib"].has("ecs_value_init_w_type_info", "cdecl"):
    ecs_value_init_w_type_info = _libs["libflecs.dylib"].get(
        "ecs_value_init_w_type_info", "cdecl"
    )
    ecs_value_init_w_type_info.argtypes = [
        POINTER(ecs_world_t),
        POINTER(ecs_type_info_t),
        POINTER(None),
    ]
    ecs_value_init_w_type_info.restype = c_int

# /Users/cnifi/git/pyflecs/flecs.h: 9920
if _libs["libflecs.dylib"].has("ecs_value_new", "cdecl"):
    ecs_value_new = _libs["libflecs.dylib"].get("ecs_value_new", "cdecl")
    ecs_value_new.argtypes = [POINTER(ecs_world_t), ecs_entity_t]
    ecs_value_new.restype = POINTER(c_ubyte)
    ecs_value_new.errcheck = lambda v, *a: cast(v, c_void_p)

# /Users/cnifi/git/pyflecs/flecs.h: 9930
if _libs["libflecs.dylib"].has("ecs_value_new_w_type_info", "cdecl"):
    ecs_value_new_w_type_info = _libs["libflecs.dylib"].get(
        "ecs_value_new_w_type_info", "cdecl"
    )
    ecs_value_new_w_type_info.argtypes = [
        POINTER(ecs_world_t),
        POINTER(ecs_type_info_t),
    ]
    ecs_value_new_w_type_info.restype = POINTER(c_ubyte)
    ecs_value_new_w_type_info.errcheck = lambda v, *a: cast(v, c_void_p)

# /Users/cnifi/git/pyflecs/flecs.h: 9941
if _libs["libflecs.dylib"].has("ecs_value_fini_w_type_info", "cdecl"):
    ecs_value_fini_w_type_info = _libs["libflecs.dylib"].get(
        "ecs_value_fini_w_type_info", "cdecl"
    )
    ecs_value_fini_w_type_info.argtypes = [
        POINTER(ecs_world_t),
        POINTER(ecs_type_info_t),
        POINTER(None),
    ]
    ecs_value_fini_w_type_info.restype = c_int

# /Users/cnifi/git/pyflecs/flecs.h: 9954
if _libs["libflecs.dylib"].has("ecs_value_fini", "cdecl"):
    ecs_value_fini = _libs["libflecs.dylib"].get("ecs_value_fini", "cdecl")
    ecs_value_fini.argtypes = [POINTER(ecs_world_t), ecs_entity_t, POINTER(None)]
    ecs_value_fini.restype = c_int

# /Users/cnifi/git/pyflecs/flecs.h: 9967
if _libs["libflecs.dylib"].has("ecs_value_free", "cdecl"):
    ecs_value_free = _libs["libflecs.dylib"].get("ecs_value_free", "cdecl")
    ecs_value_free.argtypes = [POINTER(ecs_world_t), ecs_entity_t, POINTER(None)]
    ecs_value_free.restype = c_int

# /Users/cnifi/git/pyflecs/flecs.h: 9981
if _libs["libflecs.dylib"].has("ecs_value_copy_w_type_info", "cdecl"):
    ecs_value_copy_w_type_info = _libs["libflecs.dylib"].get(
        "ecs_value_copy_w_type_info", "cdecl"
    )
    ecs_value_copy_w_type_info.argtypes = [
        POINTER(ecs_world_t),
        POINTER(ecs_type_info_t),
        POINTER(None),
        POINTER(None),
    ]
    ecs_value_copy_w_type_info.restype = c_int

# /Users/cnifi/git/pyflecs/flecs.h: 9996
if _libs["libflecs.dylib"].has("ecs_value_copy", "cdecl"):
    ecs_value_copy = _libs["libflecs.dylib"].get("ecs_value_copy", "cdecl")
    ecs_value_copy.argtypes = [
        POINTER(ecs_world_t),
        ecs_entity_t,
        POINTER(None),
        POINTER(None),
    ]
    ecs_value_copy.restype = c_int

# /Users/cnifi/git/pyflecs/flecs.h: 10010
if _libs["libflecs.dylib"].has("ecs_value_move_w_type_info", "cdecl"):
    ecs_value_move_w_type_info = _libs["libflecs.dylib"].get(
        "ecs_value_move_w_type_info", "cdecl"
    )
    ecs_value_move_w_type_info.argtypes = [
        POINTER(ecs_world_t),
        POINTER(ecs_type_info_t),
        POINTER(None),
        POINTER(None),
    ]
    ecs_value_move_w_type_info.restype = c_int

# /Users/cnifi/git/pyflecs/flecs.h: 10024
if _libs["libflecs.dylib"].has("ecs_value_move", "cdecl"):
    ecs_value_move = _libs["libflecs.dylib"].get("ecs_value_move", "cdecl")
    ecs_value_move.argtypes = [
        POINTER(ecs_world_t),
        ecs_entity_t,
        POINTER(None),
        POINTER(None),
    ]
    ecs_value_move.restype = c_int

# /Users/cnifi/git/pyflecs/flecs.h: 10038
if _libs["libflecs.dylib"].has("ecs_value_move_ctor_w_type_info", "cdecl"):
    ecs_value_move_ctor_w_type_info = _libs["libflecs.dylib"].get(
        "ecs_value_move_ctor_w_type_info", "cdecl"
    )
    ecs_value_move_ctor_w_type_info.argtypes = [
        POINTER(ecs_world_t),
        POINTER(ecs_type_info_t),
        POINTER(None),
        POINTER(None),
    ]
    ecs_value_move_ctor_w_type_info.restype = c_int

# /Users/cnifi/git/pyflecs/flecs.h: 10052
if _libs["libflecs.dylib"].has("ecs_value_move_ctor", "cdecl"):
    ecs_value_move_ctor = _libs["libflecs.dylib"].get("ecs_value_move_ctor", "cdecl")
    ecs_value_move_ctor.argtypes = [
        POINTER(ecs_world_t),
        ecs_entity_t,
        POINTER(None),
        POINTER(None),
    ]
    ecs_value_move_ctor.restype = c_int

# /Users/cnifi/git/pyflecs/flecs.h: 11085
if _libs["libflecs.dylib"].has("ecs_deprecated_", "cdecl"):
    ecs_deprecated_ = _libs["libflecs.dylib"].get("ecs_deprecated_", "cdecl")
    ecs_deprecated_.argtypes = [String, c_int32, String]
    ecs_deprecated_.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 11097
if _libs["libflecs.dylib"].has("ecs_log_push_", "cdecl"):
    ecs_log_push_ = _libs["libflecs.dylib"].get("ecs_log_push_", "cdecl")
    ecs_log_push_.argtypes = [c_int32]
    ecs_log_push_.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 11106
if _libs["libflecs.dylib"].has("ecs_log_pop_", "cdecl"):
    ecs_log_pop_ = _libs["libflecs.dylib"].get("ecs_log_pop_", "cdecl")
    ecs_log_pop_.argtypes = [c_int32]
    ecs_log_pop_.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 11116
if _libs["libflecs.dylib"].has("ecs_should_log", "cdecl"):
    ecs_should_log = _libs["libflecs.dylib"].get("ecs_should_log", "cdecl")
    ecs_should_log.argtypes = [c_int32]
    ecs_should_log.restype = c_bool

# /Users/cnifi/git/pyflecs/flecs.h: 11124
if _libs["libflecs.dylib"].has("ecs_strerror", "cdecl"):
    ecs_strerror = _libs["libflecs.dylib"].get("ecs_strerror", "cdecl")
    ecs_strerror.argtypes = [c_int32]
    ecs_strerror.restype = c_char_p

# /Users/cnifi/git/pyflecs/flecs.h: 11153
if _libs["libflecs.dylib"].has("ecs_print_", "cdecl"):
    _func = _libs["libflecs.dylib"].get("ecs_print_", "cdecl")
    _restype = None
    _errcheck = None
    _argtypes = [c_int32, String, c_int32, String]
    ecs_print_ = _variadic_function(_func, _restype, _argtypes, _errcheck)

# /Users/cnifi/git/pyflecs/flecs.h: 11161
if _libs["libflecs.dylib"].has("ecs_printv_", "cdecl"):
    ecs_printv_ = _libs["libflecs.dylib"].get("ecs_printv_", "cdecl")
    ecs_printv_.argtypes = [c_int, String, c_int32, String, c_void_p]
    ecs_printv_.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 11169
if _libs["libflecs.dylib"].has("ecs_log_", "cdecl"):
    _func = _libs["libflecs.dylib"].get("ecs_log_", "cdecl")
    _restype = None
    _errcheck = None
    _argtypes = [c_int32, String, c_int32, String]
    ecs_log_ = _variadic_function(_func, _restype, _argtypes, _errcheck)

# /Users/cnifi/git/pyflecs/flecs.h: 11177
if _libs["libflecs.dylib"].has("ecs_logv_", "cdecl"):
    ecs_logv_ = _libs["libflecs.dylib"].get("ecs_logv_", "cdecl")
    ecs_logv_.argtypes = [c_int, String, c_int32, String, c_void_p]
    ecs_logv_.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 11185
if _libs["libflecs.dylib"].has("ecs_abort_", "cdecl"):
    _func = _libs["libflecs.dylib"].get("ecs_abort_", "cdecl")
    _restype = None
    _errcheck = None
    _argtypes = [c_int32, String, c_int32, String]
    ecs_abort_ = _variadic_function(_func, _restype, _argtypes, _errcheck)

# /Users/cnifi/git/pyflecs/flecs.h: 11193
if _libs["libflecs.dylib"].has("ecs_assert_log_", "cdecl"):
    _func = _libs["libflecs.dylib"].get("ecs_assert_log_", "cdecl")
    _restype = None
    _errcheck = None
    _argtypes = [c_int32, String, String, c_int32, String]
    ecs_assert_log_ = _variadic_function(_func, _restype, _argtypes, _errcheck)

# /Users/cnifi/git/pyflecs/flecs.h: 11202
if _libs["libflecs.dylib"].has("ecs_parser_error_", "cdecl"):
    _func = _libs["libflecs.dylib"].get("ecs_parser_error_", "cdecl")
    _restype = None
    _errcheck = None
    _argtypes = [String, String, c_int64, String]
    ecs_parser_error_ = _variadic_function(_func, _restype, _argtypes, _errcheck)

# /Users/cnifi/git/pyflecs/flecs.h: 11210
if _libs["libflecs.dylib"].has("ecs_parser_errorv_", "cdecl"):
    ecs_parser_errorv_ = _libs["libflecs.dylib"].get("ecs_parser_errorv_", "cdecl")
    ecs_parser_errorv_.argtypes = [String, String, c_int64, String, c_void_p]
    ecs_parser_errorv_.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 11218
if _libs["libflecs.dylib"].has("ecs_parser_warning_", "cdecl"):
    _func = _libs["libflecs.dylib"].get("ecs_parser_warning_", "cdecl")
    _restype = None
    _errcheck = None
    _argtypes = [String, String, c_int64, String]
    ecs_parser_warning_ = _variadic_function(_func, _restype, _argtypes, _errcheck)

# /Users/cnifi/git/pyflecs/flecs.h: 11226
if _libs["libflecs.dylib"].has("ecs_parser_warningv_", "cdecl"):
    ecs_parser_warningv_ = _libs["libflecs.dylib"].get("ecs_parser_warningv_", "cdecl")
    ecs_parser_warningv_.argtypes = [String, String, c_int64, String, c_void_p]
    ecs_parser_warningv_.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 11510
if _libs["libflecs.dylib"].has("ecs_log_set_level", "cdecl"):
    ecs_log_set_level = _libs["libflecs.dylib"].get("ecs_log_set_level", "cdecl")
    ecs_log_set_level.argtypes = [c_int]
    ecs_log_set_level.restype = c_int

# /Users/cnifi/git/pyflecs/flecs.h: 11518
if _libs["libflecs.dylib"].has("ecs_log_get_level", "cdecl"):
    ecs_log_get_level = _libs["libflecs.dylib"].get("ecs_log_get_level", "cdecl")
    ecs_log_get_level.argtypes = []
    ecs_log_get_level.restype = c_int

# /Users/cnifi/git/pyflecs/flecs.h: 11527
if _libs["libflecs.dylib"].has("ecs_log_enable_colors", "cdecl"):
    ecs_log_enable_colors = _libs["libflecs.dylib"].get(
        "ecs_log_enable_colors", "cdecl"
    )
    ecs_log_enable_colors.argtypes = [c_bool]
    ecs_log_enable_colors.restype = c_bool

# /Users/cnifi/git/pyflecs/flecs.h: 11538
if _libs["libflecs.dylib"].has("ecs_log_enable_timestamp", "cdecl"):
    ecs_log_enable_timestamp = _libs["libflecs.dylib"].get(
        "ecs_log_enable_timestamp", "cdecl"
    )
    ecs_log_enable_timestamp.argtypes = [c_bool]
    ecs_log_enable_timestamp.restype = c_bool

# /Users/cnifi/git/pyflecs/flecs.h: 11555
if _libs["libflecs.dylib"].has("ecs_log_enable_timedelta", "cdecl"):
    ecs_log_enable_timedelta = _libs["libflecs.dylib"].get(
        "ecs_log_enable_timedelta", "cdecl"
    )
    ecs_log_enable_timedelta.argtypes = [c_bool]
    ecs_log_enable_timedelta.restype = c_bool

# /Users/cnifi/git/pyflecs/flecs.h: 11564
if _libs["libflecs.dylib"].has("ecs_log_last_error", "cdecl"):
    ecs_log_last_error = _libs["libflecs.dylib"].get("ecs_log_last_error", "cdecl")
    ecs_log_last_error.argtypes = []
    ecs_log_last_error.restype = c_int

ecs_app_init_action_t = CFUNCTYPE(
    UNCHECKED(c_int), POINTER(ecs_world_t)
)  # /Users/cnifi/git/pyflecs/flecs.h: 11687


# /Users/cnifi/git/pyflecs/flecs.h: 11704
class struct_ecs_app_desc_t(Structure):
    pass


struct_ecs_app_desc_t.__slots__ = [
    "target_fps",
    "delta_time",
    "threads",
    "frames",
    "enable_rest",
    "enable_stats",
    "port",
    "init",
    "ctx",
]
struct_ecs_app_desc_t._fields_ = [
    ("target_fps", c_float),
    ("delta_time", c_float),
    ("threads", c_int32),
    ("frames", c_int32),
    ("enable_rest", c_bool),
    ("enable_stats", c_bool),
    ("port", uint16_t),
    ("init", ecs_app_init_action_t),
    ("ctx", POINTER(None)),
]

ecs_app_desc_t = struct_ecs_app_desc_t  # /Users/cnifi/git/pyflecs/flecs.h: 11704

ecs_app_run_action_t = CFUNCTYPE(
    UNCHECKED(c_int), POINTER(ecs_world_t), POINTER(ecs_app_desc_t)
)  # /Users/cnifi/git/pyflecs/flecs.h: 11707

ecs_app_frame_action_t = CFUNCTYPE(
    UNCHECKED(c_int), POINTER(ecs_world_t), POINTER(ecs_app_desc_t)
)  # /Users/cnifi/git/pyflecs/flecs.h: 11712

# /Users/cnifi/git/pyflecs/flecs.h: 11728
if _libs["libflecs.dylib"].has("ecs_app_run", "cdecl"):
    ecs_app_run = _libs["libflecs.dylib"].get("ecs_app_run", "cdecl")
    ecs_app_run.argtypes = [POINTER(ecs_world_t), POINTER(ecs_app_desc_t)]
    ecs_app_run.restype = c_int

# /Users/cnifi/git/pyflecs/flecs.h: 11741
if _libs["libflecs.dylib"].has("ecs_app_run_frame", "cdecl"):
    ecs_app_run_frame = _libs["libflecs.dylib"].get("ecs_app_run_frame", "cdecl")
    ecs_app_run_frame.argtypes = [POINTER(ecs_world_t), POINTER(ecs_app_desc_t)]
    ecs_app_run_frame.restype = c_int

# /Users/cnifi/git/pyflecs/flecs.h: 11751
if _libs["libflecs.dylib"].has("ecs_app_set_run_action", "cdecl"):
    ecs_app_set_run_action = _libs["libflecs.dylib"].get(
        "ecs_app_set_run_action", "cdecl"
    )
    ecs_app_set_run_action.argtypes = [ecs_app_run_action_t]
    ecs_app_set_run_action.restype = c_int

# /Users/cnifi/git/pyflecs/flecs.h: 11760
if _libs["libflecs.dylib"].has("ecs_app_set_frame_action", "cdecl"):
    ecs_app_set_frame_action = _libs["libflecs.dylib"].get(
        "ecs_app_set_frame_action", "cdecl"
    )
    ecs_app_set_frame_action.argtypes = [ecs_app_frame_action_t]
    ecs_app_set_frame_action.restype = c_int


# /Users/cnifi/git/pyflecs/flecs.h: 11826
class struct_ecs_http_server_t(Structure):
    pass


ecs_http_server_t = struct_ecs_http_server_t  # /Users/cnifi/git/pyflecs/flecs.h: 11826


# /Users/cnifi/git/pyflecs/flecs.h: 11835
class struct_anon_15(Structure):
    pass


struct_anon_15.__slots__ = [
    "id",
    "server",
    "host",
    "port",
]
struct_anon_15._fields_ = [
    ("id", uint64_t),
    ("server", POINTER(ecs_http_server_t)),
    ("host", c_char * int(128)),
    ("port", c_char * int(16)),
]

ecs_http_connection_t = struct_anon_15  # /Users/cnifi/git/pyflecs/flecs.h: 11835


# /Users/cnifi/git/pyflecs/flecs.h: 11841
class struct_anon_16(Structure):
    pass


struct_anon_16.__slots__ = [
    "key",
    "value",
]
struct_anon_16._fields_ = [
    ("key", String),
    ("value", String),
]

ecs_http_key_value_t = struct_anon_16  # /Users/cnifi/git/pyflecs/flecs.h: 11841

enum_anon_17 = c_int  # /Users/cnifi/git/pyflecs/flecs.h: 11851

EcsHttpGet = 0  # /Users/cnifi/git/pyflecs/flecs.h: 11851

EcsHttpPost = EcsHttpGet + 1  # /Users/cnifi/git/pyflecs/flecs.h: 11851

EcsHttpPut = EcsHttpPost + 1  # /Users/cnifi/git/pyflecs/flecs.h: 11851

EcsHttpDelete = EcsHttpPut + 1  # /Users/cnifi/git/pyflecs/flecs.h: 11851

EcsHttpOptions = EcsHttpDelete + 1  # /Users/cnifi/git/pyflecs/flecs.h: 11851

EcsHttpMethodUnsupported = EcsHttpOptions + 1  # /Users/cnifi/git/pyflecs/flecs.h: 11851

ecs_http_method_t = enum_anon_17  # /Users/cnifi/git/pyflecs/flecs.h: 11851


# /Users/cnifi/git/pyflecs/flecs.h: 11866
class struct_anon_18(Structure):
    pass


struct_anon_18.__slots__ = [
    "id",
    "method",
    "path",
    "body",
    "headers",
    "params",
    "header_count",
    "param_count",
    "conn",
]
struct_anon_18._fields_ = [
    ("id", uint64_t),
    ("method", ecs_http_method_t),
    ("path", String),
    ("body", String),
    ("headers", ecs_http_key_value_t * int(32)),
    ("params", ecs_http_key_value_t * int(32)),
    ("header_count", c_int32),
    ("param_count", c_int32),
    ("conn", POINTER(ecs_http_connection_t)),
]

ecs_http_request_t = struct_anon_18  # /Users/cnifi/git/pyflecs/flecs.h: 11866


# /Users/cnifi/git/pyflecs/flecs.h: 11875
class struct_anon_19(Structure):
    pass


struct_anon_19.__slots__ = [
    "code",
    "body",
    "status",
    "content_type",
    "headers",
]
struct_anon_19._fields_ = [
    ("code", c_int),
    ("body", ecs_strbuf_t),
    ("status", String),
    ("content_type", String),
    ("headers", ecs_strbuf_t),
]

ecs_http_reply_t = struct_anon_19  # /Users/cnifi/git/pyflecs/flecs.h: 11875

# /Users/cnifi/git/pyflecs/flecs.h: 11881
try:
    ecs_http_request_received_count = (c_int64).in_dll(
        _libs["libflecs.dylib"], "ecs_http_request_received_count"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 11882
try:
    ecs_http_request_invalid_count = (c_int64).in_dll(
        _libs["libflecs.dylib"], "ecs_http_request_invalid_count"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 11883
try:
    ecs_http_request_handled_ok_count = (c_int64).in_dll(
        _libs["libflecs.dylib"], "ecs_http_request_handled_ok_count"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 11884
try:
    ecs_http_request_handled_error_count = (c_int64).in_dll(
        _libs["libflecs.dylib"], "ecs_http_request_handled_error_count"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 11885
try:
    ecs_http_request_not_handled_count = (c_int64).in_dll(
        _libs["libflecs.dylib"], "ecs_http_request_not_handled_count"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 11886
try:
    ecs_http_request_preflight_count = (c_int64).in_dll(
        _libs["libflecs.dylib"], "ecs_http_request_preflight_count"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 11887
try:
    ecs_http_send_ok_count = (c_int64).in_dll(
        _libs["libflecs.dylib"], "ecs_http_send_ok_count"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 11888
try:
    ecs_http_send_error_count = (c_int64).in_dll(
        _libs["libflecs.dylib"], "ecs_http_send_error_count"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 11889
try:
    ecs_http_busy_count = (c_int64).in_dll(
        _libs["libflecs.dylib"], "ecs_http_busy_count"
    )
except:
    pass

ecs_http_reply_action_t = CFUNCTYPE(
    UNCHECKED(c_bool),
    POINTER(ecs_http_request_t),
    POINTER(ecs_http_reply_t),
    POINTER(None),
)  # /Users/cnifi/git/pyflecs/flecs.h: 11895


# /Users/cnifi/git/pyflecs/flecs.h: 11909
class struct_anon_20(Structure):
    pass


struct_anon_20.__slots__ = [
    "callback",
    "ctx",
    "port",
    "ipaddr",
    "send_queue_wait_ms",
    "cache_timeout",
    "cache_purge_timeout",
]
struct_anon_20._fields_ = [
    ("callback", ecs_http_reply_action_t),
    ("ctx", POINTER(None)),
    ("port", uint16_t),
    ("ipaddr", String),
    ("send_queue_wait_ms", c_int32),
    ("cache_timeout", c_double),
    ("cache_purge_timeout", c_double),
]

ecs_http_server_desc_t = struct_anon_20  # /Users/cnifi/git/pyflecs/flecs.h: 11909

# /Users/cnifi/git/pyflecs/flecs.h: 11918
if _libs["libflecs.dylib"].has("ecs_http_server_init", "cdecl"):
    ecs_http_server_init = _libs["libflecs.dylib"].get("ecs_http_server_init", "cdecl")
    ecs_http_server_init.argtypes = [POINTER(ecs_http_server_desc_t)]
    ecs_http_server_init.restype = POINTER(ecs_http_server_t)

# /Users/cnifi/git/pyflecs/flecs.h: 11927
if _libs["libflecs.dylib"].has("ecs_http_server_fini", "cdecl"):
    ecs_http_server_fini = _libs["libflecs.dylib"].get("ecs_http_server_fini", "cdecl")
    ecs_http_server_fini.argtypes = [POINTER(ecs_http_server_t)]
    ecs_http_server_fini.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 11937
if _libs["libflecs.dylib"].has("ecs_http_server_start", "cdecl"):
    ecs_http_server_start = _libs["libflecs.dylib"].get(
        "ecs_http_server_start", "cdecl"
    )
    ecs_http_server_start.argtypes = [POINTER(ecs_http_server_t)]
    ecs_http_server_start.restype = c_int

# /Users/cnifi/git/pyflecs/flecs.h: 11947
if _libs["libflecs.dylib"].has("ecs_http_server_dequeue", "cdecl"):
    ecs_http_server_dequeue = _libs["libflecs.dylib"].get(
        "ecs_http_server_dequeue", "cdecl"
    )
    ecs_http_server_dequeue.argtypes = [POINTER(ecs_http_server_t), c_float]
    ecs_http_server_dequeue.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 11957
if _libs["libflecs.dylib"].has("ecs_http_server_stop", "cdecl"):
    ecs_http_server_stop = _libs["libflecs.dylib"].get("ecs_http_server_stop", "cdecl")
    ecs_http_server_stop.argtypes = [POINTER(ecs_http_server_t)]
    ecs_http_server_stop.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 11971
if _libs["libflecs.dylib"].has("ecs_http_server_http_request", "cdecl"):
    ecs_http_server_http_request = _libs["libflecs.dylib"].get(
        "ecs_http_server_http_request", "cdecl"
    )
    ecs_http_server_http_request.argtypes = [
        POINTER(ecs_http_server_t),
        String,
        ecs_size_t,
        POINTER(ecs_http_reply_t),
    ]
    ecs_http_server_http_request.restype = c_int

# /Users/cnifi/git/pyflecs/flecs.h: 11979
if _libs["libflecs.dylib"].has("ecs_http_server_request", "cdecl"):
    ecs_http_server_request = _libs["libflecs.dylib"].get(
        "ecs_http_server_request", "cdecl"
    )
    ecs_http_server_request.argtypes = [
        POINTER(ecs_http_server_t),
        String,
        String,
        String,
        POINTER(ecs_http_reply_t),
    ]
    ecs_http_server_request.restype = c_int

# /Users/cnifi/git/pyflecs/flecs.h: 11988
if _libs["libflecs.dylib"].has("ecs_http_server_ctx", "cdecl"):
    ecs_http_server_ctx = _libs["libflecs.dylib"].get("ecs_http_server_ctx", "cdecl")
    ecs_http_server_ctx.argtypes = [POINTER(ecs_http_server_t)]
    ecs_http_server_ctx.restype = POINTER(c_ubyte)
    ecs_http_server_ctx.errcheck = lambda v, *a: cast(v, c_void_p)

# /Users/cnifi/git/pyflecs/flecs.h: 11998
if _libs["libflecs.dylib"].has("ecs_http_get_header", "cdecl"):
    ecs_http_get_header = _libs["libflecs.dylib"].get("ecs_http_get_header", "cdecl")
    ecs_http_get_header.argtypes = [POINTER(ecs_http_request_t), String]
    ecs_http_get_header.restype = c_char_p

# /Users/cnifi/git/pyflecs/flecs.h: 12009
if _libs["libflecs.dylib"].has("ecs_http_get_param", "cdecl"):
    ecs_http_get_param = _libs["libflecs.dylib"].get("ecs_http_get_param", "cdecl")
    ecs_http_get_param.argtypes = [POINTER(ecs_http_request_t), String]
    ecs_http_get_param.restype = c_char_p

# /Users/cnifi/git/pyflecs/flecs.h: 12074
try:
    FLECS_IDEcsRestID_ = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "FLECS_IDEcsRestID_"
    )
except:
    pass


# /Users/cnifi/git/pyflecs/flecs.h: 12081
class struct_anon_21(Structure):
    pass


struct_anon_21.__slots__ = [
    "port",
    "ipaddr",
    "impl",
]
struct_anon_21._fields_ = [
    ("port", uint16_t),
    ("ipaddr", String),
    ("impl", POINTER(None)),
]

EcsRest = struct_anon_21  # /Users/cnifi/git/pyflecs/flecs.h: 12081

# /Users/cnifi/git/pyflecs/flecs.h: 12092
if _libs["libflecs.dylib"].has("ecs_rest_server_init", "cdecl"):
    ecs_rest_server_init = _libs["libflecs.dylib"].get("ecs_rest_server_init", "cdecl")
    ecs_rest_server_init.argtypes = [
        POINTER(ecs_world_t),
        POINTER(ecs_http_server_desc_t),
    ]
    ecs_rest_server_init.restype = POINTER(ecs_http_server_t)

# /Users/cnifi/git/pyflecs/flecs.h: 12100
if _libs["libflecs.dylib"].has("ecs_rest_server_fini", "cdecl"):
    ecs_rest_server_fini = _libs["libflecs.dylib"].get("ecs_rest_server_fini", "cdecl")
    ecs_rest_server_fini.argtypes = [POINTER(ecs_http_server_t)]
    ecs_rest_server_fini.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 12112
if _libs["libflecs.dylib"].has("FlecsRestImport", "cdecl"):
    FlecsRestImport = _libs["libflecs.dylib"].get("FlecsRestImport", "cdecl")
    FlecsRestImport.argtypes = [POINTER(ecs_world_t)]
    FlecsRestImport.restype = None


# /Users/cnifi/git/pyflecs/flecs.h: 12172
class struct_EcsTimer(Structure):
    pass


struct_EcsTimer.__slots__ = [
    "timeout",
    "time",
    "overshoot",
    "fired_count",
    "active",
    "single_shot",
]
struct_EcsTimer._fields_ = [
    ("timeout", c_float),
    ("time", c_float),
    ("overshoot", c_float),
    ("fired_count", c_int32),
    ("active", c_bool),
    ("single_shot", c_bool),
]

EcsTimer = struct_EcsTimer  # /Users/cnifi/git/pyflecs/flecs.h: 12172


# /Users/cnifi/git/pyflecs/flecs.h: 12180
class struct_EcsRateFilter(Structure):
    pass


struct_EcsRateFilter.__slots__ = [
    "src",
    "rate",
    "tick_count",
    "time_elapsed",
]
struct_EcsRateFilter._fields_ = [
    ("src", ecs_entity_t),
    ("rate", c_int32),
    ("tick_count", c_int32),
    ("time_elapsed", c_float),
]

EcsRateFilter = struct_EcsRateFilter  # /Users/cnifi/git/pyflecs/flecs.h: 12180

# /Users/cnifi/git/pyflecs/flecs.h: 12202
if _libs["libflecs.dylib"].has("ecs_set_timeout", "cdecl"):
    ecs_set_timeout = _libs["libflecs.dylib"].get("ecs_set_timeout", "cdecl")
    ecs_set_timeout.argtypes = [POINTER(ecs_world_t), ecs_entity_t, c_float]
    ecs_set_timeout.restype = ecs_entity_t

# /Users/cnifi/git/pyflecs/flecs.h: 12227
if _libs["libflecs.dylib"].has("ecs_get_timeout", "cdecl"):
    ecs_get_timeout = _libs["libflecs.dylib"].get("ecs_get_timeout", "cdecl")
    ecs_get_timeout.argtypes = [POINTER(ecs_world_t), ecs_entity_t]
    ecs_get_timeout.restype = c_float

# /Users/cnifi/git/pyflecs/flecs.h: 12249
if _libs["libflecs.dylib"].has("ecs_set_interval", "cdecl"):
    ecs_set_interval = _libs["libflecs.dylib"].get("ecs_set_interval", "cdecl")
    ecs_set_interval.argtypes = [POINTER(ecs_world_t), ecs_entity_t, c_float]
    ecs_set_interval.restype = ecs_entity_t

# /Users/cnifi/git/pyflecs/flecs.h: 12263
if _libs["libflecs.dylib"].has("ecs_get_interval", "cdecl"):
    ecs_get_interval = _libs["libflecs.dylib"].get("ecs_get_interval", "cdecl")
    ecs_get_interval.argtypes = [POINTER(ecs_world_t), ecs_entity_t]
    ecs_get_interval.restype = c_float

# /Users/cnifi/git/pyflecs/flecs.h: 12274
if _libs["libflecs.dylib"].has("ecs_start_timer", "cdecl"):
    ecs_start_timer = _libs["libflecs.dylib"].get("ecs_start_timer", "cdecl")
    ecs_start_timer.argtypes = [POINTER(ecs_world_t), ecs_entity_t]
    ecs_start_timer.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 12285
if _libs["libflecs.dylib"].has("ecs_stop_timer", "cdecl"):
    ecs_stop_timer = _libs["libflecs.dylib"].get("ecs_stop_timer", "cdecl")
    ecs_stop_timer.argtypes = [POINTER(ecs_world_t), ecs_entity_t]
    ecs_stop_timer.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 12296
if _libs["libflecs.dylib"].has("ecs_reset_timer", "cdecl"):
    ecs_reset_timer = _libs["libflecs.dylib"].get("ecs_reset_timer", "cdecl")
    ecs_reset_timer.argtypes = [POINTER(ecs_world_t), ecs_entity_t]
    ecs_reset_timer.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 12307
if _libs["libflecs.dylib"].has("ecs_randomize_timers", "cdecl"):
    ecs_randomize_timers = _libs["libflecs.dylib"].get("ecs_randomize_timers", "cdecl")
    ecs_randomize_timers.argtypes = [POINTER(ecs_world_t)]
    ecs_randomize_timers.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 12341
if _libs["libflecs.dylib"].has("ecs_set_rate", "cdecl"):
    ecs_set_rate = _libs["libflecs.dylib"].get("ecs_set_rate", "cdecl")
    ecs_set_rate.argtypes = [POINTER(ecs_world_t), ecs_entity_t, c_int32, ecs_entity_t]
    ecs_set_rate.restype = ecs_entity_t

# /Users/cnifi/git/pyflecs/flecs.h: 12370
if _libs["libflecs.dylib"].has("ecs_set_tick_source", "cdecl"):
    ecs_set_tick_source = _libs["libflecs.dylib"].get("ecs_set_tick_source", "cdecl")
    ecs_set_tick_source.argtypes = [POINTER(ecs_world_t), ecs_entity_t, ecs_entity_t]
    ecs_set_tick_source.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 12389
if _libs["libflecs.dylib"].has("FlecsTimerImport", "cdecl"):
    FlecsTimerImport = _libs["libflecs.dylib"].get("FlecsTimerImport", "cdecl")
    FlecsTimerImport.argtypes = [POINTER(ecs_world_t)]
    FlecsTimerImport.restype = None


# /Users/cnifi/git/pyflecs/flecs.h: 12523
class struct_ecs_pipeline_desc_t(Structure):
    pass


struct_ecs_pipeline_desc_t.__slots__ = [
    "entity",
    "query",
]
struct_ecs_pipeline_desc_t._fields_ = [
    ("entity", ecs_entity_t),
    ("query", ecs_query_desc_t),
]

ecs_pipeline_desc_t = (
    struct_ecs_pipeline_desc_t  # /Users/cnifi/git/pyflecs/flecs.h: 12523
)

# /Users/cnifi/git/pyflecs/flecs.h: 12532
if _libs["libflecs.dylib"].has("ecs_pipeline_init", "cdecl"):
    ecs_pipeline_init = _libs["libflecs.dylib"].get("ecs_pipeline_init", "cdecl")
    ecs_pipeline_init.argtypes = [POINTER(ecs_world_t), POINTER(ecs_pipeline_desc_t)]
    ecs_pipeline_init.restype = ecs_entity_t

# /Users/cnifi/git/pyflecs/flecs.h: 12543
if _libs["libflecs.dylib"].has("ecs_set_pipeline", "cdecl"):
    ecs_set_pipeline = _libs["libflecs.dylib"].get("ecs_set_pipeline", "cdecl")
    ecs_set_pipeline.argtypes = [POINTER(ecs_world_t), ecs_entity_t]
    ecs_set_pipeline.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 12554
if _libs["libflecs.dylib"].has("ecs_get_pipeline", "cdecl"):
    ecs_get_pipeline = _libs["libflecs.dylib"].get("ecs_get_pipeline", "cdecl")
    ecs_get_pipeline.argtypes = [POINTER(ecs_world_t)]
    ecs_get_pipeline.restype = ecs_entity_t

# /Users/cnifi/git/pyflecs/flecs.h: 12576
if _libs["libflecs.dylib"].has("ecs_progress", "cdecl"):
    ecs_progress = _libs["libflecs.dylib"].get("ecs_progress", "cdecl")
    ecs_progress.argtypes = [POINTER(ecs_world_t), c_float]
    ecs_progress.restype = c_bool

# /Users/cnifi/git/pyflecs/flecs.h: 12587
if _libs["libflecs.dylib"].has("ecs_set_time_scale", "cdecl"):
    ecs_set_time_scale = _libs["libflecs.dylib"].get("ecs_set_time_scale", "cdecl")
    ecs_set_time_scale.argtypes = [POINTER(ecs_world_t), c_float]
    ecs_set_time_scale.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 12597
if _libs["libflecs.dylib"].has("ecs_reset_clock", "cdecl"):
    ecs_reset_clock = _libs["libflecs.dylib"].get("ecs_reset_clock", "cdecl")
    ecs_reset_clock.argtypes = [POINTER(ecs_world_t)]
    ecs_reset_clock.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 12617
if _libs["libflecs.dylib"].has("ecs_run_pipeline", "cdecl"):
    ecs_run_pipeline = _libs["libflecs.dylib"].get("ecs_run_pipeline", "cdecl")
    ecs_run_pipeline.argtypes = [POINTER(ecs_world_t), ecs_entity_t, c_float]
    ecs_run_pipeline.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 12639
if _libs["libflecs.dylib"].has("ecs_set_threads", "cdecl"):
    ecs_set_threads = _libs["libflecs.dylib"].get("ecs_set_threads", "cdecl")
    ecs_set_threads.argtypes = [POINTER(ecs_world_t), c_int32]
    ecs_set_threads.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 12660
if _libs["libflecs.dylib"].has("ecs_set_task_threads", "cdecl"):
    ecs_set_task_threads = _libs["libflecs.dylib"].get("ecs_set_task_threads", "cdecl")
    ecs_set_task_threads.argtypes = [POINTER(ecs_world_t), c_int32]
    ecs_set_task_threads.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 12670
if _libs["libflecs.dylib"].has("ecs_using_task_threads", "cdecl"):
    ecs_using_task_threads = _libs["libflecs.dylib"].get(
        "ecs_using_task_threads", "cdecl"
    )
    ecs_using_task_threads.argtypes = [POINTER(ecs_world_t)]
    ecs_using_task_threads.restype = c_bool

# /Users/cnifi/git/pyflecs/flecs.h: 12686
if _libs["libflecs.dylib"].has("FlecsPipelineImport", "cdecl"):
    FlecsPipelineImport = _libs["libflecs.dylib"].get("FlecsPipelineImport", "cdecl")
    FlecsPipelineImport.argtypes = [POINTER(ecs_world_t)]
    FlecsPipelineImport.restype = None


# /Users/cnifi/git/pyflecs/flecs.h: 12739
class struct_EcsTickSource(Structure):
    pass


struct_EcsTickSource.__slots__ = [
    "tick",
    "time_elapsed",
]
struct_EcsTickSource._fields_ = [
    ("tick", c_bool),
    ("time_elapsed", c_float),
]

EcsTickSource = struct_EcsTickSource  # /Users/cnifi/git/pyflecs/flecs.h: 12739


# /Users/cnifi/git/pyflecs/flecs.h: 12802
class struct_ecs_system_desc_t(Structure):
    pass


struct_ecs_system_desc_t.__slots__ = [
    "_canary",
    "entity",
    "query",
    "callback",
    "run",
    "ctx",
    "ctx_free",
    "callback_ctx",
    "callback_ctx_free",
    "run_ctx",
    "run_ctx_free",
    "interval",
    "rate",
    "tick_source",
    "multi_threaded",
    "immediate",
]
struct_ecs_system_desc_t._fields_ = [
    ("_canary", c_int32),
    ("entity", ecs_entity_t),
    ("query", ecs_query_desc_t),
    ("callback", ecs_iter_action_t),
    ("run", ecs_run_action_t),
    ("ctx", POINTER(None)),
    ("ctx_free", ecs_ctx_free_t),
    ("callback_ctx", POINTER(None)),
    ("callback_ctx_free", ecs_ctx_free_t),
    ("run_ctx", POINTER(None)),
    ("run_ctx_free", ecs_ctx_free_t),
    ("interval", c_float),
    ("rate", c_int32),
    ("tick_source", ecs_entity_t),
    ("multi_threaded", c_bool),
    ("immediate", c_bool),
]

ecs_system_desc_t = struct_ecs_system_desc_t  # /Users/cnifi/git/pyflecs/flecs.h: 12802

# /Users/cnifi/git/pyflecs/flecs.h: 12806
if _libs["libflecs.dylib"].has("ecs_system_init", "cdecl"):
    ecs_system_init = _libs["libflecs.dylib"].get("ecs_system_init", "cdecl")
    ecs_system_init.argtypes = [POINTER(ecs_world_t), POINTER(ecs_system_desc_t)]
    ecs_system_init.restype = ecs_entity_t


# /Users/cnifi/git/pyflecs/flecs.h: 12864
class struct_ecs_system_t(Structure):
    pass


struct_ecs_system_t.__slots__ = [
    "hdr",
    "run",
    "action",
    "query",
    "tick_source",
    "multi_threaded",
    "immediate",
    "name",
    "ctx",
    "callback_ctx",
    "run_ctx",
    "ctx_free",
    "callback_ctx_free",
    "run_ctx_free",
    "time_spent",
    "time_passed",
    "last_frame",
    "dtor",
]
struct_ecs_system_t._fields_ = [
    ("hdr", ecs_header_t),
    ("run", ecs_run_action_t),
    ("action", ecs_iter_action_t),
    ("query", POINTER(ecs_query_t)),
    ("tick_source", ecs_entity_t),
    ("multi_threaded", c_bool),
    ("immediate", c_bool),
    ("name", String),
    ("ctx", POINTER(None)),
    ("callback_ctx", POINTER(None)),
    ("run_ctx", POINTER(None)),
    ("ctx_free", ecs_ctx_free_t),
    ("callback_ctx_free", ecs_ctx_free_t),
    ("run_ctx_free", ecs_ctx_free_t),
    ("time_spent", c_float),
    ("time_passed", c_float),
    ("last_frame", c_int64),
    ("dtor", flecs_poly_dtor_t),
]

ecs_system_t = struct_ecs_system_t  # /Users/cnifi/git/pyflecs/flecs.h: 12864

# /Users/cnifi/git/pyflecs/flecs.h: 12875
if _libs["libflecs.dylib"].has("ecs_system_get", "cdecl"):
    ecs_system_get = _libs["libflecs.dylib"].get("ecs_system_get", "cdecl")
    ecs_system_get.argtypes = [POINTER(ecs_world_t), ecs_entity_t]
    ecs_system_get.restype = POINTER(ecs_system_t)

# /Users/cnifi/git/pyflecs/flecs.h: 12976
if _libs["libflecs.dylib"].has("ecs_run", "cdecl"):
    ecs_run = _libs["libflecs.dylib"].get("ecs_run", "cdecl")
    ecs_run.argtypes = [POINTER(ecs_world_t), ecs_entity_t, c_float, POINTER(None)]
    ecs_run.restype = ecs_entity_t

# /Users/cnifi/git/pyflecs/flecs.h: 12993
if _libs["libflecs.dylib"].has("ecs_run_worker", "cdecl"):
    ecs_run_worker = _libs["libflecs.dylib"].get("ecs_run_worker", "cdecl")
    ecs_run_worker.argtypes = [
        POINTER(ecs_world_t),
        ecs_entity_t,
        c_int32,
        c_int32,
        c_float,
        POINTER(None),
    ]
    ecs_run_worker.restype = ecs_entity_t

# /Users/cnifi/git/pyflecs/flecs.h: 13010
if _libs["libflecs.dylib"].has("FlecsSystemImport", "cdecl"):
    FlecsSystemImport = _libs["libflecs.dylib"].get("FlecsSystemImport", "cdecl")
    FlecsSystemImport.argtypes = [POINTER(ecs_world_t)]
    FlecsSystemImport.restype = None


# /Users/cnifi/git/pyflecs/flecs.h: 13075
class struct_ecs_gauge_t(Structure):
    pass


struct_ecs_gauge_t.__slots__ = [
    "avg",
    "min",
    "max",
]
struct_ecs_gauge_t._fields_ = [
    ("avg", c_float * int(60)),
    ("min", c_float * int(60)),
    ("max", c_float * int(60)),
]

ecs_gauge_t = struct_ecs_gauge_t  # /Users/cnifi/git/pyflecs/flecs.h: 13075


# /Users/cnifi/git/pyflecs/flecs.h: 13081
class struct_ecs_counter_t(Structure):
    pass


struct_ecs_counter_t.__slots__ = [
    "rate",
    "value",
]
struct_ecs_counter_t._fields_ = [
    ("rate", ecs_gauge_t),
    ("value", c_double * int(60)),
]

ecs_counter_t = struct_ecs_counter_t  # /Users/cnifi/git/pyflecs/flecs.h: 13081


# /Users/cnifi/git/pyflecs/flecs.h: 13087
class union_ecs_metric_t(Union):
    pass


union_ecs_metric_t.__slots__ = [
    "gauge",
    "counter",
]
union_ecs_metric_t._fields_ = [
    ("gauge", ecs_gauge_t),
    ("counter", ecs_counter_t),
]

ecs_metric_t = union_ecs_metric_t  # /Users/cnifi/git/pyflecs/flecs.h: 13087


# /Users/cnifi/git/pyflecs/flecs.h: 13093
class struct_anon_22(Structure):
    pass


struct_anon_22.__slots__ = [
    "count",
    "not_alive_count",
]
struct_anon_22._fields_ = [
    ("count", ecs_metric_t),
    ("not_alive_count", ecs_metric_t),
]


# /Users/cnifi/git/pyflecs/flecs.h: 13099
class struct_anon_23(Structure):
    pass


struct_anon_23.__slots__ = [
    "tag_count",
    "component_count",
    "pair_count",
    "type_count",
    "create_count",
    "delete_count",
]
struct_anon_23._fields_ = [
    ("tag_count", ecs_metric_t),
    ("component_count", ecs_metric_t),
    ("pair_count", ecs_metric_t),
    ("type_count", ecs_metric_t),
    ("create_count", ecs_metric_t),
    ("delete_count", ecs_metric_t),
]


# /Users/cnifi/git/pyflecs/flecs.h: 13109
class struct_anon_24(Structure):
    pass


struct_anon_24.__slots__ = [
    "count",
    "empty_count",
    "create_count",
    "delete_count",
]
struct_anon_24._fields_ = [
    ("count", ecs_metric_t),
    ("empty_count", ecs_metric_t),
    ("create_count", ecs_metric_t),
    ("delete_count", ecs_metric_t),
]


# /Users/cnifi/git/pyflecs/flecs.h: 13117
class struct_anon_25(Structure):
    pass


struct_anon_25.__slots__ = [
    "query_count",
    "observer_count",
    "system_count",
]
struct_anon_25._fields_ = [
    ("query_count", ecs_metric_t),
    ("observer_count", ecs_metric_t),
    ("system_count", ecs_metric_t),
]


# /Users/cnifi/git/pyflecs/flecs.h: 13124
class struct_anon_26(Structure):
    pass


struct_anon_26.__slots__ = [
    "add_count",
    "remove_count",
    "delete_count",
    "clear_count",
    "set_count",
    "ensure_count",
    "modified_count",
    "other_count",
    "discard_count",
    "batched_entity_count",
    "batched_count",
]
struct_anon_26._fields_ = [
    ("add_count", ecs_metric_t),
    ("remove_count", ecs_metric_t),
    ("delete_count", ecs_metric_t),
    ("clear_count", ecs_metric_t),
    ("set_count", ecs_metric_t),
    ("ensure_count", ecs_metric_t),
    ("modified_count", ecs_metric_t),
    ("other_count", ecs_metric_t),
    ("discard_count", ecs_metric_t),
    ("batched_entity_count", ecs_metric_t),
    ("batched_count", ecs_metric_t),
]


# /Users/cnifi/git/pyflecs/flecs.h: 13139
class struct_anon_27(Structure):
    pass


struct_anon_27.__slots__ = [
    "frame_count",
    "merge_count",
    "rematch_count",
    "pipeline_build_count",
    "systems_ran",
    "observers_ran",
    "event_emit_count",
]
struct_anon_27._fields_ = [
    ("frame_count", ecs_metric_t),
    ("merge_count", ecs_metric_t),
    ("rematch_count", ecs_metric_t),
    ("pipeline_build_count", ecs_metric_t),
    ("systems_ran", ecs_metric_t),
    ("observers_ran", ecs_metric_t),
    ("event_emit_count", ecs_metric_t),
]


# /Users/cnifi/git/pyflecs/flecs.h: 13150
class struct_anon_28(Structure):
    pass


struct_anon_28.__slots__ = [
    "world_time_raw",
    "world_time",
    "frame_time",
    "system_time",
    "emit_time",
    "merge_time",
    "rematch_time",
    "fps",
    "delta_time",
]
struct_anon_28._fields_ = [
    ("world_time_raw", ecs_metric_t),
    ("world_time", ecs_metric_t),
    ("frame_time", ecs_metric_t),
    ("system_time", ecs_metric_t),
    ("emit_time", ecs_metric_t),
    ("merge_time", ecs_metric_t),
    ("rematch_time", ecs_metric_t),
    ("fps", ecs_metric_t),
    ("delta_time", ecs_metric_t),
]


# /Users/cnifi/git/pyflecs/flecs.h: 13162
class struct_anon_29(Structure):
    pass


struct_anon_29.__slots__ = [
    "alloc_count",
    "realloc_count",
    "free_count",
    "outstanding_alloc_count",
    "block_alloc_count",
    "block_free_count",
    "block_outstanding_alloc_count",
    "stack_alloc_count",
    "stack_free_count",
    "stack_outstanding_alloc_count",
]
struct_anon_29._fields_ = [
    ("alloc_count", ecs_metric_t),
    ("realloc_count", ecs_metric_t),
    ("free_count", ecs_metric_t),
    ("outstanding_alloc_count", ecs_metric_t),
    ("block_alloc_count", ecs_metric_t),
    ("block_free_count", ecs_metric_t),
    ("block_outstanding_alloc_count", ecs_metric_t),
    ("stack_alloc_count", ecs_metric_t),
    ("stack_free_count", ecs_metric_t),
    ("stack_outstanding_alloc_count", ecs_metric_t),
]


# /Users/cnifi/git/pyflecs/flecs.h: 13179
class struct_anon_30(Structure):
    pass


struct_anon_30.__slots__ = [
    "request_received_count",
    "request_invalid_count",
    "request_handled_ok_count",
    "request_handled_error_count",
    "request_not_handled_count",
    "request_preflight_count",
    "send_ok_count",
    "send_error_count",
    "busy_count",
]
struct_anon_30._fields_ = [
    ("request_received_count", ecs_metric_t),
    ("request_invalid_count", ecs_metric_t),
    ("request_handled_ok_count", ecs_metric_t),
    ("request_handled_error_count", ecs_metric_t),
    ("request_not_handled_count", ecs_metric_t),
    ("request_preflight_count", ecs_metric_t),
    ("send_ok_count", ecs_metric_t),
    ("send_error_count", ecs_metric_t),
    ("busy_count", ecs_metric_t),
]


# /Users/cnifi/git/pyflecs/flecs.h: 13195
class struct_ecs_world_stats_t(Structure):
    pass


struct_ecs_world_stats_t.__slots__ = [
    "first_",
    "entities",
    "components",
    "tables",
    "queries",
    "commands",
    "frame",
    "performance",
    "memory",
    "http",
    "last_",
    "t",
]
struct_ecs_world_stats_t._fields_ = [
    ("first_", c_int64),
    ("entities", struct_anon_22),
    ("components", struct_anon_23),
    ("tables", struct_anon_24),
    ("queries", struct_anon_25),
    ("commands", struct_anon_26),
    ("frame", struct_anon_27),
    ("performance", struct_anon_28),
    ("memory", struct_anon_29),
    ("http", struct_anon_30),
    ("last_", c_int64),
    ("t", c_int32),
]

ecs_world_stats_t = struct_ecs_world_stats_t  # /Users/cnifi/git/pyflecs/flecs.h: 13195


# /Users/cnifi/git/pyflecs/flecs.h: 13207
class struct_ecs_query_stats_t(Structure):
    pass


struct_ecs_query_stats_t.__slots__ = [
    "first_",
    "result_count",
    "matched_table_count",
    "matched_entity_count",
    "last_",
    "t",
]
struct_ecs_query_stats_t._fields_ = [
    ("first_", c_int64),
    ("result_count", ecs_metric_t),
    ("matched_table_count", ecs_metric_t),
    ("matched_entity_count", ecs_metric_t),
    ("last_", c_int64),
    ("t", c_int32),
]

ecs_query_stats_t = struct_ecs_query_stats_t  # /Users/cnifi/git/pyflecs/flecs.h: 13207


# /Users/cnifi/git/pyflecs/flecs.h: 13218
class struct_ecs_system_stats_t(Structure):
    pass


struct_ecs_system_stats_t.__slots__ = [
    "first_",
    "time_spent",
    "last_",
    "task",
    "query",
]
struct_ecs_system_stats_t._fields_ = [
    ("first_", c_int64),
    ("time_spent", ecs_metric_t),
    ("last_", c_int64),
    ("task", c_bool),
    ("query", ecs_query_stats_t),
]

ecs_system_stats_t = (
    struct_ecs_system_stats_t  # /Users/cnifi/git/pyflecs/flecs.h: 13218
)


# /Users/cnifi/git/pyflecs/flecs.h: 13230
class struct_ecs_sync_stats_t(Structure):
    pass


struct_ecs_sync_stats_t.__slots__ = [
    "first_",
    "time_spent",
    "commands_enqueued",
    "last_",
    "system_count",
    "multi_threaded",
    "immediate",
]
struct_ecs_sync_stats_t._fields_ = [
    ("first_", c_int64),
    ("time_spent", ecs_metric_t),
    ("commands_enqueued", ecs_metric_t),
    ("last_", c_int64),
    ("system_count", c_int32),
    ("multi_threaded", c_bool),
    ("immediate", c_bool),
]

ecs_sync_stats_t = struct_ecs_sync_stats_t  # /Users/cnifi/git/pyflecs/flecs.h: 13230


# /Users/cnifi/git/pyflecs/flecs.h: 13250
class struct_ecs_pipeline_stats_t(Structure):
    pass


struct_ecs_pipeline_stats_t.__slots__ = [
    "canary_",
    "systems",
    "sync_points",
    "t",
    "system_count",
    "active_system_count",
    "rebuild_count",
]
struct_ecs_pipeline_stats_t._fields_ = [
    ("canary_", c_int8),
    ("systems", ecs_vec_t),
    ("sync_points", ecs_vec_t),
    ("t", c_int32),
    ("system_count", c_int32),
    ("active_system_count", c_int32),
    ("rebuild_count", c_int32),
]

ecs_pipeline_stats_t = (
    struct_ecs_pipeline_stats_t  # /Users/cnifi/git/pyflecs/flecs.h: 13250
)

# /Users/cnifi/git/pyflecs/flecs.h: 13258
if _libs["libflecs.dylib"].has("ecs_world_stats_get", "cdecl"):
    ecs_world_stats_get = _libs["libflecs.dylib"].get("ecs_world_stats_get", "cdecl")
    ecs_world_stats_get.argtypes = [POINTER(ecs_world_t), POINTER(ecs_world_stats_t)]
    ecs_world_stats_get.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 13264
if _libs["libflecs.dylib"].has("ecs_world_stats_reduce", "cdecl"):
    ecs_world_stats_reduce = _libs["libflecs.dylib"].get(
        "ecs_world_stats_reduce", "cdecl"
    )
    ecs_world_stats_reduce.argtypes = [
        POINTER(ecs_world_stats_t),
        POINTER(ecs_world_stats_t),
    ]
    ecs_world_stats_reduce.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 13270
if _libs["libflecs.dylib"].has("ecs_world_stats_reduce_last", "cdecl"):
    ecs_world_stats_reduce_last = _libs["libflecs.dylib"].get(
        "ecs_world_stats_reduce_last", "cdecl"
    )
    ecs_world_stats_reduce_last.argtypes = [
        POINTER(ecs_world_stats_t),
        POINTER(ecs_world_stats_t),
        c_int32,
    ]
    ecs_world_stats_reduce_last.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 13277
if _libs["libflecs.dylib"].has("ecs_world_stats_repeat_last", "cdecl"):
    ecs_world_stats_repeat_last = _libs["libflecs.dylib"].get(
        "ecs_world_stats_repeat_last", "cdecl"
    )
    ecs_world_stats_repeat_last.argtypes = [POINTER(ecs_world_stats_t)]
    ecs_world_stats_repeat_last.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 13282
if _libs["libflecs.dylib"].has("ecs_world_stats_copy_last", "cdecl"):
    ecs_world_stats_copy_last = _libs["libflecs.dylib"].get(
        "ecs_world_stats_copy_last", "cdecl"
    )
    ecs_world_stats_copy_last.argtypes = [
        POINTER(ecs_world_stats_t),
        POINTER(ecs_world_stats_t),
    ]
    ecs_world_stats_copy_last.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 13287
if _libs["libflecs.dylib"].has("ecs_world_stats_log", "cdecl"):
    ecs_world_stats_log = _libs["libflecs.dylib"].get("ecs_world_stats_log", "cdecl")
    ecs_world_stats_log.argtypes = [POINTER(ecs_world_t), POINTER(ecs_world_stats_t)]
    ecs_world_stats_log.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 13299
if _libs["libflecs.dylib"].has("ecs_query_stats_get", "cdecl"):
    ecs_query_stats_get = _libs["libflecs.dylib"].get("ecs_query_stats_get", "cdecl")
    ecs_query_stats_get.argtypes = [
        POINTER(ecs_world_t),
        POINTER(ecs_query_t),
        POINTER(ecs_query_stats_t),
    ]
    ecs_query_stats_get.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 13306
if _libs["libflecs.dylib"].has("ecs_query_cache_stats_reduce", "cdecl"):
    ecs_query_cache_stats_reduce = _libs["libflecs.dylib"].get(
        "ecs_query_cache_stats_reduce", "cdecl"
    )
    ecs_query_cache_stats_reduce.argtypes = [
        POINTER(ecs_query_stats_t),
        POINTER(ecs_query_stats_t),
    ]
    ecs_query_cache_stats_reduce.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 13312
if _libs["libflecs.dylib"].has("ecs_query_cache_stats_reduce_last", "cdecl"):
    ecs_query_cache_stats_reduce_last = _libs["libflecs.dylib"].get(
        "ecs_query_cache_stats_reduce_last", "cdecl"
    )
    ecs_query_cache_stats_reduce_last.argtypes = [
        POINTER(ecs_query_stats_t),
        POINTER(ecs_query_stats_t),
        c_int32,
    ]
    ecs_query_cache_stats_reduce_last.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 13319
if _libs["libflecs.dylib"].has("ecs_query_cache_stats_repeat_last", "cdecl"):
    ecs_query_cache_stats_repeat_last = _libs["libflecs.dylib"].get(
        "ecs_query_cache_stats_repeat_last", "cdecl"
    )
    ecs_query_cache_stats_repeat_last.argtypes = [POINTER(ecs_query_stats_t)]
    ecs_query_cache_stats_repeat_last.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 13324
if _libs["libflecs.dylib"].has("ecs_query_cache_stats_copy_last", "cdecl"):
    ecs_query_cache_stats_copy_last = _libs["libflecs.dylib"].get(
        "ecs_query_cache_stats_copy_last", "cdecl"
    )
    ecs_query_cache_stats_copy_last.argtypes = [
        POINTER(ecs_query_stats_t),
        POINTER(ecs_query_stats_t),
    ]
    ecs_query_cache_stats_copy_last.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 13337
if _libs["libflecs.dylib"].has("ecs_system_stats_get", "cdecl"):
    ecs_system_stats_get = _libs["libflecs.dylib"].get("ecs_system_stats_get", "cdecl")
    ecs_system_stats_get.argtypes = [
        POINTER(ecs_world_t),
        ecs_entity_t,
        POINTER(ecs_system_stats_t),
    ]
    ecs_system_stats_get.restype = c_bool

# /Users/cnifi/git/pyflecs/flecs.h: 13344
if _libs["libflecs.dylib"].has("ecs_system_stats_reduce", "cdecl"):
    ecs_system_stats_reduce = _libs["libflecs.dylib"].get(
        "ecs_system_stats_reduce", "cdecl"
    )
    ecs_system_stats_reduce.argtypes = [
        POINTER(ecs_system_stats_t),
        POINTER(ecs_system_stats_t),
    ]
    ecs_system_stats_reduce.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 13350
if _libs["libflecs.dylib"].has("ecs_system_stats_reduce_last", "cdecl"):
    ecs_system_stats_reduce_last = _libs["libflecs.dylib"].get(
        "ecs_system_stats_reduce_last", "cdecl"
    )
    ecs_system_stats_reduce_last.argtypes = [
        POINTER(ecs_system_stats_t),
        POINTER(ecs_system_stats_t),
        c_int32,
    ]
    ecs_system_stats_reduce_last.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 13357
if _libs["libflecs.dylib"].has("ecs_system_stats_repeat_last", "cdecl"):
    ecs_system_stats_repeat_last = _libs["libflecs.dylib"].get(
        "ecs_system_stats_repeat_last", "cdecl"
    )
    ecs_system_stats_repeat_last.argtypes = [POINTER(ecs_system_stats_t)]
    ecs_system_stats_repeat_last.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 13362
if _libs["libflecs.dylib"].has("ecs_system_stats_copy_last", "cdecl"):
    ecs_system_stats_copy_last = _libs["libflecs.dylib"].get(
        "ecs_system_stats_copy_last", "cdecl"
    )
    ecs_system_stats_copy_last.argtypes = [
        POINTER(ecs_system_stats_t),
        POINTER(ecs_system_stats_t),
    ]
    ecs_system_stats_copy_last.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 13375
if _libs["libflecs.dylib"].has("ecs_pipeline_stats_get", "cdecl"):
    ecs_pipeline_stats_get = _libs["libflecs.dylib"].get(
        "ecs_pipeline_stats_get", "cdecl"
    )
    ecs_pipeline_stats_get.argtypes = [
        POINTER(ecs_world_t),
        ecs_entity_t,
        POINTER(ecs_pipeline_stats_t),
    ]
    ecs_pipeline_stats_get.restype = c_bool

# /Users/cnifi/git/pyflecs/flecs.h: 13385
if _libs["libflecs.dylib"].has("ecs_pipeline_stats_fini", "cdecl"):
    ecs_pipeline_stats_fini = _libs["libflecs.dylib"].get(
        "ecs_pipeline_stats_fini", "cdecl"
    )
    ecs_pipeline_stats_fini.argtypes = [POINTER(ecs_pipeline_stats_t)]
    ecs_pipeline_stats_fini.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 13390
if _libs["libflecs.dylib"].has("ecs_pipeline_stats_reduce", "cdecl"):
    ecs_pipeline_stats_reduce = _libs["libflecs.dylib"].get(
        "ecs_pipeline_stats_reduce", "cdecl"
    )
    ecs_pipeline_stats_reduce.argtypes = [
        POINTER(ecs_pipeline_stats_t),
        POINTER(ecs_pipeline_stats_t),
    ]
    ecs_pipeline_stats_reduce.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 13396
if _libs["libflecs.dylib"].has("ecs_pipeline_stats_reduce_last", "cdecl"):
    ecs_pipeline_stats_reduce_last = _libs["libflecs.dylib"].get(
        "ecs_pipeline_stats_reduce_last", "cdecl"
    )
    ecs_pipeline_stats_reduce_last.argtypes = [
        POINTER(ecs_pipeline_stats_t),
        POINTER(ecs_pipeline_stats_t),
        c_int32,
    ]
    ecs_pipeline_stats_reduce_last.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 13403
if _libs["libflecs.dylib"].has("ecs_pipeline_stats_repeat_last", "cdecl"):
    ecs_pipeline_stats_repeat_last = _libs["libflecs.dylib"].get(
        "ecs_pipeline_stats_repeat_last", "cdecl"
    )
    ecs_pipeline_stats_repeat_last.argtypes = [POINTER(ecs_pipeline_stats_t)]
    ecs_pipeline_stats_repeat_last.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 13414
if _libs["libflecs.dylib"].has("ecs_pipeline_stats_copy_last", "cdecl"):
    ecs_pipeline_stats_copy_last = _libs["libflecs.dylib"].get(
        "ecs_pipeline_stats_copy_last", "cdecl"
    )
    ecs_pipeline_stats_copy_last.argtypes = [
        POINTER(ecs_pipeline_stats_t),
        POINTER(ecs_pipeline_stats_t),
    ]
    ecs_pipeline_stats_copy_last.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 13420
if _libs["libflecs.dylib"].has("ecs_metric_reduce", "cdecl"):
    ecs_metric_reduce = _libs["libflecs.dylib"].get("ecs_metric_reduce", "cdecl")
    ecs_metric_reduce.argtypes = [
        POINTER(ecs_metric_t),
        POINTER(ecs_metric_t),
        c_int32,
        c_int32,
    ]
    ecs_metric_reduce.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 13428
if _libs["libflecs.dylib"].has("ecs_metric_reduce_last", "cdecl"):
    ecs_metric_reduce_last = _libs["libflecs.dylib"].get(
        "ecs_metric_reduce_last", "cdecl"
    )
    ecs_metric_reduce_last.argtypes = [POINTER(ecs_metric_t), c_int32, c_int32]
    ecs_metric_reduce_last.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 13435
if _libs["libflecs.dylib"].has("ecs_metric_copy", "cdecl"):
    ecs_metric_copy = _libs["libflecs.dylib"].get("ecs_metric_copy", "cdecl")
    ecs_metric_copy.argtypes = [POINTER(ecs_metric_t), c_int32, c_int32]
    ecs_metric_copy.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 13440
try:
    FLECS_IDFlecsStatsID_ = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "FLECS_IDFlecsStatsID_"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 13441
try:
    FLECS_IDEcsWorldStatsID_ = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "FLECS_IDEcsWorldStatsID_"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 13442
try:
    FLECS_IDEcsWorldSummaryID_ = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "FLECS_IDEcsWorldSummaryID_"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 13443
try:
    FLECS_IDEcsSystemStatsID_ = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "FLECS_IDEcsSystemStatsID_"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 13444
try:
    FLECS_IDEcsPipelineStatsID_ = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "FLECS_IDEcsPipelineStatsID_"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 13446
try:
    EcsPeriod1s = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsPeriod1s")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 13447
try:
    EcsPeriod1m = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsPeriod1m")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 13448
try:
    EcsPeriod1h = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsPeriod1h")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 13449
try:
    EcsPeriod1d = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsPeriod1d")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 13450
try:
    EcsPeriod1w = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsPeriod1w")
except:
    pass


# /Users/cnifi/git/pyflecs/flecs.h: 13456
class struct_anon_31(Structure):
    pass


struct_anon_31.__slots__ = [
    "elapsed",
    "reduce_count",
]
struct_anon_31._fields_ = [
    ("elapsed", c_float),
    ("reduce_count", c_int32),
]

EcsStatsHeader = struct_anon_31  # /Users/cnifi/git/pyflecs/flecs.h: 13456


# /Users/cnifi/git/pyflecs/flecs.h: 13462
class struct_anon_32(Structure):
    pass


struct_anon_32.__slots__ = [
    "hdr",
    "stats",
]
struct_anon_32._fields_ = [
    ("hdr", EcsStatsHeader),
    ("stats", ecs_world_stats_t),
]

EcsWorldStats = struct_anon_32  # /Users/cnifi/git/pyflecs/flecs.h: 13462


# /Users/cnifi/git/pyflecs/flecs.h: 13468
class struct_anon_33(Structure):
    pass


struct_anon_33.__slots__ = [
    "hdr",
    "stats",
]
struct_anon_33._fields_ = [
    ("hdr", EcsStatsHeader),
    ("stats", ecs_map_t),
]

EcsSystemStats = struct_anon_33  # /Users/cnifi/git/pyflecs/flecs.h: 13468


# /Users/cnifi/git/pyflecs/flecs.h: 13474
class struct_anon_34(Structure):
    pass


struct_anon_34.__slots__ = [
    "hdr",
    "stats",
]
struct_anon_34._fields_ = [
    ("hdr", EcsStatsHeader),
    ("stats", ecs_map_t),
]

EcsPipelineStats = struct_anon_34  # /Users/cnifi/git/pyflecs/flecs.h: 13474


# /Users/cnifi/git/pyflecs/flecs.h: 13497
class struct_anon_35(Structure):
    pass


struct_anon_35.__slots__ = [
    "target_fps",
    "time_scale",
    "frame_time_total",
    "system_time_total",
    "merge_time_total",
    "frame_time_last",
    "system_time_last",
    "merge_time_last",
    "frame_count",
    "command_count",
    "build_info",
]
struct_anon_35._fields_ = [
    ("target_fps", c_double),
    ("time_scale", c_double),
    ("frame_time_total", c_double),
    ("system_time_total", c_double),
    ("merge_time_total", c_double),
    ("frame_time_last", c_double),
    ("system_time_last", c_double),
    ("merge_time_last", c_double),
    ("frame_count", c_int64),
    ("command_count", c_int64),
    ("build_info", ecs_build_info_t),
]

EcsWorldSummary = struct_anon_35  # /Users/cnifi/git/pyflecs/flecs.h: 13497

# /Users/cnifi/git/pyflecs/flecs.h: 13508
if _libs["libflecs.dylib"].has("FlecsStatsImport", "cdecl"):
    FlecsStatsImport = _libs["libflecs.dylib"].get("FlecsStatsImport", "cdecl")
    FlecsStatsImport.argtypes = [POINTER(ecs_world_t)]
    FlecsStatsImport.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 13565
try:
    FLECS_IDFlecsMetricsID_ = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "FLECS_IDFlecsMetricsID_"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 13568
try:
    EcsMetric = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsMetric")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 13568
try:
    FLECS_IDEcsMetricID_ = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "FLECS_IDEcsMetricID_"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 13571
try:
    EcsCounter = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsCounter")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 13571
try:
    FLECS_IDEcsCounterID_ = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "FLECS_IDEcsCounterID_"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 13574
try:
    EcsCounterIncrement = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "EcsCounterIncrement"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 13574
try:
    FLECS_IDEcsCounterIncrementID_ = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "FLECS_IDEcsCounterIncrementID_"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 13577
try:
    EcsCounterId = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsCounterId")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 13577
try:
    FLECS_IDEcsCounterIdID_ = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "FLECS_IDEcsCounterIdID_"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 13580
try:
    EcsGauge = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsGauge")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 13580
try:
    FLECS_IDEcsGaugeID_ = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "FLECS_IDEcsGaugeID_"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 13583
try:
    EcsMetricInstance = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "EcsMetricInstance"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 13583
try:
    FLECS_IDEcsMetricInstanceID_ = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "FLECS_IDEcsMetricInstanceID_"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 13586
try:
    FLECS_IDEcsMetricValueID_ = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "FLECS_IDEcsMetricValueID_"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 13589
try:
    FLECS_IDEcsMetricSourceID_ = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "FLECS_IDEcsMetricSourceID_"
    )
except:
    pass


# /Users/cnifi/git/pyflecs/flecs.h: 13594
class struct_EcsMetricValue(Structure):
    pass


struct_EcsMetricValue.__slots__ = [
    "value",
]
struct_EcsMetricValue._fields_ = [
    ("value", c_double),
]

EcsMetricValue = struct_EcsMetricValue  # /Users/cnifi/git/pyflecs/flecs.h: 13594


# /Users/cnifi/git/pyflecs/flecs.h: 13599
class struct_EcsMetricSource(Structure):
    pass


struct_EcsMetricSource.__slots__ = [
    "entity",
]
struct_EcsMetricSource._fields_ = [
    ("entity", ecs_entity_t),
]

EcsMetricSource = struct_EcsMetricSource  # /Users/cnifi/git/pyflecs/flecs.h: 13599


# /Users/cnifi/git/pyflecs/flecs.h: 13632
class struct_ecs_metric_desc_t(Structure):
    pass


struct_ecs_metric_desc_t.__slots__ = [
    "_canary",
    "entity",
    "member",
    "dotmember",
    "id",
    "targets",
    "kind",
    "brief",
]
struct_ecs_metric_desc_t._fields_ = [
    ("_canary", c_int32),
    ("entity", ecs_entity_t),
    ("member", ecs_entity_t),
    ("dotmember", String),
    ("id", ecs_id_t),
    ("targets", c_bool),
    ("kind", ecs_entity_t),
    ("brief", String),
]

ecs_metric_desc_t = struct_ecs_metric_desc_t  # /Users/cnifi/git/pyflecs/flecs.h: 13632

# /Users/cnifi/git/pyflecs/flecs.h: 13674
if _libs["libflecs.dylib"].has("ecs_metric_init", "cdecl"):
    ecs_metric_init = _libs["libflecs.dylib"].get("ecs_metric_init", "cdecl")
    ecs_metric_init.argtypes = [POINTER(ecs_world_t), POINTER(ecs_metric_desc_t)]
    ecs_metric_init.restype = ecs_entity_t

# /Users/cnifi/git/pyflecs/flecs.h: 13701
if _libs["libflecs.dylib"].has("FlecsMetricsImport", "cdecl"):
    FlecsMetricsImport = _libs["libflecs.dylib"].get("FlecsMetricsImport", "cdecl")
    FlecsMetricsImport.argtypes = [POINTER(ecs_world_t)]
    FlecsMetricsImport.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 13753
try:
    FLECS_IDFlecsAlertsID_ = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "FLECS_IDFlecsAlertsID_"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 13757
try:
    FLECS_IDEcsAlertID_ = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "FLECS_IDEcsAlertID_"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 13758
try:
    FLECS_IDEcsAlertInstanceID_ = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "FLECS_IDEcsAlertInstanceID_"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 13759
try:
    FLECS_IDEcsAlertsActiveID_ = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "FLECS_IDEcsAlertsActiveID_"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 13760
try:
    FLECS_IDEcsAlertTimeoutID_ = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "FLECS_IDEcsAlertTimeoutID_"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 13763
try:
    EcsAlertInfo = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsAlertInfo")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 13763
try:
    FLECS_IDEcsAlertInfoID_ = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "FLECS_IDEcsAlertInfoID_"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 13764
try:
    EcsAlertWarning = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsAlertWarning")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 13764
try:
    FLECS_IDEcsAlertWarningID_ = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "FLECS_IDEcsAlertWarningID_"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 13765
try:
    EcsAlertError = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsAlertError")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 13765
try:
    FLECS_IDEcsAlertErrorID_ = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "FLECS_IDEcsAlertErrorID_"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 13766
try:
    EcsAlertCritical = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "EcsAlertCritical"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 13766
try:
    FLECS_IDEcsAlertCriticalID_ = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "FLECS_IDEcsAlertCriticalID_"
    )
except:
    pass


# /Users/cnifi/git/pyflecs/flecs.h: 13771
class struct_EcsAlertInstance(Structure):
    pass


struct_EcsAlertInstance.__slots__ = [
    "message",
]
struct_EcsAlertInstance._fields_ = [
    ("message", String),
]

EcsAlertInstance = struct_EcsAlertInstance  # /Users/cnifi/git/pyflecs/flecs.h: 13771


# /Users/cnifi/git/pyflecs/flecs.h: 13779
class struct_EcsAlertsActive(Structure):
    pass


struct_EcsAlertsActive.__slots__ = [
    "info_count",
    "warning_count",
    "error_count",
    "alerts",
]
struct_EcsAlertsActive._fields_ = [
    ("info_count", c_int32),
    ("warning_count", c_int32),
    ("error_count", c_int32),
    ("alerts", ecs_map_t),
]

EcsAlertsActive = struct_EcsAlertsActive  # /Users/cnifi/git/pyflecs/flecs.h: 13779


# /Users/cnifi/git/pyflecs/flecs.h: 13793
class struct_ecs_alert_severity_filter_t(Structure):
    pass


struct_ecs_alert_severity_filter_t.__slots__ = [
    "severity",
    "with",
    "var",
    "_var_index",
]
struct_ecs_alert_severity_filter_t._fields_ = [
    ("severity", ecs_entity_t),
    ("with", ecs_id_t),
    ("var", String),
    ("_var_index", c_int32),
]

ecs_alert_severity_filter_t = (
    struct_ecs_alert_severity_filter_t  # /Users/cnifi/git/pyflecs/flecs.h: 13793
)


# /Users/cnifi/git/pyflecs/flecs.h: 13852
class struct_ecs_alert_desc_t(Structure):
    pass


struct_ecs_alert_desc_t.__slots__ = [
    "_canary",
    "entity",
    "query",
    "message",
    "doc_name",
    "brief",
    "severity",
    "severity_filters",
    "retain_period",
    "member",
    "id",
    "var",
]
struct_ecs_alert_desc_t._fields_ = [
    ("_canary", c_int32),
    ("entity", ecs_entity_t),
    ("query", ecs_query_desc_t),
    ("message", String),
    ("doc_name", String),
    ("brief", String),
    ("severity", ecs_entity_t),
    ("severity_filters", ecs_alert_severity_filter_t * int(4)),
    ("retain_period", c_float),
    ("member", ecs_entity_t),
    ("id", ecs_id_t),
    ("var", String),
]

ecs_alert_desc_t = struct_ecs_alert_desc_t  # /Users/cnifi/git/pyflecs/flecs.h: 13852

# /Users/cnifi/git/pyflecs/flecs.h: 13880
if _libs["libflecs.dylib"].has("ecs_alert_init", "cdecl"):
    ecs_alert_init = _libs["libflecs.dylib"].get("ecs_alert_init", "cdecl")
    ecs_alert_init.argtypes = [POINTER(ecs_world_t), POINTER(ecs_alert_desc_t)]
    ecs_alert_init.restype = ecs_entity_t

# /Users/cnifi/git/pyflecs/flecs.h: 13902
if _libs["libflecs.dylib"].has("ecs_get_alert_count", "cdecl"):
    ecs_get_alert_count = _libs["libflecs.dylib"].get("ecs_get_alert_count", "cdecl")
    ecs_get_alert_count.argtypes = [POINTER(ecs_world_t), ecs_entity_t, ecs_entity_t]
    ecs_get_alert_count.restype = c_int32

# /Users/cnifi/git/pyflecs/flecs.h: 13917
if _libs["libflecs.dylib"].has("ecs_get_alert", "cdecl"):
    ecs_get_alert = _libs["libflecs.dylib"].get("ecs_get_alert", "cdecl")
    ecs_get_alert.argtypes = [POINTER(ecs_world_t), ecs_entity_t, ecs_entity_t]
    ecs_get_alert.restype = ecs_entity_t

# /Users/cnifi/git/pyflecs/flecs.h: 13931
if _libs["libflecs.dylib"].has("FlecsAlertsImport", "cdecl"):
    FlecsAlertsImport = _libs["libflecs.dylib"].get("FlecsAlertsImport", "cdecl")
    FlecsAlertsImport.argtypes = [POINTER(ecs_world_t)]
    FlecsAlertsImport.restype = None


# /Users/cnifi/git/pyflecs/flecs.h: 14005
class struct_ecs_from_json_desc_t(Structure):
    pass


struct_ecs_from_json_desc_t.__slots__ = [
    "name",
    "expr",
    "lookup_action",
    "lookup_ctx",
    "strict",
]
struct_ecs_from_json_desc_t._fields_ = [
    ("name", String),
    ("expr", String),
    (
        "lookup_action",
        CFUNCTYPE(UNCHECKED(ecs_entity_t), POINTER(ecs_world_t), String, POINTER(None)),
    ),
    ("lookup_ctx", POINTER(None)),
    ("strict", c_bool),
]

ecs_from_json_desc_t = (
    struct_ecs_from_json_desc_t  # /Users/cnifi/git/pyflecs/flecs.h: 14005
)

# /Users/cnifi/git/pyflecs/flecs.h: 14019
if _libs["libflecs.dylib"].has("ecs_ptr_from_json", "cdecl"):
    ecs_ptr_from_json = _libs["libflecs.dylib"].get("ecs_ptr_from_json", "cdecl")
    ecs_ptr_from_json.argtypes = [
        POINTER(ecs_world_t),
        ecs_entity_t,
        POINTER(None),
        String,
        POINTER(ecs_from_json_desc_t),
    ]
    ecs_ptr_from_json.restype = c_char_p

# /Users/cnifi/git/pyflecs/flecs.h: 14037
if _libs["libflecs.dylib"].has("ecs_entity_from_json", "cdecl"):
    ecs_entity_from_json = _libs["libflecs.dylib"].get("ecs_entity_from_json", "cdecl")
    ecs_entity_from_json.argtypes = [
        POINTER(ecs_world_t),
        ecs_entity_t,
        String,
        POINTER(ecs_from_json_desc_t),
    ]
    ecs_entity_from_json.restype = c_char_p

# /Users/cnifi/git/pyflecs/flecs.h: 14052
if _libs["libflecs.dylib"].has("ecs_world_from_json", "cdecl"):
    ecs_world_from_json = _libs["libflecs.dylib"].get("ecs_world_from_json", "cdecl")
    ecs_world_from_json.argtypes = [
        POINTER(ecs_world_t),
        String,
        POINTER(ecs_from_json_desc_t),
    ]
    ecs_world_from_json.restype = c_char_p

# /Users/cnifi/git/pyflecs/flecs.h: 14065
if _libs["libflecs.dylib"].has("ecs_world_from_json_file", "cdecl"):
    ecs_world_from_json_file = _libs["libflecs.dylib"].get(
        "ecs_world_from_json_file", "cdecl"
    )
    ecs_world_from_json_file.argtypes = [
        POINTER(ecs_world_t),
        String,
        POINTER(ecs_from_json_desc_t),
    ]
    ecs_world_from_json_file.restype = c_char_p

# /Users/cnifi/git/pyflecs/flecs.h: 14085
if _libs["libflecs.dylib"].has("ecs_array_to_json", "cdecl"):
    ecs_array_to_json = _libs["libflecs.dylib"].get("ecs_array_to_json", "cdecl")
    ecs_array_to_json.argtypes = [
        POINTER(ecs_world_t),
        ecs_entity_t,
        POINTER(None),
        c_int32,
    ]
    if sizeof(c_int) == sizeof(c_void_p):
        ecs_array_to_json.restype = ReturnString
    else:
        ecs_array_to_json.restype = String
        ecs_array_to_json.errcheck = ReturnString

# /Users/cnifi/git/pyflecs/flecs.h: 14102
if _libs["libflecs.dylib"].has("ecs_array_to_json_buf", "cdecl"):
    ecs_array_to_json_buf = _libs["libflecs.dylib"].get(
        "ecs_array_to_json_buf", "cdecl"
    )
    ecs_array_to_json_buf.argtypes = [
        POINTER(ecs_world_t),
        ecs_entity_t,
        POINTER(None),
        c_int32,
        POINTER(ecs_strbuf_t),
    ]
    ecs_array_to_json_buf.restype = c_int

# /Users/cnifi/git/pyflecs/flecs.h: 14118
if _libs["libflecs.dylib"].has("ecs_ptr_to_json", "cdecl"):
    ecs_ptr_to_json = _libs["libflecs.dylib"].get("ecs_ptr_to_json", "cdecl")
    ecs_ptr_to_json.argtypes = [POINTER(ecs_world_t), ecs_entity_t, POINTER(None)]
    if sizeof(c_int) == sizeof(c_void_p):
        ecs_ptr_to_json.restype = ReturnString
    else:
        ecs_ptr_to_json.restype = String
        ecs_ptr_to_json.errcheck = ReturnString

# /Users/cnifi/git/pyflecs/flecs.h: 14133
if _libs["libflecs.dylib"].has("ecs_ptr_to_json_buf", "cdecl"):
    ecs_ptr_to_json_buf = _libs["libflecs.dylib"].get("ecs_ptr_to_json_buf", "cdecl")
    ecs_ptr_to_json_buf.argtypes = [
        POINTER(ecs_world_t),
        ecs_entity_t,
        POINTER(None),
        POINTER(ecs_strbuf_t),
    ]
    ecs_ptr_to_json_buf.restype = c_int

# /Users/cnifi/git/pyflecs/flecs.h: 14150
if _libs["libflecs.dylib"].has("ecs_type_info_to_json", "cdecl"):
    ecs_type_info_to_json = _libs["libflecs.dylib"].get(
        "ecs_type_info_to_json", "cdecl"
    )
    ecs_type_info_to_json.argtypes = [POINTER(ecs_world_t), ecs_entity_t]
    if sizeof(c_int) == sizeof(c_void_p):
        ecs_type_info_to_json.restype = ReturnString
    else:
        ecs_type_info_to_json.restype = String
        ecs_type_info_to_json.errcheck = ReturnString

# /Users/cnifi/git/pyflecs/flecs.h: 14163
if _libs["libflecs.dylib"].has("ecs_type_info_to_json_buf", "cdecl"):
    ecs_type_info_to_json_buf = _libs["libflecs.dylib"].get(
        "ecs_type_info_to_json_buf", "cdecl"
    )
    ecs_type_info_to_json_buf.argtypes = [
        POINTER(ecs_world_t),
        ecs_entity_t,
        POINTER(ecs_strbuf_t),
    ]
    ecs_type_info_to_json_buf.restype = c_int


# /Users/cnifi/git/pyflecs/flecs.h: 14180
class struct_ecs_entity_to_json_desc_t(Structure):
    pass


struct_ecs_entity_to_json_desc_t.__slots__ = [
    "serialize_entity_id",
    "serialize_doc",
    "serialize_full_paths",
    "serialize_inherited",
    "serialize_values",
    "serialize_builtin",
    "serialize_type_info",
    "serialize_alerts",
    "serialize_refs",
    "serialize_matches",
]
struct_ecs_entity_to_json_desc_t._fields_ = [
    ("serialize_entity_id", c_bool),
    ("serialize_doc", c_bool),
    ("serialize_full_paths", c_bool),
    ("serialize_inherited", c_bool),
    ("serialize_values", c_bool),
    ("serialize_builtin", c_bool),
    ("serialize_type_info", c_bool),
    ("serialize_alerts", c_bool),
    ("serialize_refs", ecs_entity_t),
    ("serialize_matches", c_bool),
]

ecs_entity_to_json_desc_t = (
    struct_ecs_entity_to_json_desc_t  # /Users/cnifi/git/pyflecs/flecs.h: 14180
)

# /Users/cnifi/git/pyflecs/flecs.h: 14222
if _libs["libflecs.dylib"].has("ecs_entity_to_json", "cdecl"):
    ecs_entity_to_json = _libs["libflecs.dylib"].get("ecs_entity_to_json", "cdecl")
    ecs_entity_to_json.argtypes = [
        POINTER(ecs_world_t),
        ecs_entity_t,
        POINTER(ecs_entity_to_json_desc_t),
    ]
    if sizeof(c_int) == sizeof(c_void_p):
        ecs_entity_to_json.restype = ReturnString
    else:
        ecs_entity_to_json.restype = String
        ecs_entity_to_json.errcheck = ReturnString

# /Users/cnifi/git/pyflecs/flecs.h: 14236
if _libs["libflecs.dylib"].has("ecs_entity_to_json_buf", "cdecl"):
    ecs_entity_to_json_buf = _libs["libflecs.dylib"].get(
        "ecs_entity_to_json_buf", "cdecl"
    )
    ecs_entity_to_json_buf.argtypes = [
        POINTER(ecs_world_t),
        ecs_entity_t,
        POINTER(ecs_strbuf_t),
        POINTER(ecs_entity_to_json_desc_t),
    ]
    ecs_entity_to_json_buf.restype = c_int


# /Users/cnifi/git/pyflecs/flecs.h: 14262
class struct_ecs_iter_to_json_desc_t(Structure):
    pass


struct_ecs_iter_to_json_desc_t.__slots__ = [
    "serialize_entity_ids",
    "serialize_values",
    "serialize_builtin",
    "serialize_doc",
    "serialize_full_paths",
    "serialize_fields",
    "serialize_inherited",
    "serialize_table",
    "serialize_type_info",
    "serialize_field_info",
    "serialize_query_info",
    "serialize_query_plan",
    "serialize_query_profile",
    "dont_serialize_results",
    "serialize_alerts",
    "serialize_refs",
    "serialize_matches",
    "query",
]
struct_ecs_iter_to_json_desc_t._fields_ = [
    ("serialize_entity_ids", c_bool),
    ("serialize_values", c_bool),
    ("serialize_builtin", c_bool),
    ("serialize_doc", c_bool),
    ("serialize_full_paths", c_bool),
    ("serialize_fields", c_bool),
    ("serialize_inherited", c_bool),
    ("serialize_table", c_bool),
    ("serialize_type_info", c_bool),
    ("serialize_field_info", c_bool),
    ("serialize_query_info", c_bool),
    ("serialize_query_plan", c_bool),
    ("serialize_query_profile", c_bool),
    ("dont_serialize_results", c_bool),
    ("serialize_alerts", c_bool),
    ("serialize_refs", ecs_entity_t),
    ("serialize_matches", c_bool),
    ("query", POINTER(ecs_poly_t)),
]

ecs_iter_to_json_desc_t = (
    struct_ecs_iter_to_json_desc_t  # /Users/cnifi/git/pyflecs/flecs.h: 14262
)

# /Users/cnifi/git/pyflecs/flecs.h: 14317
if _libs["libflecs.dylib"].has("ecs_iter_to_json", "cdecl"):
    ecs_iter_to_json = _libs["libflecs.dylib"].get("ecs_iter_to_json", "cdecl")
    ecs_iter_to_json.argtypes = [POINTER(ecs_iter_t), POINTER(ecs_iter_to_json_desc_t)]
    if sizeof(c_int) == sizeof(c_void_p):
        ecs_iter_to_json.restype = ReturnString
    else:
        ecs_iter_to_json.restype = String
        ecs_iter_to_json.errcheck = ReturnString

# /Users/cnifi/git/pyflecs/flecs.h: 14329
if _libs["libflecs.dylib"].has("ecs_iter_to_json_buf", "cdecl"):
    ecs_iter_to_json_buf = _libs["libflecs.dylib"].get("ecs_iter_to_json_buf", "cdecl")
    ecs_iter_to_json_buf.argtypes = [
        POINTER(ecs_iter_t),
        POINTER(ecs_strbuf_t),
        POINTER(ecs_iter_to_json_desc_t),
    ]
    ecs_iter_to_json_buf.restype = c_int


# /Users/cnifi/git/pyflecs/flecs.h: 14338
class struct_ecs_world_to_json_desc_t(Structure):
    pass


struct_ecs_world_to_json_desc_t.__slots__ = [
    "serialize_builtin",
    "serialize_modules",
]
struct_ecs_world_to_json_desc_t._fields_ = [
    ("serialize_builtin", c_bool),
    ("serialize_modules", c_bool),
]

ecs_world_to_json_desc_t = (
    struct_ecs_world_to_json_desc_t  # /Users/cnifi/git/pyflecs/flecs.h: 14338
)

# /Users/cnifi/git/pyflecs/flecs.h: 14358
if _libs["libflecs.dylib"].has("ecs_world_to_json", "cdecl"):
    ecs_world_to_json = _libs["libflecs.dylib"].get("ecs_world_to_json", "cdecl")
    ecs_world_to_json.argtypes = [
        POINTER(ecs_world_t),
        POINTER(ecs_world_to_json_desc_t),
    ]
    if sizeof(c_int) == sizeof(c_void_p):
        ecs_world_to_json.restype = ReturnString
    else:
        ecs_world_to_json.restype = String
        ecs_world_to_json.errcheck = ReturnString

# /Users/cnifi/git/pyflecs/flecs.h: 14370
if _libs["libflecs.dylib"].has("ecs_world_to_json_buf", "cdecl"):
    ecs_world_to_json_buf = _libs["libflecs.dylib"].get(
        "ecs_world_to_json_buf", "cdecl"
    )
    ecs_world_to_json_buf.argtypes = [
        POINTER(ecs_world_t),
        POINTER(ecs_strbuf_t),
        POINTER(ecs_world_to_json_desc_t),
    ]
    ecs_world_to_json_buf.restype = c_int

# /Users/cnifi/git/pyflecs/flecs.h: 14448
try:
    EcsUnitPrefixes = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsUnitPrefixes")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14450
try:
    EcsYocto = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsYocto")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14451
try:
    EcsZepto = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsZepto")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14452
try:
    EcsAtto = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsAtto")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14453
try:
    EcsFemto = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsFemto")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14454
try:
    EcsPico = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsPico")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14455
try:
    EcsNano = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsNano")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14456
try:
    EcsMicro = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsMicro")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14457
try:
    EcsMilli = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsMilli")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14458
try:
    EcsCenti = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsCenti")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14459
try:
    EcsDeci = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsDeci")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14460
try:
    EcsDeca = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsDeca")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14461
try:
    EcsHecto = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsHecto")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14462
try:
    EcsKilo = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsKilo")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14463
try:
    EcsMega = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsMega")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14464
try:
    EcsGiga = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsGiga")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14465
try:
    EcsTera = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsTera")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14466
try:
    EcsPeta = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsPeta")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14467
try:
    EcsExa = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsExa")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14468
try:
    EcsZetta = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsZetta")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14469
try:
    EcsYotta = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsYotta")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14471
try:
    EcsKibi = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsKibi")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14472
try:
    EcsMebi = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsMebi")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14473
try:
    EcsGibi = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsGibi")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14474
try:
    EcsTebi = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsTebi")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14475
try:
    EcsPebi = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsPebi")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14476
try:
    EcsExbi = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsExbi")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14477
try:
    EcsZebi = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsZebi")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14478
try:
    EcsYobi = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsYobi")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14488
try:
    EcsDuration = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsDuration")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14489
try:
    EcsPicoSeconds = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsPicoSeconds")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14490
try:
    EcsNanoSeconds = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsNanoSeconds")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14491
try:
    EcsMicroSeconds = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsMicroSeconds")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14492
try:
    EcsMilliSeconds = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsMilliSeconds")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14493
try:
    EcsSeconds = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsSeconds")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14494
try:
    EcsMinutes = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsMinutes")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14495
try:
    EcsHours = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsHours")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14496
try:
    EcsDays = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsDays")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14506
try:
    EcsTime = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsTime")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14507
try:
    EcsDate = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsDate")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14517
try:
    EcsMass = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsMass")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14518
try:
    EcsGrams = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsGrams")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14519
try:
    EcsKiloGrams = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsKiloGrams")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14529
try:
    EcsElectricCurrent = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "EcsElectricCurrent"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14530
try:
    EcsAmpere = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsAmpere")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14540
try:
    EcsAmount = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsAmount")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14541
try:
    EcsMole = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsMole")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14551
try:
    EcsLuminousIntensity = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "EcsLuminousIntensity"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14552
try:
    EcsCandela = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsCandela")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14562
try:
    EcsForce = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsForce")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14563
try:
    EcsNewton = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsNewton")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14573
try:
    EcsLength = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsLength")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14574
try:
    EcsMeters = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsMeters")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14575
try:
    EcsPicoMeters = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsPicoMeters")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14576
try:
    EcsNanoMeters = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsNanoMeters")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14577
try:
    EcsMicroMeters = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsMicroMeters")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14578
try:
    EcsMilliMeters = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsMilliMeters")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14579
try:
    EcsCentiMeters = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsCentiMeters")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14580
try:
    EcsKiloMeters = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsKiloMeters")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14581
try:
    EcsMiles = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsMiles")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14582
try:
    EcsPixels = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsPixels")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14592
try:
    EcsPressure = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsPressure")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14593
try:
    EcsPascal = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsPascal")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14594
try:
    EcsBar = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsBar")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14604
try:
    EcsSpeed = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsSpeed")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14605
try:
    EcsMetersPerSecond = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "EcsMetersPerSecond"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14606
try:
    EcsKiloMetersPerSecond = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "EcsKiloMetersPerSecond"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14607
try:
    EcsKiloMetersPerHour = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "EcsKiloMetersPerHour"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14608
try:
    EcsMilesPerHour = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsMilesPerHour")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14618
try:
    EcsTemperature = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsTemperature")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14619
try:
    EcsKelvin = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsKelvin")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14620
try:
    EcsCelsius = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsCelsius")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14621
try:
    EcsFahrenheit = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsFahrenheit")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14631
try:
    EcsData = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsData")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14632
try:
    EcsBits = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsBits")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14633
try:
    EcsKiloBits = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsKiloBits")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14634
try:
    EcsMegaBits = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsMegaBits")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14635
try:
    EcsGigaBits = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsGigaBits")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14636
try:
    EcsBytes = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsBytes")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14637
try:
    EcsKiloBytes = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsKiloBytes")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14638
try:
    EcsMegaBytes = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsMegaBytes")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14639
try:
    EcsGigaBytes = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsGigaBytes")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14640
try:
    EcsKibiBytes = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsKibiBytes")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14641
try:
    EcsMebiBytes = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsMebiBytes")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14642
try:
    EcsGibiBytes = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsGibiBytes")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14652
try:
    EcsDataRate = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsDataRate")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14653
try:
    EcsBitsPerSecond = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "EcsBitsPerSecond"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14654
try:
    EcsKiloBitsPerSecond = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "EcsKiloBitsPerSecond"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14655
try:
    EcsMegaBitsPerSecond = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "EcsMegaBitsPerSecond"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14656
try:
    EcsGigaBitsPerSecond = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "EcsGigaBitsPerSecond"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14657
try:
    EcsBytesPerSecond = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "EcsBytesPerSecond"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14658
try:
    EcsKiloBytesPerSecond = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "EcsKiloBytesPerSecond"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14659
try:
    EcsMegaBytesPerSecond = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "EcsMegaBytesPerSecond"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14660
try:
    EcsGigaBytesPerSecond = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "EcsGigaBytesPerSecond"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14670
try:
    EcsAngle = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsAngle")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14671
try:
    EcsRadians = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsRadians")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14672
try:
    EcsDegrees = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsDegrees")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14682
try:
    EcsFrequency = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsFrequency")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14683
try:
    EcsHertz = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsHertz")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14684
try:
    EcsKiloHertz = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsKiloHertz")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14685
try:
    EcsMegaHertz = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsMegaHertz")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14686
try:
    EcsGigaHertz = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsGigaHertz")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14696
try:
    EcsUri = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsUri")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14697
try:
    EcsUriHyperlink = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsUriHyperlink")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14698
try:
    EcsUriImage = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsUriImage")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14699
try:
    EcsUriFile = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsUriFile")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14709
try:
    EcsColor = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsColor")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14710
try:
    EcsColorRgb = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsColorRgb")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14711
try:
    EcsColorHsl = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsColorHsl")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14712
try:
    EcsColorCss = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsColorCss")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14717
try:
    EcsAcceleration = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsAcceleration")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14718
try:
    EcsPercentage = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsPercentage")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14719
try:
    EcsBel = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsBel")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14720
try:
    EcsDeciBel = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsDeciBel")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14735
if _libs["libflecs.dylib"].has("FlecsUnitsImport", "cdecl"):
    FlecsUnitsImport = _libs["libflecs.dylib"].get("FlecsUnitsImport", "cdecl")
    FlecsUnitsImport.argtypes = [POINTER(ecs_world_t)]
    FlecsUnitsImport.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 14870
try:
    FLECS_IDEcsScriptID_ = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "FLECS_IDEcsScriptID_"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14873
try:
    EcsScriptTemplate = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "EcsScriptTemplate"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14873
try:
    FLECS_IDEcsScriptTemplateID_ = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "FLECS_IDEcsScriptTemplateID_"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14876
try:
    FLECS_IDEcsScriptConstVarID_ = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "FLECS_IDEcsScriptConstVarID_"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14879
try:
    FLECS_IDEcsScriptFunctionID_ = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "FLECS_IDEcsScriptFunctionID_"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14882
try:
    FLECS_IDEcsScriptMethodID_ = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "FLECS_IDEcsScriptMethodID_"
    )
except:
    pass


# /Users/cnifi/git/pyflecs/flecs.h: 14885
class struct_ecs_script_template_t(Structure):
    pass


ecs_script_template_t = (
    struct_ecs_script_template_t  # /Users/cnifi/git/pyflecs/flecs.h: 14885
)


# /Users/cnifi/git/pyflecs/flecs.h: 14894
class struct_ecs_script_var_t(Structure):
    pass


struct_ecs_script_var_t.__slots__ = [
    "name",
    "value",
    "type_info",
    "sp",
    "is_const",
]
struct_ecs_script_var_t._fields_ = [
    ("name", String),
    ("value", ecs_value_t),
    ("type_info", POINTER(ecs_type_info_t)),
    ("sp", c_int32),
    ("is_const", c_bool),
]

ecs_script_var_t = struct_ecs_script_var_t  # /Users/cnifi/git/pyflecs/flecs.h: 14894


# /Users/cnifi/git/pyflecs/flecs.h: 14897
class struct_ecs_script_vars_t(Structure):
    pass


struct_ecs_script_vars_t.__slots__ = [
    "parent",
    "sp",
    "var_index",
    "vars",
    "world",
    "stack",
    "cursor",
    "allocator",
]
struct_ecs_script_vars_t._fields_ = [
    ("parent", POINTER(struct_ecs_script_vars_t)),
    ("sp", c_int32),
    ("var_index", ecs_hashmap_t),
    ("vars", ecs_vec_t),
    ("world", POINTER(ecs_world_t)),
    ("stack", POINTER(struct_ecs_stack_t)),
    ("cursor", POINTER(ecs_stack_cursor_t)),
    ("allocator", POINTER(ecs_allocator_t)),
]

ecs_script_vars_t = struct_ecs_script_vars_t  # /Users/cnifi/git/pyflecs/flecs.h: 14908


# /Users/cnifi/git/pyflecs/flecs.h: 14915
class struct_ecs_script_t(Structure):
    pass


struct_ecs_script_t.__slots__ = [
    "world",
    "name",
    "code",
]
struct_ecs_script_t._fields_ = [
    ("world", POINTER(ecs_world_t)),
    ("name", String),
    ("code", String),
]

ecs_script_t = struct_ecs_script_t  # /Users/cnifi/git/pyflecs/flecs.h: 14915


# /Users/cnifi/git/pyflecs/flecs.h: 14918
class struct_ecs_script_runtime_t(Structure):
    pass


ecs_script_runtime_t = (
    struct_ecs_script_runtime_t  # /Users/cnifi/git/pyflecs/flecs.h: 14918
)


# /Users/cnifi/git/pyflecs/flecs.h: 14926
class struct_EcsScript(Structure):
    pass


struct_EcsScript.__slots__ = [
    "script",
    "template_",
]
struct_EcsScript._fields_ = [
    ("script", POINTER(ecs_script_t)),
    ("template_", POINTER(ecs_script_template_t)),
]

EcsScript = struct_EcsScript  # /Users/cnifi/git/pyflecs/flecs.h: 14926


# /Users/cnifi/git/pyflecs/flecs.h: 14933
class struct_ecs_function_ctx_t(Structure):
    pass


struct_ecs_function_ctx_t.__slots__ = [
    "world",
    "function",
    "ctx",
]
struct_ecs_function_ctx_t._fields_ = [
    ("world", POINTER(ecs_world_t)),
    ("function", ecs_entity_t),
    ("ctx", POINTER(None)),
]

ecs_function_ctx_t = (
    struct_ecs_function_ctx_t  # /Users/cnifi/git/pyflecs/flecs.h: 14933
)

ecs_function_callback_t = CFUNCTYPE(
    UNCHECKED(None),
    POINTER(ecs_function_ctx_t),
    c_int32,
    POINTER(ecs_value_t),
    POINTER(ecs_value_t),
)  # /Users/cnifi/git/pyflecs/flecs.h: 14936


# /Users/cnifi/git/pyflecs/flecs.h: 14946
class struct_ecs_script_parameter_t(Structure):
    pass


struct_ecs_script_parameter_t.__slots__ = [
    "name",
    "type",
]
struct_ecs_script_parameter_t._fields_ = [
    ("name", String),
    ("type", ecs_entity_t),
]

ecs_script_parameter_t = (
    struct_ecs_script_parameter_t  # /Users/cnifi/git/pyflecs/flecs.h: 14946
)


# /Users/cnifi/git/pyflecs/flecs.h: 14954
class struct_EcsScriptConstVar(Structure):
    pass


struct_EcsScriptConstVar.__slots__ = [
    "value",
    "type_info",
]
struct_EcsScriptConstVar._fields_ = [
    ("value", ecs_value_t),
    ("type_info", POINTER(ecs_type_info_t)),
]

EcsScriptConstVar = struct_EcsScriptConstVar  # /Users/cnifi/git/pyflecs/flecs.h: 14954


# /Users/cnifi/git/pyflecs/flecs.h: 14964
class struct_EcsScriptFunction(Structure):
    pass


struct_EcsScriptFunction.__slots__ = [
    "return_type",
    "params",
    "callback",
    "ctx",
]
struct_EcsScriptFunction._fields_ = [
    ("return_type", ecs_entity_t),
    ("params", ecs_vec_t),
    ("callback", ecs_function_callback_t),
    ("ctx", POINTER(None)),
]

EcsScriptFunction = struct_EcsScriptFunction  # /Users/cnifi/git/pyflecs/flecs.h: 14964


# /Users/cnifi/git/pyflecs/flecs.h: 14976
class struct_EcsScriptMethod(Structure):
    pass


struct_EcsScriptMethod.__slots__ = [
    "return_type",
    "params",
    "callback",
    "ctx",
]
struct_EcsScriptMethod._fields_ = [
    ("return_type", ecs_entity_t),
    ("params", ecs_vec_t),
    ("callback", ecs_function_callback_t),
    ("ctx", POINTER(None)),
]

EcsScriptMethod = struct_EcsScriptMethod  # /Users/cnifi/git/pyflecs/flecs.h: 14976


# /Users/cnifi/git/pyflecs/flecs.h: 14984
class struct_ecs_script_eval_desc_t(Structure):
    pass


struct_ecs_script_eval_desc_t.__slots__ = [
    "vars",
    "runtime",
]
struct_ecs_script_eval_desc_t._fields_ = [
    ("vars", POINTER(ecs_script_vars_t)),
    ("runtime", POINTER(ecs_script_runtime_t)),
]

ecs_script_eval_desc_t = (
    struct_ecs_script_eval_desc_t  # /Users/cnifi/git/pyflecs/flecs.h: 14984
)

# /Users/cnifi/git/pyflecs/flecs.h: 15001
if _libs["libflecs.dylib"].has("ecs_script_parse", "cdecl"):
    ecs_script_parse = _libs["libflecs.dylib"].get("ecs_script_parse", "cdecl")
    ecs_script_parse.argtypes = [
        POINTER(ecs_world_t),
        String,
        String,
        POINTER(ecs_script_eval_desc_t),
    ]
    ecs_script_parse.restype = POINTER(ecs_script_t)

# /Users/cnifi/git/pyflecs/flecs.h: 15019
if _libs["libflecs.dylib"].has("ecs_script_eval", "cdecl"):
    ecs_script_eval = _libs["libflecs.dylib"].get("ecs_script_eval", "cdecl")
    ecs_script_eval.argtypes = [POINTER(ecs_script_t), POINTER(ecs_script_eval_desc_t)]
    ecs_script_eval.restype = c_int

# /Users/cnifi/git/pyflecs/flecs.h: 15033
if _libs["libflecs.dylib"].has("ecs_script_free", "cdecl"):
    ecs_script_free = _libs["libflecs.dylib"].get("ecs_script_free", "cdecl")
    ecs_script_free.argtypes = [POINTER(ecs_script_t)]
    ecs_script_free.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 15052
if _libs["libflecs.dylib"].has("ecs_script_run", "cdecl"):
    ecs_script_run = _libs["libflecs.dylib"].get("ecs_script_run", "cdecl")
    ecs_script_run.argtypes = [POINTER(ecs_world_t), String, String]
    ecs_script_run.restype = c_int

# /Users/cnifi/git/pyflecs/flecs.h: 15067
if _libs["libflecs.dylib"].has("ecs_script_run_file", "cdecl"):
    ecs_script_run_file = _libs["libflecs.dylib"].get("ecs_script_run_file", "cdecl")
    ecs_script_run_file.argtypes = [POINTER(ecs_world_t), String]
    ecs_script_run_file.restype = c_int

# /Users/cnifi/git/pyflecs/flecs.h: 15085
if _libs["libflecs.dylib"].has("ecs_script_runtime_new", "cdecl"):
    ecs_script_runtime_new = _libs["libflecs.dylib"].get(
        "ecs_script_runtime_new", "cdecl"
    )
    ecs_script_runtime_new.argtypes = []
    ecs_script_runtime_new.restype = POINTER(ecs_script_runtime_t)

# /Users/cnifi/git/pyflecs/flecs.h: 15093
if _libs["libflecs.dylib"].has("ecs_script_runtime_free", "cdecl"):
    ecs_script_runtime_free = _libs["libflecs.dylib"].get(
        "ecs_script_runtime_free", "cdecl"
    )
    ecs_script_runtime_free.argtypes = [POINTER(ecs_script_runtime_t)]
    ecs_script_runtime_free.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 15105
if _libs["libflecs.dylib"].has("ecs_script_ast_to_buf", "cdecl"):
    ecs_script_ast_to_buf = _libs["libflecs.dylib"].get(
        "ecs_script_ast_to_buf", "cdecl"
    )
    ecs_script_ast_to_buf.argtypes = [
        POINTER(ecs_script_t),
        POINTER(ecs_strbuf_t),
        c_bool,
    ]
    ecs_script_ast_to_buf.restype = c_int

# /Users/cnifi/git/pyflecs/flecs.h: 15118
if _libs["libflecs.dylib"].has("ecs_script_ast_to_str", "cdecl"):
    ecs_script_ast_to_str = _libs["libflecs.dylib"].get(
        "ecs_script_ast_to_str", "cdecl"
    )
    ecs_script_ast_to_str.argtypes = [POINTER(ecs_script_t), c_bool]
    if sizeof(c_int) == sizeof(c_void_p):
        ecs_script_ast_to_str.restype = ReturnString
    else:
        ecs_script_ast_to_str.restype = String
        ecs_script_ast_to_str.errcheck = ReturnString


# /Users/cnifi/git/pyflecs/flecs.h: 15130
class struct_ecs_script_desc_t(Structure):
    pass


struct_ecs_script_desc_t.__slots__ = [
    "entity",
    "filename",
    "code",
]
struct_ecs_script_desc_t._fields_ = [
    ("entity", ecs_entity_t),
    ("filename", String),
    ("code", String),
]

ecs_script_desc_t = struct_ecs_script_desc_t  # /Users/cnifi/git/pyflecs/flecs.h: 15130

# /Users/cnifi/git/pyflecs/flecs.h: 15143
if _libs["libflecs.dylib"].has("ecs_script_init", "cdecl"):
    ecs_script_init = _libs["libflecs.dylib"].get("ecs_script_init", "cdecl")
    ecs_script_init.argtypes = [POINTER(ecs_world_t), POINTER(ecs_script_desc_t)]
    ecs_script_init.restype = ecs_entity_t

# /Users/cnifi/git/pyflecs/flecs.h: 15158
if _libs["libflecs.dylib"].has("ecs_script_update", "cdecl"):
    ecs_script_update = _libs["libflecs.dylib"].get("ecs_script_update", "cdecl")
    ecs_script_update.argtypes = [
        POINTER(ecs_world_t),
        ecs_entity_t,
        ecs_entity_t,
        String,
    ]
    ecs_script_update.restype = c_int

# /Users/cnifi/git/pyflecs/flecs.h: 15171
if _libs["libflecs.dylib"].has("ecs_script_clear", "cdecl"):
    ecs_script_clear = _libs["libflecs.dylib"].get("ecs_script_clear", "cdecl")
    ecs_script_clear.argtypes = [POINTER(ecs_world_t), ecs_entity_t, ecs_entity_t]
    ecs_script_clear.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 15196
if _libs["libflecs.dylib"].has("ecs_script_vars_init", "cdecl"):
    ecs_script_vars_init = _libs["libflecs.dylib"].get("ecs_script_vars_init", "cdecl")
    ecs_script_vars_init.argtypes = [POINTER(ecs_world_t)]
    ecs_script_vars_init.restype = POINTER(ecs_script_vars_t)

# /Users/cnifi/git/pyflecs/flecs.h: 15206
if _libs["libflecs.dylib"].has("ecs_script_vars_fini", "cdecl"):
    ecs_script_vars_fini = _libs["libflecs.dylib"].get("ecs_script_vars_fini", "cdecl")
    ecs_script_vars_fini.argtypes = [POINTER(ecs_script_vars_t)]
    ecs_script_vars_fini.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 15221
if _libs["libflecs.dylib"].has("ecs_script_vars_push", "cdecl"):
    ecs_script_vars_push = _libs["libflecs.dylib"].get("ecs_script_vars_push", "cdecl")
    ecs_script_vars_push.argtypes = [POINTER(ecs_script_vars_t)]
    ecs_script_vars_push.restype = POINTER(ecs_script_vars_t)

# /Users/cnifi/git/pyflecs/flecs.h: 15233
if _libs["libflecs.dylib"].has("ecs_script_vars_pop", "cdecl"):
    ecs_script_vars_pop = _libs["libflecs.dylib"].get("ecs_script_vars_pop", "cdecl")
    ecs_script_vars_pop.argtypes = [POINTER(ecs_script_vars_t)]
    ecs_script_vars_pop.restype = POINTER(ecs_script_vars_t)

# /Users/cnifi/git/pyflecs/flecs.h: 15249
if _libs["libflecs.dylib"].has("ecs_script_vars_declare", "cdecl"):
    ecs_script_vars_declare = _libs["libflecs.dylib"].get(
        "ecs_script_vars_declare", "cdecl"
    )
    ecs_script_vars_declare.argtypes = [POINTER(ecs_script_vars_t), String]
    ecs_script_vars_declare.restype = POINTER(ecs_script_var_t)

# /Users/cnifi/git/pyflecs/flecs.h: 15269
if _libs["libflecs.dylib"].has("ecs_script_vars_define_id", "cdecl"):
    ecs_script_vars_define_id = _libs["libflecs.dylib"].get(
        "ecs_script_vars_define_id", "cdecl"
    )
    ecs_script_vars_define_id.argtypes = [
        POINTER(ecs_script_vars_t),
        String,
        ecs_entity_t,
    ]
    ecs_script_vars_define_id.restype = POINTER(ecs_script_var_t)

# /Users/cnifi/git/pyflecs/flecs.h: 15287
if _libs["libflecs.dylib"].has("ecs_script_vars_lookup", "cdecl"):
    ecs_script_vars_lookup = _libs["libflecs.dylib"].get(
        "ecs_script_vars_lookup", "cdecl"
    )
    ecs_script_vars_lookup.argtypes = [POINTER(ecs_script_vars_t), String]
    ecs_script_vars_lookup.restype = POINTER(ecs_script_var_t)

# /Users/cnifi/git/pyflecs/flecs.h: 15304
if _libs["libflecs.dylib"].has("ecs_script_vars_from_sp", "cdecl"):
    ecs_script_vars_from_sp = _libs["libflecs.dylib"].get(
        "ecs_script_vars_from_sp", "cdecl"
    )
    ecs_script_vars_from_sp.argtypes = [POINTER(ecs_script_vars_t), c_int32]
    ecs_script_vars_from_sp.restype = POINTER(ecs_script_var_t)

# /Users/cnifi/git/pyflecs/flecs.h: 15314
if _libs["libflecs.dylib"].has("ecs_script_vars_print", "cdecl"):
    ecs_script_vars_print = _libs["libflecs.dylib"].get(
        "ecs_script_vars_print", "cdecl"
    )
    ecs_script_vars_print.argtypes = [POINTER(ecs_script_vars_t)]
    ecs_script_vars_print.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 15326
if _libs["libflecs.dylib"].has("ecs_script_vars_set_size", "cdecl"):
    ecs_script_vars_set_size = _libs["libflecs.dylib"].get(
        "ecs_script_vars_set_size", "cdecl"
    )
    ecs_script_vars_set_size.argtypes = [POINTER(ecs_script_vars_t), c_int32]
    ecs_script_vars_set_size.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 15363
if _libs["libflecs.dylib"].has("ecs_script_vars_from_iter", "cdecl"):
    ecs_script_vars_from_iter = _libs["libflecs.dylib"].get(
        "ecs_script_vars_from_iter", "cdecl"
    )
    ecs_script_vars_from_iter.argtypes = [
        POINTER(ecs_iter_t),
        POINTER(ecs_script_vars_t),
        c_int,
    ]
    ecs_script_vars_from_iter.restype = None


# /Users/cnifi/git/pyflecs/flecs.h: 15396
class struct_ecs_expr_eval_desc_t(Structure):
    pass


struct_ecs_expr_eval_desc_t.__slots__ = [
    "name",
    "expr",
    "vars",
    "type",
    "lookup_action",
    "lookup_ctx",
    "disable_folding",
    "disable_dynamic_variable_binding",
    "allow_unresolved_identifiers",
    "runtime",
]
struct_ecs_expr_eval_desc_t._fields_ = [
    ("name", String),
    ("expr", String),
    ("vars", POINTER(ecs_script_vars_t)),
    ("type", ecs_entity_t),
    (
        "lookup_action",
        CFUNCTYPE(UNCHECKED(ecs_entity_t), POINTER(ecs_world_t), String, POINTER(None)),
    ),
    ("lookup_ctx", POINTER(None)),
    ("disable_folding", c_bool),
    ("disable_dynamic_variable_binding", c_bool),
    ("allow_unresolved_identifiers", c_bool),
    ("runtime", POINTER(ecs_script_runtime_t)),
]

ecs_expr_eval_desc_t = (
    struct_ecs_expr_eval_desc_t  # /Users/cnifi/git/pyflecs/flecs.h: 15396
)

# /Users/cnifi/git/pyflecs/flecs.h: 15413
if _libs["libflecs.dylib"].has("ecs_expr_run", "cdecl"):
    ecs_expr_run = _libs["libflecs.dylib"].get("ecs_expr_run", "cdecl")
    ecs_expr_run.argtypes = [
        POINTER(ecs_world_t),
        String,
        POINTER(ecs_value_t),
        POINTER(ecs_expr_eval_desc_t),
    ]
    ecs_expr_run.restype = c_char_p

# /Users/cnifi/git/pyflecs/flecs.h: 15429
if _libs["libflecs.dylib"].has("ecs_expr_parse", "cdecl"):
    ecs_expr_parse = _libs["libflecs.dylib"].get("ecs_expr_parse", "cdecl")
    ecs_expr_parse.argtypes = [
        POINTER(ecs_world_t),
        String,
        POINTER(ecs_expr_eval_desc_t),
    ]
    ecs_expr_parse.restype = POINTER(ecs_script_t)

# /Users/cnifi/git/pyflecs/flecs.h: 15449
if _libs["libflecs.dylib"].has("ecs_expr_eval", "cdecl"):
    ecs_expr_eval = _libs["libflecs.dylib"].get("ecs_expr_eval", "cdecl")
    ecs_expr_eval.argtypes = [
        POINTER(ecs_script_t),
        POINTER(ecs_value_t),
        POINTER(ecs_expr_eval_desc_t),
    ]
    ecs_expr_eval.restype = c_int

# /Users/cnifi/git/pyflecs/flecs.h: 15467
if _libs["libflecs.dylib"].has("ecs_script_string_interpolate", "cdecl"):
    ecs_script_string_interpolate = _libs["libflecs.dylib"].get(
        "ecs_script_string_interpolate", "cdecl"
    )
    ecs_script_string_interpolate.argtypes = [
        POINTER(ecs_world_t),
        String,
        POINTER(ecs_script_vars_t),
    ]
    if sizeof(c_int) == sizeof(c_void_p):
        ecs_script_string_interpolate.restype = ReturnString
    else:
        ecs_script_string_interpolate.restype = String
        ecs_script_string_interpolate.errcheck = ReturnString


# /Users/cnifi/git/pyflecs/flecs.h: 15489
class struct_ecs_const_var_desc_t(Structure):
    pass


struct_ecs_const_var_desc_t.__slots__ = [
    "name",
    "parent",
    "type",
    "value",
]
struct_ecs_const_var_desc_t._fields_ = [
    ("name", String),
    ("parent", ecs_entity_t),
    ("type", ecs_entity_t),
    ("value", POINTER(None)),
]

ecs_const_var_desc_t = (
    struct_ecs_const_var_desc_t  # /Users/cnifi/git/pyflecs/flecs.h: 15489
)

# /Users/cnifi/git/pyflecs/flecs.h: 15498
if _libs["libflecs.dylib"].has("ecs_const_var_init", "cdecl"):
    ecs_const_var_init = _libs["libflecs.dylib"].get("ecs_const_var_init", "cdecl")
    ecs_const_var_init.argtypes = [POINTER(ecs_world_t), POINTER(ecs_const_var_desc_t)]
    ecs_const_var_init.restype = ecs_entity_t


# /Users/cnifi/git/pyflecs/flecs.h: 15527
class struct_ecs_function_desc_t(Structure):
    pass


struct_ecs_function_desc_t.__slots__ = [
    "name",
    "parent",
    "params",
    "return_type",
    "callback",
    "ctx",
]
struct_ecs_function_desc_t._fields_ = [
    ("name", String),
    ("parent", ecs_entity_t),
    ("params", ecs_script_parameter_t * int(16)),
    ("return_type", ecs_entity_t),
    ("callback", ecs_function_callback_t),
    ("ctx", POINTER(None)),
]

ecs_function_desc_t = (
    struct_ecs_function_desc_t  # /Users/cnifi/git/pyflecs/flecs.h: 15527
)

# /Users/cnifi/git/pyflecs/flecs.h: 15537
if _libs["libflecs.dylib"].has("ecs_function_init", "cdecl"):
    ecs_function_init = _libs["libflecs.dylib"].get("ecs_function_init", "cdecl")
    ecs_function_init.argtypes = [POINTER(ecs_world_t), POINTER(ecs_function_desc_t)]
    ecs_function_init.restype = ecs_entity_t

# /Users/cnifi/git/pyflecs/flecs.h: 15557
if _libs["libflecs.dylib"].has("ecs_method_init", "cdecl"):
    ecs_method_init = _libs["libflecs.dylib"].get("ecs_method_init", "cdecl")
    ecs_method_init.argtypes = [POINTER(ecs_world_t), POINTER(ecs_function_desc_t)]
    ecs_method_init.restype = ecs_entity_t

# /Users/cnifi/git/pyflecs/flecs.h: 15577
if _libs["libflecs.dylib"].has("ecs_ptr_to_expr", "cdecl"):
    ecs_ptr_to_expr = _libs["libflecs.dylib"].get("ecs_ptr_to_expr", "cdecl")
    ecs_ptr_to_expr.argtypes = [POINTER(ecs_world_t), ecs_entity_t, POINTER(None)]
    if sizeof(c_int) == sizeof(c_void_p):
        ecs_ptr_to_expr.restype = ReturnString
    else:
        ecs_ptr_to_expr.restype = String
        ecs_ptr_to_expr.errcheck = ReturnString

# /Users/cnifi/git/pyflecs/flecs.h: 15592
if _libs["libflecs.dylib"].has("ecs_ptr_to_expr_buf", "cdecl"):
    ecs_ptr_to_expr_buf = _libs["libflecs.dylib"].get("ecs_ptr_to_expr_buf", "cdecl")
    ecs_ptr_to_expr_buf.argtypes = [
        POINTER(ecs_world_t),
        ecs_entity_t,
        POINTER(None),
        POINTER(ecs_strbuf_t),
    ]
    ecs_ptr_to_expr_buf.restype = c_int

# /Users/cnifi/git/pyflecs/flecs.h: 15610
if _libs["libflecs.dylib"].has("ecs_ptr_to_str", "cdecl"):
    ecs_ptr_to_str = _libs["libflecs.dylib"].get("ecs_ptr_to_str", "cdecl")
    ecs_ptr_to_str.argtypes = [POINTER(ecs_world_t), ecs_entity_t, POINTER(None)]
    if sizeof(c_int) == sizeof(c_void_p):
        ecs_ptr_to_str.restype = ReturnString
    else:
        ecs_ptr_to_str.restype = String
        ecs_ptr_to_str.errcheck = ReturnString

# /Users/cnifi/git/pyflecs/flecs.h: 15625
if _libs["libflecs.dylib"].has("ecs_ptr_to_str_buf", "cdecl"):
    ecs_ptr_to_str_buf = _libs["libflecs.dylib"].get("ecs_ptr_to_str_buf", "cdecl")
    ecs_ptr_to_str_buf.argtypes = [
        POINTER(ecs_world_t),
        ecs_entity_t,
        POINTER(None),
        POINTER(ecs_strbuf_t),
    ]
    ecs_ptr_to_str_buf.restype = c_int


# /Users/cnifi/git/pyflecs/flecs.h: 15631
class struct_ecs_expr_node_t(Structure):
    pass


ecs_expr_node_t = struct_ecs_expr_node_t  # /Users/cnifi/git/pyflecs/flecs.h: 15631

# /Users/cnifi/git/pyflecs/flecs.h: 15642
if _libs["libflecs.dylib"].has("FlecsScriptImport", "cdecl"):
    FlecsScriptImport = _libs["libflecs.dylib"].get("FlecsScriptImport", "cdecl")
    FlecsScriptImport.argtypes = [POINTER(ecs_world_t)]
    FlecsScriptImport.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 15692
try:
    FLECS_IDEcsDocDescriptionID_ = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "FLECS_IDEcsDocDescriptionID_"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 15697
try:
    EcsDocUuid = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsDocUuid")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 15702
try:
    EcsDocBrief = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsDocBrief")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 15707
try:
    EcsDocDetail = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsDocDetail")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 15712
try:
    EcsDocLink = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsDocLink")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 15717
try:
    EcsDocColor = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsDocColor")
except:
    pass


# /Users/cnifi/git/pyflecs/flecs.h: 15729
class struct_EcsDocDescription(Structure):
    pass


struct_EcsDocDescription.__slots__ = [
    "value",
]
struct_EcsDocDescription._fields_ = [
    ("value", String),
]

EcsDocDescription = struct_EcsDocDescription  # /Users/cnifi/git/pyflecs/flecs.h: 15729

# /Users/cnifi/git/pyflecs/flecs.h: 15743
if _libs["libflecs.dylib"].has("ecs_doc_set_uuid", "cdecl"):
    ecs_doc_set_uuid = _libs["libflecs.dylib"].get("ecs_doc_set_uuid", "cdecl")
    ecs_doc_set_uuid.argtypes = [POINTER(ecs_world_t), ecs_entity_t, String]
    ecs_doc_set_uuid.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 15761
if _libs["libflecs.dylib"].has("ecs_doc_set_name", "cdecl"):
    ecs_doc_set_name = _libs["libflecs.dylib"].get("ecs_doc_set_name", "cdecl")
    ecs_doc_set_name.argtypes = [POINTER(ecs_world_t), ecs_entity_t, String]
    ecs_doc_set_name.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 15777
if _libs["libflecs.dylib"].has("ecs_doc_set_brief", "cdecl"):
    ecs_doc_set_brief = _libs["libflecs.dylib"].get("ecs_doc_set_brief", "cdecl")
    ecs_doc_set_brief.argtypes = [POINTER(ecs_world_t), ecs_entity_t, String]
    ecs_doc_set_brief.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 15793
if _libs["libflecs.dylib"].has("ecs_doc_set_detail", "cdecl"):
    ecs_doc_set_detail = _libs["libflecs.dylib"].get("ecs_doc_set_detail", "cdecl")
    ecs_doc_set_detail.argtypes = [POINTER(ecs_world_t), ecs_entity_t, String]
    ecs_doc_set_detail.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 15809
if _libs["libflecs.dylib"].has("ecs_doc_set_link", "cdecl"):
    ecs_doc_set_link = _libs["libflecs.dylib"].get("ecs_doc_set_link", "cdecl")
    ecs_doc_set_link.argtypes = [POINTER(ecs_world_t), ecs_entity_t, String]
    ecs_doc_set_link.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 15826
if _libs["libflecs.dylib"].has("ecs_doc_set_color", "cdecl"):
    ecs_doc_set_color = _libs["libflecs.dylib"].get("ecs_doc_set_color", "cdecl")
    ecs_doc_set_color.argtypes = [POINTER(ecs_world_t), ecs_entity_t, String]
    ecs_doc_set_color.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 15841
if _libs["libflecs.dylib"].has("ecs_doc_get_uuid", "cdecl"):
    ecs_doc_get_uuid = _libs["libflecs.dylib"].get("ecs_doc_get_uuid", "cdecl")
    ecs_doc_get_uuid.argtypes = [POINTER(ecs_world_t), ecs_entity_t]
    ecs_doc_get_uuid.restype = c_char_p

# /Users/cnifi/git/pyflecs/flecs.h: 15870
if _libs["libflecs.dylib"].has("ecs_doc_get_name", "cdecl"):
    ecs_doc_get_name = _libs["libflecs.dylib"].get("ecs_doc_get_name", "cdecl")
    ecs_doc_get_name.argtypes = [POINTER(ecs_world_t), ecs_entity_t]
    ecs_doc_get_name.restype = c_char_p

# /Users/cnifi/git/pyflecs/flecs.h: 15885
if _libs["libflecs.dylib"].has("ecs_doc_get_brief", "cdecl"):
    ecs_doc_get_brief = _libs["libflecs.dylib"].get("ecs_doc_get_brief", "cdecl")
    ecs_doc_get_brief.argtypes = [POINTER(ecs_world_t), ecs_entity_t]
    ecs_doc_get_brief.restype = c_char_p

# /Users/cnifi/git/pyflecs/flecs.h: 15900
if _libs["libflecs.dylib"].has("ecs_doc_get_detail", "cdecl"):
    ecs_doc_get_detail = _libs["libflecs.dylib"].get("ecs_doc_get_detail", "cdecl")
    ecs_doc_get_detail.argtypes = [POINTER(ecs_world_t), ecs_entity_t]
    ecs_doc_get_detail.restype = c_char_p

# /Users/cnifi/git/pyflecs/flecs.h: 15915
if _libs["libflecs.dylib"].has("ecs_doc_get_link", "cdecl"):
    ecs_doc_get_link = _libs["libflecs.dylib"].get("ecs_doc_get_link", "cdecl")
    ecs_doc_get_link.argtypes = [POINTER(ecs_world_t), ecs_entity_t]
    ecs_doc_get_link.restype = c_char_p

# /Users/cnifi/git/pyflecs/flecs.h: 15930
if _libs["libflecs.dylib"].has("ecs_doc_get_color", "cdecl"):
    ecs_doc_get_color = _libs["libflecs.dylib"].get("ecs_doc_get_color", "cdecl")
    ecs_doc_get_color.argtypes = [POINTER(ecs_world_t), ecs_entity_t]
    ecs_doc_get_color.restype = c_char_p

# /Users/cnifi/git/pyflecs/flecs.h: 15943
if _libs["libflecs.dylib"].has("FlecsDocImport", "cdecl"):
    FlecsDocImport = _libs["libflecs.dylib"].get("FlecsDocImport", "cdecl")
    FlecsDocImport.argtypes = [POINTER(ecs_world_t)]
    FlecsDocImport.restype = None

ecs_bool_t = c_bool  # /Users/cnifi/git/pyflecs/flecs.h: 16056

ecs_char_t = c_char  # /Users/cnifi/git/pyflecs/flecs.h: 16057

ecs_byte_t = c_ubyte  # /Users/cnifi/git/pyflecs/flecs.h: 16058

ecs_u8_t = uint8_t  # /Users/cnifi/git/pyflecs/flecs.h: 16059

ecs_u16_t = uint16_t  # /Users/cnifi/git/pyflecs/flecs.h: 16060

ecs_u32_t = uint32_t  # /Users/cnifi/git/pyflecs/flecs.h: 16061

ecs_u64_t = uint64_t  # /Users/cnifi/git/pyflecs/flecs.h: 16062

ecs_uptr_t = uintptr_t  # /Users/cnifi/git/pyflecs/flecs.h: 16063

ecs_i8_t = c_int8  # /Users/cnifi/git/pyflecs/flecs.h: 16064

ecs_i16_t = c_int16  # /Users/cnifi/git/pyflecs/flecs.h: 16065

ecs_i32_t = c_int32  # /Users/cnifi/git/pyflecs/flecs.h: 16066

ecs_i64_t = c_int64  # /Users/cnifi/git/pyflecs/flecs.h: 16067

ecs_iptr_t = intptr_t  # /Users/cnifi/git/pyflecs/flecs.h: 16068

ecs_f32_t = c_float  # /Users/cnifi/git/pyflecs/flecs.h: 16069

ecs_f64_t = c_double  # /Users/cnifi/git/pyflecs/flecs.h: 16070

ecs_string_t = String  # /Users/cnifi/git/pyflecs/flecs.h: 16071

# /Users/cnifi/git/pyflecs/flecs.h: 16074
try:
    FLECS_IDEcsTypeID_ = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "FLECS_IDEcsTypeID_"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 16075
try:
    FLECS_IDEcsTypeSerializerID_ = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "FLECS_IDEcsTypeSerializerID_"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 16076
try:
    FLECS_IDEcsPrimitiveID_ = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "FLECS_IDEcsPrimitiveID_"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 16077
try:
    FLECS_IDEcsEnumID_ = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "FLECS_IDEcsEnumID_"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 16078
try:
    FLECS_IDEcsBitmaskID_ = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "FLECS_IDEcsBitmaskID_"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 16079
try:
    FLECS_IDEcsMemberID_ = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "FLECS_IDEcsMemberID_"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 16080
try:
    FLECS_IDEcsMemberRangesID_ = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "FLECS_IDEcsMemberRangesID_"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 16081
try:
    FLECS_IDEcsStructID_ = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "FLECS_IDEcsStructID_"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 16082
try:
    FLECS_IDEcsArrayID_ = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "FLECS_IDEcsArrayID_"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 16083
try:
    FLECS_IDEcsVectorID_ = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "FLECS_IDEcsVectorID_"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 16084
try:
    FLECS_IDEcsOpaqueID_ = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "FLECS_IDEcsOpaqueID_"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 16085
try:
    FLECS_IDEcsUnitID_ = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "FLECS_IDEcsUnitID_"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 16086
try:
    FLECS_IDEcsUnitPrefixID_ = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "FLECS_IDEcsUnitPrefixID_"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 16087
try:
    EcsQuantity = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsQuantity")
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 16091
try:
    FLECS_IDecs_bool_tID_ = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "FLECS_IDecs_bool_tID_"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 16092
try:
    FLECS_IDecs_char_tID_ = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "FLECS_IDecs_char_tID_"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 16093
try:
    FLECS_IDecs_byte_tID_ = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "FLECS_IDecs_byte_tID_"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 16094
try:
    FLECS_IDecs_u8_tID_ = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "FLECS_IDecs_u8_tID_"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 16095
try:
    FLECS_IDecs_u16_tID_ = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "FLECS_IDecs_u16_tID_"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 16096
try:
    FLECS_IDecs_u32_tID_ = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "FLECS_IDecs_u32_tID_"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 16097
try:
    FLECS_IDecs_u64_tID_ = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "FLECS_IDecs_u64_tID_"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 16098
try:
    FLECS_IDecs_uptr_tID_ = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "FLECS_IDecs_uptr_tID_"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 16099
try:
    FLECS_IDecs_i8_tID_ = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "FLECS_IDecs_i8_tID_"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 16100
try:
    FLECS_IDecs_i16_tID_ = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "FLECS_IDecs_i16_tID_"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 16101
try:
    FLECS_IDecs_i32_tID_ = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "FLECS_IDecs_i32_tID_"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 16102
try:
    FLECS_IDecs_i64_tID_ = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "FLECS_IDecs_i64_tID_"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 16103
try:
    FLECS_IDecs_iptr_tID_ = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "FLECS_IDecs_iptr_tID_"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 16104
try:
    FLECS_IDecs_f32_tID_ = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "FLECS_IDecs_f32_tID_"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 16105
try:
    FLECS_IDecs_f64_tID_ = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "FLECS_IDecs_f64_tID_"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 16106
try:
    FLECS_IDecs_string_tID_ = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "FLECS_IDecs_string_tID_"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 16107
try:
    FLECS_IDecs_entity_tID_ = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "FLECS_IDecs_entity_tID_"
    )
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 16108
try:
    FLECS_IDecs_id_tID_ = (ecs_entity_t).in_dll(
        _libs["libflecs.dylib"], "FLECS_IDecs_id_tID_"
    )
except:
    pass

enum_ecs_type_kind_t = c_int  # /Users/cnifi/git/pyflecs/flecs.h: 16120

EcsPrimitiveType = 0  # /Users/cnifi/git/pyflecs/flecs.h: 16120

EcsBitmaskType = EcsPrimitiveType + 1  # /Users/cnifi/git/pyflecs/flecs.h: 16120

EcsEnumType = EcsBitmaskType + 1  # /Users/cnifi/git/pyflecs/flecs.h: 16120

EcsStructType = EcsEnumType + 1  # /Users/cnifi/git/pyflecs/flecs.h: 16120

EcsArrayType = EcsStructType + 1  # /Users/cnifi/git/pyflecs/flecs.h: 16120

EcsVectorType = EcsArrayType + 1  # /Users/cnifi/git/pyflecs/flecs.h: 16120

EcsOpaqueType = EcsVectorType + 1  # /Users/cnifi/git/pyflecs/flecs.h: 16120

EcsTypeKindLast = EcsOpaqueType  # /Users/cnifi/git/pyflecs/flecs.h: 16120

ecs_type_kind_t = enum_ecs_type_kind_t  # /Users/cnifi/git/pyflecs/flecs.h: 16120


# /Users/cnifi/git/pyflecs/flecs.h: 16127
class struct_EcsType(Structure):
    pass


struct_EcsType.__slots__ = [
    "kind",
    "existing",
    "partial",
]
struct_EcsType._fields_ = [
    ("kind", ecs_type_kind_t),
    ("existing", c_bool),
    ("partial", c_bool),
]

EcsType = struct_EcsType  # /Users/cnifi/git/pyflecs/flecs.h: 16127

enum_ecs_primitive_kind_t = c_int  # /Users/cnifi/git/pyflecs/flecs.h: 16150

EcsBool = 1  # /Users/cnifi/git/pyflecs/flecs.h: 16150

EcsChar = EcsBool + 1  # /Users/cnifi/git/pyflecs/flecs.h: 16150

EcsByte = EcsChar + 1  # /Users/cnifi/git/pyflecs/flecs.h: 16150

EcsU8 = EcsByte + 1  # /Users/cnifi/git/pyflecs/flecs.h: 16150

EcsU16 = EcsU8 + 1  # /Users/cnifi/git/pyflecs/flecs.h: 16150

EcsU32 = EcsU16 + 1  # /Users/cnifi/git/pyflecs/flecs.h: 16150

EcsU64 = EcsU32 + 1  # /Users/cnifi/git/pyflecs/flecs.h: 16150

EcsI8 = EcsU64 + 1  # /Users/cnifi/git/pyflecs/flecs.h: 16150

EcsI16 = EcsI8 + 1  # /Users/cnifi/git/pyflecs/flecs.h: 16150

EcsI32 = EcsI16 + 1  # /Users/cnifi/git/pyflecs/flecs.h: 16150

EcsI64 = EcsI32 + 1  # /Users/cnifi/git/pyflecs/flecs.h: 16150

EcsF32 = EcsI64 + 1  # /Users/cnifi/git/pyflecs/flecs.h: 16150

EcsF64 = EcsF32 + 1  # /Users/cnifi/git/pyflecs/flecs.h: 16150

EcsUPtr = EcsF64 + 1  # /Users/cnifi/git/pyflecs/flecs.h: 16150

EcsIPtr = EcsUPtr + 1  # /Users/cnifi/git/pyflecs/flecs.h: 16150

EcsString = EcsIPtr + 1  # /Users/cnifi/git/pyflecs/flecs.h: 16150

EcsEntity = EcsString + 1  # /Users/cnifi/git/pyflecs/flecs.h: 16150

EcsId = EcsEntity + 1  # /Users/cnifi/git/pyflecs/flecs.h: 16150

EcsPrimitiveKindLast = EcsId  # /Users/cnifi/git/pyflecs/flecs.h: 16150

ecs_primitive_kind_t = (
    enum_ecs_primitive_kind_t  # /Users/cnifi/git/pyflecs/flecs.h: 16150
)


# /Users/cnifi/git/pyflecs/flecs.h: 16155
class struct_EcsPrimitive(Structure):
    pass


struct_EcsPrimitive.__slots__ = [
    "kind",
]
struct_EcsPrimitive._fields_ = [
    ("kind", ecs_primitive_kind_t),
]

EcsPrimitive = struct_EcsPrimitive  # /Users/cnifi/git/pyflecs/flecs.h: 16155


# /Users/cnifi/git/pyflecs/flecs.h: 16164
class struct_EcsMember(Structure):
    pass


struct_EcsMember.__slots__ = [
    "type",
    "count",
    "unit",
    "offset",
    "use_offset",
]
struct_EcsMember._fields_ = [
    ("type", ecs_entity_t),
    ("count", c_int32),
    ("unit", ecs_entity_t),
    ("offset", c_int32),
    ("use_offset", c_bool),
]

EcsMember = struct_EcsMember  # /Users/cnifi/git/pyflecs/flecs.h: 16164


# /Users/cnifi/git/pyflecs/flecs.h: 16170
class struct_ecs_member_value_range_t(Structure):
    pass


struct_ecs_member_value_range_t.__slots__ = [
    "min",
    "max",
]
struct_ecs_member_value_range_t._fields_ = [
    ("min", c_double),
    ("max", c_double),
]

ecs_member_value_range_t = (
    struct_ecs_member_value_range_t  # /Users/cnifi/git/pyflecs/flecs.h: 16170
)


# /Users/cnifi/git/pyflecs/flecs.h: 16177
class struct_EcsMemberRanges(Structure):
    pass


struct_EcsMemberRanges.__slots__ = [
    "value",
    "warning",
    "error",
]
struct_EcsMemberRanges._fields_ = [
    ("value", ecs_member_value_range_t),
    ("warning", ecs_member_value_range_t),
    ("error", ecs_member_value_range_t),
]

EcsMemberRanges = struct_EcsMemberRanges  # /Users/cnifi/git/pyflecs/flecs.h: 16177


# /Users/cnifi/git/pyflecs/flecs.h: 16220
class struct_ecs_member_t(Structure):
    pass


struct_ecs_member_t.__slots__ = [
    "name",
    "type",
    "count",
    "offset",
    "unit",
    "use_offset",
    "range",
    "error_range",
    "warning_range",
    "size",
    "member",
]
struct_ecs_member_t._fields_ = [
    ("name", String),
    ("type", ecs_entity_t),
    ("count", c_int32),
    ("offset", c_int32),
    ("unit", ecs_entity_t),
    ("use_offset", c_bool),
    ("range", ecs_member_value_range_t),
    ("error_range", ecs_member_value_range_t),
    ("warning_range", ecs_member_value_range_t),
    ("size", ecs_size_t),
    ("member", ecs_entity_t),
]

ecs_member_t = struct_ecs_member_t  # /Users/cnifi/git/pyflecs/flecs.h: 16220


# /Users/cnifi/git/pyflecs/flecs.h: 16226
class struct_EcsStruct(Structure):
    pass


struct_EcsStruct.__slots__ = [
    "members",
]
struct_EcsStruct._fields_ = [
    ("members", ecs_vec_t),
]

EcsStruct = struct_EcsStruct  # /Users/cnifi/git/pyflecs/flecs.h: 16226


# /Users/cnifi/git/pyflecs/flecs.h: 16241
class struct_ecs_enum_constant_t(Structure):
    pass


struct_ecs_enum_constant_t.__slots__ = [
    "name",
    "value",
    "value_unsigned",
    "constant",
]
struct_ecs_enum_constant_t._fields_ = [
    ("name", String),
    ("value", c_int64),
    ("value_unsigned", uint64_t),
    ("constant", ecs_entity_t),
]

ecs_enum_constant_t = (
    struct_ecs_enum_constant_t  # /Users/cnifi/git/pyflecs/flecs.h: 16241
)


# /Users/cnifi/git/pyflecs/flecs.h: 16252
class struct_EcsEnum(Structure):
    pass


struct_EcsEnum.__slots__ = [
    "underlying_type",
    "constants",
    "ordered_constants",
]
struct_EcsEnum._fields_ = [
    ("underlying_type", ecs_entity_t),
    ("constants", ecs_map_t),
    ("ordered_constants", ecs_vec_t),
]

EcsEnum = struct_EcsEnum  # /Users/cnifi/git/pyflecs/flecs.h: 16252


# /Users/cnifi/git/pyflecs/flecs.h: 16267
class struct_ecs_bitmask_constant_t(Structure):
    pass


struct_ecs_bitmask_constant_t.__slots__ = [
    "name",
    "value",
    "_unused",
    "constant",
]
struct_ecs_bitmask_constant_t._fields_ = [
    ("name", String),
    ("value", ecs_flags64_t),
    ("_unused", c_int64),
    ("constant", ecs_entity_t),
]

ecs_bitmask_constant_t = (
    struct_ecs_bitmask_constant_t  # /Users/cnifi/git/pyflecs/flecs.h: 16267
)


# /Users/cnifi/git/pyflecs/flecs.h: 16275
class struct_EcsBitmask(Structure):
    pass


struct_EcsBitmask.__slots__ = [
    "constants",
    "ordered_constants",
]
struct_EcsBitmask._fields_ = [
    ("constants", ecs_map_t),
    ("ordered_constants", ecs_vec_t),
]

EcsBitmask = struct_EcsBitmask  # /Users/cnifi/git/pyflecs/flecs.h: 16275


# /Users/cnifi/git/pyflecs/flecs.h: 16281
class struct_EcsArray(Structure):
    pass


struct_EcsArray.__slots__ = [
    "type",
    "count",
]
struct_EcsArray._fields_ = [
    ("type", ecs_entity_t),
    ("count", c_int32),
]

EcsArray = struct_EcsArray  # /Users/cnifi/git/pyflecs/flecs.h: 16281


# /Users/cnifi/git/pyflecs/flecs.h: 16286
class struct_EcsVector(Structure):
    pass


struct_EcsVector.__slots__ = [
    "type",
]
struct_EcsVector._fields_ = [
    ("type", ecs_entity_t),
]

EcsVector = struct_EcsVector  # /Users/cnifi/git/pyflecs/flecs.h: 16286


# /Users/cnifi/git/pyflecs/flecs.h: 16294
class struct_ecs_serializer_t(Structure):
    pass


struct_ecs_serializer_t.__slots__ = [
    "value",
    "member",
    "world",
    "ctx",
]
struct_ecs_serializer_t._fields_ = [
    (
        "value",
        CFUNCTYPE(
            UNCHECKED(c_int),
            POINTER(struct_ecs_serializer_t),
            ecs_entity_t,
            POINTER(None),
        ),
    ),
    ("member", CFUNCTYPE(UNCHECKED(c_int), POINTER(struct_ecs_serializer_t), String)),
    ("world", POINTER(ecs_world_t)),
    ("ctx", POINTER(None)),
]

ecs_serializer_t = struct_ecs_serializer_t  # /Users/cnifi/git/pyflecs/flecs.h: 16308

ecs_meta_serialize_t = CFUNCTYPE(
    UNCHECKED(c_int), POINTER(ecs_serializer_t), POINTER(None)
)  # /Users/cnifi/git/pyflecs/flecs.h: 16345

ecs_meta_serialize_member_t = CFUNCTYPE(
    UNCHECKED(c_int), POINTER(ecs_serializer_t), POINTER(None), String
)  # /Users/cnifi/git/pyflecs/flecs.h: 16351

ecs_meta_serialize_element_t = CFUNCTYPE(
    UNCHECKED(c_int), POINTER(ecs_serializer_t), POINTER(None), c_size_t
)  # /Users/cnifi/git/pyflecs/flecs.h: 16357


# /Users/cnifi/git/pyflecs/flecs.h: 16446
class struct_EcsOpaque(Structure):
    pass


struct_EcsOpaque.__slots__ = [
    "as_type",
    "serialize",
    "serialize_member",
    "serialize_element",
    "assign_bool",
    "assign_char",
    "assign_int",
    "assign_uint",
    "assign_float",
    "assign_string",
    "assign_entity",
    "assign_id",
    "assign_null",
    "clear",
    "ensure_element",
    "ensure_member",
    "count",
    "resize",
]
struct_EcsOpaque._fields_ = [
    ("as_type", ecs_entity_t),
    ("serialize", ecs_meta_serialize_t),
    ("serialize_member", ecs_meta_serialize_member_t),
    ("serialize_element", ecs_meta_serialize_element_t),
    ("assign_bool", CFUNCTYPE(UNCHECKED(None), POINTER(None), c_bool)),
    ("assign_char", CFUNCTYPE(UNCHECKED(None), POINTER(None), c_char)),
    ("assign_int", CFUNCTYPE(UNCHECKED(None), POINTER(None), c_int64)),
    ("assign_uint", CFUNCTYPE(UNCHECKED(None), POINTER(None), uint64_t)),
    ("assign_float", CFUNCTYPE(UNCHECKED(None), POINTER(None), c_double)),
    ("assign_string", CFUNCTYPE(UNCHECKED(None), POINTER(None), String)),
    (
        "assign_entity",
        CFUNCTYPE(UNCHECKED(None), POINTER(None), POINTER(ecs_world_t), ecs_entity_t),
    ),
    (
        "assign_id",
        CFUNCTYPE(UNCHECKED(None), POINTER(None), POINTER(ecs_world_t), ecs_id_t),
    ),
    ("assign_null", CFUNCTYPE(UNCHECKED(None), POINTER(None))),
    ("clear", CFUNCTYPE(UNCHECKED(None), POINTER(None))),
    ("ensure_element", CFUNCTYPE(UNCHECKED(POINTER(c_ubyte)), POINTER(None), c_size_t)),
    ("ensure_member", CFUNCTYPE(UNCHECKED(POINTER(c_ubyte)), POINTER(None), String)),
    ("count", CFUNCTYPE(UNCHECKED(c_size_t), POINTER(None))),
    ("resize", CFUNCTYPE(UNCHECKED(None), POINTER(None), c_size_t)),
]

EcsOpaque = struct_EcsOpaque  # /Users/cnifi/git/pyflecs/flecs.h: 16446


# /Users/cnifi/git/pyflecs/flecs.h: 16461
class struct_ecs_unit_translation_t(Structure):
    pass


struct_ecs_unit_translation_t.__slots__ = [
    "factor",
    "power",
]
struct_ecs_unit_translation_t._fields_ = [
    ("factor", c_int32),
    ("power", c_int32),
]

ecs_unit_translation_t = (
    struct_ecs_unit_translation_t  # /Users/cnifi/git/pyflecs/flecs.h: 16461
)


# /Users/cnifi/git/pyflecs/flecs.h: 16470
class struct_EcsUnit(Structure):
    pass


struct_EcsUnit.__slots__ = [
    "symbol",
    "prefix",
    "base",
    "over",
    "translation",
]
struct_EcsUnit._fields_ = [
    ("symbol", String),
    ("prefix", ecs_entity_t),
    ("base", ecs_entity_t),
    ("over", ecs_entity_t),
    ("translation", ecs_unit_translation_t),
]

EcsUnit = struct_EcsUnit  # /Users/cnifi/git/pyflecs/flecs.h: 16470


# /Users/cnifi/git/pyflecs/flecs.h: 16476
class struct_EcsUnitPrefix(Structure):
    pass


struct_EcsUnitPrefix.__slots__ = [
    "symbol",
    "translation",
]
struct_EcsUnitPrefix._fields_ = [
    ("symbol", String),
    ("translation", ecs_unit_translation_t),
]

EcsUnitPrefix = struct_EcsUnitPrefix  # /Users/cnifi/git/pyflecs/flecs.h: 16476

enum_ecs_meta_type_op_kind_t = c_int  # /Users/cnifi/git/pyflecs/flecs.h: 16519

EcsOpArray = 0  # /Users/cnifi/git/pyflecs/flecs.h: 16519

EcsOpVector = EcsOpArray + 1  # /Users/cnifi/git/pyflecs/flecs.h: 16519

EcsOpOpaque = EcsOpVector + 1  # /Users/cnifi/git/pyflecs/flecs.h: 16519

EcsOpPush = EcsOpOpaque + 1  # /Users/cnifi/git/pyflecs/flecs.h: 16519

EcsOpPop = EcsOpPush + 1  # /Users/cnifi/git/pyflecs/flecs.h: 16519

EcsOpScope = EcsOpPop + 1  # /Users/cnifi/git/pyflecs/flecs.h: 16519

EcsOpEnum = EcsOpScope + 1  # /Users/cnifi/git/pyflecs/flecs.h: 16519

EcsOpBitmask = EcsOpEnum + 1  # /Users/cnifi/git/pyflecs/flecs.h: 16519

EcsOpPrimitive = EcsOpBitmask + 1  # /Users/cnifi/git/pyflecs/flecs.h: 16519

EcsOpBool = EcsOpPrimitive + 1  # /Users/cnifi/git/pyflecs/flecs.h: 16519

EcsOpChar = EcsOpBool + 1  # /Users/cnifi/git/pyflecs/flecs.h: 16519

EcsOpByte = EcsOpChar + 1  # /Users/cnifi/git/pyflecs/flecs.h: 16519

EcsOpU8 = EcsOpByte + 1  # /Users/cnifi/git/pyflecs/flecs.h: 16519

EcsOpU16 = EcsOpU8 + 1  # /Users/cnifi/git/pyflecs/flecs.h: 16519

EcsOpU32 = EcsOpU16 + 1  # /Users/cnifi/git/pyflecs/flecs.h: 16519

EcsOpU64 = EcsOpU32 + 1  # /Users/cnifi/git/pyflecs/flecs.h: 16519

EcsOpI8 = EcsOpU64 + 1  # /Users/cnifi/git/pyflecs/flecs.h: 16519

EcsOpI16 = EcsOpI8 + 1  # /Users/cnifi/git/pyflecs/flecs.h: 16519

EcsOpI32 = EcsOpI16 + 1  # /Users/cnifi/git/pyflecs/flecs.h: 16519

EcsOpI64 = EcsOpI32 + 1  # /Users/cnifi/git/pyflecs/flecs.h: 16519

EcsOpF32 = EcsOpI64 + 1  # /Users/cnifi/git/pyflecs/flecs.h: 16519

EcsOpF64 = EcsOpF32 + 1  # /Users/cnifi/git/pyflecs/flecs.h: 16519

EcsOpUPtr = EcsOpF64 + 1  # /Users/cnifi/git/pyflecs/flecs.h: 16519

EcsOpIPtr = EcsOpUPtr + 1  # /Users/cnifi/git/pyflecs/flecs.h: 16519

EcsOpString = EcsOpIPtr + 1  # /Users/cnifi/git/pyflecs/flecs.h: 16519

EcsOpEntity = EcsOpString + 1  # /Users/cnifi/git/pyflecs/flecs.h: 16519

EcsOpId = EcsOpEntity + 1  # /Users/cnifi/git/pyflecs/flecs.h: 16519

EcsMetaTypeOpKindLast = EcsOpId  # /Users/cnifi/git/pyflecs/flecs.h: 16519

ecs_meta_type_op_kind_t = (
    enum_ecs_meta_type_op_kind_t  # /Users/cnifi/git/pyflecs/flecs.h: 16519
)


# /Users/cnifi/git/pyflecs/flecs.h: 16532
class struct_ecs_meta_type_op_t(Structure):
    pass


struct_ecs_meta_type_op_t.__slots__ = [
    "kind",
    "offset",
    "count",
    "name",
    "op_count",
    "size",
    "type",
    "member_index",
    "members",
]
struct_ecs_meta_type_op_t._fields_ = [
    ("kind", ecs_meta_type_op_kind_t),
    ("offset", ecs_size_t),
    ("count", c_int32),
    ("name", String),
    ("op_count", c_int32),
    ("size", ecs_size_t),
    ("type", ecs_entity_t),
    ("member_index", c_int32),
    ("members", POINTER(ecs_hashmap_t)),
]

ecs_meta_type_op_t = (
    struct_ecs_meta_type_op_t  # /Users/cnifi/git/pyflecs/flecs.h: 16532
)


# /Users/cnifi/git/pyflecs/flecs.h: 16539
class struct_EcsTypeSerializer(Structure):
    pass


struct_EcsTypeSerializer.__slots__ = [
    "ops",
]
struct_EcsTypeSerializer._fields_ = [
    ("ops", ecs_vec_t),
]

EcsTypeSerializer = struct_EcsTypeSerializer  # /Users/cnifi/git/pyflecs/flecs.h: 16539


# /Users/cnifi/git/pyflecs/flecs.h: 16565
class struct_ecs_meta_scope_t(Structure):
    pass


struct_ecs_meta_scope_t.__slots__ = [
    "type",
    "ops",
    "op_count",
    "op_cur",
    "elem_cur",
    "prev_depth",
    "ptr",
    "comp",
    "opaque",
    "vector",
    "members",
    "is_collection",
    "is_inline_array",
    "is_empty_scope",
]
struct_ecs_meta_scope_t._fields_ = [
    ("type", ecs_entity_t),
    ("ops", POINTER(ecs_meta_type_op_t)),
    ("op_count", c_int32),
    ("op_cur", c_int32),
    ("elem_cur", c_int32),
    ("prev_depth", c_int32),
    ("ptr", POINTER(None)),
    ("comp", POINTER(EcsComponent)),
    ("opaque", POINTER(EcsOpaque)),
    ("vector", POINTER(ecs_vec_t)),
    ("members", POINTER(ecs_hashmap_t)),
    ("is_collection", c_bool),
    ("is_inline_array", c_bool),
    ("is_empty_scope", c_bool),
]

ecs_meta_scope_t = struct_ecs_meta_scope_t  # /Users/cnifi/git/pyflecs/flecs.h: 16565


# /Users/cnifi/git/pyflecs/flecs.h: 16578
class struct_ecs_meta_cursor_t(Structure):
    pass


struct_ecs_meta_cursor_t.__slots__ = [
    "world",
    "scope",
    "depth",
    "valid",
    "is_primitive_scope",
    "lookup_action",
    "lookup_ctx",
]
struct_ecs_meta_cursor_t._fields_ = [
    ("world", POINTER(ecs_world_t)),
    ("scope", ecs_meta_scope_t * int(32)),
    ("depth", c_int32),
    ("valid", c_bool),
    ("is_primitive_scope", c_bool),
    (
        "lookup_action",
        CFUNCTYPE(UNCHECKED(ecs_entity_t), POINTER(ecs_world_t), String, POINTER(None)),
    ),
    ("lookup_ctx", POINTER(None)),
]

ecs_meta_cursor_t = struct_ecs_meta_cursor_t  # /Users/cnifi/git/pyflecs/flecs.h: 16578

# /Users/cnifi/git/pyflecs/flecs.h: 16595
if _libs["libflecs.dylib"].has("ecs_meta_cursor", "cdecl"):
    ecs_meta_cursor = _libs["libflecs.dylib"].get("ecs_meta_cursor", "cdecl")
    ecs_meta_cursor.argtypes = [POINTER(ecs_world_t), ecs_entity_t, POINTER(None)]
    ecs_meta_cursor.restype = ecs_meta_cursor_t

# /Users/cnifi/git/pyflecs/flecs.h: 16606
if _libs["libflecs.dylib"].has("ecs_meta_get_ptr", "cdecl"):
    ecs_meta_get_ptr = _libs["libflecs.dylib"].get("ecs_meta_get_ptr", "cdecl")
    ecs_meta_get_ptr.argtypes = [POINTER(ecs_meta_cursor_t)]
    ecs_meta_get_ptr.restype = POINTER(c_ubyte)
    ecs_meta_get_ptr.errcheck = lambda v, *a: cast(v, c_void_p)

# /Users/cnifi/git/pyflecs/flecs.h: 16615
if _libs["libflecs.dylib"].has("ecs_meta_next", "cdecl"):
    ecs_meta_next = _libs["libflecs.dylib"].get("ecs_meta_next", "cdecl")
    ecs_meta_next.argtypes = [POINTER(ecs_meta_cursor_t)]
    ecs_meta_next.restype = c_int

# /Users/cnifi/git/pyflecs/flecs.h: 16624
if _libs["libflecs.dylib"].has("ecs_meta_elem", "cdecl"):
    ecs_meta_elem = _libs["libflecs.dylib"].get("ecs_meta_elem", "cdecl")
    ecs_meta_elem.argtypes = [POINTER(ecs_meta_cursor_t), c_int32]
    ecs_meta_elem.restype = c_int

# /Users/cnifi/git/pyflecs/flecs.h: 16635
if _libs["libflecs.dylib"].has("ecs_meta_member", "cdecl"):
    ecs_meta_member = _libs["libflecs.dylib"].get("ecs_meta_member", "cdecl")
    ecs_meta_member.argtypes = [POINTER(ecs_meta_cursor_t), String]
    ecs_meta_member.restype = c_int

# /Users/cnifi/git/pyflecs/flecs.h: 16647
if _libs["libflecs.dylib"].has("ecs_meta_dotmember", "cdecl"):
    ecs_meta_dotmember = _libs["libflecs.dylib"].get("ecs_meta_dotmember", "cdecl")
    ecs_meta_dotmember.argtypes = [POINTER(ecs_meta_cursor_t), String]
    ecs_meta_dotmember.restype = c_int

# /Users/cnifi/git/pyflecs/flecs.h: 16657
if _libs["libflecs.dylib"].has("ecs_meta_push", "cdecl"):
    ecs_meta_push = _libs["libflecs.dylib"].get("ecs_meta_push", "cdecl")
    ecs_meta_push.argtypes = [POINTER(ecs_meta_cursor_t)]
    ecs_meta_push.restype = c_int

# /Users/cnifi/git/pyflecs/flecs.h: 16666
if _libs["libflecs.dylib"].has("ecs_meta_pop", "cdecl"):
    ecs_meta_pop = _libs["libflecs.dylib"].get("ecs_meta_pop", "cdecl")
    ecs_meta_pop.argtypes = [POINTER(ecs_meta_cursor_t)]
    ecs_meta_pop.restype = c_int

# /Users/cnifi/git/pyflecs/flecs.h: 16675
if _libs["libflecs.dylib"].has("ecs_meta_is_collection", "cdecl"):
    ecs_meta_is_collection = _libs["libflecs.dylib"].get(
        "ecs_meta_is_collection", "cdecl"
    )
    ecs_meta_is_collection.argtypes = [POINTER(ecs_meta_cursor_t)]
    ecs_meta_is_collection.restype = c_bool

# /Users/cnifi/git/pyflecs/flecs.h: 16684
if _libs["libflecs.dylib"].has("ecs_meta_get_type", "cdecl"):
    ecs_meta_get_type = _libs["libflecs.dylib"].get("ecs_meta_get_type", "cdecl")
    ecs_meta_get_type.argtypes = [POINTER(ecs_meta_cursor_t)]
    ecs_meta_get_type.restype = ecs_entity_t

# /Users/cnifi/git/pyflecs/flecs.h: 16693
if _libs["libflecs.dylib"].has("ecs_meta_get_unit", "cdecl"):
    ecs_meta_get_unit = _libs["libflecs.dylib"].get("ecs_meta_get_unit", "cdecl")
    ecs_meta_get_unit.argtypes = [POINTER(ecs_meta_cursor_t)]
    ecs_meta_get_unit.restype = ecs_entity_t

# /Users/cnifi/git/pyflecs/flecs.h: 16702
if _libs["libflecs.dylib"].has("ecs_meta_get_member", "cdecl"):
    ecs_meta_get_member = _libs["libflecs.dylib"].get("ecs_meta_get_member", "cdecl")
    ecs_meta_get_member.argtypes = [POINTER(ecs_meta_cursor_t)]
    ecs_meta_get_member.restype = c_char_p

# /Users/cnifi/git/pyflecs/flecs.h: 16711
if _libs["libflecs.dylib"].has("ecs_meta_get_member_id", "cdecl"):
    ecs_meta_get_member_id = _libs["libflecs.dylib"].get(
        "ecs_meta_get_member_id", "cdecl"
    )
    ecs_meta_get_member_id.argtypes = [POINTER(ecs_meta_cursor_t)]
    ecs_meta_get_member_id.restype = ecs_entity_t

# /Users/cnifi/git/pyflecs/flecs.h: 16725
if _libs["libflecs.dylib"].has("ecs_meta_set_bool", "cdecl"):
    ecs_meta_set_bool = _libs["libflecs.dylib"].get("ecs_meta_set_bool", "cdecl")
    ecs_meta_set_bool.argtypes = [POINTER(ecs_meta_cursor_t), c_bool]
    ecs_meta_set_bool.restype = c_int

# /Users/cnifi/git/pyflecs/flecs.h: 16736
if _libs["libflecs.dylib"].has("ecs_meta_set_char", "cdecl"):
    ecs_meta_set_char = _libs["libflecs.dylib"].get("ecs_meta_set_char", "cdecl")
    ecs_meta_set_char.argtypes = [POINTER(ecs_meta_cursor_t), c_char]
    ecs_meta_set_char.restype = c_int

# /Users/cnifi/git/pyflecs/flecs.h: 16747
if _libs["libflecs.dylib"].has("ecs_meta_set_int", "cdecl"):
    ecs_meta_set_int = _libs["libflecs.dylib"].get("ecs_meta_set_int", "cdecl")
    ecs_meta_set_int.argtypes = [POINTER(ecs_meta_cursor_t), c_int64]
    ecs_meta_set_int.restype = c_int

# /Users/cnifi/git/pyflecs/flecs.h: 16758
if _libs["libflecs.dylib"].has("ecs_meta_set_uint", "cdecl"):
    ecs_meta_set_uint = _libs["libflecs.dylib"].get("ecs_meta_set_uint", "cdecl")
    ecs_meta_set_uint.argtypes = [POINTER(ecs_meta_cursor_t), uint64_t]
    ecs_meta_set_uint.restype = c_int

# /Users/cnifi/git/pyflecs/flecs.h: 16769
if _libs["libflecs.dylib"].has("ecs_meta_set_float", "cdecl"):
    ecs_meta_set_float = _libs["libflecs.dylib"].get("ecs_meta_set_float", "cdecl")
    ecs_meta_set_float.argtypes = [POINTER(ecs_meta_cursor_t), c_double]
    ecs_meta_set_float.restype = c_int

# /Users/cnifi/git/pyflecs/flecs.h: 16780
if _libs["libflecs.dylib"].has("ecs_meta_set_string", "cdecl"):
    ecs_meta_set_string = _libs["libflecs.dylib"].get("ecs_meta_set_string", "cdecl")
    ecs_meta_set_string.argtypes = [POINTER(ecs_meta_cursor_t), String]
    ecs_meta_set_string.restype = c_int

# /Users/cnifi/git/pyflecs/flecs.h: 16791
if _libs["libflecs.dylib"].has("ecs_meta_set_string_literal", "cdecl"):
    ecs_meta_set_string_literal = _libs["libflecs.dylib"].get(
        "ecs_meta_set_string_literal", "cdecl"
    )
    ecs_meta_set_string_literal.argtypes = [POINTER(ecs_meta_cursor_t), String]
    ecs_meta_set_string_literal.restype = c_int

# /Users/cnifi/git/pyflecs/flecs.h: 16802
if _libs["libflecs.dylib"].has("ecs_meta_set_entity", "cdecl"):
    ecs_meta_set_entity = _libs["libflecs.dylib"].get("ecs_meta_set_entity", "cdecl")
    ecs_meta_set_entity.argtypes = [POINTER(ecs_meta_cursor_t), ecs_entity_t]
    ecs_meta_set_entity.restype = c_int

# /Users/cnifi/git/pyflecs/flecs.h: 16813
if _libs["libflecs.dylib"].has("ecs_meta_set_id", "cdecl"):
    ecs_meta_set_id = _libs["libflecs.dylib"].get("ecs_meta_set_id", "cdecl")
    ecs_meta_set_id.argtypes = [POINTER(ecs_meta_cursor_t), ecs_id_t]
    ecs_meta_set_id.restype = c_int

# /Users/cnifi/git/pyflecs/flecs.h: 16823
if _libs["libflecs.dylib"].has("ecs_meta_set_null", "cdecl"):
    ecs_meta_set_null = _libs["libflecs.dylib"].get("ecs_meta_set_null", "cdecl")
    ecs_meta_set_null.argtypes = [POINTER(ecs_meta_cursor_t)]
    ecs_meta_set_null.restype = c_int

# /Users/cnifi/git/pyflecs/flecs.h: 16833
if _libs["libflecs.dylib"].has("ecs_meta_set_value", "cdecl"):
    ecs_meta_set_value = _libs["libflecs.dylib"].get("ecs_meta_set_value", "cdecl")
    ecs_meta_set_value.argtypes = [POINTER(ecs_meta_cursor_t), POINTER(ecs_value_t)]
    ecs_meta_set_value.restype = c_int

# /Users/cnifi/git/pyflecs/flecs.h: 16845
if _libs["libflecs.dylib"].has("ecs_meta_get_bool", "cdecl"):
    ecs_meta_get_bool = _libs["libflecs.dylib"].get("ecs_meta_get_bool", "cdecl")
    ecs_meta_get_bool.argtypes = [POINTER(ecs_meta_cursor_t)]
    ecs_meta_get_bool.restype = c_bool

# /Users/cnifi/git/pyflecs/flecs.h: 16854
if _libs["libflecs.dylib"].has("ecs_meta_get_char", "cdecl"):
    ecs_meta_get_char = _libs["libflecs.dylib"].get("ecs_meta_get_char", "cdecl")
    ecs_meta_get_char.argtypes = [POINTER(ecs_meta_cursor_t)]
    ecs_meta_get_char.restype = c_char

# /Users/cnifi/git/pyflecs/flecs.h: 16863
if _libs["libflecs.dylib"].has("ecs_meta_get_int", "cdecl"):
    ecs_meta_get_int = _libs["libflecs.dylib"].get("ecs_meta_get_int", "cdecl")
    ecs_meta_get_int.argtypes = [POINTER(ecs_meta_cursor_t)]
    ecs_meta_get_int.restype = c_int64

# /Users/cnifi/git/pyflecs/flecs.h: 16872
if _libs["libflecs.dylib"].has("ecs_meta_get_uint", "cdecl"):
    ecs_meta_get_uint = _libs["libflecs.dylib"].get("ecs_meta_get_uint", "cdecl")
    ecs_meta_get_uint.argtypes = [POINTER(ecs_meta_cursor_t)]
    ecs_meta_get_uint.restype = uint64_t

# /Users/cnifi/git/pyflecs/flecs.h: 16881
if _libs["libflecs.dylib"].has("ecs_meta_get_float", "cdecl"):
    ecs_meta_get_float = _libs["libflecs.dylib"].get("ecs_meta_get_float", "cdecl")
    ecs_meta_get_float.argtypes = [POINTER(ecs_meta_cursor_t)]
    ecs_meta_get_float.restype = c_double

# /Users/cnifi/git/pyflecs/flecs.h: 16892
if _libs["libflecs.dylib"].has("ecs_meta_get_string", "cdecl"):
    ecs_meta_get_string = _libs["libflecs.dylib"].get("ecs_meta_get_string", "cdecl")
    ecs_meta_get_string.argtypes = [POINTER(ecs_meta_cursor_t)]
    ecs_meta_get_string.restype = c_char_p

# /Users/cnifi/git/pyflecs/flecs.h: 16902
if _libs["libflecs.dylib"].has("ecs_meta_get_entity", "cdecl"):
    ecs_meta_get_entity = _libs["libflecs.dylib"].get("ecs_meta_get_entity", "cdecl")
    ecs_meta_get_entity.argtypes = [POINTER(ecs_meta_cursor_t)]
    ecs_meta_get_entity.restype = ecs_entity_t

# /Users/cnifi/git/pyflecs/flecs.h: 16911
if _libs["libflecs.dylib"].has("ecs_meta_get_id", "cdecl"):
    ecs_meta_get_id = _libs["libflecs.dylib"].get("ecs_meta_get_id", "cdecl")
    ecs_meta_get_id.argtypes = [POINTER(ecs_meta_cursor_t)]
    ecs_meta_get_id.restype = ecs_id_t

# /Users/cnifi/git/pyflecs/flecs.h: 16921
if _libs["libflecs.dylib"].has("ecs_meta_ptr_to_float", "cdecl"):
    ecs_meta_ptr_to_float = _libs["libflecs.dylib"].get(
        "ecs_meta_ptr_to_float", "cdecl"
    )
    ecs_meta_ptr_to_float.argtypes = [ecs_primitive_kind_t, POINTER(None)]
    ecs_meta_ptr_to_float.restype = c_double


# /Users/cnifi/git/pyflecs/flecs.h: 16931
class struct_ecs_primitive_desc_t(Structure):
    pass


struct_ecs_primitive_desc_t.__slots__ = [
    "entity",
    "kind",
]
struct_ecs_primitive_desc_t._fields_ = [
    ("entity", ecs_entity_t),
    ("kind", ecs_primitive_kind_t),
]

ecs_primitive_desc_t = (
    struct_ecs_primitive_desc_t  # /Users/cnifi/git/pyflecs/flecs.h: 16931
)

# /Users/cnifi/git/pyflecs/flecs.h: 16940
if _libs["libflecs.dylib"].has("ecs_primitive_init", "cdecl"):
    ecs_primitive_init = _libs["libflecs.dylib"].get("ecs_primitive_init", "cdecl")
    ecs_primitive_init.argtypes = [POINTER(ecs_world_t), POINTER(ecs_primitive_desc_t)]
    ecs_primitive_init.restype = ecs_entity_t


# /Users/cnifi/git/pyflecs/flecs.h: 16950
class struct_ecs_enum_desc_t(Structure):
    pass


struct_ecs_enum_desc_t.__slots__ = [
    "entity",
    "constants",
    "underlying_type",
]
struct_ecs_enum_desc_t._fields_ = [
    ("entity", ecs_entity_t),
    ("constants", ecs_enum_constant_t * int(32)),
    ("underlying_type", ecs_entity_t),
]

ecs_enum_desc_t = struct_ecs_enum_desc_t  # /Users/cnifi/git/pyflecs/flecs.h: 16950

# /Users/cnifi/git/pyflecs/flecs.h: 16959
if _libs["libflecs.dylib"].has("ecs_enum_init", "cdecl"):
    ecs_enum_init = _libs["libflecs.dylib"].get("ecs_enum_init", "cdecl")
    ecs_enum_init.argtypes = [POINTER(ecs_world_t), POINTER(ecs_enum_desc_t)]
    ecs_enum_init.restype = ecs_entity_t


# /Users/cnifi/git/pyflecs/flecs.h: 16968
class struct_ecs_bitmask_desc_t(Structure):
    pass


struct_ecs_bitmask_desc_t.__slots__ = [
    "entity",
    "constants",
]
struct_ecs_bitmask_desc_t._fields_ = [
    ("entity", ecs_entity_t),
    ("constants", ecs_bitmask_constant_t * int(32)),
]

ecs_bitmask_desc_t = (
    struct_ecs_bitmask_desc_t  # /Users/cnifi/git/pyflecs/flecs.h: 16968
)

# /Users/cnifi/git/pyflecs/flecs.h: 16977
if _libs["libflecs.dylib"].has("ecs_bitmask_init", "cdecl"):
    ecs_bitmask_init = _libs["libflecs.dylib"].get("ecs_bitmask_init", "cdecl")
    ecs_bitmask_init.argtypes = [POINTER(ecs_world_t), POINTER(ecs_bitmask_desc_t)]
    ecs_bitmask_init.restype = ecs_entity_t


# /Users/cnifi/git/pyflecs/flecs.h: 16987
class struct_ecs_array_desc_t(Structure):
    pass


struct_ecs_array_desc_t.__slots__ = [
    "entity",
    "type",
    "count",
]
struct_ecs_array_desc_t._fields_ = [
    ("entity", ecs_entity_t),
    ("type", ecs_entity_t),
    ("count", c_int32),
]

ecs_array_desc_t = struct_ecs_array_desc_t  # /Users/cnifi/git/pyflecs/flecs.h: 16987

# /Users/cnifi/git/pyflecs/flecs.h: 16996
if _libs["libflecs.dylib"].has("ecs_array_init", "cdecl"):
    ecs_array_init = _libs["libflecs.dylib"].get("ecs_array_init", "cdecl")
    ecs_array_init.argtypes = [POINTER(ecs_world_t), POINTER(ecs_array_desc_t)]
    ecs_array_init.restype = ecs_entity_t


# /Users/cnifi/git/pyflecs/flecs.h: 17005
class struct_ecs_vector_desc_t(Structure):
    pass


struct_ecs_vector_desc_t.__slots__ = [
    "entity",
    "type",
]
struct_ecs_vector_desc_t._fields_ = [
    ("entity", ecs_entity_t),
    ("type", ecs_entity_t),
]

ecs_vector_desc_t = struct_ecs_vector_desc_t  # /Users/cnifi/git/pyflecs/flecs.h: 17005

# /Users/cnifi/git/pyflecs/flecs.h: 17014
if _libs["libflecs.dylib"].has("ecs_vector_init", "cdecl"):
    ecs_vector_init = _libs["libflecs.dylib"].get("ecs_vector_init", "cdecl")
    ecs_vector_init.argtypes = [POINTER(ecs_world_t), POINTER(ecs_vector_desc_t)]
    ecs_vector_init.restype = ecs_entity_t


# /Users/cnifi/git/pyflecs/flecs.h: 17023
class struct_ecs_struct_desc_t(Structure):
    pass


struct_ecs_struct_desc_t.__slots__ = [
    "entity",
    "members",
]
struct_ecs_struct_desc_t._fields_ = [
    ("entity", ecs_entity_t),
    ("members", ecs_member_t * int(32)),
]

ecs_struct_desc_t = struct_ecs_struct_desc_t  # /Users/cnifi/git/pyflecs/flecs.h: 17023

# /Users/cnifi/git/pyflecs/flecs.h: 17032
if _libs["libflecs.dylib"].has("ecs_struct_init", "cdecl"):
    ecs_struct_init = _libs["libflecs.dylib"].get("ecs_struct_init", "cdecl")
    ecs_struct_init.argtypes = [POINTER(ecs_world_t), POINTER(ecs_struct_desc_t)]
    ecs_struct_init.restype = ecs_entity_t


# /Users/cnifi/git/pyflecs/flecs.h: 17041
class struct_ecs_opaque_desc_t(Structure):
    pass


struct_ecs_opaque_desc_t.__slots__ = [
    "entity",
    "type",
]
struct_ecs_opaque_desc_t._fields_ = [
    ("entity", ecs_entity_t),
    ("type", EcsOpaque),
]

ecs_opaque_desc_t = struct_ecs_opaque_desc_t  # /Users/cnifi/git/pyflecs/flecs.h: 17041

# /Users/cnifi/git/pyflecs/flecs.h: 17066
if _libs["libflecs.dylib"].has("ecs_opaque_init", "cdecl"):
    ecs_opaque_init = _libs["libflecs.dylib"].get("ecs_opaque_init", "cdecl")
    ecs_opaque_init.argtypes = [POINTER(ecs_world_t), POINTER(ecs_opaque_desc_t)]
    ecs_opaque_init.restype = ecs_entity_t


# /Users/cnifi/git/pyflecs/flecs.h: 17098
class struct_ecs_unit_desc_t(Structure):
    pass


struct_ecs_unit_desc_t.__slots__ = [
    "entity",
    "symbol",
    "quantity",
    "base",
    "over",
    "translation",
    "prefix",
]
struct_ecs_unit_desc_t._fields_ = [
    ("entity", ecs_entity_t),
    ("symbol", String),
    ("quantity", ecs_entity_t),
    ("base", ecs_entity_t),
    ("over", ecs_entity_t),
    ("translation", ecs_unit_translation_t),
    ("prefix", ecs_entity_t),
]

ecs_unit_desc_t = struct_ecs_unit_desc_t  # /Users/cnifi/git/pyflecs/flecs.h: 17098

# /Users/cnifi/git/pyflecs/flecs.h: 17107
if _libs["libflecs.dylib"].has("ecs_unit_init", "cdecl"):
    ecs_unit_init = _libs["libflecs.dylib"].get("ecs_unit_init", "cdecl")
    ecs_unit_init.argtypes = [POINTER(ecs_world_t), POINTER(ecs_unit_desc_t)]
    ecs_unit_init.restype = ecs_entity_t


# /Users/cnifi/git/pyflecs/flecs.h: 17122
class struct_ecs_unit_prefix_desc_t(Structure):
    pass


struct_ecs_unit_prefix_desc_t.__slots__ = [
    "entity",
    "symbol",
    "translation",
]
struct_ecs_unit_prefix_desc_t._fields_ = [
    ("entity", ecs_entity_t),
    ("symbol", String),
    ("translation", ecs_unit_translation_t),
]

ecs_unit_prefix_desc_t = (
    struct_ecs_unit_prefix_desc_t  # /Users/cnifi/git/pyflecs/flecs.h: 17122
)

# /Users/cnifi/git/pyflecs/flecs.h: 17131
if _libs["libflecs.dylib"].has("ecs_unit_prefix_init", "cdecl"):
    ecs_unit_prefix_init = _libs["libflecs.dylib"].get("ecs_unit_prefix_init", "cdecl")
    ecs_unit_prefix_init.argtypes = [
        POINTER(ecs_world_t),
        POINTER(ecs_unit_prefix_desc_t),
    ]
    ecs_unit_prefix_init.restype = ecs_entity_t

# /Users/cnifi/git/pyflecs/flecs.h: 17143
if _libs["libflecs.dylib"].has("ecs_quantity_init", "cdecl"):
    ecs_quantity_init = _libs["libflecs.dylib"].get("ecs_quantity_init", "cdecl")
    ecs_quantity_init.argtypes = [POINTER(ecs_world_t), POINTER(ecs_entity_desc_t)]
    ecs_quantity_init.restype = ecs_entity_t

# /Users/cnifi/git/pyflecs/flecs.h: 17199
if _libs["libflecs.dylib"].has("FlecsMetaImport", "cdecl"):
    FlecsMetaImport = _libs["libflecs.dylib"].get("FlecsMetaImport", "cdecl")
    FlecsMetaImport.argtypes = [POINTER(ecs_world_t)]
    FlecsMetaImport.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 17270
if _libs["libflecs.dylib"].has("ecs_meta_from_desc", "cdecl"):
    ecs_meta_from_desc = _libs["libflecs.dylib"].get("ecs_meta_from_desc", "cdecl")
    ecs_meta_from_desc.argtypes = [
        POINTER(ecs_world_t),
        ecs_entity_t,
        ecs_type_kind_t,
        String,
    ]
    ecs_meta_from_desc.restype = c_int

# /Users/cnifi/git/pyflecs/flecs.h: 17394
if _libs["libflecs.dylib"].has("ecs_set_os_api_impl", "cdecl"):
    ecs_set_os_api_impl = _libs["libflecs.dylib"].get("ecs_set_os_api_impl", "cdecl")
    ecs_set_os_api_impl.argtypes = []
    ecs_set_os_api_impl.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 17458
if _libs["libflecs.dylib"].has("ecs_import", "cdecl"):
    ecs_import = _libs["libflecs.dylib"].get("ecs_import", "cdecl")
    ecs_import.argtypes = [POINTER(ecs_world_t), ecs_module_action_t, String]
    ecs_import.restype = ecs_entity_t

# /Users/cnifi/git/pyflecs/flecs.h: 17472
if _libs["libflecs.dylib"].has("ecs_import_c", "cdecl"):
    ecs_import_c = _libs["libflecs.dylib"].get("ecs_import_c", "cdecl")
    ecs_import_c.argtypes = [POINTER(ecs_world_t), ecs_module_action_t, String]
    ecs_import_c.restype = ecs_entity_t

# /Users/cnifi/git/pyflecs/flecs.h: 17496
if _libs["libflecs.dylib"].has("ecs_import_from_library", "cdecl"):
    ecs_import_from_library = _libs["libflecs.dylib"].get(
        "ecs_import_from_library", "cdecl"
    )
    ecs_import_from_library.argtypes = [POINTER(ecs_world_t), String, String]
    ecs_import_from_library.restype = ecs_entity_t

# /Users/cnifi/git/pyflecs/flecs.h: 17503
if _libs["libflecs.dylib"].has("ecs_module_init", "cdecl"):
    ecs_module_init = _libs["libflecs.dylib"].get("ecs_module_init", "cdecl")
    ecs_module_init.argtypes = [
        POINTER(ecs_world_t),
        String,
        POINTER(ecs_component_desc_t),
    ]
    ecs_module_init.restype = ecs_entity_t

# /Users/cnifi/git/pyflecs/flecs.h: 17591
if _libs["libflecs.dylib"].has("ecs_cpp_get_type_name", "cdecl"):
    ecs_cpp_get_type_name = _libs["libflecs.dylib"].get(
        "ecs_cpp_get_type_name", "cdecl"
    )
    ecs_cpp_get_type_name.argtypes = [String, String, c_size_t, c_size_t]
    if sizeof(c_int) == sizeof(c_void_p):
        ecs_cpp_get_type_name.restype = ReturnString
    else:
        ecs_cpp_get_type_name.restype = String
        ecs_cpp_get_type_name.errcheck = ReturnString

# /Users/cnifi/git/pyflecs/flecs.h: 17598
if _libs["libflecs.dylib"].has("ecs_cpp_get_symbol_name", "cdecl"):
    ecs_cpp_get_symbol_name = _libs["libflecs.dylib"].get(
        "ecs_cpp_get_symbol_name", "cdecl"
    )
    ecs_cpp_get_symbol_name.argtypes = [String, String, c_size_t]
    if sizeof(c_int) == sizeof(c_void_p):
        ecs_cpp_get_symbol_name.restype = ReturnString
    else:
        ecs_cpp_get_symbol_name.restype = String
        ecs_cpp_get_symbol_name.errcheck = ReturnString

# /Users/cnifi/git/pyflecs/flecs.h: 17604
if _libs["libflecs.dylib"].has("ecs_cpp_get_constant_name", "cdecl"):
    ecs_cpp_get_constant_name = _libs["libflecs.dylib"].get(
        "ecs_cpp_get_constant_name", "cdecl"
    )
    ecs_cpp_get_constant_name.argtypes = [String, String, c_size_t, c_size_t]
    if sizeof(c_int) == sizeof(c_void_p):
        ecs_cpp_get_constant_name.restype = ReturnString
    else:
        ecs_cpp_get_constant_name.restype = String
        ecs_cpp_get_constant_name.errcheck = ReturnString

# /Users/cnifi/git/pyflecs/flecs.h: 17611
if _libs["libflecs.dylib"].has("ecs_cpp_trim_module", "cdecl"):
    ecs_cpp_trim_module = _libs["libflecs.dylib"].get("ecs_cpp_trim_module", "cdecl")
    ecs_cpp_trim_module.argtypes = [POINTER(ecs_world_t), String]
    ecs_cpp_trim_module.restype = c_char_p

# /Users/cnifi/git/pyflecs/flecs.h: 17616
if _libs["libflecs.dylib"].has("ecs_cpp_component_register", "cdecl"):
    ecs_cpp_component_register = _libs["libflecs.dylib"].get(
        "ecs_cpp_component_register", "cdecl"
    )
    ecs_cpp_component_register.argtypes = [
        POINTER(ecs_world_t),
        ecs_entity_t,
        c_int32,
        String,
        String,
        String,
        c_size_t,
        c_size_t,
        c_bool,
        c_bool,
        POINTER(c_bool),
        POINTER(c_bool),
    ]
    ecs_cpp_component_register.restype = ecs_entity_t

# /Users/cnifi/git/pyflecs/flecs.h: 17631
if _libs["libflecs.dylib"].has("ecs_cpp_enum_init", "cdecl"):
    ecs_cpp_enum_init = _libs["libflecs.dylib"].get("ecs_cpp_enum_init", "cdecl")
    ecs_cpp_enum_init.argtypes = [POINTER(ecs_world_t), ecs_entity_t, ecs_entity_t]
    ecs_cpp_enum_init.restype = None

# /Users/cnifi/git/pyflecs/flecs.h: 17637
if _libs["libflecs.dylib"].has("ecs_cpp_enum_constant_register", "cdecl"):
    ecs_cpp_enum_constant_register = _libs["libflecs.dylib"].get(
        "ecs_cpp_enum_constant_register", "cdecl"
    )
    ecs_cpp_enum_constant_register.argtypes = [
        POINTER(ecs_world_t),
        ecs_entity_t,
        ecs_entity_t,
        String,
        POINTER(None),
        ecs_entity_t,
        c_size_t,
    ]
    ecs_cpp_enum_constant_register.restype = ecs_entity_t


# /Users/cnifi/git/pyflecs/flecs.h: 17649
class struct_ecs_cpp_get_mut_t(Structure):
    pass


struct_ecs_cpp_get_mut_t.__slots__ = [
    "ptr",
    "call_modified",
]
struct_ecs_cpp_get_mut_t._fields_ = [
    ("ptr", POINTER(None)),
    ("call_modified", c_bool),
]

ecs_cpp_get_mut_t = struct_ecs_cpp_get_mut_t  # /Users/cnifi/git/pyflecs/flecs.h: 17649

# /Users/cnifi/git/pyflecs/flecs.h: 17652
if _libs["libflecs.dylib"].has("ecs_cpp_set", "cdecl"):
    ecs_cpp_set = _libs["libflecs.dylib"].get("ecs_cpp_set", "cdecl")
    ecs_cpp_set.argtypes = [
        POINTER(ecs_world_t),
        ecs_entity_t,
        ecs_id_t,
        POINTER(None),
        c_size_t,
    ]
    ecs_cpp_set.restype = ecs_cpp_get_mut_t

# /Users/cnifi/git/pyflecs/flecs.h: 17660
if _libs["libflecs.dylib"].has("ecs_cpp_assign", "cdecl"):
    ecs_cpp_assign = _libs["libflecs.dylib"].get("ecs_cpp_assign", "cdecl")
    ecs_cpp_assign.argtypes = [
        POINTER(ecs_world_t),
        ecs_entity_t,
        ecs_id_t,
        POINTER(None),
        c_size_t,
    ]
    ecs_cpp_assign.restype = ecs_cpp_get_mut_t

# /Users/cnifi/git/pyflecs/flecs.h: 17669
if _libs["libflecs.dylib"].has("ecs_cpp_last_member", "cdecl"):
    ecs_cpp_last_member = _libs["libflecs.dylib"].get("ecs_cpp_last_member", "cdecl")
    ecs_cpp_last_member.argtypes = [POINTER(ecs_world_t), ecs_entity_t]
    ecs_cpp_last_member.restype = POINTER(ecs_member_t)

# <built-in>
try:
    __clang_major__ = 15
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 36
try:
    FLECS_VERSION_MAJOR = 4
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 37
try:
    FLECS_VERSION_MINOR = 1
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 38
try:
    FLECS_VERSION_PATCH = 0
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 41
try:
    FLECS_VERSION = FLECS_VERSION_IMPL(
        FLECS_VERSION_MAJOR, FLECS_VERSION_MINOR, FLECS_VERSION_PATCH
    )
except:
    pass

ecs_float_t = c_float  # /Users/cnifi/git/pyflecs/flecs.h: 54

# /Users/cnifi/git/pyflecs/flecs.h: 265
try:
    FLECS_HI_COMPONENT_ID = 256
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 275
try:
    FLECS_HI_ID_RECORD_ID = 1024
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 284
try:
    FLECS_SPARSE_PAGE_BITS = 6
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 290
try:
    FLECS_ENTITY_PAGE_BITS = 10
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 302
try:
    FLECS_ID_DESC_MAX = 32
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 308
try:
    FLECS_EVENT_DESC_MAX = 8
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 313
try:
    FLECS_VARIABLE_COUNT_MAX = 64
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 318
try:
    FLECS_TERM_COUNT_MAX = 32
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 324
try:
    FLECS_TERM_ARG_COUNT_MAX = 16
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 330
try:
    FLECS_QUERY_VARIABLE_COUNT_MAX = 64
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 336
try:
    FLECS_QUERY_SCOPE_NESTING_MAX = 8
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 344
try:
    FLECS_DAG_DEPTH_MAX = 128
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 378
try:
    EcsWorldQuitWorkers = 1 << 0
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 379
try:
    EcsWorldReadonly = 1 << 1
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 380
try:
    EcsWorldInit = 1 << 2
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 381
try:
    EcsWorldQuit = 1 << 3
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 382
try:
    EcsWorldFini = 1 << 4
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 383
try:
    EcsWorldMeasureFrameTime = 1 << 5
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 384
try:
    EcsWorldMeasureSystemTime = 1 << 6
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 385
try:
    EcsWorldMultiThreaded = 1 << 7
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 386
try:
    EcsWorldFrameInProgress = 1 << 8
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 392
try:
    EcsOsApiHighResolutionTimer = 1 << 0
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 393
try:
    EcsOsApiLogWithColors = 1 << 1
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 394
try:
    EcsOsApiLogWithTimeStamp = 1 << 2
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 395
try:
    EcsOsApiLogWithTimeDelta = 1 << 3
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 402
try:
    EcsEntityIsId = 1 << 31
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 403
try:
    EcsEntityIsTarget = 1 << 30
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 404
try:
    EcsEntityIsTraversable = 1 << 29
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 405
try:
    EcsEntityHasDontFragment = 1 << 28
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 412
try:
    EcsIdOnDeleteRemove = 1 << 0
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 413
try:
    EcsIdOnDeleteDelete = 1 << 1
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 414
try:
    EcsIdOnDeletePanic = 1 << 2
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 415
try:
    EcsIdOnDeleteMask = (EcsIdOnDeletePanic | EcsIdOnDeleteRemove) | EcsIdOnDeleteDelete
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 418
try:
    EcsIdOnDeleteTargetRemove = 1 << 3
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 419
try:
    EcsIdOnDeleteTargetDelete = 1 << 4
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 420
try:
    EcsIdOnDeleteTargetPanic = 1 << 5
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 421
try:
    EcsIdOnDeleteTargetMask = (
        EcsIdOnDeleteTargetPanic | EcsIdOnDeleteTargetRemove
    ) | EcsIdOnDeleteTargetDelete
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 425
try:
    EcsIdOnInstantiateOverride = 1 << 6
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 426
try:
    EcsIdOnInstantiateInherit = 1 << 7
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 427
try:
    EcsIdOnInstantiateDontInherit = 1 << 8
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 428
try:
    EcsIdOnInstantiateMask = (
        EcsIdOnInstantiateOverride | EcsIdOnInstantiateInherit
    ) | EcsIdOnInstantiateDontInherit
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 432
try:
    EcsIdExclusive = 1 << 9
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 433
try:
    EcsIdTraversable = 1 << 10
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 434
try:
    EcsIdTag = 1 << 11
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 435
try:
    EcsIdWith = 1 << 12
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 436
try:
    EcsIdCanToggle = 1 << 13
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 437
try:
    EcsIdIsTransitive = 1 << 14
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 438
try:
    EcsIdIsInheritable = 1 << 15
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 440
try:
    EcsIdHasOnAdd = 1 << 16
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 441
try:
    EcsIdHasOnRemove = 1 << 17
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 442
try:
    EcsIdHasOnSet = 1 << 18
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 443
try:
    EcsIdHasOnTableCreate = 1 << 21
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 444
try:
    EcsIdHasOnTableDelete = 1 << 22
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 445
try:
    EcsIdIsSparse = 1 << 23
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 446
try:
    EcsIdDontFragment = 1 << 24
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 447
try:
    EcsIdMatchDontFragment = 1 << 25
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 448
try:
    EcsIdOrderedChildren = 1 << 28
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 449
try:
    EcsIdEventMask = (
        (
            (
                ((EcsIdHasOnAdd | EcsIdHasOnRemove) | EcsIdHasOnSet)
                | EcsIdHasOnTableCreate
            )
            | EcsIdHasOnTableDelete
        )
        | EcsIdIsSparse
    ) | EcsIdOrderedChildren
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 454
try:
    EcsIdMarkedForDelete = 1 << 30
except:
    pass


# /Users/cnifi/git/pyflecs/flecs.h: 461
def ECS_ID_ON_DELETE_FLAG(id):
    return 1 << (id - EcsRemove)


# /Users/cnifi/git/pyflecs/flecs.h: 462
def ECS_ID_ON_DELETE_TARGET_FLAG(id):
    return 1 << (3 + (id - EcsRemove))


# /Users/cnifi/git/pyflecs/flecs.h: 468
def ECS_ID_ON_INSTANTIATE_FLAG(id):
    return 1 << (6 + (id - EcsOverride))


# /Users/cnifi/git/pyflecs/flecs.h: 475
try:
    EcsNonTrivialIdSparse = 1 << 0
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 476
try:
    EcsNonTrivialIdNonFragmenting = 1 << 1
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 477
try:
    EcsNonTrivialIdInherit = 1 << 2
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 484
try:
    EcsIterIsValid = 1 << 0
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 485
try:
    EcsIterNoData = 1 << 1
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 486
try:
    EcsIterNoResults = 1 << 2
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 487
try:
    EcsIterMatchEmptyTables = 1 << 3
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 488
try:
    EcsIterIgnoreThis = 1 << 4
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 489
try:
    EcsIterTrivialChangeDetection = 1 << 5
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 490
try:
    EcsIterHasCondSet = 1 << 6
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 491
try:
    EcsIterProfile = 1 << 7
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 492
try:
    EcsIterTrivialSearch = 1 << 8
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 493
try:
    EcsIterTrivialTest = 1 << 11
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 494
try:
    EcsIterTrivialCached = 1 << 14
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 495
try:
    EcsIterCached = 1 << 15
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 496
try:
    EcsIterFixedInChangeComputed = 1 << 16
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 497
try:
    EcsIterFixedInChanged = 1 << 17
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 498
try:
    EcsIterSkip = 1 << 18
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 499
try:
    EcsIterCppEach = 1 << 19
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 503
try:
    EcsIterTableOnly = 1 << 20
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 510
try:
    EcsEventTableOnly = 1 << 20
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 511
try:
    EcsEventNoOnSet = 1 << 16
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 519
try:
    EcsQueryMatchThis = 1 << 11
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 520
try:
    EcsQueryMatchOnlyThis = 1 << 12
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 521
try:
    EcsQueryMatchOnlySelf = 1 << 13
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 522
try:
    EcsQueryMatchWildcards = 1 << 14
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 523
try:
    EcsQueryMatchNothing = 1 << 15
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 524
try:
    EcsQueryHasCondSet = 1 << 16
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 525
try:
    EcsQueryHasPred = 1 << 17
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 526
try:
    EcsQueryHasScopes = 1 << 18
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 527
try:
    EcsQueryHasRefs = 1 << 19
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 528
try:
    EcsQueryHasOutTerms = 1 << 20
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 529
try:
    EcsQueryHasNonThisOutTerms = 1 << 21
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 530
try:
    EcsQueryHasChangeDetection = 1 << 22
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 531
try:
    EcsQueryIsTrivial = 1 << 23
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 532
try:
    EcsQueryHasCacheable = 1 << 24
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 533
try:
    EcsQueryIsCacheable = 1 << 25
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 534
try:
    EcsQueryHasTableThisVar = 1 << 26
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 535
try:
    EcsQueryCacheYieldEmptyTables = 1 << 27
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 536
try:
    EcsQueryTrivialCache = 1 << 28
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 537
try:
    EcsQueryNested = 1 << 29
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 543
try:
    EcsTermMatchAny = 1 << 0
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 544
try:
    EcsTermMatchAnySrc = 1 << 1
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 545
try:
    EcsTermTransitive = 1 << 2
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 546
try:
    EcsTermReflexive = 1 << 3
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 547
try:
    EcsTermIdInherited = 1 << 4
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 548
try:
    EcsTermIsTrivial = 1 << 5
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 549
try:
    EcsTermIsCacheable = 1 << 7
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 550
try:
    EcsTermIsScope = 1 << 8
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 551
try:
    EcsTermIsMember = 1 << 9
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 552
try:
    EcsTermIsToggle = 1 << 10
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 553
try:
    EcsTermKeepAlive = 1 << 11
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 554
try:
    EcsTermIsSparse = 1 << 12
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 555
try:
    EcsTermIsOr = 1 << 13
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 556
try:
    EcsTermDontFragment = 1 << 14
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 563
try:
    EcsObserverMatchPrefab = 1 << 1
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 564
try:
    EcsObserverMatchDisabled = 1 << 2
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 565
try:
    EcsObserverIsMulti = 1 << 3
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 566
try:
    EcsObserverIsMonitor = 1 << 4
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 567
try:
    EcsObserverIsDisabled = 1 << 5
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 568
try:
    EcsObserverIsParentDisabled = 1 << 6
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 569
try:
    EcsObserverBypassQuery = 1 << 7
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 570
try:
    EcsObserverYieldOnCreate = 1 << 8
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 571
try:
    EcsObserverYieldOnDelete = 1 << 9
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 572
try:
    EcsObserverKeepAlive = 1 << 11
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 578
try:
    EcsTableHasBuiltins = 1 << 1
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 579
try:
    EcsTableIsPrefab = 1 << 2
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 580
try:
    EcsTableHasIsA = 1 << 3
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 581
try:
    EcsTableHasChildOf = 1 << 4
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 582
try:
    EcsTableHasName = 1 << 5
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 583
try:
    EcsTableHasPairs = 1 << 6
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 584
try:
    EcsTableHasModule = 1 << 7
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 585
try:
    EcsTableIsDisabled = 1 << 8
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 586
try:
    EcsTableNotQueryable = 1 << 9
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 587
try:
    EcsTableHasCtors = 1 << 10
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 588
try:
    EcsTableHasDtors = 1 << 11
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 589
try:
    EcsTableHasCopy = 1 << 12
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 590
try:
    EcsTableHasMove = 1 << 13
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 591
try:
    EcsTableHasToggle = 1 << 14
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 592
try:
    EcsTableHasOverrides = 1 << 15
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 594
try:
    EcsTableHasOnAdd = 1 << 16
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 595
try:
    EcsTableHasOnRemove = 1 << 17
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 596
try:
    EcsTableHasOnSet = 1 << 18
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 597
try:
    EcsTableHasOnTableFill = 1 << 19
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 598
try:
    EcsTableHasOnTableEmpty = 1 << 20
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 599
try:
    EcsTableHasOnTableCreate = 1 << 21
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 600
try:
    EcsTableHasOnTableDelete = 1 << 22
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 601
try:
    EcsTableHasSparse = 1 << 23
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 602
try:
    EcsTableHasDontFragment = 1 << 24
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 603
try:
    EcsTableOverrideDontFragment = 1 << 25
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 605
try:
    EcsTableHasTraversable = 1 << 27
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 606
try:
    EcsTableHasOrderedChildren = 1 << 28
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 607
try:
    EcsTableEdgeReparent = 1 << 29
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 608
try:
    EcsTableMarkedForDelete = 1 << 30
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 611
try:
    EcsTableHasLifecycle = EcsTableHasCtors | EcsTableHasDtors
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 612
try:
    EcsTableIsComplex = (EcsTableHasLifecycle | EcsTableHasToggle) | EcsTableHasSparse
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 613
try:
    EcsTableHasAddActions = (
        (EcsTableHasIsA | EcsTableHasCtors) | EcsTableHasOnAdd
    ) | EcsTableHasOnSet
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 614
try:
    EcsTableHasRemoveActions = (EcsTableHasIsA | EcsTableHasDtors) | EcsTableHasOnRemove
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 615
try:
    EcsTableEdgeFlags = (EcsTableHasOnAdd | EcsTableHasOnRemove) | EcsTableHasSparse
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 616
try:
    EcsTableAddEdgeFlags = EcsTableHasOnAdd | EcsTableHasSparse
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 617
try:
    EcsTableRemoveEdgeFlags = (
        EcsTableHasOnRemove | EcsTableHasSparse
    ) | EcsTableHasOrderedChildren
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 623
try:
    EcsAperiodicComponentMonitors = 1 << 2
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 624
try:
    EcsAperiodicEmptyQueries = 1 << 4
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 683
try:
    ECS_CLANG_VERSION = __clang_major__
except:
    pass

# /Library/Developer/CommandLineTools/usr/lib/clang/15.0.0/include/stdbool.h: 21
try:
    true = 1
except:
    pass

# /Library/Developer/CommandLineTools/usr/lib/clang/15.0.0/include/stdbool.h: 22
try:
    false = 0
except:
    pass


# /Users/cnifi/git/pyflecs/flecs.h: 904
def ECS_SIZEOF(T):
    return ECS_CAST(ecs_size_t, sizeof(T))


# /Users/cnifi/git/pyflecs/flecs.h: 943
def ECS_ALIGN(size, alignment):
    return (
        ecs_size_t(
            ord_if_char(
                (
                    (
                        (
                            ((c_size_t(ord_if_char(size))).value - 1)
                            / (c_size_t(ord_if_char(alignment))).value
                        )
                        + 1
                    )
                    * (c_size_t(ord_if_char(alignment))).value
                )
            )
        )
    ).value


# /Users/cnifi/git/pyflecs/flecs.h: 946
def ECS_MAX(a, b):
    return (a > b) and a or b


# /Users/cnifi/git/pyflecs/flecs.h: 947
def ECS_MIN(a, b):
    return (a < b) and a or b


# /Users/cnifi/git/pyflecs/flecs.h: 952
def ECS_CAST(T, V):
    return T(V)


# /Users/cnifi/git/pyflecs/flecs.h: 959
def ECS_CONST_CAST(type, value):
    return (type(uintptr_t))(value)


# /Users/cnifi/git/pyflecs/flecs.h: 966
def ECS_PTR_CAST(type, value):
    return (type(uintptr_t))(value)


# /Users/cnifi/git/pyflecs/flecs.h: 972
def ECS_EQ(a, b):
    return (ecs_os_memcmp(pointer(a), pointer(b), sizeof(a))) == 0


# /Users/cnifi/git/pyflecs/flecs.h: 973
def ECS_NEQ(a, b):
    return not (ECS_EQ(a, b))


# /Users/cnifi/git/pyflecs/flecs.h: 978
def FLECS_VERSION_IMPLSTR(major, minor, patch):
    return (((major + ".") + minor) + ".") + patch


# /Users/cnifi/git/pyflecs/flecs.h: 979
def FLECS_VERSION_IMPL(major, minor, patch):
    return FLECS_VERSION_IMPLSTR(major, minor, patch)


# /Users/cnifi/git/pyflecs/flecs.h: 989
try:
    ecs_world_t_magic = 0x65637377
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 990
try:
    ecs_stage_t_magic = 0x65637373
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 991
try:
    ecs_query_t_magic = 0x65637375
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 992
try:
    ecs_observer_t_magic = 0x65637362
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 999
try:
    ECS_ROW_MASK = 0x0FFFFFFF
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 1000
try:
    ECS_ROW_FLAGS_MASK = ~ECS_ROW_MASK
except:
    pass


# /Users/cnifi/git/pyflecs/flecs.h: 1001
def ECS_RECORD_TO_ROW(v):
    return ECS_CAST(c_int32, ((ECS_CAST(uint32_t, v)) & ECS_ROW_MASK))


# /Users/cnifi/git/pyflecs/flecs.h: 1002
def ECS_RECORD_TO_ROW_FLAGS(v):
    return (ECS_CAST(uint32_t, v)) & ECS_ROW_FLAGS_MASK


# /Users/cnifi/git/pyflecs/flecs.h: 1003
def ECS_ROW_TO_RECORD(row, flags):
    return ECS_CAST(uint32_t, ((ECS_CAST(uint32_t, row)) | flags))


# /Users/cnifi/git/pyflecs/flecs.h: 1005
try:
    ECS_ID_FLAGS_MASK = 0xFF << 60
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 1006
try:
    ECS_ENTITY_MASK = 0xFFFFFFFF
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 1007
try:
    ECS_GENERATION_MASK = 0xFFFF << 32
except:
    pass


# /Users/cnifi/git/pyflecs/flecs.h: 1008
def ECS_GENERATION(e):
    return (e & ECS_GENERATION_MASK) >> 32


# /Users/cnifi/git/pyflecs/flecs.h: 1009
def ECS_GENERATION_INC(e):
    return (e & (~ECS_GENERATION_MASK)) | ((0xFFFF & ((ECS_GENERATION(e)) + 1)) << 32)


# /Users/cnifi/git/pyflecs/flecs.h: 1010
try:
    ECS_COMPONENT_MASK = ~ECS_ID_FLAGS_MASK
except:
    pass


# /Users/cnifi/git/pyflecs/flecs.h: 1012
def ECS_IS_PAIR(id):
    return (id & ECS_ID_FLAGS_MASK) == ECS_PAIR


# /Users/cnifi/git/pyflecs/flecs.h: 1013
def ECS_PAIR_FIRST(e):
    return ecs_entity_t_hi((e & ECS_COMPONENT_MASK))


# /Users/cnifi/git/pyflecs/flecs.h: 1014
def ECS_PAIR_SECOND(e):
    return ecs_entity_t_lo(e)


# /Users/cnifi/git/pyflecs/flecs.h: 1017
def ECS_TERM_REF_FLAGS(ref):
    return ((ref.contents.id).value) & EcsTermRefFlags


# /Users/cnifi/git/pyflecs/flecs.h: 1018
def ECS_TERM_REF_ID(ref):
    return ((ref.contents.id).value) & (~EcsTermRefFlags)


# /Users/cnifi/git/pyflecs/flecs.h: 1032
def ecs_entity_t_lo(value):
    return ECS_CAST(uint32_t, value)


# /Users/cnifi/git/pyflecs/flecs.h: 1033
def ecs_entity_t_hi(value):
    return ECS_CAST(uint32_t, (value >> 32))


# /Users/cnifi/git/pyflecs/flecs.h: 1034
def ecs_entity_t_comb(lo, hi):
    return ((ECS_CAST(uint64_t, hi)) << 32) + (ECS_CAST(uint32_t, lo))


# /Users/cnifi/git/pyflecs/flecs.h: 1036
def ecs_pair(pred, obj):
    return ECS_PAIR | (ecs_entity_t_comb(obj, pred))


# /Users/cnifi/git/pyflecs/flecs.h: 1038
def ecs_pair_first(world, pair):
    return ecs_get_alive(world, (ECS_PAIR_FIRST(pair)))


# /Users/cnifi/git/pyflecs/flecs.h: 1039
def ecs_pair_second(world, pair):
    return ecs_get_alive(world, (ECS_PAIR_SECOND(pair)))


# /Users/cnifi/git/pyflecs/flecs.h: 1040
try:
    ecs_pair_relation = ecs_pair_first
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 1041
try:
    ecs_pair_target = ecs_pair_second
except:
    pass


# /Users/cnifi/git/pyflecs/flecs.h: 1051
def ECS_TABLE_LOCK(world, table):
    return ecs_table_lock(world, table)


# /Users/cnifi/git/pyflecs/flecs.h: 1052
def ECS_TABLE_UNLOCK(world, table):
    return ecs_table_unlock(world, table)


# /Users/cnifi/git/pyflecs/flecs.h: 1183
def ecs_vec_init_t(allocator, vec, T, elem_count):
    return ecs_vec_init_w_dbg_info(
        allocator, vec, (ECS_SIZEOF(T)), elem_count, (("vec<" + T) + ">")
    )


# /Users/cnifi/git/pyflecs/flecs.h: 1191
def ecs_vec_init_if_t(vec, T):
    return ecs_vec_init_if(vec, (ECS_SIZEOF(T)))


# /Users/cnifi/git/pyflecs/flecs.h: 1200
def ecs_vec_fini_t(allocator, vec, T):
    return ecs_vec_fini(allocator, vec, (ECS_SIZEOF(T)))


# /Users/cnifi/git/pyflecs/flecs.h: 1209
def ecs_vec_reset_t(allocator, vec, T):
    return ecs_vec_reset(allocator, vec, (ECS_SIZEOF(T)))


# /Users/cnifi/git/pyflecs/flecs.h: 1231
def ecs_vec_remove_t(vec, T, elem):
    return ecs_vec_remove(vec, (ECS_SIZEOF(T)), elem)


# /Users/cnifi/git/pyflecs/flecs.h: 1240
def ecs_vec_remove_ordered_t(vec, T, elem):
    return ecs_vec_remove_ordered(vec, (ECS_SIZEOF(T)), elem)


# /Users/cnifi/git/pyflecs/flecs.h: 1253
def ecs_vec_copy_t(allocator, vec, T):
    return ecs_vec_copy(allocator, vec, (ECS_SIZEOF(T)))


# /Users/cnifi/git/pyflecs/flecs.h: 1262
def ecs_vec_copy_shrink_t(allocator, vec, T):
    return ecs_vec_copy_shrink(allocator, vec, (ECS_SIZEOF(T)))


# /Users/cnifi/git/pyflecs/flecs.h: 1271
def ecs_vec_reclaim_t(allocator, vec, T):
    return ecs_vec_reclaim(allocator, vec, (ECS_SIZEOF(T)))


# /Users/cnifi/git/pyflecs/flecs.h: 1281
def ecs_vec_set_size_t(allocator, vec, T, elem_count):
    return ecs_vec_set_size(allocator, vec, (ECS_SIZEOF(T)), elem_count)


# /Users/cnifi/git/pyflecs/flecs.h: 1291
def ecs_vec_set_min_size_t(allocator, vec, T, elem_count):
    return ecs_vec_set_min_size(allocator, vec, (ECS_SIZEOF(T)), elem_count)


# /Users/cnifi/git/pyflecs/flecs.h: 1301
def ecs_vec_set_min_count_t(allocator, vec, T, elem_count):
    return ecs_vec_set_min_count(allocator, vec, (ECS_SIZEOF(T)), elem_count)


# /Users/cnifi/git/pyflecs/flecs.h: 1311
def ecs_vec_set_min_count_zeromem_t(allocator, vec, T, elem_count):
    return ecs_vec_set_min_count_zeromem(allocator, vec, (ECS_SIZEOF(T)), elem_count)


# /Users/cnifi/git/pyflecs/flecs.h: 1321
def ecs_vec_set_count_t(allocator, vec, T, elem_count):
    return ecs_vec_set_count(allocator, vec, (ECS_SIZEOF(T)), elem_count)


# /Users/cnifi/git/pyflecs/flecs.h: 1331
def ecs_vec_grow_t(allocator, vec, T, elem_count):
    return ecs_vec_grow(allocator, vec, (ECS_SIZEOF(T)), elem_count)


# /Users/cnifi/git/pyflecs/flecs.h: 1386
try:
    FLECS_SPARSE_PAGE_SIZE = 1 << FLECS_SPARSE_PAGE_BITS
except:
    pass


# /Users/cnifi/git/pyflecs/flecs.h: 1389
def FLECS_SPARSE_PAGE(index):
    return (
        c_int32(
            ord_if_char(
                ((uint32_t(ord_if_char(index))).value >> FLECS_SPARSE_PAGE_BITS)
            )
        )
    ).value


# /Users/cnifi/git/pyflecs/flecs.h: 1392
def FLECS_SPARSE_OFFSET(index):
    return (c_int32(ord_if_char(index))).value & (FLECS_SPARSE_PAGE_SIZE - 1)


# /Users/cnifi/git/pyflecs/flecs.h: 1416
def flecs_sparse_init_t(result, allocator, page_allocator, T):
    return flecs_sparse_init(result, allocator, page_allocator, (ECS_SIZEOF(T)))


# /Users/cnifi/git/pyflecs/flecs.h: 1454
def flecs_sparse_remove_t(sparse, T, id):
    return flecs_sparse_remove(sparse, (ECS_SIZEOF(T)), id)


# /Users/cnifi/git/pyflecs/flecs.h: 1463
def flecs_sparse_remove_w_gen_t(sparse, T, id):
    return flecs_sparse_remove_w_gen(sparse, (ECS_SIZEOF(T)), id)


# /Users/cnifi/git/pyflecs/flecs.h: 1552
def ecs_sparse_init_t(sparse, T):
    return ecs_sparse_init(sparse, (ECS_SIZEOF(T)))


# /Users/cnifi/git/pyflecs/flecs.h: 1635
def flecs_ballocator_init_t(ba, T):
    return flecs_ballocator_init(ba, (ECS_SIZEOF(T)))


# /Users/cnifi/git/pyflecs/flecs.h: 1637
def flecs_ballocator_init_n(ba, T, count):
    return flecs_ballocator_init(ba, ((ECS_SIZEOF(T)) * count))


# /Users/cnifi/git/pyflecs/flecs.h: 1644
def flecs_ballocator_new_t(T):
    return flecs_ballocator_new((ECS_SIZEOF(T)))


# /Users/cnifi/git/pyflecs/flecs.h: 1646
def flecs_ballocator_new_n(T, count):
    return flecs_ballocator_new(((ECS_SIZEOF(T)) * count))


# /Users/cnifi/git/pyflecs/flecs.h: 1742
try:
    FLECS_STACK_PAGE_OFFSET = ECS_ALIGN((ECS_SIZEOF(ecs_stack_page_t)), 16)
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 1743
try:
    FLECS_STACK_PAGE_SIZE = 1024 - FLECS_STACK_PAGE_OFFSET
except:
    pass


# /Users/cnifi/git/pyflecs/flecs.h: 1782
def flecs_stack_free_t(ptr, T):
    return flecs_stack_free(ptr, (ECS_SIZEOF(T)))


# /Users/cnifi/git/pyflecs/flecs.h: 1785
def flecs_stack_free_n(ptr, T, count):
    return flecs_stack_free(ptr, ((ECS_SIZEOF(T)) * count))


# /Users/cnifi/git/pyflecs/flecs.h: 1952
def ecs_map_count(map):
    return map and (map.contents.count) or 0


# /Users/cnifi/git/pyflecs/flecs.h: 1955
def ecs_map_is_init(map):
    return map and (((map.contents.bucket_shift).value) != 0) or false


# /Users/cnifi/git/pyflecs/flecs.h: 1975
def ecs_map_get_ptr(m, k):
    return ECS_CAST(POINTER(None), (ecs_map_get_deref_(m, k)))


# /Users/cnifi/git/pyflecs/flecs.h: 1978
def ecs_map_insert_ptr(m, k, v):
    return ecs_map_insert(m, k, (ECS_CAST(ecs_map_val_t, (ECS_PTR_CAST(uintptr_t, v)))))


# /Users/cnifi/git/pyflecs/flecs.h: 1981
def ecs_map_remove_ptr(m, k):
    return ECS_PTR_CAST(POINTER(None), (ECS_CAST(uintptr_t, (ecs_map_remove(m, k)))))


# /Users/cnifi/git/pyflecs/flecs.h: 1983
def ecs_map_key(it):
    return (it.contents.res)[0]


# /Users/cnifi/git/pyflecs/flecs.h: 1984
def ecs_map_value(it):
    return (it.contents.res)[1]


# /Users/cnifi/git/pyflecs/flecs.h: 1985
def ecs_map_ptr(it):
    return ECS_PTR_CAST(POINTER(None), (ECS_CAST(uintptr_t, (ecs_map_value(it)))))


# /Users/cnifi/git/pyflecs/flecs.h: 2047
def flecs_allocator(obj):
    return pointer(((obj.contents.allocators).dyn))


# /Users/cnifi/git/pyflecs/flecs.h: 2049
def flecs_alloc(a, size):
    return flecs_balloc((flecs_allocator_get(a, size)))


# /Users/cnifi/git/pyflecs/flecs.h: 2050
def flecs_alloc_w_dbg_info(a, size, type_name):
    return flecs_balloc_w_dbg_info((flecs_allocator_get(a, size)), type_name)


# /Users/cnifi/git/pyflecs/flecs.h: 2051
def flecs_alloc_t(a, T):
    return flecs_alloc_w_dbg_info(a, (ECS_SIZEOF(T)), T)


# /Users/cnifi/git/pyflecs/flecs.h: 2052
def flecs_alloc_n(a, T, count):
    return flecs_alloc_w_dbg_info(a, ((ECS_SIZEOF(T)) * count), T)


# /Users/cnifi/git/pyflecs/flecs.h: 2054
def flecs_calloc(a, size):
    return flecs_bcalloc((flecs_allocator_get(a, size)))


# /Users/cnifi/git/pyflecs/flecs.h: 2055
def flecs_calloc_w_dbg_info(a, size, type_name):
    return flecs_bcalloc_w_dbg_info((flecs_allocator_get(a, size)), type_name)


# /Users/cnifi/git/pyflecs/flecs.h: 2056
def flecs_calloc_t(a, T):
    return flecs_calloc_w_dbg_info(a, (ECS_SIZEOF(T)), T)


# /Users/cnifi/git/pyflecs/flecs.h: 2057
def flecs_calloc_n(a, T, count):
    return flecs_calloc_w_dbg_info(a, ((ECS_SIZEOF(T)) * count), T)


# /Users/cnifi/git/pyflecs/flecs.h: 2059
def flecs_free(a, size, ptr):
    return flecs_bfree(ptr and (flecs_allocator_get(a, size)) or NULL, ptr)


# /Users/cnifi/git/pyflecs/flecs.h: 2061
def flecs_free_t(a, T, ptr):
    return flecs_bfree_w_dbg_info(
        ptr and (flecs_allocator_get(a, (ECS_SIZEOF(T)))) or NULL, ptr, T
    )


# /Users/cnifi/git/pyflecs/flecs.h: 2063
def flecs_free_n(a, T, count, ptr):
    return flecs_bfree_w_dbg_info(
        ptr and (flecs_allocator_get(a, ((ECS_SIZEOF(T)) * count))) or NULL, ptr, T
    )


# /Users/cnifi/git/pyflecs/flecs.h: 2067
def flecs_realloc(a, size_dst, size_src, ptr):
    return flecs_brealloc(
        (flecs_allocator_get(a, size_dst)), (flecs_allocator_get(a, size_src)), ptr
    )


# /Users/cnifi/git/pyflecs/flecs.h: 2071
def flecs_realloc_w_dbg_info(a, size_dst, size_src, ptr, type_name):
    return flecs_brealloc_w_dbg_info(
        (flecs_allocator_get(a, size_dst)),
        (flecs_allocator_get(a, size_src)),
        ptr,
        type_name,
    )


# /Users/cnifi/git/pyflecs/flecs.h: 2076
def flecs_realloc_n(a, T, count_dst, count_src, ptr):
    return flecs_realloc(
        a, ((ECS_SIZEOF(T)) * count_dst), ((ECS_SIZEOF(T)) * count_src), ptr
    )


# /Users/cnifi/git/pyflecs/flecs.h: 2079
def flecs_dup_n(a, T, count, ptr):
    return flecs_dup(a, ((ECS_SIZEOF(T)) * count), ptr)


# /Users/cnifi/git/pyflecs/flecs.h: 2145
try:
    ECS_STRBUF_SMALL_STRING_SIZE = 512
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 2146
try:
    ECS_STRBUF_MAX_LIST_DEPTH = 32
except:
    pass


# /Users/cnifi/git/pyflecs/flecs.h: 2294
def ecs_strbuf_appendlit(buf, str):
    return ecs_strbuf_appendstrn(
        buf, str, (c_int32(ord_if_char((sizeof(str) - 1)))).value
    )


# /Users/cnifi/git/pyflecs/flecs.h: 2297
def ecs_strbuf_list_appendlit(buf, str):
    return ecs_strbuf_list_appendstrn(
        buf, str, (c_int32(ord_if_char((sizeof(str) - 1)))).value
    )


# /Users/cnifi/git/pyflecs/flecs.h: 2700
def ecs_os_malloc(size):
    return (ecs_os_api.malloc_)(size)


# /Users/cnifi/git/pyflecs/flecs.h: 2703
def ecs_os_free(ptr):
    return (ecs_os_api.free_)(ptr)


# /Users/cnifi/git/pyflecs/flecs.h: 2706
def ecs_os_realloc(ptr, size):
    return (ecs_os_api.realloc_)(ptr, size)


# /Users/cnifi/git/pyflecs/flecs.h: 2709
def ecs_os_calloc(size):
    return (ecs_os_api.calloc_)(size)


# /Users/cnifi/git/pyflecs/flecs.h: 2714
def ecs_os_alloca(size):
    return alloca((c_size_t(ord_if_char(size))).value)


# /Users/cnifi/git/pyflecs/flecs.h: 2729
def ecs_os_strdup(str):
    return (ecs_os_api.strdup_)(str)


# /Users/cnifi/git/pyflecs/flecs.h: 2740
def ecs_os_strlen(str):
    return (ecs_size_t(ord_if_char((strlen(str))))).value


# /Users/cnifi/git/pyflecs/flecs.h: 2741
def ecs_os_strncmp(str1, str2, num):
    return strncmp(str1, str2, (c_size_t(ord_if_char(num))).value)


# /Users/cnifi/git/pyflecs/flecs.h: 2742
def ecs_os_memcmp(ptr1, ptr2, num):
    return memcmp(ptr1, ptr2, (c_size_t(ord_if_char(num))).value)


# /Users/cnifi/git/pyflecs/flecs.h: 2743
def ecs_os_memcpy(ptr1, ptr2, num):
    return memcpy(ptr1, ptr2, (c_size_t(ord_if_char(num))).value)


# /Users/cnifi/git/pyflecs/flecs.h: 2744
def ecs_os_memset(ptr, value, num):
    return memset(ptr, value, (c_size_t(ord_if_char(num))).value)


# /Users/cnifi/git/pyflecs/flecs.h: 2745
def ecs_os_memmove(dst, src, size):
    return memmove(dst, src, (c_size_t(ord_if_char(size))).value)


# /Users/cnifi/git/pyflecs/flecs.h: 2748
def ecs_os_memcpy_t(ptr1, ptr2, T):
    return ecs_os_memcpy(ptr1, ptr2, (ECS_SIZEOF(T)))


# /Users/cnifi/git/pyflecs/flecs.h: 2749
def ecs_os_memcpy_n(ptr1, ptr2, T, count):
    return ecs_os_memcpy(
        ptr1, ptr2, ((ECS_SIZEOF(T)) * (c_size_t(ord_if_char(count))).value)
    )


# /Users/cnifi/git/pyflecs/flecs.h: 2750
def ecs_os_memcmp_t(ptr1, ptr2, T):
    return ecs_os_memcmp(ptr1, ptr2, (ECS_SIZEOF(T)))


# /Users/cnifi/git/pyflecs/flecs.h: 2752
def ecs_os_memmove_t(ptr1, ptr2, T):
    return ecs_os_memmove(ptr1, ptr2, (ECS_SIZEOF(T)))


# /Users/cnifi/git/pyflecs/flecs.h: 2753
def ecs_os_memmove_n(ptr1, ptr2, T, count):
    return ecs_os_memmove(
        ptr1, ptr2, ((ECS_SIZEOF(T)) * (c_size_t(ord_if_char(count))).value)
    )


# /Users/cnifi/git/pyflecs/flecs.h: 2754
def ecs_os_memmove_t(ptr1, ptr2, T):
    return ecs_os_memmove(ptr1, ptr2, (ECS_SIZEOF(T)))


# /Users/cnifi/git/pyflecs/flecs.h: 2756
def ecs_os_strcmp(str1, str2):
    return strcmp(str1, str2)


# /Users/cnifi/git/pyflecs/flecs.h: 2757
def ecs_os_memset_t(ptr, value, T):
    return ecs_os_memset(ptr, value, (ECS_SIZEOF(T)))


# /Users/cnifi/git/pyflecs/flecs.h: 2758
def ecs_os_memset_n(ptr, value, T, count):
    return ecs_os_memset(
        ptr, value, ((ECS_SIZEOF(T)) * (c_size_t(ord_if_char(count))).value)
    )


# /Users/cnifi/git/pyflecs/flecs.h: 2759
def ecs_os_zeromem(ptr):
    return ecs_os_memset(ptr, 0, (ECS_SIZEOF((ptr[0]))))


# /Users/cnifi/git/pyflecs/flecs.h: 2761
def ecs_os_memdup_t(ptr, T):
    return ecs_os_memdup(ptr, (ECS_SIZEOF(T)))


# /Users/cnifi/git/pyflecs/flecs.h: 2762
def ecs_os_memdup_n(ptr, T, count):
    return ecs_os_memdup(ptr, ((ECS_SIZEOF(T)) * count))


# /Users/cnifi/git/pyflecs/flecs.h: 2774
def ecs_os_strcat(str1, str2):
    return strcat(str1, str2)


# /Users/cnifi/git/pyflecs/flecs.h: 2777
def ecs_os_strcpy(str1, str2):
    return strcpy(str1, str2)


# /Users/cnifi/git/pyflecs/flecs.h: 2778
def ecs_os_strncpy(str1, str2, len):
    return strncpy(str1, str2, (ECS_CAST(c_size_t, len)))


# /Users/cnifi/git/pyflecs/flecs.h: 2789
def ecs_os_thread_new(callback, param):
    return (ecs_os_api.thread_new_)(callback, param)


# /Users/cnifi/git/pyflecs/flecs.h: 2790
def ecs_os_thread_join(thread):
    return (ecs_os_api.thread_join_)(thread)


# TODO
# /Users/cnifi/git/pyflecs/flecs.h: 2791
# try:
#   ecs_os_thread_self = (ecs_os_api.thread_self_)()
# except:
#   pass


# /Users/cnifi/git/pyflecs/flecs.h: 2794
def ecs_os_task_new(callback, param):
    return (ecs_os_api.task_new_)(callback, param)


# /Users/cnifi/git/pyflecs/flecs.h: 2795
def ecs_os_task_join(thread):
    return (ecs_os_api.task_join_)(thread)


# /Users/cnifi/git/pyflecs/flecs.h: 2798
def ecs_os_ainc(value):
    return (ecs_os_api.ainc_)(value)


# /Users/cnifi/git/pyflecs/flecs.h: 2799
def ecs_os_adec(value):
    return (ecs_os_api.adec_)(value)


# /Users/cnifi/git/pyflecs/flecs.h: 2800
def ecs_os_lainc(value):
    return (ecs_os_api.lainc_)(value)


# /Users/cnifi/git/pyflecs/flecs.h: 2801
def ecs_os_ladec(value):
    return (ecs_os_api.ladec_)(value)


# TODO
# /Users/cnifi/git/pyflecs/flecs.h: 2804
# try:
#   ecs_os_mutex_new = (ecs_os_api.mutex_new_)()
# except:
#   pass


# /Users/cnifi/git/pyflecs/flecs.h: 2805
def ecs_os_mutex_free(mutex):
    return (ecs_os_api.mutex_free_)(mutex)


# /Users/cnifi/git/pyflecs/flecs.h: 2806
def ecs_os_mutex_lock(mutex):
    return (ecs_os_api.mutex_lock_)(mutex)


# /Users/cnifi/git/pyflecs/flecs.h: 2807
def ecs_os_mutex_unlock(mutex):
    return (ecs_os_api.mutex_unlock_)(mutex)


# TODO
# /Users/cnifi/git/pyflecs/flecs.h: 2810
# try:
#   ecs_os_cond_new = (ecs_os_api.cond_new_)()
# except:
#   pass


# /Users/cnifi/git/pyflecs/flecs.h: 2811
def ecs_os_cond_free(cond):
    return (ecs_os_api.cond_free_)(cond)


# /Users/cnifi/git/pyflecs/flecs.h: 2812
def ecs_os_cond_signal(cond):
    return (ecs_os_api.cond_signal_)(cond)


# /Users/cnifi/git/pyflecs/flecs.h: 2813
def ecs_os_cond_broadcast(cond):
    return (ecs_os_api.cond_broadcast_)(cond)


# /Users/cnifi/git/pyflecs/flecs.h: 2814
def ecs_os_cond_wait(cond, mutex):
    return (ecs_os_api.cond_wait_)(cond, mutex)


# /Users/cnifi/git/pyflecs/flecs.h: 2817
def ecs_os_sleep(sec, nanosec):
    return (ecs_os_api.sleep_)(sec, nanosec)


# TODO
# /Users/cnifi/git/pyflecs/flecs.h: 2818
# try:
#   ecs_os_now = (ecs_os_api.now_)()
# except:
#   pass


# /Users/cnifi/git/pyflecs/flecs.h: 2819
def ecs_os_get_time(time_out):
    return (ecs_os_api.get_time_)(time_out)


# /Users/cnifi/git/pyflecs/flecs.h: 2828
def ecs_os_inc(v):
    return (v[0]) + 1


# /Users/cnifi/git/pyflecs/flecs.h: 2829
def ecs_os_linc(v):
    return (v[0]) + 1


# /Users/cnifi/git/pyflecs/flecs.h: 2830
def ecs_os_dec(v):
    return (v[0]) - 1


# /Users/cnifi/git/pyflecs/flecs.h: 2831
def ecs_os_ldec(v):
    return (v[0]) - 1


# TODO
# /Users/cnifi/git/pyflecs/flecs.h: 2852
# try:
#   ecs_os_abort = (ecs_os_api.abort_)()
# except:
#   pass


# /Users/cnifi/git/pyflecs/flecs.h: 2855
def ecs_os_dlopen(libname):
    return (ecs_os_api.dlopen_)(libname)


# /Users/cnifi/git/pyflecs/flecs.h: 2856
def ecs_os_dlproc(lib, procname):
    return (ecs_os_api.dlproc_)(lib, procname)


# /Users/cnifi/git/pyflecs/flecs.h: 2857
def ecs_os_dlclose(lib):
    return (ecs_os_api.dlclose_)(lib)


# /Users/cnifi/git/pyflecs/flecs.h: 2860
def ecs_os_module_to_dl(lib):
    return (ecs_os_api.module_to_dl_)(lib)


# /Users/cnifi/git/pyflecs/flecs.h: 2861
def ecs_os_module_to_etc(lib):
    return (ecs_os_api.module_to_etc_)(lib)


# /Users/cnifi/git/pyflecs/flecs.h: 3426
try:
    EcsSelf = 1 << 63
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 3432
try:
    EcsUp = 1 << 62
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 3438
try:
    EcsTrav = 1 << 61
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 3444
try:
    EcsCascade = 1 << 60
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 3450
try:
    EcsDesc = 1 << 59
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 3456
try:
    EcsIsVariable = 1 << 58
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 3462
try:
    EcsIsEntity = 1 << 57
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 3468
try:
    EcsIsName = 1 << 56
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 3474
try:
    EcsTraverseFlags = (((EcsSelf | EcsUp) | EcsTrav) | EcsCascade) | EcsDesc
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 3480
try:
    EcsTermRefFlags = ((EcsTraverseFlags | EcsIsVariable) | EcsIsEntity) | EcsIsName
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 3597
try:
    ECS_TYPE_HOOK_CTOR = ECS_CAST(ecs_flags32_t, (1 << 0))
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 3598
try:
    ECS_TYPE_HOOK_DTOR = ECS_CAST(ecs_flags32_t, (1 << 1))
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 3599
try:
    ECS_TYPE_HOOK_COPY = ECS_CAST(ecs_flags32_t, (1 << 2))
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 3600
try:
    ECS_TYPE_HOOK_MOVE = ECS_CAST(ecs_flags32_t, (1 << 3))
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 3601
try:
    ECS_TYPE_HOOK_COPY_CTOR = ECS_CAST(ecs_flags32_t, (1 << 4))
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 3602
try:
    ECS_TYPE_HOOK_MOVE_CTOR = ECS_CAST(ecs_flags32_t, (1 << 5))
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 3603
try:
    ECS_TYPE_HOOK_CTOR_MOVE_DTOR = ECS_CAST(ecs_flags32_t, (1 << 6))
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 3604
try:
    ECS_TYPE_HOOK_MOVE_DTOR = ECS_CAST(ecs_flags32_t, (1 << 7))
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 3605
try:
    ECS_TYPE_HOOK_CMP = ECS_CAST(ecs_flags32_t, (1 << 8))
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 3606
try:
    ECS_TYPE_HOOK_EQUALS = ECS_CAST(ecs_flags32_t, (1 << 9))
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 3610
try:
    ECS_TYPE_HOOK_CTOR_ILLEGAL = ECS_CAST(ecs_flags32_t, (1 << 10))
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 3611
try:
    ECS_TYPE_HOOK_DTOR_ILLEGAL = ECS_CAST(ecs_flags32_t, (1 << 12))
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 3612
try:
    ECS_TYPE_HOOK_COPY_ILLEGAL = ECS_CAST(ecs_flags32_t, (1 << 13))
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 3613
try:
    ECS_TYPE_HOOK_MOVE_ILLEGAL = ECS_CAST(ecs_flags32_t, (1 << 14))
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 3614
try:
    ECS_TYPE_HOOK_COPY_CTOR_ILLEGAL = ECS_CAST(ecs_flags32_t, (1 << 15))
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 3615
try:
    ECS_TYPE_HOOK_MOVE_CTOR_ILLEGAL = ECS_CAST(ecs_flags32_t, (1 << 16))
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 3616
try:
    ECS_TYPE_HOOK_CTOR_MOVE_DTOR_ILLEGAL = ECS_CAST(ecs_flags32_t, (1 << 17))
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 3617
try:
    ECS_TYPE_HOOK_MOVE_DTOR_ILLEGAL = ECS_CAST(ecs_flags32_t, (1 << 18))
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 3618
try:
    ECS_TYPE_HOOK_CMP_ILLEGAL = ECS_CAST(ecs_flags32_t, (1 << 19))
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 3619
try:
    ECS_TYPE_HOOK_EQUALS_ILLEGAL = ECS_CAST(ecs_flags32_t, (1 << 20))
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 3623
try:
    ECS_TYPE_HOOKS = (
        (
            (
                (
                    (
                        (
                            (
                                (ECS_TYPE_HOOK_CTOR | ECS_TYPE_HOOK_DTOR)
                                | ECS_TYPE_HOOK_COPY
                            )
                            | ECS_TYPE_HOOK_MOVE
                        )
                        | ECS_TYPE_HOOK_COPY_CTOR
                    )
                    | ECS_TYPE_HOOK_MOVE_CTOR
                )
                | ECS_TYPE_HOOK_CTOR_MOVE_DTOR
            )
            | ECS_TYPE_HOOK_MOVE_DTOR
        )
        | ECS_TYPE_HOOK_CMP
    ) | ECS_TYPE_HOOK_EQUALS
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 3629
try:
    ECS_TYPE_HOOKS_ILLEGAL = (
        (
            (
                (
                    (
                        (
                            (
                                (
                                    ECS_TYPE_HOOK_CTOR_ILLEGAL
                                    | ECS_TYPE_HOOK_DTOR_ILLEGAL
                                )
                                | ECS_TYPE_HOOK_COPY_ILLEGAL
                            )
                            | ECS_TYPE_HOOK_MOVE_ILLEGAL
                        )
                        | ECS_TYPE_HOOK_COPY_CTOR_ILLEGAL
                    )
                    | ECS_TYPE_HOOK_MOVE_CTOR_ILLEGAL
                )
                | ECS_TYPE_HOOK_CTOR_MOVE_DTOR_ILLEGAL
            )
            | ECS_TYPE_HOOK_MOVE_DTOR_ILLEGAL
        )
        | ECS_TYPE_HOOK_CMP_ILLEGAL
    ) | ECS_TYPE_HOOK_EQUALS_ILLEGAL
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 3899
try:
    ECS_MAX_COMPONENT_ID = ~(uint32_t(ord_if_char((ECS_ID_FLAGS_MASK >> 32)))).value
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 3903
try:
    ECS_MAX_RECURSION = 512
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 3906
try:
    ECS_MAX_TOKEN_SIZE = 256
except:
    pass


# /Users/cnifi/git/pyflecs/flecs.h: 4216
def ECS_OFFSET(o, offset):
    return cast(
        ((uintptr_t(ord_if_char(o))).value + (uintptr_t(ord_if_char(offset))).value),
        POINTER(None),
    )


# /Users/cnifi/git/pyflecs/flecs.h: 4218
def ECS_OFFSET_T(o, T):
    return ECS_OFFSET(o, (ECS_SIZEOF(T)))


# /Users/cnifi/git/pyflecs/flecs.h: 4220
def ECS_ELEM(ptr, size, index):
    return ECS_OFFSET(ptr, (size * index))


# /Users/cnifi/git/pyflecs/flecs.h: 4221
def ECS_ELEM_T(o, T, index):
    return ECS_ELEM(o, (ECS_SIZEOF(T)), index)


# /Users/cnifi/git/pyflecs/flecs.h: 4235
def ECS_BIT_IS_SET(flags, bit):
    return flags & bit


# /Users/cnifi/git/pyflecs/flecs.h: 4296
def flecs_hashmap_init(hm, K, V, hash, compare, allocator):
    return flecs_hashmap_init_(
        hm, (ECS_SIZEOF(K)), (ECS_SIZEOF(V)), hash, compare, allocator
    )


# /Users/cnifi/git/pyflecs/flecs.h: 4320
def flecs_hashmap_ensure(map, key, V):
    return flecs_hashmap_ensure_(map, (ECS_SIZEOF((key[0]))), key, (ECS_SIZEOF(V)))


# /Users/cnifi/git/pyflecs/flecs.h: 4331
def flecs_hashmap_set(map, key, value):
    return flecs_hashmap_set_(
        map, (ECS_SIZEOF((key[0]))), key, (ECS_SIZEOF((value[0]))), value
    )


# /Users/cnifi/git/pyflecs/flecs.h: 4341
def flecs_hashmap_remove(map, key, V):
    return flecs_hashmap_remove_(map, (ECS_SIZEOF((key[0]))), key, (ECS_SIZEOF(V)))


# /Users/cnifi/git/pyflecs/flecs.h: 4352
def flecs_hashmap_remove_w_hash(map, key, V, hash):
    return flecs_hashmap_remove_w_hash_(
        map, (ECS_SIZEOF((key[0]))), key, (ECS_SIZEOF(V)), hash
    )


# /Users/cnifi/git/pyflecs/flecs.h: 4936
try:
    EcsQueryMatchPrefab = 1 << 1
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 4942
try:
    EcsQueryMatchDisabled = 1 << 2
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 4948
try:
    EcsQueryMatchEmptyTables = 1 << 3
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 4954
try:
    EcsQueryAllowUnresolvedByName = 1 << 6
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 4960
try:
    EcsQueryTableOnly = 1 << 7
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 4972
try:
    EcsQueryDetectChanges = 1 << 8
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5402
try:
    EcsSingleton = EcsVariable
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5672
try:
    EcsFirstUserComponentId = 8
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 5676
try:
    EcsFirstUserEntityId = FLECS_HI_COMPONENT_ID + 128
except:
    pass


# /Users/cnifi/git/pyflecs/flecs.h: 10387
def ecs_new_w_pair(world, first, second):
    return ecs_new_w_id(world, (ecs_pair(first, second)))


# /Users/cnifi/git/pyflecs/flecs.h: 10403
def ecs_add_pair(world, subject, first, second):
    return ecs_add_id(world, subject, (ecs_pair(first, second)))


# /Users/cnifi/git/pyflecs/flecs.h: 10410
def ecs_remove_pair(world, subject, first, second):
    return ecs_remove_id(world, subject, (ecs_pair(first, second)))


# /Users/cnifi/git/pyflecs/flecs.h: 10417
def ecs_auto_override_pair(world, subject, first, second):
    return ecs_auto_override_id(world, subject, (ecs_pair(first, second)))


# /Users/cnifi/git/pyflecs/flecs.h: 10508
def ecs_modified_pair(world, subject, first, second):
    return ecs_modified_id(world, subject, (ecs_pair(first, second)))


# /Users/cnifi/git/pyflecs/flecs.h: 10588
def ecs_has_pair(world, entity, first, second):
    return ecs_has_id(world, entity, (ecs_pair(first, second)))


# /Users/cnifi/git/pyflecs/flecs.h: 10591
def ecs_owns_pair(world, entity, first, second):
    return ecs_owns_id(world, entity, (ecs_pair(first, second)))


# /Users/cnifi/git/pyflecs/flecs.h: 10636
def ecs_lookup_from(world, parent, path):
    return ecs_lookup_path_w_sep(world, parent, path, ".", NULL, true)


# /Users/cnifi/git/pyflecs/flecs.h: 10639
def ecs_get_path_from(world, parent, child):
    return ecs_get_path_w_sep(world, parent, child, ".", NULL)


# /Users/cnifi/git/pyflecs/flecs.h: 10642
def ecs_get_path(world, child):
    return ecs_get_path_w_sep(world, 0, child, ".", NULL)


# /Users/cnifi/git/pyflecs/flecs.h: 10645
def ecs_get_path_buf(world, child, buf):
    return ecs_get_path_w_sep_buf(world, 0, child, ".", NULL, buf, false)


# /Users/cnifi/git/pyflecs/flecs.h: 10648
def ecs_new_from_path(world, parent, path):
    return ecs_new_from_path_w_sep(world, parent, path, ".", NULL)


# /Users/cnifi/git/pyflecs/flecs.h: 10651
def ecs_add_path(world, entity, parent, path):
    return ecs_add_path_w_sep(world, entity, parent, path, ".", NULL)


# /Users/cnifi/git/pyflecs/flecs.h: 10654
def ecs_add_fullpath(world, entity, path):
    return ecs_add_path_w_sep(world, entity, 0, path, ".", NULL)


# /Users/cnifi/git/pyflecs/flecs.h: 10939
def ecs_isa(e):
    return ecs_pair(EcsIsA, e)


# /Users/cnifi/git/pyflecs/flecs.h: 10940
def ecs_childof(e):
    return ecs_pair(EcsChildOf, e)


# /Users/cnifi/git/pyflecs/flecs.h: 10941
def ecs_dependson(e):
    return ecs_pair(EcsDependsOn, e)


# /Users/cnifi/git/pyflecs/flecs.h: 10942
def ecs_with(e):
    return ecs_pair(EcsWith, e)


# /Users/cnifi/git/pyflecs/flecs.h: 10945
def ecs_each_pair(world, r, t):
    return ecs_each_id(world, (ecs_pair(r, t)))


# /Users/cnifi/git/pyflecs/flecs.h: 11303
try:
    ecs_should_log_1 = ecs_should_log(1)
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 11304
try:
    ecs_should_log_2 = ecs_should_log(2)
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 11305
try:
    ecs_should_log_3 = ecs_should_log(3)
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 11388
try:
    ecs_log_push = ecs_log_push_(0)
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 11389
try:
    ecs_log_pop = ecs_log_pop_(0)
except:
    pass


# /Users/cnifi/git/pyflecs/flecs.h: 11475
def ecs_parser_errorv(name, expr, column, fmt, args):
    return ecs_parser_errorv_(name, expr, column, fmt, args)


# /Users/cnifi/git/pyflecs/flecs.h: 11481
def ecs_parser_warningv(name, expr, column, fmt, args):
    return ecs_parser_warningv_(name, expr, column, fmt, args)


# /Users/cnifi/git/pyflecs/flecs.h: 11571
try:
    ECS_INVALID_OPERATION = 1
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 11572
try:
    ECS_INVALID_PARAMETER = 2
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 11573
try:
    ECS_CONSTRAINT_VIOLATED = 3
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 11574
try:
    ECS_OUT_OF_MEMORY = 4
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 11575
try:
    ECS_OUT_OF_RANGE = 5
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 11576
try:
    ECS_UNSUPPORTED = 6
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 11577
try:
    ECS_INTERNAL_ERROR = 7
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 11578
try:
    ECS_ALREADY_DEFINED = 8
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 11579
try:
    ECS_MISSING_OS_API = 9
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 11580
try:
    ECS_OPERATION_FAILED = 10
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 11581
try:
    ECS_INVALID_CONVERSION = 11
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 11582
try:
    ECS_ID_IN_USE = 12
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 11583
try:
    ECS_CYCLE_DETECTED = 13
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 11584
try:
    ECS_LEAK_DETECTED = 14
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 11585
try:
    ECS_DOUBLE_FREE = 15
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 11587
try:
    ECS_INCONSISTENT_NAME = 20
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 11588
try:
    ECS_NAME_IN_USE = 21
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 11589
try:
    ECS_NOT_A_COMPONENT = 22
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 11590
try:
    ECS_INVALID_COMPONENT_SIZE = 23
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 11591
try:
    ECS_INVALID_COMPONENT_ALIGNMENT = 24
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 11592
try:
    ECS_COMPONENT_NOT_REGISTERED = 25
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 11593
try:
    ECS_INCONSISTENT_COMPONENT_ID = 26
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 11594
try:
    ECS_INCONSISTENT_COMPONENT_ACTION = 27
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 11595
try:
    ECS_MODULE_UNDEFINED = 28
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 11596
try:
    ECS_MISSING_SYMBOL = 29
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 11597
try:
    ECS_ALREADY_IN_USE = 30
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 11599
try:
    ECS_ACCESS_VIOLATION = 40
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 11600
try:
    ECS_COLUMN_INDEX_OUT_OF_RANGE = 41
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 11601
try:
    ECS_COLUMN_IS_NOT_SHARED = 42
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 11602
try:
    ECS_COLUMN_IS_SHARED = 43
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 11603
try:
    ECS_COLUMN_TYPE_MISMATCH = 45
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 11605
try:
    ECS_INVALID_WHILE_READONLY = 70
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 11606
try:
    ECS_LOCKED_STORAGE = 71
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 11607
try:
    ECS_INVALID_FROM_WORKER = 72
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 11614
try:
    ECS_BLACK = "\\033[1;30m"
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 11615
try:
    ECS_RED = "\\033[0;31m"
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 11616
try:
    ECS_GREEN = "\\033[0;32m"
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 11617
try:
    ECS_YELLOW = "\\033[0;33m"
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 11618
try:
    ECS_BLUE = "\\033[0;34m"
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 11619
try:
    ECS_MAGENTA = "\\033[0;35m"
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 11620
try:
    ECS_CYAN = "\\033[0;36m"
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 11621
try:
    ECS_WHITE = "\\033[1;37m"
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 11622
try:
    ECS_GREY = "\\033[0;37m"
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 11623
try:
    ECS_NORMAL = "\\033[0;49m"
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 11624
try:
    ECS_BOLD = "\\033[1;49m"
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 11816
try:
    ECS_HTTP_HEADER_COUNT_MAX = 32
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 11819
try:
    ECS_HTTP_QUERY_PARAM_COUNT_MAX = 32
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 12071
try:
    ECS_REST_DEFAULT_PORT = 27750
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 13068
try:
    ECS_STAT_WINDOW = 60
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 13750
try:
    ECS_ALERT_MAX_SEVERITY_FILTERS = 4
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 14867
try:
    FLECS_SCRIPT_FUNCTION_ARGS_MAX = 16
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 16040
try:
    ECS_MEMBER_DESC_CACHE_SIZE = 32
except:
    pass

# /Users/cnifi/git/pyflecs/flecs.h: 16547
try:
    ECS_META_MAX_SCOPE_DEPTH = 32
except:
    pass

ecs_allocator_t = struct_ecs_allocator_t  # /Users/cnifi/git/pyflecs/flecs.h: 2008

ecs_vec_t = struct_ecs_vec_t  # /Users/cnifi/git/pyflecs/flecs.h: 1166

ecs_block_allocator_t = (
    struct_ecs_block_allocator_t  # /Users/cnifi/git/pyflecs/flecs.h: 1615
)

ecs_sparse_t = struct_ecs_sparse_t  # /Users/cnifi/git/pyflecs/flecs.h: 1406

ecs_map_t = struct_ecs_map_t  # /Users/cnifi/git/pyflecs/flecs.h: 1830

ecs_block_allocator_block_t = (
    struct_ecs_block_allocator_block_t  # /Users/cnifi/git/pyflecs/flecs.h: 1606
)

ecs_block_allocator_chunk_header_t = (
    struct_ecs_block_allocator_chunk_header_t  # /Users/cnifi/git/pyflecs/flecs.h: 1611
)

ecs_stack_page_t = struct_ecs_stack_page_t  # /Users/cnifi/git/pyflecs/flecs.h: 1716

ecs_stack_cursor_t = struct_ecs_stack_cursor_t  # /Users/cnifi/git/pyflecs/flecs.h: 1723

ecs_stack_t = struct_ecs_stack_t  # /Users/cnifi/git/pyflecs/flecs.h: 1733

ecs_bucket_entry_t = struct_ecs_bucket_entry_t  # /Users/cnifi/git/pyflecs/flecs.h: 1820

ecs_bucket_t = struct_ecs_bucket_t  # /Users/cnifi/git/pyflecs/flecs.h: 1828

ecs_map_iter_t = struct_ecs_map_iter_t  # /Users/cnifi/git/pyflecs/flecs.h: 1843

ecs_map_params_t = struct_ecs_map_params_t  # /Users/cnifi/git/pyflecs/flecs.h: 1848

ecs_strbuf_list_elem = (
    struct_ecs_strbuf_list_elem  # /Users/cnifi/git/pyflecs/flecs.h: 2151
)

ecs_strbuf_t = struct_ecs_strbuf_t  # /Users/cnifi/git/pyflecs/flecs.h: 2162

ecs_time_t = struct_ecs_time_t  # /Users/cnifi/git/pyflecs/flecs.h: 2345

ecs_os_api_t = struct_ecs_os_api_t  # /Users/cnifi/git/pyflecs/flecs.h: 2641

ecs_world_t = struct_ecs_world_t  # /Users/cnifi/git/pyflecs/flecs.h: 3135

ecs_stage_t = struct_ecs_stage_t  # /Users/cnifi/git/pyflecs/flecs.h: 3138

ecs_table_t = struct_ecs_table_t  # /Users/cnifi/git/pyflecs/flecs.h: 3141

ecs_term_t = struct_ecs_term_t  # /Users/cnifi/git/pyflecs/flecs.h: 3498

ecs_query_t = struct_ecs_query_t  # /Users/cnifi/git/pyflecs/flecs.h: 3522

ecs_observer_t = struct_ecs_observer_t  # /Users/cnifi/git/pyflecs/flecs.h: 3563

ecs_observable_t = struct_ecs_observable_t  # /Users/cnifi/git/pyflecs/flecs.h: 3756

ecs_iter_t = struct_ecs_iter_t  # /Users/cnifi/git/pyflecs/flecs.h: 4870

ecs_ref_t = struct_ecs_ref_t  # /Users/cnifi/git/pyflecs/flecs.h: 3784

ecs_type_hooks_t = struct_ecs_type_hooks_t  # /Users/cnifi/git/pyflecs/flecs.h: 3635

ecs_type_info_t = struct_ecs_type_info_t  # /Users/cnifi/git/pyflecs/flecs.h: 3705

ecs_record_t = struct_ecs_record_t  # /Users/cnifi/git/pyflecs/flecs.h: 4411

ecs_component_record_t = (
    struct_ecs_component_record_t  # /Users/cnifi/git/pyflecs/flecs.h: 3203
)

ecs_mixins_t = struct_ecs_mixins_t  # /Users/cnifi/git/pyflecs/flecs.h: 3227

ecs_header_t = struct_ecs_header_t  # /Users/cnifi/git/pyflecs/flecs.h: 3234

ecs_table_record_t = struct_ecs_table_record_t  # /Users/cnifi/git/pyflecs/flecs.h: 4428

ecs_term_ref_t = struct_ecs_term_ref_t  # /Users/cnifi/git/pyflecs/flecs.h: 3495

ecs_data_t = struct_ecs_data_t  # /Users/cnifi/git/pyflecs/flecs.h: 3735

ecs_query_cache_match_t = (
    struct_ecs_query_cache_match_t  # /Users/cnifi/git/pyflecs/flecs.h: 3738
)

ecs_query_cache_group_t = (
    struct_ecs_query_cache_group_t  # /Users/cnifi/git/pyflecs/flecs.h: 3741
)

ecs_event_id_record_t = (
    struct_ecs_event_id_record_t  # /Users/cnifi/git/pyflecs/flecs.h: 3749
)

ecs_event_record_t = struct_ecs_event_record_t  # /Users/cnifi/git/pyflecs/flecs.h: 3754

ecs_table_range_t = struct_ecs_table_range_t  # /Users/cnifi/git/pyflecs/flecs.h: 3770

ecs_var_t = struct_ecs_var_t  # /Users/cnifi/git/pyflecs/flecs.h: 3781

ecs_page_iter_t = struct_ecs_page_iter_t  # /Users/cnifi/git/pyflecs/flecs.h: 3799

ecs_worker_iter_t = struct_ecs_worker_iter_t  # /Users/cnifi/git/pyflecs/flecs.h: 3805

ecs_table_cache_hdr_t = (
    struct_ecs_table_cache_hdr_t  # /Users/cnifi/git/pyflecs/flecs.h: 4419
)

ecs_table_cache_iter_t = (
    struct_ecs_table_cache_iter_t  # /Users/cnifi/git/pyflecs/flecs.h: 3812
)

ecs_each_iter_t = struct_ecs_each_iter_t  # /Users/cnifi/git/pyflecs/flecs.h: 3824

ecs_query_op_profile_t = (
    struct_ecs_query_op_profile_t  # /Users/cnifi/git/pyflecs/flecs.h: 3828
)

ecs_query_var_t = struct_ecs_query_var_t  # /Users/cnifi/git/pyflecs/flecs.h: 3833

ecs_query_op_t = struct_ecs_query_op_t  # /Users/cnifi/git/pyflecs/flecs.h: 3834

ecs_query_op_ctx_t = struct_ecs_query_op_ctx_t  # /Users/cnifi/git/pyflecs/flecs.h: 3835

ecs_query_iter_t = struct_ecs_query_iter_t  # /Users/cnifi/git/pyflecs/flecs.h: 3849

ecs_iter_private_t = struct_ecs_iter_private_t  # /Users/cnifi/git/pyflecs/flecs.h: 3863

ecs_commands_t = struct_ecs_commands_t  # /Users/cnifi/git/pyflecs/flecs.h: 3870

ecs_suspend_readonly_state_t = (
    struct_ecs_suspend_readonly_state_t  # /Users/cnifi/git/pyflecs/flecs.h: 4061
)

ecs_table_diff_t = struct_ecs_table_diff_t  # /Users/cnifi/git/pyflecs/flecs.h: 4442

ecs_table_records_t = (
    struct_ecs_table_records_t  # /Users/cnifi/git/pyflecs/flecs.h: 4681
)

ecs_value_t = struct_ecs_value_t  # /Users/cnifi/git/pyflecs/flecs.h: 4740

ecs_entity_desc_t = struct_ecs_entity_desc_t  # /Users/cnifi/git/pyflecs/flecs.h: 4786

ecs_bulk_desc_t = struct_ecs_bulk_desc_t  # /Users/cnifi/git/pyflecs/flecs.h: 4816

ecs_component_desc_t = (
    struct_ecs_component_desc_t  # /Users/cnifi/git/pyflecs/flecs.h: 4830
)

ecs_query_desc_t = struct_ecs_query_desc_t  # /Users/cnifi/git/pyflecs/flecs.h: 5047

ecs_observer_desc_t = (
    struct_ecs_observer_desc_t  # /Users/cnifi/git/pyflecs/flecs.h: 5109
)

ecs_event_desc_t = struct_ecs_event_desc_t  # /Users/cnifi/git/pyflecs/flecs.h: 5158

ecs_build_info_t = struct_ecs_build_info_t  # /Users/cnifi/git/pyflecs/flecs.h: 5179

ecs_world_info_t = struct_ecs_world_info_t  # /Users/cnifi/git/pyflecs/flecs.h: 5238

ecs_query_group_info_t = (
    struct_ecs_query_group_info_t  # /Users/cnifi/git/pyflecs/flecs.h: 5246
)

EcsIdentifier = struct_EcsIdentifier  # /Users/cnifi/git/pyflecs/flecs.h: 5264

EcsComponent = struct_EcsComponent  # /Users/cnifi/git/pyflecs/flecs.h: 5270

EcsPoly = struct_EcsPoly  # /Users/cnifi/git/pyflecs/flecs.h: 5275

EcsDefaultChildComponent = (
    struct_EcsDefaultChildComponent  # /Users/cnifi/git/pyflecs/flecs.h: 5284
)

ecs_entities_t = struct_ecs_entities_t  # /Users/cnifi/git/pyflecs/flecs.h: 5768

ecs_delete_empty_tables_desc_t = (
    struct_ecs_delete_empty_tables_desc_t  # /Users/cnifi/git/pyflecs/flecs.h: 6406
)

ecs_query_count_t = struct_ecs_query_count_t  # /Users/cnifi/git/pyflecs/flecs.h: 8744

ecs_app_desc_t = struct_ecs_app_desc_t  # /Users/cnifi/git/pyflecs/flecs.h: 11704

ecs_http_server_t = struct_ecs_http_server_t  # /Users/cnifi/git/pyflecs/flecs.h: 11826

EcsTimer = struct_EcsTimer  # /Users/cnifi/git/pyflecs/flecs.h: 12172

EcsRateFilter = struct_EcsRateFilter  # /Users/cnifi/git/pyflecs/flecs.h: 12180

ecs_pipeline_desc_t = (
    struct_ecs_pipeline_desc_t  # /Users/cnifi/git/pyflecs/flecs.h: 12523
)

EcsTickSource = struct_EcsTickSource  # /Users/cnifi/git/pyflecs/flecs.h: 12739

ecs_system_desc_t = struct_ecs_system_desc_t  # /Users/cnifi/git/pyflecs/flecs.h: 12802

ecs_system_t = struct_ecs_system_t  # /Users/cnifi/git/pyflecs/flecs.h: 12864

ecs_gauge_t = struct_ecs_gauge_t  # /Users/cnifi/git/pyflecs/flecs.h: 13075

ecs_counter_t = struct_ecs_counter_t  # /Users/cnifi/git/pyflecs/flecs.h: 13081

ecs_metric_t = union_ecs_metric_t  # /Users/cnifi/git/pyflecs/flecs.h: 13087

ecs_world_stats_t = struct_ecs_world_stats_t  # /Users/cnifi/git/pyflecs/flecs.h: 13195

ecs_query_stats_t = struct_ecs_query_stats_t  # /Users/cnifi/git/pyflecs/flecs.h: 13207

ecs_system_stats_t = (
    struct_ecs_system_stats_t  # /Users/cnifi/git/pyflecs/flecs.h: 13218
)

ecs_sync_stats_t = struct_ecs_sync_stats_t  # /Users/cnifi/git/pyflecs/flecs.h: 13230

ecs_pipeline_stats_t = (
    struct_ecs_pipeline_stats_t  # /Users/cnifi/git/pyflecs/flecs.h: 13250
)

EcsMetricValue = struct_EcsMetricValue  # /Users/cnifi/git/pyflecs/flecs.h: 13594

EcsMetricSource = struct_EcsMetricSource  # /Users/cnifi/git/pyflecs/flecs.h: 13599

ecs_metric_desc_t = struct_ecs_metric_desc_t  # /Users/cnifi/git/pyflecs/flecs.h: 13632

EcsAlertInstance = struct_EcsAlertInstance  # /Users/cnifi/git/pyflecs/flecs.h: 13771

EcsAlertsActive = struct_EcsAlertsActive  # /Users/cnifi/git/pyflecs/flecs.h: 13779

ecs_alert_severity_filter_t = (
    struct_ecs_alert_severity_filter_t  # /Users/cnifi/git/pyflecs/flecs.h: 13793
)

ecs_alert_desc_t = struct_ecs_alert_desc_t  # /Users/cnifi/git/pyflecs/flecs.h: 13852

ecs_from_json_desc_t = (
    struct_ecs_from_json_desc_t  # /Users/cnifi/git/pyflecs/flecs.h: 14005
)

ecs_entity_to_json_desc_t = (
    struct_ecs_entity_to_json_desc_t  # /Users/cnifi/git/pyflecs/flecs.h: 14180
)

ecs_iter_to_json_desc_t = (
    struct_ecs_iter_to_json_desc_t  # /Users/cnifi/git/pyflecs/flecs.h: 14262
)

ecs_world_to_json_desc_t = (
    struct_ecs_world_to_json_desc_t  # /Users/cnifi/git/pyflecs/flecs.h: 14338
)

ecs_script_template_t = (
    struct_ecs_script_template_t  # /Users/cnifi/git/pyflecs/flecs.h: 14885
)

ecs_script_var_t = struct_ecs_script_var_t  # /Users/cnifi/git/pyflecs/flecs.h: 14894

ecs_script_vars_t = struct_ecs_script_vars_t  # /Users/cnifi/git/pyflecs/flecs.h: 14897

ecs_script_t = struct_ecs_script_t  # /Users/cnifi/git/pyflecs/flecs.h: 14915

ecs_script_runtime_t = (
    struct_ecs_script_runtime_t  # /Users/cnifi/git/pyflecs/flecs.h: 14918
)

EcsScript = struct_EcsScript  # /Users/cnifi/git/pyflecs/flecs.h: 14926

ecs_function_ctx_t = (
    struct_ecs_function_ctx_t  # /Users/cnifi/git/pyflecs/flecs.h: 14933
)

ecs_script_parameter_t = (
    struct_ecs_script_parameter_t  # /Users/cnifi/git/pyflecs/flecs.h: 14946
)

EcsScriptConstVar = struct_EcsScriptConstVar  # /Users/cnifi/git/pyflecs/flecs.h: 14954

EcsScriptFunction = struct_EcsScriptFunction  # /Users/cnifi/git/pyflecs/flecs.h: 14964

EcsScriptMethod = struct_EcsScriptMethod  # /Users/cnifi/git/pyflecs/flecs.h: 14976

ecs_script_eval_desc_t = (
    struct_ecs_script_eval_desc_t  # /Users/cnifi/git/pyflecs/flecs.h: 14984
)

ecs_script_desc_t = struct_ecs_script_desc_t  # /Users/cnifi/git/pyflecs/flecs.h: 15130

ecs_expr_eval_desc_t = (
    struct_ecs_expr_eval_desc_t  # /Users/cnifi/git/pyflecs/flecs.h: 15396
)

ecs_const_var_desc_t = (
    struct_ecs_const_var_desc_t  # /Users/cnifi/git/pyflecs/flecs.h: 15489
)

ecs_function_desc_t = (
    struct_ecs_function_desc_t  # /Users/cnifi/git/pyflecs/flecs.h: 15527
)

ecs_expr_node_t = struct_ecs_expr_node_t  # /Users/cnifi/git/pyflecs/flecs.h: 15631

EcsDocDescription = struct_EcsDocDescription  # /Users/cnifi/git/pyflecs/flecs.h: 15729

EcsType = struct_EcsType  # /Users/cnifi/git/pyflecs/flecs.h: 16127

EcsPrimitive = struct_EcsPrimitive  # /Users/cnifi/git/pyflecs/flecs.h: 16155

EcsMember = struct_EcsMember  # /Users/cnifi/git/pyflecs/flecs.h: 16164

ecs_member_value_range_t = (
    struct_ecs_member_value_range_t  # /Users/cnifi/git/pyflecs/flecs.h: 16170
)

EcsMemberRanges = struct_EcsMemberRanges  # /Users/cnifi/git/pyflecs/flecs.h: 16177

ecs_member_t = struct_ecs_member_t  # /Users/cnifi/git/pyflecs/flecs.h: 16220

EcsStruct = struct_EcsStruct  # /Users/cnifi/git/pyflecs/flecs.h: 16226

ecs_enum_constant_t = (
    struct_ecs_enum_constant_t  # /Users/cnifi/git/pyflecs/flecs.h: 16241
)

EcsEnum = struct_EcsEnum  # /Users/cnifi/git/pyflecs/flecs.h: 16252

ecs_bitmask_constant_t = (
    struct_ecs_bitmask_constant_t  # /Users/cnifi/git/pyflecs/flecs.h: 16267
)

EcsBitmask = struct_EcsBitmask  # /Users/cnifi/git/pyflecs/flecs.h: 16275

EcsArray = struct_EcsArray  # /Users/cnifi/git/pyflecs/flecs.h: 16281

EcsVector = struct_EcsVector  # /Users/cnifi/git/pyflecs/flecs.h: 16286

ecs_serializer_t = struct_ecs_serializer_t  # /Users/cnifi/git/pyflecs/flecs.h: 16294

EcsOpaque = struct_EcsOpaque  # /Users/cnifi/git/pyflecs/flecs.h: 16446

ecs_unit_translation_t = (
    struct_ecs_unit_translation_t  # /Users/cnifi/git/pyflecs/flecs.h: 16461
)

EcsUnit = struct_EcsUnit  # /Users/cnifi/git/pyflecs/flecs.h: 16470

EcsUnitPrefix = struct_EcsUnitPrefix  # /Users/cnifi/git/pyflecs/flecs.h: 16476

ecs_meta_type_op_t = (
    struct_ecs_meta_type_op_t  # /Users/cnifi/git/pyflecs/flecs.h: 16532
)

EcsTypeSerializer = struct_EcsTypeSerializer  # /Users/cnifi/git/pyflecs/flecs.h: 16539

ecs_meta_scope_t = struct_ecs_meta_scope_t  # /Users/cnifi/git/pyflecs/flecs.h: 16565

ecs_meta_cursor_t = struct_ecs_meta_cursor_t  # /Users/cnifi/git/pyflecs/flecs.h: 16578

ecs_primitive_desc_t = (
    struct_ecs_primitive_desc_t  # /Users/cnifi/git/pyflecs/flecs.h: 16931
)

ecs_enum_desc_t = struct_ecs_enum_desc_t  # /Users/cnifi/git/pyflecs/flecs.h: 16950

ecs_bitmask_desc_t = (
    struct_ecs_bitmask_desc_t  # /Users/cnifi/git/pyflecs/flecs.h: 16968
)

ecs_array_desc_t = struct_ecs_array_desc_t  # /Users/cnifi/git/pyflecs/flecs.h: 16987

ecs_vector_desc_t = struct_ecs_vector_desc_t  # /Users/cnifi/git/pyflecs/flecs.h: 17005

ecs_struct_desc_t = struct_ecs_struct_desc_t  # /Users/cnifi/git/pyflecs/flecs.h: 17023

ecs_opaque_desc_t = struct_ecs_opaque_desc_t  # /Users/cnifi/git/pyflecs/flecs.h: 17041

ecs_unit_desc_t = struct_ecs_unit_desc_t  # /Users/cnifi/git/pyflecs/flecs.h: 17098

ecs_unit_prefix_desc_t = (
    struct_ecs_unit_prefix_desc_t  # /Users/cnifi/git/pyflecs/flecs.h: 17122
)

ecs_cpp_get_mut_t = struct_ecs_cpp_get_mut_t  # /Users/cnifi/git/pyflecs/flecs.h: 17649

# No inserted files

# No prefix-stripping
