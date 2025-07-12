r"""Wrapper for flecs.h

Generated with:
.venv/bin/ctypesgen -llibflecs.dylib ../flecs/include/flecs.h -o flecs.py

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
          yield os.path.abspath(os.path.join(os.path.dirname(__file__), fmt % libname))

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

uint16_t = c_ushort  # /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/_types/_uint16_t.h: 31

uint32_t = c_uint  # /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/_types/_uint32_t.h: 31

uint64_t = c_ulonglong  # /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/_types/_uint64_t.h: 31

ecs_flags16_t = (
  uint16_t  # /Users/eherz/git/flecs/include/flecs/private/api_defines.h: 223
)

ecs_flags32_t = (
  uint32_t  # /Users/eherz/git/flecs/include/flecs/private/api_defines.h: 224
)

ecs_flags64_t = (
  uint64_t  # /Users/eherz/git/flecs/include/flecs/private/api_defines.h: 225
)

ecs_size_t = c_int32  # /Users/eherz/git/flecs/include/flecs/private/api_defines.h: 242


# /Users/eherz/git/flecs/include/flecs/datastructures/allocator.h: 16
class struct_ecs_allocator_t(Structure):
  pass


# /Users/eherz/git/flecs/include/flecs/datastructures/vec.h: 24
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

ecs_vec_t = (
  struct_ecs_vec_t  # /Users/eherz/git/flecs/include/flecs/datastructures/vec.h: 24
)


# /Users/eherz/git/flecs/include/flecs/datastructures/block_allocator.h: 22
class struct_ecs_block_allocator_t(Structure):
  pass


# /Users/eherz/git/flecs/include/flecs/datastructures/sparse.h: 36
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

ecs_sparse_t = struct_ecs_sparse_t  # /Users/eherz/git/flecs/include/flecs/datastructures/sparse.h: 36


# /Users/eherz/git/flecs/include/flecs/datastructures/map.h: 30
class struct_ecs_map_t(Structure):
  pass


ecs_map_t = struct_ecs_map_t  # /Users/eherz/git/flecs/include/flecs/datastructures/block_allocator.h: 11


# /Users/eherz/git/flecs/include/flecs/datastructures/block_allocator.h: 13
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

ecs_block_allocator_block_t = struct_ecs_block_allocator_block_t  # /Users/eherz/git/flecs/include/flecs/datastructures/block_allocator.h: 16


# /Users/eherz/git/flecs/include/flecs/datastructures/block_allocator.h: 18
class struct_ecs_block_allocator_chunk_header_t(Structure):
  pass


struct_ecs_block_allocator_chunk_header_t.__slots__ = [
  "next",
]
struct_ecs_block_allocator_chunk_header_t._fields_ = [
  ("next", POINTER(struct_ecs_block_allocator_chunk_header_t)),
]

ecs_block_allocator_chunk_header_t = struct_ecs_block_allocator_chunk_header_t  # /Users/eherz/git/flecs/include/flecs/datastructures/block_allocator.h: 20

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

ecs_block_allocator_t = struct_ecs_block_allocator_t  # /Users/eherz/git/flecs/include/flecs/datastructures/block_allocator.h: 35


# /Users/eherz/git/flecs/include/flecs/datastructures/stack_allocator.h: 11
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

ecs_stack_page_t = struct_ecs_stack_page_t  # /Users/eherz/git/flecs/include/flecs/datastructures/stack_allocator.h: 16


# /Users/eherz/git/flecs/include/flecs/datastructures/stack_allocator.h: 18
class struct_ecs_stack_cursor_t(Structure):
  pass


# /Users/eherz/git/flecs/include/flecs/datastructures/stack_allocator.h: 28
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

ecs_stack_cursor_t = struct_ecs_stack_cursor_t  # /Users/eherz/git/flecs/include/flecs/datastructures/stack_allocator.h: 26

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

ecs_map_data_t = (
  uint64_t  # /Users/eherz/git/flecs/include/flecs/datastructures/map.h: 15
)

ecs_map_key_t = (
  ecs_map_data_t  # /Users/eherz/git/flecs/include/flecs/datastructures/map.h: 16
)

ecs_map_val_t = (
  ecs_map_data_t  # /Users/eherz/git/flecs/include/flecs/datastructures/map.h: 17
)


# /Users/eherz/git/flecs/include/flecs/datastructures/map.h: 20
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

ecs_bucket_entry_t = struct_ecs_bucket_entry_t  # /Users/eherz/git/flecs/include/flecs/datastructures/map.h: 24


# /Users/eherz/git/flecs/include/flecs/datastructures/map.h: 28
class struct_ecs_bucket_t(Structure):
  pass


struct_ecs_bucket_t.__slots__ = [
  "first",
]
struct_ecs_bucket_t._fields_ = [
  ("first", POINTER(ecs_bucket_entry_t)),
]

ecs_bucket_t = (
  struct_ecs_bucket_t  # /Users/eherz/git/flecs/include/flecs/datastructures/map.h: 28
)

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

struct_ecs_allocator_t.__slots__ = [
  "chunks",
  "sizes",
]
struct_ecs_allocator_t._fields_ = [
  ("chunks", ecs_block_allocator_t),
  ("sizes", struct_ecs_sparse_t),
]


# /Users/eherz/git/flecs/include/flecs/datastructures/strbuf.h: 28
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

ecs_strbuf_list_elem = struct_ecs_strbuf_list_elem  # /Users/eherz/git/flecs/include/flecs/datastructures/strbuf.h: 28


# /Users/eherz/git/flecs/include/flecs/datastructures/strbuf.h: 39
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

ecs_strbuf_t = struct_ecs_strbuf_t  # /Users/eherz/git/flecs/include/flecs/datastructures/strbuf.h: 39

ecs_id_t = uint64_t  # /Users/eherz/git/flecs/include/flecs.h: 377

ecs_entity_t = ecs_id_t  # /Users/eherz/git/flecs/include/flecs.h: 384


# /Users/eherz/git/flecs/include/flecs.h: 404
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

ecs_type_t = struct_anon_8  # /Users/eherz/git/flecs/include/flecs.h: 404


# /Users/eherz/git/flecs/include/flecs.h: 428
class struct_ecs_world_t(Structure):
  pass


ecs_world_t = struct_ecs_world_t  # /Users/eherz/git/flecs/include/flecs.h: 428


# /Users/eherz/git/flecs/include/flecs.h: 431
class struct_ecs_stage_t(Structure):
  pass


ecs_stage_t = struct_ecs_stage_t  # /Users/eherz/git/flecs/include/flecs.h: 431


# /Users/eherz/git/flecs/include/flecs.h: 434
class struct_ecs_table_t(Structure):
  pass


ecs_table_t = struct_ecs_table_t  # /Users/eherz/git/flecs/include/flecs.h: 434


# /Users/eherz/git/flecs/include/flecs.h: 791
class struct_ecs_term_t(Structure):
  pass


ecs_term_t = struct_ecs_term_t  # /Users/eherz/git/flecs/include/flecs.h: 437


# /Users/eherz/git/flecs/include/flecs.h: 815
class struct_ecs_query_t(Structure):
  pass


ecs_query_t = struct_ecs_query_t  # /Users/eherz/git/flecs/include/flecs.h: 440


# /Users/eherz/git/flecs/include/flecs.h: 856
class struct_ecs_observer_t(Structure):
  pass


ecs_observer_t = struct_ecs_observer_t  # /Users/eherz/git/flecs/include/flecs.h: 459


# /Users/eherz/git/flecs/include/flecs/private/api_types.h: 45
class struct_ecs_observable_t(Structure):
  pass


ecs_observable_t = (
  struct_ecs_observable_t  # /Users/eherz/git/flecs/include/flecs.h: 464
)


# /Users/eherz/git/flecs/include/flecs.h: 1145
class struct_ecs_iter_t(Structure):
  pass


ecs_iter_t = struct_ecs_iter_t  # /Users/eherz/git/flecs/include/flecs.h: 470


# /Users/eherz/git/flecs/include/flecs/private/api_types.h: 73
class struct_ecs_ref_t(Structure):
  pass


ecs_ref_t = struct_ecs_ref_t  # /Users/eherz/git/flecs/include/flecs.h: 480


# /Users/eherz/git/flecs/include/flecs.h: 928
class struct_ecs_type_hooks_t(Structure):
  pass


ecs_type_hooks_t = (
  struct_ecs_type_hooks_t  # /Users/eherz/git/flecs/include/flecs.h: 485
)


# /Users/eherz/git/flecs/include/flecs.h: 998
class struct_ecs_type_info_t(Structure):
  pass


ecs_type_info_t = struct_ecs_type_info_t  # /Users/eherz/git/flecs/include/flecs.h: 490


# /Users/eherz/git/flecs/include/flecs/private/api_internals.h: 18
class struct_ecs_record_t(Structure):
  pass


ecs_record_t = struct_ecs_record_t  # /Users/eherz/git/flecs/include/flecs.h: 493


# /Users/eherz/git/flecs/include/flecs.h: 496
class struct_ecs_component_record_t(Structure):
  pass


ecs_component_record_t = (
  struct_ecs_component_record_t  # /Users/eherz/git/flecs/include/flecs.h: 496
)

ecs_poly_t = None  # /Users/eherz/git/flecs/include/flecs.h: 517


# /Users/eherz/git/flecs/include/flecs.h: 520
class struct_ecs_mixins_t(Structure):
  pass


ecs_mixins_t = struct_ecs_mixins_t  # /Users/eherz/git/flecs/include/flecs.h: 520


# /Users/eherz/git/flecs/include/flecs.h: 527
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

ecs_header_t = struct_ecs_header_t  # /Users/eherz/git/flecs/include/flecs.h: 527


# /Users/eherz/git/flecs/include/flecs/private/api_internals.h: 35
class struct_ecs_table_record_t(Structure):
  pass


ecs_table_record_t = (
  struct_ecs_table_record_t  # /Users/eherz/git/flecs/include/flecs.h: 529
)

ecs_run_action_t = CFUNCTYPE(
  UNCHECKED(None), POINTER(ecs_iter_t)
)  # /Users/eherz/git/flecs/include/flecs.h: 549

ecs_iter_action_t = CFUNCTYPE(
  UNCHECKED(None), POINTER(ecs_iter_t)
)  # /Users/eherz/git/flecs/include/flecs.h: 558

ecs_iter_next_action_t = CFUNCTYPE(
  UNCHECKED(c_bool), POINTER(ecs_iter_t)
)  # /Users/eherz/git/flecs/include/flecs.h: 568

ecs_iter_fini_action_t = CFUNCTYPE(
  UNCHECKED(None), POINTER(ecs_iter_t)
)  # /Users/eherz/git/flecs/include/flecs.h: 576

ecs_order_by_action_t = CFUNCTYPE(
  UNCHECKED(c_int), ecs_entity_t, POINTER(None), ecs_entity_t, POINTER(None)
)  # /Users/eherz/git/flecs/include/flecs.h: 580

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
)  # /Users/eherz/git/flecs/include/flecs.h: 587

ecs_group_by_action_t = CFUNCTYPE(
  UNCHECKED(uint64_t),
  POINTER(ecs_world_t),
  POINTER(ecs_table_t),
  ecs_id_t,
  POINTER(None),
)  # /Users/eherz/git/flecs/include/flecs.h: 598

ecs_group_create_action_t = CFUNCTYPE(
  UNCHECKED(POINTER(c_ubyte)), POINTER(ecs_world_t), uint64_t, POINTER(None)
)  # /Users/eherz/git/flecs/include/flecs.h: 605

ecs_group_delete_action_t = CFUNCTYPE(
  UNCHECKED(None), POINTER(ecs_world_t), uint64_t, POINTER(None), POINTER(None)
)  # /Users/eherz/git/flecs/include/flecs.h: 611

ecs_module_action_t = CFUNCTYPE(
  UNCHECKED(None), POINTER(ecs_world_t)
)  # /Users/eherz/git/flecs/include/flecs.h: 618

ecs_fini_action_t = CFUNCTYPE(
  UNCHECKED(None), POINTER(ecs_world_t), POINTER(None)
)  # /Users/eherz/git/flecs/include/flecs.h: 622

ecs_ctx_free_t = CFUNCTYPE(
  UNCHECKED(None), POINTER(None)
)  # /Users/eherz/git/flecs/include/flecs.h: 627

ecs_compare_action_t = CFUNCTYPE(
  UNCHECKED(c_int), POINTER(None), POINTER(None)
)  # /Users/eherz/git/flecs/include/flecs.h: 631

ecs_hash_value_action_t = CFUNCTYPE(
  UNCHECKED(uint64_t), POINTER(None)
)  # /Users/eherz/git/flecs/include/flecs.h: 636

ecs_xtor_t = CFUNCTYPE(
  UNCHECKED(None), POINTER(None), c_int32, POINTER(ecs_type_info_t)
)  # /Users/eherz/git/flecs/include/flecs.h: 640

ecs_copy_t = CFUNCTYPE(
  UNCHECKED(None), POINTER(None), POINTER(None), c_int32, POINTER(ecs_type_info_t)
)  # /Users/eherz/git/flecs/include/flecs.h: 646

ecs_move_t = CFUNCTYPE(
  UNCHECKED(None), POINTER(None), POINTER(None), c_int32, POINTER(ecs_type_info_t)
)  # /Users/eherz/git/flecs/include/flecs.h: 653

ecs_cmp_t = CFUNCTYPE(
  UNCHECKED(c_int), POINTER(None), POINTER(None), POINTER(ecs_type_info_t)
)  # /Users/eherz/git/flecs/include/flecs.h: 660

ecs_equals_t = CFUNCTYPE(
  UNCHECKED(c_bool), POINTER(None), POINTER(None), POINTER(ecs_type_info_t)
)  # /Users/eherz/git/flecs/include/flecs.h: 666

flecs_poly_dtor_t = CFUNCTYPE(
  UNCHECKED(None), POINTER(ecs_poly_t)
)  # /Users/eherz/git/flecs/include/flecs.h: 672

enum_ecs_inout_kind_t = c_int  # /Users/eherz/git/flecs/include/flecs.h: 692

EcsInOutDefault = 0  # /Users/eherz/git/flecs/include/flecs.h: 692

EcsInOutNone = EcsInOutDefault + 1  # /Users/eherz/git/flecs/include/flecs.h: 692

EcsInOutFilter = EcsInOutNone + 1  # /Users/eherz/git/flecs/include/flecs.h: 692

EcsInOut = EcsInOutFilter + 1  # /Users/eherz/git/flecs/include/flecs.h: 692

EcsIn = EcsInOut + 1  # /Users/eherz/git/flecs/include/flecs.h: 692

EcsOut = EcsIn + 1  # /Users/eherz/git/flecs/include/flecs.h: 692

ecs_inout_kind_t = enum_ecs_inout_kind_t  # /Users/eherz/git/flecs/include/flecs.h: 692

enum_ecs_oper_kind_t = c_int  # /Users/eherz/git/flecs/include/flecs.h: 703

EcsAnd = 0  # /Users/eherz/git/flecs/include/flecs.h: 703

EcsOr = EcsAnd + 1  # /Users/eherz/git/flecs/include/flecs.h: 703

EcsNot = EcsOr + 1  # /Users/eherz/git/flecs/include/flecs.h: 703

EcsOptional = EcsNot + 1  # /Users/eherz/git/flecs/include/flecs.h: 703

EcsAndFrom = EcsOptional + 1  # /Users/eherz/git/flecs/include/flecs.h: 703

EcsOrFrom = EcsAndFrom + 1  # /Users/eherz/git/flecs/include/flecs.h: 703

EcsNotFrom = EcsOrFrom + 1  # /Users/eherz/git/flecs/include/flecs.h: 703

ecs_oper_kind_t = enum_ecs_oper_kind_t  # /Users/eherz/git/flecs/include/flecs.h: 703

enum_ecs_query_cache_kind_t = c_int  # /Users/eherz/git/flecs/include/flecs.h: 711

EcsQueryCacheDefault = 0  # /Users/eherz/git/flecs/include/flecs.h: 711

EcsQueryCacheAuto = (
  EcsQueryCacheDefault + 1
)  # /Users/eherz/git/flecs/include/flecs.h: 711

EcsQueryCacheAll = EcsQueryCacheAuto + 1  # /Users/eherz/git/flecs/include/flecs.h: 711

EcsQueryCacheNone = EcsQueryCacheAll + 1  # /Users/eherz/git/flecs/include/flecs.h: 711

ecs_query_cache_kind_t = (
  enum_ecs_query_cache_kind_t  # /Users/eherz/git/flecs/include/flecs.h: 711
)


# /Users/eherz/git/flecs/include/flecs.h: 788
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

ecs_term_ref_t = struct_ecs_term_ref_t  # /Users/eherz/git/flecs/include/flecs.h: 788

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


# /Users/eherz/git/flecs/include/flecs/private/api_types.h: 27
class struct_ecs_query_cache_match_t(Structure):
  pass


ecs_query_cache_match_t = struct_ecs_query_cache_match_t  # /Users/eherz/git/flecs/include/flecs/private/api_types.h: 27


# /Users/eherz/git/flecs/include/flecs/private/api_types.h: 30
class struct_ecs_query_cache_group_t(Structure):
  pass


ecs_query_cache_group_t = struct_ecs_query_cache_group_t  # /Users/eherz/git/flecs/include/flecs/private/api_types.h: 30


# /Users/eherz/git/flecs/include/flecs/private/api_types.h: 38
class struct_ecs_event_id_record_t(Structure):
  pass


# /Users/eherz/git/flecs/include/flecs/private/api_types.h: 43
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

ecs_event_record_t = struct_ecs_event_record_t  # /Users/eherz/git/flecs/include/flecs/private/api_types.h: 43

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


# /Users/eherz/git/flecs/include/flecs/private/api_types.h: 59
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

ecs_table_range_t = struct_ecs_table_range_t  # /Users/eherz/git/flecs/include/flecs/private/api_types.h: 59


# /Users/eherz/git/flecs/include/flecs/private/api_types.h: 70
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

ecs_var_t = (
  struct_ecs_var_t  # /Users/eherz/git/flecs/include/flecs/private/api_types.h: 70
)

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


# /Users/eherz/git/flecs/include/flecs/private/api_types.h: 88
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

ecs_page_iter_t = (
  struct_ecs_page_iter_t  # /Users/eherz/git/flecs/include/flecs/private/api_types.h: 88
)


# /Users/eherz/git/flecs/include/flecs/private/api_types.h: 94
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

ecs_worker_iter_t = struct_ecs_worker_iter_t  # /Users/eherz/git/flecs/include/flecs/private/api_types.h: 94


# /Users/eherz/git/flecs/include/flecs/private/api_internals.h: 26
class struct_ecs_table_cache_hdr_t(Structure):
  pass


# /Users/eherz/git/flecs/include/flecs/private/api_types.h: 101
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

ecs_table_cache_iter_t = struct_ecs_table_cache_iter_t  # /Users/eherz/git/flecs/include/flecs/private/api_types.h: 101


# /Users/eherz/git/flecs/include/flecs/private/api_types.h: 113
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

ecs_each_iter_t = struct_ecs_each_iter_t  # /Users/eherz/git/flecs/include/flecs/private/api_types.h: 113


# /Users/eherz/git/flecs/include/flecs/private/api_types.h: 117
class struct_ecs_query_op_profile_t(Structure):
  pass


struct_ecs_query_op_profile_t.__slots__ = [
  "count",
]
struct_ecs_query_op_profile_t._fields_ = [
  ("count", c_int32 * int(2)),
]

ecs_query_op_profile_t = struct_ecs_query_op_profile_t  # /Users/eherz/git/flecs/include/flecs/private/api_types.h: 117


# /Users/eherz/git/flecs/include/flecs/private/api_types.h: 122
class struct_ecs_query_var_t(Structure):
  pass


# /Users/eherz/git/flecs/include/flecs/private/api_types.h: 123
class struct_ecs_query_op_t(Structure):
  pass


# /Users/eherz/git/flecs/include/flecs/private/api_types.h: 124
class struct_ecs_query_op_ctx_t(Structure):
  pass


# /Users/eherz/git/flecs/include/flecs/private/api_types.h: 138
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

ecs_query_iter_t = struct_ecs_query_iter_t  # /Users/eherz/git/flecs/include/flecs/private/api_types.h: 138


# /Users/eherz/git/flecs/include/flecs/private/api_types.h: 143
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


# /Users/eherz/git/flecs/include/flecs/private/api_types.h: 152
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

ecs_iter_private_t = struct_ecs_iter_private_t  # /Users/eherz/git/flecs/include/flecs/private/api_types.h: 152


# /Users/eherz/git/flecs/include/flecs/datastructures/hashmap.h: 28
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

ecs_hashmap_t = (
  struct_anon_11  # /Users/eherz/git/flecs/include/flecs/datastructures/hashmap.h: 28
)

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


# /Users/eherz/git/flecs/include/flecs/private/api_internals.h: 27
class struct_ecs_table_cache_t(Structure):
  pass


struct_ecs_table_cache_hdr_t.__slots__ = [
  "cache",
  "table",
  "prev",
  "next",
]
struct_ecs_table_cache_hdr_t._fields_ = [
  ("cache", POINTER(struct_ecs_table_cache_t)),
  ("table", POINTER(ecs_table_t)),
  ("prev", POINTER(struct_ecs_table_cache_hdr_t)),
  ("next", POINTER(struct_ecs_table_cache_hdr_t)),
]

ecs_table_cache_hdr_t = struct_ecs_table_cache_hdr_t  # /Users/eherz/git/flecs/include/flecs/private/api_internals.h: 30

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


# /Users/eherz/git/flecs/include/flecs.h: 1015
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

ecs_value_t = struct_ecs_value_t  # /Users/eherz/git/flecs/include/flecs.h: 1015


# /Users/eherz/git/flecs/include/flecs.h: 1061
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

ecs_entity_desc_t = (
  struct_ecs_entity_desc_t  # /Users/eherz/git/flecs/include/flecs.h: 1061
)


# /Users/eherz/git/flecs/include/flecs.h: 1091
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

ecs_bulk_desc_t = struct_ecs_bulk_desc_t  # /Users/eherz/git/flecs/include/flecs.h: 1091


# /Users/eherz/git/flecs/include/flecs.h: 1105
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
  struct_ecs_component_desc_t  # /Users/eherz/git/flecs/include/flecs.h: 1105
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


# /Users/eherz/git/flecs/include/flecs.h: 1322
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

ecs_query_desc_t = (
  struct_ecs_query_desc_t  # /Users/eherz/git/flecs/include/flecs.h: 1322
)


# /Users/eherz/git/flecs/include/flecs.h: 1384
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
  struct_ecs_observer_desc_t  # /Users/eherz/git/flecs/include/flecs.h: 1384
)


# /Users/eherz/git/flecs/include/flecs.h: 1433
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

ecs_event_desc_t = (
  struct_ecs_event_desc_t  # /Users/eherz/git/flecs/include/flecs.h: 1433
)


# /Users/eherz/git/flecs/include/flecs.h: 1454
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

ecs_build_info_t = (
  struct_ecs_build_info_t  # /Users/eherz/git/flecs/include/flecs.h: 1454
)


# /Users/eherz/git/flecs/include/flecs.h: 1494
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


# /Users/eherz/git/flecs/include/flecs.h: 1513
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

ecs_world_info_t = (
  struct_ecs_world_info_t  # /Users/eherz/git/flecs/include/flecs.h: 1513
)


# /Users/eherz/git/flecs/include/flecs.h: 1521
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
  struct_ecs_query_group_info_t  # /Users/eherz/git/flecs/include/flecs.h: 1521
)


# /Users/eherz/git/flecs/include/flecs.h: 1539
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

EcsIdentifier = struct_EcsIdentifier  # /Users/eherz/git/flecs/include/flecs.h: 1539


# /Users/eherz/git/flecs/include/flecs.h: 1545
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

EcsComponent = struct_EcsComponent  # /Users/eherz/git/flecs/include/flecs.h: 1545


# /Users/eherz/git/flecs/include/flecs.h: 1550
class struct_EcsPoly(Structure):
  pass


struct_EcsPoly.__slots__ = [
  "poly",
]
struct_EcsPoly._fields_ = [
  ("poly", POINTER(ecs_poly_t)),
]

EcsPoly = struct_EcsPoly  # /Users/eherz/git/flecs/include/flecs.h: 1550


# /Users/eherz/git/flecs/include/flecs.h: 1559
class struct_EcsDefaultChildComponent(Structure):
  pass


struct_EcsDefaultChildComponent.__slots__ = [
  "component",
]
struct_EcsDefaultChildComponent._fields_ = [
  ("component", ecs_id_t),
]

EcsDefaultChildComponent = (
  struct_EcsDefaultChildComponent  # /Users/eherz/git/flecs/include/flecs.h: 1559
)

# /Users/eherz/git/flecs/include/flecs.h: 1584
try:
  ECS_PAIR = (ecs_id_t).in_dll(_libs["libflecs.dylib"], "ECS_PAIR")
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1587
try:
  ECS_AUTO_OVERRIDE = (ecs_id_t).in_dll(_libs["libflecs.dylib"], "ECS_AUTO_OVERRIDE")
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1590
try:
  ECS_TOGGLE = (ecs_id_t).in_dll(_libs["libflecs.dylib"], "ECS_TOGGLE")
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1602
try:
  FLECS_IDEcsComponentID_ = (ecs_entity_t).in_dll(
    _libs["libflecs.dylib"], "FLECS_IDEcsComponentID_"
  )
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1605
try:
  FLECS_IDEcsIdentifierID_ = (ecs_entity_t).in_dll(
    _libs["libflecs.dylib"], "FLECS_IDEcsIdentifierID_"
  )
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1608
try:
  FLECS_IDEcsPolyID_ = (ecs_entity_t).in_dll(
    _libs["libflecs.dylib"], "FLECS_IDEcsPolyID_"
  )
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1611
try:
  FLECS_IDEcsDefaultChildComponentID_ = (ecs_entity_t).in_dll(
    _libs["libflecs.dylib"], "FLECS_IDEcsDefaultChildComponentID_"
  )
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1614
try:
  EcsQuery = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsQuery")
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1617
try:
  EcsObserver = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsObserver")
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1620
try:
  EcsSystem = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsSystem")
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1623
try:
  FLECS_IDEcsTickSourceID_ = (ecs_entity_t).in_dll(
    _libs["libflecs.dylib"], "FLECS_IDEcsTickSourceID_"
  )
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1626
for _lib in _libs.values():
  try:
    FLECS_IDEcsPipelineQueryID_ = (ecs_entity_t).in_dll(
      _lib, "FLECS_IDEcsPipelineQueryID_"
    )
    break
  except:
    pass

# /Users/eherz/git/flecs/include/flecs.h: 1629
try:
  FLECS_IDEcsTimerID_ = (ecs_entity_t).in_dll(
    _libs["libflecs.dylib"], "FLECS_IDEcsTimerID_"
  )
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1632
try:
  FLECS_IDEcsRateFilterID_ = (ecs_entity_t).in_dll(
    _libs["libflecs.dylib"], "FLECS_IDEcsRateFilterID_"
  )
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1635
try:
  EcsFlecs = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsFlecs")
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1638
try:
  EcsFlecsCore = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsFlecsCore")
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1641
try:
  EcsWorld = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsWorld")
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1644
try:
  EcsWildcard = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsWildcard")
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1647
try:
  EcsAny = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsAny")
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1650
try:
  EcsThis = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsThis")
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1653
try:
  EcsVariable = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsVariable")
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1665
try:
  EcsTransitive = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsTransitive")
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1674
try:
  EcsReflexive = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsReflexive")
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1686
try:
  EcsFinal = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsFinal")
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1693
try:
  EcsInheritable = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsInheritable")
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1696
try:
  EcsOnInstantiate = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsOnInstantiate")
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1701
try:
  EcsOverride = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsOverride")
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1706
try:
  EcsInherit = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsInherit")
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1712
try:
  EcsDontInherit = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsDontInherit")
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1721
try:
  EcsSymmetric = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsSymmetric")
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1732
try:
  EcsExclusive = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsExclusive")
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1735
try:
  EcsAcyclic = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsAcyclic")
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1739
try:
  EcsTraversable = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsTraversable")
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1750
try:
  EcsWith = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsWith")
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1761
try:
  EcsOneOf = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsOneOf")
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1764
try:
  EcsCanToggle = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsCanToggle")
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1769
try:
  EcsTrait = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsTrait")
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1780
try:
  EcsRelationship = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsRelationship")
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1791
try:
  EcsTarget = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsTarget")
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1795
try:
  EcsPairIsTag = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsPairIsTag")
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1798
try:
  EcsName = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsName")
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1801
try:
  EcsSymbol = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsSymbol")
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1804
try:
  EcsAlias = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsAlias")
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1807
try:
  EcsChildOf = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsChildOf")
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1810
try:
  EcsIsA = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsIsA")
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1813
try:
  EcsDependsOn = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsDependsOn")
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1816
try:
  EcsSlotOf = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsSlotOf")
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1819
try:
  EcsOrderedChildren = (ecs_entity_t).in_dll(
    _libs["libflecs.dylib"], "EcsOrderedChildren"
  )
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1822
try:
  EcsModule = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsModule")
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1825
try:
  EcsPrivate = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsPrivate")
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1829
try:
  EcsPrefab = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsPrefab")
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1833
try:
  EcsDisabled = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsDisabled")
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1838
try:
  EcsNotQueryable = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsNotQueryable")
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1841
try:
  EcsOnAdd = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsOnAdd")
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1844
try:
  EcsOnRemove = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsOnRemove")
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1847
try:
  EcsOnSet = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsOnSet")
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1850
try:
  EcsMonitor = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsMonitor")
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1853
try:
  EcsOnTableCreate = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsOnTableCreate")
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1856
try:
  EcsOnTableDelete = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsOnTableDelete")
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1859
try:
  EcsOnDelete = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsOnDelete")
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1863
try:
  EcsOnDeleteTarget = (ecs_entity_t).in_dll(
    _libs["libflecs.dylib"], "EcsOnDeleteTarget"
  )
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1867
try:
  EcsRemove = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsRemove")
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1871
try:
  EcsDelete = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsDelete")
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1875
try:
  EcsPanic = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsPanic")
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1878
try:
  EcsSparse = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsSparse")
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1881
try:
  EcsDontFragment = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsDontFragment")
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1884
try:
  EcsPredEq = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsPredEq")
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1887
try:
  EcsPredMatch = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsPredMatch")
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1890
try:
  EcsPredLookup = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsPredLookup")
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1893
try:
  EcsScopeOpen = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsScopeOpen")
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1896
try:
  EcsScopeClose = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsScopeClose")
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1902
try:
  EcsEmpty = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsEmpty")
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1904
try:
  FLECS_IDEcsPipelineID_ = (ecs_entity_t).in_dll(
    _libs["libflecs.dylib"], "FLECS_IDEcsPipelineID_"
  )
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1905
try:
  EcsOnStart = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsOnStart")
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1906
try:
  EcsPreFrame = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsPreFrame")
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1907
try:
  EcsOnLoad = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsOnLoad")
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1908
try:
  EcsPostLoad = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsPostLoad")
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1909
try:
  EcsPreUpdate = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsPreUpdate")
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1910
try:
  EcsOnUpdate = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsOnUpdate")
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1911
try:
  EcsOnValidate = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsOnValidate")
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1912
try:
  EcsPostUpdate = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsPostUpdate")
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1913
try:
  EcsPreStore = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsPreStore")
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1914
try:
  EcsOnStore = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsOnStore")
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1915
try:
  EcsPostFrame = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsPostFrame")
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1916
try:
  EcsPhase = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsPhase")
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1918
try:
  EcsConstant = (ecs_entity_t).in_dll(_libs["libflecs.dylib"], "EcsConstant")
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1960
if _libs["libflecs.dylib"].has("ecs_init", "cdecl"):
  ecs_init = _libs["libflecs.dylib"].get("ecs_init", "cdecl")
  ecs_init.argtypes = []
  ecs_init.restype = POINTER(ecs_world_t)

# /Users/eherz/git/flecs/include/flecs.h: 1969
if _libs["libflecs.dylib"].has("ecs_mini", "cdecl"):
  ecs_mini = _libs["libflecs.dylib"].get("ecs_mini", "cdecl")
  ecs_mini.argtypes = []
  ecs_mini.restype = POINTER(ecs_world_t)

# /Users/eherz/git/flecs/include/flecs.h: 1979
if _libs["libflecs.dylib"].has("ecs_init_w_args", "cdecl"):
  ecs_init_w_args = _libs["libflecs.dylib"].get("ecs_init_w_args", "cdecl")
  ecs_init_w_args.argtypes = [c_int, POINTER(POINTER(c_char))]
  ecs_init_w_args.restype = POINTER(ecs_world_t)

# /Users/eherz/git/flecs/include/flecs.h: 1990
if _libs["libflecs.dylib"].has("ecs_fini", "cdecl"):
  ecs_fini = _libs["libflecs.dylib"].get("ecs_fini", "cdecl")
  ecs_fini.argtypes = [POINTER(ecs_world_t)]
  ecs_fini.restype = c_int

# /Users/eherz/git/flecs/include/flecs.h: 2001
if _libs["libflecs.dylib"].has("ecs_is_fini", "cdecl"):
  ecs_is_fini = _libs["libflecs.dylib"].get("ecs_is_fini", "cdecl")
  ecs_is_fini.argtypes = [POINTER(ecs_world_t)]
  ecs_is_fini.restype = c_bool

# /Users/eherz/git/flecs/include/flecs.h: 2012
if _libs["libflecs.dylib"].has("ecs_atfini", "cdecl"):
  ecs_atfini = _libs["libflecs.dylib"].get("ecs_atfini", "cdecl")
  ecs_atfini.argtypes = [POINTER(ecs_world_t), ecs_fini_action_t, POINTER(None)]
  ecs_atfini.restype = None


# /Users/eherz/git/flecs/include/flecs.h: 2022
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

ecs_entities_t = struct_ecs_entities_t  # /Users/eherz/git/flecs/include/flecs.h: 2022

# /Users/eherz/git/flecs/include/flecs.h: 2051
if _libs["libflecs.dylib"].has("ecs_get_entities", "cdecl"):
  ecs_get_entities = _libs["libflecs.dylib"].get("ecs_get_entities", "cdecl")
  ecs_get_entities.argtypes = [POINTER(ecs_world_t)]
  ecs_get_entities.restype = ecs_entities_t

# /Users/eherz/git/flecs/include/flecs.h: 2062
if _libs["libflecs.dylib"].has("ecs_world_get_flags", "cdecl"):
  ecs_world_get_flags = _libs["libflecs.dylib"].get("ecs_world_get_flags", "cdecl")
  ecs_world_get_flags.argtypes = [POINTER(ecs_world_t)]
  ecs_world_get_flags.restype = ecs_flags32_t

# /Users/eherz/git/flecs/include/flecs.h: 2091
if _libs["libflecs.dylib"].has("ecs_frame_begin", "cdecl"):
  ecs_frame_begin = _libs["libflecs.dylib"].get("ecs_frame_begin", "cdecl")
  ecs_frame_begin.argtypes = [POINTER(ecs_world_t), c_float]
  ecs_frame_begin.restype = c_float

# /Users/eherz/git/flecs/include/flecs.h: 2102
if _libs["libflecs.dylib"].has("ecs_frame_end", "cdecl"):
  ecs_frame_end = _libs["libflecs.dylib"].get("ecs_frame_end", "cdecl")
  ecs_frame_end.argtypes = [POINTER(ecs_world_t)]
  ecs_frame_end.restype = None

# /Users/eherz/git/flecs/include/flecs.h: 2113
if _libs["libflecs.dylib"].has("ecs_run_post_frame", "cdecl"):
  ecs_run_post_frame = _libs["libflecs.dylib"].get("ecs_run_post_frame", "cdecl")
  ecs_run_post_frame.argtypes = [POINTER(ecs_world_t), ecs_fini_action_t, POINTER(None)]
  ecs_run_post_frame.restype = None

# /Users/eherz/git/flecs/include/flecs.h: 2125
if _libs["libflecs.dylib"].has("ecs_quit", "cdecl"):
  ecs_quit = _libs["libflecs.dylib"].get("ecs_quit", "cdecl")
  ecs_quit.argtypes = [POINTER(ecs_world_t)]
  ecs_quit.restype = None

# /Users/eherz/git/flecs/include/flecs.h: 2135
if _libs["libflecs.dylib"].has("ecs_should_quit", "cdecl"):
  ecs_should_quit = _libs["libflecs.dylib"].get("ecs_should_quit", "cdecl")
  ecs_should_quit.argtypes = [POINTER(ecs_world_t)]
  ecs_should_quit.restype = c_bool

# /Users/eherz/git/flecs/include/flecs.h: 2149
if _libs["libflecs.dylib"].has("ecs_measure_frame_time", "cdecl"):
  ecs_measure_frame_time = _libs["libflecs.dylib"].get(
    "ecs_measure_frame_time", "cdecl"
  )
  ecs_measure_frame_time.argtypes = [POINTER(ecs_world_t), c_bool]
  ecs_measure_frame_time.restype = None

# /Users/eherz/git/flecs/include/flecs.h: 2163
if _libs["libflecs.dylib"].has("ecs_measure_system_time", "cdecl"):
  ecs_measure_system_time = _libs["libflecs.dylib"].get(
    "ecs_measure_system_time", "cdecl"
  )
  ecs_measure_system_time.argtypes = [POINTER(ecs_world_t), c_bool]
  ecs_measure_system_time.restype = None

# /Users/eherz/git/flecs/include/flecs.h: 2183
if _libs["libflecs.dylib"].has("ecs_set_target_fps", "cdecl"):
  ecs_set_target_fps = _libs["libflecs.dylib"].get("ecs_set_target_fps", "cdecl")
  ecs_set_target_fps.argtypes = [POINTER(ecs_world_t), c_float]
  ecs_set_target_fps.restype = None

# /Users/eherz/git/flecs/include/flecs.h: 2201
if _libs["libflecs.dylib"].has("ecs_set_default_query_flags", "cdecl"):
  ecs_set_default_query_flags = _libs["libflecs.dylib"].get(
    "ecs_set_default_query_flags", "cdecl"
  )
  ecs_set_default_query_flags.argtypes = [POINTER(ecs_world_t), ecs_flags32_t]
  ecs_set_default_query_flags.restype = None

# /Users/eherz/git/flecs/include/flecs.h: 2282
if _libs["libflecs.dylib"].has("ecs_readonly_begin", "cdecl"):
  ecs_readonly_begin = _libs["libflecs.dylib"].get("ecs_readonly_begin", "cdecl")
  ecs_readonly_begin.argtypes = [POINTER(ecs_world_t), c_bool]
  ecs_readonly_begin.restype = c_bool

# /Users/eherz/git/flecs/include/flecs.h: 2294
if _libs["libflecs.dylib"].has("ecs_readonly_end", "cdecl"):
  ecs_readonly_end = _libs["libflecs.dylib"].get("ecs_readonly_end", "cdecl")
  ecs_readonly_end.argtypes = [POINTER(ecs_world_t)]
  ecs_readonly_end.restype = None

# /Users/eherz/git/flecs/include/flecs.h: 2308
if _libs["libflecs.dylib"].has("ecs_merge", "cdecl"):
  ecs_merge = _libs["libflecs.dylib"].get("ecs_merge", "cdecl")
  ecs_merge.argtypes = [POINTER(ecs_world_t)]
  ecs_merge.restype = None

# /Users/eherz/git/flecs/include/flecs.h: 2327
if _libs["libflecs.dylib"].has("ecs_defer_begin", "cdecl"):
  ecs_defer_begin = _libs["libflecs.dylib"].get("ecs_defer_begin", "cdecl")
  ecs_defer_begin.argtypes = [POINTER(ecs_world_t)]
  ecs_defer_begin.restype = c_bool

# /Users/eherz/git/flecs/include/flecs.h: 2341
if _libs["libflecs.dylib"].has("ecs_is_deferred", "cdecl"):
  ecs_is_deferred = _libs["libflecs.dylib"].get("ecs_is_deferred", "cdecl")
  ecs_is_deferred.argtypes = [POINTER(ecs_world_t)]
  ecs_is_deferred.restype = c_bool

# /Users/eherz/git/flecs/include/flecs.h: 2358
if _libs["libflecs.dylib"].has("ecs_defer_end", "cdecl"):
  ecs_defer_end = _libs["libflecs.dylib"].get("ecs_defer_end", "cdecl")
  ecs_defer_end.argtypes = [POINTER(ecs_world_t)]
  ecs_defer_end.restype = c_bool

# /Users/eherz/git/flecs/include/flecs.h: 2376
if _libs["libflecs.dylib"].has("ecs_defer_suspend", "cdecl"):
  ecs_defer_suspend = _libs["libflecs.dylib"].get("ecs_defer_suspend", "cdecl")
  ecs_defer_suspend.argtypes = [POINTER(ecs_world_t)]
  ecs_defer_suspend.restype = None

# /Users/eherz/git/flecs/include/flecs.h: 2390
if _libs["libflecs.dylib"].has("ecs_defer_resume", "cdecl"):
  ecs_defer_resume = _libs["libflecs.dylib"].get("ecs_defer_resume", "cdecl")
  ecs_defer_resume.argtypes = [POINTER(ecs_world_t)]
  ecs_defer_resume.restype = None

# /Users/eherz/git/flecs/include/flecs.h: 2407
if _libs["libflecs.dylib"].has("ecs_set_stage_count", "cdecl"):
  ecs_set_stage_count = _libs["libflecs.dylib"].get("ecs_set_stage_count", "cdecl")
  ecs_set_stage_count.argtypes = [POINTER(ecs_world_t), c_int32]
  ecs_set_stage_count.restype = None

# /Users/eherz/git/flecs/include/flecs.h: 2418
if _libs["libflecs.dylib"].has("ecs_get_stage_count", "cdecl"):
  ecs_get_stage_count = _libs["libflecs.dylib"].get("ecs_get_stage_count", "cdecl")
  ecs_get_stage_count.argtypes = [POINTER(ecs_world_t)]
  ecs_get_stage_count.restype = c_int32

# /Users/eherz/git/flecs/include/flecs.h: 2437
if _libs["libflecs.dylib"].has("ecs_get_stage", "cdecl"):
  ecs_get_stage = _libs["libflecs.dylib"].get("ecs_get_stage", "cdecl")
  ecs_get_stage.argtypes = [POINTER(ecs_world_t), c_int32]
  ecs_get_stage.restype = POINTER(ecs_world_t)

# /Users/eherz/git/flecs/include/flecs.h: 2449
if _libs["libflecs.dylib"].has("ecs_stage_is_readonly", "cdecl"):
  ecs_stage_is_readonly = _libs["libflecs.dylib"].get("ecs_stage_is_readonly", "cdecl")
  ecs_stage_is_readonly.argtypes = [POINTER(ecs_world_t)]
  ecs_stage_is_readonly.restype = c_bool

# /Users/eherz/git/flecs/include/flecs.h: 2460
if _libs["libflecs.dylib"].has("ecs_stage_new", "cdecl"):
  ecs_stage_new = _libs["libflecs.dylib"].get("ecs_stage_new", "cdecl")
  ecs_stage_new.argtypes = [POINTER(ecs_world_t)]
  ecs_stage_new.restype = POINTER(ecs_world_t)

# /Users/eherz/git/flecs/include/flecs.h: 2468
if _libs["libflecs.dylib"].has("ecs_stage_free", "cdecl"):
  ecs_stage_free = _libs["libflecs.dylib"].get("ecs_stage_free", "cdecl")
  ecs_stage_free.argtypes = [POINTER(ecs_world_t)]
  ecs_stage_free.restype = None

# /Users/eherz/git/flecs/include/flecs.h: 2479
if _libs["libflecs.dylib"].has("ecs_stage_get_id", "cdecl"):
  ecs_stage_get_id = _libs["libflecs.dylib"].get("ecs_stage_get_id", "cdecl")
  ecs_stage_get_id.argtypes = [POINTER(ecs_world_t)]
  ecs_stage_get_id.restype = c_int32

# /Users/eherz/git/flecs/include/flecs.h: 2498
if _libs["libflecs.dylib"].has("ecs_set_ctx", "cdecl"):
  ecs_set_ctx = _libs["libflecs.dylib"].get("ecs_set_ctx", "cdecl")
  ecs_set_ctx.argtypes = [POINTER(ecs_world_t), POINTER(None), ecs_ctx_free_t]
  ecs_set_ctx.restype = None

# /Users/eherz/git/flecs/include/flecs.h: 2512
if _libs["libflecs.dylib"].has("ecs_set_binding_ctx", "cdecl"):
  ecs_set_binding_ctx = _libs["libflecs.dylib"].get("ecs_set_binding_ctx", "cdecl")
  ecs_set_binding_ctx.argtypes = [POINTER(ecs_world_t), POINTER(None), ecs_ctx_free_t]
  ecs_set_binding_ctx.restype = None

# /Users/eherz/git/flecs/include/flecs.h: 2525
if _libs["libflecs.dylib"].has("ecs_get_ctx", "cdecl"):
  ecs_get_ctx = _libs["libflecs.dylib"].get("ecs_get_ctx", "cdecl")
  ecs_get_ctx.argtypes = [POINTER(ecs_world_t)]
  ecs_get_ctx.restype = POINTER(c_ubyte)
  ecs_get_ctx.errcheck = lambda v, *a: cast(v, c_void_p)

# /Users/eherz/git/flecs/include/flecs.h: 2536
if _libs["libflecs.dylib"].has("ecs_get_binding_ctx", "cdecl"):
  ecs_get_binding_ctx = _libs["libflecs.dylib"].get("ecs_get_binding_ctx", "cdecl")
  ecs_get_binding_ctx.argtypes = [POINTER(ecs_world_t)]
  ecs_get_binding_ctx.restype = POINTER(c_ubyte)
  ecs_get_binding_ctx.errcheck = lambda v, *a: cast(v, c_void_p)

# /Users/eherz/git/flecs/include/flecs.h: 2545
if _libs["libflecs.dylib"].has("ecs_get_build_info", "cdecl"):
  ecs_get_build_info = _libs["libflecs.dylib"].get("ecs_get_build_info", "cdecl")
  ecs_get_build_info.argtypes = []
  ecs_get_build_info.restype = POINTER(ecs_build_info_t)

# /Users/eherz/git/flecs/include/flecs.h: 2553
if _libs["libflecs.dylib"].has("ecs_get_world_info", "cdecl"):
  ecs_get_world_info = _libs["libflecs.dylib"].get("ecs_get_world_info", "cdecl")
  ecs_get_world_info.argtypes = [POINTER(ecs_world_t)]
  ecs_get_world_info.restype = POINTER(ecs_world_info_t)

# /Users/eherz/git/flecs/include/flecs.h: 2565
if _libs["libflecs.dylib"].has("ecs_dim", "cdecl"):
  ecs_dim = _libs["libflecs.dylib"].get("ecs_dim", "cdecl")
  ecs_dim.argtypes = [POINTER(ecs_world_t), c_int32]
  ecs_dim.restype = None

# /Users/eherz/git/flecs/include/flecs.h: 2584
if _libs["libflecs.dylib"].has("ecs_shrink", "cdecl"):
  ecs_shrink = _libs["libflecs.dylib"].get("ecs_shrink", "cdecl")
  ecs_shrink.argtypes = [POINTER(ecs_world_t)]
  ecs_shrink.restype = None

# /Users/eherz/git/flecs/include/flecs.h: 2603
if _libs["libflecs.dylib"].has("ecs_set_entity_range", "cdecl"):
  ecs_set_entity_range = _libs["libflecs.dylib"].get("ecs_set_entity_range", "cdecl")
  ecs_set_entity_range.argtypes = [POINTER(ecs_world_t), ecs_entity_t, ecs_entity_t]
  ecs_set_entity_range.restype = None

# /Users/eherz/git/flecs/include/flecs.h: 2619
if _libs["libflecs.dylib"].has("ecs_enable_range_check", "cdecl"):
  ecs_enable_range_check = _libs["libflecs.dylib"].get(
    "ecs_enable_range_check", "cdecl"
  )
  ecs_enable_range_check.argtypes = [POINTER(ecs_world_t), c_bool]
  ecs_enable_range_check.restype = c_bool

# /Users/eherz/git/flecs/include/flecs.h: 2629
if _libs["libflecs.dylib"].has("ecs_get_max_id", "cdecl"):
  ecs_get_max_id = _libs["libflecs.dylib"].get("ecs_get_max_id", "cdecl")
  ecs_get_max_id.argtypes = [POINTER(ecs_world_t)]
  ecs_get_max_id.restype = ecs_entity_t

# /Users/eherz/git/flecs/include/flecs.h: 2646
if _libs["libflecs.dylib"].has("ecs_run_aperiodic", "cdecl"):
  ecs_run_aperiodic = _libs["libflecs.dylib"].get("ecs_run_aperiodic", "cdecl")
  ecs_run_aperiodic.argtypes = [POINTER(ecs_world_t), ecs_flags32_t]
  ecs_run_aperiodic.restype = None


# /Users/eherz/git/flecs/include/flecs.h: 2660
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
  struct_ecs_delete_empty_tables_desc_t  # /Users/eherz/git/flecs/include/flecs.h: 2660
)

# /Users/eherz/git/flecs/include/flecs.h: 2692
if _libs["libflecs.dylib"].has("ecs_delete_empty_tables", "cdecl"):
  ecs_delete_empty_tables = _libs["libflecs.dylib"].get(
    "ecs_delete_empty_tables", "cdecl"
  )
  ecs_delete_empty_tables.argtypes = [
    POINTER(ecs_world_t),
    POINTER(ecs_delete_empty_tables_desc_t),
  ]
  ecs_delete_empty_tables.restype = c_int32

# /Users/eherz/git/flecs/include/flecs.h: 2702
if _libs["libflecs.dylib"].has("ecs_get_world", "cdecl"):
  ecs_get_world = _libs["libflecs.dylib"].get("ecs_get_world", "cdecl")
  ecs_get_world.argtypes = [POINTER(ecs_poly_t)]
  ecs_get_world.restype = POINTER(ecs_world_t)

# /Users/eherz/git/flecs/include/flecs.h: 2711
if _libs["libflecs.dylib"].has("ecs_get_entity", "cdecl"):
  ecs_get_entity = _libs["libflecs.dylib"].get("ecs_get_entity", "cdecl")
  ecs_get_entity.argtypes = [POINTER(ecs_poly_t)]
  ecs_get_entity.restype = ecs_entity_t

# /Users/eherz/git/flecs/include/flecs.h: 2728
if _libs["libflecs.dylib"].has("flecs_poly_is_", "cdecl"):
  flecs_poly_is_ = _libs["libflecs.dylib"].get("flecs_poly_is_", "cdecl")
  flecs_poly_is_.argtypes = [POINTER(ecs_poly_t), c_int32]
  flecs_poly_is_.restype = c_bool

# /Users/eherz/git/flecs/include/flecs.h: 2747
if _libs["libflecs.dylib"].has("ecs_make_pair", "cdecl"):
  ecs_make_pair = _libs["libflecs.dylib"].get("ecs_make_pair", "cdecl")
  ecs_make_pair.argtypes = [ecs_entity_t, ecs_entity_t]
  ecs_make_pair.restype = ecs_id_t

# /Users/eherz/git/flecs/include/flecs.h: 2774
if _libs["libflecs.dylib"].has("ecs_exclusive_access_begin", "cdecl"):
  ecs_exclusive_access_begin = _libs["libflecs.dylib"].get(
    "ecs_exclusive_access_begin", "cdecl"
  )
  ecs_exclusive_access_begin.argtypes = [POINTER(ecs_world_t), String]
  ecs_exclusive_access_begin.restype = None

# /Users/eherz/git/flecs/include/flecs.h: 2801
if _libs["libflecs.dylib"].has("ecs_exclusive_access_end", "cdecl"):
  ecs_exclusive_access_end = _libs["libflecs.dylib"].get(
    "ecs_exclusive_access_end", "cdecl"
  )
  ecs_exclusive_access_end.argtypes = [POINTER(ecs_world_t), c_bool]
  ecs_exclusive_access_end.restype = None

# /Users/eherz/git/flecs/include/flecs.h: 2832
if _libs["libflecs.dylib"].has("ecs_new", "cdecl"):
  ecs_new = _libs["libflecs.dylib"].get("ecs_new", "cdecl")
  ecs_new.argtypes = [POINTER(ecs_world_t)]
  ecs_new.restype = ecs_entity_t

# /Users/eherz/git/flecs/include/flecs.h: 2853
if _libs["libflecs.dylib"].has("ecs_new_low_id", "cdecl"):
  ecs_new_low_id = _libs["libflecs.dylib"].get("ecs_new_low_id", "cdecl")
  ecs_new_low_id.argtypes = [POINTER(ecs_world_t)]
  ecs_new_low_id.restype = ecs_entity_t

# /Users/eherz/git/flecs/include/flecs.h: 2865
if _libs["libflecs.dylib"].has("ecs_new_w_id", "cdecl"):
  ecs_new_w_id = _libs["libflecs.dylib"].get("ecs_new_w_id", "cdecl")
  ecs_new_w_id.argtypes = [POINTER(ecs_world_t), ecs_id_t]
  ecs_new_w_id.restype = ecs_entity_t

# /Users/eherz/git/flecs/include/flecs.h: 2877
if _libs["libflecs.dylib"].has("ecs_new_w_table", "cdecl"):
  ecs_new_w_table = _libs["libflecs.dylib"].get("ecs_new_w_table", "cdecl")
  ecs_new_w_table.argtypes = [POINTER(ecs_world_t), POINTER(ecs_table_t)]
  ecs_new_w_table.restype = ecs_entity_t

# /Users/eherz/git/flecs/include/flecs.h: 2900
if _libs["libflecs.dylib"].has("ecs_entity_init", "cdecl"):
  ecs_entity_init = _libs["libflecs.dylib"].get("ecs_entity_init", "cdecl")
  ecs_entity_init.argtypes = [POINTER(ecs_world_t), POINTER(ecs_entity_desc_t)]
  ecs_entity_init.restype = ecs_entity_t

# /Users/eherz/git/flecs/include/flecs.h: 2931
if _libs["libflecs.dylib"].has("ecs_bulk_init", "cdecl"):
  ecs_bulk_init = _libs["libflecs.dylib"].get("ecs_bulk_init", "cdecl")
  ecs_bulk_init.argtypes = [POINTER(ecs_world_t), POINTER(ecs_bulk_desc_t)]
  ecs_bulk_init.restype = POINTER(ecs_entity_t)

# /Users/eherz/git/flecs/include/flecs.h: 2945
if _libs["libflecs.dylib"].has("ecs_bulk_new_w_id", "cdecl"):
  ecs_bulk_new_w_id = _libs["libflecs.dylib"].get("ecs_bulk_new_w_id", "cdecl")
  ecs_bulk_new_w_id.argtypes = [POINTER(ecs_world_t), ecs_id_t, c_int32]
  ecs_bulk_new_w_id.restype = POINTER(ecs_entity_t)

# /Users/eherz/git/flecs/include/flecs.h: 2966
if _libs["libflecs.dylib"].has("ecs_clone", "cdecl"):
  ecs_clone = _libs["libflecs.dylib"].get("ecs_clone", "cdecl")
  ecs_clone.argtypes = [POINTER(ecs_world_t), ecs_entity_t, ecs_entity_t, c_bool]
  ecs_clone.restype = ecs_entity_t

# /Users/eherz/git/flecs/include/flecs.h: 2981
if _libs["libflecs.dylib"].has("ecs_delete", "cdecl"):
  ecs_delete = _libs["libflecs.dylib"].get("ecs_delete", "cdecl")
  ecs_delete.argtypes = [POINTER(ecs_world_t), ecs_entity_t]
  ecs_delete.restype = None

# /Users/eherz/git/flecs/include/flecs.h: 2993
if _libs["libflecs.dylib"].has("ecs_delete_with", "cdecl"):
  ecs_delete_with = _libs["libflecs.dylib"].get("ecs_delete_with", "cdecl")
  ecs_delete_with.argtypes = [POINTER(ecs_world_t), ecs_id_t]
  ecs_delete_with.restype = None

# /Users/eherz/git/flecs/include/flecs.h: 3015
if _libs["libflecs.dylib"].has("ecs_set_child_order", "cdecl"):
  ecs_set_child_order = _libs["libflecs.dylib"].get("ecs_set_child_order", "cdecl")
  ecs_set_child_order.argtypes = [
    POINTER(ecs_world_t),
    ecs_entity_t,
    POINTER(ecs_entity_t),
    c_int32,
  ]
  ecs_set_child_order.restype = None

# /Users/eherz/git/flecs/include/flecs.h: 3031
if _libs["libflecs.dylib"].has("ecs_get_ordered_children", "cdecl"):
  ecs_get_ordered_children = _libs["libflecs.dylib"].get(
    "ecs_get_ordered_children", "cdecl"
  )
  ecs_get_ordered_children.argtypes = [POINTER(ecs_world_t), ecs_entity_t]
  ecs_get_ordered_children.restype = ecs_entities_t

# /Users/eherz/git/flecs/include/flecs.h: 3053
if _libs["libflecs.dylib"].has("ecs_add_id", "cdecl"):
  ecs_add_id = _libs["libflecs.dylib"].get("ecs_add_id", "cdecl")
  ecs_add_id.argtypes = [POINTER(ecs_world_t), ecs_entity_t, ecs_id_t]
  ecs_add_id.restype = None

# /Users/eherz/git/flecs/include/flecs.h: 3067
if _libs["libflecs.dylib"].has("ecs_remove_id", "cdecl"):
  ecs_remove_id = _libs["libflecs.dylib"].get("ecs_remove_id", "cdecl")
  ecs_remove_id.argtypes = [POINTER(ecs_world_t), ecs_entity_t, ecs_id_t]
  ecs_remove_id.restype = None

# /Users/eherz/git/flecs/include/flecs.h: 3124
if _libs["libflecs.dylib"].has("ecs_auto_override_id", "cdecl"):
  ecs_auto_override_id = _libs["libflecs.dylib"].get("ecs_auto_override_id", "cdecl")
  ecs_auto_override_id.argtypes = [POINTER(ecs_world_t), ecs_entity_t, ecs_id_t]
  ecs_auto_override_id.restype = None

# /Users/eherz/git/flecs/include/flecs.h: 3136
if _libs["libflecs.dylib"].has("ecs_clear", "cdecl"):
  ecs_clear = _libs["libflecs.dylib"].get("ecs_clear", "cdecl")
  ecs_clear.argtypes = [POINTER(ecs_world_t), ecs_entity_t]
  ecs_clear.restype = None

# /Users/eherz/git/flecs/include/flecs.h: 3148
if _libs["libflecs.dylib"].has("ecs_remove_all", "cdecl"):
  ecs_remove_all = _libs["libflecs.dylib"].get("ecs_remove_all", "cdecl")
  ecs_remove_all.argtypes = [POINTER(ecs_world_t), ecs_id_t]
  ecs_remove_all.restype = None

# /Users/eherz/git/flecs/include/flecs.h: 3160
if _libs["libflecs.dylib"].has("ecs_set_with", "cdecl"):
  ecs_set_with = _libs["libflecs.dylib"].get("ecs_set_with", "cdecl")
  ecs_set_with.argtypes = [POINTER(ecs_world_t), ecs_id_t]
  ecs_set_with.restype = ecs_entity_t

# /Users/eherz/git/flecs/include/flecs.h: 3171
if _libs["libflecs.dylib"].has("ecs_get_with", "cdecl"):
  ecs_get_with = _libs["libflecs.dylib"].get("ecs_get_with", "cdecl")
  ecs_get_with.argtypes = [POINTER(ecs_world_t)]
  ecs_get_with.restype = ecs_id_t

# /Users/eherz/git/flecs/include/flecs.h: 3193
if _libs["libflecs.dylib"].has("ecs_enable", "cdecl"):
  ecs_enable = _libs["libflecs.dylib"].get("ecs_enable", "cdecl")
  ecs_enable.argtypes = [POINTER(ecs_world_t), ecs_entity_t, c_bool]
  ecs_enable.restype = None

# /Users/eherz/git/flecs/include/flecs.h: 3212
if _libs["libflecs.dylib"].has("ecs_enable_id", "cdecl"):
  ecs_enable_id = _libs["libflecs.dylib"].get("ecs_enable_id", "cdecl")
  ecs_enable_id.argtypes = [POINTER(ecs_world_t), ecs_entity_t, ecs_id_t, c_bool]
  ecs_enable_id.restype = None

# /Users/eherz/git/flecs/include/flecs.h: 3229
if _libs["libflecs.dylib"].has("ecs_is_enabled_id", "cdecl"):
  ecs_is_enabled_id = _libs["libflecs.dylib"].get("ecs_is_enabled_id", "cdecl")
  ecs_is_enabled_id.argtypes = [POINTER(ecs_world_t), ecs_entity_t, ecs_id_t]
  ecs_is_enabled_id.restype = c_bool

# /Users/eherz/git/flecs/include/flecs.h: 3258
if _libs["libflecs.dylib"].has("ecs_get_id", "cdecl"):
  ecs_get_id = _libs["libflecs.dylib"].get("ecs_get_id", "cdecl")
  ecs_get_id.argtypes = [POINTER(ecs_world_t), ecs_entity_t, ecs_id_t]
  ecs_get_id.restype = POINTER(c_ubyte)
  ecs_get_id.errcheck = lambda v, *a: cast(v, c_void_p)

# /Users/eherz/git/flecs/include/flecs.h: 3275
if _libs["libflecs.dylib"].has("ecs_get_mut_id", "cdecl"):
  ecs_get_mut_id = _libs["libflecs.dylib"].get("ecs_get_mut_id", "cdecl")
  ecs_get_mut_id.argtypes = [POINTER(ecs_world_t), ecs_entity_t, ecs_id_t]
  ecs_get_mut_id.restype = POINTER(c_ubyte)
  ecs_get_mut_id.errcheck = lambda v, *a: cast(v, c_void_p)

# /Users/eherz/git/flecs/include/flecs.h: 3297
if _libs["libflecs.dylib"].has("ecs_ensure_id", "cdecl"):
  ecs_ensure_id = _libs["libflecs.dylib"].get("ecs_ensure_id", "cdecl")
  ecs_ensure_id.argtypes = [POINTER(ecs_world_t), ecs_entity_t, ecs_id_t, c_size_t]
  ecs_ensure_id.restype = POINTER(c_ubyte)
  ecs_ensure_id.errcheck = lambda v, *a: cast(v, c_void_p)

# /Users/eherz/git/flecs/include/flecs.h: 3314
if _libs["libflecs.dylib"].has("ecs_ref_init_id", "cdecl"):
  ecs_ref_init_id = _libs["libflecs.dylib"].get("ecs_ref_init_id", "cdecl")
  ecs_ref_init_id.argtypes = [POINTER(ecs_world_t), ecs_entity_t, ecs_id_t]
  ecs_ref_init_id.restype = ecs_ref_t

# /Users/eherz/git/flecs/include/flecs.h: 3328
if _libs["libflecs.dylib"].has("ecs_ref_get_id", "cdecl"):
  ecs_ref_get_id = _libs["libflecs.dylib"].get("ecs_ref_get_id", "cdecl")
  ecs_ref_get_id.argtypes = [POINTER(ecs_world_t), POINTER(ecs_ref_t), ecs_id_t]
  ecs_ref_get_id.restype = POINTER(c_ubyte)
  ecs_ref_get_id.errcheck = lambda v, *a: cast(v, c_void_p)

# /Users/eherz/git/flecs/include/flecs.h: 3341
if _libs["libflecs.dylib"].has("ecs_ref_update", "cdecl"):
  ecs_ref_update = _libs["libflecs.dylib"].get("ecs_ref_update", "cdecl")
  ecs_ref_update.argtypes = [POINTER(ecs_world_t), POINTER(ecs_ref_t)]
  ecs_ref_update.restype = None

# /Users/eherz/git/flecs/include/flecs.h: 3365
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

# /Users/eherz/git/flecs/include/flecs.h: 3381
if _libs["libflecs.dylib"].has("ecs_modified_id", "cdecl"):
  ecs_modified_id = _libs["libflecs.dylib"].get("ecs_modified_id", "cdecl")
  ecs_modified_id.argtypes = [POINTER(ecs_world_t), ecs_entity_t, ecs_id_t]
  ecs_modified_id.restype = None

# /Users/eherz/git/flecs/include/flecs.h: 3402
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

# /Users/eherz/git/flecs/include/flecs.h: 3437
if _libs["libflecs.dylib"].has("ecs_is_valid", "cdecl"):
  ecs_is_valid = _libs["libflecs.dylib"].get("ecs_is_valid", "cdecl")
  ecs_is_valid.argtypes = [POINTER(ecs_world_t), ecs_entity_t]
  ecs_is_valid.restype = c_bool

# /Users/eherz/git/flecs/include/flecs.h: 3467
if _libs["libflecs.dylib"].has("ecs_is_alive", "cdecl"):
  ecs_is_alive = _libs["libflecs.dylib"].get("ecs_is_alive", "cdecl")
  ecs_is_alive.argtypes = [POINTER(ecs_world_t), ecs_entity_t]
  ecs_is_alive.restype = c_bool

# /Users/eherz/git/flecs/include/flecs.h: 3477
if _libs["libflecs.dylib"].has("ecs_strip_generation", "cdecl"):
  ecs_strip_generation = _libs["libflecs.dylib"].get("ecs_strip_generation", "cdecl")
  ecs_strip_generation.argtypes = [ecs_entity_t]
  ecs_strip_generation.restype = ecs_id_t

# /Users/eherz/git/flecs/include/flecs.h: 3497
if _libs["libflecs.dylib"].has("ecs_get_alive", "cdecl"):
  ecs_get_alive = _libs["libflecs.dylib"].get("ecs_get_alive", "cdecl")
  ecs_get_alive.argtypes = [POINTER(ecs_world_t), ecs_entity_t]
  ecs_get_alive.restype = ecs_entity_t

# /Users/eherz/git/flecs/include/flecs.h: 3524
if _libs["libflecs.dylib"].has("ecs_make_alive", "cdecl"):
  ecs_make_alive = _libs["libflecs.dylib"].get("ecs_make_alive", "cdecl")
  ecs_make_alive.argtypes = [POINTER(ecs_world_t), ecs_entity_t]
  ecs_make_alive.restype = None

# /Users/eherz/git/flecs/include/flecs.h: 3547
if _libs["libflecs.dylib"].has("ecs_make_alive_id", "cdecl"):
  ecs_make_alive_id = _libs["libflecs.dylib"].get("ecs_make_alive_id", "cdecl")
  ecs_make_alive_id.argtypes = [POINTER(ecs_world_t), ecs_id_t]
  ecs_make_alive_id.restype = None

# /Users/eherz/git/flecs/include/flecs.h: 3559
if _libs["libflecs.dylib"].has("ecs_exists", "cdecl"):
  ecs_exists = _libs["libflecs.dylib"].get("ecs_exists", "cdecl")
  ecs_exists.argtypes = [POINTER(ecs_world_t), ecs_entity_t]
  ecs_exists.restype = c_bool

# /Users/eherz/git/flecs/include/flecs.h: 3578
if _libs["libflecs.dylib"].has("ecs_set_version", "cdecl"):
  ecs_set_version = _libs["libflecs.dylib"].get("ecs_set_version", "cdecl")
  ecs_set_version.argtypes = [POINTER(ecs_world_t), ecs_entity_t]
  ecs_set_version.restype = None

# /Users/eherz/git/flecs/include/flecs.h: 3598
if _libs["libflecs.dylib"].has("ecs_get_type", "cdecl"):
  ecs_get_type = _libs["libflecs.dylib"].get("ecs_get_type", "cdecl")
  ecs_get_type.argtypes = [POINTER(ecs_world_t), ecs_entity_t]
  ecs_get_type.restype = POINTER(ecs_type_t)

# /Users/eherz/git/flecs/include/flecs.h: 3609
if _libs["libflecs.dylib"].has("ecs_get_table", "cdecl"):
  ecs_get_table = _libs["libflecs.dylib"].get("ecs_get_table", "cdecl")
  ecs_get_table.argtypes = [POINTER(ecs_world_t), ecs_entity_t]
  ecs_get_table.restype = POINTER(ecs_table_t)

# /Users/eherz/git/flecs/include/flecs.h: 3621
if _libs["libflecs.dylib"].has("ecs_type_str", "cdecl"):
  ecs_type_str = _libs["libflecs.dylib"].get("ecs_type_str", "cdecl")
  ecs_type_str.argtypes = [POINTER(ecs_world_t), POINTER(ecs_type_t)]
  if sizeof(c_int) == sizeof(c_void_p):
    ecs_type_str.restype = ReturnString
  else:
    ecs_type_str.restype = String
    ecs_type_str.errcheck = ReturnString

# /Users/eherz/git/flecs/include/flecs.h: 3637
if _libs["libflecs.dylib"].has("ecs_table_str", "cdecl"):
  ecs_table_str = _libs["libflecs.dylib"].get("ecs_table_str", "cdecl")
  ecs_table_str.argtypes = [POINTER(ecs_world_t), POINTER(ecs_table_t)]
  if sizeof(c_int) == sizeof(c_void_p):
    ecs_table_str.restype = ReturnString
  else:
    ecs_table_str.restype = String
    ecs_table_str.errcheck = ReturnString

# /Users/eherz/git/flecs/include/flecs.h: 3656
if _libs["libflecs.dylib"].has("ecs_entity_str", "cdecl"):
  ecs_entity_str = _libs["libflecs.dylib"].get("ecs_entity_str", "cdecl")
  ecs_entity_str.argtypes = [POINTER(ecs_world_t), ecs_entity_t]
  if sizeof(c_int) == sizeof(c_void_p):
    ecs_entity_str.restype = ReturnString
  else:
    ecs_entity_str.restype = String
    ecs_entity_str.errcheck = ReturnString

# /Users/eherz/git/flecs/include/flecs.h: 3671
if _libs["libflecs.dylib"].has("ecs_has_id", "cdecl"):
  ecs_has_id = _libs["libflecs.dylib"].get("ecs_has_id", "cdecl")
  ecs_has_id.argtypes = [POINTER(ecs_world_t), ecs_entity_t, ecs_id_t]
  ecs_has_id.restype = c_bool

# /Users/eherz/git/flecs/include/flecs.h: 3687
if _libs["libflecs.dylib"].has("ecs_owns_id", "cdecl"):
  ecs_owns_id = _libs["libflecs.dylib"].get("ecs_owns_id", "cdecl")
  ecs_owns_id.argtypes = [POINTER(ecs_world_t), ecs_entity_t, ecs_id_t]
  ecs_owns_id.restype = c_bool

# /Users/eherz/git/flecs/include/flecs.h: 3707
if _libs["libflecs.dylib"].has("ecs_get_target", "cdecl"):
  ecs_get_target = _libs["libflecs.dylib"].get("ecs_get_target", "cdecl")
  ecs_get_target.argtypes = [POINTER(ecs_world_t), ecs_entity_t, ecs_entity_t, c_int32]
  ecs_get_target.restype = ecs_entity_t

# /Users/eherz/git/flecs/include/flecs.h: 3727
if _libs["libflecs.dylib"].has("ecs_get_parent", "cdecl"):
  ecs_get_parent = _libs["libflecs.dylib"].get("ecs_get_parent", "cdecl")
  ecs_get_parent.argtypes = [POINTER(ecs_world_t), ecs_entity_t]
  ecs_get_parent.restype = ecs_entity_t

# /Users/eherz/git/flecs/include/flecs.h: 3752
if _libs["libflecs.dylib"].has("ecs_get_target_for_id", "cdecl"):
  ecs_get_target_for_id = _libs["libflecs.dylib"].get("ecs_get_target_for_id", "cdecl")
  ecs_get_target_for_id.argtypes = [
    POINTER(ecs_world_t),
    ecs_entity_t,
    ecs_entity_t,
    ecs_id_t,
  ]
  ecs_get_target_for_id.restype = ecs_entity_t

# /Users/eherz/git/flecs/include/flecs.h: 3769
if _libs["libflecs.dylib"].has("ecs_get_depth", "cdecl"):
  ecs_get_depth = _libs["libflecs.dylib"].get("ecs_get_depth", "cdecl")
  ecs_get_depth.argtypes = [POINTER(ecs_world_t), ecs_entity_t, ecs_entity_t]
  ecs_get_depth.restype = c_int32

# /Users/eherz/git/flecs/include/flecs.h: 3782
if _libs["libflecs.dylib"].has("ecs_count_id", "cdecl"):
  ecs_count_id = _libs["libflecs.dylib"].get("ecs_count_id", "cdecl")
  ecs_count_id.argtypes = [POINTER(ecs_world_t), ecs_id_t]
  ecs_count_id.restype = c_int32

# /Users/eherz/git/flecs/include/flecs.h: 3806
if _libs["libflecs.dylib"].has("ecs_get_name", "cdecl"):
  ecs_get_name = _libs["libflecs.dylib"].get("ecs_get_name", "cdecl")
  ecs_get_name.argtypes = [POINTER(ecs_world_t), ecs_entity_t]
  ecs_get_name.restype = c_char_p

# /Users/eherz/git/flecs/include/flecs.h: 3820
if _libs["libflecs.dylib"].has("ecs_get_symbol", "cdecl"):
  ecs_get_symbol = _libs["libflecs.dylib"].get("ecs_get_symbol", "cdecl")
  ecs_get_symbol.argtypes = [POINTER(ecs_world_t), ecs_entity_t]
  ecs_get_symbol.restype = c_char_p

# /Users/eherz/git/flecs/include/flecs.h: 3838
if _libs["libflecs.dylib"].has("ecs_set_name", "cdecl"):
  ecs_set_name = _libs["libflecs.dylib"].get("ecs_set_name", "cdecl")
  ecs_set_name.argtypes = [POINTER(ecs_world_t), ecs_entity_t, String]
  ecs_set_name.restype = ecs_entity_t

# /Users/eherz/git/flecs/include/flecs.h: 3857
if _libs["libflecs.dylib"].has("ecs_set_symbol", "cdecl"):
  ecs_set_symbol = _libs["libflecs.dylib"].get("ecs_set_symbol", "cdecl")
  ecs_set_symbol.argtypes = [POINTER(ecs_world_t), ecs_entity_t, String]
  ecs_set_symbol.restype = ecs_entity_t

# /Users/eherz/git/flecs/include/flecs.h: 3874
if _libs["libflecs.dylib"].has("ecs_set_alias", "cdecl"):
  ecs_set_alias = _libs["libflecs.dylib"].get("ecs_set_alias", "cdecl")
  ecs_set_alias.argtypes = [POINTER(ecs_world_t), ecs_entity_t, String]
  ecs_set_alias.restype = None

# /Users/eherz/git/flecs/include/flecs.h: 3895
if _libs["libflecs.dylib"].has("ecs_lookup", "cdecl"):
  ecs_lookup = _libs["libflecs.dylib"].get("ecs_lookup", "cdecl")
  ecs_lookup.argtypes = [POINTER(ecs_world_t), String]
  ecs_lookup.restype = ecs_entity_t

# /Users/eherz/git/flecs/include/flecs.h: 3914
if _libs["libflecs.dylib"].has("ecs_lookup_child", "cdecl"):
  ecs_lookup_child = _libs["libflecs.dylib"].get("ecs_lookup_child", "cdecl")
  ecs_lookup_child.argtypes = [POINTER(ecs_world_t), ecs_entity_t, String]
  ecs_lookup_child.restype = ecs_entity_t

# /Users/eherz/git/flecs/include/flecs.h: 3942
if _libs["libflecs.dylib"].has("ecs_lookup_path_w_sep", "cdecl"):
  ecs_lookup_path_w_sep = _libs["libflecs.dylib"].get("ecs_lookup_path_w_sep", "cdecl")
  ecs_lookup_path_w_sep.argtypes = [
    POINTER(ecs_world_t),
    ecs_entity_t,
    String,
    String,
    String,
    c_bool,
  ]
  ecs_lookup_path_w_sep.restype = ecs_entity_t

# /Users/eherz/git/flecs/include/flecs.h: 3968
if _libs["libflecs.dylib"].has("ecs_lookup_symbol", "cdecl"):
  ecs_lookup_symbol = _libs["libflecs.dylib"].get("ecs_lookup_symbol", "cdecl")
  ecs_lookup_symbol.argtypes = [POINTER(ecs_world_t), String, c_bool, c_bool]
  ecs_lookup_symbol.restype = ecs_entity_t

# /Users/eherz/git/flecs/include/flecs.h: 3996
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

# /Users/eherz/git/flecs/include/flecs.h: 4016
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

# /Users/eherz/git/flecs/include/flecs.h: 4041
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

# /Users/eherz/git/flecs/include/flecs.h: 4063
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

# /Users/eherz/git/flecs/include/flecs.h: 4085
if _libs["libflecs.dylib"].has("ecs_set_scope", "cdecl"):
  ecs_set_scope = _libs["libflecs.dylib"].get("ecs_set_scope", "cdecl")
  ecs_set_scope.argtypes = [POINTER(ecs_world_t), ecs_entity_t]
  ecs_set_scope.restype = ecs_entity_t

# /Users/eherz/git/flecs/include/flecs.h: 4097
if _libs["libflecs.dylib"].has("ecs_get_scope", "cdecl"):
  ecs_get_scope = _libs["libflecs.dylib"].get("ecs_get_scope", "cdecl")
  ecs_get_scope.argtypes = [POINTER(ecs_world_t)]
  ecs_get_scope.restype = ecs_entity_t

# /Users/eherz/git/flecs/include/flecs.h: 4110
if _libs["libflecs.dylib"].has("ecs_set_name_prefix", "cdecl"):
  ecs_set_name_prefix = _libs["libflecs.dylib"].get("ecs_set_name_prefix", "cdecl")
  ecs_set_name_prefix.argtypes = [POINTER(ecs_world_t), String]
  ecs_set_name_prefix.restype = c_char_p

# /Users/eherz/git/flecs/include/flecs.h: 4141
if _libs["libflecs.dylib"].has("ecs_set_lookup_path", "cdecl"):
  ecs_set_lookup_path = _libs["libflecs.dylib"].get("ecs_set_lookup_path", "cdecl")
  ecs_set_lookup_path.argtypes = [POINTER(ecs_world_t), POINTER(ecs_entity_t)]
  ecs_set_lookup_path.restype = POINTER(ecs_entity_t)

# /Users/eherz/git/flecs/include/flecs.h: 4152
if _libs["libflecs.dylib"].has("ecs_get_lookup_path", "cdecl"):
  ecs_get_lookup_path = _libs["libflecs.dylib"].get("ecs_get_lookup_path", "cdecl")
  ecs_get_lookup_path.argtypes = [POINTER(ecs_world_t)]
  ecs_get_lookup_path.restype = POINTER(ecs_entity_t)

# /Users/eherz/git/flecs/include/flecs.h: 4180
if _libs["libflecs.dylib"].has("ecs_component_init", "cdecl"):
  ecs_component_init = _libs["libflecs.dylib"].get("ecs_component_init", "cdecl")
  ecs_component_init.argtypes = [POINTER(ecs_world_t), POINTER(ecs_component_desc_t)]
  ecs_component_init.restype = ecs_entity_t

# /Users/eherz/git/flecs/include/flecs.h: 4194
if _libs["libflecs.dylib"].has("ecs_get_type_info", "cdecl"):
  ecs_get_type_info = _libs["libflecs.dylib"].get("ecs_get_type_info", "cdecl")
  ecs_get_type_info.argtypes = [POINTER(ecs_world_t), ecs_id_t]
  ecs_get_type_info.restype = POINTER(ecs_type_info_t)

# /Users/eherz/git/flecs/include/flecs.h: 4210
if _libs["libflecs.dylib"].has("ecs_set_hooks_id", "cdecl"):
  ecs_set_hooks_id = _libs["libflecs.dylib"].get("ecs_set_hooks_id", "cdecl")
  ecs_set_hooks_id.argtypes = [
    POINTER(ecs_world_t),
    ecs_entity_t,
    POINTER(ecs_type_hooks_t),
  ]
  ecs_set_hooks_id.restype = None

# /Users/eherz/git/flecs/include/flecs.h: 4222
if _libs["libflecs.dylib"].has("ecs_get_hooks_id", "cdecl"):
  ecs_get_hooks_id = _libs["libflecs.dylib"].get("ecs_get_hooks_id", "cdecl")
  ecs_get_hooks_id.argtypes = [POINTER(ecs_world_t), ecs_entity_t]
  ecs_get_hooks_id.restype = POINTER(ecs_type_hooks_t)

# /Users/eherz/git/flecs/include/flecs.h: 4250
if _libs["libflecs.dylib"].has("ecs_id_is_tag", "cdecl"):
  ecs_id_is_tag = _libs["libflecs.dylib"].get("ecs_id_is_tag", "cdecl")
  ecs_id_is_tag.argtypes = [POINTER(ecs_world_t), ecs_id_t]
  ecs_id_is_tag.restype = c_bool

# /Users/eherz/git/flecs/include/flecs.h: 4263
if _libs["libflecs.dylib"].has("ecs_id_in_use", "cdecl"):
  ecs_id_in_use = _libs["libflecs.dylib"].get("ecs_id_in_use", "cdecl")
  ecs_id_in_use.argtypes = [POINTER(ecs_world_t), ecs_id_t]
  ecs_id_in_use.restype = c_bool

# /Users/eherz/git/flecs/include/flecs.h: 4287
if _libs["libflecs.dylib"].has("ecs_get_typeid", "cdecl"):
  ecs_get_typeid = _libs["libflecs.dylib"].get("ecs_get_typeid", "cdecl")
  ecs_get_typeid.argtypes = [POINTER(ecs_world_t), ecs_id_t]
  ecs_get_typeid.restype = ecs_entity_t

# /Users/eherz/git/flecs/include/flecs.h: 4300
if _libs["libflecs.dylib"].has("ecs_id_match", "cdecl"):
  ecs_id_match = _libs["libflecs.dylib"].get("ecs_id_match", "cdecl")
  ecs_id_match.argtypes = [ecs_id_t, ecs_id_t]
  ecs_id_match.restype = c_bool

# /Users/eherz/git/flecs/include/flecs.h: 4310
if _libs["libflecs.dylib"].has("ecs_id_is_pair", "cdecl"):
  ecs_id_is_pair = _libs["libflecs.dylib"].get("ecs_id_is_pair", "cdecl")
  ecs_id_is_pair.argtypes = [ecs_id_t]
  ecs_id_is_pair.restype = c_bool

# /Users/eherz/git/flecs/include/flecs.h: 4319
if _libs["libflecs.dylib"].has("ecs_id_is_wildcard", "cdecl"):
  ecs_id_is_wildcard = _libs["libflecs.dylib"].get("ecs_id_is_wildcard", "cdecl")
  ecs_id_is_wildcard.argtypes = [ecs_id_t]
  ecs_id_is_wildcard.restype = c_bool

# /Users/eherz/git/flecs/include/flecs.h: 4327
if _libs["libflecs.dylib"].has("ecs_id_is_any", "cdecl"):
  ecs_id_is_any = _libs["libflecs.dylib"].get("ecs_id_is_any", "cdecl")
  ecs_id_is_any.argtypes = [ecs_id_t]
  ecs_id_is_any.restype = c_bool

# /Users/eherz/git/flecs/include/flecs.h: 4344
if _libs["libflecs.dylib"].has("ecs_id_is_valid", "cdecl"):
  ecs_id_is_valid = _libs["libflecs.dylib"].get("ecs_id_is_valid", "cdecl")
  ecs_id_is_valid.argtypes = [POINTER(ecs_world_t), ecs_id_t]
  ecs_id_is_valid.restype = c_bool

# /Users/eherz/git/flecs/include/flecs.h: 4357
if _libs["libflecs.dylib"].has("ecs_id_get_flags", "cdecl"):
  ecs_id_get_flags = _libs["libflecs.dylib"].get("ecs_id_get_flags", "cdecl")
  ecs_id_get_flags.argtypes = [POINTER(ecs_world_t), ecs_id_t]
  ecs_id_get_flags.restype = ecs_flags32_t

# /Users/eherz/git/flecs/include/flecs.h: 4368
if _libs["libflecs.dylib"].has("ecs_id_flag_str", "cdecl"):
  ecs_id_flag_str = _libs["libflecs.dylib"].get("ecs_id_flag_str", "cdecl")
  ecs_id_flag_str.argtypes = [ecs_id_t]
  ecs_id_flag_str.restype = c_char_p

# /Users/eherz/git/flecs/include/flecs.h: 4379
if _libs["libflecs.dylib"].has("ecs_id_str", "cdecl"):
  ecs_id_str = _libs["libflecs.dylib"].get("ecs_id_str", "cdecl")
  ecs_id_str.argtypes = [POINTER(ecs_world_t), ecs_id_t]
  if sizeof(c_int) == sizeof(c_void_p):
    ecs_id_str.restype = ReturnString
  else:
    ecs_id_str.restype = String
    ecs_id_str.errcheck = ReturnString

# /Users/eherz/git/flecs/include/flecs.h: 4391
if _libs["libflecs.dylib"].has("ecs_id_str_buf", "cdecl"):
  ecs_id_str_buf = _libs["libflecs.dylib"].get("ecs_id_str_buf", "cdecl")
  ecs_id_str_buf.argtypes = [POINTER(ecs_world_t), ecs_id_t, POINTER(ecs_strbuf_t)]
  ecs_id_str_buf.restype = None

# /Users/eherz/git/flecs/include/flecs.h: 4404
if _libs["libflecs.dylib"].has("ecs_id_from_str", "cdecl"):
  ecs_id_from_str = _libs["libflecs.dylib"].get("ecs_id_from_str", "cdecl")
  ecs_id_from_str.argtypes = [POINTER(ecs_world_t), String]
  ecs_id_from_str.restype = ecs_id_t

# /Users/eherz/git/flecs/include/flecs.h: 4422
if _libs["libflecs.dylib"].has("ecs_term_ref_is_set", "cdecl"):
  ecs_term_ref_is_set = _libs["libflecs.dylib"].get("ecs_term_ref_is_set", "cdecl")
  ecs_term_ref_is_set.argtypes = [POINTER(ecs_term_ref_t)]
  ecs_term_ref_is_set.restype = c_bool

# /Users/eherz/git/flecs/include/flecs.h: 4437
if _libs["libflecs.dylib"].has("ecs_term_is_initialized", "cdecl"):
  ecs_term_is_initialized = _libs["libflecs.dylib"].get(
    "ecs_term_is_initialized", "cdecl"
  )
  ecs_term_is_initialized.argtypes = [POINTER(ecs_term_t)]
  ecs_term_is_initialized.restype = c_bool

# /Users/eherz/git/flecs/include/flecs.h: 4455
if _libs["libflecs.dylib"].has("ecs_term_match_this", "cdecl"):
  ecs_term_match_this = _libs["libflecs.dylib"].get("ecs_term_match_this", "cdecl")
  ecs_term_match_this.argtypes = [POINTER(ecs_term_t)]
  ecs_term_match_this.restype = c_bool

# /Users/eherz/git/flecs/include/flecs.h: 4471
if _libs["libflecs.dylib"].has("ecs_term_match_0", "cdecl"):
  ecs_term_match_0 = _libs["libflecs.dylib"].get("ecs_term_match_0", "cdecl")
  ecs_term_match_0.argtypes = [POINTER(ecs_term_t)]
  ecs_term_match_0.restype = c_bool

# /Users/eherz/git/flecs/include/flecs.h: 4483
if _libs["libflecs.dylib"].has("ecs_term_str", "cdecl"):
  ecs_term_str = _libs["libflecs.dylib"].get("ecs_term_str", "cdecl")
  ecs_term_str.argtypes = [POINTER(ecs_world_t), POINTER(ecs_term_t)]
  if sizeof(c_int) == sizeof(c_void_p):
    ecs_term_str.restype = ReturnString
  else:
    ecs_term_str.restype = String
    ecs_term_str.errcheck = ReturnString

# /Users/eherz/git/flecs/include/flecs.h: 4495
if _libs["libflecs.dylib"].has("ecs_query_str", "cdecl"):
  ecs_query_str = _libs["libflecs.dylib"].get("ecs_query_str", "cdecl")
  ecs_query_str.argtypes = [POINTER(ecs_query_t)]
  if sizeof(c_int) == sizeof(c_void_p):
    ecs_query_str.restype = ReturnString
  else:
    ecs_query_str.restype = String
    ecs_query_str.errcheck = ReturnString

# /Users/eherz/git/flecs/include/flecs.h: 4539
if _libs["libflecs.dylib"].has("ecs_each_id", "cdecl"):
  ecs_each_id = _libs["libflecs.dylib"].get("ecs_each_id", "cdecl")
  ecs_each_id.argtypes = [POINTER(ecs_world_t), ecs_id_t]
  ecs_each_id.restype = ecs_iter_t

# /Users/eherz/git/flecs/include/flecs.h: 4549
if _libs["libflecs.dylib"].has("ecs_each_next", "cdecl"):
  ecs_each_next = _libs["libflecs.dylib"].get("ecs_each_next", "cdecl")
  ecs_each_next.argtypes = [POINTER(ecs_iter_t)]
  ecs_each_next.restype = c_bool

# /Users/eherz/git/flecs/include/flecs.h: 4569
if _libs["libflecs.dylib"].has("ecs_children", "cdecl"):
  ecs_children = _libs["libflecs.dylib"].get("ecs_children", "cdecl")
  ecs_children.argtypes = [POINTER(ecs_world_t), ecs_entity_t]
  ecs_children.restype = ecs_iter_t

# /Users/eherz/git/flecs/include/flecs.h: 4579
if _libs["libflecs.dylib"].has("ecs_children_next", "cdecl"):
  ecs_children_next = _libs["libflecs.dylib"].get("ecs_children_next", "cdecl")
  ecs_children_next.argtypes = [POINTER(ecs_iter_t)]
  ecs_children_next.restype = c_bool

# /Users/eherz/git/flecs/include/flecs.h: 4598
if _libs["libflecs.dylib"].has("ecs_query_init", "cdecl"):
  ecs_query_init = _libs["libflecs.dylib"].get("ecs_query_init", "cdecl")
  ecs_query_init.argtypes = [POINTER(ecs_world_t), POINTER(ecs_query_desc_t)]
  ecs_query_init.restype = POINTER(ecs_query_t)

# /Users/eherz/git/flecs/include/flecs.h: 4607
if _libs["libflecs.dylib"].has("ecs_query_fini", "cdecl"):
  ecs_query_fini = _libs["libflecs.dylib"].get("ecs_query_fini", "cdecl")
  ecs_query_fini.argtypes = [POINTER(ecs_query_t)]
  ecs_query_fini.restype = None

# /Users/eherz/git/flecs/include/flecs.h: 4619
if _libs["libflecs.dylib"].has("ecs_query_find_var", "cdecl"):
  ecs_query_find_var = _libs["libflecs.dylib"].get("ecs_query_find_var", "cdecl")
  ecs_query_find_var.argtypes = [POINTER(ecs_query_t), String]
  ecs_query_find_var.restype = c_int32

# /Users/eherz/git/flecs/include/flecs.h: 4631
if _libs["libflecs.dylib"].has("ecs_query_var_name", "cdecl"):
  ecs_query_var_name = _libs["libflecs.dylib"].get("ecs_query_var_name", "cdecl")
  ecs_query_var_name.argtypes = [POINTER(ecs_query_t), c_int32]
  ecs_query_var_name.restype = c_char_p

# /Users/eherz/git/flecs/include/flecs.h: 4646
if _libs["libflecs.dylib"].has("ecs_query_var_is_entity", "cdecl"):
  ecs_query_var_is_entity = _libs["libflecs.dylib"].get(
    "ecs_query_var_is_entity", "cdecl"
  )
  ecs_query_var_is_entity.argtypes = [POINTER(ecs_query_t), c_int32]
  ecs_query_var_is_entity.restype = c_bool

# /Users/eherz/git/flecs/include/flecs.h: 4719
if _libs["libflecs.dylib"].has("ecs_query_iter", "cdecl"):
  ecs_query_iter = _libs["libflecs.dylib"].get("ecs_query_iter", "cdecl")
  ecs_query_iter.argtypes = [POINTER(ecs_world_t), POINTER(ecs_query_t)]
  ecs_query_iter.restype = ecs_iter_t

# /Users/eherz/git/flecs/include/flecs.h: 4731
if _libs["libflecs.dylib"].has("ecs_query_next", "cdecl"):
  ecs_query_next = _libs["libflecs.dylib"].get("ecs_query_next", "cdecl")
  ecs_query_next.argtypes = [POINTER(ecs_iter_t)]
  ecs_query_next.restype = c_bool

# /Users/eherz/git/flecs/include/flecs.h: 4753
if _libs["libflecs.dylib"].has("ecs_query_has", "cdecl"):
  ecs_query_has = _libs["libflecs.dylib"].get("ecs_query_has", "cdecl")
  ecs_query_has.argtypes = [POINTER(ecs_query_t), ecs_entity_t, POINTER(ecs_iter_t)]
  ecs_query_has.restype = c_bool

# /Users/eherz/git/flecs/include/flecs.h: 4777
if _libs["libflecs.dylib"].has("ecs_query_has_table", "cdecl"):
  ecs_query_has_table = _libs["libflecs.dylib"].get("ecs_query_has_table", "cdecl")
  ecs_query_has_table.argtypes = [
    POINTER(ecs_query_t),
    POINTER(ecs_table_t),
    POINTER(ecs_iter_t),
  ]
  ecs_query_has_table.restype = c_bool

# /Users/eherz/git/flecs/include/flecs.h: 4809
if _libs["libflecs.dylib"].has("ecs_query_has_range", "cdecl"):
  ecs_query_has_range = _libs["libflecs.dylib"].get("ecs_query_has_range", "cdecl")
  ecs_query_has_range.argtypes = [
    POINTER(ecs_query_t),
    POINTER(ecs_table_range_t),
    POINTER(ecs_iter_t),
  ]
  ecs_query_has_range.restype = c_bool

# /Users/eherz/git/flecs/include/flecs.h: 4822
if _libs["libflecs.dylib"].has("ecs_query_match_count", "cdecl"):
  ecs_query_match_count = _libs["libflecs.dylib"].get("ecs_query_match_count", "cdecl")
  ecs_query_match_count.argtypes = [POINTER(ecs_query_t)]
  ecs_query_match_count.restype = c_int32

# /Users/eherz/git/flecs/include/flecs.h: 4835
if _libs["libflecs.dylib"].has("ecs_query_plan", "cdecl"):
  ecs_query_plan = _libs["libflecs.dylib"].get("ecs_query_plan", "cdecl")
  ecs_query_plan.argtypes = [POINTER(ecs_query_t)]
  if sizeof(c_int) == sizeof(c_void_p):
    ecs_query_plan.restype = ReturnString
  else:
    ecs_query_plan.restype = String
    ecs_query_plan.errcheck = ReturnString

# /Users/eherz/git/flecs/include/flecs.h: 4853
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

# /Users/eherz/git/flecs/include/flecs.h: 4875
if _libs["libflecs.dylib"].has("ecs_query_args_parse", "cdecl"):
  ecs_query_args_parse = _libs["libflecs.dylib"].get("ecs_query_args_parse", "cdecl")
  ecs_query_args_parse.argtypes = [POINTER(ecs_query_t), POINTER(ecs_iter_t), String]
  ecs_query_args_parse.restype = c_char_p

# /Users/eherz/git/flecs/include/flecs.h: 4908
if _libs["libflecs.dylib"].has("ecs_query_changed", "cdecl"):
  ecs_query_changed = _libs["libflecs.dylib"].get("ecs_query_changed", "cdecl")
  ecs_query_changed.argtypes = [POINTER(ecs_query_t)]
  ecs_query_changed.restype = c_bool

# /Users/eherz/git/flecs/include/flecs.h: 4920
if _libs["libflecs.dylib"].has("ecs_query_get", "cdecl"):
  ecs_query_get = _libs["libflecs.dylib"].get("ecs_query_get", "cdecl")
  ecs_query_get.argtypes = [POINTER(ecs_world_t), ecs_entity_t]
  ecs_query_get.restype = POINTER(ecs_query_t)

# /Users/eherz/git/flecs/include/flecs.h: 4935
if _libs["libflecs.dylib"].has("ecs_iter_skip", "cdecl"):
  ecs_iter_skip = _libs["libflecs.dylib"].get("ecs_iter_skip", "cdecl")
  ecs_iter_skip.argtypes = [POINTER(ecs_iter_t)]
  ecs_iter_skip.restype = None

# /Users/eherz/git/flecs/include/flecs.h: 4961
if _libs["libflecs.dylib"].has("ecs_iter_set_group", "cdecl"):
  ecs_iter_set_group = _libs["libflecs.dylib"].get("ecs_iter_set_group", "cdecl")
  ecs_iter_set_group.argtypes = [POINTER(ecs_iter_t), uint64_t]
  ecs_iter_set_group.restype = None

# /Users/eherz/git/flecs/include/flecs.h: 4974
if _libs["libflecs.dylib"].has("ecs_query_get_group_ctx", "cdecl"):
  ecs_query_get_group_ctx = _libs["libflecs.dylib"].get(
    "ecs_query_get_group_ctx", "cdecl"
  )
  ecs_query_get_group_ctx.argtypes = [POINTER(ecs_query_t), uint64_t]
  ecs_query_get_group_ctx.restype = POINTER(c_ubyte)
  ecs_query_get_group_ctx.errcheck = lambda v, *a: cast(v, c_void_p)

# /Users/eherz/git/flecs/include/flecs.h: 4987
if _libs["libflecs.dylib"].has("ecs_query_get_group_info", "cdecl"):
  ecs_query_get_group_info = _libs["libflecs.dylib"].get(
    "ecs_query_get_group_info", "cdecl"
  )
  ecs_query_get_group_info.argtypes = [POINTER(ecs_query_t), uint64_t]
  ecs_query_get_group_info.restype = POINTER(ecs_query_group_info_t)


# /Users/eherz/git/flecs/include/flecs.h: 4998
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

ecs_query_count_t = (
  struct_ecs_query_count_t  # /Users/eherz/git/flecs/include/flecs.h: 4998
)

# /Users/eherz/git/flecs/include/flecs.h: 5007
if _libs["libflecs.dylib"].has("ecs_query_count", "cdecl"):
  ecs_query_count = _libs["libflecs.dylib"].get("ecs_query_count", "cdecl")
  ecs_query_count.argtypes = [POINTER(ecs_query_t)]
  ecs_query_count.restype = ecs_query_count_t

# /Users/eherz/git/flecs/include/flecs.h: 5016
if _libs["libflecs.dylib"].has("ecs_query_is_true", "cdecl"):
  ecs_query_is_true = _libs["libflecs.dylib"].get("ecs_query_is_true", "cdecl")
  ecs_query_is_true.argtypes = [POINTER(ecs_query_t)]
  ecs_query_is_true.restype = c_bool

# /Users/eherz/git/flecs/include/flecs.h: 5028
if _libs["libflecs.dylib"].has("ecs_query_get_cache_query", "cdecl"):
  ecs_query_get_cache_query = _libs["libflecs.dylib"].get(
    "ecs_query_get_cache_query", "cdecl"
  )
  ecs_query_get_cache_query.argtypes = [POINTER(ecs_query_t)]
  ecs_query_get_cache_query.restype = POINTER(ecs_query_t)

# /Users/eherz/git/flecs/include/flecs.h: 5060
if _libs["libflecs.dylib"].has("ecs_emit", "cdecl"):
  ecs_emit = _libs["libflecs.dylib"].get("ecs_emit", "cdecl")
  ecs_emit.argtypes = [POINTER(ecs_world_t), POINTER(ecs_event_desc_t)]
  ecs_emit.restype = None

# /Users/eherz/git/flecs/include/flecs.h: 5075
if _libs["libflecs.dylib"].has("ecs_enqueue", "cdecl"):
  ecs_enqueue = _libs["libflecs.dylib"].get("ecs_enqueue", "cdecl")
  ecs_enqueue.argtypes = [POINTER(ecs_world_t), POINTER(ecs_event_desc_t)]
  ecs_enqueue.restype = None

# /Users/eherz/git/flecs/include/flecs.h: 5090
if _libs["libflecs.dylib"].has("ecs_observer_init", "cdecl"):
  ecs_observer_init = _libs["libflecs.dylib"].get("ecs_observer_init", "cdecl")
  ecs_observer_init.argtypes = [POINTER(ecs_world_t), POINTER(ecs_observer_desc_t)]
  ecs_observer_init.restype = ecs_entity_t

# /Users/eherz/git/flecs/include/flecs.h: 5103
if _libs["libflecs.dylib"].has("ecs_observer_get", "cdecl"):
  ecs_observer_get = _libs["libflecs.dylib"].get("ecs_observer_get", "cdecl")
  ecs_observer_get.argtypes = [POINTER(ecs_world_t), ecs_entity_t]
  ecs_observer_get.restype = POINTER(ecs_observer_t)

# /Users/eherz/git/flecs/include/flecs.h: 5130
if _libs["libflecs.dylib"].has("ecs_iter_next", "cdecl"):
  ecs_iter_next = _libs["libflecs.dylib"].get("ecs_iter_next", "cdecl")
  ecs_iter_next.argtypes = [POINTER(ecs_iter_t)]
  ecs_iter_next.restype = c_bool

# /Users/eherz/git/flecs/include/flecs.h: 5143
if _libs["libflecs.dylib"].has("ecs_iter_fini", "cdecl"):
  ecs_iter_fini = _libs["libflecs.dylib"].get("ecs_iter_fini", "cdecl")
  ecs_iter_fini.argtypes = [POINTER(ecs_iter_t)]
  ecs_iter_fini.restype = None

# /Users/eherz/git/flecs/include/flecs.h: 5158
if _libs["libflecs.dylib"].has("ecs_iter_count", "cdecl"):
  ecs_iter_count = _libs["libflecs.dylib"].get("ecs_iter_count", "cdecl")
  ecs_iter_count.argtypes = [POINTER(ecs_iter_t)]
  ecs_iter_count.restype = c_int32

# /Users/eherz/git/flecs/include/flecs.h: 5174
if _libs["libflecs.dylib"].has("ecs_iter_is_true", "cdecl"):
  ecs_iter_is_true = _libs["libflecs.dylib"].get("ecs_iter_is_true", "cdecl")
  ecs_iter_is_true.argtypes = [POINTER(ecs_iter_t)]
  ecs_iter_is_true.restype = c_bool

# /Users/eherz/git/flecs/include/flecs.h: 5185
if _libs["libflecs.dylib"].has("ecs_iter_first", "cdecl"):
  ecs_iter_first = _libs["libflecs.dylib"].get("ecs_iter_first", "cdecl")
  ecs_iter_first.argtypes = [POINTER(ecs_iter_t)]
  ecs_iter_first.restype = ecs_entity_t

# /Users/eherz/git/flecs/include/flecs.h: 5227
if _libs["libflecs.dylib"].has("ecs_iter_set_var", "cdecl"):
  ecs_iter_set_var = _libs["libflecs.dylib"].get("ecs_iter_set_var", "cdecl")
  ecs_iter_set_var.argtypes = [POINTER(ecs_iter_t), c_int32, ecs_entity_t]
  ecs_iter_set_var.restype = None

# /Users/eherz/git/flecs/include/flecs.h: 5243
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

# /Users/eherz/git/flecs/include/flecs.h: 5259
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

# /Users/eherz/git/flecs/include/flecs.h: 5277
if _libs["libflecs.dylib"].has("ecs_iter_get_var", "cdecl"):
  ecs_iter_get_var = _libs["libflecs.dylib"].get("ecs_iter_get_var", "cdecl")
  ecs_iter_get_var.argtypes = [POINTER(ecs_iter_t), c_int32]
  ecs_iter_get_var.restype = ecs_entity_t

# /Users/eherz/git/flecs/include/flecs.h: 5288
if _libs["libflecs.dylib"].has("ecs_iter_get_var_name", "cdecl"):
  ecs_iter_get_var_name = _libs["libflecs.dylib"].get("ecs_iter_get_var_name", "cdecl")
  ecs_iter_get_var_name.argtypes = [POINTER(ecs_iter_t), c_int32]
  ecs_iter_get_var_name.restype = c_char_p

# /Users/eherz/git/flecs/include/flecs.h: 5298
if _libs["libflecs.dylib"].has("ecs_iter_get_var_count", "cdecl"):
  ecs_iter_get_var_count = _libs["libflecs.dylib"].get(
    "ecs_iter_get_var_count", "cdecl"
  )
  ecs_iter_get_var_count.argtypes = [POINTER(ecs_iter_t)]
  ecs_iter_get_var_count.restype = c_int32

# /Users/eherz/git/flecs/include/flecs.h: 5307
if _libs["libflecs.dylib"].has("ecs_iter_get_vars", "cdecl"):
  ecs_iter_get_vars = _libs["libflecs.dylib"].get("ecs_iter_get_vars", "cdecl")
  ecs_iter_get_vars.argtypes = [POINTER(ecs_iter_t)]
  ecs_iter_get_vars.restype = POINTER(ecs_var_t)

# /Users/eherz/git/flecs/include/flecs.h: 5324
if _libs["libflecs.dylib"].has("ecs_iter_get_var_as_table", "cdecl"):
  ecs_iter_get_var_as_table = _libs["libflecs.dylib"].get(
    "ecs_iter_get_var_as_table", "cdecl"
  )
  ecs_iter_get_var_as_table.argtypes = [POINTER(ecs_iter_t), c_int32]
  ecs_iter_get_var_as_table.restype = POINTER(ecs_table_t)

# /Users/eherz/git/flecs/include/flecs.h: 5342
if _libs["libflecs.dylib"].has("ecs_iter_get_var_as_range", "cdecl"):
  ecs_iter_get_var_as_range = _libs["libflecs.dylib"].get(
    "ecs_iter_get_var_as_range", "cdecl"
  )
  ecs_iter_get_var_as_range.argtypes = [POINTER(ecs_iter_t), c_int32]
  ecs_iter_get_var_as_range.restype = ecs_table_range_t

# /Users/eherz/git/flecs/include/flecs.h: 5358
if _libs["libflecs.dylib"].has("ecs_iter_var_is_constrained", "cdecl"):
  ecs_iter_var_is_constrained = _libs["libflecs.dylib"].get(
    "ecs_iter_var_is_constrained", "cdecl"
  )
  ecs_iter_var_is_constrained.argtypes = [POINTER(ecs_iter_t), c_int32]
  ecs_iter_var_is_constrained.restype = c_bool

# /Users/eherz/git/flecs/include/flecs.h: 5374
if _libs["libflecs.dylib"].has("ecs_iter_get_group", "cdecl"):
  ecs_iter_get_group = _libs["libflecs.dylib"].get("ecs_iter_get_group", "cdecl")
  ecs_iter_get_group.argtypes = [POINTER(ecs_iter_t)]
  ecs_iter_get_group.restype = uint64_t

# /Users/eherz/git/flecs/include/flecs.h: 5389
if _libs["libflecs.dylib"].has("ecs_iter_changed", "cdecl"):
  ecs_iter_changed = _libs["libflecs.dylib"].get("ecs_iter_changed", "cdecl")
  ecs_iter_changed.argtypes = [POINTER(ecs_iter_t)]
  ecs_iter_changed.restype = c_bool

# /Users/eherz/git/flecs/include/flecs.h: 5404
if _libs["libflecs.dylib"].has("ecs_iter_str", "cdecl"):
  ecs_iter_str = _libs["libflecs.dylib"].get("ecs_iter_str", "cdecl")
  ecs_iter_str.argtypes = [POINTER(ecs_iter_t)]
  if sizeof(c_int) == sizeof(c_void_p):
    ecs_iter_str.restype = ReturnString
  else:
    ecs_iter_str.restype = String
    ecs_iter_str.errcheck = ReturnString

# /Users/eherz/git/flecs/include/flecs.h: 5423
if _libs["libflecs.dylib"].has("ecs_page_iter", "cdecl"):
  ecs_page_iter = _libs["libflecs.dylib"].get("ecs_page_iter", "cdecl")
  ecs_page_iter.argtypes = [POINTER(ecs_iter_t), c_int32, c_int32]
  ecs_page_iter.restype = ecs_iter_t

# /Users/eherz/git/flecs/include/flecs.h: 5435
if _libs["libflecs.dylib"].has("ecs_page_next", "cdecl"):
  ecs_page_next = _libs["libflecs.dylib"].get("ecs_page_next", "cdecl")
  ecs_page_next.argtypes = [POINTER(ecs_iter_t)]
  ecs_page_next.restype = c_bool

# /Users/eherz/git/flecs/include/flecs.h: 5459
if _libs["libflecs.dylib"].has("ecs_worker_iter", "cdecl"):
  ecs_worker_iter = _libs["libflecs.dylib"].get("ecs_worker_iter", "cdecl")
  ecs_worker_iter.argtypes = [POINTER(ecs_iter_t), c_int32, c_int32]
  ecs_worker_iter.restype = ecs_iter_t

# /Users/eherz/git/flecs/include/flecs.h: 5471
if _libs["libflecs.dylib"].has("ecs_worker_next", "cdecl"):
  ecs_worker_next = _libs["libflecs.dylib"].get("ecs_worker_next", "cdecl")
  ecs_worker_next.argtypes = [POINTER(ecs_iter_t)]
  ecs_worker_next.restype = c_bool

# /Users/eherz/git/flecs/include/flecs.h: 5516
if _libs["libflecs.dylib"].has("ecs_field_w_size", "cdecl"):
  ecs_field_w_size = _libs["libflecs.dylib"].get("ecs_field_w_size", "cdecl")
  ecs_field_w_size.argtypes = [POINTER(ecs_iter_t), c_size_t, c_int8]
  ecs_field_w_size.restype = POINTER(c_ubyte)
  ecs_field_w_size.errcheck = lambda v, *a: cast(v, c_void_p)

# /Users/eherz/git/flecs/include/flecs.h: 5543
if _libs["libflecs.dylib"].has("ecs_field_at_w_size", "cdecl"):
  ecs_field_at_w_size = _libs["libflecs.dylib"].get("ecs_field_at_w_size", "cdecl")
  ecs_field_at_w_size.argtypes = [POINTER(ecs_iter_t), c_size_t, c_int8, c_int32]
  ecs_field_at_w_size.restype = POINTER(c_ubyte)
  ecs_field_at_w_size.errcheck = lambda v, *a: cast(v, c_void_p)

# /Users/eherz/git/flecs/include/flecs.h: 5558
if _libs["libflecs.dylib"].has("ecs_field_is_readonly", "cdecl"):
  ecs_field_is_readonly = _libs["libflecs.dylib"].get("ecs_field_is_readonly", "cdecl")
  ecs_field_is_readonly.argtypes = [POINTER(ecs_iter_t), c_int8]
  ecs_field_is_readonly.restype = c_bool

# /Users/eherz/git/flecs/include/flecs.h: 5573
if _libs["libflecs.dylib"].has("ecs_field_is_writeonly", "cdecl"):
  ecs_field_is_writeonly = _libs["libflecs.dylib"].get(
    "ecs_field_is_writeonly", "cdecl"
  )
  ecs_field_is_writeonly.argtypes = [POINTER(ecs_iter_t), c_int8]
  ecs_field_is_writeonly.restype = c_bool

# /Users/eherz/git/flecs/include/flecs.h: 5584
if _libs["libflecs.dylib"].has("ecs_field_is_set", "cdecl"):
  ecs_field_is_set = _libs["libflecs.dylib"].get("ecs_field_is_set", "cdecl")
  ecs_field_is_set.argtypes = [POINTER(ecs_iter_t), c_int8]
  ecs_field_is_set.restype = c_bool

# /Users/eherz/git/flecs/include/flecs.h: 5595
if _libs["libflecs.dylib"].has("ecs_field_id", "cdecl"):
  ecs_field_id = _libs["libflecs.dylib"].get("ecs_field_id", "cdecl")
  ecs_field_id.argtypes = [POINTER(ecs_iter_t), c_int8]
  ecs_field_id.restype = ecs_id_t

# /Users/eherz/git/flecs/include/flecs.h: 5608
if _libs["libflecs.dylib"].has("ecs_field_column", "cdecl"):
  ecs_field_column = _libs["libflecs.dylib"].get("ecs_field_column", "cdecl")
  ecs_field_column.argtypes = [POINTER(ecs_iter_t), c_int8]
  ecs_field_column.restype = c_int32

# /Users/eherz/git/flecs/include/flecs.h: 5620
if _libs["libflecs.dylib"].has("ecs_field_src", "cdecl"):
  ecs_field_src = _libs["libflecs.dylib"].get("ecs_field_src", "cdecl")
  ecs_field_src.argtypes = [POINTER(ecs_iter_t), c_int8]
  ecs_field_src.restype = ecs_entity_t

# /Users/eherz/git/flecs/include/flecs.h: 5632
if _libs["libflecs.dylib"].has("ecs_field_size", "cdecl"):
  ecs_field_size = _libs["libflecs.dylib"].get("ecs_field_size", "cdecl")
  ecs_field_size.argtypes = [POINTER(ecs_iter_t), c_int8]
  ecs_field_size.restype = c_size_t

# /Users/eherz/git/flecs/include/flecs.h: 5650
if _libs["libflecs.dylib"].has("ecs_field_is_self", "cdecl"):
  ecs_field_is_self = _libs["libflecs.dylib"].get("ecs_field_is_self", "cdecl")
  ecs_field_is_self.argtypes = [POINTER(ecs_iter_t), c_int8]
  ecs_field_is_self.restype = c_bool

# /Users/eherz/git/flecs/include/flecs.h: 5670
if _libs["libflecs.dylib"].has("ecs_table_get_type", "cdecl"):
  ecs_table_get_type = _libs["libflecs.dylib"].get("ecs_table_get_type", "cdecl")
  ecs_table_get_type.argtypes = [POINTER(ecs_table_t)]
  ecs_table_get_type.restype = POINTER(ecs_type_t)

# /Users/eherz/git/flecs/include/flecs.h: 5684
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

# /Users/eherz/git/flecs/include/flecs.h: 5699
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

# /Users/eherz/git/flecs/include/flecs.h: 5712
if _libs["libflecs.dylib"].has("ecs_table_column_count", "cdecl"):
  ecs_table_column_count = _libs["libflecs.dylib"].get(
    "ecs_table_column_count", "cdecl"
  )
  ecs_table_column_count.argtypes = [POINTER(ecs_table_t)]
  ecs_table_column_count.restype = c_int32

# /Users/eherz/git/flecs/include/flecs.h: 5729
if _libs["libflecs.dylib"].has("ecs_table_type_to_column_index", "cdecl"):
  ecs_table_type_to_column_index = _libs["libflecs.dylib"].get(
    "ecs_table_type_to_column_index", "cdecl"
  )
  ecs_table_type_to_column_index.argtypes = [POINTER(ecs_table_t), c_int32]
  ecs_table_type_to_column_index.restype = c_int32

# /Users/eherz/git/flecs/include/flecs.h: 5742
if _libs["libflecs.dylib"].has("ecs_table_column_to_type_index", "cdecl"):
  ecs_table_column_to_type_index = _libs["libflecs.dylib"].get(
    "ecs_table_column_to_type_index", "cdecl"
  )
  ecs_table_column_to_type_index.argtypes = [POINTER(ecs_table_t), c_int32]
  ecs_table_column_to_type_index.restype = c_int32

# /Users/eherz/git/flecs/include/flecs.h: 5755
if _libs["libflecs.dylib"].has("ecs_table_get_column", "cdecl"):
  ecs_table_get_column = _libs["libflecs.dylib"].get("ecs_table_get_column", "cdecl")
  ecs_table_get_column.argtypes = [POINTER(ecs_table_t), c_int32, c_int32]
  ecs_table_get_column.restype = POINTER(c_ubyte)
  ecs_table_get_column.errcheck = lambda v, *a: cast(v, c_void_p)

# /Users/eherz/git/flecs/include/flecs.h: 5770
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

# /Users/eherz/git/flecs/include/flecs.h: 5784
if _libs["libflecs.dylib"].has("ecs_table_get_column_size", "cdecl"):
  ecs_table_get_column_size = _libs["libflecs.dylib"].get(
    "ecs_table_get_column_size", "cdecl"
  )
  ecs_table_get_column_size.argtypes = [POINTER(ecs_table_t), c_int32]
  ecs_table_get_column_size.restype = c_size_t

# /Users/eherz/git/flecs/include/flecs.h: 5795
if _libs["libflecs.dylib"].has("ecs_table_count", "cdecl"):
  ecs_table_count = _libs["libflecs.dylib"].get("ecs_table_count", "cdecl")
  ecs_table_count.argtypes = [POINTER(ecs_table_t)]
  ecs_table_count.restype = c_int32

# /Users/eherz/git/flecs/include/flecs.h: 5806
if _libs["libflecs.dylib"].has("ecs_table_size", "cdecl"):
  ecs_table_size = _libs["libflecs.dylib"].get("ecs_table_size", "cdecl")
  ecs_table_size.argtypes = [POINTER(ecs_table_t)]
  ecs_table_size.restype = c_int32

# /Users/eherz/git/flecs/include/flecs.h: 5816
if _libs["libflecs.dylib"].has("ecs_table_entities", "cdecl"):
  ecs_table_entities = _libs["libflecs.dylib"].get("ecs_table_entities", "cdecl")
  ecs_table_entities.argtypes = [POINTER(ecs_table_t)]
  ecs_table_entities.restype = POINTER(ecs_entity_t)

# /Users/eherz/git/flecs/include/flecs.h: 5830
if _libs["libflecs.dylib"].has("ecs_table_has_id", "cdecl"):
  ecs_table_has_id = _libs["libflecs.dylib"].get("ecs_table_has_id", "cdecl")
  ecs_table_has_id.argtypes = [POINTER(ecs_world_t), POINTER(ecs_table_t), ecs_id_t]
  ecs_table_has_id.restype = c_bool

# /Users/eherz/git/flecs/include/flecs.h: 5846
if _libs["libflecs.dylib"].has("ecs_table_get_depth", "cdecl"):
  ecs_table_get_depth = _libs["libflecs.dylib"].get("ecs_table_get_depth", "cdecl")
  ecs_table_get_depth.argtypes = [
    POINTER(ecs_world_t),
    POINTER(ecs_table_t),
    ecs_entity_t,
  ]
  ecs_table_get_depth.restype = c_int32

# /Users/eherz/git/flecs/include/flecs.h: 5861
if _libs["libflecs.dylib"].has("ecs_table_add_id", "cdecl"):
  ecs_table_add_id = _libs["libflecs.dylib"].get("ecs_table_add_id", "cdecl")
  ecs_table_add_id.argtypes = [POINTER(ecs_world_t), POINTER(ecs_table_t), ecs_id_t]
  ecs_table_add_id.restype = POINTER(ecs_table_t)

# /Users/eherz/git/flecs/include/flecs.h: 5877
if _libs["libflecs.dylib"].has("ecs_table_find", "cdecl"):
  ecs_table_find = _libs["libflecs.dylib"].get("ecs_table_find", "cdecl")
  ecs_table_find.argtypes = [POINTER(ecs_world_t), POINTER(ecs_id_t), c_int32]
  ecs_table_find.restype = POINTER(ecs_table_t)

# /Users/eherz/git/flecs/include/flecs.h: 5892
if _libs["libflecs.dylib"].has("ecs_table_remove_id", "cdecl"):
  ecs_table_remove_id = _libs["libflecs.dylib"].get("ecs_table_remove_id", "cdecl")
  ecs_table_remove_id.argtypes = [POINTER(ecs_world_t), POINTER(ecs_table_t), ecs_id_t]
  ecs_table_remove_id.restype = POINTER(ecs_table_t)

# /Users/eherz/git/flecs/include/flecs.h: 5913
if _libs["libflecs.dylib"].has("ecs_table_lock", "cdecl"):
  ecs_table_lock = _libs["libflecs.dylib"].get("ecs_table_lock", "cdecl")
  ecs_table_lock.argtypes = [POINTER(ecs_world_t), POINTER(ecs_table_t)]
  ecs_table_lock.restype = None

# /Users/eherz/git/flecs/include/flecs.h: 5924
if _libs["libflecs.dylib"].has("ecs_table_unlock", "cdecl"):
  ecs_table_unlock = _libs["libflecs.dylib"].get("ecs_table_unlock", "cdecl")
  ecs_table_unlock.argtypes = [POINTER(ecs_world_t), POINTER(ecs_table_t)]
  ecs_table_unlock.restype = None

# /Users/eherz/git/flecs/include/flecs.h: 5938
if _libs["libflecs.dylib"].has("ecs_table_has_flags", "cdecl"):
  ecs_table_has_flags = _libs["libflecs.dylib"].get("ecs_table_has_flags", "cdecl")
  ecs_table_has_flags.argtypes = [POINTER(ecs_table_t), ecs_flags32_t]
  ecs_table_has_flags.restype = c_bool

# /Users/eherz/git/flecs/include/flecs.h: 5950
if _libs["libflecs.dylib"].has("ecs_table_has_traversable", "cdecl"):
  ecs_table_has_traversable = _libs["libflecs.dylib"].get(
    "ecs_table_has_traversable", "cdecl"
  )
  ecs_table_has_traversable.argtypes = [POINTER(ecs_table_t)]
  ecs_table_has_traversable.restype = c_bool

# /Users/eherz/git/flecs/include/flecs.h: 5961
if _libs["libflecs.dylib"].has("ecs_table_swap_rows", "cdecl"):
  ecs_table_swap_rows = _libs["libflecs.dylib"].get("ecs_table_swap_rows", "cdecl")
  ecs_table_swap_rows.argtypes = [
    POINTER(ecs_world_t),
    POINTER(ecs_table_t),
    c_int32,
    c_int32,
  ]
  ecs_table_swap_rows.restype = None

# /Users/eherz/git/flecs/include/flecs.h: 5990
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

# /Users/eherz/git/flecs/include/flecs.h: 6018
if _libs["libflecs.dylib"].has("ecs_search", "cdecl"):
  ecs_search = _libs["libflecs.dylib"].get("ecs_search", "cdecl")
  ecs_search.argtypes = [
    POINTER(ecs_world_t),
    POINTER(ecs_table_t),
    ecs_id_t,
    POINTER(ecs_id_t),
  ]
  ecs_search.restype = c_int32

# /Users/eherz/git/flecs/include/flecs.h: 6060
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

# /Users/eherz/git/flecs/include/flecs.h: 6109
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

# /Users/eherz/git/flecs/include/flecs.h: 6128
if _libs["libflecs.dylib"].has("ecs_table_clear_entities", "cdecl"):
  ecs_table_clear_entities = _libs["libflecs.dylib"].get(
    "ecs_table_clear_entities", "cdecl"
  )
  ecs_table_clear_entities.argtypes = [POINTER(ecs_world_t), POINTER(ecs_table_t)]
  ecs_table_clear_entities.restype = None

# /Users/eherz/git/flecs/include/flecs.h: 6149
if _libs["libflecs.dylib"].has("ecs_value_init", "cdecl"):
  ecs_value_init = _libs["libflecs.dylib"].get("ecs_value_init", "cdecl")
  ecs_value_init.argtypes = [POINTER(ecs_world_t), ecs_entity_t, POINTER(None)]
  ecs_value_init.restype = c_int

# /Users/eherz/git/flecs/include/flecs.h: 6162
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

# /Users/eherz/git/flecs/include/flecs.h: 6174
if _libs["libflecs.dylib"].has("ecs_value_new", "cdecl"):
  ecs_value_new = _libs["libflecs.dylib"].get("ecs_value_new", "cdecl")
  ecs_value_new.argtypes = [POINTER(ecs_world_t), ecs_entity_t]
  ecs_value_new.restype = POINTER(c_ubyte)
  ecs_value_new.errcheck = lambda v, *a: cast(v, c_void_p)

# /Users/eherz/git/flecs/include/flecs.h: 6184
if _libs["libflecs.dylib"].has("ecs_value_new_w_type_info", "cdecl"):
  ecs_value_new_w_type_info = _libs["libflecs.dylib"].get(
    "ecs_value_new_w_type_info", "cdecl"
  )
  ecs_value_new_w_type_info.argtypes = [POINTER(ecs_world_t), POINTER(ecs_type_info_t)]
  ecs_value_new_w_type_info.restype = POINTER(c_ubyte)
  ecs_value_new_w_type_info.errcheck = lambda v, *a: cast(v, c_void_p)

# /Users/eherz/git/flecs/include/flecs.h: 6195
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

# /Users/eherz/git/flecs/include/flecs.h: 6208
if _libs["libflecs.dylib"].has("ecs_value_fini", "cdecl"):
  ecs_value_fini = _libs["libflecs.dylib"].get("ecs_value_fini", "cdecl")
  ecs_value_fini.argtypes = [POINTER(ecs_world_t), ecs_entity_t, POINTER(None)]
  ecs_value_fini.restype = c_int

# /Users/eherz/git/flecs/include/flecs.h: 6221
if _libs["libflecs.dylib"].has("ecs_value_free", "cdecl"):
  ecs_value_free = _libs["libflecs.dylib"].get("ecs_value_free", "cdecl")
  ecs_value_free.argtypes = [POINTER(ecs_world_t), ecs_entity_t, POINTER(None)]
  ecs_value_free.restype = c_int

# /Users/eherz/git/flecs/include/flecs.h: 6235
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

# /Users/eherz/git/flecs/include/flecs.h: 6250
if _libs["libflecs.dylib"].has("ecs_value_copy", "cdecl"):
  ecs_value_copy = _libs["libflecs.dylib"].get("ecs_value_copy", "cdecl")
  ecs_value_copy.argtypes = [
    POINTER(ecs_world_t),
    ecs_entity_t,
    POINTER(None),
    POINTER(None),
  ]
  ecs_value_copy.restype = c_int

# /Users/eherz/git/flecs/include/flecs.h: 6264
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

# /Users/eherz/git/flecs/include/flecs.h: 6278
if _libs["libflecs.dylib"].has("ecs_value_move", "cdecl"):
  ecs_value_move = _libs["libflecs.dylib"].get("ecs_value_move", "cdecl")
  ecs_value_move.argtypes = [
    POINTER(ecs_world_t),
    ecs_entity_t,
    POINTER(None),
    POINTER(None),
  ]
  ecs_value_move.restype = c_int

# /Users/eherz/git/flecs/include/flecs.h: 6292
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

# /Users/eherz/git/flecs/include/flecs.h: 6306
if _libs["libflecs.dylib"].has("ecs_value_move_ctor", "cdecl"):
  ecs_value_move_ctor = _libs["libflecs.dylib"].get("ecs_value_move_ctor", "cdecl")
  ecs_value_move_ctor.argtypes = [
    POINTER(ecs_world_t),
    ecs_entity_t,
    POINTER(None),
    POINTER(None),
  ]
  ecs_value_move_ctor.restype = c_int

# /Users/eherz/git/flecs/include/flecs.h: 34
try:
  FLECS_VERSION_MAJOR = 4
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 35
try:
  FLECS_VERSION_MINOR = 1
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 36
try:
  FLECS_VERSION_PATCH = 0
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 39
try:
  FLECS_VERSION = FLECS_VERSION_IMPL(
    FLECS_VERSION_MAJOR, FLECS_VERSION_MINOR, FLECS_VERSION_PATCH
  )
except:
  pass

ecs_float_t = c_float  # /Users/eherz/git/flecs/include/flecs.h: 52

# /Users/eherz/git/flecs/include/flecs.h: 263
try:
  FLECS_HI_COMPONENT_ID = 256
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 273
try:
  FLECS_HI_ID_RECORD_ID = 1024
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 282
try:
  FLECS_SPARSE_PAGE_BITS = 6
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 288
try:
  FLECS_ENTITY_PAGE_BITS = 10
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 300
try:
  FLECS_ID_DESC_MAX = 32
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 306
try:
  FLECS_EVENT_DESC_MAX = 8
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 311
try:
  FLECS_VARIABLE_COUNT_MAX = 64
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 316
try:
  FLECS_TERM_COUNT_MAX = 32
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 322
try:
  FLECS_TERM_ARG_COUNT_MAX = 16
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 328
try:
  FLECS_QUERY_VARIABLE_COUNT_MAX = 64
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 334
try:
  FLECS_QUERY_SCOPE_NESTING_MAX = 8
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 342
try:
  FLECS_DAG_DEPTH_MAX = 128
except:
  pass


# /Users/eherz/git/flecs/include/flecs/private/api_defines.h: 295
def ECS_CAST(T, V):
  return T(V)


# /Users/eherz/git/flecs/include/flecs/private/api_defines.h: 321
def FLECS_VERSION_IMPLSTR(major, minor, patch):
  return (((major + ".") + minor) + ".") + patch


# /Users/eherz/git/flecs/include/flecs/private/api_defines.h: 322
def FLECS_VERSION_IMPL(major, minor, patch):
  return FLECS_VERSION_IMPLSTR(major, minor, patch)


# /Users/eherz/git/flecs/include/flecs.h: 719
try:
  EcsSelf = 1 << 63
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 725
try:
  EcsUp = 1 << 62
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 731
try:
  EcsTrav = 1 << 61
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 737
try:
  EcsCascade = 1 << 60
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 743
try:
  EcsDesc = 1 << 59
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 749
try:
  EcsIsVariable = 1 << 58
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 755
try:
  EcsIsEntity = 1 << 57
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 761
try:
  EcsIsName = 1 << 56
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 767
try:
  EcsTraverseFlags = (((EcsSelf | EcsUp) | EcsTrav) | EcsCascade) | EcsDesc
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 773
try:
  EcsTermRefFlags = ((EcsTraverseFlags | EcsIsVariable) | EcsIsEntity) | EcsIsName
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 890
try:
  ECS_TYPE_HOOK_CTOR = ECS_CAST(ecs_flags32_t, (1 << 0))
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 891
try:
  ECS_TYPE_HOOK_DTOR = ECS_CAST(ecs_flags32_t, (1 << 1))
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 892
try:
  ECS_TYPE_HOOK_COPY = ECS_CAST(ecs_flags32_t, (1 << 2))
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 893
try:
  ECS_TYPE_HOOK_MOVE = ECS_CAST(ecs_flags32_t, (1 << 3))
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 894
try:
  ECS_TYPE_HOOK_COPY_CTOR = ECS_CAST(ecs_flags32_t, (1 << 4))
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 895
try:
  ECS_TYPE_HOOK_MOVE_CTOR = ECS_CAST(ecs_flags32_t, (1 << 5))
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 896
try:
  ECS_TYPE_HOOK_CTOR_MOVE_DTOR = ECS_CAST(ecs_flags32_t, (1 << 6))
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 897
try:
  ECS_TYPE_HOOK_MOVE_DTOR = ECS_CAST(ecs_flags32_t, (1 << 7))
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 898
try:
  ECS_TYPE_HOOK_CMP = ECS_CAST(ecs_flags32_t, (1 << 8))
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 899
try:
  ECS_TYPE_HOOK_EQUALS = ECS_CAST(ecs_flags32_t, (1 << 9))
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 903
try:
  ECS_TYPE_HOOK_CTOR_ILLEGAL = ECS_CAST(ecs_flags32_t, (1 << 10))
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 904
try:
  ECS_TYPE_HOOK_DTOR_ILLEGAL = ECS_CAST(ecs_flags32_t, (1 << 12))
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 905
try:
  ECS_TYPE_HOOK_COPY_ILLEGAL = ECS_CAST(ecs_flags32_t, (1 << 13))
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 906
try:
  ECS_TYPE_HOOK_MOVE_ILLEGAL = ECS_CAST(ecs_flags32_t, (1 << 14))
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 907
try:
  ECS_TYPE_HOOK_COPY_CTOR_ILLEGAL = ECS_CAST(ecs_flags32_t, (1 << 15))
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 908
try:
  ECS_TYPE_HOOK_MOVE_CTOR_ILLEGAL = ECS_CAST(ecs_flags32_t, (1 << 16))
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 909
try:
  ECS_TYPE_HOOK_CTOR_MOVE_DTOR_ILLEGAL = ECS_CAST(ecs_flags32_t, (1 << 17))
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 910
try:
  ECS_TYPE_HOOK_MOVE_DTOR_ILLEGAL = ECS_CAST(ecs_flags32_t, (1 << 18))
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 911
try:
  ECS_TYPE_HOOK_CMP_ILLEGAL = ECS_CAST(ecs_flags32_t, (1 << 19))
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 912
try:
  ECS_TYPE_HOOK_EQUALS_ILLEGAL = ECS_CAST(ecs_flags32_t, (1 << 20))
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 916
try:
  ECS_TYPE_HOOKS = (
    (
      (
        (
          (
            (
              ((ECS_TYPE_HOOK_CTOR | ECS_TYPE_HOOK_DTOR) | ECS_TYPE_HOOK_COPY)
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

# /Users/eherz/git/flecs/include/flecs.h: 922
try:
  ECS_TYPE_HOOKS_ILLEGAL = (
    (
      (
        (
          (
            (
              (
                (ECS_TYPE_HOOK_CTOR_ILLEGAL | ECS_TYPE_HOOK_DTOR_ILLEGAL)
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

# /Users/eherz/git/flecs/include/flecs.h: 1211
try:
  EcsQueryMatchPrefab = 1 << 1
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1217
try:
  EcsQueryMatchDisabled = 1 << 2
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1223
try:
  EcsQueryMatchEmptyTables = 1 << 3
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1229
try:
  EcsQueryAllowUnresolvedByName = 1 << 6
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1235
try:
  EcsQueryTableOnly = 1 << 7
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1247
try:
  EcsQueryDetectChanges = 1 << 8
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1656
try:
  EcsSingleton = EcsVariable
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1926
try:
  EcsFirstUserComponentId = 8
except:
  pass

# /Users/eherz/git/flecs/include/flecs.h: 1930
try:
  EcsFirstUserEntityId = FLECS_HI_COMPONENT_ID + 128
except:
  pass

ecs_world_t = struct_ecs_world_t  # /Users/eherz/git/flecs/include/flecs.h: 428

ecs_stage_t = struct_ecs_stage_t  # /Users/eherz/git/flecs/include/flecs.h: 431

ecs_table_t = struct_ecs_table_t  # /Users/eherz/git/flecs/include/flecs.h: 434

ecs_term_t = struct_ecs_term_t  # /Users/eherz/git/flecs/include/flecs.h: 791

ecs_query_t = struct_ecs_query_t  # /Users/eherz/git/flecs/include/flecs.h: 815

ecs_observer_t = struct_ecs_observer_t  # /Users/eherz/git/flecs/include/flecs.h: 856

ecs_iter_t = struct_ecs_iter_t  # /Users/eherz/git/flecs/include/flecs.h: 1145

ecs_type_hooks_t = (
  struct_ecs_type_hooks_t  # /Users/eherz/git/flecs/include/flecs.h: 928
)

ecs_type_info_t = struct_ecs_type_info_t  # /Users/eherz/git/flecs/include/flecs.h: 998

ecs_component_record_t = (
  struct_ecs_component_record_t  # /Users/eherz/git/flecs/include/flecs.h: 496
)

ecs_mixins_t = struct_ecs_mixins_t  # /Users/eherz/git/flecs/include/flecs.h: 520

ecs_header_t = struct_ecs_header_t  # /Users/eherz/git/flecs/include/flecs.h: 527

ecs_term_ref_t = struct_ecs_term_ref_t  # /Users/eherz/git/flecs/include/flecs.h: 788

ecs_value_t = struct_ecs_value_t  # /Users/eherz/git/flecs/include/flecs.h: 1015

ecs_entity_desc_t = (
  struct_ecs_entity_desc_t  # /Users/eherz/git/flecs/include/flecs.h: 1061
)

ecs_bulk_desc_t = struct_ecs_bulk_desc_t  # /Users/eherz/git/flecs/include/flecs.h: 1091

ecs_component_desc_t = (
  struct_ecs_component_desc_t  # /Users/eherz/git/flecs/include/flecs.h: 1105
)

ecs_query_desc_t = (
  struct_ecs_query_desc_t  # /Users/eherz/git/flecs/include/flecs.h: 1322
)

ecs_observer_desc_t = (
  struct_ecs_observer_desc_t  # /Users/eherz/git/flecs/include/flecs.h: 1384
)

ecs_event_desc_t = (
  struct_ecs_event_desc_t  # /Users/eherz/git/flecs/include/flecs.h: 1433
)

ecs_build_info_t = (
  struct_ecs_build_info_t  # /Users/eherz/git/flecs/include/flecs.h: 1454
)

ecs_world_info_t = (
  struct_ecs_world_info_t  # /Users/eherz/git/flecs/include/flecs.h: 1513
)

ecs_query_group_info_t = (
  struct_ecs_query_group_info_t  # /Users/eherz/git/flecs/include/flecs.h: 1521
)

EcsIdentifier = struct_EcsIdentifier  # /Users/eherz/git/flecs/include/flecs.h: 1539

EcsComponent = struct_EcsComponent  # /Users/eherz/git/flecs/include/flecs.h: 1545

EcsPoly = struct_EcsPoly  # /Users/eherz/git/flecs/include/flecs.h: 1550

EcsDefaultChildComponent = (
  struct_EcsDefaultChildComponent  # /Users/eherz/git/flecs/include/flecs.h: 1559
)

ecs_entities_t = struct_ecs_entities_t  # /Users/eherz/git/flecs/include/flecs.h: 2022

ecs_delete_empty_tables_desc_t = (
  struct_ecs_delete_empty_tables_desc_t  # /Users/eherz/git/flecs/include/flecs.h: 2660
)

ecs_query_count_t = (
  struct_ecs_query_count_t  # /Users/eherz/git/flecs/include/flecs.h: 4998
)

# No inserted files

# No prefix-stripping
