# pyflecs

Python bindings for the excellent [flecs](https://www.flecs.dev/flecs/) library.

## Design

pyflecs consists of two parts; 1- `cflecs` and 2- `pyflecs`.

### cflecs

Part #1 is the auto-generated C binding which uses [ctypesgen](https://github.com/ctypesgen/ctypesgen). If so desired you can use this binding directly. For more information, see the section on [cflecs](#cflecs).

### pyflecs

pyflecs itself attempts to present a more python-like interface to to the user. See the user guide below for an introduction to the pyflecs API.

## User Guide

### Factory methods

Pyflecs makes heavy use of several factory method conventions:

- Builders
- Kwargs
- Tuples

Tips:
- Use builders whenever assignment of certain fields is conditional, or you need to construct part of the object, perform some logic, and then construct the rest of the fields
- Use kwargs where builder's expressiveness is desired but the assignment is a single expression and does not require any conditional logic or delayed assignment
- Use tuples when you prefer conciseness and only assigning required fields

### Components

Components are described using decorated classes as templates, similar to python's dataclasses.

```python
from pyflecs.component import component
from pyflecs.types import Double

@component
class Position:
    x: Double
    y: Double
```

#### Limitations of python typing

Python's type are much more flexible than C types, but strict data structure sizing is part of how flecs can be efficient and performant. Therefore, it's recommended that you think of the size of your types, and use the type aliases that are provided in the `pyflecs.types` package in place of python native types.

#### Using python native types (NOT YET SUPPORTED)

In python `int` is not a sized type. Using `int` is a little more natural in python, so we consider it's OK to do so. The same applies to other types, such as `float`, or `str`. The user should be aware of the following:

- When using int, pyflecs assumes you mean CInt32
- When using float, pyflecs assumes you mean CDouble
- When using str, pyflecs assumes you mean CString (char *)

Here's an example of the same code as above, using python `float` instead.

```python
from pyflecs.component import component
from pyflecs.types import Double

@component
class Position:
    x: float
    y: float
```

#### Using strings

Flecs uses utf-8 null terminated C strings. If you specify `str` as the type of your field, in flecs it will be a c_void_p. str allocates data on the heap, so if you want your string to be backed by struct data you'll have to declare a C type character array on your component and then marshal/unmarshal the data to the appropriate type you wish to work with manually. Support for these types of strings may be added in the future.

### Creating components

To create your component, simply call the world method `component`.

```python
from pyflecs.component import component
from pyflecs.types import Double
from pyflecs.world import World

@component
class Position:
    x: Double
    y: Double

def main():
    world = World()
    world.component(Position)
```

The world `component` method returns an `EntityId` which represents the component inside the flecs world. When calling other methods, or interacting with flecs in a way in which you need to provide the component ID, pyflecs also allows you to use the type (`Position`, for instance) as a reference, and the flecs ID will be looked up automatically.


### The component decorator

Because components are decorated using the `@component` decorator, a lot of ritual about preparing components is taken care of for you by pyflecs. However, it should be noted that the returned type is not a subclass of your component class, and therefore any methods, for instance, declared on the original component class will not be carried over. This is by design: you should not be declaring methods on your components, but as it happens, this is also necessary in some instances. For instance, when declaring a component with a field of python type `int`, the returned component will have a property getter and setter that performs autoconversion of the data to its underlying Int64 representation.

## Entities

### Creating an entity

```python
def main():
    world = World()
    e = world.entity()

```

Similarly to as in the component case, `world.entity()` returns an EntityId.

## Queries

In flecs, a query is built off of a prepared template called a query description. Pyflecs provides a wrapper class and builder interface with various methods you can use to easily build instances of query description:

```python
    qd1 = QueryDescriptionBuilder()     # Builder
    qd2 = QueryDescription.builder()    # Convenience method of Term
    qd3 = QueryDescription.kwargs()     # Forward args to builder via kwargs
    qd4 = QueryDescription.tuple()      # Forward term args only via tuple
```

You'll also need to utilize wrapper classes for some struct data contained inside a query description such as terms, term refs, etc. Term is wrapped the same way and provides the same kind of interfaces for construction:

```python
    t = TermBuilder()                   # A term builder object
    t = Term.builder()                  # Convenience method
    t = Term.kwargs()                   # Kwargs shorthand
    t = Term.tuple()                    # Tuple shorthand
```

### Terms

Refer to the flecs manual for an in depth look. In flecs, basically Terms are a set of 4 values, `id`, `src`, `first`, and `second`, and a bit of optional config. Because of this fact it's also very convenient to construct a Term using a tuple. Pyflecs expects you to provide a tuple with 1-4 elements of the form `(id, src, first, second)`.

#### Basic query example

```python
    @component
    class Position:
        x: float
        y: float

    qd = (
        QueryDescription.builder()
        .terms([Term.tuple((Position,))]) # Index 0 of the array is always `id`
        .build()
    )
```

### The query description tuple shorthand

If you don't need any additional config, you can construct a simple query with the QueryDescription.tuple shorthand, which expects an array of tuple `[(id,src,first,second),...]`:

```python
    @component
    class Position:
        x: float
        y: float

    qd = QueryDescription.tuple([(Position,)]) 
```

### Creating the query

Creating a query must be done through world. Some convenience methods are also provided.

```python
    @component
    class Position:
        x: float
        y: float

    # Using description
    q = world.query(QueryDescription.tuple([(Position,)]))

    # Kwargs shorthand
    q = world.query_kwargs(
        terms=[Term.tuple((Position,))]
    )

    # Terms (tuple) shorthand
    q = world.query_terms([(Position,)])
```

### Query execution

In the flecs world we have basically only one concept in regards to execution of queries, which is the iterator. Pyflecs wraps iterator in two different objects; the QueryExecutor and the QueryResult. QueryExecutor represents an iterator that is ready to be advanced, while QueryResult represents an iterator that is currently pointing at a distinct result and may not be advanced. Because QueryExecutor performs iteration, it is a python iterable and can be used simply the way the any python iterable may be used, and returns a sequence of QueryResult objects, which have accessor methods on the underlying data.

```python
    executor = q.executor()
    for result in executor:
        print(result) # type QueryResult
```


### Getting the component

```python
    @component
    class Position:
        x: float
        y: float

    # Using description
    q = world.query(QueryDescription.tuple([(Position,)]))

    executor = q.executor()

    for result in executor:
        c = result.component(0, Position) # Instance of component
        print(c.x)
        print(c.y)
```


### A warning about query result access

Note that accessing components in flecs is dependent upon struct size. When you call `result.component(0, Position)`, you're specify that you want to get index `0` of the components returned as part of this query result, and the object is of type `Position`. If you specify the wrong component object, or specify the components in the wrong order, pyflecs cannot guarantee what will happen. Take this, for example:

```python
    @component
    class Foo:
        a: int
        b: int

    @component
    class Bar:
        b: float
```

This describes two components. Component `Foo` has a computed size `sizeof(Int64) * 2`, while Bar has a single float value and is of size `sizeof(float)`. If we were to ask flecs for a `Foo` when a QueryResult contains a `Foo`, flecs will give us a chunk of memory sized `sizeof(Int64) * 2`, which is too big. This is basically a classic C buffer overrun scenario. There are no protective mechanisms in place to prevent you from doing this at the current time, although there may be in future. The bottom of line is, be careful about defining and using your types, and be aware of how the underlying data is structured.


## Systems

In flecs, systems are basically processes that execute a `run` method. Flecs also supports _iterative systems_ which bind a query together with a callback that is executed conditionally based on the results of the executed query. See the flecs documentation on systems for more information.

### Defining a basic system

pyflecs provides a declarative template syntax similar to the one used to declare components, which uses a class and a class decorator. You can also create systems in the typical manner, similar to queries, using builders and/or convenience methods, but class syntax is a good starting point for most cases.

```python
@system()
class RunSystem:
    def run(executor: QueryExecutor):
        print("I run")
```

Systems with a `run` method may also perform queries, but since they override the default `run` function, iteration must be performed manually with the provided `QueryExecutor` object. An iterative system, on the other hand, performs query execution and provides its callback with a `QueryResult` object.

```python
@system(query=QueryDescription.tuple([(Position,)]))
class IteratingSystem:
    def each(result: QueryResult):
        print("I process result")
```

### Creating a system

Creating a system must be done through world, similarly as components.

```python
    run_system_instance = world.system(RunSystem)
    iterating_system_instance = world.system(IteratingSystem)
    print(run_system_instance.id) # the EntityId
```

### Running a system

Systems must be run inside world.

```python
    world.run(system_instance)
    world.run(system_instance.id) # either works
```

## cflecs

#### Updating the autogenerated bindings

In order to regenerate cflecs, you can run:

TODO: We should have a `uv` command for this

```python
.venv/bin/ctypesgen -llibflecs.dylib ./flecs.h ./flecs.c -o cflecs.py
```

Of course, this won't be that useful without first updating to the version of flecs to which you wish to port. The easiest way to do this is to build flecs from source. Following the steps in the flecs manual in order to build with cmake, you should be able to create a distributable in shared object form, e.g. `libflecs.dylib`.

From there, you can either copy the header and source
files from flecs' `distr` directory, or, alternatively, if you're building
a version of `libflecs.dylib` that includes a customized set of flecs addons,
you can copy the appropriate files from flecs' `src` directory to generate
the minimal set of bindings that you need for whatever your purposes are.
It should be noted that pyflecs currently depends upon, and assumes, that
`cflecs.py` will contain the autogenerated bindings for the full version of
flecs, including addons.

The output will be written into `cflecs.py` and references within `pyflecs` will
automatically point to the updated bindings, so you can begin porting right
away.
