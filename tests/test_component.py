from pyflecs.component import component
from pyflecs.types import Double, UInt32
from pyflecs.world import World


@component
class BlankComponent:
    pass


@component
class UInt32Component:
    foo: UInt32


@component
class IntComponent:
    foo: int


@component
class StringComponent:
    foo: str


@component
class CompositeComponent:
    foo: int
    bar: str
    baz: Double


def test_blank_component():
    world = World()
    component = world.component(BlankComponent)
    entity = world.entity()
    world.add(entity, component)
    # TODO
    # instance = BlankComponent()
    # world.set(entity, instance)


def test_uint32_component():
    world = World()
    component = world.component(UInt32Component)
    entity = world.entity()
    world.add(entity, component)
    instance = UInt32Component()
    instance.foo = 5
    world.set(entity, component, instance)


def test_int_component():
    world = World()
    component = world.component(IntComponent)
    entity = world.entity()
    world.add(entity, component)
    instance = IntComponent()
    instance.foo = 5
    world.set(entity, component, instance)


def test_string_component():
    world = World()
    component = world.component(StringComponent)
    entity = world.entity()
    world.add(entity, component)
    instance = StringComponent()
    instance.foo = "bar"
    world.set(entity, component, instance)


def test_composite_component():
    world = World()
    component = world.component(CompositeComponent)
    entity = world.entity()
    world.add(entity, component)
    inst = CompositeComponent()
    inst.foo = 5
    inst.bar = "bar"
    inst.baz = 5.1
    world.set(entity, component, inst)
    inst2 = world.get(entity, component)
    assert inst.foo == inst2.foo
    assert inst.bar == inst2.bar
    assert inst.baz == inst2.baz
