from pyflecs.component import component
from pyflecs.query import QueryDescription
from pyflecs.system import SystemDescription
from pyflecs.world import World


@component
class BasicComponent:
    bar: int


def test_system_run():
    world = World()

    executed = False

    def once(it):
        nonlocal executed
        executed = True

    system_desc = SystemDescription.once(once)

    system_id = world.system(system_desc)

    try:
        world.run(system_id)
    finally:
        pass
        # world.delete_system(sid)

    assert executed


def _test_system_callback():
    world = World()

    foo = world.component(BasicComponent)

    entity = world.entity()
    world.add(entity, foo)

    executed = False

    def action(it):
        nonlocal executed
        executed = True

    system_id = world.system_each(
        QueryDescription.tuple(
            [
                (BasicComponent,),
            ]
        ),
        action,
    )

    try:
        world.run(system_id)
    finally:
        pass
    #        world.delete_system(sid)

    assert executed


def _test_system_kwargs():
    world = World()

    executed = False

    def action(it):
        nonlocal executed
        executed = True

    s = world.system_kwargs(run=action)

    world.run(s)

    assert executed
