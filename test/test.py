from ctypes import c_wchar

from pyflecs.component import component
from pyflecs.inspect import stringify_ecs_iter_t
from pyflecs.query import QueryDescription
from pyflecs.system import system
from pyflecs.types import Double, String
from pyflecs.world import World

# from pyflecs.pipeline import Phase


@component
class Position:
    x: int
    y: int
    z: str


@system(query=QueryDescription.tuple([(Position,)]))
class EachSystem:
    def each(self, result):
        p = result.component(0, Position)
        print(p.z)


def main():
    world = World()

    world.component(Position)

    p = Position(5, 6, "FER")

    entity = world.entity()

    # world.add(entity, Position)
    world.set(entity, p)

    each_system = world.system(EachSystem)

    world.run(each_system)


if __name__ == "__main__":
    main()
#
