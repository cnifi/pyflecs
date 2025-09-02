from pyflecs.component import component
from pyflecs.inspect import stringify_ecs_iter_t
from pyflecs.query import QueryDescription
from pyflecs.system import system
from pyflecs.types import Double
from pyflecs.world import World

# from pyflecs.pipeline import Phase


@component
class Position:
    x: Double
    y: Double


@system(query=QueryDescription.tuple([(Position,)]))
class EachSystem:
    def each(self, iter):
        p = iter.component(0, Position)
        print(p.x)
        print(p.y)


def main():
    world = World()

    world.component(Position)

    entity = world.entity()

    print(entity)

    world.add(entity, Position)
    world.set(entity, Position(5, 5))

    each_system = world.system(EachSystem)

    world.run(each_system)


if __name__ == "__main__":
    main()
#
