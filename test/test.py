from pyflecs.component import component
from pyflecs.query import QueryDescription
from pyflecs.system import system
from pyflecs.world import World


@component
class Position:
    x: int
    y: int
    z: str


@component
class Bar:
    x: float


@system(query=QueryDescription.tuple([(Position,)]))
class EachSystem:
    def each(self, result):
        c = result.component(0, Position)
        print(c.x)
        print(c.y)
        print(c.z)


def main():
    world = World()

    world.component(Position)
    # world.component(Bar)

    entity = world.entity()

    p = Position()
    p.x = 1
    p.y = 7
    p.z = "HEO"

    world.set(entity, p)

    each_system = world.system(EachSystem)

    world.run(each_system)


if __name__ == "__main__":
    main()
