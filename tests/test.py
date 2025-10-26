from pyflecs.component import component
from pyflecs.query import QueryDescription
from pyflecs.system import system
from pyflecs.world import World


@component
class Position:
    x: int
    y: int
    z: str


@system(query=QueryDescription.tuple([(Position,)]))
class EachSystem:
    def each(self, result):
        c = result.component(0, Position)
        print(c.x)
        print(c.y)


@system()
class RunSystem:
    def run(self, it):
        print("GOODBYE")


def main():
    world = World()

    world.component(Position)

    e = world.entity()

    p = Position()
    p.x = 1
    p.y = 7
    p.z = "HEO"

    world.set(e, p)

    sid, _ = world.system_once(lambda it: print("HELLO"))

    world.run(sid)

    world.delete(e)


if __name__ == "__main__":
    main()
