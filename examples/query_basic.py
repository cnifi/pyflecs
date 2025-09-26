from pyflecs.component import component
from pyflecs.query import QueryDescription
from pyflecs.system import system
from pyflecs.world import World


@component
class Position:
    x: int
    y: int
    z: str


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

    s = world.system_once(lambda it: print("HELLO"))
    # s = world.system(RunSystem)

    world.run(s)

    # world.delete(e)


if __name__ == "__main__":
    main()
