import flecs


class Position(flecs.Component):
  x: float
  y: float


def main():
  world = flecs.World()
  world.component(Position)

  entity = world.entity("Poopy")
  world.add(entity, Position)

  query = (
    world.query_builder()
    .term(world.query_term_builder().source(component=Position).build())
    .build()
  )

  print(entity)


#   print(ctypes.util.find_library("libflecs.dylib"))


if __name__ == "__main__":
  main()
