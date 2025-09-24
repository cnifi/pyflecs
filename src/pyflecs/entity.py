EntityId = int

EntityIdPair = tuple[int, int]


class Entity:
    def __init__(self, id: int | None):
        self._id = id

    @property
    def id(self):
        return self._id
