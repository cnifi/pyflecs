from .types import EntityId

EntityIdPair = tuple[EntityId, EntityId]


class Entity:
    def __init__(self, id: int | None):
        self._id = id

    @property
    def id(self):
        return self._id
