from xml.sax.handler import property_encoding

from .types import EntityId


class Entity:
    def __init__(self, id: EntityId | None):
        self._id = id

    @property
    def id(self):
        return self._id
