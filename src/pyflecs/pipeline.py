from enum import Enum

from .cflecs import (
    EcsConstant,
    EcsOnLoad,
    EcsOnStart,
    EcsOnStore,
    EcsOnUpdate,
    EcsOnValidate,
    EcsPhase,
    EcsPostFrame,
    EcsPostLoad,
    EcsPostUpdate,
    EcsPreFrame,
    EcsPreStore,
    EcsPreUpdate,
)
from .query import QueryDescription


class Phase(Enum):
    ON_START = EcsOnStart
    PRE_FRAME = EcsPreFrame
    PreFrame = EcsPreFrame
    OnLoad = EcsOnLoad
    PostLoad = EcsPostLoad
    PreUpdate = EcsPreUpdate
    OnUpdate = EcsOnUpdate
    OnValidate = EcsOnValidate
    PostUpdate = EcsPostUpdate
    PreStore = EcsPreStore
    OnStore = EcsOnStore
    PostFrame = EcsPostFrame
    Phase = EcsPhase
    Constant = EcsConstant


class PipelineBuilder:
    def __init__(self, query: QueryDescription | None):
        self._query = query

    def query(self, query: QueryDescription):
        self._query = query

    def build(self):
        pass


class Pipeline:
    pass
