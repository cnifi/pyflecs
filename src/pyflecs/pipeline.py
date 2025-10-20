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
    struct_ecs_pipeline_desc_t,
)
from .query import QueryDescription


class Phase(Enum):
    """Builtin entity id declarations that represent pipeline phases."""

    ON_START = EcsOnStart
    PRE_FRAME = EcsPreFrame
    ON_LOAD = EcsOnLoad
    POST_LOAD = EcsPostLoad
    PRE_UPDATE = EcsPreUpdate
    ON_UPDATE = EcsOnUpdate
    ON_VALIDATE = EcsOnValidate
    POST_UPDATE = EcsPostUpdate
    PRE_STORE = EcsPreStore
    ON_STORE = EcsOnStore
    POST_FRAME = EcsPostFrame
    PHASE = EcsPhase
    CONSTANT = EcsConstant


class PipelineDescriptionBuilder:
    """Builds a pyflecs.PipelineDescription."""

    def __init__(self, query: QueryDescription | None):
        self._query = query

    def query(self, query: QueryDescription):
        self._query = query

    def build(self):
        pd = struct_ecs_pipeline_desc_t()
        if self._query is None:
            raise Exception("_query must be set")
        pd._query = self._query._value
        return PipelineDescription(pd)


class PipelineDescription:
    """A template for generating pyflecs.Pipeline objects."""

    def __init__(self, value: struct_ecs_pipeline_desc_t):
        self._value = value

    @property
    def query(self):
        return self._value.query


class Pipeline:
    """A configurable system processing pipeline."""

    def __init__(self, description: PipelineDescription):
        self._description = description

    @property
    def description(self):
        return self._description
