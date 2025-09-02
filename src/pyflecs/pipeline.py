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


class Phase(Enum):
    OnStart = EcsOnStart
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
