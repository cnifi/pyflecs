from enum import Enum

from .cflecs import (
    EcsAcyclic,
    EcsAlias,
    EcsAny,
    EcsCanToggle,
    EcsChildOf,
    EcsComponent,
    EcsConstant,
    EcsDefaultChildComponent,
    EcsDelete,
    EcsDependsOn,
    EcsDisabled,
    EcsDontFragment,
    EcsDontInherit,
    EcsEmpty,
    EcsExclusive,
    EcsFinal,
    EcsFlecs,
    EcsFlecsCore,
    EcsIdentifier,
    EcsInherit,
    EcsInheritable,
    EcsIsA,
    EcsModule,
    EcsMonitor,
    EcsName,
    EcsNotQueryable,
    EcsObserver,
    EcsOnAdd,
    EcsOnDelete,
    EcsOnDeleteTarget,
    EcsOneOf,
    EcsOnInstantiate,
    EcsOnLoad,
    EcsOnRemove,
    EcsOnSet,
    EcsOnStart,
    EcsOnStore,
    EcsOnTableCreate,
    EcsOnTableDelete,
    EcsOnUpdate,
    EcsOnValidate,
    EcsOrderedChildren,
    EcsOverride,
    EcsPairIsTag,
    EcsPanic,
    EcsPhase,
    EcsPoly,
    EcsPostFrame,
    EcsPostLoad,
    EcsPostUpdate,
    EcsPredEq,
    EcsPredLookup,
    EcsPredMatch,
    EcsPrefab,
    EcsPreFrame,
    EcsPreStore,
    EcsPreUpdate,
    EcsPrivate,
    EcsQuery,
    EcsRateFilter,
    EcsReflexive,
    EcsRelationship,
    EcsRemove,
    EcsScopeClose,
    EcsScopeOpen,
    EcsSlotOf,
    EcsSparse,
    EcsSymbol,
    EcsSymmetric,
    EcsSystem,
    EcsTarget,
    EcsThis,
    EcsTickSource,
    EcsTimer,
    EcsTrait,
    EcsTransitive,
    EcsTraversable,
    EcsVariable,
    EcsWildcard,
    EcsWith,
    EcsWorld,
)


class Builtins(Enum):
    Component = EcsComponent
    Id = EcsIdentifier
    Poly = EcsPoly
    DefaultChildComponent = EcsDefaultChildComponent
    Query = EcsQuery
    Observer = EcsObserver
    System = EcsSystem
    TickSource = EcsTickSource
    # PIPELINE_QUERY = ecs_id(EcsPipelineQuery)
    TIMER = EcsTimer
    RATE_FILTER = EcsRateFilter
    FLECS = EcsFlecs
    FLECS_CORE = EcsFlecsCore
    WORLD = EcsWorld
    WILDCARD = EcsWildcard
    ANY = EcsAny
    THIS = EcsThis
    VARIABLE = EcsVariable
    TRANSITIVE = EcsTransitive
    REFLEXIVE = EcsReflexive
    FINAL = EcsFinal
    INHERITABLE = EcsInheritable
    ON_INSTANTIATE = EcsOnInstantiate
    OVERRIDE = EcsOverride
    INHERIT = EcsInherit
    DONT_INHERIT = EcsDontInherit
    SYMMETRIC = EcsSymmetric
    EXCLUSIVE = EcsExclusive
    ACYCLIC = EcsAcyclic
    TRAVERSABLE = EcsTraversable
    WITH = EcsWith
    ONE_OF = EcsOneOf
    CAN_TOGGLE = EcsCanToggle
    TRAIT = EcsTrait
    RELATIONSHIP = EcsRelationship
    TARGET = EcsTarget
    PAIR_IS_TAG = EcsPairIsTag
    NAME = EcsName
    SYMBOL = EcsSymbol
    ALIAS = EcsAlias
    CHILD_OF = EcsChildOf
    IS_A = EcsIsA
    DEPENDS_ON = EcsDependsOn
    SLOT_OF = EcsSlotOf
    ORDERED_CHILDREN = EcsOrderedChildren
    MODULE = EcsModule
    PRIVATE = EcsPrivate
    PREFAB = EcsPrefab
    DISABLED = EcsDisabled
    NOT_QUERYABLE = EcsNotQueryable
    ON_ADD = EcsOnAdd
    ON_REMOVE = EcsOnRemove
    ON_SET = EcsOnSet
    MONITOR = EcsMonitor
    ON_TABLE_CREATE = EcsOnTableCreate
    ON_TABLE_DELETE = EcsOnTableDelete
    ON_DELETE = EcsOnDelete
    ON_DELETE_TARGET = EcsOnDeleteTarget
    REMOVE = EcsRemove
    DELETE = EcsDelete
    PANIC = EcsPanic
    SPARSE = EcsSparse
    DONT_FRAGMENT = EcsDontFragment
    PRED_EQ = EcsPredEq
    PRED_MATCH = EcsPredMatch
    PRED_LOOKUP = EcsPredLookup
    SCOPE_OPEN = EcsScopeOpen
    SCOPE_CLOSE = EcsScopeClose
    EMPTY = EcsEmpty
    # It seems like these are only used in pipeline so they are declared there
    # PIPELINE = ecs_id(EcsPipeline) #< Pipeline component id.
    # ON_START = EcsOnStart
    # PRE_FRAME = EcsPreFrame
    # ON_LOAD = EcsOnLoad
    # POST_LOAD = EcsPostLoad
    # PRE_UPDATE = EcsPreUpdate
    # ON_UPDATE = EcsOnUpdate
    # ON_VALIDATE = EcsOnValidate
    # POST_UPDATE = EcsPostUpdate
    # PRE_STORE = EcsPreStore
    # ON_STORE = EcsOnStore
    # POST_FRAME = EcsPostFrame
    # PHASE = EcsPhase
    # CONSTANT = EcsConstant
