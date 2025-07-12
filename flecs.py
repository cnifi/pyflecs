# https://ajmmertens.medium.com/why-it-is-time-to-start-thinking-of-games-as-databases-e7971da33ac3

import ctypes

import cflecs


class Component:
  _id: int


class QueryTermBuilderError(Exception):
  pass


# A more python-like wrapper class for struct_ecs_term_t
class QueryTerm:
  _term: cflecs.struct_ecs_term_t

  def __init__(self, term):
    self._term = term

  @property
  def id(self):
    return self._term.id

  @property
  def source(self):
    return self._term.name

  @property
  def first(self):
    return self._term.first

  @property
  def second(self):
    return self._term.second

  @property
  def traverse(self):
    return self._term.trav

  @property
  def inout(self):
    return self._term.inout

  @property
  def operator(self):
    return self._term.oper

  @property
  def field_index(self):
    return self._term.field_index


class QueryTermBuilder:
  _source: cflecs.struct_ecs_term_ref_t | None = None
  _first: cflecs.struct_ecs_term_ref_t | None = None
  _second: cflecs.struct_ecs_term_ref_t | None = None

  def _term_ref(
    self, name: str | None = None, component: type[Component] | None = None
  ):
    term_ref = cflecs.ecs_term_ref_t()
    if name is not None:
      term_ref.name = name
    elif component is not None:
      term_ref.id = component._id
    else:
      raise QueryTermBuilderError("One of name | component must be set")
    return term_ref

  def source(self, name: str | None = None, component: type[Component] | None = None):
    self._source = self._term_ref(name, component)
    return self

  def first(self, name: str | None = None, component: type[Component] | None = None):
    self._first = self._term_ref(name, component)
    return self

  def second(self, name: str, component: type[Component]):
    self._second = self._term_ref(name, component)
    return self

  def build(self):
    term = cflecs.ecs_term_t()
    term.source = self._source
    if self._first is not None:
      term.first = self._first
    if self._second is not None:
      term.second = self._second
    return QueryTerm(term)


class Query:
  """A more python-like wrapper class for struct_ecs_query_desc_t."""

  _query_desc: cflecs.struct_ecs_query_desc_t

  def __init__(self, query_desc):
    self._query_desc = query_desc

  @property
  def terms(self):
    return self._query_desc.terms

  @property
  def expr(self):
    return self._query_desc.expr

  @property
  def cache_kind(self):
    return self._query_desc.cache_kind

  @property
  def flags(self):
    return self._query_desc.flags

  @property
  def order_by_callback(self):
    return self._query_desc.order_by_callback

  def execute(self, world):
    pass


class QueryBuilder:
  _world: cflecs.struct_ecs_world_t
  _terms: list[cflecs.struct_ecs_term_t]

  def __init__(self, world):
    self._world = world
    self._terms = []

  def term_builder(self):
    return QueryTermBuilder()

  def term(self, term):
    self._terms.append(term)
    return self

  def build(self):
    query_desc = cflecs.ecs_query_desc_t()
    query_desc._canary = 0

    num_terms = len(self._terms)
    terms_carray = (cflecs.struct_ecs_term_t * num_terms)()
    for i in range(0, num_terms):
      terms_carray[i] = self._terms[i]
    query_desc.terms = terms_carray

    # query_desc.terms = ecs_term_t[FLECS_TERM_COUNT_MAX]
    # query.expr = const char* (optional)
    # query_desc.cache_kind = ecs_query_cache_kind_t
    # query_desc.flags = ecs_flags32_t
    # query_desc.order_by_callback = ecs_order_by_action_t
    # query_desc.order_by_table_callback = ecs_sort_table_action_t
    # query_desc.order_by = ecs_entity_t
    # query_desc.group_by = ecs_id_t
    # query_desc.group_by_callback = ecs_group_by_action_t
    # query_desc.on_group_create = ecs_group_create_action_t
    # query_desc.on_group_delete = ecs_group_delete_action_t
    # query_desc.group_by_ctx = void *
    # query_desc.group_by_ctx_free = ecs_ctx_free_t
    # query_desc.ctx = void *
    # query_desc.binding_ctx = void *
    # query_desc.ctx_free = ecs_ctx_free_t
    # query_desc.binding_ctx_free = ecs_ctx_free_t
    # query_desc.entity = ecs_entity_t

    return Query(query_desc)


class World:
  _world: cflecs.struct_ecs_world_t

  def __init__(self):
    self._world = cflecs.ecs_init()

  def component(self, claz: type[Component]):
    entity_desc = cflecs.struct_ecs_entity_desc_t()
    entity_desc.id = 0
    entity_desc.use_low_id = True
    comp_name = f"component:{type.__name__}"
    byte_name = cflecs.String(comp_name.encode("utf-8"))
    entity_desc.name = byte_name
    entity_desc.symbol = byte_name
    entity = cflecs.ecs_entity_init(self._world, ctypes.byref(entity_desc))

    # Create component type info
    type_info = cflecs.ecs_type_info_t()
    type_size = claz.__basicsize__
    type_info.size = type_size
    type_info.alignment = type_size
    type_info.hooks = cflecs.ecs_type_hooks_t()
    type_info.component = 0

    # Create component descriptor
    desc = cflecs.struct_ecs_component_desc_t()
    desc._canary = 0
    desc.entity = entity

    # Finally, register the component o_O
    component_id = cflecs.ecs_component_init(self._world, ctypes.byref(desc))
    claz._id = component_id
    return component_id

  def entity(self, name):
    entity_desc = cflecs.struct_ecs_entity_desc_t()
    entity_desc.id = 0
    entity_desc.use_low_id = True
    bname = cflecs.String(name.encode("utf-8"))
    entity_desc.name = bname
    entity_desc.symbol = bname
    return cflecs.ecs_entity_init(self._world, ctypes.byref(entity_desc))

  def add(self, e, component):
    cflecs.ecs_add_id(self._world, e, component._id)

  def query_builder(self):
    return QueryBuilder(self._world)

  def query_term_builder(self):
    return QueryTermBuilder()

    # T.__name__
    # term.trav = ecs_entity_t

    # term.inout = int16_t
    # term.oper = int16_t

    # term.field_index = int8_t
    # term.flags = ecs_flags16

    # return cflecs.ecs_query_init(self.world, query_desc)

  # def system(self, query, callback):
  # system_desc = cflecs.ecs_system_desc_t()
  # system_desc._canary = 0
  # system_desc.entity =

  # query_desc = cflecs.ecs_query_desc_t()

  # system_desc.query = query_desc
  # ecs_iter_action_t
  # system_desc.callback = callback

  # system_desc.run = ecs_run_action_t;

  # TODO
  # system_desc.ctx = void *
  # system_desc.ctx_free = ecs_ctx_free_t
  # system_desc.callback_ctx = void *
  # system_desc.callback_ctx_free = ecs_ctx_free_t
  # system_desc.run_ctx = void *
  # system_desc.run_ctx_free = ecs_ctx_free_t

  # Required
  # system_desc.interval = float
  # system_desc.rate = int32_t

  # TODO: is this required??
  # tick_source = ecs_entity_t

  # system_desc.multi_threaded = False
  # system_desc.immediate = False

  # return cflecs.ecs_system_init(self.world, ctypes.byref(system_desc))
  # .query.terms = {
  #     { ecs_id(Position) },
  #     { ecs_id(Velocity) },
  # },
  # .callback = Move
