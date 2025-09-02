from .cflecs import ecs_strbuf_append


class String:
    def __init__(self, s=""):
        pass

    def append(self):
        ecs_strbuf_append(self._value)
