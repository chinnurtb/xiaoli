#coding=utf-8
from __future__ import absolute_import, unicode_literals

from flask import url_for

from .utils import *

from jinja2 import Markup

__dict__ = ['Action']

class Action(object):
    creation_counter = 0

    def __init__(self, name, endpoint, icon=None, attrs=None):
        self.name = name
        self.endpoint = endpoint
        self.icon = icon

        default_attrs = Attrs(a={'class': 'btn btn-small'})
        attrs = attrs or Attrs()
        if not isinstance(attrs, Attrs):
            warnings.warn('attrs must be Attrs object, not %s'
                          % type(attrs).__name__, DeprecationWarning)
            attrs = Attrs(attrs)
        default_attrs.update(attrs)
        self.attrs = default_attrs

        self.creation_counter = Action.creation_counter
        Action.creation_counter += 1
    
    def render(self, record):
        
        uri = url_for(self.endpoint, id=getattr(record, 'id', None))
        text = u'<a class="btn btn-small" href="%s">%s</a>' % (uri, self.name)
        return Markup(text)

