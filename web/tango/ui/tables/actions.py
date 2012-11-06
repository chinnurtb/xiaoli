# coding: utf-8

from __future__ import absolute_import, unicode_literals

from flask import url_for

from .utils import *

from jinja2 import Markup

__all__ = ['Action']

class Action(object):
    creation_counter = 0

    def __init__(self, name, endpoint=None, icon=None, modalable=False, attrs=None, url=None):
        self.name = name
        self.endpoint = endpoint
        self.icon = icon
        self.url = url

        css_class = 'btn btn-small '
        if modalable:
            css_class += 'modal-btn'
        default_attrs = Attrs(a={'class': css_class})
        attrs = attrs or Attrs()
        if not isinstance(attrs, Attrs):
            warnings.warn('attrs must be Attrs object, not %s'
                          % type(attrs).__name__, DeprecationWarning)
            attrs = Attrs(attrs)
        # print 'B', default_attrs
        default_attrs.update(attrs)
        # print 'E', default_attrs
        self.attrs = default_attrs

        self.creation_counter = Action.creation_counter
        Action.creation_counter += 1
    
    def render(self, record):
        attrs = AttributeDict(self.attrs.get('a', {}))
        if self.url:
            uri = self.url(record)
        else:
            uri = url_for(self.endpoint, id=getattr(record, 'id', None))
        text = u'<a %s href="%s">%s</a>' % (attrs.as_html(), uri, self.name)
        return Markup(text)

