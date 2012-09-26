# -*- coding: utf-8 -*-

import re
import copy
import inspect

from itertools import chain, ifilter

from jinja2 import Markup
from jinja2.filters import escape

__dict__ = ['getargspec', 'title', 'Accessor', 'A', 'AttributeDict', 'Attrs', 'Sequence' ]


## getargspec
def curry(_curried_func, *args, **kwargs):
    def _curried(*moreargs, **morekwargs):
        return _curried_func(*(args+moreargs), **dict(kwargs, **morekwargs))
    return _curried

funcs = ifilter(curry(hasattr, inspect), ('getfullargspec', 'getargspec'))
getargspec = getattr(inspect, next(funcs))
del funcs


## title
def django_title(value):
    """Converts a string into titlecase."""
    t = re.sub("([a-z])'([A-Z])", lambda m: m.group(0).lower(), value.title())
    return re.sub("\d([A-Z])", lambda m: m.group(0).lower(), t)


RE_UPPERCASE = re.compile('[A-Z]')

def title(value):
    title_word = lambda w: w if RE_UPPERCASE.search(w) else django_title(w)
    return re.sub('(\S+)', lambda m: title_word(m.group(0)), value)


class Accessor(str):
    SEPARATOR = '.'

    def resolve(self, context, safe=True):
        current = context
        for bit in self.bits:
            try:  # dictionary lookup
                current = current[bit]
            except (TypeError, AttributeError, KeyError):
                try:  # attribute lookup
                    current = getattr(current, bit)
                except (TypeError, AttributeError):
                    try:  # list-index lookup
                        current = current[int(bit)]
                    except (IndexError,  # list index out of range
                            ValueError,  # invalid literal for int()
                            KeyError,    # dict without `int(bit)` key
                            TypeError,   # unsubscriptable object
                            ):
                        raise ValueError('Failed lookup for key [%s] in %r'
                                         ', when resolving the accessor %s'
                                          % (bit, current, self))
            if callable(current):
                if safe and getattr(current, 'alters_data', False):
                    raise ValueError('refusing to call %s() because `.alters_data = True`'
                                     % repr(current))
                current = current()
            if current is None:
                break
        return current

    @property
    def bits(self):
        if self == '':
            return ()
        return self.split(self.SEPARATOR)


A = Accessor  # alias


class AttributeDict(dict):
    def as_html(self, order=True, **kwargs):
        new_attrs = self
        if not order:
            all_class = new_attrs.pop('class','').replace('orderable','').replace('sortable','')
            new_attrs['class'] = all_class
        if kwargs:
            new_attrs = copy.deepcopy(self)
            AttributeDict.merge(new_attrs, kwargs)
        return Markup(' '.join(['%s="%s"' % (k, escape(v))
                                for k, v in new_attrs.iteritems()]))

    @staticmethod
    def merge(default, extra):
        """ 此方法对default参数有副作用 """
        all_class = default.pop('class', '') + ' ' + extra.pop('class', '')
        default['class'] = all_class
        default.update(extra)
        return default

        
class Attrs(dict):
    """
    """
    def update(self, extra):
        for key in ('th', 'td', 'table'):
            self[key] = AttributeDict.merge(self.get(key, {}), extra.get(key, {}))




class Sequence(list):

    def expand(self, columns):

        ellipses = self.count("...")
        if ellipses > 1:
            raise ValueError("'...' must be used at most once in a sequence.")
        elif ellipses == 0:
            self.append("...")

        # everything looks good, let's expand the "..." item
        columns = columns[:]  # don't modify
        head = []
        tail = []
        target = head  # start by adding things to the head
        for name in self:
            if name == "...":
                # now we'll start adding elements to the tail
                target = tail
                continue
            target.append(name)
            if name in columns:
                columns.pop(columns.index(name))
        self[:] = chain(head, columns, tail)


