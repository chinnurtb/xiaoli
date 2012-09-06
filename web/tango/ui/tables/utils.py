#coding=utf-8

import re
import copy
import inspect
from types import GeneratorType
from itertools import chain, ifilter

from jinja2 import Markup
from jinja2.filters import escape

__dict__ = ['getargspec', 'title', 'Accessor', 'A', 'AttributeDict', 'Attrs',
            'Sequence', 'SortedDict']


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
    def as_html(self, **kwargs):
        new_attrs = self
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
        for key in ('th', 'td'):
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


class SortedDict(dict):
    """
    A dictionary that keeps its keys in the order in which they're inserted.
    """
    def __new__(cls, *args, **kwargs):
        instance = super(SortedDict, cls).__new__(cls, *args, **kwargs)
        instance.keyOrder = []
        return instance

    def __init__(self, data=None):
        if data is None:
            data = {}
        elif isinstance(data, GeneratorType):
            # Unfortunately we need to be able to read a generator twice.  Once
            # to get the data into self with our super().__init__ call and a
            # second time to setup keyOrder correctly
            data = list(data)
        super(SortedDict, self).__init__(data)
        if isinstance(data, dict):
            self.keyOrder = data.keys()
        else:
            self.keyOrder = []
            seen = set()
            for key, value in data:
                if key not in seen:
                    self.keyOrder.append(key)
                    seen.add(key)

    def __deepcopy__(self, memo):
        return self.__class__([(key, copy.deepcopy(value, memo))
                               for key, value in self.iteritems()])

    def __copy__(self):
        # The Python's default copy implementation will alter the state
        # of self. The reason for this seems complex but is likely related to
        # subclassing dict.
        return self.copy()

    def __setitem__(self, key, value):
        if key not in self:
            self.keyOrder.append(key)
        super(SortedDict, self).__setitem__(key, value)

    def __delitem__(self, key):
        super(SortedDict, self).__delitem__(key)
        self.keyOrder.remove(key)

    def __iter__(self):
        return iter(self.keyOrder)

    def pop(self, k, *args):
        result = super(SortedDict, self).pop(k, *args)
        try:
            self.keyOrder.remove(k)
        except ValueError:
            # Key wasn't in the dictionary in the first place. No problem.
            pass
        return result

    def popitem(self):
        result = super(SortedDict, self).popitem()
        self.keyOrder.remove(result[0])
        return result

    def items(self):
        return zip(self.keyOrder, self.values())

    def iteritems(self):
        for key in self.keyOrder:
            yield key, self[key]

    def keys(self):
        return self.keyOrder[:]

    def iterkeys(self):
        return iter(self.keyOrder)

    def values(self):
        return map(self.__getitem__, self.keyOrder)

    def itervalues(self):
        for key in self.keyOrder:
            yield self[key]

    def update(self, dict_):
        for k, v in dict_.iteritems():
            self[k] = v

    def setdefault(self, key, default):
        if key not in self:
            self.keyOrder.append(key)
        return super(SortedDict, self).setdefault(key, default)

    def value_for_index(self, index):
        """Returns the value of the item at the given zero-based index."""
        return self[self.keyOrder[index]]

    def insert(self, index, key, value):
        """Inserts the key, value pair before the item with the given index."""
        if key in self.keyOrder:
            n = self.keyOrder.index(key)
            del self.keyOrder[n]
            if n < index:
                index -= 1
        self.keyOrder.insert(index, key)
        super(SortedDict, self).__setitem__(key, value)

    def copy(self):
        """Returns a copy of this object."""
        # This way of initializing the copy means it works for subclasses, too.
        return self.__class__(self)

    def __repr__(self):
        """
        Replaces the normal dict.__repr__ with a version that returns the keys
        in their sorted order.
        """
        return '{%s}' % ', '.join(['%r: %r' % (k, v) for k, v in self.items()])

    def clear(self):
        super(SortedDict, self).clear()
        self.keyOrder = []

