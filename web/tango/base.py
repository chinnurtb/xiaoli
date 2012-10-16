# coding: utf-8

if __name__ == '__main__':
    import sys
    sys.path.insert(0, '..')

import copy
from types import GeneratorType

from flask import Flask, Blueprint

from .models import Profile

from .login import current_user

"""
Get current user's profile.
"""
def user_profile(grp):
    return Profile.load(current_user.id, grp)

class Tango(Flask):
    #TODO:
    pass

    
class Page(Blueprint):
    #TODO:
    pass


class AutoIncrDict(dict):
    '''
    Used by :: [demjson.py, highcharts.py]
    
    ``` Example:
        >>> d = AutoIncrSortedDict()
        >>> d['a']['aa']['aaa'] = 'aaaa'
        >>> d['a']['aa']['aaa'] = 'AAAA'
        >>> d['b']['bb']['bbb'] = 'bbbb'
        >>> print d
        >>> {'a': {'aa': {'aaa': 'AAAA'}}, 'b': {'bb': {'bbb': 'bbbb'}}} # In Fact, it's NOT *sorted*
    '''
    
    def __getitem__(self, key):
        value = None
        if key not in self.keys():
            value = AutoIncrDict()
            dict.__setitem__(self, key, value)
        else:
            value = dict.__getitem__(self, key)
        return value
    


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


        
class AutoIncrSortedDict(SortedDict):
    '''
    Used by :: [users/models.py, class NestedDict]
    
    ```Example:
        >>> d = AutoIncrSortedDict()
        >>> d['a']['aa']['aaa'] = 'aaaa'
        >>> d['a']['aa']['aaa'] = 'AAAA'
        >>> d['b']['bb']['bbb'] = 'bbbb'
        >>> print d
        >>> {'a': {'aa': {'aaa': ['aaaa', 'AAAA']}}, 'b': {'bb': {'bbb': 'bbbb'}}} # In Fact, it's *sorted*

    '''
    def __getitem__(self, key):
        value = None
        if key not in self.keys():
            value = AutoIncrSortedDict()
            self.keyOrder.append(key)
            dict.__setitem__(self, key, value)
        else:
            value = dict.__getitem__(self, key)
        return value

    def __setitem__(self, key, value):
        if key not in self.keys():
            self.keyOrder.append(key)
        else:
            oldvalue = dict.get(self, key)
            if not isinstance(oldvalue, list):
                oldvalue = [oldvalue]
            oldvalue.append(value)
            value = oldvalue
        dict.__setitem__(self, key, value)
    
    
class NestedDict(AutoIncrSortedDict):
    ''' 接收一个 flask.Request 对象, 提取其中的所有参数, 并生成一个嵌套的字典
    具有相同 key 的 values 会得到一个由 values 组成的列表(list)
    '''
    
    def __init__(self, request=None, kv_list=None):
        if request is None and kv_list is None:
            super(NestedDict, self).__init__()
            return
            
        if kv_list is None:
            kv_list = request.values.lists()
        for k, v in kv_list:
            if k == 'csrf_token': continue
            # key 的层级用 '.' 分割
            # 例如, 给出 a.b.c=3 ,则会得到 {'a':{'b':{'c':'3'}}}
            k_chains = k.split('.')
            cur_level = self
            while len(k_chains) > 1:
                cur_level = cur_level[k_chains.pop(0)]
            if isinstance(cur_level, list):
                raise KeyError('(key : %s) already exists,so (key : %s) is not allowed' % ('.'.join(k.split('.')[:-1]),k))
            if isinstance(v, list) and len(v) == 1:
                v = v[0]
            cur_level[k_chains[0]] = v

            
# ==============================================================================
#  Unit Test
# ==============================================================================
if __name__ == '__main__':
    d = {}
    aid = AutoIncrDict()
    aisd = AutoIncrSortedDict()

    for c in range(ord('a'), ord('z')+1):
        c = chr(c)
        d[c] = c*2
        aid[c][c*2][c*3] = c*4
        aisd[c][c*2][c*3] = c*4

    def print_dict(d):
        for k, v in d.iteritems():
            print "%s:: %s" % (k, v)
            print '-------'
    print '==============================='
    print_dict(d)
    print '==============================='
    print_dict(aid)
    print '==============================='
    print_dict(aisd)
    print '==============================='
