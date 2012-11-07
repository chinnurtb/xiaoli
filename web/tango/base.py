# coding: utf-8

if __name__ == '__main__':
    import sys
    sys.path.insert(0, '..')

from flask import Flask, Blueprint

from tango.ui.tables.utils import SortedDict

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
