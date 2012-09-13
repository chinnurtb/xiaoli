#!/usr/bin/env python
#coding=utf-8

from flask import Flask, Blueprint
from tango.ui.tables.utils import SortedDict

class Tango(Flask):
    #TODO:
    pass

    
class Page(Blueprint):
    #TODO:
    pass


class AutoIncrSortedDict(SortedDict):
    '''See the name, then you know it.'''
    def __getitem__(self, key):
        value = None
        if key not in self.keys():
            value = AutoIncrSortedDict()
            super(AutoIncrSortedDict, self).__setitem__(key, value)
        else:
            value = super(AutoIncrSortedDict, self).__getitem__(key)
        return value

    def __setitem__(self, key, value):
        oldvalue = super(AutoIncrSortedDict, self).get(key, None)
        if oldvalue:
            if not isinstance(oldvalue, list):
                oldvalue = [oldvalue]
            oldvalue.append(value)
            value = oldvalue
        super(AutoIncrSortedDict, self).__setitem__(key, value)
    
    
class NestedDict(AutoIncrSortedDict):
    ''' 接收一个 flask.Request 对象, 提取其中的所有参数, 并生成一个嵌套的字典
    具有相同 key 的 values 会得到一个由 values 组成的列表(list)
    '''
    
    def __init__(self, request):
        if request is None:
            super(NestedDict, self).__init__()
            return
        kv_lists = request.values.lists()
        for k, v in kv_lists:
            if k == 'csrf_token' or v == '': continue
            # key 的层级用 '.' 分割
            # 例如, 给出 a.b.c=3 ,则会得到 {'a':{'b':{'c':'3'}}}
            k_chains = k.split('.')
            cur_level = self
            while len(k_chains) > 1:
                cur_level = cur_level[k_chains.pop(0)]
            if isinstance(cur_level, list):
                raise KeyError('(key : %s) already exists,so (key : %s) is not allowed' %
                               ('.'.join(k.split('.')[:-1]),k))
            cur_level[k_chains[0]] = v

            
        
