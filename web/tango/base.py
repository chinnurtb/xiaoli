#!/usr/bin/env python
#coding=utf-8

from flask import Flask, Blueprint

class Tango(Flask):
    #TODO:
    pass

class Page(Blueprint):
    #TODO:
    pass

class NestedDict(dict):

    def __init__(self, request=None):
        if request is None:
            super(NestedDict, self).__init__()
            return
        kv_lists = request.values.lists()
        for k, v in kv_lists:
            if k == 'csrf_token' or v == '': continue
            k_chains = k.split('.')
            cur_level = self
            while len(k_chains) > 1:
                cur_level = cur_level[k_chains.pop(0)]
            if isinstance(cur_level, list):
                raise KeyError('(key : %s) already exists,so (key : %s) is not allowed' %
                               ('.'.join(k.split('.')[:-1]),k))
            cur_level[k_chains[0]] = v

            
    def __getitem__(self, key):
        value = None
        if key not in self.keys():
            value = NestedDict()
            super(NestedDict, self).__setitem__(key, value)
        else:
            value = super(NestedDict, self).__getitem__(key)
        return value

    def __setitem__(self, key, value):
        oldvalue = super(NestedDict, self).get(key, None)
        if oldvalue:
            if not isinstance(oldvalue, list):
                oldvalue = [oldvalue]
            oldvalue.append(value)
            value = oldvalue
        super(NestedDict, self).__setitem__(key, value)
        
