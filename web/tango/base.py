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

    def __init__(self, request):
        pass

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
        
