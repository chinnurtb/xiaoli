#!/usr/bin/env python
# -*- coding: utf-8 -*-
from __future__ import absolute_import, unicode_literals

if __name__ == '__main__':
    import sys
    sys.path.insert(0,  '../..')
    from webapp import db
    
from flask_wtf import (TextField, IntegerField, SelectFieldPro, DateField, HiddenField)
from tango.base import NestedDict
from nodes.models import Node

# class QueryFilter(object):
#     operators = {"="     :   'equals',
#                  "!"     :   'not_equals',
#                  "o"     :   'label_open_issues',
#                  "c"     :   'closed_issues',
#                  "!*"    :   'none',
#                  "*"     :   'all',
#                  ">="    :   'greater_or_equal',
#                  "<="    :   'less_or_equal',
#                  "><"    :   'between',
#                  "<t+"   :   'in_less_than',
#                  ">t+"   :   'in_more_than',
#                  "t+"    :   'in',
#                  "t"     :   'today',
#                  "w"     :   'this_week',
#                  ">t-"   :   'less_than_ago',
#                  "<t-"   :   'more_than_ago',
#                  "t-"    :   'ago',
#                  "~"     :   'contains',
#                  "!~"    :   'not_contains' }

#     def __init__(self, field, operator, values):
#         self.field = field
#         self.operator = operator
#         self.values = values


class QueryField(object):
    pass
    

class QueryForm(object):
    def __init__(self,):
        pass
    

# class QueryColumn(object):
#     def __init__(self, name, **kwargs):
#         self.name = name
#         self.sortable = kwargs.get('sortable', False)
#         self.groupable = kwargs.get('groupable', False)
#         self.default_order = kwargs.get('default_order', None)


class Filter(object):
    operators = {
        '>'  : '>',
        '<'  : '<',
        '>=' : '>=',
        '<=' : '<=',
        '==' : '=',
        '!=' : '!=',
        
        'in' : 'IN',
        'not_in' : 'NOT IN',
        'like' : 'LIKE',
        'not_like' : 'NOT LIKE',
        'ilike' : 'ILIKE',
        'not_ilike' : 'NOT ILIKE',
        
        #### DateTime
        # 'today' : '',
        # 'this_week' : '',
        # 'less_than_ago' : '',
        # 'more_than_ago' : '',
    }

    def __init__(self, field_prefix, kwargs):
        self.field_prefix = field_prefix
        for k, v in kwargs.iteritems():
            setattr(self, k, v) # keys:: [operator, value, accessor]

            
    def to_str(self):
        value = None
        if self.operator in ['in', 'not_in']:
            if not isinstance(self.value, list):
                self.value = [self.value]
            value = '(%s)' % ', '.join(["'%s'" % v for v in self.value])
        elif self.operator in ['like', 'ilike', 'not_like', 'not_ilike']:
            value = "'%%%s%%'" % self.value
        else:
            value = "'%s'" % self.value

        return ' '.join([self.field_prefix, Filter.operators[self.operator], value])

        
class Filters(object):

    def __init__(self, model=None, request=None, kv_list=None, key_prefix='filters'):
        self.model = model
        tablename = model.__tablename__
        arg_dict =  NestedDict(request, kv_list=kv_list)[key_prefix]
        self.arg_dict = arg_dict
        self.filters = []
        
        for field_name, items in arg_dict.iteritems():
            field_prefix = '.'.join([tablename, field_name])
            filter = Filter(field_prefix, items)
            self.filters.append(filter)
            
            
    def to_str(self):
        return ' AND '.join([filter.to_str() for filter in self.filters])

# ==============================================================================
#  Unit Test
# ==============================================================================
if __name__ == '__main__':
    kv_list = [
        ('filters.addr.operator' , 'ilike'),
        ('filters.addr.value', '127.0.0.1'),
        
        ('filters.name.operator', 'ilike'),
        ('filters.name.value', 'ad'),
        
        ('filters.area_id.operator', 'in'),
        ('filters.area_id.value', '1001'),
        ('filters.area_id.value', '1002'),
        ('filters.area_id.value', '1003'),
        ('filters.area_id.value', '1004'),
        ('filters.area_id.value', '1005'),
        ('filters.area_id.value', '1010'),
        
        ('filters.vendor_id.operator', '=='),
        ('filters.vendor_id.value', '1'),

        # ('filters.area_name.operator', '=='),
        # ('filters.area_name.accessor', 'area.name')
        # ('filters.area_name.value', u'电信大楼'),
        
        ('filters.model_id.operator', '=='),
        ('filters.model_id.value', '9'),
    ]

    filters = Filters(model=Node, kv_list=kv_list)
    print filters.to_str()
    print [node.id for node in Node.query.filter(filters.to_str()).all()]

