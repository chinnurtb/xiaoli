#!/usr/bin/env python
# -*- coding: utf-8 -*-
from __future__ import absolute_import, unicode_literals

if __name__ == '__main__':
    import sys
    sys.path.insert(0,  '../..')

from tango.base import NestedDict

class QueryFilter(object):
    operators = {"="     :   'equals',
                 "!"     :   'not_equals',
                 "o"     :   'label_open_issues',
                 "c"     :   'closed_issues',
                 "!*"    :   'none',
                 "*"     :   'all',
                 ">="    :   'greater_or_equal',
                 "<="    :   'less_or_equal',
                 "><"    :   'between',
                 "<t+"   :   'in_less_than',
                 ">t+"   :   'in_more_than',
                 "t+"    :   'in',
                 "t"     :   'today',
                 "w"     :   'this_week',
                 ">t-"   :   'less_than_ago',
                 "<t-"   :   'more_than_ago',
                 "t-"    :   'ago',
                 "~"     :   'contains',
                 "!~"    :   'not_contains' }

    def __init__(self, field, operator, values):
        self.field = field
        self.operator = operator
        self.values = values


class QueryColumn(object):
    def __init__(self, name, **kwargs):
        self.name = name
        self.sortable = kwargs.get('sortable', False)
        self.groupable = kwargs.get('groupable', False)
        self.default_order = kwargs.get('default_order', None)

    
class Filter(object):
    operators = {
        '>'  : '__gt__',
        '<'  : '__lt__',
        '>=' : '__ge__',
        '<=' : '__le__',
        '==' : '__eq__',
        '!=' : '__ne__',
        'in' : 'in_',
        'not_in' : '',
        'like' : 'like',
        'not_like' : '',
        'ilike' : 'ilike',
        'not_ilike' : '',
        # DateTime
        'today' : '',
        'this_week' : '',
        'less_than_ago' : '',
        'more_than_ago' : '',
    }

    def __init__(self, model=None, request=None, kv_list=None, key_prefix='filters'):
        self.model = model
        self.arg_dict = NestedDict(request, kv_list=kv_list)[key_prefix]
        

if __name__ == '__main__':
    kv_list = [
        ('filters.addr.operator' , '=='),
        ('filters.addr.value', '127.0.0.1'),
        
        ('filters.name.operator', 'ilike'),
        ('filters.name.value', 'ad'),
        
        ('filters.area_id.operator', 'in'),
        ('filters.area_id.value', '1001'),
        ('filters.area_id.value', '1002'),
        ('filters.area_id.value', '1003'),
        ('filters.area_id.value', '1004'),
        ('filters.area_id.value', '1005'),
        
        ('filters.vendor_id.operator', '=='),
        ('filters.vendor_id.value', '1'),

        # ('filters.area_name.operator', '=='),
        # ('filters.area_name.accessor', 'area.name')
        # ('filters.area_name.value', u'电信大楼'),
        
        ('filters.model_id.operator', '=='),
        ('filters.model_id.value', '9'),
    ]

    from tango.ui.tables.utils import A
    qfilter = Filter(kv_list=kv_list)
    for k, v in qfilter.arg_dict.iteritems():
        accessor = A(k)
        field = accessor.resolve(qfilter.model)
        if v['operator'] in ['==', '!=', '>', '<', '>=', '<=']:
            
            pass
        print k
        print v
        print '-----'
