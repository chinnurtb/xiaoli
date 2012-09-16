# -*- coding: utf-8 -*-
from __future__ import absolute_import, unicode_literals

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
 

class QueryForm(object):
    pass

