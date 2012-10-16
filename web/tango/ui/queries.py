#!/usr/bin/env python
# -*- coding: utf-8 -*-
from __future__ import absolute_import, unicode_literals

if __name__ == '__main__':
    import sys
    sys.path.insert(0,  '../..')
    from webapp import db

from flask import request
from jinja2 import Markup
from wtforms import widgets
from tango.base import NestedDict, SortedDict
from tango.models import QueryFilter
from tango.login import current_user
from nodes.models import Node

        
class Flags(object):
    """
    Holds a set of boolean flags as attributes.

    Accessing a non-existing attribute returns False for its value.
    """
    def __getattr__(self, name):
        if name.startswith('_'):
            return super(Flags, self).__getattr__(name)
        return False

    def __contains__(self, name):
        return getattr(self, name)

    def __repr__(self):
        flags = (name for name in dir(self) if not name.startswith('_'))
        return '<wtforms.fields.Flags: {%s}>' % ', '.join(flags)

        
class Label(object):
    """
    An HTML form label.
    """
    def __init__(self, field_id, text):
        self.field_id = field_id
        self.text = text

    def __str__(self):
        return self()

    def __unicode__(self):
        return self()

    def __html__(self):
        return self()

    def __call__(self, text=None, **kwargs):
        kwargs['for'] = self.field_id
        attributes = widgets.html_params(**kwargs)
        return widgets.HTMLString('<label %s>%s</label>' % (attributes, text or self.text))

    def __repr__(self):
        return 'Label(%r, %r)' % (self.field_id, self.text)


class Field(object):

    def __new__(cls, *args, **kwargs):
        if kwargs.get('_form', None):
            return super(Field, cls).__new__(cls)
        else:
            return UnboundField(cls, *args, **kwargs)
            
    
    def __init__(self, label=None, operator=None, default=None, id=None, _name=None, _form=None):

        self.type = 'value'
        # is HiddenField
        if label is None:
            self.type = 'operator'
        elif operator is None:
            raise ValueError('*operator* required!')

        if self.type == 'value':
            self.operator = HiddenField(default=operator, _name=_name, _form=_form)
            
        self.data = default
        self.name = '.'.join([Filters.prefix, _name, self.type])
        self.id = id or self.name

        self.label = Label(self.id, label if label is not None else _name.replace('_', ' ').title())
        self.flags = Flags()
        self.flags.required = False
        self.errors = None
        
    def process_data(self, value):
        self.data = value
        
    def _value(self):
        return self.data if self.data is not None else ''
        
    def __call__(self, **kwargs):
        return Markup(self.operator() + self.widget(self, **kwargs))


class UnboundField(object):
    creation_counter = 0
    
    def __init__(self, field_class, *args, **kwargs):
        UnboundField.creation_counter += 1
        self.field_class = field_class
        self.args = args
        self.kwargs = kwargs
        self.name = kwargs.pop('name', None)
        self.creation_counter = UnboundField.creation_counter

    def bind(self, form, name, **kwargs):
        return self.field_class(_form=form, _name=name, *self.args, **dict(self.kwargs, **kwargs))

    def __repr__(self):
        return '<UnboundField(%s, %r, %r)>' % (self.field_class.__name__, self.args, self.kwargs)
        
        
class TextField(Field):
    widget = widgets.TextInput()
    

class HiddenField(TextField):
    widget = widgets.HiddenInput()
    
    def __call__(self, **kwargs):
        return self.widget(self, **kwargs)
    

class SelectField(Field):
    widget = widgets.Select()

    def __init__(self, label=None, **kwargs):
        choices = kwargs.pop('choices', None)
        if choices is None:
            raise ValueError('*choices* required!')
        self.choices = choices() if callable(choices) else choices
        super(SelectField, self).__init__(label=label, **kwargs)

    def iter_choices(self):
        for value, label in self.choices:
            yield (value, label, unicode(value) == self.data)

        
class IntegerField(Field):
    widget = widgets.TextInput()

    def process_data(self, value):
        self.data = int(value)
        
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
        if not self.value: return None
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
    prefix = 'filters'

    def __init__(self, model=None, request=None, kv_list=None, arg_dict=None, key_prefix=prefix):
        self.model = model
        tablename = model.__tablename__
        if arg_dict is None:
            arg_dict =  NestedDict(request, kv_list=kv_list).get(key_prefix, {})
        self.arg_dict = arg_dict
        self.filters = []

        for field_name, items in arg_dict.iteritems():
            field_prefix = '.'.join([tablename, field_name])
            filter = Filter(field_prefix, items) # items = {'operator': '...', 'value':['a','b']}
            self.filters.append(filter)

    def __iter__(self):
        return iter(self.arg_dict.keys())

    def iteritems(self):
        return self.arg_dict.iteritems()
            
    def to_str(self):
        query_strs = []
        for filter in self.filters:
            query_str = filter.to_str()
            if query_str:
                query_strs.append(query_str)
        return ' AND '.join(query_strs)

        
        
class FormMeta(type):
    def __new__(cls, name, bases, attrs):
        unbound_fields = [(name_, attrs.get(name_)) for name_, field in attrs.items()
                  if isinstance(field, UnboundField)]
        unbound_fields.sort(lambda x, y: cmp(x[1].creation_counter, y[1].creation_counter))
        attrs['unbound_fields'] = unbound_fields
        return super(FormMeta, cls).__new__(cls, name, bases, attrs)
        

class QueryForm(object):
    __metaclass__ = FormMeta
    
    def __init__(self, kv_list=None):
        self._fields = SortedDict()
        for name, unbound_field in self.unbound_fields:
            if unbound_field.name:
                name = unbound_field.name
            field = unbound_field.bind(form=self, name=name)
            self._fields[name] = field
        
        if self.is_submitted():
            if kv_list is None: kv_list = request.values.lists()
            self.filters = Filters(model=self.Meta.model, kv_list=kv_list)
            self.process()
        else:
            self.filters = None

    def clear(self):
        for k in self.filters:
            self[k].process_data('')
        self.filters = None
            
    def process(self):
        for k, v in self.filters.iteritems():
            self[k].process_data(v['value'])

    def is_submitted(self):
        return request and request.method in ("PUT", "POST")

    def __iter__(self):
        return iter(self._fields.itervalues())

    def iteritems(self):
        return iter(self._fields.iteritems())

    def __getitem__(self, name):
        return self._fields[name]

    def save_filter(self, table_name, user_id=None):
        if not request.form['filter-name']:
            return '请给检索条件一个名字!'
            
        filter = QueryFilter()
        filter.name = request.form['filter-name']
        filter.user_id = current_user.id if user_id is None else user_id
        filter.table = table_name
        filter.arg_dict = self.kv_list_str
        db.session.add(filter)
        db.session.commit()

            
    @property
    def kv_list_str(self):
        arg_dict = self.filters.arg_dict
        field_prefix = 'filters'
        kv_list = []
        for field_name, items in arg_dict.iteritems():
            for k, v in items.iteritems():
                if isinstance(v, (list, tuple)):
                    kv_list.extend([('.'.join([field_prefix, field_name, k]), i) for i in v])
                else:
                    kv_list.append(('.'.join([field_prefix, field_name, k]), v))
                
        return str(kv_list)
        
    @property
    def query_str(self):
        if self.filters is None:
            raise ValueError('May not submitted!')
        return self.filters.to_str()


from nodes.models import Vendor, Model
class NodeForm(QueryForm):
    addr = TextField(u'地址', operator='ilike', default='127.0.0.1')
    name = TextField(u'名称', operator='ilike', default='ad')
    area_id = SelectField(u'区域', operator='in',
                          choices=[('', u'选择'), ('1001', '1001'), ('1002', '1002'), ('1003', '1003')])
    vendor_id = SelectField(u'制造商', operator='==',
                             choices=lambda: [('', u'请选择生产厂家')] + [(unicode(r.id), r.alias) for r in Vendor.query])
    model_id = SelectField(u'设备', name="model_id", operator='==',
                           choices=lambda: [('', u'请选择设备型号')] + [(unicode(r.id), r.alias) for r in Model.query])

    class Meta():
        model = Node
    
        
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


    form = NodeForm(kv_list=kv_list)
    field = form.area_id
    print form.filters.to_str()
    print '-----'
    print field(a='addr')
    print '-----'

    addr_o = '<input name="filters.addr.operator" type="hidden" value="ilike" />'
    addr_v = '<input name="filters.addr.value" value="127.0.0.1" />'
    
    areaid_o = '<input name="filters.area_id.operator" type="hidden" value="in" />'
    areaid_v = '''<select id="area_id" multiple="multiple" name="filters.area_id.value">
                    <option value="">请选择管理域</option>
                    <option value="1001">Test add domain 1</option>
                    <option value="1002">Test add domain 2</option>
                    <option value="1003">Test add domain 3</option>
                    <option value="1004">Test add domain 4</option>
                    <option value="1005">Test add domain 5</option>
                    <option value="1010">Great</option>
                  </select>'''

    
    filters = Filters(model=Node, kv_list=kv_list)
    print filters.to_str()
    print [node.id for node in Node.query.filter(filters.to_str()).all()]
