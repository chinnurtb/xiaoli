#!/usr/bin/env python
# -*- coding: utf-8 -*-

from wtforms.compat import text_type
from wtforms import SelectField, Field
from wtforms.ext.dateutil.fields import DateTimeField, DateField

from tango.ui.form.widgets import AreaSelectWidget

class SelectFieldPro(SelectField):
    def __init__(self, label=None, validators=None, coerce=text_type, choices=None, **kwargs):
        if callable(choices):
            choices = choices()
        super(SelectFieldPro, self).__init__(label, validators, coerce, choices, **kwargs)

class DateTimeFieldPro(DateTimeField):
    '''允许日期时间为空'''
    def __init__(self, label=None, validators=None, parse_kwargs=None,
                 display_format='%Y-%m-%d', **kwargs):
        super(DateTimeFieldPro, self).__init__(label, validators, parse_kwargs=parse_kwargs, display_format=display_format, **kwargs)

    def process_formdata(self, valuelist):
        if valuelist:
            date_str = ' '.join(valuelist)
            if not date_str:
                self.data = None
            else:
                super(DateTimeFieldPro, self).process_formdata(valuelist)

class DateFieldPro(DateTimeFieldPro):
    def __init__(self, label=None, validators=None, parse_kwargs=None,
                 display_format='%Y-%m-%d', **kwargs):
        super(DateFieldPro, self).__init__(label, validators, parse_kwargs=parse_kwargs, display_format=display_format, **kwargs)

    def process_formdata(self, valuelist):
        super(DateFieldPro, self).process_formdata(valuelist)
        if self.data is not None and hasattr(self.data, 'date'):
            self.data = self.data.date()
        

class AreaSelectField(Field):
    widget = AreaSelectWidget()

    def __init__(self, label=None, select_mode=2, **kwargs):
        super(AreaSelectField, self).__init__(label,**kwargs)
        self.select_mode = select_mode

    def process(self, formdata, data=object()):
        ori_name = self.name
        self.name = ori_name + '_selected'
        Field.process(self, formdata, data)
        self.name = ori_name

    def _value(self):
        if self.data:
            return u', '.join(self.data)
        else:
            return u''

    def populate_obj(self, obj, name):
        data = self.data
        if self.select_mode == 1:
            data = "".join(self.data)
        setattr(obj, name, data)

    def process_formdata(self, valuelist):
        if valuelist and valuelist[0]:
            self.data = [x.strip() for x in valuelist[0].split(',')]
        else:
            self.data = []
