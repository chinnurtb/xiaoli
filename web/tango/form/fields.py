#!/usr/bin/env python
# -*- coding: utf-8 -*-

from wtforms.compat import text_type
from wtforms import SelectField, Field

from tango.form.widgets import AreaSelectWidget

class SelectFieldPro(SelectField):
    def __init__(self, label=None, validators=None, coerce=text_type, choices=None, **kwargs):
        if callable(choices):
            choices = choices()
        super(SelectFieldPro, self).__init__(label, validators, coerce, choices, **kwargs)


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
