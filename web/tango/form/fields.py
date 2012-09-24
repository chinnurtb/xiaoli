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

    def _value(self):
        if self.data:
            return u', '.join(self.data)
        else:
            return u''

    def process_formdata(self, valuelist):
        if valuelist:
            self.data = [x.strip() for x in valuelist[0].split(',')]
        else:
            self.data = []
