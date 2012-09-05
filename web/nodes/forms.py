#!/usr/bin/env python
# -*- coding: utf-8 -*-

import re

from wtforms import BooleanField 
from wtforms import validators as v
from tango.forms import SelectFieldPro
from .models import Node, Board, Port

from flask_wtf import (Form, TextField, PasswordField,
                       TextAreaField, ValidationError, required, equal_to, email)

class NodeNewForm(Form):
    alias            = TextField(u'名称', validators=[required(message=u'必填')])
    addr             = TextField(u'地址', validators=[required(message=u'必填')])
