# coding: utf-8
from flask_wtf import Form, TextField, widgets

class SearchForm(Form):
    keyword = TextField()

