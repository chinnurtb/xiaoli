#coding=utf-8

from flask_wtf import Form, TextField

class SearchForm(Form):
    keyword = TextField()
