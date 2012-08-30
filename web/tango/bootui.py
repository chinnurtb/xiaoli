#!/usr/bin/env python  
# -*- coding: utf-8 -*-

menus = []

widgets = []

dashboards = []

class Menu(object):
    
    def __init__(self, name, title, href):
        self.name = name
        self.title = title
        self.href = href

    def render(self, active = None):
        css = 'active' if active == self.name else ''
        return "<li id=\"menu-%s\" class=\"%s\"><a href=\"%s\">%s</a></li>" % (self.name, css, self.href, self.title)

    def __repr__(self):
        return "Menu(name = %s, title = %s, href = %s)" % (self.name, self.title, self.href)

class Widget(object):

    def __init__(self, name, html):
        self.name = name
        self.html = html

    def __repr__(self):
        return "Widget(name = %s)" % self.name
    
    def render(self):
        return self.html

class Dashboard(object):
    
    def __init__(self, id, name, callback):
        self.id = id
        self.name = name
        self.callback = callback
