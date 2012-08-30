#!/usr/bin/env python  
# -*- coding: utf-8 -*-

menus = []

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
