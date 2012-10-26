# coding: utf-8

from jinja2 import Markup

class Navbar(object):
    
    def __init__(self):
        self.navs = []

    def __iter__(self):
        return iter(self.navs)

    def __len__(self):
        return len(self.navs)

    def add(self, name, title, href):
        self.navs.append(Nav(name, title, href))

class Nav(object):
    
    def __init__(self, name, title, href):
        self.name = name
        self.title = title
        self.href = href

    def render(self, active = None):
        css = 'active' if active == self.name else ''
        return Markup("<li id=\"menu-%s\" class=\"%s\"><a href=\"%s\">%s</a></li>"
                       % (self.name, css, self.href, self.title))

    def __repr__(self):
        return "Nav(name = %s, title = %s, href = %s)" % (self.name, self.title, self.href)

