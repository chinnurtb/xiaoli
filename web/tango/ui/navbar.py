# coding: utf-8

from jinja2 import Markup
from tango import db

class Navbar(object):
    
    def __init__(self):
        self.navs = []

    def __iter__(self):
        return iter(self.navs)

    def __len__(self):
        return len(self.navs)

    def add(self, name, title, icon, href):
        self.navs.append(Nav(name, title, icon, href))

class Nav(object):
    
    def __init__(self, name, title, icon, href):
        self.name = name
        self.title = title
        self.icon = icon
        self.href = href

    def render(self, active = None):
        css = 'active' if active == self.name else ''
        if db.app.config['license_permit'].has_key(self.name):
            return Markup("<li id=\"menu-%s\" class=\"%s\"><a href=\"%s\"><i class=\"icon-%s\"></i>%s</a></li>"
                       % (self.name, css, self.href, self.icon, self.title))
        else:
            return ''

    def __repr__(self):
        return "Nav(name = %s, title = %s, href = %s)" % (self.name, self.title, self.href)

