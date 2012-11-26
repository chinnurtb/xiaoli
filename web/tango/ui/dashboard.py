# coding: utf-8
from tango import db

class Dashboard(object):
    
    layout_meta = 'screen.layout'

    refresh_meta = 'page.refresh'

    order_meta = 'box.order'

    closed_meta = 'closedbox' 

    hidden_meta = 'metaboxhidden'

    welcome_meta = 'welcome.panel'

    def __init__(self, id='dashboard'):
        self.id = id
        self.widgets = [] 
        self.layout = 2
        self.order = {}
        self.closed = []
        self.hidden = []
        self.welcome = '1'

    def add_widget(self, id, title, content=None, url=None, column='normal'):
        if db.app.config['license_permit'].has_key(id):
            self.widgets.append(Widget(id, title, content, url, column))

    def configure(self, profile):
        self.layout = int(profile.get(Dashboard.layout_meta, '2'))
        self.refresh = int(profile.get(Dashboard.refresh_meta, '5'))
        self.refreshsecs = self.refresh*60
        self.order = eval(profile.get(Dashboard.order_meta, '{}'))
        for col, val in self.order.items():
            self.order[col] = val.split(',')
        self.closed = profile.get(Dashboard.closed_meta, '').split(',')
        self.hidden = profile.get(Dashboard.hidden_meta, '').split(',')
        self.welcome = profile.get(Dashboard.welcome_meta, '1')

    def widgets(self):
        self.widgets

    def get_widget(self, wid):
        for widget in self.widgets:
            if widget.id == wid:
                return widget
        return None

    def column_widgets(self, column):
        return [w for w in self.widgets if w.column == column]

    def is_hidden(self, wid):
        return wid in self.hidden

    def is_closed(self, wid):
        return wid in self.closed

    def in_order(self, wid):
        for l in self.order.values(): 
            if wid in l: return True
        return False 

class Widget(object):

    def __init__(self, id, title, content=None, url=None, column='normal'):
        self.id = id
        self.url = url
        self.title = title
        self.content = content
        self.column = column

    def __repr__(self):
        return "Widget(id = '%s', title = '%s', url = '%s')" % (self.id, self.title, self.url)

