# dashboard
widgets = []

def add_widget(widget):
    widgets.append(widget)     

class Widget(object):

    def __init__(self, id, title, content=None, url=None, column='normal'):
        self.id = id
        self.url = url
        self.title = title
        self.content = content
        self.column = column

    def __repr__(self):
        return "Widget(id = '%s', title = '%s', url = '%s')" % (self.id, self.title, self.url)

class Dashboard(object):
    
    layout_meta = 'dashboard.screen.layout'

    order_meta = 'dashboard.box.order'

    closed_meta = 'dashboard.closedbox' 

    hidden_meta = 'dashboard.metaboxhidden'

    welcome_meta = 'dashboard.welcome.panel'

    def __init__(self, widgets):
        self.widgets = widgets
        self.layout = 2
        self.order = {}
        self.closed = []
        self.hidden = []
        self.welcome = '1'

    def configure(self, profile):
        self.layout = int(profile.get(Dashboard.layout_meta, '2'))
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
