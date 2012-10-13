# coding: utf-8

DEFAULT_PAGE = 1
DEFAULT_PER_PAGE = 30

class TableConfig(object):

    def __init__(self, request, profile={}):
        self._request = request
        self._profile = profile

    def configure(self, table):
        order_by = self._request.args.get('order_by', None)
        table.order_by = order_by

        hidden_columns = self._profile.get(table. profile_hiddens_key, '')
        table.hiddens = hidden_columns

        page = int(self._request.args.get('page', DEFAULT_PAGE))
        per_page = int(self._profile.get(table.profile_perpage_key, DEFAULT_PER_PAGE))
        table.paginate(page, per_page)
        
