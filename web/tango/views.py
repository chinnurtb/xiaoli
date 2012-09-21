from tango.ui import tables

class SettingTable(tables.Table):
    
    alias   = table.Column(verbose_name='名称', endpoint='settings_edit')
