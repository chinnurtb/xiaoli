#!/usr/bin/env python
# -*- coding: utf-8 -*-


        
from flask_wtf.form import Form
from jinja2 import Markup
from tango.ui.tables.utils import Attrs, AttributeDict
from tango.form.widgets import Media

class  FormPro(Form):
    class Meta:
        pass

    def _html_output(self, label_row, label_attrs,field_row, field_attrs, column, row_start, row_end):
        "Helper function for outputting HTML. Used by as_table(), as_ul(), as_p()."
        output = []
        show_fields,hidden_fields = [],[]
        for field in self:
            if field.type == 'HiddenField' or field.type == 'CSRFTokenField':
                hidden_fields.append(field())
            elif hasattr(self.Meta,'list_display') and field.id not in self.Meta.list_display:
                pass
            else:
                show_fields.append(field)
        if int(column) > 0:
            goup_list = [show_fields[i:i+column] for i in range(0,len(show_fields),column)]
            for goup in goup_list:
                output.append(row_start)
                for field in goup:
                    output.append((label_row+field_row) % {
                        'label': field.label.text,
                        'label_attrs': label_attrs,
                        'field': field(),
                        'field_attrs':field_attrs,
                        'field_id': field.id
                    })
                output.append(row_end)
        else:
            for field in show_fields:
                output.append(row_start)
                output.append(label_row % {
                    'label': field.label.text,
                    'label_attrs': label_attrs,
                    'field_id': field.id
                })
                output.append(row_end)
                output.append(row_start)
                output.append(field_row % {
                    'field': field(),
                    'field_attrs':field_attrs,
                })
                output.append(row_end)

        output.extend(hidden_fields)
        return output

    def set_attrs(self):
        attrs = getattr(self.Meta,'attrs',{}) or Attrs()
        self.table_attrs = AttributeDict(**(attrs.get('table') or {}))
        self.label_attrs = AttributeDict(**(attrs.get('label') or {}))
        self.field_attrs = AttributeDict(**(attrs.get('field') or {}))

    def as_table(self, column=0):
        "Returns this form rendered as HTML <tr>s -- excluding the <table></table>."
        output = []
        self.set_attrs()
        output.append(u'<table %s>' % AttributeDict.merge(self.table_attrs, {'class': 'form_table'}).as_html())
        trs = self._html_output(
            label_row = u'<td %(label_attrs)s><label for="%(field_id)s"><span>%(label)s</span></label></td>',
            label_attrs = AttributeDict.merge(self.label_attrs, {'class': 'table_label'}).as_html(),
            field_row = u'<td %(field_attrs)s>%(field)s</td>',
            field_attrs = AttributeDict.merge(self.field_attrs, {'class': 'table_field'}).as_html(),
            column = column,
            row_start = u'<tr>',
            row_end = u'</tr>'
        )
        output.extend(trs)
        output.append(u'</table>')
        return Markup(u'\n'.join(output))

    def as_p(self):
        pass

    def as_ul(self):
        pass

    def _get_media(self):
        media = Media()
        for field in self:
            if hasattr(field.widget,'media'): media = media + field.widget.media
        return media
    media = property(_get_media)

