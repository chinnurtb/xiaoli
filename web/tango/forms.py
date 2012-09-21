
from wtforms.compat import text_type
from wtforms import SelectField
class SelectFieldPro(SelectField):
    def __init__(self, label=None, validators=None, coerce=text_type, choices=None, **kwargs):
        if callable(choices):
            choices = choices()
        super(SelectFieldPro, self).__init__(label, validators, coerce, choices, **kwargs)


        
from flask_wtf.form import Form, _Auto
from jinja2 import Markup
class  FormPro(Form):
    
    def _html_output(self, normal_row, column, row_start, row_end):
        "Helper function for outputting HTML. Used by as_table(), as_ul(), as_p()."
        output = []
        show_fields,hidden_fields = [],[]
        for name,field in self._fields.items():
            if field.type == 'HiddenField' or field.type == 'CSRFTokenField':
                hidden_fields.append(field())
            else:
                show_fields.append((name,field))
        goup_list = [show_fields[i:i+column] for i in range(0,len(show_fields),column)]

        for goup in goup_list:
            output.append(row_start)
            for name, field in goup:
                output.append(normal_row % {
                    'label': field.label.text,
                    'field': field()
                })
            output.append(row_end)
        output.extend(hidden_fields)
        return Markup(u'\n'.join(output))

    def as_table(self, column=1):
        "Returns this form rendered as HTML <tr>s -- excluding the <table></table>."
        return self._html_output(
            normal_row = u'<td class="label">%(label)s</td><td class="field">%(field)s</td>',
            column = column,
            row_start = u'<tr>',
            row_end = u'</tr>'
        )

    def as_p(self):
        pass

    def as_ul(self):
        pass
