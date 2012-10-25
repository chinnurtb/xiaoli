#!/usr/bin/env python
# -*- coding: utf-8 -*-

from itertools import chain
from jinja2 import Markup
from flask import request

MEDIA_TYPES = ('css','js')

class Media(object):
    def __init__(self, media=None, **kwargs):
        if media:
            media_attrs = media
        else:
            media_attrs = kwargs

        self._css = []
        self._js = []

        for name in MEDIA_TYPES:
            getattr(self, 'add_' + name)(media_attrs.get(name, None))

    def __unicode__(self):
        return self.render()

    def render(self):
        return Markup(u'\n'.join(chain(*[getattr(self, 'render_' + name)() for name in MEDIA_TYPES])))

    def render_js(self):
        return [u'<script type="text/javascript" src="%s"></script>' % path for path in self._js]

    def render_css(self):
        return [u'<link href="%s" type="text/css" rel="stylesheet" />' % path for path in self._css]

    def __getitem__(self, name):
        "Returns a Media object that only contains media of the given type"
        if name in MEDIA_TYPES:
            return Media(**{str(name): getattr(self, '_' + name)})
        raise KeyError('Unknown media type "%s"' % name)

    def add_js(self, data):
        if data:
            for path in data:
                if path not in self._js:
                    self._js.append(path)

    def add_css(self, data):
        if data:
            for path in data:
                if path not in self._css:
                    self._css.append(path)

    def __add__(self, other):
        combined = Media()
        for name in MEDIA_TYPES:
            getattr(combined, 'add_' + name)(getattr(self, '_' + name, None))
            getattr(combined, 'add_' + name)(getattr(other, '_' + name, None))
        return combined


class AreaSelectWidget(object):
    def __call__(self, field, **kwargs):
        if request.method == 'POST':
            area_value = request.form.get(field.name,'')
            netloc_value = request.form.get(field.name+'_netloc','')
            selected_value = request.form.get(field.name+'_selected','')
        else:
            area_value = request.args.get(field.name,'')
            netloc_value = request.args.get(field.name+'_netloc','')
            selected_value = request.args.get(field.name+'_selected','')
            if hasattr(field.data,'get') and area_value == '':
                area_value = field.data.get('area_value')
                netloc_value = field.data.get('netloc_value')
                selected_value = field.data.get('selected_value')
        if area_value == '':
            area_value_chosen = u'请选择所属区域'
        else:
            area_value_chosen = area_value
        html = u'''
        <script type="text/javascript">
            $(function(){
                var config = {
                    replace_search_input: function(){
                        return '<div id="tree" style="height: 150px;"></div>'
                    }
                };
                $("#%(field_id)s_chosen").chosen(config);
                $("#tree").dynatree({
                    checkbox: true,
                    selectMode: %(select_mode)s,
                    imagePath: '',
                    initAjax:{
                        url:"/area_select",
                        data:{
                            selected_ids: $('#%(field_id)s_selected').val()
                        }
                    },
                    onActivate: function(dtnode) {},
                    onLazyRead: function(dtnode){
                        dtnode.appendAjax({
                            url:"/area_select",
                            data:{
                                key: dtnode.data.key,
                                selected_ids: $('#%(field_id)s_selected').val()
                            }
                        });
                    },
                    onSelect: function(select, dtnode) {
                        var selNodes = dtnode.tree.getSelectedNodes();
                        var selkeys = [];
                        var selTitles = [];
                        var selvals = $.map(selNodes, function(node){
                            selkeys.push(node.data.key);
                            selTitles.push(node.data.title);
                            return node.data.area_type + '=' + node.data.key;
                        });
                        $("#%(field_id)s_netloc").val(selvals.join(" or "));
                        $("#%(field_id)s_selected").val(selkeys.join(","));
                        $("#%(field_id)s").val(selTitles.join(","));
                        if(selTitles == "") {
                            $("a.chzn-single.chzn-default span").html("请选择所属区域");
                        }else {
                            $("a.chzn-single.chzn-default span").html(selTitles.join(","));
                        }
                    },
                    strings: {
                        loading: "加载中…",
                        loadError: "加载错误!"
                    },
                    onKeydown: function(dtnode, event) {
                        if( event.which == 13 ) {
                            dtnode.toggleSelect();
                            return false;
                        }
                    }
                });
                $("a.chzn-single.chzn-default span").html("%(area_value_chosen)s");
            });
        </script>
        <select id="%(field_id)s_chosen"></select>
        <input id="%(field_id)s" type="hidden" name="%(field_name)s" value="%(area_value)s" />
        <input id="%(field_id)s_netloc" type="hidden" value="%(netloc_value)s" name="%(field_name)s_netloc" />
        <input id="%(field_id)s_selected" type="hidden" value="%(selected_value)s" name="%(field_name)s_selected" />
        ''' % {
            'select_mode': field.select_mode,
            'field_id': field.id,
            'field_name': field.name,
            'area_value':area_value,
            'area_value_chosen':area_value_chosen,
            'netloc_value': netloc_value,
            'selected_value': selected_value
        }
        return Markup(html)

    def _get_media(self):
        media = Media(media={
            'css': [
                '/static/css/chosen.css',
                '/static/css/dynatree/skin/ui.dynatree.css'
            ],
            'js': [
                '/static/js/chosen.jquery.js',
                '/static/js/jquery.dynatree.js',
                '/static/js/jquery-ui.custom.min.js',
                '/static/js/jquery.cookie.js'
            ]
        })
        return media
    media = property(_get_media)
