#!/usr/bin/env python
# -*- coding: utf-8 -*-

from itertools import chain
from jinja2 import Markup

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
        html = u'''
        <script type="text/javascript">
            $(function(){
                var config = {
                    replaceText: "加载中...",
                    onShow: function(dropPanel){
                        $("#dropContent").dynatree({
                            checkbox: true,
                            selectMode: 3,
                            imagePath: '',
                            initAjax:{
                                url:"/area_select",
                                data:{
                                    domain_ids: $('#domain_ids').val(),
                                    expand_level : 3,
                                    guid : $('#conditions_guid').val()
                                }
                            },
                            onActivate: function(dtnode) {},
                            onLazyRead: function(dtnode){
                                dtnode.appendAjax({
                                    url:"/area_select",
                                    data:{
                                        key: dtnode.data.key,
                                        domain_ids: $('#domain_ids').val(),
                                        guid : $('#conditions_guid').val()
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
                                    return node.data.level_desc + '=' + node.data.key;
                                });
                                $("#domains").val(selvals.join(" or "));
                                $("#domain_ids").val(selkeys.join(","));
                                $(".dropPanel_txt").val(selTitles.join(","));
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
                    }
                };
                $("#%(field_id)s").dropPanel(config);
            });
        </script>
        <select id="%(field_id)s" name="%(field_name)s"></select>
        <input id="domains" type="hidden" value="" name="netloc">
        <input id="domain_ids" type="hidden" value="" name="area_ids">
        ''' % {
            'field_id': field.id,
            'field_name': field.name
        }
        return html

    def _get_media(self):
        media = Media(media={
            'css': [
                '/static/css/jquery.dropPanel.css',
                '/static/css/dynatree/skin/ui.dynatree.css'
            ],
            'js': [
                '/static/js/jquery.dropPanel.js',
                '/static/js/jquery.dynatree.js',
                '/static/js/jquery-ui.custom.min.js',
                '/static/js/jquery.cookie.js'
            ]
        })
        return media
    media = property(_get_media)
