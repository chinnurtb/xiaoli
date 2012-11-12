var make_option = function(value, name) {
    return '<option value="'+value+'">'+name+'</option>';
};

var empty_options = function(id) {
    $("#"+id+" option[value!='']").remove();
}
/**
 * 级联下拉框ajax操作
 * @param config 配置信息
 */
jQuery.fn.select_change_ajax = function(config) {
    $select = this;
    var default_config = {
        id: "", // ajax 数据返回的下拉框id
        url: "",// ajax url
        option: "请选择**", //空option显示
        empty: [] // 需要置空的下拉框id
    }
    $.extend(default_config, config);
    $select.change(function() {
        var select_value = $(this).val();
        if(select_value=="") return;
        $.get(default_config.url, {key: select_value},function(data) {
            var data = eval("("+data+")");
            var options = [make_option('',default_config.option)];
            for(var i=0; i< data.length; i++) {
                options.push(make_option(data[i].value,data[i].name));
            }
            $("#"+default_config.id).html(options.join(""));
            for(var i=0; i< default_config.empty.length; i++) {
                empty_options(default_config.empty[i]);
            }
        });
    });
    if($select.val() != "") {
        var select_value = $("#"+default_config.id).val();
        $.get(default_config.url, {key: $select.val()},function(data) {
            var data = eval("("+data+")");
            var options = [make_option('',default_config.option)];
            for(var i=0; i< data.length; i++) {
                options.push(make_option(data[i].value,data[i].name));
            }
            $("#"+default_config.id).html(options.join(""));
            for(var i=0; i< default_config.empty.length; i++) {
                empty_options(default_config.empty[i]);
            }
            $("#"+default_config.id).find("option[value='"+select_value+"']").attr("selected","selected");
        });
    }
};

$(function(){
    $(".delete").click(function() {
        if(confirm("确认删除？")) {
            var input = $(this).parent().parent().next();
            input.attr({checked: "checked"});
            $("body").append('<form id="delete_'+input.val()+'" method="POST" action="'+ $(this).src+'"></form>')
            $("#delete_"+input.val()).attr("action",$(this).attr("href")).append(input.clone()).submit();
        }
        return false;
    })
})
