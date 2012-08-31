
# 导入 tables 
```python
import tables
```


# 定义 SomeTable
```python
class UserTable(tables.Table):
    check      = tables.CheckBoxColumn()
    edit_btn   = tables.EditBtnColumn(endpoint='user_edit')
    delete_btn = tables.DeleteBtnColumn(endpoint='user_delete')
	...
    login          = tables.Column(verbose_name=u'用户名', orderable=True)
    name           = tables.Column(verbose_name=u'用户名', attrs=Attrs{th={'class': 'warning-info'}})
    telephone      = tables.Column()
    role_name      = tables.Column(verbose_name=u'角色名称', accessor='role.name')
    created_at     = tables.DateTimeColumn(verbose_name=u'注册时间',format='%Y-%m-%d %H:%M')
	...
    class Meta():
        model = User
        per_page = 3
        order_by = '-login'
		...
```
  + 继承 Table 类
  + 定义特殊字段 (`check`, `edit_btn`, ...) **$** 其中的 `check` 字段为固定写法必须一字不差, 即 ``check = tables.CheckBoxColumn()``
  + 定义普通字段 (`login`, `name`, ...) **$** 普通字段名应该与相应 Model 中的字段名保持一致, 如果不一致需要在相应的 Column 声明中指定 `accessor` 参数.
  + 在 `class Meta()` 中定义该 Table 的配置选项 (`order_by`, `per_page`, ...)



# 给 Table 添加配置选项
```python
class Meta():
    model = User
    per_page = 3
    order_by = '-login'
    url_makers = {'parent': lambda record: url_for('user', id=getattr(record, 'id', 1),
                                                   mk='QUERY_STRING')}
```
目前只支持: (`model`, `per_page`, `order_by`, `url_makers`)

1. model
  指定和那个数据模型绑定()
2. per_page
  这张表每页显示的列数
3. order_by
  默认按照某个字段排序, 如果字段名前面加 `-` 号表示按该字段倒序
4. url_makers
   如果需要给某个字段加上链接, 并且需要指定特殊的 URL 生成规则, 则需要在 `url_makers` 中给出相应的生成函数(或 lambda). 该类型为一个字典, key 为相应的字段名, value 为相应的生成函数. 而且生成函数需要在参数列表的第一个位置上声明一个参数(即下面的`record`)用来传入某条记录的对象.

    ```python
    url_makers = {'parent': lambda record: url_for('user', id=getattr(record, 'id', 1),
                                                   mk='QUERY_STRING'),
                  'role': lambda record: url_for('role_edit', id=getattr(record, 'id', None))}
    ```
    

# 声明 Column
## 内建 columns([类名] :: [最直接的基类名])
### Column *最基本的列类型, 下面所有类型的基类*
  参数列表(**[参数名]**, **[参数类型]**):
  1. (verbose_name, `str` or `unicode`) **$** 用来指定列的标题, 如果不指定该参数默认是首字母大写的字段名
  2. (accessor, `A` or `Accessor`) **$** 用来指定该字段实际显示的内容. 这个参数可能不太好理解. 假设我们直接从数据库中读取了一个 `User` 对象的记录 user, 如果我们要访问该 user 的角色名称我们会使用 `user.role.name` 来访问. 像这种涉及到两步访问的动作都需要指定 accessor 来完成

    ```python
    user = User.query.get(user_id)
    role_name = user.role.name
    ```

  3. (attrs, `Attrs`) **$** 用来给相应 `HTML` 元素指定属性
    
    ```python
    attrs=Attrs{th={'class': 'warning-info', id="telephone"},
                td={'class': 'render-black', scope="row"}}
    ```
    
  4. (orderable, `bool`) **$** 用来指定是否允许在页面中点击列标题进行排序操作
  
### BaseLinkColumn :: Column
  主要是定义了一个 `render_link` 方法用来生成某个链接的 HTML 字符串

### CheckBoxColumn :: BaseLinkColumn
  表体最左边用来选择列的 `checkbox`, ``<input type="checkbox" name="check">``

### LinkColumn :: BaseLinkColumn
  参数列表(**[参数名]**, **[参数类型]**): 
  1. (endpoint, `str`) **$** 用来指定一个视图的函数名(如`user_edit`), 供反向路由函数(`url_for`)使用. 参见 `url_for` 的文档.
  2. (values, `dict`) **$** 用来生成链接中的 **QUERY_STRING**.参见 `url_for` 的文档.
  3. (_external, `bool`) **$** 用来生成完整的链接, 参见 `url_for` 的文档.

### EmailColumn :: BaseLinkColumn
  把 Email 地址都加上 `mailto:`, 一般没必要使用该类型

### ButtonColumn :: BaseLinkColumn
  这里所谓的 Button 其实就是一个链接

  参数列表(**[参数名]**, **[参数类型]**):
  1. (endpoint, `str`) **$** 同 `LinkColumn` 中的 endpoint
  2. (icon_type, `str`) **$** 用来指定该按钮上显示的 icon 图样, 可选名字来自 [Bootstrap](http://twitter.github.com/bootstrap/base-css.html#icons), 格式为相应的 icon 名字去掉前面的 `icon-` 部份


### EditBtnColumn :: ButtonColumn
  用来生成特定的**编辑**某一条记录的按钮

### DeleteBtnColumn :: ButtonColumn
  用来生成特定的**删除**某一条记录的按钮

### DateTimeColumn :: Column
  用来生成日期的列
  参数列表(**[参数名]**, **[参数类型]**):  
  1. (format, `str`) **$** 用来格式化日期的字符串



# 排序
  1. 在视图逻辑中初始化 Table 时指定

    ```python
    table = UserTable(query, order_by='')
    ```
    
  2. 在 URL 的 `QUERY_STRING` 中指定
  3. 在 `Meta` 类种指定

  当插件在处理排序操作时, 会按照一定的**优先级**来选择需要排序的字段:
	 ``视图逻辑 > QUERY_STRING > Meta类``

# 分页
  主要指定 `per_page` 的值, 用法和**排序**(`order_by`)的用法 无明显差异


# 自定义样式
	详见 Column 中的 attrs 参数



# 使用 table
## 在视图中使用 Table

```python
...
query = User.query
table = UserTable(query, request)
return render_template('test_table.html',table=table)
```

初始化 Table 的参数列表(**[参数名]**, **[参数类型]**):
  1. (query, `sqlalchemy.orm.query.Query`) **$** 一个查询(query)对象 [**必要参数**]
  2. (request, `flask.Request`) **$** 标准的 flask Request 对象, [**必要参数**]
  3. (page, `int`) **$** 显示第 `page` 页的数据
  4. (per_page, `int`) **$** 每页最多显示的记录数
  5. (order_by, `str`) **$** 需要排序字段
  6. (sequence, `list` or `tuple`) **$** 指定显示列的顺序
  7. (template, `str`) **$** 指定渲染 Table 的**核心**模板文件的相对路径

## 在模板中使用 table

```python
{% from "_table_h.html" import render_table, render_screen_meta %}
...
{{ render_screen_meta(table) }}
...
{{ render_table(table) }}
...
```
    
# Create Date: `<2012-08-30 四>`
