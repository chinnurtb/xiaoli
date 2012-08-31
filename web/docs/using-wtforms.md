
# 导入
```python
from flask_wtf import Form, TextField, ...
```

# 定义 SomeForm
```python
class UserNewForm(Form):
    name             = TextField(u'真实姓名', validators=[required(message=u'必填')])
    password         = PasswordField(u'密码', validators=[required(message=u'必填')])
    password_confirm = PasswordField(u'重复密码', validators=[required(message=u'必填'), 
                                                             equal_to('password', message=u'两次输入的密码不同')])
    department       = TextField(u'部门')
    email            = TextField(u'邮箱', validators=[email(message=u'不是合法的邮箱地址')])
    role_id          = SelectField(u'角色', validators=[required(message=u'必填')],
                                   choices=[('', u'请选择角色'), ('1', u'系统管理员'), ('1', u'系统配置人员')]
```


# 常用的 Fields
## TextField

## TextAreaField

## PasswordField

## IntegerField

## DateField

## RadioField

## SelectField
需要指定 `choices` 参数

```python
choices=[('', u'请选择角色'), ('1', u'系统管理员'), ('1', u'系统配置人员')
```	

## HiddenField



# 常用的内置验证函数
## required
该字段应该不为空
	
## equal_to 或 EqualTo
该字段应该与已声明的某字段值相等

## email
该字段应该是合法的邮箱地址


# 自定义验证函数(总共 4 种方式)
## 作为 SomeForm 中的方法
```python
class MyForm(Form):
    name = TextField('Name', [Required()])
	...
    def validate_name(form, field):
        if len(field.data) > 50:
            raise ValidationError('Name must be less than 50 characters')
```
    
## 作为不能指定参数的外部函数
```python
def my_length_check(form, field):
    if len(field.data) > 50:
        raise ValidationError('Field must be less than 50 characters')
...
class MyForm(Form):
    name = TextField('Name', [Required(), my_length_check])
```

## 作为可以指定参数的外部函数
```python
def length(min=-1, max=-1):
    message = 'Must be between %d and %d characters long.' % (min, max)
	...
    def _length(form, field):
        l = field.data and len(field.data) or 0
        if l < min or max != -1 and l > max:
            raise ValidationError(message)
	...
    return _length
...
class MyForm(Form):
    name = TextField('Name', [Required(), length(max=50)])
```

## 作为 callable 的类 (实现了 __call__ 方法) 
```python
class Length(object):
    def __init__(self, min=-1, max=-1, message=None):
        self.min = min
        self.max = max
        if not message:
            message = u'Field must be between %i and %i characters long.' % (min, max)
        self.message = message
	...
    def __call__(self, form, field):
        l = field.data and len(field.data) or 0
        if l < self.min or self.max != -1 and l > self.max:
            raise ValidationError(self.message)
...
length = Length
```

# 在视图中使用 SomeForm
```python
@app.route('/user/edit/<int:id>/', methods=['POST', 'GET'])
def user_edit(id):
    form = UserEditForm()
    user = User.query.get_or_404(id)
	...
    if request.method == 'POST' and form.validate_on_submit():
        form.populate_obj(user)
        db.session.add(user)
        db.session.commit()
        return redirect(url_for('users'))
	...
    form.process(obj=user)
    return render_template('test_edit_user.html',
                           user=user,
                           form=form)
```


# 在模板中使用 form 
## 定义渲染字段的模板宏
```html
{% macro render_field(field) %}
<div class="control-group {% if field.errors %}{{'error'}}{% endif %}">
    {{ field.label(class="control-label") }}
    <div class="controls">
	{{ field(**kwargs) }}
	{% if field.errors %}
	<span class="help-inline">
	    {{ field.errors[0]|e }}
	</span>
	{% endif %}
    </div>
</div>
{% endmacro %}
```

## 在页面中使用模板宏
```html
<form  method="POST" action="{{url_for('user_edit', id=user.id)}}">
	{{ form.hidden_tag() }}
	{{ render_field(form.role_id) }}
	{{ render_field(form.memo, rows=4, class="input-xlarge") }}
	<button type="submit" class="btn btn-primary">确定</button>
</form>
```

# 参考文档
1. [WTForms Documentation](http://wtforms.simplecodes.com/docs/1.0.2/) **重点**
2. [Flask-WTF](http://packages.python.org/Flask-WTF/) `内容很有限`
