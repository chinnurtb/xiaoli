# -*- coding: utf-8 -*-

import cgi
from StringIO import StringIO
from UserDict import DictMixin

# ==============================================================================
#  from paste.util.multidict import MultiDict
# ==============================================================================
class MultiDict(DictMixin):

    """
    An ordered dictionary that can have multiple values for each key.
    Adds the methods getall, getone, mixed, and add to the normal
    dictionary interface.
    """

    def __init__(self, *args, **kw):
        if len(args) > 1:
            raise TypeError(
                "MultiDict can only be called with one positional argument")
        if args:
            if hasattr(args[0], 'iteritems'):
                items = list(args[0].iteritems())
            elif hasattr(args[0], 'items'):
                items = args[0].items()
            else:
                items = list(args[0])
            self._items = items
        else:
            self._items = []
        self._items.extend(kw.iteritems())

    def __getitem__(self, key):
        for k, v in self._items:
            if k == key:
                return v
        raise KeyError(repr(key))

    def __setitem__(self, key, value):
        try:
            del self[key]
        except KeyError:
            pass
        self._items.append((key, value))

    def add(self, key, value):
        """
        Add the key and value, not overwriting any previous value.
        """
        self._items.append((key, value))

    def getall(self, key):
        """
        Return a list of all values matching the key (may be an empty list)
        """
        result = []
        for k, v in self._items:
            if key == k:
                result.append(v)
        return result

    def getone(self, key):
        """
        Get one value matching the key, raising a KeyError if multiple
        values were found.
        """
        v = self.getall(key)
        if not v:
            raise KeyError('Key not found: %r' % key)
        if len(v) > 1:
            raise KeyError('Multiple values match %r: %r' % (key, v))
        return v[0]

    def mixed(self):
        """
        Returns a dictionary where the values are either single
        values, or a list of values when a key/value appears more than
        once in this dictionary.  This is similar to the kind of
        dictionary often used to represent the variables in a web
        request.
        """
        result = {}
        multi = {}
        for key, value in self._items:
            if key in result:
                # We do this to not clobber any lists that are
                # *actual* values in this dictionary:
                if key in multi:
                    result[key].append(value)
                else:
                    result[key] = [result[key], value]
                    multi[key] = None
            else:
                result[key] = value
        return result

    def dict_of_lists(self):
        """
        Returns a dictionary where each key is associated with a
        list of values.
        """
        result = {}
        for key, value in self._items:
            if key in result:
                result[key].append(value)
            else:
                result[key] = [value]
        return result

    def __delitem__(self, key):
        items = self._items
        found = False
        for i in range(len(items)-1, -1, -1):
            if items[i][0] == key:
                del items[i]
                found = True
        if not found:
            raise KeyError(repr(key))

    def __contains__(self, key):
        for k, v in self._items:
            if k == key:
                return True
        return False

    has_key = __contains__

    def clear(self):
        self._items = []

    def copy(self):
        return MultiDict(self)

    def setdefault(self, key, default=None):
        for k, v in self._items:
            if key == k:
                return v
        self._items.append((key, default))
        return default

    def pop(self, key, *args):
        if len(args) > 1:
            raise TypeError, "pop expected at most 2 arguments, got "\
                              + repr(1 + len(args))
        for i in range(len(self._items)):
            if self._items[i][0] == key:
                v = self._items[i][1]
                del self._items[i]
                return v
        if args:
            return args[0]
        else:
            raise KeyError(repr(key))

    def popitem(self):
        return self._items.pop()

    def update(self, other=None, **kwargs):
        if other is None:
            pass
        elif hasattr(other, 'items'):
            self._items.extend(other.items())
        elif hasattr(other, 'keys'):
            for k in other.keys():
                self._items.append((k, other[k]))
        else:
            for k, v in other:
                self._items.append((k, v))
        if kwargs:
            self.update(kwargs)

    def __repr__(self):
        items = ', '.join(['(%r, %r)' % v for v in self._items])
        return '%s([%s])' % (self.__class__.__name__, items)

    def __len__(self):
        return len(self._items)

    ##
    ## All the iteration:
    ##

    def keys(self):
        return [k for k, v in self._items]

    def iterkeys(self):
        for k, v in self._items:
            yield k

    __iter__ = iterkeys

    def items(self):
        return self._items[:]

    def iteritems(self):
        return iter(self._items)

    def values(self):
        return [v for k, v in self._items]

    def itervalues(self):
        for k, v in self._items:
            yield v


# ==============================================================================
#  from paste.request import parse_formvars, parse_dict_querystring
# ==============================================================================
def parse_dict_querystring(environ):
    """Parses a query string like parse_querystring, but returns a MultiDict

    Caches this value in case parse_dict_querystring is called again
    for the same request.

    Example::

        >>> environ = {'QUERY_STRING': 'day=Monday&user=fred&user=jane'}
        >>> parsed = parse_dict_querystring(environ)

        >>> parsed['day']
        'Monday'
        >>> parsed['user']
        'fred'
        >>> parsed.getall('user')
        ['fred', 'jane']

    """
    source = environ.get('QUERY_STRING', '')
    if not source:
        return MultiDict()
    if 'paste.parsed_dict_querystring' in environ:
        parsed, check_source = environ['paste.parsed_dict_querystring']
        if check_source == source:
            return parsed
    parsed = cgi.parse_qsl(source, keep_blank_values=True,
                           strict_parsing=False)
    multi = MultiDict(parsed)
    environ['paste.parsed_dict_querystring'] = (multi, source)
    return multi


def parse_formvars(environ, include_get_vars=True):
    """Parses the request, returning a MultiDict of form variables.

    If ``include_get_vars`` is true then GET (query string) variables
    will also be folded into the MultiDict.

    All values should be strings, except for file uploads which are
    left as ``FieldStorage`` instances.

    If the request was not a normal form request (e.g., a POST with an
    XML body) then ``environ['wsgi.input']`` won't be read.
    """
    source = environ['wsgi.input']
    if 'paste.parsed_formvars' in environ:
        parsed, check_source = environ['paste.parsed_formvars']
        if check_source == source:
            if include_get_vars:
                parsed.update(parse_querystring(environ))
            return parsed
    # @@: Shouldn't bother FieldStorage parsing during GET/HEAD and
    # fake_out_cgi requests
    type = environ.get('CONTENT_TYPE', '').lower()
    if ';' in type:
        type = type.split(';', 1)[0]
    fake_out_cgi = type not in ('', 'application/x-www-form-urlencoded',
                                'multipart/form-data')
    # FieldStorage assumes a default CONTENT_LENGTH of -1, but a
    # default of 0 is better:
    if not environ.get('CONTENT_LENGTH'):
        environ['CONTENT_LENGTH'] = '0'
    # Prevent FieldStorage from parsing QUERY_STRING during GET/HEAD
    # requests
    old_query_string = environ.get('QUERY_STRING','')
    environ['QUERY_STRING'] = ''
    if fake_out_cgi:
        input = StringIO('')
        old_content_type = environ.get('CONTENT_TYPE')
        old_content_length = environ.get('CONTENT_LENGTH')
        environ['CONTENT_LENGTH'] = '0'
        environ['CONTENT_TYPE'] = ''
    else:
        input = environ['wsgi.input']
    fs = cgi.FieldStorage(fp=input,
                          environ=environ,
                          keep_blank_values=1)
    environ['QUERY_STRING'] = old_query_string
    if fake_out_cgi:
        environ['CONTENT_TYPE'] = old_content_type
        environ['CONTENT_LENGTH'] = old_content_length
    formvars = MultiDict()
    if isinstance(fs.value, list):
        for name in fs.keys():
            values = fs[name]
            if not isinstance(values, list):
                values = [values]
            for value in values:
                if not value.filename:
                    value = value.value
                formvars.add(name, value)
    environ['paste.parsed_formvars'] = (formvars, source)
    if include_get_vars:
        formvars.update(parse_querystring(environ))
    return formvars


# ==============================================================================
#  from repoze.what.predicates import Predicate
# ==============================================================================
class PredicateError(Exception):
    """
    Former exception raised by a :class:`Predicate` if it's not met.

    .. deprecated:: 1.0.4
        Deprecated in favor of :class:`NotAuthorizedError`, for forward
        compatibility with :mod:`repoze.what` v2.

    """

    # Ugly workaround for Python < 2.6:
    if not hasattr(Exception, '__unicode__'):
        def __unicode__(self):
            return unicode(self.args and self.args[0] or '')


class NotAuthorizedError(PredicateError):
    """
    Exception raised by :meth:`Predicate.check_authorization` if the subject
    is not allowed to access the requested source.

    This exception deprecates :class:`PredicateError` as of v1.0.4, but
    extends it to avoid breaking backwards compatibility.

    .. versionchanged:: 1.0.4
        This exception was defined at :mod:`repoze.what.authorize` until
        version 1.0.3, but is still imported into that module to keep backwards
        compatibility with v1.X releases -- but it won't work in
        :mod:`repoze.what` v2.

    """
    pass

class Predicate(object):
    """
    Generic predicate checker.

    This is the base predicate class. It won't do anything useful for you,
    unless you subclass it.

    """

    def __init__(self, msg=None):
        """
        Create a predicate and use ``msg`` as the error message if it fails.

        :param msg: The error message, if you want to override the default one
            defined by the predicate.
        :type msg: str

        You may use the ``msg`` keyword argument with any predicate.

        """
        if msg:
            self.message = msg

    def evaluate(self, environ, credentials):
        """
        Raise an exception if the predicate is not met.

        :param environ: The WSGI environment.
        :type environ: dict
        :param credentials: The :mod:`repoze.what` ``credentials`` dictionary
            as a short-cut.
        :type credentials: dict
        :raise NotImplementedError: When the predicate doesn't define this
            method.
        :raises NotAuthorizedError: If the predicate is not met (use
            :meth:`unmet` to raise it).

        This is the method that must be overridden by any predicate checker.

        For example, if your predicate is "The current month is the specified
        one", you may define the following predicate checker::

            from datetime import date
            from repoze.what.predicates import Predicate

            class is_month(Predicate):
                message = 'The current month must be %(right_month)s'

                def __init__(self, right_month, **kwargs):
                    self.right_month = right_month
                    super(is_month, self).__init__(**kwargs)

                def evaluate(self, environ, credentials):
                    today = date.today()
                    if today.month != self.right_month:
                        # Raise an exception because the predicate is not met.
                        self.unmet()

        .. versionadded:: 1.0.2

        .. attention::
            Do not evaluate predicates by yourself using this method. See
            :meth:`check_authorization` and :meth:`is_met`.

        .. warning::

            To make your predicates thread-safe, keep in mind that they may
            be instantiated at module-level and then shared among many threads,
            so avoid predicates from being modified after their evaluation.
            This is, the ``evaluate()`` method should not add, modify or
            delete any attribute of the predicate.

        """
        self.eval_with_environ(environ)

    def _eval_with_environ(self, environ):
        """
        Check whether the predicate is met.

        :param environ: The WSGI environment.
        :type environ: dict
        :return: Whether the predicate is met or not.
        :rtype: bool
        :raise NotImplementedError: This must be defined by the predicate
            itself.

        .. deprecated:: 1.0.2
            Only :meth:`evaluate` will be used as of :mod:`repoze.what` v2.

        """
        raise NotImplementedError

    def eval_with_environ(self, environ):
        """
        Make sure this predicate is met.

        :param environ: The WSGI environment.
        :raises NotAuthorizedError: If the predicate is not met.

        .. versionchanged:: 1.0.1
            In :mod:`repoze.what`<1.0.1, this method returned a ``bool`` and
            set the ``error`` instance attribute of the predicate to the
            predicate message.

        .. deprecated:: 1.0.2
            Define :meth:`evaluate` instead.

        """
        from warnings import warn
        msg = 'Predicate._eval_with_environ(environ) is deprecated ' \
              'for forward compatibility with repoze.what v2; define ' \
              'Predicate.evaluate(environ, credentials) instead'
        warn(msg, DeprecationWarning, stacklevel=2)
        if not self._eval_with_environ(environ):
            self.unmet()

    def unmet(self, msg=None, **placeholders):
        """
        Raise an exception because this predicate is not met.

        :param msg: The error message to be used; overrides the predicate's
            default one.
        :type msg: str
        :raises NotAuthorizedError: If the predicate is not met.

        ``placeholders`` represent the placeholders for the predicate message.
        The predicate's attributes will also be taken into account while
        creating the message with its placeholders.

        For example, if you have a predicate that checks that the current
        month is the specified one, where the predicate message is defined with
        two placeholders as in::

            The current month must be %(right_month)s and it is %(this_month)s

        and the predicate has an attribute called ``right_month`` which
        represents the expected month, then you can use this method as in::

            self.unmet(this_month=this_month)

        Then :meth:`unmet` will build the message using the ``this_month``
        keyword argument and the ``right_month`` attribute as the placeholders
        for ``this_month`` and ``right_month``, respectively. So, if
        ``this_month`` equals ``3`` and ``right_month`` equals ``5``,
        the message for the exception to be raised will be::

            The current month must be 5 and it is 3

        If you have a context-sensitive predicate checker and thus you want
        to change the error message on evaluation, you can call :meth:`unmet`
        as::

            self.unmet('%(this_month)s is not a good month',
                       this_month=this_month)

        The exception raised would contain the following message::

            3 is not a good month

        .. versionadded:: 1.0.2

        .. versionchanged:: 1.0.4
            Introduced the ``msg`` argument.

        .. attention::

            This method should only be called from :meth:`evaluate`.

        """
        if msg:
            message = msg
        else:
            message = self.message
        # Let's convert it into unicode because it may be just a class, as a
        # Pylons' "lazy" translation message:
        message = unicode(message)
        # Include the predicate attributes in the placeholders:
        all_placeholders = self.__dict__.copy()
        all_placeholders.update(placeholders)
        raise NotAuthorizedError(message % all_placeholders)

    def check_authorization(self, environ):
        """
        Evaluate the predicate and raise an exception if it's not met.

        :param environ: The WSGI environment.
        :raise NotAuthorizedError: If it the predicate is not met.

        Example::

            >>> from repoze.what.predicates import is_user
            >>> environ = gimme_the_environ()
            >>> p = is_user('gustavo')
            >>> p.check_authorization(environ)
            # ...
            repoze.what.predicates.NotAuthorizedError: The current user must be "gustavo"

        .. versionadded:: 1.0.4
            Backported from :mod:`repoze.what` v2; deprecates
            :func:`repoze.what.authorize.check_authorization`.

        """
        logger = environ.get('repoze.who.logger')
        credentials = environ.get('repoze.what.credentials', {})
        try:
            self.evaluate(environ, credentials)
        except NotAuthorizedError, error:
            logger and logger.info(u'Authorization denied: %s' % error)
            raise
        logger and logger.info('Authorization granted')

    def is_met(self, environ):
        """
        Find whether the predicate is met or not.

        :param environ: The WSGI environment.
        :return: Whether the predicate is met or not.
        :rtype: bool

        Example::

            >>> from repoze.what.predicates import is_user
            >>> environ = gimme_the_environ()
            >>> p = is_user('gustavo')
            >>> p.is_met(environ)
            False

        .. versionadded:: 1.0.4
            Backported from :mod:`repoze.what` v2.

        """
        credentials = environ.get('repoze.what.credentials', {})
        try:
            self.evaluate(environ, credentials)
            return True
        except NotAuthorizedError, error:
            return False

    def parse_variables(self, environ):
        """
        Return the GET and POST variables in the request, as well as
        ``wsgiorg.routing_args`` arguments.

        :param environ: The WSGI environ.
        :return: The GET and POST variables and ``wsgiorg.routing_args``
            arguments.
        :rtype: dict

        This is a handy method for request-sensitive predicate checkers.

        It will return a dictionary for the POST and GET variables, as well as
        the `wsgiorg.routing_args
        <http://www.wsgi.org/wsgi/Specifications/routing_args>`_'s
        ``positional_args`` and ``named_args`` arguments, in the ``post``,
        ``get``, ``positional_args`` and ``named_args`` items (respectively) of
        the returned dictionary.

        For example, if the user submits a form using the POST method to
        ``http://example.com/blog/hello-world/edit_post?wysiwyg_editor=Yes``,
        this method may return::

            {
            'post': {'new_post_contents': 'These are the new contents'},
            'get': {'wysiwyg_editor': 'Yes'},
            'named_args': {'post_slug': 'hello-world'},
            'positional_args': (),
            }

        But note that the ``named_args`` and ``positional_args`` items depend
        completely on how you configured the dispatcher.

        .. versionadded:: 1.0.4

        """
        get_vars = parse_dict_querystring(environ) or {}
        try:
            post_vars = parse_formvars(environ, False) or {}
        except KeyError:
            post_vars = {}
        routing_args = environ.get('wsgiorg.routing_args', ([], {}))
        positional_args = routing_args[0] or ()
        named_args = routing_args[1] or {}
        variables = {
            'post': post_vars,
            'get': get_vars,
            'positional_args': positional_args,
            'named_args': named_args}
        return variables

