"""
Tango
=====
Tango is micro application framework for enterprise software.

:copyright: Copyright (c) 2012 by Ery Lee <ery.lee@gmail.com> and OPENGOSS.COM
:license: BSD, see LICENSE.txt for details.
"""

from tango.models import db

from tango.base import Tango, Page

from tango.base import user_profile

from tango.login import LoginManager

login_mgr = LoginManager()

__version__ = '0.1.0'
