# coding: utf-8

from .cache import cache

from .models import Profile

from .login import current_user

def get_profile(grp):
    """ Get current user's profile. """
    return cached_profile(current_user.id, grp)

@cache.memoize(3600)
def cached_profile(uid, grp):
    return Profile.load(uid, grp)

def update_profile(grp, key, val):
    uid = current_user.id
    Profile(uid, grp, key, val).update()
    cache.delete_memoized(cached_profile, uid, grp)

