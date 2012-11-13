# -*- coding: utf-8 -*-

DEBUG = True

SAFE_ENDPOINTS = ('static', 'users.login', 'users.logout')
SUPER_USERS = ('root', 'admin')

SECRET_KEY = 'public'

SESSION_COOKIE_NAME = 'xiaoli'

SQLALCHEMY_DATABASE_URI = 'postgresql://ipon:public@localhost/ipon'

#SQLALCHEMY_BINDS = {
#    'report': 'sqlite://'
#}

SQLALCHEMY_POOL_SIZE = 5

SQLALCHEMY_POOL_TIMEOUT = 10

#=========================
# Cache config
#=========================

#simple: SimpleCache
#memcached: MemcachedCache (pylibmc required)
#gaememcached: GAEMemcachedCache
#redis: RedisCache (Werkzeug 0.7 required)
#filesystem: FileSystemCacheche config

CACHE_TYPE = "redis"


#Optional list to unpack and pass during the cache class instantiation.
#CACHE_ARGS = 

#Optional dictionary to pass during the cache class instantiation.
#CACHE_OPTIONS =    

#The default timeout that is used if no timeout is specified. Unit of time is seconds.
CACHE_DEFAULT_TIMEOUT = 300 

#The maximum number of items the cache will store before it starts deleting some. Used only for SimpleCache and FileSystemCache
#CACHE_THRESHOLD = 

#A prefix that is added before all keys. This makes it possible to use the same memcached server for different apps. Used only for MemcachedCache and GAEMemcachedCache.
#CACHE_KEY_PREFIX    

#A list or a tuple of server addresses. Used only for MemcachedCache
#CACHE_MEMCACHED_SERVERS =

#A Redis server host. Used only for RedisCache.
CACHE_REDIS_HOST = "127.0.0.1"

#A Redis server port. Default is 6379. Used only for RedisCache.
CACHE_REDIS_PORT = 6379

#A Redis password for server. Used only for RedisCache.
#CACHE_REDIS_PASSWORD =

#Directory to store cache. Used only for FileSystemCache.
#CACHE_DIR =


try:
    from local_settings import *
except Exception:
    pass

