
# Install pacakges #

## Virtualenv ##
``` sh
    pip install virtualenv
    cd /opt/xiaoli-env/
    virtualenv XIAOLI
    cd XIAOLI
    source bin/activate
```


## Setup Github ##
[Setup Git](https://help.github.com/articles/set-up-git)
[Generate SSH keys](https://help.github.com/articles/generating-ssh-keys)


## Install python modules ##

``` sh
    pip install -r deps.txt
```

## Test Running ##
*ImportError: libpq.so.5: cannot open shared object file: No such file or directory*
Solution:

``` sh
    locate libpq.so.5
    export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/opt/PostgreSQL/9.0/lib
```

Add `local_settings.py` to `xiaoli/web/`

``` python
    #coding=utf-8
    SQLALCHEMY_DATABASE_URI = 'postgresql://postgres:postgres@192.168.100.71/ipon'
```

## Deploy stuff ##
1. Install lastest Nginx
``` sh
    # Download PCRE
    wget http://iweb.dl.sourceforge.net/project/pcre/pcre/8.31/pcre-8.31.tar.gz
    tar -zxvf pcre-8.31.tar.gz
    
    wget http://nginx.org/download/nginx-1.2.4.tar.gz
    tar -zxvf nginx-1.2.4.tar.gz
    cd nginx-1.2.4
    ./configure --prefix=SOME-PATH --with-pcre=PATH-TO-PCRE-SRC
    make
    make install
```   
2. copy web/deploy/conf/nginx.conf --> `NGINX-CONFIG-FOLDER/nginx.conf`

