RabbitMQ
========

http://www.rabbitmq.com/

RabbitMQ Config
=============

./rabbitmqctl add_user xiaoli public
./rabbitmqctl add_vhost /xiaoli
./rabbitmqctl set_permissions -p /xiaoli xiaoli ".*" ".*" ".*"
./rabbitmqctl list_exchanges -p /xiaoli

