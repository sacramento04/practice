#!/bin/sh
IPADDR=`LC_ALL=C ifconfig  | grep 'inet addr:'| grep -v '127.0.0.1' | cut -d: -f2 | awk '{ print $1}'`
erl +P 102400 +K true -boot start_sasl -pa ebin -s app_client start -name app_client@${IPADDR} -setcookie jarvis -env ERL_MAX_ETS_TABLES 100000
