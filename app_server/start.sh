#!/bin/sh
erl +P 102400 +K true -boot start_sasl -pa ebin -s app_server start -name jarvis_server@192.168.24.89 -setcookie jarvis -env ERL_MAX_ETS_TABLES 100000
