#!/bin/sh
erl -noshell -pa ebin/ -run app_tool main 0 -s init stop
