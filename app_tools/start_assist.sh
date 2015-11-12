#!/bin/sh
erl -noshell -pa ebin/ -run app_assist main 1 -s init stop
