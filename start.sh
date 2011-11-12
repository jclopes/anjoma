#! /usr/bin/env bash

erl -pz ebin -hms1500m -smp disable -noshell -s my_bot start -s init stop

# test with smp enable
#erl -pz ebin -hms1500m -noshell -s my_bot start -s init stop
