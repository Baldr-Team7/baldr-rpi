#!/bin/sh

rsync -rvt --exclude='*/ebin' --exclude='*/erlang_ale/priv'  --exclude='*/jiffy/priv' --exclude='*.o' --exclude='*/.beam' * 10.42.0.111:/home/pi/baldr-rpi
