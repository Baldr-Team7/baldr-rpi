
#!/bin/sh
# -*- tab-width:4;indent-tabs-mode:nil -*-
# ex: ts=4 sw=4 et

erl -pa ebin -pa deps/*/ebin -eval "application:start(baldr_rpi)"