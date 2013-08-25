#!/bin/sh
cd `dirname $0`
exec erl -sname chromechat -pa $PWD/ebin $PWD/deps/*/ebin -boot start_sasl -s chromechat
