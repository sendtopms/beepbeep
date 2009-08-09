#!/bin/sh
cd `dirname $0`

exec erl -heart -detached -pa $PWD/ebin $PWD/../../deps/*/ebin $PWD/../../ebin -boot start_sasl -s reloader -s beep
