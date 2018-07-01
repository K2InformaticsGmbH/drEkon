#!/bin/bash

if [[ "$unamestr" == 'Linux' || "$unamestr" == 'Darwin' ]]; then
     exename=erl
else
    exename='start werl.exe'
    #exename='erl.exe'
fi

# PATHS
paths="-pa"
paths=$paths" $PWD/_build/default/lib/*/ebin"

start_opts="$paths"

# Starting emtt
echo $exename $start_opts -s drEkon
$exename $start_opts -s drEkon
