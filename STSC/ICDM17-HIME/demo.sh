#!/bin/bash


time java -Xmx8g -jar HIME.jar dishwasher.txt 4 300  > tmp.log
grep -i "Rule" tmp.log | cut -d' ' -f 2- > res.txt
