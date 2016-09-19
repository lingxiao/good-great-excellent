#!/bin/bash

set -x


# make sure I set my $CWD (current working directory)
cd /Users/lingxiao/Documents/research/data/ngrams/raw/1gms

nohpu grep "hello" *txt >> out.txt

