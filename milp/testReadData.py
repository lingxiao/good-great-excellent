############################################################
# Module: Test readData.py
# Author: Xiao Ling
# Date  : October 28th, 2016
############################################################

import readData
import pytest

############################################################
# Main
############################################################

def main():

    tweakOver  ()
    tstrongOver()

    print "============================="
    print "passed all tests"
    return True

############################################################
# tests
############################################################

patternDir    = "/Users/lingxiao/Documents/research/code/good-great-excellent/inputs/"
strongWeakDir = "/Users/lingxiao/Documents/research/code/good-great-excellent/out/strong-weak-words"
weakStrongDir = "/Users/lingxiao/Documents/research/code/good-great-excellent/out/weak-strong-words"
CON           = config(patternDir, weakStrongDir, strongWeakDir)
words         = ["good","better","best"]

# @Read: Functions are tested against manally compiled data
def tweakOver():
    P_ws = {"good-better"  : [970 ,66445,0 ,0,13139,0,0,0],
            "good-best"    : [3057,249  ,46,0,137  ,0,0,0],
            "better-good"  : [1745,0    ,0 ,0,52   ,0,0,0],
            "better-best"  : [147 ,539  ,0 ,0,0    ,0,0,0],
            "best-good"    : [107 ,0    ,0 ,0,286  ,0,0,0],
            "best-better"  : [0   ,0    ,0 ,0,4077 ,0,0,0],

            "good-good"    : [11355,0   ,0,0,99 ,102,0,0],
            "better-better": [85   ,0   ,0,0,560,0  ,0,0],
            "best-best"    : [57   ,5558,0,0,0  ,0  ,0,0]}

    test = weakOver(CON,words)

    for aij in P_ws:
        assert P_ws[aij] == test[aij]

    print "passed weakOver"
    return True

def tstrongOver():
    P_sw    = {"good-better"  : [0,0,0,0,0,0,0],
               "good-best"    : [0,0,0,0,0,0,0],
               "better-good"  : [0,0,0,0,0,0,0],
               "better-best"  : [0,0,0,0,0,0,0],
               "best-good"    : [0,0,0,0,0,0,0],
               "best-better"  : [0,0,0,0,0,0,0],

               "good-good"    : [0,0,0,0,0,0,68238],
               "better-better": [0,0,0,0,0,0,0    ],
               "best-best"    : [0,0,0,0,0,0,0    ]}

    test = strongOver(CON,words)

    for aij in P_sw:
        assert P_sw[aij] == test[aij]

    print "passed strongOver"
    return True


