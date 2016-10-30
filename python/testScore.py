############################################################
# Module: Test scores
# Author: Xiao Ling
# Date  : October 28th, 2016
############################################################

import readData
import pytest

############################################################
# Ma1n
############################################################

def ma1n():

    tgoScore  ()

    print "============================="
    print "passed all tests"
    return True

############################################################
# Dummy data
############################################################


def tgoScore():

    # suppose a1 > a2
  count  = {"a1"   : 100.0, "a2": 80.0}

  strong = { "a1-a1": [3.0,1.0,1.0] \
           , "a1-a2": [10.0,20.0,30.0]  
           , "a2-a1": [1.0,1.0,1.0]
           , "a2-a2": [0,0,1.0]}


  weak   = { "a1-a1": [3.0,0,0] \
           , "a1-a2": [1.0,2.0,3.0]  
           , "a2-a1": [10.0,20.0,50.0]
           , "a2-a2": [9.0,0,1.0]}

  P1 = 1000
  P2 = 1200

  W1 = sum(weak  ['a1-a2'])/P1
  W2 = sum(weak  ['a2-a1'])/P1
  S1 = sum(strong['a1-a2'])/P2
  S2 = sum(strong['a2-a1'])/P2

  s1  = ((W1 - S1) - (W2 - S2))/(count['a1']*count['a2'])
  s2 = goScores(count,strong,weak,P1,P2,"a1","a2")
  s3 = goScores(count,strong,weak,P1,P2,"a2","a1")
  s4 = goScores(count,strong,weak,P1,P2,"a1","a1")
  s5 = goScores(count,strong,weak,P1,P2,"a2","a2")

  # correct compuation
  assert s1      == s2
  # symmetry
  assert abs(s2) == abs(s1)
  # reflexivity
  assert s4 == 0.0
  assert s5 == 0.0

  print "passed goScores"
  return True











