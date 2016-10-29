############################################################
# Module  : milp solve for this paper
# Date    : Oct. 23rd
############################################################

import pulp
import math

############################################################
# Construct the milp problem
############################################################

# @Use: Given a dictionary mapping word pairs to frequencies, 
# output an instance of pulp milp class

# @Example: milp({"good_great": 1, "great_good": -1})

# milp :: Dict String Int -> MilpProblem
def milp(scores):

  ######################################
  # compute constants and get words

  # construct constant so that "C is a very large constant 
  # greater than Sum_{ij} |score(ai,aj)|"
  C = 0
  for key, n in scores.iteritems():
    C += abs(n)

  C = C * 10

  # pull out the keys in scores dictionary
  # and construct its powerset
  words  = _toWords(scores)
  wwords = [u + '_' + v for u in words for v in words]

  ######################################
  # Construct the milp Program

  prob = LpProblem("milp problem", LpMaximize)

  # variable names
  xs = ['x_' + n for n in words ]   
  ds = ['d_' + n for n in wwords]
  ws = ['w_' + n for n in wwords]
  ss = ['s_' + n for n in wwords]

  # pulp variabbles
  x = LpVariable.dict("x", words , 0, 1, LpContinuous)
  d = LpVariable.dict("d", wwords, 0, 1, LpContinuous)
  w = LpVariable.dict('w', wwords, 0, 1, LpInteger   )
  s = LpVariable.dict('s', wwords, 0, 1, LpInteger   )

  # objective function
  obj  = [ (w[ij] - s[ij]) * scores[ij] for ij in wwords if _iNotj(ij)    ] \
       + [ (w[ii] + s[ii]) * -C         for ii in wwords if not _iNotj(ii)]

  prob += lpSum(obj)

  # constraints
  C1 = [ d[i + "_" + j] - x[j] + x[i]  for i in words for j in words]    # xj - xi          = dij
  C2 = [ (d[ij] - w[ij])*C             for ij in wwords             ]    # dij - wij * C    <= 0
  C3 = [ (d[ij] + (1 - w[ij]))*C       for ij in wwords             ]    # dij + (1- wij)*C >= 0
  C4 = [ (d[ij] + s[ij])*C             for ij in wwords             ]    # dij + sij*C      >= 0
  C5 = [ (d[ij] - (1-s[ij]))*C         for ij in wwords             ]    # dij - (1 - sij)*C < 0

  prob += lpSum(C1) == 0
  prob += lpSum(C2) <= 0
  prob += lpSum(C3) >= 0   # TODO: this here might be causing all x's to become 0.0
  prob += lpSum(C4) >= 0
  prob += lpSum(C5) <= 0

  return prob

############################################################
# run the milp problem and print solutions
############################################################

# @Use: Given an instance of Milp problem, solve it and 
#       print output to console

# @Example: p = solve(milp({"good_great": 1, "great_good": -1}))

# solve :: MilpProblem -> Eff [State MilpProblem, IO ()] MilpProblem
def solve(prob):

    prob.solve()

    print "======================================="

    print "status: " + LpStatus[prob.status]

    print "========= All variable values ========="

    for v in prob.variables():
        print (v.name, "= ", v.varValue)

    return prob


############################################################
# helper functions
############################################################

 # Given string of form "i_j", check if i == j
# _iNotj :: String -> Bool
def _iNotj(xs):
  words = xs.split("_")
  if len(words) == 2: return words[0] != words[1]
  else           : return False

# _toWords :: Dict String Int -> [String]
def _toWords(scores):

  words = set()

  for key, val in scores.iteritems():
    ws = key.split("_")

    if len(ws) == 2:
      words.add(ws[0])
      words.add(ws[1])

  return words






