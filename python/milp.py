############################################################
# Module  : milp solve for this paper
# Date    : Oct. 23rd
############################################################

from pulp import *
import math


############################################################
# LpProblem Object
############################################################

# Given a Dict mapping Strings to Float, constructed by 
# score function in score.py, construct a wrapper
# around pulp's LpProblem
class PaperMilp(object):

  def __init__(self,scores):
    self.Scores = scores
    self.Raw    = paperMilp(scores)

  def score(self):
    return self.Scores

  def solve(self):
    self.Raw.solve()
    return self

  def prettyPrint(self,words):
    print ("ranking words: ")
    rs = rankAll(self.Raw,words)
    print rs
    print "=====================\n"
    return rs

  def show(self):
    prettyPrint(self.Raw)
    return self


############################################################
# Construct the LpProblem
############################################################

# @Use: Given a dictionary mapping word pairs to frequencies, 
# output an instance of pulp milp class

# @Example: milp({"good_great": 1, "great_good": -1})

# paperMilp :: Dict String Int -> MilpProblem
def paperMilp(scores):

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
# Interpret the LpProblem
############################################################

# @Use: given list of words `ws`, output list of list
#       of words, where list_i in front of list_j
#       if every word in list_i is less than list_j
#       under `rank`,
#       and every word in list_i is of same intensity
#       under `rank`
# 
# @rankAll :: LpProblem -> [String] -> [[String]]
def rankAll(rank,xs):
  xxs = []
  for x in xs:
    xxs = insertWord(rank,x,xxs)
  return xxs 


# @Use: given `word_i` and `wordss`, a list of ranked words
#       where each sublist in wordss is a list of words of 
#       equal intensity, insert [word_i] into the appropriate
#       place in `wordss`
# 
# @Output: output modified wordss or False if a place for word_i
#          cannot be found

# insertWord :: LpProblem -> String -> [[String]] -> [[String]] | False
def insertWord(rank,word_i,wordss):

  if not wordss: return [[word_i]]
  else:
    for words in wordss:
      if same(rank,word_i,words):
        xxs    = deepcopy(wordss)
        i      = xxs.index(words)
        words  = words + [word_i]
        xxs[i] = words
        return xxs

      elif weaker(rank,word_i,words):
        return insertFront([word_i],words,wordss)

      elif not weaker(rank,word_i,words):
        return insertBehind([word_i],words,wordss)

    return False


# @Use: give word `ai` and list of words `words`, return True
#       if `ai` is `weaker` than every word in `bs` under
#       `rank`, a solved LpProblem instance 
#       else return False

# weaker :: LpProblem -> String -> [String] -> Bool
def weaker(rank,ai,words):
  ret = True
  for ak in words:
    wik = varValue(rank,'w_' + ai + "_" + ak)
    sik = varValue(rank,'s_' + ai + "_" + ak)
    ret = ret and wik == 1 and sik == 0
  return ret

# @Use: give word `ai` and list of words `words`, return True
#       if `ai` is `same` as every `ak` in words under
#       solved LpProblem instance `rank`
#       else return False

# same :: LpProblem -> String -> [String] -> Bool
def same(rank,ai,words):
  ret = True
  for ak in words:
    wik = varValue(rank,'w_' + ai + "_" + ak)
    sik = varValue(rank,'s_' + ai + "_" + ak)
    ret = ret and wik == 0 and sik == 0
  return ret


# @Use: Given word `ai` to be infronted infront of
#       of word `ak` in list `xs`
#       insert ai infront of ak if ak exist in list
#       else ouptut list unchanged
#  
# @Out: list with ai inserted or False

# insertFront :: a -> a -> [a] -> [a]
def insertFront(ai,ak,xs):
  if not xs:
    return []
  else:
    a = xs[0]
    if ak != a: 
      ys = insertFront(ai,ak,xs[1:])
      return [a] + ys
    else:
      return [ai,ak] + xs[1:]   


# @Use: Given word `ai` to be infronted behind
#       of word `ak` in list `xs`
#       insert ai behind ak if ak exist in list
#       else ouptut list unchanged
#  
# @Out: list with ai inserted or False

# insertBehind :: a -> a -> [a] -> [a]
def insertBehind(ai,ak,xs):
  if not xs:
    return []
  else:
    a = xs[0]
    if ak != a: 
      ys = insertBehind(ai,ak,xs[1:])
      return [a] + ys
    else:
      return [ak,ai] + xs[1:]   


# varValue :: LpProblem -> String -> Error String Float 
def varValue(rank,name):
  ret = [x.varValue for x in rank.variables() \
                  if x._LpElement__name == name]
  if ret: return ret[0]
  else  : raise NameError(name + " not found")


############################################################
# Pretty Print 
############################################################

# @Use: Given an instance of Milp problem, solve it and 
#       print output to console

# @Example: p = solve(milp({"good_great": 1, "great_good": -1}))

# prettyPrint :: MilpProblem -> Eff [State MilpProblem, IO ()] MilpProblem
def prettyPrint(prob):

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






