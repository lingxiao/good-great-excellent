############################################################
# TUTORIAL: PULP
# Source  : https://pythonhosted.org/PuLP/CaseStudies/a_blending_problem.html
# Date    : Sept 27th. 2016
############################################################

from pulp import *

############################################################
# Example one
############################################################


# LpProblem :: String -> Max | Min -> Problem
prob = LpProblem("tutorial", LpMinimize)

# create variable
# LpVariable :: String 
#            -> Int       -- * LowerBound 
#            -> Int       -- * UpperBound 
#            -> LpInteger | LpContinous
#            -> Variable

# we can inspect the fields by: 
# [ (k,v) for k,v in x.__dict__.iteritems() ]
x1   = LpVariable("chicken-percent", 0, None, LpInteger)
x2   = LpVariable("beef-percent"   , 0, None, LpInteger)

# now we construct the problem

# first the objective function
prob += 0.013*x1 + 0.008*x2

# The five constraints are entered
prob += x1 + x2 == 100,             "PercentagesSum"
prob += 0.100*x1 + 0.200*x2 >= 8.0, "ProteinRequirement"
prob += 0.080*x1 + 0.100*x2 >= 6.0, "FatRequirement"
prob += 0.001*x1 + 0.005*x2 <= 2.0, "FibreRequirement"
prob += 0.002*x1 + 0.005*x2 <= 0.4, "SaltRequirement"

# solve problem
prob.solve()

# check status and print output
def showProb(prob):
    print "status: " + LpStatus[prob.status]
    for v in prob.variables():
        print (v.name, "= ", v.varValue)







    
