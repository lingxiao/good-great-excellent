############################################################
# Module  : Pulp tutorial
# Source  : https://pythonhosted.org/PuLP/CaseStudies/a_blending_problem.html
# Date    : Sept 27th. 2016
############################################################

from pulp import *

############################################################
# Example one, simple formulation
############################################################

# LpProblem :: String -> Max | Min -> Problemn
probs = LpProblem("tutorial", LpMinimize)

# create variable
# LpVariable :: String 
#            -> Int       -- * LowerBound 
#            -> Int       -- * UpperBound 
#            -> LpInteger | LpContinuous
#            -> Variable

# we can inspect the fields by: 
# [ (k,v) for k,v in x.__dict__.iteritems() ]
x1   = LpVariable("chicken-percent", 0, 100, LpInteger)
x2   = LpVariable("beef-percent"   , 0, 100, LpInteger)

##### now we construct the problem #####

# first the objective function
probs += 0.013*x1 + 0.008*x2

# The five constraints are entered
probs += x1 + x2 == 100            , "PercentagesSum"
probs += 0.100*x1 + 0.200*x2 >= 8.0, "ProteinRequirement"
probs += 0.080*x1 + 0.100*x2 >= 6.0, "FatRequirement"
probs += 0.001*x1 + 0.005*x2 <= 2.0, "FibreRequirement"
probs += 0.002*x1 + 0.005*x2 <= 0.4, "SaltRequirement"

# solve, check status and print output
def solve(prob):
    prob.writeLP(prob.name + ".lp")
    prob.solve()
    print "status: " + LpStatus[prob.status]
    for v in prob.variables():
        print (v.name, "= ", v.varValue)


############################################################
# Example Two, full formulation
############################################################

# list of ingredients
ingredients = ['CHICKEN', 'BEEF', 'MUTTON', 'RICE', 'WHEAT', 'GEL']

# dictionary of ingriendients to attributes
costs = { 'CHICKEN': 0.013
        , 'BEEF'   : 0.008
        , 'MUTTON' : 0.010
        , 'RICE'   : 0.002
        , 'WHEAT'  : 0.005
        , 'GEL'    : 0.001
        }

protein = { 'CHICKEN': 0.1
          , 'BEEF'   : 0.2
          , 'MUTTON' : 0.15
          , 'RICE'   : 0.0
          , 'WHEAT'  : 0.04
          , 'GEL'    : 0.0
          }


fat   = { 'CHICKEN': 0.08
        , 'BEEF'   : 0.1
        , 'MUTTON' : 0.11
        , 'RICE'   : 0.01
        , 'WHEAT'  : 0.01
        , 'GEL'    : 0.0
        }

fiber = { 'CHICKEN': 0.001
        , 'BEEF'   : 0.005
        , 'MUTTON' : 0.003
        , 'RICE'   : 0.1
        , 'WHEAT'  : 0.15
        , 'GEL'    : 0.0
        }

salt  = { 'CHICKEN': 0.002
        , 'BEEF'   : 0.005
        , 'MUTTON' : 0.007
        , 'RICE'   : 0.002
        , 'WHEAT'  : 0.008
        , 'GEL'    : 0.000
        }


# declare problem
prob = LpProblem("full problem formulation", LpMinimize)

# variables
# Lp.Varible.dicts :: String 
#                  -> [String] 
#                  -> Int        -- * lowerbound on variables
#                  -> Dict
xs  = LpVariable.dicts("ingredients", ingredients,0)

# objective function encoded
prob += lpSum([ costs[i]*xs[i] for i in ingredients])

# constraints encoded:
# The five constraints are added to 'prob'
prob += lpSum([xs[i] for i in ingredients])                     == 100, "PercentagesSum"
prob += lpSum([protein[i] * xs[i] for i in ingredients]) >= 8.0, "ProteinRequirement"
prob += lpSum([fat[i]     * xs[i] for i in ingredients]) >= 6.0, "FatRequirement"
prob += lpSum([fiber[i]   * xs[i] for i in ingredients]) <= 2.0, "FibreRequirement"
prob += lpSum([salt[i]    * xs[i] for i in ingredients]) <= 0.4, "SaltRequirement"












    
