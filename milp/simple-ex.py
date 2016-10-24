############################################################
# Module  : simple version of adjective ranking with two words
# Date    : Sept 27th. 2016
############################################################

from pulp import *

############################################################
# Constants
############################################################

c1 = 0
c2 = 0
c3 = -1


############################################################
# Manually set up problem where E = {(good,good),(great,great)}
############################################################

# declare problem
prob = LpProblem("simple example", LpMinimize)

# declare variables
x1 = LpVariable("x1", 0, None, LpContinuous)
x2 = LpVariable("x2", 0, None, LpContinuous)
x3 = LpVariable("x3", 0, None, LpContinuous)

# objective function
prob += c1 * x1 + c2 * x2 + c3 * x3

# constraints
prob += x1 + x2     >= 1
prob += x1 + 2 * x2 <= 3


############################################################
# solve
############################################################


# solve(prob,[x1,x2,x3])

def solve(prob,xs):
    # prob.writeLP(prob.name + ".lp")
    prob.solve()

    print "======================================="


    print "status: " + LpStatus[prob.status]

    if xs:
        print "======== Select variable values ======="
        for x in xs:
            print (x.name, "= ", x.varValue)

    print "========= All variable values ========="

    for v in prob.variables():
        print (v.name, "= ", v.varValue)


