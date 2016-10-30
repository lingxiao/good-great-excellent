############################################################
# Module  : simple version of adjective ranking with two words
# Date    : Sept 27th. 2016
############################################################

from pulp import *

############################################################
# Constants
############################################################

# score between good
score12 =  96.15094952470072
score21 = -96.15094952470072
C       = 10000000

############################################################
# Manually set up problem where E = {}
############################################################

prob = LpProblem("two words", LpMaximize)

# declare variable
w12 = LpVariable("w_12",0,1, LpInteger)
w21 = LpVariable("w_21",0,1, LpInteger)

s12 = LpVariable("s_12",0,1, LpInteger)
s21 = LpVariable("s_21",0,1, LpInteger)

x1  = LpVariable("x_1"  ,0,1, LpContinuous)
x2  = LpVariable("x_2"  ,0,1, LpContinuous)

d12 = LpVariable("d_12" ,0,1, LpContinuous)
d21 = LpVariable("d_21" ,0,1, LpContinuous)

# objective function - for now we don't care about synonymy
prob += (w12 - s12)* score12 + (w21 - s21) * score21
# - C*(w11 + s11)

# constraints
prob += d12 == x2 - x1
prob += d21 == x1 - x2

prob += d12 - w12*C <= 0
prob += d21 - w21*C <= 0

prob += d12 + (1 - w12)*C > 0
prob += d21 + (1 - w21)*C > 0

prob += d12 + s12*C      >= 0
prob += d21 + s21*C      >= 0

prob += d12 - (1 - s12)*C <= 0 
prob += d21 - (1 - s21)*C <= 0 


############################################################
# solve
############################################################

# solve, check status and print output
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


solve(prob,[x1,x2,w12,w21,s12,s21])




    
