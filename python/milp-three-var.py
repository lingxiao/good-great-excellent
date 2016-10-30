############################################################
# Module  : simple demo over real data and a few words
# Date    : Oct. 19th
# Source  : https://pythonhosted.org/PuLP/CaseStudies/a_blending_problem.html
############################################################

import milp
import pulp

############################################################
# Scores - naive implementation
############################################################

# dummy score
scores = {'1_2'  :  1, 
          '2_1'  : -1,
          '1_3'  :  2,
          '3_1'  : -2,
          '2_3'  :  3,
          '3_2'  : -3}

scores3 = {'1_2'  :  1, 
           '2_1'  : -1,
           '1_3'  :  2,
           '3_1'  : -2,
           '1_4'  :  4,
           '4_1'  : -4,
           '2_3'  :  3,
           '3_2'  : -3,
           '2_4'  :  5,
           '4_2'  : -5,
           '3_4'  :  6,
           '4_3'  : -6}

############################################################
# run the milp problem and print solutions
############################################################

# fourvar  = milp.solve(milp.solve(scores3))

# fourvar = milp.solve(milp.milp(scores))


############################################################
# Program Manual
############################################################


C = 0
for key, n in scores.iteritems():
  C += abs(n)

C = C * 10

############################################################
# Program Manual
############################################################

prob  = LpProblem("three words", LpMaximize)

# declare variables
x1 = LpVariable("x1"  ,0,1,LpContinuous)
x2 = LpVariable("x2"  ,0,1,LpContinuous)
x3 = LpVariable("x3"  ,0,1,LpContinuous)

w11 = LpVariable("w11"  ,0,1,LpInteger)
w12 = LpVariable("w12"  ,0,1,LpInteger)
w13 = LpVariable("w13"  ,0,1,LpInteger)
w21 = LpVariable("w21"  ,0,1,LpInteger)
w22 = LpVariable("w22"  ,0,1,LpInteger)
w23 = LpVariable("w23"  ,0,1,LpInteger)
w31 = LpVariable("w31"  ,0,1,LpInteger)
w32 = LpVariable("w32"  ,0,1,LpInteger)
w33 = LpVariable("w33"  ,0,1,LpInteger)


s11 = LpVariable("s11"  ,0,1,LpInteger)
s12 = LpVariable("s12"  ,0,1,LpInteger)
s13 = LpVariable("s13"  ,0,1,LpInteger)
s21 = LpVariable("s21"  ,0,1,LpInteger)
s22 = LpVariable("s22"  ,0,1,LpInteger)
s23 = LpVariable("s23"  ,0,1,LpInteger)
s31 = LpVariable("s31"  ,0,1,LpInteger)
s32 = LpVariable("s32"  ,0,1,LpInteger)
s33 = LpVariable("s33"  ,0,1,LpInteger)

d11 = LpVariable("d11" ,0,1, LpContinuous)
d12 = LpVariable("d12" ,0,1, LpContinuous)
d13 = LpVariable("d13" ,0,1, LpContinuous)
d21 = LpVariable("d21" ,0,1, LpContinuous)
d22 = LpVariable("d22" ,0,1, LpContinuous)
d23 = LpVariable("d23" ,0,1, LpContinuous)
d31 = LpVariable("d31" ,0,1, LpContinuous)
d32 = LpVariable("d32" ,0,1, LpContinuous)
d33 = LpVariable("d33" ,0,1, LpContinuous)


# objective function
nonsyn = (w12 - s12)*scores['1_2'] \
       + (w13 - s13)*scores['1_3'] \
       + (w21 - s21)*scores['2_1'] \
       + (w23 - s23)*scores['2_3'] \
       + (w31 - s31)*scores['3_1'] \
       + (w32 - s32)*scores['3_2']

syn    = (w11 + s11)*C \
       + (w22 + s22)*C \
       + (w33 + s33)*C

prob  += nonsyn - syn

# constraints
prob += d11 == x1 - x1
prob += d12 == x2 - x1
prob += d13 == x3 - x1
prob += d21 == x1 - x2
prob += d22 == x2 - x2
prob += d23 == x3 - x2
prob += d31 == x1 - x3
prob += d32 == x2 - x3
prob += d33 == x3 - x3

prob += d11 - w11*C <= 0
prob += d12 - w12*C <= 0
prob += d13 - w13*C <= 0
prob += d21 - w21*C <= 0
prob += d22 - w22*C <= 0
prob += d23 - w23*C <= 0
prob += d31 - w31*C <= 0
prob += d32 - w32*C <= 0
prob += d33 - w33*C <= 0

prob += d11 + (1 - w11)*C > 0
prob += d12 + (1 - w12)*C > 0
prob += d13 + (1 - w13)*C > 0
prob += d21 + (1 - w21)*C > 0
prob += d22 + (1 - w22)*C > 0
prob += d23 + (1 - w23)*C > 0
prob += d31 + (1 - w31)*C > 0
prob += d32 + (1 - w32)*C > 0
prob += d33 + (1 - w33)*C > 0


prob += d11 + s11*C      >= 0
prob += d12 + s12*C      >= 0
prob += d13 + s13*C      >= 0
prob += d21 + s21*C      >= 0
prob += d22 + s22*C      >= 0
prob += d23 + s23*C      >= 0
prob += d31 + s31*C      >= 0
prob += d32 + s32*C      >= 0
prob += d33 + s33*C      >= 0

prob += d11 - (1 - s11)*C <= 0 
prob += d12 - (1 - s12)*C <= 0 
prob += d13 - (1 - s13)*C <= 0 
prob += d21 - (1 - s21)*C <= 0 
prob += d22 - (1 - s22)*C <= 0 
prob += d23 - (1 - s23)*C <= 0 
prob += d31 - (1 - s31)*C <= 0 
prob += d32 - (1 - s32)*C <= 0 
prob += d33 - (1 - s33)*C <= 0 
