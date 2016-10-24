############################################################
# Module  : simple demo over real data and a few words
# Date    : Oct. 19th
############################################################

from pulp import *
import math

############################################################
# Data
############################################################

words   = ["good", "better", "best"]

pairs   = [u+"-"+v for u in words for v in words if u != v]
synonym = [u+"-"+v for u in words for v in words if u == v]

# weak-strong patterns over words
P_ws    = {"good-better"  : [970 ,66445,0 ,0,13139,0,0,0],
           "good-best"    : [3057,249  ,46,0,137  ,0,0,0],
           "better-good"  : [1745,0    ,0 ,0,52   ,0,0,0],
           "better-best"  : [147 ,539  ,0 ,0,0    ,0,0,0],
           "best-good"    : [107 ,0    ,0 ,0,286  ,0,0,0],
           "best-better"  : [0   ,0    ,0 ,0,4077 ,0,0,0],

           "good-good"    : [11355,0   ,0,0,99 ,102,0,0],
           "better-better": [85   ,0   ,0,0,560,0  ,0,0],
           "best-best"    : [57   ,5558,0,0,0  ,0  ,0,0]}

# strong-weak patterns over words
P_sw    = {"good-better"  : [0,0,0,0,0,0,0],
           "good-best"    : [0,0,0,0,0,0,0],
           "better-good"  : [0,0,0,0,0,0,0],
           "better-best"  : [0,0,0,0,0,0,0],
           "best-good"    : [0,0,0,0,0,0,0],
           "best-better"  : [0,0,0,0,0,0,0],

           "good-good"    : [0,0,0,0,0,0,68238],
           "better-better": [0,0,0,0,0,0,0    ],
           "best-best"    : [0,0,0,0,0,0,0    ]}

# word count
cnt        = {"good":89827779, "better":157090854, "best":371880036}

# weak-strong patter over all words
Pattern_ws = [23017080,5254676,160986,327214,191796585,41506861,379849,45258]
Pattern_sw = [2627272,1193,162425,4499,0,0,1317774]

P1         = sum(Pattern_ws)
P2         = sum(Pattern_sw)
C          = 10000000000

############################################################
# Scores - naive implementation
############################################################

# score :: String -> String -> (Int,Int)
def score(a1,a2):

    w1 = sum(P_ws[a1 + "-" + a2])
    w2 = sum(P_ws[a2 + "-" + a1])
    s1 = sum(P_sw[a1 + "-" + a2])
    s2 = sum(P_sw[a2 + "-" + a1])

    top = P2 * (w1 - w2) - P1 * (s1 - s2)
    return (top, cnt[a1]*cnt[a2])

# unweighted scores and normalization constant
rscores = {"good-better"  : score("good"  ,"better"),
           "better-good"  : score("better","good"  ),
           "good-best"    : score("good"  ,"best"  ),
           "best-good"    : score("best"  ,"good"  ),
           "better-best"  : score("better","best"  ),
           "best-better"  : score("best"  ,"better")}
 
# unormalized scores
uscores = [("good_better" ,  2.2956386213990712e-5),
          ("better_good"  , -2.2956386213990712e-5),
          ("good_best"    ,  3.812091763438227e-7 ),
          ("best_good"    , -3.812091763438227e-7 ),
          ("better_best"  , -2.3875360906439436e-7),
          ("best_better"  ,  2.3875360906439436e-7)]

# normalized scores
mins   = min( [abs(x) for (_,x) in uscores] )

# actual score
# scores = [(n,x/mins) for (n,x) in uscores]
# scores = {'good_better' :  96.15094952470072,
#          'better_good'  : -96.15094952470072 ,
#          'good_best'    : 1.5966635136435008 ,
#          'best_good'    : -1.5966635136435008,
#          'better_best'  : -1.0               ,
#          'best_better'  : 1.0                }

# dummy score
scores = {'good_better' :  1, 
         'better_good'  : -1,
         'good_best'    :  2,
         'best_good'    : -2,
         'better_best'  :  3,
         'best_better'  : -3}
############################################################
# Program
############################################################

prob  = LpProblem("three words", LpMaximize)

# declare variables
x1 = LpVariable("x_good"  ,0,1,LpContinuous)
x2 = LpVariable("x_better",0,1,LpContinuous)
x3 = LpVariable("x_best"  ,0,1,LpContinuous)

w11 = LpVariable("w_good-good"    ,0,1,LpInteger)
w12 = LpVariable("w_good-better"  ,0,1,LpInteger)
w13 = LpVariable("w_good-best"    ,0,1,LpInteger)
w21 = LpVariable("w_better-good"  ,0,1,LpInteger)
w22 = LpVariable("w_better-better",0,1,LpInteger)
w23 = LpVariable("w_better-best"  ,0,1,LpInteger)
w31 = LpVariable("w_best-good"    ,0,1,LpInteger)
w32 = LpVariable("w_best-better"  ,0,1,LpInteger)
w33 = LpVariable("w_best-best"    ,0,1,LpInteger)


s11 = LpVariable("s_good-good"    ,0,1,LpInteger)
s12 = LpVariable("s_good-better"  ,0,1,LpInteger)
s13 = LpVariable("s_good-best"    ,0,1,LpInteger)
s21 = LpVariable("s_better-good"  ,0,1,LpInteger)
s22 = LpVariable("s_better-better",0,1,LpInteger)
s23 = LpVariable("s_better-best"  ,0,1,LpInteger)
s31 = LpVariable("s_best-good"    ,0,1,LpInteger)
s32 = LpVariable("s_best-better"  ,0,1,LpInteger)
s33 = LpVariable("s_best-best"    ,0,1,LpInteger)

d11 = LpVariable("d_11" ,0,1, LpContinuous)
d12 = LpVariable("d_12" ,0,1, LpContinuous)
d13 = LpVariable("d_13" ,0,1, LpContinuous)
d21 = LpVariable("d_21" ,0,1, LpContinuous)
d22 = LpVariable("d_22" ,0,1, LpContinuous)
d23 = LpVariable("d_23" ,0,1, LpContinuous)
d31 = LpVariable("d_31" ,0,1, LpContinuous)
d32 = LpVariable("d_32" ,0,1, LpContinuous)
d33 = LpVariable("d_33" ,0,1, LpContinuous)


# objective function
nonsyn = (w12 - s12)*scores[w12.name[2:]] + (w13 - s13) * scores[w13.name[2:]] + (w21 - s21) * scores[w21.name[2:]] + (w23 - s23) * scores[w23.name[2:]]+ (w31 - s31) * scores[w31.name[2:]] + (w32 - s32) * scores[w32.name[2:]]
syn    = (w11 + s11)*C + (w22 + s22)*C + (w33 + s33)*C
prob  += nonsyn 
# - syn

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


############################################################
# Scores - naive implementation
############################################################

def W1(a1,a2): return count(P_ws,P1,a1,a2)
def W2(a1,a2): return count(P_ws,P1,a2,a1)

def S1(a1,a2): return count(P_sw,P2,a1,a2)
def S2(a1,a2): return count(P_sw,P2,a2,a1)

def count(patterns,P,a1,a2):
    key = a1 + "-" + a2
    top = sum(patterns[key])
    return top/P

def score_old(a1,a2):
    top = (W1(a1,a2) - S1(a1,a2)) - (W2(a1,a2) - S2(a1,a2))
    bot = cnt[a1] * cnt[a2]
    return (top * 1000)/bot

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

solve(prob,[x1,x2,x3,w12,w13,w23,s21,s31,s32])

