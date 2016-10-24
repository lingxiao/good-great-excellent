############################################################
# Module  : simple demo over real data and a few words
# Date    : Oct. 19th
############################################################

from pulp import *
import math
from milp import milp, run

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
scores = [(n,x/mins) for (n,x) in uscores]
scores = {'good_better' :  96.15094952470072,
         'better_good'  : -96.15094952470072 ,
         'good_best'    : 1.5966635136435008 ,
         'best_good'    : -1.5966635136435008,
         'better_best'  : -1.0               ,
         'best_better'  : 1.0                }

############################################################
# Program
############################################################

