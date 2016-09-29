############################################################
# Module  : simple version of adjective ranking tutorial
# Source  : https://pythonhosted.org/PuLP/CaseStudies/a_blending_problem.html
# Date    : Sept 27th. 2016
############################################################

from pulp import *

############################################################
# Constants
############################################################

# Arbitrary constant
C   = 10^10

# P1 := sum_{p in Pws} cnt(p(*,*))
P1 = 415502394.0

# P2:= sum_{p in Psw} cnt(p(*,*))
P2 = 3260968.0   


# sum of occurences of p_ws(good,great)
W_good_great = 84862.0/P1

# sum of occurences of great `p_ws` good
W_great_good = 1829.0/P1

# sum of occurences of good `s_ws` great
S_good_great = 138.0/P2

# sum of occurences of great `s_ws` good
S_great_good = 384.0/P2

# cnt(good), cn(great)
ngood  = 89827779.0
ngreat = 106080147.0


# scores from corpus data
R11 = 0        # good-good
R22 = 0        # great-great

# good-great
# R12 = ((W_good_great - S_good_great) - (W_great_good - S_great_good))/(ngood*ngreat)
# great-good
# R21 = ((W_great_good - S_great_good) - (W_good_great - S_good_great))/(ngood*ngreat)
R12 = 1
R21 = -1

############################################################
# Manually set up problem where E = {(good,good),(great,great)}
############################################################

# Set up the Lp problem
paper  = LpProblem("rank: {good,great}, E = {}", LpMaximize)

# manually set up all possible relations among two words
w11 = LpVariable("weak:good-good"    , 0,1, LpInteger)
w12 = LpVariable("weak:good-great"   , 0,1, LpInteger)
w21 = LpVariable("weak:great-good"   , 0,1, LpInteger)
w22 = LpVariable("weak:great-great"  , 0,1, LpInteger)

s11 = LpVariable("strong:good-good"  , 0,1, LpInteger)
s12 = LpVariable("strong:good-great" , 0,1, LpInteger)
s21 = LpVariable("strong:great-good" , 0,1, LpInteger)
s22 = LpVariable("strong:great-great", 0,1, LpInteger)

# continous variables
x1  = LpVariable("good" , 0, 1)
x2  = LpVariable("great", 0, 1)

d11 = LpVariable("dist:good-good"  , 0, 1)
d12 = LpVariable("dist:good-great" , 0, 1)
d21 = LpVariable("dist:great-great", 0, 1)
d22 = LpVariable("dist:great-good" , 0, 1)


# # # objective function # # #
antonym = (w12 - s12)*R12 + (w21 - s21)*R21

# this value is actually zero since Rii for every i
# synonym = (w11 + s11)*R11 + (w22 - s22)*R22  
paper  += antonym   # + C*synonym

# # # constraints # # #

# distance constraints
paper += d11 == x1 - x1
paper += d12 == x1 - x2
paper += d21 == x2 - x1
paper += d22 == x2 - x2


paper += d11 - w11*C <= 0
paper += d12 - w12*C <= 0
paper += d21 - w21*C <= 0
paper += d22 - w22*C <= 0


paper += d11 + (1-w11)*C > 0
paper += d12 + (1-w12)*C > 0
paper += d21 + (1-w21)*C > 0
paper += d22 + (1-w22)*C > 0


paper += d11 + s11*C >= 0
paper += d12 + s12*C >= 0
paper += d21 + s21*C >= 0
paper += d22 + s22*C >= 0

paper += d11 - (1-s11)*C < 0
paper += d12 - (1-s12)*C < 0
paper += d21 - (1-s21)*C < 0
paper += d22 - (1-s22)*C < 0


# solve, check status and print output
def solve(prob):
    prob.writeLP(prob.name + ".lp")
    prob.solve()
    print "status: " + LpStatus[prob.status]
    for v in prob.variables():
        print (v.name, "= ", v.varValue)


############################################################
# Automatically set up problem where E = {}
############################################################



############################################################
# Score
############################################################





    
