############################################################
# Module  : simple version of adjective ranking with two words
# Date    : Sept 27th. 2016
############################################################

from pulp import *

############################################################
# Constants
############################################################

# Arbitrary constant
C  = 1.1e10

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

# raw good-great score
R12r = ((W_good_great - S_good_great) - (W_great_good - S_great_good))/(ngood*ngreat)
# raw great-good score
R21r = ((W_great_good - S_great_good) - (W_good_great - S_good_great))/(ngood*ngreat)

norm = min(R12r,R21r)

R12 = R12r/norm
R21 = R21r/norm

############################################################
# Manually set up problem where E = {(good,good),(great,great)}
############################################################

# Set up the Lp problem
paper  = LpProblem("order {good,great} where E = {}", LpMaximize)

# manually set up all possible relations among two words
w11 = LpVariable("w11"  , 0,1, LpInteger)    # good  < good
w12 = LpVariable("w12"  , 0,1, LpInteger)    # good  < great
w21 = LpVariable("w21"  , 0,1, LpInteger)    # great < good
w22 = LpVariable("w22"  , 0,1, LpInteger)    # great < great

s11 = LpVariable("s11" , 0,1, LpInteger)     # good > good
s12 = LpVariable("s12" , 0,1, LpInteger)     # good > great
s21 = LpVariable("s21" , 0,1, LpInteger)     # great > good
s22 = LpVariable("s22" , 0,1, LpInteger)     # great > great

# continous variables
x1  = LpVariable("x1"  , 0, 1)              # val of good
x2  = LpVariable("x2"  , 0, 1)              # val of great

d11 = LpVariable("d11", 0, 1)               # x1 - x1
d12 = LpVariable("d12", 0, 1)               # x1 - x2
d21 = LpVariable("d21", 0, 1)               # x2 - x1
d22 = LpVariable("d22", 0, 1)               # x2 - x2


# # # # objective function # # #
order   = (w12 - s12)*R12 + (w21 - s21)*R21
synonym = (w11 + s11)*R11 + (w22 - s22)*R22  

paper  += order

# # # # constraints # # #

# # distance constraints
paper += d11 == x1 - x1
paper += d12 == x1 - x2
paper += d21 == x2 - x1
paper += d22 == x2 - x2

# 
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
paper += d22 + s22*C >= 0

# paper += d11 - (1-s11)*C < 0
# paper += d12 - (1-s12)*C < 0
# paper += d21 - (1-s21)*C < 0
# paper += d22 - (1-s22)*C < 0


# solve, check status and print output
def solve(prob):
    # prob.writeLP(prob.name + ".lp")
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





    
