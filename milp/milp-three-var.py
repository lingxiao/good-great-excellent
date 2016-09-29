############################################################
# Module  : simple version of adjective ranking with three words
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

# sum of occurences of p_ws(i,j)
W_good_great   = 84862.0/P1
W_good_superb  = 283.9  /P1
W_great_good   = 1829.0 /P1
W_great_superb = 0.0    /P1
W_superb_good  = 0.0    /P1
W_superb_great = 0.0    /P1


# sum of occurences of p_sw(i,j)
S_good_great   = 138.0/P2
S_good_superb  = 0.0  /P2
S_great_good   = 384.0/P2
S_great_superb = 0.0  /P2
S_superb_good  = 42.0 /P2
S_superb_great = 0.0  /P2


# cnt(good), cnt(great), cnt(superb)
ngood   = 89827779.0
ngreat  = 106080147.0
nsuperb = 2050800.0


# scores from corpus data
R11 = 0        # good-good
R22 = 0        # great-great
R33 = 0        # superb-superb

# raw iRj
R12r = ((W_good_great  - S_good_great  ) - (W_great_good   - S_great_good  ))/(ngood*ngreat  )
R13r = ((W_good_superb - S_good_superb ) - (W_superb_good  - S_superb_good ))/(ngood*nsuperb )
R21r = ((W_great_good  - S_great_good  ) - (W_good_great   - S_good_great  ))/(ngood*ngreat  )
R23r = ((W_great_superb- S_great_superb) - (W_superb_great - S_superb_great))/(ngreat*nsuperb)
R31r = ((W_superb_good - S_superb_good ) - (W_good_superb  - S_good_superb ))/(nsuperb*ngood )
R32r = ((W_superb_great - S_superb_great)- (W_great_superb - S_great_superb))/(nsuperb*ngreat)

# raw good-superb score

# raw great-superb score
norms = [x for x in [R12r,R13r,R21r,R23r,R31r,R32r] if x !=0]

norm = min(norms)

R12 = R12r/norm
R13 = R13r/norm
R21 = R21r/norm
R23 = R23r/norm
R31 = R31r/norm
R32 = R32r/norm

############################################################
# Manually set up problem where E = {(good,good),(great,great)}
############################################################

# Set up the Lp problem
paper  = LpProblem("order {good,great} where E = {}", LpMaximize)

# manually set up all possible relations among two words
w11 = LpVariable("w11"  , 0,1, LpInteger)    # good  < good
w12 = LpVariable("w12"  , 0,1, LpInteger)    # good  < great
w13 = LpVariable("w13"  , 0,1, LpInteger)    # good  < superb


w21 = LpVariable("w21"  , 0,1, LpInteger)    # great < good
w22 = LpVariable("w22"  , 0,1, LpInteger)    # great < great
w23 = LpVariable("w23"  , 0,1, LpInteger)    # great < superb

w31 = LpVariable("w31"  , 0,1, LpInteger)    # superb < good
w32 = LpVariable("w32"  , 0,1, LpInteger)    # superb < great
w33 = LpVariable("w33"  , 0,1, LpInteger)    # superb < superb


s11 = LpVariable("s11"  , 0,1, LpInteger)    # good   > good
s12 = LpVariable("s12"  , 0,1, LpInteger)    # good   > great
s13 = LpVariable("s13"  , 0,1, LpInteger)    # good   > superb


s21 = LpVariable("s21"  , 0,1, LpInteger)    # great  > good
s22 = LpVariable("s22"  , 0,1, LpInteger)    # great  > great
s23 = LpVariable("s23"  , 0,1, LpInteger)    # great  > superb

s31 = LpVariable("s31"  , 0,1, LpInteger)    # superb > good
s32 = LpVariable("s32"  , 0,1, LpInteger)    # superb > great
s33 = LpVariable("s33"  , 0,1, LpInteger)    # superb > superb


# continous variables
x1  = LpVariable("x1"  , 0, 1)              # val of good
x2  = LpVariable("x2"  , 0, 1)              # val of great
x3  = LpVariable("x3"  , 0, 1)              # val of superb

d11 = LpVariable("d11", 0, 1)               # x1 - x1
d12 = LpVariable("d12", 0, 1)               # x1 - x2
d13 = LpVariable("d13", 0, 1)               # x1 - x3
d21 = LpVariable("d21", 0, 1)               # x2 - x1
d22 = LpVariable("d22", 0, 1)               # x2 - x2
d23 = LpVariable("d23", 0, 1)               # x2 - x3
d31 = LpVariable("d31", 0, 1)               # x3 - x1
d32 = LpVariable("d32", 0, 1)               # x3 - x2
d33 = LpVariable("d33", 0, 1)               # x3 - x3


# # # # objective function # # #
order = (w12 - s12)*R12 + (w13 - s13)*R13 + (w21 - s21)*R21	+ (w23 - s23)*R23 + (w31 - s31)*R31	+ (w32 - s32)*R32


# synonym = (w11 + s11)*R11 + (w22 - s22)*R22  

# where you left off:
# the order is exactly reversed, does this come from poor data? or improper constraints, or bad C?
paper  += order

# # # # constraints # # #

# # distance constraints
# paper += d11 == x1 - x1
# paper += d12 == x1 - x2
# paper += d21 == x2 - x1
# paper += d22 == x2 - x2

# # 
# paper += d11 - w11*C <= 0
# paper += d12 - w12*C <= 0
# paper += d21 - w21*C <= 0
# paper += d22 - w22*C <= 0


# paper += d11 + (1-w11)*C > 0
# paper += d12 + (1-w12)*C > 0
# paper += d21 + (1-w21)*C > 0
# paper += d22 + (1-w22)*C > 0


# paper += d11 + s11*C >= 0
# paper += d12 + s12*C >= 0
# paper += d21 + s21*C >= 0
# paper += d22 + s22*C >= 0
# paper += d22 + s22*C >= 0

# paper += d11 - (1-s11)*C < 0
# paper += d12 - (1-s12)*C < 0
# paper += d21 - (1-s21)*C < 0
# paper += d22 - (1-s22)*C < 0


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





    
