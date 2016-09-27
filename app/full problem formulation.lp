\* full problem formulation *\
Minimize
OBJ: 0.008 ingredients_BEEF + 0.013 ingredients_CHICKEN
 + 0.001 ingredients_GEL + 0.01 ingredients_MUTTON + 0.002 ingredients_RICE
 + 0.005 ingredients_WHEAT
Subject To
FatRequirement: 0.1 ingredients_BEEF + 0.08 ingredients_CHICKEN
 + 0.11 ingredients_MUTTON + 0.01 ingredients_RICE + 0.01 ingredients_WHEAT
 >= 6
FibreRequirement: 0.005 ingredients_BEEF + 0.001 ingredients_CHICKEN
 + 0.003 ingredients_MUTTON + 0.1 ingredients_RICE + 0.15 ingredients_WHEAT
 <= 2
PercentagesSum: ingredients_BEEF + ingredients_CHICKEN + ingredients_GEL
 + ingredients_MUTTON + ingredients_RICE + ingredients_WHEAT = 100
ProteinRequirement: 0.2 ingredients_BEEF + 0.1 ingredients_CHICKEN
 + 0.15 ingredients_MUTTON + 0.04 ingredients_WHEAT >= 8
SaltRequirement: 0.005 ingredients_BEEF + 0.002 ingredients_CHICKEN
 + 0.007 ingredients_MUTTON + 0.002 ingredients_RICE + 0.008 ingredients_WHEAT
 <= 0.4
End
