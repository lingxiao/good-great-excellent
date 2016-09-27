\* tutorial *\
Minimize
OBJ: 0.008 beef_percent + 0.013 chicken_percent
Subject To
FatRequirement: 0.1 beef_percent + 0.08 chicken_percent >= 6
FibreRequirement: 0.005 beef_percent + 0.001 chicken_percent <= 2
PercentagesSum: beef_percent + chicken_percent = 100
ProteinRequirement: 0.2 beef_percent + 0.1 chicken_percent >= 8
SaltRequirement: 0.005 beef_percent + 0.002 chicken_percent <= 0.4
Bounds
0 <= beef_percent
0 <= chicken_percent
Generals
beef_percent
chicken_percent
End
