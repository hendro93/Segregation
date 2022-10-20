#data ethnic-religion_4 (ea4) use randomSearch
#ea1 and ea2 use StandardGA rel 24 T
#ea3 problem
#ea5 use MutationHillClimber and is still favor our standpoint rel 12 T
#ea6 use StandardGA beta_rel 0 T
#ea7 use MutationHillClimber rel 12
#ea8 standardGA = 4
#ea9 MutationHillClimber = 16 T
#ea10 MutationHillClimber = 16 T
#ea11 MutationHillClimber = 16 T
#ea12 MutationHillClimber = 16 T
#eaA MutationHillClimber = 24F
#ea13 MutationHillClimber = 

bsData <- read.table(file = "ethnic-religion_5.bestHistory.csv", header = T
                     , sep = "," )


bsData
