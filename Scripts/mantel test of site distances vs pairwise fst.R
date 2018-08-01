library(tidyverse)
library(gdata)

distance <- read.csv("Outputs/site_distance_matrix.csv")

fst <- read.csv( "../../Lab_Work/Data_analysis/PopGenReport/Outputs/Reports/mkcomplete/PopGenReport-pairwise_Fst.csv", row.names = 1, header = T)

fst
str(fst)
as.matrix(fst)
upper

upperTriangle(fst) <- lowerTriangle(fst)


