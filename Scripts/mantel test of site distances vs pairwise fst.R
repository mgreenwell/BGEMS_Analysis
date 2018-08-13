# ============================== Overview =====================================


# Script takes two dataframes, converts them into distance matricies with the
# same number and same titled rows and columns and compares the two using a 
# mantel test.

# Mantel test result shows no significant correlation between the two matricies
 # I.e. the two matricies are unrelated.


# ======================== Packages Required ==================================


library(tidyverse)
library(gdata)
library(ade4)


# ============================ Read in Data ===================================


# site_distance_matrix.csv is a file containing the pairwise distances between 
# all BGEMS sites i.e. the distances between each site and every other site.

distance <- read.csv("Outputs/site_distance_matrix.csv", 
                     row.names = 1, header = T)

# PopGenReport-pairwise_Fst.csv is a matrix of pairwise Fst values between 
# all sites, calculated using PopGenReport

Fst <- read.csv("Data/Labs/PopGenReport-pairwise_Fst.csv", 
                row.names = 1, header = T)


# ========================== Format Data ======================================


# Need to make complete matrix from only bottom half of matrix for Fst

upperTriangle(Fst) <- lowerTriangle(Fst)


# Need to match the order of rows and columns in both dataframes
# I.e. rows and columns need to be alphabetical

distance <- distance[order(rownames(distance)), 
                     order(colnames(distance))]


# Need to remove any rows or columns that don't appear in both dataframes

# Get names of columns in both

a <- names(Fst)
b <- names(distance)


# Create new list of all names shared by both dataframes
# removing those occuring only in one or the other

shared_names <- a[a %in% b]


# select only columns in distance in shared_names

distance <- distance %>% select(shared_names)


# select only rows in distance in shared_names

distance <- distance[c(shared_names), ] 


# select only columns in Fst in shared_names 

Fst <- Fst %>% select(shared_names)


# select only rows in Fst in shared names

Fst <- Fst[c(shared_names), ] 


# Convert both dataframes to as.distance matricies

distance <- as.dist(distance)
Fst <- as.dist(Fst)


# ======================= Mantel test and interpretation ======================


# Run mantel test

mantel.rtest(distance, 
             Fst, 
             nrepet = 9999)


# Mantel test output

# Monte-Carlo test
# Call: mantelnoneuclid(m1 = m1, m2 = m2, nrepet = nrepet)
# 
# Observation: -0.0609018 
# 
# Based on 9999 replicates
# Simulated p-value: 0.602 
# Alternative hypothesis: greater 
# 
# Std.Obs         Expectation     Variance 
# -0.3318897153   -0.0006773807  0.0329274196 


# H0 the two matracies are unrelated at alpha = 0.5

# Based on these results we can accept the null hypothesis that these two 
# matrices are unrelated with alpha = .05. 
# p = 0.602

# Note that since this test is based on random permutations, the same code will
# always arrive at the same observed correlation but rarely the same p-value.