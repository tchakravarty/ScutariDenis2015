#===============================================================================
# purpose: examples and exercises in Chapter 1 of Scutari and Denis (2015)
# author: tirthankar chakravarty
# comments:
# TODO:
#===============================================================================

rm(list = ls())

#==========================================================
# Travel mode choice survey
#==========================================================
# read in the data
dfSurvey = read.table("Data/Chapter1//survey.txt", sep = " ", header = TRUE,
                      stringsAsFactors = TRUE)

# set up an empty graph
bnSurvey = empty.graph(names(dfSurvey), num = 1) 

#================================================
# set up the graphical structure of the Bayesian network
#================================================
# add the direct dependencies to the graph (am = adjacency matrix;
#   dag = directed acyclic graph)
dagSurvey = matrix(c("A", "E",
                    "S", "E", 
                    "E", "O",
                    "E", "R",
                    "O", "T",
                    "R", "T"),
                  byrow = TRUE, ncol = 2, dimnames = list(NULL, c("from", "to")))
arcs(bnSurvey) = dagSurvey

# NOTE: the same conditional independencies as encoded by the directed arcs
#   can be encoded using the local distributional structure
bnSurvey2 = model2network("[A][S][E|A:S][O|E][R|E][T|O:R]")
all.equal(bnSurvey, bnSurvey2)

# plot the graph
plot(bnSurvey)

#================================================
# set up the probabilistic structure of the BN: expert system (known CPTs) 
#================================================
# levels of the variables
A.lev = levels(dfSurvey$A)
E.lev = levels(dfSurvey$E)
R.lev = levels(dfSurvey$R)
S.lev = levels(dfSurvey$S)
T.lev = levels(dfSurvey$T)
O.lev = levels(dfSurvey$O)

# create arrays of the conditional probability tables of each of the nodes
cptA = array(c(0.3, 0.5, 0.2), dim = 3, dimnames = list(A = A.lev))
cptE = array(c(0.75, 0.25,
               0.72, 0.28,
               0.88, 0.12,
               0.64, 0.36,
               0.70, 0.30,
               0.90, 0.10), 
             dim = c(2, 3, 2), 
             dimnames = list(E = E.lev, A = rev(A.lev), S = rev(S.lev)))
cptR = array(c(0.25, 0.75,
               0.2, 0.8),
             dim = c(2, 2),
             dimnames = list(R = rev(R.lev), E = E.lev))
cptS = array(c(0.6, 0.4), dim = 2, dimnames = list(S = S.lev))
cptT = array(c(0.48, 0.42, 0.10,
               0.56, 0.36, 0.08,
               0.58, 0.24, 0.18,
               0.70, 0.21, 0.09), 
             dim = c(3, 2, 2), 
             dimnames = list(T = T.lev, O = O.lev, R = rev(R.lev)))
cptO = array(c(0.96, 0.04,
               0.92, 0.08), 
             dim = c(2, 2),
             dimnames = list(O = O.lev, E = E.lev))

cptSurvey = list(A = cptA, E = cptE, R = cptR, S = cptS, T = cptT, O = cptO)

#==========================================================
# put the graphical and probabilistic structure together
#==========================================================
bnSurvey =  custom.fit(bnSurvey, cptSurvey)
