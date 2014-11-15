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
# plot(bnSurvey)

#================================================
# set up the probabilistic structure of the BN: expert system (known CPTs) 
# 1. Note that the levels must always be entered in the same way in the 
#   CPTs -- they cannot be reordered. 
#   This means that the CPT arrays need to be remade.
#================================================
# levels of the variables
A.lev = levels(dfSurvey$A)
E.lev = levels(dfSurvey$E)
R.lev = levels(dfSurvey$R)
S.lev = levels(dfSurvey$S)
T.lev = levels(dfSurvey$T)
O.lev = levels(dfSurvey$O)

# create arrays of the conditional probability tables of each of the nodes
cptA = array(c(0.5, 0.2, 0.3), dim = 3, dimnames = list(A = A.lev))
cptE = array(c(0.70, 0.30,
               0.90, 0.10,
               0.64, 0.36,
               
               
               0.75, 0.25,
               0.88, 0.12,
               0.72, 0.28), 
             dim = c(2, 3, 2), 
             dimnames = list(E = E.lev, A = A.lev, S = S.lev))

cptR = array(c(0.75, 0.25,
               0.8, 0.2),
             dim = c(2, 2),
             dimnames = list(R = R.lev, E = E.lev))
cptS = array(c(0.4, 0.6), dim = 2, dimnames = list(S = S.lev))

cptT = array(c(0.48, 0.10, 0.42, 
               0.56, 0.08, 0.36, 
               0.58, 0.18, 0.24,
               0.70, 0.09, 0.21), 
             dim = c(3, 2, 2), 
             dimnames = list(T = T.lev, O = O.lev, R = R.lev))

cptO = array(c(0.96, 0.04,
               0.92, 0.08), 
             dim = c(2, 2),
             dimnames = list(O = O.lev, E = E.lev))

cptSurvey = list(A = cptA, E = cptE, R = cptR, S = cptS, T = cptT, O = cptO)

#==========================================================
# put the graphical and probabilistic structure together
#==========================================================
bnfitSurvey =  custom.fit(bnSurvey, cptSurvey)

#================================================
# estimate the probabilistic structure of the BN: MLE 
#================================================
bnfitSurvey = bn.fit(bnSurvey, data = dfSurvey, method = "mle")

#================================================
# check that these are nothing but the parameters of the contingency tables
# "[A][S][E|A:S][O|E][R|E][T|O:R]"
# NOTE: margin = c(2, 3), means that compute the ratio by keeping those
#   indices constant. a_{ijk}/sum_i sum_j sum_k a_{ijk} versus
#   a_{ijk}/sum_i a_{ijk}
#================================================
with(dfSurvey, prop.table(table(A)))
bnfitSurvey$A

with(dfSurvey, prop.table(table(S)))
bnfitSurvey$S

with(dfSurvey, prop.table(table(E, A, S), margin = c(2, 3)))
bnfitSurvey$E

graphviz.plot(fitted)
