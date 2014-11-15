#====================================================================
# purpose: examples and exercises in chapter 2 of Scutari and Denis (2015) 
# author: tirthankar chakravarty
# comments: The data used in this chapter is to decide the determinants of
#   crop yield.
# 1. Hungarian notation for Bayesian networks:
#   - Bayesian network = "bn" (class = "bn.fit")
#   - DAG = "dag" (class = "bn")
#   - Distributions = "dist" (class = "list")
# 2. Prefer to use the model2network function to specify the DAG of a BN
# 3. Note that in the case of structure learning, the part of the complexity
#   coefficient is played by the "alpha" parameter that thresholds the test
#   p-value, in case conditional independence tests are being used
#
# TODO:
# 1. In the case of the actual versus fitted for cross-section data, 
#   the actual versus predicted plot is probably not the best plot to show.
# 2. Try out experiments with lm, and penalized for fitting the local distri-
#   butions.
# 3. How to get the confidence intervals for the parameters of the local 
#     distribution models fitted using bn.fit?
# 4. Use the cluster option of the constraint-based structure learning algorithms
#   to see if speedups in the structure learning process are possible.
# 5. Gives examples of how the network scores are computed, but not how the
#   network scores can be used to find the best fitting network model.
# 6. Test that the conditional distribution returned by exact inference
#   is not the same as the local distribution. 
#   Answer: It is.
#====================================================================

rm(list = ls())

# load in the crop data
dfCrop1 = read.table(file = "Data/Chapter2/cropdata1.txt", header = TRUE)
dfCrop2 = read.table(file = "Data/Chapter2/cropdata2.txt", header = TRUE)

#==========================================================
# 2.2 Graphical Representation
#==========================================================
dagCrop = model2network("[V|G:E][N|V][W|V][C|N:W][G][E]") 

#==========================================================
# 2.3 Probabilistic Representation
#==========================================================
distCrop = list(
  E = list(coef = c("(Intercept)" = 50), sd = 10),
  G = list(coef = c("(Intercept)" = 50), sd = 10),
  V = list(coef = c("(Intercept)" = -10.35534, G = 0.5, E = 0.70711), sd = 5),
  N = list(coef = c("(Intercept)" = 45, V = 0.1), sd = 9.949874),
  W = list(coef = c("(Intercept)" = 15, V = 0.7), sd = 7.141428),
  C = list(coef = c("(Intercept)" = 0, N = 0.3, W = 0.7), sd = 6.25)
  )

# create the Bayesian network object
bnCrop = custom.fit(x = dagCrop, dist = distCrop)

#==========================================================
# 2.4 Estimating the Parameters: Correlation Coefficients
#==========================================================
bnCrop = bn.fit(x = dagCrop, data = dfCrop2)

# test the predictive accuracy of the network in-sample
predBNCropC = predict(bnCrop$C, data = dfCrop2)
accuracy(x = dfCrop2[, "C"], f = predBNCropC)

# plot the actual and predicted
dfPredBNCrop = melt(data.frame(id = seq_len(dim(dfCrop2)[1]), 
                          actual = dfCrop2[, "C"], predicted = predBNCropC),
                    id.vars = "id")
ggplot(data = dfPredBNCrop, aes(x = id, y = value, color = variable)) + 
  geom_line() +
  theme_bw() + xlab("ID") + ylab("Crop Yield (actual/fitted)") +
  scale_color_discrete(name = "Actual/Fitted")

#==========================================================
# 2.5 Learning DAG Structure: Tests and Scores
# - conditional independence tests
# - network scores: 
#==========================================================
dagCropIAMB = iamb(x = dfCrop2, test = "cor")
plot(dagCropIAMB)
stopifnot(all.equal(dagCrop, dagCropIAMB))

#==========================================================
# 2.6 Using Gaussian Networks
# - Exact Inference
# - Approximate Inference
#==========================================================
# conver the bn.fit object to the GBN, GEMA and MVN objects
gbnCrop = bnfit2nbn(bnCrop)
gemaCrop = nbn2gema(gbnCrop)
mvnCrop = gema2mn(gemaCrop)

# print each of the global distribution structures
print8nbn(gbnCrop)
print8gema(gemaCrop)
print8mn(mvnCrop)

# print the conditional distribution of nodes given exact values of some other nodes
condi4joint(mvnCrop, par = c("C"), pour = c("N", "W"), x2 = NULL)
bnCrop$C

#================================================
# Approximate inference
#================================================
# simulate from the Bayesian network
simCrop = rbn(bnCrop, n = 1000)
head(simCrop)

# simulate values from conditional distribution conditional on 
#   interval-valued evidence
# NOTE: this works by retaining draws from the networks that satisfy the
#   evidence condition
cpdCrop1 = cpdist(bnCrop, nodes = c("C", "W", "N"), evidence = (C > 80))
dim(cpdCrop1)

# in the case of point-valued evidence, use likelihood weighting simulation
cpdCrop2 = cpdist(bnCrop, nodes = c("V"), evidence = list(E = 90, G = 10),
                  method = "lw")
dim(cpdCrop2)

# in the case of point-valued events, use cpquery
cpqCrop1 = cpquery(bnCrop, event = (V > 70), evidence = list(E = 90, G = 10),
                   method = "lw")
