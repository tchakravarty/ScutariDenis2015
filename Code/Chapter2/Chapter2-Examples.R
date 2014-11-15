#====================================================================
# purpose: examples and exercises in chapter 2 of Scutari and Denis (2015) 
# author: tirthankar chakravarty
# comments: The data used in this chapter is to decide the determinants of
#   crop yield
# TODO:
#====================================================================

rm(list = ls())

# load in the crop data
dfCrop1 = read.table(file = "Data/Chapter2/cropdata1.txt", header = TRUE)
dfCrop2 = read.table(file = "Data/Chapter2/cropdata2.txt", header = TRUE)

#==========================================================
# 2.5 Learning DAG Structure: Tests and Scores
#==========================================================
dfCrop1

