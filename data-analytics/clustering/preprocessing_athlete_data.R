library(tidyverse)
# data from https://www.kaggle.com/datasets/shashwatwork/injury-prediction-for-competitive-runners

training_timeseries <- read_csv("data/week_approach_maskedID_timeseries.csv")
athelete_data_means <- aggregate(.~`Athlete ID`,training_timeseries,mean)
# did they have an injury?
athelete_data_means$injury <- ifelse(athelete_data_means$injury==0,1,2)
#how many injuries
num_injuries <- aggregate(injury~`Athlete ID`,training_timeseries,sum)
athelete_data_means$num_injuries <- num_injuries$injury

colnames(athelete_data_means)

athelete_data_means.cluster_subset <- athelete_data_means[,c(2,3,4,5,13,14,15,16,17,21,22,23,73)]
colnames(athelete_data_means.cluster_subset)
