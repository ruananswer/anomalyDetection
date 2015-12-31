#dependendy
#install.packages("devtools")
#devtools::install_github("hafen/stlplus")
#devtools::install_github("hafen/stl2")
library(stl2)
#library(lubridate)
##################

source('detect.R')
source('date_utils.R')

# load data
load("data/raw_data.rda");
#raw_data <- read.csv("inst/extdata/data.csv")

#############
# start time
time1 <- Sys.time();
res <- AnomalyDetectionTs(raw_data, max_anoms=0.02, num_period_in_part = 3, sample_step = 10, anoms_threshold = 1.05, down_sample_step = 60, direction='both', plot=TRUE)
# end time
time2 <- Sys.time();
calTime <- time2 - time1;
print(calTime[1])
res$plot



#pointPerDay <- length(createDay())
# 
# #sucess
#bumpToEarly()    
#smallChangeOnStrictModel() 
#smallChange()  
#moreNoise() 
#plateau()  
#growSuddenly() 
#floor()
#speark()
#bumpInDoublePick()
#justGrow()
#linearGrow()
#stopSuddenly()
#tmp()
#removeNoise()
#flat()
#exponentialGrow()
#linearGrowWithError()
#justGrowWithError()
