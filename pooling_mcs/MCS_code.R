#################################################################################
###Pooling optimization MCS################################

##This MCS establishes the performance of different pooling optimization methods
#on historical surveillance data. Historical data for a sample submission is 
#converted to binary (1 = positive sample, 0 = negative sample) and pooling 
#optimization methods are simulated on 1,000 randomization of each submission to
#determine how total tests count differs between pooling methods (which method 
#achieves greater savings). The output of this simulation is used as the input for
#the processing time DES.

##Needed packages
library(readxl)
library(binGroup2)
library(dplyr)
library(tidyverse)

##load input data file
#an example data set was generated to demonstrate a sampling ofdata input structure. 
#Full data is available from authors upon reasonable request
Covid_sim_input.file <-  "./data/MCS_input_data.csv"
Covid_sim_input <- read.csv(Covid_sim_input.file)

##compile all sample sources into vector to group simulation
pool_sources <- unique(Covid_sim_input$sample_source)

##Load prevalence estimation reference data file for PET pooling method
prevest.file <- "C:/Users/catie/Desktop/COVID_MCS/PET_reference.xlsx"
prevdata <- read_excel(prevest.file, sheet = 1)
##in this data set 0 pools being positive and all pools being positive are set to 0% anf 100% prevalence respectively 
##to ensure the appropriate pool size is assigne for a high/low prevalence- not to indicatethat 0% or 100% prevalence
##is the estimated value.


####Function to convert historical data into binary dataframe of positive/negative results for pooling simulation
MCS_DF <- function(hyp_data) {
  
  #print(case_id_data)
  pos <- as.numeric(hyp_data$pos_count)
  neg <- as.numeric(hyp_data$neg_count)
  
  
  #set up pos/neg vectors
  P <- c(1)
  N <- c(0)
  
  # make Pos data frame
  if(pos > 0){
    P_num <- as.data.frame(matrix(rep(P, times = pos), dimnames= list(NULL, "result")))
  } else {
    P_num <- NULL
  }
  # make Neg data frame
  if(neg > 0){
    N_num <- as.data.frame(matrix(rep(N, times = neg), dimnames= list(NULL, "result")))
  } else {
    N_num <- NULL
  }
  #paste +/- DF together
  results.df <- rbind(P_num, N_num)
  return(results.df$result)
  #print(results.df)
  
}

# function to randomize the order of binary results for repeated pooling iterations (x1000)
randomize_rows <- function(df) {
  rand_rows <- sample(length(df)) 
  return(df[rand_rows])
}

#########################################################################################################################
#####Functions for pooling optimization methods

#####tOPS function
Covid_tOPS <- function(results, tP, sample_count) {
  results <- as.vector(results)
  samples <- sample_count
  prev <- tP
  
  ####condition to make sure pooling is not applied when prevalence is too high-
  ##pool size set to 1 to mean pooling will not be used, only individual testing
  ##if prevalence is low enough the optimal pool size will be calculated from the 
  ##true prevalence of the submission
  if (prev > 0.33) {
    
    ops <- 1
    
    total_pools_GT <- 0
    PET_count <- 0
    PRR <- 0
    Indv <- samples
    total_pooling_tests <- Indv
    aops <- 1
    
  } else {
    # calculate optimal pool size (OPS) (between 3 and 20 samples) based 
    #on prevalence 
    otc_result <- binGroup2::OTC1(algorithm = 'D2', 
                                  Se = c(0.95), 
                                  Sp = 1, 
                                  group.sz = c(3:20), 
                                  p = prev,
                                  trace = FALSE, 
                                  print.time = FALSE)
    
    ops <- otc_result$Configs[1, 1]
    
    
    
    
    #set up condition if the number of remaining samples is less than the ops 
    ##(the applied ops)
    if (ops > length(results)) {
      app_pool_size <- length(results)
    } else {
      app_pool_size <- ops
    }
    aops <- app_pool_size
    
    
    
    # determine number of pools and remainders
    pool_count <- floor(length(results) / app_pool_size)
    remainder <- length(results) %% app_pool_size
    rem_count <- ifelse(remainder == 0, 0, 1)
    
    # define tracker for number of positive pools and if a remainder pool 
    #(pool with fewer samples than aops) occurs and is positive
    pos_pools <- 0
    pos_remainder <- 0
    
    #####break up sample results in binary data set into pools
    for (i in seq(1, pool_count)) {
      
      # define pool ranges
      # subset individual pools
      pool_end <- i * app_pool_size
      pool_start <- pool_end - app_pool_size + 1
      pool <- results[pool_start : pool_end]
      
      
      # search pool for positive, add to positive pool tally if positive is found
      if (1 %in% pool) {
        pos_pools <- pos_pools + 1
        
      }
      
    }
    
    #search remainder pool for positive
    if (rem_count != 0){
      rem_start <- pool_count * app_pool_size + 1
      rem_end <- rem_start + (remainder - 1)
      rem_pool <- results[rem_start:rem_end]
      
      #search rem for positive
      if (1 %in% rem_pool) {
        pos_remainder <- pos_remainder + 1
        
      }
    }
    
    # calculate total test count
    #GT = Group testing (pooling over the whole submission, versus a subset of the
    #submission in PET method)
    #PRR = positive pool reruns (samples tested individually from positive pools)
    #Indv= a sample run as an individual, never pooled
    ##PET count = number of tests to complete the prevalence estimation phase of the 
    #PET method of testing- this value will always be 0 for non-PET methods
    #set up condition for when only one sample remains after pooling a submission (and cannot be pooled)
    if ( remainder == 1) {
      total_pools_GT <- pool_count
      PRR <- app_pool_size * pos_pools
      Indv <- 1
      PET_count <- 0
    } else {
      total_pools_GT <- pool_count + rem_count
      Indv <- 0
      PRR <- app_pool_size * pos_pools + pos_remainder * remainder
      PET_count <- 0
      #total_pooling_tests <- app_pool_size * pos_pools + pool_count + rem_count + pos_remainder * remainder
    }
    
    total_pooling_tests <- total_pools_GT + PRR + Indv + PET_count
  }  
  
  ###return multiple values in dataframe that can be bound to results dataframes of other methods
  return(data.frame(total_pooling_tests, total_pools_GT, PET_count, PRR, Indv, aops))
  
}

#######fuction for Individual testing- no pooling applied
Covid_indv <- function(results) {
  ###individual testing only
  results <- as.vector(results)
  #test count = number of samples submitted
  total_pooling_tests <- length(results)
  #other values set to 0 and aops set to 1 to allow results to be bound to other methods
  total_pools_GT <- 0
  PET_count <- 0
  PRR <- 0
  Indv <- length(results)
  aops <- 1
  ###return multiple values in dataframe that can be bound to results dataframes of other methods
  return(data.frame(total_pooling_tests, total_pools_GT, PET_count, PRR, Indv, aops))
}

#####Historical prevalence pooling method
Covid_HP <- function(results, HP, sample_count) {
  results <- as.vector(results)
  samples <- sample_count
  ##Historical prevalence (HP) previously calculated as the average of each source's previous submission
  hist_prev <- HP
  
  ####condition to make sure pooling is not applied when HP is too high-
  ##pool size set to 1 to mean pooling will not be used, only individual testing
  ##if HP is low enough the optimal pool size will be calculated from the 
  ##HP of the submission
  
  if (hist_prev > 0.33) {
    
    ops <- 1
    total_pools_GT <- 0
    PET_count <- 0
    PRR <- 0
    Indv <- samples
    total_pooling_tests <- Indv
    aops <- 1
    
  } else {
    # calculate optimal pool size (OPS) (between 3 and 20 samples) based on HP
    otc_result <- binGroup2::OTC1(algorithm = 'D2', 
                                  Se = c(0.95), 
                                  Sp = 1, 
                                  group.sz = c(3:20), 
                                  p = hist_prev,
                                  trace = FALSE, 
                                  print.time = FALSE)
    
    ops <- otc_result$Configs[1, 1]
    
    
    
    
    
    #set up condition if number of remaining samples < pool size
    if (ops > length(results)) {
      app_pool_size <- length(results)
    } else {
      app_pool_size <- ops
    }
    
    aops <- app_pool_size
    
    # determine number of pools and remainders
    pool_count <- floor(length(results) / app_pool_size)
    remainder <- length(results) %% app_pool_size
    rem_count <- ifelse(remainder == 0, 0, 1)
    
    # define tracker for number of positive pools and if a remainder pool 
    #(pool with fewer samples than aops) occurs and is positive
    pos_pools <- 0
    pos_remainder <- 0
    
    
    #####break up sample results in binary data set into pools
    for (i in seq(1, pool_count)) {
      
      # define pool ranges
      # subset individual pools
      pool_end <- i * app_pool_size
      pool_start <- pool_end - app_pool_size + 1
      pool <- results[pool_start : pool_end]
      
      
      # search pool for positive
      if (1 %in% pool) {
        pos_pools <- pos_pools + 1
        
      }
      
    }
    
    #establish remainder pool (final pool < aops)
    if (rem_count != 0){
      rem_start <- pool_count * app_pool_size + 1
      rem_end <- rem_start + (remainder - 1)
      rem_pool <- results[rem_start:rem_end]
      
      #search remainder pool for positive
      if (1 %in% rem_pool) {
        pos_remainder <- pos_remainder + 1
        
      }
    }
    
    
    # calculate total test count
    #GT = Group testing (pooling over the whole submission, versus a subset of the
    #submission in PET method)
    #PRR = positive pool reruns (samples tested individually from positive pools)
    #Indv= a sample run as an individual, never pooled
    ##PET count = number of tests to complete the prevalence estimation phase of the 
    #PET method of testing- this value will always be 0 for non-PET methods
    #set up condition for when only one sample remains after pooling a submission (and cannot be pooled)
    if ( remainder == 1) {
      total_pools_GT <- pool_count
      PRR <- app_pool_size * pos_pools
      Indv <- 1
      PET_count <- 0
    } else {
      total_pools_GT <- pool_count + rem_count
      Indv <- 0
      PRR <- app_pool_size * pos_pools + pos_remainder * remainder
      PET_count <- 0
      #total_pooling_tests <- app_pool_size * pos_pools + pool_count + rem_count + pos_remainder * remainder
    }
    
    total_pooling_tests <- total_pools_GT + PRR + Indv + PET_count
  }  
  
  ###return multiple values in dataframe that can be bound to results dataframes of other methods
  return(data.frame(total_pooling_tests, total_pools_GT, PET_count, PRR, Indv, aops))
}


##Function for fixed pooling 
Fixed_covid <- function(results, pool_size, sample_count) {
  results <- as.vector(results)
  samples <- as.numeric(sample_count)
  ##make pool size changeable depending on if fixed conservative (4 samples) or 
  #maximal (20 samples) is used
  pool_size <- as.numeric(pool_size)
  #set up condition if number of remaining samples is < pool size
  if (pool_size > samples) {
    app_pool_size <- samples
  } else {
    app_pool_size <- pool_size
  }
  
  aops <- app_pool_size
  
  # determine number of pools and remainders
  pool_count <- floor(samples / app_pool_size)
  remainder <- samples %% app_pool_size
  rem_count <- ifelse(remainder == 0, 0, 1)
  
  # define tracker for number of positive pools and if a remainder pool 
  #(pool with fewer samples than aops) occurs and is positive
  pos_pools <- 0
  pos_remainder <- 0
  
  for (i in seq(1, pool_count)) {
    
    # define pool ranges
    # subset individual pools
    pool_end <- i * app_pool_size
    pool_start <- pool_end - app_pool_size + 1
    pool <- results[pool_start : pool_end]
    
    
    # search pool for positive samples
    if (1 %in% pool) {
      pos_pools <- pos_pools + 1
      
    }
    
  }
  
  #eastablish remainder pool
  if (rem_count != 0){
    rem_start <- pool_count * app_pool_size + 1
    rem_end <- rem_start + (remainder - 1)
    rem_pool <- results[rem_start:rem_end]
    
    #search remainder pool for positive
    if (1 %in% rem_pool) {
      pos_remainder <- pos_remainder + 1
      
    }
  }
  
  
  # calculate total test count
  #GT = Group testing (pooling over the whole submission, versus a subset of the
  #submission in PET method)
  #PRR = positive pool reruns (samples tested individually from positive pools)
  #Indv= a sample run as an individual, never pooled
  ##PET count = number of tests to complete the prevalence estimation phase of the 
  #PET method of testing- this value will always be 0 for non-PET methods
  #set up condition for when only one sample remains after pooling a submission (and cannot be pooled)
  if ( remainder == 1) {
    total_pools_GT <- pool_count
    PRR <- app_pool_size * pos_pools
    Indv <- 1
    PET_count <- 0
  } else {
    total_pools_GT <- pool_count + rem_count
    Indv <- 0
    PRR <- app_pool_size * pos_pools + pos_remainder * remainder
    PET_count <- 0
  }
  
  total_pooling_tests <- total_pools_GT + PRR + Indv + PET_count
  ###return multiple values in dataframe that can be bound to results dataframes of other methods
  return(data.frame(total_pooling_tests, total_pools_GT, PET_count, PRR, Indv, aops))
}  


######Function for PET pooling optimization method
Covid_PET <- function(results, prev_df = prevdata, sample_count) {
  samples <- sample_count
  results <- as.vector(results)
  
  ##establish subset of submission to use for prevalence estimation
  # 25% rounded up to nearest factor of 3 
  PET_size <- ceiling(length(results) * 0.25 / 3) * 3
  sample_index <- seq(1, PET_size)
  ##pull out samples from binary results dataframe to be used for prevalence
  #estimation phase
  PET_samples <- results[sample_index]
  ##set up tracker for positive pools from prevalence estimation phas
  PET_pos_pools <- 0
  ##all pools in prevalence estiation phase have 3 samples in them
  for (i in seq(1, PET_size, 3)) {
    pool_start <- i
    pool_end <- pool_start + 2
    
    pool <- PET_samples[pool_start : pool_end]
    
    if (1 %in% pool) {
      PET_pos_pools <- PET_pos_pools + 1
    }
  }
  # calculate estimated prevalence with estimated prevalence reference data sheet
  #caculated from Ausvet online tool https://epitools.ausvet.com.au/ppfreqone
  #number of pools run and number of positive pools in prevalence estimation phase
  #used to parse data sheet to find estimated prevalence value
  est_prev <- prevdata %>% 
    filter(numpools == PET_size / 3, numpos == PET_pos_pools) %>% 
    select(estprev)
  est_prev <- as.numeric(est_prev)
  
  # calculate estimated OPS
  #set up condition for individual testing to be applied when estimated prevalence
  #is too high to pool
  if (est_prev > 0.33) {
    
    ops <- 1
    total_pools_GT <- 0
    PET_count <- PET_size/3
    PRR <- PET_pos_pools*3
    Indv <- samples - PET_size
    total_pooling_tests <- Indv +PRR + PET_count
    aops <- 1
    reruns <- PET_pos_pools * 3
    
    
  } else {
    # calculate estimated optimal pool size (OPS) (between 3 and 20 samples) based 
    #estimated prevalence
    otc_result <- binGroup2::OTC1(algorithm = 'D2', 
                                  Se = c(0.95), 
                                  Sp = 1, 
                                  group.sz = c(3:20), 
                                  p = est_prev,
                                  trace = FALSE, 
                                  print.time = FALSE)
    
    ops <- otc_result$Configs[1, 1]
    
    
    
    
    # apply OPS to remainder of results (GT = group testing, remainder of submission)
    gt <- results[-sample_index]
   
    #set up condition if OPS is greater than number of remaining samples
    if (ops > length(gt)) {
      aops <- length(gt)
    } else {
      aops <- ops
    }
    
    
    #continue pooling with applied OPS (aops)
    gt_pool_count <- floor(length(gt) / aops)
    gt_remainders <- length(gt) %% aops
    gt_rem_count <- ifelse(gt_remainders == 0, 0, 1)
    ##set up tracker for positive pool counts from group testing phase
    GT_pos_pools <- 0
    GT_rem_pos_pools <- 0
    for (i in seq(1, gt_pool_count)) {
      
      # define pool ranges
      # subset results by pools
      pool_end <- i * aops
      pool_start <- pool_end - (aops - 1)
      
      pool <- gt[pool_start : pool_end]
      
      # search pool for positive samples
      if (1 %in% pool) {
        GT_pos_pools <- GT_pos_pools + 1
      }
    }
    
    #establish remainder pool
    if (gt_remainders > 1){
      rem_start <- gt_pool_count * aops + 1
      rem_end <- rem_start + (gt_remainders - 1)
      rem_pool <- results[rem_start:rem_end]
      
      #search remainder pool for positive
      if (1 %in% rem_pool) {
        GT_rem_pos_pools <- GT_rem_pos_pools + 1
        
      }
    }
    if(gt_remainders == 1){
      single_remainder <- 1
    } else {
      single_remainder <- 0
    }
    
    ##calculate the number of individual rerun tests from positive pools in
    #the group testing phase
    GT_rr <- GT_pos_pools * aops
    ##calculate the number of samples run as individuals
    if (est_prev < 0.33) {
      Indv <- 0 + single_remainder
      
    } 
    
    ##calculate the number of individual rerun tests from positive pools in both
    #the GT and prevalence estimation phases
    if(est_prev < .33) {
      reruns <- PET_pos_pools * 3 + GT_rr + GT_rem_pos_pools * gt_remainders
    }
    
    # calculate total test count
    #GT = Group testing (pooling over the whole submission, versus a subset of the
    #submission in PET method)
    #PRR = positive pool reruns (samples tested individually from positive pools)
    #Indv= a sample run as an individual, never pooled
   
    PRR <- reruns
    PET_count <- PET_size / 3
    total_pools_GT <- gt_pool_count
    total_pooling_tests <- PET_count + total_pools_GT + PRR + Indv
    
  }
  ###return multiple values in dataframe that can be bound to results dataframes of other methods
  return(data.frame(total_pooling_tests, total_pools_GT, PET_count, PRR, Indv, aops))
  
}


####################################################################################
######### Monte Carlo Simulation ###
################################################

##Make unique identifyer for each submission 
Covid_sim_input$case_ID <- paste(Covid_sim_input$sample_source, "_", Covid_sim_input$date)
##make vector of submissions to parse through in simulation
cases <- Covid_sim_input$case_ID
##set up results repository
sim_res <- NULL
##set seed for replicability
set.seed(19)

# loop over each submission case id 
for (c in cases) {
  
  # run eqch results sequence 1000 times per case, result order randomized each time
  for (i in seq(1, 1000)) {
    #subset out 1 submission
    case_sub <- subset(Covid_sim_input, Covid_sim_input$case_ID == c)
    #randomize order of results
    rand_data <- randomize_rows(MCS_DF(case_sub))
    #pull out HP value for submission
    HP <- case_sub$Hist_prev
    #pull out true prevalence for submission
    tP <- case_sub$prev
    ##pull out total samples in submission
    samp <- case_sub$total_samp
    
    ###run submission results through each pooling method
    HP_res <- Covid_HP(rand_data, HP, sample_count =samp)
    PET_res <- Covid_PET(rand_data, sample_count = samp)
    Fixed_20_res <- Fixed_covid(rand_data, pool_size =20, sample_count =samp)
    Fixed_4_res <- Fixed_covid(rand_data, pool_size =4, sample_count =samp)
    tOPS_res <- Covid_tOPS(rand_data, tP, sample_count= samp)
    indv_res <- Covid_indv(rand_data)
    
    HP_res$method <- c("HPO")
    PET_res$method <- c("PET")
    Fixed_20_res$method <- c("Fixed maximal")
    Fixed_4_res$method <- c("Fixed conservative")
    tOPS_res$method <- c("tOPS")
    indv_res$method <- c("Individual")
    
    
    All_res <- data.frame(c, rbind(HP_res, PET_res, 
                                   Fixed_20_res, Fixed_4_res, tOPS_res, indv_res))
    
    sim_res <- rbind(sim_res, All_res)
    
    
    
  }
  print(paste('Completed case', c))
}

#####################################################################################
###calculate descriptive statistics for the 1,000 replications of each submission

Covid_sim_results <- sim_res %>%  
  group_by(c, method) %>%
  summarise(
    mean_total_pooling_tests = mean(total_pooling_tests),
    median_total_pooling_tests = median(total_pooling_tests),
    sd_total_pooling_tests = sd(total_pooling_tests),
    mean_GT_pools = mean(total_pools_GT),
    median_GT_pools = median(total_pools_GT),
    sd_GT_pools = sd(total_pools_GT),
    mean_PRR = mean(PRR),
    median_PRR = median(PRR),
    sd_PRR = sd(PRR),
    mean_PET_pools = mean(PET_count),
    median_PET_pools = median(PET_count),
    sd_PET_pools = sd(PET_count),
    mean_Indv = mean(Indv),
    median_Indv = median(Indv),
    sd_Indv = sd(Indv),
    mean_aops = mean(aops),
    median_aops = median(aops),
    sd_aops = sd(aops),
    
  )


#resotre date column
Covid_sim_results$c <- gsub(".*_","",Covid_sim_results$c)
colnames(Covid_sim_results)[1] <- "date"
#pull out median values to combine for DES input
Median_MCS_totals <- Covid_sim_results[,c(1,2,13,7,10,16)]

##combine median test values for each testing day for DES processing time input
Median_MCS_totals$date <- as.character(Median_MCS_totals$date)
daily_totals_comb_median <- Median_MCS_totals  %>%  
  group_by(date, method) %>%
  summarise(
    PET_pools_total = sum(median_PET_pools),
    GT_pools_total = sum(median_GT_pools),
    PRR_total = sum(median_PRR),
    Indv_total = sum(median_Indv),
    
  )

##define column names as sample classes for DES
colnames(daily_totals_comb_median) <- c("date", "method", "class_1", "class_2", "class_3", "class_4")

##save full data for processing time DES input
write.csv(daily_totals_comb_median, "../python_des/data/daily_sample_input_median.csv")
