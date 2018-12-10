### Assignment 3 LI Chuyuan
### Question 2 & 3

# Function to simulate normal distributed data
## ARGUMENTS : 
##  - diff in means
##  - nb of good language and bad language
##  - standard deviation for good language and bad language
## RETURN:
##  - a data frame with 2 colomns: language and rating score
##
sample_df <- function(diff_mean, nb_g, nb_b, sd_g, sd_b){
  m_b <- sample(0:100, 1, replace=TRUE) # mean of bad by sampling
  m_g <- m_b + diff_mean
  group_b <- as.integer(rnorm(nb_b, mean = m_b, sd = sd_b)) #generate random bad group points, following normal distribution
  group_g <- as.integer(rnorm(nb_g, mean = m_g, sd = sd_g))
  
  new_df <- tibble::tibble(language = c(rep("adger-good", nb_g), rep("adger-bad", nb_b)),
                           rating = c(group_g, group_b))
  return (new_df)  
}


# Function to simulate normal distributed data and then hew the value outside (0,100)
## ARGUMENTS : 
##  - diff in means
##  - nb of good language and bad language
##  - standard deviation for good language and bad language
## RETURN:
##  - a data frame with 2 colomns: language and rating score
##
sample_df_c <- function(diff_mean, nb_g, nb_b, sd_g, sd_b){
  m_b <- sample(0:100, 1, replace=TRUE)
  m_g <- m_b + diff_mean
  
  group_b <- as.integer(rnorm(nb_b, mean = m_b, sd = sd_b))
  group_b <- replace(group_b, group_b < 0, 0)
  group_b <- replace(group_b, group_b > 100, 100)
  
  group_g <- as.integer(rnorm(nb_g, mean = m_g, sd = sd_g))
  group_g <- replace(group_g, group_g < 0, 0)
  group_g <- replace(group_g, group_g > 100, 100)
    
  new_df <- tibble::tibble(language = c(rep("adger-good", nb_g), rep("adger-bad", nb_b)),
                             rating = c(group_g, group_b))
  return (new_df)
}



# Function to generate N_SAMPLE times of data sets
## ARGUMENTS : 
##  - diff in means
##  - nb of good language and bad language
##  - standard deviation for good language and bad language
##  - statistic function sampling_f
##  - number of iteration N_SAMLPES
## RETURN:
##  - a data frame with one colomn = coef value
##
coef_from_sampling <- function(diff_mean, nb_g, nb_b, sd_g, sd_b, sampling_f, N_SAMPLES=99){
  v_coef_hypo <- rep(0, N_SAMPLES) # a vector to record coef for language
  for (i in 1:N_SAMPLES){
    dataset <- sampling_f(diff_mean, nb_g, nb_b, sd_g, sd_b)
    m_hypo <- lm(rating ~ language, data = dataset)
    coef_lang <- coef(m_hypo)[2]
    v_coef_hypo[i] <- coef_lang
  }
  return (tibble::tibble(coef = v_coef_hypo))
}

