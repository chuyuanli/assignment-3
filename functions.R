### Assignment 3 LI Chuyuan
### Question 2 & 3

# function to simulate normal distributed data
sample_df <- function(diff_mean, nb_g, nb_b, sd_g, sd_b){
  m_b <- sample(0:100, 1, replace=TRUE) # mean of bad by sampling
  m_g <- m_b + diff_mean
  group_b <- as.integer(rnorm(nb_b, mean = m_b, sd = sd_b)) #generate random bad group points, following normal distribution
  group_g <- as.integer(rnorm(nb_g, mean = m_g, sd = sd_g))
  
  new_df <- tibble::tibble(language = c(rep("adger-good", nb_g), rep("adger-bad", nb_b)),
                           rating = c(group_g, group_b))
  return (new_df)  
}


# function to generate N_SAMPLE times of data sets, return a data frame with one colomn = coef value
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


# function to simulate normal distributed data and then hew the value outside (0,100)
sample_df_cut <- function(diff_mean, nb_g, nb_b, sd_g, sd_b){
  m_b <- sample(0:100, 1, replace=TRUE)
  m_g <- m_b + diff_mean
  
  new_df_cut <- tibble::tibble(language=c(NA, NA))
  # make sure the final df has at least one value for "adger-good" and one for "adger-bad"
  while(length(unique(new_df_cut$language)) < 2){
    group_b <- as.integer(rnorm(nb_b, mean = m_b, sd = sd_b)) 
    group_g <- as.integer(rnorm(nb_g, mean = m_g, sd = sd_g))

    new_df <- tibble::tibble(language = c(rep("adger-good", nb_g), rep("adger-bad", nb_b)),
                             rating = c(group_g, group_b))
  
    # cut off the values which are less than 0 or bigger than 100
    new_df_cut <- dplyr::filter(new_df, rating>=0 & rating <= 100)
  }
  return (new_df_cut)
}



