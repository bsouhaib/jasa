source("nicefigs.R")

hour    <- rep(c("00", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", 
                 "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23"), each = 2)
minutes <- rep(c("00", "30"), 24)
tday <- paste(hour, minutes, sep=":")

abbr.dweek <- c("Mon","Tue","Wed","Thu","Fri", "Sat","Sun")

alphas_integer <- c(1, seq(5,95, by = 5), 99)
alphas <- rev(alphas_integer/100)

hours_night <- night_hours <-  c(seq(1, 12), 46, 47, 48)
hours_day   <- day_hours <- setdiff(seq(1, 48), hours_night)
hours_all <- c(hours_night, hours_day)
index_hours <- sort(hours_all, index = T)$ix

iday_withmodels <- c(1, seq(10, 90, 10))

#n_past_obs_kd    <- 90 *48
n_past_obs_kd    <- 60 *48
n_past_obs_tbats <- (31 + 28 + 31 + 30)*48

#M <- 1128
M <- 948
q_probs <- seq(M)/(M + 1)

#all_algorithms <- c("Uncond", "TBATS", "KD-IC-NML", "PeriodOfDay", "BU-NNLS-INDEP-TBATS",  "BU-NNLS-PERM-TBATS", "BU-INDEP-TBATS", "BU-PERM-TBATS")
#all_algorithms <- c(all_algorithms, "BU-NNLS-INDEP-KD", "BU-NNLS-PERM-KD", "BU-INDEP-KD", "BU-PERM-KD")
#all_algorithms <- c(all_algorithms, "BU-NNLS-PERM-TBATS-KD", "BU-NNLS-INDEP-TBATS-KD") 
#all_algorithms <- c(all_algorithms, "BU-WNNLS-PERM-TBATS")
#all_algorithms <- c(all_algorithms, "BU-WNNLSAAAI-PERM-TBATS")

#all_colors <- c("red", "brown", "green", "darkblue","cyan", "orange", "purple", "blue")
#all_colors <- c(all_colors, "cyan", "orange", "purple", "blue")
#all_colors <- c(all_colors, "darkgreen", "darkgreen")
#all_colors <- c(all_colors, "grey")
#all_colors <- c(all_colors, "orange")

