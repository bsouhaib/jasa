rm(list = ls())
source("config_paths.R")
source("config_general.R")
source("config_splitting.R")
source("jasa_utils.R")
source("utils.R")
library(igraph)

load(file.path(work.folder, "myinfo.Rdata"))

algo.agg <- "DYNREG"
algo.bottom  <- "KD-IC-NML"

ntest <- length(test$id)
n_bottom <- length(bottomSeries)

#nbperjob <- 276
nbperjob <- 69

njobs <- ntest/nbperjob

#agg_methods <- c("BASE", "NAIVEBU", "PERMBU", "NAIVEBU-MINT", "PERMBU-MINT")
#bot_methods <- c("BASE", "BASE-MINT")
#crps_agg    <- array(NA, c(n_agg, ntest, length(agg_methods)))
#crps_bottom <- array(NA, c(n_bottom, ntest, length(bot_methods)))

agg_methods <- c("BASE", "NAIVEBU", "PERMBU")
color.agg <- c("black", "orange", "darkblue")
bot_methods <- c("BASE")
color.bot <- c("black")
crps_agg    <- array(NA, c(n_agg, ntest, length(agg_methods)))
crps_bottom <- array(NA, c(n_bottom, ntest, length(bot_methods)))

total_qscores_agg <- total_qscores_bot <- 0

for(idjob in seq(njobs)){
  print(idjob)
  allidtest <- (idjob - 1) * nbperjob + seq(nbperjob) 
  
  res_job <- file.path(loss.folder, paste("results_HTS_", algo.agg, "_", algo.bottom, "_", idjob, ".Rdata", sep = "")) 
  load(res_job)
  
  list_crps_agg_nonull <- list_crps_agg[-which(sapply(list_crps_agg, is.null))]
  mat_crps_agg <- sapply(seq_along(list_crps_agg_nonull),  function(i){list_crps_agg_nonull[[i]]}, simplify = 'array')
  
  list_crps_bot_nonull <- list_crps_bot[-which(sapply(list_crps_bot, is.null))]
  mat_crps_bot <- sapply(seq_along(list_crps_bot_nonull),  function(i){list_crps_bot_nonull[[i]]}, simplify = 'array')
  
  #allmethods_samples_agg[, allidtest, ] <-  aperm(mat_res, c(1, 3, 2))
  
  crps_agg[, allidtest,]     <- aperm(mat_crps_agg, c(1, 3, 2))
  crps_bottom[, allidtest, ] <- aperm(mat_crps_bot, c(1, 3, 2))
  total_qscores_agg <- total_qscores_agg + avg_qscores_agg
  #total_qscores_bot <- total_qscores_bot + avg_qscores_bot

}

total_qscores_agg <- total_qscores_agg / njobs
#total_qscores_bot <- total_qscores_bot / njobs

stop("done")
# crps_agg   total_qscores_agg
# crps_bottom total_qscores_bot

crps_agg_byhour <- sapply(seq(n_agg), function(iagg){
  sapply(seq_along(agg_methods), function(imethod){
    apply(matrix(crps_agg[iagg, , imethod], ncol = 48, byrow = T), 2, mean)
  })
}, simplify = 'array')

savepdf(file.path(results.folder, paste("AGG-CRPS", sep = "") ))
for(iagg in seq(n_agg)){
  matplot(crps_agg_byhour[, , iagg], type = 'l', col = color.agg, lty = 1, main = sum(Sagg[iagg, ]))
  legend("topleft", agg_methods, col = color.agg, lty = 1)
}
dev.off()

crps_bot_byhour <- sapply(seq(n_bottom), function(ibot){
  sapply(seq_along(bot_methods), function(imethod){
    apply(matrix(crps_bottom[ibot, , imethod], ncol = 48, byrow = T), 2, mean)
  })
}, simplify = 'array')

savepdf(file.path(results.folder, paste("BOT-CRPS", sep = "") ))
for(ibot in seq(n_bottom)){
  print(ibot)
  matplot(crps_bot_byhour[, , ibot], type = 'l', col = color.bot, lty = 1)
  legend("topleft", agg_methods, col = color.agg, lty = 1)
}
dev.off()

# AVG AGG
avg_agg <- apply(crps_agg_byhour, c(1, 2), mean)
matplot(avg_agg, type = 'l', col = color.agg, lty = 1)

tt <- apply(total_qscores_agg, c(1, 2), mean)
matplot(tt, col = color.agg, type = "l")

# AVG BOTTOM
avg_bot <-apply(crps_bot_byhour, c(1, 2), mean)
matplot(avg_bot, type = 'l', col = color.bot)

tt <- apply(total_qscores_bot, c(1, 2), mean)
matplot(tt)

######
v <- sapply(seq(n_agg), function(iagg){
  sapply(seq_along(agg_methods), function(imethod){
    matrix(crps_agg[iagg, , imethod], ncol = 48, byrow = T)
  }, simplify = 'array')
}, simplify = 'array')

B <- 1000
savepdf(file.path(results.folder, paste("AGG-CRPS-withbands", sep = "") ))
for(iagg in seq(n_agg)){
  print(iagg)
  final_mat <- NULL
  for(i_method in seq_along(agg_methods)){
    if(i_method != 1){
      agg_method <- agg_methods[i_method]
      
      mat <- v[, , i_method, iagg]
      n <- nrow(mat)
      
      bmat <- matrix(NA, nrow = B, ncol = 48)
      for(b in seq(B)){
        bmat[b, ] <- apply(mat[sample(n, replace = T), ], 2, mean)
      }
      sd_method <- apply(bmat, 2, sd)
      mu_method <- crps_agg_byhour[, i_method, iagg]
      final_mat <- cbind(final_mat,  mu_method - 2 * sd_method, mu_method, mu_method + 2 * sd_method)
    }
  }
  matplot(crps_agg_byhour[, , iagg], type = 'l', col = color.agg, lty = 1)
  matplot(final_mat, type = 'l', col = rep(color.agg, each = 3), lty = 1, lwd = rep(c(.5, 1, .5), length(agg_methods) ))
}
dev.off()
######

savepdf(file.path(results.folder, paste("AGG-QSCORES", sep = "") ))
par(mfrow = c(2, 2))
for(iagg in seq(n_agg)){
  matplot(y = total_qscores_agg[, , iagg], x = seq(1, M)/M, lty = 1, type = 'l', cex = .5, 
          col = color.agg, ylab = "CRPS", xlab = "horizon")
  legend("topright", agg_methods, col = color.agg, lty = 1, cex = .5)
  #MAT <- cbind(sorted_samples_agg[, iagg,], obs_agg_idtest[iagg])
  #matplot(x = MAT, y = seq(1, M)/M, pch = 1, cex = .5, col = c("black", "red", "blue", "orange", "darkblue"))
}
dev.off()

savepdf(file.path(results.folder, paste("BOT-QSCORES", sep = "") ))
par(mfrow = c(2, 2))
for(i in seq(100)){
  print(i)
  matplot(y = total_qscores_bot[, , i], x = seq(1, M)/M, type = 'l', lty = 1, cex = .5, col = color.bot)
  #sorted_samples_bot <- apply(samples_bot, c(2, 3), sort)
  #MAT <- cbind(sorted_samples_bot[, i,], obs_bottom_idtest[i])
  #matplot(x = MAT, y = seq(1, M)/M, pch = 1, cex = .5, col = c("black", "magenta"))
}
dev.off()

