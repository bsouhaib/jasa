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
#color.agg <- c("black", "orange", "darkblue")
#bot_methods <- c("BASE", "BASE-MINT")
#color.bot <- c("black")

agg_methods <- c("BASE", "NAIVEBU", "PERMBU", "PERMBU-MINT", "PERMBU-MEANCOMB")
color.agg <- c("grey", "orange", "cyan", "purple", "darkblue")
bot_methods <- c("BASE", "BASE-MINT", "BASE-MEANCOMB")
color.bot <- c("black", "purple", "darkblue")

crps_agg    <- array(NA, c(n_agg, ntest, length(agg_methods)))
crps_bottom <- array(NA, c(n_bottom, ntest, length(bot_methods)))

mse_agg    <- array(NA, c(n_agg, ntest, length(agg_methods)))
mse_bottom <- array(NA, c(n_bottom, ntest, length(bot_methods)))

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

  crps_agg[, allidtest,]     <- aperm(mat_crps_agg, c(1, 3, 2))
  crps_bottom[, allidtest, ] <- aperm(mat_crps_bot, c(1, 3, 2))
  total_qscores_agg <- total_qscores_agg + avg_qscores_agg
  #total_qscores_bot <- total_qscores_bot + avg_qscores_bot

  list_mse_agg_nonull <- list_mse_agg[-which(sapply(list_mse_agg, is.null))]
  mat_mse_agg <- sapply(seq_along(list_mse_agg_nonull),  function(i){list_mse_agg_nonull[[i]]}, simplify = 'array')
  list_mse_bot_nonull <- list_mse_bot[-which(sapply(list_mse_bot, is.null))]
  mat_mse_bot <- sapply(seq_along(list_mse_bot_nonull),  function(i){list_mse_bot_nonull[[i]]}, simplify = 'array')
  
  mse_agg[, allidtest,]     <- aperm(mat_mse_agg, c(1, 3, 2))
  mse_bottom[, allidtest,]     <- aperm(mat_mse_bot, c(1, 3, 2))
}

total_qscores_agg <- total_qscores_agg / njobs
#total_qscores_bot <- total_qscores_bot / njobs

print("CHECK MSE AS WELL !!!!!")

stop("done")
# crps_agg   total_qscores_agg
# crps_bottom total_qscores_bot

# AGG MSE
mse_agg_byhour <- sapply(seq(n_agg), function(iagg){
  sapply(seq_along(agg_methods), function(imethod){
    apply(matrix(mse_agg[iagg, , imethod], ncol = 48, byrow = T), 2, mean)
  })
}, simplify = 'array')

comment <- ""
savepdf(file.path(results.folder, paste("AGG-MSE-", comment, sep = "") ), height = 27 * 0.3)
par(mfrow = c(1, 2))
set_methods <- vector("list", 2)
set_methods[[1]] <-  c(2, 3, 5)
set_methods[[2]] <-  c(1, 4, 5)
for(iagg in seq(n_agg)){
  for(i in seq_along(set_methods)){
    agg_imethods <- set_methods[[i]]
    matplot(mse_agg_byhour[, agg_imethods, iagg], type = 'l', col = color.agg[agg_imethods], lty = 1, main = sum(Sagg[iagg, ]), 
            ylab = "MSE", xlab = "Horizon")
    legend("topleft", agg_methods[agg_imethods], col = color.agg[agg_imethods], lty = 1, cex = .4)
  }
}
dev.off()

# BOT MSE
mse_bot_byhour <- sapply(seq(n_bottom), function(ibot){
  sapply(seq_along(bot_methods), function(imethod){
    apply(matrix(mse_bottom[ibot, , imethod], ncol = 48, byrow = T), 2, mean)
  })
}, simplify = 'array')

savepdf(file.path(results.folder, paste("BOT-MSE-", comment, sep = "") ))
for(ibot in seq(n_bottom)){
  print(ibot)
  matplot(mse_bot_byhour[, , ibot], type = 'l', col = color.bot, lty = 1)
  legend("topleft", bot_methods, col = color.bot, lty = 1)
}
dev.off()

avg_mse_agg <- apply(mse_agg_byhour, c(1, 2), mean)
matplot(avg_mse_agg, type = 'l', col = color.agg, lty = 1)

avg_mse_bot <- apply(mse_bot_byhour, c(1, 2), mean)
matplot(avg_mse_bot, type = 'l', col = color.bot, lty = 1)

mybot_methods <- c("BASE", "BASE", "BASE", "BASE-MINT", "BASE-MEANCOMB")
myavg_agg <- avg_mse_agg
myagg_methods <- agg_methods
myavg_bot <- avg_mse_bot[, match(mybot_methods, bot_methods)]
matplot( (myavg_bot + myavg_agg)/2, type = 'l', col = color.agg, lty = 1)


# AGG CRPS
crps_agg_byhour <- sapply(seq(n_agg), function(iagg){
  sapply(seq_along(agg_methods), function(imethod){
    apply(matrix(crps_agg[iagg, , imethod], ncol = 48, byrow = T), 2, mean)
  })
}, simplify = 'array')

comment <- ""
savepdf(file.path(results.folder, paste("AGG-CRPS-", comment, sep = "") ), height = 27 * 0.3)
par(mfrow = c(1, 2))
set_methods <- vector("list", 2)
set_methods[[1]] <-  c(2, 3, 5)
set_methods[[2]] <-  c(1, 4, 5)
for(iagg in seq(n_agg)){
    for(i in seq_along(set_methods)){
      agg_imethods <- set_methods[[i]]
    matplot(crps_agg_byhour[, agg_imethods, iagg], type = 'l', col = color.agg[agg_imethods], lty = 1, main = sum(Sagg[iagg, ]), 
            ylab = "CRPS", xlab = "Horizon")
    legend("topleft", agg_methods[agg_imethods], col = color.agg[agg_imethods], lty = 1, cex = .4)
  }
}
dev.off()

# BOT CRPS
crps_bot_byhour <- sapply(seq(n_bottom), function(ibot){
  sapply(seq_along(bot_methods), function(imethod){
    apply(matrix(crps_bottom[ibot, , imethod], ncol = 48, byrow = T), 2, mean)
  })
}, simplify = 'array')

savepdf(file.path(results.folder, paste("BOT-CRPS-", comment, sep = "") ))
for(ibot in seq(n_bottom)){
  print(ibot)
  matplot(crps_bot_byhour[, , ibot], type = 'l', col = color.bot, lty = 1)
  legend("topleft", bot_methods, col = color.bot, lty = 1)
}
dev.off()

# AVG AGG CRPS
avg_agg <- apply(crps_agg_byhour, c(1, 2), mean)
matplot(avg_agg, type = 'l', col = color.agg, lty = 1)

tt <- apply(total_qscores_agg, c(1, 2), mean)
matplot(tt, col = color.agg, type = "l")

# AVG BOTTOM CRPS
avg_bot <- apply(crps_bot_byhour, c(1, 2), mean)
matplot(avg_bot, type = 'l', col = color.bot, lty = 1)

tt <- apply(total_qscores_bot, c(1, 2), mean)
matplot(tt, type = 'l', col = color.bot)

# AVG BOTTOM + AGG CRPS
mybot_methods <- c("BASE", "BASE", "BASE", "BASE-MINT", "BASE-MEANCOMB")
myavg_agg <- avg_agg
myagg_methods <- agg_methods
myavg_bot <- avg_bot[, match(mybot_methods, bot_methods)]
matplot( (myavg_bot + myavg_agg)/2, type = 'l', col = color.agg, lty = 1)

# AGG QSCORES
savepdf(file.path(results.folder, paste("AGG-QSCORES", sep = "") ))
par(mfrow = c(2, 2))
for(iagg in seq(n_agg)){
  matplot(y = total_qscores_agg[, , iagg], x = seq(1, M)/M, lty = 1, type = 'l', cex = .5, 
          col = color.agg, ylab = "QS", xlab = "horizon", main = paste(aggSeries[iagg], " - nb. kids: ", sum(Sagg[iagg, ]), sep = "") )
  legend("topright", agg_methods, col = color.agg, lty = 1, cex = .5)
  #MAT <- cbind(sorted_samples_agg[, iagg,], obs_agg_idtest[iagg])
  #matplot(x = MAT, y = seq(1, M)/M, pch = 1, cex = .5, col = c("black", "red", "blue", "orange", "darkblue"))
}
dev.off()

# BOT QSCORES
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

