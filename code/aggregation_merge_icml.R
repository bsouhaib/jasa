rm(list = ls())
source("config_paths.R")
source("config_general.R")
source("config_splitting.R")
source("jasa_utils.R")
source("utils.R")
library(igraph)

load(file.path(work.folder, "myinfo.Rdata"))

node_nbkids <- apply(Sagg, 1, sum)
node_order <- sort(node_nbkids, index = T, decreasing = T)$ix

ntest <- length(test$id)
n_bottom <- length(bottomSeries)


  algo.agg <- "DYNREG"
  algo.bottom <- "KD-IC-NML"
  nbperjob <- 123
  njobs <- 36
  do.twentyfour <- FALSE

leaves <- V(itree)[degree(itree, mode="out") == 0]
agg_nodes <- V(itree)[degree(itree, mode="out") != 0]

depth_aggnodes <- sapply(agg_nodes, function(agg_node){
  vec <- distances(itree, agg_node, leaves, mode = "out")
  max( vec[which(vec!=Inf)])
})

#agg_methods <- c("BASE", "NAIVEBU", "PERMBU", "NAIVEBU-MINT", "PERMBU-MINT")
#color.agg <- c("black", "orange", "darkblue")
#bot_methods <- c("BASE", "BASE-MINT")
#color.bot <- c("black")

#agg_methods <- c("BASE", "NAIVEBU", "PERMBU", "PERMBU-MINT", "PERMBU-MEANCOMB")
#color.agg <- c("grey", "orange", "cyan", "purple", "darkblue")
#bot_methods <- c("BASE", "BASE-MINT", "BASE-MEANCOMB")
#color.bot <- c("black", "purple", "darkblue")

#agg_methods <- c("BASE", "NAIVEBU", "PERMBU")
#color.agg <- c("grey", "orange", "cyan")
#bot_methods <- c("BASE", "BASE-MINT")
#color.bot <- c("black", "purple")

#bot_methods <- c("BASE", "BASE-MINT", "BASE-MCOMB", "BASE-MCOMBRECON")
#color.bot <- c("black", "purple", "darkgreen", "darkblue")
#agg_methods <- c("BASE", "NAIVEBU", "PERMBU", "PERMBU-MINT", "PERMBU-MCOMB", "PERMBU-MCOMBRECON")
#color.agg <- c("grey", "orange", "cyan", "purple", "darkgreen", "darkblue")

if(FALSE){
  bot_methods <- c("BASE", "BASE-MINT", "BASE-MCOMB", "BASE-MCOMBRECON", "PROBMINT")
  color.bot <- c("black", "purple", "darkgreen", "darkblue", "green")
  agg_methods <- c("BASE", "NAIVEBU", "PERMBU", "PERMBU-MINT", "PERMBU-MCOMB", 
                   "PERMBU-MCOMBRECON", "PERMBU-MCOMBUNRECON", "NAIVEBU-MINT", "PROBMINT")
  color.agg <- c("black", "orange", "cyan", "purple", "darkgreen", "darkblue", "red", "pink", "green")
  
  agg_better_names <- c("BASE", "NAIVEBU", "PERMBU", "PERMBU-MINT", "PERMBU-GTOP1", "PERMBU-GTOP2", "PERMBU-COMB")
  bot_better_names <- c("BASE", "PERMBU-MINT", "PERMBU-GTOP1", "PERMBU-GTOP2")
}

  bot_methods <- c("BASE", "BASE-MINT", "BASE-MCOMB", "BASE-MCOMBRECON")
  color.bot <- c("black", "purple", "darkgreen", "darkblue")
  agg_methods <- c("BASE", "NAIVEBU", "PERMBU", "PERMBU-MINT", "PERMBU-MCOMB", 
                   "PERMBU-MCOMBRECON", "PERMBU-MCOMBUNRECON")
  color.agg <- c("black", "orange", "cyan", "purple", "darkgreen", "darkblue", "red")

  ##### JASA PAPER
  #bot_methods <- c("BASE", "BASE-MINT", "MINTdiag", "MINTshrink")
  #color.bot <- c("black", "purple", "red", "green")
  
  #agg_methods <- c("BASE", "NAIVEBU", "PERMBU", "PERMBU-MINT", "NAIVEBU-MINT", "MINTdiag", "MINTshrink")
  #color.agg <- c("black", "orange", "purple", "cyan", "pink", "red", "green")
  
  pch.agg <- c(8, 0, 2, 2, 0, 1, 3) 
  pch.bot <- c(8, 2, 1, 3)
  lty.agg <- c(1, 3, 2, 2, 3, 6, 5)
  lty.bot <- c(1, 2, 6, 5)


crps_agg    <- array(NA, c(n_agg, ntest, length(agg_methods)))
crps_bottom <- array(NA, c(n_bottom, ntest, length(bot_methods)))

mse_agg    <- array(NA, c(n_agg, ntest, length(agg_methods)))
mse_bottom <- array(NA, c(n_bottom, ntest, length(bot_methods)))

total_qscores_agg <- total_qscores_bot <- 0

for(idjob in seq(njobs)){
  print(idjob)
  allidtest <- (idjob - 1) * nbperjob + seq(nbperjob) 
  
  if(nbperjob == 123 && idjob == 36){
    allidtest <- 4306:4416
    #allidtest <- 4291:4416
  }
  
    res_job <- file.path(loss.folder, "LOSSICML", paste("results_HTS_", algo.agg, "_", algo.bottom, "_", idjob, ".Rdata", sep = "")) 
    load(res_job)
  
  
  # crps agg
  list_crps_agg_nonull <- list_crps_agg[-which(sapply(list_crps_agg, is.null))]
  mat_crps_agg <- sapply(seq_along(list_crps_agg_nonull),  function(i){list_crps_agg_nonull[[i]]}, simplify = 'array')
  
  # crps bot
  list_crps_bot_nonull <- list_crps_bot[-which(sapply(list_crps_bot, is.null))]
  mat_crps_bot <- sapply(seq_along(list_crps_bot_nonull),  function(i){list_crps_bot_nonull[[i]]}, simplify = 'array')
  
  
  #
  crps_bottom[, allidtest, ] <- aperm(mat_crps_bot, c(1, 3, 2))
  crps_agg[, allidtest,]     <- aperm(mat_crps_agg, c(1, 3, 2))

  
  total_qscores_agg <- total_qscores_agg + avg_qscores_agg

  list_mse_agg_nonull <- list_mse_agg[-which(sapply(list_mse_agg, is.null))]
  mat_mse_agg <- sapply(seq_along(list_mse_agg_nonull),  function(i){list_mse_agg_nonull[[i]]}, simplify = 'array')
  list_mse_bot_nonull <- list_mse_bot[-which(sapply(list_mse_bot, is.null))]
  mat_mse_bot <- sapply(seq_along(list_mse_bot_nonull),  function(i){list_mse_bot_nonull[[i]]}, simplify = 'array')
  
  mse_agg[, allidtest,]     <- aperm(mat_mse_agg, c(1, 3, 2))
  mse_bottom[, allidtest,]     <- aperm(mat_mse_bot, c(1, 3, 2))
}

total_qscores_agg <- total_qscores_agg / njobs
total_qscores_bot <- total_qscores_bot / njobs


# crps_agg   total_qscores_agg
# crps_bottom total_qscores_bot

# AGG MSE
mse_agg_byhour <- sapply(seq(n_agg), function(iagg){
  sapply(seq_along(agg_methods), function(imethod){
    res <- apply(matrix(mse_agg[iagg, , imethod], ncol = 48, byrow = T), 2, mean)
    
    if(do.twentyfour){
      res <- sapply(seq(1, 48, by = 2), function(i){
        mean(res[seq(i, i+1)])
      })
    }
    res
  })
}, simplify = 'array')

# BOT MSE
mse_bot_byhour <- sapply(seq(n_bottom), function(ibot){
  sapply(seq_along(bot_methods), function(imethod){
    res <- apply(matrix(mse_bottom[ibot, , imethod], ncol = 48, byrow = T), 2, mean)
    if(do.twentyfour){
      res <- sapply(seq(1, 48, by = 2), function(i){
        mean(res[seq(i, i+1)])
      })
    }
    res
  })
}, simplify = 'array')

# BOT CRPS
crps_bot_byhour <- sapply(seq(n_bottom), function(ibot){
  sapply(seq_along(bot_methods), function(imethod){
    res <- apply(matrix(crps_bottom[ibot, , imethod], ncol = 48, byrow = T), 2, mean)
    if(do.twentyfour){
      res <- sapply(seq(1, 48, by = 2), function(i){
        mean(res[seq(i, i+1)])
      })
    }
    res
  })
}, simplify = 'array')

# AGG CRPS
crps_agg_byhour <- sapply(seq(n_agg), function(iagg){
  sapply(seq_along(agg_methods), function(imethod){
    res <- apply(matrix(crps_agg[iagg, , imethod], ncol = 48, byrow = T), 2, mean)
    if(do.twentyfour){
      res <- sapply(seq(1, 48, by = 2), function(i){
        mean(res[seq(i, i+1)])
      })
    }
    res
  })
}, simplify = 'array')



res_info <- getInfoNode("nb_nodes")
#res_info <- getInfoNode("kwh")
agg_nodes_order <- sort(res_info$info_nodes_agg, index = T, decreasing = T)$ix
bot_nodes_order <- sort(res_info$info_nodes_bottom, index = T, decreasing = T)$ix


stop("done")

savepdf(file.path(results.folder, paste("TEMP-1", sep = "")), height = 27 * 0.5, width = 27)

ylim_1 <- c(-0.156, 0.149)
ylim_2 <- c(-0.303, 0.161)
ylim_3 <- c(-0.279, 0.248)

#agg_imethods <- match(c("BASE", "NAIVEBU-MINT", "PERMBU-MINT", "PROBMINT"), agg_methods)
#bot_imethods <- match(c("BASE", "BASE-MINT", "PROBMINT"), bot_methods) 

#agg_imethods <- match(c("BASE", "NAIVEBU-MINT", "PERMBU-MINT"), agg_methods)
#bot_imethods <- match(c("BASE", "BASE-MINT"), bot_methods) 

agg_imethods <- match(c("BASE", "NAIVEBU","PERMBU"), agg_methods)
bot_imethods <- match(c("BASE"), bot_methods) 

#agg_imethods <- match(c("BASE", "NAIVEBU-MINT", "PERMBU-MINT", "PROBMINT", "PERMBU-MCOMBRECON"), agg_methods)
#bot_imethods <- match(c("BASE", "BASE-MINT", "PROBMINT", "BASE-MCOMBRECON"), bot_methods) 

my_ylab <- ""

do.relative <- TRUE
par(mfrow = c(3, 6))
x <- c(1, 3, 5, 39, 7)
stopifnot(sum(x) == n_agg)
z <- cumsum(x)

for(iweight in c(1, 3)){
  
  if(iweight == 1){
    my_ylim <- ylim_1
  }else if(iweight == 3){
    my_ylim <- ylim_2
  }
  
  for(i in seq_along(z)){
    
    if(i == 1){
      interval <- seq(1, z[i])
    }else{
      interval <- seq(z[i - 1] + 1, z[i])
    }
    #print(interval)
    mat <- apply(wcrps_agg_byhour[iweight, , agg_imethods, node_order[interval], drop = F], c(2, 3), mean)
    
    if(do.relative){
      ibase <- which(agg_methods[agg_imethods] == "BASE")
      mat <- sapply(seq(ncol(mat)), function(imethod){
        (mat[, ibase] - mat[, imethod])/mat[, ibase]
      })
    }
    
    my_main <- ""
    if(iweight == 1){
      x <- mean(node_nbkids[interval])
      my_main <- paste(format(x, digits = 3), " - ", "(", length(interval), ")", sep = "")
    }
    
    res <- sapply(seq(ncol(mat)), function(icol){lowess(mat[, icol], f = 1/10)$y})
    #matplot(res, type = 'l', lty = 1)
    #matlines(mat)
    mat <- res
    #stop("done")
    
    matplot(mat, type = 'l', col = color.agg[agg_imethods], lty = 1, main = my_main, 
            ylab = my_ylab, xlab = "Horizon", ylim = my_ylim)
    print(range(mat))
  }
  
  avg_bot <- apply(wcrps_bot_byhour[iweight, , bot_imethods, , drop = F], c(2, 3), mean)
  
  if(do.relative){
    ibase <- which(bot_methods[bot_imethods] == "BASE")
    avg_bot <- sapply(seq(ncol(avg_bot)), function(imethod){
      (avg_bot[, ibase] - avg_bot[, imethod])/avg_bot[, ibase]
    })
  }
  
  matplot(avg_bot, type = 'l', col = color.bot[bot_imethods], lty = 1, ylab = "", xlab= "Horizon", ylim = my_ylim)
  print(range(avg_bot))
}

for(i in seq_along(z)){
  
  if(i == 1){
    interval <- seq(1, z[i])
  }else{
    interval <- seq(z[i - 1] + 1, z[i])
  }
  
  mat <- apply(mse_agg_byhour[, agg_imethods, node_order[interval], drop = F], c(1, 2), mean)
  
  if(do.relative){
    ibase <- which(agg_methods[agg_imethods] == "BASE")
    mat <- sapply(seq(ncol(mat)), function(imethod){
      (mat[, ibase] - mat[, imethod])/mat[, ibase]
    })
  }
  
  res <- sapply(seq(ncol(mat)), function(icol){lowess(mat[, icol], f = 1/10)$y})
  #matplot(res, type = 'l', lty = 1)
  #matlines(mat)
  mat <- res
  #stop("done")
  
  print(range(mat))
  
  matplot(mat, type = 'l', col = color.agg[agg_imethods], lty = 1, main = "", 
          ylab = my_ylab, xlab = "Horizon")
}          
avg_mse_bot <- apply(mse_bot_byhour[, bot_imethods, , drop = F], c(1, 2), mean)

if(do.relative){
  ibase <- which(bot_methods[bot_imethods] == "BASE")
  avg_mse_bot <- sapply(seq(ncol(avg_mse_bot)), function(imethod){
    (avg_mse_bot[, ibase] - avg_mse_bot[, imethod])/avg_mse_bot[, ibase]
  })
}
print(range(avg_mse_bot))

matplot(avg_mse_bot, type = 'l', col = color.bot[bot_imethods], lty = 1, ylab = "", xlab= "Horizon")

dev.off()









############

if(FALSE){
  test <- sapply(seq(5), function(iweight){
    sapply(seq(n_agg), function(iagg){
      sapply(seq_along(agg_methods), function(imethod){
        matrix(wcrps_agg[iweight, iagg, , imethod], ncol = 48, byrow = T)
      }, simplify = 'array')
    }, simplify = 'array')
  }, simplify = 'array')
  
  day_remove <- sort(apply(test[, , 1, 3, 1], 1, mean), decreasing = T, index = T)$ix[seq(3)]
  test <- test[-day_remove, , , , ]
  wcrps_agg_byhour <- apply(test, c(2, 3, 4, 5), mean)
  wcrps_agg_byhour <- aperm(wcrps_agg_byhour, c(4, 1, 2, 3))
}



# for(i in seq(5))
# matplot(t(test[-day_remove, , 1, 3, i]), type = 'l')

##################
# FIGURE ICML
mymethods_agg <- c("BASE", "NAIVEBU",  "PERMBU", "PERMBU-MINT", "PERMBU-MCOMB", "PERMBU-MCOMBRECON")
#mymethods <- c("BASE", "NAIVEBU",  "PERMBU", "PERMBU-MINT", "PERMBU-MCOMBRECON")

#mymethods_bot <- c("BASE", "BASE-MINT", "BASE-MCOMB", "BASE-MCOMBRECON")
mymethods_bot <- c("BASE", "BASE-MINT", "BASE-MCOMB", "BASE-MCOMBRECON")

do.skill <- FALSE
do.icml <- TRUE
myidsagg <- match(mymethods_agg, agg_methods)
myidsbot <- match(mymethods_bot, bot_methods)

#savepdf(file.path(results.folder, paste("ICML-RES", sep = "") ), width = 21 , height = 29.7 * 0.2)
savepdf(file.path(results.folder, paste("ICML-RES", sep = "") ), width = 21 * 0.6 , height = 29.7 * 0.4)

if(do.icml){
  #par(mfrow = c(1, 4))
  par(mfrow = c(2, 2))
}else{
  par(mfrow = c(1, 3))
}
avg_qs_agg <- apply(total_qscores_agg, c(1, 2), mean)
avg_agg <- apply(crps_agg_byhour, c(1, 2), mean)
avg_bot <- apply(crps_bot_byhour, c(1, 2), mean)

skillcrps_agg <- sapply(myidsagg, function(iaggmethod){
  (avg_agg[, match("BASE", agg_methods)] - avg_agg[, iaggmethod])/avg_agg[, match("BASE", agg_methods)]
})
skillqs_agg <- sapply(myidsagg, function(iaggmethod){
  (avg_qs_agg[, match("BASE", agg_methods)] - avg_qs_agg[, iaggmethod])/avg_qs_agg[, match("BASE", agg_methods)]
})
skillcrps_bot <- sapply(myidsbot, function(ibotgmethod){
  (avg_bot[, match("BASE", bot_methods)] - avg_bot[, ibotgmethod])/avg_bot[, match("BASE", bot_methods)]
})

#savepdf(file.path(results.folder, paste("MSEBOT", sep = "") ), width = 21 * 0.6 , height = 29.7 * 0.4)
avg_mse_bot <- apply(mse_bot_byhour, c(1, 2), mean)
skillmse_bot <- sapply(myidsbot, function(ibotgmethod){
  (avg_mse_bot[, match("BASE", bot_methods)] - avg_mse_bot[, ibotgmethod])/avg_mse_bot[, match("BASE", bot_methods)]
})
matplot(skillmse_bot,  type = 'l', col = color.bot, lty = 1)


if(do.skill){
  
  
  matplot(skillcrps_agg, col = color.agg[myidsagg], type = "l", lty = 1, ylab = "Skill CRPS", xlab = "Horizon",  main = "Aggregate levels")
  matplot(x = q_probs, y = skillqs_agg, col = color.agg[myidsagg], type = "l", lty = 1, ylab = "Skill QS", xlab = "Horizon",  main = "Aggregate levels")
  matplot(skillcrps_bot, col = color.bot[myidsbot], type = "l", lty = 1, main = "Bottom level")
  
}else{
  
  if(do.icml){
    set_one <- match(c("BASE", "NAIVEBU", "PERMBU"), agg_methods)
    #set_two <- match(c("BASE", "PERMBU-MINT", "PERMBU-MCOMB", "PERMBU-MCOMBRECON", "PERMBU-MCOMBUNRECON"), agg_methods)
    set_two <- match(c("BASE", "PERMBU-MINT", "PERMBU-MCOMB", "PERMBU-MCOMBRECON"), agg_methods)
    
    #matplot(avg_agg[, union(set_one, set_two) ], ylab = "CRPS", xlab = "Horizon", main = "Aggregate levels", type = "n")
    #matlines(avg_agg[, set_one], col = color.agg[set_one], lty = 1)
    #legend(20, 0.9, agg_methods[set_one], col = color.agg[set_one], lty = 1, cex = 0.4)
    #matplot(avg_agg[, union(set_one, set_two) ], ylab = "CRPS", xlab = "Horizon", main = "Aggregate levels", type = "n")
    #matlines(avg_agg[, set_two], col = color.agg[set_two], lty = 1)
    #legend(20, 0.9, agg_methods[set_two], col = color.agg[set_two], lty = 1, cex = 0.4)
    
    
    matplot(skillcrps_agg[, union(set_one, set_two) ], ylab = "Skill CRPS", xlab = "Hour of the day", main = "Aggregate levels", type = "n", xaxt = "n")
    axis(1, labels = tday[seq(1, 48, by = 8)], at = seq(1, 48, by = 8), cex.axis =  0.6)
    matlines(skillcrps_agg[, set_one], col = color.agg[set_one], lty = 1)
    
    #legend(20, -0.4, agg_methods[set_one], col = color.agg[set_one], lty = 1, cex = 0.7)
    legend(15, -0.3, agg_better_names[set_one], col = color.agg[set_one], lty = 1, cex = 0.6)
    
    matplot(skillcrps_agg[, union(set_one, set_two) ], ylab = "Skill CRPS", xlab = "Hour of the day", main = "Aggregate levels", type = "n", xaxt = "n")
    axis(1, labels = tday[seq(1, 48, by = 8)], at = seq(1, 48, by = 8), cex.axis = 0.6)
    matlines(skillcrps_agg[, set_two], col = color.agg[set_two], lty = 1)
    
    #legend(20, -0.4, agg_methods[set_two], col = color.agg[set_two], lty = 1, cex = 0.7)
    legend(15, -0.2, agg_better_names[set_two], col = color.agg[set_two], lty = 1, cex = 0.6)
    
  }else{
    matplot(avg_agg[, myidsagg], type = 'l', col = color.agg[myidsagg], lty = 1, ylab = "CRPS", xlab = "Hour of the day", main = "Aggregate levels")
    legend("topleft", agg_methods[myidsagg], col = color.agg[myidsagg], lty = 1, cex = 0.3)
  }
  
  matplot(x = q_probs, y = avg_qs_agg[, myidsagg], col = color.agg[myidsagg], type = 'l', lty = 1, ylab = "QS", xlab = "Probability level", main = "Aggregate levels", 
          cex.axis = 0.6)
  #matplot(avg_bot[, myidsbot], type = 'l', col = color.bot, lty = 1, ylab = "CRPS", xlab = "Hour of the day", main = "Bottom level")
  
  #matplot(x = q_probs, y = skillqs_agg[, myidsagg], col = color.agg[myidsagg], type = 'l', lty = 1, ylab = "Skill QS", xlab = "Probability level", main = "Aggregate levels")
  matplot(skillcrps_bot[, myidsbot], type = 'l', col = color.bot, lty = 1, ylab = "Skill CRPS", xlab = "Hour of the day", main = "Bottom level", xaxt = "n")
  axis(1, labels = tday[seq(1, 48, by = 8)], at = seq(1, 48, by = 8), cex.axis = 0.6)
}

dev.off()
##################


set_methods <- vector("list", 2)
set_methods[[1]] <-  c(2, 3, 5, 6)
set_methods[[2]] <-  c(1, 4, 5, 6, 7)
#set_methods <- vector("list", 2)
#set_methods[[1]] <-  c(1, 2, 3)
#set_methods[[2]] <-  c(1, 2, 3)

comment <- ""
savepdf(file.path(results.folder, paste("AGG-MSE", sep = "") ), height = 27 * 0.3)
par(mfrow = c(1, 2))

for(iagg in seq(n_agg)){
  for(i in seq_along(set_methods)){
    agg_imethods <- set_methods[[i]]
    matplot(mse_agg_byhour[, agg_imethods, iagg], type = 'l', col = color.agg[agg_imethods], lty = 1, main = sum(Sagg[iagg, ]), 
            ylab = "MSE", xlab = "Horizon")
    legend("topleft", agg_methods[agg_imethods], col = color.agg[agg_imethods], lty = 1, cex = .4)
  }
}
dev.off()



savepdf(file.path(results.folder, paste("BOT-MSE", sep = "") ))
for(ibot in seq(n_bottom)){
  print(ibot)
  matplot(mse_bot_byhour[, , ibot], type = 'l', col = color.bot, lty = 1)
  legend("topleft", bot_methods, col = color.bot, lty = 1)
}
dev.off()

if(FALSE){
  avg_mse_agg <- apply(mse_agg_byhour, c(1, 2), mean)
  matplot(avg_mse_agg, type = 'l', col = color.agg, lty = 1)
  avg_mse_bot <- apply(mse_bot_byhour, c(1, 2), mean)
  matplot(avg_mse_bot, type = 'l', col = color.bot, lty = 1)
  mybot_methods <- c("BASE", "BASE", "BASE", "BASE-MINT", "BASE-MEANCOMB")
  myavg_agg <- avg_mse_agg
  myagg_methods <- agg_methods
  myavg_bot <- avg_mse_bot[, match(mybot_methods, bot_methods)]
  matplot( (myavg_bot + myavg_agg)/2, type = 'l', col = color.agg, lty = 1)
}

#######
#agg_imethods <- match(c("BASE", "NAIVEBU","PERMBU", "NAIVEBU-MINT", "PERMBU-MINT", "PROBMINT"), agg_methods)
#bot_imethods <- match(c("BASE", "BASE-MINT", "PROBMINT"), bot_methods) #c(1, 2)

agg_imethods <- match(c("BASE", "NAIVEBU-MINT", "PERMBU-MINT", "PROBMINT", "NAIVEBU","PERMBU"), agg_methods)
bot_imethods <- match(c("BASE", "BASE-MINT", "PROBMINT"), bot_methods) 


#####
if(FALSE){
  ibase <- which(agg_methods[agg_imethods] == "BASE")
  relative_wcrps_agg <- sapply(seq_along(agg_methods), function(imethod){
    (wcrps_agg[, , , ibase] - wcrps_agg[, , , imethod])/wcrps_agg[, , , ibase]	
  }, simplify = "array")
  wcrps_agg_byhour <- sapply(seq(5), function(iweight){
    sapply(seq(n_agg), function(iagg){
      sapply(seq_along(agg_methods), function(imethod){
        apply(matrix(relative_wcrps_agg[iweight, iagg, , imethod], ncol = 48, byrow = T), 2, mean)
      })
    }, simplify = 'array')
  }, simplify = 'array')
  wcrps_agg_byhour <- aperm(wcrps_agg_byhour, c(4, 1, 2, 3))
}
#####


do.relative <- FALSE
#####################
savepdf(file.path(results.folder, paste("AGG-WCRPS-LEVEL",sep = "") ), height = 27 * 0.3)
par(mfrow = c(3, 5))
#par(mfrow = c(3, 5), 
#    oma = c(5,4,0,0),
#    mar = c(0,0,2,2))

for(iweight in c(1, 3)){
  if(iweight == 1){
    my_ylab <- "wCRPS (uniform)"
  }else if(iweight == 2){
    my_ylab <- "wCRPS (center)"
  }else if(iweight == 3){
    my_ylab <- "wCRPS (both tails)"
  }else if(iweight == 4){
    my_ylab <- "wCRPS (right tail)"
  }else if(iweight == 5){
    my_ylab <- "wCRPS (left tail)"
  }
  for(level in seq(4, 1)){
    id <- which(depth_aggnodes == level)
    
    mat <- apply(wcrps_agg_byhour[iweight, , agg_imethods, id, drop = F], c(2, 3), mean)
    
    
    if(do.relative){
      ibase <- which(agg_methods[agg_imethods] == "BASE")
      mat <- sapply(seq(ncol(mat)), function(imethod){
        (mat[, ibase] - mat[, imethod])/mat[, ibase]
      })
    }
    
    my_main <- paste("level ", level, " - ", "(", length(id), ")", sep = "")
    if(iweight != 1){
      my_main <- ""
    }
    if(level != 4){
      my_ylab <- ""
    }
    
    matplot(mat, type = 'l', col = color.agg[agg_imethods], lty = 1, main = my_main, 
            ylab = my_ylab, xlab = "Horizon")
    if(level == 4){
      if(!do.relative){
        legend("topleft", agg_methods[agg_imethods], col = color.agg[agg_imethods], lty = 1, cex = .3)
      }else{
        legend("bottomright", agg_methods[agg_imethods], col = color.agg[agg_imethods], lty = 1, cex = .3)
      }
    }
  }
  #avg_bot <- apply(crps_bot_byhour[, bot_imethods, , drop = F], c(1, 2), mean)
  avg_bot <- apply(wcrps_bot_byhour[iweight, , bot_imethods, , drop = F], c(2, 3), mean)
  
  if(do.relative){
    ibase <- which(bot_methods[bot_imethods] == "BASE")
    avg_bot <- sapply(seq(ncol(avg_bot)), function(imethod){
      (avg_bot[, ibase] - avg_bot[, imethod])/avg_bot[, ibase]
    })
  }
  
  matplot(avg_bot, type = 'l', col = color.bot[bot_imethods], lty = 1, ylab = "", xlab= "Horizon")
}

# MSE
for(level in seq(4, 1)){
  id <- which(depth_aggnodes == level)
  mat <- apply(mse_agg_byhour[, agg_imethods, id, drop = F], c(1, 2), mean)
  
  if(do.relative){
    ibase <- which(agg_methods[agg_imethods] == "BASE")
    mat <- sapply(seq(ncol(mat)), function(imethod){
      (mat[, ibase] - mat[, imethod])/mat[, ibase]
    })
  }
  
  my_ylab <- "MSE"
  if(level != 4){
    my_ylab <- ""
  }
  
  matplot(mat, type = 'l', col = color.agg[agg_imethods], lty = 1, main = "", 
          ylab = my_ylab, xlab = "Horizon")
  if(level == 4)
    legend("topleft", agg_methods[agg_imethods], col = color.agg[agg_imethods], lty = 1, cex = .3)
}
avg_mse_bot <- apply(mse_bot_byhour[, bot_imethods, , drop = F], c(1, 2), mean)

if(do.relative){
  ibase <- which(bot_methods[bot_imethods] == "BASE")
  avg_mse_bot <- sapply(seq(ncol(avg_mse_bot)), function(imethod){
    (avg_mse_bot[, ibase] - avg_mse_bot[, imethod])/avg_mse_bot[, ibase]
  })
}

matplot(avg_mse_bot, type = 'l', col = color.bot[bot_imethods], lty = 1, ylab = "", xlab= "Horizon")

dev.off()
#####################

savepdf(file.path(results.folder, paste("OLDDDDDAGG-WCRPS-LEVEL",sep = "") ), height = 27 * 0.3)

for(level in seq(4, 1)){
  id <- which(depth_aggnodes == level)
  
  par(mfrow = c(2, 3))
  for(iweight in seq(5)){
    
    if(iweight == 1){
      my_ylab <- "wCRPS (uniform)"
    }else if(iweight == 2){
      my_ylab <- "wCRPS (center)"
    }else if(iweight == 3){
      my_ylab <- "wCRPS (both tails)"
    }else if(iweight == 4){
      my_ylab <- "wCRPS (right tail)"
    }else if(iweight == 5){
      my_ylab <- "wCRPS (left tail)"
    }
    
    mat <- apply(wcrps_agg_byhour[iweight, , agg_imethods, id, drop = F], c(2, 3), mean)
    
    matplot(mat, type = 'l', col = color.agg[agg_imethods], lty = 1, main = paste("level ", level, " - ", "(", length(id), ")", sep = ""), 
            ylab = my_ylab, xlab = "Horizon")
    legend("topleft", agg_methods[agg_imethods], col = color.agg[agg_imethods], lty = 1, cex = .4)
  }# iweight
  plot.new()
  
  par(mfrow = c(1, 1))
  mat <- apply(mse_agg_byhour[, agg_imethods, id, drop = F], c(1, 2), mean)
  matplot(mat, type = 'l', col = color.agg[agg_imethods], lty = 1, main = paste("level ", level, " - ", "(", length(id), ")", sep = ""), 
          ylab = my_ylab, xlab = "Horizon")
  legend("topleft", agg_methods[agg_imethods], col = color.agg[agg_imethods], lty = 1, cex = .4)
  
}# level

bot_imethods <- c(1, 2)
#
avg_bot <- apply(crps_bot_byhour[, bot_imethods, , drop = F], c(1, 2), mean)
matplot(avg_bot, type = 'l', col = color.bot, lty = 1, ylab = "CRPS", xlab= "Horizon")

#
avg_mse_bot <- apply(mse_bot_byhour[, bot_imethods, , drop = F], c(1, 2), mean)
matplot(avg_mse_bot, type = 'l', col = color.bot, lty = 1, ylab = "MSE", xlab= "Horizon")

dev.off()
######




savepdf(file.path(results.folder, paste("AGG-CRPS",sep = "") ), height = 27 * 0.3)
#par(mfrow = c(1, 2))

agg_imethods <- 1:7
for(iagg in seq(n_agg)){
  #for(i in seq_along(set_methods)){
  #agg_imethods <- set_methods[[i]]
  matplot(crps_agg_byhour[, agg_imethods, iagg], type = 'l', col = color.agg[agg_imethods], lty = 1, main = sum(Sagg[iagg, ]), 
          ylab = "CRPS", xlab = "Horizon")
  legend("topleft", agg_methods[agg_imethods], col = color.agg[agg_imethods], lty = 1, cex = .4)
  #}
}
dev.off()

savepdf(file.path(results.folder, paste("AGG-CRPS-LEVEL",sep = "") ), height = 27 * 0.3)
par(mfrow = c(1, 2))
for(level in seq(4, 1)){
  id <- which(depth_aggnodes == level)
  res <- apply(crps_agg_byhour[, , id, drop = F], c(1, 2), mean)
  for(i in seq_along(set_methods)){
    agg_imethods <- set_methods[[i]]
    matplot(res[, agg_imethods], type = 'l', col = color.agg[agg_imethods], lty = 1, main = level, 
            ylab = "CRPS", xlab = "Horizon")
    legend("topleft", agg_methods[agg_imethods], col = color.agg[agg_imethods], lty = 1, cex = .4)
  }
}

avg_bot <- apply(crps_bot_byhour, c(1, 2), mean)
matplot(avg_bot, type = 'l', col = color.bot, lty = 1)
dev.off()

savepdf(file.path(results.folder, paste("AGG-WCRPS",sep = "") ), height = 27 * 0.3)
par(mfrow = c(1, 2))

for(iagg in seq(n_agg)){
  for(iweight in seq(5)){
    
    if(iweight == 1){
      my_ylab <- "wCRPS (uniform)"
    }else if(iweight == 2){
      my_ylab <- "wCRPS (center)"
    }else if(iweight == 3){
      my_ylab <- "wCRPS (both tails)"
    }else if(iweight == 4){
      my_ylab <- "wCRPS (right tail)"
    }else if(iweight == 5){
      my_ylab <- "wCRPS (left tail)"
    }
    
    for(i in seq_along(set_methods)){
      agg_imethods <- set_methods[[i]]
      matplot(wcrps_agg_byhour[iweight, ,agg_imethods, iagg], type = 'l', col = color.agg[agg_imethods], lty = 1, main = sum(Sagg[iagg, ]), 
              ylab = my_ylab, xlab = "Horizon")
      legend("topleft", agg_methods[agg_imethods], col = color.agg[agg_imethods], lty = 1, cex = .4)
    }
  }
}
dev.off()



savepdf(file.path(results.folder, paste("BOT-CRPS", sep = "") ))
for(ibot in seq(n_bottom)){
  print(ibot)
  matplot(crps_bot_byhour[, , ibot], type = 'l', col = color.bot, lty = 1)
  legend("topleft", bot_methods, col = color.bot, lty = 1)
}
dev.off()

vv <- apply(crps_bot_byhour, c(2, 3), mean)
uu <- t(t(vv) - vv[match("BASE", bot_methods), ])
matplot(t(uu), type = 'l', lty = 1)
boxplot(t(uu))

# AVG AGG CRPS
avg_agg <- apply(crps_agg_byhour, c(1, 2), mean)
matplot(avg_agg, type = 'l', col = color.agg, lty = 1)

tt <- apply(total_qscores_agg, c(1, 2), mean)
matplot(tt, col = color.agg, type = "l", lty = 1)

scaled_tt <- sapply(seq_along(agg_methods), function(iaggmethod){
  (tt[, match("BASE", agg_methods)] - tt[, iaggmethod])/tt[, match("BASE", agg_methods)]
})
matplot(scaled_tt, col = color.agg, type = "l", lty = 1)

# AVG BOTTOM CRPS
avg_bot <- apply(crps_bot_byhour, c(1, 2), mean)
matplot(avg_bot, type = 'l', col = color.bot, lty = 1)

#tt <- apply(total_qscores_bot, c(1, 2), mean)
#matplot(tt, type = 'l', col = color.bot)

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






#savepdf(file.path(results.folder, paste("TESTING", sep = "") ))
#for(i in seq(2, n_bottom)){
#  print(i)
#  avg_bot <- apply(crps_bot_byhour[, , seq(i)], c(1, 2), median)
#  matplot(avg_bot, type = 'l', col = color.bot, lty = 1)
#}
#dev.off()


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

