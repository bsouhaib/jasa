source("results_utils.R")
color.agg[match(c("PERMBU-MINT", "NAIVEBU-MINT"), agg_methods)] <- color.agg[match(c("PERMBU", "NAIVEBU"), agg_methods)]

do.mse <- TRUE 
do.slides <- TRUE
do.seperate <- TRUE

#do.skill <- ifelse(do.mse, FALSE, TRUE)
do.skill <- TRUE
######
# y <- c(res_info$info_nodes_agg, res_info$info_nodes_bot)
# plot(log(sort(1 + y) ), pch = 20)

if(do.mse){
  if(!do.seperate){
    savepdf(file.path(results.folder, paste("RESULTS_1bis", sep = "")))
    par(mfrow = c(1, 2))
  }
  measures <- c("MSE")
}else{
  if(!do.seperate){
    savepdf(file.path(results.folder, paste("RESULTS_1", sep = "")))
    #"CRPS Left tail", "CRPS Right tail"
    par(mfrow = c(2, 2))
  }
  #measures <- c("MSE", "CRPS", "CRPS Tails")
  measures <- c("CRPS", "CRPS Tails")
}

mylist <- as.list(seq(48))
mylist <- as.list(ceiling(seq(1, 48, length.out = 5)))


for(myrow in 1:2){
  
  for(k in seq(length(mylist) -1)){
    
    if(do.mse && k %in% c(1)){
      savepdf(file.path(results.folder, paste("RESULTS_MSE_", myrow, k, sep = "")))
      #par(mfrow = c(1, 2))
      par(mfrow = c(2, 2))
    }else if(!do.mse && k %in% c(1, 3)){
      savepdf(file.path(results.folder, paste("RESULTS_CRPS_", myrow, k, sep = "")))
      par(mfrow = c(2, 2))
    }
    
    
    
    print(k)
    myinterval <- mylist[[k]]:mylist[[k+1]]
    maink <- paste(tday[range(myinterval)], collapse = " - ")
    
    if(myrow == 1){
      if(do.mse){
        algos <- c("BASE", "NAIVEBU")
        #algos <- c("BASE", "NAIVEBU", "MINTdiag", "MINTshrink")
      }else{
        algos <- c("BASE", "NAIVEBU", "PERMBU")
        #algos <- c("BASE", "NAIVEBU", "PERMBU", "MINTdiag", "MINTshrink")
      }
    }else{
      if(do.mse){
        algos <- c("BASE", "MINTdiag", "MINTshrink")
      }else{
        algos <- c("BASE", "PERMBU-MINT", "NAIVEBU-MINT", "MINTdiag", "MINTshrink")
        #algos <- c("BASE", "NAIVEBU", "PERMBU-MINT", "NAIVEBU-MINT", "MINTshrink")
      }
    }
    #algos <- c("BASE", "NAIVEBU", "PERMBU", "PERMBU-MINT", "NAIVEBU-MINT", "MINTdiag", "MINTshrink")
    id_wanted_agg <- match(algos, agg_methods)
    id_wanted_bot <- match(sapply(algos, to_aggname), bot_methods)
    
    if(do.slides){
      mycolors <- color.agg[id_wanted_agg]
    }else{
      mycolors <- "black"
    }
    
    d <- length(measures)
    all_ranges <- vector("list", d)
    for(step in 1:2){
      if(step == 2){
        #my_range <- c(min(sapply(all_ranges, head, 1)), max(sapply(all_ranges, tail, 1)))
        my_range <- NULL
      }
      for(i_measure in seq_along(measures)){
        measure <- measures[i_measure]
        
        list_mat <- get_mat(measure, do.skill = do.skill)
        
        res_agg <- list_mat$res_agg
        res_bot <- list_mat$res_bot
        
        res_agg <- t(apply(res_agg[myinterval, , , drop = F], c(2, 3), mean))  
        res_bot <- t(apply(res_bot[myinterval, , , drop = F], c(2, 3), mean))  
        
        
        ## bot
        vec <- c(526, 526, 526)
        vec <- 526*3
        stopifnot(sum(vec) == n_bottom)
        cum_vec <- cumsum(vec)
        x_vec_bot <- y_mat_bot <- NULL
        for(i in seq_along(cum_vec)){
          if(i == 1){
            interval <- seq(1, cum_vec[i])
          }else{
            interval <- seq(cum_vec[i - 1] + 1, cum_vec[i])
          }
          y_mat_bot <- rbind(y_mat_bot, apply(res_bot[bot_nodes_order[interval], id_wanted_bot, drop = F], 2, mean) )
          x_vec_bot <- c(x_vec_bot, log(1 + mean(res_info$info_nodes_bot[bot_nodes_order[interval]])) )
          #bisxres <- c(bisxres, mean(node_nbkids[agg_nodes_order[interval]]))
        }
        #matplot(x = x_vec_bot, y_mat_bot, type = 'o', pch = 21,  lty = 1, main = measure, col = color.bot[id_wanted_bot])
        
        
        ## agg 
        vec <- c(rep(1, 6), 8, 5, 7, 10, 9, 5, 5)
        stopifnot(sum(vec) == n_agg)
        cum_vec <- cumsum(vec)
        
        x_vec <- y_mat <- NULL
        for(i in seq_along(cum_vec)){
          if(i == 1){
            interval <- seq(1, cum_vec[i])
          }else{
            interval <- seq(cum_vec[i - 1] + 1, cum_vec[i])
          }
          y_mat <- rbind(y_mat, apply(res_agg[agg_nodes_order[interval], id_wanted_agg, drop = F], 2, mean) )
          x_vec <- c(x_vec, log(1 + mean(res_info$info_nodes_agg[agg_nodes_order[interval]])) )
          #bisxres <- c(bisxres, mean(node_nbkids[agg_nodes_order[interval]]))
        }
        #matplot(x = x_vec, y_mat, type = 'o', pch = 21,  lty = 1, main = measure, col = color.agg[id_wanted_agg])
        if(step == 1){
          all_ranges[[i_measure]] <- range(rbind(y_mat, y_mat_bot))
        }else if(step == 2){
          matplot(x = c(x_vec, x_vec_bot), y = rbind(y_mat, y_mat_bot), ylim = my_range, cex = .5,
                  type = 'o', pch = pch.agg[id_wanted_agg],  lty = 1, col = mycolors, #col = color.agg[id_wanted_agg],
                  xlab = "Number of aggregated meters (log scale)", ylab = paste(measure, " skill", sep = ""), main = maink)
          if(measure == measures[1]){
            legend("bottomleft", agg_methods[id_wanted_agg], lty = 1, pch = pch.agg[id_wanted_agg], col = mycolors, cex = .8)
          }
          
        }    
      }
    }
    
    if(do.seperate){
      if(do.mse && k %in% c(4)){
        print("dev off")
        dev.off()
      }else if(!do.mse && k %in% c(2, 4)){
        print("dev off")
        dev.off()
      }
    }
    
    
  } # k 
  
}
