source("results_utils.R")

myfct <- "mean"

#measures <- c("CRPS", "CRPS Tails", "CRPS Left tail", "CRPS Right tail", "MSE", "QS")
#measures <- c("CRPS", "MSE", "QS")
measures <- c("CRPS Tails", "MSE", "QS")



#x <- c(rep(1, 6), 13, 17, 19)
x <- c(rep(1, 4), 15, 17, 19)
#x <- c(rep(1, 2), 2, 25, 26)

stopifnot(sum(x) == n_agg)
z <- cumsum(x)
z <- rev(z)
d <- length(z) + 1

for(measure in measures){
  
  list_agg_imethods <- list_bot_imethods  <- vector("list", 2)
  if(measure == "MSE"){
    list_agg_imethods[[1]] <- match(c("BASE", "NAIVEBU"), agg_methods)
    list_bot_imethods[[1]] <- match(c("BASE"), bot_methods)
    
    list_agg_imethods[[2]] <- match(c("BASE", "MINTdiag", "MINTshrink"), agg_methods)
    list_bot_imethods[[2]] <- match(c("BASE", "MINTdiag", "MINTshrink"), bot_methods)
  }else{
    list_agg_imethods[[1]] <- match(c("BASE", "NAIVEBU","PERMBU"), agg_methods)
    list_bot_imethods[[1]] <- match(c("BASE"), bot_methods) 
    
    list_agg_imethods[[2]] <- match(c("BASE", "NAIVEBU-MINT", "PERMBU-MINT", "MINTshrink"), agg_methods)
    list_bot_imethods[[2]] <- match(c("BASE", "BASE-MINT", "MINTshrink"), bot_methods)
  }
  
  #savepdf(file.path(results.folder, paste("RESULTS_2", "_", measure, sep = "")), height = 27 * 0.5, width = 27)
  
  for(myrow in c(1, 2)){
    savepdf(file.path(results.folder, paste("RESULTS_2", "_", measure, "_", myrow, sep = "")), height = 27 * 0.5)
    if(measure == "MSE"){
      par(mfrow = c(3, 3))
    }else{
      par(mfrow = c(2, 4))
    }
    
    if(myrow == 1){
      do.relative <- F
    }else{
      #do.relative <- ifelse(measure == "QS", FALSE, TRUE)
      do.relative <- TRUE
    }
    
    my_ylab <- ifelse(do.relative, paste(measure, " skill", sep = ""), measure)
    
    list_mat <- get_mat(measure, do.skill = do.relative)
    res_agg <- list_mat$res_agg
    res_bot <- list_mat$res_bot
    
    agg_imethods <- list_agg_imethods[[myrow]]
    bot_imethods <- list_bot_imethods[[myrow]]
    
    all_ranges <- vector("list", d)
    
    my_range <- NULL
    for(step in c(1, 2)){
      if(myrow == 2 && step == 2){
        #my_range <- c(min(sapply(all_ranges[-d], head, 1)), max(sapply(all_ranges[-d], tail, 1)))
        my_range <- NULL
      }
      
      # bottom
      avg_bot <- apply(res_bot[ , bot_imethods, , drop = F], c(1, 2), myfct)
      mat <- avg_bot
      
      # my_main <- paste("1 - (", n_bottom, ")", sep = "")
      x <- mean(res_info$info_nodes_bot)
      my_main <-  paste(format(x, digits = 3), " - (", n_bottom, ")", sep = "")
        
      if(measure != "QS"){
        #Averaging to obtain 24 hours 
        res <- sapply(seq(ncol(mat)), function(icol){
          x <- mat[, icol]
          sol <- sapply(seq(1, 48, by = 2), function(i){
            mean(x[seq(i, i+1)])
          })
          sol  
        })
        avg_bot <- res
      }
      
      if(step == 1){
        all_ranges[[d]] <- range(avg_bot)
        #all_ranges[[length(z) + 1]] <- c(0, 0)
      }
      
      if(step == 2){
        #my_range_bottom <- my_range
        #my_range_bottom[1] <- all_ranges[[d]][1]
        if(measure == "QS"){
          #matplot(x = q_probs, y = avg_bot, col = color.bot[bot_imethods], type = 'l', lty = 1, ylab = "QS", xlab = "Probability level", main = my_main)
          matplot(x = q_probs, y = avg_bot, type = 'l', lty = lty.bot[bot_imethods], ylab = "QS", xlab = "Probability level", main = my_main, col = "black")
          
        }else{
          #matplot(avg_bot, type = 'l', lty = lty.bot[bot_imethods], 
          #        ylab = "", xlab= "Horizon", ylim = my_range, col = "black", main = paste("1 - (", n_bottom, ")", sep = ""))
          
          matplot(avg_bot, type = 'o', ylab = my_ylab, xlab=  "Hour of the day", ylim = my_range, 
                  col = "black", main = my_main,
                  pch = pch.bot[bot_imethods], cex = .5, lty = 1, xaxt = "n")
          
          tday_hourly <- tday[seq(1, 48, 2)]
          idaxis <- seq(1, 24, by = 6)
          axis(1, labels = tday_hourly[idaxis], at = idaxis, cex.axis =  0.6)
        }
        
        # keep agg here
        legend("bottomleft", agg_methods[agg_imethods], lty = 1, pch = pch.agg[agg_imethods], cex = .5)
      }
      
      # levels
      for(i in seq_along(z)){
        
        #if(i == 1){
        #  interval <- seq(1, z[i])
        #}else{
        #  interval <- seq(z[i - 1] + 1, z[i])
        #}
        if(i == length(z)){
          interval <- seq(z[i], 1)
        }else{
          interval <- seq(z[i], z[i + 1] + 1)
        }
        
        #print(interval)
        
        mat <- apply(res_agg[, agg_imethods, agg_nodes_order[interval], drop = F], c(1, 2), myfct)
        
        if(measure != "QS"){
          
          # smoothing
          #res <- sapply(seq(ncol(mat)), function(icol){lowess(mat[, icol], f = 1/6)$y})
          #mat <- res
          
           #Averaging to obtain 24 hours 
           res <- sapply(seq(ncol(mat)), function(icol){
          		x <- mat[, icol]
          		sol <- sapply(seq(1, 48, by = 2), function(i){
          	        mean(x[seq(i, i+1)])
          	      })
            	    sol  
          	})
          mat <- res
          
          #res <- sapply(seq(ncol(mat)), function(icol){lowess(mat[, icol], f = 1/6)$y})
          #mat <- res
        }
        
        if(step == 1){
          all_ranges[[i]] <- range(mat)
        }
        
        
        if(step == 2){
            my_main <- ""
          #if(exists("iweight") && iweight == 1){
            x <- mean(res_info$info_nodes_agg[agg_nodes_order[interval]])
            my_main <- paste(format(x, digits = 3), " - ", "(", length(interval), ")", sep = "")
          #}
          
          #matplot(mat, type = 'l', col = color.agg[agg_imethods], lty = 1, main = my_main, 
          #    ylab = my_ylab, xlab = "Horizon", ylim = my_range)
          
          if(measure == "QS"){
            #matplot(x = q_probs, y = mat, col = color.agg[agg_imethods], type = 'l', lty = 1, ylab = "QS", xlab = "Probability level", main = my_main)
            
            #id <- seq(1, 5760, length.out = 50)
            matplot(x = q_probs, y = mat, type = 'l', lty = lty.agg[agg_imethods], col = "black",
                    ylab = "QS", xlab = "Probability level", main = my_main, ylim = c(quantile(mat, 0.005), max(mat)))
            #if(myrow == 2){
            #  browser()
            #}
          }else{
            #matplot(mat, type = 'l', col = color.agg[agg_imethods], lty = 1, main = my_main, 
            #	ylab = my_ylab, xlab = "Horizon", ylim = my_range)
            
            #matplot(mat, 
            #        type = 'l', lty = lty.agg[agg_imethods], col = "black",
            #        ylab = "", xlab = "Horizon", main = my_main, ylim = my_range)
            
            matplot(mat, 
                    type = 'o', lty = 1, col = "black",
                    ylab = my_ylab, xlab = "Hour of the day", main = my_main, ylim = my_range, pch = pch.agg[agg_imethods], cex = .5,
                    xaxt = "n")
            
            tday_hourly <- tday[seq(1, 48, 2)]
            idaxis <- seq(1, 24, by = 6)
            axis(1, labels = tday_hourly[idaxis], at = idaxis, cex.axis =  0.6)
            
          }
          #if(i == 1){
          #  legend("bottomleft", agg_methods[agg_imethods], lty = lty.agg[agg_imethods], cex = .5)
          #}
          
        }# step
      }# i
      
     
    }
    dev.off()
  }# myrow
  
}

