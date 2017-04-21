# color.bot <- c("black", "purple", "red", "green")
# color.agg <- c("black", "orange", "cyan", "purple", "pink", "red", "green")

#pch.bot <- c(21, 21, 23, 23)
#pch.agg <- c(21, 21, 22, 21, 22, 23, 23)

do.avg <- TRUE
if(do.avg){
  par(mfrow = c(1, 4))
}

node_nbkids <- apply(Sagg, 1, sum)
node_order <- sort(node_nbkids, index = T, decreasing = T)$ix

lty.bot <- c(1, 2, 6, 5)
lty.agg <- c(1, 3, 2, 2, 3, 6, 5)

my_ylab <- ""
measures <- c("CRPS", "CRPS Tails", "CRPS Left tail", "CRPS Right tail", "MSE", "QS")
myfct <- "mean"
#myfct <- "median"

#x <- c(1, 3, 5, 39, 7)
x <- c(1, 8, 39, 7)
#x <- rep(1, 55)

stopifnot(sum(x) == n_agg)
z <- cumsum(x)
d <- length(z) + 1

for(measure in measures){
  
  if(grepl("CRPS", measure)){ 
    if(measure == "CRPS"){
      iweight <- 1
    }else if(measure == "CRPS Tails"){
      iweight <- 2
    }else if(measure == "CRPS Right tail"){
      iweight <- 4
    }else if(measure == "CRPS Left tail"){
      iweight <- 5
    }
    mat_agg <- wcrps_agg_byhour[iweight, , , ]
    mat_bot <- wcrps_bot_byhour[iweight, , , ]
  }else if(measure == "MSE"){
    mat_agg <- mse_agg_byhour
    mat_bot <- mse_bot_byhour
  }else if(measure == "QS"){
    mat_agg <- total_qscores_agg
    mat_bot <- total_qscores_bot
  }
  
  if(FALSE){
    if(measure != "QS"){
      mat_agg <- sapply(seq_along(agg_methods), function(i){
        sapply(seq(n_agg), function(j){	
          x <- mat_agg[, i, j]
          sol <- sapply(seq(1, 48, by = 2), function(i){
            mean(x[seq(i, i+1)])
          })
          sol
        })
      }, simplify = "array")
      mat_agg <- aperm(mat_agg, c(1, 3, 2))
      
      mat_bot <- sapply(seq_along(bot_methods), function(i){
        sapply(seq(n_bottom), function(j){	
          x <- mat_bot[, i, j]
          sol <- sapply(seq(1, 48, by = 2), function(i){
            mean(x[seq(i, i+1)])
          })
          sol
        })
      }, simplify = "array")
      mat_bot <- aperm(mat_bot, c(1, 3, 2))
    }
  }
  
  mat_agg_skill <- sapply(seq_along(agg_methods), function(iaggmethod){
    (mat_agg[, match("BASE", agg_methods), ] - mat_agg[, iaggmethod, ])/mat_agg[, match("BASE", agg_methods), ]
  }, simplify = 'array')
  
  mat_bot_skill <- sapply(seq_along(bot_methods), function(ibotgmethod){
    (mat_bot[, match("BASE", bot_methods), ] - mat_bot[, ibotgmethod, ])/mat_bot[, match("BASE", bot_methods), ]
  }, simplify = 'array')
  
  mat_agg_skill <- aperm(mat_agg_skill, c(1, 3, 2))
  mat_bot_skill <- aperm(mat_bot_skill, c(1, 3, 2))
  
  ####
  if(do.avg){
    
    idwanted <- c(1, 4, 5, 6, 7)
    
    MAT <- mat_agg_skill
    res <- t(apply(MAT, c(2, 3), mean))
    matplot(x = log(node_nbkids), res[, idwanted], type = "p", pch = 22, cex = .8, main = measure, col = color.agg[idwanted])
    #res <- sapply(seq(ncol(res)), function(icol){lowess(x = rev(log(node_nbkids)),y =  res[, icol], f = 1/8)$y})
    #matlines(x = log(node_nbkids), res)
    
    #newx <- c(1, 8, 39, 7) 
    #newx <- c(rep(1, 20), 20, 10, 5)
    newx <- c(rep(1, 6), 8, 5, 7, 10, 9, 5, 5)
    stopifnot(sum(newx) == n_agg)
    newz <- cumsum(newx)
    
    newres <- xres <- NULL
    newz <- 
    for(i in seq_along(newz)){
      
      if(i == 1){
        interval <- seq(1, newz[i])
      }else{
        interval <- seq(newz[i - 1] + 1, newz[i])
      }
    newres <- rbind(newres, apply(res[node_order[interval], idwanted, drop = F], 2, mean) )
    xres <- c(xres, log(mean(node_nbkids[node_order[interval]])) )
    }
    matplot(x = xres, newres, type = 'o', pch = 21,  lty = 1, main = measure, col = color.agg[idwanted])
  }else{

    list_agg_imethods <- list_bot_imethods  <- vector("list", 2)
    list_agg_imethods[[1]] <- match(c("BASE", "NAIVEBU","PERMBU"), agg_methods)
    list_bot_imethods[[1]] <- match(c("BASE"), bot_methods) 
    
    list_agg_imethods[[2]] <- match(c("BASE", "NAIVEBU-MINT", "PERMBU-MINT", "MINTshrink"), agg_methods)
    list_bot_imethods[[2]] <- match(c("BASE", "BASE-MINT", "MINTshrink"), bot_methods)
    
    
    #list_agg_imethods[[2]] <- match(c("BASE", "NAIVEBU-MINT", "PERMBU-MINT", "MINTdiag", "MINTshrink"), agg_methods)
    #list_bot_imethods[[2]] <- match(c("BASE", "BASE-MINT", "MINTdiag", "MINTshrink"), bot_methods)
    
    
    #savepdf(file.path(results.folder, paste("ZPROBRES", "_", measure, sep = "")), height = 27 * 0.5, width = 27)
    par(mfrow = c(2, 5))
    
    for(myrow in c(1, 2)){
      if(myrow == 1){
        do.relative <- F
      }else{
        do.relative <- T
      }
      
      #if(measure == "qs"){
      #	do.relative <- F
      #}
      
      agg_imethods <- list_agg_imethods[[myrow]]
      bot_imethods <- list_bot_imethods[[myrow]]
      
      all_ranges <- vector("list", d)
      
      my_range <- NULL
      for(step in c(1, 2)){
        if(myrow == 2 && step == 2){
          my_range <- c(min(sapply(all_ranges[-d], head, 1)), max(sapply(all_ranges[-d], tail, 1)))
          #my_range[1] <- 0.7 * my_range[1]
        }
        for(i in seq_along(z)){
          
          if(i == 1){
            interval <- seq(1, z[i])
          }else{
            interval <- seq(z[i - 1] + 1, z[i])
          }
          
          mat <- apply(mat_agg[, agg_imethods, node_order[interval], drop = F], c(1, 2), myfct)
          if(do.relative && measure != "QS"){
            mat <- apply(mat_agg_skill[ , agg_imethods, node_order[interval], drop = F], c(1, 2), myfct)
            #ibase <- which(agg_methods[agg_imethods] == "BASE")
            #  mat <- sapply(seq(ncol(mat)), function(imethod){
            #    (mat[, ibase] - mat[, imethod])/mat[, ibase]
            #  })
          }
          
          if(measure != "QS"){
            # Averaging to obtain 24 hours 
            res <- sapply(seq(ncol(mat)), function(icol){lowess(mat[, icol], f = 1/8)$y})
            mat <- res
            
            #res <- sapply(seq(ncol(mat)), function(icol){
            #		x <- mat[, icol]
            #		sol <- sapply(seq(1, 48, by = 2), function(i){
            #	        mean(x[seq(i, i+1)])
            #	      })
            #	    sol  
            #	})
            #mat <- res
          }
          
          if(step == 1){
            all_ranges[[i]] <- range(mat)
          }
          
          
          if(step == 2){
            my_main <- ""
            if(exists("iweight") && iweight == 1){
              x <- mean(node_nbkids[interval])
              my_main <- paste(format(x, digits = 3), " - ", "(", length(interval), ")", sep = "")
            }
            #matplot(mat, type = 'l', col = color.agg[agg_imethods], lty = 1, main = my_main, 
            #    ylab = my_ylab, xlab = "Horizon", ylim = my_range)
            
            if(measure == "QS"){
              #matplot(x = q_probs, y = mat, col = color.agg[agg_imethods], type = 'l', lty = 1, ylab = "QS", xlab = "Probability level", main = my_main)
              matplot(x = q_probs, y = mat, type = 'l', lty = lty.agg[agg_imethods], col = "black",
                      ylab = "QS", xlab = "Probability level", main = my_main)
              
            }else{
              #matplot(mat, type = 'l', col = color.agg[agg_imethods], lty = 1, main = my_main, 
              #	ylab = my_ylab, xlab = "Horizon", ylim = my_range)
              
              matplot(mat, type = 'l', lty = lty.agg[agg_imethods], col = "black", 
                      ylab = my_ylab, xlab = "Horizon", main = my_main, ylim = my_range)
              
            }
          }
        }
        
        avg_bot <- apply(mat_bot[ , bot_imethods, , drop = F], c(1, 2), myfct)
        if(do.relative){
          avg_bot <- apply(mat_bot_skill[ , bot_imethods, , drop = F], c(1, 2), myfct)
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
            #matplot(avg_bot, type = 'l', lty = 1, ylab = "", xlab= "Horizon", ylim = my_range)
            matplot(avg_bot, type = 'l', lty = lty.bot[bot_imethods], ylab = "", xlab= "Horizon", ylim = my_range, col = "black")
          }
        }
        
      }
    }
    
    #dev.off()
  }
}