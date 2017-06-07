source("results_utils.R")
color.agg[match(c("PERMBU-MINT", "NAIVEBU-MINT"), agg_methods)] <- color.agg[match(c("PERMBU", "NAIVEBU"), agg_methods)]

do.skill <- TRUE
do.colors <- TRUE


better_names <- function(x){
  if(measure == "MSE"){
    x[which(x == "NAIVEBU")] <- "IndepBU and DepBU"
  }else if(measure == "CRPS" || measure == "CRPS Tails"){
    x[which(x == "NAIVEBU")] <- "IndepBU"
    x[which(x == "PERMBU")] <- "DepBU"
    x[which(x == "NAIVEBU-MINT")] <- "IndepBU-MinTShrink"
    x[which(x == "PERMBU-MINT")]  <- "DepBU-MinTShrink"
  }
  x[which(x == "MINTdiag")] <- "MinTDiag"
  x[which(x == "MINTshrink")] <- "MinTShrink"
  
  return(x)
}

grouping_hours <- c(10, 8, 8, 8, 8, 6) 
myfactor_hours <- rep(seq(length( grouping_hours )), times = grouping_hours)

grouping_series_agg <- c(rep(1, 6), 8, 5, 7, 10, 9, 5, 5)
myfactor_series_agg <- rep(seq(length( grouping_series_agg )), times = grouping_series_agg)

grouping_series_bot <- 526*3
myfactor_series_bot <- rep(seq(length( grouping_series_bot )), times = grouping_series_bot)

measures <- c("CRPS", "CRPS Tails", "MSE")
for(measure in measures){
  print(measure)
  list_mat <- get_mat(measure, do.skill = FALSE)
  
  for(id in 1:2){
    if(measure == "MSE"){
      if(id == 1){
        #algos <- c("BASE", "NAIVEBU")
        algos <- c("NAIVEBU")
      }else{
        #algos <- c("BASE", "MINTdiag", "MINTshrink")
        algos <- c("MINTdiag", "MINTshrink")
      }	
    }else{
      if(id == 1){
        #algos <- c("BASE", "NAIVEBU", "PERMBU")
        algos <- c("NAIVEBU", "PERMBU")
      }else{
        #algos <- c("BASE", "PERMBU-MINT", "NAIVEBU-MINT", "MINTdiag", "MINTshrink")
        algos <- c("PERMBU-MINT", "NAIVEBU-MINT", "MINTdiag", "MINTshrink")
      }
    }
    id_wanted_agg <- match(algos, agg_methods)
    id_wanted_bot <- match(sapply(algos, to_aggname), bot_methods)  
    if(do.colors){
      mycolors <- color.agg[id_wanted_agg]
    }else{
      mycolors <- "black"
    }   
    
    filename <- paste("RESULTS_JASA_", gsub(" ", "", measure, fixed = TRUE), "_", id, "_", ifelse(do.colors, "COLOR", "BLACK"), sep = "")
    
    savepdf(file.path(results.folder, filename))
    #pdf(file.path(results.folder, filename), width = 7, height = 3.5)
    #filename <- paste(filename, ".pdf", sep = "")
    par(mfrow = c(2, 3))
    
    results_bot <- list_mat$res_bot
    results_bot_avg <- apply(results_bot, c(2, 3), function(x){ tapply(x, myfactor_hours, mean) })
    results_bot_avg <- apply(results_bot_avg, c(1, 2), function(x){ tapply(x, myfactor_series_bot, mean) })
    x_nbbot <- 1
    
    results_agg <- list_mat$res_agg
    results_agg <- results_agg[, , node_order]
    results_agg_avg <- apply(results_agg, c(2, 3), function(x){ tapply(x, myfactor_hours, mean) })
    results_agg_avg <- apply(results_agg_avg, c(1, 2), function(x){ tapply(x, myfactor_series_agg, mean) })
    results_agg_avg <- aperm(results_agg_avg, c(2, 3, 1))
    x_nbagg <- tapply(res_info$info_nodes_agg[node_order], myfactor_series_agg, mean)
    
    
    for(k in seq( length(grouping_hours) ) ){
      print(k)
      maink <- paste(tday[range(which(k == myfactor_hours))], collapse = " - ")
      
      i_base <- match("BASE", bot_methods)
      u_bot <- t( (results_bot_avg[k, i_base] - t(results_bot_avg[k, id_wanted_bot]))/results_bot_avg[k, i_base])
      u_bot <- t(u_bot) * 100
      
      i_base <- match("BASE", agg_methods)
      u_agg <- t( (results_agg_avg[k, i_base, ] - t(results_agg_avg[k, id_wanted_agg, ]))/results_agg_avg[k, i_base, ])
      u_agg <- t(u_agg) * 100
      
      if(length(id_wanted_agg) == 1)
        u_agg <- t(u_agg)
      
      u_all <- rbind(u_agg, u_bot)
      x_all <- c(x_nbagg, x_nbbot)
      
      matplot(log10(x_all), u_all, 
              type = 'o', pch = pch.agg[id_wanted_agg],  lty = 1, col = mycolors,
              xlab = expression(Log[10]("number of aggregated meters")), 
              ylab = paste(measure, " skill (%)", sep = ""), main = maink)
      abline(h = 0)
      if(k == 1){
        legend("bottomleft", better_names(agg_methods[id_wanted_agg]), 
               lty = 1, pch = pch.agg[id_wanted_agg], col = mycolors, cex = 1.1)
      }	
      
    } # k 
    dev.off()
  }# id
} # measures
