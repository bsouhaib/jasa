to_botname <- function(algo){
  if(algo %in% c("NAIVEBU", "PERMBU"))
  {
    res <- "BASE"
  }else if(algo %in% c("PERMBU-MINT")){
    res <- "BASE-MINT"
  }else if(algo %in% c("PERMBU-MCOMB")){
    res <- "BASE-MCOMB"
  }else if(algo %in% c("PERMBU-MCOMBRECON")){
    res <- "BASE-MCOMBRECON"
  }else{
    res <- algo
  } 
  res
}

better_names <- function(x){
  
  x[which(x == "PERMBU-MCOMB")] <- "PERMBU-GTOP1"
  x[which(x == "PERMBU-MCOMBRECON")] <- "PERMBU-GTOP2"
  x
}


do.skill <- TRUE
do.colors <- TRUE

grouping_hours <-  c(48)  #c(10, 8, 8, 8, 8, 6)
index_apply <- c(1, 2)
if(length(grouping_hours) == 1){
  index_apply <- 1
}
myfactor_hours <- rep(seq(length( grouping_hours )), times = grouping_hours)

grouping_series_agg <- c(rep(1, 6), 8, 5, 7, 10, 9, 5, 5)
myfactor_series_agg <- rep(seq(length( grouping_series_agg )), times = grouping_series_agg)

grouping_series_bot <- 526*3
myfactor_series_bot <- rep(seq(length( grouping_series_bot )), times = grouping_series_bot)

 
    #algos <- c("BASE", "PERMBU-MINT", "NAIVEBU-MINT", "MINTdiag", "MINTshrink")
    algos <- c("NAIVEBU", "PERMBU", "PERMBU-MINT", "PERMBU-MCOMB", "PERMBU-MCOMBRECON")

    id_wanted_agg <- match(algos, agg_methods)
    id_wanted_bot <- match(sapply(algos, to_botname), bot_methods)  
    if(do.colors){
      mycolors <- color.agg[id_wanted_agg]
    }else{
      mycolors <- "black"
    }   
    
    filename <- paste("RESULTS_ICML_", ifelse(do.colors, "COLOR", "BLACK"), sep = "")
    
    savepdf(file.path(results.folder, filename), height = 7)
    #pdf(file.path(results.folder, filename), width = 7, height = 3.5)
    #filename <- paste(filename, ".pdf", sep = "")
    par(mfrow = c(1, 2))
    
    results_bot <- crps_bot_byhour
    results_bot_avg <- apply(results_bot, c(2, 3), function(x){ tapply(x, myfactor_hours, mean) })
    results_bot_avg <- apply(results_bot_avg, index_apply, function(x){ tapply(x, myfactor_series_bot, mean) })
    if(length(grouping_hours) == 1){
      results_bot_avg <- matrix(results_bot_avg, nrow = 1)
    }
    x_nbbot <- 1
    
    results_agg <- crps_agg_byhour
    results_agg <- results_agg[, , node_order]
    results_agg_avg <- apply(results_agg, c(2, 3), function(x){ tapply(x, myfactor_hours, mean) })
    results_agg_avg <- apply(results_agg_avg, index_apply, function(x){ tapply(x, myfactor_series_agg, mean) })
    if(length(grouping_hours) == 1){
      results_agg_avg <- array(results_agg_avg, c(1, dim(results_agg_avg)))
      results_agg_avg <- aperm(results_agg_avg, c(1, 3, 2))
    }else{
      results_agg_avg <- aperm(results_agg_avg, c(2, 3, 1))
    }
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
              ylab = paste("CRPS", " skill (%)", sep = ""))
      abline(h = 0)
      if(k == 1){
        legend("topleft", better_names(agg_methods[id_wanted_agg]), 
               lty = 1, pch = pch.agg[id_wanted_agg], col = mycolors, cex = 0.7)
      }	
      
    } # k 
    avg_qs_agg <- apply(total_qscores_agg, c(1, 2), mean)
    matplot(x = q_probs, y = avg_qs_agg[, id_wanted_agg], col = mycolors, type = 'l', lty = 1, ylab = "QS", xlab = "Probability level", main = "Aggregate levels", 
            cex.axis = 0.6)
    dev.off()
 
