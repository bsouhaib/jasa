source("results_utils.R")
color.agg[match(c("PERMBU-MINT", "NAIVEBU-MINT"), agg_methods)] <- color.agg[match(c("PERMBU", "NAIVEBU"), agg_methods)]

do.skill <- TRUE
do.colors <- TRUE

 
better_names <- function(x){
  if(measure == "MSE"){
    x[which(x == "NAIVEBU")] <- "BU"
  }
  return(x)
}


#group_hours <- ceiling(seq(1, 48, length.out = 9))
#group_hours <- ceiling(seq(1, 48, length.out = 48))
#group_hours <- ceiling(seq(1, 48, length.out = 24))
group_hours <- c(1, cumsum(c(10, 8, 8, 8, 8, 6)))

measures <- c("CRPS", "CRPS Tails", "MSE")
for(measure in measures){
  list_mat <- get_mat(measure, do.skill = do.skill)
  for(id in 1:2){
    if(measure == "MSE"){
      if(id == 1){
        algos <- c("BASE", "NAIVEBU")
      }else{
        algos <- c("BASE", "MINTdiag", "MINTshrink")
      }	
    }else{
      if(id == 1){
        algos <- c("BASE", "NAIVEBU", "PERMBU")
      }else{
        algos <- c("BASE", "PERMBU-MINT", "NAIVEBU-MINT", "MINTdiag", "MINTshrink")
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
    par(mfrow = c(2, 3))
    #par(mfrow = c(2, 4))
    #par(mfrow = c(4, 6))
    
    for(k in seq(length(group_hours) -1)){
      print(k)
      if(k == 1){
        myinterval <- seq(group_hours[k], group_hours[k+1])
      }else{
        myinterval <- seq(group_hours[k] + 1, group_hours[k+1])
      }
      maink <- paste(tday[range(myinterval)], collapse = " - ")
      
      
      #####
      results_agg <- list_mat$res_agg
      results_bot <- list_mat$res_bot
      
      res_agg <- t(apply(results_agg[myinterval, , , drop = F], c(2, 3), mean))  
      res_bot <- t(apply(results_bot[myinterval, , , drop = F], c(2, 3), mean))  
      
      
      #### bottom
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
      }
      
      #### agg 
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
      }
      
      matplot(x = c(x_vec, x_vec_bot), y = rbind(y_mat, y_mat_bot),
              type = 'o', pch = pch.agg[id_wanted_agg],  lty = 1, col = mycolors, #col = color.agg[id_wanted_agg],
              xlab = "Number of aggregated meters (log scale)", ylab = paste(measure, " skill", sep = ""), main = maink)
      if(k == 1){
        legend("bottomleft", better_names(agg_methods[id_wanted_agg]), lty = 1, pch = pch.agg[id_wanted_agg], col = mycolors, cex = .8)
      }	
      
    } # k 
    dev.off()
  }# id
} # measures
