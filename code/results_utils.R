
getInfoNode <- function(typeinfo)
{
  if(typeinfo == "nb_nodes"){
    info_nodes_agg <- apply(Sagg, 1, sum)
    info_nodes_bottom <- rep(1, n_bottom)
  }else if(typeinfo == "kwh"){
    for(do.agg in c(TRUE, FALSE)){
      nseries <- ifelse(do.agg, n_agg, n_bottom)
      x <- numeric(nseries)
      for(iseries in seq(nseries)){
        if(do.agg){
          idseries <- aggSeries[iseries]
          load(file.path(aggseries.folder, paste("series-", idseries, ".Rdata", sep = "")))
        }else{
          idseries <- bottomSeries[iseries]
          load(file.path(mymeters.folder, paste("mymeter-", idseries, ".Rdata", sep = "")))
        }
        #x[iseries] <- mean(demand)
        x[iseries] <- mean(apply(matrix(demand, nrow = 2), 2, sum))
      }
      if(do.agg){
        info_nodes_agg <- x
      }else{
        info_nodes_bottom <- x
      }
    }
  }
  list(info_nodes_agg = info_nodes_agg, info_nodes_bottom = info_nodes_bottom)
}


get_mat <- function(measure, do.skill){
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
    res_agg <- wcrps_agg_byhour[iweight, , , ]
    res_bot <- wcrps_bot_byhour[iweight, , , ]
  }else if(measure == "MSE"){
    res_agg <- mse_agg_byhour
    res_bot <- mse_bot_byhour
  }else if(measure == "QS"){
    res_agg <- total_qscores_agg
    res_bot <- total_qscores_bot
  }else{
    stop("error")
  }
  
  if(do.skill){
    mat_agg_skill <- sapply(seq_along(agg_methods), function(iaggmethod){
      (res_agg[, match("BASE", agg_methods), ] - res_agg[, iaggmethod, ])/res_agg[, match("BASE", agg_methods), ]
    }, simplify = 'array')
    
    mat_bot_skill <- sapply(seq_along(bot_methods), function(ibotgmethod){
      (res_bot[, match("BASE", bot_methods), ] - res_bot[, ibotgmethod, ])/res_bot[, match("BASE", bot_methods), ]
    }, simplify = 'array')
    
    res_agg <- aperm(mat_agg_skill, c(1, 3, 2))
    res_bot <- aperm(mat_bot_skill, c(1, 3, 2))
  }
  
  list(res_agg = res_agg, res_bot = res_bot)
}

to_aggname <- function(algo){
  if(algo %in% c("NAIVEBU", "PERMBU"))
  {
    res <- "BASE"
  }else if(algo %in% c("PERMBU-MINT", "NAIVEBU-MINT")){
    res <- "MINTshrink"
  }else{
    res <- algo
  } 
  res
}