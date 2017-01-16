rm(list = ls())
source("config_paths.R")
source("config_general.R")
source("config_splitting.R")
source("jasa_utils.R")
source("utils.R")
library(parallel)

load(file.path(work.folder, "myinfo.Rdata"))

# compute the parsing order of the aggregate nodes
leaves <- V(itree)[degree(itree, mode="out") == 0]
agg_nodes <- V(itree)[degree(itree, mode="out") != 0]

for(inode in seq_along(agg_nodes)){
  agg_node <- agg_nodes[inode]
  idseries_agg <- names(agg_node)
  children_nodes <- ego(itree, order = 1, nodes = agg_node, mode = "out")[[1]][-1]
  print(children_nodes)
    
    mat_residuals <- sapply(seq_along(children_nodes), function(inode){
      child_node <- children_nodes[inode]
      isbottom <- (child_node %in% leaves)
      idseries <- names(child_node)
      if(isbottom){
        res_file <- file.path(basef.folder, "KD-IC-NML", paste("results_", idseries, "_", "KD-IC-NML", ".Rdata", sep = "")) 
        load(res_file)
        e_residuals <- sapply(tail(learn$id, -n_past_obs_kd), function(id){
          #print(id)
          #if(id == 15169)
          #  browser()
          
          iday <- getInfo(id)$iday
          iday <- iday - n_past_obs_kd/48
          
          hour <- getInfo(id)$hour
          #browser()
          if(hour %in% hours_night){
            index <- match(hour, hours_night)
            residhat <- res_residuals$res_nighthours[[iday]][[index]]$residuals
          }else{
            index <- match(hour, hours_day)
            residhat <- res_residuals$res_dayhours[[iday]][[index]]$residuals
          }
          residhat
        })
        e_residuals <- c(rep(NA, n_past_obs_kd), e_residuals)

      }else{
        resid_file <- file.path(residuals.folder, "TBATS", paste("residuals_", idseries, "_", "TBATS", "_", 1, ".Rdata", sep = "")) 
        load(resid_file)
        #e_residuals
      }
      e_residuals
    })
    
    # remove the first few rows (not available for KD)
    mat_residuals <- tail(mat_residuals, -n_past_obs_kd)
    
    #hour_alli <- getInfo(tail(learn$id, -n_past_obs_kd))$hour
    id_all <- tail(learn$id, -n_past_obs_kd)
    
    # compute ranks by time of day
    n_resid <- nrow(mat_residuals)
    stopifnot(n_resid %% 48 == 0)
    list_permutations <- list_ties <- vector("list", 48)
    for(h in seq(48)){
      
      ih <- id_all[which(calendar$periodOfDay[id_all] == h)]
      i_selected <- c(ih - 1, ih, ih + 1)
      if(h == 1 | h == 48){
        i_selected <- c(i_selected, tail(ih, 1) - 2)
      }
      iremove <- which(!(i_selected %in% id_all))
      if(length(iremove)>0){
        i_selected <- i_selected[-iremove]
      }
      i_selected <- sort(i_selected)
      
      #mat_residuals_h <- mat_residuals[seq(h, n_resid, by = 48), ]
      mat_residuals_h <- mat_residuals[match(i_selected, id_all), ]
        
      # Check ties
      vec_ties <- sapply(seq(ncol(mat_residuals_h)), function(j){
        (nrow(mat_residuals_h) - length(unique(mat_residuals_h[, j]))) / nrow(mat_residuals_h)
      }) * 100
      #print(max(vec_ties))
      
      # rank vs order ???
      permutations <- apply(mat_residuals_h, 2, rank, ties.method = "random")
      colnames(permutations) <- names(children_nodes)
      #print(dim(permutations))
      stopifnot(nrow(permutations) == M)
      
      list_permutations[[h]] <- permutations
      list_ties[[h]] <- vec_ties
  
    }
    perm_file <- file.path(permutations.folder, paste("perm_", idseries_agg, ".Rdata", sep = "")) 
    save(file = perm_file, list = c("list_permutations", "list_ties"))
}
