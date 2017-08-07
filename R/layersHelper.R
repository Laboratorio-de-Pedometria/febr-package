# What to do with the plus sign?
# Two options are implemented:
# 1. Remove plus sign, using the already existing value as the maximum observation depth.
# 2. Add a given quantity thus increasing the maximum observation depth.
# 
# We recommend adding some amount because many authors describe the depth of the last layer using a single 
# number and a plus sign. See for example ctb0014.
# 
# obj is a data.frame with the layers of one or many soil observations
# 
# Example:
# source("R/layers.R")
# res <- layers("ctb0016")
# res[, c("profund_sup", "profund_inf")]
# res <- .setMaximumObservationDepth(res)
# res[, c("profund_sup", "profund_inf")]
# 
.setMaximumObservationDepth <-
  function (obj, id.col = "observacao_id", depth.cols = c("profund_sup", "profund_inf"), plus.sign = "add",
            plus.depth = 2.5) {
    
      # Process data
      idx_plus <- grep("+", obj[, depth.cols[2]], fixed = TRUE)
      if (length(idx_plus >= 1)) {
        switch(
          plus.sign,
          # Remove plus sign
          remove = {
            obj[idx_plus, depth.cols[2]] <- gsub("+", "", obj[idx_plus, depth.cols[2]], fixed = TRUE)
          },
          # Add a given quantity
          add = {
            obj[idx_plus, depth.cols[2]] <-
              gsub("+", paste(" +", plus.depth), obj[idx_plus, depth.cols[2]], fixed = TRUE)
            obj[idx_plus, depth.cols[2]] <-
              sapply(obj[idx_plus, depth.cols[2]], function (x) eval(parse(text = x)))
          }
        )
      }
    return (obj)
  }
# What to do with wavy and irregular layer transitions?
# 
# Note that we can only deal with thansitions after we have dealt with the plus sign
# 
# Note that layers with missing data can only be dropped after we have dealt with transitions 
# 
# obj is a data.frame with the layers of many soil observations
# 
# Example 1 (one):
# rm(list = ls())
# source("R/layers.R")
# res <- layers("ctb0011", missing.data = "keep")
# res[, c("profund_sup", "profund_inf")]
# res <- .solveIrregularLayerTransition(res)
# res[, c("profund_sup", "profund_inf")]
# 
# Example 2 (many):
# rm(list = ls())
# source("R/layers.R")
# res <- layers("ctb0014", missing.data = "keep")
# res[, c("profund_sup", "profund_inf")]
# res <- .setMaximumObservationDepth(res)
# res[, c("profund_sup", "profund_inf")]
# res2 <- .solveIrregularLayerTransition(res)
# cbind(res[, c("profund_sup", "profund_inf")], res2[, c("profund_sup", "profund_inf")])
# 
# Example 2 different length for sup and inf:
# rm(list = ls())
# source("R/layers.R")
# source("R/standards.R")
# source("R/febrHelper.R")
# res <- layers("ctb0025", missing.data = "keep")
# res[, c("profund_sup", "profund_inf")]
# res2 <- .solveIrregularLayerTransition(res)
# cbind(res[, c("profund_sup", "profund_inf")], res2[, c("profund_sup", "profund_inf")])
#' 
#' @importFrom stats median 
.solveIrregularLayerTransition <-
  function (obj, id.col = "observacao_id", depth.cols = c("profund_sup", "profund_inf"),
            smoothing.fun = "mean") {
    
    # Note that a wavy/irregular transition at 'profund_inf' does not necessarily mean a wavy/irregular
    # transition at the next 'profund_sup' because the consistency of the order of the layers is not
    # guaranteed -- we are dealing with character data.
    idx_wavy <- lapply(obj[depth.cols], function (x) grep(pattern = "/", x = x, fixed = TRUE))
    # idx_wavy <- as.data.frame(idx_wavy)
    
    wavy <- sum(sapply(idx_wavy, length))
    if (wavy >= 1) {
    # if (nrow(idx_wavy) >= 1) {
      
      # Prepare data
      # i <- data.frame(row = c(idx_wavy[, 1], idx_wavy[, 2]), col = rep(1:2, each = nrow(idx_wavy)))
      i <- rbind(data.frame(row = idx_wavy[[1]], col = 1), data.frame(row = idx_wavy[[2]], col = 2))
      new_depth <- stringr::str_split_fixed(obj[depth.cols][as.matrix(i)], "/", Inf)
      
      # Apply smoothing function
      switch(
        smoothing.fun,
        mean = {
          obj[depth.cols][as.matrix(i)] <-
            apply(new_depth, 1, function (x) mean(as.numeric(x), na.rm = TRUE))
        },
        min = {
          obj[depth.cols][as.matrix(i)] <-
            apply(new_depth, 1, function (x) min(as.numeric(x), na.rm = TRUE))
        },
        max = {
          obj[depth.cols][as.matrix(i)] <-
            apply(new_depth, 1, function (x) max(as.numeric(x), na.rm = TRUE))
        },
        median = {
          obj[depth.cols][as.matrix(i)] <-
            apply(new_depth, 1, function (x) stats::median(as.numeric(x), na.rm = TRUE))
        }
      )
    }
    return (obj)
  }
# What to do with broken layer transitions?
# 
# obj is a data.frame with the layers of one or more soils observations
# 
# Example:
# rm(list = ls())
# source("R/layers.R")
# source("R/standards.R")
# res <- layers("ctb0643", missing.data = "keep")
# res[res$observacao_id == "Perfil-01", ]
# res <- .solveBrokenLayerTransition(res)
# res[res$observacao_id == "Perfil-01", ]
# res[res$observacao_id == "Perfil-10", ]
# 
# res <- layers("ctb0002", missing.data = "keep")
# res
# res <- .solveBrokenLayerTransition(res)
# res
.solveBrokenLayerTransition <-
  function (obj, id.cols = c("observacao_id", "camada_numero", "camada_nome", "amostra_codigo"),
            depth.cols = c("profund_sup", "profund_inf")) {
    
    # Identificar colunas potencialmente com dados de ferro
    fe_cols <- colnames(obj)[grep("^fe_", colnames(obj))]
    
    # Split layers by observation for further processing 
    split_obj <- split(obj, obj$observacao_id)
    
    # Type 1.
    # Two or more layers have equal values for 'profund_sup' (but not necessarilly for 'profund_inf'),  
    # meaning that they start at the same depth (but not necessarilly end at the same depth).
    broken1 <- any(sapply(split_obj, function (x) any(duplicated(x[depth.cols[1]]))))
    if (broken1) {
      res <- lapply(split_obj, function (obj) {
        
        # Which layers share the same 'profund_sup'?
        idx <-  match(obj[, depth.cols[1]], obj[, depth.cols[1]])

        # Split layers by 'profund_sup'
        new_obj <- split(obj, idx)
        idx2 <- sapply(new_obj, function (x) nrow(x) > 1)
          
        # Process data
        new_obj[idx2] <- lapply(new_obj[idx2], function (x) {
          
          width <- as.numeric(x$profund_inf) - as.numeric(x$profund_sup)
          total <- sum(width)
          width <- width / total
          id_top <- which.min(as.numeric(x$profund_sup))
          id_bottom <- which.max(as.numeric(x$profund_inf))
          
          # Use the first layer to stare new data
          x[1, id.cols[-1]] <- apply(x[id.cols[-1]], 2, paste, collapse = "+")
          x[1, depth.cols] <- c(x[id_top, depth.cols[1]], x[id_bottom, depth.cols[2]])
          x[1, fe_cols] <- colSums(x[fe_cols] * width)
          
          # Return only the first layer
          x[1, ]
        })
        do.call(rbind, new_obj)
      })
      res <- do.call(rbind, c(res, make.row.names = FALSE, stringsAsFactors = FALSE))  
    } else {
      res <- obj
    }
    return (res)
  }
