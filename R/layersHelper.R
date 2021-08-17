# What to do with the plus sign?
# Two options are implemented:
# 1. Remove plus sign, using the already existing value as the maximum observation depth.
# 2. Add a given quantity thus increasing the maximum observation depth.
# 
# We recommend adding some amount because many authors describe the depth of the last layer using a
# single number and a plus sign. See for example ctb0014.
# 
# obj is a data.frame with the layers of one or many soil observations
# 
# Example:
# source("R/layer.R")
# source("R/layersHelper.R")
# source("R/febrHelper.R")
# Sinal de adição na última profundidade
# res <- layer("ctb0016")
# (res <- res[res$observacao_id == "LVd-1", c("profund_sup", "profund_inf")])
# .setMaximumObservationDepth(res)
# Última profundidade ondulada/irregular com sinal de adição
# res <- layer("ctb0770")
# (res <- res[res$observacao_id == "E85", c("profund_sup", "profund_inf")])
# .setMaximumObservationDepth(res)
.setMaximumObservationDepth <-
  function(obj, id.col = "observacao_id", depth.cols = c("profund_sup", "profund_inf"),
            plus.sign = "add", plus.depth = 2.5) {
    if (!requireNamespace("stringr")) stop("stringr package is missing")
    plus.depth <- paste("+", plus.depth)
    
    # Process data
    idx_plus <- grep("+", obj[, depth.cols[2]], fixed = TRUE)
    if (length(idx_plus >= 1)) {
      switch(
        plus.sign,
        
        # Remove plus sign
        remove = {
          obj[idx_plus, depth.cols[2]] <- gsub("+", "", obj[idx_plus, depth.cols[2]], fixed = TRUE)
        },
        
        # Adicionar uma dada quantidade definida pelo usuário.
        # É preciso atentar para o fato de que a última profundidade, por mais incrível que pareça,
        # também pode ser irregular, ou seja, identificada por uma barra (/). Nesse caso, a simples
        # avaliação da expressão usando eval() e parse() resultaria numa operação de divisão.
        add = {
          obj[idx_plus, depth.cols[2]] <-
            gsub("+", plus.depth, obj[idx_plus, depth.cols[2]], fixed = TRUE)
          obj[idx_plus, depth.cols[2]] <-
            sapply(obj[idx_plus, depth.cols[2]], function(x) {
              is_broken <- grepl(pattern = "/", x = x, fixed = TRUE)
              if (is_broken) {
                y <- stringr::str_split_fixed(x, "/", Inf)
                y <- sapply(y, function(y) eval(parse(text = y)))
                paste(y, collapse = "/")
              } else {
                eval(parse(text = x))
              }
            })
        }
      )
    }
    return(obj)
  }

# Símbolo indicador do limite inferior de detecção do método de determinação (<) ###################
# obj <- febrlayer(dataset = "ctb0018", variable = "prata")
# obj <- data.frame(a = c("<50a", "5,0"), stringsAsFactors = FALSE)
# .setLowestMeasuredValue(obj = obj, lessthan.frac = 0.5)
# x <- tmp$zinco_aquaregia_icpms
# .hasLessThanSign(x)
.hasLessThanSign <-
  function(x) {
    all(any(grepl(pattern = "^<[0-9]+", x = x)) && all(!grepl(pattern = "[:alpha:]", x = x)))
    # all(nchar(x = x) <= 10)
  }
.setLowestMeasuredValue <-
  function(obj, lessthan.sign = "subtract", lessthan.frac = 0.5) {
    
    # if (!requireNamespace("glue")) stop("glue package is missing")
    
    # Variáveis contínuas interpretadas como corrente de caracteres
    id_class <- sapply(obj, class)
    id_class <- id_class[!names(id_class) %in% c("dataset_id", .opt()$layer$std.cols)]
    id_cha <- names(id_class[id_class %in% "character"])
    
    # A corrente de caracteres começa com o símbolo '<', seguido de um ou mais dígitos, não podendo
    # haver qualquer caracter alfabético
    if (length(id_cha) >= 1) {
      idx_lessthan <- names(which(sapply(obj[id_cha], function(x) {
        any(.hasLessThanSign(stats::na.omit(x)))
      })))
      # Processar dados
      if (length(idx_lessthan) >= 1) {
        switch(
          lessthan.sign,
          
          # Remover o sinal '<'
          # Precisa substituir vírgula por ponto como separador decimal
          remove = {
            obj[idx_lessthan] <- 
              lapply(obj[idx_lessthan], function(x) {
                # alguns casos incluem espaço após '<'
                out <- gsub(pattern = "^< ", replacement = "", x = x)
                out <- gsub(pattern = "^<", replacement = "", x = x)
                out <- gsub(pattern = "(.),(.)", replacement = "\\1.\\2", x = out)
                as.numeric(out)
              })
          },
          
          # Subtrair uma dada quantidade (fração)
          # Precisa substituir vírgula por ponto como separador decimal
          subtract = {
            obj[idx_lessthan] <-
              lapply(obj[idx_lessthan], function(x) {
                out <- gsub(
                  # pattern = "^<", replacement = glue::glue("{1 - lessthan.frac}*"), x = x)
                  pattern = "^<", replacement = paste0(1 - lessthan.frac, "*"), x = x)
                out <- gsub(pattern = "(.),(.)", replacement = "\\1.\\2", x = out)
                sapply(out, function(out) eval(parse(text = out)), USE.NAMES = FALSE)
              })
          }
        )
      }
    }
    
    return(obj)
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
# res <- .solveWavyLayerTransition(res)
# res[, c("profund_sup", "profund_inf")]
# 
# Example 2 (many):
# rm(list = ls())
# source("R/layers.R")
# res <- layers("ctb0014", missing.data = "keep")
# res[, c("profund_sup", "profund_inf")]
# res <- .setMaximumObservationDepth(res)
# res[, c("profund_sup", "profund_inf")]
# res2 <- .solveWavyLayerTransition(res)
# cbind(res[, c("profund_sup", "profund_inf")], res2[, c("profund_sup", "profund_inf")])
# 
# Example 2 different length for sup and inf:
# rm(list = ls())
# source("R/layers.R")
# source("R/standards.R")
# source("R/febrHelper.R")
# res <- layers("ctb0025", missing.data = "keep")
# res[, c("profund_sup", "profund_inf")]
# res2 <- .solveWavyLayerTransition(res)
# cbind(res[, c("profund_sup", "profund_inf")], res2[, c("profund_sup", "profund_inf")])
# 
#' @importFrom stats median 
.solveWavyLayerTransition <-
  function(obj, id.col = "observacao_id", depth.cols = c("profund_sup", "profund_inf"),
            smoothing.fun = "mean") {
    if (!requireNamespace("stringr")) stop("stringr package is missing")
    # Note that a wavy/irregular transition at 'profund_inf' does not necessarily mean a
    # wavy/irregular transition at the next 'profund_sup' because the consistency of the order of
    # the layers is not guaranteed -- we are dealing with character data.
    idx_wavy <- lapply(obj[depth.cols], function(x) grep(pattern = "/", x = x, fixed = TRUE))
    
    
    wavy <- sum(sapply(idx_wavy, length))
    if (wavy >= 1) {
      
      
      # Prepare data
      i <- rbind(data.frame(row = idx_wavy[[1]], col = 1), data.frame(row = idx_wavy[[2]], col = 2))
      new_depth <- stringr::str_split_fixed(obj[depth.cols][as.matrix(i)], "/", Inf)
      # Apply smoothing function
      switch(
        smoothing.fun,
        mean = {
          obj[depth.cols][as.matrix(i)] <-
            apply(new_depth, 1, function(x) mean(as.numeric(x), na.rm = TRUE))
        },
        min = {
          obj[depth.cols][as.matrix(i)] <-
            apply(new_depth, 1, function(x) min(as.numeric(x), na.rm = TRUE))
        },
        max = {
          obj[depth.cols][as.matrix(i)] <-
            apply(new_depth, 1, function(x) max(as.numeric(x), na.rm = TRUE))
        },
        median = {
          obj[depth.cols][as.matrix(i)] <-
            apply(new_depth, 1, function(x) stats::median(as.numeric(x), na.rm = TRUE))
        }
      )
    }
    return(obj)
  }
# What to do with broken layer transitions?
# 
# obj is a data.frame with the layers of one or more soils observations
# 
# Example:
# rm(list = ls())
# source("R/layers.R")
# source("R/standards.R")
# res <- febrlayer("ctb0643", variable = "all")
# obj <- res[res$observacao_id == "Perfil-01", 1:10]
# .solveBrokenLayerTransition(obj[c(1:7, 52)])
# res <- .solveBrokenLayerTransition(res)
# res[res$observacao_id == "Perfil-01", ]
# res[res$observacao_id == "Perfil-10", ]
# 
# res <- layers("ctb0002", missing.data = "keep")
# res
# res <- .solveBrokenLayerTransition(res)
# res
# .solveBrokenLayerTransition(obj)
# .weightedTable <-
#   function(x, w) {
#     res <- by(data = w, INDICES = x, FUN = sum)
#     res <- names(which.max(res))
#     return(res)
#   }
# .solveBrokenLayerTransition <-
#   function(obj, depth.cols = c("profund_sup", "profund_inf"), merge.fun = "weighted.mean",
#             id.cols = c("observacao_id", "camada_id", "camada_nome", "amostra_id")) {
#     
#     # Dividir camadas por 'observacao_id' 
#     split_obj <- split(x = obj, f = obj[[id.cols[1]]])
#     
#     # Tipo 1: Uma ou mais camadas possuem valores idênticos de 'profund_sup' (mas não
#     necessariamente de 'profund_inf'), indicando que elas começam na mesma profundidade (mas não
#     necessariamente terminam na mesma profundidade).
#     has_broken1 <- sapply(split_obj, function(x) any(duplicated(x[depth.cols[1]])))
#     
#     # Tipo 2: Uma ou mais camadas possuem valores idênticos de 'profund_inf' (mas não
#     necessariamente de 'profund_sup'), indicando que elas terminam na mesma profundidade (mas não
#     necessariamente começam na mesma profundidade).
#     has_broken2 <- sapply(split_obj, function(x) any(duplicated(x[depth.cols[2]])))
#     
#     if (length(has_broken1) >= 1) {
#       
#       id_class <- lapply(obj, class)
#       
#       res <- split_obj
#       
#       res[has_broken1] <- lapply(split_obj[has_broken1], function(obj) {
#         
#         # Which layers share the same 'profund_sup'?
#         idx <-  match(obj[[depth.cols[1]]], obj[[depth.cols[1]]])
# 
#         # Dividir camadas por 'profund_sup'
#         new_obj <- split(x = obj, f = idx)
#         idx2 <- sapply(new_obj, function(x) nrow(x) > 1)
#           
#         # Processar os dados
#         # Usar a primeira camada para armazenar os dados
#         new_obj[idx2] <- lapply(new_obj[idx2], function(x) {
#           
#           # Número de camadas
#           n <- nrow(x)
#           i <- sample(x = seq(n), size = 1)
#           
#           # Calcular espessura das camadas
#           thick <- as.numeric(x[[depth.cols[2]]]) - as.numeric(x[[depth.cols[1]]])
#           total <- sum(thick)
#           thick <- thick / total
#           id_top <- which.min(as.numeric(x[[depth.cols[1]]]))
#           id_bottom <- which.max(as.numeric(x[[depth.cols[2]]]))
#           
#           # Atualizar nomes das colunas de identificação e profundidades
#           x[1, id.cols[-1]] <- apply(x[id.cols[-1]], 2, paste, collapse = "+")
#           x[1, depth.cols] <- c(x[id_top, depth.cols[1]], x[id_bottom, depth.cols[2]])
#           
#           # Variáveis contínuas
#           id_con <- which(id_class %in% c("numeric", "integer"))
#           if (length(id_con) >= 1) {
#             switch(
#               merge.fun,
#               weighted.mean = {
#                 x[1, id_con] <- 
#                   apply(x[id_con], 2, function(y) stats::weighted.mean(x = y, w = thick, na.rm = TRUE))
#               },
#               mean = {
#                 x[1, id_con] <- apply(x[id_con], 2, mean, na.rm = TRUE)
#               },
#               min = {
#                 x[1, id_con] <- apply(x[id_con], 2, min, na.rm = TRUE)
#               },
#               max = {
#                 x[1, id_con] <- apply(x[id_con], 2, max, na.rm = TRUE)
#               },
#               median = {
#                 x[1, id_con] <- apply(x[id_con], 2, stats::median, na.rm = TRUE)
#               }
#             )
#           }
#           
#           # Variáveis categóricas
#           id_cat <- which(id_class %in% c("logical", "factor", "character"))
#           id_cat <- id_cat[!names(id_class[id_cat]) %in% c("dataset_id", id.cols, depth.cols)]
#           if (length(id_cat) >= 1) {
#             if (n >= 3) {
#               x[1, id_cat] <- apply(x[id_cat], 2, function(y) .weightedTable(x = y, w = thick))
#             } else {
#               x[1, id_cat] <- apply(x[id_cat], 2, function(y) y[i])
#             }
#           }
#           
#           # Retornar apenas a primeira camada
#           x[1, ]
#         })
#         do.call(rbind, new_obj)
#       })
#       res <- do.call(rbind, c(res, make.row.names = FALSE, stringsAsFactors = FALSE))  
#     } else {
#       res <- obj
#     }
#     return(res)
#   }
# Repetições de laboratório ########################################################################
.solveLayerRepetition <-
  function(obj, observation.id = "observacao_id", layer.id = "camada_id", sample.id = "amostra_id",
          combine.fun = "mean") {
    # Dividir camadas por 'observacao_id'
    split_obj <- split(x = obj, f = obj[[observation.id]])
    # Duas ou mais camadas possuem valor idêntico de 'camada_id' -- exceto NA, ou seja, quando as
    # camadas não possuem código de identificação (caso de conjuntos de dados ainda não revisados).
    # has_rep <- sapply(split_obj, function(x) any(duplicated(x[[layer.id]])))
    has_rep <- sapply(split_obj, function(x) any(duplicated(x[[layer.id]], incomparables = NA)))
    if (length(has_rep) >= 1) {
      id_class <- lapply(obj, class)
      res <- split_obj
      res[has_rep] <- lapply(split_obj[has_rep], function(obj) {
        # Quais camadas possuem o mesmo 'camada_id'?
        idx <-  match(obj[[layer.id]], obj[[layer.id]])
        # Dividir camadas por 'camada_id'
        new_obj <- split(x = obj, f = idx)
        idx2 <- sapply(new_obj, function(x) nrow(x) > 1)
        # Processar os dados
        # Usar a primeira camada para armazenar os dados
        new_obj[idx2] <- lapply(new_obj[idx2], function(x) {
          # Variáveis contínuas
          id_con <- which(id_class %in% c("numeric", "integer"))
          if (length(id_con) >= 1) {
            switch(
              combine.fun,
              mean = {
                x[1, id_con] <- apply(x[id_con], 2, mean, na.rm = TRUE)
              },
              min = {
                x[1, id_con] <- apply(x[id_con], 2, min, na.rm = TRUE)
              },
              max = {
                x[1, id_con] <- apply(x[id_con], 2, max, na.rm = TRUE)
              },
              median = {
                x[1, id_con] <- apply(x[id_con], 2, stats::median, na.rm = TRUE)
              }
            )
          }
          # Variáveis categóricas
          n <- nrow(x) # número de camadas
          i <- sample(x = seq(n), size = 1) # selecionar uma camada aleatoriamente
          id_cat <- which(id_class %in% c("logical", "factor", "character"))
          if (length(id_cat) >= 1) {
            if (n >= 3) { # Se houver três ou mais, seleciona-se a mais comum (maior frequência)
              x[1, id_cat] <- 
                as.character(
                  apply(x[id_cat], 2, function(y) names(sort(table(y), decreasing = TRUE))[1]))
            } else { # Se houver apenas duas, seleciona-se aleatoriamente
              x[1, id_cat] <- apply(x[id_cat], 2, function(y) y[i])
            }
          }
          # Remover 'amostra_id'
          x[1, sample.id] <- NA
          # Retornar apenas a primeira camada
          x[1, ]
        })
        do.call(rbind, new_obj)
      })
      res <- do.call(rbind, c(res, make.row.names = FALSE, stringsAsFactors = FALSE))  
    } else {
      res <- obj
    }
    return(res)
  }
