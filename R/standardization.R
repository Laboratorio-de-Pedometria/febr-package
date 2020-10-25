#' Data standardization
#' 
#' @param layers Data frame with soil horizon data, i.e. sampling layers.
#' 
#' @param plus.sign plus.sign
#' 
#' @param plus.depth plus.depth
#' 
#' @export
#' 
###############################################################################################################
#' @rdname standardization
depth <-
  function(
    layers, plus.sign = c("remove", "add"), plus.depth) {
    
    id.col <- "observacao_id"
    depth.cols <- c("profund_sup", "profund_inf")
    
    idx_plus <- grep("+", layers[, depth.cols[2]], fixed = TRUE)
    if (length(idx_plus >= 1)) {
      switch(
        plus.sign,
        remove = {
          layers[idx_plus, depth.cols[2]] <- 
            sub("+", "", layers[idx_plus, depth.cols[2]], fixed = TRUE)
        },
        # Adicionar determinada quantidade definida pelo usuário
        # É preciso atentar para o fato de que a última profundidade, por mais incrível que pareça, também pode
        # ser irregular, ou seja, identificada por uma barra (/). Nesse caso, a simples avaliação da expressão
        # usando eval() e parse() resultaria numa operação de divisão.
        add = {
          layers[idx_plus, depth.cols[2]] <- 
            sub("+", paste0("+", plus.depth), layers[idx_plus, depth.cols[2]], fixed = TRUE)
          layers[idx_plus, depth.cols[2]] <-
            sapply(layers[idx_plus, depth.cols[2]], function (x) {
              is_broken <- grepl(pattern = "/", x = x, fixed = TRUE)
              if (is_broken) {
                y <- stringr::str_split_fixed(x, "/", Inf)
                y <- sapply(y, function (y) eval(parse(text = y)))
                paste(y, collapse = "/")
              } else {
                eval(parse(text = x))
              }
            })
        }
      )
    }
    return(layers)
  }
