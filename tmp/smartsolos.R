# ##################################################################################################
# sibcs <-
#   function(obj, token, ...) {
#     cmd <- paste0(
#       "curl -k -X POST 'https://api.cnptia.embrapa.br/smartsolos/sibcs/v1/classification' ",
#       "-H  'accept: application/json' -H  'Content-Type: application/json' -H  'Authorization: ",
#       "Bearer ", token, "' -d '", obj, "'")
#     res <- system(command = cmd, intern = TRUE)
#     res <- jsonlite::fromJSON(res)
#     return(res$items)
#   }
# ##################################################################################################
# profiles <- observation(
#   dataset = "ctb0025", variable = c("taxon_sibcs", "relevo_drenagem"),
#   standardization = list(units = TRUE, round = TRUE))
# idx <- profiles$observacao_id[9]
# profiles <- profiles[profiles$observacao_id %in% idx, ]
# horizons <- layer(
#   dataset = "ctb0025", variable = "all",
#   standardization =
#     list(plus.sign = "remove", lessthan.sign = "remove",
#          transition = "smooth", units = TRUE, round = TRUE))
# horizons <- horizons[horizons$observacao_id %in% idx, ]
# horizons[, 7:46] <- lapply(horizons[, 7:46], as.numeric)
# horizons <- cbind(
#   horizons,
#   morphology(x = horizons$morfologia_descricao, variable = "color"),
#   morphology(x = horizons$morfologia_descricao, variable = "structure"),
#   stringsAsFactors = FALSE)
# file <- paste0("~/ownCloud/febr-repo/equipe/smartsolos/febr2smartsolos-", idx, ".json")
# febr2smartsolos(profiles, horizons, file)
# x <- sibcs(
#   obj = jsonlite::toJSON(jsonlite::read_json(file), pretty = TRUE, auto_unbox = TRUE),
#   # obj = febr2smartsolos(profiles = profiles, horizons = horizons),
#   token = "0eba4f14-68a8-32dd-8b1e-384a017de0b6")
# profiles$taxon_sibcs_xxxx;x
