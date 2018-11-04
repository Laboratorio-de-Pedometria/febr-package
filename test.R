# library(febr)
camada <- layer(
  dataset = "all",
  variable = 
    c("carbono", "argila", "areia", "silte", "dsi", "ctc", "cascalho", "ferro", "fe2o3", "ph",
      "calhaus", "terrafina"),
  stack = TRUE,
  harmonization = list(harmonize = TRUE, level = 2),
  standardization = list(
    plus.sign = "remove", lessthan.sign = "remove",
    lessthan.sign = "remove",
    repetition = "combine", combine.fun = "mean",
    transition = "smooth", smoothing.fun = "mean",
    units = TRUE, units = TRUE))
camada %>% str
attributes(camada)[c("names", "field_name", "field_unit")] %>% as.data.frame()
camada$ph_cacl2 %>% range(na.rm = T)
