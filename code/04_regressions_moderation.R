# This program run moderation analysis
set.seed(1234)

# Oppening ----------------------------------------------------------------
df <- readRDS(paste(data_dir, "/final/", data, sep = ""))

# Creating vars
df_subset <- df
df_subset$inter_tenure_stem  <- df_subset$tenure  * (as.double(df_subset$stem_background) - 1)


# Samples
pdata_top3 <- pdata.frame(df_subset, c("sigla_uf"))
pdata_top1 <- pdata_top3[pdata_top3$stem_background == 1, ]
pdata_top2 <- pdata_top3[pdata_top3$stem_position %in% c("eleito","segundo") , ]

# Final table

top3 <- plm(
  Y_deaths_sivep ~ stem_background + inter_tenure_stem ,
  data = pdata_top3,
  index = c("sigla_uf"),
  model = "within" ,
  effect = "twoways"
)

top3_controls <- plm(
  Y_deaths_sivep ~ stem_background + inter_tenure_stem + mulher + ideology_party + reeleito + instrucao,
  data = pdata_top3,
  index = c("sigla_uf"),
  model = "within" ,
  effect = "twoways"
)

top2 <- plm(
  Y_deaths_sivep ~  stem_background + inter_tenure_stem ,
  data = pdata_top2,
  index = c("sigla_uf"),
  model = "within" ,
  effect = "twoways"
)
top2_controls <- plm(
  Y_deaths_sivep ~  stem_background + inter_tenure_stem + mulher + ideology_party + reeleito + instrucao ,
  data = pdata_top2,
  index = c("sigla_uf"),
  model = "within"  ,
  effect = "twoways"
)

top1 <- plm(
  Y_deaths_sivep ~  tenure,
  data = pdata_top1,
  index = c("sigla_uf"),
  model = "within" ,
  effect = "twoways"
)
top1_controls <- plm(
  Y_deaths_sivep ~  tenure + mulher + ideology_party + reeleito + instrucao ,
  data = pdata_top1,
  index = c("sigla_uf"),
  model = "within"  ,
  effect = "twoways"
)


models <- list(
  "STEM Top 3" = top3,
  "STEM Top 3" = top3_controls,
  "STEM Top 2" = top2,
  "STEM Top 2" = top2_controls,
  "STEM Top 1" = top1,
  "STEM Top 1" = top1_controls
)

modelsummary(
  models,
  vcov = function(x) vcovHC(x, type = "HC1", cluster = "group"),
  output = paste(output_dir, "/tables/moderation_tenure.tex", sep = ""),
  title = "Moderating effects of scientific intensity on the impact of STEM background",
  stars = TRUE,
 coef_map = c(
   "stem_background1" = "STEM Background",
   "inter_tenure_stem" = "STEM x Sci. Intensity",
   "tenure" = "Scientific Intensity",
   "mulherTRUE" = "Woman Mayor",
   "ideology_party" = "Ideology of the Party",
   "reeleitoTRUE" = "Reelected",
   "instrucao" = "Education Level"
 ),
 gof_omit = "IC|Log|Adj|Within|Between|R2|RMSE|Std.Errors",
  add_rows = data.frame(
    term = c("Mayor Controls", "State FE"),
    `Model 1` = c("No", "Yes"),
    `Model 2` = c("Yes", "Yes"),
    `Model 3` = c("No", "Yes"),
    `Model 4` = c("Yes", "Yes"),
    `Model 5` = c("No", "Yes"),
    `Model 6` = c("Yes", "Yes"),
    check.names = FALSE
  )
)

