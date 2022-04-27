# redirects for defunct functions

defunct_message <-  function(){
  message("COINr syntax and functionality has significantly changed. See vignette('v1') for details.")
}

COINrX_message <-  function(){
  stop("This function has been removed from the main COINr package but is now available in COINrX.
       See vignette('v1') for details.",  call. = FALSE)
}

BoxCox <- function(...){
  defunct_message()
  .Defunct("boxcox")
}

COINToolIn <- function(...){
  defunct_message()
  .Defunct("import_COIN_tool")
}

assemble <- function(...){
  defunct_message()
  .Defunct("new_coin")
}

build_ASEM <- function(...){
  defunct_message()
  .Defunct("build_example_coin")
}

checkData <- function(...){
  defunct_message()
  .Defunct("Screen")
}

coin2Excel <- function(...){
  defunct_message()
  .Defunct("export_to_excel")
}

coin_win <- function(...){
  defunct_message()
  .Defunct("winsorise")
}

colourTable <- function(...){
  COINrX_message()
}

compTable <- function(...){
  defunct_message()
  .Defunct("compare_coins")
}

compTableMulti <- function(...){
  defunct_message()
  .Defunct("compare_coins_multi")
}

compareDF <- function(...){
  defunct_message()
  .Defunct("compare_df")
}

copeland <- function(...){
  defunct_message()
  .Defunct("a_copeland")
}

corrweightscat <- function(...){
  COINrX_message()
}

denominate <- function(...){
  defunct_message()
  .Defunct("Denominate")
}

effectiveWeight <- function(...){
  defunct_message()
  .Defunct("get_eff_wts")
}

extractYear <- function(...){
  defunct_message()
  message("This function has been incorporated inside of new_coin.")
  .Defunct("new_coin")
}

geoMean <- function(...){
  defunct_message()
  .Defunct("a_gmean")
}

geoMean_rescaled <- function(...){
  defunct_message()
  .Defunct("a_gmean")
}

getCorr <- function(...){
  defunct_message()
  .Defunct("get_corr")
}

getCronbach <- function(...){
  defunct_message()
  .Defunct("get_cronbach")
}

getIn <- function(...){
  defunct_message()
  .Defunct("get_data")
}

getPCA <- function(...){
  defunct_message()
  .Defunct("get_PCA")
}

getResults <- function(...){
  defunct_message()
  .Defunct("get_results")
}

getStats <- function(...){
  defunct_message()
  .Defunct("get_stats")
}

getStrengthNWeak <- function(...){
  defunct_message()
  .Defunct("get_str_weak")
}

getUnitReport <- function(...){
  COINrX_message()
}

getUnitSummary <- function(...){
  defunct_message()
  .Defunct("get_unit_summary")
}

harMean <- function(...){
  defunct_message()
  .Defunct("a_hmean")
}

hicorrSP <- function(...){
  defunct_message()
  .Defunct("get_corr_flags")
}

impute <- function(...){
  defunct_message()
  .Defunct("Impute")
}

indChange <- function(...){
  defunct_message()
  .Defunct("change_ind")
}

indDash <- function(...){
  COINrX_message()
}
iplotBar <- function(...){
  COINrX_message()
}
iplotCorr <- function(...){
  COINrX_message()
}
iplotIndDist <- function(...){
  COINrX_message()
}
iplotIndDist2 <- function(...){
  COINrX_message()
}
iplotMap <- function(...){
  COINrX_message()
}
iplotRadar <- function(...){
  COINrX_message()
}
iplotTable <- function(...){
  COINrX_message()
}

loggish <- function(...){
  defunct_message()
  message("This function has been split into multiple functions.")
  .Defunct("Treat")
}

names2Codes <- function(...){
  defunct_message()
  .Defunct("names_to_codes")
}

noisyWeights <- function(...){
  defunct_message()
  .Defunct("get_noisy_weights")
}

normalise <- function(...){
  defunct_message()
  .Defunct("Normalise")
}

plotCorr <- function(...){
  defunct_message()
  .Defunct("plot_corr")
}

plotIndDist <- function(...){
  defunct_message()
  .Defunct("plot_dist")
}

plotIndDot <- function(...){
  defunct_message()
  .Defunct("plot_dot")
}

plotSA <- function(...){
  defunct_message()
  .Defunct("plot_sensitivity")
}

plotSARanks <- function(...){
  defunct_message()
  .Defunct("plot_uncertainty")
}

plotframework <- function(...){
  defunct_message()
  .Defunct("plot_framework")
}

rankDF <- function(...){
  defunct_message()
  .Defunct("rank_df")
}

regen <- function(...){
  defunct_message()
  .Defunct("Regen")
}

removeElements <- function(...){
  defunct_message()
  .Defunct("remove_elements")
}

replaceDF <- function(...){
  defunct_message()
  .Defunct("replace_df")
}

resultsDash <- function(...){
  COINrX_message()
}

rew8r <- function(...){
  COINrX_message()
}

roundDF <- function(...){
  defunct_message()
  .Defunct("round_df")
}

sensitivity <- function(...){
  defunct_message()
  .Defunct("get_sensitivity")
}

treat <- function(...){
  defunct_message()
  .Defunct("Treat")
}

weightOpt <- function(...){
  defunct_message()
  .Defunct("get_opt_weights")
}
