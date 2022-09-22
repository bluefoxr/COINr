# # redirects for defunct functions
#
# defunct_message <-  function(){
#   message("COINr syntax and functionality has significantly changed. See vignette('v1') for details.")
# }
#
# COINrX_message <-  function(){
#   stop("This function has been removed from the main COINr package but is now available in COINrX.
#        See vignette('v1') for details.",  call. = FALSE)
# }
#
# # BoxCox
# #
# # @param ... deprecated parameter
# # @export
# BoxCox <- function(...){
#   defunct_message()
#   .Defunct("boxcox")
# }
#
# #' COINToolIn
# #'
# #' @param ... deprecated parameter
# #' @export
# COINToolIn <- function(...){
#   defunct_message()
#   .Defunct("import_coin_tool")
# }
#
# #' assemble
# #'
# #' @param ... deprecated parameter
# #' @export
# assemble <- function(...){
#   defunct_message()
#   .Defunct("new_coin")
# }
#
# #' build_ASEM
# #'
# #' @param ... deprecated parameter
# #' @export
# build_ASEM <- function(...){
#   defunct_message()
#   .Defunct("build_example_coin")
# }
#
# #' checkData
# #'
# #' @param ... deprecated parameter
# #' @export
# checkData <- function(...){
#   defunct_message()
#   .Defunct("Screen")
# }
#
# #' coin2Excel
# #'
# #' @param ... deprecated parameter
# #' @export
# coin2Excel <- function(...){
#   defunct_message()
#   .Defunct("export_to_excel")
# }
#
# #' coin_win
# #'
# #' @param ... deprecated parameter
# #' @export
# coin_win <- function(...){
#   defunct_message()
#   .Defunct("winsorise")
# }
#
# #' colourTable
# #'
# #' @param ... deprecated parameter
# #' @export
# colourTable <- function(...){
#   COINrX_message()
# }
#
# #' compTable
# #'
# #' @param ... deprecated parameter
# #' @export
# compTable <- function(...){
#   defunct_message()
#   .Defunct("compare_coins")
# }
#
# #' compTableMulti
# #'
# #' @param ... deprecated parameter
# #' @export
# compTableMulti <- function(...){
#   defunct_message()
#   .Defunct("compare_coins_multi")
# }
#
# #' compareDF
# #'
# #' @param ... deprecated parameter
# #' @export
# compareDF <- function(...){
#   defunct_message()
#   .Defunct("compare_df")
# }
#
# #' copeland
# #'
# #' @param ... deprecated parameter
# #' @export
# copeland <- function(...){
#   defunct_message()
#   .Defunct("a_copeland")
# }
#
# #' corrweightscat
# #'
# #' @param ... deprecated parameter
# #' @export
# corrweightscat <- function(...){
#   COINrX_message()
# }
#
# # denominate
# #
# # @param ... deprecated parameter
# # @export
# denominate <- function(...){
#   defunct_message()
#   .Defunct("Denominate")
# }
#
# #' effectiveWeight
# #'
# #' @param ... deprecated parameter
# #' @export
# effectiveWeight <- function(...){
#   defunct_message()
#   .Defunct("get_eff_weights")
# }
#
# #' extractYear
# #'
# #' @param ... deprecated parameter
# #' @export
# extractYear <- function(...){
#   defunct_message()
#   message("This function has been incorporated inside of new_coin.")
#   .Defunct("new_coin")
# }
#
# #' geoMean
# #'
# #' @param ... deprecated parameter
# #' @export
# geoMean <- function(...){
#   defunct_message()
#   .Defunct("a_gmean")
# }
#
# #' geoMean_rescaled
# #'
# #' @param ... deprecated parameter
# #' @export
# geoMean_rescaled <- function(...){
#   defunct_message()
#   .Defunct("a_gmean")
# }
#
# #' getCorr
# #'
# #' @param ... deprecated parameter
# #' @export
# getCorr <- function(...){
#   defunct_message()
#   .Defunct("get_corr")
# }
#
# #' getCronbach
# #'
# #' @param ... deprecated parameter
# #' @export
# getCronbach <- function(...){
#   defunct_message()
#   .Defunct("get_cronbach")
# }
#
# #' getIn
# #'
# #' @param ... deprecated parameter
# #' @export
# getIn <- function(...){
#   defunct_message()
#   .Defunct("get_data")
# }
#
# #' getPCA
# #'
# #' @param ... deprecated parameter
# #' @export
# getPCA <- function(...){
#   defunct_message()
#   .Defunct("get_PCA")
# }
#
# #' getResults
# #'
# #' @param ... deprecated parameter
# #' @export
# getResults <- function(...){
#   defunct_message()
#   .Defunct("get_results")
# }
#
# #' getStats
# #'
# #' @param ... deprecated parameter
# #' @export
# getStats <- function(...){
#   defunct_message()
#   .Defunct("get_stats")
# }
#
# #' getStrengthNWeak
# #'
# #' @param ... deprecated parameter
# #' @export
# getStrengthNWeak <- function(...){
#   defunct_message()
#   .Defunct("get_str_weak")
# }
#
# #' getUnitReport
# #'
# #' @param ... deprecated parameter
# #' @export
# getUnitReport <- function(...){
#   COINrX_message()
# }
#
# #' getUnitSummary
# #'
# #' @param ... deprecated parameter
# #' @export
# getUnitSummary <- function(...){
#   defunct_message()
#   .Defunct("get_unit_summary")
# }
#
# #' harMean
# #'
# #' @param ... deprecated parameter
# #' @export
# harMean <- function(...){
#   defunct_message()
#   .Defunct("a_hmean")
# }
#
# #' hicorrSP
# #'
# #' @param ... deprecated parameter
# #' @export
# hicorrSP <- function(...){
#   defunct_message()
#   .Defunct("get_corr_flags")
# }
#
# # impute
# #
# # @param ... deprecated parameter
# # @export
# impute <- function(...){
#   defunct_message()
#   .Defunct("Impute")
# }
#
# #' indChange
# #'
# #' @param ... deprecated parameter
# #' @export
# indChange <- function(...){
#   defunct_message()
#   .Defunct("change_ind")
# }
#
# #' indDash
# #'
# #' @param ... deprecated parameter
# #' @export
# indDash <- function(...){
#   COINrX_message()
# }
#
# #' iplotBar
# #'
# #' @param ... deprecated parameter
# #' @export
# iplotBar <- function(...){
#   COINrX_message()
# }
# #' iplotCorr
# #'
# #' @param ... deprecated parameter
# #' @export
# iplotCorr <- function(...){
#   COINrX_message()
# }
#
# #' iplotIndDist
# #'
# #' @param ... deprecated parameter
# #' @export
# iplotIndDist <- function(...){
#   COINrX_message()
# }
#
# #' iplotIndDist2
# #'
# #' @param ... deprecated parameter
# #' @export
# iplotIndDist2 <- function(...){
#   COINrX_message()
# }
#
# #' iplotMap
# #'
# #' @param ... deprecated parameter
# #' @export
# iplotMap <- function(...){
#   COINrX_message()
# }
#
# #' iplotRadar
# #'
# #' @param ... deprecated parameter
# #' @export
# iplotRadar <- function(...){
#   COINrX_message()
# }
#
# #' iplotTable
# #'
# #' @param ... deprecated parameter
# #' @export
# iplotTable <- function(...){
#   COINrX_message()
# }
#
# #' loggish
# #'
# #' @param ... deprecated parameter
# #' @export
# loggish <- function(...){
#   defunct_message()
#   message("This function has been split into multiple functions.")
#   .Defunct("Treat")
# }
#
# #' names2Codes
# #'
# #' @param ... deprecated parameter
# #' @export
# names2Codes <- function(...){
#   defunct_message()
#   .Defunct("names_to_codes")
# }
#
# #' noisyWeights
# #'
# #' @param ... deprecated parameter
# #' @export
# noisyWeights <- function(...){
#   defunct_message()
#   .Defunct("get_noisy_weights")
# }
#
# # normalise
# #
# # @param ... deprecated parameter
# # @export
# normalise <- function(...){
#   defunct_message()
#   .Defunct("Normalise")
# }
#
# #' plotCorr
# #'
# #' @param ... deprecated parameter
# #' @export
# plotCorr <- function(...){
#   defunct_message()
#   .Defunct("plot_corr")
# }
#
# #' plotIndDist
# #'
# #' @param ... deprecated parameter
# #' @export
# plotIndDist <- function(...){
#   defunct_message()
#   .Defunct("plot_dist")
# }
#
# #' plotIndDot
# #'
# #' @param ... deprecated parameter
# #' @export
# plotIndDot <- function(...){
#   defunct_message()
#   .Defunct("plot_dot")
# }
#
# #' plotSA
# #'
# #' @param ... deprecated parameter
# #' @export
# plotSA <- function(...){
#   defunct_message()
#   .Defunct("plot_sensitivity")
# }
#
# #' plotSARanks
# #'
# #' @param ... deprecated parameter
# #' @export
# plotSARanks <- function(...){
#   defunct_message()
#   .Defunct("plot_uncertainty")
# }
#
# #' plotframework
# #'
# #' @param ... deprecated parameter
# #' @export
# plotframework <- function(...){
#   defunct_message()
#   .Defunct("plot_framework")
# }
#
# #' rankDF
# #'
# #' @param ... deprecated parameter
# #' @export
# rankDF <- function(...){
#   defunct_message()
#   .Defunct("rank_df")
# }
#
# # assemble
# #
# # @param ... deprecated parameter
# # @export
# regen <- function(...){
#   defunct_message()
#   .Defunct("Regen")
# }
#
# #' removeElements
# #'
# #' @param ... deprecated parameter
# #' @export
# removeElements <- function(...){
#   defunct_message()
#   .Defunct("remove_elements")
# }
#
# #' replaceDF
# #'
# #' @param ... deprecated parameter
# #' @export
# replaceDF <- function(...){
#   defunct_message()
#   .Defunct("replace_df")
# }
#
# #' resultsDash
# #'
# #' @param ... deprecated parameter
# #' @export
# resultsDash <- function(...){
#   COINrX_message()
# }
#
# #' rew8r
# #'
# #' @param ... deprecated parameter
# #' @export
# rew8r <- function(...){
#   COINrX_message()
# }
#
# #' roundDF
# #'
# #' @param ... deprecated parameter
# #' @export
# roundDF <- function(...){
#   defunct_message()
#   .Defunct("round_df")
# }
#
# #' sensitivity
# #'
# #' @param ... deprecated parameter
# #' @export
# sensitivity <- function(...){
#   defunct_message()
#   .Defunct("get_sensitivity")
# }
#
# # treat
# #
# # @param ... deprecated parameter
# # @export
# treat <- function(...){
#   defunct_message()
#   .Defunct("Treat")
# }
#
# #' weightOpt
# #'
# #' @param ... deprecated parameter
# #' @export
# weightOpt <- function(...){
#   defunct_message()
#   .Defunct("get_opt_weights")
# }
