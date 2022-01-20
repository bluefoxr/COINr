# generate panel data

iData <- ASEM_iData
iCodes <- ASEM_iMeta$iCode[ASEM_iMeta$Type == "Indicator"]

yrs <- 2019:2022

bump_ind <- function(x, yr){
  x + runif(length(x))*(max(x, na.rm = T)-min(x, na.rm = T))*0.05*(yr-2018)
}

make_year <- function(yr){
  iData_yr <- iData
  iData_yr$Time <- yr
  iData_yr[iCodes] <- sapply(iData_yr[iCodes], bump_ind, yr)
  iData_yr
}

df_list <- lapply(yrs, make_year)
df_list <- c(list(iData), df_list)

ASEM_iData_p <- Reduce("rbind", df_list)
