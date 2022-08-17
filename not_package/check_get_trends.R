# testing the get_trends function. This is done here informally because my current example purse doesn't
# have a realistic missing data pattern. So I am using the bioeconomy indicators data set. I will do some
# manual cross-checks.

# first, read in bio purse from debug
bio <- readRDS("C:/Users/becke/OneDrive/Work/Consultancy/COINr/debug/bio.RDS")

# get trends on raw dset. We use a subset of units
t1 <- get_trends(bio, dset = "Raw", uCodes = c("AT", "BG"), use_latest = 5,
                 f_trend = "prc_change", interp_at = "all")

# extract the data set from the purse for cross checking
iData <- get_dset(bio, dset = "Raw")
iCodes <- names(iData)[names(iData) %nin% c("uCode", "Time")]

# The output is a df in long form. First check expected uCodes and iCodes
# uCodes
stopifnot(
  setequal(unique(t1$uCode), c("AT", "BG"))
)

# iCodes - expect all to be there
stopifnot(
  setequal(unique(t1$iCode), iCodes)
)


# TIME SERIES CHECK 1 -----------------------------------------------------

# the objective here is to see if we get the same value for a given indicator-country pair
# compared to when calculated manually. This is more a check on (a) the sorting of data and
# (b) the selection of the last N data points

# OK. Now we have to manually check some time series
# let's get a time series: for AT for indicator in col 3
icode <- names(iData)[3]

# this is the data for AT for this indicator
# the indicator vals
yt <- iData[[icode]][iData$uCode == "AT"]
# the corresponding time code
tt <- iData[["Time"]][iData$uCode == "AT"]

# since this has no NA values between other data points it cannot be interpolated.
# Expect that if I take the last 5 points I should get equiv answer to get_trends()

i_first <- length(yt) - 4
i_last <- length(yt)

prc <- prc_change(yt[i_first:i_last], tt[i_first:i_last])

# check
stopifnot(
  prc == t1$prc_change[(t1$uCode == "AT") & (t1$iCode == icode)]
)

# also check data avail stats. Series is complete from 2001-2021
stopifnot(
  t1$Avail[(t1$uCode == "AT") & (t1$iCode == icode)] == mean(!is.na(yt)),
  t1$t_first[(t1$uCode == "AT") & (t1$iCode == icode)] == 2001,
  t1$t_latest[(t1$uCode == "AT") & (t1$iCode == icode)] == 2021,
  t1$Avail_use_latest[(t1$uCode == "AT") & (t1$iCode == icode)] == mean(!is.na(yt[i_first:i_last])),
  t1$t_first_use_latest[(t1$uCode == "AT") & (t1$iCode == icode)] == 2017
)


# TIME SERIES CHECK 2 -----------------------------------------------------

# We do the same again but with a different series

icode <- names(iData)[4]
yt <- iData[[icode]][iData$uCode == "BG"]
tt <- iData[["Time"]][iData$uCode == "BG"]

# This series has no interpolation. It has an uninterrupted time series between 2000-2017

i_first <- which(tt == 2013)
i_last <- which(tt == 2017)

prc <- prc_change(yt[i_first:i_last], tt[i_first:i_last])

# check
stopifnot(
  prc == t1$prc_change[(t1$uCode == "BG") & (t1$iCode == icode)]
)

stopifnot(
  t1$Avail[(t1$uCode == "BG") & (t1$iCode == icode)] == mean(!is.na(yt)),
  t1$t_first[(t1$uCode == "BG") & (t1$iCode == icode)] == 2000,
  t1$t_latest[(t1$uCode == "BG") & (t1$iCode == icode)] == 2017,
  t1$Avail_use_latest[(t1$uCode == "BG") & (t1$iCode == icode)] == mean(!is.na(yt[i_first:i_last])),
  t1$t_first_use_latest[(t1$uCode == "BG") & (t1$iCode == icode)] == 2013
)

# TIME SERIES CHECK 3
# this check will use a series that uses interpolation

icode <- names(iData)[13]
yt <- iData[[icode]][iData$uCode == "BG"]
tt <- iData[["Time"]][iData$uCode == "BG"]

# This series has several gaps, with data between 2005-2016. We interpolate using approx()
l_out <- approx(tt, yt, xout = tt)

i_first <- which(tt == 2012)
i_last <- which(tt == 2016)

prc <- prc_change(l_out$y[i_first:i_last], l_out$x[i_first:i_last])

# check
stopifnot(
  prc == t1$prc_change[(t1$uCode == "BG") & (t1$iCode == icode)]
)

stopifnot(
  t1$Avail[(t1$uCode == "BG") & (t1$iCode == icode)] == mean(!is.na(yt)),
  t1$t_first[(t1$uCode == "BG") & (t1$iCode == icode)] == 2005,
  t1$t_latest[(t1$uCode == "BG") & (t1$iCode == icode)] == 2016,
  t1$Avail_use_latest[(t1$uCode == "BG") & (t1$iCode == icode)] == mean(!is.na(yt[i_first:i_last])),
  t1$t_first_use_latest[(t1$uCode == "BG") & (t1$iCode == icode)] == 2012
)


# IMPUTATION CHECK --------------------------------------------------------
# Here we check that imputation works as expected.

# get a df with some missing data (remove redundant uCode col)
df1 <- iData[iData$uCode == "RO", ]
df1 <- df1[names(df1) != "uCode"]

# interpolate at all time points
l_int <- approx_df(df1[names(df1) %nin% c("Time")],
                   tt = df1$Time, tt_est = unique(df1$Time))

# the interpolated df
dfi <- cbind(Time = l_int$tt, l_int$Y)

# row names will have changed - we don't care but need to match for testing
row.names(dfi) <- NULL
row.names(df1) <- NULL

# now we will check specific cols
# First, function should not extrapolate outside of time range. Hence, some cols should be equal.
# the following are some cols where no interpolation should occur.
stopifnot(
  identical(df1[2:11], dfi[2:11])
)

# Now check some columns where interpolation does occur

# col having incomplete data between 2005-2016
icol = 12
dfc <- cbind(df1[icol], dfi[icol])

# checks
stopifnot(
  # non-NA values are the same
  all(dfc[1][!is.na(dfc[1])] == dfc[2][!is.na(dfc[1])]),
  # values outside of known time range are still NA
  all(is.na(dfc[[1]][df1$Time < 2005])),
  all(is.na(dfc[[2]][df1$Time < 2005])),
  all(is.na(dfc[[1]][df1$Time > 2016])),
  all(is.na(dfc[[2]][df1$Time > 2016])),
  # values within known time range are complete for interpolated
  all(!is.na(dfc[[2]][df1$Time %in% 2005:2016]))
)

# same for another column
# incomplete data from 2012-2018
icol = 33
dfc <- cbind(df1[icol], dfi[icol])

# checks
stopifnot(
  # non-NA values are the same
  all(dfc[1][!is.na(dfc[1])] == dfc[2][!is.na(dfc[1])]),
  # values outside of known time range are still NA
  all(is.na(dfc[[1]][df1$Time < 2012])),
  all(is.na(dfc[[2]][df1$Time < 2012])),
  all(is.na(dfc[[1]][df1$Time > 2018])),
  all(is.na(dfc[[2]][df1$Time > 2018])),
  # values within known time range are complete for interpolated
  all(!is.na(dfc[[2]][df1$Time %in% 2012:2018]))
)

# PRC CHANGE CHECK
# final check is the percentage change function itself
# We can just do this with some random data

x <- 2011:2020
y <- runif(10)

prc <- prc_change(y, x)

# when we have more than two obs, the function should perform a linear regression
# then the prc change is the prc change of the last fitted value compared to the first *fitted* value
lm1 <- lm(y ~ x)

# fitted vals
yfit <- lm1$fitted.values

# prc change
# note: this is *100 to get to %, then /9 because number of years and we want per year
prc1 <- as.numeric((yfit[length(yfit)] - yfit[1])/yfit[1]*100/9)

stopifnot(
  all.equal(prc, prc1)
)
