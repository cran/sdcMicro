## ----setup, include=FALSE-----------------------------------------------------
library(data.table)
library(sdcMicro)

## -----------------------------------------------------------------------------
library(sdcMicro)
dat <- createDat(N=100000)
dat

## -----------------------------------------------------------------------------
hierarchy <- c("nuts1","nuts2")
risk_variables <- c("hincome","ageGroup","gender")
k_anonymity <- 3
swaprate <- .05
hid <- "hid"
similar <- "hsize"

dat_swapped <- recordSwap(data = dat, hid = hid,
                          hierarchy = hierarchy,
                          similar = similar,
                          risk_variables = risk_variables,
                          k_anonymity = k_anonymity,
                          swaprate = swaprate)

dat_swapped

## -----------------------------------------------------------------------------
k_anonymity <- 0
swaprate <- .05
dat_swapped <- recordSwap(data = dat, hid = hid,
                          hierarchy = hierarchy,
                          similar = similar,
                          risk_variables = risk_variables,
                          k_anonymity = k_anonymity,
                          swaprate = swaprate)
dat_swapped

## -----------------------------------------------------------------------------
dat_compare <- merge(dat[,.(paste(nuts1[1],nuts2[1])),by=hid],
                     dat_swapped[,.(paste(nuts1[1],nuts2[1])),by=hid],by="hid")

# number of swapped households
nrow(dat_compare[V1.x!=V1.y])

## -----------------------------------------------------------------------------
# swaprate times number of households in data
dat[,uniqueN(hid)]*swaprate

## -----------------------------------------------------------------------------
hierarchy <- c(1,2) # ~ c("nuts1","nuts2")
risk_variables <- c(11,7,8) # ~ c("hincome","ageGroup","gender")
hid <- 5 # ~ "hid"
similar <- 6 # ~ "hsize"

dat_swapped <- recordSwap(data = dat, hid = hid,
                          hierarchy = hierarchy,
                          similar = similar,
                          risk_variables = risk_variables,
                          k_anonymity = k_anonymity,
                          swaprate = swaprate)

## -----------------------------------------------------------------------------
# demonstrate on small data set
dat <- createDat(N=10000)
hierarchy <- c("nuts1","nuts2")
risk_variables <- "gender"
# similarity profile contains:
# nuts1 + hsize + htype + hincome
similar <- c("nuts1","hsize","htype","hincome")

# procedure will not always find a suitable donor
dat_swapped <- recordSwap(data = dat, hid = hid,
                          hierarchy = hierarchy,
                          similar = similar,
                          risk_variables = risk_variables,
                          k_anonymity = 3,
                          swaprate = 0.05,
                          seed = 123456L)

## -----------------------------------------------------------------------------
dat_compare <- merge(dat[,.(paste(nuts1[1],nuts2[1])),by=hid],
                     dat_swapped[,.(paste(nuts1[1],nuts2[1])),by=hid],by="hid")

# number of swapped households
nrow(dat_compare[V1.x!=V1.y])

## -----------------------------------------------------------------------------
# additional profile contains only hsize
similar <- list(similar)
similar[[2]] <- "hsize"
similar

## -----------------------------------------------------------------------------
# procedure found donors for every record
dat_swapped <- recordSwap(data = dat, hid = hid,
                          hierarchy = hierarchy,
                          similar = similar,
                          risk_variables = risk_variables,
                          k_anonymity = 3,
                          swaprate = 0.05,
                          seed = 123456L)
dat_compare <- merge(dat[,.(paste(nuts1[1],nuts2[1])),by=hid],
                     dat_swapped[,.(paste(nuts1[1],nuts2[1])),by=hid],by="hid")

# number of swapped households
nrow(dat_compare[V1.x!=V1.y])

## -----------------------------------------------------------------------------
hid <- "hid"
hierarchy <- c("nuts1","nuts2")
similar <- c("hsize")
risk_variables <- c("hincome","htype")

dat_swapped <- recordSwap(data = copy(dat),
                        hid = hid,
                        hierarchy = hierarchy,
                        similar = similar,
                        risk_variables = risk_variables,
                        swaprate = 0.05,
                        seed=1234L)

# compare results
dat_compare <- merge(dat[,.(paste(nuts1[1],nuts2[1])),by=hid],
                     dat_swapped[,.(paste(nuts1[1],nuts2[1])),by=hid],by="hid")
head(dat_compare[V1.x!=V1.y])

## -----------------------------------------------------------------------------
dat[nuts1==1&nuts2==14,sort(unique(nuts3))]

## -----------------------------------------------------------------------------
dat_swapped[nuts1==1&nuts2==14,sort(unique(nuts3))]

## -----------------------------------------------------------------------------
dat_swapped2 <- recordSwap(data = copy(dat),
                        hid = hid,
                        hierarchy = hierarchy,
                        similar = similar,
                        risk_variables = risk_variables,
                        swaprate = 0.05,
                        carry_along = c("nuts3","lau2"), # <- swap nuts3 and lau2 variable as well
                        seed=1234L)

geoVars <- c("nuts1", "nuts2", "nuts3")
dat_geo <- dat[!duplicated(hid),..geoVars]
setorderv(dat_geo,geoVars)
dat_geo_swapped <- dat_swapped2[!duplicated(hid),..geoVars]
setorderv(dat_geo_swapped,geoVars)

# check if value combinations of swapped and original data are the same
all.equal(dat_geo,dat_geo_swapped)

## -----------------------------------------------------------------------------
dat_compare2 <- merge(dat[,.(paste(nuts1[1],nuts2[1])),by=hid],
                     dat_swapped[,.(paste(nuts1[1],nuts2[1])),by=hid],by="hid")

# check if same hid were swapped in both cases
all.equal(dat_compare2[order(hid),.(hid)],
          dat_compare[order(hid),.(hid)])

## -----------------------------------------------------------------------------
dat_swapped3 <- recordSwap(data = copy(dat),
                        hid = hid,
                        hierarchy = hierarchy,
                        similar = similar,
                        risk_variables = risk_variables,
                        swaprate = 0.05,
                        carry_along = "nuts3",
                        return_swapped_id = TRUE,
                        seed=1234L)

## -----------------------------------------------------------------------------
dat_swapped3[!duplicated(hid),.N,by=.(id_swapped = hid!=hid_swapped)]

## -----------------------------------------------------------------------------
# calculate information loss for frequecy table nuts2 x national
table_vars <- c("nuts2","national")
iloss <- infoLoss(data=dat, data_swapped = dat_swapped3,
                  table_vars = table_vars)
iloss$measures

## -----------------------------------------------------------------------------
# define squared distance as custom metric
squareD <- function(x,y){
   (x-y)^2
}

iloss <- infoLoss(data=dat, data_swapped = dat_swapped3,
                  table_vars = c("nuts2","national"),
                  custom_metric = list(squareD=squareD))
iloss$measures # includes custom loss as well

## -----------------------------------------------------------------------------
# define paramters
hierarchy <- c("nuts1","nuts2")
risk_variables <- c("hincome","ageGroup","gender")
k_anonymity <- 3
swaprate <- .05
hid <- "hid"
similar <- "hsize"

# create sdcMicro object with parameters for recordSwap()
data_sdc <- createSdcObj(dat,hhId = hid,
                         keyVars=risk_variables,
                         options = list(k_anonymity = k_anonymity,
                                                    swaprate = swaprate,
                                                    similar = similar,
                                                    hierarchy = hierarchy))

## -----------------------------------------------------------------------------
dat_swapped_sdc <- recordSwap(data = data_sdc,
                              return_swapped_id = TRUE)
dat_swapped_sdc[!duplicated(hid),.N,by=.(id_swapped = hid!=hid_swapped)]

