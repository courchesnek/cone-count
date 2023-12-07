library(data.table)
library(ggplot2)
library(dplyr)
library(lubridate)

#data.table syntax
## DT[with i, do j, by group]

dat <- fread("input/MiddenConeCounts.csv")



# basic cleaning and investigating ----------------------------------------

#get rid of totals columns
dat[, grep("total", names(dat)) := NULL ]

#GRAB ALL COLUMNS WITH OLD AND NEW
conecols <- c(grep("old", names(dat), value = TRUE), grep("new", names(dat), value = TRUE))

#classify all cone cols to be numeric
dat <- dat %>% as.data.table(mutate_at(conecols, as.numeric))

#examples of how to explore data
dat[gr == "BT", length(unique(squirrel_id)), Year]
dat[gr == "BT", .N, Year]

#remove weird NAs from grid column
dat <- dat[!gr == ""]

#create a midden ID by pasting grid and ref location
dat[, midden := paste0(gr, "_", reflo)]

#classify year as numeric
dat[, Year := as.numeric(Year)]



# create new data frame ---------------------------------------------------

#melt down all cone count columns by the quadrat. Value is the cone count
meltdat <- melt(dat, measure.vars = conecols, value.name = "count", variable.name = "quadrat")

#subset the melted data to just columns of interest
dt <- meltdat[, .(Year, midden, squirrel_id, quadrat, count)]

#make age of cones its own column by splitting from quadrat column
dt[, age := tstrsplit(quadrat, "_", keep = 2)]

#remove age from quadrat name
dt[, quadrat := gsub(c("_old"), c(""), quadrat)]
dt[, quadrat := gsub(c("_new"), c(""), quadrat)]

#set order of subset data to by by midden then year
dt <- setorder(dt, midden, Year)


#create list of midden things
vars <- c("Year", "midden", "squirrel_id")

dt[, Year := as.numeric(Year)]

dt[, count := as.numeric(count)]

#remove NAs from dt counts
dt <- dt[!is.na(count)]
#make sure there are no remaining NAs
na_rows <- dt[is.na(count)]

# sample ------------------------------------------------------------------

#sum all cone count data by year, midden and squirrel id
#this combines old and new cones. If you want them separated at 'age' to the by group
all_8 <- dt[, sum(count), by = vars]
setnames(all_8, "V1", "count_8")


# 
# testmid <- sample(dt$midden, 1)
# test <- dt[midden %in% testmid]
# 
# 
# 
# quads <- test[, unique(quadrat)]
# 
# samplequads <- sample(quads, 4, replace = FALSE)
# 
# test2 <- test[quadrat %in% samplequads]
# 
# test2[, sum(count)]
# 
# test[, sum(count)]


#function that randomly samples 4 quadrats
randomcount <- function(cd, samplecol, numb){
  
  #pull all unique quadrat names from quadrat col
  quads <- cd[, unique(samplecol)]
  
  #sample 4 of these quadrat names randomly
  samplenumb <- sample(quads, numb, replace = FALSE)
  
  #subset the data to only include these four quadrats
  subset <- cd[samplecol %in% samplenumb]
  
  #return the sum of this data subset. If you want it partitioned by age, do a by = age
  return(subset[, sum(count)])
  
}

#run the function by year, midden, and squirrel id.
all_4 <- dt[, randomcount(cd = .SD, samplecol = quadrat, numb = 4), by = vars]
setnames(all_4, "V1", "other_count")
all_4[, sample := 4]

all_5 <- dt[, randomcount(cd = .SD, samplecol = quadrat, numb = 5), by = vars]
setnames(all_5, "V1", "other_count")
all_5[, sample := 5]

all_6 <- dt[, randomcount(cd = .SD, samplecol = quadrat, numb = 6), by = vars]
setnames(all_6, "V1", "other_count")
all_6[, sample := 6]

all_7 <- dt[, randomcount(cd = .SD, samplecol = quadrat, numb = 7), by = vars]
setnames(all_7, "V1", "other_count")
all_7[, sample := 7]

all_samples <- rbind(all_4, all_5, all_6, all_7)


#to merge into a wide table
countcompare <- merge(all_samples, all_8, by = vars, all.x = TRUE)
countcompare[, sample := as.factor(sample)]


#correlation coefficient run by sample size, added r^2 and sample size
correlations <- countcompare[, .(correlation = cor(count_8, other_count, use = "complete.obs"),
                                 r_squared = cor(count_8, other_count, use = "complete.obs")^2,
                                 n = sum(!is.na(count_8))),
                             by = sample]

#calculate standard errors
correlations[, se_correlation := sqrt((1 - correlation^2) / (n - 2))]


#using color to differentiate
(conefig <- 
  ggplot(countcompare)+
  geom_abline(intercept = 0, slope = 1, linetype = 2, linewidth = 2, color = "grey40")+
  geom_point(aes(x = count_8, y = other_count, color = sample), size = 3, alpha = .3)+
  geom_smooth(aes(x = count_8, y = other_count, color = sample), fill = NA, method = "lm")+
  labs(x = "Cone count with 8 quadrats", y = "Other cone count")+
  theme_minimal())

#use facet wrap to differentiate
ggplot(countcompare)+
  geom_abline(intercept = 0, slope = 1, linetype = 2, linewidth = 2, color = "grey40")+
  geom_point(aes(x = count_8, y = other_count), color = "blue", size = 3, alpha = .3)+
  geom_smooth(aes(x = count_8, y = other_count), fill = NA, method = "lm")+ 
  labs(x = "Cone count with 8 quadrats", y = "Other cone count")+
  facet_wrap(~sample, scales = "fixed")+
  theme_minimal()






# save things -------------------------------------------------------------

#file name, figure name, size, units
ggsave("output/samplesize.jpeg", conefig, width = 6, height = 4, unit = "in")



