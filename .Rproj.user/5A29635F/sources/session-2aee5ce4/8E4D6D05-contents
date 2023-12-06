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
dat <- dat %>% mutate_at(conecols, as.numeric)

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



# sample ------------------------------------------------------------------

#sum all cone count data by year, midden and squirrel id
#this combines old and new cones. If you want them separated at 'age' to the by group
all_8 <- dt[, sum(count), by = .(Year, midden, squirrel_id)] 
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
randomcount4 <- function(cd, samplecol){
  #pull all unique quadrat names from quadrat col
  quads <- cd[, unique(samplecol)]
  #sample 4 of these quadrat names randomly
  samplequads <- sample(quads, 4, replace = FALSE)
  #subset the data to only include these four quadrats
  subset <- cd[samplecol %in% samplequads]
  #return the sum of this data subset. If you want it partitioned by age, do a by = age
  return(subset[, sum(count)])
  
}

#run the function by year, midden, and squirrel id.
all_4 <- dt[, randomcount4(cd = .SD, samplecol = quadrat), by = .(Year, midden, squirrel_id)]
setnames(all_4, "V1", "count_4")


#if you want to merge into a long table use rbind

#to merge into a wide table
countcompare <- merge(all_8, all_4, by = c("Year", "midden", "squirrel_id"))

#ggplot 4 count against 8 count
ggplot(countcompare)+
  geom_point(aes(x = count_8, y = count_4), color = "blue3")+
  theme_minimal()



cor(countcompare$count_8, countcompare$count_4)


