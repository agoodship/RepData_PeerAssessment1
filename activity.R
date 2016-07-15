
##unzip file read in activity dataset##
        unzip("activity.zip")
        rawdata <- read.csv("activity.csv", header = TRUE)

##identify number of NA values##
  NA_values <- rawdata[(is.na(rawdata)),]
  nrow(NA_values)

##create dataset of averages by interval and replace missing values with interval means##
  average_interval <- aggregate(steps ~ interval, rawdata, mean, na.rm=TRUE)
  colnames(average_interval) = c("interval", "steps.computed")
  replaced_NA <- merge(NA_values, average_interval, by="interval", all.x = TRUE)

##create dataset of just complete observations and add a row for computed steps.  Merge into one complete dataset##
  nomissing <- rawdata[complete.cases(rawdata),]
  nomissing$steps.computed <- nomissing$steps
  activity_data <- rbind(replaced_NA, nomissing)
  activity_data$steps.computed <- round(activity_data$steps.computed, digits = 0)
