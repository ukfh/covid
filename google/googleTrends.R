library(tidyverse)
library(gtrendsR)

res <- gtrends("nhl", geo = c("CA", "US"))
View(res$interest_over_time)

storageTrend <- gtrends("storage", geo = c('GB'), time = "today 12-m")
View(storageTrend$interest_over_time)

thisTrend <- as.data.frame(storageTrend$interest_over_time)

cat <- data("categories")
