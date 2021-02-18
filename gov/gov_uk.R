# script to download all data we need for our plot.
# the calls are gleaned from the plots on the government websites
# https://coronavirus.data.gov.uk/

# hospital cases ----
endpoint <- 'https://api.coronavirus.data.gov.uk/v1/data?filters=areaName=United%2520Kingdom;areaType=overview&structure={"date":"date","hospitalCases":"hospitalCases"}'

response <- GET(url = endpoint)

# Convert response from binary to JSON:
json_text <- content(response, "text")
data      <- fromJSON(json_text)
# data$status


hospitalCases <- data.frame(dt = as.Date(data$data$date), cases = data$data$hospitalCases)

# new cases ----

endpoint <- 'https://api.coronavirus.data.gov.uk/v1/data?filters=areaName=United%2520Kingdom;areaType=overview&structure={"date":"date","newCases":"newCasesByPublishDate"}'

response <- GET(url = endpoint)

# Convert response from binary to JSON:
json_text <- content(response, "text")
data      <- fromJSON(json_text)
data$status



new_cases <- data.frame(dt = as.Date(data$data$date), cases = data$data$newCases)


# tests ----
endpoint <- "https://api.coronavirus.data.gov.uk/v1/data?filters=areaName=United%2520Kingdom;areaType=overview&structure=%7B%22date%22:%22date%22,%22newPillarOneTwoTestsByPublishDate%22:%22newPillarOneTwoTestsByPublishDate%22,%22capacityPillarOneTwo%22:%22capacityPillarOneTwo%22%7D"

response <- GET(url = endpoint)

# Convert response from binary to JSON:
json_text <- content(response, "text")
data      <- fromJSON(json_text)
data$status

tests <-  data.frame(dt = as.Date(data$data$date), test_proc = data$data$newPillarOneTwoTestsByPublishDate, test_cap = data$data$capacityPillarOneTwo)

# more tests ----

endpoint <- "https://api.coronavirus.data.gov.uk/v1/data?filters=areaName=United%2520Kingdom;areaType=overview&structure=%7B%22date%22:%22date%22,%22newPillarOneTestsByPublishDate%22:%22newPillarOneTestsByPublishDate%22,%22newPillarTwoTestsByPublishDate%22:%22newPillarTwoTestsByPublishDate%22,%22newPillarThreeTestsByPublishDate%22:%22newPillarThreeTestsByPublishDate%22,%22newPillarFourTestsByPublishDate%22:%22newPillarFourTestsByPublishDate%22%7D"


response <- GET(url = endpoint)

# Convert response from binary to JSON:
json_text <- content(response, "text")
data      <- fromJSON(json_text)
data$status

tests2 <-  data.frame(dt = as.Date(data$data$date), test_p1 = data$data$newPillarOneTestsByPublishDate, test_p2 = data$data$newPillarTwoTestsByPublishDate, test_p3 = data$data$newPillarThreeTestsByPublishDate, test_p4 = data$data$newPillarFourTestsByPublishDate)

# patients in hospital ----

endpoint <- "https://api.coronavirus.data.gov.uk/v1/data?filters=areaType=nation&structure=%7B%22date%22:%22date%22,%22areaName%22:%22areaName%22,%22covidOccupiedMVBeds%22:%22covidOccupiedMVBeds%22%7D"

response <- GET(url = endpoint)

# Convert response from binary to JSON:
json_text <- content(response, "text")
data      <- fromJSON(json_text)
data$status

patients_in_hospital <- data.frame(dt = as.Date(data$data$date), country = data$data$areaName, beds = data$data$covidOccupiedMVBeds)


# deaths ----
endpoint <- "https://api.coronavirus.data.gov.uk/v1/data?filters=areaType=nation&structure=%7B%22date%22:%22date%22,%22areaName%22:%22areaName%22,%22newDeaths28DaysByPublishDate%22:%22newDeaths28DaysByPublishDate%22%7D"

response <- GET(url = endpoint)

# Convert response from binary to JSON:
json_text <- content(response, "text")
data      <- fromJSON(json_text)
data$status

deaths <- data.frame(dt = as.Date(data$data$date), country = data$data$areaName, deaths = data$data$newDeaths28DaysByPublishDate)
