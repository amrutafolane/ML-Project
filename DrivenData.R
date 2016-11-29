library(data.table) 
library(plyr) 
library(dplyr)
library(ggplot2) 
library(vcd) 
library(dplyr) 
library(stringr) 
library(lubridate) 
library(reshape2) 
library(h2o) 

reduce.num.levels = function(x, nlevels = 12) {
  levels = table(x)
  if ( n_distinct(x) > (nlevels+1) )  {
    small.levels = names(sort(levels, decreasing = TRUE)[ - seq(nlevels)])
    x[x %in% small.levels] = "other"
  }
  return (x)
}
reduce.size.levels = function(x, min.size = 500) {
  levels = table(x)
  if ( min(levels) < min.size) {
    small.levels = names(levels[levels < min.size])
    x[x %in% small.levels] = "other"
  }
  return (x)
}
myreduce.levels = function(x) {
  return (reduce.num.levels(reduce.size.levels(x)))
}

na.strings = c(NA,"","unknown","Unknown")
colClasses = c("integer","numeric","Date","character","numeric","character",
               "numeric","numeric","character","integer","character","character",
               "character","character","character","character","character","numeric",
               "logical","character","character","character","logical","integer",
               "character","character","character","character","character","character",
               "character","character","character","character","character","character",
               "character","character","character","character")

train.label<-read.csv("C:\\MLproject\\testsetlables.csv")
train.values<-read.csv("C:\\MLproject\\trainingsetvalues.csv")
test.values<-read.csv("C:\\MLproject\\testsetvalues.csv")
train.values<-train.values[sample(nrow(train.values)),]
trainIndex <- sample(1:nrow(train.values), 0.1 * nrow(train.values));
train <- train[trainIndex, ];

test <- train[-trainIndex, ];

#train.values$subset = "train"
#test.values$subset = "test"
train = inner_join(train.label, train, by = "id")
data = tbl_df(rbind.fill(train,test))

data = data %>%
  mutate(funder = ifelse(funder == 0, NA, funder)) %>%
  mutate(installer = ifelse(installer == 0, NA, installer)) %>%
  mutate(gps_height = ifelse(gps_height == 0, NA, gps_height)) %>%
  mutate(population = ifelse(population == 0, NA, population)) %>%
  mutate(amount_tsh = ifelse(amount_tsh == 0, NA, amount_tsh)) %>%
  mutate(construction_year = ifelse(construction_year == 0, NA, construction_year))

data = data %>%
  mutate(latitude = ifelse(latitude > -1e-06, NA, latitude)) %>%
  mutate(longitude = ifelse(longitude < 1e-06, NA, longitude))

chr.cols = data %>% summarise_each(funs(is.character(.))) %>%
  unlist() %>% which() %>% names()
data = data %>% mutate_each( funs(tolower), one_of(chr.cols))

data = data %>% select( - recorded_by )

data %>%
  group_by(extraction_type_class, extraction_type_group, extraction_type) %>% tally()

data = data %>%
  mutate(extraction_type = revalue(extraction_type,
                                   c("cemo" = "other motorpump",
                                     "climax" = "other motorpump",
                                     "other - mkulima/shinyanga" = "other handpump",
                                     "other - play pump" = "other handpump",
                                     "walimi" = "other handpump",
                                     "other - swn 81" = "swn",
                                     "swn 80" = "swn",
                                     "india mark ii" = "india mark",
                                     "india mark iii" = "india mark"))) %>%
  select( - extraction_type_group ) 

data %>% group_by(management_group, management) %>% tally()
data %>% group_by(scheme_management, scheme_name) %>% tally()

data = data %>% select( - scheme_name)

data %>% group_by(payment_type, payment) %>% tally()
data = data %>% select( - payment )

data %>% group_by(quality_group, water_quality) %>% tally()
data = data %>% select( - quality_group)

data %>% group_by(quantity_group, quantity) %>% tally()
data = data %>% select( - quantity_group)

data %>% group_by(source_class, source_type, source) %>% tally()
data = data %>%
  mutate(source = revalue(source,c("other" = NA))) %>% select( - source_type)

data %>% group_by(waterpoint_type_group, waterpoint_type) %>% tally()
data = data %>% select( - waterpoint_type_group)

data %>% group_by(region, region_code, district_code) %>% tally()

data = data %>% 
  group_by(region,district_code) %>%
  mutate(district.long = mean(longitude, na.rm = TRUE)) %>%
  mutate(district.lat = mean(latitude, na.rm = TRUE)) %>%
  ungroup()
## Compute averages in regions (just in case the above is also NA)
data = data %>%
  group_by(region) %>%
  mutate(region.long = mean(longitude, na.rm = TRUE)) %>%
  mutate(region.lat = mean(latitude, na.rm = TRUE)) %>%
  ungroup()
## "Impute" missing longitude/latitude values
data = data %>%
  mutate(longitude = ifelse(!is.na(longitude), longitude,
                            ifelse(!is.na(district.long), district.long, region.long))) %>%
  mutate(latitude = ifelse(!is.na(latitude), latitude,
                           ifelse(!is.na(district.lat), district.lat, region.lat)))


data = data %>% select( - region_code, - district_code,
                        - region.long, - region.lat,
                        - district.long, - district.lat,
                        - ward , - subvillage)

data = data %>% mutate(lga = ifelse( grepl(" rural", lga), "rural",
                                     ifelse( grepl(" urban", lga), "urban","other")))

data = data %>% mutate(date_recorded = ymd(date_recorded)) %>%
  mutate(operation_years = lubridate::year(date_recorded) - construction_year) %>%
  mutate(operation_years = ifelse(operation_years < 0, NA, operation_years))

data = data %>%
  mutate(day_of_year = yday(date_recorded)) %>%
  mutate(month_recorded = lubridate::month(date_recorded)) %>%
  mutate(season = ifelse( month_recorded <= 2, "dry short",
                          ifelse( month_recorded <= 5, "wet long",
                                  ifelse(month_recorded <= 9, "dry long", "wet short")))) %>%
  select( - date_recorded, - month_recorded, - construction_year)

cbind(
  data %>% group_by(funder) %>% tally() %>% arrange(desc(n)) %>% slice(1:10),
  data %>% group_by(installer) %>% tally() %>% arrange(desc(n)) %>% slice(1:10),
  data %>% group_by(wpt_name) %>% tally() %>% arrange(desc(n)) %>% slice(1:10)
)

data = data %>% select( - wpt_name) %>%
  mutate(funder = myreduce.levels(funder)) %>%
  mutate(installer = myreduce.levels(installer))

data = data %>% select( - id , - num_private ) %>%
  filter(scheme_management != "none" | is.na(scheme_management))

mean.na = function(x) { mean(is.na(x)) }
t(data %>% summarise_each(funs(mean.na)))

data = data %>% select( - amount_tsh)

localH2O = h2o.init()
predictors = c("funder","installer","management",
               "region","lga","population",
               "latitude","longitude","gps_height",
               "scheme_management",
               "public_meeting","permit",
               "water_quality","quantity",
               "payment_type","source","source_class",
               "management","management_group",
               "basin","extraction_type","waterpoint_type")
target = "status_group.y"

trainHex = as.h2o(train, destination_frame = "train.hex")
testHex = as.h2o(test, destination_frame = "test.hex")

rfHex = h2o.randomForest(
  x = predictors,
  y = target,
  training_frame = trainHex,
  model_id = "rf_ntrees1000",
  ntrees = 1000, mtries = 10,
  seed = 123456) ## Set the seed for reproducibility of results

h2o.confusionMatrix(rfHex)

predictions = as.data.frame(h2o.predict(rfHex,testHex))[,1]




