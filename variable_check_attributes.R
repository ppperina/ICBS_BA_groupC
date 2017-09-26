library("dplyr")
load("data.Rdata")

# select relevant variables
attributes_data <- select(data, property_type, beds, bed_type, bathrooms, bedrooms, room_type, amenities, square_feet, accommodates, price)

glimpse(attributes_data)
str(attributes_data)

# check for missing values
summary(data)
# property_type - 6 factor values, 267 have a value "Other"
# beds have 173 NAs
# bed_type - 5 factor values, no "other"
# bathrooms - 260 NAs
# bedrooms - 93 NAs
# room_type - 3 factor values, no NAs
# amenities - list of amenities, some have translation missing, 685 have an empty list, 
# square_feet - only 582 out of 53904 observations include square_feet info
# accommodates - 1 to 16, no NAs

# check how they correlate with price, especially room type (flat/room), accommodates
# limit only to apartments in property_type?

# turn prices into integers first
data$price <- as.numeric(sub('\\$','',as.character(data$price)))
attributes_data$price <- as.numeric(sub('\\$','',as.character(attributes_data$price)))

# now explore scatter/boxplots
ggplot(data = data) + geom_boxplot(aes(x = room_type, y = price))

# cut accommodate into intervals for plotting
data$acc_cut <- cut(data$accommodates, c(0,1,2,3,4,6,8,10,16))
ggplot(data = data) + geom_boxplot(aes(x = acc_cut, y = price))
# as expected price goes up with more people accommodated

# now let's look at price per person
data$price_pp <- data$price / data$guests_included

ggplot(data = data) + geom_boxplot(aes(x = acc_cut, y = price_pp))
# this is interesting - price per person increases up until 8 guests staying

# obviously accommodates and guests_included are different, compare the above with this
# accommodates seems to be higher - is it max capacity and guests is the actual number of guests?
data$price_pp <- data$price / data$accommodates

ggplot(data = data) + geom_boxplot(aes(x = acc_cut, y = price_pp)) + ylim(0, 200)

# explore property_type
unique(data$property_type)
summary(data$property_type)
# 27 factor values, includes Igloo & Cave :) 267 are "Other"

# explore amenities in more detail
summary(data$amenities)
# each row has a list of amenities, not sure how to simply work with that
# I'm guessing we can check if amenity is in the list and that should work
# we could also "unroll" them into additional rows but that's not practical
# https://stackoverflow.com/questions/19693328/r-how-to-expand-a-row-containing-a-list-to-several-rows-one-for-each-list-m
# dataset would have ~5-10 times more rows

head(data$amenities)
"Cable TV" %in% data$amenities[0]
"Kitchen" %in% data$amenities[0]
data$amenities[0]
# using %in% doesn't work properly - doesn't search through the list

# Visualization
library(ggplot2)

# Look at top property types
property_type_viz <- group_by(attributes_data, property_type) 
property_type_viz %>% summarise(freq = n()) %>% mutate(rank = rank(freq)) %>% 
  filter(rank > 20) %>% arrange(-rank) -> b

ggplot(b, aes(x = property_type, y = freq)) + geom_bar(stat = "identity")

reorder(b$property_type, b$property_type, length)

ggplot(attributes_data, aes(x = attributes_data$property_type)) + geom_bar()
unique(attributes_data$property_type)

# run a regression with our variables, note R^2, see which ones matter, make plots
data$property_type <- factor(data$property_type)
data$bed_type <- factor(data$bed_type)

data$amenities_n <- NA

# for (i in 1:data$amenities){
#   data$amenities_n[i] <- sapply(gregexpr(",", data$amenities[i]), length) + 1
# }
# leave amenities out 

# regression with 0.52 adjusted R^2
fit1 <- lm(data$price ~ data$property_type + data$beds + data$bathrooms + data$bedrooms + data$room_type + data$accommodates)
summary(fit1)
# interestingly bed_type doesn't seem to matter, most property types don't matter either -> limit to those that do

# look at data only for apartments as those are the most frequent property_type
apt_data <- filter(attributes_data, property_type == "Apartment")
str(apt_data)
# re-run the regression, adj R^2 = 0.47, beds are not significant
fit2 <- lm(apt_data$price ~ apt_data$bathrooms + apt_data$beds + apt_data$bedrooms + apt_data$room_type + apt_data$accommodates)
summary(fit2)

# remove beds, adj R^2 = 0.47 and all variables are significant
fit3 <- lm(apt_data$price ~ apt_data$bathrooms + apt_data$bedrooms + apt_data$room_type + apt_data$accommodates)
summary(fit3)

library(usdm)
corr_df <- data.frame(apt_data$bathrooms, apt_data$bedrooms, apt_data$accommodates)
vif(corr_df)

# why both private and shared room have a negative impact on price?
# perhaps indicating it is just a room and not an apartment? YES
summary(apt_data$room_type)

# perhaps we should focus on private rooms (n = 15.4k) or entire home/apt (n=22.5k)
# data is a bit confusing - in property_type apartment includes rooms, and in room_type it can be entire home/apt
ggplot(data = apt_data) + geom_boxplot(aes(x = room_type, y = price))

# private room data
pvt_r_data <- filter(apt_data, room_type == "Private room")
pvt_r_data$beds <- cut(pvt_r_data$beds, c(1,2,3,5,7,9,12))
ggplot(data = pvt_r_data) + geom_boxplot(aes(x = bed_type, y = price))

home_data <- filter(apt_data, room_type == "Entire home/apt")
home_data$beds <- cut(home_data$beds, c(1,2,3,5,7,9,12))
ggplot(data = home_data) + geom_boxplot(aes(x = beds, y = price))

# the main variables we'd like to use seem to be bathrooms, bedrooms, private room, shared room, accommodates
# let's see their correlation matrix, remove room type because it's non-numeric
corr_mat_data <- data.frame(apt_data$price, apt_data$bathrooms, apt_data$bedrooms, apt_data$accommodates)
corr_mat <- corr_mat_data[, 2:length(corr_mat_data)]
round(cor(corr_mat_data, use = "pairwise.complete.obs"), 2)
# corr of accommodates and bedrooms 0.69, price and bedrooms 0.51, price & accommodates 0.59

# Checking correlations
apt_data$bedrooms_cut <- cut(apt_data$bedrooms, c(1,2,3,4,5,6))
ggplot(data = apt_data) + geom_boxplot(aes(x = apt_data$bedrooms_cut, y = apt_data$accommodates))

ggplot(data = apt_data) + geom_boxplot(aes(x = apt_data$bedrooms_cut, y = apt_data$price))

apt_data$accommodates_cut <- cut(apt_data$accommodates, c(1,2,3,4,6,8,10,12,16))
ggplot(data = apt_data) + geom_boxplot(aes(x = apt_data$accommodates_cut, y = apt_data$price))

# Stargazer is a great package for well formatted regression and summary statistics tables

