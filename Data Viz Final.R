## Prep
#load for sure gonna use em libraries
library(dplyr)
library(ggplot2)
library(patchwork)

ogReviews <- read.csv('reviews.csv',
                      header = TRUE) # Load Review data
ogNeighbourhoods <-  read.csv('neighbourhoods.csv',
                              header = TRUE) #Load Neighbourhoods data
ogListings <- read.csv('listings.csv',
                       header = TRUE) # Load the Listings data
ogSummaryListings <- read.csv('listings_summary.csv',
                               header = TRUE)
ogCalendar <- read.csv('calendar.csv',
                       header = TRUE) # Load Calendar data


#### Let's make these into relational data tables and with only the data that I care about ####


CleanedNeighbourhoods <- ogNeighbourhoods %>%
  select(
    -neighbourhood_group
  )


CleanedListings <- ogListings %>%
  select(
    id,
    name,
    neighbourhood_cleansed,
    latitude,
    longitude,
    room_type,
    accommodates,
    bathrooms,
    bedrooms,
    price,
    number_of_reviews,
    review_scores_rating,
    license,
    instant_bookable,
    host_id
  )


CleanedHosts <- ogListings %>%
  select(
    host_id,
    host_name,
    host_since,
    host_response_time,
    host_response_rate,
    host_acceptance_rate,
    host_is_superhost,
    host_listings_count
  )


CleanedCalendar <- ogCalendar %>%
  select(
    listing_id,
    date,
    price
  )


CleanedReviews <- ogReviews %>%
  select(
    listing_id,
    comments,
    id
  ) %>%
  rename(
    review_id = id
  )
## cleaning for database creation complete


library(geojsonR)

ogNeighbourhoodsJSON <- FROM_GeoJson('neighbourhoods_fixed.json') # load neighborhoodJSON file



## Step 1: explore relationships between reviews and hosts. ##
## ::::: #

# gotta get some sort of a numerical score to use for the reviews


###############################################################################################################################
library(sentimentr)

CleanedReviewsForSentimntR <- CleanedReviews %>%
  mutate(commentsForSentiment = comments %>%
  gsub('<[^>]*>', #removing htmls
       '',
       .) %>%
  gsub('https?://\\S+|www\\.\\S+', #removing urls
       '',
       .) %>%
  gsub('[^a-zA-Z0-9\\s]', #removing special characters
       ' ',
       .) %>%
  tolower()#making everything lowercase
  )


CleanedReviewsForSentimntR$commentsForSentiment <- as.character(CleanedReviews$commentsForSentiment) #making sure they are all characters

textVector <- setNames(
  CleanedReviewsForSentimntR$commentsForSentiment,
  CleanedReviewsForSentimntR$review_id
) #creating a vector that acts kinda like a dictionary in python



CleanedReviewsSentences <- get_sentences(textVector) #getting sentences from each of the reviews

ReviewsSentiment <- sentiment(CleanedReviewsSentences) #getting the review sentiment 




#NOTE#
# sentiment scores should be compared among the sample to create buckets
#NOTE#

ReviewsSentiment <- ReviewsSentiment %>%
  mutate(
    review_id = names(textVector)[element_id] #reattach the sentences and their sentiments to their original ID from ReviewsCleaned
  ) %>%
  select(
    -sentence_id,
    -word_count,
    -element_id
  )


CleanedReviews$review_id <- as.numeric(CleanedReviews$review_id)
ReviewsSentiment$review_id <- as.numeric(ReviewsSentiment$review_id)


nReviewsCleaned <- left_join(
  CleanedReviews,
  ReviewsSentiment,
  join_by(review_id),
  relationship = 'many-to-many'
)

nReviewsCleaned <- nReviewsCleaned %>%
  filter(
    !is.na(sentiment)
  )
###############################################################################################################################

CleanedHostsPropCount <- ogListings %>%
  group_by(host_id) %>%
  summarise(
    'prop_count' = n_distinct(id
    )
  ) #create distinct count column for 90 percntile calculation

percentile90Threshold <- quantile( CleanedHostsPropCount$prop_count, 
                                   0.90,
                                   na.rm = TRUE
  ) #get 90percentile threshold
print(percentile90Threshold)
#not enough tail for analysis


percentile95Threshold <- quantile( CleanedHostsPropCount$prop_count, 
                                   0.95,
                                   na.rm = TRUE
) #get top 95 percentile threshold
print(percentile95Threshold)
#still not enough tail for analysis


percentile98Threshold <- quantile( CleanedHostsPropCount$prop_count, 
                                   0.98,
                                   na.rm = TRUE
) #get top 98 percentile threshold
print(percentile98Threshold)
#maybe enough?


percentile99Threshold <- quantile( CleanedHostsPropCount$prop_count, 
                                   0.99,
                                   na.rm = TRUE
) #get top 99 percentile threshold
print(percentile99Threshold)
# lets go with the top 99% and see where that goes



Top1PercentHostByPropCount <- CleanedHostsPropCount %>%
  filter( prop_count >= percentile99Threshold
  ) #create a table of only the top 1% property count by host

listingsTop1Percent <- inner_join(
  Top1PercentHostByPropCount,
  ogListings, 
  join_by(host_id)
) # joined the table of listings to the top1percent to create a datatable that has all the listings of the top 1%

propCountCheck <- sum(Top1PercentHostByPropCount$prop_count) 
print(propCountCheck) #sum of the prop count in the top 1% hosts should equal the number of listings in the listings top 1%
#Check the join was successful

#completed the top 1% to ensure I have tables to compare 
#hosts with multiple properties and compare their rating consistency
################################################################################
























## creating a prediction model for pricing ##
#this will be used to compare all hosts pricing to predicted pricing
CleanedListings <- CleanedListings %>%
  select(-prop_count)

CleanedListingsLM <- inner_join(
  CleanedListings,
  CleanedHostsPropCount,
  join_by(host_id)
) #adding the listings count by host to the listings table



summ <- CleanedListingsLM %>%
  summarise(
    sum(is.na(price))
  ) 
print(summ)
rm(summ)#confirming that all listings have prices



install.packages("readr")
library(readr)


CleanedListings$id <- as.character(CleanedListings$id)
class(ogListings$id)
ogListings$id <- as.character(ogListings$id)
class(ogListings$id)
  
CleanedListingsLM <- CleanedListingsLM%>%
  select(
    price,
    bedrooms,
    bathrooms,
    property_type,
    neighbourhood_cleansed,
    accommodates.x
  )%>%
  rename(
    price = price.x,
    bedrooms = bedrooms.x,
    bathrooms = bathrooms.x,
    neighbourhood_cleansed = neighbourhood_cleansed.x,
    accommodates = accommodates.x
  )

CleanedListings <- CleanedListings%>%
  mutate(
    price = parse_number(
      as.character(price
      )
    )
  )



CleanedListingsForLM <- left_join(
  CleanedListingsForLM,
  
)
  #adding the property type column to the CleanedListingsForLM 



unique(CleanedListingsLM$prop_type)



priceModel <- lm(
    price ~ 
      bedrooms + 
      bathrooms + 
      prop_type + 
      neighbourhood_cleansed + 
      accommodates ,
    data = CleanedListingsLM,
    na.action = na.omit
  ) #price prediction model
summary(priceModel) #okay so there are wayyy too many property types for an accurate prediction model. I will combine property types into broader categories



propertyTypes <- unique(CleanedListingsLM$prop_type)#extract all the property types
print(propertyTypes)

PrivateRoomTypeList <- c(
  "Private room in home",
  "Private room in townhouse",
  "Private room in bed and breakfast",
  "Private room in rental unit",
  "Private room in condo",
  "Private room in guest suite",
  "Private room in villa",
  "Private room in tiny home",
  "Room in serviced apartment",
  "Private room in guesthouse",
  "Room in bed and breakfast",
  "Private room in bungalow",
  "Casa particular",
  "Private room in hostel",
  "Private room in casa particular",
  "Private room in resort",
  "Private room"
)

EntirePlaceTypeList <- c(
  "Entire rental unit",
  "Entire guest suite",
  "Entire home",
  "Entire townhouse",
  "Entire condo",
  "Entire guesthouse",
  "Entire serviced apartment",
  "Entire loft",
  "Floor",
  "Entire bungalow",
  "Entire cottage",
  "Entire vacation home"
)
  
)
SharedRoomTypeList <- c(
  "Shared room in rental unit",
  "Room in hostel",
  "Shared room in townhouse",
  "Shared room in home",
  "Shared room in hostel",
  "Shared room in hotel"
)

`Hotel/Unique/Boutique` <- c(
  "Tiny home",
  "Ranch",
  "Treehouse",
  "Room in boutique hotel",
  "Room in aparthotel",
  "Room in hotel"
)


CleanedListingsLM <- CleanedListingsLM %>%
  mutate(
    prop_type_broad = case_when(
      prop_type %in% EntirePlaceTypeList ~ 'Entire Place',
      prop_type %in% PrivateRoomTypeList ~ 'Private Room',
      prop_type %in% SharedRoomTypeList ~ 'Shared Room',
      prop_type %in% `Hotel/Unique/Boutique` ~ 'Hotel/Boutique/Unique',
      TRUE ~ 'Other'
    )
  )

priceModelBinned <- lm(
  price ~ 
    bedrooms + 
    bathrooms + 
    prop_type_broad + 
    neighbourhood_cleansed + 
    accommodates ,
  data = CleanedListingsLM,
  na.action = na.omit
) #price prediction model
summary(priceModelBinned) #okay, this isn't any better, I think I need to remove the neighborhood
#my previous attempt and model had the wrong neighborhood column. I am glad I found and fixed that 

priceModelBinned <- lm(
  price ~
    bedrooms+
    bathrooms+
    prop_type_broad+
    accommodates,
  data = CleanedListingsLM,
  na.action = na.omit
) #new price model without neighborhoods
summary(priceModelBinned) #maybe I need something else in there

priceModelBinned <- lm(
  price ~
    bedrooms+
    bathrooms+
    review_scores_rating+
    prop_type_broad+
    accommodates,
  data = CleanedListingsLM,
  na.action = na.omit
) #new price model without neighborhoods
summary(priceModelBinned) # nope, not review scores rating

hostNeighbourhoods <- ogListings%>%
  select(
    host_id,
    neighbourhood_cleansed
  )#create a table for hosts and their neighborhoods
unique(hostNeighbourhoods$neighbourhood_cleansed)





### We already know that the neighborhoods create too many dummy variables, 
#so I am going to instead separate into wards. Hopefully going from 39 dummy variables 
#down to 8 dummy variables will increase the accuracy of the model.
# I think that potentially the wards could be more telling as a price factor because each 
#ward has some autonomy with its laws and such

ward1list <- c(
  'Columbia Heights, Mt. Pleasant, Pleasant Plains, Park View',
  'Howard University, Le Droit Park, Cardozo/Shaw',
  'Kalorama Heights, Adams Morgan, Lanier Heights'
)

ward2list <- c(
  'Downtown, Chinatown, Penn Quarters, Mount Vernon Square, North Capitol Street',
  'Dupont Circle, Connecticut Avenue/K Street',
  'Georgetown, Burleith/Hillandale',
  'Shaw, Logan Circle',
  'West End, Foggy Bottom, GWU'
)

ward3list <- c(
  'Cathedral Heights, McLean Gardens, Glover Park',
  'Cleveland Park, Woodley Park, Massachusetts Avenue Heights, Woodland-Normanstone Terrace',
  'Friendship Heights, American University Park, Tenleytown',
  'North Cleveland Park, Forest Hills, Van Ness',
  'Spring Valley, Palisades, Wesley Heights, Foxhall Crescent, Foxhall Village, Georgetown Reservoir'
)

ward4list <- c(
  'Brightwood Park, Crestwood, Petworth',
  'Colonial Village, Shepherd Park, North Portal Estates',
  'Hawthorne, Barnaby Woods, Chevy Chase',
  'Lamont Riggs, Queens Chapel, Fort Totten, Pleasant Hill',
  'Takoma, Brightwood, Manor Park'
)

ward5list <- c(
  'Brookland, Brentwood, Langdon',
  'Edgewood, Bloomingdale, Truxton Circle, Eckington',
  'Ivy City, Arboretum, Trinidad, Carver Langston',
  'North Michigan Park, Michigan Park, University Heights',
  'Woodridge, Fort Lincoln, Gateway'
)

ward6list <- c(
  'Capitol Hill, Lincoln Park',
  'Near Southeast, Navy Yard',
  'Southwest Employment Area, Southwest/Waterfront, Fort McNair, Buzzard Point',
  'Union Station, Stanton Park, Kingman Park'
)

ward7list <- c(
  'Capitol View, Marshall Heights, Benning Heights',
  'Deanwood, Burrville, Grant Park, Lincoln Heights, Fairmont Heights',
  'Eastland Gardens, Kenilworth',
  'Fairfax Village, Naylor Gardens, Hillcrest, Summit Park',
  'Mayfair, Hillbrook, Mahaning Heights',
  'River Terrace, Benning, Greenway, Dupont Park',
  'Twining, Fairlawn, Randle Highlands, Penn Branch, Fort Davis Park, Fort Dupont'
)

ward8list <- c(
  'Congress Heights, Bellevue, Washington Highlands',
  'Douglas, Shipley Terrace',
  'Historic Anacostia',
  'Sheridan, Barry Farm, Buena Vista',
  'Woodland/Fort Stanton, Garfield Heights, Knox Hill'
)



CleanedListingsLM <- CleanedListingsLM %>%
  mutate(
    ward = case_when(
      neighbourhood_cleansed %in% ward1list ~ '1',
      neighbourhood_cleansed %in% ward2list ~ '2',
      neighbourhood_cleansed %in% ward3list ~ '3',
      neighbourhood_cleansed %in% ward4list ~ '4',
      neighbourhood_cleansed %in% ward5list ~ '5',
      neighbourhood_cleansed %in% ward6list ~ '6',
      neighbourhood_cleansed %in% ward7list ~ '7',
      neighbourhood_cleansed %in% ward8list ~ '8',
      TRUE ~ 'Other'
    )
  )

CleanedListingsLM$ward <- as.character(CleanedListingsLM$ward)
class(CleanedListingsLM$ward)

###

# Now we run the lm again

priceModelBinned <- lm(
  price ~
    bedrooms+
    bathrooms+
    accommodates+
    prop_type_broad+
    ward,
  data = CleanedListingsLM,
  na.action = na.omit
) #new price model with wards
summary(priceModelBinned) #it may not be great, but I'm sure if I dug further, I would be able to create a solid model

colnames(ogListings)

## well let's try some quality metrics
# adding in superhost and review scores rating

CleanedListingsLM$id <- as.character(CleanedListingsLM$id)

CleanedListingsLM <- left_join(CleanedListingsLM,
                               ogListings%>%
                                 select(id,
                                        host_is_superhost
                                        ),
                               join_by(id)
)

CleanedListings <- left_join(CleanedListings,
                               ogListings%>%
                                 select(id,
                                        host_is_superhost
                                 ),
                               join_by(id)
)


## okay rerun the regression
priceModelBinned <- lm(
  price ~
    bedrooms+
    bathrooms+
    accommodates+
    review_scores_rating+
    prop_type_broad+
    ward+
    host_is_superhost,
  data = CleanedListingsLM,
  na.action = na.omit
) #new price model with reviews scores and superhost
summary(priceModelBinned) # ehh maybe get rid of some of the variables that are possibly colinear or aren't helpful



priceModelBinned <- lm(
  price ~
    bedrooms+
    bathrooms+
    accommodates+
    review_scores_rating+
    prop_type_broad+
    ward+
    host_is_superhost,
  data = CleanedListingsLM,
  na.action = na.omit
) #new price model with reviews scores and superhost
summary(priceModelBinned)



priceModelBinned <- lm(
  price ~
    bedrooms+
    bathrooms+
    review_scores_rating+
    prop_type_broad+
    ward+
    host_is_superhost,
  data = CleanedListingsLM,
  na.action = na.omit
) #new price model with accommodates
summary(priceModelBinned) # okay maybe no review scores



priceModelBinned <- lm(
  price ~
    bedrooms+
    bathrooms+
    prop_type_broad+
    ward+
    host_is_superhost,
  data = CleanedListingsLM,
  na.action = na.omit
) #new price model without review scores
summary(priceModelBinned) #

mean_rating <- mean(CleanedListingsLM$review_scores_rating, na.rm = TRUE)
mean_rating


library(tidyverse)

CleanedListingsLM <- CleanedListingsLM %>%
  mutate(
    review_scores_rating_imputed = replace_na(review_scores_rating, mean_rating)
  ) #scaled the review_scores_rating with the average to hopefully improve the lm

CleanedListings <- CleanedListings %>%
  mutate(
    review_scores_rating_imputed = replace_na(review_scores_rating, mean_rating)
  ) #scaled the review_scores_rating with the average in case I use it during my visualizations

CleanedListingsLM <- CleanedListingsLM %>%
  mutate(
    host_is_superhost_cleaned = case_when(
      host_is_superhost == "" ~ NA_character_, # Convert blank strings to NA
      TRUE ~ host_is_superhost                 # Keep original if not blank
    ),
    # Now replace all NAs (original NAs + converted blanks) with 'f'
    host_is_superhost_cleaned = replace_na(host_is_superhost_cleaned, 'f')
  )


CleanedListings <- CleanedListings %>%
  mutate(
    host_is_superhost_cleaned = case_when(
      host_is_superhost == "" ~ NA_character_, # Convert blank strings to NA
      TRUE ~ host_is_superhost                 # Keep original if not blank
    ),
    # Now replace all NAs (original NAs + converted blanks) with 'f'
    host_is_superhost_cleaned = replace_na(host_is_superhost_cleaned, 'f')
  )


## rerun lm
priceModelBinned <- lm(
  price ~
    bedrooms+
    bathrooms+
    review_scores_rating_imputed+
    prop_type_broad+
    ward+
    host_is_superhost_cleaned,
  data = CleanedListingsLM,
  na.action = na.omit
) #new price model without review scores
summary(priceModelBinned) #not really any better

# we will continue with the simpler model

priceModelBinned <- lm(
  price ~
    bedrooms+
    bathrooms+
    prop_type_broad+
    ward,
  data = CleanedListingsLM,
  na.action = na.omit
) #new price model without review scores
summary(priceModelBinned) # this model will be used to determine the predicted price which will be compared to the

?write_csv

SummryListings <- CleanedListingsLM %>%
  group_by(prop_type_broad) %>%
  summarise(
    n = n(), # Count the number of listings in each group
    minPrice = min(price,
                   na.rm = TRUE),
    Q1_Price = quantile(price, 0.25, na.rm = TRUE),
    medianPrice = median(price, 
                         na.rm = TRUE),
    meanPrice = mean(price, 
                     na.rm = TRUE), # Often good to see mean vs median for skew
    Q3_Price = quantile(price, 0.75, na.rm = TRUE),
    maxPrice = max(price, 
                   na.rm = TRUE),
    IQR_Price = IQR(price, 
                    na.rm = TRUE), # Interquartile Range
    # Calculate outlier fences (standard 1.5 * IQR rule)
    UpperFence = Q3_Price + 1.5 * IQR_Price,
    LowerFence = Q1_Price - 1.5 * IQR_Price
  ) #find major outliers

CleanedListingsLM <- left_join(
  CleanedListingsLM,
  SummryListings,
  join_by(prop_type_broad)
)

View(CleanedListingsLM)

class(CleanedListingsLM$price)

outlierAnalysisPriceG <- ggplot(CleanedListingsLM,
                                aes(
                                  price,
                                  colour = prop_type_broad
                                )) +
  geom_boxplot()
print(outlierAnalysisPriceG) #outliers are luxury and non average properties


CleanedListingsLMOutliersRMVD <- CleanedListingsLM %>%
  filter(
    price < UpperFence
  )
View(CleanedListingsLMOutliersRMVD)

unique(CleanedListingsLMOutliersRMVD$prop_type_broad)

outlierAnalysisPriceGclean <- ggplot(CleanedListingsLMOutliersRMVD,
                                aes(
                                  price,
                                  colour = prop_type_broad
                                )) +
  geom_boxplot()
print(outlierAnalysisPriceGclean)


priceModelBinnedClean <- lm(
  price ~
    bedrooms +
    bathrooms +
    accommodates+
    prop_type_broad +
    ward,
  data = CleanedListingsLMOutliersRMVD,
  na.action = na.omit) #lm for binned properties with outliers removed to focus on average air bnb market
summary(priceModelBinnedClean) #new lm without outliers



CleanedListingsLMOutliersRMVD <- CleanedListingsLMOutliersRMVD %>%
  mutate(
    predicted_price = predict(priceModelBinnedClean, newdata = .)
  )#create predicted price column


CleanedListings$prop

CleanedListingsLM <- CleanedListingsLM %>%
  mutate(
    predicted_price = predict(priceModelBinnedClean, newdata = .)
  )#create predicted price column


CleanedListingsLMOutliersRMVD <- CleanedListingsLMOutliersRMVD %>%
  mutate(
    predict_diff = predicted_price - price
  )

CleanedListingsLM <- CleanedListingsLM %>%
  mutate(
    predict_diff = predicted_price - price
  )

summary(CleanedListingsLMOutliersRMVD$predict_diff)





#####


##### Going forward,I would like to also point at that due to the extreme outliers, 
#I should move forward looking at really only the outlier removed data.
# If you look at the differences in the models, this model makes a lot more sense
# For example, if you look at the coefficients on the dummy variables for the prop_type_broad
# You will see that the model generated on the full listings dataset,
#the shared_room type would be more valuable than the private_room
# In the outliers removed model, we see a more realistic order of value:
#the hotel/boutique/unique is most value adding, then private_rooms are next most value adding,
#followed by shared_rooms
#I think potentially, you could argue that hotel/boutique/unique could be removed from the data as well.
# Removing the luxurious stays category would likely result in a more accurate model because
#you would be comparing the differences between just true to the original intent of airbnb.
# Those two categories would be shared_rooms accommodations and private_room accommodations
##### The most convincing part though is how much more accurate the model is when removing the extreme price outliers


#####

num_hotel_boutique_unique <- CleanedListingsLMOutliersRMVD %>%
  filter(prop_type_broad == "Hotel/Boutique/Unique") %>%
  tally()
num_hotel_boutique_unique # I think it will be perfectly fine to focus on just the initial airbnb vision of peer-to-peer accommodations






CleanedListingsLMOutliersRMVD <- CleanedListingsLMOutliersRMVD %>%
  filter(
    prop_type_broad != 'Hotel/Boutique/Unique'
  )
View(CleanedListingsLMOutliersRMVD)

outlierAnalysisPriceGclean <- ggplot(CleanedListingsLMOutliersRMVD,
                                     aes(
                                       price,
                                       colour = prop_type_broad
                                     )) +
  geom_boxplot()
print(outlierAnalysisPriceGclean)


priceModelBinnedClean <- lm(
  price ~
    bedrooms +
    bathrooms +
    accommodates+
    prop_type_broad +
    ward,
  data = CleanedListingsLMOutliersRMVD,
  na.action = na.omit) #lm for binned properties with outliers removed to focus on average air bnb market
summary(priceModelBinnedClean) #new lm without outliers



CleanedListingsLMOutliersRMVD <- CleanedListingsLMOutliersRMVD %>%
  mutate(
    predicted_price = predict(priceModelBinnedClean, newdata = .)
  )#create predicted price column


CleanedListingsLM <- CleanedListingsLM %>%
  mutate(
    predicted_price = predict(priceModelBinnedClean, newdata = .)
  )#create predicted price column


CleanedListingsLMOutliersRMVD <- CleanedListingsLMOutliersRMVD %>%
  mutate(
    predict_diff = predicted_price - price
  )

summary(CleanedListingsLMOutliersRMVD$predict_diff)



#### Long story short, the outliers removed is going to be the one I have to focus on
#when I create the visuals, so I am not even going to write the CleanedListings base file into a csv.



############
############
############
#Continue from this point forward tomorrow




HostAvgPriceDiffAndAvgPrice <- CleanedListingsLMOutliersRMVD %>%
  group_by(host_id) %>%
  summarise(
    AvgPriceDifferential = mean(predict_diff) ,
    AvgPredictedPrice = mean(predicted_price)
  )#create a new datatable to aggregate the data



CleanedListingsLMOutliersRMVDTopPropOwners <- CleanedListingsLMOutliersRMVD %>%
  filter(prop_count >= 15)

`Avg. Price Differential vs the Number of properties over 15 properties` <- ggplot(
  CleanedListingsLMOutliersRMVDTopPropOwners,
  aes(
    prop_count,
    predict_diff,
    color = ward
  )
) +
  geom_point() +
  geom_smooth()#create ggplot for avg price differential per host / the number of properties that hose has
print(`Avg. Price Differential vs the Number of properties over 15 properties`)



topPropSummaryStats <- CleanedListingsLMOutliersRMVDTopPropOwners %>%
  group_by(host_id) %>%
  summary(prop_count)







## Write necessary files to csv ##

CleanedNeighbourhoods <- CleanedNeighbourhoods %>%
  mutate(
    ward = case_when(
      neighbourhood %in% ward1list ~ '1',
      neighbourhood %in% ward2list ~ '2',
      neighbourhood %in% ward3list ~ '3',
      neighbourhood %in% ward4list ~ '4',
      neighbourhood %in% ward5list ~ '5',
      neighbourhood %in% ward6list ~ '6',
      neighbourhood %in% ward7list ~ '7',
      neighbourhood %in% ward8list ~ '8'
    )
) %>%
  select(neighbourhood,
         ward)
write.csv(
  CleanedNeighbourhoods,
  file = 'Cleaned Neighbourhoods.csv'
)

write.csv(
  CleanedListingsLMOutliersRMVD,
  file = 'Cleaned Listings Without Outliers.csv'
)

write.csv(
  CleanedHosts,
  file = 'Cleaned Hosts.csv'
)

write.csv(
  CleanedCalendar,
  file = 'Cleaned Calendar.csv'
)

write.csv(
  CleanedReviews,
  file = 'Cleaned Reviews.csv'
)

write.csv(
  CleanedHostsPropCount,
  file = 'Property Count.csv'
) 

write.csv(
  Top1PercentHostByPropCount,
  file = 'Top 1 Percent Hosts By Property count.csv'
)

write_csv(
  HostAvgPriceDiffAndAvgPrice,
  file = 'Host Avg Price Differential and Avg Price.csv'
)





#Checking some tables 



toplistings <- left_join(
  Top1PercentHostByPropCount,
  
)



`15+Hosts` <- CleanedHostsPropCount %>%
  filter(prop_count >= 15 )
sum(`15+Hosts`$prop_count)
