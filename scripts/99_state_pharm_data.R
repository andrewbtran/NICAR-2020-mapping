library(tidyverse)
library(arcos)

# Get list of state abbreviations that's already in R
list_of_states <- state.abb

# Loop to download total pharmacy pill totals 
# using the arcos package

for (i in 1:length(list_of_states)) {
  
  # pulls out the state abbreviation based on where the array is at
  state_abb <- list_of_states[i]
  
  # this downloads arcos data
  state_totals <- total_pharmacies_state(state=state_abb, key="WaPo")

  # these lines below append the data set onto the first one 
  # building up one big one
  if (i==1) {
    state_df <- state_totals
  } else {
    state_df <- rbind(state_df, state_totals)
  }
    
  # just lets you know what state the loop is on
  print(list_of_states[i])
}

# pulls the county annual population 
# (don't need to loop this one)
population <- county_population(key="WaPo")

# need to summarize the annual population into average population
population <- population %>% 
  group_by(BUYER_COUNTY, BUYER_STATE, countyfips) %>% 
  # Figure out the average population between available years
  summarize(average_population=mean(population, na.rm=T)) %>% 
  ## Have to quickly rename these columns to make them lower case so they'll join easily to the other data frame
  rename(buyer_county=BUYER_COUNTY, buyer_state=BUYER_STATE)

## Join the data to the dosage units data frame
state_joined <- left_join(state_df, population)

# do some math to figure out average pills per person per year
state_joined <- state_joined %>% 
  mutate(per_person=total_dosage_unit/average_population/7) %>% 
  mutate(per_person=round(per_person, 1)) %>% 
  rename(BUYER_COUNTY=buyer_county, BUYER_STATE=buyer_state, BUYER_DEA_NO=buyer_dea_no)

## get lat lon of each pharmacy
state_loc <-pharm_latlon(key="WaPo")

## join the pharmacy analysis dataframe with the geocoded dataframe
state_loc <- left_join(state_loc, state_joined)

## download another dataframe that includes addresses data for pharmacies
buyers <- buyer_addresses(key="WaPo")

## cleaning up the data frame a bit before exporting
## rounding numbers and dropping extraneous columns
state_loc <- left_join(state_loc, buyers) %>% 
  mutate(average_population=round(average_population)) %>% 
  select(-buyer_name, -buyer_city)

## saving for later
write_csv(state_loc, "all_pharmacies_summarized.csv", na="")

