# This script uses the TidyCensus R package to explore American Community Survey data from the Census Bureau
# Application Programming Interface (API) and is used in the Census Bureau's Intro to API Course.
# The demonstration was done using RStudio, but the package can be used in any environment that runs R,
# with minor limitations.

# The course content can be found at https://www.census.gov/data/academy/courses.html.

# More information about the Census Bureau's API, including where to request an API key, can be found
# at https://www.census.gov/developers/.

# Module 3: Creating Your Datasets Using Excel and R

# First, use the function install.packages() to install TidyCensus and Tidyverse. You will only have to do this once.
# Tidyverse is a system of packages that help facilitate statistics and data analysis in R and is useful
# to help power the work we will be doing in this tutorial.

install.packages("tidycensus")
install.packages("tidyverse")

# Once these packages are installed in RStudio, you will use the function library() to load them into your
# session so that you can use them. You will have to do this each time you start a new session in which
# you want to use these packages.

library(tidycensus)
library(tidyverse)

# This is where you can install your personal API key into RStudio.
# You will need to obtain an API key in order to use tidycensus. Once you install your API key with this command
# RStudio will remember it in the local memory and you do not have to install it again.

census_api_key("API Key", install = "TRUE")

# Replace "API Key" with your API key once you get it.


# We will use the function load_variables() to view all of the possible variables we can use from the dataset
# of interest. We need to save the result as an object so that we can view it, much like you would save a file.
# We will call this result "acs18." Note that we are pulling the ACS 5-Year Estimates for the end year 2018,
# Which means we are pulling the 2014-2018 ACS 5-Year Estimates. By default, this pulls all the variables for
# detailed tables. If we wanted to pull all variables for a product such as data profiles, we would change the
# dataset to "acs5/profile."

acs18 <- load_variables(year = 2018,
                        dataset = "acs5", 
                        cache = TRUE)


# We will then view the results in a searchable table.
# Note that the View() function might not result in a searchable table outside of RStudio.

View(acs18)


# Now we will use the function get_acs() to access the Median Household Income for all counties in Maryland.
# Then we will save the results as "mhi_md" and then view the results.

mhi_md <- get_acs(geography = "county",
                     variables = "B19013_001",
                     state = "MD",
                     survey = "acs5",
                     year = 2018)

View(mhi_md)


# We will drill down to the Census Tract level, using the FIPS code 003 for Anne Arundel County (which
# we can find using the results of the previous query). This will pull Median Income for all Census
# Tracts within Anne Arundel County. Then we will save the results as "mhi_aacmd" and then view the results.

mhi_aacmd <- get_acs(geography = "tract",
                variables = "B19013_001",
                state = "MD",
                county = 003,
                survey = "acs5",
                year = 2018)

View(mhi_aacmd)


# To view a full table - for instance, a full distribution of household incomes for each Census Tract in a county
# - you can subsitute the variables argument for the table argument. We will save the results as "aacmd_val"
# and then view the results.

aacmd_inc <- get_acs(geography = "tract",
                     table = "B19001",
                     state = "MD",
                     county = 003,
                     survey = "acs5",
                     year = 2018)

View(aacmd_inc)

# We will drill down to all Places in MD. This way we can find Median Household Income
# for, say, Annapolis like we used in previous examples in this course.
# Then we will save the results as "placesmd_mhi" and then view the results.

placesmd_mhi <- get_acs(geography = "place",
                     table = "B19013",
                     state = "MD",
                     survey = "acs5",
                     year = 2018)

View(placesmd_mhi)

#Module 4: Using Your Data and Next Steps

# Use the library() function to load the packages we installed in Module 3 into your
# session so that you can use them. You will have to do this each time you start a new session in which
# you want to use these packages.

library(tidycensus)
library(tidyverse)

# Here we are going to name our variables so that we can access them easily and consistently while we work with them.
# "DP03_0062" is Median Household Income, so we are going to save this call as "mhincome." Once we save it with this
# name, we no longer have to use the API call, and we can just reference the name we've chosen.
# We will do the same for DP02_0067P, which is Bachelor's Degree or Higher. We will name it bachplus.
# Note that since we want the percent Bachelor's Degree or Higher, we added a P to the end of the call: "DP02_0067P"

mhincome <- c("DP03_0062")
bachplus <- c("DP02_0067P")

# Here, we will pull the dataset just as we did in module 3 using get_acs(), and we will call the dataset
# "mhi_by_state." However; this time instead of putting the API call in the variables argument,
# we will refer to the "mhincome" variable that we saved a few lines up.
# The program remembers that "mhincome" equals "DP03_0062" so that we don't have to state it.

mhi_by_state <- get_acs(geography = "state",
                        variables = mhincome,
                        survey = "acs5",
                        year = 2018)

# We will do the same thing for the dataset "bachplus_by_state" with the variable "bachplus."

bachplus_by_state <- get_acs(geography = "state",
                        variables = bachplus,
                        survey = "acs5",
                        year = 2018)

# In the following two plots we will add the names of the datasets that we created above ("mhi_by_state"
# and "bachplus_by_state") into a function that will plot them. Each of the two graphs will plot
# the variables ("mhincome" and "bachplus") by state.

mhi_by_state %>%
  ggplot(aes(x = estimate, y = reorder(NAME, estimate))) +
  geom_point()

bachplus_by_state %>%
  ggplot(aes(x = estimate, y = reorder(NAME, estimate))) +
  geom_point()

# The following plots will build upon the previous plots by adding margins of error
# as well as the ability to add in titles and an axis label. When making these API queries,
# tidycensus automatically pulls the margins of error for you. Thus, the margins of error
# are available if you want to use them in your analysis, as illustrated by this plot.

mhi_by_state %>%
  ggplot(aes(x = estimate, y = reorder(NAME, estimate))) +
  geom_errorbarh(aes(xmin = estimate - moe, xmax = estimate + moe)) +
  geom_point(color = "red", size = 1) +
  labs(title = "Median Household Income by State",
       subtitle = "2014-2018 American Community Survey 5-Year Estimates",
       y = "",
       x = "Median Household Income (bars represent margin of error)")

bachplus_by_state %>%
  ggplot(aes(x = estimate, y = reorder(NAME, estimate))) +
  geom_errorbarh(aes(xmin = estimate - moe, xmax = estimate + moe)) +
  geom_point(color = "red", size = 1) +
  labs(title = "Bachelor's Degree or Higher by State",
       subtitle = "2014-2018 American Community Survey 5-Year Estimates",
       y = "",
       x = "Bachelor's Degree or Higher (bars represent margin of error)")

