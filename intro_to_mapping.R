# BHCD - SORT R Support
# Intro to Mapping with R
# 09/25/2023

#---- Packages & Libraries #----
# install.packages(c("tidyverse", "sf", "tigris", "tidycensus","tmap"))
library(tidyverse)
library(sf)
library(tigris)
library(tidycensus)
library(tmap)


#---- Project Overview #----

# Suppose we have an outreach program targeted toward improving children's health
# in Baltimore and leadership has identified playgrounds as the best site to
# start that outreach. You think this is a great idea, but also are concerned
# that different communities' access to playgrounds may not be equal, so you
# want to map out the spatial distribution of playgrounds in Baltimore so you
# can be sure that there aren't any communities that wouldn't receive outreach.


#---- Step 1. Import playground data #----

# Set the working directory to the location where you saved the material for this demo
setwd(file.choose())

# Playground data (downloaded from Open Street Maps)
play <- st_read("baltimore_playgrounds_osm.shp")

# Now let's quickly map where the playgrounds are and we'll be done!
plot(play$geometry)
  # It's important to include '$geometry' in the plot command - if we don't,
  # R will plot each element of the spatial object (which can sometimes be a lot)

# Oh... it's just a bunch of dots that kind of look like Baltimore if you squint
# Let's add a basemap for context. We'll download the boundary file of the city.
bmore <- tigris::counties(state = "MD", cb = TRUE)

# Not let's plot our new Baltimore map
plot(bmore$geometry)

# Oh... it's the whole state. Let's filter for just Baltimore City. First, what
# are the names and structure of the columns in the 'bmore' object?
str(bmore)
# Let's filter using the 'NAMELSAD' column. How is Baltimore recorded?
unique(bmore$NAMELSAD)
# The last name is 'Baltimore city' (lower case c, R is case-sensitive so pay attention!)
# Now let's filter for Baltimore.

# We can use bracket filtering
bmore <- bmore[bmore$NAMELSAD == "Baltimore city", ]
  # In bracket indexing, we tell R to return only cells meeting certain criteria
  # Since bmore is a dataframe (i.e. with rows and columns), we need to tell it
  # criteria for rows and criteria for columns. Within the brackets, we provide
  # information about the rows we want first, then a comma, then information about
  # the columns we want. In this case, we want rows where NAMELSAD is 'Baltimore city',
  # and we want all columns, so we just put a comma after the row criteria and then
  # leave a blank before closing the square brackets.

# Or we can also use dplyr filtering
bmore <- bmore %>%
  dplyr::filter(NAMELSAD == "Baltimore city")

# Now let's plot the playground information again
plot(play$geometry)
# And let's add the Baltimore map
plot(bmore$geometry)
# Wait, where'd the playgrounds go? To plot multiple spatial layers, we'll need
# to include 'add = TRUE' to our plot function. Let's try again
plot(play$geometry)
plot(bmore$geometry, add = TRUE)
# Why didn't that work? Let's check to make sure the map projections of our two
# layers match.
st_crs(play)
  ## NAD83 / Maryland (ftUS)
st_crs(bmore)
  ## NAD83
# That explains it. Let's reproject the Baltimore map to match the playgrounds
bmore <- st_transform(bmore, crs = st_crs(play))

# And let's try plotting again
plot(play$geometry)
plot(bmore$geometry, add = TRUE)

# Part of Baltimore got clipped off... Let's switch the order of the plots to make
# sure everything is visible
plot(bmore$geometry)
plot(play$geometry, add = TRUE)

# Great! All done, right?


#---- Step 2. Compare to Population Density #----

# This is a nice start, but we should probably compare where people live to where 
# the playground outreach will occur to make sure we have good coverage.

# Let's download population counts by census tract using the tidycensus package.
# We'll use the 2020 American Community Survey 5-year estimates. To get tract population,
# we'll need to know the variable number.

# Let's load a list of the variables
load_variables(year = 2020, dataset = "acs5") %>%
  dplyr::filter(geography == "tract")
  ## Total population is 'B01001A_001'

tract.pop <- get_acs(
  geography = "tract", # We want data at the tract level
  state = "MD", # Specify the state
  county = "Baltimore city", # Specify the county
  variables = "B01001A_001", # This is name of the variable we want
  year = 2020, # We want the most recent year
  survey = "acs5", # We want the 5-year ACS survey
  geometry = TRUE, # tidycensus will return a spatial object if we ask - how nice!
  output = "wide", # I thinks this format is a little easier to work with
  key = "2082857cc31cbba17def539b8ab26a66b4ff4090" # My API key (ok to use here, 
                                                   # but please don't use for your
                                                   # own projects - Census will lock
                                                   # out accounts that are making
                                                   # too many API calls) - you can
                                                   # register for your own key at:
                                                   # http://api.census.gov/data/key_signup.html
  )

# We can plot population density now!
plot(tract.pop$B01001A_001E)
# Whoa, that's not what we're looking for. Turns out, making a map instead of a
# typical data plot requires a specific type of function call.
plot(tract.pop[, "B01001A_001E"])
# That's a pretty non-intuitive name for the population variable - let's rename it.
tract.pop <- tract.pop %>%
  mutate(totalpop = B01001A_001E)
plot(tract.pop[, "totalpop"])

# Now let's go ahead and re-project this spatial dataframe so it matches the playgrounds
tract.pop <- tract.pop %>%
  st_transform(st_crs(play))

# And let's overlay the playground locations
plot(tract.pop[, "totalpop"])
plot(play$geometry, add = TRUE)

# Oh, that's less than ideal. Turns out R's native graphics engine kind of fudges
# things around a little bit. Quick plots can be useful for checking our progress
# but we want something a little nicer to inform decision making.


#---- Step 3. Pretty maps with the tmap package #----

# tmap uses a similar syntax to ggplot, but is geared toward spatial data and
# mapmaking. Instead of 'geom_' calls to add layers, we use 'tm_', etc.

# Let's make a quick map of population density
tm_shape(tract.pop) +
  tm_polygons("totalpop") # Variable name we want to plot

# Oooo nice. Let's clean up the formatting
popdens.tract <- tm_shape(tract.pop,
                          unit = "mi") + # Changes the units to miles (easier to
                                         # interpret for most US readers)
  tm_polygons("totalpop", 
              title = "Total\nPopulation", # Legend title ('\n' is a line return)
              style = "cont", # Change to a continuous scale (instead of binned)
              palette = "Reds") # Let's use a red, monotone color ramp

# Storing a tmap object doesn't print it, so we have to call the object specifically
popdens.tract

# Let's add on the playground locations and some classic map elements
play.pop.tract <- popdens.tract +
  tm_shape(play) +
  tm_dots(shape = 21,
          size = 0.2,
          col = "black",
          border.col = "grey50") +
  tm_add_legend(type = "symbol", # tm_dots doesn't include a legend, so we need
                labels = "Playgrounds", # to make our own
                col = "black",
                border.col = "grey50",
                shape = 21,
                size = 0.5) +
  tm_compass(position = c("left", "BOTTOM")) + # Centering the north arrow
  tm_scale_bar(position = c("left", "BOTTOM"),
               breaks = c(0, 1, 2, 3)) + # Set up the scale breaks at 0, 1, 2, 3 miles
  tm_layout(main.title = "Playgrounds and Tract Population - Baltimore, MD",
            legend.outside = TRUE) # Moving the legends outside the map area
                                   # so it doesn't overlap.
play.pop.tract

# This is nice, but tract boundaries aren't as practically useful. Let's calculate
# population by Community Statistical Area (CSA)


#---- Step 4. Spatial Joining Tracts and CSAs #----

# Read in the CSA shapefile & reproject
csa <- st_read("Community_Statistical_Areas__2020_.shp") %>%
  st_transform(st_crs(play))

# Read in the CSA relationship file
csa.rel <- read.csv("Census_Tract_2020_to_Community_Statistical_Area_2020.csv") %>%
  mutate(
    GEOID = as.character(GEOID_Tract_2020), # Renaming to match existing column names
    CSA2020 = Community_Statistical_Area_2020)

# Assigning census tracts their appropriate CSA
tract.pop <- tract.pop %>%
  left_join(csa.rel[c("GEOID", "CSA2020")], # Merging tract population data with
                                            # CSA relationship file, but only
                                            # the columns 'GEOID' and 'CSA2020'
            by = "GEOID")

# Counting the population total by CSA
csa.sum <- tract.pop %>%
  group_by(CSA2020) %>%
  summarize(totalpop = sum(totalpop))

# Merging CSA population total with CSA shapefile
csa.pop <- csa %>%
  left_join(as.data.frame(csa.sum), # Since 'csa.sum' is a spatial object, R will
                                    # try to do a spatial merge, which we don't
                                    # want. Using 'as.data.frame' tells R to forget
                                    # that it's a spatial object.
            by = "CSA2020")

# Now lets make the same tmap figure from before, but using CSA boundaries
play.pop.csa <- tm_shape(csa.pop,
                         unit = "mi") +
  tm_polygons("totalpop", 
              title = "Total\nPopulation", 
              style = "cont", 
              palette = "Reds") +
  tm_shape(play) +
  tm_dots(shape = 21,
          size = 0.2,
          col = "black",
          border.col = "grey50") +
  tm_add_legend(type = "symbol",
                labels = "Playgrounds",
                col = "black",
                border.col = "grey50",
                shape = 21,
                size = 0.5) +
  tm_compass(position = c("left", "BOTTOM")) +
  tm_scale_bar(position = c("left", "BOTTOM"),
               breaks = c(0, 1, 2, 3)) +
  tm_layout(main.title = "Playgrounds and CSA Population - Baltimore, MD",
            legend.outside = TRUE)
play.pop.csa

# Now we're talking. But it's still a little tough to know just how many playgrounds
# there are when they show up as dots. Let's calculate the number of playgrounds
# per person in each CSA.


#---- Step 5. Scaling Playground Counts to Population #----

# Let's get a count of how many playgrounds there are in each CSA
csa.pop <- csa.pop %>%
  mutate(playgrounds = lengths(st_intersects(csa.pop, play)))
    # Creating a new variable called 'playgrounds' that counts how many points from
    # the 'play' shapefile are in each CSA polygon. st_intersects returns a list
    # of the points that are in each CSA, and the lengths function just counts how
    # long each list is.

# Now we can plot that new variable in a choropleth
play.dens <- tm_shape(csa.pop,
                      unit = "mi") +
  tm_polygons("playgrounds", 
              title = "Playgrounds\nper Capita", 
              style = "cont", 
              palette = "Reds") +
  tm_compass(position = c("left", "BOTTOM")) +
  tm_scale_bar(position = c("left", "BOTTOM"),
               breaks = c(0, 1, 2, 3)) +
  tm_layout(main.title = "Playgrounds Density by CSA - Baltimore, MD",
            legend.outside = TRUE)
play.dens

# Now that's a map we can use to try to inform good policy! Though there's maybe
# some better ways to scale the playground density calculation... maybe to the
# population count we're targeting our intervention toward?

# But this nice map only lives inside R for now. How do we make a nice copy to
# share with others?


#---- Step 6. Saving tmap Output #----

# There are 2 approaches we can use: tmap_save and a graphics device

# tmap_save approach
tmap_save(tm = play.dens,
          filename = "playground_density_tmsave.png",
          width = 6,
          height = 8,
          dpi = 300,
          units = "in")

# Graphics device
png(filename = "playground_density_grdev.png",
    width = 6,
    height = 8,
    res = 300,
    units = "in")
play.dens
dev.off()
