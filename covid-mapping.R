library('tidyverse')
library('tidyselect')
library('sf')
#library('ggplot2')
library('viridis')
library('directlabels')
library('scales')


covidDF <- read.csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv') %>%
	#dplyr::filter(Country.Region == 'Australia') %>%
	mutate(Lat  = ifelse(Country.Region == 'New Zealand', -41.289685, Lat)) %>% # Manually update the coords for New Zealand, because the JHU data has the coords in the ocean
	mutate(Long = ifelse(Country.Region == 'New Zealand', 174.764551, Long)) %>%
	
	mutate(Lat  = ifelse(Country.Region == 'Philippines', 14.642824, Lat)) %>% # Manually update the coords for Philippines
	mutate(Long = ifelse(Country.Region == 'Philippines', 121.055399, Long)) %>%
	
	mutate(Lat  = ifelse(Country.Region == 'Monaco', 43.751596, Lat)) %>% # Manually update the coords for Monaco
	mutate(Long = ifelse(Country.Region == 'Monaco', 7.437173, Long)) %>%
	
	mutate(Lat  = ifelse(Country.Region == 'Maldives', 4.174540, Lat)) %>% # Manually update the coords for Maldives
	mutate(Long = ifelse(Country.Region == 'Maldives', 73.509402, Long)) %>%
	
	
	st_as_sf(coords = c("Long", "Lat"), crs=4326, agr='identity', remove=FALSE) 

# https://github.com/datasets/geo-countries/raw/master/data/countries.geojson

# ~/Dropbox/Uni/CodeClub/world.geojson

world <- st_read('~/Dropbox/Uni/GIS-data/GIS Data/Generic datasets/world/ne_50m_admin_0_countries.shp') %>%
	dplyr::select(ADMIN)

st_write(world, '~/Dropbox/Uni/CodeClub/world.geojson')

# Check each row to make sure the coords supplied are within a country.
# Need to check this each day, as new countries are added to the above dataset.
# Island nations are more likely to have coordinates outside their land boundaries.
v <- st_intersection(world, covidDF)
setdiff(covidDF$Country.Region, v$Country.Region)


dateColNames <- vars_select(names(covidDF), starts_with("X"))
colNameFirst <- dateColNames[1]
colNameLast  <- dateColNames[length(dateColNames)]

covidDF$first <- st_set_geometry(covidDF[, colNameFirst], NULL)[, 1]
covidDF$last  <- st_set_geometry(covidDF[, colNameLast], NULL)[, 1]


# Select the top 6 countries in terms of total case numbers (including current, recovered, and deceased)
topInfectedCountries <- as.character(covidDF$Country.Region[covidDF$last %in% head(sort(covidDF$last, de=T), 5)])

# Add in Australia
topInfectedCountries <- c('Australia', topInfectedCountries)


# Plot cumulative cases for these country
covidDF %>%
	dplyr::filter(Country.Region %in% topInfectedCountries) %>% # select only the countries we're interested in
	st_set_geometry(NULL) %>% # pivot_longer doesn't seem to like the geometry column
    pivot_longer(cols=starts_with('X'), names_to='date', names_prefix='X', values_to='stateCases') %>% # convert from wide to long format for ggplot
    mutate(timestamp=as.POSIXct(strptime(date, '%m.%d.%y'))) %>% # convert date columns to proper date objects
    group_by(Country.Region, timestamp) %>% # Group by country...
    summarise(totalCases=sum(stateCases)) %>% # and total for all states/provinces
	ggplot(aes(x=timestamp, y=totalCases, colour=Country.Region, group=Country.Region)) +
		geom_line() +
		geom_dl(aes(label = Country.Region), method = list(dl.trans(x = x + 0.2), "last.points", cex = 0.8)) +
		scale_colour_discrete(guide = 'none') +
		scale_x_discrete(expand=expand_scale())



# Map and colourise countries based on cumulative case count
totalCases <- covidDF %>%
	st_intersection(world) %>%
	dplyr::select(ADMIN, last) %>%
	dplyr::group_by(ADMIN) %>%
	dplyr::summarise(total=sum(last)) %>%
	st_set_geometry(NULL)

# Now join world and totalCases by ADMIN
left_join(world, totalCases) %>% 
	mutate(total = ifelse(ADMIN == 'China', NA, total)) %>% # remove China because it's so high it swamps everything else
	ggplot() + 
		geom_sf(aes(fill=total), colour='gray50', size=0.2) + # Plot countries and colour based on total
		geom_sf(data=dplyr::filter(world, ADMIN=='China'), fill='red') + # Add China and colour with pandemic-red
		theme_classic() +
		scale_fill_viridis_c(direction = 1, na.value='lightgray', name="Total cases:") +
		coord_sf(datum = NA)  # to remove graticule


##########


library(mapview)
world %>%
	dplyr::filter(ADMIN == 'Monaco') %>%
	mapview()
	
	plot()

############


totalCases %>%
	dplyr::filter(ADMIN == 'New Zealand')

covidDF %>%
	dplyr::filter(Country.Region == 'New Zealand')


sort(unique(covidDF$Country.Region))


z <- left_join(world, totalCases)

z %>%
	arrange(desc(total)) %>%
	dplyr::filter(ADMIN == 'New Zealand') %>%
	plot()


############



vv <- v %>%
	filter(as.character(timestamp) > '2020-02-17') %>%
	#filter(as.character(timestamp) < '2020-03-01')
	filter(as.character(timestamp) >= '2020-03-01')
ggplot() + geom_line(data=vv, aes(x=timestamp, y=totalCases))


range(vv$totalCases)

range(v$timestamp)

max(vv$timestamp) - min(vv$timestamp)

max(vv$totalCases) / min(vv$totalCases)

########



ggplot() + 
	geom_sf(data=world) +
	geom_sf(data=covidDF, aes(colour=last)) +
	theme_classic() +
	coord_sf(datum = NA) # to remove graticule

hist(covidDF$X3.5.20)


selectFirst <- function(x) {
	print(x)
	return(x)
	
	
}


pre(sort(unique(covidDF$Country.Region)))

covidDF %>%
	dplyr::filter(Country.Region == 'New Zealand')

# Sort by total DESC
covidDF %>%
	#st_set_geometry(NULL) %>%
	dplyr::select(Country.Region, Province.State, last) %>%
	dplyr::group_by(Country.Region) %>%
	dplyr::summarise(total=sum(last), geom=selectFirst(geometry)) %>%
	dplyr::arrange(desc(total))

totalCases <- covidDF %>%
	dplyr::select(Country.Region, Province.State, last) %>%
	dplyr::group_by(Country.Region) %>%
	dplyr::summarise(total=sum(last)) %>%
	print(n=Inf)

ggplot() + 
	geom_sf(data=world) +
	geom_sf(data=totalCases, pch=16, aes(colour=last)) +
	theme_classic() +
	coord_sf(datum = NA) # to remove graticule

###########

# Multiplicative
x <- 1:60
y <- x^2
plot(x, y)


a <- 1 # Starting number
b <- 2 # Each iteration is raised to this exponent (2 = doubles)
tcon <- 6 # How long does it take to increase by b?
t <- 1

y <- a * b^((x * t) / tcon)

plotDF <- data.frame(x, y)

# plot(x, y, xlab="Days", ylab="Total cases", pch=NA, las=1)
# lines(x, y)

ggplot(plotDF, aes(x = x, y = y)) + 
	geom_line() +
	scale_y_continuous(labels = comma) +
	xlab("Days") +
	ylab("Total cases")


1, 2, 4, 8, 16, 32, 64, 128

y <- 1
for (i in x) {
	y <- c(y, y[

# Exponential


