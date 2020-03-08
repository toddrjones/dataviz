library(tidyverse)
library(gganimate)
library(countrycode)
library(RColorBrewer)

#set your working directory here
setwd("")

#download from https://www.gapminder.org/data/, and save to /data folder
gdp_w <- read.csv("data/income_per_person_gdppercapita_ppp_inflation_adjusted.csv")
mort_w <- read.csv("data/child_mortality_0_5_year_olds_dying_per_1000_born.csv")
pop_w <- read.csv("data/population_total.csv")

#make data from wide to long
gdp <- gather(gdp_w, Year, gdp, starts_with("X"))
mort <- gather(mort_w, Year, mort, starts_with("X"))
pop <- gather(pop_w, Year, pop, starts_with("X"))

#remove X from beginning
gdp$Year<-substring(gdp$Year, 2)
mort$Year<-substring(mort$Year, 2)
pop$Year<-substring(pop$Year, 2)

#merge
data <- merge(gdp, mort, by=c("country", "Year"))
data <- merge(data, pop, by=c("country", "Year"))

#label variable
names(data)[2] <- "year"

#create log gdp variable
data$lgdp <- log(data$gdp)

#convert year to integer
data$year <- as.integer(data$year)

#merge on continent - not used in current plot, but can be useful
data$continent <- countrycode(sourcevar = data[, "country"],
							  origin = "country.name",
							  destination = "continent")

#set the color scale here
colourCount = length(unique(data$mort))
getPalette = colorRampPalette(brewer.pal(9, "Blues"))

#plot
map <-   ggplot(data, aes(x = lgdp, y = mort)) +
	geom_point(aes(col = mort), size=4, alpha=.9, show.legend = FALSE) +
	scale_colour_gradientn(colours=rev(getPalette(colourCount))) +	
	theme(panel.background = element_rect(fill = 'white', colour = 'white')) +
	theme(plot.title = element_text(size=22)) +
	theme(plot.caption = element_text(size=16)) +
	theme(plot.title = element_text(hjust = 0.5)) +
	theme(plot.subtitle = element_text(size=16, hjust = 0.5, vjust=-10)) +
	theme(axis.text=element_text(size=16),
		  axis.title=element_text(size=16)) +	
	labs(x = "Log GDP Per Capita, PPP",
		 y = "0-5 Year Old Deaths/1000 Born") +
	labs(caption = "By @toddrjones. Source: Gapminder. ") +
	transition_time(year) +	
	labs(subtitle = paste('Year: {frame_time}'))

anim_save("output/cmort.gif", map, end_pause=12, width = 600, height = 550, duration=22, nframes=220)





