library(data.table) # fast data import
library(tidyverse) # data manipulation

library(plotly) # interactive visualizations
library(janitor) # data manipulation
library(stringr) # character class data manipulation
library(treemap) # tree map visualization
library(igraph)
library(gridExtra)
library(ggraph)
airport <- read_csv("../Documents/input/airports-train-stations-and-ferry-terminals/airports-extended.csv", 
col_names = F)
names(airport) <- c("Airpot_ID", "Airport_Name", "City", "Country", "IATA", 
 "ICAO", "Latitude", "Longitude", "Altitude", "Timezone", 
 "DST", "Tz", "Type", "Source")
airport <- airport %>% 
 filter(Type == "airport")
airline <- read_csv("../Documents/input/airline-database/airlines.csv") %>% 
 clean_names()
route <- read_csv("../Documents/input/flight-route-database/routes.csv") %>% 
 clean_names()
names(route)[5] <- "destination_airport"
countries <- read_csv("../Documents/input/countries-of-the-world/countries of the world.csv")
airport %>% 
 head(5) %>% 
 DT::datatable(options = list(
lengthMenu = c(5,3,1)
 ))
## Dataset 2
```{r}
airline %>% 
 head(5) %>% 
 DT::datatable(options = list(
 lengthMenu = c(5,3,1)
 ))
```
## Data No.3
```{r}
route %>% 
 head(5) %>% 
 DT::datatable(options = list(
 lengthMenu = c(5,3,1)
 ))
```
# Analysis
## Global Airports Distribution
```{r warning = FALSE, message = FALSE}
geo <- list(
 scope = "world",
 projection = list(type = "orthographic"),
 showland = TRUE,
 resolution = 100,
 landcolor = toRGB("gray90"),
 countrycolor = toRGB("gray80"),
 oceancolor = toRGB("lightsteelblue2"),
 showocean = TRUE
)
plot_geo(locationmode = "Greenwich") %>%
 add_markers(data = airport %>% 
 filter(Type == "airport"),
 x = ~Longitude,
 y = ~Latitude,
 text = ~paste('Airport: ', Airport_Name),
 alpha = .5, color = "red") %>%
 layout(
 title = "Global Airports",
 geo = geo,
 showlegend = FALSE
 )
```
```{r}
print(paste("There are", airport %>% 
 filter(Type == "airport") %>% 
 nrow(), 
 "airports around the world."))
```
## Global Airline route
```{r}
route <- route %>% mutate(id = rownames(route))
route <- route %>% gather('source_airport', 'destination_airport', key = "Airport_type", value = 
"Airport")
gloabal.flight.route <- merge(route, airport %>% select(Airport_Name, IATA, Latitude, 
Longitude, Country, City),
 by.x = "Airport", by.y = "IATA")
```
```{r warning = FALSE, message = FALSE}
world.map <- map_data ("world")
world.map <- world.map %>% 
filter(region != "Antarctica")
ggplot() + 
 geom_map(data=world.map, map=world.map,
 aes(x=long, y=lat, group=group, map_id=region),
 fill="white", colour="black") +
 geom_point(data = gloabal.flight.route, 
 aes(x = Longitude, y = Latitude), 
 size = .1, alpha = .5, colour = "red") +
 geom_line(data = gloabal.flight.route, 
 aes(x = Longitude, y = Latitude, group = id), 
 alpha = 0.05, colour = "red") +
 labs(title = "Global Airline Routes")
```
```{r warning = FALSE, message = FALSE, fig.width = 12, fig.height = 7}
ggplot() + 
 geom_map(data=world.map, map=world.map,
 aes(x=long, y=lat, group=group, map_id=region),
 fill="white", colour="grey") + 
 geom_point(data = airport %>% 
 filter(Altitude >= 5000),
 aes(x = Longitude, y = Latitude, colour = Altitude), 
 size = .7) +
 labs(title = "Airports located over 5,000 feet altitude") +
 ylim(-60, 90) +
 theme(legend.position = c(.1, .25))
```
```{r}
print(paste(airport %>% 
 filter(Altitude >= 5000) %>% 
 nrow(), 
 "airports are located over 5,000 feet altitude."))
```
## Which Country has the most Airports?
```{r}
connection.route <- route %>% 
 spread(key = Airport_type, value = Airport) %>% 
 select(destination_airport, source_airport, id)
airport.country <- airport %>% 
 select(City, Country, IATA)
flight.connection <- merge(connection.route, airport.country, by.x = "source_airport", by.y = 
"IATA")
names(flight.connection)[4:5] <- c("source.City", "source.Country")
flight.connection <- merge(flight.connection, airport.country, by.x = "destination_airport", by.y 
= "IATA")
names(flight.connection)[6:7] <- c("destination.City", "destination.Country")
flight.connection <- flight.connection %>% 
 select(id, contains("source"), contains("destination"))
```
```{r warning = FALSE, message = FALSE, fig.width = 12, fig.height = 7}
data.frame(table(airport$Country)) %>% 
 arrange(desc(Freq)) %>% 
 head(20) %>% 
 ggplot(aes(x = reorder(Var1, -Freq), y = Freq, fill = Var1, label = Freq)) + 
 geom_bar(stat = "identity", show.legend = F) +
 labs(title = "Top 20 Countries that has most Airports", 
 x = "Country", y = "The number of Airports") +
 geom_label(angle = 45, show.legend = F) +
 theme(axis.text.x = element_text(angle = 40, size = 15))
### Treemap Visualization
```{r fig.width = 12, fig.height = 7}
treemap(data.frame(table(airport$Country)),
 index="Var1",
 vSize="Freq",
 type="index",
 title = "Overall Number of Airport owned by each Nation")
```
## Which Country has the most Airlines?
```{r fig.width = 12, fig.height = 7}
data.frame(table(airport$Country)) %>% 
 arrange(desc(Freq)) %>% head(20) %>%
 ggplot(aes(x = reorder(Var1, -Freq), y = Freq, 
 fill = Var1, label = Freq)) + 
 geom_bar(stat = "identity", show.legend = F) +
 geom_label(show.legend = F) +
 theme(axis.text.x = element_text(angle = 40, size = 15)) +
 labs(x = "Country", y = "The number of Airlines", 
 title = "Top 20 Countries that have most airlines")
```
## anti-social countries 
```{r}
NK.airport <- airport %>% filter(Country == "North Korea")
NK.flight.connection <- flight.connection %>% 
 filter(source.Country == "North Korea" | destination.Country == "North Korea")
NK.gloabal.flight.route.id <-
 gloabal.flight.route %>% 
 filter(Country == "North Korea") %>% select(id)
NK.gloabal.flight.route.id <- NK.gloabal.flight.route.id$id %>% as.vector()
NK.gloabal.flight.route.id <-
 gloabal.flight.route %>% filter(id %in% NK.gloabal.flight.route.id)
```
```{r warning = FALSE, message = FALSE, fig.width = 10}
NorthKorea.ggmap <- ggplot() + 
 geom_map(data=world.map, map=world.map,
 aes(x=long, y=lat, group=group, map_id=region),
 fill="white", colour="black") +
 geom_point(data = NK.gloabal.flight.route.id, 
 aes(x = Longitude, y = Latitude), colour = "red") +
 geom_point(data = NK.airport, 
 aes(x = Longitude, y = Latitude), colour = "red") +
 geom_line(data = NK.gloabal.flight.route.id, 
 aes(x = Longitude, y = Latitude, group = id), colour = "red") + 
 xlim(100, 140) + ylim(0, 45) +
 labs(title = "Airports & International Airlines from/to/in North Korea") +
 coord_fixed(ratio = 1.1)
Flight.Country.Connection <- NK.flight.connection %>% 
 select(contains("Country"), id)
names(Flight.Country.Connection) <- c("From", "To", "id")
Flight.Country.Connection <- Flight.Country.Connection %>% 
 mutate(Combination = paste0(From, "-", To))
Flight.Country.Connection <- Flight.Country.Connection %>% 
 group_by(Combination) %>% 
 mutate(Weight = NROW(Combination)) %>% 
 arrange(-Weight) %>% 
 ungroup()
Flight.Country.Connection <-
Flight.Country.Connection[!duplicated(Flight.Country.Connection$Combination),] %>% 
 select(-id, -Combination)
Flight.Country.graph <- graph_from_data_frame(Flight.Country.Connection, directed = FALSE)
Flight.Country.graph$name <- "Flight Country Network"
V(Flight.Country.graph)$id <- 1:vcount(Flight.Country.graph)
NW.Plot <- ggraph(Flight.Country.graph, layout = "kk") +
 geom_edge_link(aes(alpha = Weight), 
 colour = "red") +
 geom_node_point(size = 5, colour = "red") +
 geom_node_text(aes(label = name), repel = TRUE, size = 7) +
 labs(title = "Flight Country Network", x = "", y = "") +
 theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
 axis.line = element_blank(),
 axis.text.x=element_blank(), axis.text.y=element_blank())
grid.arrange(NorthKorea.ggmap, NW.Plot, ncol=2)
```
### Airport Information that have flight access to North Korea
```{r}
NK.gloabal.flight.route.id %>% 
 filter(Country != "North Korea") %>% 
 select(Airport_Name, Country, City, Latitude, Longitude) %>% 
 distinct(City, .keep_all = T) %>% 
 DT::datatable(options = list(
 lengthMenu = c(4,1)
 ))
```
### Asian Countries' Flights Network
```{r fig.width = 18, fig.height = 13}
country.connection <- flight.connection %>% 
 select(contains("Country")) %>% 
 mutate(Link = paste0(source.Country, "-", destination.Country))
country.connection <- country.connection[!duplicated(country.connection$Link),]
names(country.connection) <- c("from", "to", "Link" )
#### Selecting Asian Countries
Country.list <- countries %>% 
 select(Country, Region)
Country.list$Country <- as.character(Country.list$Country)
Asian.Country.list <- Country.list %>% 
 arrange(Region) %>%
 head(28)
Asian.Country <- Asian.Country.list$Country
Asian.Country <- gsub(pattern = "\\, ", replacement = "", Asian.Country) %>% 
 gsub(pattern = " ", replacement = "", Asian.Country) %>% 
 gsub(pattern = "KoreaNorth", replacement = "North Korea", Asian.Country) %>% 
 gsub(pattern = "KoreaSouth", replacement = "South Korea", Asian.Country)
country.connection <- country.connection %>% 
 filter(from %in% Asian.Country & to %in% Asian.Country) %>% 
 select(-Link)
country.connection <- country.connection %>% 
 mutate(TF = str_detect(from, to)) %>% 
 filter(TF == "FALSE") %>% 
 select(-TF)
g <- graph_from_data_frame(country.connection, directed = TRUE)
V(g)$color <- ifelse(
 V(g)$name == "North Korea", "red", "yellow"
)
plot(g, layout = layout_with_dh(g),
 edge.arrow.size=0.8,
 vertex.size = 17, vertex.label.cex = 2)
```
