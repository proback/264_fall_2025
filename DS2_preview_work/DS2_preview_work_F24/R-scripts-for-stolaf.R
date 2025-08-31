
###############
###############
# CENSUS DATA
###############
###############

# wrangling, pipes
library(tidyverse)

# Basic maps
library(mapview)

# For pulling census data and shapefiles
library(tidycensus)

# https://walker-data.com/tidycensus/articles/basic-usage.html
v22 <- load_variables(2022, "acs5", cache = TRUE)
View(v22)
county_pops_by_race <- get_acs(geography = "county", survey="acs5",
                       year = 2022, variable = c(total_population = "B02001_001", 
                                                 black = "B02001_003", 
                                                 white = "B02001_002", 
                                                 aian = "B02001_004", 
                                                 asian = "B02001_005", 
                                                 nhpi = "B02001_006", 
                                                 latino_anyrace = "B03003_003",
                                                 medianincome = "B19019_001"),
                       geometry = T)
glimpse(county_pops_by_race)

county_pops_long <- county_pops_by_race %>%
  select(-c(moe)) %>%
  pivot_wider(names_from = "variable", values_from = "estimate") %>% 
  # glimpse() #%>%
  mutate(pop = total_population,
         black_pop = black/pop,
         white_pop = white/pop,
         aian_pop = aian/pop,
         asian_pop = asian/pop,
         nhpi_pop = nhpi/pop,
         latino_pop = latino_anyrace/pop) %>%
  mutate(fips = as.numeric(GEOID)) %>%
  select(NAME, GEOID, fips, pop, black_pop,white_pop,
         aian_pop,asian_pop, latino_pop, medianincome)
glimpse(county_pops_long)

# US map of Black pop
mapview(county_pops_long, 
        zcol="black_pop", lwd = 0.25,
        alpha.regions = 1)

# Minnesota map of Black pop
county_pops_long %>% 
  filter(str_detect(NAME, "Minnesota")) %>% 
  glimpse() %>% 
  mapview(zcol="black_pop", lwd = 0.25,
        alpha.regions = 1)



##################
##################
# MERGE AND MODEL
##################
##################

# wrangling, pipes
library(tidyverse)

# Basic maps
library(mapview)

airbnb = read.csv("housing/airbnbData_full.csv") 
glimpse(airbnb)

# Merge in neighborhood using point in polygon
# https://bostonopendata-boston.opendata.arcgis.com/search?q=neighborhood

library(sf)
boston = sf::st_read("housing/BPDA_Neighborhood_Boundaries.geojson") %>% 
  sf::st_transform(crs = 4326)
mapview(boston)
glimpse(boston)
# mapview(boston)
camb = sf::st_read("housing/BOUNDARY_CDDNeighborhoods.shp.zip") %>% 
  st_transform(crs = 4326)
mapview(camb)
# mapview(camb)
som = st_read("housing/somerville_Neighborhoods/Neighborhoods.shp") %>% 
  st_transform(crs = 4326)
mapview(som)

glimpse(airbnb)
airbnb = airbnb %>% 
  st_as_sf(coords = c("Long", "Lat"), 
           crs = st_crs(4326)) 
mapview(airbnb)

sf_use_s2(FALSE)
boston_props = airbnb %>% 
  st_intersection(boston) # 644
glimpse(boston_props)
mapview(boston_props)

camb_props = airbnb %>% 
  st_intersection(camb) # 770
glimpse(camb_props)
# mapview(camb_props)

som_props = airbnb %>% 
  st_intersection(som) # 260
glimpse(som_props)
mapview(som_props)

# Merge three cities, harmonize hood names, and add city names
all_props = bind_rows(boston_props %>% 
                        mutate(hood = name,
                               city = "Boston"), 
                      camb_props %>% 
                        mutate(hood = NAME,
                               city = "Cambridge"), 
                      som_props %>% 
                        mutate(hood = NBHD,
                               city = "Somerville")) %>% 
  filter(Price < 2500) # remove 1 insane outlier
glimpse(all_props)

all_props %>% 
  select(ListingID, Price, city) %>%
  # glimpse()
  mapview(zcol="city",
        cex = "Price")

# https://rpubs.com/crazyhottommy/reorder-boxplot
ggplot(all_props, aes(color=city, y=reorder(hood, Price, fun=median, .desc=T), x=Price)) + 
  # facet_grid(~city) +
  geom_boxplot() 

ggplot(all_props, aes(x = Price)) +
  geom_histogram(bins = 100, color = "black") 
 
all_props_model = all_props %>% 
  select(ListingID, Price, A_AC:A_Wheelchair, hood, city, 
         S_Accomodates, S_Bathrooms, S_Bedrooms, S_NumBeds) %>% 
  st_drop_geometry() %>% 
  mutate(S_Accomodates = as.numeric(S_Accomodates),
    S_Bathrooms = as.numeric(S_Bathrooms),
    S_Bedrooms = as.numeric(S_Bedrooms),
    S_NumBeds = as.numeric(S_NumBeds)) %>%
  drop_na()  
glimpse(all_props_model)

library(ggpmisc)
ggplot(all_props_model, aes(y=Price, x=S_Accomodates)) +
  geom_point() + geom_smooth(method="lm") + stat_poly_eq()
ggplot(all_props_model, aes(y=Price, x=S_NumBeds)) +
  geom_point() + geom_smooth(method="lm") + stat_poly_eq()
ggplot(all_props_model, aes(y=Price, x=S_Bedrooms)) +
  geom_point() + geom_smooth(method="lm") + stat_poly_eq()

all_props_model_fit = lm(Price ~ .,
 data=all_props_model)
summary(all_props_model_fit)

library(sjPlot)
plot_model(all_props_model_fit,
           title = "",
           show.p = TRUE,
           sort.est = TRUE)

###############
###############
# NETVIZ
###############
###############



# text analysis
library(tidytext)

# net viz
library(devtools)
# devtools::install_github("JohnCoene/twinetverse")
library(twinetverse)
library(graphTweets)
library(tidygraph)
library(igraph)
library(ggraph)


# Restart from saved RDS file 
blm_tweets_top300_42121_5pm_cleaned <- readRDS("netviz/blm_tweets_top300_42121_5pm_cleaned.RDS")
glimpse(blm_tweets_top300_42121_5pm_cleaned)

remove_news <- c("CNN","CBSNews","ajplus","washingtonpost","cnnbrk", 
                 "PalmerReport", "MSNBC","nowthisnews", "BreitbartNews", 
                 "AP", "NPR", "NBCNews", "nytimes", "ABC")

############################################################
# Visualize retweet network of R/D elites on Twitter  
############################################################

# A visualization of the retweet network of influential Twitter accounts 
# in the hours following the Derek Chauvin verdict announcement. Politicians, 
# journalists and BLM activists like Kamala Harris, Joy Reid and Shaun King 
# are colored blue while right-wing media personalities and conservative pundits 
# like Brit Hume, Candace Owens and Ann Coulter are colored red. Nodes are sized by retweets.
# Had problems coloring nodes!

elites_floydkeyword_after_verdict <- blm_tweets_top300_42121_5pm_cleaned %>%
  filter(!screen_name %in% remove_news) %>%
  filter(created_at > as.POSIXct("2021-04-20 21:00:00", tz="UTC")) %>%
  #filter(str_detect(text, "Chauvin|chauvin|CHAUVIN|verdict|Verdict|trial|Trial|Floyd|FLOYD|floyd")) %>%
  #filter(party == "Republican") %>%
  dplyr::select(retweet_name, name, status_id, created_at)
glimpse(elites_floydkeyword_after_verdict)

elites_floydkeyword_after_verdictv1 <- elites_floydkeyword_after_verdict %>% 
  gt_edges(source = name, target = retweet_name, tl=F) %>% # get edges
  gt_nodes() # get nodes
glimpse(elites_floydkeyword_after_verdictv1)

# Convert to list
elites_floydkeyword_after_verdictv1 <- elites_floydkeyword_after_verdictv1 %>% 
  gt_collect()
elites_floydkeyword_after_verdictv1

c(edges_20062463v1, nodes_20062463v1) %<-% elites_floydkeyword_after_verdictv1
nodes_20062463v1 <- nodes2sg(nodes_20062463v1)
edges_20062463v1 <- edges2sg(edges_20062463v1)

sigmajs() %>% 
  sg_nodes(nodes_20062463v1, id, label, size) %>% #add label to see want to see node labels
  sg_edges(edges_20062463v1, id, source, target) %>% 
  sg_layout(layout = igraph::layout_with_kk) %>% # layout_with_graphopt layout_components layout_with_kk) %>%  # https://igraph.org/r/doc/layout_with_kk.html
  sg_cluster(
    colors = c(
      "#e6e6e6"
    )
  ) %>% 
  sg_settings(
    minNodeSize = 1,
    maxNodeSize = 20,
    edgeColor = "default",
    defaultEdgeColor = "#E8E8E8"
  ) %>%
  sg_drag_nodes() 



############################################################
# BETTER VERSION WITH CLUSTERS COLORED BY PARTISANSHIP
# https://rpubs.com/Argaadya/sna_covid
############################################################
# Highly recommend reading https://kateto.net/networks-r-igraph

mention_clean <- function(x){
  if(grepl(",",x) == TRUE){
    gsub('^.|[^[:alnum:][:blank:]_,?&/\\-]',"",x)
  } else{
    x
  }
}

edge_nn <- elites_floydkeyword_after_verdict %>% 
  select(screen_name,is_retweet,mentions_screen_name) %>%
  mutate(mentions_screen_name = sapply(mentions_screen_name, mention_clean)) %>% 
  filter(mentions_screen_name != "NA")

edge_nn <- edge_nn %>% 
  mutate(type = ifelse(is_retweet == "TRUE", "retweet", "mention"))

edge_nn <- edge_nn %>% 
  select(screen_name,mentions_screen_name,type) %>%
  separate_rows(mentions_screen_name,sep = ",") %>% 
  setNames(c("from","to","type")) %>% 
  count(from,to,type)

edge_nn %>% head()

edge_nn %>% 
  count(type, name = "frequency")

nodes_nn <- data.frame(V = unique(c(edge_nn$from,edge_nn$to)),
                       stringsAsFactors = F)

tail(nodes_nn)

network_nn <- graph_from_data_frame(d = edge_nn, # Edge
                                    vertices = nodes_nn, # Vertice
                                    directed = F # Is directed Graph?
) %>%
  as_tbl_graph() # Transform graph to table

network_nn



set.seed(123)
network_nn <- network_nn %>% 
  activate(nodes) %>%
  mutate(community = group_louvain(), # Build community through clustering algorithm
         degree = centrality_degree(), # Calculate degree centrality
         between = centrality_betweenness(), # Calculate betweeness centrality
         closeness = centrality_closeness(), # Calculate closeness centrality
         eigen = centrality_eigen()) %>% # Calculate eigen centrality
  activate(edges) %>% 
  filter(!edge_is_loop()) # Remove loop edges
network_nn

network_act_df <- as.data.frame(network_nn %>% activate(nodes))
network_act_df %>% head()

network_act_df_comm <- network_act_df %>%
  filter(community %in% 1:8) 
table(network_act_df$community)

important_person <- network_act_df %>% 
  filter(community %in% 1:8) %>% 
  select(-community) %>% 
  pivot_longer(-name, names_to = "measures", values_to = "values") %>% 
  group_by(measures) %>% 
  arrange(desc(values)) %>% 
  slice(1:8) %>% 
  ungroup() %>% 
  distinct(name) %>% 
  pull(name) 

# kamala <- c("KamalaHarris")

network_nn %>%
  activate(nodes) %>%
  mutate(ids = row_number(),
         community = as.character(community)) %>%
  filter(community %in% 1:8) %>% 
  arrange(community,ids) %>% 
  mutate(node_label = ifelse(name %in% kamala, name, "")) %>%
  ggraph(layout = "kk") + #fr
  geom_edge_link(alpha = 0.3, color="#e6e6e6") + #aes(color = "#e6e6e6")) + # color = type
  geom_node_point(aes(size = degree, fill = community), 
                  shape = 21, alpha = 0.7, color = "grey30") +
  geom_node_label(aes(label = node_label), repel = T, alpha = 0.5) +
  scale_fill_manual(values = c("firebrick", "blue4",  "blue4",
                               "firebrick", "blue4", "blue4", "blue4", "blue4" 
  )) +
  guides(size = F) +
  # labs(title = "Top 5 Community of #COVID19", 
  #      color = "Interaction", fill = "Community") +
  theme_void() +
  theme(legend.position = "none")



