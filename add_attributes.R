#### INSTALLATION BLOCK ####
install.packages("igraph")   
install.packages("statnet")  #installs ergm, network, and sna
install.packages("snahelper")
install.packages("netUtils")
install.packages("ggraph")
install.packages("backbone")
install.packages("netrankr")
install.packages("signnet")
install.packages("egor")
install.packages("intergraph")
install.packages("graphlayouts")
install.packages("visNetwork")
install.packages("patchwork")
install.packages("edgebundle")
install.packages("ggplot2")
install.packages("gganimate")
install.packages("ggforce")
#install.packages("rsiena")

install.packages("pacman")

#### LOAD PACKAGES ####

pacman::p_load("igraph", "jsonlite")

#### DATA ####

data <- fromJSON("congress_network/congress_network_data.json", simplifyVector = FALSE)


inList       <- data[[1]]$inList
inWeight     <- data[[1]]$inWeight
outList      <- data[[1]]$outList
outWeight    <- data[[1]]$outWeight
usernameList <- data[[1]]$usernameList

edges <- do.call(
  rbind,
  lapply(seq_along(outList), function(i) {
    if (length(outList[[i]]) == 0) return(NULL)
    
    data.frame(
      from = i - 1,                    
      to = unlist(outList[[i]]),
      weight = unlist(outWeight[[i]])
    )
  })
)

head(edges)

g <- graph_from_data_frame(
  d = edges,
  directed = TRUE,
  vertices = data.frame(
    name = seq_along(usernameList) - 1,
    username = unlist(usernameList)
  )
)




#### Attributes ####

V(g)$username # username
V(g)$name # id

pacman::p_load("yaml", "dplyr", "purrr", "tibble", "stringr")

`%||%` <- function(x, y) if (is.null(x)) y else x

leg_url <- "https://raw.githubusercontent.com/unitedstates/congress-legislators/main/legislators-current.yaml"
soc_url <- "https://raw.githubusercontent.com/unitedstates/congress-legislators/main/legislators-social-media.yaml"

leg <- yaml::read_yaml(leg_url)
soc <- yaml::read_yaml(soc_url)

members <- map_dfr(leg, function(x) {
  terms <- x$terms
  
  map_dfr(terms, function(term) {
    tibble(
      bioguide_id = x$id$bioguide %||% NA_character_,
      first_name = x$name$first %||% NA_character_,
      last_name = x$name$last %||% NA_character_,
      full_name = paste(x$name$first %||% "", x$name$last %||% ""),
      type = term$type %||% NA_character_,
      state = term$state %||% NA_character_,
      district = term$district %||% NA_integer_,
      party = term$party %||% NA_character_,
      term_start = as.Date(term$start),
      term_end = as.Date(term$end)
    )
  })
})

social <- map_dfr(soc, function(x) {
  tibble(
    bioguide_id = x$id$bioguide %||% NA_character_,
    twitter = x$social$twitter %||% NA_character_
  )
})

members <- members %>%
  left_join(social, by = "bioguide_id")

members_117 <- members %>%
  filter(
    term_start < as.Date("2023-01-03"),
    term_end > as.Date("2021-01-03")
  ) %>%
  mutate(
    chamber = case_when(
      type == "sen" ~ "Senate",
      type == "rep" ~ "House",
      TRUE ~ NA_character_
    ),
    twitter_clean = str_to_lower(str_remove(twitter, "^@"))
  )

members_117_one <- members_117 %>%
  filter(!is.na(twitter_clean), twitter_clean != "") %>%
  arrange(twitter_clean, term_start) %>%
  group_by(twitter_clean) %>%
  summarise(
    bioguide_id = first(bioguide_id),
    full_name = first(full_name),
    chamber = first(chamber),
    party = first(party),
    state = first(state),
    district = first(district),
    term_start = min(term_start, na.rm = TRUE),
    term_end = max(term_end, na.rm = TRUE),
    .groups = "drop"
  )

graph_vertices <- tibble(
  name = V(g)$name,
  username = V(g)$username,
  twitter_clean = str_to_lower(str_remove(V(g)$username, "^@"))
)

vertex_metadata <- graph_vertices %>%
  left_join(members_117_one, by = "twitter_clean")

V(g)$full_name <- vertex_metadata$full_name
V(g)$chamber   <- vertex_metadata$chamber
V(g)$party     <- vertex_metadata$party
V(g)$state     <- vertex_metadata$state
V(g)$district  <- vertex_metadata$district

#### FULL DATASET ####

V(g)$username
V(g)$full_name
V(g)$chamber
V(g)$party
V(g)$state
V(g)$district

E(g)$weight
