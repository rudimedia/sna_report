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

pacman::p_load("igraph", "dplyr", "purrr", "tibble", "stringr")

#### DATA ####

data <- jsonlite::fromJSON("congress_network/congress_network_data.json", simplifyVector = FALSE)
voteview <- readr::read_csv("HS117_members.csv")

leg <- yaml::read_yaml("https://raw.githubusercontent.com/unitedstates/congress-legislators/main/legislators-current.yaml")
soc <- yaml::read_yaml("https://raw.githubusercontent.com/unitedstates/congress-legislators/main/legislators-social-media.yaml")

inList <- data[[1]]$inList
inWeight <- data[[1]]$inWeight
outList <- data[[1]]$outList
outWeight <- data[[1]]$outWeight
usernameList <- data[[1]]$usernameList

edges <- do.call(
  rbind, lapply(seq_along(outList), function(i) {
    if (length(outList[[i]]) == 0) return(NULL)
    data.frame(
      from = i - 1,                    
      to = unlist(outList[[i]]),
      weight = unlist(outWeight[[i]]))}))

g <- graph_from_data_frame(d = edges, directed = TRUE, vertices = data.frame(name = seq_along(usernameList) - 1, username = unlist(usernameList)))

#### Attributes ####

# V(g)$username # username
# V(g)$name # id

`%||%` <- function(x, y) if (is.null(x)) y else x

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
  filter(term_start < as.Date("2023-01-03"), term_end > as.Date("2021-01-03")) %>%
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
  twitter_clean = str_to_lower(str_remove(V(g)$username, "^@")))

vertex_metadata <- graph_vertices %>%
  left_join(members_117_one, by = "twitter_clean")

V(g)$full_name <- vertex_metadata$full_name
V(g)$chamber <- vertex_metadata$chamber
V(g)$party <- vertex_metadata$party
V(g)$state <- vertex_metadata$state
V(g)$district <- vertex_metadata$district
V(g)$bioguide_id <- vertex_metadata$bioguide_id

# IDEOLOGY
nodes <- tibble(idx = seq_len(vcount(g)), bioguide_id  = V(g)$bioguide_id) %>%
  left_join(voteview, by = "bioguide_id")

V(g)$icpsr <- nodes$icpsr
V(g)$nominate_dim1 <- nodes$nominate_dim1
V(g)$nominate_dim2 <- nodes$nominate_dim2
V(g)$born <- nodes$born
V(g)$age24 <- (2024 - nodes$born) 

#### DESCRIPTIVES ####

# size
vcount(g)          
ecount(g)          
edge_density(g)

# degree
summary(degree(g, mode = "in"))
summary(degree(g, mode = "out"))

# weighted versions (strength)
summary(strength(g, mode = "in"))
summary(strength(g, mode = "out"))

# degree distrivution
par(mfrow = c(1, 2))
hist(deg_in,  main = "In-degree distribution",  xlab = "In-degree")
hist(deg_out, main = "Out-degree distribution", xlab = "Out-degree")

# Reciprocity: do they reciprocate retweets? -> 0.46
reciprocity(g)

# Transitivity: If A retweets B, and B retweets C, does A retweet C? --> moderate
transitivity(g, type = "global")
transitivity(g, type = "average")  # average local clustering coefficient

# Assortativity: do high degree members retweet each other? --> no
assortativity_degree(g, directed = TRUE)



### HOMOPHILY by PARTY ####
el <- igraph::as_data_frame(g, what = "edges")
el$from_party <- V(g)$party[match(el$from, V(g)$name)]
el$to_party   <- V(g)$party[match(el$to, V(g)$name)]
mix <- xtabs(weight ~ from_party + to_party, data = el)
mix_prop <- mix / rowSums(mix)
round(mix_prop, 3)

# by chamber
el$from_chamber <- V(g)$chamber[match(el$from, V(g)$name)]

for (ch in c("House", "Senate")) {
  el_ch <- el[!is.na(el$from_chamber) & el$from_chamber == ch, ]
  mix   <- xtabs(weight ~ from_party + to_party, data = el_ch)
  mix_prop <- mix / rowSums(mix)
  print(ch)
  print(round(mix_prop, 3))
}

#### Assortativity for continous measures ####
assortativity(g, V(g)$nominate_dim1, directed = TRUE) # L/R
assortativity(g, V(g)$nominate_dim2, directed = TRUE) # social/racial issues
assortativity(g, V(g)$age24, directed = TRUE) # age -> basically none

#### Homophily higher for older members? ####
el$same_party <- !is.na(el$from_party) & 
  !is.na(el$to_party) & 
  el$from_party == el$to_party

# filter NAs
el_known <- el %>% filter(!is.na(from_party), !is.na(to_party))
# exclude Independents
el_known <- el_known[el_known$from_party != "Independent" & el_known$to_party != "Independent", ]

# per-node homophily score
homophily_score <- el_known %>%
  group_by(from) %>%
  summarise(total_w = sum(weight), ingroup_w = sum(weight[same_party]), h_score   = ingroup_w / total_w)

homophily_score$age <- V(g)$age24[match(homophily_score$from, V(g)$name)]
homophily_score$party <- V(g)$party[match(homophily_score$from, V(g)$name)]

# correlation
cor(homophily_score$h_score, homophily_score$age, use = "complete.obs")

# by party --> democrats weakly positive (+0.04), republicans kind of substantially negative (-0.128)
homophily_score %>%
  group_by(party) %>%
  summarise(cor_age = cor(h_score, age, use = "complete.obs"))

# scatterplot by party
ggplot(homophily_score, aes(x = age, y = h_score, colour = party)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Age", y = "Within-party retweet share", 
       title = "Homophily score by age and party") +
  theme_minimal()
