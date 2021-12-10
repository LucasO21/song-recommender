# FUNCTIONS FOR SHINY APP ----

# Libraries ----
library(tidyverse)
library(knitr)

# Load Data ----
songs_tbl <- readRDS("../Data/prepared_tbl.rds")

set.seed(100)
rand_ids <- sample(1:451, 20, replace=FALSE)

songs_tbl <- songs_tbl %>% 
    mutate(ref_song = ifelse(track_id %in% rand_ids, "Yes", "No")) %>% 
    mutate(track_artist = paste(track_name, artist_name, sep = " - "))

data_tbl[data_tbl$ref_song == "Yes",]$track_artist[1]

.data_tbl <- songs_tbl
.track_artist = "Don't Trust Geminis - Bella Alubo"


func_song_snippet <- function(.data_tbl, .track_artist){
    
    .track_artist_expr <- rlang::enquo(.track_artist)
    
    output <- .data_tbl %>% 
        filter(track_artist == .track_artist) %>% 
        pull(track_external_urls_spotify) %>% 
        str_replace_all("track", "embed/track")
    
    return(output)
    
}

.data_tbl %>% func_song_snippet(.track_artist = .track_artist)



dump(list   = c("func_song_snippet"),
     file   = "script_functions.R",
     append = FALSE)

text

url <- text
as_string(url)

enquo(url)
quo(url)
