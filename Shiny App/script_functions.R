func_song_snippet <-
function(.data_tbl, .track_artist){
    
    .track_artist_expr <- rlang::enquo(.track_artist)
    
    output <- .data_tbl %>% 
        filter(track_artist == .track_artist) %>% 
        pull(track_external_urls_spotify) %>% 
        str_replace_all("track", "embed/track")
    
    snippet <- knitr::include_url(output, height = "80")
    
    return(snippet)
    
}
