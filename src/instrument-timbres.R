get_instrument_amplitudes <- function(
  id, 
  method = mean, 
  time_window = c(0, 1),
  num_harmonics = 10
) {
  json <- jsonlite::read_json("input/instrument-timbres.json")
  stopifnot(
    num_harmonics <= json$NH,
    id %in% names(json)
  )
  sample_rate <- json$fs
  indices <- seq
  
  json[[id]][seq_len(num_harmonics)] %>% 
    map(unlist) %>% 
    map_dbl(function(.) {
      tibble(
        index = seq_along(.),
        amplitude = .,
        time = (index - 1) / sample_rate
      ) %>% 
        filter(
          time_window[1] <= time,
          time_window[2] >= time
        ) %>% 
        pull(amplitude) %>% 
        method()
    }) %>% 
    {. / max(.)}
}