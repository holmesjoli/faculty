library(magrittr)

sapply(list.files("R", full.names = TRUE, recursive = TRUE), source, .GlobalEnv)

uw <- faculty.uw()

um <- faculty.um()

l <- list(uw, um) %>% 
  purrr::transpose()

id <- do.call(rbind, l[["id"]])

specializations <- do.call(rbind, l[["specializations"]])

research_area <- do.call(rbind, l[["research_area"]])

bio <- do.call(rbind, l[["bio"]])

edu <- do.call(rbind, l[["edu"]])

pub <- do.call(rbind, l[["pub"]])