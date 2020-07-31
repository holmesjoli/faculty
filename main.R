library(magrittr)

sapply(list.files("R", full.names = TRUE, recursive = TRUE), source, .GlobalEnv)

uw <- faculty.uw()

um <- faculty.um()

illinois <- faculty.illinois()

l <- list(uw, um, illinois) %>% 
  purrr::transpose()

id <- do.call(rbind, l[["id"]]) %>% 
  parse.id() %>%
  dplyr::filter(phd_adv %in% c("Yes", NA)) %>% 
  dplyr::filter(assistant | associate) %>% 
  dplyr::filter(adjunct == FALSE & emeritus == FALSE & lecturer == FALSE & research_fellow == FALSE)

potential_adv <- id$email

write.csv(id, "./data/id.csv", row.names = FALSE)

specializations <- do.call(rbind, l[["specializations"]]) %>% 
  dplyr::filter(email %in% potential_adv)
write.csv(specializations, "./data/specializations.csv", row.names = FALSE)

research_area <- do.call(rbind, l[["research_area"]]) %>% 
  parse.research_area() %>% 
  dplyr::filter(email %in% potential_adv)
write.csv(research_area, "./data/research_area.csv", row.names = FALSE)

bio <- do.call(rbind, l[["bio"]]) %>% 
  dplyr::filter(email %in% potential_adv)
write.csv(bio, "./data/biography.csv", row.names = FALSE)

edu <- do.call(rbind, l[["edu"]]) %>% 
  dplyr::filter(email %in% potential_adv) %>% 
  parse.edu() %>% 
  dplyr::filter(degree >= 1)
write.csv(edu, "./data/education.csv", row.names = FALSE)

pub <- do.call(rbind, l[["pub"]]) %>% 
  dplyr::filter(email %in% potential_adv)
write.csv(pub, "./data/publications.csv", row.names = FALSE)
