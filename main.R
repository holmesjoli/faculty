library(magrittr)

sapply(list.files("R", full.names = TRUE, recursive = TRUE), source, .GlobalEnv)

config <- yaml::read_yaml("config.yaml")

l <- list(do.call(collect_info_main, args = create_cls(config$uw)),
          do.call(collect_info_main, args = create_cls(config$um)),
          do.call(collect_info_main, args = create_cls(config$illinois)),
          do.call(collect_info_main, args = create_cls(config$berkeley)),
          do.call(collect_info_main, args = create_cls(config$ut)), 
          do.call(collect_info_main, args = create_cls(config$unc))
          # do.call(collect_info, args = create_cls(config$syracuse))
          ) %>% 
  purrr::transpose()

id <- do.call(rbind, l[["id"]]) %>% 
  parse.id() %>%
  dplyr::filter(phd_adv %in% c("Yes", NA)) %>% 
  dplyr::filter(assistant | associate) %>% 
  dplyr::filter(adjunct == FALSE & emeritus == FALSE & lecturer == FALSE & research_fellow == FALSE)

potential_adv <- id$name

write.csv(id, "./data/id.csv", row.names = FALSE)

specializations <- do.call(rbind, l[["specializations"]]) %>% 
  dplyr::filter(name %in% potential_adv)
write.csv(specializations, "./data/specializations.csv", row.names = FALSE)

research_area <- do.call(rbind, l[["research_area"]]) %>% 
  parse.research_area() %>% 
  dplyr::filter(name %in% potential_adv)
write.csv(research_area, "./data/research_area.csv", row.names = FALSE)

bio <- do.call(rbind, l[["bio"]]) %>% 
  dplyr::filter(name %in% potential_adv)
write.csv(bio, "./data/biography.csv", row.names = FALSE)

edu <- do.call(rbind, l[["edu"]]) %>% 
  dplyr::filter(name %in% potential_adv) %>% 
  parse.edu() %>% 
  dplyr::filter(degree >= 1)
write.csv(edu, "./data/education.csv", row.names = FALSE)

pub <- do.call(rbind, l[["pub"]]) %>% 
  dplyr::filter(name %in% potential_adv)
write.csv(pub, "./data/publications.csv", row.names = FALSE)

courses <- do.call(rbind, l[["courses"]]) %>% 
  dplyr::filter(name %in% potential_adv)

write.csv(courses, "./data/courses.csv", row.names = FALSE)
