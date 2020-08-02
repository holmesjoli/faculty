#' @title Parse Research Area
parse.research_area <- function(df) {
  
  df <- df %>% 
    dplyr::mutate(data_science = grepl("data science", tolower(research_area)),
                  library_science = grepl("library science|library sciences|library", tolower(research_area)),
                  privacy = grepl("privacy|security", tolower(research_area)),
                  sts = grepl("science, technology, and society|science, technology & society|sts", tolower(research_area)),
                  archive = grepl("archive", tolower(research_area)),
                  hci = grepl("human-computer interaction|human computer interaction|hci", tolower(research_area)),
                  visualization = grepl("visualization", tolower(research_area)),
                  ethics = grepl("ethics", tolower(research_area)))
}

#' @title Parse Education
parse.edu <- function(df) {

  df <- df %>%
    dplyr::mutate(phd = grepl("PhD|Ph D|Ph.D.", edu),
                  ms = grepl("MS|M.S.|Master|master|ScM|MEng|MSC", edu),
                  ma = grepl("MA|M.A.|Master|master", edu),
                  med = grepl("M.Ed.|EdM|EDD", edu),
                  mlis = grepl("MLIS|M.I.M.S|MIMS|A.M.L.S.|AMLS", edu),
                  mba = grepl("MBA", edu),
                  mph = grepl("MPH", edu),
                  mpa = grepl("MPA", edu),
                  mfa = grepl("MFA", edu),
                  add_masters = grepl("MDP|MDes|MTech|LLM|M.Tech", edu),
                  bs = grepl("BS|B.S.|BAS|B.A.S|Bachelor|bachelor|Sc.B|BSC", edu),
                  ba = grepl("BA|B.A|Bachelor|bachelor|AB|A.B.", edu),
                  be = grepl("BE|B.E", edu),
                  jd = grepl("JD", edu),
                  ba = ifelse(mba & ba, FALSE, ba),
                  masters = ifelse(ms|ma|med|mlis|mba|mph|mpa|add_masters, TRUE, FALSE),
                  bachelors = ifelse(bs|ba|be, TRUE, FALSE))
  
  df$degree <- rowSums(df[c("phd", "masters", "bachelors", "jd")])
  
  return(df)
}

#' @title Parse ID
parse.id <- function(df) {
  
  df %>% 
    dplyr::mutate(
      emeritus = grepl("emeritus", tolower(title)),
      research_fellow = grepl("research fellow|fellow", tolower(title)),
      adjunct = grepl("adjunct", tolower(title)),
      affiliate = grepl("affiliate", tolower(title)),
      assistant = grepl("assistant", tolower(title)),
      associate = grepl("associate", tolower(title)),
      lecturer = grepl("lecturer", tolower(title)),
      phd = grepl("ph.d. candidate|ph.d. student", tolower(title)),
      phd_adv = gsub("Potential PhD Faculty Advisor: ", "", phd_adv),
      phd_adv = ifelse(phd_adv == "", NA, phd_adv)
      )

}

#' @param url string. The url
#' @param selector string. The selctor to use to scrape the page.
scrape_links <- function(url, selector) {
 
  xml2::read_html(url) %>% 
    rvest::html_nodes(selector) %>% 
    rvest::html_attr("href") 
}

#' @param page list. HTML page
#' @param selector character. The selector to use to scrape the page
scrape_text <- function(page, selector) {

  if(!is.null(selector)) {
    
    txt <- page %>% 
      rvest::html_nodes(selector) %>% 
      rvest::html_text() %>% 
      stringr::str_trim("both")
    
    if(length(txt) == 0) {
      return(NA)
    } else {
      return(txt)
    }

  } else {
    return(NA)
  }
  
}

#' @title Collects the links to the faculty pages
#' @param url string. The base url for at a particular ischool
#' @param query string. The query to go to the specific page on the ischool website
#' @param selector string. The selector scrape different links (href) from the page
collect_faculty_pages <- function(...) UseMethod("collect_faculty_pages")

collect_faculty_pages.default <- function(url, query, selector) {

  links <- scrape_links(url = glue::glue(url, query = query), selector = selector)
  links <- paste0(glue::glue(url, query = links))
  
  return(links)
}

collect_faculty_pages.um <- function(url, query, selector) {

  queries <- glue::glue(query, n = 0:6)

  links <- lapply(queries, function(query, url, selector) {
    collect_faculty_pages(url = url, query = query, selector = selector)},
    url = url, selector = selector
  )

  links <- unlist(links, recursive = FALSE)

  return(links)
}

collect_info <- function(link, ischool,
                         name_selector = NULL,
                         title_selector = NULL,
                         email_selector = NULL,
                         phone_selector = NULL,
                         spec_selector  = NULL,
                         research_selector = NULL,
                         bio_selector = NULL,
                         edu_selector = NULL,
                         pub_selector = NULL,
                         phd_selector = NULL,
                         course_selector = NULL,
                         delay = NULL) {

  page <- xml2::read_html(link)
# browser()
  name <- scrape_text(page = page, selector = name_selector)
  assertthat::assert_that(length(name) == 1 | is.null(name))

  title <- scrape_text(page = page, selector = title_selector)
  assertthat::assert_that(length(title) == 1 | is.null(title))

  email <- scrape_text(page = page, selector = email_selector)
  assertthat::assert_that(length(email) == 1 | is.null(email))

  phone <- scrape_text(page = page, selector = phone_selector)
  assertthat::assert_that(length(phone == 1) | is.null(phone))

  if(is.na(email)) {
    email <- phone
    phone <- NA
  } else {
    email <- email
  }

  phd_adv <- scrape_text(page = page, selector = phd_selector)
  assertthat::assert_that(length(phd_adv) == 1 | is.null(phd_adv))

  specializations <- scrape_text(page = page, selector = spec_selector)

  research_area <- scrape_text(page = page, selector = research_selector)

  bio <- scrape_text(page = page, selector = bio_selector)

  edu <- scrape_text(page = page, selector = edu_selector)

  pub <- scrape_text(page = page, selector = pub_selector)

  courses <- scrape_text(page = page, selector = course_selector)
  
  id <- data.frame(name, title, email, phone, ischool, phd_adv, link, stringsAsFactors = FALSE)

  if(length(specializations) > 0) {
    specializations <- data.frame(name, specializations, stringsAsFactors = FALSE)
  } else {
    specializations <- NULL
  }

  if(length(research_area) > 0) {
    research_area <- data.frame(name, research_area, stringsAsFactors = FALSE)
  } else {
    research_area <- NULL
  }

  if(length(bio) > 0) {
    bio <- data.frame(name, bio, stringsAsFactors = FALSE)
  } else {
    bio <- NULL
  }

  if(length(edu) > 0 ) {
    edu <- data.frame(name, edu, stringsAsFactors = FALSE)
  } else {
    edu <- NULL
  }

  if(length(pub) > 0) {
    pub <- data.frame(name, pub, stringsAsFactors = FALSE)
  } else {
    pub <- NULL
  }
  
  if(length(courses) > 0) {
    pub <- data.frame(name, courses, stringsAsFactors = FALSE)
  } else {
    pub <- NULL
  }

  if(!is.null(delay)) {
    Sys.sleep(delay) 
  }

  return(list(id = id, specializations = specializations, research_area = research_area,
              bio = bio, edu = edu, pub = pub, courses = courses))
}

collect_info_main <- function(url, query, ischool,
                              faculty_selector,
                              name_selector = NULL,
                              title_selector = NULL,
                              email_selector = NULL,
                              phone_selector = NULL,
                              phd_selector = NULL,
                              spec_selector = NULL,
                              research_selector = NULL,
                              bio_selector = NULL,
                              edu_selector = NULL,
                              pub_selector = NULL,
                              course_selector = NULL,
                              delay = NULL) {

  links <- collect_faculty_pages(url = url, query = query, selector = faculty_selector)

  l <- lapply(links, 
              collect_info, 
              ischool = ischool,
              name_selector = name_selector,
              title_selector = title_selector,
              email_selector = email_selector,
              phone_selector = phone_selector,
              phd_selector = phd_selector,
              spec_selector = spec_selector,
              research_selector = research_selector,
              bio_selector = bio_selector,
              edu_selector = edu_selector,
              pub_selector = pub_selector,
              course_selector = course_selector,
              delay = delay)

  l <- collapse(l)

  return(l)
}

collapse <- function(l) {
  
  l2 <- l %>% 
    plyr::compact() %>% 
    purrr::transpose()
  
  id <- do.call(rbind, l2[["id"]])
  
  specializations <- do.call(rbind, l2[["specializations"]])
  
  research_area <- do.call(rbind, l2[["research_area"]])
  
  bio <- do.call(rbind, l2[["bio"]])
  
  edu <- do.call(rbind, l2[["edu"]])
  
  pub <- do.call(rbind, l2[["pub"]])
  
  return(list(id = id, specializations = specializations, research_area = research_area,
              bio = bio, edu = edu, pub = pub))
}

create_cls <- function(school) {
  s <- structure(school, class = school$class)
  s$class <- NULL
  return(s)
}
