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
                         delay = 2) {

  page <- xml2::read_html(link)
  print(link)

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

  id <- data.frame(name, title, email, phone, ischool, phd_adv, stringsAsFactors = FALSE)
  
  if(length(specializations) > 0) {
    specializations <- data.frame(email, specializations, stringsAsFactors = FALSE)
  } else {
    specializations <- NULL
  }

  if(length(research_area) > 0) {
    research_area <- data.frame(email, research_area, stringsAsFactors = FALSE)
  } else {
    research_area <- NULL
  }

  if(length(bio) > 0) {
    bio <- data.frame(email, bio, stringsAsFactors = FALSE)
  } else {
    bio <- NULL
  }

  if(length(edu) > 0 ) {
    edu <- data.frame(email, edu, stringsAsFactors = FALSE)
  } else {
    edu <- NULL
  }

  if(length(pub) > 0) {
    pub <- data.frame(email, pub, stringsAsFactors = FALSE)
  } else {
    pub <- NULL
  }

  Sys.sleep(delay)

  return(list(id = id, specializations = specializations, research_area = research_area,
              bio = bio, edu = edu, pub = pub))
}


#' @title Scrape faculty page at University of Washington
faculty.uw <- function(url = "https://ischool.uw.edu/people/faculty") {

  links <- scrape_links(url = url, selector = ".h1 > a")

  links <- paste0(url, gsub("/people/faculty", "", links))

  l <- lapply(links, 
              collect_info, 
              ischool = "University of Washington",
              name_selector = "div > article > div.info > h1",
              title_selector = "div > article > div.info > em",
              email_selector = "div > article > div.info > address > a",
              phone_selector = NULL,
              phd_selector = NULL,
              spec_selector = ".sections > .specialization > ul > li",
              research_selector = ".sections > .research_areas > ul > li",
              bio_selector = ".sections > .biography",
              edu_selector = ".sections > .education > ul > li",
              pub_selector = ".pub > .title")

  l <- collapse(l)

  return(l)
}

#' @title Scrape faculty page at University of Michigan
faculty.um <- function(url = "https://www.si.umich.edu/people/directory/faculty?page={n}") {
  
  urls <- glue::glue(url, n = 0:6)

  links <- lapply(urls, function(url) {
    xml2::read_html(url) %>% 
      rvest::html_nodes(" div > div > div > h2 > a") %>% 
      rvest::html_attr("href")
  })

  links <- paste0("https://www.si.umich.edu", unlist(links, recursive = FALSE))

  l <- lapply(links,
              collect_info, 
              ischool = "University of Michigan",
              name_selector = "div > div > div > h1 > span",
              title_selector = "div > div > div > div > div > div.profile-card__left > span:nth-child(1) > strong",
              email_selector = "div > div > div > div > div > div.profile-card__left > span > a",
              phone_selector = NULL,
              phd_selector = "div > div > div > div > div > div.profile-card__right > span:nth-child(3)",
              spec_selector = NULL,
              research_selector = "div > div > div > div > ul > li",
              bio_selector = "div > div > div > div > div > p",
              edu_selector = "div > div > div > div > div > p",
              pub_selector = NULL)

  l <- collapse(l)
  
  return(l)
}

#' @title Scrape faculty page at University of Illinois
faculty.illinois <- function(url = "https://ischool.illinois.edu/people/faculty") {
  
  links <- scrape_links(url = url, selector = "div.profile-teaser__name > a")
  
  links <- paste0("https://ischool.illinois.edu", gsub("/people/faculty", "", links))

  l <- lapply(links, 
              collect_info, 
              ischool = "University of Illinois Urban Champagne",
              name_selector = "#page-title > span",
              title_selector = "body > div.dialog-off-canvas-main-canvas > main > div > div.basic-page__guttered-content > div.basic-page__main > div > article > div > div.profile__core-content > p.profile__title",
              email_selector = "body > div.dialog-off-canvas-main-canvas > main > div > div.basic-page__guttered-content > div.basic-page__main > div > article > div > div.profile__core-content > div.profile__contact-information > p:nth-child(3)",
              phone_selector = "body > div.dialog-off-canvas-main-canvas > main > div > div.basic-page__guttered-content > div.basic-page__main > div > article > div > div.profile__core-content > div.profile__contact-information > p:nth-child(2) > a",
              phd_selector = NULL,
              spec_selector = NULL,
              research_selector = "body > div.dialog-off-canvas-main-canvas > main > div > div.basic-page__guttered-content > div.basic-page__main > div > article > div > div.profile__core-content > div.tag-list > ul > li > a",
              bio_selector = "body > div.dialog-off-canvas-main-canvas > main > div > div.basic-page__guttered-content > div.basic-page__main > div > article > div > div.profile__core-content > div:nth-child(7) > div > p:nth-child(1)",
              edu_selector = "body > div.dialog-off-canvas-main-canvas > main > div > div.basic-page__guttered-content > div.basic-page__main > div > article > div > div.profile__core-content > p.profile__education",
              pub_selector = "body > div.dialog-off-canvas-main-canvas > main > div > div.basic-page__guttered-content > div.basic-page__main > div > article > div > div.profile__core-content > div:nth-child(10) > div > p")

  l <- collapse(l)

  return(l)
}

