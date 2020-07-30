scrape_links <- function(url, selector) {
 
  xml2::read_html(url) %>% 
    rvest::html_nodes(selector) %>% 
    rvest::html_attr("href") 
}

#' @param page list. HTML page
#' @param selector character. The selector to use to scrape the page
scrape_inner <- function(page, selector) {

  page %>% 
    rvest::html_nodes(selector) %>% 
    rvest::html_text() %>% 
    stringr::str_trim("both")
}

#' @title University of Washington
faculty.uw <- function(url = "https://ischool.uw.edu/people/faculty") {

  links <- scrape_links(url = url, selector = ".h1 > a")

  links <- paste0(url, gsub("/people/faculty", "", links))

  l <- lapply(links, function(link) {

    page <- xml2::read_html(link)

    name <- scrape_inner(page = page, selector = "div > article > div.info > h1")
    
    title <- scrape_inner(page = page, selector = "div > article > div.info > em")

    email <- page %>% 
      rvest::html_nodes("div > article > div.info > address > a") %>% 
      rvest::html_attr("href")

    email <- gsub("mailto:", "", email)
    
    specializations <- scrape_inner(page = page, selector = ".sections > .specialization > ul > li")

    research_area <- scrape_inner(page = page, selector = ".sections > .research_areas > ul > li")

    bio <- scrape_inner(page = page, selector = ".sections > .biography")

    edu <- scrape_inner(page = page, selector = ".sections > .education > ul > li")

    pub <- scrape_inner(page = page, selector = ".pub > .title")

    id <- data.frame(name, title, email, stringsAsFactors = FALSE)

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

    return(list(id = id, specializations = specializations, research_area = research_area,
                bio = bio, edu = edu, pub = pub))
    
  })
  
  l2 <- l %>% 
    purrr::transpose()
  
  id <- do.call(rbind, l2[["id"]]) %>% 
    dplyr::mutate(ischool = "University of Washington")
  
  specializations <- do.call(rbind, l2[["specializations"]])
  
  research_area <- do.call(rbind, l2[["research_area"]])
  
  bio <- do.call(rbind, l2[["bio"]])
  
  edu <- do.call(rbind, l2[["edu"]])
  
  pub <- do.call(rbind, l2[["pub"]])
  
  return(list(id = id, specializations = specializations, research_area = research_area,
              bio = bio, edu = edu, pub = pub))
}

#' @title University of Michigan
faculty.um <- function(url = "https://www.si.umich.edu/people/directory/faculty?page={n}") {
  
  urls <- glue::glue(url, n = 0:6)

  links <- lapply(urls, function(url) {
    xml2::read_html(url) %>% 
      rvest::html_nodes(" div > div > div > h2 > a") %>% 
      rvest::html_attr("href")
  })
  
  links <- paste0("https://www.si.umich.edu", unlist(links, recursive = FALSE))

  l <- lapply(links, function(link) {
    
    page <- xml2::read_html(link)

    name <- page %>% 
      rvest::html_node("div > div > div > h1 > span") %>% 
      rvest::html_text()
    
    title <- page %>% 
      rvest::html_node("div > div > div > div > div > div.profile-card__left > span > strong") %>% 
      rvest::html_text() %>% 
      stringr::str_trim("both")
    
    email <- page %>% 
      rvest::html_node("div > div > div > div > div > div.profile-card__left > span > a") %>% 
      rvest::html_text()
    
  })

}
