#' @title
#' General List scraper
#' 
#' @description
#' From a general list style page, scrape the first page with desired data using
#' css selectors.
#' @details
#' For websites that come in a 'list' or 'feed' format, this function finds the row on a page,
#' and using CSS selector that you specify, scrapes data from each row into a nice data frame
#' for research and munging purposes. This package extensively uses \code{rvest} functions.
#' @param url The link to crawl
#' @param row.selector The CSS element of rows
#' @param fields A text vector of the CSS fields used to identify wanted information within
#' the rows.
#' @return A single data frame containing all the elements scraped, with the CSS selector
#' listed as the name of each column.
#' 
#' @examples 
#' url <- http://philadelphia.craigslist.org/search/apa
#' fields <- c('time', '.hdrlnk', '.housing', 'small', '.price')
#' philly.apts <- ScrapeList(url, '.row', fields)
ScrapeList <- function(url, row.selector, fields) {
  rvest::raw.html <- html(url)
  
  # Return housing data
  overall.data <- pblapply(fields, function(field){
    field.data <- raw.html %>%
      rvest::html_nodes(row.selector) %>%
      rvest::html_node(field) %>%
      rvest::html_text()
  })
  
  overall.data <- do.call('cbind.data.frame', overall.data)
  names(overall.data) <- fields
  
  return(overall.data)
}
