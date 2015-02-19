#' @title
#' Craigslist Scraper
#' 
#' @description
#' From a general list style page on Craigslist, scrape the url with desired 
#' data using css selectors.
#' @details
#' For Craigslist pages, this function finds the row on a page,
#' and using CSS selector that you specify, scrapes data from each row into a 
#' nice data frame for research and munging purposes. This function extensively 
#' uses \code{rvest} functions.
#' 
#' To get the CSS selectors, I used the SelectorGadget chrome extension. For
#' more info, see \href{http://selectorgadget.com}{selectorgadget.com}.
#' @param url A text string of the link to crawl
#' @param row.selector The CSS selector that demarcates the rows on a page.
#' @param fields A text vector of the CSS fields used to identify desired 
#' information within the rows.
#' @return A single data frame containing all the elements scraped, with the CSS selector
#' listed as the name of each column.
#' 
#' @examples 
#' url <- 'http://philadelphia.craigslist.org/search/apa'
#' fields <- c('time', '.hdrlnk', '.housing', 'small', '.price')
#' philly.apts <- ScrapeCraigslist(url, '.row', fields)
#' 
#' # Clean up the price data
#' philly.apts[, 5] <- as.integer(gsub('\\$', '', philly.apts[, 5]))
ScrapeCraigslist <- function(url, row.selector, fields) {
  raw.html <- rvest::html(url)
  
  # Return data by css selector on the page
  overall.data <- pblapply(fields, function(field){
    field.data <- raw.html %>%
      html_nodes(row.selector) %>%
      html_node(field) %>%
      html_text()
  })
  
  overall.data <- do.call('cbind.data.frame', overall.data)
  names(overall.data) <- fields
  
  return(overall.data)
}
