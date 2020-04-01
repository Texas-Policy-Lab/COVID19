#' @title Census
#' @description Returns data from the Census API
census <- function() UseMethod("census")

census.default <- function(url = NULL, variable = NULL, geography = NULL) {

  api <- paste0(url, variable, geography)

  api <- httr::GET(api)
  response <- httr::content(api)
  l <- lapply(response, unlist)
  df <- do.call("rbind.data.frame", l)

  df[colnames(df)] <- apply(df[colnames(df)], 2, as.character)

  colnames(df) <- df[1, ]
  df <- df[-1, ] 

  df[colnames(df)] <- apply(df[colnames(df)], 2, as.numeric)

  return(df)
}

census.county_pop <- function(url = "https://api.census.gov/data/2018/acs/acs5?get=",
                              variable = "B01003_001E",
                              geography = "&for=county:*&in=state:*") {
 
  df <- census.default(url, variable, geography)
    
  colnames(df)[match(variable, colnames(df))] <- "pop"
  colnames(df)[match("state", colnames(df))] <- "stateFIPS"

  df <- df %>%
    dplyr::mutate(countyFIPS = as.numeric(paste0(stateFIPS, county))) %>% 
    dplyr::select(-county) %>% 
    dplyr::arrange(desc(pop)) %>% 
    dplyr::group_by(stateFIPS) %>% 
    dplyr::mutate(county_pop_rank = seq(1, dplyr::n())) %>% 
    dplyr::arrange(stateFIPS, pop)
}

census.state_pop <- function(url = "https://api.census.gov/data/2018/acs/acs5?get=",
                             variable = "B01003_001E",
                             geography = "&for=state:*") {

  df <- census.default(url, variable, geography)

  colnames(df)[match(variable, colnames(df))] <- "pop"
  colnames(df)[match("state", colnames(df))] <- "stateFIPS"
  
  return(df)
}
