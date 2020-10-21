utils::globalVariables(c("iso3c", "confirmed", "deaths", "recovered"))
#' Download Australia COVID-19 data from JHU CSSE
#'
#' @param data_type A string, "cumulative" or "daily" defining the type of data to be downloaded
#' 
#' @return A dataframe of confirmed COVID-19 cases, deaths and recoveries in Australia
#' 
#' @examples 
#' download_data(data_type = "cumulative")
#' download_data(data_type = "daily")
#' 
#' @importFrom magrittr %>%
#' @export
download_data <- function(data_type = "cumulative"){
  if(data_type == "cumulative"){
    return(download_jhu_data())
  } else if(data_type == "daily"){
    return(download_jhu_data() %>%
             calculate_daily_counts())
  } else {
    warning("Invalid argument. data_type can be either 'cumulative' or 'daily'")
  }
}
#' 
#' Downloads Australia data from JHU CSSE
#' 
#' @return dataframe of cumulative confirmed cases, deaths and recoveries in Australia
#' 
#' @noRd
download_jhu_data <- function(){
  tidycovid19::download_jhu_csse_covid19_data(type = "country_region", cached = TRUE) %>%
    dplyr::filter(iso3c == "AUS") %>%
    dplyr::rename("Date" = date,
                  "Confirmed" = confirmed,
                  "Deaths" = deaths,
                  "Recovered" = recovered)
}