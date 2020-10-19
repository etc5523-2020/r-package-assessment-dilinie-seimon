#' Download Australia COVID-19 data from JHU CSSE
#'
#' @param type A string, "cumulative" or "daily" defining the type of data to be downloaded
#' 
#' @return A dataframe of confirmed COVID-19 cases, deaths and recoveries in Australia
#' 
#' @examples 
#' download_data(data_type = "cumulative")
#' download_data(data_type = "daily")
#' 
#' @export
download_data <- function(data_type = "cumulative"){
  if(data_type == "cumulative"){
    return(download_jhu_data() %>%
             rename("Date" = date,
                    "Confirmed" = confirmed,
                    "Deaths" = deaths,
                    "Recovered" = recovered))
  } else if(data_type == "daily"){
    return(download_jhu_data() %>%
             group_by(region) %>%
             arrange(date) %>%
             mutate(Confirmed = replace_na((confirmed - lag(confirmed)),0),
                    Deaths = replace_na((deaths - lag(deaths)),0),
                    Recovered = replace_na((recovered - lag(recovered)),0)) %>%
             select(c(-confirmed,-deaths,-recovered)) %>%
             rename("Date" = date))
  } else {
    
  }
}
#' 
#' Downloads Australia data from JHU CSSE
#' 
#' @return dataframe of cumulative confirmed cases, deaths and recoveries in Australia
#' @noRd
download_jhu_data <- function(){
  download_jhu_csse_covid19_data(type = "country_region", cached = TRUE) %>%
    filter(iso3c == "AUS")
}