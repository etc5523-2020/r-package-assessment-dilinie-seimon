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
#' @importFrom magrittr %>%
download_data <- function(data_type = "cumulative"){
  if(data_type == "cumulative"){
    return(download_jhu_data() %>%
             dplyr::rename("Date" = date,
                    "Confirmed" = confirmed,
                    "Deaths" = deaths,
                    "Recovered" = recovered))
  } else if(data_type == "daily"){
    return(download_jhu_data() %>%
             dplyr::group_by(region) %>%
             dplyr::arrange(date) %>%
             dplyr::mutate(Confirmed = tidyr::replace_na((confirmed - stats::lag(confirmed)),0),
                    Deaths = tidyr::replace_na((deaths - stats::lag(deaths)),0),
                    Recovered = tidyr::replace_na((recovered - stats::lag(recovered)),0)) %>%
             dplyr::select(c(-confirmed,-deaths,-recovered)) %>%
             dplyr::rename("Date" = date))
  } else {
    
  }
}
#' 
#' Downloads Australia data from JHU CSSE
#' 
#' @return dataframe of cumulative confirmed cases, deaths and recoveries in Australia
#' @noRd
download_jhu_data <- function(){
  tidycovid19::download_jhu_csse_covid19_data(type = "country_region", cached = TRUE) %>%
    dplyr::filter(iso3c == "AUS")
}