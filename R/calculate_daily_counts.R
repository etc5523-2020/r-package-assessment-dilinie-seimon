#' Calculate daily COVID case counts from cumulative counts
#'
#' @param type A dataframe of cumulative confirmed cases, deaths and recoveries in Australia. Can be downloaded using download_data(data_type = "cumulative")
#' 
#' @return A dataframe of daily confirmed COVID-19 cases, deaths and recoveries in Australia
#' 
#' @examples 
#' calculate_daily_counts(cum_counts_df = download_data(data_type = "cumulative")
#' 
#' df <- download_data(data_type = "cumulative")
#' calculate_daily_counts(cum_counts_df = df)
#' 
#' @importFrom magrittr %>%
#' @export
calculate_daily_counts <- function(cum_counts_df){
  cum_counts_df %>%
    dplyr::group_by(region) %>%
    dplyr::arrange(Date) %>%
    dplyr::mutate(daily_confirmed = tidyr::replace_na((Confirmed - dplyr::lag(Confirmed)),0),
           daily_deaths = tidyr::replace_na((Deaths - dplyr::lag(Deaths)),0),
           daily_recovered = tidyr::replace_na((Recovered - dplyr::lag(Recovered)),0)) %>%
    dplyr::select(c(-Confirmed,-Deaths,-Recovered)) %>%
    dplyr::rename("Confirmed" = daily_confirmed,
           "Deaths" = daily_deaths,
           "Recovered" = daily_recovered)
}