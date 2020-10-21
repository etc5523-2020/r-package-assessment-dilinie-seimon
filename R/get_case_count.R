#' Get case count at date
#'
#' @param data_frame A dataframe, COVID-19 counts associated with dates, use download_data() function for dataframe
#' @param date A string, The date at which the case numbers are required
#' @param case_type A string, The type of case numbers required - either "Confirmed", "Deaths" or "Recovered"
#' 
#' @return An integer, sum of COVID-19 cases of required type and date
#' 
#' @examples 
#' get_case_count(data_frame = download_data(data_type = "daily"), date = "2020-10-01", case_type = "Confirmed")
#' 
#' df <- download_data(data_type = "cumulative")
#' get_case_count(data_frame = df, date = "2020-10-01", case_type = "Deaths")
#' 
#' @importFrom magrittr %>%
#' @export
get_case_count <- function(data_frame, date, case_type){
  return(sum(data_frame %>%
    dplyr::filter(Date == date) %>%
    dplyr::ungroup() %>%
    dplyr::select(case_type)))
}