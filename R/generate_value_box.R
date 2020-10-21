#' Function to generate a value box
#' 
#' @description Generate value box after summing a vector
#'
#' @param counts_vector A numeric vector, To be summed up in generating the value printed
#' @param text A String, The text printed on the value box
#' @param icon_name A String, The font-awesome icon name
#' @param color A String, The color of the box
#' 
#' @return An html widget including the summed vector, text and icon
#' 
#' @examples 
#' \dontrun{
#' generate_value_box(counts_vector=c(1, 5, 4, 9, 0),
#'   text="Confirmed Cases", icon="head-side-mask",
#'   color="green")
#' 
#' x <- c(37,42,75,29,46,85)
#' generate_value_box(counts_vector=x, text="Confirmed Cases",
#'   icon="head-side-mask", color="green")
#' 
#' generate_value_box(get_case_count(data_frame =
#'   download_data(data_type = "daily"), date = "2020-10-01",
#'   case_type = "Confirmed"), text="Confirmed Cases",
#'   icon="head-side-mask", color="green")
#' }
#' 
#' @export
generate_value_box <- function(counts_vector, text, icon_name, color){
  shinydashboard::valueBox(value = sum(counts_vector),
           subtitle = text,
           icon = shiny::icon(icon_name),
           color = color)
}