

#' Plant growth experiment
#'
#' A scientist treated 7 plants of the species *Brassica campestris* with the
#' substance Ancymidol (Ancy) and compared them to 8 plants that were not treated.
#' Height (in cm) after 14 days of growth was measured for each plant.
#'
#' @format A data frame with 3 variables:
#'
#' * `height`: height in cm after 14 days
#' * `group`: `"Ancy"` or `"Control"`
#' * `group_ind`: 0 for the control group and 1 for the Ancy group.
#'
#'
#' @examples
#' df_stats(height ~ group, data = Ancy)
"Ancy"

