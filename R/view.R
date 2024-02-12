#' View Data by UID
#'
#' This function retrieves data by UID from the API and formats it based on the specified options.
#'
#' @param uid A character vector containing UIDs to retrieve data for.
#' @param format The format of the output. Options are "raw" (default) or "dataframe".
#' @param unnest_cutoff The maximum length for entries to keep in the result when format is "dataframe".
#'
#' @return A list of data, either in raw format or as a dataframe, depending on the format parameter.
#'
#' @details
#' This function retrieves data for each UID provided in the uid parameter.
#' It first retrieves the data in raw format using the call_meteor function and
#' then formats the data based on the specified format parameter.
#' If the format is "dataframe", it reshapes the data into a rectangular dataframe
#' using tibblify::tibblify and unnests it recursively using fleece::unnest_recursively.
#' If unnest_cutoff is provided, it drops entries that would create excessively wide tibbles
#' when the format is "dataframe".
#'
#' @examples
#' news <- call_meteor(method = "get", ressource = "query",
#' type = "NewsSource", format = "dataframe",  n_max = 3, n = 3)
#'
#' view_df <- view_uid(uid = news$uid, format = "dataframe", unnest_cutoff = 1)
#'
#' @export
view_uid <- function(uid,
                     format = c("raw", "dataframe"),
                     unnest_cutoff = NULL
){

  # Set call API function with fixed arguments
  vw <- function(ui
  ){
    v <- call_meteor(method = "get",
                     ressource = "view",
                     option = "uid",
                     uid = ui,
                     format = "raw"
    )
    return(v)
  }

  # Apply on vector
  res <- pbapply::pblapply(X = uid, FUN = vw)

  # Drop entries that would create excessively wide tibbles
  if(!is.null(unnest_cutoff)){
    res <- purrr::map(res, ~purrr::keep(.x, ~length(.x) <= unnest_cutoff))
  }

  if(format == "dataframe"){
    res <- tibblify::tibblify(res) |>
      fleece::unnest_recursively()
  }

  return(res)

}
