% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/view.R
\name{view_uid}
\alias{view_uid}
\title{View Data by UID}
\usage{
view_uid(uid, format = c("raw", "dataframe"), unnest_cutoff = NULL)
}
\arguments{
\item{uid}{A character vector containing UIDs to retrieve data for.}

\item{format}{The format of the output. Options are "raw" (default) or "dataframe".}

\item{unnest_cutoff}{The maximum length for entries to keep in the result when format is "dataframe".}
}
\value{
A list of data, either in raw format or as a dataframe, depending on the format parameter.
}
\description{
This function retrieves data by UID from the API and formats it based on the specified options.
}
\details{
This function retrieves data for each UID provided in the uid parameter.
It first retrieves the data in raw format using the call_meteor function and
then formats the data based on the specified format parameter.
If the format is "dataframe", it reshapes the data into a rectangular dataframe
using tibblify::tibblify and unnests it recursively using fleece::unnest_recursively.
If unnest_cutoff is provided, it drops entries that would create excessively wide tibbles
when the format is "dataframe".
}
\examples{
news <- call_meteor(method = "get", ressource = "query",
type = "NewsSource", format = "dataframe",  n_max = 3, n = 3)

view_df <- view_uid(uid = news$uid, format = "dataframe", unnest_cutoff = 1)

}
