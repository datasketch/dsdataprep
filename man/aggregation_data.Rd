% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/01_aggregation.R
\name{aggregation_data}
\alias{aggregation_data}
\title{Aggregate data by group and perform summary operations}
\usage{
aggregation_data(
  data,
  agg,
  group_var,
  to_agg = NULL,
  agg_name = NULL,
  na_rm = TRUE,
  na_label = "(NA)",
  percentage = FALSE,
  percentage_name = NULL,
  percentage_col = NULL,
  extra_col = FALSE,
  agg_extra = "sum",
  extra_sep = ",",
  extra_group = NULL,
  collapse_columns = NULL,
  numeric_collapse_columns = NULL,
  extra_sep_collapse_columns = "-"
)
}
\arguments{
\item{data}{A data frame}

\item{agg}{The type of aggregation to perform (e.g., "count", "sum", "mean")}

\item{group_var}{A character vector of variable names to group by}

\item{to_agg}{A character vector of variable names to summarize}

\item{agg_name}{A character vector of new names for the summary variables}

\item{percentage}{A logical value indicating whether to calculate percentages}

\item{percentage_name}{A character vector of new names for percentage columns}

\item{percentage_col}{A character with the name of the variable for which you want the percentage to be calculated.}

\item{extra_col}{A logical value indicating whether to include an additional column in the output data frame containing the concatenated values of any factor or character columns.}

\item{agg_extra}{A character string indicating the aggregation function to use for the additional column if \code{extra_col} is \code{TRUE}. Options include "sum", "mean", "median", "min", "max", "count", and "n_distinct".}
}
\value{
A data frame containing the aggregated data.
}
\description{
This function aggregates and summaries data. Supported summary operations
includes count, sum, mean, and more.
}
\examples{
data(mtcars)
aggregation_data(data = mtcars,
                 agg = "mean",
                 group_var = c("cyl", "gear"),
                 to_agg = c("mpg", "disp"))

}
\seealso{
\code{\link{summarize_all}}, \code{\link{group_by}}, \code{\link{across}}
}
\concept{dplyr functions}
