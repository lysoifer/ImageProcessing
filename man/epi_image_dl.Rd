% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/epicollect_image_download.R
\name{epi_image_dl}
\alias{epi_image_dl}
\title{epi_image_dl}
\usage{
epi_image_dl(
  slug,
  form.ref,
  access,
  cID = NA,
  secret = NA,
  cname,
  path,
  df_path = NA
)
}
\arguments{
\item{slug}{project slug}

\item{form.ref}{form ref}

\item{access}{whether epicollect project is "pulic" or "private" (currently only supports public projects)}

\item{cID}{client ID if project is private}

\item{secret}{secret if project is private}

\item{cname}{name of column that photos are stored in}

\item{path}{file directory to save images to}

\item{df_path}{file path to save dataframe (must be .csv)}
}
\description{
Download images from an epicollect project
}
\details{
Slug and form ref can be found in epicollect project details. Use the project slug and the form ref. Code adapted from https://gist.github.com/mirko77/3f4a101cd4a77e2ae3e760d44d18d901
}
\examples{
epi_image_dl(slug = "test-photo-api", form.ref = "ead5f161866447249c9c57b255dae5c7_66abc8fcd2c06", access = "public", cname = "photo", path = "/path/to/photos/")

}
\keyword{download}
\keyword{epicollect,}
