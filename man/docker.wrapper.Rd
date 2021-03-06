% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/docker.wrapper.R
\name{docker.wrapper}
\alias{docker.wrapper}
\title{docker.wrapper()}
\usage{
docker.wrapper(pkg = "")
}
\arguments{
\item{pkg}{\code{\link[=character]{character()}} with the name of the R package to dispatch to.}
}
\value{
\code{null}; output will be written to \code{output.txt}
}
\description{
This function is run by Docker when a \code{vtg}-based container is
started. It handles communication (input/output) with the Node
by reading/writing to files.

It expects the following environment variables to be set:
\itemize{
\item \code{DATABASE_URI}: filename of a CSV file that can be read by
the R-function \code{\link[=read.csv]{read.csv()}}  \emph{without} specifying format-details.
\item \code{INPUT_FILE}: filename that contains algorithm input in RDS format.
Input should be a \code{\link[=list]{list()}} as created using \code{\link[=create.task.input.unserialized]{create.task.input.unserialized()}}.
\item \code{OUTPUT_FILE}: filename of file that's used to write results to.
\item \code{TOKEN_FILE}: filename of file contains a JSON Web Token.
}

Performs the following actions:
\itemize{
\item Load data from the CSV file
\item Loads the input parameters provided to the container in \code{input.txt}.
\item Wraps the docker input/output for \code{dispatch.RPC()}.
}
}
