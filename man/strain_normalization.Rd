% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_normalization.R
\name{strain_normalization}
\alias{strain_normalization}
\title{Normalize plate reader values using a given strain and induction condition}
\usage{
strain_normalization(df, .var, ref_strain, ref_induction, column_suffix)
}
\arguments{
\item{df}{A plate reader data.frame containing both OD and fluorescence values.}

\item{.var}{The unquoted column name of the variable to be normalized (i.e. fl_od).}

\item{ref_strain}{The reference strain to normalize with.}

\item{ref_induction}{The reference induction condition to normalize with.}

\item{column_suffix}{The suffix that will be appended to the column name of the normalized variable.}
}
\value{
A data.frame containing a new column with normalized variable.
}
\description{
Function to normalize by a strain and induction.
}
\examples{
strain_normalization(df.pr.bc, 
.var = fl_od, ref_strain = "J23100", 
ref_induction = "-", 
column_suffix = "norm")


}
