% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_fusion.R
\name{utilsCreateReference}
\alias{utilsCreateReference}
\title{utilsCreateReference}
\usage{
utilsCreateReference(df, method = "max", refVector = c())
}
\arguments{
\item{df}{A DataFrame.}

\item{method}{A string value specifying the reference creating method. Available options: max, min, median, mean and mixed.}

\item{refVector}{A vector of strings that specifies a method for each row.
Vector size should be equal to the number of rows in the DataFrame df.}
}
\value{
Returns a new DataFrame appended with the reference column created by the method.
}
\description{
Adds a new reference column based on the input DataFrame df and the given method. 
This function iterates over the rows and applies the given method to define the value of the reference. 
Available options are: max, min, median, mean and mixed. This column is appended to the DataFrame.
When "mixed" is specified the function will consider the refVector for creating the reference column.
}
\examples{
SRDInput <- data.frame(
A=c(32, 52, 44, 44, 47),
B=c(73, 75, 65, 76, 70),
C=c(60, 59, 57, 55, 60),
D=c(35, 24, 44, 83, 47),
E=c(41, 52, 46, 50, 65))
proc_data <- rSRD::utilsPreprocessDF(SRDInput)
ref <- c("min","max","min","max","mean")
rSRD::utilsCreateReference(proc_data, method = "mixed", ref)
}
\author{
Ali Tugay Sen, Linus Olsson \email{linusmeol@gmail.com}
}
