\name{fromCSV}
\alias{fromCSV}
\docType{data}
\title{
Sample Data
}
\description{
Dataset imported by read.csv(). The dataset contains 20 different cars from 3 continents.
}
\usage{data(fromCSV)}
\format{
data.frame with 20 rows and 10 variables.
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\arguments{
\item{V1}{Manufacturer. name of the manufacturer (Audi, BMW, Chevrolet,...)}
\item{V2}{Model. name of the model (A8, 328i, Malibu,...)}
\item{V3}{Country. numeric indicator for the country (1 = Germany, 2 = US, 3= Japan)}
\item{V4}{Car-Type. numeric indicator for the car-type (1 = PKW,2 = SUV)}
\item{V5}{Sales volume in thousand. (0.954-230.902)}
\item{V5_kl2}{Sales volume grouped. (1 = Until 30.000, 2 = More than 30.000)}
\item{V6}{Purchase price in thousand. (15.35-85.50)}
\item{V6_kl3}{Purchase price in three groups. (1 = Until 20.000, 2 = more than 20.000 until 30.000, 3 = more than 30.000)}
\item{V7_1}{PS. Amount of PS (135-310)}
\item{V7_2}{Weight. Weight of the car (94.5-138.60)}
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(fromCSV)
}
\keyword{datasets}
