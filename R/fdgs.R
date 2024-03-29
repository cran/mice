#' Fifth Dutch growth study 2009
#'
#' Age, height, weight and region of 10030 children measured within the Fifth
#' Dutch Growth Study 2009
#'
#' The data set contains data from children of Dutch descent (biological parents
#' are born in the Netherlands). Children with growth-related diseases were
#' excluded. The data were used to construct new growth charts of children of
#' Dutch descent (Schonbeck 2013), and to calculate overweight and obesity
#' prevalence (Schonbeck 2011).
#'
#' Some groups were underrepresented.  Multiple imputation was used to create
#' synthetic cases that were used to correct for the nonresponse. See Van Buuren
#' (2012), chapter 8 for details.
#'
#' @name fdgs
#' @aliases fdgs
#' @docType data
#' @format \code{fdgs} is a data frame with 10030 rows and 8 columns:
#' \describe{
#' \item{id}{Person number}
#' \item{reg}{Region (factor, 5 levels)}
#' \item{age}{Age (years)}
#' \item{sex}{Sex (boy, girl)}
#' \item{hgt}{Height (cm)}
#' \item{wgt}{Weight (kg)}
#' \item{hgt.z}{Height Z-score}
#' \item{wgt.z}{Weight Z-score}
#' }
#' @source Schonbeck, Y., Talma, H., van Dommelen, P., Bakker, B., Buitendijk,
#' S. E., Hirasing, R. A., van Buuren, S. (2011).  Increase in prevalence of
#' overweight in Dutch children and adolescents: A comparison of nationwide
#' growth studies in 1980, 1997 and 2009.  \emph{PLoS ONE}, \emph{6}(11),
#' e27608.
#'
#' Schonbeck, Y., Talma, H., van Dommelen, P., Bakker, B., Buitendijk, S. E.,
#' Hirasing, R. A., van Buuren, S. (2013). The world's tallest nation has
#' stopped growing taller: the height of Dutch children from 1955 to 2009.
#' \emph{Pediatric Research}, \emph{73}(3), 371-377.
#'
#' Van Buuren, S. (2018).
#' \href{https://stefvanbuuren.name/fimd/sec-nonresponse.html#fifth-dutch-growth-study}{\emph{Flexible Imputation of Missing Data. Second Edition.}}
#' Boca Raton, FL.: Chapman & Hall/CRC Press.
#' @keywords datasets
#' @examples
#'
#'
#' data <- data(fdgs)
#' summary(data)
NULL
