#' @importFrom tibble tibble
NULL

#' Butteflies
#'
#' Time series of butterfly populations made available by Roland and Metter (2016).
#'
#' @format A tibble with ninety-nine variables:
#' \describe{
#' \item{\code{year}}{Year}
#' \item{\code{meada}}{Letter designating each of the 21 meadows/subpopulations}
#' \item{\code{Nt}}{estimate of population size in \code{year}}
#' \item{\code{logNt}}{log10-transformed estimate of population size (plus 0.5) in \code{year}}
#' \item{\code{Rt}}{Rate of population growth from \code{year} to \code{year+1}}
#' \item{\code{maxsnow}}{Maximum recorded amount of snow for the entire winter (mm water-equivalent)}
#' \item{\code{datesnowmelt}}{Julian date in spring on which snow amount declined to 150mm water equivalent }
#' \item{\code{jansnow}}{Amount of snow on the ground on January 15 of \code{year+1}}
#' \item{\code{jansnowfall}}{Amount of snow that has fallen since December 15 of \code{year}}
#' \item{\code{janmean}}{Mean temperature (deg C) for the month of January}
#' \item{\code{janmeanmax}}{Mean maximum temperature for January}
#' \item{\code{janmeanmin}}{Mean minimum temperature for January}
#' \item{\code{janextmax}}{Extreme maximum temperature recorded in January}
#' \item{\code{janextmin}}{Extreme minimum temperature recorded in January}
#' \item{\code{febsnow}}{Amount of snow on the ground on February 15 of \code{year+1}}
#' \item{\code{febsnowfall}}{Amount of snow that has fallen since January 15 of \code{year}}
#' \item{\code{febmean}}{Mean temperature (deg C) for the month of February}
#' \item{\code{febmeanmax}}{Mean maximum temperature for February}
#' \item{\code{febmeanmin}}{Mean minimum temperature for February}
#' \item{\code{febextmax}}{Extreme maximum temperature recorded in February}
#' \item{\code{febextmin}}{Extreme minimum temperature recorded in February}
#' \item{\code{marsnow}}{Amount of snow on the ground on March 15 of \code{year+1}}
#' \item{\code{marsnowfall}}{Amount of snow that has fallen since February 15 of \code{year}}
#' \item{\code{marmean}}{Mean temperature (deg C) for the month of March}
#' \item{\code{marmeanmax}}{Mean maximum temperature for March}
#' \item{\code{marmeanmin}}{Mean minimum temperature for March}
#' \item{\code{marextmax}}{Extreme maximum temperature recorded in March}
#' \item{\code{marextmin}}{Extreme minimum temperature recorded in March}
#' \item{\code{aprsnow}}{Amount of snow on the ground on April 15 of \code{year+1}}
#' \item{\code{aprsnowfall}}{Amount of snow that has fallen since March 15 of \code{year}}
#' \item{\code{aprmean}}{Mean temperature (deg C) for the month of April}
#' \item{\code{aprmeanmax}}{Mean maximum temperature for April}
#' \item{\code{aprmeanmin}}{Mean minimum temperature for April}
#' \item{\code{aprextmax}}{Extreme maximum temperature recorded in April}
#' \item{\code{aprextmin}}{Extreme minimum temperature recorded in April}
#' \item{\code{maysnow}}{Amount of snow on the ground on May 15 of \code{year+1}}
#' \item{\code{maysnowfall}}{Amount of snow that has fallen since April 15 of \code{year}}
#' \item{\code{maymean}}{Mean temperature (deg C) for the month of May}
#' \item{\code{maymeanmax}}{Mean maximum temperature for May}
#' \item{\code{maymeanmin}}{Mean minimum temperature for May}
#' \item{\code{mayextmax}}{Extreme maximum temperature recorded in May}
#' \item{\code{mayextmin}}{Extreme minimum temperature recorded in May}
#' \item{\code{junsnow}}{Amount of snow on the ground on June 15 of \code{year+1}}
#' \item{\code{junsnowfall}}{Amount of snow that has fallen since May 15 of \code{year}}
#' \item{\code{junmean}}{Mean temperature (deg C) for the month of June}
#' \item{\code{junmeanmax}}{Mean maximum temperature for June}
#' \item{\code{junmeanmin}}{Mean minimum temperature for June}
#' \item{\code{junextmax}}{Extreme maximum temperature recorded in June}
#' \item{\code{junextmin}}{Extreme minimum temperature recorded in June}
#' \item{\code{julsnow}}{Amount of snow on the ground on July 15 of \code{year+1}}
#' \item{\code{julsnowfall}}{Amount of snow that has fallen since June 15 of \code{year}}
#' \item{\code{julmean}}{Mean temperature (deg C) for the month of July}
#' \item{\code{julmeanmax}}{Mean maximum temperature for July}
#' \item{\code{julmeanmin}}{Mean minimum temperature for July}
#' \item{\code{julextmax}}{Extreme maximum temperature recorded in July}
#' \item{\code{julextmin}}{Extreme minimum temperature recorded in July}
#' \item{\code{augsnow}}{Amount of snow on the ground on August 15 of \code{year+1}}
#' \item{\code{augsnowfall}}{Amount of snow that has fallen since July 15 of \code{year}}
#' \item{\code{augmean}}{Mean temperature (deg C) for the month of August}
#' \item{\code{augmeanmax}}{Mean maximum temperature for August}
#' \item{\code{augmeanmin}}{Mean minimum temperature for August}
#' \item{\code{augextmax}}{Extreme maximum temperature recorded in August}
#' \item{\code{augextmin}}{Extreme minimum temperature recorded in August}
#' \item{\code{sepsnow}}{Amount of snow on the ground on September 15 of \code{year+1}}
#' \item{\code{sepsnowfall}}{Amount of snow that has fallen since August 15 of \code{year}}
#' \item{\code{sepmean}}{Mean temperature (deg C) for the month of September}
#' \item{\code{sepmeanmax}}{Mean maximum temperature for September}
#' \item{\code{sepmeanmin}}{Mean minimum temperature for September}
#' \item{\code{sepextmax}}{Extreme maximum temperature recorded in September}
#' \item{\code{sepextmin}}{Extreme minimum temperature recorded in September}
#' \item{\code{octsnow}}{Amount of snow on the ground on October 15 of \code{year+1}}
#' \item{\code{octsnowfall}}{Amount of snow that has fallen since September 15 of \code{year}}
#' \item{\code{octmean}}{Mean temperature (deg C) for the month of October}
#' \item{\code{octmeanmax}}{Mean maximum temperature for October}
#' \item{\code{octmeanmin}}{Mean minimum temperature for October}
#' \item{\code{octextmax}}{Extreme maximum temperature recorded in October}
#' \item{\code{octextmin}}{Extreme minimum temperature recorded in October}
#' \item{\code{novsnow}}{Amount of snow on the ground on November 15 of \code{year+1}}
#' \item{\code{novsnowfall}}{Amount of snow that has fallen since October 15 of \code{year}}
#' \item{\code{novmean}}{Mean temperature (deg C) for the month of November}
#' \item{\code{novmeanmax}}{Mean maximum temperature for November}
#' \item{\code{novmeanmin}}{Mean minimum temperature for November}
#' \item{\code{novextmax}}{Extreme maximum temperature recorded in November}
#' \item{\code{novextmin}}{Extreme minimum temperature recorded in November}
#' \item{\code{decsnow}}{Amount of snow on the ground on December 15 of \code{year+1}}
#' \item{\code{decsnowfall}}{Amount of snow that has fallen since November 15 of \code{year}}
#' \item{\code{decmean}}{Mean temperature (deg C) for the month of December}
#' \item{\code{decmeanmax}}{Mean maximum temperature for December}
#' \item{\code{decmeanmin}}{Mean minimum temperature for December}
#' \item{\code{decextmax}}{Extreme maximum temperature recorded in December}
#' \item{\code{decextmin}}{Extreme minimum temperature recorded in December}
#' \item{\code{aprraint}}{Rainfall (mm) in April of \code{year}}
#' \item{\code{mayraint}}{Rainfall (mm) in May of \code{year}}
#' \item{\code{junraint}}{Rainfall (mm) in June of \code{year}}
#' \item{\code{julraint}}{Rainfall (mm) in July of \code{year}}
#' \item{\code{augraint}}{Rainfall (mm) in August of \code{year}}
#' \item{\code{aprraintmn1}}{Rainfall (mm) in April of \code{year-1}}
#' \item{\code{mayraintmn1}}{Rainfall (mm) in May of \code{year-1}}
#' \item{\code{junraintmn1}}{Rainfall (mm) in June of \code{year-1}}
#' \item{\code{julraintmn1}}{Rainfall (mm) in July of \code{year-1}}
#' \item{\code{augraintmn1}}{Rainfall (mm) in August of \code{year-1}}
#' \item{\code{sumraint}}{Total rainfall (mm) from May through July in \code{year}}
#' \item{\code{sumraintmn1}}{Total rainfall (mm) from May through July in \code{year-1}}
#' }
#'
#' Note: This dataset is a subset of the full Roland and Matter data. We restricted
#'   the data to subpopulations (meadows) that had an observation in each year
#'   of the time series. This is 11 out of 21 meadows.
#' For further details, see \url{https://datadryad.org/resource/doi:10.5061/dryad.tp324}
#'
"butterfly"
