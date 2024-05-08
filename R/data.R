#' Census data for Angrist and Krueger (1991).
#'
#' @format A tibble with 1,063,634 rows and 6 columns:
#' \describe{
#' \item{log_wage}{Log weekly wage.}
#' \item{education}{Years of education.}
#' \item{quarter_birth}{Quarter of birth.}
#' \item{birthyear}{Indicator variable for Hispanic.}
#' \item{census}{Year of census observation comes from.}
#' \item{race}{Respondent race (white or black).}
#' }
#' @source Angrist Data Archive.
"ak1991"

#' Orange juice sales data collected in the 1990s from Dominick's, which was a Chicago-area chain store.
#'
#' @format A tibble with 28,947 rows and 4 columns:
#' \describe{
#' \item{sales}{Sales volume ($).}
#' \item{price}{Price ($).}
#' \item{brand}{Orange juice brand.}
#' \item{feat}{Indicator for whether brand was advertised that week or not.}
#' }
#' @source Kilts Center at the University of Chicago's Booth School of Business.
"oj"

#' Text of the Gettysburg Address.
#'
#' Famous speech about America's national purpose.
#' @format A character.
#' @source President Abraham Lincoln, 16th President of the United States of America.
"gettysburg"

#' Extract from the Current Population Survey.
#'
#' The main labor force suvery in the United States.
#'
#' @format A tibble with 691,069 rows and 16 columns:
#' \describe{
#' \item{   age}{Age in years.}
#' \item{   year}{Year of survey.}
#' \item{   wage}{Hourly wage.}
#' \item{   hours_lastweek}{Hours worked last week.}
#' \item{   employed}{Indicator variable for whether employed.}
#' \item{   education_category}{Three educationa categories.}
#' \item{   educ_years}{Years of education.}
#' \item{   black}{Indicator for black.}
#' \item{   white}{Indicator for white.}
#' \item{   female}{Indicator for female.}
#' \item{   married}{Indicator for married}
#' \item{   single}{Indicator for single.}
#' \item{   divorced}{Indicator for divorced}
#' \item{   state}{State of residence.}
#' \item{   region}{Region of residence.}
#' \item{   sampling_weight}{Sampling weight.}
#' }
#' @source IPUMS at University of Minnesota.
"cps"

#' Expenditure data from a hypothetical household survey.
#'
#' Data used for class.
#'
#' @format A tibble with 100 rows and 4 columns:
#' \describe{
#' \item{   id}{Respondent id.}
#' \item{   food}{Expenditure on food.}
#' \item{   clothing}{Expenditure on clothing.}
#' \item{   housing}{Expenditure on housing.}
#' \item{   alcohol}{Expenditure on alcohol.}
#' }
#' @source Created by Andrew S. Griffen
"expenditure_data1"

#' Expenditure data from a hypothetical household survey.
#'
#' Data used for class.
#'
#' @format A tibble with 100 rows and 201 columns:
#' \describe{
#' \item{   id}{Respondent id.}
#' \item{   item"i"}{Expenditure on item i.}
#' }
#' @source Created by Andrew S. Griffen
"expenditure_data2"

#' Time series data on life expectancy for OECD countries.
#'
#' Data used for class.
#'
#' @format A tibble with 100 rows and 201 columns:
#' \describe{
#' \item{   country}{Country.}
#' \item{   year}{Year.}
#' \item{   sex}{Sex.}
#' \item{   life_expectancy}{Life expectancy in years.}
#' }
#' @source Organisation for Economic Co-operation and Development (OECD) data
"oecd"

#' Estimates from Capital Asset Pricing Model for various stocks.
#'
#' Data used for class as an example of pivot_wider.
#'
#' @format A tibble with 1,096 rows and 3 columns:
#' \describe{
#' \item{   ticker}{Stock ticker.}
#' \item{   parameter}{Alpha and beta estimates.}
#' \item{   estimate}{Point estimate.}
#' }
#' @source Estimated from data used in Matt Taddy's Business Data Science
"capm"

#' Locations from the novel Blood Meridian: or, The Evening Redness in the West by Cormac McCarthy.
#'
#' Data and decriptions from the wonderful Twitter thread \url{https://twitter.com/VanDiemen_/status/1787491589948076444}
#'
#' @format A tibble with 19 rows and 4 columns:
#' \describe{
#' \item{   location}{Place name.}
#' \item{   description}{Description of location.}
#' \item{   lng}{Longitude.}
#' \item{   lat}{Latitude.}
#' }
#' @source '@VanDiemen_'
"blood_meridian"
