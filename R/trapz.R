


#' @title trapz and cumtrapz
#' 
#' @description ..
#' 
#' @param x returned value of function \link[stats]{density}
#' 
#' @details ..
#' 
#' @returns 
#' Function [trapz.density()] returns a \link[base]{numeric} scalar.
#' 
#' @examples 
#' rnorm(1e2) |> density() |> trapz.density()
#' 
#' @name trapz.density
#' @keywords internal
#' @importFrom pracma trapz
#' @export
trapz.density <- function(x) {
  
  if (!inherits(x, what = 'density')) stop('input must be convertible to density')
  
  if (anyNA(x$x) || anyNA(x$y)) stop('R package \'stats\' updated?')
  
  trapz(x = x$x, y = x$y) |>
    min(1) # trapzoid could be larger than density curve AUC
  
}



#' @rdname trapz.density
#' @returns 
#' Function [cumtrapz.density()] returns a \link[base]{numeric} \link[base]{matrix}.
#' @examples
#' rnorm(1e2) |> density(n = 16L) |> cumtrapz.density()
#' @importFrom pracma cumtrapz
#' @export
cumtrapz.density <- function(x) {
  
  if (!inherits(x, what = 'density')) stop('input must be convertible to density')
  
  if (anyNA(x$x) || anyNA(x$y)) stop('R package \'stats\' updated?')
  
  cumtrapz(x = x$x, y = x$y) |>
    pmin(1) # trapzoid could be larger than density curve AUC
  
}



