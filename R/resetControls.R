### conversion function: to (mega or kilo)joules -----

toJoules <- function(x, y = 3600L, unit = c("MJ", "KJ", "J")) {

  ## convert to joules
  out <- x * y

  ## if required, convert to kilojoules or megajoules
  if (unit[1] == "MJ") {
    out / 10^6
  } else if (unit[1] == "KJ") {
    out / 10^3
  } else {
    out
  }
}
