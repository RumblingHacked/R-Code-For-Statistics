iterated_integral <- function(xl, xu, yl, yu, f, dx){
  # computes the iterated integral of f over the region defined by
  # xl < x < xu and yl < y < yu
  # Args:
  #   xl: lower bound for x, as a function of y
  #   xu: upper bound for x, as a function of y
  #   yl: lower bound for y, as a function of x
  #   yu: upper bound for y, as a function of x
  #   dx: 1 means integrate x first (on the inside) and then y
  #   f : the integrand as a function of x and y
  #
  # Returns: the iterated integral of f over the region defined by xl, xu, yl, and yu
  if (dx == 1){
    integrate(function(y)
    {sapply(y, function(y)
    {integrate(function(x){f(x,y)}, lower = xl(y), upper=xu(y))$value})},
    lower = yl(x), upper = yu(x))
  } else {
    integrate(function(x)
    {sapply(x, function(x)
    {integrate(function(y){f(x,y)}, lower = yl(x), upper=yu(x))$value})},
    lower = xl(y), upper = xu(y))}
}
