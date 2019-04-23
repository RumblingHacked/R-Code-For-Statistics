# Quiz 9
f <- function(x){3 * x ^ 2 * (0 < x & x < 1)}

EX <- integrate(f = function(x){x * f(x)}, lower = 0, upper = 1)$value
EX
EX2 <- integrate(f = function(x){x ^ 2 * f(x)}, lower = 0, upper = 1)$value
EX2
VX <- EX2 - EX ^ 2
VX
