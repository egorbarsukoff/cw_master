
A <- function(x, a, b, l) {
  return((1-l)*a + l * sapply(b, function (s) sum(x^(s+1))/sum(x^s)))
}

f <- function(init, a, b, l, eps) {
  while (TRUE) {
    new_x <- A(init, a, b, l)
    if (norm(new_x - init, type="2") < eps) {
      return(new_x)
    }
    init <- new_x
  }
}
