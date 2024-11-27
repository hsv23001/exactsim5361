# Functions
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'


# n = sample size
# seed = set seed
# x = vector that will keep the n sample values

simexp <- function(n, lam, seed = 1) {
  result <- list()
  # fixing the random seed for reproducibility
  set.seed(seed)
  result$x <- vector()
  for(i in 1:n){
    u = runif(1,0,1)
    result$x = c(result$x, -log(u)/lam)
  }

  result$plot <- data.frame(xvar = result$x) %>% ggplot2::ggplot(aes(xvar)) +
    geom_histogram(aes(y = ..density..), bins = 50) + xlim(0, NA) +
    stat_function(fun = function(x) dexp(x,lam), color = "red") +
    ggtitle(paste0("Exp(", lam, ")")) + geom_rug()

  return(result)
}
