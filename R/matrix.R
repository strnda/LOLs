#' matRix
#' 
#' Unfortunately, no one can be told what the Matrix is. You have to see it for yourself.
#' 
#' Extention of \href{https://www.r-bloggers.com/matrix-style-screensaver-in-r/}{R-bloggers article}.
#' 
#'
#' @param size.x size of x axis
#' @param size.y size of y axis
#' @param n max number of strings
#'
#' @export
#' @import ggplot2 data.table stats
#'
#' @examples
#' 
#' library(LOLs)
#' 
#' matRix()
#' 
matRix <- function(size.x = 150, size.y = 100, n = 120) {
  
  label <- size <- NULL
  
  x <- sample(x = 1:size.x, 
              size = n, 
              replace = TRUE)
  
  y <- seq(from = -1,
           to = -size.y, 
           length = n)
  
  codes <- matrix(data = 0:127,
                  nrow = 8,
                  ncol = 16, 
                  byrow = TRUE, 
                  dimnames = list(0:7, 
                                  c(0:9, letters[1:6])))
  
  ascii <- c(apply(X = codes,
                   MARGIN = c(1, 2),
                   FUN = intToUtf8), 
             letters, 
             1:10) 
  
  ascii <- sort(ascii)
  ascii <- ascii[-c(1:37)]
  
  pnts <- lapply(seq_along(x), function(i){
    
    aux <- sample(x = 1:size.y, 
                  size = 1)
    
    out <- data.table(
      x = rep(x = x[i], 
              times = aux),
      y = y[1:aux], 
      label = sample(x = ascii, 
                     size = aux, 
                     replace = TRUE), 
      alpha = runif(n = aux, 
                    min = 0.1), 
      size = sample(x = 10:20, 
                    size = aux, 
                    replace = TRUE))
  })
  
  dta <- rbindlist(l = pnts, 
                   idcol = 'col')
  
  mat <- ggplot(data = dta) +
    geom_text(mapping = aes(x = x, 
                            y = y, 
                            label = label,
                            colour = col,
                            alpha = alpha,
                            size = size),
              show.legend = FALSE) +
    scale_color_gradient(low = '#00FF199D', 
                         high = '#00FF1982') +
    theme_void() +
    theme(panel.background = element_rect(fill = 'black'))
  
  mat
}


