#' Matrix rotation
#' 
#' internal function
#'
#' @param dta input data
#' @param degree rotation degree
#'
#' @export
#' @keywords internal
#' 
#' @examples
#' 
#' m <- matrix(data = 1:100,
#'             ncol = 2)
#' 
#' rotate(dta = m,
#'        degree = 10)
#' 
rotate <- function(dta, degree) {
  
  ## reference
  ## http://en.wikipedia.org/wiki/Rotation_matrix
  
  mat <- as.matrix(x = dta)
  
  theta <- degree / 180 * pi
  
  r <- matrix(data = c(cos(x = theta), 
                       -sin(x = theta), 
                       sin(x = theta), 
                       cos(x = theta)), 
              byrow = TRUE, 
              ncol = 2)
  mat <- (mat - colMeans(x = mat)) %*% r
  
  mat
}

#' eveRloving
#'
#' Lets yout spread the love for/of (R) everywhere...
#'
#' @param love Character string containing your name and the name of your beloved one
#' @param size grid size (of your love)
#' @param colour1,colour2 hearts colours
#' @param n.hearts number of hearts within the grid
#'
#' @export
#' @import ggplot2 data.table
#' @examples 
#' 
#' library(RFun)
#' 
#' eveRloving()
#' 
#' eveRloving('Phill + Caroline', 200, 100, '#1f3b51', '#aac6dc')
#' 
eveRloving <- function(love = NULL, size = 150, n.hearts = 50, colour1 = 'hotpink', colour2 = 'red4') {
  
  numbers <- id <- x <- y <- NULL
  
  dt <- data.table(numbers = seq(from = 0, 
                                 to = 2 * pi, 
                                 by = 0.01))
  
  dt <- dt[, `:=` (x = 16 * sin(x = numbers) ^ 3,
                   y = 13 * cos(x = numbers) - 5 * 
                     cos(x = 2 * numbers) - 2 * 
                     cos(x = 3 * numbers) - 
                     cos(x = 4 * numbers))]
  
  hearts <- list()
  
  for (i in 1:n.hearts) {
    hearts[[i]] <- cbind(rotate(dt[,2:3] * sample(seq(from = .5, 
                                                      to = 1.5,
                                                      by = .01), 1), 
                                sample(-30:30, 1)),i)
    for(j in 1:2) {
      hearts[[i]][,j] <- hearts[[i]][,j] + sample(-size:size, 1)
    }
  }
  
  gg_hearts <- data.table(do.call(what = rbind, 
                                  args = hearts))
  
  names(gg_hearts) <- c('x', 'y', 'id')
  
  h <- ggplot(data = gg_hearts,
              mapping = aes(x = x, 
                            y = y, 
                            group = factor(id), 
                            color = id, 
                            fill = id)) +
    geom_polygon(show.legend = FALSE, 
                 alpha = .75) +
    scale_color_continuous(low = colour1, 
                           high = colour2) +
    scale_fill_continuous(low = colour1, 
                          high = colour2) +
    ggtitle(bquote(bold(.(love)))) +
    coord_fixed() +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5))
  
  h
}
