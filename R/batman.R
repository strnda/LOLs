#' secRet
#'
#' Reveals your biggest secret...
#' 
#' AKA Batman equation in polygons and ggplot2 (extention of \href{https://www.r-bloggers.com/the-batman-equation/}{R-bloggers article}).
#'
#' @param name Enter your name (character string) to reveal your biggest secret...
#'
#' @export
#' @import ggplot2 data.table
#'
#' @examples 
#' 
#' library(LOLs)
#' 
#' secRet('Filip Strnad')
#' 
secRet <- function(name = 'Bruce Wayne') {
  
  x <- y <- NULL
  
  x <- seq(from = 0, 
           to = 2 * pi, 
           length = 15000)
  d0 <- data.table(x = 8 * cos(x = x), 
                   y = 4 * sin(x = x))
  
  x <- c(seq(from = 3, 
             to = 7, 
             by = 0.001),
         seq(from = -7,
             to =  -3,
             by = 0.001))
  d1 <- data.table(x = x, 
                   y = c(3 * sqrt(x = 1 - (x / 7) ^ 2),
                         -(3 * sqrt(x = 1 - (x / 7) ^ 2))))
  d1 <- d1[d1[, y] > -3 * sqrt(x = 33) / 7,]
  
  d1.1 <- d1[(d1[, y] > 0) & (d1[, x] > 0),]
  d1.1 <- rbind(list(min(d1.1[, x]), 0), d1.1)
  d1.1 <- rbind(d1.1, list(max(d1.1[, x]), 0))
  
  d1.2 <- d1[(d1[, y] > 0) & (d1[, x] < 0),]
  d1.2 <- rbind(d1.2, list(max(d1.2[, x]), 0))
  d1.2 <- rbind(list(min(d1.2[, x]), 0), d1.2)
  
  d1.3 <- d1[(d1[, y] < 0) & (d1[, x] > 0),]
  d1.3 <- rbind(list(min(d1.3[, x]), 0), d1.3)
  d1.3 <- rbind(d1.3, list(max(d1.3[, x]), 0))
  
  d1.4 <- d1[(d1[, y] < 0) & (d1[, x] < 0),]
  d1.4 <- rbind(d1.4, list(max(d1.4[, x]), 0))
  d1.4 <- rbind(list(min(d1.4[, x]), 0), d1.4)
  
  x <- seq(from = -4,
           to = 4, 
           by = 0.001)
  d2 <- data.table(x = x, 
                   y = abs(x = x / 2) - (3 * sqrt(x = 33) - 7) * x ^ 2 / 
                     112 - 3 + sqrt(x = 1 - (abs(abs(x = x) - 2) - 1) ^ 2))
  d2 <- rbind(list(min(d2[, x]), 0), d2)
  d2 <- rbind(d2, list(max(d2[, x]), 0))
  
  x <- c(seq(from = 0.75,
             to = 1,
             by = 0.001), 
         seq(from = -1, 
             to = -0.75, 
             by = 0.001))
  d3 <- data.table(x = x, 
                   y = 9 - 8 * abs(x = x))
  
  d3.1 <- d3[d3[, x] > 0,]
  d3.1 <- rbind(list(min(d3.1[, x]), 0), d3.1)
  d3.1 <- rbind(d3.1, list(max(d3.1[, x]), 0))
  
  d3.2 <- d3[d3[, x] < 0,]
  d3.2 <- rbind(d3.2, list(max(d3.2[, x]), 0))
  d3.2 <- rbind(list(min(d3.2[, x]), 0), d3.2)
  
  x <- c(seq(from = 0.5, 
             to = 0.75, 
             by = 0.001), 
         seq(from = -0.75, 
             to = -0.5, 
             by = 0.001))
  d4 <- data.table(x = x, 
                   y = 3 * abs(x = x) + 0.75)
  
  d4.1 <- d4[d4[, x] > 0,]
  d4.1 <- rbind(list(min(d4.1[, x]), 0), d4.1)
  d4.1 <- rbind(d4.1, list(max(d4.1[, x]), 0))
  
  d4.2 <- d4[d4[, x] < 0,]
  d4.2 <- rbind(d4.2, list(max(d4.2[, x]), 0))
  d4.2 <- rbind(list(min(d4.2[, x]), 0), d4.2)
  
  x <- seq(from = -0.5, 
           to = 0.5, 
           by = 0.001)
  d5 <- data.table(x = x, 
                   y = rep(x = 2.25, length(x = x)))
  d5 <- rbind(list(min(d5[, x]), 0), d5)
  d5 <- rbind(d5, list(max(d5[, x]), 0))
  
  x <- c(seq(from = -3, 
             to = -1, 
             by = 0.001), 
         seq(from = 1, 
             to = 3, 
             by = 0.001))
  d6 <- data.table(x = x, 
                   y  = 6 * sqrt(x = 10) / 7 + (1.5 - 0.5 * abs(x = x)) * 
                     sqrt(x = abs(x = abs(x = x) - 1) / 
                            (abs(x = x) - 1)) - 6 * sqrt(x = 10) * 
                     sqrt(4 - (abs(x = x) - 1) ^ 2) / 14)
  
  d6.1 <- d6[d6[, x] > 0,]
  d6.1[is.na(d6.1)] = 0
  d6.1 <- rbind(d6.1, list(max(d6.1[, x]), 0))
  
  d6.2 <- d6[d6[, x] < 0,]
  d6.2[is.na(d6.2)] = 0
  d6.2 <- rbind(list(min(d6.2[, x]), 0), d6.2)
  
  bat <- ggplot() +
    geom_polygon(data = d0, 
                 mapping = aes(x = x,
                               y = y), 
                 color = 'black', 
                 fill = 'goldenrod', 
                 lwd = 5) +
    geom_polygon(data = d1.1, 
                 mapping = aes(x = x, 
                               y = y), 
                 color = 'black',
                 fill = 'black') +
    geom_polygon(data = d1.2, 
                 mapping = aes(x = x,
                               y = y), 
                 color = 'black', 
                 fill = 'black') +
    geom_polygon(data = d1.3, 
                 mapping = aes(x = x, 
                               y = y), 
                 color = 'black',
                 fill = 'black') +
    geom_polygon(data = d1.4, 
                 mapping = aes(x = x,
                               y = y), 
                 color = 'black', 
                 fill = 'black') +
    geom_polygon(data = d2, 
                 mapping = aes(x = x, 
                               y = y), 
                 color = 'black', 
                 fill = 'black') +
    geom_polygon(data = d3.1, 
                 mapping = aes(x = x, 
                               y = y), 
                 color = 'black', 
                 fill = 'black') +
    geom_polygon(data = d3.2,
                 mapping = aes(x = x, 
                               y = y), 
                 color = 'black',
                 fill = 'black') +
    geom_polygon(data = d4.1, 
                 mapping = aes(x = x, 
                               y = y), 
                 color = 'black',
                 fill = 'black') +
    geom_polygon(data = d4.2, 
                 mapping = aes(x = x, 
                               y = y), 
                 color = 'black', 
                 fill = 'black') +
    geom_polygon(data = d5, 
                 mapping = aes(x = x, 
                               y = y), 
                 color = 'black',
                 fill = 'black') +
    geom_polygon(data = d6.1, 
                 mapping = aes(x = x, 
                               y = y), 
                 color = 'black', 
                 fill = 'black') +
    geom_polygon(data = d6.2, 
                 mapping = aes(x = x, 
                               y = y), 
                 color = 'black', 
                 fill = 'black') +
    ggtitle(paste(name,
                  'is BATMAN ! ! !')) +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5)) +
    coord_fixed()
  
  bat
}
