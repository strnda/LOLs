library(ggplot2, quietly = T)
library(data.table, quietly = T)

#' ...
#'
#' @param name Enter your name (as character string) to reveal your biggest secret...
#'
#' @return
#' @export
#'
#' @examples
secret <- function(name = 'Yannis') {

  x <- seq(0, 2*pi, length = (15000))
  d0 <- data.table(x = 8*cos(x), y = 4*sin(x))

  x <- c(seq(3, 7, 0.001), seq(-7, -3, 0.001))
  d1 <- data.table(x = x, y = c(3*sqrt(1-(x/7)^2), -(3*sqrt(1-(x/7)^2))))
  d1 <- d1[d1$y > -3*sqrt(33)/7,]

  d1.1 <- d1[(d1$y > 0) & (d1$x > 0),]
  d1.1 <- rbind(list(min(d1.1$x), 0), d1.1)
  d1.1 <- rbind(d1.1, list(max(d1.1$x), 0))

  d1.2 <- d1[(d1$y > 0) & (d1$x < 0),]
  d1.2 <- rbind(d1.2, list(max(d1.2$x), 0))
  d1.2 <- rbind(list(min(d1.2$x), 0), d1.2)

  d1.3 <- d1[(d1$y < 0) & (d1$x > 0),]
  d1.3 <- rbind(list(min(d1.3$x), 0), d1.3)
  d1.3 <- rbind(d1.3, list(max(d1.3$x), 0))

  d1.4 <- d1[(d1$y < 0) & (d1$x < 0),]
  d1.4 <- rbind(d1.4, list(max(d1.4$x), 0))
  d1.4 <- rbind(list(min(d1.4$x), 0), d1.4)

  x <- seq(-4, 4, 0.001)
  d2 <- data.table(x = x, y = abs(x/2)-(3*sqrt(33)-7)*x^2/112-3+sqrt(1-(abs(abs(x)-2)-1)^2))
  d2 <- rbind(list(min(d2$x), 0), d2)
  d2 <- rbind(d2, list(max(d2$x), 0))

  x <- c(seq(0.75, 1, 0.001), seq(-1, -0.75 , 0.001))
  d3 <- data.table(x = x, y = 9-8*abs(x))

  d3.1 <- d3[d3$x > 0,]
  d3.1 <- rbind(list(min(d3.1$x), 0), d3.1)
  d3.1 <- rbind(d3.1, list(max(d3.1$x), 0))

  d3.2 <- d3[d3$x < 0,]
  d3.2 <- rbind(d3.2, list(max(d3.2$x), 0))
  d3.2 <- rbind(list(min(d3.2$x), 0), d3.2)

  x <- c(seq(0.5, 0.75, 0.001), seq(-0.75, -0.5, 0.001))
  d4 <- data.table(x = x, y = 3*abs(x)+0.75)

  d4.1 <- d4[d4$x > 0,]
  d4.1 <- rbind(list(min(d4.1$x), 0), d4.1)
  d4.1 <- rbind(d4.1, list(max(d4.1$x), 0))

  d4.2 <- d4[d4$x < 0,]
  d4.2 <- rbind(d4.2, list(max(d4.2$x), 0))
  d4.2 <- rbind(list(min(d4.2$x), 0), d4.2)

  x <- seq(-0.5, 0.5, 0.001)
  d5 <- data.table(x = x, y = rep(2.25, length(x)))
  d5 <- rbind(list(min(d5$x), 0), d5)
  d5 <- rbind(d5, list(max(d5$x), 0))

  x <- c(seq(-3, -1, 0.001), seq(1, 3, 0.001))
  d6 <- data.table(x = x, y  = 6 * sqrt(10)/7+(1.5-0.5*abs(x))*sqrt(abs(abs(x)-1)/(abs(x)-1))-6*sqrt(10)*sqrt(4-(abs(x)-1)^2)/14)

  d6.1 <- d6[d6$x > 0,]
  d6.1[is.na(d6.1)] = 0
  d6.1 <- rbind(d6.1, list(max(d6.1$x), 0))

  d6.2 <- d6[d6$x < 0,]
  d6.2[is.na(d6.2)] = 0
  d6.2 <- rbind(list(min(d6.2$x), 0), d6.2)

  bat <- ggplot() +
    geom_polygon(data = d0, aes(x = x, y = y), color = 'black', fill = 'goldenrod', lwd = 5) +
    geom_polygon(data = d1.1, aes(x = x, y = y), color = 'black', fill = 'black') +
    geom_polygon(data = d1.2, aes(x = x, y = y), color = 'black', fill = 'black') +
    geom_polygon(data = d1.3, aes(x = x, y = y), color = 'black', fill = 'black') +
    geom_polygon(data = d1.4, aes(x = x, y = y), color = 'black', fill = 'black') +
    geom_polygon(data = d2, aes(x = x, y = y), color = 'black', fill = 'black') +
    geom_polygon(data = d3.1, aes(x = x, y = y), color = 'black', fill = 'black') +
    geom_polygon(data = d3.2, aes(x = x, y = y), color = 'black', fill = 'black') +
    geom_polygon(data = d4.1, aes(x = x, y = y), color = 'black', fill = 'black') +
    geom_polygon(data = d4.2, aes(x = x, y = y), color = 'black', fill = 'black') +
    geom_polygon(data = d5, aes(x = x, y = y), color = 'black', fill = 'black') +
    geom_polygon(data = d6.1, aes(x = x, y = y), color = 'black', fill = 'black') +
    geom_polygon(data = d6.2, aes(x = x, y = y), color = 'black', fill = 'black') +
    ggtitle(paste(name,'is BATMAN ! ! !')) +
    theme_classic() +
    coord_fixed()

  return(bat)
}

?secret
