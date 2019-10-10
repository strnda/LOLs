#' smiley
#'
#' puts a smile in your plot tab on a rainy day
#'
#' @param joke LOGICAL Make it all a F-ing joke? (Watchmen comic-book reference)
#'
#' @export
#' @import ggplot2 data.table
#'
#' @examples 
#' 
#' library(RFun)
#'
#' smiley() + theme_void()
#' 
#' #' smiley(joke = TRUE)
#' 
smiley <- function(joke = FALSE) {
  
  x <- y <- NULL
  
  x <- seq(from = 0, 
           to = 2 * pi, 
           length = 2500)
  
  d0 <- data.table(x = sin(x = x) * 25, 
                   y = cos(x = x) * 25)

  d1 <- d0[(abs(x = d0[, x]) < 15) & (d0[, y] < 10),]
  d1 <- d1[, y := y + 7.5]

  d1.1 <- d1 * 1.25
  d1.1 <- d1.1[, y := y + 5.5]
  d1.1 <- d1.1[(abs(x = d1.1[, x]) < max(abs(d1[, x]))) & 
                 (d1.1[, y] < max(d1[, y])),]
  d1.1 <- rbind(d1.1, d1)
  
  d1.2.1 <- data.table(x = min(d1[, x]) + (2 * cos(x = x) * 
                                             cos(x = 1) - 8 * sin(x = x) * 
                                             sin(x = -2)) * .15, 
                       y = max(d1[, y]) + (2 * cos(x = x) * sin(x = -2) +
                                             8 * sin(x = x) * 
                                             cos(x = .725)) * .15)
  d1.2.2 <- data.table(x = max(d1[, x]) + (2 * cos(x = x) * cos(x = 1) - 
                                             8 * sin(x = x) * 
                                             sin(x = 2)) * .15, 
                       y = max(d1[, y]) + (2 * cos(x = x) * sin(x = 2) + 
                                             8*sin(x = x) * 
                                             cos(x = .725)) * .15)
  
  d2 <- data.table(x = 10 + 4 * cos(x = x),
                   y = 7.5 + 8 * sin(x = x))
  
  if(joke) {
    
    d3.1 <- data.table(x = - 15 + sin(x = x) * 3.5,
                       y = 15 + cos(x = x) * 3.5)
    
    d3.2 <- data.table(x = - 5 + sin(x = x) * .75, 
                       y = 5 + cos(x = x) * .75)
    
    d3.3 <- data.table(x = - 15 + sin(x = x) * .25,
                       y = 10 + cos(x = x)*.25)
    
    d3.4 <- data.table(x = - 7.5 + sin(x = x) * .25, 
                       y = 15 + cos(x = x) * .25)
    
    d3.5 <- data.table(x = - 16.5 + sin(x = x) * .5, 
                       y = 7.5 + cos(x = x) * .5)
  }
  
  face <- ggplot() +
    geom_polygon(data = d0, 
                 mapping = aes(x = x, 
                               y = y), 
                 fill = 'gold', 
                 color = 'black', 
                 size = 5) +
    geom_line(data = d1.1,
              mapping = aes(x = x, 
                            y = y), 
              color = 'black', 
              size = 2) +
    geom_polygon(data = d1.2.1,
                 mapping = aes(x = x, 
                               y = y), 
                 fill = 'black', 
                 color = 'black') +
    geom_polygon(data = d1.2.2, 
                 mapping = aes(x = x,
                               y = y), 
                 fill = 'black',
                 color = 'black') +
    geom_polygon(data = d2, 
                 mapping = aes(x = x, 
                               y = y), 
                 fill = 'black', 
                 color = 'black') +
    geom_polygon(data = d2, 
                 mapping = aes(x = -x, 
                               y = y), 
                 fill = 'black', 
                 color = 'black') +
    coord_fixed() +
    theme_classic()
  
  if(joke) {
    face <- face +
      theme_void() +
      geom_polygon(data = d3.1, 
                   mapping = aes(x = x, 
                                 y = y), 
                   fill = 'red4', 
                   color = 'red4') +
      geom_polygon(data = d3.2, 
                   mapping = aes(x = x, 
                                 y = y), 
                   fill = 'red4', 
                   color = 'red4') +
      geom_segment(mapping = aes(x = mean(d3.1[, x]), 
                                 y = mean(d3.1[, y]), 
                                 xend = mean(d3.2[, x]), 
                                 yend = mean(d3.2[, y])), 
                   color = 'red4', 
                   size = 2.5) +
      geom_polygon(data = d3.3, 
                   mapping = aes(x = x, 
                                 y = y), 
                   fill = 'red4', 
                   color = 'red4') +
      geom_segment(mapping = aes(x = mean(d3.1[, x]) - 1.5, 
                                 y = mean(d3.1[, y]), 
                                 xend = mean(d3.3[, x]), 
                                 yend = mean(d3.3[, y])), 
                   color = 'red4', 
                   size = 1.5) +
      geom_polygon(data = d3.4, 
                   mapping = aes(x = x, 
                                 y = y), 
                   fill = 'red4', 
                   color = 'red4') +
      geom_segment(mapping = aes(x = mean(d3.1[, x]), 
                                 y = mean(d3.1[, y]) + 2, 
                                 xend = mean(d3.4[, x]), 
                                 yend = mean(d3.4[, y])), 
                   color = 'red4', 
                   size = 1.5) +
      geom_polygon(data = d3.5, 
                   mapping = aes(x = x, 
                                 y = y), 
                   fill = 'red4', 
                   color = 'red4') +
      geom_segment(mapping = aes(x = mean(d3.1[, x]) - 2, 
                                 y = mean(d3.1[, y]) + 2, 
                                 xend = mean(d3.5[, x]), 
                                 yend = mean(d3.5[, y])), 
                   color = 'red4', 
                   size = 2) +
      ggtitle(label = 'Quis custodiet ipsos custodes?') +
      theme(panel.background = element_rect(fill = 'red4'),
            plot.title = element_text(size = 20, 
                                      hjust = 0.5, 
                                      family = 'NewCenturySchoolbook', 
                                      face = 'italic'))
  }
  
  face
}