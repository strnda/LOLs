#' mondRian
#' 
#'  Recreates famous Piet Mondrians painting in ggplot2, hopefully there will be a random Mondrian painting generator included in the future.
#'
#' Piet Mondrian - Composition with Large Red Plane, Yellow, Black, Grey and Blue, 1921
#'
#' @param initials Character string of you initials
#' @param title LOGICAL Include funny painting title
#'
#' @export
#' @import ggplot2
#'
#' @examples 
#' 
#' library(RFun)
#' 
#' mondRian()
#' 
#' mondRian('FS') + theme_void()
#' 
mondRian <- function(initials = 'PM', title = FALSE) {
  
  initials <- paste(unlist(x = strsplit(x = toupper(x = initials), 
                                        split = '')),
                    collapse = ' ')
  
  
  mond <- ggplot(data = NULL) +
    geom_rect(mapping = aes(xmin = 0, 
                            xmax = 22, 
                            ymin = 0, 
                            ymax = 22), 
              fill = 'grey65', 
              color = NA) +
    geom_rect(mapping = aes(xmin = .5, 
                            xmax = 21.5, 
                            ymin = .5, 
                            ymax = 21.5), 
              fill = '#D6D6D0', 
              color = NA) +
    geom_rect(mapping = aes(xmin = 3, 
                            xmax = 14, 
                            ymin = 7.5, 
                            ymax = 19), 
              fill = '#BC422D', 
              color = NA) +
    geom_rect(mapping = aes(xmin = 20, 
                            xmax = 21.5,
                            ymin = .5,
                            ymax = 5.25), 
              fill = '#BC422D', 
              color = NA) +
    geom_rect(mapping = aes(xmin = 14, 
                            xmax = 20,
                            ymin = 13,
                            ymax = 21.5), 
              fill = '#E1BB4E',
              color = NA) +
    geom_rect(mapping = aes(xmin = 14, 
                            xmax = 20,
                            ymin = 13, 
                            ymax = 21.5),
              fill = '#E1BB4E',
              color = NA) +
    geom_rect(mapping = aes(xmin = .5, 
                            xmax = 3, 
                            ymin = .5, 
                            ymax = 5.25), 
              fill = '#E1BB4E', 
              color = NA) +
    geom_rect(mapping = aes(xmin = 3, 
                            xmax = 8.5, 
                            ymin = 3, 
                            ymax = 7.5),
              fill = '#181918', 
              color = NA) +
    geom_rect(mapping = aes(xmin = 8.5,
                            xmax = 14, 
                            ymin = 2, 
                            ymax = 3),
              fill = '#181918', 
              color = NA) +
    geom_rect(mapping = aes(xmin = 14, 
                            xmax = 20, 
                            ymin = 2, 
                            ymax = 5.25),
              fill = '#203F6D', 
              color = NA) +
    geom_rect(mapping = aes(xmin = 20,
                            xmax = 21.5, 
                            ymin = 5.5, 
                            ymax = 21.5), 
              fill = '#c3c3c0',
              color = NA) +
    geom_rect(mapping = aes(xmin = 8.5,
                            xmax = 14, 
                            ymin = 3.5, 
                            ymax = 7.5), 
              fill = '#c3c3c0', 
              color = NA) +
    geom_rect(mapping = aes(xmin = .5, 
                            xmax = 3, 
                            ymin = 5.25, 
                            ymax = 13),
              fill = '#c3c3c0',
              color = NA) +
    geom_rect(mapping = aes(xmin = .5,
                            xmax = 5.75,
                            ymin = 19,
                            ymax = 21.5),
              fill = '#c3c3c0', 
              color = NA) +
    geom_segment(mapping = aes(x = 1,
                               y = 19, 
                               xend = 20, 
                               yend = 19), 
                 colour = '#181911',
                 size = 3.25) +
    geom_segment(mapping = aes(x = 3,
                               y = 1, 
                               xend = 3,
                               yend = 19), 
                 colour = '#181911',
                 size = 3.25) +
    geom_segment(mapping = aes(x = 20, 
                               y = 1, 
                               xend = 20, 
                               yend = 21), 
                 colour = '#181911',
                 size = 3.25) +
    geom_segment(mapping = aes(x = 8.5, 
                               y = 2, 
                               xend = 20, 
                               yend = 2),
                 colour = '#181911', 
                 size = 3.25) +
    geom_segment(mapping = aes(x = 3,
                               y = 3, 
                               xend = 14,
                               yend = 3), 
                 colour = '#181911',
                 size = 3.25) +
    geom_segment(mapping = aes(x = 1, 
                               y = 5.25, 
                               xend = 21.5,
                               yend = 5.25), 
                 colour = '#181911', 
                 size = 3.25) +
    geom_segment(mapping = aes(x = 14, 
                               y = 2,
                               xend = 14, 
                               yend = 21), 
                 colour = '#181911', 
                 size = 3.25) +
    geom_segment(mapping = aes(x = 3,
                               y = 7.5,
                               xend = 20, 
                               yend = 7.5),
                 colour = '#181911',
                 size = 3.25) +
    geom_segment(mapping = aes(x = 8.5, 
                               y = 1, 
                               xend = 8.5, 
                               yend = 7.5), 
                 colour = '#181911',
                 size = 3.25) +
    geom_segment(mapping = aes(x = 5.75,
                               y = 3, 
                               xend = 5.75, 
                               yend = 7.5), 
                 colour = '#181911', 
                 size = 3.25) +
    geom_segment(mapping = aes(x = 5.75, 
                               y = 19, 
                               xend = 5.75,
                               yend = 21),
                 colour = '#181911',
                 size = 3.25) +
    geom_segment(mapping = aes(x = 1, 
                               y = 13, 
                               xend = 3, 
                               yend = 13), 
                 colour = '#181911',
                 size = 3.25) +
    geom_segment(mapping = aes(x = 14, 
                               y = 13, 
                               xend = 20, 
                               yend = 13), 
                 colour = '#181911', 
                 size = 3.25) +
    geom_segment(mapping = aes(x = 17, 
                               y = 7.5, 
                               xend = 17, 
                               yend = 13), 
                 colour = '#181911',
                 size = 3.25) +
    geom_text(mapping = aes(x = 10, 
                            y = 1.25),
              colour = '#181911', 
              size = 2, 
              label = paste(initials, 
                            format(x = Sys.Date(), 
                                   format = '%y')),
              fontface = 'bold',
              alpha = .75) +
    coord_fixed() +
    labs(x = '', 
         y = '', 
         title = ifelse(test = title, 
                        yes = expression(paste('the a', 
                                               bold('R'),
                                               't of ', 
                                               bold('R'))),
                        no = '')) +
    theme_classic() +
    theme(plot.title = element_text(size = 35, 
                                    hjust = .5))
  
  mond
}