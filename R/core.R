#' ...
#'
#'Function revealing that you are actually Bruce Wayne using a secret identity...
#'
#' @param name Enter your name (character string) to reveal your biggest secret...
#'
#' @return
#' @export
#'
#' @examples library(RFun)
#' secRet('Bruce Wayne')
secRet <- function(name = 'Yannis') {

  x <- seq(0, 2*pi, length = (15000))
  d0 <- data.table(x = 8*cos(x), y = 4*sin(x))

  x <- c(seq(3, 7, 0.001), seq(-7, -3, 0.001))
  d1 <- data.table(x = x, y = c(3*sqrt(1 - (x/7)^2), -(3*sqrt(1 - (x/7)^2))))
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
  d2 <- data.table(x = x, y = abs(x/2) - (3*sqrt(33) - 7)*x^2/112 - 3 + sqrt(1 - (abs(abs(x) - 2) - 1)^2))
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
  d4 <- data.table(x = x, y = 3*abs(x) + 0.75)

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
  d6 <- data.table(x = x, y  = 6 * sqrt(10)/7 + (1.5 - 0.5*abs(x))*sqrt(abs(abs(x) - 1)/(abs(x) - 1)) - 6*sqrt(10)*sqrt(4 - (abs(x) - 1)^2)/14)

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
    theme(plot.title = element_text(hjust = 0.5)) +
    coord_fixed()

  return(bat)
}

#' Love is everywhere
#'
#' Let the (R) world know...
#'
#' @param love Character string containing your name and the name of your beloved
#' @param size grid size (of your love)
#' @param colour1 .
#' @param colour2 .
#' @param n.hearts number of hearts within the grid
#'
#' @return
#' @export
#'
#' @examples library(RFun)
#' eveRloving()
#' eveRloving('Phill + Caroline', 200, 100, '#1f3b51', '#aac6dc')
eveRloving <- function(love = NULL, size = 150, n.hearts = 50, colour1 = 'hotpink', colour2 = 'red4') {

  dt <- data.table(numbers = seq(0, 2*pi, by = 0.01))

  dt <- dt[, `:=` (x = 16*sin(numbers)^3,
                   y = 13*cos(numbers) - 5*cos(2*numbers) - 2*cos(3*numbers) - cos(4*numbers))]

  rotate <- function(dta, degree) {
    ## reference
    ## http://en.wikipedia.org/wiki/Rotation_matrix
    mat <- as.matrix(dta)
    theta <- degree/180*pi
    r <- matrix(c(cos(theta), -sin(theta), sin(theta), cos(theta)), byrow = TRUE, ncol = 2)
    mat <- (mat - colMeans(mat)) %*% r
    return(mat)
  }

  hearts <- list()
  for (i in 1:n.hearts) {
    hearts[[i]] <- cbind(rotate(dt[,2:3]*sample(seq(.5,1.5,.01), 1), sample(-30:30, 1)),i)
    for(j in 1:2) {
      hearts[[i]][,j] <- hearts[[i]][,j] + sample(-size:size, 1)
    }
  }
  gg_hearts <- data.table(do.call(rbind, hearts))
  names(gg_hearts) <- c('x', 'y', 'id')

  h <- ggplot(gg_hearts, aes(x = x, y = y, group = factor(id), color = id, fill = id)) +
    geom_polygon(show.legend = FALSE, alpha = .75) +
    scale_color_continuous(low = colour1, high = colour2) +
    scale_fill_continuous(low = colour1, high = colour2) +
    ggtitle(bquote(bold(.(love)))) +
    coord_fixed() +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5))

  return(h)
}

#' Logo designer
#'
#' Simple function developed to desing company logos
#'
#' @param name Name of you company (char string)
#' @param text_size Logo text size
#' @param cols Logo colours - either avalid palette name or colour names
#' @param additional_text Additional title text
#' @param expr Posibility to add expressions to logo
#' @param expr.index Expression index (possition)
#' @param all_caps LOGICAL
#' @param include_title LOGICAL Include the name of your company in logo title
#' @param family Font style
#' @param fontface Font type
#'
#' @return
#' @export
#'
#' @examples library(RFun)
#' a <- logo(name = 'KVHEM', additional_text = 'Katedra vodního hospodářství a environmentálního modelování')
#' a + theme(title = element_text(size = 12.5))
#'
#' b <- logo(name = 'KVHEM', additional_text = 'Katedra vodního hospodářství \na environmentálního modelování', include_title = F, family = 'AvantGarde')
#' b + theme(panel.grid.major = element_blank(),
#'           panel.grid.minor = element_blank(),
#'           axis.ticks = element_blank(),
#'           axis.text = element_blank(),
#'           axis.line = element_blank(),
#'           plot.title = element_text(size = 25, hjust = 0.25, color = 'steelblue4'))
#'
#' c <- logo(name = 'R-Users Group', text_size = 10, cols = 'Greens', family = 'Courier')
#' c
#'
#' x <- logo(name = 'DRUtES', additional_text = '\nDual Richards Unsaturated Equation Solver', cols = c('royalblue4', 'lightsteelblue1'), expr = expression(integral()[Omega]), text_size = 9.5, include_title = F, fontface = 'bold.italic')
#' x + theme(panel.grid.major = element_blank(),
#'           panel.grid.minor = element_blank(),
#'           axis.ticks = element_blank(),
#'           axis.text = element_blank(),
#'           axis.line = element_line(colour = 'royalblue4'))
logo <- function(name, text_size = 20, cols = 'Blues', additional_text = NULL, expr = NULL, expr.index = seq_along(expr) - 1, all_caps = FALSE, include_title = TRUE, family = 'Palatino', fontface = 'bold') {

  if(all_caps) {
    lttrs <- unlist(strsplit(toupper(name), split = ''))
  } else {
    lttrs <- unlist(strsplit(name, split = ''))
  }

  ll <- length(lttrs)

  lttrs.w.expr <- c(lttrs, as.character(expr))
  id  <- c(seq_along(lttrs), expr.index + 0.5)
  lttrs.w.expr <- lttrs.w.expr[order(id)]

  x <- 0:(length(lttrs.w.expr) - 1)
  d <- data.table(xmin = x, xmax = x + 1, ymin = 0, ymax = 1, fill = x, txt = lttrs.w.expr, prs = nchar(lttrs.w.expr) != 1)

  d.raw <- d[which(!d[, prs]), ]
  d.expr <- d[which(d[, prs]), ]
  if(length(cols) != 1) {
    col_pal <- colorRampPalette(as.vector(unlist(strsplit(cols, split = ' '))))(ll + 3)
  } else {
    col_pal <- rev(colorRampPalette(brewer.pal(9, cols))(ll + 4))
  }

  logo <- ggplot() +
    geom_rect(data = d, aes(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax, fill = factor(fill))) +
    scale_fill_manual(values =  col_pal[1:dim(d)[1]]) +
    theme_classic() +
    theme(aspect.ratio = 1/ll,
          plot.title = element_text(hjust = 0.5, family = family, face = fontface, colour = col_pal[1]),
          plot.background = element_rect(fill = col_pal[length(col_pal) - 1]),
          panel.background = element_rect(fill = col_pal[length(col_pal)- 1]),
          axis.ticks = element_line(colour = col_pal[1]),
          axis.text = element_text(colour = col_pal[1]),
          axis.line = element_line(colour = col_pal[1]),
          legend.position = 'none') +
    geom_text(data = d.raw,
              aes(x = xmin + .5, y = ymin + .5, label = txt),
              parse = F, colour = col_pal[length(col_pal)],
              size = text_size,
              family = family,
              fontface = fontface) +
    labs(x = '', y = '', title = ifelse(include_title , paste(name, ifelse(include_title & !is.null(additional_text), '-', ''), additional_text), additional_text))

  if(!is.null(expr)) {logo <- logo +
    geom_text(data = d.expr,
              aes(x = xmin + .5, y = ymin + .5, label = txt),
              parse = T,
              colour = col_pal[length(col_pal)],
              size = text_size,
              family = family,
              fontface = fontface)}

  return(logo)
}

#' Piet Mondrian - Composition with Large Red Plane, Yellow, Black, Grey and Blue, 1921
#'
#' Recreation of famous Piet Mondrians painting in ggplot2, hopefully there will be a random Mondrian painting generator included in the future.
#'
#' @param initials Character string of you initials
#' @param title LOGICAL Include funny painting title
#'
#' @return
#' @export
#'
#' @examples library(RFun)
#' mondRian()
#' mondRian('JRB') + theme_void()
mondRian <- function(initials = 'FS', title = F) {

  initials <- paste(unlist(strsplit(toupper(initials), split = '')), collapse = ' ')

  mond <- ggplot() +
    geom_rect(aes(xmin = 0, xmax = 22, ymin = 0, ymax = 22), fill = 'grey65', color = NA) +
    geom_rect(aes(xmin = .5, xmax = 21.5, ymin = .5, ymax = 21.5), fill = '#D6D6D0', color = NA) +
    geom_rect(aes(xmin = 3, xmax = 14, ymin = 7.5, ymax = 19), fill = '#BC422D', color = NA) +
    geom_rect(aes(xmin = 20, xmax = 21.5, ymin = .5, ymax = 5.25), fill = '#BC422D', color = NA) +
    geom_rect(aes(xmin = 14, xmax = 20, ymin = 13, ymax = 21.5), fill = '#E1BB4E', color = NA) +
    geom_rect(aes(xmin = 14, xmax = 20, ymin = 13, ymax = 21.5), fill = '#E1BB4E', color = NA) +
    geom_rect(aes(xmin = .5, xmax = 3, ymin = .5, ymax = 5.25), fill = '#E1BB4E', color = NA) +
    geom_rect(aes(xmin = 3, xmax = 8.5, ymin = 3, ymax = 7.5), fill = '#181918', color = NA) +
    geom_rect(aes(xmin = 8.5, xmax = 14, ymin = 2, ymax = 3), fill = '#181918', color = NA) +
    geom_rect(aes(xmin = 14, xmax = 20, ymin = 2, ymax = 5.25), fill = '#203F6D', color = NA) +
    geom_rect(aes(xmin = 20, xmax = 21.5, ymin = 5.5, ymax = 21.5), fill = '#c3c3c0', color = NA) +
    geom_rect(aes(xmin = 8.5, xmax = 14, ymin = 3.5, ymax = 7.5), fill = '#c3c3c0', color = NA) +
    geom_rect(aes(xmin = .5, xmax = 3, ymin = 5.25, ymax = 13), fill = '#c3c3c0', color = NA) +
    geom_rect(aes(xmin = .5, xmax = 5.75, ymin = 19, ymax = 21.5), fill = '#c3c3c0', color = NA) +
    geom_segment(aes(x = 1, y = 19, xend = 20, yend = 19), colour = '#181911', size = 3.25) +
    geom_segment(aes(x = 3, y = 1, xend = 3, yend = 19), colour = '#181911', size = 3.25) +
    geom_segment(aes(x = 20, y = 1, xend = 20, yend = 21), colour = '#181911', size = 3.25) +
    geom_segment(aes(x = 8.5, y = 2, xend = 20, yend = 2), colour = '#181911', size = 3.25) +
    geom_segment(aes(x = 3, y = 3, xend = 14, yend = 3), colour = '#181911', size = 3.25) +
    geom_segment(aes(x = 1, y = 5.25, xend = 21.5, yend = 5.25), colour = '#181911', size = 3.25) +
    geom_segment(aes(x = 14, y = 2, xend = 14, yend = 21), colour = '#181911', size = 3.25) +
    geom_segment(aes(x = 3, y = 7.5, xend = 20, yend = 7.5), colour = '#181911', size = 3.25) +
    geom_segment(aes(x = 8.5, y = 1, xend = 8.5, yend = 7.5), colour = '#181911', size = 3.25) +
    geom_segment(aes(x = 5.75, y = 3, xend = 5.75, yend = 7.5), colour = '#181911', size = 3.25) +
    geom_segment(aes(x = 5.75, y = 19, xend = 5.75, yend = 21), colour = '#181911', size = 3.25) +
    geom_segment(aes(x = 1, y = 13, xend = 3, yend = 13), colour = '#181911', size = 3.25) +
    geom_segment(aes(x = 14, y = 13, xend = 20, yend = 13), colour = '#181911', size = 3.25) +
    geom_segment(aes(x = 17, y = 7.5, xend = 17, yend = 13), colour = '#181911', size = 3.25) +
    geom_text(aes(x = 10, y = 1.25), colour = '#181911', size = 2, label = paste(initials, format(Sys.Date(), '%y')), fontface = 'bold', alpha = .75) +
    coord_fixed() +
    labs(x = '', y = '', title = ifelse(title, expression(paste('the a', bold('R'),'t of ', bold('R'))), '')) +
    theme_classic() +
    theme(plot.title = element_text(size = 35, hjust = .5))

  return(mond)
}

#' Smiley face :)
#'
#' @param joke LOGICAL Make it all a f-ing joke? (Watchmen comic-book reference)
#'
#' @return
#' @export
#'
#' @examples library(RFun)
#' smiley() + theme_void()
#' smiley(joke = T)
smiley <- function(joke = F) {

  x <- seq(0, 2*pi , length = 2500)

  d0 <- data.table(x = sin(x)*25, y = cos(x)*25)
  d1 <- d0[(abs(d0$x) < 15) & (d0$y < 10),]
  d1 <- d1[, y := y + 7.5]
  d1.1 <- d1*1.25
  d1.1 <- d1.1[, y := y + 5.5]
  d1.1 <- d1.1[(abs(d1.1$x) < max(abs(d1$x))) & (d1.1$y < max(d1$y)),]
  d1.1 <- rbind(d1.1, d1)
  d1.2.1 <- data.table(x = min(d1$x) + (2*cos(x)*cos(1) - 8*sin(x)*sin(-2))*.15, y = max(d1$y) + (2*cos(x)*sin(-2) + 8*sin(x)*cos(.725))*.15)
  d1.2.2 <- data.table(x = max(d1$x) + (2*cos(x)*cos(1) - 8*sin(x)*sin(2))*.15, y = max(d1$y) + (2*cos(x)*sin(2) + 8*sin(x)*cos(.725))*.15)
  d2 <- data.table(x = 10 + 4*cos(x), y = 7.5 + 8*sin(x))

  if(joke) {
    d3.1 <- data.table(x = - 15 + sin(x)*3.5, y = 15 + cos(x)*3.5)
    d3.2 <- data.table(x = - 5 + sin(x)*.75, y = 5 + cos(x)*.75)
    d3.3 <- data.table(x = - 15 + sin(x)*.25, y = 10 + cos(x)*.25)
    d3.4 <- data.table(x = - 7.5 + sin(x)*.25, y = 15 + cos(x)*.25)
    d3.5 <- data.table(x = - 16.5 + sin(x)*.5, y = 7.5 + cos(x)*.5)
  }

  face <- ggplot() +
    geom_polygon(data = d0, aes(x = x, y = y), fill = 'gold', color = 'black', size = 5) +
    geom_line(data = d1.1, aes(x = x, y = y), color = 'black', size = 2) +
    geom_polygon(data = d1.2.1, aes(x = x, y = y), fill = 'black', color = 'black') +
    geom_polygon(data = d1.2.2, aes(x = x, y = y), fill = 'black', color = 'black') +
    geom_polygon(data = d2, aes(x = x, y = y), fill = 'black', color = 'black') +
    geom_polygon(data = d2, aes(x = - x, y = y), fill = 'black', color = 'black') +
    coord_fixed() +
    theme_classic()

  if(joke) {
    face <- face +
      theme_void() +
      geom_polygon(data = d3.1, aes(x = x, y = y), fill = 'red4', color = 'red4') +
      geom_polygon(data = d3.2, aes(x = x, y = y), fill = 'red4', color = 'red4') +
      geom_segment(aes(x = mean(d3.1$x), y = mean(d3.1$y), xend = mean(d3.2$x), yend = mean(d3.2$y)), color = 'red4', size = 2.5) +
      geom_polygon(data = d3.3, aes(x = x, y = y), fill = 'red4', color = 'red4') +
      geom_segment(aes(x = mean(d3.1$x) - 1.5, y = mean(d3.1$y), xend = mean(d3.3$x), yend = mean(d3.3$y)), color = 'red4', size = 1.5) +
      geom_polygon(data = d3.4, aes(x = x, y = y), fill = 'red4', color = 'red4') +
      geom_segment(aes(x = mean(d3.1$x), y = mean(d3.1$y) + 2, xend = mean(d3.4$x), yend = mean(d3.4$y)), color = 'red4', size = 1.5) +
      geom_polygon(data = d3.5, aes(x = x, y = y), fill = 'red4', color = 'red4') +
      geom_segment(aes(x = mean(d3.1$x) - 2, y = mean(d3.1$y) + 2, xend = mean(d3.5$x), yend = mean(d3.5$y)), color = 'red4', size = 2) +
      ggtitle('Quis custodiet ipsos custodes?') +
      theme(panel.background = element_rect(fill = 'red4'),
            plot.title = element_text(size = 20, hjust = 0.5, family = 'NewCenturySchoolbook', face = 'italic'))

  }

  return(face)
}

