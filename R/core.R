lop <- c('ggplot2', 'data.table', 'RColorBrewer')
to.instal <- lop[!(lop %in% row.names(installed.packages()))]

if(length(to.instal) != 0) {install.packages(to.instal)}

lapply(lop, library, character.only = T)

#' ...
#'
#'Function revealing that you are actually Bruce Wayne using a secret identity... (function returns grob - graphical object)
#'
#' @param name Enter your name (as character string) to reveal your biggest secret...
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
#' @param text_size Title size
#' @param cols Logo colours - either avalid palette name or colour names
#' @param additional_text Additional title text
#' @param expr Posibility to add expressions to logo
#' @param expr.index Expression index (possition)
#' @param all_caps LOGICAL
#' @param include_title LOGICAL Include the name of your company in logo title
#'
#' @return
#' @export
#'
#' @examples library(RFun)
#' a <- logo(name = 'KVHEM', additional_text = 'Katedra vodního hospodářství a environmentálního modelování')
#' a + theme(title = element_text(size = 12.5))
#'
#' b <- logo(name = 'KVHEM', additional_text = 'Katedra vodního hospodářství \na environmentálního modelování', include_title = F)
#' b + theme(panel.grid.major = element_blank(),
#'           panel.grid.minor = element_blank(),
#'           axis.ticks = element_blank(),
#'           axis.text = element_blank(),
#'           axis.line = element_blank(),
#'           plot.title = element_text(size = 25, hjust = 0.25, color = 'steelblue4'))
#'
#' c <- logo(name = 'R-Users Group', text_size = 10, cols = 'Greens')
#' c
#'
#' x <- logo(name = 'DRUtES', additional_text = '\nDual Richards Unsaturated Equation Solver', cols = c('royalblue4', 'lightsteelblue1'), expr = expression(integral()[Omega]), text_size = 9.5, include_title = F)
#' x + theme(panel.grid.major = element_blank(),
#'           panel.grid.minor = element_blank(),
#'           axis.ticks = element_blank(),
#'           axis.text = element_blank(),
#'           axis.line = element_line(colour = 'royalblue4'))
logo <- function(name, text_size = 20, cols = 'Blues', additional_text = NULL, expr = NULL, expr.index = seq_along(expr) - 1, all_caps = FALSE, include_title = TRUE) {

  if(all_caps) {
    lttrs <- unlist(strsplit(toupper(name), split = ''))
  } else {
    lttrs <- unlist(strsplit(name, split = ''))
  }

  ll <- length(lttrs)

  lttrs.w.expr <- c(lttrs, as.character(expr))
  id  <- c( seq_along(lttrs), expr.index + 0.5 )
  lttrs.w.expr <- lttrs.w.expr[order(id)]

  x <- 0:(length(lttrs.w.expr) - 1)
  d <- data.frame(xmin = x, xmax = x + 1, ymin = 0, ymax = 1, fill = x, txt = lttrs.w.expr)

  if(length(cols) != 1) {
    col_pal <- colorRampPalette(as.vector(unlist(strsplit(cols, split = ' '))))(ll + 3)
  } else {
    col_pal <- rev(colorRampPalette(brewer.pal(9, cols))(ll + 4))
  }

  logo <- ggplot(d) +
    geom_rect(aes(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax, fill = factor(fill))) +
    scale_fill_manual(values =  col_pal[1:dim(d)[1]]) +
    theme_classic() +
    theme(aspect.ratio = 1/ll,
          plot.title = element_text(hjust = 0.5, family = 'URWBookman', colour = col_pal[1]),
          plot.background = element_rect(fill = col_pal[length(col_pal) - 1]),
          panel.background = element_rect(fill = col_pal[length(col_pal)- 1]),
          axis.ticks = element_line(colour = col_pal[1]),
          axis.text = element_text(colour = col_pal[1]),
          axis.line = element_line(colour = col_pal[1]),
          legend.position = 'none') +
    annotate('text',
             x = d$xmin + .5,
             y = d$ymin + .5,
             label = d$txt,
             parse = !is.null(expr),
             colour = col_pal[length(col_pal)],
             size = text_size,
             fontface = 1) +
    labs(x = '',
         y = '',
         title = ifelse(include_title , paste(name,
                                              ifelse(include_title & !is.null(additional_text),
                                                     '-',
                                                     ''),
                                              additional_text),
                        additional_text))

  return(logo)
}
