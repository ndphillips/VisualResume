# text.outline
# adds text with a white background - taken from Dirk Wulff www.dirkwulff.org
text.outline <- function(x, y,
                         labels = 'test',
                         col = 'black',
                         font = 1,
                         bg = 'white',
                         r = 0.02,
                         h = 1,
                         w = 1,
                         cex = 1,
                         adj = .5,
                         pos = NULL,
                         family){

  # Draw background
  is <- seq(0, 2 * pi, length = 72)
  for(i in is){
    xn = x + cos(i) * r * w
    yn = y + sin(i) * r * h
    text(xn, yn, labels = labels, col = bg, cex = cex, adj = adj, pos = pos, font = font, family = family)
  }

  # Foreground
  text(x, y, labels = labels, col = col, cex = cex, adj = adj, pos = pos, font = font, family = family)
}
