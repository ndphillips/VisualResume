#' Creates a visual resume
#'
#' This function takes several arguments and returns a visual resume plot. The plot is optimized for an 8.5 in x 11 in display.
#'
#' @param titles.left,titles.right character. Vector of up to length three indicating labels to be shown in the top left / right.
#' @param titles.left.cex,titles.right.cex numeric. Vector indicating the size of the respective labels.
#' @param center.labels character. Vector of two labels for the top and bottom sections of the timeline.
#' @param top dataframe. Specifications of the top section of the timeline.
#' @param bottom dataframe. Specificaions of the bottom section of the timeline.
#' @param milestones dataframe. Specifications of brief level-1 timeline milestones (at top of timeline).
#' @param events dataframe. Specificaions of longer level-2 milestones (in bottom right plot).
#' @param interests list. A list of length up to 4 indicating one's interests / skills. Each entry should be a character vector up to length 5. The more often a value occurs in the vector, the more pronounced it will be.
#' @param font.family character. An optional google font family. Run \code{sysfonts::font.families.google()} to see all of them. Only use when creating plot in a plotting device such as pdf()
#' @param col character. The color palette for the plot. Can be a named palette from the \code{yarrr::piratepal()} function (e.g.; \code{"basel"} or \code{"xmen"}), or a vector of named colors.
#' @param trans numeric. A number between 0 and 1 indicating the transparencies of the colors
#' @param events.cex numeric. A size multiplier for level-2 milestones
#' @param year.steps integer.
#' @param year.range integer. An optional vector of minimum and maximum years to plot in the timeline.
#' @importFrom grDevices rgb col2rgb gray
#' @importFrom graphics text points abline legend mtext segments rect arrows axis par layout plot plot.new plot.window
#' @importFrom yarrr piratepal
#' @importFrom showtext showtext.begin showtext.end
#' @importFrom sysfonts font.add.google font.families.google
#' @export
#' @details
#'
#'  The middle, timeline in the plot is plotted with y values ranging from 0 to 100.
#'  The arguments \code{top} and \code{bottom}, specifying events in the bottom and top of the timeline should each be a dataframe with at least the columns title, sub, start, and end. To specify plotting locations explicitly, you can include the columns \code{box.x0, box.x1, box.y0, box.y1} for the box locations and/or \code{label.x, label.y, label.dir} for the label coordinates and direction ("left" or "right")
#'
#' @examples
#'
#'
#'VisualResume(
#'  titles.left = c("Walter White, PhD", "Chemistry, Cooking, Pizza", "*Built with love in R using the InfoResume package: www.ndphillips.github.io/inforesume"),
#'  titles.right = c("www.lospolloshermanos.com", "TheOneWhoKnocks@gmail.com", "Full Resume: www.ndphillips.github.io"),
#'  titles.right.cex = c(2, 2, 1),
#'  titles.left.cex = c(4, 2, 1),
#'  center.labels = c("Education", "Teaching"),
#'  top = data.frame(title = c("Grinnell College", "Ohio U", "U of Basel", "MPIB", "U of Konstanz", "U of Basel"),
#'                   sub = c("BA. Student", "MS. Student", "PhD. Student", "PhD. Researcher", "PostDoc", "PostDoc"),
#'                   start = c(1976, 1980, 1982, 2012, 2014.7, 2016.1),
#'                   end = c(1980, 1982, 1985, 2014.6, 2016, 2017)),
#'  bottom = data.frame(title = c("Musician's Friend", "Statistics", "Research Methods", "Information Search", "R", "R", "R", "R"),
#'                      sub = c("Statistician", "OhioU", "OhioU", "Konstanz", "Konstanz", "Konstanz", "Basel", "Basel"),
#'                      start = c(2005.5, 2009.5, 2010, 2014.8, 2014.8, 2015.4, 2016.1, 2016.7),
#'                      end = c(2006.5, 2010, 2011, 2015.3, 2015.3, 2015.9, 2016.6, 2017)),
#'  milestones = data.frame(title = c("BA Mathematics", "MS Chemistry", "Ph.D Chemistry"),
#'                          subtitle = c("2005", "2010", "2014"),
#'                          date = c(2005.5, 2010.9, 2014.5)),
#'  events = data.frame(year = c(2016, 2015, 2014, 2013, 2012),
#'                      title = c("White, W. (2016). YaRrr! The Pirate's Guide to R.\nSelf-published e-book.",
#'                                "White, W. & Pinkman, J. (2015). The Janus face of Darwinian competition.\nScientific Reports, 5.",
#'                                "White, W. & Pinkman, J. (2014). Rivals in the Dark: How competition influences [...].\nCognition, 113(1), 104-119.",
#'                                "White, W. & Pinkman, J. (2013). Early positive information impacts final [...].\nJournal of Behavioral Decision Making, 27.",
#'                                "White, W. & Pinkman, J. (2012). Predicting soccer matches [...].\nJudgment and Decision Making. 5(3). 200-206.")),
#'  interests = list("programming" = c(rep("R", 10), rep("Python", 1), rep("JavaScript", 2), "MatLab"),
#'                   "statistics" = c(rep("Decision Trees", 10), rep("Bayesian", 5), rep("Regression", 3)),
#'                   "research" = c(rep("Information Search", 10), rep("Decision Making", 5), rep("Statistical Reasoning", 3))
#'                   #            "test2" = c(rep("AAA", 10), rep("BBB", 50), rep("CCC", 3))
#'  ),
#'  col = "xmen",
#'  trans = .6)


VisualResume <- function(titles.left = c("Main Title", "Sub-title", "Sub-Sub-title"),
                          titles.left.cex = c(4, 2, 1),
                          titles.right = c("www.a.com", "me@gmail.com", ""),
                          titles.right.cex = c(3, 2, 1),
                          center.labels = c("D", "E"),
                          top = data.frame("title" = c("College A", "University B", "University C"),
                                           "sub" = c("a"),
                                           "start" = c(2001.5, 2006.5, 2011),
                                           "end" = c(2005.5, 2010.9, 2014.5)),
                          bottom = data.frame(title = c("Project A", "Project B", "Project C"),
                                              "sub" = c("b"),
                                              start = c(2010, 2014.6, 2014.6),
                                              end = c(2010.5, 2015.2, 2015.2)),
                          milestones = data.frame(title = c("BA", "MS", "Ph.D"),
                                                  subtitle = c("2005", "2010", "2014"),
                                                  date = c(2005.5, 2010.9, 2014.5)),
                          events = data.frame(year = c(2007, 2011.5, 2014.2),
                                              title = c("a", "b", "c")),
                          events.cex = 1.5,
                          year.steps = 1,
                          interests = list("Programming" = c("R", "Javascript", "HTML"),
                                           "Statistics" = c("Bayesian", "Regression", "Decision\nTrees"),
                                           "Research" = c("Decision Making", "Foraging")
                                           ),
                          font.family = NA,
                          col = "xmen",
                          trans = .6,
                          year.range = NULL
                          ) {
#
#
  #   year.range <- NULL
  # col = "xmen"
  #  trans = .6
  #  font.family <- NA
  #
  # titles.left = c("Title 1", "Title 2", "Title 3")
  # titles.right = c("Title 1", "Title 2", "Title 3")
  # center.labels = c("Label 1", "Label 2")
  # top = data.frame(title = c("A", "B", "C", "D", "E", "F"),
  #                  sub = c("a", "b", "c", "d", "e", "f"),
  #                  start = c(2001.5, 2006.5, 2011, 2012, 2014.7, 2016.1),
  #                  end = c(2005.5, 2010.9, 2014.5, 2014.6, 2016, 2017),
  #                  point.x = c(2002, 2007, 2011.2, 2015.5, 2015.5, 2016.3),
  #                  point.y = rep(55, 6)
  # )
  # bottom = data.frame(title = c("G", "H", "I", "J", "K", "L", "M", "N"),
  #                     sub = c("g", "h", "i", "j", "k", "l", "m", "n"),
  #                     start = c(2005.5, 2009.5, 2010, 2014.8, 2014.8, 2015.4, 2016.1, 2016.7),
  #                     end = c(2006.5, 2010, 2011, 2015.3, 2015.3, 2015.9, 2016.6, 2017),
  #                     point.x = c(2005.5, 2009.5, 2010, 2014.8, 2014.8, 2015.4, 2016.1, 2016.7),
  #                     point.y = rep(45, 8))
  # milestones = data.frame(title = c("M1", "M2", "M3"),
  #                         subtitle = c("2005", "2010", "2014"),
  #                         date = c(2005.5, 2010.9, 2014.5))
  # events = data.frame(year = c(2016, 2015, 2014, 2013, 2012),
  #                     title = c("EVENT 1",
  #                               "EVENT 2",
  #                               "EVENT 3",
  #                               "EVENT 4",
  #                               "EVENT 5"))
  # interests = list("AREA1" = c(rep("A", 20), rep("B", 1), rep("C", 2), "D", "E"),
  #                  "AREA2" = c(rep("A", 10), rep("B", 5), rep("C", 3), rep("D")),
  #                  "AREA3" = c(rep("A", 10), rep("B", 5), rep("C", 3), rep("D", 6)))




if(is.null(year.range)) {
year.range <- c(floor(min(c(top$start, bottom$start))),
                ceiling(max(c(top$end, bottom$end))))

year.min <- min(year.range)
year.max <- max(year.range)
year.seq <- seq(year.min, year.max, by = year.steps)
}


# Convert factors to strings

for(i in 1:ncol(bottom)) {if(class(bottom[,i]) == "factor") {bottom[,i] <- paste(bottom[,i])}}
for(i in 1:ncol(top)) {if(class(top[,i]) == "factor") {top[,i] <- paste(top[,i])}}

# Extract some parameters
events.selected <- 1:nrow(events)
top.graph.label <- center.labels[1]
bottom.graph.label <- center.labels[2]

## Colors and Fonts
{

if(font.family %in% sysfonts::font.families.google()) {

  google.font <- TRUE
  sysfonts::font.add.google(font.family, font.family)
  showtext::showtext.begin()

} else {font.family <- NULL ; google.font <- FALSE}

if(col %in% yarrr::piratepal(palette = "names")) {
  color.vec <- yarrr::piratepal(palette = col, trans = trans)
} else {

  color.vec <- col

  # Make colors transparent
  for(i in 1:length(color.vec)) {

    col.o <- grDevices::col2rgb(col = color.vec[i])
    col.n <- grDevices::rgb(red = col.o[1], green = col.o[2], blue = col.o[3], alpha = trans * 255, maxColorValue = 255)

    color.vec[i] <- col.n

  }

}

if(length(color.vec) < (nrow(top) + nrow(bottom))) {

  color.vec <- rep(color.vec, length.out = (nrow(top) + nrow(bottom)))

  }
}

# Plot Layout
layout(
  matrix(c(1, 1, 1, 2, 2, 2, 3, 3, 4), nrow = 3, ncol = 3, byrow = TRUE),
  widths = c(2.75, 2.75, 5.5),
  heights = c(1.25, 5, 2.25)
)

# ----------------
# Header
# ----------------
{
  par(mar = c(0, 0, 0, 0))

  plot(1, xlim = c(0, 1), ylim = c(0, 1), bty = "n", type = "n", xaxt = "n", yaxt = "n", ylab = "", xlab = "")

#  segments(c( .02), c( 0), c(.98), c( 0), lwd = .8, col = "darkgray")

  text(.02, .65, titles.left[1], adj = 0, cex = titles.left.cex[1], family = font.family)
  text(.02, .3, titles.left[2], adj = 0, cex = titles.left.cex[2], family = font.family)
  text(.02, .05, titles.left[3], adj = 0, cex = titles.left.cex[3], family = font.family)
  text(.98, .65, titles.right[1], adj = 1, cex = titles.right.cex[1], family = font.family)
  text(.98, .3, titles.right[2], adj = 1, cex = titles.right.cex[2], family = font.family)
  text(.98, .05, titles.right[3], adj = 1, cex = titles.right.cex[3], family = font.family)


}

# ----------------
# Body
# ----------------
{

  par(mai = c(0, 0, 0, 0))

  top.y0 <- 55
  bottom.y0 <- 45

  top$location <- "top"
  bottom$location <- "bottom"

  combined <- rbind(top, bottom)

  # Adjust simultaneous starting times

  for(i in 2:nrow(combined)) {

    if(combined$start[i] %in% combined$start[1:(i-1)]) {

      combined$start[i] <- combined$start[i] + .1}

  }


  plot(1, xlim = c(year.min - 1, year.max + 1),
       ylim = c(0, 100), type = "n", xaxt = "n",
       yaxt = "n", ylab = "", xlab = "",
       bty = "n")


  ## Background ribbons

  # Top separation
  points(seq(year.min - 1, year.max + 1, length.out = 50), rep(100, 50), pch = 16, col = c(gray(.7)), cex = c(1, .7))

  # Bottom separation
  points(seq(year.min - 1, year.max + 1, length.out = 50), rep(0, 50), pch = 16, col = c(gray(.7)), cex = c(1, .7))



  # ...
  points(seq(year.min - .75, year.min - .25, length.out = 3),
         rep(50, 3), cex = c(.5, .75, 1.75), pch = 21, bg = "white")

  points(seq(year.max +.25, year.max + .75, length.out = 3),
         rep(50, 3), cex = rev(c(.5, .75, 1.75)), pch = 21, bg = "white")

  ## Year labels

  segments(year.seq,
           rep(bottom.y0, length(year.seq)),
           year.seq,
           rep(top.y0, length(year.seq)),
           col = "lightgray")

  rect(year.seq[-length(year.seq)],
       rep(bottom.y0, length(year.seq)),
       year.seq[2:length(year.seq)],
       rep(top.y0, length(year.seq)),
       col = c("white"), border = gray(.5))

  text(x = year.seq[2:length(year.seq)] - (year.seq[2] - year.seq[1]) / 2,
       y = rep(50, length(year.seq) - 1),
       labels = year.seq[-(length(year.seq))],
       cex = 1.5, family = font.family)

 text(year.min - .5, 90, top.graph.label, family = font.family, cex = 2, adj = 0)
 text(year.min - .5, 10, bottom.graph.label, family = font.family, cex = 2, adj = 0)

# Determine coordinates..

#  -------
# BOX COORDINATES
#  --------
{

  change.box.x0 <- FALSE
  change.box.x1 <- FALSE
  change.box.y0 <- FALSE
  change.box.y1 <- FALSE

 if("box.x0" %in% names(combined) == FALSE) {

    combined$box.x0 <- NA
   change.box.x0 <- TRUE
 }

 if("box.y0" %in% names(combined) == FALSE) {

   combined$box.y0 <- NA
   change.box.y0 <- TRUE

 }

 if("box.x1" %in% names(combined) == FALSE) {

   combined$box.x1 <- NA
   change.box.x1 <- TRUE

 }

 if("box.y1" %in% names(combined) == FALSE) {

   combined$box.y1 <- NA
   change.box.y1 <- TRUE

 }

  for (i in 1:nrow(combined)) {

      # Get default locations

    if(is.na(combined$box.x0[i])) {

      location.i <- combined$location[i]
      box.x0 <- combined$start[i]
      box.y0 <- switch(location.i,
                       "bottom" = 45,
                       "top" = 55)
      box.x1 <- combined$end[i]

      box.y1 <- switch(location.i,
                       "bottom" = box.y0 - 10,
                       "top" = box.y0 + 10)

      # Am I starting at the same time as a previous box?

      simultaneous.boxes <- sum(combined$box.x0[1:(i-1)] == box.x0 &
                                  combined$location[1:(i-1)] == location.i,
                                na.rm = TRUE)

      if(simultaneous.boxes > 0) {box.x0 <- box.x0 + .1 * simultaneous.boxes}

  # Am I starting within a previous box?

      existing.boxes <- sum(combined$box.x0[-i] < box.x0 &
                            combined$box.x1[-i] > box.x0 &
                            combined$location[-i] == location.i, na.rm = TRUE)

      box.y1 <- box.y1 + existing.boxes * switch(location.i, "top" = 4, "bottom" = -4)

      if(change.box.x0) {combined$box.x0[i] <- box.x0}
      if(change.box.x1) {combined$box.x1[i] <- box.x1}
      if(change.box.y0) {combined$box.y0[i] <- box.y0}
      if(change.box.y1) {combined$box.y1[i] <- box.y1}

    }
  }
}

#  -------
# POINT COORDINATES
#  --------
{

  change.point.x <- FALSE
  change.point.y <- FALSE

 if("point.x" %in% names(combined) == FALSE) {

   change.point.x <- TRUE
   combined$point.x <- NA

   }

  if("point.y" %in% names(combined) == FALSE) {

    change.point.y <- TRUE
    combined$point.y <- NA

  }

  for (i in 1:nrow(combined)) {

      location.i <- combined$location[i]

      # Get default locations

      # Does another box start in this box?

      conflicting.l <- combined$start[-i] > combined$start[i] & combined$start[-i] < combined$end[i]

      if(any(conflicting.l) == FALSE) {

        point.x <- combined$start[i] + .25 * (combined$end[i] - combined$start[i])
        point.y <- combined$box.y0[i] + switch(location.i, "bottom" = -5, "top" = 5)


      }

      if(any(conflicting.l)) {

        next.start <- min(combined$start[-i][conflicting.l])

        point.x <- combined$start[i] + .25 * (next.start - combined$start[i])
        point.y <- combined$box.y0[i] + switch(location.i, "bottom" = -5, "top" = 5)

      }

      # Is there another point in the same location?

      points.conflicting <- sum(abs(combined$point.x[-i] - point.x) < .1 &
                                abs(combined$point.y[-i] - point.y) < .1, na.rm = TRUE)

      if(any(points.conflicting)) {

        point.x <- point.x + sum(points.conflicting) * .1

        point.y <- point.y + sum(points.conflicting)

      }

      if(change.point.x) {combined$point.x[i] <- point.x}
      if(change.point.y) {combined$point.y[i] <- point.y}

  }

 }

  }

#  -------
# LABEL COORDINATES
#  --------
 {

   change.label.dir <- FALSE
   change.text.adj <- FALSE
   change.elbow <- FALSE
   change.label.x <- FALSE
   change.label.y <- FALSE


 if("label.dir" %in% names(combined) == FALSE) {

   change.label.dir <- TRUE
   combined$label.dir <- NA

 }

 if("text.adj" %in% names(combined) == FALSE) {

   combined$text.adj <- NA
   change.text.adj <- TRUE

 }

 if("elbow" %in% names(combined) == FALSE) {

   combined$elbow <- NA
   change.elbow <- TRUE
 }


 if("label.x" %in% names(combined) == FALSE) {

   combined$label.x <- NA
   change.label.x <- TRUE

 }

 if("label.y" %in% names(combined) == FALSE) {

   combined$label.x <- NA
   change.label.y <- TRUE

 }

for(i in 1:nrow(combined)) {

    method <- 2

if(is.na(combined$label.x[i])) {

  if(method == 1) {

    location.i <- combined$location[i]
    label.x <- combined$point.x[i]

    # Do other boxes start within X years on the right or left?

    right.conflicting.l <- (combined$start[-i] >= combined$start[i]) & (combined$start[-i] <= combined$start[i] + 3) & (combined$location[-i] == location.i)
    left.conflicting.l <- combined$start[-i] <= combined$start[i] & (combined$start[-i] >= combined$start[i] - 3) & (combined$location[-i] == location.i)

    # If there are no conflicts, go right

    if(any(right.conflicting.l) == FALSE &
       any(left.conflicting.l) == FALSE) {

      label.y <- combined$box.y1[i] + switch(location.i,
                                             "bottom" = -10,
                                             "top" = 10)
      label.dir <- "right"
      text.adj <- 0
      elbow <- .1


    }

    # If there are more right than left conflicts then go left

    if(sum(right.conflicting.l) > sum(left.conflicting.l)) {

      conflicting.box.heights <- combined$box.y1[-i][left.conflicting.l | right.conflicting.l]
      conflicting.label.heights <- combined$label.y[-i][left.conflicting.l | right.conflicting.l]

      if(location.i == "top") {

        label.y <- max(c(conflicting.box.heights, combined$box.y1[i], conflicting.label.heights)) + 10

      }

      if(location.i == "bottom") {

        label.y <- min(c(conflicting.box.heights, combined$box.y1[i], conflicting.label.heights)) - 10

      }

      label.dir <- "left"
      text.adj <- 1
      elbow <- -.1
    }

    # If there are more left than right conflicts

    if(sum(right.conflicting.l) <= sum(left.conflicting.l)) {

      conflicting.box.heights <- combined$box.y1[-i][right.conflicting.l]
      conflicting.label.heights <- combined$label.y[-i][right.conflicting.l]

      if(location.i == "top") {

        label.y <- max(c(conflicting.box.heights, combined$box.y1[i], conflicting.label.heights)) + 10

      }

      if(location.i == "bottom") {

        label.y <- min(c(conflicting.box.heights, combined$box.y1[i], conflicting.label.heights)) - 10

      }

      label.dir <- "right"
      text.adj <- 0
      elbow <- .1
    }

    # Are there any long labels on the left?

    if(i > 1) {

      label.conflict.l <- (combined$point.x[-i] - combined$point.x[i]) > -4 &
        (combined$point.x[-i] - combined$point.x[i]) < 0 &
        nchar(combined$title[-i]) > 8 &
        combined$label.dir[-i] == "right" &
        combined$location[-i] == location.i

      if(any(label.conflict.l)) {

        label.y <- label.y + 4 * sum(label.conflict.l)

      }
    }

  }
  if(method == 2) {

      # Grid method

    location.i <- combined$location[i]
    point.x <- combined$point.x[i]
    box.y <- combined$box.y1[i]

    x.loc <- c(point.x)
      #x.loc <- seq(point.x - 1, point.x + 1, length.out = 5)

    if(location.i == "top") {y.loc <- seq(box.y, 90, length.out = 10)}
    if(location.i == "bottom") {y.loc <- seq(box.y, 10, length.out = 10)}

      location.mtx <- expand.grid(x = x.loc, y = y.loc)

      # Assign initial points

      x.dev <- abs(location.mtx$x - point.x)

      if(combined$location[i] == "top") {

      y.dev <- abs(location.mtx$y - (box.y + 10))

      }

      if(combined$location[i] == "bottom") {

        y.dev <- abs(location.mtx$y - (box.y - 10))

      }

      joint.dev <- x.dev + y.dev

      # joint.dev[location.mtx$y < box.y]

      if(i == 1) {

        which.min <- which(joint.dev == min(joint.dev))

        label.x <- location.mtx$x[which.min]
        label.y <- location.mtx$y[which.min]
        label.dir <- "right"
        text.adj <- 0
        elbow <- .1

      }

      if(i > 1) {

        for(j in 1:(i - 1)) {

          if(combined$location[j] == combined$location[i]) {

       other.x.loc <- combined$label.x[j]
       other.y.loc <- combined$label.y[j]

       x.dev.l <- abs(other.x.loc - location.mtx$x) < 2.5

       x.dev <- x.dev + as.numeric(x.dev.l)

       y.dev[x.dev.l] <- y.dev[x.dev.l] + as.numeric(abs(other.y.loc - location.mtx$y[x.dev.l]) < 5) * 10

       }
        }


        joint.dev <- x.dev + y.dev
        which.min <- which(joint.dev == min(joint.dev))

        label.x <- location.mtx$x[which.min][1]
        label.y <- location.mtx$y[which.min][1]

       # Determine label direction

        # Is there a previous right direction label within 2 years?

        if(any(combined$location[1:(i - 1)] == combined$location[i] &
               combined$point.x[1:(i - 1)] > (combined$point.x[i] - 2) #&
              # combined$label.dir[1:(i - 1)] == "right"
        )) {label.dir <- "right"
           text.adj <- 0
           elbow <- .1

        } else {label.dir <- "left"
        text.adj <- 1
        elbow <- -.1
        }


      }

    }

  }

    # Write values
    if(change.label.y) {combined$label.y[i] <- label.y}
    if(change.label.x) {combined$label.x[i] <- label.x}
    if(change.label.dir) {combined$label.dir[i] <- label.dir}
    if(change.text.adj) {combined$text.adj[i] <- text.adj}
    if(change.elbow) {combined$elbow[i] <- elbow}

  }
 }

# Draw!

 for(i in 1:nrow(combined)) {

  # Box

  rect(xleft = combined$box.x0[i],
       ybottom = combined$box.y0[i],
       xright = combined$box.x1[i],
       ytop = combined$box.y1[i],
       col = color.vec[i],
       border = "black",
       lwd = .5)

  # Points

  points(combined$point.x[i],
         combined$point.y[i],
         pch = 21,
         cex = 2,
         col = "black",
         bg = "white")

  # Add lines

  # Main line
  segments(combined$point.x[i],
           combined$point.y[i],
           combined$label.x[i],
           combined$label.y[i],
           lty = 2)

  # Elbow line
  segments(combined$label.x[i],
           combined$label.y[i],
           combined$label.x[i] + combined$elbow[i],
           combined$label.y[i],
           lty = 2)

  # Main Text

  text(combined$label.x[i] + combined$elbow[i],
       combined$label.y[i],
       adj = combined$text.adj[i],
       labels = combined$title[i],
       cex = 1.8,
       family = font.family
  )

  #
  # text.outline(combined$label.x[i] + combined$elbow[i],
  #              combined$label.y[i],
  #              adj = combined$text.adj[i],
  #              labels = combined$title[i],
  #              cex = 1.8,
  #              family = font.family,
  #              bg = gray(.97, .5),
  #              w = (year.max - year.min) / 100,
  #              h = 1
  # )

  # Sub Text

  text(combined$label.x[i] + combined$elbow[i],
       combined$label.y[i] - 3,
       adj = combined$text.adj[i],
       labels = combined$sub[i],
       cex = 1,
       family = font.family)

}

  # --------- Upper milestones

  text(milestones$date,
       rep(95, nrow(milestones)),
       milestones$title, cex = 2, adj = 1, family = font.family, font = 3
  )

  text(milestones$date,
       rep(90, nrow(milestones)),
       milestones$subtitle,
       cex = 1.5, adj = 1, family = font.family, font = 1)


  # ---- Add Event symbols

  points(events$year, rep(60, length(events$year)), pch = 23, col = "black", bg = "white", cex = 4)
  text(events$year, rep(60, length(events$year)), 1:length(events$year), cex = 1.5, family = font.family)

# ----------------
# Bottom Row
# ----------------
{
  # --- Bottom left graph
{

  bl <- "network2"

#   if(bl == "wordcloud") {
#
#   par(mar = c(0, 1, 0, 0))
#   if(mean(is.na(words)) != 1) {
#
#     wordcloud::wordcloud(words, words.freq, scale = c(1, 2))
#
#     text(.5, .99, bottom.labels[1], cex = 2, adj = .5, family = font.family)
#    # rect(0, 0, .9, .9, lwd = .5)
#
#   }
#   }
#
#   if(bl == "network") {
#
#     par(xpd = TRUE)
#     par(mar = c(3, 3, 3, 3))
#
#     nodesize = 5
#     edgesize = 1
#
#     for(i in 1:length(interests)) {
#
#       edges.i <- expand.grid(names(interests)[i], interests[[i]])
#
#       if(i == 1) {edges <- edges.i}
#       if(i > 1) {edges <- rbind(edges, edges.i)}
#
#     }
#
# edges$n <- 1
#    g <- igraph::graph_from_data_frame(edges, directed = FALSE)
#
#   cue.names <- igraph::get.vertex.attribute(g)$name
#    # l <- igraph::layout_with_fr(g)
#    locations <- igraph::layout_with_dh(g)
#
#    # Setup plotting region
#    plot.new()
#
#    mtext(text = bottom.labels[1], side = 3)
#
#    if(min(locations[,1]) < 0) {x.min <- min(locations[,1]) * 1.1}
#    if(max(locations[,1]) < 0) {x.max <- max(locations[,1]) * .9}
#    if(min(locations[,2]) < 0) {y.min <- min(locations[,2]) * 1.1}
#    if(max(locations[,2]) < 0) {y.max <- max(locations[,2]) * .9}
#
#    if(min(locations[,1]) > 0) {x.min <- min(locations[,1]) * .9}
#    if(max(locations[,1]) > 0) {x.max <- max(locations[,1]) * 1.1}
#    if(min(locations[,2]) > 0) {y.min <- min(locations[,2]) * .9}
#    if(max(locations[,2]) > 0) {y.max <- max(locations[,2]) * 1.1}
#
#    plot.window(xlim = c(x.min, x.max),
#                ylim = c(y.min, y.max))
#    # Create edges
#
#    for (i in 1:nrow(edges)) {
#
#      is <- c(which(cue.names == edges[i, 1]),
#              which(cue.names == edges[i, 2]))
#
#      lines(x = c(locations[is[1], 1], locations[is[2], 1]),
#            y = c(locations[is[1], 2], locations[is[2], 2]),
#            lwd = .5,
#            lty = 1,
#            col = gray(.5, .5))
#
#    }
#
#    # Add points
#
#    # for(i in 1:length(cue.names)){
#    #   #  points(l[i,1] + .04, l[i, 2] - .04, cex = frequencies[v[i]]**nodesize, pch = 16, col = 'grey50')
#    #
#    #
#    #   relfreq.i <- .5
#    #
#    #   points(x = locations[i, 1],
#    #          y = locations[i, 2],
#    #          cex = relfreq.i * nodesize,
#    #          pch = 21,
#    #          col = "black",
#    #          bg = gray(1 - relfreq.i))
#    # }
#
#    # Add text
#
#    for(i in 1:length(cue.names)){
#      text(x = locations[i, 1],
#           y = locations[i, 2],
#           labels = cue.names[i],
#           cex = 1)
#    }
#    par(xpd = FALSE)
#
#
#   }

  if(bl == "network2") {

    par(xpd = TRUE)
    par(mar = c(3, 3, 0, 1))


    # Setup plotting region
    plot.new()


   # mtext(text = bottom.labels[1], side = 3, cex = 1.5, family = font.family)

    plot.window(xlim = c(0, 100),
                ylim = c(0, 100 * 2.25 / 5.5))



   n.interests <- length(interests)

    if(n.interests == 1) {locations <- data.frame(x = 50,
                                                  y = 50 / 2.5)
    r = 10
    w = 10 / 2.5

    text.min.cex <- 1
    text.max.cex <- 4
    center.text.cex <- 4
    arrow.len <- .05
    arrow.cut <- .4
    }

    if(n.interests == 2) {locations <- data.frame(x = c(25, 75),
                                                  y = c(35, 55) / 2.5)

    r <- 7
    w <- 7 / 2.5

    text.min.cex <- 1
    text.max.cex <- 3
    center.text.cex <- 3
    arrow.len <- .05
    arrow.cut <- .4
    }

    if(n.interests == 3) {locations <- data.frame(x = c(15, 50, 85),
                                                  y = c(20, 75, 40) / 2.5)

    # r <- 5.5
    # w <- 5.5 / 2.5
    #
    x.spoke.len <- 12.1
    y.spoke.len <- 14

    text.min.cex <- 1
    text.max.cex <- 3
    center.text.cex <- 2
    arrow.len <- .05
    arrow.cut <- .4
    }


   if(n.interests == 4) {locations <- data.frame(x = seq(10, 90, length.out = 4),
                                                 y = c(20, 75, 20, 80) / 2.5)

   x.spoke.len <- 12
   y.spoke.len <- 13

   text.min.cex <- 1
   text.max.cex <- 3
   center.text.cex <- 2
   arrow.len <- .05
   arrow.cut <- .4
   }


   for(interest.i in 1:n.interests) {

     center.x <- locations$x[interest.i]
     center.y <- locations$y[interest.i]
     center.lab <- names(interests[interest.i])

     # Center text
     text(center.x, center.y, labels = center.lab,
          font = 2, cex = center.text.cex)

     interest.frequencies <- table(interests[[interest.i]])

     n.values <- length(interest.frequencies)

     for(value.i in 1:n.values) {

       value.text.i <- names(interest.frequencies)[value.i]

       x.vals <- center.x + cos(c(.25, 1.25, .75, 1.75, .5, 1.5) * pi)[value.i] * x.spoke.len
       y.vals <- center.y + sin(c(.25, 1.25, .75, 1.75, .5, 1.5) * pi)[value.i] * y.spoke.len

       # Add value label

       value.cex <- interest.frequencies[value.i] / sum(interest.frequencies)
       value.cex <- value.cex * (text.max.cex - text.min.cex) + text.min.cex

       text(x.vals,
            y.vals,
            value.text.i,
            cex = value.cex)

       # Add segments

       #segment.weight <- interest.frequencies[value.i] / sum(interest.frequencies)
        segment.weight <- .5

        line.eq <- function(x1, y1, x2, y2, short = 0) {

          if(x1 != x2) {

        m <- (y2 - y1) / (x2 - x1)
        b <- y1 - m * x1

        line.len <- sqrt(abs(x2 - x1) ^ 2 + abs(y2 - y1) ^ 2)


        if(x1 < x2) {x1.n <- x1 + (short / 2) * line.len ; x2.n <- x2 - (short / 2) * line.len}
        if(x1 > x2) {x1.n <- x1 - (short / 2) * line.len ; x2.n <- x2 + (short / 2) * line.len}

        y1.n <- x1.n * m + b
        y2.n <- x2.n * m + b

          }

        if(x1 == x2) {

          x1.n <- x1
          x2.n <- x1
          if(y2 > y1) {y2.n <- y2 - (short/2) * (y2 - y1) ;y1.n <- y1 + (short/2) * (y2 - y1) }
          if(y2 < y1) {y2.n <- y2 + (short/2) * (y2 - y1) ;y1.n <- y1 - (short/2) * (y2 - y1) }
        }

          return(list("x1" = x1.n, "y1" = y1.n, "x2" = x2.n, "y2" = y2.n))

        }

        new.coord <- line.eq(center.x, center.y, x.vals, y.vals, short = arrow.cut)

         arrows(new.coord$x1,
                new.coord$y1,
                new.coord$x2,
                new.coord$y2,
                lwd = segment.weight,
                length = arrow.len)

     }

   }





  }

}

#   # ---- Bottom middle graph
# {
#   par(mar = c(0, 0, 0, 0))
#
#
#   plot(1, xlim = c(0, 1), ylim = c(0, 1), ylab = "", xlab = "", bty = "n", type = "n", yaxt = "n", xaxt = "n")
#
#   text(.5, .99, bottom.labels[2], adj = .5, cex = 2, family = font.family)
#
#   text(rep(0, 4), c(.8, .6, .4, .2), links[1:4], adj = 0, cex = 1.7, family = font.family)
#
# }

  # ---- Bottom right graph
{

  par(mar = c(3, 0, 0, 3))

  plot(0, 0, xlim = c(0, 1), ylim = c(0, 1), xaxt = "n", yaxt = "n",
       main = "", bty = "n", type = "n", xlab = "", ylab = "")

  # Top label text
 # mtext(bottom.labels[3], side = 3, cex = 1.5, adj = .5, family = font.family)

  n.events <- length(events.selected)

  # Points
  points(rep(.05, n.events), seq(.9, .0, length.out = n.events), pch = 23, cex = 4, col = "black", bg = "white")
  text(rep(.05, n.events),  seq(.9, .0, length.out = n.events), events.selected, pch = 23, cex = 1.5, col = "black", bg = "white", family = font.family)

  # Event text
  text(rep(.1, n.events),
       seq(.9, .0, length.out = n.events),
       events[1:nrow(events) %in% events.selected,2],
       adj = 0, cex = events.cex, family = font.family)
}
}

# ----------
# Closing
# ----------
{
  if(google.font) {

    showtext::showtext.end()

  }
}

}
