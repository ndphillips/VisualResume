#' Creates a visual resume from dataframes specifying important events (e.g.; jobs or education) and interests / skills. The plot is optimized for an 8.5 in x 11 in display.
#'
#' @param titles.left,titles.right character. Vector of up to length three indicating labels to be shown in the top left / right.
#' @param titles.left.cex,titles.right.cex numeric. Vector indicating the size of the respective labels.
#' @param timeline.labels character. Vector of two labels for the top and bottom sections of the timeline.
#' @param timeline dataframe. Specifications of the elements in the timeline. See Details
#' @param timeline.cex numeric. Vector of length 2 indicating the size of the timeline text. The first element is for the main text, the second element is for the sub text.
#' @param milestones dataframe. Specifications of brief level-1 timeline milestones (at top of timeline). Should contain the columns \code{year} (integer) and \code{title} (character)
#' @param events dataframe. Specificaions of longer level-2 milestones (in bottom right plot). Should contain the columns \code{year} (integer) and \code{title} (character)
#' @param interests list. A list of length up to 4 indicating one's interests / skills. Each entry should be a character vector up to length 5. The more often a value occurs in the vector, the more pronounced it will be.
#' @param font.family character. An optional google font family. Run \code{sysfonts::font.families.google()} to see all of them. Only use when creating plot in a plotting device such as pdf()
#' @param col character. The color palette for the plot. Can be a named palette from the \code{yarrr::piratepal()} function (e.g.; \code{"basel"} or \code{"xmen"}), or a vector of named colors.
#' @param trans numeric. A number between 0 and 1 indicating the transparencies of the colors of boxes in the timeline.
#' @param events.cex numeric. A size multiplier for level-2 milestones.
#' @param year.steps integer. The step size in years of the year labels in the timeline.
#' @param year.range integer. A vector of minimum and maximum years to plot in the timeline.
#' @importFrom grDevices rgb col2rgb gray
#' @importFrom graphics text points abline legend mtext segments rect arrows axis par layout plot plot.new plot.window
#' @importFrom yarrr piratepal
#' @importFrom showtext showtext.begin showtext.end
#' @importFrom sysfonts font.add.google font.families.google
#' @export
#' @details
#'  The argument \code{timeline} specifying events in the timeline should each be a dataframe with at least the following columns:
#' \describe{
#'  \item{title, sub}{character. Titles and subs}
#'  \item{start, end}{integer. Start and end years}
#'  \item{side}{binary. Vertical location of the event. 1 = top, 0 = bottom.}
#'  }
#'  If you want to adjust the locations of elements, you can specify the locations of timeline elements explicitly with additional named columns.
#'  \describe{
#'  \item{box.x0, box.x1, box.y0, box.y1}{numeric. Coordinates of the boxes.}
#'  \item{point.x, point.y}{numeric. Coordinates of the points.}
#'  \item{label.x, label.y}{numeric. Coordinates of the labels.}
#'  \item{label.dir}{string. Directions of the labels. Either "left" or "right".}
#'  }
#'  Note that the vertical axis of the timeline ranges from 0 (the bottom) to 100 (the top).
#' @examples
#'
#' # Walter White's visual resume
#'
#'VisualResume(
#'titles.left = c("Walter White, PhD", "Chemistry, Cooking, Pizza", "*Built with love in R using the InfoResume package: www.ndphillips.github.io/inforesume"),
#'titles.right = c("www.lospolloshermanos.com", "TheOneWhoKnocks@gmail.com", "Full Resume: www.ndphillips.github.io"),
#'titles.right.cex = c(2, 2, 1),
#'titles.left.cex = c(4, 2, 1),
#'timeline.labels = c("Education", "Teaching"),
#'timeline = data.frame(title = c("Grinnell Col", "Ohio U", "U of Basel", "Max Planck Institute", "Old Van", "Gray Matter", "Sandia Laboratories", "J.P. Wynne High School", "A1A Car Wash"),
#'                      sub = c("BA. Student", "MS. Student", "PhD. Student", "PhD. Researcher", "Methamphetamine Research", "Co-Founder", "Chemist", "Chemistry Teacher", "Co-Owner"),
#'                      start = c(1976, 1980.1, 1982.2, 1985, 1996.5, 1987, 1991, 1995, 2001),
#'                      end = c(1980, 1982, 1985, 1987, 1998, 1992, 1995, 1998, 2003),
#'                      side = c(1, 1, 1, 1, 1, 0, 0, 0, 0)),
#'milestones = data.frame(title = c("BA", "MS", "PhD"),
#'                        sub = c("Mathematics", "Chemistry", "Chemistry"),
#'                        year = c(1980, 1982, 1985)),
#'events = data.frame(year = c(1985, 1995, 1997, 1999, 2012),
#'                    title = c("Contributed to Nobel Prize winning experiment.",
#'                              "Honorary mention for best Chemistry teacher of the year.",
#'                              "Created Blue Sky, the most potent methamphetamine ever produced.",
#'                              "Made first $1,000,000.",
#'                              "White, W., & Pinkman, J. (2012). Blue Sky: A method of [...].\nJournal of Psychopharmical Substances, 1(1),.")),
#'interests = list("programming" = c(rep("R", 10), rep("Python", 1), rep("JavaScript", 2), "MatLab"),
#'                 "statistics" = c(rep("Decision Trees", 10), rep("Bayesian", 5), rep("Regression", 3)),
#'                 "leadership" = c(rep("Motivation", 10), rep("Decision Making", 5), rep("Manipulation", 30))),
#'year.steps = 2)
#'


VisualResume <- function(titles.left = c("Main Title", "Sub-title", "Sub-Sub-title"),
						 titles.left.cex = c(4, 3, 2),
						 titles.right = c("A", "B", "C"),
						 titles.right.cex = c(4, 3, 2),
						 timeline.labels = c("", ""),
						 timeline.cex = c(1.8, 1),
						 timeline = NULL,
						 milestones = NULL,
						 events = NULL,
						 events.cex = 1.5,
						 interests = NULL,
						 font.family = NA,
						 col = "xmen",
						 trans = .6,
						 year.steps = 1,
						 year.range = NULL
) {


	# Convert factors to strings

	for(i in 1:ncol(timeline)) {if(class(timeline[,i]) == "factor") {timeline[,i] <- paste(timeline[,i])}}

	# Extract some parameters
	events.selected <- 1:nrow(events)
	top.graph.label <- timeline.labels[1]
	bottom.graph.label <- timeline.labels[2]

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

		if(length(color.vec) < (nrow(timeline))) {

			color.vec <- rep(color.vec, length.out = (nrow(timeline)))

		}
	}


	# Get year range

	if(is.null(year.range)) {
		year.range <- c(floor(min(timeline$start)),
						ceiling(max(timeline$end)))
	}

	year.min <- min(year.range)
	year.max <- max(year.range)


	year.seq <- seq(min(year.range), max(year.range), by = year.steps)

	if(max(year.seq) != year.max) {

		year.seq <- c(year.seq, max(year.seq) + year.steps)

		year.min <- min(year.seq)
		year.max <- max(year.seq)

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

		# Left and Right header titles
		text(rep(.02, 3), c(.65, .3, .05), titles.left, adj = 0, cex = titles.left.cex, family = font.family)
		text(rep(.98, 3), c(.65, .3, .05), titles.right, adj = 1, cex = titles.right.cex, family = font.family)

	}

	# ----------------
	# Body
	# ----------------
	{

		par(mai = c(0, 0, 0, 0))

		# Minimum values of top and bottom
		top.y0 <- 55
		bottom.y0 <- 45

		# Adjust simultaneous starting times

		for(i in 2:nrow(timeline)) {

			if(timeline$start[i] %in% timeline$start[1:(i-1)]) {

				timeline$start[i] <- timeline$start[i] + .1}

		}


		plot(1, xlim = c(year.min - 1, year.max + 1),
			 ylim = c(0, 100), type = "n", xaxt = "n",
			 yaxt = "n", ylab = "", xlab = "",
			 bty = "n")


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

			if("box.x0" %in% names(timeline) == FALSE) {

				timeline$box.x0 <- NA
				change.box.x0 <- TRUE
			}

			if("box.y0" %in% names(timeline) == FALSE) {

				timeline$box.y0 <- NA
				change.box.y0 <- TRUE

			}

			if("box.x1" %in% names(timeline) == FALSE) {

				timeline$box.x1 <- NA
				change.box.x1 <- TRUE

			}

			if("box.y1" %in% names(timeline) == FALSE) {

				timeline$box.y1 <- NA
				change.box.y1 <- TRUE

			}

			for (i in 1:nrow(timeline)) {

				# Get default locations

				if(is.na(timeline$box.x0[i])) {

					side.i <- timeline$side[i]
					box.x0 <- timeline$start[i]
					box.y0 <- switch(paste(side.i),
									 "0" = 45,
									 "1" = 55)
					box.x1 <- timeline$end[i]

					box.y1 <- switch(paste(side.i),
									 "0" = box.y0 - 10,
									 "1" = box.y0 + 10)

					# Am I starting at the same time as a previous box?

					simultaneous.boxes <- sum(timeline$box.x0[1:(i-1)] == box.x0 &
											  	timeline$side[1:(i-1)] == side.i,
											  na.rm = TRUE)

					if(simultaneous.boxes > 0) {box.x0 <- box.x0 + .1 * simultaneous.boxes}

					# Am I starting within a previous box?

					existing.boxes <- sum(timeline$box.x0[-i] < box.x0 &
										  	timeline$box.x1[-i] > box.x0 &
										  	timeline$side[-i] == side.i, na.rm = TRUE)

					box.y1 <- box.y1 + existing.boxes * switch(paste(side.i), "0" = -4, "1" = 4)

					if(change.box.x0) {timeline$box.x0[i] <- box.x0}
					if(change.box.x1) {timeline$box.x1[i] <- box.x1}
					if(change.box.y0) {timeline$box.y0[i] <- box.y0}
					if(change.box.y1) {timeline$box.y1[i] <- box.y1}

				}
			}
		}

		#  -------
		# POINT COORDINATES
		#  --------
		{

			change.point.x <- FALSE
			change.point.y <- FALSE

			if("point.x" %in% names(timeline) == FALSE) {

				change.point.x <- TRUE
				timeline$point.x <- NA

			}

			if("point.y" %in% names(timeline) == FALSE) {

				change.point.y <- TRUE
				timeline$point.y <- NA

			}

			for (i in 1:nrow(timeline)) {

				side.i <- timeline$side[i]

				# Get default locations

				# Does another box start in this box?

				conflicting.l <- timeline$start[-i] > timeline$start[i] & timeline$start[-i] < timeline$end[i]

				if(any(conflicting.l) == FALSE) {

					point.x <- timeline$start[i] + .25 * (timeline$end[i] - timeline$start[i])
					point.y <- timeline$box.y0[i] + switch(paste(side.i), "0" = -5, "1" = 5)
				}

				if(any(conflicting.l)) {

					next.start <- min(timeline$start[-i][conflicting.l])

					point.x <- timeline$start[i] + .25 * (next.start - timeline$start[i])
					point.y <- timeline$box.y0[i] + switch(paste(side.i), "0" = -5, "1" = 5)

				}

				# Is there another point in the same location?

				points.conflicting <- sum(abs(timeline$point.x[-i] - point.x) < .1 &
										  	abs(timeline$point.y[-i] - point.y) < .1, na.rm = TRUE)

				if(any(points.conflicting)) {

					point.x <- point.x + sum(points.conflicting) * .1
					point.y <- point.y + sum(points.conflicting)

				}

				if(change.point.x) {timeline$point.x[i] <- point.x}
				if(change.point.y) {timeline$point.y[i] <- point.y}

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


			if("label.dir" %in% names(timeline) == FALSE) {

				change.label.dir <- TRUE
				timeline$label.dir <- NA

			}

			if("text.adj" %in% names(timeline) == FALSE) {

				timeline$text.adj <- NA
				change.text.adj <- TRUE

			}

			if("elbow" %in% names(timeline) == FALSE) {

				timeline$elbow <- NA
				change.elbow <- TRUE
			}


			if("label.x" %in% names(timeline) == FALSE) {

				timeline$label.x <- NA
				change.label.x <- TRUE

			}

			if("label.y" %in% names(timeline) == FALSE) {

				timeline$label.x <- NA
				change.label.y <- TRUE

			}

			for(i in 1:nrow(timeline)) {

				method <- 2

				if(is.na(timeline$label.x[i])) {

					if(method == 1) {

						side.i <- timeline$sode[i]
						label.x <- timeline$point.x[i]

						# Do other boxes start within x percent of the timeline on the right or left?

						scare.p <- .1
						scare.dist <- scare.p * diff(year.range)

						right.conflicting.l <- (timeline$start[-i] >= timeline$start[i]) & (timeline$start[-i] <= timeline$start[i] + scare.dist) & (timeline$side[-i] == side.i)
						left.conflicting.l <- timeline$start[-i] <= timeline$start[i] & (timeline$start[-i] >= timeline$start[i] - scare.dist) & (timeline$side[-i] == side.i)

						# If there are no conflicts, go right

						if(any(right.conflicting.l) == FALSE &
						   any(left.conflicting.l) == FALSE) {

							label.y <- timeline$box.y1[i] + switch(paste(side.i),
																   "0" = -10,
																   "1" = 10)
							label.dir <- "right"
							text.adj <- 0
							elbow <- .1

						}




						# If there are more right than left conflicts then go left

						if(sum(right.conflicting.l) > sum(left.conflicting.l)) {

							conflicting.box.heights <- timeline$box.y1[-i][left.conflicting.l | right.conflicting.l]
							conflicting.label.heights <- timeline$label.y[-i][left.conflicting.l | right.conflicting.l]

							if(side.i == 1) {

								label.y <- max(c(conflicting.box.heights, timeline$box.y1[i], conflicting.label.heights)) + 10

							}

							if(side.i == 0) {

								label.y <- min(c(conflicting.box.heights, timeline$box.y1[i], conflicting.label.heights)) - 10

							}

							label.dir <- "left"
							text.adj <- 1
							elbow <- -.1
						}

						# If there are more left than right conflicts

						if(sum(right.conflicting.l) <= sum(left.conflicting.l)) {

							conflicting.box.heights <- timeline$box.y1[-i][right.conflicting.l]
							conflicting.label.heights <- timeline$label.y[-i][right.conflicting.l]

							if(side.i == 1) {

								label.y <- max(c(conflicting.box.heights, timeline$box.y1[i], conflicting.label.heights)) + 10

							}

							if(side.i == 0) {

								label.y <- min(c(conflicting.box.heights, timeline$box.y1[i], conflicting.label.heights)) - 10

							}

							label.dir <- "right"
							text.adj <- 0
							elbow <- .1
						}

						# Are there any long labels on the left?

						if(i > 1) {

							label.conflict.l <- (timeline$point.x[-i] - timeline$point.x[i]) > -scare.dist &
								(timeline$point.x[-i] - timeline$point.x[i]) < 0 &
								nchar(timeline$title[-i]) > 8 &
								timeline$label.dir[-i] == "right" &
								timeline$side[-i] == side.i

							if(any(label.conflict.l)) {

								label.y <- label.y + 4 * sum(label.conflict.l)

							}
						}

					}
					if(method == 2) {

						# Grid method

						side.i <- timeline$side[i]
						point.x <- timeline$point.x[i]
						box.y <- timeline$box.y1[i]

						x.loc <- c(point.x)

						if(side.i == 1) {y.loc <- seq(box.y, 90, length.out = 10)}
						if(side.i == 0) {y.loc <- seq(box.y, 10, length.out = 10)}

						location.mtx <- expand.grid(x = x.loc, y = y.loc)

						# Assign initial points

						x.dev <- abs(location.mtx$x - point.x)

						if(timeline$side[i] == 1) {

							y.dev <- abs(location.mtx$y - (box.y + 10))

						}

						if(timeline$side[i] == 0) {

							y.dev <- abs(location.mtx$y - (box.y - 10))

						}

						joint.dev <- x.dev + y.dev

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

								if(timeline$side[j] == timeline$side[i]) {

									other.x.loc <- timeline$label.x[j]
									other.y.loc <- timeline$label.y[j]

									# Are there other labels within scare.p years?

									scare.p <- .15
									scare.d <- scare.p * diff(year.range)

									x.dev.l <- abs(other.x.loc - location.mtx$x) < scare.d

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

							scare.p <- .1
							scare.d <- scare.p * diff(year.range)

							# Potentially conflicting blocks on left
							l.conflicting.l <- any(timeline$side[1:(i - 1)] == timeline$side[i] &
												   	timeline$point.x[1:(i - 1)] > (timeline$point.x[i] - scare.d))

							# Is everything clear on the right for the next scare.d?
							side.i <- timeline$side[i]
							point.x.i <- timeline$point.x[i]

							r.free.l <- nrow(subset(timeline[-i,],
													side == side.i &
														point.x > point.x.i & point.x < (point.x.i + scare.d))) == 0

							if(l.conflicting.l | r.free.l) {label.dir <- "right"
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
				if(change.label.y) {timeline$label.y[i] <- label.y}
				if(change.label.x) {timeline$label.x[i] <- label.x}
				if(change.label.dir) {timeline$label.dir[i] <- label.dir}
				if(change.text.adj) {timeline$text.adj[i] <- text.adj}
				if(change.elbow) {timeline$elbow[i] <- elbow}

			}
		}

		# Draw!

		for(i in 1:nrow(timeline)) {

			# Box

			rect(xleft = timeline$box.x0[i],
				 ybottom = timeline$box.y0[i],
				 xright = timeline$box.x1[i],
				 ytop = timeline$box.y1[i],
				 col = color.vec[i],
				 border = "black",
				 lwd = .5)

			# Points

			points(timeline$point.x[i],
				   timeline$point.y[i],
				   pch = 21,
				   cex = 2,
				   col = "black",
				   bg = "white")

			# Add lines

			# Main line
			segments(timeline$point.x[i],
					 timeline$point.y[i],
					 timeline$label.x[i],
					 timeline$label.y[i],
					 lty = 2)

			# Elbow line
			segments(timeline$label.x[i],
					 timeline$label.y[i],
					 timeline$label.x[i] + timeline$elbow[i],
					 timeline$label.y[i],
					 lty = 2)

			# Main Text

			text(timeline$label.x[i] + timeline$elbow[i],
				 timeline$label.y[i],
				 adj = timeline$text.adj[i],
				 labels = timeline$title[i],
				 cex = timeline.cex[1],
				 family = font.family
			)

			#
			# text.outline(timeline$label.x[i] + timeline$elbow[i],
			#              timeline$label.y[i],
			#              adj = timeline$text.adj[i],
			#              labels = timeline$title[i],
			#              cex = 1.8,
			#              family = font.family,
			#              bg = gray(.97, .5),
			#              w = (year.max - year.min) / 100,
			#              h = 1
			# )

			# Sub Text

			text(timeline$label.x[i] + timeline$elbow[i],
				 timeline$label.y[i] - 3,
				 adj = timeline$text.adj[i],
				 labels = timeline$sub[i],
				 cex = timeline.cex[2],
				 family = font.family)

		}

		# --------- Upper milestones

		text(milestones$year,
			 rep(95, nrow(milestones)),
			 milestones$title, cex = 2, adj = 1, family = font.family, font = 3
		)

		text(milestones$year,
			 rep(90, nrow(milestones)),
			 milestones$sub,
			 cex = 1.5, adj = 1, family = font.family, font = 1)


		# ---- Add Event symbols

		points(events$year, rep(60, length(events$year)), pch = 23, col = "black", bg = "white", cex = 4)
		text(events$year, rep(60, length(events$year)), 1:length(events$year), cex = 1.5, family = font.family)

	}

	# ----------------
	# Bottom
	# ----------------
	{
		# --- Bottom left Interests
		{

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


			for(interest.i in seq_len(n.interests)) {

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


		# ---- Bottom right Events
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
