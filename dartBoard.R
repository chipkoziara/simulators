library(raster)
library(tidyverse)

# set seed to make simulation results reproducible
set.seed(2001)

# function for throwing our darts, plotting the results
throwDartsPlot <- function(raster, n_throws) {
  # throw our darts on the dart board
  throws <- spsample(as(extent(board), 'SpatialPolygons'), n_throws, 'random')
  
  # calculate which grid each of our darts hit
  board <- rasterize(throws, board, fun='count', background = 0)
  
  # plot the board
  plot(board)
  # plot the points
  points(throws, pch=20)
}

# function for throwing our darts
throwDarts <- function(raster, n_throws) {
  
  # throw our darts on the dart board
  throws <- spsample(as(extent(board), 'SpatialPolygons'), n_throws, 'random')
  
  # calculate which grid each of our darts hit
  board <- rasterize(throws, board, fun='count', background = 0)
  
  # save board to a table
  board_table <- tbl_df(data.frame(coordinates(board), count=board[]))
  
  # return TRUE if there are 3 or more darts in any grid, else return FALSE
  ifelse(nrow(board_table %>% filter(count >= 3)) > 0, return(TRUE), return(FALSE))
}

dartSims <- function(raster, n_throws, n_sims) {
  
  # save an empty vector to store sim results
  boardEval <- c()
  
  # for loop to run throwDarts sims
  for (i in seq(n_sims)) {
    boardEval <- c(boardEval,
                   throwDarts(raster,
                              n_throws))
  }
  
  # calculates the frequency of boards with 3 or more darts in a single grid
  hitRate <- sum(boardEval)/length(boardEval)
  
  # returns this value when the function is evaluated
  return(hitRate)
}

# create a raster with 4x4 grids
board <- raster(xmn=0, ymn=0, xmx=1, ymx=1, res=0.25)

# simulate throwing 8 darts, plotting the results
throwDartsPlot(board, 8)

# run a monte carlo simulation
sim <- dartSims(board, 8, 100000)
