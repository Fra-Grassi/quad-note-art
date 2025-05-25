random_convex_quad <- function(A) {
  # Function to generate vertices of a random convex quadrilateral of a given "A" area.
  # Arguments:
  # - A: the area of the desired quadrilateral (in plotting units)
  # Value:
  # - quad_df: a data.frame with x and y coordinates of the four points defining the quadrilateral
  
  # 1. Generate random points until their hull has 4 vertices
  # (i.e., generate exactly 4 points that form a convex hull)
  repeat {
    pts <- matrix(rnorm(8), ncol = 2)  # generate 4 random points in 2D
    hull_idx <- chull(pts)  # calculate indices of convex hull in clockwise order
    
    # Check if all 4 random points lie on the hull.
    # If so, return the points in clockwise order, otherwise try again
    if (length(hull_idx) == 4) {
      quad_coords <- pts[hull_idx, , drop = FALSE]
      break
    }
  }
  
  # Compute current area via shoelace formula
  x <- quad_coords[, 1]
  y <- quad_coords[, 2]
  
  # (c(x[-1], x[1]) is used to close polygon by wrapping around first vertex, same for y-coord)
  current_area <- abs(sum(x * c(y[-1], y[1]) - c(x[-1], x[1]) * y)) / 2
  
  # 3. Scale the coordinates to achieve target (input) area
  scale_factor <- sqrt(A / current_area) # scaling all coords by k scales the area by k^2
  quad_df <- quad_coords * scale_factor
  
  # 4. Return a clean data.frame
  quad_df <- as.data.frame(quad_df)
  colnames(quad_df) <- c("x", "y")
  return(quad_df)
  
}