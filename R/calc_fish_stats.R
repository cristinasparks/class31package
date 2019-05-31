#' calc_fish_stats
#'
#' compute stuff about fish
#' @param price this is a table of prices for fish, default is fish_price
#' @param count this is a table of fish count, default is fish_count
#' @param graph does the user want a graph
#'
#' @return most common fish at each location
#' @return total revenue at each location
#' @return total revenue across all locations
#' @return a statement stating the total revenue, if graph is true
#' @return a graph of revenue by location
#'




calc_fish_stats = function(count = fish_count, price = fish_price, graph = FALSE){
  # Get the most common fish type at each location
  # Apply which.max to the count table, apply by column
  # Get rownames of the result (otherwise it just returns the index)
  max_fish <- rownames(count)[apply(count, 2, which.max)]

  # Find the total revenue at each location
  # Do matrix/vector math and multiply the price (price column) and count datasets together, giving how much money each fish makes at each location.
  # Then, take the sum of each of these (apply by column) to get a total for each location
  # This multiplication only works if data is in correct order (i.e. the same species in same row for each table), otherwise need to fix it either manually or automatically in the code
  fish_catch_price_loc <- apply(price[,2] * count, 2, FUN = sum)

  # For total revenue across all locations, take the sum of the previous result
  fish_catch_price_tot <- sum(fish_catch_price_loc)

  # For a graph of revenue at each location, convert the price at each location data into a dataframe for ggplot, rename data, plot the data, make it a pretty graph
  graph_rev <- ggplot

  rev_loc_data <- data.frame(total_revenue=fish_catch_price_loc)
  rev_loc_data$loc <- rownames(rev_loc_data)

  graph_revenue <- ggplot(rev_loc_data, aes(x = loc, y = total_revenue, fill = loc))+
    geom_col()+
    theme_minimal()+
    theme(legend.position="none")+
    xlab("Location")+
    ylab("Total Revenue ($)")

  graph_revenue

  # This will return the sentence and the value of fish_catch_price_tot

  totalrevstatement=sprintf("The total revenue across all locations is %s dollars", fish_catch_price_tot)

  # We only want the graph and the statement if the user selects graph = TRUE; the default is graph = FALSE.  Everything else will be returned as a list in both instances.
  if (graph) {
    return(list(mostcommon=max_fish, revbylocation=fish_catch_price_loc, totalrev=fish_catch_price_tot, totalrevstatement, graph_revenue))
  }
  else {
    return(list(mostcommon=max_fish, revbylocation=fish_catch_price_loc, totalrev=fish_catch_price_tot))
  }

}
