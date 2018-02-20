#' Breadth first search of a directed unweighted graph with cycles
#'
#' Performs a breadth first search of node_matrix to find a path from
#' node `from` to node `to`. If a path is found, returns path and distance.
#'
#' @param node_matrix A square matrix where cells are 1 if there is an edge
#' between the row cell and the column cell and 0 otherwise
#' @param from The origin node
#' @param to The destination node
#'
#' @return A list with the keys `status` (TRUE if there is a path,
#' FALSE if not), and if there is a path, also `distance` and `path`.
#'
breadth_first = function(node_matrix, from, to) {
  # Skip the entire algorithm -- they're directly connected
  if(node_matrix[from, to] == 1) {
    return(list(status = TRUE,
                distance = 1,
                path = c(from, to)))
  }

  # Pre-allocate scores
  score = numeric(ncol(node_matrix))

  # Nodes to visit
  unvisited = c(from)
  # Nodes we visited
  visited = c()
  # Minimum paths
  paths = list()
  # Special case for dealing with the first hop
  first_cost = TRUE
  # First node to visit is the source
  node_index = unvisited[1]

  # Loop as long as there's an unvisited node.
  while(length(unvisited)) {
    # If we can go to the goal from here, we're done.
    if(node_matrix[node_index, to] == 1) {
      return(list(status = TRUE,
                  distance = score[node_index] + 1,
                  path = c(paths[[node_index]], to)))
    }

    # Take the current node off the unvisited list.
    unvisited = setdiff(unvisited, node_index)
    # Visit the node
    visited = c(visited, node_index)

    # Which nodes is the current node connected to that we haven't seen yet
    nodes_to_visit = setdiff(which(node_matrix[node_index, ] == 1), visited)
    nodes_to_visit = setdiff(nodes_to_visit, unvisited)

    # We're going to check out any nodes we found
    unvisited = c(unvisited, nodes_to_visit)

    # Score the new nodes we found.
    score[nodes_to_visit] = ifelse(first_cost == TRUE,
                                   1,
                                   score[node_index] + 1)

    # Path the new nodes we found
    for(node in nodes_to_visit) {
      if(first_cost == TRUE) {
        paths[[node]] = c(from, node)
      } else {
        paths[[node]] = c(paths[[node_index]], node)
      }
    }

    # Remove the hack for the first node.
    first_cost = FALSE

    # Next node on deck
    node_index = unvisited[1]
  }

  # Didn't find any results, exhausted nodes to explore.
  return(list(status = FALSE))
}

is_recursive = function(node_matrix) {
  vapply(seq.int(ncol(node_matrix)),
         function(x) { breadth_first(node_matrix, x, x)$status },
         TRUE)
}
