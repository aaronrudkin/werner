% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/graph_traversal.R
\name{breadth_first}
\alias{breadth_first}
\title{Breadth first search of a directed unweighted graph with cycles}
\usage{
breadth_first(node_matrix, from, to)
}
\arguments{
\item{node_matrix}{A square matrix where cells are 1 if there is an edge
between the row cell and the column cell and 0 otherwise}

\item{from}{The origin node}

\item{to}{The destination node}
}
\value{
A list with the keys `status` (TRUE if there is a path,
FALSE if not), and if there is a path, also `distance` and `path`.
}
\description{
Performs a breadth first search of node_matrix to find a path from
node `from` to node `to`. If a path is found, returns path and distance.
}
