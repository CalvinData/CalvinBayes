#' Plot DAGs simply
#'
#' Create a simple ggplot displaying a DAG created with `dagitt::dagitty`
#' or `ggdag::dagify()`.
#'
#' @param dag a DAG
#' @param size size of the nodes
#' @param highlight a vector of names of nodes to display in a highlighted color
#'
#' @importFrom ggdag geom_dag_point geom_dag_text geom_dag_edges theme_dag
#' @export
#' @examples
#' ggdag::dagify(A ~ B + C, B ~ C, D ~ C) %>%
#'  gg_dag(highlight = c("B", "C"), size = 9 )

gg_dag <- function(dag, size = 7, highlight = "U") {

  dag %>%
    ggplot2::ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    ggdag::geom_dag_point(aes(color = name %in% highlight),
                   alpha = 1/2, size = size, show.legend = F) +
    ggdag::geom_dag_text(color = "black") +
    ggdag::geom_dag_edges() +
    scale_color_manual(values = c("steelblue", "orange")) +
    ggdag::theme_dag()
}

