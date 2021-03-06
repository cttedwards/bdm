% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggtheme.R
\name{ggtheme}
\alias{ggtheme}
\title{Theme for dignostic plotting using \pkg{ggplot2}}
\usage{
ggtheme(base_size = 12, base_family = "")
}
\description{
Diagnostic plots by \pkg{bdm} are produced using \pkg{ggplot2} with the theme set by a call to \code{ggtheme()}. Note that this function is not exported to avoid conflicts with other packages.
}
\examples{
ggtheme <- function(base_size = 12, base_family = "") 
{
    theme_grey(base_size = base_size, base_family = base_family) \%+replace\% 
      theme(axis.text = element_text(size = rel(1.0)),
            axis.title = element_text(size = rel(1.2)),
            axis.ticks = element_line(colour = "black"), 
            strip.text = element_text(size = rel(1.2)),
            legend.key = element_rect(colour = "grey80"),
            panel.background = element_rect(fill = "white",colour = NA), 
            panel.border = element_rect(fill = NA, colour = "grey50"), 
            panel.grid.major = element_line(colour = "grey80", size = 0.2), 
            panel.grid.minor = element_line(colour = "grey88", size = 0.5), 
            strip.background = element_rect(fill = "grey80", colour = "grey50", size = 0.2)
      )}
      
}
