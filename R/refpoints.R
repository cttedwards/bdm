#'
#' @title Extract reference points
#' 
#' @export
refpoints <- function(x, ...) UseMethod("refpoints")
#' @rdname refpoints
#' @export
refpoints.bdm <- function(object) {
    return(list(msy = object@trace$msy,
                depletion_at_msy = object@trace$dmsy,
                biomass_at_msy = object@trace$biomass_at_msy,
                harvest_rate_at_msy = object@trace$harvest_rate_at_msy))
    }
#}}}
