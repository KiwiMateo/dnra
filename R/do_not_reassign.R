#' Set save folder for caching
#'
#' @param folder string
#'
#' @return NULL, sets global variable \code{save_folder}.
#'
#' @export
set_save_folder <- function(folder = getwd()) {
  if (!file.exists(folder))
    stop("folder does not exist")
  Sys.setenv(save_folder = folder)
}

#' Cached assignment
#'
#' Assignment that (i) doesn't reassign if the object already exists in the
#' current environment, (ii) saves \code{lhs} to the folder set by
#' \code{\link{set_save_folder}}, and (iii) loads from file when \code{lhs.RDS}
#' exists in the save file. Intended only to be used in the global environment.
#'
#' @param lhs,rhs left and right hand side of an assignment
#'
#' @return NULL. Assigns to \code{lhs} and possibly saves an RDS to
#'   \code{save_folder}.
#' @export
#'
#' @examples
#'  \dontrun{
#'    folder <- "."
#'    set_save_folder(folder)
#'    a %<~% 1
#'    a
#'    rm(a)
#'    a %<~% 2
#'    a
#'    .delete(a)
#'    a %<~% 2
#'    a
#'    .rename(a, "b")
#'    a %<~% 1
#'    a
#'    b
#'    list.files(folder)
#'  }
`%<~%` <- function(lhs, rhs) {
  if (Sys.getenv("save_folder") == "")
    stop("cache folder must be set with \"set_save_folder\"")
  lhs <- as.character(substitute(lhs))
  rhs <- substitute(rhs)
  e   <- parent.frame()

  save_file <- file.path(Sys.getenv("save_folder"), paste0(lhs, ".RDS"))

  if (!exists(lhs, envir = e, inherits = FALSE)) {
    if (file.exists(save_file)) {
      rhs <- readRDS(save_file)
    } else {
      tm <- system.time(rhs <- eval(rhs, envir = e))[["elapsed"]]
      attr(rhs, "run_time") <- dtime(tm)
      saveRDS(rhs, file = save_file)
    }
    assign(lhs, rhs, envir = e)
  }
}


#' Delete a cached object
#'
#' @param obj object
#'
#' @return NULL. Deletes \code{obj}, if it exists, and removes saved file, if it
#'   exists.
#' @export
.delete <- function(obj) {
  obj <- as.character(substitute(obj))
  e   <- parent.frame()

  save_file <- file.path(Sys.getenv("save_folder"), paste0(obj, ".RDS"))
  if (exists(obj, envir = e)) remove(list = obj, envir = e)
  if (file.exists(save_file)) file.remove(save_file)
  invisible(NULL)
}


#' Rename a cached object
#'
#' @param obj object
#' @param new_name character
#'
#' @return Boolean.
#' @export
.rename <- function(obj,
                    new_name) {
  obj <- as.character(substitute(obj))
  e   <- parent.frame()

  old_file <- file.path(Sys.getenv("save_folder"), paste0(obj, ".RDS"))
  new_file <- file.path(Sys.getenv("save_folder"), paste0(new_name, ".RDS"))

  if (file.exists(old_file))
    file.rename(old_file, new_file)

  if (exists(obj, envir = e)) {
    assign(new_name, get(obj, envir = e), envir = e)
    remove(list = obj, envir = e)
  }
}


#' Get run time value
#'
#' @param x an object
#'
#' @return A `difftime` object
#' @export
get_runtime <- function(x) attr(x, "run_time")


#' Convert seconds to `difftime`
#'
#' @param s numeric
#'
#' @return A `difftime` object.
dtime <- function(s) {
  out <- as.difftime(s, units = "secs")
  if (s > 24 * 60 * 60) units(out) <- "days"
  else if (s > 60 * 60) units(out) <- "hours"
  else if (s > 60)      units(out) <- "mins"
  out
}
