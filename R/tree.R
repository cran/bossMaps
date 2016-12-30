#' Graphically display folder tree in terminal
#'
#' @param x File path to display
#' @description  Uses \code{sed} to display formatted folder structure.  Needs sed to be installed on system
#'  (may not be available on windows)
#' @return Prints the folder structure
#' @references \url{http://www.cnet.com/news/terminal-fun-options-for-printing-folder-and-subfolder-contents/}

tree<- function(x=NULL){
  if(is.null(x)) x="."
  system(paste0("ls -R ",x," | grep ':$' | sed -e 's/:$//' -e 's/[^-][^\\/]*\\//--/g' -e 's/^/ /' -e 's/-/|/'"))
}
