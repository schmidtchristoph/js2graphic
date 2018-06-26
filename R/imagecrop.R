#' Interface to the \code{imagemagic convert} tool for cropping images
#'
#' Automatically cropping of PNG, JPG, etc image files using the \code{convert}
#' comand line tool (\url{http://www.imagemagick.org/}) .
#'
#' @param fileName character string denoting the path/ name of the input file
#'
#' @param outFileName character string denoting the name of the output file. If
#'   not specified, "_cr.[original file format]" is appended to \code{fileName}.
#'
#' @return Returns nothing, but as a side effect saves the cropped image file.
#'
#' @seealso \pkg{knitr} \code{\link[knitr]{plot_crop}}, \pkg{ImageMagick}
#'   \url{http://www.imagemagick.org/}
#'
#' @export
#'
#' @examples
#'
#' library(networkD3)
#' data(MisLinks)
#' data(MisNodes)
#' p <- forceNetwork(Links = MisLinks, Nodes = MisNodes,
#'              Source = "source", Target = "target",
#'              Value = "value", NodeID = "name",
#'              Group = "group", opacity = 0.9,
#'              height = 900, width = 900)
#'
#' # generated PNG needs a serious cropping
#' jsGraphic2Png(p, c(900, 900, 20, 20), slow = TRUE)
#' imagecrop("JSgraphic.png", "JSgraphicCr1.png")
#'
#' # generated PNG needs a serious cropping
#' jsGraphic2Png(p, c(900, 900, 20, 20), slow = FALSE)
#' imagecrop("JSgraphic.png")
#'
#' file.remove(c("JSgraphic.png", "JSgraphicCr1.png", "JSgraphic_cr.png"))
#'
#' @author Christoph Schmidt <schmidtchristoph@@users.noreply.github.com>

# 23.03.17

imagecrop <- function(fileName, outFileName = NULL){
   if(!file.exists(fileName)){ stop("The file corresponding to input argument 'fileName' does not exist.") }


   if(is.null(outFileName)){
      ind          <- stringr::str_locate_all(fileName, stringr::fixed("."))
      nrMatch      <- length(ind)
      ind2         <- ind[[nrMatch]][1, 1]
      fileformat   <- stringr::str_sub(fileName, ind2+1, -1L)
      outFileName  <- paste(stringr::str_sub(fileName, 1L, ind2-1), "_cr.", fileformat, sep="")
   }

   system2( "convert", args = c(paste(fileName, "-trim", outFileName)), stdout = FALSE )
}
