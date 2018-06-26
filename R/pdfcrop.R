#' Interface to the \code{pdfcrop} tool shipped with LaTeX
#'
#' Cropping of PDF files using the \code{pdfcrop} comand line tool by Heiko
#' Oberdiek.
#'
#' @param fileName character string denoting the path/ name of the input PDF
#'   file
#'
#' @param outFileName character string denoting the name of the output PDF file.
#'   If not specified, "_cr.pdf" is appended to \code{fileName}.
#'
#' @param margins numeric vector defining how many points of all four white
#'   margins are cropped away (left, top, right, bottom) in addition to the
#'   automatic cropping. Negative values mean that white space is added instead
#'   of cropped away. If NULL, the default, the underlying pdfcrop tool tries to
#'   automatically crop all margins.
#'
#' @return Returns nothing, but as a side effect saves the cropped .pdf file.
#'
#' @seealso \pkg{knitr} \code{\link[knitr]{plot_crop}},
#'   \pkg{pdfcrop} \url{https://www.ctan.org/pkg/pdfcrop?lang=en}
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
#' # generated pdf needs a serious cropping
#' jsGraphic2Pdf(p, c(900, 900, 20, 20), slow = TRUE)
#'
#' # in this case, automatic results (no margins given) are not good enough
#' pdfcrop("JSgraphic.pdf", "JSgraphicCr.pdf")
#'
#' # manually setting margin produces a better result
#' pdfcrop("JSgraphic.pdf", "JSgraphicCr2.pdf", margins = c(2, 1, 93, 2))
#'
#' file.remove(c("JSgraphic.pdf", "JSgraphicCr.pdf", "JSgraphicCr2.pdf"))
#'
#' @author Christoph Schmidt <schmidtchristoph@@users.noreply.github.com>

# 23.03.17

pdfcrop <- function(fileName, outFileName = paste(stringr::str_sub(fileName, 1L, -5L), "_cr.pdf", sep=""),  margins = NULL){
   if(!file.exists(fileName)){ stop("The file corresponding to input argument 'fileName' does not exist.") }

   if( !stringr::str_detect(fileName, ".pdf") ){ stop("Input argument 'fileName' does not refer to a PDF file.") }



   if(is.null(margins)){
      system2("pdfcrop", args = c(fileName, outFileName), stdout = FALSE)
   }
   else {
      margins <- toString(-margins)
      margins <- stringr::str_replace_all(margins, pattern = ",", replacement = "")

      system2("pdfcrop", args = c(paste("--margins '", margins, "'", sep = ""), fileName, outFileName), stdout = FALSE)
   }
}
