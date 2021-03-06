% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jsGraphic2PdfPng.R
\name{jsGraphic2Image}
\alias{jsGraphic2Image}
\alias{jsGraphic2Pdf}
\alias{jsGraphic2Png}
\title{Save interactive JavaScript web graphics generated by HTML widgets as PDF /
PNG files}
\usage{
jsGraphic2Pdf(graphic, plotDims = c(1000, 500, 20, 20),
  outFile = "JSgraphic.pdf", pathPhantomJS = NULL, slow = FALSE,
  highRes = TRUE)

jsGraphic2Png(graphic, plotDims = c(1000, 500, 20, 20),
  outFile = "JSgraphic.png", pathPhantomJS = NULL, slow = FALSE,
  highRes = TRUE)
}
\arguments{
\item{graphic}{either a plot object (e.g. typeof(\code{graphic} %in%
c("list", "S4"))) generated by a JS visualization function or a character
string denoting the path to the html file that contains the JS code to
generate the graphic that should be stored as an image file. Such a html
file can be generated with \pkg{htmlwidgets}::\code{saveWidget}.}

\item{plotDims}{numeric vector specifying c(width, height, margin left/right,
margin top/bottom)}

\item{outFile}{character string denoting the title of the output file}

\item{pathPhantomJS}{character string denoting the path to the PhantomJS
binary. It is assumed that PhantomJS is on the $PATH, e.g.
"/usr/local/bin/phantomjs", which corresponds to the default value NULL.}

\item{slow}{logical indicating that PhantomJS uses more time for rendering
the visualization and rasterizing it (2500 ms vs. 200 ms). Defaults to
FALSE (i.e. faster rendering).}

\item{highRes}{logical indicating whether plots are rasterized in high
resolution}
}
\value{
Returns nothing, but as a side effect saves the interactive
  JavaScript visualization as either a .pdf or .png file.
}
\description{
JavaScript (JS) data visualization libraries are made accessible in R by
packages that are often based on the \pkg{htmlwidgets} package
(\url{http://www.htmlwidgets.org}). These two functions, \code{jsGraphic2Pdf}
and \code{jsGraphic2Png}, store such interactive web visualizations generated
by JavaScript code as a PDF or PNG file. Whereas normally JavaScript
visualizations are saved as standalone web pages, having PDF or PNG files
means that these plots can easily be used as figures in other documents. With
these two functions the conversion of web visualizations to static graphics
can be easily automated, however, plots are stored in their default layout,
i.e. they cannot be manipulated by the user (for this see also
\code{\link{svgFromHtml}}).
}
\details{
The PDF / PNG file is saved using PhantomJS
  (\url{http://phantomjs.org}) and the JavaScript code available at
  \url{https://github.com/ariya/phantomjs/blob/master/examples/rasterize.js}
}
\note{
Currently, PhantomJS crashes sometimes in case graphics were produced
  with Plotly.
}
\examples{

#####
# already rendered JavaScript visualization saved as web page complete with
# embedded svg image (alluvial/Sankey diagram)
#####

path1 <- system.file("extdata/alplots2_ff.html", package = "js2graphic")
file.copy(path1, getwd())
file  <- "alplots2_ff.html"

jsGraphic2Pdf(file, c(950, 500, 40, 20))
pdfcrop("JSgraphic.pdf")

jsGraphic2Pdf(file, c(950, 500, 40, 20), highRes = FALSE)
pdfcrop("JSgraphic.pdf")

jsGraphic2Png(file, c(950, 500, 40, 20))
imagecrop("JSgraphic.png")

jsGraphic2Png(file, c(950, 500, 40, 20), highRes = FALSE)
imagecrop("JSgraphic.png")

file.remove(c(file, "JSgraphic.pdf", "JSgraphic_cr.pdf",
            "JSgraphic.png", "JSgraphic_cr.png"))



#####
# html saved with htmlwidgets::saveWidget() that contains JavaScript code
# to generate an interactive visualization (alluvial/Sankey diagram);
# the html file does not contain the svg file
#####

path2 <- system.file("extdata/alluvial_js.html", package = "js2graphic")
file.copy(path2, getwd())
file  <- "alluvial_js.html"
jsGraphic2Pdf(file, c(1000, 500, 40, 20))
pdfcrop("JSgraphic.pdf")

jsGraphic2Png(file, c(1000, 500, 40, 20))
imagecrop("JSgraphic.png")

file.remove(c(file, "JSgraphic.pdf", "JSgraphic_cr.pdf",
            "JSgraphic.png", "JSgraphic_cr.png"))



#####
# using a generated graphic/plot object directly
# (social network) https://christophergandrud.github.io/networkD3/
#####

library(networkD3)
data(MisLinks)
data(MisNodes)
p <- forceNetwork(Links = MisLinks, Nodes = MisNodes,
             Source = "source", Target = "target",
             Value = "value", NodeID = "name",
             Group = "group", opacity = 0.9,
             height = 900, width = 900)

# short rendering time: plot has not fully 'unfolded'
jsGraphic2Pdf(p, c(900, 900, 20, 20), slow = FALSE)
jsGraphic2Png(p, c(900, 900, 20, 20), slow = FALSE)

# longer rendering time: rendering has finished
jsGraphic2Pdf(p, c(900, 900, 20, 20), slow = TRUE)
pdfcrop("JSgraphic.pdf")

jsGraphic2Png(p, c(900, 900, 20, 20), slow = TRUE)
imagecrop("JSgraphic.png")

file.remove(c("JSgraphic.pdf", "JSgraphic_cr.pdf",
            "JSgraphic.png", "JSgraphic_cr.png"))



#####
# using a generated graphic/plot object directly
# (heatmap)
#####

library(d3heatmap)
p <- d3heatmap(mtcars, scale = "column", colors = "Blues")
jsGraphic2Pdf(p, c(2500, 2000, 50, 50))
# removing the last 2 empty pages from JSgraphic.pdf
pdfPageExtract("JSgraphic.pdf", 1, 1, "JSgraphic2.pdf")
# cropping with adding a little extra white margin to the automatic crop
pdfcrop("JSgraphic2.pdf", "JSgraphic3.pdf", c(-30, -30, -70, -30))
# JSgraphic3.pdf contains the final cropped figure

file.remove("JSgraphic.pdf", "JSgraphic2.pdf", "JSgraphic3.pdf")



#####
# using a generated graphic/plot object directly
# plotly example 1
#####

library(plotly)
p <- plot_ly(data=iris, x=~Sepal.Length, y=~Petal.Length, type="scatter", mode="markers")
jsGraphic2Pdf(p, c(1200, 1200, 0, 0), slow = FALSE)
# removing the last 2 empty pages from JSgraphic.pdf
pdfPageExtract("JSgraphic.pdf", 1, 1, "JSgraphic2.pdf")

file.remove("JSgraphic.pdf", "JSgraphic2.pdf")



#####
# using a generated graphic/plot object directly
# plotly example 2
#####
library(plotly)

trace_0 <- rnorm(100, mean = 5)
trace_1 <- rnorm(100, mean = 0)
trace_2 <- rnorm(100, mean = -5)
x <- c(1:100)

data <- data.frame(x, trace_0, trace_1, trace_2)

p <- plot_ly(data, x = ~x, y = ~trace_0, name = 'trace 0', type = 'scatter', mode = 'lines') \%>\%
  add_trace(y = ~trace_1, name = 'trace 1', mode = 'lines+markers') \%>\%
  add_trace(y = ~trace_2, name = 'trace 2', mode = 'markers')

jsGraphic2Pdf(p, c(1200, 1200, 0, 0), slow = FALSE)
# removing the last 2 empty pages from JSgraphic.pdf
pdfPageExtract("JSgraphic.pdf", 1, 1, "JSgraphic2.pdf")
file.remove("JSgraphic.pdf", "JSgraphic2.pdf")

}
\seealso{
\code{\link{svgFromHtml}}, \code{\link[knitr]{plot_crop}}
}
\author{
Christoph Schmidt <schmidtchristoph@users.noreply.github.com>
}
