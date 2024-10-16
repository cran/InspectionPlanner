\name{InspectionPlanner-package}
\alias{InspectionPlanner-package}
\alias{InspectionPlanner}
\docType{package}
\title{
Phytosanitary Inspection Sampling Planner 
}
\author{
Gustavo Ramirez-Valverde, 
Maintainer: Gustavo Ramirez-Valverde <gustavoramirezvalverde@gmail.com>,
Luis Gabriel Otero-Prevost [aut], 
Pedro Macias-Canales [ctb], 
Juan A. Villanueva-Jimenez [ctb], 
Jorge Luis Leyva-Vazquez [ctb]
}
\description{
The package is a shiny application that assists in planning sampling for phytosanitary inspections, where acceptance sampling is performed on batches of products arranged on pallets. The package generates a diagram that identifies all the units on the pallets, selects the units to be sampled, and documents which units were chosen for inspection.
}
\details{
In phytosanitary inspections conducted at customs, acceptance sampling is commonly performed on batches of products arranged on pallets. This package helps to plan this type of sampling by generating a visual diagram that identifies each unit on the pallets and selects the samples to be inspected. Additionally, it documents which units were selected for inspection, aiding in recording the process and ensuring that phytosanitary inspection standards are met. It supports both simple random sampling and systematic sampling with a random start.

The package allows the user to define the size of a lot to be inspected, based on the number of rows, columns, and pallet height, providing flexibility in modeling different pallet configurations.
}
\references{
Lohr, S. (2019) Sampling, second edition, Chapman and Hall/CRC (ISBN: 9780367273415)
Cochran, W.G. (1977) Sampling Techniques, third edition, Wiley (ISBN: 9780471162407)
}
\examples{
if (interactive()) {
  run_app()
}
}
\keyword{package}
