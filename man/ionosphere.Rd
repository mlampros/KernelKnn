\name{ionosphere}
\alias{ionosphere}
\docType{data}
\title{
Johns Hopkins University Ionosphere database   (binary classification)
}
\description{
This radar data was collected by a system in Goose Bay, Labrador.
This radar data was collected by a system in Goose Bay, Labrador. This system consists of a phased array of 16 high-frequency
antennas with a total transmitted power on the order of 6.4 kilowatts. See the paper for more details.  The targets were free
electrons in the ionosphere. "Good" radar returns are those showing evidence of some type of structure in the ionosphere.
"Bad" returns are those that do not; their signals pass through the ionosphere. Received signals were processed using an autocorrelation
function whose arguments are the time of a pulse and the pulse number. There were 17 pulse numbers for the Goose Bay system.
Instances in this databse are described by 2 attributes per pulse number, corresponding to the complex values returned by the
function resulting from the complex electromagnetic signal.
}
\usage{data(ionosphere)}
\format{
  A data frame with 351 Instances and 35 attributes (including the class attribute, "class")
}
\details{
Sigillito, V. G., Wing, S. P., Hutton, L. V., Baker, K. B. (1989). Classification of radar returns from the ionosphere using neural networks.
Johns Hopkins APL Technical Digest, 10, 262-266.

They investigated using backprop and the perceptron training algorithm on this database. Using the first 200 instances for training, which
were carefully split almost 50 percent positive and 50 percent negative, they found that a "linear" perceptron attained 90.7 percent,
a "non-linear" perceptron attained 92 percent, and backprop an average of over 96 percent accuracy on the remaining 150 test instances,
consisting of 123 "good" and only 24 "bad" instances.  (There was a counting error or some mistake somewhere; there are a total of 351 rather
than 350 instances in this domain.) Accuracy on "good" instances was much higher than for "bad" instances.  Backprop was tested with several
different numbers of hidden units (in [0,15]) and incremental results were also reported (corresponding to how well the different variants of
backprop did after a periodic number of epochs). David Aha (aha@ics.uci.edu) briefly investigated this database. He found that nearest neighbor
attains an accuracy of 92.1 percent, that Ross Quinlan's C4 algorithm attains 94.0 percent (no windowing), and that IB3 (Aha & Kibler, IJCAI-1989)
attained 96.7 percent (parameter settings: 70 percent and 80 percent for acceptance and dropping respectively).
}
\source{
Donor: Vince Sigillito (vgs@aplcen.apl.jhu.edu)

Date: 1989

Source: Space Physics Group

Applied Physics Laboratory

Johns Hopkins University

Johns Hopkins Road

Laurel, MD 20723
}
\references{
https://archive.ics.uci.edu/ml/datasets/Ionosphere
}
\examples{

data(ionosphere)

X = ionosphere[, -ncol(ionosphere)]

y = ionosphere[, ncol(ionosphere)]
}
\keyword{datasets}
