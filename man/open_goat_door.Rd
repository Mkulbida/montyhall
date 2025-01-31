% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/montyhall.R
\name{open_goat_door}
\alias{open_goat_door}
\title{Opens one of the doors that contains a goat.}
\usage{
open_goat_door(a.game, a.pick)
}
\arguments{
\item{This}{function utilizes the arguements 'game' from the 'create_game'
function as well as 'a.pick' from the 'select_door' function.}
}
\value{
The function returns an integer variable that
corresponds to the number of the door that is selected.
}
\description{
'open_goat_door' selects one of the doors that the game
has placed a goat behind.
}
\details{
This function will select and open one of the remaining
doors that were not chosen with the 'select_door' function.
This door will always contain a goat behind it.
}
\examples{
 this.game <- create_game()
 this.game
 my.initial.pick <- select_door()
 my.initial.pick
 opened_door <- open_goat_door(game= this.game,
 a.pick = my.initial.pick)
}
