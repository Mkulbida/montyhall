#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies.
#'
#' @param ... no arguments are used by the function.
#'
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
}



#' @title
#'  Select a door within the Monty Hall Problem Game
#' @description
#'  'select_door'chooses 1 door
#'  within the Monty Hall Problem game
#' @details
#'  This function will select 1 door at random from 3
#'  the possible doors available for a contestant's
#'  initial selection.
#' @param
#'  No arguements are used by this function.
#' @return
#'  The function returns an integer variable that
#'  corresponds to the number of the door that is selected.
#' @examples
#'  select_door()
#' @export
select_door <- function( )
{
  doors <- c(1,2,3)
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#'  Opens one of the doors that contains a goat.
#' @description
#'  'open_goat_door' selects one of the doors that the game
#'  has placed a goat behind.
#' @details
#'  This function will select and open one of the remaining
#'  doors that were not chosen with the 'select_door' function.
#'  This door will always contain a goat behind it.
#' @param
#'  This function utilizes the arguements 'game' from the 'create_game'
#'  function as well as 'a.pick' from the 'select_door' function.
#' @return
#'  The function returns an integer variable that
#'  corresponds to the number of the door that is selected.
#' @examples
#'  this.game <- create_game()
#'  this.game
#'  my.initial.pick <- select_door()
#'  my.initial.pick
#'  opened_door <- open_goat_door(game= this.game,
#'  a.pick = my.initial.pick)
#' @export
open_goat_door <- function( a.game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats
   if( a.game[ a.pick ] == "car" )
   {
     goat.doors <- doors[ a.game != "car" ]
     opened.door <- sample( goat.doors, size=1 )
   }
   if( a.game[ a.pick ] == "goat" )
   {
     opened.door <- doors[ a.game != "car" & doors != a.pick ]
   }
   return( opened.door ) # number between 1 and 3
}



#' @title
#'  Allows the user to switch their previously selected door.
#' @description
#'  'change_door' allows the user to determine if they
#'  would like to switch their selected door to the
#'  remaining door.
#' @details
#'  This function allows the user to switch their selected
#'  door to the one not chosen by the 'select_door' and
#'  'open_goat_door' functions. Users have the ability to
#'  keep their original choice or to switch doors.
#' @param
#'  This function utilizes the arguements 'opened.door' from
#'  the 'open_goat_door' function and 'a.pick' from the
#'  'select_door' function.
#' @return
#'  This function will return an integer variable that
#'  corresponds to the door number that the user has
#'  selected after deciding if they will switch doors.
#'  Doors can be switched by utilizing 'stay=FALSE'
#' @examples
#'  final.pick <- change_door( stay=F,
#'  opened.door=opened.door,
#'  a.pick=my.initial.pick )
#'
#'  final.pick <- change_door( stay=T,
#'  opened.door=opened.door,
#'  a.pick=my.initial.pick )
#'
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3)

   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ]
   }

   return( final.pick )  # number between 1 and 3
}



#' @title
#'  Determines if the user has won the game.
#' @description
#'  'determine_winner' will decide if the user has won
#'  the game.
#' @details
#'  This function decides if the user has won the game
#'  based on their final selected door.  The final selection
#'  must correspond to the door that has a car behind it as indicated
#'  by the 'create_game' function.
#' @param
#'  This function utilizes the arguements 'final.pick' from the 'change_doors()'
#'  function and 'a.game' from the 'create_game()' function.
#' @return
#'  This function will return a character vector of either 'WIN' or 'LOSE'
#'  depending on the result of the game.
#' @examples
#'  this.game
#'  my.initial.pick
#'  my.final.pick <- change_door( stay=T,
#'                            opened.door=opened.door,
#'                            a.pick=my.initial.pick )
#'  determine_winner( final.pick=my.final.pick,
#'                  game=this.game )
#'
#'  my.final.pick <- change_door( stay=F,
#'                              opened.door=opened.door,
#'                              a.pick=my.initial.pick )
#'  determine_winner( final.pick=my.final.pick,
#'                  game=this.game )
#' @export
determine_winner <- function( final.pick, a.game )
{
   if( a.game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( a.game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}





#' @title
#'  Packages all functions into a single funtion to
#'  play the Monty Hall Problem game.
#' @description
#'  'play_game' will package all required functions
#'  to allow for the completion of the Monty Hall Problem.
#' @details
#'   This function allows a user to play the Monty Hall
#'   Problem game without the need to complete each required
#'   function individually, as everything required is bundled
#'   into this function. It will track results based on outcome
#'   (win or lose) as well as based on strategy (stay or switch)
#'   to determine the best possible strategy.
#' @param
#'   This function does not rely on any arguements.
#' @return
#'   This function will return a data frame that will show the
#'   final results (win or lose) based on the strategy chosen
#'   (to stay or switch).
#' @examples
#'   return(game.results)
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )

  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title
#'  Simulates the Monty Hall Problem game
#' @description
#'  'play_n_games' creates a loop to simulate the
#'  Monty Hall Problem game 100 times.
#' @details
#'  This function creates a loop of the Monty Hall Problem
#'  Game. The function will be simulated 100 times and the
#'  results tabulated in a data frame showing the outcome
#'  based on the chosen strategy.
#' @param
#'  This function does not rely on any arguements but
#'  requires the use of the 'dplyr' package.
#' @return
#'  This function will return a data frame detailing the
#'  final results (win or lose) based on the strategy
#'  chosen (to stay or switch doors)
#' @examples
#'  return(results.df)
#' @export
play_n_games <- function( n=100 )
{

  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome
    loop.count <- loop.count + 1
  }

  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>%
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>%
  print()

  return( results.df )

}
