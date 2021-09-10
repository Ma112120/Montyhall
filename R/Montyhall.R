#' @title
#'      Create a new Monty Hall Problem game.
#'
#'
#' @description
#'            `create_game()` generates a new game that consists of two doors
#'            with goats behind them, and one with a car.
#'
#'
#'
#' @details
#'        The game setup replicates the game on the TV show "Let's
#'        Make a Deal" where there are three doors for a contestant
#'        to choose from, one of which has a car behind it and two
#'        have goats. The contestant selects a door, then the host
#'        opens a door to reveal a goat, and then the contestant is
#'        given an opportunity to stay with their original selection
#'        or switch to the other unopened door. There was a famous
#'        debate about whether it was optimal to stay or switch when
#'        given the option to switch, so this simulation was created
#'        to test both strategies.
#'
#'
#'
#'
#' @param
#'      no arguments are used by the function.
#'
#' @return
#'       The function returns a length 3 character vector
#'       indicating the positions of goats and the car.
#'
#' @examples
#'         create_game()
#'
#' @export
create_game <- function()
{
  a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
  return( a.game )
}

#' @title
#'      The contestant makes their first selection.
#' @description
#'            select_door() allows the contestant to choose only one selection among the three doors that were previously
#'            created in step#1. The sample()allows from randomization each time the contestant makes his first choice.
#' @details
#'        Inside the select_door(), we created a number of objects in order for the contestant to be able to make one
#'        first selection. The first object assigned was the "doors <- c(1,2,3)", which uses numeric data and creates
#'        the options from which the contestant will make his first guess. The second object is "a.pick" it uses the
#'        sample(), which allows randomization while displaying the game.The sample()uses the previously assigned
#'        "doors" object and allows only for one choice by using the argument "size = 1".
#'
#' @param
#'      The "doors" object is of a numeric nature, as it allows the contestant to choose one door from doors numbered
#'       "1, 2, 3",so the result returned after running the function "a.pick" is also a number between 1 & 3
#' @return
#'       The function returns a length one numeric vector indicating the contestant's first door selection
#'
#' @examples
#'         select_door()
#'
#' @export
select_door <- function( )
{
  doors <- c(1,2,3)
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#'      The host opens a goat door
#'  @description
#'             The host has to open a door that is not initially selected by the contestant or a door with a car behind it.
#'             In case that the contestant initially selected the Car door, so the host can choose any of the other goat
#'             doors.
#' @details
#'        The open_goat_door()was used here for the host to open a goat door. Inside this function, we referred back
#'        to two of the previously created objects, game & a.pick. The game object includes character vector "goat,
#'        goat, car". The function uses the "if ()". It states that in case the contestant initially selected
#'        the car door, so the host will open any of the other 2 doors. The second "if ()" states that if the
#'        contestant initially selected one of the two goat doors, so the host will open a door that was not initially
#'        selected by the host and that is not a car door.
#'
#' @param
#'      The doors object is a numeric vector containing numeric values while the game object includes character
#'      vectors. The result returned after running the function is a numeric value between 1 & 3
#' @return
#'       The function return a length of one numeric vector indicating the goat door opened by the host.
#' @examples
#'         this.game <- c("goat","car","goat")
#'         my.initial.pick <- 1
#'         open_goat_door( this.game, my.initial.pick )
#'         my.initial.pick <- 2
#'         open_goat_door( this.game, my.initial.pick )
#'         my.initial.pick <- 3
#'         open_goat_door( this.game, my.initial.pick )

#' @export
open_goat_door <- function( game, a.pick )
{
  doors <- c(1,2,3)
  # if contestant selected car,
  # randomly select one of two goats
  if( game[ a.pick ] == "car" )
  {
    goat.doors <- doors[ game != "car" ]
    opened.door <- sample( goat.doors, size=1 )
  }
  if( game[ a.pick ] == "goat" )
  {
    opened.door <- doors[ game != "car" & doors != a.pick ]
  }
  return( opened.door ) # number between 1 and 3
}



#' @title
#'      Change doors option
#' @description
#'            The contestant is given the option of changing doors from his first selection and change to the other door
#'            that is still closed.
#' @details
#'        The change_door()uses the game-playing strategy of sticking to the contestant's first selection or not
#'        by using the arguments stay = True or stay = False. If the contestant decided to stay (i.e. stay = T), the
#'        returned result or the final pick will be the contestant's initial selection (a.pick). If the contestant
#'        decided not to stay(i.e. stay = F or ! stay), the returned result or the final pick will be the other
#'        closed door (i.e the door that was not initially selected by the contestant or the goat door opened by the
#'        host)
#' @param
#'      The stay argument is a logical vector, as it contains "True" and "false" value.
#' @return
#'       The function returns a length of one numeric vector indicating the final pick made by the contestant after
#'       being offered by the host the option to change doors.
#' @examples
#'         change_door( stay=T, opened.door=1, a.pick=3 )
#'         change_door( stay=F, opened.door=1, a.pick=3 )
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
#'      Determine if the contestant has won the game or not
#' @description
#'      determine_winner()is used along with the if()to determine whether the contestant has won the game or not
#'      based on his final pick
#' @details
#'        the determine_winner()uses the if() because based on the result we can tell whether the contestant has won
#'        or not. It uses the game argument, which is a character vector. It states that if the contestant's final
#'        choice was the car door, so the returned result will be "win". If the the contestant's final choice was a
#'        a goat door, the returned result will be "lose".
#' @param
#'      The game[final.pick] is a character vector because it contains character values, as the contestant
#'      only has to either choose a "car" door or a "goat" door.
#' @return
#'       The function returns a length of one character vector indicating whether the contestant has won or not.
#' @examples
#'         this.game <- c("goat","car","goat")
#'         determine_winner( final.pick=1, game=this.game )
#'         determine_winner( final.pick=2, game=this.game )
#'         determine_winner( final.pick=3, game=this.game )
#' @export
determine_winner <- function( final.pick, game )
{
  if( game[ final.pick ] == "car" )
  {
    return( "WIN" )
  }
  if( game[ final.pick ] == "goat" )
  {
    return( "LOSE" )
  }
}



#' @title
#'      Wrapping all the previous 5 game steps into single play_game()
#' @description
#'            The play_game()wraps up all the previously stated game steps into one function and returns the data frame
#'            that contains the results from one game.
#'
#' @details
#'        The play_game() includes all the codes used in the previous 5 steps with the aim of returning the data frame
#'        that includes all the results related to one game, which means means no repetition. Therefore, we need to
#'        use the loop() to check which is the more dominant strategy, the stay or the switch strategy.The returned
#'        result after running the function is a length of one character vector indicating whether the contestant
#'        has won or not
#' @param
#'      The stay argument is a logical vector, as it includes values of either "True" or "False"
#' @return
#'       The returned result from the function is a length of one character vector indicating whether the contestant
#'       has won or not
#' @examples
#'         play_game()
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
#'      Adding the game to a loop
#' @description
#'            The game will be added to a loop in order to be played 100 times for inferential purposes.
#' @details
#'        The game will be added to a loop and played 100 times, as the simulated results will help us get closer to
#'        the actual theoretical value (i.e. the proportion value of wins achieved by each strategy). Our loop consists
#'        of three parts; the collector, iterator & the binding step. In order to make it visually more appealing,
#'        by using the table proportions function, prob.table() in order to report results as proportions for each
#'        strategy.
#' @param
#'      n & i are numeric vectors.
#' @return
#'       The function returns the results as proportion figures indicating the proportion of the wins and losses of
#'       each strategy
#' @examples
#'         play_n_games()
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
