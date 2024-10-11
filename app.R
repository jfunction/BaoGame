source("packages.R")

PLAYERS <- c("Red", "Green")
PLAYER_COLOURS <- c("#FFB3BA", "#BAFFC9")

# Initialize game state
initialize_game <- function() {
  board <- matrix(0, nrow = 4, ncol = 8)
  board[2:3, ] <- 4
  list(
    board = board,
    p1_score = 0,
    p2_score = 0,
    current_player = 1,
    game_over = FALSE,
    winner = NULL
  )
}

# Function to determine next location
next_location <- function(current_location) {
  row <- current_location[1]
  col <- current_location[2]
  if (row %in% c(1,3)) {
    result <- if (col==8) c(row+1,col) else c(row,col+1)
  } else {
    result <- if (col==1) c(row-1,col) else c(row,col-1)
  }
  result
}

# Function to make a move
make_move <- function(game_state, row, col) {
  board <- game_state$board
  current_player <- game_state$current_player
  
  # Check if the move is valid
  if ((current_player == 1 && row > 2) || (current_player == 2 && row <= 2) || board[row, col] == 0) {
    return(game_state)
  }
  
  pieces <- board[row, col]
  board[row, col] <- 0
  current_location <- c(row, col)
  
  # Distribute pieces
  while (pieces > 0) {
    current_location <- next_location(current_location)
    row <- current_location[1]
    col <- current_location[2]
    if ((current_player == 1 && row <= 2) ||
        (current_player == 2 && row > 2)) {
      board[row, col] <- board[row, col] + 1
      pieces <- pieces - 1
    }
    # If final piece is not empty, pick all of them up and restart the process:
    if (pieces == 0 & board[row, col] > 1) {
      pieces <- board[row, col]
      board[row, col] <- 0
    }
  }
  # If the column of the final piece is in row 2 or 3 then pick up the players pieces in that column
  if (row %in% c(2, 3)) {
    captured <- board[5-row, col]
    # Update the score:
    if (row==2) {
      game_state$p1_score <- game_state$p1_score + captured
    }else{
      game_state$p2_score <- game_state$p2_score + captured
    }
    # Remove the captured pieces from the board:
    board[5-row, col] <- 0
  }
  
  # Check if game is over
  player1_pieces <- sum(board[1:2, ])
  player2_pieces <- sum(board[3:4, ])
  
  if (player1_pieces == 0 || player2_pieces == 0) {
    game_state$game_over <- TRUE
    game_state$winner <- ifelse(player1_pieces > 0, 1, 2)
    game_state$board <- board
  } else {
    game_state$board <- board
    game_state$current_player <- 3 - current_player  # Switch player
  }
  
  game_state
}

ui <- page_sidebar(
  title = "Bao Game",
  sidebar = sidebar(
    h4("Game Information"),
    textOutput("current_player"),
    textOutput("game_status"),
    br(),
    actionButton("undo", "Undo"),
    actionButton("redo", "Redo")
  ),
  layout_columns(
    fill = FALSE,
    value_box(
      title = "Player 1",
      value = textOutput("player1_pieces"),
      theme = value_box_theme(bg=PLAYER_COLOURS[1])
    )
  ),
  card(
    full_screen = TRUE,
    card_header("Bao Board"),
    uiOutput("game_board")
  ),
  layout_columns(
    fill = FALSE,
    value_box(
      title = "Player 2",
      value = textOutput("player2_pieces"),
      theme = value_box_theme(bg=PLAYER_COLOURS[2])
    )
  ),
  actionButton("reset_game", "Reset Game")
)

server <- function(input, output, session) {
  session$allowReconnect(TRUE)
  
  game_history <- reactiveVal(list(initialize_game()))
  current_state_index <- reactiveVal(1)
  
  game_state <- reactive({
    game_history()[[current_state_index()]]
  })
  
  output$current_player <- renderText({
    paste("Current Player:", PLAYERS[game_state()$current_player])
  })
  
  output$game_status <- renderText({
    if (game_state()$game_over) {
      paste("Game Over! Player", PLAYERS[game_state()$winner], "wins!")
    } else {
      "Game in progress"
    }
  })
  
  output$player1_pieces <- renderText({
    game_state()$p1_score
  })
  
  output$player2_pieces <- renderText({
    game_state()$p2_score
  })
  
  output$game_board <- renderUI({
    board <- game_state()$board
    
    lapply(1:4, function(row) {
      div(
        style = "display: flex; justify-content: center;",
        lapply(1:8, function(col) {
          actionButton(
            inputId = paste0("cell_", row, "_", col),
            label = board[row, col],
            style = paste0(
              "width: 50px; height: 50px; margin: 2px; ",
              "background-color: ", ifelse(row <= 2, PLAYER_COLOURS[1], PLAYER_COLOURS[2]), "; ",
              "font-size: 20px;"
            )
          )
        })
      )
    })
  })
  
  observeEvent(input$reset_game, {
    game_history(list(initialize_game()))
    current_state_index(1)
  })
  
  observeEvent(input$undo, {
    if (current_state_index() > 1) {
      current_state_index(current_state_index() - 1)
    }
  })
  
  observeEvent(input$redo, {
    if (current_state_index() < length(game_history())) {
      current_state_index(current_state_index() + 1)
    }
  })
  
  lapply(1:4, function(row) {
    lapply(1:8, function(col) {
      observeEvent(input[[paste0("cell_", row, "_", col)]], {
        if (!game_state()$game_over) {
          new_state <- make_move(game_state(), row, col)
          if (!identical(new_state, game_state())) {
            new_history <- game_history()[1:current_state_index()]
            new_history[[length(new_history) + 1]] <- new_state
            game_history(new_history)
            current_state_index(length(new_history))
          }
        }
      })
    })
  })
}

shinyApp(ui, server)
