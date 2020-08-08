library(reticulate)
library(dplyr)
library(ggplot2)

use_python("/usr/bin/python")

pylib <- import_builtins()
chess <- import("chess")
pgn <- import("chess.pgn")

readPGN <- function(pgnFile) {
    pgn.file <- pylib$open(pgnFile)
    games <- list()
    lgame <- NULL
    i <- 1
    repeat {
        lgame <- pgn$read_game(pgn.file)
        if (is.null(lgame)) {
            break
        }
        games <- append(games, lgame)
    }
    pgn.file$close()
    return(games)
}

marchive <- readPGN("archive.pgn")
openr1 <- readPGN("wco2018_r01_open.pgn")
womenr1 <- readPGN("wco2018_r01_women.pgn")
openr2 <- readPGN("wco2018_r02_open.pgn")
womenr2 <- readPGN("wco2018_r02_women.pgn")
openr3 <- readPGN("wco2018_r03_open.pgn")
womenr3 <- readPGN("wco2018_r03_women.pgn")

women123 <- c(womenr1, womenr2, womenr3)
open123 <- c(openr1, openr2, openr3)

availHeaders <- function(games) {
    unique(as.vector(unlist(sapply(games, function(g) {
        pylib$list(g$headers)
    }))))
}

parseHeader <- function(games, header) {
    sapply(games, function(g) {
        hv <- g$headers$get(header)
        ifelse(is.null(hv), NA, hv)
    })
}

gameDate <- function(games) {
    as.Date(parseHeader(games, "Date"), format="%Y.%m.%d")
}

kingSquareIndex <- function(games, colour) {
    sapply(games, function(g) {
        g$end()$board()$king(colour)
    })
}

indexToSquare <- function(index) {
    paste0(letters[(index %% 8) + 1], (index %/% 8) + 1)
}

kingSquare <- function(games, colour) {
    pindex <- kingSquareIndex(games, colour)
    indexToSquare(pindex)
}

resToScore <- function(result, colour = chess$WHITE) {
    cl = rep(colour, length.out = length(result))
    res <- ifelse(result == "0-1", 0,
                  ifelse(result == "1-0", 1, 0.5))
    ifelse(cl == chess$WHITE, res, 1-res)
}

isCheckmate <- function(games) {
    sapply(games, function(g) {
        g$end()$board()$is_checkmate()
    })
}

isStalemate <- function(games) {
    sapply(games, function(g) {
        g$end()$board()$is_stalemate()
    })
}

halfMoves <- function(games) {
    sapply(games, function(g) {
        length(g$end()$board()$move_stack)
    })
}

m.df <- data.frame(
    White = parseHeader(marchive, "White"),
    Black = parseHeader(marchive, "Black"),
    Date = gameDate(marchive),
    Result = parseHeader(marchive, "Result"),
    WKSI = kingSquareIndex(marchive, chess$WHITE),
    BKSI = kingSquareIndex(marchive, chess$BLACK),
    halfMoves = halfMoves(marchive),
    checkmate = isCheckmate(marchive),
    stalemate = isStalemate(marchive)
) %>% mutate(
    WKF = letters[WKSI %/% 8 + 1],
    WKR = WKSI %% 8 + 1,
    BKF = letters[BKSI %/% 8 + 1],
    BKR = BKSI %% 8 + 1,
    MastoPt = resToScore(Result, ifelse(White == "Mastodon", chess$WHITE, chess$BLACK)),
    MastoRes = factor(case_when(MastoPt == 0 ~ "Loss", MastoPt == 1 ~ "Win", TRUE ~ "Draw"),
                      levels = c("Win", "Draw", "Loss"))
)

m.wkfreq <- m.df %>% mutate(WKF = factor(WKF, levels = letters[1:8])) %>%
    group_by(WKF, WKR) %>% summarise(Freq = n(), .groups="drop")
m.wkfreq <- rbind(m.wkfreq, list(WKF = "d", WKR = 1, Freq = NA))
ggplot(m.wkfreq, aes(WKF, WKR, fill = Freq)) + geom_tile() +
    scale_fill_gradient(low = "white", high = "red", limits = c(0, NA), na.value = "white") +
    scale_y_continuous("Rank", limits = c(1, 8), breaks = 1:8) + 
    scale_x_discrete("File") +
    coord_fixed() + theme_classic()
    
m.bkfreq <- m.df %>% mutate(BKF = factor(BKF, levels = letters[1:8])) %>%
    group_by(BKF, BKR) %>% summarise(Freq = n(), .groups="drop")
ggplot(m.bkfreq, aes(BKF, BKR, fill = Freq)) + geom_tile() +
    scale_fill_gradient(low = "white", high = "red", limits = c(0, NA), na.value = "white") +
    scale_y_continuous("Rank", limits = c(1, 8), breaks = 1:8) + 
    scale_x_discrete("File") +
    coord_fixed() + theme_classic()

