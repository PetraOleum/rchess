library(reticulate)
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)

use_python("/usr/bin/python")

pylib <- import_builtins()
chess <- import("chess")
pgn <- import("chess.pgn")

# Translate between piece names (lower case) and number, value
piece <- 1:6
names(piece) <- unlist(chess$PIECE_NAMES)
piece_values <- c(1, 3, 3, 5, 9)
names(piece_values) <- unlist(chess$PIECE_NAMES)[1:5]

# Parse pgn file to a list of pgn objects
readPGN <- function(pgnFile) {
    pgn.file <- pylib$open(pgnFile)
    games <- list()
    lgame <- NULL
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

# Load votechess archive
marchive <- readPGN("archive.pgn")

# Load olympiad archives
openr1 <- readPGN("wco2018_r01_open.pgn")
womenr1 <- readPGN("wco2018_r01_women.pgn")
openr2 <- readPGN("wco2018_r02_open.pgn")
womenr2 <- readPGN("wco2018_r02_women.pgn")
openr3 <- readPGN("wco2018_r03_open.pgn")
womenr3 <- readPGN("wco2018_r03_women.pgn")

openall <- list()
womenall <- list()
for r in 1:11 {
    openall <- c(openall, readPGN(sprintf("wco2018_r%0d_open.pgn", r)))
    womenall <- c(womenall, readPGN(sprintf("wco2018_r%0d_women.pgn", r)))
}

women123 <- c(womenr1, womenr2, womenr3)
open123 <- c(openr1, openr2, openr3)

# Get a list of all PGN headers included at least once in list of pgn objects
availHeaders <- function(games) {
    unique(as.vector(unlist(sapply(games, function(g) {
        pylib$list(g$headers)
    }))))
}

# Get a single header value from PGN list; NA if missing
parseHeader <- function(games, header) {
    sapply(games, function(g) {
        hv <- g$headers$get(header)
        ifelse(is.null(hv), NA, hv)
    })
}

# Get Date header as date
gameDate <- function(games) {
    as.Date(parseHeader(games, "Date"), format="%Y.%m.%d")
}

# Get the Square Index (0-63) of the king of a specified colour
kingSquareIndex <- function(games, colour) {
    sapply(games, function(g) {
        g$end()$board()$king(colour)
    })
}

# Translate from index to algebraic square, e.g. 63 -> h8
indexToSquare <- function(index) {
    paste0(letters[(index %% 8) + 1], (index %/% 8) + 1)
}

# Get kingsquare as square
kingSquare <- function(games, colour) {
    pindex <- kingSquareIndex(games, colour)
    indexToSquare(pindex)
}

# Translate a result into a score, e.g. "0-1" from black's perspective is 1 point
resToScore <- function(result, colour = chess$WHITE) {
    cl = rep(colour, length.out = length(result))
    res <- ifelse(result == "0-1", 0,
                  ifelse(result == "1-0", 1, 0.5))
    ifelse(cl == chess$WHITE, res, 1-res)
}

# Determine if game ended in checkmate
isCheckmate <- function(games) {
    sapply(games, function(g) {
        g$end()$board()$is_checkmate()
    })
}

# Determine if game ended in stalemate
isStalemate <- function(games) {
    sapply(games, function(g) {
        g$end()$board()$is_stalemate()
    })
}

# Determine if game ended with insufficient material
insufficientMaterial <- function(games) {
    sapply(games, function(g) {
        g$end()$board()$is_insufficient_material()
    })
}

# Number of halfmoves
halfMoves <- function(games) {
    sapply(games, function(g) {
        length(g$end()$board()$move_stack)
    })
}

# Count number of surviving pieces of given type(s) for player
survPiece <- function(games, piece_name, colour) {
    sapply(games, function(g) {
        pn = 0
        eb = g$end()$board()
        for (p in piece_name) {
            pn = pn + pylib$len(eb$pieces(piece[p], colour))
        }
        return(pn)
    })
}

# Calculate pawn equivalent valuation of all surviving pieces for player
survVal <- function(games, colour) {
    sapply(games, function(g) {
        pv = 0
        eb = g$end()$board()
        for (p in names(piece_values)) {
            pv = pv + piece_values[p] * pylib$len(eb$pieces(piece[p], colour))
        }
        names(pv) <- NULL
        return(pv)
    })
}

# Calculate number of non-pawn, non-king surviving pieces for player
survPieces <- function(games, colour) {
    survPiece(games, c("knight", "bishop", "rook", "queen"), colour)
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
    WKF = letters[WKSI %% 8 + 1],
    WKR = WKSI %/% 8 + 1,
    BKF = letters[BKSI %% 8 + 1],
    BKR = BKSI %/% 8 + 1,
    MastoPt = resToScore(Result, ifelse(White == "Mastodon", chess$WHITE, chess$BLACK)),
    MastoRes = factor(case_when(MastoPt == 0 ~ "Loss", MastoPt == 1 ~ "Win", TRUE ~ "Draw"),
                      levels = c("Win", "Draw", "Loss"))
)

m.wkfreq <- m.df %>% mutate(WKF = factor(WKF, levels = letters[1:8])) %>%
    group_by(WKF, WKR) %>% summarise(Freq = n(), .groups="drop")
m.wkfreq <- rbind(m.wkfreq, list(WKF = "a", WKR = 1, Freq = NA))
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


m.df %>% select(White = WKSI, Black = BKSI) %>%
    pivot_longer(c(White, Black), names_to = "Colour", values_to = "SI") %>%
    mutate(Colour = factor(Colour, levels = c("White", "Black")),
           File = SI %% 8 + 1, Rank = SI %/% 8 + 1) %>%
    group_by(Colour, File, Rank) %>% summarise(Freq = n(), .groups="drop") -> m.kfreq

ggplot(m.kfreq, aes(File, Rank)) + geom_tile(aes(fill=Freq)) +
    facet_wrap(. ~ Colour) + coord_fixed() + theme_minimal() +
    geom_hline(yintercept = 1:9 - 0.5) +
    geom_vline(xintercept = 1:9 - 0.5) +
    scale_fill_gradient(low = "white", high = "red", limits = c(0, max(m.kfreq$Freq)), na.value = "white") +
    scale_y_continuous(breaks = 1:8, expand = c(0,0)) +
    scale_x_continuous(breaks = 1:8, labels = letters[1:8], expand = c(0,0)) +
    theme(panel.grid.major = element_blank())

carchive <- c(women123, open123)

oly.df <- data.frame(
    Division = rep(c("Women", "Open"), c(length(women123), length(open123))),
    Round = parseHeader(carchive, "Round"),
    Board = parseHeader(carchive, "Board"),
    White = parseHeader(carchive, "White"),
    Black = parseHeader(carchive, "Black"),
    WhiteElo = as.numeric(parseHeader(carchive, "WhiteElo")),
    BlackElo = as.numeric(parseHeader(carchive, "BlackElo")),
    WhiteFideID = parseHeader(carchive, "WhiteFideId"),
    BlackFideID = parseHeader(carchive, "BlackFideId"),
    Date = gameDate(carchive),
    Result = parseHeader(carchive, "Result"),
    ECO = parseHeader(carchive, "ECO"),
    WKSI = kingSquareIndex(carchive, chess$WHITE),
    BKSI = kingSquareIndex(carchive, chess$BLACK),
    halfMoves = halfMoves(carchive),
    checkmate = isCheckmate(carchive),
    stalemate = isStalemate(carchive),
    insufficient = insufficientMaterial(carchive),
    WhiteVal = survVal(carchive, chess$WHITE),
    BlackVal = survVal(carchive, chess$BLACK),
    WhitePawns = survPiece(carchive, "pawn", chess$WHITE),
    BlackPawns = survPiece(carchive, "pawn", chess$BLACK),
    WhitePieces = survPieces(carchive, chess$WHITE),
    BlackPieces = survPieces(carchive, chess$BLACK)
) %>% mutate(
    PieceScore = WhiteVal - BlackVal,
    RatingDiff = WhiteElo - BlackElo,
    Result = factor(Result, levels = c("1-0", "1/2-1/2", "0-1"))
)

oly.df <- mutate(oly.df,
    RatingDiff = WhiteElo - BlackElo,
    Result = factor(Result, levels = c("1-0", "1/2-1/2", "0-1"))
)

oly.df %>% group_by(Division, Result, PieceScore) %>%
    summarise(Freq = n(), .groups="drop") -> Pfreq

# Distribution of material difference
ggplot(Pfreq, aes(x = PieceScore, y = Freq, color = Result)) +
    geom_step(direction="mid", size=1.5) + facet_grid(Division~., scales="free_y") +
    theme_classic() + xlab("Material difference at game end (Pawn equivalents)") +
    ylab("Number of games") +
    scale_colour_manual(values = c("#7570b3", "#1b9e77", "#d95f02"),
                        labels = c("White won", "Draw", "Black won"))

# Distribution of ELO difference
ggplot(filter(oly.df, WhiteElo > 0, BlackElo > 0), aes(x = RatingDiff, color = Result)) +
    stat_density(geom="line", position="identity") +
    facet_grid(Division~., scales="free_y") +
    theme_classic() + xlab("ELO rating difference (White - Black)") +
    scale_y_continuous("Relative percentage of games", labels=scales::percent) +
    scale_colour_manual(values = c("#7570b3", "#1b9e77", "#d95f02"),
                        labels = c("White won", "Draw", "Black won"))

oly.df %>% select(Division, White = WKSI, Black = BKSI) %>%
    pivot_longer(c(White, Black), names_to = "Colour", values_to = "SI") %>%
    mutate(Colour = factor(Colour, levels = c("White", "Black")),
           File = SI %% 8 + 1, Rank = SI %/% 8 + 1) %>%
    group_by(Division, Colour, SI, File, Rank) %>% summarise(Freq = n(), .groups="drop") -> oly.kfreq

# Plot of king locations
ggplot(oly.kfreq, aes(File, Rank)) + geom_tile(aes(fill=Freq)) +
    facet_wrap(Division ~ Colour, nrow=1) + coord_fixed() + theme_minimal() +
    geom_hline(yintercept = 1:9 - 0.5) +
    geom_vline(xintercept = 1:9 - 0.5) +
    scale_fill_gradient(low = "white", high = "red", na.value = "white", trans="log10") +
    scale_y_continuous(breaks = 1:8, expand = c(0,0)) +
    scale_x_continuous(breaks = 1:8, labels = letters[1:8], expand = c(0,0)) +
    theme(panel.grid.major = element_blank())

oly.df %>% filter(checkmate) %>%
    select(Division, Result, WKSI, BKSI) %>% 
    mutate(Colour = ifelse(Result == "1-0", "Black", "White"),
           SI = ifelse(Result == "1-0", BKSI, WKSI)) %>%
    group_by(Division, Colour, SI) %>% summarise(Freq = n(), .groups="drop") %>%
    mutate(Colour = factor(Colour, levels = c("White", "Black")),
           File = SI %% 8 + 1, Rank = SI %/% 8 + 1) -> oly.checkmates

# Plot of checkmated king locations
ggplot(oly.checkmates, aes(File, Rank)) + geom_tile(aes(fill=Freq)) +
    facet_grid(Colour ~ Division, as.table = FALSE) + coord_fixed() + theme_minimal() +
    geom_hline(yintercept = 1:9 - 0.5) +
    geom_vline(xintercept = 1:9 - 0.5) +
    scale_fill_gradient(low = "white", high = "red", na.value = "white", limits=c(0, NA)) +
    scale_y_continuous(breaks = 1:8, expand = c(0,0)) +
    scale_x_continuous(breaks = 1:8, labels = letters[1:8], expand = c(0,0)) +
    theme(panel.grid.major = element_blank())

# Overall stats
oly.df %>% group_by(Division) %>% summarise(
    "Total Games" = n(),
    "White wins" = sum(Result == "1-0"),
    "Draws" = sum(Result == "1/2-1/2"),
    "Black wins" = sum(Result == "0-1"),
    "Checkmates" = sum(checkmate),
    "Stalemates" = sum(stalemate),
    "Insuff. mat." = sum(insufficient),
    "Max ELO" = max(WhiteElo, BlackElo),
    "Median ELO" = median(c(WhiteElo, BlackElo)),
    "Median turns" = median(ceiling(halfMoves/2)),
    .groups = "drop"
) %>% knitr::kable()


left_join(oly.kfreq, oly.checkmates, by=c("Division", "Colour", "SI", "File", "Rank")) %>%
    mutate(Freq.y = ifelse(is.na(Freq.y), 0, Freq.y),
           cmProp = Freq.y / Freq.x
    ) -> oly.prop

# Checkmate proportions
ggplot(oly.prop, aes(File, Rank)) + geom_tile(aes(fill=cmProp)) +
    facet_grid(Colour ~ Division, as.table = FALSE) + coord_fixed() + theme_minimal() +
    geom_hline(yintercept = 1:9 - 0.5) +
    geom_vline(xintercept = 1:9 - 0.5) +
    scale_fill_gradient("Checkmates", low = "white", high = "red", na.value = "white", limits=c(0, NA),
                        labels = scales::percent) +
    scale_y_continuous(breaks = 1:8, expand = c(0,0)) +
    scale_x_continuous(breaks = 1:8, labels = letters[1:8], expand = c(0,0)) +
    theme(panel.grid.major = element_blank())

table(oly.df$ECO, oly.df$Division)

oly.df %>% mutate(ECO_1 = substring(ECO, 1, 1),
                  ECO_1 = ifelse(is.na(ECO_1), "Game too short", ECO_1)) %>%
    group_by(Division, ECO_1, Result) %>%
    summarise(Freq = n(), .groups="drop") -> eco.df

# ECO proportions
ggplot(eco.df, aes(x = ECO_1, y = Freq, fill = Result)) +
    geom_col() + facet_grid(.~Division) + theme_classic() +
    xlab("ECO section") + ylab("Number of games") +
    scale_fill_manual(values = c("#7570b3", "#1b9e77", "#d95f02"),
                        labels = c("White won", "Draw", "Black won"))

# Game lengths
ggplot(oly.df, aes(x = ceiling(halfMoves/2), color = Round)) +
    stat_density(geom="line", position="identity") +
    facet_grid(Division~.) +
    theme_classic() +
    xlab("Game length (turns)") +
    scale_y_continuous("Relative proportion of games", labels = scales::percent)
