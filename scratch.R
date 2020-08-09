library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)

source("chess.R")

# Load votechess archive
marchive <- readPGN("archive.pgn")

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

save(m.df, file="votechess.Rdata")
load("votechess.Rdata")

# Overall stats
m.df %>% mutate("Human Colour" = ifelse(White == "Mastodon", "White", "Black"),
                "Human Colour" = factor(`Human Colour`, levels = c("White", "Black"))) %>%
    group_by(`Human Colour`) %>% summarise(
    "Total Games" = n(),
    "Wins" = sum(MastoPt == 1),
    "Draws" = sum(MastoPt == 0.5),
    "Losses" = sum(MastoPt == 0),
    "Checkmates" = sum(checkmate),
    "Median turns" = median(ceiling(halfMoves/2)),
    .groups = "drop"
) %>% knitr::kable()

# King positions
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

# Load olympiad archives
openall <- list()
womenall <- list()
for (r in 1:11) {
    openall <- c(openall, readPGN(sprintf("wco2018_r%02d_open.pgn", r)))
    womenall <- c(womenall, readPGN(sprintf("wco2018_r%02d_women.pgn", r)))
    print(r)
}
carchive <- c(womenall, openall)

# Very slow - see below
oly.df <- data.frame(
    Division = rep(c("Women", "Open"), c(length(womenall), length(openall))),
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
) %>% filter(!is.na(Result))

save(oly.df, file="Olympiad.Rdata")

# Just skip to here if file already generated
load("Olympiad.Rdata")

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

# Game lengths
ggplot(oly.df, aes(x = ceiling(halfMoves/2), color = Round)) +
    stat_density(geom="line", position="identity") +
    facet_grid(Division~.) +
    theme_classic() +
    xlab("Game length (turns)") +
    scale_y_continuous("Relative proportion of games", labels = scales::percent)

# Distribution of material difference
oly.df %>% group_by(Division, Result, PieceScore) %>%
    summarise(Freq = n(), .groups="drop") -> Pfreq

ggplot(Pfreq, aes(x = PieceScore, y = Freq, color = Result)) +
    geom_step(direction="mid", size=1.5) + facet_grid(Division~., scales="free_y") +
    theme_classic() + xlab("Material difference at game end (pawn equivalents)") +
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

# ECO proportions
oly.df %>% mutate(ECO_1 = substring(ECO, 1, 1),
                  ECO_1 = ifelse(is.na(ECO_1), "Game too short", ECO_1)) %>%
    group_by(Division, ECO_1, Result) %>%
    summarise(Freq = n(), .groups="drop") -> eco.df

ggplot(eco.df, aes(x = ECO_1, y = Freq, fill = Result)) +
    geom_col() + facet_grid(.~Division) + theme_classic() +
    xlab("ECO section") + ylab("Number of games") +
    scale_fill_manual(values = c("#7570b3", "#1b9e77", "#d95f02"),
                        labels = c("White won", "Draw", "Black won"))

# Plot of king locations
oly.df %>% select(Division, White = WKSI, Black = BKSI) %>%
    pivot_longer(c(White, Black), names_to = "Colour", values_to = "SI") %>%
    mutate(Colour = factor(Colour, levels = c("White", "Black")),
           File = SI %% 8 + 1, Rank = SI %/% 8 + 1) %>%
    group_by(Division, Colour, SI, File, Rank) %>% summarise(Freq = n(), .groups="drop") -> oly.kfreq

ggplot(oly.kfreq, aes(File, Rank)) + geom_tile(aes(fill=Freq)) +
    facet_wrap(Division ~ Colour, nrow=1) + coord_fixed() + theme_minimal() +
    geom_hline(yintercept = 1:9 - 0.5) +
    geom_vline(xintercept = 1:9 - 0.5) +
    scale_fill_gradient(low = "white", high = "red", na.value = "white", trans="log10") +
    scale_y_continuous(breaks = 1:8, expand = c(0,0)) +
    scale_x_continuous(breaks = 1:8, labels = letters[1:8], expand = c(0,0)) +
    theme(panel.grid.major = element_blank())

# Plot of checkmated king locations
oly.df %>% filter(checkmate) %>%
    select(Division, Result, WKSI, BKSI) %>% 
    mutate(Colour = ifelse(Result == "1-0", "Black", "White"),
           SI = ifelse(Result == "1-0", BKSI, WKSI)) %>%
    group_by(Division, Colour, SI) %>% summarise(Freq = n(), .groups="drop") %>%
    mutate(Colour = factor(Colour, levels = c("White", "Black")),
           File = SI %% 8 + 1, Rank = SI %/% 8 + 1) -> oly.checkmates

ggplot(oly.checkmates, aes(File, Rank)) + geom_tile(aes(fill=Freq)) +
    facet_grid(Colour ~ Division, as.table = FALSE) + coord_fixed() + theme_minimal() +
    geom_hline(yintercept = 1:9 - 0.5) +
    geom_vline(xintercept = 1:9 - 0.5) +
    scale_fill_gradient(low = "white", high = "red", na.value = "white", limits=c(0, NA)) +
    scale_y_continuous(breaks = 1:8, expand = c(0,0)) +
    scale_x_continuous(breaks = 1:8, labels = letters[1:8], expand = c(0,0)) +
    theme(panel.grid.major = element_blank())

# Checkmate proportions
left_join(oly.kfreq, oly.checkmates, by=c("Division", "Colour", "SI", "File", "Rank")) %>%
    mutate(Freq.y = ifelse(is.na(Freq.y), 0, Freq.y),
           cmProp = Freq.y / Freq.x
    ) -> oly.prop

ggplot(oly.prop, aes(File, Rank)) + geom_tile(aes(fill=cmProp)) +
    facet_grid(Colour ~ Division, as.table = FALSE) + coord_fixed() + theme_minimal() +
    geom_hline(yintercept = 1:9 - 0.5) +
    geom_vline(xintercept = 1:9 - 0.5) +
    scale_fill_gradient("Checkmates", low = "white", high = "red", na.value = "white", limits=c(0, NA),
                        labels = scales::percent) +
    scale_y_continuous(breaks = 1:8, expand = c(0,0)) +
    scale_x_continuous(breaks = 1:8, labels = letters[1:8], expand = c(0,0)) +
    theme(panel.grid.major = element_blank())
