dat <- read.table(text = "    ONE TWO THREE
1   23  234 324
2   34  534 12
3   56  324 124
4   34  234 124
5   123 534 654",sep = "",header = TRUE)
dat

datm <- melt(cbind(dat, ind = rownames(dat)), id.vars = c('a'))
datm
