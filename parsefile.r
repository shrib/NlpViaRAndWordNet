library("wordnet")
setwd("c:/data/r/")

loadWork <- function (what) {
	rl <- readLines("foo.txt")
	words <- c("")
	cnt <- 0

	for (i in 1:length(rl)) {
		splitSentence = c(unlist(strsplit(rl[i], " ")))

		if (length(splitSentence) > 0) {
			for (n in 1:length(splitSentence)) {
				words[cnt] <- splitSentence[n]
				cnt <- cnt + 1
			}
		}
	}

	print(length(words))
	words <- unique(words)
	print(length(words))

	res <- c("")
	n <- 0

	for (i in 1:length(words)) {
		filter <- getTermFilter("ExactMatchFilter", words[i], T)
		terms <- getIndexTerms(what, 1, filter)

		if (length(sapply(terms, getLemma))> 0) {
			n <- n + 1
			res[n] <- words[i]
		}
	}

	res
}

loadWork("ADVERB")
