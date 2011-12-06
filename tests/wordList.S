library(Aspell)
sp = getSpeller()
addToList(c("duncan", "Omegahat", "RMatlab", "Aspell"), sp)

getWordList(sp, "main")

