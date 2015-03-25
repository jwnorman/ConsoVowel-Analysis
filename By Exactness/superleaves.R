library(ENmisc)

leaves <- read.table(file="C:\\Kaggle - other\\Superleaves\\superleaves.txt", sep=' ', col.names=c("Leave", "Valuation"), colClasses=c("character", "numeric"))

save(leaves, file="C:\\Kaggle - other\\Superleaves\\leaves.Rda")
save(leaves, file="~/Documents/Scrabble/Studies/Vowel Consonant Ratio Performance/By Exactness/leaves.Rda")
load(file="~/Documents/Scrabble/Studies/Vowel Consonant Ratio Performance/By Exactness/leaves.Rda")

isVowel <- function(ch) {
	return (ch %in% c('A', 'E', 'I', 'O', 'U', 'a', 'e', 'i', 'o', 'u'))
}

isBlank <- function(ch) {
	return (ch == '?')
}

countBlanks <- function(lv) {
	lv.ch <- unlist(strsplit(lv, ''))
	numBlanks <- sum(isBlank(lv.ch))
	return(numBlanks)
}

countVowels <- function(lv) {
	lv.ch <- unlist(strsplit(lv, ''))
	numVowels <- sum(isVowel(lv.ch))
	return(numVowels)
}

leaves$numVowels <- sapply(leaves$Leave, countVowels)
leaves$numBlanks <- sapply(leaves$Leave, countBlanks)
leaves$numConsos <- nchar(leaves$Leave) - leaves$numVowels - leaves$numBlanks
leaves$CVratio   <- leaves$numConsos / leaves$numVowels
leaves$CminusV   <- leaves$numConsos - leaves$numVowels
leaves$racklen   <- nchar(leaves$Leave)

# Overall Valuation density for all possible racks
overallValuationDensity <- density(leaves$Valuation)
plot(overallValuationDensity)

leaves.sorted <- leaves[order(leaves$prob, decreasing=TRUE), ]
head(leaves.sorted[leaves.sorted$racklen == 6, ], 100)
tail(leaves.sorted,20)
# Valuation by Ratio, 0 blanks only
#pdf(file="C:\\Kaggle - other\\Superleaves\\valuation-by-ratio.pdf")
boxplot(leaves$Valuation[leaves$numBlanks==0] ~ signif(leaves$CVratio[leaves$numBlanks==0], 2), ylim=c(-40,60), cex.axis=.75, xlab="Consonant to Vowel Ratio", ylab="Valuation", main="No Blanks on Rack")
mtext(side=1, "Ratio:", at=0, padj=2, cex=.75)
mtext(side=1, "#Cs  :", at=0, padj=3, col="blue", cex=.75)
mtext(side=1, "#Vs  :", at=0, padj=4, col="blue", cex=.75)
mtext(side=1, c('0',   '1', '1', '1', '1,2', '2', '1-3', '3', '2,4', '3', '4', '5', '1:6'), at=1:13, padj=3, col="blue", cex=.75)
mtext(side=1, c('1-6', '5', '4', '3', '2,4', '3', '1-3', '2', '1,2', '1', '1', '1', '0'  ), at=1:13, padj=4, col="blue", cex=.75)
text(1:13, tapply(leaves$Valuation[leaves$numBlanks==0], leaves$CVratio[leaves$numBlanks==0], median)+2, tapply(leaves$Valuation[leaves$numBlanks==0], leaves$CVratio[leaves$numBlanks==0], median), cex=.5)
#dev.off()

# Compare probability to Zyzzyva's probability by looking at leaves that are alphagrams of words
# they match! yay
real <- read.table("~/Documents/Scrabble/Studies/Vowel Consonant Ratio Performance/By Exactness/2-6s_0blanks.txt", sep='\t', col.names = c("Word", "Alphagram", "ProbabilityRank"), colClasses = c("character", "character", "integer"))
realleaves <- leaves[leaves$Leave %in% real$Alphagram, ]
combined <- merge(real, realleaves, by.x = "Alphagram", by.y = "Leave")
nrow(real) - nrow(combined)
setdiff(real$Alphagram, combined$Alphagram) # 2 K's, 3 B's, 3 M's, 2 Z's, etc.
combined$rank.choose <- integer(nrow(combined))
combined$rank.choose[combined$racklen==2] <- rank(1/combined$prob.choose[combined$racklen==2], ties.method="first")
combined$rank.choose[combined$racklen==3] <- rank(1/combined$prob.choose[combined$racklen==3], ties.method="first")
combined$rank.choose[combined$racklen==4] <- rank(1/combined$prob.choose[combined$racklen==4], ties.method="first")
combined$rank.choose[combined$racklen==5] <- rank(1/combined$prob.choose[combined$racklen==5], ties.method="first")
combined$rank.choose[combined$racklen==6] <- rank(1/combined$prob.choose[combined$racklen==6], ties.method="first")
combined.sorted <- combined[order(combined$racklen, combined$rank.choose, decreasing=FALSE), ]

# Valuation by Ratio, 0 blanks only, and only 6's
#pdf(file="C:\\Kaggle - other\\Superleaves\\valuation-by-ratio6.pdf")
boxplot(leaves$Valuation[leaves$numBlanks==0] ~ signif(leaves$CVratio[leaves$numBlanks==0], 2), ylim=c(-40,60), cex.axis=.75, xlab="Consonant to Vowel Ratio", ylab="Valuation", main="No Blanks on Rack")
mtext(side=1, "Ratio:", at=0, padj=2, cex=.75)
mtext(side=1, "#Cs  :", at=0, padj=3, col="blue", cex=.75)
mtext(side=1, "#Vs  :", at=0, padj=4, col="blue", cex=.75)
mtext(side=1, c('0',   '1', '1', '1', '1,2', '2', '1-3', '3', '2,4', '3', '4', '5', '1:6'), at=1:13, padj=3, col="blue", cex=.75)
mtext(side=1, c('1-6', '5', '4', '3', '2,4', '3', '1-3', '2', '1,2', '1', '1', '1', '0'  ), at=1:13, padj=4, col="blue", cex=.75)
text(1:13, tapply(leaves$Valuation[leaves$numBlanks==0], leaves$CVratio[leaves$numBlanks==0], median)+2, tapply(leaves$Valuation[leaves$numBlanks==0], leaves$CVratio[leaves$numBlanks==0], median), cex=.5)
#dev.off()

# Valuation by Diff, 0 blanks only
pdf(file="C:\\Kaggle - other\\Superleaves\\valuation-by-vcdiff.pdf")
boxplot(leaves$Valuation[leaves$numBlanks==0] ~ leaves$CminusV[leaves$numBlanks==0], ylim=c(-40,60), cex.axis=.75, xlab="Consonant - Vowel", ylab="Valuation", main="No Blanks on Rack")
text(1:13, tapply(leaves$Valuation[leaves$numBlanks==0], leaves$CminusV[leaves$numBlanks==0], median)+2, tapply(leaves$Valuation[leaves$numBlanks==0], leaves$CminusV[leaves$numBlanks==0], median), cex=.5)
dev.off()

# Valuation by Ratio, 0,1,2 blanks
par(mfrow=c(1,3))
boxplot(leaves$Valuation[leaves$numBlanks==0] ~ signif(leaves$CVratio[leaves$numBlanks==0], 2), ylim=c(-40,60), cex.axis=.75, xlab="Consonant to Vowel Ratio", ylab="Valuation", main="No Blanks on Rack")
boxplot(leaves$Valuation[leaves$numBlanks==1] ~ signif(leaves$CVratio[leaves$numBlanks==1], 2), ylim=c(-40,60), cex.axis=.75, xlab="Consonant to Vowel Ratio", ylab="Valuation", main="Valuation Distributions by Consonants, Vowels, and Blanks\nOne Blank on Rack")
boxplot(leaves$Valuation[leaves$numBlanks==2] ~ signif(leaves$CVratio[leaves$numBlanks==2], 2), ylim=c(-40,60), cex.axis=.75, xlab="Consonant to Vowel Ratio", ylab="Valuation", main="Two Blanks on Rack")

# Plot of medians with accurate x scale
plot(x=c(0,.2,.25,.33,.5,.67,1,1.5,2,3,4,5,10), y=tapply(leaves$Valuation[leaves$numBlanks==0], leaves$CVratio[leaves$numBlanks==0], median))
lines(x=c(0,.2,.25,.33,.5,.67,1,1.5,2,3,4,5,10), y=tapply(leaves$Valuation[leaves$numBlanks==0], leaves$CVratio[leaves$numBlanks==0], median))

# Plot of medians with accurate x scale and only using 6s
plot(x=c(0,.2,.5,1,2,5,15), y=tapply(leaves$Valuation[leaves$numBlanks==0 & leaves$racklen==6], leaves$CVratio[leaves$numBlanks==0 & leaves$racklen==6], median))
lines(x=c(0,.2,.5,1,2,5,15), y=tapply(leaves$Valuation[leaves$numBlanks==0 & leaves$racklen==6], leaves$CVratio[leaves$numBlanks==0 & leaves$racklen==6], median))


# Valuation by Vowels and Consonants
par(mfrow=c(3,1))
boxplot(leaves$Valuation[leaves$numBlanks==0] ~ leaves$numVowels[leaves$numBlanks==0] + leaves$numConsos[leaves$numBlanks==0], cex.axis=.5)
boxplot(leaves$Valuation[leaves$numBlanks==1] ~ leaves$numVowels[leaves$numBlanks==1] + leaves$numConsos[leaves$numBlanks==1], cex.axis=.75)
boxplot(leaves$Valuation[leaves$numBlanks==2] ~ leaves$numVowels[leaves$numBlanks==2] + leaves$numConsos[leaves$numBlanks==2], cex.axis=.75)

# Valuation by Consonants and Vowels
par(mfrow=c(3,1))
boxplot(leaves$Valuation[leaves$numBlanks==0] ~ leaves$numConsos[leaves$numBlanks==0] + leaves$numVowels[leaves$numBlanks==0], cex.axis=.5)
boxplot(leaves$Valuation[leaves$numBlanks==1] ~ leaves$numConsos[leaves$numBlanks==1] + leaves$numVowels[leaves$numBlanks==1], cex.axis=.75)
boxplot(leaves$Valuation[leaves$numBlanks==2] ~ leaves$numConsos[leaves$numBlanks==2] + leaves$numVowels[leaves$numBlanks==2], cex.axis=.75)





















