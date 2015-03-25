# Calculate probabilities
dictionary = data.frame(numinpool = c(9, 2, 2, 4, 12, 2, 3, 2, 9, 1, 1, 4, 2,
						 	 6, 8, 2, 1, 6,  4, 6, 4, 2, 2, 1, 2, 1, 2),
						numinrack = rep(0, 27),
						row.names = c('a','b','c','d','e','f','g','h','i','j','k',
					    	'l','m','n','o','p','q','r','s','t','u','v',
					    	'w','x','y','z','?'))
					    	
dictString <- rep(rownames(dictionary), dictionary$numinpool)

createRandomDictionary <- function(poolsize=sample(1:100, 1)) {
	sample(dictString, poolsize, replace=FALSE)
}
randomDictionary <- as.data.frame(table(createRandomDictionary(poolsize=50)))

calcProb.choose <- function(rack) {
	lv.ch <- tolower(unlist(strsplit(rack, '')))
	for (i in lv.ch) {
		dictionary[i, "numinrack"] = dictionary[i, "numinrack"] + 1
	}
	dictionary$numerator <- choose(dictionary$numinpool, dictionary$numinrack)	
	numerator   = prod(dictionary$numerator)
	denominator = factorial(100)/factorial(100-length(lv.ch))
	prob        = numerator / denominator
	return (prob)
}

leaves$prob.choose <- sapply(leaves$Leave, calcProb.choose)

leaves$prob.scaled <- numeric(length = length(leaves$prob))
leaves$prob.scaled[leaves$racklen==1] <- leaves$prob.choose[leaves$racklen==1]/sum(leaves$prob.choose[leaves$racklen==1])
leaves$prob.scaled[leaves$racklen==2] <- leaves$prob.choose[leaves$racklen==2]/sum(leaves$prob.choose[leaves$racklen==2])
leaves$prob.scaled[leaves$racklen==3] <- leaves$prob.choose[leaves$racklen==3]/sum(leaves$prob.choose[leaves$racklen==3])
leaves$prob.scaled[leaves$racklen==4] <- leaves$prob.choose[leaves$racklen==4]/sum(leaves$prob.choose[leaves$racklen==4])
leaves$prob.scaled[leaves$racklen==5] <- leaves$prob.choose[leaves$racklen==5]/sum(leaves$prob.choose[leaves$racklen==5])
leaves$prob.scaled[leaves$racklen==6] <- leaves$prob.choose[leaves$racklen==6]/sum(leaves$prob.choose[leaves$racklen==6])


#########################################################################################################
#########################################################################################################
pdf(file="~/Documents/Scrabble/Studies/Vowel Consonant Ratio Performance/By Exactness/ValuationByCVDiff.pdf", width=12)

par(mfrow=c(3,3))
##### 6
racklength = 6
textsize=1
color="red"
distFromMedLine = 1
boxplotoutput <-
wtd.boxplot(leaves$Valuation[leaves$numBlanks==0 & leaves$racklen==racklength] ~ 
	        leaves$CminusV[leaves$numBlanks==0 & leaves$racklen==racklength], 
		    weights=leaves$prob.scaled[leaves$numBlanks==0 & leaves$racklen==racklength], 
		  	ylim=c(-40,60), cex.axis=.75, xlab="Consonant - Vowel", ylab="Valuation (Median in red)", main="No Blanks on Rack")
text(1:ncol(boxplotoutput$stats), boxplotoutput$stats[3,]+distFromMedLine, 
	       boxplotoutput$stats[3,], cex=textsize, col=color)
	       
boxplotoutput <-
wtd.boxplot(leaves$Valuation[leaves$numBlanks==1 & leaves$racklen==racklength] ~ 
	        leaves$CminusV[leaves$numBlanks==1 & leaves$racklen==racklength], 
		    weights=leaves$prob.scaled[leaves$numBlanks==1 & leaves$racklen==racklength], 
		  	ylim=c(-40,60), cex.axis=.75, xlab="Consonant - Vowel", ylab="Valuation (Median in red)", main="One Blank on Rack")
text(1:ncol(boxplotoutput$stats), boxplotoutput$stats[3,]+distFromMedLine, 
	       boxplotoutput$stats[3,], cex=textsize, col=color)

boxplotoutput <- 
wtd.boxplot(leaves$Valuation[leaves$numBlanks==2 & leaves$racklen==racklength] ~ 
	        leaves$CminusV[leaves$numBlanks==2 & leaves$racklen==racklength], 
		    weights=leaves$prob.scaled[leaves$numBlanks==2 & leaves$racklen==racklength], 
		  	ylim=c(-40,60), cex.axis=.75, xlab="Consonant - Vowel", ylab="Valuation (Median in red)", main="Two Blanks on Rack")
text(1:ncol(boxplotoutput$stats), boxplotoutput$stats[3,]+distFromMedLine, 
	       boxplotoutput$stats[3,], cex= textsize, col=color)

##### 5
pdf(file="~/Documents/Scrabble/Studies/Vowel Consonant Ratio Performance/By Exactness/ValuationByCVDiff_5_4.pdf", width=12)
par(mfrow=c(2,3))
racklength = 5
textsize=.7
color="red"
distFromMedLine = 2
boxplotoutput <-
wtd.boxplot(leaves$Valuation[leaves$numBlanks==0 & leaves$racklen==racklength] ~ 
	        leaves$CminusV[leaves$numBlanks==0 & leaves$racklen==racklength], 
		    weights=leaves$prob.scaled[leaves$numBlanks==0 & leaves$racklen==racklength], 
		  	ylim=c(-40,60), cex.axis=.75, xlab="Consonant - Vowel", ylab="Valuation (Median in red)", main="No Blanks on Rack")
text(1:ncol(boxplotoutput$stats), boxplotoutput$stats[3,]+distFromMedLine, 
	       boxplotoutput$stats[3,], cex=textsize, col=color)
	       
boxplotoutput <-
wtd.boxplot(leaves$Valuation[leaves$numBlanks==1 & leaves$racklen==racklength] ~ 
	        leaves$CminusV[leaves$numBlanks==1 & leaves$racklen==racklength], 
		    weights=leaves$prob.scaled[leaves$numBlanks==1 & leaves$racklen==racklength], 
		  	ylim=c(-40,60), cex.axis=.75, xlab="Consonant - Vowel", ylab="Valuation (Median in red)", main="One Blank on Rack")
text(1:ncol(boxplotoutput$stats), boxplotoutput$stats[3,]+distFromMedLine, 
	       boxplotoutput$stats[3,], cex=textsize, col=color)

boxplotoutput <- 
wtd.boxplot(leaves$Valuation[leaves$numBlanks==2 & leaves$racklen==racklength] ~ 
	        leaves$CminusV[leaves$numBlanks==2 & leaves$racklen==racklength], 
		    weights=leaves$prob.scaled[leaves$numBlanks==2 & leaves$racklen==racklength], 
		  	ylim=c(-40,60), cex.axis=.75, xlab="Consonant - Vowel", ylab="Valuation (Median in red)", main="Two Blanks on Rack")
text(1:ncol(boxplotoutput$stats), boxplotoutput$stats[3,]+distFromMedLine, 
	       boxplotoutput$stats[3,], cex= textsize, col=color)
	       	       
##### 4
#par(mfrow=c(1,3))
racklength = 4
boxplotoutput <-
wtd.boxplot(leaves$Valuation[leaves$numBlanks==0 & leaves$racklen==racklength] ~ 
	        leaves$CminusV[leaves$numBlanks==0 & leaves$racklen==racklength], 
		    weights=leaves$prob.scaled[leaves$numBlanks==0 & leaves$racklen==racklength], 
		  	ylim=c(-40,60), cex.axis=.75, xlab="Consonant - Vowel", ylab="Valuation (Median in red)", main="No Blanks on Rack")
text(1:ncol(boxplotoutput$stats), boxplotoutput$stats[3,]+distFromMedLine, 
	       boxplotoutput$stats[3,], cex=textsize, col=color)
	       
boxplotoutput <-
wtd.boxplot(leaves$Valuation[leaves$numBlanks==1 & leaves$racklen==racklength] ~ 
	        leaves$CminusV[leaves$numBlanks==1 & leaves$racklen==racklength], 
		    weights=leaves$prob.scaled[leaves$numBlanks==1 & leaves$racklen==racklength], 
		  	ylim=c(-40,60), cex.axis=.75, xlab="Consonant - Vowel", ylab="Valuation (Median in red)", main="One Blank on Rack")
text(1:ncol(boxplotoutput$stats), boxplotoutput$stats[3,]+distFromMedLine, 
	       boxplotoutput$stats[3,], cex=textsize, col=color)

boxplotoutput <- 
wtd.boxplot(leaves$Valuation[leaves$numBlanks==2 & leaves$racklen==racklength] ~ 
	        leaves$CminusV[leaves$numBlanks==2 & leaves$racklen==racklength], 
		    weights=leaves$prob.scaled[leaves$numBlanks==2 & leaves$racklen==racklength], 
		  	ylim=c(-40,60), cex.axis=.75, xlab="Consonant - Vowel", ylab="Valuation (Median in red)", main="Two Blanks on Rack")
text(1:ncol(boxplotoutput$stats), boxplotoutput$stats[3,]+distFromMedLine, 
	       boxplotoutput$stats[3,], cex= textsize, col=color)
	   
dev.off()
?pdf

#########################################################################################################
#########################################################################################################

par(mfrow=c(1,6))
wtd.boxplot(leaves$Valuation[leaves$numBlanks==0 & leaves$racklen==6] ~ 
	      leaves$CminusV[leaves$numBlanks==0 & leaves$racklen==6], 
		weights=leaves$prob.scaled[leaves$numBlanks==0 & leaves$racklen==6], 
		ylim=c(-40,60), cex.axis=.75, xlab="Consonant - Vowel", ylab="Valuation", main="No Blanks on Rack")

wtd.boxplot(leaves$Valuation[leaves$numBlanks==0 & leaves$racklen==5] ~ 
	      leaves$CminusV[leaves$numBlanks==0 & leaves$racklen==5], 
		weights=leaves$prob.scaled[leaves$numBlanks==0 & leaves$racklen==5], 
		ylim=c(-40,60), cex.axis=.75, xlab="Consonant - Vowel", ylab="Valuation", main="No Blanks on Rack")

wtd.boxplot(leaves$Valuation[leaves$numBlanks==0 & leaves$racklen==4] ~ 
	      leaves$CminusV[leaves$numBlanks==0 & leaves$racklen==4], 
		weights=leaves$prob.scaled[leaves$numBlanks==0 & leaves$racklen==4], 
		ylim=c(-40,60), cex.axis=.75, xlab="Consonant - Vowel", ylab="Valuation", main="No Blanks on Rack")

wtd.boxplot(leaves$Valuation[leaves$numBlanks==0 & leaves$racklen==3] ~ 
	      leaves$CminusV[leaves$numBlanks==0 & leaves$racklen==3], 
		weights=leaves$prob.scaled[leaves$numBlanks==0 & leaves$racklen==3], 
		ylim=c(-40,60), cex.axis=.75, xlab="Consonant - Vowel", ylab="Valuation", main="No Blanks on Rack")

wtd.boxplot(leaves$Valuation[leaves$numBlanks==0 & leaves$racklen==2] ~ 
	      leaves$CminusV[leaves$numBlanks==0 & leaves$racklen==2], 
		weights=leaves$prob.scaled[leaves$numBlanks==0 & leaves$racklen==2], 
		ylim=c(-40,60), cex.axis=.75, xlab="Consonant - Vowel", ylab="Valuation", main="No Blanks on Rack")

wtd.boxplot(leaves$Valuation[leaves$numBlanks==0 & leaves$racklen==1] ~ 
	      leaves$CminusV[leaves$numBlanks==0 & leaves$racklen==1], 
		weights=leaves$prob.scaled[leaves$numBlanks==0 & leaves$racklen==1], 
		ylim=c(-40,60), cex.axis=.75, xlab="Consonant - Vowel", ylab="Valuation", main="No Blanks on Rack")

# 0 blanks and 6 tiles
boxplotoutput <- 
wtd.boxplot(leaves$Valuation[leaves$numBlanks==0] ~ 
	        leaves$CminusV[leaves$numBlanks==0], 
		    weights=leaves$prob.scaled[leaves$numBlanks==0], 
		  	ylim=c(-40,60), cex.axis=.75, xlab="Consonant - Vowel", ylab="Valuation (Median in red)", main="Two Blanks on Rack")
text(1:ncol(boxplotoutput$stats), boxplotoutput$stats[3,]+distFromMedLine, 
	       boxplotoutput$stats[3,], cex= textsize, col=color)

# calculate valuation distribution given letters definitely on rack
givenleave <- "TV"
grepifiable <- paste(unlist(strsplit(givenleave, '')), collapse=".*")
temp <- grep(grepifiable, leaves$Leave[leaves$racklen==6])
leavesGivenLeave <- leaves[leaves$racklen==6,][temp,]
boxplotoutput <- wtd.boxplot(leavesGivenLeave$Valuation, weights=leavesGivenLeave$prob.scaled) # weights are incorrect since the probabilities are calculated NOT given the givenleave so TVV shouldn't be SO unlikely since we're given a V already...
text(1:ncol(boxplotoutput$stats), boxplotoutput$stats[3,]+distFromMedLine, 
	       boxplotoutput$stats[3,], cex= textsize, col=color)
