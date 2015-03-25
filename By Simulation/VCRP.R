# Scrabble rack simulation
	# 1.	Specify vowel to consonant ratio
	# 2.	Specify pool size
	# 3.	Randomly select a rack
	# 4.	Evaluate the rack, statically
	# 5.	Repeat 3-4, B times
	
# Assumptions / Concerns
	# 1. 'Y' is treated as a consonant
	# 2. Based on 6-tile racks
	# 3. Doesn't adjust to board positions (closed board, open board with many consos, open board with many vowels, endgame, score, etc.)
	# 4. Superleaves may be based on a full pool. I'm using the valuation of superleaves which may differ from the sampled pool.
	# 5. Pool sizes and vcrs chosen arbitrarily; maybe use a GP here?
	
# Notes
	# 1. Start of the game VCR is .75
	
# Questions
	# 1. How would results change if 'Y' was treated as a vowel?
	# 2. How do results change with # of blanks in possible pool?
	# 3. How does this differ from looking only at the superleaves: 0:6, 1:5, 2:4, ..., 5:1, 6:0
	# 4. How do results change with the size of the pool?
fix()
superleaves6 <- read.table("~/Documents/Scrabble/Studies/Vowel Consonant Ratio Performance/superleaves_6.txt", header = FALSE, sep = " ", col.names = c("Leave", "Valuation"), colClasses = c("character", "numeric"))

fullvowelpool <- c(rep('A', 9),
				   rep('E', 12),
				   rep('I', 9),
				   rep('O', 8),
				   rep('U', 4))
				   
fullconsopool <- c(rep('B', 2),
			 	   rep('C', 2),
			       rep('D', 4),
				   rep('F', 2),
				   rep('G', 3),
				   rep('H', 2),
				   rep('J', 1),
				   rep('K', 1),
				   rep('L', 4),
				   rep('M', 2),
				   rep('N', 6),
				   rep('P', 2),
				   rep('Q', 1),
				   rep('R', 6),
				   rep('S', 4),
				   rep('T', 6),
				   rep('V', 2),
				   rep('W', 2),
				   rep('X', 1),
				   rep('Y', 2),
				   rep('Z', 1))
				   

poolsize <- c(10, 15, 20, 25, 30, 35, 40, 45, 50)
vcr <- c(.2, .25, .3333, .5, .75, .8, 1, 1.25, 1.5, 2, 3, 4)
combos <- expand.grid(poolsize, vcr)

before <- Sys.time()
test <- sapply(1:nrow(combos), function(z) {
	poolsize <- combos[z, 1]
	vcr <- combos[z, 2]
	numblanks <- 0 # c(0,1,2)
	numc=round((poolsize-numblanks)/(1+vcr))
	numv=round((poolsize-numblanks)-numc)
	cat(numc, "\n")
	cat(numv, "\n")
	simsam <- unlist(sapply(1:500, function(y) {
		consosample <- sample(fullvowelpool, numv, replace = FALSE)
		vowelsample <- sample(fullconsopool, numc, replace = FALSE)
		blanks <- rep('?', numblanks)
		currentpool <- c(blanks, consosample, vowelsample)
		manyracks <- sapply(1:35, function(x) {
			paste(sort(sample(currentpool, 6, replace = FALSE)), collapse = "")
		})
		rackvals <- superleaves6[which(superleaves6$Leave %in% manyracks), "Valuation"]
		#distrox <- density(rackvals)$x
		#distroy <- density(rackvals)$y
		#c(distrox, distroy)
		mean(rackvals)
	}))
	mean(simsam)
})
after <- Sys.time()
totaltime <- after - before
results <- cbind(combos,test)

# results <- cbind(combos,test)
vec <- results[,3]
a <- .5
b <- 5
mx <- max(vec)
mn <- min(vec)
newvec <- (((b-a)*(vec-mn)) / (mx-mn)) + a
plot(results[,1], results[,2], cex=newvec, xlab="Poolsize", ylab="VCR")

plot(results[,1], results[,2], cex=newvec, xlab="Poolsize", ylab="VCR")
cbind(combos,newvec)

save(results, file = "~/Documents/Scrabble/Studies/Vowel Consonant Ratio Performance/results20150131.Rda")

# seconds for each thing
18.03877*130/60 # 39.084 minutes = 10:52
