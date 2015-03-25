# ConsoVowel-Analysis
Playing Scrabble, the leave plays a big part in determining the best move. I rummage through all 900,000+ leaves looking at the importance of the balance of consonants and vowels.

By Simulation: I'm able to specify the pool size (where By Exactness assumes, for now, a draw from a full pool), as well as being able to specify CV ratio. The results from a simulation I ran can be seen in By Simulation/results20150131.Rda. By Simulation uses 6 letter leaves only, for now.

By Exactness: I look at the exact values of all leaves between 1 and 6 letters. By Exactness looks at racks with blanks (0, 1, or 2), and finds the distributions of the leave values. Assumes a full pool.

The full superleaves file can be found at http://quackle.cvs.sourceforge.net/viewvc/quackle/quackle/data/strategy/twl06/superleaves?revision=1.1

The six letter superleave file can be found at http://www.cross-tables.com/misc/leave6.txt