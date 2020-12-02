require(feedeR)
feed_count <- 20 #Number of reports per update to check
tt <- 300 #Seconds between checking
form <- "8-K" #Form name
string <- "reverse stock split" #String to match within report
RSS <- "https://www.sec.gov/cgi-bin/browse-edgar?action=getcurrent"
sec_data <- feed.extract(paste(RSS,"&type=", form, "&count=", feed_count, "&output=atom", sep=""))
sec_results <- c()

sec <- function(sec_data, string) {
	sec_results <- c()
	for(i in 1:length(sec_data[[4]][3][[1]])) {
		x <- read.csv(url(sec_data[[4]][3][[1]][i]))
		if(length(grep(string,x)) > 0) {
			sec_results <- c(sec_results, sec_data[[4]][1][[1]][i])
		}
	}
	return(sec_results)
}

repeat {
	sec_data_new <- feed.extract(paste(RSS,"&type=", form, "&count=", feed_count, "&output=atom", sep=""))
	if(!identical(sec_data_new[[4]][1][[1]], sec_data[[4]][1][[1]])) {
		print("Poor Street Bets: Updating results...")
		new_data <- sec(sec_data, string)
		if(is.null(new_data)) {
			print("No new matches... :(")
		} else {
			sec_results <- unique(c(sec_results, new_data))
		}
	}
	if(is.null(sec_results)) {
		print("No coffee money for you!")
	} else {
		print(sec_results)
	}
	Sys.sleep(tt)
}
