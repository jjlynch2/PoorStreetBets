require(feedeR)
feed_count <- 100 #Number of reports per update to check
tt <- 300 #Seconds between checking
form <- c("8-K","10-Q","10-K") #Form names
string <- "reverse stock split" #String to match within report
RSS <- "https://www.sec.gov/cgi-bin/browse-edgar?action=getcurrent"
sec_data <- vector(mode = "list", length = length(form))
sec_results <- c()

sec <- function(sec_data_new, sec_data, string, f) {
	sec_results <- c()
	for(i in 1:length(sec_data_new[[4]][3][[1]])) {
		if(all(sec_data_new[[4]][3][[1]][i] != sec_data[[f]][[4]][3][[1]])) {
			x <- read.csv(url(sec_data_new[[4]][3][[1]][i]))
			if(length(grep(string,x)) > 0) {
				sec_results <- c(sec_results, sec_data_new[[4]][1][[1]][i])
			}
		}
	}
	return(sec_results)
}

repeat {
	for(f in 1:length(form)) {
		sec_data_new <- feed.extract(paste(RSS,"&type=", form[f], "&count=", feed_count, "&output=atom", sep=""))
		if(!identical(sec_data_new[[4]][1][[1]], sec_data[[f]][[4]][1][[1]])) {
			print(paste("Poor Street Bets: Updating results for form: ", form[f], sep=""))
			new_data <- sec(sec_data_new, sec_data, string, f)
			if(is.null(new_data)) {
				print("No new matches... :(")
			} else {
				sec_results <- unique(c(sec_results, new_data))
				print(sec_results)
			}
			sec_data[[f]] <- sec_data_new
		}
	}
	if(is.null(sec_results)) {
		print("No coffee money for you!")
	}
	print(paste("Updating in ", tt, "seconds...", sep=""))
	Sys.sleep(tt)
}
