##CHORD DATA PROCESSING

##Specify packages
	library(sqldf)

##Load in data
	setwd("C:/Users/jcchen2/Desktop/cfpb_chord")
	data <- read.csv("Consumer_Complaints.csv")

#Checks
	colnames(data)

#Process Issue to bank data
	subset_data <- sqldf("SELECT Issue as issue, count(Issue) count
				   FROM data
				   GROUP BY Issue")
	subset_data <- subset_data[order(-subset_data$count),]
	subset_data <- subset_data[1:20,]
	

	chord_data <- sqldf("SELECT Issue as issue, Company as company, count(Issue) count
				   FROM data
				   GROUP BY issue, company")
	
	over50 <- merge(chord_data,subset_data,by="issue")
	over50 <- over50[,1:3]
	colnames(over50) <- c("issue","company","count")

##process into Array

	#ISSUE LIST
		issue_sum <-sqldf("SELECT issue, SUM(count) count
				 FROM over50
				 GROUP BY issue")
		issue_sum <- issue_sum[order(-issue_sum$count),]
		issue <- unique(issue_sum[,1])

	#Company List
		issue_sum <-sqldf("SELECT company, SUM(count) count
				 FROM over50
				 GROUP BY company")

		issue_sum <- issue_sum[order(-issue_sum$count),]
		company <- unique(issue_sum[1:30,1])


	#First row
		value <- NA
		for(i in 1:length(issue)){
			if(i == 1){
					value <- paste(paste("\"Disposition\"", ",\"",issue[i],"\"",sep=""),sep="")				
				} else{
					value <- paste(value,",",paste("\"",issue[i],"\"",sep=""),sep="")
				}
		}
		value <- paste("[",value,"]",sep="")

	#Each row
		temp <- NA
		for(i in 1:length(company)){
			temp <- paste("\"",company[i],"\"",sep="")
			for(k in 1:length(issue)){
				a <- over50[ which(over50$company==company[i] & over50$issue == issue[k]),3]
				print(a)
				if(length(a)==0){
					temp <- paste(temp,",",paste(0,sep=""),sep="")
				} else {
					temp <- paste(temp,",",paste(as.numeric(a),sep=""),sep="")					
				}
				
			}
			temp <- paste("[",temp,"],",sep="")
			value <- paste(value,",",temp,sep="")
		
		}
	
		fileConn<-file("output.txt")
		writeLines(c(value),fileConn)
		close(fileConn)
		
