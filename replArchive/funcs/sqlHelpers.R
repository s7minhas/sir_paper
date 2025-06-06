# Dependencies
library(RMySQL)

# Other functions
mysqlSetup = function(user=NULL, pw=NULL, db=NULL, host=NULL) {
	tryCatch(conn <<- dbConnect(MySQL(), user=user, password=pw, 
		dbname=db, host=host), 
	error=function(e) warning("MySQL connection does not work") )
}