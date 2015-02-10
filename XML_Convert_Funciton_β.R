install.packages("XML") ##last update (2013/12/29 2:50)

###system.time is    
#user 	   system   elapsed 
#173.133   1.408    173.813  test1
#160.490   1.108    161.218  test2

library(XML)
getwd()
setwd("")
###input file_name, home team name and away team name with double quotation ("")
xml_csv_convertβ <- function(file_name, hometeam_name, awayteam_name) {
	a <- xmlParse(file_name) #Inport XML file
	match_name <- paste(hometeam_name, awayteam_name, sep = "_")　#make name of main folder
	dir.create(match_name) #make main folder
	nowdir <- getwd() #present work directory
	matchnamedir <- paste(nowdir, match_name, sep = "/")
	setwd(matchnamedir) #make path of new directory
####prepare data from certain Paths
    MATCHSHEET_tTEAM <- getNodeSet(a, "//tTEAM")
    EVENTS_PERIOD <- getNodeSet(a, "//PERIOD")
    TRAJECTORIES_PERIOD <- getNodeSet(a, "//TRAJECTORIES//PERIOD")
    TRAJECTORIES_PERIOD_PLAYER <- getNodeSet(a, "//TRAJECTORIES//PERIOD//PLAYER")
####MATCH_SHEET
########MATCHSHEET_tTEAM
    MATCHSHEET_tTEAM1 <- MATCHSHEET_tTEAM[[1]] #home team
    MATCHSHEET_tTEAM2 <- MATCHSHEET_tTEAM[[2]] #away team
########MATCHSHEET_tTEAM_PLAYER
    MATCHSHEET_tTEAM_PLAYER1 <- getNodeSet(MATCHSHEET_tTEAM1, "PLAYER") #home team players information
    MATCHSHEET_tTEAM_PLAYER2 <- getNodeSet(MATCHSHEET_tTEAM2, "PLAYER") #away team players information
########Function to make data frame of team information
    mkplayerinfo <- function(p) {
    	p_Id <- sapply(p, function(x) xmlGetAttr(x, "Id")) 
        NumAmisco <- sapply(p, function(x) xmlGetAttr(x, "NumAmisco")) #Camera info(maybe...)
        RuleId <- sapply(p, function(x) xmlGetAttr(x, "RuleId")) #I don't know what is this...
        SecondName <- sapply(p, function(x) xmlGetAttr(x, "SecondName"))
        FirstName <- sapply(p, function(x) xmlGetAttr(x, "FirstName"))
        InputMode <- sapply(p, function(x) xmlGetAttr(x, "InputMode")) # I don't know what is this...
        ShirtNumber <- sapply(p, function(x) xmlGetAttr(x, "ShirtNumber"))
        Poste <- sapply(p, function(x) xmlGetAttr(x, "Poste"))
        ShoeColor <- sapply(p, function(x) xmlGetAttr(x, "ShoeColor"))
        matchSheet_tTeam_player_df <- cbind(p_Id, NumAmisco, RuleId, SecondName, FirstName, InputMode, ShirtNumber, Poste, ShoeColor) #making data frame of home_team_player's info
        return(matchSheet_tTeam_player_df)
        }
########make data set of Player Information of Home team
    matchSheet_tTeam_player_df1 <- mkplayerinfo(MATCHSHEET_tTEAM_PLAYER1)
####Save it!!
    dir.create("PlayerInformation") #create directory
	playerinfodir <- paste(matchnamedir, "PlayerInformation", sep = "/") #get path of new dir 
	setwd(playerinfodir) #set new dir's path
	teaminfodir_home <- paste("PlayerInformation", hometeam_name, sep = "_") 
	dir.create(teaminfodir_home)
	teaminfodir_homepath <- paste(playerinfodir, teaminfodir_home, sep = "/")
	setwd(teaminfodir_homepath)
	mkfilename_home <- paste("PlayerInfo", hometeam_name, ".csv", sep = "")
    write.csv(matchSheet_tTeam_player_df1, mkfilename_home, eol = "\r") 
########make data frame of Player Information of Away team
    setwd(playerinfodir)
    matchSheet_tTeam_player_df2 <- mkplayerinfo(MATCHSHEET_tTEAM_PLAYER2)
####Save it!!
	teaminfodir_away <- paste("PlayerInformation", awayteam_name, sep = "_")
	dir.create(teaminfodir_away)
	teaminfodir_awaypath <- paste(playerinfodir, teaminfodir_away, sep = "/")
	setwd(teaminfodir_awaypath)
	mkfilename_away <- paste("PlayerInfo", awayteam_name, ".csv", sep = "")
    write.csv(matchSheet_tTeam_player_df2, mkfilename_away, eol = "\r") 
####EVENTS
    setwd(matchnamedir) #return to main dir
    dir.create("EventInformation") 
########EVENTS_PERIOD
    EVENTS_PERIOD1 <- EVENTS_PERIOD[[1]] #devide data to make ball_info of 1st period
    EVENTS_PERIOD2 <- EVENTS_PERIOD[[2]] #devide data to make ball_info of 2nd period
########EVENTS_PERIOD_EVENT
    EVENTS_PERIOD_EVENT1 <- getNodeSet(EVENTS_PERIOD1, "EVENT") #get ball_info of 1st period
    EVENTS_PERIOD_EVENT2 <- getNodeSet(EVENTS_PERIOD2, "EVENT") #get ball info of 2nd period
########Function to make data frame of event information
    mkeventinfo <- function(e){
    	e_Id <- sapply(e, function(x) xmlGetAttr(x, "Id"))
    	e_Time <- sapply(e, function(x) xmlGetAttr(x, "Time"))
    	e_PositionX <- sapply(e, function(x) xmlGetAttr(x, "PositionX"))
    	e_PositionY <- sapply(e, function(x) xmlGetAttr(x, "PositionY")) #? www
    	e_MatchEventCode <- sapply(e, function(x) xmlGetAttr(x, "MatchEventCode")) #? www
    	e_BallEventCode <- sapply(e, function(x) xmlGetAttr(x, "BallEventCode")) #? www
    	e_WithWhat <- sapply(e, function(x) xmlGetAttr(x, "WithWhat")) # which side of foot ball on
    	e_How <- sapply(e, function(x) xmlGetAttr(x, "How")) #? www
    	e_Result <- sapply(e, function(x) xmlGetAttr(x, "Result"))
    	e_NumAmiscoJ1 <- sapply(e, function(x) xmlGetAttr(x, "NumAmiscoJ1"))
    	events_period_event_df <- cbind(e_Id, e_Time, e_PositionX, e_PositionY, e_MatchEventCode, e_BallEventCode, e_WithWhat, e_How, e_Result, e_NumAmiscoJ1) #make data frame of ball_info
    	return(events_period_event_df)
    }
####make data frame of 1st half event information
    events_period_event_df1 <- mkeventinfo(EVENTS_PERIOD_EVENT1)
####Save it!!
	eventinfodir <- paste(matchnamedir, "EventInformation", sep = "/")
	setwd(eventinfodir) 
	dir.create("FirstHalf")
	eventinfodir1 <- paste(eventinfodir, "FirstHalf", sep = "/")
	setwd(eventinfodir1)
    write.csv(events_period_event_df1, "EventInfo1stHalf.csv", eol = "\r")
####make data frame of 2nd half event information
    setwd(eventinfodir)
    events_period_event_df2 <- mkeventinfo(EVENTS_PERIOD_EVENT2)
####Save it!!
    dir.create("SecondHalf")
	eventinfodir2 <- paste(eventinfodir, "SecondHalf", sep = "/")
	setwd(eventinfodir2)
    write.csv(events_period_event_df2, "EventInfo2ndHalf.csv", eol = "\r")

####TRAJECTORIES
########TRAJECTORIES_PERIOD
    setwd(matchnamedir)
    dir.create("PositionInformation")
    posinfodir <- paste(matchnamedir, "PositionInformation", sep = "/")
    setwd(posinfodir)
    TRAJECTORIES_PERIOD1 <- TRAJECTORIES_PERIOD[[1]] 
    TRAJECTORIES_PERIOD2 <- TRAJECTORIES_PERIOD[[2]]
########TRAJECTORIES_PERIOD_PLAYER
    TRAJECTORIES_PERIOD_PLAYER1 <- getNodeSet(TRAJECTORIES_PERIOD1, "PLAYER") #player's track of 1st period
    TRAJECTORIES_PERIOD_PLAYER2 <- getNodeSet(TRAJECTORIES_PERIOD2, "PLAYER") #player's track of 2nd period
################## Functions ################
###function to get the NumAmisco nomber
getna <- function(x){
	per <- getNodeSet(x, "//TRAJECTORIES//PERIOD")
	per1 <- per[[1]]
	per2 <- per[[2]]
	traplay1 <- getNodeSet(per1, "PLAYER")
	traplay2 <- getNodeSet(per2, "PLAYER")
	traplayeach1 <<- sapply(traplay1, function(x) xmlGetAttr(x, "NumAmisco"))
	traplayeach2 <<- sapply(traplay2, function(x) xmlGetAttr(x, "NumAmisco"))
}
getna(a)
########Preparing function for Position data
    posfunc <- function(x){ 
    	for(i in 1 : length(x)){
    		repos <- x[[i]]
    		x[[i]] <- getNodeSet(repos, "Pos")
    		} 
    		return(x)
    	} #XMLdata→Listdata
######## function to make data frame of player's track data
    posfunc2 <- function(x){
    	for (i in 1 : length(x)){
    		repos <- x[[i]]
    		p_Times <- sapply(repos, function(y) xmlGetAttr(y, "Times"))
    		p_X <- sapply(repos, function(y) xmlGetAttr(y, "X"))
    		p_Y <- sapply(repos, function(y) xmlGetAttr(y, "Y"))
    		x[[i]] <- cbind(p_Times, p_X, p_Y)
    		}
    		return(x)
    	}
########SaveFunction
    sfunc <- function(x, n, y) {
    	for (i in 1 : length(x)){
    		sv <- x[i]
    		fname <- as.character(paste("Pos", n, "NAis", y[i], ".csv", sep = ""))
    		write.csv(sv, fname, eol = "\r")
    		}
    	} #auto save function!!!
########TRAJECTORIES_PERIOD_PLAYER_Pos
##Position information of 1st Period
    TRAJECTORIES_PERIOD_PLAYER_Pos1 <- posfunc(TRAJECTORIES_PERIOD_PLAYER1) #XML→List

##Position information of 2nd Period
    TRAJECTORIES_PERIOD_PLAYER_Pos2 <- posfunc(TRAJECTORIES_PERIOD_PLAYER2)
########Make data set of Position of 1st period
    TRAJECTORIES_PERIOD_PLAYER__Pos1 <- posfunc2(TRAJECTORIES_PERIOD_PLAYER_Pos1) #List→data frame
    dir.create("FirstHalf")
    posinfodir1 <- paste(posinfodir, "FirstHalf", sep = "/")
    setwd(posinfodir1)
###Save it!!!
    sfunc(TRAJECTORIES_PERIOD_PLAYER__Pos1, 1, traplayeach1)
########Make data set of Position of 2nd period
    setwd(posinfodir)
    TRAJECTORIES_PERIOD_PLAYER__Pos2 <- posfunc2(TRAJECTORIES_PERIOD_PLAYER_Pos2)
###Save it !!!
    dir.create("SecondHalf")
    posinfodir2 <- paste(posinfodir, "SecondHalf", sep = "/")
    setwd(posinfodir2)
    sfunc(TRAJECTORIES_PERIOD_PLAYER__Pos2, 2, traplayeach2)
    }
######TEST
system.time(xml_csv_convertβ("espag_urugu.xml", "Espag", "Urugu"))
