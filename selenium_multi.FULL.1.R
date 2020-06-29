#################################################
# Google Translator Remote Server Retrival Tool #
#				 Version 12.0					#
#   Copyright (c) Dr. Andrea Prunotto, 2020     #
#  Institute of Medical Biometry and Statistics #
#    Faculty of Medicine and Medical Center     #
#            University of Freiburg             #
#     Stefan-Meier-Str. 26, 79104 Freiburg      #
#################################################

# Here we build one file for each support language
# containing the original term, the translation in the support lang and the translation in german

#################################################
#       Fundamental operations outside R
#
# Activate docker
#
# sudo systemctl start docker
# sudo systemctl enable docker
#
# Activate the remote server
# sudo docker run -d -p 4444:4444 -v /dev/shm:/dev/shm selenium/standalone-firefox:3.141.59-20200409
# To view open containers
# sudo docker ps --filter name=
#
# To stop selected containers
# sudo docker stop 0492a4ce2671
#
# Better to reininitialize the remote server if R stops before the UKL correct logoff!
#
#################################################

# General R options & loading essential R libraries

max_working_seconds=7200# 1 hour


options(stringsAsFactors=FALSE)

library(stringr)
library(textreadr)
library(curl)
library(XML)
library(RSelenium)
library(rJava)
library(rvest)
library(httr)

# Specify extra parameters for the remote driver

fprof=makeFirefoxProfile(list(
	"browser.cache.disk.enable"=FALSE,
	"browser.cache.memory.enable"=FALSE,
	"browser.cache.offline.enable"=FALSE,
	"network.http.use-cache"=FALSE))

# Define the remote driver

remDr=remoteDriver(remoteServerAddr="localhost",port=4444, browserName="firefox",version="76.0",extraCapabilities=fprof);

# URL of UKL portal

uniurl=URLencode(paste("https://internetzugriff.ukl.uni-freiburg.de/PortalMain"))


# Define the original languages (for which the SNOMED description files are available)

origlang=c("es")
nol=length(origlang)

# Define the support languages, used to recover the de translation
tlang=unique(c("en","da")); 
nlang=length(tlang); 

#tlang=unique(c("es","en","fr","de","it","nl","pt"))
#tlang=unique(c("de","it","nl","pt"))
#tlang=unique(c("nl","pt"))
#tlang=unique(c("da","no","sv"))
#tlang=unique(c("da","no","sv"))
# fr to be restarted
#tlang=unique(c("fr")) #,"de","it","nl","pt"))
#nlang=length(tlang); 

#################################################
# Read input file list (SNOMED description files)
#################################################

writeLines("Loading SNOMED files...")

origfile=list()
orig=list()
for(ol in 1:nol)
{
	if(origlang[ol]=="en")
	{
		origfile[[which(origlang=="en")]]="./snomed_original/full/sct2_Description_Snapshot-en_INT_20200309.txt"; 
		orig[[which(origlang=="en")]]=read.table(origfile[[which(origlang=="en")]],sep="\t",header=TRUE,comment.char="",quote="")
	}
	if(origlang[ol]=="es")
	{
		origfile[[which(origlang=="es")]]="./snomed_original/full/sct2_Description_SpanishExtensionSnapshot-es_INT_20200430.txt"
		orig[[which(origlang=="es")]]=read.table(origfile[[which(origlang=="es")]],sep="\t",header=TRUE,comment.char="",quote="")
	}

	# Select only active concepts
	
	orig[[ol]]=orig[[ol]][orig[[ol]]$active==1,]

}
writeLines("Done.")

# First access to UKL
writeLines(" ")
writeLines("Access UKL: First Login...")
logoff=0
while(logoff==0)
{
	writeLines("Access UKL: Check for First correct login.")
	writeLines("Access UKL: Start the remote driver...");
	remDr$open(silent = TRUE); 
	f1=0;while(f1==0){remDr$navigate(uniurl);f1=length(remDr$findElements(using='css selector',"#UserCheck_Login_Button_span")); 
	writeLines(paste("Access UKL: Portal... ",f1,sep=""))}
	uniwebElem1=remDr$findElements("css selector", "#LoginUserPassword_auth_username");	uniwebElem1[[1]]$sendKeysToElement(list("---------"));
	writeLines("Access UKL: Username... "); 									
	uniwebElem2=remDr$findElements("css selector", "#LoginUserPassword_auth_password");	uniwebElem2[[1]]$sendKeysToElement(list("---------")); 
	writeLines("Access UKL: Password... ")
	f2=0;while(f2==0){f2=length(remDr$findElements(using = 'css selector', "#UserCheck_Login_Button_span"));
	writeLines(paste("Access UKL: Login button... ",f2,sep=""))};
	uniwebElem3=remDr$findElements(using = 'css selector', "#UserCheck_Login_Button")
	uniwebElem3[[1]]$clickElement(); 
	logoff=length(remDr$findElements(using='css selector',"#UserCheck_Logoff_Button_span"))
}
writeLines("Access UKL: Logged in.")
writeLines(" ")

# Initialize the working time

start_time=Sys.time()

########################
# Start the translations
########################


for(ol in 1:nol)
{ 
	# Start of the cycle on the original files 

	# Specify how many entries of the original file to read

	nlines=nrow(orig[[ol]]); starting_row=1 ; nr=nlines-starting_row+1

	# Defining the "blocks", i.e. the sets of entries  to submit to google 
	# The blocks must be such that we submit no more than 5000 chars per block

	maxchar=5000

	# First we substitute special characters "%" and "&" in the whole original terms
	# This may introduce some more character to the entries to submit to google
	# We restore their value at the very end

	orig[[ol]]$term=gsub("%","_Pc_",orig[[ol]]$term)
	orig[[ol]]$term=gsub("&","_E0_",orig[[ol]]$term)

	# Determining the number and the size of the blocks to avoid the 5000 char barrier
	# We add a space for each entry (+1) plus the checktag (max +10 chars)
	# Say we add 1 char each fragment (frag=frag+1), to be sure not to exceed maxchar
	# by adding the tag, we add 2, and there should be a lot of margin

	frag=nchar(orig[[ol]]$term)
	frag=frag+3 # to each entries we add \n 
	# 16.06.2020 I add 3 (instead of 2) to reduce the splitting frequency in the translation

	blo=list(); bl=list(); p=1; b=1;blo[[1]]=1

	while(p<=length(frag))
	{
		cs=10 # the length of the checktag
		while(cs<=maxchar & p<=length(frag))
		{
			cs=cs+as.numeric(frag[p])
			p=p+1
		}
		b=b+1
		blo[[b]]=p
		bl[[b]]=blo[[b]]-blo[[b-1]]+1
	}

	nb=length(blo)-1

	# nb is the number of blocks


	# Set the number of blocks after which refreshing the server 
	# this depends on the computer memory, default 20

	r=25

	# Initialization of essential variable for the translations:

	# orig -> supp lang

	lang_head=list()
	lang_gurl=list()
	lang_eurl=list()

	# supp lang -> de

	trad_head=list()
	trad_gurl=list()
	trad_eurl=list()

	for(tl in 1:nlang)
	{ # cycle of translations on the target languages: counter tl
		# We translate only if the support languages is not equal to the original one !
		if(tlang[[tl]]!=origlang[[ol]])
		{
			# We define the number of blocks to next refresh, useful to inspect efficiency
			bref=r
			for(i in 1:(nb))
			{ # cycle on the blocks of max 5000 char (nb blocks for each support language)

				# Define start-end lines of the orginal file within the current block

				sbl=blo[[i]]
				ebl=blo[[i+1]]-1

				# The following two if are related to restarting the 
				# UKL account and refreshing the memory of the remote driver

				######################################################
				# Restarting the UKL session after max_working_seconds
				######################################################

				# Retrive the working time

				working_seconds=difftime(Sys.time(),start_time,units="secs")

				if (working_seconds > max_working_seconds)
				{
					writeLines("Access UKL: Logging off."); 
					# To logoff, we first login .......
					logoff=0
					while(logoff==0)
					{
						writeLines("Access UKL: Close/restart the remote driver...");
						lrem=1;while(lrem>0){lrem=length(remDr$closeall())};remDr$open(silent = TRUE); 
						writeLines("Access UKL: Check for correct logout.")
						f1=0;while(f1==0){remDr$navigate(uniurl);f1=length(remDr$findElements(using='css selector',"#UserCheck_Login_Button_span")); 
						writeLines(paste("Access UKL: Portal... ",f1,sep=""))}
						uniwebElem1=remDr$findElements("css selector", "#LoginUserPassword_auth_username");	uniwebElem1[[1]]$sendKeysToElement(list("----------"));
						writeLines("Access UKL: Username... "); 									
						uniwebElem2=remDr$findElements("css selector", "#LoginUserPassword_auth_password");	uniwebElem2[[1]]$sendKeysToElement(list("----------")); 
						writeLines("Access UKL: Password... ")
						f2=0;while(f2==0){f2=length(remDr$findElements(using = 'css selector', "#UserCheck_Login_Button_span"));
						writeLines(paste("Access UKL: Logoff button... ",f2,sep=""))};
						uniwebElem3=remDr$findElements(using = 'css selector', "#UserCheck_Login_Button")
						uniwebElem3[[1]]$clickElement(); 
						logoff=length(remDr$findElements(using='css selector',"#UserCheck_Logoff_Button_span"))
					}
					uniwebElem4=remDr$findElements(using = 'css selector', "#UserCheck_Logoff_Button_span")
					uniwebElem4[[1]]$clickElement();
					writeLines("Access UKL: Logged off.") 
					writeLines(" ")
					writeLines("Access UKL: Logging in.");
					logoff=0
					while(logoff==0)
					{						
						writeLines("Access UKL: Close/restart the remote driver...");
						lrem=1;while(lrem>0){lrem=length(remDr$closeall())};remDr$open(silent = TRUE);
						writeLines("Access UKL: Check for correct login.")
						# Logging in again ...................
						f4=0;while(f4==0){remDr$navigate(uniurl);f4=length(remDr$findElements(using='css selector',"#UserCheck_Login_Button_span"));
						writeLines(paste("Access UKL: Portal... ",f4,sep=""))}
						writeLines("Access UKL: Username... ");
						uniwebElem5=remDr$findElements("css selector", "#LoginUserPassword_auth_username"); uniwebElem5[[1]]$sendKeysToElement(list("--------")); 									
						writeLines("Access UKL: Password... ")
						uniwebElem6=remDr$findElements("css selector", "#LoginUserPassword_auth_password"); uniwebElem6[[1]]$sendKeysToElement(list("---------")); 
						f5=0;while(f5==0){f5=length(remDr$findElements(using='css selector',"#UserCheck_Login_Button_span"));
						writeLines(paste("Access UKL: Login button... ",f5,sep=""))}
						uniwebElem7=remDr$findElements(using = 'css selector', "#UserCheck_Login_Button") ;	
						uniwebElem7[[1]]$clickElement();  
						logoff=length(remDr$findElements(using='css selector',"#UserCheck_Logoff_Button_span"))

					}
					writeLines("Access UKL: Logged in.") 
					writeLines(" ")
					start_time=Sys.time(); 
				}

				######################################################
				# Refrshing the remote driver every r blocks
				######################################################

				if (i%%r==0)
				{
					# close Google remDr
					writeLines("Memory refresh: Closing the Remote Driver...");
					lrem=1;while(lrem>0){lrem=length(remDr$closeall())}
					# restart remDr 
					writeLines("Memory refresh: Restarting the Remote Driver...")
					remDr$open(silent = TRUE);
					bref=r
					writeLines(" ")
				}

				######################################################
				# Building the current block of entries to submit
				######################################################

				# Define a numeric tag to add to the page, to be sure we're loading the correct page
				checktag1=sample(999999:100000000,1,replace=FALSE)
				# create the list of ebl-sbl entries 
				list_term_1=orig[[ol]]$term[sbl:ebl]
				# append the tag at the END of the list
				list_term_2=append(list_term_1,checktag1)
				# Form the core of the URL to be submitted to google
				list_term_3=str_c(list_term_2,collapse="\n")

				
				###########################################
				# Translate from en to lang 
				###########################################

				# We already took care that the list_term_2 we submit to google should not 
				# contain more than 5000 chars. However, to be sure not to exceed this limit
				# we split the list_term_1 in two pieces. In the unfortunate case of somn
				# exceed in fact the remote driver stops and the translation fails


				if(nchar(list_term_3)<maxchar)
				{
					# Generate Google Translate URL for the target lang
					# header
					lang_head[tl]=paste("https://translate.google.com/?hl=en#view=home&op=translate&sl=",origlang[ol],"&tl=",tlang[tl],"&text=",sep="")
					# header+core
					lang_gurl[tl]=paste(lang_head[[tl]],list_term_3,sep="")
					# encode the URL
					lang_eurl[tl]=URLencode(lang_gurl[[tl]])

					# Navigate the well-formed URL

					remDr$navigate(lang_eurl[[tl]])

					# Check if the page is correctly loaded
					# The page should contain the tag!
					# We do several attempts, because sometimes the tag creates problems

					att1=1; sflag1=FALSE
					while(sflag1==FALSE)
					{
						# Send a message on the screen with info about the current block retrival
						writeLines(paste("Translating ",ebl-sbl," lines [",sbl,":",ebl,"/",nrow(orig[[ol]]),"] [block ",i,"/",nb,", ",nchar(list_term_3)," chars] of ",origlang[ol]," original file from ",origlang[ol]," to ",tlang[tl],". Attempt: ",att1," ... ", sep=""));
						# Retrive the tag from the page
						check1=remDr$findElements(using='css selector',"body")[[1]]$getElementText()[[1]]
						# Is the tag in the page? (Or we are still reading the old one?)
						sflag1=!is.na(grep(checktag1, check1)[1])
						att1=att1+1
						if(att1>=10)
						{	
							print("Too many attempts, restarting the server and assigning a new checktag. ")
							# Stop the remote driver
							lrem=1;while(lrem>0){lrem=length(remDr$closeall())}
							# Reassignig the tag
							checktag1=sample(999999:100000000,1, replace=FALSE)
							# Append the tag to the list of entries
							list_term_2=append(list_term_1,checktag1)
							# Form the core of the URL
							list_term_3=str_c(list_term_2,collapse="\n")
							# header
							lang_head[tl]=paste("https://translate.google.com/?hl=en#view=home&op=translate&sl=",origlang[ol],"&tl=",tlang[tl],"&text=",sep="")
							# header+core
							lang_gurl[tl]=paste(lang_head[[tl]],list_term_3,sep="")
							# encode the URL
							lang_eurl[tl]=URLencode(lang_gurl[[tl]])
							# Restart the remote driver, and navigate
							remDr$open(silent = TRUE); remDr$navigate(lang_eurl[[tl]])
							att1=1
						}
					}
					sour1=remDr$getPageSource()
					page1=read_html(sour1[[1]])
					xml_find_all(page1, ".//br") %>% xml_add_sibling("p", " \n ")
					xml_find_all(page1, ".//br") %>% xml_remove()
					tlid1=html_nodes(page1,'.tlid-translation')
					trad_clean_0=html_text(tlid1)
					trad_clean_1=strsplit(trad_clean_0,split=" \n ")
					# restore the protected chars
					trad_clean_2=gsub("_Pc_","%",trad_clean_1[[1]])
					trad_clean_3=gsub("_E0_","&",trad_clean_2)
					# remove the server tag
					trad_clean_4=trad_clean_3[-length(trad_clean_3)]
					# Now trad_clean_4 contains the translations of list_term_2
				}
				else
				{
					writeLines("Original translation terms exceed 5000 char. Splitting the list.")
					ltr1=floor(length(list_term_1)/2)
					list_term_1_split=list()
					list_term_1_split[[1]]=list_term_1[1:ltr1]
					list_term_1_split[[2]]=list_term_1[(ltr1+1):length(list_term_1)]
					trad_clean_4_split=list() # this will contain the two halves of the entries

					for(tsplit in 1:2)
					{
						checktags=sample(999999:100000000,1, replace=FALSE)
						list_term_2=append(list_term_1_split[[tsplit]],checktags)
						list_term_3=str_c(list_term_2,collapse="\n")
						# header
					    lang_head[tl]=paste("https://translate.google.com/?hl=en#view=home&op=translate&sl=",origlang[ol],"&tl=",tlang[tl],"&text=",sep="")
						# header+core
						lang_gurl[tl]=paste(lang_head[[tl]],list_term_3,sep="")
						# encode the URL
						lang_eurl[tl]=URLencode(lang_gurl[[tl]])
						remDr$navigate(lang_eurl[[tl]])

						atts=1
						sflags=FALSE
						while(sflags==FALSE)
						{
							writeLines(paste("Translating ",ebl-sbl," lines [",sbl,":",ebl,"/",nrow(orig[[ol]])," [block ",i,"-",tsplit,"/",nb,", ",sum(nchar(list_term_1_split[[tsplit]]))," chars] of ",origlang[ol]," original file from ",tlang[tl]," to de. Attempt: ",atts," ... ", sep=""));
							checks=remDr$findElements(using='css selector',"body")[[1]]$getElementText()[[1]]
							sflags=!is.na(grep(checktags, checks)[1])
							atts=atts+1
							if(atts>=10)
							{	
								# stop    remDr
								print("Too many attempts, restarting the server!")
							    lrem=1;while(lrem>0){lrem=length(remDr$closeall())}
								# restart remDr
								checktags=sample(999999:100000000,1, replace=FALSE)
								list_term_2=append(list_term_1_split[[tsplit]],checktags)
								list_term_3=str_c(list_term_2,collapse="\n")
								lang_head[tl]=paste("https://translate.google.com/?hl=en#view=home&op=translate&sl=",origlang[ol],"&tl=",tlang[tl],"&text=",sep="")
								# header+core
								lang_gurl[tl]=paste(lang_head[[tl]],list_term_3,sep="")
								# encode the URL
								lang_eurl[tl]=URLencode(lang_gurl[[tl]])
								# Restart the remote driver, and navigate
								remDr$open(silent = TRUE); remDr$navigate(lang_eurl[[tl]])
								atts=1
							}
						}

						sour1=remDr$getPageSource()
						page1=read_html(sour1[[1]])
						xml_find_all(page1, ".//br") %>% xml_add_sibling("p", " \n ")
						xml_find_all(page1, ".//br") %>% xml_remove()
						tlid1=html_nodes(page1,'.tlid-translation')
						trad_clean_0=html_text(tlid1)
						trad_clean_1=strsplit(trad_clean_0,split=" \n ")
						# restore the protected chars
						trad_clean_2=gsub("_Pc_","%",trad_clean_1[[1]])
						trad_clean_3=gsub("_E0_","&",trad_clean_2)
						# remove the server tag
						trad_clean_4_split[[tsplit]]=trad_clean_3[-length(trad_clean_3)]


					} # end of the cycle on the splitted block

					trad_clean_4=c(trad_clean_4_split[[1]],trad_clean_4_split[[2]])
				}

				# At this stage we have loaded the correct page
				# Load it in sour, read it in page, recover translations in tlid



				
				##############################
				#  Translate from lang to de   
				##############################

				# Same as before

				checktag2=sample(999999:100000000,1, replace=FALSE)
				list_trad_0=paste(trad_clean_4,sep="")
				list_trad_1=gsub("%","_Pc_",list_trad_0)
				list_trad_2=gsub("&","_E0_",list_trad_1)
				list_trad_3=append(list_trad_2,checktag2)
				list_trad_4=str_c(list_trad_3,collapse="\n")
				list_trad_check=list_trad_2

				# It can easily happen that list_trad_4 exceeds the 5000 char barrier
				# In that case we split list_trad_0 in 2, then we recompose it

				if(nchar(list_trad_4)<maxchar)
				{
					trad_head[tl]=paste("https://translate.google.com/?hl=en#view=home&op=translate&sl=",tlang[tl],"&tl=de&text=",sep="")
					trad_gurl[tl]=paste(trad_head[tl],list_trad_4,sep="")
					trad_eurl[tl]=URLencode(trad_gurl[[tl]])

					remDr$navigate(trad_eurl[[tl]])

					att2=1
					sflag2=FALSE
					while(sflag2==FALSE)
					{

						writeLines(paste("Translating ",ebl-sbl," lines [",sbl,":",ebl,"/",nrow(orig[[ol]]),"] [block ",i,"/",nb,", ",nchar(list_trad_4)," chars] of ",origlang[ol]," original file from ",tlang[tl]," to de. Attempt: ",att2," ... ", sep=""));

						check2=remDr$findElements(using='css selector',"body")[[1]]$getElementText()[[1]]
						sflag2=!is.na(grep(checktag2, check2)[1])
						att2=att2+1
						if(att2>=10)
						{	
							# stop    remDr
							print("Too many attempts, restarting the server!")
						    lrem=1;while(lrem>0){lrem=length(remDr$closeall())}
							# restart remDr
							checktag2=sample(999999:100000000,1, replace=FALSE)
							list_trad_3=append(list_trad_2,checktag2)
							list_trad_4=str_c(list_trad_3,collapse="\n")
							trad_head[tl]=paste("https://translate.google.com/?hl=en#view=home&op=translate&sl=",tlang[tl],"&tl=de&text=",sep="")
							trad_gurl[tl]=paste(trad_head[tl],list_trad_4,sep="")
							trad_eurl[tl]=URLencode(trad_gurl[[tl]])
							remDr$open(silent = TRUE)
							remDr$navigate(trad_eurl[[tl]])
							att2=1
						}
					}

					sour2=remDr$getPageSource()
					page2=read_html(sour2[[1]])
					xml_find_all(page2, ".//br") %>% xml_add_sibling("p", "\n")
					xml_find_all(page2, ".//br") %>% xml_remove()
					tlid2=html_nodes(page2,'.tlid-translation')
					trad_tode_0=html_text(tlid2)
					trad_tode_1=strsplit(trad_tode_0,split="\n")
					trad_tode_2=gsub("_Pc_","%",trad_tode_1[[1]])
					trad_tode_3=gsub("_E0_","&",trad_tode_2)
					#remove the server tag
					trad_tode_4=trad_tode_3[-length(trad_tode_3)]

					
				}else # Case of translation exceeds 5000 characters: we must split the list_trad_2 list
				{

					writeLines("Support translation terms exceed 5000 char. Splitting the list.")
					#print(list_trad_2)
					ltr2=floor(length(list_trad_2)/2)

					list_trad_2_split=list()
					list_trad_2_split[[1]]=list_trad_2[1:ltr2]
					list_trad_2_split[[2]]=list_trad_2[(ltr2+1):length(list_trad_2)]
					trad_tode_4_split=list()

					for(isplit in 1:2)
					{
						checktags=sample(999999:100000000,1, replace=FALSE)
						list_trad_0=paste(list_trad_2_split[[isplit]],sep="")
						list_trad_1=gsub("%","_Pc_",list_trad_0)
						list_trad_2=gsub("&","_E0_",list_trad_1)
						list_trad_3=append(list_trad_2,checktags)
						list_trad_4=str_c(list_trad_3,collapse="\n")
						trad_head[tl]=paste("https://translate.google.com/?hl=en#view=home&op=translate&sl=",tlang[tl],"&tl=de&text=",sep="")
						trad_gurl[tl]=paste(trad_head[tl],list_trad_4,sep="")
						trad_eurl[tl]=URLencode(trad_gurl[[tl]])
						remDr$navigate(trad_eurl[[tl]])
						atts=1
						sflags=FALSE
						while(sflags==FALSE)
						{
							writeLines(paste("Translating ",ebl-sbl," lines [",sbl,":",ebl,"/",nrow(orig[[ol]])," [block ",i,"-",isplit,"/",nb,", ",nchar(list_trad_4)," chars] of ",origlang[ol]," original file from ",tlang[tl]," to de. Attempt: ",atts," ... ", sep=""));
							checks=remDr$findElements(using='css selector',"body")[[1]]$getElementText()[[1]]
							sflags=!is.na(grep(checktags, checks)[1])
							atts=atts+1
							if(atts>=10)
							{	
								# stop    remDr
								print("Too many attempts, restarting the server!")
							    lrem=1;while(lrem>0){lrem=length(remDr$closeall())}
								# restart remDr
								checktags=sample(999999:100000000,1, replace=FALSE)
								list_trad_0=paste(list_trad_2_split[[isplit]],sep="")
								list_trad_1=gsub("%","_Pc_",list_trad_0)
								list_trad_2=gsub("&","_E0_",list_trad_1)
								list_trad_3=append(list_trad_2,checktags)
								list_trad_4=str_c(list_trad_3,collapse="\n")
								trad_head[tl]=paste("https://translate.google.com/?hl=en#view=home&op=translate&sl=",tlang[tl],"&tl=de&text=",sep="")
								trad_gurl[tl]=paste(trad_head[tl],list_trad_4,sep="")
								trad_eurl[tl]=URLencode(trad_gurl[[tl]])
								remDr=remoteDriver(remoteServerAddr = "localhost", port = 4444, browserName = "firefox",version="76.0", extraCapabilities = fprof)
								remDr$open(silent = TRUE)
								remDr$navigate(trad_eurl[[tl]])
								atts=1
							}
						}

						sour2=remDr$getPageSource()
						page2=read_html(sour2[[1]])
						xml_find_all(page2, ".//br") %>% xml_add_sibling("p", "\n")
						xml_find_all(page2, ".//br") %>% xml_remove()
						tlid2=html_nodes(page2,'.tlid-translation')
						trad_tode_0=html_text(tlid2)
						trad_tode_1=strsplit(trad_tode_0,split="\n")
						trad_tode_2=gsub("_Pc_","%",trad_tode_1[[1]])
						trad_tode_3=gsub("_E0_","&",trad_tode_2)
						#remove the server tag
						trad_tode_4_split[[isplit]]=trad_tode_3[-length(trad_tode_3)]

					} # end of the cycle on the splitted block

					trad_tode_4=c(trad_tode_4_split[[1]],trad_tode_4_split[[2]])

				} # fi of whether the translation exceeds the 5000 chars barrier

				writeLines(paste("Saving block... Blocks to next refresh: ",bref-1,". Next login in ", round(max_working_seconds-working_seconds)," secs. ", sep=""))

				# We build the lines to be saved,
				# But first we restore the special characters to 
				# the original terms (For the tranlsation, it has already done)

				list_term_1=gsub("_Pc_","%",list_term_1)
				list_term_1=gsub("_E0_","&",list_term_1)

				resu=cbind.data.frame(
						orig[[ol]][sbl:ebl,1:5], 
						orig_lang=origlang[[nol]], 
						typeId=orig[[ol]]$typeId[sbl:ebl],
						orig_term=list_term_1,
						supp_lang=tlang[tl], 
						supp_term=trad_clean_4,
						languageCode="de", 
						germ_term=trad_tode_4,
						caseSignificanceId=orig[[ol]]$caseSignificanceId[sbl:ebl]
						)
				# save the data
				resu_file_name=paste("./snomed_translations/sct2_Description_Snapshot-from_",origlang[ol],"_to_de_using_",tlang[tl],"_IMBI_2020.txt",sep="")

				if(i==1)
				{
					write.table(resu,file=resu_file_name,sep="\t",col.names=TRUE,row.names=FALSE,quot=FALSE)
				}else
				{
					write.table(resu,file=resu_file_name,sep="\t",col.names=FALSE,row.names=FALSE,quot=FALSE,append=TRUE)
				}

				writeLines(" ")
				bref=bref-1

			} # end cycle on the blocks
		} # close the if on equal languages
	} # end cycle on languages (tl)
} # end cycle on the original SNOMED files (ol)


# Logging off from UKL before leaving R

logoff=0
while(logoff==0)
{
	writeLines("Access UKL: Close/restart the remote driver...");
	lrem=1;while(lrem>0){lrem=length(remDr$closeall())};remDr$open(silent = TRUE); 
	writeLines("Access UKL: Check for correct logout.")
	f1=0;while(f1==0){remDr$navigate(uniurl);f1=length(remDr$findElements(using='css selector',"#UserCheck_Login_Button_span")); 
	writeLines(paste("Access UKL: Portal... ",f1,sep=""))}
	uniwebElem1=remDr$findElements("css selector", "#LoginUserPassword_auth_username");	uniwebElem1[[1]]$sendKeysToElement(list("------"));
	writeLines("Access UKL: Username... "); 									
	uniwebElem2=remDr$findElements("css selector", "#LoginUserPassword_auth_password");	uniwebElem2[[1]]$sendKeysToElement(list("-------")); 
	writeLines("Access UKL: Password... ")
	f2=0;while(f2==0){f2=length(remDr$findElements(using = 'css selector', "#UserCheck_Login_Button_span"));
	writeLines(paste("Access UKL: Login button... ",f2,sep=""))};
	uniwebElem3=remDr$findElements(using = 'css selector', "#UserCheck_Login_Button")
	uniwebElem3[[1]]$clickElement(); 
	logoff=length(remDr$findElements(using='css selector',"#UserCheck_Logoff_Button_span"))
}
uniwebElem4=remDr$findElements(using = 'css selector', "#UserCheck_Logoff_Button_span")
uniwebElem4[[1]]$clickElement();
writeLines("Access UKL: Logged off.") 
lrem=1;while(lrem>0){lrem=length(remDr$closeall())}
Sys.sleep(5);







