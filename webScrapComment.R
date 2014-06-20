library(RCurl)
library(XML)

trim <- function(x) gsub("^\\s+|\\s+$", "", x)
YouMah.Complete<-NA

url<-"https://www.youtube.com"
front_url<-"https://www.youtube.com/results?search_query=mahabharat+11th+june+2014+full+episode&search_sort=video_view_count&page="
url_video<-"http://gdata.youtube.com/feeds/api/videos/"

	page_num<-1
	site<-paste(front_url,page_num,sep="")
	web.code<-getURLContent(site,ssl.verifypeer = FALSE)
	web.html<-htmlTreeParse(web.code,asText=T,useInternalNodes=T)

	title<-xpathSApply(web.html,"//a[contains(@title,'Mahabharat')]",xmlValue)
	title_link<-xpathSApply(web.html,"//a[contains(@title,'Mahabharat')]/@href")
	YouMah<-data.frame(Title=title,link=title_link)

	##extracting information on each video
	###loop for each video
	for (vid in 1:length(title)){
		view_url<-paste(url,title_link[[vid]],sep="")
		view.code<-getURL(view_url,ssl.verifypeer=FALSE)
		view.html<-htmlTreeParse(view.code,asText=T,useInternalNodes=T)
		
		#view count
		view.count<-xpathSApply(view.html,"//span[@class='watch-view-count']",xmlValue)
		if(length(view.count)>0){
			YouMah$view.count[vid]<-gsub("\\.","",view.count)
		}else{
			YouMah$view.count[vid]<-NA
		}
		
		#likes count
		likes.count<-xpathSApply(view.html,"//span[@class='likes-count']",xmlValue)
		if(length(likes.count)>0){
			YouMah$likes.count[vid]<-likes.count
		}else{
			YouMah$likes.count[vid]<-NA
		}
		
		#dislikes count
		dislikes.count<-xpathSApply(view.html,"//span[@class='dislikes-count']",xmlValue)
		if(length(dislikes.count)>0){
			YouMah$dislikes.count[vid]<-dislikes.count
		}else{
			YouMah$dislikes.count[vid]<-NA
		}
		
		#counting comments
		videoID<-unlist(strsplit(title_link[[vid]],"="))[2]
		video.api.url<-paste(url_video,videoID[[1]],sep="")
		video.source<-getURL(video.api.url)
		video.xml<-htmlTreeParse(video.source,asText=T,useInternalNodes=T)
		video.top<-xmlRoot(video.xml)
		video.feedlink<-video.top[["body"]][["entry"]][["comments"]][["feedlink"]]
		
		if (!is.null(video.feedlink)){
			feedlink.attributes<-xmlAttrs(video.feedlink)
			comment.count<-feedlink.attributes[[3]]
			YouMah$comment.count[vid]<-comment.count	
		}else{
			YouMah$comment.count[vid]<-NA	
		}
		
		#finding the duration of movie
		idNodes <- getNodeSet(view.html, "//span[@class='metadata-info']")
		attributes <- lapply(idNodes, xpathApply, path = './span[@class]', xmlAttrs)
		time.val<-lapply(idNodes,xpathSApply,path='./text()',xmlValue)
		
		## loop to get time / duration of the movie
		
		if (length(attributes)>0){
			for (i in 1:length(attributes)){
			if (length(attributes[[i]])>0)
				{
				if (attributes[[i]]=="ypc-offer-duration")
					{
						dirty.time.duration=time.val[[i]]
						clean.time.duration=trim(dirty.time.duration)
						YouMah$time.duration[vid]=clean.time.duration[clean.time.duration!=""]
					}	
				}	
			}
		}else{
			YouMah$time.duration[vid]=NA
		}
	}



