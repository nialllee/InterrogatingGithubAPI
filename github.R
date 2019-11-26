#install.packages("jsonlite")
library(jsonlite)
#install.packages("httpuv")
library(httpuv)
#install.packages("httr")
library(httr)

# Can be github, linkedin etc depending on application
oauth_endpoints("github")

# Change based on what you 
myapp <- oauth_app(appname = "API_Interrogation",
                   key = "e62ae09aa37beea7b50d",
                   secret = "af6ceed4f82e31c7fecca2d6ef9e32853d5a4d77")

# Get OAuth credentials
github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)

# Use API
gtoken <- config(token = github_token)
req <- GET("https://api.github.com/users/jtleek/repos", gtoken)

# Take action on http error
stop_for_status(req)

# Extract content from a request
json1 = content(req)

# Convert to a data.frame
gitDF = jsonlite::fromJSON(jsonlite::toJSON(json1))

# Subset data.frame
gitDF[gitDF$full_name == "jtleek/datasharing", "created_at"] 

#first analysis of my own profile
myProfile = fromJSON("https://api.github.com/users/nialllee")
myProfile$followers
myProfile$public_repos

myFollowers = fromJSON("https://api.github.com/users/nialllee/followers")
myFollowers$login
length = length(myFollowers$login)
length #Number of followers

myProfile$following
following = fromJSON("https://api.github.com/users/nialllee/following")
following$login

repository = fromJSON("https://api.github.com/users/nialllee/repos")
repository$name
repository$created_at

myProfile$bio

# browser view of json
myProfileJSon = toJSON(myProfile, pretty = TRUE)
myProfileJSon

#Grant's profile
arnottgProfile = fromJSON("https://api.github.com/users/arnottg")
arnottgProfile$followers
arnottgProfile$following
arnottgProfile$public_repos

#OTHER USER
#below I am going to interrogate another user and put there data into a data.frame
#analysis of a very active user stefanprodan (found on trending)
prodanProfile = GET("https://api.github.com/users/stefanprodan/followers?per_page=100;", gtoken)
stop_for_status(prodanProfile)

# get content from prodan's profile
extract = content(prodanProfile)

# Convert content to dataframe
githubDB = jsonlite::fromJSON(jsonlite::toJSON(extract))

# Subset dataframe
githubDB$login

#PLOTS
#Gets a username list
id = githubDB$login
user_ids = c(id)

#make a data.frame and empty vector
users = c()
usersDB = data.frame(
  username = integer(),
  following = integer(),
  followers = integer(),
  repos = integer(),
  dateCreated = integer()
)

#scans through users and adds them to the list
for(i in 1:length(user_ids))
{
  
  followingURL = paste("https://api.github.com/users/", user_ids[i], "/following", sep = "")
  followingRequest = GET(followingURL, gtoken)
  followingContent = content(followingRequest)
  
  #Disregards followers with zero followers
  if(length(followingContent) == 0)
  {
    next
  }
  
  followingDF = jsonlite::fromJSON(jsonlite::toJSON(followingContent))
  followingLogin = followingDF$login
  
  #scans through following
  for (j in 1:length(followingLogin))
  {
    #ensures no duplicates
    if (is.element(followingLogin[j], users) == FALSE)
    {
      #Adds user to list
      users[length(users) + 1] = followingLogin[j]
      
      #gets user information
      followingUrl2 = paste("https://api.github.com/users/", followingLogin[j], sep = "")
      following2 = GET(followingUrl2, gtoken)
      followingContent2 = content(following2)
      followingDF2 = jsonlite::fromJSON(jsonlite::toJSON(followingContent2))
      
      #gets user's following
      followingNumber = followingDF2$following
      
      #gets users followers
      followersNumber = followingDF2$followers
      
      #gets users number of repos 
      reposNumber = followingDF2$public_repos
      
      #gets year user joined github
      yearCreated = substr(followingDF2$created_at, start = 1, stop = 4)
      
      #Adds user info to a new data frame row
      usersDB[nrow(usersDB) + 1, ] = c(followingLogin[j], followingNumber, followersNumber, reposNumber, yearCreated)
      
    }
    next
  }
  if(length(users) > 150)
  {
    break
  }
  next
}

#PLOT1
#Plotly details
Sys.setenv("plotly_username"="leen1")
Sys.setenv("plotly_api_key"="••••••••••")

#repositories vs followers year by year
plot1 = plot_ly(data = usersDB, x = ~repos, y = ~followers, text = ~paste("Followers: ", followers, "<br>Repositories: ", repos, "<br>Date Created:", dateCreated), color = ~dateCreated)
plot1

#upload to plotly
api_create(plot1, filename = "Repositories vs Followers")

#PLOT2
#following vs followers year by year
plot2 = plot_ly(data = usersDB, x = ~following, y = ~followers, text = ~paste("Followers: ", followers, "<br>Following: ", following), color = ~dateCreated)
plot2

#upload to plotly
api_create(plot2, filename = "Following vs Followers")

#PLOT3
#I will now plot the top languages used by the 150 users above.
languages = c()

for (i in 1:length(users))
{
  RepositoriesUrl = paste("https://api.github.com/users/", users[i], "/repos", sep = "")
  Repositories = GET(RepositoriesUrl, gtoken)
  RepositoriesContent = content(Repositories)
  RepositoriesDF = jsonlite::fromJSON(jsonlite::toJSON(RepositoriesContent))
  RepositoriesNames = RepositoriesDF$name
  
  #Loop through users repos
  for (j in 1: length(RepositoriesNames))
  {
    #add repos to data frame
    RepositoriesUrl2 = paste("https://api.github.com/repos/", users[i], "/", RepositoriesNames[j], sep = "")
    Repositories2 = GET(RepositoriesUrl2, gtoken)
    RepositoriesContent2 = content(Repositories2)
    RepositoriesDF2 = jsonlite::fromJSON(jsonlite::toJSON(RepositoriesContent2))
    language = RepositoriesDF2$language
    
    #Removes repos with unknown languages
    if (length(language) != 0 && language != "<NA>")
    {
      languages[length(languages)+1] = language
    }
    next
  }
  next
}

#tables the top languages
allLanguages = sort(table(languages), increasing=TRUE)

top10Languages = allLanguages[(length(allLanguages)-9):length(allLanguages)] 

#converts to dataframe
languageDF = as.data.frame(top10Languages)

#Plot data frame
plot3 = plot_ly(data = languageDF, x = languageDF$languages, y = languageDF$Freq, type = "bar")
plot3

Sys.setenv("plotly_username"="leen1")
Sys.setenv("plotly_api_key"="IIeUKBRn1wLrFXxzUmyq")
api_create(plot3, filename = "10 Most Popular Languages")

#In this plot Javascript is the most popular language and C is the least popular

