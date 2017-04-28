library(R4CouchDB)
library(jsonlite)

couchdb_un <- jsonlite::fromJSON("key.json")$couchdb_un
couchdb_pw <- jsonlite::fromJSON("key.json")$couchdb_pw

c <- GET(paste("http://webhost.pittsburghpa.gov:5984/burghs-eye-view-places/_design/search-terms/_view/search-terms"), add_headers(`Content-Type`= "application/json"))
r <- content(c, "text")
df <- jsonlite::fromJSON(r)$rows
terms <- unique(df[c("key","value")])
colnames(terms) <- c("sessionID", "searchTerm")
