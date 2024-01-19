## script tp create a users dataframe for shinymanager

# users <- data.frame(user=NA,
#            password=NA,
#            firstName=c("Miguel", "Wende", "Ismail", "Winnyfred", "Jennifer", "Yoseph", "Mark", "Parthiban", "Shalab", "Moctar", "Thiago","Khaled","Ibnou","Eduardo","Bert","Keith","Juan"),
#            lastName=c("Sanchez-Garcia", "Mengesha", "Rabbi", "Amongi", "Wilker", "Beyene", "Nas", "Prakash", "Dixit", "Kante", "Mendez","","","","","",""),
#            affiliation=c("ICARDA","IITA", "IITA", "CIAT", "CIAT", "CIMMYT", "CIMMYT", "IRRI", "IRRI", "CIP", "CIP","ICARDA","IITA","IRRI","CIP","CIMMYT","CIMMYT"),
#            email=c("M.Sanchez-Garcia@cgiar.org","W.Mengesha@cgiar.org", "I.Rabbi@cgiar.org", "w.amongi@cgiar.org", "j.wilker@cgiar.org", "y.beyene@cgiar.org", "T.NAS@cgiar.org", "p.prakash@irri.org", "s.dixit@irri.org", "M.Kante@cgiar.org", "T.Mendes@cgiar.org",
#                    "K.EL-SHAMAA@cgiar.org","i.dieng@cgiar.org","g.covarrubias@irri.org","B.DeBoeck@cgiar.org","K.GARDNER@cgiar.org","j.burgueno@cgiar.org")
#            )
#
# head(users)
# users$admin <- FALSE
# users$user <- users$email
# users$start <- NA
# users$expire <- NA
# users$stringsAsFactors <- FALSE
# users$repository <- "R/outputs"
# head(users)
#
# passwordGen <- function(nLetters, nNumbers){
#   step1 <- sample(0:9, nNumbers, replace=TRUE)
#   step2 <- sample(c(letters,LETTERS), nLetters, replace=TRUE)
#   step3 <- sample(c(as.character(step1), step2), nLetters+nNumbers)
#   pass <- paste(step3,collapse = "")
#   return(pass)
# }
#
# for(i in 1:nrow(users)){
#   users$password[i] <- passwordGen(8,8)
# }
#
# head(users)

# saveRDS(users, file = "users.rds")

