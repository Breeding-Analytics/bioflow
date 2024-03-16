# passwordGen <- function(nLetters, nNumbers){
#   step1 <- sample(0:9, nNumbers, replace=TRUE)
#   step2 <- sample(c(letters,LETTERS), nLetters, replace=TRUE)
#   step3 <- sample(c(as.character(step1), step2), nLetters+nNumbers)
#   pass <- paste(step3,collapse = "")
#   return(pass)
# }
#
# # script tp create a users dataframe for shinymanager
# users <- data.frame(user=NA,   stringsAsFactors = FALSE,
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
# # users$stringsAsFactors <- FALSE
# users$repository <- "R/outputs"
# head(users)
#
#
#
# for(i in 1:nrow(users)){
#   users$password[i] <- passwordGen(8,8)
# }
#
# head(users)
# saveRDS(users, file = "users.rds")
#
# #####################################
# #####################################
# # adding new users
#
# users <- readRDS("~/Documents/bioflow/users.rds")
# toAdd <- data.frame(user=c("r.morantte@irri.org","m.catolos@irri.org", "m.heredia@irri.org"),
#                     password=apply(data.frame(1:3), 1,function(x){passwordGen(8,8)} ),
#                     firstName=c("Zhella","Margaret","Cristina"),
#                     lastName=c("Morantte","Catolos","Heredia"),
#                     affiliation=c("IRRI","IRRI","IRRI"),
#                     email=c("r.morantte@irri.org","m.catolos@irri.org", "m.heredia@irri.org"), admin=FALSE,
#                     start=NA, expire=NA, stringsAsFactors = FALSE, repository="R/outputs"
# )
# users <- rbind(users, toAdd)
# getwd()
# saveRDS(users, file = "users.rds")
#
# #####################################
# #####################################
# # adding new users again (16.03.2024)
#
# users <- readRDS("~/Documents/bioflow/users.rds")
# toAdd <- data.frame(user=c("a.kelly1@uq.edu.au","david.jordan@uq.edu.au", "emma.mace@uq.edu.au", "zulfi.jahufer@uq.edu.au", "youngwha.lee@gatesfoundation.org", "Jeff.Ehlers@gatesfoundation.org", "Jim.Lorenzen@gatesfoundation.org", "Gary.Atlin@gatesfoundation.org"),
#                     password=apply(data.frame(1:8), 1,function(x){passwordGen(8,8)} ),
#                     firstName=c("Allison","David","Emma","Zulfi","YoungWha","Jeff","Jim","Gary"),
#                     lastName=c("Kelly","Jordan","Mace","Jahufer","Lee","Ehlers","Lorenzen","Atlin"),
#                     affiliation=c(rep("UQ",4), rep("BMGF",4)),
#                     email=c("a.kelly1@uq.edu.au","david.jordan@uq.edu.au", "emma.mace@uq.edu.au", "zulfi.jahufer@uq.edu.au", "youngwha.lee@gatesfoundation.org", "Jeff.Ehlers@gatesfoundation.org", "Jim.Lorenzen@gatesfoundation.org", "Gary.Atlin@gatesfoundation.org"),
#                     admin=FALSE, start=NA, expire=NA, stringsAsFactors = FALSE, repository="R/outputs"
# )
# users <- rbind(users, toAdd)
# getwd()
# saveRDS(users, file = "users.rds")
#
# #####################################
# #####################################
# # adding new users again (16.03.2024)
#
# users <- readRDS("~/Documents/bioflow/users.rds")
# toAdd <- data.frame(user=c("v.k.singh@irri.org","p.sinha@irri.org","p.paul@irri.org","a.k.mall@irri.org","m.budumuru@contractors.irri.org","s.alam@irri.org","atul.singh@irri.org"),
#                     password=apply(data.frame(1:7), 1,function(x){passwordGen(8,8)} ),
#                     firstName=c("Vikas","Pallavi","Pronob", "Ashish","Muralidhar","Shamshad","Atul"),
#                     lastName=c("Singh","Sinha","Paul","Mall","Budumuru","Alam","Singh"),
#                     affiliation=c(rep("IRRI",7)),
#                     email=c("v.k.singh@irri.org","p.sinha@irri.org","p.paul@irri.org","a.k.mall@irri.org","m.budumuru@contractors.irri.org","s.alam@irri.org","atul.singh@irri.org"),
#                     admin=FALSE, start=NA, expire=NA, stringsAsFactors = FALSE, repository="R/outputs"
# )
# users <- rbind(users, toAdd)
# tail(users)
# getwd()
# saveRDS(users, file = "users.rds")
#
