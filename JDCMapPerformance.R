#Overall groups
ResumeMaps <- read.csv("~/ResumeMaps4.csv")
table <- ResumeMaps$Count
table <- matrix(table, ncol = 17, byrow = T)
colnames(table) <- levels(ResumeMaps$Label)
rownames(table) <- levels(ResumeMaps$Level)
colors <- c("black","white")
barplot((table),beside = FALSE, col = colors, ylim = c(0,10), xlab = "Session and group", main = "Finished and Unfinished maps", legend.text = TRUE, args.legend = list(x = "topright", bty = "n"))

#Per session
Session2 <- read.csv("~/Session2.csv")
Session3 <- read.csv("~/Session3.csv")
Session4 <- read.csv("~/Session4.csv")
Session6 <- read.csv("~/Session6.csv")

#In session 2, we have 3 groups
table_s2 <- Session2$Count
table_s2 <- matrix(table_s2, ncol = 3, byrow = T)
colnames(table_s2) <- levels(Session2$Label)
rownames(table_s2) <- levels(Session2$Level)
colors <- c("black","white")
barplot((table_s2),beside = FALSE, col = colors, ylim = c(0,10), xlab = "Group", main = "Finished and Unfinished maps Session 2", legend.text = TRUE, args.legend = list(x = "topright", bty = "n"))

#In session 3, we have 4 groups
table_s3 <- Session3$Count
table_s3 <- matrix(table_s3, ncol = 4, byrow = T)
colnames(table_s3) <- levels(Session3$Label)
rownames(table_s3) <- levels(Session3$Level)
colors <- c("black","white")
barplot((table_s3),beside = FALSE, col = colors, ylim = c(0,10), xlab = "Group", main = "Finished and Unfinished maps Session 3", legend.text = TRUE, args.legend = list(x = "topright", bty = "n"))

#In session 4, we have 5 groups
table_s4 <- Session4$Count
table_s4 <- matrix(table_s4, ncol = 5, byrow = T)
colnames(table_s4) <- levels(Session4$Label)
rownames(table_s4) <- levels(Session4$Level)
colors <- c("black","white")
barplot((table_s4),beside = FALSE, col = colors, ylim = c(0,10), xlab = "Group", main = "Finished and Unfinished maps Session 4", legend.text = TRUE, args.legend = list(x = "topright", bty = "n"))

#In session 6, we have 5 groups
table_s6 <- Session6$Count
table_s6 <- matrix(table_s6, ncol = 5, byrow = T)
colnames(table_s6) <- levels(Session6$Label)
rownames(table_s6) <- levels(Session6$Level)
colors <- c("black","white")
barplot((table_s6),beside = FALSE, col = colors, ylim = c(0,10), xlab = "Group", main = "Finished and Unfinished maps Session 6", legend.text = TRUE, args.legend = list(x = "topright", bty = "n"))
