# Mahmoud Bakar | BDA400 Assignment 2
# Run once from the Assignment2 directory.
needed <- c("quantmod", "TTR", "xts", "zoo", "curl", "jsonlite")
missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(missing, repos = "https://cloud.r-project.org")
versions <- data.frame(Package = needed,
                       Version = vapply(needed, function(p) as.character(packageVersion(p)), character(1)))
print(versions, row.names = FALSE)
dir.create("output", showWarnings = FALSE)
write.csv(versions, "output/package_versions.csv", row.names = FALSE)
writeLines(capture.output(sessionInfo()), "output/sessionInfo.txt")
