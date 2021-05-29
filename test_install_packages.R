r = getOption("repos")
r["CRAN"] = "http://cran.us.r-project.org"
options(repos = r)

# for sure these
install.packages("curl")
install.packages("xml2")
install.packages("httr")
install.packages("aws.s3")
