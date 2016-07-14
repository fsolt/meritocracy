library(pewdata)
# Note: The Pew Research Center requires a bit of personal information
# from those wishing to download data. Before running pew_download, then,
# you should be sure to add these options to your .Rprofile substituting
# your own info for the example below:
# 
# options("pew_name" = "Juanita Herrera",
#         "pew_org" = "Upper Midwest University", 
#         "pew_phone" = "888-000-0000", 
#         "pew_email" = "jherrera@uppermidwest.edu")
#
# Note too that the pewdata package depends on a Firefox installation.

# Two Pew files could not be read into R in their original formats and so
# were converted to Stata .dta files and included in the reproducibility materials.
# The dataverse package, which provides programmatic access to, e.g., the AJPS Dataverse 
# remains a work in progress, so the NJL reproducibility file used was also included
# in the reproducibility materials.  Downloads of files from the Equality of
# Opportunity Project used in the Appendix are done programatically by the 
# merit_appendix.Rnw file.

pew_download(file_id = c(20034641, # 2011 Political Typology Survey
                         20018321, # Dec2005
                         20037102, # Dec2011 Political
                         20018361, # Immigration06
                         20018540, # Sept-10-Political-Independents
                         20018554, # typo00
                         20018557), # Typology05
             download_dir = "data/merit/version_a")

pew_download(file_id = 17708,
             area = "socialtrends",
             download_dir = "data/merit/version_a")

pew_download(file_id = "u-s-religious-landscape-survey",
             area = "religion",
             download_dir = "data/merit/version_a")

pew_download(file_id = c(20049871, 20018559, 20018560),
             download_dir = "data/merit/version_b")

pew_download(file_id = 5704,
             area = "socialtrends",
             download_dir = "data/merit/version_b") 