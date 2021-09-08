# r_public
R stuff that's public.  Mostly COVID19 graphs.  

I created these graphs and maps both as a learning excercise and a way to get custom
graphs to visualize what I wanted too.  I also didn't care for the way many US maps 
didn't include Alaska, Hawaii, and the major US territories.  So, I made insert maps
for those as well as an insert for DC which is often invisible.  

The site for my graphs and maps is
https://sites.google.com/site/salishseacam/covid19?authuser=0

It would be relatively easy for someone to pick this up and modify it to make the 
graphs they are interested in.

CSV files of the data used are published along side the graphs

The raw data used for covid19 cases is from the COVID-19 Data Repository by
the Center for Systems Science and Engineering (CSSE) at Johns Hopkins
University.  To simplify I used the population data provided in the same
project.  Vaccination data is also pulled but not used much since it's so
spotty.

For maps census bureau shapefiles are used since they contain more of the US
but maps from map_data are also used for consistent borders.  Data continuity
issues are introduced when data from inconsistent sources is joined.  Business
logic for that is regretabbly sprinkled throughout and should be re-organized.

The R code is broken into a file of functions and a much smaller script to make the 
graphs.  The covid19 functions should certainly be made into a package.  I'd played with 
that but it got complicated with some of the testing.  

There is certainly some dubious content in here.  That is part of the learning 
process.  Comments and suggestions are always welcome.  Or fork and send pull 
requests if you're git literate.  

There is some dead-ended threads in the code.  Sometimes slicing and dicing
the data in dubious way.  One such mostly abandoned approach was visualizing
cases by bifurcating based on regions voting patterns.  

My products are pushed into AWS S3 buckets.  The keys and bucket name are set in a 
command line wrapper script, an example is included.  If you wanted to use S3 
buckets you could 
set the environment var somewhere else or do a similar wrapper for however your 
running R.  

Administrative Levels

There is an effort to be more international and less US centric.  So, insteat of 
States / Counties we're trying to use Admin Level 1 and Admin Level 2.  For now
there is a mix.  

Levels break down like this:
country
admin1 e.g. us state, cnd province
admin2 e.g. us county, french departments

Stuff in here:
LICENSE - Apahce license
README.md - this file
Rwrapper.sample - sample wrapper 
covid19_functions.R - the bulk for R code
covid19_install_packages.R - script for installing all libraries used
covid19_map_run.R - just make maps
covid19_run.R - make the whole kit and kaboodle
html_table.py - make table of states and counties
run_tests.sh - for testthat
tests/ - directory for github actions tests
work_in_progress/ - mostly cruft

Work to do:
- refactor making maps
- redo maps using something snazzy like leaflet for interactive maps
- clean-up warnings from map making (probably suppressing wanrings on
duplicating projections)
- add more tests
- clean-up or remove vaccination data
- built_all_states - make wa_and to be more generic
- pull all of the configuration of making graphs and maps out and 
put in separate file.  maybe even make it explicit configuration.

