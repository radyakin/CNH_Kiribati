README

Questions for Mike from October 2020:

1. Latest vrs from sharepoint version 0 ("KIR_VILLAGE_RESOURCE_SURVEY.dta") does not have interview__id or interview__key - i.e., no common columns to join with event_roster.dta

2. Same problem as 1 for KIR_MARKET_SURVEY.dta


Questions for Mike from June 2020:

* Is it correct to remove the 0/1 columns in the market Survey data?
* Is there a way to automate this process on your end? So that code still works with new data
* Where is the market availability data that we had in the last iteration?
* [name of product]_roster is the availability data
* What is the meaning of NA in the multiselect variable columns?
* "." and ".a" in STATA both mean missing. Refer to Mike's May 27 email for example (and to check for consistency with R)
* Why does VRS have household info and anemia info?
* Why are these output as different files (vrs, market, etc. each broken into multiple separate files)
* It is easiest to have the fewest number possible
* Are they going to translate the HIES iKiribati responses?
* Variable labels are cut off (how do we get the full answers back?) see: var_labels
* How to standardize units when none is given (e.g., question == Travel time outside boundary in outsideRoster dataset includes "12", "3", "30 minutes", "1 hour", etc)
* Is a response of "zero" the same as blank response? 
* What's the difference between interview__key and interview__id? Currently using both to join eventRoster, fishRoster, etc with vrs data to get island information

Suggested path forward:
 
1. Write out tidy versions of each data set and share csv
2. Write functions to visualize each question type:
     - Multi- and single-select, produce bar chart
     - Integer, produce bar chart
     - Continuous, produce histogram and (TBD: box and whisker)
     - Free response, compile unique answers with unique IDs for translation, question, island, possibly role
3. Loop through data and produce pdf of all plots
4. TBD: Start to creat summaries by village/island