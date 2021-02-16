# MSEreview
The project reviews the documentation of management strategy evaluation (MSE) processes to aid practitioners. The aim is to store information about how prior processes were conducted so that practioners can learn from prior experience.

This document describes the procedure used to maintain the shiny application and database that display and store the MSE reviews

## New reviews are entered
There are two mechanisms by which an MSE review can be entered into this system, a user supplied review and an application management supplied review. 

A user of the application can enter a review using the "Submit MSE - Data Entry Form" tab on the shiny application. When a review is entered this way the data is added to a [Google sheet](https://docs.google.com/spreadsheets/d/1YjOTei_N7RS05rxXrVB6iuUptjYTDQC4xTLeoR-8fi8/edit#gid=0) and an email is sent to the application manager (currently Jonathan Cummings, jonathan.cummmings@gmail.com) to alert them that a new review has been added. 


## Data is stored in the database
The first is that an MSE review is entered directly into the database by the database manager (currently Jonathan Cummings, jonathan.cummings@gmail.com). 

## Data is exported from the database


## The Rdata file is updated
We searched the MSE literature via Web of Science, searching for “management strategy evaluation” by topic across all years on January 8th, 2019. This search returned 264 results, of which 154 were management strategy evaluations after removing articles that were reviews, meta-analyses, or simply cited other MSE articles. We reviewed a random sample of 30 of these 154 articles.

## The shiny application is republished