# cobol
The program and jcl were created for the IBM Master The Mainframe Grand Challenge. I decided to write a proram that took data from public datasets on the UK's Office of National Statistics website about COVID-19 deaths and indicators of social deprivation, to see if there was a corellation between them. The resulting program is a proof-of-concept, as the datasets used are not correlatable in any meaningful way.

COVID-19 Deaths by LAD with SD Index

 The purpose of the report is to see whether there is any correlation between
 COVID-19 deaths within a LAD and the LAD's index of social deprivation. The
 expectation would be that LADs with higher SD indices would also have higher
 death rates on account of social factors such as poverty, housing, social
 class etc etc.

 The report contains seven tables. Six are of COVID-19 deaths by Local Authority
 District (LAD) in three groups of two. The last table is a summary of LAD by
 English region.

 The three groups are: All LADs | Non-Metropolitan LADs | Metropolitan LADs

 Within each group are two tables: Top ten LADs | Bottom 10 LADs.

 The bottom ten LADs are in reverse order, ie.
      1 = smallest death rate
     10 = tenth smallest death rate

 The report fields for the LAD tables are:
     Num: Top ten ranking (highest to lowest or lowest to highest)
     Local Authority District
     Death Rate: per 100,000 within LAD
     # Deaths: actual number of deaths within LAD
     MDL Rank: rank within index of Multiple Deprivation (Proportion of
               Lower-layer Super Output Areas (LSOAs*) in the most
               deprived 10% nationally
     EXT Rank: rank within index of extent of proportion of a larger areas
               population living in the most deprived LSOAs in the country.
     LC Rank: rank within index of population weighted average of the ranks
              of a larger area's most deprived LSOAs that contain exactly 10%
              of the larger area's population.

 For all ranks, 1 = highest (out of 228 LADs)

 The original data were extracted from public datasets from the UK Office of
 National Statistics for March 2020.

 As such, the data and report are a snapshot of the early stages of the pandemic
 (and are therefore essentially meaningless), but demonstrate how the process
 could be developed into a meaningful analysis by, for example, extracting raw
 data over a period of time.

 The data for each summary measure are presented in paired columns
 of scores and ranks:
     The first data column in each pair is the score for the measure.
       The higher the score, the more deprived the area.
     The second data column is the rank for the measure.
       On each measure, the local authority district with a rank of 1 is the
       most deprived, and the area ranked 326 is the least deprived.

  **LSOAs are the smallest unit area within a Local Authority District: there
    are many LSOAs for each LAD.
