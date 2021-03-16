      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    GC0050.
       AUTHOR.        Tim Willmott.
       DATE-WRITTEN.  15/02/2021.
       DATE-COMPILED. 15/02/2021.
      ******************************************************************
      **** This program is part of the submission to the Master The ****
      **** Mainframe Grand Challenge.                               ****
      **** It takes data from two files extracted from the ONS      ****
      **** datasets on COVID-19 and indicators of Social            ****
      **** Deprivation in England, plus a lookup file for Local     ****
      **** Authority District (LAD) and Region.                     ****
      **** It uses these data to produce a report on the top 10 and ****
      **** bottom 10 LADs in terms of deaths from Covid and cross   ****
      **** references them against Social Deprivation indices.      ****
      **** It does this for both Metropolitan and Non-Metropolitan  ****
      **** districts.
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      *SOURCE-COMPUTER. IBM-370 WITH DEBUGGING MODE.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DXLAD                    ASSIGN TO GCDXLAD.
           SELECT LADSUM                   ASSIGN TO GCLADSUM.
           SELECT REGION                   ASSIGN TO GCREGION.
           SELECT REPTXT                   ASSIGN TO GCREPTXT.
           SELECT LOCAUTHDIST              ASSIGN TO GCLADVSM
              ORGANIZATION indexed
              ACCESS random
              RECORD KEY  L-LAD
              FILE STATUS W-LAD-FS.
           SELECT PRINT-LINE               ASSIGN TO PRTLINE.

      ******************************************************************
       DATA DIVISION.

       FILE SECTION.
      ******************************************************************
      **** DXLAD - England COVID deaths by LAD
      ****   DX-MNM-CODE   - Metropolitan/Non-Metropolitan indicator
      ****   DX-LAD        - Local Authority code
      ****   DX-D-A        - Deaths - all causes
      ****   DX-DA-RATE    - Death rate - all causes
      ****   DX-LOWER-CI-A - Lower confidence limit - all
      ****   DX-UPPER-CI-A - Upper confidence limit - all
      ****   DX-D-C        - Deaths - covid-19
      ****   DX-DX-RATE    - Death rate - covid-19
      ****   DX-LOWER-CI-C - Lower confidence limit - covid-19
      ****   DX-UPPER-CI-C - Upper confidence limit - covid-19
      ******************************************************************
       FD  DXLAD RECORDING MODE F.
       01  D-X-LAD.
           05  DX-MNM-CODE             PIC 9.
           05  DX-LAD                  PIC X(9).
           05  DX-ALL.
               10  DX-D-A              PIC 9(4).
               10  DX-DA-RATE          PIC 9(4).99.
               10  DX-LOWER-CI-A       PIC 9(4).99.
               10  DX-UPPER-CI-A       PIC 9(4).99.
           05  DX-COVID.
               10  DX-D-C              PIC 9(4).
               10  DX-DC-RATE          PIC 9(4).99.
               10  DX-LOWER-CI-C       PIC 9(4).99.
               10  DX-UPPER-CI-C       PIC 9(4).99.
           05  FILLER                  PIC X(20).

      ******************************************************************
      **** LADSUM - England Social Deprivation indices by LAD
      ****   LD-LAD           - Local Authority Code
      ****   LD-AVG-RANK      - Average rank
      ****   LD-RANK-AVG-RANK - Rank of average rank
      ****   LD-AVG           - Average
      ****   LD-RANK-AVG      - Rank of average
      ****   LD-MD-LSOA       - Avg of LSOAs
      ****   LD-MDL-RANK      - Rank of avg of LSOAs
      ****   LD-EXTENT        - Avg of extent
      ****   LD-EXT-RANK      - Rank of avg of extent
      ****   LD-LC            - Avg of location
      ****   LD-LC-RANK       - Rank of avg of location
      ******************************************************************
       FD  LADSUM RECORDING MODE F.
       01  LAD-SUM.
           05  LD-LAD              PIC X(9).
           05  LD-AVG-RANK         PIC 9(5).99.
           05  LD-RANK-AVG-RANK    PIC 999.
           05  LD-AVG              PIC 99.999.
           05  LD-RANK-AVG         PIC 999.
           05  LD-MD-LSOA          PIC 9.9999.
           05  LD-MDL-RANK         PIC 999.
           05  LD-EXTENT           PIC 9.9999.
           05  LD-EXT-RANK         PIC 999.
           05  LD-LC               PIC 9(5).99.
           05  LD-LC-RANK          PIC 999.
           05  FILLER              PIC X(22).

      ******************************************************************
      **** England Administrative locations: LSOA, LAD, Region      ****
      ******************************************************************
       FD  LOCAUTHDIST.
       01  LAD.
           05  L-LAD               PIC X(9).
           05  L-LAD-NAME          PIC X(30).
           05  L-REGION            PIC X(9).
           05  FILLER              PIC X(32).

      ******************************************************************
      **** England Administrative Regions                           ****
      ******************************************************************
       FD  REGION RECORDING MODE F.
       01  REGN.
           05  RG-REG              PIC X(9).
           05  RG-NAME             PIC X(30).
           05  FILLER              PIC X(41).

      ******************************************************************
      **** text that appears on the last page of the report         ****
      ******************************************************************
       FD  REPTXT RECORDING MODE F.
       01  REPORT-TEXT             PIC X(80).  

       FD  PRINT-LINE RECORDING MODE F
                      LINAGE        60.
       01  PRINT-REC.
           05  FILLER               PIC X(89).

      ******************************************************************
       WORKING-STORAGE SECTION.
      *****    debug code - left in the program but commented out   ****
      *****    all debug data and code prefixed with 'D'            ****
      * 01  d-debug-variables.
      *     05  d-flag            pic 9.
      *         88  debug-off              value 0.
      *         88  debug-on               value 1.

       01  W-PGM-VARIABLES.
           05  W-REG-SUB          PIC 99   VALUE 1.
           05  WC-SUB             PIC 999  VALUE 1.
           05  WC-NUM-ENTRIES     PIC 999.
           05  WC-FIRST-METRO     PIC 999.
           05  WP-COUNT           PIC 99.
           05  W-LAD-FS           PIC 99.
           05  W-PAGENO           PIC 9    VALUE 0.
           05  W-PROGNAME.
               10  FILLER         PIC X(8) VALUE " End of ".
               10  W-PROG-ID      PIC X(6) VALUE "GC0050".
               10  FILLER         PIC X    VALUE " ".
           05  W-YYYYMMDD         PIC 9(8).
           05  W-REFMOD-TIME-ITEM PIC 9(8).           
           05  WP-REGION-FLAG     PIC 9.
               88 PRINT-LAD                VALUE 0.
               88 PRINT-REGION             VALUE 1.
           05  W-EOF-FLAGS        PIC 9.
               88 NOT-EOF                  VALUE 0.
               88 END-OF-FILES             VALUE 1.
           05  W-EOF-REG-FLAG     PIC 9.
               88 NOT-EOF-REGION           VALUE 0.
               88 EOF-REGION               VALUE 1.
           05  W-EOF-RPTXT-FLAG   PIC 9.
               88  FIRST-REPTXT            VALUE 0.
               88  NOT-EOF-REPTXT          VALUE 1.
               88  EOF-REPTXT              VALUE 2.    
           05  W-LAD-ERROR        PIC 9.
               88 VSAM-OK                  VALUE 0.
               88 VSAM-FILE-ERROR          VALUE 1.
           05  W-PAGE-IND         PIC 9.
               88 TOP-OF-PAGE              VALUE 0.
               88 PAGE-OK                  VALUE 1.
               88 FORCE-PAGE-THROW         VALUE 2.

      ******************************************************************
      *****     data are read into this table and used to create    **** 
      *****     the report tables                                   ****
      ******************************************************************
       01  W-COMPOSITE-DATA.
           05 W-CD               OCCURS 228.
               10  WC-MNM-CODE   PIC 9.
               10  WC-LAD        PIC X(9).
               10  WC-LAD-NAME   PIC X(30).
               10  WC-REGION     PIC X(9).
               10  WC-DC         PIC 9(4).
               10  WC-DC-RATE    PIC 9(4)V99.
               10  WC-MDL-RANK   PIC 999.
               10  WC-EXT-RANK   PIC 999.
               10  WC-LC-RANK    PIC 999.

      ******************************************************************
      *****     data are read into this table and summarised to     **** 
      *****     create the Region report (page 3)                   ****
      ******************************************************************
       01  W-REGION.
           05 W-REG-REC          OCCURS 9.
               10  W-REG         PIC X(09).
               10  WRG-NAME      PIC X(30).
               10  WRG-NUM       PIC 9(4)    VALUE 0.
               10  WRG-A-TOP     PIC 9(4)    VALUE 0.
               10  WRG-A-BTM     PIC 9(4)    VALUE 0.
               10  WRG-M-TOP     PIC 9(4)    VALUE 0.
               10  WRG-M-BTM     PIC 9(4)    VALUE 0.
               10  WRG-N-TOP     PIC 9(4)    VALUE 0.
               10  WRG-N-BTM     PIC 9(4)    VALUE 0.
      *****    following data items unused in this program          ****
           05  WRG-DC            PIC 9(4)    VALUE 0.
           05  WRG-DC-RATE       PIC 9(4)V99 VALUE 0.
           05  WRG-MDL-RANK      PIC 999     VALUE 0.
           05  WRG-EXT-RANK      PIC 999     VALUE 0.
           05  WRG-LC-RANK       PIC 999     VALUE 0.

      ******************************************************************
      **** report related WS entries                                ****
      ******************************************************************
       01  W-REPORT-TABLE-TITLES.
           05  W-RTAB1           PIC X(30)   VALUE
               "table 1 : all LADs, top 10".
           05  W-RTAB2           PIC X(30)   VALUE
               "table 2 : all LADs, bottom 10".
           05  W-RTAB3           PIC X(40)   VALUE
               "table 3 : non-metropolitan, top 10".
           05  W-RTAB4           PIC X(40)   VALUE
               "table 4 : non-metropolitan, bottom 10".                        
           05  W-RTAB5           PIC X(30)   VALUE
               "table 5 : metropolitan, top 10".
           05  W-RTAB6           PIC X(40)   VALUE
               "table 6 : metropolitan, bottom 10".

      *****    headers/print lines = 89ch                           ****
       01  W-HEADERS.
           05  W-H1.
               10  FILLER      PIC X(12) VALUE "Program Id: ".
               10  WH1-PROG-ID PIC X(6).
               10  FILLER      PIC X(49) VALUE SPACES.
               10  FILLER      PIC XXX   VALUE "at ".
               10  WH1-TIME.
                   15  WH1-T1  PIC XX.
                   15  FILLER  PIC X     VALUE ":".
                   15  WH1-T2  PIC XX.
               10  FILLER      PIC X(4)  VALUE " on ".
               10  WH1-DATE.
                   15  WH1-DD  PIC XX.
                   15  FILLER  PIC X     VALUE "/".
                   15  WH1-DM  PIC XX.
                   15  FILLER  PIC X     VALUE "/".
                   15  WH1-DY  PIC X(4).

           05  W-H2.
               10  FILLER      PIC X(26) VALUE SPACES.
               10  FILLER      PIC X(36) VALUE
                  "COVID-19 Deaths by LAD with SD Index".
               10  FILLER      PIC X(16) VALUE SPACES.
               10  FILLER      PIC X(5)  VALUE "Page ".
               10  WH2-PAGENO  PIC 9.
               10  FILLER      PIC X(5)  VALUE " of 4".
      *****    headings for tables 1-6 (LAD summaries)              ****
           05  W-H3.
               10  FILLER      PIC X(5)   VALUE "Num  ".
               10  FILLER      PIC X(30)  VALUE
               "Local Authority District".
               10  FILLER      PIC X(5)   VALUE SPACES.
               10  FILLER      PIC X(10)  VALUE "Death Rate".
               10  FILLER      PIC XX     VALUE SPACES.
               10  FILLER      PIC X(8)   VALUE "# Deaths".
               10  FILLER      PIC XX     VALUE SPACES.
               10  FILLER      PIC X(8)   VALUE "MDL Rank".
               10  FILLER      PIC XX     VALUE SPACES.
               10  FILLER      PIC X(8)   VALUE "EXT Rank".
               10  FILLER      PIC XX     VALUE SPACES.
               10  FILLER      PIC X(7)   VALUE "LC Rank".
           05  W-H4.
               10  FILLER      PIC X(37)  VALUE ALL "=".
               10  WH4-PROG    PIC X(15)  VALUE ALL "=".
               10  FILLER      PIC X(37)  VALUE ALL "=".
      *****    headings for region table                            ****
           05  W-H5.
               10  FILLER      PIC X(30)  VALUE " ".
               10  FILLER      PIC X(12)  VALUE SPACES.
               10  FILLER      PIC X(10)  VALUE " ".
               10  FILLER      PIC XXxxx     VALUE SPACES.
               10  FILLER      PIC X(3)   VALUE "All".
               10  FILLER      PIC XX     VALUE SPACES.
               10  FILLER      PIC X(3)   VALUE "All".
               10  FILLER      PIC X(4)   VALUE SPACES.
               10  FILLER      PIC X(3)   VALUE "Met".
               10  FILLER      PIC XX     VALUE SPACES.
               10  FILLER      PIC X(3)   VALUE "Met".
               10  FILLER      PIC XXx     VALUE SPACES.
               10  FILLER      PIC X(4)   VALUE "NonM".
               10  FILLER      PIC X     VALUE SPACES.
               10  FILLER      PIC X(4)   VALUE "NonM".
           05  W-H6.
               10  FILLER      PIC X(45)  VALUE "Region ".
      *        10  FILLER      PIC X(8)   VALUE SPACES.
               10  FILLER      PIC X(5)   VALUE "# LAD".
               10  FILLER      PIC X(7)   VALUE SPACES.
               10  FILLER      PIC X(3)   VALUE "Top".
               10  FILLER      PIC XX     VALUE SPACES.
               10  FILLER      PIC X(3)   VALUE "Btm".
               10  FILLER      PIC X(4)   VALUE SPACES.
               10  FILLER      PIC X(3)   VALUE "Top".
               10  FILLER      PIC X(2)   VALUE SPACES.
               10  FILLER      PIC X(3)   VALUE "Btm".
               10  FILLER      PIC X(4)   VALUE SPACES.
               10  FILLER      PIC X(3)   VALUE "Top".
               10  FILLER      PIC X(2)   VALUE SPACES.
               10  FILLER      PIC X(3)   VALUE "Btm".

       01  W-PRINTLINES.
           05  W-PRINT-LAD.
               10  WP-L-COUNT    PIC Z9.
               10  FILLER        PIC XX    VALUE SPACES.
               10  WP-LAD-NAME   PIC X(30).
               10  FILLER        PIC X(9)  VALUE SPACES.
               10  WP-DC-RATE    PIC Z(3)9.99.
               10  FILLER        PIC X(6)  VALUE SPACES.
               10  WP-DC         PIC ZZZ9.
               10  FILLER        PIC X(7)  VALUE SPACES.
               10  WP-MDL-RANK   PIC ZZ9.
               10  FILLER        PIC X(7)  VALUE SPACES.
               10  WP-EXT-RANK   PIC ZZ9.
               10  FILLER        PIC X(6)  VALUE SPACES.
               10  WP-LC-RANK    PIC ZZ9.

           05  W-PRINT-REGION.
               10  WPR-NAME      PIC X(30).
               10  FILLER        PIC X(16) VALUE SPACES.
               10  WPR-NUM       PIC ZZZ9.
               10  FILLER        PIC X(6)  VALUE SPACES.
               10  WPR-A-TOP     PIC ZZZ9.
               10  FILLER        PIC X(1)  VALUE SPACES.
               10  WPR-A-BTM     PIC ZZZ9.
               10  FILLER        PIC X(3)  VALUE SPACES.
               10  WPR-M-TOP     PIC ZZZ9.
               10  FILLER        PIC X(1)  VALUE SPACES.
               10  WPR-M-BTM     PIC ZZZ9.
               10  FILLER        PIC XXx  VALUE SPACES.
               10  WPR-N-TOP     PIC ZZZ9.
               10  FILLER        PIC X  VALUE SPACES.
               10  WPR-N-BTM     PIC ZZZ9.

       01  W-PRINT-RECS.
           05  W-PRINT-REC        PIC X(89).
           05  W-PR-STORE         PIC X(89).

      ******************************************************************
      ******************************************************************
       PROCEDURE DIVISION.
      * Declaratives.
      * Debug-Declaratives Section.
      *     Use for Debugging on ALL PROCEDURES.
      * Debug-Declaratives-Paragraph.
      *     Display Debug-Name.
      * End Declaratives.

       A000 SECTION.
       A001.
      *****    debug code - print working-storage tables            **** 
      *     move 0 to d-flag.

           PERFORM A010-OPEN-FILES.
           PERFORM A020-READ-FILES
               UNTIL
                   (END-OF-FILES
               OR
                   VSAM-FILE-ERROR).
      *         until d-rec-count = 10.
           IF VSAM-FILE-ERROR
               NEXT SENTENCE
           ELSE
               PERFORM B000-CREATE-REPORT
               PERFORM A030-CLOSE-FILES.
       A999.
           STOP RUN.
      ******************************************************************
      ******************************************************************

      *****    performed routines in main section                   ****
       A010-OPEN-FILES.
           OPEN INPUT  DXLAD LADSUM REPTXT REGION LOCAUTHDIST.
           OPEN OUTPUT PRINT-LINE.

           PERFORM C010-SET-VARIABLES.

           PERFORM F010-READ-REGION
               UNTIL EOF-REGION.

      *     PERFORM D010-SHOW-REGION.
           PERFORM F020-READ-DXLAD.
           PERFORM F030-READ-LADSUM.
      *****
       A020-READ-FILES.
           IF DX-LAD < LD-LAD
               PERFORM F020-READ-DXLAD
                   UNTIL DX-LAD >= LD-LAD
                   OR    END-OF-FILES.
           IF DX-LAD > LD-LAD
               PERFORM F030-READ-LADSUM
                   UNTIL DX-LAD <= LD-LAD
                   OR    END-OF-FILES.

           IF END-OF-FILES
               NEXT SENTENCE
           ELSE
               IF DX-LAD = LD-LAD
                  PERFORM C020-CREATE-TABLE
                  PERFORM F020-READ-DXLAD.
      *****
       A030-CLOSE-FILES.
           CLOSE DXLAD LADSUM REPTXT REGION LOCAUTHDIST PRINT-LINE.

      ******************************************************************
      *****    B section - create report from W-COMPOSITE-DATA
      ******************************************************************
       B000-CREATE-REPORT SECTION.    
       B010-SORT-ALL.
           MOVE WC-SUB TO WC-NUM-ENTRIES.

      * DB011-DEBUG.
      *     if debug-on
      *     perform D010-SHOW-TABLES.
      *     perform A030-CLOSE-FILES.
      *     go to A999. 

      *****    sort 1 : all records                                 ****
           SORT W-CD DESCENDING WC-DC-RATE.

       B020-TABLE-1.    
      *****    report table 1 : all LADs, top 10                    ****
           MOVE W-RTAB1 to W-PRINT-REC.
           PERFORM F050-WRITE-PRINT-LINE.           
           MOVE 1 TO WC-SUB WP-COUNT.
           PERFORM UNTIL WC-SUB > 10
               PERFORM C030-SEARCH-REGION-TABLE
               PERFORM C040-PRINT-LAD
               ADD 1 TO WRG-A-TOP(W-REG-SUB) WC-SUB WP-COUNT
           END-PERFORM.
           MOVE SPACE TO W-PRINT-REC.
           PERFORM F050-WRITE-PRINT-LINE.

       B020-TABLE-2.
      *****    report table 2: all LADs, bottom 10 (reverse order)  ****
           MOVE W-RTAB2 to W-PRINT-REC.
           PERFORM F050-WRITE-PRINT-LINE.
           COMPUTE WC-SUB = WC-NUM-ENTRIES - 1.
           MOVE 1 TO WP-COUNT.
           PERFORM UNTIL WC-SUB = (WC-NUM-ENTRIES - 11)
               PERFORM C030-SEARCH-REGION-TABLE
               PERFORM C040-PRINT-LAD
               ADD 1 TO WRG-A-BTM(W-REG-SUB) WP-COUNT
               SUBTRACT 1 FROM WC-SUB
           END-PERFORM.
           MOVE SPACE TO W-PRINT-REC.
           PERFORM F050-WRITE-PRINT-LINE.

       B030-SORT-2.       
      *****    sort 2 : metro/non-metro                             ****
           SORT W-CD DESCENDING WC-MNM-CODE WC-DC-RATE.
      *****    tab  2a : find last mnm-code = 1                     ****
           MOVE 1 TO WC-SUB.
           PERFORM UNTIL WC-MNM-CODE(WC-SUB) = 1
               ADD 1 TO WC-SUB
           END-PERFORM.
           COMPUTE WC-FIRST-METRO = WC-SUB.

       B030-TABLE-3.
      *****    tab 2b :  non-metropolitan counties                  ****
      *****    report table 3: non-metro, top 10                    ****
           MOVE W-RTAB3 to W-PRINT-REC.
           PERFORM F050-WRITE-PRINT-LINE.
           MOVE 1 TO WC-SUB WP-COUNT.
           PERFORM UNTIL WC-SUB > 10
               PERFORM C030-SEARCH-REGION-TABLE
               PERFORM C040-PRINT-LAD
               ADD 1 TO WRG-N-TOP(W-REG-SUB) WC-SUB WP-COUNT
           END-PERFORM.
           MOVE SPACE TO W-PRINT-REC.
           PERFORM F050-WRITE-PRINT-LINE.

       B030-TABLE-4.
      *****    report table 4: non-metro, bottom 10 (reverse order) ****
           MOVE W-RTAB4 to W-PRINT-REC.

           PERFORM F050-WRITE-PRINT-LINE.           
           MOVE 1 TO WP-COUNT.
           COMPUTE WC-SUB = WC-FIRST-METRO - 1.
           PERFORM UNTIL WC-SUB = WC-FIRST-METRO - 11
               PERFORM C030-SEARCH-REGION-TABLE
               PERFORM C040-PRINT-LAD
               ADD 1 TO WRG-N-BTM(W-REG-SUB) WP-COUNT
               SUBTRACT 1 FROM WC-SUB
           END-PERFORM.
           MOVE SPACE TO W-PRINT-REC.
           PERFORM F050-WRITE-PRINT-LINE.

       B030-TABLE-5.
      *****    force page throw                                     ****
           MOVE 2 TO W-PAGE-IND.
      *****    tab 2c : metropolitan councils                       ****
      *****    report table 5: metro, top 10                        ****
           MOVE W-RTAB5 to W-PRINT-REC.
           PERFORM F050-WRITE-PRINT-LINE.

           MOVE 1 TO WP-COUNT.
           COMPUTE WC-SUB  =  WC-FIRST-METRO.
           PERFORM UNTIL WC-SUB = WC-FIRST-METRO + 10
               PERFORM C030-SEARCH-REGION-TABLE
               PERFORM C040-PRINT-LAD
               ADD 1 TO WRG-M-TOP(W-REG-SUB) WC-SUB WP-COUNT
           END-PERFORM.
           MOVE SPACE TO W-PRINT-REC.
           PERFORM F050-WRITE-PRINT-LINE.

       B030-TABLE-6.
      *****    report table 6: metro, bottom 10 (reverse order)     ****
           MOVE W-RTAB6 to W-PRINT-REC.
           PERFORM F050-WRITE-PRINT-LINE.

           MOVE 1 TO WP-COUNT.
           COMPUTE WC-SUB = WC-NUM-ENTRIES - 1. 
           PERFORM UNTIL WC-SUB = WC-NUM-ENTRIES - 11
               PERFORM C030-SEARCH-REGION-TABLE
      *         PERFORM D010-SHOW-DEBUG
               PERFORM C040-PRINT-LAD
               ADD 1 TO WRG-M-BTM(W-REG-SUB) WP-COUNT
               SUBTRACT 1 FROM WC-SUB
           END-PERFORM.
           MOVE SPACE TO W-PRINT-REC.
           PERFORM F050-WRITE-PRINT-LINE.           

       B040-TABLE-7.
      *****    tab 3 : by region                                    ****
      *****    report table 7: LAD summary by region                ****
           MOVE 1 TO WC-SUB WP-REGION-FLAG.
           PERFORM UNTIL WC-SUB = WC-NUM-ENTRIES
               PERFORM C030-SEARCH-REGION-TABLE
               ADD 1 TO WRG-NUM(W-REG-SUB)
               ADD 1 TO WC-SUB
           END-PERFORM.

           MOVE 1 TO W-REG-SUB .
           PERFORM UNTIL W-REG-SUB  > 9
               PERFORM C050-PRINT-REGION
               ADD 1 TO W-REG-SUB
           END-PERFORM.
           MOVE SPACE TO W-PRINT-REC.
           PERFORM F050-WRITE-PRINT-LINE.
           MOVE "* London: no datasets available" to W-PRINT-REC.
           PERFORM F050-WRITE-PRINT-LINE.           

       B050-LAST-PAGE.    
      *****    print report final page from REPTXT                  ****
           MOVE 2 TO W-PAGE-IND. 
           PERFORM F070-PRINT-REPTXT
               UNTIL EOF-REPTXT.
           MOVE W-PROGNAME TO WH4-PROG.
           MOVE W-H4       TO W-PRINT-REC.
           PERFORM F050-WRITE-PRINT-LINE.

      ******************************************************************
      *****    C section: set variables,                            ****
      *****               create table W-COMPOSITE-DATA,            ****
      *****               search REGION TABLE                       ****
      *****               print LAD entries                         ****
      *****               print REGION summary entries              ****
      ******************************************************************
       C000 SECTION.
       C010-SET-VARIABLES.
           MOVE 0 TO W-EOF-FLAGS W-EOF-REG-FLAG
                     W-LAD-ERROR W-PAGE-IND 
                     WP-REGION-FLAG W-EOF-RPTXT-FLAG.
           ADD  1 TO W-LAD-FS.

           MOVE W-PROG-ID TO WH1-PROG-ID.

           ACCEPT W-REFMOD-TIME-ITEM       FROM TIME.
           MOVE W-REFMOD-TIME-ITEM (1:2)   TO WH1-T1.
           MOVE W-REFMOD-TIME-ITEM (3:2)   TO WH1-T2.
           MOVE FUNCTION CURRENT-DATE(1:8) TO W-YYYYMMDD.
           MOVE W-YYYYMMDD (7:2)           TO WH1-DD.
           MOVE W-YYYYMMDD (5:2)           TO WH1-DM.
           MOVE W-YYYYMMDD (1:4)           TO WH1-DY.
      *****
       C020-CREATE-TABLE.
           MOVE DX-LAD TO L-LAD.
           PERFORM F040-READ-LOCAUTHDIST.

           MOVE DX-MNM-CODE TO WC-MNM-CODE(WC-SUB).
           MOVE L-LAD       TO WC-LAD(WC-SUB).
           MOVE L-LAD-NAME  TO WC-LAD-NAME(WC-SUB).
           MOVE L-REGION    TO WC-REGION(WC-SUB).
           MOVE DX-D-C      TO WC-DC(WC-SUB).
           MOVE DX-DC-RATE  TO WC-DC-RATE(WC-SUB).
           MOVE LD-MDL-RANK TO WC-MDL-RANK(WC-SUB).
           MOVE LD-EXT-RANK TO WC-EXT-RANK(WC-SUB).
           MOVE LD-LC-RANK  TO WC-LC-RANK(WC-SUB).
           ADD 1            TO WC-SUB.
      *****
       C030-SEARCH-REGION-TABLE.
      *****    ie. LAD x is part of REG y                           ****
           PERFORM VARYING W-REG-SUB FROM 1 BY 1
               UNTIL WC-REGION(WC-SUB) = W-REG(W-REG-SUB)
           END-PERFORM.
           ADD 1 TO WRG-NUM(W-REG-SUB).
      *****
       C040-PRINT-LAD.
           MOVE WP-COUNT            TO WP-L-COUNT.
           MOVE WC-LAD-NAME(WC-SUB) TO WP-LAD-NAME.
           MOVE WC-DC(WC-SUB)       TO WP-DC.
           MOVE WC-DC-RATE(WC-SUB)  TO WP-DC-RATE.
           MOVE WC-MDL-RANK(WC-SUB) TO WP-MDL-RANK.
           MOVE WC-EXT-RANK(WC-SUB) TO WP-EXT-RANK.
           MOVE WC-LC-RANK(WC-SUB)  TO WP-LC-RANK.
           MOVE W-PRINT-LAD         TO W-PRINT-REC.
           PERFORM F050-WRITE-PRINT-LINE.

       C050-PRINT-REGION.
           IF PRINT-REGION           
               MOVE 2 TO W-PAGE-IND.
           MOVE WRG-NAME(W-REG-SUB)  TO WPR-NAME.
           MOVE WRG-NUM(W-REG-SUB)   TO WPR-NUM.
           MOVE WRG-A-TOP(W-REG-SUB) TO WPR-A-TOP.
           MOVE WRG-A-BTM(W-REG-SUB) TO WPR-A-BTM.
           MOVE WRG-M-TOP(W-REG-SUB) TO WPR-M-TOP.
           MOVE WRG-M-BTM(W-REG-SUB) TO WPR-M-BTM.
           MOVE WRG-N-TOP(W-REG-SUB) TO WPR-N-TOP.
           MOVE WRG-N-BTM(W-REG-SUB) TO WPR-N-BTM.
           MOVE W-PRINT-REGION       TO W-PRINT-REC.
           PERFORM F050-WRITE-PRINT-LINE.


      ******************************************************************
      * D000-DEBUG SECTION.
      * D010-SHOW-REGION.

      *     display W-REG-REC (1).
      *     display W-REG-REC (2).
      *     display W-REG-REC (3).
      *     display W-REG-REC (4).
      *     display W-REG-REC (5).
      *     display W-REG-REC (6).
      *     display W-REG-REC (7).
      *     display W-REG-REC (8).
      *     display W-REG-REC (9).

      *     display " wc-sub = "    WC-SUB  
      *             " wp-count = "  WP-COUNT
      *             " w-reg-sub = " W-REG-SUB
      *             " w-reg = "     W-REG(W-REG-SUB)
      *             " wrg-num = "   WRG-NUM(W-REG-SUB)
      *             " wrg-name = "  WRG-NAME(W-REG-SUB)               
      *     .
      
      * D020-SHOW-TABLES.
      *     DISPLAY " ".
      *     SORT W-CD DESCENDING WC-DC-RATE.
      *     PERFORM VARYING WC-SUB FROM 1 BY 1
      *         UNTIL WC-SUB = WC-NUM-ENTRIES 
      *             DISPLAY W-CD (WC-SUB)
      *     END-PERFORM.
      *     DISPLAY " ".
      *     SORT W-CD DESCENDING WC-MNM-CODE WC-DC-RATE.
      *     PERFORM VARYING WC-SUB FROM 1 BY 1
      *         UNTIL WC-SUB = WC-NUM-ENTRIES 
      *             DISPLAY W-CD (WC-SUB)
      *     END-PERFORM.


      ******************************************************************
      *****    read files and print-related paragraphs              ****
      ******************************************************************
       F000-FILES SECTION.

       F010-READ-REGION.
           READ REGION AT END
               MOVE 1 TO  W-EOF-REG-FLAG
           NOT AT END
               MOVE REGN        TO W-REG(W-REG-SUB)
               MOVE RG-NAME     TO WRG-NAME(W-REG-SUB)
               ADD  1           TO W-REG-SUB
           END-READ.

       F020-READ-DXLAD.
           READ DXLAD
               AT END MOVE 1 TO W-EOF-FLAGS.

       F030-READ-LADSUM.
           READ LADSUM
               AT END MOVE 1 TO W-EOF-FLAGS.

       F040-READ-LOCAUTHDIST.
           READ LOCAUTHDIST RECORD
               KEY IS L-LAD
               INVALID KEY
                   DISPLAY "INVALID KEY ON GCLADVSM = " L-LAD
                   DISPLAY "ERROR CODE = " W-LAD-FS
                   MOVE 1 TO W-LAD-ERROR
           END-READ.
      *****    print-related paragraphs                             ****
       F050-WRITE-PRINT-LINE.
           IF TOP-OF-PAGE OR FORCE-PAGE-THROW
               MOVE W-PRINT-REC TO W-PR-STORE
               PERFORM F060-PRINT-HEADERS
               MOVE W-PR-STORE TO W-PRINT-REC
               MOVE 1 TO W-PAGE-IND.
           WRITE PRINT-REC FROM W-PRINT-REC
               AT END-OF-PAGE
               PERFORM F060-PRINT-HEADERS
           END-WRITE.

       F060-PRINT-HEADERS.
           ADD 1 TO W-PAGENO.
           MOVE W-PAGENO TO WH2-PAGENO.
           IF PRINT-REGION
               PERFORM F065-PRINT-REGION-HEADERS
           ELSE
               MOVE W-H1 TO W-PRINT-REC
               IF TOP-OF-PAGE
                   WRITE PRINT-REC FROM W-PRINT-REC
               ELSE
                   WRITE PRINT-REC FROM W-PRINT-REC
                       AFTER ADVANCING PAGE
               END-IF
      *****     if printing report text (last page, omit H2-H3)     ****
               MOVE W-H2       TO W-PRINT-REC
               WRITE PRINT-REC FROM W-PRINT-REC
               MOVE W-H3       TO W-PRINT-REC
               WRITE PRINT-REC FROM W-PRINT-REC
               MOVE W-H4       TO W-PRINT-REC
               WRITE PRINT-REC FROM W-PRINT-REC
               MOVE SPACES     TO W-PRINT-REC
               WRITE PRINT-REC FROM W-PRINT-REC.

       F065-PRINT-REGION-HEADERS.
               MOVE 0 TO WP-REGION-FLAG.
               MOVE 1 TO W-PAGE-IND.
               MOVE W-H1 TO W-PRINT-REC.
               WRITE PRINT-REC FROM W-PRINT-REC
                   AFTER ADVANCING PAGE.
               MOVE W-H2       TO W-PRINT-REC.
               WRITE PRINT-REC FROM W-PRINT-REC.
               MOVE W-H5       TO W-PRINT-REC.
               WRITE PRINT-REC FROM W-PRINT-REC.
               MOVE W-H6       TO W-PRINT-REC.
               WRITE PRINT-REC FROM W-PRINT-REC.
               MOVE W-H4       TO W-PRINT-REC.
               WRITE PRINT-REC FROM W-PRINT-REC.                   
               MOVE SPACES     TO W-PRINT-REC.
               WRITE PRINT-REC FROM W-PRINT-REC.

       F070-PRINT-REPTXT.
           READ REPTXT
           AT END
               MOVE 2 TO  W-EOF-RPTXT-FLAG
           NOT AT END
               IF FORCE-PAGE-THROW 
                  ADD 1 TO W-PAGENO
                  MOVE W-PAGENO TO WH2-PAGENO
                   MOVE W-H1 TO W-PRINT-REC
                   WRITE PRINT-REC FROM W-PRINT-REC
                       AFTER ADVANCING PAGE
                   MOVE W-H2       TO W-PRINT-REC
                   WRITE PRINT-REC FROM W-PRINT-REC
                   MOVE W-H4       TO W-PRINT-REC
                   WRITE PRINT-REC FROM W-PRINT-REC
                   MOVE SPACES     TO W-PRINT-REC
                   WRITE PRINT-REC FROM W-PRINT-REC                   
                   MOVE 1 TO W-PAGE-IND                          
               END-IF 
               MOVE REPORT-TEXT TO W-PRINT-REC
               PERFORM F050-WRITE-PRINT-LINE
           END-READ.

