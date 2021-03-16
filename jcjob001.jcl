//**********************************************************************
//****    JCL for MTM Grand Challenge: create (alloc) all PDS       ****
//****    data sets: .GCINPUT AND .GCWORK                           ****
//****                                                              ****
//****    N.B. data setS .GCCSVIN and .GCSVIN.LOCNCSV pre-exist     ****
//**********************************************************************
//GS0001    JOB  1,NOTIFY=&SYSUID
//****      GS005                                                   ****
//STEP01    EXEC PGM=IEFBR14
//DD01      DD DSN=GCINPUT,
//             DISP=(NEW,CATLG,DELETE),
//             SPACE=(TRK,(1,1)),
//             DCB=(LRECL=80,DSORG=PO,RECFM=FB),
//             DSNTYPE=LIBRARY,
//             VOL=SER=(VPWRKZ)
//DD02      DD DSN=GCWORK,
//             DISP=(NEW,CATLG,DELETE),
//             SPACE=(TRK,(1,1)),
//             DCB=(LRECL=80,DSORG=PO,RECFM=FB),
//             DSNTYPE=LIBRARY,
//             VOL=SER=(VPWRKZ)
//**********************************************************************
//****    use DFSORT to unpack (from .csv) and extract files from   ****
//****    raw dataset containing reference data: LSOA,              ****
//****    LAD and Region; AND LAD indexes and Covid death rates     ****
//**********************************************************************
//****    GS0010                                                    ****
//STEP02  EXEC PGM=SORT
//SYSOUT  DD   SYSOUT=*
//SORTIN  DD   DSNAME=GCCSVIN.LOCNCSV,DISP=SHR
//SORTOUT DD   DSNAME=GCWORK(GCLAD),DISP=SHR
//SYSIN   DD   *
  OPTION COPY
  OUTREC PARSE=(%01=(ENDBEFR=C',',FIXLEN=04),
    %02=(ENDBEFR=C',',FIXLEN=09),
    %03=(ENDBEFR=C',',FIXLEN=30,PAIR=QUOTE),
    %04=(ENDBEFR=C',',FIXLEN=09),
    %05=(ENDBEFR=C',',FIXLEN=30,PAIR=QUOTE),
    %06=(ENDBEFR=C',',FIXLEN=09),
    %07=(ENDBEFR=C',',FIXLEN=30,PAIR=QUOTE),
    %08=(ENDBEFR=C',',FIXLEN=09),
    %09=(FIXLEN=30,PAIR=QUOTE)),
    BUILD=(%04,%05,%08,80:X)
/*
//STEP03  EXEC PGM=SORT
//SYSOUT  DD SYSOUT=*
//SORTIN  DD DSNAME=GCCSVIN.LOCNCSV,DISP=SHR
//SORTOUT DD DSNAME=GCINPUT(GCREGION),DISP=SHR
//SYSIN   DD *
  OPTION COPY
  OUTREC PARSE=(%01=(ENDBEFR=C',',FIXLEN=04),
    %02=(ENDBEFR=C',',FIXLEN=09),
    %03=(ENDBEFR=C',',FIXLEN=30,PAIR=QUOTE),
    %04=(ENDBEFR=C',',FIXLEN=09),
    %05=(ENDBEFR=C',',FIXLEN=30,PAIR=QUOTE),
    %06=(ENDBEFR=C',',FIXLEN=09),
    %07=(ENDBEFR=C',',FIXLEN=30,PAIR=QUOTE),
    %08=(ENDBEFR=C',',FIXLEN=09),
    %09=(FIXLEN=30,PAIR=QUOTE)),
    BUILD=(%08,%09,80:X)
/*
//STEP04    EXEC PGM=SORT
//SYSOUT    DD SYSOUT=*
//SORTIN    DD DSNAME=GCWORK(GCLAD),DISP=SHR
//SORTOUT   DD DSNAME=GCWORK(GCLAD),DISP=SHR
//SYSIN     DD *
  SORT FIELDS=(1,9,CH,A)
  SUM FIELDS=NONE
/*
//STEP05    EXEC PGM=SORT
//SYSOUT    DD SYSOUT=*
//SORTIN    DD DSNAME=GCINPUT(GCREGION),DISP=SHR
//SORTOUT   DD DSNAME=GCINPUT(GCREGION),DISP=SHR
//SYSIN     DD *
  SORT FIELDS=(1,9,CH,A)
  SUM FIELDS=NONE
/*
//****    GC0020                                                    ****
//STEP06  EXEC PGM=SORT
//SYSOUT  DD   SYSOUT=*
//SORTIN  DD   DSNAME=GCCSVIN(GCRTCSV),DISP=SHR
//SORTOUT DD   DSNAME=GCINPUT(GCREPTXT),DISP=SHR
//SYSIN   DD   *
  OPTION COPY
/*
//****    GS0040                                                    ****
//STEP07  EXEC PGM=SORT
//SYSOUT  DD   SYSOUT=*
//SORTIN  DD   DSNAME=GCCSVIN(GCDXCSV),DISP=SHR
//SORTOUT DD   DSNAME=GCINPUT(GCDXLAD),DISP=SHR
//SYSIN   DD   *
  OPTION COPY
  OUTREC PARSE=(%01=(ENDBEFR=C',',FIXLEN=01),
    %02=(ENDBEFR=C',',FIXLEN=09),
    %03=(ENDBEFR=C',',FIXLEN=04),
    %04=(ENDBEFR=C',',FIXLEN=07),
    %05=(ENDBEFR=C',',FIXLEN=07),
    %06=(ENDBEFR=C',',FIXLEN=07),
    %07=(ENDBEFR=C',',FIXLEN=04),
    %08=(ENDBEFR=C',',FIXLEN=07),
    %09=(ENDBEFR=C',',FIXLEN=07),
    %10=(FIXLEN=07)),
    BUILD=(%01,%02,%03,%04,%05,%06,%07,%08,%09,%10,80:X)
/*
//STEP08  EXEC PGM=SORT
//SYSOUT  DD SYSOUT=*
//SORTIN  DD DSNAME=GCINPUT(GCDXLAD),DISP=SHR
//SORTOUT DD DSNAME=GCINPUT(GCDXLAD),DISP=SHR
//SYSIN   DD *
  SORT FIELDS=(2,9,CH,A)
  SUM FIELDS=NONE
/*
//STEP09  EXEC PGM=SORT
//SYSOUT  DD SYSOUT=*
//SORTIN  DD DSNAME=GCCSVIN(GCLDSMCV),DISP=SHR
//SORTOUT DD DSNAME=GCINPUT(GCLADSUM),DISP=SHR
//SYSIN   DD *
  OPTION COPY
  OUTREC PARSE=(%01=(ENDBEFR=C',',FIXLEN=09),
    %02=(ENDBEFR=C',',FIXLEN=08),
    %03=(ENDBEFR=C',',FIXLEN=03),
    %04=(ENDBEFR=C',',FIXLEN=06),
    %05=(ENDBEFR=C',',FIXLEN=03),
    %06=(ENDBEFR=C',',FIXLEN=06),
    %07=(ENDBEFR=C',',FIXLEN=03),
    %08=(ENDBEFR=C',',FIXLEN=06),
    %09=(ENDBEFR=C',',FIXLEN=03),
    %10=(ENDBEFR=C',',FIXLEN=08),
    %11=(FIXLEN=03)),
    BUILD=(%01,%02,%03,%04,%05,%06,%07,%08,%09,%10,%11,80:X)
/*
//STEP10  EXEC PGM=SORT
//SYSOUT  DD SYSOUT=*
//SORTIN  DD DSNAME=GCINPUT(GCLADSUM),DISP=SHR
//SORTOUT DD DSNAME=GCINPUT(GCLADSUM),DISP=SHR
//SYSIN   DD *
  SORT FIELDS=(1,9,CH,A)
  SUM FIELDS=NONE
/*
//**********************************************************************
//****    create VSM file GCLADVSM from GCWORK(GCLAD)               ****
//****     - created in step GS0010                                 ****
//**********************************************************************
//****    GS0030                                                    ****
//STEP11   EXEC PGM=IDCAMS
//SYSPRINT DD  SYSOUT=*
//SYSIN    DD  *
   DEFINE CLUSTER (NAME(GCLADVSM)   -
   VOLUMES(VPWRKE)                         -
   INDEXED                                 -
   RECSZ(80 80)                            -
   TRACKS(1,1)                             -
   KEYS(9  0)                              -
   FREESPACE(3 3) )                        -
   DATA (NAME(GCLADVSM.DATA))       -
   INDEX (NAME(GCLADVSM.INDEX))
/*
//STEP12   EXEC PGM=IDCAMS
//INPUT    DD DSNAME=GCWORK(GCLAD),DISP=SHR
//OUTPUT   DD DSNAME=GCLADVSM,SPACE=(TRK,(1,1)),DISP=OLD
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  REPRO -
    INFILE(INPUT) -
    OUTFILE(OUTPUT)
  PRINT -
    INDATASET(GCLADVSM) -
    CHARACTER
/*
//**********************************************************************
//****    compile and run Cobol program GC0050: create and print
//****    report of covid-19 deaths by indices of social deprivation
//**********************************************************************
//GC0050     EXEC IGYWCL
//****       STEP13                                                 ****
//COBOL.SYSIN  DD DSN=&SYSUID..SOURCE(GC0050),DISP=SHR
//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(GC0050),DISP=SHR
//***************************************************/
// IF RC = 0 THEN
//***************************************************/
//RUN        EXEC PGM=GC0050
//STEPLIB    DD   DSN=&SYSUID..LOAD,DISP=SHR
//GO.CEEOPTS DD *
    DEBUG
//GCDXLAD    DD   DSN=GCINPUT(GCDXLAD),DISP=SHR
//GCLADSUM   DD   DSN=GCINPUT(GCLADSUM),DISP=SHR
//GCREPTXT   DD   DSN=GCINPUT(GCREPTXT),DISP=SHR
//GCREGION   DD   DSN=GCINPUT(GCREGION),DISP=SHR
//GCLADVSM   DD   DSN=GCLADVSM,DISP=OLD
//PRTDONE    DD   SYSOUT=*,OUTLIM=15000
//PRTLINE    DD   SYSOUT=*,OUTLIM=15000
//SYSOUT     DD   SYSOUT=*,OUTLIM=15000
//CEEDUMP    DD   DUMMY
//SYSUDUMP   DD   DUMMY
//***************************************************/
// ELSE
// ENDIF
