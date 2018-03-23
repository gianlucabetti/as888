000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200011206     F*          STATISTICA CONSUMI                        *
000300150519     F* SCRIVO da DANFOS a WFDANF0F                        *
005200020607     C*---------------------------------------------------------------*
005702021211     C/EXEC SQL
005703150519     C+ INSERT INTO GAITRAFIL/WFDANF0F (PARCEL, KEY1, SERVICECOD, SCAN,
005709150519     C+ COUNTRY, DATA_00001) SELECT * FROM DANFOS
005723021211     C/END-EXEC
005724021211     C*
007000021211     C                   IF        sqlcod < 0
007001021211     C                   call      'X66CHGJOB'
007002030113     c                   seton                                        h1
007600021211     C                   end
007700021211     C                   SETON                                        LR
