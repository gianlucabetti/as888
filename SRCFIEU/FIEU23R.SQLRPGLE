000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200011206     F*          STATISTICA CONSUMI                        *
000300150519     F* SCRIVO da DANFOS a WFDANF0F                        *
000301150519     d kpjba         e ds
005200020607     C*---------------------------------------------------------------*
005201150519     c     *entry        plist
005202150519     c                   parm                    kpjba
005203150519     c                   movel     kpjbu         datfat            8 0
005204150519     C                   SETON                                        LR
005702021211     C/EXEC SQL
005703150519     C+ INSERT INTO WFDANF0F (PARCEL, KEY1, SERVICECOD, SCAN,
005709150519     C+ COUNTRY, DATA_00001) SELECT '0' concat digits(F1), digits(F2),
005710150519     C+ digits(F3), digits(F4), digits(F5), :datfat FROM DANFOS
005723021211     C/END-EXEC
007000021211     C                   IF        sqlcod < 0
007001150519     C                   exsr      fine
007600021211     C                   end
007601150519     c* elimino le riche vuote
007602150519     C/EXEC SQL
007603150519     C+ delete  WFDANF0F where parcel <'01'
007604150519     C/END-EXEC
007605150519     C                   IF        sqlcod < 0
007606150519     C                   exsr      fine
007609150519     C                   end
007610150519     c* il parcel nel excel è numerico per cui perde lo 0 iniziale
007611150519     C/EXEC SQL
007612150519     C+ UPDATE WFDANF0F SET data_fat=:datfat
007613150519     C+ WHERE parcel in ( SELECT a.parcel FROM tnefr00f
007614150519     C+ join wfdanf0f a on efrrif = a.parcel where efrdft =
007615150519     C+ :datfat and data_fat =0 )
007617150519     C/END-EXEC
007618150519     C                   IF        sqlcod < 0
007619150519     C                   exsr      fine
007620150519     C                   end
007621150519     c     fine          begsr
007622150519     C                   call      'X66CHGJOB'
007623150519     c                   seton                                        h1
007800150519     c                   endsr
