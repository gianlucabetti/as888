000100021211      *PARMS CLOSQLCSR(*ENDMOD)
000200021211      *PARMS DYNUSRPRF(*OWNER)
000301101021      * ------------------------------------------------------------------
000400101021      *  Pulizia file FIVABWKF
000600021211      * ------------------------------------------------------------------
001500021211     d KPJBA         e ds                                                       *trasmissione
001801101021
001802101021     d wrkgetlista     s           4096    varying
001900001018      *--------------------------------------------------------------------*
002000001018      *-                       M A I N    L I N E                         -*
002100001018      *--------------------------------------------------------------------*
002200021211      * Operazioni iniziali
002300001018      *
002400021211     C     *ENTRY        PLIST
002500021211     C                   PARM                    KPJBA
002501101021     c                   movel     kpjbu         datapuliz         8 0
004200021211      *
004300101021      * Prepara es esegue stringa SQL
004400001018     C                   EXSR      SetSql
004500021211      *
006400021211     c                   SETON                                        LR
006401101021     c
006402101021      /free
006403101021
006800101021        // ------------------------------------------------------------------*
007000101021        BEGSR SetSql            ;
007001101021
007002101021         wrkgetlista='DELETE from FIVABWKF WHERE vabaas= ' +
007003101021              %subst(%editc(datapuliz:'X'):1:4) + ' and   vabmgs< ' +
007004101021              %subst(%editc(datapuliz:'X'):5:4) + ' and vabaas>0 ' ;
007100101021
007101101021         exec sql   execute immediate :wrkgetlista   ;
007102101022
007103101022         wrkgetlista='DELETE from FIVABWKF WHERE vabaas< ' +
007104101022              %subst(%editc(datapuliz:'X'):1:4) + ' and vabaas>0 ' ;
007106101022
007107101022         exec sql   execute immediate :wrkgetlista   ;
007108101022
007109101022       // pulisco quelli senza data
007110101022       if datapuliz>20101101  ;
007111101022         wrkgetlista='DELETE from FIVABWKF WHERE vabaas=0 and vabmgs=0';
007112101022
007113101022         exec sql   execute immediate :wrkgetlista   ;
007114101022       endif  ;
007115101021
008600101021       ENDSR  ;
008601101021      /end-free
