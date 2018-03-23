000100950523     H DECEDIT('0,') DATEDIT(*DMY.)
000200070108     h dftactgrp(*no) actgrp(*caller)
000300070115      * Fidg05R *----------------------------------------------------*
000400120712      *         - scrive FNFVV gestione parallelo distinte
000500070115      *--------------------------------------------------------------*
000600070108     FFNFVV01L  UF A E           K DISK    commit
000700070108     FFiapd01L  iF   E           K DISK
000800070410     FFIDST09L  iF   E           K DISK    prefix(£)
000900070108     d wnpg            s                   like(fvvnpg)
001000070108     d wnfv            s                   like(fvvnfv)
001100070108     d DDatiUte      e ds
001200070108     d TIBS34ds      e ds
001300070108     d AZUTEds       e ds                  extname(AZUTE00F)
001400070108     d kpjba         e ds
001500160502     d ddstflr       e ds
001501160502     d dfvvspn       e ds
001600070108     d fidg05ds      e ds
001700070108     d fvvds         e ds                  extname(fnfvv00f)
001800070108     d fvvds2        e ds                  extname(fnfvv00f) prefix($)
001900950523     C*
002000061229     c                   select
002100061229     C* ANNULLAMENTO  FOGLIO DISTINTA
002200061229    1C     d05cmd        wheneq    'KQ'
002300950524     C                   EXSR      ANNDIS
002400061229     C* RIATTIVAZIONE FOGLIO DISTINTA
002500061229    1C     d05cmd        wheneq    'KE'
002600061229     C                   EXSR      ritDIS
002700061229     C* AGGIORNA distinta
002800061229    1C     d05cmd        wheneq    'KF'
002900061229     C                   EXSR      AGGD02
003000061229    1C                   ENDsl
003100070108     c* commit se richiesto
003200070108     c                   if        d05com = '1'
003300070108     c                   commit
003400070108     c                   end
003500070108     c                   exsr      srfine
003600950524     C*
003700070108     c**********************************************************************
003800950524     C*--- ANNULLAMENTO FOLGIO DISTINTA ------------------------------*
003900070108     c**********************************************************************
004000950524     C     ANNDIS        BEGSR
004100950524     C*
004200950525     C* ANNULLO IL RECORD IN FNFVV00F
004300070108     C                   Z-ADD     4             WNPG
004400070314     C     KFVV          CHAIN(e)  FNFVV01l
004500070314     c                   if        %error
004600070314     c                   eval      d05esi = '1'
004700070314     c                   exsr      srfine
004800070314     c                   end
004900070314     c                   if        %found(fnfvv01l)
005000950525     C                   MOVE      '*'           FVVATB
005100070108     C                   update    fnfvv000
005200070108     c                   else
005300070108     c                   eval      d05esi = '1'
005400070108     c                   exsr      srfine
005500070108     c                   end
005600000515     C* ANNULLO IL RECORD IN FNFVV00F DI TIPO 8
005700000515     C                   Z-ADD     8             WNPG
005800070314     C     KFVV          CHAIN(e)  FNFVV000
005900070314     c                   if        %error
006000070314     c                   eval      d05esi = '1'
006100070314     c                   exsr      srfine
006200070314     c                   end
006300070108     c                   if        %found(fnfvv01l)
006400070108     C                   MOVE      '*'           FVVATB
006500070108     C                   update    fnfvv000
006600070108     c                   else
006700070108     c                   eval      d05esi = '1'
006800070108     c                   exsr      srfine
006900070108     c                   end
007000950524     C*
007100950524     C                   ENDSR
007200061229     c**********************************************************************
007300120712     C* RIPRISTINO IL RECORD IN FNFVV00F
007400061229     c**********************************************************************
007500070108     C     ritDIS        BEGSR
007600061229     C* RIPRISTINO IL RECORD IN FNFVV00F
007700070223     C                   Z-ADD     4             WNPG
007800070314     C     KFVV          CHAIN(e)  FNFVV000
007900070314     c                   if        %error
008000070314     c                   eval      d05esi = '1'
008100070314     c                   exsr      srfine
008200070314     c                   end
008300070108     c                   if        %found(fnfvv01l)
008400061229     C                   CLEAR                   FVVATB
008500070108     C                   update    fnfvv000
008600070108     c                   else
008700070108     c                   eval      d05esi = '1'
008800070108     c                   exsr      srfine
008900070108     c                   end
009000061229     C* RIPRISTINO IL RECORD IN FNFVV00F DI TIPO 8
009100061229     C                   Z-ADD     8             WNPG
009200070314     C     KFVV          CHAIN(e)  FNFVV000                           28
009300070314     c                   if        %error
009400070314     c                   eval      d05esi = '1'
009500070314     c                   exsr      srfine
009600070314     c                   end
009700070108     c                   if        %found(fnfvv01l)
009800070108     C                   CLEAR                   FVVATB
009900070108     C                   update    fnfvv000
010000070108     c                   else
010100070108     c                   eval      d05esi = '1'
010200070108     c                   exsr      srfine
010300070108     c                   end
010400061229     C*
010500061229     C                   ENDSR
010600061229     C**********************************************************************
010700950525     C*--- AGGIORNAMENTO ---------------------------------------------*
010800061229     C**********************************************************************
010900950525     C     AGGD02        BEGSR
011000070108     C                   MOVE      4             WNPG
011100070410     C     Kfidst        CHAIN     FIDST09l
011200070410     c                   if        not %found(FIDST09l)
011300070108     c                   eval      d05esi = '1'
011400070108     c                   exsr      srfine
011500070108     c                   end
011600070108     c     kapd          chain     fiapd01l
011700070108     c                   if        not %found(fiapd01l)
011800070108     c                   eval      d05esi = '1'
011900070108     c                   exsr      srfine
012000070108     c                   end
012001160502     c                   movel     £dstflr       ddstflr
012100950525     C* F N F V V 0 0 F
012200070108     C                   MOVE      4             WNPG
012300070314     C     KFVV          CHAIN(e)  FNFVV01l
012400070314     c                   if        %error
012500070314     c                   eval      d05esi = '1'
012600070314     c                   exsr      srfine
012700070314     c                   end
013400070108     c                   if        %found(fnfvv01l)
013500050519     c                   movel     fvvspn        dfvvspn
013600050519     c                   else
013700050519     c                   clear                   dfvvspn
013800050519     c                   end
013900950525     C                   CLEAR                   FVVATB
014000070108     C                   MOVEL     APDRSC        FVVDSF
014100070108     C                   MOVEL     £dstdfv       FVVDFV
014200070108     C                   MOVEL     £dstfpp       FVVFPP
014300070108     C                   MOVEL     £dstpep       FVVPEP
014500070419     C                   MOVEL     'S'           FVVFSC
014600070108     c                   movel     £dsttpm       §fvvtpm
014601160502     c                   movel     §dstturno     §fvvturno
014700050519     c                   movel(p)  dfvvspn       fvvspn
014800950525     C* F N F V V 0 0 F
014900070108     C                   MOVEL     £dstfgs       FVVFGS
015000950525     C                   Z-ADD     4             FVVNPG
015100950525     C                   CLEAR                   FVVSPG
015200070108     C                   MOVE      £dstnfv       FVVNFV
015300950525     C                   CLEAR                   FVVFCF
015400950525     C                   MOVEL     SIMFEL        FVVFLE
015500070108     c                   if        not %found(fnfvv01l)
015600950525     C                   WRITE     FNFVV000
015700000515      *SCRITTURA FOGLIO DI TIPO 8 RIENTRI
015800000515     C                   MOVE      8             FVVNPG
015900000515     C                   WRITE     FNFVV000
016000950525   X1C                   ELSE
016100950525     C                   UPDATE    FNFVV000
016200000515      *ALLINEAMENTO FOGLIO DI TIPO 8 (RIENTRI)
016300000515     C                   MOVE      FVVDS         FVVDS2
016400000515     C                   Z-ADD     8             WNPG
016500070314     C     KFVV          CHAIN(e)  FNFVV01l
016600070314     c                   if        %error
016700070314     c                   eval      d05esi = '1'
016800070314     c                   exsr      srfine
016900070314     c                   end
017000070108     C                   IF        %found(fnfvv01l)
017100000515     C                   MOVE      FVVDS2        FVVDS
017200000515     C                   MOVE      8             FVVNPG
017300000515     C                   UPDATE    FNFVV000
017400000626     C                   END
017500950525    1C                   ENDIF
017600070108     c*
017700070108     C                   ENDSR
017800061229     C**********************************************************************
017900070108     C     srfine        BEGSR
018000061229     C**********************************************************************
018100061229     C*
018200070108     c                   eval      kpjbu = fidg05ds
018300070108     C                   SETON                                        LR
018400070108     c                   return
018500070108     c*
018600070108     C                   ENDSR
018700070108     C**********************************************************************
018800070108     C     *INZSR        BEGSR
018900070108     C**********************************************************************
019000070108     C*
019100061229     C     *ENTRY        PLIST
019200061229     C                   PARM                    KPJBA
019300061229     C                   MOVEL     KPJBU         fidg05ds
019400061229     C*
019500070108      * Reperisco dati job
019600070108     c                   exsr      DatiJob
019700070410     C* ACCESSO FIAPD01L
019800070108     C     Kapd          KLIST
019900070122     C                   KFLD                    £dsttipa
020000070108     C                   KFLD                    £dstpdr
020100070410     C* ACCESSO FIDST09L
020200070108     C     KFidst        KLIST
020300070410     C                   KFLD                    d05FGS
020400070108     C                   KFLD                    WNPG
020500070108     C                   KFLD                    d05nfv
020600070108     C* ACCESSO FNFVV01L
020700070108     C     KFVV          KLIST
020800070108     C                   KFLD                    WNPG
020900070108     C                   KFLD                    wnfv
021000070108     C                   KFLD                    d05FGS
021100070108     c                   eval      wnfv = d05nfv
021200061229     c                   endsr
021300070108      *---------------------------------------------------------------*
021400070108      * Reperimento Dati del job (Utente/Operativi)                   *
021500070108      *---------------------------------------------------------------*
021600070108     c     DatiJob       BEGSR
021700070108      *
021800070108     c     *dtaara       define    §azute        azuteds
021900070108     c     *dtaara       define    §datiute      ddatiute
022000070108      *
022100070108     c                   in(E)     *dtaara
022200070108     c                   IF        %ERROR or RSUT = *blanks
022300070108     c                   clear                   Tibs34Ds
022400070108     c                   call      'TIBS34R'
022500070108     c                   parm                    Tibs34Ds
022600070108     c                   in        *dtaara
022700070108     c                   ENDIF
022800070108      *
022900070108     c                   ENDSR
