000100991124     H DECEDIT('0,') DATEDIT(*DMY/)
000200981217      ***************************************************************************
000300051221      *  CREA SCAN 05 / 06 e 10  x DPD dal Manifest x singola Bolla
000400971216      ***************************************************************************
000500060622     Ffiar401L  IF   E           K DISK
000600060109     FFNARB01L  iF   E           K DISK    commit
000700060609     FTIDP301L  UF A E           K DISK    commit
000800940321      *----------------------------------------------------*
000900051221     D Wdataoggi       S               D   DATFMT(*eur)
001000991214     D Wdatagg         S              8  0 inz
001100991214     D W0140           S             14  0 inz
001200991214     D Wnotabe         S              1    inz
001300991214     D Wevd            S                   LIKE(DP3evd)
001400020919     D kpjbus          S                   LIKE(kpjbu)
001500991214      *-------------------
001600991130     D KPJBA         E DS
001700060622     D DSBL4i        E DS
001800991214     D DEVD          E DS
001900991215     D DEVD10        E DS                  extname(DEVD) prefix(H_)
002000991215     D DEVD05        E DS                  extname(DEVD) prefix(O_)
002100991214     D DEVD06        E DS                  extname(DEVD) prefix(R_)
002200991213     D CNCR80        E DS
002300991213     D UT§DSE0F      E DS
002400991214     D TIBS02DS      E DS
002500051221     D TRTC831       E DS                  EXTNAME(TRTC83DS1)
002600911213      *----------------------------------------------------*
002700051221     c     *ENTRY        PLIST
002800051221     c                   PARM                    KPJBA
002900051221      *   passa la singola Bolla
003000051221     C                   MOVEL     KPJBU         TRTC831
003100051221      *
003200051221     C                   MOVEL     kpjbu         KPJBUs
003300051221      *
003400051221     C     KSPE          KLIST
003500051221     C                   KFLD                    ARBAAS
003600051221     C                   KFLD                    ARBLNP
003700051221     C                   KFLD                    ARBNRS
003800051221     C                   KFLD                    ARBNSP
003900060622      *
004000060622     C     KAR4          KLIST
004100060622     C                   KFLD                    ARBAAS
004200060622     C                   KFLD                    ARBLNP
004300060622     C                   KFLD                    ARBNRS
004400060622     C                   KFLD                    ARBNSP
004500060622     C                   KFLD                    KTRC              1
004600051221      * Dati società
004700051221     C                   Z-ADD     1             CODUT
004800051221     C                   CALL      'X§PARUT'
004900051221     C                   PARM                    UT§DSE0F
005000051221     C                   MOVEL     REC80         CNCR80
005100051221      *
005200051221      * Carico Tabella Eventi DPD "05" - "06" - "10"
005300051221     c                   clear                   TIBS02DS
005400051221     C                   MOVEL     'C'           t02mod
005500051221     C                   MOVEL     knsif         t02sif
005600051221     C                   MOVEL     'EVD'         t02cod
005700051221     C                   MOVEL(P)  05            t02ke1
005800051221     C                   CALL      'TIBS02R'
005900051221     C                   PARM                    KPJBA
006000051221     C                   PARM                    TIBS02DS
006100051221     C                   if        t02err = *BLANKS
006200051221     C                   MOVEL     T02UNI        DEVD05
006300051221     C                   else
006400051221     C                   eval      Wnotabe = 'S'
006500051221     C                   endif
006600051221      ***
006700051221     c                   clear                   TIBS02DS
006800051221     C                   MOVEL     'C'           t02mod
006900051221     C                   MOVEL     knsif         t02sif
007000051221     C                   MOVEL     'EVD'         t02cod
007100051221     C                   MOVEL(P)  06            t02ke1
007200051221     C                   CALL      'TIBS02R'
007300051221     C                   PARM                    KPJBA
007400051221     C                   PARM                    TIBS02DS
007500051221     C                   if        t02err = *BLANKS
007600051221     C                   MOVEL     T02UNI        DEVD06
007700051221     C                   else
007800051221     C                   eval      Wnotabe = 'S'
007900051221     C                   endif
008000051221      ***
008100051221     c                   clear                   TIBS02DS
008200051221     C                   MOVEL     'C'           t02mod
008300051221     C                   MOVEL     knsif         t02sif
008400051221     C                   MOVEL     'EVD'         t02cod
008500051221     C                   MOVEL(P)  10            t02ke1
008600051221     C                   CALL      'TIBS02R'
008700051221     C                   PARM                    KPJBA
008800051221     C                   PARM                    TIBS02DS
008900051221     C                   if        t02err = *BLANKS
009000051221     C                   MOVEL     T02UNI        DEVD10
009100051221     C                   else
009200051221     C                   eval      Wnotabe = 'S'
009300051221     C                   endif
009400051221      *
009500051221      *----------------------------------------------------*
009600051221      * Elaboro solo se ho trovato le tabelle
009700051221     c                   IF        Wnotabe <> 'S'
009800051221      *
009900051221      * DATA DEL GIORNO
010000051221     C                   TIME                    W0140
010100051221     C                   MOVE      W0140         Wdatagg
010200051221     C     *eur          MOVEL     Wdatagg       Wdataoggi
010300051221     C     *iso          MOVEL     Wdataoggi     Wdatagg
010400051221      *
010500051221      * aggancia la bolla
010600051221     C                   z-add     F83AAS        ARBAAS
010700051221     C                   z-add     F83LNP        ARBLNP
010800051221     C                   z-add     F83NRS        ARBNRS
010900051221     C                   z-add     F83NSP        ARBNSP
011000051221
011100051221     C     KSPE          chain     fnarb01l
011200051221     C                   IF        %Found(FNARB01L)
011300060622      * rek 'I'
011400060622     c                   move      'I'           ktrc
011500060622     c                   clear                   DSBL4i
011600060622     c     Kar4          chain     fiar401l
011700060622     c                   if        %found(fiar401l)
011800060622     c                   movel     AR4not        DSBL4i
011900060622     c                   end
012000991214
012100991214      * Controllo che per la spedizione NON esistano record in FIDP301L
012200060609     C     KSPE          setll     tidp3000
012300991214     c                   IF        NOT %equal
012400991214
012500991214      * Controllo se la spedizione è un reso                                ti
012600991223     c                   IF          ARBfbr <> *blanks
012700051221
012800991223     c                   movel     DEVD06        DEVD
012900000103     C                   movel     '06 '         WEVD
013000051221
013100991223     c                   ELSE
013200051221
013300991223     C                   movel     DEVD05        DEVD
013400000103     C                   movel     '05 '         WEVD
013500991223    1c                   ENDIF
013600991214      *
013700991214      * Scrivo record di FIDP3
013800991214     C                   exsr      wrt_evedpd
013900991214      *
014000991213     c                   ENDIF
014100051221     c                   ENDif
014200991213     c                   ENDIF
014300991214      *
014400991214     c                   clear                   tibs02ds
014500991214     c                   movel     'C'           t02tla
014600991214     c                   call      'TIBS02R'
014700991214     c                   parm                    kpjba
014800991214     c                   parm                    tibs02ds
014900991214      *
015000020918     c     fine          tag
015100051221     C                   MOVEL     kpjbus        KPJBU
015200051221     C                   SETON                                        LR
015300991214      *----------------------------------------------------*
015400991214      *   AGGIORNA FILE EVENTI DPD                         *
015500051221      *     scrive lo SCAN 05 o 06  ed il 10               *
015600991214      *----------------------------------------------------*
015700991214     C     WRT_evedpd    BEGSR
015800991214      *
015900060609     c                   clear                   tidp3000
016000991214      *
016100991214     c                   eval      DP3aas = ARBaas
016200991214     c                   eval      DP3lnp = ARBlnp
016300991214     c                   eval      DP3nrs = ARBnrs
016400991214     c                   eval      DP3nsp = ARBnsp
016500060622     c                   eval      dp3PRN = §B4IPN
016600991214     c                   eval      DP3evd = WEVD
016700991214     c                   eval      DP3fle = ARBlnp
016800991214     c                   eval      DP3dev = ARBduc
016900991214     c                   eval      DP3hev = 2100
017000151211     C                   MOVEL     ARBAAS        DP3DTSPE
017100151211     C                   MOVE      ARBMGS        DP3DTSPE
017200060609     c                   WRITE     tidp3000
017300991215      *
017400991215      * Scrivo evento di passaggio all'Hub
017500991215     c                   eval      DP3evd = '10 '
017600991215     c                   eval      DP3fle = ARBlna
017700000831      * Data Arrivo della Trazione
017800991215     c                   If        ARBdti > *zeros
017900991215     c                   eval      DP3dev = ARBdti
018000991215     c                   Else
018100000831      * Data Arrivo della Merce a Filiale di Arrivo
018200991215     c                   eval      DP3dev = ARBdam
018300991215     c                   Endif
018400000831      *
018500991215     c                   eval      DP3hev = 1600
018600000831      *
018700000831      * per correggere l'errore di impostazione fissa delle 16:00
018800000831      *  sul TRANSITO  della Trazione
018900000831     c                   If        ARBhti > *zeros and
019000000831     c                             ARBdti > *zeros
019100000831     c                   eval      DP3hev = ARBhti
019200000831     c                   Endif
019300991215      *
019400151211     C                   MOVEL     ARBAAS        DP3DTSPE
019500151211     C                   MOVE      ARBMGS        DP3DTSPE
019600060609     c                   WRITE     tidp3000
019700991213      *
019800991213     C                   ENDSR
019900941230      *-------------------------------------------------------------------------
