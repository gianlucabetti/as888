000100991124     H DECEDIT('0,') DATEDIT(*DMY/)
000200981217      ***************************************************************************
000300051221      *  CREA SCAN 05 / 06 e 10  x DPD dal Manifest x singola Bolla
000400971216      ***************************************************************************
000500060622     Ffiar401L  IF   E           K DISK
000600060109     FFNARB01L  iF   E           K DISK    commit
000700060609     FTIDP301L  UF A E           K DISK    commit
000800020716     FAZORG01L  IF   E           K DISK
000900940321      *----------------------------------------------------*
001000051221     D Wdataoggi       S               D   DATFMT(*eur)
001100991214     D Wdatagg         S              8  0 inz
001200991214     D W0140           S             14  0 inz
001300991214     D Wnotabe         S              1    inz
001400991214     D Wevd            S                   LIKE(DP3evd)
001500020919     D kpjbus          S                   LIKE(kpjbu)
001600991214      *-------------------
001700991130     D KPJBA         E DS
001800060622     D DSBL4i        E DS
001900991214     D DEVD          E DS
002000991215     D DEVD10        E DS                  extname(DEVD) prefix(H_)
002100991215     D DEVD05        E DS                  extname(DEVD) prefix(O_)
002200991214     D DEVD06        E DS                  extname(DEVD) prefix(R_)
002300991213     D CNCR80        E DS
002400991213     D UT§DSE0F      E DS
002500991214     D TIBS02DS      E DS
002600991214     D OG143         E DS
002700020919     D TRUL06DS      E DS
002800020919     D  LIN                    1     90  0 DIM(30)                              P.O. COMODO
002900051221     D TRTC831       E DS                  EXTNAME(TRTC83DS1)
003000911213      *----------------------------------------------------*
003100051221     c     *ENTRY        PLIST
003200051221     c                   PARM                    KPJBA
003300051221      *   passa la singola Bolla
003400051221     C                   MOVEL     KPJBU         TRTC831
003500051221      *
003600051221     C                   MOVEL     kpjbu         KPJBUs
003700051221      *
003800051221     C     KSPE          KLIST
003900051221     C                   KFLD                    ARBAAS
004000051221     C                   KFLD                    ARBLNP
004100051221     C                   KFLD                    ARBNRS
004200051221     C                   KFLD                    ARBNSP
004300060622      *
004400060622     C     KAR4          KLIST
004500060622     C                   KFLD                    ARBAAS
004600060622     C                   KFLD                    ARBLNP
004700060622     C                   KFLD                    ARBNRS
004800060622     C                   KFLD                    ARBNSP
004900060622     C                   KFLD                    KTRC              1
005000051221      * Dati società
005100051221     C                   Z-ADD     1             CODUT
005200051221     C                   CALL      'X§PARUT'
005300051221     C                   PARM                    UT§DSE0F
005400051221     C                   MOVEL     REC80         CNCR80
005500051221      *
005600051221      * CARICO TABELLA PUNTI OPERATIVI GESTITI £1
005700051221     C                   CLEAR                   TRUL06DS
005800051221     C                   MOVE      '£1'          D06COD
005900051221     C                   MOVEL     SIMFEL        D06KEY
006000051221     C                   MOVEL     'L'           D06TLA
006100051221     C                   MOVEL     TRUL06DS      KPJBU
006200051221     C                   CALL      'TRUL06R'
006300051221     C                   PARM                    KPJBA
006400051221     C                   MOVEL     KPJBU         TRUL06DS
006500051221      *
006600051221     c                   move      'S'           Wfine             1
006700051221      *  controlla se fra le linee del terminal è gestita la DPD
006800051221     c                   do        30            lx                3 0
006900051221     c     lin(lx)       ifeq      *zero
007000051221     c                   leave
007100051221     c                   end
007200051221     c     lin(lx)       chain     azorg01l
007300051221     c                   if        %found(azorg01l)
007400051221     C                   movel     ORGDE3        OG143
007500051221     c                   if        §OGNTW = 'DPD'
007600051221     c                   clear                   Wfine
007700051221     c                   leave
007800051221     c                   end
007900051221     c                   endif
008000051221     c                   enddo
008100051221      *
008200051221      *  Salta tutto e non esegue il programma se non gestisce il terminal
008300051221     c     Wfine         cabeq     'S'           fine
008400051221      *
008500051221      * Carico Tabella Eventi DPD "05" - "06" - "10"
008600051221     c                   clear                   TIBS02DS
008700051221     C                   MOVEL     'C'           t02mod
008800051221     C                   MOVEL     knsif         t02sif
008900051221     C                   MOVEL     'EVD'         t02cod
009000051221     C                   MOVEL(P)  05            t02ke1
009100051221     C                   CALL      'TIBS02R'
009200051221     C                   PARM                    KPJBA
009300051221     C                   PARM                    TIBS02DS
009400051221     C                   if        t02err = *BLANKS
009500051221     C                   MOVEL     T02UNI        DEVD05
009600051221     C                   else
009700051221     C                   eval      Wnotabe = 'S'
009800051221     C                   endif
009900051221      ***
010000051221     c                   clear                   TIBS02DS
010100051221     C                   MOVEL     'C'           t02mod
010200051221     C                   MOVEL     knsif         t02sif
010300051221     C                   MOVEL     'EVD'         t02cod
010400051221     C                   MOVEL(P)  06            t02ke1
010500051221     C                   CALL      'TIBS02R'
010600051221     C                   PARM                    KPJBA
010700051221     C                   PARM                    TIBS02DS
010800051221     C                   if        t02err = *BLANKS
010900051221     C                   MOVEL     T02UNI        DEVD06
011000051221     C                   else
011100051221     C                   eval      Wnotabe = 'S'
011200051221     C                   endif
011300051221      ***
011400051221     c                   clear                   TIBS02DS
011500051221     C                   MOVEL     'C'           t02mod
011600051221     C                   MOVEL     knsif         t02sif
011700051221     C                   MOVEL     'EVD'         t02cod
011800051221     C                   MOVEL(P)  10            t02ke1
011900051221     C                   CALL      'TIBS02R'
012000051221     C                   PARM                    KPJBA
012100051221     C                   PARM                    TIBS02DS
012200051221     C                   if        t02err = *BLANKS
012300051221     C                   MOVEL     T02UNI        DEVD10
012400051221     C                   else
012500051221     C                   eval      Wnotabe = 'S'
012600051221     C                   endif
012700051221      *
012800051221      *----------------------------------------------------*
012900051221      * Elaboro solo se ho trovato le tabelle
013000051221     c                   IF        Wnotabe <> 'S'
013100051221      *
013200051221      * DATA DEL GIORNO
013300051221     C                   TIME                    W0140
013400051221     C                   MOVE      W0140         Wdatagg
013500051221     C     *eur          MOVEL     Wdatagg       Wdataoggi
013600051221     C     *iso          MOVEL     Wdataoggi     Wdatagg
013700051221      *
013800051221      * aggancia la bolla
013900051221     C                   z-add     F83AAS        ARBAAS
014000051221     C                   z-add     F83LNP        ARBLNP
014100051221     C                   z-add     F83NRS        ARBNRS
014200051221     C                   z-add     F83NSP        ARBNSP
014300051221
014400051221     C     KSPE          chain     fnarb01l
014500051221     C                   IF        %Found(FNARB01L)
014600060622      * rek 'I'
014700060622     c                   move      'I'           ktrc
014800060622     c                   clear                   DSBL4i
014900060622     c     Kar4          chain     fiar401l
015000060622     c                   if        %found(fiar401l)
015100060622     c                   movel     AR4not        DSBL4i
015200060622     c                   end
015300991214
015400991214      * Controllo che per la spedizione NON esistano record in FIDP301L
015500060609     C     KSPE          setll     tidp3000
015600991214     c                   IF        NOT %equal
015700991214
015800991214      * Controllo se la spedizione è un reso                                ti
015900991223     c                   IF          ARBfbr <> *blanks
016000051221
016100991223     c                   movel     DEVD06        DEVD
016200000103     C                   movel     '06 '         WEVD
016300051221
016400991223     c                   ELSE
016500051221
016600991223     C                   movel     DEVD05        DEVD
016700000103     C                   movel     '05 '         WEVD
016800991223    1c                   ENDIF
016900991214      *
017000991214      * Scrivo record di FIDP3
017100991214     C                   exsr      wrt_evedpd
017200991214      *
017300991213     c                   ENDIF
017400051221     c                   ENDif
017500991213     c                   ENDIF
017600991214      *
017700991214     c                   clear                   tibs02ds
017800991214     c                   movel     'C'           t02tla
017900991214     c                   call      'TIBS02R'
018000991214     c                   parm                    kpjba
018100991214     c                   parm                    tibs02ds
018200991214      *
018300020918     c     fine          tag
018400051221     C                   MOVEL     kpjbus        KPJBU
018500051221     C                   SETON                                        LR
018600991214      *----------------------------------------------------*
018700991214      *   AGGIORNA FILE EVENTI DPD                         *
018800051221      *     scrive lo SCAN 05 o 06  ed il 10               *
018900991214      *----------------------------------------------------*
019000991214     C     WRT_evedpd    BEGSR
019100991214      *
019200060609     c                   clear                   tidp3000
019300991214      *
019400991214     c                   eval      DP3aas = ARBaas
019500991214     c                   eval      DP3lnp = ARBlnp
019600991214     c                   eval      DP3nrs = ARBnrs
019700991214     c                   eval      DP3nsp = ARBnsp
019800060622     c                   eval      dp3PRN = §B4IPN
019900991214     c                   eval      DP3evd = WEVD
020000991214     c                   eval      DP3fle = ARBlnp
020100991214     c                   eval      DP3dev = ARBduc
020200991214     c                   eval      DP3hev = 2100
020300151211     C                   MOVEL     ARBAAS        DP3DTSPE
020400151211     C                   MOVE      ARBMGS        DP3DTSPE
020500060609     c                   WRITE     tidp3000
020600991215      *
020700991215      * Scrivo evento di passaggio all'Hub
020800991215     c                   eval      DP3evd = '10 '
020900991215     c                   eval      DP3fle = ARBlna
021000000831      * Data Arrivo della Trazione
021100991215     c                   If        ARBdti > *zeros
021200991215     c                   eval      DP3dev = ARBdti
021300991215     c                   Else
021400000831      * Data Arrivo della Merce a Filiale di Arrivo
021500991215     c                   eval      DP3dev = ARBdam
021600991215     c                   Endif
021700000831      *
021800991215     c                   eval      DP3hev = 1600
021900000831      *
022000000831      * per correggere l'errore di impostazione fissa delle 16:00
022100000831      *  sul TRANSITO  della Trazione
022200000831     c                   If        ARBhti > *zeros and
022300000831     c                             ARBdti > *zeros
022400000831     c                   eval      DP3hev = ARBhti
022500000831     c                   Endif
022600991215      *
022700151211     C                   MOVEL     ARBAAS        DP3DTSPE
022800151211     C                   MOVE      ARBMGS        DP3DTSPE
022900060609     c                   WRITE     tidp3000
023000991213      *
023100991213     C                   ENDSR
023200941230      *-------------------------------------------------------------------------
