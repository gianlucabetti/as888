000100991124     H DECEDIT('0,') DATEDIT(*DMY/)
000200981217      ***************************************************************************
000300051221      *  CREA SCAN 05 / 06 e 10  x DPD dal Manifest x singola Bolla
000400971216      ***************************************************************************
000500171018     F*****fiar401L  IF   E           K DISK
000600171018     Ffipnd01L  IF   E           K DISK
000700060109     FFNARB01L  iF   E           K DISK    commit
000800060609     FTIDP301L  UF A E           K DISK    commit
000900940321      *----------------------------------------------------*
001000051221     D Wdataoggi       S               D   DATFMT(*eur)
001100991214     D Wdatagg         S              8  0 inz
001200991214     D W0140           S             14  0 inz
001300991214     D Wnotabe         S              1    inz
001400991214     D Wevd            S                   LIKE(DP3evd)
001500020919     D kpjbus          S                   LIKE(kpjbu)
001600171018     D fldPARCEL       S             14
001700991214      *-------------------
001800991130     D KPJBA         E DS
001900171018     D**** DSBL4i        E DS
002000991214     D DEVD          E DS
002100991215     D DEVD10        E DS                  extname(DEVD) prefix(H_)
002200991215     D DEVD05        E DS                  extname(DEVD) prefix(O_)
002300991214     D DEVD06        E DS                  extname(DEVD) prefix(R_)
002400991213     D CNCR80        E DS
002500991213     D UT§DSE0F      E DS
002600991214     D TIBS02DS      E DS
002700051221     D TRTC831       E DS                  EXTNAME(TRTC83DS1)
002800911213      *----------------------------------------------------*
002900051221     c     *ENTRY        PLIST
003000051221     c                   PARM                    KPJBA
003100051221      *   passa la singola Bolla
003200051221     C                   MOVEL     KPJBU         TRTC831
003300051221      *
003400051221     C                   MOVEL     kpjbu         KPJBUs
003500051221      *
003600051221     C     KSPE          KLIST
003700051221     C                   KFLD                    ARBAAS
003800051221     C                   KFLD                    ARBLNP
003900051221     C                   KFLD                    ARBNRS
004000051221     C                   KFLD                    ARBNSP
004100060622      *
004200171018     C***  KAR4          KLIST
004300171018     C*****              KFLD                    ARBAAS
004400171018     C*****              KFLD                    ARBLNP
004500171018     C*****              KFLD                    ARBNRS
004600171018     C*****              KFLD                    ARBNSP
004700171018     C*****              KFLD                    KTRC              1
004800171018      *
004900171018     C     Kpnd          KLIST
005000171018     C                   KFLD                    ARBAAS
005100171018     C                   KFLD                    ARBLNP
005200171018     C                   KFLD                    ARBNRS
005300171018     C                   KFLD                    ARBNSP
005400051221      * Dati società
005500051221     C                   Z-ADD     1             CODUT
005600051221     C                   CALL      'X§PARUT'
005700051221     C                   PARM                    UT§DSE0F
005800051221     C                   MOVEL     REC80         CNCR80
005900051221      *
006000051221      * Carico Tabella Eventi DPD "05" - "06" - "10"
006100051221     c                   clear                   TIBS02DS
006200051221     C                   MOVEL     'C'           t02mod
006300051221     C                   MOVEL     knsif         t02sif
006400051221     C                   MOVEL     'EVD'         t02cod
006500051221     C                   MOVEL(P)  05            t02ke1
006600051221     C                   CALL      'TIBS02R'
006700051221     C                   PARM                    KPJBA
006800051221     C                   PARM                    TIBS02DS
006900051221     C                   if        t02err = *BLANKS
007000051221     C                   MOVEL     T02UNI        DEVD05
007100051221     C                   else
007200051221     C                   eval      Wnotabe = 'S'
007300051221     C                   endif
007400051221      ***
007500051221     c                   clear                   TIBS02DS
007600051221     C                   MOVEL     'C'           t02mod
007700051221     C                   MOVEL     knsif         t02sif
007800051221     C                   MOVEL     'EVD'         t02cod
007900051221     C                   MOVEL(P)  06            t02ke1
008000051221     C                   CALL      'TIBS02R'
008100051221     C                   PARM                    KPJBA
008200051221     C                   PARM                    TIBS02DS
008300051221     C                   if        t02err = *BLANKS
008400051221     C                   MOVEL     T02UNI        DEVD06
008500051221     C                   else
008600051221     C                   eval      Wnotabe = 'S'
008700051221     C                   endif
008800051221      ***
008900051221     c                   clear                   TIBS02DS
009000051221     C                   MOVEL     'C'           t02mod
009100051221     C                   MOVEL     knsif         t02sif
009200051221     C                   MOVEL     'EVD'         t02cod
009300051221     C                   MOVEL(P)  10            t02ke1
009400051221     C                   CALL      'TIBS02R'
009500051221     C                   PARM                    KPJBA
009600051221     C                   PARM                    TIBS02DS
009700051221     C                   if        t02err = *BLANKS
009800051221     C                   MOVEL     T02UNI        DEVD10
009900051221     C                   else
010000051221     C                   eval      Wnotabe = 'S'
010100051221     C                   endif
010200051221      *
010300051221      *----------------------------------------------------*
010400051221      * Elaboro solo se ho trovato le tabelle
010500051221     c                   IF        Wnotabe <> 'S'
010600051221      *
010700051221      * DATA DEL GIORNO
010800051221     C                   TIME                    W0140
010900051221     C                   MOVE      W0140         Wdatagg
011000051221     C     *eur          MOVEL     Wdatagg       Wdataoggi
011100051221     C     *iso          MOVEL     Wdataoggi     Wdatagg
011200051221      *
011300051221      * aggancia la bolla
011400051221     C                   z-add     F83AAS        ARBAAS
011500051221     C                   z-add     F83LNP        ARBLNP
011600051221     C                   z-add     F83NRS        ARBNRS
011700051221     C                   z-add     F83NSP        ARBNSP
011800051221
011900051221     C     KSPE          chain     fnarb01l
012000051221     C                   IF        %Found(FNARB01L)
012100060622      * rek 'I'
012200171018     c******             move      'I'           ktrc
012300171018     c*****              clear                   DSBL4i
012400171018     c**** Kar4          chain     fiar401l
012500171018     c*****              if        %found(fiar401l)
012600171018     c******             movel     AR4not        DSBL4i
012700171018     c*******            end
012800171018      *
012900171018      ** sostituito FIAR4 "I"  con il FIPND
013000171018     c                   clear                   fldPARCEL
013100171018     c     Kpnd          chain     fipnd01l
013200171018     c                   if        %found(fipnd01l)
013300171018     c                   movel     pndIPN        fldPARCEL
013400171018     c                   end
013500991214
013600991214      * Controllo che per la spedizione NON esistano record in FIDP301L
013700060609     C     KSPE          setll     tidp3000
013800991214     c                   IF        NOT %equal
013900991214
014000991214      * Controllo se la spedizione è un reso                                ti
014100991223     c                   IF          ARBfbr <> *blanks
014200051221
014300991223     c                   movel     DEVD06        DEVD
014400000103     C                   movel     '06 '         WEVD
014500051221
014600991223     c                   ELSE
014700051221
014800991223     C                   movel     DEVD05        DEVD
014900000103     C                   movel     '05 '         WEVD
015000991223    1c                   ENDIF
015100991214      *
015200991214      * Scrivo record di FIDP3
015300991214     C                   exsr      wrt_evedpd
015400991214      *
015500991213     c                   ENDIF
015600051221     c                   ENDif
015700991213     c                   ENDIF
015800991214      *
015900991214     c                   clear                   tibs02ds
016000991214     c                   movel     'C'           t02tla
016100991214     c                   call      'TIBS02R'
016200991214     c                   parm                    kpjba
016300991214     c                   parm                    tibs02ds
016400991214      *
016500020918     c     fine          tag
016600051221     C                   MOVEL     kpjbus        KPJBU
016700051221     C                   SETON                                        LR
016800991214      *----------------------------------------------------*
016900991214      *   AGGIORNA FILE EVENTI DPD                         *
017000051221      *     scrive lo SCAN 05 o 06  ed il 10               *
017100991214      *----------------------------------------------------*
017200991214     C     WRT_evedpd    BEGSR
017300991214      *
017400060609     c                   clear                   tidp3000
017500991214      *
017600991214     c                   eval      DP3aas = ARBaas
017700991214     c                   eval      DP3lnp = ARBlnp
017800991214     c                   eval      DP3nrs = ARBnrs
017900991214     c                   eval      DP3nsp = ARBnsp
018000171018     c                   eval      dp3PRN = fldPARCEL
018100991214     c                   eval      DP3evd = WEVD
018200991214     c                   eval      DP3fle = ARBlnp
018300991214     c                   eval      DP3dev = ARBduc
018400991214     c                   eval      DP3hev = 2100
018500151211     C                   MOVEL     ARBAAS        DP3DTSPE
018600151211     C                   MOVE      ARBMGS        DP3DTSPE
018700060609     c                   WRITE     tidp3000
018800991215      *
018900991215      * Scrivo evento di passaggio all'Hub
019000991215     c                   eval      DP3evd = '10 '
019100991215     c                   eval      DP3fle = ARBlna
019200000831      * Data Arrivo della Trazione
019300991215     c                   If        ARBdti > *zeros
019400991215     c                   eval      DP3dev = ARBdti
019500991215     c                   Else
019600000831      * Data Arrivo della Merce a Filiale di Arrivo
019700991215     c                   eval      DP3dev = ARBdam
019800991215     c                   Endif
019900000831      *
020000991215     c                   eval      DP3hev = 1600
020100000831      *
020200000831      * per correggere l'errore di impostazione fissa delle 16:00
020300000831      *  sul TRANSITO  della Trazione
020400000831     c                   If        ARBhti > *zeros and
020500000831     c                             ARBdti > *zeros
020600000831     c                   eval      DP3hev = ARBhti
020700000831     c                   Endif
020800991215      *
020900151211     C                   MOVEL     ARBAAS        DP3DTSPE
021000151211     C                   MOVE      ARBMGS        DP3DTSPE
021100060609     c                   WRITE     tidp3000
021200991213      *
021300991213     C                   ENDSR
021400941230      *-------------------------------------------------------------------------
