000100001006     H DECEDIT('0,') DATEDIT(*YMD.)
000200001006
000300001006      *****************************************************************
000400001006      *                                                               *
000500031127      *       ELENCO ANAGR. RITIRI FISSI NON UTILIZZATI               *
000600001006      *                                                               *
000700001006      *****************************************************************
000800001018
000900980521      ****************************************************************
001000980521      *  RIEPILOGO INDICATORI
001100981008      ****************************************************************
001200031021      * 12 - Sono in sede
001300031021      * 28 - Errore generico
001400031022      * 30 - Comodo
001500031022      * 40 - ERRORE p.o. in gestione
001600990317      ****************************************************************
001700980623
001800031021     FFIOR93D   CF   E             WORKSTN
001900031021     FAZORG01L  IF   E           K DISK
002000980623
002100980521      *------------------------------------------------------------------------*
002200031021      * Schiera per caricamento L1
002300031021     D L1              s              3  0 DIM(30)
002400031021      *
002500031021      * Ds per Trul06r - caricamento £1
002600031021     D Dsul06        e ds                  EXTNAME(trul06ds)
002700031021     D LIA                     1     90  0
002800031021     D                                     DIM(30)
002900031021      *
003000031021      * Parametri di selezione
003100031021     D Param           ds
003200031022     D $fil                           3  0
003300031021      *
003400031021     D tibs34ds      e ds                  inz
003500031021     D ddatiute      e ds
003600031021     D azuteds       e ds                  extname(AZUTE00F)
003700031021     D kpjba         e ds
003800980521      *   S C H I E R E
003900031022     D MSG             S             78    DIM(02)CTDATA PERRCD(1)              MSG VIDEO
004000070419
004100070419     d                sds
004200070419     d  vtcpgm                 1     10
004300001006
004400001006      *------------------------------------------------------------------------*
004500001009
004600001006
004700031021      * Emissione  Videata
004800980623
004900001006     C                   do        *hival
005000980623
005100031021     C                   exfmt     fior931
005200980623
005300031021     C                   setoff                                       28
005400010405
005500001006      * F3=Fine
005600001006     C   kc              leave
005700031021      * controllo P.O. in gestione
005800010405     C                   exsr      Sr_Contrpo
005900010405     C   28              iter
006000031021      * F6=Conferma
006100031021     C                   if        *inkf = *on
006200031021     C                   movel     v1cfgs        $fil
006300031021     c                   clear                   kpjbu
006400031021     c                   movel     param         kpjbu
006500031022      *
006600031022      * viene sottomessa la stampa
006700031022      *
006800031022     C                   MOVEL     'OR94'        KCOAZ
006900031022     C                   CALL      'BCH10'
007000031022     C                   PARM                    KPJBA
007100031022      *
007200031022     c                   leave
007300001009     C                   endif
007400981008
007500001006     C                   enddo
007600981204
007700001006     C                   eval      *inlr = *on
007800001009      **********************************************************************
007900010405      * CONTROLLO IL P.O. IN GESTIONE
008000001009      **********************************************************************
008100001009     C     Sr_Contrpo    begsr
008200001009
008300031022      * errore
008400031022     C                   setoff                                       40
008500031021      * se sono in sede deve essere uguale a zero
008600031022     c                   If        *in12 and v1cfgs <> 000
008700031022     C                   seton                                        40  28
008800031022     C                   movel     msg(2)        v1cmsg
008900031022     C                   goto      endsrcpo
009000031022     C                   endif
009100031022      * se non sno in sede
009200031022     c                   if        not *in12
009300031022     c                   if        v1cfgs = 0
009400031022     C                   seton                                        40  28
009500031022     C                   movel     msg(1)        v1cmsg
009600031022     C                   goto      endsrcpo
009700031022     C                   endif
009800001025     C                   clear                   v1dfgs
009900001009
010000001025     C     v1cfgs        lookup    L1                                     30
010100001025     C     *in30         ifeq      *off
010200001025     C                   seton                                        40  28
010300031021     C                   movel     msg(1)        v1cmsg
010400010405     C                   goto      endsrcpo
010500010405     C                   endif
010600001025      * Decodifica
010700001025     C     v1cfgs        chain     azorg01L
010800001025     C                   if        %found(azorg01l)
010900001025     C                   movel     orgDES        v1dfgs
011000001025     C                   endif
011100031022
011200031022     c                   endif
011300001009
011400010405     C     endsrcpo      endsr
011500980923      *****************************************************************
011600980923      * ROUTINE INIZIALE
011700980923      *****************************************************************
011800980923     C     *INZSR        BEGSR
011900980923      *
012000980521     C     *ENTRY        PLIST
012100980521     C                   PARM                    KPJBA
012200031021      *
012300031021     c     *dtaara       define    §azute        azuteds
012400031021     c     *dtaara       define    §datiute      ddatiute
012500031021     C                   in(E)     *dtaara
012600031021     C                   IF        %error  or RSUT = *blanks
012700031021     C                   CLEAR                   tibs34ds
012800031021     C                   CALL      'TIBS34R'
012900031021     C                   PARM                    tibs34ds
013000031021     C                   in        *dtaara
013100031021     c                   endif
013200031021      *
013300010405
013400010405      * Da sede non imposto il p.o. di gestione
013500010405
013600010405     C                   if        simfel = 0
013700031021     C                   eval      V1CFGS = 000
013800031021     C                   eval      *in12 = *on
013900031021     c                   eval      v1dfgs = 'Tutti          '
014000031021     C                   else
014100001006
014200001006      * IMPOSTO IL P.O. DI GESTIONE
014300010405
014400010405     C                   eval      *in12 = *off
014500031022     c                   eval      v1cfgs = Dutpou
014600010405     C                   endif
014700001025
014800001025      * Carico schiera P.O. gestiti
014900001025     C                   CLEAR                   DSUL06
015000001025     C                   MOVE      '£1'          D06COD
015100001025     C                   MOVEL     SIMFEL        D06KEY
015200001025     C                   MOVEL     DSUL06        KPJBU
015300001025     C                   CALL      'TRUL06R'
015400001025     C                   PARM                    KPJBA
015500001025     C                   MOVEL     KPJBU         DSUL06
015600031021     C                   MOVEA     LIA           L1
015700031021     C                   clear                   kpjbu
015800001009
015900031021     c                   if        *in12 = *off
016000001006     C     v1cfgs        chain     azorg01L
016100001006     C                   if        %found(azorg01l)
016200001006     C                   movel     orgDES        v1dfgs
016300001006     C                   else
016400001006     C                   clear                   v1dfgs
016500001013     C                   endif
016600031021     C                   endif
016700001006
016800010405     C                   endsr
016900980521      *---------------------------------------------------------------------------------------------
017000980521** MSG  Lungh. 78                                                            *
017100070419Filiale gestione errata                                                             1
017200070419In sede non si può selezionare la filiale gestione                                  2
