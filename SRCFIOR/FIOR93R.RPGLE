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
002200130621      * Schiera filiali gestione
002300130621     d SKpog           s              3    dim(250) inz(*zeros)                 Filiali Gestibili
002400031021      *
002500130621
002600130621     d wabi            s                   like(UTEaut)
002700130621     d wEsci           s               n   inz(*off)
002800130621     d wErrGrave       s               n   inz(*off)
002900130621     d w003a           s              3
003000031021      *
003100031021      * Parametri di selezione
003200031021     D Param           ds
003300031022     D $fil                           3  0
003400031021      *
003500031021     D tibs34ds      e ds                  inz
003600031021     D ddatiute      e ds
003700031021     D azuteds       e ds                  extname(AZUTE00F)
003800130621     d TIBS02DS      e ds
003900130621     d TRUL31DS      e ds
004000130621
004100130621     d dLAT          e ds
004200130621     d dUTE01        e ds
004300130621
004400031021     D kpjba         e ds
004500980521      *   S C H I E R E
004600031022     D MSG             S             78    DIM(02)CTDATA PERRCD(1)              MSG VIDEO
004700070419
004800070419     d                sds
004900070419     d  vtcpgm                 1     10
005000001006
005100001006      *------------------------------------------------------------------------*
005200001009
005300001006
005400031021      * Emissione  Videata
005500980623
005600001006     C                   do        *hival
005700130621
005800130621      /free
005900130621       //?Se utente non autorizzato alla funzione esco
006000130621         IF  wErrGrave;
006100130621           *in28 = *on;
006200130621           wEsci = *on;
006300130621           V1Cmsg = 'Utente non autorizzato alla funzione';
006400130621         ENDIF;
006500130621      /end-free
006600980623
006700031021     C                   exfmt     fior931
006800980623
006900031021     C                   setoff                                       28
007000130621      /free
007100130621         IF  wEsci;
007200130621           leave;
007300130621         ENDIF;
007400130621      /end-free
007500010405
007600001006      * F3=Fine
007700001006     C   kc              leave
007800031021      * controllo P.O. in gestione
007900010405     C                   exsr      Sr_Contrpo
008000010405     C   28              iter
008100031021      * F6=Conferma
008200031021     C                   if        *inkf = *on
008300031021     C                   movel     v1cfgs        $fil
008400031021     c                   clear                   kpjbu
008500031021     c                   movel     param         kpjbu
008600031022      *
008700031022      * viene sottomessa la stampa
008800031022      *
008900031022     C                   MOVEL     'OR94'        KCOAZ
009000031022     C                   CALL      'BCH10'
009100031022     C                   PARM                    KPJBA
009200031022      *
009300031022     c                   leave
009400001009     C                   endif
009500981008
009600001006     C                   enddo
009700981204
009800001006     C                   eval      *inlr = *on
009900001009      **********************************************************************
010000010405      * CONTROLLO IL P.O. IN GESTIONE
010100001009      **********************************************************************
010200001009     C     Sr_Contrpo    begsr
010300001009
010400031022      * errore
010500031022     C                   setoff                                       40
010600031021      * se sono in sede deve essere uguale a zero
010700031022     c                   If        *in12 and v1cfgs <> 000
010800031022     C                   seton                                        40  28
010900031022     C                   movel     msg(2)        v1cmsg
011000031022     C                   goto      endsrcpo
011100031022     C                   endif
011200031022      * se non sno in sede
011300031022     c                   if        not *in12
011400031022     c                   if        v1cfgs = 0
011500031022     C                   seton                                        40  28
011600031022     C                   movel     msg(1)        v1cmsg
011700031022     C                   goto      endsrcpo
011800031022     C                   endif
011900001025     C                   clear                   v1dfgs
012000001009
012100130621     c                   move      V1Cfgs        w003a
012200130621     c     w003a         lookup    SKpog                                  30
012300001025     C     *in30         ifeq      *off
012400001025     C                   seton                                        40  28
012500031021     C                   movel     msg(1)        v1cmsg
012600010405     C                   goto      endsrcpo
012700010405     C                   endif
012800001025      * Decodifica
012900001025     C     v1cfgs        chain     azorg01L
013000001025     C                   if        %found(azorg01l)
013100001025     C                   movel     orgDES        v1dfgs
013200001025     C                   endif
013300031022
013400031022     c                   endif
013500001009
013600010405     C     endsrcpo      endsr
013700980923      *****************************************************************
013800980923      * ROUTINE INIZIALE
013900980923      *****************************************************************
014000980923     C     *INZSR        BEGSR
014100980923      *
014200980521     C     *ENTRY        PLIST
014300980521     C                   PARM                    KPJBA
014400031021      *
014500031021     c     *dtaara       define    §azute        azuteds
014600031021     c     *dtaara       define    §datiute      ddatiute
014700031021     C                   in(E)     *dtaara
014800031021     C                   IF        %error  or RSUT = *blanks
014900031021     C                   CLEAR                   tibs34ds
015000031021     C                   CALL      'TIBS34R'
015100031021     C                   PARM                    tibs34ds
015200031021     C                   in        *dtaara
015300031021     c                   endif
015400130621
015500130621      * controllo abilitazione utente
015600130621     c                   clear                   wabi
015700130621     c                   clear                   dLAT
015800130621     c                   eval      dUTE01 = UTEfaf
015900130621
016000130621      * Verifica errori e autorità profilo
016100130621     c                   SELECT
016200130621      * se ho errori nei dati utente esco dal pgm
016300130621     c                   WHEN      DUTerr = 'E'
016400130621     c                   eval      wErrGrave = *on
016500130621     c                   leavesr
016600130621      * carica abilitazioni del profilo
016700130621     c                   WHEN      §UTEorm <> *blanks
016800130621     c                   eval      wabi = §UTEorm
016900130621     c                   WHEN      UTEaut <> *blanks
017000130621     c                   eval      wabi = UTEaut
017100130621      * se non c'è l'abilitazione
017200130621      * --> se 1° livello, abilitazioni al terminal
017300130621      *     se 2° livello, abilitazioni al punto operativo
017400130621      *     se sede no abilitazioni
017500130621     c                   OTHER
017600130621     c                   IF        DUTlpo = '1'
017700130621     c                   eval      wabi   = 'TP'
017800130621     c                   ENDIF
017900130621     c                   IF        DUTlpo = '2'
018000130621     c                   eval      wabi   = 'PO'
018100130621     c                   ENDIF
018200130621     c                   ENDSL
018300130621
018400130621      * controllo se ok l'abilitazione dell'utente
018500130621     c                   clear                   TIBS02DS
018600130621     c                   eval      T02mod = 'C'
018700130621     c                   eval      T02sif = knsif
018800130621     c                   eval      T02cod = 'LAT'
018900130621     c                   eval      T02ke1 = wabi
019000130621     c                   Call      'TIBS02R'
019100130621     c                   Parm                    KPJBA
019200130621     c                   Parm                    TIBS02DS
019300130621     c                   IF        T02err <> *blanks or §LATabi = 'S'
019400130621     c                   eval      wErrGrave = *on
019500130621     c                   leavesr
019600130621     c                   ENDIF
019700130621     c                   eval      dLAT = T02uni
019800130621
019900130621      * Reperimento delle filiali gestibili dall'utente
020000130621     c                   clear                   TRUL31DS
020100130621     c                   eval      I31abi = wabi
020200130621     c                   eval      I31cdi = DUTdis
020300130621     c                   eval      I31car = DUTare
020400130621     c                   eval      I31cpo = DUTpou
020500130621     c                   call      'TRUL31R'
020600130621     c                   parm                    KPJBA
020700130621     c                   parm                    TRUL31DS
020800130621     c                   IF        O31pog > *zeros
020900130621     c                   movea     O31pog        SKpog
021000130621     c                   ELSE
021100130621     c                   eval      wErrGrave = *on
021200130621     c                   leavesr
021300130621     c                   ENDIF
021400031021      *
021500010405
021600010405      * Da sede non imposto il p.o. di gestione
021700010405
021800010405     C                   if        simfel = 0
021900031021     C                   eval      V1CFGS = 000
022000031021     C                   eval      *in12 = *on
022100031021     c                   eval      v1dfgs = 'Tutti          '
022200031021     C                   else
022300001006
022400001006      * IMPOSTO IL P.O. DI GESTIONE
022500010405
022600010405     C                   eval      *in12 = *off
022700031022     c                   eval      v1cfgs = Dutpou
022800010405     C                   endif
022900001009
023000031021     c                   if        *in12 = *off
023100001006     C     v1cfgs        chain     azorg01L
023200001006     C                   if        %found(azorg01l)
023300001006     C                   movel     orgDES        v1dfgs
023400001006     C                   else
023500001006     C                   clear                   v1dfgs
023600001013     C                   endif
023700031021     C                   endif
023800001006
023900010405     C                   endsr
024000980521      *---------------------------------------------------------------------------------------------
024100980521** MSG  Lungh. 78                                                            *
024200070419Filiale gestione errata                                                             1
024300070419In sede non si può selezionare la filiale gestione                                  2
