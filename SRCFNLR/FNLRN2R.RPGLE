000100030117      * FNLRN2R *-----------------------------------------------------*
000200030117      *         - TRASMISSIONE FIAR5 TRA P.O. E A SEDE -              *
000300030117      *---------------------------------------------------------------*
000400030117
000500030117      *****************************************************************
000600030117      *  RIEPILOGO INDICATORI                                         *
000700030117      *****************************************************************
000800030117      * 30   - Comodo                                                 *
000900030117      * 97   - Membro non disponibile                                 *
001000030117      * 98   - P.O. locale                                            *
001100030117      *****************************************************************
001200030117
001300030117     h decedit('0,') datedit(*dmy.)
001400030117
001500030117      *---------------------------------------------------------------*
001600030117
001700030117     fTNTBE01L  If   e           k disk
001800030117      *
001900030117     fFIAR502L  Uf   e           k disk
002000030117      *
002100030117     fFIAR500T  O    e             disk    usropn
002200030117     f                                     rename(FIAR5000:FIAR50T)
002300030117     fFIAR546T  O    e             disk    usropn
002400030117     f                                     rename(FIAR5000:FIAR5ST)
002500030117
002600030117      *---------------------------------------------------------------*
002700030117
002800030117      * Costanti :
002900030117      *
003000030117     d EL_skPO         c                   const(250)
003100060320     D CUTI            C                   CONST('EDP*      ')
003200030117      *
003300030117      * Schiere :
003400030117      *
003500030117      * - Schiera tipi record da trasmettere in partenza
003600030117     d AR5k            s                   dim(50)  inz  like(AR5trd)
003700030117     d AR5d            s                   dim(50)  inz  like(dAR5)
003800060320      * - Schiera tipi record da non trasmettere
003900060320     d AR5n            s                   dim(50)  inz  like(AR5trd)
004000060320     D* - Schiera tipi record non presenti in tabella AR5
004100060320     D AR5M            S                   DIM(50)  INZ  LIKE(AR5TRD)
004200030117      * - Schiera numeri record da trasmettere per P.O.
004300040308      *   ("legata" alle schiere SKPO)
004400030117     d $Rec            s              7  0 dim(EL_skPO) inz                         Numero Record
004500030117     d SKok            s              1    dim(EL_skPO) inz(*all'0')
004600040308      * - Schiera p.o. a cui inviare
004700040308     d SKpo            s              3  0 dim(EL_skPO)
004800040308      * - Schiera p.o. presenti sullo stesso s.i. di simfel
004900040308     d  skias          s              3  0 dim(EL_skPO)
005000060320     D MSG             S             70    DIM(2) CTDATA PERRCD(1)
005100030117      *
005200030117      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
005300030117      * Strutture Dati :
005400030117      *
005500030117     d KPJBA         e ds
005600030117     d UT§DSE0F      e ds
005700030117     d CNCR80        e ds
005800030117      * - Ds x definizione tipi record FIAR5
005900030117     d dAR5          e ds                  inz
006000030117      * - Ds x trasmissione dati
006100030117     d DSCMZ         e ds                  inz
006200030117      * - Ds x Reperimento librerie e Sif.da/a cui trasmettere
006300030117     d TIBS55ds      e ds                  inz
006400030117      * - Ds x reperimento lista AS/P.O. a cui NON trasmettere
006500030117     d TIBS56DS      e ds
006600030117     d  ski                    7    756  0 dim(EL_skPO)
006700030117      * - Ds x caricamento tutti terminal
006800040308     d**!!! TRUL26DS      e ds
006900040308     d**!!!  skpo                  11    760  0 dim(EL_skPO)
007000040308     d**!!!  skpot                761   1510  0 dim(EL_skPO)
007100040309     d Fnlv55ds      e ds
007200030117      *
007300030117     d DSmembro        ds             7    inz
007400030117     d  MbrM                   1      1    inz('M')
007500030117     d  MbrPOP                 2      4  0 inz
007600030117     d  MbrPOA                 5      7  0 inz
007700030117      *
007800030117     d PARAM5          ds                  inz
007900030117     d  LEGFIL                 1     10
008000030117     d  MBRFIL                11     17
008100030117     d  BLKFIL                18     23
008200030117      *
008300030117     d WLBDAT          ds                  inz
008400030117     d  G02DAT                 1      8  0 inz
008500030117     d  G02INV                 9     16  0 inz
008600030117     d  G02ERR                17     17    inz
008700030117     d  G02TGI                18     22  0 inz
008800030117      *
008900030117      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
009000030117      * Variabili
009100030117      *
009200030117      * - Flag
009300030117     d $RecOK          s              1    inz(*off)
009400030117     d $Err            s              1    inz(*off)
009500030117      * - Indici di schiere
009600030117     d XX              s              3  0 inz
009700060320     d yy              s              3  0 inz
009800030117     d XF              s              3  0 inz
009900030117     d XT              s              3  0 inz
010000030117      * - Campi di comodo
010100030117     d Wn14            s             14  0                                      *numerico 14
010200030117     d Wn08            s              8  0                                      *numerico 8
010300030117     d Woggi           s              8  0                                      *d.corrente aa/m/g
010400030117     d Wora6           s              6  0 inz
010500030117     d Wora2           s              2  0 inz
010600030117     d SAVlnp          s                   like(AR5lnp) inz(*loval)
010700030117     d SAVtrd          s                   like(AR5trd) inz(*loval)
010800030117     d $RecSEDE        s              7  0 inz                                  Numero Record
010900030117     d Wrec            s              7  0 inz                                  Numero Record
011000040309     d Wtfp            s              3  0
011100030117
011200030117      *---------------------------------------------------------------*
011300030117
011400030117      * Ciclo di lettura
011500030117     c                   read      FIAR5000
011600030117      *
011700030117do  1c                   dow       not %eof(FIAR502L)
011800030117      *
011900030117      * - Controllo se devo trasmettere record ai P.O. e/o alla Sede
012000030117     c                   exsr      CtrREC
012100030117      *
012200030117if  2c                   if        $RecOK = *on
012300030117      *
012400030117      * - - apertura membro sede
012500030117if  3c                   if        Not %open(FIAR546T)
012600030117     c                             and §AR5trse = 'S'
012700030117     c                   exsr      OpnMbrSEDE
012800030117e   3c                   endif
012900030117      *
013000030117      * - - apertura membro LNP a rottura
013100030117if  3c                   if        AR5lnp <> SAVlnp
013200030117     c                             and §AR5trpa = 'S'
013300030117     c                   exsr      OpnMbrLNP
013400030117e   3c                   endif
013500030117      *
013600030117      * Gestisco scrittura se record OK:
013700030117      * - scrivo il record nei membri di trasmissione
013800030117      * - aggiorno il record come trasmesso
013900030117if  3c                   if        not *in97
014000030117     c                             and $Err = *off
014100030117     c                   exsr      GesREC
014200030117e   3c                   endif
014300030117      *
014400030130      *  Se non prevista trasmissione x lnp e sede fleggo comunque
014500030130   x2c                   else
014600030130     c                   exsr      AggFlgTra
014700030117e   2c                   endif
014800030117      *
014900030117     c                   read      FIAR5000
015000030117      *
015100030117e   1c                   enddo
015200030117      *
015300030117      * Chiudo i file
015400030117     c                   exsr      ClosePF
015500030117      *
015600030117      * Raggruppo i dati da trasferire per terminal di partenza
015700030117      *   (ovvero per P.O. con elaboratore)
015800030117     c                   exsr      C_SKok
015900030117      * Sottometto trasmissioni ai P.O. con elaboratore
016000030117do  1c     1             do        EL_skPO       XF
016100030117if  2c                   if        SKok(XF) = *on
016200030117     c                   exsr      TrasmPO
016300030117e   2c                   endif
016400030117e   1c                   enddo                                                  *fine filiali
016500030117      *
016600030117      * Se ho scritto almeno 1 record gestisco l'invio dei dati in SEDE
016700030117if  1c                   if        $RecSEDE > 0
016800030117     c                   exsr      TrasmSEDE
016900030117e   1c                   endif
017000030117      *
017100030117      * Chiusura programma
017200030117     c                   clear                   TIBS55ds
017300030117     c                   movel     'C'           I50tla
017400030117     c                   call      'TIBS55R'
017500030117     c                   parm                    TIBS55ds
017600030117      *
017700030117     c                   eval      *inLR = *on
017800030117
017900030117      **-------------------------------------------------------------**
018000030117      ** Operazioni iniziali                                         **
018100030117      **-------------------------------------------------------------**
018200030117     c     *InzSR        BEGSR
018300030117      *
018400030117     c     *Entry        Plist
018500030117     c                   parm                    KPJBA
018600030117      *
018700030117     c                   call      'X§PARUT'
018800030117     c                   parm                    UT§DSE0F
018900030117     c                   movel     REC80         CNCR80
019000030117      *
019100030117      * calcola la data corrente
019200030117     c                   time                    Wn14                           *ora(6) + data(8)
019300030117     c                   move      Wn14          Wn08                           *data(8) in g/m/aa
019400030117     c                   z-add     Wn08          G02dat
019500030117     c                   clear                   G02inv
019600030117     c                   movel     *off          G02err
019700030117     c                   call      'XSRDA8'
019800030117     c                   parm                    WLBDAT
019900030117     c                   z-add     G02inv        Woggi                          *d.corrente aa/m/g
020000030117      *
020100030117      * Carico la schiera dei P.O. presenti nel S.I.
020200030117      *
020300040308     c                   Clear                   skias
020400030117     c                   clear                   TIBS56DS
020500030117     c                   eval      I56ppo = SIMfel
020600030117     c                   eval      I56mod = 'POS'
020700030117     c                   call      'TIBS56R'
020800030117     c                   parm                    TIBS56DS
020900040308     c**!!!              if        O56err <> *blanks
021000040308     c**!!!              clear                   SKI
021100040308     c**!!!              endif
021200040308     c                   If        O56Err = *Blanks
021300040308     c                   Movea     Ski           skias
021400040308     c                   EndIf
021500040308
021600040308      * Carico la schiera dei P.O. a cui inviare
021700040308      *
021800040308     c                   Clear                   skpo
021900040308     c                   clear                   TIBS56DS
022000040308     c                   eval      I56ppo = SIMfel
022100040308     c                   eval      I56mod = 'AZI'
022200040308     c                   call      'TIBS56R'
022300040308     c                   parm                    TIBS56DS
022400040308     c                   If        O56Err = *Blanks
022500040308     c                   Movea     Ski           skpo
022600040308     c                   EndIf
022700030117      *
022800030117      * Carico le schiere dei P.O. e dei relativi ORGFEL o §OGAS
022900030117      *
023000040308     c**!!!              clear                   TRUL26DS
023100040308     c**!!!              eval      I26tpt = 'G'
023200040308     c**!!!              eval      I26tla = 'L'
023300040308     c**!!!              call      'TRUL26R'
023400040308     c**!!!              parm                    TRUL26DS
023500030117      *
023600030117      * Reperisco l'elenco di librerie per la trasmissione
023700030117      *
023800030117     c                   clear                   TIBS55DS
023900030117     c                   movel     KNSIF         I50psi
024000030117     c                   call      'TIBS55R'
024100030117     c                   parm                    TIBS55DS
024200030117      *
024300030117      *  Effettuo caricamento dei tipi record da trasmettere
024400030117     c                   clear                   xx
024500060320     c                   clear                   yy
024600030117     c                   eval      TBEcod = 'AR5'
024700030117     c     TBEcod        setll     TNTBE000
024800030117     c     TBEcod        reade     TNTBE000
024900030117do  1c                   dow       Not %eof(TNTBE01L)
025000030117     c                             and XX < %elem(AR5k)
025100030117if  2c                   if        TBEatb = *blanks
025200030117     c                   movel     TBEuni        dAR5
025300030117if  3c                   if           §AR5trpa = 'S'
025400030117     c                             or §AR5trse = 'S'
025500030117     c                   add       1             xx
025600030117     c                   movel     TBEke1        AR5k(xx)
025700030117     c                   movel     TBEuni        AR5d(xx)
025800060320     c                   else
025900060320     c                   add       1             yy
026000060320     c                   movel     tbeke1        ar5n(yy)
026100030117e   3c                   endif
026200030117e   2c                   endif
026300030117     c     TBEcod        reade     TNTBE000
026400030117e   1c                   enddo
026500030117      *
026600030117     c                   ENDSR
026700030117
026800030117      **-------------------------------------------------------------**
026900030117      ** Controllo se tipo record rientra fra quelli da trasmettere  **
027000030117      **-------------------------------------------------------------**
027100030117     c     CtrREC        BEGSR
027200030117      *
027300030117if  1c                   if        AR5trd <> SAVtrd
027400030117      *
027500030117     c                   clear                   dAR5
027600030117     c                   eval      SAVtrd = AR5trd
027700030117     c                   eval      $RecOK = *off
027800030117     c                   eval      xx     = 1
027900030117     c     AR5trd        lookup    AR5k(xx)                               30
028000030117      *
028100030117if  2c                   if        *in30
028200030117     c                   movel     AR5d(xx)      dAR5
028300030117     c                   eval      $RecOK = *on
028400060320     c                   else
028500060320     c                   eval      xx     = 1
028600060320     c     ar5trd        lookup    ar5n(xx)                               30
028700060320if  2c                   if        not *in30
028800060320     c                   eval      xx     = 1
028900060320     c     AR5trd        lookup    AR5m(xx)                               30
029000060320      *
029100060320if  2c                   if        not *in30
029200120302     C                   eval      wrkEml= 'Ced@brt.it'
029300060320     C* TESTATA MESSAGGIO
029400060320     C                   MOVEL     MSG(1)        MSGTES
029500111026     c                   eval      %subst(msgtes:40:3)=ar5trd
029600060320     C                   MOVEL     MSG(2)        MSGDET
029700111026     c                   call      'TIS701C'
029800111026     C                   parm                    wrkEml          253
029900111026     C                   PARM                    MSGTES           44
030000111026     C                   PARM                    MSGDET         5000
030100060320     c*
030200060320     c     '   '         lookup    ar5m(xx)                               30
030300060320     c   30              movel     ar5trd        ar5m(xx)
030400060320     c                   endif
030500060320     c                   endif
030600030117e   2c                   endif
030700030117      *
030800030117e   1c                   endif
030900030117      *
031000030117     c                   ENDSR
031100030117
031200030117      **-------------------------------------------------------------**
031300030117      ** Apertura file / membro di trasmissione dati a SEDE          **
031400030117      **-------------------------------------------------------------**
031500030117     c     OpnMbrSEDE    BEGSR
031600030117      *
031700030117      * Imposto il nome membro
031800030117     c                   reset                   DSmembro
031900030117     c                   eval      MbrPOp = SIMfel
032000030117     c                   eval      MbrPOa = 046
032100030117      *
032200030117      * Richiamo cl TRUL25C - tipo '1':
032300030117      * - controlla se esiste il membro
032400030117      * - se inesistente: aggiunge tale membro al file
032500030117      * - alloca il membro del file
032600030117      * - esegue la sovrapposizione sul membro
032700030117     c                   eval      UL25tc = '1'
032800030117     c                   eval      UL25pf = 'FIAR546T  '
032900030117     c                   exsr      CallTRUL25
033000030117      *
033100030117      * Se 97 acceso vuol dire che non e' stato possibile allocare il
033200030117      *  membro interessato
033300030117     c  N97              open      FIAR546T
033400030117      *
033500030117     c                   ENDSR
033600030117
033700030117      **-------------------------------------------------------------**
033800030117      ** Apertura file / membro di trasmissione dati a P.O. LNP      **
033900030117      **-------------------------------------------------------------**
034000030117     c     OpnMbrLNP     BEGSR
034100030117      *
034200030117      * Chiudo il file se aperto sul membro del precedente P.O.  e
034300030117      * Richiamo cl TRUL25C - tipo '7':
034400030117      * - elimina la sovrapposizione sul membro
034500030117      *   (lasciandolo allocato)
034600030117if  1c                   if        %open(FIAR500T)
034700030117     c                   close     FIAR500T
034800030117     c                   eval      UL25tc = '7'
034900030117     c                   eval      UL25pf = 'FIAR500T  '
035000030117     c                   exsr      CallTRUL25
035100030117e   1c                   endif
035200030117      *
035300030117     c                   eval      SAVlnp = AR5lnp
035400030117     c                   setoff                                       9798
035500030117     c                   eval      $Err = *off
035600040309      * Cerco il terminal di partenza della ar5lnp
035700040309     c                   Clear                   Fnlv55ds
035800040309     c                   Eval      d55tpt = 'P'
035900040309     c                   Eval      d55lin = Ar5Lnp
036000040309     c                   Eval      d55drf = woggi
036100040309     c                   Call      'FNLV55R'
036200040309     c                   Parm                    Fnlv55ds
036300040309     c                   Eval      wtfp = d55tfp
036400030117      * Controllo se locale
036500040309     c**!!!AR5lnp        lookup    SKIas                                  98
036600040309     c     wtfp          lookup    SKIas                                  98
036700030117      *
036800030117      * Scrivo solo se non è locale
036900030117if  1c                   if        not *in98
037000030117      *
037100030117      * Controllo il P.O. a cui trasmettere
037200030117     c                   exsr      CtrPOt
037300030117if  2c                   if        $Err = *off
037400030117      *
037500030117      * Imposto il nome membro
037600030117     c                   reset                   DSmembro
037700030117     c                   eval      MbrPOp = SIMfel
037800040308     c                   eval      MbrPOa = SKpo(XF)
037900030117      *
038000030117      * Richiamo cl TRUL25C - tipo '1':
038100030117      * - controlla se esiste il membro
038200030117      * - se inesistente: aggiunge tale membro al file
038300030117      * - alloca il membro del file
038400030117      * - esegue la sovrapposizione sul membro
038500030117     c                   eval      UL25tc = '1'
038600030117     c                   eval      UL25pf = 'FIAR500T  '
038700030117     c                   exsr      CallTRUL25
038800030117      *
038900030117      * Se 97 acceso vuol dire che non e' stato possibile allocare il
039000030117      *  membro interessato
039100030117     c  N97              open      FIAR500T
039200030117      *
039300030117e   2c                   endif
039400030117      *
039500030117e   1c                   endif
039600030117      *
039700030117     c                   ENDSR
039800030117
039900030117      **-------------------------------------------------------------**
040000030117      ** Richiamo cl TRUL25C                                         **
040100030117      **-------------------------------------------------------------**
040200030117     c     CallTRUL25    BEGSR
040300030117      *
040400030117     c                   call      'TRUL25C'                            97
040500030117     c                   parm                    UL25tc            1
040600030117     c                   parm                    UL25pf           10
040700030117     c                   parm      O50pla        UL25li           10
040800030117     c                   parm      DSmembro      UL25mb           10
040900030117     c                   parm      *blanks       UL25tx           10
041000030117     c                   parm      *blanks       UL25fr            1
041100030117     c                   parm      *blanks       UL25os            1
041200030117     c                   parm      *zeros        UL25es            2
041300030117      *
041400030117     c  N97UL25es        comp      *zeros                             9797
041500030117      *
041600030117     c                   ENDSR
041700030117
041800030117      **-------------------------------------------------------------**
041900030117      ** Reperisco il P.O. a cui trasmettere i dati                  **
042000030117      **-------------------------------------------------------------**
042100030117     c     CtrPOt        BEGSR
042200030117      *
042300030117     c                   eval      $Err = *off
042400030117      * Reperisco il P.O. al quale devo trasmettere
042500030117     c                   movel     *off          *in30
042600030117     c                   z-add     1             XF
042700040309     c**!!!AR5lnp        LOOKUP    SKpo(XF)                               30
042800040309     c     wtfp          LOOKUP    SKpo(XF)                               30
042900030117      *
043000030117      * - se non reperito lo memorizzo comunque (chi sarà mai ???)
043100030117if  1c                   if        *in30 = *off
043200030117     c                   z-add     1             XF
043300030117     c     *zeros        lookup    SKpo(XF)                               30
043400030117if  2c                   if        *in30 = *on
043500040309     c**!!!              move      AR5lnp        SKpo(XF)
043600040309     c                   move      wtfp          SKpo(XF)
043700040308     c**!!!              move      AR5lnp        SKpoT(XF)
043800030117x   2c                   else
043900030117      * - - se non reperito né memorizzabile: non trasmetto
044000030117     c                   eval      $Err = *on
044100030117e   2c                   endif
044200030117e   1c                   endif
044300030117      *
044400030117      * Imposto un flag che mi indichi i P.O. per cui ci sono dati
044500030117if  1c                   if        *in30 = *on
044600030117     c                   eval      SKok(XF) = *on
044700030117e   1c                   endif
044800030117      *
044900030117     c                   ENDSR
045000030117
045100030117      **-------------------------------------------------------------**
045200030117      ** Scrittura record x trasmissione                             **
045300030117      **-------------------------------------------------------------**
045400030117     c     GesREC        BEGSR
045500030117      *
045600030117      * Se richiesta la trasmissione al P.O. di partenza
045700030117      *  e tale P.O. NON è locale:
045800030117     c                   if            §AR5trpa = 'S'
045900030117     c                             and not *in98
046000030117      * Scrivo record x trasmissione in filiale
046100030117     c                   WRITE     FIAR50T
046200030117      * Conto i record da trasmettere per P.O.
046300030117     c                   add       1             $Rec(XF)                       *n°record x P.O.
046400030117     c                   endif
046500030117      *
046600030117      * Se richiesta la trasmissione alla SEDE:
046700030117     c                   if        §AR5trse = 'S'
046800030117      * Scrivo record x trasmissione in sede
046900030117     c                   WRITE     FIAR5ST
047000030117      * Conto i record da trasmettere in sede
047100030117     c                   add       1             $RecSEDE                       *n°record x SEDE
047200030117     c                   endif
047300030130      *
047400030130      * Aggiorno file 00F come trasmesso
047500030130     c                   exsr      AggFlgTra
047600030117      *
047700030117     c                   ENDSR
047800030130      **-------------------------------------------------------------**
047900030130      ** AGGIORNO FILE FISICO COME TRASMESSO                         **
048000030130      **-------------------------------------------------------------**
048100030130     c     AggFlgTra     BEGSR
048200030130      *
048300030130     c                   movel     'T'           AR5ft1                         *flag trasm. P.O.
048400030130     c                   z-add     Woggi         AR5dt1                         *data trasm. P.O.
048500030130     c                   movel     'T'           AR5ft2                         *flag trasm. SEDE
048600030130     c                   z-add     Woggi         AR5dt2                         *data trasm. SEDE
048700030130     c                   UPDATE    FIAR5000
048800030130      *
048900030130     c                   ENDSR
049000030117      **-------------------------------------------------------------**
049100030117      ** Chiusura dei file                                           **
049200030117      **-------------------------------------------------------------**
049300030117     c     ClosePF       BEGSR
049400030117      *
049500030117      * Chiudo il file se aperto sul membro dell'ultimo P.O.  e
049600030117      * Richiamo cl TRUL25C - tipo '7':
049700030117      * - elimina la sovrapposizione sul membro
049800030117      *   (lasciandolo allocato)
049900030117if  1c                   if        %open(FIAR500T)
050000030117      *
050100030117     c                   close     FIAR500T
050200030117      *
050300030117     c                   reset                   DSmembro
050400030117     c                   eval      MbrPOp = SIMfel
050500040308     c                   eval      MbrPOa = SKpo(XF)
050600030117     c                   eval      UL25tc = '7'
050700030117     c                   eval      UL25pf = 'FIAR500T  '
050800030117     c                   exsr      CallTRUL25
050900030117      *
051000030117e   1c                   endif
051100030117      *
051200030117      * Chiudo il file di Sede
051300030117if  1c                   if        %open(FIAR546T)
051400030117      *
051500030117     c                   close     FIAR546T
051600030117      *
051700030117     c                   reset                   DSmembro
051800030117     c                   eval      MbrPOp = SIMfel
051900030117     c                   eval      MbrPOa = 046
052000030117     c                   eval      UL25tc = '7'
052100030117     c                   eval      UL25pf = 'FIAR546T  '
052200030117     c                   exsr      CallTRUL25
052300030117      *
052400030117e   1c                   endif
052500030117      *
052600030117     c                   ENDSR
052700030117
052800030117      **-------------------------------------------------------------**
052900040308      ** Sistemazione schiere SKok e $Rec in base alla schiera SKpo  **
053000030117      **-------------------------------------------------------------**
053100030117     c     C_SKOK        BEGSR
053200030117      *
053300030117      * totalizzo il numero di record del membro per TEMINAL
053400030117      *  a cui trasmettere
053500030117do  1c     1             do        EL_skPO       XF
053600030117     c*  - per ognuno dei P.O. elaborati cerco il terminal
053700030117if  2c                   if        SKok(XF) = *on
053800030117     c                   eval      XT = 1
053900040308     c     SKpo(XF)      lookup    SKpo(XT)                               30
054000030117     c*  - e ne totalizzo il numero di record da trasmettere
054100030117     c*    (ripulendone il conteggio specifico del P.O. elaborato)
054200030117if  3c                   if        *in30
054300030117     c                             and XF <> XT
054400030117     c                   movel     *on           SKok(XT)
054500030117     c                   add       $Rec(XF)      $Rec(XT)
054600030117     c                   clear                   $Rec(XF)
054700030117     c                   movel     *off          SKok(XF)
054800030117e   3c                   endif
054900030117e   2c                   endif
055000030117e   1c                   enddo
055100030117      *
055200030117     c                   ENDSR
055300030117
055400030117      **-------------------------------------------------------------**
055500030117      ** Trasmissione del membro ai P.O.                             **
055600030117      **-------------------------------------------------------------**
055700030117     c     TrasmPO       BEGSR
055800030117      *
055900030117      * Cerco sistema infomativo di arrivo
056000030117     c                   clear                   TIBS55DS
056100030117     c                   z-add     SIMfel        I50ppo
056200040308     c                   z-add     SKpo(XF)      I50apo
056300030117     c                   call      'TIBS55R'
056400030117     c                   parm                    TIBS55DS
056500030117      *
056600030117      * Eseguo la trasmissione vera e propria
056700030117     c                   eval      wRec = $Rec(XF)
056800030117     c                   exsr      CallTIBS53
056900030117      *
057000030117     c                   ENDSR
057100030117
057200030117      **-------------------------------------------------------------**
057300030117      ** Trasmissione del membro alla SEDE                           **
057400030117      **-------------------------------------------------------------**
057500030117     c     TrasmSEDE     BEGSR
057600030117      *
057700030117      * cerco sistema infomativi di arrivo
057800030117     c                   clear                   TIBS55DS
057900030117     c                   z-add     SIMfel        I50ppo
058000030117     c                   z-add     046           I50apo
058100030117     c                   call      'TIBS55R'
058200030117     c                   parm                    TIBS55DS
058300030117      *
058400030117      * Eseguo la trasmissione vera e propria
058500030117     c                   eval      wRec = $RecSEDE
058600030117     c                   exsr      CallTIBS53
058700030117      *
058800030117     c                   ENDSR
058900030117
059000030117      **-------------------------------------------------------------**
059100030117      ** Impostazione parametri e richiamo pgm. di trasmissione      **
059200030117      **-------------------------------------------------------------**
059300030117     c     CallTIBS53    BEGSR
059400030117      *
059500030117     c                   movel     O50lcl        CMZlcl                          *locale
059600030117      * parto sempre dalla libreria aziendale
059700030117     c                   movel     O50pla        CMZlbd                          *dalla libreria
059800030117      * e vado sempre alla libreria aziendale per i P.O.,
059900030117      *    ... sempre alla libreria di gruppo per la Sede
060000030117     c                   if        I50apo = 046
060100030117     c                   movel     O50alg        CMZlba                          *alla  libreria
060200030117     c                   else
060300030117     c                   movel     O50ala        CMZlba                          *alla  libreria
060400030117     c                   endif
060500030117     c                   movel     O50asi        CMZsyf                          *sist.informativo
060600030117      *
060700030117     c                   reset                   DSmembro
060800030117     c                   eval      MbrPOp = SIMfel
060900030117     c                   eval      MbrPOa = I50apo
061000030117      *
061100030117if  1c                   IF        Wrec > *zeros                                 *se scritti dei rec
061200030117      *
061300030117     c                   move      I50apo        CMZdst                          *destinatario
061400030117     c                   if        I50apo = 046
061500030117      * trasmetto dal file 46T alla sede
061600030117     c                   movel     'FIAR546T  '  CMZfld                          *dal file -trasm-
061700030117     c                   else
061800030117      * trasmetto dal file 00T ai vari P.O.
061900030117     c                   movel     'FIAR500T  '  CMZfld                          *dal file -trasm-
062000030117     c                   endif
062100030117     c                   movel     'FIAR500R  '  CMZfla                          *al file  -ricez-
062200030117     c                   movel(p)  DSmembro      CMZmbd                          *dal membro
062300030117     c                   movel(p)  DSmembro      CMZmba                          *al membro
062400030117     c                   move      '0000'        CMZori                          *ora invio
062500030117     c                   time                    wOra6
062600030117     c                   movel     wOra6         wOra2
062700030117     c                   add       1             wOra2
062800030117     c                   movel     wOra2         CMZori
062900030117     c                   move      '0000000'     CMZdti                          *data invio
063000030117     c                   move      Woggi         CMZdti
063100030117     c                   movel     'N'           CMZurg                          *urgenza notte
063200030117     c                   movel     'A'           CMZadd                          *record in aggiunta
063300030117     c                   movel     'S'           CMZclr                          *no pulizia file
063400030117     c                   movel     'S'           CMZftr                          *invio da transito
063500030117     c                   movel     *blanks       CMZerr                          *flag errore('1')
063600030117     c                   if        I50apo = 046
063700030117     c                   movel     'TRN2'        CMZazn                          *azione ricezione
063800030117     c                   else
063900030117     c                   movel     'LRN3'        CMZazn                          *azione ricezione
064000030117     c                   endif
064100030117     c                   move      Wrec          CMZnrr                          *record inviati
064200030117     c                   movel     *blanks       CMZprr                          *prerequisito
064300030117     c                   movel(p)  Param5        w236
064400030117     c                   call      'TIBS53C'
064500030117     c                   parm                    CMZlcl            1
064600030117     c                   parm                    CMZdst
064700030117     c                   parm                    CMZfld
064800030117     c                   parm                    CMZmbd
064900030117     c                   parm                    CMZlbd
065000030117     c                   parm                    CMZfla
065100030117     c                   parm                    CMZmba
065200030117     c                   parm                    CMZlba
065300030117     c                   parm                    CMZori
065400030117     c                   parm                    CMZdti
065500030117     c                   parm                    CMZurg
065600030117     c                   parm                    CMZadd
065700030117     c                   parm                    CMZclr
065800030117     c                   parm                    CMZftr
065900030117     c                   parm                    CMZerr
066000030117     c                   parm                    CMZazn
066100030117     c                   parm                    CMZsyf
066200030117     c                   parm                    CMZnrr
066300030117     c                   parm                    CMZprr
066400030117     c                   parm                    w236            236
066500030117e   1c                   endif
066600030117      *
066700030117      * Disallocazione e Cancellazione Override dal membro
066800030117     c                   eval      UL25tc = '9'
066900030117     c                   eval      UL25pf = CMZfld
067000030117     c                   exsr      CallTRUL25
067100030117      *
067200030117     c                   ENDSR
067300060320**         MSG
067400111026TRASMISSIONE file FIAR500T tipo record xxx
067500060320Tipo Record mancante in tabella "AR5"
