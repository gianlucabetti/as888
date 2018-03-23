000100030117     h decedit('0,') datedit(*dmy.)
000200030117
000300030117      *---------------------------------------------------------------*
000400030117
000500031203     fFnblp01L  If   e           k disk
000600031203     fFIAR501L  Uf   e           k disk
000700030117     fFIAR500T  O    e             disk    usropn
000800030117     f                                     rename(FIAR5000:FIAR50T)
000900030117
001000030117      *---------------------------------------------------------------*
001100030117
001200030117      * Costanti :
001300030117      *
001400030117     d EL_skPO         c                   const(250)
001500030117      *
001600030117      * Schiere :
001700030117      *
001800030117      * - Schiera numeri record da trasmettere per P.O.
001900030117      *   ("legata" alle schiere SKPO / SKPOT)
002000030117     d $Rec            s              7  0 dim(EL_skPO) inz                         Numero Record
002100030117     d SKok            s              1    dim(EL_skPO) inz(*all'0')
002200030117      *
002300030117      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
002400030117      * Strutture Dati :
002500030117      *
002600030117     d KPJBA         e ds
002700030117     d UT§DSE0F      e ds
002800030117     d CNCR80        e ds
002900030117      * - Ds x trasmissione dati
003000030117     d DSCMZ         e ds                  inz
003100030117      * - Ds x Reperimento librerie e Sif.da/a cui trasmettere
003200030117     d TIBS55ds      e ds                  inz
003300030117      * - Ds x reperimento lista AS/P.O. a cui NON trasmettere
003400030117     d TIBS56DS      e ds
003500030117     d  ski                    7    756  0 dim(EL_skPO)
003600030117      * - Ds x caricamento tutti terminal
003700030117     d TRUL26DS      e ds
003800030117     d  skpo                  11    760  0 dim(EL_skPO)
003900030117     d  skpot                761   1510  0 dim(EL_skPO)
004000030117      *
004100030117     d DSmembro        ds             7    inz
004200030117     d  MbrM                   1      1    inz('M')
004300030117     d  MbrPOP                 2      4  0 inz
004400030117     d  MbrPOA                 5      7  0 inz
004500030117      *
004600030117     d PARAM5          ds                  inz
004700030117     d  LEGFIL                 1     10
004800030117     d  MBRFIL                11     17
004900030117     d  BLKFIL                18     23
005000030117      *
005100030117     d WLBDAT          ds                  inz
005200030117     d  G02DAT                 1      8  0 inz
005300030117     d  G02INV                 9     16  0 inz
005400030117     d  G02ERR                17     17    inz
005500030117     d  G02TGI                18     22  0 inz
005600030117      *
005700030117      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
005800030117      * Variabili
005900030117      *
006000030117      * - Flag
006100030117     d $RecOK          s              1    inz(*off)
006200030117     d $Err            s              1    inz(*off)
006300030117      * - Indici di schiere
006400030117     d XX              s              3  0 inz
006500030117     d XF              s              3  0 inz
006600030117     d XT              s              3  0 inz
006700030117      * - Campi di comodo
006800030117     d Wn14            s             14  0                                      *numerico 14
006900030117     d Wn08            s              8  0                                      *numerico 8
007000030117     d Woggi           s              8  0                                      *d.corrente aa/m/g
007100030117     d Wora6           s              6  0 inz
007200030117     d Wora2           s              2  0 inz
007300031203     d SAVlna          s                   like(AR5lnp) inz(*loval)
007400030117     d SAVtrd          s                   like(AR5trd) inz(*loval)
007500030117     d $RecSEDE        s              7  0 inz                                  Numero Record
007600030117     d Wrec            s              7  0 inz                                  Numero Record
007700030117
007800030117      *---------------------------------------------------------------*
007900030117
008000030117      * Ciclo di lettura
008100031203do  1c                   do        *Hival
008200031203     c                   read      FIAR5000
008300031203      * fine file
008400031203     c                   If        %eof(FIAR501L)
008500031203     c                   Leave
008600031203     c                   EndIF
008700031203      * solo rcd 'GEN'
008800031203     c                   if        AR5trd <> 'GEN'
008900031203     c                   iter
009000031203     c                   endif
009100031203      * solo i trasmessi
009200031203     c                   if        ar5ft1 <> 'T'
009300031203     c                   iter
009400031203     c                   endif
009500031203      * = a 03/12/03
009600031203     c                   if        ar5dt1 > 20031203
009700031203     c                   iter
009800031203     c                   endif
009900031203      * aggancio la bolla partenza
010000031203     c     kblp          chain     fnblp01l
010100031203if  2c                   If        %found(fnblp01l)
010200031203      * - - apertura membro LNA a rottura
010300031203if  3c                   if        blplna <> savlna
010400031203     c                   exsr      OpnMbrLNa
010500031203e   3c                   endif
010600030117      *
010700030117      * Gestisco scrittura se record OK:
010800030117      * - scrivo il record nei membri di trasmissione
010900030117      * - aggiorno il record come trasmesso
011000030117if  3c                   if        not *in97
011100030117     c                             and $Err = *off
011200030117     c                   exsr      GesREC
011300030117e   3c                   endif
011400031203      *
011500030117e   2c                   endif
011600030117      *
011700030117e   1c                   enddo
011800030117      *
011900030117      * Chiudo i file
012000030117     c                   exsr      ClosePF
012100030117      *
012200030117      * Raggruppo i dati da trasferire per terminal di partenza
012300030117      *   (ovvero per P.O. con elaboratore)
012400030117     c                   exsr      C_SKok
012500030117      * Sottometto trasmissioni ai P.O. con elaboratore
012600030117do  1c     1             do        EL_skPO       XF
012700030117if  2c                   if        SKok(XF) = *on
012800030117     c                   exsr      TrasmPO
012900030117e   2c                   endif
013000030117e   1c                   enddo                                                  *fine filiali
013100030117      *
013200030117      * Chiusura programma
013300030117     c                   clear                   TIBS55ds
013400030117     c                   movel     'C'           I50tla
013500030117     c                   call      'TIBS55R'
013600030117     c                   parm                    TIBS55ds
013700030117      *
013800030117     c                   eval      *inLR = *on
013900030117
014000030117      **-------------------------------------------------------------**
014100030117      ** Operazioni iniziali                                         **
014200030117      **-------------------------------------------------------------**
014300030117     c     *InzSR        BEGSR
014400030117      *
014500030117     c     *Entry        Plist
014600030117     c                   parm                    KPJBA
014700030117      *
014800030117     c                   call      'X§PARUT'
014900030117     c                   parm                    UT§DSE0F
015000030117     c                   movel     REC80         CNCR80
015100030117      *
015200030117      * calcola la data corrente
015300030117     c                   time                    Wn14                           *ora(6) + data(8)
015400030117     c                   move      Wn14          Wn08                           *data(8) in g/m/aa
015500030117     c                   z-add     Wn08          G02dat
015600030117     c                   clear                   G02inv
015700030117     c                   movel     *off          G02err
015800030117     c                   call      'XSRDA8'
015900030117     c                   parm                    WLBDAT
016000030117     c                   z-add     G02inv        Woggi                          *d.corrente aa/m/g
016100030117      *
016200030117      * Carico la schiera dei P.O. presenti nel S.I.
016300030117      *
016400030117     c                   clear                   TIBS56DS
016500030117     c                   eval      I56ppo = SIMfel
016600030117     c                   eval      I56mod = 'POS'
016700030117     c                   call      'TIBS56R'
016800030117     c                   parm                    TIBS56DS
016900030117     c                   if        O56err <> *blanks
017000030117     c                   clear                   SKI
017100030117     c                   endif
017200030117      *
017300030117      * Carico le schiere dei P.O. e dei relativi ORGFEL o §OGAS
017400030117      *
017500030117     c                   clear                   TRUL26DS
017600030117     c                   eval      I26tpt = 'G'
017700030117     c                   eval      I26tla = 'L'
017800030117     c                   call      'TRUL26R'
017900030117     c                   parm                    TRUL26DS
018000030117      *
018100030117      * Reperisco l'elenco di librerie per la trasmissione
018200030117      *
018300030117     c                   clear                   TIBS55DS
018400030117     c                   movel     KNSIF         I50psi
018500030117     c                   call      'TIBS55R'
018600030117     c                   parm                    TIBS55DS
018700031203
018800031203     c     kblp          Klist
018900031203     c                   Kfld                    ar5aas
019000031203     c                   Kfld                    ar5lnp
019100031203     c                   Kfld                    ar5nrs
019200031203     c                   Kfld                    ar5nsp
019300030117      *
019400030117     c                   ENDSR
019500030117
019600030117      **-------------------------------------------------------------**
019700031203      ** Apertura file / membro di trasmissione dati a P.O. LNa      **
019800030117      **-------------------------------------------------------------**
019900031203     c     OpnMbrLNa     BEGSR
020000030117      *
020100030117      * Chiudo il file se aperto sul membro del precedente P.O.  e
020200030117      * Richiamo cl TRUL25C - tipo '7':
020300030117      * - elimina la sovrapposizione sul membro
020400030117      *   (lasciandolo allocato)
020500030117if  1c                   if        %open(FIAR500T)
020600030117     c                   close     FIAR500T
020700030117     c                   eval      UL25tc = '7'
020800030117     c                   eval      UL25pf = 'FIAR500T  '
020900030117     c                   exsr      CallTRUL25
021000030117e   1c                   endif
021100030117      *
021200031203     c                   eval      SAVlna = blplna
021300030117     c                   setoff                                       9798
021400030117     c                   eval      $Err = *off
021500030117      * Controllo se locale
021600031203     c     blplna        lookup    SKI                                    98
021700030117      *
021800030117      * Scrivo solo se non è locale
021900030117if  1c                   if        not *in98
022000030117      *
022100030117      * Controllo il P.O. a cui trasmettere
022200030117     c                   exsr      CtrPOt
022300030117if  2c                   if        $Err = *off
022400030117      *
022500030117      * Imposto il nome membro
022600030117     c                   reset                   DSmembro
022700030117     c                   eval      MbrPOp = SIMfel
022800030117     c                   eval      MbrPOa = SKpoT(XF)
022900030117      *
023000030117      * Richiamo cl TRUL25C - tipo '1':
023100030117      * - controlla se esiste il membro
023200030117      * - se inesistente: aggiunge tale membro al file
023300030117      * - alloca il membro del file
023400030117      * - esegue la sovrapposizione sul membro
023500030117     c                   eval      UL25tc = '1'
023600030117     c                   eval      UL25pf = 'FIAR500T  '
023700030117     c                   exsr      CallTRUL25
023800030117      *
023900030117      * Se 97 acceso vuol dire che non e' stato possibile allocare il
024000030117      *  membro interessato
024100030117     c  N97              open      FIAR500T
024200030117      *
024300030117e   2c                   endif
024400030117      *
024500030117e   1c                   endif
024600030117      *
024700030117     c                   ENDSR
024800030117
024900030117      **-------------------------------------------------------------**
025000030117      ** Richiamo cl TRUL25C                                         **
025100030117      **-------------------------------------------------------------**
025200030117     c     CallTRUL25    BEGSR
025300030117      *
025400030117     c                   call      'TRUL25C'                            97
025500030117     c                   parm                    UL25tc            1
025600030117     c                   parm                    UL25pf           10
025700030117     c                   parm      O50pla        UL25li           10
025800030117     c                   parm      DSmembro      UL25mb           10
025900030117     c                   parm      *blanks       UL25tx           10
026000030117     c                   parm      *blanks       UL25fr            1
026100030117     c                   parm      *blanks       UL25os            1
026200030117     c                   parm      *zeros        UL25es            2
026300030117      *
026400030117     c  N97UL25es        comp      *zeros                             9797
026500030117      *
026600030117     c                   ENDSR
026700030117
026800030117      **-------------------------------------------------------------**
026900030117      ** Reperisco il P.O. a cui trasmettere i dati                  **
027000030117      **-------------------------------------------------------------**
027100030117     c     CtrPOt        BEGSR
027200030117      *
027300030117     c                   eval      $Err = *off
027400030117      * Reperisco il P.O. al quale devo trasmettere
027500030117     c                   movel     *off          *in30
027600030117     c                   z-add     1             XF
027700031203     c     blplna        LOOKUP    SKpo(XF)                               30
027800030117      *
027900030117      * - se non reperito lo memorizzo comunque (chi sarà mai ???)
028000030117if  1c                   if        *in30 = *off
028100030117     c                   z-add     1             XF
028200030117     c     *zeros        lookup    SKpo(XF)                               30
028300030117if  2c                   if        *in30 = *on
028400031203     c                   move      blplna        SKpo(XF)
028500031203     c                   move      blplna        SKpoT(XF)
028600030117x   2c                   else
028700030117      * - - se non reperito né memorizzabile: non trasmetto
028800030117     c                   eval      $Err = *on
028900030117e   2c                   endif
029000030117e   1c                   endif
029100030117      *
029200030117      * Imposto un flag che mi indichi i P.O. per cui ci sono dati
029300030117if  1c                   if        *in30 = *on
029400030117     c                   eval      SKok(XF) = *on
029500030117e   1c                   endif
029600030117      *
029700030117     c                   ENDSR
029800030117
029900030117      **-------------------------------------------------------------**
030000030117      ** Scrittura record x trasmissione                             **
030100030117      **-------------------------------------------------------------**
030200030117     c     GesREC        BEGSR
030300030117      *
030400031203      * Se P.O. NON è locale:
030500031203     c                   if        not *in98
030600030117      * Scrivo record x trasmissione in filiale
030700030117     c                   WRITE     FIAR50T
030800030117      * Conto i record da trasmettere per P.O.
030900030117     c                   add       1             $Rec(XF)                       *n°record x P.O.
031000030117     c                   endif
031100030130      *
031200030130      * Aggiorno file 00F come trasmesso
031300030130     c                   exsr      AggFlgTra
031400030117      *
031500030117     c                   ENDSR
031600030130      **-------------------------------------------------------------**
031700030130      ** AGGIORNO FILE FISICO COME TRASMESSO                         **
031800030130      **-------------------------------------------------------------**
031900030130     c     AggFlgTra     BEGSR
032000030130      *
032100030130     c                   movel     'T'           AR5ft1                         *flag trasm. P.O.
032200030130     c                   z-add     Woggi         AR5dt1                         *data trasm. P.O.
032300030130     c                   movel     'T'           AR5ft2                         *flag trasm. SEDE
032400030130     c                   z-add     Woggi         AR5dt2                         *data trasm. SEDE
032500030130     c                   UPDATE    FIAR5000
032600030130      *
032700030130     c                   ENDSR
032800030117      **-------------------------------------------------------------**
032900030117      ** Chiusura dei file                                           **
033000030117      **-------------------------------------------------------------**
033100030117     c     ClosePF       BEGSR
033200030117      *
033300030117      * Chiudo il file se aperto sul membro dell'ultimo P.O.  e
033400030117      * Richiamo cl TRUL25C - tipo '7':
033500030117      * - elimina la sovrapposizione sul membro
033600030117      *   (lasciandolo allocato)
033700030117if  1c                   if        %open(FIAR500T)
033800030117      *
033900030117     c                   close     FIAR500T
034000030117      *
034100030117     c                   reset                   DSmembro
034200030117     c                   eval      MbrPOp = SIMfel
034300030117     c                   eval      MbrPOa = SKpoT(XF)
034400030117     c                   eval      UL25tc = '7'
034500030117     c                   eval      UL25pf = 'FIAR500T  '
034600030117     c                   exsr      CallTRUL25
034700030117      *
034800030117e   1c                   endif
034900030117      *
035000030117     c                   ENDSR
035100030117
035200030117      **-------------------------------------------------------------**
035300030117      ** Sistemazione schiere SKok e $Rec in base alla schiera SKpoT **
035400030117      **-------------------------------------------------------------**
035500030117     c     C_SKOK        BEGSR
035600030117      *
035700030117      * totalizzo il numero di record del membro per TEMINAL
035800030117      *  a cui trasmettere
035900030117do  1c     1             do        EL_skPO       XF
036000030117     c*  - per ognuno dei P.O. elaborati cerco il terminal
036100030117if  2c                   if        SKok(XF) = *on
036200030117     c                   eval      XT = 1
036300030117     c     SKpoT(XF)     lookup    SKpoT(XT)                              30
036400030117     c*  - e ne totalizzo il numero di record da trasmettere
036500030117     c*    (ripulendone il conteggio specifico del P.O. elaborato)
036600030117if  3c                   if        *in30
036700030117     c                             and XF <> XT
036800030117     c                   movel     *on           SKok(XT)
036900030117     c                   add       $Rec(XF)      $Rec(XT)
037000030117     c                   clear                   $Rec(XF)
037100030117     c                   movel     *off          SKok(XF)
037200030117e   3c                   endif
037300030117e   2c                   endif
037400030117e   1c                   enddo
037500030117      *
037600030117     c                   ENDSR
037700030117
037800030117      **-------------------------------------------------------------**
037900030117      ** Trasmissione del membro ai P.O.                             **
038000030117      **-------------------------------------------------------------**
038100030117     c     TrasmPO       BEGSR
038200030117      *
038300030117      * Cerco sistema infomativo di arrivo
038400030117     c                   clear                   TIBS55DS
038500030117     c                   z-add     SIMfel        I50ppo
038600030117     c                   z-add     SKpoT(XF)     I50apo
038700030117     c                   call      'TIBS55R'
038800030117     c                   parm                    TIBS55DS
038900030117      *
039000030117      * Eseguo la trasmissione vera e propria
039100030117     c                   eval      wRec = $Rec(XF)
039200030117     c                   exsr      CallTIBS53
039300030117      *
039400030117     c                   ENDSR
039500030117
039600030117      **-------------------------------------------------------------**
039700030117      ** Impostazione parametri e richiamo pgm. di trasmissione      **
039800030117      **-------------------------------------------------------------**
039900030117     c     CallTIBS53    BEGSR
040000030117      *
040100030117     c                   movel     O50lcl        CMZlcl                          *locale
040200030117      * parto sempre dalla libreria aziendale
040300030117     c                   movel     O50pla        CMZlbd                          *dalla libreria
040400030117      * e vado sempre alla libreria aziendale per i P.O.,
040500030117      *    ... sempre alla libreria di gruppo per la Sede
040600030117     c                   if        I50apo = 046
040700030117     c                   movel     O50alg        CMZlba                          *alla  libreria
040800030117     c                   else
040900030117     c                   movel     O50ala        CMZlba                          *alla  libreria
041000030117     c                   endif
041100030117     c                   movel     O50asi        CMZsyf                          *sist.informativo
041200030117      *
041300030117     c                   reset                   DSmembro
041400030117     c                   eval      MbrPOp = SIMfel
041500030117     c                   eval      MbrPOa = I50apo
041600030117      *
041700030117if  1c                   IF        Wrec > *zeros                                 *se scritti dei rec
041800030117      *
041900030117     c                   move      I50apo        CMZdst                          *destinatario
042000030117     c                   if        I50apo = 046
042100030117      * trasmetto dal file 46T alla sede
042200030117     c                   movel     'FIAR546T  '  CMZfld                          *dal file -trasm-
042300030117     c                   else
042400030117      * trasmetto dal file 00T ai vari P.O.
042500030117     c                   movel     'FIAR500T  '  CMZfld                          *dal file -trasm-
042600030117     c                   endif
042700030117     c                   movel     'FIAR500R  '  CMZfla                          *al file  -ricez-
042800030117     c                   movel(p)  DSmembro      CMZmbd                          *dal membro
042900030117     c                   movel(p)  DSmembro      CMZmba                          *al membro
043000030117     c                   move      '0000'        CMZori                          *ora invio
043100030117     c                   time                    wOra6
043200030117     c                   movel     wOra6         wOra2
043300030117     c                   add       1             wOra2
043400030117     c                   movel     wOra2         CMZori
043500030117     c                   move      '0000000'     CMZdti                          *data invio
043600030117     c                   move      Woggi         CMZdti
043700030117     c                   movel     'N'           CMZurg                          *urgenza notte
043800030117     c                   movel     'A'           CMZadd                          *record in aggiunta
043900030117     c                   movel     'S'           CMZclr                          *no pulizia file
044000030117     c                   movel     'S'           CMZftr                          *invio da transito
044100030117     c                   movel     *blanks       CMZerr                          *flag errore('1')
044200030117     c                   if        I50apo = 046
044300030117     c                   movel     'TRN2'        CMZazn                          *azione ricezione
044400030117     c                   else
044500030117     c                   movel     'LRN3'        CMZazn                          *azione ricezione
044600030117     c                   endif
044700030117     c                   move      Wrec          CMZnrr                          *record inviati
044800030117     c                   movel     *blanks       CMZprr                          *prerequisito
044900030117     c                   movel(p)  Param5        w236
045000030117     c                   call      'TIBS53C'
045100030117     c                   parm                    CMZlcl            1
045200030117     c                   parm                    CMZdst
045300030117     c                   parm                    CMZfld
045400030117     c                   parm                    CMZmbd
045500030117     c                   parm                    CMZlbd
045600030117     c                   parm                    CMZfla
045700030117     c                   parm                    CMZmba
045800030117     c                   parm                    CMZlba
045900030117     c                   parm                    CMZori
046000030117     c                   parm                    CMZdti
046100030117     c                   parm                    CMZurg
046200030117     c                   parm                    CMZadd
046300030117     c                   parm                    CMZclr
046400030117     c                   parm                    CMZftr
046500030117     c                   parm                    CMZerr
046600030117     c                   parm                    CMZazn
046700030117     c                   parm                    CMZsyf
046800030117     c                   parm                    CMZnrr
046900030117     c                   parm                    CMZprr
047000030117     c                   parm                    w236            236
047100030117e   1c                   endif
047200030117      *
047300030117      * Disallocazione e Cancellazione Override dal membro
047400030117     c                   eval      UL25tc = '9'
047500030117     c                   eval      UL25pf = CMZfld
047600030117     c                   exsr      CallTRUL25
047700030117      *
047800030117     c                   ENDSR
