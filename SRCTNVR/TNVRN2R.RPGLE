000100030117     h decedit('0,') datedit(*dmy.)
000200030117
000300030117      *---------------------------------------------------------------*
000400030117
000500031203     fFnblp01L  If   e           k disk
000600031203     fFIAR501L  Uf   e           k disk
000700030117     fFIAR500T  O    e             disk    usropn
000800030117     f                                     rename(FIAR5000:FIAR50T)
000900030117     fFIAR546T  O    e             disk    usropn
001000030117     f                                     rename(FIAR5000:FIAR5ST)
001100030117
001200030117      *---------------------------------------------------------------*
001300030117
001400030117      * Costanti :
001500030117      *
001600030117     d EL_skPO         c                   const(250)
001700030117      *
001800030117      * Schiere :
001900030117      *
002000030117      * - Schiera numeri record da trasmettere per P.O.
002100030117      *   ("legata" alle schiere SKPO / SKPOT)
002200030117     d $Rec            s              7  0 dim(EL_skPO) inz                         Numero Record
002300030117     d SKok            s              1    dim(EL_skPO) inz(*all'0')
002400030117      *
002500030117      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
002600030117      * Strutture Dati :
002700030117      *
002800030117     d KPJBA         e ds
002900030117     d UT§DSE0F      e ds
003000030117     d CNCR80        e ds
003100030117      * - Ds x trasmissione dati
003200030117     d DSCMZ         e ds                  inz
003300030117      * - Ds x Reperimento librerie e Sif.da/a cui trasmettere
003400030117     d TIBS55ds      e ds                  inz
003500030117      * - Ds x reperimento lista AS/P.O. a cui NON trasmettere
003600030117     d TIBS56DS      e ds
003700030117     d  ski                    7    756  0 dim(EL_skPO)
003800030117      * - Ds x caricamento tutti terminal
003900030117     d TRUL26DS      e ds
004000030117     d  skpo                  11    760  0 dim(EL_skPO)
004100030117     d  skpot                761   1510  0 dim(EL_skPO)
004200030117      *
004300030117     d DSmembro        ds             7    inz
004400030117     d  MbrM                   1      1    inz('M')
004500030117     d  MbrPOP                 2      4  0 inz
004600030117     d  MbrPOA                 5      7  0 inz
004700030117      *
004800030117     d PARAM5          ds                  inz
004900030117     d  LEGFIL                 1     10
005000030117     d  MBRFIL                11     17
005100030117     d  BLKFIL                18     23
005200030117      *
005300030117     d WLBDAT          ds                  inz
005400030117     d  G02DAT                 1      8  0 inz
005500030117     d  G02INV                 9     16  0 inz
005600030117     d  G02ERR                17     17    inz
005700030117     d  G02TGI                18     22  0 inz
005800030117      *
005900030117      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
006000030117      * Variabili
006100030117      *
006200030117      * - Flag
006300030117     d $RecOK          s              1    inz(*off)
006400030117     d $Err            s              1    inz(*off)
006500030117      * - Indici di schiere
006600030117     d XX              s              3  0 inz
006700030117     d XF              s              3  0 inz
006800030117     d XT              s              3  0 inz
006900030117      * - Campi di comodo
007000030117     d Wn14            s             14  0                                      *numerico 14
007100030117     d Wn08            s              8  0                                      *numerico 8
007200030117     d Woggi           s              8  0                                      *d.corrente aa/m/g
007300030117     d Wora6           s              6  0 inz
007400030117     d Wora2           s              2  0 inz
007500031203     d SAVlna          s                   like(AR5lnp) inz(*loval)
007600030117     d SAVtrd          s                   like(AR5trd) inz(*loval)
007700030117     d $RecSEDE        s              7  0 inz                                  Numero Record
007800030117     d Wrec            s              7  0 inz                                  Numero Record
007900030117
008000030117      *---------------------------------------------------------------*
008100030117
008200030117      * Ciclo di lettura
008300031203do  1c                   do        *Hival
008400031203     c                   read      FIAR5000
008500031203      * fine file
008600031203     c                   If        %eof(FIAR501L)
008700031203     c                   Leave
008800031203     c                   EndIF
008900031203      * solo rcd 'GEN'
009000031203     c                   if        AR5trd <> 'GEN'
009100031203     c                   iter
009200031203     c                   endif
009300031203      * solo i trasmessi
009400031203     c                   if        ar5ft1 <> 'T'
009500031203     c                   iter
009600031203     c                   endif
009700031203      * fino a 02/12/03
009800031203     c                   if        ar5dt1 > 20031202
009900031203     c                   iter
010000031203     c                   endif
010100031203      * aggancio la bolla partenza
010200031203     c     kblp          chain     fnblp01l
010300031203if  2c                   If        %found(fnblp01l)
010400031203      * - - apertura membro LNA a rottura
010500031203if  3c                   if        blplna <> savlna
010600031203     c                   exsr      OpnMbrLNa
010700031203e   3c                   endif
010800030117      *
010900030117      * - - apertura membro sede
011000030117if  3c                   if        Not %open(FIAR546T)
011100030117     c                   exsr      OpnMbrSEDE
011200030117e   3c                   endif
011300030117      *
011400030117      * Gestisco scrittura se record OK:
011500030117      * - scrivo il record nei membri di trasmissione
011600030117      * - aggiorno il record come trasmesso
011700030117if  3c                   if        not *in97
011800030117     c                             and $Err = *off
011900030117     c                   exsr      GesREC
012000030117e   3c                   endif
012100031203      *
012200030117e   2c                   endif
012300030117      *
012400030117e   1c                   enddo
012500030117      *
012600030117      * Chiudo i file
012700030117     c                   exsr      ClosePF
012800030117      *
012900030117      * Raggruppo i dati da trasferire per terminal di partenza
013000030117      *   (ovvero per P.O. con elaboratore)
013100030117     c                   exsr      C_SKok
013200030117      * Sottometto trasmissioni ai P.O. con elaboratore
013300030117do  1c     1             do        EL_skPO       XF
013400030117if  2c                   if        SKok(XF) = *on
013500030117     c                   exsr      TrasmPO
013600030117e   2c                   endif
013700030117e   1c                   enddo                                                  *fine filiali
013800030117      *
013900030117      * Se ho scritto almeno 1 record gestisco l'invio dei dati in SEDE
014000030117if  1c                   if        $RecSEDE > 0
014100030117     c                   exsr      TrasmSEDE
014200030117e   1c                   endif
014300030117      *
014400030117      * Chiusura programma
014500030117     c                   clear                   TIBS55ds
014600030117     c                   movel     'C'           I50tla
014700030117     c                   call      'TIBS55R'
014800030117     c                   parm                    TIBS55ds
014900030117      *
015000030117     c                   eval      *inLR = *on
015100030117
015200030117      **-------------------------------------------------------------**
015300030117      ** Operazioni iniziali                                         **
015400030117      **-------------------------------------------------------------**
015500030117     c     *InzSR        BEGSR
015600030117      *
015700030117     c     *Entry        Plist
015800030117     c                   parm                    KPJBA
015900030117      *
016000030117     c                   call      'X§PARUT'
016100030117     c                   parm                    UT§DSE0F
016200030117     c                   movel     REC80         CNCR80
016300030117      *
016400030117      * calcola la data corrente
016500030117     c                   time                    Wn14                           *ora(6) + data(8)
016600030117     c                   move      Wn14          Wn08                           *data(8) in g/m/aa
016700030117     c                   z-add     Wn08          G02dat
016800030117     c                   clear                   G02inv
016900030117     c                   movel     *off          G02err
017000030117     c                   call      'XSRDA8'
017100030117     c                   parm                    WLBDAT
017200030117     c                   z-add     G02inv        Woggi                          *d.corrente aa/m/g
017300030117      *
017400030117      * Carico la schiera dei P.O. presenti nel S.I.
017500030117      *
017600030117     c                   clear                   TIBS56DS
017700030117     c                   eval      I56ppo = SIMfel
017800030117     c                   eval      I56mod = 'POS'
017900030117     c                   call      'TIBS56R'
018000030117     c                   parm                    TIBS56DS
018100030117     c                   if        O56err <> *blanks
018200030117     c                   clear                   SKI
018300030117     c                   endif
018400030117      *
018500030117      * Carico le schiere dei P.O. e dei relativi ORGFEL o §OGAS
018600030117      *
018700030117     c                   clear                   TRUL26DS
018800030117     c                   eval      I26tpt = 'G'
018900030117     c                   eval      I26tla = 'L'
019000030117     c                   call      'TRUL26R'
019100030117     c                   parm                    TRUL26DS
019200030117      *
019300030117      * Reperisco l'elenco di librerie per la trasmissione
019400030117      *
019500030117     c                   clear                   TIBS55DS
019600030117     c                   movel     KNSIF         I50psi
019700030117     c                   call      'TIBS55R'
019800030117     c                   parm                    TIBS55DS
019900031203
020000031203     c     kblp          Klist
020100031203     c                   Kfld                    ar5aas
020200031203     c                   Kfld                    ar5lnp
020300031203     c                   Kfld                    ar5nrs
020400031203     c                   Kfld                    ar5nsp
020500030117      *
020600030117     c                   ENDSR
020700030117
020800030117      **-------------------------------------------------------------**
020900030117      ** Apertura file / membro di trasmissione dati a SEDE          **
021000030117      **-------------------------------------------------------------**
021100030117     c     OpnMbrSEDE    BEGSR
021200030117      *
021300030117      * Imposto il nome membro
021400030117     c                   reset                   DSmembro
021500030117     c                   eval      MbrPOp = SIMfel
021600030117     c                   eval      MbrPOa = 046
021700030117      *
021800030117      * Richiamo cl TRUL25C - tipo '1':
021900030117      * - controlla se esiste il membro
022000030117      * - se inesistente: aggiunge tale membro al file
022100030117      * - alloca il membro del file
022200030117      * - esegue la sovrapposizione sul membro
022300030117     c                   eval      UL25tc = '1'
022400030117     c                   eval      UL25pf = 'FIAR546T  '
022500030117     c                   exsr      CallTRUL25
022600030117      *
022700030117      * Se 97 acceso vuol dire che non e' stato possibile allocare il
022800030117      *  membro interessato
022900030117     c  N97              open      FIAR546T
023000030117      *
023100030117     c                   ENDSR
023200030117
023300030117      **-------------------------------------------------------------**
023400031203      ** Apertura file / membro di trasmissione dati a P.O. LNa      **
023500030117      **-------------------------------------------------------------**
023600031203     c     OpnMbrLNa     BEGSR
023700030117      *
023800030117      * Chiudo il file se aperto sul membro del precedente P.O.  e
023900030117      * Richiamo cl TRUL25C - tipo '7':
024000030117      * - elimina la sovrapposizione sul membro
024100030117      *   (lasciandolo allocato)
024200030117if  1c                   if        %open(FIAR500T)
024300030117     c                   close     FIAR500T
024400030117     c                   eval      UL25tc = '7'
024500030117     c                   eval      UL25pf = 'FIAR500T  '
024600030117     c                   exsr      CallTRUL25
024700030117e   1c                   endif
024800030117      *
024900031203     c                   eval      SAVlna = blplna
025000030117     c                   setoff                                       9798
025100030117     c                   eval      $Err = *off
025200030117      * Controllo se locale
025300031203     c     blplna        lookup    SKI                                    98
025400030117      *
025500030117      * Scrivo solo se non è locale
025600030117if  1c                   if        not *in98
025700030117      *
025800030117      * Controllo il P.O. a cui trasmettere
025900030117     c                   exsr      CtrPOt
026000030117if  2c                   if        $Err = *off
026100030117      *
026200030117      * Imposto il nome membro
026300030117     c                   reset                   DSmembro
026400030117     c                   eval      MbrPOp = SIMfel
026500030117     c                   eval      MbrPOa = SKpoT(XF)
026600030117      *
026700030117      * Richiamo cl TRUL25C - tipo '1':
026800030117      * - controlla se esiste il membro
026900030117      * - se inesistente: aggiunge tale membro al file
027000030117      * - alloca il membro del file
027100030117      * - esegue la sovrapposizione sul membro
027200030117     c                   eval      UL25tc = '1'
027300030117     c                   eval      UL25pf = 'FIAR500T  '
027400030117     c                   exsr      CallTRUL25
027500030117      *
027600030117      * Se 97 acceso vuol dire che non e' stato possibile allocare il
027700030117      *  membro interessato
027800030117     c  N97              open      FIAR500T
027900030117      *
028000030117e   2c                   endif
028100030117      *
028200030117e   1c                   endif
028300030117      *
028400030117     c                   ENDSR
028500030117
028600030117      **-------------------------------------------------------------**
028700030117      ** Richiamo cl TRUL25C                                         **
028800030117      **-------------------------------------------------------------**
028900030117     c     CallTRUL25    BEGSR
029000030117      *
029100030117     c                   call      'TRUL25C'                            97
029200030117     c                   parm                    UL25tc            1
029300030117     c                   parm                    UL25pf           10
029400030117     c                   parm      O50pla        UL25li           10
029500030117     c                   parm      DSmembro      UL25mb           10
029600030117     c                   parm      *blanks       UL25tx           10
029700030117     c                   parm      *blanks       UL25fr            1
029800030117     c                   parm      *blanks       UL25os            1
029900030117     c                   parm      *zeros        UL25es            2
030000030117      *
030100030117     c  N97UL25es        comp      *zeros                             9797
030200030117      *
030300030117     c                   ENDSR
030400030117
030500030117      **-------------------------------------------------------------**
030600030117      ** Reperisco il P.O. a cui trasmettere i dati                  **
030700030117      **-------------------------------------------------------------**
030800030117     c     CtrPOt        BEGSR
030900030117      *
031000030117     c                   eval      $Err = *off
031100030117      * Reperisco il P.O. al quale devo trasmettere
031200030117     c                   movel     *off          *in30
031300030117     c                   z-add     1             XF
031400031203     c     blplna        LOOKUP    SKpo(XF)                               30
031500030117      *
031600030117      * - se non reperito lo memorizzo comunque (chi sarà mai ???)
031700030117if  1c                   if        *in30 = *off
031800030117     c                   z-add     1             XF
031900030117     c     *zeros        lookup    SKpo(XF)                               30
032000030117if  2c                   if        *in30 = *on
032100031203     c                   move      blplna        SKpo(XF)
032200031203     c                   move      blplna        SKpoT(XF)
032300030117x   2c                   else
032400030117      * - - se non reperito né memorizzabile: non trasmetto
032500030117     c                   eval      $Err = *on
032600030117e   2c                   endif
032700030117e   1c                   endif
032800030117      *
032900030117      * Imposto un flag che mi indichi i P.O. per cui ci sono dati
033000030117if  1c                   if        *in30 = *on
033100030117     c                   eval      SKok(XF) = *on
033200030117e   1c                   endif
033300030117      *
033400030117     c                   ENDSR
033500030117
033600030117      **-------------------------------------------------------------**
033700030117      ** Scrittura record x trasmissione                             **
033800030117      **-------------------------------------------------------------**
033900030117     c     GesREC        BEGSR
034000030117      *
034100031203      * Se P.O. NON è locale:
034200031203     c                   if        not *in98
034300030117      * Scrivo record x trasmissione in filiale
034400030117     c                   WRITE     FIAR50T
034500030117      * Conto i record da trasmettere per P.O.
034600030117     c                   add       1             $Rec(XF)                       *n°record x P.O.
034700030117     c                   endif
034800030117      *
034900030117      * Scrivo record x trasmissione in sede
035000030117     c                   WRITE     FIAR5ST
035100030117      * Conto i record da trasmettere in sede
035200030117     c                   add       1             $RecSEDE                       *n°record x SEDE
035300030130      *
035400030130      * Aggiorno file 00F come trasmesso
035500030130     c                   exsr      AggFlgTra
035600030117      *
035700030117     c                   ENDSR
035800030130      **-------------------------------------------------------------**
035900030130      ** AGGIORNO FILE FISICO COME TRASMESSO                         **
036000030130      **-------------------------------------------------------------**
036100030130     c     AggFlgTra     BEGSR
036200030130      *
036300030130     c                   movel     'T'           AR5ft1                         *flag trasm. P.O.
036400030130     c                   z-add     Woggi         AR5dt1                         *data trasm. P.O.
036500030130     c                   movel     'T'           AR5ft2                         *flag trasm. SEDE
036600030130     c                   z-add     Woggi         AR5dt2                         *data trasm. SEDE
036700030130     c                   UPDATE    FIAR5000
036800030130      *
036900030130     c                   ENDSR
037000030117      **-------------------------------------------------------------**
037100030117      ** Chiusura dei file                                           **
037200030117      **-------------------------------------------------------------**
037300030117     c     ClosePF       BEGSR
037400030117      *
037500030117      * Chiudo il file se aperto sul membro dell'ultimo P.O.  e
037600030117      * Richiamo cl TRUL25C - tipo '7':
037700030117      * - elimina la sovrapposizione sul membro
037800030117      *   (lasciandolo allocato)
037900030117if  1c                   if        %open(FIAR500T)
038000030117      *
038100030117     c                   close     FIAR500T
038200030117      *
038300030117     c                   reset                   DSmembro
038400030117     c                   eval      MbrPOp = SIMfel
038500030117     c                   eval      MbrPOa = SKpoT(XF)
038600030117     c                   eval      UL25tc = '7'
038700030117     c                   eval      UL25pf = 'FIAR500T  '
038800030117     c                   exsr      CallTRUL25
038900030117      *
039000030117e   1c                   endif
039100030117      *
039200030117      * Chiudo il file di Sede
039300030117if  1c                   if        %open(FIAR546T)
039400030117      *
039500030117     c                   close     FIAR546T
039600030117      *
039700030117     c                   reset                   DSmembro
039800030117     c                   eval      MbrPOp = SIMfel
039900030117     c                   eval      MbrPOa = 046
040000030117     c                   eval      UL25tc = '7'
040100030117     c                   eval      UL25pf = 'FIAR546T  '
040200030117     c                   exsr      CallTRUL25
040300030117      *
040400030117e   1c                   endif
040500030117      *
040600030117     c                   ENDSR
040700030117
040800030117      **-------------------------------------------------------------**
040900030117      ** Sistemazione schiere SKok e $Rec in base alla schiera SKpoT **
041000030117      **-------------------------------------------------------------**
041100030117     c     C_SKOK        BEGSR
041200030117      *
041300030117      * totalizzo il numero di record del membro per TEMINAL
041400030117      *  a cui trasmettere
041500030117do  1c     1             do        EL_skPO       XF
041600030117     c*  - per ognuno dei P.O. elaborati cerco il terminal
041700030117if  2c                   if        SKok(XF) = *on
041800030117     c                   eval      XT = 1
041900030117     c     SKpoT(XF)     lookup    SKpoT(XT)                              30
042000030117     c*  - e ne totalizzo il numero di record da trasmettere
042100030117     c*    (ripulendone il conteggio specifico del P.O. elaborato)
042200030117if  3c                   if        *in30
042300030117     c                             and XF <> XT
042400030117     c                   movel     *on           SKok(XT)
042500030117     c                   add       $Rec(XF)      $Rec(XT)
042600030117     c                   clear                   $Rec(XF)
042700030117     c                   movel     *off          SKok(XF)
042800030117e   3c                   endif
042900030117e   2c                   endif
043000030117e   1c                   enddo
043100030117      *
043200030117     c                   ENDSR
043300030117
043400030117      **-------------------------------------------------------------**
043500030117      ** Trasmissione del membro ai P.O.                             **
043600030117      **-------------------------------------------------------------**
043700030117     c     TrasmPO       BEGSR
043800030117      *
043900030117      * Cerco sistema infomativo di arrivo
044000030117     c                   clear                   TIBS55DS
044100030117     c                   z-add     SIMfel        I50ppo
044200030117     c                   z-add     SKpoT(XF)     I50apo
044300030117     c                   call      'TIBS55R'
044400030117     c                   parm                    TIBS55DS
044500030117      *
044600030117      * Eseguo la trasmissione vera e propria
044700030117     c                   eval      wRec = $Rec(XF)
044800030117     c                   exsr      CallTIBS53
044900030117      *
045000030117     c                   ENDSR
045100030117
045200030117      **-------------------------------------------------------------**
045300030117      ** Trasmissione del membro alla SEDE                           **
045400030117      **-------------------------------------------------------------**
045500030117     c     TrasmSEDE     BEGSR
045600030117      *
045700030117      * cerco sistema infomativi di arrivo
045800030117     c                   clear                   TIBS55DS
045900030117     c                   z-add     SIMfel        I50ppo
046000030117     c                   z-add     046           I50apo
046100030117     c                   call      'TIBS55R'
046200030117     c                   parm                    TIBS55DS
046300030117      *
046400030117      * Eseguo la trasmissione vera e propria
046500030117     c                   eval      wRec = $RecSEDE
046600030117     c                   exsr      CallTIBS53
046700030117      *
046800030117     c                   ENDSR
046900030117
047000030117      **-------------------------------------------------------------**
047100030117      ** Impostazione parametri e richiamo pgm. di trasmissione      **
047200030117      **-------------------------------------------------------------**
047300030117     c     CallTIBS53    BEGSR
047400030117      *
047500030117     c                   movel     O50lcl        CMZlcl                          *locale
047600030117      * parto sempre dalla libreria aziendale
047700030117     c                   movel     O50pla        CMZlbd                          *dalla libreria
047800030117      * e vado sempre alla libreria aziendale per i P.O.,
047900030117      *    ... sempre alla libreria di gruppo per la Sede
048000030117     c                   if        I50apo = 046
048100030117     c                   movel     O50alg        CMZlba                          *alla  libreria
048200030117     c                   else
048300030117     c                   movel     O50ala        CMZlba                          *alla  libreria
048400030117     c                   endif
048500030117     c                   movel     O50asi        CMZsyf                          *sist.informativo
048600030117      *
048700030117     c                   reset                   DSmembro
048800030117     c                   eval      MbrPOp = SIMfel
048900030117     c                   eval      MbrPOa = I50apo
049000030117      *
049100030117if  1c                   IF        Wrec > *zeros                                 *se scritti dei rec
049200030117      *
049300030117     c                   move      I50apo        CMZdst                          *destinatario
049400030117     c                   if        I50apo = 046
049500030117      * trasmetto dal file 46T alla sede
049600030117     c                   movel     'FIAR546T  '  CMZfld                          *dal file -trasm-
049700030117     c                   else
049800030117      * trasmetto dal file 00T ai vari P.O.
049900030117     c                   movel     'FIAR500T  '  CMZfld                          *dal file -trasm-
050000030117     c                   endif
050100030117     c                   movel     'FIAR500R  '  CMZfla                          *al file  -ricez-
050200030117     c                   movel(p)  DSmembro      CMZmbd                          *dal membro
050300030117     c                   movel(p)  DSmembro      CMZmba                          *al membro
050400030117     c                   move      '0000'        CMZori                          *ora invio
050500030117     c                   time                    wOra6
050600030117     c                   movel     wOra6         wOra2
050700030117     c                   add       1             wOra2
050800030117     c                   movel     wOra2         CMZori
050900030117     c                   move      '0000000'     CMZdti                          *data invio
051000030117     c                   move      Woggi         CMZdti
051100030117     c                   movel     'N'           CMZurg                          *urgenza notte
051200030117     c                   movel     'A'           CMZadd                          *record in aggiunta
051300030117     c                   movel     'S'           CMZclr                          *no pulizia file
051400030117     c                   movel     'S'           CMZftr                          *invio da transito
051500030117     c                   movel     *blanks       CMZerr                          *flag errore('1')
051600030117     c                   if        I50apo = 046
051700030117     c                   movel     'TRN2'        CMZazn                          *azione ricezione
051800030117     c                   else
051900030117     c                   movel     'LRN3'        CMZazn                          *azione ricezione
052000030117     c                   endif
052100030117     c                   move      Wrec          CMZnrr                          *record inviati
052200030117     c                   movel     *blanks       CMZprr                          *prerequisito
052300030117     c                   movel(p)  Param5        w236
052400030117     c                   call      'TIBS53C'
052500030117     c                   parm                    CMZlcl            1
052600030117     c                   parm                    CMZdst
052700030117     c                   parm                    CMZfld
052800030117     c                   parm                    CMZmbd
052900030117     c                   parm                    CMZlbd
053000030117     c                   parm                    CMZfla
053100030117     c                   parm                    CMZmba
053200030117     c                   parm                    CMZlba
053300030117     c                   parm                    CMZori
053400030117     c                   parm                    CMZdti
053500030117     c                   parm                    CMZurg
053600030117     c                   parm                    CMZadd
053700030117     c                   parm                    CMZclr
053800030117     c                   parm                    CMZftr
053900030117     c                   parm                    CMZerr
054000030117     c                   parm                    CMZazn
054100030117     c                   parm                    CMZsyf
054200030117     c                   parm                    CMZnrr
054300030117     c                   parm                    CMZprr
054400030117     c                   parm                    w236            236
054500030117e   1c                   endif
054600030117      *
054700030117      * Disallocazione e Cancellazione Override dal membro
054800030117     c                   eval      UL25tc = '9'
054900030117     c                   eval      UL25pf = CMZfld
055000030117     c                   exsr      CallTRUL25
055100030117      *
055200030117     c                   ENDSR
