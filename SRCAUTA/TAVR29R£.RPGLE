000100990614     h DECEDIT('0,') DATEDIT(*DMY.)
000200980323     H*------------------------------------------------------------------------*
000300071025     H* PREP. FILE WORK WFAUT00F PER AUMENTO TARIFFE 2008                      *
000400031103     H*                                                                        *
000500031103     H*                                                                        *
000600980323     H*------------------------------------------------------------------------*
000700980323     F*
000800031103     FWfdelpca  if   e             DISK
000900031107     FTntam00l  if   e           k DISK
001000031103     FWfaut01l  uf a e           k DISK
001100061124     FTabel00f  if   e           k DISK
001200990614      *
001300031103
001400031103      *------------------------------------------------------------------------*
001500031103
001600031103     D W0140           S             14  0
001700031103     D Wdtgio          S              8  0
001800031103     D datcor          S              8  0
001900041115     D dat_ultima      S              8  0
002000031103     D x               S              3  0
002100031103     D W_cliente       S              4  0
002200031103     D W_wfafbl        S                   like(WFAfbl)
002300031107     D WT_wfaftb       S                   like(WFAftb)
002400041115     D W_istat         S              1
002500051031     D W_peraum        S              5  2
002600061127     D W_fuel          S              1
002700031103     D Tipocl          S                   like(WFAtcl)
002800071026     D Kcod            S                   like(TBLCOD)
002900061127     D Kkey            S                   like(TBLKEY)
003000061124     D Codut           s              1  0 inz(1)
003100061124
003200061124      *  S C H I E R E
003300061124
003400031106     D SKC07           S              7  0 DIM(500)
003500031103
003600031103      *   D S   I N T E R N E / E S T E R N E
003700031103
003800031103     D WLBDAT          DS                  INZ
003900031103     D  G02DAT                 1      8  0
004000031103     D  G02INV                 9     16  0
004100031103     D  G02ERR                17     17
004200031103     D  G02TGI                18     22  0
004300031103
004400071026      * percentuali aumento
004500071026
004600071026     D perce           DS                  INZ
004700071026     D  perce1                 1      5  2
004800071026     D  perce2                 6     10  2
004900071026
005000031103      * Parametri per pgm. TIBS10R (reperimento codice padre cliente)
005100031103     D TIBS10ds      e ds                  inz
005200031106     D   skc11                21   5520  0 DIM(500)
005300031103
005400031103      * Parametri per pgm. TIBS69R (decodifica cliente)
005500031103     D TIBS69ds      e ds                  inz
005600031103     D CNACOds       e ds                  extname(CNACO00F) inz
005700031103     D CNINDds       e ds                  extname(CNIND00F) inz
005800031103     D CNCLPds       e ds                  extname(CNCLP00F) inz
005900031103     D FNCLSds       e ds                  extname(FNCLS00F) inz
006000061124
006100061124      * Tabella Nazioni
006200061124     D Ds15          e ds
006300031103
006400031103      *------------------------------------------------------------------------*
006500031103
006600031103     c                   do        *hival
006700031103      *
006800031103     c                   read      WFDELPCA
006900031103      *
007000031103     c                   if        %eof(WFDELPCA)
007100031103     c                   leave
007200031103     c                   endif
007300031107      *
007400031107     c                   move      wfpcli        W_cliente
007500031103      *
007600031103      * cerco i clienti figli del cliente unificante attraverso il tibs10
007700031103      *
007800031106     c                   clear                   tibs10ds
007900031106     c                   eval      d10tle = 'ST'
008000031106     c                   eval      d10paf = 'F'
008100031103     c                   z-add     wfpcli        d10cod
008200041115     c                   z-add     dat_ultima    d10drf
008300031103     c                   CALL      'TIBS10R'
008400031103     c                   parm                    tibs10ds
008500031103     c                   parm      'A'           partpexe          1
008600031103      * se non ci sono errori carico la schiera dei figli che contiene il codice del cliente padre
008700031103     c                   if        d10err = ' '
008800031103      * muovo in una schiera di 7
008900031106     c                   z-add     skc11         skc07
009000031106      *
009100031107      *
009200031106      * elaboro tutti i clienti figli
009300031106      *
009400031106     c                   exsr      SR_FIGLI
009500031106      *
009600031106      * se tutti i figli risultano bloccati vado a pulire il flag di elaborazione aumento
009700071029      * se tutti i figli hanno tariffe con ISTAT misto pulisco il flag di elaborazione aumento
009800071029      * se tutti i figli hanno tariffe con ISTAT SI    pulisco il flag di elaborazione aumento
009900071029     c                   IF        W_wfafbl = 'S'  or
010000071029     c                             W_istat =  'M'  or
010100071029     c                             W_istat =  'S'
010200071029     c                   Exsr      SR_blocco
010300071029     c                   endif
010400071026      * se non c'è istat faccio l'aumento in base al delta
010500071026     c                   IF        w_istat = 'N'
010600071026     c                   clear                   w_peraum
010700071029      * percentuale 1 per delta maggiore o uguale -2
010800071026     c                   if        wfpdac >= -2
010900071026     c                   eval      w_peraum = perce1
011000071102     c                   eval      WT_Wfaftb = '2'
011100071026     c                   endif
011200071029      * percentuale 2 per delta compreso tra -2 e -5
011300071029     c                   if        wfpdac < -2  and wfpdac >= -5
011400071029     c                   eval      w_peraum = perce2
011500071102     c                   eval      WT_Wfaftb = '4'
011600071026     c                   endif
011700071029      *
011800051031     c                   exsr      SR_aumento
011900051031     c                   endif
012000061124      * se c'è istat no faccio l'aumento
012100041115     c                   IF        w_istat = 'S'
012200061124     c                   eval      w_peraum = 0
012300051031     c                   exsr      SR_aumento
012400071102      * se con delta minore uguale -5 istat si    tabulato 7 per identificarli e basta
012500071029     c                   if        wfpdac >= -5
012600071102     c                   eval      WT_Wfaftb = '7'
012700041115     c                   endif
012800071029
012900071029     c                   endif
013000071029      * se istat misto stampo e non faccio aumento solo se delta maggiore e uguale a -5
013100071029     c                   If        w_istat = 'M' and wfpdac >= -5
013200071102     c                   eval      WT_Wfaftb = '5'
013300071029     c                   eval      w_peraum = 0
013400071029     c                   exsr      SR_aumento
013500041115     c                   endif
013600071029      * stampo e non faccio aumento solo se delta minore di -5
013700071029     c                   If        wfpdac <  -5
013800071102     c                   eval      WT_Wfaftb = '6'
013900071029     c                   endif
014000031106      * se ci sono errori elaboro l'unificante
014100031103     c                   else
014200031106      * scrivo il figlio dell'unificante
014300031106     c                   exsr      SR_FIGLIO
014400031103     c                   endif
014500031103      *
014600031103      * scrivo il record del cliente unificante
014700031103      *
014800031103     c                   Exsr      SR_UNIFIC
014900031103      *
015000031103     c                   enddo
015100031103      *
015200031103     c                   CALL      'TIBS10R'
015300031103     c                   parm                    tibs10ds
015400031103     c                   parm      'C'           partpexe
015500031103      *
015600031103     c                   seton                                        lr
015700031103      *---------------------------------------------------------------------------------------------
015800031106      * SR_FIGLIO Elaborazione del codice unificante come figlio
015900031103      *---------------------------------------------------------------------------------------------
016000031106     C     SR_FIGLIO     BEGSR
016100031106      *
016200031107      *
016300031107      * imposto il campo di lavoro per verificare se il cliente è bloccati
016400031107      *
016500031107     c                   eval      W_wfafbl = 'S'
016600031107      *
016700031107      * imposto il campo di lavoro per verificare se il cliente non ha tariffe
016800031107      *
016900031107     c                   eval      WT_wfaftb = ' '
017000041116      *
017100041116      * imposto il campo di lavoro per verificare se tutti i clienti figli hanno tariffe con ISTAT
017200041116      *
017300041116     c                   eval      W_istat = ' '
017400031106      * aggancio CNACO
017500031103      *
017600031103      * aggancio anagrafiche
017700031103     C                   clear                   TIBS69ds
017800031103     C                   clear                   CNACOds
017900031103     C                   clear                   CNINDds
018000031103     C                   clear                   CNCLPds
018100031103     C                   clear                   FNCLSds
018200031106     C                   movel     WFPcli        I69Kac
018300031106     C                   movel     WFPcli        I69Kin
018400031103     C                   call      'TIBS69R'
018500031103     C                   parm                    TIBS69ds
018600031103     C                   parm                    CNACOds
018700031103     C                   parm                    CNINDds
018800031103     C                   parm                    CNCLPds
018900031103     C                   parm                    FNCLSds
019000031103      * pulisco il record
019100031103     c                   clear                   WFAUT000
019200031107      * verifico se cliente non annullato
019300031106     c                   IF        acoflg =  ' '
019400031103      * verifico se cliente bloccato
019500031103     c                   IF        acoabl <> ' '
019600031103     c                   eval      Wfafbl = 'S'
019700031103     c                   else
019800031106      * pulisco  il campo di work
019900031103      * che pilota l'aggionamento del flag blocco del cliente unificante
020000031103     c                   clear                   W_wfafbl
020100031103     c                   endif
020200031103      * valorizzo i campi del file
020300031103     c                   eval      Wfatcl = 'C'
020400031103     c                   eval      Wfaclu = Wfpcli
020500031103     c                   eval      Wfaclf = Acoksc
020600031103     c                   eval      Wfarag = Acorag
020700031103     c                   eval      Wfadiv = Wfpdiv
020800031110     c                   eval      Wfadid = Wfpdid
020900031110     c                   eval      Wfaare = Wfpare
021000031110     c                   eval      Wfaard = Wfpard
021100031110     c                   eval      Wfafil = Wfpfil
021200031110     c                   eval      Wfafid = Wfpfid
021300031110     c                   eval      Wfaitc = Wfpitc
021400031103     c                   eval      Wfaage = Wfpage
021500031103     c                   eval      Wfaagd = Wfpagd
021600031103     c                   eval      Wfaclv = Wfpclv
021700031103     c                   eval      Wfaind = Indvia
021800031103     c                   eval      Wfacit = Indcit
021900051031     c                   eval      Wfacap = Indcae
022000051031     c                   eval      Wfasta = Indsta
022100031103     c                   eval      Wfaprv = Indprv
022200061124      * decodifca della nazione
022300061127     c                   If        wfasta <> *blanks
022400061127     c                   movel(p)  wfasta        kkey
022500061127     c     Keytab        chain     tabel00f
022600061127     c                   If        %found(tabel00f)
022700061127     c                   eval      ds15 = tbluni
022800061127     c                   eval      wfanaz = §15des
022900061127     c                   endif
023000061127     c                   endif
023100061127      * editizzo il delta del signor unificato
023200061127     c                   eval      wfadel = %editc(wfpdac:'M')
023300031103      *
023400031107      * posso eseguire l'aumento se esiste almeno una tariffa
023500031107     c     Wfaclf        chain     tntam00l
023600031107     c                   if        %found(tntam00l)
023700051031      * eseguo il controllo dell'istat
023800051031     c                   exsr      SR_ISTAT
023900051031      * verifco anche il distretto
024000071029     c                   if        wfadiv <> '6' and Wfadiv <> '7'
024100051031      * verifico anche se cliente 8888 o 9999
024200051031     c                             and W_cliente <> 8888 and W_cliente <> 9999
024300051031      * verifico se cliente bloccato
024400051031     c                             and Wfafbl <> 'S'
024500071029      * verifico il delta per valorizzare il flag del tabulato
024600071029     c                   if        Wfpdac >= -5
024700031103     c                   eval      Wfaftb = 'A'
024800051031      * se c'è istat MISTO non faccio l'aumento e stampo
024900051031     c                   IF        w_istat = 'M'
025000041115     c                   clear                   wfaftb
025100071102     c                   eval      WT_Wfaftb = '5'
025200041115     c                   endif
025300061127      * se istat SI creazione tariffa particolare fuel
025400051031     c                   If        w_istat = 'S'
025500071102     c                   eval      WT_Wfaftb = '7'
025600061127     c                   eval      wfaper = 0
025700071029     c                   clear                   wfaftb
025800041115     c                   endif
025900071029      * se istat NO aumento del in base al delta
026000051031     c                   If        w_istat = 'N' or w_istat = ' '
026100071029      * 1° lettera
026200071029     c                   if        Wfpdac >= -2
026300071102     c                   eval      WT_Wfaftb = '2'
026400071029     c                   eval      wfaper = perce1
026500051031     c                   endif
026600071029      * 2° lettera
026700071029     c                   if        Wfpdac <  -2 and wfpdac >= - 5
026800071102     c                   eval      WT_Wfaftb = '4'
026900071029     c                   eval      wfaper = perce2
027000071029     c                   endif
027100051031     c                   endif
027200071102      * se delta minore di -5 tabulato  6
027300071029     c                   else
027400071102     c                   eval      WT_Wfaftb = '6'
027500071029     c                   endif
027600071029
027700071029     c                   endif
027800041115      *
027900031107     c                   else
028000071029      * senza tariffe
028100031107     c                   eval      WT_Wfaftb = 'T'
028200031107     c                   endif
028300071029      * editizzo la percentuale di aumento
028400071029     c                   eval      wfaaum = %editc(wfaper:'M')
028500071029      *
028600031107      *
028700031103      * scrivo il record
028800031103     c                   write     wfaut000
028900031103      *
029000031106     c                   endif
029100030707      *
029200031103     c                   endsr
029300031106      *---------------------------------------------------------------------------------------------
029400031106      * SR_FIGLI Elaborazione di ogni codice figlio legato all'unificante
029500031106      *---------------------------------------------------------------------------------------------
029600031106     C     SR_FIGLI      BEGSR
029700031106      *
029800031107      * imposto il campo di lavoro per verificare se tutti i clienti figli sono bloccati
029900031106      *
030000031106     c                   eval      W_wfafbl = 'S'
030100031107      *
030200031107      * imposto il campo di lavoro per verificare se tutti i clienti figli non hanno tariffe
030300031107      *
030400031107     c                   eval      WT_wfaftb = 'T'
030500041115      *
030600041115      * imposto il campo di lavoro per verificare se tutti i clienti figli hanno tariffe con ISTAT
030700041115      *
030800041115     c                   eval      W_istat = ' '
030900031106      * per ogni figlio aggancio CNACO
031000031106      *
031100031106     c                   do        500           x
031200031106      * al primo cliente uguale a zero   esco dal ciclo
031300031106     c                   if        skc07(x) = *zeros
031400031106     c                   leave
031500031106     c                   endif
031600031106      * aggancio anagrafiche
031700031106     C                   clear                   TIBS69ds
031800031106     C                   clear                   CNACOds
031900031106     C                   clear                   CNINDds
032000031106     C                   clear                   CNCLPds
032100031106     C                   clear                   FNCLSds
032200031106     C                   movel     skc07(x)      I69Kac
032300031106     C                   movel     skc07(x)      I69Kin
032400031106     C                   call      'TIBS69R'
032500031106     C                   parm                    TIBS69ds
032600031106     C                   parm                    CNACOds
032700031106     C                   parm                    CNINDds
032800031106     C                   parm                    CNCLPds
032900031106     C                   parm                    FNCLSds
033000031106      * pulisco il record
033100031106     c                   clear                   WFAUT000
033200031106      * verifico se cliente annullato
033300031106     c                   IF        acoflg <> ' '
033400031106     c                   iter
033500031106     c                   endif
033600031106      * verifico se cliente bloccato
033700031106     c                   IF        acoabl <> ' '
033800031106     c                   eval      Wfafbl = 'S'
033900031106     c                   else
034000031106      * se esiste almeno un cliente che non è bloccato pulisco  il campo di work
034100031106      * che pilota l'aggionamento del flag blocco del cliente unificante
034200031106     c                   clear                   W_wfafbl
034300031106     c                   endif
034400031106      * valorizzo i campi del file
034500031106     c                   eval      Wfatcl = 'C'
034600031106     c                   eval      Wfaclu = Wfpcli
034700031106     c                   eval      Wfaclf = Acoksc
034800031106     c                   eval      Wfarag = Acorag
034900031106     c                   eval      Wfadiv = Wfpdiv
035000031110     c                   eval      Wfadid = Wfpdid
035100031110     c                   eval      Wfaare = Wfpare
035200031110     c                   eval      Wfaard = Wfpard
035300031110     c                   eval      Wfafil = Wfpfil
035400031110     c                   eval      Wfafid = Wfpfid
035500031110     c                   eval      Wfaitc = Wfpitc
035600031106     c                   eval      Wfaage = Wfpage
035700031106     c                   eval      Wfaagd = Wfpagd
035800031106     c                   eval      Wfaclv = Wfpclv
035900031106     c                   eval      Wfaind = Indvia
036000031106     c                   eval      Wfacit = Indcit
036100051031     c                   eval      Wfacap = Indcae
036200051031     c                   eval      Wfasta = Indsta
036300031106     c                   eval      Wfaprv = Indprv
036400061127      * decodifca della nazione
036500061127     c                   If        wfasta <> *blanks
036600061127     c                   movel(p)  wfasta        kkey
036700061127     c     Keytab        chain     tabel00f
036800061127     c                   If        %found(tabel00f)
036900061127     c                   eval      ds15 = tbluni
037000061127     c                   eval      wfanaz = §15des
037100061127     c                   endif
037200061127     c                   endif
037300061127      * editizzo il delta del signor unificato
037400061127     c                   eval      wfadel = %editc(wfpdac:'M')
037500061127      *
037600031106      *
037700031107      * posso eseguire l'aumento se cliente con tariffa
037800031107     c     Wfaclf        chain     tntam00l
037900031107     c                   if        %found(tntam00l)
038000051031      * verifico il delta per valorizzare il flag del tabulato
038100071030     c                   if        Wfpdac >= -5
038200051031      * verifco anche il distretto
038300051031     c                             and wfadiv <> '6' and Wfadiv <> '7'
038400051031      * verifico anche se cliente 8888 o 9999
038500051031     c                             and W_cliente <> 8888 and W_cliente <> 9999
038600041115      * imposto il flag per l'aumento
038700031106     c                   eval      Wfaftb = 'A'
038800031107      * pulisco il flag di lavoro cliente senza ariffe
038900031107     c                   clear                   WT_Wfaftb
039000031107     c                   endif
039100051031      * eseguo il controllo dell'istat
039200051031     c                   exsr      SR_ISTAT
039300051102     c                   endif
039400031106      * scrivo il record
039500031106     c                   write     wfaut000
039600031106      *
039700031106     c                   enddo
039800031106      *
039900031106     c                   endsr
040000041115      *---------------------------------------------------------------------------------------------
040100041115      * SR_ISTAT  Verifico le tariffe del cliente per vedere se cliente con ISTAT oppure no
040200041115      *---------------------------------------------------------------------------------------------
040300041115     C     SR_ISTAT      BEGSR
040400041115      *
040500051031     c                   clear                   wfafis
040600041115     c                   Do        *hival
040700041115      * tariffa con istat
040800041115     c                   if        tamfli  > 0
040900051031      * imposto il flag del cliente
041000051031     c                   if        wfafis  = ' ' or wfafis = 'S'
041100051031     c                   eval      wfafis  = 'S'                                si istat
041200051031     c                   else
041300051031     c                   eval      wfafis  = 'M'                                misto
041400051031     c                   endif
041500051031
041600041115      * imposto il flag dell'unificante
041700041115     c                   if        w_istat = ' ' or w_istat = 'S'
041800041115     c                   eval      w_istat = 'S'                                si istat
041900041115     c                   else
042000041115     c                   eval      W_istat = 'M'                                misto
042100041115     c                   endif
042200041115
042300041115      * tariffa senza istat
042400041115
042500041115     c                   else
042600051031      * imposto il flag del cliente
042700051031     c                   if        wfafis  = ' ' or wfafis  = 'N'
042800051031     c                   eval      wfafis  = 'N'                                no istat
042900051031     c                   else
043000051031     c                   eval      wfafis  = 'M'                                misto
043100051031     c                   endif
043200041115      * imposto il flag dell'unificante
043300041115     c                   if        w_istat = ' ' or w_istat = 'N'
043400041115     c                   eval      w_istat = 'N'                                no istat
043500041115     c                   else
043600041115     c                   eval      W_istat = 'M'                                misto
043700041115     c                   endif
043800041115      *
043900041115     c                   endif
044000041115      *
044100041115     c     Wfaclf        reade     tntam00l
044200041115     c                   if        %eof(tntam00l)
044300041115     c                   leave
044400041115     c                   endif
044500041115      *
044600041115     c                   enddo
044700041115      *
044800041115     c                   endsr
044900031103      *---------------------------------------------------------------------------------------------
045000031103      * SR_BLOCCO Elaborazione di ogni codice figlio legato all'unificante e flaggo in "B"
045100031103      *---------------------------------------------------------------------------------------------
045200031103     C     SR_BLOCCO     BEGSR
045300031103      *
045400031103     c                   feod      Wfaut01l
045500031103      *
045600031103      * imposto la chiave di lettura
045700031103      *
045800031103     c                   eval      tipocl = 'C'
045900031103      *
046000031103     c     keyaut        setll     Wfaut01l
046100031103     c                   do        *HIVAL
046200031103     c     keyaut        reade     Wfaut01l
046300031103      * fine file
046400031103     c                   if        %eof(Wfaut01l)
046500031103     c                   leave
046600031103     c                   endif
046700031103      * aggiorno
046800031107     c                   eval      Wfaftb = ' '
046900031103     c                   update    wfaut000
047000031103      *
047100031103     c                   enddo
047200031103      *
047300031103     c                   endsr
047400051031      *---------------------------------------------------------------------------------------------
047500051031      * SR_AUMENTO Elaborazione di ogni codice figlio legato all'unificante e imposto % di aumento
047600051031      *---------------------------------------------------------------------------------------------
047700051031     C     SR_AUMENTO    BEGSR
047800051031      *
047900051031     c                   feod      Wfaut01l
048000051031      *
048100051031      * imposto la chiave di lettura
048200051031      *
048300051031     c                   eval      tipocl = 'C'
048400051031      *
048500051031     c     keyaut        setll     Wfaut01l
048600051031     c                   do        *HIVAL
048700051031     c     keyaut        reade     Wfaut01l
048800051031      * fine file
048900051031     c                   if        %eof(Wfaut01l)
049000051031     c                   leave
049100051031     c                   endif
049200051031      * aggiorno solo se c'è la A di aumento
049300051031     c                   If        wfaftb = 'A'
049400051031     c                   eval      Wfaper = W_peraum
049500071029      * editizzo il delta del signor unificato
049600071029     c                   eval      wfaaum = %editc(wfaper:'M')
049700071029      *
049800051031     c                   update    wfaut000
049900051031     c                   else
050000051031     c                   unlock    wfaut01l
050100051031     c                   endif
050200051031      *
050300051031     c                   enddo
050400051031      *
050500051031     c                   endsr
050600031103      *---------------------------------------------------------------------------------------------
050700031103      * SR_UNIFIC    Elaborazione del cliente unificante
050800031103      *---------------------------------------------------------------------------------------------
050900031103     C     SR_UNIFIC     BEGSR
051000031103      *
051100031103      * aggancio anagrafiche
051200031103     C                   clear                   TIBS69ds
051300031103     C                   clear                   CNACOds
051400031103     C                   clear                   CNINDds
051500031103     C                   clear                   CNCLPds
051600031103     C                   clear                   FNCLSds
051700031103     C                   eval      I69kac =  WFPcli
051800031103     C                   eval      I69kin =  WFPcli
051900031103     C                   call      'TIBS69R'
052000031103     C                   parm                    TIBS69ds
052100031103     C                   parm                    CNACOds
052200031103     C                   parm                    CNINDds
052300031103     C                   parm                    CNCLPds
052400031103     C                   parm                    FNCLSds
052500031103      * pulisco il record
052600031103     c                   clear                   WFAUT000
052700031103      * verifico se cliente annullato
052800031103     c                   IF        acoflg <> ' '
052900031103     c                   endif
053000031103      *  cliente bloccato
053100031103     c                   eval      Wfafbl = W_wfafbl
053200031103      * valorizzo i campi del file
053300031103     c                   eval      Wfatcl = 'U'
053400031103     c                   eval      Wfaclu = Wfpcli
053500031103     c                   eval      Wfarag = Acorag
053600031103     c                   eval      Wfadiv = Wfpdiv
053700031110     c                   eval      Wfadid = Wfpdid
053800031110     c                   eval      Wfaare = Wfpare
053900031110     c                   eval      Wfaard = Wfpard
054000031110     c                   eval      Wfafil = Wfpfil
054100031110     c                   eval      Wfafid = Wfpfid
054200031110     c                   eval      Wfaitc = Wfpitc
054300031103     c                   eval      Wfaage = Wfpage
054400031103     c                   eval      Wfaagd = Wfpagd
054500031103     c                   eval      Wfaclv = Wfpclv
054600031103     c                   eval      Wfaind = Indvia
054700031103     c                   eval      Wfacit = Indcit
054800051031     c                   eval      Wfacap = Indcae
054900051031     c                   eval      Wfasta = Indsta
055000031103     c                   eval      Wfaprv = Indprv
055100031103     c                   eval      Wfaspc = Wfpsac
055200031103     c                   eval      Wfafpc = Wfpfac
055300031103     c                   eval      Wfadpc = Wfpdac
055400061127      * decodifca della nazione
055500061127     c                   If        wfasta <> *blanks
055600061127     c                   movel(p)  wfasta        kkey
055700061127     c     Keytab        chain     tabel00f
055800061127     c                   If        %found(tabel00f)
055900061127     c                   eval      ds15 = tbluni
056000061127     c                   eval      wfanaz = §15des
056100061127     c                   endif
056200061127     c                   endif
056300061127      * editizzo il delta del signor unificato
056400061127     c                   eval      wfadel = %editc(wfpdac:'M')
056500061127      *
056600031103      *
056700031103      * verifico il delta per valorizzare il flag del tabulato  se scartato
056800031103     c                   SELECT
056900031103      * distretto  6/7 scartato
057000031103     c                   When      wfadiv =  '6' or  Wfadiv =  '7'
057100031103     c                   eval      Wfaftb = 'S'
057200031103      * cliente 8888 o 9999    scartato
057300031103     c                   When      W_cliente =  8888 or  W_cliente =  9999
057400031103     c                   eval      Wfaftb = 'S'
057500071030      * delta maggiore di -5
057600071030     c                   When      Wfpdac >= -5
057700041115      * no bloccato  lettera di aumento   se almeno uno dei figli ha le tariffe e se non ha istat
057800031103     c                   If        Wfafbl = ' '
057900041115      *
058000041115      * figli senza tariffe  o con ISTAT
058100041115     c                   eval      Wfaftb = WT_wfaftb
058200031106      * bloccato  se non già scartato
058300031103     c                   else
058400031103     c                   eval      Wfaftb = 'B'
058500031106      *
058600031103     c                   endif
058700061206      * delta 0  / - 999,9  tabulato numero 1
058800071030     c                   When      Wfpdac <  -5
058900071102     c                   eval      Wfaftb = '6'
059000031103      *
059100031103     c                   endsl
059200031103      * scrivo il record
059300031103     c                   write     wfaut000
059400031103      *
059500031103     c                   endsr
059600031103      *
059700031103      *****************************************************************
059800031103      * ROUTINE INIZIALE
059900031103      *****************************************************************
060000031103     C     *INZSR        BEGSR
060100031103
060200031103      *
060300031103     c     keyaut        klist
060400031103     c                   kfld                    wfpage
060500031103     c                   kfld                    tipocl
060600031103     c                   kfld                    wfpcli
060700061124      *
060800061124     c     keytab        klist
060900061124     c                   kfld                    codut
061000061124     c                   kfld                    kcod
061100061124     c                   kfld                    kkey
061200031103      *
061300031103
061400031103      * reperisce data del giorno
061500031103     C                   TIME                    W0140
061600031103      * UDATE IN GGMMAAAA
061700031103     C                   MOVE      W0140         WDTGIO
061800031103      * UDATE IN AAAAMMGG
061900031103     C                   Z-ADD     WDTGIO        G02DAT
062000031103     C                   MOVEL     *BLANK        G02ERR
062100031103     C                   CALL      'XSRDA8'
062200031103     C                   PARM                    WLBDAT
062300031103     C                   Z-ADD     G02INV        datcor
062400051031      * imposto la data di ricerca figli con l'ultima data elaborazione
062500071026      * METTO COME GIORNO 28 COME IL PROGRAMMA TISE92 LO IMPOSTA NEL RICHIAMO DI TIBS10
062600071116     c                   eval      dat_ultima  = 20071028
062700071026      * cerco le percentuali di aumento
062800071026     c                   clear                   perce
062900071030     c                   move(p)   '2007'        kkey
063000071026     c                   movel(p)  '%A'          kcod
063100071026     c     Keytab        chain     tabel00f
063200071026     c                   If        %found(tabel00f)
063300071026     c                   movel     tbluni        perce
063400071026     c                   endif
063500071026      * imposto la chiave per agganciare la nazione
063600071026     c                   movel(p)  '15'          kcod
063700051108
063800031103     C                   endsr
