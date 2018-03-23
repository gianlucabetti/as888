000100990326     H DECEDIT('0,') DATEDIT(*DMY.)
000200990407      *-----------------------------------------------------------------------------------------*
000300990407      * ELENCO SPEDIZIONE DA CHIUDERE RELATIVE A C.A. DI MANCANZE / AVARIE RESE-DISTRUTTE CHIUSE
000400990407      *-----------------------------------------------------------------------------------------*
000500990326     FFNDCT12L  IF   E           K DISK
000600990408     FFNDCT01L  IF   E           K DISK    rename(fndct000:fndct1)
000700040804     FFNDCK01L  IF   E           K DISK
000800990326     FFNDCD01L  IF   E           K DISK
000900990329     FFNDCD02L  IF   E           K DISK    rename(fndcd000:fndcd2)
001000990402     FFNARB01L  IF   E           K DISK
001100990402     FFNART01L  IF   E           K DISK
001200051114     FFiAR901L  IF   E           K DISK
001300991105     FFIAR601L  IF   E           K DISK
001400990326     FFNLBL02L  IF   E           K DISK
001500990331     FAZORG01L  IF   E           K DISK
001600990331     FTNTBE01L  UF A E           K DISK
001700030115     Ffncde02L  IF A E           K DISK
001800990402     FTABEL00F  IF   E           K DISK
001900990331     FPRTF198   O    F  198        PRINTER oflind(*INOF)
002000990326
002100990402      *--------------------------------------------------------------*
002200990402      *  RIEPILOGO INDICATORI
002300990402      *--------------------------------------------------------------*
002400990408      * 29 - ERRORE comodo
002500990402      * 30 - ERRORE comodo
002600990402      * 31 - ERRORE comodo
002700990402      * 32 - ERRORE comodo
002800990402      * 33 - ERRORE comodo
002900990406      * 50 - CHIUDERE BOLLA
003000990402      *--------------------------------------------------------------*
003100990326      *
003200990326     D KPJBA         E DS
003300990401     D CNCR80        E DS
003400990401     D UT§DSE0F      E DS
003500990326     D FIDN43DS      E DS
003600990326     D TIBS02DS      E DS
003700990326     D DTAD          E DS
003800990329     D DCCH          E DS
003900990331     D DDCB          E DS
004000990402     D DS3A          E DS
004100040804     D DDCT01        E DS
004200040804      *
004300040804     D FIDN12DS      E DS
004400040804     d  skKey                 26    305    dim(20)
004500040804
004600040804     d dsKey           ds
004700040804     d  dsaac                         4  0
004800040804     d  dsfil                         3  0
004900040804     d  dsnca                         7  0
005000040804      *
005100990326      *
005200990326      * DS PER trul06r - caricamento £X
005300990326     D TRUL06DS      E DS
005400990326     D  LIN                    1     90  0    DIM(30)                           P.O. COMODO
005500990401
005600990401     D WLBDAT          DS                  INZ
005700990401     D  G02DAT                 1      8  0
005800990401     D  G02INV                 9     16  0
005900990401     D  G02ERR                17     17
006000990401     D  G02TGI                18     22  0
006100990507
006200990507     D DS_NUMCA        DS
006300990507     D  DCTaac                 1      4  0
006400990507     D  DCTfil                 5      7  0
006500990507     D  DCTnca                 8     14  0
006600990401
006700990401     D DSANM           DS
006800990401     D  ANMNR1               397    400B 0
006900990401
007000990326      *
007100990326      *   S C H I E R E
007200990326     D L6              S              3  0 DIM(30)                              P.O.GESTITI
007300990330     D CCH             S              2    DIM(30)                              causali chiusura
007400990330     D IMA             S              1    DIM(30)                              ima 'S'
007500990408     D TES             S            100    DIM(6) CTDATA PERRCD(1)              TESTATA STAMPA
007600990408     D MSG             S             38    DIM(3) CTDATA PERRCD(1)              MSG da stampare
007700990326      *
007800990326     D
007900990326     D W0140           S             14  0
008000990326     D Wdtgio          S              8  0
008100990331     D Wora            S              4  0
008200990326     D dateu           S              8  0
008300990329     D wdacons         S              1                                         S= da consegnare
008400990330     D e               s              3  0                                      indice
008500990330     D x               s              3  0                                      indice
008600990331     D tipo            s              1                                         P= parziale
008700990331     D stpano          S             20
008800990401     D rsut            S             20
008900990331     D ggspe           S              2  0
009000990331     D mmspe           S              2  0
009100990507     D Wcasped         S              1
009200990507     D porto1          S              1
009300990407     D porto2          S              1
009400990408     D Wica            S              2
009500990408     D Wicc            S              2
009600990407     D impfat          s              9  0
009700990407     D impcas          s             13  3
009800990407     D divcas          s              3
009900990408     D stpmsg          s             38
010000990407     D stprsm          S             20
010100990407     D stprsd          S             20
010200990407     D TES1            S            100
010300990407     D TES2            S             98
010400990407     D TESTA1          S            198
010500990407     D TESTA2          S            198
010600990407     D TRAT            S            198    INZ(*all'-')
010700990408     D Kinizio         S              8  0 INZ(19990101)
010800990331      *
010900990408     D Wrkaac          s                   like(dctaac)
011000990408     D Wrkfil          s                   like(dctfil)
011100990408     D Wrknca          s                   like(dctnca)
011200990331     D Waas            s                   like(dctaas)
011300990331     D Wlnp            s                   like(dctlnp)
011400990331     D Wnrs            s                   like(dctnrs)
011500990331     D Wnsp            s                   like(dctnsp)
011600990331     D kcod            s                   like(tbecod) inz('DCB')
011700990331     D kkey1           s                   like(tbeke1)
011800990408     D cod             S                   LIKE(TBLcod)
011900990408     D key             S                   LIKE(TBLkey)
012000001018     D datalow         S              8  0 INZ(00010101)
012100001027     D record          S              3  0 INZ(0)
012200990326     D
012300990326      *
012400990326      *   C I C L O
012500990408     c     kinizio       setll     fndct12l
012600990326      *
012700990326      * lettura fndct12l per data chiusura
012800990326     c                   do        *hival
012900990408      * pulisco  i campi di comodo
013000990408     c                   clear                   wrkaac
013100990408     c                   clear                   wrkfil
013200990408     c                   clear                   wrknca
013300990326      *
013400990326     c                   read      fndct12l                               30
013500990408      *
013600990326      *
013700990326      * per fine file o per data chiusura maggiore della data finale esco dal ciclo
013800990326      *
013900990326     c                   if        *in30 or dctdch > i43amgdha
014000990326     c                   leave
014100990326     c                   endif
014200990329      *
014300990408      *  CARICO LA CAUSALE DI CHIUSURA
014400990331     c                   z-add     1             x
014500990331     c     dctcch        lookup    cch(x)                                 29
014600990330     c* se non trovata in cch controllo se esiste nell'altra
014700990331     c                   if        not *in29
014800990331     c                   exsr      CHCCH
014900990331     c                   endif
015000990330     c* se presente causale di chiusura da non considerare in IMA leggo il record successivo
015100990407     c                   if        ima(x) = 'S'
015200990407     c                   iter
015300990407     c                   endif
015400990326      *
015500990326      * Controllo il tipo anomalia
015600990326      *
015700990326     c                   exsr      chtad
015800990331      *
015900990326      * Se tipo anomalia mancanza
016000990326     c                   if        §tadragr = 'M'    or
016100990326      * oppure se avaria resa / distrutta
016200990326     c                             (§tadragr = 'V'  and (dctdsd = 'R' or
016300990326     c                             dctdsd = 'D'))
016400990326      * prendo in considerazione il record
016500990326      *
016600990326     c                   exsr      elabora
016700990326      *
016800990326     c                   endif
016900990326      *
017000990326     c                   enddo
017100990326      *
017200990326     c* operazioni finali
017300990331      * Aggiorna la data ultima di richiesta elaborazione CA
017400020605      * per tutti i p.o. di £6
017500020605     c                   do        30            x
017600020605     c                   if        l6(x) = *zeros
017700020605     c                   leave
017800020605     c                   endif
017900020605     c                   movel     l6(x)         kkey1
018000990402     c                   exsr      aggtbe
018100020605     c                   enddo
018200990326      *
018300990326      * Chiudo pgm aperti
018400990326     c                   clear                   tibs02ds
018500990326     c                   movel     'C'           t02tla
018600990326     c                   call      'TIBS02R'
018700990326     c                   parm                    kpjba
018800990326     c                   parm                    tibs02ds
018900001027      * se richiamato da distinta automatica restituisco valore record
019000001027      * trovati forzato nel campo della filiale
019100001027     c                   if        i43dst = '§'
019200001027     c                   z-add     record        i43fil
019300001027     c                   movel     fidn43ds      kpjbu
019400001027     c                   endif
019500990326
019600990326     c                   eval      *INLR = *ON
019700990326
019800990326      *****************************************************************
019900990329      *  ELABORA I RECORD BOLLA DA CHIUDERE SE NON LO SONO GIA'
020000990326      *****************************************************************
020100990329     C     ELABORA       BEGSR
020200990329      *
020300990329     c* se la filiale di apertura CA è presente  nella £6
020400990329     c     dctfil        lookup    l6                                     31
020500990406     c* oppure la lna della CA è presenta nella £6 aggancio la bolla
020600990329      *
020700990329     c  n31dctlna        lookup    l6                                     31
020800990329     c* solo per LNA trovata
020900990329     c                   if        *in31
021000990326      *
021100990329     C* aggancio il record bolla in ARRIVO
021200990329      *
021300040804      * se bolla con legami verifico se lna presente nella L6 e recupero in FNDCK il num. spedizione
021400040804     c                   movel     dctflo        ddct01
021500040804     c                   If        §dctlega = 'S'
021600040804     c     dctlna        lookup    L6                                     31
021700040804     c                   If        %found
021800040804     c     Kdcd          chain     fndck01l
021900040804     c                   If        %found(fndck01l)
022000040804     c                   eval      dctaas = dckaas
022100040804     c                   eval      dctlnp = dcklnp
022200040804     c                   eval      dctnrs = dcknrs
022300040804     c                   eval      dctnsp = dcknsp
022400040804     c                   endif
022500040804     c                   endif
022600040804     c                   endif
022700040804      *
022800990329     c     karb          chain     fnarb01l                           32
022900990406      *
023000990406      * gestione di ogni singola bolla
023100990406      *
023200990407     c* se record esistente
023300990407     c                   if        not *in32
023400990407     c* controllo  la singola bolla sia per segnalare:
023500990407     c*                                               A) Chiusura spedizione
023600990407     c*                                               B) Ritorno all'incasso
023700990408     c* verifico l'esistenza di bolle figlie con la LNP = LNA o alla sua L6
023800990408     c* e faccio gli stessi identici controlli
023900990406     c                   exsr      bolla
024000990407     c*
024100990406     c                   endif
024200990331      *
024300990331     c                   endif
024400990408     c*
024500990408     c* verifico se ci sono delle BOLLE FIGLIE LEGATE alla spedizione
024600990408     c*                           -------------------
024700990408     c*
024800990408     c     karb          chain     fnlbl02l                           32
024900990408     c*
025000990408     c                   clear                   waas
025100990408     c                   clear                   wlnp
025200990408     c                   clear                   wnrs
025300990408     c                   clear                   wnsp
025400990408     c*
025500990408     c                   dow       not *in32
025600990408     c* controllo se la lnp è presente nella mia £6
025700990408     c     lbllpn        lookup    L6                                     38
025800990408     c*
025900990408     c                   eval      waas = lblaan
026000990408     c                   eval      wlnp = lbllpn
026100990408     c                   eval      wnrs = lblnrn
026200990408     c                   eval      wnsp = lblnsn
026300990408     c**
026400990408     c* se trovato il £6 elaboro la bolla in ARRIVO e verifico
026500990408     c*
026600990408     c                   if        *in38
026700990408     c*
026800990408     C     KLBL          CHAIN     FNARB01L                           32
026900990408     C*
027000990408     c                   if        not *in32
027100990408     c                   exsr      bolla
027200990408     c                   endif
027300990408     c*
027400990408     c                   endif
027500990408     c*
027600990408     c     klbl          chain     fnlbl02l                           32
027700990408    4c                   enddo
027800990408     c*
027900990331      *
028000990331     c                   endsr
028100990329      *
028200990406      *****************************************************************
028300990406      *  ELABORO OGNI SINGOLA TESTATA
028400990406      *****************************************************************
028500990406     C     BOLLA         BEGSR
028600990406
028700990408     c                   clear                   wdacons
028800990407     c* controllo la data di consegna
028900990407     c                   if        arbdcm = 0
029000990406     c*
029100990407      * elaboro   il dettaglio bolla e controllo se i colli sono da chiudere
029200990406     c                   exsr      ctrart
029300990406     c* se è consegnabile emetto un messaggio
029400990406     c                   if        wdacons = 'S'
029500990406     c                   exsr      subMSG
029600990408     c                   except    consegna
029700990408     c                   endif
029800990408     c*
029900990408     c                   endif
030000990507      *
030100990507      * elaboro la bolla solo se non ci sono dei colli da consegnare
030200990408     c                   if        wdacons = ' '
030300990507      *
030400990507      * Controllo se per la stessa spedizione ho CA aperte, in questo caso non chiudo la Bolla
030500990507     c                   clear                   Wcasped
030600040804      *
030700050218      * RICERCA DELLE CA LEGATE ALLA SPEDIZIONE
030800040804      * SIA COME MAMMA CHE COME FIGLIA escludendo le annullate e le chiuse
030900040804      *
031000040804     c                   clear                   fidn12ds
031100040804     c                   eval      i12aas = dctaas
031200040804     c                   eval      i12lnp = dctlnp
031300040804     c                   eval      i12nrs = dctnrs
031400040804     c                   eval      i12nsp = dctnsp
031500040804     c                   eval      i12fel = 'E'
031600040804     c                   eval      i12fan = 'E'
031700040804     c                   eval      i12fch = 'E'
031800040804      *
031900040804     c                   call      'FIDN12R'
032000040804     c                   parm                    fidn12ds
032100040804      *
032200040804     c                   z-add     *zeros        ii                2 0
032300040804      * se trovata almeno una CA
032400040804     c                   if        o12nca > 0
032500040804     c                   dow       ii <  o12nca  and WCASped <> 'S'
032600040804     c                   add       1             ii
032700040804      * se trovata CA diversa da quella che leggo
032800040804     c                   if        skkey(ii) <> DS_NUMCA
032900040804     c                   eval      WCAsped = 'S'
033000040804     c                   endif
033100040804      *
033200040804     c                   enddo
033300040804      *
033400040804     c                   endif
033500040804      *
033600990507     c                   if        WCAsped <> 'S'
033700990406     c                   exsr      elaarb
033800990407     c                   endif
033900990406      *
034000990408     c                   endif
034100990408      *
034200990406     c                   endsr
034300990406      *****************************************************************
034400990406      *  CONTROLLO DETTAGLIO BOLLA ...
034500990408      *  Se il collo ha la data di consegna non valorizzata verifico :
034600990408      *  A) se appartiene ad una CA e non è annullata
034700990408      *  B) se appartiene ad una CA ne di mancanza o avara resa/distrutta
034800990408      *  C) se il collo in CA è chiuso ma con data chiusura diversa da quella della testata
034900990408      *  D) oppure se la CA è stata chiusa con causale diversa da 03/11
035000990408      * altrimenti il collo è da consegnare
035100990408      *
035200990406      *****************************************************************
035300990406     C     CTRART        BEGSR
035400990406      *
035500990406      * aggancio dettaglio colli bolle arrivo
035600990406     c     Karb          setll     fnart01l
035700990408     c     Karb          reade     fnart01l                               33
035800990406     c*
035900990406     c                   dow       not *in33
036000990406     c* controllo la data consegna del singolo collo
036100990406     c                   if        artdcm = 0
036200990406     c* aggancio il dettaglio colli
036300990406     c     kdcds         chain     fndcd02l                           34
036400990406     c* escludo gli annullati
036500990406     c                   if        not *in34 and  dcdatb <> ' '
036600990406     c                   eval      *in34 = *on
036700990406     c                   endif
036800990406     c* se esistente in altre CA controllo se tipo anomalia mancanza / avaria resa o distrutta
036900990406     c                   if        not *in34
037000990406     c     kdct          chain     fndct01l                           35
037100990406     c*
037200990406     c                   if        not *in35  and dctatb =' '
037300990406     c* recupero la causale di chiusura
037400990406     c                   if        dctcch <> '  '
037500990406     c                   z-add     1             x
037600990406     c     dctcch        lookup    cch(x)                                 29
037700990406     c* se non trovata in cch controllo se esiste nell'altra
037800990406     c                   if        not *in29
037900990406     c                   exsr      CHCCH
038000990406     c                   endif
038100990406     c                   endif
038200990406     c* recupero il tipo anomalia
038300990406     c                   exsr      chtad
038400990406     c* verifico la consegnabilità del singolo collo
038500990406     c                   exsr      ctrcollo
038600990406     c                   endif                                                  * chain fndct
038700990406     c*
038800990406     c                   else                                                   * *in34 on
038900990406     c* come se non fosse presente in CA
039000990406     c                   eval      wdacons = 'S'
039100990406     c                   endif                                                  * *in34 on
039200990406     c*
039300990406     c                   endif                                                  * data consegna = 0
039400990408     c* appena trovo un collo da consegnare esco dal giro
039500990408     c                   if        wdacons = 'S'
039600990408     c                   leave
039700990408     c                   endif
039800990406     c*
039900990408     c     Karb          reade     fnart01l                               33
040000990406      *
040100990406     c                   enddo
040200990406     c*
040300990406     c                   endsr
040400990329      *****************************************************************
040500990329      *  CONTROLLO SE IL COLLO SI DEVE CONSEGNARE O SI PUO' CHIUDERE
040600990329      *****************************************************************
040700990329     C     CTRCOLLO      BEGSR
040800990329      *
040900990330      *
041000990329     c* se data chiusura dettaglio e testata è diversa considero il collo ritrovato
041100990329     c* anche se il tipo anomalia è avaria
041200990329     c                   if        dcddch <> dctdch and dcddch > 0
041300990329     c                   eval      wdacons = 'S'
041400990329     c                   endif
041500990329     c*
041600990329     c                   if        wdacons = ' '
041700990329     c*
041800990329     c* controllo il raggruppamento anomalia (Mancanza/Avaria)
041900990329     c*
042000990329     c                   SELECT
042100990330     c* non considero i colli da consegnare  se  CA APERTA AVARIA resa/distrutta e MANCANZA
042200990330     c*                                          ------------------------------------------
042300990329     c* avaria
042400990330     c                   when      dcddch = 0 and (§tadragr = 'V'  and
042500990329     c* disposizione merce resa o distrutta
042600990330     c                             (dctdsd='R' or dctdsd='D')) or
042700990330     c* mancanza
042800990330     c                             (§tadragr = 'M')
042900990330     c* non considero i colli da consegnare se CA CHIUSA CON CAUSALE DIVERSA DA 03/11 CHE SONO
043000990330     C*                                        ______________________MANCANZE/AVARIE RESE/DISTR
043100990330     c*
043200990330     c                   when      dcddch > 0  and ima(x)=' ' and
043300990330     c                             (§tadragr = 'M' or (§tadragr = 'V' and
043400990408     c                              dctdsd = 'R' or  dctdsd = 'D'))
043500990330     c*
043600990329     c* negli altri casi considero il collo da consegnare
043700990329     c*
043800990329     c                   other
043900990329     c*
044000990329     c                   eval      wdacons = 'S'
044100990329     c*
044200990329     c                   endsl
044300990329     c*
044400990329     c                   endif                                                  *
044500990329     c*
044600990329     c                   endsr
044700990330      *
044800990330      *****************************************************************
044900990330      *  ELABORO   TUTTA LA BOLLA
045000990330      *****************************************************************
045100990330     C     ELAARB        BEGSR
045200990408     c*
045300990408     c                   clear                   stpmsg
045400990407     c* controllo se c'è da incassare ancora qualcosa
045500990407     c*
045600990407     c* Controllo il tipo bolla
045700990407     c                   exsr      tipobo
045800990407     c*
045900990407     c* A questo punto visto che non c'è merce da consegnare e non ci sono
046000990407     c* C.A. aperte sulla bolla stampo il messaggio di chiusura della bolla
046100990407     c* e i suoi eventuali incassi
046200990408     c*
046300990408     c* data consegna merce manca quindi deve chiudere la spedizione
046400990408     c                   if        arbdcm = 0
046500990408     c                   movel     msg(1)        stpmsg
046600001027      * verifico se chiamato da programma distinta automatica e se già
046700001027      * presente su altra distinta se si scrivo fncde
046800001027     c                   if        i43dst = '§' and arbndc = 0
046900000912     c                   exsr      scrivi
047000000912     c                   endif
047100990408     c                   else
047200990408     c* altrimenti controllo se ci sono degli incassi
047300990408     c                   if        wicc = 'SI' or Wica = 'SI'
047400990408     c                   movel     msg(3)        stpmsg
047500990408     c                   endif
047600990408     c                   endif
047700990408     c*
047800001027      * se distinta automatica non stampo  i43dst = §
047900990408     c*
048000000912     c                   if        stpmsg <> ' ' and i43dst <> '§'
048100990408     c* stampo
048200990408     c                   exsr      submsg
048300990408     c* controllo se è la prima volta che stampo questa CA
048400990408     c                   if        wrkaac = 0
048500990408     c                   eval      wrkaac = dctaac
048600990408     c                   eval      wrkfil = dctfil
048700990408     c                   eval      wrknca = dctnca
048800990407     c                   except    consebol
048900990408     c                   else
049000990408     c                   except    conseleg
049100990408     c                   endif
049200990408     c* verifico se devo aggiungere altri messaggi alla spedizione precdente
049300990408     c                   if        arbdcm = 0 and ( wicc = 'SI' or Wica = 'SI' )
049400990408     c                   eval      stpmsg = msg(3)
049500990408     c                   except    messaggio
049600990408     c                   endif
049700990408     c                   endif
049800990408     c*
049900990331     c                   endsr
050000990331      *****************************************************************
050100990331      *  GESTIONE DEI MESSAGGI IN STAMPA
050200990331      *****************************************************************
050300990331     C     SUBMSG        BEGSR
050400990331      *
050500990331     c                   if        not *in99 or *inof
050600990331     c                   except    testa
050700990331     c                   eval      *in99 = *on
050800990331     c                   eval      *inof = *off
050900990331     c                   endif
051000990331      *
051100990407     c                   clear                   stprsm
051200990407     c                   clear                   stprsd
051300990331     c                   clear                   mmspe
051400990331     c                   clear                   ggspe
051500990331      *
051600990407     c                   movel     arbrsm        stprsm
051700990407     c                   movel     arbrsd        stprsd
051800990331     c                   movel     arbmgs        mmspe
051900990331     c                   move      arbmgs        ggspe
052000990331      *
052100990331      *
052200990331     c                   endsr
052300990326      *****************************************************************
052400990326      *  REPERIMENTO DATI TIPO ANOMALIA
052500990326      *****************************************************************
052600990326     C     CHTAD         BEGSR
052700990326      *
052800990326     c                   clear                   DTAD
052900990326      *
053000990326     c                   clear                   TIBS02DS
053100990326     C                   MOVEL     'C'           t02mod
053200990326     C                   MOVEL     knsif         t02sif
053300990326     C                   MOVEL     'TAD'         t02cod
053400990326     C                   MOVEL(P)  dcttad        t02ke1
053500990326      *
053600990326     C                   CALL      'TIBS02R'
053700990326     C                   PARM                    KPJBA
053800990326     C                   PARM                    TIBS02DS
053900990326      *
054000990326     C                   if        t02err = *BLANKS
054100990326     C                   MOVEL     T02UNI        DTAD
054200990331     c                   movel     §taddesc      stpano
054300990326     C                   endif
054400990326      *
054500990326     C                   ENDSR
054600990329      *****************************************************************
054700990329      *  CONTROLLO CAUSALE CHIUSURA
054800990329      *****************************************************************
054900990329     C     CHCCH         BEGSR
055000990329      *
055100990329     c                   clear                   DCCH
055200990329      *
055300990329     c                   clear                   TIBS02DS
055400990329     C                   MOVEL     'C'           t02mod
055500990329     C                   MOVEL     knsif         t02sif
055600990329     C                   MOVEL     'CCH'         t02cod
055700990329     C                   MOVEL(P)  dctcch        t02ke1
055800990329      *
055900990329     C                   CALL      'TIBS02R'
056000990329     C                   PARM                    KPJBA
056100990329     C                   PARM                    TIBS02DS
056200990329      *
056300990329     C                   if        t02err = *BLANKS
056400990329     C                   MOVEL     T02UNI        DCCH
056500990329     C                   endif
056600990330      *
056700990330     c* carico la causale di chiusura in schiera e il suo flag IMA
056800990330     c                   add       1             e
056900990330     c                   movea     dctcch        cch(e)
057000990330     c                   movea     §cchima       ima(e)
057100990330     c                   eval      x=e
057200990329      *
057300990329     C                   ENDSR
057400990407      **************************************************************************
057500990407      * VERIFICO IL TIPO BOLLA DELLA SPEDIZIONE PER CONTROLLI ALL'INCASSO
057600990407      **************************************************************************
057700990407     C     TIPOBO        BEGSR
057800990407      *
057900990407      *  TIPO PORTO
058000990408     C                   movel     '3A'          COD
058100990408     C                   movel(P)  ARBcbo        KEY
058200990407     C     ktab          CHAIN     TABEL                              31
058300990407     C                   movel     TBLUNI        DS3A
058400990407     c* controllo se tipo bolla FRANCO
058500990407     C                   movel     §3ATB1        porto1
058600990407     C                   movel     §3ATB2        porto2
058700990407     C*
058800990407     c* verifico lA BOLLA
058900990407     c                   clear                   wica
059000990407     c                   clear                   impfat
059100990407     c                   clear                   wicc
059200990407     c                   clear                   impcas
059300990407     c                   clear                   divcas
059400990407     c*
059500990407     c* Se assegnato controllo il flag di incasso
059600990420     c                   if        porto1 = 'A'  and (arbica <> 'S'  and
059700000222     c                             arbica <> 'Y' and arbica <> 'I')
059800991105      * aggancio la tassazione 1° bolla con tipo record 1
059900991105     c                   eval      ar6trc = '1'
060000991105     c     kar6          chain     fiar601l                           31
060100990408     c  n31              z-add     ar6ift        impfat
060200990408     c                   if        impfat > 0 and not *in31
060300990408     c                   eval      wica = 'SI'
060400990408     c                   endif
060500990407     c                   endif
060600990407     c*
060700991105     c* Se assegnato controllo il flag di incasso sulla seconda bolla
060800990420     c                   if        porto2 = 'A'  and (arbica <> 'S'  and
060900000222     c                             arbica <> 'Y'and arbica <> 'I')
061000991105      * aggancio la tassazione 2° bolla con tipo record 2
061100991105     c                   eval      ar6trc = '2'
061200991105     c     kar6          chain     fiar601l                           31
061300991105     c  n31              z-add     ar6ift        impfat
061400990408     c                   if        impfat > 0 and not *in31
061500991105     c                   eval      wica = 'SI'
061600990408     c                   endif
061700990407     c                   endif
061800990407     c*
061900990407     c* Se flag del contrassegno valorizzato lo vado a recuperare
062000990420     c                   if        §3afca <> 0   and (arbicc <> 'S' and
062100000222     c                             arbicc <> 'Y'and arbicc <> 'I')
062200051114     c     karb          chain     fiar901l                           31
062300990408     c  n31              z-add     ar9cas        impcas
062400990408     c                   if        impcas > 0 and not *in31
062500990408     c  n31              eval      divcas = ar9vca
062600990408     c  n31              eval      wicc = 'SI'
062700990408     c                   endif
062800990407     c                   endif
062900990407     c                   endsr
063000990326      **************************************************************************
063100990326      * CARICO SCHIERA L6 DA TABELLA £6
063200990326      **************************************************************************
063300990326     C     car£6         BEGSR
063400990326      *
063500990326     C                   CLEAR                   TRUL06DS
063600020903     c                   if        i43dst = '§'
063700020903     c                   move      i43fil        lin(1)
063800020903     c                   else
063900990326     C                   MOVE      '£6'          D06COD
064000990326     C                   MOVEL(P)  i43fil        D06key
064100990326     C                   MOVEL(P)  TRUL06DS      KPJBU
064200990326     C                   CALL      'TRUL06R'
064300990326     C                   PARM                    KPJBA
064400990326     C                   MOVEL     KPJBU         TRUL06DS
064500020903     c                   end
064600990326      *
064700990326     c                   endsr
064800990331      **************************************************************************
064900990331      * AGGIORNO L'ULTIMA DATA ELABORATA NELLA TABELLA "DCB"
065000990331      **************************************************************************
065100990331     C     AGGTBE        BEGSR
065200990331      *
065300990331     c     ktbe          chain     tntbe01l                           30
065400990331     c* se non esiste lo creo
065500990331     c                   if        *in30
065600990331     c*
065700990331     c                   clear                   tntbe000
065800990331     c                   clear                   ddcb
065900990331     c                   movel     kcod          tbecod
066000990331     c                   movel     kkey1         tbeke1
066100990331     c*
066200990331     c                   endif
066300990423     c* se è una ristampa la riconosco perchè la data finale è minore o uguale a quella della
066400990423     c* tabella e quindi non aggiorno
066500990331     c*
066600990423     c                   if        not *in30
066700990423     c                   movel     tbeuni        ddcb
066800990423     c                   endif
066900990423     c*
067000990423     c                   if        i43amgdha  <= §dcbudta
067100990423     c* non faccio niente
067200990423     c                   else
067300990331     C                   z-add     i43amgdha     §dcbudta
067400990401     c                   movel     ddcb          tbeuni
067500000809     C                   movel     tbeke1        tbeflt
067600990331     c*
067700990402     c   30              write     tntbe000
067800990402     c  n30              update    tntbe000
067900990423     c*
068000990423     c                   endif
068100990331     c*
068200990331     c                   endsr
068300990331     c*
068400000912?    C*-----------------------------------------------------------
068500000912?    C* scrivi - scrive record fncde00f da fnarb
068600000912?    C*-----------------------------------------------------------
068700000912     C     scrivi        begsr
068800030115      *
068900030115      *  deve scrivere il record del FNCDE una sola volta anche se sulla
069000030115      *  stessa bolla ci sono 2 C.A. Chiuse
069100030115     c     kcde          setll     fncde02l                               88
069200030115      *
069300030115     C                   IF        *IN88 = *Off
069400000912     c                   clear                   fncde000
069500000912     c                   z-add     arbaas        cdeaas
069600000912     c                   z-add     arblnp        cdelnp
069700000912     c                   z-add     arbnrs        cdenrs
069800000912     c                   z-add     arbnsp        cdensp
069900000912     c                   z-add     arblna        cdelna
070000000912     c                   z-add     arbrmn        cdermn
070100001026     c                   move      i43are        cdecmc
070200001026     c                   move      i43ind        cdecca
070300001018      *se data forzata chiusura maggiore di data minima la forzo in fncde
070400001018     c                   if        i43amgdhd > datalow
070500001018     c                   move      i43amgdhd     cdedcm
070600001018     c                   else
070700000922     c                   move      dateu         cdedcm
070800001018     c                   end
070900001027     c                   add       1             record
071000000912     c                   write     fncde000
071100030115     C                   END
071200030115      *
071300000912     c                   endsr
071400990326      *------------------------------------------------------------------------*
071500990326      * ROUTINE INIZIALE
071600990326      *------------------------------------------------------------------------*
071700990326     c     *INZSR        BEGSR
071800990326      *
071900990326     c     *ENTRY        PLIST
072000990326     c                   PARM                    KPJBA
072100990326     c                   movel     kpjbu         fidn43ds
072200990326      *
072300990326     C                   Z-ADD     1             CODUT
072400990326     C                   CALL      'X§PARUT'
072500990401     C                   PARM                    UT§DSE0F
072600990326     C                   MOVEL     RAGUT         RSUT
072700990326     C                   MOVEL     REC80         CNCR80
072800990326     c
072900990326
073000990326      * reperisco data e ora
073100990326     C                   TIME                    W0140
073200990331      * ora
073300990331     c                   movel     w0140         wora
073400990326      * UDATE IN GGMMAAAA
073500990326     C                   MOVE      W0140         WDTGIO
073600990326      * UDATE IN AAAAMMGG
073700990326     C                   Z-ADD     WDTGIO        G02DAT
073800990326     C                   MOVEL     *BLANK        G02ERR
073900990326     C                   CALL      'XSRDA8'
074000990326     C                   PARM                    WLBDAT
074100990326     C                   MOVEL     G02INV        DATEU
074200990326      *
074300990326      * carico la £6 della filiale richiesta
074400990329     C
074500990326     c                   exsr      car£6
074600990326     C                   MOVEA     LIN           L6
074700990329      *
074800990326      **
074900990326      *   Accesso fndcd01l
075000990326     c     kdcd          klist
075100990326     c                   kfld                    dctaac
075200990326     c                   kfld                    dctfil
075300990326     c                   kfld                    dctnca
075400990329      **
075500990329      *   Accesso fndct01l
075600990329     c     kdct          klist
075700990329     c                   kfld                    dcdaac
075800990329     c                   kfld                    dcdfil
075900990329     c                   kfld                    dcdnca
076000990326      **
076100990326      *   Accesso fnarb01l / fnart01l
076200990326     c     karb          klist
076300990326     c                   kfld                    dctaas
076400990326     c                   kfld                    dctlnp
076500990326     c                   kfld                    dctnrs
076600990326     c                   kfld                    dctnsp
076700991105      *   Accesso fiar601l
076800991105     c     kar6          klist
076900991105     c                   kfld                    dctaas
077000991105     c                   kfld                    dctlnp
077100991105     c                   kfld                    dctnrs
077200991105     c                   kfld                    dctnsp
077300991105     c                   kfld                    ar6trc
077400990326      **
077500990326      *   Accesso legame bolle spedizione
077600990326     c     klbl          klist
077700990331     c                   kfld                    waas
077800990331     c                   kfld                    wlnp
077900990331     c                   kfld                    wnrs
078000990331     c                   kfld                    wnsp
078100990326      **
078200990329      *   Accesso fndcd
078300990329     c     kdcds         klist
078400990329     C                   KFLD                    artfls
078500990329     C                   KFLD                    artlna
078600990329     C                   KFLD                    artnrs
078700990329     C                   KFLD                    artnsc
078800990329      **
078900990329      *   Accesso fnart01
079000990329     c     kart          klist
079100990401     C                   KFLD                    dctaas
079200990401     C                   KFLD                    dctlnp
079300990401     C                   KFLD                    dctnrs
079400990401     C                   KFLD                    dctnsp
079500990329     C                   KFLD                    dcdnsc
079600990401      **
079700990401      *   Accesso flanm02l
079800990401     c     kanm          klist
079900990401     C                   KFLD                    dcdfls
080000990401     C                   KFLD                    dcdlna
080100990401     C                   KFLD                    dcdnrs
080200990401     C                   KFLD                    dcdnsc
080300990408      **
080400990408      *   Accesso tntbe01l
080500990408     c     ktbe          klist
080600990408     C                   KFLD                    kcod
080700990408     C                   KFLD                    kkey1
080800990331      **
080900990408      *   Accesso tabel00f
081000990408     c     ktab          klist
081100990408     c                   KFLD                    codut
081200990408     C                   KFLD                    cod
081300990408     C                   KFLD                    key
081400030115      **
081500030115      *   Accesso fnarb01l / fnart01l
081600030115     c     kCDE          klist
081700030115     c                   kfld                    arbndc
081800030115     c                   kfld                    arbaas
081900030115     c                   kfld                    arblnp
082000030115     c                   kfld                    arbnrs
082100030115     c                   kfld                    arbnsp
082200990326      **
082300990407     c* valorizzo le costanti di stampa TESTA1 e TESTA2
082400990407     c                   movel     tes(1)        tes1
082500990407     c                   movel     tes(3)        tes2
082600990407     c                   movel     tes1          testa1
082700990407     c                   move      tes2          testa1
082800990407     c*
082900990407     c                   movel     tes(2)        tes1
083000990407     c                   movel     tes(4)        tes2
083100990407     c                   movel     tes1          testa2
083200990407     c                   move      tes2          testa2
083300990407     c*
083400990407     c
083500990326      *
083600990326     c                   endsr
083700990331      *---------------------------------------------------------------------------------------------
083800990331     O*
083900990331     OPRTF198   E            TESTA             2
084000990331     O                       RSUT              +  0
084100990331     O                                         +  0 '/'
084200990331     O                       SIMFEL            +  0
084300990408     O                                           90 'ELENCO BOLLE RELATIVE A C.'
084400990408     O                                              'A. CHIUSE ANCORA DA CHIUDE'
084500990408     o                                              'RE'
084600990331     O                                          165 'FIDN46R'
084700990331     O                       WDTGIO        Y    180
084800990331     O                                          194 'PAG.'
084900990331     O                       PAGE          Z    198
085000990331     O          E            TESTA             3
085100990331     O                       KNSIF             +  0
085200990331     O                       KNMUS             +  1
085300990331     O                       WORA               180 '  :  :  '
085400990401     O          E            TESTA       1
085500990407     O                       testa1
085600990401     O          E            TESTA       1
085700990407     O                       testa2
085800990407     O          E            TESTA       1
085900990407     O                       TRAT               198
086000990408     O          E            consegna    1
086100990408     O                       dctaac
086200990408     O                       dctfil           +   1
086300990408     O                       dctnca        Z  +   1
086400990408     O                       stpano           +   2
086500990408     O                       arbaas           +   1
086600990408     O                       arblnp           +   1
086700990408     O                       arbnrs        Z  +   1
086800990408     O                       arbnsp        Z  +   1
086900990408     O                       ggspe            +   1
087000990408     O                                              '/'
087100990408     O                       mmspe
087200990408     O                       stprsm           +   1
087300990408     O                       stprsd           +   1
087400990408     o                       msg(2)             190
087500990408     O          E            consebol    1
087600990408     O                       wrkaac
087700990408     O                       wrkfil           +   1
087800990408     O                       wrknca        Z  +   1
087900990331     O                       stpano           +   2
088000990331     O                       arbaas           +   1
088100990331     O                       arblnp           +   1
088200990331     O                       arbnrs        Z  +   1
088300990331     O                       arbnsp        Z  +   1
088400990331     O                       ggspe            +   1
088500990331     O                                              '/'
088600990331     O                       mmspe
088700990407     O                       stprsm           +   1
088800990407     O                       stprsd           +   1
088900990408     o                       impfat        1  +   1
089000990408     o                       wica             +   2
089100990408     o                       impcas        1  +   1
089200990408     o                       divcas           +   1
089300990408     o                       wicc             +   2
089400990408     o                       stpmsg           +   4
089500990408     O          E            conseleg    1
089600990408     O                       arbaas           +  39
089700990408     O                       arblnp           +   1
089800990408     O                       arbnrs        Z  +   1
089900990408     O                       arbnsp        Z  +   1
090000990408     O                       ggspe            +   1
090100990408     O                                              '/'
090200990408     O                       mmspe
090300990408     O                       stprsm           +   1
090400990408     O                       stprsd           +   1
090500990408     o                       impfat        1  +   1
090600990408     o                       wica             +   2
090700990408     o                       impcas        1  +   1
090800990408     o                       divcas           +   1
090900990408     o                       wicc             +   2
091000990408     o                       stpmsg           +   4
091100990408     O          E            messaggio   1
091200990408     o                       stpmsg             190
091300990408     O          E            recca       1
091400990408     o                       dctfil           +   1
091500990408     o                       dctnca           +   1
091600990408     o                       dctdch           +   1
091700990407** TES  (Lungh. 100)   -Disegno stampa-
091800990407                                        S P E D I Z I O N E      MITTENTE             DESTINATARIO
091900990407Anno P.O.  N°C.A. Anomalia             Anno LNP Ser Numero  Data
092000990407         IMPORTO    DA     IMPORTO            DA       DISPOSIZIONI
092100990407         Fattura   INC.   C/Assegno      DIV INC.
092200990407xxxx xxx xxxxxxx  xxxxxxxxxxxxxxxxxxxx xxxx xxx xx xxxxxxx xx/xx xxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxx
092300990408xxxxxx xxx.xxx.xxx  xx x.xxx.xxx.xxx,xxx xxx  xx    xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
092400990408** MSG  (lungh. 38)
092500990408Spedizione da chiudere
092600990407Ci sono ancora dei colli da consegnare
092700990408Incassi Sospesi
