000100001005     F*--------------------------------------------------------------------------------------------*
000200001005     F* Data base
000300001005     F*--------------------------------------------------------------------------------------------*
000400040622     Fazdstp4l  if   e           k disk
000500020423     Ftisitpp   O    e             PRINTER oflind(*in05)
000600001005     D*--------------------------------------------------------------------------------------------*
000700001005     D* Data structure
000800001005     D*--------------------------------------------------------------------------------------------*
000900020423     D*------------
001000020423     D* ARCHITETTURA
001100020423     D*------------
001200020423     D KPJBA         E DS
001300060104      *
001400060104     d Status         sds           333
001500060104     d   SDSpgm          *proc
001600001003     D*------------
001700020423     D* DS DI RIDEFINIZIONE PARAMETRI DI INPUT
001800001003     D*------------
001900020423     D PARAM           DS
002000060414     d  PARnaz                        3                                         Nazione
002100060414     d  PARprv                        3                                         Provincia
002200091012     d  PARcapI                       9                                         C.a.p. inizio range
002300091012     d  PARcapF                       9                                         C.a.p. fine   range
002400060414     d  PARrag                       35                                         Ragione Sociale
002500060414     d  PARdos                        1                                         Tipo Destinatario
002600060414     d  PARsin                        1                                         Dettaglio Sinonimi
002700060414     d  PARddt                        1                                         Dest. con obbl.DDT
002800060414     d  PARips                        1                                         Dest. con inst.prest
002900091009     d  PARipsF                       3s 0                                      FilDestconInstrPrest
003000070326     d  PARfgi                        1                                         Dest. con inst.prest
003100091009     d  PARfgiF                       3s 0                                      FilDestconForzGiroCo
003200060414     d  PARric                        7s 0                                      Ricorrenza > di
003300060414     d  PARpou                        3s 0                                      P.O. utente ins/mod
003400060414     d  PARpouvid                     3s 0                                      P.O. utente ins/mod
003500060414     d  PARrot                        1                                         Salto pag. x Prov.
003600060414     d  PARrtpo                       1                                         Salto pag. x P.O.
003700091009     d  PARgcx                        1                                         Giorno di chiusura
003800091009     d  PARfic                        1                                         Dest. con forz.t.i.c
003900021206     D*------------------
004000021206     D* DS DI PROCEDURA X PASSAGGIO PARAMETRI
004100021206     D*------------------
004200021206     D TISIT0DS      E DS                  INZ
004300040622     D*------------
004400040622     D* DS REPERIMENTO P.O.
004500040622     D*------------
004600040622     D TRUL31DS      E DS
004700060104      *
004800060104      * - Parametri x interrogazione tabelle
004900060104     d TIBS02ds      e ds                  inz
005000060104     d   T02mod      e                     inz('C')
005100060104     d   T02cod      e                     inz('TDP')
005200060104      * - Tab. "TDP" = Tipi Destinatari Particolari
005300060104     d dTDP          e ds                  inz
005400040622     D*-------------------
005500040622     D* FILIALI DEI CLIENTI DA VISUALIZZARE
005600040622     D*-------------------
005700040622     D L1              S              3    DIM(250) INZ(*zeros)
005800020424     D*------------------
005900020424     D* VARIABILI DI WRK
006000020424     D*------------------
006100020424     D wrecok          S              1
006200020424     D flgsta          S              1  0
006300020424     D depprv          S                   LIKE(dstprv)
006400040622     D deppou          S                   LIKE(dstlna)
006500020424     D wrkRICERCA      S             35    VARYING
006600020516     D totdestprv      S              9  0 INZ(*zeros)
006700020516     D totdesttot      S              9  0 INZ(*zeros)
006800001003     C*--------------------------------------------------------------------------------------------*
006900001003     C* Main lines
007000001003     C*--------------------------------------------------------------------------------------------*
007100020424     C*
007200020424     C* Imposto i parametri di lancio in stampa
007300020424     C                   exsr      imppar
007400011029     C*
007500020424     C* Stampo la testata
007600020424     C                   exsr      states
007700020423     C*
007800040622     C* Stampo l'anagrafico dei destinatari disagiati/supermercati/appuntamenti
007900020423     C                   exsr      stadet
008000020423     C*
008100020423     C* Stampo la pagina di chiusura del prospetto
008200020423     C                   exsr      stafin
008300040319     C*
008400040319     C* Lancio il driver TISIT5R inmodalità "SOLO CHIUSURA FILES"
008500040319     C                   clear                   TISIT0DS
008600040319     C                   movel     'C'           IT0TLA
008700040319     C                   call      'TISIT5R'
008800040319     C                   parm                    TISIT0DS
008900001003     C*
009000060104     c                   clear                   TIBS02ds
009100060104     c                   eval      T02tla = 'C'
009200060104     c                   call      'TIBS02R'
009300060104     c                   parm                    KPJBA
009400060104     c                   parm                    TIBS02ds
009500001003     C                   seton                                        LR
009600020423     C*--------------------------------------------------------------------------------------------*
009700020424     C* IMPPAR - Imposta la testata con i parametri di lancio
009800020423     C*--------------------------------------------------------------------------------------------*
009900020424     C     imppar        BEGSR
010000020423     C*
010100020423     C                   clear                   sitpt1
010200060104      *
010300060104      * Stampa il nome del pgm
010400060104     c                   movel     SDSpgm        STPpgm
010500040622     C*
010600040622     C* P.O. utente INS/VAR
010700040622     C                   if        parpouvid = 999
010800040622     C                   eval      stpppou = 'TUTTI I P.O. DELL''AREA'
010900040622     C                   else
011000040622     C                   if        parpouvid = *zeros
011100040622     C                   eval      stpppou = 'TUTTI'
011200040622     C                   else
011300040622     C                   movel     parpouvid     stpppou
011400040622     C                   endif
011500040622     C                   endif
011600020423     C*
011700040622     C* Nazione
011800020423     C                   if        parnaz = '999'
011900020423     C                   eval      stppnaz = 'TUTTE'
012000020423     C                   else
012100020423     C                   if        parnaz = *blanks
012200020423     C                   eval      stppnaz = 'ITA'
012300020423     C                   else
012400020423     C                   eval      stppnaz = parnaz
012500020423     C                   endif
012600020423     C                   endif
012700020423     C*
012800020423     C* Provincia
012900020423     C                   if        parprv = *blanks
013000020423     C                   eval      stppprv = 'TUTTE'
013100020423     C                   else
013200020423     C                   eval      stppprv = parprv
013300020423     C                   endif
013400091012      /free
013500091012         // - C.a.p. dal - al
013600091012         if  (PARcapI + PARcapF) = *blank;
013700091012           STPpCap = 'TUTTI';
013800091012         else;
013900091012           STPpCap = 'dal ' + %trimr(PARcapI) + ' al ' + %trimr(PARcapF);
014000091012         endif;
014100091012      /end-free
014200020423     C*
014300020423     C* Ragione Sociale
014400020423     C                   if        parrag = *blanks
014500020423     C                   eval      stpprag = 'TUTTE'
014600020423     C                   else
014700020424     C                   eval      stpprag = 'TUTTO CIO'' CHE CONTIENE "' +
014800020424     C                                       %trim(parrag) + '"'
014900020423     C                   endif
015000020423     C*
015100020423     C* Tipo particolarità
015200060104     C                   if        pardos = *blanks
015300020423     C                   eval      stppdos = 'TUTTE'
015400060104     C                   else
015500060104     c                   clear                   TIBS02ds
015600060104     c                   movel(p)  PARdos        T02ke1
015700060104     c                   exsr      Decod_DOS
015800060104     c                   movel     §TDPdes       STPpdos
015900060104     C                   endif
016000020423     C*
016100020426     C* Ricorrenze
016200020426     C                   eval      stppric = parric
016300020424     C*
016400020423     C                   ENDSR
016500020424     C*--------------------------------------------------------------------------------------------*
016600020424     C* STATES - Routine di stampa testata
016700020424     C*--------------------------------------------------------------------------------------------*
016800020424     C     states        BEGSR
016900020424     C*
017000020424     C                   if        *in05 = *on
017100020424     C                   if        flgsta <> *zeros
017200020424     C                   write     sitpri
017300020424     C                   endif
017400020424     C                   write     sitpt1
017500020424     C                   write     sitpt3
017600020424     C                   setoff                                       05
017700020424     C                   endif
017800020424     C*
017900020424     C                   ENDSR
018000020423     C*--------------------------------------------------------------------------------------------*
018100020423     C* STAFIN - Stampa la pagina di chiusura prospetto
018200020423     C*--------------------------------------------------------------------------------------------*
018300020423     C     stafin        BEGSR
018400020423     C*
018500020423     C                   write     sitpri
018600020423     C                   write     sitpfi
018700020423     C*
018800020423     C                   ENDSR
018900011026     C*--------------------------------------------------------------------------------------------*
019000040622     C* STADET - Stampa il dettaglio delle proposte destinatari disagiati/supermercati/appuntamenti
019100011026     C*--------------------------------------------------------------------------------------------*
019200020423     C     stadet        BEGSR
019300020424     C*
019400040622     C     *loval        setll     azdstp4l
019500040622     C                   read      azdstp4l
019600040622     C                   dow       not %eof(azdstp4l)
019700020423     C* Eseguo il controllo di attinenza del record in funzione degli eventuali parametri richiesti
019800020423     C                   exsr      chkrec
019900020423     C* Se il trecord è attinente
020000020423     C                   if        wrecok = 'S'
020100020423     C*
020200020424     C* Testo quindi l'eventuale rottura di codice x provincia
020300040622     C                   if        (dstprv <> depprv AND
020400040622     C                              flgsta <> *zeros)
020500020424     C*
020600020424     C* Ad ogni rottura di provincia, se richiesto nel lancio salto pagina
020700020424     C                   if        parrot = 'S'
020800020516     C                   eval      stpdestot = 'TOTALE PER PROVINCIA:'
020900020516     C                   eval      stpnumtot = totdestprv
021000020516     C                   eval      totdestprv = *zeros
021100020516     C                   write     sitptt
021200020424     C                   seton                                        05
021300020424     C                   exsr      states
021400020424     C                   endif
021500020424     C                   else
021600020424     C                   eval      flgsta = 1
021700020424     C                   endif
021800040622     C*
021900040622     C* Testo quindi l'eventuale rottura di codice x P.O.
022000040622     C                   if        (dstlna <> deppou AND
022100040622     C                              flgsta <> *zeros)
022200040622     C*
022300040622     C* Ad ogni rottura di P.O., se richiesto nel lancio salto pagina
022400040622     C                   if        parrtpo = 'S'
022500040622     C                   eval      stpdestot = 'TOTALE PER P.O.:'
022600040622     C                   eval      stpnumtot = totdestprv
022700040622     C                   eval      totdestprv = *zeros
022800040622     C                   write     sitptt
022900040622     C                   seton                                        05
023000040622     C                   exsr      states
023100040622     C                   endif
023200040622     C                   else
023300040622     C                   eval      flgsta = 1
023400040622     C                   endif
023500020424     C*
023600040622     C* Deposito la provincia corrente e il P.O. corrente
023700020429     C                   eval      depprv = dstprv
023800040622     C                   eval      deppou = dstlna
023900020429     C*
024000020423     C                   eval      stprag = dstrag
024100020426     C                   eval      stpind = %trim(dstind)
024200020423     C                   eval      stpnaz = dstnaz
024300020423     C                   eval      stpprv = dstprv
024400020423     C                   eval      stploc = dstloc
024500020423     C                   eval      stpcap = dstcap
024600060104      * Decodifico il tipo destinatario
024700060104     c                   clear                   TIBS02ds
024800060104     c                   movel(p)  DSTdos        T02ke1
024900060104     c                   exsr      Decod_DOS
025000060104     c                   movel     §TDPdes       STPdos
025100020426     C*
025200020426     C                   eval      stpric = dstnum
025300040622     C                   eval      stpute = dstute
025400040622     C                   eval      stppou = dstpou
025500040622     C                   eval      stplna = dstlna
025600020424     C*
025700020424     C* Testo il "fine-pagina"
025800020424     C                   exsr      states
025900020423     C*
026000020423     C* Quindi stampo la riga di dettaglio principale
026100020423     C                   write     sitpd1
026200020516     C                   eval      totdestprv = totdestprv + 1
026300020516     C                   eval      totdesttot = totdesttot + 1
026400021206     C*
026500021206     C* Imposto i campi della DS di passaggio procedura
026600021206     C                   exsr      impcam
026700021206     C*
026800021206     C* Chiamo il programma di reperimento
026900040319     C                   eval      IT0TLA = 'P'
027000021206     C                   call      'TISIT5R'
027100021206     C                   parm                    TISIT0DS
027200021206     C*
027300021206     C* Imposto in stampa i campi restituti dal programma di reperimento
027400021206     C                   eval      st2rag = OT0RAG
027500021206     C                   evAl      st2ind = OT0IND
027600021206     C                   eval      st2naz = OT0NAZ
027700021206     C                   eval      st2prv = OT0PRV
027800021206     C                   eval      st2cap = OT0CAP
027900021206     C                   eval      st2loc = OT0LOC
028000060104      * Decodifico il tipo destinatario
028100060104     c                   clear                   TIBS02ds
028200060104     c                   movel(p)  OT0dos        T02ke1
028300060104     c                   exsr      Decod_DOS
028400060104     c                   movel     §TDPdes       ST2dos
028500021206     C*
028600021206     C* Testo il "fine-pagina"
028700021206     C                   exsr      states
028800021206     C*
028900021206     C* Quindi stampo la riga di dettaglio secondaria
029000021206     C                   write     sitpd2
029100001003     C*
029200020423     C                   endif
029300040622     C                   read      azdstp4l
029400020423     C                   enddo
029500020516     C*
029600020516     C* A fine file stampo una riga di totali per l'ultima provincia stampata...
029700020516     C                   if        parrot = 'S'
029800020516     C                   eval      stpdestot = 'TOTALE PER PROVINCIA:'
029900020516     C                   eval      stpnumtot = totdestprv
030000020516     C                   eval      totdestprv = *zeros
030100020516     C                   write     sitptt
030200020516     C                   endif
030300040622     C*
030400040622     C* A fine file stampo una riga di totali per l'ultimo P.O. stampata...
030500040622     C                   if        parrtpo = 'S'
030600040622     C                   eval      stpdestot = 'TOTALE PER P.O.:'
030700040622     C                   eval      stpnumtot = totdestprv
030800040622     C                   eval      totdestprv = *zeros
030900040622     C                   write     sitptt
031000040622     C                   endif
031100020516     C* ...e una riga di totali generali
031200020516     C                   eval      stpdestot = 'TOTALE GENERALE:'
031300020516     C                   eval      stpnumtot = totdesttot
031400020516     C                   eval      totdesttot = *zeros
031500020516     C                   write     sitptt
031600020423     C*
031700001003     C                   ENDSR
031800060104      *
031900060104      *---------------------------------------------------------------*
032000060104      * Decodifica Tipo Destinatario                                  *
032100060104      *---------------------------------------------------------------*
032200060104     c     Decod_DOS     BEGSR
032300060104      *
032400060104     c                   clear                   dTDP
032500060104     c                   eval      T02mod  = 'C'
032600060104     c                   eval      T02cod  = 'TDP'
032700060104     c*** già impostato: movel(p)  ???           T02ke1
032800060104     c                   call      'TIBS02R'
032900060104     c                   parm                    KPJBA
033000060104     c                   parm                    TIBS02ds
033100060104     c                   if        T02err  = *blanks
033200060104     c                   movel     T02uni        dTDP
033300060104     c                   endif
033400060104      *
033500060104     c                   ENDSR
033600060104      *
033700001005     C*--------------------------------------------------------------------------------------------*
033800020423     C* CHKREC - Verifico attinenza del record con i parametri richiesti nel lancio
033900001005     C*--------------------------------------------------------------------------------------------*
034000020423     C     chkrec        BEGSR
034100020423     C*
034200020424     C                   eval      wrecok = 'S'
034300020424     C*
034400020424     C* No annullati
034500020424     C                   if        wrecok = 'S'
034600020424     C                   if        dstatb <> *blanks
034700020424     C                   eval      wrecok = 'N'
034800020424     C                   endif
034900020424     C                   endif
035000001005     C*
035100020423     C* Nazione
035200020424     C                   if        wrecok = 'S'
035300020423     C                   if        parnaz <> '999'
035400020423     C                   if        parnaz = 'ITA'
035500020423     C                   if        dstnaz <> 'ITA' AND
035600020423     C                             dstnaz <> *blanks
035700020423     C                   eval      wrecok = 'N'
035800020423     C                   endif
035900020423     C                   else
036000020423     C                   if        dstnaz <> parnaz
036100020423     C                   eval      wrecok = 'N'
036200020423     C                   endif
036300020423     C                   endif
036400020423     C                   endif
036500020424     C                   endif
036600020423     C*
036700020423     C* Provincia
036800020424     C                   if        wrecok = 'S'
036900020423     C                   if        parprv <> *blanks
037000020423     C                   if        dstprv <> parprv
037100020423     C                   eval      wrecok = 'N'
037200020423     C                   endif
037300020423     C                   endif
037400020424     C                   endif
037500091009      /free
037600091009         // - C.a.p.
037700091009         if  wRecOk = 'S'                   and
037800091009            (PARcapI + PARcapF) <> *blank   and
037900091009            (DSTcap < PARcapI   or   DSTcap > PARcapF);
038000091009           wRecOK = 'N';
038100091009         endif;
038200091009      /end-free
038300020423     C*
038400020423     C* Tipo particolarità
038500020424     C                   if        wrecok = 'S'
038600020423     C                   if        pardos <> *blanks
038700020423     C                   if        dstdos <> pardos
038800020423     C                   eval      wrecok = 'N'
038900020423     C                   endif
039000020423     C                   endif
039100020424     C                   endif
039200020424     C*
039300020424     C* Ragione Sociale
039400020424     C                   if        wrecok = 'S'
039500020424     C                   if        parrag > *blanks
039600020424     C                   eval      %len(wrkRICERCA) = %len(%trim(parrag))
039700020424     C                   eval      wrkRICERCA = %trim(parrag)
039800020424     C     wrkRICERCA    scan      dstrag
039900020424     C                   if        not %found
040000020424     C                   movel     'N'           wrecok
040100020424     C                   endif
040200020424     C                   endif
040300020424     C                   endif
040400020426     C*
040500020426     C* Ricorrenze
040600020426     C                   if        wrecok = 'S'
040700020426     C                   if        parric > *zeros
040800020516     C                   if        dstnum <= parric
040900020426     C                   movel     'N'           wrecok
041000020426     C                   endif
041100020426     C                   endif
041200020426     C                   endif
041300040622     C*
041400040622     C* P.O.
041500040622     C                   if        wrecok = 'S'
041600040622     C                   if        parpouvid <> *zeros
041700040622     C                   if        parpouvid = 999
041800040622     C                   movel     dstlna        wPouAlfa          3
041900040622     C     wPouAlfa      lookup    L1                                     54
042000040622     C                   if        *IN54 = *OFF
042100040622     C                   movel     'N'           wrecok
042200040622     C                   endif
042300040622     C                   else
042400040622     C                   if        dstlna <> parpouvid
042500040622     C                   movel     'N'           wrecok
042600040622     C                   endif
042700040622     C                   endif
042800040622     C                   endif
042900040622     C                   endif
043000020423     C*
043100001005     C                   ENDSR
043200021206     C*------------------------------------------------------------------------*
043300040622     C* IMPCAM - IMPOSTA I CAMPI DEL FILE DALLA DS DI INPUT
043400021206     C*------------------------------------------------------------------------*
043500021206     C     impcam        BEGSR
043600021206     C*
043700030423     C                   clear                   TISIT0DS
043800021206     C                   movel     stprag        IT0RAG
043900021206     C                   movel     stpnaz        IT0NAZ
044000021206     C                   movel     stpprv        IT0PRV
044100021206     C                   movel     stpcap        IT0CAP
044200021206     C                   movel     stploc        IT0LOC
044300021206     C                   movel     stpind        IT0IND
044400021206     C*
044500021206     C                   ENDSR
044600001003     C*--------------------------------------------------------------------------------------------*
044700020424     C* Impostazioni iniziali
044800011029     C*--------------------------------------------------------------------------------------------*
044900001003     C     *inzsr        BEGSR
045000001003     C*
045100001003     C* Ricevimento parametri
045200020423     C     *entry        plist
045300020423     C                   parm                    kpjba
045400001003     C*
045500020423     C                   movel     kpjbu         param
045600040622     C*
045700040622     C* Se a video richiesti tutti i P.O. della'area li reperisco
045800040622     C                   if        PARPOUVID = 999
045900040622     C                   CLEAR                   TRUL31DS
046000040622     C                   CLEAR                   L1
046100040622     C                   EVAL      I31abi = 'AR'
046200040622     C                   EVAL      I31cpo = PARPOU
046300040622     C                   CALL      'TRUL31R'
046400040622     C                   PARM                    KPJBA
046500040622     C                   PARM                    TRUL31DS
046600040622     C                   IF        O31pog > *zeros
046700040622     C                   MOVEA     O31pog        L1
046800040622     C                   ELSE
046900040622     C                   MOVEL     PARPOU        L1(1)
047000040622     C                   ENDIF
047100040622     C                   endif
047200020423     C*
047300020424     C* Inizializzo tutti gli indicatori e le variabili di wrk
047400020424     C                   seton                                        05
047500020424     C                   movel     *blanks       depprv
047600040622     C                   z-add     *zeros        deppou
047700020424     C                   z-add     *zeros        flgsta
047800020424     C*
047900001003     C                   ENDSR
