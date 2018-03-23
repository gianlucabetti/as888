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
002200060414     d  PARrag                       35                                         Ragione Sociale
002300060414     d  PARdos                        1                                         Tipo Destinatario
002400060414     d  PARsin                        1                                         Dettaglio Sinonimi
002500060414     d  PARddt                        1                                         Dest. con obbl.DDT
002600060414     d  PARips                        1                                         Dest. con inst.prest
002700070326     d  PARfgi                        1                                         Dest. con inst.prest
002800060414     d  PARric                        7s 0                                      Ricorrenza > di
002900060414     d  PARpou                        3s 0                                      P.O. utente ins/mod
003000060414     d  PARpouvid                     3s 0                                      P.O. utente ins/mod
003100060414     d  PARrot                        1                                         Salto pag. x Prov.
003200060414     d  PARrtpo                       1                                         Salto pag. x P.O.
003300021206     D*------------------
003400021206     D* DS DI PROCEDURA X PASSAGGIO PARAMETRI
003500021206     D*------------------
003600021206     D TISIT0DS      E DS                  INZ
003700040622     D*------------
003800040622     D* DS REPERIMENTO P.O.
003900040622     D*------------
004000040622     D TRUL31DS      E DS
004100060104      *
004200060104      * - Parametri x interrogazione tabelle
004300060104     d TIBS02ds      e ds                  inz
004400060104     d   T02mod      e                     inz('C')
004500060104     d   T02cod      e                     inz('TDP')
004600060104      * - Tab. "TDP" = Tipi Destinatari Particolari
004700060104     d dTDP          e ds                  inz
004800040622     D*-------------------
004900040622     D* FILIALI DEI CLIENTI DA VISUALIZZARE
005000040622     D*-------------------
005100040622     D L1              S              3    DIM(250) INZ(*zeros)
005200020424     D*------------------
005300020424     D* VARIABILI DI WRK
005400020424     D*------------------
005500020424     D wrecok          S              1
005600020424     D flgsta          S              1  0
005700020424     D depprv          S                   LIKE(dstprv)
005800040622     D deppou          S                   LIKE(dstlna)
005900020424     D wrkRICERCA      S             35    VARYING
006000020516     D totdestprv      S              9  0 INZ(*zeros)
006100020516     D totdesttot      S              9  0 INZ(*zeros)
006200001003     C*--------------------------------------------------------------------------------------------*
006300001003     C* Main lines
006400001003     C*--------------------------------------------------------------------------------------------*
006500020424     C*
006600020424     C* Imposto i parametri di lancio in stampa
006700020424     C                   exsr      imppar
006800011029     C*
006900020424     C* Stampo la testata
007000020424     C                   exsr      states
007100020423     C*
007200040622     C* Stampo l'anagrafico dei destinatari disagiati/supermercati/appuntamenti
007300020423     C                   exsr      stadet
007400020423     C*
007500020423     C* Stampo la pagina di chiusura del prospetto
007600020423     C                   exsr      stafin
007700040319     C*
007800040319     C* Lancio il driver TISIT5R inmodalità "SOLO CHIUSURA FILES"
007900040319     C                   clear                   TISIT0DS
008000040319     C                   movel     'C'           IT0TLA
008100040319     C                   call      'TISIT5R'
008200040319     C                   parm                    TISIT0DS
008300001003     C*
008400060104     c                   clear                   TIBS02ds
008500060104     c                   eval      T02tla = 'C'
008600060104     c                   call      'TIBS02R'
008700060104     c                   parm                    KPJBA
008800060104     c                   parm                    TIBS02ds
008900001003     C                   seton                                        LR
009000020423     C*--------------------------------------------------------------------------------------------*
009100020424     C* IMPPAR - Imposta la testata con i parametri di lancio
009200020423     C*--------------------------------------------------------------------------------------------*
009300020424     C     imppar        BEGSR
009400020423     C*
009500020423     C                   clear                   sitpt1
009600060104      *
009700060104      * Stampa il nome del pgm
009800060104     c                   movel     SDSpgm        STPpgm
009900040622     C*
010000040622     C* P.O. utente INS/VAR
010100040622     C                   if        parpouvid = 999
010200040622     C                   eval      stpppou = 'TUTTI I P.O. DELL''AREA'
010300040622     C                   else
010400040622     C                   if        parpouvid = *zeros
010500040622     C                   eval      stpppou = 'TUTTI'
010600040622     C                   else
010700040622     C                   movel     parpouvid     stpppou
010800040622     C                   endif
010900040622     C                   endif
011000020423     C*
011100040622     C* Nazione
011200020423     C                   if        parnaz = '999'
011300020423     C                   eval      stppnaz = 'TUTTE'
011400020423     C                   else
011500020423     C                   if        parnaz = *blanks
011600020423     C                   eval      stppnaz = 'ITA'
011700020423     C                   else
011800020423     C                   eval      stppnaz = parnaz
011900020423     C                   endif
012000020423     C                   endif
012100020423     C*
012200020423     C* Provincia
012300020423     C                   if        parprv = *blanks
012400020423     C                   eval      stppprv = 'TUTTE'
012500020423     C                   else
012600020423     C                   eval      stppprv = parprv
012700020423     C                   endif
012800020423     C*
012900020423     C* Ragione Sociale
013000020423     C                   if        parrag = *blanks
013100020423     C                   eval      stpprag = 'TUTTE'
013200020423     C                   else
013300020424     C                   eval      stpprag = 'TUTTO CIO'' CHE CONTIENE "' +
013400020424     C                                       %trim(parrag) + '"'
013500020423     C                   endif
013600020423     C*
013700020423     C* Tipo particolarità
013800060104     C                   if        pardos = *blanks
013900020423     C                   eval      stppdos = 'TUTTE'
014000060104     C                   else
014100060104     c                   clear                   TIBS02ds
014200060104     c                   movel(p)  PARdos        T02ke1
014300060104     c                   exsr      Decod_DOS
014400060104     c                   movel     §TDPdes       STPpdos
014500060104     C                   endif
014600020423     C*
014700020426     C* Ricorrenze
014800020426     C                   eval      stppric = parric
014900020424     C*
015000020423     C                   ENDSR
015100020424     C*--------------------------------------------------------------------------------------------*
015200020424     C* STATES - Routine di stampa testata
015300020424     C*--------------------------------------------------------------------------------------------*
015400020424     C     states        BEGSR
015500020424     C*
015600020424     C                   if        *in05 = *on
015700020424     C                   if        flgsta <> *zeros
015800020424     C                   write     sitpri
015900020424     C                   endif
016000020424     C                   write     sitpt1
016100020424     C                   write     sitpt3
016200020424     C                   setoff                                       05
016300020424     C                   endif
016400020424     C*
016500020424     C                   ENDSR
016600020423     C*--------------------------------------------------------------------------------------------*
016700020423     C* STAFIN - Stampa la pagina di chiusura prospetto
016800020423     C*--------------------------------------------------------------------------------------------*
016900020423     C     stafin        BEGSR
017000020423     C*
017100020423     C                   write     sitpri
017200020423     C                   write     sitpfi
017300020423     C*
017400020423     C                   ENDSR
017500011026     C*--------------------------------------------------------------------------------------------*
017600040622     C* STADET - Stampa il dettaglio delle proposte destinatari disagiati/supermercati/appuntamenti
017700011026     C*--------------------------------------------------------------------------------------------*
017800020423     C     stadet        BEGSR
017900020424     C*
018000040622     C     *loval        setll     azdstp4l
018100040622     C                   read      azdstp4l
018200040622     C                   dow       not %eof(azdstp4l)
018300020423     C* Eseguo il controllo di attinenza del record in funzione degli eventuali parametri richiesti
018400020423     C                   exsr      chkrec
018500020423     C* Se il trecord è attinente
018600020423     C                   if        wrecok = 'S'
018700020423     C*
018800020424     C* Testo quindi l'eventuale rottura di codice x provincia
018900040622     C                   if        (dstprv <> depprv AND
019000040622     C                              flgsta <> *zeros)
019100020424     C*
019200020424     C* Ad ogni rottura di provincia, se richiesto nel lancio salto pagina
019300020424     C                   if        parrot = 'S'
019400020516     C                   eval      stpdestot = 'TOTALE PER PROVINCIA:'
019500020516     C                   eval      stpnumtot = totdestprv
019600020516     C                   eval      totdestprv = *zeros
019700020516     C                   write     sitptt
019800020424     C                   seton                                        05
019900020424     C                   exsr      states
020000020424     C                   endif
020100020424     C                   else
020200020424     C                   eval      flgsta = 1
020300020424     C                   endif
020400040622     C*
020500040622     C* Testo quindi l'eventuale rottura di codice x P.O.
020600040622     C                   if        (dstlna <> deppou AND
020700040622     C                              flgsta <> *zeros)
020800040622     C*
020900040622     C* Ad ogni rottura di P.O., se richiesto nel lancio salto pagina
021000040622     C                   if        parrtpo = 'S'
021100040622     C                   eval      stpdestot = 'TOTALE PER P.O.:'
021200040622     C                   eval      stpnumtot = totdestprv
021300040622     C                   eval      totdestprv = *zeros
021400040622     C                   write     sitptt
021500040622     C                   seton                                        05
021600040622     C                   exsr      states
021700040622     C                   endif
021800040622     C                   else
021900040622     C                   eval      flgsta = 1
022000040622     C                   endif
022100020424     C*
022200040622     C* Deposito la provincia corrente e il P.O. corrente
022300020429     C                   eval      depprv = dstprv
022400040622     C                   eval      deppou = dstlna
022500020429     C*
022600020423     C                   eval      stprag = dstrag
022700020426     C                   eval      stpind = %trim(dstind)
022800020423     C                   eval      stpnaz = dstnaz
022900020423     C                   eval      stpprv = dstprv
023000020423     C                   eval      stploc = dstloc
023100020423     C                   eval      stpcap = dstcap
023200060104      * Decodifico il tipo destinatario
023300060104     c                   clear                   TIBS02ds
023400060104     c                   movel(p)  DSTdos        T02ke1
023500060104     c                   exsr      Decod_DOS
023600060104     c                   movel     §TDPdes       STPdos
023700020426     C*
023800020426     C                   eval      stpric = dstnum
023900040622     C                   eval      stpute = dstute
024000040622     C                   eval      stppou = dstpou
024100040622     C                   eval      stplna = dstlna
024200020424     C*
024300020424     C* Testo il "fine-pagina"
024400020424     C                   exsr      states
024500020423     C*
024600020423     C* Quindi stampo la riga di dettaglio principale
024700020423     C                   write     sitpd1
024800020516     C                   eval      totdestprv = totdestprv + 1
024900020516     C                   eval      totdesttot = totdesttot + 1
025000021206     C*
025100021206     C* Imposto i campi della DS di passaggio procedura
025200021206     C                   exsr      impcam
025300021206     C*
025400021206     C* Chiamo il programma di reperimento
025500040319     C                   eval      IT0TLA = 'P'
025600021206     C                   call      'TISIT5R'
025700021206     C                   parm                    TISIT0DS
025800021206     C*
025900021206     C* Imposto in stampa i campi restituti dal programma di reperimento
026000021206     C                   eval      st2rag = OT0RAG
026100021206     C                   evAl      st2ind = OT0IND
026200021206     C                   eval      st2naz = OT0NAZ
026300021206     C                   eval      st2prv = OT0PRV
026400021206     C                   eval      st2cap = OT0CAP
026500021206     C                   eval      st2loc = OT0LOC
026600060104      * Decodifico il tipo destinatario
026700060104     c                   clear                   TIBS02ds
026800060104     c                   movel(p)  OT0dos        T02ke1
026900060104     c                   exsr      Decod_DOS
027000060104     c                   movel     §TDPdes       ST2dos
027100021206     C*
027200021206     C* Testo il "fine-pagina"
027300021206     C                   exsr      states
027400021206     C*
027500021206     C* Quindi stampo la riga di dettaglio secondaria
027600021206     C                   write     sitpd2
027700001003     C*
027800020423     C                   endif
027900040622     C                   read      azdstp4l
028000020423     C                   enddo
028100020516     C*
028200020516     C* A fine file stampo una riga di totali per l'ultima provincia stampata...
028300020516     C                   if        parrot = 'S'
028400020516     C                   eval      stpdestot = 'TOTALE PER PROVINCIA:'
028500020516     C                   eval      stpnumtot = totdestprv
028600020516     C                   eval      totdestprv = *zeros
028700020516     C                   write     sitptt
028800020516     C                   endif
028900040622     C*
029000040622     C* A fine file stampo una riga di totali per l'ultimo P.O. stampata...
029100040622     C                   if        parrtpo = 'S'
029200040622     C                   eval      stpdestot = 'TOTALE PER P.O.:'
029300040622     C                   eval      stpnumtot = totdestprv
029400040622     C                   eval      totdestprv = *zeros
029500040622     C                   write     sitptt
029600040622     C                   endif
029700020516     C* ...e una riga di totali generali
029800020516     C                   eval      stpdestot = 'TOTALE GENERALE:'
029900020516     C                   eval      stpnumtot = totdesttot
030000020516     C                   eval      totdesttot = *zeros
030100020516     C                   write     sitptt
030200020423     C*
030300001003     C                   ENDSR
030400060104      *
030500060104      *---------------------------------------------------------------*
030600060104      * Decodifica Tipo Destinatario                                  *
030700060104      *---------------------------------------------------------------*
030800060104     c     Decod_DOS     BEGSR
030900060104      *
031000060104     c                   clear                   dTDP
031100060104     c                   eval      T02mod  = 'C'
031200060104     c                   eval      T02cod  = 'TDP'
031300060104     c*** già impostato: movel(p)  ???           T02ke1
031400060104     c                   call      'TIBS02R'
031500060104     c                   parm                    KPJBA
031600060104     c                   parm                    TIBS02ds
031700060104     c                   if        T02err  = *blanks
031800060104     c                   movel     T02uni        dTDP
031900060104     c                   endif
032000060104      *
032100060104     c                   ENDSR
032200060104      *
032300001005     C*--------------------------------------------------------------------------------------------*
032400020423     C* CHKREC - Verifico attinenza del record con i parametri richiesti nel lancio
032500001005     C*--------------------------------------------------------------------------------------------*
032600020423     C     chkrec        BEGSR
032700020423     C*
032800020424     C                   eval      wrecok = 'S'
032900020424     C*
033000020424     C* No annullati
033100020424     C                   if        wrecok = 'S'
033200020424     C                   if        dstatb <> *blanks
033300020424     C                   eval      wrecok = 'N'
033400020424     C                   endif
033500020424     C                   endif
033600001005     C*
033700020423     C* Nazione
033800020424     C                   if        wrecok = 'S'
033900020423     C                   if        parnaz <> '999'
034000020423     C                   if        parnaz = 'ITA'
034100020423     C                   if        dstnaz <> 'ITA' AND
034200020423     C                             dstnaz <> *blanks
034300020423     C                   eval      wrecok = 'N'
034400020423     C                   endif
034500020423     C                   else
034600020423     C                   if        dstnaz <> parnaz
034700020423     C                   eval      wrecok = 'N'
034800020423     C                   endif
034900020423     C                   endif
035000020423     C                   endif
035100020424     C                   endif
035200020423     C*
035300020423     C* Provincia
035400020424     C                   if        wrecok = 'S'
035500020423     C                   if        parprv <> *blanks
035600020423     C                   if        dstprv <> parprv
035700020423     C                   eval      wrecok = 'N'
035800020423     C                   endif
035900020423     C                   endif
036000020424     C                   endif
036100020423     C*
036200020423     C* Tipo particolarità
036300020424     C                   if        wrecok = 'S'
036400020423     C                   if        pardos <> *blanks
036500020423     C                   if        dstdos <> pardos
036600020423     C                   eval      wrecok = 'N'
036700020423     C                   endif
036800020423     C                   endif
036900020424     C                   endif
037000020424     C*
037100020424     C* Ragione Sociale
037200020424     C                   if        wrecok = 'S'
037300020424     C                   if        parrag > *blanks
037400020424     C                   eval      %len(wrkRICERCA) = %len(%trim(parrag))
037500020424     C                   eval      wrkRICERCA = %trim(parrag)
037600020424     C     wrkRICERCA    scan      dstrag
037700020424     C                   if        not %found
037800020424     C                   movel     'N'           wrecok
037900020424     C                   endif
038000020424     C                   endif
038100020424     C                   endif
038200020426     C*
038300020426     C* Ricorrenze
038400020426     C                   if        wrecok = 'S'
038500020426     C                   if        parric > *zeros
038600020516     C                   if        dstnum <= parric
038700020426     C                   movel     'N'           wrecok
038800020426     C                   endif
038900020426     C                   endif
039000020426     C                   endif
039100040622     C*
039200040622     C* P.O.
039300040622     C                   if        wrecok = 'S'
039400040622     C                   if        parpouvid <> *zeros
039500040622     C                   if        parpouvid = 999
039600040622     C                   movel     dstlna        wPouAlfa          3
039700040622     C     wPouAlfa      lookup    L1                                     54
039800040622     C                   if        *IN54 = *OFF
039900040622     C                   movel     'N'           wrecok
040000040622     C                   endif
040100040622     C                   else
040200040622     C                   if        dstlna <> parpouvid
040300040622     C                   movel     'N'           wrecok
040400040622     C                   endif
040500040622     C                   endif
040600040622     C                   endif
040700040622     C                   endif
040800020423     C*
040900001005     C                   ENDSR
041000021206     C*------------------------------------------------------------------------*
041100040622     C* IMPCAM - IMPOSTA I CAMPI DEL FILE DALLA DS DI INPUT
041200021206     C*------------------------------------------------------------------------*
041300021206     C     impcam        BEGSR
041400021206     C*
041500030423     C                   clear                   TISIT0DS
041600021206     C                   movel     stprag        IT0RAG
041700021206     C                   movel     stpnaz        IT0NAZ
041800021206     C                   movel     stpprv        IT0PRV
041900021206     C                   movel     stpcap        IT0CAP
042000021206     C                   movel     stploc        IT0LOC
042100021206     C                   movel     stpind        IT0IND
042200021206     C*
042300021206     C                   ENDSR
042400001003     C*--------------------------------------------------------------------------------------------*
042500020424     C* Impostazioni iniziali
042600011029     C*--------------------------------------------------------------------------------------------*
042700001003     C     *inzsr        BEGSR
042800001003     C*
042900001003     C* Ricevimento parametri
043000020423     C     *entry        plist
043100020423     C                   parm                    kpjba
043200001003     C*
043300020423     C                   movel     kpjbu         param
043400040622     C*
043500040622     C* Se a video richiesti tutti i P.O. della'area li reperisco
043600040622     C                   if        PARPOUVID = 999
043700040622     C                   CLEAR                   TRUL31DS
043800040622     C                   CLEAR                   L1
043900040622     C                   EVAL      I31abi = 'AR'
044000040622     C                   EVAL      I31cpo = PARPOU
044100040622     C                   CALL      'TRUL31R'
044200040622     C                   PARM                    KPJBA
044300040622     C                   PARM                    TRUL31DS
044400040622     C                   IF        O31pog > *zeros
044500040622     C                   MOVEA     O31pog        L1
044600040622     C                   ELSE
044700040622     C                   MOVEL     PARPOU        L1(1)
044800040622     C                   ENDIF
044900040622     C                   endif
045000020423     C*
045100020424     C* Inizializzo tutti gli indicatori e le variabili di wrk
045200020424     C                   seton                                        05
045300020424     C                   movel     *blanks       depprv
045400040622     C                   z-add     *zeros        deppou
045500020424     C                   z-add     *zeros        flgsta
045600020424     C*
045700001003     C                   ENDSR
