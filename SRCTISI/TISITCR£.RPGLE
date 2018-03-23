000100001005     F*--------------------------------------------------------------------------------------------*
000200001005     F* Data base
000300001005     F*--------------------------------------------------------------------------------------------*
000400060116     Fazdst99j  if   e           k disk
000500020426     Ftisitcp   O    e             PRINTER oflind(*in05)
000600070913      *
000700070913      * SCHIERE
000800070913      *
000900070913      * - Decodifica giorni di chiusura
001000070913     d $GgS            s              1    dim(9)   ctdata  perrcd(1)
001100070913     d $Gio            s             20    dim(9)   alt($GgS)
001200001005     D*--------------------------------------------------------------------------------------------*
001300001005     D* Data structure
001400001005     D*--------------------------------------------------------------------------------------------*
001500020423     D*------------
001600020423     D* ARCHITETTURA
001700020423     D*------------
001800020423     D KPJBA         E DS
001900060428      *
002000060428      * - Parametri x Controllo profilo utenti
002100060428     d TIBS34ds      e ds                  inz
002200060428      * - Ds di riferimento al file esterno AZUTE00F
002300060428     d AZUTEds       e ds                  inz  extname(AZUTE00F)
002400060428      * - Ds per dati organigramma
002500070913     d dDatiUte      e ds                  inz
002600060104      *
002700060104     d Status         sds           333
002800060104     d   SDSpgm          *proc
002900060104      *
003000070917      * - Parametri x ricerca/controllo tabelle
003100060104     d TIBS02ds      e ds                  inz
003200060104     d   T02mod      e                     inz('C')
003300060104     d   T02cod      e                     inz('TDP')
003400060104      * - Tab. "TDP" = Tipi Destinatari Particolari
003500060104     d dTDP          e ds                  inz
003600070917      * - Tab. VPO/DECO = Variabili Programmi Operativi: Decorrenze
003700070917     d dVPOdeco      e ds                  inz
003800060414      *
003900060414      * - Flag Operativi di AZDST (DSTFLO)
004000060414     d dDSTflo       e ds                  inz
004100070905      *
004200070905      * - Parametri per TISIT5R (reperimento destinatari particolari)
004300070905     d TISIT0ds      e ds                  inz
004400070905     d TISIT0X1ds    e ds                  inz
004500070913      *------------
004600070913      * DS DI RIDEFINIZIONE PARAMETRI DI INPUT
004700070913      *------------
004800070913     d PARAM           DS
004900060414     d  PARnaz                        3                                         Nazione
005000060414     d  PARprv                        3                                         Provincia
005100060414     d  PARrag                       35                                         Ragione Sociale
005200060414     d  PARdos                        1                                         Tipo Destinatario
005300060414     d  PARsin                        1                                         Dettaglio Sinonimi
005400060414     d  PARddt                        1                                         Dest. con obbl.DDT
005500060414     d  PARips                        1                                         Dest. con inst.prest
005600070326     d  PARfgi                        1                                         Dest. con inst.prest
005700060414     d  PARric                        7s 0                                      Ricorrenza > di
005800060414     d  PARpou                        3s 0                                      P.O. utente ins/mod
005900060414     d  PARpouvid                     3s 0                                      P.O. utente ins/mod
006000060414     d  PARrot                        1                                         Salto pag. x Prov.
006100060414     d  PARrtpo                       1                                         Salto pag. x P.O.
006200070913     d  PARgcx                        1                                         Dest. con gg.chius.
006300090708     d  PARfic                        1                                         Dest. con forz.t.i.c
006400070913      *------------------
006500070917      * FLAGS BOOLEANI
006600070913      *------------------
006700070917     d $RecOk          s              1    inz(*off)
006800070917     d $FlgSta         s              1    inz(*off)
006900070917      *------------------
007000070917      * VARIABILI DI WRK
007100070917      *------------------
007200070917     d wDate_Iso       s               d   datfmt(*iso)  inz
007300070917     d wDate           s              8  0 inz
007400070917     d TotDestPrv      s              9  0 inz(*zeros)
007500070917     d TotDestTot      s              9  0 inz(*zeros)
007600070917     d wrkRICERCA      s             35    varying       inz
007700070913     d DEPprv          s                   like(STPprv)  inz
007800070913     d SAVrag          s                   like(STPrag)  inz
007900070913     d SAVind          s                   like(STPind)  inz
008000070913     d SAVnaz          s                   like(STPnaz)  inz
008100070913     d SAVprv          s                   like(STPprv)  inz
008200070913     d SAVloc          s                   like(STPloc)  inz
008300070913     d SAVcap          s                   like(STPcap)  inz
008400001003     C*--------------------------------------------------------------------------------------------*
008500001003     C* Main lines
008600001003     C*--------------------------------------------------------------------------------------------*
008700020424     C*
008800020424     C* Imposto i parametri di lancio in stampa
008900020424     C                   exsr      imppar
009000011029     C*
009100020424     C* Stampo la testata
009200020424     C                   exsr      states
009300020423     C*
009400040622     C* Stampo l'anagrafico dei destinatari disagiati/supermercati/appuntamenti
009500020423     C                   exsr      stadet
009600020423     C*
009700020423     C* Stampo la pagina di chiusura del prospetto
009800070913     C                   write     sitcfi
009900001003     C*
010000060104     c                   clear                   TIBS02ds
010100060104     c                   eval      T02tla = 'C'
010200060104     c                   call      'TIBS02R'
010300060104     c                   parm                    KPJBA
010400060104     c                   parm                    TIBS02ds
010500001003     C                   seton                                        LR
010600020423     C*--------------------------------------------------------------------------------------------*
010700020424     C* IMPPAR - Imposta la testata con i parametri di lancio
010800020423     C*--------------------------------------------------------------------------------------------*
010900020424     C     imppar        BEGSR
011000020423     C*
011100060414     C                   clear                   SITCT0
011200060414     C                   clear                   SITCP1
011300060104      *
011400060428      * Stampa la ragione sociale del P.O. utente di lancio
011500060428     c                   movel     RSUT          STPrsut
011600060104      * Stampa il nome del pgm
011700060104     c                   movel     SDSpgm        STPpgm
011800070917      *
011900070917      * Reperimento data corrente
012000070917     c                   movel     *date         wDate_Iso
012100070917     c                   move      wDate_Iso     wDate
012200070917      *
012300070917      * Aggancio tabella "VPO/DECO" per reperimento data attivazione
012400070917      *   dati in trk "XT1": filiale giro  e  giorni chiusura
012500070917     c                   clear                   dVPOdeco
012600070917     c                   clear                   TIBS02ds
012700070917     c                   eval      T02mod  = 'C'
012800070917     c                   eval      T02sif  = KNSIF
012900070917     c                   eval      T02cod  = 'VPO'
013000070917     c                   movel(P)  'DECO'        T02ke1
013100070917     c                   call      'TIBS02R'
013200070917     c                   parm                    KPJBA
013300070917     c                   parm                    TIBS02ds
013400070917    2c                   if        T02err  = *blanks
013500070917     c                   movel     T02uni        dVPOdeco
013600070917    2c                   endif
013700070917      *
013800070917      * Test se iniziata nuova gestione del trk "XT1" di AZDSE00F
013900070917     c                   eval      *in43   = (§VPOxt1 > Wdate)
014000020423     C*
014100060414     C* Nazione
014200020423     C                   if        parnaz = '999'
014300020423     C                   eval      stppnaz = 'TUTTE'
014400020423     C                   else
014500020423     C                   if        parnaz = *blanks
014600020423     C                   eval      stppnaz = 'ITA'
014700020423     C                   else
014800020423     C                   eval      stppnaz = parnaz
014900020423     C                   endif
015000020423     C                   endif
015100020423     C*
015200020423     C* Provincia
015300020423     C                   if        parprv = *blanks
015400020423     C                   eval      stppprv = 'TUTTE'
015500020423     C                   else
015600020423     C                   eval      stppprv = parprv
015700020423     C                   endif
015800020423     C*
015900020423     C* Ragione Sociale
016000020423     C                   if        parrag = *blanks
016100020423     C                   eval      stpprag = 'TUTTE'
016200020423     C                   else
016300020424     C                   eval      stpprag = 'TUTTO CIO'' CHE CONTIENE "' +
016400020424     C                                       %trim(parrag) + '"'
016500020423     C                   endif
016600060414      *
016700060414      * Solo destinatari con obbligo DDT in consegna
016800060414     c                   if        parDDT  = 'S'
016900060414     c                   eval      STPpddt = 'SI'
017000060414     c                   else
017100060414     c                   eval      STPpddt = 'NO'
017200060414     c                   endif
017300060414      *
017400060414      * Solo destinatari con instradamento prestabilito
017500060414     c                   if        parIPS  = 'S'
017600060414     c                   eval      STPpips = 'SI'
017700060414     c                   else
017800060414     c                   eval      STPpips = 'NO'
017900060414     c                   endif
018000070326      *
018100070326      * Solo destinatari con forzatura giro consegna
018200070326     c                   if        parFGI  = 'S'
018300070326     c                   eval      STPpfgi = 'SI'
018400070326     c                   else
018500070326     c                   eval      STPpfgi = 'NO'
018600070326     c                   endif
018700070913      *
018800070913      * Solo destinatari con giorni di chiusura
018900070913     c                   if        parGCX  = 'S'
019000070913     c                   eval      STPpgcx = 'SI'
019100070913     c                   else
019200070913     c                   eval      STPpgcx = 'NO'
019300070913     c                   endif
019400090708      *
019500090708      * Solo destinatari con forzatura incasso c/assegno
019600090708     c                   if        parFIC  = 'S'
019700090708     c                   eval      STPpfic = 'SI'
019800090708     c                   else
019900090708     c                   eval      STPpfic = 'NO'
020000090708     c                   endif
020100020423     C*
020200020423     C* Tipo particolarità
020300020423     C                   if        pardos = *blanks
020400020423     C                   eval      stppdos = 'TUTTE'
020500020423     C                   else
020600060104     c                   clear                   TIBS02ds
020700070913     c                   movel(p)  parDOS        T02ke1
020800060104     c                   exsr      Decod_DOS
020900060104     c                   movel     §TDPdes       STPpdos
021000020423     C                   endif
021100020423     C*
021200020423     C* Dettaglio sinonimi
021300060414     C                   if        parsin  = 'S'
021400060414     C                   eval      stppsin = 'SI'
021500020423     C                   else
021600060414     C                   eval      stppsin = 'NO'
021700020423     C                   endif
021800060414      *
021900060414      * Stampa dei parametri di lancio
022000060414     c                   write     SITCT0
022100060414     c                   write     SITCP1
022200060414      *
022300060414     c                   eval      *in05 = *on
022400060414      *
022500020423     C                   ENDSR
022600060104      *
022700060104      *---------------------------------------------------------------*
022800060104      * Decodifica Tipo Destinatario                                  *
022900060104      *---------------------------------------------------------------*
023000060104     c     Decod_DOS     BEGSR
023100060104      *
023200060104     c                   clear                   dTDP
023300060104     c                   eval      T02mod  = 'C'
023400060104     c                   eval      T02cod  = 'TDP'
023500060104     c*** già impostato: movel(p)  ???           T02ke1
023600060104     c                   call      'TIBS02R'
023700060104     c                   parm                    KPJBA
023800060104     c                   parm                    TIBS02ds
023900060104     c                   if        T02err  = *blanks
024000060104     c                   movel     T02uni        dTDP
024100060104     c                   endif
024200060104      *
024300060104     c                   ENDSR
024400060104      *
024500020424     C*--------------------------------------------------------------------------------------------*
024600020424     C* STATES - Routine di stampa testata
024700020424     C*--------------------------------------------------------------------------------------------*
024800020424     C     states        BEGSR
024900020424     C*
025000020424     C                   if        *in05 = *on
025100060414     C                   write     SITCT0
025200060414     C                   write     SITCT1
025300060414     C                   if        parsin = 'S'
025400060414     C                   write     SITCT2
025500020424     C                   endif
025600060414     C                   write     SITCT3
025700020424     C                   setoff                                       05
025800020424     C                   endif
025900020424     C*
026000020424     C                   ENDSR
026100011026     C*--------------------------------------------------------------------------------------------*
026200040622     C* STADET - Stampa il dettaglio dell'anagrafico destinatari disagiati/supermercati/appuntamenti
026300011026     C*--------------------------------------------------------------------------------------------*
026400020423     C     stadet        BEGSR
026500020424     C*
026600060116     C     *loval        setll     azdst99j
026700060116     C                   read      azdst99j
026800070917do  1C                   dow       not %eof(azdst99j)
026900020423     C* Eseguo il controllo di attinenza del record in funzione degli eventuali parametri richiesti
027000020423     C                   exsr      chkrec
027100020423     C* Se il trecord è attinente
027200070917if  2C                   if        $RecOK = *on
027300020423     C*
027400020424     C* Testo quindi l'eventuale rottura di codice x provincia
027500070917if  3C                   if        dstprv <> depprv AND
027600070917     C                             $FlgSta = *on
027700020424     C*
027800020424     C* Ad ogni rottura di provincia, se richiesto nel lancio salto pagina
027900070917if  4C                   if        parrot = 'S'
028000020516     C                   eval      stpdestot = 'TOTALE PER PROVINCIA:'
028100020516     C                   eval      stpnumtot = totdestprv
028200020516     C                   eval      totdestprv = *zeros
028300060414     C                   write     SITCTT
028400020424     C                   seton                                        05
028500020424     C                   exsr      states
028600070917e   4C                   endif
028700070917x   3C                   else
028800070917     C                   eval      $FlgSta = *on
028900070917e   3C                   endif
029000020424     C*
029100020429     C* Deposito la provincia corrente
029200020429     C                   eval      depprv = dstprv
029300020429     C*
029400060414     c                   clear                   SITCD1
029500020423     C                   eval      stprag = dstrag
029600020424     C                   eval      stpind = %trim(dstind) + ' >>> ' +
029700020424     C                                      %trim(dstkwi)
029800020423     C                   eval      stpnaz = dstnaz
029900020423     C                   eval      stpprv = dstprv
030000060414     C                   eval      stpcap = dstcap
030100020423     C                   eval      stploc = dstloc
030200060104      * Decodifico il tipo destinatario
030300060104     c                   clear                   TIBS02ds
030400060104     c                   movel(p)  DSTdos        T02ke1
030500060104     c                   exsr      Decod_DOS
030600060104     c                   movel     §TDPdes       STPdos
030700040622     C* Verifico se il destinatario disagiato/supermercato/appuntamento ha sinonimi
030800070917if  3C                   if        dsssin > *blanks
030900020423     C                   eval      stpess = 'SI'
031000060414     C***                else
031100060414     C***                eval      stpess = 'NO'
031200070917e   3C                   endif
031300060414      * Obbligo DDT in consegna
031400070917if  3c                   if        §DSTddt = 'S'
031500060414     c                   eval      STPddt  = 'SI'
031600070917e   3c                   endif
031700060414      * Instradamento prestabilito
031800070917if  3c                   if        §DSTlna > *zeros
031900060414     c                   move      §DSTlna       STPlni
032000060414     c                   move      §DSTznc       STPzni
032100070917e   3c                   endif
032200070905      * Codice giro consegna
032300070917if  3c                   if             OT0X1pog <> *zeros
032400070913     c                             or   OT0X1cgi <> *blanks
032500070913     c                   eval      STPpocgi = %trim( %editw( +
032600070913     c                                          OT0X1pog : '0   ' ) )
032700070913     c                                      + '-'
032800070913     c                   eval      STPcgi   = OT0X1cgi
032900070917e   3c                   endif
033000070913      * Giorni di chiusura
033100070917     c                   eval      *in43  = (§VPOxt1 > Wdate)
033200070913     c                   eval      STPgc1 = OT0X1gc1
033300070913     c                   eval      STPgc2 = OT0X1gc2
033400090709      * Tipo incasso c/assegni forzato
033500090709     c                   eval      STPticf = OT0X1ticf
033600020424     C*
033700020424     C* Testo il "fine-pagina"
033800020424     C                   exsr      states
033900020423     C*
034000020429     C* Quindi stampo la riga di dettaglio principale (solo se diverso dal destinatario già stamp.)
034100070917if  3C                   if        stprag = savrag AND
034200020429     C                             stpind = savind AND
034300020429     C                             stpnaz = savnaz AND
034400020429     C                             stpprv = savprv AND
034500060414     C                             stpcap = savcap AND
034600060414     C                             stploc = savloc
034700070917x   3C                   else
034800020426     C                   write     sitcd1
034900020516     C                   eval      totdestprv = totdestprv + 1
035000020516     C                   eval      totdesttot = totdesttot + 1
035100020429     C                   eval      savrag = stprag
035200020429     C                   eval      savind = stpind
035300020429     C                   eval      savnaz = stpnaz
035400020429     C                   eval      savprv = stpprv
035500060414     C                   eval      savcap = stpcap
035600020429     C                   eval      savloc = stploc
035700070917e   3C                   endif
035800020423     C*
035900020423     C* Se nel lancio è stato richiesto anche il dettaglio sinonimi ed esistono sinonimi ...
036000020423     C* ... stampo la seconda riga di dettaglio
036100070917if  3C                   if        parsin = 'S' AND
036200020423     C                             stpess = 'SI'
036300040601     C                   eval      stpsin = '- ' + dsssin
036400040601     C* Se il sinonimo è annullato lo evidenzio
036500040601     C                   if        dssatb = 'A'
036600040601     C                   move      '(A)'         stpsin
036700040601     C                   endif
036800020426     C                   write     sitcd2
036900070917e   3C                   endif
037000001003     C*
037100070917e   2C                   endif
037200060116     C                   read      azdst99j
037300070917e   1C                   enddo
037400020423     C*
037500020516     C* A fine file stampo una riga di totali per l'ultima provincia stampata...
037600070917if  1C                   if        parrot = 'S'
037700020516     C                   eval      stpdestot = 'TOTALE PER PROVINCIA:'
037800020516     C                   eval      stpnumtot = totdestprv
037900020516     C                   eval      totdestprv = *zeros
038000060414     C                   write     SITCTT
038100070917e   1C                   endif
038200020516     C* ...e una riga di totali generali
038300020516     C                   eval      stpdestot = 'TOTALE GENERALE:'
038400020516     C                   eval      stpnumtot = totdesttot
038500020516     C                   eval      totdesttot = *zeros
038600060414     C                   write     SITCTT
038700020516     C*
038800001003     C                   ENDSR
038900060414      *--------------------------------------------------------------------------------------------*
039000060414      * CHKREC - Verifico attinenza del record con i parametri richiesti nel lancio
039100060414      *--------------------------------------------------------------------------------------------*
039200060414     c     chkrec        BEGSR
039300060414      *
039400070917     c                   eval      $RecOk = *off
039500060414     c                   movel     DSTflo        dDSTflo
039600060414      *
039700060414     c                   SELECT
039800060414      *
039900060414      * No annullati
040000060414     c                   WHEN      DSTatb <> *blanks
040100060414     c                   leavesr
040200060414      *
040300060414      * Nazione
040400060414     c                   WHEN           PARnaz   = 'ITA'
040500060414     c                             and  DSTnaz  <> 'ITA'
040600060414     c                             and  DSTnaz  <> *blanks
040700060414     c                   leavesr
040800060414      *
040900060414     c                   WHEN           PARnaz  <> '999'
041000060414     c                             and  PARnaz  <> 'ITA'
041100060414     c                             and  DSTnaz  <> PARnaz
041200060414     c                   leavesr
041300060414      *
041400060414      * Provincia
041500060414     c                   WHEN           PARprv  <> *blanks
041600060414     c                             and  DSTprv  <> PARprv
041700060414     c                   leavesr
041800060414      *
041900060414      * Tipo particolarità
042000060414     c                   WHEN           PARdos  <> *blanks
042100060414     c                             and  DSTdos  <> PARdos
042200060414     c                   leavesr
042300060414      *
042400060414      * Solo destinatari con obbligo DDT in consegna
042500060414     c                   WHEN           PARddt   = 'S'
042600060414     c                             and  §DSTddt <> 'S'
042700060414     c                   leavesr
042800060414      *
042900060414      * Solo destinatari con instradamento prestabilito
043000060828     c                   WHEN           PARips   = 'S'
043100060414     c***                          and  §DSTlna  = *blanks
043200060414     c                             and  §DSTlna <= *zeros
043300060414     c                   leavesr
043400060414      *
043500060414     c                   ENDSL
043600060414      *
043700060414      * Ragione Sociale
043800060414     c                   if        PARrag <> *blanks
043900060414     c                   eval      wrkRICERCA = %trim(PARrag)
044000060414     c     wrkRICERCA    scan      DSTrag
044100060414     c                   if        NOT %found
044200060414     c                   leavesr
044300060414     c                   endif
044400060414     c                   endif
044500070917      *
044600070917      * Reperisce DSEFLO del trk "XT1"
044700070917     c                   clear                   TISIT0ds
044800070917     c                   clear                   TISIT0X1ds
044900070917     c                   eval      IT0naz = DSTnaz
045000070917     c                   eval      IT0prv = DSTprv
045100070917     c                   eval      IT0cap = DStcap
045200070917     c                   eval      IT0loc = DSTloc
045300070917     c                   eval      IT0ind = DSTind
045400070917     c                   eval      IT0rag = DSTrag
045500070917     c                   call      'TISIT5R'
045600070917     c                   parm                    TISIT0ds
045700070917     c                   parm                    TISIT0X1ds
045800070917      *
045900070917     c                   SELECT
046000070917      * - Solo destinatari con forzatura giro consegna
046100070917     c                   WHEN           PARfgi   = 'S'
046200070917     c                             and (OT0X1pog = *zeros
046300070917     c                             and  OT0X1cgi = *blanks)
046400070917     c                   leavesr
046500070917      * - Solo destinatari con giorni di chiusura
046600070917     c                   WHEN           PARgcx   = 'S'
046700070917     c                             and (OT0X1gc1 = *blanks
046800070917     c                             and  OT0X1gc2 = *blanks)
046900070917     c                   leavesr
047000090708      * - Solo destinatari con forzatura incasso c/assegno
047100090708     c                   WHEN           PARfic    = 'S'
047200090708     c                             and  OT0X1ticf = *blanks
047300090708     c                   leavesr
047400070917     c                   ENDSL
047500060414      *
047600070917     c                   eval      $RecOk = *on
047700060414      *
047800060414     c                   ENDSR
047900001003     C*--------------------------------------------------------------------------------------------*
048000020424     C* Impostazioni iniziali
048100011029     C*--------------------------------------------------------------------------------------------*
048200001003     C     *inzsr        BEGSR
048300001003     C*
048400001003     C* Ricevimento parametri
048500020423     C     *entry        plist
048600020423     C                   parm                    kpjba
048700060428      *
048800060428      * Reperimento dati utente
048900060428     c                   exsr      RepDatiUte
049000001003     C*
049100020423     C                   movel     kpjbu         param
049200020423     C*
049300020424     C* Inizializzo tutti gli indicatori e le variabili di wrk
049400020424     C                   seton                                        05
049500020424     C                   movel     *blanks       depprv
049600020429     C                   movel     *blanks       savrag
049700020429     C                   movel     *blanks       savind
049800020429     C                   movel     *blanks       savnaz
049900020429     C                   movel     *blanks       savprv
050000020429     C                   movel     *blanks       savloc
050100020429     C                   movel     *blanks       savcap
050200070917     C                   eval      $FlgSta = *off
050300020424     C*
050400001003     C                   ENDSR
050500060428      *
050600060428      *---------------------------------------------------------------*
050700060428      *?Reperimento Dati Utente                                      ?*
050800060428      *---------------------------------------------------------------*
050900060428     c     RepDatiUte    BEGSR
051000060428      *
051100060428     c     *dtaara       define    §azute        azuteds
051200060428     c     *dtaara       define    §datiute      ddatiute
051300060428     c                   in(e)     *dtaara
051400060428if  1c                   if        %error
051500060428     c                   eval      I34tla = 'L'
051600060428     c                   call      'TIBS34R'
051700060428     c                   parm                    TIBS34ds
051800060428     c                   in        *dtaara
051900060428e   1c                   endif
052000060428      *
052100060428     c                   ENDSR
052200070913      *
052300070913** - $GGS/$GIO
0524000709131LUNEDI
0525000709132MARTEDI
0526000709133MERCOLEDI
0527000709134GIOVEDI
0528000709135VENERDI
0529000709136SABATO
0530000709137DOMENICA
053100070913M          MATTINA
053200070913P          POMERIGGIO
