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
005100091012     d  PARcapI                       9                                         C.a.p. inizio range
005200091012     d  PARcapF                       9                                         C.a.p. fine   range
005300060414     d  PARrag                       35                                         Ragione Sociale
005400060414     d  PARdos                        1                                         Tipo Destinatario
005500060414     d  PARsin                        1                                         Dettaglio Sinonimi
005600060414     d  PARddt                        1                                         Dest. con obbl.DDT
005700060414     d  PARips                        1                                         Dest. con inst.prest
005800091009     d  PARipsF                       3s 0                                      FilDestconInstrPrest
005900070326     d  PARfgi                        1                                         Dest. con inst.prest
006000091009     d  PARfgiF                       3s 0                                      FilDestconForzGiroCo
006100060414     d  PARric                        7s 0                                      Ricorrenza > di
006200060414     d  PARpou                        3s 0                                      P.O. utente ins/mod
006300060414     d  PARpouvid                     3s 0                                      P.O. utente ins/mod
006400060414     d  PARrot                        1                                         Salto pag. x Prov.
006500060414     d  PARrtpo                       1                                         Salto pag. x P.O.
006600070913     d  PARgcx                        1                                         Dest. con gg.chius.
006700090708     d  PARfic                        1                                         Dest. con forz.t.i.c
006800070913      *------------------
006900070917      * FLAGS BOOLEANI
007000070913      *------------------
007100070917     d $RecOk          s              1    inz(*off)
007200070917     d $FlgSta         s              1    inz(*off)
007300070917      *------------------
007400070917      * VARIABILI DI WRK
007500070917      *------------------
007600070917     d wDate_Iso       s               d   datfmt(*iso)  inz
007700070917     d wDate           s              8  0 inz
007800070917     d TotDestPrv      s              9  0 inz(*zeros)
007900070917     d TotDestTot      s              9  0 inz(*zeros)
008000070917     d wrkRICERCA      s             35    varying       inz
008100070913     d DEPprv          s                   like(STPprv)  inz
008200070913     d SAVrag          s                   like(STPrag)  inz
008300070913     d SAVind          s                   like(STPind)  inz
008400070913     d SAVnaz          s                   like(STPnaz)  inz
008500070913     d SAVprv          s                   like(STPprv)  inz
008600070913     d SAVloc          s                   like(STPloc)  inz
008700070913     d SAVcap          s                   like(STPcap)  inz
008800001003     C*--------------------------------------------------------------------------------------------*
008900001003     C* Main lines
009000001003     C*--------------------------------------------------------------------------------------------*
009100020424     C*
009200020424     C* Imposto i parametri di lancio in stampa
009300020424     C                   exsr      imppar
009400011029     C*
009500020424     C* Stampo la testata
009600020424     C                   exsr      states
009700020423     C*
009800040622     C* Stampo l'anagrafico dei destinatari disagiati/supermercati/appuntamenti
009900020423     C                   exsr      stadet
010000020423     C*
010100020423     C* Stampo la pagina di chiusura del prospetto
010200070913     C                   write     sitcfi
010300001003     C*
010400060104     c                   clear                   TIBS02ds
010500060104     c                   eval      T02tla = 'C'
010600060104     c                   call      'TIBS02R'
010700060104     c                   parm                    KPJBA
010800060104     c                   parm                    TIBS02ds
010900001003     C                   seton                                        LR
011000020423     C*--------------------------------------------------------------------------------------------*
011100020424     C* IMPPAR - Imposta la testata con i parametri di lancio
011200020423     C*--------------------------------------------------------------------------------------------*
011300020424     C     imppar        BEGSR
011400020423     C*
011500060414     C                   clear                   SITCT0
011600060414     C                   clear                   SITCP1
011700060104      *
011800060428      * Stampa la ragione sociale del P.O. utente di lancio
011900060428     c                   movel     RSUT          STPrsut
012000060104      * Stampa il nome del pgm
012100060104     c                   movel     SDSpgm        STPpgm
012200070917      *
012300070917      * Reperimento data corrente
012400070917     c                   movel     *date         wDate_Iso
012500070917     c                   move      wDate_Iso     wDate
012600070917      *
012700070917      * Aggancio tabella "VPO/DECO" per reperimento data attivazione
012800070917      *   dati in trk "XT1": filiale giro  e  giorni chiusura
012900070917     c                   clear                   dVPOdeco
013000070917     c                   clear                   TIBS02ds
013100070917     c                   eval      T02mod  = 'C'
013200070917     c                   eval      T02sif  = KNSIF
013300070917     c                   eval      T02cod  = 'VPO'
013400070917     c                   movel(P)  'DECO'        T02ke1
013500070917     c                   call      'TIBS02R'
013600070917     c                   parm                    KPJBA
013700070917     c                   parm                    TIBS02ds
013800070917    2c                   if        T02err  = *blanks
013900070917     c                   movel     T02uni        dVPOdeco
014000070917    2c                   endif
014100070917      *
014200070917      * Test se iniziata nuova gestione del trk "XT1" di AZDSE00F
014300070917     c                   eval      *in43   = (§VPOxt1 > Wdate)
014400020423     C*
014500060414     C* Nazione
014600091012     C                   select
014700091012     C                   when      parnaz = '999'
014800020423     C                   eval      stppnaz = 'TUTTE'
014900091012     C                   when      parnaz = *blanks
015000020423     C                   eval      stppnaz = 'ITA'
015100091012     C                   other
015200020423     C                   eval      stppnaz = parnaz
015300091012     C                   endsl
015400020423     C*
015500020423     C* Provincia
015600020423     C                   if        parprv = *blanks
015700020423     C                   eval      stppprv = 'TUTTE'
015800020423     C                   else
015900020423     C                   eval      stppprv = parprv
016000020423     C                   endif
016100020423     C*
016200020423     C* Ragione Sociale
016300020423     C                   if        parrag = *blanks
016400020423     C                   eval      stpprag = 'TUTTE'
016500020423     C                   else
016600020424     C                   eval      stpprag = 'TUTTO CIO'' CHE CONTIENE "' +
016700020424     C                                       %trim(parrag) + '"'
016800020423     C                   endif
016900091012      /free
017000091012         // - C.a.p. dal - al
017100091012         if  (PARcapI + PARcapF) = *blank;
017200091012           STPpCap = 'TUTTI';
017300091012         else;
017400091012           STPpCap = 'dal ' + PARcapI + ' al ' + PARcapF;
017500091012         endif;
017600091012      /end-free
017700060414      *
017800060414      * Solo destinatari con obbligo DDT in consegna
017900060414     c                   if        parDDT  = 'S'
018000060414     c                   eval      STPpddt = 'SI'
018100060414     c                   else
018200060414     c                   eval      STPpddt = 'NO'
018300060414     c                   endif
018400060414      *
018500060414      * Solo destinatari con instradamento prestabilito
018600060414     c                   if        parIPS  = 'S'
018700060414     c                   eval      STPpips = 'SI'
018800060414     c                   else
018900060414     c                   eval      STPpips = 'NO'
019000060414     c                   endif
019100091009      /free
019200091009         if  PARipsF > *zero;
019300091009           STPpIpsF = %editc( PARipsF : 'X' );
019400091009         endif;
019500091009      /end-free
019600070326      *
019700070326      * Solo destinatari con forzatura giro consegna
019800070326     c                   if        parFGI  = 'S'
019900070326     c                   eval      STPpfgi = 'SI'
020000070326     c                   else
020100070326     c                   eval      STPpfgi = 'NO'
020200070326     c                   endif
020300091009      /free
020400091009         if  PARfgiF > *zero;
020500091009           STPpFgiF = %editc( PARfgiF : 'X' );
020600091009         endif;
020700091009      /end-free
020800070913      *
020900070913      * Solo destinatari con giorni di chiusura
021000070913     c                   if        parGCX  = 'S'
021100070913     c                   eval      STPpgcx = 'SI'
021200070913     c                   else
021300070913     c                   eval      STPpgcx = 'NO'
021400070913     c                   endif
021500090708      *
021600090708      * Solo destinatari con forzatura incasso c/assegno
021700090708     c                   if        parFIC  = 'S'
021800090708     c                   eval      STPpfic = 'SI'
021900090708     c                   else
022000090708     c                   eval      STPpfic = 'NO'
022100090708     c                   endif
022200020423     C*
022300020423     C* Tipo particolarità
022400020423     C                   if        pardos = *blanks
022500020423     C                   eval      stppdos = 'TUTTE'
022600020423     C                   else
022700060104     c                   clear                   TIBS02ds
022800070913     c                   movel(p)  parDOS        T02ke1
022900060104     c                   exsr      Decod_DOS
023000060104     c                   movel     §TDPdes       STPpdos
023100020423     C                   endif
023200020423     C*
023300020423     C* Dettaglio sinonimi
023400060414     C                   if        parsin  = 'S'
023500060414     C                   eval      stppsin = 'SI'
023600020423     C                   else
023700060414     C                   eval      stppsin = 'NO'
023800020423     C                   endif
023900060414      *
024000060414      * Stampa dei parametri di lancio
024100060414     c                   write     SITCT0
024200060414     c                   write     SITCP1
024300060414      *
024400060414     c                   eval      *in05 = *on
024500060414      *
024600020423     C                   ENDSR
024700060104      *
024800060104      *---------------------------------------------------------------*
024900060104      * Decodifica Tipo Destinatario                                  *
025000060104      *---------------------------------------------------------------*
025100060104     c     Decod_DOS     BEGSR
025200060104      *
025300060104     c                   clear                   dTDP
025400060104     c                   eval      T02mod  = 'C'
025500060104     c                   eval      T02cod  = 'TDP'
025600060104     c*** già impostato: movel(p)  ???           T02ke1
025700060104     c                   call      'TIBS02R'
025800060104     c                   parm                    KPJBA
025900060104     c                   parm                    TIBS02ds
026000060104     c                   if        T02err  = *blanks
026100060104     c                   movel     T02uni        dTDP
026200060104     c                   endif
026300060104      *
026400060104     c                   ENDSR
026500060104      *
026600020424     C*--------------------------------------------------------------------------------------------*
026700020424     C* STATES - Routine di stampa testata
026800020424     C*--------------------------------------------------------------------------------------------*
026900020424     C     states        BEGSR
027000020424     C*
027100020424     C                   if        *in05 = *on
027200060414     C                   write     SITCT0
027300060414     C                   write     SITCT1
027400060414     C                   if        parsin = 'S'
027500060414     C                   write     SITCT2
027600020424     C                   endif
027700060414     C                   write     SITCT3
027800020424     C                   setoff                                       05
027900020424     C                   endif
028000020424     C*
028100020424     C                   ENDSR
028200011026     C*--------------------------------------------------------------------------------------------*
028300040622     C* STADET - Stampa il dettaglio dell'anagrafico destinatari disagiati/supermercati/appuntamenti
028400011026     C*--------------------------------------------------------------------------------------------*
028500020423     C     stadet        BEGSR
028600020424     C*
028700060116     C     *loval        setll     azdst99j
028800060116     C                   read      azdst99j
028900070917do  1C                   dow       not %eof(azdst99j)
029000020423     C* Eseguo il controllo di attinenza del record in funzione degli eventuali parametri richiesti
029100020423     C                   exsr      chkrec
029200020423     C* Se il trecord è attinente
029300070917if  2C                   if        $RecOK = *on
029400020423     C*
029500020424     C* Testo quindi l'eventuale rottura di codice x provincia
029600070917if  3C                   if        dstprv <> depprv AND
029700070917     C                             $FlgSta = *on
029800020424     C*
029900020424     C* Ad ogni rottura di provincia, se richiesto nel lancio salto pagina
030000070917if  4C                   if        parrot = 'S'
030100020516     C                   eval      stpdestot = 'TOTALE PER PROVINCIA:'
030200020516     C                   eval      stpnumtot = totdestprv
030300020516     C                   eval      totdestprv = *zeros
030400060414     C                   write     SITCTT
030500020424     C                   seton                                        05
030600020424     C                   exsr      states
030700070917e   4C                   endif
030800070917x   3C                   else
030900070917     C                   eval      $FlgSta = *on
031000070917e   3C                   endif
031100020424     C*
031200020429     C* Deposito la provincia corrente
031300020429     C                   eval      depprv = dstprv
031400020429     C*
031500060414     c                   clear                   SITCD1
031600020423     C                   eval      stprag = dstrag
031700020424     C                   eval      stpind = %trim(dstind) + ' >>> ' +
031800020424     C                                      %trim(dstkwi)
031900020423     C                   eval      stpnaz = dstnaz
032000020423     C                   eval      stpprv = dstprv
032100060414     C                   eval      stpcap = dstcap
032200020423     C                   eval      stploc = dstloc
032300060104      * Decodifico il tipo destinatario
032400060104     c                   clear                   TIBS02ds
032500060104     c                   movel(p)  DSTdos        T02ke1
032600060104     c                   exsr      Decod_DOS
032700060104     c                   movel     §TDPdes       STPdos
032800040622     C* Verifico se il destinatario disagiato/supermercato/appuntamento ha sinonimi
032900070917if  3C                   if        dsssin > *blanks
033000020423     C                   eval      stpess = 'SI'
033100060414     C***                else
033200060414     C***                eval      stpess = 'NO'
033300070917e   3C                   endif
033400060414      * Obbligo DDT in consegna
033500070917if  3c                   if        §DSTddt = 'S'
033600060414     c                   eval      STPddt  = 'SI'
033700070917e   3c                   endif
033800060414      * Instradamento prestabilito
033900070917if  3c                   if        §DSTlna > *zeros
034000060414     c                   move      §DSTlna       STPlni
034100060414     c                   move      §DSTznc       STPzni
034200070917e   3c                   endif
034300070905      * Codice giro consegna
034400070917if  3c                   if             OT0X1pog <> *zeros
034500070913     c                             or   OT0X1cgi <> *blanks
034600070913     c                   eval      STPpocgi = %trim( %editw( +
034700070913     c                                          OT0X1pog : '0   ' ) )
034800070913     c                                      + '-'
034900070913     c                   eval      STPcgi   = OT0X1cgi
035000070917e   3c                   endif
035100070913      * Giorni di chiusura
035200070917     c                   eval      *in43  = (§VPOxt1 > Wdate)
035300070913     c                   eval      STPgc1 = OT0X1gc1
035400070913     c                   eval      STPgc2 = OT0X1gc2
035500090709      * Tipo incasso c/assegni forzato
035600090709     c                   eval      STPticf = OT0X1ticf
035700020424     C*
035800020424     C* Testo il "fine-pagina"
035900020424     C                   exsr      states
036000020423     C*
036100020429     C* Quindi stampo la riga di dettaglio principale (solo se diverso dal destinatario già stamp.)
036200070917if  3C                   if        stprag = savrag AND
036300020429     C                             stpind = savind AND
036400020429     C                             stpnaz = savnaz AND
036500020429     C                             stpprv = savprv AND
036600060414     C                             stpcap = savcap AND
036700060414     C                             stploc = savloc
036800070917x   3C                   else
036900020426     C                   write     sitcd1
037000020516     C                   eval      totdestprv = totdestprv + 1
037100020516     C                   eval      totdesttot = totdesttot + 1
037200020429     C                   eval      savrag = stprag
037300020429     C                   eval      savind = stpind
037400020429     C                   eval      savnaz = stpnaz
037500020429     C                   eval      savprv = stpprv
037600060414     C                   eval      savcap = stpcap
037700020429     C                   eval      savloc = stploc
037800070917e   3C                   endif
037900020423     C*
038000020423     C* Se nel lancio è stato richiesto anche il dettaglio sinonimi ed esistono sinonimi ...
038100020423     C* ... stampo la seconda riga di dettaglio
038200070917if  3C                   if        parsin = 'S' AND
038300020423     C                             stpess = 'SI'
038400040601     C                   eval      stpsin = '- ' + dsssin
038500040601     C* Se il sinonimo è annullato lo evidenzio
038600040601     C                   if        dssatb = 'A'
038700040601     C                   move      '(A)'         stpsin
038800040601     C                   endif
038900020426     C                   write     sitcd2
039000070917e   3C                   endif
039100001003     C*
039200070917e   2C                   endif
039300060116     C                   read      azdst99j
039400070917e   1C                   enddo
039500020423     C*
039600020516     C* A fine file stampo una riga di totali per l'ultima provincia stampata...
039700070917if  1C                   if        parrot = 'S'
039800020516     C                   eval      stpdestot = 'TOTALE PER PROVINCIA:'
039900020516     C                   eval      stpnumtot = totdestprv
040000020516     C                   eval      totdestprv = *zeros
040100060414     C                   write     SITCTT
040200070917e   1C                   endif
040300020516     C* ...e una riga di totali generali
040400020516     C                   eval      stpdestot = 'TOTALE GENERALE:'
040500020516     C                   eval      stpnumtot = totdesttot
040600020516     C                   eval      totdesttot = *zeros
040700060414     C                   write     SITCTT
040800020516     C*
040900001003     C                   ENDSR
041000060414      *--------------------------------------------------------------------------------------------*
041100060414      * CHKREC - Verifico attinenza del record con i parametri richiesti nel lancio
041200060414      *--------------------------------------------------------------------------------------------*
041300060414     c     chkrec        BEGSR
041400060414      *
041500070917     c                   eval      $RecOk = *off
041600060414     c                   movel     DSTflo        dDSTflo
041700060414      *
041800060414     c                   SELECT
041900060414      *
042000060414      * No annullati
042100060414     c                   WHEN      DSTatb <> *blanks
042200060414     c                   leavesr
042300060414      *
042400060414      * Nazione
042500060414     c                   WHEN           PARnaz   = 'ITA'
042600060414     c                             and  DSTnaz  <> 'ITA'
042700060414     c                             and  DSTnaz  <> *blanks
042800060414     c                   leavesr
042900060414      *
043000060414     c                   WHEN           PARnaz  <> '999'
043100060414     c                             and  PARnaz  <> 'ITA'
043200060414     c                             and  DSTnaz  <> PARnaz
043300060414     c                   leavesr
043400060414      *
043500060414      * Provincia
043600060414     c                   WHEN           PARprv  <> *blanks
043700060414     c                             and  DSTprv  <> PARprv
043800060414     c                   leavesr
043900091009      /free
044000091009           when  (PARcapI + PARcapF) <> *blank   and
044100091009                 (DSTcap  < PARcapI  or  DSTcap  > PARcapF);
044200091009             leavesr;
044300091009      /end-free
044400060414      *
044500060414      * Tipo particolarità
044600060414     c                   WHEN           PARdos  <> *blanks
044700060414     c                             and  DSTdos  <> PARdos
044800060414     c                   leavesr
044900060414      *
045000060414      * Solo destinatari con obbligo DDT in consegna
045100060414     c                   WHEN           PARddt   = 'S'
045200060414     c                             and  §DSTddt <> 'S'
045300060414     c                   leavesr
045400060414      *
045500060414      * Solo destinatari con instradamento prestabilito
045600060828     c                   WHEN           PARips   = 'S'
045700060414     c                             and  §DSTlna <= *zeros
045800060414     c                   leavesr
045900091009      /free
046000091009           when   PARips  =  'S'     and
046100091009                  PARipsF >  *zero   and
046200091009                  §DSTlna <> %editc( PARipsF : 'X' );
046300091009             leavesr;
046400091009      /end-free
046500060414      *
046600060414     c                   ENDSL
046700060414      *
046800060414      * Ragione Sociale
046900060414     c                   if        PARrag <> *blanks
047000060414     c                   eval      wrkRICERCA = %trim(PARrag)
047100060414     c     wrkRICERCA    scan      DSTrag
047200060414     c                   if        NOT %found
047300060414     c                   leavesr
047400060414     c                   endif
047500060414     c                   endif
047600070917      *
047700070917      * Reperisce DSEFLO del trk "XT1"
047800070917     c                   clear                   TISIT0ds
047900070917     c                   clear                   TISIT0X1ds
048000070917     c                   eval      IT0naz = DSTnaz
048100070917     c                   eval      IT0prv = DSTprv
048200070917     c                   eval      IT0cap = DStcap
048300070917     c                   eval      IT0loc = DSTloc
048400070917     c                   eval      IT0ind = DSTind
048500070917     c                   eval      IT0rag = DSTrag
048600070917     c                   call      'TISIT5R'
048700070917     c                   parm                    TISIT0ds
048800070917     c                   parm                    TISIT0X1ds
048900070917      *
049000070917     c                   SELECT
049100070917      * - Solo destinatari con forzatura giro consegna
049200070917     c                   WHEN           PARfgi   = 'S'
049300070917     c                             and (OT0X1pog = *zeros
049400070917     c                             and  OT0X1cgi = *blanks)
049500070917     c                   leavesr
049600091009      /free
049700091009           when   PARfgi   =  'S'     and
049800091009                  PARfgiF  >  *zero   and
049900091009                  OT0X1pog <> PARfgiF;
050000091009             leavesr;
050100091009      /end-free
050200070917      * - Solo destinatari con giorni di chiusura
050300070917     c                   WHEN           PARgcx   = 'S'
050400070917     c                             and (OT0X1gc1 = *blanks
050500070917     c                             and  OT0X1gc2 = *blanks)
050600070917     c                   leavesr
050700090708      * - Solo destinatari con forzatura incasso c/assegno
050800090708     c                   WHEN           PARfic    = 'S'
050900090708     c                             and  OT0X1ticf = *blanks
051000090708     c                   leavesr
051100070917     c                   ENDSL
051200060414      *
051300070917     c                   eval      $RecOk = *on
051400060414      *
051500060414     c                   ENDSR
051600001003     C*--------------------------------------------------------------------------------------------*
051700020424     C* Impostazioni iniziali
051800011029     C*--------------------------------------------------------------------------------------------*
051900001003     C     *inzsr        BEGSR
052000001003     C*
052100001003     C* Ricevimento parametri
052200020423     C     *entry        plist
052300020423     C                   parm                    kpjba
052400060428      *
052500060428      * Reperimento dati utente
052600060428     c                   exsr      RepDatiUte
052700001003     C*
052800020423     C                   movel     kpjbu         param
052900020423     C*
053000020424     C* Inizializzo tutti gli indicatori e le variabili di wrk
053100020424     C                   seton                                        05
053200020424     C                   movel     *blanks       depprv
053300020429     C                   movel     *blanks       savrag
053400020429     C                   movel     *blanks       savind
053500020429     C                   movel     *blanks       savnaz
053600020429     C                   movel     *blanks       savprv
053700020429     C                   movel     *blanks       savloc
053800020429     C                   movel     *blanks       savcap
053900070917     C                   eval      $FlgSta = *off
054000020424     C*
054100001003     C                   ENDSR
054200060428      *
054300060428      *---------------------------------------------------------------*
054400060428      *?Reperimento Dati Utente                                      ?*
054500060428      *---------------------------------------------------------------*
054600060428     c     RepDatiUte    BEGSR
054700060428      *
054800060428     c     *dtaara       define    §azute        azuteds
054900060428     c     *dtaara       define    §datiute      ddatiute
055000060428     c                   in(e)     *dtaara
055100060428if  1c                   if        %error
055200060428     c                   eval      I34tla = 'L'
055300060428     c                   call      'TIBS34R'
055400060428     c                   parm                    TIBS34ds
055500060428     c                   in        *dtaara
055600060428e   1c                   endif
055700060428      *
055800060428     c                   ENDSR
055900070913      *
056000070913** - $GGS/$GIO
0561000709131LUNEDI
0562000709132MARTEDI
0563000709133MERCOLEDI
0564000709134GIOVEDI
0565000709135VENERDI
0566000709136SABATO
0567000709137DOMENICA
056800070913M          MATTINA
056900070913P          POMERIGGIO
