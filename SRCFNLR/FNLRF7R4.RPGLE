000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200141120     H*
000300030707     H*         ELENCO ANOMALIE PARTENDO DALLE BOLLE                 *
000400000000     H*--------------------------------------------------------------*
000500020911     FFNARB78L  IF   E           K DISK
000600941109     FFNART01L  IF   E           K DISK
000700080507     FFipdd01t  uF   E           K DISK
000800090303     Ffnlrf71l  uF a E           K DISK
000900070130     FFNBRV01L  IF   E           K DISK
001000070130     FFNBRV09L  IF   E           K DISK
001100070130     F                                     RENAME(FNBRV000:FNBRV009)
001200030707     FFNFVV01L  IF   E           K DISK
001300141120ab   Ffidst01l  IF   E           K DISK
001400150930     FFiar501L  IF   E           K DISK
001500941109     FFNLBL01L  IF   E           K DISK
001600941109     FFNLBL02L  IF   E           K DISK
001700941109     F                                     RENAME(FNLBL000:FNLBL002)
001800030728     FFNLRF7P   O    E             PRINTER OFLIND(*IN80)
001900941110     F                                     USROPN
002000040419     FFNDCD02L  IF   E           K DISK
002100040419     FFNDCT01L  IF   E           K DISK
002200101125     FTNTBE01L  IF   E           K DISK
002300050505     FTabel00f  IF   E           K DISK    USROPN
002400101125     Fwfnlrq11l uf a E           k DISK
002500101125     Fwfnlrq20f o    E             DISK
002600020911     d Karbifp         s                   Like(ArbIfp)
002700911018     D                 DS
002800941109     D  ARBAAS                 1      4  0
002900941109     D  ARBMGS                 5      8  0
003000941109     D  ARBDSP                 1      8  0
003100911018     D                 DS
003200941109     D  COMAAS                 1      4  0
003300941109     D  ARBLNP                 5      7  0
003400941109     D  ARBNRS                 8      9  0
003500941109     D  ARBNSP                10     16  0
003600941109     D  ARBSPE                 1     16
003700941109     D***
003800941109     D* PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
003900941109     D***
004000941109     D WLBDAT          DS                  INZ
004100941109     D  G02DAT                 1      8  0
004200941109     D  G02INV                 9     16  0
004300941109     D  G02ERR                17     17
004400941109     D  G02TGI                18     22  0
004500080508     d tmp             s             17
004600090224     d comnsc          s                   like(artnsc)
004700030723     D FNLR59DS      E DS
004800110905     d dxcodta       e ds
004900050505     D ds7r          E DS
005000141120ab   D DdstFLR       E DS                  INZ
005100110729     D fnlrf7r8ds    E DS
005200110905     D trul52ds      E DS
005300110729     D fnartds       e ds                  extname(Fnart00F) INZ
005400070130     D DSbrv         e ds                  extname(Fnbrv00F) INZ
005500070130     D DSbrvc        e ds                  extname(fnbrv00F) PREFIX(c_) INZ
005600040503     D DSfvv         e ds                  extname(fnfvv00F) INZ
005700040503     D DSfvvc        e ds                  extname(fnfvv00F) prefix(c_) INZ
005800101125     D wfnlrq1ds     e ds                  extname(wfnlrq10F)
005900101125     D wfnlrq2ds     e ds                  extname(wfnlrq20F)
006000101125     D dsqa          e ds
006100150930     D Dar5emd       E DS
006200150930     D inviato_contro_alert...
006300150930     d                 s              1    inz('N')
006400941109     D*
006500030721     D ddatiute      e ds
006600030721     D azuteds       e ds                  extname(AZUTE00F)
006700030721     D tibs34ds      E DS                  inz
006800920427     D KPJBA         E DS
006900040419     D DTAD          E DS
007000040419     D  DDESC        E                     EXTFLD(§TADDESC)
007100040419     D  DRAGR        E                     EXTFLD(§TADRAGR)
007200040419     D  DCDNC        E                     EXTFLD(§TADCDNC)
007300040419     D  DCTLT        E                     EXTFLD(§TADCTLT)
007400040419     D  DCTLS        E                     EXTFLD(§TADCTLS)
007500040419     D  DDAGG        E                     EXTFLD(§TADDAGG)
007600040419     D  DMTVD        E                     EXTFLD(§TADMTVD)
007700040419     D  DDCDN        E                     EXTFLD(§TADDCDN)
007800040419     D  DTICO        E                     EXTFLD(§TADTICO)
007900040419     D  DFDDT        E                     EXTFLD(§TADFDDT)
008000040419     D  DFMED        E                     EXTFLD(§TADFMED)
008100040419     D  DPZDO        E                     EXTFLD(§TADPZDO)
008200040419     D  DKGDO        E                     EXTFLD(§TADKGDO)
008300040419     D  DEVFO        E                     EXTFLD(§TADEVFO)
008400040419     D  DIMPR        E                     EXTFLD(§TADIMPR)
008500040419     D  DTISI        E                     EXTFLD(§TADTISI)
008600040419     D  DDEST        E                     EXTFLD(§TADDEST)
008700040419     D  DRITR        E                     EXTFLD(§TADRITR)
008800040419     D  DDESI        E                     EXTFLD(§TADDESI)
008900040419     D  DCARE        E                     EXTFLD(§TADCARE)
009000040419     D  DCIDD        E                     EXTFLD(§TADCIDD)
009100021029      * TRUL06DS serve per call a TRUL06R
009200021029     d Trul06Ds      e ds
009300021029     d  L1                     1     90  0 Dim(30)
009400030723     D CMAT            C                   CONST('MATTINO   ')
009500030723     D CPOM            C                   CONST('POMERIGGIO')
009600040419     D TAD             S              2    DIM(100)
009700050505     D PAR             S              2    DIM(50)
009800141120     D PARpin          S              2    DIM(50)
009900040419     D DTA             S            256    DIM(100)
010000040421     D TPC             S             10    DIM(2) CTDATA PERRCD(1)
010100930426     C*****************************************************************
010200930426     C* RIEPILOGO INDICATORI
010300930426     C*****************************************************************
010400970327     C* 01    - STAMPA SCRITTA - NON SPUNTATO IN DATA XX/XX/XXXX
010500970327     C* 02    - STAMPA SCRITTA - SPUNTATO SU ALTRA DISTINTA N.  DEL
010600950214     C* 06    - STAMPA SCRITTA - SPUNTATO SU F.  X-XXXXX DEL XX/XX
010700920422     C* 07    - ON  CAMBIATA SPEDIZIONE
010800920422     C* 08    - ON  ESISTE SERIE
010900920423     C* 09    - SEGNACOLLI NON SEQUENZIALI
011000920424     C* 20    - STAMPO TESTATA
011100911018     C*---------------------------------------------------------------*
011200030723     C* ROUTINE INIZIALE
011300030723     C                   EXSR      DEFCAM
011400920909     C*
011500020911     C     Karb78        SETLL     FNARB78L
011600030723    0C                   DO        *hival
011700030723     C     Karb78        READE     FNARB78L
011800920417     C*
011900030723     c                   if        %eof(fnarb78l)
012000030723     c                   leave
012100030723     c                   end
012200141120     C*
012300930308     C* ESCLUDO I SOLI INCASSO SE BOLLA E' NELLO STORICO
012400941109     C*  HANNO GIA' LA DATA CONSEGNA MERCE
012500050505     C     ARBDCM        IFne      0
012600030723     c                   iter
012700030723     c                   end
012800151007     ***-----
012900151007ab    *   Controlla se per la spedizione è stato inviato un CONTRO ALERT
013000151007ab    **    x avvisare di non poter arrivare a consegnare
013100151007ab   c                   exsr      contro_alert
013200151007     ***-----
013300110729     c* verifico se perfect order
013400130221      *** Wurth adesso manda nel campo non solo numeri per identificare altre cose
013500130221      ***  quindi si deve sostituire il test per identificare il Perfect Order
013600130221     c************       if        arbxco <> ' '
013700130221     c                   if        arbxco > '0'
013800141120     C*
013900110729     c                   clear                   fnlrf7r8ds
014000110729     c                   eval      dr8aasi= arbaas
014100110729     c                   eval      dr8lnpi= arblnp
014200110729     c                   eval      dr8nrsi= arbnrs
014300110729     c                   eval      dr8nspi= arbnsp
014400110729     c                   eval      dr8rmai= arbrma
014500110729     c                   eval      dr8lnai= arblna
014600110905     c                   eval      dr8xcoi= arbxco
014700110729     c                   move      arbifp        dr8fgsi
014800110729     c                   move      arbndc        dr8ndci
014900110729     c                   eval      kpjbu = fnlrf7r8ds
015000110729     c                   call      'FNLRF7R8'
015100110729     c                   parm                    kpjba
015200110802     c                   movel     kpjbu         fnlrf7r8ds
015300141120     C*
015400110905     c                   if        dr8erro = '007' or dr8erro = '009'
015500110905     c                   clear                   trul52ds
015600141120     C*
015700110905     c                   if        arbdti <> 0
015800110905     C                   Z-ADD     §XCODDTI      D52GIO                         *
015900110905     C                   MOVE      arbdti        D52DIN                         *
016000110905     c                   else
016100110905     C                   Z-ADD     §XCODdsp      D52GIO                         *
016200110905     C                   movel     arbAAS        d52din
016300110905     C                   move      arbmgs        d52din
016400110905     c                   end
016500141120      ***
016600110905     C                   CALL      'TRUL52R'
016700110905     C                   PARM                    TRUL52ds
016800110905     C                   MOVE      D52DFI        DAT1GG            8 0          *
016900110905     c                   end
017000141120      ***
017100110905     c* non emetto errore di sorelle non in distinta o di record bolla
017200110905     c* mancanti se trascorsi i tempi di attesa
017300110905     c                   if        DAT1GG<=D59DDT
017400110905     c                   clear                   dr8erro
017500110905     c                   end
017600141120      ***
017700110905     c                   if        dr8erro<> ' '
017800110729     c                   clear                   fnartds
017900110729     C                   SETOFF                                       010206
018000110729     C                   SETOFF                                       030410
018100110729     C                   SETOn                                        11
018200110729     c                   exsr      stampa
018300110802     C                   SETOff                                       11
018400110905     c*                  iter
018500110729     c                   end
018600141120      ***
018700110729     c                   end
018800141120      ***
018900110729     c                   clear                   parzprfo          1
019000141120      ***
019100130221      *** Wurth adesso manda nel campo non solo numeri per identificare altre cose
019200130221      ***  quindi si deve sostituire il test per identificare il Perfect Order
019300130221     c*****arbxco        comp      ' '                                4141
019400130221     c     arbxco        comp      '0'                                41
019500141120      *
019600141120ab   C* controllo particolarità consegne =  se HA il PINCODE
019700141121ab   c                   clear                   SegnalaErrPCDE    1
019800050505     C* controllo particolarità consegne =  NO PARZ.
019900050505     c                   clear                   parz              1
020000050505     c                   if        arbgma <> ' '
020100050505     C                   Z-ADD     1             zz                4 0
020200050505     C     arbgma        LOOKUP    par(zz)                                40
020300141120ab    **  Controlla se bolla con PINCODE in consegna = 48(ON)
020400141120ab   C                   Z-ADD     1             pn                4 0
020500141121ab   C     arbgma        LOOKUP    parPIN(pn)                             48
020600141121ab    **  Se abilitato il PINCODE per la Consegna Particolare e la
020700141121ab    **  Distinta non ha il PDA oppure è in test x la Consegna
020800141121ab    **    Deve segnalare l'errore da gestire
020900141121ab   c                   if        %Equal and DST_PDA_noTST <> 'S'
021000141121ab   c                   eval        SegnalaErrPCDE = 'S'
021100141121ab   c                   end
021200050505     c                   else
021300141120     c                   setoff                                       40
021400050505     c                   end
021500930308     C*
021600050505     C                   SETOFF                                         12
021700941109     C     KART          SETLL     FNART01L                               33
021800050505    1C     *IN33         IFEQ      '0'
021900941109     C     KART          CHAIN     FNLBL000                           34
022000920909     C* BOLLA FIGLIA LEGATA A BOLLA MAMMA
022100050505    2C     *IN34         IFEQ      '0'
022200920622     C                   SETON                                        12
022300941109     C     KLBL2         SETLL     FNART01L
022400050505    3c                   do        *hival
022500030723     C     KLBL2         READE     FNART01L
022600050505     C                   if        %eof(fnart01l)
022700030723     c                   leave
022800030723     c                   end
022900941109     C* CONTROLLO SEGNACOLLO MANCANTE
023000941109     C                   EXSR      CTRART
023100050505    3C                   ENDDO
023200050505    2C                   END
023300920622     C*
023400050505   X1C                   ELSE
023500920909     C* SE E' MAMMA NON CONSIDERO TANTO CI SARA' LA FIGLIA
023600941109     C     KART          SETLL     FNLBL002                               31
023700050505    2C     *IN31         IFEQ      '0'
023800941109     C     KART          READE     FNART01L                               31
023900050505    3C     *IN31         DOWEQ     '0'
024000941109     C* CONTROLLO SE SEGNACOLLO MANCANTE
024100941109     C                   EXSR      CTRART
024200941109     C     KART          READE     FNART01L                               31
024300050505    3C                   ENDDO
024400050505    2C                   END
024500050505    1C                   END
024600151007     c*
024700151007     c* manca spunta 4 e ho part. consegna = NO PARZ. oppure perfect order
024800151007     c* oppure pincode e distinta no pda oppure contralert
024900110729     c                   if        parz = '1' or parzprfo = '1'
025000141120ab   c                                or
025100141120ab   c                               SegnalaErrPCDE = 'S'
025200151007ab   c                                or
025300151007ab   C                             inviato_contro_alert = 'S'
025400050505     C                   SETOFF                                       010206
025500050505     C                   SETOFF                                       0304
025600151007     c* ind10 non stampa il segnacollo, ind 11 stampa descrizione anomalia
025700151007     c                   seton                                        1110
025800151007     c* in ordine per quello che prevale in stampa
025900151007     ***  contro Alert
026000151007     c                   Select
026100151007ab   C                   when      inviato_contro_alert = 'S'
026200151007ab   c                   eval      dr8erro='012'
026300151007     ***  perfect order
026400151007     c                   when      parzprfo = '1'
026500151007     c                   eval      dr8erro='010'
026600151007     ***  parziale
026700151007     c                   when      parz = '1'
026800151007     c                   eval      dr8erro='006'
026900151007ab   c*** pin code
027000151007ab   c                   when          SegnalaErrPCDE = 'S'
027100151007ab   c                   eval      dr8erro='011'
027200151007ab   c                   endSL
027300151007     ***
027400050505     c                   exsr      stampa
027500110829     c                   setoff                                       1011
027600090224     c* scrivo il record di errore nel wf
027700110729     c                   if        parz = '1'
027800141121ab   c                               or
027900141121ab   c                             SegnalaErrPCDE = 'S'
028000151007ab   c                                or
028100151007ab   C                             inviato_contro_alert = 'S'
028200141120      *---
028300090224     c                   clear                   comnsc
028400090303     c     kwf7          chain     fnlrf71l
028500141120      *---
028600090226     c                   MOVEL     OGGI          wf7tim
028700090226     c                   MOVE      TIMES         wf7tim
028800090224     c                   clear                   wf7err
028900090224     c                   clear                   wf7nsc
029000090224     c                   clear                   wf7can
029100090224     c                   clear                   wf7fls
029200090224     c                   eval      WF7AAS = ARBAAS
029300090224     c                   eval      WF7LNP = ARBLNP
029400090224     c                   eval      WF7NRS = ARBNRS
029500090224     c                   eval      WF7NSP = ARBNSP
029600090224     c                   eval      WF7lna = ARBlna
029700090224     c                   eval      WF7FGS = d59fgs
029800090224     c                   eval      WF7DDC = d59ddt
029900090224     c                   eval      WF7NFV = wnfv
030000090224     c                   eval      WF7pdc = d59pdr
030100151007     c                   select
030200151007ab   C                   when      inviato_contro_alert = 'S'
030300151007     c                   eval      WF7not = 'INVIATO ALERT VAR.DATA CONSEGNA '
030400151007ab   c                   when      parz = '1'
030500090224     c                   eval      WF7not = 'NON EFFETTUARE CONSEGNA PARZIALE'
030600151007ab   c                   when      SegnalaErrPCDE = 'S'
030700141121ab   c                   eval      WF7not = 'PINCODE: DISTINTA NO PDA O IN TEST'
030800151007ab   c                   endsl
030900141120      *---
031000090303     c                   if        not %found(fnlrf71l)
031100090224     c                   write     wfnlrf00
031200090224     c                   else
031300090224     c                   update    wfnlrf00
031400110729     c                   end
031500141120      *---
031600090224     c                   end
031700050505     c                   end
031800930308     C*
031900941109    0C                   ENDDO
032000010613     C*
032100030723     c                   movel(p)  FNLR59DS      kpjbu
032200030723     C                   SETON                                        RT
032300030805     C**********************************************************************
032400030805     C* CONTROLLO SE SEGNACOLLO MANCANTE-----------------------------*
032500030805     C**********************************************************************
032600030805     C     CTRART        BEGSR
032700030801     C*
032800030801     C* ELABORO SOLO I COLLI NON CONSEGNATI ED ARRIVATI
032900030801    1C     ARTDCM        IFEQ      0
033000030801     C                   SETOFF                                       010206
033100040420     C                   SETOFF                                       0304
033200030801     C*
033300030801     C                   Z-ADD     4             WNPG
033400070130     C     KBRV          CHAIN     FNBRV01L                           36
033500030801     C* SPUNTA NON TROVATA
033600030801    2C     *IN36         IFEQ      *ON
033700080507     c* aggiorno il file del dettaglio segnacolli dei dati a pda
033800090223     c     kpdd          chain(n)  fipdd01t
033900141120      ***
034000080507     c                   if        %found(fipdd01t) and PDDSPUMAG ='S'
034100080507     c                             and pddatb = ' '
034200141120      ***
034300090223     c     kpdd          chain     fipdd01t
034400090223     c                   if        %found(fipdd01t)
034500080508     c                   clear                   PDDSPUMAG
034600080508     c                   eval      tmp= %char(%timestamp: *iso0)
034700080508     c                   eval      PDdDATORA =   tmp
034800080507     c                   update    fipdd000
034900080507     c                   end
035000090223     c                   end
035100030801     C*
035200040420     C* VEDO SE E' PRESENTE UNA SPUNTA SU UN'ALTRA DISTINTA del mio terminal
035300070130     C     KBRV2         CHAIN     FNBRV09L                           36
035400030801     C*
035500030801     C* COLLO NON PRESENTE IN NESSUNA DISTINTA
035600030801    3C     *IN36         IFEQ      *ON
035700030822     C                   SETON                                        01
035800030801   X3C                   ELSE
035900071204     C* SE DISTINTA CHIUSA o aperta e vecchia COME SE NON FOSSE PRESENTE
036000030801     C                   Z-ADD     BRVNFV        WNFV5
036100030801     C     KFVV2         CHAIN     FNFVV000                           31
036200030801    4C     *IN31         IFEQ      *ON
036300030801     C     FVVFCF        OREQ      'S'
036400071204     C     FVVFCF        OREQ      ' '
036500071204     c     fvvdfv        andne     d59ddt
036600030801     C                   SETON                                        01
036700030801   X4C                   ELSE
036800040712     c*segnalo solo se il po della spunta è diverso da quello della distinta
036900071128     c* che sto quadrando o se il foglio non è della data che sto quadrando.
037000071128     c* se fossero uguali ho già segnalato l'anomalia nel
037100040712     c* FNLRF7R2
037200071128     c                   if        d59fgs <> brvfgs
037300040712     C                   SETON                                        02
037400030801    4C                   ENDIF
037500040712    4C                   ENDIF
037600030801    3C                   ENDIF
037700141120     ****
037800040419     c* Se non spuntato in nessun foglio 4 verifico se ho un danno
037900040419    3C     *IN01         IFEQ      *ON
038000040419     C                   EXSR      TESDAN
038100040420     c* Se non spuntato in nessun foglio 4 e no c.a.
038200040419     c                   if        wdavm = 'N'
038300040420     c* se no data arrivo merce e no data foglio partenza stampo anomalia
038400040420     c* collo non spuntato in uscita non partito e non arrivato
038500040420     c                   if        artdam = 0 and artdfv = 0
038600040420     c                   seton                                        04
038700040420     c                   setoff                                       01
038800040420     c                   else
038900040420     c* cerco l'ultima spunta
039000030801     C                   EXSR      CHABRV
039100040420     c                   end
039200141120      ****
039300050505     c* se non ho la spunta 4 e ho la particolarità consegna = NO PARZIALI
039400090304     c* segnalo l'anomalia in stampa se bolla con + colli
039500110729     c                   if        arbncl > 1
039600110729     c                   if        *in40
039700110729     c                   eval      parz = '1'
039800050505     c                   end
039900141120      ****
040000110729     c                   if        *in41
040100110729     c                   eval      parzprfo = '1'
040200110729     c                   end
040300110729     c                   end
040400141120      ****
040500040419     c                   else
040600040420     c* Se non spuntato in nessun foglio 4 e ho c.a. stampo il segnacollo
040700040420     c* con danno
040800040419     c                   seton                                        03
040900040420     c                   setoff                                       01
041000040419     c                   end
041100141120      ****
041200040419     C                   ENDIF
041300040420     C*                  SETOFF                                       01
041400030801     C*
041500040420     c                   if        *in01 or *in02 or *in03 or *in04 or *in06
041600030801     C                   EXSR      STAMPA
041700030825     c                   end
041800030801     C*
041900080507     c                   else
042000080507     c* se spunta 4 aggiorno il file del dettaglio segnacolli dei dati a pda
042100090223     c     kpdd          chain(n)  fipdd01t
042200090224     c                   if        %found(fipdd01t)
042300080507     c                             and pddatb = ' '
042400090224     c                   if        PDDSPUMAG <>'S' or
042500090224     c                             (PDDSPUMAG ='S' and pddcan <> brvcan)
042600090223     c     kpdd          chain     fipdd01t
042700090223     c                   if        %found(fipdd01t)
042800080507     c                   eval      PDDSPUMAG ='S'
042900090224     c                   eval      PDDcan = brvcan
043000080508     c                   eval      tmp= %char(%timestamp: *iso0)
043100080508     c                   eval      PDdDATORA =   tmp
043200080507     c                   update    fipdd000
043300080507     c                   end
043400090223     c                   end
043500090224     c                   end
043600090224     c* se spunta 4 con anomalia e la bolla ha part. cons. NO PARZIALI
043700090224     c* memorizzo il file di work per controlli filiali
043800090224     c                   if        brvcan <> ' ' and *in40
043900090224     c                   eval      comnsc = artnsc
044000090303     c     kwf7          chain     fnlrf71l
044100090226     c                   MOVEL     OGGI          wf7tim
044200090226     c                   MOVE      TIMES         wf7tim
044300090224     c                   clear                   wf7err
044400090224     c                   eval      wf7fls = artfls
044500090224     c                   eval      wf7nsc = artnsc
044600090224     c                   eval      wf7can = brvcan
044700090224     c                   eval      WF7AAS = ARBAAS
044800090224     c                   eval      WF7LNP = ARBLNP
044900090224     c                   eval      WF7NRS = ARBNRS
045000090224     c                   eval      WF7NSP = ARBNSP
045100090224     c                   eval      WF7lna = ARBlna
045200090224     c                   eval      WF7FGS = d59fgs
045300090224     c                   eval      WF7DDC = d59ddt
045400090224     c                   eval      WF7NFV = wnfv
045500090224     c                   eval      WF7pdc = d59pdr
045600090303     c                   eval      WF7not = 'PART.CONSEGNA PARZIALE: COLLI CON -
045700090303     c                             ANOMALIE'
045800090303     c                   if        NOT %found(fnlrf71l)
045900090224     c                   write     wfnlrf00
046000090226     C                   ELSE
046100090226     c                   UPDATE    wfnlrf00
046200090224     c                   end
046300090224     c                   end
046400090224     c*
046500030801    2C                   END
046600030801    1C                   END
046700030801     C                   ENDSR
046800030801     C**********************************************************************
046900030801     C* CHAIN BRV CON CATEGORIA NON 4 -------------------------------*
047000030801     C**********************************************************************
047100030801     C     CHABRV        BEGSR
047200040409     c                   clear                   dsbrvc
047300040422     c                   clear                   dsfvvc
047400040422     c                   do        6             xx                2 0
047500040420     c                   move      xx            wnpg
047600040409     C*
047700070130     C     KBRV3         SETll     FNBRV09L
047800040409     c                   do        *hival
047900070130     C     KBRV3         READe     FNBRV09L
048000070130     c                   if        %eof(fnbrv09l)
048100040409     c                   leave
048200040409     c                   end
048300090121     c* nella ricerca considero solo le spunte reali cioè quelle con
048400090121     c* data caricamento impostata
048500090121     c                   if        BRVDCS = 0
048600090121     c                   iter
048700090121     c                   end
048800040419     c*
048900040422     c                   clear                   dsfvv
049000040409     C                   z-add     BRVNFV        WNFV5
049100040420     C     KFVV2         CHAIN     FNFVV01L
049200040423     C                   if        not %found(fnfvv01l)
049300040526     c                   z-add     brvfgs        fvvfgs
049400040526     c                   z-add     brvnpg        fvvnpg
049500040526     c                   if        brvnfv < 100000
049600040526     c                   z-add     brvnfv        fvvnfv
049700040422     c                   end
049800040526     c                   end
049900040419     c*
050000040419     c                   if        brvdfs > c_brvdfs or
050100040419     c                             (brvhms > c_brvhms and brvdfs = c_brvdfs)
050200040419     c                   eval      dsbrvc = dsbrv
050300040419     c                   eval      dsfvvc = dsfvv
050400040420     C                   SETON                                        06
050500040419     C                   SETOFF                                       01
050600040409     c                   end
050700040420     C                   ENDDO
050800040409     C                   ENDDO
050900040409     c*
051000040409     c                   eval      dsbrv = dsbrvc
051100040409     c                   eval      dsfvv = dsfvvc
051200030801     C                   ENDSR
051300030707     C**********************************************************************
051400920421     C* STAMPO ERRORE -----------------------------------------------*
051500030707     C**********************************************************************
051600900621     C     STAMPA        BEGSR
051700141120      **
051800030723     c                   movel     'S'           d59ANM
051900911015     C* STAMPA TESTATA
052000030728     C   80              DO
052100030723     C                   ADD       1             D59PAG
052200920422     C                   SETON                                        07
052300101201     c                   if        d59sqa = ' ' or
052400101201     c                             d59sqa = 'T'
052500030728     C                   WRITE     TESTA
052600141121      *
052700050502     c                   if        d59lsr = 'S' and d59pag = 1
052800050422     C                   write     testal
052900050426     c                   end
053000141121      *
053100030729     C                   WRITE     TESTA1
053200101125     C                   END
053300141121      *
053400030728     C                   Z-ADD     6             D59LIN
053500030728     C                   SETOFF                                       80
053600911018     C                   END
053700920422     C*
053800911018     C                   Z-ADD     ARBAAS        COMAAS
053900911018     C     SAVSPE        IFNE      ARBSPE
054000920422     C                   SETON                                        07
054100911018     C                   MOVEL     ARBSPE        SAVSPE
054200911018     C                   END
054300911018     C* DATA SPEDIZIONE
054400920422     C   07              DO
054500911018     C                   MOVE      ARBDSP        G02INV
054600911018     C                   MOVE      *ZEROS        G02DAT
054700911018     C                   MOVEL     3             G02ERR
054800941109     C                   CALL      'XSRDA8'
054900911018     C                   PARM                    WLBDAT
055000920422     C                   MOVEL     G02DAT        STADSP            4 0
055100920619     C* RAGIONE SOCIALE DESTINATARIO
055200040419     C                   MOVEL     ARBRSD        STARSD           22
055300920423     C* SEGNACOLLI NON CONSECUTIVI
055400920423     C     ARBFNS        COMP      'S'                                    09
055500920421     C* SERIE >< 0
055600920422     C     ARBNRS        COMP      0                                  0808
055700911018     C                   END
055800141121      *
055900030801     C* serie e segnacollo
056000030801     c                   z-add     artnrs        st2nrs            2 0
056100030801     c                   z-add     artnsc        st2nsc            7 0
056200141120      *---
056300920622     C* 02 ON - MANCANTE SU ALTRA DISTINTA SPUNTATO - PRENDO DATA
056400941110     C     *IN02         IFEQ      *ON
056500950214     C     *IN06         OREQ      *ON
056600040712     C                   Z-ADD     BRVfgs        ST2fgs
056700040712     C                   Z-ADD     BRVNFV        ST2NFV
056800920506     C                   MOVE      FVVDFV        G02INV
056900920506     C                   MOVE      *ZEROS        G02DAT
057000920506     C                   MOVEL     3             G02ERR
057100941109     C                   CALL      'XSRDA8'
057200920506     C                   PARM                    WLBDAT
057300920506     C                   MOVEL     G02DAT        STADDC            4 0
057400030728     C                   MOVEL     G02DAT        STADDC1           4 0
057500920506     C                   END
057600141120      *---
057700070130     c                   if        *in06
057800070130     c                   movel     brvhms        brvhms_p
057900070130     c                   endif
058000141120      *---
058100141120      *-  cerca descrizione da tabella SQA
058200141121     c                   exsr      Cerca_su_SQA
058300141120      *---
058400900622     C* STAMPO DETTAGLIO
058500101201     c                   if        d59sqa = ' ' or
058600101201     c                             d59sqa = 'T'
058700030728     C                   WRITE     DET
058800101201     c                   end
058900101125     c* video
059000101201     c                   if        d59sqa = 'S' or
059100101201     c                             d59sqa = 'T'
059200101125     c                   exsr      srsqa
059300101125     c                   end
059400141120      *-
059500030724     C                   ADD       1             D59LIN
059600920422     C                   SETOFF                                       07
059700900622     C*
059800900622     C                   ENDSR
059900141120     C*-----------------------------------------------------*
060000141120     C* cerca descrizione in tabella  stampata con 11=ON    *
060100141120     C*-----------------------------------------------------*
060200141121     C     Cerca_su_SQA  BEGSR
060300141121      *
060400141121     c                   clear                   dessqa
060500141121      *
060600141121      *  imposta la Segnalazione da Tabella sul dettaglio
060700141120     c                   if        *in11
060800141120     c                   eval      tbeke2 = dr8erro
060900141120     c                   eval      tbecod = 'SQA'
061000141120     c                   eval      tbeke1 = 'FNLRF7R4'
061100141120     c     ktbe          chain     tntbe01l
061200141120     c                   if        %found(tntbe01l)
061300141120     c                   movel     tbeuni        dsqa
061400141120     c                   movel     d§sqades      dessqa
061500141120     c                   else
061600141120     c                   clear                   dsqa
061700141120     c                   endif
061800141120     c                   endif
061900141120     C*
062000141120     C                   ENDSR
062100101125     C*-----------------------------------------------------*
062200101125     C* memorizzo l'errore nel file di work                 *
062300101125     C*-----------------------------------------------------*
062400101125     C     srsqa         BEGSR
062500101125     c                   clear                   dsqa
062600101125     c                   eval      tbecod = 'SQA'
062700101125     c                   eval      tbeke1 = 'FNLRF7R4'
062800101125     c                   select
062900101125     c* no spunta 4
063000101125     c                   when      *in01
063100101125     c                   eval      tbeke2 = '001'
063200101125     c* altra distinta
063300101125     c                   when      *in02
063400101125     c                   eval      tbeke2 = '002'
063500101125     c* ultima spunta ...
063600101125     c                   when      *in06
063700101125     c                   eval      tbeke2 = '003'
063800101125     c* c.a
063900101125     c                   when      *in03
064000101125     c                   eval      tbeke2 = '004'
064100101125     c* nessuna spunta
064200101125     c                   when      *in04
064300101125     c                   eval      tbeke2 = '005'
064400110729     c* parziale CP e perfect order
064500110729     c                   when      *in10
064600110729     c                   if        parz='1'
064700101125     c                   eval      tbeke2 = '006'
064800110729     c                   end
064900110729     c                   if        parzprfo='1'
065000110729     c                   eval      tbeke2 = '010'
065100110729     c                   end
065200110729     c* perfect order
065300141121ab   c*  o
065400141121ab   c* PINcode non congruente con la DIstinta
065500110729     c                   when      *in11
065600110729     c                   eval      tbeke2 = dr8erro
065700101125     c                   endsl
065800141121      **
065900101125     c     ktbe          chain     tntbe01l
066000101125     c                   if        %found(tntbe01l)
066100101125     c                   movel     tbeuni        dsqa
066200101125     c                   if        D§SQAELA = 'S'
066300101125     c* TESTATA
066400101125     c                   clear                   wfnlrq1ds
066500101125     c                   eval      wlrq1ndc = d59ndt
066600101125     c                   eval      wlrq1fgs = d59fgs
066700101125     c                   eval      wlrq1pdr = d59pdr
066800101203     c                   eval      wlrq1aas = arbAAS
066900101203     c                   eval      wlrq1LNP = arbLNP
067000101203     c                   eval      wlrq1NRS = arbNRS
067100101203     c                   eval      wlrq1NSP = arbNSP
067200101125     c     kwf10         chain     wfnlrq11l
067300101125     c                   if        not %found(wfnlrq11l)
067400101125     c                   movel     tbeke2        wlrq1cer
067500101125     c                   eval      wlrq1sce = D§SQASCE
067600101125     c                   eval      wlrq1err = d§sqades
067700101125     c                   write     wfnlrq10
067800101125     c                   else
067900101125     c                   eval      wlrq1err = 'Errore dettaglio colli'
068000101125     c                   clear                   wlrq1cer
068100101125     c                   clear                   wlrq1sce
068200101125     c                   update    wfnlrq10
068300101125     c                   end
068400101125     c* dettaglio
068500101125     c                   clear                   wfnlrq2ds
068600101125     c* se ho i rif. della spedizione li memorizzo altrimenti 0
068700101125     c                   eval      wlrq2aas = wlrq1aas
068800101125     c                   eval      wlrq2lnp = wlrq1lnp
068900101125     c                   eval      wlrq2nrs = wlrq1nrs
069000101125     c                   eval      wlrq2nsp = wlrq1nsp
069100101125     c                   eval      wlrq2ndc = wlrq1ndc
069200101125     c                   eval      wlrq2fgs = wlrq1fgs
069300101125     c                   eval      wlrq2pdr = wlrq1pdr
069400101125     c                   movel     tbeke2        wlrq2cer
069500101125     c                   eval      wlrq2sce = D§SQASCE
069600101125     c                   eval      wlrq2err = d§sqades
069700110104     c  n10              eval      wlrq2fls = arbfls
069800110104     c  n10              eval      wlrq2nsc = st2nsc
069900110126     c  n10              eval      wlrq2nrc = st2nrs
070000110104     c   04              eval      wlrq2fls = artfls
070100110126     c   04              eval      wlrq2nrc = artnrs
070200110104     c   04              eval      wlrq2nsc = artnsc
070300101125     c                   select
070400101125     c                   when      tbeke2 = '002'
070500110104     c                   eval      wlrq2flv = st2fgs
070600101125     c                   eval      wlrq2nfv = st2nfv
070700110117     c                   eval      wlrq2dfv = fvvdfv
070800101126     c                   if        wlrq2nfv <> 0
070900101125     c                   eval      wlrq2npg = 4
071000101126     c                   end
071100101125     c                   when      tbeke2 = '003'
071200110104     c                   eval      wlrq2flv = fvvfgs
071300101125     c                   eval      wlrq2nfv = fvvnfv
071400110117     c                   eval      wlrq2dfv = fvvdfv
071500101125     c                   eval      wlrq2npg = fvvnpg
071600101126     c                   eval      wlrq2nps = brvnps
071700101126     c                   eval      wlrq2hms = brvhms
071800101125     c                   endsl
071900101125     c*
072000101125     c                   write     wfnlrq20
072100101125     c*
072200101125     c                   end
072300101125     c                   end
072400101125     C                   ENDSR
072500040420     C*--------------------------------------------------------------*
072600040420     C*  TESDAN: Test gestione danni
072700040420     C*--------------------------------------------------------------*
072800040420     C     TESDAN        BEGSR
072900040420     C*
073000040421     C                   MOVEL     *BLANKS       sTIPCA
073100040420     C*  Flag pratica danno = 'N'
073200040420     C                   MOVE      'N'           WDAVM             1
073300040420     C*  Se il collo non è stato spuntato su quel foglio di
073400040420     C*  uscita controllo se c'è una pratica C.A. aperta
073500040420     C     KDCD          CHAIN     FNDCD02L                           38
073600040420     C*  Se esiste C.A. aperta controllo il tipo di anomalia
073700040420     C     *IN38         IFEQ      '0'
073800040420     C     DCDATB        ANDNE     'A'
073900040420     C     DCDDCH        IFEQ      0
074000040420     C                   MOVE      TPC(1)        sTIPCA
074100040420     C* Aggancio testata
074200040420     C     KDCT          CHAIN     FNDCT01L                           38
074300040420     C     *IN38         DOWEQ     '0'
074400040420     C     DCTATB        ANDEQ     'A'
074500040420     C     *IN38         OREQ      '0'
074600040420     C     DCTDCH        ANDNE     0
074700040420     C     KDCT          READE     FNDCT01L                               38
074800040420     C                   END
074900040420     C                   Z-ADD     1             YY                4 0
075000040420     C     DCTTAD        LOOKUP    TAD(YY)                                39
075100040420     C     *IN39         IFEQ      '1'
075200040420     C                   MOVEL     DTA(YY)       DTAD
075300040420     C     DRAGR         IFEQ      'M'
075400040420     C     DRAGR         OREQ      'V'
075500040420     C                   MOVE      'S'           WDAVM
075600040420     C                   END                                                    Manc.o Ava D/R
075700040420     C                   END                                                    Trv TAD
075800040420     C*
075900040420     C                   ELSE
076000040420     C                   MOVE      TPC(2)        sTIPCA
076100040420     C                   END                                                    CA Aperta
076200040420     C                   END                                                    CA no annul
076300040420     C*
076400040420     C                   ENDSR
076500050505     C**********************************************************************
076600050505     C* OPERAZIONI INIZIALI------------------------------------------*
076700050505     C**********************************************************************
076800050505     C     DEFCAM        BEGSR
076900050505     C**
077000030801     C     *ENTRY        PLIST
077100030801     C                   PARM                    KPJBA
077200030801     C                   MOVEL     KPJBU         FNLR59DS
077300050204     C* IMPOSTO L'INDICATORE PER STAMPARE IL TITOLO IN BASE AL PARAMETRO
077400050204     C* TIPO STAMPA A=CONTROLLO AUTOTRASPORTATORE " "/O= DISTINTA DI USCITA
077500050204     C* AUTOTRASPORTATORE
077600050204     C                   IF        D59STP = 'A'
077700050204     C                   SETON                                        61
077800050204     C                   SETOFF                                       62
077900050204     C                   ELSE
078000050204     C                   SETOFF                                       61
078100050204     C                   SETON                                        62
078200050204     C                   END
078300040930     c*
078400040930     c                   movel(p)  'FNLRF7R'     nompgm           10
078500040930     c*
078600030801     C                   IF        D59TPR = 'U'
078700030801     C                   SETON                                        LR
078800030801     C                   RETURN
078900030801     C                   END
079000030801     C                   Z-ADD     D59NDT        WNFV
079100030801     C                   Z-ADD     D59NDT        WNFV5
079200030801     C                   Z-ADD     4             WNPG
079300030801     C                   MOVEL     CMAT          TPPRE
079400030801     C     D59FPP        IFEQ      'P'
079500030801     C                   MOVEL     CPOM          TPPRE            10
079600030801     C                   END
079700030801     C**
079800030801     C* IMPOSTO CAMPI FISSI
079900030801     C**
080000030801     c                   Z-Add     D59fgs        Karbifp
080100030801     C                   Z-ADD     D59NDT        COMNDC            7 0
080200030801     C                   MOVEL     *ZEROS        SAVSPE           16
080300030801     c* campi testata
080400030801     C                   Z-ADD     D59pdr        prtpdr            7 0
080500030801     C                   movel     D59dsf        prtdsf           35
080600050422     C                   movel     D59dsf        prtdsf2
080700030801     c                   Z-Add     D59fgs        prtfgs
080800030801     C                   Z-ADD     D59NDT        prtndt            5 0
080900110209     C                   Z-ADD     D59NDT        prtndt3
081000110209     C*                  eval      prtndt2 = d59bcd
081100110209     C                   move      d59bcd        prtndt2
081200030801     C*
081300030801     C* CHAIN E DATA DELLA DISTINTA
081400030801     C     KFVV          CHAIN     FNFVV000                           32
081500030801     C*
081600030801     C     *IN32         IFEQ      '0'
081700030801     C     FVVDFV        ANDEQ     D59DDT
081800030801     C                   MOVE      FVVDFV        G02INV
081900030801     C                   MOVE      *ZEROS        G02DAT
082000030801     C                   MOVEL     3             G02ERR
082100030801     C                   CALL      'XSRDA8'
082200030801     C                   PARM                    WLBDAT
082300030801     C                   Z-ADD     G02DAT        STcddc            8 0
082400030801     C                   END
082500030801     C*
082600030801     C                   IF        D59LIN = 0 OR D59LIN > 54
082700030801     C                   SETON                                        80
082800030801     C                   END
082900030801     C*
083000141120ab    *   Dalla Distinta verifica se ha il PDA e non è in TEST ?
083100141120ab   C                   eval        dstNPG = 4
083200141120ab   C                   eval        dstNFV = d59ndt
083300141120ab   C                   eval        dstFGS = d59fgs
083400141120ab   c                   clear                   DST_PDA_noTST     1
083500141120ab   C     Kdst          CHAIN     fidst01l
083600141120ab   C                   if        %Found(fidst01l)
083700141120ab   c                   eval      ddstFLR = dstFLR
083800141120ab    *
083900141120ab    * Controlla se la distinta è gestita con il PDA e se in fase di test
084000141120ab   c                   If         (dstPDA ='C' or dstPDA = 'E')
084100141120ab   c                                  and
084200141120ab   c                              (§dstTSTPDA <>'C' and §dstTSTPDA <>'E')
084300141120ab    *
084400141120ab   c                   eval      DST_PDA_noTST ='S'
084500141120ab    *
084600141120ab   c                   end
084700141120ab    *
084800141120ab   C                   end
084900141120     C*
085000030801     C                   ENDSR
085100030801     C**********************************************************************
085200030801     C* OPERAZIONI INIZIALI------------------------------------------*
085300030801     C**********************************************************************
085400030801     C     *INZSR        BEGSR
085500030801     c*
085600030801     C                   OPEN      FNLRF7P
085700030801     C**
085800030801     C* KLIST
085900030801     C**
086000101125     C     Ktbe          KLIST
086100101125     C                   KFLD                    tbecod
086200101125     C                   KFLD                    tbeke1
086300101125     C                   KFLD                    tbeke2
086400101125     C     Kwf10         KLIST
086500101125     C                   KFLD                    WLRQ1AAS
086600101125     C                   KFLD                    WLRQ1LNP
086700101125     C                   KFLD                    WLRQ1NRS
086800101125     C                   KFLD                    WLRQ1NSP
086900101125     C                   KFLD                    WLRQ1FGS
087000101125     C                   KFLD                    WLRQ1NDC
087100101125     C                   KFLD                    WLRQ1PDR
087200030801     C     KART          KLIST
087300030801     C                   KFLD                    ARBAAS
087400030801     C                   KFLD                    ARBLNP
087500030801     C                   KFLD                    ARBNRS
087600030801     C                   KFLD                    ARBNSP
087700080507     C     Kpdd          KLIST
087800080507     c                   Kfld                    d59fgs
087900080507     c                   Kfld                    wnfv
088000080507     C                   KFLD                    ARBAAS
088100080507     C                   KFLD                    ARBLNP
088200080507     C                   KFLD                    ARBNRS
088300080507     C                   KFLD                    ARBNSP
088400080507     C                   KFLD                    ARTNSC
088500090224     C     Kwf7          KLIST
088600090224     c                   Kfld                    d59fgs
088700090224     c                   Kfld                    wnfv
088800090224     c                   Kfld                    d59ddt
088900090224     C                   KFLD                    ARBAAS
089000090224     C                   KFLD                    ARBLNP
089100090224     C                   KFLD                    ARBNRS
089200090224     C                   KFLD                    ARBNSP
089300090224     C                   KFLD                    comnSC
089400030801     c     Karb78        Klist
089500030801     c                   Kfld                    Karbifp
089600030801     c                   Kfld                    Comndc
089700030801
089800030801     C     KBRV          KLIST
089900030801     C                   KFLD                    wnpg
090000030801     C                   KFLD                    WNFV
090100030801     C                   KFLD                    D59FGS
090200030801     C                   KFLD                    ARTFLS
090300030801     C                   KFLD                    ARTLNA
090400030801     C                   KFLD                    ARTNRS
090500030801     C                   KFLD                    ARTNSC
090600040420     C     KBRV2         KLIST
090700030801     C                   KFLD                    wnpg
090800030801     C                   KFLD                    ARTFLS
090900030801     C                   KFLD                    ARTLNA
091000030801     C                   KFLD                    ARTNRS
091100030801     C                   KFLD                    ARTNSC
091200040420     C                   KFLD                    SIMFEL
091300040420     C     KBRV3         KLIST
091400040420     C                   KFLD                    wnpg
091500040420     C                   KFLD                    ARTFLS
091600040420     C                   KFLD                    ARTLNA
091700040420     C                   KFLD                    ARTNRS
091800040420     C                   KFLD                    ARTNSC
091900141120ab   C     Kdst          KLIST
092000141120ab   C                   KFLD                    dstNPG
092100141120ab   C                   KFLD                    dstNFV
092200141120ab   C                   KFLD                    dstFGS
092300030805     C     KFVV          KLIST
092400030805     C                   KFLD                    WNPG
092500030805     C                   KFLD                    WNFV5
092600030805     C                   KFLD                    D59FGS
092700030801     C     KFVV2         KLIST
092800030801     C                   KFLD                    BRVNPG
092900030801     C                   KFLD                    WNFV5
093000030801     C                   KFLD                    BRVFGS
093100030801     C     KLBL2         KLIST
093200030801     C                   KFLD                    LBLAAP
093300030801     C                   KFLD                    LBLLPP
093400030801     C                   KFLD                    LBLNRP
093500030801     C                   KFLD                    LBLNSP
093600040419     C     KDCD          KLIST
093700040419     C                   KFLD                    ARTFLS
093800040419     C                   KFLD                    ARTLNA
093900040419     C                   KFLD                    ARTNRS
094000040419     C                   KFLD                    ARTNSC
094100040419     C     KDCT          KLIST
094200040419     C                   KFLD                    DCDAAC
094300040419     C                   KFLD                    DCDFIL
094400050505     C     Ktab          KLIST
094500050505     C                   KFLD                    tblkut
094600050505     C                   KFLD                    tblcod
094700150930      *
094800150930     C     Kfiar5        KLIST
094900150930     C                   KFLD                    arbAAS
095000150930     C                   KFLD                    arbLNP
095100150930     C                   KFLD                    arbNRS
095200150930     C                   KFLD                    arbNSP
095300150930     C                   KFLD                    TRDar5            3
095400030801     C**
095500030801     C     *LIKE         DEFINE    FVVNPG        WNPG
095600030801     C     *LIKE         DEFINE    BRVNFV        WNFV
095700030801     C     *LIKE         DEFINE    FVVNFV        WNFV5
095800030801     C     *LIKE         DEFINE    kpjbu         savkpjbu
095900030801     C**
096000030801     C                   MOVE      *ALL'-'       CMP132          198
096100030801     C                   MOVE      *ALL'-'       CMP198          198
096200030801     C*
096300030801     c     *dtaara       define    §azute        azuteds
096400030801     c     *dtaara       define    §datiute      ddatiute
096500030801     C                   in(E)     *dtaara
096600030801     C                   IF        %Error  or  RSUT = *blanks
096700030801     C                   call      'TIBS34R'
096800030801     C                   parm                    Tibs34Ds
096900030801     C                   in        *dtaara
097000030801     c                   ENDIF
097100030801     C*
097200030801     C                   TIME                    TIMES             6 0
097300030801     C                   TIME                    WHDAT            12 0
097400030801     C                   MOVE      WHDAT         WOGGI             6 0
097500030801     C                   TIME                    W0140            14 0
097600090226     C                   MOVE      W0140         G02DAT
097700090226     C                   MOVEL     *BLANKS       G02ERR
097800090226     C                   CALL      'XSRDA8'
097900090226     C                   PARM                    WLBDAT
098000090226     C                   MOVE      G02INV        OGGI              8 0
098100030801     C                   MOVE      W0140         WDTGIO            8 0
098200030801
098300030801      * Carico i p.o. del Terminal
098400030801     c                   eval      savkpjbu = kpjbu
098500030801     c                   Clear                   Trul06Ds
098600030801     c                   Eval      D06Cod = '£1'
098700030801     c                   Movel     Simfel        D06Key
098800030801     c                   Eval      D06Tla = 'L'
098900030801     c                   Movel(p)  Trul06Ds      Kpjbu
099000030801     c                   Call      'TRUL06R'
099100030801     c                   Parm                    Kpjba
099200030801     c                   Movel     Kpjbu         Trul06Ds
099300030801     c                   eval      kpjbu = savkpjbu
099400040419     C*  Carico dati tabella DTA anomalie danni
099500040419     C                   Z-ADD     0             X                 3 0
099600040420     C     'TAD'         setll     TNTBE01L
099700040420     C                   DO        *hival
099800040420     C     'TAD'         READE     TNTBE01L
099900040420     c                   if        %eof(tntbe01l)
100000040420     c                   leave
100100040420     c                   end
100200040419     C     TBESIF        IFEQ      KNSIF
100300040419     C     TBESIF        OREQ      *BLANKS
100400040419     C                   ADD       1             X
100500040419     C                   MOVEL     TBEKE1        TAD(X)
100600040419     C                   MOVEL     TBEUNI        DTA(X)
100700040420     c                   if        X = 100
100800040420     c                   leave
100900040420     c                   end
101000040420     c                   end
101100040420     C                   ENDdo
101200110905     c* reperisco i gg limite di attesa in caso di perfect order
101300110905     c                   eval      tbecod = 'XCO'
101400110905     c                   eval      tbeke1 = 'DTA'
101500110905     c                   clear                   tbeke2
101600110905     C     ktbe          chain     TNTBE01L
101700110905     c                   if        %found(tntbe01l)
101800110905     C                   MOVEL     TBeUNI        Dxcodta
101900110905     c                   else
102000110905     c                   clear                   dxcodta
102100110905     c                   end
102200050505     C*  Carico dati tabella PAR = partcolarità consegne NO PARZ.
102300141120ab   C*  Carico dati tabella PAR = partcolarità consegne con PINCODE
102400050505     C                   OPEN      TABEL00F
102500050505     C                   Z-ADD     0             X                 3 0
102600141120ab   C                   Z-ADD     0             Xpin              3 0
102700050505     C                   Z-ADD     1             tblkut
102800050505     C                   eval      tblcod = '7R'
102900050505     C     ktab          setll     Tabel00f
103000050505     C                   DO        *hival
103100050505     C     ktab          READE     TABEL00F
103200050505     c                   if        %eof(tabel00f)
103300050505     c                   leave
103400050505     c                   end
103500050505     C                   MOVEL     TBLUNI        DS7R
103600050505     c                   if        §7r8cp = 'N'
103700050505     C                   ADD       1             X
103800050505     C                   MOVEL     TBlkey        par(X)
103900050505     c                   end
104000141120      *
104100141120ab    * carica schiera con le Particolarità che hanno la gestione del PINCODE
104200141120ab   c                   if        §7RpinCODE = 'S'
104300141120ab   C                   ADD       1             Xpin
104400141120ab   C                   MOVEL     TBlkey        PARpin(Xpin)
104500141120ab   c                   end
104600141120      *
104700050505     C                   ENDdo
104800050505     C                   CLOSE     Tabel00f
104900030801     C*
105000030801     C                   ENDSR
105100150930     C*-----------------------------------------------------*
105200150930     C* controllo se mandato un CONTRO Alert al CLIENTE
105300150930     C*-----------------------------------------------------*
105400150930     C     contro_alert  BEGSR
105500150930      * rec.EMD   invio Alert e contro Alert
105600150930     C                   eval      inviato_contro_alert = 'N'
105700150930     C                   clear                   dar5emd
105800150930     C                   move      'EMD'         TRDar5
105900150930     C     Kfiar5        CHAIN     FiAR501L
106000150930     C                   IF        %Found(FiAR501L)
106100150930     C                   MOVEL     ar5uni        dar5emd
106200150930     c                   if        §AR5ICA ='S'
106300150930     c                   if        §AR5DPC <>*blank and §AR5DPC <>*zeros
106400150930     c                   move      §AR5DPC       data8DPC          8 0
106500150930     c                   if        data8DPC = oggi
106600150930     C                   eval      inviato_contro_alert = 'S'
106700150930     C                   end
106800150930     C                   end
106900150930     C                   end
107000150930     C                   endif
107100150930     C                   ENDSR
107200150930     C*-----------------------------------------------------*
107300040419**
107400040421C.A.aperta
107500040421C.A.chiusa
