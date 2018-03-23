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
001400941109     FFNLBL01L  IF   E           K DISK
001500941109     FFNLBL02L  IF   E           K DISK
001600941109     F                                     RENAME(FNLBL000:FNLBL002)
001700030728     FFNLRF7P   O    E             PRINTER OFLIND(*IN80)
001800941110     F                                     USROPN
001900040419     FFNDCD02L  IF   E           K DISK
002000040419     FFNDCT01L  IF   E           K DISK
002100101125     FTNTBE01L  IF   E           K DISK
002200050505     FTabel00f  IF   E           K DISK    USROPN
002300101125     Fwfnlrq11l uf a E           k DISK
002400101125     Fwfnlrq20f o    E             DISK
002500020911     d Karbifp         s                   Like(ArbIfp)
002600911018     D                 DS
002700941109     D  ARBAAS                 1      4  0
002800941109     D  ARBMGS                 5      8  0
002900941109     D  ARBDSP                 1      8  0
003000911018     D                 DS
003100941109     D  COMAAS                 1      4  0
003200941109     D  ARBLNP                 5      7  0
003300941109     D  ARBNRS                 8      9  0
003400941109     D  ARBNSP                10     16  0
003500941109     D  ARBSPE                 1     16
003600941109     D***
003700941109     D* PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
003800941109     D***
003900941109     D WLBDAT          DS                  INZ
004000941109     D  G02DAT                 1      8  0
004100941109     D  G02INV                 9     16  0
004200941109     D  G02ERR                17     17
004300941109     D  G02TGI                18     22  0
004400080508     d tmp             s             17
004500090224     d comnsc          s                   like(artnsc)
004600030723     D FNLR59DS      E DS
004700110905     d dxcodta       e ds
004800050505     D ds7r          E DS
004900141120ab   D DdstFLR       E DS                  INZ
005000110729     D fnlrf7r8ds    E DS
005100110905     D trul52ds      E DS
005200110729     D fnartds       e ds                  extname(Fnart00F) INZ
005300070130     D DSbrv         e ds                  extname(Fnbrv00F) INZ
005400070130     D DSbrvc        e ds                  extname(fnbrv00F) PREFIX(c_) INZ
005500040503     D DSfvv         e ds                  extname(fnfvv00F) INZ
005600040503     D DSfvvc        e ds                  extname(fnfvv00F) prefix(c_) INZ
005700101125     D wfnlrq1ds     e ds                  extname(wfnlrq10F)
005800101125     D wfnlrq2ds     e ds                  extname(wfnlrq20F)
005900101125     D dsqa          e ds
006000941109     D*
006100030721     D ddatiute      e ds
006200030721     D azuteds       e ds                  extname(AZUTE00F)
006300030721     D tibs34ds      E DS                  inz
006400920427     D KPJBA         E DS
006500040419     D DTAD          E DS
006600040419     D  DDESC        E                     EXTFLD(§TADDESC)
006700040419     D  DRAGR        E                     EXTFLD(§TADRAGR)
006800040419     D  DCDNC        E                     EXTFLD(§TADCDNC)
006900040419     D  DCTLT        E                     EXTFLD(§TADCTLT)
007000040419     D  DCTLS        E                     EXTFLD(§TADCTLS)
007100040419     D  DDAGG        E                     EXTFLD(§TADDAGG)
007200040419     D  DMTVD        E                     EXTFLD(§TADMTVD)
007300040419     D  DDCDN        E                     EXTFLD(§TADDCDN)
007400040419     D  DTICO        E                     EXTFLD(§TADTICO)
007500040419     D  DFDDT        E                     EXTFLD(§TADFDDT)
007600040419     D  DFMED        E                     EXTFLD(§TADFMED)
007700040419     D  DPZDO        E                     EXTFLD(§TADPZDO)
007800040419     D  DKGDO        E                     EXTFLD(§TADKGDO)
007900040419     D  DEVFO        E                     EXTFLD(§TADEVFO)
008000040419     D  DIMPR        E                     EXTFLD(§TADIMPR)
008100040419     D  DTISI        E                     EXTFLD(§TADTISI)
008200040419     D  DDEST        E                     EXTFLD(§TADDEST)
008300040419     D  DRITR        E                     EXTFLD(§TADRITR)
008400040419     D  DDESI        E                     EXTFLD(§TADDESI)
008500040419     D  DCARE        E                     EXTFLD(§TADCARE)
008600040419     D  DCIDD        E                     EXTFLD(§TADCIDD)
008700021029      * TRUL06DS serve per call a TRUL06R
008800021029     d Trul06Ds      e ds
008900021029     d  L1                     1     90  0 Dim(30)
009000030723     D CMAT            C                   CONST('MATTINO   ')
009100030723     D CPOM            C                   CONST('POMERIGGIO')
009200040419     D TAD             S              2    DIM(100)
009300050505     D PAR             S              2    DIM(50)
009400141120     D PARpin          S              2    DIM(50)
009500040419     D DTA             S            256    DIM(100)
009600040421     D TPC             S             10    DIM(2) CTDATA PERRCD(1)
009700930426     C*****************************************************************
009800930426     C* RIEPILOGO INDICATORI
009900930426     C*****************************************************************
010000970327     C* 01    - STAMPA SCRITTA - NON SPUNTATO IN DATA XX/XX/XXXX
010100970327     C* 02    - STAMPA SCRITTA - SPUNTATO SU ALTRA DISTINTA N.  DEL
010200950214     C* 06    - STAMPA SCRITTA - SPUNTATO SU F.  X-XXXXX DEL XX/XX
010300920422     C* 07    - ON  CAMBIATA SPEDIZIONE
010400920422     C* 08    - ON  ESISTE SERIE
010500920423     C* 09    - SEGNACOLLI NON SEQUENZIALI
010600920424     C* 20    - STAMPO TESTATA
010700911018     C*---------------------------------------------------------------*
010800030723     C* ROUTINE INIZIALE
010900030723     C                   EXSR      DEFCAM
011000920909     C*
011100020911     C     Karb78        SETLL     FNARB78L
011200030723    0C                   DO        *hival
011300030723     C     Karb78        READE     FNARB78L
011400920417     C*
011500030723     c                   if        %eof(fnarb78l)
011600030723     c                   leave
011700030723     c                   end
011800141120     C*
011900930308     C* ESCLUDO I SOLI INCASSO SE BOLLA E' NELLO STORICO
012000941109     C*  HANNO GIA' LA DATA CONSEGNA MERCE
012100050505     C     ARBDCM        IFne      0
012200030723     c                   iter
012300030723     c                   end
012400141120     C*
012500110729     c* verifico se perfect order
012600130221      *** Wurth adesso manda nel campo non solo numeri per identificare altre cose
012700130221      ***  quindi si deve sostituire il test per identificare il Perfect Order
012800130221     c************       if        arbxco <> ' '
012900130221     c                   if        arbxco > '0'
013000141120     C*
013100110729     c                   clear                   fnlrf7r8ds
013200110729     c                   eval      dr8aasi= arbaas
013300110729     c                   eval      dr8lnpi= arblnp
013400110729     c                   eval      dr8nrsi= arbnrs
013500110729     c                   eval      dr8nspi= arbnsp
013600110729     c                   eval      dr8rmai= arbrma
013700110729     c                   eval      dr8lnai= arblna
013800110905     c                   eval      dr8xcoi= arbxco
013900110729     c                   move      arbifp        dr8fgsi
014000110729     c                   move      arbndc        dr8ndci
014100110729     c                   eval      kpjbu = fnlrf7r8ds
014200110729     c                   call      'FNLRF7R8'
014300110729     c                   parm                    kpjba
014400110802     c                   movel     kpjbu         fnlrf7r8ds
014500141120     C*
014600110905     c                   if        dr8erro = '007' or dr8erro = '009'
014700110905     c                   clear                   trul52ds
014800141120     C*
014900110905     c                   if        arbdti <> 0
015000110905     C                   Z-ADD     §XCODDTI      D52GIO                         *
015100110905     C                   MOVE      arbdti        D52DIN                         *
015200110905     c                   else
015300110905     C                   Z-ADD     §XCODdsp      D52GIO                         *
015400110905     C                   movel     arbAAS        d52din
015500110905     C                   move      arbmgs        d52din
015600110905     c                   end
015700141120      ***
015800110905     C                   CALL      'TRUL52R'
015900110905     C                   PARM                    TRUL52ds
016000110905     C                   MOVE      D52DFI        DAT1GG            8 0          *
016100110905     c                   end
016200141120      ***
016300110905     c* non emetto errore di sorelle non in distinta o di record bolla
016400110905     c* mancanti se trascorsi i tempi di attesa
016500110905     c                   if        DAT1GG<=D59DDT
016600110905     c                   clear                   dr8erro
016700110905     c                   end
016800141120      ***
016900110905     c                   if        dr8erro<> ' '
017000110729     c                   clear                   fnartds
017100110729     C                   SETOFF                                       010206
017200110729     C                   SETOFF                                       030410
017300110729     C                   SETOn                                        11
017400110729     c                   exsr      stampa
017500110802     C                   SETOff                                       11
017600110905     c*                  iter
017700110729     c                   end
017800141120      ***
017900110729     c                   end
018000141120      ***
018100110729     c                   clear                   parzprfo          1
018200141120      ***
018300130221      *** Wurth adesso manda nel campo non solo numeri per identificare altre cose
018400130221      ***  quindi si deve sostituire il test per identificare il Perfect Order
018500130221     c*****arbxco        comp      ' '                                4141
018600130221     c     arbxco        comp      '0'                                41
018700141120      *
018800141120ab   C* controllo particolarità consegne =  se HA il PINCODE
018900141121ab   c                   clear                   SegnalaErrPCDE    1
019000141120      *
019100050505     C* controllo particolarità consegne =  NO PARZ.
019200050505     c                   clear                   parz              1
019300141120      *
019400050505     c                   if        arbgma <> ' '
019500141120      *
019600050505     C                   Z-ADD     1             zz                4 0
019700050505     C     arbgma        LOOKUP    par(zz)                                40
019800141120ab    **
019900141120ab    **  Controlla se bolla con PINCODE in consegna = 48(ON)
020000141120ab   C                   Z-ADD     1             pn                4 0
020100141121ab   C     arbgma        LOOKUP    parPIN(pn)                             48
020200141121ab    **
020300141121ab    **  Se abilitato il PINCODE per la Consegna Particolare e la
020400141121ab    **  Distinta non ha il PDA oppure è in test x la Consegna
020500141121ab    **    Deve segnalare l'errore da gestire
020600141121ab   c                   if        %Equal and DST_PDA_noTST <> 'S'
020700141121ab   c                   eval        SegnalaErrPCDE = 'S'
020800141121ab   c                   end
020900141120ab    **
021000050505     c                   else
021100141120     c                   setoff                                       40
021200050505     c                   end
021300930308     C*
021400050505     C                   SETOFF                                         12
021500141120     C*
021600941109     C     KART          SETLL     FNART01L                               33
021700050505    1C     *IN33         IFEQ      '0'
021800920622     C*
021900941109     C     KART          CHAIN     FNLBL000                           34
022000920909     C*
022100920909     C* BOLLA FIGLIA LEGATA A BOLLA MAMMA
022200050505    2C     *IN34         IFEQ      '0'
022300920622     C                   SETON                                        12
022400941109     C*
022500941109     C     KLBL2         SETLL     FNART01L
022600050505    3c                   do        *hival
022700030723     C     KLBL2         READE     FNART01L
022800920909     C*
022900050505     C                   if        %eof(fnart01l)
023000030723     c                   leave
023100030723     c                   end
023200941109     C*
023300941109     C* CONTROLLO SEGNACOLLO MANCANTE
023400941109     C                   EXSR      CTRART
023500920909     C*
023600050505    3C                   ENDDO
023700050505    2C                   END
023800920622     C*
023900050505   X1C                   ELSE
024000920909     C*
024100920909     C* SE E' MAMMA NON CONSIDERO TANTO CI SARA' LA FIGLIA
024200941109     C     KART          SETLL     FNLBL002                               31
024300920909     C*
024400050505    2C     *IN31         IFEQ      '0'
024500941109     C     KART          READE     FNART01L                               31
024600920909     C*
024700050505    3C     *IN31         DOWEQ     '0'
024800941109     C*
024900941109     C* CONTROLLO SE SEGNACOLLO MANCANTE
025000941109     C                   EXSR      CTRART
025100941109     C*
025200941109     C     KART          READE     FNART01L                               31
025300050505    3C                   ENDDO
025400050505    2C                   END
025500920622     C*
025600050505    1C                   END
025700141120     ***-----
025800050505     c* se ind 10 acceso vuol dire che mi manca spunta 4 e ho part.
025900110729     c* consegna = NO PARZ. oppure perfect order
026000110729     c                   if        parz = '1' or parzprfo = '1'
026100141120ab   c                                or
026200141120ab   c                               SegnalaErrPCDE = 'S'
026300050505     C                   SETOFF                                       010206
026400050505     C                   SETOFF                                       0304
026500141120     ***
026600110829     c                   if        parz = '1'
026700050505     c                   seton                                        10
026800110829     c                   end
026900141120     ***
027000110829     c                   if        parzprfo = '1'
027100110829     c                   eval      dr8erro='010'
027200110829     c                   seton                                        11
027300110829     c                   end
027400141120      *-
027500141121ab   c***  per segnalare l'anomalia del PIN CODE imposta il CODICE-011
027600141121ab    *     che serve per la decodifica da tabella SQA (11-ind di stampa)
027700141121ab    *       lo evidenzio se non c'era già un'altra anomalia da stampare
027800141121ab   c                   if            SegnalaErrPCDE = 'S'
027900141124ab   c                   if                   dr8erro = *blank
028000141124ab   c                                          or
028100141124ab   c                                        dr8erro = '011'
028200141120ab   c                   eval      dr8erro='011'
028300141120ab   c                   seton                                        11
028400141120ab   c                   end
028500141124ab   c                   end
028600141120     ***
028700050505     c                   exsr      stampa
028800110829     c                   setoff                                       1011
028900141120     ***
029000090224     c* scrivo il record di errore nel wf
029100110729     c                   if        parz = '1'
029200141121ab   c                               or
029300141121ab   c                             SegnalaErrPCDE = 'S'
029400141120      *---
029500090224     c                   clear                   comnsc
029600090303     c     kwf7          chain     fnlrf71l
029700141120      *---
029800090226     c                   MOVEL     OGGI          wf7tim
029900090226     c                   MOVE      TIMES         wf7tim
030000090224     c                   clear                   wf7err
030100090224     c                   clear                   wf7nsc
030200090224     c                   clear                   wf7can
030300090224     c                   clear                   wf7fls
030400090224     c                   eval      WF7AAS = ARBAAS
030500090224     c                   eval      WF7LNP = ARBLNP
030600090224     c                   eval      WF7NRS = ARBNRS
030700090224     c                   eval      WF7NSP = ARBNSP
030800090224     c                   eval      WF7lna = ARBlna
030900090224     c                   eval      WF7FGS = d59fgs
031000090224     c                   eval      WF7DDC = d59ddt
031100090224     c                   eval      WF7NFV = wnfv
031200090224     c                   eval      WF7pdc = d59pdr
031300141121ab    **
031400141121ab    **   Deve esserci un solo record a livello di singola bolla
031500141121ab    **   quindi se prima gestiva la Parziale lasciamo la Parziale come primo caso
031600141121ab    **   ed in seguito  aggiungo altri eventuali segnalazioni come il PINCODE
031700141121ab   c                   If        parz = '1'
031800141121ab    **
031900090224     c                   eval      WF7not = 'NON EFFETTUARE CONSEGNA PARZIALE'
032000141121ab    **
032100141121ab    **   se non ha già un'altra segnalazione a livello di bolla.
032200141124ab   c                   elseIf    SegnalaErrPCDE = 'S' and WF7not = *blank
032300141121ab    **
032400141121ab   c                   eval      WF7not = 'PINCODE: DISTINTA NO PDA O IN TEST'
032500141121ab   c                   end
032600141120      *---
032700090303     c                   if        not %found(fnlrf71l)
032800090224     c                   write     wfnlrf00
032900090224     c                   else
033000090224     c                   update    wfnlrf00
033100110729     c                   end
033200141120      *---
033300090224     c                   end
033400050505     c                   end
033500141120     ***-----
033600930308     C*
033700941109    0C                   ENDDO
033800010613     C*
033900030723     c                   movel(p)  FNLR59DS      kpjbu
034000030723     C                   SETON                                        RT
034100030805     C**********************************************************************
034200030805     C* CONTROLLO SE SEGNACOLLO MANCANTE-----------------------------*
034300030805     C**********************************************************************
034400030805     C     CTRART        BEGSR
034500030801     C*
034600030801     C* ELABORO SOLO I COLLI NON CONSEGNATI ED ARRIVATI
034700030801    1C     ARTDCM        IFEQ      0
034800030801     C                   SETOFF                                       010206
034900040420     C                   SETOFF                                       0304
035000030801     C*
035100030801     C                   Z-ADD     4             WNPG
035200070130     C     KBRV          CHAIN     FNBRV01L                           36
035300030801     C* SPUNTA NON TROVATA
035400030801    2C     *IN36         IFEQ      *ON
035500080507     c* aggiorno il file del dettaglio segnacolli dei dati a pda
035600090223     c     kpdd          chain(n)  fipdd01t
035700141120      ***
035800080507     c                   if        %found(fipdd01t) and PDDSPUMAG ='S'
035900080507     c                             and pddatb = ' '
036000141120      ***
036100090223     c     kpdd          chain     fipdd01t
036200090223     c                   if        %found(fipdd01t)
036300080508     c                   clear                   PDDSPUMAG
036400080508     c                   eval      tmp= %char(%timestamp: *iso0)
036500080508     c                   eval      PDdDATORA =   tmp
036600080507     c                   update    fipdd000
036700080507     c                   end
036800090223     c                   end
036900030801     C*
037000040420     C* VEDO SE E' PRESENTE UNA SPUNTA SU UN'ALTRA DISTINTA del mio terminal
037100070130     C     KBRV2         CHAIN     FNBRV09L                           36
037200030801     C*
037300030801     C* COLLO NON PRESENTE IN NESSUNA DISTINTA
037400030801    3C     *IN36         IFEQ      *ON
037500030822     C                   SETON                                        01
037600030801   X3C                   ELSE
037700071204     C* SE DISTINTA CHIUSA o aperta e vecchia COME SE NON FOSSE PRESENTE
037800030801     C                   Z-ADD     BRVNFV        WNFV5
037900030801     C     KFVV2         CHAIN     FNFVV000                           31
038000030801    4C     *IN31         IFEQ      *ON
038100030801     C     FVVFCF        OREQ      'S'
038200071204     C     FVVFCF        OREQ      ' '
038300071204     c     fvvdfv        andne     d59ddt
038400030801     C                   SETON                                        01
038500030801   X4C                   ELSE
038600040712     c*segnalo solo se il po della spunta è diverso da quello della distinta
038700071128     c* che sto quadrando o se il foglio non è della data che sto quadrando.
038800071128     c* se fossero uguali ho già segnalato l'anomalia nel
038900040712     c* FNLRF7R2
039000071128     c                   if        d59fgs <> brvfgs
039100040712     C                   SETON                                        02
039200030801    4C                   ENDIF
039300040712    4C                   ENDIF
039400030801    3C                   ENDIF
039500141120     ****
039600040419     c* Se non spuntato in nessun foglio 4 verifico se ho un danno
039700040419    3C     *IN01         IFEQ      *ON
039800040419     C                   EXSR      TESDAN
039900040420     c* Se non spuntato in nessun foglio 4 e no c.a.
040000040419     c                   if        wdavm = 'N'
040100040420     c* se no data arrivo merce e no data foglio partenza stampo anomalia
040200040420     c* collo non spuntato in uscita non partito e non arrivato
040300040420     c                   if        artdam = 0 and artdfv = 0
040400040420     c                   seton                                        04
040500040420     c                   setoff                                       01
040600040420     c                   else
040700040420     c* cerco l'ultima spunta
040800030801     C                   EXSR      CHABRV
040900040420     c                   end
041000141120      ****
041100050505     c* se non ho la spunta 4 e ho la particolarità consegna = NO PARZIALI
041200090304     c* segnalo l'anomalia in stampa se bolla con + colli
041300110729     c                   if        arbncl > 1
041400110729     c                   if        *in40
041500110729     c                   eval      parz = '1'
041600050505     c                   end
041700141120      ****
041800110729     c                   if        *in41
041900110729     c                   eval      parzprfo = '1'
042000110729     c                   end
042100110729     c                   end
042200141120      ****
042300040419     c                   else
042400040420     c* Se non spuntato in nessun foglio 4 e ho c.a. stampo il segnacollo
042500040420     c* con danno
042600040419     c                   seton                                        03
042700040420     c                   setoff                                       01
042800040419     c                   end
042900141120      ****
043000040419     C                   ENDIF
043100040420     C*                  SETOFF                                       01
043200030801     C*
043300040420     c                   if        *in01 or *in02 or *in03 or *in04 or *in06
043400030801     C                   EXSR      STAMPA
043500030825     c                   end
043600030801     C*
043700080507     c                   else
043800080507     c* se spunta 4 aggiorno il file del dettaglio segnacolli dei dati a pda
043900090223     c     kpdd          chain(n)  fipdd01t
044000090224     c                   if        %found(fipdd01t)
044100080507     c                             and pddatb = ' '
044200090224     c                   if        PDDSPUMAG <>'S' or
044300090224     c                             (PDDSPUMAG ='S' and pddcan <> brvcan)
044400090223     c     kpdd          chain     fipdd01t
044500090223     c                   if        %found(fipdd01t)
044600080507     c                   eval      PDDSPUMAG ='S'
044700090224     c                   eval      PDDcan = brvcan
044800080508     c                   eval      tmp= %char(%timestamp: *iso0)
044900080508     c                   eval      PDdDATORA =   tmp
045000080507     c                   update    fipdd000
045100080507     c                   end
045200090223     c                   end
045300090224     c                   end
045400090224     c* se spunta 4 con anomalia e la bolla ha part. cons. NO PARZIALI
045500090224     c* memorizzo il file di work per controlli filiali
045600090224     c                   if        brvcan <> ' ' and *in40
045700090224     c                   eval      comnsc = artnsc
045800090303     c     kwf7          chain     fnlrf71l
045900090226     c                   MOVEL     OGGI          wf7tim
046000090226     c                   MOVE      TIMES         wf7tim
046100090224     c                   clear                   wf7err
046200090224     c                   eval      wf7fls = artfls
046300090224     c                   eval      wf7nsc = artnsc
046400090224     c                   eval      wf7can = brvcan
046500090224     c                   eval      WF7AAS = ARBAAS
046600090224     c                   eval      WF7LNP = ARBLNP
046700090224     c                   eval      WF7NRS = ARBNRS
046800090224     c                   eval      WF7NSP = ARBNSP
046900090224     c                   eval      WF7lna = ARBlna
047000090224     c                   eval      WF7FGS = d59fgs
047100090224     c                   eval      WF7DDC = d59ddt
047200090224     c                   eval      WF7NFV = wnfv
047300090224     c                   eval      WF7pdc = d59pdr
047400090303     c                   eval      WF7not = 'PART.CONSEGNA PARZIALE: COLLI CON -
047500090303     c                             ANOMALIE'
047600090303     c                   if        NOT %found(fnlrf71l)
047700090224     c                   write     wfnlrf00
047800090226     C                   ELSE
047900090226     c                   UPDATE    wfnlrf00
048000090224     c                   end
048100090224     c                   end
048200090224     c*
048300030801    2C                   END
048400030801    1C                   END
048500030801     C                   ENDSR
048600030801     C**********************************************************************
048700030801     C* CHAIN BRV CON CATEGORIA NON 4 -------------------------------*
048800030801     C**********************************************************************
048900030801     C     CHABRV        BEGSR
049000040409     c                   clear                   dsbrvc
049100040422     c                   clear                   dsfvvc
049200040422     c                   do        6             xx                2 0
049300040420     c                   move      xx            wnpg
049400040409     C*
049500070130     C     KBRV3         SETll     FNBRV09L
049600040409     c                   do        *hival
049700070130     C     KBRV3         READe     FNBRV09L
049800070130     c                   if        %eof(fnbrv09l)
049900040409     c                   leave
050000040409     c                   end
050100090121     c* nella ricerca considero solo le spunte reali cioè quelle con
050200090121     c* data caricamento impostata
050300090121     c                   if        BRVDCS = 0
050400090121     c                   iter
050500090121     c                   end
050600040419     c*
050700040422     c                   clear                   dsfvv
050800040409     C                   z-add     BRVNFV        WNFV5
050900040420     C     KFVV2         CHAIN     FNFVV01L
051000040423     C                   if        not %found(fnfvv01l)
051100040526     c                   z-add     brvfgs        fvvfgs
051200040526     c                   z-add     brvnpg        fvvnpg
051300040526     c                   if        brvnfv < 100000
051400040526     c                   z-add     brvnfv        fvvnfv
051500040422     c                   end
051600040526     c                   end
051700040419     c*
051800040419     c                   if        brvdfs > c_brvdfs or
051900040419     c                             (brvhms > c_brvhms and brvdfs = c_brvdfs)
052000040419     c                   eval      dsbrvc = dsbrv
052100040419     c                   eval      dsfvvc = dsfvv
052200040420     C                   SETON                                        06
052300040419     C                   SETOFF                                       01
052400040409     c                   end
052500040420     C                   ENDDO
052600040409     C                   ENDDO
052700040409     c*
052800040409     c                   eval      dsbrv = dsbrvc
052900040409     c                   eval      dsfvv = dsfvvc
053000030801     C                   ENDSR
053100030707     C**********************************************************************
053200920421     C* STAMPO ERRORE -----------------------------------------------*
053300030707     C**********************************************************************
053400900621     C     STAMPA        BEGSR
053500141120      **
053600030723     c                   movel     'S'           d59ANM
053700911015     C* STAMPA TESTATA
053800030728     C   80              DO
053900030723     C                   ADD       1             D59PAG
054000920422     C                   SETON                                        07
054100101201     c                   if        d59sqa = ' ' or
054200101201     c                             d59sqa = 'T'
054300030728     C                   WRITE     TESTA
054400141121      *
054500050502     c                   if        d59lsr = 'S' and d59pag = 1
054600050422     C                   write     testal
054700050426     c                   end
054800141121      *
054900030729     C                   WRITE     TESTA1
055000101125     C                   END
055100141121      *
055200030728     C                   Z-ADD     6             D59LIN
055300030728     C                   SETOFF                                       80
055400911018     C                   END
055500920422     C*
055600911018     C                   Z-ADD     ARBAAS        COMAAS
055700911018     C     SAVSPE        IFNE      ARBSPE
055800920422     C                   SETON                                        07
055900911018     C                   MOVEL     ARBSPE        SAVSPE
056000911018     C                   END
056100911018     C* DATA SPEDIZIONE
056200920422     C   07              DO
056300911018     C                   MOVE      ARBDSP        G02INV
056400911018     C                   MOVE      *ZEROS        G02DAT
056500911018     C                   MOVEL     3             G02ERR
056600941109     C                   CALL      'XSRDA8'
056700911018     C                   PARM                    WLBDAT
056800920422     C                   MOVEL     G02DAT        STADSP            4 0
056900920619     C* RAGIONE SOCIALE DESTINATARIO
057000040419     C                   MOVEL     ARBRSD        STARSD           22
057100920423     C* SEGNACOLLI NON CONSECUTIVI
057200920423     C     ARBFNS        COMP      'S'                                    09
057300920421     C* SERIE >< 0
057400920422     C     ARBNRS        COMP      0                                  0808
057500911018     C                   END
057600141121      *
057700030801     C* serie e segnacollo
057800030801     c                   z-add     artnrs        st2nrs            2 0
057900030801     c                   z-add     artnsc        st2nsc            7 0
058000141120      *---
058100920622     C* 02 ON - MANCANTE SU ALTRA DISTINTA SPUNTATO - PRENDO DATA
058200941110     C     *IN02         IFEQ      *ON
058300950214     C     *IN06         OREQ      *ON
058400040712     C                   Z-ADD     BRVfgs        ST2fgs
058500040712     C                   Z-ADD     BRVNFV        ST2NFV
058600920506     C                   MOVE      FVVDFV        G02INV
058700920506     C                   MOVE      *ZEROS        G02DAT
058800920506     C                   MOVEL     3             G02ERR
058900941109     C                   CALL      'XSRDA8'
059000920506     C                   PARM                    WLBDAT
059100920506     C                   MOVEL     G02DAT        STADDC            4 0
059200030728     C                   MOVEL     G02DAT        STADDC1           4 0
059300920506     C                   END
059400141120      *---
059500070130     c                   if        *in06
059600070130     c                   movel     brvhms        brvhms_p
059700070130     c                   endif
059800141120      *---
059900141120      *-  cerca descrizione da tabella SQA
060000141121     c                   exsr      Cerca_su_SQA
060100141120      *---
060200900622     C* STAMPO DETTAGLIO
060300101201     c                   if        d59sqa = ' ' or
060400101201     c                             d59sqa = 'T'
060500030728     C                   WRITE     DET
060600101201     c                   end
060700101125     c* video
060800101201     c                   if        d59sqa = 'S' or
060900101201     c                             d59sqa = 'T'
061000101125     c                   exsr      srsqa
061100101125     c                   end
061200141120      *-
061300030724     C                   ADD       1             D59LIN
061400920422     C                   SETOFF                                       07
061500900622     C*
061600900622     C                   ENDSR
061700141120     C*-----------------------------------------------------*
061800141120     C* cerca descrizione in tabella  stampata con 11=ON    *
061900141120     C*-----------------------------------------------------*
062000141121     C     Cerca_su_SQA  BEGSR
062100141121      *
062200141121     c                   clear                   dessqa
062300141121      *
062400141121      *  imposta la Segnalazione da Tabella sul dettaglio
062500141120     c                   if        *in11
062600141120     c                   eval      tbeke2 = dr8erro
062700141120     c                   eval      tbecod = 'SQA'
062800141120     c                   eval      tbeke1 = 'FNLRF7R4'
062900141120     c     ktbe          chain     tntbe01l
063000141120     c                   if        %found(tntbe01l)
063100141120     c                   movel     tbeuni        dsqa
063200141120     c                   movel     d§sqades      dessqa
063300141120     c                   else
063400141120     c                   clear                   dsqa
063500141120     c                   endif
063600141120     c                   endif
063700141120     C*
063800141120     C                   ENDSR
063900101125     C*-----------------------------------------------------*
064000101125     C* memorizzo l'errore nel file di work                 *
064100101125     C*-----------------------------------------------------*
064200101125     C     srsqa         BEGSR
064300101125     c                   clear                   dsqa
064400101125     c                   eval      tbecod = 'SQA'
064500101125     c                   eval      tbeke1 = 'FNLRF7R4'
064600101125     c                   select
064700101125     c* no spunta 4
064800101125     c                   when      *in01
064900101125     c                   eval      tbeke2 = '001'
065000101125     c* altra distinta
065100101125     c                   when      *in02
065200101125     c                   eval      tbeke2 = '002'
065300101125     c* ultima spunta ...
065400101125     c                   when      *in06
065500101125     c                   eval      tbeke2 = '003'
065600101125     c* c.a
065700101125     c                   when      *in03
065800101125     c                   eval      tbeke2 = '004'
065900101125     c* nessuna spunta
066000101125     c                   when      *in04
066100101125     c                   eval      tbeke2 = '005'
066200110729     c* parziale CP e perfect order
066300110729     c                   when      *in10
066400110729     c                   if        parz='1'
066500101125     c                   eval      tbeke2 = '006'
066600110729     c                   end
066700110729     c                   if        parzprfo='1'
066800110729     c                   eval      tbeke2 = '010'
066900110729     c                   end
067000110729     c* perfect order
067100141121ab   c*  o
067200141121ab   c* PINcode non congruente con la DIstinta
067300110729     c                   when      *in11
067400110729     c                   eval      tbeke2 = dr8erro
067500101125     c                   endsl
067600141121      **
067700101125     c     ktbe          chain     tntbe01l
067800101125     c                   if        %found(tntbe01l)
067900101125     c                   movel     tbeuni        dsqa
068000101125     c                   if        D§SQAELA = 'S'
068100101125     c* TESTATA
068200101125     c                   clear                   wfnlrq1ds
068300101125     c                   eval      wlrq1ndc = d59ndt
068400101125     c                   eval      wlrq1fgs = d59fgs
068500101125     c                   eval      wlrq1pdr = d59pdr
068600101203     c                   eval      wlrq1aas = arbAAS
068700101203     c                   eval      wlrq1LNP = arbLNP
068800101203     c                   eval      wlrq1NRS = arbNRS
068900101203     c                   eval      wlrq1NSP = arbNSP
069000101125     c     kwf10         chain     wfnlrq11l
069100101125     c                   if        not %found(wfnlrq11l)
069200101125     c                   movel     tbeke2        wlrq1cer
069300101125     c                   eval      wlrq1sce = D§SQASCE
069400101125     c                   eval      wlrq1err = d§sqades
069500101125     c                   write     wfnlrq10
069600101125     c                   else
069700101125     c                   eval      wlrq1err = 'Errore dettaglio colli'
069800101125     c                   clear                   wlrq1cer
069900101125     c                   clear                   wlrq1sce
070000101125     c                   update    wfnlrq10
070100101125     c                   end
070200101125     c* dettaglio
070300101125     c                   clear                   wfnlrq2ds
070400101125     c* se ho i rif. della spedizione li memorizzo altrimenti 0
070500101125     c                   eval      wlrq2aas = wlrq1aas
070600101125     c                   eval      wlrq2lnp = wlrq1lnp
070700101125     c                   eval      wlrq2nrs = wlrq1nrs
070800101125     c                   eval      wlrq2nsp = wlrq1nsp
070900101125     c                   eval      wlrq2ndc = wlrq1ndc
071000101125     c                   eval      wlrq2fgs = wlrq1fgs
071100101125     c                   eval      wlrq2pdr = wlrq1pdr
071200101125     c                   movel     tbeke2        wlrq2cer
071300101125     c                   eval      wlrq2sce = D§SQASCE
071400101125     c                   eval      wlrq2err = d§sqades
071500110104     c  n10              eval      wlrq2fls = arbfls
071600110104     c  n10              eval      wlrq2nsc = st2nsc
071700110126     c  n10              eval      wlrq2nrc = st2nrs
071800110104     c   04              eval      wlrq2fls = artfls
071900110126     c   04              eval      wlrq2nrc = artnrs
072000110104     c   04              eval      wlrq2nsc = artnsc
072100101125     c                   select
072200101125     c                   when      tbeke2 = '002'
072300110104     c                   eval      wlrq2flv = st2fgs
072400101125     c                   eval      wlrq2nfv = st2nfv
072500110117     c                   eval      wlrq2dfv = fvvdfv
072600101126     c                   if        wlrq2nfv <> 0
072700101125     c                   eval      wlrq2npg = 4
072800101126     c                   end
072900101125     c                   when      tbeke2 = '003'
073000110104     c                   eval      wlrq2flv = fvvfgs
073100101125     c                   eval      wlrq2nfv = fvvnfv
073200110117     c                   eval      wlrq2dfv = fvvdfv
073300101125     c                   eval      wlrq2npg = fvvnpg
073400101126     c                   eval      wlrq2nps = brvnps
073500101126     c                   eval      wlrq2hms = brvhms
073600101125     c                   endsl
073700101125     c*
073800101125     c                   write     wfnlrq20
073900101125     c*
074000101125     c                   end
074100101125     c                   end
074200101125     C                   ENDSR
074300040420     C*--------------------------------------------------------------*
074400040420     C*  TESDAN: Test gestione danni
074500040420     C*--------------------------------------------------------------*
074600040420     C     TESDAN        BEGSR
074700040420     C*
074800040421     C                   MOVEL     *BLANKS       sTIPCA
074900040420     C*  Flag pratica danno = 'N'
075000040420     C                   MOVE      'N'           WDAVM             1
075100040420     C*  Se il collo non è stato spuntato su quel foglio di
075200040420     C*  uscita controllo se c'è una pratica C.A. aperta
075300040420     C     KDCD          CHAIN     FNDCD02L                           38
075400040420     C*  Se esiste C.A. aperta controllo il tipo di anomalia
075500040420     C     *IN38         IFEQ      '0'
075600040420     C     DCDATB        ANDNE     'A'
075700040420     C     DCDDCH        IFEQ      0
075800040420     C                   MOVE      TPC(1)        sTIPCA
075900040420     C* Aggancio testata
076000040420     C     KDCT          CHAIN     FNDCT01L                           38
076100040420     C     *IN38         DOWEQ     '0'
076200040420     C     DCTATB        ANDEQ     'A'
076300040420     C     *IN38         OREQ      '0'
076400040420     C     DCTDCH        ANDNE     0
076500040420     C     KDCT          READE     FNDCT01L                               38
076600040420     C                   END
076700040420     C                   Z-ADD     1             YY                4 0
076800040420     C     DCTTAD        LOOKUP    TAD(YY)                                39
076900040420     C     *IN39         IFEQ      '1'
077000040420     C                   MOVEL     DTA(YY)       DTAD
077100040420     C     DRAGR         IFEQ      'M'
077200040420     C     DRAGR         OREQ      'V'
077300040420     C                   MOVE      'S'           WDAVM
077400040420     C                   END                                                    Manc.o Ava D/R
077500040420     C                   END                                                    Trv TAD
077600040420     C*
077700040420     C                   ELSE
077800040420     C                   MOVE      TPC(2)        sTIPCA
077900040420     C                   END                                                    CA Aperta
078000040420     C                   END                                                    CA no annul
078100040420     C*
078200040420     C                   ENDSR
078300050505     C**********************************************************************
078400050505     C* OPERAZIONI INIZIALI------------------------------------------*
078500050505     C**********************************************************************
078600050505     C     DEFCAM        BEGSR
078700050505     C**
078800030801     C     *ENTRY        PLIST
078900030801     C                   PARM                    KPJBA
079000030801     C                   MOVEL     KPJBU         FNLR59DS
079100050204     C* IMPOSTO L'INDICATORE PER STAMPARE IL TITOLO IN BASE AL PARAMETRO
079200050204     C* TIPO STAMPA A=CONTROLLO AUTOTRASPORTATORE " "/O= DISTINTA DI USCITA
079300050204     C* AUTOTRASPORTATORE
079400050204     C                   IF        D59STP = 'A'
079500050204     C                   SETON                                        61
079600050204     C                   SETOFF                                       62
079700050204     C                   ELSE
079800050204     C                   SETOFF                                       61
079900050204     C                   SETON                                        62
080000050204     C                   END
080100040930     c*
080200040930     c                   movel(p)  'FNLRF7R'     nompgm           10
080300040930     c*
080400030801     C                   IF        D59TPR = 'U'
080500030801     C                   SETON                                        LR
080600030801     C                   RETURN
080700030801     C                   END
080800030801     C                   Z-ADD     D59NDT        WNFV
080900030801     C                   Z-ADD     D59NDT        WNFV5
081000030801     C                   Z-ADD     4             WNPG
081100030801     C                   MOVEL     CMAT          TPPRE
081200030801     C     D59FPP        IFEQ      'P'
081300030801     C                   MOVEL     CPOM          TPPRE            10
081400030801     C                   END
081500030801     C**
081600030801     C* IMPOSTO CAMPI FISSI
081700030801     C**
081800030801     c                   Z-Add     D59fgs        Karbifp
081900030801     C                   Z-ADD     D59NDT        COMNDC            7 0
082000030801     C                   MOVEL     *ZEROS        SAVSPE           16
082100030801     c* campi testata
082200030801     C                   Z-ADD     D59pdr        prtpdr            7 0
082300030801     C                   movel     D59dsf        prtdsf           35
082400050422     C                   movel     D59dsf        prtdsf2
082500030801     c                   Z-Add     D59fgs        prtfgs
082600030801     C                   Z-ADD     D59NDT        prtndt            5 0
082700110209     C                   Z-ADD     D59NDT        prtndt3
082800110209     C*                  eval      prtndt2 = d59bcd
082900110209     C                   move      d59bcd        prtndt2
083000030801     C*
083100030801     C* CHAIN E DATA DELLA DISTINTA
083200030801     C     KFVV          CHAIN     FNFVV000                           32
083300030801     C*
083400030801     C     *IN32         IFEQ      '0'
083500030801     C     FVVDFV        ANDEQ     D59DDT
083600030801     C                   MOVE      FVVDFV        G02INV
083700030801     C                   MOVE      *ZEROS        G02DAT
083800030801     C                   MOVEL     3             G02ERR
083900030801     C                   CALL      'XSRDA8'
084000030801     C                   PARM                    WLBDAT
084100030801     C                   Z-ADD     G02DAT        STcddc            8 0
084200030801     C                   END
084300030801     C*
084400030801     C                   IF        D59LIN = 0 OR D59LIN > 54
084500030801     C                   SETON                                        80
084600030801     C                   END
084700030801     C*
084800141120ab    *   Dalla Distinta verifica se ha il PDA e non è in TEST ?
084900141120ab   C                   eval        dstNPG = 4
085000141120ab   C                   eval        dstNFV = d59ndt
085100141120ab   C                   eval        dstFGS = d59fgs
085200141120ab   c                   clear                   DST_PDA_noTST     1
085300141120ab   C     Kdst          CHAIN     fidst01l
085400141120ab   C                   if        %Found(fidst01l)
085500141120ab   c                   eval      ddstFLR = dstFLR
085600141120ab    *
085700141120ab    * Controlla se la distinta è gestita con il PDA e se in fase di test
085800141120ab   c                   If         (dstPDA ='C' or dstPDA = 'E')
085900141120ab   c                                  and
086000141120ab   c                              (§dstTSTPDA <>'C' and §dstTSTPDA <>'E')
086100141120ab    *
086200141120ab   c                   eval      DST_PDA_noTST ='S'
086300141120ab    *
086400141120ab   c                   end
086500141120ab    *
086600141120ab   C                   end
086700141120     C*
086800030801     C                   ENDSR
086900030801     C**********************************************************************
087000030801     C* OPERAZIONI INIZIALI------------------------------------------*
087100030801     C**********************************************************************
087200030801     C     *INZSR        BEGSR
087300030801     c*
087400030801     C                   OPEN      FNLRF7P
087500030801     C**
087600030801     C* KLIST
087700030801     C**
087800101125     C     Ktbe          KLIST
087900101125     C                   KFLD                    tbecod
088000101125     C                   KFLD                    tbeke1
088100101125     C                   KFLD                    tbeke2
088200101125     C     Kwf10         KLIST
088300101125     C                   KFLD                    WLRQ1AAS
088400101125     C                   KFLD                    WLRQ1LNP
088500101125     C                   KFLD                    WLRQ1NRS
088600101125     C                   KFLD                    WLRQ1NSP
088700101125     C                   KFLD                    WLRQ1FGS
088800101125     C                   KFLD                    WLRQ1NDC
088900101125     C                   KFLD                    WLRQ1PDR
089000030801     C     KART          KLIST
089100030801     C                   KFLD                    ARBAAS
089200030801     C                   KFLD                    ARBLNP
089300030801     C                   KFLD                    ARBNRS
089400030801     C                   KFLD                    ARBNSP
089500080507     C     Kpdd          KLIST
089600080507     c                   Kfld                    d59fgs
089700080507     c                   Kfld                    wnfv
089800080507     C                   KFLD                    ARBAAS
089900080507     C                   KFLD                    ARBLNP
090000080507     C                   KFLD                    ARBNRS
090100080507     C                   KFLD                    ARBNSP
090200080507     C                   KFLD                    ARTNSC
090300090224     C     Kwf7          KLIST
090400090224     c                   Kfld                    d59fgs
090500090224     c                   Kfld                    wnfv
090600090224     c                   Kfld                    d59ddt
090700090224     C                   KFLD                    ARBAAS
090800090224     C                   KFLD                    ARBLNP
090900090224     C                   KFLD                    ARBNRS
091000090224     C                   KFLD                    ARBNSP
091100090224     C                   KFLD                    comnSC
091200030801     c     Karb78        Klist
091300030801     c                   Kfld                    Karbifp
091400030801     c                   Kfld                    Comndc
091500030801
091600030801     C     KBRV          KLIST
091700030801     C                   KFLD                    wnpg
091800030801     C                   KFLD                    WNFV
091900030801     C                   KFLD                    D59FGS
092000030801     C                   KFLD                    ARTFLS
092100030801     C                   KFLD                    ARTLNA
092200030801     C                   KFLD                    ARTNRS
092300030801     C                   KFLD                    ARTNSC
092400040420     C     KBRV2         KLIST
092500030801     C                   KFLD                    wnpg
092600030801     C                   KFLD                    ARTFLS
092700030801     C                   KFLD                    ARTLNA
092800030801     C                   KFLD                    ARTNRS
092900030801     C                   KFLD                    ARTNSC
093000040420     C                   KFLD                    SIMFEL
093100040420     C     KBRV3         KLIST
093200040420     C                   KFLD                    wnpg
093300040420     C                   KFLD                    ARTFLS
093400040420     C                   KFLD                    ARTLNA
093500040420     C                   KFLD                    ARTNRS
093600040420     C                   KFLD                    ARTNSC
093700141120ab   C     Kdst          KLIST
093800141120ab   C                   KFLD                    dstNPG
093900141120ab   C                   KFLD                    dstNFV
094000141120ab   C                   KFLD                    dstFGS
094100030805     C     KFVV          KLIST
094200030805     C                   KFLD                    WNPG
094300030805     C                   KFLD                    WNFV5
094400030805     C                   KFLD                    D59FGS
094500030801     C     KFVV2         KLIST
094600030801     C                   KFLD                    BRVNPG
094700030801     C                   KFLD                    WNFV5
094800030801     C                   KFLD                    BRVFGS
094900030801     C     KLBL2         KLIST
095000030801     C                   KFLD                    LBLAAP
095100030801     C                   KFLD                    LBLLPP
095200030801     C                   KFLD                    LBLNRP
095300030801     C                   KFLD                    LBLNSP
095400040419     C     KDCD          KLIST
095500040419     C                   KFLD                    ARTFLS
095600040419     C                   KFLD                    ARTLNA
095700040419     C                   KFLD                    ARTNRS
095800040419     C                   KFLD                    ARTNSC
095900040419     C     KDCT          KLIST
096000040419     C                   KFLD                    DCDAAC
096100040419     C                   KFLD                    DCDFIL
096200050505     C     Ktab          KLIST
096300050505     C                   KFLD                    tblkut
096400050505     C                   KFLD                    tblcod
096500030801     C**
096600030801     C     *LIKE         DEFINE    FVVNPG        WNPG
096700030801     C     *LIKE         DEFINE    BRVNFV        WNFV
096800030801     C     *LIKE         DEFINE    FVVNFV        WNFV5
096900030801     C     *LIKE         DEFINE    kpjbu         savkpjbu
097000030801     C**
097100030801     C                   MOVE      *ALL'-'       CMP132          198
097200030801     C                   MOVE      *ALL'-'       CMP198          198
097300030801     C*
097400030801     c     *dtaara       define    §azute        azuteds
097500030801     c     *dtaara       define    §datiute      ddatiute
097600030801     C                   in(E)     *dtaara
097700030801     C                   IF        %Error  or  RSUT = *blanks
097800030801     C                   call      'TIBS34R'
097900030801     C                   parm                    Tibs34Ds
098000030801     C                   in        *dtaara
098100030801     c                   ENDIF
098200030801     C*
098300030801     C                   TIME                    TIMES             6 0
098400030801     C                   TIME                    WHDAT            12 0
098500030801     C                   MOVE      WHDAT         WOGGI             6 0
098600030801     C                   TIME                    W0140            14 0
098700090226     C                   MOVE      W0140         G02DAT
098800090226     C                   MOVEL     *BLANKS       G02ERR
098900090226     C                   CALL      'XSRDA8'
099000090226     C                   PARM                    WLBDAT
099100090226     C                   MOVE      G02INV        OGGI              8 0
099200030801     C                   MOVE      W0140         WDTGIO            8 0
099300030801
099400030801      * Carico i p.o. del Terminal
099500030801     c                   eval      savkpjbu = kpjbu
099600030801     c                   Clear                   Trul06Ds
099700030801     c                   Eval      D06Cod = '£1'
099800030801     c                   Movel     Simfel        D06Key
099900030801     c                   Eval      D06Tla = 'L'
100000030801     c                   Movel(p)  Trul06Ds      Kpjbu
100100030801     c                   Call      'TRUL06R'
100200030801     c                   Parm                    Kpjba
100300030801     c                   Movel     Kpjbu         Trul06Ds
100400030801     c                   eval      kpjbu = savkpjbu
100500040419     C*  Carico dati tabella DTA anomalie danni
100600040419     C                   Z-ADD     0             X                 3 0
100700040420     C     'TAD'         setll     TNTBE01L
100800040420     C                   DO        *hival
100900040420     C     'TAD'         READE     TNTBE01L
101000040420     c                   if        %eof(tntbe01l)
101100040420     c                   leave
101200040420     c                   end
101300040419     C     TBESIF        IFEQ      KNSIF
101400040419     C     TBESIF        OREQ      *BLANKS
101500040419     C                   ADD       1             X
101600040419     C                   MOVEL     TBEKE1        TAD(X)
101700040419     C                   MOVEL     TBEUNI        DTA(X)
101800040420     c                   if        X = 100
101900040420     c                   leave
102000040420     c                   end
102100040420     c                   end
102200040420     C                   ENDdo
102300110905     c* reperisco i gg limite di attesa in caso di perfect order
102400110905     c                   eval      tbecod = 'XCO'
102500110905     c                   eval      tbeke1 = 'DTA'
102600110905     c                   clear                   tbeke2
102700110905     C     ktbe          chain     TNTBE01L
102800110905     c                   if        %found(tntbe01l)
102900110905     C                   MOVEL     TBeUNI        Dxcodta
103000110905     c                   else
103100110905     c                   clear                   dxcodta
103200110905     c                   end
103300050505     C*  Carico dati tabella PAR = partcolarità consegne NO PARZ.
103400141120ab   C*  Carico dati tabella PAR = partcolarità consegne con PINCODE
103500050505     C                   OPEN      TABEL00F
103600050505     C                   Z-ADD     0             X                 3 0
103700141120ab   C                   Z-ADD     0             Xpin              3 0
103800050505     C                   Z-ADD     1             tblkut
103900050505     C                   eval      tblcod = '7R'
104000050505     C     ktab          setll     Tabel00f
104100050505     C                   DO        *hival
104200050505     C     ktab          READE     TABEL00F
104300050505     c                   if        %eof(tabel00f)
104400050505     c                   leave
104500050505     c                   end
104600050505     C                   MOVEL     TBLUNI        DS7R
104700050505     c                   if        §7r8cp = 'N'
104800050505     C                   ADD       1             X
104900050505     C                   MOVEL     TBlkey        par(X)
105000050505     c                   end
105100141120      *
105200141120ab    * carica schiera con le Particolarità che hanno la gestione del PINCODE
105300141120ab   c                   if        §7RpinCODE = 'S'
105400141120ab   C                   ADD       1             Xpin
105500141120ab   C                   MOVEL     TBlkey        PARpin(Xpin)
105600141120ab   c                   end
105700141120      *
105800050505     C                   ENDdo
105900050505     C                   CLOSE     Tabel00f
106000030801     C*
106100030801     C                   ENDSR
106200040419**
106300040421C.A.aperta
106400040421C.A.chiusa
