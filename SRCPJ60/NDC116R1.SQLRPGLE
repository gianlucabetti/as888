000100981211     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO)                ACTGRP(*CALLER)
000200981211     H*PARMS BNDDIR(PJXBND PJCBND)
000300980120     H*PARMS DATFMT(*ISO) commit(*none) rdb(*none) dbgview(*source)
000400000601B0218H*PARMS DYNUSRPRF(*OWNER)
000500950615     H DECEDIT('0,') DATEDIT(*DMY.)
000600950615     H*----------------------------------------------------*
000700950615     H*
000800950615    >H*          STAMPA REGISTRAZIONI CONTABILI
000900950615     H*         --------------------------------
001000950316     H*
001100940830     F*----------------------------------------------------*
001200950508     FANMPE01W  IF   E           K DISK
001300971222    >FANSOG01L  IF   E           K DISK
001400971224    >FNDREG20L  IF   E           K DISK
001500950619    >FNDMOV01L  IF   E           K DISK
001600991029    >FNDPAS01L  IF   E           K DISK
001700991029    >FNdPpa01L  IF   E           K DISK
001800991029    >FNDPPS01L  IF   E           K DISK
001900991029    >FNDMOA01L  IF   E           K DISK
002000991029     FNDDES01L  IF   E           K DISK
002100991029     FANCDP01L  IF   E           K DISK
002200991029     FNdIva01L  IF   E           K DISK
002300050317C1776X*NdMoc02L  IF   E           K DISK
002400050317C1776FNdMo202L  IF   E           K DISK
002500991029     FNdBmv01L  IF   E           K DISK
002600050317C1776X*NdAna01L  IF   E           K DISK
002700050317C1776FNdAn201L  IF   E           K DISK
002800950621    >FANOPE01L  IF   E           K DISK
002900980313    >FNDC116P1  O    E             PRINTER OFLIND(*IN39)
003000950620     D*-------------
003100950710     D* Controllo autorizzazione contabilità
003200950620     D XCTBDS        E DS                  EXTNAME(XAUTCTBDS)
003300950620     D*-------------
003400950710     D* Controllo autorizzazione registrazioni
003500950620     D XREGDS        E DS                  EXTNAME(XAUTREGDS)
003600950620     D*-------------
003700950620     D* Maschera dati autorizzazioni 'CONSTA'
003800950620    >DCSTADS         E DS                  EXTNAME(CONSTADS)
003900950620     D*-------------
004000950620     D* Reperimento Autorizzazioni
004100950620     D AUTDS         E DS                  EXTNAME(XCHKAUTDS)
004200950620     D*-------------
004300950621     D* Decodifica conti
004400950621     D ND002DS       E DS
004500950621     D*-------------
004600991029     D* Richiamo a XATB
004700991029     D XATBDS        E DS
004800991029     D*-------------
004900991029     D* Parametri per ricerca codice ANATB00F
005000991029     D XTABDS        E DS
005100991029     D*-------------
005200991029     D* Tabella IVA
005300991029     D*X06G48         E DS                  EXTNAME(X06G48DS)
005400991029     DTABG48         E DS                  EXTNAME(ANGG48DS)
005500991029     D*-------------
005600991029     D rif             S              3    DIM(100)                             campi chiave letti
005700991029     D drif            S             20    DIM(100)                             campi chiave salvati
005800991029     D*-------------
005900980120     D LET             S             15    DIM(10)                              campi chiave letti
006000980120     D SAV             S             15    DIM(10)                              campi chiave salvati
006100950629     D LUN             S              3  0 DIM(10)                              campi chiave salvati
006200950629     D POS             S              5  0 DIM(10)                              campi chiave salvati
006300950629     D TIP             S              1    DIM(10)                              campi chiave salvati
006400950629     D RGC             S              3    DIM(10)                              campi chiave salvati
006500950629     D* Costante
006600950629     D FETCH           C                   CONST('FOR FETCH ONLY')
006700950620     D*-------------
006800950315    >D* Indicare le ds dei file che servono per sql
006900950315    >D*
007000950615    >D NDREG         E DS                  EXTNAME(NDREG00F)
007100950620     D*-------------
007200940830     D* Passaggio Parametri
007300940830     D KPJBA         E DS
007400940830     D*-------------
007500941129     D* Dati di ambiente ottenuti da XSOC
007600941129     D SOC001        E DS                  EXTNAME(XSOC001DS)
007700961010     D    XscDare                     1    overlay(xscFgo:7)                    Scadenz. retroattivo
007800961010     D    XscAvere                    1    overlay(xscFgo:8)                    Scadenz. retroattivo
007900941129     D*-------------
008000941129     D* DS Interna per dati di output di XSOC
008100941129     D XSOCDS          DS          1000
008200950315     D*-------------
008300950315     D* Parametri per reperimento dati campo
008400950315     D A967DS        E DS                  EXTNAME(ANA967DS)
008500950315     D  ERR967       E                     INZ('0')
008600950315     D*-------------
008700950315     D* Parametri per stampa prima pagina
008800950315     D A968DS        E DS                  EXTNAME(ANA968DS)
008900950315     D*-------------
009000950131     D* Parametri del PGM di reperimento schiera selezioni
009100950131     D A969DS        E DS                  EXTNAME(ANA969DS) INZ
009200950131     D  S969         E                     EXTFLD(SEL969)
009300950131     D                                     DIM(5000)                            parametri
009400950131     D  O969         E                     EXTFLD(ORD969)
009500950131     D                                     DIM(2000)                               "
009600950315     D*-------------
009700950203     D* Parametri per programma di controllo
009800950203     D A971DS        E DS                  EXTNAME(ANA971DS)
009900950315     D*-------------
010000950315     D* Parametri di lancio
010100950615    >D A977DS        E DS                  EXTNAME(ANA977DS) INZ
010200950315     D*-------------
010300950619     D* ds per ricerca campo pgm ANA962R
010400950315     D DAT962          DS          1024
010500950315     D*-------------
010600940830     D* Reperimento nome PGM
010700940830     D STATUS         SDS           333
010800940830     D  DSPGM            *PROC
010900950315     D*-------------
011000950619     D* Schiere selezione file
011100950316     D SKK             DS
011200950619    >D  SK                     1     80
011300950619     D                                     DIM(80) CTDATA PERRCD(80)            schiere
011400950316    >D                 DS
011500950316    >D  SK1                    1     80
011600950315     D                                     DIM(80) CTDATA PERRCD(80)            schiera 1 sql
011700950316    >D  SK1X                   1     80
011800950315     D*-------------
011900950131     D* Stringa per SQL
012000950131     D SQ              DS          5000
012100950131     D* Selezioni
012200950131     D SELX            DS
012300950131     D  SEL                    1   5000
012400950131     D                                     DIM(5000)                            parametri
012500950131     D* Ordinamenti
012600950131     D ORDX            DS
012700950131     D  ORD                    1   2000
012800950131     D                                     DIM(2000)                               "
012900950203     D* tracciato
013000950203     D                 DS
013100950203     D  TRC                    1    100
013200950203     D                                     DIM(10)                              tracciati  anreo00f
013300950203     D  TRCX                   1    100
013400950202     D* campi ordinamento
013500950202     D                 DS
013600950202     D  FLD                    1    100
013700950203     D                                     DIM(10)                              campi ord. anreo00f
013800950202     D  FLDX                   1    100
013900950202     D* tipi rottura
014000950202     D                 DS
014100950202     D  ROT                    1     10
014200950202     D                                     DIM(10)                              tipi rottura
014300950202     D  ROTX                   1     10
014400950206     D* descrizioni campi
014500950206     D                 DS
014600950206     D  TIT                    1    200
014700950206     D                                     DIM(10)                              titoli x totali
014800950206     D  TITX                   1    200
014900950316     D*
015000950316     D* Definire una ds per ogni rottura
015100950316     D*
015200950316     D* ROTTURA 1
015300950316    >D DS1             DS
015400950619    >D  dare1                  1     21  3
015500950619    >D  avere1                22     42  3
015600950316     D* ROTTURA 2
015700950316    >D DS2             DS
015800950619    >D  dare2                  1     21  3
015900950619    >D  avere2                22     42  3
016000950316     D* ROTTURA 3
016100950316    >D DS3             DS
016200950620    >D  dare3                  1     21  3
016300950619    >D  avere3                22     42  3
016400950316     D* ROTTURA 4
016500950316    >D DS4             DS
016600950620    >D  dare4                  1     21  3
016700950619    >D  avere4                22     42  3
016800950316     D* ROTTURA 5
016900950316    >D DS5             DS
017000950620    >D  dare5                  1     21  3
017100950619    >D  avere5                22     42  3
017200950316     D* ROTTURA 6
017300950316    >D DS6             DS
017400950620    >D  dare6                  1     21  3
017500950619    >D  avere6                22     42  3
017600950316     D* ROTTURA 7
017700950316    >D DS7             DS
017800950620    >D  dare7                  1     21  3
017900950619    >D  avere7                22     42  3
018000950316     D* ROTTURA 8
018100950316    >D DS8             DS
018200950620    >D  dare8                  1     21  3
018300950619    >D  avere8                22     42  3
018400950316     D* ROTTURA 9
018500950316    >D DS9             DS
018600950620    >D  dare9                  1     21  3
018700950619    >D  avere9                22     42  3
018800950316     D* ROTTURA 10
018900950316    >D DS10            DS
019000950620    >D  dare0                  1     21  3
019100950619    >D  avere0                22     42  3
019200950321     D* TOTALE GENERALE
019300950523    >D DSTG            DS
019400950620    >D  dareG                  1     21  3
019500950619    >D  avereG                22     42  3
019600950619     D*-------------
019700950620     Doggi             S               D    datfmt(*iso)
019800950621     D*-------------
019900950621     D* Definizione campi di lavoro
020000950621     D DARE            S                   like(MOVIMPORTO)
020100950621     D AVERE           S                   like(MOVIMPORTO)
020200950621     D* Definizioni parametri standard chiamate moduli
020300950621     D$esito           S              1
020400950621     D$gesmsg          S              1
020500950621     D$lenout          S              9B 0
020600950621     D$struttura       S             10
020700950621     D$date            S               D
020800950621     D$kcc             S              6
020900950621     D$ksc             S              8
021000950621     D$soc             S              3
021100991029      * Definizioni parametri driver soggetti
021200991029     DDvaric           S              1
021300991029     DDvaSocieta       S              3
021400991029     DDvaUnita         S              8
021500991029     DDvaDtRif         S               D
021600991029     DDvaStrutt        S             10
021700991029     DDvaLenOut        S              9B 0
021800991029     DDvaSnatura       S              1
021900991029     DDvaCodice        S              8
022000991029     DDvaLineav        S              3
022100991029     DDvaFiliale       S              3  0
022200991029     DDvaSogg          S              8
022300991029     DDvaTpInd         S              2
022400991029     DDvaCdInd         S              3
022500991029     DDvaOutput        S           4000
022600991029     DDvaErrore        S              1
022700991029      * DS esterna per dati soggetti
022800991029     D DVAsog        E DS                  INZ
022900991029      * DS esterna per dati fornitori
023000991029     D DVAfor        E DS                  INZ
023100991029      * DS esterna per dati clienti
023200991029     D DVAcli        E DS                  INZ
023300980115     D*
023400000118C1309D ZDateISO        S               D
023500950619     D*-------------
023600950616     D/EJECT
023700940830     C*----------------------------------------------------*
023800940830     C*                MAIN LINE PROGRAM
023900940830     C*----------------------------------------------------*
024000940830     C*
024100940830     C* inizializzazione variabili
024200940830     C                   EXSR      INZVAR
024300950131     C*
024400950131     C* Reperimento schiera per SQL
024500950131     C                   EXSR      REPSQL
024600941129     C*
024700950131     C* Esecuzione SQL
024800950131     C                   EXSR      EXESQL
024900940830     C*
025000940830     C* fine programma
025100940830     C                   EXSR      ENDPGM
025200940830     C*
025300940830     C/EJECT
025400940830     C************************************************************
025500940830     C* FINE PROGRAMMA
025600940830     C************************************************************
025700940830     C     ENDPGM        BEGSR
025800940830     C*
025900940830     C                   SETON                                            LR
026000940905     C                   RETURN
026100940830     C*
026200940830     C                   ENDSR
026300950127     C/EJECT
026400940830     C************************************************************
026500950131     C* REPERIMENTO SCHIERA PER SQL
026600940830     C************************************************************
026700950131     C     REPSQL        BEGSR
026800950131     C*
026900950315     C* Imposta la società di elaborazione del programma
027000950315     C*
027100950201     C     SQ            CAT       SKK:0         SQ
027200950215     C                   MOVE      MPESOC        WRKSOC            3
027300950316     C*
027400950316    >C* Controllare la posizione della società nella schiera SK1
027500950621    >C                   MOVEA     WRKSOC        SK1(14)
027600950316    >C     SQ            CAT       SK1X:1        SQ
027700950131     C*
027800990526D0074C* escludo annullati
027900990526  "  C                   Cat       'and':1       Sq
028000990526  "  C                   Cat       'RegAnn':1    Sq
028100990526  "  C                   Cat       '<>':0        Sq
028200990526  "  C                   Cat       '''1''':0     Sq
028300990526D0074C*
028400950131     C* Reperisco selezioni/ordinamenti
028500950131     C*
028600950215     C                   MOVE      MPESOC        SOC969
028700950208     C                   Z-ADD     MPEREG        NUM969
028800950508     C                   Z-ADD     LAN968        LAN969
028900950131     C                   CALL      'ANA969R'
029000950131     C                   PARM                    KPJBA
029100950131     C                   PARM                    A969DS
029200941129     C*
029300950131     C     ERR969        IFNE      *ON
029400950131     C                   MOVEA     S969          SEL
029500950131     C                   MOVEA     O969          ORD
029600950131     C* Se esistono altre selezioni aggiungo 'AND' per l'sql
029700950131     C     SELX          IFNE      *BLANKS
029800950131     C     SQ            CAT       'AND':1       SQ
029900950131     C                   ENDIF
030000950131     C*
030100950131     C     SQ            CAT       SELX:1        SQ
030200950131     C     SQ            CAT       ORDX:1        SQ
030300950629     C     SQ            CAT       FETCH:1       SQ
030400950131     C                   ENDIF
030500950202     C*
030600950206     C* Riempio schiera campi ordinamento, tipi rottura e titoli
030700950202     C*
030800950206     C                   MOVEL     TRC969        TRCX
030900950202     C                   MOVEL     FLD969        FLDX
031000950202     C                   MOVEL     ROT969        ROTX
031100950206     C                   MOVEL     DEB969        TITX
031200950629     C*
031300950629     C* Reperisco dal repository i dati dei campi delle rotture
031400950629     C* e li memorizzo nelle schiere
031500950629     C                   EXSR      REPREP
031600950629     C*
031700950201     C*
031800940905     C                   ENDSR
031900940905     C/EJECT
032000940905     C******************************************************
032100950131     C* ESECUZIONE SQL
032200940905     C******************************************************
032300950131     C     EXESQL        BEGSR
032400950131     C*
032500950131     C/EXEC SQL
032600950131     C+     PREPARE S1 FROM :SQ
032700950131     C/END-EXEC
032800950131     C*
032900950131     C/EXEC SQL
033000950131     C+     DECLARE A1 CURSOR FOR S1
033100950131     C/END-EXEC
033200950131     C*
033300950131     C/EXEC SQL
033400950131     C+     OPEN A1
033500950131     C/END-EXEC
033600030801     C*
033700030801D1414c                   move      *on           OkSqlCod          1
033800950131     C*
033900950201     C/EXEC SQL
034000950616    >C+     FETCH A1 INTO :NDREG
034100950201     C/END-EXEC
034200030801     C*
034300030801D1414c                   exsr      ImpSqlCod
034400950131     C*
034500030801D1414X***  SQLCOD        DOWEQ     0
034600030801D1414C     OkSqlCod      DOWEQ     *on
034700950620     C*
034800950202     C* Ricerca e salvataggio campi rottura
034900950206     C                   EXSR      DEFKEY
035000950202     C*
035100950202     C*  Operazioni a rottura di chiave 1
035200950202     C*
035300950202     C                   MOVE      LET(1)        SAV(1)
035400950202     C                   EXSR      ROTT01
035500950202     C*
035600950202   B2C     LET(1)        DOWEQ     SAV(1)
035700030801D1414x***  SQLCOD        ANDEQ     0
035800030801D1414C     OkSqlCod      andeq     *on
035900950202     C*
036000950202     C*  Operazioni a rottura di chiave 2
036100950202     C*
036200950202     C                   MOVE      LET(2)        SAV(2)
036300950202     C                   EXSR      ROTT02
036400950202     C*
036500950202   B3C     LET(2)        DOWEQ     SAV(2)
036600950202     C     LET(1)        ANDEQ     SAV(1)
036700030801D1414x***  SQLCOD        ANDEQ     0
036800030801D1414C     OkSqlCod      andeq     *on
036900950202     C*
037000950202     C*  Operazioni a rottura di chiave 3
037100950202     C*
037200950202     C                   MOVE      LET(3)        SAV(3)
037300950202     C                   EXSR      ROTT03
037400950202     C*
037500950202   B4C     LET(3)        DOWEQ     SAV(3)
037600950202     C     LET(2)        ANDEQ     SAV(2)
037700950202     C     LET(1)        ANDEQ     SAV(1)
037800030801D1414x***  SQLCOD        ANDEQ     0
037900030801D1414C     OkSqlCod      andeq     *on
038000950202     C*
038100950202     C*  Operazioni a rottura di chiave 4
038200950202     C*
038300950202     C                   MOVE      LET(4)        SAV(4)
038400950202     C                   EXSR      ROTT04
038500950202     C*
038600950202   B5C     LET(4)        DOWEQ     SAV(4)
038700950202     C     LET(3)        ANDEQ     SAV(3)
038800950202     C     LET(2)        ANDEQ     SAV(2)
038900950202     C     LET(1)        ANDEQ     SAV(1)
039000030801D1414x***  SQLCOD        ANDEQ     0
039100030801D1414C     OkSqlCod      andeq     *on
039200950202     C*
039300950202     C*  Operazioni a rottura di chiave 5
039400950202     C*
039500950202     C                   MOVE      LET(5)        SAV(5)
039600950202     C                   EXSR      ROTT05
039700950202     C*
039800950202   B6C     LET(5)        DOWEQ     SAV(5)
039900950202     C     LET(4)        ANDEQ     SAV(4)
040000950202     C     LET(3)        ANDEQ     SAV(3)
040100950202     C     LET(2)        ANDEQ     SAV(2)
040200950202     C     LET(1)        ANDEQ     SAV(1)
040300030801D1414x***  SQLCOD        ANDEQ     0
040400030801D1414C     OkSqlCod      andeq     *on
040500950202     C*
040600950202     C*  Operazioni a rottura di chiave 6
040700950202     C*
040800950202     C                   MOVE      LET(6)        SAV(6)
040900950202     C                   EXSR      ROTT06
041000950202     C*
041100950202   B7C     LET(6)        DOWEQ     SAV(6)
041200950202     C     LET(5)        ANDEQ     SAV(5)
041300950202     C     LET(4)        ANDEQ     SAV(4)
041400950202     C     LET(3)        ANDEQ     SAV(3)
041500950202     C     LET(2)        ANDEQ     SAV(2)
041600950202     C     LET(1)        ANDEQ     SAV(1)
041700030801D1414x***  SQLCOD        ANDEQ     0
041800030801D1414C     OkSqlCod      andeq     *on
041900950202     C*
042000950202     C*  Operazioni a rottura di chiave 7
042100950202     C*
042200950202     C                   MOVE      LET(7)        SAV(7)
042300950202     C                   EXSR      ROTT07
042400950202     C*
042500950202   B8C     LET(7)        DOWEQ     SAV(7)
042600950202     C     LET(6)        ANDEQ     SAV(6)
042700950202     C     LET(5)        ANDEQ     SAV(5)
042800950202     C     LET(4)        ANDEQ     SAV(4)
042900950202     C     LET(3)        ANDEQ     SAV(3)
043000950202     C     LET(2)        ANDEQ     SAV(2)
043100950202     C     LET(1)        ANDEQ     SAV(1)
043200030801D1414x***  SQLCOD        ANDEQ     0
043300030801D1414C     OkSqlCod      andeq     *on
043400950202     C*
043500950202     C*  Operazioni a rottura di chiave 8
043600950202     C*
043700950202     C                   MOVE      LET(8)        SAV(8)
043800950202     C                   EXSR      ROTT08
043900950202     C*
044000950202   B9C     LET(8)        DOWEQ     SAV(8)
044100950202     C     LET(7)        ANDEQ     SAV(7)
044200950202     C     LET(6)        ANDEQ     SAV(6)
044300950202     C     LET(5)        ANDEQ     SAV(5)
044400950202     C     LET(4)        ANDEQ     SAV(4)
044500950202     C     LET(3)        ANDEQ     SAV(3)
044600950202     C     LET(2)        ANDEQ     SAV(2)
044700950202     C     LET(1)        ANDEQ     SAV(1)
044800030801D1414x***  SQLCOD        ANDEQ     0
044900030801D1414C     OkSqlCod      andeq     *on
045000950202     C*
045100950202     C*  Operazioni a rottura di chiave 9
045200950202     C*
045300950202     C                   MOVE      LET(9)        SAV(9)
045400950202     C                   EXSR      ROTT09
045500950202     C*
045600950202  B10C     LET(9)        DOWEQ     SAV(9)
045700950202     C     LET(8)        ANDEQ     SAV(8)
045800950202     C     LET(7)        ANDEQ     SAV(7)
045900950202     C     LET(6)        ANDEQ     SAV(6)
046000950202     C     LET(5)        ANDEQ     SAV(5)
046100950202     C     LET(4)        ANDEQ     SAV(4)
046200950202     C     LET(3)        ANDEQ     SAV(3)
046300950202     C     LET(2)        ANDEQ     SAV(2)
046400950202     C     LET(1)        ANDEQ     SAV(1)
046500030801D1414x***  SQLCOD        ANDEQ     0
046600030801D1414C     OkSqlCod      andeq     *on
046700950202     C*
046800950202     C*  Operazioni a rottura di chiave 10
046900950202     C*
047000950202     C                   MOVE      LET(10)       SAV(10)
047100950202     C                   EXSR      ROTT10
047200950202     C*
047300950202  B11C     LET(10)       DOWEQ     SAV(10)
047400950202     C     LET(9)        ANDEQ     SAV(9)
047500950202     C     LET(8)        ANDEQ     SAV(8)
047600950202     C     LET(7)        ANDEQ     SAV(7)
047700950202     C     LET(6)        ANDEQ     SAV(6)
047800950202     C     LET(5)        ANDEQ     SAV(5)
047900950202     C     LET(4)        ANDEQ     SAV(4)
048000950202     C     LET(3)        ANDEQ     SAV(3)
048100950202     C     LET(2)        ANDEQ     SAV(2)
048200950202     C     LET(1)        ANDEQ     SAV(1)
048300030801D1414x***  SQLCOD        ANDEQ     0
048400030801D1414C     OkSqlCod      andeq     *on
048500950621     C*
048600950621     C* Controllo autorizzazioni
048700950621     C                   EXSR      CTRAUT
048800950131     C*
048900950623     C* Se autorizzato alla contabilità ....
049000950621     C     XACABL        IFEQ      '1'
049100950623     C* .... e alla registrazione
049200950621     C     XARABIL       ANDEQ     '1'
049300950621     C*
049400950202     C* Stampa dettaglio e calcoli per totali
049500950202     C                   EXSR      STADET
049600991230D0360C* E' stato eliminato questo ciclo perchè se le registrazioni
049700991230  "  C* collegate rientravano nei criteri di selezione, risultava
049800991230  "  C* stampata due volte la stessa registrazione (la reg. e la sua
049900991230  "  C* collegata)
050000991230  "  C***
050100991227  "  C*** Stampa Registrazione Collegata
050200991227  "  C***
050300991227  "   *** controlla registrazioni collegate se definitive ....
050400991227  "1 c***                if        regtpregz='D'
050500991227  "  C***                z-add     regnrasreg    REGNRASCOL
050600991227  "  C***  K02Reg01      Chain     Ndreg20l                           27
050700991227  "  C***
050800991227  "2 c***                Dow       *in27=*off
050900991227  "3 C***                If        *in27=*off and
051000991227  "  c***                          regtpregz='D'
051100991227  "  C*** Ricerca e salvataggio campi rottura
051200991227  "  C***                EXSR      DEFKEY
051300991227  "  C***
051400991227  "  C***                EXSR      STADET
051500991227  "  C***
051600991227  "3 c***                endif
051700991227  "  C***  K02Reg01      reade     Ndreg20l                               27
051800991227  "2 C***                ENDDo
051900991227D0360c***                endif
052000991029     C*
052100950621     C                   ENDIF
052200950621     C*
052300950201     C/EXEC SQL
052400950616    >C+     FETCH A1 INTO :NDREG
052500950201     C/END-EXEC
052600030801D1414c                   exsr      ImpSqlCod
052700950206     C                   EXSR      DEFKEY
052800950202     C*
052900950202  E11C                   ENDDO
053000950202     C*
053100950202     C* Spengo indicatore di  salto pagina
053200950202     C                   SETOFF                                       85
053300950202     C*
053400950202     C* Totalizzazione chiave 10 se richiesta
053500950202     C*
053600950202     C     ROT(10)       IFEQ      '2'
053700950202     C     ROT(10)       OREQ      '3'
053800950202     C                   EXSR      TOTT10
053900950202     C                   ENDIF
054000950202  E10C                   ENDDO
054100950202     C*
054200950202     C* Totalizzazione chiave  9 se richiesta
054300950202     C*
054400950202     C     ROT(9)        IFEQ      '2'
054500950202     C     ROT(9)        OREQ      '3'
054600950202     C                   EXSR      TOTT09
054700950202     C                   ENDIF
054800950202   E9C                   ENDDO
054900950202     C*
055000950202     C* Totalizzazione chiave  8 se richiesta
055100950202     C*
055200950202     C     ROT(8)        IFEQ      '2'
055300950202     C     ROT(8)        OREQ      '3'
055400950202     C                   EXSR      TOTT08
055500950202     C                   ENDIF
055600950202   E8C                   ENDDO
055700950202     C*
055800950202     C* Totalizzazione chiave  7 se richiesta
055900950202     C*
056000950202     C     ROT(7)        IFEQ      '2'
056100950202     C     ROT(7)        OREQ      '3'
056200950202     C                   EXSR      TOTT07
056300950202     C                   ENDIF
056400950202   E7C                   ENDDO
056500950202     C*
056600950202     C* Totalizzazione chiave  6 se richiesta
056700950202     C*
056800950202     C     ROT(6)        IFEQ      '2'
056900950202     C     ROT(6)        OREQ      '3'
057000950202     C                   EXSR      TOTT06
057100950202     C                   ENDIF
057200950202   E6C                   ENDDO
057300950202     C*
057400950202     C* Totalizzazione chiave  5 se richiesta
057500950202     C*
057600950202     C     ROT(5)        IFEQ      '2'
057700950202     C     ROT(5)        OREQ      '3'
057800950202     C                   EXSR      TOTT05
057900950202     C                   ENDIF
058000950202   E5C                   ENDDO
058100950202     C*
058200950202     C* Totalizzazione chiave  4 se richiesta
058300950202     C*
058400950202     C     ROT(4)        IFEQ      '2'
058500950202     C     ROT(4)        OREQ      '3'
058600950202     C                   EXSR      TOTT04
058700950202     C                   ENDIF
058800950202   E4C                   ENDDO
058900950202     C*
059000950202     C* Totalizzazione chiave  3 se richiesta
059100950202     C*
059200950202     C     ROT(3)        IFEQ      '2'
059300950202     C     ROT(3)        OREQ      '3'
059400950202     C                   EXSR      TOTT03
059500950202     C                   ENDIF
059600950202   E3C                   ENDDO
059700950202     C*
059800950202     C* Totalizzazione chiave  2 se richiesta
059900950202     C*
060000950202     C     ROT(2)        IFEQ      '2'
060100950202     C     ROT(2)        OREQ      '3'
060200950202     C                   EXSR      TOTT02
060300950202     C                   ENDIF
060400950202   E2C                   ENDDO
060500950202     C*
060600950202     C* Totalizzazione chiave  1 se richiesta
060700950202     C*
060800950202     C     ROT(1)        IFEQ      '2'
060900950202     C     ROT(1)        OREQ      '3'
061000950202     C                   EXSR      TOTT01
061100950202     C                   ENDIF
061200950202   E1C                   ENDDO
061300950202     C*
061400950202     C* Totale generale
061500950202     C*
061600950202     C                   EXSR      TOTGEN
061700950131     C*
061800950131     C/EXEC SQL
061900950131     C+     CLOSE A1
062000950131     C/END-EXEC
062100950131     C*
062200950131     C*
062300940905     C                   ENDSR
062400030801D1414C************************************************************
062500030801  "  C* Imposta Ok sqlcod dopo occur
062600030801D1414C************************************************************
062700030801     C     ImpSqlCod     BEGSR
062800030801     c*
062900030801     c                   select
063000030801     c* Errore sql eseguo cambio job per ottenere spool del joblog
063100030801     c                   when      sqlcod < 0
063200030801     c                   call      'X66CHGJOB'
063300030801     c                   eval      OkSqlCod = *off
063400030801     c* fine lettura
063500030801     c                   when      sqlcod = 100
063600030801     c                   eval      OkSqlCod = *off
063700030801     c* ok sql
063800030801     c                   other
063900030801     c                   eval      OkSqlCod = *on
064000030801     c*
064100030801     C                   endsl
064200030801     c*
064300030801D1414c                   endsr
064400940905     C/EJECT
064500950620     C************************************************************
064600950620     C* CONTROLLO AUTORIZZAZIONI
064700950620     C************************************************************
064800950620     C     CTRAUT        BEGSR
064900950620     C*
065000950620     C* Controllo autorizzazione contabilità
065100950620     C                   CLEAR                   XCTBDS
065200950620     C*
065300950704     C                   MOVE      KNMUS         XACPRF
065400950620     C                   MOVE      MPESOC        XACSOC
065500950620     C                   MOVE      REGCTB        XACCTB
065600950620     C                   CALL      'XAUTCTB'
065700950620     C                   PARM                    XCTBDS
065800950620     C*
065900950621     C* Se abilitato alla contabilità....
066000950620     C     XACABL        IFEQ      '1'
066100950620     C*
066200950621     C* ...controllo autorizzazione alla registrazione
066300950620     C                   CLEAR                   XREGDS
066400950620     C*
066500950620     C                   MOVE      MPESOC        XARSOCIETA
066600950620     C                   MOVE      '2'           XARAMBITO
066700950620     C                   MOVE      XSCCUT        XARUTENTER
066800950620     C                   MOVE      XSCGRP        XARGRUPPOR
066900950620     C                   MOVE      REGUTEULMO    XARUTENTEP
067000950620     C                   MOVE      REGLIVRISE    XARLIVRENT
067100950620     C                   MOVE      RITREG        XARAUTTOT
067200950620     C                   MOVE      RIGREG        XARAUTGRU
067300950620     C                   MOVE      RIPREG        XARAUTPRF
067400950620     C                   MOVE      RILREG        XARAUTLIS
067500950620     C                   MOVE      LVRREG        XARAUTLIVR
067600950620     C                   CALL      'XAUTREG'
067700950620     C                   PARM                    XREGDS
067800950620     C*
067900950620     C                   ENDIF
068000950620     C*
068100950620     C                   ENDSR
068200950620     C/EJECT
068300950202     C************************************************************
068400950202     C* REPERIMENTO E SALVATAGGIO CAMPI CHIAVE
068500950202     C************************************************************
068600950206     C     DEFKEY        BEGSR
068700950202     C*
068800950202     C* Memorizza campi chiave letti in campi della schiera LET
068900950202     C*
069000950629   B1C                   DO        10            $X                3 0
069100950202     C*
069200950206   B2C     FLD($X)       IFNE      *BLANKS
069300950629     C*
069400950629   B3C     LUN($X)       IFNE      0
069500950629     C     POS($X)       ANDNE     0
069600950629     C     TIP($X)       ANDNE     *BLANKS
069700950316     C*
069800950316     C* Chiamo pgm per estrarre campo da record (valore del campo)
069900950316     C*
070000950203     C                   SELECT
070100950616    >C     TRC($X)       WHENEQ    'NDREG000'
070200950202     C                   CALL      'ANA962R'
070300950619    >C                   PARM      NDREG         DAT962
070400950202     C                   PARM                    FLD962           30
070500950629     C                   PARM      LUN($X)       LUN962            3 0
070600950629     C                   PARM      POS($X)       POS962            5 0
070700950629     C                   PARM      TIP($X)       TIP962            1
070800950202     C                   MOVEL     FLD962        LET($X)
070900950203     C                   ENDSL
071000950202     C*
071100950206   E3C                   ENDIF
071200950203   E2C                   ENDIF
071300950203   E1C                   ENDDO
071400950202     C*
071500950202     C                   ENDSR
071600950629     C/EJECT
071700950629     C************************************************************
071800950629     C* REPERIMENTO dati campi delle rotture
071900950629     C************************************************************
072000950629     C     REPREP        BEGSR
072100950629     C*
072200950629     C* Reperisce dal repository la lunghezza e il tipo dei campi di rottura
072300950629     C*
072400950629   B1C                   DO        10            $X                3 0
072500950629     C*
072600950629   B2C     FLD($X)       IFNE      *BLANKS
072700950629     C*
072800950629     C                   RESET                   A967DS
072900950629     C                   MOVE      TRC($X)       TRC967
073000950629     C                   MOVE      FLD($X)       FLD967
073100950629     C                   MOVEL     A967DS        KPJBU
073200950629     C                   CALL      'ANA967R'
073300950629     C                   PARM                    KPJBA
073400950629     C                   MOVEL     KPJBU         A967DS
073500950629     C*
073600950629   B3C     ERR967        IFEQ      *OFF
073700950629     C*
073800950629     C                   Z-ADD     LUN967        LUN($X)
073900950629     C                   Z-ADD     POS967        POS($X)
074000950629     C                   MOVE      TIP967        TIP($X)
074100950629     C                   MOVE      RGC967        RGC($X)
074200950629     C*
074300950629   E3C                   ENDIF
074400950629   E2C                   ENDIF
074500950629   E1C                   ENDDO
074600950629     C                   ENDSR
074700950210     C/EJECT
074800950210     C********************************************************************
074900950210     C/SPACE 5
075000950210     C********************************************************************
075100950210     C* Stampa testata e gestione overflow
075200950210     C*
075300950210     C     TESTAT        BEGSR
075400950615     C*
075500950315     C                   WRITE     TESTA
075600950210     C*
075700950210     C* Spengo se acceso indicatore di overflow
075800950210     C*
075900950210     C   39              SETOFF                                       39
076000950210     C*
076100950210     C                   ENDSR
076200950202     C********************************************************************
076300950202     C/SPACE 5
076400950202     C********************************************************************
076500950202     C* Operazioni a rottura della chiave 1
076600950202     C*
076700950202     C     ROTT01        BEGSR
076800950202     C*
076900950315     C                   CLEAR                   DS1
077000950202     C*
077100950202     C* Utilizzo questo flags per sapere se ho rotto la chiave e di
077200950202     C* conseguenza non stampare la testata del salto pagina.
077300950202     C*
077400950203     C                   MOVE      'R'           ROT01             1
077500950202     C*
077600950202     C* Se non chiesto salto pagina della prima chiave stampo primo titolo
077700950202     C*
077800950202     C     ROT(1)        IFNE      '1'
077900950202     C     ROT(1)        ANDNE     '3'
078000950202     C     WRKFLG        IFEQ      ' '
078100950210     C                   EXSR      TESTAT
078200950202     C                   MOVE      '1'           WRKFLG            1
078300950202     C                   SETON                                        85
078400950202     C                   ENDIF
078500950202     C                   ENDIF
078600950202     C*
078700950210     C* Salto pagina se richiesto o se overflow
078800950202     C*
078900950202     C     ROT(1)        IFEQ      '1'
079000950202     C     ROT(1)        OREQ      '3'
079100950210     C     *IN39         OREQ      *ON
079200950615     C*
079300950210     C     *IN85         IFEQ      *OFF
079400950210     C     *IN39         OREQ      *ON
079500950210     C                   EXSR      TESTAT
079600950202     C                   SETON                                        85
079700950207     C                   ENDIF
079800950210     C*
079900950210     C                   ENDIF
080000950202     C*
080100950207     C* Titolo se richiesto
080200950202     C*
080300950207     C     ROT(1)        IFNE      '0'
080400950208     C     ROT(1)        ANDNE     ' '
080500950203     C                   CLEAR                   DESL1
080600950202     C                   MOVEL     TIT(1)        L1TITD
080700950202     C                   MOVEL     SAV(1)        WRKKEY           15
080800950202     C                   MOVEL     WRKKEY        L1KEYD
080900950202     C*
081000950202     C*  Eseguo decodifica titoli
081100950202     C*
081200950203     C                   MOVEL     TRC(1)        WRKTRC
081300950203     C                   MOVEL     FLD(1)        WRKFLD
081400950629     C                   MOVEL     RGC(1)        WRKRGC
081500950206     C                   EXSR      DECTIT
081600950202     C                   MOVEL     WRKDES        L1DESD
081700950202     C*
081800950206     C     TIT(1)        IFNE      *BLANKS
081900950203     C                   WRITE     DESL1
082000950206     C                   ENDIF
082100950207     C                   ENDIF
082200950202     C*
082300950202     C                   ENDSR
082400950202     C/EJECT
082500950202     C********************************************************************
082600950202     C/SPACE 5
082700950202     C********************************************************************
082800950202     C* Operazioni a rottura della chiave 2
082900950202     C*
083000950202     C     ROTT02        BEGSR
083100950202     C*
083200950315     C                   CLEAR                   DS2
083300950202     C*
083400950202     C* Utilizzo questo flags per sapere se ho rotto la chiave e di
083500950202     C* conseguenza non stampare la testata del salto pagina.
083600950202     C*
083700950203     C                   MOVE      'R'           ROT02             1
083800950202     C*
083900950210     C* Salto pagina se richiesto o se overflow
084000950202     C*
084100950202     C     ROT(2)        IFEQ      '1'
084200950202     C     ROT(2)        OREQ      '3'
084300950210     C     *IN39         OREQ      *ON
084400950207     C*
084500950210     C     *IN85         IFEQ      *OFF
084600950210     C     *IN39         OREQ      *ON
084700950210     C                   EXSR      TESTAT
084800950202     C                   SETON                                        85
084900950207     C                   ENDIF
085000950207     C                   ENDIF
085100950207     C*
085200950207     C* Titolo se richiesto
085300950207     C*
085400950207     C     ROT(2)        IFNE      '0'
085500950208     C     ROT(2)        ANDNE     ' '
085600950203     C                   CLEAR                   DESL2
085700950202     C                   MOVEL     TIT(2)        L2TITD
085800950202     C                   MOVEL     SAV(2)        WRKKEY           15
085900950202     C                   MOVEL     WRKKEY        L2KEYD
086000950202     C*
086100950202     C*  Eseguo decodifica titoli
086200950203     C                   MOVEL     TRC(2)        WRKTRC
086300950203     C                   MOVEL     FLD(2)        WRKFLD
086400950629     C                   MOVEL     RGC(2)        WRKRGC
086500950206     C                   EXSR      DECTIT
086600950202     C                   MOVEL     WRKDES        L2DESD
086700950203     C*
086800950209     C* Stampo riga vuota se stampati i totali precedenti, ma non l'int
086900950209     C* estazione precedente
087000950209     C     ROT(2)        IFEQ      '2'
087100950209     C     ROT01         ANDNE     'R'
087200950209     C                   WRITE     VUOTA
087300950209     C                   ENDIF
087400950206     C     TIT(2)        IFNE      *BLANKS
087500950203     C                   WRITE     DESL2
087600950206     C                   ENDIF
087700950206     C                   ENDIF
087800950202     C*
087900950202     C                   ENDSR
088000950202     C********************************************************************
088100950202     C/SPACE 5
088200950202     C********************************************************************
088300950202     C* Operazioni a rottura della chiave 3
088400950202     C*
088500950202     C     ROTT03        BEGSR
088600950202     C*
088700950315     C                   CLEAR                   DS3
088800950202     C*
088900950202     C* Utilizzo questo flags per sapere se ho rotto la chiave e di
089000950202     C* conseguenza non stampare la testata del salto pagina.
089100950202     C*
089200950203     C                   MOVE      'R'           ROT03             1
089300950202     C*
089400950210     C* Salto pagina se richiesto o se overflow
089500950202     C*
089600950202     C     ROT(3)        IFEQ      '1'
089700950202     C     ROT(3)        OREQ      '3'
089800950210     C     *IN39         OREQ      *ON
089900950207     C*
090000950210     C     *IN85         IFEQ      *OFF
090100950210     C     *IN39         OREQ      *ON
090200950210     C                   EXSR      TESTAT
090300950202     C                   SETON                                        85
090400950207     C                   ENDIF
090500950210     C*
090600950207     C                   ENDIF
090700950207     C*
090800950207     C* Titolo se richiesto
090900950207     C*
091000950207     C     ROT(3)        IFNE      '0'
091100950208     C     ROT(3)        ANDNE     ' '
091200950203     C                   CLEAR                   DESL3
091300950206     C                   MOVEL     TIT(3)        L3TITD
091400950202     C                   MOVEL     SAV(3)        WRKKEY           15
091500950208     C                   MOVEL     WRKKEY        L3KEYD
091600950202     C*
091700950202     C*  Eseguo decodifica titoli
091800950202     C*
091900950203     C                   MOVEL     TRC(3)        WRKTRC
092000950203     C                   MOVEL     FLD(3)        WRKFLD
092100950629     C                   MOVEL     RGC(3)        WRKRGC
092200950206     C                   EXSR      DECTIT
092300950202     C                   MOVEL     WRKDES        L3DESD
092400950206     C*
092500950209     C* Stampo riga vuota se stampati i totali precedenti, ma non l'int
092600950209     C* estazione precedente
092700950209     C     ROT(3)        IFEQ      '2'
092800950209     C     ROT02         ANDNE     'R'
092900950209     C                   WRITE     VUOTA
093000950209     C                   ENDIF
093100950206     C     TIT(3)        IFNE      *BLANKS
093200950203     C                   WRITE     DESL3
093300950206     C                   ENDIF
093400950206     C                   ENDIF
093500950202     C*
093600950202     C                   ENDSR
093700950202     C********************************************************************
093800950202     C/SPACE 5
093900950202     C********************************************************************
094000950202     C* Operazioni a rottura della chiave 4
094100950202     C*
094200950202     C     ROTT04        BEGSR
094300950202     C*
094400950315     C                   CLEAR                   DS4
094500950202     C*
094600950202     C* Utilizzo questo flags per sapere se ho rotto la chiave e di
094700950202     C* conseguenza non stampare la testata del salto pagina.
094800950202     C*
094900950203     C                   MOVE      'R'           ROT04             1
095000950202     C*
095100950202     C* Salto pagina se richiesto
095200950202     C*
095300950202     C     ROT(4)        IFEQ      '1'
095400950202     C     ROT(4)        OREQ      '3'
095500950210     C     *IN39         OREQ      *ON
095600950207     C*
095700950210     C     *IN85         IFEQ      *OFF
095800950210     C     *IN39         OREQ      *ON
095900950210     C                   EXSR      TESTAT
096000950202     C                   SETON                                        85
096100950207     C                   ENDIF
096200950207     C                   ENDIF
096300950207     C*
096400950207     C* Titolo se richiesto
096500950207     C*
096600950207     C     ROT(4)        IFNE      '0'
096700950208     C     ROT(4)        ANDNE     ' '
096800950203     C                   CLEAR                   DESL4
096900950206     C                   MOVEL     TIT(4)        L4TITD
097000950202     C                   MOVEL     SAV(4)        WRKKEY           15
097100950208     C                   MOVEL     WRKKEY        L4KEYD
097200950202     C*
097300950202     C*  Eseguo decodifica titoli
097400950202     C*
097500950203     C                   MOVEL     TRC(4)        WRKTRC
097600950203     C                   MOVEL     FLD(4)        WRKFLD
097700950629     C                   MOVEL     RGC(4)        WRKRGC
097800950206     C                   EXSR      DECTIT
097900950202     C                   MOVEL     WRKDES        L4DESD
098000950202     C*
098100950209     C* Stampo riga vuota se stampati i totali precedenti, ma non l'int
098200950209     C* estazione precedente
098300950209     C     ROT(4)        IFEQ      '2'
098400950209     C     ROT03         ANDNE     'R'
098500950209     C                   WRITE     VUOTA
098600950209     C                   ENDIF
098700950206     C     TIT(4)        IFNE      *BLANKS
098800950203     C                   WRITE     DESL4
098900950206     C                   ENDIF
099000950206     C                   ENDIF
099100950202     C*
099200950202     C                   ENDSR
099300950202     C********************************************************************
099400950202     C/SPACE 5
099500950202     C********************************************************************
099600950202     C* Operazioni a rottura della chiave 1
099700950202     C*
099800950202     C     ROTT05        BEGSR
099900950202     C*
100000950315     C                   CLEAR                   DS5
100100950202     C*
100200950202     C* Utilizzo questo flags per sapere se ho rotto la chiave e di
100300950202     C* conseguenza non stampare la testata del salto pagina.
100400950202     C*
100500950203     C                   MOVE      'R'           ROT05             1
100600950202     C*
100700950210     C* Salto pagina se richiesto o se overflow
100800950202     C*
100900950202     C     ROT(5)        IFEQ      '1'
101000950202     C     ROT(5)        OREQ      '3'
101100950210     C     *IN39         OREQ      *ON
101200950207     C*
101300950210     C     *IN85         IFEQ      *OFF
101400950210     C     *IN39         OREQ      *ON
101500950210     C                   EXSR      TESTAT
101600950202     C                   SETON                                        85
101700950207     C                   ENDIF
101800950207     C                   ENDIF
101900950207     C*
102000950207     C* Titolo se richiesto
102100950207     C*
102200950207     C     ROT(5)        IFNE      '0'
102300950208     C     ROT(5)        ANDNE     ' '
102400950203     C                   CLEAR                   DESL5
102500950202     C                   MOVEL     TIT(5)        L5TITD
102600950202     C                   MOVEL     SAV(5)        WRKKEY           15
102700950202     C                   MOVEL     WRKKEY        L5KEYD
102800950202     C*
102900950202     C*  Eseguo decodifica titoli
103000950202     C*
103100950203     C                   MOVEL     TRC(5)        WRKTRC
103200950203     C                   MOVEL     FLD(5)        WRKFLD
103300950629     C                   MOVEL     RGC(5)        WRKRGC
103400950206     C                   EXSR      DECTIT
103500950202     C                   MOVEL     WRKDES        L5DESD
103600950209     C*
103700950209     C* Stampo riga vuota se stampati i totali precedenti, ma non l'int
103800950209     C* estazione precedente
103900950209     C     ROT(5)        IFEQ      '2'
104000950209     C     ROT04         ANDNE     'R'
104100950209     C                   WRITE     VUOTA
104200950209     C                   ENDIF
104300950206     C*
104400950206     C     TIT(5)        IFNE      *BLANKS
104500950203     C                   WRITE     DESL5
104600950206     C                   ENDIF
104700950206     C                   ENDIF
104800950202     C*
104900950202     C                   ENDSR
105000950202     C********************************************************************
105100950202     C/SPACE 5
105200950202     C********************************************************************
105300950202     C* Operazioni a rottura della chiave 6
105400950202     C*
105500950202     C     ROTT06        BEGSR
105600950202     C*
105700950315     C                   CLEAR                   DS6
105800950202     C*
105900950202     C* Utilizzo questo flags per sapere se ho rotto la chiave e di
106000950202     C* conseguenza non stampare la testata del salto pagina.
106100950202     C*
106200950203     C                   MOVE      'R'           ROT06             1
106300950202     C*
106400950210     C* Salto pagina se richiesto o se overflow
106500950202     C*
106600950202     C     ROT(6)        IFEQ      '1'
106700950202     C     ROT(6)        OREQ      '3'
106800950210     C     *IN39         OREQ      *ON
106900950207     C*
107000950210     C     *IN85         IFEQ      *OFF
107100950210     C     *IN39         OREQ      *ON
107200950210     C                   EXSR      TESTAT
107300950202     C                   SETON                                        85
107400950207     C                   ENDIF
107500950207     C                   ENDIF
107600950207     C*
107700950207     C* Titolo se richiesto
107800950207     C*
107900950207     C     ROT(6)        IFNE      '0'
108000950208     C     ROT(6)        ANDNE     ' '
108100950203     C                   CLEAR                   DESL6
108200950202     C                   MOVEL     TIT(6)        L6TITD
108300950202     C                   MOVEL     SAV(6)        WRKKEY           15
108400950202     C                   MOVEL     WRKKEY        L6KEYD
108500950202     C*
108600950202     C*  Eseguo decodifica titoli
108700950202     C*
108800950203     C                   MOVEL     TRC(6)        WRKTRC
108900950203     C                   MOVEL     FLD(6)        WRKFLD
109000950629     C                   MOVEL     RGC(6)        WRKRGC
109100950206     C                   EXSR      DECTIT
109200950202     C                   MOVEL     WRKDES        L6DESD
109300950209     C*
109400950209     C* Stampo riga vuota se stampati i totali precedenti, ma non l'int
109500950209     C* estazione precedente
109600950209     C     ROT(6)        IFEQ      '2'
109700950209     C     ROT05         ANDNE     'R'
109800950209     C                   WRITE     VUOTA
109900950209     C                   ENDIF
110000950206     C*
110100950206     C     TIT(6)        IFNE      *BLANKS
110200950203     C                   WRITE     DESL6
110300950206     C                   ENDIF
110400950206     C                   ENDIF
110500950202     C*
110600950202     C                   ENDSR
110700950202     C********************************************************************
110800950202     C/SPACE 5
110900950202     C********************************************************************
111000950202     C* Operazioni a rottura della chiave 1
111100950202     C*
111200950202     C     ROTT07        BEGSR
111300950202     C*
111400950315     C                   CLEAR                   DS7
111500950202     C*
111600950202     C* Utilizzo questo flags per sapere se ho rotto la chiave e di
111700950202     C* conseguenza non stampare la testata del salto pagina.
111800950202     C*
111900950203     C                   MOVE      'R'           ROT07             1
112000950202     C*
112100950210     C* Salto pagina se richiesto o se overflow
112200950202     C*
112300950202     C     ROT(7)        IFEQ      '1'
112400950202     C     ROT(7)        OREQ      '3'
112500950210     C     *IN39         OREQ      *ON
112600950207     C*
112700950210     C     *IN85         IFEQ      *OFF
112800950210     C     *IN39         OREQ      *ON
112900950210     C                   EXSR      TESTAT
113000950202     C                   SETON                                        85
113100950207     C                   ENDIF
113200950207     C                   ENDIF
113300950207     C*
113400950207     C* Titolo se richiesto
113500950207     C*
113600950207     C     ROT(7)        IFNE      '0'
113700950208     C     ROT(7)        ANDNE     ' '
113800950203     C                   CLEAR                   DESL7
113900950202     C                   MOVEL     TIT(7)        L7TITD
114000950202     C                   MOVEL     SAV(7)        WRKKEY           15
114100950202     C                   MOVEL     WRKKEY        L7KEYD
114200950202     C*
114300950202     C*  Eseguo decodifica titoli
114400950202     C*
114500950203     C                   MOVEL     TRC(7)        WRKTRC
114600950203     C                   MOVEL     FLD(7)        WRKFLD
114700950629     C                   MOVEL     RGC(7)        WRKRGC
114800950206     C                   EXSR      DECTIT
114900950202     C                   MOVEL     WRKDES        L7DESD
115000950209     C*
115100950209     C* Stampo riga vuota se stampati i totali precedenti, ma non l'int
115200950209     C* estazione precedente
115300950209     C     ROT(7)        IFEQ      '2'
115400950209     C     ROT06         ANDNE     'R'
115500950209     C                   WRITE     VUOTA
115600950209     C                   ENDIF
115700950206     C     TIT(7)        IFNE      *BLANKS
115800950203     C                   WRITE     DESL7
115900950206     C                   ENDIF
116000950206     C                   ENDIF
116100950202     C*
116200950202     C                   ENDSR
116300950202     C********************************************************************
116400950202     C/SPACE 5
116500950202     C********************************************************************
116600950202     C* Operazioni a rottura della chiave 8
116700950202     C*
116800950202     C     ROTT08        BEGSR
116900950202     C*
117000950315     C                   CLEAR                   DS8
117100950202     C*
117200950202     C* Utilizzo questo flags per sapere se ho rotto la chiave e di
117300950202     C* conseguenza non stampare la testata del salto pagina.
117400950202     C*
117500950203     C                   MOVE      'R'           ROT08             1
117600950202     C*
117700950210     C* Salto pagina se richiesto o se overflow
117800950202     C*
117900950202     C     ROT(8)        IFEQ      '1'
118000950202     C     ROT(8)        OREQ      '3'
118100950210     C     *IN39         OREQ      *ON
118200950207     C*
118300950210     C     *IN85         IFEQ      *OFF
118400950210     C     *IN39         OREQ      *ON
118500950210     C                   EXSR      TESTAT
118600950202     C                   SETON                                        85
118700950207     C                   ENDIF
118800950207     C                   ENDIF
118900950207     C*
119000950207     C* Titolo se richiesto
119100950207     C*
119200950207     C     ROT(8)        IFNE      '0'
119300950208     C     ROT(8)        ANDNE     ' '
119400950203     C                   CLEAR                   DESL8
119500950202     C                   MOVEL     TIT(8)        L8TITD
119600950202     C                   MOVEL     SAV(8)        WRKKEY           15
119700950202     C                   MOVEL     WRKKEY        L8KEYD
119800950202     C*
119900950202     C*  Eseguo decodifica titoli
120000950202     C*
120100950203     C                   MOVEL     TRC(8)        WRKTRC
120200950203     C                   MOVEL     FLD(8)        WRKFLD
120300950629     C                   MOVEL     RGC(8)        WRKRGC
120400950206     C                   EXSR      DECTIT
120500950202     C                   MOVEL     WRKDES        L8DESD
120600950209     C*
120700950209     C* Stampo riga vuota se stampati i totali precedenti, ma non l'int
120800950209     C* estazione precedente
120900950209     C     ROT(8)        IFEQ      '2'
121000950209     C     ROT07         ANDNE     'R'
121100950209     C                   WRITE     VUOTA
121200950209     C                   ENDIF
121300950206     C     TIT(8)        IFNE      *BLANKS
121400950203     C                   WRITE     DESL8
121500950206     C                   ENDIF
121600950206     C                   ENDIF
121700950202     C*
121800950202     C                   ENDSR
121900950202     C********************************************************************
122000950202     C/SPACE 5
122100950202     C********************************************************************
122200950202     C* Operazioni a rottura della chiave 9
122300950202     C*
122400950202     C     ROTT09        BEGSR
122500950202     C*
122600950315     C                   CLEAR                   DS9
122700950202     C*
122800950202     C* Utilizzo questo flags per sapere se ho rotto la chiave e di
122900950202     C* conseguenza non stampare la testata del salto pagina.
123000950202     C*
123100950203     C                   MOVE      'R'           ROT09             1
123200950202     C*
123300950210     C* Salto pagina se richiesto o se overflow
123400950202     C*
123500950202     C     ROT(9)        IFEQ      '1'
123600950202     C     ROT(9)        OREQ      '3'
123700950210     C     *IN39         OREQ      *ON
123800950207     C*
123900950210     C     *IN85         IFEQ      *OFF
124000950210     C     *IN39         OREQ      *ON
124100950210     C                   EXSR      TESTAT
124200950202     C                   SETON                                        85
124300950207     C                   ENDIF
124400950207     C                   ENDIF
124500950207     C*
124600950207     C* Titolo se richiesto
124700950207     C*
124800950207     C     ROT(9)        IFNE      '0'
124900950208     C     ROT(9)        ANDNE     ' '
125000950203     C                   CLEAR                   DESL9
125100950206     C                   MOVEL     TIT(9)        L9TITD
125200950202     C                   MOVEL     SAV(9)        WRKKEY           15
125300950208     C                   MOVEL     WRKKEY        L9KEYD
125400950202     C*
125500950202     C*  Eseguo decodifica titoli
125600950202     C*
125700950203     C                   MOVEL     TRC(9)        WRKTRC
125800950203     C                   MOVEL     FLD(9)        WRKFLD
125900950629     C                   MOVEL     RGC(9)        WRKRGC
126000950206     C                   EXSR      DECTIT
126100950208     C                   MOVEL     WRKDES        L9DESD
126200950209     C*
126300950209     C* Stampo riga vuota se stampati i totali precedenti, ma non l'int
126400950209     C* estazione precedente
126500950209     C     ROT(9)        IFEQ      '2'
126600950209     C     ROT08         ANDNE     'R'
126700950209     C                   WRITE     VUOTA
126800950209     C                   ENDIF
126900950206     C     TIT(9)        IFNE      *BLANKS
127000950203     C                   WRITE     DESL9
127100950206     C                   ENDIF
127200950206     C                   ENDIF
127300950202     C*
127400950202     C                   ENDSR
127500950202     C/EJECT
127600950202     C********************************************************************
127700950202     C/SPACE 5
127800950202     C********************************************************************
127900950202     C* Operazioni a rottura della chiave 10
128000950202     C*
128100950202     C     ROTT10        BEGSR
128200950202     C*
128300950315     C                   CLEAR                   DS10
128400950202     C*
128500950202     C* Utilizzo questo flags per sapere se ho rotto la chiave e di
128600950202     C* conseguenza non stampare la testata del salto pagina.
128700950202     C*
128800950203     C                   MOVE      'R'           ROT10             1
128900950202     C*
129000950210     C* Salto pagina se richiesto o se overflow
129100950202     C*
129200950202     C     ROT(10)       IFEQ      '1'
129300950202     C     ROT(10)       OREQ      '3'
129400950210     C     *IN39         OREQ      *ON
129500950207     C*
129600950210     C     *IN85         IFEQ      *OFF
129700950210     C     *IN39         OREQ      *ON
129800950210     C                   EXSR      TESTAT
129900950202     C                   SETON                                        85
130000950207     C                   ENDIF
130100950207     C                   ENDIF
130200950207     C*
130300950207     C* Titolo se richiesto
130400950207     C*
130500950207     C     ROT(10)       IFNE      '0'
130600950208     C     ROT(10)       ANDNE     ' '
130700950203     C                   CLEAR                   DESL0
130800950202     C                   MOVEL     TIT(10)       L0TITD
130900950202     C                   MOVEL     SAV(10)       WRKKEY           15
131000950202     C                   MOVEL     WRKKEY        L0KEYD
131100950202     C*
131200950202     C*  Eseguo decodifica titoli
131300950202     C*
131400950203     C                   MOVEL     TRC(10)       WRKTRC
131500950203     C                   MOVEL     FLD(10)       WRKFLD
131600950629     C                   MOVEL     RGC(10)       WRKRGC
131700950206     C                   EXSR      DECTIT
131800950202     C                   MOVEL     WRKDES        L0DESD
131900950209     C*
132000950209     C* Stampo riga vuota se stampati i totali precedenti, ma non l'int
132100950209     C* estazione precedente
132200950209     C     ROT(10)       IFEQ      '2'
132300950209     C     ROT09         ANDNE     'R'
132400950209     C                   WRITE     VUOTA
132500950209     C                   ENDIF
132600950206     C     TIT(10)       IFNE      *BLANKS
132700950203     C                   WRITE     DESL0
132800950206     C                   ENDIF
132900950206     C                   ENDIF
133000950207     C*
133100950207     C* Titoli righe campi (da stampare solo se effettuato il salto
133200950209     C* pagina e se chiesto stampa dettaglio si)
133300950207     C     *IN85         IFEQ      *ON
133400950622     C                   WRITE     TEST2
133500950207     C                   ENDIF
133600950202     C*
133700950202     C                   ENDSR
133800950202     C/EJECT
133900950202     C********************************************************************
134000950202     C/SPACE 5
134100950202     C********************************************************************
134200950202     C* Totalizzazioni chiave 10
134300950202     C*
134400950202     C     TOTT10        BEGSR
134500950210     C*Oveflow
134600950210     C   39              EXSR      TESTAT
134700950202     C*
134800950202     C                   MOVEL     SAV(10)       WRKKEY
134900950202     C                   MOVEL     WRKKEY        T0KEYD
135000950202     C*
135100950202     C*  Eseguo decodifica titoli
135200950202     C*
135300950203     C                   MOVEL     TRC(10)       WRKTRC
135400950203     C                   MOVEL     FLD(10)       WRKFLD
135500950629     C                   MOVEL     RGC(10)       WRKRGC
135600950206     C                   EXSR      DECTIT
135700950202     C                   MOVEL     WRKDES        T0DESD
135800950202     C*
135900950621    >C                   Z-ADD     dare0         T1dare
136000950621    >C                   Z-ADD     avere0        T1avere
136100950203     C                   WRITE     TOTT1
136200950202     C*
136300950203     C                   WRITE     DEST0
136400950202     C*
136500950202     C                   ENDSR
136600950202     C/EJECT
136700950202     C********************************************************************
136800950202     C/SPACE 5
136900950202     C********************************************************************
137000950202     C* Totalizzazioni chiave 9
137100950202     C*
137200950202     C     TOTT09        BEGSR
137300950210     C*Oveflow
137400950210     C   39              EXSR      TESTAT
137500950202     C*
137600950202     C                   MOVEL     SAV(9)        WRKKEY
137700950202     C                   MOVEL     WRKKEY        T9KEYD
137800950202     C*
137900950202     C*  Eseguo decodifica titoli
138000950202     C*
138100950203     C                   MOVEL     TRC(9)        WRKTRC
138200950203     C                   MOVEL     FLD(9)        WRKFLD
138300950629     C                   MOVEL     RGC(9)        WRKRGC
138400950206     C                   EXSR      DECTIT
138500950202     C                   MOVEL     WRKDES        T9DESD
138600950202     C*
138700950621    >C                   Z-ADD     dare9         T1dare
138800950621    >C                   Z-ADD     avere9        T1avere
138900950203     C                   WRITE     TOTT1
139000950202     C*
139100950203     C                   WRITE     DEST9
139200950202     C*
139300950202     C                   ENDSR
139400950202     C/EJECT
139500950202     C********************************************************************
139600950202     C/SPACE 5
139700950202     C********************************************************************
139800950202     C* Totalizzazioni chiave 8
139900950202     C*
140000950202     C     TOTT08        BEGSR
140100950210     C*Oveflow
140200950210     C   39              EXSR      TESTAT
140300950202     C*
140400950202     C                   MOVEL     SAV(8)        WRKKEY
140500950202     C                   MOVEL     WRKKEY        T8KEYD
140600950202     C*
140700950202     C*  Eseguo decodifica titoli
140800950202     C*
140900950203     C                   MOVEL     TRC(8)        WRKTRC
141000950203     C                   MOVEL     FLD(8)        WRKFLD
141100950629     C                   MOVEL     RGC(8)        WRKRGC
141200950206     C                   EXSR      DECTIT
141300950202     C                   MOVEL     WRKDES        T8DESD
141400950202     C*
141500950621    >C                   Z-ADD     dare8         T1dare
141600950621    >C                   Z-ADD     avere8        T1avere
141700950203     C                   WRITE     TOTT1
141800950202     C*
141900950203     C                   WRITE     DEST8
142000950202     C*
142100950202     C                   ENDSR
142200950202     C/EJECT
142300950202     C********************************************************************
142400950202     C/SPACE 5
142500950202     C********************************************************************
142600950202     C* Totalizzazioni chiave 7
142700950202     C*
142800950202     C     TOTT07        BEGSR
142900950210     C*Oveflow
143000950210     C   39              EXSR      TESTAT
143100950202     C*
143200950202     C                   MOVEL     SAV(7)        WRKKEY
143300950202     C                   MOVEL     WRKKEY        T7KEYD
143400950202     C*
143500950202     C*  Eseguo decodifica titoli
143600950202     C*
143700950203     C                   MOVEL     TRC(7)        WRKTRC
143800950203     C                   MOVEL     FLD(7)        WRKFLD
143900950629     C                   MOVEL     RGC(7)        WRKRGC
144000950206     C                   EXSR      DECTIT
144100950202     C                   MOVEL     WRKDES        T7DESD
144200950202     C*
144300950621    >C                   Z-ADD     dare7         T1dare
144400950621    >C                   Z-ADD     avere7        T1avere
144500950203     C                   WRITE     TOTT1
144600950202     C*
144700950203     C                   WRITE     DEST7
144800950202     C*
144900950202     C                   ENDSR
145000950202     C/EJECT
145100950202     C********************************************************************
145200950202     C/SPACE 5
145300950202     C********************************************************************
145400950202     C* Totalizzazioni chiave  6
145500950202     C*
145600950202     C     TOTT06        BEGSR
145700950210     C*Oveflow
145800950210     C   39              EXSR      TESTAT
145900950210     C*
146000950202     C                   MOVEL     SAV(6)        WRKKEY
146100950202     C                   MOVEL     WRKKEY        T6KEYD
146200950202     C*
146300950202     C*  Eseguo decodifica titoli
146400950202     C*
146500950203     C                   MOVEL     TRC(6)        WRKTRC
146600950203     C                   MOVEL     FLD(6)        WRKFLD
146700950629     C                   MOVEL     RGC(6)        WRKRGC
146800950206     C                   EXSR      DECTIT
146900950202     C                   MOVEL     WRKDES        T6DESD
147000950202     C*
147100950621    >C                   Z-ADD     dare6         T1dare
147200950621    >C                   Z-ADD     avere6        T1avere
147300950203     C                   WRITE     TOTT1
147400950202     C*
147500950203     C                   WRITE     DEST6
147600950202     C*
147700950202     C                   ENDSR
147800950202     C/EJECT
147900950202     C********************************************************************
148000950202     C/SPACE 5
148100950202     C********************************************************************
148200950202     C* Totalizzazioni chiave 5
148300950202     C*
148400950202     C     TOTT05        BEGSR
148500950210     C*Oveflow
148600950210     C   39              EXSR      TESTAT
148700950202     C*
148800950202     C                   MOVEL     SAV(5)        WRKKEY
148900950202     C                   MOVEL     WRKKEY        T5KEYD
149000950202     C*
149100950202     C*  Eseguo decodifica titoli
149200950202     C*
149300950203     C                   MOVEL     TRC(5)        WRKTRC
149400950203     C                   MOVEL     FLD(5)        WRKFLD
149500950629     C                   MOVEL     RGC(5)        WRKRGC
149600950206     C                   EXSR      DECTIT
149700950202     C                   MOVEL     WRKDES        T5DESD
149800950202     C*
149900950621    >C                   Z-ADD     dare5         T1dare
150000950621    >C                   Z-ADD     avere5        T1avere
150100950203     C                   WRITE     TOTT1
150200950202     C*
150300950203     C                   WRITE     DEST5
150400950202     C*
150500950202     C                   ENDSR
150600950202     C/EJECT
150700950202     C********************************************************************
150800950202     C/SPACE 5
150900950202     C********************************************************************
151000950202     C* Totalizzazioni chiave 4
151100950202     C*
151200950202     C     TOTT04        BEGSR
151300950210     C*Oveflow
151400950210     C   39              EXSR      TESTAT
151500950202     C*
151600950202     C                   MOVEL     SAV(4)        WRKKEY
151700950202     C                   MOVEL     WRKKEY        T4KEYD
151800950202     C*
151900950202     C*  Eseguo decodifica titoli
152000950202     C*
152100950203     C                   MOVEL     TRC(4)        WRKTRC
152200950203     C                   MOVEL     FLD(4)        WRKFLD
152300950629     C                   MOVEL     RGC(4)        WRKRGC
152400950206     C                   EXSR      DECTIT
152500950202     C                   MOVEL     WRKDES        T4DESD
152600950202     C*
152700950621    >C                   Z-ADD     dare4         T1dare
152800950621    >C                   Z-ADD     avere4        T1avere
152900950203     C                   WRITE     TOTT1
153000950202     C*
153100950203     C                   WRITE     DEST4
153200950202     C*
153300950202     C                   ENDSR
153400950202     C/EJECT
153500950202     C********************************************************************
153600950202     C/SPACE 5
153700950202     C********************************************************************
153800950202     C* Totalizzazioni chiave 3
153900950202     C*
154000950202     C     TOTT03        BEGSR
154100950210     C*Oveflow
154200950210     C   39              EXSR      TESTAT
154300950202     C*
154400950202     C                   MOVEL     SAV(3)        WRKKEY
154500950202     C                   MOVEL     WRKKEY        T3KEYD
154600950202     C*
154700950202     C*  Eseguo decodifica titoli
154800950202     C*
154900950203     C                   MOVEL     TRC(3)        WRKTRC
155000950203     C                   MOVEL     FLD(3)        WRKFLD
155100950629     C                   MOVEL     RGC(3)        WRKRGC
155200950206     C                   EXSR      DECTIT
155300950202     C                   MOVEL     WRKDES        T3DESD
155400950202     C*
155500950621    >C                   Z-ADD     dare3         T1dare
155600950621    >C                   Z-ADD     avere3        T1avere
155700950203     C                   WRITE     TOTT1
155800950202     C*
155900950203     C                   WRITE     DEST3
156000950202     C*
156100950202     C                   ENDSR
156200950202     C/EJECT
156300950202     C********************************************************************
156400950202     C/SPACE 5
156500950202     C********************************************************************
156600950202     C* Totalizzazioni chiave 02
156700950202     C*
156800950202     C     TOTT02        BEGSR
156900950210     C*Oveflow
157000950210     C   39              EXSR      TESTAT
157100950202     C*
157200950202     C                   MOVEL     SAV(2)        WRKKEY
157300950202     C                   MOVEL     WRKKEY        T2KEYD
157400950202     C*
157500950202     C*  Eseguo decodifica titoli
157600950202     C*
157700950203     C                   MOVEL     TRC(2)        WRKTRC
157800950203     C                   MOVEL     FLD(2)        WRKFLD
157900950629     C                   MOVEL     RGC(2)        WRKRGC
158000950206     C                   EXSR      DECTIT
158100950202     C                   MOVEL     WRKDES        T2DESD
158200950202     C*
158300950621    >C                   Z-ADD     dare2         T1dare
158400950621    >C                   Z-ADD     avere2        T1avere
158500950203     C                   WRITE     TOTT1
158600950202     C*
158700950203     C                   WRITE     DEST2
158800950202     C*
158900950202     C                   ENDSR
159000950202     C/EJECT
159100950202     C********************************************************************
159200950202     C/SPACE 5
159300950202     C********************************************************************
159400950202     C* Totalizzazioni chiave 1
159500950202     C*
159600950202     C     TOTT01        BEGSR
159700950210     C*Oveflow
159800950210     C   39              EXSR      TESTAT
159900950202     C*
160000950202     C                   MOVEL     SAV(1)        WRKKEY
160100950202     C                   MOVEL     WRKKEY        T1KEYD
160200950202     C*
160300950202     C*  Eseguo decodifica titoli
160400950202     C*
160500950203     C                   MOVEL     TRC(1)        WRKTRC
160600950203     C                   MOVEL     FLD(1)        WRKFLD
160700950629     C                   MOVEL     RGC(1)        WRKRGC
160800950206     C                   EXSR      DECTIT
160900950202     C                   MOVEL     WRKDES        T1DESD
161000950202     C*
161100950621    >C                   Z-ADD     dare1         T1dare
161200950621    >C                   Z-ADD     avere1        T1avere
161300950203     C                   WRITE     TOTT1
161400950202     C*
161500950203     C                   WRITE     DEST1
161600950202     C*
161700950202     C                   ENDSR
161800950202     C/EJECT
161900950202     C********************************************************************
162000950202     C/SPACE 5
162100950202     C********************************************************************
162200950202     C* Totalizzazioni generali
162300950202     C*
162400950202     C     TOTGEN        BEGSR
162500950210     C*Oveflow
162600950210     C   39              EXSR      TESTAT
162700950316     C*
162800950621    >C                   Z-ADD     dareG         T1dare
162900950621    >C                   Z-ADD     avereG        T1avere
163000950623     C*
163100950623     C                   WRITE     VUOTA
163200950623     C                   SETON                                        80
163300950203     C                   WRITE     TOTT1
163400950206     C                   WRITE     TOTG
163500950623     C                   SETOFF                                       80
163600950202     C*
163700950202     C                   ENDSR
163800950202     C/EJECT
163900950131     C******************************************************
164000950202     C* STAMPA DETTAGLIO
164100950131     C******************************************************
164200950202     C     STADET        BEGSR
164300950621     C*
164400950203     C                   MOVE      *BLANKS       ROT01
164500950203     C                   MOVE      *BLANKS       ROT02
164600950203     C                   MOVE      *BLANKS       ROT03
164700950203     C                   MOVE      *BLANKS       ROT04
164800950203     C                   MOVE      *BLANKS       ROT05
164900950203     C                   MOVE      *BLANKS       ROT06
165000950203     C                   MOVE      *BLANKS       ROT07
165100950203     C                   MOVE      *BLANKS       ROT08
165200950203     C                   MOVE      *BLANKS       ROT09
165300950203     C                   MOVE      *BLANKS       ROT10
165400950619     C*
165500950619    >C* Impostare i campi di stampa da campi del file
165600950131     C*
165700950619    >C                   z-add     REGnrreg      P1nrreg
165800950619    >C                   move      REGserreg     P1serreg
165900950621     C     REGdtreg      IFEQ      *LOVAL
166000950621     C                   z-add     0             P1dtreg
166100950621     C                   ELSE
166200000118C1309C******dmy          move      REGdtreg      P1dtreg
166300000118C1309C     *JobRun       move      REGdtreg      P1dtreg
166400950621     C                   ENDIF
166500950621     C     REGdtcoco     IFEQ      *LOVAL
166600950621     C                   z-add     0             P1dtcoco
166700950621     C                   ELSE
166800000118C1309C******dmy          move      REGdtcoco     P1dtcoco
166900000118C1309C     *JobRun       move      REGdtcoco     P1dtcoco
167000950621     C                   ENDIF
167100950621     C     REGdtlgio     IFEQ      *LOVAL
167200950621     C                   z-add     0             P1dtlgio
167300950621     C                   ELSE
167400000118C1309C******dmy          move      REGdtlgio     P1dtlgio
167500000118C1309C     *JobRun       move      REGdtlgio     P1dtlgio
167600950621     C                   ENDIF
167700991029    >C                   move      REGtpregz     P1tpregz
167800950619    >C                   move      REGcaustes    P1caustes
167900950619    >C                   move      REGctb        P1ctb
168000950619    >C                   move      REGclasse     P1classe
168100950619    >C                   move      REGunita      P1unita
168200950621     C     REGdtdoc      IFEQ      *LOVAL
168300950621     C                   z-add     0             P1dtdoc
168400950621     C                   ELSE
168500000118C1309C******dmy          move      REGdtdoc      P1dtdoc
168600000118C1309C     *JobRun       move      REGdtdoc      P1dtdoc
168700950621     C                   ENDIF
168800950619    >C                   z-add     REGnrdoc      P1nrdoc
168900950619    >C                   move      REGserdoc     P1serdoc
169000950619    >C                   move      REGuteulmo    P1uteulmo
169100950131     C*
169200950316     C* Calcoli stampa (se ce ne sono)
169300950529    >C                   EXSR      CLCSTP
169400950131     C*
169500950619    >C* Stampa
169600950619     C*
169700950131     C     *IN39         IFEQ      *ON
169800950315     C                   WRITE     TESTA
169900950622     C                   WRITE     TEST2
170000950131     C                   SETOFF                                       39
170100950131     C                   ENDIF
170200991102     C*
170300950203     C                   WRITE     RIGP1
170400991102     C*
170500991029D0291C                   Exsr      StaDesR
170600991029D0291C*
170700991029D0291C* NOOO! Stampa descrizioni aggiuntive in coda a tutte le altre
170800991029D0291C* Stampa descrizioni aggiuntive per ogni movimento
170900950621     C* Lettura movimenti
171000950621     C                   EXSR      LETMOV
171100950315     C*
171200950316     C* Calcoli per impostare il record dei totali
171300950202     C                   EXSR      CALTOT
171400950621     C*
171500950131     C                   ENDSR
171600950131     C/EJECT
171700950621     C************************************************************
171800950621     C* LETTURA MOVIMENTI
171900950621     C************************************************************
172000950621     C     LETMOV        BEGSR
172100950621     C*
172200950622     C                   z-add     0             T2DARE
172300950622     C                   z-add     0             T2AVERE
172400950621     C*
172500950621     C                   MOVE      REGSYS        MOVSYS
172600950621     C                   MOVE      REGNRASREG    MOVNRASREG
172700950622     C     K2MOV1        SETLL     NDMOV01L
172800950621     C*
172900950621     C                   DO        *HIVAL
173000950621     C*
173100950621     C     K2MOV1        READE     NDMOV000                               21
173200950621     C   21              LEAVE
173300950621     C*
173400950621     C     MOVIMPORTO    MULT      MOVDARE       DARE
173500950621     C     MOVIMPORTO    MULT      MOVAVERE      AVERE
173600950621     C*
173700950621     C* stampa riga movimento
173800971014     C*
173900950621     C                   EXSR      STAMOV
174000971014     C*
174100991029D0291C***                MOVEL     MovNrRigam    DesNrRigAm
174200991029D0291C***                MOVEL     'M'           DesTipDes
174300971120     C*
174400991029D0291C                   EXSR      STADESM
174500991029D0291C*
174600991029     C                   EXSR      STAPAS
174700980115     C*
174800991029     C                   exsr      staiva
174900991029     C                   exsr      staces
175000991029     C                   exsr      staltri
175100971222     C*
175200991029     C                   EXSR      STAMOA
175300950621     C*
175400991029D0291C***                EXSR      STADES
175500980115     C*
175600950621     C                   ENDDO
175700950621     C* Oveflow
175800950621     C     *IN39         IFEQ      *ON
175900950621     C                   WRITE     TESTA
176000950621     C                   WRITE     TEST2
176100950621     C                   SETOFF                                       39
176200950621     C                   ENDIF
176300950622     C*
176400950621     C* Stampa totali registrazione
176500950621     C                   WRITE     TOTT2
176600950622     C*
176700950622     C* Verifica squadratura
176800950622     C                   EVAL      T2squad=T2dare-T2avere
176900950622     C     T2squad       IFNE      0
177000950622     C     *IN39         IFEQ      *ON
177100950622     C                   WRITE     TESTA
177200950622     C                   WRITE     TEST2
177300950622     C                   SETOFF                                       39
177400950622     C                   ENDIF
177500950626     C     T2squad       IFGT      0
177600961010     C                   MOVE      XscDare       T2segno
177700950626     C                   ELSE
177800961010     C                   MOVE      XscAvere      T2segno
177900950626     C                   ENDIF
178000950622     C                   WRITE     SQUT2
178100950622     C                   ENDIF
178200950621     C*
178300950621     C                   ENDSR
178400950621     C/EJECT
178500950621      ************************************************************
178600950621      * STAMPA RIGA MOVIMENTO
178700950621      ************************************************************
178800950621     C     STAMOV        BEGSR
178900950621     C*
179000950621     C                   CLEAR                   RIGP2
179100950621     C*
179200950621     C                   Z-ADD     MOVimporto    P2importo
179300950621     C     MOVdare       IFEQ      1
179400961010     C                   MOVE      XscDare       P2segno
179500950621     C                   ELSE
179600950621     C     MOVavere      IFEQ      1
179700961010     C                   MOVE      XscAvere      P2segno
179800950621     C                   ENDIF
179900950621     C                   ENDIF
180000950626     C     MOVdivisa     IFNE      XSCDIV
180100950621     C                   MOVE      MOVdivisa     P2divisa
180200950621     C                   Z-ADD     MOVimportd    P2importd
180300950626     C                   ENDIF
180400950621     C                   MOVE      MOVkcc        P2kcc
180500950621     C                   MOVE      MOVksc        P2ksc
180600991029     **                  clear                   p2condpag
180700950621     C* decodifica conto
180800950621     C                   EXSR      CALLMVC002
180900950621     C     $esito        IFEQ      *OFF
181000950621     C                   MOVEL     descr002      P2descr
181100991029      * ulteriori dati descrittivi...
181200991029     C                   exsr      ctrsog
181300991029      *
181400950621     C                   ENDIF
181500980115      *
181600950621     C                   MOVE      MOVcausrig    P2causrig
181700950621     C                   MOVEL     MOVdesbrev    P2desbrev
181800950621     C     P2desbrev     IFEQ      *blank
181900950621     C                   MOVE      MOVsocieta    OPEsocieta
182000950621     C                   MOVE      MOVcausrig    OPEcausale
182100950621     C     K2OPE1        CHAIN     ANOPE01L                           21
182200950621     C  N21              MOVEL     OPEdesbrev    P2desbrev
182300950621     C                   ENDIF
182400950621     C*
182500950621     C* totali registrazione
182600950621     C                   add       DARE          T2dare
182700950621     C                   add       AVERE         T2avere
182800950621     C* Oveflow
182900950621     C     *IN39         IFEQ      *ON
183000950621     C                   WRITE     TESTA
183100950621     C                   WRITE     TEST2
183200950621     C                   SETOFF                                       39
183300950621     C                   ENDIF
183400950621     C* Stampa
183500950621     C                   WRITE     RIGP2
183600950621     C*
183700950621     C                   ENDSR
183800980115     C************************************************************
183900980115      /EJECT
184000991029      ************************************************************
184100991029      * Soggetti
184200991029      ************************************************************
184300991029     C     CTRSOG        BEGSR
184400991029     C*
184500991029     C                   MOVE      '1'           DVARIC
184600991029     C                   MOVE      XSCSOC        DVASOCIETA
184700991029     C                   MOVE      sogg002       DVASOGG
184800991029     C                   MOVE      cdind002      DVAcdind
184900991029     C                   MOVE      tpind002      DVAtpind
185000991029     c                   select
185100991029     c                   when      snatura002='C'
185200991029     C                   MOVEL     'DVACLI'      dvastrutt
185300991029     C                   EVAL      DVALenOut = %Size(DVAcli)
185400991029     c                   when      snatura002='F'
185500991029     C                   MOVEL     'DVAFOR'      dvastrutt
185600991029     C                   EVAL      DVALenOut = %Size(DVAfor)
185700991029     c                   other
185800991029     C                   MOVEL     'DVASOG'      dvastrutt
185900991029     C                   EVAL      DVALenOut = %Size(DVASOG)
186000991029     c                   endsl
186100991029      *
186200991029     C                   CALLB     'NDDVASOG'
186300991029     C                   PARM                    DVARIC
186400991029     C                   PARM                    DVASOCIETA
186500991029     C                   PARM                    DVAUNITA
186600991029     C                   PARM                    DVASTRUTT
186700991029     C                   PARM                    DVADTRIF
186800991029     C                   PARM                    DVALENOUT
186900991029      *                  PARM                    DVASNATURA
187000991029      *                  PARM                    DVACODICE
187100991029      *                  PARM                    DVALINEAV
187200991029      *                  PARM                    DVAFILIALE
187300991029     c                   parm                    *omit
187400991029     c                   parm                    *omit
187500991029     c                   parm                    *omit
187600991029     c                   parm                    *omit
187700991029     C                   PARM                    DVASOGG
187800991029     C                   PARM                    DVATPIND
187900991029     C                   PARM                    DVACDIND
188000991029     C                   PARM                    DVAERRORE
188100991029     C                   PARM                    DVAOUTPUT
188200991029      *
188300991029     C     DVAERRORE     IFEQ      *OFF
188400991029     c                   select
188500991029     c                   when      snatura002='C'
188600991029     C                   eval      %subst(DVAcli:1:DVALENOUT)
188700991029     C                              = %subst(DVAOUTPUT:1:DVALENOUT)
188800991029     c                   movel     dvcdesfil     p2descr
188900991029     **                  movel     dvccondpag    p2condpag
189000991029      *
189100991029     c                   when      snatura002='F'
189200991029     C                   eval      %subst(DVAfor:1:DVALENOUT)
189300991029     C                              = %subst(DVAOUTPUT:1:DVALENOUT)
189400991029     c                   movel     dvfdesfil     p2descr
189500991029     **                  movel     dvfcondpag    p2condpag
189600991029      *
189700991029     c                   other
189800991029     C                   eval      %subst(DVASOG:1:DVALENOUT)
189900991029     C                              = %subst(DVAOUTPUT:1:DVALENOUT)
190000991029     c                   endsl
190100991029     C                   ENDIF
190200991029      *
190300991029     C                   ENDSR
190400980115      /EJECT
190500991029D0291C************************************************************
190600991029D0291C*  CARICAMENTO Descrizioni Aggiuntive
190700991029D0291C************************************************************
190800991029D0291C**** StaDes        BEGSR
190900991029D0291C****
191000991029D0291c****               if        regsys<>dessys or regnrasreg<>desnrasreg
191100991029D0291C****
191200991029D0291C****               MOVE      RegSys        DesSys
191300991029D0291C****               MOVEL     RegNrasReg    DesNrasReg
191400991029D0291C****               CLEAR                   DesNrRigAm
191500991029D0291C****               MOVEL     'T'           DesTipDes
191600991029D0291C****
191700991029D0291C**Caricamento sfl completo
191800991029D0291C****
191900991029D0291C**** K05Des01      SETLL     NdDes01L
192000991029D0291C**** LEGGIDES      TAG
192100991029D0291C****
192200991029D0291C**** K05des01      READE     Nddes01L                               21
192300991029D0291C****
192400991029D0291C**** *IN21         IFEQ      *OFF
192500991029D0291C****               MOVEL     DesTesRig     P3Descr
192600991029D0291C****
192700991029D0291C**** *IN39         IFEQ      *ON
192800991029D0291C****               WRITE     TESTA
192900991029D0291C****               WRITE     TEST2
193000991029D0291C****               SETOFF                                       39
193100991029D0291C****               ENDIF
193200991029D0291C**stampa
193300991029D0291C****               WRITE     RIGP3
193400991029D0291C****
193500991029D0291C****               GOTO      LEGGIDES
193600991029D021EC****               ENDif
193700991029D0291C****
193800991029D0291c****               endif
193900991029D0291C****
194000991029D0291C****               ENDSR
194100991029D0291c/EJECT
194200991029D0291C************************************************************
194300991029  "  C*  CARICAMENTO Descrizioni Aggiuntive per registrazione
194400991029  "  C************************************************************
194500991029  "  C     StaDesR       BEGSR
194600991029  "  C*
194700991029  "  C                   MOVE      RegSys        DesSys
194800991029  "  C                   MOVEL     RegNrasReg    DesNrasReg
194900991029  "  C                   MOVEL     'T'           DesTipDes
195000991029  "  C     K03Des01      SETLL     NdDes01L
195100991029  "  C     K03Des01      ReadE     NdDes01L                               21
195200991029  "  C     *IN21         DOWEQ     '0'
195300991029  "  C                   MOVEL     DesTesRig     P3Descr
195400991029  "  C
195500991029  "  C     *IN39         IFEQ      *ON
195600991029  "  C                   WRITE     TESTA
195700991029  "  C                   WRITE     TEST2
195800991029  "  C                   SETOFF                                       39
195900991029  "  C                   ENDIF
196000991029  "  C*
196100991029  "  C* stampa riga descrizione aggiuntiva
196200991029  "  C                   WRITE     RIGP3
196300991029  "  C*
196400991029  "  C     K03Des01      ReadE     NdDes01L                               21
196500991029  "  C                   EndDo
196600991029  "  C*
196700991029  "  C                   EndSr
196800991029D0291c/EJECT
196900991029D0291C************************************************************
197000991029  "  C*  CARICAMENTO Descrizioni Aggiuntive per movimento
197100991029  "  C************************************************************
197200991029  "  C     StaDesM       BEGSR
197300991029  "  C*
197400991029  "  C                   MOVE      RegSys        DesSys
197500991029  "  C                   MOVEL     RegNrasReg    DesNrasReg
197600991029  "  C                   MOVEL     'M'           DesTipDes
197700991029  "  C                   Move      MOVNrRigaM    DesNrRigAm
197800991029  "  C     K04Des01      SETLL     NdDes01L
197900991029  "  C     K04Des01      ReadE     NdDes01L                               21
198000991029  "  C     *IN21         DOWEQ     '0'
198100991029  "  C                   MOVEL     DesTesRig     P3Descr
198200991029  "  C
198300991029  "  C     *IN39         IFEQ      *ON
198400991029  "  C                   WRITE     TESTA
198500991029  "  C                   WRITE     TEST2
198600991029  "  C                   SETOFF                                       39
198700991029  "  C                   ENDIF
198800991029  "  C*
198900991029  "  C* stampa riga descrizione aggiuntiva
199000991029  "  C                   WRITE     RIGP3
199100991029  "  C*
199200991029  "  C     K04Des01      ReadE     NdDes01L                               21
199300991029  "  C                   EndDo
199400991029  "  C*
199500991029  "  C                   EndSr
199600991029D0291c/EJECT
199700971120      ************************************************************
199800971120      *  CARICAMENTO Particolarita Partita
199900971120      ************************************************************
200000991029     C     StaPas        BEGSR
200100991029      *
200200991029     C                   MOVE      MovSys        PasSys
200300991029     C                   MOVE      MovNrasReg    PasNrasReg
200400991029     C                   MOVE      MovNrRigAm    PasNrRigAm
200500991029      *
200600991029      * Caricamento partite
200700991029      *
200800991029     C     K03Pas01      SETLL     NdPas01L
200900991029      *
201000991029     C     LeggiPas      TAG
201100991029      *
201200991029     C     K03Pas01      READE     NdPas01L                               22
201300991029      *
201400991029     C     *IN22         IFEQ      *OFF
201500991029      *
201600991029     C                   z-add     PASnrpar      P4nrpar
201700991029      *
201800991029     C     PASdtPar      IFEQ      *LOVAL
201900991029     C                   z-add     0             P4dtpar
202000991029     C                   ELSE
202100000118C1309C******dmy          move      PASdtPar      P4dtpar
202200000118C1309C     *JobRun       move      PASdtPar      P4dtpar
202300991029     C                   ENDIF
202400991029      *
202500991029     c                   move      passerpar     P4srpar
202600991029      *
202700991029     C     PASdtsca      IFEQ      *LOVAL
202800991029     C                   z-add     0             P4dtsca
202900991029     C                   ELSE
203000000118C1309C******dmy          move      PASdtsca      P4dtsca
203100000118C1309C     *JobRun       move      PASdtsca      P4dtsca
203200991029     C                   ENDIF
203300991029      *
203400991029     C                   z-add     PASimporto    P4importo
203500991029     C                   z-add     PASimportd    P4importd
203600991029      *
203700991029     C     *IN39         IFEQ      *ON
203800991029     C                   WRITE     TESTA
203900991029     C                   WRITE     TEST2
204000991029     C                   SETOFF                                       39
204100991029     C                   ENDIF
204200991029      *
204300991029     C                   clear                   NdPpa000
204400991029     C                   clear                   NdPps000
204500991029     C                   clear                   P4CondPag
204600991029     C                   clear                   P4CdpDes
204700991029     C                   clear                   P4BkAmm
204800030729      *
204900030729D1413 * CONDIZIONI DI PAGAMENTO: si deve usare RegCondPag e non
205000030729  "   * PpsCondPag , quindi sposto qui il reperimento della decodifica
205100030729  "  c                   Movel     RegCondPag    P4CondPag
205200030729  "  C     RegCondPag    IFne      *BLANK
205300030729  "  C                   MOVEL     RegSocieta    CdpSocieta
205400030729  "  C                   MOVEL     RegCondPag    CdpCondPag
205500030729  "  C     K02CDP01      CHAIN     ANCDP000                           24
205600030729  "   *
205700030729  "  C     *IN24         IFEQ      *OFF
205800030729  "  C                   MOVEL     CdpDesBrev    P4CdpDes
205900030729  "  C                   Endif
206000030729D1413C                   Endif
206100991029      *
206200991029     C     K07Ppa01      Chain     NdPpa01L                           23
206300991029     C     K02Pps01      Chain     NdPps01L                           23
206400991029      *
206500991029     C     *in23         ifeq      *off
206600030729D1413X****               Movel     PpsCondPag    P4CondPag
206700030729D1413X***  PpsCondPag    IFne      *BLANK
206800991029     C                   Movel     PpsBkAmm      P4BkAmm
206900991029     C                   Endif
207000991029      * stampa
207100991029     C                   WRITE     RIGP4
207200991029      *
207300991029     C                   clear                   P4CondPag
207400991029     C                   clear                   P4CdpDes
207500991029     C                   clear                   P4BkAmm
207600991029     C                   GOTO      LeggiPas
207700991029     C                   ENDif
207800991029      *
207900991029     C                   ENDSR
208000991029     c/EJECT
208100991029      ************************************************************
208200991029      *  Dati IVA
208300991029      ************************************************************
208400991029     C     StaIva        BEGSR
208500991029      *
208600991029     c                   clear                   rigp6
208700991029     c     k03mov01      chain     NdIva01l                           25
208800991029  b1 c                   dow       *in25=*off
208900991029      *
209000991029     c     IvaDare       ifeq      1
209100991029     c                   move      'D'           P6SEGNO
209200991029     c                   Endif
209300991029      *
209400991029     c     IvaAvere      ifeq      1
209500991029     c                   move      'A'           P6SEGNO
209600991029     c                   Endif
209700991029      *
209800991029     c                   z-add     ivaimporto    p6importo
209900991029     c                   z-add     ivaimponib    p6imponib
210000991029     c                   move      ivatpreg      p6tpreg
210100991029     c                   move      ivalibro      p6libro
210200991029      *                  move      ivacausrig    p6causrig
210300991029     c                   z-add     ivaaliq       p6aliq
210400991029     c                   move      ivarifiva     p6rifiva
210500991029     c                   clear                   p6desiva
210600991029      * descrizione da G48 ...
210700991029     c                   if        ivarifiva<>*blank
210800991029     c                   z-add     1             £x
210900991029     c     ivarifiva     lookup    rif(£x)                                22
211000991029     C                   if        *in22=*on
211100991029     C                   MOVEL     Drif(£x)      p6desiva
211200991029     C                   endif
211300991029     C                   endif
211400991029      *
211500991029     c                   write     rigp6
211600991029      *
211700991029     c     k03mov01      reade     NdIva01l                               25
211800991029     c                   enddo
211900991029      *
212000991029     c                   ENDSR
212100991029      ************************************************************
212200991029      *  Dati Cespiti
212300991029      ************************************************************
212400991029     C     StaCes        BEGSR
212500991029      *
212600991029     c                   clear                   rigp7
212700050317     **    k02moc02      chain     NdMoc02l                           25
212800980205      * sys,nrasreg,rigam ...
212900050317c1776x**   k03mov01      chain     NdMoc02l                           25
213000050317c1776c     k03mov01      chain     NdMo202l                           25
213100991029      * chain secca o ciclo ?
213200991029  b1 c                   if        *in25=*off
213300991029     c*                  dow       *in25=*off
213400991029      *
213500991029     c                   move      moccc1        p7cc1
213600991029     c                   move      moccc2        p7cc2
213700050317C1776X**   k03ana01      chain     ndana01l                           25
213800050317C1776c     k03ana01      chain     ndan201l                           25
213900991029  b2 c                   if        *in25=*off
214000991029     c                   move      anadesbrev    p7desbrev
214100991029  e2 c                   endif
214200991029      *
214300991029     c*                  write     rigp7
214400991029     c*                  enddo
214500991029  e1 c                   endif
214600991029      *
214700991029     c     k03mov01      chain     NdBmv01l                           25
214800991029  b1 c                   if        *in25=*off
214900991029     c                   if        bmvdtope>*loval and bmvdtope<*hival
215000000118C1309c******dmy          move      bmvdtope      p7dtope
215100000118C1309c     *JobRun       move      bmvdtope      p7dtope
215200991029     c                   endif
215300991029     c                   if        bmvdtval>*loval and bmvdtval<*hival
215400000118C1309c******dmy          move      bmvdtval      p7dtval
215500000118C1309c     *JobRun       move      bmvdtval      p7dtval
215600991029     c                   endif
215700991029  e1 c                   endif
215800991029      *
215900991029      *
216000991029     c                   if        p7cc1<>*blank or p7cc2<>*blank or
216100991029     c                             p7desbrev<>*blank or
216200991029     c                             p7dtope>0 or p7dtval>0
216300991029     c                   write     rigp7
216400991029     c                   endif
216500991029      *
216600991029     c                   ENDSR
216700991029      *
216800991029      ************************************************************
216900991029      *  Dati Vari
217000991029      ************************************************************
217100991029     C     Staltri       BEGSR
217200991029      *
217300991029      *
217400991029     c                   ENDSR
217500991029      *
217600991029     c/EJECT
217700980116      ************************************************************
217800980116      *  CARICAMENTO Movimenti Analitica
217900980116      ************************************************************
218000991029     C     StaMoa        BEGSR
218100991029      **
218200991029     C                   MOVE      MovSys        MOASys
218300991029     C                   MOVE      MovNrasReg    MOANrasReg
218400991029     C                   MOVE      MovNrRigAm    MOANrRigAm
218500991029      *
218600991029     C                   clear                   NdMoa000
218700991029     c                   clear                   rigp5
218800991029     c                   movel     movcdrateo    p5cdrateo
218900991029     c                   z-add     movratrisc    p5ratrisc
219000991029     c                   movel     *off          stamp_p5          1
219100991029      *
219200991029      * Caricamento sfl completo
219300991029      *
219400991029     C     K03Moa01      SETLL     NdMoa01L
219500991029     C     LeggiMoa      TAG
219600991029      *
219700991029     C     K03Moa01      READE     NdMoa01L                               25
219800991029      *
219900991029     C     *IN25         IFEQ      *OFF
220000991029      *
220100971222     c     Moadare       ifeq      1
220200971222     c                   move      'D'           P5SEGNO
220300971222     c                   Endif
220400991029      *
220500971222     c     Moaavere      ifeq      1
220600971222     c                   move      'A'           P5SEGNO
220700971222     c                   Endif
220800991029      *
220900971222     c                   move      moadivisa     P5DIVISA
221000971222     c                   z-add     moaimporto    P5IMPORTO
221100971222     c                   z-add     moaimportd    P5IMPORTD
221200991029      *
221300971222     c                   movel     moavoce       P5VOCE
221400971222     c                   movel     moadimen      P5DIMEN
221500090504>>>>> *
221600090504>>>>>C     MOAdtcoan     IFEQ      *LOVAL
221700090504>>>>>C                   z-add     0             P5dtcom
221800090504>>>>>C                   ELSE
221900090504>>>>>C     *dmy          move      MOAdtcoan     P5dtcom
222000090504>>>>>C                   ENDIF
222100090504>>>>> *
222200971222     c                   movel     moatprif1     P5TPRIF1
222300971222     c                   movel     moaentit1     P5ENTIT1
222400971222     c                   movel     moatprif2     P5TPRIF2
222500971222     c                   movel     moaentit2     P5ENTIT2
222600971222     c                   movel     moatprif3     P5TPRIF3
222700971222     c                   movel     moaentit3     P5ENTIT3
222800090504>>>>>c**                 movel     moatprif4     P5TPRIF4
222900090504>>>>>c**                 movel     moaentit4     P5ENTIT4
223000971222     c                   movel     moatprif5     P5TPRIF5
223100971222     c                   movel     moaentit5     P5ENTIT5
223200971222     c                   movel     moatprif6     P5TPRIF6
223300971222     c                   movel     moaentit6     P5ENTIT6
223400971222     c                   movel     moatprif7     P5TPRIF7
223500971222     c                   movel     moaentit7     P5ENTIT7
223600971222     c                   movel     moatprif8     P5TPRIF8
223700971222     c                   movel     moaentit8     P5ENTIT8
223800991029      *
223900991029      *
224000991029     C     *IN39         IFEQ      *ON
224100991029     C                   WRITE     TESTA
224200991029     C                   WRITE     TEST2
224300991029     C                   SETOFF                                       39
224400991029     C                   ENDIF
224500991029      * stampa
224600991029     C                   WRITE     RIGP5
224700991029     c                   movel     *on           stamp_p5
224800991029     c                   clear                   p5cdrateo
224900991029     c                   clear                   p5ratrisc
225000991029      *
225100991029     C                   GOTO      LeggiMoa
225200991029     C                   ENDif
225300991029      * se non stampato e presenti dati di rateo
225400991029     c                   if        stamp_p5=*off and
225500991029     c                             (p5cdrateo<>*blank or p5ratrisc>0)
225600991029     C                   WRITE     RIGP5
225700991029     c                   movel     *on           stamp_p5
225800991029     c                   endif
225900991029      *
226000991029     C                   ENDSR
226100991029     c/EJECT
226200950621      ************************************************************
226300950621      * DECODIFICA CONTO
226400950621      ************************************************************
226500950621     C     CALLMVC002    BEGSR
226600950621     C*
226700950621     C* Decodifica conto
226800950621     C                   Clear                   ND002DS
226900950621     C                   EVAL      $LenOut = %Size(ND002DS)
227000950621     C                   CallB     'NDMVC002'
227100950621     C                   PARM      MOVsocieta    $soc
227200950621     C                   PARM      MOVkcc        $kcc
227300950621     C                   PARM      MOVksc        $ksc
227400950621     C                   PARM                    $date
227500950621     C                   PARM      *OFF          $GesMsg
227600950621     C                   PARM      *OFF          $Esito
227700950621     C                   PARM      'ND002DS'     $Struttura
227800950621     C                   PARM                    ND002DS
227900950621     C                   PARM                    $LenOut
228000971222     C*
228100971222     C                   CLEAR                   P2PTIVA
228200971224     c                   Clear                   Sogpartiva
228300971222     C*
228400971222     c     KCCsogg002    IFeq      '1'
228500971222     c     Sogg002       andne     *blank
228600971222     c     Sogg002       chain     ansog01l                           26
228700971224     c     *in26         ifeq      *off
228800971224     C                   movel     sogpartiva    P2PTIVA
228900971222     c                   endif
229000971222     c                   endif
229100950621     C*
229200950621     C                   ENDSR
229300950621     C/EJECT
229400950131     C************************************************************
229500950315     C* CALCOLI
229600950131     C************************************************************
229700950315     C     CLCSTP        BEGSR
229800950315     C*
229900950619     C* In questa routine vanno immessi i calcoli necessari ...
230000950619     C*
230100950131     C                   ENDSR
230200950201     C/EJECT
230300950202     C************************************************************
230400950202     C* CALCOLO TOTALI
230500950202     C************************************************************
230600950202     C     CALTOT        BEGSR
230700950316     C*                                                          *
230800950316     C* Incrementare i campi dei totali di ogni rottura          *
230900950316     C* con i campi del dettaglio                                *
231000950316     C*
231100950202     C* Rottura 1
231200950621    >C                   ADD       T2dare        dare1
231300950621    >C                   ADD       T2avere       avere1
231400950316    >C* Rottura 2
231500950621    >C                   ADD       T2dare        dare2
231600950621    >C                   ADD       T2avere       avere2
231700950316    >C* Rottura 3
231800950621    >C                   ADD       T2dare        dare3
231900950621    >C                   ADD       T2avere       avere3
232000950316    >C* Rottura 4
232100950621    >C                   ADD       T2dare        dare4
232200950621    >C                   ADD       T2avere       avere4
232300950316    >C* Rottura 5
232400950621    >C                   ADD       T2dare        dare5
232500950621    >C                   ADD       T2avere       avere5
232600950316    >C* Rottura 6
232700950621    >C                   ADD       T2dare        dare6
232800950621    >C                   ADD       T2avere       avere6
232900950316    >C* Rottura 7
233000950621    >C                   ADD       T2dare        dare7
233100950621    >C                   ADD       T2avere       avere7
233200950316    >C* Rottura 8
233300950621    >C                   ADD       T2dare        dare8
233400950621    >C                   ADD       T2avere       avere8
233500950316    >C* Rottura 9
233600950621    >C                   ADD       T2dare        dare9
233700950621    >C                   ADD       T2avere       avere9
233800950316    >C* Rottura 10
233900950621    >C                   ADD       T2dare        dare0
234000950621    >C                   ADD       T2avere       avere0
234100950316    >C* Totale generale
234200950622    >C                   ADD       T2dare        dareG
234300950622    >C                   ADD       T2avere       avereG
234400950202     C*
234500950202     C                   ENDSR
234600950621     C/EJECT
234700940927     C************************************************************
234800940927     C* OPERAZIONI INIZIALI
234900940927     C************************************************************
235000940927     C     *INZSR        BEGSR
235100000118C1309C     *DMY          Move      Udate         ZDateISO
235200000118C1309C     *JobRun       Move      ZDateISO      ZDate             6 0
235300940927     C*
235400940830     C* Reperimento parametri
235500940830     C*
235600940830     C     *ENTRY        PLIST
235700940830     C                   PARM                    KPJBA
235800950208     C                   PARM                    TITOLO           50
235900950208     C*
236000950208     C                   MOVEL     TITOLO        T1TIT
236100940830     C*
236200940830     C                   ENDSR
236300941129     C/EJECT
236400941129     C************************************************************
236500941129     C* Reperimento dati società
236600941129     C************************************************************
236700941129     C     REPSOC        BEGSR
236800941129     C*
236900950630     C                   CALLB     'XSOC'
237000941129     C                   PARM                    TIPXSC            6
237100941129     C                   PARM                    SOCXSC            3
237200941129     C                   PARM                    CDSXSC            9 0
237300941129     C                   PARM                    MODXSC            3
237400941129     C                   PARM      *BLANKS       RTNXSC            1
237500941129     C                   PARM                    XSOCDS
237600941129     C                   PARM                    KPJBA
237700941129      *
237800941129     C                   ENDSR
237900940830     C/EJECT
238000950621     C************************************************************
238100950621     C* REPERIMENTO AUTORIZZAZIONI UTENTE
238200950621     C************************************************************
238300950621     C     REPAUT        BEGSR
238400950621     C*
238500950621     C                   CLEAR                   AUTDS
238600950621     C*
238700950621     C                   MOVEL     MPESOC        XCASOC                         SOCIETA
238800950621     C                   MOVEL     XSCDUN        XCAUNI                         UNITA
238900950621     C                   Z-ADD     0             XCAGRP                         GRUPPO
239000950704     C                   MOVEL     KNMUS         XCAPRF                         PROFILO
239100950621    >C                   MOVEL     'CONSTA'      XCAFNC                         FUNZIONE
239200950621     C                   MOVEL     *ZERO         XCAVFU                         VARIABILE
239300950621     C                   MOVE      'CK'          XCATCK                         TIPO CTRL
239400950621     C                   MOVE      '1'           XCANAU                         RICH.XNOAUT
239500950621     C*
239600950621     C                   CALLB     'XCHKAUT'
239700950621     C                   PARM                    AUTDS
239800950621     C*
239900950621     C* errori/non abilitazione
240000950621     C     XCARTN        IFNE      0
240100950621     C                   EXSR      ENDPGM
240200950621     C                   ELSE
240300950621    >C                   MOVEL     XCADDS        CSTADS
240400950621     C                   ENDIF
240500950621     C*
240600950621     C                   ENDSR
240700950621     C/EJECT
240800940830     C************************************************************
240900940830     C* INIZIALIZZAZIONE VARIABILI
241000940830     C************************************************************
241100940830     C     INZVAR        BEGSR
241200940830     C*
241300950208     C                   MOVEL     KPJBU         A968DS
241400950509     C*
241500950509     C* Reperimento parametri da ANMPE di lavoro
241600950509     C*
241700950616     C                   Z-ADD     LAN968        MPEPRGLAN
241800950509     C                   MOVE      SOC968        MPESOC
241900950509     C     K2MPE1        CHAIN     ANMPE01W                           21
242000950509     C     *IN21         IFEQ      *OFF
242100950616    >C                   MOVEL     MPEPAR        A977DS
242200950509     C                   ELSE
242300950509     C                   EXSR      ENDPGM
242400950509     C                   ENDIF
242500950509     C*
242600950509     C* Reperimento dati società
242700950509     C*
242800950509     C                   MOVEL     'SOC001'      TIPXSC
242900950509     C                   MOVEL     MPESOC        SOCXSC
243000950509     C                   EXSR      REPSOC
243100950509     C     RTNXSC        IFNE      '1'
243200950509     C                   MOVEL     XSOCDS        SOC001
243300950509     C                   ELSE
243400950509     C                   EXSR      ENDPGM
243500950509     C                   ENDIF
243600950621     C*
243700950621     C* Reperimento autorizzazioni
243800950621     C*
243900950621     C                   EXSR      REPAUT
244000941129     C*
244100941129     C* Valorizzazione campi univoci testate
244200941129     C*
244300941129     C                   MOVEL     KNSIF         NOMSIF
244400941129     C                   MOVEL     XSCDSI        NOMDIT
244500941129     C                   MOVEL     DSPGM         NOMPGM
244600950704     C* Reperisco nome sistema informatico
244700950704     C                   CALL      'XNETA'                              21
244800950704     C                   PARM                    NOMSYS
244900950315     C*
245000950315     C* Reperimento data del giorno
245100950315     C*
245200950619     C     *eur          move      *date         oggi
245300991029      *
245400991029     c                   z-add     0             £x                3 0
245500991029     c                   MOVE      '2'           XTBRIC
245600991029     c                   MOVE      xscsoc        XTBAZI
245700991029     c                   MOVE      *blank        XTBKEY
245800991029     c                   MOVE      'G48'         XTBCOD
245900991029     c                   CALLb     'XATB'
246000991029     c                   PARM                    XATBDS
246100991029      *
246200991029  b1 c                   dow       xtberr=*off
246300991029     c                   MOVEL     XTBUNI        TABG48
246400991029      *
246500991029  b2 c                   if        kscg48<>*blank
246600991029     c                   z-add     1             £x
246700991029     c     kscg48        lookup    rif(£x)                                22
246800991029  b3 c                   if        *in22=*on
246900991029     c                   movel     desg48        drif(£x)
247000991029  x3 c                   else
247100991029     c                   z-add     1             £x
247200991029     c     *blank        lookup    rif(£x)                                22
247300991029  b4 c                   if        *in22=*on
247400991029     c                   move      kscg48        rif(£x)
247500991029     c                   movel     desg48        drif(£x)
247600991029  e4 c                   endif
247700991029  e3 c                   endif
247800991029  e2 c                   endif
247900991029      *
248000991029     C                   MOVE      '3'           XTBRIC
248100991029     C                   CALLb     'XATB'
248200991029     C                   PARM                    XATBDS
248300991029  e1 c                   enddo
248400950509     C*
248500950622     C* Inizializzazione totali generali
248600950622     C*
248700950622     C                   CLEAR                   DSTG
248800950622     C*
248900950509     C                   ENDSR
249000940830     C************************************************************
249100940830     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
249200940830     C************************************************************
249300940830     C     DEFCAM        BEGSR
249400950203     C*
249500950315     C                   MOVE      *BLANKS       WRKTRC           10
249600950315     C                   MOVE      *BLANKS       WRKFLD           10
249700950629     C                   MOVE      *BLANKS       WRKRGC            3
249800971223      *
249900950508      * KLIST per CHAIN su ANMPE
250000950508      *                                                          *
250100950508     C     K2MPE1        KLIST                                                        *
250200950616     C                   KFLD                    MPEPRGLAN                            *
250300950508     C                   KFLD                    MPESOC                               *
250400950619     C*
250500950619     C* KLIST per NDMOV
250600950619     C*
250700950619     C     K2MOV1        KLIST
250800950619     C                   KFLD                    MOVSYS
250900950619     C                   KFLD                    MOVNRASREG
251000971222     C*
251100971222     C* KLIST per NDMOA
251200971222     C*
251300971222     C     K03MOA01      KLIST
251400971222     C                   KFLD                    MOASYS
251500971222     C                   KFLD                    MOANRASREG
251600971222     C                   KFLD                    MOANRRIGAM
251700950621     C*
251800950621     C* KLIST per ANOPE
251900950621     C*
252000950621     C     K2OPE1        KLIST
252100950621     C                   KFLD                    OPESOCIETA
252200950621     C                   KFLD                    OPECAUSALE
252300991029     C*
252400991029     C* KLIST per Nddes01L
252500991029     C*
252600991029D0291C**** K05Des01      KLIST
252700991029D0291C****               KFLD                    DesSys
252800991029D0291C****               KFLD                    DesNrasReg
252900991029     C*$ova              KFLD                    DesTipDes
253000991029     C*$ova              KFLD                    DesNrRigaM
253100991029     C*$ova              KFLD                    DesNrRigaX
253200991029D0291C     K03Des01      KLIST
253300991029D0291C                   KFLD                    DesSys
253400991029D0291C                   KFLD                    DesNrasReg
253500991029D0291C                   KFLD                    DesTipDes
253600991029D0291C     K04Des01      KLIST
253700991029D0291C                   KFLD                    DesSys
253800991029D0291C                   KFLD                    DesNrasReg
253900991029D0291C                   KFLD                    DesTipDes
254000991029D0291C                   KFLD                    DesNrRigaM
254100991029     C*
254200991029     C* KLIST per NdPas01L
254300991029     C*
254400991029     C     K03Pas01      KLIST
254500991029     C                   KFLD                    PasSys
254600991029     C                   KFLD                    PasNrasReg
254700991029     C                   KFLD                    PasNrRigAm
254800991029     C*
254900991029     C* KLIST per NdPpa01L
255000991029     C*
255100991029     C     K07Ppa01      KLIST
255200991029     C                   KFLD                    PasSocieta
255300991029     C                   KFLD                    PasCtb
255400991029     C                   KFLD                    PasKcc
255500991029     C                   KFLD                    PasKsc
255600991029     C                   KFLD                    PasDtpar
255700991029     C                   KFLD                    PasNrpar
255800991029     C                   KFLD                    PasSerpar
255900991029     C*
256000991029     C* KLIST per NdPps01L
256100991029     C*
256200991029     C     K02Pps01      KLIST
256300991029     C                   KFLD                    PpaSys
256400991029     C                   KFLD                    PpaNrasInt
256500991029     C*
256600991029     C* ANCDP01L
256700991029     C*
256800991029     C     K02CDP01      KLIST
256900991029     C                   KFLD                    CdpSocieta
257000991029     C                   KFLD                    CdpCondPag
257100991029     C*
257200991029     C* KLIST per NdReg01L
257300991029     C*
257400991029     C     K02REG01      KLIST
257500991029     C                   KFLD                    RegSys
257600991029     C                   KFLD                    RegNrasCol
257700991029      *
257800991029      * KLIST per NdPas01L
257900991029      *
258000991029     C     K03Mov01      KLIST
258100991029     C                   KFLD                    MovSys
258200991029     C                   KFLD                    MovNrasReg
258300991029     C                   KFLD                    MovNrRigAm
258400991029     C     K02Moc02      KLIST
258500991029     C                   KFLD                    MovSys
258600991029     C                   KFLD                    MovNrasReg
258700991029      *
258800050317      * KLIST per NdAn201L
258900991029      *
259000991029     C     K03Ana01      KLIST
259100991029     C                   KFLD                    MocSocieta
259200991029     C                   KFLD                    MocCc1
259300991029     C                   KFLD                    MocCc2
259400991029      *
259500950127     C                   ENDSR
259600950202     C/EJECT
259700950202     C********************************************************************
259800950202      /SPACE 5
259900950202     C********************************************************************
260000950202     C* Decodifica chiavi per titoli
260100950202     C*
260200950206     C     DECTIT        BEGSR
260300950207     C                   MOVE      *BLANKS       WRKDES          256
260400950206     C*
260500950629   B1C     WRKRGC        IFNE      *BLANKS
260600950315     C*
260700950203     C                   CLEAR                   A971DS
260800950203     C                   MOVE      XSCSOC        SOC971
260900950203     C                   MOVEL     WRKTRC        TRC971
261000950203     C                   MOVEL     WRKFLD        FLD971
261100950203     C                   MOVEL     'EQ'          OPE971
261200950203     C                   MOVEL     WRKKEY        KEY971
261300950203     C                   MOVEL     'C'           MOD971
261400950203     C                   MOVEL     *BLANK        FLG971
261500950203     C                   MOVEL     A971DS        KPJBU
261600950203     C                   CALL      'ANA971R'                            98
261700950203     C                   PARM                    KPJBA
261800950203     C                   MOVEL     KPJBU         A971DS
261900950203     C*
262000950203     C                   MOVEL     DEC971        WRKDES
262100950206     C*
262200950206   E1C                   ENDIF
262300950202     C*
262400950202     C                   ENDSR
262500950413     C/EJECT
262600950619** SK
262700950619select * from NDREG00F where
262800950619** SK1
262900950621REGSOCIETA ='XXX'
