000100981126     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND PJCBND)
000200981126     H*PARMS ACTGRP(*caller)
000300000601B0218H*PARMS DYNUSRPRF(*OWNER) RDB(*NONE) DATFMT(*ISO) COMMIT(*NONE)
000400940830     H*--------INIZIO SPECIFICHE --------------------------*
000500940830     H DECEDIT('0,') DATEDIT(*DMY.)
000600970808     H*                  STAMPA situazione banche
000700950316     H*                 -------------------------------
000800950316     H*
000900940830     F*----------------------------------------------------*
001000970813     FANLSS01L  IF   E           K DISK
001100970813     FANBFI01L  IF   E           K DISK
001200970808     FANBCT01L  IF   E           K DISK
001300981126    >FANBRA01L  IF   E           k Disk    rename(ANBRA000:ANBRA001)
001400970808     FANMPE01W  IF   E           K DISK
001500020125C1540FNDSIB00W  O    E             DISK    USROPN
001600970808     FNDCT52P   O    E             PRINTER OFLIND(*IN39)
001700090504     FYndct52F  O    E             DISK
001800950131     D*
001900950202     D LET             S             15    DIM(10)                              campi chiave letti
002000950202     D SAV             S             15    DIM(10)                              campi chiave salvati
002100950629     D LUN             S              3  0 DIM(10)                              campi chiave salvati
002200950629     D POS             S              5  0 DIM(10)                              campi chiave salvati
002300950629     D TIP             S              1    DIM(10)                              campi chiave salvati
002400960528     D RGC             S              4    DIM(10)                              campi chiave salvati
002500951025     D*
002600951025      * Skiere x memorizzare cod. e descriz. x ogni livello
002700960524     D SkCampo         S                   DIM(10) like(T1Campo)                campi chiave salvati
002800960517     D SkCod           S                   DIM(10) like(L1Chiave)               campi chiave salvati
002900960517     D SkDescr         S                   DIM(10) like(L1Decodif)              campi chiave salvati
003000951025     D*
003100950629     D* Costante
003200950629     D FETCH           C                   CONST('FOR FETCH ONLY')
003300970808     D*
003400970808     D* Indicare la ds del file che serve per l'sql se il file è uno
003500950718     D* solo
003600970808     D ANBRA         E DS                  occurs(100) EXTNAME(ANBRA00F)
003700970808     D*-------------
003800940830     D* Passaggio Parametri
003900940830     D KPJBA         E DS
004000940830     D*-------------
004100941129     D* Dati di ambiente ottenuti da XSOC
004200941129     D SOC001        E DS                  EXTNAME(XSOC001DS)
004300941129     D*-------------
004400941129     D* DS Interna per dati di output di XSOC
004500941129     D XSOCDS          DS          1000
004600950315     D*-------------
004700950315     D* Parametri per reperimento dati campo
004800960527     D Ana967ds      E DS                  INZ
004900960527     D  ERR967       E                     INZ('0')
005000950315     D*-------------
005100950315     D* Parametri per stampa prima pagina
005200960527     D ANA968DS      E DS                  INZ
005300950315     D*-------------
005400950131     D* Parametri del PGM di reperimento schiera selezioni
005500960527     D ANA969DS      E DS                  INZ
005600950131     D  S969         E                     EXTFLD(SEL969)
005700950131     D                                     DIM(5000)                            parametri
005800950131     D  O969         E                     EXTFLD(ORD969)
005900950131     D                                     DIM(2000)                               "
006000950315     D*-------------
006100950203     D* Parametri per programma di controllo
006200960527     D ANA971DS      E DS                  INZ
006300950315     D*-------------
006400950621     D* LANCIATORE
006500950621     D* Parametri di lancio
006600970808     D NDCT51DS      E DS                  INZ
006700970813     D*-------------
006800970813     D* Reprimento saldi
006900970813     D NDCT21DS      E DS                  inz
007000020125     D*-------------
007100020125C1540D* Gestione file x pc
007200020125C1540D X24FpcDS      E DS
007300950621     D*-------------
007400950315     D*  ds per ricerca campo pgm ANA962R
007500961015     D DAT962          DS          4096
007600950315     D*-------------
007700940830     D* Reperimento nome PGM
007800940830     D STATUS         SDS           333
007900940830     D  DSPGM            *PROC
008000950315     D*-------------
008100950201     D* Schiera selezione file
008200950316     D SKK             DS
008300970813     D  SK                     1    320
008400950206     D                                     DIM(4) CTDATA PERRCD(1)              schiere
008500970813     D                 DS
008600090504>>>>>D  SK1                    1    640
008700090504>>>>>D                                     DIM(640) CTDATA PERRCD(80)           schiera 1 sql
008800090504>>>>>D  SK1X                   1    640
008900970813     D*-------------
009000970813     D                 DS
009100970813     D  SK2                    1      6
009200970813     D                                     DIM(6) CTDATA PERRCD(6)
009300990128      *-------------
009400990128      *Saldo da stampare
009500970813     D P1Saldo         S                   DIM(7) like(P1Saldo1)
009600990128      *-------------
009700990128      *Saldo reale (è possibile che sia + grande di quello da stampare)
009800990128     D WSaldoCalc      S                   DIM(7) like(SlInDOpT21)
009900990128      *-------------
010000990128      *Cifra massima del saldo stampabile
010100990128     D WPSaldoMax      S                   DIM(7) like(P1Saldo1)
010200990128     D                                     Inz(*Hival)
010300990128      *-------------
010400990128      *Cifra minima del saldo stampabile
010500990128     D WPSaldoMin      S                   DIM(7) like(P1Saldo1)
010600990128     D                                     Inz(*Loval)
010700020125     D*-------------
010800020125c1540D SAVJBA          s                   like(Kpjba)
010900970813     D*-------------
011000950131     D* Stringa per SQL
011100950131     D SQ              DS          5000
011200950131     D* Selezioni
011300950131     D SELX            DS
011400950131     D  SEL                    1   5000
011500950131     D                                     DIM(5000)                            parametri
011600950131     D* Ordinamenti
011700950131     D ORDX            DS
011800950131     D  ORD                    1   2000
011900950131     D                                     DIM(2000)                               "
012000950203     D* tracciato
012100950203     D                 DS
012200950203     D  TRC                    1    100
012300950203     D                                     DIM(10)                              tracciati  anreo00f
012400950203     D  TRCX                   1    100
012500950202     D* campi ordinamento
012600950202     D                 DS
012700950202     D  FLD                    1    100
012800950203     D                                     DIM(10)                              campi ord. anreo00f
012900950202     D  FLDX                   1    100
013000950202     D* tipi rottura
013100950202     D                 DS
013200950202     D  ROT                    1     10
013300950202     D                                     DIM(10)                              tipi rottura
013400950202     D  ROTX                   1     10
013500950206     D* descrizioni campi
013600950206     D                 DS
013700950206     D  TIT                    1    200
013800950206     D                                     DIM(10)                              titoli x totali
013900950206     D  TITX                   1    200
014000950316     D*
014100950316     D* Definire una ds per ogni rottura
014200950316     D*
014300950316     D* ROTTURA 1
014400990129     D**DS1            DS                  inz
014500990128     D**FL101                  1     13  2
014600990128     D**FL201                 14     26  2
014700990128     D**FL301                 27     39  2
014800990128     D**FL401                 40     52  2
014900990128     D**FL501                 53     65  2
015000990128     D**FL601                 66     78  2
015100990128     D**FL701                 79     91  2
015200990129     D FL1             S                   DIM(7) Like(SlInDOpT21)
015300950316     D* ROTTURA 2
015400990129     D*DS2             DS                  inz
015500990129     D**FL201                              Like(SlInDOpT21)
015600990129     D**FL202                              Like(SlInDOpT21)
015700990129     D**FL302                              Like(SlInDOpT21)
015800990129     D**FL402                              Like(SlInDOpT21)
015900990129     D**FL502                              Like(SlInDOpT21)
016000990129     D**FL602                              Like(SlInDOpT21)
016100990129     D**FL702                              Like(SlInDOpT21)
016200990129     D FL2             S                   DIM(7) Like(SlInDOpT21)
016300970909     D* ROTTURA 3
016400990129     D*DS3             DS                  inz
016500990129     D**FL103                              Like(SlInDOpT21)
016600990129     D**FL203                              Like(SlInDOpT21)
016700990129     D**FL303                              Like(SlInDOpT21)
016800990129     D**FL403                              Like(SlInDOpT21)
016900990129     D**FL503                              Like(SlInDOpT21)
017000990129     D**FL603                              Like(SlInDOpT21)
017100990129     D**FL703                              Like(SlInDOpT21)
017200990129     D FL3             S                   DIM(7) Like(SlInDOpT21)
017300950316     D* ROTTURA 4
017400990129     D*DS4             DS                  inz
017500990129     D**FL104                              Like(SlInDOpT21)
017600990129     D**FL204                              Like(SlInDOpT21)
017700990129     D**FL304                              Like(SlInDOpT21)
017800990129     D**FL404                              Like(SlInDOpT21)
017900990129     D**FL504                              Like(SlInDOpT21)
018000990129     D**FL604                              Like(SlInDOpT21)
018100990129     D**FL704                              Like(SlInDOpT21)
018200990129     D FL4             S                   DIM(7) Like(SlInDOpT21)
018300950316     D* ROTTURA 5
018400990129     D*DS5             DS                  inz
018500990129     D**FL105                              Like(SlInDOpT21)
018600990129     D**FL205                              Like(SlInDOpT21)
018700990129     D**FL305                              Like(SlInDOpT21)
018800990129     D**FL405                              Like(SlInDOpT21)
018900990129     D**FL505                              Like(SlInDOpT21)
019000990129     D**FL605                              Like(SlInDOpT21)
019100990129     D**FL705                              Like(SlInDOpT21)
019200990129     D FL5             S                   DIM(7) Like(SlInDOpT21)
019300950316     D* ROTTURA 6
019400990129     D*DS6             DS                  inz
019500990129     D**FL106                              Like(SlInDOpT21)
019600990129     D**FL206                              Like(SlInDOpT21)
019700990129     D**FL306                              Like(SlInDOpT21)
019800990129     D**FL406                              Like(SlInDOpT21)
019900990129     D**FL506                              Like(SlInDOpT21)
020000990129     D**FL606                              Like(SlInDOpT21)
020100990129     D**FL706                              Like(SlInDOpT21)
020200990129     D FL6             S                   DIM(7) Like(SlInDOpT21)
020300950316     D* ROTTURA 7
020400990129     D*DS7             DS                   inz
020500990129     D**FL107                              Like(SlInDOpT21)
020600990129     D**FL207                              Like(SlInDOpT21)
020700990129     D**FL307                              Like(SlInDOpT21)
020800990129     D**FL407                              Like(SlInDOpT21)
020900990129     D**FL507                              Like(SlInDOpT21)
021000990129     D**FL607                              Like(SlInDOpT21)
021100990129     D**FL707                              Like(SlInDOpT21)
021200990129     D FL7             S                   DIM(7) Like(SlInDOpT21)
021300950316     D* ROTTURA 8
021400990129     D*DS8             DS                   Inz
021500990129     D**FL108                              Like(SlInDOpT21)
021600990129     D**FL208                              Like(SlInDOpT21)
021700990129     D**FL308                              Like(SlInDOpT21)
021800990129     D**FL408                              Like(SlInDOpT21)
021900990129     D**FL508                              Like(SlInDOpT21)
022000990129     D**FL608                              Like(SlInDOpT21)
022100990129     D**FL708                              Like(SlInDOpT21)
022200990129     D FL8             S                   DIM(7) Like(SlInDOpT21)
022300950316     D* ROTTURA 9
022400990129     D*DS9             DS                   inz
022500990129     D**FL109                              Like(SlInDOpT21)
022600990129     D**FL209                              Like(SlInDOpT21)
022700990129     D**FL309                              Like(SlInDOpT21)
022800990129     D**FL409                              Like(SlInDOpT21)
022900990129     D**FL509                              Like(SlInDOpT21)
023000990129     D**FL609                              Like(SlInDOpT21)
023100990129     D**FL709                              Like(SlInDOpT21)
023200990129     D FL9             S                   DIM(7) Like(SlInDOpT21)
023300950316     D* ROTTURA 10
023400990129     D*DS10            DS                   inz
023500990129     D**FL100                              Like(SlInDOpT21)
023600990129     D**FL200                              Like(SlInDOpT21)
023700990129     D**FL300                              Like(SlInDOpT21)
023800990129     D**FL400                              Like(SlInDOpT21)
023900990129     D**FL500                              Like(SlInDOpT21)
024000990129     D**FL600                              Like(SlInDOpT21)
024100990129     D**FL700                              Like(SlInDOpT21)
024200990129     D FL0             S                   DIM(7) Like(SlInDOpT21)
024300950321     D* TOTALE GENERALE
024400990129     D*DSTG            DS                   inz
024500990129     D**FL10G                              Like(SlInDOpT21)
024600990129     D**FL20G                              Like(SlInDOpT21)
024700990129     D**FL30G                              Like(SlInDOpT21)
024800990129     D**FL40G                              Like(SlInDOpT21)
024900990129     D**FL50G                              Like(SlInDOpT21)
025000990129     D**FL60G                              Like(SlInDOpT21)
025100990129     D**FL70G                              Like(SlInDOpT21)
025200990129     D FLG             S                   DIM(7) Like(SlInDOpT21)
025300970813     D*-------------
025400970814     D DtRif           s               D    dim(7)
025500970813     D FaiLaCall       S              1a
025600970813     D x               S              3  0
025700970814     D y               S              3  0
025800981126     D WSocieta        S                   like(BRASocieta)
025900981126     D WBanca          S                   like(BRABanca)
026000981126     D WLinea          S                   like(BRALinea)
026100000117C1309D ZDateISO        S               D
026200981126     D*-------------
026300981127     D* Ridenomino i campi di ANBRA01L
026400981126     IANBRA001
026500981126     I              BRASocieta                  WRASocieta
026600981126     I              BRABanca                    WRABanca
026700981126     I              BRALinea                    WRALinea
026800970813     D*-------------
026900970813     C/EJECT
027000940830     C*----------------------------------------------------*
027100940830     C*                MAIN LINE PROGRAM
027200940830     C*----------------------------------------------------*
027300940830     C*
027400940830     C* inizializzazione variabili
027500940830     C                   EXSR      INZVAR
027600020125     C*
027700020125C1540C                   If        NomFilT51     <> *blanks
027800020125  "  C* Creazione o pulizia file per scarico
027900020125  "  C                   Exsr      CallCT52C1
028000020125  "   * Ovrdbf  NdSIB
028100020125  "  C                   Exsr      OvrNdSIB
028200020125  "  C                   Open      NDSIB00W
028300020125C1540C                   EndIf
028400950131     C*
028500950131     C* Reperimento schiera per SQL
028600950131     C                   EXSR      REPSQL
028700941129     C*
028800950131     C* Esecuzione SQL
028900950131     C                   EXSR      EXESQL
029000020125     C*
029100020125C1540C* Iscrizione del file scarico nel catalogo
029200020125  "  C                   If        NomFilT51     <> *blanks
029300020125  "  C                   Exsr      CallX24
029400020125C1540C                   EndIf
029500940830     C*
029600940830     C* fine programma
029700940830     C                   EXSR      ENDPGM
029800940830     C*
029900940830     C/EJECT
030000940830     C************************************************************
030100940830     C* FINE PROGRAMMA
030200940830     C************************************************************
030300940830     C     ENDPGM        BEGSR
030400940830     C*
030500940830     C                   SETON                                            LR
030600940905     C                   RETURN
030700940830     C*
030800940830     C                   ENDSR
030900950127     C/EJECT
031000940830     C************************************************************
031100950131     C* REPERIMENTO SCHIERA PER SQL
031200940830     C************************************************************
031300950131     C     REPSQL        BEGSR
031400970813     C*
031500970813     C* Imposta la select
031600970813     C     SQ            CAT       SKK:0         SQ
031700970813     C*
031800970813     C* Reperisco società o lista
031900970813     C                   EXSR      RepSocieta
032000970813     C* aggiungo società o lista
032100970813     C     SQ            CAT       SK1X:1        SQ
032200950131     C*
032300950131     C* Reperisco selezioni/ordinamenti
032400950131     C*
032500950215     C                   MOVE      MPESOC        SOC969
032600950208     C                   Z-ADD     MPEREG        NUM969
032700950508     C                   Z-ADD     LAN968        LAN969
032800950131     C                   CALL      'ANA969R'
032900950131     C                   PARM                    KPJBA
033000960527     C                   PARM                    ANA969DS
033100941129     C*
033200950131     C     ERR969        IFNE      *ON
033300950131     C                   MOVEA     S969          SEL
033400950131     C                   MOVEA     O969          ORD
033500950131     C* Se esistono altre selezioni aggiungo 'AND' per l'sql
033600950131     C     SELX          IFNE      *BLANKS
033700950131     C     SQ            CAT       'AND':1       SQ
033800950131     C                   ENDIF
033900950131     C*
034000950131     C     SQ            CAT       SELX:1        SQ
034100950131     C     SQ            CAT       ORDX:1        SQ
034200950629     C     SQ            CAT       FETCH:1       SQ
034300950131     C                   ENDIF
034400950202     C*
034500950206     C* Riempio schiera campi ordinamento, tipi rottura e titoli
034600950202     C*
034700950206     C                   MOVEL     TRC969        TRCX
034800950202     C                   MOVEL     FLD969        FLDX
034900950202     C                   MOVEL     ROT969        ROTX
035000950206     C                   MOVEL     DEB969        TITX
035100950629     C*
035200950629     C* Reperisco dal repository i dati dei campi delle rotture
035300950629     C* e li memorizzo nelle schiere
035400950629     C                   EXSR      REPREP
035500950201     C*
035600940905     C                   ENDSR
035700970813     C/EJECT
035800970813     C************************************************************
035900970813     C* REPERIMENTO società o lista di società
036000970813     C************************************************************
036100970813     C     REPSocieta    BEGSR
036200970813     C*
036300970813     C* Reperisce la socità o la lista di società richiesta
036400970813     c                   z-add     21            $x                3 0
036500970813     c                   if        ListSocT51 <> *blanks
036600970813     c     ListSocT51    setll     anlss01l
036700970813     c                   DO        *hival
036800970813     c     ListSocT51    reade     anlss01l                               21
036900970813     c   21              leave
037000970813     C                   MOVEA     lssSocieta    SK2(2)
037100970813     C                   MOVEA     sk2           SK1($x)
037200970813     c                   add       7             $X
037300970813     c                   enddo
037400970813     C* se ultima società della lista, metto la parentesi sulla virgola
037500970813     c                   sub       2             $X
037600970813     C                   MOVEA     ')'           SK1($X)
037700970813     c                   else
037800970813     C                   MOVEA     SocietaT51    SK2(2)
037900970813     C                   MOVEA     sk2           SK1($x)
038000970813     c                   add       5             $X
038100970813     C                   MOVEA     ')'           SK1($X)
038200970813     c                   endif
038300970813     c*
038400970813     C                   ENDSR
038500940905     C/EJECT
038600940905     C******************************************************
038700950131     C* ESECUZIONE SQL
038800940905     C******************************************************
038900950131     C     EXESQL        BEGSR
039000950131     C*
039100950131     C/EXEC SQL
039200950131     C+     PREPARE S1 FROM :SQ
039300950131     C/END-EXEC
039400950131     C*
039500950131     C/EXEC SQL
039600950131     C+     DECLARE A1 CURSOR FOR S1
039700950131     C/END-EXEC
039800950131     C*
039900950131     C/EXEC SQL
040000950131     C+     OPEN A1
040100950131     C/END-EXEC
040200030731     C*
040300030731D1414c                   move      *on           OkSqlCod          1
040400950131     C*
040500950201     C/EXEC SQL
040600970808     C+     FETCH A1 FOR 100 ROWS INTO :ANBRA
040700950201     C/END-EXEC
040800950131     C*
040900950718     C*
041000950718     C                   Z-ADD     1             C$
041100970808     C     C$            OCCUR     ANBRA
041200030731     C*
041300030731D1414c                   exsr      ImpSqlCod
041400970808     C*
041500030731D1414X***  SQLCOD        DOWEQ     0
041600030731D1414C     OkSqlCod      DOWEQ     *on
041700950202     C*
041800950202     C* Ricerca e salvataggio campi rottura
041900950206     C                   EXSR      DEFKEY
042000950202     C*
042100950202     C*  Operazioni a rottura di chiave 1
042200950202     C*
042300950202     C                   MOVE      LET(1)        SAV(1)
042400951025     C                   EXSR      Rottura1
042500950202     C*
042600950202   B2C     LET(1)        DOWEQ     SAV(1)
042700030731D1414x***  SQLCOD        ANDEQ     0
042800030731D1414C     OkSqlCod      andeq     *on
042900950202     C*
043000950202     C*  Operazioni a rottura di chiave 2
043100950202     C*
043200950202     C                   MOVE      LET(2)        SAV(2)
043300951025     C                   EXSR      Rottura2
043400950202     C*
043500950202   B3C     LET(2)        DOWEQ     SAV(2)
043600950202     C     LET(1)        ANDEQ     SAV(1)
043700030731D1414x***  SQLCOD        ANDEQ     0
043800030731D1414C     OkSqlCod      andeq     *on
043900950202     C*
044000950202     C*  Operazioni a rottura di chiave 3
044100950202     C*
044200950202     C                   MOVE      LET(3)        SAV(3)
044300951025     C                   EXSR      Rottura3
044400950202     C*
044500950202   B4C     LET(3)        DOWEQ     SAV(3)
044600950202     C     LET(2)        ANDEQ     SAV(2)
044700950202     C     LET(1)        ANDEQ     SAV(1)
044800030731D1414x***  SQLCOD        ANDEQ     0
044900030731D1414C     OkSqlCod      andeq     *on
045000950202     C*
045100950202     C*  Operazioni a rottura di chiave 4
045200950202     C*
045300950202     C                   MOVE      LET(4)        SAV(4)
045400951025     C                   EXSR      Rottura4
045500950202     C*
045600950202   B5C     LET(4)        DOWEQ     SAV(4)
045700950202     C     LET(3)        ANDEQ     SAV(3)
045800950202     C     LET(2)        ANDEQ     SAV(2)
045900950202     C     LET(1)        ANDEQ     SAV(1)
046000030731D1414x***  SQLCOD        ANDEQ     0
046100030731D1414C     OkSqlCod      andeq     *on
046200950202     C*
046300950202     C*  Operazioni a rottura di chiave 5
046400950202     C*
046500950202     C                   MOVE      LET(5)        SAV(5)
046600951025     C                   EXSR      Rottura5
046700950202     C*
046800950202   B6C     LET(5)        DOWEQ     SAV(5)
046900950202     C     LET(4)        ANDEQ     SAV(4)
047000950202     C     LET(3)        ANDEQ     SAV(3)
047100950202     C     LET(2)        ANDEQ     SAV(2)
047200950202     C     LET(1)        ANDEQ     SAV(1)
047300030731D1414x***  SQLCOD        ANDEQ     0
047400030731D1414C     OkSqlCod      andeq     *on
047500950202     C*
047600950202     C*  Operazioni a rottura di chiave 6
047700950202     C*
047800950202     C                   MOVE      LET(6)        SAV(6)
047900951025     C                   EXSR      Rottura6
048000950202     C*
048100950202   B7C     LET(6)        DOWEQ     SAV(6)
048200950202     C     LET(5)        ANDEQ     SAV(5)
048300950202     C     LET(4)        ANDEQ     SAV(4)
048400950202     C     LET(3)        ANDEQ     SAV(3)
048500950202     C     LET(2)        ANDEQ     SAV(2)
048600950202     C     LET(1)        ANDEQ     SAV(1)
048700030731D1414x***  SQLCOD        ANDEQ     0
048800030731D1414C     OkSqlCod      andeq     *on
048900950202     C*
049000950202     C*  Operazioni a rottura di chiave 7
049100950202     C*
049200950202     C                   MOVE      LET(7)        SAV(7)
049300951025     C                   EXSR      Rottura7
049400950202     C*
049500950202   B8C     LET(7)        DOWEQ     SAV(7)
049600950202     C     LET(6)        ANDEQ     SAV(6)
049700950202     C     LET(5)        ANDEQ     SAV(5)
049800950202     C     LET(4)        ANDEQ     SAV(4)
049900950202     C     LET(3)        ANDEQ     SAV(3)
050000950202     C     LET(2)        ANDEQ     SAV(2)
050100950202     C     LET(1)        ANDEQ     SAV(1)
050200030731D1414x***  SQLCOD        ANDEQ     0
050300030731D1414C     OkSqlCod      andeq     *on
050400950202     C*
050500950202     C*  Operazioni a rottura di chiave 8
050600950202     C*
050700950202     C                   MOVE      LET(8)        SAV(8)
050800951025     C                   EXSR      Rottura8
050900950202     C*
051000950202   B9C     LET(8)        DOWEQ     SAV(8)
051100950202     C     LET(7)        ANDEQ     SAV(7)
051200950202     C     LET(6)        ANDEQ     SAV(6)
051300950202     C     LET(5)        ANDEQ     SAV(5)
051400950202     C     LET(4)        ANDEQ     SAV(4)
051500950202     C     LET(3)        ANDEQ     SAV(3)
051600950202     C     LET(2)        ANDEQ     SAV(2)
051700950202     C     LET(1)        ANDEQ     SAV(1)
051800030731D1414x***  SQLCOD        ANDEQ     0
051900030731D1414C     OkSqlCod      andeq     *on
052000950202     C*
052100950202     C*  Operazioni a rottura di chiave 9
052200950202     C*
052300950202     C                   MOVE      LET(9)        SAV(9)
052400951025     C                   EXSR      Rottura9
052500950202     C*
052600950202  B10C     LET(9)        DOWEQ     SAV(9)
052700950202     C     LET(8)        ANDEQ     SAV(8)
052800950202     C     LET(7)        ANDEQ     SAV(7)
052900950202     C     LET(6)        ANDEQ     SAV(6)
053000950202     C     LET(5)        ANDEQ     SAV(5)
053100950202     C     LET(4)        ANDEQ     SAV(4)
053200950202     C     LET(3)        ANDEQ     SAV(3)
053300950202     C     LET(2)        ANDEQ     SAV(2)
053400950202     C     LET(1)        ANDEQ     SAV(1)
053500030731D1414x***  SQLCOD        ANDEQ     0
053600030731D1414C     OkSqlCod      andeq     *on
053700950202     C*
053800950202     C*  Operazioni a rottura di chiave 10
053900950202     C*
054000950202     C                   MOVE      LET(10)       SAV(10)
054100951025     C                   EXSR      Rottura10
054200950202     C*
054300950202  B11C     LET(10)       DOWEQ     SAV(10)
054400950202     C     LET(9)        ANDEQ     SAV(9)
054500950202     C     LET(8)        ANDEQ     SAV(8)
054600950202     C     LET(7)        ANDEQ     SAV(7)
054700950202     C     LET(6)        ANDEQ     SAV(6)
054800950202     C     LET(5)        ANDEQ     SAV(5)
054900950202     C     LET(4)        ANDEQ     SAV(4)
055000950202     C     LET(3)        ANDEQ     SAV(3)
055100950202     C     LET(2)        ANDEQ     SAV(2)
055200950202     C     LET(1)        ANDEQ     SAV(1)
055300030731D1414x***  SQLCOD        ANDEQ     0
055400030731D1414C     OkSqlCod      andeq     *on
055500950131     C*
055600950202     C* Stampa dettaglio e calcoli per totali
055700960517     C                   EXSR      StampaDet
055800950201     C*
055900950718     C     C$            IFEQ      SQLER3
056000950201     C/EXEC SQL
056100970808     C+     FETCH A1 FOR 100 ROWS INTO :ANBRA
056200950201     C/END-EXEC
056300950718     C                   Z-ADD     1             C$                3 0
056400950718     C                   ELSE
056500950718     C                   ADD       1             C$
056600950718     C                   ENDIF
056700950718     C*
056800970808     C     C$            OCCUR     ANBRA
056900030731     C*
057000030731D1414c                   exsr      ImpSqlCod
057100950718     C*
057200950206     C                   EXSR      DEFKEY
057300950202     C*
057400950202  E11C                   ENDDO
057500950202     C*
057600950202     C* Spengo indicatore di  salto pagina
057700950202     C                   SETOFF                                       85
057800950202     C*
057900950202     C* Totalizzazione chiave 10 se richiesta
058000950202     C*
058100950202     C     ROT(10)       IFEQ      '2'
058200950202     C     ROT(10)       OREQ      '3'
058300951025     C                   EXSR      Totale10
058400950202     C                   ENDIF
058500950202  E10C                   ENDDO
058600950202     C*
058700950202     C* Totalizzazione chiave  9 se richiesta
058800950202     C*
058900950202     C     ROT(9)        IFEQ      '2'
059000950202     C     ROT(9)        OREQ      '3'
059100951025     C                   EXSR      Totale9
059200950202     C                   ENDIF
059300950202   E9C                   ENDDO
059400950202     C*
059500950202     C* Totalizzazione chiave  8 se richiesta
059600950202     C*
059700950202     C     ROT(8)        IFEQ      '2'
059800950202     C     ROT(8)        OREQ      '3'
059900951025     C                   EXSR      Totale8
060000950202     C                   ENDIF
060100950202   E8C                   ENDDO
060200950202     C*
060300950202     C* Totalizzazione chiave  7 se richiesta
060400950202     C*
060500950202     C     ROT(7)        IFEQ      '2'
060600950202     C     ROT(7)        OREQ      '3'
060700951025     C                   EXSR      Totale7
060800950202     C                   ENDIF
060900950202   E7C                   ENDDO
061000950202     C*
061100950202     C* Totalizzazione chiave  6 se richiesta
061200950202     C*
061300950202     C     ROT(6)        IFEQ      '2'
061400950202     C     ROT(6)        OREQ      '3'
061500951025     C                   EXSR      Totale6
061600950202     C                   ENDIF
061700950202   E6C                   ENDDO
061800950202     C*
061900950202     C* Totalizzazione chiave  5 se richiesta
062000950202     C*
062100950202     C     ROT(5)        IFEQ      '2'
062200950202     C     ROT(5)        OREQ      '3'
062300951025     C                   EXSR      Totale5
062400950202     C                   ENDIF
062500950202   E5C                   ENDDO
062600950202     C*
062700950202     C* Totalizzazione chiave  4 se richiesta
062800950202     C*
062900950202     C     ROT(4)        IFEQ      '2'
063000950202     C     ROT(4)        OREQ      '3'
063100951025     C                   EXSR      Totale4
063200950202     C                   ENDIF
063300950202   E4C                   ENDDO
063400950202     C*
063500950202     C* Totalizzazione chiave  3 se richiesta
063600950202     C*
063700950202     C     ROT(3)        IFEQ      '2'
063800950202     C     ROT(3)        OREQ      '3'
063900951025     C                   EXSR      Totale3
064000950202     C                   ENDIF
064100950202   E3C                   ENDDO
064200950202     C*
064300950202     C* Totalizzazione chiave  2 se richiesta
064400950202     C*
064500950202     C     ROT(2)        IFEQ      '2'
064600950202     C     ROT(2)        OREQ      '3'
064700951025     C                   EXSR      Totale2
064800950202     C                   ENDIF
064900950202   E2C                   ENDDO
065000950202     C*
065100950202     C* Totalizzazione chiave  1 se richiesta
065200950202     C*
065300950202     C     ROT(1)        IFEQ      '2'
065400950202     C     ROT(1)        OREQ      '3'
065500951025     C                   EXSR      Totale1
065600950202     C                   ENDIF
065700950202   E1C                   ENDDO
065800950202     C*
065900950202     C* Totale generale
066000950202     C*
066100951025     C                   EXSR      TOTGEN
066200950131     C*
066300950131     C/EXEC SQL
066400950131     C+     CLOSE A1
066500950131     C/END-EXEC
066600950131     C*
066700950131     C*
066800940905     C                   ENDSR
066900940905     C/EJECT
067000030731D1414C************************************************************
067100030731  "  C* Imposta Ok sqlcod dopo occur
067200030731D1414C************************************************************
067300030731     C     ImpSqlCod     BEGSR
067400030731     c*
067500030731     c                   select
067600030731     c* Errore sql eseguo cambio job per ottenere spool del joblog
067700030731     c                   when      sqlcod < 0
067800030731     c                   call      'X66CHGJOB'
067900030731     c                   eval      OkSqlCod = *off
068000030731     c* fine lettura
068100030731     c                   when      sqlcod = 100
068200030731     c                   eval      OkSqlCod = *off
068300030731     c* ok sql
068400030731     c                   other
068500030731     c                   eval      OkSqlCod = *on
068600030731     c*
068700030731     C                   endsl
068800030731     c*
068900030731D1414c                   endsr
069000950202     C************************************************************
069100950202     C* REPERIMENTO E SALVATAGGIO CAMPI CHIAVE
069200950202     C************************************************************
069300950206     C     DEFKEY        BEGSR
069400950202     C*
069500950202     C* Memorizza campi chiave letti in campi della schiera LET
069600950202     C*
069700950629   B1C                   DO        10            $X                3 0
069800950202     C*
069900950206   B2C     FLD($X)       IFNE      *BLANKS
070000950315     C*
070100950629   B3C     LUN($X)       IFNE      0
070200950629     C     POS($X)       ANDNE     0
070300950629     C     TIP($X)       ANDNE     *BLANKS
070400950316     C*
070500950316     C* Chiamo pgm per estrarre campo da record (valore del campo)
070600950316     C*
070700950203     C                   SELECT
070800970808     C*
070900970808     C     TRC($X)       WHENEQ    'ANBRA000'
071000970522     C                   CALLB     'ANA962R'
071100970808     C                   PARM      ANBRA         DAT962
071200950202     C                   PARM                    FLD962           30
071300950629     C                   PARM      LUN($X)       LUN962            3 0
071400950629     C                   PARM      POS($X)       POS962            5 0
071500950629     C                   PARM      TIP($X)       TIP962            1
071600950202     C                   MOVEL     FLD962        LET($X)
071700960528     C*
071800950203     C                   ENDSL
071900950202     C*
072000950206   E3C                   ENDIF
072100950203   E2C                   ENDIF
072200950203   E1C                   ENDDO
072300950202     C*
072400950202     C                   ENDSR
072500950629     C/EJECT
072600950629     C************************************************************
072700950629     C* REPERIMENTO dati campi delle rotture
072800950629     C************************************************************
072900950629     C     REPREP        BEGSR
073000950629     C*
073100950629     C* Reperisce dal repository la lunghezza e il tipo dei campi di rottura
073200950629     C*
073300950629   B1C                   DO        10            $X                3 0
073400950629     C*
073500950629   B2C     FLD($X)       IFNE      *BLANKS
073600950629     C*
073700960527     C                   RESET                   ANA967DS
073800950629     C                   MOVE      TRC($X)       TRC967
073900950629     C                   MOVE      FLD($X)       FLD967
074000960527     C                   MOVEL     ANA967DS      KPJBU
074100970522     C                   CALLB     'ANA967R'
074200950629     C                   PARM                    KPJBA
074300960527     C                   MOVEL     KPJBU         ANA967DS
074400950629     C*
074500950629   B3C     ERR967        IFEQ      *OFF
074600950629     C*
074700950629     C                   Z-ADD     LUN967        LUN($X)
074800950629     C                   Z-ADD     POS967        POS($X)
074900950629     C                   MOVE      TIP967        TIP($X)
075000950629     C                   MOVE      RGC967        RGC($X)
075100950629     C*
075200950629   E3C                   ENDIF
075300950629   E2C                   ENDIF
075400950629   E1C                   ENDDO
075500950629     C                   ENDSR
075600950210     C/EJECT
075700950210     C********************************************************************
075800950210     C/SPACE 5
075900950210     C********************************************************************
076000950210     C* Stampa testata e gestione overflow
076100950210     C*
076200950210     C     TESTAT        BEGSR
076300950210     c*
076400951025     C                   WRITE     TESTATA
076500950210     C*
076600950210     C* Spengo se acceso indicatore di overflow
076700950210     C*
076800950210     C   39              SETOFF                                       39
076900950210     C*
077000950210     C                   ENDSR
077100950210     C*
077200950202     C********************************************************************
077300950202     C/SPACE 5
077400950202     C********************************************************************
077500950202     C* Operazioni a rottura della chiave 1
077600950202     C*
077700951025     C     Rottura1      BEGSR
077800950202     C*
077900990129     C************       CLEAR                   DS1
078000990129     C                   CLEAR                   FL1
078100950202     C*
078200950202     C* Utilizzo questo flags per sapere se ho rotto la chiave e di
078300950202     C* conseguenza non stampare la testata del salto pagina.
078400950202     C*
078500950203     C                   MOVE      'R'           ROT01             1
078600950202     C*
078700950202     C* Se non chiesto salto pagina della prima chiave stampo primo titolo
078800950202     C*
078900950202     C     ROT(1)        IFNE      '1'
079000950202     C     ROT(1)        ANDNE     '3'
079100950202     C     WRKFLG        IFEQ      ' '
079200950210     C                   EXSR      TESTAT
079300950202     C                   MOVE      '1'           WRKFLG            1
079400950202     C                   SETON                                        85
079500950202     C                   ENDIF
079600950202     C                   ENDIF
079700950202     C*
079800950210     C* Salto pagina se richiesto o se overflow
079900950202     C*
080000950202     C     ROT(1)        IFEQ      '1'
080100950202     C     ROT(1)        OREQ      '3'
080200950210     C     *IN39         OREQ      *ON
080300950210     c*
080400950210     C     *IN85         IFEQ      *OFF
080500950210     C     *IN39         OREQ      *ON
080600950210     C                   EXSR      TESTAT
080700950202     C                   SETON                                        85
080800950207     C                   ENDIF
080900950210     C*
081000950210     C                   ENDIF
081100950202     C*
081200950207     C* Titolo se richiesto
081300950202     C*
081400950207     C     ROT(1)        IFNE      '0'
081500950208     C     ROT(1)        ANDNE     ' '
081600951025     C                   CLEAR                   L1rottura
081700951025     C                   MOVEL     TIT(1)        L1Campo
081800950202     C                   MOVEL     SAV(1)        WRKKEY           15
081900951025     C                   MOVEL     WRKKEY        L1chiave
082000950202     C*
082100950202     C*  Eseguo decodifica titoli
082200950202     C*
082300950203     C                   MOVEL     TRC(1)        WRKTRC
082400950203     C                   MOVEL     FLD(1)        WRKFLD
082500950629     C                   MOVEL     RGC(1)        WRKRGC
082600950206     C                   EXSR      DECTIT
082700951025     C                   MOVEL     WRKDES        L1decodif
082800951025     C*
082900951025      * Salvo decodifiche chiave per stamparle a rottura di totale
083000951025     C* senza reperirle di nuovo
083100951025     C                   MOVEL     WRKKEY        SkCod(1)
083200951025     C                   MOVEL     WRKDES        SkDescr(1)
083300960524     C                   MOVEL     L1campo       SkCampo(1)
083400950202     C*
083500950206     C     TIT(1)        IFNE      *BLANKS
083600970818     C                   WRITE     RigaVuota
083700970818     C                   WRITE     L1rottura
083800950206     C                   ENDIF
083900950207     C                   ENDIF
084000950202     C*
084100950202     C                   ENDSR
084200950202     C/EJECT
084300950202     C********************************************************************
084400950202     C/SPACE 5
084500950202     C********************************************************************
084600950202     C* Operazioni a rottura della chiave 2
084700950202     C*
084800951025     C     Rottura2      BEGSR
084900950202     C*
085000990129     C************       CLEAR                   DS2
085100990129     C                   CLEAR                   FL2
085200950202     C*
085300950202     C* Utilizzo questo flags per sapere se ho rotto la chiave e di
085400950202     C* conseguenza non stampare la testata del salto pagina.
085500950202     C*
085600950203     C                   MOVE      'R'           ROT02             1
085700950202     C*
085800950210     C* Salto pagina se richiesto o se overflow
085900950202     C*
086000950202     C     ROT(2)        IFEQ      '1'
086100950202     C     ROT(2)        OREQ      '3'
086200950210     C     *IN39         OREQ      *ON
086300950207     C*
086400950210     C     *IN85         IFEQ      *OFF
086500950210     C     *IN39         OREQ      *ON
086600950210     C                   EXSR      TESTAT
086700950202     C                   SETON                                        85
086800950207     C                   ENDIF
086900950207     C                   ENDIF
087000950207     C*
087100950207     C* Titolo se richiesto
087200950207     C*
087300950207     C     ROT(2)        IFNE      '0'
087400950208     C     ROT(2)        ANDNE     ' '
087500951025     C                   CLEAR                   L2rottura
087600951025     C                   MOVEL     TIT(2)        L2Campo
087700950202     C                   MOVEL     SAV(2)        WRKKEY           15
087800951025     C                   MOVEL     WRKKEY        L2chiave
087900950202     C*
088000950202     C*  Eseguo decodifica titoli
088100950203     C                   MOVEL     TRC(2)        WRKTRC
088200950203     C                   MOVEL     FLD(2)        WRKFLD
088300950629     C                   MOVEL     RGC(2)        WRKRGC
088400950206     C                   EXSR      DECTIT
088500951025     C                   MOVEL     WRKDES        L2decodif
088600951025      *
088700951025     C                   MOVEL     WRKKEY        SkCod(2)
088800951025     C                   MOVEL     WRKDES        SkDescr(2)
088900960524     C                   MOVEL     L2campo       SkCampo(2)
089000950203     C*
089100950209     C* Stampo riga vuota se stampati i totali precedenti, ma non l'int
089200950209     C* estazione precedente
089300950209     C     ROT(2)        IFEQ      '2'
089400950209     C     ROT01         ANDNE     'R'
089500951025     C                   WRITE     RigaVuota
089600950209     C                   ENDIF
089700950206     C     TIT(2)        IFNE      *BLANKS
089800951025     C                   WRITE     L2rottura
089900950206     C                   ENDIF
090000950206     C                   ENDIF
090100950202     C*
090200950202     C                   ENDSR
090300950202     C********************************************************************
090400950202     C/SPACE 5
090500950202     C********************************************************************
090600950202     C* Operazioni a rottura della chiave 3
090700950202     C*
090800951025     C     Rottura3      BEGSR
090900950202     C*
091000990129     C************       CLEAR                   DS3
091100990129     C                   CLEAR                   FL3
091200950202     C*
091300950202     C* Utilizzo questo flags per sapere se ho rotto la chiave e di
091400950202     C* conseguenza non stampare la testata del salto pagina.
091500950202     C*
091600950203     C                   MOVE      'R'           ROT03             1
091700950202     C*
091800950210     C* Salto pagina se richiesto o se overflow
091900950202     C*
092000950202     C     ROT(3)        IFEQ      '1'
092100950202     C     ROT(3)        OREQ      '3'
092200950210     C     *IN39         OREQ      *ON
092300950207     C*
092400950210     C     *IN85         IFEQ      *OFF
092500950210     C     *IN39         OREQ      *ON
092600950210     C                   EXSR      TESTAT
092700950202     C                   SETON                                        85
092800950207     C                   ENDIF
092900950210     C*
093000950207     C                   ENDIF
093100950207     C*
093200950207     C* Titolo se richiesto
093300950207     C*
093400950207     C     ROT(3)        IFNE      '0'
093500950208     C     ROT(3)        ANDNE     ' '
093600951025     C                   CLEAR                   L3rottura
093700951025     C                   MOVEL     TIT(3)        L3Campo
093800950202     C                   MOVEL     SAV(3)        WRKKEY           15
093900951025     C                   MOVEL     WRKKEY        L3chiave
094000950202     C*
094100950202     C*  Eseguo decodifica titoli
094200950202     C*
094300950203     C                   MOVEL     TRC(3)        WRKTRC
094400950203     C                   MOVEL     FLD(3)        WRKFLD
094500950629     C                   MOVEL     RGC(3)        WRKRGC
094600950206     C                   EXSR      DECTIT
094700951025     C                   MOVEL     WRKDES        L3decodif
094800951025      * Salvo decodifiche
094900951025     C                   MOVEL     WRKKEY        SkCod(3)
095000951025     C                   MOVEL     WRKDES        SkDescr(3)
095100960524     C                   MOVEL     L3campo       SkCampo(3)
095200950206     C*
095300950209     C* Stampo riga vuota se stampati i totali precedenti, ma non l'int
095400950209     C* estazione precedente
095500950209     C     ROT(3)        IFEQ      '2'
095600950209     C     ROT02         ANDNE     'R'
095700951025     C                   WRITE     RigaVuota
095800950209     C                   ENDIF
095900950206     C     TIT(3)        IFNE      *BLANKS
096000951025     C                   WRITE     L3rottura
096100950206     C                   ENDIF
096200950206     C                   ENDIF
096300950202     C*
096400950202     C                   ENDSR
096500950202     C********************************************************************
096600950202     C/SPACE 5
096700950202     C********************************************************************
096800950202     C* Operazioni a rottura della chiave 4
096900950202     C*
097000951025     C     Rottura4      BEGSR
097100950202     C*
097200990129     C************       CLEAR                   DS4
097300990129     C                   CLEAR                   FL4
097400950202     C*
097500950202     C* Utilizzo questo flags per sapere se ho rotto la chiave e di
097600950202     C* conseguenza non stampare la testata del salto pagina.
097700950202     C*
097800950203     C                   MOVE      'R'           ROT04             1
097900950202     C*
098000950202     C* Salto pagina se richiesto
098100950202     C*
098200950202     C     ROT(4)        IFEQ      '1'
098300950202     C     ROT(4)        OREQ      '3'
098400950210     C     *IN39         OREQ      *ON
098500950207     C*
098600950210     C     *IN85         IFEQ      *OFF
098700950210     C     *IN39         OREQ      *ON
098800950210     C                   EXSR      TESTAT
098900950202     C                   SETON                                        85
099000950207     C                   ENDIF
099100950207     C                   ENDIF
099200950207     C*
099300950207     C* Titolo se richiesto
099400950207     C*
099500950207     C     ROT(4)        IFNE      '0'
099600950208     C     ROT(4)        ANDNE     ' '
099700951025     C                   CLEAR                   L4rottura
099800951025     C                   MOVEL     TIT(4)        L4Campo
099900950202     C                   MOVEL     SAV(4)        WRKKEY           15
100000951025     C                   MOVEL     WRKKEY        L4chiave
100100950202     C*
100200950202     C*  Eseguo decodifica titoli
100300950202     C*
100400950203     C                   MOVEL     TRC(4)        WRKTRC
100500950203     C                   MOVEL     FLD(4)        WRKFLD
100600950629     C                   MOVEL     RGC(4)        WRKRGC
100700950206     C                   EXSR      DECTIT
100800951025     C                   MOVEL     WRKDES        L4decodif
100900951025      * Salvo decodifiche
101000951025     C                   MOVEL     WRKKEY        SkCod(4)
101100951025     C                   MOVEL     WRKDES        SkDescr(4)
101200960524     C                   MOVEL     L4campo       SkCampo(4)
101300950202     C*
101400950209     C* Stampo riga vuota se stampati i totali precedenti, ma non l'int
101500950209     C* estazione precedente
101600950209     C     ROT(4)        IFEQ      '2'
101700950209     C     ROT03         ANDNE     'R'
101800951025     C                   WRITE     RigaVuota
101900950209     C                   ENDIF
102000950206     C     TIT(4)        IFNE      *BLANKS
102100951025     C                   WRITE     L4rottura
102200950206     C                   ENDIF
102300950206     C                   ENDIF
102400950202     C*
102500950202     C                   ENDSR
102600950202     C********************************************************************
102700950202     C/SPACE 5
102800950202     C********************************************************************
102900950202     C* Operazioni a rottura della chiave 1
103000950202     C*
103100951025     C     Rottura5      BEGSR
103200950202     C*
103300990129     C************       CLEAR                   DS5
103400990129     C                   CLEAR                   FL5
103500950202     C*
103600950202     C* Utilizzo questo flags per sapere se ho rotto la chiave e di
103700950202     C* conseguenza non stampare la testata del salto pagina.
103800950202     C*
103900950203     C                   MOVE      'R'           ROT05             1
104000950202     C*
104100950210     C* Salto pagina se richiesto o se overflow
104200950202     C*
104300950202     C     ROT(5)        IFEQ      '1'
104400950202     C     ROT(5)        OREQ      '3'
104500950210     C     *IN39         OREQ      *ON
104600950207     C*
104700950210     C     *IN85         IFEQ      *OFF
104800950210     C     *IN39         OREQ      *ON
104900950210     C                   EXSR      TESTAT
105000950202     C                   SETON                                        85
105100950207     C                   ENDIF
105200950207     C                   ENDIF
105300950207     C*
105400950207     C* Titolo se richiesto
105500950207     C*
105600950207     C     ROT(5)        IFNE      '0'
105700950208     C     ROT(5)        ANDNE     ' '
105800951025     C                   CLEAR                   L5rottura
105900951025     C                   MOVEL     TIT(5)        L5Campo
106000950202     C                   MOVEL     SAV(5)        WRKKEY           15
106100951025     C                   MOVEL     WRKKEY        L5chiave
106200950202     C*
106300950202     C*  Eseguo decodifica titoli
106400950202     C*
106500950203     C                   MOVEL     TRC(5)        WRKTRC
106600950203     C                   MOVEL     FLD(5)        WRKFLD
106700950629     C                   MOVEL     RGC(5)        WRKRGC
106800950206     C                   EXSR      DECTIT
106900951025     C                   MOVEL     WRKDES        L5decodif
107000951025      * Salvo decodifiche
107100951025     C                   MOVEL     WRKKEY        SkCod(5)
107200951025     C                   MOVEL     WRKDES        SkDescr(5)
107300960524     C                   MOVEL     L5campo       SkCampo(5)
107400950209     C*
107500950209     C* Stampo riga vuota se stampati i totali precedenti, ma non l'int
107600950209     C* estazione precedente
107700950209     C     ROT(5)        IFEQ      '2'
107800950209     C     ROT04         ANDNE     'R'
107900951025     C                   WRITE     RigaVuota
108000950209     C                   ENDIF
108100950206     C*
108200950206     C     TIT(5)        IFNE      *BLANKS
108300951025     C                   WRITE     L5rottura
108400950206     C                   ENDIF
108500950206     C                   ENDIF
108600950202     C*
108700950202     C                   ENDSR
108800950202     C********************************************************************
108900950202     C/SPACE 5
109000950202     C********************************************************************
109100950202     C* Operazioni a rottura della chiave 6
109200950202     C*
109300951025     C     Rottura6      BEGSR
109400950202     C*
109500990129     C************       CLEAR                   DS6
109600990129     C                   CLEAR                   FL6
109700950202     C*
109800950202     C* Utilizzo questo flags per sapere se ho rotto la chiave e di
109900950202     C* conseguenza non stampare la testata del salto pagina.
110000950202     C*
110100950203     C                   MOVE      'R'           ROT06             1
110200950202     C*
110300950210     C* Salto pagina se richiesto o se overflow
110400950202     C*
110500950202     C     ROT(6)        IFEQ      '1'
110600950202     C     ROT(6)        OREQ      '3'
110700950210     C     *IN39         OREQ      *ON
110800950207     C*
110900950210     C     *IN85         IFEQ      *OFF
111000950210     C     *IN39         OREQ      *ON
111100950210     C                   EXSR      TESTAT
111200950202     C                   SETON                                        85
111300950207     C                   ENDIF
111400950207     C                   ENDIF
111500950207     C*
111600950207     C* Titolo se richiesto
111700950207     C*
111800950207     C     ROT(6)        IFNE      '0'
111900950208     C     ROT(6)        ANDNE     ' '
112000951025     C                   CLEAR                   L6rottura
112100951025     C                   MOVEL     TIT(6)        L6Campo
112200950202     C                   MOVEL     SAV(6)        WRKKEY           15
112300951025     C                   MOVEL     WRKKEY        L6chiave
112400950202     C*
112500950202     C*  Eseguo decodifica titoli
112600950202     C*
112700950203     C                   MOVEL     TRC(6)        WRKTRC
112800950203     C                   MOVEL     FLD(6)        WRKFLD
112900950629     C                   MOVEL     RGC(6)        WRKRGC
113000950206     C                   EXSR      DECTIT
113100951025     C                   MOVEL     WRKDES        L6decodif
113200951025      * Salvo decodifiche
113300951025     C                   MOVEL     WRKKEY        SkCod(6)
113400951025     C                   MOVEL     WRKDES        SkDescr(6)
113500960524     C                   MOVEL     L6campo       SkCampo(6)
113600950209     C*
113700950209     C* Stampo riga vuota se stampati i totali precedenti, ma non l'int
113800950209     C* estazione precedente
113900950209     C     ROT(6)        IFEQ      '2'
114000950209     C     ROT05         ANDNE     'R'
114100951025     C                   WRITE     RigaVuota
114200950209     C                   ENDIF
114300950206     C*
114400950206     C     TIT(6)        IFNE      *BLANKS
114500951025     C                   WRITE     L6rottura
114600950206     C                   ENDIF
114700950206     C                   ENDIF
114800950202     C*
114900950202     C                   ENDSR
115000950202     C********************************************************************
115100950202     C/SPACE 5
115200950202     C********************************************************************
115300950202     C* Operazioni a rottura della chiave 1
115400950202     C*
115500951025     C     Rottura7      BEGSR
115600950202     C*
115700990129     C************       CLEAR                   DS7
115800990129     C                   CLEAR                   FL7
115900950202     C*
116000950202     C* Utilizzo questo flags per sapere se ho rotto la chiave e di
116100950202     C* conseguenza non stampare la testata del salto pagina.
116200950202     C*
116300950203     C                   MOVE      'R'           ROT07             1
116400950202     C*
116500950210     C* Salto pagina se richiesto o se overflow
116600950202     C*
116700950202     C     ROT(7)        IFEQ      '1'
116800950202     C     ROT(7)        OREQ      '3'
116900950210     C     *IN39         OREQ      *ON
117000950207     C*
117100950210     C     *IN85         IFEQ      *OFF
117200950210     C     *IN39         OREQ      *ON
117300950210     C                   EXSR      TESTAT
117400950202     C                   SETON                                        85
117500950207     C                   ENDIF
117600950207     C                   ENDIF
117700950207     C*
117800950207     C* Titolo se richiesto
117900950207     C*
118000950207     C     ROT(7)        IFNE      '0'
118100950208     C     ROT(7)        ANDNE     ' '
118200951025     C                   CLEAR                   L7rottura
118300951025     C                   MOVEL     TIT(7)        L7Campo
118400950202     C                   MOVEL     SAV(7)        WRKKEY           15
118500951025     C                   MOVEL     WRKKEY        L7chiave
118600950202     C*
118700950202     C*  Eseguo decodifica titoli
118800950202     C*
118900950203     C                   MOVEL     TRC(7)        WRKTRC
119000950203     C                   MOVEL     FLD(7)        WRKFLD
119100950629     C                   MOVEL     RGC(7)        WRKRGC
119200950206     C                   EXSR      DECTIT
119300951025     C                   MOVEL     WRKDES        L7decodif
119400951025      * Salvo decodifiche
119500951025     C                   MOVEL     WRKKEY        SkCod(7)
119600951025     C                   MOVEL     WRKDES        SkDescr(7)
119700960524     C                   MOVEL     L7campo       SkCampo(7)
119800950209     C*
119900950209     C* Stampo riga vuota se stampati i totali precedenti, ma non l'int
120000950209     C* estazione precedente
120100950209     C     ROT(7)        IFEQ      '2'
120200950209     C     ROT06         ANDNE     'R'
120300951025     C                   WRITE     RigaVuota
120400950209     C                   ENDIF
120500950206     C     TIT(7)        IFNE      *BLANKS
120600951025     C                   WRITE     L7rottura
120700950206     C                   ENDIF
120800950206     C                   ENDIF
120900950202     C*
121000950202     C                   ENDSR
121100950202     C********************************************************************
121200950202     C/SPACE 5
121300950202     C********************************************************************
121400950202     C* Operazioni a rottura della chiave 8
121500950202     C*
121600951025     C     Rottura8      BEGSR
121700950202     C*
121800990129     C************       CLEAR                   DS8
121900990129     C                   CLEAR                   FL8
122000950202     C*
122100950202     C* Utilizzo questo flags per sapere se ho rotto la chiave e di
122200950202     C* conseguenza non stampare la testata del salto pagina.
122300950202     C*
122400950203     C                   MOVE      'R'           ROT08             1
122500950202     C*
122600950210     C* Salto pagina se richiesto o se overflow
122700950202     C*
122800950202     C     ROT(8)        IFEQ      '1'
122900950202     C     ROT(8)        OREQ      '3'
123000950210     C     *IN39         OREQ      *ON
123100950207     C*
123200950210     C     *IN85         IFEQ      *OFF
123300950210     C     *IN39         OREQ      *ON
123400950210     C                   EXSR      TESTAT
123500950202     C                   SETON                                        85
123600950207     C                   ENDIF
123700950207     C                   ENDIF
123800950207     C*
123900950207     C* Titolo se richiesto
124000950207     C*
124100950207     C     ROT(8)        IFNE      '0'
124200950208     C     ROT(8)        ANDNE     ' '
124300951025     C                   CLEAR                   L8rottura
124400951025     C                   MOVEL     TIT(8)        L8Campo
124500950202     C                   MOVEL     SAV(8)        WRKKEY           15
124600951025     C                   MOVEL     WRKKEY        L8chiave
124700950202     C*
124800950202     C*  Eseguo decodifica titoli
124900950202     C*
125000950203     C                   MOVEL     TRC(8)        WRKTRC
125100950203     C                   MOVEL     FLD(8)        WRKFLD
125200950629     C                   MOVEL     RGC(8)        WRKRGC
125300950206     C                   EXSR      DECTIT
125400951025     C                   MOVEL     WRKDES        L8decodif
125500951025      * Salvo decodifiche
125600951025     C                   MOVEL     WRKKEY        SkCod(8)
125700951025     C                   MOVEL     WRKDES        SkDescr(8)
125800960524     C                   MOVEL     L8campo       SkCampo(8)
125900950209     C*
126000950209     C* Stampo riga vuota se stampati i totali precedenti, ma non l'int
126100950209     C* estazione precedente
126200950209     C     ROT(8)        IFEQ      '2'
126300950209     C     ROT07         ANDNE     'R'
126400951025     C                   WRITE     RigaVuota
126500950209     C                   ENDIF
126600950206     C     TIT(8)        IFNE      *BLANKS
126700951025     C                   WRITE     L8rottura
126800950206     C                   ENDIF
126900950206     C                   ENDIF
127000950202     C*
127100950202     C                   ENDSR
127200950202     C********************************************************************
127300950202     C/SPACE 5
127400950202     C********************************************************************
127500950202     C* Operazioni a rottura della chiave 9
127600950202     C*
127700960517     C     ROTTura9      BEGSR
127800950202     C*
127900990129     C************       CLEAR                   DS9
128000990129     C                   CLEAR                   FL9
128100950202     C*
128200950202     C* Utilizzo questo flags per sapere se ho rotto la chiave e di
128300950202     C* conseguenza non stampare la testata del salto pagina.
128400950202     C*
128500950203     C                   MOVE      'R'           ROT09             1
128600950202     C*
128700950210     C* Salto pagina se richiesto o se overflow
128800950202     C*
128900950202     C     ROT(9)        IFEQ      '1'
129000950202     C     ROT(9)        OREQ      '3'
129100950210     C     *IN39         OREQ      *ON
129200950207     C*
129300950210     C     *IN85         IFEQ      *OFF
129400950210     C     *IN39         OREQ      *ON
129500950210     C                   EXSR      TESTAT
129600950202     C                   SETON                                        85
129700950207     C                   ENDIF
129800950207     C                   ENDIF
129900950207     C*
130000950207     C* Titolo se richiesto
130100950207     C*
130200950207     C     ROT(9)        IFNE      '0'
130300950208     C     ROT(9)        ANDNE     ' '
130400951025     C                   CLEAR                   L9rottura
130500951025     C                   MOVEL     TIT(9)        L9Campo
130600950202     C                   MOVEL     SAV(9)        WRKKEY           15
130700951025     C                   MOVEL     WRKKEY        L9chiave
130800950202     C*
130900950202     C*  Eseguo decodifica titoli
131000950202     C*
131100950203     C                   MOVEL     TRC(9)        WRKTRC
131200950203     C                   MOVEL     FLD(9)        WRKFLD
131300950629     C                   MOVEL     RGC(9)        WRKRGC
131400950206     C                   EXSR      DECTIT
131500951025     C                   MOVEL     WRKDES        L9decodif
131600951025      * Salvo decodifiche
131700951025     C                   MOVEL     WRKKEY        SkCod(9)
131800951025     C                   MOVEL     WRKDES        SkDescr(9)
131900960524     C                   MOVEL     L9campo       SkCampo(9)
132000950209     C*
132100950209     C* Stampo riga vuota se stampati i totali precedenti, ma non l'int
132200950209     C* estazione precedente
132300950209     C     ROT(9)        IFEQ      '2'
132400950209     C     ROT08         ANDNE     'R'
132500951025     C                   WRITE     RigaVuota
132600950209     C                   ENDIF
132700950206     C     TIT(9)        IFNE      *BLANKS
132800951025     C                   WRITE     L9rottura
132900950206     C                   ENDIF
133000950206     C                   ENDIF
133100950202     C*
133200950202     C                   ENDSR
133300950202     C/EJECT
133400950202     C********************************************************************
133500950202     C/SPACE 5
133600950202     C********************************************************************
133700950202     C* Operazioni a rottura della chiave 10
133800950202     C*
133900951025     C     Rottura10     BEGSR
134000950202     C*
134100990129     C************       CLEAR                   DS10
134200990129     C                   CLEAR                   FL0
134300950202     C*
134400950202     C* Utilizzo questo flags per sapere se ho rotto la chiave e di
134500950202     C* conseguenza non stampare la testata del salto pagina.
134600950202     C*
134700950203     C                   MOVE      'R'           ROT10             1
134800950202     C*
134900950210     C* Salto pagina se richiesto o se overflow
135000950202     C*
135100950202     C     ROT(10)       IFEQ      '1'
135200950202     C     ROT(10)       OREQ      '3'
135300950210     C     *IN39         OREQ      *ON
135400950207     C*
135500950210     C     *IN85         IFEQ      *OFF
135600950210     C     *IN39         OREQ      *ON
135700950210     C                   EXSR      TESTAT
135800950202     C                   SETON                                        85
135900950207     C                   ENDIF
136000950207     C                   ENDIF
136100950207     C*
136200950207     C* Titolo se richiesto
136300950207     C*
136400950207     C     ROT(10)       IFNE      '0'
136500950208     C     ROT(10)       ANDNE     ' '
136600951025     C                   CLEAR                   L0rottura
136700951025     C                   MOVEL     TIT(10)       L0Campo
136800950202     C                   MOVEL     SAV(10)       WRKKEY           15
136900951025     C                   MOVEL     WRKKEY        L0chiave
137000950202     C*
137100950202     C*  Eseguo decodifica titoli
137200950202     C*
137300950203     C                   MOVEL     TRC(10)       WRKTRC
137400950203     C                   MOVEL     FLD(10)       WRKFLD
137500950629     C                   MOVEL     RGC(10)       WRKRGC
137600950206     C                   EXSR      DECTIT
137700951025     C                   MOVEL     WRKDES        L0decodif
137800951025      * Salvo decodifiche
137900951025     C                   MOVEL     WRKKEY        SkCod(10)
138000951025     C                   MOVEL     WRKDES        SkDescr(10)
138100970813     C                   MOVEL     L0Campo       SkCampo(10)
138200950209     C*
138300950209     C* Stampo riga vuota se stampati i totali precedenti, ma non l'int
138400950209     C* estazione precedente
138500950209     C     ROT(10)       IFEQ      '2'
138600950209     C     ROT09         ANDNE     'R'
138700951025     C                   WRITE     RigaVuota
138800950209     C                   ENDIF
138900950206     C     TIT(10)       IFNE      *BLANKS
139000951025     C                   WRITE     L0rottura
139100950206     C                   ENDIF
139200950206     C                   ENDIF
139300950207     C*
139400950207     C* Titoli righe campi (da stampare solo se effettuato il salto
139500950209     C* pagina e se chiesto stampa dettaglio si)
139600950207     C     *IN85         IFEQ      *ON
139700950206     C                   WRITE     TEST1
139800950207     C                   ENDIF
139900950202     C*
140000950202     C                   ENDSR
140100950202     C/EJECT
140200950202     C********************************************************************
140300950202     C/SPACE 5
140400950202     C********************************************************************
140500950202     C* Totalizzazioni chiave 10
140600950202     C*
140700951025     C     Totale10      BEGSR
140800950210     C*Oveflow
140900950210     C   39              EXSR      TESTAT
141000950202     C*
141100990129     C*
141200951025     C                   MOVEL     SkCod(10)     T0Chiave
141300951025     C                   MOVEL     SkDescr(10)   T0Decodif
141400960528     C     'TOTALE'      CAT       SkCampo(10):1 T0Campo
141500950202     C*
141600990129      *    Azzero indicatori di eccedenza cifra
141700990129      *    rispetto ai campi di stampa
141800990129     C                   MOVE      *ALL'0'       IN4147            7
141900990129     C                   MOVEA     IN4147        *IN(41)
142000990129      *Per ogni tot.saldo stabilisco se:
142100990129      *il campo di stampa è sufficientemente capiente per contenere
142200990203      *il valore calcolato (se non lo è viene stampato:"###########")
142300990129     C                   DO        7             $X
142400990129     C                   If        Fl0($X)     < WPSaldoMin($X) OR
142500990129     C                             Fl0($X)     > WPSaldoMax($X)
142600990129     C                   Eval      *In(40 + $X)= *On
142700990129     C                   Eval      Fl0($X)     = *Zero
142800990129     C                   EndIf
142900990129     C                   EndDo
143000990129     C*
143100990129      *Imposto i campi di stampa
143200990129     C                   Z-ADD     FL0(1)        T1FL1
143300990129     C                   Z-ADD     FL0(2)        T1FL2
143400990129     C                   Z-ADD     FL0(3)        T1FL3
143500990129     C                   Z-ADD     FL0(4)        T1FL4
143600990129     C                   Z-ADD     FL0(5)        T1FL5
143700990129     C                   Z-ADD     FL0(6)        T1FL6
143800990129     C                   Z-ADD     FL0(7)        T1FL7
143900990129     C*
144000990129     C************       Z-ADD     FL100         T1FL1
144100990129     C************       Z-ADD     FL200         T1FL2
144200990129     C************       Z-ADD     FL300         T1FL3
144300990129     C************       Z-ADD     FL400         T1FL4
144400990129     C************       Z-ADD     FL500         T1FL5
144500990129     C************       Z-ADD     FL600         T1FL6
144600990129     C************       Z-ADD     FL700         T1FL7
144700951025     C                   WRITE     ValoriTot
144800950202     C*
144900951025     C                   WRITE     T0Totale
145000950202     C*
145100950202     C                   ENDSR
145200950202     C/EJECT
145300950202     C********************************************************************
145400950202     C/SPACE 5
145500950202     C********************************************************************
145600950202     C* Totalizzazioni chiave 9
145700950202     C*
145800951025     C     Totale9       BEGSR
145900950210     C*Oveflow
146000950210     C   39              EXSR      TESTAT
146100950202     C*
146200951025     C                   MOVEL     SkCod(9)      T9Chiave
146300951025     C                   MOVEL     SkDescr(9)    T9Decodif
146400960528     C     'TOTALE'      CAT       SkCampo(9):1  T9Campo
146500950202     C*
146600990129      *    Azzero indicatori di eccedenza cifra
146700990129      *    rispetto ai campi di stampa
146800990129     C                   MOVE      *ALL'0'       IN4147            7
146900990129     C                   MOVEA     IN4147        *IN(41)
147000990129      *Per ogni tot.saldo stabilisco se:
147100990129      *il campo di stampa è sufficientemente capiente per contenere
147200990203      *il valore calcolato (se non lo è viene stampato:"###########")
147300990129     C                   DO        7             $X
147400990129     C                   If        Fl9($X)     < WPSaldoMin($X) OR
147500990129     C                             Fl9($X)     > WPSaldoMax($X)
147600990129     C                   Eval      *In(40 + $X)= *On
147700990129     C                   Eval      Fl9($X)     = *Zero
147800990129     C                   EndIf
147900990129     C                   EndDo
148000990129     C*
148100990129      *Imposto i campi di stampa
148200990129     C                   Z-ADD     FL9(1)        T1FL1
148300990129     C                   Z-ADD     FL9(2)        T1FL2
148400990129     C                   Z-ADD     FL9(3)        T1FL3
148500990129     C                   Z-ADD     FL9(4)        T1FL4
148600990129     C                   Z-ADD     FL9(5)        T1FL5
148700990129     C                   Z-ADD     FL9(6)        T1FL6
148800990129     C                   Z-ADD     FL9(7)        T1FL7
148900990129     C*
149000990129     C************       Z-ADD     FL109         T1FL1
149100990129     C************       Z-ADD     FL209         T1FL2
149200990129     C************       Z-ADD     FL309         T1FL3
149300990129     C************       Z-ADD     FL409         T1FL4
149400990129     C************       Z-ADD     FL509         T1FL5
149500990129     C************       Z-ADD     FL609         T1FL6
149600990129     C************       Z-ADD     FL709         T1FL7
149700951025     C                   WRITE     ValoriTot
149800950202     C*
149900951025     C                   WRITE     T9Totale
150000950202     C*
150100950202     C                   ENDSR
150200950202     C/EJECT
150300950202     C********************************************************************
150400950202     C/SPACE 5
150500950202     C********************************************************************
150600950202     C* Totalizzazioni chiave 8
150700950202     C*
150800951025     C     Totale8       BEGSR
150900950210     C*Oveflow
151000950210     C   39              EXSR      TESTAT
151100951025     C*
151200951025     C                   MOVEL     SkCod(8)      T8Chiave
151300951025     C                   MOVEL     SkDescr(8)    T8Decodif
151400960528     C     'TOTALE'      CAT       SkCampo(8):1  T8Campo
151500950202     C*
151600990129      *    Azzero indicatori di eccedenza cifra
151700990129      *    rispetto ai campi di stampa
151800990129     C                   MOVE      *ALL'0'       IN4147            7
151900990129     C                   MOVEA     IN4147        *IN(41)
152000990129      *Per ogni tot.saldo stabilisco se:
152100990129      *il campo di stampa è sufficientemente capiente per contenere
152200990203      *il valore calcolato (se non lo è viene stampato:"###########")
152300990129     C                   DO        7             $X
152400990129     C                   If        Fl8($X)     < WPSaldoMin($X) OR
152500990129     C                             Fl8($X)     > WPSaldoMax($X)
152600990129     C                   Eval      *In(40 + $X)= *On
152700990129     C                   Eval      Fl8($X)     = *Zero
152800990129     C                   EndIf
152900990129     C                   EndDo
153000990129     C*
153100990129      *Imposto i campi di stampa
153200990129     C                   Z-ADD     FL8(1)        T1FL1
153300990129     C                   Z-ADD     FL8(2)        T1FL2
153400990129     C                   Z-ADD     FL8(3)        T1FL3
153500990129     C                   Z-ADD     FL8(4)        T1FL4
153600990129     C                   Z-ADD     FL8(5)        T1FL5
153700990129     C                   Z-ADD     FL8(6)        T1FL6
153800990129     C                   Z-ADD     FL8(7)        T1FL7
153900990129     C*
154000990129     C************       Z-ADD     FL108         T1FL1
154100990129     C************       Z-ADD     FL208         T1FL2
154200990129     C************       Z-ADD     FL308         T1FL3
154300990129     C************       Z-ADD     FL408         T1FL4
154400990129     C************       Z-ADD     FL508         T1FL5
154500990129     C************       Z-ADD     FL608         T1FL6
154600990129     C************       Z-ADD     FL708         T1FL7
154700951025     C                   WRITE     ValoriTot
154800950202     C*
154900951025     C                   WRITE     T8Totale
155000950202     C*
155100950202     C                   ENDSR
155200950202     C/EJECT
155300950202     C********************************************************************
155400950202     C/SPACE 5
155500950202     C********************************************************************
155600950202     C* Totalizzazioni chiave 7
155700950202     C*
155800951025     C     Totale7       BEGSR
155900950210     C*Oveflow
156000950210     C   39              EXSR      TESTAT
156100951025     C*
156200951025     C                   MOVEL     SkCod(7)      T7Chiave
156300951025     C                   MOVEL     SkDescr(7)    T7Decodif
156400960528     C     'TOTALE'      CAT       SkCampo(7):1  T7Campo
156500990129     C*
156600990129      *    Azzero indicatori di eccedenza cifra
156700990129      *    rispetto ai campi di stampa
156800990129     C                   MOVE      *ALL'0'       IN4147            7
156900990129     C                   MOVEA     IN4147        *IN(41)
157000990129      *Per ogni tot.saldo stabilisco se:
157100990129      *il campo di stampa è sufficientemente capiente per contenere
157200990203      *il valore calcolato (se non lo è viene stampato:"###########")
157300990129     C                   DO        7             $X
157400990129     C                   If        Fl7($X)     < WPSaldoMin($X) OR
157500990129     C                             Fl7($X)     > WPSaldoMax($X)
157600990129     C                   Eval      *In(40 + $X)= *On
157700990129     C                   Eval      Fl7($X)     = *Zero
157800990129     C                   EndIf
157900990129     C                   EndDo
158000990129     C*
158100990129      *Imposto i campi di stampa
158200990129     C                   Z-ADD     FL7(1)        T1FL1
158300990129     C                   Z-ADD     FL7(2)        T1FL2
158400990129     C                   Z-ADD     FL7(3)        T1FL3
158500990129     C                   Z-ADD     FL7(4)        T1FL4
158600990129     C                   Z-ADD     FL7(5)        T1FL5
158700990129     C                   Z-ADD     FL7(6)        T1FL6
158800990129     C                   Z-ADD     FL7(7)        T1FL7
158900990129     C*
159000990129     C************       Z-ADD     FL107         T1FL1
159100990129     C************       Z-ADD     FL207         T1FL2
159200990129     C************       Z-ADD     FL307         T1FL3
159300990129     C************       Z-ADD     FL407         T1FL4
159400990129     C************       Z-ADD     FL507         T1FL5
159500990129     C************       Z-ADD     FL607         T1FL6
159600990129     C************       Z-ADD     FL707         T1FL7
159700951025     C                   WRITE     ValoriTot
159800950202     C*
159900951025     C                   WRITE     T7Totale
160000950202     C*
160100950202     C                   ENDSR
160200950202     C/EJECT
160300950202     C********************************************************************
160400950202     C/SPACE 5
160500950202     C********************************************************************
160600950202     C* Totalizzazioni chiave  6
160700950202     C*
160800960517     C     Totale6       BEGSR
160900950210     C*Oveflow
161000950210     C   39              EXSR      TESTAT
161100951025     C*
161200951025     C                   MOVEL     SkCod(6)      T6Chiave
161300951025     C                   MOVEL     SkDescr(6)    T6Decodif
161400960528     C     'TOTALE'      CAT       SkCampo(6):1  T6Campo
161500950202     C*
161600990129      *    Azzero indicatori di eccedenza cifra
161700990129      *    rispetto ai campi di stampa
161800990129     C                   MOVE      *ALL'0'       IN4147            7
161900990129     C                   MOVEA     IN4147        *IN(41)
162000990129      *Per ogni tot.saldo stabilisco se:
162100990129      *il campo di stampa è sufficientemente capiente per contenere
162200990203      *il valore calcolato (se non lo è viene stampato:"###########")
162300990129     C                   DO        7             $X
162400990129     C                   If        Fl6($X)     < WPSaldoMin($X) OR
162500990129     C                             Fl6($X)     > WPSaldoMax($X)
162600990129     C                   Eval      *In(40 + $X)= *On
162700990129     C                   Eval      Fl6($X)     = *Zero
162800990129     C                   EndIf
162900990129     C                   EndDo
163000990129     C*
163100990129      *Imposto i campi di stampa
163200990129     C                   Z-ADD     FL6(1)        T1FL1
163300990129     C                   Z-ADD     FL6(2)        T1FL2
163400990129     C                   Z-ADD     FL6(3)        T1FL3
163500990129     C                   Z-ADD     FL6(4)        T1FL4
163600990129     C                   Z-ADD     FL6(5)        T1FL5
163700990129     C                   Z-ADD     FL6(6)        T1FL6
163800990129     C                   Z-ADD     FL6(7)        T1FL7
163900990129     C*
164000990129     C************       Z-ADD     FL106         T1FL1
164100990129     C************       Z-ADD     FL206         T1FL2
164200990129     C************       Z-ADD     FL306         T1FL3
164300990129     C************       Z-ADD     FL406         T1FL4
164400990129     C************       Z-ADD     FL506         T1FL5
164500990129     C************       Z-ADD     FL606         T1FL6
164600990129     C************       Z-ADD     FL706         T1FL7
164700951025     C                   WRITE     ValoriTot
164800950202     C*
164900951025     C                   WRITE     T6Totale
165000950202     C*
165100950202     C                   ENDSR
165200950202     C/EJECT
165300950202     C********************************************************************
165400950202     C/SPACE 5
165500950202     C********************************************************************
165600950202     C* Totalizzazioni chiave 5
165700950202     C*
165800951025     C     Totale5       BEGSR
165900950210     C*Oveflow
166000950210     C   39              EXSR      TESTAT
166100951025     C*
166200951025     C                   MOVEL     SkCod(5)      T5Chiave
166300951025     C                   MOVEL     SkDescr(5)    T5Decodif
166400960528     C     'TOTALE'      CAT       SkCampo(5):1  T5Campo
166500990129     C*
166600990129      *    Azzero indicatori di eccedenza cifra
166700990129      *    rispetto ai campi di stampa
166800990129     C                   MOVE      *ALL'0'       IN4147            7
166900990129     C                   MOVEA     IN4147        *IN(41)
167000990129      *Per ogni tot.saldo stabilisco se:
167100990129      *il campo di stampa è sufficientemente capiente per contenere
167200990203      *il valore calcolato (se non lo è viene stampato:"###########")
167300990129     C                   DO        7             $X
167400990129     C                   If        Fl5($X)     < WPSaldoMin($X) OR
167500990129     C                             Fl5($X)     > WPSaldoMax($X)
167600990129     C                   Eval      *In(40 + $X)= *On
167700990129     C                   Eval      Fl5($X)     = *Zero
167800990129     C                   EndIf
167900990129     C                   EndDo
168000990129     C*
168100990129      *Imposto i campi di stampa
168200990129     C                   Z-ADD     FL5(1)        T1FL1
168300990129     C                   Z-ADD     FL5(2)        T1FL2
168400990129     C                   Z-ADD     FL5(3)        T1FL3
168500990129     C                   Z-ADD     FL5(4)        T1FL4
168600990129     C                   Z-ADD     FL5(5)        T1FL5
168700990129     C                   Z-ADD     FL5(6)        T1FL6
168800990129     C                   Z-ADD     FL5(7)        T1FL7
168900990129     C*
169000990129     C************       Z-ADD     FL105         T1FL1
169100990129     C************       Z-ADD     FL205         T1FL2
169200990129     C************       Z-ADD     FL305         T1FL3
169300990129     C************       Z-ADD     FL405         T1FL4
169400990129     C************       Z-ADD     FL505         T1FL5
169500990129     C************       Z-ADD     FL605         T1FL6
169600990129     C************       Z-ADD     FL705         T1FL7
169700951025     C                   WRITE     ValoriTot
169800950202     C*
169900951025     C                   WRITE     T5Totale
170000950202     C*
170100950202     C                   ENDSR
170200950202     C/EJECT
170300950202     C********************************************************************
170400950202     C/SPACE 5
170500950202     C********************************************************************
170600950202     C* Totalizzazioni chiave 4
170700950202     C*
170800951025     C     Totale4       BEGSR
170900950210     C*Oveflow
171000950210     C   39              EXSR      TESTAT
171100951025     C*
171200951025     C                   MOVEL     SkCod(4)      T4Chiave
171300951025     C                   MOVEL     SkDescr(4)    T4Decodif
171400960528     C     'TOTALE'      CAT       SkCampo(4):1  T4Campo
171500950202     C*
171600990129      *    Azzero indicatori di eccedenza cifra
171700990129      *    rispetto ai campi di stampa
171800990129     C                   MOVE      *ALL'0'       IN4147            7
171900990129     C                   MOVEA     IN4147        *IN(41)
172000990129      *Per ogni tot.saldo stabilisco se:
172100990129      *il campo di stampa è sufficientemente capiente per contenere
172200990203      *il valore calcolato (se non lo è viene stampato:"###########")
172300990129     C                   DO        7             $X
172400990129     C                   If        Fl4($X)     < WPSaldoMin($X) OR
172500990129     C                             Fl4($X)     > WPSaldoMax($X)
172600990129     C                   Eval      *In(40 + $X)= *On
172700990129     C                   Eval      Fl4($X)     = *Zero
172800990129     C                   EndIf
172900990129     C                   EndDo
173000990129     C*
173100990129      *Imposto i campi di stampa
173200990129     C                   Z-ADD     FL4(1)        T1FL1
173300990129     C                   Z-ADD     FL4(2)        T1FL2
173400990129     C                   Z-ADD     FL4(3)        T1FL3
173500990129     C                   Z-ADD     FL4(4)        T1FL4
173600990129     C                   Z-ADD     FL4(5)        T1FL5
173700990129     C                   Z-ADD     FL4(6)        T1FL6
173800990129     C                   Z-ADD     FL4(7)        T1FL7
173900990129     C*
174000990129     C************       Z-ADD     FL104         T1FL1
174100990129     C************       Z-ADD     FL204         T1FL2
174200990129     C************       Z-ADD     FL304         T1FL3
174300990129     C************       Z-ADD     FL404         T1FL4
174400990129     C************       Z-ADD     FL504         T1FL5
174500990129     C************       Z-ADD     FL604         T1FL6
174600990129     C************       Z-ADD     FL704         T1FL7
174700951025     C                   WRITE     ValoriTot
174800950202     C*
174900951025     C                   WRITE     T4Totale
175000950202     C*
175100950202     C                   ENDSR
175200950202     C/EJECT
175300950202     C********************************************************************
175400950202     C/SPACE 5
175500950202     C********************************************************************
175600950202     C* Totalizzazioni chiave 3
175700950202     C*
175800951025     C     Totale3       BEGSR
175900950210     C*Oveflow
176000950210     C   39              EXSR      TESTAT
176100951025     C*
176200951025     C                   MOVEL     SkCod(3)      T3Chiave
176300951025     C                   MOVEL     SkDescr(3)    T3Decodif
176400960528     C     'TOTALE'      CAT       SkCampo(3):1  T3Campo
176500990129     C*
176600990129      *    Azzero indicatori di eccedenza cifra
176700990129      *    rispetto ai campi di stampa
176800990129     C                   MOVE      *ALL'0'       IN4147            7
176900990129     C                   MOVEA     IN4147        *IN(41)
177000990129      *Per ogni tot.saldo stabilisco se:
177100990129      *il campo di stampa è sufficientemente capiente per contenere
177200990203      *il valore calcolato (se non lo è viene stampato:"###########")
177300990129     C                   DO        7             $X
177400990129     C                   If        Fl3($X)     < WPSaldoMin($X) OR
177500990129     C                             Fl3($X)     > WPSaldoMax($X)
177600990129     C                   Eval      *In(40 + $X)= *On
177700990129     C                   Eval      Fl3($X)     = *Zero
177800990129     C                   EndIf
177900990129     C                   EndDo
178000990129     C*
178100990129      *Imposto i campi di stampa
178200990129     C                   Z-ADD     FL3(1)        T1FL1
178300990129     C                   Z-ADD     FL3(2)        T1FL2
178400990129     C                   Z-ADD     FL3(3)        T1FL3
178500990129     C                   Z-ADD     FL3(4)        T1FL4
178600990129     C                   Z-ADD     FL3(5)        T1FL5
178700990129     C                   Z-ADD     FL3(6)        T1FL6
178800990129     C                   Z-ADD     FL3(7)        T1FL7
178900990129     C*
179000990129     C************       Z-ADD     FL103         T1FL1
179100990129     C************       Z-ADD     FL203         T1FL2
179200990129     C************       Z-ADD     FL303         T1FL3
179300990129     C************       Z-ADD     FL403         T1FL4
179400990129     C************       Z-ADD     FL503         T1FL5
179500990129     C************       Z-ADD     FL603         T1FL6
179600990129     C************       Z-ADD     FL703         T1FL7
179700951025     C                   WRITE     ValoriTot
179800950202     C*
179900951025     C                   WRITE     T3Totale
180000950202     C*
180100950202     C                   ENDSR
180200950202     C/EJECT
180300950202     C********************************************************************
180400950202     C/SPACE 5
180500950202     C********************************************************************
180600950202     C* Totalizzazioni chiave 02
180700950202     C*
180800951025     C     Totale2       BEGSR
180900950210     C*Oveflow
181000950210     C   39              EXSR      TESTAT
181100951025     C*
181200951025     C                   MOVEL     SkCod(2)      T2Chiave
181300951025     C                   MOVEL     SkDescr(2)    T2Decodif
181400960528     C     'TOTALE'      CAT       SkCampo(2):1  T2Campo
181500950202     C*
181600990129      *    Azzero indicatori di eccedenza cifra
181700990129      *    rispetto ai campi di stampa
181800990129     C                   MOVE      *ALL'0'       IN4147            7
181900990129     C                   MOVEA     IN4147        *IN(41)
182000990129      *Per ogni tot.saldo stabilisco se:
182100990129      *il campo di stampa è sufficientemente capiente per contenere
182200990203      *il valore calcolato (se non lo è viene stampato:"###########")
182300990129     C                   DO        7             $X
182400990129     C                   If        Fl2($X)     < WPSaldoMin($X) OR
182500990129     C                             Fl2($X)     > WPSaldoMax($X)
182600990129     C                   Eval      *In(40 + $X)= *On
182700990129     C                   Eval      Fl2($X)     = *Zero
182800990129     C                   EndIf
182900990129     C                   EndDo
183000990129     C*
183100990129      *Imposto i campi di stampa
183200990129     C                   Z-ADD     FL2(1)        T1FL1
183300990129     C                   Z-ADD     FL2(2)        T1FL2
183400990129     C                   Z-ADD     FL2(3)        T1FL3
183500990129     C                   Z-ADD     FL2(4)        T1FL4
183600990129     C                   Z-ADD     FL2(5)        T1FL5
183700990129     C                   Z-ADD     FL2(6)        T1FL6
183800990129     C                   Z-ADD     FL2(7)        T1FL7
183900990129     C*
184000990129     C************       Z-ADD     FL102         T1FL1
184100990129     C************       Z-ADD     FL202         T1FL2
184200990129     C************       Z-ADD     FL302         T1FL3
184300990129     C************       Z-ADD     FL402         T1FL4
184400990129     C************       Z-ADD     FL502         T1FL5
184500990129     C************       Z-ADD     FL602         T1FL6
184600990129     C************       Z-ADD     FL702         T1FL7
184700951025     C                   WRITE     ValoriTot
184800950202     C*
184900951025     C                   WRITE     T2Totale
185000950202     C*
185100950202     C                   ENDSR
185200950202     C/EJECT
185300950202     C********************************************************************
185400950202     C/SPACE 5
185500950202     C********************************************************************
185600950202     C* Totalizzazioni chiave 1
185700950202     C*
185800951025     C     Totale1       BEGSR
185900950210     C*Oveflow
186000950210     C   39              EXSR      TESTAT
186100951025     C*
186200951025     C                   MOVEL     SkCod(1)      T1Chiave
186300951025     C                   MOVEL     SkDescr(1)    T1Decodif
186400960528     C     'TOTALE'      CAT       SkCampo(1):1  T1Campo
186500950202     C*
186600990129      *    Azzero indicatori di eccedenza cifra
186700990129      *    rispetto ai campi di stampa
186800990129     C                   MOVE      *ALL'0'       IN4147            7
186900990129     C                   MOVEA     IN4147        *IN(41)
187000990129      *Per ogni tot.saldo stabilisco se:
187100990129      *il campo di stampa è sufficientemente capiente per contenere
187200990203      *il valore calcolato (se non lo è viene stampato:"###########")
187300990129     C                   DO        7             $X
187400990129     C                   If        Fl1($X)     < WPSaldoMin($X) OR
187500990129     C                             Fl1($X)     > WPSaldoMax($X)
187600990129     C                   Eval      *In(40 + $X)= *On
187700990129     C                   Eval      Fl1($X)     = *Zero
187800990129     C                   EndIf
187900990129     C                   EndDo
188000990129     C*
188100990129      *Imposto i campi di stampa
188200990129     C                   Z-ADD     FL1(1)        T1FL1
188300990129     C                   Z-ADD     FL1(2)        T1FL2
188400990129     C                   Z-ADD     FL1(3)        T1FL3
188500990129     C                   Z-ADD     FL1(4)        T1FL4
188600990129     C                   Z-ADD     FL1(5)        T1FL5
188700990129     C                   Z-ADD     FL1(6)        T1FL6
188800990129     C                   Z-ADD     FL1(7)        T1FL7
188900990129     C*
189000990129     C************       Z-ADD     FL101         T1FL1
189100990129     C************       Z-ADD     FL201         T1FL2
189200990129     C************       Z-ADD     FL301         T1FL3
189300990129     C************       Z-ADD     FL401         T1FL4
189400990129     C************       Z-ADD     FL501         T1FL5
189500990129     C************       Z-ADD     FL601         T1FL6
189600990129     C************       Z-ADD     FL701         T1FL7
189700951025     C                   WRITE     ValoriTot
189800950202     C*
189900951025     C                   WRITE     T1Totale
190000950202     C*
190100950202     C                   ENDSR
190200950202     C/EJECT
190300950202     C********************************************************************
190400950202     C/SPACE 5
190500950202     C********************************************************************
190600950202     C* Totalizzazioni generali
190700950202     C*
190800950202     C     TOTGEN        BEGSR
190900950210     C*Oveflow
191000950210     C   39              EXSR      TESTAT
191100990129     C*
191200990129      *    Azzero indicatori di eccedenza cifra
191300990129      *    rispetto ai campi di stampa
191400990129     C                   MOVE      *ALL'0'       IN4147            7
191500990129     C                   MOVEA     IN4147        *IN(41)
191600990129      *Per ogni tot.saldo stabilisco se:
191700990129      *il campo di stampa è sufficientemente capiente per contenere
191800990203      *il valore calcolato (se non lo è viene stampato:"###########")
191900990129     C                   DO        7             $X
192000990129     C                   If        FlG($X)     < WPSaldoMin($X) OR
192100990129     C                             FlG($X)     > WPSaldoMax($X)
192200990129     C                   Eval      *In(40 + $X)= *On
192300990129     C                   Eval      FlG($X)     = *Zero
192400990129     C                   EndIf
192500990129     C                   EndDo
192600990129     C*
192700990129      *Imposto i campi di stampa
192800990129     C                   Z-ADD     FLG(1)        T1FL1
192900990129     C                   Z-ADD     FLG(2)        T1FL2
193000990129     C                   Z-ADD     FLG(3)        T1FL3
193100990129     C                   Z-ADD     FLG(4)        T1FL4
193200990129     C                   Z-ADD     FLG(5)        T1FL5
193300990129     C                   Z-ADD     FLG(6)        T1FL6
193400990129     C                   Z-ADD     FLG(7)        T1FL7
193500950316     C*
193600990129     C************       Z-ADD     FL10G         T1FL1
193700990129     C************       Z-ADD     FL20G         T1FL2
193800990129     C************       Z-ADD     FL30G         T1FL3
193900990129     C************       Z-ADD     FL40G         T1FL4
194000990129     C************       Z-ADD     FL50G         T1FL5
194100990129     C************       Z-ADD     FL60G         T1FL6
194200990129     C************       Z-ADD     FL70G         T1FL7
194300951025     C                   WRITE     ValoriTot
194400951025     C*
194500951025     C                   WRITE     TOTGeneral
194600950202     C*
194700950202     C                   ENDSR
194800950202     C/EJECT
194900950131     C******************************************************
195000950202     C* STAMPA DETTAGLIO
195100950131     C******************************************************
195200960517     C     STAmpadet     BEGSR
195300950315     c*
195400950203     C                   MOVE      *BLANKS       ROT01
195500950203     C                   MOVE      *BLANKS       ROT02
195600950203     C                   MOVE      *BLANKS       ROT03
195700950203     C                   MOVE      *BLANKS       ROT04
195800950203     C                   MOVE      *BLANKS       ROT05
195900950203     C                   MOVE      *BLANKS       ROT06
196000950203     C                   MOVE      *BLANKS       ROT07
196100950203     C                   MOVE      *BLANKS       ROT08
196200950203     C                   MOVE      *BLANKS       ROT09
196300950203     C                   MOVE      *BLANKS       ROT10
196400950316     c*
196500970813     C* Impostare i campi di stampa da campi del file
196600950131     C*
196700970808     C                   MOVEL     BRASocieta    P1Societa
196800970808     C                   MOVEL     BRABanca      P1Banca
196900970808     C                   MOVEL     BRALinea      P1Linea
197000950131     C*
197100950316     C* Calcoli stampa (se ce ne sono)
197200970813     C                   EXSR      CalcoliStp
197300950922     C*
197400950131     C     *IN39         IFEQ      *ON
197500951025     C                   WRITE     TESTATA
197600950203     C                   WRITE     TEST1
197700950131     C                   SETOFF                                       39
197800950131     C                   ENDIF
197900950203     C                   WRITE     RIGP1
198000090504>>>>>C*
198100090504>>>>>C* scrive 1 rk sul file YNDCT52F come la riga di dettaglio
198200090504>>>>>C*
198300090504>>>>>c                   z-add     p1saldo1      f1saldo1
198400090504>>>>>c                   z-add     p1saldo2      f1saldo2
198500090504>>>>>c                   z-add     p1saldo3      f1saldo3
198600090504>>>>>c                   z-add     p1saldo4      f1saldo4
198700090504>>>>>c                   z-add     p1saldo5      f1saldo5
198800090504>>>>>c                   z-add     p1saldo6      f1saldo6
198900090504>>>>>c                   z-add     p1saldo7      f1saldo7
199000090504>>>>>c*
199100090504>>>>>C                   WRITE     Yndct52r00
199200950315     C*
199300950316     C* Calcoli per impostare il record dei totali
199400960603     C                   EXSR      CalcoliTot
199500950131     C*
199600950131     C                   ENDSR
199700950131     C/EJECT
199800950131     C************************************************************
199900950315     C* CALCOLI
200000950131     C************************************************************
200100960603     C     CalcoliSTP    BEGSR
200200950315     C*
200300970808     C* Reperisco il fido bancario per il rapporto
200400970813     C*
200500970814     C                   Z-ADD     0             P1ImFido1
200600970813     C                   MOVE      BRASocieta    BFISocieta
200700970813     C                   MOVE      BRABanca      BFIBanca
200800970813     C                   MOVE      BRACdFido     BFICdFido
200900040107c1713x***  K03BFI01      CHAIN     ANBFI000                           21
201000040107  "  c     K03BFI01      Setll     ANBFI000
201100040107  "  c                   do        *hival
201200040107  "  c     K03BFI01      reade     ANBFI000                               21
201300040107  "  C                   IF        *in21 = *on
201400040107  "  c                   leave
201500040107c1713c                   endif
201600040107     c*
201700970813     C                   IF        *in21 = *off         and
201800970813     C* solo se l'affidamento è singolo
201900970813     C                             BFITpAffid = '1'     and
202000970813     C* ed è iniziato
202100970814     C                             BFIDtFido <= DtRif(1)  and
202200970813     C* e non è scaduto
202300970814     C                             ( BFIDtSca>*loval and BFIDtSca>DtRif(1)
202400970814     C                               or BFIDtSca = *loval )
202500970910     C* ne mostro l'importo (in rapporto al divisore)
202600970910     C     BFIImFido1    DIV       DivisorT51    P1ImFido1
202700040107c1713c                   leave
202800970813     C                   ENDIF
202900040107c1713c                   enddo
203000981126     c*
203100981126     c* controllo se il rapporto è collegato o meno
203200981126     c* se il rapporto è collegato, utilizzo i tassi del rapporto a cui
203300981126     c* è collegato
203400981126     C                   MOVE      BRASocieta    WSocieta
203500981126     C                   MOVE      BRABanca      WBanca
203600981126     C                   MOVE      BRALinea      WLinea
203700981126     C                   IF        BRASocCt <> *blanks
203800981126     C                   MOVE      BRASocCt      WRASocieta
203900981126     C                   MOVE      BRABanCt      WRABanca
204000981126     C                   MOVE      BRALinCt      WRALinea
204100981126     C     K03BRA01      CHAIN     ANBRA001                           21
204200981126     C                   If        *in21         = *off
204300981126     C                   MOVE      WRASocieta    WSocieta
204400981126     C                   MOVE      WRABanca      WBanca
204500981126     C                   MOVE      WRALinea      WLinea
204600981126     C                   EndIf
204700981126     C                   EndIf
204800970808     C*
204900970808     C* Reperisco il tasso passivo per il rapporto
205000970813     C*
205100970814     C                   Z-ADD     0             P1TassoP
205200981126     C                   MOVE      WSocieta      BCTSocieta
205300981126     C                   MOVE      WBanca        BCTBanca
205400981126     C                   MOVE      WLinea        BCTLinea
205500970813     C                   MOVE      '2'           BCTTpTasso
205600970814     C                   MOVE      DtRif(1)      BCTDtIniz
205700970813     C     K05BCT01      SETLL     ANBCT000
205800970813     C     K04BCT01      READE     ANBCT000                               21
205900970813     C                   IF        *in21 = *off
206000970814     C                   Z-ADD     BCTTasso1     P1TassoP
206100970813     C                   ENDIF
206200970814     C*
206300970814     C* Reperisco il tasso attivo per il rapporto
206400970814     C*
206500970814     C                   Z-ADD     0             P1TassoA
206600981126     C                   MOVE      WSocieta      BCTSocieta
206700981126     C                   MOVE      WBanca        BCTBanca
206800981126     C                   MOVE      WLinea        BCTLinea
206900970814     C                   MOVE      '1'           BCTTpTasso
207000970814     C                   MOVE      DtRif(1)      BCTDtIniz
207100970814     C     K05BCT01      SETLL     ANBCT000
207200970814     C     K04BCT01      READE     ANBCT000                               21
207300970814     C                   IF        *in21 = *off
207400970814     C                   Z-ADD     BCTTasso1     P1TassoA
207500970814     C                   ENDIF
207600970813     C*
207700970813     C* Reperisco il saldo alla data di riferimento e per il passo giorni
207800970813     C*
207900970813     C* imposto la chiamata al reperimento saldi
208000970813     C                   RESET                   NDCT21DS
208100970813     C                   EVAL      SocietaT21 = BRASocieta
208200970813     C                   EVAL      CtbT21 = CtbT51
208300970813     C                   EVAL      BancaT21 = BRABanca
208400970813     C                   EVAL      LineaT21 = BRALinea
208500970910     C                   EVAL      DivisaT21 = DivisaT51
208600970813     C                   IF        DefinT51 = XscOn
208700970813     C                   EVAL      DefinT21 = *on
208800970813     C                   ELSE
208900970813     C                   EVAL      DefinT21 = *off
209000970813     C                   ENDIF
209100970813     C                   IF        ProvvT51 = XscOn
209200970813     C                   EVAL      ProvvT21 = *on
209300970813     C                   ELSE
209400970813     C                   EVAL      ProvvT21 = *off
209500970813     C                   ENDIF
209600970813     C                   IF        GestiT51 = XscOn
209700970813     C                   EVAL      GestiT21 = *on
209800970813     C                   ELSE
209900970813     C                   EVAL      GestiT21 = *off
210000970813     C                   ENDIF
210100970813     C* con ripresa saldo
210200970813     C                   EVAL      RipresaT21 = *on
210300970813     C                   EVAL      SoloChiT21 = *off
210400970813     C*
210500970813     C                   EVAL      FaiLaCall = *on
210600990129     C*
210700990129      *Azzero indicatori di eccedenza cifra rispetto ai campi di stampa
210800990128     C                   MOVE      *ALL'0'       IN4147            7
210900990128     C                   MOVEA     IN4147        *IN(41)
211000990129      *Azzero la schiera che memorizza i 7 saldi calcolati
211100990129     C                   CLEAR                   WSaldoCalc
211200970813     C*
211300970813     C* ciclo per i 7 saldi incrementando la data di riferimento del
211400970813     C* passo giorni
211500970813     C                   DO        7             x
211600970814     C                   MOVE      DtRif(x)      DataDaT21
211700970814     C                   MOVE      DtRif(x)      DataAT21
211800970813     C*
211900970813     C* necessito di chiamare il pgm dei saldi
212000970813     C                   IF        FaiLaCall = *on
212100970813     C                   CALL      'NDCT21R'
212200970813     C                   PARM                    NDCT21DS
212300990128      *
212400990128      *ASTERISCO per riprenderle nella routine RIESALDO
212500970813     C* modalità data operazione
212600990128     C******             IF        ModoT51 = '1'
212700990128     C******             EVAL      P1Saldo(x) = (SlFiDOpT21 - SlFiAOpT21)/
212800990128     C******                                    DivisorT51
212900990128     C******             ENDIF
213000990128     C**modalità data valuta
213100990128     C******             IF        ModoT51 = '0'
213200990128     C******             EVAL      P1Saldo(x) = (SlFiDVaT21 - SlFiAVaT21)/
213300990128     C******                                    DivisorT51
213400990128     C******             ENDIF
213500990128      *
213600990129      * Riempio riga con i saldi saldo  oppure
213700990203      * con '#######', se la cifra eccede il campo numerico di stampa
213800990129     C                   EXSR      RIERiga
213900970813     C*
214000970813     C* se NON necessito di chiamare il pgm dei saldi
214100970813     C                   ELSE
214200970813     C* significa che il saldo a questo passo giorni è uguale al precedente
214300970813     C                   EVAL      P1Saldo(x) = P1Saldo(x-1)
214400990129      * e anche l'importo realmente calcolato
214500990129     C                   EVAL      WSaldoCalc(x) = WSaldoCalc(x-1)
214600990128      * e anche l'eventuale eccedenza della cifra rispetto al fld di stampa
214700990129     C                   EVAL      *In(40 + x)   = *In(40 + x - 1)
214800970813     C                   ENDIF
214900970813     C*
215000971112     C* se la data prossimo saldo è > del successivo passo giorni
215100970814     C                   IF        DtDopoT21 <> *loval    and
215200970814     C                             x < 7                  and
215300971112     C                             DtDopoT21 > DtRif(x+1)
215400970813     C* non c'è bisogno di richiamare il pgm dei saldi perchè il nuovo
215500970813     C* saldo sarà uguale al precedente
215600970813     C                   MOVE      *off          FaiLaCall
215700970814     C                   ELSE
215800970814     C                   MOVE      *on           FaiLaCall
215900970814     C                   ENDIF
216000970813     C*
216100970813     C* se c'è un errore nel reperimento emetto lo specifico messaggio
216200970813     C*                  IF        ErroreT21 = '1'
216300970813     C*  ??????????????  SETON                                        5099
216400970813     C*                  ENDIF
216500970813     C*
216600970813     C                   ENDDO
216700970813     C* imposto i valori calcolati sui campi di stampa
216800970813     C                   EVAL      P1Saldo1 = P1Saldo(1)
216900970813     C                   EVAL      P1Saldo2 = P1Saldo(2)
217000970813     C                   EVAL      P1Saldo3 = P1Saldo(3)
217100970813     C                   EVAL      P1Saldo4 = P1Saldo(4)
217200970813     C                   EVAL      P1Saldo5 = P1Saldo(5)
217300970813     C                   EVAL      P1Saldo6 = P1Saldo(6)
217400970813     C                   EVAL      P1Saldo7 = P1Saldo(7)
217500020125     C*
217600020125c1540C* scriver file per PC
217700020125  "  C                   IF        NomfilT51  <> *blanks
217800020125  "  C                   EXSR      ScriviSIB
217900020125c1540c                   endif
218000020125     C*
218100020125     C                   ENDSR
218200020125c1540C************************************************************
218300020125  "  C* Scrivi NDSIB
218400020125  "  C************************************************************
218500020125  "  C     ScriviSIB     BEGSR
218600020125  "  C*
218700020125  "  C                   Move      P1Societa     SibSocieta
218800020125  "  C                   Move      P1Banca       SibBanca
218900020125  "  C                   Move      P1Linea       SibLinea
219000020125  "  C                   Z-add     P1ImFido1     SibFido
219100020128  "  C                   Z-add     P1TassoA      SibTassoA
219200020128  "  C                   Z-add     P1TassoP      SibTassoP
219300020128  "  C                   eval      SibData1 = DtRif(1)
219400020125  "  C                   Z-add     P1Saldo1      Sibsaldo1
219500020128  "  C                   eval      SibData2 = DtRif(2)
219600020125  "  C                   Z-add     P1Saldo2      Sibsaldo2
219700020128  "  C                   eval      SibData3 = DtRif(3)
219800020125  "  C                   Z-add     P1Saldo3      Sibsaldo3
219900020128  "  C                   eval      SibData4 = DtRif(4)
220000020125  "  C                   Z-add     P1Saldo4      Sibsaldo4
220100020128  "  C                   eval      SibData5 = DtRif(5)
220200020125  "  C                   Z-add     P1Saldo5      Sibsaldo5
220300020128  "  C                   eval      SibData6 = DtRif(6)
220400020125  "  C                   Z-add     P1Saldo6      Sibsaldo6
220500020128  "  C                   eval      SibData7 = DtRif(7)
220600020125  "  C                   Z-add     P1Saldo7      Sibsaldo7
220700020125  "  C                   Write     NDSIB000
220800020125  "  C*
220900020125  "  C                   ENDSR
221000020125c1540C/EJECT
221100990128      ************************************************************
221200990203      * Riempio riga saldi  oppure  stampo '#######'
221300990129      * se le cifre eccedono   il campo numerico di stampa                 m
221400990128      ************************************************************
221500990129     C     RieRiga       BEGSR
221600990128      *                                                          *
221700990128      * modalità data operazione
221800990128     C                   Select
221900990128     C     ModoT51       WhenEq    '1'
222000990128     C                   EVAL      WSaldoCalc(x) =
222100990128     C                             (SlFiDOpT21 - SlFiAOpT21)/DivisorT51
222200990128      *
222300990128      * modalità data valuta
222400990128     C     ModoT51       WhenEq    '0'
222500990128     C                   EVAL      WSaldoCalc(x) =
222600990128     C                             (SlFiDVaT21 - SlFiAVaT21)/DivisorT51
222700990128     C                   EndSl
222800990128      *
222900990128     C                   If        WSaldoCalc(x) < WPSaldoMin(x)  OR
223000990128     C                             WSaldoCalc(x) > WPSaldoMax(x)
223100990128     C                   Eval      *In(40 + x) = *On
223200990128     C                   Eval      P1Saldo(x)  = *Zero
223300990128     C                   Else
223400990128     C                   Eval      P1Saldo(x)  = WSaldoCalc(x)
223500990128     C                   EndIf
223600990128      *                                                          *
223700990129     C     XRieRiga      ENDSR
223800990128     C/EJECT
223900950202     C************************************************************
224000950202     C* CALCOLO TOTALI
224100950202     C************************************************************
224200960603     C     CalcoliTOT    BEGSR
224300950316     C*                                                          *
224400950316     C* Incrementare i campi dei totali di ogni rottura          *
224500950316     C* con i campi del dettaglio                                *
224600950316     C*
224700990129   B1C                   DO        7             $X                3 0
224800990129     C* Rottura 1
224900990129     C                   ADD       WSaldoCalc($X)FL1($X)
225000990129     C* Rottura 2
225100990129     C                   ADD       WSaldoCalc($X)FL2($X)
225200990129     C* Rottura 3
225300990129     C                   ADD       WSaldoCalc($X)FL3($X)
225400990129     C* Rottura 4
225500990129     C                   ADD       WSaldoCalc($X)FL4($X)
225600990129     C* Rottura 5
225700990129     C                   ADD       WSaldoCalc($X)FL5($X)
225800990129     C* Rottura 6
225900990129     C                   ADD       WSaldoCalc($X)FL6($X)
226000990129     C* Rottura 7
226100990129     C                   ADD       WSaldoCalc($X)FL7($X)
226200990129     C* Rottura 8
226300990129     C                   ADD       WSaldoCalc($X)FL8($X)
226400990129     C* Rottura 9
226500990129     C                   ADD       WSaldoCalc($X)FL9($X)
226600990129     C* Rottura 10
226700990129     C                   ADD       WSaldoCalc($X)FL0($X)
226800990129     C* Totale generale
226900990129     C                   ADD       WSaldoCalc($X)FLG($X)
227000990129   B1C                   EndDo
227100990129      *
227200990129     C************       ADD       P1Saldo1      FL101
227300990129     C************       ADD       P1Saldo2      FL201
227400990129     C************       ADD       P1Saldo3      FL301
227500990129     C************       ADD       P1Saldo4      FL401
227600990129     C************       ADD       P1Saldo5      FL501
227700990129     C************       ADD       P1Saldo6      FL601
227800990129     C************       ADD       P1Saldo7      FL701
227900970813     C* Rottura 2
228000990129     C************       ADD       P1Saldo1      FL102
228100990129     C************       ADD       P1Saldo2      FL202
228200990129     C************       ADD       P1Saldo3      FL302
228300990129     C************       ADD       P1Saldo4      FL402
228400990129     C************       ADD       P1Saldo5      FL502
228500990129     C************       ADD       P1Saldo6      FL602
228600990129     C************       ADD       P1Saldo7      FL702
228700970813     C* Rottura 3
228800990129     C************       ADD       P1Saldo1      FL103
228900990129     C************       ADD       P1Saldo2      FL203
229000990129     C************       ADD       P1Saldo3      FL303
229100990129     C************       ADD       P1Saldo4      FL403
229200990129     C************       ADD       P1Saldo5      FL503
229300990129     C************       ADD       P1Saldo6      FL603
229400990129     C************       ADD       P1Saldo7      FL703
229500970813     C* Rottura 4
229600990129     C************       ADD       P1Saldo1      FL104
229700990129     C************       ADD       P1Saldo2      FL204
229800990129     C************       ADD       P1Saldo3      FL304
229900990129     C************       ADD       P1Saldo4      FL404
230000990129     C************       ADD       P1Saldo5      FL504
230100990129     C************       ADD       P1Saldo6      FL604
230200990129     C************       ADD       P1Saldo7      FL704
230300970813     C* Rottura 5
230400990129     C************       ADD       P1Saldo1      FL105
230500990129     C************       ADD       P1Saldo2      FL205
230600990129     C************       ADD       P1Saldo3      FL305
230700990129     C************       ADD       P1Saldo4      FL405
230800990129     C************       ADD       P1Saldo5      FL505
230900990129     C************       ADD       P1Saldo6      FL605
231000990129     C************       ADD       P1Saldo7      FL705
231100970813     C* Rottura 6
231200990129     C************       ADD       P1Saldo1      FL106
231300990129     C************       ADD       P1Saldo2      FL206
231400990129     C************       ADD       P1Saldo3      FL306
231500990129     C************       ADD       P1Saldo4      FL406
231600990129     C************       ADD       P1Saldo5      FL506
231700990129     C************       ADD       P1Saldo6      FL606
231800990129     C************       ADD       P1Saldo7      FL706
231900970813     C* Rottura 7
232000990129     C************       ADD       P1Saldo1      FL107
232100990129     C************       ADD       P1Saldo2      FL207
232200990129     C************       ADD       P1Saldo3      FL307
232300990129     C************       ADD       P1Saldo4      FL407
232400990129     C************       ADD       P1Saldo5      FL507
232500990129     C************       ADD       P1Saldo6      FL607
232600990129     C************       ADD       P1Saldo7      FL707
232700970813     C* Rottura 8
232800990129     C************       ADD       P1Saldo1      FL108
232900990129     C************       ADD       P1Saldo2      FL208
233000990129     C************       ADD       P1Saldo3      FL308
233100990129     C************       ADD       P1Saldo4      FL408
233200990129     C************       ADD       P1Saldo5      FL508
233300990129     C************       ADD       P1Saldo6      FL608
233400990129     C************       ADD       P1Saldo7      FL708
233500970813     C* Rottura 9
233600990129     C************       ADD       P1Saldo1      FL109
233700990129     C************       ADD       P1Saldo2      FL209
233800990129     C************       ADD       P1Saldo3      FL309
233900990129     C************       ADD       P1Saldo4      FL409
234000990129     C************       ADD       P1Saldo5      FL509
234100990129     C************       ADD       P1Saldo6      FL609
234200990129     C************       ADD       P1Saldo7      FL709
234300970813     C* Rottura 10
234400990129     C************       ADD       P1Saldo1      FL100
234500990129     C************       ADD       P1Saldo2      FL200
234600990129     C************       ADD       P1Saldo3      FL300
234700990129     C************       ADD       P1Saldo4      FL400
234800990129     C************       ADD       P1Saldo5      FL500
234900990129     C************       ADD       P1Saldo6      FL600
235000990129     C************       ADD       P1Saldo7      FL700
235100970813     C* Totale generale
235200990129     C************       ADD       P1Saldo1      FL10G
235300990129     C************       ADD       P1Saldo2      FL20G
235400990129     C************       ADD       P1Saldo3      FL30G
235500990129     C************       ADD       P1Saldo4      FL40G
235600990129     C************       ADD       P1Saldo5      FL50G
235700990129     C************       ADD       P1Saldo6      FL60G
235800990129     C************       ADD       P1Saldo7      FL70G
235900990129     C*
236000950202     C                   ENDSR
236100940927     C************************************************************
236200940927     C* OPERAZIONI INIZIALI
236300940927     C************************************************************
236400940927     C     *INZSR        BEGSR
236500000117C1309C     *DMY          Move      Udate         ZDateISO
236600000117C1309C     *JobRun       Move      ZDateISO      ZDate             6 0
236700940927     C*
236800940830     C* Reperimento parametri
236900940830     C*
237000940830     C     *ENTRY        PLIST
237100940830     C                   PARM                    KPJBA
237200950208     C                   PARM                    TITOLO           50
237300950208     C*
237400950208     C                   MOVEL     TITOLO        T1TIT
237500990128     C*
237600990129     C                   Eval      WPSaldoMin  = *Hival
237700990129     C                   Eval      WPSaldoMin  = WPSaldoMin * -1
237800940830     C*
237900940830     C                   ENDSR
238000941129     C/EJECT
238100941129     C************************************************************
238200941129     C* Reperimento dati società
238300941129     C************************************************************
238400941129     C     REPSOC        BEGSR
238500941129     C*
238600950629     C                   CALLb     'XSOC'
238700941129     C                   PARM                    TIPXSC            6
238800941129     C                   PARM                    SOCXSC            3
238900941129     C                   PARM                    CDSXSC            9 0
239000941129     C                   PARM                    MODXSC            3
239100941129     C                   PARM      *BLANKS       RTNXSC            1
239200941129     C                   PARM                    XSOCDS
239300941129     C                   PARM                    KPJBA
239400941129      *
239500941129     C                   ENDSR
239600940830     C/EJECT
239700940830     C************************************************************
239800940830     C* INIZIALIZZAZIONE VARIABILI
239900940830     C************************************************************
240000940830     C     INZVAR        BEGSR
240100940830     C*
240200960527     C                   MOVEL     KPJBU         ANA968DS
240300950509     C*
240400950509     C* Reperimento parametri da ANMPE di lavoro
240500950509     C*
240600950621     C                   Z-ADD     LAN968        MPEPRGLAN
240700950509     C                   MOVE      SOC968        MPESOC
240800950509     C     K2MPE1        CHAIN     ANMPE01W                           21
240900950509     C     *IN21         IFEQ      *OFF
241000950621     C* LANCIATORE
241100970813     C                   MOVEL     MPEPAR        NDCT51ds
241200950509     C                   ELSE
241300950509     C                   EXSR      ENDPGM
241400950509     C                   ENDIF
241500950509     C*
241600950509     C* Reperimento dati società
241700950509     C*
241800950509     C                   MOVEL     'SOC001'      TIPXSC
241900950509     C                   MOVEL     MPESOC        SOCXSC
242000950509     C                   EXSR      REPSOC
242100950509     C     RTNXSC        IFNE      '1'
242200950509     C                   MOVEL     XSOCDS        SOC001
242300950509     C                   ELSE
242400950509     C                   EXSR      ENDPGM
242500950509     C                   ENDIF
242600941129     C*
242700941129     C* Valorizzazione campi univoci testate
242800941129     C*
242900950704     C*  reperisco nome sistema informatico
243000950704     C                   CALL      'XNETA'                              21
243100950704     C                   PARM                    NOMSYS
243200950704     c*
243300941129     C                   MOVEL     KNSIF         NOMSIF
243400941129     C                   MOVEL     XSCDSI        NOMDIT
243500941129     C                   MOVEL     DSPGM         NOMPGM
243600950509     C*
243700970814     C* definisco qual'è la data riferimento e le date successive al passo
243800970814     C* giorni
243900970814     C                   CLEAR                   DtRif
244000970814     C                   IF        DtRifT51 <> *loval
244100970814     C                   MOVE      DtRifT51      DtRif(1)
244200970814     C                   ELSE
244300970814     C* chiamo la routine che ricava una data da un codice
244400970814     C                   MOVEL     CdDtRifT51    XCdData           2 0
244500970814     C                   CALL      'XDATCD'
244600970814     C                   PARM                    XCdData
244700970814     C                   PARM      *loval        DtRif(1)
244800970814     C                   ENDIF
244900970814     C* definisco tutte le altre date del passo (se c'è un passo giorni)
245000970814     C                   IF        PassoGGT51 > 0
245100970814     C     2             DO        7             x
245200970814     C                   EVAL      y = x-1
245300970814     C     DtRif(y)      ADDDUR    PassoGGT51:*d DtRif(x)
245400970814     C                   ENDDO
245500970814     C                   ENDIF
245600970814     C*
245700970814     C* riempio i campi di intestazione con le date di riferimento per saldi
245800000117C1309C******dmy          MOVE      DtRif(1)      DtRif1
245900000117C1309C     *JobRun       MOVE      DtRif(1)      DtRif1
246000970814     C                   IF        PassoGGT51 > 0
246100000117C1309C******dmy          MOVE      DtRif(2)      DtRif2
246200000117C1309C     *JobRun       MOVE      DtRif(2)      DtRif2
246300000117C1309C******dmy          MOVE      DtRif(3)      DtRif3
246400000117C1309C     *JobRun       MOVE      DtRif(3)      DtRif3
246500000117C1309C******dmy          MOVE      DtRif(4)      DtRif4
246600000117C1309C     *JobRun       MOVE      DtRif(4)      DtRif4
246700000117C1309C******dmy          MOVE      DtRif(5)      DtRif5
246800000117C1309C     *JobRun       MOVE      DtRif(5)      DtRif5
246900000117C1309C******dmy          MOVE      DtRif(6)      DtRif6
247000000117C1309C     *JobRun       MOVE      DtRif(6)      DtRif6
247100000117C1309C******dmy          MOVE      DtRif(7)      DtRif7
247200000117C1309C     *JobRun       MOVE      DtRif(7)      DtRif7
247300970814     C                   ELSE
247400970814     C                   Z-ADD     0             DtRif2
247500970814     C                   Z-ADD     0             DtRif3
247600970814     C                   Z-ADD     0             DtRif4
247700970814     C                   Z-ADD     0             DtRif5
247800970814     C                   Z-ADD     0             DtRif6
247900970814     C                   Z-ADD     0             DtRif7
248000970814     C                   ENDIF
248100970814     C*
248200950509     C                   ENDSR
248300940830     C/EJECT
248400940830     C************************************************************
248500940830     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
248600940830     C************************************************************
248700940830     C     DEFCAM        BEGSR
248800950203     C*
248900950315     C                   MOVE      *BLANKS       WRKTRC           10
249000950315     C                   MOVE      *BLANKS       WRKFLD           10
249100961017     C                   MOVE      *BLANKS       WRKRGC            4
249200970813     C*
249300970808     C* KLIST per CHAIN su ANMPE
249400970813     C*
249500950508     C     K2MPE1        KLIST                                                        *
249600950621     C                   KFLD                    MPEPRGLAN                            *
249700950508     C                   KFLD                    MPESOC                               *
249800970813     C*
249900970808     C* ANBFI01L
250000970813     C*
250100970813     C     K03BFI01      KLIST                                                        *
250200970813     C                   KFLD                    BFISocieta                           *
250300970813     C                   KFLD                    BFIBanca                             *
250400970813     C                   KFLD                    BFICdFido                            *
250500970813     C*
250600970813     C* ANBCT01L
250700981126     C*
250800970813     C     K05BCT01      KLIST                                                        *
250900970813     C                   KFLD                    BCTSocieta                           *
251000970813     C                   KFLD                    BCTTpTasso                           *
251100970813     C                   KFLD                    BCTBanca                             *
251200970813     C                   KFLD                    BCTLinea                             *
251300970813     C                   KFLD                    BCTDtIniz                            *
251400970813     C     K04BCT01      KLIST                                                        *
251500970813     C                   KFLD                    BCTSocieta                           *
251600970813     C                   KFLD                    BCTTpTasso                           *
251700970813     C                   KFLD                    BCTBanca                             *
251800970813     C                   KFLD                    BCTLinea                             *
251900981126     C*
252000981126     C* ANBRA01L
252100981126     C*
252200981126     C     K03BRA01      KLIST
252300981126     C                   KFLD                    WRASocieta
252400981126     C                   KFLD                    WRABanca
252500981126     C                   KFLD                    WRALinea
252600950127     C*
252700950127     C                   ENDSR
252800950202     C/EJECT
252900950202     C********************************************************************
253000950202      /SPACE 5
253100950202     C********************************************************************
253200950202     C* Decodifica chiavi per titoli
253300950202     C*
253400950206     C     DECTIT        BEGSR
253500950207     C                   MOVE      *BLANKS       WRKDES          256
253600950315     C*
253700950629   B1C     WRKRGC        IFNE      *BLANKS
253800961111     c*
253900961111     c* Se si tratta di una chiave composta, cambio il nome del tracciato
254000961111     C* e del campo per ottenere la decodifica( mi appoggio su uno virtuale)
254100961111     C                   select
254200961111     C                   when      wrkrgc = 'KSC' or wrkrgc = 'FOR' or
254300961111     C                             wrkrgc = 'CLI'
254400961111     c                   eval      wrktrc = 'ANDIZ000'
254500961111     c                   eval      wrkfld = 'VKCCKSC'
254600961111     C                   when      wrkrgc = 'ENT'
254700961111     c                   eval      wrktrc = 'ANDIZ000'
254800961111     c                   eval      wrkfld = 'VTIPENT'
254900961111     C                   when      wrkrgc = 'FILC'
255000961111     c                   eval      wrktrc = 'ANDIZ000'
255100961111     c                   eval      wrkfld = 'VFILCLI'
255200961111     C                   when      wrkrgc = 'FILF'
255300961111     c                   eval      wrktrc = 'ANDIZ000'
255400961111     c                   eval      wrkfld = 'VFILFOR'
255500961111     c                   endsl
255600950315     C*
255700960527     C                   CLEAR                   ANA971DS
255800950203     C                   MOVE      XSCSOC        SOC971
255900950203     C                   MOVEL     WRKTRC        TRC971
256000950203     C                   MOVEL     WRKFLD        FLD971
256100950203     C                   MOVEL     'EQ'          OPE971
256200950203     C                   MOVEL     WRKKEY        KEY971
256300950203     C                   MOVEL     'C'           MOD971
256400950203     C                   MOVEL     *BLANK        FLG971
256500960527     C                   MOVEL     ANA971DS      KPJBU
256600950203     C                   CALL      'ANA971R'                            98
256700950203     C                   PARM                    KPJBA
256800960527     C                   MOVEL     KPJBU         ANA971DS
256900950203     C*
257000950203     C                   MOVEL     DEC971        WRKDES
257100950206     C*
257200950206   E1C                   ENDIF
257300950202     C*
257400950202     C                   ENDSR
257500950413     C/EJECT
257600020125C1540 ************************************************************
257700020125  "   * Override  Ndsib
257800020125  "   ************************************************************
257900020125  "  C     OvrNdSIB      Begsr
258000020125  "   *
258100020125  "  C                   Clear                   Cmd
258200020125  "  C     'OVRDBF'      Cat       'NDSIB00W':1  Cmd
258300020125  "  C                   Cat       NomLibT51:1   Cmd
258400020125  "  C                   Cat       '/':0         Cmd
258500020125  "  C                   Cat       NomFilT51:0   Cmd
258600020125  "   *
258700020125  "  C                   Call      'QCMDEXC'
258800020125  "  C                   Parm                    Cmd              50
258900020125  "  C                   Parm      50            len              15 5
259000020125  "   *
259100020125  "  C                   Endsr
259200020125  "   ************************************************************
259300020125  "   * Creazione/pulizia file per scarico
259400020125  "   ************************************************************
259500020125  "  C     CallCT52C1    BEGSR
259600020125  "   *
259700020125  "  C                   Movel     Kpjba         Savjba
259800020125  "  C                   Movel     Ndct51Ds      Kpjbu
259900020125  "  C                   Call      'NDCT52C1'
260000020125  "  C                   PARM                    Kpjba
260100020125  "  C                   Movel     Savjba        Kpjba
260200020125  "   *
260300020125  "  C                   Endsr
260400020125  "   ************************************************************
260500020125  "   * Iscrizione file scarico a catalogo
260600020125  "   ************************************************************
260700020125  "  C     CALLX24       BEGSR
260800020125  "   *
260900020125  "  C                   Clear                   X24FpcDs
261000020125  "  C                   Clear                   X24File
261100020125  "  C                   Movel     NomFilT51     X24File
261200020125  "  C                   Movel     NomLibT51     X24Libre
261300020125  "  C                   Movel     '3'           X24Opz
261400020125  "  C                   Movel     XscSoc        X24Societa
261500020125  "  C                   Movel     'STASITB'     X24FunCre
261600020125  "  C                   Movel     'NDCT52R'     X24Pgm
261700020125  "  C                   Movel     MPEPrf        X24Utente
261800020125  "  C                   Movel     MPEDes        X24Descriz
261900020125  "  C                   Movel     'NDSIB00W'    X24Filem
262000020125  "  C                   Call      'X24FPCR'
262100020125  "  C                   Parm                    X24FpcDs
262200020125  "   *
262300020125C1540C                   Endsr
262400950215**CTDATA SK
262500970808select * from ANBRA00F
262600970813**CTDATA SK1
262700970813where BRASocieta in(
262800970813
262900970813**CTDATA SK2
263000970813'   ',
