000100000000     H*------------------------------------------------------------------------*
000200000000     H* STATISTICA TRAINI - ESEGUE
000300000000     H* NB. NON ELABORABILE A CAVALLO D'ANNO
000400000000     H*------------------------------------------------------------------------*
000500000000     H DECEDIT('0,') DATEDIT(*DMY.)
000600000000     F*------------------------------------------------------------------------*
000700000000     F* DATA BASE
000800000000     F*------------------------------------------------------------------------*
000900000000     FAZORG01L  IF   E           K DISK
001000000000     FTABEL00F  IF   E           K DISK
001100011011     FTNTBE01L  IF   E           K DISK
001200140114     FECEVD06L  IF   E           K DISK
001300000000     FTNSS76P   O    E             PRINTER OFLIND(*IN01)
001400000000     F*------------
001500000000     F* SALDI TRAZIONE
001600000000     F*------------
001700990609     FSISDT04L  IF   E           K DISK    USROPN
001800990609     FSISDT05L  IF   E           K DISK    USROPN
001900990609     F                                     RENAME(SISDT000:SISDT5)
002000000000     F*------------
002100000000     F* WORK FILE
002200000000     F*------------
002300000000     FWFTRA01L  IF   E           K DISK    USROPN
002400000000     FWFTRA02L  IF   E           K DISK    USROPN
002500000000     F                                     RENAME(WFTRA000:WFTRA2)
002600000000     F*------------
002700000000     F* FILE PC
002800000000     F*------------
002900011019     FTIPCT01L  UF A E           K DISK    USROPN
003000000000     D*------------------------------------------------------------------------*
003100000000     D* SCHIERE
003200000000     D*------------------------------------------------------------------------*
003300000000     D*------------------
003400000000     D* DECODIFICA RIGHE IN TESTATA
003500000000     D*------------------
003600000000     D DTE             S             20    DIM(5) CTDATA PERRCD(1)
003700000000     D*------------------
003800000000     D* DECODIFICA RIGHE DI STAMPA
003900000000     D*------------------
004000000000     D DRI             S             30    DIM(30) CTDATA PERRCD(1)
004100000000     D*------------------
004200000000     D* DECODIFICA MESI DELL'ANNO
004300000000     D*------------------
004400000000     D DMM             S              3    DIM(12) CTDATA PERRCD(1)
004500000000     D*------------------
004600000000     D* PERIDO DA ELABORARE
004700000000     D*------------------
004800990604     D ANN             S              4  0 DIM(12)
004900000000     D MES             S              2  0 DIM(12)
005000000000     D*------------------
005100000000     D* ORGANIGRAMMA
005200000000     D*------------------
005300980224     D ORG             S              3  0 DIM(500)                             *CODICE
005400980224     D ODES            S             20    DIM(500)                             *DESCRIZIONE
005500980224     D ODIT            S              3    DIM(500)                             *DITTA
005600020627     D OPOS            S              3    DIM(500)                             *NETWORK POSTE
005700000000     D*------------------
005800000000     D* FASCE DI PESO
005900000000     D*------------------
006000020522     D FPE             S             14    DIM(11)
006100011011     D*------------------
006200011012     D* DS LETTURA TABELLA "GED" PER DIVISE
006300011011     D*------------------
006400011015     D DGEDST        E DS                                                       *STATISTICHE
006500011012     D DGEDAC        E DS                                                       *ANAGRAFICHE C/E
006600011221     D DSGEDCE       E DS                                                       *C/ECONOMICO
006700000000     D*------------------
006800000000     D* SCHIERE VARIE -> 12 MESI + 3 DECADE + 1 TOTALE
006900000000     D*------------------
007000000000     D* DI LAVORO
007100000000     D $GPN            S             13  0 DIM(16)                              *GG PARTENZE NORMALI
007200000000     D $MAN            S             13  3 DIM(16)                              *MC ASSEGNATI NORMAL
007300000000     D $MAB            S             13  3 DIM(16)                              *MC ASSEGNATI BIS
007400000000     D $MAS            S             13  3 DIM(16)                              *MC ASSEGNATI SEDE
007500990217     D $MPF            S             13  3 DIM(16)                              *MC PARTITI FINO 50Q
007600990217     D $MPO            S             13  3 DIM(16)                              *MC PARTITI OLTRE 50
007700090924     D $ITR            S             16  5 DIM(16)                              *TN PER MCUBO
007800011019     D $CTL            S             16  3 DIM(16)                              *RICAVO TN TOTALE
007900990217     D $SPF            S             13  0 DIM(16)                              *SPEDIZIONI FINO 50Q
008000990217     D $QLF            S             13  3 DIM(16)                              *KILOGRAMMI FINO 50Q
008100990217     D $SPO            S             13  0 DIM(16)                              *SPEDIZIONI OLTRE 50
008200990217     D $QLO            S             13  3 DIM(16)                              *KILOGRAMMI OLTRE 50
008300090924     D $CTT            S             16  5 DIM(16)                              *COSTO TOTALE TN
008400090924     D $CST            S             16  5 DIM(16)                              *COSTO SEDE   TN
008500000000     D* DI PASSAGGIO
008600090924     D PV1             S             16  5 DIM(16)                              *VALORE UNO
008700000000     D PP1             S              6  3 DIM(16)                              *PERCENTUALE UNO
008800000000     D*------------------
008900011019     D* SCHIERE MULTIPLE
009000000000     D*------------------
009100011019     D* Numero di byte packed
009200011019     D* ES. (13:2)+1 = 7,5 ; (7X12) =  84
009300011019     D*     (16:2)+1 = 9   ; (9X12) = 108
009400000000     D*------------------
009500000000     D* DS MULTIPLA LINEA                 -3 DECADI-
009600000000     D*------------------
009700011019     D LDEC            DS                  OCCURS(3) INZ
009800011019     D  LGPN                   1     84P 0 DIM(12)                              *GG PARTENZE NORMALI
009900011019     D  LMAN                  85    168P 3 DIM(12)                              *MC ASSEGNATI NORMAL
010000011019     D  LMAB                 169    252P 3 DIM(12)                              *MC ASSEGNATI BIS
010100011019     D  LMAS                 253    336P 3 DIM(12)                              *MC ASSEGNATI SEDE
010200011019     D  LMPF                 337    420P 3 DIM(12)                              *MC PARTITI FINO 50Q
010300011019     D  LMPO                 421    504P 3 DIM(12)                              *MC PARTITI OLTRE 50
010400090924     D  LITR                 505    612P 5 DIM(12)                              *TN PER MCUBO
010500011019     D  LCTL                 613    720P 3 DIM(12)                              *RICAVO TN TOTALE
010600011019     D  LSPF                 721    804P 0 DIM(12)                              *SPEDIZIONI FINO 50Q
010700011019     D  LQLF                 805    888P 3 DIM(12)                              *KILOGRAMMI FINO 50Q
010800011019     D  LSPO                 889    972P 0 DIM(12)                              *SPEDIZIONI OLTRE 50
010900011019     D  LQLO                 973   1056P 3 DIM(12)                              *KILOGRAMMI OLTRE 50
011000090924     D  LCTT                1057   1164P 5 DIM(12)                              *COSTO TOTALE TN
011100090924     D  LCST                1165   1272P 5 DIM(12)                              *COSTO SEDE   TN
011200000000     D*------------------
011300000000     D* DS MULTIPLA TERMINAL              -3 DECADI-
011400000000     D*------------------
011500011019     D TDEC            DS                  OCCURS(3) INZ
011600011019     D  TGPN                   1     84P 0 DIM(12)                              *GG PARTENZE NORMALI
011700011019     D  TMAN                  85    168P 3 DIM(12)                              *MC ASSEGNATI NORMAL
011800011019     D  TMAB                 169    252P 3 DIM(12)                              *MC ASSEGNATI BIS
011900011019     D  TMAS                 253    336P 3 DIM(12)                              *MC ASSEGNATI SEDE
012000011019     D  TMPF                 337    420P 3 DIM(12)                              *MC PARTITI FINO 50Q
012100011019     D  TMPO                 421    504P 3 DIM(12)                              *MC PARTITI OLTRE 50
012200090924     D  TITR                 505    612P 5 DIM(12)                              *TN PER MCUBO
012300011019     D  TCTL                 613    720P 3 DIM(12)                              *RICAVO TN TOTALE
012400011019     D  TSPF                 721    804P 0 DIM(12)                              *SPEDIZIONI FINO 50Q
012500011019     D  TQLF                 805    888P 3 DIM(12)                              *KILOGRAMMI FINO 50Q
012600011019     D  TSPO                 889    972P 0 DIM(12)                              *SPEDIZIONI OLTRE 50
012700011019     D  TQLO                 973   1056P 3 DIM(12)                              *KILOGRAMMI OLTRE 50
012800090924     D  TCTT                1057   1164P 5 DIM(12)                              *COSTO TOTALE TN
012900090924     D  TCST                1165   1272P 5 DIM(12)                              *COSTO SEDE   TN
013000980224     D*------------------
013100980224     D* DS MULTIPLA DIVISIONE BARTOLINI   -3 DECADI-
013200980224     D*------------------
013300011019     D BDEC            DS                  OCCURS(3) INZ
013400011019     D  BGPN                   1     84P 0 DIM(12)                              *GG PARTENZE NORMALI
013500011019     D  BMAN                  85    168P 3 DIM(12)                              *MC ASSEGNATI NORMAL
013600011019     D  BMAB                 169    252P 3 DIM(12)                              *MC ASSEGNATI BIS
013700011019     D  BMAS                 253    336P 3 DIM(12)                              *MC ASSEGNATI SEDE
013800011019     D  BMPF                 337    420P 3 DIM(12)                              *MC PARTITI FINO 50Q
013900011019     D  BMPO                 421    504P 3 DIM(12)                              *MC PARTITI OLTRE 50
014000090924     D  BITR                 505    612P 5 DIM(12)                              *TN PER MCUBO
014100011019     D  BCTL                 613    720P 3 DIM(12)                              *RICAVO TN TOTALE
014200011019     D  BSPF                 721    804P 0 DIM(12)                              *SPEDIZIONI FINO 50Q
014300011019     D  BQLF                 805    888P 3 DIM(12)                              *KILOGRAMMI FINO 50Q
014400011019     D  BSPO                 889    972P 0 DIM(12)                              *SPEDIZIONI OLTRE 50
014500011019     D  BQLO                 973   1056P 3 DIM(12)                              *KILOGRAMMI OLTRE 50
014600090924     D  BCTT                1057   1164P 5 DIM(12)                              *COSTO TOTALE TN
014700090924     D  BCST                1165   1272P 5 DIM(12)                              *COSTO SEDE   TN
014800980224     D*------------------
014900980224     D* DS MULTIPLA DIVISIONE S.D.I.      -3 DECADI-
015000980224     D*------------------
015100011019     D SDEC            DS                  OCCURS(3) INZ
015200011019     D  SGPN                   1     84P 0 DIM(12)                              *GG PARTENZE NORMALI
015300011019     D  SMAN                  85    168P 3 DIM(12)                              *MC ASSEGNATI NORMAL
015400011019     D  SMAB                 169    252P 3 DIM(12)                              *MC ASSEGNATI BIS
015500011019     D  SMAS                 253    336P 3 DIM(12)                              *MC ASSEGNATI SEDE
015600011019     D  SMPF                 337    420P 3 DIM(12)                              *MC PARTITI FINO 50Q
015700011019     D  SMPO                 421    504P 3 DIM(12)                              *MC PARTITI OLTRE 50
015800090924     D  SITR                 505    612P 5 DIM(12)                              *TN PER MCUBO
015900011019     D  SCTL                 613    720P 3 DIM(12)                              *RICAVO TN TOTALE
016000011019     D  SSPF                 721    804P 0 DIM(12)                              *SPEDIZIONI FINO 50Q
016100011019     D  SQLF                 805    888P 3 DIM(12)                              *KILOGRAMMI FINO 50Q
016200011019     D  SSPO                 889    972P 0 DIM(12)                              *SPEDIZIONI OLTRE 50
016300011019     D  SQLO                 973   1056P 3 DIM(12)                              *KILOGRAMMI OLTRE 50
016400090924     D  SCTT                1057   1164P 5 DIM(12)                              *COSTO TOTALE TN
016500090924     D  SCST                1165   1272P 5 DIM(12)                              *COSTO SEDE   TN
016600000000     D*------------------
016700000000     D* DS MULTIPLA GENERALE              -3 DECADI-
016800000000     D*------------------
016900011019     D GDEC            DS                  OCCURS(3) INZ
017000011019     D  GGPN                   1     84P 0 DIM(12)                              *GG PARTENZE NORMALI
017100011019     D  GMAN                  85    168P 3 DIM(12)                              *MC ASSEGNATI NORMAL
017200011019     D  GMAB                 169    252P 3 DIM(12)                              *MC ASSEGNATI BIS
017300011019     D  GMAS                 253    336P 3 DIM(12)                              *MC ASSEGNATI SEDE
017400011019     D  GMPF                 337    420P 3 DIM(12)                              *MC PARTITI FINO 50Q
017500011019     D  GMPO                 421    504P 3 DIM(12)                              *MC PARTITI OLTRE 50
017600090924     D  GITR                 505    612P 5 DIM(12)                              *TN PER MCUBO
017700011019     D  GCTL                 613    720P 3 DIM(12)                              *RICAVO TN TOTALE
017800011019     D  GSPF                 721    804P 0 DIM(12)                              *SPEDIZIONI FINO 50Q
017900011019     D  GQLF                 805    888P 3 DIM(12)                              *KILOGRAMMI FINO 50Q
018000011019     D  GSPO                 889    972P 0 DIM(12)                              *SPEDIZIONI OLTRE 50
018100011019     D  GQLO                 973   1056P 3 DIM(12)                              *KILOGRAMMI OLTRE 50
018200090924     D  GCTT                1057   1164P 5 DIM(12)                              *COSTO TOTALE TN
018300090924     D  GCST                1165   1272P 5 DIM(12)                              *COSTO SEDE   TN
018400000000     D*------------------
018500000000     D* DS MULTIPLA STAMPA                -3 DECADI-
018600000000     D*------------------
018700011019     D XDEC            DS                  OCCURS(3) INZ
018800011019     D  XGPN                   1     84P 0 DIM(12)                              *GG PARTENZE NORMALI
018900011019     D  XMAN                  85    168P 3 DIM(12)                              *MC ASSEGNATI NORMAL
019000011019     D  XMAB                 169    252P 3 DIM(12)                              *MC ASSEGNATI BIS
019100011019     D  XMAS                 253    336P 3 DIM(12)                              *MC ASSEGNATI SEDE
019200011019     D  XMPF                 337    420P 3 DIM(12)                              *MC PARTITI FINO 50Q
019300011019     D  XMPO                 421    504P 3 DIM(12)                              *MC PARTITI OLTRE 50
019400090924     D  XITR                 505    612P 5 DIM(12)                              *TN PER MCUBO
019500011019     D  XCTL                 613    720P 3 DIM(12)                              *RICAVO TN TOTALE
019600011019     D  XSPF                 721    804P 0 DIM(12)                              *SPEDIZIONI FINO 50Q
019700011019     D  XQLF                 805    888P 3 DIM(12)                              *KILOGRAMMI FINO 50Q
019800011019     D  XSPO                 889    972P 0 DIM(12)                              *SPEDIZIONI OLTRE 50
019900011019     D  XQLO                 973   1056P 3 DIM(12)                              *KILOGRAMMI OLTRE 50
020000090924     D  XCTT                1057   1164P 5 DIM(12)                              *COSTO TOTALE TN
020100090924     D  XCST                1165   1272P 5 DIM(12)                              *COSTO SEDE   TN
020200000000     D*------------------
020300000000     D* ARCHITETTURA
020400000000     D*------------------
020500000000     D KPJBA         E DS
020600000000     D  PROFIL                 1     10
020700000000     D*------------------
020800000000     D* DS PARAMETRI LANCIO
020900000000     D*------------------
021000000000     D PARAM           DS
021100990604     D  PARDAT                 1      6  0
021200990604     D  PARAAI                 1      4  0
021300990604     D  PARMMI                 5      6  0
021400990604     D  PARDA2                 7     12  0
021500990604     D  PARAAF                 7     10  0
021600990604     D  PARMMF                11     12  0
021700990604     D  PARMPA                13     13
021800990604     D  PARFPC                14     14
021900990604     D  PARCRE                15     15
022000000706     D  PARPOS                16     16
022100011018     D*------------------
022200011018     D* Variabili di controllo
022300011018     D*------------------
022400011018     D $DIVAC          S              3                                         *div INP anag C/Eco
022500011221     D $DIVCE          S              3                                         *div INP C/economico
022600011018     D $DIVSTT         S              3                                         *div OUT statistica
022700011018     D §DIV            S              3                                         *divisa generica
022800011018     D*------------------
022900011018     D* CONTROLLA DATA
023000011018     D*------------------
023100011018     D wlbda8          DS                  INZ                                  *controlla data
023200011018     D  g08dat                        8  0                                       -data dritta
023300011018     D  g08inv                        8  0                                       -data invertita
023400011018     D  g08err                        1                                          -errore
023500011018     D  g08tgi                        5  0                                       -giorni fra date
023600000000     D*------------------
023700000000     D* DS UNIONE DATA SALDI TRAZIONE
023800000000     D*------------------
023900000000     D                 DS
024000990604     D  SDTANN                 1      4  0
024100990604     D  SDTMES                 5      6  0
024200990604     D  SDTDAT                 1      6  0
024300000000     D*------------------
024400000000     D* DS DEPOSITO DATA SALDI TRAZIONE
024500000000     D*------------------
024600000000     D                 DS
024700990604     D  DEPANN                 1      4  0
024800990604     D  DEPMES                 5      6  0
024900990604     D  DEPDAT                 1      6  0
025000000000     D*------------------
025100000000     D* DS UNIONE DATA FILE PC
025200000000     D*------------------
025300000000     D                 DS
025400011019     D  PCTANN                 1      4  0
025500990604     D  PCTMES                 5      6  0
025600990604     D  PCTDAT                 1      6  0
025700000000     D*------------------
025800000000     D* DS SCOMPOSIZIONE DATA DECORRENZA TARIFFA
025900000000     D*------------------
026000000000     D                 DS
026100990604     D  EVDDEC                 1      6  0
026200000000     D  EVDDDT                 1      8  0
026300000000     D*------------------
026400000000     D* DS SCOMPOSIZIONE DATA DECORRENZA TARIFFA
026500000000     D*------------------
026600000000     D                 DS
026700990604     D  EVDSCA                 1      6  0
026800000000     D  EVDDST                 1      8  0
026900011011     D*------------------
027000011011     D* DS COMPOSIZIONE DATA -AAAA/MM/GG-
027100011011     D*------------------
027200011011     D                 DS
027300011011     D  DSANN                  1      4  0
027400011011     D  DSMES                  5      6  0
027500011011     D  DSGIO                  7      8  0
027600011011     D DSDAT                   1      8  0
027700000000     D*------------------
027800000000     D* DS FASCE DI PESO
027900000000     D*------------------
028000000000     D DS2L          E DS
028100000706     D*------------------
028200000706     D* DS LETTURA CAMPO ORGDE3 DI AZORG00F
028300000706     D*------------------
028400000706     D OG143         E DS
028500011011     D*-------------------
028600011011     D* CONVERSIONI IMPORTI
028700011011     D*-------------------
028800011011     D YEUR          E DS                  EXTNAME(YEURCODS)
028900000000     C*------------------------------------------------------------------------*
029000000000     C* MAIN LINE
029100000000     C*------------------------------------------------------------------------*
029200000000     C*
029300000000     C* STAMPA LA PAGINA DEI PARAMETRI DI LANCIO
029400000000     C                   EXSR      STAPAR
029500000000     C*
029600000000     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
029700000000     C                   EXSR      SETFIL
029800000000     C*
029900000000     C* CICLO FINO A FINE FILE
030000000000DO  1C     $FINE         DOWEQ     '0'
030100000000     C*
030200000000     C* OPERAZIONI PER NUOVO TERMINAL UNO
030300000000     C                   EXSR      NEWTF1
030400000000     C*
030500000000     C* CICLO FINO A ROTTURA TERMINAL UNO
030600000000DO  2C     $FINE         DOWEQ     '0'
030700000000     C     WTF1          ANDEQ     DEPTF1
030800000000     C*
030900000000     C* OPERAZIONI PER NUOVO TERMINAL DUE
031000000000     C                   EXSR      NEWTF2
031100000000     C*
031200000000     C* CICLO FINO A ROTTURA TERMINAL DUE
031300000000DO  3C     $FINE         DOWEQ     '0'
031400000000     C     WTF1          ANDEQ     DEPTF1
031500000000     C     WTF2          ANDEQ     DEPTF2
031600000000     C*
031700000000     C* OPERAZIONI PER NUOVO MESE
031800000000     C                   EXSR      NEWMES
031900000000     C*
032000000000     C* CICLO FINO A ROTTURA MESE
032100000000DO  4C     $FINE         DOWEQ     '0'
032200000000     C     WTF1          ANDEQ     DEPTF1
032300000000     C     WTF2          ANDEQ     DEPTF2
032400000000     C     SDTMES        ANDEQ     DEPMES
032500000000     C*
032600000000     C* OPERAZIONI PER NUOVA DECADE
032700000000     C                   EXSR      NEWDEC
032800000000     C*
032900000000     C* CICLO FINO A ROTTURA DECADE
033000000000DO  5C     $FINE         DOWEQ     '0'
033100000000     C     WTF1          ANDEQ     DEPTF1
033200000000     C     WTF2          ANDEQ     DEPTF2
033300000000     C     SDTMES        ANDEQ     DEPMES
033400000000     C     SDTDEC        ANDEQ     DEPDEC
033500000000     C*
033600000000     C* MEMORIZZA I DATI DEI SALDI
033700000000     C                   EXSR      MEMSAL
033800000000     C*
033900000000     C* LEGGE IL RECORD SUCCESSIVO
034000000000     C                   EXSR      LETFIL
034100000000E   5C                   ENDDO                                                  *ROTTURA DECADE
034200000000E   4C                   ENDDO                                                  *ROTTURA MESE
034300000000     C*
034400000000     C* MEMORIZZA I DATI DEL WORKFILE
034500000000     C                   EXSR      MEMWFR
034600000000E   3C                   ENDDO                                                  *ROTTURA LINEA
034700000000     C*
034800000000     C* STAMPA TOTALI PER TERMINAL DUE (LINEA)
034900000000     C                   EXSR      STATF2
035000000000     C*
035100000000     C* SCRIVE TOTALI PER TERMINAL DUE (LINEA) DEL FILE PC (SE RICHIESTO)
035200000000IF  3C     PARFPC        IFEQ      'S'
035300000000     C                   EXSR      SCRFPC
035400000000E   2C                   ENDIF
035500000000E   2C                   ENDDO                                                  *ROTTURA TERMINAL
035600000000     C*
035700000000     C* STAMPA TOTALI PER TERMINAL UNO
035800000000     C                   EXSR      STATF1
035900000000E   1C                   ENDDO                                                  *FINE FILE
036000980224     C*
036100980224     C* STAMPA TOTALI PER DIVISIONE BARTOLINI
036200980224     C                   EXSR      STABAR
036300980224     C*
036400980224     C* STAMPA TOTALI PER DIVISIONE S.D.I.
036500980224     C                   EXSR      STASDI
036600000000     C*
036700000000     C* STAMPA TOTALI GENERALI
036800000000     C                   EXSR      STAGEN
036900000000     C*
037000000000     C* STAMPA RIGA DI FINE PROSPETTO
037100000000     C                   WRITE     SS76FI
037200000000     C*
037300000000     C* CHIUSURA FILE
037400000000     C                   EXSR      CLOFIL
037500000000     C*
037600000000     C                   SETON                                        LR
037700000000     C*------------------------------------------------------------------------*
037800000000     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA
037900000000     C*------------------------------------------------------------------------*
038000000000     C     SETFIL        BEGSR
038100000000     C*
038200000000     C                   Z-ADD     PARAAI        KSTANN                         *ANNO
038300000000IF  1C     PARMPA        IFEQ      'P'                                          P A R T E N Z A
038400990609     C     KSTANN        SETLL     SISDT04L                           99        *FINE ARCHIVIO
038500000000X   1C                   ELSE                                                   A R R I V O
038600990609     C     KSTANN        SETLL     SISDT05L                           99        *FINE ARCHIVIO
038700000000E   1C                   ENDIF
038800000000     C*
038900000000IF  1C     *IN99         IFEQ      *ON
039000000000     C                   MOVEL     '1'           $FINE
039100000000X   1C                   ELSE
039200000000     C                   EXSR      LETFIL                                       *LETTURA RECORD
039300000000E   1C                   ENDIF
039400000000     C*
039500000000     C                   ENDSR
039600000000     C*------------------------------------------------------------------------*
039700000000     C* LETFIL - LETTURA PROSSIMO RECORD
039800000000     C*------------------------------------------------------------------------*
039900000000     C     LETFIL        BEGSR
040000000000     C*
040100000000     C* LEGGE FINO A:
040200000000     C*     - FINE FILE
040300000000     C*     - TROVATO RECORD VALIDO
040400000000     C                   MOVEL     'N'           WRECOK
040500000000DO  1C     $FINE         DOWEQ     '0'
040600000000     C     WRECOK        ANDEQ     'N'
040700000000     C*
040800990610IF  2C     PARMPA        IFEQ      'P'                                          P A R T E N Z A
040900990609     C     KSTANN        READE     SISDT04L                               99
041000000000X   2C                   ELSE                                                   A R R I V O
041100990609     C     KSTANN        READE     SISDT05L                               99
041200000000E   2C                   ENDIF
041300000000     C*
041400000000IF  2C     *IN99         IFEQ      *ON                                          *FINE ARCHIVIO
041500000000     C                   MOVEL     '1'           $FINE
041600000000X   2C                   ELSE
041700000000     C                   EXSR      CHKREC                                       *CONTROLLO RECORD
041800000000E   2C                   ENDIF
041900000000E   1C                   ENDDO
042000000000     C*
042100000000     C                   ENDSR
042200000000     C*------------------------------------------------------------------------*
042300000000     C* CHKREC - CONTROLLA VALIDITA' RECORD
042400000000     C*------------------------------------------------------------------------*
042500000000     C     CHKREC        BEGSR
042600000000     C*
042700000000     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
042800000000     C*
042900000000     C* PERIODO NON RICHIESTO, ESCLUDE
043000000000IF  1C     SDTDAT        IFGE      PARDAT
043100000000     C     SDTDAT        ANDLE     PARDA2
043200000000X   1C                   ELSE
043300000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
043400000000E   1C                   ENDIF
043500960927     C*
043600960927     C* IL PERIODO PRIMA DI GIUGNO 96 E' ERRATO, QUINDI ESCLUDE
043700990604IF  1C     SDTDAT        IFLT      199606
043800960927     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
043900960927E   1C                   ENDIF
044000000706     C*
044100000706     C* SE NON RICHIESTI I DATI POSTE, ESCLUDE I TFP E I TFA POSTE (E VICEVERSA)
044200000706IF  1C     WRECOK        IFEQ      'S'
044300000706     C                   Z-ADD     1             I
044400000706     C                   SETOFF                                           28
044500000706     C     SDTTFP        LOOKUP    ORG(I)                                 28
044600000706     C                   MOVEL     OPOS(I)       WPPOS                          *FLAG POSTE TFP
044700000706     C                   Z-ADD     1             I
044800000706     C                   SETOFF                                           28
044900000706     C     SDTTFA        LOOKUP    ORG(I)                                 28
045000000706     C                   MOVEL     OPOS(I)       WAPOS                          *FLAG POSTE TFA
045100000706IF  2C     PARPOS        IFEQ      'N'                                          *NO DATI POSTE
045200020627     C     WPPOS         ANDNE     'PPT'                                         TFP <> POSTE e
045300000706     C     WAPOS         ANDNE     'S'                                           TFA <> POSTE
045400000706     C     PARPOS        OREQ      'S'                                          *SI DATI POSTE
045500020627     C     WPPOS         ANDEQ     'PPT'                                         TFP = POSTE o
045600000706     C     PARPOS        OREQ      'S'
045700000706     C     WAPOS         ANDEQ     'S'                                           TFA = POSTE
045800000706X   2C                   ELSE
045900000706     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
046000000706E   2C                   ENDIF
046100000706E   1C                   ENDIF
046200000000     C***
046300000000     C* PARTENZA = FIL PARTENZA -> TERMINAL UNO  E  FIL ARRIVO   -> TERMINAL DUE
046400000000     C* ARRIVO   = FIL ARRIVO   -> TERMINAL UNO  E  FIL PARTENZA -> TERMINAL DUE
046500000000     C***
046600000000IF  1C     PARMPA        IFEQ      'P'                                          P A R T E N Z A
046700000000     C                   Z-ADD     SDTTFP        WTF1                           *FIL PART->TERM UNO
046800000000     C                   Z-ADD     SDTTFA        WTF2                           *FIL ARR ->TERM DUE
046900000000X   1C                   ELSE                                                   A R R I V O
047000000000     C                   Z-ADD     SDTTFA        WTF1                           *FIL ARR ->TERM UNO
047100000000     C                   Z-ADD     SDTTFP        WTF2                           *FIL PART->TERM DUE
047200000000E   1C                   ENDIF
047300000000     C*
047400000000     C                   ENDSR
047500000000     C*------------------------------------------------------------------------*
047600000000     C* MEMSAL - MEMORIZZA I DATI DAI SALDI TRAZIONE
047700000000     C*------------------------------------------------------------------------*
047800000000     C     MEMSAL        BEGSR
047900011018     C*
048000011019     C* SE LA DIVISA DI OUT DELLA STATISTICA E' DIVERSA DELLA DIVISA DEI SALDI
048100011019     C* CONVERTE GLI IMPORTI NELLA DIVISA DI OUT DELLA STATISTICA -SECONDO LA DATA CORRENTE-
048200011018     C                   Z-ADD     *ZEROS        DEPCTL
048300011018     C                   EXSR      CNVSDT
048400980224     C* MESE
048500980224     C                   Z-ADD     1             M                 2 0
048600980224     C                   SETOFF                                           28
048700980224     C     DEPMES        LOOKUP    MES(M)                                 28
048800980224IF  1C     *IN28         IFEQ      *ON
048900980224     C* DECADE
049000980224     C                   MOVEL     DEPDEC        D                 1 0
049100980224     C***
049200980224     C* TERMINAL DUE (LINEA)
049300980224     C***
049400980224     C     D             OCCUR     LDEC
049500980224     C                   MOVEL     '1'           PASTF2                         *MEMORIZZATO QC TF2
049600990217IF  2C     SDTFPE        IFEQ      '01'                                         *FINO 50 QL
049700990217     C                   ADD       SDTVOL        LMPF(M)                        *MC PARTITI FINO 50Q
049800980224     C                   ADD       SDTNSP        LSPF(M)                        *SPEDIZIONI
049900980224     C                   ADD       SDTPKG        LQLF(M)                        *KILOGRAMMI
050000990217X   2C                   ELSE                                                   *OLTRE 50 QL
050100990217     C                   ADD       SDTVOL        LMPO(M)                        *MC PARTITI OLTRE 50
050200980224     C                   ADD       SDTNSP        LSPO(M)                        *SPEDIZIONI
050300980224     C                   ADD       SDTPKG        LQLO(M)                        *KILOGRAMMI
050400980224E   2C                   ENDIF
050500011018     C                   ADD       DEPCTL        LCTL(M)                        *RICAVO TN TOTALE
050600980224     C***
050700980224     C* TERMINAL UNO
050800980224     C***
050900980224     C* SE LOCALE NON MEMORIZZA I DATI NEL TOTALE
051000980224IF  2C     DEPTF1        IFNE      DEPTF2                                       *NON LOCALE
051100980224     C     D             OCCUR     TDEC
051200980224     C                   MOVEL     '1'           PASTF1                         *MEMORIZZATO QC TF1
051300990217IF  3C     SDTFPE        IFEQ      '01'                                         *FINO 50 QL
051400990217     C                   ADD       SDTVOL        TMPF(M)                        *MC PARTITI FINO 50Q
051500980224     C                   ADD       SDTNSP        TSPF(M)                        *SPEDIZIONI
051600980224     C                   ADD       SDTPKG        TQLF(M)                        *KILOGRAMMI
051700990217X   3C                   ELSE                                                   *OLTRE 50 QL
051800990217     C                   ADD       SDTVOL        TMPO(M)                        *MC PARTITI OLTRE 50
051900980224     C                   ADD       SDTNSP        TSPO(M)                        *SPEDIZIONI
052000980224     C                   ADD       SDTPKG        TQLO(M)                        *KILOGRAMMI
052100980224E   3C                   ENDIF
052200011018     C                   ADD       DEPCTL        TCTL(M)                        *RICAVO TN TOTALE
052300980224E   2C                   ENDIF
052400980224     C***
052500980224     C* DIVISIONE BARTOLINI
052600980224     C***
052700980224IF  2C     DEPDI1        IFEQ      'BAR'
052800001214     C     PARDAT        ORGE      200101                                       *X FUSIONE
052900001213     C     D             OCCUR     BDEC
053000980224     C                   MOVEL     '1'           PASDIB                         *MEMORIZZATO QC DIV
053100990217IF  3C     SDTFPE        IFEQ      '01'                                         *FINO 50 QL
053200990217     C                   ADD       SDTVOL        BMPF(M)                        *MC PARTITI FINO 50Q
053300980224     C                   ADD       SDTNSP        BSPF(M)                        *SPEDIZIONI
053400980224     C                   ADD       SDTPKG        BQLF(M)                        *KILOGRAMMI
053500990217X   3C                   ELSE                                                   *OLTRE 50 QL
053600990217     C                   ADD       SDTVOL        BMPO(M)                        *MC PARTITI OLTRE 50
053700980224     C                   ADD       SDTNSP        BSPO(M)                        *SPEDIZIONI
053800980224     C                   ADD       SDTPKG        BQLO(M)                        *KILOGRAMMI
053900980224E   3C                   ENDIF
054000011018     C                   ADD       DEPCTL        BCTL(M)                        *RICAVO TN TOTALE
054100980224     C***
054200980224     C* DIVISIONE S.D.I.
054300980224     C***
054400980224X   2C                   ELSE
054500980224     C     D             OCCUR     SDEC
054600980224     C                   MOVEL     '1'           PASDIS                         *MEMORIZZATO QC DIV
054700990217IF  3C     SDTFPE        IFEQ      '01'                                         *FINO 50 QL
054800990217     C                   ADD       SDTVOL        SMPF(M)                        *MC PARTITI FINO 50Q
054900980224     C                   ADD       SDTNSP        SSPF(M)                        *SPEDIZIONI
055000980224     C                   ADD       SDTPKG        SQLF(M)                        *KILOGRAMMI
055100990217X   3C                   ELSE                                                   *OLTRE 50 QL
055200990217     C                   ADD       SDTVOL        SMPO(M)                        *MC PARTITI OLTRE 50
055300980224     C                   ADD       SDTNSP        SSPO(M)                        *SPEDIZIONI
055400980224     C                   ADD       SDTPKG        SQLO(M)                        *KILOGRAMMI
055500980224E   3C                   ENDIF
055600011018     C                   ADD       DEPCTL        SCTL(M)                        *RICAVO TN TOTALE
055700980224E   2C                   ENDIF
055800980224     C***
055900980224     C* GENERALE
056000980224     C***
056100980224     C     D             OCCUR     GDEC
056200980224     C                   MOVEL     '1'           PASGEN                         *MEMORIZZATO QC GEN
056300990217IF  2C     SDTFPE        IFEQ      '01'                                         *FINO 50 QL
056400990217     C                   ADD       SDTVOL        GMPF(M)                        *MC PARTITI FINO 50Q
056500980224     C                   ADD       SDTNSP        GSPF(M)                        *SPEDIZIONI
056600980224     C                   ADD       SDTPKG        GQLF(M)                        *KILOGRAMMI
056700990217X   2C                   ELSE                                                   *OLTRE 50 QL
056800990217     C                   ADD       SDTVOL        GMPO(M)                        *MC PARTITI OLTRE 50
056900980224     C                   ADD       SDTNSP        GSPO(M)                        *SPEDIZIONI
057000980224     C                   ADD       SDTPKG        GQLO(M)                        *KILOGRAMMI
057100980224E   2C                   ENDIF
057200011018     C                   ADD       DEPCTL        GCTL(M)                        *RICAVO TN TOTALE
057300980224E   1C                   ENDIF
057400000000     C*
057500000000     C                   ENDSR
057600011018     C*------------------------------------------------------------------------*
057700011018     C* CNVSDT - CONVERTE IMPORTI DI SISDT
057800011018     C*------------------------------------------------------------------------*
057900011018     C     CNVSDT        BEGSR
058000011018     C*
058100011221IF  1C     $DIVSTT       IFNE      $DIVCE                                       *DIVISE <>
058200011221     C                   MOVEL     $DIVCE        WDVI              3            *DIVISA  INPUT
058300011018     C                   Z-ADD     SDTCTL        WIMI             30 9          *IMPORTO INPUT
058400011018     C                   EXSR      CNVEUR
058500011018     C                   Z-ADD     WIMO          DEPCTL                         *TN PER MC
058600011018X   1C                   ELSE                                                   *DIVISE =
058700011018     C                   Z-ADD     SDTCTL        DEPCTL                         *TN PER MC
058800011018E   1C                   ENDIF
058900011018     C*
059000011018     C                   ENDSR
059100000000     C*------------------------------------------------------------------------*
059200000000     C* MEMWFR - MEMORIZZA I DATI DAL WORKFILE
059300000000     C*------------------------------------------------------------------------*
059400000000     C     MEMWFR        BEGSR
059500000000     C*
059600000000     C* IMPOSTA LA CHIAVE CON I DATI DEPOSITATI
059700000000     C                   Z-ADD     DEPTF1        KWRTF1                         *FILIALE UNO
059800000000     C                   Z-ADD     DEPTF2        KWRTF2                         *FILIALE DUE
059900000000     C                   Z-ADD     DEPANN        KWRANN                         *ANNO
060000000000     C                   Z-ADD     DEPMES        KWRMES                         *MESE
060100000000     C*
060200000000     C* PER IL MESE DEPOSITATO
060300000000     C                   Z-ADD     1             M                 2 0          *MESE
060400000000     C                   SETOFF                                           28
060500000000     C     DEPMES        LOOKUP    MES(M)                                 28
060600000000IF  1C     *IN28         IFEQ      *ON
060700000000     C*
060800000000     C* E PER CIASCUNA DECADE DEL MESE, LEGGE I DATI DAL WORKFILE
060900000000DO  2C     1             DO        3             D
061000000000     C                   MOVEL     D             KWRDEC                         *DECADE
061100000000IF  3C     PARMPA        IFEQ      'P'                                          P A R T E N Z A
061200000000     C     KEYTRA        CHAIN     WFTRA01L                           98
061300000000X   3C                   ELSE                                                   A R R I V O
061400000000     C     KEYTRA        CHAIN     WFTRA02L                           98
061500000000E   3C                   ENDIF
061600000000     C*
061700000000     C* CALCOLA IL COSTO TOTALE TN
061800090924     C                   Z-ADD     *ZEROS        DEPCTT           16 5
061900000000IF  3C     *IN98         IFEQ      *OFF
062000000000     C     WFRMAN        ADD       WFRMAB        WN133            13 3
062100000000     C     WN133         MULT(H)   DEPITR        DEPCTT
062200000000E   3C                   ENDIF
062300000000     C*
062400000000     C* CALCOLA IL COSTO SEDE TN
062500090924     C                   Z-ADD     *ZEROS        DEPCST           16 5
062600000000IF  3C     *IN98         IFEQ      *OFF
062700000000     C     WFRMAS        MULT(H)   DEPITR        DEPCST
062800000000E   3C                   ENDIF
062900000000     C***
063000000000     C* TERMINAL DUE (LINEA)
063100000000     C***
063200000000     C     D             OCCUR     LDEC
063300000000IF  3C     *IN98         IFEQ      *OFF
063400000000     C                   ADD       WFRGPN        LGPN(M)                        *GG PARTENZE NORMALI
063500000000     C                   ADD       WFRMAN        LMAN(M)                        *MC ASSEGNATI NORMAL
063600000000     C                   ADD       WFRMAB        LMAB(M)                        *MC ASSEGNATI BIS
063700000000     C                   ADD       WFRMAS        LMAS(M)                        *MC ASSEGNATI SEDE
063800000000     C                   ADD       DEPCTT        LCTT(M)                        *COSTO TN TOTALE
063900000000     C                   ADD       DEPCST        LCST(M)                        *COSTO TN SEDE
064000000000E   3C                   ENDIF
064100000000     C                   ADD       DEPITR        LITR(M)                        *TN PER MCUBO
064200000000     C***
064300000000     C* TERMINAL UNO
064400000000     C***
064500960614     C* SE LOCALE NON MEMORIZZA I DATI NEL TOTALE
064600960614IF  3C     DEPTF1        IFNE      DEPTF2                                       *NON LOCALE
064700000000     C     D             OCCUR     TDEC
064800960614IF  4C     *IN98         IFEQ      *OFF
064900000000     C                   ADD       WFRGPN        TGPN(M)                        *GG PARTENZE NORMALI
065000000000     C                   ADD       WFRMAN        TMAN(M)                        *MC ASSEGNATI NORMAL
065100000000     C                   ADD       WFRMAB        TMAB(M)                        *MC ASSEGNATI BIS
065200000000     C                   ADD       WFRMAS        TMAS(M)                        *MC ASSEGNATI SEDE
065300000000     C                   ADD       DEPCTT        TCTT(M)                        *COSTO TN TOTALE
065400000000     C                   ADD       DEPCST        TCST(M)                        *COSTO TN SEDE
065500960614E   4C                   ENDIF
065600000000     C                   ADD       DEPITR        TITR(M)                        *TN PER MCUBO
065700960614E   3C                   ENDIF
065800980224     C***
065900980224     C* DIVISIONE BARTOLINI
066000980224     C***
066100980224IF  3C     DEPDI1        IFEQ      'BAR'
066200001214     C     PARDAT        ORGE      200101                                       *X FUSIONE
066300980224     C     D             OCCUR     BDEC
066400980224IF  4C     *IN98         IFEQ      *OFF
066500980224     C                   ADD       WFRGPN        BGPN(M)                        *GG PARTENZE NORMALI
066600980224     C                   ADD       WFRMAN        BMAN(M)                        *MC ASSEGNATI NORMAL
066700980224     C                   ADD       WFRMAB        BMAB(M)                        *MC ASSEGNATI BIS
066800980224     C                   ADD       WFRMAS        BMAS(M)                        *MC ASSEGNATI SEDE
066900980224     C                   ADD       DEPCTT        BCTT(M)                        *COSTO TN TOTALE
067000980224     C                   ADD       DEPCST        BCST(M)                        *COSTO TN SEDE
067100980224E   4C                   ENDIF
067200980224     C                   ADD       DEPITR        BITR(M)                        *TN PER MCUBO
067300980224     C***
067400980224     C* DIVISIONE S.D.I.
067500980224     C***
067600980224X   3C                   ELSE
067700980224     C     D             OCCUR     SDEC
067800980224IF  4C     *IN98         IFEQ      *OFF
067900980224     C                   ADD       WFRGPN        SGPN(M)                        *GG PARTENZE NORMALI
068000980224     C                   ADD       WFRMAN        SMAN(M)                        *MC ASSEGNATI NORMAL
068100980224     C                   ADD       WFRMAB        SMAB(M)                        *MC ASSEGNATI BIS
068200980224     C                   ADD       WFRMAS        SMAS(M)                        *MC ASSEGNATI SEDE
068300980224     C                   ADD       DEPCTT        SCTT(M)                        *COSTO TN TOTALE
068400980224     C                   ADD       DEPCST        SCST(M)                        *COSTO TN SEDE
068500980224E   4C                   ENDIF
068600980224     C                   ADD       DEPITR        SITR(M)                        *TN PER MCUBO
068700980224E   3C                   ENDIF
068800000000     C***
068900000000     C* GENERALE
069000000000     C***
069100000000     C     D             OCCUR     GDEC
069200000000IF  3C     *IN98         IFEQ      *OFF
069300000000     C                   ADD       WFRGPN        GGPN(M)                        *GG PARTENZE NORMALI
069400000000     C                   ADD       WFRMAN        GMAN(M)                        *MC ASSEGNATI NORMAL
069500000000     C                   ADD       WFRMAB        GMAB(M)                        *MC ASSEGNATI BIS
069600000000     C                   ADD       WFRMAS        GMAS(M)                        *MC ASSEGNATI SEDE
069700000000     C                   ADD       DEPCTT        GCTT(M)                        *COSTO TN TOTALE
069800000000     C                   ADD       DEPCST        GCST(M)                        *COSTO TN SEDE
069900000000E   3C                   ENDIF
070000000000     C                   ADD       DEPITR        GITR(M)                        *TN PER MCUBO
070100000000E   2C                   ENDDO
070200000000E   1C                   ENDIF
070300000000     C*
070400000000     C                   ENDSR
070500000000     C*------------------------------------------------------------------------*
070600000000     C* SCRFPC - SCRIVE IL FILE PC
070700000000     C*------------------------------------------------------------------------*
070800000000     C     SCRFPC        BEGSR
070900000000     C*
071000000000     C* PULISCE LE SCHIERE DI LAVORO
071100000000     C                   CLEAR                   $GPN
071200000000     C                   CLEAR                   $MAN
071300000000     C                   CLEAR                   $MAB
071400000000     C                   CLEAR                   $MAS
071500000000     C                   CLEAR                   $MPF
071600000000     C                   CLEAR                   $MPO
071700000000     C                   CLEAR                   $ITR
071800000000     C                   CLEAR                   $CTL
071900000000     C                   CLEAR                   $SPF
072000000000     C                   CLEAR                   $QLF
072100000000     C                   CLEAR                   $SPO
072200000000     C                   CLEAR                   $QLO
072300000000     C                   CLEAR                   $CTT
072400000000     C                   CLEAR                   $CST
072500000000     C*
072600000000     C* PER CIASCUN MESE MEMORIZZATO
072700000000DO  1C     1             DO        DEPM          M
072800000000     C*
072900000000     C* SOMMA I DATI DELLE TRE DECADI DEL MESE
073000000000DO  2C     1             DO        3             D
073100000000     C     D             OCCUR     LDEC
073200000000     C                   ADD       LGPN(M)       $GPN(M)
073300000000     C                   ADD       LMAN(M)       $MAN(M)
073400000000     C                   ADD       LMAB(M)       $MAB(M)
073500000000     C                   ADD       LMAS(M)       $MAS(M)
073600000000     C                   ADD       LMPF(M)       $MPF(M)
073700000000     C                   ADD       LMPO(M)       $MPO(M)
073800000000IF  3C     LITR(M)       IFGT      *ZEROS
073900000000     C                   Z-ADD     LITR(M)       $ITR(M)                        SEMPRE E SOLO MENSIL
074000000000E   3C                   ENDIF
074100000000     C                   ADD       LCTL(M)       $CTL(M)
074200000000     C                   ADD       LSPF(M)       $SPF(M)
074300000000     C                   ADD       LQLF(M)       $QLF(M)
074400000000     C                   ADD       LSPO(M)       $SPO(M)
074500000000     C                   ADD       LQLO(M)       $QLO(M)
074600000000     C                   ADD       LCTT(M)       $CTT(M)
074700000000     C                   ADD       LCST(M)       $CST(M)
074800000000E   2C                   ENDDO
074900000000     C*
075000000000     C* LEGGE IL RECORD DEL FILE PC
075100000000     C                   MOVEL     PARMPA        KPCMPA                         *PARTENZA/ARRIVO
075200000000     C                   Z-ADD     PARAAI        KPCANN                         *ANNO INIZIALE
075300000000     C                   Z-ADD     M             KPCMES                         *MESE
075400000000     C                   Z-ADD     DEPTF1        KPCTFP                         *FILIALE UNO
075500000000     C                   Z-ADD     DEPTF2        KPCTFA                         *FILIALE DUE
075600011019     C     KEYPCT        CHAIN     TIPCT01L                           98
075700000000     C* AGGIORNAMENTO
075800000000IF  2C     *IN98         IFEQ      *OFF
075900000000     C                   ADD       $GPN(M)       PCTGPN
076000000000     C                   ADD       $MAN(M)       PCTMAN
076100000000     C                   ADD       $MAB(M)       PCTMAB
076200000000     C                   ADD       $MAS(M)       PCTMAS
076300000000     C                   ADD       $MPF(M)       PCTMPF
076400000000     C                   ADD       $MPO(M)       PCTMPO
076500011019IF  3C                   IF        $DIVSTT = 'ITL'                              *IN LIRE
076600011019     C     $ITR(M)       DIV(H)    1000          $ITR(M)                        *IN MIGLIAIA
076700011019E   3C                   ENDIF
076800000000     C                   ADD       $ITR(M)       PCTITR
076900011019IF  3C                   IF        $DIVSTT = 'ITL'                              *IN LIRE
077000011019     C     $CTT(M)       DIV(H)    1000          $CTT(M)                        *IN MIGLIAIA
077100011019E   3C                   ENDIF
077200000000     C                   ADD       $CTT(M)       PCTCTT
077300011019IF  3C                   IF        $DIVSTT = 'ITL'                              *IN LIRE
077400011019     C     $CTL(M)       DIV(H)    1000          $CTL(M)                        *IN MIGLIAIA
077500011019E   3C                   ENDIF
077600000000     C                   ADD       $CTL(M)       PCTCTL
077700000000     C                   ADD       $SPF(M)       PCTSPF
077800000000     C     $QLF(M)       DIV       100           $QLF(M)                        *KG --> QL
077900000000     C                   ADD       $QLF(M)       PCTQLF
078000000000     C                   ADD       $SPO(M)       PCTSPO
078100000000     C     $QLO(M)       DIV       100           $QLO(M)                        *KG --> QL
078200000000     C                   ADD       $QLO(M)       PCTQLO
078300011019     C                   UPDATE    TIPCT000
078400000000     C* INSERIMENTO
078500000000X   2C                   ELSE
078600000000     C                   MOVEL     KPCMPA        PCTMPA                         *PARTENZA/ARRIVO
078700000000     C                   Z-ADD     KPCANN        PCTANN                         *ANNO INIZIALE
078800000000     C                   Z-ADD     KPCMES        PCTMES                         *MESE
078900000000     C                   Z-ADD     KPCTFP        PCTTFP                         *FILIALE UNO
079000000000     C                   Z-ADD     KPCTFA        PCTTFA                         *FILIALE DUE
079100000000     C                   Z-ADD     $GPN(M)       PCTGPN
079200000000     C                   Z-ADD     $MAN(M)       PCTMAN
079300000000     C                   Z-ADD     $MAB(M)       PCTMAB
079400000000     C                   Z-ADD     $MAS(M)       PCTMAS
079500000000     C                   Z-ADD     $MPF(M)       PCTMPF
079600000000     C                   Z-ADD     $MPO(M)       PCTMPO
079700011019IF  3C                   IF        $DIVSTT = 'ITL'                              *IN LIRE
079800011019     C     $ITR(M)       DIV(H)    1000          $ITR(M)                        *IN MIGLIAIA
079900011019E   3C                   ENDIF
080000000000     C                   Z-ADD     $ITR(M)       PCTITR
080100011019IF  3C                   IF        $DIVSTT = 'ITL'                              *IN LIRE
080200011019     C     $CTT(M)       DIV(H)    1000          $CTT(M)                        *IN MIGLIAIA
080300011019E   3C                   ENDIF
080400000000     C                   Z-ADD     $CTT(M)       PCTCTT
080500011019IF  3C                   IF        $DIVSTT = 'ITL'                              *IN LIRE
080600011019     C     $CTL(M)       DIV(H)    1000          $CTL(M)                        *IN MIGLIAIA
080700011019E   3C                   ENDIF
080800000000     C                   Z-ADD     $CTL(M)       PCTCTL
080900000000     C                   Z-ADD     $SPF(M)       PCTSPF
081000000000     C     $QLF(M)       DIV       100           $QLF(M)                        *KG --> QL
081100000000     C                   Z-ADD     $QLF(M)       PCTQLF
081200000000     C                   Z-ADD     $SPO(M)       PCTSPO
081300000000     C     $QLO(M)       DIV       100           $QLO(M)                        *KG --> QL
081400000000     C                   Z-ADD     $QLO(M)       PCTQLO
081500011019     C                   WRITE     TIPCT000
081600000000E   2C                   ENDIF
081700000000E   1C                   ENDDO                                                  *FINE MESI
081800000000     C*
081900000000     C                   ENDSR
082000000000     C*------------------------------------------------------------------------*
082100000000     C* NEWTF1 - OPERAZIONI PER NUOVO TERMINAL UNO
082200000000     C*------------------------------------------------------------------------*
082300000000     C     NEWTF1        BEGSR
082400000000     C*
082500000000     C* REIMPOSTA IL FLAG DI PASSAGGIO MEMORIZZAZIONE PER TERMINAL UNO
082600000000     C                   MOVEL     '0'           PASTF1
082700000000     C*
082800000000     C* MEMORIZZA IL NUOVO TERMINAL UNO IN UN DEPOSITO
082900000000IF  1C     PARMPA        IFEQ      'P'                                          P A R T E N Z A
083000000000     C                   Z-ADD     SDTTFP        DEPTF1                         *FILIALE PARTENZA
083100000000X   1C                   ELSE                                                   A R R I V O
083200000000     C                   Z-ADD     SDTTFA        DEPTF1                         *FILIALE ARRIVO
083300000000E   1C                   ENDIF
083400000000     C*
083500000000     C* DECODIFICA IL TERMINAL UNO
083600000000     C                   MOVEL     *BLANKS       DECTF1           20
083700000000     C                   Z-ADD     1             I
083800000000     C                   SETOFF                                           28
083900000000     C     DEPTF1        LOOKUP    ORG(I)                                 28
084000000000IF  1C     *IN28         IFEQ      *ON
084100980224     C                   MOVEL     ODES(I)       DECTF1                         *DECODIFICA
084200980224     C                   MOVEL     ODIT(I)       DEPDI1                         *DITTA
084300000000E   1C                   ENDIF
084400000000     C*
084500000000     C* PULISCE I TOTALI PER TERMINAL UNO
084600000000DO  1C     1             DO        3             I
084700000000     C     I             OCCUR     TDEC
084800000000     C                   CLEAR                   TGPN
084900000000     C                   CLEAR                   TMAN
085000000000     C                   CLEAR                   TMAB
085100000000     C                   CLEAR                   TMAS
085200000000     C                   CLEAR                   TMPF
085300000000     C                   CLEAR                   TMPO
085400000000     C                   CLEAR                   TITR
085500000000     C                   CLEAR                   TCTL
085600000000     C                   CLEAR                   TSPF
085700000000     C                   CLEAR                   TQLF
085800000000     C                   CLEAR                   TSPO
085900000000     C                   CLEAR                   TQLO
086000000000     C                   CLEAR                   TCTT
086100000000     C                   CLEAR                   TCST
086200000000E   1C                   ENDDO
086300000000     C*
086400000000     C                   ENDSR
086500000000     C*------------------------------------------------------------------------*
086600000000     C* NEWTF2 - OPERAZIONI PER NUOVO TERMINAL DUE (LINEA)
086700000000     C*------------------------------------------------------------------------*
086800000000     C     NEWTF2        BEGSR
086900000000     C*
087000000000     C* REIMPOSTA IL FLAG DI PASSAGGIO MEMORIZZAZIONE PER TERMINAL DUE
087100000000     C                   MOVEL     '0'           PASTF2
087200000000     C*
087300000000     C* MEMORIZZA IL NUOVO TERMINAL DUE IN UN DEPOSITO
087400000000IF  1C     PARMPA        IFEQ      'P'                                          P A R T E N Z A
087500000000     C                   Z-ADD     SDTTFA        DEPTF2                         *FILIALE ARRIVO
087600000000X   1C                   ELSE                                                   A R R I V O
087700000000     C                   Z-ADD     SDTTFP        DEPTF2                         *FILIALE PARTENZA
087800000000E   1C                   ENDIF
087900000000     C*
088000000000     C* DECODIFICA IL TERMINAL DUE
088100000000     C                   MOVEL     *BLANKS       DECTF2           20
088200000000     C                   Z-ADD     1             I
088300000000     C                   SETOFF                                           28
088400000000     C     DEPTF2        LOOKUP    ORG(I)                                 28
088500000000IF  1C     *IN28         IFEQ      *ON
088600980224     C                   MOVEL     ODES(I)       DECTF2
088700980224     C                   MOVEL     ODIT(I)       DEPDI2
088800000000E   1C                   ENDIF
088900000000     C*
089000000000     C* PULISCE I TOTALI PER TERMINAL DUE (LINEA)
089100000000DO  1C     1             DO        3             I
089200000000     C     I             OCCUR     LDEC
089300000000     C                   CLEAR                   LGPN
089400000000     C                   CLEAR                   LMAN
089500000000     C                   CLEAR                   LMAB
089600000000     C                   CLEAR                   LMAS
089700000000     C                   CLEAR                   LMPF
089800000000     C                   CLEAR                   LMPO
089900000000     C                   CLEAR                   LITR
090000000000     C                   CLEAR                   LCTL
090100000000     C                   CLEAR                   LSPF
090200000000     C                   CLEAR                   LQLF
090300000000     C                   CLEAR                   LSPO
090400000000     C                   CLEAR                   LQLO
090500000000     C                   CLEAR                   LCTT
090600000000     C                   CLEAR                   LCST
090700000000E   1C                   ENDDO
090800000000     C*
090900000000     C                   ENDSR
091000000000     C*------------------------------------------------------------------------*
091100000000     C* NEWMES - OPERAZIONI PER NUOVO MESE
091200000000     C*------------------------------------------------------------------------*
091300000000     C     NEWMES        BEGSR
091400000000     C*
091500000000     C* MEMORIZZA LA NUOVA DATA IN UN DEPOSITO
091600000000     C                   Z-ADD     SDTDAT        DEPDAT
091700000000     C*
091800000000     C* IMPOSTA LA LINE TN PER REPERIRE LA TARIFFA (RICAPOVOLGE PARTENZA-ARRIVO)
091900000000IF  1C     PARMPA        IFEQ      'P'
092000000000     C                   Z-ADD     DEPTF1        DEPFT1                         *PARTENZA
092100000000     C                   Z-ADD     DEPTF2        DEPFT2                         *ARRIVO
092200000000X   1C                   ELSE
092300000000     C                   Z-ADD     DEPTF2        DEPFT1                         *PARTENZA
092400000000     C                   Z-ADD     DEPTF1        DEPFT2                         *ARRIVO
092500000000E   1C                   ENDIF
092600980415     C*
092700980415     C* CALCOLA IL CODICE VOCE DELLA TARIFFA TRAINI LINEA DA UTILIZZARE
092800001213IF  1C     SDTDAT        IFGE      200101                                       *DAL  01/2001
092900001213     C                   Z-ADD     212           KVDVOC                         *VOCE UNICA X FUSION
093000001213X   1C                   ELSE                                                   *PRIMA DEL 01/2001
093100980415IF  2C     DEPDI1        IFEQ      'BAR'                                        *TERM PARTENZA BAR
093200980415     C                   Z-ADD     210           KVDVOC                         *VOCE BAR
093300980415X   2C                   ELSE                                                   *TERM PARTENZA SDI
093400980415     C                   Z-ADD     211           KVDVOC                         *VOCE SDI
093500980415E   2C                   ENDIF
093600980415E   1C                   ENDIF
093700980415     C*
093800980415     C* CERCA LA TARIFFA TRAINI LINEA DEL MESE E LA DEPOSITA
093900090924     C                   Z-ADD     *ZEROS        DEPITR           13 5
094000140114     C     KEYEVD        SETLL     ECEVD06L
094100140114     C     KVDVOC        READE     ECEVD06L                               98
094200000000DO  1C     *IN98         DOWEQ     *OFF
094300000000IF  2C     EVDATB        IFEQ      *BLANKS                                      *TARIFFA VALIDA
094400000000     C     EVDTTV        ANDEQ     'F'                                          *TARIFFA DI FILIALE
094500000000     C     EVDFT1        ANDEQ     DEPFT1                                       *PARTENZA
094600000000     C     EVDFT2        ANDEQ     DEPFT2                                       *ARRIVO
094700000000     C     DEPDAT        ANDGE     EVDDEC                                       *IN DECORRENZA
094800000000     C     DEPDAT        ANDLE     EVDSCA
094900011018     C*---               Z-ADD     EVDITR        DEPITR                         *TN PER MC
095000011018     C*
095100011018     C* SE LA DIVISA DI OUT DELLA STATISTICA E' DIVERSA DELLA DIVISA DELLE ANAGRAFICHE DEL C/E,
095200011018     C* CONVERTE GLI IMPORTI NELLA DIVISTA DI OUTPUT DELLA STATISTICA -SECONDO LA DATA CORRENTE-
095300011018     C                   EXSR      CNVEVD
095400011018     C                   LEAVE
095500011018E   2C                   ENDIF
095600140114     C     KVDVOC        READE     ECEVD06L                               98
095700000000E   1C                   ENDDO
095800000000     C*
095900000000     C                   ENDSR
096000011018     C*------------------------------------------------------------------------*
096100011018     C* CNVEVD - CONVERTE IMPORTI DI ECEVD
096200011018     C*------------------------------------------------------------------------*
096300011018     C     CNVEVD        BEGSR
096400011018     C*
096500011018IF  1C     $DIVSTT       IFNE      $DIVAC                                       *DIVISE <>
096600011018     C                   MOVEL     $DIVAC        WDVI              3            *DIVISA  INPUT
096700011018     C                   Z-ADD     EVDITR        WIMI             30 9          *IMPORTO INPUT
096800011018     C                   EXSR      CNVEUR
096900011018     C                   Z-ADD     WIMO          DEPITR                         *TN PER MC
097000011018X   1C                   ELSE                                                   *DIVISE =
097100011018     C                   Z-ADD     EVDITR        DEPITR                         *TN PER MC
097200011018E   1C                   ENDIF
097300011018     C*
097400011018     C                   ENDSR
097500000000     C*------------------------------------------------------------------------*
097600000000     C* NEWDEC - OPERAZIONI PER NUOVA DECADE
097700000000     C*------------------------------------------------------------------------*
097800000000     C     NEWDEC        BEGSR
097900000000     C*
098000000000     C* MEMORIZZA LA NUOVA DECADE IN UN DEPOSITO
098100000000     C                   MOVEL     SDTDEC        DEPDEC
098200000000     C*
098300000000     C                   ENDSR
098400000000     C*------------------------------------------------------------------------*
098500000000     C* STATF2 - STAMPA TOTALI PER TERMINAL DUE (LINEA)
098600000000     C*------------------------------------------------------------------------*
098700000000     C     STATF2        BEGSR
098800000000     C*
098900000000     C* STAMPA LA PAGINA SOLO SE HA MEMORIZZATO QC
099000000000IF  1C     PASTF2        IFEQ      '1'
099100000000     C*
099200000000     C* IMPOSTA L'INTESTAZIONE
099300000000     C                   MOVEL(P)  DECTF1        PIDTF1                         *DESCR TERMINAL UNO
099400000000     C                   MOVEL(P)  DECTF2        PIDTF2                         *DESCR TERMINAL DUE
099500000000     C*
099600000000     C* SCHIERE DI MEMORIZZAZIONE --> SCHIERE DI STAMPA
099700000000DO  2C     1             DO        3             I
099800000000     C     I             OCCUR     LDEC
099900980224     C     I             OCCUR     XDEC
100000980224     C                   Z-ADD     LGPN          XGPN
100100980224     C                   Z-ADD     LMAN          XMAN
100200980224     C                   Z-ADD     LMAB          XMAB
100300980224     C                   Z-ADD     LMAS          XMAS
100400980224     C                   Z-ADD     LMPF          XMPF
100500980224     C                   Z-ADD     LMPO          XMPO
100600980224     C                   Z-ADD     LITR          XITR
100700980224     C                   Z-ADD     LCTL          XCTL
100800980224     C                   Z-ADD     LSPF          XSPF
100900980224     C                   Z-ADD     LQLF          XQLF
101000980224     C                   Z-ADD     LSPO          XSPO
101100980224     C                   Z-ADD     LQLO          XQLO
101200980224     C                   Z-ADD     LCTT          XCTT
101300980224     C                   Z-ADD     LCST          XCST
101400000000E   2C                   ENDDO
101500000000     C*
101600000000     C* CONTEGGIA QUESTO NEL NUMERO DEI BLOCCHI STAMPATI (MAX 2 PER PAG)
101700000000     C                   ADD       1             CONBLO
101800000000     C* STAMPA TOTALI
101900000000     C                   EXSR      STAMPA
102000000000E   1C                   ENDIF
102100000000     C*
102200000000     C                   ENDSR
102300000000     C*------------------------------------------------------------------------*
102400000000     C* STATF1 - STAMPA TOTALI PER TERMINAL UNO
102500000000     C*------------------------------------------------------------------------*
102600000000     C     STATF1        BEGSR
102700000000     C*
102800000000     C* STAMPA LA PAGINA SOLO SE HA MEMORIZZATO QC
102900000000IF  1C     PASTF1        IFEQ      '1'
103000000000     C*
103100000000     C* IMPOSTA L'INTESTAZIONE
103200000000     C                   MOVEL     DRI(26)       PIDTF1                         *DESCR "FILIALE"
103300000000     C                   MOVEL(P)  DECTF1        PIDTF2                         *DESCR TERMINAL UNO
103400000000     C*
103500000000     C* SCHIERE DI MEMORIZZAZIONE --> SCHIERE DI STAMPA
103600000000DO  2C     1             DO        3             I
103700000000     C     I             OCCUR     TDEC
103800980224     C     I             OCCUR     XDEC
103900980224     C*---                 Z-ADDTGPN      XGPN
104000980224     C                   Z-ADD     *ZEROS        XGPN
104100980224     C                   Z-ADD     TMAN          XMAN
104200980224     C                   Z-ADD     TMAB          XMAB
104300980224     C                   Z-ADD     TMAS          XMAS
104400980224     C                   Z-ADD     TMPF          XMPF
104500980224     C                   Z-ADD     TMPO          XMPO
104600980224     C                   Z-ADD     TITR          XITR
104700980224     C                   Z-ADD     TCTL          XCTL
104800980224     C                   Z-ADD     TSPF          XSPF
104900980224     C                   Z-ADD     TQLF          XQLF
105000980224     C                   Z-ADD     TSPO          XSPO
105100980224     C                   Z-ADD     TQLO          XQLO
105200980224     C                   Z-ADD     TCTT          XCTT
105300980224     C                   Z-ADD     TCST          XCST
105400000000E   2C                   ENDDO
105500000000     C* PAGINA NUOVA
105600000000     C                   MOVEL     '1'           *IN01                          *OVERFLOW
105700000000     C* STAMPA TOTALI
105800000000     C                   EXSR      STAMPA
105900000000E   1C                   ENDIF
106000000000     C* PAGINA NUOVA
106100000000     C                   MOVEL     '1'           *IN01                          *OVERFLOW
106200000000     C*
106300000000     C                   ENDSR
106400980224     C*------------------------------------------------------------------------*
106500980224     C* STABAR - STAMPA TOTALI PER DIVISIONE BARTOLINI
106600980224     C*------------------------------------------------------------------------*
106700980224     C     STABAR        BEGSR
106800980224     C*
106900980224     C* STAMPA LA PAGINA SOLO SE HA MEMORIZZATO QC
107000980224IF  1C     PASDIB        IFEQ      '1'
107100980224     C*
107200980224     C* IMPOSTA L'INTESTAZIONE
107300980224     C                   MOVEL(P)  DRI(27)       PIDTF1                         *DESCR "TOTALE"
107400980224     C                   MOVEL(P)  DRI(29)       PIDTF2                         *DESCR "BARTOLINI"
107500980224     C                   SETON                                        01        *OVERFLOW
107600980224     C*
107700980224     C* SCHIERE DI MEMORIZZAZIONE --> SCHIERE DI STAMPA
107800980224DO  2C     1             DO        3             I
107900980224     C     I             OCCUR     BDEC
108000980224     C     I             OCCUR     XDEC
108100980224     C*---                 Z-ADDBGPN      XGPN
108200980224     C                   Z-ADD     *ZEROS        XGPN
108300980224     C                   Z-ADD     BMAN          XMAN
108400980224     C                   Z-ADD     BMAB          XMAB
108500980224     C                   Z-ADD     BMAS          XMAS
108600980224     C                   Z-ADD     BMPF          XMPF
108700980224     C                   Z-ADD     BMPO          XMPO
108800980224     C                   Z-ADD     BITR          XITR
108900980224     C                   Z-ADD     BCTL          XCTL
109000980224     C                   Z-ADD     BSPF          XSPF
109100980224     C                   Z-ADD     BQLF          XQLF
109200980224     C                   Z-ADD     BSPO          XSPO
109300980224     C                   Z-ADD     BQLO          XQLO
109400980224     C                   Z-ADD     BCTT          XCTT
109500980224     C                   Z-ADD     BCST          XCST
109600980224E   2C                   ENDDO
109700980224     C* PAGINA NUOVA
109800980224     C                   MOVEL     '1'           *IN01                          *OVERFLOW
109900980224     C* STAMPA TOTALI
110000980224     C                   EXSR      STAMPA
110100980224E   1C                   ENDIF
110200980224     C*
110300980224     C                   ENDSR
110400980224     C*------------------------------------------------------------------------*
110500980224     C* STASDI - STAMPA TOTALI PER DIVISIONE SDI
110600980224     C*------------------------------------------------------------------------*
110700980224     C     STASDI        BEGSR
110800980224     C*
110900980224     C* STAMPA LA PAGINA SOLO SE HA MEMORIZZATO QC
111000980224IF  1C     PASDIS        IFEQ      '1'
111100980224     C*
111200980224     C* IMPOSTA L'INTESTAZIONE
111300980224     C                   MOVEL(P)  DRI(27)       PIDTF1                         *DESCR "TOTALE"
111400980224     C                   MOVEL(P)  DRI(30)       PIDTF2                         *DESCR "S.D.I."
111500980224     C                   SETON                                        01        *OVERFLOW
111600980224     C*
111700980224     C* SCHIERE DI MEMORIZZAZIONE --> SCHIERE DI STAMPA
111800980224DO  2C     1             DO        3             I
111900980224     C     I             OCCUR     SDEC
112000980224     C     I             OCCUR     XDEC
112100980224     C*---                 Z-ADDSGPN      XGPN
112200980224     C                   Z-ADD     *ZEROS        XGPN
112300980224     C                   Z-ADD     SMAN          XMAN
112400980224     C                   Z-ADD     SMAB          XMAB
112500980224     C                   Z-ADD     SMAS          XMAS
112600980224     C                   Z-ADD     SMPF          XMPF
112700980224     C                   Z-ADD     SMPO          XMPO
112800980224     C                   Z-ADD     SITR          XITR
112900980224     C                   Z-ADD     SCTL          XCTL
113000980224     C                   Z-ADD     SSPF          XSPF
113100980224     C                   Z-ADD     SQLF          XQLF
113200980224     C                   Z-ADD     SSPO          XSPO
113300980224     C                   Z-ADD     SQLO          XQLO
113400980224     C                   Z-ADD     SCTT          XCTT
113500980224     C                   Z-ADD     SCST          XCST
113600980224E   2C                   ENDDO
113700980224     C* PAGINA NUOVA
113800980224     C                   MOVEL     '1'           *IN01                          *OVERFLOW
113900980224     C* STAMPA TOTALI
114000980224     C                   EXSR      STAMPA
114100980224E   1C                   ENDIF
114200980224     C*
114300980224     C                   ENDSR
114400000000     C*------------------------------------------------------------------------*
114500000000     C* STAGEN - STAMPA TOTALI GENERALI
114600000000     C*------------------------------------------------------------------------*
114700000000     C     STAGEN        BEGSR
114800000000     C*
114900000000     C* STAMPA LA PAGINA SOLO SE HA MEMORIZZATO QC
115000000000IF  1C     PASGEN        IFEQ      '1'
115100000000     C*
115200000000     C* IMPOSTA L'INTESTAZIONE
115300000000     C                   MOVEL(P)  DRI(27)       PIDTF1                         *DESCR "TOTALE"
115400000000     C                   MOVEL(P)  DRI(28)       PIDTF2                         *DESCR "GENERALE"
115500000000     C                   SETON                                        01        *OVERFLOW
115600000000     C*
115700000000     C* SCHIERE DI MEMORIZZAZIONE --> SCHIERE DI STAMPA
115800000000DO  2C     1             DO        3             I
115900000000     C     I             OCCUR     GDEC
116000980224     C     I             OCCUR     XDEC
116100980224     C*---                 Z-ADDGGPN      XGPN
116200980224     C                   Z-ADD     *ZEROS        XGPN
116300980224     C                   Z-ADD     GMAN          XMAN
116400980224     C                   Z-ADD     GMAB          XMAB
116500980224     C                   Z-ADD     GMAS          XMAS
116600980224     C                   Z-ADD     GMPF          XMPF
116700980224     C                   Z-ADD     GMPO          XMPO
116800980224     C                   Z-ADD     GITR          XITR
116900980224     C                   Z-ADD     GCTL          XCTL
117000980224     C                   Z-ADD     GSPF          XSPF
117100980224     C                   Z-ADD     GQLF          XQLF
117200980224     C                   Z-ADD     GSPO          XSPO
117300980224     C                   Z-ADD     GQLO          XQLO
117400980224     C                   Z-ADD     GCTT          XCTT
117500980224     C                   Z-ADD     GCST          XCST
117600000000E   2C                   ENDDO
117700000000     C* PAGINA NUOVA
117800000000     C                   MOVEL     '1'           *IN01                          *OVERFLOW
117900000000     C* STAMPA TOTALI
118000000000     C                   EXSR      STAMPA
118100000000E   1C                   ENDIF
118200000000     C*
118300000000     C                   ENDSR
118400000000     C*------------------------------------------------------------------------*
118500000000     C* STAMPA - STAMPA TOTALI
118600000000     C*------------------------------------------------------------------------*
118700000000     C     STAMPA        BEGSR
118800000000     C*
118900000000     C* PULISCE LE SCHIERE DI LAVORO
119000000000     C                   CLEAR                   $GPN
119100000000     C                   CLEAR                   $MAN
119200000000     C                   CLEAR                   $MAB
119300000000     C                   CLEAR                   $MAS
119400000000     C                   CLEAR                   $MPF
119500000000     C                   CLEAR                   $MPO
119600000000     C                   CLEAR                   $ITR
119700000000     C                   CLEAR                   $CTL
119800000000     C                   CLEAR                   $SPF
119900000000     C                   CLEAR                   $QLF
120000000000     C                   CLEAR                   $SPO
120100000000     C                   CLEAR                   $QLO
120200000000     C                   CLEAR                   $CTT
120300000000     C                   CLEAR                   $CST
120400000000     C*
120500000000     C* PULISCE LE SCHIERE DI PASSAGGIO
120600000000     C                   CLEAR                   PV1
120700000000     C                   CLEAR                   PP1
120800000000     C*--------------------
120900000000     C* RAGGRUPPA I DATI PER MESE
121000000000     C*--------------------
121100000000     C* PER CIASCUN MESE MEMORIZZATO SOMMA I DATI DELLE TRE DECADI
121200000000DO  1C     1             DO        DEPM          M
121300000000DO  2C     1             DO        3             D
121400980224     C     D             OCCUR     XDEC
121500980224     C                   ADD       XGPN(M)       $GPN(M)
121600980224     C                   ADD       XMAN(M)       $MAN(M)
121700980224     C                   ADD       XMAB(M)       $MAB(M)
121800980224     C                   ADD       XMAS(M)       $MAS(M)
121900980224     C                   ADD       XMPF(M)       $MPF(M)
122000980224     C                   ADD       XMPO(M)       $MPO(M)
122100980224IF  3C     XITR(M)       IFGT      *ZEROS
122200980224     C                   Z-ADD     XITR(M)       $ITR(M)                        SEMPRE E SOLO MENSIL
122300000000E   3C                   ENDIF
122400980224     C                   ADD       XCTL(M)       $CTL(M)
122500980224     C                   ADD       XSPF(M)       $SPF(M)
122600980224     C                   ADD       XQLF(M)       $QLF(M)
122700980224     C                   ADD       XSPO(M)       $SPO(M)
122800980224     C                   ADD       XQLO(M)       $QLO(M)
122900980224     C                   ADD       XCTT(M)       $CTT(M)
123000980224     C                   ADD       XCST(M)       $CST(M)
123100000000     C*
123200000000     C* TIENE L'ULTIMO MESE CON LE DECADI SEPARATE
123300000000IF  3C     M             IFEQ      DEPM
123400000000     C                   Z-ADD     *ZEROS        I
123500000000     C     12            ADD       D             I
123600980224     C                   ADD       XGPN(M)       $GPN(I)
123700980224     C                   ADD       XMAN(M)       $MAN(I)
123800980224     C                   ADD       XMAB(M)       $MAB(I)
123900980224     C                   ADD       XMAS(M)       $MAS(I)
124000980224     C                   ADD       XMPF(M)       $MPF(I)
124100980224     C                   ADD       XMPO(M)       $MPO(I)
124200980224IF  4C     XITR(M)       IFGT      *ZEROS
124300980224     C                   Z-ADD     XITR(M)       $ITR(I)                        SEMPRE E SOLO MENSIL
124400000000E   4C                   ENDIF
124500980224     C                   ADD       XCTL(M)       $CTL(I)
124600980224     C                   ADD       XSPF(M)       $SPF(I)
124700980224     C                   ADD       XQLF(M)       $QLF(I)
124800980224     C                   ADD       XSPO(M)       $SPO(I)
124900980224     C                   ADD       XQLO(M)       $QLO(I)
125000980224     C                   ADD       XCTT(M)       $CTT(I)
125100980224     C                   ADD       XCST(M)       $CST(I)
125200000000E   3C                   ENDIF
125300000000E   2C                   ENDDO                                                  *FINE DECADI
125400000000E   1C                   ENDDO                                                  *FINE MESI
125500000000     C* TOTALE MESI
125600000000     C                   Z-ADD     16            I                              *ALLA 16^ COLONNA
125700000000DO  1C     1             DO        DEPM          M
125800000000     C                   ADD       $GPN(M)       $GPN(I)
125900000000     C                   ADD       $MAN(M)       $MAN(I)
126000000000     C                   ADD       $MAB(M)       $MAB(I)
126100000000     C                   ADD       $MAS(M)       $MAS(I)
126200000000     C                   ADD       $MPF(M)       $MPF(I)
126300000000     C                   ADD       $MPO(M)       $MPO(I)
126400000000     C                   ADD       $ITR(M)       $ITR(I)
126500000000     C                   ADD       $CTL(M)       $CTL(I)
126600000000     C                   ADD       $SPF(M)       $SPF(I)
126700000000     C                   ADD       $QLF(M)       $QLF(I)
126800000000     C                   ADD       $SPO(M)       $SPO(I)
126900000000     C                   ADD       $QLO(M)       $QLO(I)
127000000000     C                   ADD       $CTT(M)       $CTT(I)
127100000000     C                   ADD       $CST(M)       $CST(I)
127200000000E   1C                   ENDDO
127300000000     C*--------------------
127400000000     C* STAMPA TESTATA
127500000000     C*--------------------
127600000000IF  1C     CONBLO        IFGT      2                                            *MAX 2 BLOCCHI X PAG
127700000000     C     *IN01         OREQ      '1'                                          *OVERFLOW
127800000000     C                   WRITE     SS76PT                                       *TESTATA
127900000000     C                   Z-ADD     1             CONBLO                         *AZZERA IL CONTATAOR
128000000000     C                   MOVEL     '0'           *IN01                          *REIMPOSTA OVERFLOW
128100000000E   1C                   ENDIF
128200000000     C*--------------------
128300000000     C* STAMPA RIGA
128400000000     C*--------------------
128500000000     C                   WRITE     SS76PI                                       *INTESTAZIONE
128600000000     C***
128700000000     C* GIORNI PARTENZE NORMALI
128800000000     C***
128900000000     C                   Z-ADD     $GPN          PV1
129000000000     C                   MOVEL     DRI(1)        WDES             30
129100000000     C                   EXSR      STAVAL
129200000000     C***
129300000000     C* MC ASSEGNATI NORMALI
129400000000     C***
129500000000     C                   Z-ADD     $MAN          PV1
129600000000     C                   MOVEL     DRI(2)        WDES
129700000000     C                   EXSR      STAVAL
129800000000     C***
129900000000     C* MC ASSEGNATI BIS
130000000000     C***
130100000000     C                   Z-ADD     $MAB          PV1
130200000000     C                   MOVEL     DRI(3)        WDES
130300000000     C                   EXSR      STAVAL
130400000000     C***
130500000000     C* MC ASSEGNATI SEDE
130600000000     C***
130700000000     C                   Z-ADD     $MAS          PV1
130800000000     C                   MOVEL     DRI(4)        WDES
130900000000     C                   EXSR      STAVAL
131000000000     C***
131100000000     C* MC ASS NORMALI / GIORNO
131200000000     C***
131300000000     C                   CLEAR                   PV1
131400000000DO  1C     1             DO        16            M
131500000000IF  2C     $GPN(M)       IFNE      *ZEROS
131600000000     C     $MAN(M)       DIV(H)    $GPN(M)       PV1(M)
131700000000E   2C                   ENDIF
131800000000E   1C                   ENDDO
131900000000     C                   MOVEL     DRI(5)        WDES
132000000000     C                   EXSR      STAVAL
132100000000     C***
132200000000     C* MC PARTITI
132300000000     C***
132400990217     C* FINO E OLTRE 50QL
132500000000     C     $MPF          ADD       $MPO          PV1
132600000000     C                   MOVEL     DRI(6)        WDES
132700000000     C                   EXSR      STAVAL
132800000000     C***
132900000000     C* MC PARTITI / GIORNO
133000000000     C***
133100990217     C* FINO E OLTRE 50QL
133200000000     C                   CLEAR                   PV1
133300000000DO  1C     1             DO        16            M
133400000000IF  2C     $GPN(M)       IFNE      *ZEROS
133500981005     C     $MPF(M)       ADD       $MPO(M)       PV1(M)
133600981005     C     PV1(M)        DIV(H)    $GPN(M)       PV1(M)
133700000000E   2C                   ENDIF
133800000000E   1C                   ENDDO
133900000000     C                   MOVEL     DRI(7)        WDES
134000000000     C                   EXSR      STAVAL
134100000000     C***
134200000000     C* DELTA MC PARTITI / ASSEGNATI (MC)
134300000000     C***
134400990217     C* PARTITI:   FINO E OLTRE 50QL
134500000000     C* ASSEGNATI: NORMALI E BIS
134600000000     C     $MPF          ADD       $MPO          PV1
134700000000     C     PV1           SUB       $MAN          PV1
134800000000     C     PV1           SUB       $MAB          PV1
134900000000     C                   MOVEL     DRI(8)        WDES
135000000000     C                   EXSR      STAVAL
135100000000     C***
135200000000     C* DELTA MC PARTITI / ASSEGNATI (%)
135300000000     C***
135400000000     C* 100 : X = MC ASSEGNATI : (MC PARTITI - MC ASSEGNATI)
135500000000     C* 100 * (MC PARTITI - MC ASSEGNATI) / MC ASSEGNATI
135600000000     C                   CLEAR                   PP1
135700000000DO  1C     1             DO        16            M
135800000000     C     $MPF(M)       ADD       $MPO(M)       WDIVID           18 3
135900000000     C     WDIVID        SUB       $MAN(M)       WDIVID           18 3
136000000000     C     WDIVID        SUB       $MAB(M)       WDIVID           18 3
136100000000     C     $MAN(M)       ADD       $MAB(M)       WDIVIS           18 3
136200000000     C                   Z-ADD     *ZEROS        WPERC             6 3
136300000000     C                   EXSR      CALPER
136400000000     C                   Z-ADD     WPERC         PP1(M)
136500000000E   1C                   ENDDO
136600000000     C                   MOVEL     DRI(9)        WDES
136700000000     C                   EXSR      STAPER
136800000000     C***
136900000000     C* TN PER MC
137000000000     C***
137100011019IF  1C                   IF        $DIVSTT = 'ITL'                              *IN LIRE
137200000000     C     $ITR          DIV(H)    1000          PV1                            *IN MIGLIAIA
137300011019X   1C                   ELSE                                                   *IN EURO
137400011019     C                   Z-ADD     $ITR          PV1                            *TRONCATO
137500011019E   1C                   ENDIF
137600000000     C                   MOVEL     DRI(10)       WDES
137700000000     C                   EXSR      STAVAL
137800000000     C***
137900000000     C* COSTO TN TOTALE
138000000000     C***
138100011019IF  1C                   IF        $DIVSTT = 'ITL'                              *IN LIRE
138200000000     C     $CTT          DIV(H)    1000          PV1                            *IN MIGLIAIA
138300011019X   1C                   ELSE                                                   *IN EURO
138400011019     C                   Z-ADD     $CTT          PV1                            *TRONCATO
138500011019E   1C                   ENDIF
138600000000     C                   MOVEL     DRI(11)       WDES
138700000000     C                   EXSR      STAVAL
138800000000     C***
138900000000     C* RICAVO TN TOTALE
139000000000     C***
139100011019IF  1C                   IF        $DIVSTT = 'ITL'                              *IN LIRE
139200000000     C     $CTL          DIV(H)    1000          PV1                            *IN MIGLIAIA
139300011019X   1C                   ELSE                                                   *IN EURO
139400011019     C                   Z-ADD     $CTL          PV1                            *TRONCATO
139500011019E   1C                   ENDIF
139600000000     C                   MOVEL     DRI(12)       WDES
139700000000     C                   EXSR      STAVAL
139800000000     C***
139900000000     C* DELTA RICAVO TN / COSTO TN (£.)
140000000000     C***
140100000000     C     $CTL          SUB       $CTT          PV1
140200011019IF  1C                   IF        $DIVSTT = 'ITL'                              *IN LIRE
140300000000     C     PV1           DIV(H)    1000          PV1                            *IN MIGLIAIA
140400011019E   1C                   ENDIF
140500000000     C                   MOVEL     DRI(13)       WDES
140600000000     C                   EXSR      STAVAL
140700000000     C***
140800000000     C* DELTA RICAVO TN / COSTO TN (%)
140900000000     C***
141000000000     C* 100 : X = COSTO TN : (RICAVO TN - COSTO TN)
141100000000     C* 100 * (RICAVO TN - COSTO TN) / COSTO TN
141200000000     C                   CLEAR                   PP1
141300000000DO  1C     1             DO        16            M
141400000000     C                   Z-ADD     $CTT(M)       WDIVIS           18 3
141500000000     C     $CTL(M)       SUB       $CTT(M)       WDIVID           18 3
141600000000     C                   Z-ADD     *ZEROS        WPERC             6 3
141700000000     C                   EXSR      CALPER
141800000000     C                   Z-ADD     WPERC         PP1(M)
141900000000E   1C                   ENDDO
142000000000     C                   MOVEL     DRI(14)       WDES
142100000000     C                   EXSR      STAPER
142200000000     C***
142300000000     C* COSTO TN SEDE
142400000000     C***
142500011019IF  1C                   IF        $DIVSTT = 'ITL'                              *IN LIRE
142600000000     C     $CST          DIV(H)    1000          PV1                            *IN MIGLIAIA
142700011019X   1C                   ELSE                                                   *IN EURO
142800011019     C                   Z-ADD     $CST          PV1                            *TRONCATO
142900011019E   1C                   ENDIF
143000000000     C                   MOVEL     DRI(15)       WDES
143100000000     C                   EXSR      STAVAL
143200000000     C***
143300000000     C* (RICAVO TN - COSTO TN) - COSTO TN SEDE
143400000000     C***
143500000000DO  1C     1             DO        16            M
143600000000     C     $CTL(M)       SUB       $CTT(M)       PV1(M)                         *RICAVO TN-COSTO TN
143700000000IF  2C     PV1(M)        IFGT      *ZEROS                                       *SE RICAVO-COSTO > 0
143800000000     C     PV1(M)        SUB       $CST(M)       PV1(M)                         *(RICAVO-COSTO)-SEDE
143900000000IF  3C     PV1(M)        IFLT      *ZEROS                                       *SE < 0 IMPOSTA = 0
144000000000     C                   Z-ADD     *ZEROS        PV1(M)
144100000000E   3C                   ENDIF
144200000000X   2C                   ELSE
144300000000     C                   Z-ADD     *ZEROS        PV1(M)                         *SE <> 0 IMPOSTA = 0
144400000000E   2C                   ENDIF
144500000000E   1C                   ENDDO
144600011019IF  1C                   IF        $DIVSTT = 'ITL'                              *IN LIRE
144700000000     C     PV1           DIV(H)    1000          PV1                            *IN MIGLIAIA
144800011019E   1C                   ENDIF
144900000000     C                   MOVEL     DRI(16)       WDES
145000000000     C                   EXSR      STAVAL
145100000000     C***
145200990217     C* NUMERO SPED FINO 50QL
145300000000     C***
145400000000     C                   Z-ADD     $SPF          PV1
145500000000     C                   MOVEL     DRI(17)       WDES
145600000000     C                   EXSR      STAVAL
145700000000     C***
145800990217     C* QUINTALI SPED FINO 50QL
145900000000     C***
146000000000     C     $QLF          DIV       100           PV1                            *IN QUINTALI
146100000000     C                   MOVEL     DRI(18)       WDES
146200000000     C                   EXSR      STAVAL
146300000000     C***
146400990217     C* NUMERO SPED OLTRE 50QL
146500000000     C***
146600000000     C                   Z-ADD     $SPO          PV1
146700000000     C                   MOVEL     DRI(19)       WDES
146800000000     C                   EXSR      STAVAL
146900000000     C***
147000990217     C* QUINTALI SPED OLTRE 50QL
147100000000     C***
147200000000     C     $QLO          DIV       100           PV1                            *IN QUINTALI
147300000000     C                   MOVEL     DRI(20)       WDES
147400000000     C                   EXSR      STAVAL
147500000000     C***
147600990217     C* MCUBI SPED OLTRE 50QL
147700000000     C***
147800000000     C                   Z-ADD     $MPO          PV1
147900000000     C                   MOVEL     DRI(21)       WDES
148000000000     C                   EXSR      STAVAL
148100000000     C*--------------------
148200000000     C* R I G A  DI CHIUSURA
148300000000     C*--------------------
148400000000     C                   WRITE     SS76RI
148500000000     C*
148600000000     C                   ENDSR
148700000000     C*------------------------------------------------------------------------*
148800000000     C* STAVAL - STAMPA UNA RIGA DI VALORE
148900000000     C*------------------------------------------------------------------------*
149000000000     C     STAVAL        BEGSR
149100000000     C*
149200000000     C                   CLEAR                   SS76P1                         *PULISCE RIGA
149300000000     C                   MOVEL     WDES          P1DDES                         *DESCRIZIONE VALORE
149400000000     C                   Z-ADD     PV1(1)        P1CVA1                         *MESE UNO
149500000000     C                   Z-ADD     PV1(2)        P1CVA2                         *MESE DUE
149600000000     C                   Z-ADD     PV1(3)        P1CVA3                         *MESE TRE
149700000000     C                   Z-ADD     PV1(4)        P1CVA4                         *MESE QUATTRO
149800000000     C                   Z-ADD     PV1(5)        P1CVA5                         *MESE CINQUE
149900000000     C                   Z-ADD     PV1(6)        P1CVA6                         *MESE SEI
150000000000     C                   Z-ADD     PV1(7)        P1CVA7                         *MESE SETTE
150100000000     C                   Z-ADD     PV1(8)        P1CVA8                         *MESE OTTO
150200000000     C                   Z-ADD     PV1(9)        P1CVA9                         *MESE NOVE
150300000000     C                   Z-ADD     PV1(10)       P1CVA0                         *MESE DIECI
150400000000     C                   Z-ADD     PV1(11)       P1CVAA                         *MESE UNDICI
150500000000     C                   Z-ADD     PV1(12)       P1CVAB                         *MESE DODICI
150600000000     C                   Z-ADD     PV1(13)       P1CDE1                         *1^DECADE
150700000000     C                   Z-ADD     PV1(14)       P1CDE2                         *2^DECADE
150800000000     C                   Z-ADD     PV1(15)       P1CDE3                         *3^DECADE
150900000000     C                   Z-ADD     PV1(16)       P1CVAT                         *TOTALE 12 MESI
151000000000     C*
151100000000     C                   WRITE     SS76P1                                       *STAMPA RIGA
151200000000     C*
151300000000     C                   ENDSR
151400000000     C*------------------------------------------------------------------------*
151500000000     C* STAPER - STAMPA UNA RIGA DI PERCENTUALE
151600000000     C*------------------------------------------------------------------------*
151700000000     C     STAPER        BEGSR
151800000000     C*
151900000000     C                   CLEAR                   SS76P2                         *PULISCE RIGA
152000000000     C                   MOVEL     WDES          P2DDES                         *DESCRIZ.PERCENTUALE
152100000000     C                   Z-ADD     PP1(1)        P2CVA1                         *MESE UNO
152200000000     C                   Z-ADD     PP1(2)        P2CVA2                         *MESE DUE
152300000000     C                   Z-ADD     PP1(3)        P2CVA3                         *MESE TRE
152400000000     C                   Z-ADD     PP1(4)        P2CVA4                         *MESE QUATTRO
152500000000     C                   Z-ADD     PP1(5)        P2CVA5                         *MESE CINQUE
152600000000     C                   Z-ADD     PP1(6)        P2CVA6                         *MESE SEI
152700000000     C                   Z-ADD     PP1(7)        P2CVA7                         *MESE SETTE
152800000000     C                   Z-ADD     PP1(8)        P2CVA8                         *MESE OTTO
152900000000     C                   Z-ADD     PP1(9)        P2CVA9                         *MESE NOVE
153000000000     C                   Z-ADD     PP1(10)       P2CVA0                         *MESE DIECI
153100000000     C                   Z-ADD     PP1(11)       P2CVAA                         *MESE UNDICI
153200000000     C                   Z-ADD     PP1(12)       P2CVAB                         *MESE DODICI
153300000000     C                   Z-ADD     PP1(13)       P2CDE1                         *1^DECADE
153400000000     C                   Z-ADD     PP1(14)       P2CDE2                         *2^DECADE
153500000000     C                   Z-ADD     PP1(15)       P2CDE3                         *3^DECADE
153600000000     C                   Z-ADD     PP1(16)       P2CVAT                         *TOTALE 12 MESI
153700000000     C*
153800000000     C                   WRITE     SS76P2                                       *STAMPA RIGA
153900000000     C*
154000000000     C                   ENDSR
154100000000     C*------------------------------------------------------------------------*
154200000000     C* CALPER - CALCOLA LA PERCENTUALE
154300000000     C*          INPUT  -> WDIVID = DIVIDENDO / WDIVIS = DIVISORE
154400000000     C*          OUTPUT -> WPERC  = PERCENTUALE
154500000000     C*------------------------------------------------------------------------*
154600000000     C     CALPER        BEGSR
154700000000     C*
154800000000     C*   DIVIDENDO *  100
154900000000     C*  ------------------ =  PERCENTUALE
155000000000     C*        DIVISORE
155100000000     C***
155200000000     C* A) IL DIVIDENDO CHE IL DIVISORE SONO = ZERO;
155300000000     C*    PERCENTUALE = 0
155400000000     C* B) IL DIVIDENDO E' = ZERO MENTRE IL DIVISORE NO;
155500000000     C*    PERCENTUALE = -999,9 SE DIVISIORE < 0 ; +999,9 SE DIVISIORE > 0
155600000000     C* C) IL DIVISORE E' = ZERO MENTRE IL DIVIDENDO NO;
155700000000     C*    PERCENTUALE = -999,9 SE DIVIDENDO < 0 ; +999,9 SE DIVIDENDO > 0
155800000000     C* D) IL DIVIDENDO E IL DIVISORE SONO > ZERO;
155900000000     C*    PERCENTUALE = +/-XXX,X
156000000000     C***
156100000000IF  1C     WDIVID        IFEQ      *ZEROS
156200000000IF  2C     WDIVIS        IFEQ      *ZEROS
156300000000     C                   Z-ADD     *ZEROS        WPERC                          *CASO A
156400000000X   2C                   ELSE
156500000000IF  3C     WDIVIS        IFLT      *ZEROS
156600000000     C                   Z-ADD     -999,9        WPERC                          *CASO B
156700000000X   3C                   ELSE
156800000000     C                   Z-ADD     +999,9        WPERC                          *CASO B
156900000000E   3C                   ENDIF
157000000000E   2C                   ENDIF
157100000000X   1C                   ELSE
157200000000IF  2C     WDIVIS        IFEQ      *ZEROS
157300000000IF  3C     WDIVID        IFLT      *ZEROS
157400000000     C                   Z-ADD     -999,9        WPERC                          *CASO C
157500000000X   3C                   ELSE
157600000000     C                   Z-ADD     +999,9        WPERC                          *CASO C
157700000000E   3C                   ENDIF
157800000000X   2C                   ELSE
157900000000     C     WDIVID        MULT      100           WNUM19           19 3          *CASO D
158000000000     C     WNUM19        DIV       WDIVIS        WNUM19
158100000000     C*
158200000000     C* SE PERCENTUALE = +/-1000 METTE +/-999,9
158300000000     C                   SETOFF                                           28
158400000000     C                   Z-ADD     WNUM19        WPERC
158500000000     C     WNUM19        COMP      0                                    28
158600000000IF  3C     *IN28         IFEQ      *OFF                                         *POSITIVO
158700000000IF  4C     WNUM19        IFGE      1000
158800000000     C                   Z-ADD     +999,9        WPERC
158900000000E   4C                   ENDIF
159000000000X   3C                   ELSE                                                   *NEGATIVO
159100000000IF  4C     WNUM19        IFLE      -1000
159200000000     C                   Z-ADD     -999,9        WPERC
159300000000E   4C                   ENDIF
159400000000E   3C                   ENDIF
159500000000E   2C                   ENDIF
159600000000E   1C                   ENDIF
159700000000     C*
159800000000     C                   ENDSR
159900011011     C*------------------------------------------------------------------------*
160000011018     C* CONVERTE I CAMPI NELLA DIVISA DI OUTPUT DELLA STATISTICA TRAINI
160100011011     C*------------------------------------------------------------------------*
160200011011     C     CNVEUR        BEGSR
160300011011     C*
160400011011     C* AZZERA VARIABILI DI LAVORO
160500011018     C                   Z-ADD     *ZEROS        WIMO             30 9          *IMPORTO OUTPUT
160600011018     C                   MOVEL     *BLANKS       NDEC              1            *N° DEC DI OUTPUT
160700011011     C*
160800011011     C* IMPOSTO I DECIMALI DI OUTPUT A SECONDA DELLA DIVISA DI OUTPUT
160900011018     C                   IF        $DIVSTT = 'ITL'
161000011011     C                   EVAL      NDEC = '0'
161100011011     C                   ELSE
161200011018     C                   EVAL      NDEC = '3'
161300011011     C                   ENDIF
161400011011     C*
161500011011IF  1C     WIMI          IFNE      *ZEROS
161600011011     C                   CLEAR                   YEUR
161700011018     C                   MOVEL     WDVI          YECDVI                         *DIVISA INPUT
161800011018     C                   Z-ADD     WIMI          YECIMI                         *IMPORTO INPUT
161900011018     C                   MOVEL     $DIVSTT       YECDVO                         *DIVISA OUTPUT
162000011011     C                   MOVEL     NDEC          YECDCO                         *N° DECIMALI
162100011011     C                   MOVEL     *BLANKS       YECTAR                         *ARROTONDAMENTO "H"
162200011011     C                   CALL      'YEURCO'
162300011011     C                   PARM                    YEUR
162400011011IF  2C     YECESI        IFEQ      ' '                                          *NO ERRORI
162500011011     C                   Z-ADD     YECIMO        WIMO                            -IMPORTO CONVERTITO
162600011011X   2C                   ELSE                                                   *SI ERRORI
162700011011     C                   Z-ADD     WIMI          WIMO                            -IMPOROTO DA CONVER
162800011011E   2C                   ENDIF
162900011011E   1C                   ENDIF
163000011011     C*
163100011011     C                   ENDSR
163200000000     C*------------------------------------------------------------------------*
163300000000     C* STAPAR - STAMPA LA PAGINA DEI PARAMETRI
163400000000     C*------------------------------------------------------------------------*
163500000000     C     STAPAR        BEGSR
163600000000     C*
163700000000IF  1C     PARCRE        IFEQ      'S'
163800000000     C                   MOVEL     'SI'          PACCRE
163900000000X   1C                   ELSE
164000000000     C                   MOVEL     'NO'          PACCRE
164100000000E   1C                   ENDIF
164200000000     C                   Z-ADD     PARDAT        PACDAT                         *ANNO/MESE INIZIALE
164300000000     C                   Z-ADD     PARDA2        PACDA2                         *ANNO/MESE FINALE
164400000000     C                   MOVEL     *BLANKS       PACMPA                         *TIPO STATISTICA
164500000000IF  1C     PARMPA        IFEQ      'P'
164600000000     C                   MOVEL     DTE(1)        PACMPA
164700000000E   1C                   ENDIF
164800000000IF  1C     PARMPA        IFEQ      'A'
164900000000     C                   MOVEL     DTE(2)        PACMPA
165000000000E   1C                   ENDIF
165100000000IF  1C     PARFPC        IFEQ      'S'
165200000000     C                   MOVEL     'SI'          PACFPC
165300000000X   1C                   ELSE
165400000000     C                   MOVEL     'NO'          PACFPC
165500000000E   1C                   ENDIF
165600000706IF  1C     PARPOS        IFEQ      'S'
165700000706     C                   MOVEL     'SI'          PACPOS
165800000706X   1C                   ELSE
165900000706     C                   MOVEL     'NO'          PACPOS
166000000706E   1C                   ENDIF
166100000000     C*
166200000000     C                   WRITE     SS76PT                                       *INTESTAZIONE
166300000000     C                   WRITE     SS76PA                                       *PARAMETRI
166400000000     C*
166500000000     C                   ENDSR
166600000000     C*------------------------------------------------------------------------*
166700000000     C* CLOFIL - CHIUSURA FILES UTILIZZATI
166800000000     C*------------------------------------------------------------------------*
166900000000     C     CLOFIL        BEGSR
167000000000     C***
167100000000     C* FILE DATABASE
167200000000     C***
167300000000IF  1C     PARMPA        IFEQ      'P'                                          *PARTENZA
167400990609     C                   CLOSE     SISDT04L                                     S A L D I
167500000000     C                   CLOSE     WFTRA01L                                     W O R K F I L E
167600000000E   1C                   ENDIF
167700000000IF  1C     PARMPA        IFEQ      'A'                                          *ARRIVO
167800990609     C                   CLOSE     SISDT05L                                     S A L D I
167900000000     C                   CLOSE     WFTRA02L                                     W O R K F I L E
168000000000E   1C                   ENDIF
168100000000     C***
168200000000     C* FILE PC
168300000000     C***
168400000000IF  1C     PARFPC        IFEQ      'S'
168500011019     C                   CLOSE     TIPCT01L
168600000000E   1C                   ENDIF
168700000000     C*
168800000000     C                   ENDSR
168900000000     C*------------------------------------------------------------------------*
169000000000     C* OPEFIL - OPERAZIONI INIZIALI SUI FILES DA UTILIZZARE
169100000000     C*------------------------------------------------------------------------*
169200000000     C     OPEFIL        BEGSR
169300000000     C***
169400000000     C* FILE DATABASE
169500000000     C***
169600000000IF  1C     PARMPA        IFEQ      'P'                                          *PARTENZA
169700990609     C                   OPEN      SISDT04L                                     S A L D I
169800000000     C                   OPEN      WFTRA01L                                     W O R K F I L E
169900000000E   1C                   ENDIF
170000000000IF  1C     PARMPA        IFEQ      'A'                                          *ARRIVO
170100990609     C                   OPEN      SISDT05L                                     S A L D I
170200000000     C                   OPEN      WFTRA02L                                     W O R K F I L E
170300000000E   1C                   ENDIF
170400000000     C***
170500000000     C* FILE PC
170600000000     C***
170700000000IF  1C     PARFPC        IFEQ      'S'
170800011019     C                   OPEN      TIPCT01L                                     *APERTURA FILE
170900000000     C                   MOVEL     PARMPA        KPCMPA                         *PARTENZA O ARRIVO
171000000000     C                   Z-ADD     PARAAI        KPCANN                         *ANNO INIZIALE
171100000000     C                   Z-ADD     PARMMI        KPCMES                         *MESE INIZIALE
171200000000     C                   Z-ADD     *ZEROS        KPCTFP                         *FILIALE UNO
171300000000     C                   Z-ADD     *ZEROS        KPCTFA                         *FILIALE DUE
171400011019     C     KEYPCT        SETLL     TIPCT01L
171500011019     C     KPCMPA        READE     TIPCT01L                               98
171600000000DO  2C     *IN98         DOWEQ     *OFF
171700000706     C*
171800000706     C* RECUPERA FLAG POSTA
171900000706     C                   Z-ADD     1             I
172000000706     C                   SETOFF                                           28
172100000706     C     PCTTFP        LOOKUP    ORG(I)                                 28
172200000706     C                   MOVEL     OPOS(I)       WPPOS
172300000706     C                   Z-ADD     1             I
172400000706     C                   SETOFF                                           28
172500000706     C     PCTTFA        LOOKUP    ORG(I)                                 28
172600000706     C                   MOVEL     OPOS(I)       WAPOS
172700000706     C*
172800000706     C* CANCELLA I DATI DEL PERIODO RICHIESTO
172900000000IF  3C     PCTDAT        IFGE      PARDAT
173000000000     C     PCTDAT        ANDLE     PARDA2
173100000706IF  4C     PARPOS        IFEQ      'N'                                          *NO DATI POSTE
173200020627     C     WPPOS         ANDNE     'PPT'                                         TFP <> POSTE e
173300000706     C     WAPOS         ANDNE     'S'                                           TFA <> POSTE
173400000706     C     PARPOS        OREQ      'S'                                          *SI DATI POSTE
173500020627     C     WPPOS         ANDEQ     'PPT'                                         TFP = POSTE o
173600000706     C     PARPOS        OREQ      'S'
173700000706     C     WAPOS         ANDEQ     'S'                                           TFA = POSTE
173800011019     C                   DELETE    TIPCT000
173900000706E   4C                   ENDIF
174000000706E   3C                   ENDIF
174100011019     C     KPCMPA        READE     TIPCT01L                               98
174200000000E   2C                   ENDDO
174300000000E   1C                   ENDIF
174400000000     C*
174500000000     C                   ENDSR
174600000000     C*------------------------------------------------------------------------*
174700000000     C* CARTAB - CARICA LE TABELLE
174800000000     C*------------------------------------------------------------------------*
174900000000     C     CARTAB        BEGSR
175000000000     C*--------------------
175100000000     C* FILIALI
175200000000     C*--------------------
175300000000     C                   Z-ADD     *ZEROS        I                 4 0
175400000000     C                   READ      AZORG01L                               99
175500000000DO  1C     *IN99         DOWEQ     *OFF
175600000000IF  2C     ORGFVA        IFEQ      *BLANKS
175700000706     C                   MOVEL     ORGDE3        OG143
175800000000     C                   ADD       1             I
175900980224     C                   Z-ADD     ORGFIL        ORG(I)                         *CODICE
176000980224     C                   MOVEL     ORGDES        ODES(I)                        *DESCRIZIONE
176100980224     C                   MOVEL     ORGDIT        ODIT(I)                        *DITTA
176200020627     C                   MOVEL     §OGNTW        OPOS(I)                        *FLAG POSTE
176300000000E   2C                   ENDIF
176400000000     C                   READ      AZORG01L                               99
176500000000E   1C                   ENDDO
176600000000     C*--------------------
176700000000     C* FASCE DI PESO
176800000000     C*--------------------
176900990608IF  1C     PARDA2        IFLE      199812                                       *FASCE FINO AL '98
177000990217     C                   MOVEL     'SDT'         WSX               3
177100020328X   1C                   ELSE                                                   *FASCE DAL '99
177200020326     C                   MOVEL     'S9T'         WSX
177300020326E   1C                   ENDIF
177400000000     C                   Z-ADD     *ZEROS        I
177500020906     C                   Z-ADD     1             KTBKUT
177600000000     C                   MOVEL     '2L'          KTBCOD
177700000000     C     KE2TAB        SETLL     TABEL00F
177800000000     C     KE2TAB        READE     TABEL00F                               99
177900000000DO  1C     *IN99         DOWEQ     *OFF
178000000000     C                   MOVEL     TBLKEY        WALF3             3
178100990217IF  2C     WALF3         IFEQ      WSX
178200000000     C                   ADD       1             I
178300000000     C                   MOVEL     TBLUNI        DS2L
178400000000     C                   MOVEL     §2LDFP        FPE(I)                         *DESCRIZIONE
178500000000E   2C                   ENDIF
178600000000     C     KE2TAB        READE     TABEL00F                               99
178700000000E   1C                   ENDDO
178800011011     C*--------------------
178900011015     C* DIVISE UTILI
179000011011     C*--------------------
179100011018     C* ANAGRAFICHE (INP)
179200011018     C                   CLEAR                   DGEDAC
179300011019     C                   MOVEL(P)  'GED'         KBECOD
179400011019     C                   MOVEL(P)  'AC'          KBEKE1
179500011018     C     KEYTBE        CHAIN     TNTBE01L
179600011018IF  1C                   IF        %FOUND(TNTBE01L)
179700011018     C                   MOVEL     TBEUNI        DGEDAC
179800011018E   1C                   ENDIF
179900011018IF  1C                   IF        §GEDACDAT >= DATCOR                          *D.RIF su D.LANCIO
180000011018     C                   EVAL      $DIVAC = §GEDACDIP                           *DIVISA PRIMA (ITL)
180100011018X   1C                   ELSE
180200011018     C                   EVAL      $DIVAC = §GEDACDID                           *DIVISA DOPO (EUR)
180300011018E   1C                   ENDIF
180400011018     C* SALDI (INP)
180500011221     C                   CLEAR                   DSGEDCE
180600011019     C                   MOVEL(P)  'GED'         KBECOD
180700011221     C                   MOVEL(P)  'CE'          KBEKE1
180800011018     C     KEYTBE        CHAIN     TNTBE01L
180900011018IF  1C                   IF        %FOUND(TNTBE01L)
181000011221     C                   MOVEL     TBEUNI        DSGEDCE
181100011018E   1C                   ENDIF
181200011221IF  1C                   IF        §GEDCEDAT >= DATCOR                          *D.RIF su D.LANCIO
181300011221     C                   EVAL      $DIVCE = §GEDCEDIP                           *DIVISA PRIMA (ITL)
181400011018X   1C                   ELSE
181500011221     C                   EVAL      $DIVCE = §GEDCEDIV                           *DIVISA DOPO (EUR)
181600011018E   1C                   ENDIF
181700011018     C* STATISTICHE (OUT)
181800011018     C                   CLEAR                   DGEDST
181900011019     C                   MOVEL(P)  'GED'         KBECOD
182000011019     C                   MOVEL(P)  'ST'          KBEKE1
182100011018     C     KEYTBE        CHAIN     TNTBE01L
182200011018IF  1C                   IF        %FOUND(TNTBE01L)
182300011018     C                   MOVEL     TBEUNI        DGEDST
182400011018E   1C                   ENDIF
182500011018IF  1C                   IF        §GEDSTDATT >= DATCOR                         *D.RIF su D.LANCIO
182600011018     C                   EVAL      $DIVSTT = §GEDSTDIPT                         *DIVISA PRIMA (ITL)
182700011018X   1C                   ELSE
182800011018     C                   EVAL      $DIVSTT = §GEDSTDIDT                         *DIVISA DOPO (EUR)
182900011018E   1C                   ENDIF
183000000000     C*
183100000000     C                   ENDSR
183200000000     C*------------------------------------------------------------------------*
183300000000     C* CARMES - CARICA I MESI DA ELEBORARE
183400000000     C*------------------------------------------------------------------------*
183500000000     C     CARMES        BEGSR
183600000000     C*
183700000000     C                   CLEAR                   ANN                            *ANNI
183800000000     C                   CLEAR                   MES                            *MESI
183900000000     C                   Z-ADD     *ZEROS        J                 4 0          *N° MESI DA ELABORAR
184000990608     C                   Z-ADD     PARAAF        WAA2              4 0          *ANNO FINALE LAVORO
184100000000     C                   Z-ADD     PARMMF        WMM2              2 0          *MESE FINALE LAVORO
184200990608     C                   Z-ADD     PARAAI        WAA               4 0          *ANNO INIZIALE LAVOR
184300000000     C                   Z-ADD     PARMMI        WMM               2 0          *MESE INIZIALE LAVOR
184400000000     C                   Z-ADD     *ZEROS        DEPM              4 0          *MESI SELEZIONATI
184500000000     C***
184600000000     C* ANNO INIZIALE UGUALE AD ANNO FINALE
184700000000     C***
184800000000     C* CALCOLA I MESI DI DIFFERENZA E LASCIA IL RESTO DELLA SCHIERA A ZERO
184900000000IF  1C     WAA2          IFEQ      WAA
185000000000DO  2C     WMM           DOWLE     WMM2
185100000000     C                   ADD       1             J
185200000000     C                   Z-ADD     WAA           ANN(J)
185300000000     C                   Z-ADD     WMM           MES(J)
185400000000     C                   ADD       1             WMM
185500000000E   2C                   ENDDO
185600000000     C                   Z-ADD     J             DEPM                           *MESI SELEZIONATI
185700000000DO  2C     J             DOWLT     12
185800000000     C                   ADD       1             J
185900000000     C                   Z-ADD     *ZEROS        ANN(J)
186000000000     C                   Z-ADD     *ZEROS        MES(J)
186100000000E   2C                   ENDDO
186200000000     C***
186300000000     C* ANNO FINALE DIVERSO DA ANNO INIZIALE
186400000000     C***
186500000000     C* MEMORIZZA I MESI DELL'ANNO INIZIALE E FINALE E IL RESTO A ZERO
186600000000X   1C                   ELSE
186700000000DO  2C     WMM           DOWLE     12
186800000000     C                   ADD       1             J
186900000000     C                   Z-ADD     WAA           ANN(J)
187000000000     C                   Z-ADD     WMM           MES(J)
187100000000     C                   ADD       1             WMM
187200000000E   2C                   ENDDO
187300000000     C                   Z-ADD     01            WMM
187400000000DO  2C     WMM           DOWLE     WMM2
187500000000     C                   ADD       1             J
187600000000     C                   Z-ADD     WAA2          ANN(J)
187700000000     C                   Z-ADD     WMM           MES(J)
187800000000     C                   ADD       1             WMM
187900000000E   2C                   ENDDO
188000000000     C                   Z-ADD     J             DEPM                           *MESI SELEZIONATI
188100000000DO  2C     J             DOWLT     12
188200000000     C                   ADD       1             J
188300000000     C                   Z-ADD     *ZEROS        ANN(J)
188400000000     C                   Z-ADD     *ZEROS        MES(J)
188500000000E   2C                   ENDDO
188600000000E   1C                   ENDIF
188700000000     C*
188800000000     C                   ENDSR
188900000000     C*------------------------------------------------------------------------*
189000000000     C* DECINT - DECODIFICA LA TESTATA E L'INTESTAZIONE
189100000000     C*------------------------------------------------------------------------*
189200000000     C     DECINT        BEGSR
189300000000     C*--------------------
189400000000     C* TESTATA
189500000000     C*--------------------
189600000000     C                   MOVEL     *BLANKS       PTCMPA
189700000000IF  1C     PARMPA        IFEQ      'P'
189800000000     C                   MOVEL     DTE(1)        PTCMPA
189900000000E   1C                   ENDIF
190000000000IF  1C     PARMPA        IFEQ      'A'
190100000000     C                   MOVEL     DTE(2)        PTCMPA
190200000000E   1C                   ENDIF
190300011019     C                   MOVEL     $DIVSTT       PTCDIV
190400000000     C*--------------------
190500000000     C* INTESTAZIONE
190600000000     C*--------------------
190700000000     C***
190800000000     C* DECODIFICA MESI
190900000000     C***
191000000000IF  1C     MES(1)        IFGT      *ZEROS
191100000000     C                   MOVEL     MES(1)        IN                2 0          *NUMERO MESE MEMORIZ
191200000000     C                   MOVEL     DMM(IN)       PIDME1                         *DESC MESE IN STAMPA
191300990608     C                   MOVE      ANN(1)        PICAN1                         *NUM. ANNO IN STAMPA
191400000000E   1C                   ENDIF
191500000000     C*
191600000000IF  1C     MES(2)        IFGT      *ZEROS
191700000000     C                   MOVEL     MES(2)        IN                             *NUMERO MESE MEMORIZ
191800000000     C                   MOVEL     DMM(IN)       PIDME2                         *DESC MESE IN STAMPA
191900990608     C                   MOVE      ANN(2)        PICAN2                         *NUM. ANNO IN STAMPA
192000000000E   1C                   ENDIF
192100000000     C*
192200000000IF  1C     MES(3)        IFGT      *ZEROS
192300000000     C                   MOVEL     MES(3)        IN                             *NUMERO MESE MEMORIZ
192400000000     C                   MOVEL     DMM(IN)       PIDME3                         *DESC MESE IN STAMPA
192500990608     C                   MOVE      ANN(3)        PICAN3                         *NUM. ANNO IN STAMPA
192600000000E   1C                   ENDIF
192700000000     C*
192800000000IF  1C     MES(4)        IFGT      *ZEROS
192900000000     C                   MOVEL     MES(4)        IN                             *NUMERO MESE MEMORIZ
193000000000     C                   MOVEL     DMM(IN)       PIDME4                         *DESC MESE IN STAMPA
193100990608     C                   MOVE      ANN(4)        PICAN4                         *NUM. ANNO IN STAMPA
193200000000E   1C                   ENDIF
193300000000     C*
193400000000IF  1C     MES(5)        IFGT      *ZEROS
193500000000     C                   MOVEL     MES(5)        IN                             *NUMERO MESE MEMORIZ
193600000000     C                   MOVEL     DMM(IN)       PIDME5                         *DESC MESE IN STAMPA
193700990608     C                   MOVE      ANN(5)        PICAN5                         *NUM. ANNO IN STAMPA
193800000000E   1C                   ENDIF
193900000000     C*
194000000000IF  1C     MES(6)        IFGT      *ZEROS
194100000000     C                   MOVEL     MES(6)        IN                             *NUMERO MESE MEMORIZ
194200000000     C                   MOVEL     DMM(IN)       PIDME6                         *DESC MESE IN STAMPA
194300990608     C                   MOVE      ANN(6)        PICAN6                         *NUM. ANNO IN STAMPA
194400000000E   1C                   ENDIF
194500000000     C*
194600000000IF  1C     MES(7)        IFGT      *ZEROS
194700000000     C                   MOVEL     MES(7)        IN                             *NUMERO MESE MEMORIZ
194800000000     C                   MOVEL     DMM(IN)       PIDME7                         *DESC MESE IN STAMPA
194900990608     C                   MOVE      ANN(7)        PICAN7                         *NUM. ANNO IN STAMPA
195000000000E   1C                   ENDIF
195100000000     C*
195200000000IF  1C     MES(8)        IFGT      *ZEROS
195300000000     C                   MOVEL     MES(8)        IN                             *NUMERO MESE MEMORIZ
195400000000     C                   MOVEL     DMM(IN)       PIDME8                         *DESC MESE IN STAMPA
195500990608     C                   MOVE      ANN(8)        PICAN8                         *NUM. ANNO IN STAMPA
195600000000E   1C                   ENDIF
195700000000     C*
195800000000IF  1C     MES(9)        IFGT      *ZEROS
195900000000     C                   MOVEL     MES(9)        IN                             *NUMERO MESE MEMORIZ
196000000000     C                   MOVEL     DMM(IN)       PIDME9                         *DESC MESE IN STAMPA
196100990608     C                   MOVE      ANN(9)        PICAN9                         *NUM. ANNO IN STAMPA
196200000000E   1C                   ENDIF
196300000000     C*
196400000000IF  1C     MES(10)       IFGT      *ZEROS
196500000000     C                   MOVEL     MES(10)       IN                             *NUMERO MESE MEMORIZ
196600000000     C                   MOVEL     DMM(IN)       PIDME0                         *DESC MESE IN STAMPA
196700990608     C                   MOVE      ANN(10)       PICAN0                         *NUM. ANNO IN STAMPA
196800000000E   1C                   ENDIF
196900000000     C*
197000000000IF  1C     MES(11)       IFGT      *ZEROS
197100000000     C                   MOVEL     MES(11)       IN                             *NUMERO MESE MEMORIZ
197200000000     C                   MOVEL     DMM(IN)       PIDMEA                         *DESC MESE IN STAMPA
197300990608     C                   MOVE      ANN(11)       PICANA                         *NUM. ANNO IN STAMPA
197400000000E   1C                   ENDIF
197500000000     C*
197600000000IF  1C     MES(12)       IFGT      *ZEROS
197700000000     C                   MOVEL     MES(12)       IN                             *NUMERO MESE MEMORIZ
197800000000     C                   MOVEL     DMM(IN)       PIDMEB                         *DESC MESE IN STAMPA
197900990608     C                   MOVE      ANN(12)       PICANB                         *NUM. ANNO IN STAMPA
198000000000E   1C                   ENDIF
198100000000     C*
198200000000     C                   ENDSR
198300000000     C*------------------------------------------------------------------------*
198400000000     C* *INZSR - OPERAZIONI INIZIALI
198500000000     C*------------------------------------------------------------------------*
198600000000     C     *INZSR        BEGSR
198700000000     C*--------------------
198800000000     C* RICEVIMENTO PARAMETRI
198900000000     C*--------------------
199000000000     C     *ENTRY        PLIST
199100000000     C                   PARM                    KPJBA
199200000000     C                   MOVEL     KPJBU         PARAM
199300000000     C*--------------------
199400000000     C* VARIABILI RIFERITE AL DATA BASE
199500000000     C*--------------------
199600000000     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
199700000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
199800011011     C     *LIKE         DEFINE    TBECOD        KBECOD                         *TNTBE00F
199900011011     C     *LIKE         DEFINE    TBEKE1        KBEKE1
200000000000     C     *LIKE         DEFINE    EVDVOC        KVDVOC                         *ECEVD00F
200100000000     C     *LIKE         DEFINE    EVDPRG        KVDPRG
200200000000     C     *LIKE         DEFINE    EVDTTV        KVDTTV
200300000000     C     *LIKE         DEFINE    EVDFT1        KVDFT1
200400000000     C     *LIKE         DEFINE    EVDFT2        KVDFT2
200500990609     C     *LIKE         DEFINE    SDTANN        KSTANN                         *SISDT00F
200600000000     C     *LIKE         DEFINE    WFRTFP        KWRTF1                         *WFTRA00F
200700000000     C     *LIKE         DEFINE    WFRTFP        KWRTF2
200800000000     C     *LIKE         DEFINE    WFRANN        KWRANN
200900000000     C     *LIKE         DEFINE    WFRMES        KWRMES
201000000000     C     *LIKE         DEFINE    WFRDEC        KWRDEC
201100011019     C     *LIKE         DEFINE    PCTMPA        KPCMPA                         *TIPCT00F
201200000000     C     *LIKE         DEFINE    PCTANN        KPCANN
201300000000     C     *LIKE         DEFINE    PCTMES        KPCMES
201400000000     C     *LIKE         DEFINE    PCTTFP        KPCTFP
201500000000     C     *LIKE         DEFINE    PCTTFA        KPCTFA
201600000000     C     *LIKE         DEFINE    SDTTFP        DEPTF1                         *DEPOSITI
201700000000     C     *LIKE         DEFINE    SDTTFP        DEPTF2
201800000000     C     *LIKE         DEFINE    SDTTFP        WTF1
201900000000     C     *LIKE         DEFINE    SDTTFP        WTF2
202000090907     C*    *LIKE         DEFINE    EVDITR        DEPITR
202100011018     C     *LIKE         DEFINE    SDTCTL        DEPCTL
202200000000     C     *LIKE         DEFINE    SDTDEC        DEPDEC
202300000000     C     *LIKE         DEFINE    EVDFT1        DEPFT1
202400000000     C     *LIKE         DEFINE    EVDFT2        DEPFT2
202500980224     C     *LIKE         DEFINE    ORGDIT        DEPDI1
202600980224     C     *LIKE         DEFINE    ORGDIT        DEPDI2
202700020627     C     *LIKE         DEFINE    §OGNTW        WPPOS                          *DI LAVORO
202800020627     C     *LIKE         DEFINE    §OGNTW        WAPOS
202900000000     C*--------------------
203000000000     C* CHIAVI DI LETTURA
203100000000     C*--------------------
203200000000     C* LETTURA TABEL00F - PARZIALE
203300000000     C     KE2TAB        KLIST
203400000000     C                   KFLD                    KTBKUT                         *CODICE UTENTE
203500000000     C                   KFLD                    KTBCOD                         *CODICE TABELLA
203600011011     C* LETTURA TNTBE01L - PARZIALE
203700011011     C     KEYTBE        KLIST
203800011011     C                   KFLD                    KBECOD                         *UTENTE
203900011011     C                   KFLD                    KBEKE1                         *CODICE
204000140114     C* LETTURA ECEVD06L - PARZIALE
204100000000     C     KEYEVD        KLIST
204200000000     C                   KFLD                    KVDVOC                         *VOCE
204300000000     C                   KFLD                    KVDPRG                         *PROGRESSIVO
204400000000     C                   KFLD                    KVDTTV                         *TIPO TARIFFA
204500000000     C                   KFLD                    KVDFT1                         *FILIALE UNO
204600000000     C                   KFLD                    KVDFT2                         *FILIALE DUE
204700000000     C* LETTURA WFTRA01L - PARZIALE
204800000000     C     KEYTRA        KLIST
204900000000     C                   KFLD                    KWRTF1                         *FILIALE UNO
205000000000     C                   KFLD                    KWRTF2                         *FILIALE DUE
205100000000     C                   KFLD                    KWRANN                         *ANNO
205200000000     C                   KFLD                    KWRMES                         *MESE
205300000000     C                   KFLD                    KWRDEC                         *DECADE
205400011019     C* LETTURA TIPCT01L - COMPLETA
205500000000     C     KEYPCT        KLIST
205600000000     C                   KFLD                    KPCMPA                         *PARTENZA O ARRIVO
205700000000     C                   KFLD                    KPCANN                         *ANNO
205800000000     C                   KFLD                    KPCMES                         *MESE
205900000000     C                   KFLD                    KPCTFP                         *FILIALE UNO
206000000000     C                   KFLD                    KPCTFA                         *FILIALE DUE
206100000000     C*--------------------
206200000000     C* VARIABILI DI CONTROLLO
206300000000     C*--------------------
206400000000     C                   MOVEL     '0'           $FINE             1            *FINE PGM
206500000000     C                   MOVEL     'N'           WRECOK            1            *VALIDITA' RECORD
206600000000     C                   MOVEL     '0'           PASTF2            1            *PASS MEMOR TF2
206700000000     C                   MOVEL     '0'           PASTF1            1            *PASS MEMOR TF1
206800980224     C                   MOVEL     '0'           PASDIB            1            *PASS MEMOR DIV.BAR.
206900980224     C                   MOVEL     '0'           PASDIS            1            *PASS MEMOR DIV.SDI
207000980224     C                   MOVEL     '0'           PASGEN            1            *PASS MEMOR GENERALE
207100000000     C*--------------------
207200000000     C* VARIABILI NON RIFERITE AL DATA BASE
207300000000     C*--------------------
207400000000     C                   Z-ADD     *ZEROS        CONBLO            2 0          *CONT DI BLOCCHI STA
207500000000     C                   MOVEL     '1'           *IN01                          *OVERFLOW
207600011018     c*
207700011018     c* reperimento data corrente
207800011018     C                   TIME                    n14              14 0          *ora (6)+ data (8)
207900011018     C                   MOVE      n14           n8                8 0          *data (8) in g/m/a
208000011018     C                   Z-ADD     n8            g08dat
208100011018     C                   Z-ADD     *zeros        g08inv
208200011018     C                   MOVEL     '0'           g08err
208300011018     c                   CALL      'XSRDA8'
208400011019     c                   PARM                    wlbda8
208500011019     C                   Z-ADD     g08inv        datcor            8 0          *Data corrente a/m/g
208600011011     C*
208700011011     C* IMPOSTA LA DATA FINALE LANCIO
208800011011     C                   Z-ADD     PARAAF        DSANN
208900011011     C                   Z-ADD     PARMMF        DSMES
209000011011     C                   Z-ADD     31            DSGIO
209100011011     C                   Z-ADD     DSDAT         DATFIN            8 0          *DATA FINALE LANCIO
209200000000     C*
209300000000     C* OPERAZIONI INIZIALI SUI FILE DA UTILIZZARE
209400000000     C                   EXSR      OPEFIL
209500000000     C*
209600000000     C* CARICA LE TABELLE
209700000000     C                   EXSR      CARTAB
209800000000     C*
209900000000     C* CARICA I MESI DA ELABORARE
210000000000     C                   EXSR      CARMES
210100000000     C*
210200000000     C* DECODIFICA INTESTAZIONE
210300000000     C                   EXSR      DECINT
210400000000     C*
210500000000     C                   ENDSR
210600000000     O*------------------------------------------------------------------------*
210700000000**   DTE - DECODIFICHE RIGHE DI TESTATA
210800000000PARTENZA            1
210900000000ARRIVO              2
211000000706                    3
211100000706                    4
2112000000005
211300000000**   DRI - DECODIFICHE RIGHE DI DETTAGLIO
211400000000GIORNI PARTENZE NORMALI       1
211500000000MC ASSEGNATI NORMALI          2
211600000000MC ASSEGNATI BIS              3
211700000000MC ASSEGNATI SEDE             4
211800960701MEDIA MC ASSEGNATI NORMALI/GG 5
211900000000MC PARTITI                    6
212000000000MC PARTITI/GIORNO             7
212100000000DELTA MC PARTITI/ASSEGNATI  MC8
212200000000DELTA MC PARTITI/ASSEGNATI  % 9
212300000000TN PER MCUBO                  10
212400000000COSTO  TN TOTALE              11
212500000000RICAVO TN TOTALE              12
212600011019DELTA RICAVO TN/COSTO TN    $ 13
212700000000DELTA RICAVO TN/COSTO TN    % 14
212800000000COSTO TN SEDE                 15
212900000000(RICAVO-COSTO TN) - COSTO SEDE16
213000990217NUMERO   SPED FINO  50QL      17
213100000000QUINTALI SPED   "    "        18
213200990217NUMERO   SPED OLTRE 50QL      19
213300000000QUINTALI SPED   "    "        20
213400000000MCUBI    SPED   "    "        21
21350000000022
21360000000023
21370000000024
21380000000025
213900980223P.OPERATIVO                   26
214000000000T O T A L E                   27
214100000000GENERALE                      28
214200110511BRT                           29
214300980224S.D.I.                        30
214400000000**   DMM - DECODIFICHE MESI DELL'ANNO
214500000000GEN  1
214600000000FEB  2
214700000000MAR  3
214800000000APR  4
214900000000MAG  5
215000000000GIU  6
215100000000LUG  7
215200000000AGO  8
215300000000SET  9
215400000000OTT  10
215500000000NOV  11
215600000000DIC  12
