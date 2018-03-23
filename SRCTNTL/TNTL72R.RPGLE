000100971007     H*------------------------------------------------------------------------*
000200971007     H* CONTO ECONOMICO TRAINI - ESEGUE
000300971007     H*------------------------------------------------------------------------*
000400971007     H DECEDIT('0,') DATEDIT(*DMY.)
000500971007     F*------------------------------------------------------------------------*
000600971007     F* DATA BASE
000700971007     F*------------------------------------------------------------------------*
000800971007     F*------------
000900971007     F* WORK FILE
001000971007     F*------------
001100011022     FTETEC01L  IF   E           K DISK    USROPN
001200011022     FTETEC02L  IF   E           K DISK    USROPN
001300011022     F                                     RENAME(TETEC000:TETEC2)
001400971007     F*------------
001500971007     F* STORICO TRAINI
001600971007     F*------------
001700971007     FTNTST03L  IF   E           K DISK
001800971007     F*------------
001900971007     F* VOCI DI C/ECONOMICO TRAINI
002000971007     F*------------
002100011022     FTITCG01L  IF   E           K DISK
002200971007     F*------------
002300971007     F* CALENDARIO
002400971007     F*------------
002500971007     FAZCLN01L  IF   E           K DISK
002600971007     F*------------
002700971007     F* ORGANIGRAMMA
002800971007     F*------------
002900971007     FAZORG01L  IF   E           K DISK
003000971007     F*------------
003100971007     F* TABELLE
003200971007     F*------------
003300971007     FTABEL00F  IF   E           K DISK
003400011022     FTNTBE01L  IF   E           K DISK
003500971007     F*------------
003600971007     F* PRINTER FILE
003700971007     F*------------
003800971007     FTNTL72P   O    E             PRINTER OFLIND(*IN01)
003900971007     D*------------------------------------------------------------------------*
004000971007     D* SCHIERE
004100971007     D*------------------------------------------------------------------------*
004200971007     D*------------------
004300971007     D* DECODIFICA RIGHE IN TESTATA
004400971007     D*------------------
004500971007     D DEC             S             50    DIM(10) CTDATA PERRCD(1)
004600971007     D*------------------
004700971007     D* ORGANIGRAMMA
004800971007     D*------------------
004900971125     D ORG             S              3  0 DIM(500)
005000971125     D DORG            S             20    DIM(500)
005100971007     D*------------------
005200971007     D* SIMBOLI CALENDARIO
005300971007     D*------------------
005400971007     D SIM             S              1    DIM(10)
005500971007     D*------------------
005600971007     D* CALENDARIO
005700971007     D*------------------
005800971007     D MAT             S              1    DIM(31)                              *MATTINO
005900971007     D POM             S              1    DIM(31)                              *POMERIGGIO
006000971007     D*------------------
006100971007     D* TIPI TRAINO
006200971007     D*------------------
006300971007     D TTR             S              1    DIM(20)                              *TIPO TRAINO DA USAR
006400011022     D*------------------
006500011022     D* Variabili di controllo
006600011022     D*------------------
006700011022     D $DIVSTE         S              3                                         *div OUT C/E traini
006800971007     D*------------------
006900971007     D* DS "XSRDA8" - CONTROLLA DATA (8)
007000971007     D*------------------
007100971007     D WLBDA8          DS                  INZ
007200971007     D  G08DAT                 1      8  0
007300971007     D  G08INV                 9     16  0
007400971007     D  G08ERR                17     17
007500971007     D  G08TGI                18     22  0
007600971007     D*------------------
007700011022     D* DS SCOMPOSIZIONE DATA -TETEC-
007800971007     D*------------------
007900971007     D                 DS
008000971007     D  TECANN                 1      4  0
008100971007     D  TECMES                 5      6  0
008200971007     D  TECDAT                 1      8  0
008300971007     D*------------------
008400971007     D* DS COMPOSIZIONE DATA GENERICA
008500971007     D*------------------
008600971007     D                 DS
008700971007     D  $ANN                   1      4  0
008800971007     D  $MES                   5      6  0
008900971007     D  $GIO                   7      8  0
009000971007     D  $DAT                   1      8  0
009100971007     D*------------------
009200971007     D* DS COMPOSIZIONE DESCRIZIONE
009300971007     D*------------------
009400971007     D                 DS
009500971007     D  DD1TOT                 1      7
009600971007     D  DC1TRN                 9     15
009700971007     D  DC1TFP                17     19
009800971007     D  DD1TRA                21     21
009900971007     D  DC1TFA                23     25
010000971007     D  DD1DES                 1     25
010100971007     D*------------------
010200971007     D* DS COMPOSIZIONE DESCRIZIONE
010300971007     D*------------------
010400971007     D                 DS
010500971007     D  DD2TOT                 1      9
010600971007     D  DC2TFP                11     13
010700971007     D  DD2TFP                15     25
010800971007     D  DD2DES                 1     25
010900971007     D*------------------
011000971007     D* DS COMPOSIZIONE DESCRIZIONE
011100971007     D*------------------
011200971007     D                 DS
011300971007     D  DC3LTP                10     12
011400971007     D  DD3TRA                14     14
011500971007     D  DC3LTA                16     18
011600971007     D  DD3DES                 1     25
011700971007     D*------------------
011800971007     D* DS COMPOSIZIONE DESCRIZIONE
011900971007     D*------------------
012000971007     D                 DS
012100971007     D*------------------
012200971007     D* DS PARAMETRI DI LANCIO
012300971007     D*------------------
012400971007     D PARAM           DS
012500971007     D  PARORD                 1      1
012600971007     D  PARCRE                 2      2
012700971007     D  PARDAI                 3     10  0
012800971007     D  PARAAI                 3      6  0
012900971007     D  PARMMI                 7      8  0
013000971007     D  PARGGI                 9     10  0
013100971007     D  PARDAF                11     18  0
013200971007     D  PARAAF                11     14  0
013300971007     D  PARMMF                15     16  0
013400971007     D  PARGGF                17     18  0
013500971007     D  PARTRI                19     25  0
013600971007     D  PARTRF                26     32  0
013700971007     D  PARTFP                33     35  0
013800011022     D*------------------
013900011022     D* DS LETTURA TABELLA "GED" PER DIVISE
014000011022     D*------------------
014100011022     D DGEDST        E DS                                                       *SALDI
014200011022     D*-------------------
014300011022     D* CONVERSIONI IMPORTI
014400011022     D*-------------------
014500011022     D YEUR          E DS                  EXTNAME(YEURCODS)
014600971007     D*------------------
014700971007     D* DS SIMBOLI CALENDARIO
014800971007     D*------------------
014900971007     D DS1Y          E DS
015000971007     D*------------------
015100971007     D* DS TIPI TRAINO
015200971007     D*------------------
015300971007     D DSTV          E DS
015400971007     D*------------------
015500971007     D* ARCHITETTURA
015600971007     D*------------------
015700971007     D KPJBA         E DS
015800971007     C*------------------------------------------------------------------------*
015900971007     C* MAIN LINE
016000971007     C*------------------------------------------------------------------------*
016100971007     C*
016200971007     C* STAMPA PARAMETRI DI LANCIO
016300971007     C                   EXSR      STAPAR
016400971007     C*
016500971007     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
016600971007     C                   EXSR      SETFIL
016700971007     C*
016800971007     C* CICLO FINO A FINE FILE
016900971007DO  1C     $FINE         DOWEQ     'N'
017000971007     C*
017100971007     C* OPERAZIONE PER NUOVA DATA
017200971007     C                   EXSR      NEWDTN
017300971007     C*
017400971007     C* CICLO FINO A ROTTURA DATA TRAINO
017500971007DO  2C     $FINE         DOWEQ     'N'
017600971007     C     TECDTN        ANDEQ     DEPDTN
017700971007     C*
017800971007     C* OPERAZIONE PER NUOVA ROTTURA TRAINO o LINEA PARTENZA
017900971007     C                   EXSR      NEWROT
018000971007     C*
018100971007     C* CICLO FINO A ROTTURA TRAINO o LINEA PARTENZA
018200971007DO  3C     $FINE         DOWEQ     'N'
018300971007     C     $ROTT         ANDEQ     'N'
018400971007     C*
018500971007     C* OPERAZIONE PER NUOVA LINEA PARTENZA/ARRIVO
018600971007     C                   EXSR      NEWLIN
018700971007     C*
018800971007     C* CICLO FINO A ROTTURA LINEA
018900971007DO  4C     $FINE         DOWEQ     'N'
019000971007     C     $ROTT         ANDEQ     'N'
019100971007     C     TECTFP        ANDEQ     DEPLTP
019200971007     C     TECTFA        ANDEQ     DEPLTA
019300971007     C*
019400971007     C* MEMORIZZA I DATI
019500971007     C                   EXSR      MEMDAT
019600971007     C*
019700971007     C* LETTURA SUCCESSIVA RECORD
019800971007     C                   EXSR      LETFIL
019900971007E   4C                   ENDDO                                                  *ROTTURA LINEA
020000971007     C*
020100971007     C* STAMPA TOTALI ROTTURA LINEA PARTENZA/ARRIVO
020200971007     C                   EXSR      STALIN
020300971007E   3C                   ENDDO                                                  *ROTTURA TRN/TFP
020400971007     C*
020500971007     C* STAMPA TOTALI ROTTURA TRAINO o LINEA PARTENZA
020600971007     C                   EXSR      STAROT
020700971007E   2C                   ENDDO                                                  *ROTTURA DATA
020800971007E   1C                   ENDDO                                                  *FINE FILE
020900971007     C*
021000971007     C* STAMPA TOTALI GENERALI
021100971007     C                   EXSR      STAGEN
021200971007     C*
021300971007     C* MEMORIZZA DATI PER PROSPETTO TOTALI
021400971007IF  1C     PARZ          IFEQ      'N'                                          *NO PARZIALIZZAZIONI
021500971007     C                   EXSR      MEMTOT
021600971007     C*
021700971007     C* STAMPA PROSPETTO TOTALI
021800971007     C                   EXSR      STATOT
021900971007E   1C                   ENDIF
022000971007     C*
022100971007     C* STAMPA RIGA DI FINE PROSPETTO
022200971007     C                   WRITE     TL72FI
022300971007     C*
022400971007     C* CHIUSURA FILE
022500971007     C                   EXSR      CLOFIL
022600971007     C*
022700971007     C                   SETON                                        LR
022800971007     C*------------------------------------------------------------------------*
022900971007     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA
023000971007     C*------------------------------------------------------------------------*
023100971007     C     SETFIL        BEGSR
023200971007     C*
023300971007     C                   Z-ADD     PARDAI        KWTDTN                         *DATA
023400971007IF  1C     PARORD        IFEQ      '1'                                          *NUMERO TRAINO
023500011022     C     KEYTEC        SETLL     TETEC01L                           99
023600971007X   1C                   ELSE                                                   *LINEA DI PARTENZA
023700011022     C     KEYTEC        SETLL     TETEC02L                           99
023800971007E   1C                   ENDIF
023900971007IF  1C     *IN99         IFEQ      *ON                                          *FINE FILE
024000971007     C                   MOVEL     'S'           $FINE                          *FINE PGM
024100971007X   1C                   ELSE
024200971007     C                   EXSR      LETFIL                                       *LETTURA RECORD
024300971007E   1C                   ENDIF
024400971007     C*
024500971007     C                   ENDSR
024600971007     C*------------------------------------------------------------------------*
024700971007     C* LETFIL - LETTURA PROSSIMO RECORD
024800971007     C*------------------------------------------------------------------------*
024900971007     C     LETFIL        BEGSR
025000971007     C*
025100971007     C* LEGGE FINO A:
025200971007     C*     - FINE FILE
025300971007     C*     - TROVATO RECORD VALIDO
025400971007     C                   MOVEL     'N'           WRECOK
025500971007DO  1C     $FINE         DOWEQ     'N'
025600971007     C     WRECOK        ANDEQ     'N'
025700971007IF  2C     PARORD        IFEQ      '1'                                          *NUMERO TRAINO
025800011022     C                   READ      TETEC01L                               99
025900971007X   2C                   ELSE                                                   *LINEA DI PARTENZA
026000011022     C                   READ      TETEC02L                               99
026100971007E   2C                   ENDIF
026200971007IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
026300971007     C                   MOVEL     'S'           $FINE                          *FINE PGM
026400971007X   2C                   ELSE
026500971007     C                   EXSR      CHKREC                                       *CONTROLLO RECORD
026600971007E   2C                   ENDIF
026700971007E   1C                   ENDDO
026800971007     C*
026900971007     C* CONTROLLA LA ROTTURA
027000971007     C                   EXSR      CHKROT
027100971007     C*
027200971007     C                   ENDSR
027300971007     C*------------------------------------------------------------------------*
027400971007     C* CHKREC - CONTROLLA VALIDITA' RECORD
027500971007     C*------------------------------------------------------------------------*
027600971007     C     CHKREC        BEGSR
027700971007     C*
027800971007     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
027900971007     C*
028000971007     C* PERIODO NON RICHIESTO, ESCLUDE
028100971007IF  1C     TECDTN        IFGE      PARDAI
028200971007     C     TECDTN        ANDLE     PARDAF
028300971007X   1C                   ELSE
028400971007     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
028500971007IF  2C     TECDTN        IFGT      PARDAF
028600971007     C                   MOVEL     'S'           $FINE                          *FINE PGM
028700971007E   2C                   ENDIF
028800971007E   1C                   ENDIF
028900971007     C*
029000971007     C* TRAINO NON RICHIESTO, ESCLUDE
029100971007IF  1C     PARTRI        IFGT      *ZEROS
029200971007IF  2C     TECTRN        IFGE      PARTRI
029300971007     C     TECTRN        ANDLE     PARTRF
029400971007X   1C                   ELSE
029500971007     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
029600971007E   2C                   ENDIF
029700971007E   1C                   ENDIF
029800971007     C*
029900971007     C* LINEA DI PARTENZA NON RICHIESTA, ESCLUDE
030000971007IF  1C     PARTFP        IFGT      *ZEROS
030100971007IF  2C     TECTFP        IFNE      PARTFP
030200971007     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
030300971007E   2C                   ENDIF
030400971007E   1C                   ENDIF
030500971007     C*
030600971007     C                   ENDSR
030700971007     C*------------------------------------------------------------------------*
030800971007     C* CHKROT - CONTROLLA LA ROTTURA
030900971007     C*------------------------------------------------------------------------*
031000971007     C     CHKROT        BEGSR
031100971007     C*
031200971007IF  1C     PARORD        IFEQ      '1'                                          *TRAINO
031300971007IF  2C     TECTRN        IFNE      DEPTRN                                       *LETTURA <> DEPOSITO
031400971007     C                   MOVEL     'S'           $ROTT                          *DA ROMPERE
031500971007E   2C                   ENDIF
031600971007X   1C                   ELSE                                                   *LINEA DI PARTENZA
031700971007IF  2C     TECTFP        IFNE      DEPTFP                                       *LETTURA <> DEPOSITO
031800971007     C                   MOVEL     'S'           $ROTT                          *DA ROMPERE
031900971007E   2C                   ENDIF
032000971007E   1C                   ENDIF
032100971007     C*
032200971007     C                   ENDSR
032300971007     C*------------------------------------------------------------------------*
032400971007     C* NEWDTN - OPERAZIONI PER NUOVA DATA TRAINO
032500971007     C*------------------------------------------------------------------------*
032600971007     C     NEWDTN        BEGSR
032700971007     C*
032800971007     C* MEMORIZZA LA NUOVA DATA IN UN DEPOSITO
032900971007     C                   Z-ADD     TECDTN        DEPDTN
033000971007     C*
033100971007     C* OPERAZIONI DI SALTO PAGINA SUCCESSIVO
033200971007     C                   MOVEL     '1'           *IN01                          *SI SALTO PAGINA
033300971007     C                   Z-ADD     DEPDTN        G08INV                         *INTESTAZIONE
033400971007     C                   Z-ADD     *ZEROS        G08DAT
033500971007     C                   MOVEL     '3'           G08ERR
033600971007     C                   CALL      'XSRDA8'
033700971007     C                   PARM                    WLBDA8
033800971007     C                   Z-ADD     G08DAT        INCDTN
033900971007     C*
034000971007     C                   ENDSR
034100971007     C*------------------------------------------------------------------------*
034200971007     C* NEWROT - OPERAZIONI PER NUOVO TRAINO o LINEA PARTENZA
034300971007     C*------------------------------------------------------------------------*
034400971007     C     NEWROT        BEGSR
034500971007     C*
034600971007     C* REIMPOSTA LE VARIABILI DI LAVORO
034700971007     C                   MOVEL     'N'           $ROTT                          *ROTTURA
034800971007     C                   MOVEL     'N'           PASROT                         *MEM.TOT.ROTTURA
034900971007     C                   MOVEL     'N'           STMINR                         *STAMPA ROTTURA
035000971007     C*
035100971007     C* MEMORIZZA IL NUOVO TRAINO o LA NUOVA LINEA PARTENZA IN UN DEPOSITO
035200971007IF  1C     PARORD        IFEQ      '1'                                          *TRAINO
035300971007     C                   Z-ADD     TECTRN        DEPTRN
035400971007X   1C                   ELSE                                                   *LINEA DI PARTENZA
035500971007     C                   Z-ADD     TECTFP        DEPTFP
035600971007E   1C                   ENDIF
035700971007     C*
035800971007     C* DEPOSITA ALTRI DATI (PER LA STAMPA)
035900971007     C                   Z-ADD     TECTFP        DEPTFP                         *FILIALE PARTENZA
036000971007     C                   Z-ADD     TECTFA        DEPTFA                         *FILIALE ARRIVO
036100971007     C*
036200971007     C* AZZERA I TOTALI
036300971007     C                   Z-ADD     *ZEROS        ROTMCA            9 2
036400971007     C                   Z-ADD     *ZEROS        ROTMCR            9 2
036500011022     C                   Z-ADD     *ZEROS        ROTCMA           15 3
036600011022     C                   Z-ADD     *ZEROS        ROTCMR           15 3
036700971007     C*
036800971007     C                   ENDSR
036900971007     C*------------------------------------------------------------------------*
037000971007     C* NEWLIN - OPERAZIONI PER NUOVA LINEA PARTENZA/ARRIVO
037100971007     C*------------------------------------------------------------------------*
037200971007     C     NEWLIN        BEGSR
037300971007     C*
037400971007     C* MEMORIZZA LA NUOVA LINEA PARTENZA/ARRIVO IN UN DEPOSITO
037500971007     C                   Z-ADD     TECTFP        DEPLTP
037600971007     C                   Z-ADD     TECTFA        DEPLTA
037700971007     C*
037800971007     C* AZZERA I TOTALI
037900971007     C                   Z-ADD     *ZEROS        LINMCA            9 2
038000971007     C                   Z-ADD     *ZEROS        LINMCR            9 2
038100011022     C                   Z-ADD     *ZEROS        LINCMA           15 3
038200011022     C                   Z-ADD     *ZEROS        LINCMR           15 3
038300971007     C*
038400971007     C                   ENDSR
038500971007     C*------------------------------------------------------------------------*
038600971007     C* MEMDAT - MEMORIZZA I DATI
038700971007     C*------------------------------------------------------------------------*
038800971007     C     MEMDAT        BEGSR
038900971007     C*
039000011022     C* AZZERA LE VARIABILI DI LAVORO
039100971007     C                   MOVEL     'S'           PASROT                         *MEM.TOT.ROTTURA
039200971007     C                   MOVEL     'S'           PASGEN                         *MEM.TOT.GENERALE
039300011022     C*
039400011022     C* SE LA DIVISA DI OUT DEL C/E TRAINI E' DIVERSA DELLA DIVISA DEL WFILE DEL C/E TRAINI
039500011022     C* (CREAZIONE NON CONTESTUALE ALLA STAMPA), CONVERTE GLI IMPORTI NELLA DIVISA DI OUT DEL C/E
039600011022     C                   EXSR      CNVTEC
039700971007     C* LINEA
039800971007     C                   ADD       TECMCA        LINMCA                         *MC ASSEGNATI
039900971007     C                   ADD       TECMCR        LINMCR                         *MC PARTITI
040000971007     C                   ADD       TECCMA        LINCMA                         *TN MC ASSEGNATI
040100971007     C                   ADD       TECCMR        LINCMR                         *TN MC PARTITI
040200971007     C* TRAINO o FILIALE
040300971007     C                   ADD       TECMCA        ROTMCA                         *MC ASSEGNATI
040400971007     C                   ADD       TECMCR        ROTMCR                         *MC PARTITI
040500971007     C                   ADD       TECCMA        ROTCMA                         *TN MC ASSEGNATI
040600971007     C                   ADD       TECCMR        ROTCMR                         *TN MC PARTITI
040700971007     C* GENERALE
040800971007     C                   ADD       TECMCA        GENMCA            9 2          *MC ASSEGNATI
040900971007     C                   ADD       TECMCR        GENMCR            9 2          *MC PARTITI
041000011022     C                   ADD       TECCMA        GENCMA           15 3          *TN MC ASSEGNATI
041100011022     C                   ADD       TECCMR        GENCMR           15 3          *TN MC PARTITI
041200971007     C*
041300971007     C                   ENDSR
041400011022     C*------------------------------------------------------------------------*
041500011022     C* CNVTEC - CONVERTE IMPORTI DI TETEC
041600011022     C*------------------------------------------------------------------------*
041700011022     C     CNVTEC        BEGSR
041800011022     C*
041900011022IF  1C     $DIVSTE       IFNE      TECDIV                                       *DIVISE <>
042000011022     C*
042100011022     C                   MOVEL     TECDIV        WDVI              3            *DIVISA  INPUT
042200011022     C                   Z-ADD     TECCMA        WIMI             30 9          *IMPORTO INPUT
042300011022     C                   EXSR      CNVEUR
042400011022     C                   Z-ADD     WIMO          TECCMA                         *TN MC ASSEGNATI
042500011022     C*
042600011022     C                   MOVEL     TECDIV        WDVI              3            *DIVISA  INPUT
042700011022     C                   Z-ADD     TECCMR        WIMI             30 9          *IMPORTO INPUT
042800011022     C                   EXSR      CNVEUR
042900011022     C                   Z-ADD     WIMO          TECCMR                         *TN MC PARTITI
043000011022     C*
043100011022E   1C                   ENDIF
043200011022     C*
043300011022     C                   ENDSR
043400971007     C*------------------------------------------------------------------------*
043500971007     C* MEMTOT - MEMORIZZA DATI PER PROSPETTO TOTALI
043600971007     C*------------------------------------------------------------------------*
043700971007     C     MEMTOT        BEGSR
043800971007     C*
043900971007     C* MEMORIZZA DATI -C/ECONOMICO-
044000971007     C                   EXSR      MEMTCG
044100971007     C*
044200971007     C* MEMORIZZA DATI -STORICO TRAINI-
044300971007     C                   EXSR      MEMTST
044400971007     C*
044500971007     C                   ENDSR
044600971007     C*------------------------------------------------------------------------*
044700971007     C* MEMTCG - MEMORIZZA DATI -C/ECONOMICO-
044800971007     C*------------------------------------------------------------------------*
044900971007     C     MEMTCG        BEGSR
045000971007     C*
045100971007     C* POSIZIONAMENTO E PRIMA LETTURA -CALENDARIO-
045200971007     C                   EXSR      SETCLN
045300971007     C*
045400971007     C* CICLO FINO A FINE FILE
045500971007DO  1C     $FCLN         DOWEQ     'N'
045600971007     C*
045700971007     C*
045800971007     C* SPOSTA I CAMPI DA TESTARE IN SCHIERE
045900971007     C                   MOVEA     CLNMAT        MAT                            *MATTINE
046000971007     C                   MOVEA     CLNPOM        POM                            *POMERIGGI
046100971007     C*
046200971007     C* PER CIASCUN GIORNO DI CALENDARIO
046300971007DO  2C     1             DO        31            J                 2 0
046400971007     C                   Z-ADD     CLNANN        $ANN
046500971007     C                   Z-ADD     CLNMES        $MES
046600971007     C                   Z-ADD     J             $GIO
046700971007     C* RICHIESTO
046800971007IF  3C     $DAT          IFGE      PARDAI
046900971007     C     $DAT          ANDLE     PARDAF
047000971007     C* VALIDO
047100971007     C                   MOVEL     'N'           WRECOK
047200971007     C                   SETOFF                                           98
047300971007     C     MAT(J)        LOOKUP    SIM                                    98
047400971007IF  4C     *IN98         IFEQ      *ON
047500971007     C                   MOVEL     'S'           WRECOK
047600971007X   4C                   ELSE
047700971007     C                   SETOFF                                           98
047800971007     C     POM(J)        LOOKUP    SIM                                    98
047900971007IF  5C     *IN98         IFEQ      *ON
048000971007     C                   MOVEL     'S'           WRECOK
048100971007E   5C                   ENDIF
048200971007E   4C                   ENDIF
048300971007IF  4C     WRECOK        IFEQ      'S'
048400971007     C*
048500971007     C* MEMORIZZA I DATI DEL C/ECONOMICO
048600971007     C                   Z-ADD     $DAT          KCGDAT                         *DATA
048700011022     C     KEYTCG        SETGT     TITCG01L
048800011022     C                   READP     TITCG01L                               98
048900971007IF  5C     *IN98         IFEQ      *OFF
049000971007     C                   MOVEL     'S'           PASGEN                         *MEM.TOT.GENERALE
049100011022     C*
049200011022     C* SE LA DIVISA DI OUT DEL C/E TRAINI E' DIVERSA DELLA DIVISA DEL FILE COSTI/RICAVI C/E TRAINI,
049300011022     C* CONVERTE GLI IMPORTI NELLA DIVISA DI OUT DEL C/E
049400011022     C                   EXSR      CNVTCG
049500011022     C*
049600011022     C                   ADD       TCGRCI        GENTRF           15 3          *RICAVI FISSI
049700011022     C                   ADD       TCGCCA        GENTCF           15 3          *COSTI FISSI
049800971007     C                   ADD       TCGCSA        GENTCF
049900971007     C                   ADD       TCGCAA        GENTCF
050000971007     C                   ADD       TCGCCV        GENTCF
050100971007     C                   ADD       TCGCSV        GENTCF
050200971007     C                   ADD       TCGCAV        GENTCF
050300971007     C                   ADD       TCGCOP        GENTCF
050400971007     C                   ADD       TCGCSP        GENTCF
050500971007     C                   ADD       TCGCIM        GENTCF
050600971007     C                   ADD       TCGCFA        GENTCF
050700971007     C                   ADD       TCGCVI        GENTCF
050800971007     C                   ADD       TCGCCT        GENTCF
050900971007     C                   ADD       TCGCTM        GENTCF
051000971007     C                   ADD       TCGCVS        GENTCF
051100971007     C                   ADD       TCGCTE        GENTCF
051200971007     C                   ADD       TCGCVA        GENTCF
051300971007E   5C                   ENDIF
051400971007E   4C                   ENDIF
051500971007E   3C                   ENDIF
051600971007E   2C                   ENDDO                                                  *FINE GIORNI MESE
051700971007     C*
051800971007     C* LETTURA SUCCESSIVA RECORD -CALENDARIO-
051900971007     C                   EXSR      LETCLN
052000971007E   1C                   ENDDO                                                  *FINE FILE
052100971007     C*
052200971007     C                   ENDSR
052300011022     C*------------------------------------------------------------------------*
052400011022     C* CNVTCG - CONVERTE IMPORTI DI TITCG
052500011022     C*------------------------------------------------------------------------*
052600011022     C     CNVTCG        BEGSR
052700011022     C*
052800011022IF  1C     $DIVSTE       IFNE      TCGDIV                                       *DIVISE <>
052900011022     C*
053000011022     C                   MOVEL     TCGDIV        WDVI              3            *DIVISA  INPUT
053100011022     C                   Z-ADD     TCGRCI        WIMI             30 9          *IMPORTO INPUT
053200011022     C                   EXSR      CNVEUR
053300011022     C                   Z-ADD     WIMO          TCGRCI
053400011022     C*
053500011022     C                   MOVEL     TCGDIV        WDVI              3            *DIVISA  INPUT
053600011022     C                   Z-ADD     TCGCCA        WIMI             30 9          *IMPORTO INPUT
053700011022     C                   EXSR      CNVEUR
053800011022     C                   Z-ADD     WIMO          TCGCCA
053900011022     C*
054000011022     C                   MOVEL     TCGDIV        WDVI              3            *DIVISA  INPUT
054100011022     C                   Z-ADD     TCGCSA        WIMI             30 9          *IMPORTO INPUT
054200011022     C                   EXSR      CNVEUR
054300011022     C                   Z-ADD     WIMO          TCGCSA
054400011022     C*
054500011022     C                   MOVEL     TCGDIV        WDVI              3            *DIVISA  INPUT
054600011022     C                   Z-ADD     TCGCAA        WIMI             30 9          *IMPORTO INPUT
054700011022     C                   EXSR      CNVEUR
054800011022     C                   Z-ADD     WIMO          TCGCAA
054900011022     C*
055000011022     C                   MOVEL     TCGDIV        WDVI              3            *DIVISA  INPUT
055100011022     C                   Z-ADD     TCGCCV        WIMI             30 9          *IMPORTO INPUT
055200011022     C                   EXSR      CNVEUR
055300011022     C                   Z-ADD     WIMO          TCGCCV
055400011022     C*
055500011022     C                   MOVEL     TCGDIV        WDVI              3            *DIVISA  INPUT
055600011022     C                   Z-ADD     TCGCSV        WIMI             30 9          *IMPORTO INPUT
055700011022     C                   EXSR      CNVEUR
055800011022     C                   Z-ADD     WIMO          TCGCSV
055900011022     C*
056000011022     C                   MOVEL     TCGDIV        WDVI              3            *DIVISA  INPUT
056100011022     C                   Z-ADD     TCGCAV        WIMI             30 9          *IMPORTO INPUT
056200011022     C                   EXSR      CNVEUR
056300011022     C                   Z-ADD     WIMO          TCGCAV
056400011022     C*
056500011022     C                   MOVEL     TCGDIV        WDVI              3            *DIVISA  INPUT
056600011022     C                   Z-ADD     TCGCOP        WIMI             30 9          *IMPORTO INPUT
056700011022     C                   EXSR      CNVEUR
056800011022     C                   Z-ADD     WIMO          TCGCOP
056900011022     C*
057000011022     C                   MOVEL     TCGDIV        WDVI              3            *DIVISA  INPUT
057100011022     C                   Z-ADD     TCGCSP        WIMI             30 9          *IMPORTO INPUT
057200011022     C                   EXSR      CNVEUR
057300011022     C                   Z-ADD     WIMO          TCGCSP
057400011022     C*
057500011022     C                   MOVEL     TCGDIV        WDVI              3            *DIVISA  INPUT
057600011022     C                   Z-ADD     TCGCIM        WIMI             30 9          *IMPORTO INPUT
057700011022     C                   EXSR      CNVEUR
057800011022     C                   Z-ADD     WIMO          TCGCIM
057900011022     C*
058000011022     C                   MOVEL     TCGDIV        WDVI              3            *DIVISA  INPUT
058100011022     C                   Z-ADD     TCGCFA        WIMI             30 9          *IMPORTO INPUT
058200011022     C                   EXSR      CNVEUR
058300011022     C                   Z-ADD     WIMO          TCGCFA
058400011022     C*
058500011022     C                   MOVEL     TCGDIV        WDVI              3            *DIVISA  INPUT
058600011022     C                   Z-ADD     TCGCVI        WIMI             30 9          *IMPORTO INPUT
058700011022     C                   EXSR      CNVEUR
058800011022     C                   Z-ADD     WIMO          TCGCVI
058900011022     C*
059000011022     C                   MOVEL     TCGDIV        WDVI              3            *DIVISA  INPUT
059100011022     C                   Z-ADD     TCGCCT        WIMI             30 9          *IMPORTO INPUT
059200011022     C                   EXSR      CNVEUR
059300011022     C                   Z-ADD     WIMO          TCGCCT
059400011022     C*
059500011022     C                   MOVEL     TCGDIV        WDVI              3            *DIVISA  INPUT
059600011022     C                   Z-ADD     TCGCTM        WIMI             30 9          *IMPORTO INPUT
059700011022     C                   EXSR      CNVEUR
059800011022     C                   Z-ADD     WIMO          TCGCTM
059900011022     C*
060000011022     C                   MOVEL     TCGDIV        WDVI              3            *DIVISA  INPUT
060100011022     C                   Z-ADD     TCGCVS        WIMI             30 9          *IMPORTO INPUT
060200011022     C                   EXSR      CNVEUR
060300011022     C                   Z-ADD     WIMO          TCGCVS
060400011022     C*
060500011022     C                   MOVEL     TCGDIV        WDVI              3            *DIVISA  INPUT
060600011022     C                   Z-ADD     TCGCTE        WIMI             30 9          *IMPORTO INPUT
060700011022     C                   EXSR      CNVEUR
060800011022     C                   Z-ADD     WIMO          TCGCTE
060900011022     C*
061000011022     C                   MOVEL     TCGDIV        WDVI              3            *DIVISA  INPUT
061100011022     C                   Z-ADD     TCGCVA        WIMI             30 9          *IMPORTO INPUT
061200011022     C                   EXSR      CNVEUR
061300011022     C                   Z-ADD     WIMO          TCGCVA
061400011022     C*
061500011022E   1C                   ENDIF
061600011022     C*
061700011022     C                   ENDSR
061800971007     C*------------------------------------------------------------------------*
061900971007     C* SETCLN - POSIZIONAMENTO E PRIMA LETTURA                     -CALENDARIO-
062000971007     C*------------------------------------------------------------------------*
062100971007     C     SETCLN        BEGSR
062200971007     C*
062300971007     C                   Z-ADD     068           KCLTFP                         *PARTENZA
062400971007     C                   Z-ADD     *ZEROS        KCLTFA                         *ARRIVO
062500011022     C                   Z-ADD     PARAAI        KCLANN                         *ANNO
062600971007     C                   Z-ADD     PARMMI        KCLMES                         *MESE
062700971007     C     KEYCLN        SETLL     AZCLN01L                           99
062800971007IF  1C     *IN99         IFEQ      *ON                                          *FINE FILE
062900971007     C                   MOVEL     'S'           $FCLN                          *FINE CICLO
063000971007X   1C                   ELSE
063100971007     C                   EXSR      LETCLN                                       *LETTURA RECORD
063200971007E   1C                   ENDIF
063300971007     C*
063400971007     C                   ENDSR
063500971007     C*------------------------------------------------------------------------*
063600971007     C* LETCLN - LETTURA PROSSIMO RECORD                            -CALENDARIO-
063700971007     C*------------------------------------------------------------------------*
063800971007     C     LETCLN        BEGSR
063900971007     C*
064000971007     C* LEGGE FINO A:
064100971007     C*     - FINE FILE
064200971007     C*     - TROVATO RECORD VALIDO
064300971007     C                   MOVEL     'N'           WRECOK
064400971007DO  1C     $FCLN         DOWEQ     'N'
064500971007     C     WRECOK        ANDEQ     'N'
064600971007     C     KE2CLN        READE     AZCLN01L                               99
064700971007IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
064800971007     C                   MOVEL     'S'           $FCLN                          *FINE CICLO
064900971007X   2C                   ELSE
065000971007     C                   EXSR      CHKCLN                                       *CONTROLLO RECORD
065100971007E   2C                   ENDIF
065200971007E   1C                   ENDDO
065300971007     C*
065400971007     C                   ENDSR
065500971007     C*------------------------------------------------------------------------*
065600971007     C* CHKCLN - CONTROLLA VALIDITA' RECORD                         -CALENDARIO-
065700971007     C*------------------------------------------------------------------------*
065800971007     C     CHKCLN        BEGSR
065900971007     C*
066000971007     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
066100971007     C*
066200971007     C* PERIODO -ANNO- NON RICHIESTO, ESCLUDE
066300971007IF  1C     CLNANN        IFGE      PARAAI
066400971007     C     CLNANN        ANDLE     PARAAF
066500971007X   1C                   ELSE
066600971007     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
066700971007E   1C                   ENDIF
066800971007     C*
066900971007     C* PERIODO -MESE- NON RICHIESTO, ESCLUDE
067000971007IF  1C     CLNMES        IFGE      PARMMI
067100971007     C     CLNMES        ANDLE     PARMMF
067200971007X   1C                   ELSE
067300971007     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
067400971007E   1C                   ENDIF
067500971007     C*
067600971007     C                   ENDSR
067700971007     C*------------------------------------------------------------------------*
067800971007     C* MEMTST - MEMORIZZA DATI -STORICO TRAINI-
067900971007     C*------------------------------------------------------------------------*
068000971007     C     MEMTST        BEGSR
068100971007     C*
068200971007     C* POSIZIONAMENTO E PRIMA LETTURA -STORICO TRAINI-
068300971007     C                   EXSR      SETTST
068400971007     C*
068500971007     C* CICLO FINO A FINE FILE
068600971007DO  1C     $FTST         DOWEQ     'N'
068700971007     C*
068800971007     C* MEMORIZZA I DATI DEL TRAINO
068900971007     C                   MOVEL     'S'           PASGEN                         *MEM.TOT.GENERALE
069000011022     C*
069100011022     C* SE LA DIVISA DI OUT DEL C/E TRAINI E' DIVERSA DELLA DIVISA DELLO STORICO TRAINI,
069200011022     C* CONVERTE GLI IMPORTI NELLA DIVISA DI OUT DEL C/E TRAINI
069300011022     C                   EXSR      CNVTST
069400011022     C                   ADD       TSTICO        GENTCT           15 3          *COSTO TRAINI
069500971007     C*
069600971007     C* LETTURA SUCCESSIVA RECORD -STORICO TRAINI-
069700971007     C                   EXSR      LETTST
069800971007E   1C                   ENDDO                                                  *FINE FILE
069900971007     C*
070000971007     C                   ENDSR
070100011022     C*------------------------------------------------------------------------*
070200011022     C* CNVTST - CONVERTE IMPORTI DI TETEC
070300011022     C*------------------------------------------------------------------------*
070400011022     C     CNVTST        BEGSR
070500011022     C*
070600011022IF  1C     $DIVSTE       IFNE      TSTDIV                                       *DIVISE <>
070700011022     C*
070800011022     C                   MOVEL     TSTDIV        WDVI              3            *DIVISA  INPUT
070900011022     C                   Z-ADD     TSTICO        WIMI             30 9          *IMPORTO INPUT
071000011022     C                   EXSR      CNVEUR
071100011022     C                   Z-ADD     WIMO          TSTICO                         *TN MC ASSEGNATI
071200011022     C*
071300011022E   1C                   ENDIF
071400011022     C*
071500011022     C                   ENDSR
071600971007     C*------------------------------------------------------------------------*
071700971007     C* SETTST - POSIZIONAMENTO E PRIMA LETTURA                 -STORICO TRAINI-
071800971007     C*------------------------------------------------------------------------*
071900971007     C     SETTST        BEGSR
072000971007     C*
072100971007     C                   Z-ADD     PARDAI        KSTDTN                         *DATA TRAINO
072200971007     C     KEYTST        SETLL     TNTST03L                           99
072300971007IF  1C     *IN99         IFEQ      *ON                                          *FINE FILE
072400971007     C                   MOVEL     'S'           $FTST                          *FINE CICLO
072500971007X   1C                   ELSE
072600971007     C                   EXSR      LETTST                                       *LETTURA RECORD
072700971007E   1C                   ENDIF
072800971007     C*
072900971007     C                   ENDSR
073000971007     C*------------------------------------------------------------------------*
073100971007     C* LETTST - LETTURA PROSSIMO RECORD                        -STORICO TRAINI-
073200971007     C*------------------------------------------------------------------------*
073300971007     C     LETTST        BEGSR
073400971007     C*
073500971007     C* LEGGE FINO A:
073600971007     C*     - FINE FILE
073700971007     C*     - TROVATO RECORD VALIDO
073800971007     C                   MOVEL     'N'           WRECOK
073900971007DO  1C     $FTST         DOWEQ     'N'
074000971007     C     WRECOK        ANDEQ     'N'
074100971007     C                   READ      TNTST03L                               99
074200971007IF  2C     *IN99         IFEQ      *ON                                          *FINE FINE
074300971007     C                   MOVEL     'S'           $FTST                          *FINE CICLO
074400971007X   2C                   ELSE
074500971007     C                   EXSR      CHKTST                                       *CONTROLLO RECORD
074600971007E   2C                   ENDIF
074700971007E   1C                   ENDDO
074800971007     C*
074900971007     C                   ENDSR
075000971007     C*------------------------------------------------------------------------*
075100971007     C* CHKTST - CONTROLLA VALIDITA' RECORD                     -STORICO TRAINI-
075200971007     C*------------------------------------------------------------------------*
075300971007     C     CHKTST        BEGSR
075400971007     C*
075500971007     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
075600971007     C*
075700971007     C* PERIODO NON RICHIESTO, ESCLUDE
075800971007IF  1C     TSTDTN        IFGE      PARDAI
075900971007     C     TSTDTN        ANDLE     PARDAF
076000971007X   1C                   ELSE
076100971007     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
076200971007E   1C                   ENDIF
076300971007     C*
076400971007     C* TRAINO ANNULLATO, ESCLUDE
076500971007IF  1C     TSTATB        IFNE      *BLANKS
076600971007     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
076700971007E   1C                   ENDIF
076800971007     C*
076900971007     C* TRAINO NON CONFERMATO, ESCLUDE
077000971007IF  1C     TSTCNF        IFEQ      *BLANKS
077100971007     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
077200971007E   1C                   ENDIF
077300971007     C*
077400971007     C* TRAINO DA NON USARE, ESCLUDE
077500971007     C                   SETOFF                                           98
077600971007     C     TSTTTR        LOOKUP    TTR                                    98
077700971007IF  1C     *IN98         IFEQ      *OFF                                         *NON TROVATO
077800971007     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
077900971007E   1C                   ENDIF
078000971007     C*
078100971007     C                   ENDSR
078200971007     C*------------------------------------------------------------------------*
078300971007     C* STALIN - STAMPA TOTALI LINEA PARTENZA/ARRIVO
078400971007     C*------------------------------------------------------------------------*
078500971007     C     STALIN        BEGSR
078600971007     C*
078700971007     C* SE MEMORIZZATO QUALCOSA
078800971007IF  1C     PASROT        IFEQ      'S'
078900971007     C                   EXSR      STATES                                       *STAMPA TESTATA
079000971007     C*
079100971007     C* STAMPA INTESTAZIONE ROTTURA -SE ANCORA DA STAMPARE-
079200971007IF  2C     STMINR        IFEQ      'N'
079300971007     C                   CLEAR                   TL72DE                         *PULISCE FORMATO
079400971007     C                   EXSR      DECINR                                       *DECOD INTESTAZIONE
079500971007     C                   WRITE     TL72DE                                       *DETTAGLIO
079600971007     C                   MOVEL     'S'           STMINR
079700971007E   2C                   ENDIF
079800971007     C*
079900971007     C* STAMPA RIGA DI DETTAGLIO
080000971007     C                   CLEAR                   TL72DE
080100971007     C                   MOVEL     DEPLTP        DC3LTP
080200971007     C                   MOVEL     '/'           DD3TRA
080300971007     C                   MOVEL     DEPLTA        DC3LTA
080400971007     C                   MOVEL     DD3DES        DEDDES
080500971007     C                   ADD       LINMCA        DECMCA                         *MC ASSEGNATI
080600971007     C                   ADD       LINMCR        DECMCR                         *MC PARTITI
080700971007     C     LINMCA        SUB       LINMCR        DECMDF                         *MC ASSEG - PARTITI
080800971007     C                   ADD       LINCMA        DECTMA                         *TN MC ASSEGNATI
080900971007     C                   ADD       LINCMR        DECTMR                         *TN MC PARTITI
081000971007     C     LINCMA        SUB       LINCMR        DECTDF                         *TN ASSEG - PARTITI
081100971007     C                   WRITE     TL72DE                                       *DETTAGLIO
081200971007E   1C                   ENDIF
081300971007     C*
081400971007     C                   ENDSR
081500971007     C*------------------------------------------------------------------------*
081600971007     C* STAROT - STAMPA TOTALI TRAINO o LINEA PARTENZA
081700971007     C*------------------------------------------------------------------------*
081800971007     C     STAROT        BEGSR
081900971007     C*
082000971007     C* SE MEMORIZZATO QUALCOSA
082100971007IF  1C     PASROT        IFEQ      'S'
082200971007     C                   EXSR      STATES                                       *STAMPA TESTATA
082300971007     C*
082400971007     C* STAMPA RIGA DI TOTALE
082500971007     C                   CLEAR                   TL72DE                         *PULISCE FORMATO
082600971007IF  2C     PARORD        IFEQ      '1'                                          *NUMERO TRAINO
082700971007     C                   MOVEL     DEC(5)        DEDDES
082800971007X   2C                   ELSE                                                   *LINEA PARTENZA
082900971007     C                   MOVEL     DEC(6)        DEDDES
083000971007E   2C                   ENDIF
083100971007     C                   ADD       ROTMCA        DECMCA                         *MC ASSEGNATI
083200971007     C                   ADD       ROTMCR        DECMCR                         *MC PARTITI
083300971007     C     ROTMCA        SUB       ROTMCR        DECMDF                         *MC ASSEG - PARTITI
083400971007     C                   ADD       ROTCMA        DECTMA                         *TN MC ASSEGNATI
083500971007     C                   ADD       ROTCMR        DECTMR                         *TN MC PARTITI
083600971007     C     ROTCMA        SUB       ROTCMR        DECTDF                         *TN ASSEG - PARTITI
083700971007     C                   WRITE     TL72DE                                       *DETTAGLIO
083800971008     C                   WRITE     TL72RI                                       *RIGA SEPARAZIONE
083900971007E   1C                   ENDIF
084000971007     C*
084100971007     C                   ENDSR
084200971007     C*------------------------------------------------------------------------*
084300971007     C* STAGEN - STAMPA TOTALI GENERALI
084400971007     C*------------------------------------------------------------------------*
084500971007     C     STAGEN        BEGSR
084600971007     C*
084700971007     C* SE MEMORIZZATO QUALCOSA
084800971007IF  1C     PASGEN        IFEQ      'S'
084900971007     C                   EXSR      STATES                                       *STAMPA TESTATA
085000971007     C*
085100971007     C* STAMPA RIGA DI TOTALE
085200971007     C                   CLEAR                   TL72DE                         *PULISCE FORMATO
085300971007     C                   MOVEL     DEC(7)        DEDDES
085400971007     C                   ADD       GENMCA        DECMCA                         *MC ASSEGNATI
085500971007     C                   ADD       GENMCR        DECMCR                         *MC PARTITI
085600971007     C     GENMCA        SUB       GENMCR        DECMDF                         *MC ASSEG - PARTITI
085700971007     C                   ADD       GENCMA        DECTMA                         *TN MC ASSEGNATI
085800971007     C                   ADD       GENCMR        DECTMR                         *TN MC PARTITI
085900971007     C     GENCMA        SUB       GENCMR        DECTDF                         *TN ASSEG - PARTITI
086000971007     C                   WRITE     TL72DE                                       *DETTAGLIO
086100971007     C                   WRITE     TL72RI                                       *RIGA SEPARAZIONE
086200971007E   1C                   ENDIF
086300971007     C*
086400971007     C                   ENDSR
086500971007     C*------------------------------------------------------------------------*
086600971007     C* STATOT - STAMPA PROSPETTO TOTALI
086700971007     C*------------------------------------------------------------------------*
086800971007     C     STATOT        BEGSR
086900971007     C*
087000971007     C                   Z-ADD     GENCMR        DTCTRT                         *RICAVI TRAINO
087100971007     C                   Z-ADD     GENTRF        DTCTRF                         *RICAVI FISSI
087200971007     C                   Z-ADD     GENTCT        DTCTCT                         *COSTI  TRAINO
087300971007     C                   Z-ADD     GENTCF        DTCTCF                         *COSTI  FISSI
087400011022     C     GENCMR        ADD       GENTRF        RICAVI           15 3
087500011022     C     GENTCT        ADD       GENTCF        COSTI            15 3
087600971007     C     RICAVI        SUB       COSTI         DTCDIF                         *RICAVI - COSTI
087700971007     C                   WRITE     TL72TE                                       *TESTATA
087800971007     C                   WRITE     TL72IT                                       *INTESTAZIONE
087900971007     C                   WRITE     TL72DT                                       *TOTALI
088000971007     C                   WRITE     TL72R2                                       *RIGA SEPARAZIONE
088100971007     C*
088200971007     C                   ENDSR
088300971007     C*------------------------------------------------------------------------*
088400971007     C* DECINR - DECODIFICA INTESTAZIONE ROTTURA
088500971007     C*------------------------------------------------------------------------*
088600971007     C     DECINR        BEGSR
088700971007     C*
088800971007     C* ORDINAMENTO -> NUMERO TRAINO
088900971007IF  1C     PARORD        IFEQ      '1'
089000971007     C                   MOVEL     DEC(3)        DD1TOT
089100971007     C                   MOVEL     DEPTRN        DC1TRN
089200971007     C                   MOVEL     DEPTFP        DC1TFP
089300971007     C                   MOVEL     '-'           DD1TRA
089400971007     C                   MOVEL     DEPTFA        DC1TFA
089500971007     C                   MOVEL     DD1DES        DEDDES
089600971007     C*
089700971007     C* ORDINAMENTO -> LINEA DI PARTENZA
089800971007X   1C                   ELSE
089900971007     C                   MOVEL     DEC(4)        DD2TOT
090000971007     C                   MOVEL     DEPTFP        DC2TFP
090100971007     C                   Z-ADD     1             I
090200971007     C                   SETOFF                                           98
090300971007     C     DEPTFP        LOOKUP    ORG(I)                                 98
090400971007IF  2C     *IN98         IFEQ      *ON
090500971007     C                   MOVEL     DORG(I)       DD2TFP
090600971007E   2C                   ENDIF
090700971007     C                   MOVEL     DD2DES        DEDDES
090800971007E   1C                   ENDIF
090900971007     C*
091000971007     C                   ENDSR
091100971007     C*------------------------------------------------------------------------*
091200971007     C* STATES - STAMPA TESTATA E INTESTAZIONE
091300971007     C*------------------------------------------------------------------------*
091400971007     C     STATES        BEGSR
091500971007     C*
091600971007IF  1C     *IN01         IFEQ      *ON                                          *FINE PAGINA
091700971007     C                   WRITE     TL72TE                                       *TESTATA
091800971007     C                   WRITE     TL72IN                                       *INTESTAZIONE
091900971007     C                   SETOFF                                       01
092000971007E   1C                   ENDIF
092100971007     C*
092200971007     C                   ENDSR
092300971007     C*------------------------------------------------------------------------*
092400971007     C* STAPAR - STAMPA LA PAGINA DEI PARAMETRI
092500971007     C*------------------------------------------------------------------------*
092600971007     C     STAPAR        BEGSR
092700971007     C*
092800011022     C* INTESTAZIONE
092900011022     C                   EVAL      TECDIV = $DIVSTE                             *DIVISA
093000011022     C*
093100011022     C* PARAMETRI
093200971007IF  1C     PARORD        IFEQ      '1'
093300971007     C                   MOVEL     DEC(1)        PADORD                         *ORDINAMENTO
093400971007X   1C                   ELSE
093500971007     C                   MOVEL     DEC(2)        PADORD                         *ORDINAMENTO
093600971007E   1C                   ENDIF
093700971007IF  1C     PARCRE        IFEQ      'S'
093800971007     C                   MOVEL     'SI'          PADCRE                         *CREAZIONE WFILE
093900971007X   1C                   ELSE
094000971007     C                   MOVEL     'NO'          PADCRE                         *CREAZIONE WFILE
094100971007E   1C                   ENDIF
094200971007     C                   Z-ADD     PARDAI        G08INV                         *DATA INIZIALE
094300971007     C                   Z-ADD     *ZEROS        G08DAT
094400971007     C                   MOVEL     '3'           G08ERR
094500971007     C                   CALL      'XSRDA8'
094600971007     C                   PARM                    WLBDA8
094700971007     C                   Z-ADD     G08DAT        PACDAI
094800971007     C                   Z-ADD     PARDAF        G08INV                         *DATA FINALE
094900971007     C                   Z-ADD     *ZEROS        G08DAT
095000971007     C                   MOVEL     '3'           G08ERR
095100971007     C                   CALL      'XSRDA8'
095200971007     C                   PARM                    WLBDA8
095300971007     C                   Z-ADD     G08DAT        PACDAF
095400971007     C                   Z-ADD     PARTRI        PACTRI                         *TRAINO INIZIALE
095500971007     C                   Z-ADD     PARTRF        PACTRF                         *TRAINO FINALE
095600971007     C                   Z-ADD     PARTFP        PACTFP                         *FILIALE PARTENZA
095700971007IF  1C     PARTFP        IFGT      *ZEROS
095800971007     C                   Z-ADD     1             I
095900971007     C                   SETOFF                                           98
096000971007     C     PARTFP        LOOKUP    ORG(I)                                 98
096100971007IF  2C     *IN98         IFEQ      *ON
096200971007     C                   MOVEL     DORG(I)       PADTFP                         *DESCRIZIONE FILIALE
096300971007E   2C                   ENDIF
096400971007E   1C                   ENDIF
096500971007     C*
096600971007     C                   WRITE     TL72TE                                       *TESTATA
096700971007     C                   WRITE     TL72PA                                       *PARAMETRI
096800971007     C*
096900971007     C                   ENDSR
097000011022     C*------------------------------------------------------------------------*
097100011022     C* CONVERTE I CAMPI NELLA DIVISA DI OUTPUT DEL C/ECONOMICO TRAINI
097200011022     C*------------------------------------------------------------------------*
097300011022     C     CNVEUR        BEGSR
097400011022     C*
097500011022     C* AZZERA VARIABILI DI LAVORO
097600011022     C                   Z-ADD     *ZEROS        WIMO             30 9          *IMPORTO OUTPUT
097700011022     C                   MOVEL     *BLANKS       NDEC              1            *N° DEC DI OUTPUT
097800011022     C*
097900011022     C* IMPOSTO I DECIMALI DI OUTPUT A SECONDA DELLA DIVISA DI OUTPUT
098000011022     C                   IF        $DIVSTE = 'ITL'
098100011022     C                   EVAL      NDEC = '0'
098200011022     C                   ELSE
098300011022     C                   EVAL      NDEC = '3'
098400011022     C                   ENDIF
098500011022     C*
098600011022IF  1C     WIMI          IFNE      *ZEROS
098700011022     C                   CLEAR                   YEUR
098800011022     C                   MOVEL     WDVI          YECDVI                         *DIVISA INPUT
098900011022     C                   Z-ADD     WIMI          YECIMI                         *IMPORTO INPUT
099000011022     C                   MOVEL     $DIVSTE       YECDVO                         *DIVISA OUTPUT
099100011022     C                   MOVEL     NDEC          YECDCO                         *N° DECIMALI
099200011022     C                   MOVEL     *BLANKS       YECTAR                         *ARROTONDAMENTO "H"
099300011022     C                   CALL      'YEURCO'
099400011022     C                   PARM                    YEUR
099500011022IF  2C     YECESI        IFEQ      ' '                                          *NO ERRORI
099600011022     C                   Z-ADD     YECIMO        WIMO                            -IMPORTO CONVERTITO
099700011022X   2C                   ELSE                                                   *SI ERRORI
099800011022     C                   Z-ADD     WIMI          WIMO                            -IMPOROTO DA CONVER
099900011022E   2C                   ENDIF
100000011022E   1C                   ENDIF
100100011022     C*
100200011022     C                   ENDSR
100300971007     C*------------------------------------------------------------------------*
100400971007     C* CLOFIL - OPERAZIONI FINALI FILE
100500971007     C*------------------------------------------------------------------------*
100600971007     C     CLOFIL        BEGSR
100700971007     C*
100800971007IF  1C     PARORD        IFEQ      '1'                                          *TRAINO
100900011022     C                   CLOSE     TETEC01L
101000971007E   1C                   ENDIF
101100971007IF  1C     PARORD        IFEQ      '2'                                          *FILIALE PARTENZA
101200011022     C                   CLOSE     TETEC02L
101300971007E   1C                   ENDIF
101400971007     C*
101500971007     C                   ENDSR
101600971007     C*------------------------------------------------------------------------*
101700971007     C* OPEFIL - OPERAZIONI INIZIALI FILE
101800971007     C*------------------------------------------------------------------------*
101900971007     C     OPEFIL        BEGSR
102000971007     C*
102100971007IF  1C     PARORD        IFEQ      '1'                                          *TRAINO
102200011022     C                   OPEN      TETEC01L
102300971007E   1C                   ENDIF
102400971007IF  1C     PARORD        IFEQ      '2'                                          *FILIALE PARTENZA
102500011022     C                   OPEN      TETEC02L
102600971007E   1C                   ENDIF
102700971007     C*
102800971007     C                   ENDSR
102900971007     C*------------------------------------------------------------------------*
103000971007     C* CARTAB - CARICA LE TABELLE OCCORRENTI
103100971007     C*------------------------------------------------------------------------*
103200971007     C     CARTAB        BEGSR
103300971007     C*--------------------
103400971007     C* ORGANIGRAMMA
103500971007     C*--------------------
103600971007     C                   Z-ADD     *ZEROS        I                 5 0
103700971007     C     *LOVAL        SETLL     AZORG01L
103800971007     C                   READ      AZORG01L                               99
103900971007DO  1C     *IN99         DOWEQ     *OFF
104000971007IF  2C     ORGFVA        IFEQ      *BLANKS
104100971007IF  3C     ORGFAG        IFNE      'V'
104200971007     C                   ADD       1             I
104300971007     C                   Z-ADD     ORGFIL        ORG(I)                         *CODICE
104400971007     C                   MOVEL     ORGDES        DORG(I)                        *DESCRIZIONE
104500971007E   3C                   ENDIF
104600971007E   2C                   ENDIF
104700971007     C                   READ      AZORG01L                               99
104800971007E   1C                   ENDDO
104900971007     C*--------------------
105000971007     C* SIMBOLI CALENDARIO
105100971007     C*--------------------
105200971007     C                   Z-ADD     *ZEROS        I                 5 0
105300020906     C                   Z-ADD     1             KTBKUT
105400971007     C                   MOVEL     '1Y'          KTBCOD
105500971007     C     KEYTAB        SETLL     TABEL00F
105600971007     C     KEYTAB        READE     TABEL00F                               99
105700971007DO  1C     *IN99         DOWEQ     *OFF
105800971007IF  2C     TBLFLG        IFEQ      *BLANKS
105900971007     C                   MOVEL     TBLUNI        DS1Y
106000971007IF  3C     §1YFET        IFNE      'B'                                          *DA UTILIZZARE
106100971007     C                   ADD       1             I
106200971007     C                   MOVEL     TBLKEY        SIM(I)
106300971007E   3C                   ENDIF
106400971007E   2C                   ENDIF
106500971007     C     KEYTAB        READE     TABEL00F                               99
106600971007E   1C                   ENDDO
106700971007     C*-------------------------
106800971007     C* TIPI TRAINO
106900971007     C*-------------------------
107000971007     C                   Z-ADD     *ZEROS        I                 5 0
107100020906     C                   Z-ADD     1             KTBKUT
107200971007     C                   MOVEL     'TV'          KTBCOD
107300971007     C     KEYTAB        SETLL     TABEL00F
107400971007     C     KEYTAB        READE     TABEL00F                               99
107500971007DO  1C     *IN99         DOWEQ     *OFF
107600971007IF  2C     TBLFLG        IFEQ      *BLANKS
107700971007     C                   MOVEL     TBLUNI        DSTV
107800971007IF  3C     §TVUTC        IFEQ      'S'                                          *DA UTILIZZARE
107900971007     C                   ADD       1             I
108000971007     C                   MOVEL     TBLKEY        TTR(I)                         *TIPO TRAINO
108100971007E   3C                   ENDIF
108200971007E   2C                   ENDIF
108300971007     C     KEYTAB        READE     TABEL00F                               99
108400971007E   1C                   ENDDO
108500011022     C*--------------------
108600011022     C* DIVISE UTILI
108700011022     C*--------------------
108800011022     C* C/ECONOMICO (OUT)
108900011022     C                   CLEAR                   DGEDST
109000011022     C                   MOVEL(P)  'GED'         KBECOD
109100011022     C                   MOVEL(P)  'ST'          KBEKE1
109200011022     C     KEYTBE        CHAIN     TNTBE01L
109300011022IF  1C                   IF        %FOUND(TNTBE01L)
109400011022     C                   MOVEL     TBEUNI        DGEDST
109500011022E   1C                   ENDIF
109600011022IF  1C                   IF        §GEDSTDATE >= DATCOR                         *D.RIF su D.LANCIO
109700011022     C                   EVAL      $DIVSTE = §GEDSTDIPE                         *DIVISA PRIMA (ITL)
109800011022X   1C                   ELSE
109900011022     C                   EVAL      $DIVSTE = §GEDSTDIDE                         *DIVISA DOPO (EUR)
110000011022E   1C                   ENDIF
110100971007     C*
110200971007     C                   ENDSR
110300971007     C*------------------------------------------------------------------------*
110400971007     C* *INZSR - OPERAZIONI INIZIALI
110500971007     C*------------------------------------------------------------------------*
110600971007     C     *INZSR        BEGSR
110700971007     C*--------------------
110800971007     C* RICEVIMENTO PARAMETRI
110900971007     C*--------------------
111000971007     C     *ENTRY        PLIST
111100971007     C                   PARM                    KPJBA
111200971007     C                   MOVEL     KPJBU         PARAM
111300971007     C*--------------------
111400971007     C* VARIABILI RIFERITE AL DATA BASE
111500971007     C*--------------------
111600971007     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
111700971007     C     *LIKE         DEFINE    TBLCOD        KTBCOD
111800011022     C     *LIKE         DEFINE    TBECOD        KBECOD                         *TNTBE01L
111900011022     C     *LIKE         DEFINE    TBEKE1        KBEKE1
112000971007     C     *LIKE         DEFINE    CLNTFP        KCLTFP                         *AZCLN00F
112100971007     C     *LIKE         DEFINE    CLNTFA        KCLTFA
112200971007     C     *LIKE         DEFINE    CLNANN        KCLANN
112300971007     C     *LIKE         DEFINE    CLNMES        KCLMES
112400011022     C     *LIKE         DEFINE    TECDTN        KWTDTN                         *TETEC00F
112500971007     C     *LIKE         DEFINE    TSTDTN        KSTDTN                         *TNTST00F
112600011022     C     *LIKE         DEFINE    TCGDAT        KCGDAT                         *TITCG00F
112700971007     C     *LIKE         DEFINE    TECDTN        DEPDTN                         *DEPOSITI
112800971007     C     *LIKE         DEFINE    TECTRN        DEPTRN
112900971007     C     *LIKE         DEFINE    TECTFP        DEPTFP
113000971007     C     *LIKE         DEFINE    TECTFA        DEPTFA
113100971007     C     *LIKE         DEFINE    TECTFP        DEPLTP
113200971007     C     *LIKE         DEFINE    TECTFA        DEPLTA
113300971007     C*--------------------
113400971007     C* CHIAVI DI LETTURA
113500971007     C*--------------------
113600971007     C* LETTURA TEBEL00F - PARZIALE
113700971007     C     KEYTAB        KLIST
113800971007     C                   KFLD                    KTBKUT                         *UTENTE
113900971007     C                   KFLD                    KTBCOD                         *TABELLA
114000011022     C* LETTURA TNTBE01L - PARZIALE
114100011022     C     KEYTBE        KLIST
114200011022     C                   KFLD                    KBECOD                         *TABELLA
114300011022     C                   KFLD                    KBEKE1                         *CHIAVE 1
114400011022     C* LETTURA TETEC01L - COMPLETA
114500971007     C     KEYTEC        KLIST
114600971007     C                   KFLD                    KWTDTN                         *DATA TRAINO
114700971007     C* LETTURA AZCLN01L - COMPLETA
114800971007     C     KEYCLN        KLIST
114900971007     C                   KFLD                    KCLTFP                         *FILIALE PARTENZA
115000971007     C                   KFLD                    KCLTFA                         *FILIALE ARRIVO
115100971007     C                   KFLD                    KCLANN                         *ANNO
115200971007     C                   KFLD                    KCLMES                         *MESE
115300971007     C* LETTURA AZCLN01L - PARZIALE
115400971007     C     KE2CLN        KLIST
115500971007     C                   KFLD                    KCLTFP                         *FILIALE PARTENZA
115600971007     C                   KFLD                    KCLTFA                         *FILIALE ARRIVO
115700971007     C* LETTURA TNTST03L - COMPLETA
115800971007     C     KEYTST        KLIST
115900971007     C                   KFLD                    KSTDTN                         *DATA TRAINO
116000011022     C* LETTURA TITCG01L - COMPLETA
116100971007     C     KEYTCG        KLIST
116200971007     C                   KFLD                    KCGDAT                         *DATA
116300971007     C*--------------------
116400971007     C* VARIABILI DI CONTROLLO
116500971007     C*--------------------
116600971007     C                   MOVEL     'N'           $FINE             1            *FINE PGM
116700971007     C                   MOVEL     'N'           $FCLN             1            *FINE CICLO AZCLN
116800971007     C                   MOVEL     'N'           $FTST             1            *FINE CICLO TNTST
116900971007     C                   MOVEL     'N'           $ROTT             1            *ROTTURA CICLO
117000971007     C                   MOVEL     'N'           STMINR            1            *STM INTEST ROTTURA
117100971007     C                   MOVEL     'N'           WRECOK            1            *VALIDITA' RECORD
117200971007     C                   MOVEL     'N'           PASROT            1            *MEM.TOT.ROTTURA
117300971007     C                   MOVEL     'N'           PASGEN            1            *MEM.TOT.GENERALE
117400971007     C                   MOVEL     'N'           PARZ              1            *PARZIALIZZAZIONI
117500971007     C                   MOVEL     '1'           *IN01                          *OVERFLOW
117600971007     C*
117700971007     C* OPERAZIONI INIZIALI SUI FILE DA UTILIZZARE
117800971007     C                   EXSR      OPEFIL
117900011022     c*
118000011022     c* reperimento data corrente
118100011022     C                   TIME                    n14              14 0          *ora (6)+ data (8)
118200011022     C                   MOVE      n14           n8                8 0          *data (8) in g/m/a
118300011022     C                   Z-ADD     n8            g08dat
118400011022     C                   Z-ADD     *zeros        g08inv
118500011022     C                   MOVEL     '0'           g08err
118600011022     c                   CALL      'XSRDA8'
118700011022     c                   PARM                    wlbda8
118800011022     C                   Z-ADD     g08inv        datcor            8 0          *Data corrente a/m/g
118900971007     C*
119000971007     C* CARICA LE TABELLE
119100971007     C                   EXSR      CARTAB
119200971007     C*
119300971007     C* CONTROLLA SE RICHIESTE PARZIALIZZAZIONI
119400971007IF  1C     PARTRI        IFGT      *ZEROS
119500971007     C     PARTRF        ORGT      *ZEROS
119600971007     C     PARTFP        ORGT      *ZEROS
119700971007     C                   MOVEL     'S'           PARZ                           *SI PARZIALIZZAZIONI
119800971007E   1C                   ENDIF
119900971007     C*
120000971007     C                   ENDSR
120100971007     O*------------------------------------------------------------------------*
120200971007**   DEC - DECODIFICHE
120300971007NUMERO TRAINO                                     1
120400971007LINEA DI PARTENZA                                 2
120500971007TRAINO:                                           3
120600971007PARTENZA:                                         4
120700971007TOTALE TRAINO                                     5
120800971007TOTALE PARTENZA                                   6
120900971007TOTALE GENERALE                                   7
1210009710078
1211009710079
12120097100710
