000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200960321     H* FNLS08R *-----------------------------------------------------*
000300960321     H*  LETTERA COMUNICAZIONE VARIAZIONE PESO AI MITTENTI  -BATCH-   **
000400960321     H*---------------------------------------------------------------*
000500960326     FFNAWK01L  UF A E           K DISK    USROPN
000600960326     FFNAWK02L  IF   E           K DISK    USROPN
000700960322     F                                     RENAME(FNAWK000:FNAWK002)
000800131014     FFIARBF2C  UF   E           K DISK
000900960322     FFNBLP01L  IF   E           K DISK
001000960328     FLET198    O    F  132        PRINTER OFLIND(*IN05)
001100960328     F                                     USROPN
001200020709
001300020709     d Kaas            s                   like(Arbaas) Inz
001400020709
001500960321     D*
001600960321     D* DEFINIZIONE SCHIERE
001700960327     D L1              S              3  0 DIM(30)
001800960326     D ER              S              1    DIM(201)
001900960326     D ST              S             55    DIM(10) CTDATA PERRCD(1)
002000960326     D CMD             S             43    DIM(1) CTDATA PERRCD(1)
002100960326     D CMD1            S             21    DIM(1) CTDATA PERRCD(1)
002200020709     D CMD2            S             69    DIM(1) CTDATA PERRCD(1)
002300020709     D MSG             S             67    DIM(5) CTDATA PERRCD(1)
002400941223     D*
002500960321     D* PASSAGGIO DATI DAL FILTRO                         - FNLS07R -
002600960321     D DSLS08        E DS                  EXTNAME(FNLS08DS)
002700971121     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
002800960327     D*
002900960327     D* DS PER TRUL06R - CARICAMENTO £1
003000960327     D DSUL06        E DS                  EXTNAME(TRUL06DS)
003100960327     D  LIN                    1     90  0
003200960327     D                                     DIM(30)
003300960325     D***
003400960325     D* PARAM PER STAMPA LETTERE - FNLV45C E TUTTI GLI ALTRI PREVISTI
003500981116     D DSLV45        E DS                  EXTNAME(FNLV45DS)
003600960325     D                 DS
003700960328     D  GG                     1      2  0
003800960325     D  MM                     3      4  0
003900960328     D  AA                     5      8  0
004000960325     D  WDATA                  1      8  0
004100960325     D*
004200960325     D                 DS
004300960325     D  MES                    1      2  0
004400960325     D  GIO                    3      4  0
004500960325     D  AWKMGS                 1      4  0
004600900517     D KPJBA         E DS
004700020709
004800020709      * Parametri per pgm Trul29c
004900020709     d Tr29obj         s             10    inz('FNAWK00F  ')
005000020709     d Tr29typ         s              8    inz('*FILE   ')
005100020709     d Tr29lib         s             10    inz
005200960321     C*****************************************************************
005300960321     C* RIEPILOGO INDICATORI
005400960321     C*****************************************************************
005500960328     C* 05    - INDICATORE DI OVERFLOW
005600960326     C* 12    - NUMERO SERIE <> 0  --->  STAMPO TRATTINO SEPARATORE
005700960322     C* 15    - RECORD DA SCRIVERE NEL FILE DI WORK
005800960326     C* 17    - USATO PER NON STAMPARE LA 1° VOLTA IL FINALE LETTERA
005900960326     C* 30/32 - DI COMODO
006000960327     C* 35    - USATO PER LOKUP
006100960326     C* 90    - ERRORE DEL QCMDEXC SUL FILE PRINTER
006200020709     C* 99    - Errore esco dal Pgm
006300960321     C*****************************************************************
006400960326     C*
006500020709      * Reperisco la libreria in cui si trova il file di work
006600020709     c                   ExSr      RtnLib
006700020709     c   99              GoTo      Fine
006800020709      * Creo il file di work in qtemp
006900020709     c                   ExSr      CrtWrk
007000020709     c   99              GoTo      Fine
007100960326     C****  ESEGUO CLEAR DEL FILE DI WORK  ****
007200960326     C                   EXSR      CLRAWK
007300960326     C   99              GOTO      FINE
007400920925     C*
007500960325     C****  E L A B O R A Z I O N E  ****
007600020709     C**!!!*LOVAL        SETLL     FNARBD00
007700020709     c     Kaas          Setgt     Fnarbd00
007800960326     C                   READ      FNARBD00                               30
007900020709     c                   Eval      Kaas = Arbaas
008000020709
008100020709     c     Leggi         Tag
008200020709     c     Karbd1        Setll     Fnarbd00
008300020709     c     Karbd1        Reade     Fnarbd00                               30
008400960322     C*
008500960322    1C     *IN30         DOWEQ     *OFF
008600960322     C*                      - CON LINEA PARTENZA IMMESSA A VIDEO
008700110708    2C     ARBLNP        ifeq      D08LNP
008800960322     C*                      - CHE SONO DA STAMPARE O DA RISTAMPARE
008900960322     C     ARBFST        ANDNE     'N'
009000110708     C* LEGGO tutti i record con causale variazione di peso 8non solo IC)
009100110708    2C***  ARBCVB        IFEQ      'IC'
009200110708   2ac                   if        arbcvb='IC' or arbcvb='IB'
009300960322     C                   CLEAR                   WELAB
009400960322     C*
009500960322     C* RICHIESTA LA STAMPA
009600960322    3C     D08RIS        IFEQ      'NO'
009700960322     C     ARBFST        IFEQ      ' '
009800960322     C                   MOVEL     'S'           WELAB             1
009900960322     C                   ENDIF
010000960322     C*
010100960322   X3C                   ELSE
010200960322     C* RICHIESTA LA RISTAMPA
010300960322     C     ARBFST        IFEQ      'S'
010400960322     C                   MOVEL     'S'           WELAB
010500960322     C                   ENDIF
010600960322    3C                   ENDIF
010700960322     C*
010800960322     C* WELAB = 'S' ---> RECORD DA ELABORARE
010900960322    3C     WELAB         IFEQ      'S'
011000960322     C                   EXSR      WRTAWK
011100960322    3C                   ENDIF
011200960322     C*
011300110708   2aC                   ENDIF
011400110708    2C                   ENDIF
011500110708     c
011600020709     C**!!!              READ      FNARBD00                               30
011700020709     c     Karbd1        Reade     Fnarbd00                               30
011800960322    1C                   ENDDO
011900020709
012000020709      * Devo leggere l'anno successivo
012100020709     c     Kaas          Setgt     Fnarbd00
012200020709     c                   Read      Fnarbd00                               30
012300020709     c                   If        Not *In30
012400020709     c                   Eval      Kaas = Arbaas
012500020709     c                   GoTo      Leggi
012600020709     c                   EndIf
012700960322     C*
012800960325     C****  S T A M P A  ****
012900960322     C     *LOVAL        SETLL     FNAWK02L
013000960322     C                   READ      FNAWK02L                               30
013100960326     C*
013200960326     C* ESEGUO OVRPRTF SUL FILE DI STAMPA PER APERTURA CONDIVISA
013300960326    1C     *IN30         IFEQ      *OFF
013400960326     C                   Z-ADD     43            LUNG             15 5
013500960326     C                   MOVEL     *BLANKS       COMMAN           80
013600960326     C                   MOVEA     CMD(1)        COMMAN
013700960326     C                   CALL      'QCMDEXC'                            90
013800960326     C                   PARM                    COMMAN
013900960326     C                   PARM                    LUNG
014000960326     C*
014100960326     C                   OPEN      LET198
014200960326    1C                   ENDIF
014300960322     C*
014400960326    1C  N90*IN30         DOWEQ     *OFF
014500960326     C                   MOVE      AWKKSC        W0040
014600960322     C*
014700960322     C* SE CLIENTE NON CODIFICATO  --->  ROTTURA PER RAGIONE SOCIALE
014800960322    2C     W0040         IFEQ      8888
014900960322     C*
015000960322     C     AWKRSM        IFNE      WRSM
015100960326     C* STAMPO FINALE LETTERA
015200960328     C   05
015300960328     CAN 17              EXCEPT    FITT
015400960326     C   17              EXCEPT    FINLET
015500960326     C  N17              SETON                                        17
015600960328     C*
015700960326     C* STAMPO INIZIO LETTERA
015800981116     C                   CLEAR                   DSLV45
015900981116     C                   MOVEL     AWKINM        I45VIA
016000981116     C                   MOVEL     AWKLOM        I45CIT
016100981116     C                   MOVEL     AWKCAM        I45CAP
016200981116     C                   MOVEL     AWKNZM        I45NAR
016300981116     C                   Z-ADD     1             I45NCO
016400981116     C                   MOVEL     UDATE8        I45GDS
016500981116     C                   MOVEL     AWKPRM        I45PRO
016600981116     C                   MOVEL     AWKLNP        I45APP
016700981116     C                   MOVEL(P)  AWKRSM        I45RAG
016800981116     C                   MOVEL     'WW'          I45TTX
016900981116     C                   MOVEL     'L'           I45AST
017000981116     C                   MOVEL     'I'           I45LIN
017100981116     C                   MOVEL     'FNLV45R'     I45PGM
017200960326     C                   CALL      'FNLV45C'
017300960326     C                   PARM                    KPJBA
017400981116     C                   PARM                    DSLV45
017500960328     C*
017600960327     C* STAMPO INTESTAZIONE DETTAGLIO
017700960327     C                   EXCEPT    TES
017800960326     C*
017900960322     C                   MOVEL     AWKRSM        WRSM
018000960322     C                   ENDIF
018100960322     C*
018200960322   X2C                   ELSE
018300960322     C* SE CLIENTE     CODIFICATO  --->  ROTTURA PER CODICE CLIENTE
018400960326     C*
018500960326    3C     AWKKSC        IFNE      WKCLI
018600960326     C* STAMPO FINALE LETTERA
018700960328     C   05
018800960328     CAN 17              EXCEPT    FITT
018900960326     C   17              EXCEPT    FINLET
019000960326     C  N17              SETON                                        17
019100960326     C*
019200960326     C* STAMPO INIZIO LETTERA
019300981116     C                   CLEAR                   DSLV45
019400981116     C                   MOVEL     AWKKSC        I45KSC
019500981116     C                   Z-ADD     1             I45NCO
019600981116     C                   MOVEL     UDATE8        I45GDS
019700981116     C                   MOVEL     DATEU8        I45ADS
019800981116     C                   MOVEL     '999'         I45NAR
019900981116     C                   MOVEL     D08KCI        I45KCC
020000981116     C                   MOVEL     'WW'          I45TTX
020100981116     C                   MOVEL     'C'           I45AST
020200981116     C                   MOVEL     'I'           I45LIN
020300981116     C                   MOVEL     'FNLV45R'     I45PGM
020400960325     C                   CALL      'FNLV45C'
020500960325     C                   PARM                    KPJBA
020600981116     C                   PARM                    DSLV45
020700960327     C*
020800960327     C* STAMPO INTESTAZIONE DETTAGLIO
020900960327     C                   EXCEPT    TES
021000960325     C*
021100960326     C                   MOVEL     AWKKSC        WKCLI
021200960325    3C                   ENDIF
021300960322    2C                   ENDIF
021400960326     C*
021500960326     C* STAMPO DETTAGLIO
021600960326     C                   EXSR      STADET
021700960322     C*
021800960326     C* SOLO PER STAMPA: AGGIORNO IL FLAG DI STAMPA
021900960326    1C     D08RIS        IFEQ      'NO'
022000960326     C     KARBD         CHAIN     FNARBD00                           31
022100960326     C     *IN31         IFEQ      *OFF
022200960326     C                   MOVEL     'S'           ARBFST
022300960326     C                   UPDATE    FNARBD00
022400960326     C                   ENDIF
022500960326    1C                   ENDIF
022600960326     C*
022700960322     C                   READ      FNAWK02L                               30
022800960322    1C                   ENDDO
022900960322     C*
023000960326     C* STAMPO FINALE LETTERA
023100960328     C   05
023200960328     CAN 17              EXCEPT    FITT
023300960326     C   17              EXCEPT    FINLET
023400960326     C*
023500960326     C     FINE          TAG
023600000000     C                   SETON                                        LR
023700920925     C*
023800020709      *---------------------------------------------------------------*
023900020709      *   Reperisco la libreria del file di work
024000020709      *---------------------------------------------------------------*
024100020709     c     RtnLib        BegSr
024200020709
024300020709     c                   Call      'TRUL29C'
024400020709     c                   Parm                    Tr29Obj
024500020709     c                   Parm                    Tr29Typ
024600020709     c                   Parm                    Tr29Lib
024700020709      * Se la libreria ritorna a blank vuol dire che non ha trovato il file
024800020709      * in questo caso chiudo il pgm e invio un msg alla coda dell'opratore
024900020709    1c                   If        Tr29Lib = *blanks
025000020709     c                   clear                   Er
025100020709     c                   Movea     Msg(4)        Er(1)
025200020709     c                   Movea     Msg(3)        Er(135)
025300020709     c                   Movea     Er            Comm
025400020709     c                   z-add     201           Lung
025500020709     c                   Call      'QCMDEXC'
025600020709     c                   Parm                    Comm
025700020709     c                   Parm                    Lung
025800020709     c                   Eval      *In99 = *On
025900020709    1c                   EndIf
026000020709
026100020709     c                   EndSr
026200020709      *---------------------------------------------------------------*
026300020709      *   Creo il file di work in qtemp
026400020709      *---------------------------------------------------------------*
026500020709     c     CrtWrk        BegSr
026600020709     c                   Z-add     69            Lung
026700020709     c                   Clear                   comman           80
026800020709     c                   Movea     Cmd2(1)       comman
026900020709     c                   Eval      %Subst(Comman:31:10) = Tr29Lib
027000020709     c                   Call      'QCMDEXC'                            99
027100020709     c                   Parm                    Comman
027200020709     c                   Parm                    Lung
027300020709      * Duplica oggetti non andata a buon fine
027400020709      * in questo caso chiudo il pgm e invio un msg alla coda dell'opratore
027500020709    1c                   If        *In99
027600020709     c                   Clear                   Er
027700020709     c                   Movea     Msg(5)        Er(1)
027800020709     c                   Movea     Msg(3)        Er(135)
027900020709     c                   Movea     Er            Comm
028000020709     c                   Z-add     201           Lung
028100020709     c                   Call      'QCMDEXC'
028200020709     c                   Parm                    Comm
028300020709     c                   Parm                    Lung
028400020709    1c                   EndIf
028500020709
028600020709     c                   EndSr
028700960326     C*--- CLEAR FILE DI WORK ----------------------------------------*
028800960326     C     CLRAWK        BEGSR
028900960326     C                   Z-ADD     21            LUNG
029000960326     C                   MOVEL     *BLANKS       COMMAN           80
029100960326     C                   MOVEA     CMD1(1)       COMMAN
029200960326     C                   CALL      'QCMDEXC'                            99
029300960326     C                   PARM                    COMMAN           80
029400960326     C                   PARM                    LUNG             15 5
029500960326     C*
029600960326     C* 99 ON  - CLEAR NON ANDATA A BUON FINE: INVIO MESSAGGIO ALLA
029700960326     C*   CODA DELL'OPERATORE ED ESCO DAL PGM
029800960326    1C     *IN99         IFEQ      *ON
029900960326     C                   MOVEA     MSG(1)        ER(1)
030000960326     C                   MOVEA     MSG(2)        ER(68)
030100960326     C                   MOVEA     MSG(3)        ER(135)
030200960326     C                   MOVEA     ER            COMM            201
030300960326     C                   Z-ADD     201           LUNG
030400960326     C                   CALL      'QCMDEXC'
030500960326     C                   PARM                    COMM
030600960326     C                   PARM                    LUNG
030700960326     C*
030800960326   X1C                   ELSE
030900960326     C                   OPEN      FNAWK01L
031000960326     C                   OPEN      FNAWK02L
031100960326    1C                   ENDIF
031200960326     C                   ENDSR
031300960326     C*
031400960322     C*--- SCRIVO FILE DI WORK ---------------------------------------*
031500960322     C     WRTAWK        BEGSR
031600960322     C*
031700960322     C* AGGANCIO LA BOLLA
031800960322     C     KBLP          CHAIN     FNBLP01L                           31
031900960322     C*
032000960327     C* SCRIVO IL FILE DI WORK SOLO SE ARBPKB E' DIVERSO DA BLPPKF
032100960327    1C     ARBPKB        IFNE      BLPPKF
032200960322     C                   SETON                                        15
032300960322     C*
032400960322     C* IMPOSTO IL CODICE CLIENTE
032500960322    2C     BLPCCM        IFGT      0
032600960322     C                   MOVEL     BLPCCM        WKSC
032700960322   X2C                   ELSE
032800960322     C                   MOVEL     BLPKSC        WKSC
032900960322    2C                   ENDIF
033000960322     C*
033100960322     C* SIA PER STAMPA CHE RISTAMPA: SE IMMESSO CONTROLLO IL CLIENTE
033200960322    2C     D08KSC        IFGT      0
033300960322     C     D08KSC        ANDNE     WKSC
033400960322     C                   SETOFF                                       15
033500960322    2C                   ENDIF
033600960322     C*
033700960322    2C   15D08RIS        IFEQ      'SI'
033800960322     C* SOLO PER RISTAMPA: SE IMMESSA CONTROLLO LA DATA VARIAZIONE
033900960322    3C     D08DVD        IFGT      0
034000960322     C     ARBDTV        IFLT      D08DVD
034100960322     C     ARBDTV        ORGT      D08DVA
034200960322     C                   SETOFF                                       15
034300960322     C                   ENDIF
034400960322    3C                   ENDIF
034500960322     C* SOLO PER RISTAMPA: SE IMMESSA CONTROLLO LA LINEA DI ARRIVO
034600960322    3C   15D08LNA        IFGT      0
034700960322     C     D08LNA        ANDNE     BLPLNA
034800960322     C                   SETOFF                                       15
034900960322    3C                   ENDIF
035000960322    2C                   ENDIF
035100960322     C*
035200960322     C* 15 ON  - RECORD DA SCRIVERE NEL FILE DI WORK
035300960322    2C     *IN15         IFEQ      *ON
035400960322     C*
035500960322    3C     D08RIS        IFEQ      'NO'
035600960327     C* SE TROVO PIU' VARIAZIONI PESO NON ANCORA STAMPATE (ARBFST=' ')
035700960327     C*   PER LA STESSA SPEDIZIONE, STAMPO SOLO LA 1° PERCHE' CONTIENE
035800960327     C*   IL PESO ORIGINALE CON CUI LA BOLLA E' STATA BOLLETTATA
035900960322     C     KAWK          CHAIN     FNAWK01L                           32
036000960326     C*
036100960327     C* SE TROVO ALTRE VARIAZIONI PER LA STESSA SPEDIZIONE LE FLAGGO
036200960327     C*   PER NON LEGGERLE PIU' NEMMENO SE RICHIESTA LA RISTAMPA
036300960327     C     *IN32         IFEQ      *OFF
036400960327     C                   MOVEL     'N'           ARBFST
036500960327     C                   UPDATE    FNARBD00
036600960327     C                   ENDIF
036700960322     C*
036800960322     C* SE TROVO PIU' VARIAZIONI DI PESO PER LA STESSA SPEDIZIONE
036900960322     C*   GIA' STAMPATE (ARBFST='S'), LE RISTAMPO TUTTE
037000960322   X3C                   ELSE
037100960322     C                   SETON                                        32
037200960322    3C                   ENDIF
037300960322     C*
037400960327    3C     *IN32         IFEQ      *ON
037500960327     C                   MOVEL     ARBAAS        AWKAAS
037600960327     C                   MOVEL     ARBLNP        AWKLNP
037700960327     C                   MOVEL     ARBNRS        AWKNRS
037800960327     C                   MOVEL     ARBNSP        AWKNSP
037900960322     C                   MOVEL     BLPMGS        AWKMGS
038000960329     C                   MOVEL     BLPLNA        AWKLNA
038100960326     C                   MOVEL     WKSC          AWKKSC
038200960327     C*
038300960327     C* CONTROLLO SE CODICE CLIENTE DI ALTRA FILIALE
038400960327     C                   MOVEL     AWKKSC        W0030             3 0
038500960327     C     W0030         LOOKUP    L1                                     35
038600960327     C*
038700960327     C     *IN35         IFEQ      *ON
038800960322     C* PER COMODITA' SE CLIENTE 9999 CI IMPOSTO 8888
038900960322     C                   MOVE      AWKKSC        W0040             4 0
039000960327    4C     W0040         IFEQ      9999
039100960322     C                   MOVE      8888          AWKKSC
039200960327    4C                   ENDIF
039300960327     C*
039400960327     C                   ELSE
039500960327     C* PER COMODITA' SE CLIENTE DI ALTRA FILIALE CI IMPOSTO 8888
039600960327     C                   MOVE      8888          AWKKSC
039700960327     C                   ENDIF
039800960327     C*
039900960322     C                   MOVEL     BLPRSM        AWKRSM
040000960322     C                   MOVEL     BLPINM        AWKINM
040100960322     C                   MOVEL     BLPCAM        AWKCAM
040200960322     C                   MOVEL     BLPLOM        AWKLOM
040300960322     C                   MOVEL     BLPPRM        AWKPRM
040400960322     C                   MOVEL     BLPNZM        AWKNZM
040500960322     C                   MOVEL     BLPRMN        AWKRMN
040600960322     C                   MOVEL     ARBRSD        AWKRSD
040700960322     C                   MOVEL     BLPNCL        AWKNCL
040800960327     C                   MOVEL     BLPPKF        AWKPKF
040900960322     C                   MOVEL     ARBPKB        AWKPKV
041000960322     C                   MOVEL     ARBDTV        AWKDTV
041100960322     C                   MOVEL     ARBORV        AWKORV
041200960329     C                   MOVEL     BLPSOP        AWKSOP
041300960322     C                   WRITE     FNAWK000
041400960322    3C                   ENDIF
041500960326     C*
041600960322    2C                   ENDIF
041700960322     C*
041800960322   X1C                   ELSE
041900960327     C* ARBPKB = BLPPKF ---> FLAGGO IL RECORD PER NON LEGGERLO PIU'
042000960322     C*   NEMMENO SE RICHIESTA LA RISTAMPA
042100960322     C                   MOVEL     'N'           ARBFST
042200960322     C                   UPDATE    FNARBD00
042300960322    1C                   ENDIF
042400960322     C*
042500960322     C                   ENDSR
042600960322     C*
042700960326     C*--- STAMPO DETTAGLIO ------------------------------------------*
042800960326     C     STADET        BEGSR
042900960326     C*
043000960326     C****  STAMPO TESTATA  ****
043100960328     C     *IN05         IFEQ      *ON
043200960327     C                   EXCEPT    FITT
043300960327     C                   EXCEPT    TES
043400960328     C                   SETOFF                                       05
043500960326     C                   ENDIF
043600960326     C*
043700960326     C****  STAMPO DETTAGLIO  ****
043800960326     C* 12 ON  - NUMERO SERIE <> 0  ---> STAMPO TRATTINO SEPARATORE
043900960326     C     AWKNRS        COMP      0                                  1212
044000960326     C*
044100960326     C* GIRO DATA SPEDIZIONE
044200960326     C                   Z-ADD     MES           GGMM
044300960326     C                   MOVEL     GIO           GGMM              4 0
044400960326     C                   Z-ADD     AWKAAS        STADSP
044500960327     C                   MOVEL     GGMM          STADSP            6 0
044600960326     C*
044700960326     C* RAGIONE SOCIALE DESTINATARIO
044800960326     C                   MOVEL     AWKRSD        STARSD           15
044900960326     C*
045000960326     C                   EXCEPT    DETT
045100960326     C                   ENDSR
045200960326     C*
045300960321     C*--- SR INIZIALE -----------------------------------------------*
045400960321     C     *INZSR        BEGSR
045500960321     C****
045600960321     C** KLIST
045700960321     C****
045800960322     C* ACCESSO FNBLP01L
045900960322     C     KBLP          KLIST
046000960322     C                   KFLD                    ARBAAS
046100960322     C                   KFLD                    ARBLNP
046200960322     C                   KFLD                    ARBNRS
046300960322     C                   KFLD                    ARBNSP
046400960322     C* ACCESSO FNAWK01L
046500960322     C     KAWK          KLIST
046600960322     C                   KFLD                    ARBAAS
046700960322     C                   KFLD                    ARBLNP
046800960322     C                   KFLD                    ARBNRS
046900960322     C                   KFLD                    ARBNSP
047000131014     C* ACCESSO FIARBF2C
047100960326     C     KARBD         KLIST
047200960326     C                   KFLD                    AWKAAS
047300960326     C                   KFLD                    AWKLNP
047400960326     C                   KFLD                    AWKNRS
047500960326     C                   KFLD                    AWKNSP
047600960326     C                   KFLD                    AWKDTV
047700960326     C                   KFLD                    AWKORV
047800020709
047900020709     c     Karbd1        Klist
048000020709     c                   Kfld                    Kaas
048100020709     c                   Kfld                    D08lnp
048200020709
048300960326     C****
048400960321     C** DEFINIZIONE CAMPI
048500960321     C****
048600960326     C     *LIKE         DEFINE    AWKKSC        WKSC
048700960326     C     *LIKE         DEFINE    AWKKSC        WKCLI
048800960326     C     *LIKE         DEFINE    AWKRSM        WRSM
048900960321     C*
049000960325     C****  PRENDO DATA E ORA CORRENTE  ****
049100960325     C                   TIME                    W0140            14 0
049200960325     C                   MOVE      W0140         UDATE8            8 0
049300960325     C* GIRO DATA DEL GIORNO
049400960325     C                   Z-ADD     UDATE8        WDATA
049500960325     C                   Z-ADD     GG            MMGG
049600960325     C                   MOVEL     MM            MMGG              4 0
049700960325     C                   Z-ADD     MMGG          DATEU8
049800960325     C                   MOVEL     AA            DATEU8            8 0
049900960327     C*
050000960326     C****  PARAMETRI PASSATI  ****
050100960326     C     *ENTRY        PLIST
050200960326     C                   PARM                    KPJBA
050300960326     C                   MOVEL     KPJBU         DSLS08
050400971121     C*
050500971121     C                   Z-ADD     1             CODUT
050600971121     C                   CALL      'X§PARUT'
050700971121     C                   PARM                    UT§DSE
050800971124     C                   MOVEL     RAGUT         RSUTP            24
050900960325     C****  DATA VARIAZIONE  ****
051000960325     C* SE DATA VARIAZIONE AL = 0 --> METTO DATA DAL IN DATA AL
051100960325    1C     D08DVD        IFGT      0
051200960325     C     D08DVA        ANDEQ     0
051300960325     C                   Z-ADD     D08DVD        D08DVA
051400960325    1C                   ENDIF
051500960327     C*
051600960327     C****  CARICO TABELLA FILIALI GESTITE £1  ****
051700960327     C                   CLEAR                   DSUL06
051800960327     C                   MOVE      '£1'          D06COD
051900960327     C                   MOVEL     D08FEL        D06KEY
052000960327     C                   MOVEL     DSUL06        KPJBU
052100960327     C                   CALL      'TRUL06R'
052200960327     C                   PARM                    KPJBA
052300960327     C                   MOVEL     KPJBU         DSUL06
052400960327     C                   MOVEA     LIN           L1
052500920925     C                   ENDSR
052600960326     C*
052700960325     O*---------------------------------------------------------------*
052800960326     O* INTESTAZIONE RIGHE DETTAGLIO
052900960326     OLET198    E            TES         2
053000960326     O                       ST(1)               55
053100960326     O                       ST(4)              110
053200960326     O          E            TES         1
053300960326     O                       ST(2)               55
053400960326     O                       ST(5)              110
053500960326     O* DETTAGLIO
053600960326     O          E            DETT        1
053700960327     O                       AWKLNP              22
053800960327     O                     12AWKNRS        Z     25
053900960327     O                     12                    26 '-'
054000960327     O                       AWKNSP        Z     33
054100960327     O                       STADSP              42 '  /  /  '
054200960326     O                       AWKRMN        Z     58
054300960326     O                       STARSD              75
054400960326     O                       AWKNCL        Z     82
054500960327     O                       AWKPKV        4     92
054600960403     O                       AWKPKF        4    105
054700960326     O* FINE LETTERA
054800960325     O          E            FINLET      2
054900960326     O                       ST(7)               55
055000960326     O                       ST(8)              110
055100960326     O          E            FINLET      2
055200960326     O                       ST(9)               55
055300960326     O                       ST(10)             110
055400960325     O          E            FINLET      3
055500971124     O                       RSUTP              106
055600960326     O* EXCPT FITTIZIA PER SALTO PAGINA
055700960326     O          E            FITT             09
055800960326     O                                          110 ' '
055900960326     O*---------------------------------------------------------------*
056000960326** ST
056100960401                   Ns.spedizione   Data     Vs.riferime1
056200960326                   ------------------------------------2
056300960327                   xxx xx-xxxxxxx xx/xx/xx xxxxxxxxxxxx3
056400981125nto  Destinatario     Colli  Kg. DDT   Kg.riscontrati  4
056500960403-----------------------------------------------------  5
056600960403xxx  xxxxxxxxxxxxxxx  xxxxx  xxxxxx,x     xxxxxx,x     6
056700960326                            I nostri Uffici rimangono a7
056800960326 Vs. disposizione per qualsiasi chiarimento.           8
056900960326                            L'occasione ci è gradita pe9
057000960326r porgerVi distinti saluti.                            10
057100960326** CMD
057200960401OVRPRTF FILE(LET198) SAVE(*NO)  SHARE(*YES)                           1
057300960326** CMD1
057400960326CLRPFM FILE(FNAWK00F)                                                 1
057500020709** CMD2
057600020709CRTDUPOBJ OBJ(FNAWK*) FROMLIB(XXXXXXXXXX) OBJTYPE(*FILE) TOLIB(QTEMP)
057700960326** MSG
057800960326SNDMSG MSG('ATTENZIONE!! FILE FNAWK00F GIA'' UTILIZZATO DA UN ALTRO   1
057900960328 LAVORO !!')                                                          2
058000960326                                                    TOUSR(*SYSOPR)    3
058100020709SNDMSG MSG('ATTENZIONE!! FILE FNAWK00F NON TROVATO NEL SISTEMA!!')    4
058200020709SNDMSG MSG('ATTENZIONE!! FILE FNAWK00F NON DUPLICATO IN QTEMP !!')    5
