000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200960321     H* FNLS08R *-----------------------------------------------------*
000300960321     H*  LETTERA COMUNICAZIONE VARIAZIONE PESO AI MITTENTI  -BATCH-   **
000400100107     H*          oppure
000500100107     H*  CREAZIONE file TIVGD00F per la trasmissione della variazione
000600100107     H*           per ora solo per la WURTH
000700960321     H*---------------------------------------------------------------*
000800960326     FFNAWK01L  UF A E           K DISK    USROPN
000900960326     FFNAWK02L  IF   E           K DISK    USROPN
001000960322     F                                     RENAME(FNAWK000:FNAWK002)
001100991005     FFIARBF1C  UF   E           K DISK
001200960322     FFNBLP01L  IF   E           K DISK
001300100107     FTIVGD00F  o    E             DISK
001400960328     FLET198    O    F  132        PRINTER OFLIND(*IN05)
001500960328     F                                     USROPN
001600020709
001700020709     d Kaas            s                   like(Arbaas) Inz
001800960321     D*
001900960321     D* DEFINIZIONE SCHIERE
002000100105     D** L1              S              3  0 DIM(30)
002100960326     D ER              S              1    DIM(201)
002200960326     D ST              S             55    DIM(10) CTDATA PERRCD(1)
002300960326     D CMD             S             43    DIM(1) CTDATA PERRCD(1)
002400960326     D CMD1            S             21    DIM(1) CTDATA PERRCD(1)
002500020709     D CMD2            S             69    DIM(1) CTDATA PERRCD(1)
002600020709     D MSG             S             67    DIM(5) CTDATA PERRCD(1)
002700941223     D*
002800100105     D darbpkf_i     E DS
002900100105
003000100105     D* PASSAGGIO DATI DAL FILTRO                         - FNLS07R -
003100100105     D DSLS08        E DS                  EXTNAME(FNLS08DS)
003200100105
003300971121     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
003400960325     D***
003500960325     D* PARAM PER STAMPA LETTERE - FNLV45C E TUTTI GLI ALTRI PREVISTI
003600981116     D DSLV45        E DS                  EXTNAME(FNLV45DS)
003700960325     D                 DS
003800960328     D  GG                     1      2  0
003900960325     D  MM                     3      4  0
004000960328     D  AA                     5      8  0
004100960325     D  WDATA                  1      8  0
004200960325     D*
004300960325     D                 DS
004400960325     D  MES                    1      2  0
004500960325     D  GIO                    3      4  0
004600960325     D  AWKMGS                 1      4  0
004700100107     d
004800900517     D KPJBA         E DS
004900100107     D tibs10ds      E DS
005000100107     d  skfigli               21   5520  0 dim(500)
005100020709
005200020709      * Parametri per pgm Trul29c
005300020709     d Tr29obj         s             10    inz('FNAWK00F  ')
005400020709     d Tr29typ         s              8    inz('*FILE   ')
005500020709     d Tr29lib         s             10    inz
005600100107     d w0110           s             11  0 inz
005700100107     d wunif           s              8    inz
005800960321     C*****************************************************************
005900960321     C* RIEPILOGO INDICATORI
006000960321     C*****************************************************************
006100960328     C* 05    - INDICATORE DI OVERFLOW
006200960326     C* 12    - NUMERO SERIE <> 0  --->  STAMPO TRATTINO SEPARATORE
006300960322     C* 15    - RECORD DA SCRIVERE NEL FILE DI WORK
006400960326     C* 17    - USATO PER NON STAMPARE LA 1° VOLTA IL FINALE LETTERA
006500960326     C* 30/32 - DI COMODO
006600960327     C* 35    - USATO PER LOKUP
006700960326     C* 90    - ERRORE DEL QCMDEXC SUL FILE PRINTER
006800020709     C* 99    - Errore esco dal Pgm
006900960321     C*****************************************************************
007000100105     c
007100100105     c* I valori  del falg ARBFST sono:
007200100105     c* " " -->  ancora da elaborare
007300100105     c* "F" -->  elaborato solo file
007400100105     c* "S" -->  elaborata solo stampa
007500100105     c* "E" -->  elaborate entrambe
007600100105     c
007700100107     c* In caso di elaborazione file non è obbligatoria la linea di
007800100105     c*  partenza ma lo è il codice cliente ( da cui leggerò il padre
007900100105     c*  e tutti i figli collegati )
008000100105     C*****************************************************************
008100960326     C*
008200020709      * Reperisco la libreria in cui si trova il file di work
008300100105     c
008400020709     c                   ExSr      RtnLib
008500020709     c   99              GoTo      Fine
008600020709      * Creo il file di work in qtemp
008700020709     c                   ExSr      CrtWrk
008800020709     c   99              GoTo      Fine
008900960326     C****  ESEGUO CLEAR DEL FILE DI WORK  ****
009000960326     C                   EXSR      CLRAWK
009100960326     C   99              GOTO      FINE
009200100105     C
009300920925     C*
009400960325     C****  E L A B O R A Z I O N E  ****
009500020709     c     Kaas          Setgt     Fnarbd00
009600960326     C                   READ      FNARBD00                               30
009700020709     c                   Eval      Kaas = Arbaas
009800020709
009900020709     c     Leggi         Tag
010000100107     C                   IF        D08LNP>0
010100020709     c     Karbd1        Setll     Fnarbd00
010200020709     c     Karbd1        Reade     Fnarbd00                               30
010300100107     C                   ELSE
010400100107     c     KAAS          Setll     Fnarbd00
010500100107     c     KAAS          Reade     Fnarbd00                               30
010600100107     C                   ENDIF
010700960322     C*
010800960322    1C     *IN30         DOWEQ     *OFF
010900960322     C* LEGGO SOLO I RECORD: - CON CAUSALE VARIAZIONE "IC"
011000960322    2C     ARBCVB        IFEQ      'IC'
011100960322     C*                      - CHE SONO DA STAMPARE O DA RISTAMPARE
011200960322     C     ARBFST        ANDNE     'N'
011300100105     c
011400100105     C*                      - CON LINEA PARTENZA IMMESSA A VIDEO
011500100105   2ac                   if        d08lnp= 0 or arblnp=d08lnp
011600960322     C                   CLEAR                   WELAB
011700960322     C*
011800100105     C* RICHIESTA LA stampa
011900100107   3aC                   IF        D08TELA<>'F'
012000960322    3C     D08RIS        IFEQ      'NO'
012100960322     C     ARBFST        IFEQ      ' '
012200100105     C     ARBFST        oreq      'F'
012300960322     C                   MOVEL     'S'           WELAB             1
012400960322     C                   ENDIF
012500960322     C*
012600960322   X3C                   ELSE
012700960322     C* RICHIESTA LA RISTAMPA
012800960322     C     ARBFST        IFEQ      'S'
012900100105     C     ARBFST        oreq      'E'
013000960322     C                   MOVEL     'S'           WELAB
013100960322     C                   ENDIF
013200960322    3C                   ENDIF
013300100107   3aC                   ENDIF
013400100105     c
013500100105     C* RICHIESTO il File
013600100107   3aC                   IF        D08TELA= 'F'
013700100105    3C     D08RIS        IFEQ      'NO'
013800100107    4C     ARBFST        IFEQ      ' '
013900100105     C     ARBFST        oreq      'S'
014000100105     C                   MOVEL     'S'           WELAB             1
014100100107    4C                   ENDIF
014200100105     C*
014300100105   X3C                   ELSE
014400100105     C* RICHIESTA LA rielaborazione
014500100107    4C     ARBFST        IFEQ      'F'
014600100105     C     ARBFST        oreq      'E'
014700100105     C                   MOVEL     'S'           WELAB
014800100107    4C                   ENDIF
014900100105    3C                   ENDIF
015000100107   3aC                   ENDIF
015100960322     C*
015200960322     C* WELAB = 'S' ---> RECORD DA ELABORARE
015300960322    3C     WELAB         IFEQ      'S'
015400960322     C                   EXSR      WRTAWK
015500960322    3C                   ENDIF
015600960322     C*
015700100105   2aC                   ENDIF
015800100105    2C                   ENDIF
015900100105     c
016000100107     C                   IF        D08LNP>0
016100020709     c     Karbd1        Reade     Fnarbd00                               30
016200100107     C                   ELSE
016300100107     c     KAAS          Reade     Fnarbd00                               30
016400100107     C                   ENDIF
016500100107     C
016600960322    1C                   ENDDO
016700020709
016800020709      * Devo leggere l'anno successivo
016900020709     c     Kaas          Setgt     Fnarbd00
017000020709     c                   Read      Fnarbd00                               30
017100020709     c                   If        Not *In30
017200020709     c                   Eval      Kaas = Arbaas
017300020709     c                   GoTo      Leggi
017400020709     c                   EndIf
017500960322     C*
017600960325     C****  S T A M P A  ****
017700100107     c* solo se richiesta
017800100107    0c                   if        d08tela<>'F'
017900100107     c
018000960322     C     *LOVAL        SETLL     FNAWK02L
018100960322     C                   READ      FNAWK02L                               30
018200960326     C*
018300960326     C* ESEGUO OVRPRTF SUL FILE DI STAMPA PER APERTURA CONDIVISA
018400960326    1C     *IN30         IFEQ      *OFF
018500960326     C                   Z-ADD     43            LUNG             15 5
018600960326     C                   MOVEL     *BLANKS       COMMAN           80
018700960326     C                   MOVEA     CMD(1)        COMMAN
018800960326     C                   CALL      'QCMDEXC'                            90
018900960326     C                   PARM                    COMMAN
019000960326     C                   PARM                    LUNG
019100960326     C*
019200960326     C                   OPEN      LET198
019300960326    1C                   ENDIF
019400960322     C*
019500960326    1C  N90*IN30         DOWEQ     *OFF
019600960326     C                   MOVE      AWKKSC        W0040
019700960322     C*
019800960322     C* SE CLIENTE NON CODIFICATO  --->  ROTTURA PER RAGIONE SOCIALE
019900960322    2C     W0040         IFEQ      8888
020000960322     C*
020100960322     C     AWKRSM        IFNE      WRSM
020200960326     C* STAMPO FINALE LETTERA
020300960328     C   05
020400960328     CAN 17              EXCEPT    FITT
020500960326     C   17              EXCEPT    FINLET
020600960326     C  N17              SETON                                        17
020700960328     C*
020800960326     C* STAMPO INIZIO LETTERA
020900981116     C                   CLEAR                   DSLV45
021000981116     C                   MOVEL     AWKINM        I45VIA
021100981116     C                   MOVEL     AWKLOM        I45CIT
021200981116     C                   MOVEL     AWKCAM        I45CAP
021300981116     C                   MOVEL     AWKNZM        I45NAR
021400981116     C                   Z-ADD     1             I45NCO
021500981116     C                   MOVEL     UDATE8        I45GDS
021600981116     C                   MOVEL     AWKPRM        I45PRO
021700981116     C                   MOVEL     AWKLNP        I45APP
021800981116     C                   MOVEL(P)  AWKRSM        I45RAG
021900981116     C                   MOVEL     'WW'          I45TTX
022000981116     C                   MOVEL     'L'           I45AST
022100981116     C                   MOVEL     'I'           I45LIN
022200981116     C                   MOVEL     'FNLV45R'     I45PGM
022300960326     C                   CALL      'FNLV45C'
022400960326     C                   PARM                    KPJBA
022500981116     C                   PARM                    DSLV45
022600960328     C*
022700960327     C* STAMPO INTESTAZIONE DETTAGLIO
022800960327     C                   EXCEPT    TES
022900960326     C*
023000960322     C                   MOVEL     AWKRSM        WRSM
023100960322     C                   ENDIF
023200960322     C*
023300960322   X2C                   ELSE
023400960322     C* SE CLIENTE     CODIFICATO  --->  ROTTURA PER CODICE CLIENTE
023500960326     C*
023600960326    3C     AWKKSC        IFNE      WKCLI
023700960326     C* STAMPO FINALE LETTERA
023800960328     C   05
023900960328     CAN 17              EXCEPT    FITT
024000960326     C   17              EXCEPT    FINLET
024100960326     C  N17              SETON                                        17
024200960326     C*
024300960326     C* STAMPO INIZIO LETTERA
024400981116     C                   CLEAR                   DSLV45
024500981116     C                   MOVEL     AWKKSC        I45KSC
024600981116     C                   Z-ADD     1             I45NCO
024700981116     C                   MOVEL     UDATE8        I45GDS
024800981116     C                   MOVEL     DATEU8        I45ADS
024900981116     C                   MOVEL     '999'         I45NAR
025000981116     C                   MOVEL     D08KCI        I45KCC
025100981116     C                   MOVEL     'WW'          I45TTX
025200981116     C                   MOVEL     'C'           I45AST
025300981116     C                   MOVEL     'I'           I45LIN
025400981116     C                   MOVEL     'FNLV45R'     I45PGM
025500960325     C                   CALL      'FNLV45C'
025600960325     C                   PARM                    KPJBA
025700981116     C                   PARM                    DSLV45
025800960327     C*
025900960327     C* STAMPO INTESTAZIONE DETTAGLIO
026000960327     C                   EXCEPT    TES
026100960325     C*
026200960326     C                   MOVEL     AWKKSC        WKCLI
026300960325    3C                   ENDIF
026400960322    2C                   ENDIF
026500960326     C*
026600960326     C* STAMPO DETTAGLIO
026700960326     C                   EXSR      STADET
026800960322     C*
026900960326     C* SOLO PER STAMPA: AGGIORNO IL FLAG DI STAMPA
027000960326    1C     D08RIS        IFEQ      'NO'
027100960326     C     KARBD         CHAIN     FNARBD00                           31
027200960326     C     *IN31         IFEQ      *OFF
027300100107     c                   if        arbfst=' '
027400960326     C                   MOVEL     'S'           ARBFST
027500100107     c                   else
027600100107     C                   MOVEL     'E'           ARBFST
027700100107     c                   endif
027800960326     C                   UPDATE    FNARBD00
027900960326     C                   ENDIF
028000960326    1C                   ENDIF
028100960326     C*
028200960322     C                   READ      FNAWK02L                               30
028300960322    1C                   ENDDO
028400960322     C*
028500960326     C* STAMPO FINALE LETTERA
028600960328     C   05
028700960328     CAN 17              EXCEPT    FITT
028800960326     C   17              EXCEPT    FINLET
028900960326     C*
029000100107    0c                   endif
029100100107     c
029200960326     C     FINE          TAG
029300000000     C                   SETON                                        LR
029400920925     C*
029500020709      *---------------------------------------------------------------*
029600020709      *   Reperisco la libreria del file di work
029700020709      *---------------------------------------------------------------*
029800020709     c     RtnLib        BegSr
029900020709
030000020709     c                   Call      'TRUL29C'
030100020709     c                   Parm                    Tr29Obj
030200020709     c                   Parm                    Tr29Typ
030300020709     c                   Parm                    Tr29Lib
030400020709      * Se la libreria ritorna a blank vuol dire che non ha trovato il file
030500020709      * in questo caso chiudo il pgm e invio un msg alla coda dell'opratore
030600020709    1c                   If        Tr29Lib = *blanks
030700020709     c                   clear                   Er
030800020709     c                   Movea     Msg(4)        Er(1)
030900020709     c                   Movea     Msg(3)        Er(135)
031000020709     c                   Movea     Er            Comm
031100020709     c                   z-add     201           Lung
031200020709     c                   Call      'QCMDEXC'
031300020709     c                   Parm                    Comm
031400020709     c                   Parm                    Lung
031500020709     c                   Eval      *In99 = *On
031600020709    1c                   EndIf
031700020709
031800020709     c                   EndSr
031900020709      *---------------------------------------------------------------*
032000020709      *   Creo il file di work in qtemp
032100020709      *---------------------------------------------------------------*
032200020709     c     CrtWrk        BegSr
032300020709     c                   Z-add     69            Lung
032400020709     c                   Clear                   comman           80
032500020709     c                   Movea     Cmd2(1)       comman
032600020709     c                   Eval      %Subst(Comman:31:10) = Tr29Lib
032700020709     c                   Call      'QCMDEXC'                            99
032800020709     c                   Parm                    Comman
032900020709     c                   Parm                    Lung
033000020709      * Duplica oggetti non andata a buon fine
033100020709      * in questo caso chiudo il pgm e invio un msg alla coda dell'opratore
033200020709    1c                   If        *In99
033300020709     c                   Clear                   Er
033400020709     c                   Movea     Msg(5)        Er(1)
033500020709     c                   Movea     Msg(3)        Er(135)
033600020709     c                   Movea     Er            Comm
033700020709     c                   Z-add     201           Lung
033800020709     c                   Call      'QCMDEXC'
033900020709     c                   Parm                    Comm
034000020709     c                   Parm                    Lung
034100020709    1c                   EndIf
034200020709
034300020709     c                   EndSr
034400960326     C*--- CLEAR FILE DI WORK ----------------------------------------*
034500960326     C     CLRAWK        BEGSR
034600960326     C                   Z-ADD     21            LUNG
034700960326     C                   MOVEL     *BLANKS       COMMAN           80
034800960326     C                   MOVEA     CMD1(1)       COMMAN
034900960326     C                   CALL      'QCMDEXC'                            99
035000960326     C                   PARM                    COMMAN           80
035100960326     C                   PARM                    LUNG             15 5
035200960326     C*
035300960326     C* 99 ON  - CLEAR NON ANDATA A BUON FINE: INVIO MESSAGGIO ALLA
035400960326     C*   CODA DELL'OPERATORE ED ESCO DAL PGM
035500960326    1C     *IN99         IFEQ      *ON
035600960326     C                   MOVEA     MSG(1)        ER(1)
035700960326     C                   MOVEA     MSG(2)        ER(68)
035800960326     C                   MOVEA     MSG(3)        ER(135)
035900960326     C                   MOVEA     ER            COMM            201
036000960326     C                   Z-ADD     201           LUNG
036100960326     C                   CALL      'QCMDEXC'
036200960326     C                   PARM                    COMM
036300960326     C                   PARM                    LUNG
036400960326     C*
036500960326   X1C                   ELSE
036600960326     C                   OPEN      FNAWK01L
036700960326     C                   OPEN      FNAWK02L
036800960326    1C                   ENDIF
036900960326     C                   ENDSR
037000960326     C*
037100960322     C*--- SCRIVO FILE DI WORK ---------------------------------------*
037200960322     C     WRTAWK        BEGSR
037300960322     C*
037400960322     C* AGGANCIO LA BOLLA
037500960322     C     KBLP          CHAIN     FNBLP01L                           31
037600960322     C*
037700960327     C* SCRIVO IL FILE DI WORK SOLO SE ARBPKB E' DIVERSO DA BLPPKF
037800960327    1C     ARBPKB        IFNE      BLPPKF
037900960322     C                   SETON                                        15
038000960322     C*
038100960322     C* IMPOSTO IL CODICE CLIENTE
038200960322    2C     BLPCCM        IFGT      0
038300960322     C                   MOVEL     BLPCCM        WKSC
038400960322   X2C                   ELSE
038500960322     C                   MOVEL     BLPKSC        WKSC
038600960322    2C                   ENDIF
038700960322     C*
038800100107     c                   if        d08tela<>'F'
038900100107     C* SIA PER STAMPA CHE RISTAMPA: SE IMMESSO CONTROLLO IL CLIENTE
039000960322    2C     D08KSC        IFGT      0
039100960322     C     D08KSC        ANDNE     WKSC
039200960322     C                   SETOFF                                       15
039300960322    2C                   ENDIF
039400100107     c
039500100107     c                   else
039600100107     c
039700100107     c* Per FILE   il codice deve essere quello passato o uno dei figli
039800100107     c                   eval      w0110   = wksc
039900100107     c                   if        %lookup(w0110:skfigli) = *zeros
040000100107     c                   setoff                                       15
040100100107     c                   endif
040200100107
040300100107     c                   endif
040400960322     C*
040500960322    2C   15D08RIS        IFEQ      'SI'
040600960322     C* SOLO PER RISTAMPA: SE IMMESSA CONTROLLO LA DATA VARIAZIONE
040700960322    3C     D08DVD        IFGT      0
040800960322     C     ARBDTV        IFLT      D08DVD
040900960322     C     ARBDTV        ORGT      D08DVA
041000960322     C                   SETOFF                                       15
041100960322     C                   ENDIF
041200960322    3C                   ENDIF
041300100107
041400960322     C* SOLO PER RISTAMPA: SE IMMESSA CONTROLLO LA LINEA DI ARRIVO
041500960322    3C   15D08LNA        IFGT      0
041600960322     C     D08LNA        ANDNE     BLPLNA
041700960322     C                   SETOFF                                       15
041800960322    3C                   ENDIF
041900960322    2C                   ENDIF
042000960322     C*
042100960322     C* 15 ON  - RECORD DA SCRIVERE NEL FILE DI WORK
042200960322    2C     *IN15         IFEQ      *ON
042300960322     C*
042400960322    3C     D08RIS        IFEQ      'NO'
042500960327     C* SE TROVO PIU' VARIAZIONI PESO NON ANCORA STAMPATE (ARBFST=' ')
042600960327     C*   PER LA STESSA SPEDIZIONE, STAMPO SOLO LA 1° PERCHE' CONTIENE
042700960327     C*   IL PESO ORIGINALE CON CUI LA BOLLA E' STATA BOLLETTATA
042800960322     C     KAWK          CHAIN     FNAWK01L                           32
042900960326     C*
043000960327     C* SE TROVO ALTRE VARIAZIONI PER LA STESSA SPEDIZIONE LE FLAGGO
043100960327     C*   PER NON LEGGERLE PIU' NEMMENO SE RICHIESTA LA RISTAMPA
043200960327     C     *IN32         IFEQ      *OFF
043300960327     C                   MOVEL     'N'           ARBFST
043400960327     C                   UPDATE    FNARBD00
043500960327     C                   ENDIF
043600960322     C*
043700960322     C* SE TROVO PIU' VARIAZIONI DI PESO PER LA STESSA SPEDIZIONE
043800960322     C*   GIA' STAMPATE (ARBFST='S'), LE RISTAMPO TUTTE
043900960322   X3C                   ELSE
044000960322     C                   SETON                                        32
044100960322    3C                   ENDIF
044200960322     C*
044300960327    3C     *IN32         IFEQ      *ON
044400960327     C                   MOVEL     ARBAAS        AWKAAS
044500960327     C                   MOVEL     ARBLNP        AWKLNP
044600960327     C                   MOVEL     ARBNRS        AWKNRS
044700960327     C                   MOVEL     ARBNSP        AWKNSP
044800960322     C                   MOVEL     BLPMGS        AWKMGS
044900960329     C                   MOVEL     BLPLNA        AWKLNA
045000960326     C                   MOVEL     WKSC          AWKKSC
045100960327     C*
045200960327     C* CONTROLLO SE CODICE CLIENTE DI ALTRA FILIALE
045300100105     c* non più necessario
045400100105     C***                MOVEL     AWKKSC        W0030             3 0
045500100105     C***  W0030         LOOKUP    L1                                     35
045600960327     C*
045700100105     C***  *IN35         IFEQ      *ON
045800960322     C* PER COMODITA' SE CLIENTE 9999 CI IMPOSTO 8888
045900960322     C                   MOVE      AWKKSC        W0040             4 0
046000960327    4C     W0040         IFEQ      9999
046100960322     C                   MOVE      8888          AWKKSC
046200960327    4C                   ENDIF
046300960327     C*
046400100105     C***                ELSE
046500960327     C* PER COMODITA' SE CLIENTE DI ALTRA FILIALE CI IMPOSTO 8888
046600100105     C***                MOVE      8888          AWKKSC
046700100105     C***                ENDIF
046800960327     C*
046900960322     C                   MOVEL     BLPRSM        AWKRSM
047000960322     C                   MOVEL     BLPINM        AWKINM
047100960322     C                   MOVEL     BLPCAM        AWKCAM
047200960322     C                   MOVEL     BLPLOM        AWKLOM
047300960322     C                   MOVEL     BLPPRM        AWKPRM
047400960322     C                   MOVEL     BLPNZM        AWKNZM
047500960322     C                   MOVEL     BLPRMN        AWKRMN
047600960322     C                   MOVEL     ARBRSD        AWKRSD
047700960322     C                   MOVEL     BLPNCL        AWKNCL
047800960327     C                   MOVEL     BLPPKF        AWKPKF
047900960322     C                   MOVEL     ARBPKB        AWKPKV
048000960322     C                   MOVEL     ARBDTV        AWKDTV
048100960322     C                   MOVEL     ARBORV        AWKORV
048200960329     C                   MOVEL     BLPSOP        AWKSOP
048300100107     c* il file di work lo aggiorno anche per file perchè mi serve per
048400100107     c*  controllare eventuali variazioni doppie (se ne deve scrivere una sola)
048500960322     C                   WRITE     FNAWK000
048600100107     c*
048700100107    4c                   if        d08tela='F'
048800100107     c                   clear                   darbpkf_i
048900100107     c                   clear                   tivgd000
049000100107     c                   eval      vgdtip='VP'
049100100107     c                   eval      vgdksu=wunif
049200100107     c                   eval      vgdtsc='WW'
049300100107     c
049400100107     c                   eval      vgddaT=dateu8
049500100107     c                   eval      vgdpgm='FNLS08R'
049600100107     c                   eval      §arpkaas=%editc(blpaas:'Z')
049700100107     c                   eval      §arpklnp=%editc(blplnp:'Z')
049800100107     c                   eval      §arpknrs=%Editc(blpnrs:'Z')
049900100107     c                   eval      §arpknsp=%Editc(blpnsp:'Z')
050000100107     c                   eval      §arpkmgs=%Editc(blpmgs:'Z')
050100100107     c                   eval      §arpkccm=wksc
050200100107     c                   eval      §arpkrsm=blprsm
050300100107     c                   eval      §arpkrmn=%Editc(blprmn:'Z')
050400100107     c                   eval      §arpkrma=blprma
050500100107     c                   eval      §arpkrsd=blprsd
050600100107     c                   eval      §arpkind=blpind
050700100107     c                   eval      §arpkcad=blpcad
050800100107     c                   eval      §arpkprd=blpprd
050900100107     c                   eval      §arpklod=blplod
051000100107     c                   eval      §arpknzd=blpnzd
051100100107     c                   eval      §arpknzd=blpnzd
051200100107     c                   eval      §arpkncl=blpncl
051300100107     c                   eval      §arpkvlf=blpvlf
051400100107     c                   eval      §arpkvlf=blpvlf
051500100107     c                   eval      §arpkpkr=blppkf
051600100107     c                   eval      §arpkpkb=arbpkb
051700100107     c                   eval      VGDDTA =%subst(Darbpkf_I:1:%size(Darbpkf_I))
051800100107     c*
051900100107     c                   write     tivgd000
052000100107     c
052100100107     C* AGGIORNO IL FLAG di elaborazione file
052200100107    5C     D08RIS        IFEQ      'NO'
052300100107     C     KARBD         CHAIN     FNARBD00                           31
052400100107    6C     *IN31         IFEQ      *OFF
052500100107
052600100107     c                   if        arbfst=' '
052700100107     C                   MOVEL     'F'           ARBFST
052800100107     c                   else
052900100107     C                   MOVEL     'E'           ARBFST
053000100107     c                   endif
053100100107     c
053200100107     C                   UPDATE    FNARBD00
053300100107    6C                   ENDIF
053400100107    5C                   ENDIF
053500100107     c
053600100107    4C                   ENDIF
053700100107     c
053800100107    3C                   ENDIF
053900960326     C*
054000960322    2C                   ENDIF
054100960322     C*
054200960322   X1C                   ELSE
054300960327     C* ARBPKB = BLPPKF ---> FLAGGO IL RECORD PER NON LEGGERLO PIU'
054400960322     C*   NEMMENO SE RICHIESTA LA RISTAMPA
054500960322     C                   MOVEL     'N'           ARBFST
054600960322     C                   UPDATE    FNARBD00
054700960322    1C                   ENDIF
054800960322     C*
054900960322     C                   ENDSR
055000960322     C*
055100960326     C*--- STAMPO DETTAGLIO ------------------------------------------*
055200960326     C     STADET        BEGSR
055300960326     C*
055400960326     C****  STAMPO TESTATA  ****
055500960328     C     *IN05         IFEQ      *ON
055600960327     C                   EXCEPT    FITT
055700960327     C                   EXCEPT    TES
055800960328     C                   SETOFF                                       05
055900960326     C                   ENDIF
056000960326     C*
056100960326     C****  STAMPO DETTAGLIO  ****
056200960326     C* 12 ON  - NUMERO SERIE <> 0  ---> STAMPO TRATTINO SEPARATORE
056300960326     C     AWKNRS        COMP      0                                  1212
056400960326     C*
056500960326     C* GIRO DATA SPEDIZIONE
056600960326     C                   Z-ADD     MES           GGMM
056700960326     C                   MOVEL     GIO           GGMM              4 0
056800960326     C                   Z-ADD     AWKAAS        STADSP
056900960327     C                   MOVEL     GGMM          STADSP            6 0
057000960326     C*
057100960326     C* RAGIONE SOCIALE DESTINATARIO
057200960326     C                   MOVEL     AWKRSD        STARSD           15
057300960326     C*
057400960326     C                   EXCEPT    DETT
057500960326     C                   ENDSR
057600960326     C*
057700960321     C*--- SR INIZIALE -----------------------------------------------*
057800960321     C     *INZSR        BEGSR
057900960321     C****
058000960321     C** KLIST
058100960321     C****
058200960322     C* ACCESSO FNBLP01L
058300960322     C     KBLP          KLIST
058400960322     C                   KFLD                    ARBAAS
058500960322     C                   KFLD                    ARBLNP
058600960322     C                   KFLD                    ARBNRS
058700960322     C                   KFLD                    ARBNSP
058800960322     C* ACCESSO FNAWK01L
058900960322     C     KAWK          KLIST
059000960322     C                   KFLD                    ARBAAS
059100960322     C                   KFLD                    ARBLNP
059200960322     C                   KFLD                    ARBNRS
059300960322     C                   KFLD                    ARBNSP
059400991005     C* ACCESSO FIARBF1C
059500960326     C     KARBD         KLIST
059600960326     C                   KFLD                    AWKAAS
059700960326     C                   KFLD                    AWKLNP
059800960326     C                   KFLD                    AWKNRS
059900960326     C                   KFLD                    AWKNSP
060000960326     C                   KFLD                    AWKDTV
060100960326     C                   KFLD                    AWKORV
060200020709
060300020709     c     Karbd1        Klist
060400020709     c                   Kfld                    Kaas
060500020709     c                   Kfld                    D08lnp
060600020709
060700960326     C****
060800960321     C** DEFINIZIONE CAMPI
060900960321     C****
061000960326     C     *LIKE         DEFINE    AWKKSC        WKSC
061100960326     C     *LIKE         DEFINE    AWKKSC        WKCLI
061200960326     C     *LIKE         DEFINE    AWKRSM        WRSM
061300960321     C*
061400960325     C****  PRENDO DATA E ORA CORRENTE  ****
061500960325     C                   TIME                    W0140            14 0
061600960325     C                   MOVE      W0140         UDATE8            8 0
061700960325     C* GIRO DATA DEL GIORNO
061800960325     C                   Z-ADD     UDATE8        WDATA
061900960325     C                   Z-ADD     GG            MMGG
062000960325     C                   MOVEL     MM            MMGG              4 0
062100960325     C                   Z-ADD     MMGG          DATEU8
062200960325     C                   MOVEL     AA            DATEU8            8 0
062300960327     C*
062400960326     C****  PARAMETRI PASSATI  ****
062500960326     C     *ENTRY        PLIST
062600960326     C                   PARM                    KPJBA
062700960326     C                   MOVEL     KPJBU         DSLS08
062800971121     C*
062900971121     C                   Z-ADD     1             CODUT
063000971121     C                   CALL      'X§PARUT'
063100971121     C                   PARM                    UT§DSE
063200971124     C                   MOVEL     RAGUT         RSUTP            24
063300960325     C****  DATA VARIAZIONE  ****
063400960325     C* SE DATA VARIAZIONE AL = 0 --> METTO DATA DAL IN DATA AL
063500960325    1C     D08DVD        IFGT      0
063600960325     C     D08DVA        ANDEQ     0
063700960325     C                   Z-ADD     D08DVD        D08DVA
063800960325    1C                   ENDIF
063900100107     c
064000100107     c* Se si tratta di FILE, cerco i figli del codice passato in DS
064100100107     c                   if        d08tela='F'
064200100107
064300100107     c                   clear                   tibs10ds
064400100107     c                   eval      d10drf=dateu8
064500100107     c                   eval      d10tle='WW'
064600100107     c                   eval      d10paf='F'
064700100107     c                   eval      d10cod=d08ksc
064800100107     c                   call      'TIBS10R'
064900100107     c                   parm                    tibs10ds
065000100107     c*
065100100107     c                   if        d10err<>' '
065200100107     c                   clear                   d10err
065300100107     c                   eval      d10paf='P'
065400100107     c                   eval      d10drf=dateu8
065500100107     c                   call      'TIBS10R'
065600100107     c                   parm                    tibs10ds
065700100107     c                   endif
065800100107     c                   if        d10err<>' '
065900100107     c                   move      *all'0'       wunif
066000100107     c                   move      d08ksc        wunif
066100100107     c                   else
066200100107     c                   move      d10cop        wunif
066300100107     c                   endif
066400100107     c
066500100107     c                   endif
066600100107     c
066700960327     C*
066800960327     C****  CARICO TABELLA FILIALI GESTITE £1  ****
066900100105     C***                CLEAR                   DSUL06
067000100105     C***                MOVE      '£1'          D06COD
067100100105     C***                MOVEL     D08FEL        D06KEY
067200100105     C***                MOVEL     DSUL06        KPJBU
067300100105     C***                CALL      'TRUL06R'
067400100105     C***                PARM                    KPJBA
067500100105     C***                MOVEL     KPJBU         DSUL06
067600100105     C***                MOVEA     LIN           L1
067700920925     C                   ENDSR
067800960326     C*
067900960325     O*---------------------------------------------------------------*
068000960326     O* INTESTAZIONE RIGHE DETTAGLIO
068100960326     OLET198    E            TES         2
068200960326     O                       ST(1)               55
068300960326     O                       ST(4)              110
068400960326     O          E            TES         1
068500960326     O                       ST(2)               55
068600960326     O                       ST(5)              110
068700960326     O* DETTAGLIO
068800960326     O          E            DETT        1
068900960327     O                       AWKLNP              22
069000960327     O                     12AWKNRS        Z     25
069100960327     O                     12                    26 '-'
069200960327     O                       AWKNSP        Z     33
069300960327     O                       STADSP              42 '  /  /  '
069400960326     O                       AWKRMN        Z     58
069500960326     O                       STARSD              75
069600960326     O                       AWKNCL        Z     82
069700960327     O                       AWKPKV        4     92
069800960403     O                       AWKPKF        4    105
069900960326     O* FINE LETTERA
070000960325     O          E            FINLET      2
070100960326     O                       ST(7)               55
070200960326     O                       ST(8)              110
070300960326     O          E            FINLET      2
070400960326     O                       ST(9)               55
070500960326     O                       ST(10)             110
070600960325     O          E            FINLET      3
070700971124     O                       RSUTP              106
070800960326     O* EXCPT FITTIZIA PER SALTO PAGINA
070900960326     O          E            FITT             09
071000960326     O                                          110 ' '
071100960326     O*---------------------------------------------------------------*
071200960326** ST
071300960401                   Ns.spedizione   Data     Vs.riferime1
071400960326                   ------------------------------------2
071500960327                   xxx xx-xxxxxxx xx/xx/xx xxxxxxxxxxxx3
071600981125nto  Destinatario     Colli  Kg. DDT   Kg.riscontrati  4
071700960403-----------------------------------------------------  5
071800960403xxx  xxxxxxxxxxxxxxx  xxxxx  xxxxxx,x     xxxxxx,x     6
071900960326                            I nostri Uffici rimangono a7
072000960326 Vs. disposizione per qualsiasi chiarimento.           8
072100960326                            L'occasione ci è gradita pe9
072200960326r porgerVi distinti saluti.                            10
072300960326** CMD
072400960401OVRPRTF FILE(LET198) SAVE(*NO)  SHARE(*YES)                           1
072500960326** CMD1
072600960326CLRPFM FILE(FNAWK00F)                                                 1
072700020709** CMD2
072800020709CRTDUPOBJ OBJ(FNAWK*) FROMLIB(XXXXXXXXXX) OBJTYPE(*FILE) TOLIB(QTEMP)
072900960326** MSG
073000960326SNDMSG MSG('ATTENZIONE!! FILE FNAWK00F GIA'' UTILIZZATO DA UN ALTRO   1
073100960328 LAVORO !!')                                                          2
073200960326                                                    TOUSR(*SYSOPR)    3
073300020709SNDMSG MSG('ATTENZIONE!! FILE FNAWK00F NON TROVATO NEL SISTEMA!!')    4
073400020709SNDMSG MSG('ATTENZIONE!! FILE FNAWK00F NON DUPLICATO IN QTEMP !!')    5
