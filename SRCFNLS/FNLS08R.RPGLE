000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200960321     H* FNLS08R *-----------------------------------------------------*
000300960321     H*  LETTERA COMUNICAZIONE VARIAZIONE PESO AI MITTENTI  -BATCH-   **
000400960321     H*---------------------------------------------------------------*
000500960326     FFNAWK01L  UF A E           K DISK    USROPN
000600960326     FFNAWK02L  IF   E           K DISK    USROPN
000700960322     F                                     RENAME(FNAWK000:FNAWK002)
000800131014     FFIARBF2C  UF   E           K DISK
000900170522     F***FNBLP01L  IF   E           K DISK
001000170522     fTITAS30C  if   e           k disk    extfile(wTAS30) usropn
001100170522     fTITAa30C  if   e           k disk    extfile(wTAa30) usropn
001200960328     FLET198    O    F  132        PRINTER OFLIND(*IN05)
001300960328     F                                     USROPN
001400020709
001500020709     d Kaas            s                   like(Arbaas) Inz
001600020709
001700960321     D*
001800960321     D* DEFINIZIONE SCHIERE
001900960327     D L1              S              3  0 DIM(30)
002000960326     D ER              S              1    DIM(201)
002100960326     D ST              S             55    DIM(10) CTDATA PERRCD(1)
002200960326     D CMD             S             43    DIM(1) CTDATA PERRCD(1)
002300960326     D CMD1            S             21    DIM(1) CTDATA PERRCD(1)
002400020709     D CMD2            S             69    DIM(1) CTDATA PERRCD(1)
002500020709     D MSG             S             67    DIM(5) CTDATA PERRCD(1)
002600941223     D*
002700960321     D* PASSAGGIO DATI DAL FILTRO                         - FNLS07R -
002800960321     D DSLS08        E DS                  EXTNAME(FNLS08DS)
002900170522     d
003000170522     d Azuteds       e ds                  Extname(Azute00f)
003100170522     d dDatiute      e ds
003200170522     d Tibs34ds      e ds                  Inz
003300960327     D*
003400960327     D* DS PER TRUL06R - CARICAMENTO £1
003500960327     D DSUL06        E DS                  EXTNAME(TRUL06DS)
003600960327     D  LIN                    1     90  0
003700960327     D                                     DIM(30)
003800960325     D***
003900960325     D* PARAM PER STAMPA LETTERE - FNLV45C E TUTTI GLI ALTRI PREVISTI
004000981116     D DSLV45        E DS                  EXTNAME(FNLV45DS)
004100960325     D                 DS
004200960328     D  GG                     1      2  0
004300960325     D  MM                     3      4  0
004400960328     D  AA                     5      8  0
004500960325     D  WDATA                  1      8  0
004600960325     D*
004700960325     D                 DS
004800960325     D  MES                    1      2  0
004900960325     D  GIO                    3      4  0
005000960325     D  AWKMGS                 1      4  0
005100900517     D KPJBA         E DS
005200170522     D BS69DS        E DS                  EXTNAME(TIBS69DS) INZ
005300170522     D ACODS         E DS                  EXTNAME(CNACO00F) INZ
005400170522     D INDDS         E DS                  EXTNAME(CNIND00F) INZ
005500170522     D CLPDS         E DS                  EXTNAME(CNCLP00F) INZ
005600170522     D CLSDS         E DS                  EXTNAME(FNCLS00F) INZ
005700020709
005800020709      * Parametri per pgm Trul29c
005900020709     d Tr29obj         s             10    inz('FNAWK00F  ')
006000020709     d Tr29typ         s              8    inz('*FILE   ')
006100020709     d Tr29lib         s             10    inz
006200170522     d wTAS30          s             21    inz('GAITRAGRU /TITAS30C')
006300170522     d wTAA30          s             21    inz('GAITRAGRU /TITAA30C')
006400960321     C*****************************************************************
006500960321     C* RIEPILOGO INDICATORI
006600960321     C*****************************************************************
006700960328     C* 05    - INDICATORE DI OVERFLOW
006800960326     C* 12    - NUMERO SERIE <> 0  --->  STAMPO TRATTINO SEPARATORE
006900960322     C* 15    - RECORD DA SCRIVERE NEL FILE DI WORK
007000960326     C* 17    - USATO PER NON STAMPARE LA 1° VOLTA IL FINALE LETTERA
007100960326     C* 30/32 - DI COMODO
007200960327     C* 35    - USATO PER LOKUP
007300960326     C* 90    - ERRORE DEL QCMDEXC SUL FILE PRINTER
007400020709     C* 99    - Errore esco dal Pgm
007500960321     C*****************************************************************
007600960326     C*
007700020709      * Reperisco la libreria in cui si trova il file di work
007800020709     c                   ExSr      RtnLib
007900020709     c   99              GoTo      Fine
008000020709      * Creo il file di work in qtemp
008100020709     c                   ExSr      CrtWrk
008200020709     c   99              GoTo      Fine
008300960326     C****  ESEGUO CLEAR DEL FILE DI WORK  ****
008400960326     C                   EXSR      CLRAWK
008500960326     C   99              GOTO      FINE
008600170522
008700170522      /free
008800170522         if  %subst( knsif : 7 : 1) = 'P';
008900170522           %subst( wTAS30 : 7 : 4 ) = 'GRPS';
009000170522           %subst( wTAA30 : 7 : 4 ) = 'GRPS';
009100170522         endif;
009200170522         open titas30c  ;
009300170522         open titaa30c  ;
009400170522      /end-free
009500920925     C*
009600960325     C****  E L A B O R A Z I O N E  ****
009700020709     C**!!!*LOVAL        SETLL     FNARBD00
009800020709     c     Kaas          Setgt     Fnarbd00
009900960326     C                   READ      FNARBD00                               30
010000020709     c                   Eval      Kaas = Arbaas
010100020709
010200020709     c     Leggi         Tag
010300020709     c     Karbd1        Setll     Fnarbd00
010400020709     c     Karbd1        Reade     Fnarbd00                               30
010500960322     C*
010600960322    1C     *IN30         DOWEQ     *OFF
010700960322     C*                      - CON LINEA PARTENZA IMMESSA A VIDEO
010800110708    2C     ARBLNP        ifeq      D08LNP
010900960322     C*                      - CHE SONO DA STAMPARE O DA RISTAMPARE
011000960322     C     ARBFST        ANDNE     'N'
011100110708     C* LEGGO tutti i record con causale variazione di peso 8non solo IC)
011200110708    2C***  ARBCVB        IFEQ      'IC'
011300110708   2ac                   if        arbcvb='IC' or arbcvb='IB'
011400960322     C                   CLEAR                   WELAB
011500960322     C*
011600960322     C* RICHIESTA LA STAMPA
011700960322    3C     D08RIS        IFEQ      'NO'
011800960322     C     ARBFST        IFEQ      ' '
011900960322     C                   MOVEL     'S'           WELAB             1
012000960322     C                   ENDIF
012100960322     C*
012200960322   X3C                   ELSE
012300960322     C* RICHIESTA LA RISTAMPA
012400960322     C     ARBFST        IFEQ      'S'
012500960322     C                   MOVEL     'S'           WELAB
012600960322     C                   ENDIF
012700960322    3C                   ENDIF
012800960322     C*
012900960322     C* WELAB = 'S' ---> RECORD DA ELABORARE
013000960322    3C     WELAB         IFEQ      'S'
013100960322     C                   EXSR      WRTAWK
013200960322    3C                   ENDIF
013300960322     C*
013400110708   2aC                   ENDIF
013500110708    2C                   ENDIF
013600110708     c
013700020709     C**!!!              READ      FNARBD00                               30
013800020709     c     Karbd1        Reade     Fnarbd00                               30
013900960322    1C                   ENDDO
014000020709
014100020709      * Devo leggere l'anno successivo
014200020709     c     Kaas          Setgt     Fnarbd00
014300020709     c                   Read      Fnarbd00                               30
014400020709     c                   If        Not *In30
014500020709     c                   Eval      Kaas = Arbaas
014600020709     c                   GoTo      Leggi
014700020709     c                   EndIf
014800960322     C*
014900960325     C****  S T A M P A  ****
015000960322     C     *LOVAL        SETLL     FNAWK02L
015100960322     C                   READ      FNAWK02L                               30
015200960326     C*
015300960326     C* ESEGUO OVRPRTF SUL FILE DI STAMPA PER APERTURA CONDIVISA
015400960326    1C     *IN30         IFEQ      *OFF
015500960326     C                   Z-ADD     43            LUNG             15 5
015600960326     C                   MOVEL     *BLANKS       COMMAN           80
015700960326     C                   MOVEA     CMD(1)        COMMAN
015800960326     C                   CALL      'QCMDEXC'                            90
015900960326     C                   PARM                    COMMAN
016000960326     C                   PARM                    LUNG
016100960326     C*
016200960326     C                   OPEN      LET198
016300960326    1C                   ENDIF
016400960322     C*
016500960326    1C  N90*IN30         DOWEQ     *OFF
016600960326     C                   MOVE      AWKKSC        W0040
016700960322     C*
016800960322     C* SE CLIENTE NON CODIFICATO  --->  ROTTURA PER RAGIONE SOCIALE
016900960322    2C     W0040         IFEQ      8888
017000960322     C*
017100960322     C     AWKRSM        IFNE      WRSM
017200960326     C* STAMPO FINALE LETTERA
017300960328     C   05
017400960328     CAN 17              EXCEPT    FITT
017500960326     C   17              EXCEPT    FINLET
017600960326     C  N17              SETON                                        17
017700960328     C*
017800960326     C* STAMPO INIZIO LETTERA
017900981116     C                   CLEAR                   DSLV45
018000981116     C                   MOVEL     AWKINM        I45VIA
018100981116     C                   MOVEL     AWKLOM        I45CIT
018200981116     C                   MOVEL     AWKCAM        I45CAP
018300981116     C                   MOVEL     AWKNZM        I45NAR
018400981116     C                   Z-ADD     1             I45NCO
018500981116     C                   MOVEL     UDATE8        I45GDS
018600981116     C                   MOVEL     AWKPRM        I45PRO
018700981116     C                   MOVEL     AWKLNP        I45APP
018800981116     C                   MOVEL(P)  AWKRSM        I45RAG
018900981116     C                   MOVEL     'WW'          I45TTX
019000981116     C                   MOVEL     'L'           I45AST
019100981116     C                   MOVEL     'I'           I45LIN
019200981116     C                   MOVEL     'FNLV45R'     I45PGM
019300960326     C                   CALL      'FNLV45C'
019400960326     C                   PARM                    KPJBA
019500981116     C                   PARM                    DSLV45
019600960328     C*
019700960327     C* STAMPO INTESTAZIONE DETTAGLIO
019800960327     C                   EXCEPT    TES
019900960326     C*
020000960322     C                   MOVEL     AWKRSM        WRSM
020100960322     C                   ENDIF
020200960322     C*
020300960322   X2C                   ELSE
020400960322     C* SE CLIENTE     CODIFICATO  --->  ROTTURA PER CODICE CLIENTE
020500960326     C*
020600960326    3C     AWKKSC        IFNE      WKCLI
020700960326     C* STAMPO FINALE LETTERA
020800960328     C   05
020900960328     CAN 17              EXCEPT    FITT
021000960326     C   17              EXCEPT    FINLET
021100960326     C  N17              SETON                                        17
021200960326     C*
021300960326     C* STAMPO INIZIO LETTERA
021400981116     C                   CLEAR                   DSLV45
021500981116     C                   MOVEL     AWKKSC        I45KSC
021600981116     C                   Z-ADD     1             I45NCO
021700981116     C                   MOVEL     UDATE8        I45GDS
021800981116     C                   MOVEL     DATEU8        I45ADS
021900981116     C                   MOVEL     '999'         I45NAR
022000981116     C                   MOVEL     D08KCI        I45KCC
022100981116     C                   MOVEL     'WW'          I45TTX
022200981116     C                   MOVEL     'C'           I45AST
022300981116     C                   MOVEL     'I'           I45LIN
022400981116     C                   MOVEL     'FNLV45R'     I45PGM
022500960325     C                   CALL      'FNLV45C'
022600960325     C                   PARM                    KPJBA
022700981116     C                   PARM                    DSLV45
022800960327     C*
022900960327     C* STAMPO INTESTAZIONE DETTAGLIO
023000960327     C                   EXCEPT    TES
023100960325     C*
023200960326     C                   MOVEL     AWKKSC        WKCLI
023300960325    3C                   ENDIF
023400960322    2C                   ENDIF
023500960326     C*
023600960326     C* STAMPO DETTAGLIO
023700960326     C                   EXSR      STADET
023800960322     C*
023900960326     C* SOLO PER STAMPA: AGGIORNO IL FLAG DI STAMPA
024000960326    1C     D08RIS        IFEQ      'NO'
024100960326     C     KARBD         CHAIN     FNARBD00                           31
024200960326     C     *IN31         IFEQ      *OFF
024300960326     C                   MOVEL     'S'           ARBFST
024400960326     C                   UPDATE    FNARBD00
024500960326     C                   ENDIF
024600960326    1C                   ENDIF
024700960326     C*
024800960322     C                   READ      FNAWK02L                               30
024900960322    1C                   ENDDO
025000960322     C*
025100960326     C* STAMPO FINALE LETTERA
025200960328     C   05
025300960328     CAN 17              EXCEPT    FITT
025400960326     C   17              EXCEPT    FINLET
025500960326     C*
025600960326     C     FINE          TAG
025700000000     C                   SETON                                        LR
025800920925     C*
025900020709      *---------------------------------------------------------------*
026000020709      *   Reperisco la libreria del file di work
026100020709      *---------------------------------------------------------------*
026200020709     c     RtnLib        BegSr
026300020709
026400020709     c                   Call      'TRUL29C'
026500020709     c                   Parm                    Tr29Obj
026600020709     c                   Parm                    Tr29Typ
026700020709     c                   Parm                    Tr29Lib
026800020709      * Se la libreria ritorna a blank vuol dire che non ha trovato il file
026900020709      * in questo caso chiudo il pgm e invio un msg alla coda dell'opratore
027000020709    1c                   If        Tr29Lib = *blanks
027100020709     c                   clear                   Er
027200020709     c                   Movea     Msg(4)        Er(1)
027300020709     c                   Movea     Msg(3)        Er(135)
027400020709     c                   Movea     Er            Comm
027500020709     c                   z-add     201           Lung
027600020709     c                   Call      'QCMDEXC'
027700020709     c                   Parm                    Comm
027800020709     c                   Parm                    Lung
027900020709     c                   Eval      *In99 = *On
028000020709    1c                   EndIf
028100020709
028200020709     c                   EndSr
028300020709      *---------------------------------------------------------------*
028400020709      *   Creo il file di work in qtemp
028500020709      *---------------------------------------------------------------*
028600020709     c     CrtWrk        BegSr
028700020709     c                   Z-add     69            Lung
028800020709     c                   Clear                   comman           80
028900020709     c                   Movea     Cmd2(1)       comman
029000020709     c                   Eval      %Subst(Comman:31:10) = Tr29Lib
029100020709     c                   Call      'QCMDEXC'                            99
029200020709     c                   Parm                    Comman
029300020709     c                   Parm                    Lung
029400020709      * Duplica oggetti non andata a buon fine
029500020709      * in questo caso chiudo il pgm e invio un msg alla coda dell'opratore
029600020709    1c                   If        *In99
029700020709     c                   Clear                   Er
029800020709     c                   Movea     Msg(5)        Er(1)
029900020709     c                   Movea     Msg(3)        Er(135)
030000020709     c                   Movea     Er            Comm
030100020709     c                   Z-add     201           Lung
030200020709     c                   Call      'QCMDEXC'
030300020709     c                   Parm                    Comm
030400020709     c                   Parm                    Lung
030500020709    1c                   EndIf
030600020709
030700020709     c                   EndSr
030800960326     C*--- CLEAR FILE DI WORK ----------------------------------------*
030900960326     C     CLRAWK        BEGSR
031000960326     C                   Z-ADD     21            LUNG
031100960326     C                   MOVEL     *BLANKS       COMMAN           80
031200960326     C                   MOVEA     CMD1(1)       COMMAN
031300960326     C                   CALL      'QCMDEXC'                            99
031400960326     C                   PARM                    COMMAN           80
031500960326     C                   PARM                    LUNG             15 5
031600960326     C*
031700960326     C* 99 ON  - CLEAR NON ANDATA A BUON FINE: INVIO MESSAGGIO ALLA
031800960326     C*   CODA DELL'OPERATORE ED ESCO DAL PGM
031900960326    1C     *IN99         IFEQ      *ON
032000960326     C                   MOVEA     MSG(1)        ER(1)
032100960326     C                   MOVEA     MSG(2)        ER(68)
032200960326     C                   MOVEA     MSG(3)        ER(135)
032300960326     C                   MOVEA     ER            COMM            201
032400960326     C                   Z-ADD     201           LUNG
032500960326     C                   CALL      'QCMDEXC'
032600960326     C                   PARM                    COMM
032700960326     C                   PARM                    LUNG
032800960326     C*
032900960326   X1C                   ELSE
033000960326     C                   OPEN      FNAWK01L
033100960326     C                   OPEN      FNAWK02L
033200960326    1C                   ENDIF
033300960326     C                   ENDSR
033400960326     C*
033500960322     C*--- SCRIVO FILE DI WORK ---------------------------------------*
033600960322     C     WRTAWK        BEGSR
033700960322     C*
033800960322     C* AGGANCIO LA BOLLA
033900170522     C***  KBLP          CHAIN     FNBLP01L                           31
034000170522     C     KBLP          CHAIN     titas30c
034100170522    0c                   if        %found(titas30c)
034200960322     C*
034300960327     C* SCRIVO IL FILE DI WORK SOLO SE ARBPKB E' DIVERSO DA BLPPKF
034400170522    1C     ARBPKB        IFNE      tasPKF
034500960322     C                   SETON                                        15
034600960322     C*
034700960322     C* IMPOSTO IL CODICE CLIENTE
034800170522    2C**   BLPCCM        IFGT      0
034900170522     C**                 MOVEL     BLPCCM        WKSC
035000170522   X2C**                 ELSE
035100170522     C**                 MOVEL     BLPKSC        WKSC
035200170522    2C**                 ENDIF
035300170522     c
035400170522     c                   movel     tasccm        wksc
035500960322     C*
035600960322     C* SIA PER STAMPA CHE RISTAMPA: SE IMMESSO CONTROLLO IL CLIENTE
035700960322    2C     D08KSC        IFGT      0
035800960322     C     D08KSC        ANDNE     WKSC
035900960322     C                   SETOFF                                       15
036000960322    2C                   ENDIF
036100960322     C*
036200960322    2C   15D08RIS        IFEQ      'SI'
036300960322     C* SOLO PER RISTAMPA: SE IMMESSA CONTROLLO LA DATA VARIAZIONE
036400960322    3C     D08DVD        IFGT      0
036500960322     C     ARBDTV        IFLT      D08DVD
036600960322     C     ARBDTV        ORGT      D08DVA
036700960322     C                   SETOFF                                       15
036800960322     C                   ENDIF
036900960322    3C                   ENDIF
037000960322     C* SOLO PER RISTAMPA: SE IMMESSA CONTROLLO LA LINEA DI ARRIVO
037100960322    3C   15D08LNA        IFGT      0
037200170522     C     D08LNA        ANDNE     tasLNA
037300960322     C                   SETOFF                                       15
037400960322    3C                   ENDIF
037500960322    2C                   ENDIF
037600960322     C*
037700960322     C* 15 ON  - RECORD DA SCRIVERE NEL FILE DI WORK
037800960322    2C     *IN15         IFEQ      *ON
037900960322     C*
038000960322    3C     D08RIS        IFEQ      'NO'
038100960327     C* SE TROVO PIU' VARIAZIONI PESO NON ANCORA STAMPATE (ARBFST=' ')
038200960327     C*   PER LA STESSA SPEDIZIONE, STAMPO SOLO LA 1° PERCHE' CONTIENE
038300960327     C*   IL PESO ORIGINALE CON CUI LA BOLLA E' STATA BOLLETTATA
038400960322     C     KAWK          CHAIN     FNAWK01L                           32
038500960326     C*
038600960327     C* SE TROVO ALTRE VARIAZIONI PER LA STESSA SPEDIZIONE LE FLAGGO
038700960327     C*   PER NON LEGGERLE PIU' NEMMENO SE RICHIESTA LA RISTAMPA
038800960327     C     *IN32         IFEQ      *OFF
038900960327     C                   MOVEL     'N'           ARBFST
039000960327     C                   UPDATE    FNARBD00
039100960327     C                   ENDIF
039200960322     C*
039300960322     C* SE TROVO PIU' VARIAZIONI DI PESO PER LA STESSA SPEDIZIONE
039400960322     C*   GIA' STAMPATE (ARBFST='S'), LE RISTAMPO TUTTE
039500960322   X3C                   ELSE
039600960322     C                   SETON                                        32
039700960322    3C                   ENDIF
039800960322     C*
039900960327    3C     *IN32         IFEQ      *ON
040000960327     C                   MOVEL     ARBAAS        AWKAAS
040100960327     C                   MOVEL     ARBLNP        AWKLNP
040200960327     C                   MOVEL     ARBNRS        AWKNRS
040300960327     C                   MOVEL     ARBNSP        AWKNSP
040400170522     C                   MOVEL     tasMGS        AWKMGS
040500170522     C                   MOVEL     tasLNA        AWKLNA
040600960326     C                   MOVEL     WKSC          AWKKSC
040700170522     c* Reperisco mittente
040800170522     c                   exsr      deccli
040900960327     C*
041000960327     C* CONTROLLO SE CODICE CLIENTE DI ALTRA FILIALE
041100960327     C                   MOVEL     AWKKSC        W0030             3 0
041200960327     C     W0030         LOOKUP    L1                                     35
041300960327     C*
041400960327     C     *IN35         IFEQ      *ON
041500960322     C* PER COMODITA' SE CLIENTE 9999 CI IMPOSTO 8888
041600960322     C                   MOVE      AWKKSC        W0040             4 0
041700960327    4C     W0040         IFEQ      9999
041800960322     C                   MOVE      8888          AWKKSC
041900960327    4C                   ENDIF
042000960327     C*
042100960327     C                   ELSE
042200960327     C* PER COMODITA' SE CLIENTE DI ALTRA FILIALE CI IMPOSTO 8888
042300960327     C                   MOVE      8888          AWKKSC
042400960327     C                   ENDIF
042500960327     C*
042600170522     C                   MOVEL     acorag        AWKRSM
042700170522     C                   MOVEL     indvia        AWKINM
042800170522     C                   MOVEL     indcae        AWKCAM
042900170522     C                   MOVEL     indcit        AWKLOM
043000170522     C                   MOVEL     indprv        AWKPRM
043100170522     C                   MOVEL     indsta        AWKNZM
043200170522     C                   MOVEL     tasRMN        AWKRMN
043300170522     C                   MOVEL     tasRSD        AWKRSD
043400170522     C                   MOVEL     tasNCL        AWKNCL
043500170522     C                   MOVEL     tasPKF        AWKPKF
043600960322     C                   MOVEL     ARBPKB        AWKPKV
043700960322     C                   MOVEL     ARBDTV        AWKDTV
043800960322     C                   MOVEL     ARBORV        AWKORV
043900170522     C                   MOVEL     tasSOP        AWKSOP
044000960322     C                   WRITE     FNAWK000
044100960322    3C                   ENDIF
044200960326     C*
044300960322    2C                   ENDIF
044400960322     C*
044500960322   X1C                   ELSE
044600960327     C* ARBPKB = BLPPKF ---> FLAGGO IL RECORD PER NON LEGGERLO PIU'
044700960322     C*   NEMMENO SE RICHIESTA LA RISTAMPA
044800960322     C                   MOVEL     'N'           ARBFST
044900960322     C                   UPDATE    FNARBD00
045000960322    1C                   ENDIF
045100170522
045200170522   X0C                   ELSE
045300170522     C                   MOVEL     'N'           ARBFST
045400170522     C                   UPDATE    FNARBD00
045500170522    1C                   ENDIF
045600960322     C*
045700960322     C                   ENDSR
045800170522     C*------------------------------------------------------------------------*
045900170522     C* DECCLI - DECODIFICA CLIENTE
046000170522     C*------------------------------------------------------------------------*
046100170522     C     DECCLI        BEGSR
046200170522     C*
046300170522     c* se il cliente è 8888 chain in titaa
046400170522     c                   move      wksc          w0040             4 0
046500170522     C                   CLEAR                   BS69DS
046600170522     C                   CLEAR                   ACODS
046700170522     C                   CLEAR                   indDS
046800170522     c
046900170522     c                   if        w0040=8888 or w0040= 0000
047000170522     c     kblp          chain     titaa30c
047100170522     c                   if        %found(titaa30c)
047200170522     c                   eval      acorag=taarsc
047300170522     c                   eval      indvia=taaind
047400170522     c                   eval      indcit=taaloc
047500170522     c                   eval      indcae=taacap
047600170522     c                   eval      indprv=taaprv
047700170522     c                   eval      indsta=taanaz
047800170522     c                   endif
047900170522     c
048000170522     c                   else
048100170522     C                   MOVEL     KNSIF         I69SIF
048200170522     C                   Z-ADD     Wksc          I69KAC
048300170522     C                   Z-ADD     Wksc          I69Kin
048400170522     C                   CALL      'TIBS69R'
048500170522     C                   PARM                    BS69DS
048600170522     C                   PARM                    ACODS
048700170522     C                   PARM                    INDDS
048800170522     C                   PARM                    CLPDS
048900170522     C                   PARM                    CLSDS
049000170522     c                   endif
049100170522     c
049200170522     C                   ENDSR
049300170522     C*
049400960322     C*
049500960326     C*--- STAMPO DETTAGLIO ------------------------------------------*
049600960326     C     STADET        BEGSR
049700960326     C*
049800960326     C****  STAMPO TESTATA  ****
049900960328     C     *IN05         IFEQ      *ON
050000960327     C                   EXCEPT    FITT
050100960327     C                   EXCEPT    TES
050200960328     C                   SETOFF                                       05
050300960326     C                   ENDIF
050400960326     C*
050500960326     C****  STAMPO DETTAGLIO  ****
050600960326     C* 12 ON  - NUMERO SERIE <> 0  ---> STAMPO TRATTINO SEPARATORE
050700960326     C     AWKNRS        COMP      0                                  1212
050800960326     C*
050900960326     C* GIRO DATA SPEDIZIONE
051000960326     C                   Z-ADD     MES           GGMM
051100960326     C                   MOVEL     GIO           GGMM              4 0
051200960326     C                   Z-ADD     AWKAAS        STADSP
051300960327     C                   MOVEL     GGMM          STADSP            6 0
051400960326     C*
051500960326     C* RAGIONE SOCIALE DESTINATARIO
051600960326     C                   MOVEL     AWKRSD        STARSD           15
051700960326     C*
051800960326     C                   EXCEPT    DETT
051900960326     C                   ENDSR
052000960326     C*
052100960321     C*--- SR INIZIALE -----------------------------------------------*
052200960321     C     *INZSR        BEGSR
052300960321     C****
052400960321     C** KLIST
052500960321     C****
052600960322     C* ACCESSO FNBLP01L
052700960322     C     KBLP          KLIST
052800960322     C                   KFLD                    ARBAAS
052900960322     C                   KFLD                    ARBLNP
053000960322     C                   KFLD                    ARBNRS
053100960322     C                   KFLD                    ARBNSP
053200960322     C* ACCESSO FNAWK01L
053300960322     C     KAWK          KLIST
053400960322     C                   KFLD                    ARBAAS
053500960322     C                   KFLD                    ARBLNP
053600960322     C                   KFLD                    ARBNRS
053700960322     C                   KFLD                    ARBNSP
053800131014     C* ACCESSO FIARBF2C
053900960326     C     KARBD         KLIST
054000960326     C                   KFLD                    AWKAAS
054100960326     C                   KFLD                    AWKLNP
054200960326     C                   KFLD                    AWKNRS
054300960326     C                   KFLD                    AWKNSP
054400960326     C                   KFLD                    AWKDTV
054500960326     C                   KFLD                    AWKORV
054600020709
054700020709     c     Karbd1        Klist
054800020709     c                   Kfld                    Kaas
054900020709     c                   Kfld                    D08lnp
055000020709
055100960326     C****
055200960321     C** DEFINIZIONE CAMPI
055300960321     C****
055400960326     C     *LIKE         DEFINE    AWKKSC        WKSC
055500960326     C     *LIKE         DEFINE    AWKKSC        WKCLI
055600960326     C     *LIKE         DEFINE    AWKRSM        WRSM
055700960321     C*
055800960325     C****  PRENDO DATA E ORA CORRENTE  ****
055900960325     C                   TIME                    W0140            14 0
056000960325     C                   MOVE      W0140         UDATE8            8 0
056100960325     C* GIRO DATA DEL GIORNO
056200960325     C                   Z-ADD     UDATE8        WDATA
056300960325     C                   Z-ADD     GG            MMGG
056400960325     C                   MOVEL     MM            MMGG              4 0
056500960325     C                   Z-ADD     MMGG          DATEU8
056600960325     C                   MOVEL     AA            DATEU8            8 0
056700960327     C*
056800960326     C****  PARAMETRI PASSATI  ****
056900960326     C     *ENTRY        PLIST
057000960326     C                   PARM                    KPJBA
057100960326     C                   MOVEL     KPJBU         DSLS08
057200971121     C*
057300170522     C                   Z-ADD     1             CODUT             1 0
057400170522      *
057500170522     c     *dtaara       define    §azute        azuteds
057600170522     c     *dtaara       define    §datiute      ddatiute
057700170522     c                   in(E)     *dtaara
057800170522     c                   If        %error  or RSUT = *blanks
057900170522     c                   CLEAR                   tibs34ds
058000170522     c                   CALL      'TIBS34R'
058100170522     c                   PARM                    tibs34ds
058200170522     c                   in        *dtaara
058300170522     c                   EndIf
058400170522     C                   MOVEL     rsut          RSUTP            24
058500170522     c
058600960325     C****  DATA VARIAZIONE  ****
058700960325     C* SE DATA VARIAZIONE AL = 0 --> METTO DATA DAL IN DATA AL
058800960325    1C     D08DVD        IFGT      0
058900960325     C     D08DVA        ANDEQ     0
059000960325     C                   Z-ADD     D08DVD        D08DVA
059100960325    1C                   ENDIF
059200960327     C*
059300960327     C****  CARICO TABELLA FILIALI GESTITE £1  ****
059400960327     C                   CLEAR                   DSUL06
059500960327     C                   MOVE      '£1'          D06COD
059600960327     C                   MOVEL     D08FEL        D06KEY
059700960327     C                   MOVEL     DSUL06        KPJBU
059800960327     C                   CALL      'TRUL06R'
059900960327     C                   PARM                    KPJBA
060000960327     C                   MOVEL     KPJBU         DSUL06
060100960327     C                   MOVEA     LIN           L1
060200920925     C                   ENDSR
060300960326     C*
060400960325     O*---------------------------------------------------------------*
060500960326     O* INTESTAZIONE RIGHE DETTAGLIO
060600960326     OLET198    E            TES         2
060700960326     O                       ST(1)               55
060800960326     O                       ST(4)              110
060900960326     O          E            TES         1
061000960326     O                       ST(2)               55
061100960326     O                       ST(5)              110
061200960326     O* DETTAGLIO
061300960326     O          E            DETT        1
061400960327     O                       AWKLNP              22
061500960327     O                     12AWKNRS        Z     25
061600960327     O                     12                    26 '-'
061700960327     O                       AWKNSP        Z     33
061800960327     O                       STADSP              42 '  /  /  '
061900960326     O                       AWKRMN        Z     58
062000960326     O                       STARSD              75
062100960326     O                       AWKNCL        Z     82
062200960327     O                       AWKPKV        4     92
062300960403     O                       AWKPKF        4    105
062400960326     O* FINE LETTERA
062500960325     O          E            FINLET      2
062600960326     O                       ST(7)               55
062700960326     O                       ST(8)              110
062800960326     O          E            FINLET      2
062900960326     O                       ST(9)               55
063000960326     O                       ST(10)             110
063100960325     O          E            FINLET      3
063200971124     O                       RSUTP              106
063300960326     O* EXCPT FITTIZIA PER SALTO PAGINA
063400960326     O          E            FITT             09
063500960326     O                                          110 ' '
063600960326     O*---------------------------------------------------------------*
063700960326** ST
063800960401                   Ns.spedizione   Data     Vs.riferime1
063900960326                   ------------------------------------2
064000960327                   xxx xx-xxxxxxx xx/xx/xx xxxxxxxxxxxx3
064100981125nto  Destinatario     Colli  Kg. DDT   Kg.riscontrati  4
064200960403-----------------------------------------------------  5
064300960403xxx  xxxxxxxxxxxxxxx  xxxxx  xxxxxx,x     xxxxxx,x     6
064400960326                            I nostri Uffici rimangono a7
064500960326 Vs. disposizione per qualsiasi chiarimento.           8
064600960326                            L'occasione ci è gradita pe9
064700960326r porgerVi distinti saluti.                            10
064800960326** CMD
064900960401OVRPRTF FILE(LET198) SAVE(*NO)  SHARE(*YES)                           1
065000960326** CMD1
065100960326CLRPFM FILE(FNAWK00F)                                                 1
065200020709** CMD2
065300020709CRTDUPOBJ OBJ(FNAWK*) FROMLIB(XXXXXXXXXX) OBJTYPE(*FILE) TOLIB(QTEMP)
065400960326** MSG
065500960326SNDMSG MSG('ATTENZIONE!! FILE FNAWK00F GIA'' UTILIZZATO DA UN ALTRO   1
065600960328 LAVORO !!')                                                          2
065700960326                                                    TOUSR(*SYSOPR)    3
065800020709SNDMSG MSG('ATTENZIONE!! FILE FNAWK00F NON TROVATO NEL SISTEMA!!')    4
065900020709SNDMSG MSG('ATTENZIONE!! FILE FNAWK00F NON DUPLICATO IN QTEMP !!')    5
