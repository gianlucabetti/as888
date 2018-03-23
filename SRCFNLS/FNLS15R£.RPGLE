000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200961015     H* FNLS15R *-----------------------------------------------------*
000300960321     H*-----------------*                                             *
000400961016     H*           ELENCO VARIZIONI VOLUME                 - BATCH -   **
000500960321     H*---------------------------------------------------------------*
000600960329     FFNAWK01L  O  A E           K DISK    USROPN
000700960328     FFNAWK03L  IF   E           K DISK    USROPN
000800960328     F                                     RENAME(FNAWK000:FNAWK003)
000900991005     FFIARBF1C  UF   E           K DISK
001000960322     FFNBLP01L  IF   E           K DISK
001100110413     Fazute01l  IF   E           K DISK
001200960328     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001300020709
001400020709     d Kaas            s                   like(Arbaas) Inz
001500020709
001600960321     D*
001700960321     D* DEFINIZIONE SCHIERE
001800960326     D ER              S              1    DIM(201)
001900960401     D ST              S             66    DIM(14) CTDATA PERRCD(1)
002000960326     D CMD1            S             21    DIM(1) CTDATA PERRCD(1)
002100020709     D CMD2            S             69    DIM(1) CTDATA PERRCD(1)
002200020709     D MSG             S             67    DIM(5) CTDATA PERRCD(1)
002300941223     D*
002400960321     D* PASSAGGIO DATI DAL FILTRO                         - FNLS07R -
002500961015     D DSLS15        E DS                  EXTNAME(FNLS15DS)
002600960327     D*
002700960325     D                 DS
002800960329     D  AA                     1      4  0
002900960329     D  MM                     5      6  0
003000960329     D  GG                     7      8  0
003100960325     D  WDATA                  1      8  0
003200960325     D                 DS
003300960325     D  MES                    1      2  0
003400960325     D  GIO                    3      4  0
003500960325     D  AWKMGS                 1      4  0
003600960329     D                 DS
003700960401     D  ARBAAS                 1      4  0
003800960401     D  ARBLNP                 5      7  0
003900960401     D  ARBNRS                 8      9  0
004000960401     D  ARBNSP                10     16  0
004100960401     D  AWKSPE                 1     16  0
004200900517     D KPJBA         E DS
004300960329     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
004400020709
004500020709      * Parametri per pgm Trul29c
004600020709     d Tr29obj         s             10    inz('FNAWK00F  ')
004700020709     d Tr29typ         s              8    inz('*FILE   ')
004800020709     d Tr29lib         s             10    inz
004900110413     d kpru            s             10    inz
005000960321     C*****************************************************************
005100960321     C* RIEPILOGO INDICATORI
005200960321     C*****************************************************************
005300960401     C* 01    - STAMPA DEI PIEDI
005400960328     C* 06    - IMMESSO UNO SPECIFICO CODICE CLIENTE
005500960328     C* 07    - IMMESSA LA DATA VARIAZIONE
005600960328     C* 08    - IMMESSA SPECIFICA LINEA DI ARRIVO
005700961016     C* 09    - OFF  ESISTONO PIU' VARIAZIONI PER LA STESSA SPEDIZIONE
005800960329     C* 11    - USATO PER STAMPARE LA SIGLA OPERATORE SOLO LA 1° VOLTA
005900960326     C* 12    - NUMERO SERIE <> 0  --->  STAMPO TRATTINO SEPARATORE
006000960322     C* 15    - RECORD DA SCRIVERE NEL FILE DI WORK
006100960329     C* 17    - USATO PER NON STAMPARE LA 1° VOLTA IL TOTALE BOLLE
006200960329     C* 22 OFF- NON C'E' NULLA DA STAMPARE: STAMPO LA TESTATA
006300960326     C* 30/32 - DI COMODO
006400020709     C* 99    - Errore esco dal pgm
006500960321     C*****************************************************************
006600960326     C*
006700020709      * Reperisco la libreria in cui si trova il file di work
006800020709     c                   ExSr      RtnLib
006900020709     c   99              GoTo      Fine
007000020709      * Creo il file di work in qtemp
007100020709     c                   ExSr      CrtWrk
007200020709     c   99              GoTo      Fine
007300960326     C****  ESEGUO CLEAR DEL FILE DI WORK  ****
007400960326     C                   EXSR      CLRAWK
007500960326     C   99              GOTO      FINE
007600920925     C*
007700960325     C****  E L A B O R A Z I O N E  ****
007800020709     C**!!!*LOVAL        SETLL     FNARBD00
007900020709     c     Kaas          Setgt     Fnarbd00
008000960326     C                   READ      FNARBD00                               30
008100020709     c                   Eval      Kaas = Arbaas
008200020709
008300020709     c     Leggi         Tag
008400020709     c     Karbd1        Setll     Fnarbd00
008500020709     c     Karbd1        Reade     Fnarbd00                               30
008600960322     C*
008700960322    1C     *IN30         DOWEQ     *OFF
008800961015     C* LEGGO SOLO I RECORD: - CON CAUSALE VARIAZIONE "I2"
008900961015    2C     ARBCVB        IFEQ      'I2'
009000960322     C*                      - CON LINEA PARTENZA IMMESSA A VIDEO
009100961015     C     ARBLNP        ANDEQ     D15LNP
009200961016     C*                      - CHE SONO DA STAMPARE
009300961015     C     ARBFST        ANDNE     'N'
009400960401     C**
009500960401     C* SE QUI SONO ENTRATA MEMORIZZO IL NUMERO DI SPEDIZIONE
009600961015    3C     AWKSPE        IFNE      WSPE
009700960401     C                   SETON                                        09
009800960401     C                   MOVEL     AWKSPE        WSPE
009900961015   X3C                   ELSE
010000961016     C* SE ESISTONO PIU' VARIAZIONI PER LA STESSA SPEDIZIONE CONTO
010100961016     C*   SOLO LA PRIMA PERCHE' LE ALTRE SONO VARIAZIONI FATTE A CAUSA
010200961016     C*   DI UN INSERIMENTO ERRATO DA PARTE DELLA FILIALE DI ARRIVO
010300960401     C                   SETOFF                                       09
010400961015    3C                   ENDIF
010500961015     C*
010600961016     C* SCRIVO FILE DI WORK
010700960322     C                   EXSR      WRTAWK
010800960322    2C                   ENDIF
010900961015     C*
011000020709     C**!!!              READ      FNARBD00                               30
011100020709     c     Karbd1        Reade     Fnarbd00                               30
011200960322    1C                   ENDDO
011300020709
011400020709      * Devo leggere l'anno successivo
011500020709     c     Kaas          Setgt     Fnarbd00
011600020709     c                   Read      Fnarbd00                               30
011700020709     c                   If        Not *In30
011800020709     c                   Eval      Kaas = Arbaas
011900020709     c                   GoTo      Leggi
012000020709     c                   EndIf
012100960322     C*
012200960325     C****  S T A M P A  ****
012300960328     C     *LOVAL        SETLL     FNAWK03L
012400960328     C                   READ      FNAWK03L                               30
012500960326     C*
012600960328    1C     *IN30         DOWEQ     *OFF
012700960329     C                   SETOFF                                       11
012800960329     C*
012900960329     C  N17              MOVEL     AWKSOP        WSOP
013000960329     C  N17              SETON                                        1711
013100960322     C*
013200960329     C* A CAMBIO SIGLA OPERATORE STAMPO TOTALE BOLLE
013300960329    2C     AWKSOP        IFNE      WSOP
013400960329     C                   SETON                                        11
013500960329     C     *INOF         IFEQ      *ON
013600960401     C   01              EXCEPT    PIEDI
013700960401     C                   EXCEPT    TES
013800960329     C                   SETOFF                                       OF
013900960401     C                   SETON                                        01
014000960329     C                   ENDIF
014100960329     C                   EXCEPT    TOT
014200960329     C*
014300960329     C                   CLEAR                   TOTBOL
014400960329     C                   MOVEL     AWKSOP        WSOP
014500960322    2C                   ENDIF
014600960329     C*
014700960329     C* STAMPO DETTAGLIO
014800960329     C                   EXSR      STADET
014900960326     C*
015000960328     C                   READ      FNAWK03L                               30
015100960322    1C                   ENDDO
015200960322     C*
015300960329     C* STAMPO ULTIMO TOTALE BOLLE
015400960329    1C   17*INOF         IFEQ      *ON
015500960401     C   01              EXCEPT    PIEDI
015600960401     C                   EXCEPT    TES
015700960329     C                   SETOFF                                       OF
015800960401     C                   SETON                                        01
015900960329    1C                   ENDIF
016000960329     C   17              EXCEPT    TOT
016100960326     C*
016200960329     C****  SE NON C'E' NIENTE DA STAMPARE STAMPO LA TESTATA  ****
016300960329    1C     *IN22         IFEQ      *OFF
016400960401     C   01              EXCEPT    PIEDI
016500960329     C                   EXCEPT    TES
016600960401     C                   SETON                                        01
016700960329    2C                   DO        10
016800960329     C                   EXCEPT    FITT
016900960329    2C                   ENDDO
017000960329    1C                   ENDIF
017100960329     C*
017200960329     C****  STAMPA DICITURA: FINE STAMPA  ****
017300960401     C                   EXCEPT    FINEST
017400960401     C                   EXCEPT    PIEDI
017500960329     C*
017600960326     C     FINE          TAG
017700000000     C                   SETON                                        LR
017800020709      *---------------------------------------------------------------*
017900020709      *   Reperisco la libreria del file di work
018000020709      *---------------------------------------------------------------*
018100020709     c     RtnLib        BegSr
018200020709
018300020709     c                   Call      'TRUL29C'
018400020709     c                   Parm                    Tr29Obj
018500020709     c                   Parm                    Tr29Typ
018600020709     c                   Parm                    Tr29Lib
018700020709      * Se la libreria ritorna a blank vuol dire che non ha trovato il file
018800020709      * in questo caso chiudo il pgm e invio un msg alla coda dell'opratore
018900020709    1c                   If        Tr29Lib = *blanks
019000020709     c                   clear                   Er
019100020709     c                   Movea     Msg(4)        Er(1)
019200020709     c                   Movea     Msg(3)        Er(135)
019300020709     c                   Movea     Er            Comm
019400020709     c                   z-add     201           Lung
019500020709     c                   Call      'QCMDEXC'
019600020709     c                   Parm                    Comm
019700020709     c                   Parm                    Lung
019800020709     c                   Eval      *In99 = *On
019900020709    1c                   EndIf
020000020709
020100020709     c                   EndSr
020200020709      *---------------------------------------------------------------*
020300020709      *   Creo il file di work in qtemp
020400020709      *---------------------------------------------------------------*
020500020709     c     CrtWrk        BegSr
020600020709     c                   Z-add     69            Lung
020700020709     c                   Clear                   comman           80
020800020709     c                   Movea     Cmd2(1)       comman
020900020709     c                   Eval      %Subst(Comman:31:10) = Tr29Lib
021000020709     c                   Call      'QCMDEXC'                            99
021100020709     c                   Parm                    Comman
021200020709     c                   Parm                    Lung
021300020709      * Duplica oggetti non andata a buon fine
021400020709      * in questo caso chiudo il pgm e invio un msg alla coda dell'opratore
021500020709    1c                   If        *In99
021600020709     c                   Clear                   Er
021700020709     c                   Movea     Msg(5)        Er(1)
021800020709     c                   Movea     Msg(3)        Er(135)
021900020709     c                   Movea     Er            Comm
022000020709     c                   Z-add     201           Lung
022100020709     c                   Call      'QCMDEXC'
022200020709     c                   Parm                    Comm
022300020709     c                   Parm                    Lung
022400020709    1c                   EndIf
022500020709
022600020709     c                   EndSr
022700920925     C*
022800960326     C*--- CLEAR FILE DI WORK ----------------------------------------*
022900960326     C     CLRAWK        BEGSR
023000960326     C                   Z-ADD     21            LUNG
023100960326     C                   MOVEL     *BLANKS       COMMAN           80
023200960326     C                   MOVEA     CMD1(1)       COMMAN
023300960326     C                   CALL      'QCMDEXC'                            99
023400960326     C                   PARM                    COMMAN           80
023500960326     C                   PARM                    LUNG             15 5
023600960326     C*
023700960326     C* 99 ON  - CLEAR NON ANDATA A BUON FINE: INVIO MESSAGGIO ALLA
023800960326     C*   CODA DELL'OPERATORE ED ESCO DAL PGM
023900960326    1C     *IN99         IFEQ      *ON
024000960326     C                   MOVEA     MSG(1)        ER(1)
024100960326     C                   MOVEA     MSG(2)        ER(68)
024200960326     C                   MOVEA     MSG(3)        ER(135)
024300960326     C                   MOVEA     ER            COMM            201
024400960326     C                   Z-ADD     201           LUNG
024500960326     C                   CALL      'QCMDEXC'
024600960326     C                   PARM                    COMM
024700960326     C                   PARM                    LUNG
024800960326     C*
024900960326   X1C                   ELSE
025000960326     C                   OPEN      FNAWK01L
025100960329     C                   OPEN      FNAWK03L
025200960326    1C                   ENDIF
025300960326     C                   ENDSR
025400960326     C*
025500960322     C*--- SCRIVO FILE DI WORK ---------------------------------------*
025600960322     C     WRTAWK        BEGSR
025700960322     C*
025800960322     C* AGGANCIO LA BOLLA
025900960322     C     KBLP          CHAIN     FNBLP01L                           31
026000980518     C* SE NON TROVO LA BOLLA SIGNIFICA CHE SI TRATTA DI UNA VARIAZIONE
026100980518     C*  LEGATA ALLA BOLLA IN IARRIVO (LOCALE): LA IGNORO
026200980518     C     *IN31         IFEQ      *OFF
026300960322     C*
026400961015     C* SCRIVO IL FILE DI WORK SOLO SE ARBVLB E' DIVERSO DA BLPVLF
026500961015    1C     ARBVLB        IFNE      BLPVLF
026600960322     C                   SETON                                        15
026700960322     C*
026800960322     C* IMPOSTO IL CODICE CLIENTE
026900960322    2C     BLPCCM        IFGT      0
027000960322     C                   MOVEL     BLPCCM        WKSC
027100960322   X2C                   ELSE
027200960322     C                   MOVEL     BLPKSC        WKSC
027300960322    2C                   ENDIF
027400960322     C*
027500961016     C* SE IMMESSO CONTROLLO IL CLIENTE
027600961015    2C     D15KSC        IFGT      0
027700961015     C     D15KSC        ANDNE     WKSC
027800960322     C                   SETOFF                                       15
027900960322    2C                   ENDIF
028000960322     C*
028100961016     C* SE IMMESSA CONTROLLO LA DATA VARIAZIONE
028200961016    2C   15D15DVD        IFGT      0
028300961015     C     ARBDTV        IFLT      D15DVD
028400961015     C     ARBDTV        ORGT      D15DVA
028500960322     C                   SETOFF                                       15
028600960322     C                   ENDIF
028700961016    2C                   ENDIF
028800961016     C* SE IMMESSA CONTROLLO LA LINEA DI ARRIVO
028900110413     c* ES - non è la linea di arrivo perchè potrebbe essere anche la partenza
029000110413     c*      a fare la variazione
029100961016    2C   15D15LNA        IFGT      0
029200110413     c                   eval      kpru=%subst(arbpru:1:9)
029300110413     c     kpru          chain     azute01l
029400110413     c                   if        %found(azute01l) and utefil<>d15lna
029500110413     C                   SETOFF                                       15
029600110413    2C                   ENDIF
029700110413     c
029800110413     C**   D15LNA        ANDNE     BLPLNA
029900110413     C**                 SETOFF                                       15
030000110413    2C**                 ENDIF
030100110413    2C                   ENDIF
030200960322     C*
030300960322     C* 15 ON  - RECORD DA SCRIVERE NEL FILE DI WORK
030400960322    2C     *IN15         IFEQ      *ON
030500960327     C                   MOVEL     ARBAAS        AWKAAS
030600960327     C                   MOVEL     ARBLNP        AWKLNP
030700960327     C                   MOVEL     ARBNRS        AWKNRS
030800960327     C                   MOVEL     ARBNSP        AWKNSP
030900960328     C                   MOVEL     BLPLNA        AWKLNA
031000960322     C                   MOVEL     BLPMGS        AWKMGS
031100960326     C                   MOVEL     WKSC          AWKKSC
031200960322     C                   MOVEL     BLPRSM        AWKRSM
031300960322     C                   MOVEL     BLPINM        AWKINM
031400960322     C                   MOVEL     BLPCAM        AWKCAM
031500960322     C                   MOVEL     BLPLOM        AWKLOM
031600960322     C                   MOVEL     BLPPRM        AWKPRM
031700960322     C                   MOVEL     BLPNZM        AWKNZM
031800960322     C                   MOVEL     BLPRMN        AWKRMN
031900960401     C                   MOVEL     BLPRSD        AWKRSD
032000960322     C                   MOVEL     BLPNCL        AWKNCL
032100960327     C                   MOVEL     BLPPKF        AWKPKF
032200961016     C                   CLEAR                   AWKPKV
032300961016     C                   MOVEL     BLPVLF        AWKVLF
032400961016     C                   MOVEL     BLPFVF        AWKFVF
032500961016     C                   MOVEL     ARBVLB        AWKVLV
032600961016     C                   MOVEL     ARBFVB        AWKFVV
032700960322     C                   MOVEL     ARBDTV        AWKDTV
032800960322     C                   MOVEL     ARBORV        AWKORV
032900960328     C                   MOVEL     BLPSOP        AWKSOP
033000960401     C   09              MOVEL     ' '           AWKESV
033100960401     C  N09              MOVEL     'S'           AWKESV
033200960322     C                   WRITE     FNAWK000
033300960322    2C                   ENDIF
033400960322     C*
033500960322   X1C                   ELSE
033600961015     C* ARBVLB = BLPVLF ---> FLAGGO IL RECORD PER NON LEGGERLO PIU'
033700960322     C                   MOVEL     'N'           ARBFST
033800960322     C                   UPDATE    FNARBD00
033900960322    1C                   ENDIF
034000980518    1C                   ENDIF
034100960322     C*
034200960322     C                   ENDSR
034300960322     C*
034400960326     C*--- STAMPO DETTAGLIO ------------------------------------------*
034500960326     C     STADET        BEGSR
034600960329     C                   SETON                                        22
034700960329     C                   SETOFF                                       09
034800960326     C*
034900960326     C****  STAMPO TESTATA  ****
035000960329     C     *INOF         IFEQ      *ON
035100960401     C   01              EXCEPT    PIEDI
035200960401     C                   SETON                                        1101
035300960327     C                   EXCEPT    TES
035400960329     C                   SETOFF                                       OF
035500960326     C                   ENDIF
035600960326     C*
035700960326     C****  STAMPO DETTAGLIO  ****
035800960326     C* 12 ON  - NUMERO SERIE <> 0  ---> STAMPO TRATTINO SEPARATORE
035900960326     C     AWKNRS        COMP      0                                  1212
036000960326     C*
036100960326     C* GIRO DATA SPEDIZIONE
036200960326     C                   Z-ADD     MES           GGMM
036300960326     C                   MOVEL     GIO           GGMM              4 0
036400960326     C                   Z-ADD     AWKAAS        STADSP
036500960328     C                   MOVEL     GGMM          STADSP            8 0
036600960326     C*
036700960401     C* RAGIONE SOCIALE MITTENTE E DESTINATARIO
036800960328     C                   MOVEL     AWKRSM        STARSM           20
036900960328     C                   MOVEL     AWKRSD        STARSD           20
037000960326     C*
037100960401     C     AWKESV        IFEQ      ' '
037200960329     C                   ADD       1             TOTBOL            9 0
037300960329     C                   ELSE
037400960329     C                   SETON                                        09
037500960329     C                   ENDIF
037600960329     C*
037700960326     C                   EXCEPT    DETT
037800960326     C                   ENDSR
037900960326     C*
038000960321     C*--- SR INIZIALE -----------------------------------------------*
038100960321     C     *INZSR        BEGSR
038200960321     C****
038300960321     C** KLIST
038400960321     C****
038500960322     C* ACCESSO FNBLP01L
038600960322     C     KBLP          KLIST
038700960322     C                   KFLD                    ARBAAS
038800960322     C                   KFLD                    ARBLNP
038900960322     C                   KFLD                    ARBNRS
039000960322     C                   KFLD                    ARBNSP
039100020709
039200020709     c     Karbd1        Klist
039300020709     c                   Kfld                    Kaas
039400020709     c                   Kfld                    D15lnp
039500020709
039600960326     C****
039700960321     C** DEFINIZIONE CAMPI
039800960321     C****
039900960401     C     *LIKE         DEFINE    AWKKSC        WKSC
040000960329     C     *LIKE         DEFINE    AWKSOP        WSOP
040100961015     C     *LIKE         DEFINE    D15DVD        STADVD
040200961015     C     *LIKE         DEFINE    D15DVA        STADVA
040300960329     C     *LIKE         DEFINE    AWKSPE        WSPE
040400960321     C*
040500960325     C****  PRENDO DATA E ORA CORRENTE  ****
040600960325     C                   TIME                    W0140            14 0
040700960325     C                   MOVE      W0140         UDATE8            8 0
040800960329     C                   MOVEL     W0140         UTIME             6 0
040900960329     C*
041000960329     C* ACCENDO INDICATORE DI OVERFLOW
041100960329     C                   SETON                                        OF
041200960329     C*
041300960329     C                   Z-ADD     1             CODUT
041400960329     C                   CALL      'X§PARUT'
041500960329     C                   PARM                    UT§DSE
041600960329     C                   MOVEL     RAGUT         RSUT             20
041700960327     C*
041800960326     C****  PARAMETRI PASSATI  ****
041900960326     C     *ENTRY        PLIST
042000960326     C                   PARM                    KPJBA
042100961015     C                   MOVEL     KPJBU         DSLS15
042200960329     C*
042300960329     C****  CODICE CLIENTE  ****
042400961015     C     D15KSC        IFGT      0
042500960329     C                   SETON                                        06
042600960329     C                   ENDIF
042700960329     C*
042800960325     C****  DATA VARIAZIONE  ****
042900960325     C* SE DATA VARIAZIONE AL = 0 --> METTO DATA DAL IN DATA AL
043000961016    1C     D15DVD        IFGT      0
043100960328     C                   SETON                                        07
043200961015     C     D15DVA        IFEQ      0
043300961015     C                   Z-ADD     D15DVD        D15DVA
043400960328     C                   ENDIF
043500960329     C* GIRO DATA VARIAZIONE DAL
043600961015     C                   Z-ADD     D15DVD        WDATA
043700960329     C                   Z-ADD     MM            GGMM              4 0
043800960328     C                   MOVEL     GG            GGMM
043900960329     C                   Z-ADD     AA            STADVD
044000960329     C                   MOVEL     GGMM          STADVD
044100960329     C* GIRO DATA VARIAZIONE  AL
044200961015     C                   Z-ADD     D15DVA        WDATA
044300960329     C                   Z-ADD     MM            GGMM
044400960329     C                   MOVEL     GG            GGMM
044500960329     C                   Z-ADD     AA            STADVA
044600960329     C                   MOVEL     GGMM          STADVA
044700961016    1C                   ENDIF
044800961016     C*
044900960329     C****  LINEA ARRIVO  ****
045000961016    1C     D15LNA        IFGT      0
045100960329     C                   SETON                                        08
045200961016    1C                   ENDIF
045300960329     C*
045400920925     C                   ENDSR
045500960326     C*
045600960325     O*---------------------------------------------------------------*
045700960328     O****  TESTATA  ****
045800960328     OQSYSPRT   E            TES              02
045900960328     O                       ST(1)               66
046000960328     O                       ST(2)              132
046100960328     O                       RSUT                20
046200960328     O                                           21 '/'
046300961015     O                       D15FEL              24
046400960328     O                       UDATE8             117 '  /  /    '
046500960328     O                       PAGE          Z    132
046600960328     O          E            TES              03
046700960328     O                       KNSIF               10
046800960328     O                       KNMUS               24
046900960328     O                       UTIME              115 '  :  :  '
047000960328     O          E            TES              05
047100960328     O                                           30 'LINEA PARTENZA:'
047200961015     O                       D15LNP              34
047300960328     O                     06                    51 'COD.CLIENTE:'
047400961015     O                     06D15KSC        Z     59
047500960328     O                     07                    83 'DATA VARIAZIONE DAL'
047600960328     O                     07STADVD              94 '  /  /    '
047700960328     O                     07                    97 'AL'
047800960328     O                     07STADVA             108 '  /  /    '
047900110413     O                     08                   126 'FIL. VARIAZIONE:'
048000961015     O                     08D15LNA             130
048100960328     O*
048200960328     O          E            TES              07
048300960328     O                       ST(5)               66
048400960328     O                       ST(8)              132
048500960328     O          E            TES            1 08
048600960328     O                       ST(6)               66
048700960328     O                       ST(9)              132
048800960328     O****  DETTAGLIO  ****
048900960328     O          E            DETT        1
049000960329     O                     11AWKSOP              03
049100961016     O                       AWKLNP              10
049200961016     O                     12AWKNRS        Z     13
049300961016     O                     12                    14 '-'
049400961016     O                       AWKNSP        Z     21
049500961016     O                       STADSP              32 '  /  /    '
049600961016     O                       AWKLNA              36
049700961016     O                       STARSM              58
049800961016     O                       STARSD              79
049900961016     O                       AWKRMN        Z     95
050000961016     O                       AWKNCL        Z    102
050100961016     O                       AWKPKF        4    111
050200961016     O                       AWKFVV             114
050300961016     O                       AWKVLV        4    120
050400961016     O                       AWKFVF             123
050500961016     O                       AWKVLF        4    129
050600961016     O                     09                   132 '*'
050700960401     O*
050800960401     O          E            PIEDI            65
050900960401     O                       ST(13)              66
051000960401     O*
051100960328     O****  TOTALE BOLLE VARIATE PER BOLLETTISTA  ****
051200960328     O          E            TOT         2  3
051300960328     O                                           25 'TOTALE BOLLE :'
051400960328     O                       TOTBOL        2     37
051500960329     O****  FITTIZZIO PER IL SALTO GIUSTO DELLE RIGHE  ****
051600960329     O          E            FITT        1
051700960329     O                                              ' '
051800960329     O****  DICITURA FINE STAMPA  ****
051900960329     O          E            FINEST      2
052000960329     O                       ST(11)              66
052100960329     O                       ST(12)             132
052200960326     O*---------------------------------------------------------------*
052300960326** ST
052400961016XXXXXXXXXXXXXXXXXXXX/XXX       ***   E L E N C O    V A R I A Z I
052500961016O N I    V O L U M E   ***   FNLS15R     XX/XX/XXXX      PAG. XXXX   2
052600961016               LINEA PARTENZA: XXX     COD.CLIENTE: XXXXXXX     DA
052700110413TA VARIAZIONE DAL XX/XX/XXXX AL XX/XX/XXXX     FIL.VARIAZIO: XXX
052800961016SIGLA  S  P  E  D  I  Z  I  O  N  E   MITTENTE             DESTINA  5
052900961016OPER.  LNP   NUMERO     DATA     LNA                                6
053000961016  X    XXX XX-XXXXXXX XX/XX/XXXX XXX  XXXXXXXXXXXXXXXXXXXX XXXXXXX
053100961016TARIO             RIFERIMENTO  COLLI PESO KG.   VOLUME   VOLUME     8
053200961016                     MITTENTE                          MODIFICATO   9
053300961016XXXXXXXXXXXXX XXXXXXXXXXXXXXX  XXXXX XXXXXX,X  XXX,XXX  XXX,XXX  *
053400960329                                             * * *     F I N E      11
053500960329  S T A M P A    * * *                                              12
053600961016 * = VARIAZIONI EFFETTUATE PIU' VOLTE SULLA STESSA SPEDIZIONE       13
053700961016                                                                    14
053800960326** CMD1
053900960326CLRPFM FILE(FNAWK00F)                                                 1
054000020709** CMD2
054100020709CRTDUPOBJ OBJ(FNAWK*) FROMLIB(XXXXXXXXXX) OBJTYPE(*FILE) TOLIB(QTEMP)
054200960326** MSG
054300960326SNDMSG MSG('ATTENZIONE!! FILE FNAWK00F GIA'' UTILIZZATO DA UN ALTRO   1
054400960328 LAVORO !!')                                                          2
054500960326                                                    TOUSR(*SYSOPR)    3
054600020709SNDMSG MSG('ATTENZIONE!! FILE FNAWK00F NON TROVATO NEL SISTEMA!!')    4
054700020709SNDMSG MSG('ATTENZIONE!! FILE FNAWK00F NON DUPLICATO IN QTEMP !!')    5
