000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200961015     H* FNLS15R *-----------------------------------------------------*
000300961016     H*           ELENCO VARIZIONI VOLUME                 - BATCH -   **
000400960321     H*---------------------------------------------------------------*
000500960329     FFNAWK01L  O  A E           K DISK    USROPN
000600960328     FFNAWK03L  IF   E           K DISK    USROPN
000700960328     F                                     RENAME(FNAWK000:FNAWK003)
000800131014     FFIARBF2C  UF   E           K DISK
000900960322     FFNBLP01L  IF   E           K DISK
001000110413     Fazute01l  IF   E           K DISK
001100960328     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001200020709
001300020709     d Kaas            s                   like(Arbaas) Inz
001400020709
001500960321     D*
001600960321     D* DEFINIZIONE SCHIERE
001700960326     D ER              S              1    DIM(201)
001800960401     D ST              S             66    DIM(14) CTDATA PERRCD(1)
001900960326     D CMD1            S             21    DIM(1) CTDATA PERRCD(1)
002000020709     D CMD2            S             69    DIM(1) CTDATA PERRCD(1)
002100020709     D MSG             S             67    DIM(5) CTDATA PERRCD(1)
002200941223     D*
002300960321     D* PASSAGGIO DATI DAL FILTRO                         - FNLS07R -
002400961015     D DSLS15        E DS                  EXTNAME(FNLS15DS)
002500960327     D*
002600960325     D                 DS
002700960329     D  AA                     1      4  0
002800960329     D  MM                     5      6  0
002900960329     D  GG                     7      8  0
003000960325     D  WDATA                  1      8  0
003100960325     D                 DS
003200960325     D  MES                    1      2  0
003300960325     D  GIO                    3      4  0
003400960325     D  AWKMGS                 1      4  0
003500960329     D                 DS
003600960401     D  ARBAAS                 1      4  0
003700960401     D  ARBLNP                 5      7  0
003800960401     D  ARBNRS                 8      9  0
003900960401     D  ARBNSP                10     16  0
004000960401     D  AWKSPE                 1     16  0
004100900517     D KPJBA         E DS
004200960329     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
004300020709
004400020709      * Parametri per pgm Trul29c
004500020709     d Tr29obj         s             10    inz('FNAWK00F  ')
004600020709     d Tr29typ         s              8    inz('*FILE   ')
004700020709     d Tr29lib         s             10    inz
004800110413     d kpru            s             10    inz
004900960321     C*****************************************************************
005000960321     C* RIEPILOGO INDICATORI
005100960321     C*****************************************************************
005200960401     C* 01    - STAMPA DEI PIEDI
005300960328     C* 06    - IMMESSO UNO SPECIFICO CODICE CLIENTE
005400960328     C* 07    - IMMESSA LA DATA VARIAZIONE
005500960328     C* 08    - IMMESSA SPECIFICA LINEA DI ARRIVO
005600961016     C* 09    - OFF  ESISTONO PIU' VARIAZIONI PER LA STESSA SPEDIZIONE
005700960329     C* 11    - USATO PER STAMPARE LA SIGLA OPERATORE SOLO LA 1° VOLTA
005800960326     C* 12    - NUMERO SERIE <> 0  --->  STAMPO TRATTINO SEPARATORE
005900960322     C* 15    - RECORD DA SCRIVERE NEL FILE DI WORK
006000960329     C* 17    - USATO PER NON STAMPARE LA 1° VOLTA IL TOTALE BOLLE
006100960329     C* 22 OFF- NON C'E' NULLA DA STAMPARE: STAMPO LA TESTATA
006200960326     C* 30/32 - DI COMODO
006300020709     C* 99    - Errore esco dal pgm
006400960321     C*****************************************************************
006500960326     C*
006600020709      * Reperisco la libreria in cui si trova il file di work
006700020709     c                   ExSr      RtnLib
006800020709     c   99              GoTo      Fine
006900020709      * Creo il file di work in qtemp
007000020709     c                   ExSr      CrtWrk
007100020709     c   99              GoTo      Fine
007200960326     C****  ESEGUO CLEAR DEL FILE DI WORK  ****
007300960326     C                   EXSR      CLRAWK
007400960326     C   99              GOTO      FINE
007500920925     C*
007600960325     C****  E L A B O R A Z I O N E  ****
007700020709     C**!!!*LOVAL        SETLL     FNARBD00
007800020709     c     Kaas          Setgt     Fnarbd00
007900960326     C                   READ      FNARBD00                               30
008000020709     c                   Eval      Kaas = Arbaas
008100020709
008200020709     c     Leggi         Tag
008300020709     c     Karbd1        Setll     Fnarbd00
008400020709     c     Karbd1        Reade     Fnarbd00                               30
008500960322     C*
008600960322    1C     *IN30         DOWEQ     *OFF
008700961015     C* LEGGO SOLO I RECORD: - CON CAUSALE VARIAZIONE "I2"
008800961015    2C     ARBCVB        IFEQ      'I2'
008900960322     C*                      - CON LINEA PARTENZA IMMESSA A VIDEO
009000961015     C     ARBLNP        ANDEQ     D15LNP
009100961016     C*                      - CHE SONO DA STAMPARE
009200961015     C     ARBFST        ANDNE     'N'
009300960401     C**
009400960401     C* SE QUI SONO ENTRATA MEMORIZZO IL NUMERO DI SPEDIZIONE
009500961015    3C     AWKSPE        IFNE      WSPE
009600960401     C                   SETON                                        09
009700960401     C                   MOVEL     AWKSPE        WSPE
009800961015   X3C                   ELSE
009900961016     C* SE ESISTONO PIU' VARIAZIONI PER LA STESSA SPEDIZIONE CONTO
010000961016     C*   SOLO LA PRIMA PERCHE' LE ALTRE SONO VARIAZIONI FATTE A CAUSA
010100961016     C*   DI UN INSERIMENTO ERRATO DA PARTE DELLA FILIALE DI ARRIVO
010200960401     C                   SETOFF                                       09
010300961015    3C                   ENDIF
010400961015     C*
010500961016     C* SCRIVO FILE DI WORK
010600960322     C                   EXSR      WRTAWK
010700960322    2C                   ENDIF
010800961015     C*
010900020709     C**!!!              READ      FNARBD00                               30
011000020709     c     Karbd1        Reade     Fnarbd00                               30
011100960322    1C                   ENDDO
011200020709
011300020709      * Devo leggere l'anno successivo
011400020709     c     Kaas          Setgt     Fnarbd00
011500020709     c                   Read      Fnarbd00                               30
011600020709     c                   If        Not *In30
011700020709     c                   Eval      Kaas = Arbaas
011800020709     c                   GoTo      Leggi
011900020709     c                   EndIf
012000960322     C*
012100960325     C****  S T A M P A  ****
012200960328     C     *LOVAL        SETLL     FNAWK03L
012300960328     C                   READ      FNAWK03L                               30
012400960326     C*
012500960328    1C     *IN30         DOWEQ     *OFF
012600960329     C                   SETOFF                                       11
012700960329     C*
012800960329     C  N17              MOVEL     AWKSOP        WSOP
012900960329     C  N17              SETON                                        1711
013000960322     C*
013100960329     C* A CAMBIO SIGLA OPERATORE STAMPO TOTALE BOLLE
013200960329    2C     AWKSOP        IFNE      WSOP
013300960329     C                   SETON                                        11
013400960329     C     *INOF         IFEQ      *ON
013500960401     C   01              EXCEPT    PIEDI
013600960401     C                   EXCEPT    TES
013700960329     C                   SETOFF                                       OF
013800960401     C                   SETON                                        01
013900960329     C                   ENDIF
014000960329     C                   EXCEPT    TOT
014100960329     C*
014200960329     C                   CLEAR                   TOTBOL
014300960329     C                   MOVEL     AWKSOP        WSOP
014400960322    2C                   ENDIF
014500960329     C*
014600960329     C* STAMPO DETTAGLIO
014700960329     C                   EXSR      STADET
014800960326     C*
014900960328     C                   READ      FNAWK03L                               30
015000960322    1C                   ENDDO
015100960322     C*
015200960329     C* STAMPO ULTIMO TOTALE BOLLE
015300960329    1C   17*INOF         IFEQ      *ON
015400960401     C   01              EXCEPT    PIEDI
015500960401     C                   EXCEPT    TES
015600960329     C                   SETOFF                                       OF
015700960401     C                   SETON                                        01
015800960329    1C                   ENDIF
015900960329     C   17              EXCEPT    TOT
016000960326     C*
016100960329     C****  SE NON C'E' NIENTE DA STAMPARE STAMPO LA TESTATA  ****
016200960329    1C     *IN22         IFEQ      *OFF
016300960401     C   01              EXCEPT    PIEDI
016400960329     C                   EXCEPT    TES
016500960401     C                   SETON                                        01
016600960329    2C                   DO        10
016700960329     C                   EXCEPT    FITT
016800960329    2C                   ENDDO
016900960329    1C                   ENDIF
017000960329     C*
017100960329     C****  STAMPA DICITURA: FINE STAMPA  ****
017200960401     C                   EXCEPT    FINEST
017300960401     C                   EXCEPT    PIEDI
017400960329     C*
017500960326     C     FINE          TAG
017600000000     C                   SETON                                        LR
017700020709      *---------------------------------------------------------------*
017800020709      *   Reperisco la libreria del file di work
017900020709      *---------------------------------------------------------------*
018000020709     c     RtnLib        BegSr
018100020709
018200020709     c                   Call      'TRUL29C'
018300020709     c                   Parm                    Tr29Obj
018400020709     c                   Parm                    Tr29Typ
018500020709     c                   Parm                    Tr29Lib
018600020709      * Se la libreria ritorna a blank vuol dire che non ha trovato il file
018700020709      * in questo caso chiudo il pgm e invio un msg alla coda dell'opratore
018800020709    1c                   If        Tr29Lib = *blanks
018900020709     c                   clear                   Er
019000020709     c                   Movea     Msg(4)        Er(1)
019100020709     c                   Movea     Msg(3)        Er(135)
019200020709     c                   Movea     Er            Comm
019300020709     c                   z-add     201           Lung
019400020709     c                   Call      'QCMDEXC'
019500020709     c                   Parm                    Comm
019600020709     c                   Parm                    Lung
019700020709     c                   Eval      *In99 = *On
019800020709    1c                   EndIf
019900020709
020000020709     c                   EndSr
020100020709      *---------------------------------------------------------------*
020200020709      *   Creo il file di work in qtemp
020300020709      *---------------------------------------------------------------*
020400020709     c     CrtWrk        BegSr
020500020709     c                   Z-add     69            Lung
020600020709     c                   Clear                   comman           80
020700020709     c                   Movea     Cmd2(1)       comman
020800020709     c                   Eval      %Subst(Comman:31:10) = Tr29Lib
020900020709     c                   Call      'QCMDEXC'                            99
021000020709     c                   Parm                    Comman
021100020709     c                   Parm                    Lung
021200020709      * Duplica oggetti non andata a buon fine
021300020709      * in questo caso chiudo il pgm e invio un msg alla coda dell'opratore
021400020709    1c                   If        *In99
021500020709     c                   Clear                   Er
021600020709     c                   Movea     Msg(5)        Er(1)
021700020709     c                   Movea     Msg(3)        Er(135)
021800020709     c                   Movea     Er            Comm
021900020709     c                   Z-add     201           Lung
022000020709     c                   Call      'QCMDEXC'
022100020709     c                   Parm                    Comm
022200020709     c                   Parm                    Lung
022300020709    1c                   EndIf
022400020709
022500020709     c                   EndSr
022600920925     C*
022700960326     C*--- CLEAR FILE DI WORK ----------------------------------------*
022800960326     C     CLRAWK        BEGSR
022900960326     C                   Z-ADD     21            LUNG
023000960326     C                   MOVEL     *BLANKS       COMMAN           80
023100960326     C                   MOVEA     CMD1(1)       COMMAN
023200960326     C                   CALL      'QCMDEXC'                            99
023300960326     C                   PARM                    COMMAN           80
023400960326     C                   PARM                    LUNG             15 5
023500960326     C*
023600960326     C* 99 ON  - CLEAR NON ANDATA A BUON FINE: INVIO MESSAGGIO ALLA
023700960326     C*   CODA DELL'OPERATORE ED ESCO DAL PGM
023800960326    1C     *IN99         IFEQ      *ON
023900960326     C                   MOVEA     MSG(1)        ER(1)
024000960326     C                   MOVEA     MSG(2)        ER(68)
024100960326     C                   MOVEA     MSG(3)        ER(135)
024200960326     C                   MOVEA     ER            COMM            201
024300960326     C                   Z-ADD     201           LUNG
024400960326     C                   CALL      'QCMDEXC'
024500960326     C                   PARM                    COMM
024600960326     C                   PARM                    LUNG
024700960326     C*
024800960326   X1C                   ELSE
024900960326     C                   OPEN      FNAWK01L
025000960329     C                   OPEN      FNAWK03L
025100960326    1C                   ENDIF
025200960326     C                   ENDSR
025300960326     C*
025400960322     C*--- SCRIVO FILE DI WORK ---------------------------------------*
025500960322     C     WRTAWK        BEGSR
025600960322     C*
025700960322     C* AGGANCIO LA BOLLA
025800960322     C     KBLP          CHAIN     FNBLP01L                           31
025900980518     C* SE NON TROVO LA BOLLA SIGNIFICA CHE SI TRATTA DI UNA VARIAZIONE
026000980518     C*  LEGATA ALLA BOLLA IN IARRIVO (LOCALE): LA IGNORO
026100980518     C     *IN31         IFEQ      *OFF
026200960322     C*
026300961015     C* SCRIVO IL FILE DI WORK SOLO SE ARBVLB E' DIVERSO DA BLPVLF
026400961015    1C     ARBVLB        IFNE      BLPVLF
026500960322     C                   SETON                                        15
026600960322     C*
026700960322     C* IMPOSTO IL CODICE CLIENTE
026800960322    2C     BLPCCM        IFGT      0
026900960322     C                   MOVEL     BLPCCM        WKSC
027000960322   X2C                   ELSE
027100960322     C                   MOVEL     BLPKSC        WKSC
027200960322    2C                   ENDIF
027300960322     C*
027400961016     C* SE IMMESSO CONTROLLO IL CLIENTE
027500961015    2C     D15KSC        IFGT      0
027600961015     C     D15KSC        ANDNE     WKSC
027700960322     C                   SETOFF                                       15
027800960322    2C                   ENDIF
027900960322     C*
028000961016     C* SE IMMESSA CONTROLLO LA DATA VARIAZIONE
028100961016    2C   15D15DVD        IFGT      0
028200961015     C     ARBDTV        IFLT      D15DVD
028300961015     C     ARBDTV        ORGT      D15DVA
028400960322     C                   SETOFF                                       15
028500960322     C                   ENDIF
028600961016    2C                   ENDIF
028700961016     C* SE IMMESSA CONTROLLO LA LINEA DI ARRIVO
028800110413     c* ES - non è la linea di arrivo perchè potrebbe essere anche la partenza
028900110413     c*      a fare la variazione
029000961016    2C   15D15LNA        IFGT      0
029100110413     c                   eval      kpru=%subst(arbpru:1:9)
029200110413     c     kpru          chain     azute01l
029300110413     c                   if        %found(azute01l) and utefil<>d15lna
029400110413     C                   SETOFF                                       15
029500110413    2C                   ENDIF
029600110413     c
029700110413     C**   D15LNA        ANDNE     BLPLNA
029800110413     C**                 SETOFF                                       15
029900110413    2C**                 ENDIF
030000110413    2C                   ENDIF
030100960322     C*
030200960322     C* 15 ON  - RECORD DA SCRIVERE NEL FILE DI WORK
030300960322    2C     *IN15         IFEQ      *ON
030400960327     C                   MOVEL     ARBAAS        AWKAAS
030500960327     C                   MOVEL     ARBLNP        AWKLNP
030600960327     C                   MOVEL     ARBNRS        AWKNRS
030700960327     C                   MOVEL     ARBNSP        AWKNSP
030800960328     C                   MOVEL     BLPLNA        AWKLNA
030900960322     C                   MOVEL     BLPMGS        AWKMGS
031000960326     C                   MOVEL     WKSC          AWKKSC
031100960322     C                   MOVEL     BLPRSM        AWKRSM
031200960322     C                   MOVEL     BLPINM        AWKINM
031300960322     C                   MOVEL     BLPCAM        AWKCAM
031400960322     C                   MOVEL     BLPLOM        AWKLOM
031500960322     C                   MOVEL     BLPPRM        AWKPRM
031600960322     C                   MOVEL     BLPNZM        AWKNZM
031700960322     C                   MOVEL     BLPRMN        AWKRMN
031800960401     C                   MOVEL     BLPRSD        AWKRSD
031900960322     C                   MOVEL     BLPNCL        AWKNCL
032000960327     C                   MOVEL     BLPPKF        AWKPKF
032100961016     C                   CLEAR                   AWKPKV
032200961016     C                   MOVEL     BLPVLF        AWKVLF
032300961016     C                   MOVEL     BLPFVF        AWKFVF
032400961016     C                   MOVEL     ARBVLB        AWKVLV
032500961016     C                   MOVEL     ARBFVB        AWKFVV
032600960322     C                   MOVEL     ARBDTV        AWKDTV
032700960322     C                   MOVEL     ARBORV        AWKORV
032800960328     C                   MOVEL     BLPSOP        AWKSOP
032900960401     C   09              MOVEL     ' '           AWKESV
033000960401     C  N09              MOVEL     'S'           AWKESV
033100960322     C                   WRITE     FNAWK000
033200960322    2C                   ENDIF
033300960322     C*
033400960322   X1C                   ELSE
033500961015     C* ARBVLB = BLPVLF ---> FLAGGO IL RECORD PER NON LEGGERLO PIU'
033600960322     C                   MOVEL     'N'           ARBFST
033700960322     C                   UPDATE    FNARBD00
033800960322    1C                   ENDIF
033900980518    1C                   ENDIF
034000960322     C*
034100960322     C                   ENDSR
034200960322     C*
034300960326     C*--- STAMPO DETTAGLIO ------------------------------------------*
034400960326     C     STADET        BEGSR
034500960329     C                   SETON                                        22
034600960329     C                   SETOFF                                       09
034700960326     C*
034800960326     C****  STAMPO TESTATA  ****
034900960329     C     *INOF         IFEQ      *ON
035000960401     C   01              EXCEPT    PIEDI
035100960401     C                   SETON                                        1101
035200960327     C                   EXCEPT    TES
035300960329     C                   SETOFF                                       OF
035400960326     C                   ENDIF
035500960326     C*
035600960326     C****  STAMPO DETTAGLIO  ****
035700960326     C* 12 ON  - NUMERO SERIE <> 0  ---> STAMPO TRATTINO SEPARATORE
035800960326     C     AWKNRS        COMP      0                                  1212
035900960326     C*
036000960326     C* GIRO DATA SPEDIZIONE
036100960326     C                   Z-ADD     MES           GGMM
036200960326     C                   MOVEL     GIO           GGMM              4 0
036300960326     C                   Z-ADD     AWKAAS        STADSP
036400960328     C                   MOVEL     GGMM          STADSP            8 0
036500960326     C*
036600960401     C* RAGIONE SOCIALE MITTENTE E DESTINATARIO
036700960328     C                   MOVEL     AWKRSM        STARSM           20
036800960328     C                   MOVEL     AWKRSD        STARSD           20
036900960326     C*
037000960401     C     AWKESV        IFEQ      ' '
037100960329     C                   ADD       1             TOTBOL            9 0
037200960329     C                   ELSE
037300960329     C                   SETON                                        09
037400960329     C                   ENDIF
037500960329     C*
037600960326     C                   EXCEPT    DETT
037700960326     C                   ENDSR
037800960326     C*
037900960321     C*--- SR INIZIALE -----------------------------------------------*
038000960321     C     *INZSR        BEGSR
038100960321     C****
038200960321     C** KLIST
038300960321     C****
038400960322     C* ACCESSO FNBLP01L
038500960322     C     KBLP          KLIST
038600960322     C                   KFLD                    ARBAAS
038700960322     C                   KFLD                    ARBLNP
038800960322     C                   KFLD                    ARBNRS
038900960322     C                   KFLD                    ARBNSP
039000020709
039100020709     c     Karbd1        Klist
039200020709     c                   Kfld                    Kaas
039300020709     c                   Kfld                    D15lnp
039400020709
039500960326     C****
039600960321     C** DEFINIZIONE CAMPI
039700960321     C****
039800960401     C     *LIKE         DEFINE    AWKKSC        WKSC
039900960329     C     *LIKE         DEFINE    AWKSOP        WSOP
040000961015     C     *LIKE         DEFINE    D15DVD        STADVD
040100961015     C     *LIKE         DEFINE    D15DVA        STADVA
040200960329     C     *LIKE         DEFINE    AWKSPE        WSPE
040300960321     C*
040400960325     C****  PRENDO DATA E ORA CORRENTE  ****
040500960325     C                   TIME                    W0140            14 0
040600960325     C                   MOVE      W0140         UDATE8            8 0
040700960329     C                   MOVEL     W0140         UTIME             6 0
040800960329     C*
040900960329     C* ACCENDO INDICATORE DI OVERFLOW
041000960329     C                   SETON                                        OF
041100960329     C*
041200960329     C                   Z-ADD     1             CODUT
041300960329     C                   CALL      'X§PARUT'
041400960329     C                   PARM                    UT§DSE
041500960329     C                   MOVEL     RAGUT         RSUT             20
041600960327     C*
041700960326     C****  PARAMETRI PASSATI  ****
041800960326     C     *ENTRY        PLIST
041900960326     C                   PARM                    KPJBA
042000961015     C                   MOVEL     KPJBU         DSLS15
042100960329     C*
042200960329     C****  CODICE CLIENTE  ****
042300961015     C     D15KSC        IFGT      0
042400960329     C                   SETON                                        06
042500960329     C                   ENDIF
042600960329     C*
042700960325     C****  DATA VARIAZIONE  ****
042800960325     C* SE DATA VARIAZIONE AL = 0 --> METTO DATA DAL IN DATA AL
042900961016    1C     D15DVD        IFGT      0
043000960328     C                   SETON                                        07
043100961015     C     D15DVA        IFEQ      0
043200961015     C                   Z-ADD     D15DVD        D15DVA
043300960328     C                   ENDIF
043400960329     C* GIRO DATA VARIAZIONE DAL
043500961015     C                   Z-ADD     D15DVD        WDATA
043600960329     C                   Z-ADD     MM            GGMM              4 0
043700960328     C                   MOVEL     GG            GGMM
043800960329     C                   Z-ADD     AA            STADVD
043900960329     C                   MOVEL     GGMM          STADVD
044000960329     C* GIRO DATA VARIAZIONE  AL
044100961015     C                   Z-ADD     D15DVA        WDATA
044200960329     C                   Z-ADD     MM            GGMM
044300960329     C                   MOVEL     GG            GGMM
044400960329     C                   Z-ADD     AA            STADVA
044500960329     C                   MOVEL     GGMM          STADVA
044600961016    1C                   ENDIF
044700961016     C*
044800960329     C****  LINEA ARRIVO  ****
044900961016    1C     D15LNA        IFGT      0
045000960329     C                   SETON                                        08
045100961016    1C                   ENDIF
045200960329     C*
045300920925     C                   ENDSR
045400960326     C*
045500960325     O*---------------------------------------------------------------*
045600960328     O****  TESTATA  ****
045700960328     OQSYSPRT   E            TES              02
045800960328     O                       ST(1)               66
045900960328     O                       ST(2)              132
046000960328     O                       RSUT                20
046100960328     O                                           21 '/'
046200961015     O                       D15FEL              24
046300960328     O                       UDATE8             117 '  /  /    '
046400960328     O                       PAGE          Z    132
046500960328     O          E            TES              03
046600960328     O                       KNSIF               10
046700960328     O                       KNMUS               24
046800960328     O                       UTIME              115 '  :  :  '
046900960328     O          E            TES              05
047000960328     O                                           30 'LINEA PARTENZA:'
047100961015     O                       D15LNP              34
047200960328     O                     06                    51 'COD.CLIENTE:'
047300961015     O                     06D15KSC        Z     59
047400960328     O                     07                    83 'DATA VARIAZIONE DAL'
047500960328     O                     07STADVD              94 '  /  /    '
047600960328     O                     07                    97 'AL'
047700960328     O                     07STADVA             108 '  /  /    '
047800110413     O                     08                   126 'FIL. VARIAZIONE:'
047900961015     O                     08D15LNA             130
048000960328     O*
048100960328     O          E            TES              07
048200960328     O                       ST(5)               66
048300960328     O                       ST(8)              132
048400960328     O          E            TES            1 08
048500960328     O                       ST(6)               66
048600960328     O                       ST(9)              132
048700960328     O****  DETTAGLIO  ****
048800960328     O          E            DETT        1
048900960329     O                     11AWKSOP              03
049000961016     O                       AWKLNP              10
049100961016     O                     12AWKNRS        Z     13
049200961016     O                     12                    14 '-'
049300961016     O                       AWKNSP        Z     21
049400961016     O                       STADSP              32 '  /  /    '
049500961016     O                       AWKLNA              36
049600961016     O                       STARSM              58
049700961016     O                       STARSD              79
049800961016     O                       AWKRMN        Z     95
049900961016     O                       AWKNCL        Z    102
050000961016     O                       AWKPKF        4    111
050100961016     O                       AWKFVV             114
050200961016     O                       AWKVLV        4    120
050300961016     O                       AWKFVF             123
050400961016     O                       AWKVLF        4    129
050500961016     O                     09                   132 '*'
050600960401     O*
050700960401     O          E            PIEDI            65
050800960401     O                       ST(13)              66
050900960401     O*
051000960328     O****  TOTALE BOLLE VARIATE PER BOLLETTISTA  ****
051100960328     O          E            TOT         2  3
051200960328     O                                           25 'TOTALE BOLLE :'
051300960328     O                       TOTBOL        2     37
051400960329     O****  FITTIZZIO PER IL SALTO GIUSTO DELLE RIGHE  ****
051500960329     O          E            FITT        1
051600960329     O                                              ' '
051700960329     O****  DICITURA FINE STAMPA  ****
051800960329     O          E            FINEST      2
051900960329     O                       ST(11)              66
052000960329     O                       ST(12)             132
052100960326     O*---------------------------------------------------------------*
052200960326** ST
052300961016XXXXXXXXXXXXXXXXXXXX/XXX       ***   E L E N C O    V A R I A Z I
052400961016O N I    V O L U M E   ***   FNLS15R     XX/XX/XXXX      PAG. XXXX   2
052500961016               LINEA PARTENZA: XXX     COD.CLIENTE: XXXXXXX     DA
052600110413TA VARIAZIONE DAL XX/XX/XXXX AL XX/XX/XXXX     FIL.VARIAZIO: XXX
052700961016SIGLA  S  P  E  D  I  Z  I  O  N  E   MITTENTE             DESTINA  5
052800961016OPER.  LNP   NUMERO     DATA     LNA                                6
052900961016  X    XXX XX-XXXXXXX XX/XX/XXXX XXX  XXXXXXXXXXXXXXXXXXXX XXXXXXX
053000961016TARIO             RIFERIMENTO  COLLI PESO KG.   VOLUME   VOLUME     8
053100961016                     MITTENTE                          MODIFICATO   9
053200961016XXXXXXXXXXXXX XXXXXXXXXXXXXXX  XXXXX XXXXXX,X  XXX,XXX  XXX,XXX  *
053300960329                                             * * *     F I N E      11
053400960329  S T A M P A    * * *                                              12
053500961016 * = VARIAZIONI EFFETTUATE PIU' VOLTE SULLA STESSA SPEDIZIONE       13
053600961016                                                                    14
053700960326** CMD1
053800960326CLRPFM FILE(FNAWK00F)                                                 1
053900020709** CMD2
054000020709CRTDUPOBJ OBJ(FNAWK*) FROMLIB(XXXXXXXXXX) OBJTYPE(*FILE) TOLIB(QTEMP)
054100960326** MSG
054200960326SNDMSG MSG('ATTENZIONE!! FILE FNAWK00F GIA'' UTILIZZATO DA UN ALTRO   1
054300960328 LAVORO !!')                                                          2
054400960326                                                    TOUSR(*SYSOPR)    3
054500020709SNDMSG MSG('ATTENZIONE!! FILE FNAWK00F NON TROVATO NEL SISTEMA!!')    4
054600020709SNDMSG MSG('ATTENZIONE!! FILE FNAWK00F NON DUPLICATO IN QTEMP !!')    5
