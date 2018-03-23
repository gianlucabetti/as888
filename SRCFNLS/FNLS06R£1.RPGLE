000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200960328     H* FNLS06R *-----------------------------------------------------*
000300960321     H*-----------------*                                             *
000400960328     H*  ELENCO VARIAZIONI PESO CAUSATE DAL BOLLETTISTA     -BATCH-   **
000500960321     H*---------------------------------------------------------------*
000600960329     FFNAWK01L  O  A E           K DISK    USROPN
000700960328     FFNAWK03L  IF   E           K DISK    USROPN
000800960328     F                                     RENAME(FNAWK000:FNAWK003)
000900991005     FFIARBF1C  UF   E           K DISK
001000960322     FFNBLP01L  IF   E           K DISK
001100960328     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001200020703
001300020709     d kaas            s                   like(arbaas) inz
001400020703
001500960321     D*
001600960321     D* DEFINIZIONE SCHIERE
001700960326     D ER              S              1    DIM(201)
001800960401     D ST              S             66    DIM(14) CTDATA PERRCD(1)
001900960326     D CMD1            S             21    DIM(1) CTDATA PERRCD(1)
002000020709     D CMD2            S             69    DIM(1) CTDATA PERRCD(1)
002100020709     D MSG             S             67    DIM(5) CTDATA PERRCD(1)
002200941223     D*
002300960321     D* PASSAGGIO DATI DAL FILTRO                         - FNLS07R -
002400960321     D DSLS08        E DS                  EXTNAME(FNLS08DS)
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
004800960321     C*****************************************************************
004900960321     C* RIEPILOGO INDICATORI
005000960321     C*****************************************************************
005100960401     C* 01    - STAMPA DEI PIEDI
005200960401     C* 05    - SONO IN RISTAMPA
005300960328     C* 06    - IMMESSO UNO SPECIFICO CODICE CLIENTE
005400960328     C* 07    - IMMESSA LA DATA VARIAZIONE
005500960328     C* 08    - IMMESSA SPECIFICA LINEA DI ARRIVO
005600960328     C* 09    - VARIAZIONE CAUSATA DA UNA ERRATA CORREZIONE APPORTATA
005700960328     C*         ALLA FILIALE DI ARRIVO
005800960329     C* 11    - USATO PER STAMPARE LA SIGLA OPERATORE SOLO LA 1° VOLTA
005900960326     C* 12    - NUMERO SERIE <> 0  --->  STAMPO TRATTINO SEPARATORE
006000960322     C* 15    - RECORD DA SCRIVERE NEL FILE DI WORK
006100960329     C* 17    - USATO PER NON STAMPARE LA 1° VOLTA IL TOTALE BOLLE
006200960329     C* 22 OFF- NON C'E' NULLA DA STAMPARE: STAMPO LA TESTATA
006300960326     C* 30/32 - DI COMODO
006400020709     C* 99    - errore esco dal pgm
006500960321     C*****************************************************************
006600960326     C*
006700020709      * Reperisco la libreria in cui si trova il file di work
006800020709     c                   ExSr      RtnLib
006900020709     c   99              GoTo      Fine
007000020703      * creo il file di work in qtemp
007100020703     c                   exsr      crtwrk
007200020709     c   99              GoTo      Fine
007300960326     C****  ESEGUO CLEAR DEL FILE DI WORK  ****
007400960326     C                   EXSR      CLRAWK
007500960326     C   99              GOTO      FINE
007600920925     C*
007700960325     C****  E L A B O R A Z I O N E  ****
007800020703     C**!!!*LOVAL        SETLL     FNARBD00
007900020703     c     kaas          setgt     fnarbd00
008000991005     C                   READ      FNARBD00                               30
008100020709     c                   eval      kaas = arbaas
008200020703
008300020703     c     leggi         tag
008400020709     c     karbd1        setll     fnarbd00
008500020709     c     karbd1        reade     fnarbd00                               30
008600020703
008700960322     C*
008800960322    1C     *IN30         DOWEQ     *OFF
008900960322     C*                      - CON LINEA PARTENZA IMMESSA A VIDEO
009000110708    2C     ARBLNP        ifeq      D08LNP
009100960322     C*                      - CHE SONO DA STAMPARE O DA RISTAMPARE
009200960322     C     ARBFST        ANDNE     'N'
009300110708     C* LEGGO tutte le causali variazioni di peso e non solo IB
009400110708    2C***  ARBCVB        IFEQ      'IB'
009500110708   2ac                   if        arbcvb='IB'  or arbcvb='IC'
009600960322     C                   CLEAR                   WELAB
009700960401     C**
009800960401     C* SE QUI SONO ENTRATA MEMORIZZO IL NUMERO DI SPEDIZIONE
009900960401     C     AWKSPE        IFNE      WSPE
010000960401     C* SE ESISTONO PIU' VARIAZIONI PER LA STESSA SPEDIZIONE CONTO
010100960401     C*   SOLO LA PRIMA PERCHE' LE ALTRE SONO VARIAZIONI FATTE A CAUSA
010200960401     C*   DI UN INSERIMENTO ERRATO DA PARTE DELLA FILIALE DI ARRIVO
010300960401     C                   SETON                                        09
010400960401     C                   MOVEL     AWKSPE        WSPE
010500960401     C                   ELSE
010600960401     C                   SETOFF                                       09
010700960401     C                   ENDIF
010800960322     C*
010900960322     C* RICHIESTA LA STAMPA
011000960322    3C     D08RIS        IFEQ      'NO'
011100960322     C     ARBFST        IFEQ      ' '
011200960322     C                   MOVEL     'S'           WELAB             1
011300960322     C                   ENDIF
011400960322     C*
011500960322   X3C                   ELSE
011600960322     C* RICHIESTA LA RISTAMPA
011700960322     C     ARBFST        IFEQ      'S'
011800960322     C                   MOVEL     'S'           WELAB
011900960322     C                   ENDIF
012000960322    3C                   ENDIF
012100960322     C*
012200960322     C* WELAB = 'S' ---> RECORD DA ELABORARE
012300960322    3C     WELAB         IFEQ      'S'
012400960322     C                   EXSR      WRTAWK
012500960322    3C                   ENDIF
012600960322     C*
012700110708   2aC                   ENDIF
012800110708    2C                   ENDIF
012900110708     c
013000020703     C**!!!              READ      FNARBD00                               30
013100020703     c     karbd1        reade     fnarbd00                               30
013200960322    1C                   ENDDO
013300020703
013400020709      * devo leggere l'anno successivo
013500020703     c     kaas          setgt     fnarbd00
013600020703     c                   read      fnarbd00                               30
013700020703     c                   if        not *in30
013800020709     c                   eval      kaas = arbaas
013900020703     c                   goto      leggi
014000020703     c                   endif
014100960322     C*
014200960325     C****  S T A M P A  ****
014300960328     C     *LOVAL        SETLL     FNAWK03L
014400960328     C                   READ      FNAWK03L                               30
014500960326     C*
014600960328    1C     *IN30         DOWEQ     *OFF
014700960329     C                   SETOFF                                       11
014800960329     C*
014900960329     C  N17              MOVEL     AWKSOP        WSOP
015000960329     C  N17              SETON                                        1711
015100960322     C*
015200960329     C* A CAMBIO SIGLA OPERATORE STAMPO TOTALE BOLLE
015300960329    2C     AWKSOP        IFNE      WSOP
015400960329     C                   SETON                                        11
015500960329     C     *INOF         IFEQ      *ON
015600960401     C   01              EXCEPT    PIEDI
015700960401     C                   EXCEPT    TES
015800960329     C                   SETOFF                                       OF
015900960401     C                   SETON                                        01
016000960329     C                   ENDIF
016100960329     C                   EXCEPT    TOT
016200960329     C*
016300960329     C                   CLEAR                   TOTBOL
016400960329     C                   MOVEL     AWKSOP        WSOP
016500960322    2C                   ENDIF
016600960329     C*
016700960329     C* STAMPO DETTAGLIO
016800960329     C                   EXSR      STADET
016900960326     C*
017000960326     C* SOLO PER STAMPA: AGGIORNO IL FLAG DI STAMPA
017100960329    2C     D08RIS        IFEQ      'NO'
017200991005     C     KARBD         CHAIN     FNARBD00                           31
017300960326     C     *IN31         IFEQ      *OFF
017400960326     C                   MOVEL     'S'           ARBFST
017500991005     C                   UPDATE    FNARBD00
017600960326     C                   ENDIF
017700960329    2C                   ENDIF
017800960326     C*
017900960328     C                   READ      FNAWK03L                               30
018000960322    1C                   ENDDO
018100960322     C*
018200960329     C* STAMPO ULTIMO TOTALE BOLLE
018300960329    1C   17*INOF         IFEQ      *ON
018400960401     C   01              EXCEPT    PIEDI
018500960401     C                   EXCEPT    TES
018600960329     C                   SETOFF                                       OF
018700960401     C                   SETON                                        01
018800960329    1C                   ENDIF
018900960329     C   17              EXCEPT    TOT
019000960326     C*
019100960329     C****  SE NON C'E' NIENTE DA STAMPARE STAMPO LA TESTATA  ****
019200960329    1C     *IN22         IFEQ      *OFF
019300960401     C   01              EXCEPT    PIEDI
019400960329     C                   EXCEPT    TES
019500960401     C                   SETON                                        01
019600960329    2C                   DO        10
019700960329     C                   EXCEPT    FITT
019800960329    2C                   ENDDO
019900960329    1C                   ENDIF
020000960329     C*
020100960329     C****  STAMPA DICITURA: FINE STAMPA  ****
020200960401     C                   EXCEPT    FINEST
020300960401     C                   EXCEPT    PIEDI
020400960329     C*
020500960326     C     FINE          TAG
020600000000     C                   SETON                                        LR
020700020703     C*
020800020709      *---------------------------------------------------------------*
020900020709      *   Reperisco la libreria del file di work
021000020709      *---------------------------------------------------------------*
021100020709     c     RtnLib        BegSr
021200020709
021300020709     c                   Call      'TRUL29C'
021400020709     c                   Parm                    Tr29Obj
021500020709     c                   Parm                    Tr29Typ
021600020709     c                   Parm                    Tr29Lib
021700020709      * Se la libreria ritorna a blank vuol dire che non ha trovato il file
021800020709      * in questo caso chiudo il pgm e invio un msg alla coda dell'opratore
021900020709    1c                   If        Tr29Lib = *blanks
022000020709     c                   clear                   Er
022100020709     c                   Movea     Msg(4)        Er(1)
022200020709     c                   Movea     Msg(3)        Er(135)
022300020709     c                   Movea     Er            Comm
022400020709     c                   z-add     201           Lung
022500020709     c                   Call      'QCMDEXC'
022600020709     c                   Parm                    Comm
022700020709     c                   Parm                    Lung
022800020709     c                   Eval      *In99 = *On
022900020709    1c                   EndIf
023000020709
023100020709     c                   EndSr
023200020709      *---------------------------------------------------------------*
023300020709      *   Creo il file di work in qtemp
023400020709      *---------------------------------------------------------------*
023500020709     c     crtwrk        begsr
023600020709     c                   z-add     69            lung
023700020709     c                   clear                   comman           80
023800020709     c                   movea     cmd2(1)       comman
023900020709     c                   eval      %subst(comman:31:10) = Tr29Lib
024000020709     c                   call      'QCMDEXC'                            99
024100020709     c                   parm                    comman
024200020709     c                   parm                    lung
024300020709      * Duplica oggetti non andata a buon fine
024400020709      * in questo caso chiudo il pgm e invio un msg alla coda dell'opratore
024500020709    1c                   If        *In99
024600020709     c                   clear                   Er
024700020709     c                   Movea     Msg(5)        Er(1)
024800020709     c                   Movea     Msg(3)        Er(135)
024900020709     c                   Movea     Er            Comm
025000020709     c                   z-add     201           Lung
025100020709     c                   Call      'QCMDEXC'
025200020709     c                   Parm                    Comm
025300020709     c                   Parm                    Lung
025400020709    1c                   EndIf
025500020709
025600020709     c                   endsr
025700020709     C*
025800960326     C*--- CLEAR FILE DI WORK ----------------------------------------*
025900960326     C     CLRAWK        BEGSR
026000960326     C                   Z-ADD     21            LUNG
026100960326     C                   MOVEL     *BLANKS       COMMAN           80
026200960326     C                   MOVEA     CMD1(1)       COMMAN
026300960326     C                   CALL      'QCMDEXC'                            99
026400960326     C                   PARM                    COMMAN           80
026500960326     C                   PARM                    LUNG             15 5
026600960326     C*
026700960326     C* 99 ON  - CLEAR NON ANDATA A BUON FINE: INVIO MESSAGGIO ALLA
026800960326     C*   CODA DELL'OPERATORE ED ESCO DAL PGM
026900960326    1C     *IN99         IFEQ      *ON
027000960326     C                   MOVEA     MSG(1)        ER(1)
027100960326     C                   MOVEA     MSG(2)        ER(68)
027200960326     C                   MOVEA     MSG(3)        ER(135)
027300960326     C                   MOVEA     ER            COMM            201
027400960326     C                   Z-ADD     201           LUNG
027500960326     C                   CALL      'QCMDEXC'
027600960326     C                   PARM                    COMM
027700960326     C                   PARM                    LUNG
027800960326     C*
027900960326   X1C                   ELSE
028000960326     C                   OPEN      FNAWK01L
028100960329     C                   OPEN      FNAWK03L
028200960326    1C                   ENDIF
028300960326     C                   ENDSR
028400960326     C*
028500960322     C*--- SCRIVO FILE DI WORK ---------------------------------------*
028600960322     C     WRTAWK        BEGSR
028700960322     C*
028800960322     C* AGGANCIO LA BOLLA
028900960322     C     KBLP          CHAIN     FNBLP01L                           31
029000960322     C*
029100960327     C* SCRIVO IL FILE DI WORK SOLO SE ARBPKB E' DIVERSO DA BLPPKF
029200960401    1C     ARBPKB        IFNE      BLPPKF
029300960322     C                   SETON                                        15
029400960322     C*
029500960322     C* IMPOSTO IL CODICE CLIENTE
029600960322    2C     BLPCCM        IFGT      0
029700960322     C                   MOVEL     BLPCCM        WKSC
029800960322   X2C                   ELSE
029900960322     C                   MOVEL     BLPKSC        WKSC
030000960322    2C                   ENDIF
030100960322     C*
030200960322     C* SIA PER STAMPA CHE RISTAMPA: SE IMMESSO CONTROLLO IL CLIENTE
030300960322    2C     D08KSC        IFGT      0
030400960322     C     D08KSC        ANDNE     WKSC
030500960322     C                   SETOFF                                       15
030600960322    2C                   ENDIF
030700960322     C*
030800960322    2C   15D08RIS        IFEQ      'SI'
030900960322     C* SOLO PER RISTAMPA: SE IMMESSA CONTROLLO LA DATA VARIAZIONE
031000960322    3C     D08DVD        IFGT      0
031100960322     C     ARBDTV        IFLT      D08DVD
031200960322     C     ARBDTV        ORGT      D08DVA
031300960322     C                   SETOFF                                       15
031400960322     C                   ENDIF
031500960322    3C                   ENDIF
031600960322     C* SOLO PER RISTAMPA: SE IMMESSA CONTROLLO LA LINEA DI ARRIVO
031700960322    3C   15D08LNA        IFGT      0
031800960322     C     D08LNA        ANDNE     BLPLNA
031900960322     C                   SETOFF                                       15
032000960322    3C                   ENDIF
032100960322    2C                   ENDIF
032200960322     C*
032300960322     C* 15 ON  - RECORD DA SCRIVERE NEL FILE DI WORK
032400960322    2C     *IN15         IFEQ      *ON
032500960327     C                   MOVEL     ARBAAS        AWKAAS
032600960327     C                   MOVEL     ARBLNP        AWKLNP
032700960327     C                   MOVEL     ARBNRS        AWKNRS
032800960327     C                   MOVEL     ARBNSP        AWKNSP
032900960328     C                   MOVEL     BLPLNA        AWKLNA
033000960322     C                   MOVEL     BLPMGS        AWKMGS
033100960326     C                   MOVEL     WKSC          AWKKSC
033200960322     C                   MOVEL     BLPRSM        AWKRSM
033300960322     C                   MOVEL     BLPINM        AWKINM
033400960322     C                   MOVEL     BLPCAM        AWKCAM
033500960322     C                   MOVEL     BLPLOM        AWKLOM
033600960322     C                   MOVEL     BLPPRM        AWKPRM
033700960322     C                   MOVEL     BLPNZM        AWKNZM
033800960322     C                   MOVEL     BLPRMN        AWKRMN
033900960401     C                   MOVEL     BLPRSD        AWKRSD
034000960322     C                   MOVEL     BLPNCL        AWKNCL
034100960327     C                   MOVEL     BLPPKF        AWKPKF
034200960401     C                   MOVEL     ARBPKB        AWKPKV
034300960322     C                   MOVEL     ARBDTV        AWKDTV
034400960322     C                   MOVEL     ARBORV        AWKORV
034500960328     C                   MOVEL     BLPSOP        AWKSOP
034600960401     C   09              MOVEL     ' '           AWKESV
034700960401     C  N09              MOVEL     'S'           AWKESV
034800960322     C                   WRITE     FNAWK000
034900960322    2C                   ENDIF
035000960322     C*
035100960322   X1C                   ELSE
035200960327     C* ARBPKB = BLPPKF ---> FLAGGO IL RECORD PER NON LEGGERLO PIU'
035300960322     C*   NEMMENO SE RICHIESTA LA RISTAMPA
035400960322     C                   MOVEL     'N'           ARBFST
035500991005     C                   UPDATE    FNARBD00
035600960322    1C                   ENDIF
035700960322     C*
035800960322     C                   ENDSR
035900960322     C*
036000960326     C*--- STAMPO DETTAGLIO ------------------------------------------*
036100960326     C     STADET        BEGSR
036200960329     C                   SETON                                        22
036300960329     C                   SETOFF                                       09
036400960326     C*
036500960326     C****  STAMPO TESTATA  ****
036600960329     C     *INOF         IFEQ      *ON
036700960401     C   01              EXCEPT    PIEDI
036800960401     C                   SETON                                        1101
036900960327     C                   EXCEPT    TES
037000960329     C                   SETOFF                                       OF
037100960326     C                   ENDIF
037200960326     C*
037300960326     C****  STAMPO DETTAGLIO  ****
037400960326     C* 12 ON  - NUMERO SERIE <> 0  ---> STAMPO TRATTINO SEPARATORE
037500960326     C     AWKNRS        COMP      0                                  1212
037600960326     C*
037700960326     C* GIRO DATA SPEDIZIONE
037800960326     C                   Z-ADD     MES           GGMM
037900960326     C                   MOVEL     GIO           GGMM              4 0
038000960326     C                   Z-ADD     AWKAAS        STADSP
038100960328     C                   MOVEL     GGMM          STADSP            8 0
038200960326     C*
038300960401     C* RAGIONE SOCIALE MITTENTE E DESTINATARIO
038400960328     C                   MOVEL     AWKRSM        STARSM           20
038500960328     C                   MOVEL     AWKRSD        STARSD           20
038600960326     C*
038700960401     C     AWKESV        IFEQ      ' '
038800960329     C                   ADD       1             TOTBOL            9 0
038900960329     C                   ELSE
039000960329     C                   SETON                                        09
039100960329     C                   ENDIF
039200960329     C*
039300960326     C                   EXCEPT    DETT
039400960326     C                   ENDSR
039500960326     C*
039600960321     C*--- SR INIZIALE -----------------------------------------------*
039700960321     C     *INZSR        BEGSR
039800960321     C****
039900960321     C** KLIST
040000960321     C****
040100960322     C* ACCESSO FNBLP01L
040200960322     C     KBLP          KLIST
040300960322     C                   KFLD                    ARBAAS
040400960322     C                   KFLD                    ARBLNP
040500960322     C                   KFLD                    ARBNRS
040600960322     C                   KFLD                    ARBNSP
040700991005     C* ACCESSO FIARBF1C
040800960326     C     KARBD         KLIST
040900960326     C                   KFLD                    AWKAAS
041000960326     C                   KFLD                    AWKLNP
041100960326     C                   KFLD                    AWKNRS
041200960326     C                   KFLD                    AWKNSP
041300960326     C                   KFLD                    AWKDTV
041400960326     C                   KFLD                    AWKORV
041500020703
041600020703     c     karbd1        klist
041700020703     c                   kfld                    kaas
041800020703     c                   kfld                    d08lnp
041900020703
042000960326     C****
042100960321     C** DEFINIZIONE CAMPI
042200960321     C****
042300960401     C     *LIKE         DEFINE    ARBPKB        WPKB
042400960401     C     *LIKE         DEFINE    AWKKSC        WKSC
042500960329     C     *LIKE         DEFINE    AWKSOP        WSOP
042600960329     C     *LIKE         DEFINE    D08DVD        STADVD
042700960329     C     *LIKE         DEFINE    D08DVA        STADVA
042800960329     C     *LIKE         DEFINE    AWKSPE        WSPE
042900960321     C*
043000960325     C****  PRENDO DATA E ORA CORRENTE  ****
043100960325     C                   TIME                    W0140            14 0
043200960325     C                   MOVE      W0140         UDATE8            8 0
043300960329     C                   MOVEL     W0140         UTIME             6 0
043400960329     C*
043500960329     C* ACCENDO INDICATORE DI OVERFLOW
043600960329     C                   SETON                                        OF
043700960329     C*
043800960329     C                   Z-ADD     1             CODUT
043900960329     C                   CALL      'X§PARUT'
044000960329     C                   PARM                    UT§DSE
044100960329     C                   MOVEL     RAGUT         RSUT             20
044200960327     C*
044300960326     C****  PARAMETRI PASSATI  ****
044400960326     C     *ENTRY        PLIST
044500960326     C                   PARM                    KPJBA
044600960326     C                   MOVEL     KPJBU         DSLS08
044700960329     C*
044800960329     C****  CODICE CLIENTE  ****
044900960329     C     D08KSC        IFGT      0
045000960329     C                   SETON                                        06
045100960329     C                   ENDIF
045200960329     C*
045300960329     C****  RISTAMPA  ****
045400960329    1C     D08RIS        IFEQ      'SI'
045500960329     C                   SETON                                        05
045600960329     C*
045700960325     C****  DATA VARIAZIONE  ****
045800960325     C* SE DATA VARIAZIONE AL = 0 --> METTO DATA DAL IN DATA AL
045900960329    2C     D08DVD        IFGT      0
046000960328     C                   SETON                                        07
046100960328     C     D08DVA        IFEQ      0
046200960325     C                   Z-ADD     D08DVD        D08DVA
046300960328     C                   ENDIF
046400960329     C* GIRO DATA VARIAZIONE DAL
046500960328     C                   Z-ADD     D08DVD        WDATA
046600960329     C                   Z-ADD     MM            GGMM              4 0
046700960328     C                   MOVEL     GG            GGMM
046800960329     C                   Z-ADD     AA            STADVD
046900960329     C                   MOVEL     GGMM          STADVD
047000960329     C* GIRO DATA VARIAZIONE  AL
047100960329     C                   Z-ADD     D08DVA        WDATA
047200960329     C                   Z-ADD     MM            GGMM
047300960329     C                   MOVEL     GG            GGMM
047400960329     C                   Z-ADD     AA            STADVA
047500960329     C                   MOVEL     GGMM          STADVA
047600960329    2C                   ENDIF
047700960329     C****  LINEA ARRIVO  ****
047800960329    2C     D08LNA        IFGT      0
047900960329     C                   SETON                                        08
048000960329    2C                   ENDIF
048100960329    1C                   ENDIF
048200960329     C*
048300920925     C                   ENDSR
048400960326     C*
048500960325     O*---------------------------------------------------------------*
048600960328     O****  TESTATA  ****
048700960328     OQSYSPRT   E            TES              02
048800960328     O                       ST(1)               66
048900960328     O                       ST(2)              132
049000960328     O                       RSUT                20
049100960328     O                                           21 '/'
049200960328     O                       D08FEL              24
049300960328     O                       UDATE8             117 '  /  /    '
049400960328     O                       PAGE          Z    132
049500960328     O          E            TES              03
049600960328     O                       KNSIF               10
049700960328     O                       KNMUS               24
049800960328     O                       UTIME              115 '  :  :  '
049900960328     O          E            TES              05
050000960328     O                     05                    10 'RISTAMPA'
050100960328     O                                           30 'LINEA PARTENZA:'
050200960329     O                       D08LNP              34
050300960328     O                     06                    51 'COD.CLIENTE:'
050400960328     O                     06D08KSC        Z     59
050500960328     O                     07                    83 'DATA VARIAZIONE DAL'
050600960328     O                     07STADVD              94 '  /  /    '
050700960328     O                     07                    97 'AL'
050800960328     O                     07STADVA             108 '  /  /    '
050900960328     O                     08                   126 'LINEA ARRIVO:'
051000960328     O                     08D08LNA             130
051100960328     O*
051200960328     O          E            TES              07
051300960328     O                       ST(5)               66
051400960328     O                       ST(8)              132
051500960328     O          E            TES            1 08
051600960328     O                       ST(6)               66
051700960328     O                       ST(9)              132
051800960328     O****  DETTAGLIO  ****
051900960328     O          E            DETT        1
052000960329     O                     11AWKSOP              03
052100960328     O                       AWKLNP              12
052200960328     O                     12AWKNRS        Z     15
052300960328     O                     12                    16 '-'
052400960328     O                       AWKNSP        Z     23
052500960328     O                       STADSP              34 '  /  /    '
052600960328     O                       AWKLNA              38
052700960328     O                       STARSM              60
052800960328     O                       STARSD              82
052900960328     O                       AWKRMN        Z     99
053000960328     O                       AWKNCL        Z    106
053100960328     O                       AWKPKV        4    116
053200960328     O                       AWKPKF        4    127
053300960401     O                     09                   130 '*'
053400960401     O*
053500960401     O          E            PIEDI            65
053600960401     O                       ST(13)              66
053700960401     O                       ST(14)             132
053800960401     O*
053900960328     O****  TOTALE BOLLE VARIATE PER BOLLETTISTA  ****
054000960328     O          E            TOT         2  3
054100960328     O                                           25 'TOTALE BOLLE :'
054200960328     O                       TOTBOL        2     37
054300960329     O****  FITTIZZIO PER IL SALTO GIUSTO DELLE RIGHE  ****
054400960329     O          E            FITT        1
054500960329     O                                              ' '
054600960329     O****  DICITURA FINE STAMPA  ****
054700960329     O          E            FINEST      2
054800960329     O                       ST(11)              66
054900960329     O                       ST(12)             132
055000960326     O*---------------------------------------------------------------*
055100960326** ST
055200960401XXXXXXXXXXXXXXXXXXXX/XXX         ** VARIAZIONI PESO SU SPEDIZIONI
055300960401ERRONEAMENTE BOLLETTATE **   FNLS06R     XX/XX/XXXX      PAG. XXXX   2
055400960328  RISTAMPA     LINEA PARTENZA: XXX     COD.CLIENTE: XXXXXXX     DA
055500960328TA VARIAZIONE DAL XX/XX/XXXX AL XX/XX/XXXX     LINEA ARRIVO: XXX
055600960328SIGLA    S  P  E  D  I  Z  I  O  N  E   MITTENTE              DEST   5
055700960328OPERAT.  LNP   NUMERO     DATA     LNA                               6
055800960328  X      XXX XX-XXXXXXX XX/XX/XXXX XXX  XXXXXXXXXXXXXXXXXXXX  XXXX
055900960328INATARIO              RIFERIMENTO  COLLI  PESO KG.  PESO REALE       8
056000960328                         MITTENTE                                    9
056100960401XXXXXXXXXXXXXXXX  XXXXXXXXXXXXXXX  XXXXX  XXXXXX,X   XXXXXX,X  *
056200960329                                             * * *     F I N E      11
056300960329  S T A M P A    * * *                                              12
056400961016 * = VARIAZIONI EFFETTUATE PIU' VOLTE IN ARRIVO SULLA STESSA SPEDI  13
056500961016ZIONE                                                               14
056600960326** CMD1
056700960326CLRPFM FILE(FNAWK00F)                                                 1
056800020703** CMD2
056900020709CRTDUPOBJ OBJ(FNAWK*) FROMLIB(XXXXXXXXXX) OBJTYPE(*FILE) TOLIB(QTEMP)
057000960326** MSG
057100960326SNDMSG MSG('ATTENZIONE!! FILE FNAWK00F GIA'' UTILIZZATO DA UN ALTRO   1
057200960328 LAVORO !!')                                                          2
057300960326                                                    TOUSR(*SYSOPR)    3
057400020709SNDMSG MSG('ATTENZIONE!! FILE FNAWK00F NON TROVATO NEL SISTEMA!!')    4
057500020709SNDMSG MSG('ATTENZIONE!! FILE FNAWK00F NON DUPLICATO IN QTEMP !!')    5
