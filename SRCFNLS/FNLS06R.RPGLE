000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200960328     H* FNLS06R *-----------------------------------------------------*
000300960328     H*  ELENCO VARIAZIONI PESO CAUSATE DAL BOLLETTISTA     -BATCH-   **
000400960321     H*---------------------------------------------------------------*
000500960329     FFNAWK01L  O  A E           K DISK    USROPN
000600960328     FFNAWK03L  IF   E           K DISK    USROPN
000700960328     F                                     RENAME(FNAWK000:FNAWK003)
000800131014     FFIARBF2C  UF   E           K DISK
000900170522     F***FNBLP01L  IF   E           K DISK
001000170522     fTITAS30C  if   e           k disk    extfile(wTAS30) usropn
001100170522     fTITAa30C  if   e           k disk    extfile(wTAA30) usropn
001200960328     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001300020703
001400020709     d kaas            s                   like(arbaas) inz
001500020703
001600960321     D*
001700960321     D* DEFINIZIONE SCHIERE
001800960326     D ER              S              1    DIM(201)
001900960401     D ST              S             66    DIM(14) CTDATA PERRCD(1)
002000960326     D CMD1            S             21    DIM(1) CTDATA PERRCD(1)
002100020709     D CMD2            S             69    DIM(1) CTDATA PERRCD(1)
002200020709     D MSG             S             67    DIM(5) CTDATA PERRCD(1)
002300941223     D*
002400960321     D* PASSAGGIO DATI DAL FILTRO                         - FNLS07R -
002500960321     D DSLS08        E DS                  EXTNAME(FNLS08DS)
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
004300170522     d Azuteds       e ds                  Extname(Azute00f)
004400170522     d dDatiute      e ds
004500170522     d Tibs34ds      e ds                  Inz
004600170522     D BS69DS        E DS                  EXTNAME(TIBS69DS) INZ
004700170522     D ACODS         E DS                  EXTNAME(CNACO00F) INZ
004800170522     D INDDS         E DS                  EXTNAME(CNIND00F) INZ
004900170522     D CLPDS         E DS                  EXTNAME(CNCLP00F) INZ
005000170522     D CLSDS         E DS                  EXTNAME(FNCLS00F) INZ
005100020709
005200020709      * Parametri per pgm Trul29c
005300020709     d Tr29obj         s             10    inz('FNAWK00F  ')
005400020709     d Tr29typ         s              8    inz('*FILE   ')
005500020709     d Tr29lib         s             10    inz
005600170522     d wTAS30          s             21    inz('GAITRAGRU /TITAS30C')
005700170522     d wTAA30          s             21    inz('GAITRAGRU /TITAA30C')
005800960321     C*****************************************************************
005900960321     C* RIEPILOGO INDICATORI
006000960321     C*****************************************************************
006100960401     C* 01    - STAMPA DEI PIEDI
006200960401     C* 05    - SONO IN RISTAMPA
006300960328     C* 06    - IMMESSO UNO SPECIFICO CODICE CLIENTE
006400960328     C* 07    - IMMESSA LA DATA VARIAZIONE
006500960328     C* 08    - IMMESSA SPECIFICA LINEA DI ARRIVO
006600960328     C* 09    - VARIAZIONE CAUSATA DA UNA ERRATA CORREZIONE APPORTATA
006700960328     C*         ALLA FILIALE DI ARRIVO
006800960329     C* 11    - USATO PER STAMPARE LA SIGLA OPERATORE SOLO LA 1° VOLTA
006900960326     C* 12    - NUMERO SERIE <> 0  --->  STAMPO TRATTINO SEPARATORE
007000960322     C* 15    - RECORD DA SCRIVERE NEL FILE DI WORK
007100960329     C* 17    - USATO PER NON STAMPARE LA 1° VOLTA IL TOTALE BOLLE
007200960329     C* 22 OFF- NON C'E' NULLA DA STAMPARE: STAMPO LA TESTATA
007300960326     C* 30/32 - DI COMODO
007400020709     C* 99    - errore esco dal pgm
007500960321     C*****************************************************************
007600960326     C*
007700020709      * Reperisco la libreria in cui si trova il file di work
007800020709     c                   ExSr      RtnLib
007900020709     c   99              GoTo      Fine
008000020703      * creo il file di work in qtemp
008100020703     c                   exsr      crtwrk
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
009700020703     C**!!!*LOVAL        SETLL     FNARBD00
009800020703     c     kaas          setgt     fnarbd00
009900991005     C                   READ      FNARBD00                               30
010000020709     c                   eval      kaas = arbaas
010100020703
010200020703     c     leggi         tag
010300020709     c     karbd1        setll     fnarbd00
010400020709     c     karbd1        reade     fnarbd00                               30
010500020703
010600960322     C*
010700960322    1C     *IN30         DOWEQ     *OFF
010800960322     C*                      - CON LINEA PARTENZA IMMESSA A VIDEO
010900110708    2C     ARBLNP        ifeq      D08LNP
011000960322     C*                      - CHE SONO DA STAMPARE O DA RISTAMPARE
011100960322     C     ARBFST        ANDNE     'N'
011200110708     C* LEGGO tutte le causali variazioni di peso e non solo IB
011300110708    2C***  ARBCVB        IFEQ      'IB'
011400110708   2ac                   if        arbcvb='IB'  or arbcvb='IC'
011500960322     C                   CLEAR                   WELAB
011600960401     C**
011700960401     C* SE QUI SONO ENTRATA MEMORIZZO IL NUMERO DI SPEDIZIONE
011800960401     C     AWKSPE        IFNE      WSPE
011900960401     C* SE ESISTONO PIU' VARIAZIONI PER LA STESSA SPEDIZIONE CONTO
012000960401     C*   SOLO LA PRIMA PERCHE' LE ALTRE SONO VARIAZIONI FATTE A CAUSA
012100960401     C*   DI UN INSERIMENTO ERRATO DA PARTE DELLA FILIALE DI ARRIVO
012200960401     C                   SETON                                        09
012300960401     C                   MOVEL     AWKSPE        WSPE
012400960401     C                   ELSE
012500960401     C                   SETOFF                                       09
012600960401     C                   ENDIF
012700960322     C*
012800960322     C* RICHIESTA LA STAMPA
012900960322    3C     D08RIS        IFEQ      'NO'
013000960322     C     ARBFST        IFEQ      ' '
013100960322     C                   MOVEL     'S'           WELAB             1
013200960322     C                   ENDIF
013300960322     C*
013400960322   X3C                   ELSE
013500960322     C* RICHIESTA LA RISTAMPA
013600960322     C     ARBFST        IFEQ      'S'
013700960322     C                   MOVEL     'S'           WELAB
013800960322     C                   ENDIF
013900960322    3C                   ENDIF
014000960322     C*
014100960322     C* WELAB = 'S' ---> RECORD DA ELABORARE
014200960322    3C     WELAB         IFEQ      'S'
014300960322     C                   EXSR      WRTAWK
014400960322    3C                   ENDIF
014500960322     C*
014600110708   2aC                   ENDIF
014700110708    2C                   ENDIF
014800110708     c
014900020703     C**!!!              READ      FNARBD00                               30
015000020703     c     karbd1        reade     fnarbd00                               30
015100960322    1C                   ENDDO
015200020703
015300020709      * devo leggere l'anno successivo
015400020703     c     kaas          setgt     fnarbd00
015500020703     c                   read      fnarbd00                               30
015600020703     c                   if        not *in30
015700020709     c                   eval      kaas = arbaas
015800020703     c                   goto      leggi
015900020703     c                   endif
016000960322     C*
016100960325     C****  S T A M P A  ****
016200960328     C     *LOVAL        SETLL     FNAWK03L
016300960328     C                   READ      FNAWK03L                               30
016400960326     C*
016500960328    1C     *IN30         DOWEQ     *OFF
016600960329     C                   SETOFF                                       11
016700960329     C*
016800960329     C  N17              MOVEL     AWKSOP        WSOP
016900960329     C  N17              SETON                                        1711
017000960322     C*
017100960329     C* A CAMBIO SIGLA OPERATORE STAMPO TOTALE BOLLE
017200960329    2C     AWKSOP        IFNE      WSOP
017300960329     C                   SETON                                        11
017400960329     C     *INOF         IFEQ      *ON
017500960401     C   01              EXCEPT    PIEDI
017600960401     C                   EXCEPT    TES
017700960329     C                   SETOFF                                       OF
017800960401     C                   SETON                                        01
017900960329     C                   ENDIF
018000960329     C                   EXCEPT    TOT
018100960329     C*
018200960329     C                   CLEAR                   TOTBOL
018300960329     C                   MOVEL     AWKSOP        WSOP
018400960322    2C                   ENDIF
018500960329     C*
018600960329     C* STAMPO DETTAGLIO
018700960329     C                   EXSR      STADET
018800960326     C*
018900960326     C* SOLO PER STAMPA: AGGIORNO IL FLAG DI STAMPA
019000960329    2C     D08RIS        IFEQ      'NO'
019100991005     C     KARBD         CHAIN     FNARBD00                           31
019200960326     C     *IN31         IFEQ      *OFF
019300960326     C                   MOVEL     'S'           ARBFST
019400991005     C                   UPDATE    FNARBD00
019500960326     C                   ENDIF
019600960329    2C                   ENDIF
019700960326     C*
019800960328     C                   READ      FNAWK03L                               30
019900960322    1C                   ENDDO
020000960322     C*
020100960329     C* STAMPO ULTIMO TOTALE BOLLE
020200960329    1C   17*INOF         IFEQ      *ON
020300960401     C   01              EXCEPT    PIEDI
020400960401     C                   EXCEPT    TES
020500960329     C                   SETOFF                                       OF
020600960401     C                   SETON                                        01
020700960329    1C                   ENDIF
020800960329     C   17              EXCEPT    TOT
020900960326     C*
021000960329     C****  SE NON C'E' NIENTE DA STAMPARE STAMPO LA TESTATA  ****
021100960329    1C     *IN22         IFEQ      *OFF
021200960401     C   01              EXCEPT    PIEDI
021300960329     C                   EXCEPT    TES
021400960401     C                   SETON                                        01
021500960329    2C                   DO        10
021600960329     C                   EXCEPT    FITT
021700960329    2C                   ENDDO
021800960329    1C                   ENDIF
021900960329     C*
022000960329     C****  STAMPA DICITURA: FINE STAMPA  ****
022100960401     C                   EXCEPT    FINEST
022200960401     C                   EXCEPT    PIEDI
022300960329     C*
022400960326     C     FINE          TAG
022500000000     C                   SETON                                        LR
022600020703     C*
022700020709      *---------------------------------------------------------------*
022800020709      *   Reperisco la libreria del file di work
022900020709      *---------------------------------------------------------------*
023000020709     c     RtnLib        BegSr
023100020709
023200020709     c                   Call      'TRUL29C'
023300020709     c                   Parm                    Tr29Obj
023400020709     c                   Parm                    Tr29Typ
023500020709     c                   Parm                    Tr29Lib
023600020709      * Se la libreria ritorna a blank vuol dire che non ha trovato il file
023700020709      * in questo caso chiudo il pgm e invio un msg alla coda dell'opratore
023800020709    1c                   If        Tr29Lib = *blanks
023900020709     c                   clear                   Er
024000020709     c                   Movea     Msg(4)        Er(1)
024100020709     c                   Movea     Msg(3)        Er(135)
024200020709     c                   Movea     Er            Comm
024300020709     c                   z-add     201           Lung
024400020709     c                   Call      'QCMDEXC'
024500020709     c                   Parm                    Comm
024600020709     c                   Parm                    Lung
024700020709     c                   Eval      *In99 = *On
024800020709    1c                   EndIf
024900020709
025000020709     c                   EndSr
025100020709      *---------------------------------------------------------------*
025200020709      *   Creo il file di work in qtemp
025300020709      *---------------------------------------------------------------*
025400020709     c     crtwrk        begsr
025500020709     c                   z-add     69            lung
025600020709     c                   clear                   comman           80
025700020709     c                   movea     cmd2(1)       comman
025800020709     c                   eval      %subst(comman:31:10) = Tr29Lib
025900020709     c                   call      'QCMDEXC'                            99
026000020709     c                   parm                    comman
026100020709     c                   parm                    lung
026200020709      * Duplica oggetti non andata a buon fine
026300020709      * in questo caso chiudo il pgm e invio un msg alla coda dell'opratore
026400020709    1c                   If        *In99
026500020709     c                   clear                   Er
026600020709     c                   Movea     Msg(5)        Er(1)
026700020709     c                   Movea     Msg(3)        Er(135)
026800020709     c                   Movea     Er            Comm
026900020709     c                   z-add     201           Lung
027000020709     c                   Call      'QCMDEXC'
027100020709     c                   Parm                    Comm
027200020709     c                   Parm                    Lung
027300020709    1c                   EndIf
027400020709
027500020709     c                   endsr
027600020709     C*
027700960326     C*--- CLEAR FILE DI WORK ----------------------------------------*
027800960326     C     CLRAWK        BEGSR
027900960326     C                   Z-ADD     21            LUNG
028000960326     C                   MOVEL     *BLANKS       COMMAN           80
028100960326     C                   MOVEA     CMD1(1)       COMMAN
028200960326     C                   CALL      'QCMDEXC'                            99
028300960326     C                   PARM                    COMMAN           80
028400960326     C                   PARM                    LUNG             15 5
028500960326     C*
028600960326     C* 99 ON  - CLEAR NON ANDATA A BUON FINE: INVIO MESSAGGIO ALLA
028700960326     C*   CODA DELL'OPERATORE ED ESCO DAL PGM
028800960326    1C     *IN99         IFEQ      *ON
028900960326     C                   MOVEA     MSG(1)        ER(1)
029000960326     C                   MOVEA     MSG(2)        ER(68)
029100960326     C                   MOVEA     MSG(3)        ER(135)
029200960326     C                   MOVEA     ER            COMM            201
029300960326     C                   Z-ADD     201           LUNG
029400960326     C                   CALL      'QCMDEXC'
029500960326     C                   PARM                    COMM
029600960326     C                   PARM                    LUNG
029700960326     C*
029800960326   X1C                   ELSE
029900960326     C                   OPEN      FNAWK01L
030000960329     C                   OPEN      FNAWK03L
030100960326    1C                   ENDIF
030200960326     C                   ENDSR
030300960326     C*
030400960322     C*--- SCRIVO FILE DI WORK ---------------------------------------*
030500960322     C     WRTAWK        BEGSR
030600960322     C*
030700960322     C* AGGANCIO LA BOLLA
030800170522     C***  KBLP          CHAIN     FNBLP01L                           31
030900170522     C     KBLP          CHAIN     titas30c
031000170522     c                   if        %found(titas30c)
031100170522     c
031200960322     C*
031300960327     C* SCRIVO IL FILE DI WORK SOLO SE ARBPKB E' DIVERSO DA BLPPKF
031400170522    1C     ARBPKB        IFNE      tasPKF
031500960322     C                   SETON                                        15
031600960322     C*
031700960322     C* IMPOSTO IL CODICE CLIENTE
031800170522    2C**   BLPCCM        IFGT      0
031900170522     C**                 MOVEL     BLPCCM        WKSC
032000170522   X2C**                 ELSE
032100170522     C**                 MOVEL     BLPKSC        WKSC
032200170522    2C**                 ENDIF
032300170522     c
032400170522     c                   movel     tasccm        wksc
032500960322     C*
032600960322     C* SIA PER STAMPA CHE RISTAMPA: SE IMMESSO CONTROLLO IL CLIENTE
032700960322    2C     D08KSC        IFGT      0
032800960322     C     D08KSC        ANDNE     WKSC
032900960322     C                   SETOFF                                       15
033000960322    2C                   ENDIF
033100960322     C*
033200960322    2C   15D08RIS        IFEQ      'SI'
033300960322     C* SOLO PER RISTAMPA: SE IMMESSA CONTROLLO LA DATA VARIAZIONE
033400960322    3C     D08DVD        IFGT      0
033500960322     C     ARBDTV        IFLT      D08DVD
033600960322     C     ARBDTV        ORGT      D08DVA
033700960322     C                   SETOFF                                       15
033800960322     C                   ENDIF
033900960322    3C                   ENDIF
034000960322     C* SOLO PER RISTAMPA: SE IMMESSA CONTROLLO LA LINEA DI ARRIVO
034100960322    3C   15D08LNA        IFGT      0
034200170522     C     D08LNA        ANDNE     tasLNA
034300960322     C                   SETOFF                                       15
034400960322    3C                   ENDIF
034500960322    2C                   ENDIF
034600960322     C*
034700960322     C* 15 ON  - RECORD DA SCRIVERE NEL FILE DI WORK
034800960322    2C     *IN15         IFEQ      *ON
034900960327     C                   MOVEL     ARBAAS        AWKAAS
035000960327     C                   MOVEL     ARBLNP        AWKLNP
035100960327     C                   MOVEL     ARBNRS        AWKNRS
035200960327     C                   MOVEL     ARBNSP        AWKNSP
035300170522     C                   MOVEL     tasLNA        AWKLNA
035400170522     C                   MOVEL     tasMGS        AWKMGS
035500960326     C                   MOVEL     WKSC          AWKKSC
035600170522     c* Reperisco mittente
035700170522     c                   exsr      deccli
035800170522     C                   MOVEL     acorag        AWKRSM
035900170522     C                   MOVEL     indvia        AWKINM
036000170522     C                   MOVEL     indcae        AWKCAM
036100170522     C                   MOVEL     indcit        AWKLOM
036200170522     C                   MOVEL     indprv        AWKPRM
036300170522     C                   MOVEL     indsta        AWKNZM
036400170522     C                   MOVEL     tasRMN        AWKRMN
036500170522     C                   MOVEL     tasRSD        AWKRSD
036600170522     C                   MOVEL     tasNCL        AWKNCL
036700170522     C                   MOVEL     tasPKF        AWKPKF
036800170522     C                   MOVEL     tasPKB        AWKPKV
036900960322     C                   MOVEL     ARBDTV        AWKDTV
037000960322     C                   MOVEL     ARBORV        AWKORV
037100170522     C                   MOVEL     tasSOP        AWKSOP
037200960401     C   09              MOVEL     ' '           AWKESV
037300960401     C  N09              MOVEL     'S'           AWKESV
037400960322     C                   WRITE     FNAWK000
037500960322    2C                   ENDIF
037600960322     C*
037700960322   X1C                   ELSE
037800960327     C* ARBPKB = BLPPKF ---> FLAGGO IL RECORD PER NON LEGGERLO PIU'
037900960322     C*   NEMMENO SE RICHIESTA LA RISTAMPA
038000960322     C                   MOVEL     'N'           ARBFST
038100991005     C                   UPDATE    FNARBD00
038200170522     C                   ENDIF
038300170522     c
038400170522     c* se non trovata bolla --> flaggo come da non stampare
038500170522     c                   else
038600170522     C                   MOVEL     'N'           ARBFST
038700170522     C                   UPDATE    FNARBD00
038800170522     c
038900170522    1C                   ENDIF
039000960322     C*
039100960322     C                   ENDSR
039200960322     C*
039300960326     C*--- STAMPO DETTAGLIO ------------------------------------------*
039400960326     C     STADET        BEGSR
039500960329     C                   SETON                                        22
039600960329     C                   SETOFF                                       09
039700960326     C*
039800960326     C****  STAMPO TESTATA  ****
039900960329     C     *INOF         IFEQ      *ON
040000960401     C   01              EXCEPT    PIEDI
040100960401     C                   SETON                                        1101
040200960327     C                   EXCEPT    TES
040300960329     C                   SETOFF                                       OF
040400960326     C                   ENDIF
040500960326     C*
040600960326     C****  STAMPO DETTAGLIO  ****
040700960326     C* 12 ON  - NUMERO SERIE <> 0  ---> STAMPO TRATTINO SEPARATORE
040800960326     C     AWKNRS        COMP      0                                  1212
040900960326     C*
041000960326     C* GIRO DATA SPEDIZIONE
041100960326     C                   Z-ADD     MES           GGMM
041200960326     C                   MOVEL     GIO           GGMM              4 0
041300960326     C                   Z-ADD     AWKAAS        STADSP
041400960328     C                   MOVEL     GGMM          STADSP            8 0
041500960326     C*
041600960401     C* RAGIONE SOCIALE MITTENTE E DESTINATARIO
041700960328     C                   MOVEL     AWKRSM        STARSM           20
041800960328     C                   MOVEL     AWKRSD        STARSD           20
041900960326     C*
042000960401     C     AWKESV        IFEQ      ' '
042100960329     C                   ADD       1             TOTBOL            9 0
042200960329     C                   ELSE
042300960329     C                   SETON                                        09
042400960329     C                   ENDIF
042500960329     C*
042600960326     C                   EXCEPT    DETT
042700960326     C                   ENDSR
042800960326     C*
042900960321     C*--- SR INIZIALE -----------------------------------------------*
043000960321     C     *INZSR        BEGSR
043100960321     C****
043200960321     C** KLIST
043300960321     C****
043400960322     C* ACCESSO FNBLP01L
043500960322     C     KBLP          KLIST
043600960322     C                   KFLD                    ARBAAS
043700960322     C                   KFLD                    ARBLNP
043800960322     C                   KFLD                    ARBNRS
043900960322     C                   KFLD                    ARBNSP
044000131014     C* ACCESSO FIARBF2C
044100960326     C     KARBD         KLIST
044200960326     C                   KFLD                    AWKAAS
044300960326     C                   KFLD                    AWKLNP
044400960326     C                   KFLD                    AWKNRS
044500960326     C                   KFLD                    AWKNSP
044600960326     C                   KFLD                    AWKDTV
044700960326     C                   KFLD                    AWKORV
044800020703
044900020703     c     karbd1        klist
045000020703     c                   kfld                    kaas
045100020703     c                   kfld                    d08lnp
045200020703
045300960326     C****
045400960321     C** DEFINIZIONE CAMPI
045500960321     C****
045600960401     C     *LIKE         DEFINE    ARBPKB        WPKB
045700960401     C     *LIKE         DEFINE    AWKKSC        WKSC
045800960329     C     *LIKE         DEFINE    AWKSOP        WSOP
045900960329     C     *LIKE         DEFINE    D08DVD        STADVD
046000960329     C     *LIKE         DEFINE    D08DVA        STADVA
046100960329     C     *LIKE         DEFINE    AWKSPE        WSPE
046200960321     C*
046300960325     C****  PRENDO DATA E ORA CORRENTE  ****
046400960325     C                   TIME                    W0140            14 0
046500960325     C                   MOVE      W0140         UDATE8            8 0
046600960329     C                   MOVEL     W0140         UTIME             6 0
046700960329     C*
046800960329     C* ACCENDO INDICATORE DI OVERFLOW
046900960329     C                   SETON                                        OF
047000960329     C*
047100170522     C                   Z-ADD     1             CODUT             1 0
047200170522      *
047300170522     c     *dtaara       define    §azute        azuteds
047400170522     c     *dtaara       define    §datiute      ddatiute
047500170522     c                   in(E)     *dtaara
047600170522     c                   If        %error  or RSUT = *blanks
047700170522     c                   CLEAR                   tibs34ds
047800170522     c                   CALL      'TIBS34R'
047900170522     c                   PARM                    tibs34ds
048000170522     c                   in        *dtaara
048100170522     c                   EndIf
048200960327     C*
048300960326     C****  PARAMETRI PASSATI  ****
048400960326     C     *ENTRY        PLIST
048500960326     C                   PARM                    KPJBA
048600960326     C                   MOVEL     KPJBU         DSLS08
048700960329     C*
048800960329     C****  CODICE CLIENTE  ****
048900960329     C     D08KSC        IFGT      0
049000960329     C                   SETON                                        06
049100960329     C                   ENDIF
049200960329     C*
049300960329     C****  RISTAMPA  ****
049400960329    1C     D08RIS        IFEQ      'SI'
049500960329     C                   SETON                                        05
049600960329     C*
049700960325     C****  DATA VARIAZIONE  ****
049800960325     C* SE DATA VARIAZIONE AL = 0 --> METTO DATA DAL IN DATA AL
049900960329    2C     D08DVD        IFGT      0
050000960328     C                   SETON                                        07
050100960328     C     D08DVA        IFEQ      0
050200960325     C                   Z-ADD     D08DVD        D08DVA
050300960328     C                   ENDIF
050400960329     C* GIRO DATA VARIAZIONE DAL
050500960328     C                   Z-ADD     D08DVD        WDATA
050600960329     C                   Z-ADD     MM            GGMM              4 0
050700960328     C                   MOVEL     GG            GGMM
050800960329     C                   Z-ADD     AA            STADVD
050900960329     C                   MOVEL     GGMM          STADVD
051000960329     C* GIRO DATA VARIAZIONE  AL
051100960329     C                   Z-ADD     D08DVA        WDATA
051200960329     C                   Z-ADD     MM            GGMM
051300960329     C                   MOVEL     GG            GGMM
051400960329     C                   Z-ADD     AA            STADVA
051500960329     C                   MOVEL     GGMM          STADVA
051600960329    2C                   ENDIF
051700960329     C****  LINEA ARRIVO  ****
051800960329    2C     D08LNA        IFGT      0
051900960329     C                   SETON                                        08
052000960329    2C                   ENDIF
052100960329    1C                   ENDIF
052200960329     C*
052300920925     C                   ENDSR
052400170522     C*------------------------------------------------------------------------*
052500170522     C* DECCLI - DECODIFICA CLIENTE
052600170522     C*------------------------------------------------------------------------*
052700170522     C     DECCLI        BEGSR
052800170522     C*
052900170522     c* se il cliente è 8888 chain in titaa
053000170522     c                   move      wksc          w0040             4 0
053100170522     C                   CLEAR                   BS69DS
053200170522     C                   CLEAR                   ACODS
053300170522     C                   CLEAR                   indDS
053400170522     c
053500170522     c                   if        w0040=8888 or w0040= 0000
053600170522     c     kblp          chain     titaa30c
053700170522     c                   if        %found(titaa30c)
053800170522     c                   eval      acorag=taarsc
053900170522     c                   eval      indvia=taaind
054000170522     c                   eval      indcit=taaloc
054100170522     c                   eval      indcae=taacap
054200170522     c                   eval      indprv=taaprv
054300170522     c                   eval      indsta=taanaz
054400170522     c                   endif
054500170522     c
054600170522     c                   else
054700170522     C                   MOVEL     KNSIF         I69SIF
054800170522     C                   Z-ADD     Wksc          I69KAC
054900170522     C                   Z-ADD     Wksc          I69Kin
055000170522     C                   CALL      'TIBS69R'
055100170522     C                   PARM                    BS69DS
055200170522     C                   PARM                    ACODS
055300170522     C                   PARM                    INDDS
055400170522     C                   PARM                    CLPDS
055500170522     C                   PARM                    CLSDS
055600170522     c                   endif
055700170522     c
055800170522     C                   ENDSR
055900960326     C*
056000960325     O*---------------------------------------------------------------*
056100960328     O****  TESTATA  ****
056200960328     OQSYSPRT   E            TES              02
056300960328     O                       ST(1)               66
056400960328     O                       ST(2)              132
056500960328     O                       RSUT                20
056600960328     O                       UDATE8             117 '  /  /    '
056700960328     O                       PAGE          Z    132
056800960328     O          E            TES              03
056900960328     O                       KNSIF               10
057000960328     O                       KNMUS               24
057100960328     O                       UTIME              115 '  :  :  '
057200960328     O          E            TES              05
057300960328     O                     05                    10 'RISTAMPA'
057400960328     O                                           30 'LINEA PARTENZA:'
057500960329     O                       D08LNP              34
057600960328     O                     06                    51 'COD.CLIENTE:'
057700960328     O                     06D08KSC        Z     59
057800960328     O                     07                    83 'DATA VARIAZIONE DAL'
057900960328     O                     07STADVD              94 '  /  /    '
058000960328     O                     07                    97 'AL'
058100960328     O                     07STADVA             108 '  /  /    '
058200960328     O                     08                   126 'LINEA ARRIVO:'
058300960328     O                     08D08LNA             130
058400960328     O*
058500960328     O          E            TES              07
058600960328     O                       ST(5)               66
058700960328     O                       ST(8)              132
058800960328     O          E            TES            1 08
058900960328     O                       ST(6)               66
059000960328     O                       ST(9)              132
059100960328     O****  DETTAGLIO  ****
059200960328     O          E            DETT        1
059300960329     O                     11AWKSOP              03
059400960328     O                       AWKLNP              12
059500960328     O                     12AWKNRS        Z     15
059600960328     O                     12                    16 '-'
059700960328     O                       AWKNSP        Z     23
059800960328     O                       STADSP              34 '  /  /    '
059900960328     O                       AWKLNA              38
060000960328     O                       STARSM              60
060100960328     O                       STARSD              82
060200960328     O                       AWKRMN        Z     99
060300960328     O                       AWKNCL        Z    106
060400960328     O                       AWKPKV        4    116
060500960328     O                       AWKPKF        4    127
060600960401     O                     09                   130 '*'
060700960401     O*
060800960401     O          E            PIEDI            65
060900960401     O                       ST(13)              66
061000960401     O                       ST(14)             132
061100960401     O*
061200960328     O****  TOTALE BOLLE VARIATE PER BOLLETTISTA  ****
061300960328     O          E            TOT         2  3
061400960328     O                                           25 'TOTALE BOLLE :'
061500960328     O                       TOTBOL        2     37
061600960329     O****  FITTIZZIO PER IL SALTO GIUSTO DELLE RIGHE  ****
061700960329     O          E            FITT        1
061800960329     O                                              ' '
061900960329     O****  DICITURA FINE STAMPA  ****
062000960329     O          E            FINEST      2
062100960329     O                       ST(11)              66
062200960329     O                       ST(12)             132
062300960326     O*---------------------------------------------------------------*
062400960326** ST
062500170522XXXXXXXXXXXXXXXXXXXX             ** VARIAZIONI PESO SU SPEDIZIONI
062600960401ERRONEAMENTE BOLLETTATE **   FNLS06R     XX/XX/XXXX      PAG. XXXX   2
062700960328  RISTAMPA     LINEA PARTENZA: XXX     COD.CLIENTE: XXXXXXX     DA
062800960328TA VARIAZIONE DAL XX/XX/XXXX AL XX/XX/XXXX     LINEA ARRIVO: XXX
062900960328SIGLA    S  P  E  D  I  Z  I  O  N  E   MITTENTE              DEST   5
063000960328OPERAT.  LNP   NUMERO     DATA     LNA                               6
063100960328  X      XXX XX-XXXXXXX XX/XX/XXXX XXX  XXXXXXXXXXXXXXXXXXXX  XXXX
063200960328INATARIO              RIFERIMENTO  COLLI  PESO KG.  PESO REALE       8
063300960328                         MITTENTE                                    9
063400960401XXXXXXXXXXXXXXXX  XXXXXXXXXXXXXXX  XXXXX  XXXXXX,X   XXXXXX,X  *
063500960329                                             * * *     F I N E      11
063600960329  S T A M P A    * * *                                              12
063700961016 * = VARIAZIONI EFFETTUATE PIU' VOLTE IN ARRIVO SULLA STESSA SPEDI  13
063800961016ZIONE                                                               14
063900960326** CMD1
064000960326CLRPFM FILE(FNAWK00F)                                                 1
064100020703** CMD2
064200020709CRTDUPOBJ OBJ(FNAWK*) FROMLIB(XXXXXXXXXX) OBJTYPE(*FILE) TOLIB(QTEMP)
064300960326** MSG
064400960326SNDMSG MSG('ATTENZIONE!! FILE FNAWK00F GIA'' UTILIZZATO DA UN ALTRO   1
064500960328 LAVORO !!')                                                          2
064600960326                                                    TOUSR(*SYSOPR)    3
064700020709SNDMSG MSG('ATTENZIONE!! FILE FNAWK00F NON TROVATO NEL SISTEMA!!')    4
064800020709SNDMSG MSG('ATTENZIONE!! FILE FNAWK00F NON DUPLICATO IN QTEMP !!')    5
