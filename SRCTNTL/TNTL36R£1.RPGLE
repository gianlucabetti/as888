000100940912     H DECEDIT('0,') DATEDIT(*DMY.)
000200960223     H* TNTL36R  *----------------------------------------------------*
000300960223     H*              ELENCO TRAZIONI IN RITARDO           - BATCH -  *
000400951110     H*---------------------------------------------------------------*
000500960229     FTNTWK01L  UF A E           K DISK    USROPN
000600960228     FTNTSR03L  IF   E           K DISK
000700960228     FTNTSR08L  IF   E           K DISK
000800960229     F                                     RENAME(TNTSR000:TNTSR008)
000900960223     FTNTSR09L  IF   E           K DISK
001000960229     F                                     RENAME(TNTSR000:TNTSR009)
001100960229     FTNTST01L  IF   E           K DISK
001200960311     FTNTST03L  IF   E           K DISK
001300960311     F                                     RENAME(TNTST000:TNTST003)
001400960311     FTNTST04L  IF   E           K DISK
001500960311     F                                     RENAME(TNTST000:TNTST004)
001600960307     FTNTSM01L  IF   E           K DISK
001700960223     FTNFVS01L  IF   E           K DISK
001800000504     FTNFWS01L  IF   E           K DISK
001900960311     FTNFVS02L  IF   E           K DISK    USROPN
002000960302     F                                     RENAME(TNFVS000:TNFVS002)
002100960311     FFNFV201L  IF   E           K DISK    USROPN
002200960228     FFNFV401L  IF   E           K DISK
002300960301     FTNTLZ01L  IF   E           K DISK
002400960301     FAZORG01L  IF   E           K DISK
002500951110     FTABEL00F  IF   E           K DISK
002600960229     FPRTF198   O    F  198        PRINTER OFLIND(*INOF)
002700960229     F                                     FORMLEN(066)
002800960229     F                                     FORMOFL(063)
002900951110     D*
003000951110     D* DEFINIZIONE SCHIERE
003100960228     D TTC             S              1    DIM(40)                              TIPO  CAUSALE
003200960228     D DTC             S             35    DIM(40)                              DESCR.CAUSALE
003300960228     D ITC             S              1    DIM(40)                              IMPUTAZIONE RT
003400960228     D FER             S              3  0 DIM(100)                             FERMATE TRAINO
003500960229     D SK              S              3    DIM(100)                             PROV.CAR/SCAR
003600960229     D ER              S              1    DIM(201)
003700000504     D PLP             S              3    DIM(300)
003800960307     D CMD1            S             21    DIM(1) CTDATA PERRCD(1)              CLRPFM
003900960229     D MSG             S             67    DIM(3) CTDATA PERRCD(1)              MESSAGGI ERRORE
004000960229     D TS              S             66    DIM(24) CTDATA PERRCD(1)             RIGHE STAMPA
004100960305     D*
004200960223     D* DS PER TNTL36R - DS DI PASSAGGIO PARAMETRI PER PGM CHIAMANTI
004300960223     D DSTL36        E DS                  EXTNAME(TNTL36DS)
004400951113     D*
004500960228     D                 DS
004600960228     D  AA                     1      4  0
004700960228     D  MM                     5      6  0
004800960228     D  GG                     7      8  0
004900960228     D  WDATA                  1      8  0
005000960302     D                 DS
005100960304     D  FVSLNA                 1      3  0
005200960302     D  FVSFLP                 4    243
005300960302     D  FVSFL2               244    450
005400000504     D  FWSFL3               451    690
005500000504     D  FWSFL4               691    900
005600000504     D  VUOTO2               901    903
005700000504     D  FLP                    1    903
005800000504     D                                     DIM(301)                             LINEE TRANSITO
005900960311     D*
006000960311     D* DATA/ORA TEORICA PARTENZA
006100960311     D                 DS
006200960311     D  TSRDTP                 1      8  0
006300960311     D  TSRHTP                 9     12  0
006400960311     D  WDHTP                  1     12  0
006500960311     D* DATA/ORA REALE   PARTENZA
006600960311     D                 DS
006700960311     D  TSRDRP                 1      8  0
006800960311     D  TSRHRP                 9     12  0
006900960311     D  WDHRP                  1     12  0
007000960311     D* DATA/ORA TEORICA ARRIVO
007100960311     D                 DS
007200960311     D  TSRDTA                 1      8  0
007300960311     D  TSRHTA                 9     12  0
007400960311     D  WDHTA                  1     12  0
007500960311     D* DATA/ORA REALE   ARRIVO
007600960311     D                 DS
007700960311     D  TSRDRA                 1      8  0
007800960311     D  TSRHRA                 9     12  0
007900960311     D  WDHRA                  1     12  0
008000960307     D*
008100960307     D* DS PER XDTORA  - DIFFERENZA FRA DUE DATA/ORA
008200960229     D WXDTOR          DS
008300960229     D  WXDT1                  1      8  0
008400960229     D  WXHM1                  9     12  0
008500960229     D  WXDT2                 13     20  0
008600960229     D  WXHM2                 21     24  0
008700960229     D  WXFLG                 25     25
008800960229     D  WXMIN                 26     30  0
008900960229     D*
009000960228     D DSTC          E DS
009100951110     D KPJBA         E DS
009200960229     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
009300960304     D*
009400960304     D* DEFINIZIONE COSTANTI
009500960304     D CRIT            C                   CONST('RICHIESTI SOLO I -
009600960304     D                                     RITARDI')
009700960304     D CANT            C                   CONST('RICHIESTI SOLO GLI -
009800960304     D                                     ANTICIPI')
009900961022     D CTFP            C                   CONST('P.O.PARTENZA TRAINO:')
010000960311     D CTRN            C                   CONST('      NUMERO TRAINO:')
010100961022     D CLNP            C                   CONST('P.O.PARTENZA F.VIAGG-
010200960313     D                                     IO:')
010300951110     I*
010400960305     I* RIDENOMINO ALCUNI CAMPI DELLA VISTA LOGICA TNTSR08L
010500960305     ITNTSR008
010600960311     I              TSRDKP                      TS8DKP
010700960305     I              TSRFFP                      TS8FFP
010800960305     I              TSRDTT                      TS8DTT
010900960305     I              TSRDTN                      TS8DTN
011000960305     I              TSRTRN                      TS8TRN
011100960305     I* RIDENOMINO ALCUNI CAMPI DELLA VISTA LOGICA TNTSR09L
011200960305     ITNTSR009
011300960311     I              TSRDKA                      TS9DKA
011400960305     I              TSRFFP                      TS9FFP
011500960305     I              TSRDTT                      TS9DTT
011600960305     I              TSRDTN                      TS9DTN
011700960305     I              TSRTRN                      TS9TRN
011800951110     I/SPACE 3
011900951110     C*****************************************************************
012000951110     C* RIEPILOGO INDICATORI
012100951110     C*****************************************************************
012200960311     C* 02    - RICHIESTA UNA SPECIFICA FILIALE PARTENZA TRAINO
012300960308     C* 04    - TROVATO ALMENO UN RITARDO PER LA LNP FV IMMESSA
012400960228     C* 05    - CAMBIATO TRAINO
012500960308     C* 06    - ON  --> RICHIESTA ELABORAZIONE PER DATA TRAINO
012600960308     C*         OFF --> RICHIESTA ELABORAZIONE PER DATA CONTROLLO
012700960228     C* 07    - IMMESSA DATA TRAINO AL
012800960228     C* 08    - IMMESSA DATA CONTROLLO AL
012900960228     C* 09    - STAMPO CATEGORIA TRAINO : BIS
013000960229     C* 10    - DEVO STAMPARE DELLE FILIALI DI CARICO/SCARICO IN 2 RIGA
013100960229     C* 12    - USATO PER ESEGUIRE UNA ISTRUZIONE UNA SOLA VOLTA
013200960229     C* 13    - CAUSALE RITARDO "G"
013300960229     C* 14    - PRESENTE CAUSALE RITARDO
013400960229     C* 15    - MANCA LA DATA PARTENZA/ARRIVO REALE
013500960229     C* 16    - NON STAMPO LA PRIMA VOLTA I PIEDI
013600960302     C* 17    - STAMPO DATI DEL FOGLIO VIAGGIO
013700960301     C* 18    - STAMPO CATEGORIA TRAINO : ECCEZIONALE
013800960229     C* 20    - PRESENTE FNFV4 DA STAMPARE
013900960229     C* 22 OFF- NON C'E' NULLA DA STAMPARE: STAMPO LA TESTATA
014000960302     C* 23    - SONO IN STAMPA DI FV NON ABBINATI A TRAINO
014100960312     C* 25    - STAMPATO ALMENO UN FV NON ABBINATO A TRAINO
014200960229     C* 29    - LE DATE CONTROLLO DEL TRAINO SONO TUTTE 99999999
014300960301     C* 30/33 - DI COMODO
014400960302     C* 35    - USATO PER LOKUP
014500951110     C*****************************************************************
014600951110     C     *ENTRY        PLIST
014700951110     C                   PARM                    KPJBA
014800960223     C                   MOVEL     KPJBU         DSTL36
014900951113     C*
015000951110     C*---------------------------------------------------------------*
015100960307     C****  ESEGUO CLEAR DEL FILE DI WORK  ****
015200960229     C                   EXSR      CLRTWK
015300960229     C   99              GOTO      FINE
015400960223     C*
015500960307     C****  CONTROLLO RICHIESTE FATTE A VIDEO  ****
015600960223     C                   EXSR      CTRRIC
015700960223     C*
015800960308     C****  ELABORAZIONE PER DATA TRAINO  ****
015900960311    1C     *IN06         IFEQ      *ON
016000960308     C                   EXSR      ELABDT
016100960311   X1C                   ELSE
016200960307     C****  ELABORAZIONE PER DATA CONTROLLO  ****
016300960308     C                   EXSR      ELABDK
016400960311    1C                   ENDIF
016500960223     C*
016600960307     C****  STAMPA DEI RITARDI/ANTICIPI  ****
016700960229     C     *LOVAL        SETLL     TNTWK01L
016800960229     C                   READ      TNTWK01L                               30
016900960228     C*
017000960229    1C     *IN30         DOWEQ     *OFF
017100960229    2C     TWKDRE        IFNE      0
017200960228     C                   EXSR      STAMPA
017300960229    2C                   ENDIF
017400960228     C*
017500960229     C                   READ      TNTWK01L                               30
017600960229    1C                   ENDDO
017700960228     C*
017800960307     C****  STAMPA DEI FOGLI VIAGGIO NON ABBINATI A TRAINO  ****
017900960311    1C  N06D36FRA        IFEQ      'E'
018000960302     C     D36FRA        OREQ      'R'
018100960312     C                   SETOFF                                       16
018200960312     C                   SETON                                        231705
018300960312     C*
018400960312     C     *INOF         IFEQ      *ON
018500960312     C                   SETON                                        25
018600960312     C                   ELSE
018700960302     C                   SETON                                        OF
018800960312     C                   ENDIF
018900960302     C*
019000960311     C* APERTURA FILES
019100960311     C                   OPEN      TNFVS02L
019200960311     C                   OPEN      FNFV201L
019300960311     C*
019400960302     C     *LOVAL        SETLL     TNFVS02L
019500960302     C                   READ      TNFVS02L                               30
019600960302    2C     *IN30         DOWEQ     *OFF
019700960305   2AC     FVSATB        IFEQ      ' '
019800960305     C     FVSTRS        ANDEQ     0
019900960302     C*
020000960302     C* CONTROLLO FINE LETTURA SE IMMESSA DATA AL
020100960311    3C     FVSDKF        IFGT      D36DKA
020200960302     C                   SETON                                        30
020300960302    3C                   ENDIF
020400960302     C*
020500960308     C* SE IMMESSA CONTROLLO LA LINEA PARTENZA F.VIAGGIO
020600960308     C  N30D36LNP        IFGT      0
020700960308     C     D36LNP        ANDNE     FVSLNP
020800960308     C                   SETON                                        30
020900960308     C                   ENDIF
021000960308     C*
021100960302    3C     *IN30         IFEQ      *OFF
021200960302     C     FVSDKF        ANDGT     0
021300960312     C                   SETON                                        25
021400960302     C                   EXSR      STAMP3
021500960302    3C                   ENDIF
021600960302     C*
021700960305   2AC                   ENDIF
021800960302     C                   READ      TNFVS02L                               30
021900960302    2C                   ENDDO
022000960312     C*
022100960312     C* SE NON HO STAMPATO NEMMENO UN FV, SPENGO INDICATORE DI OVERFLOW
022200960312     C*   PER STAMPARE "FINE STAMPA" NELLA GIUSTA POSIZIONE
022300960312     C  N25              SETOFF                                       OF
022400960302    1C                   ENDIF
022500960302     C*
022600960307     C****  SE NON C'E' NIENTE DA STAMPARE STAMPO LA TESTATA  ****
022700960229    1C     *IN22         IFEQ      *OFF
022800960229     C                   EXCEPT    TESTA2
022900960308     C                   SETOFF                                       OF
023000960229    2C                   DO        10
023100960301     C                   EXCEPT    FITT
023200960229    2C                   ENDDO
023300960229    1C                   ENDIF
023400960229     C*
023500960307     C****  STAMPA DICITURA: FINE STAMPA  ****
023600960311    1C     *INOF         IFEQ      *ON
023700960305     C                   EXCEPT    PIEDI
023800960305     C                   EXCEPT    FINES2
023900960311   X1C                   ELSE
024000960229     C                   EXCEPT    FINEST
024100960229     C                   EXCEPT    PIEDI
024200960311    1C                   ENDIF
024300960229     C*
024400960307     C****  CHIUSURA FILE DI WORK  ****
024500960229     C                   CLOSE     TNTWK01L
024600960307     C*
024700951110     C     FINE          TAG
024800951113     C                   SETON                                        LR
024900960117     C*
025000960229     C*--- CLEAR FILE DI WORK ----------------------------------------*
025100960229     C     CLRTWK        BEGSR
025200960229     C                   Z-ADD     21            LUNG
025300960229     C                   MOVEL     *BLANKS       COMMAN           80
025400960229     C                   MOVEA     CMD1(1)       COMMAN
025500960229     C                   CALL      'QCMDEXC'                            99
025600960229     C                   PARM                    COMMAN           80
025700960229     C                   PARM                    LUNG             15 5
025800960229     C*
025900960229     C* 99 ON  - CLEAR NON ANDATA A BUON FINE: INVIO MESSAGGIO ALLA
026000960229     C*   CODA DELL'OPERATORE ED ESCO DAL PGM
026100960229    1C     *IN99         IFEQ      *ON
026200960229     C                   MOVEA     MSG(1)        ER(1)
026300960229     C                   MOVEA     MSG(2)        ER(68)
026400960229     C                   MOVEA     MSG(3)        ER(135)
026500960229     C                   MOVEA     ER            COMM            201
026600960229     C                   Z-ADD     201           LUNG
026700960229     C                   CALL      'QCMDEXC'
026800960229     C                   PARM                    COMM
026900960229     C                   PARM                    LUNG
027000960229     C*
027100960229   X1C                   ELSE
027200960229     C                   OPEN      TNTWK01L
027300960229    1C                   ENDIF
027400960229     C                   ENDSR
027500960229     C*
027600960223     C*--- CONTROLLO RICHIESTE FATTE A VIDEO -------------------------*
027700960223     C     CTRRIC        BEGSR
027800960311     C                   CLEAR                   PRTTRA
027900960311     C                   CLEAR                   PRTFRA
028000960313     C                   CLEAR                   PRTLNP
028100960223     C*
028200960223     C****  DATA TRAINO  ****
028300960223     C* SE DATA TRAINO  AL = 0 --> METTO DATA DAL IN DATA AL
028400960308    0C     D36DTD        IFGT      0
028500960308     C                   SETON                                        06
028600960308     C*
028700960229    1C     D36DTA        IFEQ      0
028800960223     C                   Z-ADD     D36DTD        D36DTA
028900960229   X1C                   ELSE
029000960223     C* 07 ON  - IMMESSA DATA TRAINO  AL
029100960229    2C     D36DTA        IFNE      D36DTD
029200960223     C                   SETON                                        07
029300960229    2C                   ENDIF
029400960229    1C                   ENDIF
029500960308     C*
029600960308     C* GIRO DATA TRAINO DAL
029700960308     C                   Z-ADD     D36DTD        WDATA
029800960308     C                   Z-ADD     MM            GGMM
029900960308     C                   MOVEL     GG            GGMM
030000960308     C                   Z-ADD     AA            PRTDAL
030100960308     C                   MOVEL     GGMM          PRTDAL
030200960308     C* GIRO DATA TRAINO  AL
030300960308     C                   Z-ADD     D36DTA        WDATA
030400960308     C                   Z-ADD     MM            GGMM
030500960308     C                   MOVEL     GG            GGMM
030600960308     C                   Z-ADD     AA            PRTAL
030700960308     C                   MOVEL     GGMM          PRTAL
030800960311     C*
030900960313     C* CONTROLLO SE IMMESSI LA LINEA PARTENZA TRAINO O IL NUM.TRAINO
031000960311    1C     D36TFP        IFGT      0
031100960311     C                   SETON                                        02
031200960311     C                   MOVEL     CTFP          PRTTRA           20
031300960311   X1C                   ELSE
031400960311     C     D36TRN        IFGT      0
031500960311     C                   MOVEL     CTRN          PRTTRA
031600960311     C                   ENDIF
031700960311    1C                   ENDIF
031800960308    0C                   ENDIF
031900960223     C*
032000960223     C****  DATA CONTROLLO  ****
032100960223     C* SE DATA CONTROLLO  AL = 0 --> METTO DATA DAL IN DATA AL
032200960308    0C     D36DKD        IFGT      0
032300960308     C                   SETOFF                                       06
032400960308     C*
032500960229    1C     D36DKA        IFEQ      0
032600960223     C                   Z-ADD     D36DKD        D36DKA
032700960229   X1C                   ELSE
032800960223     C* 08 ON  - IMMESSA DATA CONTROLLO  AL
032900960229    2C     D36DKA        IFNE      D36DKD
033000960223     C                   SETON                                        08
033100960229    2C                   ENDIF
033200960229    1C                   ENDIF
033300960223     C*
033400960302     C* GIRO DATA CONTROLLO DAL
033500960228     C                   Z-ADD     D36DKD        WDATA
033600960228     C                   Z-ADD     MM            GGMM
033700960228     C                   MOVEL     GG            GGMM
033800960228     C                   Z-ADD     AA            PRTDAL
033900960228     C                   MOVEL     GGMM          PRTDAL
034000960302     C* GIRO DATA CONTROLLO  AL
034100960228     C                   Z-ADD     D36DKA        WDATA
034200960228     C                   Z-ADD     MM            GGMM
034300960228     C                   MOVEL     GG            GGMM
034400960228     C                   Z-ADD     AA            PRTAL
034500960228     C                   MOVEL     GGMM          PRTAL
034600960313     C*
034700960313     C* CONTROLLO SE IMMESSA LA LINEA PARTENZA FOGLIO VIAGGIO
034800960313    1C     D36LNP        IFGT      0
034900960313     C                   MOVEL     CLNP          PRTLNP           23
035000960313    1C                   ENDIF
035100960308    0C                   ENDIF
035200960302     C*
035300960304     C****  RICHIESTA DEI RITARDI/ANTICIPI/ENTRAMBI  ****
035400960304     C     D36FRA        IFEQ      'R'
035500960304     C                   MOVEL     CRIT          PRTFRA           27
035600960304     C                   ENDIF
035700960304     C     D36FRA        IFEQ      'A'
035800960304     C                   MOVEL     CANT          PRTFRA
035900960304     C                   ENDIF
036000960304     C*
036100960223     C                   ENDSR
036200960223     C*
036300960308     C*--- ELABORAZIONE PER DATA TRAINO ------------------------------*
036400960308     C     ELABDT        BEGSR
036500960308     C*
036600960311     C* IMMESSI DATA TRAINO E LINEA PARTENZA TRAINO
036700960311    1C     D36TFP        IFGT      0
036800960311     C     KTST2         SETLL     TNTST04L
036900960311   X1C                   ELSE
037000960311     C* IMMESSI DATA TRAINO E NUMERO TRAINO
037100960311    2C     D36TRN        IFGT      0
037200960311     C     KTST3         SETLL     TNTST01L
037300960311   X2C                   ELSE
037400960311     C* IMMESSA SOLO LA DATA TRAINO
037500960311     C     D36DTD        SETLL     TNTST03L
037600960311    2C                   ENDIF
037700960311    1C                   ENDIF
037800960311     C*
037900960311     C* LETTURA RECORD
038000960311     C                   EXSR      REDTST
038100960311     C*
038200960311    1C     *IN30         DOWEQ     *OFF
038300960311     C*
038400960311     C* CONTROLLO FINE LETTURA SE IMMESSA DATA AL
038500960311    2C   07TSTDTN        IFGT      D36DTA
038600960311     C                   SETON                                        30
038700960311    2C                   ENDIF
038800960311     C*
038900960311    2C     *IN30         IFEQ      *OFF
039000960311     C* ESCLUDO RECORD ANNULLATI
039100960311    3C     TSTATB        IFEQ      ' '
039200960311     C                   MOVEL     'N'           WELAB
039300960311     C                   CLEAR                   WCONT
039400960311     C                   CLEAR                   WFFP
039500960311     C                   CLEAR                   WFFC
039600960311     C* MEMORIZZO LA FILIALE PARTENZA DEL TRAINO
039700960311     C                   MOVEL     TSTTFP        WTFP
039800960311     C*
039900960311     C* LEGGO TUTTE LE FERMATE DEL TRAINO PER VEDERE SE ESISTE ALMENO
040000960311     C*   UN RITARDO/ANTICIPO OPPURE UN MANCA DATA REALE
040100960311     C     KTSR4         SETLL     TNTSR03L
040200960311     C     KTSR4         READE     TNTSR03L                               31
040300960311     C*
040400960311    4C     *IN31         DOWEQ     *OFF
040500960311     C* VERIFICO SE IL TRAINO E' DA STAMPARE
040600960311    5C     TSRDRP        IFEQ      0
040700960311     C     TSRDRA        OREQ      0
040800960311     C     WDHRP         ORNE      WDHTP
040900960311     C     WDHRA         ORNE      WDHTA
041000970411     C* Testo anche la data controllo perchè se diversa da 99999999
041100970411     C* significa che potrei avere una causale ritardo nonostante
041200970411     C* data/ora teorica = data/ora reale
041300970411     C     TSRDKP        ORNE      99999999
041400970411     C     TSRDKA        ORNE      99999999
041500960311     C                   CLEAR                   WELAB
041600960311    5C                   ENDIF
041700960311     C*
041800960311     C* VERIFICO SE IL TRAINO HA UNA SOLA FERMATA DI CARICO
041900960311    5C     WFFP          IFNE      TSRFFP
042000960311     C                   ADD       1             WCONT
042100960311     C                   MOVEL     TSRFFP        WFFP
042200960311    5C                   ENDIF
042300960311     C*
042400960311     C* SE IL TRAINO HA PIU' FERMATE DI CARICO ED E' UN TRAINO DA
042500960311     C*   STAMPARE E' INUTILE CONTINUARE A LEGGERE ALTRI RECORD
042600960311    5C     WCONT         IFGT      1
042700960311     C     WELAB         ANDEQ     ' '
042800960311     C                   SETON                                        31
042900960311    5C                   ENDIF
043000960311     C*
043100960311     C  N31KTSR4         READE     TNTSR03L                               31
043200960311    4C                   ENDDO
043300960311     C*
043400960311     C* WELAB = ' ' ---> TRAINO DA STAMPARE: SCRIVO FILE DI WORK
043500960311    4C     WELAB         IFEQ      ' '
043600960311     C*
043700960311     C* MEMORIZZO SE IL TRAINO HA UNA SOLA FERMATA DI CARICO
043800960311    5C     WCONT         IFEQ      1
043900960311     C                   MOVEL     'S'           WFFC
044000960311    5C                   ENDIF
044100960311     C*
044200960311     C     KTSR4         SETLL     TNTSR03L
044300960311     C     KTSR4         READE     TNTSR03L                               31
044400960311    5C     *IN31         DOWEQ     *OFF
044500960311     C* CREO FILE DI WORK
044600960311     C                   EXSR      CR3TWK
044700960311     C*
044800960311     C     KTSR4         READE     TNTSR03L                               31
044900960311    5C                   ENDDO
045000960311     C*
045100960311    4C                   ENDIF
045200960311    3C                   ENDIF
045300960311     C*
045400960311     C* LETTURA RECORD SUCESSIVO
045500960311     C                   EXSR      REDTST
045600960311    2C                   ENDIF
045700960311    1C                   ENDDO
045800960311     C*
045900960308     C                   ENDSR
046000960308     C*
046100960311     C*--- LETTURA RECORD DI TNTST -----------------------------------*
046200960311     C     REDTST        BEGSR
046300960311     C*
046400960311     C* IMMESSI DATA TRAINO E LINEA PARTENZA TRAINO
046500960311    1C     D36TFP        IFGT      0
046600960311     C  N07KTST2         READE     TNTST04L                               30
046700960311     C   07D36TFP        READE     TNTST04L                               30
046800960311     C*
046900960311   X1C                   ELSE
047000960311     C* IMMESSI DATA TRAINO E NUMERO TRAINO
047100960311    2C     D36TRN        IFGT      0
047200960311     C  N07KTST3         READE     TNTST01L                               30
047300960311     C   07D36TRN        READE     TNTST01L                               30
047400960311     C*
047500960311   X2C                   ELSE
047600960311     C* IMMESSA SOLO LA DATA TRAINO
047700960311     C  N07D36DTD        READE     TNTST03L                               30
047800960311     C   07              READ      TNTST03L                               30
047900960311    2C                   ENDIF
048000960311    1C                   ENDIF
048100960311     C*
048200960311     C                   ENDSR
048300960311     C*
048400960311     C*--- CREO FILE DI WORK -----------------------------------------*
048500960311     C     CR3TWK        BEGSR
048600960311     C*
048700960311     C****  FILIALE FERMATA PARTENZA  ****
048800960311     C                   MOVEL     TSRNFP        WNFM
048900960311     C                   MOVEL     'P'           WEPA
049000960311     C     KTWK          CHAIN     TNTWK01L                           32
049100960311     C*
049200960311     C   32              CLEAR                   TNTWK000
049300960311     C*
049400960311     C* SE DATA/ORA REALE PARTENZA E' UGUALE ALLA DATA/ORA TEORICA
049500960311     C*   PARTENZA  --->  NON C'E' RITARDO
049600970411     C* PER NON ESSERCI RITARDO E' ANCHE NECESSARIO CHE LA DATA CONTROL
049700970411     C* SIA = 99999999
049800960311    1C     WDHTP         IFEQ      WDHRP
049900970411     C     TSRDKP        ANDEQ     99999999
050000060407     c                   if        *in32
050100960311     C                   CLEAR                   TWKDTE
050200960311     C                   CLEAR                   TWKHTE
050300960311     C                   CLEAR                   TWKDRE
050400960311     C                   CLEAR                   TWKHRE
050500060411     C                   MOVEL     TSRRTP        TWKRTC
050600060411     C                   MOVEL     TSRLNP        TWKLNP
050700060411     C                   MOVEL     TSRNFV        TWKNFV
050800060411     C                   MOVEL     TSRDFV        TWKDFV
050900060407     c                   endif
051000960311     C*
051100960311   X1C                   ELSE
051200960311     C*
051300960311    2C* MANCA DATA PARTENZA REALE
051400960311     C     TSRDRP        IFEQ      0
051500960311     C                   MOVEL     *ALL'9'       TWKDRE
051600960311     C                   MOVEL     *ALL'9'       TWKHRE
051700960311     C                   MOVEL     TSRDTP        TWKDTE
051800960311     C                   MOVEL     TSRHTP        TWKHTE
051900960311   X2C                   ELSE
052000960311     C* PRESENTE RITARDO
052100960311     C                   MOVEL     TSRDRP        TWKDRE
052200960311     C                   MOVEL     TSRHRP        TWKHRE
052300960311     C                   MOVEL     TSRDTP        TWKDTE
052400960311     C                   MOVEL     TSRHTP        TWKHTE
052500960311    2C                   ENDIF
052600060411     C                   MOVEL     TSRRTP        TWKRTC
052700060411     C                   MOVEL     TSRLNP        TWKLNP
052800060411     C                   MOVEL     TSRNFV        TWKNFV
052900060411     C                   MOVEL     TSRDFV        TWKDFV
053000960311    1C                   ENDIF
053100960311     C*
053200960311     C*
053300960311     C     WFFC          IFEQ      'S'
053400960311     C                   MOVEL     'S'           TWKFFC
053500960311     C                   ELSE
053600960311     C                   CLEAR                   TWKFFC
053700960311     C                   ENDIF
053800960311     C*
053900960311    1C     *IN32         IFEQ      *ON
054000960311     C                   MOVEL     TSRTRN        TWKTRN
054100960311     C                   MOVEL     TSRDTN        TWKDTN
054200960311     C                   MOVEL     WTFP          TWKTFP
054300960311     C                   MOVEL     'P'           TWKEPA
054400960311     C                   MOVEL     TSRNFP        TWKNFM
054500960311     C                   MOVEL     TSRFFP        TWKFIL
054600960311     C                   WRITE     TNTWK000
054700960311     C*
054800960311   X1C                   ELSE
054900960311     C                   UPDATE    TNTWK000
055000960311    1C                   ENDIF
055100960311     C*
055200960311     C****  FILIALE FERMATA ARRIVO  ****
055300960311     C                   MOVEL     TSRNFA        WNFM
055400960311     C                   MOVEL     'A'           WEPA
055500960311     C     KTWK          CHAIN     TNTWK01L                           32
055600960311     C*
055700960311     C   32              CLEAR                   TNTWK000
055800960311     C*
055900960311     C* SE DATA/ORA REALE ARRIVO E' UGUALE ALLA DATA/ORA TEORICA
056000960311     C*   ARRIVO  --->  NON C'E' RITARDO
056100970411     C* PER NON ESSERCI RITARDO E' ANCHE NECESSARIO CHE LA DATA CONTROL
056200970411     C* SIA = 99999999
056300960311    1C     WDHTA         IFEQ      WDHRA
056400970411     C     TSRDKA        ANDEQ     99999999
056500060407     c                   if        *in32
056600960311     C                   CLEAR                   TWKDTE
056700960311     C                   CLEAR                   TWKHTE
056800960311     C                   CLEAR                   TWKDRE
056900960311     C                   CLEAR                   TWKHRE
057000060411     C                   MOVEL     TSRRTA        TWKRTC
057100060411     C                   MOVEL     TSRLNP        TWKLNP
057200060411     C                   MOVEL     TSRNFV        TWKNFV
057300060411     C                   MOVEL     TSRDFV        TWKDFV
057400060407     c                   endif
057500960311     C*
057600960311   X1C                   ELSE
057700960311     C*
057800960311     C* MANCA DATA ARRIVO REALE
057900960311    2C     TSRDRA        IFEQ      0
058000960311     C                   MOVEL     *ALL'9'       TWKDRE
058100960311     C                   MOVEL     *ALL'9'       TWKHRE
058200960311     C                   MOVEL     TSRDTA        TWKDTE
058300960311     C                   MOVEL     TSRHTA        TWKHTE
058400960311   X2C                   ELSE
058500960311     C* PRESENTE RITARDO
058600960311     C                   MOVEL     TSRDRA        TWKDRE
058700960311     C                   MOVEL     TSRHRA        TWKHRE
058800960311     C                   MOVEL     TSRDTA        TWKDTE
058900960311     C                   MOVEL     TSRHTA        TWKHTE
059000960311    2C                   ENDIF
059100060411     c
059200060411     C                   MOVEL     TSRRTA        TWKRTC
059300060411     C                   MOVEL     TSRLNP        TWKLNP
059400060411     C                   MOVEL     TSRNFV        TWKNFV
059500060411     C                   MOVEL     TSRDFV        TWKDFV
059600960311    1C                   ENDIF
059700960311     C*
059800960311     C*
059900960311     C     WFFC          IFEQ      'S'
060000960311     C                   MOVEL     'S'           TWKFFC
060100960311     C                   ELSE
060200960311     C                   CLEAR                   TWKFFC
060300960311     C                   ENDIF
060400960311     C*
060500960311    1C     *IN32         IFEQ      *ON
060600960311     C                   MOVEL     TSRTRN        TWKTRN
060700960311     C                   MOVEL     TSRDTN        TWKDTN
060800960311     C                   MOVEL     WTFP          TWKTFP
060900960311     C                   MOVEL     'A'           TWKEPA
061000960311     C                   MOVEL     TSRNFA        TWKNFM
061100960311     C                   MOVEL     TSRFFA        TWKFIL
061200960311     C                   WRITE     TNTWK000
061300960311     C*
061400960311   X1C                   ELSE
061500960311     C                   UPDATE    TNTWK000
061600960311    1C                   ENDIF
061700960311     C*
061800960311     C                   ENDSR
061900960311     C*
062000960228     C*--- ELABORAZIONE PER DATA CONTROLLO ---------------------------*
062100960228     C     ELABDK        BEGSR
062200960223     C*
062300960226     C*****
062400960307     C* DATA CONTROLLO BUCO/RITARDO    A  R  R  I  V  O
062500960226     C*****
062600960307     C                   MOVEL     'A'           WFLG              1
062700960307     C*
062800960227     C     D36DKD        SETLL     TNTSR09L
062900960311     C  N08D36DKD        READE     TNTSR09L                               30
063000960311     C   08              READ      TNTSR09L                               30
063100960223     C*
063200960223    1C     *IN30         DOWEQ     *OFF
063300960307     C* SALVO CAMPI KIAVE
063400960311     C                   MOVEL     TS9DKA        KEYDK
063500960307     C                   MOVEL     TS9FFP        KEYFFP
063600960307     C                   MOVEL     TS9DTT        KEYDTT
063700960307     C                   MOVEL     TS9DTN        KEYDTN
063800960307     C                   MOVEL     TS9TRN        KEYTRN
063900960307     C*
064000960223     C* CONTROLLO FINE LETTURA SE IMMESSA DATA AL
064100960311    2C   08TS9DKA        IFGT      D36DKA
064200960223     C                   SETON                                        30
064300960223    2C                   ENDIF
064400960223     C*
064500960223    2C     *IN30         IFEQ      *OFF
064600960307     C* LEGGO TUTTE LE FERMATE DEL TRAINO
064700960307     C                   EXSR      REDTSR
064800960307     C*
064900960307     C* SE TUTTO OK ---> SCRIVO FILE DI WORK
065000960229    3C     WELAB         IFEQ      ' '
065100960229     C     *IN29         ANDEQ     *OFF
065200960307     C                   EXSR      REDFER
065300960307     C*
065400960226   X3C                   ELSE
065500960307     C*              ... ALTRIMENTI MI POSIZIONO SUL TRAINO SUCESSIVO
065600960307     C     KTSR3         SETGT     TNTSR09L
065700960226    3C                   ENDIF
065800960227     C*
065900960311     C  N08D36DKD        READE     TNTSR09L                               30
066000960311     C   08              READ      TNTSR09L                               30
066100960227    2C                   ENDIF
066200960227    1C                   ENDDO
066300960226     C*
066400960227     C*****
066500960307     C* DATA CONTROLLO BUCO/RITARDO    P  A  R  T  E  N  Z  A
066600960227     C*****
066700960307     C                   MOVEL     'P'           WFLG
066800960307     C*
066900960227     C     D36DKD        SETLL     TNTSR08L
067000960311     C  N08D36DKD        READE     TNTSR08L                               30
067100960311     C   08              READ      TNTSR08L                               30
067200960227     C*
067300960227    1C     *IN30         DOWEQ     *OFF
067400960307     C* SALVO CAMPI KIAVE
067500960311     C                   MOVEL     TS8DKP        KEYDK
067600960307     C                   MOVEL     TS8FFP        KEYFFP
067700960307     C                   MOVEL     TS8DTT        KEYDTT
067800960307     C                   MOVEL     TS8DTN        KEYDTN
067900960307     C                   MOVEL     TS8TRN        KEYTRN
068000960227     C*
068100960227     C* CONTROLLO FINE LETTURA SE IMMESSA DATA AL
068200960311    2C   08TS8DKP        IFGT      D36DKA
068300960227     C                   SETON                                        30
068400960227    2C                   ENDIF
068500960227     C*
068600960227    2C     *IN30         IFEQ      *OFF
068700960307     C* LEGGO TUTTE LE FERMATE DEL TRAINO
068800960307     C                   EXSR      REDTSR
068900960306     C*
069000960307     C* SE TUTTO OK ---> SCRIVO FILE DI WORK
069100960229    3C     WELAB         IFEQ      ' '
069200960229     C     *IN29         ANDEQ     *OFF
069300960307     C                   EXSR      REDFER
069400960229     C*
069500960227   X3C                   ELSE
069600960307     C*              ... ALTRIMENTI MI POSIZIONO SUL TRAINO SUCESSIVO
069700960307     C     KTSR3         SETGT     TNTSR08L
069800960227    3C                   ENDIF
069900960227     C*
070000960311     C  N08D36DKD        READE     TNTSR08L                               30
070100960311     C   08              READ      TNTSR08L                               30
070200960227    2C                   ENDIF
070300960227    1C                   ENDDO
070400960227     C*
070500960223     C                   ENDSR
070600960223     C*
070700960307     C*--- LETTURA FERMATE DEL TRAINO --------------------------------*
070800960307     C     REDTSR        BEGSR
070900960307     C                   SETON                                        29
071000960307     C                   SETOFF                                       04
071100960307     C                   CLEAR                   FER
071200960307     C                   CLEAR                   WELAB
071300960307     C                   CLEAR                   WCONT
071400960307     C                   CLEAR                   WFFP
071500960307     C                   CLEAR                   WFFC
071600960307     C*
071700960307     C* LEGGO TUTTE LE FERMATE DEL TRAINO
071800960307     C     KTSR          SETLL     TNTSR03L
071900960307     C     KTSR          READE     TNTSR03L                               31
072000960307     C*
072100960307    1C     *IN31         DOWEQ     *OFF
072200960307     C*
072300960307     C****  DATA CONTROLLO   A  R  R  I  V  O  ****
072400960307    2C     WFLG          IFEQ      'A'
072500960307     C*
072600960307     C* SE TROVO UNA DATA CONTROLLO UGUALE A ZERO O MAGGIORE DI QUELLA
072700960307     C*   RICHIESTA A VIDEO --> NON ELABORO E LEGGO IL TRAINO SUCESSIVO
072800960307    3C     TSRDKA        IFNE      *ALL'9'
072900960307     C                   SETOFF                                       29
073000960307     C     TSRDKA        IFGT      D36DKA
073100960307     C     TSRDKA        OREQ      0
073200960307     C                   MOVEL     'N'           WELAB             1
073300960307     C                   SETON                                        31
073400960307     C                   ENDIF
073500960307    3C                   ENDIF
073600960307     C*
073700960307     C* SE IMMESSA LNP F.V. STAMPO IL TRAINO SOLO SE TROVO UN RITARDO
073800960307     C*  SU UNA FERMATA ARRIVO PRESENTE NEI MCUBI CON LNP = LNP IMMESSA
073900960307    3C  N31D36LNP        IFGT      0
074000960307     C     TSRDKA        ANDNE     *ALL'9'
074100960307     C                   EXSR      REDTSM
074200960307     C     W001A         IFNE      ' '
074300960307     C                   SETON                                        04
074400960307     C                   ENDIF
074500960307    3C                   ENDIF
074600960307     C*
074700960307   X2C                   ELSE
074800960307     C****  DATA CONTROLLO   P  A  R  T  E  N  Z  A  ****
074900960307     C*
075000960307     C* SE TROVO UNA DATA CONTROLLO UGUALE A ZERO O MAGGIORE DI QUELLA
075100960307     C*   RICHIESTA A VIDEO --> NON ELABORO E LEGGO IL TRAINO SUCESSIVO
075200960307    3C     TSRDKP        IFNE      *ALL'9'
075300960307     C                   SETOFF                                       29
075400960307     C     TSRDKP        IFGT      D36DKA
075500960307     C     TSRDKP        OREQ      0
075600960308     C                   MOVEL     'N'           WELAB
075700960307     C                   SETON                                        31
075800960307     C                   ENDIF
075900960307    3C                   ENDIF
076000960307     C*
076100960307     C* SE IMMESSA LNP F.V. STAMPO IL TRAINO SOLO SE TROVO UN RITARDO
076200960307     C*   SU UNA FERMATA PARTENZA UGUALE A QUELLA IMMESSA
076300960307    3C  N31D36LNP        IFGT      0
076400960307    4C     TSRFFP        IFEQ      D36LNP
076500960307     C     TSRDKP        IFNE      *ALL'9'
076600960307     C                   SETON                                        04
076700960307     C                   ENDIF
076800960307     C*
076900960307   X4C                   ELSE
077000960307     C     *IN04         IFEQ      *OFF
077100960307     C     TSRFFP        ANDGT     D36LNP
077200960308     C                   MOVEL     'N'           WELAB
077300960307     C                   SETON                                        31
077400960307     C                   ENDIF
077500960307    4C                   ENDIF
077600960307    3C                   ENDIF
077700960307     C*
077800960307    2C                   ENDIF
077900960307     C*
078000960307     C*
078100960307    2C     *IN31         IFEQ      *OFF
078200960307     C*
078300960307     C* VERIFICO SE IL TRAINO HA UNA SOLA FERMATA DI CARICO
078400960307     C     WFFP          IFNE      TSRFFP
078500960307     C                   ADD       1             WCONT             3 0
078600960307     C                   MOVEL     TSRFFP        WFFP
078700960307     C                   ENDIF
078800960307     C*
078900960307     C* CARICO LE FERMATE NELLA SCHIERA FER
079000960307     C                   MOVEL     TSRNFP        FP
079100960307     C                   MOVEL     TSRNFA        FA
079200960307     C*
079300960307     C* FILIALE FERMATA PARTENZA
079400960307     C     TSRFFP        LOOKUP    FER                                    35
079500960307     C  N35              MOVEA     TSRFFP        FER(FP)
079600960307     C*
079700960307     C* FILIALE FERMATA ARRIVO
079800960307     C     TSRFFA        LOOKUP    FER                                    35
079900960307     C  N35              MOVEA     TSRFFA        FER(FA)
080000960307     C*
080100960307     C     KTSR          READE     TNTSR03L                               31
080200960307    2C                   ENDIF
080300960307    1C                   ENDDO
080400960307     C*
080500960307     C* SE NON E' STATO TROVATO NEMMENO UN RITARDO PER LA LNP F.V.
080600960307     C*   IMMESSA --> NON ELABORO E LEGGO IL TRAINO SUCESSIVO
080700960307    1C     D36LNP        IFGT      0
080800960307     C     *IN04         ANDEQ     *OFF
080900960307     C                   MOVEL     'N'           WELAB
081000960307    1C                   ENDIF
081100960307     C*
081200960307     C                   ENDSR
081300960307     C*
081400960307     C*--- LETTURA FILE METRI CUBI -----------------------------------*
081500960307     C     REDTSM        BEGSR
081600960307     C                   CLEAR                   W001A
081700960307     C*
081800960307     C     KTSR          SETLL     TNTSM01L
081900960307     C     KTSR          READE     TNTSM01L                               32
082000960307     C*
082100960307    1C     *IN32         DOWEQ     *OFF
082200960307     C* CONTROLLO SE ESISTE UNA FERMATA DI ARRIVO PRESENTE NEL FILE
082300960307     C*   DEI METRI CUBI CON LNP UGUALE A QUELLA RICHIESTA
082400960307    2C     TSMATB        IFEQ      ' '
082500960307     C     TSRFFA        ANDEQ     TSMTFA
082600960307     C     TSMTFP        ANDEQ     D36LNP
082700960307     C                   MOVEL     '1'           W001A             1
082800960307     C                   SETON                                        32
082900960307    2C                   ENDIF
083000960307     C*
083100960307     C  N32KTSR          READE     TNTSM01L                               32
083200960307    1C                   ENDDO
083300960307     C*
083400960307     C                   ENDSR
083500960307     C*
083600960307     C*--- LETTURA FERMATE CARICATE IN SKIERA ------------------------*
083700960307     C     REDFER        BEGSR
083800960307     C*
083900960307     C* MEMORIZZO SE IL TRAINO HA UNA SOLA FERMATA DI CARICO
084000960307    1C     WCONT         IFEQ      1
084100960307     C                   MOVEL     'S'           WFFC
084200960307    1C                   ENDIF
084300960307     C*
084400960307     C* AGGANCIO TNTST00F PER REPERIRE LA FILIALE PARTENZA DEL TRAINO
084500960307     C     KTSR          CHAIN     TNTST01L                           31
084600960307     C  N31              MOVEL     TSTTFP        WTFP
084700960307     C*
084800960307     C                   Z-ADD     1             X
084900960307    1C                   DO        100           X
085000960307    2C     FER(X)        IFGT      0
085100960307     C*
085200960307     C     KTSR2         SETLL     TNTSR03L
085300960307     C     KTSR2         READE     TNTSR03L                               31
085400960307     C*
085500960307    3C     *IN31         DOWEQ     *OFF
085600960307     C* CREO FILE DI WORK
085700960307    4C     WFLG          IFEQ      'A'
085800960307     C                   EXSR      CRTTWK                                       *DK ARRIVO
085900960307   X4C                   ELSE
086000960307     C                   EXSR      CR2TWK                                       *DK PARTENZA
086100960307    4C                   ENDIF
086200960307     C*
086300960307     C     KTSR2         READE     TNTSR03L                               31
086400960307    3C                   ENDDO
086500960307     C*
086600960307    2C                   ENDIF
086700960307    1C                   ENDDO
086800960307     C*
086900960307     C                   ENDSR
087000960307     C*
087100960226     C*--- CREO FILE DI WORK -----------------------------------------*
087200960226     C     CRTTWK        BEGSR
087300960226     C*
087400960228     C* FILIALE FERMATA PARTENZA
087500960228     C                   MOVEL     TSRNFP        WNFM
087600960228     C                   MOVEL     'P'           WEPA
087700960229     C     KTWK          CHAIN     TNTWK01L                           32
087800960228     C* SE GIA' ESISTENTE NON LO RISCRIVO
087900960228    1C     *IN32         IFEQ      *ON
088000960228     C                   CLEAR                   TNTWK000
088100960228     C                   MOVEL     TSRTRN        TWKTRN
088200960228     C                   MOVEL     TSRDTN        TWKDTN
088300960228     C                   MOVEL     WTFP          TWKTFP
088400960228     C                   MOVEL     'P'           TWKEPA
088500960228     C                   MOVEL     TSRNFP        TWKNFM
088600960228     C                   MOVEL     TSRFFP        TWKFIL
088700960228     C                   WRITE     TNTWK000
088800960228    1C                   ENDIF
088900960228     C*
089000960228     C* FILIALE FERMATA ARRIVO
089100960228     C                   MOVEL     TSRNFA        WNFM
089200960228     C                   MOVEL     'A'           WEPA
089300960229     C     KTWK          CHAIN     TNTWK01L                           32
089400960311     C*
089500960301     C   32              CLEAR                   TNTWK000
089600960301     C*
089700960301     C* TSRDKA = 99999999  --->  NON C'E' RITARDO
089800960301    1C     TSRDKA        IFEQ      *ALL'9'
089900060407     c
090000060407     c* Imposto i campi solo il write: in update solo se c'e' ritardo
090100060407     c                   if        *in32
090200960301     C                   CLEAR                   TWKDTE
090300960301     C                   CLEAR                   TWKHTE
090400960301     C                   CLEAR                   TWKDRE
090500960301     C                   CLEAR                   TWKHRE
090600060411     C                   MOVEL     TSRRTA        TWKRTC
090700060411     C                   MOVEL     TSRLNP        TWKLNP
090800060411     C                   MOVEL     TSRNFV        TWKNFV
090900060411     C                   MOVEL     TSRDFV        TWKDFV
091000060407     c                   endif
091100960301     C*
091200960301   X1C                   ELSE
091300960301     C*
091400960301     C* MANCA DATA ARRIVO REALE
091500960301    2C     TSRDRA        IFEQ      0
091600060407     c
091700060407     c* Imposto i campi solo il write: in update solo se c'e' ritardo
091800960301     C                   MOVEL     *ALL'9'       TWKDRE
091900960301     C                   MOVEL     *ALL'9'       TWKHRE
092000960301     C                   MOVEL     TSRDTA        TWKDTE
092100960301     C                   MOVEL     TSRHTA        TWKHTE
092200060407     c
092300960301   X2C                   ELSE
092400960301     C* PRESENTE RITARDO
092500960301     C                   MOVEL     TSRDRA        TWKDRE
092600960301     C                   MOVEL     TSRHRA        TWKHRE
092700960301     C                   MOVEL     TSRDTA        TWKDTE
092800960301     C                   MOVEL     TSRHTA        TWKHTE
092900960301    2C                   ENDIF
093000060411     C                   MOVEL     TSRRTA        TWKRTC
093100060411     C                   MOVEL     TSRLNP        TWKLNP
093200060411     C                   MOVEL     TSRNFV        TWKNFV
093300060411     C                   MOVEL     TSRDFV        TWKDFV
093400960301    1C                   ENDIF
093500960301     C*
093600960301     C*
093700960301     C     WFFC          IFEQ      'S'
093800960301     C                   MOVEL     'S'           TWKFFC
093900960301     C                   ELSE
094000960301     C                   CLEAR                   TWKFFC
094100960301     C                   ENDIF
094200960301     C*
094300960228    1C     *IN32         IFEQ      *ON
094400960228     C                   MOVEL     TSRTRN        TWKTRN
094500960228     C                   MOVEL     TSRDTN        TWKDTN
094600960228     C                   MOVEL     WTFP          TWKTFP
094700960228     C                   MOVEL     'A'           TWKEPA
094800960228     C                   MOVEL     TSRNFA        TWKNFM
094900960228     C                   MOVEL     TSRFFA        TWKFIL
095000960226     C                   WRITE     TNTWK000
095100960301     C*
095200960301   X1C                   ELSE
095300960301     C                   UPDATE    TNTWK000
095400960228    1C                   ENDIF
095500960226     C*
095600960226     C                   ENDSR
095700960228     C*
095800960228     C*--- CREO FILE DI WORK -----------------------------------------*
095900960228     C     CR2TWK        BEGSR
096000960228     C*
096100960228     C* FILIALE FERMATA ARRIVO
096200960228     C                   MOVEL     TSRNFA        WNFM
096300960228     C                   MOVEL     'A'           WEPA
096400960229     C     KTWK          CHAIN     TNTWK01L                           32
096500960228     C* SE GIA' ESISTENTE NON LO RISCRIVO
096600960228    1C     *IN32         IFEQ      *ON
096700960228     C                   CLEAR                   TNTWK000
096800960228     C                   MOVEL     TSRTRN        TWKTRN
096900960228     C                   MOVEL     TSRDTN        TWKDTN
097000960228     C                   MOVEL     WTFP          TWKTFP
097100960228     C                   MOVEL     'A'           TWKEPA
097200960228     C                   MOVEL     TSRNFA        TWKNFM
097300960228     C                   MOVEL     TSRFFA        TWKFIL
097400960228     C                   WRITE     TNTWK000
097500960228    1C                   ENDIF
097600960228     C*
097700960228     C* FILIALE FERMATA PARTENZA
097800960228     C                   MOVEL     TSRNFP        WNFM
097900960228     C                   MOVEL     'P'           WEPA
098000960229     C     KTWK          CHAIN     TNTWK01L                           32
098100960311     C*
098200960228     C   32              CLEAR                   TNTWK000
098300960228     C*
098400960307     C* TSRDKP = 99999999  --->  NON C'E' RITARDO
098500960228    1C     TSRDKP        IFEQ      *ALL'9'
098600060407     c                   if        *in32
098700960228     C                   CLEAR                   TWKDTE
098800960228     C                   CLEAR                   TWKHTE
098900960228     C                   CLEAR                   TWKDRE
099000960228     C                   CLEAR                   TWKHRE
099100060411     C*
099200060411     C                   MOVEL     TSRRTP        TWKRTC
099300060411     C                   MOVEL     TSRLNP        TWKLNP
099400060411     C                   MOVEL     TSRNFV        TWKNFV
099500060411     C                   MOVEL     TSRDFV        TWKDFV
099600060407     c                   endif
099700960228     C*
099800960228   X1C                   ELSE
099900960228     C*
100000960311    2C* MANCA DATA PARTENZA REALE
100100960228     C     TSRDRP        IFEQ      0
100200960228     C                   MOVEL     *ALL'9'       TWKDRE
100300960228     C                   MOVEL     *ALL'9'       TWKHRE
100400960228     C                   MOVEL     TSRDTP        TWKDTE
100500960228     C                   MOVEL     TSRHTP        TWKHTE
100600960228   X2C                   ELSE
100700960228     C* PRESENTE RITARDO
100800960228     C                   MOVEL     TSRDRP        TWKDRE
100900960228     C                   MOVEL     TSRHRP        TWKHRE
101000960228     C                   MOVEL     TSRDTP        TWKDTE
101100960228     C                   MOVEL     TSRHTP        TWKHTE
101200960228    2C                   ENDIF
101300060411     C*
101400060411     C                   MOVEL     TSRRTP        TWKRTC
101500060411     C                   MOVEL     TSRLNP        TWKLNP
101600060411     C                   MOVEL     TSRNFV        TWKNFV
101700060411     C                   MOVEL     TSRDFV        TWKDFV
101800960228    1C                   ENDIF
101900960229     C*
102000960229     C     WFFC          IFEQ      'S'
102100960229     C                   MOVEL     'S'           TWKFFC
102200960229     C                   ELSE
102300960229     C                   CLEAR                   TWKFFC
102400960229     C                   ENDIF
102500960228     C*
102600960228    1C     *IN32         IFEQ      *ON
102700960228     C                   MOVEL     TSRTRN        TWKTRN
102800960228     C                   MOVEL     TSRDTN        TWKDTN
102900960228     C                   MOVEL     WTFP          TWKTFP
103000960228     C                   MOVEL     'P'           TWKEPA
103100960228     C                   MOVEL     TSRNFP        TWKNFM
103200960228     C                   MOVEL     TSRFFP        TWKFIL
103300960228     C                   WRITE     TNTWK000
103400960228     C*
103500960228   X1C                   ELSE
103600960228     C                   UPDATE    TNTWK000
103700960228    1C                   ENDIF
103800960228     C*
103900960228     C                   ENDSR
104000960228     C*
104100960228     C*--- ROUTINE DI STAMPA -----------------------------------------*
104200960228     C     STAMPA        BEGSR
104300960229     C                   SETOFF                                       050917
104400960229     C                   SETOFF                                       101320
104500960301     C                   SETOFF                                       1215
104600960301     C                   SETOFF                                       1418
104700960302     C                   CLEAR                   WOK
104800960229     C*
104900960228     C* PULIZIA CAMPI NON SEMPRE PIENI
105000960229     C                   CLEAR                   PR1LNP
105100960229     C                   CLEAR                   PR1NFV
105200960229     C                   CLEAR                   PR1DFV
105300960229     C                   CLEAR                   PD1RTC
105400960229     C                   CLEAR                   PR1IMP
105500960229     C                   CLEAR                   PR1DTE
105600960229     C                   CLEAR                   PR1HTE
105700960229     C                   CLEAR                   PR1DRE
105800960229     C                   CLEAR                   PR1HRE
105900960229     C                   CLEAR                   PR1MIN
106000960301     C                   CLEAR                   PR1FRA
106100960302     C*
106200960302     C* 15 ON  - MANCA LA DATA PARTENZA/ARRIVO REALE
106300960302    1C     TWKDRE        IFEQ      *ALL'9'
106400960302     C                   SETON                                        15
106500960302     C*
106600960302   X1C                   ELSE
106700960302     C* DATA/ORA REALE
106800960302     C                   Z-ADD     TWKDRE        WDATA
106900960302     C                   Z-ADD     MM            GGMM
107000960302     C                   MOVEL     GG            GGMM
107100960302     C                   Z-ADD     AA            PR1DRE            6 0
107200960302     C                   MOVEL     GGMM          PR1DRE
107300960302     C                   Z-ADD     TWKHRE        PR1HRE
107400960302     C*
107500960302     C* SE E' PRESENTE RITARDO DICO QUANTI MINUTI SONO
107600960302     C                   CLEAR                   WXDTOR
107700960302     C                   MOVEL     TWKDRE        WXDT1
107800960302     C                   MOVEL     TWKHRE        WXHM1
107900960302     C                   MOVEL     TWKDTE        WXDT2
108000960302     C                   MOVEL     TWKHTE        WXHM2
108100960302     C                   MOVEL     'D'           WXFLG
108200960302     C                   CALL      'XDTORA'
108300960302     C                   PARM                    WXDTOR
108400960302     C* MINUTI RITARDO/ANTICIPO
108500960302     C                   Z-ADD     WXMIN         PR1MIN            4 0
108600960302    2C     WXMIN         IFGT      9999
108700960302     C                   Z-ADD     9999          PR1MIN
108800960302    2C                   ENDIF
108900960302     C* R=RITARDO / A=ANTICIPO
109000960302    2C     WXFLG         IFEQ      '+'
109100970328     C     TWKRTC        ORNE      *BLANKS
109200960302     C                   MOVEL     'R'           PR1FRA            1            *RITARDO
109300960302   X2C                   ELSE
109400960302     C     WXFLG         IFEQ      '-'
109500960302     C                   MOVEL     'A'           PR1FRA                         *ANTICIPO
109600960302     C                   ENDIF
109700960302    2C                   ENDIF
109800960302    1C                   ENDIF
109900960228     C*
110000960302     C* CONTROLLO RICHIESTA RITARDI/ANTICIPI/ENTRAMBI
110100960302    1C     D36FRA        IFEQ      'E'
110200960302     C                   MOVEL     '1'           WOK               1
110300960302   X1C                   ELSE
110400960302     C*
110500960302     C* RITARDI
110600960302    2C     D36FRA        IFEQ      'R'
110700960302    3C     *IN15         IFEQ      *ON
110800960302     C                   MOVEL     '1'           WOK
110900960302   X3C                   ELSE
111000960302    4C     PR1FRA        IFEQ      'R'
111100960302     C                   MOVEL     '1'           WOK
111200960302    4C                   ENDIF
111300960302    3C                   ENDIF
111400960302    2C                   ENDIF
111500960302     C*
111600960302     C* ANTICIPI
111700960302    2C     D36FRA        IFEQ      'A'
111800960302     C     *IN15         ANDEQ     *OFF
111900960302     C     PR1FRA        ANDEQ     'A'
112000960302     C                   MOVEL     '1'           WOK
112100960302    2C                   ENDIF
112200960302    1C                   ENDIF
112300960302     C*
112400960302     C* WOK = '1'  --->  R E C O R D    D A    S T A M P A R E
112500960302    0C     WOK           IFEQ      '1'
112600960312     C                   SETON                                        22
112700960302     C*
112800960228     C* SALVO TFP/TRN/DTN: SE CAMBIANO RICERCO IL TRAINO
112900960228    1C     TWKTFP        IFNE      SAVTFP
113000960228     C     TWKTRN        ORNE      SAVTRN
113100960228     C     TWKDTN        ORNE      SAVDTN
113200960228     C                   SETON                                        05
113300960228     C*
113400960228     C* AGGANCIO STORICO TRAINI
113500960228     C     KTST          CHAIN     TNTST01L                           31
113600960302     C*
113700960301     C* DATA TRAINO
113800960301     C                   MOVEL     TWKDTN        WDATA                          DATA TRAINO
113900960301     C                   MOVEL     GG            PR1DTN
114000960301     C                   MOVE      MM            PR1DTN            4 0
114100960228     C* NUMERO TRAINO
114200960228     C                   MOVEL     TWKTRN        PR1TRN
114300960228     C* FILIALE PARTENZA TRAINO
114400960228     C                   MOVEL     TWKTFP        PR1TFP
114500960228     C* FILIALE ARRIVO   TRAINO
114600960228     C                   MOVEL     TSTTFA        PR1TFA
114700960228     C* CATEGORIA TRAINO
114800960229    2C     TSTBIS        IFEQ      'B'
114900960228     C                   SETON                                        09
115000960229    2C                   ENDIF
115100960301    2C     TSTBIS        IFEQ      'E'
115200960301     C                   SETON                                        18
115300960301    2C                   ENDIF
115400960228     C* TIPO TRAINO
115500960228     C                   MOVEL     TSTTTR        PR1TTR
115600960301     C* TRAZIONISTA
115700960301     C                   MOVEL     TSTPDR        PR1PDR
115800000807     C     KTNTLZ        CHAIN     TNTLZ01L                           33
115900960301     C  N33              MOVEL     TLZRSC        PR1DPD           11
116000960304     C   33              CLEAR                   PR1DPD
116100960228     C* CARICO FERMATE TRAINO
116200960228     C                   EXSR      CARDES
116300960228     C*
116400960228     C                   MOVEL     TWKTFP        SAVTFP
116500960228     C                   MOVEL     TWKTRN        SAVTRN
116600960228     C                   MOVEL     TWKDTN        SAVDTN
116700960228    1C                   ENDIF
116800960228     C*
116900960229     C* CONTROLLO SE SI TRATTA DI RITARDO PARTENZA O ARRIVO
117000960229    1C     TWKEPA        IFEQ      'P'
117100960229     C                   MOVEL     'PART'        PR1FPA            4
117200960229     C                   SETON                                        17
117300960229     C                   CLEAR                   WLAI
117400960229   X1C                   ELSE
117500960229     C                   MOVEL     'ARR '        PR1FPA
117600960229     C                   MOVEL     TWKFIL        WLAI
117700960229    2C     TWKFFC        IFEQ      'S'
117800960229     C                   SETON                                        17
117900960229    2C                   ENDIF
118000960229    1C                   ENDIF
118100960229     C*
118200960301     C* DATI DEL FOGLIO VIAGGIO
118300960301     C                   MOVEL     TWKLNP        PR1LNP                         LNP FV
118400960301     C                   MOVEL     TWKNFV        PR1NFV                         NUMERO FV
118500960301     C                   MOVEL     TWKDFV        WDATA                          DATA FV
118600960301     C                   MOVEL     GG            PR1DFV
118700960301     C                   MOVE      MM            PR1DFV            4 0
118800960302     C*
118900960228     C* FILIALE SEGNALAZIONE
119000960228     C                   MOVEL     TWKFIL        PR1SEG
119100960228     C* DATA/ORA TEORICA
119200960228     C                   Z-ADD     TWKDTE        WDATA
119300960228     C                   Z-ADD     MM            GGMM              4 0
119400960228     C                   MOVEL     GG            GGMM
119500960229     C                   Z-ADD     AA            PR1DTE            6 0
119600960228     C                   MOVEL     GGMM          PR1DTE
119700960228     C                   Z-ADD     TWKHTE        PR1HTE
119800960229     C*
119900960228     C*
120000960228     C* CAUSALE RITARDO
120100960228     C                   MOVEL     TWKRTC        PR1RTC
120200960229     C*
120300960229   1AC     PR1RTC        IFNE      ' '
120400960229     C                   SETON                                        14
120500960307     C* DECODIFICA CAUSALE
120600960307     C                   EXSR      DECCAU
120700960229   1AC                   ENDIF
120800960228     C*
120900960312     C****  STAMPA TESTATA E DETTAGLIO  ****
121000960307     C                   EXSR      STARIG
121100960302    0C                   ENDIF
121200960229     C*
121300960228     C                   ENDSR
121400960228     C*
121500960228     C*--- LETTURA PER CARICO FERMATE TRAINO ------------------------*
121600960228     C     CARDES        BEGSR
121700960228     C                   Z-ADD     0             DS                3 0
121800960228     C                   CLEAR                   SK
121900960229     C*
122000960229     C* SALVO CAMPI KIAVE PER RICHAINARE IL RECORD CHE STO LEGGENDO
122100960229     C*   PERCHE' PERDO IL POSIZIONAMENTO A CAUSA DEL CARICAMENTO
122200960229     C*   DELLE FERMATE DEL TRAINO
122300960229     C                   MOVEL     TWKTFP        KSTFP
122400960229     C                   MOVEL     TWKDTN        KSDTN
122500960229     C                   MOVEL     TWKTRN        KSTRN
122600960229     C                   MOVEL     TWKNFM        KSNFM
122700960229     C                   MOVEL     TWKEPA        KSEPA
122800960228     C*
122900960312     C****  CARICO LE FERMATE DEL TRAINO  ****
123000960229     C     KTWK2         SETLL     TNTWK01L
123100960229     C     KTWK2         READE     TNTWK01L                               32
123200960228     C*
123300960229    1C     *IN32         DOWEQ     *OFF
123400960312     C                   MOVEL     TWKNFM        DS
123500960229     C*
123600960229     C     TWKFIL        CHAIN     AZORG01L                           33
123700960229     C*
123800960228     C                   MOVEL     ORGPRO        SK(DS)
123900960228     C                   MOVE      '/'           SK(DS)
124000960228     C*
124100960229     C     KTWK2         READE     TNTWK01L                               32
124200960229    1C                   ENDDO
124300960228     C*
124400960229     C                   MOVEA     SK            PR1DES           14
124500960229     C*
124600960312     C****  REPERISCO IL FV DA CUI PRENDERE IL CODUCENTE  ****
124700960302     C     KTWK3         CHAIN     TNTWK01L                           32
124800960302     C*
124900960302     C  N32KFVS          CHAIN     TNFVS01L                           32
125000960302     C  N32              MOVEL     FVSDCN        PR1DCN           10            CONDUCENTE
125100960302     C   32              CLEAR                   PR1DCN
125200960302     C*
125300960312     C****  RIPOSIZIONAMENTO SUL RECORD GIUSTO DI TNTWK00F  ****
125400960229     C     KSAV          CHAIN     TNTWK01L                           33
125500960312     C*
125600960312     C****  VERIFICO LE DESTINAZIONI  ****
125700960312     C                   Z-ADD     6             DS
125800960312    1C     SK(DS)        IFNE      ' '
125900960312     C                   SETON                                        10
126000960312    1C                   ENDIF
126100960312     C*
126200960228     C                   ENDSR
126300960228     C*
126400960307     C*--- DECODIFICA CAUSALE RITARDO -------------------------------*
126500960307     C     DECCAU        BEGSR
126600960307     C                   Z-ADD     1             X
126700960307     C*
126800960307     C     PR1RTC        LOOKUP    TTC(X)                                 35
126900960307     C   35              MOVEL     ITC(X)        PR1IMP            1
127000960307     C  N35              MOVEL     '?'           PR1IMP
127100960307     C*
127200960307    1C     PR1RTC        IFNE      'G'
127300960307     C   35              MOVEL     DTC(X)        PD1RTC
127400960307     C  N35              MOVEL     *ALL'?'       PD1RTC
127500960307     C*
127600960307   X1C                   ELSE
127700960307     C* 13 ON  - CAUSALE RITARDO = "G"
127800960307     C                   SETON                                        13
127900960307     C*
128000960307     C     KFV4          SETLL     FNFV401L
128100960307     C     KFV4          READE     FNFV401L                               32
128200960307    2C     *IN32         DOWEQ     *OFF
128300960307    3C     FV4ATB        IFEQ      ' '
128400960307     C                   MOVEL     FV4NOT        PD1RTC
128500960307     C                   SETON                                        32
128600960307    3C                   ENDIF
128700960307     C  N32KFV4          READE     FNFV401L                               32
128800960307    2C                   ENDDO
128900960307    1C                   ENDIF
129000960307     C                   ENDSR
129100960307     C*
129200960307     C*--- STAMPA TESTATA E DETTAGLIO -------------------------------*
129300960307     C     STARIG        BEGSR
129400960307     C*
129500960307     C****  STAMPO TESTATA  ****
129600960307    1C     *INOF         IFEQ      *ON
129700960307     C   16              EXCEPT    PIEDI
129800960307     C                   EXCEPT    TESTA2
129900960307     C                   SETOFF                                       OF
130000960307     C                   SETON                                        16
130100960307    1C                   ENDIF
130200960307     C*
130300960307     C****  STAMPO DETTAGLIO  ****
130400960307     C                   EXCEPT    DETT
130500960307     C*
130600960307     C****  FINISCO DI STAMPARE DESTINAZIONI E FNFV4  ****
130700960307     C   14              SETON                                        20
130800960307    1C     *IN10         DOWEQ     *ON
130900960307     C     *IN20         OREQ      *ON
131000960307     C   10              EXSR      REDDES
131100960307     C   20              EXSR      REDFV4
131200960307     C   10
131300960307     COR 20              EXSR      STAMP2
131400960307    1C                   ENDDO
131500960307     C*
131600960307     C                   EXCEPT    FITT
131700960307     C                   ENDSR
131800960307     C*
131900960307     C*--- LETTURA DELLE DESTINAZIONI ANCORA DA STAMPARE--------------*
132000960307     C     REDDES        BEGSR
132100960307    1C     SK(DS)        IFNE      '  '
132200960307     C                   CLEAR                   PR2DES
132300960307     C                   MOVEA     SK(DS)        PR2DES           14
132400960307     C                   ADD       5             DS
132500960307   X1C                   ELSE
132600960307     C                   SETOFF                                       10
132700960307    1C                   ENDIF
132800960307     C                   ENDSR
132900960307     C*
133000960307     C*--- LETTURA FILE FNFV401L ------------------------------------*
133100960307     C     REDFV4        BEGSR
133200960307     C  N13
133300960307     CANN12KFV4          SETLL     FNFV401L
133400960307     C  N12              SETON                                        12
133500960307     C*
133600960307     C     KFV4          READE     FNFV401L                               32
133700960307    1C     *IN32         DOWEQ     *OFF
133800960307     C     FV4ATB        ANDNE     ' '
133900960307     C     KFV4          READE     FNFV401L                               32
134000960307    1C                   ENDDO
134100960307     C*
134200960307     C  N32              SETON                                        20
134300960307     C  N32              MOVEL     FV4NOT        PR2NOT
134400960307     C   32              SETOFF                                       20
134500960307     C                   ENDSR
134600960307     C*
134700960307     C*--- STAMPA DESTINAZIONI E FNFV4 -------------------------------*
134800960307     C     STAMP2        BEGSR
134900960307     C*
135000960307    1C     *INOF         IFEQ      *ON
135100960307     C   16              EXCEPT    PIEDI
135200960307     C                   EXCEPT    TESTA2
135300960307     C                   EXCEPT    DETT
135400960307     C                   SETOFF                                       OF
135500960307    1C                   ENDIF
135600960307     C*
135700960307     C* 2° RIGA DI DETTAGLIO
135800960307     C                   EXCEPT    DETT2
135900960307     C                   ENDSR
136000960307     C*
136100960302     C*--- STAMPA FV NON ABBINATI A TRAINO ---------------------------*
136200960302     C     STAMP3        BEGSR
136300960302     C                   SETON                                        22
136400960302     C                   SETOFF                                       09
136500960302     C                   SETOFF                                       101320
136600960302     C                   SETOFF                                       1215
136700960302     C                   SETOFF                                       1418
136800960302     C*
136900960302     C* PULIZIA CAMPI NON SEMPRE PIENI
137000960302     C                   CLEAR                   PR1DTN
137100960302     C                   CLEAR                   PR1TRN
137200960302     C                   CLEAR                   PR1TFP
137300960302     C                   CLEAR                   PR1TFA
137400960302     C                   CLEAR                   PR1MIN
137500960302     C                   CLEAR                   PR1FRA
137600960302     C                   CLEAR                   PR1DTE
137700960302     C                   CLEAR                   PR1HTE
137800960302     C                   CLEAR                   PD1RTC
137900960302     C                   CLEAR                   PR1IMP
138000960302     C                   CLEAR                   PR1SEG
138100960302     C                   MOVEL     'PART'        PR1FPA
138200960302     C*
138300960302     C* CATEGORIA TRAINO
138400960302    1C     FVSBIS        IFEQ      'B'
138500960302     C                   SETON                                        09
138600960302    1C                   ENDIF
138700960302    1C     FVSBIS        IFEQ      'E'
138800960302     C                   SETON                                        18
138900960302    1C                   ENDIF
139000960302     C* TIPO TRAINO
139100960302     C                   MOVEL     FVSTTR        PR1TTR
139200960302     C* CARICO FILIALI DI SCARICO
139300960302     C                   EXSR      CARFIL
139400960302     C* TRAZIONISTA
139500960302     C                   MOVEL     FVSPDR        PR1PDR
139600960302     C                   MOVEL     FVSDPD        PR1DPD
139700960302     C* CONDUCENTE
139800960302     C                   MOVEL     FVSDCN        PR1DCN
139900960302     C* DATI DEL FOGLIO VIAGGIO
140000960302     C                   MOVEL     FVSLNP        PR1LNP                         LNP FV
140100960305     C                   MOVEL     FVSLNP        PR1SEG
140200960302     C                   MOVEL     FVSNFV        PR1NFV                         NUMERO FV
140300960302     C                   MOVEL     FVSDFV        WDATA                          DATA FV
140400960302     C                   MOVEL     GG            PR1DFV
140500960302     C                   MOVE      MM            PR1DFV
140600960302     C* DATA/ORA REALE PARTENZA
140700960302     C                   Z-ADD     FVSDRP        WDATA
140800960302     C                   Z-ADD     MM            GGMM
140900960302     C                   MOVEL     GG            GGMM
141000960302     C                   Z-ADD     AA            PR1DRE
141100960302     C                   MOVEL     GGMM          PR1DRE
141200960302     C                   Z-ADD     FVSHRP        PR1HRE
141300960302     C*
141400960302     C* CAUSALE RITARDO
141500960302     C                   MOVEL     FVSRTP        PR1RTC
141600960307    1C     PR1RTC        IFNE      ' '
141700960302     C                   SETON                                        14
141800960302     C* IMPOSTA CAMPI KIAVE PER ACCESSO FNFV4
141900960302     C                   MOVEL     FVSLNP        TWKLNP
142000960302     C                   MOVEL     FVSNFV        TWKNFV
142100960302     C                   MOVEL     'P'           TWKEPA
142200960302     C                   CLEAR                   WLAI
142300960307     C* DECODIFICA CAUSALE
142400960307     C                   EXSR      DECCAU
142500960307    1C                   ENDIF
142600960302     C*
142700960307     C****  STAMPA TESTATA E DETTAGLIO  ****
142800960307     C                   EXSR      STARIG
142900960302     C*
143000960307     C****  STAMPA DI FNFV2  ****
143100960302     C                   EXSR      STAFV2
143200960302     C                   ENDSR
143300960302     C*
143400960302     C*--- LETTURA PER CARICO DESTINAZIONI FV -----------------------*
143500960302     C     CARFIL        BEGSR
143600960302     C                   Z-ADD     1             I                 3 0
143700960302     C                   Z-ADD     0             DS                3 0
143800960304     C                   CLEAR                   SK
143900960305     C                   CLEAR                   PLP
144000000504     C*
144100000504     C     KFWS          CHAIN     TNFWS01L                           39
144200000504     C     *IN39         IFEQ      *ON
144300000504     C     FWSATB        ORNE      ' '
144400000504     C                   CLEAR                   FWSFF3
144500000504     C                   CLEAR                   FWSFF4
144600000504     C                   CLEAR                   FWSFL3
144700000504     C                   CLEAR                   FWSFL4
144800000504     C                   ENDIF
144900960304     C*
145000960307    1C     FLP(I)        DOWNE     *BLANKS
145100960302     C     FLP(I)        LOOKUP    PLP                                    35
145200960302     C     *IN35         IFEQ      *OFF
145300960302     C                   MOVEL     FLP(I)        W0030             3 0
145400960302     C     W0030         CHAIN     AZORG01L                           32
145500960302     C                   ADD       1             DS
145600960302     C                   MOVEL     ORGPRO        SK(DS)
145700960302     C                   MOVE      '/'           SK(DS)
145800960302     C                   MOVEL     FLP(I)        PLP(DS)
145900960302     C                   ENDIF
146000960302     C*
146100960302     C                   ADD       1             I
146200960307    1C                   ENDDO
146300960302     C*
146400960302     C                   MOVEA     SK            PR1DES
146500960312     C*
146600960312     C****  VERIFICO LE DESTINAZIONI  ****
146700960312     C                   Z-ADD     6             DS
146800960312    1C     SK(DS)        IFNE      ' '
146900960312     C                   SETON                                        10
147000960312    1C                   ENDIF
147100960302     C                   ENDSR
147200960302     C*
147300960302     C*--- STAMPA DI FNFV2 ------------------------------------------*
147400960302     C     STAFV2        BEGSR
147500960304     C                   SETOFF                                       10
147600960302     C                   MOVEL     'ARR '        PR3FPA
147700960302     C*
147800960302     C     KFV2          SETLL     FNFV201L
147900960302     C     KFV2          READE     FNFV201L                               31
148000960307    1C     *IN31         DOWEQ     *OFF
148100960304     C*
148200960307    2C     FV2ATB        IFEQ      ' '
148300960302     C     FV2TDH        ANDEQ     'R'
148400960304     C                   SETOFF                                       142013
148500960304     C                   SETOFF                                       12
148600960307     C*
148700960304     C* PULIZIA CAMPI NON SEMPRE PIENI
148800960304     C                   CLEAR                   PR3SEG
148900960304     C                   CLEAR                   PR3DRE
149000960304     C                   CLEAR                   PR3HRE
149100960304     C                   CLEAR                   PR3RTC
149200960304     C                   CLEAR                   PD3RTC
149300960304     C                   CLEAR                   PR3IMP
149400960304     C*
149500960302     C* DATA/ORA REALE PARTENZA
149600960302     C                   Z-ADD     FV2DPA        WDATA
149700960302     C                   Z-ADD     MM            GGMM
149800960302     C                   MOVEL     GG            GGMM
149900960302     C                   Z-ADD     AA            PR3DRE
150000960302     C                   MOVEL     GGMM          PR3DRE
150100960302     C                   Z-ADD     FV2HPA        PR3HRE
150200960305     C* FILIALE SEGNALAZIONE
150300960305     C                   MOVEL     FV2LAI        PR3SEG
150400960302     C*
150500960302     C* CAUSALE RITARDO
150600960302     C                   MOVEL     FV2RTC        PR3RTC
150700960302     C*
150800960307    3C     PR3RTC        IFNE      ' '
150900960302     C                   SETON                                        14
151000960302     C* IMPOSTO CAMPI KIAVE PER ACCESSO FNFV4
151100960302     C                   MOVEL     FV2LNP        TWKLNP
151200960302     C                   MOVEL     FV2NFV        TWKNFV
151300960302     C                   MOVEL     FV2EPA        TWKEPA
151400960302     C                   MOVEL     FV2LAI        WLAI
151500960302     C*
151600960302     C                   Z-ADD     1             X
151700960302     C     PR3RTC        LOOKUP    TTC(X)                                 35
151800960302     C   35              MOVEL     ITC(X)        PR3IMP
151900960302     C  N35              MOVEL     '?'           PR3IMP
152000960302     C*
152100960307    4C     PR3RTC        IFNE      'G'
152200960302     C   35              MOVEL     DTC(X)        PD3RTC
152300960302     C  N35              MOVEL     *ALL'?'       PD3RTC
152400960302     C*
152500960307   X4C                   ELSE
152600960302     C* 13 ON  - CAUSALE RITARDO = "G"
152700960302     C                   SETON                                        13
152800960302     C*
152900960302     C     KFV4          SETLL     FNFV401L
153000960302     C     KFV4          READE     FNFV401L                               32
153100960307    5C     *IN32         DOWEQ     *OFF
153200960307     C     FV4ATB        IFEQ      ' '
153300960302     C                   MOVEL     FV4NOT        PD3RTC
153400960302     C                   SETON                                        32
153500960307     C                   ENDIF
153600960302     C  N32KFV4          READE     FNFV401L                               32
153700960307    5C                   ENDDO
153800960307    4C                   ENDIF
153900960307    3C                   ENDIF
154000960302     C*
154100960307     C****  STAMPO TESTATA  ****
154200960307    3C     *INOF         IFEQ      *ON
154300960302     C   16              EXCEPT    PIEDI
154400960302     C                   EXCEPT    TESTA2
154500960302     C                   SETOFF                                       OF
154600960307    3C                   ENDIF
154700960307     C*
154800960307     C****  STAMPO DETTAGLIO  ****
154900960302     C                   EXCEPT    DETFV2
155000960302     C*
155100960307     C****  FINISCO DI STAMPARE DESTINAZIONI E FNFV4  ****
155200960302     C   14              SETON                                        20
155300960307    3C     *IN20         DOWEQ     *ON
155400960302     C                   EXSR      REDFV4
155500960302     C*
155600960307    4C   20*INOF         IFEQ      *ON
155700960302     C   16              EXCEPT    PIEDI
155800960302     C                   EXCEPT    TESTA2
155900960302     C                   EXCEPT    DETFV2
156000960302     C                   SETOFF                                       OF
156100960307    4C                   ENDIF
156200960302     C*
156300960307     C* 2° RIGA DI DETTAGLIO
156400960302     C   20              EXCEPT    DETT2
156500960307    3C                   ENDDO
156600960302     C*
156700960302     C                   EXCEPT    FITT
156800960307    2C                   ENDIF
156900960307     C*
157000960302     C     KFV2          READE     FNFV201L                               31
157100960307    1C                   ENDDO
157200960302     C*
157300960302     C                   ENDSR
157400960302     C*
157500960226     C*--- SR INIZIALE -----------------------------------------------*
157600951110     C     *INZSR        BEGSR
157700960307     C****
157800960307     C** KLIST
157900960307     C****
158000000807     C* ACCESSO TNTLZ01L
158100000807     C     KTNTLZ        KLIST
158200000807     C                   KFLD                    KTLTIP            2
158300000807     C                   KFLD                    TSTPDR
158400000807     C                   MOVEL     'T '          KTLTIP
158500960307     C* ACCESSO TNTSR03L/TNTSM01L
158600960307     C     KTSR          KLIST
158700960307     C                   KFLD                    KEYTRN
158800960307     C                   KFLD                    KEYDTN
158900960307     C* ACCESSO TNTSR03L
159000960307     C     KTSR2         KLIST
159100960307     C                   KFLD                    KEYTRN
159200960307     C                   KFLD                    KEYDTN
159300960226     C                   KFLD                    FER(X)
159400960311     C     KTSR4         KLIST
159500960311     C                   KFLD                    TSTTRN
159600960311     C                   KFLD                    TSTDTN
159700960307     C* ACCESSO TNTSR09L/TNTSR08L
159800960307     C     KTSR3         KLIST
159900960311     C                   KFLD                    KEYDK
160000960307     C                   KFLD                    KEYFFP
160100960307     C                   KFLD                    KEYDTT
160200960307     C                   KFLD                    KEYDTN
160300960307     C                   KFLD                    KEYTRN
160400960229     C* ACCESSO TNTWK01L
160500960228     C     KTWK          KLIST
160600960228     C                   KFLD                    WTFP
160700960228     C                   KFLD                    TSRDTN
160800960228     C                   KFLD                    TSRTRN
160900960228     C                   KFLD                    WNFM
161000960228     C                   KFLD                    WEPA
161100960229     C     KTWK2         KLIST
161200960229     C                   KFLD                    TWKTFP
161300960229     C                   KFLD                    TWKDTN
161400960229     C                   KFLD                    TWKTRN
161500960302     C     KTWK3         KLIST
161600960302     C                   KFLD                    TWKTFP
161700960302     C                   KFLD                    TWKDTN
161800960302     C                   KFLD                    TWKTRN
161900960302     C                   KFLD                    TSTNFE
162000960229     C     KSAV          KLIST
162100960229     C                   KFLD                    KSTFP
162200960229     C                   KFLD                    KSDTN
162300960229     C                   KFLD                    KSTRN
162400960229     C                   KFLD                    KSNFM
162500960229     C                   KFLD                    KSEPA
162600960228     C* ACCESSO TNFVS01L
162700960228     C     KFVS          KLIST
162800960228     C                   KFLD                    TWKLNP
162900960228     C                   KFLD                    TWKNFV
163000960229     C* ACCESSO TNTST01L
163100960229     C     KTST          KLIST
163200960229     C                   KFLD                    TWKTRN
163300960229     C                   KFLD                    TWKDTN
163400960311     C     KTST3         KLIST
163500960311     C                   KFLD                    D36TRN
163600960311     C                   KFLD                    D36DTD
163700960311     C* ACCESSO TNTST04L
163800960311     C     KTST2         KLIST
163900960311     C                   KFLD                    D36TFP
164000960311     C                   KFLD                    D36DTD
164100960229     C* ACCESSO FNFV401L
164200960229     C     KFV4          KLIST
164300960229     C                   KFLD                    TWKLNP
164400960229     C                   KFLD                    TWKNFV
164500960229     C                   KFLD                    TWKEPA
164600960229     C                   KFLD                    WLAI
164700960229     C                   KFLD                    WTRC
164800960302     C* ACCESSO FNFV201L
164900960302     C     KFV2          KLIST
165000960302     C                   KFLD                    FVSLNP
165100960302     C                   KFLD                    FVSNFV
165200960302     C                   KFLD                    WEPA2
165300000504     C     KFWS          KLIST
165400000504     C                   KFLD                    FVSLNP
165500000504     C                   KFLD                    FVSNFV
165600951110     C* ACCESSO TABEL00F
165700960228     C     KTAB2         KLIST
165800951110     C                   KFLD                    CODUT
165900951110     C                   KFLD                    COD
166000951110     C*
166100960228     C****
166200960228     C** DEFINIZIONE CAMPI
166300960228     C****
166400960229     C     *LIKE         DEFINE    TBLCOD        COD
166500960307     C     *LIKE         DEFINE    §TCDES        PD1RTC
166600960307     C     *LIKE         DEFINE    D36DKD        PRTDAL
166700960307     C     *LIKE         DEFINE    D36DKA        PRTAL
166800960307     C*
166900960228     C     *LIKE         DEFINE    TWKEPA        WEPA
167000960228     C     *LIKE         DEFINE    TWKNFM        WNFM
167100960307     C     *LIKE         DEFINE    TWKFFC        WFFC
167200960307     C     *LIKE         DEFINE    TWKFIL        PR1SEG
167300960307     C     *LIKE         DEFINE    TWKHRE        PR1HRE
167400960307     C     *LIKE         DEFINE    TWKHTE        PR1HTE
167500960307     C     *LIKE         DEFINE    TWKTFP        SAVTFP
167600960307     C     *LIKE         DEFINE    TWKTRN        SAVTRN
167700960307     C     *LIKE         DEFINE    TWKDTN        SAVDTN
167800960307     C     *LIKE         DEFINE    TWKTFP        KSTFP
167900960307     C     *LIKE         DEFINE    TWKTRN        KSTRN
168000960307     C     *LIKE         DEFINE    TWKDTN        KSDTN
168100960307     C     *LIKE         DEFINE    TWKEPA        KSEPA
168200960307     C     *LIKE         DEFINE    TWKNFM        KSNFM
168300960307     C*
168400960307     C     *LIKE         DEFINE    FV2EPA        WEPA2
168500960229     C     *LIKE         DEFINE    FV4TRC        WTRC
168600960229     C     *LIKE         DEFINE    FV4LAI        WLAI
168700960229     C     *LIKE         DEFINE    FV4NOT        PR2NOT
168800960307     C*
168900960229     C     *LIKE         DEFINE    TSRNFP        FP
169000960229     C     *LIKE         DEFINE    TSRNFA        FA
169100960302     C     *LIKE         DEFINE    TSRFFP        WFFP
169200960229     C     *LIKE         DEFINE    TSRRTA        PR1RTC
169300960307     C*
169400960307     C     *LIKE         DEFINE    TSTTFP        WTFP
169500960229     C     *LIKE         DEFINE    TSTTRN        PR1TRN
169600960229     C     *LIKE         DEFINE    TSTTFP        PR1TFP
169700960229     C     *LIKE         DEFINE    TSTTFA        PR1TFA
169800960229     C     *LIKE         DEFINE    TSTTTR        PR1TTR
169900960301     C     *LIKE         DEFINE    TSTPDR        PR1PDR
170000960307     C*
170100960229     C     *LIKE         DEFINE    FVSLNP        PR1LNP
170200960229     C     *LIKE         DEFINE    FVSNFV        PR1NFV
170300960307     C*
170400960302     C     *LIKE         DEFINE    PR1SEG        PR3SEG
170500960302     C     *LIKE         DEFINE    PR1FPA        PR3FPA
170600960302     C     *LIKE         DEFINE    PR1DRE        PR3DRE
170700960302     C     *LIKE         DEFINE    PR1HRE        PR3HRE
170800960302     C     *LIKE         DEFINE    PR1RTC        PR3RTC
170900960302     C     *LIKE         DEFINE    PD1RTC        PD3RTC
171000960302     C     *LIKE         DEFINE    PR1IMP        PR3IMP
171100960229     C*
171200960311     C     *LIKE         DEFINE    TS9DKA        KEYDK
171300960307     C     *LIKE         DEFINE    TS9FFP        KEYFFP
171400960307     C     *LIKE         DEFINE    TS9DTT        KEYDTT
171500960307     C     *LIKE         DEFINE    TS9DTN        KEYDTN
171600960307     C     *LIKE         DEFINE    TS9TRN        KEYTRN
171700960307     C*
171800960307     C****  IMPOSTAZIONE CAMPI FISSI  ****
171900960307     C                   MOVEL     'R'           WTRC                           *R=RITARDO
172000960307     C                   MOVEL     'A'           WEPA2                          *A=ARRIVO
172100960307     C                   SETON                                        OF
172200951110     C*
172300960229     C                   Z-ADD     1             CODUT
172400960229     C                   CALL      'X§PARUT'
172500960229     C                   PARM                    UT§DSE
172600960229     C                   MOVEL     RAGUT         RSUT             20
172700960307     C*
172800960307     C****  PRENDO DATA E ORA CORRENTE  ****
172900960229     C                   TIME                    W0140            14 0
173000960229     C                   MOVE      W0140         UDATE8            8 0
173100960229     C                   MOVEL     W0140         UTIME             6 0
173200960228     C****
173300960228     C** CARICO TABELLA DELLE CAUSALI RITARDO
173400960228     C****
173500960228     C                   MOVEL     'TC'          COD
173600960228     C                   Z-ADD     1             X                 3 0
173700960228     C     KTAB2         SETLL     TABEL
173800960228     C     KTAB2         READE     TABEL                                  30
173900960228    1C     *IN30         DOWEQ     *OFF
174000960228    2C     TBLFLG        IFEQ      ' '
174100960228     C                   MOVEL     TBLKEY        TTC(X)
174200960228     C                   MOVEL     TBLUNI        DSTC
174300960228     C                   MOVEL     §TCDES        DTC(X)
174400960228     C                   MOVEL     §TCIMP        ITC(X)
174500960228     C                   ADD       1             X
174600960228    2C                   ENDIF
174700960228     C     KTAB2         READE     TABEL                                  30
174800960228    1C                   ENDDO
174900960228     C*
175000960229     C                   ENDSR
175100960228     O*--------------------------------------------------------------*
175200960228     O*     TESTATA                                                  *
175300960228     O*--------------------------------------------------------------*
175400960228     OPRTF198   E            TESTA2           02
175500960228     O                       TS(1)               66
175600960228     O                       TS(2)              132
175700960228     O                       TS(3)              198
175800960228     O                       RSUT                20
175900960228     O                       UDATE8             180 '  /  /    '
176000960228     O                       PAGE          Z    198
176100960228     O          E            TESTA2           03
176200960228     O                       KNSIF               10
176300960228     O                       KNMUS               24
176400960228     O                       UTIME              178 '  :  :  '
176500960228     O          E            TESTA2           05
176600960229     O                       TS(17)             132
176700960304     O                     23                    20 'ELENCO F.VIAGGIO NON'
176800960304     O                     23                  +  1 'ABBINATI'
176900960304     O                       PRTFRA              66
177000960311     O                     06                    85 'TRAINO   '
177100960311     O                       PRTDAL             103 '  /  /    '
177200960311     O                       PRTAL              119 '  /  /    '
177300960311     O                     06PRTTRA             145
177400960311     O                  02 06D36TFP             149
177500960312     O                 N02 06D36TRN        Z    153
177600960313     O                    N06PRTLNP             148
177700960313     O                    N06D36LNP        Z    152
177800960228     O*
177900960228     O          E            TESTA2           07
178000960228     O                       TS(5)               66
178100960228     O                       TS(9)              132
178200960228     O                       TS(13)             198
178300961022     O                     23                    43 'P.O.DI SCARICO'
178400960228     O          E            TESTA2           08
178500960228     O                       TS(6)               66
178600960228     O                       TS(10)             132
178700960228     O                       TS(14)             198
178800960302     O                     23                    98 '    '
178900960228     O          E            TESTA2           09
179000960228     O                       TS(7)               66
179100960228     O                       TS(11)             132
179200960228     O                       TS(15)             198
179300960302     O                     23                    98 '    '
179400960228     O          E            TESTA2           10
179500960228     O                       TS(21)              66
179600960228     O                       TS(21)             132
179700960228     O                       TS(21)             198
179800960228     O*
179900960228     O*--------------------------------------------------------------*
180000960228     O*     DETTAGLIO                                                *
180100960228     O*--------------------------------------------------------------*
180200960228     O          E            DETT        1
180300960228     O                       TS(18)              66
180400960228     O                       TS(19)             132
180500960228     O                       TS(20)             198
180600960301     O* DATA   TRAINO
180700960301     O                  05   PR1DTN               5 '  /  '
180800960228     O* NUMERO TRAINO
180900960301     O                  05   PR1TRN        Z     13
181000960228     O* PARTENZA/ARRIVO  TRAINO
181100960301     O                  05   PR1TFP        Z     17
181200960301     O                  05                       18 '/'
181300960301     O                  05   PR1TFA        Z     21
181400960228     O* TIPO TRAINO
181500960301     O                  05 09                    25 'BIS'
181600960301     O                  05 18                    25 'ECC'
181700960301     O                  05   PR1TTR              27
181800960228     O* DESTINAZIONI
181900960301     O                  05   PR1DES              43
182000960228     O* TRAZIONISTA
182100960301     O                  05   PR1PDR        Z     51
182200960301     O                  05                       52 '-'
182300960301     O                  05   PR1DPD              63
182400960228     O* CONDUCENTE
182500960301     O                  05   PR1DCN              75
182600960228     O* LNP / NUMERO / DATA  FOGLIO VIAGGIO
182700960301     O                     17PR1LNP        Z     80
182800960301     O                     17PR1NFV        Z     86
182900960301     O                     17PR1DFV              92 '  /  '
183000960228     O* FILIALE CHE SEGNALA
183100960301     O                       PR1SEG        Z     97
183200960228     O* PARTENZA/ARRIVO
183300960301     O                       PR1FPA             104
183400960229     O* PARTENZA/ARRIVO TEORICO
183500960301     O                       PR1DTE             113 '  /  /  '
183600960304     O                       PR1HTE             119 '  :  '
183700960229     O* PARTENZA/ARRIVO REALE
183800960301     O                       PR1DRE             128 '  /  /  '
183900960304     O                       PR1HRE             134 '  :  '
184000960228     O* MINUTI RITARDO
184100960301     O                       PR1MIN        Z    141
184200960301     O* R=RITARDO / A=ANTICIPO
184300960301     O                       PR1FRA             143
184400960229     O* CAUSALE RITARDO
184500960301     O                     14PR1RTC             146
184600960301     O                     14                   147 '-'
184700960301     O                     14PD1RTC             182
184800960228     O* IMPUTAZIONE
184900960301     O                       PR1IMP             185
185000960229     O* MESSAGGIO PER AVVISARE CHE MANCA LA DATA REALE PARTENZA/ARRIVO
185100960301     O                     15                   198 'MANCA DATA'
185200960228     O*--------------------------------------------------------------*
185300960228     O*     DETTAGLIO   DI   FNFV4                                   *
185400960228     O*--------------------------------------------------------------*
185500960301     O          E            DETT2       1
185600960229     O                       TS(18)              66
185700960229     O                       TS(19)             132
185800960229     O                       TS(20)             198
185900960228     O* DESTINAZIONI
186000960312     O               10      PR2DES              43
186100960229     O* DESCRIZIONI RITARDO
186200960301     O               20      PR2NOT             182
186300960228     O*
186400960302     O*--------------------------------------------------------------*
186500960302     O*     DETTAGLIO FNFV2                                          *
186600960302     O*--------------------------------------------------------------*
186700960302     O          E            DETFV2      1
186800960304     O                       TS(18)              66
186900960304     O                       TS(19)             132
187000960304     O                       TS(20)             198
187100960302     O* FILIALE CHE SEGNALA
187200960302     O                       PR3SEG        Z     97
187300960302     O* PARTENZA/ARRIVO
187400960302     O                       PR3FPA             104
187500960302     O* PARTENZA/ARRIVO REALE
187600960302     O                       PR3DRE             128 '  /  /  '
187700960304     O                       PR3HRE             134 '  :  '
187800960302     O* CAUSALE RITARDO
187900960302     O                     14PR3RTC             146
188000960302     O                     14                   147 '-'
188100960302     O                     14PD3RTC             182
188200960302     O* IMPUTAZIONE
188300960302     O                       PR3IMP             185
188400960302     O*
188500960307     O*--------------------------------------------------------------*
188600960307     O****  FITTIZZIO PER IL SALTO GIUSTO DELLE RIGHE  ****
188700960301     O          E            FITT        1
188800960301     O                                              ' '
188900960307     O****  FINE STAMPA  ****
189000960228     O          E            FINEST      2
189100960229     O                       TS(24)             132
189200960307     O****  FINE STAMPA 2  ****
189300960305     O          E            FINES2           66
189400960305     O                       TS(24)             132
189500960307     O****  PIEDI  ****
189600960228     O          E            PIEDI            65
189700960229     O                       TS(22)              66
189800960229     O                       TS(23)             132
189900960228     O*--------------------------------------------------------------*
190000960229**         CMD1
190100960223CLRPFM FILE(TNTWK00F)
190200960228**         MSG
190300960229SNDMSG MSG('ATTENZIONE!! FILE TNTWK00F GIA'' UTILIZZATO DA UN ALTRO
190400960328 LAVORO !!')
190500960312                                                    TOUSR(*SYSOPR)
190600960228**         TS
190700960305                                                                     1
190800960304     **  ELENCO  TRAZIONI  IN  RITARDO  **                           2
190900960228                   TNTL36R                               PAG. XXXX   3
191000960228                                                                     4
191100960301  T  R  A  I  N  O   |   TP  FERMATE TRAINO   TRAZIONISTA        C   5
191200960301DATA  NUMERO  LNP/LNA|   TR                                          6
191300960301                     |                                               7
191400960301XX/XX XXXXXXX XXX/XXX BIS X  XX/XX/XX/XX/XX XXXXXXX-XXXXXXXXXXX  X   8
191500961022ONDUCENTE  F.V I A G G I O |P.O.|       T E O R I C O    R E A L E   9
191600960301           LNP NUMERO DATA |SEGN|                                   10
191700960301                           |ALAZ|                                   11
191800960301XXXXXXXXX  XXX XXXXX XX/XX  XXX   XXXX XX/XX/XX XX:XX XX/XX/XX XX:  12
191900960301   |            R   I   T   A   R   D   O         IMPU|             13
192000960301   |  MIN  |            C A U S A L E             |TAZ|             14
192100960301   |  UTI  |                                      |   |             15
192200960301XX  XXXXX X  X-XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  X   MANCA DATA  16
192300960311     DATA CONTROLLO :  DAL XX/XX/XXXX   AL XX/XX/XXXX               17
192400960301                     |                                              18
192500960301                           |    |                                   19
192600960301   |       |                                      |   |             20
192700960229------------------------------------------------------------------  21
192800961022IMPUTAZIONE:   A=AUTISTA    F=FORZA MAGGIORE    P=P.O.PARTENZA      22
192900961022T=P.O.TRANSITO    S=SEDE                                            23
193000960229           * * *     F I N E      S T A M P A    * * *              24
