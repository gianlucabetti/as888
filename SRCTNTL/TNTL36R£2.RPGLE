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
009300070517     d
009400070517     d win04           s                   like(*in04)
009500070517     d win11           s                   like(*in11)
009600960304     D*
009700960304     D* DEFINIZIONE COSTANTI
009800960304     D CRIT            C                   CONST('RICHIESTI SOLO I -
009900960304     D                                     RITARDI')
010000960304     D CANT            C                   CONST('RICHIESTI SOLO GLI -
010100960304     D                                     ANTICIPI')
010200961022     D CTFP            C                   CONST('P.O.PARTENZA TRAINO:')
010300960311     D CTRN            C                   CONST('      NUMERO TRAINO:')
010400961022     D CLNP            C                   CONST('P.O.PARTENZA F.VIAGG-
010500960313     D                                     IO:')
010600951110     I*
010700960305     I* RIDENOMINO ALCUNI CAMPI DELLA VISTA LOGICA TNTSR08L
010800960305     ITNTSR008
010900960311     I              TSRDKP                      TS8DKP
011000960305     I              TSRFFP                      TS8FFP
011100960305     I              TSRDTT                      TS8DTT
011200960305     I              TSRDTN                      TS8DTN
011300960305     I              TSRTRN                      TS8TRN
011400960305     I* RIDENOMINO ALCUNI CAMPI DELLA VISTA LOGICA TNTSR09L
011500960305     ITNTSR009
011600960311     I              TSRDKA                      TS9DKA
011700960305     I              TSRFFP                      TS9FFP
011800960305     I              TSRDTT                      TS9DTT
011900960305     I              TSRDTN                      TS9DTN
012000960305     I              TSRTRN                      TS9TRN
012100951110     I/SPACE 3
012200951110     C*****************************************************************
012300951110     C* RIEPILOGO INDICATORI
012400951110     C*****************************************************************
012500960311     C* 02    - RICHIESTA UNA SPECIFICA FILIALE PARTENZA TRAINO
012600070521     C* 03    - RICHIESTO UNO SPECIFICO trazionista
012700960308     C* 04    - TROVATO ALMENO UN RITARDO PER LA LNP FV IMMESSA
012800960228     C* 05    - CAMBIATO TRAINO
012900960308     C* 06    - ON  --> RICHIESTA ELABORAZIONE PER DATA TRAINO
013000960308     C*         OFF --> RICHIESTA ELABORAZIONE PER DATA CONTROLLO
013100960228     C* 07    - IMMESSA DATA TRAINO AL
013200960228     C* 08    - IMMESSA DATA CONTROLLO AL
013300960228     C* 09    - STAMPO CATEGORIA TRAINO : BIS
013400960229     C* 10    - DEVO STAMPARE DELLE FILIALI DI CARICO/SCARICO IN 2 RIGA
013500960229     C* 12    - USATO PER ESEGUIRE UNA ISTRUZIONE UNA SOLA VOLTA
013600960229     C* 13    - CAUSALE RITARDO "G"
013700960229     C* 14    - PRESENTE CAUSALE RITARDO
013800960229     C* 15    - MANCA LA DATA PARTENZA/ARRIVO REALE
013900960229     C* 16    - NON STAMPO LA PRIMA VOLTA I PIEDI
014000960302     C* 17    - STAMPO DATI DEL FOGLIO VIAGGIO
014100960301     C* 18    - STAMPO CATEGORIA TRAINO : ECCEZIONALE
014200070521     C* 19    - RICHIESTA UNA SPECIFICA filiale fermata arrivo
014300960229     C* 20    - PRESENTE FNFV4 DA STAMPARE
014400070521     C* 21    - stampo n.ro traino in caso di selez. per data f.v.teor.
014500960229     C* 22 OFF- NON C'E' NULLA DA STAMPARE: STAMPO LA TESTATA
014600960302     C* 23    - SONO IN STAMPA DI FV NON ABBINATI A TRAINO
014700960312     C* 25    - STAMPATO ALMENO UN FV NON ABBINATO A TRAINO
014800960229     C* 29    - LE DATE CONTROLLO DEL TRAINO SONO TUTTE 99999999
014900960301     C* 30/33 - DI COMODO
015000960302     C* 35    - USATO PER LOKUP
015100951110     C*****************************************************************
015200951110     C     *ENTRY        PLIST
015300951110     C                   PARM                    KPJBA
015400960223     C                   MOVEL     KPJBU         DSTL36
015500951113     C*
015600951110     C*---------------------------------------------------------------*
015700960307     C****  ESEGUO CLEAR DEL FILE DI WORK  ****
015800960229     C                   EXSR      CLRTWK
015900960229     C   99              GOTO      FINE
016000960223     C*
016100960307     C****  CONTROLLO RICHIESTE FATTE A VIDEO  ****
016200960223     C                   EXSR      CTRRIC
016300960223     C*
016400960308     C****  ELABORAZIONE PER DATA TRAINO  ****
016500960311    1C     *IN06         IFEQ      *ON
016600960308     C                   EXSR      ELABDT
016700960311   X1C                   ELSE
016800960307     C****  ELABORAZIONE PER DATA CONTROLLO  ****
016900960308     C                   EXSR      ELABDK
017000960311    1C                   ENDIF
017100960223     C*
017200960307     C****  STAMPA DEI RITARDI/ANTICIPI  ****
017300960229     C     *LOVAL        SETLL     TNTWK01L
017400960229     C                   READ      TNTWK01L                               30
017500960228     C*
017600960229    1C     *IN30         DOWEQ     *OFF
017700960229    2C     TWKDRE        IFNE      0
017800960228     C                   EXSR      STAMPA
017900960229    2C                   ENDIF
018000960228     C*
018100960229     C                   READ      TNTWK01L                               30
018200960229    1C                   ENDDO
018300960228     C*
018400960307     C****  STAMPA DEI FOGLI VIAGGIO NON ABBINATI A TRAINO  ****
018500960311    1C  N06D36FRA        IFEQ      'E'
018600960302     C     D36FRA        OREQ      'R'
018700960312     C                   SETOFF                                       16
018800960312     C                   SETON                                        231705
018900070523     C                   SETOFF                                       031921
019000960312     C*
019100960312     C     *INOF         IFEQ      *ON
019200960312     C                   SETON                                        25
019300960312     C                   ELSE
019400960302     C                   SETON                                        OF
019500960312     C                   ENDIF
019600960302     C*
019700960311     C* APERTURA FILES
019800960311     C                   OPEN      TNFVS02L
019900960311     C                   OPEN      FNFV201L
020000960311     C*
020100960302     C     *LOVAL        SETLL     TNFVS02L
020200960302     C                   READ      TNFVS02L                               30
020300960302    2C     *IN30         DOWEQ     *OFF
020400960305   2AC     FVSATB        IFEQ      ' '
020500960305     C     FVSTRS        ANDEQ     0
020600960302     C*
020700960302     C* CONTROLLO FINE LETTURA SE IMMESSA DATA AL
020800960311    3C     FVSDKF        IFGT      D36DKA
020900960302     C                   SETON                                        30
021000960302    3C                   ENDIF
021100960302     C*
021200960308     C* SE IMMESSA CONTROLLO LA LINEA PARTENZA F.VIAGGIO
021300960308     C  N30D36LNP        IFGT      0
021400960308     C     D36LNP        ANDNE     FVSLNP
021500960308     C                   SETON                                        30
021600960308     C                   ENDIF
021700960308     C*
021800960302    3C     *IN30         IFEQ      *OFF
021900960302     C     FVSDKF        ANDGT     0
022000960312     C                   SETON                                        25
022100960302     C                   EXSR      STAMP3
022200960302    3C                   ENDIF
022300960302     C*
022400960305   2AC                   ENDIF
022500960302     C                   READ      TNFVS02L                               30
022600960302    2C                   ENDDO
022700960312     C*
022800960312     C* SE NON HO STAMPATO NEMMENO UN FV, SPENGO INDICATORE DI OVERFLOW
022900960312     C*   PER STAMPARE "FINE STAMPA" NELLA GIUSTA POSIZIONE
023000960312     C  N25              SETOFF                                       OF
023100960302    1C                   ENDIF
023200960302     C*
023300960307     C****  SE NON C'E' NIENTE DA STAMPARE STAMPO LA TESTATA  ****
023400960229    1C     *IN22         IFEQ      *OFF
023500960229     C                   EXCEPT    TESTA2
023600960308     C                   SETOFF                                       OF
023700960229    2C                   DO        10
023800960301     C                   EXCEPT    FITT
023900960229    2C                   ENDDO
024000960229    1C                   ENDIF
024100960229     C*
024200960307     C****  STAMPA DICITURA: FINE STAMPA  ****
024300960311    1C     *INOF         IFEQ      *ON
024400960305     C                   EXCEPT    PIEDI
024500960305     C                   EXCEPT    FINES2
024600960311   X1C                   ELSE
024700960229     C                   EXCEPT    FINEST
024800960229     C                   EXCEPT    PIEDI
024900960311    1C                   ENDIF
025000960229     C*
025100960307     C****  CHIUSURA FILE DI WORK  ****
025200960229     C                   CLOSE     TNTWK01L
025300960307     C*
025400951110     C     FINE          TAG
025500951113     C                   SETON                                        LR
025600960117     C*
025700960229     C*--- CLEAR FILE DI WORK ----------------------------------------*
025800960229     C     CLRTWK        BEGSR
025900960229     C                   Z-ADD     21            LUNG
026000960229     C                   MOVEL     *BLANKS       COMMAN           80
026100960229     C                   MOVEA     CMD1(1)       COMMAN
026200960229     C                   CALL      'QCMDEXC'                            99
026300960229     C                   PARM                    COMMAN           80
026400960229     C                   PARM                    LUNG             15 5
026500960229     C*
026600960229     C* 99 ON  - CLEAR NON ANDATA A BUON FINE: INVIO MESSAGGIO ALLA
026700960229     C*   CODA DELL'OPERATORE ED ESCO DAL PGM
026800960229    1C     *IN99         IFEQ      *ON
026900960229     C                   MOVEA     MSG(1)        ER(1)
027000960229     C                   MOVEA     MSG(2)        ER(68)
027100960229     C                   MOVEA     MSG(3)        ER(135)
027200960229     C                   MOVEA     ER            COMM            201
027300960229     C                   Z-ADD     201           LUNG
027400960229     C                   CALL      'QCMDEXC'
027500960229     C                   PARM                    COMM
027600960229     C                   PARM                    LUNG
027700960229     C*
027800960229   X1C                   ELSE
027900960229     C                   OPEN      TNTWK01L
028000960229    1C                   ENDIF
028100960229     C                   ENDSR
028200960229     C*
028300960223     C*--- CONTROLLO RICHIESTE FATTE A VIDEO -------------------------*
028400960223     C     CTRRIC        BEGSR
028500960311     C                   CLEAR                   PRTTRA
028600960311     C                   CLEAR                   PRTFRA
028700960313     C                   CLEAR                   PRTLNP
028800960223     C*
028900960223     C****  DATA TRAINO  ****
029000960223     C* SE DATA TRAINO  AL = 0 --> METTO DATA DAL IN DATA AL
029100960308    0C     D36DTD        IFGT      0
029200960308     C                   SETON                                        06
029300960308     C*
029400960229    1C     D36DTA        IFEQ      0
029500960223     C                   Z-ADD     D36DTD        D36DTA
029600960229   X1C                   ELSE
029700960223     C* 07 ON  - IMMESSA DATA TRAINO  AL
029800960229    2C     D36DTA        IFNE      D36DTD
029900960223     C                   SETON                                        07
030000960229    2C                   ENDIF
030100960229    1C                   ENDIF
030200960308     C*
030300960308     C* GIRO DATA TRAINO DAL
030400960308     C                   Z-ADD     D36DTD        WDATA
030500960308     C                   Z-ADD     MM            GGMM
030600960308     C                   MOVEL     GG            GGMM
030700960308     C                   Z-ADD     AA            PRTDAL
030800960308     C                   MOVEL     GGMM          PRTDAL
030900960308     C* GIRO DATA TRAINO  AL
031000960308     C                   Z-ADD     D36DTA        WDATA
031100960308     C                   Z-ADD     MM            GGMM
031200960308     C                   MOVEL     GG            GGMM
031300960308     C                   Z-ADD     AA            PRTAL
031400960308     C                   MOVEL     GGMM          PRTAL
031500960311     C*
031600960313     C* CONTROLLO SE IMMESSI LA LINEA PARTENZA TRAINO O IL NUM.TRAINO
031700960311    1C     D36TFP        IFGT      0
031800960311     C                   SETON                                        02
031900960311     C                   MOVEL     CTFP          PRTTRA           20
032000960311   X1C                   ELSE
032100960311     C     D36TRN        IFGT      0
032200960311     C                   MOVEL     CTRN          PRTTRA
032300960311     C                   ENDIF
032400960311    1C                   ENDIF
032500960308    0C                   ENDIF
032600960223     C*
032700960223     C****  DATA CONTROLLO  ****
032800960223     C* SE DATA CONTROLLO  AL = 0 --> METTO DATA DAL IN DATA AL
032900960308    0C     D36DKD        IFGT      0
033000960308     C                   SETOFF                                       06
033100960308     C*
033200960229    1C     D36DKA        IFEQ      0
033300960223     C                   Z-ADD     D36DKD        D36DKA
033400960229   X1C                   ELSE
033500960223     C* 08 ON  - IMMESSA DATA CONTROLLO  AL
033600960229    2C     D36DKA        IFNE      D36DKD
033700960223     C                   SETON                                        08
033800960229    2C                   ENDIF
033900960229    1C                   ENDIF
034000960223     C*
034100960302     C* GIRO DATA CONTROLLO DAL
034200960228     C                   Z-ADD     D36DKD        WDATA
034300960228     C                   Z-ADD     MM            GGMM
034400960228     C                   MOVEL     GG            GGMM
034500960228     C                   Z-ADD     AA            PRTDAL
034600960228     C                   MOVEL     GGMM          PRTDAL
034700960302     C* GIRO DATA CONTROLLO  AL
034800960228     C                   Z-ADD     D36DKA        WDATA
034900960228     C                   Z-ADD     MM            GGMM
035000960228     C                   MOVEL     GG            GGMM
035100960228     C                   Z-ADD     AA            PRTAL
035200960228     C                   MOVEL     GGMM          PRTAL
035300960313     C*
035400960313     C* CONTROLLO SE IMMESSA LA LINEA PARTENZA FOGLIO VIAGGIO
035500960313    1C     D36LNP        IFGT      0
035600960313     C                   MOVEL     CLNP          PRTLNP           23
035700960313    1C                   ENDIF
035800960308    0C                   ENDIF
035900960302     C*
036000960304     C****  RICHIESTA DEI RITARDI/ANTICIPI/ENTRAMBI  ****
036100960304     C     D36FRA        IFEQ      'R'
036200960304     C                   MOVEL     CRIT          PRTFRA           27
036300960304     C                   ENDIF
036400960304     C     D36FRA        IFEQ      'A'
036500960304     C                   MOVEL     CANT          PRTFRA
036600960304     C                   ENDIF
036700070521     c*
036800070521     c     d36pdr        comp      0                                  03
036900070521     c     d36ffa        comp      0                                  19
037000070521     c                   setoff                                       21
037100070521     c                   if        *in06=*off and d36trn>0
037200070521     c                   seton                                        21
037300070521     c                   endif
037400960304     C*
037500960223     C                   ENDSR
037600960223     C*
037700960308     C*--- ELABORAZIONE PER DATA TRAINO ------------------------------*
037800960308     C     ELABDT        BEGSR
037900960308     C*
038000960311     C* IMMESSI DATA TRAINO E LINEA PARTENZA TRAINO
038100960311    1C     D36TFP        IFGT      0
038200960311     C     KTST2         SETLL     TNTST04L
038300960311   X1C                   ELSE
038400960311     C* IMMESSI DATA TRAINO E NUMERO TRAINO
038500960311    2C     D36TRN        IFGT      0
038600960311     C     KTST3         SETLL     TNTST01L
038700960311   X2C                   ELSE
038800960311     C* IMMESSA SOLO LA DATA TRAINO
038900960311     C     D36DTD        SETLL     TNTST03L
039000960311    2C                   ENDIF
039100960311    1C                   ENDIF
039200960311     C*
039300960311     C* LETTURA RECORD
039400960311     C                   EXSR      REDTST
039500960311     C*
039600960311    1C     *IN30         DOWEQ     *OFF
039700960311     C*
039800960311     C* CONTROLLO FINE LETTURA SE IMMESSA DATA AL
039900960311    2C   07TSTDTN        IFGT      D36DTA
040000960311     C                   SETON                                        30
040100960311    2C                   ENDIF
040200960311     C*
040300960311    2C     *IN30         IFEQ      *OFF
040400960311     C* ESCLUDO RECORD ANNULLATI
040500960311    3C     TSTATB        IFEQ      ' '
040600960311     C                   MOVEL     'N'           WELAB
040700960311     C                   CLEAR                   WCONT
040800960311     C                   CLEAR                   WFFP
040900960311     C                   CLEAR                   WFFC
041000960311     C* MEMORIZZO LA FILIALE PARTENZA DEL TRAINO
041100960311     C                   MOVEL     TSTTFP        WTFP
041200960311     C*
041300960311     C* LEGGO TUTTE LE FERMATE DEL TRAINO PER VEDERE SE ESISTE ALMENO
041400960311     C*   UN RITARDO/ANTICIPO OPPURE UN MANCA DATA REALE
041500960311     C     KTSR4         SETLL     TNTSR03L
041600960311     C     KTSR4         READE     TNTSR03L                               31
041700960311     C*
041800960311    4C     *IN31         DOWEQ     *OFF
041900960311     C* VERIFICO SE IL TRAINO E' DA STAMPARE
042000960311    5C     TSRDRP        IFEQ      0
042100960311     C     TSRDRA        OREQ      0
042200960311     C     WDHRP         ORNE      WDHTP
042300960311     C     WDHRA         ORNE      WDHTA
042400970411     C* Testo anche la data controllo perchè se diversa da 99999999
042500970411     C* significa che potrei avere una causale ritardo nonostante
042600970411     C* data/ora teorica = data/ora reale
042700970411     C     TSRDKP        ORNE      99999999
042800970411     C     TSRDKA        ORNE      99999999
042900960311     C                   CLEAR                   WELAB
043000960311    5C                   ENDIF
043100960311     C*
043200960311     C* VERIFICO SE IL TRAINO HA UNA SOLA FERMATA DI CARICO
043300960311    5C     WFFP          IFNE      TSRFFP
043400960311     C                   ADD       1             WCONT
043500960311     C                   MOVEL     TSRFFP        WFFP
043600960311    5C                   ENDIF
043700960311     C*
043800960311     C* SE IL TRAINO HA PIU' FERMATE DI CARICO ED E' UN TRAINO DA
043900960311     C*   STAMPARE E' INUTILE CONTINUARE A LEGGERE ALTRI RECORD
044000960311    5C     WCONT         IFGT      1
044100960311     C     WELAB         ANDEQ     ' '
044200960311     C                   SETON                                        31
044300960311    5C                   ENDIF
044400960311     C*
044500960311     C  N31KTSR4         READE     TNTSR03L                               31
044600960311    4C                   ENDDO
044700960311     C*
044800960311     C* WELAB = ' ' ---> TRAINO DA STAMPARE: SCRIVO FILE DI WORK
044900960311    4C     WELAB         IFEQ      ' '
045000960311     C*
045100960311     C* MEMORIZZO SE IL TRAINO HA UNA SOLA FERMATA DI CARICO
045200960311    5C     WCONT         IFEQ      1
045300960311     C                   MOVEL     'S'           WFFC
045400960311    5C                   ENDIF
045500960311     C*
045600960311     C     KTSR4         SETLL     TNTSR03L
045700960311     C     KTSR4         READE     TNTSR03L                               31
045800960311    5C     *IN31         DOWEQ     *OFF
045900960311     C* CREO FILE DI WORK
046000960311     C                   EXSR      CR3TWK
046100960311     C*
046200960311     C     KTSR4         READE     TNTSR03L                               31
046300960311    5C                   ENDDO
046400960311     C*
046500960311    4C                   ENDIF
046600960311    3C                   ENDIF
046700960311     C*
046800960311     C* LETTURA RECORD SUCESSIVO
046900960311     C                   EXSR      REDTST
047000960311    2C                   ENDIF
047100960311    1C                   ENDDO
047200960311     C*
047300960308     C                   ENDSR
047400960308     C*
047500960311     C*--- LETTURA RECORD DI TNTST -----------------------------------*
047600960311     C     REDTST        BEGSR
047700960311     C*
047800960311     C* IMMESSI DATA TRAINO E LINEA PARTENZA TRAINO
047900960311    1C     D36TFP        IFGT      0
048000960311     C  N07KTST2         READE     TNTST04L                               30
048100960311     C   07D36TFP        READE     TNTST04L                               30
048200960311     C*
048300960311   X1C                   ELSE
048400960311     C* IMMESSI DATA TRAINO E NUMERO TRAINO
048500960311    2C     D36TRN        IFGT      0
048600960311     C  N07KTST3         READE     TNTST01L                               30
048700960311     C   07D36TRN        READE     TNTST01L                               30
048800960311     C*
048900960311   X2C                   ELSE
049000960311     C* IMMESSA SOLO LA DATA TRAINO
049100960311     C  N07D36DTD        READE     TNTST03L                               30
049200960311     C   07              READ      TNTST03L                               30
049300960311    2C                   ENDIF
049400960311    1C                   ENDIF
049500960311     C*
049600960311     C                   ENDSR
049700960311     C*
049800960311     C*--- CREO FILE DI WORK -----------------------------------------*
049900960311     C     CR3TWK        BEGSR
050000960311     C*
050100960311     C****  FILIALE FERMATA PARTENZA  ****
050200960311     C                   MOVEL     TSRNFP        WNFM
050300960311     C                   MOVEL     'P'           WEPA
050400960311     C     KTWK          CHAIN     TNTWK01L                           32
050500960311     C*
050600960311     C   32              CLEAR                   TNTWK000
050700960311     C*
050800960311     C* SE DATA/ORA REALE PARTENZA E' UGUALE ALLA DATA/ORA TEORICA
050900960311     C*   PARTENZA  --->  NON C'E' RITARDO
051000970411     C* PER NON ESSERCI RITARDO E' ANCHE NECESSARIO CHE LA DATA CONTROL
051100970411     C* SIA = 99999999
051200960311    1C     WDHTP         IFEQ      WDHRP
051300970411     C     TSRDKP        ANDEQ     99999999
051400060407     c                   if        *in32
051500960311     C                   CLEAR                   TWKDTE
051600960311     C                   CLEAR                   TWKHTE
051700960311     C                   CLEAR                   TWKDRE
051800960311     C                   CLEAR                   TWKHRE
051900060411     C                   MOVEL     TSRRTP        TWKRTC
052000060411     C                   MOVEL     TSRLNP        TWKLNP
052100060411     C                   MOVEL     TSRNFV        TWKNFV
052200060411     C                   MOVEL     TSRDFV        TWKDFV
052300060407     c                   endif
052400960311     C*
052500960311   X1C                   ELSE
052600960311     C*
052700960311    2C* MANCA DATA PARTENZA REALE
052800960311     C     TSRDRP        IFEQ      0
052900960311     C                   MOVEL     *ALL'9'       TWKDRE
053000960311     C                   MOVEL     *ALL'9'       TWKHRE
053100960311     C                   MOVEL     TSRDTP        TWKDTE
053200960311     C                   MOVEL     TSRHTP        TWKHTE
053300960311   X2C                   ELSE
053400960311     C* PRESENTE RITARDO
053500960311     C                   MOVEL     TSRDRP        TWKDRE
053600960311     C                   MOVEL     TSRHRP        TWKHRE
053700960311     C                   MOVEL     TSRDTP        TWKDTE
053800960311     C                   MOVEL     TSRHTP        TWKHTE
053900960311    2C                   ENDIF
054000060411     C                   MOVEL     TSRRTP        TWKRTC
054100060411     C                   MOVEL     TSRLNP        TWKLNP
054200060411     C                   MOVEL     TSRNFV        TWKNFV
054300060411     C                   MOVEL     TSRDFV        TWKDFV
054400960311    1C                   ENDIF
054500960311     C*
054600960311     C*
054700960311     C     WFFC          IFEQ      'S'
054800960311     C                   MOVEL     'S'           TWKFFC
054900960311     C                   ELSE
055000960311     C                   CLEAR                   TWKFFC
055100960311     C                   ENDIF
055200960311     C*
055300960311    1C     *IN32         IFEQ      *ON
055400960311     C                   MOVEL     TSRTRN        TWKTRN
055500960311     C                   MOVEL     TSRDTN        TWKDTN
055600960311     C                   MOVEL     WTFP          TWKTFP
055700960311     C                   MOVEL     'P'           TWKEPA
055800960311     C                   MOVEL     TSRNFP        TWKNFM
055900960311     C                   MOVEL     TSRFFP        TWKFIL
056000960311     C                   WRITE     TNTWK000
056100960311     C*
056200960311   X1C                   ELSE
056300960311     C                   UPDATE    TNTWK000
056400960311    1C                   ENDIF
056500960311     C*
056600960311     C****  FILIALE FERMATA ARRIVO  ****
056700960311     C                   MOVEL     TSRNFA        WNFM
056800960311     C                   MOVEL     'A'           WEPA
056900960311     C     KTWK          CHAIN     TNTWK01L                           32
057000960311     C*
057100960311     C   32              CLEAR                   TNTWK000
057200960311     C*
057300960311     C* SE DATA/ORA REALE ARRIVO E' UGUALE ALLA DATA/ORA TEORICA
057400960311     C*   ARRIVO  --->  NON C'E' RITARDO
057500970411     C* PER NON ESSERCI RITARDO E' ANCHE NECESSARIO CHE LA DATA CONTROL
057600970411     C* SIA = 99999999
057700960311    1C     WDHTA         IFEQ      WDHRA
057800970411     C     TSRDKA        ANDEQ     99999999
057900060407     c                   if        *in32
058000960311     C                   CLEAR                   TWKDTE
058100960311     C                   CLEAR                   TWKHTE
058200960311     C                   CLEAR                   TWKDRE
058300960311     C                   CLEAR                   TWKHRE
058400060411     C                   MOVEL     TSRRTA        TWKRTC
058500060411     C                   MOVEL     TSRLNP        TWKLNP
058600060411     C                   MOVEL     TSRNFV        TWKNFV
058700060411     C                   MOVEL     TSRDFV        TWKDFV
058800060407     c                   endif
058900960311     C*
059000960311   X1C                   ELSE
059100960311     C*
059200960311     C* MANCA DATA ARRIVO REALE
059300960311    2C     TSRDRA        IFEQ      0
059400960311     C                   MOVEL     *ALL'9'       TWKDRE
059500960311     C                   MOVEL     *ALL'9'       TWKHRE
059600960311     C                   MOVEL     TSRDTA        TWKDTE
059700960311     C                   MOVEL     TSRHTA        TWKHTE
059800960311   X2C                   ELSE
059900960311     C* PRESENTE RITARDO
060000960311     C                   MOVEL     TSRDRA        TWKDRE
060100960311     C                   MOVEL     TSRHRA        TWKHRE
060200960311     C                   MOVEL     TSRDTA        TWKDTE
060300960311     C                   MOVEL     TSRHTA        TWKHTE
060400960311    2C                   ENDIF
060500060411     c
060600060411     C                   MOVEL     TSRRTA        TWKRTC
060700060411     C                   MOVEL     TSRLNP        TWKLNP
060800060411     C                   MOVEL     TSRNFV        TWKNFV
060900060411     C                   MOVEL     TSRDFV        TWKDFV
061000960311    1C                   ENDIF
061100960311     C*
061200960311     C*
061300960311     C     WFFC          IFEQ      'S'
061400960311     C                   MOVEL     'S'           TWKFFC
061500960311     C                   ELSE
061600960311     C                   CLEAR                   TWKFFC
061700960311     C                   ENDIF
061800960311     C*
061900960311    1C     *IN32         IFEQ      *ON
062000960311     C                   MOVEL     TSRTRN        TWKTRN
062100960311     C                   MOVEL     TSRDTN        TWKDTN
062200960311     C                   MOVEL     WTFP          TWKTFP
062300960311     C                   MOVEL     'A'           TWKEPA
062400960311     C                   MOVEL     TSRNFA        TWKNFM
062500960311     C                   MOVEL     TSRFFA        TWKFIL
062600960311     C                   WRITE     TNTWK000
062700960311     C*
062800960311   X1C                   ELSE
062900960311     C                   UPDATE    TNTWK000
063000960311    1C                   ENDIF
063100960311     C*
063200960311     C                   ENDSR
063300960311     C*
063400960228     C*--- ELABORAZIONE PER DATA CONTROLLO ---------------------------*
063500960228     C     ELABDK        BEGSR
063600960223     C*
063700960226     C*****
063800960307     C* DATA CONTROLLO BUCO/RITARDO    A  R  R  I  V  O
063900960226     C*****
064000960307     C                   MOVEL     'A'           WFLG              1
064100960307     C*
064200960227     C     D36DKD        SETLL     TNTSR09L
064300960311     C  N08D36DKD        READE     TNTSR09L                               30
064400960311     C   08              READ      TNTSR09L                               30
064500960223     C*
064600960223    1C     *IN30         DOWEQ     *OFF
064700960307     C* SALVO CAMPI KIAVE
064800960311     C                   MOVEL     TS9DKA        KEYDK
064900960307     C                   MOVEL     TS9FFP        KEYFFP
065000960307     C                   MOVEL     TS9DTT        KEYDTT
065100960307     C                   MOVEL     TS9DTN        KEYDTN
065200960307     C                   MOVEL     TS9TRN        KEYTRN
065300960307     C*
065400960223     C* CONTROLLO FINE LETTURA SE IMMESSA DATA AL
065500960311    2C   08TS9DKA        IFGT      D36DKA
065600960223     C                   SETON                                        30
065700960223    2C                   ENDIF
065800960223     C*
065900960223    2C     *IN30         IFEQ      *OFF
066000960307     C* LEGGO TUTTE LE FERMATE DEL TRAINO
066100960307     C                   EXSR      REDTSR
066200960307     C*
066300960307     C* SE TUTTO OK ---> SCRIVO FILE DI WORK
066400960229    3C     WELAB         IFEQ      ' '
066500960229     C     *IN29         ANDEQ     *OFF
066600960307     C                   EXSR      REDFER
066700960307     C*
066800960226   X3C                   ELSE
066900960307     C*              ... ALTRIMENTI MI POSIZIONO SUL TRAINO SUCESSIVO
067000960307     C     KTSR3         SETGT     TNTSR09L
067100960226    3C                   ENDIF
067200960227     C*
067300960311     C  N08D36DKD        READE     TNTSR09L                               30
067400960311     C   08              READ      TNTSR09L                               30
067500960227    2C                   ENDIF
067600960227    1C                   ENDDO
067700960226     C*
067800960227     C*****
067900960307     C* DATA CONTROLLO BUCO/RITARDO    P  A  R  T  E  N  Z  A
068000960227     C*****
068100070518     c* se richiesta una specifica filiale fermata arrivo si vogliono stampa
068200070518     c* re solo i ritardi arrivo e quindi salto il seguente ciclo
068300070518     c                   if        d36ffa=0
068400960307     C                   MOVEL     'P'           WFLG
068500960307     C*
068600960227     C     D36DKD        SETLL     TNTSR08L
068700960311     C  N08D36DKD        READE     TNTSR08L                               30
068800960311     C   08              READ      TNTSR08L                               30
068900960227     C*
069000960227    1C     *IN30         DOWEQ     *OFF
069100960307     C* SALVO CAMPI KIAVE
069200960311     C                   MOVEL     TS8DKP        KEYDK
069300960307     C                   MOVEL     TS8FFP        KEYFFP
069400960307     C                   MOVEL     TS8DTT        KEYDTT
069500960307     C                   MOVEL     TS8DTN        KEYDTN
069600960307     C                   MOVEL     TS8TRN        KEYTRN
069700960227     C*
069800960227     C* CONTROLLO FINE LETTURA SE IMMESSA DATA AL
069900960311    2C   08TS8DKP        IFGT      D36DKA
070000960227     C                   SETON                                        30
070100960227    2C                   ENDIF
070200960227     C*
070300960227    2C     *IN30         IFEQ      *OFF
070400960307     C* LEGGO TUTTE LE FERMATE DEL TRAINO
070500960307     C                   EXSR      REDTSR
070600960306     C*
070700960307     C* SE TUTTO OK ---> SCRIVO FILE DI WORK
070800960229    3C     WELAB         IFEQ      ' '
070900960229     C     *IN29         ANDEQ     *OFF
071000960307     C                   EXSR      REDFER
071100960229     C*
071200960227   X3C                   ELSE
071300960307     C*              ... ALTRIMENTI MI POSIZIONO SUL TRAINO SUCESSIVO
071400960307     C     KTSR3         SETGT     TNTSR08L
071500960227    3C                   ENDIF
071600960227     C*
071700960311     C  N08D36DKD        READE     TNTSR08L                               30
071800960311     C   08              READ      TNTSR08L                               30
071900960227    2C                   ENDIF
072000960227    1C                   ENDDO
072100070518     c                   endif
072200960227     C*
072300960223     C                   ENDSR
072400960223     C*
072500960307     C*--- LETTURA FERMATE DEL TRAINO --------------------------------*
072600960307     C     REDTSR        BEGSR
072700960307     C                   SETON                                        29
072800070517     C                   SETOFF                                       0411
072900960307     C                   CLEAR                   FER
073000960307     C                   CLEAR                   WELAB
073100960307     C                   CLEAR                   WCONT
073200960307     C                   CLEAR                   WFFP
073300960307     C                   CLEAR                   WFFC
073400960307     C*
073500960307     C* LEGGO TUTTE LE FERMATE DEL TRAINO
073600960307     C     KTSR          SETLL     TNTSR03L
073700960307     C     KTSR          READE     TNTSR03L                               31
073800960307     C*
073900960307    1C     *IN31         DOWEQ     *OFF
074000960307     C*
074100960307     C****  DATA CONTROLLO   A  R  R  I  V  O  ****
074200960307    2C     WFLG          IFEQ      'A'
074300960307     C*
074400960307     C* SE TROVO UNA DATA CONTROLLO UGUALE A ZERO O MAGGIORE DI QUELLA
074500960307     C*   RICHIESTA A VIDEO --> NON ELABORO E LEGGO IL TRAINO SUCESSIVO
074600960307    3C     TSRDKA        IFNE      *ALL'9'
074700960307     C                   SETOFF                                       29
074800960307     C     TSRDKA        IFGT      D36DKA
074900960307     C     TSRDKA        OREQ      0
075000960307     C                   MOVEL     'N'           WELAB             1
075100960307     C                   SETON                                        31
075200960307     C                   ENDIF
075300960307    3C                   ENDIF
075400960307     C*
075500960307     C* SE IMMESSA LNP F.V. STAMPO IL TRAINO SOLO SE TROVO UN RITARDO
075600960307     C*  SU UNA FERMATA ARRIVO PRESENTE NEI MCUBI CON LNP = LNP IMMESSA
075700070517     c                   clear                   win04
075800070517     c                   clear                   win11
075900960307    3C  N31D36LNP        IFGT      0
076000960307     C     TSRDKA        ANDNE     *ALL'9'
076100960307     C                   EXSR      REDTSM
076200960307     C     W001A         IFNE      ' '
076300070517     C****               SETON                                        04
076400070517     c                   eval      win04='1'
076500960307     C                   ENDIF
076600960307    3C                   ENDIF
076700070517     c* se immessa filiale fermata arrivo stampo il traino solo se
076800070517     c* la filiale arrivo letta coincide con quella richiesta
076900070517     c  n31              if        d36ffa>0 and tsrdka<>*all'9'
077000070517     c                             and d36ffa=tsrffa
077100070517     c****               seton                                        11
077200070517     c                   eval      win11='1'
077300070517     c                   endif
077400070517     c* se immessa sia lnp che lna verifico che entrambe le condizioni sia-
077500070517     c* no soddisfatte
077600070517     c  n31              if        d36lnp>0 and d36ffa>0
077700070517     c                   if        tsrdka<>*all'9' and win04=win11
077800070518     c                             and win04='1'
077900070517     c                   eval      *in04=win04
078000070517     c                   eval      *in11=win11
078100070517     c                   endif
078200070517     c                   else
078300070518     c                   if        win04='1'
078400070517     c                   eval      *in04=win04
078500070518     c                   endif
078600070518     c                   if        win11='1'
078700070517     c                   eval      *in11=win11
078800070518     c                   endif
078900070517     c                   endif
079000960307     C*
079100960307   X2C                   ELSE
079200960307     C****  DATA CONTROLLO   P  A  R  T  E  N  Z  A  ****
079300960307     C*
079400960307     C* SE TROVO UNA DATA CONTROLLO UGUALE A ZERO O MAGGIORE DI QUELLA
079500960307     C*   RICHIESTA A VIDEO --> NON ELABORO E LEGGO IL TRAINO SUCESSIVO
079600960307    3C     TSRDKP        IFNE      *ALL'9'
079700960307     C                   SETOFF                                       29
079800960307     C     TSRDKP        IFGT      D36DKA
079900960307     C     TSRDKP        OREQ      0
080000960308     C                   MOVEL     'N'           WELAB
080100960307     C                   SETON                                        31
080200960307     C                   ENDIF
080300960307    3C                   ENDIF
080400960307     C*
080500070517     c                   clear                   win04
080600070517     c                   clear                   win11
080700960307     C* SE IMMESSA LNP F.V. STAMPO IL TRAINO SOLO SE TROVO UN RITARDO
080800960307     C*   SU UNA FERMATA PARTENZA UGUALE A QUELLA IMMESSA
080900960307    3C  N31D36LNP        IFGT      0
081000960307    4C     TSRFFP        IFEQ      D36LNP
081100960307     C     TSRDKP        IFNE      *ALL'9'
081200070517     C****               SETON                                        04
081300070517     c                   eval      win04='1'
081400960307     C                   ENDIF
081500960307     C*
081600960307   X4C                   ELSE
081700960307     C     *IN04         IFEQ      *OFF
081800960307     C     TSRFFP        ANDGT     D36LNP
081900960308     C                   MOVEL     'N'           WELAB
082000960307     C                   SETON                                        31
082100960307     C                   ENDIF
082200960307    4C                   ENDIF
082300960307    3C                   ENDIF
082400070517     c* Se immessa linea di arrivo stampo solo se trovo un ritardo
082500070517     c* su una fermata partenza presente nei mcubi con lna=lna immessa
082600070518     c*
082700070518     c* Se d36ffa>0 non mi trovo più in questo giro dei ritardi partenza
082800070518     c* in quanto all'ufficio traini non interessa che vengano stampati
082900070518     c* i ritardi partenza se hanno richiesto una specifica filiale fermata
083000070518     c* arrivo. Siccome la modifica l'avevo già fatta la lascio lo stesso.
083100070517    3C  N31D36ffa        IFGT      0
083200070517     C     TSRDKP        ANDNE     *ALL'9'
083300070517     C                   EXSR      REDTSM
083400070517     C     W001A         IFNE      ' '
083500070517     c                   eval      win11='1'
083600070517     C                   ENDIF
083700070517    3C                   ENDIF
083800070517     c* se immessa sia lnp che lna verifico che entrambe le condizioni sia-
083900070517     c* no soddisfatte
084000070517     c  n31              if        d36lnp>0 and d36ffa>0
084100070517     c                   if        tsrdkp<>*all'9' and win04=win11
084200070518     c                             and win04='1'
084300070517     c                   eval      *in04=win04
084400070517     c                   eval      *in11=win11
084500070517     c                   endif
084600070517     c                   else
084700070518     c                   if        win04='1'
084800070517     c                   eval      *in04=win04
084900070518     c                   endif
085000070518     c                   if        win11='1'
085100070517     c                   eval      *in11=win11
085200070518     c                   endif
085300070517     c                   endif
085400960307     C*
085500960307    2C                   ENDIF
085600960307     C*
085700960307     C*
085800960307    2C     *IN31         IFEQ      *OFF
085900960307     C*
086000960307     C* VERIFICO SE IL TRAINO HA UNA SOLA FERMATA DI CARICO
086100960307     C     WFFP          IFNE      TSRFFP
086200960307     C                   ADD       1             WCONT             3 0
086300960307     C                   MOVEL     TSRFFP        WFFP
086400960307     C                   ENDIF
086500960307     C*
086600960307     C* CARICO LE FERMATE NELLA SCHIERA FER
086700960307     C                   MOVEL     TSRNFP        FP
086800960307     C                   MOVEL     TSRNFA        FA
086900960307     C*
087000960307     C* FILIALE FERMATA PARTENZA
087100960307     C     TSRFFP        LOOKUP    FER                                    35
087200960307     C  N35              MOVEA     TSRFFP        FER(FP)
087300960307     C*
087400960307     C* FILIALE FERMATA ARRIVO
087500960307     C     TSRFFA        LOOKUP    FER                                    35
087600960307     C  N35              MOVEA     TSRFFA        FER(FA)
087700960307     C*
087800960307     C     KTSR          READE     TNTSR03L                               31
087900960307    2C                   ENDIF
088000960307    1C                   ENDDO
088100960307     C*
088200960307     C* SE NON E' STATO TROVATO NEMMENO UN RITARDO PER LA LNP F.V.
088300960307     C*   IMMESSA --> NON ELABORO E LEGGO IL TRAINO SUCESSIVO
088400960307    1C     D36LNP        IFGT      0
088500960307     C     *IN04         ANDEQ     *OFF
088600960307     C                   MOVEL     'N'           WELAB
088700960307    1C                   ENDIF
088800070517     C* SE NON E' STATO TROVATO NEMMENO UN RITARDO PER LA LNa
088900070517     C*   IMMESSA --> NON ELABORO E LEGGO IL TRAINO SUCESSIVO
089000070517    1C     D36ffa        IFGT      0
089100070517     C     *IN11         ANDEQ     *OFF
089200070517     C                   MOVEL     'N'           WELAB
089300070517    1C                   ENDIF
089400960307     C*
089500960307     C                   ENDSR
089600960307     C*
089700960307     C*--- LETTURA FILE METRI CUBI -----------------------------------*
089800960307     C     REDTSM        BEGSR
089900960307     C                   CLEAR                   W001A
090000960307     C*
090100960307     C     KTSR          SETLL     TNTSM01L
090200960307     C     KTSR          READE     TNTSM01L                               32
090300960307     C*
090400960307    1C     *IN32         DOWEQ     *OFF
090500960307     C* CONTROLLO SE ESISTE UNA FERMATA DI ARRIVO PRESENTE NEL FILE
090600960307     C*   DEI METRI CUBI CON LNP UGUALE A QUELLA RICHIESTA
090700070517     c                   if        wflg='A'
090800960307    2C     TSMATB        IFEQ      ' '
090900960307     C     TSRFFA        ANDEQ     TSMTFA
091000960307     C     TSMTFP        ANDEQ     D36LNP
091100960307     C                   MOVEL     '1'           W001A             1
091200960307     C                   SETON                                        32
091300960307    2C                   ENDIF
091400070517     c                   else
091500070517    2C     TSMATB        IFEQ      ' '
091600070517     C     TSRFFP        ANDEQ     TSMTFP
091700070517     C     TSMTFA        ANDEQ     D36FFA
091800070517     C                   MOVEL     '1'           W001A             1
091900070517     C                   SETON                                        32
092000070517    2C                   ENDIF
092100070517     c                   endif
092200960307     C*
092300960307     C  N32KTSR          READE     TNTSM01L                               32
092400960307    1C                   ENDDO
092500960307     C*
092600960307     C                   ENDSR
092700960307     C*
092800960307     C*--- LETTURA FERMATE CARICATE IN SKIERA ------------------------*
092900960307     C     REDFER        BEGSR
093000960307     C*
093100960307     C* MEMORIZZO SE IL TRAINO HA UNA SOLA FERMATA DI CARICO
093200960307    1C     WCONT         IFEQ      1
093300960307     C                   MOVEL     'S'           WFFC
093400960307    1C                   ENDIF
093500960307     C*
093600960307     C* AGGANCIO TNTST00F PER REPERIRE LA FILIALE PARTENZA DEL TRAINO
093700960307     C     KTSR          CHAIN     TNTST01L                           31
093800960307     C  N31              MOVEL     TSTTFP        WTFP
093900070516     c* se il trasionizta non è quello richiesto vado a fine routine
094000070516     c                   if        d36pdr>0 and tstpdr<>d36pdr
094100070516     c                   goto      f_redfer
094200070516     c                   endif
094300070517     c* se il n.traino non è quello richiesto vado a fine routine
094400070517     c                   if        d36trn>0 and tsttrn<>d36trn
094500070517     c                   goto      f_redfer
094600070517     c                   endif
094700960307     C*
094800960307     C                   Z-ADD     1             X
094900960307    1C                   DO        100           X
095000960307    2C     FER(X)        IFGT      0
095100960307     C*
095200960307     C     KTSR2         SETLL     TNTSR03L
095300960307     C     KTSR2         READE     TNTSR03L                               31
095400960307     C*
095500960307    3C     *IN31         DOWEQ     *OFF
095600960307     C* CREO FILE DI WORK
095700960307    4C     WFLG          IFEQ      'A'
095800960307     C                   EXSR      CRTTWK                                       *DK ARRIVO
095900960307   X4C                   ELSE
096000960307     C                   EXSR      CR2TWK                                       *DK PARTENZA
096100960307    4C                   ENDIF
096200960307     C*
096300960307     C     KTSR2         READE     TNTSR03L                               31
096400960307    3C                   ENDDO
096500960307     C*
096600960307    2C                   ENDIF
096700960307    1C                   ENDDO
096800960307     C*
096900070516     C     f_redfer      ENDSR
097000960307     C*
097100960226     C*--- CREO FILE DI WORK -----------------------------------------*
097200960226     C     CRTTWK        BEGSR
097300960226     C*
097400960228     C* FILIALE FERMATA PARTENZA
097500960228     C                   MOVEL     TSRNFP        WNFM
097600960228     C                   MOVEL     'P'           WEPA
097700960229     C     KTWK          CHAIN     TNTWK01L                           32
097800960228     C* SE GIA' ESISTENTE NON LO RISCRIVO
097900960228    1C     *IN32         IFEQ      *ON
098000960228     C                   CLEAR                   TNTWK000
098100960228     C                   MOVEL     TSRTRN        TWKTRN
098200960228     C                   MOVEL     TSRDTN        TWKDTN
098300960228     C                   MOVEL     WTFP          TWKTFP
098400960228     C                   MOVEL     'P'           TWKEPA
098500960228     C                   MOVEL     TSRNFP        TWKNFM
098600960228     C                   MOVEL     TSRFFP        TWKFIL
098700960228     C                   WRITE     TNTWK000
098800960228    1C                   ENDIF
098900960228     C*
099000960228     C* FILIALE FERMATA ARRIVO
099100960228     C                   MOVEL     TSRNFA        WNFM
099200960228     C                   MOVEL     'A'           WEPA
099300960229     C     KTWK          CHAIN     TNTWK01L                           32
099400960311     C*
099500960301     C   32              CLEAR                   TNTWK000
099600960301     C*
099700960301     C* TSRDKA = 99999999  --->  NON C'E' RITARDO
099800960301    1C     TSRDKA        IFEQ      *ALL'9'
099900060407     c
100000060407     c* Imposto i campi solo il write: in update solo se c'e' ritardo
100100060407     c                   if        *in32
100200960301     C                   CLEAR                   TWKDTE
100300960301     C                   CLEAR                   TWKHTE
100400960301     C                   CLEAR                   TWKDRE
100500960301     C                   CLEAR                   TWKHRE
100600060411     C                   MOVEL     TSRRTA        TWKRTC
100700060411     C                   MOVEL     TSRLNP        TWKLNP
100800060411     C                   MOVEL     TSRNFV        TWKNFV
100900060411     C                   MOVEL     TSRDFV        TWKDFV
101000060407     c                   endif
101100960301     C*
101200960301   X1C                   ELSE
101300960301     C*
101400960301     C* MANCA DATA ARRIVO REALE
101500960301    2C     TSRDRA        IFEQ      0
101600060407     c
101700060407     c* Imposto i campi solo il write: in update solo se c'e' ritardo
101800960301     C                   MOVEL     *ALL'9'       TWKDRE
101900960301     C                   MOVEL     *ALL'9'       TWKHRE
102000960301     C                   MOVEL     TSRDTA        TWKDTE
102100960301     C                   MOVEL     TSRHTA        TWKHTE
102200060407     c
102300960301   X2C                   ELSE
102400960301     C* PRESENTE RITARDO
102500960301     C                   MOVEL     TSRDRA        TWKDRE
102600960301     C                   MOVEL     TSRHRA        TWKHRE
102700960301     C                   MOVEL     TSRDTA        TWKDTE
102800960301     C                   MOVEL     TSRHTA        TWKHTE
102900960301    2C                   ENDIF
103000060411     C                   MOVEL     TSRRTA        TWKRTC
103100060411     C                   MOVEL     TSRLNP        TWKLNP
103200060411     C                   MOVEL     TSRNFV        TWKNFV
103300060411     C                   MOVEL     TSRDFV        TWKDFV
103400960301    1C                   ENDIF
103500960301     C*
103600960301     C*
103700960301     C     WFFC          IFEQ      'S'
103800960301     C                   MOVEL     'S'           TWKFFC
103900960301     C                   ELSE
104000960301     C                   CLEAR                   TWKFFC
104100960301     C                   ENDIF
104200960301     C*
104300960228    1C     *IN32         IFEQ      *ON
104400960228     C                   MOVEL     TSRTRN        TWKTRN
104500960228     C                   MOVEL     TSRDTN        TWKDTN
104600960228     C                   MOVEL     WTFP          TWKTFP
104700960228     C                   MOVEL     'A'           TWKEPA
104800960228     C                   MOVEL     TSRNFA        TWKNFM
104900960228     C                   MOVEL     TSRFFA        TWKFIL
105000960226     C                   WRITE     TNTWK000
105100960301     C*
105200960301   X1C                   ELSE
105300960301     C                   UPDATE    TNTWK000
105400960228    1C                   ENDIF
105500960226     C*
105600960226     C                   ENDSR
105700960228     C*
105800960228     C*--- CREO FILE DI WORK -----------------------------------------*
105900960228     C     CR2TWK        BEGSR
106000960228     C*
106100960228     C* FILIALE FERMATA ARRIVO
106200960228     C                   MOVEL     TSRNFA        WNFM
106300960228     C                   MOVEL     'A'           WEPA
106400960229     C     KTWK          CHAIN     TNTWK01L                           32
106500960228     C* SE GIA' ESISTENTE NON LO RISCRIVO
106600960228    1C     *IN32         IFEQ      *ON
106700960228     C                   CLEAR                   TNTWK000
106800960228     C                   MOVEL     TSRTRN        TWKTRN
106900960228     C                   MOVEL     TSRDTN        TWKDTN
107000960228     C                   MOVEL     WTFP          TWKTFP
107100960228     C                   MOVEL     'A'           TWKEPA
107200960228     C                   MOVEL     TSRNFA        TWKNFM
107300960228     C                   MOVEL     TSRFFA        TWKFIL
107400960228     C                   WRITE     TNTWK000
107500960228    1C                   ENDIF
107600960228     C*
107700960228     C* FILIALE FERMATA PARTENZA
107800960228     C                   MOVEL     TSRNFP        WNFM
107900960228     C                   MOVEL     'P'           WEPA
108000960229     C     KTWK          CHAIN     TNTWK01L                           32
108100960311     C*
108200960228     C   32              CLEAR                   TNTWK000
108300960228     C*
108400960307     C* TSRDKP = 99999999  --->  NON C'E' RITARDO
108500960228    1C     TSRDKP        IFEQ      *ALL'9'
108600060407     c                   if        *in32
108700960228     C                   CLEAR                   TWKDTE
108800960228     C                   CLEAR                   TWKHTE
108900960228     C                   CLEAR                   TWKDRE
109000960228     C                   CLEAR                   TWKHRE
109100060411     C*
109200060411     C                   MOVEL     TSRRTP        TWKRTC
109300060411     C                   MOVEL     TSRLNP        TWKLNP
109400060411     C                   MOVEL     TSRNFV        TWKNFV
109500060411     C                   MOVEL     TSRDFV        TWKDFV
109600060407     c                   endif
109700960228     C*
109800960228   X1C                   ELSE
109900960228     C*
110000960311    2C* MANCA DATA PARTENZA REALE
110100960228     C     TSRDRP        IFEQ      0
110200960228     C                   MOVEL     *ALL'9'       TWKDRE
110300960228     C                   MOVEL     *ALL'9'       TWKHRE
110400960228     C                   MOVEL     TSRDTP        TWKDTE
110500960228     C                   MOVEL     TSRHTP        TWKHTE
110600960228   X2C                   ELSE
110700960228     C* PRESENTE RITARDO
110800960228     C                   MOVEL     TSRDRP        TWKDRE
110900960228     C                   MOVEL     TSRHRP        TWKHRE
111000960228     C                   MOVEL     TSRDTP        TWKDTE
111100960228     C                   MOVEL     TSRHTP        TWKHTE
111200960228    2C                   ENDIF
111300060411     C*
111400060411     C                   MOVEL     TSRRTP        TWKRTC
111500060411     C                   MOVEL     TSRLNP        TWKLNP
111600060411     C                   MOVEL     TSRNFV        TWKNFV
111700060411     C                   MOVEL     TSRDFV        TWKDFV
111800960228    1C                   ENDIF
111900960229     C*
112000960229     C     WFFC          IFEQ      'S'
112100960229     C                   MOVEL     'S'           TWKFFC
112200960229     C                   ELSE
112300960229     C                   CLEAR                   TWKFFC
112400960229     C                   ENDIF
112500960228     C*
112600960228    1C     *IN32         IFEQ      *ON
112700960228     C                   MOVEL     TSRTRN        TWKTRN
112800960228     C                   MOVEL     TSRDTN        TWKDTN
112900960228     C                   MOVEL     WTFP          TWKTFP
113000960228     C                   MOVEL     'P'           TWKEPA
113100960228     C                   MOVEL     TSRNFP        TWKNFM
113200960228     C                   MOVEL     TSRFFP        TWKFIL
113300960228     C                   WRITE     TNTWK000
113400960228     C*
113500960228   X1C                   ELSE
113600960228     C                   UPDATE    TNTWK000
113700960228    1C                   ENDIF
113800960228     C*
113900960228     C                   ENDSR
114000960228     C*
114100960228     C*--- ROUTINE DI STAMPA -----------------------------------------*
114200960228     C     STAMPA        BEGSR
114300960229     C                   SETOFF                                       050917
114400960229     C                   SETOFF                                       101320
114500960301     C                   SETOFF                                       1215
114600960301     C                   SETOFF                                       1418
114700960302     C                   CLEAR                   WOK
114800960229     C*
114900960228     C* PULIZIA CAMPI NON SEMPRE PIENI
115000960229     C                   CLEAR                   PR1LNP
115100960229     C                   CLEAR                   PR1NFV
115200960229     C                   CLEAR                   PR1DFV
115300960229     C                   CLEAR                   PD1RTC
115400960229     C                   CLEAR                   PR1IMP
115500960229     C                   CLEAR                   PR1DTE
115600960229     C                   CLEAR                   PR1HTE
115700960229     C                   CLEAR                   PR1DRE
115800960229     C                   CLEAR                   PR1HRE
115900960229     C                   CLEAR                   PR1MIN
116000960301     C                   CLEAR                   PR1FRA
116100960302     C*
116200960302     C* 15 ON  - MANCA LA DATA PARTENZA/ARRIVO REALE
116300960302    1C     TWKDRE        IFEQ      *ALL'9'
116400960302     C                   SETON                                        15
116500960302     C*
116600960302   X1C                   ELSE
116700960302     C* DATA/ORA REALE
116800960302     C                   Z-ADD     TWKDRE        WDATA
116900960302     C                   Z-ADD     MM            GGMM
117000960302     C                   MOVEL     GG            GGMM
117100960302     C                   Z-ADD     AA            PR1DRE            6 0
117200960302     C                   MOVEL     GGMM          PR1DRE
117300960302     C                   Z-ADD     TWKHRE        PR1HRE
117400960302     C*
117500960302     C* SE E' PRESENTE RITARDO DICO QUANTI MINUTI SONO
117600960302     C                   CLEAR                   WXDTOR
117700960302     C                   MOVEL     TWKDRE        WXDT1
117800960302     C                   MOVEL     TWKHRE        WXHM1
117900960302     C                   MOVEL     TWKDTE        WXDT2
118000960302     C                   MOVEL     TWKHTE        WXHM2
118100960302     C                   MOVEL     'D'           WXFLG
118200960302     C                   CALL      'XDTORA'
118300960302     C                   PARM                    WXDTOR
118400960302     C* MINUTI RITARDO/ANTICIPO
118500960302     C                   Z-ADD     WXMIN         PR1MIN            4 0
118600960302    2C     WXMIN         IFGT      9999
118700960302     C                   Z-ADD     9999          PR1MIN
118800960302    2C                   ENDIF
118900960302     C* R=RITARDO / A=ANTICIPO
119000960302    2C     WXFLG         IFEQ      '+'
119100970328     C     TWKRTC        ORNE      *BLANKS
119200960302     C                   MOVEL     'R'           PR1FRA            1            *RITARDO
119300960302   X2C                   ELSE
119400960302     C     WXFLG         IFEQ      '-'
119500960302     C                   MOVEL     'A'           PR1FRA                         *ANTICIPO
119600960302     C                   ENDIF
119700960302    2C                   ENDIF
119800960302    1C                   ENDIF
119900960228     C*
120000960302     C* CONTROLLO RICHIESTA RITARDI/ANTICIPI/ENTRAMBI
120100960302    1C     D36FRA        IFEQ      'E'
120200960302     C                   MOVEL     '1'           WOK               1
120300960302   X1C                   ELSE
120400960302     C*
120500960302     C* RITARDI
120600960302    2C     D36FRA        IFEQ      'R'
120700960302    3C     *IN15         IFEQ      *ON
120800960302     C                   MOVEL     '1'           WOK
120900960302   X3C                   ELSE
121000960302    4C     PR1FRA        IFEQ      'R'
121100960302     C                   MOVEL     '1'           WOK
121200960302    4C                   ENDIF
121300960302    3C                   ENDIF
121400960302    2C                   ENDIF
121500960302     C*
121600960302     C* ANTICIPI
121700960302    2C     D36FRA        IFEQ      'A'
121800960302     C     *IN15         ANDEQ     *OFF
121900960302     C     PR1FRA        ANDEQ     'A'
122000960302     C                   MOVEL     '1'           WOK
122100960302    2C                   ENDIF
122200960302    1C                   ENDIF
122300960302     C*
122400960302     C* WOK = '1'  --->  R E C O R D    D A    S T A M P A R E
122500960302    0C     WOK           IFEQ      '1'
122600960312     C                   SETON                                        22
122700960302     C*
122800960228     C* SALVO TFP/TRN/DTN: SE CAMBIANO RICERCO IL TRAINO
122900960228    1C     TWKTFP        IFNE      SAVTFP
123000960228     C     TWKTRN        ORNE      SAVTRN
123100960228     C     TWKDTN        ORNE      SAVDTN
123200960228     C                   SETON                                        05
123300960228     C*
123400960228     C* AGGANCIO STORICO TRAINI
123500960228     C     KTST          CHAIN     TNTST01L                           31
123600960302     C*
123700960301     C* DATA TRAINO
123800960301     C                   MOVEL     TWKDTN        WDATA                          DATA TRAINO
123900960301     C                   MOVEL     GG            PR1DTN
124000960301     C                   MOVE      MM            PR1DTN            4 0
124100960228     C* NUMERO TRAINO
124200960228     C                   MOVEL     TWKTRN        PR1TRN
124300960228     C* FILIALE PARTENZA TRAINO
124400960228     C                   MOVEL     TWKTFP        PR1TFP
124500960228     C* FILIALE ARRIVO   TRAINO
124600960228     C                   MOVEL     TSTTFA        PR1TFA
124700960228     C* CATEGORIA TRAINO
124800960229    2C     TSTBIS        IFEQ      'B'
124900960228     C                   SETON                                        09
125000960229    2C                   ENDIF
125100960301    2C     TSTBIS        IFEQ      'E'
125200960301     C                   SETON                                        18
125300960301    2C                   ENDIF
125400960228     C* TIPO TRAINO
125500960228     C                   MOVEL     TSTTTR        PR1TTR
125600960301     C* TRAZIONISTA
125700960301     C                   MOVEL     TSTPDR        PR1PDR
125800000807     C     KTNTLZ        CHAIN     TNTLZ01L                           33
125900960301     C  N33              MOVEL     TLZRSC        PR1DPD           11
126000960304     C   33              CLEAR                   PR1DPD
126100960228     C* CARICO FERMATE TRAINO
126200960228     C                   EXSR      CARDES
126300960228     C*
126400960228     C                   MOVEL     TWKTFP        SAVTFP
126500960228     C                   MOVEL     TWKTRN        SAVTRN
126600960228     C                   MOVEL     TWKDTN        SAVDTN
126700960228    1C                   ENDIF
126800960228     C*
126900960229     C* CONTROLLO SE SI TRATTA DI RITARDO PARTENZA O ARRIVO
127000960229    1C     TWKEPA        IFEQ      'P'
127100960229     C                   MOVEL     'PART'        PR1FPA            4
127200960229     C                   SETON                                        17
127300960229     C                   CLEAR                   WLAI
127400960229   X1C                   ELSE
127500960229     C                   MOVEL     'ARR '        PR1FPA
127600960229     C                   MOVEL     TWKFIL        WLAI
127700960229    2C     TWKFFC        IFEQ      'S'
127800960229     C                   SETON                                        17
127900960229    2C                   ENDIF
128000960229    1C                   ENDIF
128100960229     C*
128200960301     C* DATI DEL FOGLIO VIAGGIO
128300960301     C                   MOVEL     TWKLNP        PR1LNP                         LNP FV
128400960301     C                   MOVEL     TWKNFV        PR1NFV                         NUMERO FV
128500960301     C                   MOVEL     TWKDFV        WDATA                          DATA FV
128600960301     C                   MOVEL     GG            PR1DFV
128700960301     C                   MOVE      MM            PR1DFV            4 0
128800960302     C*
128900960228     C* FILIALE SEGNALAZIONE
129000960228     C                   MOVEL     TWKFIL        PR1SEG
129100960228     C* DATA/ORA TEORICA
129200960228     C                   Z-ADD     TWKDTE        WDATA
129300960228     C                   Z-ADD     MM            GGMM              4 0
129400960228     C                   MOVEL     GG            GGMM
129500960229     C                   Z-ADD     AA            PR1DTE            6 0
129600960228     C                   MOVEL     GGMM          PR1DTE
129700960228     C                   Z-ADD     TWKHTE        PR1HTE
129800960229     C*
129900960228     C*
130000960228     C* CAUSALE RITARDO
130100960228     C                   MOVEL     TWKRTC        PR1RTC
130200960229     C*
130300960229   1AC     PR1RTC        IFNE      ' '
130400960229     C                   SETON                                        14
130500960307     C* DECODIFICA CAUSALE
130600960307     C                   EXSR      DECCAU
130700960229   1AC                   ENDIF
130800960228     C*
130900960312     C****  STAMPA TESTATA E DETTAGLIO  ****
131000960307     C                   EXSR      STARIG
131100960302    0C                   ENDIF
131200960229     C*
131300960228     C                   ENDSR
131400960228     C*
131500960228     C*--- LETTURA PER CARICO FERMATE TRAINO ------------------------*
131600960228     C     CARDES        BEGSR
131700960228     C                   Z-ADD     0             DS                3 0
131800960228     C                   CLEAR                   SK
131900960229     C*
132000960229     C* SALVO CAMPI KIAVE PER RICHAINARE IL RECORD CHE STO LEGGENDO
132100960229     C*   PERCHE' PERDO IL POSIZIONAMENTO A CAUSA DEL CARICAMENTO
132200960229     C*   DELLE FERMATE DEL TRAINO
132300960229     C                   MOVEL     TWKTFP        KSTFP
132400960229     C                   MOVEL     TWKDTN        KSDTN
132500960229     C                   MOVEL     TWKTRN        KSTRN
132600960229     C                   MOVEL     TWKNFM        KSNFM
132700960229     C                   MOVEL     TWKEPA        KSEPA
132800960228     C*
132900960312     C****  CARICO LE FERMATE DEL TRAINO  ****
133000960229     C     KTWK2         SETLL     TNTWK01L
133100960229     C     KTWK2         READE     TNTWK01L                               32
133200960228     C*
133300960229    1C     *IN32         DOWEQ     *OFF
133400960312     C                   MOVEL     TWKNFM        DS
133500960229     C*
133600960229     C     TWKFIL        CHAIN     AZORG01L                           33
133700960229     C*
133800960228     C                   MOVEL     ORGPRO        SK(DS)
133900960228     C                   MOVE      '/'           SK(DS)
134000960228     C*
134100960229     C     KTWK2         READE     TNTWK01L                               32
134200960229    1C                   ENDDO
134300960228     C*
134400960229     C                   MOVEA     SK            PR1DES           14
134500960229     C*
134600960312     C****  REPERISCO IL FV DA CUI PRENDERE IL CODUCENTE  ****
134700960302     C     KTWK3         CHAIN     TNTWK01L                           32
134800960302     C*
134900960302     C  N32KFVS          CHAIN     TNFVS01L                           32
135000960302     C  N32              MOVEL     FVSDCN        PR1DCN           10            CONDUCENTE
135100960302     C   32              CLEAR                   PR1DCN
135200960302     C*
135300960312     C****  RIPOSIZIONAMENTO SUL RECORD GIUSTO DI TNTWK00F  ****
135400960229     C     KSAV          CHAIN     TNTWK01L                           33
135500960312     C*
135600960312     C****  VERIFICO LE DESTINAZIONI  ****
135700960312     C                   Z-ADD     6             DS
135800960312    1C     SK(DS)        IFNE      ' '
135900960312     C                   SETON                                        10
136000960312    1C                   ENDIF
136100960312     C*
136200960228     C                   ENDSR
136300960228     C*
136400960307     C*--- DECODIFICA CAUSALE RITARDO -------------------------------*
136500960307     C     DECCAU        BEGSR
136600960307     C                   Z-ADD     1             X
136700960307     C*
136800960307     C     PR1RTC        LOOKUP    TTC(X)                                 35
136900960307     C   35              MOVEL     ITC(X)        PR1IMP            1
137000960307     C  N35              MOVEL     '?'           PR1IMP
137100960307     C*
137200960307    1C     PR1RTC        IFNE      'G'
137300960307     C   35              MOVEL     DTC(X)        PD1RTC
137400960307     C  N35              MOVEL     *ALL'?'       PD1RTC
137500960307     C*
137600960307   X1C                   ELSE
137700960307     C* 13 ON  - CAUSALE RITARDO = "G"
137800960307     C                   SETON                                        13
137900960307     C*
138000960307     C     KFV4          SETLL     FNFV401L
138100960307     C     KFV4          READE     FNFV401L                               32
138200960307    2C     *IN32         DOWEQ     *OFF
138300960307    3C     FV4ATB        IFEQ      ' '
138400960307     C                   MOVEL     FV4NOT        PD1RTC
138500960307     C                   SETON                                        32
138600960307    3C                   ENDIF
138700960307     C  N32KFV4          READE     FNFV401L                               32
138800960307    2C                   ENDDO
138900960307    1C                   ENDIF
139000960307     C                   ENDSR
139100960307     C*
139200960307     C*--- STAMPA TESTATA E DETTAGLIO -------------------------------*
139300960307     C     STARIG        BEGSR
139400960307     C*
139500960307     C****  STAMPO TESTATA  ****
139600960307    1C     *INOF         IFEQ      *ON
139700960307     C   16              EXCEPT    PIEDI
139800960307     C                   EXCEPT    TESTA2
139900960307     C                   SETOFF                                       OF
140000960307     C                   SETON                                        16
140100960307    1C                   ENDIF
140200960307     C*
140300960307     C****  STAMPO DETTAGLIO  ****
140400960307     C                   EXCEPT    DETT
140500960307     C*
140600960307     C****  FINISCO DI STAMPARE DESTINAZIONI E FNFV4  ****
140700960307     C   14              SETON                                        20
140800960307    1C     *IN10         DOWEQ     *ON
140900960307     C     *IN20         OREQ      *ON
141000960307     C   10              EXSR      REDDES
141100960307     C   20              EXSR      REDFV4
141200960307     C   10
141300960307     COR 20              EXSR      STAMP2
141400960307    1C                   ENDDO
141500960307     C*
141600960307     C                   EXCEPT    FITT
141700960307     C                   ENDSR
141800960307     C*
141900960307     C*--- LETTURA DELLE DESTINAZIONI ANCORA DA STAMPARE--------------*
142000960307     C     REDDES        BEGSR
142100960307    1C     SK(DS)        IFNE      '  '
142200960307     C                   CLEAR                   PR2DES
142300960307     C                   MOVEA     SK(DS)        PR2DES           14
142400960307     C                   ADD       5             DS
142500960307   X1C                   ELSE
142600960307     C                   SETOFF                                       10
142700960307    1C                   ENDIF
142800960307     C                   ENDSR
142900960307     C*
143000960307     C*--- LETTURA FILE FNFV401L ------------------------------------*
143100960307     C     REDFV4        BEGSR
143200960307     C  N13
143300960307     CANN12KFV4          SETLL     FNFV401L
143400960307     C  N12              SETON                                        12
143500960307     C*
143600960307     C     KFV4          READE     FNFV401L                               32
143700960307    1C     *IN32         DOWEQ     *OFF
143800960307     C     FV4ATB        ANDNE     ' '
143900960307     C     KFV4          READE     FNFV401L                               32
144000960307    1C                   ENDDO
144100960307     C*
144200960307     C  N32              SETON                                        20
144300960307     C  N32              MOVEL     FV4NOT        PR2NOT
144400960307     C   32              SETOFF                                       20
144500960307     C                   ENDSR
144600960307     C*
144700960307     C*--- STAMPA DESTINAZIONI E FNFV4 -------------------------------*
144800960307     C     STAMP2        BEGSR
144900960307     C*
145000960307    1C     *INOF         IFEQ      *ON
145100960307     C   16              EXCEPT    PIEDI
145200960307     C                   EXCEPT    TESTA2
145300960307     C                   EXCEPT    DETT
145400960307     C                   SETOFF                                       OF
145500960307    1C                   ENDIF
145600960307     C*
145700960307     C* 2° RIGA DI DETTAGLIO
145800960307     C                   EXCEPT    DETT2
145900960307     C                   ENDSR
146000960307     C*
146100960302     C*--- STAMPA FV NON ABBINATI A TRAINO ---------------------------*
146200960302     C     STAMP3        BEGSR
146300960302     C                   SETON                                        22
146400960302     C                   SETOFF                                       09
146500960302     C                   SETOFF                                       101320
146600960302     C                   SETOFF                                       1215
146700960302     C                   SETOFF                                       1418
146800960302     C*
146900960302     C* PULIZIA CAMPI NON SEMPRE PIENI
147000960302     C                   CLEAR                   PR1DTN
147100960302     C                   CLEAR                   PR1TRN
147200960302     C                   CLEAR                   PR1TFP
147300960302     C                   CLEAR                   PR1TFA
147400960302     C                   CLEAR                   PR1MIN
147500960302     C                   CLEAR                   PR1FRA
147600960302     C                   CLEAR                   PR1DTE
147700960302     C                   CLEAR                   PR1HTE
147800960302     C                   CLEAR                   PD1RTC
147900960302     C                   CLEAR                   PR1IMP
148000960302     C                   CLEAR                   PR1SEG
148100960302     C                   MOVEL     'PART'        PR1FPA
148200960302     C*
148300960302     C* CATEGORIA TRAINO
148400960302    1C     FVSBIS        IFEQ      'B'
148500960302     C                   SETON                                        09
148600960302    1C                   ENDIF
148700960302    1C     FVSBIS        IFEQ      'E'
148800960302     C                   SETON                                        18
148900960302    1C                   ENDIF
149000960302     C* TIPO TRAINO
149100960302     C                   MOVEL     FVSTTR        PR1TTR
149200960302     C* CARICO FILIALI DI SCARICO
149300960302     C                   EXSR      CARFIL
149400960302     C* TRAZIONISTA
149500960302     C                   MOVEL     FVSPDR        PR1PDR
149600960302     C                   MOVEL     FVSDPD        PR1DPD
149700960302     C* CONDUCENTE
149800960302     C                   MOVEL     FVSDCN        PR1DCN
149900960302     C* DATI DEL FOGLIO VIAGGIO
150000960302     C                   MOVEL     FVSLNP        PR1LNP                         LNP FV
150100960305     C                   MOVEL     FVSLNP        PR1SEG
150200960302     C                   MOVEL     FVSNFV        PR1NFV                         NUMERO FV
150300960302     C                   MOVEL     FVSDFV        WDATA                          DATA FV
150400960302     C                   MOVEL     GG            PR1DFV
150500960302     C                   MOVE      MM            PR1DFV
150600960302     C* DATA/ORA REALE PARTENZA
150700960302     C                   Z-ADD     FVSDRP        WDATA
150800960302     C                   Z-ADD     MM            GGMM
150900960302     C                   MOVEL     GG            GGMM
151000960302     C                   Z-ADD     AA            PR1DRE
151100960302     C                   MOVEL     GGMM          PR1DRE
151200960302     C                   Z-ADD     FVSHRP        PR1HRE
151300960302     C*
151400960302     C* CAUSALE RITARDO
151500960302     C                   MOVEL     FVSRTP        PR1RTC
151600960307    1C     PR1RTC        IFNE      ' '
151700960302     C                   SETON                                        14
151800960302     C* IMPOSTA CAMPI KIAVE PER ACCESSO FNFV4
151900960302     C                   MOVEL     FVSLNP        TWKLNP
152000960302     C                   MOVEL     FVSNFV        TWKNFV
152100960302     C                   MOVEL     'P'           TWKEPA
152200960302     C                   CLEAR                   WLAI
152300960307     C* DECODIFICA CAUSALE
152400960307     C                   EXSR      DECCAU
152500960307    1C                   ENDIF
152600960302     C*
152700960307     C****  STAMPA TESTATA E DETTAGLIO  ****
152800960307     C                   EXSR      STARIG
152900960302     C*
153000960307     C****  STAMPA DI FNFV2  ****
153100960302     C                   EXSR      STAFV2
153200960302     C                   ENDSR
153300960302     C*
153400960302     C*--- LETTURA PER CARICO DESTINAZIONI FV -----------------------*
153500960302     C     CARFIL        BEGSR
153600960302     C                   Z-ADD     1             I                 3 0
153700960302     C                   Z-ADD     0             DS                3 0
153800960304     C                   CLEAR                   SK
153900960305     C                   CLEAR                   PLP
154000000504     C*
154100000504     C     KFWS          CHAIN     TNFWS01L                           39
154200000504     C     *IN39         IFEQ      *ON
154300000504     C     FWSATB        ORNE      ' '
154400000504     C                   CLEAR                   FWSFF3
154500000504     C                   CLEAR                   FWSFF4
154600000504     C                   CLEAR                   FWSFL3
154700000504     C                   CLEAR                   FWSFL4
154800000504     C                   ENDIF
154900960304     C*
155000960307    1C     FLP(I)        DOWNE     *BLANKS
155100960302     C     FLP(I)        LOOKUP    PLP                                    35
155200960302     C     *IN35         IFEQ      *OFF
155300960302     C                   MOVEL     FLP(I)        W0030             3 0
155400960302     C     W0030         CHAIN     AZORG01L                           32
155500960302     C                   ADD       1             DS
155600960302     C                   MOVEL     ORGPRO        SK(DS)
155700960302     C                   MOVE      '/'           SK(DS)
155800960302     C                   MOVEL     FLP(I)        PLP(DS)
155900960302     C                   ENDIF
156000960302     C*
156100960302     C                   ADD       1             I
156200960307    1C                   ENDDO
156300960302     C*
156400960302     C                   MOVEA     SK            PR1DES
156500960312     C*
156600960312     C****  VERIFICO LE DESTINAZIONI  ****
156700960312     C                   Z-ADD     6             DS
156800960312    1C     SK(DS)        IFNE      ' '
156900960312     C                   SETON                                        10
157000960312    1C                   ENDIF
157100960302     C                   ENDSR
157200960302     C*
157300960302     C*--- STAMPA DI FNFV2 ------------------------------------------*
157400960302     C     STAFV2        BEGSR
157500960304     C                   SETOFF                                       10
157600960302     C                   MOVEL     'ARR '        PR3FPA
157700960302     C*
157800960302     C     KFV2          SETLL     FNFV201L
157900960302     C     KFV2          READE     FNFV201L                               31
158000960307    1C     *IN31         DOWEQ     *OFF
158100960304     C*
158200960307    2C     FV2ATB        IFEQ      ' '
158300960302     C     FV2TDH        ANDEQ     'R'
158400960304     C                   SETOFF                                       142013
158500960304     C                   SETOFF                                       12
158600960307     C*
158700960304     C* PULIZIA CAMPI NON SEMPRE PIENI
158800960304     C                   CLEAR                   PR3SEG
158900960304     C                   CLEAR                   PR3DRE
159000960304     C                   CLEAR                   PR3HRE
159100960304     C                   CLEAR                   PR3RTC
159200960304     C                   CLEAR                   PD3RTC
159300960304     C                   CLEAR                   PR3IMP
159400960304     C*
159500960302     C* DATA/ORA REALE PARTENZA
159600960302     C                   Z-ADD     FV2DPA        WDATA
159700960302     C                   Z-ADD     MM            GGMM
159800960302     C                   MOVEL     GG            GGMM
159900960302     C                   Z-ADD     AA            PR3DRE
160000960302     C                   MOVEL     GGMM          PR3DRE
160100960302     C                   Z-ADD     FV2HPA        PR3HRE
160200960305     C* FILIALE SEGNALAZIONE
160300960305     C                   MOVEL     FV2LAI        PR3SEG
160400960302     C*
160500960302     C* CAUSALE RITARDO
160600960302     C                   MOVEL     FV2RTC        PR3RTC
160700960302     C*
160800960307    3C     PR3RTC        IFNE      ' '
160900960302     C                   SETON                                        14
161000960302     C* IMPOSTO CAMPI KIAVE PER ACCESSO FNFV4
161100960302     C                   MOVEL     FV2LNP        TWKLNP
161200960302     C                   MOVEL     FV2NFV        TWKNFV
161300960302     C                   MOVEL     FV2EPA        TWKEPA
161400960302     C                   MOVEL     FV2LAI        WLAI
161500960302     C*
161600960302     C                   Z-ADD     1             X
161700960302     C     PR3RTC        LOOKUP    TTC(X)                                 35
161800960302     C   35              MOVEL     ITC(X)        PR3IMP
161900960302     C  N35              MOVEL     '?'           PR3IMP
162000960302     C*
162100960307    4C     PR3RTC        IFNE      'G'
162200960302     C   35              MOVEL     DTC(X)        PD3RTC
162300960302     C  N35              MOVEL     *ALL'?'       PD3RTC
162400960302     C*
162500960307   X4C                   ELSE
162600960302     C* 13 ON  - CAUSALE RITARDO = "G"
162700960302     C                   SETON                                        13
162800960302     C*
162900960302     C     KFV4          SETLL     FNFV401L
163000960302     C     KFV4          READE     FNFV401L                               32
163100960307    5C     *IN32         DOWEQ     *OFF
163200960307     C     FV4ATB        IFEQ      ' '
163300960302     C                   MOVEL     FV4NOT        PD3RTC
163400960302     C                   SETON                                        32
163500960307     C                   ENDIF
163600960302     C  N32KFV4          READE     FNFV401L                               32
163700960307    5C                   ENDDO
163800960307    4C                   ENDIF
163900960307    3C                   ENDIF
164000960302     C*
164100960307     C****  STAMPO TESTATA  ****
164200960307    3C     *INOF         IFEQ      *ON
164300960302     C   16              EXCEPT    PIEDI
164400960302     C                   EXCEPT    TESTA2
164500960302     C                   SETOFF                                       OF
164600960307    3C                   ENDIF
164700960307     C*
164800960307     C****  STAMPO DETTAGLIO  ****
164900960302     C                   EXCEPT    DETFV2
165000960302     C*
165100960307     C****  FINISCO DI STAMPARE DESTINAZIONI E FNFV4  ****
165200960302     C   14              SETON                                        20
165300960307    3C     *IN20         DOWEQ     *ON
165400960302     C                   EXSR      REDFV4
165500960302     C*
165600960307    4C   20*INOF         IFEQ      *ON
165700960302     C   16              EXCEPT    PIEDI
165800960302     C                   EXCEPT    TESTA2
165900960302     C                   EXCEPT    DETFV2
166000960302     C                   SETOFF                                       OF
166100960307    4C                   ENDIF
166200960302     C*
166300960307     C* 2° RIGA DI DETTAGLIO
166400960302     C   20              EXCEPT    DETT2
166500960307    3C                   ENDDO
166600960302     C*
166700960302     C                   EXCEPT    FITT
166800960307    2C                   ENDIF
166900960307     C*
167000960302     C     KFV2          READE     FNFV201L                               31
167100960307    1C                   ENDDO
167200960302     C*
167300960302     C                   ENDSR
167400960302     C*
167500960226     C*--- SR INIZIALE -----------------------------------------------*
167600951110     C     *INZSR        BEGSR
167700960307     C****
167800960307     C** KLIST
167900960307     C****
168000000807     C* ACCESSO TNTLZ01L
168100000807     C     KTNTLZ        KLIST
168200000807     C                   KFLD                    KTLTIP            2
168300000807     C                   KFLD                    TSTPDR
168400000807     C                   MOVEL     'T '          KTLTIP
168500960307     C* ACCESSO TNTSR03L/TNTSM01L
168600960307     C     KTSR          KLIST
168700960307     C                   KFLD                    KEYTRN
168800960307     C                   KFLD                    KEYDTN
168900960307     C* ACCESSO TNTSR03L
169000960307     C     KTSR2         KLIST
169100960307     C                   KFLD                    KEYTRN
169200960307     C                   KFLD                    KEYDTN
169300960226     C                   KFLD                    FER(X)
169400960311     C     KTSR4         KLIST
169500960311     C                   KFLD                    TSTTRN
169600960311     C                   KFLD                    TSTDTN
169700960307     C* ACCESSO TNTSR09L/TNTSR08L
169800960307     C     KTSR3         KLIST
169900960311     C                   KFLD                    KEYDK
170000960307     C                   KFLD                    KEYFFP
170100960307     C                   KFLD                    KEYDTT
170200960307     C                   KFLD                    KEYDTN
170300960307     C                   KFLD                    KEYTRN
170400960229     C* ACCESSO TNTWK01L
170500960228     C     KTWK          KLIST
170600960228     C                   KFLD                    WTFP
170700960228     C                   KFLD                    TSRDTN
170800960228     C                   KFLD                    TSRTRN
170900960228     C                   KFLD                    WNFM
171000960228     C                   KFLD                    WEPA
171100960229     C     KTWK2         KLIST
171200960229     C                   KFLD                    TWKTFP
171300960229     C                   KFLD                    TWKDTN
171400960229     C                   KFLD                    TWKTRN
171500960302     C     KTWK3         KLIST
171600960302     C                   KFLD                    TWKTFP
171700960302     C                   KFLD                    TWKDTN
171800960302     C                   KFLD                    TWKTRN
171900960302     C                   KFLD                    TSTNFE
172000960229     C     KSAV          KLIST
172100960229     C                   KFLD                    KSTFP
172200960229     C                   KFLD                    KSDTN
172300960229     C                   KFLD                    KSTRN
172400960229     C                   KFLD                    KSNFM
172500960229     C                   KFLD                    KSEPA
172600960228     C* ACCESSO TNFVS01L
172700960228     C     KFVS          KLIST
172800960228     C                   KFLD                    TWKLNP
172900960228     C                   KFLD                    TWKNFV
173000960229     C* ACCESSO TNTST01L
173100960229     C     KTST          KLIST
173200960229     C                   KFLD                    TWKTRN
173300960229     C                   KFLD                    TWKDTN
173400960311     C     KTST3         KLIST
173500960311     C                   KFLD                    D36TRN
173600960311     C                   KFLD                    D36DTD
173700960311     C* ACCESSO TNTST04L
173800960311     C     KTST2         KLIST
173900960311     C                   KFLD                    D36TFP
174000960311     C                   KFLD                    D36DTD
174100960229     C* ACCESSO FNFV401L
174200960229     C     KFV4          KLIST
174300960229     C                   KFLD                    TWKLNP
174400960229     C                   KFLD                    TWKNFV
174500960229     C                   KFLD                    TWKEPA
174600960229     C                   KFLD                    WLAI
174700960229     C                   KFLD                    WTRC
174800960302     C* ACCESSO FNFV201L
174900960302     C     KFV2          KLIST
175000960302     C                   KFLD                    FVSLNP
175100960302     C                   KFLD                    FVSNFV
175200960302     C                   KFLD                    WEPA2
175300000504     C     KFWS          KLIST
175400000504     C                   KFLD                    FVSLNP
175500000504     C                   KFLD                    FVSNFV
175600951110     C* ACCESSO TABEL00F
175700960228     C     KTAB2         KLIST
175800951110     C                   KFLD                    CODUT
175900951110     C                   KFLD                    COD
176000951110     C*
176100960228     C****
176200960228     C** DEFINIZIONE CAMPI
176300960228     C****
176400960229     C     *LIKE         DEFINE    TBLCOD        COD
176500960307     C     *LIKE         DEFINE    §TCDES        PD1RTC
176600960307     C     *LIKE         DEFINE    D36DKD        PRTDAL
176700960307     C     *LIKE         DEFINE    D36DKA        PRTAL
176800960307     C*
176900960228     C     *LIKE         DEFINE    TWKEPA        WEPA
177000960228     C     *LIKE         DEFINE    TWKNFM        WNFM
177100960307     C     *LIKE         DEFINE    TWKFFC        WFFC
177200960307     C     *LIKE         DEFINE    TWKFIL        PR1SEG
177300960307     C     *LIKE         DEFINE    TWKHRE        PR1HRE
177400960307     C     *LIKE         DEFINE    TWKHTE        PR1HTE
177500960307     C     *LIKE         DEFINE    TWKTFP        SAVTFP
177600960307     C     *LIKE         DEFINE    TWKTRN        SAVTRN
177700960307     C     *LIKE         DEFINE    TWKDTN        SAVDTN
177800960307     C     *LIKE         DEFINE    TWKTFP        KSTFP
177900960307     C     *LIKE         DEFINE    TWKTRN        KSTRN
178000960307     C     *LIKE         DEFINE    TWKDTN        KSDTN
178100960307     C     *LIKE         DEFINE    TWKEPA        KSEPA
178200960307     C     *LIKE         DEFINE    TWKNFM        KSNFM
178300960307     C*
178400960307     C     *LIKE         DEFINE    FV2EPA        WEPA2
178500960229     C     *LIKE         DEFINE    FV4TRC        WTRC
178600960229     C     *LIKE         DEFINE    FV4LAI        WLAI
178700960229     C     *LIKE         DEFINE    FV4NOT        PR2NOT
178800960307     C*
178900960229     C     *LIKE         DEFINE    TSRNFP        FP
179000960229     C     *LIKE         DEFINE    TSRNFA        FA
179100960302     C     *LIKE         DEFINE    TSRFFP        WFFP
179200960229     C     *LIKE         DEFINE    TSRRTA        PR1RTC
179300960307     C*
179400960307     C     *LIKE         DEFINE    TSTTFP        WTFP
179500960229     C     *LIKE         DEFINE    TSTTRN        PR1TRN
179600960229     C     *LIKE         DEFINE    TSTTFP        PR1TFP
179700960229     C     *LIKE         DEFINE    TSTTFA        PR1TFA
179800960229     C     *LIKE         DEFINE    TSTTTR        PR1TTR
179900960301     C     *LIKE         DEFINE    TSTPDR        PR1PDR
180000960307     C*
180100960229     C     *LIKE         DEFINE    FVSLNP        PR1LNP
180200960229     C     *LIKE         DEFINE    FVSNFV        PR1NFV
180300960307     C*
180400960302     C     *LIKE         DEFINE    PR1SEG        PR3SEG
180500960302     C     *LIKE         DEFINE    PR1FPA        PR3FPA
180600960302     C     *LIKE         DEFINE    PR1DRE        PR3DRE
180700960302     C     *LIKE         DEFINE    PR1HRE        PR3HRE
180800960302     C     *LIKE         DEFINE    PR1RTC        PR3RTC
180900960302     C     *LIKE         DEFINE    PD1RTC        PD3RTC
181000960302     C     *LIKE         DEFINE    PR1IMP        PR3IMP
181100960229     C*
181200960311     C     *LIKE         DEFINE    TS9DKA        KEYDK
181300960307     C     *LIKE         DEFINE    TS9FFP        KEYFFP
181400960307     C     *LIKE         DEFINE    TS9DTT        KEYDTT
181500960307     C     *LIKE         DEFINE    TS9DTN        KEYDTN
181600960307     C     *LIKE         DEFINE    TS9TRN        KEYTRN
181700960307     C*
181800960307     C****  IMPOSTAZIONE CAMPI FISSI  ****
181900960307     C                   MOVEL     'R'           WTRC                           *R=RITARDO
182000960307     C                   MOVEL     'A'           WEPA2                          *A=ARRIVO
182100960307     C                   SETON                                        OF
182200951110     C*
182300960229     C                   Z-ADD     1             CODUT
182400960229     C                   CALL      'X§PARUT'
182500960229     C                   PARM                    UT§DSE
182600960229     C                   MOVEL     RAGUT         RSUT             20
182700960307     C*
182800960307     C****  PRENDO DATA E ORA CORRENTE  ****
182900960229     C                   TIME                    W0140            14 0
183000960229     C                   MOVE      W0140         UDATE8            8 0
183100960229     C                   MOVEL     W0140         UTIME             6 0
183200960228     C****
183300960228     C** CARICO TABELLA DELLE CAUSALI RITARDO
183400960228     C****
183500960228     C                   MOVEL     'TC'          COD
183600960228     C                   Z-ADD     1             X                 3 0
183700960228     C     KTAB2         SETLL     TABEL
183800960228     C     KTAB2         READE     TABEL                                  30
183900960228    1C     *IN30         DOWEQ     *OFF
184000960228    2C     TBLFLG        IFEQ      ' '
184100960228     C                   MOVEL     TBLKEY        TTC(X)
184200960228     C                   MOVEL     TBLUNI        DSTC
184300960228     C                   MOVEL     §TCDES        DTC(X)
184400960228     C                   MOVEL     §TCIMP        ITC(X)
184500960228     C                   ADD       1             X
184600960228    2C                   ENDIF
184700960228     C     KTAB2         READE     TABEL                                  30
184800960228    1C                   ENDDO
184900960228     C*
185000960229     C                   ENDSR
185100960228     O*--------------------------------------------------------------*
185200960228     O*     TESTATA                                                  *
185300960228     O*--------------------------------------------------------------*
185400960228     OPRTF198   E            TESTA2           02
185500960228     O                       TS(1)               66
185600960228     O                       TS(2)              132
185700960228     O                       TS(3)              198
185800960228     O                       RSUT                20
185900960228     O                       UDATE8             180 '  /  /    '
186000960228     O                       PAGE          Z    198
186100960228     O          E            TESTA2           03
186200960228     O                       KNSIF               10
186300960228     O                       KNMUS               24
186400960228     O                       UTIME              178 '  :  :  '
186500960228     O          E            TESTA2           05
186600960229     O                       TS(17)             132
186700960304     O                     23                    20 'ELENCO F.VIAGGIO NON'
186800960304     O                     23                  +  1 'ABBINATI'
186900070518     O                       PRTFRA              63
187000070518     O                     06                    81 'TRAINO   '
187100070518     O                       PRTDAL              99 '  /  /    '
187200070518     O                       PRTAL              115 '  /  /    '
187300070518     O                     06PRTTRA             141
187400070518     O                  02 06D36TFP             145
187500070518     O                 N02 06D36TRN        Z    149
187600070518     O                    N06PRTLNP             144
187700070518     O                    N06D36LNP        Z    148
187800070521     o                     03                   162 'TRAZIONISTA:'
187900070521     o                     03d36pdr        z    169
188000070521     o                     19                   179 'FIL.ARR:'
188100070521     o                     19d36ffa        z    182
188200070521     o                     21                   191 'TRAINO:'
188300070521     o                     21d36trn        Z    198
188400960228     O*
188500960228     O          E            TESTA2           07
188600960228     O                       TS(5)               66
188700960228     O                       TS(9)              132
188800960228     O                       TS(13)             198
188900961022     O                     23                    43 'P.O.DI SCARICO'
189000960228     O          E            TESTA2           08
189100960228     O                       TS(6)               66
189200960228     O                       TS(10)             132
189300960228     O                       TS(14)             198
189400960302     O                     23                    98 '    '
189500960228     O          E            TESTA2           09
189600960228     O                       TS(7)               66
189700960228     O                       TS(11)             132
189800960228     O                       TS(15)             198
189900960302     O                     23                    98 '    '
190000960228     O          E            TESTA2           10
190100960228     O                       TS(21)              66
190200960228     O                       TS(21)             132
190300960228     O                       TS(21)             198
190400960228     O*
190500960228     O*--------------------------------------------------------------*
190600960228     O*     DETTAGLIO                                                *
190700960228     O*--------------------------------------------------------------*
190800960228     O          E            DETT        1
190900960228     O                       TS(18)              66
191000960228     O                       TS(19)             132
191100960228     O                       TS(20)             198
191200960301     O* DATA   TRAINO
191300960301     O                  05   PR1DTN               5 '  /  '
191400960228     O* NUMERO TRAINO
191500960301     O                  05   PR1TRN        Z     13
191600960228     O* PARTENZA/ARRIVO  TRAINO
191700960301     O                  05   PR1TFP        Z     17
191800960301     O                  05                       18 '/'
191900960301     O                  05   PR1TFA        Z     21
192000960228     O* TIPO TRAINO
192100960301     O                  05 09                    25 'BIS'
192200960301     O                  05 18                    25 'ECC'
192300960301     O                  05   PR1TTR              27
192400960228     O* DESTINAZIONI
192500960301     O                  05   PR1DES              43
192600960228     O* TRAZIONISTA
192700960301     O                  05   PR1PDR        Z     51
192800960301     O                  05                       52 '-'
192900960301     O                  05   PR1DPD              63
193000960228     O* CONDUCENTE
193100960301     O                  05   PR1DCN              75
193200960228     O* LNP / NUMERO / DATA  FOGLIO VIAGGIO
193300960301     O                     17PR1LNP        Z     80
193400960301     O                     17PR1NFV        Z     86
193500960301     O                     17PR1DFV              92 '  /  '
193600960228     O* FILIALE CHE SEGNALA
193700960301     O                       PR1SEG        Z     97
193800960228     O* PARTENZA/ARRIVO
193900960301     O                       PR1FPA             104
194000960229     O* PARTENZA/ARRIVO TEORICO
194100960301     O                       PR1DTE             113 '  /  /  '
194200960304     O                       PR1HTE             119 '  :  '
194300960229     O* PARTENZA/ARRIVO REALE
194400960301     O                       PR1DRE             128 '  /  /  '
194500960304     O                       PR1HRE             134 '  :  '
194600960228     O* MINUTI RITARDO
194700960301     O                       PR1MIN        Z    141
194800960301     O* R=RITARDO / A=ANTICIPO
194900960301     O                       PR1FRA             143
195000960229     O* CAUSALE RITARDO
195100960301     O                     14PR1RTC             146
195200960301     O                     14                   147 '-'
195300960301     O                     14PD1RTC             182
195400960228     O* IMPUTAZIONE
195500960301     O                       PR1IMP             185
195600960229     O* MESSAGGIO PER AVVISARE CHE MANCA LA DATA REALE PARTENZA/ARRIVO
195700960301     O                     15                   198 'MANCA DATA'
195800960228     O*--------------------------------------------------------------*
195900960228     O*     DETTAGLIO   DI   FNFV4                                   *
196000960228     O*--------------------------------------------------------------*
196100960301     O          E            DETT2       1
196200960229     O                       TS(18)              66
196300960229     O                       TS(19)             132
196400960229     O                       TS(20)             198
196500960228     O* DESTINAZIONI
196600960312     O               10      PR2DES              43
196700960229     O* DESCRIZIONI RITARDO
196800960301     O               20      PR2NOT             182
196900960228     O*
197000960302     O*--------------------------------------------------------------*
197100960302     O*     DETTAGLIO FNFV2                                          *
197200960302     O*--------------------------------------------------------------*
197300960302     O          E            DETFV2      1
197400960304     O                       TS(18)              66
197500960304     O                       TS(19)             132
197600960304     O                       TS(20)             198
197700960302     O* FILIALE CHE SEGNALA
197800960302     O                       PR3SEG        Z     97
197900960302     O* PARTENZA/ARRIVO
198000960302     O                       PR3FPA             104
198100960302     O* PARTENZA/ARRIVO REALE
198200960302     O                       PR3DRE             128 '  /  /  '
198300960304     O                       PR3HRE             134 '  :  '
198400960302     O* CAUSALE RITARDO
198500960302     O                     14PR3RTC             146
198600960302     O                     14                   147 '-'
198700960302     O                     14PD3RTC             182
198800960302     O* IMPUTAZIONE
198900960302     O                       PR3IMP             185
199000960302     O*
199100960307     O*--------------------------------------------------------------*
199200960307     O****  FITTIZZIO PER IL SALTO GIUSTO DELLE RIGHE  ****
199300960301     O          E            FITT        1
199400960301     O                                              ' '
199500960307     O****  FINE STAMPA  ****
199600960228     O          E            FINEST      2
199700960229     O                       TS(24)             132
199800960307     O****  FINE STAMPA 2  ****
199900960305     O          E            FINES2           66
200000960305     O                       TS(24)             132
200100960307     O****  PIEDI  ****
200200960228     O          E            PIEDI            65
200300960229     O                       TS(22)              66
200400960229     O                       TS(23)             132
200500960228     O*--------------------------------------------------------------*
200600960229**         CMD1
200700960223CLRPFM FILE(TNTWK00F)
200800960228**         MSG
200900960229SNDMSG MSG('ATTENZIONE!! FILE TNTWK00F GIA'' UTILIZZATO DA UN ALTRO
201000960328 LAVORO !!')
201100960312                                                    TOUSR(*SYSOPR)
201200960228**         TS
201300960305                                                                     1
201400960304     **  ELENCO  TRAZIONI  IN  RITARDO  **                           2
201500960228                   TNTL36R                               PAG. XXXX   3
201600960228                                                                     4
201700960301  T  R  A  I  N  O   |   TP  FERMATE TRAINO   TRAZIONISTA        C   5
201800960301DATA  NUMERO  LNP/LNA|   TR                                          6
201900960301                     |                                               7
202000960301XX/XX XXXXXXX XXX/XXX BIS X  XX/XX/XX/XX/XX XXXXXXX-XXXXXXXXXXX  X   8
202100961022ONDUCENTE  F.V I A G G I O |P.O.|       T E O R I C O    R E A L E   9
202200960301           LNP NUMERO DATA |SEGN|                                   10
202300960301                           |ALAZ|                                   11
202400960301XXXXXXXXX  XXX XXXXX XX/XX  XXX   XXXX XX/XX/XX XX:XX XX/XX/XX XX:  12
202500960301   |            R   I   T   A   R   D   O         IMPU|             13
202600960301   |  MIN  |            C A U S A L E             |TAZ|             14
202700960301   |  UTI  |                                      |   |             15
202800960301XX  XXXXX X  X-XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  X   MANCA DATA  16
202900070518 DATA CONTROLLO :  DAL XX/XX/XXXX   AL XX/XX/XXXX               17
203000960301                     |                                              18
203100960301                           |    |                                   19
203200960301   |       |                                      |   |             20
203300960229------------------------------------------------------------------  21
203400961022IMPUTAZIONE:   A=AUTISTA    F=FORZA MAGGIORE    P=P.O.PARTENZA      22
203500961022T=P.O.TRANSITO    S=SEDE                                            23
203600960229           * * *     F I N E      S T A M P A    * * *              24
