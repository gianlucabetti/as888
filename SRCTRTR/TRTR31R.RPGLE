000100910926     H DECEDIT('0,') DATEDIT(*DMY.)
000200990519     H* TRTR31R *----------------------------------------------------*
000300920217     H*  STAMPA VARIAZIONI NON EFFETTUATE                            *
000400000000     H*--------------------------------------------------------------*
000500991025     FFIARBE1C  UF   E           K DISK    USROPN
000600991025     FFIARBE2C  IF   E           K DISK    USROPN
000700950220     F                                     RENAME(FNARBD00:FNARD2)
000800950220     F                                     RENAME(FNARBK00:FNARK2)
000900991025     F                                     RENAME(FIARBT00:FIART2)
001000950220     F                                     RENAME(FNARBM00:FNARM2)
001100950220     F                                     RENAME(FNARBV00:FNARV2)
001200950220     F                                     RENAME(FNARBG00:FNARG2)
001300990916     F                                     RENAME(FNARBP00:FNARP2)
001400991025     FFIARBU1E  IF   E           K DISK
001500920218     FAZORG01L  IF   E           K DISK
001600941118     FTNCSV01L  IF   E           K DISK
001700920218     FTABEL00F  IF   E           K DISK
001800990622     FTITAS30C  IF   E           K DISK
001900990623     FTITAA30C  IF   E           K DISK
002000990622     FTITA730C  IF   E           K DISK
002100941205     FTNTAM01L  IF   E           K DISK
002200990719     FTITPT01L  IF   E           K DISK
002300990719     FTITPD01L  IF   E           K DISK
002400950222     FPRTF198   O    F  198        PRINTER OFLIND(*INOF)
002500950222     F                                     FORMLEN(66)
002600950222     F                                     FORMOFL(58)
002601131017     f                                     usropn
002700920113     D*
002800950224     D TES             S             66    DIM(10) CTDATA PERRCD(1)             X TESTATA     A
002900990429     D TAS             S             66    DIM(26) CTDATA PERRCD(1)             X TASSAZIONE  A
003000991028     D DES             S             66    DIM(32) CTDATA PERRCD(1)             X DESTINATARIOA
003100920219     D CBL             S             66    DIM(14) CTDATA PERRCD(1)             X CODICE BOLLAA
003200920220     D ERR             S             35    DIM(4) CTDATA PERRCD(1)              DES ERRORI    A
003300990426     D STA             S             25    DIM(3) CTDATA PERRCD(1)              STAMPE X SEDE A
003400941118     D*
003500991025     D VAR             S             11  3 DIM(20)                                    VARIE TNT
003600950210     D SVC             S              1    DIM(20)                              SIGLA VARIE ARB
003700991025     D SEA             S             11  3 DIM(20)                                    VARIE ARB
003800991026     D SVP             S             11    DIM(20)                              VARIE IN STAMPA
003900991026     D SVS             S             11    DIM(20)                              VARIE IN STAMPA
004000950217     D MIT             S             66    DIM(14) CTDATA PERRCD(1)             X MITTENTE    A
004100991202     D CPT             S             66    DIM(15) CTDATA PERRCD(1)             X CONS.PART.  A
004200061129     D CDF             S             66    DIM(4) CTDATA PERRCD(1)              X CONS.PART.  A
004201140401     D MSG             S             35    DIM(10) CTDATA PERRCD(1)             COSTANTI ERRORA
004300991026     d*
004400991026     D Wtstdec         S                   LIKE(§CVfdc)
004500991026     D Wfdcarb         S                   LIKE(§CVfdc)
004600991026     D Wfdctas         S                   LIKE(§CVfdc)
004700991027     D prtqft          S             13
004800991027     D prtpor          S             11
004900991027     D prfqft          S             13
005000991027     D prfpor          S             11
005100991029     D W014A           s             14
005200991026     D Wedtcde         s              1
005300991026     D IMPeditato      s             13
005400991029     D w0140           s             14  0
005500991026     D w0133           s             13  3
005600991026     d*
005700990519     D DSBS69        E DS                  EXTNAME(TIBS69DS)
005800990519     D DSACO         E DS                  EXTNAME(CNACO00F)
005900990519     D DSIND         E DS                  EXTNAME(CNIND00F)
006000990519     D DSCLP         E DS                  EXTNAME(CNCLP00F)
006100990519     D DSCLS         E DS                  EXTNAME(FNCLS00F)
006200991026     D DSCV          E DS
006300991027     D DS$2          E DS
006400920218     D PARAM           DS
006500950209     D  VIDDAL                 1      8  0
006600950209     D  VIDAL                  9     16  0
006700950209     D  COMDAL                17     24  0
006800950209     D  COMAL                 25     32  0
006900950209     D  VIDFEV                33     35  0
007000950209     D  PARAAS                36     39  0
007100950209     D  PARLNP                40     42  0
007200950209     D  PARNRS                43     44  0
007300950209     D  PARNSP                45     51  0
007400950220     D  NUMSPE                36     51  0
007401131017     D  nomeprt              101    110
007500941205     D                 DS
007600941205     D  TASAAS                 1      4  0
007700941205     D  TASMGS                 5      8  0
007800941205     D  TASDSP                 1      8  0
007900941118     D***
008000941118     D* PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
008100941118     D***
008200941118     D WLBDAT          DS                  INZ
008300941118     D  G02DAT                 1      8  0
008400941118     D  G02INV                 9     16  0
008500941118     D  G02ERR                17     17
008600941118     D  G02TGI                18     22  0
008700950210     D                 DS
008800950210     D  GG                     1      2  0
008900950210     D  AA                     5      6  0
009000950210     D  DATA                   1      6  0
009100920113     D KPJBA         E DS
009200920113     D DS3A          E DS
009300941205     D DS1P          E DS
009400000710     D OG143         E DS
009500920113     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
009600950214     D  TCU                  398    697
009700950214     D                                     DIM(50)                              TAB FLAG CNT
009800950217     D  KCU                  698    847P 0
009900950217     D                                     DIM(50)                              TAB CAPO CONTO
010000950217     D                                     PACKEVEN
010100950214     D TCUDS           DS
010200950214     D  F1                     1      1
010300950214     D  F3                     3      3
010400950214     D  F2                     2      2
010500950214     D  F4                     4      4
010600950214     D  F56                    5      6
010700950217     D                 DS
010800991029     D  SVRDS                  1     20
010900991029     D  SVRDS1                 5     20
011000950217     D  SVR                    1     20
011100950210     D                                     DIM(20)                              SIGLA VARIE TNT
011200950217     D                 DS
011300991029     D  SVEDS                  1     20
011400991029     D  SVEDS1                 5     20
011500950217     D  SVE                    1     20
011600950210     D                                     DIM(20)                              SIGLA VARIE ARB
011700960930     D*
011800960930     D* DEFINIZIONE COSTANTI
011900960930     D*
012000960930     D COS102          C                   CONST('Attenzione:La lna co-
012100060110     D                                     ntiene il TermPart -
012200060110     D                                     del P.O. che ha fatto la-
012300960930     D                                      variaz.')
012400061129     Ddecomitdes       s             13
012401131017      // - Campi per QCMDEXC
012402131017     d Qcmd            s            512    inz
012500061129     d
012501131017      // - Esecuzione comando di sistema
012502131017     d qCmdExc         pr                  extpgm('QCMDEXC')
012503131017     d  Qcmd                        512    const  options(*varsize)
012504131017     d  Qlen                         15  5 const
012600950209     IFNARBD00      11
012700950209     IFNARBK00      12
012800991025     IFIARBT00      13
012900950209     IFNARBM00      15
013000950209     IFNARBV00      16
013100950209     IFNARBG00      17
013200991028     IFNARBP00      23
013300950227     IFNARD2        11
013400950227     IFNARK2        12
013500991025     IFIART2        13
013600950227     IFNARM2        15
013700950227     IFNARV2        16
013800950227     IFNARG2        17
013900991028     IFNARP2        23
014000941118     I*
014100990622     ITITAS000      19
014200990622     ITITAS010      20
014300990622     ITITASP00      21
014400920113     I/SPACE 3
014500920113     C****************************************************************
014600920113     C*  RIEPILOGO INDICATORI
014700920113     C***************************************************************
014800930513     C* 01    - SE NON VARIO PESO O VOLUME O FTC CANCELLO VARIAZ
014900930513     C*         SENZA STAMPARE
015000930513     C* 02    - ESISTE DATA AL
015100920218     C* 03    - ESISTE DATA DAL
015200920218     C* 04    - ESISTE FEV
015300950227     C* 05    - ON - CAUSALE I0
015400950227     C* 06    - ON - CAUSALE I2
015500950227     C* 07    - ON - CAUSALE IB O IC
015600950227     C* 08    - STAMPA PER NUMERO SPEDIZIONE
015700941205     C* 10    - VARIAZIONE DI 102
015800950209     C* 11/17 - INDICATORI TIPO RECORD FILE VARIAZIONI
015900990519     C* 18    - STAMPO INDIRIZZO MITTENTE PER FRANCHI, INDIRIZZO DEST
016000990519     C*         PER ASSEGNATI SE C'E'
016100990622     C* 19/21 - INDICATORI TIPO RECORD FILE TITAS
016200991028     C* 23    - INDICATORE TIPO RECORD variazioni di peso
016300061129     C* 24    - INDICATORE TIPO RECORD 'FI'-CODICE FISCALE
016400920218     C* 29    - DEVO CONFRONTARE CON BLTAS
016500941118     C* 30    - FINE LETTURA DI UNA SPECIFICA FEV
016600920218     C* 31    - COMODO
016700920218     C* 32    - FINE LETTURA ESCO DA PROGRAMMA
016800000710     C* 33    - COMODO
016900000710     C* 38    - COMODO
017000000710     C* 39    - VARIAZ. POSTE CON DATA STAMPA = 0 NON 102
017100950223     C* 50    - ESISTONO DELLE VARIE
017200950210     C* 51/60 - CODIZIONANO LA STAMPA
017300990429     C* 63    - CONDIZIONA LA STAMPA PER LA 603 "UFF.ASSIC.MERCE"
017400990622     C* 70    - ESISTONO VARIE SU TITAS
017500990622     C* 81    - LA CONS. PARTICOLARE 1 NON ESISTE COME VARIA IN TITAS
017600990622     C* 82    - LA CONS. PARTICOLARE 2 NON ESISTE COME VARIA IN TITAS
017700920218     C* 90    - ERRORE GENERICO
017800941205     C*****************************************************************
017900920218     C*
018000920218     C                   EXSR      IMPO
018100920218     C*
018200920218     C* INIZIO LETTURA SE NON C'E FIL ELABORATORE
018300950227     C* E SE NON E' RISTAMPA PER NUMERO SPEDIZIONE
018400950227    1C     *IN32         DOWEQ     '0'
018500980109     C     LEGGI         TAG
018600920305     C                   SETOFF                                       111213
018700991025     C                   SETOFF                                       1510
018800991028     C                   SETOFF                                       161723
018900061129     C                   SETOFF                                       050607
019000061129     C                   SETOFF                                       24
019100990429     C*
019200991025     C  N08
019300991025     CANN04VIDFEV        SETGT     FIARBE1C
019400991025     C  N08
019500991025     CANN04              READ      FIARBE1C                               32
019600920218     C*
019700950227    2C  N32              DO
019800991025     C  N04
019900991025     CANN08              Z-ADD     ARBFEV        VIDFEV
020000920123     C*
020100920219     C* LETTURA
020200991025     C  N08KARBE         SETLL     FIARBE1C
020300991025     C   08KAR2          SETLL     FIARBE2C
020400920305     C*
020500920305     C* DECODIFICA
020600950227     C     *IN08         IFEQ      *OFF
020700920305     C                   MOVEL     *BLANKS       DESFEV
020800980109     C* VERIFICO CHE LA FILIALE APPARTENGA AL MIO RAMO AZIENDA
020900980109     C* ALTRIMENTI LA SALTO
021000020704     C     VIDFEV        CHAIN     AZORG                              31
021100991202     c
021200991202     c* escludo la filiale 603 e ho richisto tutti i p.o.
021300991202     c                   if        not *in04  and vidfev=603
021400991202     c                   goto      leggi
021500991202     c                   endif
021600991202     c
021700990429     C                   SETOFF                                       63
021800950227    3C     VIDFEV        IFEQ      46
021900920305     C                   MOVEL     STA(1)        DESFEV
022000920305     C                   ELSE
022100950227    4C     VIDFEV        IFEQ      102
022200920305     C                   MOVEL     STA(2)        DESFEV
022300941205     C                   SETON                                        10
022400990420     C* STAMPE PER UFFICIO ASSICURAZIONE MERCE QUANDO VIENE MODIFICATO
022500990420     C* L'IMPORTO DA ASSICURARE DI UNA BOLLA GIA' CONTABILIZZATA
022600990428     C                   ELSE
022700990420    5C     VIDFEV        IFEQ      603
022800990420     C                   MOVEL     STA(3)        DESFEV
022900990429     C                   SETON                                        63
023000920305     C                   ELSE
023100920305     C  N31              MOVEL     ORGDES        DESFEV           25
023200990420    5C                   END
023300950227    4C                   END
023400950227    3C                   END
023500950227     C                   END
023600920305     C* ACCENDO OF
023700920305     C                   SETON                                        OF
023800920305     C                   SETOFF                                       111213
023900991025     C                   SETOFF                                       151617
024000950217     C                   SETOFF                                       050607
024100061129     C                   SETOFF                                       2324
024200920217     C*
024300950227     C  N03
024400950227     CANN08
024500991025     COR 03
024600991025     CANN02KARBE         READE     FIARBE1C                               30
024700991025     C   03
024800991025     CAN 02VIDFEV        READE     FIARBE1C                               30
024900991025     C   08KAR2          READE     FIARBE2C                               30
025000920113     C*
025100950227    3C     *IN30         DOWEQ     '0'
025200920219     C* SE ANCHE DATA AL CONTROLLO CHE SIA COMPRESA NEI RANGE
025300950227     C   03
025400950227    4CAN 02              DO
025500991025     C                   Z-ADD     ARBDAT        COMDAT            8 0
025600950227    5C     COMDAT        IFGT      COMAL
025700920218     C                   SETON                                            30
025800950227    5C                   END
025900950227    4C                   END
026000920219     C*
026100000710     C* Non stampo se LNP = POSTE, Data Stampa = 0, ARBFEV <> 102
026200000710     C*
026300000710     C                   eval      *in39 = *OFF
026400000710     C     ARBFEV        ifne      102
026500000710     C     ARBDAT        andeq     0
026600000710     C     ARBLNP        chain     AZORG01L                           38
026700000710     C     *in38         ifeq      *off
026800000711     C                   movel     orgde3        og143
026900040203     C**!!!§ogpt         ifeq      'S'
027000040203     c                   If        §OgNtw = 'PPT'
027100000710     C                   seton                                        39
027200000710     C                   endif
027300000710     C                   endif
027400000710     C                   endif
027500000710     C*
027600920305     C* S T A M P A  VARIAZIONE
027700000710     C  N39
027800000710     CANN30              EXSR      STAARB
027900920219     C*
028000000710     C* A G G I O R N O  CON LA DATA DEL GIORNO (PER N39)
028100000710     C*                  CON DATA 31/12/2000    (PER  39)
028200950227     C  N03
028300950227    4CANN08              DO
028400000710      *
028500000710     C  N39              MOVEL     DATEU8        ARBDAT
028600000710     C   39              eval      ARBDAT = 20001231
028700920218     C*
028800950209     C   11              UPDATE    FNARBD00
028900950209     C   12              UPDATE    FNARBK00
029000991025     C   13              UPDATE    FIARBT00
029100950209     C   15              UPDATE    FNARBM00
029200950209     C   16              UPDATE    FNARBV00
029300950209     C   17              UPDATE    FNARBG00
029400991028     C   23              UPDATE    FNARBP00
029500000710     C*
029600950227    4C                   END
029700920113     C*
029800920113     C                   SETOFF                                       111213
029900991025     C                   SETOFF                                       2960
030000990623     C                   SETOFF                                       61  15
030100991028     C                   SETOFF                                       161723
030200950217     C                   SETOFF                                       050607
030300061129     C                   SETOFF                                       24
030400950227     C  N30
030500950227     CANN03
030600950227     CANN08
030700991025     CORN30
030800991025     CAN 03
030900991025     CANN02KARBE         READE     FIARBE1C                               30
031000991025     C  N30
031100991025     CAN 03
031200991025     CAN 02VIDFEV        READE     FIARBE1C                               30
031300991025     C   08
031400991025     CANN30KAR2          READE     FIARBE2C                               30
031500950227    3C                   END
031600950227    2C                   END
031700950227     C   08              SETON                                        32
031800920218     C   04              SETON                                        32
031900950227    1C                   END
032000920113     C*
032100920113     C     FINE          TAG
032200920219     C   98              EXCEPT    PIEDI
032300991202     c* solo per l'assicurazione stampo la pagina vuota
032400991202     c                   if        not *in98 and vidfev=603
032500991202     C                   EXCEPT    TESTA
032600991202     C                   EXCEPT    NOBOL
032700991202     C                   EXCEPT    PIEDI
032800991202     c                   endif
032900920113     C*
033000990519     C                   CLEAR                   DSBS69
033100990519     C                   MOVEL     'C'           I69TLA
033200990519     C                   CALL      'TIBS69R'
033300990519     C                   PARM                    DSBS69
033400990519     C                   PARM                    DSACO
033500990519     C                   PARM                    DSIND
033600990519     C                   PARM                    DSCLP
033700990519     C                   PARM                    DSCLS
033800990519     C*
033900920113     C                   SETON                                        LR
034000950227     C*****************************************************************
034100950227     C* DEFINIZIONE KLIST E CAMPI
034200950227     C*****************************************************************
034300950227     C     *INZSR        BEGSR
034400950227     C*
034500950227     C     *ENTRY        PLIST
034600950227     C                   PARM                    KPJBA
034700950227     C                   MOVEL     KPJBU         PARAM
034800950227     C*
034900950227     C                   Z-ADD     1             CODUT
035000950227     C                   CALL      'X§PARUT'
035100950227     C                   PARM                    UT§DSE
035200950227     C                   MOVEL     RAGUT         RSUT             20
035300950227     C*
035400950227     C     KARB          KLIST
035500950227     C                   KFLD                    WAAS              4 0
035600950227     C                   KFLD                    ARBLNP
035700950227     C                   KFLD                    ARBNRS
035800950227     C                   KFLD                    ARBNSP
035900950227     C                   KFLD                    COMTBL            2
036000950227     C     KAR2          KLIST
036100950227     C                   KFLD                    PARAAS
036200950227     C                   KFLD                    PARLNP
036300950227     C                   KFLD                    PARNRS
036400950227     C                   KFLD                    PARNSP
036500950227     C     KARBU         KLIST
036600950227     C                   KFLD                    ARBAAS
036700950227     C                   KFLD                    ARBLNP
036800950227     C                   KFLD                    ARBNRS
036900950227     C                   KFLD                    ARBNSP
037000991025     C                   KFLD                    ARBTRC
037100950227     C     KCSV          KLIST
037200950227     C                   KFLD                    TASAAS
037300950227     C                   KFLD                    TASLNP
037400950227     C                   KFLD                    TASNRS
037500950227     C                   KFLD                    TASNSP
037600950227     C                   KFLD                    ARBDTV
037700950227     C                   KFLD                    ARBORV
037800990623     C     KTAA          KLIST
037900990623     C                   KFLD                    TASAAS
038000990623     C                   KFLD                    TASLNP
038100990623     C                   KFLD                    TASNRS
038200990623     C                   KFLD                    TASNSP
038300990623     C                   KFLD                    §TRC
038400950227     C     KTAB          KLIST
038500950227     C                   KFLD                    CODUT
038600950227     C                   KFLD                    COD               2
038700950227     C                   KFLD                    KEY               8
038800950227     C     KARBE         KLIST
038900950227     C                   KFLD                    VIDFEV
039000950227     C                   KFLD                    DAT               8 0
039100950227     C     KTAS          KLIST
039200950227     C                   KFLD                    TASAAS
039300950227     C                   KFLD                    TASLNP
039400950227     C                   KFLD                    TASNRS
039500950227     C                   KFLD                    TASNSP
039600950227     C                   KFLD                    TASTBL
039700950227     C     KTAM          KLIST
039800950227     C                   KFLD                    TASKSC
039900950227     C                   KFLD                    TASCTR
040000950227     C     KTAM2         KLIST
040100950227     C                   KFLD                    TASKSC
040200950227     C                   KFLD                    TASCTR
040300950227     C                   KFLD                    PRG               3 0
040400950227     C                   KFLD                    COMFTC
040500950227     C*
040600990719     C     *LIKE         DEFINE    TPTFTC        COMFTC
040700950227     C     *LIKE         DEFINE    INDKCC        KCC
040800950227     C     *LIKE         DEFINE    ARBDTV        WRBDTV
040900960930     C     *LIKE         DEFINE    *IN33         WIN33
041000990623     C     *LIKE         DEFINE    TAATRC        §TRC
041100950227     C*
041200950227     C                   ENDSR
041300920218     C**---------------------------------------------------------
041400920218     C** IMPOSTAZIONI INIZIALI
041500920218     C**---------------------------------------------------------
041600920218     C     IMPO          BEGSR
041700950227     C*
041701131017     c* se ricevuta la coda di stampa eseguo ovrprtf prima di aprire il prtf
041702131017     c                   if        nomeprt<>*blanks
041716131017      /free
041717131017           Qcmd = 'OVRPRTF FILE(PRTF198) OUTQ('
041718131017                + nomeprt + ') HOLD(*YES)';
041719131017           callp    qCmdExc(Qcmd : %size(Qcmd));
041721131017      /end-free
041722131017     c                   endif
041723131017     c                   open      prtf198
041724131017
041800950227     C     NUMSPE        IFNE      *ZEROS
041900991025     C                   OPEN      FIARBE2C
042000950227     C                   SETON                                        08
042100950227     C                   ELSE
042200991025     C                   OPEN      FIARBE1C
042300950227     C                   END
042400920218     C* ESISTE DAL AL
042500920218     C     VIDAL         COMP      0                                  0202
042600920218     C*
042700920218     C* ESISTE DAL
042800920218     C     VIDDAL        IFNE      0
042900920218     C                   SETON                                        03
043000950209     C                   Z-ADD     COMDAL        DAT               8 0
043100920218     C                   END
043200920218     C*
043300920218     C     VIDFEV        IFNE      0
043400920218     C                   SETON                                        04
043500920218     C                   END
043600920218     C*
043700920218     C                   TIME                    UTIME             6 0
043800950209     C                   TIME                    DATORA           14 0
043900950209     C                   MOVE      DATORA        UDATE8            8 0
044000920219     C* SE STAMPA METTO DATA DEL GIORNO
044100920219     C     VIDDAL        IFEQ      0
044200950209     C                   Z-ADD     UDATE8        VIDDAL
044300920219     C* GIRO UDATE
044400950209     C                   Z-ADD     UDATE8        G02DAT
044500950209     C                   MOVE      *ZEROS        G02INV
044600950209     C                   MOVE      *BLANKS       G02ERR
044700950209     C                   CALL      'XSRDA8'
044800950209     C                   PARM                    WLBDAT
044900950217     C                   Z-ADD     G02INV        DATEU8            8 0
045000920219     C                   END
045100920218     C*
045200920218     C                   Z-ADD     0             IND
045300991027     C* CARICO DA SIGLE VARIE
045400991027     C                   MOVEL     '$2'          COD
045500991027     C                   MOVEL(p)  '1'           KEY
045600991027     C     KTAB          CHAIN     TABEL                              31
045700991027     C  N31              MOVEL     TBLUNI        DS$2
045800991027     C   31              MOVEL     *BLANKS       DS$2
045900941202     C*
046000920218     C                   ENDSR
046100920211     C**---------------------------------------------------------
046200950214     C** STAMPA ERRORI DI FNARB
046300920211     C**---------------------------------------------------------
046400920217     C     STAARB        BEGSR
046500950303     C*
046501140401     c* se l'errore da stampare è di quelli previsti nella schiera MSG
046502140401     c* non lo stampo
046503140401     c                   if        %lookup(arbder:msg)>0
046504140401     C                   GOTO      ENDSTA
046505140401     c                   endif
046600950303     C* Se variaz.mittente ignoro se causale variazione di contrassegno
046700950303     C     *IN15         IFEQ      *ON
046800950303     C     ARBCVB        ANDEQ     'K8'
046900950303     C     *IN15         OREQ      *ON
047000950303     C     ARBCVB        ANDEQ     'K5'
047100950303     C                   GOTO      ENDSTA
047200950303     C                   END
047300110906     c
047400110906     c* Stesso per destintario
047500110906     C     *IN11         IFEQ      *ON
047600110906     C     ARBCVB        ANDEQ     'K8'
047700110906     C     *IN11         OREQ      *ON
047800110906     C     ARBCVB        ANDEQ     'K5'
047900110906     C                   GOTO      ENDSTA
048000110906     C                   END
048100110906     c
048200930513     C                   SETOFF                                       01
048300920217     C* PRENDO TIPO BOLLA
048400920217     C                   MOVEL     '3A'          COD
048500920217     C                   MOVEL     *BLANKS       KEY
048600991025     C  N12              MOVEL     ARBCBO        KEY
048700991025     C   12              MOVEL     ARBCBP        KEY
048800920218     C     KTAB          CHAIN     TABEL                              31
048900920218     C  N31              MOVEL     TBLUNI        DS3A
049000920219     C   31              MOVEL     *BLANKS       DS3A
049100920219     C   31              MOVEL     0             §3AFCA
049200920211     C*
049300920217     C* PRENDO CAUSALE VARIAZIONE
049400920217     C                   MOVEL     '7L'          COD
049500920217     C                   MOVEL     *BLANKS       KEY
049600991025     C                   MOVEL     ARBCVB        KEY
049700920218     C     KTAB          CHAIN     TABEL                              31
049800920218     C  N31              MOVEL     TBLUNI        DESCVB           20
049900920218     C   31              MOVEL     *BLANKS       DESCVB           20
050000920218     C*
050100920218     C* SERIE
050200991025     C     ARBNRS        COMP      0                                      22
050300950210     C* ANNO SPEDIZIONE
050400991025     C                   MOVE      ARBAAS        STAAAS            2 0
050500950220     C* Rovescio ARBDTV se variazione di 102
050600950220     C     *IN10         IFEQ      *ON
050700991025     C                   Z-ADD     ARBDTV        G02INV
050800950220     C                   MOVE      *ZEROS        G02DAT
050900950220     C                   MOVEL     3             G02ERR
051000950220     C                   CALL      'XSRDA8'
051100950220     C                   PARM                    WLBDAT
051200950220     C                   Z-ADD     G02DAT        WRBDTV
051300950220     C                   END
051400920218     C*
051500950210     C* VEDO SE E' CAUSALE DA STAMPARE CON VERSIONE DI SEDE E
051600920217     C*  VERSIONE DI FILIALE O SOLO QUELLA DELLA FILIALE
051700920217     C     ARBDER        LOOKUP    ERR                                    29
051800950303     C*
051900920218     C*
052000950210     C* SE FNARBK0E ENTRO SEMPRE PER CHAINARE BOLLA
052100920218     C   12              SETON                                            29
052200990511     C* SE E' PER L'IMPORTO DA ASSICURARE, DEVO CHAINARE TAS
052300990511     C*  PER IL NUMERO E DATA FATTURA
052400990511     C   13ARBVAR        IFNE      ' '
052500990511     C                   SETON                                            29
052600990511     C                   ENDIF
052700920312     C*
052800920312     C* VEDO CHE CAUSALE E'
052900950213     C     *IN11         IFEQ      *ON
053000950213     C     ARBCVB        COMP      'I0'                                   05
053100950213     C     ARBCVB        COMP      'I2'                                   06
053200950213     C     ARBCVB        COMP      'IB'                                   07
053300950213     C  N07ARBCVB        COMP      'IC'                                   07
053400061129     C     ARBCVB        COMP      'FI'                                   24
053500950213     C                   END
053600991026     C* operazioni per record di tassazione
053700991025     C     *IN13         IFEQ      *ON
053800991026     C* Caricamento schiere varie di ARBT
053900991026     C                   EXSR      VARARB
054000991027     c                   setoff                                       50
054100991026     C     ARBTRC        ifeq      '1'
054200991026     c                   setoff                                       14
054300991029     C     sveds1        comp      *blanks                            5050
054400991026     c                   else
054500991026     c                   seton                                        14
054600991025     C                   ENDIF
054700991026     c* memorizzo se divisa accetta i decimali
054800991026     c* divisa di arb
054900991026     C                   MOVEL     'CV'          COD
055000991026     C                   MOVEL(P)  ARBdiv        KEY
055100991026     C     KTAB          CHAIN     TABEL                              31
055200991026     C  N31              MOVEL     TBLUNI        DScv
055300991026     C   31              clear                   DScv
055400991026     c                   movel     §cvfdc        wfdcarb
055500991026     c                   endif
055600941118     C**
055700990622     C* N O N   L E G G O   T I T A S
055800941118     C**
055900920312     C* PER 29 OFF LO DECODIFICO LO STESSO
056000941118    1C     *IN29         IFEQ      *OFF
056100941118     C*
056200921211     C*
056300921211     C* PER 29 OFF EMETTO COMUNQUE I CAMPI DI STAMPA
056400941118     C   11
056500941118    2CAN 05              DO
056600051108     c                   select
056700051108     c                   when      arbffd='S'
056800051108     C                   MOVEL     'SI '         ST2FFD            3
056900051108     c                   when      arbffd='M'
057000051108     C                   MOVEL     'SAB'         ST2FFD            3
057100051108     C                   other
057200051108     C                   MOVEL     '   '         ST2FFD
057300051108    3C                   ENDsl
057400941118    2C                   END
057500991026     c* preparo schiera per stampa varie di arb editate
057600991026     c                   clear                   svp
057700991026    5C                   DO        20            II
057800991026     C                   z-add     SEA(II)       w0133
057900991026     c                   movel     wfdcarb       wtstdec
058000991026     c                   movel     '3'           wedtcde
058100991026     c                   exsr      edtimporti
058200991026     c                   move      impeditato    svp(ii)
058300991026     c                   movel     sve(ii)       svp(ii)
058400991026    5C                   ENDDO
058500920217     C*
058600941118     C                   ELSE
058700941118     C**
058800990622     C*         L E G G O   T I T A S
058900941118     C**
059000950214     C* CHAINO BOLLA CON CONTRASSEGNO
059100941118    2C   12§3ATB2        IFNE      '  '
059200920601     C                   MOVEL     §3ATB2        COMTBL
059300920218     C                   ELSE
059400920601     C                   MOVEL     §3ATB1        COMTBL            2
059500941118    2C                   END
059600991025     C*
059700991025     C   13ARBTRC        IFEQ      '1'
059800991025     C                   MOVEL     §3ATB1        COMTBL
059900991025     C                   ELSE
060000991025     C                   MOVEL     §3ATB2        COMTBL            2
060100991025     C                   ENDIF
060200920218     C*
060300990622     C* SE FNARBD CHAIN SEMPRE  PRIMA   BOLLA PER TUTTE LE CAUSALI
060400990622     C* (ANCHE PER 'I0' VISTO CHE ADESSO IL DESTINATARIO SU TITAS C'E'
060500990622     C* SEMPRE)
060600991028     C* SE FNARBG  O FNARBV O ARBM CHAIN SEMPRE  PRIMA   BOLLA
060700990622    2C     *IN11         IFEQ      *ON
060800990622    2C     *IN17         OREQ      *ON
060900950210     C     *IN16         OREQ      *ON
061000950214     C     *IN15         OREQ      *ON
061100991028     C     *IN23         OREQ      *ON
061200950210     C                   MOVEL     §3ATB1        COMTBL
061300950217    2C                   END
061400920217     C*
061500991025     C                   Z-ADD     ARBAAS        WAAS
061600941118     C**
061700941117     C                   SETOFF                                       192021
061800991025     C     KARB          CHAIN     TITAS30C                           33
061900990624     C* MEMORIZZO SE RIUSCITA CHAIN SU TITAS
062000960930     C                   MOVE      *IN33         WIN33
062100941118     C*
062200920218     C* DATA FATTURA BOLLA DI SEDE
062300960930     C     *IN33         IFEQ      *OFF
062400941117     C                   Z-ADD     TASDFT        G02INV
062500920218     C                   MOVE      *ZEROS        G02DAT
062600920218     C                   MOVEL     3             G02ERR
062700941117     C                   CALL      'XSRDA8'
062800920218     C                   PARM                    WLBDAT
062900920218     C                   MOVEL     G02DAT        STTDFT            4 0
063000960930     C                   ELSE
063100960930     C                   CLEAR                   STTDFT
063200960930     C                   CLEAR                   TASNFT
063300960930     C                   END
063400941118     C*
063500990622     C* CARICO LE VARIE DI TITAS
063600960930     C  N33              EXSR      VARIE
063700991026     c* divisa di titas
063800991026     c     *in33         ifeq      *off
063900991026     c     *in13         andeq     *on
064000991026     C                   MOVEL     'CV'          COD
064100991026     C                   MOVEL(P)  tasdiv        KEY
064200991026     C     KTAB          CHAIN     TABEL                              31
064300991026     C  N31              MOVEL     TBLUNI        DScv
064400991026     C   31              clear                   DScv
064500991026     c                   movel     §cvfdc        wfdctas
064600991026     c                   endif
064700920218     C*
064800040203     C* F I A R B T 0 F  (TASSAZIONE  BOLLA)
064900990511    2C   13ARBVAR        IFEQ      ' '
065000040203     C**!!!ARBTRC        ANDEQ     '1'
065100941118     C*
065200920218     C* CONFRONTO CAMPI MODIFICATI
065300920218     C     TASKSC        COMP      ARBKSC                             4040
065400920218     C*
065500941117     C     TASCTR        COMP      ARBCTR                             4141
065600950210     C*
065700950210     C     TASQFT        COMP      ARBQFT                             4848
065800920218     C*
065900991102     C     tasdiv        COMP      ARBDIV                             4444
066000991025     C*
066100991025     C     TASPOR        COMP      ARBPOR                             4545
066200941118     C**
066300941118     C* VARIE
066400991029     C**
066500950306     C                   SETOFF                                       42
066600950216     C                   CLEAR                   SVP
066700991029     C                   CLEAR                   SVS
066800950223     C*
066900991029     C     SVRDS         COMP      *BLANKS                                33
067000991029     C     SVEDS         COMP      *BLANKS                                99
067100991029     c*
067200991029     c* Se Titas ha delle varie le memorizzo in schiera di stampa
067300991029     c     *in33         ifeq      *off
067400991029    5C                   DO        20            II
067500991029     C                   z-add     VAR(II)       w0133
067600991029     c                   movel     wfdctas       wtstdec
067700991029     c                   movel     '4'           wedtcde
067800991029     c                   exsr      edtimporti
067900991029     c                   move      impeditato    svs(ii)
068000991029     c                   movel     svr(ii)       svs(ii)
068100991029    5C                   ENDDO
068200991029     c                   endif
068300991029     c*
068400991029     c* se arb non ha varie verifico se tas ne ha più di 5 per sapere se
068500991029     c* stampare la riga altre varie
068600991029     c   99svrds1        comp      *blanks                            5050
068700991102     c* Anche se arb non ha varie inizializzo schiera stampa
068800991102     c* solo per gli elementi che per tas sono pieni
068900991102     c     *in99         ifeq      *on
069000991102     c     *in33         andeq     *off
069100991102    5C                   DO        20            II
069200991102     c     svr(ii)       ifne      *blanks
069300991102     C                   z-add     sea(II)       w0133
069400991102     c                   movel     wfdctas       wtstdec
069500991102     c                   movel     '3'           wedtcde
069600991102     c                   exsr      edtimporti
069700991102     c                   move      impeditato    svp(ii)
069800991102     c                   movel     sve(ii)       svp(ii)
069900991102     c                   endif
070000991102    5C                   ENDDO
070100991102     c                   end
070200950223     C*
070300991026     C* Se TITAS non ha varie stampo solo le varie dell'arbt
070400950217    3C     *IN99         IFEQ      *OFF
070500950217    4C     *IN33         IFEQ      *ON
070600950306     C*
070700991029     c     sveds1        comp      *blanks                            5050
070800950306     C                   SETON                                        42
070900991026     c* anche se titas non ha varie devo ugualmente impostare la schiera
071000991026     c* di stampa delle varie di titas
071100991026     c                   clear                   svs
071200991026    5C                   DO        20            II
071300991026     C                   z-add     VAR(II)       w0133
071400991026     c                   movel     wfdctas       wtstdec
071500991029     c                   movel     '4'           wedtcde
071600991026     c                   exsr      edtimporti
071700991026     c                   move      impeditato    svs(ii)
071800991026     c                   movel     svr(ii)       svs(ii)
071900991026    5C                   ENDDO
072000991026     c
072100991026     C* Riempio schiera varie di arbt
072200950217    5C                   DO        20            II
072300950217     C     SEA(II)       IFNE      *ZEROS
072400991026     C                   z-add     SEA(II)       w0133
072500991027     c                   movel     '4'           wedtcde
072600991026     c                   movel     wfdcarb       wtstdec
072700991026     c                   exsr      edtimporti
072800991026     c                   move      impeditato    svp(ii)
072900950217     C                   END
073000991026     C                   MOVEL     sve(ii)       SVP(II)
073100950217    5C                   ENDDO
073200950216     C*
073300950216     C                   ELSE
073400950216     C*
073500950210     C* Controllo variazioni
073600991026     c*
073700950217    5C                   DO        20            II                2 0
073800950217    6C     SVR(II)       IFEQ      *BLANKS
073900950220     C                   SUB       1             II
074000950210     C                   LEAVE
074100950217    6C                   END
074200950210     C                   Z-ADD     1             XX                2 0
074300950210     C     SVR(II)       LOOKUP    SVE(XX)                                99
074400991026     C* Non trovata varia di TITAS in varia di ARBT (tolta varia)
074500991026     C  N99              z-add     0             w0133
074600991026     C  N99              movel     wfdcarb       wtstdec
074700991026     C  N99              movel     '3'           wedtcde
074800991026     C  N99              exsr      edtimporti
074900991026     c  n99              move      impeditato    svp(ii)
075000991026     c  n99              movel     ' '           svp(ii)
075100991026     C  N99              seton                                        42
075200950217    6C     *IN99         IFEQ      *ON
075300950210     C                   MOVE      *BLANKS       SVC(XX)
075400950210     C* Trovata varia ma importo diverso
075500950217    7C     VAR(II)       IFNE      SEA(XX)
075600950306     C                   SETON                                        42
075700991026     C                   z-add     sea(xx)       w0133
075800991026     C                   movel     wfdcarb       wtstdec
075900991026     C                   movel     '3'           wedtcde
076000991026     C                   exsr      edtimporti
076100991026     c                   move      impeditato    svp(ii)
076200991026     c                   movel     sve(xx)       svp(ii)
076300950217    7C                   ENDIF
076400950210     C* Trovata varia e stesso importo
076500950217    7C     VAR(II)       IFEQ      SEA(XX)
076600950210     C                   MOVE      *BLANKS       SVP(II)
076700950217    7C                   ENDIF
076800950217    6C                   ENDIF
076900950217    5C                   ENDDO
077000950210     C* Verifico se sono state aggiunte delle varie
077100950217    5C                   DO        20            XX                2 0
077200950217    6C     SVC(XX)       IFNE      *BLANKS
077300950306     C                   SETON                                        42
077400950210     C                   ADD       1             II
077500991026     C                   z-add     sea(xx)       w0133
077600991026     C                   movel     wfdcarb       wtstdec
077700991026     C                   movel     '3'           wedtcde
077800991026     C                   exsr      edtimporti
077900991026     c                   move      impeditato    svp(ii)
078000991026     c                   movel     svc(xx)       svp(ii)
078100950217    6C                   ENDIF
078200950217    5C                   ENDDO
078300991029     c*
078400991029     c     ii            ifgt      4
078500991029     c                   seton                                        50
078600991029     c                   endif
078700950216     C*
078800950217    4C                   END
078900950217    3C                   END
079000920218     C*
079100950306     C                   SETOFF                                       56
079200950306     C     TASNFT        IFNE      ARBNFT
079300950306     C     ARBNFT        ANDNE     *ZEROS
079400950306     C                   SETON                                        56
079500950306     C                   END
079600950324     C*
079700950324     C                   SETOFF                                       54
079800950324     C     TASFIV        IFNE      ARBFIV
079900950324     C     ARBFIV        ANDNE     *ZEROS
080000950324     C                   SETON                                        54
080100950324     C                   END
080200950324     C*
080300950306     C     TASDFT        COMP      ARBDFT                             5757
080400950306     C* Non stampo se tutto invariato
080500950303     C     *IN40         IFEQ      *OFF
080600950303     C     *IN41         ANDEQ     *OFF
080700950306     C     *IN42         ANDEQ     *OFF
080800950303     C     *IN44         ANDEQ     *OFF
080900950303     C     *IN45         ANDEQ     *OFF
081000950303     C     *IN48         ANDEQ     *OFF
081100950324     C     *IN54         ANDEQ     *OFF
081200950306     C     *IN56         ANDEQ     *OFF
081300950303     C                   GOTO      ENDSTA
081400950303     C                   END
081500990426    2C                   END
081600920218     C*
081700991026     C* F I A R B T 0 E  (tassazione 2bolla)
081800040203    2C**!13arbtrc        ifeq      '2'
081900920218     C*
082000920218     C* CONFRONTO CAMPI MODIFICATI
082100040203     C**!!!TASKSC        COMP      ARbKSC                             4040
082200920218     C*
082300040203     C**!!!TASCTR        COMP      ARbCTR                             4141
082400920218     C*
082500991027     C*
082600040203     C**!!!tasdiv        COMP      ARBDIV                             4444
082700941118     C* VARIE
082800040203     c**!!!              clear                   svp
082900040203     c**!!!              clear                   svs
083000941118     C*
083100991027     C* IN TITAS non c'e' la varia "G"
083200040203     C**!!!              SETOFF                                       49
083300040203    3C**!!!SVR(1)        IFEQ      ' '
083400040203    4C**!!!sve(1)        IFNE      *blanks
083500040203     C**!!!              SETON                                        49
083600040203    4C**!!!              ENDIF
083700941118     C*
083800040203   X3C**!!!              ELSE
083900991027     C* IN titas c'e' la varia "G"
084000991027     c* carico la varia di titas in schiera editata per la stampa
084100040203     C**!!!              z-add     var(1)        w0133
084200040203     c**!!!              movel     wfdctas       wtstdec
084300040203     c**!!!              movel     '4'           wedtcde
084400040203     c**!!!              exsr      edtimporti
084500040203     c**!!!              move      impeditato    svs(1)
084600040203     c**!!!              movel     svr(1)        svs(1)
084700991027     c* varia "G" diversa
084800040203    4C**!!!sve(1)        IFNE      *BLANKS
084900040203    5C**!!!VAR(1)        IFNE      sea(1)
085000040203     C**!!!              SETON                                        49
085100040203    5C**!!!              ENDIF
085200040203    4C**!!!              ENDIF
085300040203    3C**!!!              ENDIF
085400941118     C* VEDO SE HO ELIMINATO DELLE VARIE
085500040203    3C**!!!*IN49         IFEQ      *OFF
085600040203    4C**!!!SVR(1)        andne     ' '
085700040203    5C**!!!SVR(1)        andne     ARBSV1
085800040203     C**!!!              SETON                                        49
085900040203    3C**!!!              ENDIF
086000991027     c* Edito la varia di arb per la stampa
086100040203     c**!!!*in49         ifeq      *on
086200040203     C**!!!              z-add     sea(1)        w0133
086300040203     c**!!!              movel     wfdcarb       wtstdec
086400040203     c**!!!              movel     '3'           wedtcde
086500040203     c**!!!              exsr      edtimporti
086600040203     c**!!!              move      impeditato    svp(1)
086700040203     c**!!!              movel     sve(1)        svp(1)
086800040203     C**!!!              endif
086900920218     C*
087000040203     C**!!!              SETOFF                                       54
087100040203     C**!!!TASFIV        IFNE      ARBFIV
087200040203     C**!!!ARBFIV        ANDNE     *ZEROS
087300040203     C**!!!              SETON                                        54
087400040203     C**!!!              END
087500950324     C*
087600040203     C**!!!TASNFT        COMP      ARBNFT                             5656
087700920218     C*
087800040203     C**!!!TASDFT        COMP      ARBDFT                             5757
087900040203    2C**!!!              END
088000920218     C*
088100950213     C*  F N A R B D 0 E
088200941118    2C   11              DO
088300920309     C*
088400950213     C* SOLO SE I2 (VARIATO VOLUME)
088500950213    3C     ARBCVB        IFEQ      'I2'
088600920218     C*
088700950214     C* Volume da fatturare
088800950214     C     TASVLF        COMP      ARBVLF                             4949
088900950214     C* Volume bollettato
089000950214     C     TASVLB        COMP      ARBVLB                             4848
089100941205     C*
089200950213     C  N48
089300950213     CANN49              GOTO      ENDSTA
089400930513     C*
089500941118    3C                   END
089600920218     C*
089700950213     C* SOLO SE I0 (DESTINATARIO E PARTITA IVA)
089800950213    3C     ARBCVB        IFEQ      'I0'
089900950223     C*
090000990623     C                   SETOFF                                       4060
090100930303     C*  FERMO DEPOSITO
090200051108     c                   select
090300051108     c                   when      tasffd='S'
090400051108     C                   MOVEL     'SI '         ST1FFD
090500051108     c                   when      tasffd='M'
090600051108     C                   MOVEL     'SAB'         ST1FFD
090700051108     C                   other
090800051108     C                   MOVEL     '   '         ST1FFD            3
090900051108    4C                   ENDsl
091000930303     C*
091100051108     c                   select
091200051108     c                   when      arbffd='S'
091300051108     C                   MOVEL     'SI '         ST2FFD            3
091400051108     c                   when      arbffd='M'
091500051108     C                   MOVEL     'SAB'         ST2FFD            3
091600051108   X4C                   other
091700051108     C                   MOVEL     '   '         ST2FFD
091800051108    4C                   ENDsl
091900930303     C**
092000990623     C  N19
092100990623     CANN20
092200990623     CANN21              SETON                                        4060
092300920218     C*
092400920218     C*
092500990623     C  N40TASNZD        COMP      ARBNZD                             4040
092600990623     C  N40TASCAD        COMP      ARBCAD                             4040
092700990623     C  N40TASRSD        COMP      ARBRSD                             4040
092800950213     C*
092900950213     C  N40TASIND        COMP      ARBIND                             4040
093000920218     C*
093100950213     C  N40TASLOD        COMP      ARBLOD                             4040
093200920402     C*
093300990623     C  N40TASPRD        COMP      ARBPRD                             4040
093400950213     C*
093500990623     C  N40ARBCPI        COMP      TASCPD                             4040
093600941118    5C   40ARBCPI        IFEQ      *BLANKS
093700930303     C                   MOVE      '0'           ARBCPI
093800941118    5C                   END
093900950301     C  N40              GOTO      ENDSTA
094000920218     C*
094100941118    3C                   END
094200950213     C* SOLO SE IB (PESO CAUSA BOLLETTISTA)
094300950213     C*              OPPURE
094400950213     C* SOLO SE IC (PESO CAUSA CLIENTE)
094500950213    3C     ARBCVB        IFEQ      'IB'
094600950213    3C     ARBCVB        OREQ      'IC'
094700991026     C     TASPKB        IFEQ      ARBPKB
094800991026     C                   GOTO      ENDSTA
094900950213     C                   END
095000991026     C                   END
095100941118    2C                   END
095200920218     C*
095300950213     C*  F N A R B K 0 E
095400941118    2C   12              DO
095500051124     C                   MOVEL     §3ADES        DESCBP           17
095600920220     C                   MOVEL     §3ATB1        OLDTB1            2
095700920220     C                   MOVEL     §3ATB2        OLDTB2            2
095800920218     C*
095900920218     C                   MOVEL     '3A'          COD
096000920218     C                   MOVEL     *BLANKS       KEY
096100920218     C                   MOVEL     ARBCBN        KEY
096200920218     C     KTAB          CHAIN     TABEL                              33
096300920220     C  N33              MOVEL     TBLUNI        DS3A
096400051124     C  N33              MOVEL     §3ADES        DESCBN           15
096500920219     C   33              MOVEL     *BLANKS       DESCBN
096600920220     C   33              MOVEL     *BLANKS       DS3A
096700920218     C*
096800920601     C* PRENDO PRECEDENTE IMPORTO C/ASSEGNO
096900950213     C                   MOVE      *ZEROS        CSVCAS
097000960930     C                   CLEAR                   CSVVCA
097100990623     C* SOLO SE RIUSCITA CHAIN SU TITAS
097200960930     C     WIN33         IFEQ      '0'
097300941118     C     KCSV          CHAIN     TNCSV000                           31
097400960930     C                   END
097500950217     C                   ENDDO
097600950213     C*  F N A R B G 0 E
097700950213     C   17              DO
097800950228     C                   CLEAR                   WVARC1
097900950228     C                   CLEAR                   WVARC2
098000950215     C                   SETOFF                                       4849
098100950228     C                   SETOFF                                       8182
098200950220     C                   MOVEL     *BLANKS       DETFTC
098300950220     C                   MOVEL     *BLANKS       DETTC2
098400950220     C                   MOVEL     '1P'          COD
098500950228     C     TASFTC        IFNE      *BLANKS
098600950213     C                   MOVEL     *BLANKS       KEY
098700950213     C                   MOVEL     TASFTC        KEY
098800950213     C     KTAB          CHAIN     TABEL                              31
098900950228     C     *IN31         IFEQ      *OFF
099000950228     C                   MOVEL     TBLUNI        DETFTC           15
099100950228     C                   MOVEL     TBLUNI        DS1P
099200950228     C                   MOVEL     §1PVAR        WVARC1            1
099300950228     C     §1PVAR        LOOKUP    SVR                                    81
099400950228     C                   END
099500950228     C                   END
099600950228     C     TASTC2        IFNE      *BLANKS
099700950213     C                   MOVEL     *BLANKS       KEY
099800950213     C                   MOVEL     TASTC2        KEY
099900950213     C     KTAB          CHAIN     TABEL                              31
100000950228     C     *IN31         IFEQ      *OFF
100100950228     C                   MOVEL     TBLUNI        DETTC2           15
100200950228     C                   MOVEL     TBLUNI        DS1P
100300950228     C                   MOVEL     §1PVAR        WVARC2            1
100400950228     C     §1PVAR        LOOKUP    SVR                                    82
100500950228     C                   END
100600950228     C                   END
100700950215     C* Se immessa la cons particolare, e non trovata la relativa
100800950215     C*  varia --> stampo errore se la tariffa prevede la tassazione
100900950228     C     TASFTC        IFNE      *BLANKS
101000950228     C     *IN81         ANDEQ     *OFF
101100950228     C     TASTC2        ORNE      *BLANKS
101200950228     C     *IN82         ANDEQ     *OFF
101300950213     C                   EXSR      CTRPAR
101400950228     C                   ENDIF
101500950227     C  N48
101600950227     CANN49              GOTO      ENDSTA
101700950213     C                   ENDDO
101800950214     C*  F N A R B M 0 E
101900950217    2C   15              DO
102000950303     C*
102100950222     C                   SETOFF                                       40
102200990623     C* Se in TITAS il mittente è codificato prendo i dati da CNIND
102300950214     C                   MOVE      TASKSC        COM888            4 0
102400950217    3C     COM888        IFNE      8888
102500950215     C                   EXSR      CAPOC
102600990519     C                   CLEAR                   DSBS69
102700990519     C                   MOVEL     KNSIF         I69SIF
102800990519     C                   MOVEL     KCC           I69KCC
102900990519     C                   MOVEL     TASKSC        I69KAC
103000990519     C                   MOVEL     TASKSC        I69KIN
103100990519     C                   CALL      'TIBS69R'
103200990519     C                   PARM                    DSBS69
103300990519     C                   PARM                    DSACO
103400990519     C                   PARM                    DSIND
103500990519     C                   PARM                    DSCLP
103600990519     C                   PARM                    DSCLS
103700990519     C     O69ERR        IFNE      ' '
103800990519     C                   CLEAR                   DSIND
103900990519     C                   CLEAR                   DSACO
104000990519     C                   ENDIF
104100990623     C                   MOVEL     INDVIA        TAAIND                         indirizzo
104200990623     C                   MOVEL     INDCIT        TAALOC                         località
104300990623     C                   MOVEL     INDPRV        TAAPRV                         provincia
104400990623     C                   MOVEL     INDIVA        TAACPI                         partita IVA
104500990623     C                   MOVEL     ACORAG        TAARSC
104600950222     C                   ELSE
104700990623     C* SE NON CODIFICATO CHAINO ANAGRAFICA DELLA BOLLA
104800990623     C                   MOVEL     'M'           §TRC
104900990623     C     KTAA          CHAIN     TITAA30C                           31
105000990623     C   31              MOVEL     *BLANKS       TAAIND                         indirizzo
105100990623     C   31              MOVEL     *BLANKS       TAALOC                         località
105200990623     C   31              MOVEL     *BLANKS       TAAPRV                         provincia
105300990623     C   31              MOVEL     *BLANKS       TAACPI                         partita IVA
105400990623     C   31              MOVEL     *BLANKS       TAARSC
105500950217    3C                   END
105600950222     C* Confronto se campi variati
105700950221     C* Verifico se mittente di ARBM è codificato
105800950221     C                   MOVE      ARBCCM        COM888
105900950221     C     COM888        COMP      8888                               4141
106000950214     C* Codice cliente mittente
106100950214     C     TASKSC        COMP      ARBCCM                             4040
106200950215     C* Ragione sociale mittente
106300990623     C  N41TAARSC        COMP      ARBRSM                             4040
106400950214     C* Indirizzo
106500990623     C  N41
106600990623     CANN40TAAIND        COMP      ARBINM                             4040
106700950214     C* CAP
106800950214     C  N40TASCAM        COMP      ARBCAM                             4040
106900950214     C* Località
107000990623     C  N41
107100990623     CANN40TAALOC        COMP      ARBLOM                             4040
107200950214     C* Provincia
107300990623     C  N41
107400990623     CANN40TAAPRV        COMP      ARBPRM                             4040
107500950214     C* Nazione
107600950214     C  N40TASNZM        COMP      ARBNZM                             4040
107700950214     C* Anteporto
107800950221     C  N41
107900950221     CANN40TASFAP        COMP      ARBFAP                             4040
108000950214     C* Partita IVA
108100990623     C  N41
108200990623     CANN40TAACPI        COMP      ARBCPI                             4040
108300950221     C  N41
108400950221    4CAN 40ARBCPI        IFEQ      *BLANKS
108500950214     C                   MOVE      '0'           ARBCPI
108600950217    4C                   END
108700950221     C  N40              GOTO      ENDSTA
108800950217    2C                   ENDDO
108900950215     C* F N A R B V 0 E
109000950215     C   16              DO
109100950216     C     TASVLF        COMP      ARBVLC                             4040
109200950216     C     TASFVF        COMP      ARBFVC                             4141
109300950216     C  N40
109400950216     CANN41              GOTO      ENDSTA
109500950215     C                   ENDDO
109600991028     c* N.B.: Per FNARBP0E non prevedo confronti perchè per ora l'unico
109700991028     c*       tipo di errore e' di spedizione inesistente e quindi non
109800991028     c*       dovrebbe mai passare di qui
109900061129     c* N.B.: Idem per Causale Variazione FI
110000920218     C*
110100941118    1C                   END
110200061129     c*
110300061129     c                   if        *in24
110400061129     c                   movel     arbrd2        flagmd            1
110500061129     c                   if        flagmd='D'
110600061129     c                   eval      decomitdes='Destinatario:'
110700061129     c                   else
110800061129     c                   eval      decomitdes='  Mittente  :'
110900061129     c                   endif
111000061129     c                   endif
111100941118     C**
111200920218     C* S T A M P A
111300941118     C**
111400941118    1C   OF              DO
111500920219     C   98              EXCEPT    PIEDI
111600950213     C*
111700920218     C                   EXCEPT    TESTA
111800920218     C*
111900920218     C                   SETOFF                                       OF
112000950213     C     *IN11         IFEQ      *ON
112100950213     C   05              Z-ADD     05            IND               2 0
112200950213     C   06              Z-ADD     06            IND
112300950213     C   07              Z-ADD     07            IND
112400061129     C   24              Z-ADD     24            IND
112500950213     C                   END
112600920218     C   12              Z-ADD     12            IND
112700920218     C   13              Z-ADD     13            IND
112800950214     C   15              Z-ADD     15            IND
112900950213     C   16              Z-ADD     16            IND
113000950213     C   17              Z-ADD     17            IND
113100991028     C   23              Z-ADD     23            IND
113200920219     C                   SETON                                        98
113300941118    1C                   END
113400920219     C***********
113500941118    1C   11              DO
113600991026     C  N29              SETON                                        404849
113700950213    2C   05IND           IFNE      05
113800950213     C                   EXCEPT    DESTES
113900950213     C                   Z-ADD     05            IND               2 0
114000950220     C                   END
114100950213    2C   06IND           IFNE      06
114200950213     C                   EXCEPT    DESTES
114300950213     C                   Z-ADD     06            IND
114400950220     C                   END
114500950213    2C   07IND           IFNE      07
114600950213     C                   EXCEPT    DESTES
114700950213     C                   Z-ADD     07            IND
114800950213     C                   END
114900061129    2C   24IND           IFNE      24
115000061129     C                   EXCEPT    DESTES
115100061129     C                   Z-ADD     24            IND
115200061129     C                   END
115300920219     C                   EXCEPT    BOLDET
115400920219     C                   EXCEPT    DESDET
115500060110     c  n05
115600060110     Can 10              EXCEPT    oraDET
115700060110     C   29              EXCEPT    BLKDET
115800941118    1C                   END
115900920219     C***********
116000941118    1C   12IND           IFNE      12
116100920218     C                   EXCEPT    CBLTES
116200920218     C                   Z-ADD     12            IND
116300941118    1C                   END
116400920219     C   12              EXCEPT    BOLDET
116500920219     C   12              EXCEPT    CBLDET
116600060110     c   12
116700060110     Can 10              EXCEPT    oraDET
116800920219     C***********
116900941118    1C   13              DO
117000920219     C* GIRO SEMPRE DATA FATTURA
117100950216     C                   MOVE      ARBDFT        DATA
117200950216     C                   MOVEL     DATA          STADFT
117300941118     C                   MOVEL     AA            STADFT            4 0
117400920219     C*
117500991027     C  N29              SETON                                        404149
117600991026     C  N29              SETON                                        444548
117700990622     C  N29              SETON                                        565754
117800941118    2C     IND           IFNE      13
117900920218     C                   EXCEPT    TASTES
118000920218     C                   Z-ADD     13            IND
118100941118    2C                   END
118200991027     c* Editazione quantità da fatturare e porto
118300991027     c                   clear                   prtqft
118400991027     c                   clear                   prtpor
118500991027     c                   clear                   prfqft
118600991027     c                   clear                   prfpor
118700991027     c     *in29         ifeq      *on
118800991027     c     *in14         andeq     *off
118900991027     c     tasqft        ifgt      0
119000991027     c                   z-add     tasqft        w0133
119100991027     c                   movel     wfdctas       wtstdec
119200991027     c                   movel     '4'           wedtcde
119300991027     c                   exsr      edtimporti
119400991027     c                   move      impeditato    prtqft
119500991027     c                   endif
119600991027     c*
119700991027     c     taspor        ifgt      *Zeros
119800991027     c                   z-add     taspor        w0133
119900991027     c                   movel     wfdctas       wtstdec
120000991027     c                   movel     '4'           wedtcde
120100991027     c                   exsr      edtimporti
120200991027     c                   move      impeditato    prtpor
120300991027     c                   endif
120400991027     c                   endif
120500991027     c*
120600991027     c     *in48         ifeq      *on
120700991027     c     *in14         andeq     *off
120800991027     c                   z-add     arbqft        w0133
120900991027     c                   movel     wfdcarb       wtstdec
121000991027     c                   movel     '3'           wedtcde
121100991027     c                   exsr      edtimporti
121200991027     c                   move      impeditato    prfqft
121300991027     c                   endif
121400991027     c*
121500991027     c     *in45         ifeq      *on
121600991027     c     *in14         andeq     *off
121700991027     c                   z-add     arbpor        w0133
121800991027     c                   movel     wfdcarb       wtstdec
121900991027     c                   movel     '3'           wedtcde
122000991027     c                   exsr      edtimporti
122100991027     c                   move      impeditato    prfpor
122200991027     c                   endif
122300990428     C* SE STAMPA PER 603 STAMPO SOLO L'IMPORTO D'ASSICURARE
122400990429     C* NEL FILE FNARBT0E C'E' UN CAMPO ARBVAR CHE E' UGUALE A "R" SE SI TRATTA
122500990429     C* DI VARIAZIONE IMPORTO D'ASSICURARE
122600990429     C* VARIAZIONE SULL'IMPORTO DA ASSICURARE
122700990519     C                   CLEAR                   DESCLI
122800990519     C                   SETOFF                                       18
122900990429    2C     ARBVAR        IFEQ      'R'
123000990519     C* GIRO LA DATA SPEDIZIONE
123100990519     C                   Z-ADD     TASDSP        G02INV
123200990519     C                   MOVE      *ZEROS        G02DAT
123300990519     C                   MOVEL     3             G02ERR
123400990519     C                   CALL      'XSRDA8'
123500990519     C                   PARM                    WLBDAT
123600990519     C                   MOVEL     G02DAT        STTDSP            8 0
123700990519     C*
123800990519     C* PER FRANCO STAMPO INDIRIZZO MITTENTE
123900990519     C* PER ASSEGNATO,    INDIRIZZO DESTINATARIO
124000990519     C                   MOVE      TASKSC        W0040             4 0
124100990519    3C     W0040         IFNE      8888
124200990519     C     W0040         ANDNE     9999
124300990519     C                   SETON                                        18
124400990519     C                   EXSR      CAPOC
124500990519     C                   CLEAR                   DSBS69
124600990519     C                   MOVEL     KNSIF         I69SIF
124700990519     C                   MOVEL     KCC           I69KCC
124800990519     C                   MOVEL     TASKSC        I69KAC
124900990519     C                   MOVEL     TASKSC        I69KIN
125000990519     C                   CALL      'TIBS69R'
125100990519     C                   PARM                    DSBS69
125200990519     C                   PARM                    DSACO
125300990519     C                   PARM                    DSIND
125400990519     C                   PARM                    DSCLP
125500990519     C                   PARM                    DSCLS
125600990519    4C     O69ERR        IFNE      ' '
125700990519     C                   SETOFF                                       18
125800990519     C                   ELSE
125900990519     C                   MOVEL     ACORAG        STTRAG           40
126000990519    4C                   ENDIF
126100990519   X3C                   ELSE
126200990623     C     W0040         IFEQ      9999
126300990519     C                   MOVEL(P)  TASRSD        ACORAG
126400990519     C                   MOVEL(P)  TASIND        INDVIA
126500990624     C                   MOVEL(P)  TASCAD        INDCAE
126600990519     C                   MOVEL(P)  TASLOD        INDCIT
126700990624     C                   MOVEL(P)  TASPRD        INDPRV
126800990624     C                   MOVEL(P)  TASNZD        INDSTA
126900990623     C                   SETON                                        18
127000990623     C                   ELSE
127100990623     C                   MOVEL     'M'           §TRC
127200990623     C     KTAA          CHAIN     TITAA30C                           31
127300990623     C  N31              MOVEL(P)  TAARSC        ACORAG
127400990623     C  N31              MOVEL(P)  TAAIND        INDVIA
127500990623     C  N31              MOVEL(P)  TASCAM        INDCAE
127600990623     C  N31              MOVEL(P)  TAALOC        INDCIT
127700990623     C  N31              MOVEL(P)  TAAPRV        INDPRV
127800990623     C  N31              MOVEL(P)  TASNZM        INDSTA
127900990623     C   31              SETOFF                                       18
128000990623     C  N31              SETON                                        18
128100990623     C                   END
128200990519    3C                   ENDIF
128300990519     C*
128400990519     C                   MOVEL     TASTBL        FLGTB1            1
128500990519    3C   18FLGTB1        IFEQ      'F'
128600990519     C                   MOVEL     'MITT.'       DESCLI
128700990519     C                   ELSE
128800990519     C                   MOVEL     'DEST.'       DESCLI            5
128900990519    3C                   ENDIF
129000990519     C*
129100990428     C                   EXCEPT    IASDET
129200990519   X2C                   ELSE
129300990429     C                   EXCEPT    BOLDET
129400920218     C                   EXCEPT    TASDET
129500060110     C   10              EXCEPT    oraDET
129600950213     C   29              EXCEPT    BLKDET
129700990519    2C                   ENDIF
129800950210     C*
129900941118    1C                   END
130000950214     C***********
130100950214    1C   15              DO
130200950214     C  N29              SETON                                        40
130300950214    2C     IND           IFNE      15
130400950217     C                   EXCEPT    MITTES
130500950214     C                   Z-ADD     15            IND               2 0
130600950214     C                   END
130700950214     C                   EXCEPT    BOLDET
130800950217     C                   EXCEPT    MITDET
130900950214     C   29              EXCEPT    BLKDET
131000950214    1C                   END
131100950216     C***********
131200950216    1C   16              DO
131300950216     C  N29              SETON                                        4041
131400950216    2C     IND           IFNE      16
131500950216     C                   EXCEPT    DESTES
131600950216     C                   Z-ADD     16            IND               2 0
131700950216     C                   END
131800950216     C                   EXCEPT    BOLDET
131900950216     C                   EXCEPT    DESDET
132000060110     C   10              EXCEPT    oraDET
132100950216     C   29              EXCEPT    BLKDET
132200950216    1C                   END
132300950216     C***********
132400950216    1C   17              DO
132500950216    2C     IND           IFNE      17
132600950217     C                   EXCEPT    CPTTES
132700950216     C                   Z-ADD     17            IND               2 0
132800950216     C                   END
132900950216     C                   EXCEPT    BOLDET
133000950217     C                   EXCEPT    CPTDET
133100060110     C   10              EXCEPT    oraDET
133200950217     C   29              EXCEPT    BLKDET
133300950216    1C                   END
133400991028     C*
133500991028    1C   23              DO
133600991028    2C     IND           IFNE      23
133700991028     C                   EXCEPT    DESTES
133800991028     C                   Z-ADD     23            IND               2 0
133900991028     C                   END
134000991028     C                   EXCEPT    BOLDET
134100991028     C                   EXCEPT    DESDET
134200060110     C   10              EXCEPT    oraDET
134300991028    1C                   END
134400920218     C*
134500941205     C     ENDSTA        ENDSR
134600941118     C*
134700990622     C* CARICO LE VARIE DELLA BOLLA TITAS ----------------------------*
134800941118     C     VARIE         BEGSR
134900941118     C                   CLEAR                   SVR
135000941118     C                   CLEAR                   VAR
135100950222     C                   MOVE      *ZEROS        A                 3 0
135200950222     C     TASSV1        IFNE      *BLANKS
135300991025     C     TASSV1        ANDNE     §$2BOL
135400991025     C     TASSV1        ANDNE     §$2IVA
135500040203     c**!!!*in14         andeq     *off
135600991027     c* se seconda bolla carico solo la varia "G"
135700040203     c**!!!tassv1        oreq      §$2vrg
135800950222     C                   ADD       1             A
135900950222     C                   MOVEL     TASSV1        SVR(A)
136000950222     C                   Z-ADD     TASVA1        VAR(A)
136100950222     C                   END
136200950222     C     TASSV2        IFNE      *BLANKS
136300991025     C     TASSV2        ANDNE     §$2BOL
136400991025     C     TASSV2        ANDNE     §$2IVA
136500040203     c**!!!*in14         andeq     *off
136600991027     c* se seconda bolla carico solo la varia "G"
136700040203     c**!!!tassv2        oreq      §$2vrg
136800950222     C                   ADD       1             A
136900950222     C                   MOVEL     TASSV2        SVR(A)
137000950222     C                   Z-ADD     TASVA2        VAR(A)
137100990622     C                   END
137200950222     C     TASSV3        IFNE      *BLANKS
137300991025     C     TASSV3        ANDNE     §$2BOL
137400991025     C     TASSV3        ANDNE     §$2IVA
137500040203     c**!!!*in14         andeq     *off
137600991027     c* se seconda bolla carico solo la varia "G"
137700040203     c**!!!tassv3        oreq      §$2vrg
137800950222     C                   ADD       1             A
137900950222     C                   MOVEL     TASSV3        SVR(A)
138000950222     C                   Z-ADD     TASVA3        VAR(A)
138100941118     C*
138200990622     C     KTAS          CHAIN     TITA730C                           31
138300941205     C     *IN31         DOWEQ     *OFF
138400991025     C     TA7SVN        IFNE      §$2BOL
138500991025     C     TA7SVN        ANDNE     §$2IVA
138600950223     C                   ADD       1             A
138700941118     C                   MOVEL     TA7SVN        SVR(A)
138800950217     C                   Z-ADD     TA7VAN        VAR(A)
138900991025     C                   END
139000941118     C*
139100990622     C     KTAS          READE     TITA730C                               31
139200941118     C                   ENDDO
139300950223     C*
139400941118     C                   ENDIF
139500941118     C*
139600941118     C                   ENDSR
139700950210     C*****************************************************************
139800950223     C* VARARB - CARICAMENTO SCHIERE VARIE DA FNARB
139900950210     C*****************************************************************
140000950210     C     VARARB        BEGSR
140100950210     C                   CLEAR                   SVE
140200950210     C                   CLEAR                   SEA
140300950210     C                   CLEAR                   SVC
140400950210     C*
140500950223     C                   MOVE      *ZEROS        A                 3 0
140600950223     C*
140700950223     C     ARBSV1        IFNE      *BLANKS
140800991025     C* NON CARICO SE E' BOLLO O IVA
140900991025     C     ARBSV1        ANDNE     §$2BOL
141000991025     C     ARBSV1        ANDNE     §$2IVA
141100950223     C                   ADD       1             A
141200950223     C                   MOVEL     ARBSV1        SVE(A)
141300950223     C                   Z-ADD     ARBVA1        SEA(A)
141400950223     C                   END
141500950223     C     ARBSV2        IFNE      *BLANKS
141600991025     C     ARBSV2        ANDNE     §$2BOL
141700991025     C     ARBSV2        ANDNE     §$2IVA
141800950223     C                   ADD       1             A
141900950223     C                   MOVEL     ARBSV2        SVE(A)
142000950223     C                   Z-ADD     ARBVA2        SEA(A)
142100950223     C                   END
142200950223     C     ARBSV3        IFNE      *BLANKS
142300991025     C     ARBSV3        ANDNE     §$2BOL
142400991025     C     ARBSV3        ANDNE     §$2IVA
142500950223     C                   ADD       1             A
142600950223     C                   MOVEL     ARBSV3        SVE(A)
142700950223     C                   Z-ADD     ARBVA3        SEA(A)
142800950223     C*
142900991025     C     KARBU         SETLL     FIARBU1E
143000991025     C     KARBU         READE     FIARBU1E                               31
143100950210     C     *IN31         DOWEQ     *OFF
143200991025     C     ARBSVN        IFNE      §$2BOL
143300991025     C     ARBSVN        ANDNE     §$2IVA
143400950223     C                   ADD       1             A
143500950210     C                   MOVEL     ARBSVN        SVE(A)
143600950217     C                   Z-ADD     ARBVAN        SEA(A)
143700991025     C                   END
143800991025     C     KARBU         READE     FIARBU1E                               31
143900950210     C                   ENDDO
144000950223     C*
144100950210     C                   ENDIF
144200950210     C*
144300950210     C                   MOVEA     SVE           SVC
144400991027     d*
144500950210     C*
144600991029     C                   ENDSR
144700991026      **********************************************************************
144800991026      * EDITO IMPORTI PER CAMPI in stampa
144900991026      **********************************************************************
145000991026     C     EDTIMPORTI    BEGSR
145100991026
145200991026      * -----------------------------------------------------------------------------------
145300991026      * W0133      contiene il numero da editare
145400991026      * WTSTDEC    corrisponde a §CVfdc della tabella CV, vale "S" se il num
145500991026      *            ero ha decimali
145600991026      * wedtcde    contiene il codice di editazione da usare
145700991026      * IMPEDITATO ritorna il valore editato per la stampa lungo13 (troncate
145800991026      *            cifre oltre 10 mld)
145900991026      * -----------------------------------------------------------------------------------
146000991026
146100991026     c                   clear                   IMPeditato
146200991029     c                   clear                   W014A
146300991029     c                   select
146400991029     c     wedtcde       wheneq    '4'
146500991026     c                   if        Wtstdec = 'S'
146600991029     C                   eval      W014a = %EDITC(W0133:'4')
146700991026     c                   else
146800991029     c                   z-add     W0133         W0140
146900991029     C                   eval      W014a = %EDITC(W0140:'4')
147000991026     c                   endif
147100991029     c                   other
147200991029     c                   if        Wtstdec = 'S'
147300991029     C                   eval      W014a = %EDITC(W0133:'3')
147400991029     c                   else
147500991029     c                   z-add     W0133         W0140
147600991029     C                   eval      W014a = %EDITC(W0140:'3')
147700991029     c                   endif
147800991029     c
147900991029     c                   endsl
148000991026
148100991029     c                   move      W014a         IMPeditato
148200991026
148300991026     c                   ENDSR
148400941205     C*
148500941205     C* VEDO SE IN TARIFFE C'E' CONSEGNA PARTICOLARE ----------------*
148600941205     C     CTRPAR        BEGSR
148700941205     C*
148800941205     C                   MOVE      TASKSC        KSC4              4 0
148900941205     C*
149000941205     C* SE E' 8888 O 9999 ERRORE SUBITO PERCHE' UTILIZZO TARIFFE CART
149100941205    2C     KSC4          IFEQ      9999
149200941205     C     KSC4          OREQ      8888
149300950228     C     TASFTC        IFNE      *BLANKS
149400950228     C                   SETON                                        48
149500950228     C                   END
149600950228     C     TASTC2        IFNE      *BLANKS
149700950228     C                   SETON                                        49
149800950228     C                   END
149900941205     C*
150000941205   X2C                   ELSE
150100941205     C*
150200941205     C                   Z-ADD     999           TARPRG
150300941205     C                   Z-ADD     999           PREPRG
150400941205     C                   MOVEL     §3ATB1        UNOTB1            1
150500941205     C*
150600941205     C* CIO' CHE NON DEVE ESSERE LA TARIFFA (SICURAMENTE UN FRANCO
150700941205     C*  NON PUO' AVERE IN TARIFFA A DI ARRIVO)
150800941205    3C     UNOTB1        IFEQ      'F'
150900941205     C                   MOVEL     'A'           TFP               1
151000941205     C                   ELSE
151100941205     C                   MOVEL     'F'           TFP
151200941205    3C                   END
151300941205     C*
151400941205     C* SE C'E' A ZERO NON DO' L'ERRORE
151500941205     C     KTAM          SETLL     TNTAM000
151600941205     C     KTAM          READE     TNTAM000                               35
151700941205     C*
151800941205    3C     *IN35         DOWEQ     *OFF
151900941205     C*
152000941205     C* ESCLUDO SE ANNULLATO  BLOCCATA O PER ARRIVI
152100941205    4C     TAMATB        IFNE      'A'
152200941205     C*
152300941205    5C     TAMBAP        IFNE      'B'
152400941205     C     TAMBAP        ANDNE     TFP
152500941205     C*
152600941205     C                   MOVE      TAMPRG        TARPRG            3 0
152700941205     C*
152800941205     C* VEDO SE E' TARIFFA VALIDA
152900941205    6C     TASDSP        IFGE      TAMDDT
153000941205     C     TASDSP        ANDLE     TAMDST
153100941205     C                   SETON                                        35
153200941205    6C                   END
153300941205     C*
153400941205   X5C                   ELSE
153500941205     C                   MOVE      TAMPRG        PREPRG            3 0
153600941205    5C                   END
153700941205    4C                   END
153800941205     C*
153900941205     C  N35KTAM          READE     TNTAM000                               35
154000941205    3C                   END
154100941205     C*
154200941205     C* NON TROVATA TARIFFA: STAMPO ERRORE
154300941205    3C     TARPRG        IFEQ      999
154400941205     C     PREPRG        ANDEQ     999
154500950228     C     TASFTC        IFNE      *BLANKS
154600950228     C                   SETON                                        48
154700950228     C                   END
154800950228     C     TASTC2        IFNE      *BLANKS
154900950228     C                   SETON                                        49
155000950228     C                   END
155100941205   X3C                   ELSE
155200941205     C*
155300941205    4C     TARPRG        IFNE      999
155400941205     C                   MOVEL     TARPRG        PRG
155500941205   X4C                   ELSE
155600941205     C                   MOVEL     PREPRG        PRG
155700941205    4C                   ENDIF
155800950215     C*
155900950215     C     ARBTC1        IFNE      *BLANKS
156000990719     C                   MOVEL(P)  ARBTC1        COMFTC
156100950215     C                   EXSR      CTRTC1
156200950215     C                   END
156300950215     C     ARBTC2        IFNE      *BLANKS
156400990719     C                   MOVEL(P)  ARBTC2        COMFTC
156500950215     C                   EXSR      CTRTC2
156600950215     C                   END
156700941205     C*
156800941205    3C                   END
156900941205    2C                   END
157000941205     C*
157100941205     C                   ENDSR
157200950215     C* CTRTC1 - CONTROLLO CONSEGNA PARTICOLARE 1 --------------------*
157300950215     C     CTRTC1        BEGSR
157400950215     C*
157500950215     C* Vedo se tariffa particolare a 0
157600990719     C     KTAM2         CHAIN     TITPT000                           48
157700950215     C* 49 ON - non esiste tariffa
157800950215    1C     *IN48         IFEQ      *OFF
157900950215     C*         NON ESISTE PERCHE' ANNULLATA
158000950215    2C     TPTATB        IFEQ      'A'
158100950215     C                   SETON                                        48
158200950215   X2C                   ELSE
158300950215     C* Vedo se esiste dettaglio tariffa particolare
158400990719     C     KTAM2         SETLL     TITPD000
158500990719     C     KTAM2         READE     TITPD000                               35
158600950215     C*
158700950215    3C     *IN35         DOWEQ     *OFF
158800950215     C* No annullato
158900950215    4C     TPDATB        IFEQ      ' '
159000950215     C* Se > 0 errore
159100950215    5C     TPDITR        IFNE      0
159200950215     C     TPDMIN        ORNE      0
159300950215     C     TPDMAX        ORNE      0
159400950215     C                   SETON                                        4835
159500950215    5C                   END
159600950215    4C                   END
159700950215     C*
159800990719     C  N35KTAM2         READE     TITPD000                               35
159900950215    3C                   END
160000950215    2C                   END
160100950215    1C                   END
160200950215     C*
160300950215     C                   ENDSR
160400950215     C* CTRTC2 - CONTROLLO CONSEGNA PARTICOLARE 2 --------------------*
160500950215     C     CTRTC2        BEGSR
160600950215     C*
160700950215     C* Vedo se tariffa particolare a 0
160800990719     C     KTAM2         CHAIN     TITPT000                           49
160900950215     C* 49 ON - non esiste tariffa
161000950215    1C     *IN49         IFEQ      *OFF
161100950215     C*         NON ESISTE PERCHE' ANNULLATA
161200950215    2C     TPTATB        IFEQ      'A'
161300950215     C                   SETON                                        49
161400950215   X2C                   ELSE
161500950215     C* Vedo se esiste dettaglio tariffa particolare
161600990719     C     KTAM2         SETLL     TITPD000
161700990719     C     KTAM2         READE     TITPD000                               35
161800950215     C*
161900950215    3C     *IN35         DOWEQ     *OFF
162000950215     C* No annullato
162100950215    4C     TPDATB        IFEQ      ' '
162200950215     C* Se > 0 errore
162300950215    5C     TPDITR        IFNE      0
162400950215     C     TPDMIN        ORNE      0
162500950215     C     TPDMAX        ORNE      0
162600950215     C                   SETON                                        4935
162700950215    5C                   END
162800950215    4C                   END
162900950215     C*
163000990719     C  N35KTAM2         READE     TITPD000                               35
163100950215    3C                   END
163200950215    2C                   END
163300950215    1C                   END
163400950215     C*
163500950215     C                   ENDSR
163600950214     C*
163700950214     C*--- RICERCA CAPOCONTO
163800950214     C     CAPOC         BEGSR
163900950214     C                   DO        50            X                 2 0
164000950214     C                   MOVE      TCU(X)        TCUDS
164100950214     C     F56           CABNE     'CG'          END1
164200950214     C* F4 =1  => CLIENTE
164300950214     C     F4            IFEQ      '1'
164400950214     C* F3='0' => ITALIA
164500950214     C     F3            ANDEQ     '0'
164600950215     C                   Z-ADD     KCU(X)        KCC
164700950214     C                   END
164800950214     C     END1          TAG
164900950214     C                   END
165000950214     C                   ENDSR
165100941118     C*
165200920218     OPRTF198   E            TESTA            02
165300920218     O                       TES(1)              66
165400920218     O                    N02TES(2)             132
165500950224     O                     02TES(10)            132
165600920218     O                       TES(3)             198
165700920122     O                       RSUT                20
165800950217     O                       VIDDAL             100 '  /  /    '
165900950217     O                     02VIDAL              114 '  /  /    '
166000950301     O                       UTIME              166 '0 :  :  '
166100920122     O                       PAGE          Z    198
166200920218     O          E            TESTA            04
166300950227     O                 N08   TES(4)              66
166400961022     O                 N08   VIDFEV              13
166500961022     O                 N08                       14 '-'
166600950227     O                 N08   DESFEV            +  0
166700960930     O                  10   COS102            +  1
166800920219     O                     03                   130 '- R I S T A M P A -'
166900950227     O                     08                   130 '- R I S T A M P A -'
167000920219     O*
167100950216     O          E       13   TESTA            06
167200990429     O              N63      TAS(13)             66
167300990429     O              N63      TAS(4)             132
167400990429     O              N63      TAS(7)             198
167500990429     O               63      TAS(20)             66
167600990429     O               63      TAS(25)            132
167700990429     O               63      TAS(22)            198
167800950216     O          E       13   TESTA       0  0
167900990429     O              N63      TAS(13)             66
168000990429     O              N63      TAS(4)             132
168100990429     O              N63      TAS(7)             198
168200990429     O               63      TAS(20)             66
168300990429     O               63      TAS(25)            132
168400990429     O               63      TAS(22)            198
168500950216     O          E       13   TESTA            07
168600990429     O              N63      TAS(14)             66
168700990429     O              N63      TAS(5)             132
168800990429     O              N63      TAS(8)             198
168900990429     O               63      TAS(21)             66
169000990429     O               63      TAS(25)            132
169100990429     O               63      TAS(23)            198
169200950216     O          E       13   TESTA       0  2
169300990429     O              N63      TAS(14)             66
169400990429     O              N63      TAS(5)             132
169500990429     O              N63      TAS(8)             198
169600990429     O               63      TAS(21)             66
169700990429     O               63      TAS(25)            132
169800990429     O               63      TAS(23)            198
169900920219     O          E       11   TESTA            06
170000920219     O                       DES(13)             66
170100930303     O                     05DES(4)             132
170200930303     O                     05DES(7)             198
170300950216     O                    N05                    66 '                 '
170400950216     O                     06DES(15)            132
170500950216     O                     06DES(18)            198
170600950216     O                     07DES(23)            132
170700950216     O                     07DES(18)            198
170800061129     O                     24CDF(1)             132
170900920219     O          E       11   TESTA       0  0
171000920219     O                       DES(13)             66
171100930303     O                     05DES(4)             132
171200930303     O                     05DES(7)             198
171300950216     O                    N05                    66 '                 '
171400950216     O                     06DES(15)            132
171500950216     O                     06DES(18)            198
171600950216     O                     07DES(23)            132
171700950216     O                     07DES(18)            198
171800061129     O                     24CDF(1)             132
171900920219     O          E       11   TESTA            07
172000920219     O                       DES(14)             66
172100930303     O                     05DES(5)             132
172200930303     O                     05DES(8)             198
172300950216     O                    N05                    66 '                 '
172400950216     O                     06DES(16)            132
172500950216     O                     06DES(19)            198
172600950216     O                     07DES(24)            132
172700950216     O                     07DES(19)            198
172800061129     O                     24CDF(2)             132
172900920219     O          E       11   TESTA       0  2
173000920219     O                       DES(14)             66
173100930303     O                     05DES(5)             132
173200930303     O                     05DES(8)             198
173300950216     O                    N05                    66 '                 '
173400950216     O                     06DES(16)            132
173500950216     O                     06DES(19)            198
173600950216     O                     07DES(24)            132
173700950216     O                     07DES(19)            198
173800061129     O                     24CDF(2)             132
173900920219     O          E       12   TESTA            06
174000920219     O                       CBL(13)             66
174100920219     O                       CBL(4)             132
174200920219     O                       CBL(7)             198
174300920219     O          E       12   TESTA       0  0
174400920219     O                       CBL(13)             66
174500920219     O                       CBL(4)             132
174600920219     O                       CBL(7)             198
174700920219     O          E       12   TESTA            07
174800920219     O                       CBL(14)             66
174900920219     O                       CBL(5)             132
175000920219     O                       CBL(8)             198
175100920219     O          E       12   TESTA       0  2
175200920219     O                       CBL(14)             66
175300920219     O                       CBL(5)             132
175400920219     O                       CBL(8)             198
175500950216     O          E       15   TESTA            06
175600950216     O                       MIT(13)             66
175700950216     O                       MIT(4)             132
175800950216     O                       MIT(7)             198
175900950216     O          E       15   TESTA       0  0
176000950216     O                       MIT(13)             66
176100950216     O                       MIT(4)             132
176200950216     O                       MIT(7)             198
176300950216     O          E       15   TESTA            07
176400950216     O                       MIT(14)             66
176500950216     O                       MIT(5)             132
176600950216     O                       MIT(8)             198
176700950216     O          E       15   TESTA       0  2
176800950216     O                       MIT(14)             66
176900950216     O                       MIT(5)             132
177000950216     O                       MIT(8)             198
177100950216     O          E       16   TESTA            06
177200950216     O                       DES(13)             66
177300950216     O                                           66 '                 '
177400950216     O                       DES(26)            132
177500950216     O                       DES(18)            198
177600950216     O          E       16   TESTA       0  0
177700950216     O                       DES(13)             66
177800950216     O                                           66 '                 '
177900950216     O                       DES(26)            132
178000950216     O                       DES(18)            198
178100950216     O          E       16   TESTA            07
178200950216     O                       DES(14)             66
178300950216     O                                           66 '                 '
178400950216     O                       DES(27)            132
178500950216     O                       DES(19)            198
178600950216     O          E       16   TESTA       0  2
178700950216     O                       DES(14)             66
178800950216     O                                           66 '                 '
178900950216     O                       DES(27)            132
179000950216     O                       DES(19)            198
179100950217     O          E       17   TESTA            06
179200950217     O                       CPT(13)             66
179300950217     O                       CPT(4)             132
179400950217     O                       CPT(7)             198
179500950216     O          E       17   TESTA       0  0
179600950216     O                       CPT(13)             66
179700950216     O                       CPT(4)             132
179800950216     O                       CPT(7)             198
179900950216     O          E       17   TESTA            07
180000950216     O                       CPT(14)             66
180100950216     O                       CPT(5)             132
180200950216     O                       CPT(8)             198
180300950216     O          E       17   TESTA       0  2
180400950216     O                       CPT(14)             66
180500950216     O                       CPT(5)             132
180600950216     O                       CPT(8)             198
180700991028     O          E       23   TESTA            06
180800991028     O                       DES(13)             66
180900991028     O                                           66 '                 '
181000991028     O                       DES(30)            132
181100991028     O                       DES(18)            198
181200991028     O          E       23   TESTA       0  0
181300991028     O                       DES(13)             66
181400991028     O                                           66 '                 '
181500991028     O                       DES(30)            132
181600991028     O                       DES(18)            198
181700991028     O          E       23   TESTA            07
181800991028     O                       DES(14)             66
181900991028     O                                           66 '                 '
182000991028     O                       DES(32)            132
182100991028     O                       DES(19)            198
182200991028     O          E       23   TESTA       0  2
182300991028     O                       DES(14)             66
182400991028     O                                           66 '                 '
182500991028     O                       DES(32)            132
182600991028     O                       DES(19)            198
182700920218     O*
182800950217     O          E    13      TASTES      1  0
182900920218     O                       TAS(1)              66
183000920218     O                       TAS(4)             132
183100920218     O                       TAS(7)             198
183200950217     O          E    13      TASTES      0  1
183300920219     O                       TAS(1)              66
183400920219     O                       TAS(4)             132
183500920219     O                       TAS(7)             198
183600950217     O          E    13      TASTES      0  0
183700920218     O                       TAS(2)              66
183800920218     O                       TAS(5)             132
183900920218     O                       TAS(8)             198
184000950217     O          E    13      TASTES      0  1
184100920219     O                       TAS(2)              66
184200920219     O                       TAS(5)             132
184300920219     O                       TAS(8)             198
184400930303     O          E          05DESTES      1  0
184500920218     O                       DES(1)              66
184600920218     O                       DES(4)             132
184700920218     O                       DES(7)             198
184800930303     O          E          05DESTES      0  1
184900920219     O                       DES(1)              66
185000920219     O                       DES(4)             132
185100920219     O                       DES(7)             198
185200930303     O          E          05DESTES      0  0
185300920218     O                       DES(2)              66
185400920218     O                       DES(5)             132
185500920218     O                       DES(8)             198
185600930303     O          E          05DESTES      0  2
185700920219     O                       DES(2)              66
185800920219     O                       DES(5)             132
185900920219     O                       DES(8)             198
186000950213     O          E          06DESTES      1  0
186100930303     O                       DES(15)            132
186200930303     O                       DES(18)            198
186300950213     O          E          06DESTES      0  1
186400930303     O                       DES(15)            132
186500930303     O                       DES(18)            198
186600950213     O          E          06DESTES      0  0
186700930303     O                       DES(16)            132
186800930303     O                       DES(19)            198
186900950213     O          E          06DESTES      0  2
187000950213     O                       DES(16)            132
187100950213     O                       DES(19)            198
187200950213     O          E          07DESTES      1  0
187300950216     O                       DES(23)            132
187400950213     O                       DES(18)            198
187500950213     O          E          07DESTES      0  1
187600950216     O                       DES(23)            132
187700950213     O                       DES(18)            198
187800950213     O          E          07DESTES      0  0
187900950216     O                       DES(24)            132
188000950213     O                       DES(19)            198
188100950213     O          E          07DESTES      0  2
188200950216     O                       DES(24)            132
188300950213     O                       DES(19)            198
188400061129     O          E          24DESTES      1  0
188500061129     O                       CDF(1)             132
188600061129     O          E          24DESTES      0  1
188700061129     O                       CDF(1)             132
188800061129     O          E          24DESTES      0  0
188900061129     O                       CDF(2)             132
189000061129     O          E          24DESTES      0  2
189100061129     O                       CDF(2)             132
189200950217     O*
189300950217     O          E          16DESTES      1  0
189400950217     O                       DES(26)            132
189500950217     O                       DES(18)            198
189600950217     O          E          16DESTES      0  1
189700950217     O                       DES(26)            132
189800950217     O                       DES(18)            198
189900950217     O          E          16DESTES      0  0
190000950217     O                       DES(27)            132
190100950217     O                       DES(19)            198
190200950217     O          E          16DESTES      0  2
190300950217     O                       DES(27)            132
190400950217     O                       DES(19)            198
190500991028     O*
190600991028     O          E          23DESTES      1  0
190700991028     O                       DES(30)            132
190800991028     O                       DES(18)            198
190900991028     O          E          23DESTES      0  1
191000991028     O                       DES(30)            132
191100991028     O                       DES(18)            198
191200991028     O          E          23DESTES      0  0
191300991028     O                       DES(32)            132
191400991028     O                       DES(19)            198
191500991028     O          E          23DESTES      0  2
191600991028     O                       DES(32)            132
191700991028     O                       DES(19)            198
191800920218     O*
191900920219     O          E            CBLTES      1  0
192000920218     O                       CBL(1)              66
192100920218     O                       CBL(4)             132
192200920218     O                       CBL(7)             198
192300920219     O          E            CBLTES      0  1
192400920219     O                       CBL(1)              66
192500920219     O                       CBL(4)             132
192600920219     O                       CBL(7)             198
192700920219     O          E            CBLTES      0  0
192800920218     O                       CBL(2)              66
192900920218     O                       CBL(5)             132
193000920218     O                       CBL(8)             198
193100920219     O          E            CBLTES      0  2
193200920219     O                       CBL(2)              66
193300920219     O                       CBL(5)             132
193400920219     O                       CBL(8)             198
193500950217     O*
193600950217     O          E            MITTES      1  0
193700950217     O                       MIT(1)              66
193800950217     O                       MIT(4)             132
193900950217     O                       MIT(7)             198
194000950217     O          E            MITTES      0  1
194100950217     O                       MIT(1)              66
194200950217     O                       MIT(4)             132
194300950217     O                       MIT(7)             198
194400950217     O          E            MITTES      0  0
194500950217     O                       MIT(2)              66
194600950217     O                       MIT(5)             132
194700950217     O                       MIT(8)             198
194800950217     O          E            MITTES      0  2
194900950217     O                       MIT(2)              66
195000950217     O                       MIT(5)             132
195100950217     O                       MIT(8)             198
195200950217     O*
195300950217     O          E            CPTTES      1  0
195400950217     O                       CPT(1)              66
195500950217     O                       CPT(4)             132
195600950217     O                       CPT(7)             198
195700950217     O          E            CPTTES      0  1
195800950217     O                       CPT(1)              66
195900950217     O                       CPT(4)             132
196000950217     O                       CPT(7)             198
196100950217     O          E            CPTTES      0  0
196200950217     O                       CPT(2)              66
196300950217     O                       CPT(5)             132
196400950217     O                       CPT(8)             198
196500950217     O          E            CPTTES      0  2
196600950217     O                       CPT(2)              66
196700950217     O                       CPT(5)             132
196800950217     O                       CPT(8)             198
196900920218     O*
197000920219     O          E            BOLDET      0  0
197100990429     O               13N63   TAS(10)             66
197200990429     O               13N63   TAS(11)            132
197300990429     O               13N63   TAS(12)            198
197400920219     O*
197500950216     O                  11   DES(10)             66
197600950216     O                  11 05DES(11)            132
197700950216     O                  11 05DES(12)            198
197800950216     O                  11 06DES(21)            132
197900950216     O                  11 06DES(22)            198
198000950216     O                  11 07DES(24)            132
198100950216     O                  11 07DES(22)            198
198200061129     O                  11 24CDF(2)             132
198300920219     O*
198400920219     O                     12CBL(10)             66
198500920219     O                     12CBL(11)            132
198600920219     O                     12CBL(12)            198
198700950216     O*
198800950216     O                     15MIT(10)             66
198900950216     O                     15MIT(11)            132
199000950216     O                     15MIT(12)            198
199100950216     O*
199200950216     O                     16DES(10)             66
199300991028     O                     16DES(29)            132
199400950216     O                     16DES(22)            198
199500950216     O*
199600950216     O                     17CPT(10)             66
199700950216     O                     17CPT(11)            132
199800950216     O                     17CPT(12)            198
199900991028     O*
200000991028     O                     23DES(10)             66
200100991028     O                     23DES(32)            132
200200991028     O                     23DES(22)            198
200300950216     O*
200400950210     O                       STAAAS               2
200500991027     O                       ARBLNP        Z      6
200600991027     O                       ARBNRS        Z      9
200700920218     O                 N22                       10 '-'
200800991027     O                       ARBNSP        Z     17
200900991027     O                 N12   ARBCBO              20
201000991027     O                       ARBLNA        Z     24
201100991027     O                       ARBCVB              27
201200950216     O                                           28 '-'
201300950216     O                       DESCVB              48
201400950217     O          E    29 13   TASDET         1
201500950217     O                       TASKSC        Z     56
201600950217     O                       TASCTR              60
201700991027     O              N14      prtqft              75
201800991102     O                       tasdiv              79
201900991027     O              N14      prtpor              92
202000991027     O                       svs(1)             104
202100040203     O                       svs(2)             116
202200040203     O                       svs(3)             128
202300040203     O                       svs(4)             140
202400991027     O                       TASFEI             143
202500950324     O                       TASFIV             148
202600950217     O                       TASNFT        Z    156
202700950217     O                       STTDFT            +  1 '  /  '
202800941118     O*
202900941118     O          E       13   TASDET         1
203000920218     O               29      TAS(10)             66
203100920218     O               29      TAS(11)            132
203200920218     O               29      TAS(12)            198
203300950217     O                  40   ARBKSC        Z     56
203400950217     O                  41   ARBCTR              60
203500991027     O                  48   prfQFT              75
203600991027     O                  44   ARBDIV              79
203700991027     O                  45   prfPOR              92
203800040203     O**!!!          14 49   svp(1)             104
203900040203     O                       svp(1)             104
204000040203     O                       svp(2)             116
204100040203     O                       svp(3)             128
204200040203     O                       svp(4)             140
204300991027     O                       ARBCEI             143
204400950324     O                  54   ARBFIV             148
204500950306     O                  56   ARBNFT        3    156
204600950306     O                  57   STADFT            +  1 '  /  '
204700920218     O                       ARBDER             198
204800950217     O          E    29 13 50TASDET         1
204900991028     O                                           42 'Altre varie:'
205000950222     O                                           49 ''''
205100991027     O                       SVS(5)              60
205200950222     O                                         +  0 ''''
205300991027     O                       SVS(6)            +  0
205400950222     O                                         +  0 ''''
205500991027     O                       SVS(7)            +  0
205600950222     O                                         +  0 ''''
205700991027     O                       SVS(8)            +  0
205800950222     O                                         +  0 ''''
205900991027     O                       SVS(9)            +  0
206000950222     O                                         +  0 ''''
206100991027     O                       SVS(10)           +  0
206200950222     O                                         +  0 ''''
206300991027     O                       SVS(11)           +  0
206400950222     O                                         +  0 ''''
206500991027     O                       SVS(12)           +  0
206600950222     O                                         +  0 ''''
206700950217     O          E       13 50TASDET         1
206800991028     O                                           42 'Altre varie:'
206900991027     O                                           49 ''''
207000991027     O                       SVP(5)              60
207100991027     O                                         +  0 ''''
207200991027     O                       SVP(6)            +  0
207300991027     O                                         +  0 ''''
207400991027     O                       SVP(7)            +  0
207500991027     O                                         +  0 ''''
207600991027     O                       SVP(8)            +  0
207700991027     O                                         +  0 ''''
207800991027     O                       SVP(9)            +  0
207900991027     O                                         +  0 ''''
208000991027     O                       SVP(10)           +  0
208100991027     O                                         +  0 ''''
208200991027     O                       SVP(11)           +  0
208300991027     O                                         +  0 ''''
208400991027     O                       SVP(12)           +  0
208500991027     O                                         +  0 ''''
208600920218     O*
208700990429     O          E    11N05N06DESDET         1
208800061129     O         AND  N07N24
208900990624     O          E    29 05N60DESDET         1
209000990624     O                       TASRSD              84
209100990624     O                       TASIND            +  1
209200990624     O                       TASCAD            +  1
209300990624     O                       TASNZD            +  1
209400990624     O                       ST1FFD            +  2
209500930303     O*
209600950213     O          E    29 06   DESDET         1
209700950216     O                       TASFVB              69
209800950216     O                       TASVLB        2     76
209900950216     O                       TASFVF              80
210000950216     O                       TASVLF        2     87
210100950220     O                       TASNFT        Z    152
210200930303     O                       STTDFT            +  3 '  /  '
210300950213     O          E    29 07   DESDET         1
210400950216     O                       TASPKB        2     77
210500950220     O                       TASNFT        Z    152
210600950213     O                       STTDFT            +  3 '  /  '
210700990624     O          E    29 05N60DESDET         1
210800990624     O                       TASLOD              84
210900990624     O                       TASPRD            +  1
211000990624     O                                         +  0 '-'
211100990624     O                       TASFIN            +  0
211200990624     O                                         +  0 '-'
211300990624     O                                         +  2 'P.IVA'
211400990624     O                       TASCPD            +  1
211500990624     O                       TASNFT        Z    156
211600990624     O                       STTDFT             162 '  /  '
211700950216     O*
211800930303     O          E       05   DESDET         1
211900920218     O               29      DES(10)             66
212000920218     O               29      DES(11)            132
212100920218     O               29      DES(12)            198
212200950216     O                  40   ARBRSD              84
212300941118     O                  40   ARBIND            +  1
212400941118     O                  40   ARBCAD            +  1
212500950222     O                       ST2FFD            +  6
212600950216     O          E       05   DESDET         1
212700060110     O                       WRBDTV              36 '  /  /    '
212800060110     O                       ARBORV              46 '  :  :  '
212900950216     O                  40   ARBLOD              84
213000941118     O                  40   ARBPRD            +  1
213100941118     O                                         +  0 '-'
213200941118     O                       ARBFIN            +  0
213300941118     O                                         +  0 '-'
213400950216     O                  40                     +  2 'P.IVA'
213500950216     O                  40   ARBCPI            +  1
213600930303     O                       ARBDER             198
213700930303     O*
213800950213     O          E       06   DESDET         1
213900930303     O               29      DES(21)            132
214000930303     O               29      DES(22)            198
214100950216     O               48      ARBFVB              69
214200950216     O               48      ARBVLB        2     76
214300950216     O               49      ARBFVF            +  3
214400950216     O               49      ARBVLF        2   +  1
214500920219     O                       ARBDER             198
214600950213     O          E       07   DESDET         1
214700950216     O               29      DES(24)            132
214800950213     O               29      DES(22)            198
214900950217     O                       ARBPKB        4     77
215000950213     O                       ARBDER             198
215100950217     O*
215200950220     O          E       16   DESDET         1
215300950220     O               29      TASFVF              68
215400950220     O               29      TASVLF        4   +  1
215500950220     O                       ARBFVC            +  3
215600950220     O                       ARBVLC        4   +  1
215700950220     O                       TASNFT        Z    152
215800950217     O                       STTDFT            +  3 '  /  '
215900950220     O                       ARBDER             198
216000061129     O*
216100061129     O          E       24   DESDET         1
216200061129     O                       DECOMITDES          81
216300061129     O                       ARBCPI            +  1
216400061129     O                       ARBDER             198
216500991028     O*
216600991028     O          E       23   DESDET         1
216700991028     O                       ARBFPC              67
216800991028     O                       ARBPKC        4   +  1
216900991028     O                       TASNFT        Z    152
217000991028     O                       STTDFT            +  3 '  /  '
217100991028     O                       ARBDER             198
217200990421     O*
217300990421     O          E            CBLDET         1
217400051124     O                    n10tasksc              56
217500051124     O                       ARBCBP              59
217600051124     O                                           60 '-'
217700051124     O                       DESCBP              77
217800051124     O                       OLDTB1              81
217900051124     O                       OLDTB2              84
218000990421     O                       ARBCBN            +  1
218100990421     O                                         +  0 '-'
218200990421     O                       DESCBN            +  0
218300990421     O                       §3ATB1            +  2
218400990421     O                       §3ATB2            +  1
218500990421     O                       CSVCAS        4   +  1
218600990421     O                       CSVVCA            +  1
218700990421     O                       ARBCAS        4   +  1
218800990421     O                       ARBVCA            +  1
218900990421     O                       TASNFT        Z   +  1
219000990421     O                       STTDFT            +  1 '  /  '
219100990421     O                       ARBDER             198
219200920218     O*
219300990421     O          E            IASDET         1
219400990429     O                       TAS(24)             66
219500990429     O                       TAS(25)            132
219600990429     O                       TAS(26)            198
219700990429     O                       STAAAS               2
219800990429     O                       ARBLNP        Z      6
219900990429     O                       ARBNRS        Z      9
220000990429     O                 N22                       10 '-'
220100990429     O                       ARBNSP        Z     17
220200990519     O                       TASTBL              20
220300990429     O                       ARBLNA        Z     24
220400990429     O                       ARBCVB              27
220500990429     O                                           28 '-'
220600990429     O                       DESCVB              48
220700990421     O                                           77 'Imp. Assicur. PRECEDENTE'
220800990421     O                       ARBIAP        4   +  1
220900990421     O                       ARBVAP            +  1
221000990429     O                                         +  1 'Imp. Assicur. NUOVO'
221100990421     O                       ARBIAS        4   +  1
221200990421     O                       ARBVAS            +  1
221300990519     O                       STTDSP             148 '  /  /    '
221400990511     O                       TASNFT        Z    156
221500990511     O                       STTDFT            +  1 '  /  '
221600990429     O                       ARBDER             198
221700990519     O          E    18      IASDET         2
221800990519     O                       DESCLI              30
221900990519     O                       ACORAG            +  0
222000990519     O                       INDVIA            +  0
222100990519     O                       INDCIT            +  0
222200990519     O                       INDPRV            +  1
222300990519     O                       INDCAE            +  1
222400990519     O                       INDSTA            +  0
222500990519     C*
222600950217     O          E    29      MITDET         1
222700950222     O                       TASKSC              56
222800990624     O                       TAARSC            +  1
222900990624     O                       TAAIND            +  2
223000950222     O                       TASCAM            +  1
223100950222     O                       TASNZM            +  1
223200950222     O                       TASFAP            +  3
223300950217     O          E    29      MITDET         1
223400990624     O                       TAALOC              92
223500990624     O                       TAAPRV            +  2
223600950222     O                                         +  2 'P.IVA'
223700990624     O                       TAACPI            +  1
223800950217     O                       TASNFT        Z    156
223900950217     O                       STTDFT             162 '  /  '
224000950217     O*
224100950217     O          E            MITDET         1
224200950217     O               29      MIT(10)             66
224300950217     O               29      MIT(11)            132
224400950217     O               29      MIT(12)            198
224500950217     O                  40   ARBCCM              56
224600950217     O                  40   ARBRSM            +  1
224700950217     O                  40   ARBINM            +  2
224800950217     O                  40   ARBCAM            +  1
224900950217     O                  40   ARBNZM            +  1
225000950217     O                  40   ARBFAP            +  3
225100950217     O          E            MITDET         1
225200060110     O                       WRBDTV              36 '  /  /    '
225300060110     O                       ARBORV              46 '  :  :  '
225400950220     O                  40   ARBLOM              93
225500950217     O                  40   ARBPRM            +  1
225600950217     O                  40                     +  2 'P.IVA'
225700950217     O                  40   ARBCPI            +  1
225800950217     O                       ARBDER             198
225900950217     O*
226000950217     O          E    29      CPTDET         1
226100950217     O                       TASFTC              50
226200950217     O                                         +  0 '-'
226300950217     O                       DETFTC            +  0
226400950217     O                       TASTC2            +  1
226500950217     O                                         +  0 '-'
226600950217     O                       DETTC2            +  0
226700950228     O                       TASNFT        Z    152
226800950217     O                       STTDFT            +  3 '  /  '
226900951013     O          E    29      CPTDET      0  0
227000951013     O                       CPT(10)            132
227100951013     O                       CPT(11)            198
227200951013     O                       CPT(12)            198
227300951013     O               48                          61 'MANCA VARIA"'
227400951013     O               48      WVARC1            +  0
227500951013     O               48                        +  0 '"!!'
227600951013     O               49                          78 'MANCA VARIA"'
227700951013     O               49      WVARC2            +  0
227800951013     O               49                        +  0 '"!!'
227900951013     O                                          152 'CLIENTE'
228000951013     O                       TASKSC             160
228100951013     O          E            CPTDET         1
228200950217     O                       ARBDER             198
228300950217     O*
228400060110     O          E    10      oraDET      0  1
228500060110     O                       WRBDTV              36 '  /  /    '
228600060110     O                       ARBORV              46 '  :  :  '
228700920219     O          E            BLKDET         1
228800920219     O                                           66 '  '
228900000710     O          E            PIEDI            64
229000920219     O                       TES(5)              66
229100920219     O                       TES(6)             132
229200000710     O          E            PIEDI            65
229300920219     O                       TES(8)             132
229400950224     O                       TES(9)             198
229500991202     O          E            NOBOL            20
229600991202     O                       cpt(15)            132
229700991202     O          E            NOBOL       0  0
229800991202     O                       cpt(15)            132
229900950216** TES
230000920219XXXXXXXXXXXXXXXXXXXX                         ***  VARIAZIONE  BOLL   1
230100950217E  NON  EFFETTUATE  AL  XX/XX/XXXX ***                               2
230200950213TRTR31R                   XX:XX:XX                       PAG. XXXX   3
230300961022P.O. DI : XXX-XXXXXXXXXXXXXXXXXXXXXXXXX                              4
230400950224PER OGNI SPEDIZIONE CHE PRESENTA PIU' DI UNA RIGA DI STAMPA VENGON   5
230500950224O RIPORTATI: SULLE PRIME RIGHE I DATI DELLA BOLLA DI SEDE            6
230600920219                                                                     7
230700961022             SULLE RIGHE SUCCESSIVE LE VARIAZIONI FATTE DAL PUNTO    8
230800961022OPERATIVO                                                            9
230900950224E  NON  EFFETTUATE  DAL XX/XX/XXXX AL XX/XX/XXXX ***                10
231000920220** TAS                 '
231100991026                                                'CODICE 'COD' QUAN  1
231200991026                                                'CLIENTE'TAR'   FA  2
231300991026XX XXX XX XXXXXXX XX XXX XX-XXXXXXXXXXXXXXXXXXXX XXXXXXX XXX XXXXX  3
231400991026TITA' DA 'DIV' TRASPORTO  '  VARIA 1  '  VARIA 2  '  VARIA 3  '  V  4
231500991026TTURARE  '   '            '           '           '           '     5
231600991026XXXXX,XXX XXX XXXXXXXX,XXX XXXXXXXXXXX XXXXXXXXXXX XXXXXXXXXXX XXX  6
231700991026ARIA 4  ' ES'REG.'   FATTURA  'DESCRIZIONE  ERRORE                  7
231800991026        'IVA'IVA ' NUMERO DATA'                                     8
231900991026XXXXXXXX  X  XXX  XXXXXX XX/XX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  9
232000950220                                                '       '   '      10
232100991026         '   '            '           '           '           '    11
232200991026        '   '    '            '                                    12
232300991027 S P E D I Z I O N E    'CAUSALE VARIAZIONE     'CODICE 'COD' QUAN 13
232400991027AN LNP SR NUMERO  BL LNA                        'CLIENTE'TAR'   FA 14
232500991026                                                                   15
232600991026                                                                   16
232700991026                                                                   17
232800991026                                                                   18
232900991026                                                                   19
233000990429 S P E D I Z I O N E    'CAUSALE VARIAZIONE     '   '              20
233100990429AN LNP SR NUMERO  BL LNA                        '   '              21
233200990519   '     DATA     '  FATTURA  'DESCRIZIONE  ERRORE                 22
233300990519   '  SPEDIZIONE  'NUMERO DATA'                                    23
233400990429                        '                       '   '              24
233500990429           '              '   '                   '              ' 25
233600990429   '              '           '                                    26
233700920218**  DES
233800950216                                                '     D      E      1
233900950216                                                'RAGIONE SOCIALE    2
234000950216XX XXX XX XXXXXXX XX XXX XX-XXXXXXXXXXXXXXXXXXXX'XXXXXXXXXXXXXXXXX  3
234100941118  S      T      I      N      A      T      A      R      I      O  4
234200950216                   INDIRIZZO                              C.A.P. N  5
234300950216XXXXXXXXXXXXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX XXXXXXXXX X  6
234400950216  'FERMO'         '  FATTURA  'DESCRIZIONE  ERRORE                  7
234500950216AZ'DEPOS'         'NUMERO DATA'                                     8
234600051108XX  XX            XXXXXX XX/XX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  9
234700950220                                                '                  10
234800950216                  '                                   '         '  11
234900950216  '     '         '           '                                    12
235000950216 S P E D I Z I O N E    'CAUSALE  VARIAZIONE    '     D      E     13
235100950216AN LNP SR NUMERO  BL LNA'                       'RAGIONE SOCIALE   14
235200950216'  VOLUME  '   VOLUME   '                                          15
235300950216'BOLLETTATO'DA FATTURARE'                                          16
235400950216  X XX,XXX ' X XX,XXX   '
235500930303           '      FATTURA     'DESCRIZIONE  ERRORE                 18
235600930303           '  NUMERO    DATA  '                                    19
235700930303              XXXXXX   XX/XX  'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX 20
235800950216'          '            '                                          21
235900930303           '                  '                                    22
236000950213'    PESO   '                                                      23
236100950213'           '                                                      24
236200950216   XXXXXX,X                                                        25
236300950220'  VOLUME  '  VOLUME  '                                            26
236400950220'PRECEDENTE'   NUOVO  '                                            27
236500950220  X XX,XXX ' X XX,XXX '                                            28
236600950220'          '          '                                            29
236700991028'PESO NUOVO'                                                       30
236800991028'X XXXXXX,X'                                                       31
236900991028'          '                                                       32
237000950216** CBL
237100051124                                                'CODICE '  C O D I  1
237200051124                                                'CLIENTE'     P R   2
237300051124XX XXX XX XXXXXXX XX XXX XX-XXXXXXXXXXXXXXXXXXXX xxxxxxx XX-xxxxxx  3
237400051124 C E   B O L L A  ' C O D I C E   B O L L A ' C  O  N  T  R  A  S   4
237500051124E C E D E N T E   '         N U O V O       '    PRECEDENTE    '    5
237600051124xxxxxxxxxxx  XX XX XX-XXXXXXXXXXXXXXX  XX XX XXXXXXXXXX,XXX XXX XX  6
237700950217 S  E  G  N  O  '   FATTURA   'DESCRIZIONE  ERRORE                  7
237800950217    NUOVO       ' NUMERO DATA '                                     8
237900950217XXXXXXXX,XXX XXX XXXXXX XX/XX  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  9
238000051124                        '                       '       '          10
238100051124           ->     '                  ->     '                  '   11
238200950217                '             '                                    12
238300051124 S P E D I Z I O N E    'CAUSALE  VARIAZIONE    'CODICE '  C O D I 13
238400051124AN LNP SR NUMERO  BL LNA'                       'CLIENTE'     P R  14
238500920217**
238600920220SPEDIZIONE GIA' TASSATA                                             8
238700920217SPEDIZIONE GIA' CONTABILIZZATA                                      9
238800920217C/ASSEGNO GIA' INCASSATO - PAGATO                                   10
238900920220SPEDIZIONE GIA' STORICIZZATA                                        13
239000920220**
239100130327PER UFFICIO ICT Operativo
239200920220PER L'UFFICIO C.E.D.
239300990420PER UFFICIO ASSIC.MERCE
239400950216**  MIT
239500950216                                                '       M       I   1
239600950216                                                'CODICE  RAGIONE    2
239700950217XX XXX XX XXXXXXX XX XXX XX-XXXXXXXXXXXXXXXXXXXX'XXXXXXX XXXXXXXXX  3
239800950216      T       T       E       N       T       E                     4
239900950217SOCIALE                     INDIRIZZO                           C.  5
240000950217XXXXXXXXXXXXXXXXXXXXXXXXXX  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX XX  6
240100950216           'ANTE  '  FATTURA  'DESCRIZIONE  ERRORE                  7
240200950217A.P.    NAZ'PORTO 'NUMERO DATA'                                     8
240300950216XXXXXXX XXX   X   XXXXXX XX/XX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  9
240400950220                                                '       '          10
240500950216                           '                                   '   11
240600950216       '   '     '            '                                    12
240700950216 S P E D I Z I O N E    'CAUSALE  VARIAZIONE    '       M       I  13
240800950216AN LNP SR NUMERO  BL LNA'                       'CODICE  RAGIONE   14
240900950216**  CPT
241000950216                                                'C O N S E G N E    1
241100950216                                                '                   2
241200950216XX XXX XX XXXXXXX XX XXX XX-XXXXXXXXXXXXXXXXXXXX'X-XXXXXXXXXXXXXXX  3
241300950216P A R T I C O L A R I                                               4
241400950216                                                                    5
241500950216 X-XXXXXXXXXXXXXXX                                                  6
241600950216           '      FATTURA     'DESCRIZIONE  ERRORE                  7
241700950216           '  NUMERO    DATA  '                                     8
241800950216              XXXXXX   XX/XX  'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  9
241900950220                                                '                  10
242000950216'                 '                                                11
242100950216           '                  '                                    12
242200950216 S P E D I Z I O N E    'CAUSALE  VARIAZIONE    'C O N S E G N E   13
242300950216AN LNP SR NUMERO  BL LNA'                       '                  14
242400991202                   Nessuna  spedizione  da  segnalare              15
242500061129**  cdf
242600061129'   C O D I C E   F I C A L E   '                                   1
242700061129'                               '                                   2
242800061129' XXXXXXXXXXXXX xxxxxxxxxxxxxxxx'
242900061129XX XXX XX XXXXXXX XX XXX XX-XXXXXXXXXXXXXXXXXXXX'                   4
243000140401** MSG da non stampare
243100140401SPEDIZIONE GIA' CONTABILIZZATA                                      1
243200140401C/ASSEGNO GIA' INCASSATO - PAGATO                                   2
243300140401GIA' CONTABIL.:NO CAMBIO TIPO BOLLA                                 3
243400140401                                                                    4
243500140401                                                                    5
243600140401                                                                    6
243700140401                                                                    7
243800140401                                                                    8
243900140401                                                                    9
244000140401                                                                    10
