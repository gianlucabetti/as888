000100960329     H DECEDIT('0,') DATEDIT(*DMY.)
000200960409     H* FNLR44R *----------------------------------------------------*
000300960404     H*         - GESTIONE ANOMALIE
000400960404     H*--------------------------------------------------------------*
000500960328     FTABEL00F  IF   E           K DISK
000600960329     FAZORG01L  IF   E           K DISK
000700960429     FFNBLP01L  IF   E           K DISK
000800970507     FFNARB01L  IF   E           K DISK
000900060216     FFIAR401L  IF   E           K DISK
001200990512     FFNANM00F  UF A E             DISK
001300990512     FFNANM01L  IF   E           K DISK
001400990512     F                                     RENAME(FNANM000:FNANM001)
001500990512     F                                     INFDS(FNANM1)
001600990512     FFNANM02L  IF   E           K DISK
001700990512     F                                     RENAME(FNANM000:FNANM002)
001800990512     F                                     INFDS(FNANM2)
001900990512     FFNANM04L  IF   E           K DISK
002000990512     F                                     RENAME(FNANM000:FNANM004)
002300090311     FFiART01L  IF   E           K DISK    USROPN
002400090311     FFiART27L  IF   E           K DISK
002500090311     F                                     RENAME(FiART000:FiAR27)
002800960327     FFNLR44D   CF   E             WORKSTN
002900960326     F                                     SFILE(LR44S01:NRR1)
003000960401     F*
003100970507     D MSG             S             78    DIM(40) CTDATA PERRCD(1)             MSG VIDEO
003200960403     D Z01             S             40    DIM(3) CTDATA PERRCD(1)              PIEDI VIDEO
003300960521     D Z02             S             40    DIM(6) CTDATA PERRCD(1)              PIEDI VIDEO
003400960422     D Z03             S             40    DIM(2) CTDATA PERRCD(1)              PIEDI VIDEO
003500960329     D L6S             S              3  0 DIM(30)                              FIL GEST.DA AL
003600960523     D L1              S              3  0 DIM(30)                              FIL.GESTITE
003700961118     D TAB             S              1    DIM(4) CTDATA PERRCD(1)              INT/IDD/NONIDD
003800961118     D TABDES          S             10    DIM(4) ALT(TAB)
003900960405     D SKB             S             17    DIM(500)                             PER CONTR.
004000960405     D SKS             S             18    DIM(500)                             PER CONTR.
004100960405     D WMS             S              1    DIM(78)                              COMODO X MSG
004200960521     D*
004300960521     D DSLR44        E DS                  EXTNAME(FNLR44DS)
004400960402     D DSLS05          DS
004500960402     D  PA0AAS                 1      4  0
004600960402     D  PA0LNP                 5      7  0
004700960402     D  PA0NRS                 8      9  0
004800960402     D  PA0NSP                10     16  0
004900960402     D  PA0F03                17     17
005000960402     D  PA0FLG                18     18
005100960402     D  PA0A9                 19     19
005200960402     D  PA0B5                 20     20
005300960402     D  PA075                 21     21
005400960402     D  PA098                 22     22
005500960402     D  PA0RSU                23     42
005600960402     D  PA0CML                43     43
005700960402     D  PA0SIM                44     46  0
005800960402      * PA0CLI indica se il pgm e' richiamato da un cliente (='S')
005900960402     D  PA0CLI                47     47
006000960402      * PA0GIA indica se il pgm e' richiamato dalle giacenze
006100960402     D  PA0GIA                48     48
006200960403     D DSLR04          DS                  INZ
006300960403     D  PA4NFV                 1      5  0
006400960403     D  PA4LNP                 6      8  0
006500960403     D  PA4LNA                 9     11  0
006600960403     D  PA4NRS                12     13  0
006700960403     D  PA4NCD                14     20  0
006800960403     D  PA4NCA                21     27  0
006900960403     D  PA4NPS                28     29  0
007000960403     D  PA4NPG                30     30  0
007100960403     D  PA4FLG                31     31
007200960403     D  PA4PGM                32     38
007300960403     D  PA4FGS                39     41  0
007400960402     D DSLR36          DS
007500960402     D  PA1AAS                14     17  0
007600960402     D  PA1LNP                18     20  0
007700960402     D  PA1NRS                21     22  0
007800960402     D  PA1NSP                23     29  0
007900960402     D  PA1F03                30     30
008000960402     D  PA1FLG                31     31
008100960402     D  PA1PGM                32     41
008200960402     D  PA1DSA                42     61
008300960402     D  PA1CML                62     62
008400960402     D  PA1DSP                63     70  0
008500960402     D  PA1RMN                71     85  0
008600960402     D  PA1LNA                86     88  0
008700960402     D  PA1LN1                89    108
008800960402     D  PA1RSD               109    143
008900960402     D* SE RICH DA PGM GIAC. = 1
009000960402     D  PA1GIA               144    144
009100960402     D  PA1FEL               145    147  0
009200960402     D  PA1TBO               148    148
009300960326     D KPJBA         E DS
009400960326     D DS7C          E DS
009500960403     D DS7A2         E DS
009600960326     D DS7D          E DS
009700960329     D CNCR80        E DS
009800960326     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
009900990512     D DSANM         E DS                  EXTNAME(FNANM00F)
010000960419     D DSLR33        E DS                  EXTNAME(FNLR33DS)
010100960327     D DSLR34        E DS                  EXTNAME(FNLR34DS)
010200040923     D  ANM                   89    118
010300040923     D                                     DIM(10)                              ANOM.DA FILTRO
010400040531     D DANMFLO       E DS
010500960329     D DSUL06        E DS                  EXTNAME(TRUL06DS)
010600960329     D  LIN                    1     90  0
010700960329     D                                     DIM(30)                              FIL. COMODO
010800971118     D DSLV55        E DS                  EXTNAME(FNLV55DS)
010900031028     d dsbl4a        e ds
011000990512     D FNANM1          DS
011100960401     D  ANMNR1               397    400B 0
011200990512     D FNANM2          DS
011300960401     D  ANMNR2               397    400B 0
011400971118     D WLBDA8          DS
011500971118     D  G08DAT                 1      8  0
011600971118     D  G08INV                 9     16  0
011700971118     D  G08ERR                17     17
011800971118     D  G08TGI                18     22  0
011900000824     D WDT10D          DS
012000000824     D  WDT02G                 1      2
012100000824     D  WTRAT1                 3      3
012200000824     D  WDT02M                 4      5
012300000824     D  WTRAT2                 6      6
012400000824     D  WDT04A                 7     10
012500960329     D*
012600960403     D* DS PER CONTROLLI IN MODO INSERIMENTO
012700960329     D                 DS
012800960329     D  V1SAAS                 1      2  0
012900960329     D  V1SLNP                 3      5  0
013000960329     D  V1SNSP                 6     12  0
013100960402     D  KEYNSP                 1     12  0
013200960403     D  V1SSCN                13     19  0
013300960403     D  V1SFLS                20     22  0
013400960403     D  V1SNRS                23     24  0
013500960403     D  IOFLD                  1     24
013600960405     D                 DS
013700960405     D  WSAAS                  1      2  0
013800960405     D  WSLNP                  3      5  0
013900960405     D  WSNRB                  6      7  0
014000960405     D  WSNSP                  8     14  0
014100960405     D  WSCAB                 15     17
014200960405     D  WKEYB                  1     17
014300960405     D                 DS
014400960405     D  WSFLS                  1      3  0
014500960405     D  WSLNA                  4      6  0
014600960405     D  WSNRS                  7      8  0
014700960405     D  WSSCN                  9     15  0
014800960405     D  WSCAS                 16     18
014900960405     D  WKEYS                  1     18
015000960405     D                 DS
015100990513     D  V2DCCH                 1     23
015200960405     D  W2DCAA                 1      3
015300990513     D  W2DDES                 4     23
015400960411     D                 DS
015500960411     D  V2DFSA                 1     17
015600960411     D  W2CFSA                 1      2
015700960411     D  W2DFSA                 3     17
015800960411     D                 DS
015900960411     D  V2DFSC                 1     17
016000960411     D  W2CFSC                 1      2
016100960411     D  W2DFSC                 3     17
016200960326     D                SDS
016300960326     D  V1DPGM                 1      8
016400041125     D GIRI            s              1  0
016500041125     D WCAAELAB        s                   LIKE(ANMCAA)
016600960521     D TIT1            C                   CONST('***   GESTIONE ANOMA-
016700960521     D                                     LIE   ***')
016800960521     D TIT2            C                   CONST('** INTERROGAZIONE AN-
016900960521     D                                     OMALIE **')
017000040603     D WTRA            C                   CONST('TRASMISSIONE .....:')
017100040603     D WRIC            C                   CONST('RICEZIONE ........:')
017200960327     I*
017300090311     IFIART000
017400960530     I              ARTFLS                      DETFLS
017500960521     I              ARTLNA                      DETLNA
017600960521     I              ARTNRS                      DETNRS
017700960521     I              ARTNSC                      DETNSC
018300090311     IFIAR27
018400960605     I              ARTFLS                      D27FLS
018500960605     I              ARTLNA                      D27LNA
018600960605     I              ARTNRS                      D27NRS
018700960605     I              ARTNSC                      D27NSC
018800960605     I              ARTAAS                      D27AAS
018900960605     I              ARTLNP                      D27LNP
019000960605     I              ARTNSP                      D27NSP
019900960326     C****************************************************************
020000960326     C*  RIEPILOGO INDICATORI
020100960326     C***************************************************************
020200960422     C* 02    -  ON --> MANUTENZIONE NON ABILITATO
020300960422     C* 03    -  ON --> INSERIMENTO NON ABILITATO
020400960402     C* 04    -  PROTEZIONE CAMPI SUBFILE
020500960506     C* 05    -  PROTEZIONE CODICE ANOMALIA
020600960402     C* 06    -  OFF --> ND DESCRIZIONE "ANNULLATA" SU VIDEATA D02
020700960403     C* 07    -  'HI' DI CHIAVE SPEDIZIONE SU VIDEATA D02
020800960403     C* 08    -  'HI' DI CHIAVE SEGNACOLLO SU VIDEATA D02
020900960405     C* 09    -  'HI' CAMPI DELLA CHIAVE IN SUBFILE SE CAMPI SPROTETTI
021000960412     C* 10    -  ON - MODALITA' INSERIMENTO
021100960401     C* 11/19 -  DUP SU CAMPI DI I/O
021200960326     C* 28    -  ERRORE GENERICO VIDEATE
021300960326     C* 30    -  COMODO
021400960402     C* 31    -  COMODO
021500961210     C* 35    -  LETTURA ANOMALIE IN MODALITA' INTERROGAZIONE
021600961210     C* 36    -  LETTURA BOLLA IN MODALITA' INTERROGAZIONE
021700960506     C* 40    -  ERRORE CODICE ANOMALIA
021800960401     C* 41    -  ERRORE NUMERO SPEDIZIONE
021900960521     C* 42    -  ERRORE NUMERO SEGNACOLLO
022000960401     C* 43    -  ERRORE LINEA DI PARTENZA
022100960401     C* 44    -  ERRORE ANNO SPEDIZIONE
022200960401     C* 45    -  ERRORE LINEA DI ARRIVO
022300960401     C* 46    -  ERRORE DATA APERTURA ANOMALIA
022400960401     C* 47    -  ERRORE SU CAMPO OPZIONE
022500960401     C* 48    -  ERRORE SU FILIALE SEGNACOLLO
022600960401     C* 49    -  ERRORE NUMERO SERIE
022700960523     C* 51    -  ND CAMPI KEY SPEDIZIONE/SEGNACOLLO
022800960523     C* 52    -  ND SEGNACOLLO
022900960521     C* 98/99 -  PER I FILE DI TRASMISSIONE
023000971103     C* 90    -  RIEMISSIONE FORMATO VIDEO
023100960521     C* 91    -  INTERROGAZIONE ANOMALIE PER SPEDIZIONE
023200960521     C* 92    -  INTERROGAZIONE ANOMALIE PER SEGNACOLLO
023300960326     C***************************************************************
023400960327     C     *ENTRY        PLIST
023500960327     C                   PARM                    KPJBA
023600960522     C                   MOVEL     KPJBU         DSLR44
023700960523     C* SOLO LA PRIMA VOLTA
023800960523     C     WVOLT         IFEQ      ' '
023900960523     C* CARICO £1
024000960523     C                   CLEAR                   DSUL06
024100960523     C                   MOVE      '£1'          D06COD
024200960523     C                   MOVEL     SIMFEL        D06KEY
024300960523     C                   MOVEL     DSUL06        KPJBU
024400960523     C                   CALL      'TRUL06R'
024500960523     C                   PARM                    KPJBA
024600960523     C                   MOVEL     KPJBU         DSUL06
024700960523     C                   MOVEA     LIN           L1
024800960523     C                   MOVE      '1'           WVOLT             1
024900960523     C                   END
025000960327     C*
025100960521     C                   SELECT
025200960521     C* MODALITA' GESTIONE
025300960521     C     D44MOD        WHENEQ    'G'
025400960521     C                   EXSR      SRGEST
025500960524     C* Emissione subfile se ho finito di caricare la pagina
025600960524    2C     D34END        IFEQ      'S'
025700960524     C     NRR1          OREQ      NRRAL
025800960521     C                   EXSR      SRXFMT
025900960521     C                   END
026000960521     C* MODALITA' INTERROGAZIONE
026100960521     C     D44MOD        WHENEQ    'I'
026200960521     C                   EXSR      SRINTE
026300960521     C* Emissione subfile
026400960521     C                   EXSR      SRXFMT
026500960521     C                   ENDSL
026600960327     C*
026700960326     C     FINE          TAG
026800030717     C*
026900030717     C* VALORIZZO NUOVI CAMPI DELLA DS VALIDI UNICAMENTE COME SELEZIONE IN FASE DI STAMPA
027000030717     C* (ESEGUO UNA VALORIZZAZIONE NEUTRA OVVERO COME SE NN FOSSERO STATE RICHIESTE SELEZIONI)
027100030717     C                   Z-ADD     *ZEROS        D34POS
027200030717     C                   Z-ADD     99            D34NRS
027300030717     C*
027400960327     C                   MOVEL     DSLR34        D44LR
027500960522     C                   MOVEL     DSLR44        KPJBU
027600960521     C*
027700960521     C     D44MOD        IFEQ      'G'
027800960327     C                   SETON                                        RT
027900960521     C                   ELSE
028000971118     C                   MOVEL     'C'           D55TLA
028100971118     C                   CALL      'FNLV55R'
028200971118     C                   PARM                    DSLV55
028300960521     C                   SETON                                        LR
028400960521     C                   END
028500960521     C*
028600960521     C****************   MODALITA' GESTIONE ANOMALIE  *****************
028700960521     C     SRGEST        BEGSR
028800960521     C                   MOVEL     D44ANM        DSANM
028900960521     C                   MOVEL     D44LR         DSLR34
029000970214     C* VERIFICO SE RICHIESTA ANOMALIA 300-NR.RITIRI EFFETTUATI
029100970214     C     '300'         LOOKUP    ANM                                    31
029200040923     c     *in31         ifeq      *on
029300040923     C                   MOVEL     '7C'          KCOD
029400040923     C                   MOVEL(P)  300           KKEY
029500040923     C     KTAB          CHAIN     TABEL00F                           30
029600040923     C  N30              MOVEL     TBLUNI        DS7C
029700040923     C   30              CLEAR                   DS7C
029800040923     c                   endif
029900040923     C     *IN31         IFEQ      *ON
030000040923     c     §7cges        andeq     'S'
030100040923     C     D34INC        ANDEQ     'I'
030200970214     C                   MOVE      '1'           WA300             1
030300970214     C                   ELSE
030400970214     C                   MOVE      *BLANKS       WA300
030500970214     C                   END
030600960521     C* PULISCO CAMPI DI OUTPUT
030700960521     C                   CLEAR                   D34F03
030800960521     C                   CLEAR                   D34F12
030900960521     C                   CLEAR                   D34RIL
031000960521     C                   SETON                                        20
031100960521     C* CARICO £6 A CAMBIO DI FILIALE GESTIONE
031200960521     C     D34FGS        IFNE      SAVFGS
031300960521     C                   CLEAR                   L6S
031400960521     C                   CLEAR                   DSUL06
031500960521     C                   MOVE      '£6'          D06COD
031600020509     C**!!!              MOVEL     'S'           D06ESC
031700020509     c                   eval      d06esc = 'G'
031800020509     c                   movel     simfel        d06key
031900960521     C                   MOVEL     'L'           D06TLA
032000960521     C                   MOVEL     DSUL06        KPJBU
032100960521     C                   CALL      'TRUL06R'
032200960521     C                   PARM                    KPJBA
032300960521     C                   MOVEL     KPJBU         DSUL06
032400960521     C                   MOVEA     LIN           L6S
032500961210     C                   MOVE      D34FGS        SAVFGS
032600960521     C                   END
032700960521     C*
032800960521     C                   SETOFF                                       0203
032900960521     C* SE RICHIESTE SOLO ANOMALIE IN PARTENZA ACCENDO INDICATORE
033000960521     C* PER DISABILITARE L'INSERIMENTO
033100970214     C* AD ECCEZIONE DELL'ANOMALIA 300-N°RITIRI EFFETTUATI (WA300=' ')
033200960521    1C     D34PAR        IFEQ      'P'
033300970214     C     WA300         ANDEQ     *BLANKS
033400960521     C                   SETON                                        03
033500960521     C                   END
033600960521     C* SE IN INTERROGAZIONE ACCENDO INDICATORI PER DISABILITARE
033700960521     C* MANAUTENZIONE ED INSERIMENTO
033800960521     C     D34GES        IFEQ      'I'
033900960521     C                   SETON                                        0203
034000960521     C                   MOVEL     TIT2          V1DTIT
034100960521     C                   ELSE
034200960521     C                   MOVEL     TIT1          V1DTIT
034300960521    1C                   END
034400960521     C*    FINE FILE: IMPOSTO INDICATORE DI SFLEND
034500960521    1C     D34END        IFEQ      'S'
034600960521     C                   SETON                                        23
034700960521   X1C                   ELSE
034800960521     C* NO FINE FILE: CARICAMENTO SUBFILE
034900960521    2C     NRR1          IFLT      NRRAL
035000960521     C                   SETON                                        0405
035100960521     C                   SETOFF                                       09
035200960521     C                   EXSR      RIS01A
035300960523     C                   MOVE      '0'           V1HI51
035400960523     C                   MOVE      '0'           V1HI52
035500960521     C                   ADD       1             NRR1
035600960521     C                   WRITE     LR44S01
035700960521    2C                   END
035800960521    1C                   END
035900960521     C                   ENDSR
036000960521     C****************   MODALITA' INTERROGAZIONE ANOMALIE  ***********
036100960521     C     SRINTE        BEGSR
036200960522     C*
036300960521     C                   EXSR      PULSFL
036400970217     C                   SETOFF                                       09
036500960521     C* PROTEGGO CAMPI I/O DEL SUBFILE
036600960521     C                   SETON                                        0405
036700960521     C* DISABILITO TASTI DI CONFERMA/INSER...+ OPZIONI VARIAZ/ANN/RIPR.
036800960521     C                   SETON                                        0203
036900960521     C                   MOVEL     TIT2          V1DTIT
037000960521     C*
037100960521     C* POSIZIONAMENTO INIZIALE SU FILE ANOMALIE PER CARICAMENTO SUBFIL
037200960521     C     D44NSP        IFGT      *ZEROS
037300960521     C* Interrogazione anomalie bolla
037400960521     C                   SETON                                        91
037500990512     C     KANM01        SETLL     FNANM01L
037600990512     C     KANM01        READE     FNANM01L                               35
037700960521     C                   MOVE      '1'           WFILE             1
037800960521     C                   ELSE
037900960521     C* Interrogazione anomalie segnacollo
038000960521     C                   SETON                                        92
038100960521     C                   MOVE      D44FIL        W44FIL
038200960521     C                   MOVE      D44LNA        W44LNA
038300960521     C                   MOVE      D44NRS        W44NRS
038400960521     C                   MOVE      D44SCN        W44SCN
038500990512     C     KANM02        SETLL     FNANM02L
038600990512     C     KANM02        READE     FNANM02L                               35
038700960521     C                   MOVE      '2'           WFILE             1
038800960521     C                   END
038900960521     C                   EXSR      RIS01I
039000960524     C* SE NON HO CARICATO NULLA SPENGO INDICATORE SFLDSP
039100960524     C     NRR1          IFEQ      *ZEROS
039200960524     C                   SETOFF                                       20
039300960524     C                   END
039400960521     C                   ENDSR
039500960521     C*********   CARICAMENTO SUBFILE MODALITA INTERROGAZIONE  ********
039600960521     C     RIS01I        BEGSR
039700960524     C                   CLEAR                   WFIRST
039800960524     C*
039900960524     C                   EXSR      RIS1IB
040000960524     C*
040100961210     C* SE NON HO RIEMPITO TUTTA LA PAGINA DI SUBFILE ED E' FINE FILE
040200990512     C* (DI FNANM01L); SE STO INTERROGANDO PER SPEDIZIONE,RICHIAMO ROU-
040300961210     C* TINE DI RICERCA  ANOMALIE PER OGNI SEGNACOLLO DELLA SPEDIZIONE
040400961210    1C     WFILE         IFEQ      '1'
040500960521     C     *IN91         ANDEQ     *ON
040600960521     C     NRR1          ANDLT     NRRAL
040700961210     C     *IN35         ANDEQ     *ON
040701090311     c
041001090311     c                   if        not %open(fiart01l)
041100090311     C                   OPEN      FiART01L
041300090311     C                   ENDif
041301090311     c
041400090311     C     KART01        SETLL     FiART01L
041500090311     C     KART01        READE     FiART01L                               36
041501090311     c
043200960523     C                   MOVE      '2'           WFILE
043300961210     C  N36              EXSR      RICSCN
043400961210   X1C                   ELSE
043500050929     c
043600961210     C     WFILE         IFEQ      '2'
043700961210     C     *IN91         ANDEQ     *ON
043800961210     C     NRR1          ANDLT     NRRAL
043900050929     c                   if        finiscelett=' '
044000961210     C                   EXSR      RICSCN
044100961210     C* 23 --> SFLEND
044200961210     C   92
044300961210     CAN 36              SETON                                        23
044400050929     c                   else
044500050929     c                   seton                                        23
044600050929     c                   clear                   finiscelett
044700050929     C                   ENDif
044800050929     C                   END
044900961210    1C                   END
045000960524     C*
045100960524     C* SE HO CARICATO ALMENO UN RECORD DI SFL INCREMENTO SFLRCDNBR
045200960524    1C     NRR1          IFGT      NRRSAV
045300960524     C     NRRSAV        ADD       1             V1CNBR
045400960524    1C                   END
045500960524     C* SE FINE LETTURA MEMORIZZO SFLRCDNBR DELL'ULTIMA PAGINA
045600960524     C   23              Z-ADD     V1CNBR        W1CNBR
045700960524     C* MEMORIZZO NRR
045800960524     C                   Z-ADD     NRR1          NRRSAV
045900960521     C                   ENDSR
046000960523     C*********   CARICAMENTO SUBFILE MODALITA INTERROGAZIONE  ********
046100960523     C     RIS1IB        BEGSR
046200960523     C                   SETOFF                                       22  49
046300960523     C                   SETOFF                                       404142
046400960523     C                   SETOFF                                       434445
046500960523     C                   SETOFF                                       464748
046600960523    1C     NRR1          DOWLT     NRRAL
046700961210     C     *IN35         ANDEQ     *OFF
046800960524     C* ESCLUDO RECORD SE STO LEGGENDO PER SPEDIZIONE E SEGNACOLLO>0
046900960524     C* (LO TRATTERO' NEL GIRO PER SEGNACOLLO)
047000960524     C     WFILE         IFEQ      '1'
047100960524     C     ANMSCN        ANDGT     *ZEROS
047200960524     C                   GOTO      LEGGAN
047300960524     C                   END
047400960524     C*
047500960523     C     WFILE         IFEQ      '1'
047600960523     C                   Z-ADD     ANMNR1        D44NRR
047700960523     C                   ELSE
047800960523     C                   Z-ADD     ANMNR2        D44NRR
047900960523     C                   END
048000960523     C                   EXSR      RIS01A
048100960523     C*
048200960523     C* SOLO LA PRIMA VOLTA VISUALIZZO CAMPI DELLA CHIAVE
048300960524     C     WFIRST        IFEQ      *BLANKS
048400960524     C                   MOVE      '1'           WFIRST            1
048500960523     C                   SETOFF                                       5152
048600960523     C                   ELSE
048700960524     C     *IN91         IFEQ      *ON
048800960524     C                   SETOFF                                       52
048900960524     C                   ELSE
049000960524     C                   SETON                                        52
049100960524     C                   END
049200960523     C                   SETON                                        51
049300960523     C                   END
049400960523     C*
049500960524     C                   MOVE      *IN51         V1HI51
049600960524     C                   MOVE      *IN52         V1HI52
049700960523     C                   ADD       1             NRR1
049800960523     C                   WRITE     LR44S01
049900960524     C     LEGGAN        TAG
050000960523     C     WFILE         IFEQ      '1'
050100990512     C     KANM01        READE     FNANM01L                               35
050200960523     C                   ELSE
050300990512     C     KANM02        READE     FNANM02L                               35
050400960523     C                   END
050500960523     C                   ENDDO
050600990512     C* MEMORIZZO CAMPI DI FNANM PER NON SPORCARMELI SE PER CASO
050700961210     C* ENTRERO' IN SRINT
050800961210     C  N35              MOVEL     DSANM         DSANMX
050900960523     C                   ENDSR
051000960521     C****************  RICERCA SEGNACOLLI DELLA SPEDIZIONE  **********
051100960521     C     RICSCN        BEGSR
051200960521    1C     NRR1          DOWLT     NRRAL
051300961210     C     *IN36         ANDEQ     *OFF
051400960530     C                   MOVE      DETFLS        W44FIL
051500960521     C                   MOVE      DETLNA        W44LNA
051600960521     C                   MOVE      DETNRS        W44NRS
051700960521     C                   MOVE      DETNSC        W44SCN
051800990512     C     KANM02        SETLL     FNANM02L
051900990512     C     KANM02        READE     FNANM02L                               35
052000961210     C  N35              EXSR      RIS1IB
052100960521     C*
052400090311     C     KART01        READE     FiART01L                               36
053200960521     C                   ENDDO
053300960521     C*
053400050929     c                   if        *in35
053500961210     C   36              SETON                                        23
053600050929     c   36              clear                   finiscelett       1
053700050929     c                   else
053800050929     c* per 35 off - devop finire di leggere le anomalie dell'ultimo
053900050929     c*  segnacollo
054000050929     c                   if        *in36
054100050929     c                   eval      finiscelett='S'
054200050929     c                   endif
054300050929     c                   endif
054400960521     C                   ENDSR
054500960521     C****************  EMISSIONE/GESTIONE SUBFILE  *******************
054600960521     C     SRXFMT        BEGSR
054700960524     C* SOLO SE MODALITA' GESTIONE:
054800960524    1C     D44MOD        IFEQ      'G'
054900960524     C* Se non ho caricato nulla spengo indicatore sfldsp
055000960524    2C     NRR1          IFEQ      *ZEROS
055100960524     C                   SETOFF                                       20
055200960524     C                   MOVE      'S'           D34F12
055300960524   X2C                   ELSE
055400960524     C* Se ho caricato almeno un record di sfl incremento sflrcdnbr
055500960524     C                   Z-ADD     NRRSAV        V1CNBR
055600960524    3C     NRR1          IFGT      NRRSAV
055700960524     C                   ADD       1             V1CNBR
055800960524    3C                   END
055900960524     C* Memorizzo nrr
056000960524     C                   Z-ADD     NRR1          NRRSAV
056100960524     C* Se fine lettura memorizzo sflrcdnbr dell'ultima pagina
056200960524     C   23              Z-ADD     V1CNBR        W1CNBR
056300960524    2C                   END
056400960521     C* Imposto piedi del video
056500960521     C***
056600960521     C*** Modalità gestione
056700960521     C***
056800960522     C  N03
056900960522     CANN20              MOVEL     Z02(2)        V1ZCMD
057000960522     C   03
057100960522     CANN20              MOVEL     Z02(4)        V1ZCMD
057200960521     C* solo interrogazione
057300960521     C   02
057400960521     CAN 20              MOVEL     Z02(1)        V1ZCMD
057500960521     C* solo manutenzione
057600960521     C   03
057700960521     CANN02
057800960521     CAN 20              MOVEL     Z03(1)        V1ZCMD
057900960521     C   03
058000960521     CANN02
058100960521     CAN 20              Z-ADD     1             WF                1 0
058200960521     C   03
058300960521     CANN02
058400960521     CAN 20              Z-ADD     2             WMAX              1 0
058500960521     C* tutte le funzioni abilitate (manutenzione/inserimento/interrog)
058600960521     C  N03
058700960521     CAN 20              MOVEL     Z01(1)        V1ZCMD
058800960521     C  N03
058900960521     CAN 20              Z-ADD     1             WF                1 0
059000960521     C  N03
059100960521     CAN 20              Z-ADD     3             WMAX              1 0
059200960524   X1C                   ELSE
059300960521     C***
059400960521     C*** Modalità interrogazione
059500960521     C***
059600960521     C  N20              MOVE      Z02(4)        V1ZCMD
059700960521     C   91
059800960521     CAN 20              MOVE      Z02(5)        V1ZCMD
059900960521     C   92
060000960521     CAN 20              MOVE      Z02(6)        V1ZCMD
060100960524    1C                   END
060200960521     C* EMISSIONE SUBFILE
060300960521     C     EMC01         TAG
060400960521     C                   WRITE     LR44Z01
060500960521     C  N20              WRITE     LR44D03
060600960521     C                   WRITE     LR44D01
060700960521     C                   EXFMT     LR44C01
060800971103     C                   SETOFF                                       289040
060900960521     C                   SETOFF                                       414243
061000960521     C                   SETOFF                                       444546
061100960521     C                   SETOFF                                       474849
061200960521     C                   CLEAR                   V1ZMSG
061300960521     C* Tasto F3=Fine
061400960521    2C     *INKC         IFEQ      *ON
061500960521     C                   MOVE      'S'           D34F03
061600960521     C                   EXSR      PULSFL
061700960521     C                   GOTO      FXFMT
061800960521    2C                   END
061900960521     C* Tasto F12=Ritorno
062000960521    2C     *INKL         IFEQ      *ON
062100960521     C   10              MOVEL     'S'           D34RIL
062200960521     C  N10              MOVE      'S'           D34F12
062300960521     C                   EXSR      PULSFL
062400960521     C                   GOTO      FXFMT
062500960521    2C                   END
062600960521     C* Tasto F24=Altri Tasti
062700960521    2C     *INKY         IFEQ      *ON
062800960521     C     WF            IFLT      WMAX
062900960521     C                   ADD       1             WF
063000960521     C  N03              MOVEL     Z01(WF)       V1ZCMD
063100960521     C   03              MOVEL     Z03(WF)       V1ZCMD
063200960521     C                   ELSE
063300960521     C                   Z-ADD     1             WF
063400960521     C  N03              MOVEL     Z01(WF)       V1ZCMD
063500960521     C   03              MOVEL     Z03(WF)       V1ZCMD
063600960521     C                   END
063700960521     C                   GOTO      EMC01
063800960521    2C                   END
063900960521     C* Tasto F7=Interrogazione bolla
064000960521    2C     *INKG         IFEQ      *ON
064100960521     C                   SETON                                        22
064200960521     C     V1CPOS        CHAIN     LR44S01                            30
064300960521    3C     *IN30         IFEQ      *OFF
064400960523     C                   MOVE      V1HI51        *IN51
064500960523     C                   MOVE      V1HI52        *IN52
064600960521     C                   Z-ADD     NRR1          V1CNBR
064700960521     C                   UPDATE    LR44S01
064800960521     C                   SETOFF                                       22
064900960605     C                   CLEAR                   WAAS
065000960605     C                   MOVE      V1SLNP        WLNP
065100960605     C                   MOVE      V1SNRS        WNRS
065200960605     C                   MOVE      V1SNSP        WNSP
065300960605     C* Solo se ho la lna:
065400960605    4C     V1SLNA        IFGT      *ZEROS
065900021028     c*
066000021028     c                   setoff                                       3031
066100021028     c* 30 off - ce'e' bolla ARRIVI
066101090311     c* 30 on  - 31 off c'e' bolla partenza
066200090311     C     Kart          CHAIN     FIART27L                           30
066500021028     c*
066600021028    5C     V1SNSP        IFEQ      *ZEROS
066700090311    6C     *IN30         IFEQ      *OFF
066900960605     C                   MOVE      D27AAS        WAAS
067000960605     C                   MOVE      D27LNP        WLNP
067100960605     C                   MOVE      D27NRS        WNRS
067200960605     C                   MOVE      D27NSP        WNSP
067300090311    6C                   END
067400960605    5C                   END
067500960605     C*
067600960605    5C     WNSP          IFGT      *ZEROS
067700990513     C* Imposto anno a 4 cifre
067800960605    6C     WAAS          IFEQ      *ZEROS
067900990513    7C     V1SAAS        IFGT      60
068000990513     C                   MOVEL     19            WAAS
068100990513   X7C                   ELSE
068200990513     C                   MOVEL     20            WAAS
068300990513    7C                   END
068400990513     C                   MOVE      V1SAAS        WAAS
068500960605    6C                   END
068600021028     c*
068700021028     C* interrogo bolle arrivi
068701090311     c  n30kbolla        setll     fnarb01l
068800090311    6C                   if        not *in30 and %equal(fnarb01l)
068900960523     C                   CLEAR                   DSLR36
069000960523     C                   MOVE      WAAS          PA1AAS
069100960605     C                   MOVE      WLNP          PA1LNP
069200960605     C                   MOVE      WNRS          PA1NRS
069300960605     C                   MOVE      WNSP          PA1NSP
069400960903     C                   MOVE      'N'           PA1FLG
069500040121     C                   MOVE      *blanks       PA1CML
069600960523     C                   MOVEL     V1DPGM        PA1PGM
069700960523     C                   MOVEL     DSLR36        KPJBU
069800960523     C                   CALL      'FNLR36R'
069900960523     C                   PARM                    KPJBA
070000960605   X6C                   ELSE
070100021028     c**
070200960523     C* anomalia della partenza
070501090311     c  n30kbolla        setll     fnblp01l
070502090311    7C                   if        not *in30 and %equal(fnblp01l)
070600960523     C                   CLEAR                   DSLS05
070700960523     C                   MOVE      WAAS          PA0AAS
070800960605     C                   MOVE      WLNP          PA0LNP
070900960605     C                   MOVE      WNRS          PA0NRS
071000960605     C                   MOVE      WNSP          PA0NSP
071100960903     C                   MOVEL     'N'           PA0FLG
071200960523     C                   MOVEL     RAGUT         PA0RSU
071300960523     C                   MOVE      SIMFEL        PA0SIM
071400040121     C                   MOVE      *blanks       PA0CML
071500960523     C                   MOVEL     DSLS05        KPJBU
071600960523     C                   CALL      'FNLS05R'
071700960523     C                   PARM                    KPJBA
071800090311    7C                   END
072000021028    6C                   END
072100960605    5C                   END
072200960605    4C                   END
072300960605    3C                   END
072400960521     C                   GOTO      EMC01
072500960521    2C                   END
072600960521     C* Tasto F14=Spunte
072700960521    2C     *INKN         IFEQ      *ON
072800960521     C                   SETON                                        22
072900960521     C     V1CPOS        CHAIN     LR44S01                            30
073000960521    3C     *IN30         IFEQ      *OFF
073100960523     C                   MOVE      V1HI51        *IN51
073200960523     C                   MOVE      V1HI52        *IN52
073300960521     C                   Z-ADD     NRR1          V1CNBR
073400960521     C                   UPDATE    LR44S01
073500960521     C                   SETOFF                                       22
073600960521    4C     V1SSCN        IFGT      *ZEROS
073700960521     C                   CLEAR                   DSLR04
073800960521     C                   Z-ADD     0             PA4NFV
073900960521     C                   MOVE      V1SFLS        PA4LNP
074000960521     C                   MOVE      V1SLNA        PA4LNA
074100960521     C                   MOVE      V1SNRS        PA4NRS
074200960521     C                   MOVEL     9             PA4NPG
074300960521     C                   Z-ADD     V1SSCN        PA4NCD
074400960521     C                   Z-ADD     0             PA4NCA
074500960521     C                   MOVEL     99            PA4NPS
074600960521     C                   MOVEL     'FNLR44R'     PA4PGM
074700960903     C                   MOVEL     '1'           PA4FLG
074800960521     C                   MOVEL     DSLR04        KPJBU
074900960521     C                   CALL      'FNLR04R'
075000960521     C                   PARM                    KPJBA
075100960521    4C                   END
075200960521    3C                   END
075300960521     C                   GOTO      EMC01
075400960521    2C                   END
075500960521     C* Tasto F10=Inserimento
075600960521    2C     *INKJ         IFEQ      *ON
075700970214     C* Se anomalia 300 emetto window di immissione
075800970214    3C     WA300         IFEQ      '1'
075900970217     C                   SETON                                        10
076000970214     C                   EXSR      WDW04
076100970217     C                   EXSR      PULSFL
076200970214     C                   GOTO      FXFMT
076300970214   X3C                   ELSE
076400960521     C                   EXSR      PULSFL
076500970217     C                   SETON                                        10
076600960521     C                   EXSR      RIS01B
076700960521     C                   MOVEL     Z02(3)        V1ZCMD
076800970214    3C                   END
076900960521     C                   GOTO      EMC01
077000960521    2C                   END
077100960521     C* Solo se ho caricato qualcosa:
077200960521    2C     NRR1          IFGT      *ZEROS
077300960521     C* Tasto Rollup
077400960521    3C     *IN25         IFEQ      *ON
077500960521     C* Fine lettura --> emetto messaggio fine scorrimento
077600960521    4C     *IN23         IFEQ      *ON                                          FINE LETTURA
077700960521     C                   Z-ADD     W1CNBR        V1CNBR
077800960521     C                   MOVEL     MSG(1)        V1ZMSG
077900960521     C                   SETON                                        28
078000960521     C                   GOTO      EMC01
078100960521   X4C                   ELSE
078200960521     C     *IN10         IFEQ      *ON
078300960521     C                   EXSR      RIS01B
078400960521     C                   GOTO      EMC01
078500960521     C                   ELSE
078600960521     C                   ADD       9             NRRAL
078700960521     C                   Z-ADD     NRRSAV        NRR1
078800960521     C     D44MOD        IFEQ      'G'
078900960521     C                   GOTO      FXFMT
079000960521     C                   ELSE
079100960521     C                   EXSR      RIS01I
079200960521     C                   GOTO      EMC01
079300960521     C                   END
079400960521     C                   END
079500960521    4C                   END
079600960521    3C                   END
079700960521     C* CONTROLLI SUBFILE
079800960521    3C     *IN10         IFEQ      *OFF
079900960521     C                   EXSR      CTRINT
080000960521     C   KC              MOVE      'S'           D34F12
080100960521     C   KC              EXSR      PULSFL
080200960521     C   KC              GOTO      FXFMT
080300960521     C* Se interrogate anomalie riemetto il subfile
080400960521     C     WINT          CABEQ     '1'           EMC01
080500960521     C                   END
080600960521     C* Solo se non sono in interrogazione
080700960521     C     D44MOD        IFEQ      'G'
080800960521     C     D34GES        ANDNE     'I'
080900960521     C  N10              EXSR      CTS01A
081000960521     C   10              EXSR      CTS01B
081100960521     C***
081200960521     C  NKF
081300971103     COR 90
081400960521     COR 28              GOTO      EMC01
081500960521     C*
081600960521    3C     *IN10         IFEQ      *ON
081700960521     C   KF              EXSR      WTRANM
081800960521     C                   EXSR      PULSFL
081900960521     C                   ELSE
082000960521     C   KF              EXSR      AGGANM
082100960521     C                   GOTO      EMC01
082200960521    3C                   END
082300960523     C                   END
082400960521    2C                   END
082500960521     C     FXFMT         ENDSR
082600960327     C****************   PULIZIA SUBFILE ******************************
082700960327     C     PULSFL        BEGSR
082800960327     C                   SETOFF                                       2123
082900960327     C                   WRITE     LR44C01
083000960327     C                   SETON                                        2021
083100960327     C                   MOVE      *ZEROS        NRR1              5 0
083200960327     C                   MOVE      *ZEROS        NRRSAV
083300960327     C                   MOVE      *ZEROS        W1CNBR
083400960327     C                   MOVE      *ZEROS        V1CNBR
083500960327     C                   Z-ADD     9             NRRAL
083600960402     C                   SETOFF                                       100405
083700960405     C                   SETON                                        09
083800960327     C                   ENDSR
083900960327     C*********** RIEMPIMENTO CAMPI SUBFILE S01 ***********************
084000960328     C     RIS01A        BEGSR
084100960405     C                   SETOFF                                       22  49
084200960401     C                   SETOFF                                       404142
084300960401     C                   SETOFF                                       434445
084400960401     C                   SETOFF                                       464748
084500960328     C                   MOVE      ANMAAS        V1SAAS
084600960328     C                   MOVE      ANMLNP        V1SLNP
084700960328     C                   MOVE      ANMNRS        V1SNRS
084800960328     C                   MOVE      ANMNSP        V1SNSP
084900960328     C                   MOVE      ANMLNA        V1SLNA
085000960401     C                   MOVE      ANMFLS        V1SFLS
085100960328     C                   MOVE      ANMSCN        V1SSCN
085200040603     c                   move      anmft4        v1sbva
085300960327     C* DECODIFICO TIPO ANOMALIA
085400960328     C                   MOVEL     '7F'          KCOD
085500960328     C                   MOVEL(P)  ANMTAN        KKEY
085600960328     C     KTAB          CHAIN     TABEL00F                           30
085700960328     C  N30              MOVEL     TBLUNI        V1DTAN
085800960328     C   30              CLEAR                   V1DTAN
085900960404     C                   MOVE      ANMTAN        V1HTAN
086000960327     C* DECODIFICO SOGGETTO ANOMALIA
086100960328     C                   MOVEL     '7G'          KCOD
086200960328     C                   MOVEL(P)  ANMSAN        KKEY
086300960328     C     KTAB          CHAIN     TABEL00F                           30
086400960328     C  N30              MOVEL     TBLUNI        V1DSAN
086500960404     C   30              CLEAR                   V1DSAN
086600960404     C                   MOVE      ANMSAN        V1HSAN
086700960328     C                   MOVE      ANMCAA        V1SCAA
086800960403     C                   MOVE      ANMCAA        V1HCAA
086900960409     C                   CLEAR                   V1HKSP
087000970507     C                   CLEAR                   V1HARB
087100960506     C* DECODIFICO CODICE ANOMALIA
087200960401     C                   CLEAR                   DS7C
087300960328     C                   MOVEL     '7C'          KCOD
087400960328     C                   MOVEL(P)  ANMCAA        KKEY
087500960328     C                   CLEAR                   V1DCAA
087600960328     C                   CLEAR                   V1HGES
087700960328     C                   CLEAR                   V1HANN
087800960402     C                   CLEAR                   V1HKEY
087900960404     C                   CLEAR                   V1HLIO
088000960411     C                   CLEAR                   V1HUEA
088100960328     C     KTAB          CHAIN     TABEL00F                           30
088200960328     C     *IN30         IFEQ      *OFF
088300960328     C                   MOVEL     TBLUNI        DS7C
088400960328     C                   MOVEL     §7CDES        V1DCAA
088500960328     C* Memorizzo in campi hidden se caus.gestibile in manut./ins./ann
088600960328     C                   MOVEL     §7CGES        V1HGES
088700960418     C     D34APA        IFEQ      'P'
088800960328     C                   MOVEL     §7CANN        V1HANN
088900960418     C                   ELSE
089000960418     C                   MOVEL     §7CANA        V1HANN
089100960418     C                   END
089200960402     C                   MOVEL     §7CKEY        V1HKEY
089300960404     C     §7CIDD        IFEQ      'S'
089400960404     C                   MOVEL     §7CLID        V1HLIO
089500960404     C                   END
089600960411     C                   MOVE      §7CUEA        V1HUEA
089700960328     C                   END
089800960507     C                   MOVE      ANMAIE        V1HAIE
089900960507     C     ANMAIE        IFEQ      'I'
090000960328     C                   MOVE      ANMAIE        V1SAIE
090100960507     C                   ELSE
090200960507     C     §7CIDD        IFEQ      'S'
090300960507     C                   MOVEL     'D'           V1SAIE
090400960507     C                   ELSE
090500960507     C                   MOVEL     'N'           V1SAIE
090600960507     C                   END
090700960507     C                   END
090800960401     C     §7CIDD        IFEQ      'S'
090900960328     C                   MOVE      ANMLID        V1SLID
091000960401     C                   ELSE
091100960401     C                   CLEAR                   V1SLID
091200960401     C                   END
091300960328     C* DECODIFICO FASE APERTURA ANOMALIA
091400960328     C                   MOVEL     '7D'          KCOD
091500960328     C                   MOVEL(P)  ANMFSA        KKEY
091600960328     C                   CLEAR                   V1DFSA
091700960328     C     KTAB          CHAIN     TABEL00F                           30
091800960328     C     *IN30         IFEQ      *OFF
091900960328     C                   MOVEL     TBLUNI        DS7D
092000960328     C                   MOVEL     §7DDES        V1DFSA
092100960328     C                   END
092200960404     C                   MOVE      ANMFSA        V1HFSA
092300960328     C* ROVESCIO DATA ANOMALIA
092400990513     C                   CLEAR                   WLBDA8
092500990513     C                   MOVE      ANMDAO        G08INV
092600990513     C                   MOVEL     '3'           G08ERR
092700990513     C                   CALL      'XSRDA8'
092800990513     C                   PARM                    WLBDA8
092900990513     C                   MOVEL     G08DAT        V1SDAO
093000990513     C                   MOVE      G08DAT        W0020             2 0
093100990513     C                   MOVE      W0020         V1SDAO
093200960419     C* RIEMPIO CAMPI RELATIVI AI DATI DI CHIUSURA (FASE,DATA,CAUSALE)
093300960419     C                   EXSR      RIECHI
093400960403     C     ANMCDU        IFGT      *ZEROS
093500960328     C                   MOVE      ANMCDU        V1SCDU
093600960403     C                   ELSE
093700960403     C                   CLEAR                   V1SCDU
093800960403     C                   END
093900960328     C                   MOVE      ANMNFV        V1SNFV
094000960328     C                   MOVE      ANMNPS        V1SNPS
094100960412     C                   CLEAR                   V1SVAR
094200960412     C                   CLEAR                   V1HVAR
094300960402     C                   MOVEL     ANMATB        V1SATB
094400960412     C                   Z-ADD     D44NRR        V1HNRR
094500960402     C                   MOVE      *IN04         V1HIN4
094600960402     C                   MOVE      *IN05         V1HIN5
094700960405     C                   MOVE      *IN09         V1HIN9
094800960401     C                   MOVEL     D34APA        V1HAPA
094900960401     C                   CLEAR                   V1HPRE
095000960327     C                   ENDSR
095100960419     C*********** RIEMPIMENTO CAMPI CHIUSURA ANOMALIA DI SFL S01 ******
095200960419     C     RIECHI        BEGSR
095300960419     C* DECODIFICO FASE CHIUSURA ANOMALIA
095400960419     C                   MOVE      *BLANKS       V1DFSC
095500960419     C     ANMFSC        IFNE      *BLANKS
095600960419     C                   MOVEL     '7D'          KCOD
095700960419     C                   MOVEL(P)  ANMFSC        KKEY
095800960419     C     KTAB          CHAIN     TABEL00F                           30
095900960419     C     *IN30         IFEQ      *OFF
096000960419     C                   MOVEL     TBLUNI        DS7D
096100960419     C                   MOVEL     §7DDES        V1DFSC
096200960419     C                   END
096300960419     C                   END
096400960419     C* ROVESCIO DATA CHIUSURA ANOMALIA SE DIVERSA DA 0
096500960419     C                   MOVE      *ZEROS        V1SDCH
096600960419     C     ANMDCH        IFGT      *ZEROS
096700990513     C                   CLEAR                   WLBDA8
096800990513     C                   MOVEL     ANMDCH        G08INV
096900990513     C                   MOVEL     '3'           G08ERR
097000990513     C                   CALL      'XSRDA8'
097100990513     C                   PARM                    WLBDA8
097200990514     C                   MOVEL     G08DAT        V1SDCH
097300990513     C                   MOVE      G08DAT        W0020             2 0
097400990513     C                   MOVE      W0020         V1SDCH
097500960419     C                   END
097600041125     C
097700960419     C                   MOVE      ANMCCH        V1SCCH
097800970522     C*
097900960419     C                   ENDSR
098000960328     C**************  RIEMPIMENTO SUBFILE PER INSERIMENTO *************
098100960328     C     RIS01B        BEGSR
098200960429     C                   SETOFF                                       22
098300960523     C                   SETOFF                                       5152
098400960328     C                   Z-ADD     NRRSAV        NRR1
098500960328     C     NRR1          ADD       1             V1CNBR
098600960329     C     NRR1          DOWLT     NRRAL
098700960328     C                   CLEAR                   V1SAAS
098800960328     C                   CLEAR                   V1SLNP
098900960328     C                   CLEAR                   V1SNRS
099000960328     C                   CLEAR                   V1SNSP
099100960402     C* IMPOSTO LINEA DI ARRIVO = FILIALE GESITIONE
099200960402     C                   MOVE      D34FGS        V1SLNA
099300960401     C                   CLEAR                   V1SFLS
099400960328     C                   CLEAR                   V1SSCN
099500960328     C                   CLEAR                   V1DTAN
099600960328     C                   CLEAR                   V1DSAN
099700960329     C                   CLEAR                   V1SCAA
099800960328     C                   CLEAR                   V1DCAA
099900960328     C                   CLEAR                   V1HGES
100000960328     C                   CLEAR                   V1HANN
100100960402     C                   CLEAR                   V1HTAN
100200960402     C                   CLEAR                   V1HSAN
100300960402     C                   CLEAR                   V1HKEY
100400960404     C                   CLEAR                   V1HLIO
100500960403     C                   CLEAR                   V1HCAA
100600960409     C                   CLEAR                   V1HKSP
100700970507     C                   CLEAR                   V1HARB
100800960329     C                   CLEAR                   V1HNRR
100900960411     C                   CLEAR                   V1HUEA
101000960328     C                   CLEAR                   V1SAIE
101100960507     C                   CLEAR                   V1HAIE
101200960328     C                   CLEAR                   V1SLID
101300960401     C                   MOVE      'A'           V1HAPA
101400960329     C                   MOVEL     '2'           V1HFSA
101500960329     C                   MOVEL     '7D'          KCOD
101600960329     C                   MOVEL(P)  '2'           KKEY
101700960329     C     KTAB          CHAIN     TABEL00F                           30
101800960329     C  N30              MOVEL     TBLUNI        V1DFSA
101900960329     C   30              CLEAR                   V1DFSA
102000971118     C* IMPOSTO DATA ANOMALIA = DATA DEL GIORNO
102100971118     C                   MOVE      DTGIO6        V1SDAO
102200990513     C                   MOVE      DATEU8        V1HDAO
102300960328     C                   CLEAR                   V1DFSC
102400960328     C                   CLEAR                   V1SDCH
102500960328     C                   CLEAR                   V1SCDU
102600960328     C                   CLEAR                   V1SNFV
102700960328     C                   CLEAR                   V1SNPS
102800960328     C                   CLEAR                   V1SCCH
102900960401     C                   CLEAR                   V1HPRE
103000960402     C                   MOVE      *IN04         V1HIN4
103100960402     C                   MOVE      *IN05         V1HIN5
103200960405     C                   MOVE      *IN09         V1HIN9
103300960523     C                   MOVE      '0'           V1HI51
103400960523     C                   MOVE      '0'           V1HI52
103500960328     C                   ADD       1             NRR1
103600960328     C                   WRITE     LR44S01
103700960328     C                   ENDDO
103800960328     C                   Z-ADD     NRR1          NRRSAV
103900960328     C                   ADD       9             NRRAL
104000960328     C                   ENDSR
104100960329     C******  CONTROLLI SUBFILE  PER INTERROGAZIONE RECORD ANOMALIA ***
104200960329     C     CTRINT        BEGSR
104300960403     C                   SETON                                        22
104400960329     C                   MOVE      *BLANKS       WINT              1
104500960329     C                   READC     LR44S01                                29
104600960329    1C     *IN29         DOWEQ     *OFF
104700960412     C     V1SVAR        IFEQ      'I'
104800960329     C* ESPLODO RECORD DI SUBFILE
104900960412     C                   CLEAR                   V1SVAR
105000960329     C                   MOVE      '1'           WINT
105100960402     C                   EXSR      SRINT
105200960509     C                   Z-ADD     NRR1          V1CNBR
105300961210     C                   MOVEL     DSANMX        DSANM
105400960329     C                   END
105500960402     C                   MOVE      V1HIN4        *IN04
105600960402     C                   MOVE      V1HIN5        *IN05
105700960405     C                   MOVE      V1HIN9        *IN09
105800960523     C                   MOVE      V1HI51        *IN51
105900960523     C                   MOVE      V1HI52        *IN52
106000960329     C                   UPDATE    LR44S01
106100960405     C* PREMUTO F3 DA VIDEATA INTERROGAZIONE ANOMALIA: ESCO DAL PGM
106200960403     C     *INKC         CABEQ     '1'           ENDINT
106300960329     C                   READC     LR44S01                                29
106400960329     C                   ENDDO
106500960403     C     ENDINT        ENDSR
106600960328     C******  CONTROLLI SUBFILE  MODALITA' INTERROGAZ/MANUTENZ. *******
106700960328     C     CTS01A        BEGSR
106800960403     C                   SETON                                        22
106900960426     C                   MOVE      ' '           SAVNBR            1
107000960329     C                   READC     LR44S01                                29
107100960329    1C     *IN29         DOWEQ     *OFF
107200960412    2C     V1SVAR        IFNE      *BLANKS
107300960412     C     V1HVAR        ORNE      *BLANKS
107400990512     C     V1HNRR        CHAIN(N)  FNANM00F                           30
107500960412     C* SE NON TROVATA ANOMALIA ERRORE
107600960412     C     *IN30         IFEQ      *ON
107700960412     C                   MOVE      'A'           V1SATB
107800960412     C                   MOVEL     MSG(31)       V1ZMSG
107900960412     C                   SETON                                        4728
108000960412     C                   GOTO      ENDCTA
108100960412     C                   END
108200960418     C* ERRORE SE RICHIESTA MANUTENZIONE/ANNULLAMENTO SU ANOMALIA GIà
108300960418     C*           CHIUSA CON CAUSALE DIVERSA DA 'AN'
108400970217     C* ESLUDO IL SEGUENTE CONTROLLO SE SI TRATTA DI AN.300=N°RITIRI
108500960418     C     ANMDCH        IFGT      *ZEROS
108600960418     C     ANMCCH        ANDNE     'AN'
108700960422     C     ANMCCH        ANDNE     'PR'
108800970217     C     ANMCAA        ANDNE     300
108900960403     C                   SELECT
109000960412     C     V1SVAR        WHENEQ    'A'
109100960403     C                   MOVEL     MSG(12)       V1ZMSG
109200960412     C                   SETON                                        4728
109300960412     C                   GOTO      ENDCTA
109400960412     C     V1SVAR        WHENEQ    'R'
109500960403     C                   MOVEL     MSG(14)       V1ZMSG
109600960412     C                   SETON                                        4728
109700960412     C                   GOTO      ENDCTA
109800960412     C     V1HVAR        WHENEQ    'V'
109900960412     C     *INKF         ANDEQ     *ON
110000960412     C                   MOVEL     MSG(30)       V1ZMSG
110100960412     C                   SETON                                        040528
110200960412     C                   SETOFF                                       09
110300960412     C                   MOVE      V1HNRR        D44NRR
110400960412     C                   EXSR      RIS01A
110500960412     C                   SETON                                        2247
110600960412     C                   GOTO      ENDCTA
110700960412     C     V1SVAR        WHENEQ    'V'
110800960403     C                   MOVEL     MSG(16)       V1ZMSG
110900960412     C                   SETON                                        4728
111000960412     C                   GOTO      ENDCTA
111100960403     C                   ENDSL
111200960329    3C                   END
111300960423     C**** VERIFICO ANNULLABILITA' DELL'ANOMALIA ***
111400960412    3C     V1SVAR        IFEQ      'A'
111500960418     c* Mai annullabile
111600960329    4C     V1HANN        IFEQ      *BLANKS
111700960418     C* Annullabile solo prima della trasmissione
111800960418     C     V1HANN        OREQ      'P'
111900960418     C     ANMFT1        ANDNE     *BLANKS
112000960418     C     V1HANN        OREQ      'P'
112100960418     C     V1HLIO        ANDNE     V1SLID
112200960506     C     V1SCAA        ANDNE     '070'
112300960506     C     V1SCAA        ANDNE     '071'
112400960418     C* Annullabile solo dopo la trasmissione
112500960418     C     V1HANN        OREQ      'D'
112600960418     C     ANMFT1        ANDEQ     *BLANKS
112700960418     C     V1HANN        OREQ      'D'
112800960418     C     V1HLIO        ANDEQ     V1SLID
112900960506     C     V1SCAA        ANDNE     '070'
113000960506     C     V1SCAA        ANDNE     '071'
113100960329     C                   MOVEL     MSG(12)       V1ZMSG
113200960401     C                   SETON                                        4728
113300960329     C                   GOTO      ENDCTA
113400960329    4C                   END
113500960418     C* Anomalia già annullata
113600960329    4C     V1SATB        IFNE      *BLANKS
113700960418     C     ANMCCH        OREQ      'AN'
113800960329     C                   MOVEL     MSG(13)       V1ZMSG
113900960401     C                   SETON                                        4728
114000960329     C                   GOTO      ENDCTA
114100960329    4C                   END
114200960429     C* Controlli particolari per annullamento anomalie partenza:
114300960429     C* 005 - Collo partito non arrivato
114400960429     C* 010 - No record bolla arrivo
114500960429     C* 012 - Manca record bolla e colli
114600960429    4C     V1HAPA        IFEQ      'P'
114700960429    5C     V1SCAA        IFEQ      '005'
114800960429     C     V1SCAA        OREQ      '010'
114900960429     C     V1SCAA        OREQ      '012'
115000960429     C                   EXSR      CTRANN
115100960429     C     WFLG01        IFEQ      'E'
115200960429     C                   MOVEL     MSG(34)       V1ZMSG
115300960429     C                   SETON                                        4728
115400960429     C                   GOTO      ENDCTA
115500960429     C                   END
115600960429    5C                   END
115700960429    4C                   END
115800970214    3C                   END
115900960423     C**** VERIFICO SE ANOMALIA RIPRISTINABILE ***
116000960412    3C     V1SVAR        IFEQ      'R'
116100960419     c* Mai ripristinabile
116200960329    4C     V1HANN        IFEQ      *BLANKS
116300960401     C                   MOVEL     MSG(14)       V1ZMSG
116400960401     C                   SETON                                        4728
116500960329     C                   GOTO      ENDCTA
116600960329    4C                   END
116700960329    4C     V1SATB        IFEQ      *BLANKS
116800960419     C     ANMCCH        ANDNE     'AN'
116900960329     C                   MOVEL     MSG(15)       V1ZMSG
117000960401     C                   SETON                                        4728
117100960329     C                   GOTO      ENDCTA
117200960329    4C                   END
117300960401    3C                   END
117400960423     C**** VERIFICO SE ANOMALIA MANUTENZIONABILE  ***
117500960412    3C     V1SVAR        IFEQ      'V'
117600960423     C* Non manutenzionabile se anomalia della partenza o anomalia
117700960423     C* già trasmessa
117800960419     C     V1HAPA        IFEQ      'P'
117900970214     C     ANMCAA        ANDNE     300
118000960423     C     ANMFT1        ORNE      *BLANKS
118100960423     C     V1SLID        ORNE      V1HLIO
118200960506     C     V1SCAA        ANDNE     '070'
118300960506     C     V1SCAA        ANDNE     '071'
118400960419     C                   MOVEL     MSG(16)       V1ZMSG
118500960419     C                   SETON                                        4728
118600960419     C                   GOTO      ENDCTA
118700960419     C                   END
118800970214     C* Se richiesta manutenzione anomalia 300 emetto window anzichè
118900970214     C* sproteggere i campi del subfile
119000970214     C     ANMCAA        IFEQ      300
119100040923     c     v1hges        ifeq      'S'
119200970214     C                   EXSR      WDW04
119300040923     C                   ELSE
119400040923     C                   MOVEL     MSG(16)       V1ZMSG
119500040923     C                   SETON                                        4728
119600040923     c                   endif
119700970214     C                   GOTO      ENDCTA
119800970214     C                   END
119900960412     C* CAMPI PROTETTI: SE POSSIBILE LI SPROTEGGO
120000960412    4C     V1HIN4        IFEQ      '1'
120100960412   4BC     *INKF         IFEQ      *OFF
120200960412    5C     V1HGES        IFEQ      'S'
120300960412     C                   SETOFF                                       04
120400960412     C                   SETON                                        09
120500960412     C                   MOVE      *IN04         V1HIN4
120600960412     C                   MOVE      *IN09         V1HIN9
120700960412     C                   MOVE      V1SVAR        V1HVAR
120800960506     C* Sproteggo codice anomalia solo se annullabile:
120900960412    6C     V1HANN        IFEQ      *BLANKS
121000960419     C     V1HANN        OREQ      'P'
121100960419     C     ANMFT1        ANDNE     *BLANKS
121200960419     C     V1HANN        OREQ      'P'
121300960419     C     V1HLIO        ANDNE     V1SLID
121400960506     C     V1SCAA        ANDNE     '070'
121500960506     C     V1SCAA        ANDNE     '071'
121600960419     C     V1HANN        OREQ      'D'
121700960419     C     ANMFT1        ANDEQ     *BLANKS
121800960419     C     V1HLIO        ANDEQ     V1SLID
121900960506     C     V1SCAA        ANDNE     '070'
122000960506     C     V1SCAA        ANDNE     '071'
122100960419     C* . . . Protezione
122200960412     C                   SETON                                        05
122300960412   X6C                   ELSE
122400960419     C* . . . Sprotezione
122500960412     C                   SETOFF                                       05
122600960412    6C                   END
122700960412     C                   MOVE      *IN05         V1HIN5
122800960412   X5C                   ELSE
122900960412     C                   MOVEL     MSG(16)       V1ZMSG
123000960412     C                   SETON                                        4728
123100960412     C                   GOTO      ENDCTA
123200960412    5C                   END
123300960412   4BC                   END
123400960412   X4C                   ELSE
123500960412     C* CAMPI GIA' SPROTETTI --> LI CONTROLLO
123600960412     C                   EXSR      CTRS01
123700960412    4C                   END
123800960412     C                   MOVE      *BLANKS       V1SVAR
123900960412   X3C                   ELSE
124000960412     C* CAMPI GIA' SPROTETTI --> LI CONTROLLO
124100960412    4C     V1HVAR        IFEQ      'V'
124200960423     C* Se premuto tasto di conferma e anomalia già trasmessa: errore
124300960423     C   KFANMFT1        IFNE      *BLANKS
124400960423     C     V1HLIO        ORNE      V1SLID
124500960506     C     V1SCAA        ANDNE     '070'
124600960506     C     V1SCAA        ANDNE     '071'
124700960423     C                   MOVEL     MSG(30)       V1ZMSG
124800960423     C                   SETON                                        040528
124900960423     C                   SETOFF                                       09
125000960423     C                   MOVE      V1HNRR        D44NRR
125100960423     C                   EXSR      RIS01A
125200960423     C                   SETON                                        2247
125300960423     C                   GOTO      ENDCTA
125400960423     C                   END
125500960412     C                   EXSR      CTRS01
125600960412    4C                   END
125700960412    3C                   END
125800960329     C*
125900960509    2C                   END
126000960401     C     ENDCTA        TAG
126100960402     C                   MOVE      V1HIN4        *IN04
126200960402     C                   MOVE      V1HIN5        *IN05
126300960405     C                   MOVE      V1HIN9        *IN09
126400960426     C     *IN28         IFEQ      *ON
126500960513     C     V1SVAR        ORNE      *BLANKS
126600960426     C     SAVNBR        ANDEQ     ' '
126700960426     C     V1HVAR        OREQ      'V'
126800960426     C     SAVNBR        ANDEQ     ' '
126900960329     C                   Z-ADD     NRR1          V1CNBR
127000960426     C                   MOVE      '1'           SAVNBR
127100960426     C                   END
127200960523     C                   MOVE      V1HI51        *IN51
127300960523     C                   MOVE      V1HI52        *IN52
127400960329     C                   UPDATE    LR44S01
127500971103     C   28              SETON                                        2990
127600960329     C  N29              READC     LR44S01                                29
127700960329    1C                   ENDDO
127800960401     C                   ENDSR
127900960328     C******  CONTROLLI SUBFILE  MODALITA' INSERIMENTO     ************
128000960328     C     CTS01B        BEGSR
128100960403     C                   SETON                                        22
128200960405     C* PULIZIA CAMPI DI COMODO
128300960405     C                   MOVE      *ZEROS        II
128400960405     C                   MOVE      *ZEROS        LL
128500960405     C                   CLEAR                   SKB
128600960405     C                   CLEAR                   SKS
128700960329     C* azzero campi per gestione DUP
128800960329     C                   CLEAR                   DUPAAS
128900960329     C                   CLEAR                   DUPCAA
129000960401     C                   CLEAR                   DUPLNP
129100960401     C                   CLEAR                   DUPNRS
129200960401     C                   CLEAR                   DUPNSP
129300960401     C                   CLEAR                   DUPLNA
129400960401     C                   CLEAR                   DUPFLS
129500960401     C                   CLEAR                   DUPSCN
129600960401     C                   CLEAR                   DUPDAO
129700960329     C*
129800960329     C                   READC     LR44S01                                29
129900960329    1C     *IN29         DOWEQ     *OFF
130000960329     C                   CLEAR                   V1DCAA
130100960329     C                   CLEAR                   V1DSAN
130200960329     C                   CLEAR                   V1DTAN
130300960329     C                   CLEAR                   V1SAIE
130400960507     C                   CLEAR                   V1HAIE
130500960329     C*   Gestione tasto DUP
130600960401     C   11              MOVE      DUPCAA        V1SCAA
130700960401     C   12              MOVE      DUPAAS        V1SAAS
130800960401     C   13              MOVE      DUPNRS        V1SNRS
130900960401     C   14              MOVE      DUPNSP        V1SNSP
131000960401     C   15              MOVE      DUPLNA        V1SLNA
131100960401     C   16              MOVE      DUPLNP        V1SLNP
131200960401     C   17              MOVE      DUPSCN        V1SSCN
131300960401     C   18              MOVE      DUPDAO        V1SDAO
131400960401     C   19              MOVE      DUPFLS        V1SFLS
131500960329     C* CONTROLLO SOLO SE IMMESSI DATI
131600960329    2C     IOFLD         IFGT      *ZEROS
131700960329     C     V1SCAA        ORNE      *BLANKS
131800960329     C     V1SCAA        ANDNE     *ZEROS
131900960329     C                   EXSR      CTRS01
132000960409     C                   ELSE
132100960409     C                   CLEAR                   V1HKSP
132200970507     C                   CLEAR                   V1HARB
132300960409     C                   CLEAR                   V1HPRE
132400960329    2C                   END
132500960523     C                   MOVE      V1HI51        *IN51
132600960523     C                   MOVE      V1HI52        *IN52
132700960329     C                   Z-ADD     NRR1          V1CNBR
132800960329     C                   UPDATE    LR44S01
132900971103     C   28              SETON                                        2990
133000960329     C  N29              READC     LR44S01                                29
133100960329    1C                   ENDDO
133200960328     C                   ENDSR
133300960329     C*************  CONTROLLI COMUNI SUBFILE  ************************
133400960329     C     CTRS01        BEGSR
133500960506     C*** CODICE ANOMALIA  (SOLO SE SPROTETTO)
133600960402   1AC     V1HIN5        IFEQ      '0'
133700960329     C*   Ricerca in tabella
133800960401     C     '?'           SCAN      V1SCAA                                 30
133900960401    1C     *IN30         IFEQ      *ON
134000960329     C                   MOVEL     CODUT         §KUT
134100960329     C                   MOVEL     '7C'          §COD
134200960329     C                   MOVEL     *BLANKS       §KEY
134300960329     C                   CALL      'X§TABER'
134400960329     C                   PARM                    §KUT              1 0
134500960329     C                   PARM                    §COD              2
134600960329     C                   PARM                    §KEY              8
134700960329     C                   PARM                    §DES             30
134800960329     C                   MOVEL     §KEY          V1SCAA
134900960401    2C     V1SCAA        IFNE      *BLANKS
135000960329     C                   MOVEL     §DES          V1DCAA
135100960401    2C                   END
135200971103     C                   SETON                                        2990
135300960329     C                   GOTO      ENDCTR
135400960401    1C                   END
135500960329     C*   Controlli di validità'
135600960401    1C     V1SCAA        IFEQ      *ZEROS
135700960329     C     V1SCAA        OREQ      *BLANKS
135800960329     C                   MOVEL     MSG(2)        V1ZMSG
135900960401     C                   SETON                                        4028
136000960329     C                   GOTO      ENDCTR
136100960401    1C                   END
136200960329     C                   TESTN                   V1SCAA               30
136300960401    1C     *IN30         IFEQ      *ON
136400960329     C                   MOVE      V1SCAA        W001A             1
136500960401    2C     W001A         IFLT      '0'
136600960329     C                   SETOFF                                       30
136700960401    2C                   END
136800960401    1C                   END
136900960401    1C     *IN30         IFEQ      *OFF
137000960329     C                   MOVEL     MSG(2)        V1ZMSG
137100960401     C                   SETON                                        4028
137200960329     C                   GOTO      ENDCTR
137300960401    1C                   END
137400960329     C* Chaino tabella per reperimento altri dati
137500960329     C                   MOVEL     '7C'          KCOD
137600960329     C                   MOVEL(P)  V1SCAA        KKEY
137700960329     C     KTAB          CHAIN     TABEL00F                           30
137800960401    1C     *IN30         IFEQ      *ON
137900960329     C     TBLFLG        ORNE      *BLANKS
138000960329     C                   MOVEL     MSG(2)        V1ZMSG
138100960401     C                   SETON                                        4028
138200960329     C                   GOTO      ENDCTR
138300960401    1C                   END
138400960329     C                   MOVEL     TBLUNI        DS7C
138500960329     C                   MOVEL     §7CDES        V1DCAA
138600960506     C*  Controllo che codice anomalia sia gestibile in inserimento
138700960402    1C     §7CGES        IFEQ      *BLANKS
138800960329     C                   MOVEL     MSG(5)        V1ZMSG
138900960401     C                   SETON                                        4028
139000960329     C                   GOTO      ENDCTR
139100960401    1C                   END
139200960506     C*  Se codice ok imposto anche altri campi di output del video
139300960402     C*  (N.B.: v1hges e v1hann mantengo quelli di origine)
139400960329     C                   MOVEL     §7CTAN        V1HTAN
139500960329     C                   MOVEL     §7CSAN        V1HSAN
139600960402     C                   MOVEL     §7CKEY        V1HKEY
139700960411     C                   MOVEL     §7CUEA        V1HUEA
139800960329     C                   MOVEL     '7F'          KCOD
139900960329     C                   MOVEL(P)  §7CTAN        KKEY
140000960329     C     KTAB          CHAIN     TABEL00F                           30
140100960329     C  N30              MOVEL     TBLUNI        V1DTAN
140200960329     C                   MOVEL     '7G'          KCOD
140300960329     C                   MOVEL(P)  §7CSAN        KKEY
140400960329     C     KTAB          CHAIN     TABEL00F                           30
140500960329     C  N30              MOVEL     TBLUNI        V1DSAN
140600960507     C                   MOVEL     §7CAIE        V1HAIE
140700960507     C     §7CIDD        IFEQ      'S'
140800960507     C                   MOVEL     'D'           V1SAIE
140900960507     C                   ELSE
141000960507     C                   MOVEL     'N'           V1SAIE
141100960507     C                   END
141200960402   1AC                   END
141300960402     C* CONTROLLO LINEE SE IMMESSE
141400960402     C* - LA LNP, SE IMMESSA, Deve esistere in organigramma
141500960402    1C     V1SLNP        IFGT      *ZEROS
141600960402     C     V1SLNP        CHAIN     AZORG01L                           30
141700960402    2C     *IN30         IFEQ      *ON
141800960402     C     ORGFVA        ORNE      *BLANKS
141900960402     C                   MOVEL     MSG(3)        V1ZMSG
142000960402     C                   SETON                                        4328
142100960402     C                   GOTO      ENDCTR
142200960402    2C                   END
142300960402    1C                   END
142400960402     C* - LA Fls, SE IMMESSA, Deve esistere in organigramma
142500960402    1C     V1SFLS        IFGT      *ZEROS
142600960402     C     V1SFLS        CHAIN     AZORG01L                           30
142700960402    2C     *IN30         IFEQ      *ON
142800960402     C     ORGFVA        ORNE      *BLANKS
142900960402     C                   MOVEL     MSG(4)        V1ZMSG
143000960402     C                   SETON                                        4828
143100960402     C                   GOTO      ENDCTR
143200960402    2C                   END
143300960402    1C                   END
143400960411     C* - La Lna deve essere una filiale in gestione
143500960418     C*   (Se è un'anomalia per la quale occorre utilizzare la LNA
143600960418     C*   anzichè la filiale emittente)
143700960402    1C     V1SLNA        IFGT      *ZEROS
143800960411    2C     V1HUEA        IFEQ      *BLANKS
143900960411    3C     V1SLNA        IFNE      D34FGS
144000960402     C     V1SLNA        LOOKUP    L6S                                    30
144100960411    4C     *IN30         IFEQ      *OFF
144200960402     C                   MOVEL     MSG(8)        V1ZMSG
144300960402     C                   SETON                                        4528
144400960402     C                   GOTO      ENDCTR
144500960411    4C                   END
144600960411    3C                   END
144700960411   X2C                   ELSE
144800960524     C* - La Lna deve esistere in organigramma e deve essere diversa
144900960524     C*   dalla filiale emittente
145000960411     C*   (Se per questa anomalia occorre utilizzare la filiale emit-
145100960411     C*   tente)
145200960524    3C     V1SLNA        IFEQ      D34FGS
145300960524     C                   MOVEL     MSG(36)       V1ZMSG
145400960524     C                   SETON                                        4528
145500960524     C                   GOTO      ENDCTR
145600960524    3C                   END
145700960411     C     V1SLNA        SETLL     AZORG01L                               30
145800960411    3C     *IN30         IFEQ      *OFF
145900960411     C                   MOVEL     MSG(7)        V1ZMSG
146000960411     C                   SETON                                        4528
146100960411     C                   GOTO      ENDCTR
146200960411    3C                   END
146300960402    2C                   END
146400960402    1C                   END
146500960401     C***** CONTROLLO CHIAVE PRINCIPALE:
146600960401    1C                   SELECT
146700960402     C*     - PER SPEDIZIONE (AAS/LNP/NRS/NSP/LNA)
146800960402     C     V1HKEY        WHENEQ    'B'
146900960401     C* LINEA DI PARTENZA OBBLIGATORIA
147000960401    2C     V1SLNP        IFEQ      *ZEROS
147100960401     C                   SETON                                        4328
147200960329     C                   MOVEL     MSG(3)        V1ZMSG
147300960329     C                   GOTO      ENDCTR
147400960401    2C                   END
147500960401     C* NUMERO SPEDIZIONE OBBLIGATORIO
147600960401    2C     V1SNSP        IFEQ      *ZEROS
147700960401     C                   SETON                                        4128
147800960401     C                   MOVEL     MSG(6)        V1ZMSG
147900960401     C                   GOTO      ENDCTR
148000960401    2C                   END
148100960402     C     V1HKEY        WHENEQ    'S'
148200960401     C*    - PER SEGNACOLLO (FLS/LNA/NRS/SCN)
148300960401     C* FILIALE SEGNACOLLO OBBLIGATORIO
148400960401    2C     V1SFLS        IFEQ      *ZEROS
148500960401     C                   SETON                                        4828
148600960401     C                   MOVEL     MSG(4)        V1ZMSG
148700960401     C                   GOTO      ENDCTR
148800960401    2C                   END
148900960401     C* NUMERO SEGNACOLLO OBBLIGATORIO
149000960401    2C     V1SSCN        IFEQ      *ZEROS
149100960401     C                   SETON                                        4228
149200960401     C                   MOVEL     MSG(17)       V1ZMSG
149300960401     C                   GOTO      ENDCTR
149400960401    2C                   END
149500960401    1C                   ENDSL
149600960402     C* LINEA DI ARRIVO SEMPRE OBBLIGATORIA
149700960402     C* (Sia in key per spedizione che in key per segnacollo)
149800960402    2C     V1SLNA        IFEQ      *ZEROS
149900960402     C                   MOVEL     MSG(7)        V1ZMSG
150000960402     C                   SETON                                        4528
150100960402     C                   GOTO      ENDCTR
150200960402    2C                   END
150300960401     C**** DATA APERTURA ANOMALIA
150400960401    1C     V1SDAO        IFEQ      *ZEROS
150500960401     C                   MOVEL     MSG(9)        V1ZMSG
150600960401     C                   SETON                                        4628
150700960401     C                   GOTO      ENDCTR
150800960401    1C                   END
150900990513     C                   CLEAR                   WLBDA8
151000990513     C                   MOVE      V1SDAO        G08DAT
151100990513     C                   CALL      'XSRDA8'
151200990513     C                   PARM                    WLBDA8
151300990513    1C     G08ERR        IFEQ      '1'
151400960401     C                   MOVEL     MSG(9)        V1ZMSG
151500960401     C                   SETON                                        4628
151600960401     C                   GOTO      ENDCTR
151700960401   X1C                   ELSE
151800990513     C                   MOVE      G08INV        V1HDAO
151900960401    1C                   END
152000960401     C**** CONTROLLI DI CHIAVE DOPPIA
152100960403     C                   CLEAR                   WFLAG
152200960830     C                   CLEAR                   SAVFLS
152300960830     C                   CLEAR                   SAVSCN
152400960404     C     V1SCAA        IFEQ      '070'
152500960404     C     V1SCAA        OREQ      '071'
152600960404     C                   CLEAR                   V1SLID
152700960404     C                   END
152800960401     C                   MOVE      V1SCAA        WCAA
152900960401    1C                   SELECT
153000960402     C     V1HKEY        WHENEQ    'B'
153100990513     C     V1SAAS        IFGT      60
153200990513     C                   MOVEL     19            WAAS
153300990513     C                   ELSE
153400990513     C                   MOVEL     20            WAAS
153500990513     C                   END
153600990513     C                   MOVE      V1SAAS        WAAS
153700990512     C     KANM1         SETGT     FNANM01L
153800990512     C     KANM1         READPE    FNANM01L                               30
153900960401    2C     *IN30         DOWEQ     *OFF
154000960401    3C     V1HNRR        IFNE      ANMNR1
154100960401     C                   EXSR      CTRDOP
154200960401     C   28              GOTO      ENDCTR
154300960401    3C                   END
154400960404     C* SOLO PER ANOMALIE 070-ECCEDE BAM E 071-MANCA BAM: MEMORIZZO IL
154500960404     C* LIVELLO IDD DELL'ANOMALIA PIù RECENTE.
154600960830     C* MEMORIZZO ANCHE FILIALE E NUMERO SEGNAC. > 0 PER POI
154700960830     C* CONFRONTARLI CON QUELLI IMMESSI EVENTUALMENTE A VIDEO
154800960404     C     V1SCAA        IFEQ      '070'
154900960404     C     V1SCAA        OREQ      '071'
155000960404     C     WFLAG         IFEQ      *BLANKS
155100960506     C     ANMFLE        ANDEQ     D34FGS
155200960404     C                   MOVE      ANMLID        V1SLID
155300960404     C                   MOVE      '1'           WFLAG             1
155400960404     C                   END
155500960830     C     ANMFLS        IFGT      *ZEROS
155600960830     C                   MOVE      ANMFLS        SAVFLS
155700960830     C                   MOVE      ANMSCN        SAVSCN
155800960830     C                   END
155900960404     C                   END
156000990512     C     KANM1         READPE    FNANM01L                               30
156100960401    2C                   ENDDO
156200960401     C*
156300960402     C     V1HKEY        WHENEQ    'S'
156400990512     C     KANM2         SETGT     FNANM02L
156500990512     C     KANM2         READPE    FNANM02L                               30
156600960401    2C     *IN30         DOWEQ     *OFF
156700960401    3C     V1HNRR        IFNE      ANMNR2
156800960401     C                   EXSR      CTRDOP
156900960401     C   28              GOTO      ENDCTR
157000960401    3C                   END
157100990512     C     KANM2         READPE    FNANM02L                               30
157200960401    2C                   ENDDO
157300960401    1C                   ENDSL
157400960405     C********************
157500960405     C* SOLO IN MANUTENZIONE :
157600960405     C*********************
157700960403     C* VERIFICO SE OCCORRE IMPOSTARE NUOVO LIVELLO IDD
157800960403    1C     *IN10         IFEQ      *OFF
157900960403    2C     WCAA          IFNE      V1HCAA
158000960403    3C     §7CIDD        IFEQ      'S'
158100960404     C                   MOVE      §7CLID        V1SLID
158200960506     C                   ELSE
158300960506     C                   CLEAR                   V1SLID
158400960403    3C                   END
158500960403   X2C                   ELSE
158600960404     C     V1SLID        IFEQ      *BLANKS
158700960404     C                   MOVE      V1HLIO        V1SLID
158800960404     C                   END
158900960403    2C                   END
159000960403   X1C                   ELSE
159100960405     C********************
159200960405     C* SOLO IN IMMISSIONE:
159300960405     C********************
159400960405     C* VERIFICO SE OCCORRE IMPOSTARE NUOVO LIVELLO IDD
159500960403    2C     V1SLID        IFEQ      *BLANKS
159600960403     C     §7CIDD        ANDEQ     'S'
159700960403     C                   MOVE      §7CLID        V1SLID
159800960403    2C                   END
159900960405     C* Controllo cha non sia già stato inserito un record con la
160000960409     C* stessa chiave
160100960405    2C                   SELECT
160200960405     C     V1HKEY        WHENEQ    'B'
160300960405     C                   MOVE      V1SAAS        WSAAS
160400960405     C                   MOVE      V1SLNP        WSLNP
160500960405     C                   MOVE      V1SNRS        WSNRB
160600960405     C                   MOVE      V1SNSP        WSNSP
160700960405     C                   MOVE      V1SCAA        WSCAB
160800960405     C     WKEYB         LOOKUP    SKB                                    30
160900960405    3C     *IN30         IFEQ      *ON
161000960405     C                   MOVEL     MSG(26)       V1ZMSG
161100960405     C                   SETON                                        284443
161200960405     C                   SETON                                        494140
161300960405    3C                   END
161400960405     C                   ADD       1             II                3 0
161500960405     C                   MOVE      WKEYB         SKB(II)
161600960405     C     V1HKEY        WHENEQ    'S'
161700960405     C                   MOVE      V1SFLS        WSFLS
161800960405     C                   MOVE      V1SLNA        WSLNA
161900960405     C                   MOVE      V1SNRS        WSNRS
162000960405     C                   MOVE      V1SSCN        WSSCN
162100960405     C                   MOVE      V1SCAA        WSCAS
162200960405     C     WKEYS         LOOKUP    SKS                                    30
162300960405    3C     *IN30         IFEQ      *ON
162400960405     C                   MOVEL     MSG(26)       V1ZMSG
162500960405     C                   SETON                                        284845
162600960405     C                   SETON                                        494240
162700960405    3C                   END
162800960405     C                   ADD       1             LL                3 0
162900960405     C                   MOVE      WKEYS         SKS(LL)
163000960405    2C                   ENDSL
163100960405     C   28              GOTO      ENDCTR
163200960403    1C                   END
163300960402     C* CONTROLLO COMPLETEZZA CHIAVE FACOLTATIVA, SE IMMESSA
163400960402    1C                   SELECT
163500960402     C     V1HKEY        WHENEQ    'B'
163600960402     C* Immessa filiale segnacollo e non il numero
163700960402    2C     V1SFLS        IFGT      *ZEROS
163800960402     C     V1SSCN        ANDEQ     *ZEROS
163900960403     C                   SETON                                        2842
164000960402     C                   MOVEL     MSG(19)       V1ZMSG
164100960402   X2C                   ELSE
164200960402     C* Immesso il numero segnacollo e non la filiale
164300960402    3C     V1SFLS        IFEQ      *ZEROS
164400960402     C     V1SSCN        ANDGT     *ZEROS
164500960403     C                   SETON                                        2848
164600960402     C                   MOVEL     MSG(21)       V1ZMSG
164700960402    3C                   END
164800960402    2C                   END
164900960402     C     V1HKEY        WHENEQ    'S'
165000960402    2C     KEYNSP        IFGT      *ZEROS
165100960402    3C     V1SNSP        IFEQ      *ZEROS
165200960403     C                   SETON                                        2841
165300960402     C                   MOVEL     MSG(25)       V1ZMSG
165400960402   X3C                   ELSE
165500000225     C     V1SLNP        IFEQ      *ZEROS
165600960403     C                   SETON                                        284344
165700960402     C                   MOVEL     MSG(24)       V1ZMSG
165800960402    4C                   END
165900960402    3C                   END
166000960402    2C                   END
166100960402    1C                   ENDSL
166200960402     C   28              GOTO      ENDCTR
166300960409     C*
166400960409     C******************************
166500960409     C* Solo per anomalie 070 e 071:
166600960409     C******************************
166700960409    1C     V1SCAA        IFEQ      '070'
166800960409     C     V1SCAA        OREQ      '071'
166900960830     C* Errore se immessi filiale/numero segnacollo diversi da quelli
167000960830     C* di un'altra anomalia con la stessa chiave (questo controllo
167100960830     C* serve per ricevere correttamente le anomalie visto che il
167200960830     C* programma di ricezione se fls/scn>0 legge le anomalie con key
167300960830     C* segnacollo)
167400960830     C     V1SFLS        IFGT      *ZEROS
167500960830     C     SAVFLS        ANDGT     *ZEROS
167600960830     C     V1SFLS        IFNE      SAVFLS
167700960830     C     V1SSCN        ORNE      SAVSCN
167800960830     C                   MOVEA     MSG(37)       WMS
167900960830     C                   MOVE      SAVFLS        W003A             3
168000960830     C                   MOVEA     W003A         WMS(67)
168100960830     C                   MOVE      SAVSCN        W007A             7
168200960830     C                   MOVEA     W007A         WMS(71)
168300960830     C                   MOVEA     WMS           V1ZMSG
168400960830     C                   SETON                                        284248
168500960830     C                   GOTO      ENDCTR
168600960830     C                   END
168700960830     C                   END
168800960830     C*
168900960409    2C     V1SCAA        IFEQ      '070'
169000960409     C                   MOVE      071           WSCAB
169100960409   X2C                   ELSE
169200960409     C                   MOVE      070           WSCAB
169300960409    2C                   END
169400960409     C                   MOVE      V1SAAS        WSAAS
169500960409     C                   MOVE      V1SLNP        WSLNP
169600960409     C                   MOVE      V1SNRS        WSNRB
169700960409     C                   MOVE      V1SNSP        WSNSP
169800960409     C************
169900960409     C* Immissione
170000960409     C************
170100960409    2C     *IN10         IFEQ      *ON
170200960506     C* Controllo che per la stessa bolla non sia stato immesso sia il
170300960506     C* codice 070 che 071
170400960409     C     WKEYB         LOOKUP    SKB                                    30
170500960409    3C     *IN30         IFEQ      *ON
170600960409     C                   MOVEL     MSG(27)       V1ZMSG
170700960409     C                   SETON                                        284443
170800960409     C                   SETON                                        4941
170900960409     C                   GOTO      ENDCTR
171000960409    3C                   END
171100960409    2C                   END
171200960830     C*****************************
171300960830     C* Manutenzione o inserimento:
171400960830     C*****************************
171500960409    2C     *IN10         IFEQ      *OFF
171600960409     C     WCAA          ANDNE     V1HCAA
171700960409     C     *IN10         OREQ      *ON
171800960506     C                   CLEAR                   WNTRM
171900960506     C                   CLEAR                   WTRM
172000960506     C                   CLEAR                   WNMIA
172100960506     C                   CLEAR                   WFLE
172200960506     C                   MOVE      WSCAB         WCAA
172300990513     C     V1SAAS        IFGT      60
172400990513     C                   MOVEL     19            WAAS
172500990513     C                   ELSE
172600990513     C                   MOVEL     20            WAAS
172700990513     C                   END
172800990513     C                   MOVE      V1SAAS        WAAS
172900990512     C     KANM1         SETLL     FNANM01L
173000990512     C     KANM1         READE     FNANM01L                               30
173100960506    3C     *IN30         DOWEQ     *OFF
173200960506     C     WNTRM         ANDEQ     *BLANKS
173300960409     C* Se in manutenzione scarto me stesso
173400960506    4C     *IN10         IFEQ      *OFF
173500960409     C     V1HNRR        ANDEQ     ANMNR1
173600960506     C                   GOTO      LEGGI
173700960506    4C                   END
173800960506     C* Anomalia non mia
173900960506    4C     ANMFLE        IFNE      D34FGS
174000960506     C                   MOVE      '1'           WNMIA             1
174100960506     C                   MOVE      ANMFLE        WFLE
174200960506   X4C                   ELSE
174300960506    5C     ANMFT1        IFEQ      *BLANKS
174400960506     C* Mia anomalia non trasmessa
174500960506     C                   MOVE      '1'           WNTRM             1
174600960506   x5C                   ELSE
174700960506    6C     ANMFT1        IFNE      *BLANKS
174800960506     C* Mia anomalia trasmessa
174900960506     C                   MOVE      '1'           WTRM              1
175000960506    6C                   END
175100960506    5C                   END
175200960506    4C                   END
175300960506     C     LEGGI         TAG
175400990512     C     KANM1         READE     FNANM01L                               30
175500960506    3C                   ENDDO
175600970507     C**
175700970507     C* SE E' MODIFICA LA SPEDIZIONE CONTROLLO SE C'E' IN ARB
175800970507     C     WKEYB         IFNE      V1HKSP
175900970507     C                   CLEAR                   V1HARB
176000970507     C                   ENDIF
176100970507     C**
176200960506    3C     WNTRM         IFEQ      '1'
176300960506     C     WTRM          OREQ      '1'
176400960506     C     WNMIA         OREQ      '1'
176500960506     C* Decodifico codice anomalia
176600960506     C                   MOVE      *BLANKS       WDES             24
176700960506     C                   MOVE      *BLANKS       WDES13           13
176800960409     C                   MOVEL     '7C'          KCOD
176900960409     C                   MOVEL(P)  ANMCAA        KKEY
177000960409     C     KTAB          CHAIN     TABEL00F                           30
177100960409     C  N30              MOVEL     TBLUNI        WDES
177200960506     C  N30              MOVEL     TBLUNI        WDES13
177300960506     C* Mia anomalia non trasmessa: errore bloccante
177400960506    4C     WNTRM         IFEQ      '1'
177500960506     C                   SETON                                        284443
177600960506     C                   SETON                                        4941
177700960506     C                   MOVEA     MSG(29)       WMS
177800960506     C                   MOVE      ANMCAA        W003A             3
177900960506     C                   MOVEA     W003A         WMS(38)
178000960506     C                   MOVEA     WDES          WMS(42)
178100960506     C                   MOVEA     WMS           V1ZMSG
178200960506     C                   GOTO      ENDCTR
178300960506   x4C                   ELSE
178400960506     C*
178500960506     C* Mia anomalia già trasmessa o anomalia non mia: errore forzabile
178600960506    5C                   SELECT
178700960506     C     WTRM          WHENEQ    '1'
178800960506     C     WKEYB         IFNE      V1HKSP
178900960506     C                   SETON                                        284443
179000960506     C                   SETON                                        4941
179100960506     C                   MOVEA     MSG(28)       WMS
179200960506     C                   MOVE      ANMCAA        W003A             3
179300960506     C                   MOVEA     W003A         WMS(38)
179400960506     C                   MOVEA     WDES          WMS(42)
179500960506     C                   MOVEA     WMS           V1ZMSG
179600960506     C                   GOTO      ENDCTR
179700960506     C                   END
179800960506     C     WNMIA         WHENEQ    '1'
179900960506     C     WKEYB         IFNE      V1HKSP
180000960506     C                   SETON                                        284443
180100960506     C                   SETON                                        4941
180200960506     C                   MOVEA     MSG(35)       WMS
180300960409     C                   MOVE      ANMCAA        W003A             3
180400960506     C                   MOVEA     W003A         WMS(34)
180500960506     C                   MOVEA     WDES13        WMS(38)
180600960506     C                   MOVE      WFLE          W003A
180700960506     C                   MOVEA     W003A         WMS(63)
180800960409     C                   MOVEA     WMS           V1ZMSG
180900960409     C                   GOTO      ENDCTR
181000960506     C                   END
181100960506    5C                   ENDSL
181200960506    4C                   END
181300960506    3C                   END
181400970507     C*
181500970507     C* IN MODIFICA: SE CAMBIATO SOLO IL NUMERO SPEDIZIONE RICONTROLLO
181600970507     C*  SU FNARB
181700970507   X2C                   ELSE
181800970507     C**
181900970507     C* SE E' MODIFICA LA SPEDIZIONE CONTROLLO SE C'E' IN ARB
182000970507     C     WKEYB         IFNE      V1HKSP
182100970507     C                   CLEAR                   V1HARB
182200970507     C                   ENDIF
182300960506    2C                   END
182400960506    1C                   END
182500970507     C*****
182600970507     C* SOLO PER ANOMALIA MANCA DDT
182700970507     C*****
182800970507     C* SE MODIFICATA LA SPEDIZIONE RICONTROLLO
182900970507    1C     V1SCAA        IFEQ      '071'
183000970507     C* VERIFICO SE C'E' LA BOLLA--> MESSAGGIO FORZABILE
183100970507     C     V1HARB        ANDEQ     ' '
183200990513     C     V1SAAS        IFGT      60
183300970507     C                   MOVEL     19            WAAS
183400970507     C                   ELSE
183500970507     C                   MOVEL     20            WAAS
183600970507     C                   END
183700970507     C                   MOVE      V1SAAS        WAAS
183800970507     C*
183900970507     C     KBLP          SETLL     FNARB01L                               30
184000970507     C*
184100970507    2C     *IN30         IFEQ      *OFF
184200970507     C                   MOVEL     'S'           V1HARB
184300970507     C                   SETON                                        284443
184400970507     C                   SETON                                        4941
184500970507     C                   MOVEA     MSG(39)       V1ZMSG
184600970507     C                   GOTO      ENDCTR
184700970507   X2C                   ELSE
184800970507     C* CONTROLLO SE E' UN NO DDT --> ERRORE NON FORZABILE
184900060216     C     KAR4          CHAIN     FIAR401L                           30
185000970507    3C     *IN30         IFEQ      *OFF
185100031028     c                   Movel     Ar4not        dsbl4a
185200031028     c                   Move      §b4abm        wddt              1
185300970507     C*
185400970507    4C     WDDT          IFNE      'S'
185500970507     C     WDDT          ANDNE     'C'
185600031028     c     wddt          andne     'P'
185700031028     c     wddt          andne     'Q'
185800970507     C                   SETON                                        284443
185900970507     C                   SETON                                        4941
186000970507     C                   MOVEA     MSG(40)       V1ZMSG
186100970507     C                   GOTO      ENDCTR
186200970507     C*
186300970507   X4C                   ELSE
186400970507     C                   MOVEL     'S'           V1HARB
186500970507    4C                   ENDIF
186600970507     C*
186700970507   X3C                   ELSE
186800970507     C                   MOVEL     'S'           V1HARB
186900970507    3C                   ENDIF
187000970507    2C                   ENDIF
187100970507    1C                   ENDIF
187200970507     C*
187300960401     C* Memorizzo CAMPI per DUP
187400960401     C                   MOVE      V1SCAA        DUPCAA
187500960401     C                   MOVE      V1SAAS        DUPAAS
187600960401     C                   MOVE      V1SLNP        DUPLNP
187700960401     C                   MOVE      V1SNRS        DUPNRS
187800960401     C                   MOVE      V1SNSP        DUPNSP
187900960401     C                   MOVE      V1SFLS        DUPFLS
188000960401     C                   MOVE      V1SSCN        DUPSCN
188100960401     C                   MOVE      V1SLNA        DUPLNA
188200960401     C                   MOVE      V1SDAO        DUPDAO
188300960401     C* MESSAGGI DI AVVISO:
188400971118     C* Avviso se data apertura anomalia > di data del giorno
188500990513    1C     V1HDAO        IFGT      DATEU8
188600960401     C     V1HDAO        ANDNE     V1HPRE
188700960401     C                   SETON                                        4628
188800960401     C                   MOVEL     MSG(23)       V1ZMSG
188900960409     C                   MOVE      V1HDAO        V1HPRE
189000960401    1C                   END
189100960409     C     ENDCTR        TAG
189200960410    1C     V1SCAA        IFEQ      '070'
189300960410     C     V1SCAA        OREQ      '071'
189400960410     C     WKEYB         IFNE      V1HKSP
189500960410     C                   MOVE      WKEYB         V1HKSP
189600960410     C                   END
189700960410     C                   END
189800960409     C                   ENDSR
189900960401     C*************  CONTROLLI DI CHIAVE DOPPIA ***********************
190000960401     C     CTRDOP        BEGSR
190100960418     C*
190200960418    1C     V1SCAA        IFEQ      '012'
190300960418     C                   SETON                                        444341
190400960418     C                   SETON                                        404928
190500960418     C                   MOVEL     MSG(32)       V1ZMSG
190600960418     C                   GOTO      ENDDOP
190700960418    1C                   END
190800960418     C*
190900960401    1C     ANMDCH        IFEQ      *ZEROS
191000960405     C     V1HDAO        ORLE      ANMDAO
191100960403     C     ANMLID        ORGE      WLCI
191200960401    2C                   SELECT
191300960402     C     V1HKEY        WHENEQ    'B'
191400960401     C                   SETON                                        444341
191500960402     C     V1HKEY        WHENEQ    'S'
191600960403     C                   SETON                                        4842
191700960401    2C                   ENDSL
191800960403    2C                   SELECT
191900960403     C* Errore di anomalia già aperta
192000960403     C     ANMDCH        WHENEQ    *ZEROS
192100960401     C                   MOVEL     MSG(18)       V1ZMSG
192200960403     C* errore di esistenza anomalia con data apertura più recente
192300960403     C     V1HDAO        WHENLT    ANMDAO
192400960401     C                   MOVEL     MSG(20)       V1ZMSG
192500960403     C                   SETON                                        46
192600960403     C* Errore se anomalia già CHIUSA COME IDD
192700960403     C     ANMLID        WHENGE    WLCI
192800960401     C                   MOVEL     MSG(22)       V1ZMSG
192900960403    2C                   ENDSL
193000960403     C*
193100960403     C                   SETON                                        28
193200960403     C                   SETON                                        404549
193300960403     C                   GOTO      ENDDOP
193400960403    1C                   END
193500960401     C     ENDDOP        ENDSR
193600960429     C****** CONTROLLI PARTICOLARI/DI ANNULLABILITA'/RIPR. ************
193700960429     C     CTRANN        BEGSR
193800960429     C                   CLEAR                   WFLG01
193900960429     C                   SETOFF                                       30
194000960429     C                   SELECT
194100960429     C     V1HKEY        WHENEQ    'B'
194200990513     C* Imposto anno a 4 cifre
194300041124     C     V1SAAS        IFGT      80
194400990513     C                   MOVEL     19            WAAS
194500990513     C                   ELSE
194600990513     C                   MOVEL     20            WAAS
194700990513     C                   END
194800990513     C                   MOVE      V1SAAS        WAAS
194900960429     C     KBLP          SETLL     FNBLP01L                               30
195000960429     C     V1HKEY        WHENEQ    'S'
195100090311     C     Kart          SETLL     Fiart27L                               30
195500960429     C                   ENDSL
195600960429     C   30              MOVE      'E'           WFLG01            1
195700960429     C                   ENDSR
195800960402     C***********  INTERROGAZIONE DI UN RECORD SINGOLO  ***************
195900960402     C     SRINT         BEGSR
196000990512     C     V1HNRR        CHAIN(N)  FNANM00F                           30
196100960605    1C     *IN30         IFEQ      *OFF
196200960403     C     ANMATB        COMP      *BLANKS                            0606
196300960403     C                   MOVE      ANMAAS        V2CAAS
196400960403     C                   MOVE      ANMLNP        V2CLNP
196500960605     C                   MOVE      ANMNSP        V2CNSP
196600960605    2C     ANMNSP        IFGT      *ZEROS
196700960403     C                   MOVE      ANMNRS        V2CSRB
196800960403     C                   MOVE      ANMLNA        V2CLAB
196900960605   X2C                   ELSE
197000960605     C                   CLEAR                   V2CSRB
197100960605     C                   CLEAR                   V2CLAB
197200960605     C* Cerco spedizione per poterla visualizzare
197300960605    3C     ANMLNA        IFGT      *ZEROS
197400090311     C     Kart2         CHAIN     Fiart27L                           31
197500960605    4C     *IN31         IFEQ      *OFF
197600960605     C                   MOVE      D27AAS        V2CAAS
197700960605     C                   MOVE      D27LNP        V2CLNP
197800960605     C                   MOVE      D27NRS        V2CSRB
197900960605     C                   MOVE      D27NSP        V2CNSP
198000960605     C                   MOVE      D27LNA        V2CLAB
198100040531     c* se non impostata zona, la imposto
198200040531     c                   if        anmfl4=000  or anmfl4=999
198300040531     c     kblp27        chain     fnblp01l
198400040531     c                   if        %found(fnblp01l)
198500040531     c                   z-add     blpznc        anmfl4
198600040531     c                   endif
198700040531     c                   endif
198800960605    4C                   END
198900960605    3C                   END
199000960605    2C                   END
199100960605     C*
199200960403     C                   MOVE      ANMFLS        V2CFLS
199300960605    2C     ANMSCN        IFGT      *ZEROS
199400960403     C                   MOVE      ANMLNA        V2CLAS
199500960403     C                   MOVE      ANMNRS        V2CSRS
199600960605   X2C                   ELSE
199700960403     C                   CLEAR                   V2CLAS
199800960403     C                   CLEAR                   V2CSRS
199900960605    2C                   END
200000960403     C                   MOVE      ANMSCN        V2CSCN
200100960403     C                   MOVE      ANMNPS        V2CNPS
200200040531     C* VISLUALIZZO SE COLLO DI VALORE
200300040531     C                   SETOFF                                       93
200400040531     C                   IF        ANMFT4='V'
200500040531     C                   SETON                                        93
200600040531     C                   ENDIF
200700040531     C
200800960403     C* DECODIFICO TIPO ANOMALIA
200900960403     C                   MOVEL     '7F'          KCOD
201000960403     C                   MOVEL(P)  ANMTAN        KKEY
201100960403     C     KTAB          CHAIN     TABEL00F                           30
201200960403     C  N30              MOVEL     TBLUNI        V2DTAN
201300960403     C   30              CLEAR                   V2DTAN
201400960403     C* DECODIFICO SOGGETTO ANOMALIA
201500960403     C                   MOVEL     '7G'          KCOD
201600960403     C                   MOVEL(P)  ANMSAN        KKEY
201700960403     C     KTAB          CHAIN     TABEL00F                           30
201800960403     C  N30              MOVEL     TBLUNI        V2DSAN
201900960403     C   30              CLEAR                   V2DSAN
202000960506     C* DECODIFICO CODICE ANOMALIA
202100960403     C                   MOVE      ANMCAA        V2CCAA
202200960403     C                   MOVEL     '7C'          KCOD
202300960403     C                   MOVEL(P)  ANMCAA        KKEY
202400960403     C     KTAB          CHAIN     TABEL00F                           30
202500960403     C  N30              MOVEL     TBLUNI        DS7C
202600960403     C   30              CLEAR                   DS7C
202700960403     C                   MOVEL     §7CDES        V2DCAA
202800960507     C* INTERNA/I.D.D./NON I.D.D.
202900960507     C                   CLEAR                   W001A
203000960507     C     ANMAIE        IFEQ      'I'
203100961118     C     ANMAIE        OREQ      'X'
203200960507     C                   MOVE      ANMAIE        W001A
203300960507     C                   ELSE
203400960507     C     §7CIDD        IFEQ      'S'
203500960507     C                   MOVE      'D'           W001A             1
203600960507     C                   ELSE
203700960507     C                   MOVE      'N'           W001A
203800960507     C                   END
203900960507     C                   END
204000960507     C     W001A         LOOKUP    TAB           TABDES                   30
204100960507     C                   MOVEL     TABDES        V2DAIE
204200960507     C*
204300960403     C     §7CKEY        COMP      'B'                                080807
204400960403     C*
204500960403     C     §7CIDD        IFEQ      'S'
204600960403     C                   MOVE      ANMLID        V2CLID
204700960403     C                   ELSE
204800960403     C                   CLEAR                   V2CLID
204900960403     C                   END
205000960403     C* APERTURA:
205100960403     C* - decodifico fase
205200960403     C                   MOVEL     '7D'          KCOD
205300960403     C                   MOVEL(P)  ANMFSA        KKEY
205400960403     C     KTAB          CHAIN     TABEL00F                           30
205500960403     C  N30              MOVEL     TBLUNI        DS7D
205600960403     C   30              CLEAR                   DS7D
205700960411     C                   MOVEL     §7DDES        W2DFSA
205800960411     C                   MOVEL     ANMFSA        W2CFSA
205900960411     C                   MOVE      '-'           W2CFSA
206000960403     C*
206100990513     C                   CLEAR                   WLBDA8
206200990513     C                   MOVE      ANMDAO        G08INV
206300990513     C                   MOVEL     '3'           G08ERR
206400990513     C                   CALL      'XSRDA8'
206500990513     C                   PARM                    WLBDA8
206600990513     C                   MOVE      G08DAT        V2CDAO
206700960403     C                   MOVE      ANMCDU        V2CCDU
206800960403     C                   MOVE      ANMNFV        V2CNFV
206900960403     C* CHIUSURA:
207000960403     C* - decodifico fase
207100960405     C                   CLEAR                   V2DFSC
207200960405     C     ANMFSC        IFNE      *BLANKS
207300960403     C                   MOVEL     '7D'          KCOD
207400960403     C                   MOVEL(P)  ANMFSC        KKEY
207500960403     C     KTAB          CHAIN     TABEL00F                           30
207600960403     C  N30              MOVEL     TBLUNI        DS7D
207700960403     C   30              CLEAR                   DS7D
207800960411     C                   MOVEL     §7DDES        W2DFSC
207900960411     C                   MOVEL     ANMFSC        W2CFSC
208000960411     C                   MOVE      '-'           W2CFSC
208100960405     C                   END
208200960403     C*
208300990513     C                   CLEAR                   WLBDA8
208400990513     C                   MOVE      ANMDCH        G08INV
208500990513     C                   MOVEL     '3'           G08ERR
208600990513     C                   CALL      'XSRDA8'
208700990513     C                   PARM                    WLBDA8
208800990513     C                   MOVE      G08DAT        V2CDCH
208900960403     C* - decodifico causale
209000960405     C                   CLEAR                   V2DCCH
209100960405     C     ANMCCH        IFNE      *BLANKS
209200970522     C     ANMCCH        ANDNE     'RA'
209300960405     C                   MOVEL     ANMCCH        W2DCAA
209400960405     C                   MOVE      '-'           W2DCAA
209500960403     C                   MOVEL     '7E'          KCOD
209600960403     C                   MOVEL(P)  ANMCCH        KKEY
209700960403     C     KTAB          CHAIN     TABEL00F                           30
209800960405     C  N30              MOVEL     TBLUNI        W2DDES
209900960405     C                   END
210000040531     C* imposto la zona da anmfl4
210100000516     C                   SETOFF                                       01
210200040531     c                   if        anmfl4<>999
210300040531     c                   move      anmfl4        v2cznc
210400040531     c                   seton                                        01
210500040531     c                   endif
210600040531     c
210700040531     c* Data e ora di creazione anomalia
210800040531     c                   clear                   v2cdac
210900040531     c                   clear                   v2corc
211000040531     c                   if        anmflo<>*blanks
211100040531     c                   movel     anmflo        danmflo
211200040531     c                   if        §anmdac>0
211300040531     C                   CLEAR                   WLBDA8
211400040531     C                   MOVE      §anmdac       G08INV
211500040531     C                   MOVEL     '3'           G08ERR
211600040531     C                   CALL      'XSRDA8'
211700040531     C                   PARM                    WLBDA8
211800040531     C                   MOVE      G08DAT        V2Cdac
211900040531     c                   endif
212000040531     c                   z-add     §anmorc       v2corc
212100040531     c                   endif
212200040531     c
212300040531     c* cerco la zona dalla bolla, sel atrovo
212400040531     C**                 MOVEL     ANMFLO        DANM01
212500040531     C**   §ANZNC        IFNE      *BLANKS
212600040531     C**                 SETON                                        01
212700040531     C**                 MOVEL     §ANZNC        V2CZNC
212800040531     C**                 ENDIF
212900960403     C*
213000960403     C                   MOVE      ANMFLE        V2CFLE
213100960403     C                   MOVE      ANMFL1        V2CFL1
213200960403     C                   MOVE      ANMFL2        V2CFL2
213300960411     C                   CLEAR                   V2DFT1
213400960411     C                   CLEAR                   V2DFT2
213500000904     C* SOLO SE SONO DIVERSI LI DECODIFICO UNO PER UNO
213600000904    2C     ANMFT1        IFNE      ANMFT2
213700000904    3C     ANMFT1        IFEQ      'T'
213800960411     C                   MOVEL     'Trm'         V2DFT1
213900960605     C                   ELSE
214000960605     C     ANMFT1        IFEQ      'R'
214100960605     C                   MOVEL     'Ric'         V2DFT1
214200960411     C                   END
214300000904    3C                   END
214400000904    3C     ANMFT2        IFEQ      'T'
214500960411     C                   MOVEL     'Trm'         V2DFT2
214600960605     C                   ELSE
214700960605     C     ANMFT2        IFEQ      'R'
214800960605     C                   MOVEL     'Ric'         V2DFT2
214900960605     C                   END
215000000904    3C                   END
215100000904    2C                   END
215200000904     C*
215300000904     C                   CLEAR                   V2DDTR
215400000904     C                   CLEAR                   V2CDTR
215500000824     C*
215600000904     C**         ANMDTR    IFNE 0
215700000904     C**         ANMFT1    ANDNE' '
215800000904     C**         ANMDTR    ORNE 0
215900000904     C**         ANMAIE    ANDEQ'T'
216000000831     C*
216100000904     C     ANMFT1        IFEQ      'R'
216200000904     C                   MOVEL     WRIC          V2DDTR
216300000904     C                   ELSE
216400000904     C* O E' GIA' TRASMESSO O SFLAGGATO IN ATTESA DI TRASMISSIONE
216500000904     C                   MOVEL     WTRA          V2DDTR
216600000824     C                   ENDIF
216700000904     C*
216800000904    2C     ANMDTR        IFGT      0
216900000904    3C     ANMFT1        IFNE      ' '
217000000904     C     ANMAIE        OREQ      'T'
217100000824     C                   MOVEL     ANMDTR        WDT04A            4
217200000824     C                   MOVEL     ANMDTR        WDT06X            6
217300000824     C                   MOVE      WDT06X        WDT02M            2
217400000824     C                   MOVE      ANMDTR        WDT02G            2
217500000824     C                   MOVE      WDT10D        V2CDTR
217600000904    3C                   ENDIF
217700000904    2C                   ENDIF
217800000904     C**                   ELSE
217900000904     C**                   CLEARV2DDTR
218000000904     C**                   CLEARV2CDTR
218100000904     C**                   ENDIF
218200960403     C*****************
218300960403     C* EMISSIONE VIDEO
218400960403     C*****************
218500960403     C                   WRITE     LR44D01
218600960403     C                   EXFMT     LR44D02
218700960403     C*
218800960605    1C                   END
218900960402     C                   ENDSR
219000960328     C***********  AGGIORNAMENTO FILE ANOMALIE  ***********************
219100960328     C     AGGANM        BEGSR
219200960329     C                   READC     LR44S01                                29
219300960405    1C     *IN29         DOWEQ     *OFF
219400960419     C                   SETOFF                                       47
219500960412    2C     V1SVAR        IFNE      *BLANKS
219600960412     C     V1HVAR        ORNE      *BLANKS
219700990512     C     V1HNRR        CHAIN     FNANM00F                           30
219800960405    3C     *IN30         IFEQ      *OFF
219900041125     C**                 MOVE      *BLANKS       WALC              1
220000960405    4C                   SELECT
220100960419     C* GESTIONE ANNULLAMENTO ANOMALIA
220200960412     C     V1SVAR        WHENEQ    'A'
220300960514     C*
220400041124    5C**   V1HAPA        IFEQ      'P'
220500041124     C**   ANMAIE        ANDNE     'I'
220600041124     C**                 EXSR      SRCAEX
220700041124    5C**                 END
220800960514     C****
220900960514     C* Se chiudo anomalia 105-Manca record bolla partenza chiudo anche
221000960514     C* le anomalie 5 o 10 e viceversa
221100960514     C*
221200041125    5C     ANMCAA        IFEQ      5
221300960514     C     ANMCAA        OREQ      10
221400960514     C     ANMCAA        OREQ      105
221500960514     C                   EXSR      CHI105
221600041125    5C                   END
221700960514     C***
221800960419     C* Solo se riuscita l'allocazione del file di trasmissione:
221900041125    5C**   WALC          IFEQ      *BLANKS
222000041125     C**   WALC          OREQ      'S'
222100960419     C                   CLEAR                   DSLR33
222200960419     C* N.B.:Richiamo pgm chiusura anomalia senza sflaggare ft1/ft2
222300960419     C*      dando per scontato che le anomalie arrivo possono essere
222400960419     C*      annullate solo se non ancora trasmesse. Provvederò invece
222500960419     C*      alla scrittura del file di trasmissione a fronte di
222600960419     C*      annull/ripr di anomalie partenza
222700041125     c* 25/11/04: non inviamo più le anomalie ma subito annullate o ripristi
222800041125     c*           nate tutte . non importa quindi lo stesso sflaggare
222900960419     C                   MOVE      'N'           D33FTR
223000960419     C                   Z-ADD     V1HNRR        D33NRR
223100960419     C                   MOVE      'AN'          D33CCH
223200960419     C                   MOVE      'M'           D33FSC
223300990513     C                   Z-ADD     DATEU8        D33DCH
223400960514     C                   MOVEL     'S'           D33MAC
223500990512     C                   UNLOCK    FNANM00F
223600960419     C                   CALL      'FNLR33R'
223700960419     C                   PARM                    DSLR33
223800990512     C     V1HNRR        CHAIN(N)  FNANM00F                           30
223900960419     C   30              MOVE      'A'           V1SATB
224000960419     C  N30              EXSR      RIECHI
224100960419     C* Scrivo file di trasmissione
224200041125    6C**   WALC          IFEQ      'S'
224300041125     C**                 EXSR      TRMANM
224400041125    6C**                 END
224500041125    5C**                 END
224600041124     c
224700041124     c
224800960419     C* GESTIONE RIPRISTINO ANOMALIA
224900960412     C     V1SVAR        WHENEQ    'R'
225000041125    5C**   V1HAPA        IFEQ      'P'
225100041125     C**   ANMAIE        ANDNE     'I'
225200041125     C**                 EXSR      SRCAEX
225300041125    5C**                 END
225400960513     C* Se ripristino anomalia 105-Manca record bolla partenza
225500960513     C* ripristino anche le anomalie 5 o 10 e viceversa
225600041125     C**   WALC          IFEQ      'S'
225700041125     C**   WALC          OREQ      *BLANKS
225800960513     C     ANMCAA        IFEQ      5
225900960513     C     ANMCAA        OREQ      10
226000960513     C     ANMCAA        OREQ      105
226100960513     C                   EXSR      RIP105
226200960513     C                   END
226300041125     C**                 END
226400960419     C* Solo se riuscita l'allocazione del file di trasmissione:
226500041125    5C**   WALC          IFEQ      *BLANKS
226600041125     C**   WALC          OREQ      'S'
226700960419    6C     V1SATB        IFEQ      'A'
226800960419     C                   MOVE      *BLANKS       V1SATB
226900960329     C                   MOVE      *BLANKS       ANMATB
227000960419     C                   ELSE
227100960426     C* Clearo campi se anomalia non ha raggiunto il livello max
227200960508     C* N.B.:Se v1slid = blanks il codice operativo move sposta 0
227300960508     C*      in w0010 per cui non ci sono problemi
227400960507     C                   MOVE      V1SLID        W0010             1 0
227500960426     C     W0010         IFLT      WLCI
227600960419     C                   CLEAR                   ANMFSC
227700960419     C                   CLEAR                   ANMDCH
227800960419     C                   CLEAR                   ANMCCH
227900960419     C                   CLEAR                   V1DFSC
228000960426     C                   CLEAR                   V1SDCH
228100960419     C                   CLEAR                   V1SCCH
228200960426     C                   ELSE
228300960426     C* Altrimenti imposto chiusura in pratica Idd
228400960426     C                   MOVEL     'PR'          ANMCCH
228500990513     C                   MOVE      DATEU8        ANMDCH
228600960426     C                   MOVEL     'D'           ANMFSC
228700971118     C                   MOVE      DTGIO6        V1SDCH
228800960426     C                   MOVEL     'PR'          V1SCCH
228900960426     C                   MOVEL     '7D'          KCOD
229000960426     C                   MOVEL(P)  'D'           KKEY
229100960426     C     KTAB          CHAIN     TABEL00F                           30
229200960426     C  N30              MOVEL     TBLUNI        DS7D
229300960426     C  N30              MOVEL     §7DDES        V1DFSC
229400960426     C   30              CLEAR                   V1DFSC
229500960426     C                   END
229600960419    6C                   END
229700041125     C**   WALC          IFEQ      'S'
229800041125     C     WFEL1         IFGT      *ZEROS
229900960605     C     WFEL1         ANDNE     SIMFEL
230000960605     C                   MOVEL     'T'           ANMFT1
230100000824     C                   MOVE      DATEU8        ANMDTR
230200960605     C                   END
230300960605     C     WFEL2         IFGT      *ZEROS
230400960605     C     WFEL2         ANDNE     SIMFEL
230500960605     C     WFEL2         ANDNE     WFEL1
230600960605     C                   MOVEL     'T'           ANMFT2
230700000824     C                   MOVE      DATEU8        ANMDTR
230800960605     C                   END
230900041125     C**                 END
231000990512     C                   UPDATE    FNANM000
231100041125    6C**   WALC          IFEQ      'S'
231200041125     C**                 EXSR      TRMANM
231300041125    6C**                 END
231400041125    5C**                 END
231500960419     C* GESTIONE VARIAZIONE ANOMALIA
231600960412     C     V1HVAR        WHENEQ    'V'
231700960329     C                   EXSR      RISANM
231800990512     C                   UPDATE    FNANM000
231900960405    4C                   ENDSL
232000960405    3C                   END
232100960405    2C                   END
232200960402     C                   SETON                                        04
232300960405     C                   SETON                                        05
232400960405     C                   SETOFF                                       09
232500960402     C                   MOVE      *IN04         V1HIN4
232600960402     C                   MOVE      *IN05         V1HIN5
232700960405     C                   MOVE      *IN09         V1HIN9
232800041125     C                   CLEAR                   V1SVAR
232900041125     c
233000960412     C                   CLEAR                   V1HVAR
233100960523     C                   MOVE      V1HI51        *IN51
233200960523     C                   MOVE      V1HI52        *IN52
233300960329     C                   UPDATE    LR44S01
233400960329     C                   READC     LR44S01                                29
233500960405    1C                   ENDDO
233600000224     C*
233700960329     C                   ENDSR
233800960328     C***************  SCRITTURA FILE ANOMALIE   **********************
233900960328     C     WTRANM        BEGSR
234000960329     C                   READC     LR44S01                                29
234100960329     C     *IN29         DOWEQ     *OFF
234200960402     C     V1SCAA        IFNE      *BLANKS
234300960405     C     V1SCAA        ANDNE     *ZEROS
234400960402     C                   MOVE      V1SCAA        WCAA
234500990512     C                   CLEAR                   FNANM000
234600960329     C                   EXSR      RISANM
234700990512     C                   WRITE     FNANM000
234800960329     C                   END
234900960329     C                   READC     LR44S01                                29
235000960329     C                   ENDDO
235100960329     C   10              MOVEL     'S'           D34RIL
235200960328     C                   ENDSR
235300960329     C********  RIEMPIMENTO CAMPI FILE ANOMALIE  **********************
235400960329     C     RISANM        BEGSR
235500960329     C                   MOVE      V1SNSP        ANMNSP
235600960329     C                   MOVE      V1SSCN        ANMSCN
235700960401     C                   MOVE      V1SFLS        ANMFLS
235800960329     C                   MOVE      V1SLNP        ANMLNP
235900990524     C                   MOVE      V1SAAS        ANMAAS
236000000225     C     ANMNSP        IFGT      0
236100990513     C     V1SAAS        IFGT      60
236200990513     C                   MOVEL     19            ANMAAS
236300990513     C                   ELSE
236400990513     C                   MOVEL     20            ANMAAS
236500990513     C                   END
236600990524     C                   END
236700960329     C                   MOVE      V1SLNA        ANMLNA
236800960329     C                   MOVE      V1SNRS        ANMNRS
236900960329     C                   MOVEL     V1HTAN        ANMTAN
237000960329     C                   MOVEL     V1HSAN        ANMSAN
237100960329     C                   MOVE      V1SCAA        ANMCAA
237200960329     C                   MOVEL     V1HFSA        ANMFSA
237300960329     C                   MOVE      V1HDAO        ANMDAO
237400960507     C                   MOVEL     V1HAIE        ANMAIE
237500960329     C                   MOVE      V1SLID        ANMLID
237600960411     C                   MOVE      D34FGS        ANMFLE
237700960404     C     V1SLNP        IFGT      *ZEROS
237800960404     C                   MOVE      V1SLNP        ANMFL1
237900960404     C                   ELSE
238000960404     C                   MOVE      V1SFLS        ANMFL1
238100960404     C                   END
238200960404     C                   CLEAR                   ANMFT1
238300960404     C                   CLEAR                   ANMFT2
238400000824     C                   CLEAR                   ANMDTR
238500960329     C                   ENDSR
238600960513     C***  CHIUSURA ANOMALIA 5 O 10 PER ANOMALIA 105  E VICEVERSA  ****
238700960513     C     CHI105        BEGSR
238800041124     C*
238900041124     c* anm 105 - annullo la 10 o le 05
239000041124     c* anm  05 - annullo la 105 e altre 05
239100041124     c* anm  10 - annullo la 105
239200041124     c
239300041125     C                   eval      wcaaelab=anmcaa
239400041124     c                   eval      giri=1
239500041124     c
239600960513     C                   EXSR      LEGANM
239700041125     C
239800041125    0c                   dow       giri<=2
239900960513     C*
240000041124     C**                 MOVE      *BLANKS       WALC1             1
240100041124    1C     *IN30         doweq     *OFF
240200051006     c* Per le anom 05 escluse se stesso
240300051006     c                   if        wcaa=wcaaelab and anmnr2<>v1hnrr
240400041124     c*
240500041124    2C**   V1HAPA        IFEQ      'P'
240600041124     C**   ANMAIE        ANDNE     'I'
240700041124     C**                 EXSR      SRCAEX
240800041124     C**                 MOVE      WALC          WALC1
240900041124    2C**                 END
241000041124    2C**   WALC1         IFEQ      'S'
241100041124     C**   WALC1         OREQ      *BLANKS
241200041124     C*
241300960513     C                   CLEAR                   DSLR33
241400960513     C                   MOVE      'N'           D33FTR
241500960513     C                   Z-ADD     ANMNR2        D33NRR
241600960513     C                   MOVE      'AN'          D33CCH
241700960513     C                   MOVE      'M'           D33FSC
241800990513     C                   Z-ADD     DATEU8        D33DCH
241900960514     C* N.B.: in questo caso l'impostazione del flag d33mac è di poca
242000960514     C*       importanza visto che le anomalie di questa routine non
242100960514     C*       sono autochiuse. Lo imposto solo per renderlo uguale
242200960514     C*       all'altro richiamo di fnlr33r
242300960514     C                   MOVEL     'S'           D33MAC
242400960513     C                   CALL      'FNLR33R'
242500960513     C                   PARM                    DSLR33
242600960513     C* Scrivo file di trasmissione
242700041124    3C**   WALC1         IFEQ      'S'
242800041124     C**   ANMNR2        CHAIN(N)  FNANM00F                           30
242900041124     C**                 EXSR      TRMANM
243000041124     C**                 CLEAR                   WALC
243100041124    3C**                 END
243200041124    2C**                 END
243300051006     c                   endif
243400041124    c
243500041124     c
243600041124     c* Verifico se ci sono altre anomalie stesso segnacollo stesso
243700041124     c*  cod.anomalia da chiudere (sono poi solo le 005)
243800041124     C     KANM2         reade     FNANM02L                               30
243900041124    1C                   ENDdo
244000041124     c                   eval      giri=giri+1
244100041124     c
244200041124     c* Per anom 10  e 105 esco
244300041124    1c                   if        wcaaelab<>05
244400041124     c                   eval      giri=3
244500041124     c                   else
244600041124     c* Per anomalia 05 leggo altre 05 se presenti, escludo record in
244700041124     c*  annullamento
244800041124     C                   Z-ADD     05            WCAA
244900041124     C     KANM2         CHAIN     FNANM02L                           30
245000041124    2c                   dow       not *in30 and anmnr2=v1hnrr
245100041124     C     KANM2         reade     FNANM02L                               30
245200041124    2c                   enddo
245300041124     c
245400041124    1c                   endif
245500041124    0c                   enddo
245600960513     C                   ENDSR
245700960513     C***  RIPRISTINO ANOMALIA 5 O 15 PER ANOMALIA 105 E VICEVERSA ****
245800960513     C     RIP105        BEGSR
245900041124     c* anm 105 - annullo la 10 o le 05
246000041124     c* anm  05 - annullo la 105 e altre 05
246100041124     c* anm  10 - annullo la 105
246200041124     c
246300041124     c                   eval      wcaaelab=anmcaa
246400041124     c                   eval      giri=1
246500041124     c
246600960513     C                   EXSR      LEGANM
246700041125     c
246800041125    0c                   dow       giri<=2
246900960513     C*
247000041125     C***                MOVE      *BLANKS       WALC1             1
247100041125    1C     *IN30         doweq     *OFF
247200960513     C*
247300041124    2C**   V1HAPA        IFEQ      'P'
247400041124     C**   ANMAIE        ANDNE     'I'
247500041124     C**                 EXSR      SRCAEX
247600041124     C**                 MOVE      WALC          WALC1
247700041124    2C**                 END
247800960513     C*
247900041124    2C**   WALC1         IFEQ      'S'
248000041124     C**   WALC1         OREQ      *BLANKS
248100990512     C     ANMNR2        CHAIN     FNANM00F                           30
248200041125    3C     *IN30         IFEQ      *OFF
248300041125    4C     ANMLID        IFLT      WLCI
248400960513     C                   CLEAR                   ANMFSC
248500960513     C                   CLEAR                   ANMDCH
248600960513     C                   CLEAR                   ANMCCH
248700960513     C                   ELSE
248800960513     C* Altrimenti imposto chiusura in pratica Idd
248900960513     C                   MOVEL     'PR'          ANMCCH
249000990513     C                   MOVE      DATEU8        ANMDCH
249100960513     C                   MOVEL     'D'           ANMFSC
249200041125    4C                   END
249300041125    4C**   WALC1         IFEQ      'S'
249400041125    5C     WFEL1         IFGT      *ZEROS
249500960605     C                   MOVEL     'T'           ANMFT1
249600000824     C                   MOVE      DATEU8        ANMDTR
249700041125    5C                   END
249800041125    5C     WFEL2         IFGT      *ZEROS
249900960605     C     WFEL2         ANDNE     WFEL1
250000960605     C                   MOVEL     'T'           ANMFT2
250100000824     C                   MOVE      DATEU8        ANMDTR
250200041125    5C                   END
250300041125    4C**                 END
250400990512     C                   UPDATE    FNANM000
250500041125    3C                   END
250600041125    2C**                 END
250700041125     c
250800041125     c* Verifico se ci sono altre anomalie stesso segnacollo stesso
250900041125     c*  cod.anomalia da chiudere (sono poi solo le 005)
251000041125     C     KANM2         reade     FNANM02L                               30
251100041125    1C                   ENDdo
251200041125     c                   eval      giri=giri+1
251300041125     c
251400041125     c* Per anom 10  e 105 esco
251500041125    1c                   if        wcaaelab<>05
251600041125     c                   eval      giri=3
251700041125     c                   else
251800041125     c* Per anomalia 05 leggo altre 05 se presenti, escludo record in
251900041125     c*  annullamento
252000041125     C                   Z-ADD     05            WCAA
252100041125     C     KANM2         CHAIN     FNANM02L                           30
252200041125    2c                   dow       not *in30 and anmnr2=v1hnrr
252300041125     C     KANM2         reade     FNANM02L                               30
252400041125    2c                   enddo
252500041125     c
252600041125    1c                   endif
252700041125    0c                   enddo
252800960513     C*
252900041125     C**   WALC1         IFEQ      'S'
253000041125     C**                 EXSR      TRMANM
253100041125     C**                 CLEAR                   WALC
253200041125     C**                 END
253300960513     C* RICHAINO RECORD SCELTO A VIDEO
253400041125     C**   WALC1         IFNE      'N'
253500990512     C     V1HNRR        CHAIN     FNANM00F                           30
253600041125     C**                 END
253700960513     C*
253800041125    1C**                 END
253900960513     C                   ENDSR
254000960513     C**************  LETTURA ANOMALIA 5/10/105   *********************
254100960513     C     LEGANM        BEGSR
254200960513    1C                   SELECT
254300960513     C     ANMCAA        WHENEQ    105
254400960514     C                   Z-ADD     10            WCAA
254500990512     C     KANM2         CHAIN     FNANM02L                           30
254600960514     C   30              Z-ADD     5             WCAA
254700990512     C   30KANM2         CHAIN     FNANM02L                           30
254800960513     C     ANMCAA        WHENEQ    5
254900960513     C     ANMCAA        OREQ      10
255000960513     C                   Z-ADD     105           WCAA
255100990512     C     KANM2         CHAIN     FNANM02L                           30
255200960513    1C                   ENDSL
255300960513     C                   ENDSR
255400970214     C**********  GESTIONE VIDEATA MANUTENZIONE ANOMALIA 300  *********
255500970214     C     WDW04         BEGSR
255600970214     C* La causale anomalia la imposto sempre = a 300
255700970214     C                   Z-ADD     300           W4SCAA
255800970214     C                   MOVEL     '7C'          KCOD
255900970214     C                   MOVEL(P)  300           KKEY
256000970214     C     KTAB          CHAIN     TABEL00F                           30
256100970214     C  N30              MOVEL     TBLUNI        DS7C
256200970214     C   30              CLEAR                   DS7C
256300970214     C                   MOVEL     §7CDES        W4DCAA
256400970214     C* Spengo indicatori del video
256500970214     C                   SETOFF                                       282555
256600970217     C                   SETOFF                                       5654
256700970214     C* 10 OFF --> MANUTENZIONE
256800970214     C     *IN10         IFEQ      *OFF
256900970214     C* Imposto la lnp
257000970214     C                   MOVE      ANMLNP        W4SLNP
257100970214     C* Imposto il numero dei ritiri
257200970214     C                   MOVE      ANMSCN        W4SNRT
257300970214     C* imposto la data anomalia
257400990513     C                   CLEAR                   WLBDA8
257500990513     C                   MOVE      ANMDAO        G08INV
257600990513     C                   MOVEL     '3'           G08ERR
257700990513     C                   CALL      'XSRDA8'
257800990513     C                   PARM                    WLBDA8
257900990513     C                   MOVE      G08DAT        W4SDAO
258000970214     C                   ELSE
258100970214     C* 10 ON --> INSERIMENTO
258200970214     C                   MOVE      D34FGS        W4SLNP
258300990513     C                   Z-ADD     WDTGIO        W4SDAO
258400970214     C                   MOVE      *ZEROS        W4SNRT
258500970214     C                   END
258600970214     C**
258700970214     C* EMISSIONE WINDOW
258800970214     C**
258900970214     C     EMWDW4        TAG
259000970214     C                   EXFMT     LR44W04
259100970214     C                   SETOFF                                       28
259200970214     C     *INKL         CABEQ     *ON           EWDW04
259300970214     C                   EXSR      CTRWDW
259400970214     C   28
259500970214     CORNKF              GOTO      EMWDW4
259600970214     C* CONFERMO DATI
259700970214     C     *IN10         IFEQ      *OFF
259800970214     C                   EXSR      AGG300
259900970214     C                   ELSE
260000970214     C                   EXSR      WTR300
260100970214     C                   END
260200970217     C*
260300970217     C     EWDW04        TAG
260400970217     C                   MOVE      *BLANKS       V1SVAR
260500970214     C*
260600970217     C                   ENDSR
260700970214     C**********  CONTROLLI WINDOW MANUTENZIONE ANOMALIA 300  *********
260800970214     C     CTRWDW        BEGSR
260900970214     C                   SETOFF                                       545556
261000970214     C*
261100970214    1C     *IN10         IFEQ      *ON
261200970214     C* LNP NON IMMESSA
261300970214    2C     W4SLNP        IFEQ      *ZEROS
261400970214     C                   SETON                                        5428
261500970214     C                   MOVEL     MSG(3)        W4ZMSG
261600970214     C                   GOTO      ENDCTW
261700970214    2C                   END
261800970214     C* LA LNP DEVE ESSERE UNA FILIALE IN GESTIONE
261900970214    2C     W4SLNP        IFNE      D34FGS
262000970214     C     W4SLNP        LOOKUP    L6S                                    30
262100970214    3C     *IN30         IFEQ      *OFF
262200970214     C                   MOVEL     MSG(8)        W4ZMSG
262300970214     C                   SETON                                        5428
262400970214     C                   GOTO      ENDCTW
262500970214    3C                   END
262600970214    2C                   END
262700970214    1C                   END
262800970214     C*
262900970214     C* ERRORE SE NON IMMESSO IL NUMERO RITIRI EFFETTUATI
263000970304    1C*          W4SNRT    IFEQ *ZEROS
263100970304     C*                    MOVELMSG,38    W4ZMSG
263200970304     C*                    SETON                     5528
263300970304     C*                    GOTO ENDCTW
263400970304    1C*                    END
263500970214     C*
263600970214    1C     *IN10         IFEQ      *ON
263700970214     C* ERRORE SE DATA ANOMALIA = 0
263800970214    2C     W4SDAO        IFEQ      *ZEROS
263900970214     C                   MOVEL     MSG(9)        W4ZMSG
264000970214     C                   SETON                                        5628
264100970214     C                   GOTO      ENDCTW
264200970214    2C                   END
264300970214     C* CONTROLLO FORMALE DATA ANOMALIA
264400990513     C                   CLEAR                   WLBDA8
264500990513     C                   Z-ADD     W4SDAO        G08DAT
264600990513     C                   CALL      'XSRDA8'
264700990513     C                   PARM                    WLBDA8
264800990513    2C     G08ERR        IFEQ      '1'
264900970214     C                   MOVE      *ZEROS        W4SINV
265000970214     C                   MOVEL     MSG(9)        W4ZMSG
265100970214     C                   SETON                                        5628
265200970214     C                   GOTO      ENDCTW
265300970214     C                   ELSE
265400990513     C                   MOVE      G08INV        W4SINV
265500990513     C                   MOVE      G08DAT        W4SDAO
265600970214    2C                   END
265700970214     C* ERRORE SE CHIAVE GIA' ESISTENTE
265800990513     C     KANM4         SETLL     FNANM04L
265900990512     C     KANM4         READE     FNANM04L                               30
266000970214     C     *IN30         DOWEQ     *OFF
266100970214     C     ANMLNP        IFEQ      W4SLNP
266200970214     C                   MOVEL     MSG(32)       W4ZMSG
266300970214     C                   SETON                                        2854
266400970214     C                   GOTO      ENDCTW
266500970214     C                   END
266600990512     C     KANM4         READE     FNANM04L                               30
266700970214     C                   ENDDO
266800970214    1C                   END
266900970214     C     ENDCTW        ENDSR
267000970214     C**************  AGGIORNAMENTO ANOMALIA 300 **********************
267100970214     C     AGG300        BEGSR
267200990512     C     V1HNRR        CHAIN     FNANM00F                           30
267300970214     C     *IN30         IFEQ      *OFF
267400970214     C                   Z-ADD     W4SNRT        ANMSCN
267500990512     C                   UPDATE    FNANM000
267600970214     C                   Z-ADD     W4SNRT        V1SSCN
267700970214     C                   END
267800970214     C                   ENDSR
267900970214     C**************  SCRITTURA ANOMALIA 300     **********************
268000970214     C     WTR300        BEGSR
268100990512     C                   CLEAR                   FNANM000
268200970214     C                   Z-ADD     W4SLNP        ANMLNP
268300970214     C                   Z-ADD     W4SNRT        ANMSCN
268400970214     C                   Z-ADD     W4SINV        ANMDAO
268500970214     C                   Z-ADD     W4SINV        ANMDCH
268600970214     C                   Z-ADD     300           ANMCAA
268700970214     C                   MOVE      D34FGS        ANMFLE
268800970214     C                   MOVE      §7CTAN        ANMTAN
268900970214     C                   MOVE      §7CSAN        ANMSAN
269000970214     C                   MOVE      §7CAIE        ANMAIE
269100970214     C                   MOVEL     '1'           ANMFSA
269200970214     C                   MOVEL     '1'           ANMFSC
269300990512     C                   WRITE     FNANM000
269400970214     C                   ENDSR
269500960326     C**************  OPERAZIONI INIZIALI  ****************************
269600960326     C     *INZSR        BEGSR
269700960326     C*
269800960326     C                   Z-ADD     1             CODUT
269900960326     C                   CALL      'X§PARUT'
270000960326     C                   PARM                    UT§DSE
270100960326     C                   MOVEL     RAGUT         V1DRSU
270200960329     C                   MOVEL     REC80         CNCR80
270300960327     C*
270400960327     C     *LIKE         DEFINE    NRR1          NRRAL
270500960327     C     *LIKE         DEFINE    NRR1          NRRSAV
270600960327     C     *LIKE         DEFINE    V1CNBR        W1CNBR
270700960328     C     *LIKE         DEFINE    TBLCOD        KCOD
270800960328     C     *LIKE         DEFINE    TBLKEY        KKEY
270900960329     C     *LIKE         DEFINE    D34FGS        SAVFGS
271000960329     C     *LIKE         DEFINE    V1SCAA        DUPCAA
271100960329     C     *LIKE         DEFINE    V1SAAS        DUPAAS
271200960401     C     *LIKE         DEFINE    V1SLNP        DUPLNP
271300960401     C     *LIKE         DEFINE    V1SNRS        DUPNRS
271400960401     C     *LIKE         DEFINE    V1SNSP        DUPNSP
271500960401     C     *LIKE         DEFINE    V1SFLS        DUPFLS
271600960401     C     *LIKE         DEFINE    V1SSCN        DUPSCN
271700960401     C     *LIKE         DEFINE    V1SDAO        DUPDAO
271800960401     C     *LIKE         DEFINE    V1SLNA        DUPLNA
271900971118     C     *LIKE         DEFINE    G08INV        DATEU8
272000960401     C     *LIKE         DEFINE    ANMCAA        WCAA
272100960506     C     *LIKE         DEFINE    ANMFLE        WFLE
272200960402     C     *LIKE         DEFINE    PA1AAS        WAAS
272300960403     C     *LIKE         DEFINE    §7ALCI        WLCI
272400971118     C     *LIKE         DEFINE    D55TFP        WFEL1
272500971118     C     *LIKE         DEFINE    D55TFP        WFEL2
272600041125     C***  *LIKE         DEFINE    D55TFP        SAVOVR
272700960521     C     *LIKE         DEFINE    D44FIL        W44FIL
272800960521     C     *LIKE         DEFINE    D44LNA        W44LNA
272900960521     C     *LIKE         DEFINE    D44NRS        W44NRS
273000960521     C     *LIKE         DEFINE    D44SCN        W44SCN
273100960605     C     *LIKE         DEFINE    ANMLNP        WLNP
273200960605     C     *LIKE         DEFINE    ANMNRS        WNRS
273300960605     C     *LIKE         DEFINE    ANMNSP        WNSP
273400960830     C     *LIKE         DEFINE    ANMFLS        SAVFLS
273500960830     C     *LIKE         DEFINE    ANMSCN        SAVSCN
273600961210     C     *LIKE         DEFINE    DSANM         DSANMX
273700990513     C     *LIKE         DEFINE    W4SDAO        W4SINV
273800970507     C     *LIKE         DEFINE    AR4TRC        WTRC
273900970507     C                   MOVEL     'A'           WTRC
274000960328     C*
274100040531     C     KTAB          klist
274200960328     C                   KFLD                    CODUT
274300960328     C                   KFLD                    KCOD
274400960328     C                   KFLD                    KKEY
274500960429     C*
274600960429     C     KBLP          KLIST
274700960429     C                   KFLD                    WAAS
274800960429     C                   KFLD                    V1SLNP
274900960429     C                   KFLD                    V1SNRS
275000960429     C                   KFLD                    V1SNSP
275100970507     C     KAR4          KLIST
275200970507     C                   KFLD                    WAAS
275300970507     C                   KFLD                    V1SLNP
275400970507     C                   KFLD                    V1SNRS
275500970507     C                   KFLD                    V1SNSP
275600970507     C                   KFLD                    WTRC
275700960605     C*
275800090311     C     Kbolla        KLIST
275900960605     C                   KFLD                    WAAS
276000960605     C                   KFLD                    WLNP
276100960605     C                   KFLD                    WNRS
276200960605     C                   KFLD                    WNSP
276300960429     C*
276400090311     C     Kart          KLIST
276500960429     C                   KFLD                    V1SFLS
276600960429     C                   KFLD                    V1SLNA
276700960429     C                   KFLD                    V1SNRS
276800960429     C                   KFLD                    V1SSCN
276900960605     C*
277000090311     C     Kart2         KLIST
277100960605     C                   KFLD                    ANMFLS
277200960605     C                   KFLD                    ANMLNA
277300960605     C                   KFLD                    ANMNRS
277400960605     C                   KFLD                    ANMSCN
277500960401     C*
277600960401     C     KANM1         KLIST
277700990513     C                   KFLD                    WAAS
277800960401     C                   KFLD                    V1SLNP
277900960401     C                   KFLD                    V1SNRS
278000960401     C                   KFLD                    V1SNSP
278100960401     C                   KFLD                    WCAA
278200960401     C*
278300960401     C     KANM2         KLIST
278400960401     C                   KFLD                    V1SFLS
278500960401     C                   KFLD                    V1SLNA
278600960401     C                   KFLD                    V1SNRS
278700960401     C                   KFLD                    V1SSCN
278800960401     C                   KFLD                    WCAA
278900970214     C*
279000970214     C     KANM4         KLIST
279100970214     C                   KFLD                    W4SCAA
279200970214     C                   KFLD                    W4SINV
279300960521     C*
279400960521     C     KANM01        KLIST
279500990513     C                   KFLD                    D44AAS
279600960521     C                   KFLD                    D44FIL
279700960521     C                   KFLD                    D44NRS
279800960521     C                   KFLD                    D44NSP
279900040531     C     Kblp27        KLIST
280000040531     C                   KFLD                    D27AAS
280100040531     C                   KFLD                    D27lnp
280200040531     C                   KFLD                    D27NRS
280300040531     C                   KFLD                    D27NSP
280400960522     C*
280500960522     C     KART01        KLIST
280600960522     C                   KFLD                    D44AAS
280700960522     C                   KFLD                    D44FIL
280800960522     C                   KFLD                    D44NRS
280900960522     C                   KFLD                    D44NSP
281000960521     C*
281100960521     C     KANM02        KLIST
281200960521     C                   KFLD                    W44FIL
281300960521     C                   KFLD                    W44LNA
281400960521     C                   KFLD                    W44NRS
281500960521     C                   KFLD                    W44SCN
281600960326     C*
281700971118     C* DETERMINO DATA DEL GIORNO
281800971118     C*
281900971118     C                   TIME                    W0140            14 0
282000971118     C                   MOVE      W0140         WDTGIO            8 0
282100971118     C                   CLEAR                   WLBDA8
282200971118     C                   Z-ADD     WDTGIO        G08DAT
282300971118     C                   CALL      'XSRDA8'
282400971118     C                   PARM                    WLBDA8
282500971118     C                   MOVE      G08INV        DATEU8
282600971118     C                   MOVEL     WDTGIO        DTGIO6            6 0
282700971118     C                   MOVE      WDTGIO        W0020             2 0
282800971118     C                   MOVE      W0020         DTGIO6
282900960401     C*
283000960329     C                   CLEAR                   SAVFGS
283100960327     C                   EXSR      PULSFL
283200960403     C*  Reperisco livello di chiusura anomalia come IDD
283300960403     C                   MOVEL     '7A'          KCOD
283400960403     C                   MOVEL(P)  '2'           KKEY
283500960403     C     KTAB          CHAIN     TABEL00F                           30
283600960403     C  N30              MOVEL     TBLUNI        DS7A2
283700960403     C   30              CLEAR                   DS7A2
283800960403     C                   MOVE      §7ALCI        WLCI
283900960423     C*
284000021008     C                   MOVE      ')'           VAR               8
284100021008     C                   MOVE      '))'          VAR2              9
284200960521     C*
284300960521     C                   SETOFF                                       9192
284400960523     C                   SETOFF                                       5152
284800000824     C                   MOVE      '/'           WTRAT1
284900000824     C                   MOVE      '/'           WTRAT2
285000960327     C*
285100990513     C                   ENDSR
285200990512** MSG
285300960326Fine scorrimento                                                              01
285400960506Immettere Codice Anomalia valido                                              02
285500960401Immettere Linea di Partenza valida                                            03
285600970507Immettere linea  Segnacollo valida                                            04
285700960506Non è ammesso l'inserimento di anomalie con questo codice                     05
285800960401Immettere Numero Spedizione                                                   06
285900960401Immettere Linea di Arrivo valida                                              07
286000970507Immettere un punto operativo in gestione                                      08
286100960401Immettere Data Apertura anomalia valida                                       09
286200000104                                                                              10
286300960401Non ammessa la manutenzione o anomalia già trasmessa                          11
286400960329Anomalia non annullabile                                                      12
286500960329Anomalia già annullata                                                        13
286600960329Anomalia non ripristinabile                                                   14
286700960329Anomalia già ripristinata                                                     15
286800960329Anomalia non manutenzionabile                                                 16
286900960401Immettere Numero Segnacollo                                                   17
287000960401Presente anomalia ancora aperta                                               18
287100970507Se immessa la linea segnacollo immettere anche il numero                      19
287200960401Anomalia già aperta con data > alla data apertura immessa                     20
287300970507Se immesso il numero segnacollo immettere anche la linea                      21
287400960403Anomalia già chiusa in fase IDD                                               22
287500960402Data anomalia > di data odierna. F6 per forzare.                              23
287600000225Se immesso il numero spedizione immettere anche la linea di partenza          24
287700960402Se immesso l'anno e la linea di partenza immettere anche il n.spedizione      25
287800960405Non è possibile inserire più di una volta la stessa anomalia                  26
287900960409Non è possibile,per la stessa bolla,inserire sia anomalia 070 che 071.        27
288000960506Per questa bolla esiste già anomalia XXX-123456789012345678901234 F6 per forz.28
288100960506Per questa bolla esiste già anomalia XXX-123456789012345678901234             29
288200960412Questa anomalia non verrà variata in quanto trasmessa/chiusa.F6 per continuare30
288300960412Anomalia non più esistente                                                    31
288400960418Anomalia già esistente                                                        32
288500960419Alcune anomalie non aggiornate: attendere 1 minuto e riprovare                33
288600960429Impossibile annullare: presente spedizione                                    34
288700960506Per la bolla esiste già anomalia XXX-1234567890123 immessa da xxx.F6 per forz.35
288800970507Immettere linea di arrivo diversa da punto operativo in gestione              36
288900960830Per questa bolla e causale anomalia immettere Fil./N.Segnacollo = xxx/xxxxxxx 37
289000970214Immettere il numero dei ritiri effettuati                                     38
289100970507La spedizione non esiste nell'archivio bolle arrivi: F6 per forzare           39
289200970507Impossibile inserire un MANCA DDT per una spedizione che e' NO DDT
289300960328** Z01
289400960422F6=CONFERMA F10=Inserimento F24=AlTasti
289500960403F7=Bolla F14=Spunte F3=Fine F24=AlTasti
289600960422F12=Ritorno F24=AlTasti
289700960403** Z02
289800960403F7=Bolla F14=Spunte F3=Fine F12=Ritorno
289900960409F3=Fine  F10=Inserimento  F12=Ritorno
290000960403F6=CONFERMA F3=Fine F12=Ritorno
290100960521F3=Fine  F12=Ritorno
290200960521F14=Spunte F3=Fine F12=Ritorno
290300960521F7=Bolla  F3=Fine F12=Ritorno
290400960422** Z03
290500960422F6=CONFERMA F7=Bolla F3=Fine F24=AlTasti
290600960422F14=Spunte F12=Ritorno F24=AlTasti
290700960403** TAB/TABDES
290800960403IINTERNA
290900960507DI.D.D.
291000960507NNON IDD
291100961118XDISCOR.EXP
