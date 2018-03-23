000100960329     H DECEDIT('0,') DATEDIT(*DMY.)
000200960409     H* FNLR44R *----------------------------------------------------*
000300960404     H*         - GESTIONE ANOMALIE
000400960404     H*--------------------------------------------------------------*
000500960328     FTABEL00F  IF   E           K DISK
000600960329     FAZORG01L  IF   E           K DISK
000700960429     FFNBLP01L  IF   E           K DISK
000800970507     FFNARB01L  IF   E           K DISK
000900060216     FFIAR401L  IF   E           K DISK
001000960429     FFNBLT27L  IF   E           K DISK
001100040615     F*FNBTT27L  IF   E           K DISK
001200990512     FFNANM00F  UF A E             DISK
001300990512     FFNANM01L  IF   E           K DISK
001400990512     F                                     RENAME(FNANM000:FNANM001)
001500990512     F                                     INFDS(FNANM1)
001600990512     FFNANM02L  IF   E           K DISK
001700990512     F                                     RENAME(FNANM000:FNANM002)
001800990512     F                                     INFDS(FNANM2)
001900990512     FFNANM04L  IF   E           K DISK
002000990512     F                                     RENAME(FNANM000:FNANM004)
002100041125     F**FNANM00T  O  A E             DISK    USROPN
002200041125     F**                                     RENAME(FNANM000:FNANM0TT)
002300960605     FFNART01L  IF   E           K DISK    USROPN
002400960605     FFNART27L  IF   E           K DISK
002500960605     F                                     RENAME(FNART000:FNAR27)
002600960605     FFNBLT01L  IF   E           K DISK    USROPN
002700960521     F                                     RENAME(FNBLT000:FNBLT1)
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
007400101203     d* bolla
007500101203     d  pa4bolp               42     44  0
007600101203     d  pa4bors               45     46  0
007700101203     d  pa4bosp               47     53  0
007800101203     d  pa4boan               54     57  0
007900101203     D  Pa4ERR                58     58
008000101203     d
008100960402     D DSLR36          DS
008200960402     D  PA1AAS                14     17  0
008300960402     D  PA1LNP                18     20  0
008400960402     D  PA1NRS                21     22  0
008500960402     D  PA1NSP                23     29  0
008600960402     D  PA1F03                30     30
008700960402     D  PA1FLG                31     31
008800960402     D  PA1PGM                32     41
008900960402     D  PA1DSA                42     61
009000960402     D  PA1CML                62     62
009100960402     D  PA1DSP                63     70  0
009200960402     D  PA1RMN                71     85  0
009300960402     D  PA1LNA                86     88  0
009400960402     D  PA1LN1                89    108
009500960402     D  PA1RSD               109    143
009600960402     D* SE RICH DA PGM GIAC. = 1
009700960402     D  PA1GIA               144    144
009800960402     D  PA1FEL               145    147  0
009900960402     D  PA1TBO               148    148
010000960326     D KPJBA         E DS
010100960326     D DS7C          E DS
010200960403     D DS7A2         E DS
010300960326     D DS7D          E DS
010400960329     D CNCR80        E DS
010500960326     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
010600990512     D DSANM         E DS                  EXTNAME(FNANM00F)
010700960419     D DSLR33        E DS                  EXTNAME(FNLR33DS)
010800960327     D DSLR34        E DS                  EXTNAME(FNLR34DS)
010900040923     D  ANM                   89    118
011000040923     D                                     DIM(10)                              ANOM.DA FILTRO
011100111115     D parmlr34      E DS                  EXTNAME(FNLR34DS) prefix(P_)
011200040531     D DANMFLO       E DS
011300960329     D DSUL06        E DS                  EXTNAME(TRUL06DS)
011400960329     D  LIN                    1     90  0
011500960329     D                                     DIM(30)                              FIL. COMODO
011600971118     D DSLV55        E DS                  EXTNAME(FNLV55DS)
011700031028     d dsbl4a        e ds
011800990512     D FNANM1          DS
011900960401     D  ANMNR1               397    400B 0
012000990512     D FNANM2          DS
012100960401     D  ANMNR2               397    400B 0
012200971118     D WLBDA8          DS
012300971118     D  G08DAT                 1      8  0
012400971118     D  G08INV                 9     16  0
012500971118     D  G08ERR                17     17
012600971118     D  G08TGI                18     22  0
012700000824     D WDT10D          DS
012800000824     D  WDT02G                 1      2
012900000824     D  WTRAT1                 3      3
013000000824     D  WDT02M                 4      5
013100000824     D  WTRAT2                 6      6
013200000824     D  WDT04A                 7     10
013300960329     D*
013400960403     D* DS PER CONTROLLI IN MODO INSERIMENTO
013500960329     D                 DS
013600960329     D  V1SAAS                 1      2  0
013700960329     D  V1SLNP                 3      5  0
013800960329     D  V1SNSP                 6     12  0
013900960402     D  KEYNSP                 1     12  0
014000960403     D  V1SSCN                13     19  0
014100960403     D  V1SFLS                20     22  0
014200960403     D  V1SNRS                23     24  0
014300960403     D  IOFLD                  1     24
014400960405     D                 DS
014500960405     D  WSAAS                  1      2  0
014600960405     D  WSLNP                  3      5  0
014700960405     D  WSNRB                  6      7  0
014800960405     D  WSNSP                  8     14  0
014900960405     D  WSCAB                 15     17
015000960405     D  WKEYB                  1     17
015100960405     D                 DS
015200960405     D  WSFLS                  1      3  0
015300960405     D  WSLNA                  4      6  0
015400960405     D  WSNRS                  7      8  0
015500960405     D  WSSCN                  9     15  0
015600960405     D  WSCAS                 16     18
015700960405     D  WKEYS                  1     18
015800960405     D                 DS
015900990513     D  V2DCCH                 1     23
016000960405     D  W2DCAA                 1      3
016100990513     D  W2DDES                 4     23
016200960411     D                 DS
016300960411     D  V2DFSA                 1     17
016400960411     D  W2CFSA                 1      2
016500960411     D  W2DFSA                 3     17
016600960411     D                 DS
016700960411     D  V2DFSC                 1     17
016800960411     D  W2CFSC                 1      2
016900960411     D  W2DFSC                 3     17
017000960326     D                SDS
017100960326     D  V1DPGM                 1      8
017200041125     D GIRI            s              1  0
017300041125     D WCAAELAB        s                   LIKE(ANMCAA)
017400960521     D TIT1            C                   CONST('***   GESTIONE ANOMA-
017500960521     D                                     LIE   ***')
017600960521     D TIT2            C                   CONST('** INTERROGAZIONE AN-
017700960521     D                                     OMALIE **')
017800040603     D WTRA            C                   CONST('TRASMISSIONE .....:')
017900040603     D WRIC            C                   CONST('RICEZIONE ........:')
018000960327     I*
018100960521     IFNART000
018200960530     I              ARTFLS                      DETFLS
018300960521     I              ARTLNA                      DETLNA
018400960521     I              ARTNRS                      DETNRS
018500960521     I              ARTNSC                      DETNSC
018600960521     IFNBLT1
018700960530     I              BLTFLS                      DETFLS
018800960521     I              BLTLNA                      DETLNA
018900960521     I              BLTNRS                      DETNRS
019000960521     I              BLTNSC                      DETNSC
019100960605     IFNAR27
019200960605     I              ARTFLS                      D27FLS
019300960605     I              ARTLNA                      D27LNA
019400960605     I              ARTNRS                      D27NRS
019500960605     I              ARTNSC                      D27NSC
019600960605     I              ARTAAS                      D27AAS
019700960605     I              ARTLNP                      D27LNP
019800960605     I              ARTNSP                      D27NSP
019900960605     IFNBLT000
020000960605     I              BLTFLS                      D27FLS
020100960605     I              BLTLNA                      D27LNA
020200960605     I              BLTNRS                      D27NRS
020300960605     I              BLTNSC                      D27NSC
020400960605     I              BLTAAS                      D27AAS
020500960605     I              BLTLNP                      D27LNP
020600960605     I              BLTNSP                      D27NSP
020700960326     C****************************************************************
020800960326     C*  RIEPILOGO INDICATORI
020900960326     C***************************************************************
021000960422     C* 02    -  ON --> MANUTENZIONE NON ABILITATO
021100960422     C* 03    -  ON --> INSERIMENTO NON ABILITATO
021200960402     C* 04    -  PROTEZIONE CAMPI SUBFILE
021300960506     C* 05    -  PROTEZIONE CODICE ANOMALIA
021400960402     C* 06    -  OFF --> ND DESCRIZIONE "ANNULLATA" SU VIDEATA D02
021500960403     C* 07    -  'HI' DI CHIAVE SPEDIZIONE SU VIDEATA D02
021600960403     C* 08    -  'HI' DI CHIAVE SEGNACOLLO SU VIDEATA D02
021700960405     C* 09    -  'HI' CAMPI DELLA CHIAVE IN SUBFILE SE CAMPI SPROTETTI
021800960412     C* 10    -  ON - MODALITA' INSERIMENTO
021900960401     C* 11/19 -  DUP SU CAMPI DI I/O
022000960326     C* 28    -  ERRORE GENERICO VIDEATE
022100960326     C* 30    -  COMODO
022200960402     C* 31    -  COMODO
022300961210     C* 35    -  LETTURA ANOMALIE IN MODALITA' INTERROGAZIONE
022400961210     C* 36    -  LETTURA BOLLA IN MODALITA' INTERROGAZIONE
022500960506     C* 40    -  ERRORE CODICE ANOMALIA
022600960401     C* 41    -  ERRORE NUMERO SPEDIZIONE
022700960521     C* 42    -  ERRORE NUMERO SEGNACOLLO
022800960401     C* 43    -  ERRORE LINEA DI PARTENZA
022900960401     C* 44    -  ERRORE ANNO SPEDIZIONE
023000960401     C* 45    -  ERRORE LINEA DI ARRIVO
023100960401     C* 46    -  ERRORE DATA APERTURA ANOMALIA
023200960401     C* 47    -  ERRORE SU CAMPO OPZIONE
023300960401     C* 48    -  ERRORE SU FILIALE SEGNACOLLO
023400960401     C* 49    -  ERRORE NUMERO SERIE
023500960523     C* 51    -  ND CAMPI KEY SPEDIZIONE/SEGNACOLLO
023600960523     C* 52    -  ND SEGNACOLLO
023700960521     C* 98/99 -  PER I FILE DI TRASMISSIONE
023800971103     C* 90    -  RIEMISSIONE FORMATO VIDEO
023900960521     C* 91    -  INTERROGAZIONE ANOMALIE PER SPEDIZIONE
024000960521     C* 92    -  INTERROGAZIONE ANOMALIE PER SEGNACOLLO
024100960326     C***************************************************************
024200960327     C     *ENTRY        PLIST
024300960327     C                   PARM                    KPJBA
024400111115     C                   PARM                    parmlr34
024500111115     c
024600960522     C                   MOVEL     KPJBU         DSLR44
024700960523     C* SOLO LA PRIMA VOLTA
024800960523     C     WVOLT         IFEQ      ' '
024900960523     C* CARICO £1
025000960523     C                   CLEAR                   DSUL06
025100960523     C                   MOVE      '£1'          D06COD
025200960523     C                   MOVEL     SIMFEL        D06KEY
025300960523     C                   MOVEL     DSUL06        KPJBU
025400960523     C                   CALL      'TRUL06R'
025500960523     C                   PARM                    KPJBA
025600960523     C                   MOVEL     KPJBU         DSUL06
025700960523     C                   MOVEA     LIN           L1
025800960523     C                   MOVE      '1'           WVOLT             1
025900960523     C                   END
026000960327     C*
026100960521     C                   SELECT
026200960521     C* MODALITA' GESTIONE
026300960521     C     D44MOD        WHENEQ    'G'
026400960521     C                   EXSR      SRGEST
026500960524     C* Emissione subfile se ho finito di caricare la pagina
026600960524    2C     D34END        IFEQ      'S'
026700960524     C     NRR1          OREQ      NRRAL
026800960521     C                   EXSR      SRXFMT
026900960521     C                   END
027000960521     C* MODALITA' INTERROGAZIONE
027100960521     C     D44MOD        WHENEQ    'I'
027200960521     C                   EXSR      SRINTE
027300960521     C* Emissione subfile
027400960521     C                   EXSR      SRXFMT
027500960521     C                   ENDSL
027600960327     C*
027700960326     C     FINE          TAG
027800030717     C*
027900030717     C* VALORIZZO NUOVI CAMPI DELLA DS VALIDI UNICAMENTE COME SELEZIONE IN FASE DI STAMPA
028000030717     C* (ESEGUO UNA VALORIZZAZIONE NEUTRA OVVERO COME SE NN FOSSERO STATE RICHIESTE SELEZIONI)
028100111116     C****               Z-ADD     *ZEROS        D34POS
028200111116     C*****              Z-ADD     99            D34NRS
028300030717     C*
028400111116     c                   if        %parms=1
028500960327     C                   MOVEL     DSLR34        D44LR
028600111116     c                   else
028700111116     C                   MOVEL     DSLR34        parmlr34
028800111116     c                   endif
028900111116     c
029000960522     C                   MOVEL     DSLR44        KPJBU
029100960521     C*
029200960521     C     D44MOD        IFEQ      'G'
029300960327     C                   SETON                                        RT
029400960521     C                   ELSE
029500971118     C                   MOVEL     'C'           D55TLA
029600971118     C                   CALL      'FNLV55R'
029700971118     C                   PARM                    DSLV55
029800960521     C                   SETON                                        LR
029900960521     C                   END
030000960521     C*
030100960521     C****************   MODALITA' GESTIONE ANOMALIE  *****************
030200960521     C     SRGEST        BEGSR
030300960521     C                   MOVEL     D44ANM        DSANM
030400111115     c
030500111115     c* Se passato un solo parametro
030600111115     c                   if        %parms=1
030700960521     C                   MOVEL     D44LR         DSLR34
030800111115     c                   else
030900111115     c
031000111115     c* se passati due parametri passo la DS a parte
031100111115     c                   movel     parmlr34      dslr34
031200111115     c                   endif
031300111111     c*
031400110909     c*  visibile solo per EDP
031500111111     c***                if        d34end='S'
031600111111     c*****                        %subst(knmus:1:3)='EDP'   or anmcaa<>115
031700110909     c
031800970214     C* VERIFICO SE RICHIESTA ANOMALIA 300-NR.RITIRI EFFETTUATI
031900970214     C     '300'         LOOKUP    ANM                                    31
032000040923     c     *in31         ifeq      *on
032100040923     C                   MOVEL     '7C'          KCOD
032200040923     C                   MOVEL(P)  300           KKEY
032300040923     C     KTAB          CHAIN     TABEL00F                           30
032400040923     C  N30              MOVEL     TBLUNI        DS7C
032500040923     C   30              CLEAR                   DS7C
032600040923     c                   endif
032700040923     C     *IN31         IFEQ      *ON
032800040923     c     §7cges        andeq     'S'
032900040923     C     D34INC        ANDEQ     'I'
033000970214     C                   MOVE      '1'           WA300             1
033100970214     C                   ELSE
033200970214     C                   MOVE      *BLANKS       WA300
033300970214     C                   END
033400960521     C* PULISCO CAMPI DI OUTPUT
033500960521     C                   CLEAR                   D34F03
033600960521     C                   CLEAR                   D34F12
033700960521     C                   CLEAR                   D34RIL
033800960521     C                   SETON                                        20
033900960521     C* CARICO £6 A CAMBIO DI FILIALE GESTIONE
034000960521     C     D34FGS        IFNE      SAVFGS
034100960521     C                   CLEAR                   L6S
034200960521     C                   CLEAR                   DSUL06
034300960521     C                   MOVE      '£6'          D06COD
034400020509     C**!!!              MOVEL     'S'           D06ESC
034500020509     c                   eval      d06esc = 'G'
034600020509     c                   movel     simfel        d06key
034700960521     C                   MOVEL     'L'           D06TLA
034800960521     C                   MOVEL     DSUL06        KPJBU
034900960521     C                   CALL      'TRUL06R'
035000960521     C                   PARM                    KPJBA
035100960521     C                   MOVEL     KPJBU         DSUL06
035200960521     C                   MOVEA     LIN           L6S
035300961210     C                   MOVE      D34FGS        SAVFGS
035400960521     C                   END
035500960521     C*
035600960521     C                   SETOFF                                       0203
035700960521     C* SE RICHIESTE SOLO ANOMALIE IN PARTENZA ACCENDO INDICATORE
035800960521     C* PER DISABILITARE L'INSERIMENTO
035900970214     C* AD ECCEZIONE DELL'ANOMALIA 300-N°RITIRI EFFETTUATI (WA300=' ')
036000960521    1C     D34PAR        IFEQ      'P'
036100970214     C     WA300         ANDEQ     *BLANKS
036200960521     C                   SETON                                        03
036300960521     C                   END
036400960521     C* SE IN INTERROGAZIONE ACCENDO INDICATORI PER DISABILITARE
036500960521     C* MANAUTENZIONE ED INSERIMENTO
036600960521     C     D34GES        IFEQ      'I'
036700960521     C                   SETON                                        0203
036800960521     C                   MOVEL     TIT2          V1DTIT
036900960521     C                   ELSE
037000960521     C                   MOVEL     TIT1          V1DTIT
037100960521    1C                   END
037200960521     C*    FINE FILE: IMPOSTO INDICATORE DI SFLEND
037300960521    1C     D34END        IFEQ      'S'
037400960521     C                   SETON                                        23
037500960521   X1C                   ELSE
037600960521     C* NO FINE FILE: CARICAMENTO SUBFILE
037700960521    2C     NRR1          IFLT      NRRAL
037800960521     C                   SETON                                        0405
037900960521     C                   SETOFF                                       09
038000960521     C                   EXSR      RIS01A
038100960523     C                   MOVE      '0'           V1HI51
038200960523     C                   MOVE      '0'           V1HI52
038300960521     C                   ADD       1             NRR1
038400960521     C                   WRITE     LR44S01
038500960521    2C                   END
038600960521    1C                   END
038700110909     c
038800111111     c****               endif
038900110909     c
039000960521     C                   ENDSR
039100960521     C****************   MODALITA' INTERROGAZIONE ANOMALIE  ***********
039200960521     C     SRINTE        BEGSR
039300960522     C*
039400960521     C                   EXSR      PULSFL
039500970217     C                   SETOFF                                       09
039600960521     C* PROTEGGO CAMPI I/O DEL SUBFILE
039700960521     C                   SETON                                        0405
039800960521     C* DISABILITO TASTI DI CONFERMA/INSER...+ OPZIONI VARIAZ/ANN/RIPR.
039900960521     C                   SETON                                        0203
040000960521     C                   MOVEL     TIT2          V1DTIT
040100960521     C*
040200960521     C* POSIZIONAMENTO INIZIALE SU FILE ANOMALIE PER CARICAMENTO SUBFIL
040300960521     C     D44NSP        IFGT      *ZEROS
040400960521     C* Interrogazione anomalie bolla
040500960521     C                   SETON                                        91
040600990512     C     KANM01        SETLL     FNANM01L
040700990512     C     KANM01        READE     FNANM01L                               35
040800960521     C                   MOVE      '1'           WFILE             1
040900960521     C                   ELSE
041000960521     C* Interrogazione anomalie segnacollo
041100960521     C                   SETON                                        92
041200960521     C                   MOVE      D44FIL        W44FIL
041300960521     C                   MOVE      D44LNA        W44LNA
041400960521     C                   MOVE      D44NRS        W44NRS
041500960521     C                   MOVE      D44SCN        W44SCN
041600990512     C     KANM02        SETLL     FNANM02L
041700990512     C     KANM02        READE     FNANM02L                               35
041800960521     C                   MOVE      '2'           WFILE             1
041900960521     C                   END
042000110909     C
042100960521     C                   EXSR      RIS01I
042200110909     c
042300960524     C* SE NON HO CARICATO NULLA SPENGO INDICATORE SFLDSP
042400960524     C     NRR1          IFEQ      *ZEROS
042500960524     C                   SETOFF                                       20
042600960524     C                   END
042700960521     C                   ENDSR
042800960521     C*********   CARICAMENTO SUBFILE MODALITA INTERROGAZIONE  ********
042900960521     C     RIS01I        BEGSR
043000960524     C                   CLEAR                   WFIRST
043100960524     C*
043200960524     C                   EXSR      RIS1IB
043300960524     C*
043400961210     C* SE NON HO RIEMPITO TUTTA LA PAGINA DI SUBFILE ED E' FINE FILE
043500990512     C* (DI FNANM01L); SE STO INTERROGANDO PER SPEDIZIONE,RICHIAMO ROU-
043600961210     C* TINE DI RICERCA  ANOMALIE PER OGNI SEGNACOLLO DELLA SPEDIZIONE
043700961210    1C     WFILE         IFEQ      '1'
043800960521     C     *IN91         ANDEQ     *ON
043900960521     C     NRR1          ANDLT     NRRAL
044000961210     C     *IN35         ANDEQ     *ON
044100960521     C                   SELECT
044200960521     C     D44TBO        WHENEQ    'A'                                          ARRIVI
044300960605     C     WOPNAR        IFEQ      ' '
044400960605     C                   OPEN      FNART01L
044500960605     C                   MOVE      '1'           WOPNAR            1
044600960605     C                   END
044700960522     C     KART01        SETLL     FNART01L
044800961210     C     KART01        READE     FNART01L                               36
044900960521     C     D44TBO        WHENEQ    'P'                                          PARTENZE
045000040615     c     d44tbo        oreq      'T'
045100960605     C     WOPNBL        IFEQ      ' '
045200960605     C                   OPEN      FNBLT01L
045300960605     C                   MOVE      '1'           WOPNBL            1
045400960605     C                   END
045500960522     C     KART01        SETLL     FNBLT01L
045600961210     C     KART01        READE     FNBLT01L                               36
045700040615     C**   D44TBO        WHENEQ    'T'                                          TRANSITI
045800040615     C**   WOPNBT        IFEQ      ' '
045900040615     C**                 OPEN      FNBTT01L
046000040615     C**                 MOVE      '1'           WOPNBT            1
046100040615     C**                 END
046200040615     C**   KART01        SETLL     FNBTT01L
046300040615     C**   KART01        READE     FNBTT01L                               36
046400960521     C                   ENDSL
046500960523     C                   MOVE      '2'           WFILE
046600961210     C  N36              EXSR      RICSCN
046700961210   X1C                   ELSE
046800050929     c
046900961210     C     WFILE         IFEQ      '2'
047000961210     C     *IN91         ANDEQ     *ON
047100961210     C     NRR1          ANDLT     NRRAL
047200050929     c                   if        finiscelett=' '
047300961210     C                   EXSR      RICSCN
047400961210     C* 23 --> SFLEND
047500961210     C   92
047600961210     CAN 36              SETON                                        23
047700050929     c                   else
047800050929     c                   seton                                        23
047900050929     c                   clear                   finiscelett
048000050929     C                   ENDif
048100050929     C                   END
048200961210    1C                   END
048300960524     C*
048400960524     C* SE HO CARICATO ALMENO UN RECORD DI SFL INCREMENTO SFLRCDNBR
048500960524    1C     NRR1          IFGT      NRRSAV
048600960524     C     NRRSAV        ADD       1             V1CNBR
048700960524    1C                   END
048800960524     C* SE FINE LETTURA MEMORIZZO SFLRCDNBR DELL'ULTIMA PAGINA
048900960524     C   23              Z-ADD     V1CNBR        W1CNBR
049000960524     C* MEMORIZZO NRR
049100960524     C                   Z-ADD     NRR1          NRRSAV
049200960521     C                   ENDSR
049300960523     C*********   CARICAMENTO SUBFILE MODALITA INTERROGAZIONE  ********
049400960523     C     RIS1IB        BEGSR
049500960523     C                   SETOFF                                       22  49
049600960523     C                   SETOFF                                       404142
049700960523     C                   SETOFF                                       434445
049800960523     C                   SETOFF                                       464748
049900960523    1C     NRR1          DOWLT     NRRAL
050000961210     C     *IN35         ANDEQ     *OFF
050100960524     C* ESCLUDO RECORD SE STO LEGGENDO PER SPEDIZIONE E SEGNACOLLO>0
050200960524     C* (LO TRATTERO' NEL GIRO PER SEGNACOLLO)
050300960524     C     WFILE         IFEQ      '1'
050400960524     C     ANMSCN        ANDGT     *ZEROS
050500960524     C                   GOTO      LEGGAN
050600960524     C                   END
050700110909     C
050800111115     c***                IF        %subst(knmus:1:3)<>'EDP'  AND anmcaa= 115
050900111115     C***                GOTO      LEGGAN
051000111115     C***                END
051100110909     c
051200960524     C*
051300960523     C     WFILE         IFEQ      '1'
051400960523     C                   Z-ADD     ANMNR1        D44NRR
051500960523     C                   ELSE
051600960523     C                   Z-ADD     ANMNR2        D44NRR
051700960523     C                   END
051800960523     C                   EXSR      RIS01A
051900960523     C*
052000960523     C* SOLO LA PRIMA VOLTA VISUALIZZO CAMPI DELLA CHIAVE
052100960524     C     WFIRST        IFEQ      *BLANKS
052200960524     C                   MOVE      '1'           WFIRST            1
052300960523     C                   SETOFF                                       5152
052400960523     C                   ELSE
052500960524     C     *IN91         IFEQ      *ON
052600960524     C                   SETOFF                                       52
052700960524     C                   ELSE
052800960524     C                   SETON                                        52
052900960524     C                   END
053000960523     C                   SETON                                        51
053100960523     C                   END
053200960523     C*
053300960524     C                   MOVE      *IN51         V1HI51
053400960524     C                   MOVE      *IN52         V1HI52
053500960523     C                   ADD       1             NRR1
053600960523     C                   WRITE     LR44S01
053700960524     C     LEGGAN        TAG
053800960523     C     WFILE         IFEQ      '1'
053900990512     C     KANM01        READE     FNANM01L                               35
054000960523     C                   ELSE
054100990512     C     KANM02        READE     FNANM02L                               35
054200960523     C                   END
054300960523     C                   ENDDO
054400990512     C* MEMORIZZO CAMPI DI FNANM PER NON SPORCARMELI SE PER CASO
054500961210     C* ENTRERO' IN SRINT
054600961210     C  N35              MOVEL     DSANM         DSANMX
054700960523     C                   ENDSR
054800960521     C****************  RICERCA SEGNACOLLI DELLA SPEDIZIONE  **********
054900960521     C     RICSCN        BEGSR
055000960521    1C     NRR1          DOWLT     NRRAL
055100961210     C     *IN36         ANDEQ     *OFF
055200960530     C                   MOVE      DETFLS        W44FIL
055300960521     C                   MOVE      DETLNA        W44LNA
055400960521     C                   MOVE      DETNRS        W44NRS
055500960521     C                   MOVE      DETNSC        W44SCN
055600990512     C     KANM02        SETLL     FNANM02L
055700990512     C     KANM02        READE     FNANM02L                               35
055800961210     C  N35              EXSR      RIS1IB
055900960521     C*
056000960521     C                   SELECT
056100960521     C     D44TBO        WHENEQ    'A'                                          ARRIVI
056200961210     C     KART01        READE     FNART01L                               36
056300960521     C     D44TBO        WHENEQ    'P'                                          PARTENZE
056400040615     c     d44tbo        oreq      'T'
056500961210     C     KART01        READE     FNBLT01L                               36
056600040615     C***  D44TBO        WHENEQ    'T'                                          TRANSITI
056700040615     C***  KART01        READE     FNBTT01L                               36
056800960521     C                   ENDSL
056900960521     C*
057000960521     C                   ENDDO
057100960521     C*
057200050929     c                   if        *in35
057300961210     C   36              SETON                                        23
057400050929     c   36              clear                   finiscelett       1
057500050929     c                   else
057600050929     c* per 35 off - devop finire di leggere le anomalie dell'ultimo
057700050929     c*  segnacollo
057800050929     c                   if        *in36
057900050929     c                   eval      finiscelett='S'
058000050929     c                   endif
058100050929     c                   endif
058200960521     C                   ENDSR
058300960521     C****************  EMISSIONE/GESTIONE SUBFILE  *******************
058400960521     C     SRXFMT        BEGSR
058500960524     C* SOLO SE MODALITA' GESTIONE:
058600960524    1C     D44MOD        IFEQ      'G'
058700960524     C* Se non ho caricato nulla spengo indicatore sfldsp
058800960524    2C     NRR1          IFEQ      *ZEROS
058900960524     C                   SETOFF                                       20
059000960524     C                   MOVE      'S'           D34F12
059100960524   X2C                   ELSE
059200960524     C* Se ho caricato almeno un record di sfl incremento sflrcdnbr
059300960524     C                   Z-ADD     NRRSAV        V1CNBR
059400960524    3C     NRR1          IFGT      NRRSAV
059500960524     C                   ADD       1             V1CNBR
059600960524    3C                   END
059700960524     C* Memorizzo nrr
059800960524     C                   Z-ADD     NRR1          NRRSAV
059900960524     C* Se fine lettura memorizzo sflrcdnbr dell'ultima pagina
060000960524     C   23              Z-ADD     V1CNBR        W1CNBR
060100960524    2C                   END
060200960521     C* Imposto piedi del video
060300960521     C***
060400960521     C*** Modalità gestione
060500960521     C***
060600960522     C  N03
060700960522     CANN20              MOVEL     Z02(2)        V1ZCMD
060800960522     C   03
060900960522     CANN20              MOVEL     Z02(4)        V1ZCMD
061000960521     C* solo interrogazione
061100960521     C   02
061200960521     CAN 20              MOVEL     Z02(1)        V1ZCMD
061300960521     C* solo manutenzione
061400960521     C   03
061500960521     CANN02
061600960521     CAN 20              MOVEL     Z03(1)        V1ZCMD
061700960521     C   03
061800960521     CANN02
061900960521     CAN 20              Z-ADD     1             WF                1 0
062000960521     C   03
062100960521     CANN02
062200960521     CAN 20              Z-ADD     2             WMAX              1 0
062300960521     C* tutte le funzioni abilitate (manutenzione/inserimento/interrog)
062400960521     C  N03
062500960521     CAN 20              MOVEL     Z01(1)        V1ZCMD
062600960521     C  N03
062700960521     CAN 20              Z-ADD     1             WF                1 0
062800960521     C  N03
062900960521     CAN 20              Z-ADD     3             WMAX              1 0
063000960524   X1C                   ELSE
063100960521     C***
063200960521     C*** Modalità interrogazione
063300960521     C***
063400960521     C  N20              MOVE      Z02(4)        V1ZCMD
063500960521     C   91
063600960521     CAN 20              MOVE      Z02(5)        V1ZCMD
063700960521     C   92
063800960521     CAN 20              MOVE      Z02(6)        V1ZCMD
063900960524    1C                   END
064000960521     C* EMISSIONE SUBFILE
064100960521     C     EMC01         TAG
064200960521     C                   WRITE     LR44Z01
064300960521     C  N20              WRITE     LR44D03
064400960521     C                   WRITE     LR44D01
064500960521     C                   EXFMT     LR44C01
064600971103     C                   SETOFF                                       289040
064700960521     C                   SETOFF                                       414243
064800960521     C                   SETOFF                                       444546
064900960521     C                   SETOFF                                       474849
065000960521     C                   CLEAR                   V1ZMSG
065100960521     C* Tasto F3=Fine
065200960521    2C     *INKC         IFEQ      *ON
065300960521     C                   MOVE      'S'           D34F03
065400960521     C                   EXSR      PULSFL
065500960521     C                   GOTO      FXFMT
065600960521    2C                   END
065700960521     C* Tasto F12=Ritorno
065800960521    2C     *INKL         IFEQ      *ON
065900960521     C   10              MOVEL     'S'           D34RIL
066000960521     C  N10              MOVE      'S'           D34F12
066100960521     C                   EXSR      PULSFL
066200960521     C                   GOTO      FXFMT
066300960521    2C                   END
066400960521     C* Tasto F24=Altri Tasti
066500960521    2C     *INKY         IFEQ      *ON
066600960521     C     WF            IFLT      WMAX
066700960521     C                   ADD       1             WF
066800960521     C  N03              MOVEL     Z01(WF)       V1ZCMD
066900960521     C   03              MOVEL     Z03(WF)       V1ZCMD
067000960521     C                   ELSE
067100960521     C                   Z-ADD     1             WF
067200960521     C  N03              MOVEL     Z01(WF)       V1ZCMD
067300960521     C   03              MOVEL     Z03(WF)       V1ZCMD
067400960521     C                   END
067500960521     C                   GOTO      EMC01
067600960521    2C                   END
067700960521     C* Tasto F7=Interrogazione bolla
067800960521    2C     *INKG         IFEQ      *ON
067900960521     C                   SETON                                        22
068000960521     C     V1CPOS        CHAIN     LR44S01                            30
068100960521    3C     *IN30         IFEQ      *OFF
068200960523     C                   MOVE      V1HI51        *IN51
068300960523     C                   MOVE      V1HI52        *IN52
068400960521     C                   Z-ADD     NRR1          V1CNBR
068500960521     C                   UPDATE    LR44S01
068600960521     C                   SETOFF                                       22
068700960605     C                   CLEAR                   WAAS
068800960605     C                   MOVE      V1SLNP        WLNP
068900960605     C                   MOVE      V1SNRS        WNRS
069000960605     C                   MOVE      V1SNSP        WNSP
069100960605     C* Solo se ho la lna:
069200960605    4C     V1SLNA        IFGT      *ZEROS
069300021028     C**   V1SLNA        LOOKUP    L1                                     30
069400960605     C* Se non ho numero spedizione la cerco
069500021028     c* se non trovata bolla arrivi cerco nelle bolle partenza
069600021028     c*  a prescidenre che sia mia la bolla o non mia: tanto posso interrog
069700021028     c*
069800021028     c                   setoff                                       3031
069900021028     c* 30 off - ce'e' bolla ARRIVI
070000021028     C     KBLT          CHAIN     FNART27L                           30
070100021028     c* 30 on  - 31 off c'e' bolla partenza
070200021028     C   30KBLT          CHAIN     FNBLT27L                           31
070300021028     c*
070400021028    5C     V1SNSP        IFEQ      *ZEROS
070500021028     C     *IN30         IFEQ      *OFF
070600040531     C     *IN31         orEQ      *OFF
070700960605     C                   MOVE      D27AAS        WAAS
070800960605     C                   MOVE      D27LNP        WLNP
070900960605     C                   MOVE      D27NRS        WNRS
071000960605     C                   MOVE      D27NSP        WNSP
071100960605     C                   END
071200960605    5C                   END
071300960605     C*
071400960605    5C     WNSP          IFGT      *ZEROS
071500990513     C* Imposto anno a 4 cifre
071600960605    6C     WAAS          IFEQ      *ZEROS
071700990513    7C     V1SAAS        IFGT      60
071800990513     C                   MOVEL     19            WAAS
071900990513   X7C                   ELSE
072000990513     C                   MOVEL     20            WAAS
072100990513    7C                   END
072200990513     C                   MOVE      V1SAAS        WAAS
072300960605    6C                   END
072400021028     c*
072500021028     C* interrogo bolle arrivi
072600021028    6C     *IN30         IFEQ      *Off
072700960523     C                   CLEAR                   DSLR36
072800960523     C                   MOVE      WAAS          PA1AAS
072900960605     C                   MOVE      WLNP          PA1LNP
073000960605     C                   MOVE      WNRS          PA1NRS
073100960605     C                   MOVE      WNSP          PA1NSP
073200960903     C                   MOVE      'N'           PA1FLG
073300040121     C                   MOVE      *blanks       PA1CML
073400960523     C                   MOVEL     V1DPGM        PA1PGM
073500960523     C                   MOVEL     DSLR36        KPJBU
073600960523     C                   CALL      'FNLR36R'
073700960523     C                   PARM                    KPJBA
073800960605   X6C                   ELSE
073900021028     c**
074000960523     C* anomalia della partenza
074100021028   6ac                   if        not *in31
074200021028     C**   KBLP2         SETLL     FNBLP01L                               30
074300021028    7C**   *IN30         IFEQ      *ON
074400960523     C                   CLEAR                   DSLS05
074500960523     C                   MOVE      WAAS          PA0AAS
074600960605     C                   MOVE      WLNP          PA0LNP
074700960605     C                   MOVE      WNRS          PA0NRS
074800960605     C                   MOVE      WNSP          PA0NSP
074900960903     C                   MOVEL     'N'           PA0FLG
075000960523     C                   MOVEL     RAGUT         PA0RSU
075100960523     C                   MOVE      SIMFEL        PA0SIM
075200040121     C                   MOVE      *blanks       PA0CML
075300960523     C                   MOVEL     DSLS05        KPJBU
075400960523     C                   CALL      'FNLS05R'
075500960523     C                   PARM                    KPJBA
075600021028    7C**                 END
075700021028   6aC                   END
075800021028    6C                   END
075900960605    5C                   END
076000960605    4C                   END
076100960605    3C                   END
076200960521     C                   GOTO      EMC01
076300960521    2C                   END
076400960521     C* Tasto F14=Spunte
076500960521    2C     *INKN         IFEQ      *ON
076600960521     C                   SETON                                        22
076700960521     C     V1CPOS        CHAIN     LR44S01                            30
076800960521    3C     *IN30         IFEQ      *OFF
076900960523     C                   MOVE      V1HI51        *IN51
077000960523     C                   MOVE      V1HI52        *IN52
077100960521     C                   Z-ADD     NRR1          V1CNBR
077200960521     C                   UPDATE    LR44S01
077300960521     C                   SETOFF                                       22
077400960521    4C     V1SSCN        IFGT      *ZEROS
077500960521     C                   CLEAR                   DSLR04
077600960521     C                   Z-ADD     0             PA4NFV
077700960521     C                   MOVE      V1SFLS        PA4LNP
077800960521     C                   MOVE      V1SLNA        PA4LNA
077900960521     C                   MOVE      V1SNRS        PA4NRS
078000960521     C                   MOVEL     9             PA4NPG
078100960521     C                   Z-ADD     V1SSCN        PA4NCD
078200960521     C                   Z-ADD     0             PA4NCA
078300960521     C                   MOVEL     99            PA4NPS
078400960521     C                   MOVEL     'FNLR44R'     PA4PGM
078500960903     C                   MOVEL     '1'           PA4FLG
078600960521     C                   MOVEL     DSLR04        KPJBU
078700960521     C                   CALL      'FNLR04R'
078800960521     C                   PARM                    KPJBA
078900960521    4C                   END
079000960521    3C                   END
079100960521     C                   GOTO      EMC01
079200960521    2C                   END
079300960521     C* Tasto F10=Inserimento
079400960521    2C     *INKJ         IFEQ      *ON
079500970214     C* Se anomalia 300 emetto window di immissione
079600970214    3C     WA300         IFEQ      '1'
079700970217     C                   SETON                                        10
079800970214     C                   EXSR      WDW04
079900970217     C                   EXSR      PULSFL
080000970214     C                   GOTO      FXFMT
080100970214   X3C                   ELSE
080200960521     C                   EXSR      PULSFL
080300970217     C                   SETON                                        10
080400960521     C                   EXSR      RIS01B
080500960521     C                   MOVEL     Z02(3)        V1ZCMD
080600970214    3C                   END
080700960521     C                   GOTO      EMC01
080800960521    2C                   END
080900960521     C* Solo se ho caricato qualcosa:
081000960521    2C     NRR1          IFGT      *ZEROS
081100960521     C* Tasto Rollup
081200960521    3C     *IN25         IFEQ      *ON
081300960521     C* Fine lettura --> emetto messaggio fine scorrimento
081400960521    4C     *IN23         IFEQ      *ON                                          FINE LETTURA
081500960521     C                   Z-ADD     W1CNBR        V1CNBR
081600960521     C                   MOVEL     MSG(1)        V1ZMSG
081700960521     C                   SETON                                        28
081800960521     C                   GOTO      EMC01
081900960521   X4C                   ELSE
082000960521     C     *IN10         IFEQ      *ON
082100960521     C                   EXSR      RIS01B
082200960521     C                   GOTO      EMC01
082300960521     C                   ELSE
082400960521     C                   ADD       9             NRRAL
082500960521     C                   Z-ADD     NRRSAV        NRR1
082600960521     C     D44MOD        IFEQ      'G'
082700960521     C                   GOTO      FXFMT
082800960521     C                   ELSE
082900960521     C                   EXSR      RIS01I
083000960521     C                   GOTO      EMC01
083100960521     C                   END
083200960521     C                   END
083300960521    4C                   END
083400960521    3C                   END
083500960521     C* CONTROLLI SUBFILE
083600960521    3C     *IN10         IFEQ      *OFF
083700960521     C                   EXSR      CTRINT
083800960521     C   KC              MOVE      'S'           D34F12
083900960521     C   KC              EXSR      PULSFL
084000960521     C   KC              GOTO      FXFMT
084100960521     C* Se interrogate anomalie riemetto il subfile
084200960521     C     WINT          CABEQ     '1'           EMC01
084300960521     C                   END
084400960521     C* Solo se non sono in interrogazione
084500960521     C     D44MOD        IFEQ      'G'
084600960521     C     D34GES        ANDNE     'I'
084700960521     C  N10              EXSR      CTS01A
084800960521     C   10              EXSR      CTS01B
084900960521     C***
085000960521     C  NKF
085100971103     COR 90
085200960521     COR 28              GOTO      EMC01
085300960521     C*
085400960521    3C     *IN10         IFEQ      *ON
085500960521     C   KF              EXSR      WTRANM
085600960521     C                   EXSR      PULSFL
085700960521     C                   ELSE
085800960521     C   KF              EXSR      AGGANM
085900960521     C                   GOTO      EMC01
086000960521    3C                   END
086100960523     C                   END
086200960521    2C                   END
086300960521     C     FXFMT         ENDSR
086400960327     C****************   PULIZIA SUBFILE ******************************
086500960327     C     PULSFL        BEGSR
086600960327     C                   SETOFF                                       2123
086700960327     C                   WRITE     LR44C01
086800960327     C                   SETON                                        2021
086900960327     C                   MOVE      *ZEROS        NRR1              5 0
087000960327     C                   MOVE      *ZEROS        NRRSAV
087100960327     C                   MOVE      *ZEROS        W1CNBR
087200960327     C                   MOVE      *ZEROS        V1CNBR
087300960327     C                   Z-ADD     9             NRRAL
087400960402     C                   SETOFF                                       100405
087500960405     C                   SETON                                        09
087600960327     C                   ENDSR
087700960327     C*********** RIEMPIMENTO CAMPI SUBFILE S01 ***********************
087800960328     C     RIS01A        BEGSR
087900960405     C                   SETOFF                                       22  49
088000960401     C                   SETOFF                                       404142
088100960401     C                   SETOFF                                       434445
088200960401     C                   SETOFF                                       464748
088300960328     C                   MOVE      ANMAAS        V1SAAS
088400960328     C                   MOVE      ANMLNP        V1SLNP
088500960328     C                   MOVE      ANMNRS        V1SNRS
088600960328     C                   MOVE      ANMNSP        V1SNSP
088700960328     C                   MOVE      ANMLNA        V1SLNA
088800960401     C                   MOVE      ANMFLS        V1SFLS
088900960328     C                   MOVE      ANMSCN        V1SSCN
089000040603     c                   move      anmft4        v1sbva
089100960327     C* DECODIFICO TIPO ANOMALIA
089200960328     C                   MOVEL     '7F'          KCOD
089300960328     C                   MOVEL(P)  ANMTAN        KKEY
089400960328     C     KTAB          CHAIN     TABEL00F                           30
089500960328     C  N30              MOVEL     TBLUNI        V1DTAN
089600960328     C   30              CLEAR                   V1DTAN
089700960404     C                   MOVE      ANMTAN        V1HTAN
089800960327     C* DECODIFICO SOGGETTO ANOMALIA
089900960328     C                   MOVEL     '7G'          KCOD
090000960328     C                   MOVEL(P)  ANMSAN        KKEY
090100960328     C     KTAB          CHAIN     TABEL00F                           30
090200960328     C  N30              MOVEL     TBLUNI        V1DSAN
090300960404     C   30              CLEAR                   V1DSAN
090400960404     C                   MOVE      ANMSAN        V1HSAN
090500960328     C                   MOVE      ANMCAA        V1SCAA
090600960403     C                   MOVE      ANMCAA        V1HCAA
090700960409     C                   CLEAR                   V1HKSP
090800970507     C                   CLEAR                   V1HARB
090900960506     C* DECODIFICO CODICE ANOMALIA
091000960401     C                   CLEAR                   DS7C
091100960328     C                   MOVEL     '7C'          KCOD
091200960328     C                   MOVEL(P)  ANMCAA        KKEY
091300960328     C                   CLEAR                   V1DCAA
091400960328     C                   CLEAR                   V1HGES
091500960328     C                   CLEAR                   V1HANN
091600960402     C                   CLEAR                   V1HKEY
091700960404     C                   CLEAR                   V1HLIO
091800960411     C                   CLEAR                   V1HUEA
091900960328     C     KTAB          CHAIN     TABEL00F                           30
092000960328     C     *IN30         IFEQ      *OFF
092100960328     C                   MOVEL     TBLUNI        DS7C
092200960328     C                   MOVEL     §7CDES        V1DCAA
092300960328     C* Memorizzo in campi hidden se caus.gestibile in manut./ins./ann
092400960328     C                   MOVEL     §7CGES        V1HGES
092500960418     C     D34APA        IFEQ      'P'
092600960328     C                   MOVEL     §7CANN        V1HANN
092700960418     C                   ELSE
092800960418     C                   MOVEL     §7CANA        V1HANN
092900960418     C                   END
093000960402     C                   MOVEL     §7CKEY        V1HKEY
093100960404     C     §7CIDD        IFEQ      'S'
093200960404     C                   MOVEL     §7CLID        V1HLIO
093300960404     C                   END
093400960411     C                   MOVE      §7CUEA        V1HUEA
093500960328     C                   END
093600960507     C                   MOVE      ANMAIE        V1HAIE
093700960507     C     ANMAIE        IFEQ      'I'
093800960328     C                   MOVE      ANMAIE        V1SAIE
093900960507     C                   ELSE
094000960507     C     §7CIDD        IFEQ      'S'
094100960507     C                   MOVEL     'D'           V1SAIE
094200960507     C                   ELSE
094300960507     C                   MOVEL     'N'           V1SAIE
094400960507     C                   END
094500960507     C                   END
094600960401     C     §7CIDD        IFEQ      'S'
094700960328     C                   MOVE      ANMLID        V1SLID
094800960401     C                   ELSE
094900960401     C                   CLEAR                   V1SLID
095000960401     C                   END
095100960328     C* DECODIFICO FASE APERTURA ANOMALIA
095200960328     C                   MOVEL     '7D'          KCOD
095300960328     C                   MOVEL(P)  ANMFSA        KKEY
095400960328     C                   CLEAR                   V1DFSA
095500960328     C     KTAB          CHAIN     TABEL00F                           30
095600960328     C     *IN30         IFEQ      *OFF
095700960328     C                   MOVEL     TBLUNI        DS7D
095800960328     C                   MOVEL     §7DDES        V1DFSA
095900960328     C                   END
096000960404     C                   MOVE      ANMFSA        V1HFSA
096100960328     C* ROVESCIO DATA ANOMALIA
096200990513     C                   CLEAR                   WLBDA8
096300990513     C                   MOVE      ANMDAO        G08INV
096400990513     C                   MOVEL     '3'           G08ERR
096500990513     C                   CALL      'XSRDA8'
096600990513     C                   PARM                    WLBDA8
096700990513     C                   MOVEL     G08DAT        V1SDAO
096800990513     C                   MOVE      G08DAT        W0020             2 0
096900990513     C                   MOVE      W0020         V1SDAO
097000960419     C* RIEMPIO CAMPI RELATIVI AI DATI DI CHIUSURA (FASE,DATA,CAUSALE)
097100960419     C                   EXSR      RIECHI
097200960403     C     ANMCDU        IFGT      *ZEROS
097300960328     C                   MOVE      ANMCDU        V1SCDU
097400960403     C                   ELSE
097500960403     C                   CLEAR                   V1SCDU
097600960403     C                   END
097700960328     C                   MOVE      ANMNFV        V1SNFV
097800960328     C                   MOVE      ANMNPS        V1SNPS
097900960412     C                   CLEAR                   V1SVAR
098000960412     C                   CLEAR                   V1HVAR
098100960402     C                   MOVEL     ANMATB        V1SATB
098200960412     C                   Z-ADD     D44NRR        V1HNRR
098300960402     C                   MOVE      *IN04         V1HIN4
098400960402     C                   MOVE      *IN05         V1HIN5
098500960405     C                   MOVE      *IN09         V1HIN9
098600960401     C                   MOVEL     D34APA        V1HAPA
098700960401     C                   CLEAR                   V1HPRE
098800960327     C                   ENDSR
098900960419     C*********** RIEMPIMENTO CAMPI CHIUSURA ANOMALIA DI SFL S01 ******
099000960419     C     RIECHI        BEGSR
099100960419     C* DECODIFICO FASE CHIUSURA ANOMALIA
099200960419     C                   MOVE      *BLANKS       V1DFSC
099300960419     C     ANMFSC        IFNE      *BLANKS
099400960419     C                   MOVEL     '7D'          KCOD
099500960419     C                   MOVEL(P)  ANMFSC        KKEY
099600960419     C     KTAB          CHAIN     TABEL00F                           30
099700960419     C     *IN30         IFEQ      *OFF
099800960419     C                   MOVEL     TBLUNI        DS7D
099900960419     C                   MOVEL     §7DDES        V1DFSC
100000960419     C                   END
100100960419     C                   END
100200960419     C* ROVESCIO DATA CHIUSURA ANOMALIA SE DIVERSA DA 0
100300960419     C                   MOVE      *ZEROS        V1SDCH
100400960419     C     ANMDCH        IFGT      *ZEROS
100500990513     C                   CLEAR                   WLBDA8
100600990513     C                   MOVEL     ANMDCH        G08INV
100700990513     C                   MOVEL     '3'           G08ERR
100800990513     C                   CALL      'XSRDA8'
100900990513     C                   PARM                    WLBDA8
101000990514     C                   MOVEL     G08DAT        V1SDCH
101100990513     C                   MOVE      G08DAT        W0020             2 0
101200990513     C                   MOVE      W0020         V1SDCH
101300960419     C                   END
101400041125     C
101500960419     C                   MOVE      ANMCCH        V1SCCH
101600970522     C*
101700960419     C                   ENDSR
101800960328     C**************  RIEMPIMENTO SUBFILE PER INSERIMENTO *************
101900960328     C     RIS01B        BEGSR
102000960429     C                   SETOFF                                       22
102100960523     C                   SETOFF                                       5152
102200960328     C                   Z-ADD     NRRSAV        NRR1
102300960328     C     NRR1          ADD       1             V1CNBR
102400960329     C     NRR1          DOWLT     NRRAL
102500960328     C                   CLEAR                   V1SAAS
102600960328     C                   CLEAR                   V1SLNP
102700960328     C                   CLEAR                   V1SNRS
102800960328     C                   CLEAR                   V1SNSP
102900960402     C* IMPOSTO LINEA DI ARRIVO = FILIALE GESITIONE
103000960402     C                   MOVE      D34FGS        V1SLNA
103100960401     C                   CLEAR                   V1SFLS
103200960328     C                   CLEAR                   V1SSCN
103300960328     C                   CLEAR                   V1DTAN
103400960328     C                   CLEAR                   V1DSAN
103500960329     C                   CLEAR                   V1SCAA
103600960328     C                   CLEAR                   V1DCAA
103700960328     C                   CLEAR                   V1HGES
103800960328     C                   CLEAR                   V1HANN
103900960402     C                   CLEAR                   V1HTAN
104000960402     C                   CLEAR                   V1HSAN
104100960402     C                   CLEAR                   V1HKEY
104200960404     C                   CLEAR                   V1HLIO
104300960403     C                   CLEAR                   V1HCAA
104400960409     C                   CLEAR                   V1HKSP
104500970507     C                   CLEAR                   V1HARB
104600960329     C                   CLEAR                   V1HNRR
104700960411     C                   CLEAR                   V1HUEA
104800960328     C                   CLEAR                   V1SAIE
104900960507     C                   CLEAR                   V1HAIE
105000960328     C                   CLEAR                   V1SLID
105100960401     C                   MOVE      'A'           V1HAPA
105200960329     C                   MOVEL     '2'           V1HFSA
105300960329     C                   MOVEL     '7D'          KCOD
105400960329     C                   MOVEL(P)  '2'           KKEY
105500960329     C     KTAB          CHAIN     TABEL00F                           30
105600960329     C  N30              MOVEL     TBLUNI        V1DFSA
105700960329     C   30              CLEAR                   V1DFSA
105800971118     C* IMPOSTO DATA ANOMALIA = DATA DEL GIORNO
105900971118     C                   MOVE      DTGIO6        V1SDAO
106000990513     C                   MOVE      DATEU8        V1HDAO
106100960328     C                   CLEAR                   V1DFSC
106200960328     C                   CLEAR                   V1SDCH
106300960328     C                   CLEAR                   V1SCDU
106400960328     C                   CLEAR                   V1SNFV
106500960328     C                   CLEAR                   V1SNPS
106600960328     C                   CLEAR                   V1SCCH
106700960401     C                   CLEAR                   V1HPRE
106800960402     C                   MOVE      *IN04         V1HIN4
106900960402     C                   MOVE      *IN05         V1HIN5
107000960405     C                   MOVE      *IN09         V1HIN9
107100960523     C                   MOVE      '0'           V1HI51
107200960523     C                   MOVE      '0'           V1HI52
107300960328     C                   ADD       1             NRR1
107400960328     C                   WRITE     LR44S01
107500960328     C                   ENDDO
107600960328     C                   Z-ADD     NRR1          NRRSAV
107700960328     C                   ADD       9             NRRAL
107800960328     C                   ENDSR
107900960329     C******  CONTROLLI SUBFILE  PER INTERROGAZIONE RECORD ANOMALIA ***
108000960329     C     CTRINT        BEGSR
108100960403     C                   SETON                                        22
108200960329     C                   MOVE      *BLANKS       WINT              1
108300960329     C                   READC     LR44S01                                29
108400960329    1C     *IN29         DOWEQ     *OFF
108500960412     C     V1SVAR        IFEQ      'I'
108600960329     C* ESPLODO RECORD DI SUBFILE
108700960412     C                   CLEAR                   V1SVAR
108800960329     C                   MOVE      '1'           WINT
108900960402     C                   EXSR      SRINT
109000960509     C                   Z-ADD     NRR1          V1CNBR
109100961210     C                   MOVEL     DSANMX        DSANM
109200960329     C                   END
109300960402     C                   MOVE      V1HIN4        *IN04
109400960402     C                   MOVE      V1HIN5        *IN05
109500960405     C                   MOVE      V1HIN9        *IN09
109600960523     C                   MOVE      V1HI51        *IN51
109700960523     C                   MOVE      V1HI52        *IN52
109800960329     C                   UPDATE    LR44S01
109900960405     C* PREMUTO F3 DA VIDEATA INTERROGAZIONE ANOMALIA: ESCO DAL PGM
110000960403     C     *INKC         CABEQ     '1'           ENDINT
110100960329     C                   READC     LR44S01                                29
110200960329     C                   ENDDO
110300960403     C     ENDINT        ENDSR
110400960328     C******  CONTROLLI SUBFILE  MODALITA' INTERROGAZ/MANUTENZ. *******
110500960328     C     CTS01A        BEGSR
110600960403     C                   SETON                                        22
110700960426     C                   MOVE      ' '           SAVNBR            1
110800960329     C                   READC     LR44S01                                29
110900960329    1C     *IN29         DOWEQ     *OFF
111000960412    2C     V1SVAR        IFNE      *BLANKS
111100960412     C     V1HVAR        ORNE      *BLANKS
111200990512     C     V1HNRR        CHAIN(N)  FNANM00F                           30
111300960412     C* SE NON TROVATA ANOMALIA ERRORE
111400960412     C     *IN30         IFEQ      *ON
111500960412     C                   MOVE      'A'           V1SATB
111600960412     C                   MOVEL     MSG(31)       V1ZMSG
111700960412     C                   SETON                                        4728
111800960412     C                   GOTO      ENDCTA
111900960412     C                   END
112000960418     C* ERRORE SE RICHIESTA MANUTENZIONE/ANNULLAMENTO SU ANOMALIA GIà
112100960418     C*           CHIUSA CON CAUSALE DIVERSA DA 'AN'
112200970217     C* ESLUDO IL SEGUENTE CONTROLLO SE SI TRATTA DI AN.300=N°RITIRI
112300960418     C     ANMDCH        IFGT      *ZEROS
112400960418     C     ANMCCH        ANDNE     'AN'
112500960422     C     ANMCCH        ANDNE     'PR'
112600970217     C     ANMCAA        ANDNE     300
112700960403     C                   SELECT
112800960412     C     V1SVAR        WHENEQ    'A'
112900960403     C                   MOVEL     MSG(12)       V1ZMSG
113000960412     C                   SETON                                        4728
113100960412     C                   GOTO      ENDCTA
113200960412     C     V1SVAR        WHENEQ    'R'
113300960403     C                   MOVEL     MSG(14)       V1ZMSG
113400960412     C                   SETON                                        4728
113500960412     C                   GOTO      ENDCTA
113600960412     C     V1HVAR        WHENEQ    'V'
113700960412     C     *INKF         ANDEQ     *ON
113800960412     C                   MOVEL     MSG(30)       V1ZMSG
113900960412     C                   SETON                                        040528
114000960412     C                   SETOFF                                       09
114100960412     C                   MOVE      V1HNRR        D44NRR
114200960412     C                   EXSR      RIS01A
114300960412     C                   SETON                                        2247
114400960412     C                   GOTO      ENDCTA
114500960412     C     V1SVAR        WHENEQ    'V'
114600960403     C                   MOVEL     MSG(16)       V1ZMSG
114700960412     C                   SETON                                        4728
114800960412     C                   GOTO      ENDCTA
114900960403     C                   ENDSL
115000960329    3C                   END
115100960423     C**** VERIFICO ANNULLABILITA' DELL'ANOMALIA ***
115200960412    3C     V1SVAR        IFEQ      'A'
115300960418     c* Mai annullabile
115400960329    4C     V1HANN        IFEQ      *BLANKS
115500960418     C* Annullabile solo prima della trasmissione
115600960418     C     V1HANN        OREQ      'P'
115700960418     C     ANMFT1        ANDNE     *BLANKS
115800960418     C     V1HANN        OREQ      'P'
115900960418     C     V1HLIO        ANDNE     V1SLID
116000960506     C     V1SCAA        ANDNE     '070'
116100960506     C     V1SCAA        ANDNE     '071'
116200960418     C* Annullabile solo dopo la trasmissione
116300960418     C     V1HANN        OREQ      'D'
116400960418     C     ANMFT1        ANDEQ     *BLANKS
116500960418     C     V1HANN        OREQ      'D'
116600960418     C     V1HLIO        ANDEQ     V1SLID
116700960506     C     V1SCAA        ANDNE     '070'
116800960506     C     V1SCAA        ANDNE     '071'
116900960329     C                   MOVEL     MSG(12)       V1ZMSG
117000960401     C                   SETON                                        4728
117100960329     C                   GOTO      ENDCTA
117200960329    4C                   END
117300960418     C* Anomalia già annullata
117400960329    4C     V1SATB        IFNE      *BLANKS
117500960418     C     ANMCCH        OREQ      'AN'
117600960329     C                   MOVEL     MSG(13)       V1ZMSG
117700960401     C                   SETON                                        4728
117800960329     C                   GOTO      ENDCTA
117900960329    4C                   END
118000960429     C* Controlli particolari per annullamento anomalie partenza:
118100960429     C* 005 - Collo partito non arrivato
118200960429     C* 010 - No record bolla arrivo
118300960429     C* 012 - Manca record bolla e colli
118400960429    4C     V1HAPA        IFEQ      'P'
118500960429    5C     V1SCAA        IFEQ      '005'
118600960429     C     V1SCAA        OREQ      '010'
118700960429     C     V1SCAA        OREQ      '012'
118800960429     C                   EXSR      CTRANN
118900960429     C     WFLG01        IFEQ      'E'
119000960429     C                   MOVEL     MSG(34)       V1ZMSG
119100960429     C                   SETON                                        4728
119200960429     C                   GOTO      ENDCTA
119300960429     C                   END
119400960429    5C                   END
119500960429    4C                   END
119600970214    3C                   END
119700960423     C**** VERIFICO SE ANOMALIA RIPRISTINABILE ***
119800960412    3C     V1SVAR        IFEQ      'R'
119900960419     c* Mai ripristinabile
120000960329    4C     V1HANN        IFEQ      *BLANKS
120100960401     C                   MOVEL     MSG(14)       V1ZMSG
120200960401     C                   SETON                                        4728
120300960329     C                   GOTO      ENDCTA
120400960329    4C                   END
120500960329    4C     V1SATB        IFEQ      *BLANKS
120600960419     C     ANMCCH        ANDNE     'AN'
120700960329     C                   MOVEL     MSG(15)       V1ZMSG
120800960401     C                   SETON                                        4728
120900960329     C                   GOTO      ENDCTA
121000960329    4C                   END
121100960401    3C                   END
121200960423     C**** VERIFICO SE ANOMALIA MANUTENZIONABILE  ***
121300960412    3C     V1SVAR        IFEQ      'V'
121400960423     C* Non manutenzionabile se anomalia della partenza o anomalia
121500960423     C* già trasmessa
121600960419     C     V1HAPA        IFEQ      'P'
121700970214     C     ANMCAA        ANDNE     300
121800960423     C     ANMFT1        ORNE      *BLANKS
121900960423     C     V1SLID        ORNE      V1HLIO
122000960506     C     V1SCAA        ANDNE     '070'
122100960506     C     V1SCAA        ANDNE     '071'
122200960419     C                   MOVEL     MSG(16)       V1ZMSG
122300960419     C                   SETON                                        4728
122400960419     C                   GOTO      ENDCTA
122500960419     C                   END
122600970214     C* Se richiesta manutenzione anomalia 300 emetto window anzichè
122700970214     C* sproteggere i campi del subfile
122800970214     C     ANMCAA        IFEQ      300
122900040923     c     v1hges        ifeq      'S'
123000970214     C                   EXSR      WDW04
123100040923     C                   ELSE
123200040923     C                   MOVEL     MSG(16)       V1ZMSG
123300040923     C                   SETON                                        4728
123400040923     c                   endif
123500970214     C                   GOTO      ENDCTA
123600970214     C                   END
123700960412     C* CAMPI PROTETTI: SE POSSIBILE LI SPROTEGGO
123800960412    4C     V1HIN4        IFEQ      '1'
123900960412   4BC     *INKF         IFEQ      *OFF
124000960412    5C     V1HGES        IFEQ      'S'
124100960412     C                   SETOFF                                       04
124200960412     C                   SETON                                        09
124300960412     C                   MOVE      *IN04         V1HIN4
124400960412     C                   MOVE      *IN09         V1HIN9
124500960412     C                   MOVE      V1SVAR        V1HVAR
124600960506     C* Sproteggo codice anomalia solo se annullabile:
124700960412    6C     V1HANN        IFEQ      *BLANKS
124800960419     C     V1HANN        OREQ      'P'
124900960419     C     ANMFT1        ANDNE     *BLANKS
125000960419     C     V1HANN        OREQ      'P'
125100960419     C     V1HLIO        ANDNE     V1SLID
125200960506     C     V1SCAA        ANDNE     '070'
125300960506     C     V1SCAA        ANDNE     '071'
125400960419     C     V1HANN        OREQ      'D'
125500960419     C     ANMFT1        ANDEQ     *BLANKS
125600960419     C     V1HLIO        ANDEQ     V1SLID
125700960506     C     V1SCAA        ANDNE     '070'
125800960506     C     V1SCAA        ANDNE     '071'
125900960419     C* . . . Protezione
126000960412     C                   SETON                                        05
126100960412   X6C                   ELSE
126200960419     C* . . . Sprotezione
126300960412     C                   SETOFF                                       05
126400960412    6C                   END
126500960412     C                   MOVE      *IN05         V1HIN5
126600960412   X5C                   ELSE
126700960412     C                   MOVEL     MSG(16)       V1ZMSG
126800960412     C                   SETON                                        4728
126900960412     C                   GOTO      ENDCTA
127000960412    5C                   END
127100960412   4BC                   END
127200960412   X4C                   ELSE
127300960412     C* CAMPI GIA' SPROTETTI --> LI CONTROLLO
127400960412     C                   EXSR      CTRS01
127500960412    4C                   END
127600960412     C                   MOVE      *BLANKS       V1SVAR
127700960412   X3C                   ELSE
127800960412     C* CAMPI GIA' SPROTETTI --> LI CONTROLLO
127900960412    4C     V1HVAR        IFEQ      'V'
128000960423     C* Se premuto tasto di conferma e anomalia già trasmessa: errore
128100960423     C   KFANMFT1        IFNE      *BLANKS
128200960423     C     V1HLIO        ORNE      V1SLID
128300960506     C     V1SCAA        ANDNE     '070'
128400960506     C     V1SCAA        ANDNE     '071'
128500960423     C                   MOVEL     MSG(30)       V1ZMSG
128600960423     C                   SETON                                        040528
128700960423     C                   SETOFF                                       09
128800960423     C                   MOVE      V1HNRR        D44NRR
128900960423     C                   EXSR      RIS01A
129000960423     C                   SETON                                        2247
129100960423     C                   GOTO      ENDCTA
129200960423     C                   END
129300960412     C                   EXSR      CTRS01
129400960412    4C                   END
129500960412    3C                   END
129600960329     C*
129700960509    2C                   END
129800960401     C     ENDCTA        TAG
129900960402     C                   MOVE      V1HIN4        *IN04
130000960402     C                   MOVE      V1HIN5        *IN05
130100960405     C                   MOVE      V1HIN9        *IN09
130200960426     C     *IN28         IFEQ      *ON
130300960513     C     V1SVAR        ORNE      *BLANKS
130400960426     C     SAVNBR        ANDEQ     ' '
130500960426     C     V1HVAR        OREQ      'V'
130600960426     C     SAVNBR        ANDEQ     ' '
130700960329     C                   Z-ADD     NRR1          V1CNBR
130800960426     C                   MOVE      '1'           SAVNBR
130900960426     C                   END
131000960523     C                   MOVE      V1HI51        *IN51
131100960523     C                   MOVE      V1HI52        *IN52
131200960329     C                   UPDATE    LR44S01
131300971103     C   28              SETON                                        2990
131400960329     C  N29              READC     LR44S01                                29
131500960329    1C                   ENDDO
131600960401     C                   ENDSR
131700960328     C******  CONTROLLI SUBFILE  MODALITA' INSERIMENTO     ************
131800960328     C     CTS01B        BEGSR
131900960403     C                   SETON                                        22
132000960405     C* PULIZIA CAMPI DI COMODO
132100960405     C                   MOVE      *ZEROS        II
132200960405     C                   MOVE      *ZEROS        LL
132300960405     C                   CLEAR                   SKB
132400960405     C                   CLEAR                   SKS
132500960329     C* azzero campi per gestione DUP
132600960329     C                   CLEAR                   DUPAAS
132700960329     C                   CLEAR                   DUPCAA
132800960401     C                   CLEAR                   DUPLNP
132900960401     C                   CLEAR                   DUPNRS
133000960401     C                   CLEAR                   DUPNSP
133100960401     C                   CLEAR                   DUPLNA
133200960401     C                   CLEAR                   DUPFLS
133300960401     C                   CLEAR                   DUPSCN
133400960401     C                   CLEAR                   DUPDAO
133500960329     C*
133600960329     C                   READC     LR44S01                                29
133700960329    1C     *IN29         DOWEQ     *OFF
133800960329     C                   CLEAR                   V1DCAA
133900960329     C                   CLEAR                   V1DSAN
134000960329     C                   CLEAR                   V1DTAN
134100960329     C                   CLEAR                   V1SAIE
134200960507     C                   CLEAR                   V1HAIE
134300960329     C*   Gestione tasto DUP
134400960401     C   11              MOVE      DUPCAA        V1SCAA
134500960401     C   12              MOVE      DUPAAS        V1SAAS
134600960401     C   13              MOVE      DUPNRS        V1SNRS
134700960401     C   14              MOVE      DUPNSP        V1SNSP
134800960401     C   15              MOVE      DUPLNA        V1SLNA
134900960401     C   16              MOVE      DUPLNP        V1SLNP
135000960401     C   17              MOVE      DUPSCN        V1SSCN
135100960401     C   18              MOVE      DUPDAO        V1SDAO
135200960401     C   19              MOVE      DUPFLS        V1SFLS
135300960329     C* CONTROLLO SOLO SE IMMESSI DATI
135400960329    2C     IOFLD         IFGT      *ZEROS
135500960329     C     V1SCAA        ORNE      *BLANKS
135600960329     C     V1SCAA        ANDNE     *ZEROS
135700960329     C                   EXSR      CTRS01
135800960409     C                   ELSE
135900960409     C                   CLEAR                   V1HKSP
136000970507     C                   CLEAR                   V1HARB
136100960409     C                   CLEAR                   V1HPRE
136200960329    2C                   END
136300960523     C                   MOVE      V1HI51        *IN51
136400960523     C                   MOVE      V1HI52        *IN52
136500960329     C                   Z-ADD     NRR1          V1CNBR
136600960329     C                   UPDATE    LR44S01
136700971103     C   28              SETON                                        2990
136800960329     C  N29              READC     LR44S01                                29
136900960329    1C                   ENDDO
137000960328     C                   ENDSR
137100960329     C*************  CONTROLLI COMUNI SUBFILE  ************************
137200960329     C     CTRS01        BEGSR
137300960506     C*** CODICE ANOMALIA  (SOLO SE SPROTETTO)
137400960402   1AC     V1HIN5        IFEQ      '0'
137500960329     C*   Ricerca in tabella
137600960401     C     '?'           SCAN      V1SCAA                                 30
137700960401    1C     *IN30         IFEQ      *ON
137800960329     C                   MOVEL     CODUT         §KUT
137900960329     C                   MOVEL     '7C'          §COD
138000960329     C                   MOVEL     *BLANKS       §KEY
138100960329     C                   CALL      'X§TABER'
138200960329     C                   PARM                    §KUT              1 0
138300960329     C                   PARM                    §COD              2
138400960329     C                   PARM                    §KEY              8
138500960329     C                   PARM                    §DES             30
138600960329     C                   MOVEL     §KEY          V1SCAA
138700960401    2C     V1SCAA        IFNE      *BLANKS
138800960329     C                   MOVEL     §DES          V1DCAA
138900960401    2C                   END
139000971103     C                   SETON                                        2990
139100960329     C                   GOTO      ENDCTR
139200960401    1C                   END
139300960329     C*   Controlli di validità'
139400960401    1C     V1SCAA        IFEQ      *ZEROS
139500960329     C     V1SCAA        OREQ      *BLANKS
139600960329     C                   MOVEL     MSG(2)        V1ZMSG
139700960401     C                   SETON                                        4028
139800960329     C                   GOTO      ENDCTR
139900960401    1C                   END
140000960329     C                   TESTN                   V1SCAA               30
140100960401    1C     *IN30         IFEQ      *ON
140200960329     C                   MOVE      V1SCAA        W001A             1
140300960401    2C     W001A         IFLT      '0'
140400960329     C                   SETOFF                                       30
140500960401    2C                   END
140600960401    1C                   END
140700960401    1C     *IN30         IFEQ      *OFF
140800960329     C                   MOVEL     MSG(2)        V1ZMSG
140900960401     C                   SETON                                        4028
141000960329     C                   GOTO      ENDCTR
141100960401    1C                   END
141200960329     C* Chaino tabella per reperimento altri dati
141300960329     C                   MOVEL     '7C'          KCOD
141400960329     C                   MOVEL(P)  V1SCAA        KKEY
141500960329     C     KTAB          CHAIN     TABEL00F                           30
141600960401    1C     *IN30         IFEQ      *ON
141700960329     C     TBLFLG        ORNE      *BLANKS
141800960329     C                   MOVEL     MSG(2)        V1ZMSG
141900960401     C                   SETON                                        4028
142000960329     C                   GOTO      ENDCTR
142100960401    1C                   END
142200960329     C                   MOVEL     TBLUNI        DS7C
142300960329     C                   MOVEL     §7CDES        V1DCAA
142400960506     C*  Controllo che codice anomalia sia gestibile in inserimento
142500960402    1C     §7CGES        IFEQ      *BLANKS
142600960329     C                   MOVEL     MSG(5)        V1ZMSG
142700960401     C                   SETON                                        4028
142800960329     C                   GOTO      ENDCTR
142900960401    1C                   END
143000960506     C*  Se codice ok imposto anche altri campi di output del video
143100960402     C*  (N.B.: v1hges e v1hann mantengo quelli di origine)
143200960329     C                   MOVEL     §7CTAN        V1HTAN
143300960329     C                   MOVEL     §7CSAN        V1HSAN
143400960402     C                   MOVEL     §7CKEY        V1HKEY
143500960411     C                   MOVEL     §7CUEA        V1HUEA
143600960329     C                   MOVEL     '7F'          KCOD
143700960329     C                   MOVEL(P)  §7CTAN        KKEY
143800960329     C     KTAB          CHAIN     TABEL00F                           30
143900960329     C  N30              MOVEL     TBLUNI        V1DTAN
144000960329     C                   MOVEL     '7G'          KCOD
144100960329     C                   MOVEL(P)  §7CSAN        KKEY
144200960329     C     KTAB          CHAIN     TABEL00F                           30
144300960329     C  N30              MOVEL     TBLUNI        V1DSAN
144400960507     C                   MOVEL     §7CAIE        V1HAIE
144500960507     C     §7CIDD        IFEQ      'S'
144600960507     C                   MOVEL     'D'           V1SAIE
144700960507     C                   ELSE
144800960507     C                   MOVEL     'N'           V1SAIE
144900960507     C                   END
145000960402   1AC                   END
145100960402     C* CONTROLLO LINEE SE IMMESSE
145200960402     C* - LA LNP, SE IMMESSA, Deve esistere in organigramma
145300960402    1C     V1SLNP        IFGT      *ZEROS
145400960402     C     V1SLNP        CHAIN     AZORG01L                           30
145500960402    2C     *IN30         IFEQ      *ON
145600960402     C     ORGFVA        ORNE      *BLANKS
145700960402     C                   MOVEL     MSG(3)        V1ZMSG
145800960402     C                   SETON                                        4328
145900960402     C                   GOTO      ENDCTR
146000960402    2C                   END
146100960402    1C                   END
146200960402     C* - LA Fls, SE IMMESSA, Deve esistere in organigramma
146300960402    1C     V1SFLS        IFGT      *ZEROS
146400960402     C     V1SFLS        CHAIN     AZORG01L                           30
146500960402    2C     *IN30         IFEQ      *ON
146600960402     C     ORGFVA        ORNE      *BLANKS
146700960402     C                   MOVEL     MSG(4)        V1ZMSG
146800960402     C                   SETON                                        4828
146900960402     C                   GOTO      ENDCTR
147000960402    2C                   END
147100960402    1C                   END
147200960411     C* - La Lna deve essere una filiale in gestione
147300960418     C*   (Se è un'anomalia per la quale occorre utilizzare la LNA
147400960418     C*   anzichè la filiale emittente)
147500960402    1C     V1SLNA        IFGT      *ZEROS
147600960411    2C     V1HUEA        IFEQ      *BLANKS
147700960411    3C     V1SLNA        IFNE      D34FGS
147800960402     C     V1SLNA        LOOKUP    L6S                                    30
147900960411    4C     *IN30         IFEQ      *OFF
148000960402     C                   MOVEL     MSG(8)        V1ZMSG
148100960402     C                   SETON                                        4528
148200960402     C                   GOTO      ENDCTR
148300960411    4C                   END
148400960411    3C                   END
148500960411   X2C                   ELSE
148600960524     C* - La Lna deve esistere in organigramma e deve essere diversa
148700960524     C*   dalla filiale emittente
148800960411     C*   (Se per questa anomalia occorre utilizzare la filiale emit-
148900960411     C*   tente)
149000960524    3C     V1SLNA        IFEQ      D34FGS
149100960524     C                   MOVEL     MSG(36)       V1ZMSG
149200960524     C                   SETON                                        4528
149300960524     C                   GOTO      ENDCTR
149400960524    3C                   END
149500960411     C     V1SLNA        SETLL     AZORG01L                               30
149600960411    3C     *IN30         IFEQ      *OFF
149700960411     C                   MOVEL     MSG(7)        V1ZMSG
149800960411     C                   SETON                                        4528
149900960411     C                   GOTO      ENDCTR
150000960411    3C                   END
150100960402    2C                   END
150200960402    1C                   END
150300960401     C***** CONTROLLO CHIAVE PRINCIPALE:
150400960401    1C                   SELECT
150500960402     C*     - PER SPEDIZIONE (AAS/LNP/NRS/NSP/LNA)
150600960402     C     V1HKEY        WHENEQ    'B'
150700960401     C* LINEA DI PARTENZA OBBLIGATORIA
150800960401    2C     V1SLNP        IFEQ      *ZEROS
150900960401     C                   SETON                                        4328
151000960329     C                   MOVEL     MSG(3)        V1ZMSG
151100960329     C                   GOTO      ENDCTR
151200960401    2C                   END
151300960401     C* NUMERO SPEDIZIONE OBBLIGATORIO
151400960401    2C     V1SNSP        IFEQ      *ZEROS
151500960401     C                   SETON                                        4128
151600960401     C                   MOVEL     MSG(6)        V1ZMSG
151700960401     C                   GOTO      ENDCTR
151800960401    2C                   END
151900960402     C     V1HKEY        WHENEQ    'S'
152000960401     C*    - PER SEGNACOLLO (FLS/LNA/NRS/SCN)
152100960401     C* FILIALE SEGNACOLLO OBBLIGATORIO
152200960401    2C     V1SFLS        IFEQ      *ZEROS
152300960401     C                   SETON                                        4828
152400960401     C                   MOVEL     MSG(4)        V1ZMSG
152500960401     C                   GOTO      ENDCTR
152600960401    2C                   END
152700960401     C* NUMERO SEGNACOLLO OBBLIGATORIO
152800960401    2C     V1SSCN        IFEQ      *ZEROS
152900960401     C                   SETON                                        4228
153000960401     C                   MOVEL     MSG(17)       V1ZMSG
153100960401     C                   GOTO      ENDCTR
153200960401    2C                   END
153300960401    1C                   ENDSL
153400960402     C* LINEA DI ARRIVO SEMPRE OBBLIGATORIA
153500960402     C* (Sia in key per spedizione che in key per segnacollo)
153600960402    2C     V1SLNA        IFEQ      *ZEROS
153700960402     C                   MOVEL     MSG(7)        V1ZMSG
153800960402     C                   SETON                                        4528
153900960402     C                   GOTO      ENDCTR
154000960402    2C                   END
154100960401     C**** DATA APERTURA ANOMALIA
154200960401    1C     V1SDAO        IFEQ      *ZEROS
154300960401     C                   MOVEL     MSG(9)        V1ZMSG
154400960401     C                   SETON                                        4628
154500960401     C                   GOTO      ENDCTR
154600960401    1C                   END
154700990513     C                   CLEAR                   WLBDA8
154800990513     C                   MOVE      V1SDAO        G08DAT
154900990513     C                   CALL      'XSRDA8'
155000990513     C                   PARM                    WLBDA8
155100990513    1C     G08ERR        IFEQ      '1'
155200960401     C                   MOVEL     MSG(9)        V1ZMSG
155300960401     C                   SETON                                        4628
155400960401     C                   GOTO      ENDCTR
155500960401   X1C                   ELSE
155600990513     C                   MOVE      G08INV        V1HDAO
155700960401    1C                   END
155800960401     C**** CONTROLLI DI CHIAVE DOPPIA
155900960403     C                   CLEAR                   WFLAG
156000960830     C                   CLEAR                   SAVFLS
156100960830     C                   CLEAR                   SAVSCN
156200960404     C     V1SCAA        IFEQ      '070'
156300960404     C     V1SCAA        OREQ      '071'
156400960404     C                   CLEAR                   V1SLID
156500960404     C                   END
156600960401     C                   MOVE      V1SCAA        WCAA
156700960401    1C                   SELECT
156800960402     C     V1HKEY        WHENEQ    'B'
156900990513     C     V1SAAS        IFGT      60
157000990513     C                   MOVEL     19            WAAS
157100990513     C                   ELSE
157200990513     C                   MOVEL     20            WAAS
157300990513     C                   END
157400990513     C                   MOVE      V1SAAS        WAAS
157500990512     C     KANM1         SETGT     FNANM01L
157600990512     C     KANM1         READPE    FNANM01L                               30
157700960401    2C     *IN30         DOWEQ     *OFF
157800960401    3C     V1HNRR        IFNE      ANMNR1
157900960401     C                   EXSR      CTRDOP
158000960401     C   28              GOTO      ENDCTR
158100960401    3C                   END
158200960404     C* SOLO PER ANOMALIE 070-ECCEDE BAM E 071-MANCA BAM: MEMORIZZO IL
158300960404     C* LIVELLO IDD DELL'ANOMALIA PIù RECENTE.
158400960830     C* MEMORIZZO ANCHE FILIALE E NUMERO SEGNAC. > 0 PER POI
158500960830     C* CONFRONTARLI CON QUELLI IMMESSI EVENTUALMENTE A VIDEO
158600960404     C     V1SCAA        IFEQ      '070'
158700960404     C     V1SCAA        OREQ      '071'
158800960404     C     WFLAG         IFEQ      *BLANKS
158900960506     C     ANMFLE        ANDEQ     D34FGS
159000960404     C                   MOVE      ANMLID        V1SLID
159100960404     C                   MOVE      '1'           WFLAG             1
159200960404     C                   END
159300960830     C     ANMFLS        IFGT      *ZEROS
159400960830     C                   MOVE      ANMFLS        SAVFLS
159500960830     C                   MOVE      ANMSCN        SAVSCN
159600960830     C                   END
159700960404     C                   END
159800990512     C     KANM1         READPE    FNANM01L                               30
159900960401    2C                   ENDDO
160000960401     C*
160100960402     C     V1HKEY        WHENEQ    'S'
160200990512     C     KANM2         SETGT     FNANM02L
160300990512     C     KANM2         READPE    FNANM02L                               30
160400960401    2C     *IN30         DOWEQ     *OFF
160500960401    3C     V1HNRR        IFNE      ANMNR2
160600960401     C                   EXSR      CTRDOP
160700960401     C   28              GOTO      ENDCTR
160800960401    3C                   END
160900990512     C     KANM2         READPE    FNANM02L                               30
161000960401    2C                   ENDDO
161100960401    1C                   ENDSL
161200960405     C********************
161300960405     C* SOLO IN MANUTENZIONE :
161400960405     C*********************
161500960403     C* VERIFICO SE OCCORRE IMPOSTARE NUOVO LIVELLO IDD
161600960403    1C     *IN10         IFEQ      *OFF
161700960403    2C     WCAA          IFNE      V1HCAA
161800960403    3C     §7CIDD        IFEQ      'S'
161900960404     C                   MOVE      §7CLID        V1SLID
162000960506     C                   ELSE
162100960506     C                   CLEAR                   V1SLID
162200960403    3C                   END
162300960403   X2C                   ELSE
162400960404     C     V1SLID        IFEQ      *BLANKS
162500960404     C                   MOVE      V1HLIO        V1SLID
162600960404     C                   END
162700960403    2C                   END
162800960403   X1C                   ELSE
162900960405     C********************
163000960405     C* SOLO IN IMMISSIONE:
163100960405     C********************
163200960405     C* VERIFICO SE OCCORRE IMPOSTARE NUOVO LIVELLO IDD
163300960403    2C     V1SLID        IFEQ      *BLANKS
163400960403     C     §7CIDD        ANDEQ     'S'
163500960403     C                   MOVE      §7CLID        V1SLID
163600960403    2C                   END
163700960405     C* Controllo cha non sia già stato inserito un record con la
163800960409     C* stessa chiave
163900960405    2C                   SELECT
164000960405     C     V1HKEY        WHENEQ    'B'
164100960405     C                   MOVE      V1SAAS        WSAAS
164200960405     C                   MOVE      V1SLNP        WSLNP
164300960405     C                   MOVE      V1SNRS        WSNRB
164400960405     C                   MOVE      V1SNSP        WSNSP
164500960405     C                   MOVE      V1SCAA        WSCAB
164600960405     C     WKEYB         LOOKUP    SKB                                    30
164700960405    3C     *IN30         IFEQ      *ON
164800960405     C                   MOVEL     MSG(26)       V1ZMSG
164900960405     C                   SETON                                        284443
165000960405     C                   SETON                                        494140
165100960405    3C                   END
165200960405     C                   ADD       1             II                3 0
165300960405     C                   MOVE      WKEYB         SKB(II)
165400960405     C     V1HKEY        WHENEQ    'S'
165500960405     C                   MOVE      V1SFLS        WSFLS
165600960405     C                   MOVE      V1SLNA        WSLNA
165700960405     C                   MOVE      V1SNRS        WSNRS
165800960405     C                   MOVE      V1SSCN        WSSCN
165900960405     C                   MOVE      V1SCAA        WSCAS
166000960405     C     WKEYS         LOOKUP    SKS                                    30
166100960405    3C     *IN30         IFEQ      *ON
166200960405     C                   MOVEL     MSG(26)       V1ZMSG
166300960405     C                   SETON                                        284845
166400960405     C                   SETON                                        494240
166500960405    3C                   END
166600960405     C                   ADD       1             LL                3 0
166700960405     C                   MOVE      WKEYS         SKS(LL)
166800960405    2C                   ENDSL
166900960405     C   28              GOTO      ENDCTR
167000960403    1C                   END
167100960402     C* CONTROLLO COMPLETEZZA CHIAVE FACOLTATIVA, SE IMMESSA
167200960402    1C                   SELECT
167300960402     C     V1HKEY        WHENEQ    'B'
167400960402     C* Immessa filiale segnacollo e non il numero
167500960402    2C     V1SFLS        IFGT      *ZEROS
167600960402     C     V1SSCN        ANDEQ     *ZEROS
167700960403     C                   SETON                                        2842
167800960402     C                   MOVEL     MSG(19)       V1ZMSG
167900960402   X2C                   ELSE
168000960402     C* Immesso il numero segnacollo e non la filiale
168100960402    3C     V1SFLS        IFEQ      *ZEROS
168200960402     C     V1SSCN        ANDGT     *ZEROS
168300960403     C                   SETON                                        2848
168400960402     C                   MOVEL     MSG(21)       V1ZMSG
168500960402    3C                   END
168600960402    2C                   END
168700960402     C     V1HKEY        WHENEQ    'S'
168800960402    2C     KEYNSP        IFGT      *ZEROS
168900960402    3C     V1SNSP        IFEQ      *ZEROS
169000960403     C                   SETON                                        2841
169100960402     C                   MOVEL     MSG(25)       V1ZMSG
169200960402   X3C                   ELSE
169300000225     C     V1SLNP        IFEQ      *ZEROS
169400960403     C                   SETON                                        284344
169500960402     C                   MOVEL     MSG(24)       V1ZMSG
169600960402    4C                   END
169700960402    3C                   END
169800960402    2C                   END
169900960402    1C                   ENDSL
170000960402     C   28              GOTO      ENDCTR
170100960409     C*
170200960409     C******************************
170300960409     C* Solo per anomalie 070 e 071:
170400960409     C******************************
170500960409    1C     V1SCAA        IFEQ      '070'
170600960409     C     V1SCAA        OREQ      '071'
170700960830     C* Errore se immessi filiale/numero segnacollo diversi da quelli
170800960830     C* di un'altra anomalia con la stessa chiave (questo controllo
170900960830     C* serve per ricevere correttamente le anomalie visto che il
171000960830     C* programma di ricezione se fls/scn>0 legge le anomalie con key
171100960830     C* segnacollo)
171200960830     C     V1SFLS        IFGT      *ZEROS
171300960830     C     SAVFLS        ANDGT     *ZEROS
171400960830     C     V1SFLS        IFNE      SAVFLS
171500960830     C     V1SSCN        ORNE      SAVSCN
171600960830     C                   MOVEA     MSG(37)       WMS
171700960830     C                   MOVE      SAVFLS        W003A             3
171800960830     C                   MOVEA     W003A         WMS(67)
171900960830     C                   MOVE      SAVSCN        W007A             7
172000960830     C                   MOVEA     W007A         WMS(71)
172100960830     C                   MOVEA     WMS           V1ZMSG
172200960830     C                   SETON                                        284248
172300960830     C                   GOTO      ENDCTR
172400960830     C                   END
172500960830     C                   END
172600960830     C*
172700960409    2C     V1SCAA        IFEQ      '070'
172800960409     C                   MOVE      071           WSCAB
172900960409   X2C                   ELSE
173000960409     C                   MOVE      070           WSCAB
173100960409    2C                   END
173200960409     C                   MOVE      V1SAAS        WSAAS
173300960409     C                   MOVE      V1SLNP        WSLNP
173400960409     C                   MOVE      V1SNRS        WSNRB
173500960409     C                   MOVE      V1SNSP        WSNSP
173600960409     C************
173700960409     C* Immissione
173800960409     C************
173900960409    2C     *IN10         IFEQ      *ON
174000960506     C* Controllo che per la stessa bolla non sia stato immesso sia il
174100960506     C* codice 070 che 071
174200960409     C     WKEYB         LOOKUP    SKB                                    30
174300960409    3C     *IN30         IFEQ      *ON
174400960409     C                   MOVEL     MSG(27)       V1ZMSG
174500960409     C                   SETON                                        284443
174600960409     C                   SETON                                        4941
174700960409     C                   GOTO      ENDCTR
174800960409    3C                   END
174900960409    2C                   END
175000960830     C*****************************
175100960830     C* Manutenzione o inserimento:
175200960830     C*****************************
175300960409    2C     *IN10         IFEQ      *OFF
175400960409     C     WCAA          ANDNE     V1HCAA
175500960409     C     *IN10         OREQ      *ON
175600960506     C                   CLEAR                   WNTRM
175700960506     C                   CLEAR                   WTRM
175800960506     C                   CLEAR                   WNMIA
175900960506     C                   CLEAR                   WFLE
176000960506     C                   MOVE      WSCAB         WCAA
176100990513     C     V1SAAS        IFGT      60
176200990513     C                   MOVEL     19            WAAS
176300990513     C                   ELSE
176400990513     C                   MOVEL     20            WAAS
176500990513     C                   END
176600990513     C                   MOVE      V1SAAS        WAAS
176700990512     C     KANM1         SETLL     FNANM01L
176800990512     C     KANM1         READE     FNANM01L                               30
176900960506    3C     *IN30         DOWEQ     *OFF
177000960506     C     WNTRM         ANDEQ     *BLANKS
177100960409     C* Se in manutenzione scarto me stesso
177200960506    4C     *IN10         IFEQ      *OFF
177300960409     C     V1HNRR        ANDEQ     ANMNR1
177400960506     C                   GOTO      LEGGI
177500960506    4C                   END
177600960506     C* Anomalia non mia
177700960506    4C     ANMFLE        IFNE      D34FGS
177800960506     C                   MOVE      '1'           WNMIA             1
177900960506     C                   MOVE      ANMFLE        WFLE
178000960506   X4C                   ELSE
178100960506    5C     ANMFT1        IFEQ      *BLANKS
178200960506     C* Mia anomalia non trasmessa
178300960506     C                   MOVE      '1'           WNTRM             1
178400960506   x5C                   ELSE
178500960506    6C     ANMFT1        IFNE      *BLANKS
178600960506     C* Mia anomalia trasmessa
178700960506     C                   MOVE      '1'           WTRM              1
178800960506    6C                   END
178900960506    5C                   END
179000960506    4C                   END
179100960506     C     LEGGI         TAG
179200990512     C     KANM1         READE     FNANM01L                               30
179300960506    3C                   ENDDO
179400970507     C**
179500970507     C* SE E' MODIFICA LA SPEDIZIONE CONTROLLO SE C'E' IN ARB
179600970507     C     WKEYB         IFNE      V1HKSP
179700970507     C                   CLEAR                   V1HARB
179800970507     C                   ENDIF
179900970507     C**
180000960506    3C     WNTRM         IFEQ      '1'
180100960506     C     WTRM          OREQ      '1'
180200960506     C     WNMIA         OREQ      '1'
180300960506     C* Decodifico codice anomalia
180400960506     C                   MOVE      *BLANKS       WDES             24
180500960506     C                   MOVE      *BLANKS       WDES13           13
180600960409     C                   MOVEL     '7C'          KCOD
180700960409     C                   MOVEL(P)  ANMCAA        KKEY
180800960409     C     KTAB          CHAIN     TABEL00F                           30
180900960409     C  N30              MOVEL     TBLUNI        WDES
181000960506     C  N30              MOVEL     TBLUNI        WDES13
181100960506     C* Mia anomalia non trasmessa: errore bloccante
181200960506    4C     WNTRM         IFEQ      '1'
181300960506     C                   SETON                                        284443
181400960506     C                   SETON                                        4941
181500960506     C                   MOVEA     MSG(29)       WMS
181600960506     C                   MOVE      ANMCAA        W003A             3
181700960506     C                   MOVEA     W003A         WMS(38)
181800960506     C                   MOVEA     WDES          WMS(42)
181900960506     C                   MOVEA     WMS           V1ZMSG
182000960506     C                   GOTO      ENDCTR
182100960506   x4C                   ELSE
182200960506     C*
182300960506     C* Mia anomalia già trasmessa o anomalia non mia: errore forzabile
182400960506    5C                   SELECT
182500960506     C     WTRM          WHENEQ    '1'
182600960506     C     WKEYB         IFNE      V1HKSP
182700960506     C                   SETON                                        284443
182800960506     C                   SETON                                        4941
182900960506     C                   MOVEA     MSG(28)       WMS
183000960506     C                   MOVE      ANMCAA        W003A             3
183100960506     C                   MOVEA     W003A         WMS(38)
183200960506     C                   MOVEA     WDES          WMS(42)
183300960506     C                   MOVEA     WMS           V1ZMSG
183400960506     C                   GOTO      ENDCTR
183500960506     C                   END
183600960506     C     WNMIA         WHENEQ    '1'
183700960506     C     WKEYB         IFNE      V1HKSP
183800960506     C                   SETON                                        284443
183900960506     C                   SETON                                        4941
184000960506     C                   MOVEA     MSG(35)       WMS
184100960409     C                   MOVE      ANMCAA        W003A             3
184200960506     C                   MOVEA     W003A         WMS(34)
184300960506     C                   MOVEA     WDES13        WMS(38)
184400960506     C                   MOVE      WFLE          W003A
184500960506     C                   MOVEA     W003A         WMS(63)
184600960409     C                   MOVEA     WMS           V1ZMSG
184700960409     C                   GOTO      ENDCTR
184800960506     C                   END
184900960506    5C                   ENDSL
185000960506    4C                   END
185100960506    3C                   END
185200970507     C*
185300970507     C* IN MODIFICA: SE CAMBIATO SOLO IL NUMERO SPEDIZIONE RICONTROLLO
185400970507     C*  SU FNARB
185500970507   X2C                   ELSE
185600970507     C**
185700970507     C* SE E' MODIFICA LA SPEDIZIONE CONTROLLO SE C'E' IN ARB
185800970507     C     WKEYB         IFNE      V1HKSP
185900970507     C                   CLEAR                   V1HARB
186000970507     C                   ENDIF
186100960506    2C                   END
186200960506    1C                   END
186300970507     C*****
186400970507     C* SOLO PER ANOMALIA MANCA DDT
186500970507     C*****
186600970507     C* SE MODIFICATA LA SPEDIZIONE RICONTROLLO
186700970507    1C     V1SCAA        IFEQ      '071'
186800970507     C* VERIFICO SE C'E' LA BOLLA--> MESSAGGIO FORZABILE
186900970507     C     V1HARB        ANDEQ     ' '
187000990513     C     V1SAAS        IFGT      60
187100970507     C                   MOVEL     19            WAAS
187200970507     C                   ELSE
187300970507     C                   MOVEL     20            WAAS
187400970507     C                   END
187500970507     C                   MOVE      V1SAAS        WAAS
187600970507     C*
187700970507     C     KBLP          SETLL     FNARB01L                               30
187800970507     C*
187900970507    2C     *IN30         IFEQ      *OFF
188000970507     C                   MOVEL     'S'           V1HARB
188100970507     C                   SETON                                        284443
188200970507     C                   SETON                                        4941
188300970507     C                   MOVEA     MSG(39)       V1ZMSG
188400970507     C                   GOTO      ENDCTR
188500970507   X2C                   ELSE
188600970507     C* CONTROLLO SE E' UN NO DDT --> ERRORE NON FORZABILE
188700060216     C     KAR4          CHAIN     FIAR401L                           30
188800970507    3C     *IN30         IFEQ      *OFF
188900031028     c                   Movel     Ar4not        dsbl4a
189000031028     c                   Move      §b4abm        wddt              1
189100970507     C*
189200970507    4C     WDDT          IFNE      'S'
189300970507     C     WDDT          ANDNE     'C'
189400031028     c     wddt          andne     'P'
189500031028     c     wddt          andne     'Q'
189600970507     C                   SETON                                        284443
189700970507     C                   SETON                                        4941
189800970507     C                   MOVEA     MSG(40)       V1ZMSG
189900970507     C                   GOTO      ENDCTR
190000970507     C*
190100970507   X4C                   ELSE
190200970507     C                   MOVEL     'S'           V1HARB
190300970507    4C                   ENDIF
190400970507     C*
190500970507   X3C                   ELSE
190600970507     C                   MOVEL     'S'           V1HARB
190700970507    3C                   ENDIF
190800970507    2C                   ENDIF
190900970507    1C                   ENDIF
191000970507     C*
191100960401     C* Memorizzo CAMPI per DUP
191200960401     C                   MOVE      V1SCAA        DUPCAA
191300960401     C                   MOVE      V1SAAS        DUPAAS
191400960401     C                   MOVE      V1SLNP        DUPLNP
191500960401     C                   MOVE      V1SNRS        DUPNRS
191600960401     C                   MOVE      V1SNSP        DUPNSP
191700960401     C                   MOVE      V1SFLS        DUPFLS
191800960401     C                   MOVE      V1SSCN        DUPSCN
191900960401     C                   MOVE      V1SLNA        DUPLNA
192000960401     C                   MOVE      V1SDAO        DUPDAO
192100960401     C* MESSAGGI DI AVVISO:
192200971118     C* Avviso se data apertura anomalia > di data del giorno
192300990513    1C     V1HDAO        IFGT      DATEU8
192400960401     C     V1HDAO        ANDNE     V1HPRE
192500960401     C                   SETON                                        4628
192600960401     C                   MOVEL     MSG(23)       V1ZMSG
192700960409     C                   MOVE      V1HDAO        V1HPRE
192800960401    1C                   END
192900960409     C     ENDCTR        TAG
193000960410    1C     V1SCAA        IFEQ      '070'
193100960410     C     V1SCAA        OREQ      '071'
193200960410     C     WKEYB         IFNE      V1HKSP
193300960410     C                   MOVE      WKEYB         V1HKSP
193400960410     C                   END
193500960410     C                   END
193600960409     C                   ENDSR
193700960401     C*************  CONTROLLI DI CHIAVE DOPPIA ***********************
193800960401     C     CTRDOP        BEGSR
193900960418     C*
194000960418    1C     V1SCAA        IFEQ      '012'
194100960418     C                   SETON                                        444341
194200960418     C                   SETON                                        404928
194300960418     C                   MOVEL     MSG(32)       V1ZMSG
194400960418     C                   GOTO      ENDDOP
194500960418    1C                   END
194600960418     C*
194700960401    1C     ANMDCH        IFEQ      *ZEROS
194800960405     C     V1HDAO        ORLE      ANMDAO
194900960403     C     ANMLID        ORGE      WLCI
195000960401    2C                   SELECT
195100960402     C     V1HKEY        WHENEQ    'B'
195200960401     C                   SETON                                        444341
195300960402     C     V1HKEY        WHENEQ    'S'
195400960403     C                   SETON                                        4842
195500960401    2C                   ENDSL
195600960403    2C                   SELECT
195700960403     C* Errore di anomalia già aperta
195800960403     C     ANMDCH        WHENEQ    *ZEROS
195900960401     C                   MOVEL     MSG(18)       V1ZMSG
196000960403     C* errore di esistenza anomalia con data apertura più recente
196100960403     C     V1HDAO        WHENLT    ANMDAO
196200960401     C                   MOVEL     MSG(20)       V1ZMSG
196300960403     C                   SETON                                        46
196400960403     C* Errore se anomalia già CHIUSA COME IDD
196500960403     C     ANMLID        WHENGE    WLCI
196600960401     C                   MOVEL     MSG(22)       V1ZMSG
196700960403    2C                   ENDSL
196800960403     C*
196900960403     C                   SETON                                        28
197000960403     C                   SETON                                        404549
197100960403     C                   GOTO      ENDDOP
197200960403    1C                   END
197300960401     C     ENDDOP        ENDSR
197400960429     C****** CONTROLLI PARTICOLARI/DI ANNULLABILITA'/RIPR. ************
197500960429     C     CTRANN        BEGSR
197600960429     C                   CLEAR                   WFLG01
197700960429     C                   SETOFF                                       30
197800960429     C                   SELECT
197900960429     C     V1HKEY        WHENEQ    'B'
198000990513     C* Imposto anno a 4 cifre
198100041124     C     V1SAAS        IFGT      80
198200990513     C                   MOVEL     19            WAAS
198300990513     C                   ELSE
198400990513     C                   MOVEL     20            WAAS
198500990513     C                   END
198600990513     C                   MOVE      V1SAAS        WAAS
198700960429     C     KBLP          SETLL     FNBLP01L                               30
198800960429     C     V1HKEY        WHENEQ    'S'
198900960429     C     KBLT          SETLL     FNBLT27L                               30
199000040615     C**N30V1SCAA        IFEQ      '005'
199100040615     C***  KBLT          SETLL     FNBTT27L                               30
199200040615     C***                END
199300960429     C                   ENDSL
199400960429     C   30              MOVE      'E'           WFLG01            1
199500960429     C                   ENDSR
199600960402     C***********  INTERROGAZIONE DI UN RECORD SINGOLO  ***************
199700960402     C     SRINT         BEGSR
199800990512     C     V1HNRR        CHAIN(N)  FNANM00F                           30
199900960605    1C     *IN30         IFEQ      *OFF
200000960403     C     ANMATB        COMP      *BLANKS                            0606
200100960403     C                   MOVE      ANMAAS        V2CAAS
200200960403     C                   MOVE      ANMLNP        V2CLNP
200300960605     C                   MOVE      ANMNSP        V2CNSP
200400960605    2C     ANMNSP        IFGT      *ZEROS
200500960403     C                   MOVE      ANMNRS        V2CSRB
200600960403     C                   MOVE      ANMLNA        V2CLAB
200700960605   X2C                   ELSE
200800960605     C                   CLEAR                   V2CSRB
200900960605     C                   CLEAR                   V2CLAB
201000960605     C* Cerco spedizione per poterla visualizzare
201100960605    3C     ANMLNA        IFGT      *ZEROS
201200040531     C     KBLT2         CHAIN     FNBLT27L                           31
201300960605    4C     *IN31         IFEQ      *OFF
201400960605     C                   MOVE      D27AAS        V2CAAS
201500960605     C                   MOVE      D27LNP        V2CLNP
201600960605     C                   MOVE      D27NRS        V2CSRB
201700960605     C                   MOVE      D27NSP        V2CNSP
201800960605     C                   MOVE      D27LNA        V2CLAB
201900040531     c* se non impostata zona, la imposto
202000040531     c                   if        anmfl4=000  or anmfl4=999
202100040531     c     kblp27        chain     fnblp01l
202200040531     c                   if        %found(fnblp01l)
202300040531     c                   z-add     blpznc        anmfl4
202400040531     c                   endif
202500040531     c                   endif
202600960605    4C                   END
202700960605    3C                   END
202800960605    2C                   END
202900960605     C*
203000960403     C                   MOVE      ANMFLS        V2CFLS
203100960605    2C     ANMSCN        IFGT      *ZEROS
203200960403     C                   MOVE      ANMLNA        V2CLAS
203300960403     C                   MOVE      ANMNRS        V2CSRS
203400960605   X2C                   ELSE
203500960403     C                   CLEAR                   V2CLAS
203600960403     C                   CLEAR                   V2CSRS
203700960605    2C                   END
203800960403     C                   MOVE      ANMSCN        V2CSCN
203900960403     C                   MOVE      ANMNPS        V2CNPS
204000040531     C* VISLUALIZZO SE COLLO DI VALORE
204100040531     C                   SETOFF                                       93
204200040531     C                   IF        ANMFT4='V'
204300040531     C                   SETON                                        93
204400040531     C                   ENDIF
204500040531     C
204600960403     C* DECODIFICO TIPO ANOMALIA
204700960403     C                   MOVEL     '7F'          KCOD
204800960403     C                   MOVEL(P)  ANMTAN        KKEY
204900960403     C     KTAB          CHAIN     TABEL00F                           30
205000960403     C  N30              MOVEL     TBLUNI        V2DTAN
205100960403     C   30              CLEAR                   V2DTAN
205200960403     C* DECODIFICO SOGGETTO ANOMALIA
205300960403     C                   MOVEL     '7G'          KCOD
205400960403     C                   MOVEL(P)  ANMSAN        KKEY
205500960403     C     KTAB          CHAIN     TABEL00F                           30
205600960403     C  N30              MOVEL     TBLUNI        V2DSAN
205700960403     C   30              CLEAR                   V2DSAN
205800960506     C* DECODIFICO CODICE ANOMALIA
205900960403     C                   MOVE      ANMCAA        V2CCAA
206000960403     C                   MOVEL     '7C'          KCOD
206100960403     C                   MOVEL(P)  ANMCAA        KKEY
206200960403     C     KTAB          CHAIN     TABEL00F                           30
206300960403     C  N30              MOVEL     TBLUNI        DS7C
206400960403     C   30              CLEAR                   DS7C
206500960403     C                   MOVEL     §7CDES        V2DCAA
206600960507     C* INTERNA/I.D.D./NON I.D.D.
206700960507     C                   CLEAR                   W001A
206800960507     C     ANMAIE        IFEQ      'I'
206900961118     C     ANMAIE        OREQ      'X'
207000960507     C                   MOVE      ANMAIE        W001A
207100960507     C                   ELSE
207200960507     C     §7CIDD        IFEQ      'S'
207300960507     C                   MOVE      'D'           W001A             1
207400960507     C                   ELSE
207500960507     C                   MOVE      'N'           W001A
207600960507     C                   END
207700960507     C                   END
207800960507     C     W001A         LOOKUP    TAB           TABDES                   30
207900960507     C                   MOVEL     TABDES        V2DAIE
208000960507     C*
208100960403     C     §7CKEY        COMP      'B'                                080807
208200960403     C*
208300960403     C     §7CIDD        IFEQ      'S'
208400960403     C                   MOVE      ANMLID        V2CLID
208500960403     C                   ELSE
208600960403     C                   CLEAR                   V2CLID
208700960403     C                   END
208800960403     C* APERTURA:
208900960403     C* - decodifico fase
209000960403     C                   MOVEL     '7D'          KCOD
209100960403     C                   MOVEL(P)  ANMFSA        KKEY
209200960403     C     KTAB          CHAIN     TABEL00F                           30
209300960403     C  N30              MOVEL     TBLUNI        DS7D
209400960403     C   30              CLEAR                   DS7D
209500960411     C                   MOVEL     §7DDES        W2DFSA
209600960411     C                   MOVEL     ANMFSA        W2CFSA
209700960411     C                   MOVE      '-'           W2CFSA
209800960403     C*
209900990513     C                   CLEAR                   WLBDA8
210000990513     C                   MOVE      ANMDAO        G08INV
210100990513     C                   MOVEL     '3'           G08ERR
210200990513     C                   CALL      'XSRDA8'
210300990513     C                   PARM                    WLBDA8
210400990513     C                   MOVE      G08DAT        V2CDAO
210500960403     C                   MOVE      ANMCDU        V2CCDU
210600960403     C                   MOVE      ANMNFV        V2CNFV
210700960403     C* CHIUSURA:
210800960403     C* - decodifico fase
210900960405     C                   CLEAR                   V2DFSC
211000960405     C     ANMFSC        IFNE      *BLANKS
211100960403     C                   MOVEL     '7D'          KCOD
211200960403     C                   MOVEL(P)  ANMFSC        KKEY
211300960403     C     KTAB          CHAIN     TABEL00F                           30
211400960403     C  N30              MOVEL     TBLUNI        DS7D
211500960403     C   30              CLEAR                   DS7D
211600960411     C                   MOVEL     §7DDES        W2DFSC
211700960411     C                   MOVEL     ANMFSC        W2CFSC
211800960411     C                   MOVE      '-'           W2CFSC
211900960405     C                   END
212000960403     C*
212100990513     C                   CLEAR                   WLBDA8
212200990513     C                   MOVE      ANMDCH        G08INV
212300990513     C                   MOVEL     '3'           G08ERR
212400990513     C                   CALL      'XSRDA8'
212500990513     C                   PARM                    WLBDA8
212600990513     C                   MOVE      G08DAT        V2CDCH
212700960403     C* - decodifico causale
212800960405     C                   CLEAR                   V2DCCH
212900960405     C     ANMCCH        IFNE      *BLANKS
213000970522     C     ANMCCH        ANDNE     'RA'
213100960405     C                   MOVEL     ANMCCH        W2DCAA
213200960405     C                   MOVE      '-'           W2DCAA
213300960403     C                   MOVEL     '7E'          KCOD
213400960403     C                   MOVEL(P)  ANMCCH        KKEY
213500960403     C     KTAB          CHAIN     TABEL00F                           30
213600960405     C  N30              MOVEL     TBLUNI        W2DDES
213700960405     C                   END
213800040531     C* imposto la zona da anmfl4
213900000516     C                   SETOFF                                       01
214000040531     c                   if        anmfl4<>999
214100040531     c                   move      anmfl4        v2cznc
214200040531     c                   seton                                        01
214300040531     c                   endif
214400040531     c
214500040531     c* Data e ora di creazione anomalia
214600040531     c                   clear                   v2cdac
214700040531     c                   clear                   v2corc
214800040531     c                   if        anmflo<>*blanks
214900040531     c                   movel     anmflo        danmflo
215000040531     c                   if        §anmdac>0
215100040531     C                   CLEAR                   WLBDA8
215200040531     C                   MOVE      §anmdac       G08INV
215300040531     C                   MOVEL     '3'           G08ERR
215400040531     C                   CALL      'XSRDA8'
215500040531     C                   PARM                    WLBDA8
215600040531     C                   MOVE      G08DAT        V2Cdac
215700040531     c                   endif
215800040531     c                   z-add     §anmorc       v2corc
215900040531     c                   endif
216000040531     c
216100040531     c* cerco la zona dalla bolla, sel atrovo
216200040531     C**                 MOVEL     ANMFLO        DANM01
216300040531     C**   §ANZNC        IFNE      *BLANKS
216400040531     C**                 SETON                                        01
216500040531     C**                 MOVEL     §ANZNC        V2CZNC
216600040531     C**                 ENDIF
216700960403     C*
216800960403     C                   MOVE      ANMFLE        V2CFLE
216900960403     C                   MOVE      ANMFL1        V2CFL1
217000960403     C                   MOVE      ANMFL2        V2CFL2
217100960411     C                   CLEAR                   V2DFT1
217200960411     C                   CLEAR                   V2DFT2
217300000904     C* SOLO SE SONO DIVERSI LI DECODIFICO UNO PER UNO
217400000904    2C     ANMFT1        IFNE      ANMFT2
217500000904    3C     ANMFT1        IFEQ      'T'
217600960411     C                   MOVEL     'Trm'         V2DFT1
217700960605     C                   ELSE
217800960605     C     ANMFT1        IFEQ      'R'
217900960605     C                   MOVEL     'Ric'         V2DFT1
218000960411     C                   END
218100000904    3C                   END
218200000904    3C     ANMFT2        IFEQ      'T'
218300960411     C                   MOVEL     'Trm'         V2DFT2
218400960605     C                   ELSE
218500960605     C     ANMFT2        IFEQ      'R'
218600960605     C                   MOVEL     'Ric'         V2DFT2
218700960605     C                   END
218800000904    3C                   END
218900000904    2C                   END
219000000904     C*
219100000904     C                   CLEAR                   V2DDTR
219200000904     C                   CLEAR                   V2CDTR
219300000824     C*
219400000904     C**         ANMDTR    IFNE 0
219500000904     C**         ANMFT1    ANDNE' '
219600000904     C**         ANMDTR    ORNE 0
219700000904     C**         ANMAIE    ANDEQ'T'
219800000831     C*
219900000904     C     ANMFT1        IFEQ      'R'
220000000904     C                   MOVEL     WRIC          V2DDTR
220100000904     C                   ELSE
220200000904     C* O E' GIA' TRASMESSO O SFLAGGATO IN ATTESA DI TRASMISSIONE
220300000904     C                   MOVEL     WTRA          V2DDTR
220400000824     C                   ENDIF
220500000904     C*
220600000904    2C     ANMDTR        IFGT      0
220700000904    3C     ANMFT1        IFNE      ' '
220800000904     C     ANMAIE        OREQ      'T'
220900000824     C                   MOVEL     ANMDTR        WDT04A            4
221000000824     C                   MOVEL     ANMDTR        WDT06X            6
221100000824     C                   MOVE      WDT06X        WDT02M            2
221200000824     C                   MOVE      ANMDTR        WDT02G            2
221300000824     C                   MOVE      WDT10D        V2CDTR
221400000904    3C                   ENDIF
221500000904    2C                   ENDIF
221600000904     C**                   ELSE
221700000904     C**                   CLEARV2DDTR
221800000904     C**                   CLEARV2CDTR
221900000904     C**                   ENDIF
222000960403     C*****************
222100960403     C* EMISSIONE VIDEO
222200960403     C*****************
222300960403     C                   WRITE     LR44D01
222400960403     C                   EXFMT     LR44D02
222500960403     C*
222600960605    1C                   END
222700960402     C                   ENDSR
222800960328     C***********  AGGIORNAMENTO FILE ANOMALIE  ***********************
222900960328     C     AGGANM        BEGSR
223000960329     C                   READC     LR44S01                                29
223100960405    1C     *IN29         DOWEQ     *OFF
223200960419     C                   SETOFF                                       47
223300960412    2C     V1SVAR        IFNE      *BLANKS
223400960412     C     V1HVAR        ORNE      *BLANKS
223500990512     C     V1HNRR        CHAIN     FNANM00F                           30
223600960405    3C     *IN30         IFEQ      *OFF
223700041125     C**                 MOVE      *BLANKS       WALC              1
223800960405    4C                   SELECT
223900960419     C* GESTIONE ANNULLAMENTO ANOMALIA
224000960412     C     V1SVAR        WHENEQ    'A'
224100960514     C*
224200041124    5C**   V1HAPA        IFEQ      'P'
224300041124     C**   ANMAIE        ANDNE     'I'
224400041124     C**                 EXSR      SRCAEX
224500041124    5C**                 END
224600960514     C****
224700960514     C* Se chiudo anomalia 105-Manca record bolla partenza chiudo anche
224800960514     C* le anomalie 5 o 10 e viceversa
224900960514     C*
225000041125    5C     ANMCAA        IFEQ      5
225100960514     C     ANMCAA        OREQ      10
225200960514     C     ANMCAA        OREQ      105
225300960514     C                   EXSR      CHI105
225400041125    5C                   END
225500960514     C***
225600960419     C* Solo se riuscita l'allocazione del file di trasmissione:
225700041125    5C**   WALC          IFEQ      *BLANKS
225800041125     C**   WALC          OREQ      'S'
225900960419     C                   CLEAR                   DSLR33
226000960419     C* N.B.:Richiamo pgm chiusura anomalia senza sflaggare ft1/ft2
226100960419     C*      dando per scontato che le anomalie arrivo possono essere
226200960419     C*      annullate solo se non ancora trasmesse. Provvederò invece
226300960419     C*      alla scrittura del file di trasmissione a fronte di
226400960419     C*      annull/ripr di anomalie partenza
226500041125     c* 25/11/04: non inviamo più le anomalie ma subito annullate o ripristi
226600041125     c*           nate tutte . non importa quindi lo stesso sflaggare
226700960419     C                   MOVE      'N'           D33FTR
226800960419     C                   Z-ADD     V1HNRR        D33NRR
226900960419     C                   MOVE      'AN'          D33CCH
227000960419     C                   MOVE      'M'           D33FSC
227100990513     C                   Z-ADD     DATEU8        D33DCH
227200960514     C                   MOVEL     'S'           D33MAC
227300990512     C                   UNLOCK    FNANM00F
227400960419     C                   CALL      'FNLR33R'
227500960419     C                   PARM                    DSLR33
227600990512     C     V1HNRR        CHAIN(N)  FNANM00F                           30
227700960419     C   30              MOVE      'A'           V1SATB
227800960419     C  N30              EXSR      RIECHI
227900960419     C* Scrivo file di trasmissione
228000041125    6C**   WALC          IFEQ      'S'
228100041125     C**                 EXSR      TRMANM
228200041125    6C**                 END
228300041125    5C**                 END
228400041124     c
228500041124     c
228600960419     C* GESTIONE RIPRISTINO ANOMALIA
228700960412     C     V1SVAR        WHENEQ    'R'
228800041125    5C**   V1HAPA        IFEQ      'P'
228900041125     C**   ANMAIE        ANDNE     'I'
229000041125     C**                 EXSR      SRCAEX
229100041125    5C**                 END
229200960513     C* Se ripristino anomalia 105-Manca record bolla partenza
229300960513     C* ripristino anche le anomalie 5 o 10 e viceversa
229400041125     C**   WALC          IFEQ      'S'
229500041125     C**   WALC          OREQ      *BLANKS
229600960513     C     ANMCAA        IFEQ      5
229700960513     C     ANMCAA        OREQ      10
229800960513     C     ANMCAA        OREQ      105
229900960513     C                   EXSR      RIP105
230000960513     C                   END
230100041125     C**                 END
230200960419     C* Solo se riuscita l'allocazione del file di trasmissione:
230300041125    5C**   WALC          IFEQ      *BLANKS
230400041125     C**   WALC          OREQ      'S'
230500960419    6C     V1SATB        IFEQ      'A'
230600960419     C                   MOVE      *BLANKS       V1SATB
230700960329     C                   MOVE      *BLANKS       ANMATB
230800960419     C                   ELSE
230900960426     C* Clearo campi se anomalia non ha raggiunto il livello max
231000960508     C* N.B.:Se v1slid = blanks il codice operativo move sposta 0
231100960508     C*      in w0010 per cui non ci sono problemi
231200960507     C                   MOVE      V1SLID        W0010             1 0
231300960426     C     W0010         IFLT      WLCI
231400960419     C                   CLEAR                   ANMFSC
231500960419     C                   CLEAR                   ANMDCH
231600960419     C                   CLEAR                   ANMCCH
231700960419     C                   CLEAR                   V1DFSC
231800960426     C                   CLEAR                   V1SDCH
231900960419     C                   CLEAR                   V1SCCH
232000960426     C                   ELSE
232100960426     C* Altrimenti imposto chiusura in pratica Idd
232200960426     C                   MOVEL     'PR'          ANMCCH
232300990513     C                   MOVE      DATEU8        ANMDCH
232400960426     C                   MOVEL     'D'           ANMFSC
232500971118     C                   MOVE      DTGIO6        V1SDCH
232600960426     C                   MOVEL     'PR'          V1SCCH
232700960426     C                   MOVEL     '7D'          KCOD
232800960426     C                   MOVEL(P)  'D'           KKEY
232900960426     C     KTAB          CHAIN     TABEL00F                           30
233000960426     C  N30              MOVEL     TBLUNI        DS7D
233100960426     C  N30              MOVEL     §7DDES        V1DFSC
233200960426     C   30              CLEAR                   V1DFSC
233300960426     C                   END
233400960419    6C                   END
233500041125     C**   WALC          IFEQ      'S'
233600041125     C     WFEL1         IFGT      *ZEROS
233700960605     C     WFEL1         ANDNE     SIMFEL
233800960605     C                   MOVEL     'T'           ANMFT1
233900000824     C                   MOVE      DATEU8        ANMDTR
234000960605     C                   END
234100960605     C     WFEL2         IFGT      *ZEROS
234200960605     C     WFEL2         ANDNE     SIMFEL
234300960605     C     WFEL2         ANDNE     WFEL1
234400960605     C                   MOVEL     'T'           ANMFT2
234500000824     C                   MOVE      DATEU8        ANMDTR
234600960605     C                   END
234700041125     C**                 END
234800990512     C                   UPDATE    FNANM000
234900041125    6C**   WALC          IFEQ      'S'
235000041125     C**                 EXSR      TRMANM
235100041125    6C**                 END
235200041125    5C**                 END
235300960419     C* GESTIONE VARIAZIONE ANOMALIA
235400960412     C     V1HVAR        WHENEQ    'V'
235500960329     C                   EXSR      RISANM
235600990512     C                   UPDATE    FNANM000
235700960405    4C                   ENDSL
235800960405    3C                   END
235900960405    2C                   END
236000960402     C                   SETON                                        04
236100960405     C                   SETON                                        05
236200960405     C                   SETOFF                                       09
236300960402     C                   MOVE      *IN04         V1HIN4
236400960402     C                   MOVE      *IN05         V1HIN5
236500960405     C                   MOVE      *IN09         V1HIN9
236600041125     C                   CLEAR                   V1SVAR
236700041125     c
236800960412     C                   CLEAR                   V1HVAR
236900960523     C                   MOVE      V1HI51        *IN51
237000960523     C                   MOVE      V1HI52        *IN52
237100960329     C                   UPDATE    LR44S01
237200960329     C                   READC     LR44S01                                29
237300960405    1C                   ENDDO
237400000224     C*
237500960329     C                   ENDSR
237600960328     C***************  SCRITTURA FILE ANOMALIE   **********************
237700960328     C     WTRANM        BEGSR
237800960329     C                   READC     LR44S01                                29
237900960329     C     *IN29         DOWEQ     *OFF
238000960402     C     V1SCAA        IFNE      *BLANKS
238100960405     C     V1SCAA        ANDNE     *ZEROS
238200960402     C                   MOVE      V1SCAA        WCAA
238300990512     C                   CLEAR                   FNANM000
238400960329     C                   EXSR      RISANM
238500990512     C                   WRITE     FNANM000
238600960329     C                   END
238700960329     C                   READC     LR44S01                                29
238800960329     C                   ENDDO
238900960329     C   10              MOVEL     'S'           D34RIL
239000960328     C                   ENDSR
239100960329     C********  RIEMPIMENTO CAMPI FILE ANOMALIE  **********************
239200960329     C     RISANM        BEGSR
239300960329     C                   MOVE      V1SNSP        ANMNSP
239400960329     C                   MOVE      V1SSCN        ANMSCN
239500960401     C                   MOVE      V1SFLS        ANMFLS
239600960329     C                   MOVE      V1SLNP        ANMLNP
239700990524     C                   MOVE      V1SAAS        ANMAAS
239800000225     C     ANMNSP        IFGT      0
239900990513     C     V1SAAS        IFGT      60
240000990513     C                   MOVEL     19            ANMAAS
240100990513     C                   ELSE
240200990513     C                   MOVEL     20            ANMAAS
240300990513     C                   END
240400990524     C                   END
240500960329     C                   MOVE      V1SLNA        ANMLNA
240600960329     C                   MOVE      V1SNRS        ANMNRS
240700960329     C                   MOVEL     V1HTAN        ANMTAN
240800960329     C                   MOVEL     V1HSAN        ANMSAN
240900960329     C                   MOVE      V1SCAA        ANMCAA
241000960329     C                   MOVEL     V1HFSA        ANMFSA
241100960329     C                   MOVE      V1HDAO        ANMDAO
241200960507     C                   MOVEL     V1HAIE        ANMAIE
241300960329     C                   MOVE      V1SLID        ANMLID
241400960411     C                   MOVE      D34FGS        ANMFLE
241500960404     C     V1SLNP        IFGT      *ZEROS
241600960404     C                   MOVE      V1SLNP        ANMFL1
241700960404     C                   ELSE
241800960404     C                   MOVE      V1SFLS        ANMFL1
241900960404     C                   END
242000960404     C                   CLEAR                   ANMFT1
242100960404     C                   CLEAR                   ANMFT2
242200000824     C                   CLEAR                   ANMDTR
242300140523     c* imposto in anmflo data e ora creazione spunta
242400140523     C                   TIME                    TIMEanm          14 0
242500140523     c                   movel     timeanm       wora4             4 0
242600140523     C                   move      timeanm       G08DAT
242700140523     C                   Z-ADD     *ZERO         G08INV
242800140523     C                   MOVEL     *BLANKS       G08ERR
242900140523     C                   CALL      'XSRDA8'
243000140523     C                   PARM                    WLBDA8
243100140523     C                   move      G08INV        WDAT6             6 0
243200140523     c                   movel     wdat6         anmflo
243300140523     c                   move      wora4         anmflo
243400960329     C                   ENDSR
243500960513     C***  CHIUSURA ANOMALIA 5 O 10 PER ANOMALIA 105  E VICEVERSA  ****
243600960513     C     CHI105        BEGSR
243700041124     C*
243800041124     c* anm 105 - annullo la 10 o le 05
243900041124     c* anm  05 - annullo la 105 e altre 05
244000041124     c* anm  10 - annullo la 105
244100041124     c
244200041125     C                   eval      wcaaelab=anmcaa
244300041124     c                   eval      giri=1
244400041124     c
244500960513     C                   EXSR      LEGANM
244600041125     C
244700041125    0c                   dow       giri<=2
244800960513     C*
244900041124     C**                 MOVE      *BLANKS       WALC1             1
245000041124    1C     *IN30         doweq     *OFF
245100051006     c* Per le anom 05 escluse se stesso
245200051006     c                   if        wcaa=wcaaelab and anmnr2<>v1hnrr
245300041124     c*
245400041124    2C**   V1HAPA        IFEQ      'P'
245500041124     C**   ANMAIE        ANDNE     'I'
245600041124     C**                 EXSR      SRCAEX
245700041124     C**                 MOVE      WALC          WALC1
245800041124    2C**                 END
245900041124    2C**   WALC1         IFEQ      'S'
246000041124     C**   WALC1         OREQ      *BLANKS
246100041124     C*
246200960513     C                   CLEAR                   DSLR33
246300960513     C                   MOVE      'N'           D33FTR
246400960513     C                   Z-ADD     ANMNR2        D33NRR
246500960513     C                   MOVE      'AN'          D33CCH
246600960513     C                   MOVE      'M'           D33FSC
246700990513     C                   Z-ADD     DATEU8        D33DCH
246800960514     C* N.B.: in questo caso l'impostazione del flag d33mac è di poca
246900960514     C*       importanza visto che le anomalie di questa routine non
247000960514     C*       sono autochiuse. Lo imposto solo per renderlo uguale
247100960514     C*       all'altro richiamo di fnlr33r
247200960514     C                   MOVEL     'S'           D33MAC
247300960513     C                   CALL      'FNLR33R'
247400960513     C                   PARM                    DSLR33
247500960513     C* Scrivo file di trasmissione
247600041124    3C**   WALC1         IFEQ      'S'
247700041124     C**   ANMNR2        CHAIN(N)  FNANM00F                           30
247800041124     C**                 EXSR      TRMANM
247900041124     C**                 CLEAR                   WALC
248000041124    3C**                 END
248100041124    2C**                 END
248200051006     c                   endif
248300041124    c
248400041124     c
248500041124     c* Verifico se ci sono altre anomalie stesso segnacollo stesso
248600041124     c*  cod.anomalia da chiudere (sono poi solo le 005)
248700041124     C     KANM2         reade     FNANM02L                               30
248800041124    1C                   ENDdo
248900041124     c                   eval      giri=giri+1
249000041124     c
249100041124     c* Per anom 10  e 105 esco
249200041124    1c                   if        wcaaelab<>05
249300041124     c                   eval      giri=3
249400041124     c                   else
249500041124     c* Per anomalia 05 leggo altre 05 se presenti, escludo record in
249600041124     c*  annullamento
249700041124     C                   Z-ADD     05            WCAA
249800041124     C     KANM2         CHAIN     FNANM02L                           30
249900041124    2c                   dow       not *in30 and anmnr2=v1hnrr
250000041124     C     KANM2         reade     FNANM02L                               30
250100041124    2c                   enddo
250200041124     c
250300041124    1c                   endif
250400041124    0c                   enddo
250500960513     C                   ENDSR
250600960513     C***  RIPRISTINO ANOMALIA 5 O 15 PER ANOMALIA 105 E VICEVERSA ****
250700960513     C     RIP105        BEGSR
250800041124     c* anm 105 - annullo la 10 o le 05
250900041124     c* anm  05 - annullo la 105 e altre 05
251000041124     c* anm  10 - annullo la 105
251100041124     c
251200041124     c                   eval      wcaaelab=anmcaa
251300041124     c                   eval      giri=1
251400041124     c
251500960513     C                   EXSR      LEGANM
251600041125     c
251700041125    0c                   dow       giri<=2
251800960513     C*
251900041125     C***                MOVE      *BLANKS       WALC1             1
252000041125    1C     *IN30         doweq     *OFF
252100960513     C*
252200041124    2C**   V1HAPA        IFEQ      'P'
252300041124     C**   ANMAIE        ANDNE     'I'
252400041124     C**                 EXSR      SRCAEX
252500041124     C**                 MOVE      WALC          WALC1
252600041124    2C**                 END
252700960513     C*
252800041124    2C**   WALC1         IFEQ      'S'
252900041124     C**   WALC1         OREQ      *BLANKS
253000990512     C     ANMNR2        CHAIN     FNANM00F                           30
253100041125    3C     *IN30         IFEQ      *OFF
253200041125    4C     ANMLID        IFLT      WLCI
253300960513     C                   CLEAR                   ANMFSC
253400960513     C                   CLEAR                   ANMDCH
253500960513     C                   CLEAR                   ANMCCH
253600960513     C                   ELSE
253700960513     C* Altrimenti imposto chiusura in pratica Idd
253800960513     C                   MOVEL     'PR'          ANMCCH
253900990513     C                   MOVE      DATEU8        ANMDCH
254000960513     C                   MOVEL     'D'           ANMFSC
254100041125    4C                   END
254200041125    4C**   WALC1         IFEQ      'S'
254300041125    5C     WFEL1         IFGT      *ZEROS
254400960605     C                   MOVEL     'T'           ANMFT1
254500000824     C                   MOVE      DATEU8        ANMDTR
254600041125    5C                   END
254700041125    5C     WFEL2         IFGT      *ZEROS
254800960605     C     WFEL2         ANDNE     WFEL1
254900960605     C                   MOVEL     'T'           ANMFT2
255000000824     C                   MOVE      DATEU8        ANMDTR
255100041125    5C                   END
255200041125    4C**                 END
255300990512     C                   UPDATE    FNANM000
255400041125    3C                   END
255500041125    2C**                 END
255600041125     c
255700041125     c* Verifico se ci sono altre anomalie stesso segnacollo stesso
255800041125     c*  cod.anomalia da chiudere (sono poi solo le 005)
255900041125     C     KANM2         reade     FNANM02L                               30
256000041125    1C                   ENDdo
256100041125     c                   eval      giri=giri+1
256200041125     c
256300041125     c* Per anom 10  e 105 esco
256400041125    1c                   if        wcaaelab<>05
256500041125     c                   eval      giri=3
256600041125     c                   else
256700041125     c* Per anomalia 05 leggo altre 05 se presenti, escludo record in
256800041125     c*  annullamento
256900041125     C                   Z-ADD     05            WCAA
257000041125     C     KANM2         CHAIN     FNANM02L                           30
257100041125    2c                   dow       not *in30 and anmnr2=v1hnrr
257200041125     C     KANM2         reade     FNANM02L                               30
257300041125    2c                   enddo
257400041125     c
257500041125    1c                   endif
257600041125    0c                   enddo
257700960513     C*
257800041125     C**   WALC1         IFEQ      'S'
257900041125     C**                 EXSR      TRMANM
258000041125     C**                 CLEAR                   WALC
258100041125     C**                 END
258200960513     C* RICHAINO RECORD SCELTO A VIDEO
258300041125     C**   WALC1         IFNE      'N'
258400990512     C     V1HNRR        CHAIN     FNANM00F                           30
258500041125     C**                 END
258600960513     C*
258700041125    1C**                 END
258800960513     C                   ENDSR
258900960513     C**************  LETTURA ANOMALIA 5/10/105   *********************
259000960513     C     LEGANM        BEGSR
259100960513    1C                   SELECT
259200960513     C     ANMCAA        WHENEQ    105
259300960514     C                   Z-ADD     10            WCAA
259400990512     C     KANM2         CHAIN     FNANM02L                           30
259500960514     C   30              Z-ADD     5             WCAA
259600990512     C   30KANM2         CHAIN     FNANM02L                           30
259700960513     C     ANMCAA        WHENEQ    5
259800960513     C     ANMCAA        OREQ      10
259900960513     C                   Z-ADD     105           WCAA
260000990512     C     KANM2         CHAIN     FNANM02L                           30
260100960513    1C                   ENDSL
260200960513     C                   ENDSR
260300970214     C**********  GESTIONE VIDEATA MANUTENZIONE ANOMALIA 300  *********
260400970214     C     WDW04         BEGSR
260500970214     C* La causale anomalia la imposto sempre = a 300
260600970214     C                   Z-ADD     300           W4SCAA
260700970214     C                   MOVEL     '7C'          KCOD
260800970214     C                   MOVEL(P)  300           KKEY
260900970214     C     KTAB          CHAIN     TABEL00F                           30
261000970214     C  N30              MOVEL     TBLUNI        DS7C
261100970214     C   30              CLEAR                   DS7C
261200970214     C                   MOVEL     §7CDES        W4DCAA
261300970214     C* Spengo indicatori del video
261400970214     C                   SETOFF                                       282555
261500970217     C                   SETOFF                                       5654
261600970214     C* 10 OFF --> MANUTENZIONE
261700970214     C     *IN10         IFEQ      *OFF
261800970214     C* Imposto la lnp
261900970214     C                   MOVE      ANMLNP        W4SLNP
262000970214     C* Imposto il numero dei ritiri
262100970214     C                   MOVE      ANMSCN        W4SNRT
262200970214     C* imposto la data anomalia
262300990513     C                   CLEAR                   WLBDA8
262400990513     C                   MOVE      ANMDAO        G08INV
262500990513     C                   MOVEL     '3'           G08ERR
262600990513     C                   CALL      'XSRDA8'
262700990513     C                   PARM                    WLBDA8
262800990513     C                   MOVE      G08DAT        W4SDAO
262900970214     C                   ELSE
263000970214     C* 10 ON --> INSERIMENTO
263100970214     C                   MOVE      D34FGS        W4SLNP
263200990513     C                   Z-ADD     WDTGIO        W4SDAO
263300970214     C                   MOVE      *ZEROS        W4SNRT
263400970214     C                   END
263500970214     C**
263600970214     C* EMISSIONE WINDOW
263700970214     C**
263800970214     C     EMWDW4        TAG
263900970214     C                   EXFMT     LR44W04
264000970214     C                   SETOFF                                       28
264100970214     C     *INKL         CABEQ     *ON           EWDW04
264200970214     C                   EXSR      CTRWDW
264300970214     C   28
264400970214     CORNKF              GOTO      EMWDW4
264500970214     C* CONFERMO DATI
264600970214     C     *IN10         IFEQ      *OFF
264700970214     C                   EXSR      AGG300
264800970214     C                   ELSE
264900970214     C                   EXSR      WTR300
265000970214     C                   END
265100970217     C*
265200970217     C     EWDW04        TAG
265300970217     C                   MOVE      *BLANKS       V1SVAR
265400970214     C*
265500970217     C                   ENDSR
265600970214     C**********  CONTROLLI WINDOW MANUTENZIONE ANOMALIA 300  *********
265700970214     C     CTRWDW        BEGSR
265800970214     C                   SETOFF                                       545556
265900970214     C*
266000970214    1C     *IN10         IFEQ      *ON
266100970214     C* LNP NON IMMESSA
266200970214    2C     W4SLNP        IFEQ      *ZEROS
266300970214     C                   SETON                                        5428
266400970214     C                   MOVEL     MSG(3)        W4ZMSG
266500970214     C                   GOTO      ENDCTW
266600970214    2C                   END
266700970214     C* LA LNP DEVE ESSERE UNA FILIALE IN GESTIONE
266800970214    2C     W4SLNP        IFNE      D34FGS
266900970214     C     W4SLNP        LOOKUP    L6S                                    30
267000970214    3C     *IN30         IFEQ      *OFF
267100970214     C                   MOVEL     MSG(8)        W4ZMSG
267200970214     C                   SETON                                        5428
267300970214     C                   GOTO      ENDCTW
267400970214    3C                   END
267500970214    2C                   END
267600970214    1C                   END
267700970214     C*
267800970214     C* ERRORE SE NON IMMESSO IL NUMERO RITIRI EFFETTUATI
267900970304    1C*          W4SNRT    IFEQ *ZEROS
268000970304     C*                    MOVELMSG,38    W4ZMSG
268100970304     C*                    SETON                     5528
268200970304     C*                    GOTO ENDCTW
268300970304    1C*                    END
268400970214     C*
268500970214    1C     *IN10         IFEQ      *ON
268600970214     C* ERRORE SE DATA ANOMALIA = 0
268700970214    2C     W4SDAO        IFEQ      *ZEROS
268800970214     C                   MOVEL     MSG(9)        W4ZMSG
268900970214     C                   SETON                                        5628
269000970214     C                   GOTO      ENDCTW
269100970214    2C                   END
269200970214     C* CONTROLLO FORMALE DATA ANOMALIA
269300990513     C                   CLEAR                   WLBDA8
269400990513     C                   Z-ADD     W4SDAO        G08DAT
269500990513     C                   CALL      'XSRDA8'
269600990513     C                   PARM                    WLBDA8
269700990513    2C     G08ERR        IFEQ      '1'
269800970214     C                   MOVE      *ZEROS        W4SINV
269900970214     C                   MOVEL     MSG(9)        W4ZMSG
270000970214     C                   SETON                                        5628
270100970214     C                   GOTO      ENDCTW
270200970214     C                   ELSE
270300990513     C                   MOVE      G08INV        W4SINV
270400990513     C                   MOVE      G08DAT        W4SDAO
270500970214    2C                   END
270600970214     C* ERRORE SE CHIAVE GIA' ESISTENTE
270700990513     C     KANM4         SETLL     FNANM04L
270800990512     C     KANM4         READE     FNANM04L                               30
270900970214     C     *IN30         DOWEQ     *OFF
271000970214     C     ANMLNP        IFEQ      W4SLNP
271100970214     C                   MOVEL     MSG(32)       W4ZMSG
271200970214     C                   SETON                                        2854
271300970214     C                   GOTO      ENDCTW
271400970214     C                   END
271500990512     C     KANM4         READE     FNANM04L                               30
271600970214     C                   ENDDO
271700970214    1C                   END
271800970214     C     ENDCTW        ENDSR
271900970214     C**************  AGGIORNAMENTO ANOMALIA 300 **********************
272000970214     C     AGG300        BEGSR
272100990512     C     V1HNRR        CHAIN     FNANM00F                           30
272200970214     C     *IN30         IFEQ      *OFF
272300970214     C                   Z-ADD     W4SNRT        ANMSCN
272400990512     C                   UPDATE    FNANM000
272500970214     C                   Z-ADD     W4SNRT        V1SSCN
272600970214     C                   END
272700970214     C                   ENDSR
272800970214     C**************  SCRITTURA ANOMALIA 300     **********************
272900970214     C     WTR300        BEGSR
273000990512     C                   CLEAR                   FNANM000
273100970214     C                   Z-ADD     W4SLNP        ANMLNP
273200970214     C                   Z-ADD     W4SNRT        ANMSCN
273300970214     C                   Z-ADD     W4SINV        ANMDAO
273400970214     C                   Z-ADD     W4SINV        ANMDCH
273500970214     C                   Z-ADD     300           ANMCAA
273600970214     C                   MOVE      D34FGS        ANMFLE
273700970214     C                   MOVE      §7CTAN        ANMTAN
273800970214     C                   MOVE      §7CSAN        ANMSAN
273900970214     C                   MOVE      §7CAIE        ANMAIE
274000970214     C                   MOVEL     '1'           ANMFSA
274100970214     C                   MOVEL     '1'           ANMFSC
274200140523     c* imposto in anmflo data e ora creazione spunta
274300140523     C                   TIME                    TIMEanm          14 0
274400140523     c                   movel     timeanm       wora4             4 0
274500140523     C                   move      timeanm       G08DAT
274600140523     C                   Z-ADD     *ZERO         G08INV
274700140523     C                   MOVEL     *BLANKS       G08ERR
274800140523     C                   CALL      'XSRDA8'
274900140523     C                   PARM                    WLBDA8
275000140523     C                   move      G08INV        WDAT6             6 0
275100140523     c                   movel     wdat6         anmflo
275200140523     c                   move      wora4         anmflo
275300990512     C                   WRITE     FNANM000
275400970214     C                   ENDSR
275500960326     C**************  OPERAZIONI INIZIALI  ****************************
275600960326     C     *INZSR        BEGSR
275700960326     C*
275800960326     C                   Z-ADD     1             CODUT
275900960326     C                   CALL      'X§PARUT'
276000960326     C                   PARM                    UT§DSE
276100960326     C                   MOVEL     RAGUT         V1DRSU
276200960329     C                   MOVEL     REC80         CNCR80
276300960327     C*
276400960327     C     *LIKE         DEFINE    NRR1          NRRAL
276500960327     C     *LIKE         DEFINE    NRR1          NRRSAV
276600960327     C     *LIKE         DEFINE    V1CNBR        W1CNBR
276700960328     C     *LIKE         DEFINE    TBLCOD        KCOD
276800960328     C     *LIKE         DEFINE    TBLKEY        KKEY
276900960329     C     *LIKE         DEFINE    D34FGS        SAVFGS
277000960329     C     *LIKE         DEFINE    V1SCAA        DUPCAA
277100960329     C     *LIKE         DEFINE    V1SAAS        DUPAAS
277200960401     C     *LIKE         DEFINE    V1SLNP        DUPLNP
277300960401     C     *LIKE         DEFINE    V1SNRS        DUPNRS
277400960401     C     *LIKE         DEFINE    V1SNSP        DUPNSP
277500960401     C     *LIKE         DEFINE    V1SFLS        DUPFLS
277600960401     C     *LIKE         DEFINE    V1SSCN        DUPSCN
277700960401     C     *LIKE         DEFINE    V1SDAO        DUPDAO
277800960401     C     *LIKE         DEFINE    V1SLNA        DUPLNA
277900971118     C     *LIKE         DEFINE    G08INV        DATEU8
278000960401     C     *LIKE         DEFINE    ANMCAA        WCAA
278100960506     C     *LIKE         DEFINE    ANMFLE        WFLE
278200960402     C     *LIKE         DEFINE    PA1AAS        WAAS
278300960403     C     *LIKE         DEFINE    §7ALCI        WLCI
278400971118     C     *LIKE         DEFINE    D55TFP        WFEL1
278500971118     C     *LIKE         DEFINE    D55TFP        WFEL2
278600041125     C***  *LIKE         DEFINE    D55TFP        SAVOVR
278700960521     C     *LIKE         DEFINE    D44FIL        W44FIL
278800960521     C     *LIKE         DEFINE    D44LNA        W44LNA
278900960521     C     *LIKE         DEFINE    D44NRS        W44NRS
279000960521     C     *LIKE         DEFINE    D44SCN        W44SCN
279100960605     C     *LIKE         DEFINE    ANMLNP        WLNP
279200960605     C     *LIKE         DEFINE    ANMNRS        WNRS
279300960605     C     *LIKE         DEFINE    ANMNSP        WNSP
279400960830     C     *LIKE         DEFINE    ANMFLS        SAVFLS
279500960830     C     *LIKE         DEFINE    ANMSCN        SAVSCN
279600961210     C     *LIKE         DEFINE    DSANM         DSANMX
279700990513     C     *LIKE         DEFINE    W4SDAO        W4SINV
279800970507     C     *LIKE         DEFINE    AR4TRC        WTRC
279900970507     C                   MOVEL     'A'           WTRC
280000960328     C*
280100040531     C     KTAB          klist
280200960328     C                   KFLD                    CODUT
280300960328     C                   KFLD                    KCOD
280400960328     C                   KFLD                    KKEY
280500960429     C*
280600960429     C     KBLP          KLIST
280700960429     C                   KFLD                    WAAS
280800960429     C                   KFLD                    V1SLNP
280900960429     C                   KFLD                    V1SNRS
281000960429     C                   KFLD                    V1SNSP
281100970507     C     KAR4          KLIST
281200970507     C                   KFLD                    WAAS
281300970507     C                   KFLD                    V1SLNP
281400970507     C                   KFLD                    V1SNRS
281500970507     C                   KFLD                    V1SNSP
281600970507     C                   KFLD                    WTRC
281700960605     C*
281800960605     C     KBLP2         KLIST
281900960605     C                   KFLD                    WAAS
282000960605     C                   KFLD                    WLNP
282100960605     C                   KFLD                    WNRS
282200960605     C                   KFLD                    WNSP
282300960429     C*
282400960429     C     KBLT          KLIST
282500960429     C                   KFLD                    V1SFLS
282600960429     C                   KFLD                    V1SLNA
282700960429     C                   KFLD                    V1SNRS
282800960429     C                   KFLD                    V1SSCN
282900960605     C*
283000960605     C     KBLT2         KLIST
283100960605     C                   KFLD                    ANMFLS
283200960605     C                   KFLD                    ANMLNA
283300960605     C                   KFLD                    ANMNRS
283400960605     C                   KFLD                    ANMSCN
283500960401     C*
283600960401     C     KANM1         KLIST
283700990513     C                   KFLD                    WAAS
283800960401     C                   KFLD                    V1SLNP
283900960401     C                   KFLD                    V1SNRS
284000960401     C                   KFLD                    V1SNSP
284100960401     C                   KFLD                    WCAA
284200960401     C*
284300960401     C     KANM2         KLIST
284400960401     C                   KFLD                    V1SFLS
284500960401     C                   KFLD                    V1SLNA
284600960401     C                   KFLD                    V1SNRS
284700960401     C                   KFLD                    V1SSCN
284800960401     C                   KFLD                    WCAA
284900970214     C*
285000970214     C     KANM4         KLIST
285100970214     C                   KFLD                    W4SCAA
285200970214     C                   KFLD                    W4SINV
285300960521     C*
285400960521     C     KANM01        KLIST
285500990513     C                   KFLD                    D44AAS
285600960521     C                   KFLD                    D44FIL
285700960521     C                   KFLD                    D44NRS
285800960521     C                   KFLD                    D44NSP
285900040531     C     Kblp27        KLIST
286000040531     C                   KFLD                    D27AAS
286100040531     C                   KFLD                    D27lnp
286200040531     C                   KFLD                    D27NRS
286300040531     C                   KFLD                    D27NSP
286400960522     C*
286500960522     C     KART01        KLIST
286600960522     C                   KFLD                    D44AAS
286700960522     C                   KFLD                    D44FIL
286800960522     C                   KFLD                    D44NRS
286900960522     C                   KFLD                    D44NSP
287000960521     C*
287100960521     C     KANM02        KLIST
287200960521     C                   KFLD                    W44FIL
287300960521     C                   KFLD                    W44LNA
287400960521     C                   KFLD                    W44NRS
287500960521     C                   KFLD                    W44SCN
287600960326     C*
287700971118     C* DETERMINO DATA DEL GIORNO
287800971118     C*
287900971118     C                   TIME                    W0140            14 0
288000971118     C                   MOVE      W0140         WDTGIO            8 0
288100971118     C                   CLEAR                   WLBDA8
288200971118     C                   Z-ADD     WDTGIO        G08DAT
288300971118     C                   CALL      'XSRDA8'
288400971118     C                   PARM                    WLBDA8
288500971118     C                   MOVE      G08INV        DATEU8
288600971118     C                   MOVEL     WDTGIO        DTGIO6            6 0
288700971118     C                   MOVE      WDTGIO        W0020             2 0
288800971118     C                   MOVE      W0020         DTGIO6
288900960401     C*
289000960329     C                   CLEAR                   SAVFGS
289100960327     C                   EXSR      PULSFL
289200960403     C*  Reperisco livello di chiusura anomalia come IDD
289300960403     C                   MOVEL     '7A'          KCOD
289400960403     C                   MOVEL(P)  '2'           KKEY
289500960403     C     KTAB          CHAIN     TABEL00F                           30
289600960403     C  N30              MOVEL     TBLUNI        DS7A2
289700960403     C   30              CLEAR                   DS7A2
289800960403     C                   MOVE      §7ALCI        WLCI
289900960423     C*
290000021008     C                   MOVE      ')'           VAR               8
290100021008     C                   MOVE      '))'          VAR2              9
290200960521     C*
290300960521     C                   SETOFF                                       9192
290400960523     C                   SETOFF                                       5152
290500960605     C                   CLEAR                   WOPNAR
290600960605     C                   CLEAR                   WOPNBL
290700040615     C***                CLEAR                   WOPNBT
290800000824     C                   MOVE      '/'           WTRAT1
290900000824     C                   MOVE      '/'           WTRAT2
291000960327     C*
291100990513     C                   ENDSR
291200990512** MSG
291300960326Fine scorrimento                                                              01
291400960506Immettere Codice Anomalia valido                                              02
291500960401Immettere Linea di Partenza valida                                            03
291600970507Immettere linea  Segnacollo valida                                            04
291700960506Non è ammesso l'inserimento di anomalie con questo codice                     05
291800960401Immettere Numero Spedizione                                                   06
291900960401Immettere Linea di Arrivo valida                                              07
292000970507Immettere un punto operativo in gestione                                      08
292100960401Immettere Data Apertura anomalia valida                                       09
292200000104                                                                              10
292300960401Non ammessa la manutenzione o anomalia già trasmessa                          11
292400960329Anomalia non annullabile                                                      12
292500960329Anomalia già annullata                                                        13
292600960329Anomalia non ripristinabile                                                   14
292700960329Anomalia già ripristinata                                                     15
292800960329Anomalia non manutenzionabile                                                 16
292900960401Immettere Numero Segnacollo                                                   17
293000960401Presente anomalia ancora aperta                                               18
293100970507Se immessa la linea segnacollo immettere anche il numero                      19
293200960401Anomalia già aperta con data > alla data apertura immessa                     20
293300970507Se immesso il numero segnacollo immettere anche la linea                      21
293400960403Anomalia già chiusa in fase IDD                                               22
293500960402Data anomalia > di data odierna. F6 per forzare.                              23
293600000225Se immesso il numero spedizione immettere anche la linea di partenza          24
293700960402Se immesso l'anno e la linea di partenza immettere anche il n.spedizione      25
293800960405Non è possibile inserire più di una volta la stessa anomalia                  26
293900960409Non è possibile,per la stessa bolla,inserire sia anomalia 070 che 071.        27
294000960506Per questa bolla esiste già anomalia XXX-123456789012345678901234 F6 per forz.28
294100960506Per questa bolla esiste già anomalia XXX-123456789012345678901234             29
294200960412Questa anomalia non verrà variata in quanto trasmessa/chiusa.F6 per continuare30
294300960412Anomalia non più esistente                                                    31
294400960418Anomalia già esistente                                                        32
294500960419Alcune anomalie non aggiornate: attendere 1 minuto e riprovare                33
294600960429Impossibile annullare: presente spedizione                                    34
294700960506Per la bolla esiste già anomalia XXX-1234567890123 immessa da xxx.F6 per forz.35
294800970507Immettere linea di arrivo diversa da punto operativo in gestione              36
294900960830Per questa bolla e causale anomalia immettere Fil./N.Segnacollo = xxx/xxxxxxx 37
295000970214Immettere il numero dei ritiri effettuati                                     38
295100970507La spedizione non esiste nell'archivio bolle arrivi: F6 per forzare           39
295200970507Impossibile inserire un MANCA DDT per una spedizione che e' NO DDT
295300960328** Z01
295400960422F6=CONFERMA F10=Inserimento F24=AlTasti
295500960403F7=Bolla F14=Spunte F3=Fine F24=AlTasti
295600960422F12=Ritorno F24=AlTasti
295700960403** Z02
295800960403F7=Bolla F14=Spunte F3=Fine F12=Ritorno
295900960409F3=Fine  F10=Inserimento  F12=Ritorno
296000960403F6=CONFERMA F3=Fine F12=Ritorno
296100960521F3=Fine  F12=Ritorno
296200960521F14=Spunte F3=Fine F12=Ritorno
296300960521F7=Bolla  F3=Fine F12=Ritorno
296400960422** Z03
296500960422F6=CONFERMA F7=Bolla F3=Fine F24=AlTasti
296600960422F14=Spunte F12=Ritorno F24=AlTasti
296700960403** TAB/TABDES
296800960403IINTERNA
296900960507DI.D.D.
297000960507NNON IDD
297100961118XDISCOR.EXP
