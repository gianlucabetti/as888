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
002800151116      * clienti/filiali con sospensione bolle
002900151116     ffibsp01l  if   e           k disk
003000151116     ffibsp02l  if   e           k disk    rename(fibsp000:fibsp02)
003100960327     FFNLR44D   CF   E             WORKSTN
003200960326     F                                     SFILE(LR44S01:NRR1)
003300960401     F*
003400160203     D MSG             S             78    DIM(46) CTDATA PERRCD(1)             MSG VIDEO
003500960403     D Z01             S             40    DIM(3) CTDATA PERRCD(1)              PIEDI VIDEO
003600960521     D Z02             S             40    DIM(6) CTDATA PERRCD(1)              PIEDI VIDEO
003700960422     D Z03             S             40    DIM(2) CTDATA PERRCD(1)              PIEDI VIDEO
003800960329     D L6S             S              3  0 DIM(30)                              FIL GEST.DA AL
003900960523     D L1              S              3  0 DIM(30)                              FIL.GESTITE
004000151116     D est             S              3  0 DIM(30)                              FIL.GESTITE
004100961118     D TAB             S              1    DIM(4) CTDATA PERRCD(1)              INT/IDD/NONIDD
004200961118     D TABDES          S             10    DIM(4) ALT(TAB)
004300960405     D SKB             S             17    DIM(500)                             PER CONTR.
004400960405     D SKS             S             18    DIM(500)                             PER CONTR.
004500960405     D WMS             S              1    DIM(78)                              COMODO X MSG
004600151116     D i               S              3  0
004700151116     D in              S              3  0
004800151116     d w_clibo         s                   like(blpccm)
004900151116     D w_SiBsp         S              1
005000960521     D*
005100960521     D DSLR44        E DS                  EXTNAME(FNLR44DS)
005200960402     D DSLS05          DS
005300960402     D  PA0AAS                 1      4  0
005400960402     D  PA0LNP                 5      7  0
005500960402     D  PA0NRS                 8      9  0
005600960402     D  PA0NSP                10     16  0
005700960402     D  PA0F03                17     17
005800960402     D  PA0FLG                18     18
005900960402     D  PA0A9                 19     19
006000960402     D  PA0B5                 20     20
006100960402     D  PA075                 21     21
006200960402     D  PA098                 22     22
006300960402     D  PA0RSU                23     42
006400960402     D  PA0CML                43     43
006500960402     D  PA0SIM                44     46  0
006600960402      * PA0CLI indica se il pgm e' richiamato da un cliente (='S')
006700960402     D  PA0CLI                47     47
006800960402      * PA0GIA indica se il pgm e' richiamato dalle giacenze
006900960402     D  PA0GIA                48     48
007000960403     D DSLR04          DS                  INZ
007100960403     D  PA4NFV                 1      5  0
007200960403     D  PA4LNP                 6      8  0
007300960403     D  PA4LNA                 9     11  0
007400960403     D  PA4NRS                12     13  0
007500960403     D  PA4NCD                14     20  0
007600960403     D  PA4NCA                21     27  0
007700960403     D  PA4NPS                28     29  0
007800960403     D  PA4NPG                30     30  0
007900960403     D  PA4FLG                31     31
008000960403     D  PA4PGM                32     38
008100960403     D  PA4FGS                39     41  0
008200101203     d* bolla
008300101203     d  pa4bolp               42     44  0
008400101203     d  pa4bors               45     46  0
008500101203     d  pa4bosp               47     53  0
008600101203     d  pa4boan               54     57  0
008700101203     D  Pa4ERR                58     58
008800101203     d
008900960402     D DSLR36          DS
009000960402     D  PA1AAS                14     17  0
009100960402     D  PA1LNP                18     20  0
009200960402     D  PA1NRS                21     22  0
009300960402     D  PA1NSP                23     29  0
009400960402     D  PA1F03                30     30
009500960402     D  PA1FLG                31     31
009600960402     D  PA1PGM                32     41
009700960402     D  PA1DSA                42     61
009800960402     D  PA1CML                62     62
009900960402     D  PA1DSP                63     70  0
010000960402     D  PA1RMN                71     85  0
010100960402     D  PA1LNA                86     88  0
010200960402     D  PA1LN1                89    108
010300960402     D  PA1RSD               109    143
010400960402     D* SE RICH DA PGM GIAC. = 1
010500960402     D  PA1GIA               144    144
010600960402     D  PA1FEL               145    147  0
010700960402     D  PA1TBO               148    148
010800960326     D KPJBA         E DS
010900960326     D DS7C          E DS
011000960403     D DS7A2         E DS
011100960326     D DS7D          E DS
011200960329     D CNCR80        E DS
011300960326     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
011400990512     D DSANM         E DS                  EXTNAME(FNANM00F)
011500960419     D DSLR33        E DS                  EXTNAME(FNLR33DS)
011600960327     D DSLR34        E DS                  EXTNAME(FNLR34DS)
011700040923     D  ANM                   89    118
011800040923     D                                     DIM(10)                              ANOM.DA FILTRO
011900111115     D parmlr34      E DS                  EXTNAME(FNLR34DS) prefix(P_)
012000040531     D DANMFLO       E DS
012100960329     D DSUL06        E DS                  EXTNAME(TRUL06DS)
012200960329     D  LIN                    1     90  0
012300960329     D                                     DIM(30)                              FIL. COMODO
012400971118     D DSLV55        E DS                  EXTNAME(FNLV55DS)
012500031028     d dsbl4a        e ds
012600151116     d og143         e ds
012700990512     D FNANM1          DS
012800960401     D  ANMNR1               397    400B 0
012900990512     D FNANM2          DS
013000960401     D  ANMNR2               397    400B 0
013100971118     D WLBDA8          DS
013200971118     D  G08DAT                 1      8  0
013300971118     D  G08INV                 9     16  0
013400971118     D  G08ERR                17     17
013500971118     D  G08TGI                18     22  0
013600000824     D WDT10D          DS
013700000824     D  WDT02G                 1      2
013800000824     D  WTRAT1                 3      3
013900000824     D  WDT02M                 4      5
014000000824     D  WTRAT2                 6      6
014100000824     D  WDT04A                 7     10
014200960329     D*
014300960403     D* DS PER CONTROLLI IN MODO INSERIMENTO
014400960329     D                 DS
014500960329     D  V1SAAS                 1      2  0
014600960329     D  V1SLNP                 3      5  0
014700960329     D  V1SNSP                 6     12  0
014800960402     D  KEYNSP                 1     12  0
014900960403     D  V1SSCN                13     19  0
015000960403     D  V1SFLS                20     22  0
015100960403     D  V1SNRS                23     24  0
015200960403     D  IOFLD                  1     24
015300960405     D                 DS
015400960405     D  WSAAS                  1      2  0
015500960405     D  WSLNP                  3      5  0
015600960405     D  WSNRB                  6      7  0
015700960405     D  WSNSP                  8     14  0
015800960405     D  WSCAB                 15     17
015900960405     D  WKEYB                  1     17
016000960405     D                 DS
016100960405     D  WSFLS                  1      3  0
016200960405     D  WSLNA                  4      6  0
016300960405     D  WSNRS                  7      8  0
016400960405     D  WSSCN                  9     15  0
016500960405     D  WSCAS                 16     18
016600960405     D  WKEYS                  1     18
016700960405     D                 DS
016800990513     D  V2DCCH                 1     23
016900960405     D  W2DCAA                 1      3
017000990513     D  W2DDES                 4     23
017100960411     D                 DS
017200960411     D  V2DFSA                 1     17
017300960411     D  W2CFSA                 1      2
017400960411     D  W2DFSA                 3     17
017500960411     D                 DS
017600960411     D  V2DFSC                 1     17
017700960411     D  W2CFSC                 1      2
017800960411     D  W2DFSC                 3     17
017900960326     D                SDS
018000960326     D  V1DPGM                 1      8
018100041125     D GIRI            s              1  0
018200041125     D WCAAELAB        s                   LIKE(ANMCAA)
018300960521     D TIT1            C                   CONST('***   GESTIONE ANOMA-
018400960521     D                                     LIE   ***')
018500960521     D TIT2            C                   CONST('** INTERROGAZIONE AN-
018600960521     D                                     OMALIE **')
018700040603     D WTRA            C                   CONST('TRASMISSIONE .....:')
018800040603     D WRIC            C                   CONST('RICEZIONE ........:')
018900960327     I*
019000960521     IFNART000
019100960530     I              ARTFLS                      DETFLS
019200960521     I              ARTLNA                      DETLNA
019300960521     I              ARTNRS                      DETNRS
019400960521     I              ARTNSC                      DETNSC
019500960521     IFNBLT1
019600960530     I              BLTFLS                      DETFLS
019700960521     I              BLTLNA                      DETLNA
019800960521     I              BLTNRS                      DETNRS
019900960521     I              BLTNSC                      DETNSC
020000960605     IFNAR27
020100960605     I              ARTFLS                      D27FLS
020200960605     I              ARTLNA                      D27LNA
020300960605     I              ARTNRS                      D27NRS
020400960605     I              ARTNSC                      D27NSC
020500960605     I              ARTAAS                      D27AAS
020600960605     I              ARTLNP                      D27LNP
020700960605     I              ARTNSP                      D27NSP
020800960605     IFNBLT000
020900960605     I              BLTFLS                      D27FLS
021000960605     I              BLTLNA                      D27LNA
021100960605     I              BLTNRS                      D27NRS
021200960605     I              BLTNSC                      D27NSC
021300960605     I              BLTAAS                      D27AAS
021400960605     I              BLTLNP                      D27LNP
021500960605     I              BLTNSP                      D27NSP
021600960326     C****************************************************************
021700960326     C*  RIEPILOGO INDICATORI
021800960326     C***************************************************************
021900960422     C* 02    -  ON --> MANUTENZIONE NON ABILITATO
022000960422     C* 03    -  ON --> INSERIMENTO NON ABILITATO
022100960402     C* 04    -  PROTEZIONE CAMPI SUBFILE
022200960506     C* 05    -  PROTEZIONE CODICE ANOMALIA
022300960402     C* 06    -  OFF --> ND DESCRIZIONE "ANNULLATA" SU VIDEATA D02
022400960403     C* 07    -  'HI' DI CHIAVE SPEDIZIONE SU VIDEATA D02
022500960403     C* 08    -  'HI' DI CHIAVE SEGNACOLLO SU VIDEATA D02
022600960405     C* 09    -  'HI' CAMPI DELLA CHIAVE IN SUBFILE SE CAMPI SPROTETTI
022700960412     C* 10    -  ON - MODALITA' INSERIMENTO
022800960401     C* 11/19 -  DUP SU CAMPI DI I/O
022900960326     C* 28    -  ERRORE GENERICO VIDEATE
023000960326     C* 30    -  COMODO
023100960402     C* 31    -  COMODO
023200961210     C* 35    -  LETTURA ANOMALIE IN MODALITA' INTERROGAZIONE
023300961210     C* 36    -  LETTURA BOLLA IN MODALITA' INTERROGAZIONE
023400960506     C* 40    -  ERRORE CODICE ANOMALIA
023500960401     C* 41    -  ERRORE NUMERO SPEDIZIONE
023600960521     C* 42    -  ERRORE NUMERO SEGNACOLLO
023700960401     C* 43    -  ERRORE LINEA DI PARTENZA
023800960401     C* 44    -  ERRORE ANNO SPEDIZIONE
023900960401     C* 45    -  ERRORE LINEA DI ARRIVO
024000960401     C* 46    -  ERRORE DATA APERTURA ANOMALIA
024100960401     C* 47    -  ERRORE SU CAMPO OPZIONE
024200960401     C* 48    -  ERRORE SU FILIALE SEGNACOLLO
024300960401     C* 49    -  ERRORE NUMERO SERIE
024400960523     C* 51    -  ND CAMPI KEY SPEDIZIONE/SEGNACOLLO
024500960523     C* 52    -  ND SEGNACOLLO
024600960521     C* 98/99 -  PER I FILE DI TRASMISSIONE
024700971103     C* 90    -  RIEMISSIONE FORMATO VIDEO
024800960521     C* 91    -  INTERROGAZIONE ANOMALIE PER SPEDIZIONE
024900960521     C* 92    -  INTERROGAZIONE ANOMALIE PER SEGNACOLLO
025000960326     C***************************************************************
025100960327     C     *ENTRY        PLIST
025200960327     C                   PARM                    KPJBA
025300111115     C                   PARM                    parmlr34
025400111115     c
025500960522     C                   MOVEL     KPJBU         DSLR44
025600960523     C* SOLO LA PRIMA VOLTA
025700960523     C     WVOLT         IFEQ      ' '
025800960523     C* CARICO £1
025900960523     C                   CLEAR                   DSUL06
026000960523     C                   MOVE      '£1'          D06COD
026100960523     C                   MOVEL     SIMFEL        D06KEY
026200960523     C                   MOVEL     DSUL06        KPJBU
026300960523     C                   CALL      'TRUL06R'
026400960523     C                   PARM                    KPJBA
026500960523     C                   MOVEL     KPJBU         DSUL06
026600960523     C                   MOVEA     LIN           L1
026700960523     C                   MOVE      '1'           WVOLT             1
026800151116     c* carico le filiali estere della l1
026900151116     c                   eval      i=1
027000151116     c                   eval      ii=0
027100151116     c                   dow       i<=%elem(l1) and l1(i)>0
027200151116     c     l1(i)         chain     azorg01l
027300151116     c                   if        %found(azorg01l)
027400151116     c                   movel     orgde3        og143
027500151116     c                   if        §ogntw='EEX' or §ogntw='FED' or §ogntw='DPD'
027600151116     c                   add       1             ii
027700151116     c                   eval      est(ii)=l1(i)
027800151116     c                   endif
027900151116     c                   endif
028000151116     c                   add       1             i
028100151116     c                   enddo
028200960523     C                   END
028300960327     C*
028400960521     C                   SELECT
028500960521     C* MODALITA' GESTIONE
028600960521     C     D44MOD        WHENEQ    'G'
028700960521     C                   EXSR      SRGEST
028800960524     C* Emissione subfile se ho finito di caricare la pagina
028900960524    2C     D34END        IFEQ      'S'
029000960524     C     NRR1          OREQ      NRRAL
029100960521     C                   EXSR      SRXFMT
029200960521     C                   END
029300960521     C* MODALITA' INTERROGAZIONE
029400960521     C     D44MOD        WHENEQ    'I'
029500960521     C                   EXSR      SRINTE
029600960521     C* Emissione subfile
029700960521     C                   EXSR      SRXFMT
029800960521     C                   ENDSL
029900960327     C*
030000960326     C     FINE          TAG
030100030717     C*
030200030717     C* VALORIZZO NUOVI CAMPI DELLA DS VALIDI UNICAMENTE COME SELEZIONE IN FASE DI STAMPA
030300030717     C* (ESEGUO UNA VALORIZZAZIONE NEUTRA OVVERO COME SE NN FOSSERO STATE RICHIESTE SELEZIONI)
030400111116     C****               Z-ADD     *ZEROS        D34POS
030500111116     C*****              Z-ADD     99            D34NRS
030600030717     C*
030700111116     c                   if        %parms=1
030800960327     C                   MOVEL     DSLR34        D44LR
030900111116     c                   else
031000111116     C                   MOVEL     DSLR34        parmlr34
031100111116     c                   endif
031200111116     c
031300960522     C                   MOVEL     DSLR44        KPJBU
031400960521     C*
031500960521     C     D44MOD        IFEQ      'G'
031600960327     C                   SETON                                        RT
031700960521     C                   ELSE
031800971118     C                   MOVEL     'C'           D55TLA
031900971118     C                   CALL      'FNLV55R'
032000971118     C                   PARM                    DSLV55
032100960521     C                   SETON                                        LR
032200960521     C                   END
032300960521     C*
032400960521     C****************   MODALITA' GESTIONE ANOMALIE  *****************
032500960521     C     SRGEST        BEGSR
032600960521     C                   MOVEL     D44ANM        DSANM
032700111115     c
032800111115     c* Se passato un solo parametro
032900111115     c                   if        %parms=1
033000960521     C                   MOVEL     D44LR         DSLR34
033100111115     c                   else
033200111115     c
033300111115     c* se passati due parametri passo la DS a parte
033400111115     c                   movel     parmlr34      dslr34
033500111115     c                   endif
033600111111     c*
033700110909     c*  visibile solo per EDP
033800111111     c***                if        d34end='S'
033900111111     c*****                        %subst(knmus:1:3)='EDP'   or anmcaa<>115
034000110909     c
034100150924     C* VERIFICO SE RICHIESTE ANOMALIE inseribili in partenza
034200150924     C                   MOVE      *BLANKS       WinsP
034300150924     c                   if        d34par='P'
034400150924     c                   z-add     1             xx                3 0
034500150924     c                   dow       xx<=10 and anm(xx) <>*blanks
034600040923     C                   MOVEL     '7C'          KCOD
034700150924     C                   MOVEL(P)  anm(xx)       KKEY
034800040923     C     KTAB          CHAIN     TABEL00F                           30
034900040923     C  N30              MOVEL     TBLUNI        DS7C
035000040923     C   30              CLEAR                   DS7C
035100150924     c
035200150924     c     §7cges        ifeq      'S'
035300040923     C     D34INC        ANDEQ     'I'
035400150924     c     §7cpar        andeq     'P'
035500150924     C                   MOVE      '1'           WInsP             1
035600150924     C                   endif
035700150924     c                   add       1             xx
035800150924     c                   enddo
035900150924     C                   endif
036000150924     c
036100960521     C* PULISCO CAMPI DI OUTPUT
036200960521     C                   CLEAR                   D34F03
036300960521     C                   CLEAR                   D34F12
036400960521     C                   CLEAR                   D34RIL
036500960521     C                   SETON                                        20
036600960521     C* CARICO £6 A CAMBIO DI FILIALE GESTIONE
036700960521     C     D34FGS        IFNE      SAVFGS
036800960521     C                   CLEAR                   L6S
036900960521     C                   CLEAR                   DSUL06
037000960521     C                   MOVE      '£6'          D06COD
037100020509     C**!!!              MOVEL     'S'           D06ESC
037200020509     c                   eval      d06esc = 'G'
037300020509     c                   movel     simfel        d06key
037400960521     C                   MOVEL     'L'           D06TLA
037500960521     C                   MOVEL     DSUL06        KPJBU
037600960521     C                   CALL      'TRUL06R'
037700960521     C                   PARM                    KPJBA
037800960521     C                   MOVEL     KPJBU         DSUL06
037900960521     C                   MOVEA     LIN           L6S
038000961210     C                   MOVE      D34FGS        SAVFGS
038100960521     C                   END
038200960521     C*
038300960521     C                   SETOFF                                       0203
038400960521     C* SE RICHIESTE SOLO ANOMALIE IN PARTENZA ACCENDO INDICATORE
038500150924     C* vedo se abilitare in partenza l'inserimento
038600960521    1C     D34PAR        IFEQ      'P'
038700150924     C     WInsP         ANDEQ     *BLANKS
038800960521     C                   SETON                                        03
038900960521     C                   END
039000960521     C* SE IN INTERROGAZIONE ACCENDO INDICATORI PER DISABILITARE
039100960521     C* MANAUTENZIONE ED INSERIMENTO
039200960521     C     D34GES        IFEQ      'I'
039300960521     C                   SETON                                        0203
039400960521     C                   MOVEL     TIT2          V1DTIT
039500960521     C                   ELSE
039600960521     C                   MOVEL     TIT1          V1DTIT
039700960521    1C                   END
039800960521     C*    FINE FILE: IMPOSTO INDICATORE DI SFLEND
039900960521    1C     D34END        IFEQ      'S'
040000960521     C                   SETON                                        23
040100960521   X1C                   ELSE
040200960521     C* NO FINE FILE: CARICAMENTO SUBFILE
040300960521    2C     NRR1          IFLT      NRRAL
040400960521     C                   SETON                                        0405
040500960521     C                   SETOFF                                       09
040600960521     C                   EXSR      RIS01A
040700960523     C                   MOVE      '0'           V1HI51
040800960523     C                   MOVE      '0'           V1HI52
040900960521     C                   ADD       1             NRR1
041000960521     C                   WRITE     LR44S01
041100960521    2C                   END
041200960521    1C                   END
041300110909     c
041400111111     c****               endif
041500110909     c
041600960521     C                   ENDSR
041700960521     C****************   MODALITA' INTERROGAZIONE ANOMALIE  ***********
041800960521     C     SRINTE        BEGSR
041900960522     C*
042000960521     C                   EXSR      PULSFL
042100970217     C                   SETOFF                                       09
042200960521     C* PROTEGGO CAMPI I/O DEL SUBFILE
042300960521     C                   SETON                                        0405
042400960521     C* DISABILITO TASTI DI CONFERMA/INSER...+ OPZIONI VARIAZ/ANN/RIPR.
042500960521     C                   SETON                                        0203
042600960521     C                   MOVEL     TIT2          V1DTIT
042700960521     C*
042800960521     C* POSIZIONAMENTO INIZIALE SU FILE ANOMALIE PER CARICAMENTO SUBFIL
042900960521     C     D44NSP        IFGT      *ZEROS
043000960521     C* Interrogazione anomalie bolla
043100960521     C                   SETON                                        91
043200990512     C     KANM01        SETLL     FNANM01L
043300990512     C     KANM01        READE     FNANM01L                               35
043400960521     C                   MOVE      '1'           WFILE             1
043500960521     C                   ELSE
043600960521     C* Interrogazione anomalie segnacollo
043700960521     C                   SETON                                        92
043800960521     C                   MOVE      D44FIL        W44FIL
043900960521     C                   MOVE      D44LNA        W44LNA
044000960521     C                   MOVE      D44NRS        W44NRS
044100960521     C                   MOVE      D44SCN        W44SCN
044200990512     C     KANM02        SETLL     FNANM02L
044300990512     C     KANM02        READE     FNANM02L                               35
044400960521     C                   MOVE      '2'           WFILE             1
044500960521     C                   END
044600110909     C
044700960521     C                   EXSR      RIS01I
044800110909     c
044900960524     C* SE NON HO CARICATO NULLA SPENGO INDICATORE SFLDSP
045000960524     C     NRR1          IFEQ      *ZEROS
045100960524     C                   SETOFF                                       20
045200960524     C                   END
045300960521     C                   ENDSR
045400960521     C*********   CARICAMENTO SUBFILE MODALITA INTERROGAZIONE  ********
045500960521     C     RIS01I        BEGSR
045600960524     C                   CLEAR                   WFIRST
045700960524     C*
045800960524     C                   EXSR      RIS1IB
045900960524     C*
046000961210     C* SE NON HO RIEMPITO TUTTA LA PAGINA DI SUBFILE ED E' FINE FILE
046100990512     C* (DI FNANM01L); SE STO INTERROGANDO PER SPEDIZIONE,RICHIAMO ROU-
046200961210     C* TINE DI RICERCA  ANOMALIE PER OGNI SEGNACOLLO DELLA SPEDIZIONE
046300961210    1C     WFILE         IFEQ      '1'
046400960521     C     *IN91         ANDEQ     *ON
046500960521     C     NRR1          ANDLT     NRRAL
046600961210     C     *IN35         ANDEQ     *ON
046700960521     C                   SELECT
046800960521     C     D44TBO        WHENEQ    'A'                                          ARRIVI
046900960605     C     WOPNAR        IFEQ      ' '
047000960605     C                   OPEN      FNART01L
047100960605     C                   MOVE      '1'           WOPNAR            1
047200960605     C                   END
047300960522     C     KART01        SETLL     FNART01L
047400961210     C     KART01        READE     FNART01L                               36
047500960521     C     D44TBO        WHENEQ    'P'                                          PARTENZE
047600040615     c     d44tbo        oreq      'T'
047700960605     C     WOPNBL        IFEQ      ' '
047800960605     C                   OPEN      FNBLT01L
047900960605     C                   MOVE      '1'           WOPNBL            1
048000960605     C                   END
048100960522     C     KART01        SETLL     FNBLT01L
048200961210     C     KART01        READE     FNBLT01L                               36
048300040615     C**   D44TBO        WHENEQ    'T'                                          TRANSITI
048400040615     C**   WOPNBT        IFEQ      ' '
048500040615     C**                 OPEN      FNBTT01L
048600040615     C**                 MOVE      '1'           WOPNBT            1
048700040615     C**                 END
048800040615     C**   KART01        SETLL     FNBTT01L
048900040615     C**   KART01        READE     FNBTT01L                               36
049000960521     C                   ENDSL
049100960523     C                   MOVE      '2'           WFILE
049200961210     C  N36              EXSR      RICSCN
049300961210   X1C                   ELSE
049400050929     c
049500961210     C     WFILE         IFEQ      '2'
049600961210     C     *IN91         ANDEQ     *ON
049700961210     C     NRR1          ANDLT     NRRAL
049800050929     c                   if        finiscelett=' '
049900961210     C                   EXSR      RICSCN
050000961210     C* 23 --> SFLEND
050100961210     C   92
050200961210     CAN 36              SETON                                        23
050300050929     c                   else
050400050929     c                   seton                                        23
050500050929     c                   clear                   finiscelett
050600050929     C                   ENDif
050700050929     C                   END
050800961210    1C                   END
050900960524     C*
051000960524     C* SE HO CARICATO ALMENO UN RECORD DI SFL INCREMENTO SFLRCDNBR
051100960524    1C     NRR1          IFGT      NRRSAV
051200960524     C     NRRSAV        ADD       1             V1CNBR
051300960524    1C                   END
051400960524     C* SE FINE LETTURA MEMORIZZO SFLRCDNBR DELL'ULTIMA PAGINA
051500960524     C   23              Z-ADD     V1CNBR        W1CNBR
051600960524     C* MEMORIZZO NRR
051700960524     C                   Z-ADD     NRR1          NRRSAV
051800960521     C                   ENDSR
051900960523     C*********   CARICAMENTO SUBFILE MODALITA INTERROGAZIONE  ********
052000960523     C     RIS1IB        BEGSR
052100960523     C                   SETOFF                                       22  49
052200960523     C                   SETOFF                                       404142
052300960523     C                   SETOFF                                       434445
052400960523     C                   SETOFF                                       464748
052500960523    1C     NRR1          DOWLT     NRRAL
052600961210     C     *IN35         ANDEQ     *OFF
052700960524     C* ESCLUDO RECORD SE STO LEGGENDO PER SPEDIZIONE E SEGNACOLLO>0
052800960524     C* (LO TRATTERO' NEL GIRO PER SEGNACOLLO)
052900960524     C     WFILE         IFEQ      '1'
053000960524     C     ANMSCN        ANDGT     *ZEROS
053100960524     C                   GOTO      LEGGAN
053200960524     C                   END
053300110909     C
053400111115     c***                IF        %subst(knmus:1:3)<>'EDP'  AND anmcaa= 115
053500111115     C***                GOTO      LEGGAN
053600111115     C***                END
053700110909     c
053800960524     C*
053900960523     C     WFILE         IFEQ      '1'
054000960523     C                   Z-ADD     ANMNR1        D44NRR
054100960523     C                   ELSE
054200960523     C                   Z-ADD     ANMNR2        D44NRR
054300960523     C                   END
054400960523     C                   EXSR      RIS01A
054500960523     C*
054600960523     C* SOLO LA PRIMA VOLTA VISUALIZZO CAMPI DELLA CHIAVE
054700960524     C     WFIRST        IFEQ      *BLANKS
054800960524     C                   MOVE      '1'           WFIRST            1
054900960523     C                   SETOFF                                       5152
055000960523     C                   ELSE
055100960524     C     *IN91         IFEQ      *ON
055200960524     C                   SETOFF                                       52
055300960524     C                   ELSE
055400960524     C                   SETON                                        52
055500960524     C                   END
055600960523     C                   SETON                                        51
055700960523     C                   END
055800960523     C*
055900960524     C                   MOVE      *IN51         V1HI51
056000960524     C                   MOVE      *IN52         V1HI52
056100960523     C                   ADD       1             NRR1
056200960523     C                   WRITE     LR44S01
056300960524     C     LEGGAN        TAG
056400960523     C     WFILE         IFEQ      '1'
056500990512     C     KANM01        READE     FNANM01L                               35
056600960523     C                   ELSE
056700990512     C     KANM02        READE     FNANM02L                               35
056800960523     C                   END
056900960523     C                   ENDDO
057000990512     C* MEMORIZZO CAMPI DI FNANM PER NON SPORCARMELI SE PER CASO
057100961210     C* ENTRERO' IN SRINT
057200961210     C  N35              MOVEL     DSANM         DSANMX
057300960523     C                   ENDSR
057400960521     C****************  RICERCA SEGNACOLLI DELLA SPEDIZIONE  **********
057500960521     C     RICSCN        BEGSR
057600960521    1C     NRR1          DOWLT     NRRAL
057700961210     C     *IN36         ANDEQ     *OFF
057800960530     C                   MOVE      DETFLS        W44FIL
057900960521     C                   MOVE      DETLNA        W44LNA
058000960521     C                   MOVE      DETNRS        W44NRS
058100960521     C                   MOVE      DETNSC        W44SCN
058200990512     C     KANM02        SETLL     FNANM02L
058300990512     C     KANM02        READE     FNANM02L                               35
058400961210     C  N35              EXSR      RIS1IB
058500960521     C*
058600960521     C                   SELECT
058700960521     C     D44TBO        WHENEQ    'A'                                          ARRIVI
058800961210     C     KART01        READE     FNART01L                               36
058900960521     C     D44TBO        WHENEQ    'P'                                          PARTENZE
059000040615     c     d44tbo        oreq      'T'
059100961210     C     KART01        READE     FNBLT01L                               36
059200040615     C***  D44TBO        WHENEQ    'T'                                          TRANSITI
059300040615     C***  KART01        READE     FNBTT01L                               36
059400960521     C                   ENDSL
059500960521     C*
059600960521     C                   ENDDO
059700960521     C*
059800050929     c                   if        *in35
059900961210     C   36              SETON                                        23
060000050929     c   36              clear                   finiscelett       1
060100050929     c                   else
060200050929     c* per 35 off - devop finire di leggere le anomalie dell'ultimo
060300050929     c*  segnacollo
060400050929     c                   if        *in36
060500050929     c                   eval      finiscelett='S'
060600050929     c                   endif
060700050929     c                   endif
060800960521     C                   ENDSR
060900960521     C****************  EMISSIONE/GESTIONE SUBFILE  *******************
061000960521     C     SRXFMT        BEGSR
061100960524     C* SOLO SE MODALITA' GESTIONE:
061200960524    1C     D44MOD        IFEQ      'G'
061300960524     C* Se non ho caricato nulla spengo indicatore sfldsp
061400960524    2C     NRR1          IFEQ      *ZEROS
061500960524     C                   SETOFF                                       20
061600960524     C                   MOVE      'S'           D34F12
061700960524   X2C                   ELSE
061800960524     C* Se ho caricato almeno un record di sfl incremento sflrcdnbr
061900960524     C                   Z-ADD     NRRSAV        V1CNBR
062000960524    3C     NRR1          IFGT      NRRSAV
062100960524     C                   ADD       1             V1CNBR
062200960524    3C                   END
062300960524     C* Memorizzo nrr
062400960524     C                   Z-ADD     NRR1          NRRSAV
062500960524     C* Se fine lettura memorizzo sflrcdnbr dell'ultima pagina
062600960524     C   23              Z-ADD     V1CNBR        W1CNBR
062700960524    2C                   END
062800960521     C* Imposto piedi del video
062900960521     C***
063000960521     C*** Modalità gestione
063100960521     C***
063200960522     C  N03
063300960522     CANN20              MOVEL     Z02(2)        V1ZCMD
063400960522     C   03
063500960522     CANN20              MOVEL     Z02(4)        V1ZCMD
063600960521     C* solo interrogazione
063700960521     C   02
063800960521     CAN 20              MOVEL     Z02(1)        V1ZCMD
063900960521     C* solo manutenzione
064000960521     C   03
064100960521     CANN02
064200960521     CAN 20              MOVEL     Z03(1)        V1ZCMD
064300960521     C   03
064400960521     CANN02
064500960521     CAN 20              Z-ADD     1             WF                1 0
064600960521     C   03
064700960521     CANN02
064800960521     CAN 20              Z-ADD     2             WMAX              1 0
064900960521     C* tutte le funzioni abilitate (manutenzione/inserimento/interrog)
065000960521     C  N03
065100960521     CAN 20              MOVEL     Z01(1)        V1ZCMD
065200960521     C  N03
065300960521     CAN 20              Z-ADD     1             WF                1 0
065400960521     C  N03
065500960521     CAN 20              Z-ADD     3             WMAX              1 0
065600960524   X1C                   ELSE
065700960521     C***
065800960521     C*** Modalità interrogazione
065900960521     C***
066000960521     C  N20              MOVE      Z02(4)        V1ZCMD
066100960521     C   91
066200960521     CAN 20              MOVE      Z02(5)        V1ZCMD
066300960521     C   92
066400960521     CAN 20              MOVE      Z02(6)        V1ZCMD
066500960524    1C                   END
066600960521     C* EMISSIONE SUBFILE
066700960521     C     EMC01         TAG
066800960521     C                   WRITE     LR44Z01
066900960521     C  N20              WRITE     LR44D03
067000960521     C                   WRITE     LR44D01
067100960521     C                   EXFMT     LR44C01
067200971103     C                   SETOFF                                       289040
067300960521     C                   SETOFF                                       414243
067400960521     C                   SETOFF                                       444546
067500960521     C                   SETOFF                                       474849
067600960521     C                   CLEAR                   V1ZMSG
067700960521     C* Tasto F3=Fine
067800960521    2C     *INKC         IFEQ      *ON
067900960521     C                   MOVE      'S'           D34F03
068000960521     C                   EXSR      PULSFL
068100960521     C                   GOTO      FXFMT
068200960521    2C                   END
068300960521     C* Tasto F12=Ritorno
068400960521    2C     *INKL         IFEQ      *ON
068500960521     C   10              MOVEL     'S'           D34RIL
068600960521     C  N10              MOVE      'S'           D34F12
068700960521     C                   EXSR      PULSFL
068800960521     C                   GOTO      FXFMT
068900960521    2C                   END
069000960521     C* Tasto F24=Altri Tasti
069100960521    2C     *INKY         IFEQ      *ON
069200960521     C     WF            IFLT      WMAX
069300960521     C                   ADD       1             WF
069400960521     C  N03              MOVEL     Z01(WF)       V1ZCMD
069500960521     C   03              MOVEL     Z03(WF)       V1ZCMD
069600960521     C                   ELSE
069700960521     C                   Z-ADD     1             WF
069800960521     C  N03              MOVEL     Z01(WF)       V1ZCMD
069900960521     C   03              MOVEL     Z03(WF)       V1ZCMD
070000960521     C                   END
070100960521     C                   GOTO      EMC01
070200960521    2C                   END
070300960521     C* Tasto F7=Interrogazione bolla
070400960521    2C     *INKG         IFEQ      *ON
070500960521     C                   SETON                                        22
070600960521     C     V1CPOS        CHAIN     LR44S01                            30
070700960521    3C     *IN30         IFEQ      *OFF
070800960523     C                   MOVE      V1HI51        *IN51
070900960523     C                   MOVE      V1HI52        *IN52
071000960521     C                   Z-ADD     NRR1          V1CNBR
071100960521     C                   UPDATE    LR44S01
071200960521     C                   SETOFF                                       22
071300960605     C                   CLEAR                   WAAS
071400960605     C                   MOVE      V1SLNP        WLNP
071500960605     C                   MOVE      V1SNRS        WNRS
071600960605     C                   MOVE      V1SNSP        WNSP
071700960605     C* Solo se ho la lna:
071800960605    4C     V1SLNA        IFGT      *ZEROS
071900021028     C**   V1SLNA        LOOKUP    L1                                     30
072000960605     C* Se non ho numero spedizione la cerco
072100021028     c* se non trovata bolla arrivi cerco nelle bolle partenza
072200021028     c*  a prescidenre che sia mia la bolla o non mia: tanto posso interrog
072300021028     c*
072400021028     c                   setoff                                       3031
072500021028     c* 30 off - ce'e' bolla ARRIVI
072600021028     C     KBLT          CHAIN     FNART27L                           30
072700021028     c* 30 on  - 31 off c'e' bolla partenza
072800021028     C   30KBLT          CHAIN     FNBLT27L                           31
072900021028     c*
073000021028    5C     V1SNSP        IFEQ      *ZEROS
073100021028     C     *IN30         IFEQ      *OFF
073200040531     C     *IN31         orEQ      *OFF
073300960605     C                   MOVE      D27AAS        WAAS
073400960605     C                   MOVE      D27LNP        WLNP
073500960605     C                   MOVE      D27NRS        WNRS
073600960605     C                   MOVE      D27NSP        WNSP
073700960605     C                   END
073800960605    5C                   END
073900960605     C*
074000960605    5C     WNSP          IFGT      *ZEROS
074100990513     C* Imposto anno a 4 cifre
074200960605    6C     WAAS          IFEQ      *ZEROS
074300990513    7C     V1SAAS        IFGT      60
074400990513     C                   MOVEL     19            WAAS
074500990513   X7C                   ELSE
074600990513     C                   MOVEL     20            WAAS
074700990513    7C                   END
074800990513     C                   MOVE      V1SAAS        WAAS
074900960605    6C                   END
075000021028     c*
075100021028     C* interrogo bolle arrivi
075200021028    6C     *IN30         IFEQ      *Off
075300960523     C                   CLEAR                   DSLR36
075400960523     C                   MOVE      WAAS          PA1AAS
075500960605     C                   MOVE      WLNP          PA1LNP
075600960605     C                   MOVE      WNRS          PA1NRS
075700960605     C                   MOVE      WNSP          PA1NSP
075800960903     C                   MOVE      'N'           PA1FLG
075900040121     C                   MOVE      *blanks       PA1CML
076000960523     C                   MOVEL     V1DPGM        PA1PGM
076100960523     C                   MOVEL     DSLR36        KPJBU
076200960523     C                   CALL      'FNLR36R'
076300960523     C                   PARM                    KPJBA
076400960605   X6C                   ELSE
076500021028     c**
076600960523     C* anomalia della partenza
076700021028   6ac                   if        not *in31
076800021028     C**   KBLP2         SETLL     FNBLP01L                               30
076900021028    7C**   *IN30         IFEQ      *ON
077000960523     C                   CLEAR                   DSLS05
077100960523     C                   MOVE      WAAS          PA0AAS
077200960605     C                   MOVE      WLNP          PA0LNP
077300960605     C                   MOVE      WNRS          PA0NRS
077400960605     C                   MOVE      WNSP          PA0NSP
077500960903     C                   MOVEL     'N'           PA0FLG
077600960523     C                   MOVEL     RAGUT         PA0RSU
077700960523     C                   MOVE      SIMFEL        PA0SIM
077800040121     C                   MOVE      *blanks       PA0CML
077900960523     C                   MOVEL     DSLS05        KPJBU
078000960523     C                   CALL      'FNLS05R'
078100960523     C                   PARM                    KPJBA
078200021028    7C**                 END
078300021028   6aC                   END
078400021028    6C                   END
078500960605    5C                   END
078600960605    4C                   END
078700960605    3C                   END
078800960521     C                   GOTO      EMC01
078900960521    2C                   END
079000960521     C* Tasto F14=Spunte
079100960521    2C     *INKN         IFEQ      *ON
079200960521     C                   SETON                                        22
079300960521     C     V1CPOS        CHAIN     LR44S01                            30
079400960521    3C     *IN30         IFEQ      *OFF
079500960523     C                   MOVE      V1HI51        *IN51
079600960523     C                   MOVE      V1HI52        *IN52
079700960521     C                   Z-ADD     NRR1          V1CNBR
079800960521     C                   UPDATE    LR44S01
079900960521     C                   SETOFF                                       22
080000960521    4C     V1SSCN        IFGT      *ZEROS
080100960521     C                   CLEAR                   DSLR04
080200960521     C                   Z-ADD     0             PA4NFV
080300960521     C                   MOVE      V1SFLS        PA4LNP
080400960521     C                   MOVE      V1SLNA        PA4LNA
080500960521     C                   MOVE      V1SNRS        PA4NRS
080600960521     C                   MOVEL     9             PA4NPG
080700960521     C                   Z-ADD     V1SSCN        PA4NCD
080800960521     C                   Z-ADD     0             PA4NCA
080900960521     C                   MOVEL     99            PA4NPS
081000960521     C                   MOVEL     'FNLR44R'     PA4PGM
081100960903     C                   MOVEL     '1'           PA4FLG
081200960521     C                   MOVEL     DSLR04        KPJBU
081300960521     C                   CALL      'FNLR04R'
081400960521     C                   PARM                    KPJBA
081500960521    4C                   END
081600960521    3C                   END
081700960521     C                   GOTO      EMC01
081800960521    2C                   END
081900960521     C* Tasto F10=Inserimento
082000960521    2C     *INKJ         IFEQ      *ON
082100150924     C*
082200150924    3C**   WInsP         IFEQ      '1'
082300150924     C**                 SETON                                        10
082400150924     C**                 EXSR      WDW04
082500150924     C**                 EXSR      PULSFL
082600150924     C**                 GOTO      FXFMT
082700150924   X3C**                 ELSE
082800960521     C                   EXSR      PULSFL
082900970217     C                   SETON                                        10
083000960521     C                   EXSR      RIS01B
083100960521     C                   MOVEL     Z02(3)        V1ZCMD
083200150924    3C**                 END
083300960521     C                   GOTO      EMC01
083400960521    2C                   END
083500960521     C* Solo se ho caricato qualcosa:
083600960521    2C     NRR1          IFGT      *ZEROS
083700960521     C* Tasto Rollup
083800960521    3C     *IN25         IFEQ      *ON
083900960521     C* Fine lettura --> emetto messaggio fine scorrimento
084000960521    4C     *IN23         IFEQ      *ON                                          FINE LETTURA
084100960521     C                   Z-ADD     W1CNBR        V1CNBR
084200960521     C                   MOVEL     MSG(1)        V1ZMSG
084300960521     C                   SETON                                        28
084400960521     C                   GOTO      EMC01
084500960521   X4C                   ELSE
084600960521     C     *IN10         IFEQ      *ON
084700960521     C                   EXSR      RIS01B
084800960521     C                   GOTO      EMC01
084900960521     C                   ELSE
085000960521     C                   ADD       9             NRRAL
085100960521     C                   Z-ADD     NRRSAV        NRR1
085200960521     C     D44MOD        IFEQ      'G'
085300960521     C                   GOTO      FXFMT
085400960521     C                   ELSE
085500960521     C                   EXSR      RIS01I
085600960521     C                   GOTO      EMC01
085700960521     C                   END
085800960521     C                   END
085900960521    4C                   END
086000960521    3C                   END
086100960521     C* CONTROLLI SUBFILE
086200960521    3C     *IN10         IFEQ      *OFF
086300960521     C                   EXSR      CTRINT
086400960521     C   KC              MOVE      'S'           D34F12
086500960521     C   KC              EXSR      PULSFL
086600960521     C   KC              GOTO      FXFMT
086700960521     C* Se interrogate anomalie riemetto il subfile
086800960521     C     WINT          CABEQ     '1'           EMC01
086900960521     C                   END
087000960521     C* Solo se non sono in interrogazione
087100960521     C     D44MOD        IFEQ      'G'
087200960521     C     D34GES        ANDNE     'I'
087300960521     C  N10              EXSR      CTS01A
087400960521     C   10              EXSR      CTS01B
087500960521     C***
087600960521     C  NKF
087700971103     COR 90
087800960521     COR 28              GOTO      EMC01
087900960521     C*
088000960521    3C     *IN10         IFEQ      *ON
088100960521     C   KF              EXSR      WTRANM
088200960521     C                   EXSR      PULSFL
088300960521     C                   ELSE
088400960521     C   KF              EXSR      AGGANM
088500960521     C                   GOTO      EMC01
088600960521    3C                   END
088700960523     C                   END
088800960521    2C                   END
088900960521     C     FXFMT         ENDSR
089000960327     C****************   PULIZIA SUBFILE ******************************
089100960327     C     PULSFL        BEGSR
089200960327     C                   SETOFF                                       2123
089300960327     C                   WRITE     LR44C01
089400960327     C                   SETON                                        2021
089500960327     C                   MOVE      *ZEROS        NRR1              5 0
089600960327     C                   MOVE      *ZEROS        NRRSAV
089700960327     C                   MOVE      *ZEROS        W1CNBR
089800960327     C                   MOVE      *ZEROS        V1CNBR
089900960327     C                   Z-ADD     9             NRRAL
090000960402     C                   SETOFF                                       100405
090100960405     C                   SETON                                        09
090200960327     C                   ENDSR
090300960327     C*********** RIEMPIMENTO CAMPI SUBFILE S01 ***********************
090400960328     C     RIS01A        BEGSR
090500960405     C                   SETOFF                                       22  49
090600960401     C                   SETOFF                                       404142
090700960401     C                   SETOFF                                       434445
090800960401     C                   SETOFF                                       464748
090900960328     C                   MOVE      ANMAAS        V1SAAS
091000960328     C                   MOVE      ANMLNP        V1SLNP
091100960328     C                   MOVE      ANMNRS        V1SNRS
091200960328     C                   MOVE      ANMNSP        V1SNSP
091300960328     C                   MOVE      ANMLNA        V1SLNA
091400960401     C                   MOVE      ANMFLS        V1SFLS
091500960328     C                   MOVE      ANMSCN        V1SSCN
091600040603     c                   move      anmft4        v1sbva
091700960327     C* DECODIFICO TIPO ANOMALIA
091800960328     C                   MOVEL     '7F'          KCOD
091900960328     C                   MOVEL(P)  ANMTAN        KKEY
092000960328     C     KTAB          CHAIN     TABEL00F                           30
092100960328     C  N30              MOVEL     TBLUNI        V1DTAN
092200960328     C   30              CLEAR                   V1DTAN
092300960404     C                   MOVE      ANMTAN        V1HTAN
092400960327     C* DECODIFICO SOGGETTO ANOMALIA
092500960328     C                   MOVEL     '7G'          KCOD
092600960328     C                   MOVEL(P)  ANMSAN        KKEY
092700960328     C     KTAB          CHAIN     TABEL00F                           30
092800960328     C  N30              MOVEL     TBLUNI        V1DSAN
092900960404     C   30              CLEAR                   V1DSAN
093000960404     C                   MOVE      ANMSAN        V1HSAN
093100960328     C                   MOVE      ANMCAA        V1SCAA
093200960403     C                   MOVE      ANMCAA        V1HCAA
093300960409     C                   CLEAR                   V1HKSP
093400970507     C                   CLEAR                   V1HARB
093500960506     C* DECODIFICO CODICE ANOMALIA
093600960401     C                   CLEAR                   DS7C
093700960328     C                   MOVEL     '7C'          KCOD
093800960328     C                   MOVEL(P)  ANMCAA        KKEY
093900960328     C                   CLEAR                   V1DCAA
094000960328     C                   CLEAR                   V1HGES
094100960328     C                   CLEAR                   V1HANN
094200960402     C                   CLEAR                   V1HKEY
094300960404     C                   CLEAR                   V1HLIO
094400960411     C                   CLEAR                   V1HUEA
094500960328     C     KTAB          CHAIN     TABEL00F                           30
094600960328     C     *IN30         IFEQ      *OFF
094700960328     C                   MOVEL     TBLUNI        DS7C
094800960328     C                   MOVEL     §7CDES        V1DCAA
094900960328     C* Memorizzo in campi hidden se caus.gestibile in manut./ins./ann
095000960328     C                   MOVEL     §7CGES        V1HGES
095100960418     C     D34APA        IFEQ      'P'
095200960328     C                   MOVEL     §7CANN        V1HANN
095300960418     C                   ELSE
095400960418     C                   MOVEL     §7CANA        V1HANN
095500960418     C                   END
095600960402     C                   MOVEL     §7CKEY        V1HKEY
095700960404     C     §7CIDD        IFEQ      'S'
095800960404     C                   MOVEL     §7CLID        V1HLIO
095900960404     C                   END
096000960411     C                   MOVE      §7CUEA        V1HUEA
096100151113     c                   move      §7cuep        v1huep
096200151113     c                   move      §7cpar        v1hpar
096300960328     C                   END
096400960507     C                   MOVE      ANMAIE        V1HAIE
096500960507     C     ANMAIE        IFEQ      'I'
096600960328     C                   MOVE      ANMAIE        V1SAIE
096700960507     C                   ELSE
096800960507     C     §7CIDD        IFEQ      'S'
096900960507     C                   MOVEL     'D'           V1SAIE
097000960507     C                   ELSE
097100960507     C                   MOVEL     'N'           V1SAIE
097200960507     C                   END
097300960507     C                   END
097400960401     C     §7CIDD        IFEQ      'S'
097500960328     C                   MOVE      ANMLID        V1SLID
097600960401     C                   ELSE
097700960401     C                   CLEAR                   V1SLID
097800960401     C                   END
097900960328     C* DECODIFICO FASE APERTURA ANOMALIA
098000960328     C                   MOVEL     '7D'          KCOD
098100960328     C                   MOVEL(P)  ANMFSA        KKEY
098200960328     C                   CLEAR                   V1DFSA
098300960328     C     KTAB          CHAIN     TABEL00F                           30
098400960328     C     *IN30         IFEQ      *OFF
098500960328     C                   MOVEL     TBLUNI        DS7D
098600960328     C                   MOVEL     §7DDES        V1DFSA
098700960328     C                   END
098800960404     C                   MOVE      ANMFSA        V1HFSA
098900960328     C* ROVESCIO DATA ANOMALIA
099000990513     C                   CLEAR                   WLBDA8
099100990513     C                   MOVE      ANMDAO        G08INV
099200990513     C                   MOVEL     '3'           G08ERR
099300990513     C                   CALL      'XSRDA8'
099400990513     C                   PARM                    WLBDA8
099500990513     C                   MOVEL     G08DAT        V1SDAO
099600990513     C                   MOVE      G08DAT        W0020             2 0
099700990513     C                   MOVE      W0020         V1SDAO
099800960419     C* RIEMPIO CAMPI RELATIVI AI DATI DI CHIUSURA (FASE,DATA,CAUSALE)
099900960419     C                   EXSR      RIECHI
100000960403     C     ANMCDU        IFGT      *ZEROS
100100960328     C                   MOVE      ANMCDU        V1SCDU
100200960403     C                   ELSE
100300960403     C                   CLEAR                   V1SCDU
100400960403     C                   END
100500960328     C                   MOVE      ANMNFV        V1SNFV
100600960328     C                   MOVE      ANMNPS        V1SNPS
100700960412     C                   CLEAR                   V1SVAR
100800960412     C                   CLEAR                   V1HVAR
100900960402     C                   MOVEL     ANMATB        V1SATB
101000960412     C                   Z-ADD     D44NRR        V1HNRR
101100960402     C                   MOVE      *IN04         V1HIN4
101200960402     C                   MOVE      *IN05         V1HIN5
101300960405     C                   MOVE      *IN09         V1HIN9
101400960401     C                   MOVEL     D34APA        V1HAPA
101500960401     C                   CLEAR                   V1HPRE
101600960327     C                   ENDSR
101700960419     C*********** RIEMPIMENTO CAMPI CHIUSURA ANOMALIA DI SFL S01 ******
101800960419     C     RIECHI        BEGSR
101900960419     C* DECODIFICO FASE CHIUSURA ANOMALIA
102000960419     C                   MOVE      *BLANKS       V1DFSC
102100960419     C     ANMFSC        IFNE      *BLANKS
102200960419     C                   MOVEL     '7D'          KCOD
102300960419     C                   MOVEL(P)  ANMFSC        KKEY
102400960419     C     KTAB          CHAIN     TABEL00F                           30
102500960419     C     *IN30         IFEQ      *OFF
102600960419     C                   MOVEL     TBLUNI        DS7D
102700960419     C                   MOVEL     §7DDES        V1DFSC
102800960419     C                   END
102900960419     C                   END
103000960419     C* ROVESCIO DATA CHIUSURA ANOMALIA SE DIVERSA DA 0
103100960419     C                   MOVE      *ZEROS        V1SDCH
103200960419     C     ANMDCH        IFGT      *ZEROS
103300990513     C                   CLEAR                   WLBDA8
103400990513     C                   MOVEL     ANMDCH        G08INV
103500990513     C                   MOVEL     '3'           G08ERR
103600990513     C                   CALL      'XSRDA8'
103700990513     C                   PARM                    WLBDA8
103800990514     C                   MOVEL     G08DAT        V1SDCH
103900990513     C                   MOVE      G08DAT        W0020             2 0
104000990513     C                   MOVE      W0020         V1SDCH
104100960419     C                   END
104200041125     C
104300960419     C                   MOVE      ANMCCH        V1SCCH
104400970522     C*
104500960419     C                   ENDSR
104600960328     C**************  RIEMPIMENTO SUBFILE PER INSERIMENTO *************
104700960328     C     RIS01B        BEGSR
104800960429     C                   SETOFF                                       22
104900960523     C                   SETOFF                                       5152
105000960328     C                   Z-ADD     NRRSAV        NRR1
105100960328     C     NRR1          ADD       1             V1CNBR
105200960329     C     NRR1          DOWLT     NRRAL
105300960328     C                   CLEAR                   V1SAAS
105400960328     C                   CLEAR                   V1SLNP
105500960328     C                   CLEAR                   V1SNRS
105600960328     C                   CLEAR                   V1SNSP
105700960402     C* IMPOSTO LINEA DI ARRIVO = FILIALE GESITIONE
105800151113     c                   clear                   v1slna
105900151113     c                   if        winsp=*blanks
106000960402     C                   MOVE      D34FGS        V1SLNA
106100151113     c                   endif
106200960401     C                   CLEAR                   V1SFLS
106300960328     C                   CLEAR                   V1SSCN
106400960328     C                   CLEAR                   V1DTAN
106500960328     C                   CLEAR                   V1DSAN
106600960329     C                   CLEAR                   V1SCAA
106700960328     C                   CLEAR                   V1DCAA
106800960328     C                   CLEAR                   V1HGES
106900960328     C                   CLEAR                   V1HANN
107000960402     C                   CLEAR                   V1HTAN
107100960402     C                   CLEAR                   V1HSAN
107200960402     C                   CLEAR                   V1HKEY
107300960404     C                   CLEAR                   V1HLIO
107400960403     C                   CLEAR                   V1HCAA
107500960409     C                   CLEAR                   V1HKSP
107600970507     C                   CLEAR                   V1HARB
107700960329     C                   CLEAR                   V1HNRR
107800960411     C                   CLEAR                   V1HUEA
107900960328     C                   CLEAR                   V1SAIE
108000960507     C                   CLEAR                   V1HAIE
108100960328     C                   CLEAR                   V1SLID
108200150924     c                   if        winsp='1'
108300150924     C                   MOVE      'P'           V1HAPA
108400150924     C                   MOVEL     '1'           V1HFSA
108500150924     c                   else
108600150924     C                   MOVE      'A'           V1HAPA
108700960329     C                   MOVEL     '2'           V1HFSA
108800150924     c                   endif
108900151113     c                   clear                   v1hpar
109000151113     c                   clear                   v1huep
109100150924     c
109200960329     C                   MOVEL     '7D'          KCOD
109300150924     C                   MOVEL(P)  v1hfsa        KKEY
109400960329     C     KTAB          CHAIN     TABEL00F                           30
109500960329     C  N30              MOVEL     TBLUNI        V1DFSA
109600960329     C   30              CLEAR                   V1DFSA
109700971118     C* IMPOSTO DATA ANOMALIA = DATA DEL GIORNO
109800971118     C                   MOVE      DTGIO6        V1SDAO
109900990513     C                   MOVE      DATEU8        V1HDAO
110000960328     C                   CLEAR                   V1DFSC
110100960328     C                   CLEAR                   V1SDCH
110200960328     C                   CLEAR                   V1SCDU
110300960328     C                   CLEAR                   V1SNFV
110400960328     C                   CLEAR                   V1SNPS
110500960328     C                   CLEAR                   V1SCCH
110600960401     C                   CLEAR                   V1HPRE
110700960402     C                   MOVE      *IN04         V1HIN4
110800960402     C                   MOVE      *IN05         V1HIN5
110900960405     C                   MOVE      *IN09         V1HIN9
111000960523     C                   MOVE      '0'           V1HI51
111100960523     C                   MOVE      '0'           V1HI52
111200960328     C                   ADD       1             NRR1
111300960328     C                   WRITE     LR44S01
111400960328     C                   ENDDO
111500960328     C                   Z-ADD     NRR1          NRRSAV
111600960328     C                   ADD       9             NRRAL
111700960328     C                   ENDSR
111800960329     C******  CONTROLLI SUBFILE  PER INTERROGAZIONE RECORD ANOMALIA ***
111900960329     C     CTRINT        BEGSR
112000960403     C                   SETON                                        22
112100960329     C                   MOVE      *BLANKS       WINT              1
112200960329     C                   READC     LR44S01                                29
112300960329    1C     *IN29         DOWEQ     *OFF
112400960412     C     V1SVAR        IFEQ      'I'
112500960329     C* ESPLODO RECORD DI SUBFILE
112600960412     C                   CLEAR                   V1SVAR
112700960329     C                   MOVE      '1'           WINT
112800960402     C                   EXSR      SRINT
112900960509     C                   Z-ADD     NRR1          V1CNBR
113000961210     C                   MOVEL     DSANMX        DSANM
113100960329     C                   END
113200960402     C                   MOVE      V1HIN4        *IN04
113300960402     C                   MOVE      V1HIN5        *IN05
113400960405     C                   MOVE      V1HIN9        *IN09
113500960523     C                   MOVE      V1HI51        *IN51
113600960523     C                   MOVE      V1HI52        *IN52
113700960329     C                   UPDATE    LR44S01
113800960405     C* PREMUTO F3 DA VIDEATA INTERROGAZIONE ANOMALIA: ESCO DAL PGM
113900960403     C     *INKC         CABEQ     '1'           ENDINT
114000960329     C                   READC     LR44S01                                29
114100960329     C                   ENDDO
114200960403     C     ENDINT        ENDSR
114300960328     C******  CONTROLLI SUBFILE  MODALITA' INTERROGAZ/MANUTENZ. *******
114400960328     C     CTS01A        BEGSR
114500960403     C                   SETON                                        22
114600960426     C                   MOVE      ' '           SAVNBR            1
114700960329     C                   READC     LR44S01                                29
114800960329    1C     *IN29         DOWEQ     *OFF
114900960412    2C     V1SVAR        IFNE      *BLANKS
115000960412     C     V1HVAR        ORNE      *BLANKS
115100990512     C     V1HNRR        CHAIN(N)  FNANM00F                           30
115200960412     C* SE NON TROVATA ANOMALIA ERRORE
115300960412     C     *IN30         IFEQ      *ON
115400960412     C                   MOVE      'A'           V1SATB
115500960412     C                   MOVEL     MSG(31)       V1ZMSG
115600960412     C                   SETON                                        4728
115700960412     C                   GOTO      ENDCTA
115800960412     C                   END
115900960418     C* ERRORE SE RICHIESTA MANUTENZIONE/ANNULLAMENTO SU ANOMALIA GIà
116000960418     C*           CHIUSA CON CAUSALE DIVERSA DA 'AN'
116100970217     C* ESLUDO IL SEGUENTE CONTROLLO SE SI TRATTA DI AN.300=N°RITIRI
116200960418     C     ANMDCH        IFGT      *ZEROS
116300960418     C     ANMCCH        ANDNE     'AN'
116400960422     C     ANMCCH        ANDNE     'PR'
116500970217     C     ANMCAA        ANDNE     300
116600960403     C                   SELECT
116700960412     C     V1SVAR        WHENEQ    'A'
116800960403     C                   MOVEL     MSG(12)       V1ZMSG
116900960412     C                   SETON                                        4728
117000960412     C                   GOTO      ENDCTA
117100960412     C     V1SVAR        WHENEQ    'R'
117200960403     C                   MOVEL     MSG(14)       V1ZMSG
117300960412     C                   SETON                                        4728
117400960412     C                   GOTO      ENDCTA
117500960412     C     V1HVAR        WHENEQ    'V'
117600960412     C     *INKF         ANDEQ     *ON
117700960412     C                   MOVEL     MSG(30)       V1ZMSG
117800960412     C                   SETON                                        040528
117900960412     C                   SETOFF                                       09
118000960412     C                   MOVE      V1HNRR        D44NRR
118100960412     C                   EXSR      RIS01A
118200960412     C                   SETON                                        2247
118300960412     C                   GOTO      ENDCTA
118400960412     C     V1SVAR        WHENEQ    'V'
118500960403     C                   MOVEL     MSG(16)       V1ZMSG
118600960412     C                   SETON                                        4728
118700960412     C                   GOTO      ENDCTA
118800960403     C                   ENDSL
118900960329    3C                   END
119000960423     C**** VERIFICO ANNULLABILITA' DELL'ANOMALIA ***
119100960412    3C     V1SVAR        IFEQ      'A'
119200960418     c* Mai annullabile
119300960329    4C     V1HANN        IFEQ      *BLANKS
119400960418     C* Annullabile solo prima della trasmissione
119500960418     C     V1HANN        OREQ      'P'
119600960418     C     ANMFT1        ANDNE     *BLANKS
119700960418     C     V1HANN        OREQ      'P'
119800960418     C     V1HLIO        ANDNE     V1SLID
119900960506     C     V1SCAA        ANDNE     '070'
120000960506     C     V1SCAA        ANDNE     '071'
120100960418     C* Annullabile solo dopo la trasmissione
120200960418     C     V1HANN        OREQ      'D'
120300960418     C     ANMFT1        ANDEQ     *BLANKS
120400960418     C     V1HANN        OREQ      'D'
120500960418     C     V1HLIO        ANDEQ     V1SLID
120600960506     C     V1SCAA        ANDNE     '070'
120700960506     C     V1SCAA        ANDNE     '071'
120800960329     C                   MOVEL     MSG(12)       V1ZMSG
120900960401     C                   SETON                                        4728
121000960329     C                   GOTO      ENDCTA
121100960329    4C                   END
121200960418     C* Anomalia già annullata
121300960329    4C     V1SATB        IFNE      *BLANKS
121400960418     C     ANMCCH        OREQ      'AN'
121500960329     C                   MOVEL     MSG(13)       V1ZMSG
121600960401     C                   SETON                                        4728
121700960329     C                   GOTO      ENDCTA
121800960329    4C                   END
121900960429     C* Controlli particolari per annullamento anomalie partenza:
122000960429     C* 005 - Collo partito non arrivato
122100960429     C* 010 - No record bolla arrivo
122200960429     C* 012 - Manca record bolla e colli
122300960429    4C     V1HAPA        IFEQ      'P'
122400960429    5C     V1SCAA        IFEQ      '005'
122500960429     C     V1SCAA        OREQ      '010'
122600960429     C     V1SCAA        OREQ      '012'
122700960429     C                   EXSR      CTRANN
122800960429     C     WFLG01        IFEQ      'E'
122900960429     C                   MOVEL     MSG(34)       V1ZMSG
123000960429     C                   SETON                                        4728
123100960429     C                   GOTO      ENDCTA
123200960429     C                   END
123300960429    5C                   END
123400960429    4C                   END
123500970214    3C                   END
123600960423     C**** VERIFICO SE ANOMALIA RIPRISTINABILE ***
123700960412    3C     V1SVAR        IFEQ      'R'
123800960419     c* Mai ripristinabile
123900960329    4C     V1HANN        IFEQ      *BLANKS
124000960401     C                   MOVEL     MSG(14)       V1ZMSG
124100960401     C                   SETON                                        4728
124200960329     C                   GOTO      ENDCTA
124300960329    4C                   END
124400960329    4C     V1SATB        IFEQ      *BLANKS
124500960419     C     ANMCCH        ANDNE     'AN'
124600960329     C                   MOVEL     MSG(15)       V1ZMSG
124700960401     C                   SETON                                        4728
124800960329     C                   GOTO      ENDCTA
124900960329    4C                   END
125000960401    3C                   END
125100960423     C**** VERIFICO SE ANOMALIA MANUTENZIONABILE  ***
125200960412    3C     V1SVAR        IFEQ      'V'
125300960423     C* Non manutenzionabile se anomalia della partenza o anomalia
125400960423     C* già trasmessa
125500960419     C     V1HAPA        IFEQ      'P'
125600970214     C     ANMCAA        ANDNE     300
125700960423     C     ANMFT1        ORNE      *BLANKS
125800960423     C     V1SLID        ORNE      V1HLIO
125900960506     C     V1SCAA        ANDNE     '070'
126000960506     C     V1SCAA        ANDNE     '071'
126100960419     C                   MOVEL     MSG(16)       V1ZMSG
126200960419     C                   SETON                                        4728
126300960419     C                   GOTO      ENDCTA
126400960419     C                   END
126500970214     C* Se richiesta manutenzione anomalia 300 emetto window anzichè
126600970214     C* sproteggere i campi del subfile
126700970214     C     ANMCAA        IFEQ      300
126800040923     c     v1hges        ifeq      'S'
126900970214     C                   EXSR      WDW04
127000040923     C                   ELSE
127100040923     C                   MOVEL     MSG(16)       V1ZMSG
127200040923     C                   SETON                                        4728
127300040923     c                   endif
127400970214     C                   GOTO      ENDCTA
127500970214     C                   END
127600960412     C* CAMPI PROTETTI: SE POSSIBILE LI SPROTEGGO
127700960412    4C     V1HIN4        IFEQ      '1'
127800960412   4BC     *INKF         IFEQ      *OFF
127900960412    5C     V1HGES        IFEQ      'S'
128000960412     C                   SETOFF                                       04
128100960412     C                   SETON                                        09
128200960412     C                   MOVE      *IN04         V1HIN4
128300960412     C                   MOVE      *IN09         V1HIN9
128400960412     C                   MOVE      V1SVAR        V1HVAR
128500960506     C* Sproteggo codice anomalia solo se annullabile:
128600960412    6C     V1HANN        IFEQ      *BLANKS
128700960419     C     V1HANN        OREQ      'P'
128800960419     C     ANMFT1        ANDNE     *BLANKS
128900960419     C     V1HANN        OREQ      'P'
129000960419     C     V1HLIO        ANDNE     V1SLID
129100960506     C     V1SCAA        ANDNE     '070'
129200960506     C     V1SCAA        ANDNE     '071'
129300960419     C     V1HANN        OREQ      'D'
129400960419     C     ANMFT1        ANDEQ     *BLANKS
129500960419     C     V1HLIO        ANDEQ     V1SLID
129600960506     C     V1SCAA        ANDNE     '070'
129700960506     C     V1SCAA        ANDNE     '071'
129800960419     C* . . . Protezione
129900960412     C                   SETON                                        05
130000960412   X6C                   ELSE
130100960419     C* . . . Sprotezione
130200960412     C                   SETOFF                                       05
130300960412    6C                   END
130400960412     C                   MOVE      *IN05         V1HIN5
130500960412   X5C                   ELSE
130600960412     C                   MOVEL     MSG(16)       V1ZMSG
130700960412     C                   SETON                                        4728
130800960412     C                   GOTO      ENDCTA
130900960412    5C                   END
131000960412   4BC                   END
131100960412   X4C                   ELSE
131200960412     C* CAMPI GIA' SPROTETTI --> LI CONTROLLO
131300960412     C                   EXSR      CTRS01
131400960412    4C                   END
131500960412     C                   MOVE      *BLANKS       V1SVAR
131600960412   X3C                   ELSE
131700960412     C* CAMPI GIA' SPROTETTI --> LI CONTROLLO
131800960412    4C     V1HVAR        IFEQ      'V'
131900960423     C* Se premuto tasto di conferma e anomalia già trasmessa: errore
132000960423     C   KFANMFT1        IFNE      *BLANKS
132100960423     C     V1HLIO        ORNE      V1SLID
132200960506     C     V1SCAA        ANDNE     '070'
132300960506     C     V1SCAA        ANDNE     '071'
132400960423     C                   MOVEL     MSG(30)       V1ZMSG
132500960423     C                   SETON                                        040528
132600960423     C                   SETOFF                                       09
132700960423     C                   MOVE      V1HNRR        D44NRR
132800960423     C                   EXSR      RIS01A
132900960423     C                   SETON                                        2247
133000960423     C                   GOTO      ENDCTA
133100960423     C                   END
133200960412     C                   EXSR      CTRS01
133300960412    4C                   END
133400960412    3C                   END
133500960329     C*
133600960509    2C                   END
133700960401     C     ENDCTA        TAG
133800960402     C                   MOVE      V1HIN4        *IN04
133900960402     C                   MOVE      V1HIN5        *IN05
134000960405     C                   MOVE      V1HIN9        *IN09
134100960426     C     *IN28         IFEQ      *ON
134200960513     C     V1SVAR        ORNE      *BLANKS
134300960426     C     SAVNBR        ANDEQ     ' '
134400960426     C     V1HVAR        OREQ      'V'
134500960426     C     SAVNBR        ANDEQ     ' '
134600960329     C                   Z-ADD     NRR1          V1CNBR
134700960426     C                   MOVE      '1'           SAVNBR
134800960426     C                   END
134900960523     C                   MOVE      V1HI51        *IN51
135000960523     C                   MOVE      V1HI52        *IN52
135100960329     C                   UPDATE    LR44S01
135200971103     C   28              SETON                                        2990
135300960329     C  N29              READC     LR44S01                                29
135400960329    1C                   ENDDO
135500960401     C                   ENDSR
135600960328     C******  CONTROLLI SUBFILE  MODALITA' INSERIMENTO     ************
135700960328     C     CTS01B        BEGSR
135800960403     C                   SETON                                        22
135900960405     C* PULIZIA CAMPI DI COMODO
136000960405     C                   MOVE      *ZEROS        II
136100960405     C                   MOVE      *ZEROS        LL
136200960405     C                   CLEAR                   SKB
136300960405     C                   CLEAR                   SKS
136400960329     C* azzero campi per gestione DUP
136500960329     C                   CLEAR                   DUPAAS
136600960329     C                   CLEAR                   DUPCAA
136700960401     C                   CLEAR                   DUPLNP
136800960401     C                   CLEAR                   DUPNRS
136900960401     C                   CLEAR                   DUPNSP
137000960401     C                   CLEAR                   DUPLNA
137100960401     C                   CLEAR                   DUPFLS
137200960401     C                   CLEAR                   DUPSCN
137300960401     C                   CLEAR                   DUPDAO
137400960329     C*
137500960329     C                   READC     LR44S01                                29
137600960329    1C     *IN29         DOWEQ     *OFF
137700960329     C                   CLEAR                   V1DCAA
137800960329     C                   CLEAR                   V1DSAN
137900960329     C                   CLEAR                   V1DTAN
138000960329     C                   CLEAR                   V1SAIE
138100960507     C                   CLEAR                   V1HAIE
138200960329     C*   Gestione tasto DUP
138300960401     C   11              MOVE      DUPCAA        V1SCAA
138400960401     C   12              MOVE      DUPAAS        V1SAAS
138500960401     C   13              MOVE      DUPNRS        V1SNRS
138600960401     C   14              MOVE      DUPNSP        V1SNSP
138700960401     C   15              MOVE      DUPLNA        V1SLNA
138800960401     C   16              MOVE      DUPLNP        V1SLNP
138900960401     C   17              MOVE      DUPSCN        V1SSCN
139000960401     C   18              MOVE      DUPDAO        V1SDAO
139100960401     C   19              MOVE      DUPFLS        V1SFLS
139200960329     C* CONTROLLO SOLO SE IMMESSI DATI
139300960329    2C     IOFLD         IFGT      *ZEROS
139400960329     C     V1SCAA        ORNE      *BLANKS
139500960329     C     V1SCAA        ANDNE     *ZEROS
139600960329     C                   EXSR      CTRS01
139700960409     C                   ELSE
139800960409     C                   CLEAR                   V1HKSP
139900970507     C                   CLEAR                   V1HARB
140000960409     C                   CLEAR                   V1HPRE
140100960329    2C                   END
140200960523     C                   MOVE      V1HI51        *IN51
140300960523     C                   MOVE      V1HI52        *IN52
140400960329     C                   Z-ADD     NRR1          V1CNBR
140500960329     C                   UPDATE    LR44S01
140600971103     C   28              SETON                                        2990
140700960329     C  N29              READC     LR44S01                                29
140800960329    1C                   ENDDO
140900960328     C                   ENDSR
141000960329     C*************  CONTROLLI COMUNI SUBFILE  ************************
141100960329     C     CTRS01        BEGSR
141200960506     C*** CODICE ANOMALIA  (SOLO SE SPROTETTO)
141300960402   1AC     V1HIN5        IFEQ      '0'
141400960329     C*   Ricerca in tabella
141500960401     C     '?'           SCAN      V1SCAA                                 30
141600960401    1C     *IN30         IFEQ      *ON
141700960329     C                   MOVEL     CODUT         §KUT
141800960329     C                   MOVEL     '7C'          §COD
141900960329     C                   MOVEL     *BLANKS       §KEY
142000960329     C                   CALL      'X§TABER'
142100960329     C                   PARM                    §KUT              1 0
142200960329     C                   PARM                    §COD              2
142300960329     C                   PARM                    §KEY              8
142400960329     C                   PARM                    §DES             30
142500960329     C                   MOVEL     §KEY          V1SCAA
142600960401    2C     V1SCAA        IFNE      *BLANKS
142700960329     C                   MOVEL     §DES          V1DCAA
142800960401    2C                   END
142900971103     C                   SETON                                        2990
143000960329     C                   GOTO      ENDCTR
143100960401    1C                   END
143200960329     C*   Controlli di validità'
143300960401    1C     V1SCAA        IFEQ      *ZEROS
143400960329     C     V1SCAA        OREQ      *BLANKS
143500960329     C                   MOVEL     MSG(2)        V1ZMSG
143600960401     C                   SETON                                        4028
143700960329     C                   GOTO      ENDCTR
143800960401    1C                   END
143900960329     C                   TESTN                   V1SCAA               30
144000960401    1C     *IN30         IFEQ      *ON
144100960329     C                   MOVE      V1SCAA        W001A             1
144200960401    2C     W001A         IFLT      '0'
144300960329     C                   SETOFF                                       30
144400960401    2C                   END
144500960401    1C                   END
144600960401    1C     *IN30         IFEQ      *OFF
144700960329     C                   MOVEL     MSG(2)        V1ZMSG
144800960401     C                   SETON                                        4028
144900960329     C                   GOTO      ENDCTR
145000960401    1C                   END
145100960329     C* Chaino tabella per reperimento altri dati
145200960329     C                   MOVEL     '7C'          KCOD
145300960329     C                   MOVEL(P)  V1SCAA        KKEY
145400960329     C     KTAB          CHAIN     TABEL00F                           30
145500960401    1C     *IN30         IFEQ      *ON
145600960329     C     TBLFLG        ORNE      *BLANKS
145700960329     C                   MOVEL     MSG(2)        V1ZMSG
145800960401     C                   SETON                                        4028
145900960329     C                   GOTO      ENDCTR
146000960401    1C                   END
146100960329     C                   MOVEL     TBLUNI        DS7C
146200960329     C                   MOVEL     §7CDES        V1DCAA
146300960506     C*  Controllo che codice anomalia sia gestibile in inserimento
146400960402    1C     §7CGES        IFEQ      *BLANKS
146500960329     C                   MOVEL     MSG(5)        V1ZMSG
146600960401     C                   SETON                                        4028
146700960329     C                   GOTO      ENDCTR
146800960401    1C                   END
146900960506     C*  Se codice ok imposto anche altri campi di output del video
147000960402     C*  (N.B.: v1hges e v1hann mantengo quelli di origine)
147100960329     C                   MOVEL     §7CTAN        V1HTAN
147200960329     C                   MOVEL     §7CSAN        V1HSAN
147300960402     C                   MOVEL     §7CKEY        V1HKEY
147400960411     C                   MOVEL     §7CUEA        V1HUEA
147500960329     C                   MOVEL     '7F'          KCOD
147600960329     C                   MOVEL(P)  §7CTAN        KKEY
147700960329     C     KTAB          CHAIN     TABEL00F                           30
147800960329     C  N30              MOVEL     TBLUNI        V1DTAN
147900960329     C                   MOVEL     '7G'          KCOD
148000960329     C                   MOVEL(P)  §7CSAN        KKEY
148100960329     C     KTAB          CHAIN     TABEL00F                           30
148200960329     C  N30              MOVEL     TBLUNI        V1DSAN
148300960507     C                   MOVEL     §7CAIE        V1HAIE
148400960507     C     §7CIDD        IFEQ      'S'
148500960507     C                   MOVEL     'D'           V1SAIE
148600960507     C                   ELSE
148700960507     C                   MOVEL     'N'           V1SAIE
148800960507     C                   END
148900151113     c                   movel     §7cpar        v1hpar
149000151113     C                   MOVEL     §7CUEp        V1HUEp
149100151116     c* Anomalia esterna o interna non della partenza --> solo da arrivo
149200151116     c* anomalia interna della partenza --> solo dalla partenza
149300151116     c                   if        v1hapa='P' and (v1haie='E' or
149400151116     c                                             v1hpar='A' or
149500151116     c                                             v1hpar=' '    )
149600151116     C                   MOVEL     MSG(45)       V1ZMSG
149700151116     C                   SETON                                        4028
149800151116     C                   GOTO      ENDCTR
149900151116     c                   endif
150000151116     c                   if        v1hapa='A' and  v1hpar='P'
150100151116     C                   MOVEL     MSG(44)       V1ZMSG
150200151116     C                   SETON                                        4028
150300151116     C                   GOTO      ENDCTR
150400151116     c                   endif
150500151116     c
150600960402   1AC                   END
150700960402     C* CONTROLLO LINEE SE IMMESSE
150800960402     C* - LA LNP, SE IMMESSA, Deve esistere in organigramma
150900960402    1C     V1SLNP        IFGT      *ZEROS
151000960402     C     V1SLNP        CHAIN     AZORG01L                           30
151100960402    2C     *IN30         IFEQ      *ON
151200960402     C     ORGFVA        ORNE      *BLANKS
151300960402     C                   MOVEL     MSG(3)        V1ZMSG
151400960402     C                   SETON                                        4328
151500960402     C                   GOTO      ENDCTR
151600960402    2C                   END
151700151116     c* Se anomalia interna della partenza deve essere una filiale in
151800151113     c* gestione
151900151113    2c                   if        v1haie='I' and v1hpar='P'
152000151113    3c                   if        v1huep=*blanks
152100151113    4C     V1SLNp        IFNE      D34FGS
152200151113     C     V1SLNp        LOOKUP    L6S                                    30
152300151116     C     V1SLNp        LOOKUP    est                                    31
152400151113    5C     *IN30         IFEQ      *OFF
152500151116    5C     *IN31         andeq     *OFF
152600151113     C                   MOVEL     MSG(8)        V1ZMSG
152700151113     C                   SETON                                        4328
152800151113     C                   GOTO      ENDCTR
152900151113    5C                   END
153000151113    4C                   END
153100151113   x3c                   else
153200151113     c* se anomalia che deve usare la filiale emittente la lnp deve essere diversa
153300151113    4c                   if        v1slnp=d34fgs
153400151113     C                   MOVEL     MSG(10)       V1ZMSG
153500151113     C                   SETON                                        4328
153600151113     C                   GOTO      ENDCTR
153700151113    4c                   endif
153800151113    3c                   endif
153900151113    2c                   endif
154000960402    1C                   END
154100960402     C* - LA Fls, SE IMMESSA, Deve esistere in organigramma
154200960402    1C     V1SFLS        IFGT      *ZEROS
154300960402     C     V1SFLS        CHAIN     AZORG01L                           30
154400960402    2C     *IN30         IFEQ      *ON
154500960402     C     ORGFVA        ORNE      *BLANKS
154600960402     C                   MOVEL     MSG(4)        V1ZMSG
154700960402     C                   SETON                                        4828
154800960402     C                   GOTO      ENDCTR
154900960402    2C                   END
155000960402    1C                   END
155100960411     C* - La Lna deve essere una filiale in gestione
155200960418     C*   (Se è un'anomalia per la quale occorre utilizzare la LNA
155300960418     C*   anzichè la filiale emittente)
155400151113     c* Solo se anomalia esterna o se interna solo se anomalia <>'P'
155500151113    1C***  V1SLNA        IFGT      *ZEROS
155600151113    1c                   if        v1slna>*zeros and
155700151113     c                             (v1haie='E' or v1hpar=' ' or
155800151113     c                                            v1hpar='A')
155900960411    2C     V1HUEA        IFEQ      *BLANKS
156000960411    3C     V1SLNA        IFNE      D34FGS
156100960402     C     V1SLNA        LOOKUP    L6S                                    30
156200960411    4C     *IN30         IFEQ      *OFF
156300960402     C                   MOVEL     MSG(8)        V1ZMSG
156400960402     C                   SETON                                        4528
156500960402     C                   GOTO      ENDCTR
156600960411    4C                   END
156700960411    3C                   END
156800960411   X2C                   ELSE
156900960524     C* - La Lna deve esistere in organigramma e deve essere diversa
157000960524     C*   dalla filiale emittente
157100960411     C*   (Se per questa anomalia occorre utilizzare la filiale emit-
157200960411     C*   tente)
157300960524    3C     V1SLNA        IFEQ      D34FGS
157400960524     C                   MOVEL     MSG(36)       V1ZMSG
157500960524     C                   SETON                                        4528
157600960524     C                   GOTO      ENDCTR
157700960524    3C                   END
157800960411     C     V1SLNA        SETLL     AZORG01L                               30
157900960411    3C     *IN30         IFEQ      *OFF
158000960411     C                   MOVEL     MSG(7)        V1ZMSG
158100960411     C                   SETON                                        4528
158200960411     C                   GOTO      ENDCTR
158300960411    3C                   END
158400960402    2C                   END
158500960402    1C                   END
158600960401     C***** CONTROLLO CHIAVE PRINCIPALE:
158700960401    1C                   SELECT
158800960402     C*     - PER SPEDIZIONE (AAS/LNP/NRS/NSP/LNA)
158900960402     C     V1HKEY        WHENEQ    'B'
159000960401     C* LINEA DI PARTENZA OBBLIGATORIA
159100960401    2C     V1SLNP        IFEQ      *ZEROS
159200960401     C                   SETON                                        4328
159300960329     C                   MOVEL     MSG(3)        V1ZMSG
159400960329     C                   GOTO      ENDCTR
159500960401    2C                   END
159600960401     C* NUMERO SPEDIZIONE OBBLIGATORIO
159700960401    2C     V1SNSP        IFEQ      *ZEROS
159800960401     C                   SETON                                        4128
159900960401     C                   MOVEL     MSG(6)        V1ZMSG
160000960401     C                   GOTO      ENDCTR
160100960401    2C                   END
160200151116     c* CONTROLLI PER ANOMALIA 20
160300151116     C     V1SCAA        IFEQ      '020'
160400151116     C     V1SAAS        IFGT      60
160500151116     C                   MOVEL     19            WAAS
160600151116     C                   ELSE
160700151116     C                   MOVEL     20            WAAS
160800151116     C                   END
160900151116     C                   MOVE      V1SAAS        WAAS
161000151116     c* la spedizione deve esistere su blp
161100151116     c     kblp          chain     fnblp01l
161200151116     c                   if        not %found(fnblp01l)
161300151116     C                   MOVEL     MSG(41)       V1ZMSG
161400151116     C                   SETON                                        444128
161500151116     C                   GOTO      ENDCTR
161600151116     c                   else
161700151116     c* se lna a video non immessa la imposto prendendola dal record bolla
161800151116     c                   move      blplna        v1slna
161900151116     c* errore se la spedizione è partita o chiusa
162000151116     c                   if        blpft1<>*blanks
162100151116     C                   MOVEL     MSG(42)       V1ZMSG
162200151116     C                   SETON                                        4128
162300151116     C                   GOTO      ENDCTR
162400151116     c                   endif
162500151116     c* errore se la lnp non è fra quelle per cui è prevista la sospensione automatica
162600151116     c* in chiusura f.v.p.
162700151116     c                   exsr      sr_fibsp
162800151116     c                   if        w_Sibsp=*blanks
162900151116     C                   MOVEL     MSG(43)       V1ZMSG
163000151116     C                   SETON                                        4128
163100151116     C                   GOTO      ENDCTR
163200151116     c                   endif
163300151116     c
163400151116     c                   endif
163500151116     c*
163600151116     c                   endif
163700960402     C     V1HKEY        WHENEQ    'S'
163800960401     C*    - PER SEGNACOLLO (FLS/LNA/NRS/SCN)
163900960401     C* FILIALE SEGNACOLLO OBBLIGATORIO
164000960401    2C     V1SFLS        IFEQ      *ZEROS
164100960401     C                   SETON                                        4828
164200960401     C                   MOVEL     MSG(4)        V1ZMSG
164300960401     C                   GOTO      ENDCTR
164400960401    2C                   END
164500960401     C* NUMERO SEGNACOLLO OBBLIGATORIO
164600960401    2C     V1SSCN        IFEQ      *ZEROS
164700960401     C                   SETON                                        4228
164800960401     C                   MOVEL     MSG(17)       V1ZMSG
164900960401     C                   GOTO      ENDCTR
165000960401    2C                   END
165100960401    1C                   ENDSL
165200960402     C* LINEA DI ARRIVO SEMPRE OBBLIGATORIA
165300960402     C* (Sia in key per spedizione che in key per segnacollo)
165400960402    2C     V1SLNA        IFEQ      *ZEROS
165500960402     C                   MOVEL     MSG(7)        V1ZMSG
165600960402     C                   SETON                                        4528
165700960402     C                   GOTO      ENDCTR
165800960402    2C                   END
165900960401     C**** DATA APERTURA ANOMALIA
166000960401    1C     V1SDAO        IFEQ      *ZEROS
166100960401     C                   MOVEL     MSG(9)        V1ZMSG
166200960401     C                   SETON                                        4628
166300960401     C                   GOTO      ENDCTR
166400960401    1C                   END
166500990513     C                   CLEAR                   WLBDA8
166600990513     C                   MOVE      V1SDAO        G08DAT
166700990513     C                   CALL      'XSRDA8'
166800990513     C                   PARM                    WLBDA8
166900990513    1C     G08ERR        IFEQ      '1'
167000960401     C                   MOVEL     MSG(9)        V1ZMSG
167100960401     C                   SETON                                        4628
167200960401     C                   GOTO      ENDCTR
167300960401   X1C                   ELSE
167400990513     C                   MOVE      G08INV        V1HDAO
167500960401    1C                   END
167600960401     C**** CONTROLLI DI CHIAVE DOPPIA
167700960403     C                   CLEAR                   WFLAG
167800960830     C                   CLEAR                   SAVFLS
167900960830     C                   CLEAR                   SAVSCN
168000960404     C     V1SCAA        IFEQ      '070'
168100960404     C     V1SCAA        OREQ      '071'
168200960404     C                   CLEAR                   V1SLID
168300960404     C                   END
168400960401     C                   MOVE      V1SCAA        WCAA
168500960401    1C                   SELECT
168600960402     C     V1HKEY        WHENEQ    'B'
168700990513     C     V1SAAS        IFGT      60
168800990513     C                   MOVEL     19            WAAS
168900990513     C                   ELSE
169000990513     C                   MOVEL     20            WAAS
169100990513     C                   END
169200990513     C                   MOVE      V1SAAS        WAAS
169300990512     C     KANM1         SETGT     FNANM01L
169400990512     C     KANM1         READPE    FNANM01L                               30
169500960401    2C     *IN30         DOWEQ     *OFF
169600960401    3C     V1HNRR        IFNE      ANMNR1
169700960401     C                   EXSR      CTRDOP
169800960401     C   28              GOTO      ENDCTR
169900960401    3C                   END
170000960404     C* SOLO PER ANOMALIE 070-ECCEDE BAM E 071-MANCA BAM: MEMORIZZO IL
170100960404     C* LIVELLO IDD DELL'ANOMALIA PIù RECENTE.
170200960830     C* MEMORIZZO ANCHE FILIALE E NUMERO SEGNAC. > 0 PER POI
170300960830     C* CONFRONTARLI CON QUELLI IMMESSI EVENTUALMENTE A VIDEO
170400960404     C     V1SCAA        IFEQ      '070'
170500960404     C     V1SCAA        OREQ      '071'
170600960404     C     WFLAG         IFEQ      *BLANKS
170700960506     C     ANMFLE        ANDEQ     D34FGS
170800960404     C                   MOVE      ANMLID        V1SLID
170900960404     C                   MOVE      '1'           WFLAG             1
171000960404     C                   END
171100960830     C     ANMFLS        IFGT      *ZEROS
171200960830     C                   MOVE      ANMFLS        SAVFLS
171300960830     C                   MOVE      ANMSCN        SAVSCN
171400960830     C                   END
171500960404     C                   END
171600990512     C     KANM1         READPE    FNANM01L                               30
171700960401    2C                   ENDDO
171800960401     C*
171900960402     C     V1HKEY        WHENEQ    'S'
172000990512     C     KANM2         SETGT     FNANM02L
172100990512     C     KANM2         READPE    FNANM02L                               30
172200960401    2C     *IN30         DOWEQ     *OFF
172300960401    3C     V1HNRR        IFNE      ANMNR2
172400960401     C                   EXSR      CTRDOP
172500960401     C   28              GOTO      ENDCTR
172600960401    3C                   END
172700990512     C     KANM2         READPE    FNANM02L                               30
172800960401    2C                   ENDDO
172900960401    1C                   ENDSL
173000960405     C********************
173100960405     C* SOLO IN MANUTENZIONE :
173200960405     C*********************
173300960403     C* VERIFICO SE OCCORRE IMPOSTARE NUOVO LIVELLO IDD
173400960403    1C     *IN10         IFEQ      *OFF
173500960403    2C     WCAA          IFNE      V1HCAA
173600960403    3C     §7CIDD        IFEQ      'S'
173700960404     C                   MOVE      §7CLID        V1SLID
173800960506     C                   ELSE
173900960506     C                   CLEAR                   V1SLID
174000960403    3C                   END
174100960403   X2C                   ELSE
174200960404     C     V1SLID        IFEQ      *BLANKS
174300960404     C                   MOVE      V1HLIO        V1SLID
174400960404     C                   END
174500960403    2C                   END
174600960403   X1C                   ELSE
174700960405     C********************
174800960405     C* SOLO IN IMMISSIONE:
174900960405     C********************
175000960405     C* VERIFICO SE OCCORRE IMPOSTARE NUOVO LIVELLO IDD
175100960403    2C     V1SLID        IFEQ      *BLANKS
175200960403     C     §7CIDD        ANDEQ     'S'
175300960403     C                   MOVE      §7CLID        V1SLID
175400960403    2C                   END
175500960405     C* Controllo cha non sia già stato inserito un record con la
175600960409     C* stessa chiave
175700960405    2C                   SELECT
175800960405     C     V1HKEY        WHENEQ    'B'
175900960405     C                   MOVE      V1SAAS        WSAAS
176000960405     C                   MOVE      V1SLNP        WSLNP
176100960405     C                   MOVE      V1SNRS        WSNRB
176200960405     C                   MOVE      V1SNSP        WSNSP
176300960405     C                   MOVE      V1SCAA        WSCAB
176400960405     C     WKEYB         LOOKUP    SKB                                    30
176500960405    3C     *IN30         IFEQ      *ON
176600960405     C                   MOVEL     MSG(26)       V1ZMSG
176700960405     C                   SETON                                        284443
176800960405     C                   SETON                                        494140
176900960405    3C                   END
177000960405     C                   ADD       1             II                3 0
177100960405     C                   MOVE      WKEYB         SKB(II)
177200960405     C     V1HKEY        WHENEQ    'S'
177300960405     C                   MOVE      V1SFLS        WSFLS
177400960405     C                   MOVE      V1SLNA        WSLNA
177500960405     C                   MOVE      V1SNRS        WSNRS
177600960405     C                   MOVE      V1SSCN        WSSCN
177700960405     C                   MOVE      V1SCAA        WSCAS
177800960405     C     WKEYS         LOOKUP    SKS                                    30
177900960405    3C     *IN30         IFEQ      *ON
178000960405     C                   MOVEL     MSG(26)       V1ZMSG
178100960405     C                   SETON                                        284845
178200960405     C                   SETON                                        494240
178300960405    3C                   END
178400960405     C                   ADD       1             LL                3 0
178500960405     C                   MOVE      WKEYS         SKS(LL)
178600960405    2C                   ENDSL
178700960405     C   28              GOTO      ENDCTR
178800960403    1C                   END
178900960402     C* CONTROLLO COMPLETEZZA CHIAVE FACOLTATIVA, SE IMMESSA
179000960402    1C                   SELECT
179100960402     C     V1HKEY        WHENEQ    'B'
179200960402     C* Immessa filiale segnacollo e non il numero
179300960402    2C     V1SFLS        IFGT      *ZEROS
179400960402     C     V1SSCN        ANDEQ     *ZEROS
179500960403     C                   SETON                                        2842
179600960402     C                   MOVEL     MSG(19)       V1ZMSG
179700960402   X2C                   ELSE
179800960402     C* Immesso il numero segnacollo e non la filiale
179900960402    3C     V1SFLS        IFEQ      *ZEROS
180000960402     C     V1SSCN        ANDGT     *ZEROS
180100960403     C                   SETON                                        2848
180200960402     C                   MOVEL     MSG(21)       V1ZMSG
180300960402    3C                   END
180400960402    2C                   END
180500960402     C     V1HKEY        WHENEQ    'S'
180600960402    2C     KEYNSP        IFGT      *ZEROS
180700960402    3C     V1SNSP        IFEQ      *ZEROS
180800960403     C                   SETON                                        2841
180900960402     C                   MOVEL     MSG(25)       V1ZMSG
181000960402   X3C                   ELSE
181100000225     C     V1SLNP        IFEQ      *ZEROS
181200960403     C                   SETON                                        284344
181300960402     C                   MOVEL     MSG(24)       V1ZMSG
181400960402    4C                   END
181500960402    3C                   END
181600960402    2C                   END
181700960402    1C                   ENDSL
181800960402     C   28              GOTO      ENDCTR
181900960409     C*
182000960409     C******************************
182100960409     C* Solo per anomalie 070 e 071:
182200960409     C******************************
182300960409    1C     V1SCAA        IFEQ      '070'
182400960409     C     V1SCAA        OREQ      '071'
182500960830     C* Errore se immessi filiale/numero segnacollo diversi da quelli
182600960830     C* di un'altra anomalia con la stessa chiave (questo controllo
182700960830     C* serve per ricevere correttamente le anomalie visto che il
182800960830     C* programma di ricezione se fls/scn>0 legge le anomalie con key
182900960830     C* segnacollo)
183000960830     C     V1SFLS        IFGT      *ZEROS
183100960830     C     SAVFLS        ANDGT     *ZEROS
183200960830     C     V1SFLS        IFNE      SAVFLS
183300960830     C     V1SSCN        ORNE      SAVSCN
183400960830     C                   MOVEA     MSG(37)       WMS
183500960830     C                   MOVE      SAVFLS        W003A             3
183600960830     C                   MOVEA     W003A         WMS(67)
183700960830     C                   MOVE      SAVSCN        W007A             7
183800960830     C                   MOVEA     W007A         WMS(71)
183900960830     C                   MOVEA     WMS           V1ZMSG
184000960830     C                   SETON                                        284248
184100960830     C                   GOTO      ENDCTR
184200960830     C                   END
184300960830     C                   END
184400960830     C*
184500960409    2C     V1SCAA        IFEQ      '070'
184600960409     C                   MOVE      071           WSCAB
184700960409   X2C                   ELSE
184800960409     C                   MOVE      070           WSCAB
184900960409    2C                   END
185000960409     C                   MOVE      V1SAAS        WSAAS
185100960409     C                   MOVE      V1SLNP        WSLNP
185200960409     C                   MOVE      V1SNRS        WSNRB
185300960409     C                   MOVE      V1SNSP        WSNSP
185400960409     C************
185500960409     C* Immissione
185600960409     C************
185700960409    2C     *IN10         IFEQ      *ON
185800960506     C* Controllo che per la stessa bolla non sia stato immesso sia il
185900960506     C* codice 070 che 071
186000960409     C     WKEYB         LOOKUP    SKB                                    30
186100960409    3C     *IN30         IFEQ      *ON
186200960409     C                   MOVEL     MSG(27)       V1ZMSG
186300960409     C                   SETON                                        284443
186400960409     C                   SETON                                        4941
186500960409     C                   GOTO      ENDCTR
186600960409    3C                   END
186700960409    2C                   END
186800960830     C*****************************
186900960830     C* Manutenzione o inserimento:
187000960830     C*****************************
187100960409    2C     *IN10         IFEQ      *OFF
187200960409     C     WCAA          ANDNE     V1HCAA
187300960409     C     *IN10         OREQ      *ON
187400960506     C                   CLEAR                   WNTRM
187500960506     C                   CLEAR                   WTRM
187600960506     C                   CLEAR                   WNMIA
187700960506     C                   CLEAR                   WFLE
187800960506     C                   MOVE      WSCAB         WCAA
187900990513     C     V1SAAS        IFGT      60
188000990513     C                   MOVEL     19            WAAS
188100990513     C                   ELSE
188200990513     C                   MOVEL     20            WAAS
188300990513     C                   END
188400990513     C                   MOVE      V1SAAS        WAAS
188500990512     C     KANM1         SETLL     FNANM01L
188600990512     C     KANM1         READE     FNANM01L                               30
188700960506    3C     *IN30         DOWEQ     *OFF
188800960506     C     WNTRM         ANDEQ     *BLANKS
188900960409     C* Se in manutenzione scarto me stesso
189000960506    4C     *IN10         IFEQ      *OFF
189100960409     C     V1HNRR        ANDEQ     ANMNR1
189200960506     C                   GOTO      LEGGI
189300960506    4C                   END
189400960506     C* Anomalia non mia
189500960506    4C     ANMFLE        IFNE      D34FGS
189600960506     C                   MOVE      '1'           WNMIA             1
189700960506     C                   MOVE      ANMFLE        WFLE
189800960506   X4C                   ELSE
189900960506    5C     ANMFT1        IFEQ      *BLANKS
190000960506     C* Mia anomalia non trasmessa
190100960506     C                   MOVE      '1'           WNTRM             1
190200960506   x5C                   ELSE
190300960506    6C     ANMFT1        IFNE      *BLANKS
190400960506     C* Mia anomalia trasmessa
190500960506     C                   MOVE      '1'           WTRM              1
190600960506    6C                   END
190700960506    5C                   END
190800960506    4C                   END
190900960506     C     LEGGI         TAG
191000990512     C     KANM1         READE     FNANM01L                               30
191100960506    3C                   ENDDO
191200970507     C**
191300970507     C* SE E' MODIFICA LA SPEDIZIONE CONTROLLO SE C'E' IN ARB
191400970507     C     WKEYB         IFNE      V1HKSP
191500970507     C                   CLEAR                   V1HARB
191600970507     C                   ENDIF
191700970507     C**
191800960506    3C     WNTRM         IFEQ      '1'
191900960506     C     WTRM          OREQ      '1'
192000960506     C     WNMIA         OREQ      '1'
192100960506     C* Decodifico codice anomalia
192200960506     C                   MOVE      *BLANKS       WDES             24
192300960506     C                   MOVE      *BLANKS       WDES13           13
192400960409     C                   MOVEL     '7C'          KCOD
192500960409     C                   MOVEL(P)  ANMCAA        KKEY
192600960409     C     KTAB          CHAIN     TABEL00F                           30
192700960409     C  N30              MOVEL     TBLUNI        WDES
192800960506     C  N30              MOVEL     TBLUNI        WDES13
192900960506     C* Mia anomalia non trasmessa: errore bloccante
193000960506    4C     WNTRM         IFEQ      '1'
193100960506     C                   SETON                                        284443
193200960506     C                   SETON                                        4941
193300960506     C                   MOVEA     MSG(29)       WMS
193400960506     C                   MOVE      ANMCAA        W003A             3
193500960506     C                   MOVEA     W003A         WMS(38)
193600960506     C                   MOVEA     WDES          WMS(42)
193700960506     C                   MOVEA     WMS           V1ZMSG
193800960506     C                   GOTO      ENDCTR
193900960506   x4C                   ELSE
194000960506     C*
194100960506     C* Mia anomalia già trasmessa o anomalia non mia: errore forzabile
194200960506    5C                   SELECT
194300960506     C     WTRM          WHENEQ    '1'
194400960506     C     WKEYB         IFNE      V1HKSP
194500960506     C                   SETON                                        284443
194600960506     C                   SETON                                        4941
194700960506     C                   MOVEA     MSG(28)       WMS
194800960506     C                   MOVE      ANMCAA        W003A             3
194900960506     C                   MOVEA     W003A         WMS(38)
195000960506     C                   MOVEA     WDES          WMS(42)
195100960506     C                   MOVEA     WMS           V1ZMSG
195200960506     C                   GOTO      ENDCTR
195300960506     C                   END
195400960506     C     WNMIA         WHENEQ    '1'
195500960506     C     WKEYB         IFNE      V1HKSP
195600960506     C                   SETON                                        284443
195700960506     C                   SETON                                        4941
195800960506     C                   MOVEA     MSG(35)       WMS
195900960409     C                   MOVE      ANMCAA        W003A             3
196000960506     C                   MOVEA     W003A         WMS(34)
196100960506     C                   MOVEA     WDES13        WMS(38)
196200960506     C                   MOVE      WFLE          W003A
196300960506     C                   MOVEA     W003A         WMS(63)
196400960409     C                   MOVEA     WMS           V1ZMSG
196500960409     C                   GOTO      ENDCTR
196600960506     C                   END
196700960506    5C                   ENDSL
196800960506    4C                   END
196900960506    3C                   END
197000970507     C*
197100970507     C* IN MODIFICA: SE CAMBIATO SOLO IL NUMERO SPEDIZIONE RICONTROLLO
197200970507     C*  SU FNARB
197300970507   X2C                   ELSE
197400970507     C**
197500970507     C* SE E' MODIFICA LA SPEDIZIONE CONTROLLO SE C'E' IN ARB
197600970507     C     WKEYB         IFNE      V1HKSP
197700970507     C                   CLEAR                   V1HARB
197800970507     C                   ENDIF
197900960506    2C                   END
198000960506    1C                   END
198100970507     C*****
198200970507     C* SOLO PER ANOMALIA MANCA DDT
198300970507     C*****
198400970507     C* SE MODIFICATA LA SPEDIZIONE RICONTROLLO
198500970507    1C     V1SCAA        IFEQ      '071'
198600970507     C* VERIFICO SE C'E' LA BOLLA--> MESSAGGIO FORZABILE
198700970507     C     V1HARB        ANDEQ     ' '
198800990513     C     V1SAAS        IFGT      60
198900970507     C                   MOVEL     19            WAAS
199000970507     C                   ELSE
199100970507     C                   MOVEL     20            WAAS
199200970507     C                   END
199300970507     C                   MOVE      V1SAAS        WAAS
199400970507     C*
199500970507     C     KBLP          SETLL     FNARB01L                               30
199600970507     C*
199700970507    2C     *IN30         IFEQ      *OFF
199800970507     C                   MOVEL     'S'           V1HARB
199900970507     C                   SETON                                        284443
200000970507     C                   SETON                                        4941
200100970507     C                   MOVEA     MSG(39)       V1ZMSG
200200970507     C                   GOTO      ENDCTR
200300970507   X2C                   ELSE
200400970507     C* CONTROLLO SE E' UN NO DDT --> ERRORE NON FORZABILE
200500060216     C     KAR4          CHAIN     FIAR401L                           30
200600970507    3C     *IN30         IFEQ      *OFF
200700031028     c                   Movel     Ar4not        dsbl4a
200800031028     c                   Move      §b4abm        wddt              1
200900970507     C*
201000970507    4C     WDDT          IFNE      'S'
201100970507     C     WDDT          ANDNE     'C'
201200031028     c     wddt          andne     'P'
201300031028     c     wddt          andne     'Q'
201400970507     C                   SETON                                        284443
201500970507     C                   SETON                                        4941
201600970507     C                   MOVEA     MSG(40)       V1ZMSG
201700970507     C                   GOTO      ENDCTR
201800970507     C*
201900970507   X4C                   ELSE
202000970507     C                   MOVEL     'S'           V1HARB
202100970507    4C                   ENDIF
202200970507     C*
202300970507   X3C                   ELSE
202400970507     C                   MOVEL     'S'           V1HARB
202500970507    3C                   ENDIF
202600970507    2C                   ENDIF
202700970507    1C                   ENDIF
202800970507     C*
202900960401     C* Memorizzo CAMPI per DUP
203000960401     C                   MOVE      V1SCAA        DUPCAA
203100960401     C                   MOVE      V1SAAS        DUPAAS
203200960401     C                   MOVE      V1SLNP        DUPLNP
203300960401     C                   MOVE      V1SNRS        DUPNRS
203400960401     C                   MOVE      V1SNSP        DUPNSP
203500960401     C                   MOVE      V1SFLS        DUPFLS
203600960401     C                   MOVE      V1SSCN        DUPSCN
203700960401     C                   MOVE      V1SLNA        DUPLNA
203800960401     C                   MOVE      V1SDAO        DUPDAO
203900960401     C* MESSAGGI DI AVVISO:
204000971118     C* Avviso se data apertura anomalia > di data del giorno
204100990513    1C     V1HDAO        IFGT      DATEU8
204200960401     C     V1HDAO        ANDNE     V1HPRE
204300960401     C                   SETON                                        4628
204400960401     C                   MOVEL     MSG(23)       V1ZMSG
204500960409     C                   MOVE      V1HDAO        V1HPRE
204600960401    1C                   END
204700960409     C     ENDCTR        TAG
204800960410    1C     V1SCAA        IFEQ      '070'
204900960410     C     V1SCAA        OREQ      '071'
205000960410     C     WKEYB         IFNE      V1HKSP
205100960410     C                   MOVE      WKEYB         V1HKSP
205200960410     C                   END
205300960410     C                   END
205400960409     C                   ENDSR
205500960401     C*************  CONTROLLI DI CHIAVE DOPPIA ***********************
205600960401     C     CTRDOP        BEGSR
205700960418     C*
205800960418    1C     V1SCAA        IFEQ      '012'
205900960418     C                   SETON                                        444341
206000960418     C                   SETON                                        404928
206100960418     C                   MOVEL     MSG(32)       V1ZMSG
206200960418     C                   GOTO      ENDDOP
206300960418    1C                   END
206400160203    1C     V1SCAA        IFEQ      '020'
206500160203     C                   SETON                                        444341
206600160203     C                   SETON                                        404928
206700160203     C                   MOVEL     MSG(46)       V1ZMSG
206800160203     C                   GOTO      ENDDOP
206900160203    1C                   END
207000960418     C*
207100960401    1C     ANMDCH        IFEQ      *ZEROS
207200960405     C     V1HDAO        ORLE      ANMDAO
207300960403     C     ANMLID        ORGE      WLCI
207400960401    2C                   SELECT
207500960402     C     V1HKEY        WHENEQ    'B'
207600960401     C                   SETON                                        444341
207700960402     C     V1HKEY        WHENEQ    'S'
207800960403     C                   SETON                                        4842
207900960401    2C                   ENDSL
208000960403    2C                   SELECT
208100960403     C* Errore di anomalia già aperta
208200960403     C     ANMDCH        WHENEQ    *ZEROS
208300960401     C                   MOVEL     MSG(18)       V1ZMSG
208400960403     C* errore di esistenza anomalia con data apertura più recente
208500960403     C     V1HDAO        WHENLT    ANMDAO
208600960401     C                   MOVEL     MSG(20)       V1ZMSG
208700960403     C                   SETON                                        46
208800960403     C* Errore se anomalia già CHIUSA COME IDD
208900960403     C     ANMLID        WHENGE    WLCI
209000960401     C                   MOVEL     MSG(22)       V1ZMSG
209100960403    2C                   ENDSL
209200960403     C*
209300960403     C                   SETON                                        28
209400960403     C                   SETON                                        404549
209500960403     C                   GOTO      ENDDOP
209600960403    1C                   END
209700960401     C     ENDDOP        ENDSR
209800960429     C****** CONTROLLI PARTICOLARI/DI ANNULLABILITA'/RIPR. ************
209900960429     C     CTRANN        BEGSR
210000960429     C                   CLEAR                   WFLG01
210100960429     C                   SETOFF                                       30
210200960429     C                   SELECT
210300960429     C     V1HKEY        WHENEQ    'B'
210400990513     C* Imposto anno a 4 cifre
210500041124     C     V1SAAS        IFGT      80
210600990513     C                   MOVEL     19            WAAS
210700990513     C                   ELSE
210800990513     C                   MOVEL     20            WAAS
210900990513     C                   END
211000990513     C                   MOVE      V1SAAS        WAAS
211100960429     C     KBLP          SETLL     FNBLP01L                               30
211200960429     C     V1HKEY        WHENEQ    'S'
211300960429     C     KBLT          SETLL     FNBLT27L                               30
211400040615     C**N30V1SCAA        IFEQ      '005'
211500040615     C***  KBLT          SETLL     FNBTT27L                               30
211600040615     C***                END
211700960429     C                   ENDSL
211800960429     C   30              MOVE      'E'           WFLG01            1
211900960429     C                   ENDSR
212000960402     C***********  INTERROGAZIONE DI UN RECORD SINGOLO  ***************
212100960402     C     SRINT         BEGSR
212200990512     C     V1HNRR        CHAIN(N)  FNANM00F                           30
212300960605    1C     *IN30         IFEQ      *OFF
212400960403     C     ANMATB        COMP      *BLANKS                            0606
212500960403     C                   MOVE      ANMAAS        V2CAAS
212600960403     C                   MOVE      ANMLNP        V2CLNP
212700960605     C                   MOVE      ANMNSP        V2CNSP
212800960605    2C     ANMNSP        IFGT      *ZEROS
212900960403     C                   MOVE      ANMNRS        V2CSRB
213000960403     C                   MOVE      ANMLNA        V2CLAB
213100960605   X2C                   ELSE
213200960605     C                   CLEAR                   V2CSRB
213300960605     C                   CLEAR                   V2CLAB
213400960605     C* Cerco spedizione per poterla visualizzare
213500960605    3C     ANMLNA        IFGT      *ZEROS
213600040531     C     KBLT2         CHAIN     FNBLT27L                           31
213700960605    4C     *IN31         IFEQ      *OFF
213800960605     C                   MOVE      D27AAS        V2CAAS
213900960605     C                   MOVE      D27LNP        V2CLNP
214000960605     C                   MOVE      D27NRS        V2CSRB
214100960605     C                   MOVE      D27NSP        V2CNSP
214200960605     C                   MOVE      D27LNA        V2CLAB
214300040531     c* se non impostata zona, la imposto
214400040531     c                   if        anmfl4=000  or anmfl4=999
214500040531     c     kblp27        chain     fnblp01l
214600040531     c                   if        %found(fnblp01l)
214700040531     c                   z-add     blpznc        anmfl4
214800040531     c                   endif
214900040531     c                   endif
215000960605    4C                   END
215100960605    3C                   END
215200960605    2C                   END
215300960605     C*
215400960403     C                   MOVE      ANMFLS        V2CFLS
215500960605    2C     ANMSCN        IFGT      *ZEROS
215600960403     C                   MOVE      ANMLNA        V2CLAS
215700960403     C                   MOVE      ANMNRS        V2CSRS
215800960605   X2C                   ELSE
215900960403     C                   CLEAR                   V2CLAS
216000960403     C                   CLEAR                   V2CSRS
216100960605    2C                   END
216200960403     C                   MOVE      ANMSCN        V2CSCN
216300960403     C                   MOVE      ANMNPS        V2CNPS
216400040531     C* VISLUALIZZO SE COLLO DI VALORE
216500040531     C                   SETOFF                                       93
216600040531     C                   IF        ANMFT4='V'
216700040531     C                   SETON                                        93
216800040531     C                   ENDIF
216900040531     C
217000960403     C* DECODIFICO TIPO ANOMALIA
217100960403     C                   MOVEL     '7F'          KCOD
217200960403     C                   MOVEL(P)  ANMTAN        KKEY
217300960403     C     KTAB          CHAIN     TABEL00F                           30
217400960403     C  N30              MOVEL     TBLUNI        V2DTAN
217500960403     C   30              CLEAR                   V2DTAN
217600960403     C* DECODIFICO SOGGETTO ANOMALIA
217700960403     C                   MOVEL     '7G'          KCOD
217800960403     C                   MOVEL(P)  ANMSAN        KKEY
217900960403     C     KTAB          CHAIN     TABEL00F                           30
218000960403     C  N30              MOVEL     TBLUNI        V2DSAN
218100960403     C   30              CLEAR                   V2DSAN
218200960506     C* DECODIFICO CODICE ANOMALIA
218300960403     C                   MOVE      ANMCAA        V2CCAA
218400960403     C                   MOVEL     '7C'          KCOD
218500960403     C                   MOVEL(P)  ANMCAA        KKEY
218600960403     C     KTAB          CHAIN     TABEL00F                           30
218700960403     C  N30              MOVEL     TBLUNI        DS7C
218800960403     C   30              CLEAR                   DS7C
218900960403     C                   MOVEL     §7CDES        V2DCAA
219000960507     C* INTERNA/I.D.D./NON I.D.D.
219100960507     C                   CLEAR                   W001A
219200960507     C     ANMAIE        IFEQ      'I'
219300961118     C     ANMAIE        OREQ      'X'
219400960507     C                   MOVE      ANMAIE        W001A
219500960507     C                   ELSE
219600960507     C     §7CIDD        IFEQ      'S'
219700960507     C                   MOVE      'D'           W001A             1
219800960507     C                   ELSE
219900960507     C                   MOVE      'N'           W001A
220000960507     C                   END
220100960507     C                   END
220200960507     C     W001A         LOOKUP    TAB           TABDES                   30
220300960507     C                   MOVEL     TABDES        V2DAIE
220400960507     C*
220500960403     C     §7CKEY        COMP      'B'                                080807
220600960403     C*
220700960403     C     §7CIDD        IFEQ      'S'
220800960403     C                   MOVE      ANMLID        V2CLID
220900960403     C                   ELSE
221000960403     C                   CLEAR                   V2CLID
221100960403     C                   END
221200960403     C* APERTURA:
221300960403     C* - decodifico fase
221400960403     C                   MOVEL     '7D'          KCOD
221500960403     C                   MOVEL(P)  ANMFSA        KKEY
221600960403     C     KTAB          CHAIN     TABEL00F                           30
221700960403     C  N30              MOVEL     TBLUNI        DS7D
221800960403     C   30              CLEAR                   DS7D
221900960411     C                   MOVEL     §7DDES        W2DFSA
222000960411     C                   MOVEL     ANMFSA        W2CFSA
222100960411     C                   MOVE      '-'           W2CFSA
222200960403     C*
222300990513     C                   CLEAR                   WLBDA8
222400990513     C                   MOVE      ANMDAO        G08INV
222500990513     C                   MOVEL     '3'           G08ERR
222600990513     C                   CALL      'XSRDA8'
222700990513     C                   PARM                    WLBDA8
222800990513     C                   MOVE      G08DAT        V2CDAO
222900960403     C                   MOVE      ANMCDU        V2CCDU
223000960403     C                   MOVE      ANMNFV        V2CNFV
223100960403     C* CHIUSURA:
223200960403     C* - decodifico fase
223300960405     C                   CLEAR                   V2DFSC
223400960405     C     ANMFSC        IFNE      *BLANKS
223500960403     C                   MOVEL     '7D'          KCOD
223600960403     C                   MOVEL(P)  ANMFSC        KKEY
223700960403     C     KTAB          CHAIN     TABEL00F                           30
223800960403     C  N30              MOVEL     TBLUNI        DS7D
223900960403     C   30              CLEAR                   DS7D
224000960411     C                   MOVEL     §7DDES        W2DFSC
224100960411     C                   MOVEL     ANMFSC        W2CFSC
224200960411     C                   MOVE      '-'           W2CFSC
224300960405     C                   END
224400960403     C*
224500990513     C                   CLEAR                   WLBDA8
224600990513     C                   MOVE      ANMDCH        G08INV
224700990513     C                   MOVEL     '3'           G08ERR
224800990513     C                   CALL      'XSRDA8'
224900990513     C                   PARM                    WLBDA8
225000990513     C                   MOVE      G08DAT        V2CDCH
225100960403     C* - decodifico causale
225200960405     C                   CLEAR                   V2DCCH
225300960405     C     ANMCCH        IFNE      *BLANKS
225400970522     C     ANMCCH        ANDNE     'RA'
225500960405     C                   MOVEL     ANMCCH        W2DCAA
225600960405     C                   MOVE      '-'           W2DCAA
225700960403     C                   MOVEL     '7E'          KCOD
225800960403     C                   MOVEL(P)  ANMCCH        KKEY
225900960403     C     KTAB          CHAIN     TABEL00F                           30
226000960405     C  N30              MOVEL     TBLUNI        W2DDES
226100960405     C                   END
226200040531     C* imposto la zona da anmfl4
226300000516     C                   SETOFF                                       01
226400040531     c                   if        anmfl4<>999
226500040531     c                   move      anmfl4        v2cznc
226600040531     c                   seton                                        01
226700040531     c                   endif
226800040531     c
226900040531     c* Data e ora di creazione anomalia
227000040531     c                   clear                   v2cdac
227100040531     c                   clear                   v2corc
227200040531     c                   if        anmflo<>*blanks
227300040531     c                   movel     anmflo        danmflo
227400040531     c                   if        §anmdac>0
227500040531     C                   CLEAR                   WLBDA8
227600040531     C                   MOVE      §anmdac       G08INV
227700040531     C                   MOVEL     '3'           G08ERR
227800040531     C                   CALL      'XSRDA8'
227900040531     C                   PARM                    WLBDA8
228000040531     C                   MOVE      G08DAT        V2Cdac
228100040531     c                   endif
228200040531     c                   z-add     §anmorc       v2corc
228300040531     c                   endif
228400040531     c
228500040531     c* cerco la zona dalla bolla, sel atrovo
228600040531     C**                 MOVEL     ANMFLO        DANM01
228700040531     C**   §ANZNC        IFNE      *BLANKS
228800040531     C**                 SETON                                        01
228900040531     C**                 MOVEL     §ANZNC        V2CZNC
229000040531     C**                 ENDIF
229100960403     C*
229200960403     C                   MOVE      ANMFLE        V2CFLE
229300960403     C                   MOVE      ANMFL1        V2CFL1
229400960403     C                   MOVE      ANMFL2        V2CFL2
229500960411     C                   CLEAR                   V2DFT1
229600960411     C                   CLEAR                   V2DFT2
229700000904     C* SOLO SE SONO DIVERSI LI DECODIFICO UNO PER UNO
229800000904    2C     ANMFT1        IFNE      ANMFT2
229900000904    3C     ANMFT1        IFEQ      'T'
230000960411     C                   MOVEL     'Trm'         V2DFT1
230100960605     C                   ELSE
230200960605     C     ANMFT1        IFEQ      'R'
230300960605     C                   MOVEL     'Ric'         V2DFT1
230400960411     C                   END
230500000904    3C                   END
230600000904    3C     ANMFT2        IFEQ      'T'
230700960411     C                   MOVEL     'Trm'         V2DFT2
230800960605     C                   ELSE
230900960605     C     ANMFT2        IFEQ      'R'
231000960605     C                   MOVEL     'Ric'         V2DFT2
231100960605     C                   END
231200000904    3C                   END
231300000904    2C                   END
231400000904     C*
231500000904     C                   CLEAR                   V2DDTR
231600000904     C                   CLEAR                   V2CDTR
231700000824     C*
231800000904     C**         ANMDTR    IFNE 0
231900000904     C**         ANMFT1    ANDNE' '
232000000904     C**         ANMDTR    ORNE 0
232100000904     C**         ANMAIE    ANDEQ'T'
232200000831     C*
232300000904     C     ANMFT1        IFEQ      'R'
232400000904     C                   MOVEL     WRIC          V2DDTR
232500000904     C                   ELSE
232600000904     C* O E' GIA' TRASMESSO O SFLAGGATO IN ATTESA DI TRASMISSIONE
232700000904     C                   MOVEL     WTRA          V2DDTR
232800000824     C                   ENDIF
232900000904     C*
233000000904    2C     ANMDTR        IFGT      0
233100000904    3C     ANMFT1        IFNE      ' '
233200000904     C     ANMAIE        OREQ      'T'
233300000824     C                   MOVEL     ANMDTR        WDT04A            4
233400000824     C                   MOVEL     ANMDTR        WDT06X            6
233500000824     C                   MOVE      WDT06X        WDT02M            2
233600000824     C                   MOVE      ANMDTR        WDT02G            2
233700000824     C                   MOVE      WDT10D        V2CDTR
233800000904    3C                   ENDIF
233900000904    2C                   ENDIF
234000000904     C**                   ELSE
234100000904     C**                   CLEARV2DDTR
234200000904     C**                   CLEARV2CDTR
234300000904     C**                   ENDIF
234400960403     C*****************
234500960403     C* EMISSIONE VIDEO
234600960403     C*****************
234700960403     C                   WRITE     LR44D01
234800960403     C                   EXFMT     LR44D02
234900960403     C*
235000960605    1C                   END
235100960402     C                   ENDSR
235200960328     C***********  AGGIORNAMENTO FILE ANOMALIE  ***********************
235300960328     C     AGGANM        BEGSR
235400960329     C                   READC     LR44S01                                29
235500960405    1C     *IN29         DOWEQ     *OFF
235600960419     C                   SETOFF                                       47
235700960412    2C     V1SVAR        IFNE      *BLANKS
235800960412     C     V1HVAR        ORNE      *BLANKS
235900990512     C     V1HNRR        CHAIN     FNANM00F                           30
236000960405    3C     *IN30         IFEQ      *OFF
236100041125     C**                 MOVE      *BLANKS       WALC              1
236200960405    4C                   SELECT
236300960419     C* GESTIONE ANNULLAMENTO ANOMALIA
236400960412     C     V1SVAR        WHENEQ    'A'
236500960514     C*
236600041124    5C**   V1HAPA        IFEQ      'P'
236700041124     C**   ANMAIE        ANDNE     'I'
236800041124     C**                 EXSR      SRCAEX
236900041124    5C**                 END
237000960514     C****
237100960514     C* Se chiudo anomalia 105-Manca record bolla partenza chiudo anche
237200960514     C* le anomalie 5 o 10 e viceversa
237300960514     C*
237400041125    5C     ANMCAA        IFEQ      5
237500960514     C     ANMCAA        OREQ      10
237600960514     C     ANMCAA        OREQ      105
237700960514     C                   EXSR      CHI105
237800041125    5C                   END
237900960514     C***
238000960419     C* Solo se riuscita l'allocazione del file di trasmissione:
238100041125    5C**   WALC          IFEQ      *BLANKS
238200041125     C**   WALC          OREQ      'S'
238300960419     C                   CLEAR                   DSLR33
238400960419     C* N.B.:Richiamo pgm chiusura anomalia senza sflaggare ft1/ft2
238500960419     C*      dando per scontato che le anomalie arrivo possono essere
238600960419     C*      annullate solo se non ancora trasmesse. Provvederò invece
238700960419     C*      alla scrittura del file di trasmissione a fronte di
238800960419     C*      annull/ripr di anomalie partenza
238900041125     c* 25/11/04: non inviamo più le anomalie ma subito annullate o ripristi
239000041125     c*           nate tutte . non importa quindi lo stesso sflaggare
239100960419     C                   MOVE      'N'           D33FTR
239200960419     C                   Z-ADD     V1HNRR        D33NRR
239300960419     C                   MOVE      'AN'          D33CCH
239400960419     C                   MOVE      'M'           D33FSC
239500990513     C                   Z-ADD     DATEU8        D33DCH
239600960514     C                   MOVEL     'S'           D33MAC
239700990512     C                   UNLOCK    FNANM00F
239800960419     C                   CALL      'FNLR33R'
239900960419     C                   PARM                    DSLR33
240000990512     C     V1HNRR        CHAIN(N)  FNANM00F                           30
240100960419     C   30              MOVE      'A'           V1SATB
240200960419     C  N30              EXSR      RIECHI
240300960419     C* Scrivo file di trasmissione
240400041125    6C**   WALC          IFEQ      'S'
240500041125     C**                 EXSR      TRMANM
240600041125    6C**                 END
240700041125    5C**                 END
240800041124     c
240900041124     c
241000960419     C* GESTIONE RIPRISTINO ANOMALIA
241100960412     C     V1SVAR        WHENEQ    'R'
241200041125    5C**   V1HAPA        IFEQ      'P'
241300041125     C**   ANMAIE        ANDNE     'I'
241400041125     C**                 EXSR      SRCAEX
241500041125    5C**                 END
241600960513     C* Se ripristino anomalia 105-Manca record bolla partenza
241700960513     C* ripristino anche le anomalie 5 o 10 e viceversa
241800041125     C**   WALC          IFEQ      'S'
241900041125     C**   WALC          OREQ      *BLANKS
242000960513     C     ANMCAA        IFEQ      5
242100960513     C     ANMCAA        OREQ      10
242200960513     C     ANMCAA        OREQ      105
242300960513     C                   EXSR      RIP105
242400960513     C                   END
242500041125     C**                 END
242600960419     C* Solo se riuscita l'allocazione del file di trasmissione:
242700041125    5C**   WALC          IFEQ      *BLANKS
242800041125     C**   WALC          OREQ      'S'
242900960419    6C     V1SATB        IFEQ      'A'
243000960419     C                   MOVE      *BLANKS       V1SATB
243100960329     C                   MOVE      *BLANKS       ANMATB
243200960419     C                   ELSE
243300960426     C* Clearo campi se anomalia non ha raggiunto il livello max
243400960508     C* N.B.:Se v1slid = blanks il codice operativo move sposta 0
243500960508     C*      in w0010 per cui non ci sono problemi
243600960507     C                   MOVE      V1SLID        W0010             1 0
243700960426     C     W0010         IFLT      WLCI
243800960419     C                   CLEAR                   ANMFSC
243900960419     C                   CLEAR                   ANMDCH
244000960419     C                   CLEAR                   ANMCCH
244100960419     C                   CLEAR                   V1DFSC
244200960426     C                   CLEAR                   V1SDCH
244300960419     C                   CLEAR                   V1SCCH
244400960426     C                   ELSE
244500960426     C* Altrimenti imposto chiusura in pratica Idd
244600960426     C                   MOVEL     'PR'          ANMCCH
244700990513     C                   MOVE      DATEU8        ANMDCH
244800960426     C                   MOVEL     'D'           ANMFSC
244900971118     C                   MOVE      DTGIO6        V1SDCH
245000960426     C                   MOVEL     'PR'          V1SCCH
245100960426     C                   MOVEL     '7D'          KCOD
245200960426     C                   MOVEL(P)  'D'           KKEY
245300960426     C     KTAB          CHAIN     TABEL00F                           30
245400960426     C  N30              MOVEL     TBLUNI        DS7D
245500960426     C  N30              MOVEL     §7DDES        V1DFSC
245600960426     C   30              CLEAR                   V1DFSC
245700960426     C                   END
245800960419    6C                   END
245900041125     C**   WALC          IFEQ      'S'
246000041125     C     WFEL1         IFGT      *ZEROS
246100960605     C     WFEL1         ANDNE     SIMFEL
246200960605     C                   MOVEL     'T'           ANMFT1
246300000824     C                   MOVE      DATEU8        ANMDTR
246400960605     C                   END
246500960605     C     WFEL2         IFGT      *ZEROS
246600960605     C     WFEL2         ANDNE     SIMFEL
246700960605     C     WFEL2         ANDNE     WFEL1
246800960605     C                   MOVEL     'T'           ANMFT2
246900000824     C                   MOVE      DATEU8        ANMDTR
247000960605     C                   END
247100041125     C**                 END
247200990512     C                   UPDATE    FNANM000
247300041125    6C**   WALC          IFEQ      'S'
247400041125     C**                 EXSR      TRMANM
247500041125    6C**                 END
247600041125    5C**                 END
247700960419     C* GESTIONE VARIAZIONE ANOMALIA
247800960412     C     V1HVAR        WHENEQ    'V'
247900960329     C                   EXSR      RISANM
248000990512     C                   UPDATE    FNANM000
248100960405    4C                   ENDSL
248200960405    3C                   END
248300960405    2C                   END
248400960402     C                   SETON                                        04
248500960405     C                   SETON                                        05
248600960405     C                   SETOFF                                       09
248700960402     C                   MOVE      *IN04         V1HIN4
248800960402     C                   MOVE      *IN05         V1HIN5
248900960405     C                   MOVE      *IN09         V1HIN9
249000041125     C                   CLEAR                   V1SVAR
249100041125     c
249200960412     C                   CLEAR                   V1HVAR
249300960523     C                   MOVE      V1HI51        *IN51
249400960523     C                   MOVE      V1HI52        *IN52
249500960329     C                   UPDATE    LR44S01
249600960329     C                   READC     LR44S01                                29
249700960405    1C                   ENDDO
249800000224     C*
249900960329     C                   ENDSR
250000960328     C***************  SCRITTURA FILE ANOMALIE   **********************
250100960328     C     WTRANM        BEGSR
250200960329     C                   READC     LR44S01                                29
250300960329     C     *IN29         DOWEQ     *OFF
250400960402     C     V1SCAA        IFNE      *BLANKS
250500960405     C     V1SCAA        ANDNE     *ZEROS
250600960402     C                   MOVE      V1SCAA        WCAA
250700990512     C                   CLEAR                   FNANM000
250800960329     C                   EXSR      RISANM
250900990512     C                   WRITE     FNANM000
251000960329     C                   END
251100960329     C                   READC     LR44S01                                29
251200960329     C                   ENDDO
251300960329     C   10              MOVEL     'S'           D34RIL
251400960328     C                   ENDSR
251500960329     C********  RIEMPIMENTO CAMPI FILE ANOMALIE  **********************
251600960329     C     RISANM        BEGSR
251700960329     C                   MOVE      V1SNSP        ANMNSP
251800960329     C                   MOVE      V1SSCN        ANMSCN
251900960401     C                   MOVE      V1SFLS        ANMFLS
252000960329     C                   MOVE      V1SLNP        ANMLNP
252100990524     C                   MOVE      V1SAAS        ANMAAS
252200000225     C     ANMNSP        IFGT      0
252300990513     C     V1SAAS        IFGT      60
252400990513     C                   MOVEL     19            ANMAAS
252500990513     C                   ELSE
252600990513     C                   MOVEL     20            ANMAAS
252700990513     C                   END
252800990524     C                   END
252900960329     C                   MOVE      V1SLNA        ANMLNA
253000960329     C                   MOVE      V1SNRS        ANMNRS
253100960329     C                   MOVEL     V1HTAN        ANMTAN
253200960329     C                   MOVEL     V1HSAN        ANMSAN
253300960329     C                   MOVE      V1SCAA        ANMCAA
253400960329     C                   MOVEL     V1HFSA        ANMFSA
253500960329     C                   MOVE      V1HDAO        ANMDAO
253600960507     C                   MOVEL     V1HAIE        ANMAIE
253700960329     C                   MOVE      V1SLID        ANMLID
253800960411     C                   MOVE      D34FGS        ANMFLE
253900960404     C     V1SLNP        IFGT      *ZEROS
254000960404     C                   MOVE      V1SLNP        ANMFL1
254100960404     C                   ELSE
254200960404     C                   MOVE      V1SFLS        ANMFL1
254300960404     C                   END
254400960404     C                   CLEAR                   ANMFT1
254500960404     C                   CLEAR                   ANMFT2
254600000824     C                   CLEAR                   ANMDTR
254700140523     c* imposto in anmflo data e ora creazione spunta
254800140523     C                   TIME                    TIMEanm          14 0
254900140523     c                   movel     timeanm       wora4             4 0
255000140523     C                   move      timeanm       G08DAT
255100140523     C                   Z-ADD     *ZERO         G08INV
255200140523     C                   MOVEL     *BLANKS       G08ERR
255300140523     C                   CALL      'XSRDA8'
255400140523     C                   PARM                    WLBDA8
255500140523     C                   move      G08INV        WDAT6             6 0
255600140523     c                   movel     wdat6         anmflo
255700140523     c                   move      wora4         anmflo
255800960329     C                   ENDSR
255900960513     C***  CHIUSURA ANOMALIA 5 O 10 PER ANOMALIA 105  E VICEVERSA  ****
256000960513     C     CHI105        BEGSR
256100041124     C*
256200041124     c* anm 105 - annullo la 10 o le 05
256300041124     c* anm  05 - annullo la 105 e altre 05
256400041124     c* anm  10 - annullo la 105
256500041124     c
256600041125     C                   eval      wcaaelab=anmcaa
256700041124     c                   eval      giri=1
256800041124     c
256900960513     C                   EXSR      LEGANM
257000041125     C
257100041125    0c                   dow       giri<=2
257200960513     C*
257300041124     C**                 MOVE      *BLANKS       WALC1             1
257400041124    1C     *IN30         doweq     *OFF
257500051006     c* Per le anom 05 escluse se stesso
257600051006     c                   if        wcaa=wcaaelab and anmnr2<>v1hnrr
257700041124     c*
257800041124    2C**   V1HAPA        IFEQ      'P'
257900041124     C**   ANMAIE        ANDNE     'I'
258000041124     C**                 EXSR      SRCAEX
258100041124     C**                 MOVE      WALC          WALC1
258200041124    2C**                 END
258300041124    2C**   WALC1         IFEQ      'S'
258400041124     C**   WALC1         OREQ      *BLANKS
258500041124     C*
258600960513     C                   CLEAR                   DSLR33
258700960513     C                   MOVE      'N'           D33FTR
258800960513     C                   Z-ADD     ANMNR2        D33NRR
258900960513     C                   MOVE      'AN'          D33CCH
259000960513     C                   MOVE      'M'           D33FSC
259100990513     C                   Z-ADD     DATEU8        D33DCH
259200960514     C* N.B.: in questo caso l'impostazione del flag d33mac è di poca
259300960514     C*       importanza visto che le anomalie di questa routine non
259400960514     C*       sono autochiuse. Lo imposto solo per renderlo uguale
259500960514     C*       all'altro richiamo di fnlr33r
259600960514     C                   MOVEL     'S'           D33MAC
259700960513     C                   CALL      'FNLR33R'
259800960513     C                   PARM                    DSLR33
259900960513     C* Scrivo file di trasmissione
260000041124    3C**   WALC1         IFEQ      'S'
260100041124     C**   ANMNR2        CHAIN(N)  FNANM00F                           30
260200041124     C**                 EXSR      TRMANM
260300041124     C**                 CLEAR                   WALC
260400041124    3C**                 END
260500041124    2C**                 END
260600051006     c                   endif
260700041124    c
260800041124     c
260900041124     c* Verifico se ci sono altre anomalie stesso segnacollo stesso
261000041124     c*  cod.anomalia da chiudere (sono poi solo le 005)
261100041124     C     KANM2         reade     FNANM02L                               30
261200041124    1C                   ENDdo
261300041124     c                   eval      giri=giri+1
261400041124     c
261500041124     c* Per anom 10  e 105 esco
261600041124    1c                   if        wcaaelab<>05
261700041124     c                   eval      giri=3
261800041124     c                   else
261900041124     c* Per anomalia 05 leggo altre 05 se presenti, escludo record in
262000041124     c*  annullamento
262100041124     C                   Z-ADD     05            WCAA
262200041124     C     KANM2         CHAIN     FNANM02L                           30
262300041124    2c                   dow       not *in30 and anmnr2=v1hnrr
262400041124     C     KANM2         reade     FNANM02L                               30
262500041124    2c                   enddo
262600041124     c
262700041124    1c                   endif
262800041124    0c                   enddo
262900960513     C                   ENDSR
263000960513     C***  RIPRISTINO ANOMALIA 5 O 15 PER ANOMALIA 105 E VICEVERSA ****
263100960513     C     RIP105        BEGSR
263200041124     c* anm 105 - annullo la 10 o le 05
263300041124     c* anm  05 - annullo la 105 e altre 05
263400041124     c* anm  10 - annullo la 105
263500041124     c
263600041124     c                   eval      wcaaelab=anmcaa
263700041124     c                   eval      giri=1
263800041124     c
263900960513     C                   EXSR      LEGANM
264000041125     c
264100041125    0c                   dow       giri<=2
264200960513     C*
264300041125     C***                MOVE      *BLANKS       WALC1             1
264400041125    1C     *IN30         doweq     *OFF
264500960513     C*
264600041124    2C**   V1HAPA        IFEQ      'P'
264700041124     C**   ANMAIE        ANDNE     'I'
264800041124     C**                 EXSR      SRCAEX
264900041124     C**                 MOVE      WALC          WALC1
265000041124    2C**                 END
265100960513     C*
265200041124    2C**   WALC1         IFEQ      'S'
265300041124     C**   WALC1         OREQ      *BLANKS
265400990512     C     ANMNR2        CHAIN     FNANM00F                           30
265500041125    3C     *IN30         IFEQ      *OFF
265600041125    4C     ANMLID        IFLT      WLCI
265700960513     C                   CLEAR                   ANMFSC
265800960513     C                   CLEAR                   ANMDCH
265900960513     C                   CLEAR                   ANMCCH
266000960513     C                   ELSE
266100960513     C* Altrimenti imposto chiusura in pratica Idd
266200960513     C                   MOVEL     'PR'          ANMCCH
266300990513     C                   MOVE      DATEU8        ANMDCH
266400960513     C                   MOVEL     'D'           ANMFSC
266500041125    4C                   END
266600041125    4C**   WALC1         IFEQ      'S'
266700041125    5C     WFEL1         IFGT      *ZEROS
266800960605     C                   MOVEL     'T'           ANMFT1
266900000824     C                   MOVE      DATEU8        ANMDTR
267000041125    5C                   END
267100041125    5C     WFEL2         IFGT      *ZEROS
267200960605     C     WFEL2         ANDNE     WFEL1
267300960605     C                   MOVEL     'T'           ANMFT2
267400000824     C                   MOVE      DATEU8        ANMDTR
267500041125    5C                   END
267600041125    4C**                 END
267700990512     C                   UPDATE    FNANM000
267800041125    3C                   END
267900041125    2C**                 END
268000041125     c
268100041125     c* Verifico se ci sono altre anomalie stesso segnacollo stesso
268200041125     c*  cod.anomalia da chiudere (sono poi solo le 005)
268300041125     C     KANM2         reade     FNANM02L                               30
268400041125    1C                   ENDdo
268500041125     c                   eval      giri=giri+1
268600041125     c
268700041125     c* Per anom 10  e 105 esco
268800041125    1c                   if        wcaaelab<>05
268900041125     c                   eval      giri=3
269000041125     c                   else
269100041125     c* Per anomalia 05 leggo altre 05 se presenti, escludo record in
269200041125     c*  annullamento
269300041125     C                   Z-ADD     05            WCAA
269400041125     C     KANM2         CHAIN     FNANM02L                           30
269500041125    2c                   dow       not *in30 and anmnr2=v1hnrr
269600041125     C     KANM2         reade     FNANM02L                               30
269700041125    2c                   enddo
269800041125     c
269900041125    1c                   endif
270000041125    0c                   enddo
270100960513     C*
270200041125     C**   WALC1         IFEQ      'S'
270300041125     C**                 EXSR      TRMANM
270400041125     C**                 CLEAR                   WALC
270500041125     C**                 END
270600960513     C* RICHAINO RECORD SCELTO A VIDEO
270700041125     C**   WALC1         IFNE      'N'
270800990512     C     V1HNRR        CHAIN     FNANM00F                           30
270900041125     C**                 END
271000960513     C*
271100041125    1C**                 END
271200960513     C                   ENDSR
271300960513     C**************  LETTURA ANOMALIA 5/10/105   *********************
271400960513     C     LEGANM        BEGSR
271500960513    1C                   SELECT
271600960513     C     ANMCAA        WHENEQ    105
271700960514     C                   Z-ADD     10            WCAA
271800990512     C     KANM2         CHAIN     FNANM02L                           30
271900960514     C   30              Z-ADD     5             WCAA
272000990512     C   30KANM2         CHAIN     FNANM02L                           30
272100960513     C     ANMCAA        WHENEQ    5
272200960513     C     ANMCAA        OREQ      10
272300960513     C                   Z-ADD     105           WCAA
272400990512     C     KANM2         CHAIN     FNANM02L                           30
272500960513    1C                   ENDSL
272600960513     C                   ENDSR
272700970214     C**********  GESTIONE VIDEATA MANUTENZIONE ANOMALIA 300  *********
272800970214     C     WDW04         BEGSR
272900970214     C* La causale anomalia la imposto sempre = a 300
273000970214     C                   Z-ADD     300           W4SCAA
273100970214     C                   MOVEL     '7C'          KCOD
273200970214     C                   MOVEL(P)  300           KKEY
273300970214     C     KTAB          CHAIN     TABEL00F                           30
273400970214     C  N30              MOVEL     TBLUNI        DS7C
273500970214     C   30              CLEAR                   DS7C
273600970214     C                   MOVEL     §7CDES        W4DCAA
273700970214     C* Spengo indicatori del video
273800970214     C                   SETOFF                                       282555
273900970217     C                   SETOFF                                       5654
274000970214     C* 10 OFF --> MANUTENZIONE
274100970214     C     *IN10         IFEQ      *OFF
274200970214     C* Imposto la lnp
274300970214     C                   MOVE      ANMLNP        W4SLNP
274400970214     C* Imposto il numero dei ritiri
274500970214     C                   MOVE      ANMSCN        W4SNRT
274600970214     C* imposto la data anomalia
274700990513     C                   CLEAR                   WLBDA8
274800990513     C                   MOVE      ANMDAO        G08INV
274900990513     C                   MOVEL     '3'           G08ERR
275000990513     C                   CALL      'XSRDA8'
275100990513     C                   PARM                    WLBDA8
275200990513     C                   MOVE      G08DAT        W4SDAO
275300970214     C                   ELSE
275400970214     C* 10 ON --> INSERIMENTO
275500970214     C                   MOVE      D34FGS        W4SLNP
275600990513     C                   Z-ADD     WDTGIO        W4SDAO
275700970214     C                   MOVE      *ZEROS        W4SNRT
275800970214     C                   END
275900970214     C**
276000970214     C* EMISSIONE WINDOW
276100970214     C**
276200970214     C     EMWDW4        TAG
276300970214     C                   EXFMT     LR44W04
276400970214     C                   SETOFF                                       28
276500970214     C     *INKL         CABEQ     *ON           EWDW04
276600970214     C                   EXSR      CTRWDW
276700970214     C   28
276800970214     CORNKF              GOTO      EMWDW4
276900970214     C* CONFERMO DATI
277000970214     C     *IN10         IFEQ      *OFF
277100970214     C                   EXSR      AGG300
277200970214     C                   ELSE
277300970214     C                   EXSR      WTR300
277400970214     C                   END
277500970217     C*
277600970217     C     EWDW04        TAG
277700970217     C                   MOVE      *BLANKS       V1SVAR
277800970214     C*
277900970217     C                   ENDSR
278000970214     C**********  CONTROLLI WINDOW MANUTENZIONE ANOMALIA 300  *********
278100970214     C     CTRWDW        BEGSR
278200970214     C                   SETOFF                                       545556
278300970214     C*
278400970214    1C     *IN10         IFEQ      *ON
278500970214     C* LNP NON IMMESSA
278600970214    2C     W4SLNP        IFEQ      *ZEROS
278700970214     C                   SETON                                        5428
278800970214     C                   MOVEL     MSG(3)        W4ZMSG
278900970214     C                   GOTO      ENDCTW
279000970214    2C                   END
279100970214     C* LA LNP DEVE ESSERE UNA FILIALE IN GESTIONE
279200970214    2C     W4SLNP        IFNE      D34FGS
279300970214     C     W4SLNP        LOOKUP    L6S                                    30
279400970214    3C     *IN30         IFEQ      *OFF
279500970214     C                   MOVEL     MSG(8)        W4ZMSG
279600970214     C                   SETON                                        5428
279700970214     C                   GOTO      ENDCTW
279800970214    3C                   END
279900970214    2C                   END
280000970214    1C                   END
280100970214     C*
280200970214     C* ERRORE SE NON IMMESSO IL NUMERO RITIRI EFFETTUATI
280300970304    1C*          W4SNRT    IFEQ *ZEROS
280400970304     C*                    MOVELMSG,38    W4ZMSG
280500970304     C*                    SETON                     5528
280600970304     C*                    GOTO ENDCTW
280700970304    1C*                    END
280800970214     C*
280900970214    1C     *IN10         IFEQ      *ON
281000970214     C* ERRORE SE DATA ANOMALIA = 0
281100970214    2C     W4SDAO        IFEQ      *ZEROS
281200970214     C                   MOVEL     MSG(9)        W4ZMSG
281300970214     C                   SETON                                        5628
281400970214     C                   GOTO      ENDCTW
281500970214    2C                   END
281600970214     C* CONTROLLO FORMALE DATA ANOMALIA
281700990513     C                   CLEAR                   WLBDA8
281800990513     C                   Z-ADD     W4SDAO        G08DAT
281900990513     C                   CALL      'XSRDA8'
282000990513     C                   PARM                    WLBDA8
282100990513    2C     G08ERR        IFEQ      '1'
282200970214     C                   MOVE      *ZEROS        W4SINV
282300970214     C                   MOVEL     MSG(9)        W4ZMSG
282400970214     C                   SETON                                        5628
282500970214     C                   GOTO      ENDCTW
282600970214     C                   ELSE
282700990513     C                   MOVE      G08INV        W4SINV
282800990513     C                   MOVE      G08DAT        W4SDAO
282900970214    2C                   END
283000970214     C* ERRORE SE CHIAVE GIA' ESISTENTE
283100990513     C     KANM4         SETLL     FNANM04L
283200990512     C     KANM4         READE     FNANM04L                               30
283300970214     C     *IN30         DOWEQ     *OFF
283400970214     C     ANMLNP        IFEQ      W4SLNP
283500970214     C                   MOVEL     MSG(32)       W4ZMSG
283600970214     C                   SETON                                        2854
283700970214     C                   GOTO      ENDCTW
283800970214     C                   END
283900990512     C     KANM4         READE     FNANM04L                               30
284000970214     C                   ENDDO
284100970214    1C                   END
284200970214     C     ENDCTW        ENDSR
284300970214     C**************  AGGIORNAMENTO ANOMALIA 300 **********************
284400970214     C     AGG300        BEGSR
284500990512     C     V1HNRR        CHAIN     FNANM00F                           30
284600970214     C     *IN30         IFEQ      *OFF
284700970214     C                   Z-ADD     W4SNRT        ANMSCN
284800990512     C                   UPDATE    FNANM000
284900970214     C                   Z-ADD     W4SNRT        V1SSCN
285000970214     C                   END
285100970214     C                   ENDSR
285200970214     C**************  SCRITTURA ANOMALIA 300     **********************
285300970214     C     WTR300        BEGSR
285400990512     C                   CLEAR                   FNANM000
285500970214     C                   Z-ADD     W4SLNP        ANMLNP
285600970214     C                   Z-ADD     W4SNRT        ANMSCN
285700970214     C                   Z-ADD     W4SINV        ANMDAO
285800970214     C                   Z-ADD     W4SINV        ANMDCH
285900970214     C                   Z-ADD     300           ANMCAA
286000970214     C                   MOVE      D34FGS        ANMFLE
286100970214     C                   MOVE      §7CTAN        ANMTAN
286200970214     C                   MOVE      §7CSAN        ANMSAN
286300970214     C                   MOVE      §7CAIE        ANMAIE
286400970214     C                   MOVEL     '1'           ANMFSA
286500970214     C                   MOVEL     '1'           ANMFSC
286600140523     c* imposto in anmflo data e ora creazione spunta
286700140523     C                   TIME                    TIMEanm          14 0
286800140523     c                   movel     timeanm       wora4             4 0
286900140523     C                   move      timeanm       G08DAT
287000140523     C                   Z-ADD     *ZERO         G08INV
287100140523     C                   MOVEL     *BLANKS       G08ERR
287200140523     C                   CALL      'XSRDA8'
287300140523     C                   PARM                    WLBDA8
287400140523     C                   move      G08INV        WDAT6             6 0
287500140523     c                   movel     wdat6         anmflo
287600140523     c                   move      wora4         anmflo
287700990512     C                   WRITE     FNANM000
287800970214     C                   ENDSR
287900151116     C**********************************************************************
288000151116     C*  FIBSP00F - lnp/clienti con sospensione bolle
288100151116     C**********************************************************************
288200151116     c     sr_fibsp      begsr
288300151116     c                   clear                   w_SiBsp
288400151116    2c                   if        blpccm>0
288500151116     c                   eval      w_clibo=blpccm
288600151116     c                   else
288700151116     c                   eval      w_clibo=blpksc
288800151116    2c                   endif
288900151116     c     w_clibo       chain     fibsp02l
289000151116     c                   if        %found(fibsp02l) and bspfpa='S'
289100151214     c                                              and bspaut=*blanks
289200151116     C                   eval      w_SiBsp='S'
289300151116     c                   else
289400151116     c     blplnp        chain     fibsp01l
289500151116     c                   if        %found(fibsp01l) and bspfpa='S'
289600151214     c                                              and bspaut=*blanks
289700151116     C                   eval      w_SiBsp='S'
289800151116     c                   endif
289900151116     c                   endif
290000151116     c                   endsr
290100960326     C**************  OPERAZIONI INIZIALI  ****************************
290200960326     C     *INZSR        BEGSR
290300960326     C*
290400960326     C                   Z-ADD     1             CODUT
290500960326     C                   CALL      'X§PARUT'
290600960326     C                   PARM                    UT§DSE
290700960326     C                   MOVEL     RAGUT         V1DRSU
290800960329     C                   MOVEL     REC80         CNCR80
290900960327     C*
291000960327     C     *LIKE         DEFINE    NRR1          NRRAL
291100960327     C     *LIKE         DEFINE    NRR1          NRRSAV
291200960327     C     *LIKE         DEFINE    V1CNBR        W1CNBR
291300960328     C     *LIKE         DEFINE    TBLCOD        KCOD
291400960328     C     *LIKE         DEFINE    TBLKEY        KKEY
291500960329     C     *LIKE         DEFINE    D34FGS        SAVFGS
291600960329     C     *LIKE         DEFINE    V1SCAA        DUPCAA
291700960329     C     *LIKE         DEFINE    V1SAAS        DUPAAS
291800960401     C     *LIKE         DEFINE    V1SLNP        DUPLNP
291900960401     C     *LIKE         DEFINE    V1SNRS        DUPNRS
292000960401     C     *LIKE         DEFINE    V1SNSP        DUPNSP
292100960401     C     *LIKE         DEFINE    V1SFLS        DUPFLS
292200960401     C     *LIKE         DEFINE    V1SSCN        DUPSCN
292300960401     C     *LIKE         DEFINE    V1SDAO        DUPDAO
292400960401     C     *LIKE         DEFINE    V1SLNA        DUPLNA
292500971118     C     *LIKE         DEFINE    G08INV        DATEU8
292600960401     C     *LIKE         DEFINE    ANMCAA        WCAA
292700960506     C     *LIKE         DEFINE    ANMFLE        WFLE
292800960402     C     *LIKE         DEFINE    PA1AAS        WAAS
292900960403     C     *LIKE         DEFINE    §7ALCI        WLCI
293000971118     C     *LIKE         DEFINE    D55TFP        WFEL1
293100971118     C     *LIKE         DEFINE    D55TFP        WFEL2
293200041125     C***  *LIKE         DEFINE    D55TFP        SAVOVR
293300960521     C     *LIKE         DEFINE    D44FIL        W44FIL
293400960521     C     *LIKE         DEFINE    D44LNA        W44LNA
293500960521     C     *LIKE         DEFINE    D44NRS        W44NRS
293600960521     C     *LIKE         DEFINE    D44SCN        W44SCN
293700960605     C     *LIKE         DEFINE    ANMLNP        WLNP
293800960605     C     *LIKE         DEFINE    ANMNRS        WNRS
293900960605     C     *LIKE         DEFINE    ANMNSP        WNSP
294000960830     C     *LIKE         DEFINE    ANMFLS        SAVFLS
294100960830     C     *LIKE         DEFINE    ANMSCN        SAVSCN
294200961210     C     *LIKE         DEFINE    DSANM         DSANMX
294300990513     C     *LIKE         DEFINE    W4SDAO        W4SINV
294400970507     C     *LIKE         DEFINE    AR4TRC        WTRC
294500970507     C                   MOVEL     'A'           WTRC
294600960328     C*
294700040531     C     KTAB          klist
294800960328     C                   KFLD                    CODUT
294900960328     C                   KFLD                    KCOD
295000960328     C                   KFLD                    KKEY
295100960429     C*
295200960429     C     KBLP          KLIST
295300960429     C                   KFLD                    WAAS
295400960429     C                   KFLD                    V1SLNP
295500960429     C                   KFLD                    V1SNRS
295600960429     C                   KFLD                    V1SNSP
295700970507     C     KAR4          KLIST
295800970507     C                   KFLD                    WAAS
295900970507     C                   KFLD                    V1SLNP
296000970507     C                   KFLD                    V1SNRS
296100970507     C                   KFLD                    V1SNSP
296200970507     C                   KFLD                    WTRC
296300960605     C*
296400960605     C     KBLP2         KLIST
296500960605     C                   KFLD                    WAAS
296600960605     C                   KFLD                    WLNP
296700960605     C                   KFLD                    WNRS
296800960605     C                   KFLD                    WNSP
296900960429     C*
297000960429     C     KBLT          KLIST
297100960429     C                   KFLD                    V1SFLS
297200960429     C                   KFLD                    V1SLNA
297300960429     C                   KFLD                    V1SNRS
297400960429     C                   KFLD                    V1SSCN
297500960605     C*
297600960605     C     KBLT2         KLIST
297700960605     C                   KFLD                    ANMFLS
297800960605     C                   KFLD                    ANMLNA
297900960605     C                   KFLD                    ANMNRS
298000960605     C                   KFLD                    ANMSCN
298100960401     C*
298200960401     C     KANM1         KLIST
298300990513     C                   KFLD                    WAAS
298400960401     C                   KFLD                    V1SLNP
298500960401     C                   KFLD                    V1SNRS
298600960401     C                   KFLD                    V1SNSP
298700960401     C                   KFLD                    WCAA
298800960401     C*
298900960401     C     KANM2         KLIST
299000960401     C                   KFLD                    V1SFLS
299100960401     C                   KFLD                    V1SLNA
299200960401     C                   KFLD                    V1SNRS
299300960401     C                   KFLD                    V1SSCN
299400960401     C                   KFLD                    WCAA
299500970214     C*
299600970214     C     KANM4         KLIST
299700970214     C                   KFLD                    W4SCAA
299800970214     C                   KFLD                    W4SINV
299900960521     C*
300000960521     C     KANM01        KLIST
300100990513     C                   KFLD                    D44AAS
300200960521     C                   KFLD                    D44FIL
300300960521     C                   KFLD                    D44NRS
300400960521     C                   KFLD                    D44NSP
300500040531     C     Kblp27        KLIST
300600040531     C                   KFLD                    D27AAS
300700040531     C                   KFLD                    D27lnp
300800040531     C                   KFLD                    D27NRS
300900040531     C                   KFLD                    D27NSP
301000960522     C*
301100960522     C     KART01        KLIST
301200960522     C                   KFLD                    D44AAS
301300960522     C                   KFLD                    D44FIL
301400960522     C                   KFLD                    D44NRS
301500960522     C                   KFLD                    D44NSP
301600960521     C*
301700960521     C     KANM02        KLIST
301800960521     C                   KFLD                    W44FIL
301900960521     C                   KFLD                    W44LNA
302000960521     C                   KFLD                    W44NRS
302100960521     C                   KFLD                    W44SCN
302200960326     C*
302300971118     C* DETERMINO DATA DEL GIORNO
302400971118     C*
302500971118     C                   TIME                    W0140            14 0
302600971118     C                   MOVE      W0140         WDTGIO            8 0
302700971118     C                   CLEAR                   WLBDA8
302800971118     C                   Z-ADD     WDTGIO        G08DAT
302900971118     C                   CALL      'XSRDA8'
303000971118     C                   PARM                    WLBDA8
303100971118     C                   MOVE      G08INV        DATEU8
303200971118     C                   MOVEL     WDTGIO        DTGIO6            6 0
303300971118     C                   MOVE      WDTGIO        W0020             2 0
303400971118     C                   MOVE      W0020         DTGIO6
303500960401     C*
303600960329     C                   CLEAR                   SAVFGS
303700960327     C                   EXSR      PULSFL
303800960403     C*  Reperisco livello di chiusura anomalia come IDD
303900960403     C                   MOVEL     '7A'          KCOD
304000960403     C                   MOVEL(P)  '2'           KKEY
304100960403     C     KTAB          CHAIN     TABEL00F                           30
304200960403     C  N30              MOVEL     TBLUNI        DS7A2
304300960403     C   30              CLEAR                   DS7A2
304400960403     C                   MOVE      §7ALCI        WLCI
304500960423     C*
304600021008     C                   MOVE      ')'           VAR               8
304700021008     C                   MOVE      '))'          VAR2              9
304800960521     C*
304900960521     C                   SETOFF                                       9192
305000960523     C                   SETOFF                                       5152
305100960605     C                   CLEAR                   WOPNAR
305200960605     C                   CLEAR                   WOPNBL
305300040615     C***                CLEAR                   WOPNBT
305400000824     C                   MOVE      '/'           WTRAT1
305500000824     C                   MOVE      '/'           WTRAT2
305600960327     C*
305700990513     C                   ENDSR
305800990512** MSG
305900960326Fine scorrimento                                                              01
306000960506Immettere Codice Anomalia valido                                              02
306100960401Immettere Linea di Partenza valida                                            03
306200970507Immettere linea  Segnacollo valida                                            04
306300960506Non è ammesso l'inserimento di anomalie con questo codice                     05
306400960401Immettere Numero Spedizione                                                   06
306500960401Immettere Linea di Arrivo valida                                              07
306600970507Immettere un punto operativo in gestione                                      08
306700960401Immettere Data Apertura anomalia valida                                       09
306800151113Immettere linea di partenza diversa dalla filiale in gestione                 10
306900960401Non ammessa la manutenzione o anomalia già trasmessa                          11
307000960329Anomalia non annullabile                                                      12
307100960329Anomalia già annullata                                                        13
307200960329Anomalia non ripristinabile                                                   14
307300960329Anomalia già ripristinata                                                     15
307400960329Anomalia non manutenzionabile                                                 16
307500960401Immettere Numero Segnacollo                                                   17
307600960401Presente anomalia ancora aperta                                               18
307700970507Se immessa la linea segnacollo immettere anche il numero                      19
307800960401Anomalia già aperta con data > alla data apertura immessa                     20
307900970507Se immesso il numero segnacollo immettere anche la linea                      21
308000960403Anomalia già chiusa in fase IDD                                               22
308100960402Data anomalia > di data odierna. F6 per forzare.                              23
308200000225Se immesso il numero spedizione immettere anche la linea di partenza          24
308300960402Se immesso l'anno e la linea di partenza immettere anche il n.spedizione      25
308400960405Non è possibile inserire più di una volta la stessa anomalia                  26
308500960409Non è possibile,per la stessa bolla,inserire sia anomalia 070 che 071.        27
308600960506Per questa bolla esiste già anomalia XXX-123456789012345678901234 F6 per forz.28
308700960506Per questa bolla esiste già anomalia XXX-123456789012345678901234             29
308800960412Questa anomalia non verrà variata in quanto trasmessa/chiusa.F6 per continuare30
308900960412Anomalia non più esistente                                                    31
309000960418Anomalia già esistente                                                        32
309100960419Alcune anomalie non aggiornate: attendere 1 minuto e riprovare                33
309200960429Impossibile annullare: presente spedizione                                    34
309300960506Per la bolla esiste già anomalia XXX-1234567890123 immessa da xxx.F6 per forz.35
309400970507Immettere linea di arrivo diversa da punto operativo in gestione              36
309500960830Per questa bolla e causale anomalia immettere Fil./N.Segnacollo = xxx/xxxxxxx 37
309600970214Immettere il numero dei ritiri effettuati                                     38
309700970507La spedizione non esiste nell'archivio bolle arrivi: F6 per forzare           39
309800970507Impossibile inserire un MANCA DDT per una spedizione che e' NO DDT
309900151116La spedizione non esiste nell'archivio bolle partenze                         41
310000151116Spedizione già partita OPPURE chiusa in partenza                              42
310100151116Sped. di cliente per cui non è prevista la sospensione autom. in chius. F.V.P.43
310200151116Anomalia non gestibile in arrivo                                              44
310300151116Anomalia non gestibile in partenza                                            45
310400160203Anomalia già presente: riaprire quella esistente se chiusa                    46
310500960328** Z01
310600960422F6=CONFERMA F10=Inserimento F24=AlTasti
310700960403F7=Bolla F14=Spunte F3=Fine F24=AlTasti
310800960422F12=Ritorno F24=AlTasti
310900960403** Z02
311000960403F7=Bolla F14=Spunte F3=Fine F12=Ritorno
311100960409F3=Fine  F10=Inserimento  F12=Ritorno
311200960403F6=CONFERMA F3=Fine F12=Ritorno
311300960521F3=Fine  F12=Ritorno
311400960521F14=Spunte F3=Fine F12=Ritorno
311500960521F7=Bolla  F3=Fine F12=Ritorno
311600960422** Z03
311700960422F6=CONFERMA F7=Bolla F3=Fine F24=AlTasti
311800960422F14=Spunte F12=Ritorno F24=AlTasti
311900960403** TAB/TABDES
312000960403IINTERNA
312100960507DI.D.D.
312200960507NNON IDD
312300961118XDISCOR.EXP
