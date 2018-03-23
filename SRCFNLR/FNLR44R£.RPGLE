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
002701151116      * clienti/filiali con sospensione bolle
002702151116     ffibsp01l  if   e           k disk
002703151116     ffibsp02l  if   e           k disk    rename(fibsp000:fibsp02)
002800960327     FFNLR44D   CF   E             WORKSTN
002900960326     F                                     SFILE(LR44S01:NRR1)
003000960401     F*
003100151116     D MSG             S             78    DIM(45) CTDATA PERRCD(1)             MSG VIDEO
003200960403     D Z01             S             40    DIM(3) CTDATA PERRCD(1)              PIEDI VIDEO
003300960521     D Z02             S             40    DIM(6) CTDATA PERRCD(1)              PIEDI VIDEO
003400960422     D Z03             S             40    DIM(2) CTDATA PERRCD(1)              PIEDI VIDEO
003500960329     D L6S             S              3  0 DIM(30)                              FIL GEST.DA AL
003600960523     D L1              S              3  0 DIM(30)                              FIL.GESTITE
003601151116     D est             S              3  0 DIM(30)                              FIL.GESTITE
003700961118     D TAB             S              1    DIM(4) CTDATA PERRCD(1)              INT/IDD/NONIDD
003800961118     D TABDES          S             10    DIM(4) ALT(TAB)
003900960405     D SKB             S             17    DIM(500)                             PER CONTR.
004000960405     D SKS             S             18    DIM(500)                             PER CONTR.
004100960405     D WMS             S              1    DIM(78)                              COMODO X MSG
004101151116     D i               S              3  0
004102151116     D in              S              3  0
004103151116     d w_clibo         s                   like(blpccm)
004104151116     D w_SiBsp         S              1
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
011701151116     d og143         e ds
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
025801151116     c* carico le filiali estere della l1
025802151116     c                   eval      i=1
025803151116     c                   eval      ii=0
025804151116     c                   dow       i<=%elem(l1) and l1(i)>0
025805151116     c     l1(i)         chain     azorg01l
025806151116     c                   if        %found(azorg01l)
025807151116     c                   movel     orgde3        og143
025808151116     c                   if        §ogntw='EEX' or §ogntw='FED' or §ogntw='DPD'
025809151116     c                   add       1             ii
025810151116     c                   eval      est(ii)=l1(i)
025811151116     c                   endif
025812151116     c                   endif
025813151116     c                   add       1             i
025814151116     c                   enddo
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
031800150924     C* VERIFICO SE RICHIESTE ANOMALIE inseribili in partenza
031900150924     C                   MOVE      *BLANKS       WinsP
032000150924     c                   if        d34par='P'
032100150924     c                   z-add     1             xx                3 0
032200150924     c                   dow       xx<=10 and anm(xx) <>*blanks
032300040923     C                   MOVEL     '7C'          KCOD
032400150924     C                   MOVEL(P)  anm(xx)       KKEY
032500040923     C     KTAB          CHAIN     TABEL00F                           30
032600040923     C  N30              MOVEL     TBLUNI        DS7C
032700040923     C   30              CLEAR                   DS7C
032800150924     c
032900150924     c     §7cges        ifeq      'S'
033000040923     C     D34INC        ANDEQ     'I'
033100150924     c     §7cpar        andeq     'P'
033200150924     C                   MOVE      '1'           WInsP             1
033300150924     C                   endif
033400150924     c                   add       1             xx
033500150924     c                   enddo
033600150924     C                   endif
033700150924     c
033800960521     C* PULISCO CAMPI DI OUTPUT
033900960521     C                   CLEAR                   D34F03
034000960521     C                   CLEAR                   D34F12
034100960521     C                   CLEAR                   D34RIL
034200960521     C                   SETON                                        20
034300960521     C* CARICO £6 A CAMBIO DI FILIALE GESTIONE
034400960521     C     D34FGS        IFNE      SAVFGS
034500960521     C                   CLEAR                   L6S
034600960521     C                   CLEAR                   DSUL06
034700960521     C                   MOVE      '£6'          D06COD
034800020509     C**!!!              MOVEL     'S'           D06ESC
034900020509     c                   eval      d06esc = 'G'
035000020509     c                   movel     simfel        d06key
035100960521     C                   MOVEL     'L'           D06TLA
035200960521     C                   MOVEL     DSUL06        KPJBU
035300960521     C                   CALL      'TRUL06R'
035400960521     C                   PARM                    KPJBA
035500960521     C                   MOVEL     KPJBU         DSUL06
035600960521     C                   MOVEA     LIN           L6S
035700961210     C                   MOVE      D34FGS        SAVFGS
035800960521     C                   END
035900960521     C*
036000960521     C                   SETOFF                                       0203
036100960521     C* SE RICHIESTE SOLO ANOMALIE IN PARTENZA ACCENDO INDICATORE
036200150924     C* vedo se abilitare in partenza l'inserimento
036300960521    1C     D34PAR        IFEQ      'P'
036400150924     C     WInsP         ANDEQ     *BLANKS
036500960521     C                   SETON                                        03
036600960521     C                   END
036700960521     C* SE IN INTERROGAZIONE ACCENDO INDICATORI PER DISABILITARE
036800960521     C* MANAUTENZIONE ED INSERIMENTO
036900960521     C     D34GES        IFEQ      'I'
037000960521     C                   SETON                                        0203
037100960521     C                   MOVEL     TIT2          V1DTIT
037200960521     C                   ELSE
037300960521     C                   MOVEL     TIT1          V1DTIT
037400960521    1C                   END
037500960521     C*    FINE FILE: IMPOSTO INDICATORE DI SFLEND
037600960521    1C     D34END        IFEQ      'S'
037700960521     C                   SETON                                        23
037800960521   X1C                   ELSE
037900960521     C* NO FINE FILE: CARICAMENTO SUBFILE
038000960521    2C     NRR1          IFLT      NRRAL
038100960521     C                   SETON                                        0405
038200960521     C                   SETOFF                                       09
038300960521     C                   EXSR      RIS01A
038400960523     C                   MOVE      '0'           V1HI51
038500960523     C                   MOVE      '0'           V1HI52
038600960521     C                   ADD       1             NRR1
038700960521     C                   WRITE     LR44S01
038800960521    2C                   END
038900960521    1C                   END
039000110909     c
039100111111     c****               endif
039200110909     c
039300960521     C                   ENDSR
039400960521     C****************   MODALITA' INTERROGAZIONE ANOMALIE  ***********
039500960521     C     SRINTE        BEGSR
039600960522     C*
039700960521     C                   EXSR      PULSFL
039800970217     C                   SETOFF                                       09
039900960521     C* PROTEGGO CAMPI I/O DEL SUBFILE
040000960521     C                   SETON                                        0405
040100960521     C* DISABILITO TASTI DI CONFERMA/INSER...+ OPZIONI VARIAZ/ANN/RIPR.
040200960521     C                   SETON                                        0203
040300960521     C                   MOVEL     TIT2          V1DTIT
040400960521     C*
040500960521     C* POSIZIONAMENTO INIZIALE SU FILE ANOMALIE PER CARICAMENTO SUBFIL
040600960521     C     D44NSP        IFGT      *ZEROS
040700960521     C* Interrogazione anomalie bolla
040800960521     C                   SETON                                        91
040900990512     C     KANM01        SETLL     FNANM01L
041000990512     C     KANM01        READE     FNANM01L                               35
041100960521     C                   MOVE      '1'           WFILE             1
041200960521     C                   ELSE
041300960521     C* Interrogazione anomalie segnacollo
041400960521     C                   SETON                                        92
041500960521     C                   MOVE      D44FIL        W44FIL
041600960521     C                   MOVE      D44LNA        W44LNA
041700960521     C                   MOVE      D44NRS        W44NRS
041800960521     C                   MOVE      D44SCN        W44SCN
041900990512     C     KANM02        SETLL     FNANM02L
042000990512     C     KANM02        READE     FNANM02L                               35
042100960521     C                   MOVE      '2'           WFILE             1
042200960521     C                   END
042300110909     C
042400960521     C                   EXSR      RIS01I
042500110909     c
042600960524     C* SE NON HO CARICATO NULLA SPENGO INDICATORE SFLDSP
042700960524     C     NRR1          IFEQ      *ZEROS
042800960524     C                   SETOFF                                       20
042900960524     C                   END
043000960521     C                   ENDSR
043100960521     C*********   CARICAMENTO SUBFILE MODALITA INTERROGAZIONE  ********
043200960521     C     RIS01I        BEGSR
043300960524     C                   CLEAR                   WFIRST
043400960524     C*
043500960524     C                   EXSR      RIS1IB
043600960524     C*
043700961210     C* SE NON HO RIEMPITO TUTTA LA PAGINA DI SUBFILE ED E' FINE FILE
043800990512     C* (DI FNANM01L); SE STO INTERROGANDO PER SPEDIZIONE,RICHIAMO ROU-
043900961210     C* TINE DI RICERCA  ANOMALIE PER OGNI SEGNACOLLO DELLA SPEDIZIONE
044000961210    1C     WFILE         IFEQ      '1'
044100960521     C     *IN91         ANDEQ     *ON
044200960521     C     NRR1          ANDLT     NRRAL
044300961210     C     *IN35         ANDEQ     *ON
044400960521     C                   SELECT
044500960521     C     D44TBO        WHENEQ    'A'                                          ARRIVI
044600960605     C     WOPNAR        IFEQ      ' '
044700960605     C                   OPEN      FNART01L
044800960605     C                   MOVE      '1'           WOPNAR            1
044900960605     C                   END
045000960522     C     KART01        SETLL     FNART01L
045100961210     C     KART01        READE     FNART01L                               36
045200960521     C     D44TBO        WHENEQ    'P'                                          PARTENZE
045300040615     c     d44tbo        oreq      'T'
045400960605     C     WOPNBL        IFEQ      ' '
045500960605     C                   OPEN      FNBLT01L
045600960605     C                   MOVE      '1'           WOPNBL            1
045700960605     C                   END
045800960522     C     KART01        SETLL     FNBLT01L
045900961210     C     KART01        READE     FNBLT01L                               36
046000040615     C**   D44TBO        WHENEQ    'T'                                          TRANSITI
046100040615     C**   WOPNBT        IFEQ      ' '
046200040615     C**                 OPEN      FNBTT01L
046300040615     C**                 MOVE      '1'           WOPNBT            1
046400040615     C**                 END
046500040615     C**   KART01        SETLL     FNBTT01L
046600040615     C**   KART01        READE     FNBTT01L                               36
046700960521     C                   ENDSL
046800960523     C                   MOVE      '2'           WFILE
046900961210     C  N36              EXSR      RICSCN
047000961210   X1C                   ELSE
047100050929     c
047200961210     C     WFILE         IFEQ      '2'
047300961210     C     *IN91         ANDEQ     *ON
047400961210     C     NRR1          ANDLT     NRRAL
047500050929     c                   if        finiscelett=' '
047600961210     C                   EXSR      RICSCN
047700961210     C* 23 --> SFLEND
047800961210     C   92
047900961210     CAN 36              SETON                                        23
048000050929     c                   else
048100050929     c                   seton                                        23
048200050929     c                   clear                   finiscelett
048300050929     C                   ENDif
048400050929     C                   END
048500961210    1C                   END
048600960524     C*
048700960524     C* SE HO CARICATO ALMENO UN RECORD DI SFL INCREMENTO SFLRCDNBR
048800960524    1C     NRR1          IFGT      NRRSAV
048900960524     C     NRRSAV        ADD       1             V1CNBR
049000960524    1C                   END
049100960524     C* SE FINE LETTURA MEMORIZZO SFLRCDNBR DELL'ULTIMA PAGINA
049200960524     C   23              Z-ADD     V1CNBR        W1CNBR
049300960524     C* MEMORIZZO NRR
049400960524     C                   Z-ADD     NRR1          NRRSAV
049500960521     C                   ENDSR
049600960523     C*********   CARICAMENTO SUBFILE MODALITA INTERROGAZIONE  ********
049700960523     C     RIS1IB        BEGSR
049800960523     C                   SETOFF                                       22  49
049900960523     C                   SETOFF                                       404142
050000960523     C                   SETOFF                                       434445
050100960523     C                   SETOFF                                       464748
050200960523    1C     NRR1          DOWLT     NRRAL
050300961210     C     *IN35         ANDEQ     *OFF
050400960524     C* ESCLUDO RECORD SE STO LEGGENDO PER SPEDIZIONE E SEGNACOLLO>0
050500960524     C* (LO TRATTERO' NEL GIRO PER SEGNACOLLO)
050600960524     C     WFILE         IFEQ      '1'
050700960524     C     ANMSCN        ANDGT     *ZEROS
050800960524     C                   GOTO      LEGGAN
050900960524     C                   END
051000110909     C
051100111115     c***                IF        %subst(knmus:1:3)<>'EDP'  AND anmcaa= 115
051200111115     C***                GOTO      LEGGAN
051300111115     C***                END
051400110909     c
051500960524     C*
051600960523     C     WFILE         IFEQ      '1'
051700960523     C                   Z-ADD     ANMNR1        D44NRR
051800960523     C                   ELSE
051900960523     C                   Z-ADD     ANMNR2        D44NRR
052000960523     C                   END
052100960523     C                   EXSR      RIS01A
052200960523     C*
052300960523     C* SOLO LA PRIMA VOLTA VISUALIZZO CAMPI DELLA CHIAVE
052400960524     C     WFIRST        IFEQ      *BLANKS
052500960524     C                   MOVE      '1'           WFIRST            1
052600960523     C                   SETOFF                                       5152
052700960523     C                   ELSE
052800960524     C     *IN91         IFEQ      *ON
052900960524     C                   SETOFF                                       52
053000960524     C                   ELSE
053100960524     C                   SETON                                        52
053200960524     C                   END
053300960523     C                   SETON                                        51
053400960523     C                   END
053500960523     C*
053600960524     C                   MOVE      *IN51         V1HI51
053700960524     C                   MOVE      *IN52         V1HI52
053800960523     C                   ADD       1             NRR1
053900960523     C                   WRITE     LR44S01
054000960524     C     LEGGAN        TAG
054100960523     C     WFILE         IFEQ      '1'
054200990512     C     KANM01        READE     FNANM01L                               35
054300960523     C                   ELSE
054400990512     C     KANM02        READE     FNANM02L                               35
054500960523     C                   END
054600960523     C                   ENDDO
054700990512     C* MEMORIZZO CAMPI DI FNANM PER NON SPORCARMELI SE PER CASO
054800961210     C* ENTRERO' IN SRINT
054900961210     C  N35              MOVEL     DSANM         DSANMX
055000960523     C                   ENDSR
055100960521     C****************  RICERCA SEGNACOLLI DELLA SPEDIZIONE  **********
055200960521     C     RICSCN        BEGSR
055300960521    1C     NRR1          DOWLT     NRRAL
055400961210     C     *IN36         ANDEQ     *OFF
055500960530     C                   MOVE      DETFLS        W44FIL
055600960521     C                   MOVE      DETLNA        W44LNA
055700960521     C                   MOVE      DETNRS        W44NRS
055800960521     C                   MOVE      DETNSC        W44SCN
055900990512     C     KANM02        SETLL     FNANM02L
056000990512     C     KANM02        READE     FNANM02L                               35
056100961210     C  N35              EXSR      RIS1IB
056200960521     C*
056300960521     C                   SELECT
056400960521     C     D44TBO        WHENEQ    'A'                                          ARRIVI
056500961210     C     KART01        READE     FNART01L                               36
056600960521     C     D44TBO        WHENEQ    'P'                                          PARTENZE
056700040615     c     d44tbo        oreq      'T'
056800961210     C     KART01        READE     FNBLT01L                               36
056900040615     C***  D44TBO        WHENEQ    'T'                                          TRANSITI
057000040615     C***  KART01        READE     FNBTT01L                               36
057100960521     C                   ENDSL
057200960521     C*
057300960521     C                   ENDDO
057400960521     C*
057500050929     c                   if        *in35
057600961210     C   36              SETON                                        23
057700050929     c   36              clear                   finiscelett       1
057800050929     c                   else
057900050929     c* per 35 off - devop finire di leggere le anomalie dell'ultimo
058000050929     c*  segnacollo
058100050929     c                   if        *in36
058200050929     c                   eval      finiscelett='S'
058300050929     c                   endif
058400050929     c                   endif
058500960521     C                   ENDSR
058600960521     C****************  EMISSIONE/GESTIONE SUBFILE  *******************
058700960521     C     SRXFMT        BEGSR
058800960524     C* SOLO SE MODALITA' GESTIONE:
058900960524    1C     D44MOD        IFEQ      'G'
059000960524     C* Se non ho caricato nulla spengo indicatore sfldsp
059100960524    2C     NRR1          IFEQ      *ZEROS
059200960524     C                   SETOFF                                       20
059300960524     C                   MOVE      'S'           D34F12
059400960524   X2C                   ELSE
059500960524     C* Se ho caricato almeno un record di sfl incremento sflrcdnbr
059600960524     C                   Z-ADD     NRRSAV        V1CNBR
059700960524    3C     NRR1          IFGT      NRRSAV
059800960524     C                   ADD       1             V1CNBR
059900960524    3C                   END
060000960524     C* Memorizzo nrr
060100960524     C                   Z-ADD     NRR1          NRRSAV
060200960524     C* Se fine lettura memorizzo sflrcdnbr dell'ultima pagina
060300960524     C   23              Z-ADD     V1CNBR        W1CNBR
060400960524    2C                   END
060500960521     C* Imposto piedi del video
060600960521     C***
060700960521     C*** Modalità gestione
060800960521     C***
060900960522     C  N03
061000960522     CANN20              MOVEL     Z02(2)        V1ZCMD
061100960522     C   03
061200960522     CANN20              MOVEL     Z02(4)        V1ZCMD
061300960521     C* solo interrogazione
061400960521     C   02
061500960521     CAN 20              MOVEL     Z02(1)        V1ZCMD
061600960521     C* solo manutenzione
061700960521     C   03
061800960521     CANN02
061900960521     CAN 20              MOVEL     Z03(1)        V1ZCMD
062000960521     C   03
062100960521     CANN02
062200960521     CAN 20              Z-ADD     1             WF                1 0
062300960521     C   03
062400960521     CANN02
062500960521     CAN 20              Z-ADD     2             WMAX              1 0
062600960521     C* tutte le funzioni abilitate (manutenzione/inserimento/interrog)
062700960521     C  N03
062800960521     CAN 20              MOVEL     Z01(1)        V1ZCMD
062900960521     C  N03
063000960521     CAN 20              Z-ADD     1             WF                1 0
063100960521     C  N03
063200960521     CAN 20              Z-ADD     3             WMAX              1 0
063300960524   X1C                   ELSE
063400960521     C***
063500960521     C*** Modalità interrogazione
063600960521     C***
063700960521     C  N20              MOVE      Z02(4)        V1ZCMD
063800960521     C   91
063900960521     CAN 20              MOVE      Z02(5)        V1ZCMD
064000960521     C   92
064100960521     CAN 20              MOVE      Z02(6)        V1ZCMD
064200960524    1C                   END
064300960521     C* EMISSIONE SUBFILE
064400960521     C     EMC01         TAG
064500960521     C                   WRITE     LR44Z01
064600960521     C  N20              WRITE     LR44D03
064700960521     C                   WRITE     LR44D01
064800960521     C                   EXFMT     LR44C01
064900971103     C                   SETOFF                                       289040
065000960521     C                   SETOFF                                       414243
065100960521     C                   SETOFF                                       444546
065200960521     C                   SETOFF                                       474849
065300960521     C                   CLEAR                   V1ZMSG
065400960521     C* Tasto F3=Fine
065500960521    2C     *INKC         IFEQ      *ON
065600960521     C                   MOVE      'S'           D34F03
065700960521     C                   EXSR      PULSFL
065800960521     C                   GOTO      FXFMT
065900960521    2C                   END
066000960521     C* Tasto F12=Ritorno
066100960521    2C     *INKL         IFEQ      *ON
066200960521     C   10              MOVEL     'S'           D34RIL
066300960521     C  N10              MOVE      'S'           D34F12
066400960521     C                   EXSR      PULSFL
066500960521     C                   GOTO      FXFMT
066600960521    2C                   END
066700960521     C* Tasto F24=Altri Tasti
066800960521    2C     *INKY         IFEQ      *ON
066900960521     C     WF            IFLT      WMAX
067000960521     C                   ADD       1             WF
067100960521     C  N03              MOVEL     Z01(WF)       V1ZCMD
067200960521     C   03              MOVEL     Z03(WF)       V1ZCMD
067300960521     C                   ELSE
067400960521     C                   Z-ADD     1             WF
067500960521     C  N03              MOVEL     Z01(WF)       V1ZCMD
067600960521     C   03              MOVEL     Z03(WF)       V1ZCMD
067700960521     C                   END
067800960521     C                   GOTO      EMC01
067900960521    2C                   END
068000960521     C* Tasto F7=Interrogazione bolla
068100960521    2C     *INKG         IFEQ      *ON
068200960521     C                   SETON                                        22
068300960521     C     V1CPOS        CHAIN     LR44S01                            30
068400960521    3C     *IN30         IFEQ      *OFF
068500960523     C                   MOVE      V1HI51        *IN51
068600960523     C                   MOVE      V1HI52        *IN52
068700960521     C                   Z-ADD     NRR1          V1CNBR
068800960521     C                   UPDATE    LR44S01
068900960521     C                   SETOFF                                       22
069000960605     C                   CLEAR                   WAAS
069100960605     C                   MOVE      V1SLNP        WLNP
069200960605     C                   MOVE      V1SNRS        WNRS
069300960605     C                   MOVE      V1SNSP        WNSP
069400960605     C* Solo se ho la lna:
069500960605    4C     V1SLNA        IFGT      *ZEROS
069600021028     C**   V1SLNA        LOOKUP    L1                                     30
069700960605     C* Se non ho numero spedizione la cerco
069800021028     c* se non trovata bolla arrivi cerco nelle bolle partenza
069900021028     c*  a prescidenre che sia mia la bolla o non mia: tanto posso interrog
070000021028     c*
070100021028     c                   setoff                                       3031
070200021028     c* 30 off - ce'e' bolla ARRIVI
070300021028     C     KBLT          CHAIN     FNART27L                           30
070400021028     c* 30 on  - 31 off c'e' bolla partenza
070500021028     C   30KBLT          CHAIN     FNBLT27L                           31
070600021028     c*
070700021028    5C     V1SNSP        IFEQ      *ZEROS
070800021028     C     *IN30         IFEQ      *OFF
070900040531     C     *IN31         orEQ      *OFF
071000960605     C                   MOVE      D27AAS        WAAS
071100960605     C                   MOVE      D27LNP        WLNP
071200960605     C                   MOVE      D27NRS        WNRS
071300960605     C                   MOVE      D27NSP        WNSP
071400960605     C                   END
071500960605    5C                   END
071600960605     C*
071700960605    5C     WNSP          IFGT      *ZEROS
071800990513     C* Imposto anno a 4 cifre
071900960605    6C     WAAS          IFEQ      *ZEROS
072000990513    7C     V1SAAS        IFGT      60
072100990513     C                   MOVEL     19            WAAS
072200990513   X7C                   ELSE
072300990513     C                   MOVEL     20            WAAS
072400990513    7C                   END
072500990513     C                   MOVE      V1SAAS        WAAS
072600960605    6C                   END
072700021028     c*
072800021028     C* interrogo bolle arrivi
072900021028    6C     *IN30         IFEQ      *Off
073000960523     C                   CLEAR                   DSLR36
073100960523     C                   MOVE      WAAS          PA1AAS
073200960605     C                   MOVE      WLNP          PA1LNP
073300960605     C                   MOVE      WNRS          PA1NRS
073400960605     C                   MOVE      WNSP          PA1NSP
073500960903     C                   MOVE      'N'           PA1FLG
073600040121     C                   MOVE      *blanks       PA1CML
073700960523     C                   MOVEL     V1DPGM        PA1PGM
073800960523     C                   MOVEL     DSLR36        KPJBU
073900960523     C                   CALL      'FNLR36R'
074000960523     C                   PARM                    KPJBA
074100960605   X6C                   ELSE
074200021028     c**
074300960523     C* anomalia della partenza
074400021028   6ac                   if        not *in31
074500021028     C**   KBLP2         SETLL     FNBLP01L                               30
074600021028    7C**   *IN30         IFEQ      *ON
074700960523     C                   CLEAR                   DSLS05
074800960523     C                   MOVE      WAAS          PA0AAS
074900960605     C                   MOVE      WLNP          PA0LNP
075000960605     C                   MOVE      WNRS          PA0NRS
075100960605     C                   MOVE      WNSP          PA0NSP
075200960903     C                   MOVEL     'N'           PA0FLG
075300960523     C                   MOVEL     RAGUT         PA0RSU
075400960523     C                   MOVE      SIMFEL        PA0SIM
075500040121     C                   MOVE      *blanks       PA0CML
075600960523     C                   MOVEL     DSLS05        KPJBU
075700960523     C                   CALL      'FNLS05R'
075800960523     C                   PARM                    KPJBA
075900021028    7C**                 END
076000021028   6aC                   END
076100021028    6C                   END
076200960605    5C                   END
076300960605    4C                   END
076400960605    3C                   END
076500960521     C                   GOTO      EMC01
076600960521    2C                   END
076700960521     C* Tasto F14=Spunte
076800960521    2C     *INKN         IFEQ      *ON
076900960521     C                   SETON                                        22
077000960521     C     V1CPOS        CHAIN     LR44S01                            30
077100960521    3C     *IN30         IFEQ      *OFF
077200960523     C                   MOVE      V1HI51        *IN51
077300960523     C                   MOVE      V1HI52        *IN52
077400960521     C                   Z-ADD     NRR1          V1CNBR
077500960521     C                   UPDATE    LR44S01
077600960521     C                   SETOFF                                       22
077700960521    4C     V1SSCN        IFGT      *ZEROS
077800960521     C                   CLEAR                   DSLR04
077900960521     C                   Z-ADD     0             PA4NFV
078000960521     C                   MOVE      V1SFLS        PA4LNP
078100960521     C                   MOVE      V1SLNA        PA4LNA
078200960521     C                   MOVE      V1SNRS        PA4NRS
078300960521     C                   MOVEL     9             PA4NPG
078400960521     C                   Z-ADD     V1SSCN        PA4NCD
078500960521     C                   Z-ADD     0             PA4NCA
078600960521     C                   MOVEL     99            PA4NPS
078700960521     C                   MOVEL     'FNLR44R'     PA4PGM
078800960903     C                   MOVEL     '1'           PA4FLG
078900960521     C                   MOVEL     DSLR04        KPJBU
079000960521     C                   CALL      'FNLR04R'
079100960521     C                   PARM                    KPJBA
079200960521    4C                   END
079300960521    3C                   END
079400960521     C                   GOTO      EMC01
079500960521    2C                   END
079600960521     C* Tasto F10=Inserimento
079700960521    2C     *INKJ         IFEQ      *ON
079800150924     C*
079900150924    3C**   WInsP         IFEQ      '1'
080000150924     C**                 SETON                                        10
080100150924     C**                 EXSR      WDW04
080200150924     C**                 EXSR      PULSFL
080300150924     C**                 GOTO      FXFMT
080400150924   X3C**                 ELSE
080500960521     C                   EXSR      PULSFL
080600970217     C                   SETON                                        10
080700960521     C                   EXSR      RIS01B
080800960521     C                   MOVEL     Z02(3)        V1ZCMD
080900150924    3C**                 END
081000960521     C                   GOTO      EMC01
081100960521    2C                   END
081200960521     C* Solo se ho caricato qualcosa:
081300960521    2C     NRR1          IFGT      *ZEROS
081400960521     C* Tasto Rollup
081500960521    3C     *IN25         IFEQ      *ON
081600960521     C* Fine lettura --> emetto messaggio fine scorrimento
081700960521    4C     *IN23         IFEQ      *ON                                          FINE LETTURA
081800960521     C                   Z-ADD     W1CNBR        V1CNBR
081900960521     C                   MOVEL     MSG(1)        V1ZMSG
082000960521     C                   SETON                                        28
082100960521     C                   GOTO      EMC01
082200960521   X4C                   ELSE
082300960521     C     *IN10         IFEQ      *ON
082400960521     C                   EXSR      RIS01B
082500960521     C                   GOTO      EMC01
082600960521     C                   ELSE
082700960521     C                   ADD       9             NRRAL
082800960521     C                   Z-ADD     NRRSAV        NRR1
082900960521     C     D44MOD        IFEQ      'G'
083000960521     C                   GOTO      FXFMT
083100960521     C                   ELSE
083200960521     C                   EXSR      RIS01I
083300960521     C                   GOTO      EMC01
083400960521     C                   END
083500960521     C                   END
083600960521    4C                   END
083700960521    3C                   END
083800960521     C* CONTROLLI SUBFILE
083900960521    3C     *IN10         IFEQ      *OFF
084000960521     C                   EXSR      CTRINT
084100960521     C   KC              MOVE      'S'           D34F12
084200960521     C   KC              EXSR      PULSFL
084300960521     C   KC              GOTO      FXFMT
084400960521     C* Se interrogate anomalie riemetto il subfile
084500960521     C     WINT          CABEQ     '1'           EMC01
084600960521     C                   END
084700960521     C* Solo se non sono in interrogazione
084800960521     C     D44MOD        IFEQ      'G'
084900960521     C     D34GES        ANDNE     'I'
085000960521     C  N10              EXSR      CTS01A
085100960521     C   10              EXSR      CTS01B
085200960521     C***
085300960521     C  NKF
085400971103     COR 90
085500960521     COR 28              GOTO      EMC01
085600960521     C*
085700960521    3C     *IN10         IFEQ      *ON
085800960521     C   KF              EXSR      WTRANM
085900960521     C                   EXSR      PULSFL
086000960521     C                   ELSE
086100960521     C   KF              EXSR      AGGANM
086200960521     C                   GOTO      EMC01
086300960521    3C                   END
086400960523     C                   END
086500960521    2C                   END
086600960521     C     FXFMT         ENDSR
086700960327     C****************   PULIZIA SUBFILE ******************************
086800960327     C     PULSFL        BEGSR
086900960327     C                   SETOFF                                       2123
087000960327     C                   WRITE     LR44C01
087100960327     C                   SETON                                        2021
087200960327     C                   MOVE      *ZEROS        NRR1              5 0
087300960327     C                   MOVE      *ZEROS        NRRSAV
087400960327     C                   MOVE      *ZEROS        W1CNBR
087500960327     C                   MOVE      *ZEROS        V1CNBR
087600960327     C                   Z-ADD     9             NRRAL
087700960402     C                   SETOFF                                       100405
087800960405     C                   SETON                                        09
087900960327     C                   ENDSR
088000960327     C*********** RIEMPIMENTO CAMPI SUBFILE S01 ***********************
088100960328     C     RIS01A        BEGSR
088200960405     C                   SETOFF                                       22  49
088300960401     C                   SETOFF                                       404142
088400960401     C                   SETOFF                                       434445
088500960401     C                   SETOFF                                       464748
088600960328     C                   MOVE      ANMAAS        V1SAAS
088700960328     C                   MOVE      ANMLNP        V1SLNP
088800960328     C                   MOVE      ANMNRS        V1SNRS
088900960328     C                   MOVE      ANMNSP        V1SNSP
089000960328     C                   MOVE      ANMLNA        V1SLNA
089100960401     C                   MOVE      ANMFLS        V1SFLS
089200960328     C                   MOVE      ANMSCN        V1SSCN
089300040603     c                   move      anmft4        v1sbva
089400960327     C* DECODIFICO TIPO ANOMALIA
089500960328     C                   MOVEL     '7F'          KCOD
089600960328     C                   MOVEL(P)  ANMTAN        KKEY
089700960328     C     KTAB          CHAIN     TABEL00F                           30
089800960328     C  N30              MOVEL     TBLUNI        V1DTAN
089900960328     C   30              CLEAR                   V1DTAN
090000960404     C                   MOVE      ANMTAN        V1HTAN
090100960327     C* DECODIFICO SOGGETTO ANOMALIA
090200960328     C                   MOVEL     '7G'          KCOD
090300960328     C                   MOVEL(P)  ANMSAN        KKEY
090400960328     C     KTAB          CHAIN     TABEL00F                           30
090500960328     C  N30              MOVEL     TBLUNI        V1DSAN
090600960404     C   30              CLEAR                   V1DSAN
090700960404     C                   MOVE      ANMSAN        V1HSAN
090800960328     C                   MOVE      ANMCAA        V1SCAA
090900960403     C                   MOVE      ANMCAA        V1HCAA
091000960409     C                   CLEAR                   V1HKSP
091100970507     C                   CLEAR                   V1HARB
091200960506     C* DECODIFICO CODICE ANOMALIA
091300960401     C                   CLEAR                   DS7C
091400960328     C                   MOVEL     '7C'          KCOD
091500960328     C                   MOVEL(P)  ANMCAA        KKEY
091600960328     C                   CLEAR                   V1DCAA
091700960328     C                   CLEAR                   V1HGES
091800960328     C                   CLEAR                   V1HANN
091900960402     C                   CLEAR                   V1HKEY
092000960404     C                   CLEAR                   V1HLIO
092100960411     C                   CLEAR                   V1HUEA
092200960328     C     KTAB          CHAIN     TABEL00F                           30
092300960328     C     *IN30         IFEQ      *OFF
092400960328     C                   MOVEL     TBLUNI        DS7C
092500960328     C                   MOVEL     §7CDES        V1DCAA
092600960328     C* Memorizzo in campi hidden se caus.gestibile in manut./ins./ann
092700960328     C                   MOVEL     §7CGES        V1HGES
092800960418     C     D34APA        IFEQ      'P'
092900960328     C                   MOVEL     §7CANN        V1HANN
093000960418     C                   ELSE
093100960418     C                   MOVEL     §7CANA        V1HANN
093200960418     C                   END
093300960402     C                   MOVEL     §7CKEY        V1HKEY
093400960404     C     §7CIDD        IFEQ      'S'
093500960404     C                   MOVEL     §7CLID        V1HLIO
093600960404     C                   END
093700960411     C                   MOVE      §7CUEA        V1HUEA
093701151113     c                   move      §7cuep        v1huep
093702151113     c                   move      §7cpar        v1hpar
093800960328     C                   END
093900960507     C                   MOVE      ANMAIE        V1HAIE
094000960507     C     ANMAIE        IFEQ      'I'
094100960328     C                   MOVE      ANMAIE        V1SAIE
094200960507     C                   ELSE
094300960507     C     §7CIDD        IFEQ      'S'
094400960507     C                   MOVEL     'D'           V1SAIE
094500960507     C                   ELSE
094600960507     C                   MOVEL     'N'           V1SAIE
094700960507     C                   END
094800960507     C                   END
094900960401     C     §7CIDD        IFEQ      'S'
095000960328     C                   MOVE      ANMLID        V1SLID
095100960401     C                   ELSE
095200960401     C                   CLEAR                   V1SLID
095300960401     C                   END
095400960328     C* DECODIFICO FASE APERTURA ANOMALIA
095500960328     C                   MOVEL     '7D'          KCOD
095600960328     C                   MOVEL(P)  ANMFSA        KKEY
095700960328     C                   CLEAR                   V1DFSA
095800960328     C     KTAB          CHAIN     TABEL00F                           30
095900960328     C     *IN30         IFEQ      *OFF
096000960328     C                   MOVEL     TBLUNI        DS7D
096100960328     C                   MOVEL     §7DDES        V1DFSA
096200960328     C                   END
096300960404     C                   MOVE      ANMFSA        V1HFSA
096400960328     C* ROVESCIO DATA ANOMALIA
096500990513     C                   CLEAR                   WLBDA8
096600990513     C                   MOVE      ANMDAO        G08INV
096700990513     C                   MOVEL     '3'           G08ERR
096800990513     C                   CALL      'XSRDA8'
096900990513     C                   PARM                    WLBDA8
097000990513     C                   MOVEL     G08DAT        V1SDAO
097100990513     C                   MOVE      G08DAT        W0020             2 0
097200990513     C                   MOVE      W0020         V1SDAO
097300960419     C* RIEMPIO CAMPI RELATIVI AI DATI DI CHIUSURA (FASE,DATA,CAUSALE)
097400960419     C                   EXSR      RIECHI
097500960403     C     ANMCDU        IFGT      *ZEROS
097600960328     C                   MOVE      ANMCDU        V1SCDU
097700960403     C                   ELSE
097800960403     C                   CLEAR                   V1SCDU
097900960403     C                   END
098000960328     C                   MOVE      ANMNFV        V1SNFV
098100960328     C                   MOVE      ANMNPS        V1SNPS
098200960412     C                   CLEAR                   V1SVAR
098300960412     C                   CLEAR                   V1HVAR
098400960402     C                   MOVEL     ANMATB        V1SATB
098500960412     C                   Z-ADD     D44NRR        V1HNRR
098600960402     C                   MOVE      *IN04         V1HIN4
098700960402     C                   MOVE      *IN05         V1HIN5
098800960405     C                   MOVE      *IN09         V1HIN9
098900960401     C                   MOVEL     D34APA        V1HAPA
099000960401     C                   CLEAR                   V1HPRE
099100960327     C                   ENDSR
099200960419     C*********** RIEMPIMENTO CAMPI CHIUSURA ANOMALIA DI SFL S01 ******
099300960419     C     RIECHI        BEGSR
099400960419     C* DECODIFICO FASE CHIUSURA ANOMALIA
099500960419     C                   MOVE      *BLANKS       V1DFSC
099600960419     C     ANMFSC        IFNE      *BLANKS
099700960419     C                   MOVEL     '7D'          KCOD
099800960419     C                   MOVEL(P)  ANMFSC        KKEY
099900960419     C     KTAB          CHAIN     TABEL00F                           30
100000960419     C     *IN30         IFEQ      *OFF
100100960419     C                   MOVEL     TBLUNI        DS7D
100200960419     C                   MOVEL     §7DDES        V1DFSC
100300960419     C                   END
100400960419     C                   END
100500960419     C* ROVESCIO DATA CHIUSURA ANOMALIA SE DIVERSA DA 0
100600960419     C                   MOVE      *ZEROS        V1SDCH
100700960419     C     ANMDCH        IFGT      *ZEROS
100800990513     C                   CLEAR                   WLBDA8
100900990513     C                   MOVEL     ANMDCH        G08INV
101000990513     C                   MOVEL     '3'           G08ERR
101100990513     C                   CALL      'XSRDA8'
101200990513     C                   PARM                    WLBDA8
101300990514     C                   MOVEL     G08DAT        V1SDCH
101400990513     C                   MOVE      G08DAT        W0020             2 0
101500990513     C                   MOVE      W0020         V1SDCH
101600960419     C                   END
101700041125     C
101800960419     C                   MOVE      ANMCCH        V1SCCH
101900970522     C*
102000960419     C                   ENDSR
102100960328     C**************  RIEMPIMENTO SUBFILE PER INSERIMENTO *************
102200960328     C     RIS01B        BEGSR
102300960429     C                   SETOFF                                       22
102400960523     C                   SETOFF                                       5152
102500960328     C                   Z-ADD     NRRSAV        NRR1
102600960328     C     NRR1          ADD       1             V1CNBR
102700960329     C     NRR1          DOWLT     NRRAL
102800960328     C                   CLEAR                   V1SAAS
102900960328     C                   CLEAR                   V1SLNP
103000960328     C                   CLEAR                   V1SNRS
103100960328     C                   CLEAR                   V1SNSP
103200960402     C* IMPOSTO LINEA DI ARRIVO = FILIALE GESITIONE
103201151113     c                   clear                   v1slna
103202151113     c                   if        winsp=*blanks
103300960402     C                   MOVE      D34FGS        V1SLNA
103301151113     c                   endif
103400960401     C                   CLEAR                   V1SFLS
103500960328     C                   CLEAR                   V1SSCN
103600960328     C                   CLEAR                   V1DTAN
103700960328     C                   CLEAR                   V1DSAN
103800960329     C                   CLEAR                   V1SCAA
103900960328     C                   CLEAR                   V1DCAA
104000960328     C                   CLEAR                   V1HGES
104100960328     C                   CLEAR                   V1HANN
104200960402     C                   CLEAR                   V1HTAN
104300960402     C                   CLEAR                   V1HSAN
104400960402     C                   CLEAR                   V1HKEY
104500960404     C                   CLEAR                   V1HLIO
104600960403     C                   CLEAR                   V1HCAA
104700960409     C                   CLEAR                   V1HKSP
104800970507     C                   CLEAR                   V1HARB
104900960329     C                   CLEAR                   V1HNRR
105000960411     C                   CLEAR                   V1HUEA
105100960328     C                   CLEAR                   V1SAIE
105200960507     C                   CLEAR                   V1HAIE
105300960328     C                   CLEAR                   V1SLID
105400150924     c                   if        winsp='1'
105500150924     C                   MOVE      'P'           V1HAPA
105600150924     C                   MOVEL     '1'           V1HFSA
105700150924     c                   else
105800150924     C                   MOVE      'A'           V1HAPA
105900960329     C                   MOVEL     '2'           V1HFSA
106000150924     c                   endif
106001151113     c                   clear                   v1hpar
106002151113     c                   clear                   v1huep
106100150924     c
106200960329     C                   MOVEL     '7D'          KCOD
106300150924     C                   MOVEL(P)  v1hfsa        KKEY
106400960329     C     KTAB          CHAIN     TABEL00F                           30
106500960329     C  N30              MOVEL     TBLUNI        V1DFSA
106600960329     C   30              CLEAR                   V1DFSA
106700971118     C* IMPOSTO DATA ANOMALIA = DATA DEL GIORNO
106800971118     C                   MOVE      DTGIO6        V1SDAO
106900990513     C                   MOVE      DATEU8        V1HDAO
107000960328     C                   CLEAR                   V1DFSC
107100960328     C                   CLEAR                   V1SDCH
107200960328     C                   CLEAR                   V1SCDU
107300960328     C                   CLEAR                   V1SNFV
107400960328     C                   CLEAR                   V1SNPS
107500960328     C                   CLEAR                   V1SCCH
107600960401     C                   CLEAR                   V1HPRE
107700960402     C                   MOVE      *IN04         V1HIN4
107800960402     C                   MOVE      *IN05         V1HIN5
107900960405     C                   MOVE      *IN09         V1HIN9
108000960523     C                   MOVE      '0'           V1HI51
108100960523     C                   MOVE      '0'           V1HI52
108200960328     C                   ADD       1             NRR1
108300960328     C                   WRITE     LR44S01
108400960328     C                   ENDDO
108500960328     C                   Z-ADD     NRR1          NRRSAV
108600960328     C                   ADD       9             NRRAL
108700960328     C                   ENDSR
108800960329     C******  CONTROLLI SUBFILE  PER INTERROGAZIONE RECORD ANOMALIA ***
108900960329     C     CTRINT        BEGSR
109000960403     C                   SETON                                        22
109100960329     C                   MOVE      *BLANKS       WINT              1
109200960329     C                   READC     LR44S01                                29
109300960329    1C     *IN29         DOWEQ     *OFF
109400960412     C     V1SVAR        IFEQ      'I'
109500960329     C* ESPLODO RECORD DI SUBFILE
109600960412     C                   CLEAR                   V1SVAR
109700960329     C                   MOVE      '1'           WINT
109800960402     C                   EXSR      SRINT
109900960509     C                   Z-ADD     NRR1          V1CNBR
110000961210     C                   MOVEL     DSANMX        DSANM
110100960329     C                   END
110200960402     C                   MOVE      V1HIN4        *IN04
110300960402     C                   MOVE      V1HIN5        *IN05
110400960405     C                   MOVE      V1HIN9        *IN09
110500960523     C                   MOVE      V1HI51        *IN51
110600960523     C                   MOVE      V1HI52        *IN52
110700960329     C                   UPDATE    LR44S01
110800960405     C* PREMUTO F3 DA VIDEATA INTERROGAZIONE ANOMALIA: ESCO DAL PGM
110900960403     C     *INKC         CABEQ     '1'           ENDINT
111000960329     C                   READC     LR44S01                                29
111100960329     C                   ENDDO
111200960403     C     ENDINT        ENDSR
111300960328     C******  CONTROLLI SUBFILE  MODALITA' INTERROGAZ/MANUTENZ. *******
111400960328     C     CTS01A        BEGSR
111500960403     C                   SETON                                        22
111600960426     C                   MOVE      ' '           SAVNBR            1
111700960329     C                   READC     LR44S01                                29
111800960329    1C     *IN29         DOWEQ     *OFF
111900960412    2C     V1SVAR        IFNE      *BLANKS
112000960412     C     V1HVAR        ORNE      *BLANKS
112100990512     C     V1HNRR        CHAIN(N)  FNANM00F                           30
112200960412     C* SE NON TROVATA ANOMALIA ERRORE
112300960412     C     *IN30         IFEQ      *ON
112400960412     C                   MOVE      'A'           V1SATB
112500960412     C                   MOVEL     MSG(31)       V1ZMSG
112600960412     C                   SETON                                        4728
112700960412     C                   GOTO      ENDCTA
112800960412     C                   END
112900960418     C* ERRORE SE RICHIESTA MANUTENZIONE/ANNULLAMENTO SU ANOMALIA GIà
113000960418     C*           CHIUSA CON CAUSALE DIVERSA DA 'AN'
113100970217     C* ESLUDO IL SEGUENTE CONTROLLO SE SI TRATTA DI AN.300=N°RITIRI
113200960418     C     ANMDCH        IFGT      *ZEROS
113300960418     C     ANMCCH        ANDNE     'AN'
113400960422     C     ANMCCH        ANDNE     'PR'
113500970217     C     ANMCAA        ANDNE     300
113600960403     C                   SELECT
113700960412     C     V1SVAR        WHENEQ    'A'
113800960403     C                   MOVEL     MSG(12)       V1ZMSG
113900960412     C                   SETON                                        4728
114000960412     C                   GOTO      ENDCTA
114100960412     C     V1SVAR        WHENEQ    'R'
114200960403     C                   MOVEL     MSG(14)       V1ZMSG
114300960412     C                   SETON                                        4728
114400960412     C                   GOTO      ENDCTA
114500960412     C     V1HVAR        WHENEQ    'V'
114600960412     C     *INKF         ANDEQ     *ON
114700960412     C                   MOVEL     MSG(30)       V1ZMSG
114800960412     C                   SETON                                        040528
114900960412     C                   SETOFF                                       09
115000960412     C                   MOVE      V1HNRR        D44NRR
115100960412     C                   EXSR      RIS01A
115200960412     C                   SETON                                        2247
115300960412     C                   GOTO      ENDCTA
115400960412     C     V1SVAR        WHENEQ    'V'
115500960403     C                   MOVEL     MSG(16)       V1ZMSG
115600960412     C                   SETON                                        4728
115700960412     C                   GOTO      ENDCTA
115800960403     C                   ENDSL
115900960329    3C                   END
116000960423     C**** VERIFICO ANNULLABILITA' DELL'ANOMALIA ***
116100960412    3C     V1SVAR        IFEQ      'A'
116200960418     c* Mai annullabile
116300960329    4C     V1HANN        IFEQ      *BLANKS
116400960418     C* Annullabile solo prima della trasmissione
116500960418     C     V1HANN        OREQ      'P'
116600960418     C     ANMFT1        ANDNE     *BLANKS
116700960418     C     V1HANN        OREQ      'P'
116800960418     C     V1HLIO        ANDNE     V1SLID
116900960506     C     V1SCAA        ANDNE     '070'
117000960506     C     V1SCAA        ANDNE     '071'
117100960418     C* Annullabile solo dopo la trasmissione
117200960418     C     V1HANN        OREQ      'D'
117300960418     C     ANMFT1        ANDEQ     *BLANKS
117400960418     C     V1HANN        OREQ      'D'
117500960418     C     V1HLIO        ANDEQ     V1SLID
117600960506     C     V1SCAA        ANDNE     '070'
117700960506     C     V1SCAA        ANDNE     '071'
117800960329     C                   MOVEL     MSG(12)       V1ZMSG
117900960401     C                   SETON                                        4728
118000960329     C                   GOTO      ENDCTA
118100960329    4C                   END
118200960418     C* Anomalia già annullata
118300960329    4C     V1SATB        IFNE      *BLANKS
118400960418     C     ANMCCH        OREQ      'AN'
118500960329     C                   MOVEL     MSG(13)       V1ZMSG
118600960401     C                   SETON                                        4728
118700960329     C                   GOTO      ENDCTA
118800960329    4C                   END
118900960429     C* Controlli particolari per annullamento anomalie partenza:
119000960429     C* 005 - Collo partito non arrivato
119100960429     C* 010 - No record bolla arrivo
119200960429     C* 012 - Manca record bolla e colli
119300960429    4C     V1HAPA        IFEQ      'P'
119400960429    5C     V1SCAA        IFEQ      '005'
119500960429     C     V1SCAA        OREQ      '010'
119600960429     C     V1SCAA        OREQ      '012'
119700960429     C                   EXSR      CTRANN
119800960429     C     WFLG01        IFEQ      'E'
119900960429     C                   MOVEL     MSG(34)       V1ZMSG
120000960429     C                   SETON                                        4728
120100960429     C                   GOTO      ENDCTA
120200960429     C                   END
120300960429    5C                   END
120400960429    4C                   END
120500970214    3C                   END
120600960423     C**** VERIFICO SE ANOMALIA RIPRISTINABILE ***
120700960412    3C     V1SVAR        IFEQ      'R'
120800960419     c* Mai ripristinabile
120900960329    4C     V1HANN        IFEQ      *BLANKS
121000960401     C                   MOVEL     MSG(14)       V1ZMSG
121100960401     C                   SETON                                        4728
121200960329     C                   GOTO      ENDCTA
121300960329    4C                   END
121400960329    4C     V1SATB        IFEQ      *BLANKS
121500960419     C     ANMCCH        ANDNE     'AN'
121600960329     C                   MOVEL     MSG(15)       V1ZMSG
121700960401     C                   SETON                                        4728
121800960329     C                   GOTO      ENDCTA
121900960329    4C                   END
122000960401    3C                   END
122100960423     C**** VERIFICO SE ANOMALIA MANUTENZIONABILE  ***
122200960412    3C     V1SVAR        IFEQ      'V'
122300960423     C* Non manutenzionabile se anomalia della partenza o anomalia
122400960423     C* già trasmessa
122500960419     C     V1HAPA        IFEQ      'P'
122600970214     C     ANMCAA        ANDNE     300
122700960423     C     ANMFT1        ORNE      *BLANKS
122800960423     C     V1SLID        ORNE      V1HLIO
122900960506     C     V1SCAA        ANDNE     '070'
123000960506     C     V1SCAA        ANDNE     '071'
123100960419     C                   MOVEL     MSG(16)       V1ZMSG
123200960419     C                   SETON                                        4728
123300960419     C                   GOTO      ENDCTA
123400960419     C                   END
123500970214     C* Se richiesta manutenzione anomalia 300 emetto window anzichè
123600970214     C* sproteggere i campi del subfile
123700970214     C     ANMCAA        IFEQ      300
123800040923     c     v1hges        ifeq      'S'
123900970214     C                   EXSR      WDW04
124000040923     C                   ELSE
124100040923     C                   MOVEL     MSG(16)       V1ZMSG
124200040923     C                   SETON                                        4728
124300040923     c                   endif
124400970214     C                   GOTO      ENDCTA
124500970214     C                   END
124600960412     C* CAMPI PROTETTI: SE POSSIBILE LI SPROTEGGO
124700960412    4C     V1HIN4        IFEQ      '1'
124800960412   4BC     *INKF         IFEQ      *OFF
124900960412    5C     V1HGES        IFEQ      'S'
125000960412     C                   SETOFF                                       04
125100960412     C                   SETON                                        09
125200960412     C                   MOVE      *IN04         V1HIN4
125300960412     C                   MOVE      *IN09         V1HIN9
125400960412     C                   MOVE      V1SVAR        V1HVAR
125500960506     C* Sproteggo codice anomalia solo se annullabile:
125600960412    6C     V1HANN        IFEQ      *BLANKS
125700960419     C     V1HANN        OREQ      'P'
125800960419     C     ANMFT1        ANDNE     *BLANKS
125900960419     C     V1HANN        OREQ      'P'
126000960419     C     V1HLIO        ANDNE     V1SLID
126100960506     C     V1SCAA        ANDNE     '070'
126200960506     C     V1SCAA        ANDNE     '071'
126300960419     C     V1HANN        OREQ      'D'
126400960419     C     ANMFT1        ANDEQ     *BLANKS
126500960419     C     V1HLIO        ANDEQ     V1SLID
126600960506     C     V1SCAA        ANDNE     '070'
126700960506     C     V1SCAA        ANDNE     '071'
126800960419     C* . . . Protezione
126900960412     C                   SETON                                        05
127000960412   X6C                   ELSE
127100960419     C* . . . Sprotezione
127200960412     C                   SETOFF                                       05
127300960412    6C                   END
127400960412     C                   MOVE      *IN05         V1HIN5
127500960412   X5C                   ELSE
127600960412     C                   MOVEL     MSG(16)       V1ZMSG
127700960412     C                   SETON                                        4728
127800960412     C                   GOTO      ENDCTA
127900960412    5C                   END
128000960412   4BC                   END
128100960412   X4C                   ELSE
128200960412     C* CAMPI GIA' SPROTETTI --> LI CONTROLLO
128300960412     C                   EXSR      CTRS01
128400960412    4C                   END
128500960412     C                   MOVE      *BLANKS       V1SVAR
128600960412   X3C                   ELSE
128700960412     C* CAMPI GIA' SPROTETTI --> LI CONTROLLO
128800960412    4C     V1HVAR        IFEQ      'V'
128900960423     C* Se premuto tasto di conferma e anomalia già trasmessa: errore
129000960423     C   KFANMFT1        IFNE      *BLANKS
129100960423     C     V1HLIO        ORNE      V1SLID
129200960506     C     V1SCAA        ANDNE     '070'
129300960506     C     V1SCAA        ANDNE     '071'
129400960423     C                   MOVEL     MSG(30)       V1ZMSG
129500960423     C                   SETON                                        040528
129600960423     C                   SETOFF                                       09
129700960423     C                   MOVE      V1HNRR        D44NRR
129800960423     C                   EXSR      RIS01A
129900960423     C                   SETON                                        2247
130000960423     C                   GOTO      ENDCTA
130100960423     C                   END
130200960412     C                   EXSR      CTRS01
130300960412    4C                   END
130400960412    3C                   END
130500960329     C*
130600960509    2C                   END
130700960401     C     ENDCTA        TAG
130800960402     C                   MOVE      V1HIN4        *IN04
130900960402     C                   MOVE      V1HIN5        *IN05
131000960405     C                   MOVE      V1HIN9        *IN09
131100960426     C     *IN28         IFEQ      *ON
131200960513     C     V1SVAR        ORNE      *BLANKS
131300960426     C     SAVNBR        ANDEQ     ' '
131400960426     C     V1HVAR        OREQ      'V'
131500960426     C     SAVNBR        ANDEQ     ' '
131600960329     C                   Z-ADD     NRR1          V1CNBR
131700960426     C                   MOVE      '1'           SAVNBR
131800960426     C                   END
131900960523     C                   MOVE      V1HI51        *IN51
132000960523     C                   MOVE      V1HI52        *IN52
132100960329     C                   UPDATE    LR44S01
132200971103     C   28              SETON                                        2990
132300960329     C  N29              READC     LR44S01                                29
132400960329    1C                   ENDDO
132500960401     C                   ENDSR
132600960328     C******  CONTROLLI SUBFILE  MODALITA' INSERIMENTO     ************
132700960328     C     CTS01B        BEGSR
132800960403     C                   SETON                                        22
132900960405     C* PULIZIA CAMPI DI COMODO
133000960405     C                   MOVE      *ZEROS        II
133100960405     C                   MOVE      *ZEROS        LL
133200960405     C                   CLEAR                   SKB
133300960405     C                   CLEAR                   SKS
133400960329     C* azzero campi per gestione DUP
133500960329     C                   CLEAR                   DUPAAS
133600960329     C                   CLEAR                   DUPCAA
133700960401     C                   CLEAR                   DUPLNP
133800960401     C                   CLEAR                   DUPNRS
133900960401     C                   CLEAR                   DUPNSP
134000960401     C                   CLEAR                   DUPLNA
134100960401     C                   CLEAR                   DUPFLS
134200960401     C                   CLEAR                   DUPSCN
134300960401     C                   CLEAR                   DUPDAO
134400960329     C*
134500960329     C                   READC     LR44S01                                29
134600960329    1C     *IN29         DOWEQ     *OFF
134700960329     C                   CLEAR                   V1DCAA
134800960329     C                   CLEAR                   V1DSAN
134900960329     C                   CLEAR                   V1DTAN
135000960329     C                   CLEAR                   V1SAIE
135100960507     C                   CLEAR                   V1HAIE
135200960329     C*   Gestione tasto DUP
135300960401     C   11              MOVE      DUPCAA        V1SCAA
135400960401     C   12              MOVE      DUPAAS        V1SAAS
135500960401     C   13              MOVE      DUPNRS        V1SNRS
135600960401     C   14              MOVE      DUPNSP        V1SNSP
135700960401     C   15              MOVE      DUPLNA        V1SLNA
135800960401     C   16              MOVE      DUPLNP        V1SLNP
135900960401     C   17              MOVE      DUPSCN        V1SSCN
136000960401     C   18              MOVE      DUPDAO        V1SDAO
136100960401     C   19              MOVE      DUPFLS        V1SFLS
136200960329     C* CONTROLLO SOLO SE IMMESSI DATI
136300960329    2C     IOFLD         IFGT      *ZEROS
136400960329     C     V1SCAA        ORNE      *BLANKS
136500960329     C     V1SCAA        ANDNE     *ZEROS
136600960329     C                   EXSR      CTRS01
136700960409     C                   ELSE
136800960409     C                   CLEAR                   V1HKSP
136900970507     C                   CLEAR                   V1HARB
137000960409     C                   CLEAR                   V1HPRE
137100960329    2C                   END
137200960523     C                   MOVE      V1HI51        *IN51
137300960523     C                   MOVE      V1HI52        *IN52
137400960329     C                   Z-ADD     NRR1          V1CNBR
137500960329     C                   UPDATE    LR44S01
137600971103     C   28              SETON                                        2990
137700960329     C  N29              READC     LR44S01                                29
137800960329    1C                   ENDDO
137900960328     C                   ENDSR
138000960329     C*************  CONTROLLI COMUNI SUBFILE  ************************
138100960329     C     CTRS01        BEGSR
138200960506     C*** CODICE ANOMALIA  (SOLO SE SPROTETTO)
138300960402   1AC     V1HIN5        IFEQ      '0'
138400960329     C*   Ricerca in tabella
138500960401     C     '?'           SCAN      V1SCAA                                 30
138600960401    1C     *IN30         IFEQ      *ON
138700960329     C                   MOVEL     CODUT         §KUT
138800960329     C                   MOVEL     '7C'          §COD
138900960329     C                   MOVEL     *BLANKS       §KEY
139000960329     C                   CALL      'X§TABER'
139100960329     C                   PARM                    §KUT              1 0
139200960329     C                   PARM                    §COD              2
139300960329     C                   PARM                    §KEY              8
139400960329     C                   PARM                    §DES             30
139500960329     C                   MOVEL     §KEY          V1SCAA
139600960401    2C     V1SCAA        IFNE      *BLANKS
139700960329     C                   MOVEL     §DES          V1DCAA
139800960401    2C                   END
139900971103     C                   SETON                                        2990
140000960329     C                   GOTO      ENDCTR
140100960401    1C                   END
140200960329     C*   Controlli di validità'
140300960401    1C     V1SCAA        IFEQ      *ZEROS
140400960329     C     V1SCAA        OREQ      *BLANKS
140500960329     C                   MOVEL     MSG(2)        V1ZMSG
140600960401     C                   SETON                                        4028
140700960329     C                   GOTO      ENDCTR
140800960401    1C                   END
140900960329     C                   TESTN                   V1SCAA               30
141000960401    1C     *IN30         IFEQ      *ON
141100960329     C                   MOVE      V1SCAA        W001A             1
141200960401    2C     W001A         IFLT      '0'
141300960329     C                   SETOFF                                       30
141400960401    2C                   END
141500960401    1C                   END
141600960401    1C     *IN30         IFEQ      *OFF
141700960329     C                   MOVEL     MSG(2)        V1ZMSG
141800960401     C                   SETON                                        4028
141900960329     C                   GOTO      ENDCTR
142000960401    1C                   END
142100960329     C* Chaino tabella per reperimento altri dati
142200960329     C                   MOVEL     '7C'          KCOD
142300960329     C                   MOVEL(P)  V1SCAA        KKEY
142400960329     C     KTAB          CHAIN     TABEL00F                           30
142500960401    1C     *IN30         IFEQ      *ON
142600960329     C     TBLFLG        ORNE      *BLANKS
142700960329     C                   MOVEL     MSG(2)        V1ZMSG
142800960401     C                   SETON                                        4028
142900960329     C                   GOTO      ENDCTR
143000960401    1C                   END
143100960329     C                   MOVEL     TBLUNI        DS7C
143200960329     C                   MOVEL     §7CDES        V1DCAA
143300960506     C*  Controllo che codice anomalia sia gestibile in inserimento
143400960402    1C     §7CGES        IFEQ      *BLANKS
143500960329     C                   MOVEL     MSG(5)        V1ZMSG
143600960401     C                   SETON                                        4028
143700960329     C                   GOTO      ENDCTR
143800960401    1C                   END
143900960506     C*  Se codice ok imposto anche altri campi di output del video
144000960402     C*  (N.B.: v1hges e v1hann mantengo quelli di origine)
144100960329     C                   MOVEL     §7CTAN        V1HTAN
144200960329     C                   MOVEL     §7CSAN        V1HSAN
144300960402     C                   MOVEL     §7CKEY        V1HKEY
144400960411     C                   MOVEL     §7CUEA        V1HUEA
144500960329     C                   MOVEL     '7F'          KCOD
144600960329     C                   MOVEL(P)  §7CTAN        KKEY
144700960329     C     KTAB          CHAIN     TABEL00F                           30
144800960329     C  N30              MOVEL     TBLUNI        V1DTAN
144900960329     C                   MOVEL     '7G'          KCOD
145000960329     C                   MOVEL(P)  §7CSAN        KKEY
145100960329     C     KTAB          CHAIN     TABEL00F                           30
145200960329     C  N30              MOVEL     TBLUNI        V1DSAN
145300960507     C                   MOVEL     §7CAIE        V1HAIE
145400960507     C     §7CIDD        IFEQ      'S'
145500960507     C                   MOVEL     'D'           V1SAIE
145600960507     C                   ELSE
145700960507     C                   MOVEL     'N'           V1SAIE
145800960507     C                   END
145801151113     c                   movel     §7cpar        v1hpar
145802151113     C                   MOVEL     §7CUEp        V1HUEp
145803151116     c* Anomalia esterna o interna non della partenza --> solo da arrivo
145804151116     c* anomalia interna della partenza --> solo dalla partenza
145805151116     c                   if        v1hapa='P' and (v1haie='E' or
145806151116     c                                             v1hpar='A' or
145807151116     c                                             v1hpar=' '    )
145808151116     C                   MOVEL     MSG(45)       V1ZMSG
145809151116     C                   SETON                                        4028
145810151116     C                   GOTO      ENDCTR
145811151116     c                   endif
145812151116     c                   if        v1hapa='A' and  v1hpar='P'
145815151116     C                   MOVEL     MSG(44)       V1ZMSG
145816151116     C                   SETON                                        4028
145817151116     C                   GOTO      ENDCTR
145818151116     c                   endif
145819151116     c
145900960402   1AC                   END
146000960402     C* CONTROLLO LINEE SE IMMESSE
146100960402     C* - LA LNP, SE IMMESSA, Deve esistere in organigramma
146200960402    1C     V1SLNP        IFGT      *ZEROS
146300960402     C     V1SLNP        CHAIN     AZORG01L                           30
146400960402    2C     *IN30         IFEQ      *ON
146500960402     C     ORGFVA        ORNE      *BLANKS
146600960402     C                   MOVEL     MSG(3)        V1ZMSG
146700960402     C                   SETON                                        4328
146800960402     C                   GOTO      ENDCTR
146900960402    2C                   END
146901151116     c* Se anomalia interna della partenza deve essere una filiale in
146902151113     c* gestione
146903151113    2c                   if        v1haie='I' and v1hpar='P'
146904151113    3c                   if        v1huep=*blanks
146905151113    4C     V1SLNp        IFNE      D34FGS
146906151113     C     V1SLNp        LOOKUP    L6S                                    30
146907151116     C     V1SLNp        LOOKUP    est                                    31
146908151113    5C     *IN30         IFEQ      *OFF
146909151116    5C     *IN31         andeq     *OFF
146910151113     C                   MOVEL     MSG(8)        V1ZMSG
146911151113     C                   SETON                                        4328
146912151113     C                   GOTO      ENDCTR
146913151113    5C                   END
146914151113    4C                   END
146915151113   x3c                   else
146916151113     c* se anomalia che deve usare la filiale emittente la lnp deve essere diversa
146917151113    4c                   if        v1slnp=d34fgs
146918151113     C                   MOVEL     MSG(10)       V1ZMSG
146919151113     C                   SETON                                        4328
146920151113     C                   GOTO      ENDCTR
146921151113    4c                   endif
146922151113    3c                   endif
146923151113    2c                   endif
147000960402    1C                   END
147100960402     C* - LA Fls, SE IMMESSA, Deve esistere in organigramma
147200960402    1C     V1SFLS        IFGT      *ZEROS
147300960402     C     V1SFLS        CHAIN     AZORG01L                           30
147400960402    2C     *IN30         IFEQ      *ON
147500960402     C     ORGFVA        ORNE      *BLANKS
147600960402     C                   MOVEL     MSG(4)        V1ZMSG
147700960402     C                   SETON                                        4828
147800960402     C                   GOTO      ENDCTR
147900960402    2C                   END
148000960402    1C                   END
148100960411     C* - La Lna deve essere una filiale in gestione
148200960418     C*   (Se è un'anomalia per la quale occorre utilizzare la LNA
148300960418     C*   anzichè la filiale emittente)
148301151113     c* Solo se anomalia esterna o se interna solo se anomalia <>'P'
148400151113    1C***  V1SLNA        IFGT      *ZEROS
148401151113    1c                   if        v1slna>*zeros and
148402151113     c                             (v1haie='E' or v1hpar=' ' or
148403151113     c                                            v1hpar='A')
148500960411    2C     V1HUEA        IFEQ      *BLANKS
148600960411    3C     V1SLNA        IFNE      D34FGS
148700960402     C     V1SLNA        LOOKUP    L6S                                    30
148800960411    4C     *IN30         IFEQ      *OFF
148900960402     C                   MOVEL     MSG(8)        V1ZMSG
149000960402     C                   SETON                                        4528
149100960402     C                   GOTO      ENDCTR
149200960411    4C                   END
149300960411    3C                   END
149400960411   X2C                   ELSE
149500960524     C* - La Lna deve esistere in organigramma e deve essere diversa
149600960524     C*   dalla filiale emittente
149700960411     C*   (Se per questa anomalia occorre utilizzare la filiale emit-
149800960411     C*   tente)
149900960524    3C     V1SLNA        IFEQ      D34FGS
150000960524     C                   MOVEL     MSG(36)       V1ZMSG
150100960524     C                   SETON                                        4528
150200960524     C                   GOTO      ENDCTR
150300960524    3C                   END
150400960411     C     V1SLNA        SETLL     AZORG01L                               30
150500960411    3C     *IN30         IFEQ      *OFF
150600960411     C                   MOVEL     MSG(7)        V1ZMSG
150700960411     C                   SETON                                        4528
150800960411     C                   GOTO      ENDCTR
150900960411    3C                   END
151000960402    2C                   END
151100960402    1C                   END
151200960401     C***** CONTROLLO CHIAVE PRINCIPALE:
151300960401    1C                   SELECT
151400960402     C*     - PER SPEDIZIONE (AAS/LNP/NRS/NSP/LNA)
151500960402     C     V1HKEY        WHENEQ    'B'
151600960401     C* LINEA DI PARTENZA OBBLIGATORIA
151700960401    2C     V1SLNP        IFEQ      *ZEROS
151800960401     C                   SETON                                        4328
151900960329     C                   MOVEL     MSG(3)        V1ZMSG
152000960329     C                   GOTO      ENDCTR
152100960401    2C                   END
152200960401     C* NUMERO SPEDIZIONE OBBLIGATORIO
152300960401    2C     V1SNSP        IFEQ      *ZEROS
152400960401     C                   SETON                                        4128
152500960401     C                   MOVEL     MSG(6)        V1ZMSG
152600960401     C                   GOTO      ENDCTR
152700960401    2C                   END
152701151116     c* CONTROLLI PER ANOMALIA 20
152703151116     C     V1SCAA        IFEQ      '020'
152704151116     C     V1SAAS        IFGT      60
152705151116     C                   MOVEL     19            WAAS
152706151116     C                   ELSE
152707151116     C                   MOVEL     20            WAAS
152708151116     C                   END
152709151116     C                   MOVE      V1SAAS        WAAS
152710151116     c* la spedizione deve esistere su blp
152711151116     c     kblp          chain     fnblp01l
152712151116     c                   if        not %found(fnblp01l)
152713151116     C                   MOVEL     MSG(41)       V1ZMSG
152714151116     C                   SETON                                        444128
152715151116     C                   GOTO      ENDCTR
152716151116     c                   else
152717151116     c* se lna a video non immessa la imposto prendendola dal record bolla
152718151116     c                   move      blplna        v1slna
152719151116     c* errore se la spedizione è partita o chiusa
152720151116     c                   if        blpft1<>*blanks
152721151116     C                   MOVEL     MSG(42)       V1ZMSG
152722151116     C                   SETON                                        4128
152723151116     C                   GOTO      ENDCTR
152724151116     c                   endif
152725151116     c* errore se la lnp non è fra quelle per cui è prevista la sospensione automatica
152726151116     c* in chiusura f.v.p.
152727151116     c                   exsr      sr_fibsp
152728151116     c                   if        w_Sibsp=*blanks
152729151116     C                   MOVEL     MSG(43)       V1ZMSG
152730151116     C                   SETON                                        4128
152731151116     C                   GOTO      ENDCTR
152732151116     c                   endif
152733151116     c
152734151116     c                   endif
152735151116     c*
152736151116     c                   endif
152800960402     C     V1HKEY        WHENEQ    'S'
152900960401     C*    - PER SEGNACOLLO (FLS/LNA/NRS/SCN)
153000960401     C* FILIALE SEGNACOLLO OBBLIGATORIO
153100960401    2C     V1SFLS        IFEQ      *ZEROS
153200960401     C                   SETON                                        4828
153300960401     C                   MOVEL     MSG(4)        V1ZMSG
153400960401     C                   GOTO      ENDCTR
153500960401    2C                   END
153600960401     C* NUMERO SEGNACOLLO OBBLIGATORIO
153700960401    2C     V1SSCN        IFEQ      *ZEROS
153800960401     C                   SETON                                        4228
153900960401     C                   MOVEL     MSG(17)       V1ZMSG
154000960401     C                   GOTO      ENDCTR
154100960401    2C                   END
154200960401    1C                   ENDSL
154300960402     C* LINEA DI ARRIVO SEMPRE OBBLIGATORIA
154400960402     C* (Sia in key per spedizione che in key per segnacollo)
154500960402    2C     V1SLNA        IFEQ      *ZEROS
154600960402     C                   MOVEL     MSG(7)        V1ZMSG
154700960402     C                   SETON                                        4528
154800960402     C                   GOTO      ENDCTR
154900960402    2C                   END
155000960401     C**** DATA APERTURA ANOMALIA
155100960401    1C     V1SDAO        IFEQ      *ZEROS
155200960401     C                   MOVEL     MSG(9)        V1ZMSG
155300960401     C                   SETON                                        4628
155400960401     C                   GOTO      ENDCTR
155500960401    1C                   END
155600990513     C                   CLEAR                   WLBDA8
155700990513     C                   MOVE      V1SDAO        G08DAT
155800990513     C                   CALL      'XSRDA8'
155900990513     C                   PARM                    WLBDA8
156000990513    1C     G08ERR        IFEQ      '1'
156100960401     C                   MOVEL     MSG(9)        V1ZMSG
156200960401     C                   SETON                                        4628
156300960401     C                   GOTO      ENDCTR
156400960401   X1C                   ELSE
156500990513     C                   MOVE      G08INV        V1HDAO
156600960401    1C                   END
156700960401     C**** CONTROLLI DI CHIAVE DOPPIA
156800960403     C                   CLEAR                   WFLAG
156900960830     C                   CLEAR                   SAVFLS
157000960830     C                   CLEAR                   SAVSCN
157100960404     C     V1SCAA        IFEQ      '070'
157200960404     C     V1SCAA        OREQ      '071'
157300960404     C                   CLEAR                   V1SLID
157400960404     C                   END
157500960401     C                   MOVE      V1SCAA        WCAA
157600960401    1C                   SELECT
157700960402     C     V1HKEY        WHENEQ    'B'
157800990513     C     V1SAAS        IFGT      60
157900990513     C                   MOVEL     19            WAAS
158000990513     C                   ELSE
158100990513     C                   MOVEL     20            WAAS
158200990513     C                   END
158300990513     C                   MOVE      V1SAAS        WAAS
158400990512     C     KANM1         SETGT     FNANM01L
158500990512     C     KANM1         READPE    FNANM01L                               30
158600960401    2C     *IN30         DOWEQ     *OFF
158700960401    3C     V1HNRR        IFNE      ANMNR1
158800960401     C                   EXSR      CTRDOP
158900960401     C   28              GOTO      ENDCTR
159000960401    3C                   END
159100960404     C* SOLO PER ANOMALIE 070-ECCEDE BAM E 071-MANCA BAM: MEMORIZZO IL
159200960404     C* LIVELLO IDD DELL'ANOMALIA PIù RECENTE.
159300960830     C* MEMORIZZO ANCHE FILIALE E NUMERO SEGNAC. > 0 PER POI
159400960830     C* CONFRONTARLI CON QUELLI IMMESSI EVENTUALMENTE A VIDEO
159500960404     C     V1SCAA        IFEQ      '070'
159600960404     C     V1SCAA        OREQ      '071'
159700960404     C     WFLAG         IFEQ      *BLANKS
159800960506     C     ANMFLE        ANDEQ     D34FGS
159900960404     C                   MOVE      ANMLID        V1SLID
160000960404     C                   MOVE      '1'           WFLAG             1
160100960404     C                   END
160200960830     C     ANMFLS        IFGT      *ZEROS
160300960830     C                   MOVE      ANMFLS        SAVFLS
160400960830     C                   MOVE      ANMSCN        SAVSCN
160500960830     C                   END
160600960404     C                   END
160700990512     C     KANM1         READPE    FNANM01L                               30
160800960401    2C                   ENDDO
160900960401     C*
161000960402     C     V1HKEY        WHENEQ    'S'
161100990512     C     KANM2         SETGT     FNANM02L
161200990512     C     KANM2         READPE    FNANM02L                               30
161300960401    2C     *IN30         DOWEQ     *OFF
161400960401    3C     V1HNRR        IFNE      ANMNR2
161500960401     C                   EXSR      CTRDOP
161600960401     C   28              GOTO      ENDCTR
161700960401    3C                   END
161800990512     C     KANM2         READPE    FNANM02L                               30
161900960401    2C                   ENDDO
162000960401    1C                   ENDSL
162100960405     C********************
162200960405     C* SOLO IN MANUTENZIONE :
162300960405     C*********************
162400960403     C* VERIFICO SE OCCORRE IMPOSTARE NUOVO LIVELLO IDD
162500960403    1C     *IN10         IFEQ      *OFF
162600960403    2C     WCAA          IFNE      V1HCAA
162700960403    3C     §7CIDD        IFEQ      'S'
162800960404     C                   MOVE      §7CLID        V1SLID
162900960506     C                   ELSE
163000960506     C                   CLEAR                   V1SLID
163100960403    3C                   END
163200960403   X2C                   ELSE
163300960404     C     V1SLID        IFEQ      *BLANKS
163400960404     C                   MOVE      V1HLIO        V1SLID
163500960404     C                   END
163600960403    2C                   END
163700960403   X1C                   ELSE
163800960405     C********************
163900960405     C* SOLO IN IMMISSIONE:
164000960405     C********************
164100960405     C* VERIFICO SE OCCORRE IMPOSTARE NUOVO LIVELLO IDD
164200960403    2C     V1SLID        IFEQ      *BLANKS
164300960403     C     §7CIDD        ANDEQ     'S'
164400960403     C                   MOVE      §7CLID        V1SLID
164500960403    2C                   END
164600960405     C* Controllo cha non sia già stato inserito un record con la
164700960409     C* stessa chiave
164800960405    2C                   SELECT
164900960405     C     V1HKEY        WHENEQ    'B'
165000960405     C                   MOVE      V1SAAS        WSAAS
165100960405     C                   MOVE      V1SLNP        WSLNP
165200960405     C                   MOVE      V1SNRS        WSNRB
165300960405     C                   MOVE      V1SNSP        WSNSP
165400960405     C                   MOVE      V1SCAA        WSCAB
165500960405     C     WKEYB         LOOKUP    SKB                                    30
165600960405    3C     *IN30         IFEQ      *ON
165700960405     C                   MOVEL     MSG(26)       V1ZMSG
165800960405     C                   SETON                                        284443
165900960405     C                   SETON                                        494140
166000960405    3C                   END
166100960405     C                   ADD       1             II                3 0
166200960405     C                   MOVE      WKEYB         SKB(II)
166300960405     C     V1HKEY        WHENEQ    'S'
166400960405     C                   MOVE      V1SFLS        WSFLS
166500960405     C                   MOVE      V1SLNA        WSLNA
166600960405     C                   MOVE      V1SNRS        WSNRS
166700960405     C                   MOVE      V1SSCN        WSSCN
166800960405     C                   MOVE      V1SCAA        WSCAS
166900960405     C     WKEYS         LOOKUP    SKS                                    30
167000960405    3C     *IN30         IFEQ      *ON
167100960405     C                   MOVEL     MSG(26)       V1ZMSG
167200960405     C                   SETON                                        284845
167300960405     C                   SETON                                        494240
167400960405    3C                   END
167500960405     C                   ADD       1             LL                3 0
167600960405     C                   MOVE      WKEYS         SKS(LL)
167700960405    2C                   ENDSL
167800960405     C   28              GOTO      ENDCTR
167900960403    1C                   END
168000960402     C* CONTROLLO COMPLETEZZA CHIAVE FACOLTATIVA, SE IMMESSA
168100960402    1C                   SELECT
168200960402     C     V1HKEY        WHENEQ    'B'
168300960402     C* Immessa filiale segnacollo e non il numero
168400960402    2C     V1SFLS        IFGT      *ZEROS
168500960402     C     V1SSCN        ANDEQ     *ZEROS
168600960403     C                   SETON                                        2842
168700960402     C                   MOVEL     MSG(19)       V1ZMSG
168800960402   X2C                   ELSE
168900960402     C* Immesso il numero segnacollo e non la filiale
169000960402    3C     V1SFLS        IFEQ      *ZEROS
169100960402     C     V1SSCN        ANDGT     *ZEROS
169200960403     C                   SETON                                        2848
169300960402     C                   MOVEL     MSG(21)       V1ZMSG
169400960402    3C                   END
169500960402    2C                   END
169600960402     C     V1HKEY        WHENEQ    'S'
169700960402    2C     KEYNSP        IFGT      *ZEROS
169800960402    3C     V1SNSP        IFEQ      *ZEROS
169900960403     C                   SETON                                        2841
170000960402     C                   MOVEL     MSG(25)       V1ZMSG
170100960402   X3C                   ELSE
170200000225     C     V1SLNP        IFEQ      *ZEROS
170300960403     C                   SETON                                        284344
170400960402     C                   MOVEL     MSG(24)       V1ZMSG
170500960402    4C                   END
170600960402    3C                   END
170700960402    2C                   END
170800960402    1C                   ENDSL
170900960402     C   28              GOTO      ENDCTR
171000960409     C*
171100960409     C******************************
171200960409     C* Solo per anomalie 070 e 071:
171300960409     C******************************
171400960409    1C     V1SCAA        IFEQ      '070'
171500960409     C     V1SCAA        OREQ      '071'
171600960830     C* Errore se immessi filiale/numero segnacollo diversi da quelli
171700960830     C* di un'altra anomalia con la stessa chiave (questo controllo
171800960830     C* serve per ricevere correttamente le anomalie visto che il
171900960830     C* programma di ricezione se fls/scn>0 legge le anomalie con key
172000960830     C* segnacollo)
172100960830     C     V1SFLS        IFGT      *ZEROS
172200960830     C     SAVFLS        ANDGT     *ZEROS
172300960830     C     V1SFLS        IFNE      SAVFLS
172400960830     C     V1SSCN        ORNE      SAVSCN
172500960830     C                   MOVEA     MSG(37)       WMS
172600960830     C                   MOVE      SAVFLS        W003A             3
172700960830     C                   MOVEA     W003A         WMS(67)
172800960830     C                   MOVE      SAVSCN        W007A             7
172900960830     C                   MOVEA     W007A         WMS(71)
173000960830     C                   MOVEA     WMS           V1ZMSG
173100960830     C                   SETON                                        284248
173200960830     C                   GOTO      ENDCTR
173300960830     C                   END
173400960830     C                   END
173500960830     C*
173600960409    2C     V1SCAA        IFEQ      '070'
173700960409     C                   MOVE      071           WSCAB
173800960409   X2C                   ELSE
173900960409     C                   MOVE      070           WSCAB
174000960409    2C                   END
174100960409     C                   MOVE      V1SAAS        WSAAS
174200960409     C                   MOVE      V1SLNP        WSLNP
174300960409     C                   MOVE      V1SNRS        WSNRB
174400960409     C                   MOVE      V1SNSP        WSNSP
174500960409     C************
174600960409     C* Immissione
174700960409     C************
174800960409    2C     *IN10         IFEQ      *ON
174900960506     C* Controllo che per la stessa bolla non sia stato immesso sia il
175000960506     C* codice 070 che 071
175100960409     C     WKEYB         LOOKUP    SKB                                    30
175200960409    3C     *IN30         IFEQ      *ON
175300960409     C                   MOVEL     MSG(27)       V1ZMSG
175400960409     C                   SETON                                        284443
175500960409     C                   SETON                                        4941
175600960409     C                   GOTO      ENDCTR
175700960409    3C                   END
175800960409    2C                   END
175900960830     C*****************************
176000960830     C* Manutenzione o inserimento:
176100960830     C*****************************
176200960409    2C     *IN10         IFEQ      *OFF
176300960409     C     WCAA          ANDNE     V1HCAA
176400960409     C     *IN10         OREQ      *ON
176500960506     C                   CLEAR                   WNTRM
176600960506     C                   CLEAR                   WTRM
176700960506     C                   CLEAR                   WNMIA
176800960506     C                   CLEAR                   WFLE
176900960506     C                   MOVE      WSCAB         WCAA
177000990513     C     V1SAAS        IFGT      60
177100990513     C                   MOVEL     19            WAAS
177200990513     C                   ELSE
177300990513     C                   MOVEL     20            WAAS
177400990513     C                   END
177500990513     C                   MOVE      V1SAAS        WAAS
177600990512     C     KANM1         SETLL     FNANM01L
177700990512     C     KANM1         READE     FNANM01L                               30
177800960506    3C     *IN30         DOWEQ     *OFF
177900960506     C     WNTRM         ANDEQ     *BLANKS
178000960409     C* Se in manutenzione scarto me stesso
178100960506    4C     *IN10         IFEQ      *OFF
178200960409     C     V1HNRR        ANDEQ     ANMNR1
178300960506     C                   GOTO      LEGGI
178400960506    4C                   END
178500960506     C* Anomalia non mia
178600960506    4C     ANMFLE        IFNE      D34FGS
178700960506     C                   MOVE      '1'           WNMIA             1
178800960506     C                   MOVE      ANMFLE        WFLE
178900960506   X4C                   ELSE
179000960506    5C     ANMFT1        IFEQ      *BLANKS
179100960506     C* Mia anomalia non trasmessa
179200960506     C                   MOVE      '1'           WNTRM             1
179300960506   x5C                   ELSE
179400960506    6C     ANMFT1        IFNE      *BLANKS
179500960506     C* Mia anomalia trasmessa
179600960506     C                   MOVE      '1'           WTRM              1
179700960506    6C                   END
179800960506    5C                   END
179900960506    4C                   END
180000960506     C     LEGGI         TAG
180100990512     C     KANM1         READE     FNANM01L                               30
180200960506    3C                   ENDDO
180300970507     C**
180400970507     C* SE E' MODIFICA LA SPEDIZIONE CONTROLLO SE C'E' IN ARB
180500970507     C     WKEYB         IFNE      V1HKSP
180600970507     C                   CLEAR                   V1HARB
180700970507     C                   ENDIF
180800970507     C**
180900960506    3C     WNTRM         IFEQ      '1'
181000960506     C     WTRM          OREQ      '1'
181100960506     C     WNMIA         OREQ      '1'
181200960506     C* Decodifico codice anomalia
181300960506     C                   MOVE      *BLANKS       WDES             24
181400960506     C                   MOVE      *BLANKS       WDES13           13
181500960409     C                   MOVEL     '7C'          KCOD
181600960409     C                   MOVEL(P)  ANMCAA        KKEY
181700960409     C     KTAB          CHAIN     TABEL00F                           30
181800960409     C  N30              MOVEL     TBLUNI        WDES
181900960506     C  N30              MOVEL     TBLUNI        WDES13
182000960506     C* Mia anomalia non trasmessa: errore bloccante
182100960506    4C     WNTRM         IFEQ      '1'
182200960506     C                   SETON                                        284443
182300960506     C                   SETON                                        4941
182400960506     C                   MOVEA     MSG(29)       WMS
182500960506     C                   MOVE      ANMCAA        W003A             3
182600960506     C                   MOVEA     W003A         WMS(38)
182700960506     C                   MOVEA     WDES          WMS(42)
182800960506     C                   MOVEA     WMS           V1ZMSG
182900960506     C                   GOTO      ENDCTR
183000960506   x4C                   ELSE
183100960506     C*
183200960506     C* Mia anomalia già trasmessa o anomalia non mia: errore forzabile
183300960506    5C                   SELECT
183400960506     C     WTRM          WHENEQ    '1'
183500960506     C     WKEYB         IFNE      V1HKSP
183600960506     C                   SETON                                        284443
183700960506     C                   SETON                                        4941
183800960506     C                   MOVEA     MSG(28)       WMS
183900960506     C                   MOVE      ANMCAA        W003A             3
184000960506     C                   MOVEA     W003A         WMS(38)
184100960506     C                   MOVEA     WDES          WMS(42)
184200960506     C                   MOVEA     WMS           V1ZMSG
184300960506     C                   GOTO      ENDCTR
184400960506     C                   END
184500960506     C     WNMIA         WHENEQ    '1'
184600960506     C     WKEYB         IFNE      V1HKSP
184700960506     C                   SETON                                        284443
184800960506     C                   SETON                                        4941
184900960506     C                   MOVEA     MSG(35)       WMS
185000960409     C                   MOVE      ANMCAA        W003A             3
185100960506     C                   MOVEA     W003A         WMS(34)
185200960506     C                   MOVEA     WDES13        WMS(38)
185300960506     C                   MOVE      WFLE          W003A
185400960506     C                   MOVEA     W003A         WMS(63)
185500960409     C                   MOVEA     WMS           V1ZMSG
185600960409     C                   GOTO      ENDCTR
185700960506     C                   END
185800960506    5C                   ENDSL
185900960506    4C                   END
186000960506    3C                   END
186100970507     C*
186200970507     C* IN MODIFICA: SE CAMBIATO SOLO IL NUMERO SPEDIZIONE RICONTROLLO
186300970507     C*  SU FNARB
186400970507   X2C                   ELSE
186500970507     C**
186600970507     C* SE E' MODIFICA LA SPEDIZIONE CONTROLLO SE C'E' IN ARB
186700970507     C     WKEYB         IFNE      V1HKSP
186800970507     C                   CLEAR                   V1HARB
186900970507     C                   ENDIF
187000960506    2C                   END
187100960506    1C                   END
187200970507     C*****
187300970507     C* SOLO PER ANOMALIA MANCA DDT
187400970507     C*****
187500970507     C* SE MODIFICATA LA SPEDIZIONE RICONTROLLO
187600970507    1C     V1SCAA        IFEQ      '071'
187700970507     C* VERIFICO SE C'E' LA BOLLA--> MESSAGGIO FORZABILE
187800970507     C     V1HARB        ANDEQ     ' '
187900990513     C     V1SAAS        IFGT      60
188000970507     C                   MOVEL     19            WAAS
188100970507     C                   ELSE
188200970507     C                   MOVEL     20            WAAS
188300970507     C                   END
188400970507     C                   MOVE      V1SAAS        WAAS
188500970507     C*
188600970507     C     KBLP          SETLL     FNARB01L                               30
188700970507     C*
188800970507    2C     *IN30         IFEQ      *OFF
188900970507     C                   MOVEL     'S'           V1HARB
189000970507     C                   SETON                                        284443
189100970507     C                   SETON                                        4941
189200970507     C                   MOVEA     MSG(39)       V1ZMSG
189300970507     C                   GOTO      ENDCTR
189400970507   X2C                   ELSE
189500970507     C* CONTROLLO SE E' UN NO DDT --> ERRORE NON FORZABILE
189600060216     C     KAR4          CHAIN     FIAR401L                           30
189700970507    3C     *IN30         IFEQ      *OFF
189800031028     c                   Movel     Ar4not        dsbl4a
189900031028     c                   Move      §b4abm        wddt              1
190000970507     C*
190100970507    4C     WDDT          IFNE      'S'
190200970507     C     WDDT          ANDNE     'C'
190300031028     c     wddt          andne     'P'
190400031028     c     wddt          andne     'Q'
190500970507     C                   SETON                                        284443
190600970507     C                   SETON                                        4941
190700970507     C                   MOVEA     MSG(40)       V1ZMSG
190800970507     C                   GOTO      ENDCTR
190900970507     C*
191000970507   X4C                   ELSE
191100970507     C                   MOVEL     'S'           V1HARB
191200970507    4C                   ENDIF
191300970507     C*
191400970507   X3C                   ELSE
191500970507     C                   MOVEL     'S'           V1HARB
191600970507    3C                   ENDIF
191700970507    2C                   ENDIF
191800970507    1C                   ENDIF
191900970507     C*
192000960401     C* Memorizzo CAMPI per DUP
192100960401     C                   MOVE      V1SCAA        DUPCAA
192200960401     C                   MOVE      V1SAAS        DUPAAS
192300960401     C                   MOVE      V1SLNP        DUPLNP
192400960401     C                   MOVE      V1SNRS        DUPNRS
192500960401     C                   MOVE      V1SNSP        DUPNSP
192600960401     C                   MOVE      V1SFLS        DUPFLS
192700960401     C                   MOVE      V1SSCN        DUPSCN
192800960401     C                   MOVE      V1SLNA        DUPLNA
192900960401     C                   MOVE      V1SDAO        DUPDAO
193000960401     C* MESSAGGI DI AVVISO:
193100971118     C* Avviso se data apertura anomalia > di data del giorno
193200990513    1C     V1HDAO        IFGT      DATEU8
193300960401     C     V1HDAO        ANDNE     V1HPRE
193400960401     C                   SETON                                        4628
193500960401     C                   MOVEL     MSG(23)       V1ZMSG
193600960409     C                   MOVE      V1HDAO        V1HPRE
193700960401    1C                   END
193800960409     C     ENDCTR        TAG
193900960410    1C     V1SCAA        IFEQ      '070'
194000960410     C     V1SCAA        OREQ      '071'
194100960410     C     WKEYB         IFNE      V1HKSP
194200960410     C                   MOVE      WKEYB         V1HKSP
194300960410     C                   END
194400960410     C                   END
194500960409     C                   ENDSR
194600960401     C*************  CONTROLLI DI CHIAVE DOPPIA ***********************
194700960401     C     CTRDOP        BEGSR
194800960418     C*
194900960418    1C     V1SCAA        IFEQ      '012'
195000960418     C                   SETON                                        444341
195100960418     C                   SETON                                        404928
195200960418     C                   MOVEL     MSG(32)       V1ZMSG
195300960418     C                   GOTO      ENDDOP
195400960418    1C                   END
195500960418     C*
195600960401    1C     ANMDCH        IFEQ      *ZEROS
195700960405     C     V1HDAO        ORLE      ANMDAO
195800960403     C     ANMLID        ORGE      WLCI
195900960401    2C                   SELECT
196000960402     C     V1HKEY        WHENEQ    'B'
196100960401     C                   SETON                                        444341
196200960402     C     V1HKEY        WHENEQ    'S'
196300960403     C                   SETON                                        4842
196400960401    2C                   ENDSL
196500960403    2C                   SELECT
196600960403     C* Errore di anomalia già aperta
196700960403     C     ANMDCH        WHENEQ    *ZEROS
196800960401     C                   MOVEL     MSG(18)       V1ZMSG
196900960403     C* errore di esistenza anomalia con data apertura più recente
197000960403     C     V1HDAO        WHENLT    ANMDAO
197100960401     C                   MOVEL     MSG(20)       V1ZMSG
197200960403     C                   SETON                                        46
197300960403     C* Errore se anomalia già CHIUSA COME IDD
197400960403     C     ANMLID        WHENGE    WLCI
197500960401     C                   MOVEL     MSG(22)       V1ZMSG
197600960403    2C                   ENDSL
197700960403     C*
197800960403     C                   SETON                                        28
197900960403     C                   SETON                                        404549
198000960403     C                   GOTO      ENDDOP
198100960403    1C                   END
198200960401     C     ENDDOP        ENDSR
198300960429     C****** CONTROLLI PARTICOLARI/DI ANNULLABILITA'/RIPR. ************
198400960429     C     CTRANN        BEGSR
198500960429     C                   CLEAR                   WFLG01
198600960429     C                   SETOFF                                       30
198700960429     C                   SELECT
198800960429     C     V1HKEY        WHENEQ    'B'
198900990513     C* Imposto anno a 4 cifre
199000041124     C     V1SAAS        IFGT      80
199100990513     C                   MOVEL     19            WAAS
199200990513     C                   ELSE
199300990513     C                   MOVEL     20            WAAS
199400990513     C                   END
199500990513     C                   MOVE      V1SAAS        WAAS
199600960429     C     KBLP          SETLL     FNBLP01L                               30
199700960429     C     V1HKEY        WHENEQ    'S'
199800960429     C     KBLT          SETLL     FNBLT27L                               30
199900040615     C**N30V1SCAA        IFEQ      '005'
200000040615     C***  KBLT          SETLL     FNBTT27L                               30
200100040615     C***                END
200200960429     C                   ENDSL
200300960429     C   30              MOVE      'E'           WFLG01            1
200400960429     C                   ENDSR
200500960402     C***********  INTERROGAZIONE DI UN RECORD SINGOLO  ***************
200600960402     C     SRINT         BEGSR
200700990512     C     V1HNRR        CHAIN(N)  FNANM00F                           30
200800960605    1C     *IN30         IFEQ      *OFF
200900960403     C     ANMATB        COMP      *BLANKS                            0606
201000960403     C                   MOVE      ANMAAS        V2CAAS
201100960403     C                   MOVE      ANMLNP        V2CLNP
201200960605     C                   MOVE      ANMNSP        V2CNSP
201300960605    2C     ANMNSP        IFGT      *ZEROS
201400960403     C                   MOVE      ANMNRS        V2CSRB
201500960403     C                   MOVE      ANMLNA        V2CLAB
201600960605   X2C                   ELSE
201700960605     C                   CLEAR                   V2CSRB
201800960605     C                   CLEAR                   V2CLAB
201900960605     C* Cerco spedizione per poterla visualizzare
202000960605    3C     ANMLNA        IFGT      *ZEROS
202100040531     C     KBLT2         CHAIN     FNBLT27L                           31
202200960605    4C     *IN31         IFEQ      *OFF
202300960605     C                   MOVE      D27AAS        V2CAAS
202400960605     C                   MOVE      D27LNP        V2CLNP
202500960605     C                   MOVE      D27NRS        V2CSRB
202600960605     C                   MOVE      D27NSP        V2CNSP
202700960605     C                   MOVE      D27LNA        V2CLAB
202800040531     c* se non impostata zona, la imposto
202900040531     c                   if        anmfl4=000  or anmfl4=999
203000040531     c     kblp27        chain     fnblp01l
203100040531     c                   if        %found(fnblp01l)
203200040531     c                   z-add     blpznc        anmfl4
203300040531     c                   endif
203400040531     c                   endif
203500960605    4C                   END
203600960605    3C                   END
203700960605    2C                   END
203800960605     C*
203900960403     C                   MOVE      ANMFLS        V2CFLS
204000960605    2C     ANMSCN        IFGT      *ZEROS
204100960403     C                   MOVE      ANMLNA        V2CLAS
204200960403     C                   MOVE      ANMNRS        V2CSRS
204300960605   X2C                   ELSE
204400960403     C                   CLEAR                   V2CLAS
204500960403     C                   CLEAR                   V2CSRS
204600960605    2C                   END
204700960403     C                   MOVE      ANMSCN        V2CSCN
204800960403     C                   MOVE      ANMNPS        V2CNPS
204900040531     C* VISLUALIZZO SE COLLO DI VALORE
205000040531     C                   SETOFF                                       93
205100040531     C                   IF        ANMFT4='V'
205200040531     C                   SETON                                        93
205300040531     C                   ENDIF
205400040531     C
205500960403     C* DECODIFICO TIPO ANOMALIA
205600960403     C                   MOVEL     '7F'          KCOD
205700960403     C                   MOVEL(P)  ANMTAN        KKEY
205800960403     C     KTAB          CHAIN     TABEL00F                           30
205900960403     C  N30              MOVEL     TBLUNI        V2DTAN
206000960403     C   30              CLEAR                   V2DTAN
206100960403     C* DECODIFICO SOGGETTO ANOMALIA
206200960403     C                   MOVEL     '7G'          KCOD
206300960403     C                   MOVEL(P)  ANMSAN        KKEY
206400960403     C     KTAB          CHAIN     TABEL00F                           30
206500960403     C  N30              MOVEL     TBLUNI        V2DSAN
206600960403     C   30              CLEAR                   V2DSAN
206700960506     C* DECODIFICO CODICE ANOMALIA
206800960403     C                   MOVE      ANMCAA        V2CCAA
206900960403     C                   MOVEL     '7C'          KCOD
207000960403     C                   MOVEL(P)  ANMCAA        KKEY
207100960403     C     KTAB          CHAIN     TABEL00F                           30
207200960403     C  N30              MOVEL     TBLUNI        DS7C
207300960403     C   30              CLEAR                   DS7C
207400960403     C                   MOVEL     §7CDES        V2DCAA
207500960507     C* INTERNA/I.D.D./NON I.D.D.
207600960507     C                   CLEAR                   W001A
207700960507     C     ANMAIE        IFEQ      'I'
207800961118     C     ANMAIE        OREQ      'X'
207900960507     C                   MOVE      ANMAIE        W001A
208000960507     C                   ELSE
208100960507     C     §7CIDD        IFEQ      'S'
208200960507     C                   MOVE      'D'           W001A             1
208300960507     C                   ELSE
208400960507     C                   MOVE      'N'           W001A
208500960507     C                   END
208600960507     C                   END
208700960507     C     W001A         LOOKUP    TAB           TABDES                   30
208800960507     C                   MOVEL     TABDES        V2DAIE
208900960507     C*
209000960403     C     §7CKEY        COMP      'B'                                080807
209100960403     C*
209200960403     C     §7CIDD        IFEQ      'S'
209300960403     C                   MOVE      ANMLID        V2CLID
209400960403     C                   ELSE
209500960403     C                   CLEAR                   V2CLID
209600960403     C                   END
209700960403     C* APERTURA:
209800960403     C* - decodifico fase
209900960403     C                   MOVEL     '7D'          KCOD
210000960403     C                   MOVEL(P)  ANMFSA        KKEY
210100960403     C     KTAB          CHAIN     TABEL00F                           30
210200960403     C  N30              MOVEL     TBLUNI        DS7D
210300960403     C   30              CLEAR                   DS7D
210400960411     C                   MOVEL     §7DDES        W2DFSA
210500960411     C                   MOVEL     ANMFSA        W2CFSA
210600960411     C                   MOVE      '-'           W2CFSA
210700960403     C*
210800990513     C                   CLEAR                   WLBDA8
210900990513     C                   MOVE      ANMDAO        G08INV
211000990513     C                   MOVEL     '3'           G08ERR
211100990513     C                   CALL      'XSRDA8'
211200990513     C                   PARM                    WLBDA8
211300990513     C                   MOVE      G08DAT        V2CDAO
211400960403     C                   MOVE      ANMCDU        V2CCDU
211500960403     C                   MOVE      ANMNFV        V2CNFV
211600960403     C* CHIUSURA:
211700960403     C* - decodifico fase
211800960405     C                   CLEAR                   V2DFSC
211900960405     C     ANMFSC        IFNE      *BLANKS
212000960403     C                   MOVEL     '7D'          KCOD
212100960403     C                   MOVEL(P)  ANMFSC        KKEY
212200960403     C     KTAB          CHAIN     TABEL00F                           30
212300960403     C  N30              MOVEL     TBLUNI        DS7D
212400960403     C   30              CLEAR                   DS7D
212500960411     C                   MOVEL     §7DDES        W2DFSC
212600960411     C                   MOVEL     ANMFSC        W2CFSC
212700960411     C                   MOVE      '-'           W2CFSC
212800960405     C                   END
212900960403     C*
213000990513     C                   CLEAR                   WLBDA8
213100990513     C                   MOVE      ANMDCH        G08INV
213200990513     C                   MOVEL     '3'           G08ERR
213300990513     C                   CALL      'XSRDA8'
213400990513     C                   PARM                    WLBDA8
213500990513     C                   MOVE      G08DAT        V2CDCH
213600960403     C* - decodifico causale
213700960405     C                   CLEAR                   V2DCCH
213800960405     C     ANMCCH        IFNE      *BLANKS
213900970522     C     ANMCCH        ANDNE     'RA'
214000960405     C                   MOVEL     ANMCCH        W2DCAA
214100960405     C                   MOVE      '-'           W2DCAA
214200960403     C                   MOVEL     '7E'          KCOD
214300960403     C                   MOVEL(P)  ANMCCH        KKEY
214400960403     C     KTAB          CHAIN     TABEL00F                           30
214500960405     C  N30              MOVEL     TBLUNI        W2DDES
214600960405     C                   END
214700040531     C* imposto la zona da anmfl4
214800000516     C                   SETOFF                                       01
214900040531     c                   if        anmfl4<>999
215000040531     c                   move      anmfl4        v2cznc
215100040531     c                   seton                                        01
215200040531     c                   endif
215300040531     c
215400040531     c* Data e ora di creazione anomalia
215500040531     c                   clear                   v2cdac
215600040531     c                   clear                   v2corc
215700040531     c                   if        anmflo<>*blanks
215800040531     c                   movel     anmflo        danmflo
215900040531     c                   if        §anmdac>0
216000040531     C                   CLEAR                   WLBDA8
216100040531     C                   MOVE      §anmdac       G08INV
216200040531     C                   MOVEL     '3'           G08ERR
216300040531     C                   CALL      'XSRDA8'
216400040531     C                   PARM                    WLBDA8
216500040531     C                   MOVE      G08DAT        V2Cdac
216600040531     c                   endif
216700040531     c                   z-add     §anmorc       v2corc
216800040531     c                   endif
216900040531     c
217000040531     c* cerco la zona dalla bolla, sel atrovo
217100040531     C**                 MOVEL     ANMFLO        DANM01
217200040531     C**   §ANZNC        IFNE      *BLANKS
217300040531     C**                 SETON                                        01
217400040531     C**                 MOVEL     §ANZNC        V2CZNC
217500040531     C**                 ENDIF
217600960403     C*
217700960403     C                   MOVE      ANMFLE        V2CFLE
217800960403     C                   MOVE      ANMFL1        V2CFL1
217900960403     C                   MOVE      ANMFL2        V2CFL2
218000960411     C                   CLEAR                   V2DFT1
218100960411     C                   CLEAR                   V2DFT2
218200000904     C* SOLO SE SONO DIVERSI LI DECODIFICO UNO PER UNO
218300000904    2C     ANMFT1        IFNE      ANMFT2
218400000904    3C     ANMFT1        IFEQ      'T'
218500960411     C                   MOVEL     'Trm'         V2DFT1
218600960605     C                   ELSE
218700960605     C     ANMFT1        IFEQ      'R'
218800960605     C                   MOVEL     'Ric'         V2DFT1
218900960411     C                   END
219000000904    3C                   END
219100000904    3C     ANMFT2        IFEQ      'T'
219200960411     C                   MOVEL     'Trm'         V2DFT2
219300960605     C                   ELSE
219400960605     C     ANMFT2        IFEQ      'R'
219500960605     C                   MOVEL     'Ric'         V2DFT2
219600960605     C                   END
219700000904    3C                   END
219800000904    2C                   END
219900000904     C*
220000000904     C                   CLEAR                   V2DDTR
220100000904     C                   CLEAR                   V2CDTR
220200000824     C*
220300000904     C**         ANMDTR    IFNE 0
220400000904     C**         ANMFT1    ANDNE' '
220500000904     C**         ANMDTR    ORNE 0
220600000904     C**         ANMAIE    ANDEQ'T'
220700000831     C*
220800000904     C     ANMFT1        IFEQ      'R'
220900000904     C                   MOVEL     WRIC          V2DDTR
221000000904     C                   ELSE
221100000904     C* O E' GIA' TRASMESSO O SFLAGGATO IN ATTESA DI TRASMISSIONE
221200000904     C                   MOVEL     WTRA          V2DDTR
221300000824     C                   ENDIF
221400000904     C*
221500000904    2C     ANMDTR        IFGT      0
221600000904    3C     ANMFT1        IFNE      ' '
221700000904     C     ANMAIE        OREQ      'T'
221800000824     C                   MOVEL     ANMDTR        WDT04A            4
221900000824     C                   MOVEL     ANMDTR        WDT06X            6
222000000824     C                   MOVE      WDT06X        WDT02M            2
222100000824     C                   MOVE      ANMDTR        WDT02G            2
222200000824     C                   MOVE      WDT10D        V2CDTR
222300000904    3C                   ENDIF
222400000904    2C                   ENDIF
222500000904     C**                   ELSE
222600000904     C**                   CLEARV2DDTR
222700000904     C**                   CLEARV2CDTR
222800000904     C**                   ENDIF
222900960403     C*****************
223000960403     C* EMISSIONE VIDEO
223100960403     C*****************
223200960403     C                   WRITE     LR44D01
223300960403     C                   EXFMT     LR44D02
223400960403     C*
223500960605    1C                   END
223600960402     C                   ENDSR
223700960328     C***********  AGGIORNAMENTO FILE ANOMALIE  ***********************
223800960328     C     AGGANM        BEGSR
223900960329     C                   READC     LR44S01                                29
224000960405    1C     *IN29         DOWEQ     *OFF
224100960419     C                   SETOFF                                       47
224200960412    2C     V1SVAR        IFNE      *BLANKS
224300960412     C     V1HVAR        ORNE      *BLANKS
224400990512     C     V1HNRR        CHAIN     FNANM00F                           30
224500960405    3C     *IN30         IFEQ      *OFF
224600041125     C**                 MOVE      *BLANKS       WALC              1
224700960405    4C                   SELECT
224800960419     C* GESTIONE ANNULLAMENTO ANOMALIA
224900960412     C     V1SVAR        WHENEQ    'A'
225000960514     C*
225100041124    5C**   V1HAPA        IFEQ      'P'
225200041124     C**   ANMAIE        ANDNE     'I'
225300041124     C**                 EXSR      SRCAEX
225400041124    5C**                 END
225500960514     C****
225600960514     C* Se chiudo anomalia 105-Manca record bolla partenza chiudo anche
225700960514     C* le anomalie 5 o 10 e viceversa
225800960514     C*
225900041125    5C     ANMCAA        IFEQ      5
226000960514     C     ANMCAA        OREQ      10
226100960514     C     ANMCAA        OREQ      105
226200960514     C                   EXSR      CHI105
226300041125    5C                   END
226400960514     C***
226500960419     C* Solo se riuscita l'allocazione del file di trasmissione:
226600041125    5C**   WALC          IFEQ      *BLANKS
226700041125     C**   WALC          OREQ      'S'
226800960419     C                   CLEAR                   DSLR33
226900960419     C* N.B.:Richiamo pgm chiusura anomalia senza sflaggare ft1/ft2
227000960419     C*      dando per scontato che le anomalie arrivo possono essere
227100960419     C*      annullate solo se non ancora trasmesse. Provvederò invece
227200960419     C*      alla scrittura del file di trasmissione a fronte di
227300960419     C*      annull/ripr di anomalie partenza
227400041125     c* 25/11/04: non inviamo più le anomalie ma subito annullate o ripristi
227500041125     c*           nate tutte . non importa quindi lo stesso sflaggare
227600960419     C                   MOVE      'N'           D33FTR
227700960419     C                   Z-ADD     V1HNRR        D33NRR
227800960419     C                   MOVE      'AN'          D33CCH
227900960419     C                   MOVE      'M'           D33FSC
228000990513     C                   Z-ADD     DATEU8        D33DCH
228100960514     C                   MOVEL     'S'           D33MAC
228200990512     C                   UNLOCK    FNANM00F
228300960419     C                   CALL      'FNLR33R'
228400960419     C                   PARM                    DSLR33
228500990512     C     V1HNRR        CHAIN(N)  FNANM00F                           30
228600960419     C   30              MOVE      'A'           V1SATB
228700960419     C  N30              EXSR      RIECHI
228800960419     C* Scrivo file di trasmissione
228900041125    6C**   WALC          IFEQ      'S'
229000041125     C**                 EXSR      TRMANM
229100041125    6C**                 END
229200041125    5C**                 END
229300041124     c
229400041124     c
229500960419     C* GESTIONE RIPRISTINO ANOMALIA
229600960412     C     V1SVAR        WHENEQ    'R'
229700041125    5C**   V1HAPA        IFEQ      'P'
229800041125     C**   ANMAIE        ANDNE     'I'
229900041125     C**                 EXSR      SRCAEX
230000041125    5C**                 END
230100960513     C* Se ripristino anomalia 105-Manca record bolla partenza
230200960513     C* ripristino anche le anomalie 5 o 10 e viceversa
230300041125     C**   WALC          IFEQ      'S'
230400041125     C**   WALC          OREQ      *BLANKS
230500960513     C     ANMCAA        IFEQ      5
230600960513     C     ANMCAA        OREQ      10
230700960513     C     ANMCAA        OREQ      105
230800960513     C                   EXSR      RIP105
230900960513     C                   END
231000041125     C**                 END
231100960419     C* Solo se riuscita l'allocazione del file di trasmissione:
231200041125    5C**   WALC          IFEQ      *BLANKS
231300041125     C**   WALC          OREQ      'S'
231400960419    6C     V1SATB        IFEQ      'A'
231500960419     C                   MOVE      *BLANKS       V1SATB
231600960329     C                   MOVE      *BLANKS       ANMATB
231700960419     C                   ELSE
231800960426     C* Clearo campi se anomalia non ha raggiunto il livello max
231900960508     C* N.B.:Se v1slid = blanks il codice operativo move sposta 0
232000960508     C*      in w0010 per cui non ci sono problemi
232100960507     C                   MOVE      V1SLID        W0010             1 0
232200960426     C     W0010         IFLT      WLCI
232300960419     C                   CLEAR                   ANMFSC
232400960419     C                   CLEAR                   ANMDCH
232500960419     C                   CLEAR                   ANMCCH
232600960419     C                   CLEAR                   V1DFSC
232700960426     C                   CLEAR                   V1SDCH
232800960419     C                   CLEAR                   V1SCCH
232900960426     C                   ELSE
233000960426     C* Altrimenti imposto chiusura in pratica Idd
233100960426     C                   MOVEL     'PR'          ANMCCH
233200990513     C                   MOVE      DATEU8        ANMDCH
233300960426     C                   MOVEL     'D'           ANMFSC
233400971118     C                   MOVE      DTGIO6        V1SDCH
233500960426     C                   MOVEL     'PR'          V1SCCH
233600960426     C                   MOVEL     '7D'          KCOD
233700960426     C                   MOVEL(P)  'D'           KKEY
233800960426     C     KTAB          CHAIN     TABEL00F                           30
233900960426     C  N30              MOVEL     TBLUNI        DS7D
234000960426     C  N30              MOVEL     §7DDES        V1DFSC
234100960426     C   30              CLEAR                   V1DFSC
234200960426     C                   END
234300960419    6C                   END
234400041125     C**   WALC          IFEQ      'S'
234500041125     C     WFEL1         IFGT      *ZEROS
234600960605     C     WFEL1         ANDNE     SIMFEL
234700960605     C                   MOVEL     'T'           ANMFT1
234800000824     C                   MOVE      DATEU8        ANMDTR
234900960605     C                   END
235000960605     C     WFEL2         IFGT      *ZEROS
235100960605     C     WFEL2         ANDNE     SIMFEL
235200960605     C     WFEL2         ANDNE     WFEL1
235300960605     C                   MOVEL     'T'           ANMFT2
235400000824     C                   MOVE      DATEU8        ANMDTR
235500960605     C                   END
235600041125     C**                 END
235700990512     C                   UPDATE    FNANM000
235800041125    6C**   WALC          IFEQ      'S'
235900041125     C**                 EXSR      TRMANM
236000041125    6C**                 END
236100041125    5C**                 END
236200960419     C* GESTIONE VARIAZIONE ANOMALIA
236300960412     C     V1HVAR        WHENEQ    'V'
236400960329     C                   EXSR      RISANM
236500990512     C                   UPDATE    FNANM000
236600960405    4C                   ENDSL
236700960405    3C                   END
236800960405    2C                   END
236900960402     C                   SETON                                        04
237000960405     C                   SETON                                        05
237100960405     C                   SETOFF                                       09
237200960402     C                   MOVE      *IN04         V1HIN4
237300960402     C                   MOVE      *IN05         V1HIN5
237400960405     C                   MOVE      *IN09         V1HIN9
237500041125     C                   CLEAR                   V1SVAR
237600041125     c
237700960412     C                   CLEAR                   V1HVAR
237800960523     C                   MOVE      V1HI51        *IN51
237900960523     C                   MOVE      V1HI52        *IN52
238000960329     C                   UPDATE    LR44S01
238100960329     C                   READC     LR44S01                                29
238200960405    1C                   ENDDO
238300000224     C*
238400960329     C                   ENDSR
238500960328     C***************  SCRITTURA FILE ANOMALIE   **********************
238600960328     C     WTRANM        BEGSR
238700960329     C                   READC     LR44S01                                29
238800960329     C     *IN29         DOWEQ     *OFF
238900960402     C     V1SCAA        IFNE      *BLANKS
239000960405     C     V1SCAA        ANDNE     *ZEROS
239100960402     C                   MOVE      V1SCAA        WCAA
239200990512     C                   CLEAR                   FNANM000
239300960329     C                   EXSR      RISANM
239400990512     C                   WRITE     FNANM000
239500960329     C                   END
239600960329     C                   READC     LR44S01                                29
239700960329     C                   ENDDO
239800960329     C   10              MOVEL     'S'           D34RIL
239900960328     C                   ENDSR
240000960329     C********  RIEMPIMENTO CAMPI FILE ANOMALIE  **********************
240100960329     C     RISANM        BEGSR
240200960329     C                   MOVE      V1SNSP        ANMNSP
240300960329     C                   MOVE      V1SSCN        ANMSCN
240400960401     C                   MOVE      V1SFLS        ANMFLS
240500960329     C                   MOVE      V1SLNP        ANMLNP
240600990524     C                   MOVE      V1SAAS        ANMAAS
240700000225     C     ANMNSP        IFGT      0
240800990513     C     V1SAAS        IFGT      60
240900990513     C                   MOVEL     19            ANMAAS
241000990513     C                   ELSE
241100990513     C                   MOVEL     20            ANMAAS
241200990513     C                   END
241300990524     C                   END
241400960329     C                   MOVE      V1SLNA        ANMLNA
241500960329     C                   MOVE      V1SNRS        ANMNRS
241600960329     C                   MOVEL     V1HTAN        ANMTAN
241700960329     C                   MOVEL     V1HSAN        ANMSAN
241800960329     C                   MOVE      V1SCAA        ANMCAA
241900960329     C                   MOVEL     V1HFSA        ANMFSA
242000960329     C                   MOVE      V1HDAO        ANMDAO
242100960507     C                   MOVEL     V1HAIE        ANMAIE
242200960329     C                   MOVE      V1SLID        ANMLID
242300960411     C                   MOVE      D34FGS        ANMFLE
242400960404     C     V1SLNP        IFGT      *ZEROS
242500960404     C                   MOVE      V1SLNP        ANMFL1
242600960404     C                   ELSE
242700960404     C                   MOVE      V1SFLS        ANMFL1
242800960404     C                   END
242900960404     C                   CLEAR                   ANMFT1
243000960404     C                   CLEAR                   ANMFT2
243100000824     C                   CLEAR                   ANMDTR
243200140523     c* imposto in anmflo data e ora creazione spunta
243300140523     C                   TIME                    TIMEanm          14 0
243400140523     c                   movel     timeanm       wora4             4 0
243500140523     C                   move      timeanm       G08DAT
243600140523     C                   Z-ADD     *ZERO         G08INV
243700140523     C                   MOVEL     *BLANKS       G08ERR
243800140523     C                   CALL      'XSRDA8'
243900140523     C                   PARM                    WLBDA8
244000140523     C                   move      G08INV        WDAT6             6 0
244100140523     c                   movel     wdat6         anmflo
244200140523     c                   move      wora4         anmflo
244300960329     C                   ENDSR
244400960513     C***  CHIUSURA ANOMALIA 5 O 10 PER ANOMALIA 105  E VICEVERSA  ****
244500960513     C     CHI105        BEGSR
244600041124     C*
244700041124     c* anm 105 - annullo la 10 o le 05
244800041124     c* anm  05 - annullo la 105 e altre 05
244900041124     c* anm  10 - annullo la 105
245000041124     c
245100041125     C                   eval      wcaaelab=anmcaa
245200041124     c                   eval      giri=1
245300041124     c
245400960513     C                   EXSR      LEGANM
245500041125     C
245600041125    0c                   dow       giri<=2
245700960513     C*
245800041124     C**                 MOVE      *BLANKS       WALC1             1
245900041124    1C     *IN30         doweq     *OFF
246000051006     c* Per le anom 05 escluse se stesso
246100051006     c                   if        wcaa=wcaaelab and anmnr2<>v1hnrr
246200041124     c*
246300041124    2C**   V1HAPA        IFEQ      'P'
246400041124     C**   ANMAIE        ANDNE     'I'
246500041124     C**                 EXSR      SRCAEX
246600041124     C**                 MOVE      WALC          WALC1
246700041124    2C**                 END
246800041124    2C**   WALC1         IFEQ      'S'
246900041124     C**   WALC1         OREQ      *BLANKS
247000041124     C*
247100960513     C                   CLEAR                   DSLR33
247200960513     C                   MOVE      'N'           D33FTR
247300960513     C                   Z-ADD     ANMNR2        D33NRR
247400960513     C                   MOVE      'AN'          D33CCH
247500960513     C                   MOVE      'M'           D33FSC
247600990513     C                   Z-ADD     DATEU8        D33DCH
247700960514     C* N.B.: in questo caso l'impostazione del flag d33mac è di poca
247800960514     C*       importanza visto che le anomalie di questa routine non
247900960514     C*       sono autochiuse. Lo imposto solo per renderlo uguale
248000960514     C*       all'altro richiamo di fnlr33r
248100960514     C                   MOVEL     'S'           D33MAC
248200960513     C                   CALL      'FNLR33R'
248300960513     C                   PARM                    DSLR33
248400960513     C* Scrivo file di trasmissione
248500041124    3C**   WALC1         IFEQ      'S'
248600041124     C**   ANMNR2        CHAIN(N)  FNANM00F                           30
248700041124     C**                 EXSR      TRMANM
248800041124     C**                 CLEAR                   WALC
248900041124    3C**                 END
249000041124    2C**                 END
249100051006     c                   endif
249200041124    c
249300041124     c
249400041124     c* Verifico se ci sono altre anomalie stesso segnacollo stesso
249500041124     c*  cod.anomalia da chiudere (sono poi solo le 005)
249600041124     C     KANM2         reade     FNANM02L                               30
249700041124    1C                   ENDdo
249800041124     c                   eval      giri=giri+1
249900041124     c
250000041124     c* Per anom 10  e 105 esco
250100041124    1c                   if        wcaaelab<>05
250200041124     c                   eval      giri=3
250300041124     c                   else
250400041124     c* Per anomalia 05 leggo altre 05 se presenti, escludo record in
250500041124     c*  annullamento
250600041124     C                   Z-ADD     05            WCAA
250700041124     C     KANM2         CHAIN     FNANM02L                           30
250800041124    2c                   dow       not *in30 and anmnr2=v1hnrr
250900041124     C     KANM2         reade     FNANM02L                               30
251000041124    2c                   enddo
251100041124     c
251200041124    1c                   endif
251300041124    0c                   enddo
251400960513     C                   ENDSR
251500960513     C***  RIPRISTINO ANOMALIA 5 O 15 PER ANOMALIA 105 E VICEVERSA ****
251600960513     C     RIP105        BEGSR
251700041124     c* anm 105 - annullo la 10 o le 05
251800041124     c* anm  05 - annullo la 105 e altre 05
251900041124     c* anm  10 - annullo la 105
252000041124     c
252100041124     c                   eval      wcaaelab=anmcaa
252200041124     c                   eval      giri=1
252300041124     c
252400960513     C                   EXSR      LEGANM
252500041125     c
252600041125    0c                   dow       giri<=2
252700960513     C*
252800041125     C***                MOVE      *BLANKS       WALC1             1
252900041125    1C     *IN30         doweq     *OFF
253000960513     C*
253100041124    2C**   V1HAPA        IFEQ      'P'
253200041124     C**   ANMAIE        ANDNE     'I'
253300041124     C**                 EXSR      SRCAEX
253400041124     C**                 MOVE      WALC          WALC1
253500041124    2C**                 END
253600960513     C*
253700041124    2C**   WALC1         IFEQ      'S'
253800041124     C**   WALC1         OREQ      *BLANKS
253900990512     C     ANMNR2        CHAIN     FNANM00F                           30
254000041125    3C     *IN30         IFEQ      *OFF
254100041125    4C     ANMLID        IFLT      WLCI
254200960513     C                   CLEAR                   ANMFSC
254300960513     C                   CLEAR                   ANMDCH
254400960513     C                   CLEAR                   ANMCCH
254500960513     C                   ELSE
254600960513     C* Altrimenti imposto chiusura in pratica Idd
254700960513     C                   MOVEL     'PR'          ANMCCH
254800990513     C                   MOVE      DATEU8        ANMDCH
254900960513     C                   MOVEL     'D'           ANMFSC
255000041125    4C                   END
255100041125    4C**   WALC1         IFEQ      'S'
255200041125    5C     WFEL1         IFGT      *ZEROS
255300960605     C                   MOVEL     'T'           ANMFT1
255400000824     C                   MOVE      DATEU8        ANMDTR
255500041125    5C                   END
255600041125    5C     WFEL2         IFGT      *ZEROS
255700960605     C     WFEL2         ANDNE     WFEL1
255800960605     C                   MOVEL     'T'           ANMFT2
255900000824     C                   MOVE      DATEU8        ANMDTR
256000041125    5C                   END
256100041125    4C**                 END
256200990512     C                   UPDATE    FNANM000
256300041125    3C                   END
256400041125    2C**                 END
256500041125     c
256600041125     c* Verifico se ci sono altre anomalie stesso segnacollo stesso
256700041125     c*  cod.anomalia da chiudere (sono poi solo le 005)
256800041125     C     KANM2         reade     FNANM02L                               30
256900041125    1C                   ENDdo
257000041125     c                   eval      giri=giri+1
257100041125     c
257200041125     c* Per anom 10  e 105 esco
257300041125    1c                   if        wcaaelab<>05
257400041125     c                   eval      giri=3
257500041125     c                   else
257600041125     c* Per anomalia 05 leggo altre 05 se presenti, escludo record in
257700041125     c*  annullamento
257800041125     C                   Z-ADD     05            WCAA
257900041125     C     KANM2         CHAIN     FNANM02L                           30
258000041125    2c                   dow       not *in30 and anmnr2=v1hnrr
258100041125     C     KANM2         reade     FNANM02L                               30
258200041125    2c                   enddo
258300041125     c
258400041125    1c                   endif
258500041125    0c                   enddo
258600960513     C*
258700041125     C**   WALC1         IFEQ      'S'
258800041125     C**                 EXSR      TRMANM
258900041125     C**                 CLEAR                   WALC
259000041125     C**                 END
259100960513     C* RICHAINO RECORD SCELTO A VIDEO
259200041125     C**   WALC1         IFNE      'N'
259300990512     C     V1HNRR        CHAIN     FNANM00F                           30
259400041125     C**                 END
259500960513     C*
259600041125    1C**                 END
259700960513     C                   ENDSR
259800960513     C**************  LETTURA ANOMALIA 5/10/105   *********************
259900960513     C     LEGANM        BEGSR
260000960513    1C                   SELECT
260100960513     C     ANMCAA        WHENEQ    105
260200960514     C                   Z-ADD     10            WCAA
260300990512     C     KANM2         CHAIN     FNANM02L                           30
260400960514     C   30              Z-ADD     5             WCAA
260500990512     C   30KANM2         CHAIN     FNANM02L                           30
260600960513     C     ANMCAA        WHENEQ    5
260700960513     C     ANMCAA        OREQ      10
260800960513     C                   Z-ADD     105           WCAA
260900990512     C     KANM2         CHAIN     FNANM02L                           30
261000960513    1C                   ENDSL
261100960513     C                   ENDSR
261200970214     C**********  GESTIONE VIDEATA MANUTENZIONE ANOMALIA 300  *********
261300970214     C     WDW04         BEGSR
261400970214     C* La causale anomalia la imposto sempre = a 300
261500970214     C                   Z-ADD     300           W4SCAA
261600970214     C                   MOVEL     '7C'          KCOD
261700970214     C                   MOVEL(P)  300           KKEY
261800970214     C     KTAB          CHAIN     TABEL00F                           30
261900970214     C  N30              MOVEL     TBLUNI        DS7C
262000970214     C   30              CLEAR                   DS7C
262100970214     C                   MOVEL     §7CDES        W4DCAA
262200970214     C* Spengo indicatori del video
262300970214     C                   SETOFF                                       282555
262400970217     C                   SETOFF                                       5654
262500970214     C* 10 OFF --> MANUTENZIONE
262600970214     C     *IN10         IFEQ      *OFF
262700970214     C* Imposto la lnp
262800970214     C                   MOVE      ANMLNP        W4SLNP
262900970214     C* Imposto il numero dei ritiri
263000970214     C                   MOVE      ANMSCN        W4SNRT
263100970214     C* imposto la data anomalia
263200990513     C                   CLEAR                   WLBDA8
263300990513     C                   MOVE      ANMDAO        G08INV
263400990513     C                   MOVEL     '3'           G08ERR
263500990513     C                   CALL      'XSRDA8'
263600990513     C                   PARM                    WLBDA8
263700990513     C                   MOVE      G08DAT        W4SDAO
263800970214     C                   ELSE
263900970214     C* 10 ON --> INSERIMENTO
264000970214     C                   MOVE      D34FGS        W4SLNP
264100990513     C                   Z-ADD     WDTGIO        W4SDAO
264200970214     C                   MOVE      *ZEROS        W4SNRT
264300970214     C                   END
264400970214     C**
264500970214     C* EMISSIONE WINDOW
264600970214     C**
264700970214     C     EMWDW4        TAG
264800970214     C                   EXFMT     LR44W04
264900970214     C                   SETOFF                                       28
265000970214     C     *INKL         CABEQ     *ON           EWDW04
265100970214     C                   EXSR      CTRWDW
265200970214     C   28
265300970214     CORNKF              GOTO      EMWDW4
265400970214     C* CONFERMO DATI
265500970214     C     *IN10         IFEQ      *OFF
265600970214     C                   EXSR      AGG300
265700970214     C                   ELSE
265800970214     C                   EXSR      WTR300
265900970214     C                   END
266000970217     C*
266100970217     C     EWDW04        TAG
266200970217     C                   MOVE      *BLANKS       V1SVAR
266300970214     C*
266400970217     C                   ENDSR
266500970214     C**********  CONTROLLI WINDOW MANUTENZIONE ANOMALIA 300  *********
266600970214     C     CTRWDW        BEGSR
266700970214     C                   SETOFF                                       545556
266800970214     C*
266900970214    1C     *IN10         IFEQ      *ON
267000970214     C* LNP NON IMMESSA
267100970214    2C     W4SLNP        IFEQ      *ZEROS
267200970214     C                   SETON                                        5428
267300970214     C                   MOVEL     MSG(3)        W4ZMSG
267400970214     C                   GOTO      ENDCTW
267500970214    2C                   END
267600970214     C* LA LNP DEVE ESSERE UNA FILIALE IN GESTIONE
267700970214    2C     W4SLNP        IFNE      D34FGS
267800970214     C     W4SLNP        LOOKUP    L6S                                    30
267900970214    3C     *IN30         IFEQ      *OFF
268000970214     C                   MOVEL     MSG(8)        W4ZMSG
268100970214     C                   SETON                                        5428
268200970214     C                   GOTO      ENDCTW
268300970214    3C                   END
268400970214    2C                   END
268500970214    1C                   END
268600970214     C*
268700970214     C* ERRORE SE NON IMMESSO IL NUMERO RITIRI EFFETTUATI
268800970304    1C*          W4SNRT    IFEQ *ZEROS
268900970304     C*                    MOVELMSG,38    W4ZMSG
269000970304     C*                    SETON                     5528
269100970304     C*                    GOTO ENDCTW
269200970304    1C*                    END
269300970214     C*
269400970214    1C     *IN10         IFEQ      *ON
269500970214     C* ERRORE SE DATA ANOMALIA = 0
269600970214    2C     W4SDAO        IFEQ      *ZEROS
269700970214     C                   MOVEL     MSG(9)        W4ZMSG
269800970214     C                   SETON                                        5628
269900970214     C                   GOTO      ENDCTW
270000970214    2C                   END
270100970214     C* CONTROLLO FORMALE DATA ANOMALIA
270200990513     C                   CLEAR                   WLBDA8
270300990513     C                   Z-ADD     W4SDAO        G08DAT
270400990513     C                   CALL      'XSRDA8'
270500990513     C                   PARM                    WLBDA8
270600990513    2C     G08ERR        IFEQ      '1'
270700970214     C                   MOVE      *ZEROS        W4SINV
270800970214     C                   MOVEL     MSG(9)        W4ZMSG
270900970214     C                   SETON                                        5628
271000970214     C                   GOTO      ENDCTW
271100970214     C                   ELSE
271200990513     C                   MOVE      G08INV        W4SINV
271300990513     C                   MOVE      G08DAT        W4SDAO
271400970214    2C                   END
271500970214     C* ERRORE SE CHIAVE GIA' ESISTENTE
271600990513     C     KANM4         SETLL     FNANM04L
271700990512     C     KANM4         READE     FNANM04L                               30
271800970214     C     *IN30         DOWEQ     *OFF
271900970214     C     ANMLNP        IFEQ      W4SLNP
272000970214     C                   MOVEL     MSG(32)       W4ZMSG
272100970214     C                   SETON                                        2854
272200970214     C                   GOTO      ENDCTW
272300970214     C                   END
272400990512     C     KANM4         READE     FNANM04L                               30
272500970214     C                   ENDDO
272600970214    1C                   END
272700970214     C     ENDCTW        ENDSR
272800970214     C**************  AGGIORNAMENTO ANOMALIA 300 **********************
272900970214     C     AGG300        BEGSR
273000990512     C     V1HNRR        CHAIN     FNANM00F                           30
273100970214     C     *IN30         IFEQ      *OFF
273200970214     C                   Z-ADD     W4SNRT        ANMSCN
273300990512     C                   UPDATE    FNANM000
273400970214     C                   Z-ADD     W4SNRT        V1SSCN
273500970214     C                   END
273600970214     C                   ENDSR
273700970214     C**************  SCRITTURA ANOMALIA 300     **********************
273800970214     C     WTR300        BEGSR
273900990512     C                   CLEAR                   FNANM000
274000970214     C                   Z-ADD     W4SLNP        ANMLNP
274100970214     C                   Z-ADD     W4SNRT        ANMSCN
274200970214     C                   Z-ADD     W4SINV        ANMDAO
274300970214     C                   Z-ADD     W4SINV        ANMDCH
274400970214     C                   Z-ADD     300           ANMCAA
274500970214     C                   MOVE      D34FGS        ANMFLE
274600970214     C                   MOVE      §7CTAN        ANMTAN
274700970214     C                   MOVE      §7CSAN        ANMSAN
274800970214     C                   MOVE      §7CAIE        ANMAIE
274900970214     C                   MOVEL     '1'           ANMFSA
275000970214     C                   MOVEL     '1'           ANMFSC
275100140523     c* imposto in anmflo data e ora creazione spunta
275200140523     C                   TIME                    TIMEanm          14 0
275300140523     c                   movel     timeanm       wora4             4 0
275400140523     C                   move      timeanm       G08DAT
275500140523     C                   Z-ADD     *ZERO         G08INV
275600140523     C                   MOVEL     *BLANKS       G08ERR
275700140523     C                   CALL      'XSRDA8'
275800140523     C                   PARM                    WLBDA8
275900140523     C                   move      G08INV        WDAT6             6 0
276000140523     c                   movel     wdat6         anmflo
276100140523     c                   move      wora4         anmflo
276200990512     C                   WRITE     FNANM000
276300970214     C                   ENDSR
276301151116     C**********************************************************************
276302151116     C*  FIBSP00F - lnp/clienti con sospensione bolle
276303151116     C**********************************************************************
276304151116     c     sr_fibsp      begsr
276305151116     c                   clear                   w_SiBsp
276306151116    2c                   if        blpccm>0
276307151116     c                   eval      w_clibo=blpccm
276308151116     c                   else
276309151116     c                   eval      w_clibo=blpksc
276310151116    2c                   endif
276311151116     c     w_clibo       chain     fibsp02l
276312151116     c                   if        %found(fibsp02l) and bspfpa='S'
276313151214     c                                              and bspaut=*blanks
276314151116     C                   eval      w_SiBsp='S'
276315151116     c                   else
276316151116     c     blplnp        chain     fibsp01l
276317151116     c                   if        %found(fibsp01l) and bspfpa='S'
276318151214     c                                              and bspaut=*blanks
276319151116     C                   eval      w_SiBsp='S'
276320151116     c                   endif
276321151116     c                   endif
276322151116     c                   endsr
276400960326     C**************  OPERAZIONI INIZIALI  ****************************
276500960326     C     *INZSR        BEGSR
276600960326     C*
276700960326     C                   Z-ADD     1             CODUT
276800960326     C                   CALL      'X§PARUT'
276900960326     C                   PARM                    UT§DSE
277000960326     C                   MOVEL     RAGUT         V1DRSU
277100960329     C                   MOVEL     REC80         CNCR80
277200960327     C*
277300960327     C     *LIKE         DEFINE    NRR1          NRRAL
277400960327     C     *LIKE         DEFINE    NRR1          NRRSAV
277500960327     C     *LIKE         DEFINE    V1CNBR        W1CNBR
277600960328     C     *LIKE         DEFINE    TBLCOD        KCOD
277700960328     C     *LIKE         DEFINE    TBLKEY        KKEY
277800960329     C     *LIKE         DEFINE    D34FGS        SAVFGS
277900960329     C     *LIKE         DEFINE    V1SCAA        DUPCAA
278000960329     C     *LIKE         DEFINE    V1SAAS        DUPAAS
278100960401     C     *LIKE         DEFINE    V1SLNP        DUPLNP
278200960401     C     *LIKE         DEFINE    V1SNRS        DUPNRS
278300960401     C     *LIKE         DEFINE    V1SNSP        DUPNSP
278400960401     C     *LIKE         DEFINE    V1SFLS        DUPFLS
278500960401     C     *LIKE         DEFINE    V1SSCN        DUPSCN
278600960401     C     *LIKE         DEFINE    V1SDAO        DUPDAO
278700960401     C     *LIKE         DEFINE    V1SLNA        DUPLNA
278800971118     C     *LIKE         DEFINE    G08INV        DATEU8
278900960401     C     *LIKE         DEFINE    ANMCAA        WCAA
279000960506     C     *LIKE         DEFINE    ANMFLE        WFLE
279100960402     C     *LIKE         DEFINE    PA1AAS        WAAS
279200960403     C     *LIKE         DEFINE    §7ALCI        WLCI
279300971118     C     *LIKE         DEFINE    D55TFP        WFEL1
279400971118     C     *LIKE         DEFINE    D55TFP        WFEL2
279500041125     C***  *LIKE         DEFINE    D55TFP        SAVOVR
279600960521     C     *LIKE         DEFINE    D44FIL        W44FIL
279700960521     C     *LIKE         DEFINE    D44LNA        W44LNA
279800960521     C     *LIKE         DEFINE    D44NRS        W44NRS
279900960521     C     *LIKE         DEFINE    D44SCN        W44SCN
280000960605     C     *LIKE         DEFINE    ANMLNP        WLNP
280100960605     C     *LIKE         DEFINE    ANMNRS        WNRS
280200960605     C     *LIKE         DEFINE    ANMNSP        WNSP
280300960830     C     *LIKE         DEFINE    ANMFLS        SAVFLS
280400960830     C     *LIKE         DEFINE    ANMSCN        SAVSCN
280500961210     C     *LIKE         DEFINE    DSANM         DSANMX
280600990513     C     *LIKE         DEFINE    W4SDAO        W4SINV
280700970507     C     *LIKE         DEFINE    AR4TRC        WTRC
280800970507     C                   MOVEL     'A'           WTRC
280900960328     C*
281000040531     C     KTAB          klist
281100960328     C                   KFLD                    CODUT
281200960328     C                   KFLD                    KCOD
281300960328     C                   KFLD                    KKEY
281400960429     C*
281500960429     C     KBLP          KLIST
281600960429     C                   KFLD                    WAAS
281700960429     C                   KFLD                    V1SLNP
281800960429     C                   KFLD                    V1SNRS
281900960429     C                   KFLD                    V1SNSP
282000970507     C     KAR4          KLIST
282100970507     C                   KFLD                    WAAS
282200970507     C                   KFLD                    V1SLNP
282300970507     C                   KFLD                    V1SNRS
282400970507     C                   KFLD                    V1SNSP
282500970507     C                   KFLD                    WTRC
282600960605     C*
282700960605     C     KBLP2         KLIST
282800960605     C                   KFLD                    WAAS
282900960605     C                   KFLD                    WLNP
283000960605     C                   KFLD                    WNRS
283100960605     C                   KFLD                    WNSP
283200960429     C*
283300960429     C     KBLT          KLIST
283400960429     C                   KFLD                    V1SFLS
283500960429     C                   KFLD                    V1SLNA
283600960429     C                   KFLD                    V1SNRS
283700960429     C                   KFLD                    V1SSCN
283800960605     C*
283900960605     C     KBLT2         KLIST
284000960605     C                   KFLD                    ANMFLS
284100960605     C                   KFLD                    ANMLNA
284200960605     C                   KFLD                    ANMNRS
284300960605     C                   KFLD                    ANMSCN
284400960401     C*
284500960401     C     KANM1         KLIST
284600990513     C                   KFLD                    WAAS
284700960401     C                   KFLD                    V1SLNP
284800960401     C                   KFLD                    V1SNRS
284900960401     C                   KFLD                    V1SNSP
285000960401     C                   KFLD                    WCAA
285100960401     C*
285200960401     C     KANM2         KLIST
285300960401     C                   KFLD                    V1SFLS
285400960401     C                   KFLD                    V1SLNA
285500960401     C                   KFLD                    V1SNRS
285600960401     C                   KFLD                    V1SSCN
285700960401     C                   KFLD                    WCAA
285800970214     C*
285900970214     C     KANM4         KLIST
286000970214     C                   KFLD                    W4SCAA
286100970214     C                   KFLD                    W4SINV
286200960521     C*
286300960521     C     KANM01        KLIST
286400990513     C                   KFLD                    D44AAS
286500960521     C                   KFLD                    D44FIL
286600960521     C                   KFLD                    D44NRS
286700960521     C                   KFLD                    D44NSP
286800040531     C     Kblp27        KLIST
286900040531     C                   KFLD                    D27AAS
287000040531     C                   KFLD                    D27lnp
287100040531     C                   KFLD                    D27NRS
287200040531     C                   KFLD                    D27NSP
287300960522     C*
287400960522     C     KART01        KLIST
287500960522     C                   KFLD                    D44AAS
287600960522     C                   KFLD                    D44FIL
287700960522     C                   KFLD                    D44NRS
287800960522     C                   KFLD                    D44NSP
287900960521     C*
288000960521     C     KANM02        KLIST
288100960521     C                   KFLD                    W44FIL
288200960521     C                   KFLD                    W44LNA
288300960521     C                   KFLD                    W44NRS
288400960521     C                   KFLD                    W44SCN
288500960326     C*
288600971118     C* DETERMINO DATA DEL GIORNO
288700971118     C*
288800971118     C                   TIME                    W0140            14 0
288900971118     C                   MOVE      W0140         WDTGIO            8 0
289000971118     C                   CLEAR                   WLBDA8
289100971118     C                   Z-ADD     WDTGIO        G08DAT
289200971118     C                   CALL      'XSRDA8'
289300971118     C                   PARM                    WLBDA8
289400971118     C                   MOVE      G08INV        DATEU8
289500971118     C                   MOVEL     WDTGIO        DTGIO6            6 0
289600971118     C                   MOVE      WDTGIO        W0020             2 0
289700971118     C                   MOVE      W0020         DTGIO6
289800960401     C*
289900960329     C                   CLEAR                   SAVFGS
290000960327     C                   EXSR      PULSFL
290100960403     C*  Reperisco livello di chiusura anomalia come IDD
290200960403     C                   MOVEL     '7A'          KCOD
290300960403     C                   MOVEL(P)  '2'           KKEY
290400960403     C     KTAB          CHAIN     TABEL00F                           30
290500960403     C  N30              MOVEL     TBLUNI        DS7A2
290600960403     C   30              CLEAR                   DS7A2
290700960403     C                   MOVE      §7ALCI        WLCI
290800960423     C*
290900021008     C                   MOVE      ')'           VAR               8
291000021008     C                   MOVE      '))'          VAR2              9
291100960521     C*
291200960521     C                   SETOFF                                       9192
291300960523     C                   SETOFF                                       5152
291400960605     C                   CLEAR                   WOPNAR
291500960605     C                   CLEAR                   WOPNBL
291600040615     C***                CLEAR                   WOPNBT
291700000824     C                   MOVE      '/'           WTRAT1
291800000824     C                   MOVE      '/'           WTRAT2
291900960327     C*
292000990513     C                   ENDSR
292100990512** MSG
292200960326Fine scorrimento                                                              01
292300960506Immettere Codice Anomalia valido                                              02
292400960401Immettere Linea di Partenza valida                                            03
292500970507Immettere linea  Segnacollo valida                                            04
292600960506Non è ammesso l'inserimento di anomalie con questo codice                     05
292700960401Immettere Numero Spedizione                                                   06
292800960401Immettere Linea di Arrivo valida                                              07
292900970507Immettere un punto operativo in gestione                                      08
293000960401Immettere Data Apertura anomalia valida                                       09
293001151113Immettere linea di partenza diversa dalla filiale in gestione                 10
293200960401Non ammessa la manutenzione o anomalia già trasmessa                          11
293300960329Anomalia non annullabile                                                      12
293400960329Anomalia già annullata                                                        13
293500960329Anomalia non ripristinabile                                                   14
293600960329Anomalia già ripristinata                                                     15
293700960329Anomalia non manutenzionabile                                                 16
293800960401Immettere Numero Segnacollo                                                   17
293900960401Presente anomalia ancora aperta                                               18
294000970507Se immessa la linea segnacollo immettere anche il numero                      19
294100960401Anomalia già aperta con data > alla data apertura immessa                     20
294200970507Se immesso il numero segnacollo immettere anche la linea                      21
294300960403Anomalia già chiusa in fase IDD                                               22
294400960402Data anomalia > di data odierna. F6 per forzare.                              23
294500000225Se immesso il numero spedizione immettere anche la linea di partenza          24
294600960402Se immesso l'anno e la linea di partenza immettere anche il n.spedizione      25
294700960405Non è possibile inserire più di una volta la stessa anomalia                  26
294800960409Non è possibile,per la stessa bolla,inserire sia anomalia 070 che 071.        27
294900960506Per questa bolla esiste già anomalia XXX-123456789012345678901234 F6 per forz.28
295000960506Per questa bolla esiste già anomalia XXX-123456789012345678901234             29
295100960412Questa anomalia non verrà variata in quanto trasmessa/chiusa.F6 per continuare30
295200960412Anomalia non più esistente                                                    31
295300960418Anomalia già esistente                                                        32
295400960419Alcune anomalie non aggiornate: attendere 1 minuto e riprovare                33
295500960429Impossibile annullare: presente spedizione                                    34
295600960506Per la bolla esiste già anomalia XXX-1234567890123 immessa da xxx.F6 per forz.35
295700970507Immettere linea di arrivo diversa da punto operativo in gestione              36
295800960830Per questa bolla e causale anomalia immettere Fil./N.Segnacollo = xxx/xxxxxxx 37
295900970214Immettere il numero dei ritiri effettuati                                     38
296000970507La spedizione non esiste nell'archivio bolle arrivi: F6 per forzare           39
296100970507Impossibile inserire un MANCA DDT per una spedizione che e' NO DDT
296101151116La spedizione non esiste nell'archivio bolle partenze                         41
296102151116Spedizione già partita OPPURE chiusa in partenza                              42
296103151116Sped. di cliente per cui non è prevista la sospensione autom. in chius. F.V.P.43
296104151116Anomalia non gestibile in arrivo                                              44
296105151116Anomalia non gestibile in partenza                                            45
296200960328** Z01
296300960422F6=CONFERMA F10=Inserimento F24=AlTasti
296400960403F7=Bolla F14=Spunte F3=Fine F24=AlTasti
296500960422F12=Ritorno F24=AlTasti
296600960403** Z02
296700960403F7=Bolla F14=Spunte F3=Fine F12=Ritorno
296800960409F3=Fine  F10=Inserimento  F12=Ritorno
296900960403F6=CONFERMA F3=Fine F12=Ritorno
297000960521F3=Fine  F12=Ritorno
297100960521F14=Spunte F3=Fine F12=Ritorno
297200960521F7=Bolla  F3=Fine F12=Ritorno
297300960422** Z03
297400960422F6=CONFERMA F7=Bolla F3=Fine F24=AlTasti
297500960422F14=Spunte F12=Ritorno F24=AlTasti
297600960403** TAB/TABDES
297700960403IINTERNA
297800960507DI.D.D.
297900960507NNON IDD
298000961118XDISCOR.EXP
