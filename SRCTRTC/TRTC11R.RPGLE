000100021002     H DECEDIT('0,') DATEDIT(*DMY.)
000200890921     H*----------------------------------------------------*
000300930608     H**  PREP. ARCHIVIO DATI BOLLE DA DISCHETTO CLIENTE   *
000400941229     H**                 VERSIONE 95                       *
000500930608     H**  PRENDENDO I DATI DAGLI ARCHIVI SCRITTI DA HALLEY *
000600890921     H*----------------------------------------------------*
000700890921     F*
000800941229     FFNVABAAR  UF   E             DISK
000900020925     FFIVAB00F  O    E             DISK
001000941229     FFNVADAAR  UF   E             DISK
001100020925     FFIVAD00F  O    E             DISK
001200930915     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001300941229     D* DS PER EDITARE I CAMPI NUMERICI IN ASCII
001400941229     D* 10+3
001500941229     D                 DS
001600941229     D  WS15                   1      1
001700941229     D  WI15                   2     11
001800941229     D  WV15                  12     12
001900941229     D  WD15                  13     15
002000941229     D  WASC15                 1     15
002100941229     D*  6+1
002200941229     D                 DS
002300941229     D  WS09                   1      1
002400941229     D  WI09                   2      7
002500941229     D  WV09                   8      8
002600941229     D  WD09                   9      9
002700941229     D  WASC09                 1      9
002800941229     D*  2+3
002900941229     D                 DS
003000941229     D  WS07                   1      1
003100941229     D  WI07                   2      3
003200941229     D  WV07                   4      4
003300941229     D  WD07                   5      7
003400941229     D  WASC07                 1      7
003500921023     C*
003600930915     C                   TIME                    HMS               6 0
003700941229     C                   EXSR      RWVAB                                        LETT/SCR. VAB
003800930408     C*
003900941229     C                   EXSR      RWVAD                                        LETT/SCR. VAD
004000930408     C*
004100930915     C* STAMPA TOTALI
004200930915     C                   EXCEPT    TESTA
004300971022     C     低TDOK        IFGT      *ZERO
004400971022     C     低TDNO        ORGT      *ZERO
004500971022     C                   SETON                                        80
004600971022     C                   ENDIF
004700930915     C                   EXCEPT    FONDO
004800921023     C                   SETON                                        LR
004900910830     C*--------------------------------------------------------
005000020925     C* RWVAB   LEGGE FNVABAAR E SCRIVE FIVAB00F              *
005100910830     C*--------------------------------------------------------
005200941229     C     RWVAB         BEGSR
005300921023     C                   DO        *HIVAL
005400941229     C                   READ      FNVABAA                                70
005500921023     C  N70              DO
005600971021     C                   SETOFF                                       303132
005700971021     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
005800971023     C* OK SE PIENO DI NUMERI O BLANKS SEGUITI DA NUMERI
005900971023     C*    OPPURE TUTTI BLANKS
006000971023     C                   TESTN                   VHBCCM               30
006100971023     C  N30              TESTN                   VHBCCM                 30
006200971023     C  N30              TESTN                   VHBCCM                   30
006300971021     C                   MOVE      VHBCCM        W001A             1
006400971021     C     *IN30         IFEQ      *OFF
006500971021     C     W001A         ORLT      '0'
006600971021     C                   SETON                                        31
006700971021     C                   ELSE
006800971021     C                   MOVE      VHBCCM        VABCCM
006900021009     C                   MOVEL     VABCCM        VABFGS
007000971024     C     VABCCM        IFLE      *ZEROS
007100971024     C                   SETON                                        31
007200971024     C                   ENDIF
007300971021     C                   ENDIF
007400971021     C*
007500971023     C                   TESTN                   VHBLNP               30
007600971023     C  N30              TESTN                   VHBLNP                 30
007700971023     C  N30              TESTN                   VHBLNP                   30
007800971021     C                   MOVE      VHBLNP        W001A             1
007900971021     C     *IN30         IFEQ      *OFF
008000971021     C     W001A         ORLT      '0'
008100971021     C                   SETON                                        31
008200971021     C                   ELSE
008300971021     C                   MOVE      VHBLNP        VABLNP
008400971024     C     VABLNP        IFLE      *ZEROS
008500971024     C                   SETON                                        31
008600971024     C                   ENDIF
008700971021     C                   ENDIF
008800971021     C*
008900971023     C                   TESTN                   VHBAAS               30
009000971023     C  N30              TESTN                   VHBAAS                 30
009100971023     C  N30              TESTN                   VHBAAS                   30
009200971021     C                   MOVE      VHBAAS        W001A             1
009300971021     C     *IN30         IFEQ      *OFF
009400971021     C     W001A         ORLT      '0'
009500971023     C     W001A         ANDNE     *BLANK
009600971021     C                   SETON                                        32
009700971021     C                   MOVE      1997          VABAAS
009800971021     C                   ELSE
009900971021     C                   MOVE      VHBAAS        VABAAS
010000971021     C                   ENDIF
010100971021     C*
010200971023     C                   TESTN                   VHBMGS               30
010300971023     C  N30              TESTN                   VHBMGS                 30
010400971023     C  N30              TESTN                   VHBMGS                   30
010500971021     C                   MOVE      VHBMGS        W001A             1
010600971021     C     *IN30         IFEQ      *OFF
010700971021     C     W001A         ORLT      '0'
010800971023     C     W001A         ANDNE     *BLANK
010900971021     C                   SETON                                        32
011000971021     C                   MOVE      0101          VABMGS
011100971021     C                   ELSE
011200971021     C                   MOVE      VHBMGS        VABMGS
011300971021     C                   ENDIF
011400971021     C*
011500971023     C                   TESTN                   VHBNRS               30
011600971023     C  N30              TESTN                   VHBNRS                 30
011700971023     C  N30              TESTN                   VHBNRS                   30
011800971021     C                   MOVE      VHBNRS        W001A             1
011900971021     C     *IN30         IFEQ      *OFF
012000971021     C     W001A         ORLT      '0'
012100971023     C     W001A         ANDNE     *BLANK
012200971021     C                   SETON                                        32
012300971021     C                   MOVE      *ZERO         VABNRS
012400971021     C                   ELSE
012500971021     C                   MOVE      VHBNRS        VABNRS
012600971021     C                   ENDIF
012700971021     C*
012800971023     C                   TESTN                   VHBNSP               30
012900971023     C  N30              TESTN                   VHBNSP                 30
013000971023     C  N30              TESTN                   VHBNSP                   30
013100971021     C                   MOVE      VHBNSP        W001A             1
013200971021     C     *IN30         IFEQ      *OFF
013300971021     C     W001A         ORLT      '0'
013400971023     C     W001A         ANDNE     *BLANK
013500971021     C                   SETON                                        32
013600971021     C                   MOVE      *ZERO         VABNSP
013700971021     C                   ELSE
013800971021     C                   MOVE      VHBNSP        VABNSP
013900971021     C                   ENDIF
014000971021     C*
014100971023     C                   TESTN                   VHBLNA               30
014200971023     C  N30              TESTN                   VHBLNA                 30
014300971023     C  N30              TESTN                   VHBLNA                   30
014400971021     C                   MOVE      VHBLNA        W001A             1
014500971021     C     *IN30         IFEQ      *OFF
014600971021     C     W001A         ORLT      '0'
014700971023     C     W001A         ANDNE     *BLANK
014800971021     C                   SETON                                        32
014900971021     C                   MOVE      *ZERO         VABLNA
015000971021     C                   ELSE
015100971021     C                   MOVE      VHBLNA        VABLNA
015200971021     C                   ENDIF
015300971021     C*
015400971023     C                   TESTN                   VHBCTR               30
015500971023     C  N30              TESTN                   VHBCTR                 30
015600971023     C  N30              TESTN                   VHBCTR                   30
015700971021     C                   MOVE      VHBCTR        W001A             1
015800971021     C     *IN30         IFEQ      *OFF
015900971021     C     W001A         ORLT      '0'
016000971023     C     W001A         ANDNE     *BLANK
016100971021     C                   SETON                                        32
016200971021     C                   MOVE      *ZERO         VABCTR
016300971021     C                   ELSE
016400971021     C                   MOVE      VHBCTR        VABCTR
016500971021     C                   ENDIF
016600941229     C*
016700971021     C                   MOVE      *ZERO         VABIAS
016800941229     C                   MOVE      VHBIAS        WASC15
016900971023     C                   TESTN                   WI15                 30
017000971023     C  N30              TESTN                   WI15                   30
017100971023     C  N30              TESTN                   WI15                     30
017200971021     C                   MOVE      WI15          W001A             1
017300971021     C     *IN30         IFEQ      *OFF
017400971021     C     W001A         ORLT      '0'
017500971023     C     W001A         ANDNE     *BLANK
017600971021     C                   SETON                                        32
017700971021     C                   MOVE      *ZERO         VABIAS
017800971021     C                   ELSE
017900971021     C                   MOVEL     WI15          VABIAS
018000971021     C                   ENDIF
018100971023     C                   TESTN                   WD15                 30
018200971023     C  N30              TESTN                   WD15                   30
018300971023     C  N30              TESTN                   WD15                     30
018400971021     C                   MOVE      WD15          W001A             1
018500971021     C     *IN30         IFEQ      *OFF
018600971021     C     W001A         ORLT      '0'
018700971023     C     W001A         ANDNE     *BLANK
018800971021     C                   SETON                                        32
018900971021     C                   MOVE      *ZERO         VABIAS
019000971021     C                   ELSE
019100971021     C                   MOVE      WD15          VABIAS
019200971021     C                   ENDIF
019300941229     C*
019400971023     C                   TESTN                   VHBNCL               30
019500971023     C  N30              TESTN                   VHBNCL                 30
019600971023     C  N30              TESTN                   VHBNCL                   30
019700971021     C                   MOVE      VHBNCL        W001A             1
019800971021     C     *IN30         IFEQ      *OFF
019900971021     C     W001A         ORLT      '0'
020000971021     C                   SETON                                        32
020100971021     C                   MOVE      *ZERO         VABNCL
020200971021     C                   ELSE
020300971021     C                   MOVE      VHBNCL        VABNCL
020400971021     C                   ENDIF
020500941229     C*
020600971021     C                   MOVE      *ZERO         VABPKB
020700971021     C                   MOVE      VHBPKB        WASC09
020800971023     C                   TESTN                   WI09                 30
020900971023     C  N30              TESTN                   WI09                   30
021000971023     C  N30              TESTN                   WI09                     30
021100971021     C                   MOVE      WI09          W001A             1
021200971021     C     *IN30         IFEQ      *OFF
021300971021     C     W001A         ORLT      '0'
021400971021     C                   SETON                                        32
021500971021     C                   MOVE      *ZERO         VABPKB
021600971021     C                   ELSE
021700971021     C                   MOVEL     WI09          VABPKB
021800971021     C                   ENDIF
021900971023     C                   TESTN                   WD09                 30
022000971023     C  N30              TESTN                   WD09                     30
022100971021     C                   MOVE      WD09          W001A             1
022200971021     C     *IN30         IFEQ      *OFF
022300971021     C     W001A         ORLT      '0'
022400971023     C     W001A         ANDNE     *BLANK
022500971021     C                   SETON                                        32
022600971021     C                   MOVE      *ZERO         VABPKB
022700971021     C                   ELSE
022800971021     C                   MOVE      WD09          VABPKB
022900971021     C                   ENDIF
023000971021     C*
023100971021     C                   MOVE      *ZERO         VABVLB
023200971021     C                   MOVE      VHBVLB        WASC07
023300971023     C                   TESTN                   WI07                 30
023400971023     C  N30              TESTN                   WI07                   30
023500971023     C  N30              TESTN                   WI07                     30
023600971021     C                   MOVE      WI07          W001A             1
023700971021     C     *IN30         IFEQ      *OFF
023800971021     C     W001A         ORLT      '0'
023900971023     C     W001A         ANDNE     *BLANK
024000971021     C                   SETON                                        32
024100971021     C                   MOVE      *ZERO         VABVLB
024200971021     C                   ELSE
024300971021     C                   MOVEL     WI07          VABVLB
024400971021     C                   ENDIF
024500971023     C                   TESTN                   WD07                 30
024600971023     C  N30              TESTN                   WD07                   30
024700971023     C  N30              TESTN                   WD07                     30
024800971021     C                   MOVE      WD07          W001A             1
024900971021     C     *IN30         IFEQ      *OFF
025000971021     C     W001A         ORLT      '0'
025100971023     C     W001A         ANDNE     *BLANK
025200971021     C                   SETON                                        32
025300971021     C                   MOVE      *ZERO         VABVLB
025400971021     C                   ELSE
025500971021     C                   MOVE      WD07          VABVLB
025600971021     C                   ENDIF
025700941229     C*
025800971021     C                   MOVE      *ZERO         VABQFT
025900971021     C                   MOVE      VHBQFT        WASC15
026000971023     C                   TESTN                   WI15                 30
026100971023     C  N30              TESTN                   WI15                   30
026200971023     C  N30              TESTN                   WI15                     30
026300971021     C                   MOVE      WI15          W001A             1
026400971021     C     *IN30         IFEQ      *OFF
026500971021     C     W001A         ORLT      '0'
026600971023     C     W001A         ANDNE     *BLANK
026700971021     C                   SETON                                        32
026800971021     C                   MOVE      *ZERO         VABQFT
026900971021     C                   ELSE
027000971021     C                   MOVEL     WI15          VABQFT
027100971021     C                   ENDIF
027200971023     C                   TESTN                   WD15                 30
027300971023     C  N30              TESTN                   WD15                   30
027400971023     C  N30              TESTN                   WD15                     30
027500971021     C                   MOVE      WD15          W001A             1
027600971021     C     *IN30         IFEQ      *OFF
027700971021     C     W001A         ORLT      '0'
027800971023     C     W001A         ANDNE     *BLANK
027900971021     C                   SETON                                        32
028000971021     C                   MOVE      *ZERO         VABQFT
028100971021     C                   ELSE
028200971021     C                   MOVE      WD15          VABQFT
028300971021     C                   ENDIF
028400941229     C*
028500971021     C                   MOVE      *ZERO         VABCAS
028600971021     C                   MOVE      VHBCAS        WASC15
028700971023     C                   TESTN                   WI15                 30
028800971023     C  N30              TESTN                   WI15                   30
028900971023     C  N30              TESTN                   WI15                     30
029000971021     C                   MOVE      WI15          W001A             1
029100971021     C     *IN30         IFEQ      *OFF
029200971021     C     W001A         ORLT      '0'
029300971023     C     W001A         ANDNE     *BLANK
029400971021     C                   SETON                                        32
029500971021     C                   MOVE      *ZERO         VABCAS
029600971021     C                   ELSE
029700971021     C                   MOVEL     WI15          VABCAS
029800971021     C                   ENDIF
029900971023     C                   TESTN                   WD15                 30
030000971023     C  N30              TESTN                   WD15                   30
030100971023     C  N30              TESTN                   WD15                     30
030200971021     C                   MOVE      WD15          W001A             1
030300971021     C     *IN30         IFEQ      *OFF
030400971021     C     W001A         ORLT      '0'
030500971023     C     W001A         ANDNE     *BLANK
030600971021     C                   SETON                                        32
030700971021     C                   MOVE      *ZERO         VABCAS
030800971021     C                   ELSE
030900971021     C                   MOVE      WD15          VABCAS
031000971021     C                   ENDIF
031100971021     C*
031200971023     C                   TESTN                   VHBRMN               30
031300971023     C  N30              TESTN                   VHBRMN                 30
031400971023     C  N30              TESTN                   VHBRMN                   30
031500971021     C                   MOVE      VHBRMN        W001A             1
031600971021     C     *IN30         IFEQ      *OFF
031700971021     C     W001A         ORLT      '0'
031800971021     C                   SETON                                        32
031900971021     C                   MOVE      *ZERO         VABRMN
032000971021     C                   ELSE
032100971021     C                   MOVE      VHBRMN        VABRMN
032200971021     C                   ENDIF
032300971021     C*
032400971023     C                   TESTN                   VHBNCD               30
032500971023     C  N30              TESTN                   VHBNCD                 30
032600971023     C  N30              TESTN                   VHBNCD                   30
032700971021     C                   MOVE      VHBNCD        W001A             1
032800971021     C     *IN30         IFEQ      *OFF
032900971021     C     W001A         ORLT      '0'
033000971023     C     W001A         ANDNE     *BLANK
033100971021     C                   SETON                                        32
033200971021     C                   MOVE      *ZERO         VABNCD
033300971021     C                   ELSE
033400971021     C                   MOVE      VHBNCD        VABNCD
033500971021     C                   ENDIF
033600971021     C*
033700971023     C                   TESTN                   VHBNCA               30
033800971023     C  N30              TESTN                   VHBNCA                 30
033900971023     C  N30              TESTN                   VHBNCA                   30
034000971021     C                   MOVE      VHBNCA        W001A             1
034100971021     C     *IN30         IFEQ      *OFF
034200971021     C     W001A         ORLT      '0'
034300971023     C     W001A         ANDNE     *BLANK
034400971021     C                   SETON                                        32
034500971021     C                   MOVE      *ZERO         VABNCA
034600971021     C                   ELSE
034700971021     C                   MOVE      VHBNCA        VABNCA
034800971021     C                   ENDIF
034900971021     C*
035000971023     C                   TESTN                   VHBZNC               30
035100971023     C  N30              TESTN                   VHBZNC                 30
035200971023     C  N30              TESTN                   VHBZNC                   30
035300971021     C                   MOVE      VHBZNC        W001A             1
035400971021     C     *IN30         IFEQ      *OFF
035500971021     C     W001A         ORLT      '0'
035600971023     C     W001A         ANDNE     *BLANK
035700971021     C                   SETON                                        32
035800971021     C                   MOVE      *ZERO         VABZNC
035900971021     C                   ELSE
036000971021     C                   MOVE      VHBZNC        VABZNC
036100971021     C                   ENDIF
036200971021     C*
036300971023     C                   TESTN                   VHBDCR               30
036400971023     C  N30              TESTN                   VHBDCR                 30
036500971023     C  N30              TESTN                   VHBDCR                   30
036600971021     C                   MOVE      VHBDCR        W001A             1
036700971021     C     *IN30         IFEQ      *OFF
036800971021     C     W001A         ORLT      '0'
036900971023     C     W001A         ANDNE     *BLANK
037000971021     C                   SETON                                        32
037100971021     C                   MOVE      *ZERO         VABDCR
037200971021     C                   ELSE
037300971021     C                   MOVE      VHBDCR        VABDCR
037400971021     C                   ENDIF
037500971021     C*
037600971023     C                   TESTN                   VHBHCR               30
037700971023     C  N30              TESTN                   VHBHCR                 30
037800971023     C  N30              TESTN                   VHBHCR                   30
037900971021     C                   MOVE      VHBHCR        W001A             1
038000971021     C     *IN30         IFEQ      *OFF
038100971021     C     W001A         ORLT      '0'
038200971023     C     W001A         ANDNE     *BLANK
038300971021     C                   SETON                                        32
038400971021     C                   MOVE      *ZERO         VABHCR
038500971021     C                   ELSE
038600971021     C                   MOVE      VHBHCR        VABHCR
038700971021     C                   ENDIF
038800941229     C*
038900971021     C                   MOVE      *ZERO         VABVMD
039000971021     C                   MOVE      VHBVMD        WASC15
039100971023     C                   TESTN                   WI15                 30
039200971023     C  N30              TESTN                   WI15                   30
039300971023     C  N30              TESTN                   WI15                     30
039400971021     C                   MOVE      WI15          W001A             1
039500971021     C     *IN30         IFEQ      *OFF
039600971021     C     W001A         ORLT      '0'
039700971023     C     W001A         ANDNE     *BLANK
039800971021     C                   SETON                                        32
039900971021     C                   MOVE      *ZERO         VABVMD
040000971021     C                   ELSE
040100971021     C                   MOVEL     WI15          VABVMD
040200971021     C                   ENDIF
040300971023     C                   TESTN                   WD15                 30
040400971023     C  N30              TESTN                   WD15                   30
040500971023     C  N30              TESTN                   WD15                     30
040600971021     C                   MOVE      WD15          W001A             1
040700971021     C     *IN30         IFEQ      *OFF
040800971021     C     W001A         ORLT      '0'
040900971023     C     W001A         ANDNE     *BLANK
041000971021     C                   SETON                                        32
041100971021     C                   MOVE      *ZERO         VABVMD
041200971021     C                   ELSE
041300971021     C                   MOVE      WD15          VABVMD
041400971021     C                   ENDIF
041500971021     C*
041600971023     C                   TESTN                   VHBANT               30
041700971023     C  N30              TESTN                   VHBANT                 30
041800971023     C  N30              TESTN                   VHBANT                   30
041900971021     C                   MOVE      VHBANT        W001A             1
042000971021     C     *IN30         IFEQ      *OFF
042100971021     C     W001A         ORLT      '0'
042200971023     C     W001A         ANDNE     *BLANK
042300971021     C                   SETON                                        32
042400971021     C                   MOVE      *ZERO         VABANT
042500971021     C                   ELSE
042600971021     C                   MOVE      VHBANT        VABANT
042700971021     C                   ENDIF
042800971021     C*
042900971021     C* CAMPI ALFABETICI:
043000971021     C                   MOVE      VHBATB        VABATB
043100971021     C                   MOVE      VHBCBO        VABCBO
043200971021     C                   MOVE      VHBRSD        VABRSD
043300971021     C                   MOVE      VHBRD2        VABRD2
043400971021     C                   MOVE      VHBIND        VABIND
043500971021     C                   MOVE      VHBCAD        VABCAD
043600971021     C                   MOVE      VHBLOD        VABLOD
043700971021     C                   MOVE      VHBPRD        VABPRD
043800971021     C                   MOVE      VHBNZD        VABNZD
043900971021     C                   MOVE      VHBGC1        VABGC1
044000971021     C                   MOVE      VHBGC2        VABGC2
044100971021     C                   MOVE      VHBTSP        VABTSP
044200971021     C                   MOVE      VHBVAS        VABVAS
044300971021     C                   MOVE      VHBNAS        VABNAS
044400971021     C                   MOVE      VHBTIC        VABTIC
044500971021     C                   MOVE      VHBVCA        VABVCA
044600971021     C                   MOVE      VHBGCA        VABGCA
044700971021     C                   MOVE      VHBRMA        VABRMA
044800971021     C                   MOVE      VHBXCO        VABXCO
044900971021     C                   MOVE      VHBNOT        VABNOT
045000971021     C                   MOVE      VHBNT2        VABNT2
045100971021     C                   MOVE      VHBCTM        VABCTM
045200971021     C                   MOVE      VHBFFD        VABFFD
045300971021     C                   MOVE      VHBTCR        VABTCR
045400971021     C                   MOVE      VHBCTS        VABCTS
045500971021     C                   MOVE      VHBFTM        VABFTM
045600971021     C                   MOVE      VHBVAD        VABVAD
045700971021     C                   MOVE      VHBGMA        VABGMA
045800971021     C                   MOVE      VHBGGA        VABGGA
045900971021     C                   MOVE      VHBGVA        VABGVA
046000971021     C                   MOVE      VHBTC1        VABTC1
046100971021     C                   MOVE      VHBTC2        VABTC2
046200971021     C                   MOVE      VHBSCL        VABSCL
046300971021     C                   MOVE      VHBRMO        VABRMO
046400971021     C                   MOVE      VHBCMO        VABCMO
046500971021     C                   MOVE      VHBNMO        VABNMO
046600971021     C* ABBLENCO SE INSERITO CARATTERE X PCSUPPORT>HALLEY
046700971021     C* NELLA NAZIONE ORIGINALE
046800971021     C                   MOVE      VHBNMO        WA1D              1
046900971021     C                   MOVEL     VHBNMO        WA1S              1
047000971021     C     WA1D          IFNE      *BLANK
047100971021     C     WA1S          ANDEQ     *BLANK
047200971021     C                   MOVE      *BLANK        VABNMO
047300971021     C                   END
047400930409     C*
047500971021     C  N31              ADD       1             低TROK            5 0
047600971021     C   32              ADD       1             低TRMO            5 0
047700971021     C   31              ADD       1             低TRNO            5 0
047800020925     C  N31              WRITE     FIVAB000
047900941229     C                   DELETE    FNVABAA
048000921023     C                   END
048100921023     C  N70              END
048200910830     C                   ENDSR
048300930408     C/SPACE 005
048400930408     C*--------------------------------------------------------
048500020925     C* RWVAD   LEGGE FNVADAAR E SCRIVE FIVAD00F              *
048600930408     C*--------------------------------------------------------
048700941229     C     RWVAD         BEGSR
048800930408     C                   DO        *HIVAL
048900941229     C                   READ      FNVADAA                                70
049000930408     C  N70              DO
049100971021     C                   SETOFF                                       303132
049200971021     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
049300971023     C                   TESTN                   VHDCCM               30
049400971023     C  N30              TESTN                   VHDCCM                 30
049500971023     C  N30              TESTN                   VHDCCM                   30
049600971021     C                   MOVE      VHDCCM        W001A             1
049700971021     C     *IN30         IFEQ      *OFF
049800971021     C     W001A         ORLT      '0'
049900971021     C                   SETON                                        31
050000971021     C                   ELSE
050100971021     C                   MOVE      VHDCCM        VADCCM
050200021009     C                   MOVEl     VADCCM        VADFGS
050300971027     C     VADCCM        IFLE      *ZEROS
050400971027     C                   SETON                                        31
050500971027     C                   ENDIF
050600971021     C                   ENDIF
050700971021     C*
050800971023     C                   TESTN                   VHDLNP               30
050900971023     C  N30              TESTN                   VHDLNP                 30
051000971023     C  N30              TESTN                   VHDLNP                   30
051100971022     C                   MOVE      VHDLNP        W001A             1
051200971022     C     *IN30         IFEQ      *OFF
051300971022     C     W001A         ORLT      '0'
051400971022     C                   SETON                                        31
051500971022     C                   ELSE
051600971022     C                   MOVE      VHDLNP        VADLNP
051700971027     C     VADLNP        IFLE      *ZEROS
051800971027     C                   SETON                                        31
051900971027     C                   ENDIF
052000971022     C                   ENDIF
052100971022     C*
052200971023     C                   TESTN                   VHDAAS               30
052300971023     C  N30              TESTN                   VHDAAS                 30
052400971023     C  N30              TESTN                   VHDAAS                   30
052500971022     C                   MOVE      VHDAAS        W001A             1
052600971022     C     *IN30         IFEQ      *OFF
052700971022     C     W001A         ORLT      '0'
052800971022     C                   SETON                                        31
052900971022     C                   ELSE
053000971022     C                   MOVE      VHDAAS        VADAAS
053100971022     C                   ENDIF
053200971022     C*
053300971023     C                   TESTN                   VHDNRS               30
053400971023     C  N30              TESTN                   VHDNRS                 30
053500971023     C  N30              TESTN                   VHDNRS                   30
053600971022     C                   MOVE      VHDNRS        W001A             1
053700971022     C     *IN30         IFEQ      *OFF
053800971022     C     W001A         ORLT      '0'
053900971023     C     W001A         ANDNE     *BLANK
054000971022     C                   SETON                                        31
054100971022     C                   ELSE
054200971022     C                   MOVE      VHDNRS        VADNRS
054300971022     C                   ENDIF
054400971022     C*
054500971023     C                   TESTN                   VHDNSP               30
054600971023     C  N30              TESTN                   VHDNSP                 30
054700971023     C  N30              TESTN                   VHDNSP                   30
054800971022     C                   MOVE      VHDNSP        W001A             1
054900971022     C     *IN30         IFEQ      *OFF
055000971022     C     W001A         ORLT      '0'
055100971022     C                   SETON                                        31
055200971022     C                   ELSE
055300971022     C                   MOVE      VHDNSP        VADNSP
055400971022     C                   ENDIF
055500971022     C*
055600971023     C                   TESTN                   VHDNCL               30
055700971023     C  N30              TESTN                   VHDNCL                 30
055800971023     C  N30              TESTN                   VHDNCL                   30
055900971022     C                   MOVE      VHDNCL        W001A             1
056000971022     C     *IN30         IFEQ      *OFF
056100971022     C     W001A         ORLT      '0'
056200971022     C                   SETON                                        31
056300971022     C                   ELSE
056400971022     C                   MOVE      VHDNCL        VADNCL
056500971022     C                   ENDIF
056600971022     C*
056700971023     C                   TESTN                   VHDNCD               30
056800971023     C  N30              TESTN                   VHDNCD                 30
056900971023     C  N30              TESTN                   VHDNCD                   30
057000971022     C                   MOVE      VHDNCD        W001A             1
057100971022     C     *IN30         IFEQ      *OFF
057200971022     C     W001A         ORLT      '0'
057300971023     C     W001A         ANDNE     *BLANK
057400971022     C                   SETON                                        31
057500971022     C                   ELSE
057600971022     C                   MOVE      VHDNCD        VADNCD
057700971022     C                   ENDIF
057800971022     C*
057900971023     C                   TESTN                   VHDNCA               30
058000971023     C  N30              TESTN                   VHDNCA                 30
058100971023     C  N30              TESTN                   VHDNCA                   30
058200971022     C                   MOVE      VHDNCA        W001A             1
058300971022     C     *IN30         IFEQ      *OFF
058400971022     C     W001A         ORLT      '0'
058500971023     C     W001A         ANDNE     *BLANK
058600971022     C                   SETON                                        31
058700971022     C                   ELSE
058800971022     C                   MOVE      VHDNCA        VADNCA
058900971022     C                   ENDIF
059000971022     C*
059100971021     C* ALFABETICI
059200971021     C                   MOVE      VHDATB        VADATB
059300971022     C                   MOVE      VHDCDP        VADCDP
059400971021     C*
059500971022     C  N31              ADD       1             低TDOK            5 0
059600971022     C   31              ADD       1             低TDNO            5 0
059700020925     C  N31              WRITE     FIVAD000
059800941229     C                   DELETE    FNVADAA
059900930408     C                   END
060000930408     C  N70              END
060100930408     C                   ENDSR
060200930915     OQSYSPRT   E            TESTA          2 02
060300941229     O                                         +  5 'CARICA *NUOVE* BOLLE'
060400930915     O                                         +  1 'DA HALLEY'
060500930915     O                                         +  1 'CLIENTE'
060600941229     O                       VABCCM            +  1 '   .    '
060700930915     O                       UDATE              100 '  /  /  '
060800930915     O                       HMS                110 '  :  :  '
060900941229     O                                          120 'TRTC11R'
061000930915     O                                          128 'PAG.'
061100930915     O                       PAGE          Z    132
061200930915     O          E            FONDO       3
061300971022     O                                           20 '*** TESTATE SPEDIZ.:'
061400971022     O                                         +  5 'OK:'
061500971022     O                       低TROK        2   +  1
061600971022     O                                         +  5 'MODIFICATE:'
061700971022     O                       低TRMO        2   +  1
061800971022     O                                         +  5 'SCARTATE:'
061900971022     O                       低TRNO        2   +  1
062000971022     O          E    80      FONDO       1
062100971022     O                                           20 '* DETTAGLIO COLLI  :'
062200971022     O                                         +  5 'OK:'
062300971022     O                       低TDOK        2   +  1
062400971022     O                                         +  5 'SCARTATI:'
062500971022     O                       低TDNO        2   +  1
