000100891030     H DFTNAME(CN0500) DATEDIT(*YMD)
000200891030     F*----------------------* USO DEGLI INDICATORI *----------------*
000300931209     F*  KC        FINE                                              *
000400940228     F*  KE        RIPRISTINO                                        *
000500940228     F*  KF        CONFERMA                                          *
000600931209     F*  KL        RITORNO                                           *
000700931209     F*  KQ        ANNULLAMENTO                                      *
000800891030     F*--------------------------------------------------------------*
000900940927     FTRTB69D   CF   E             WORKSTN
001000940418     F*----------
001100891030     FTABEL00F  UF A E           K DISK
001200940418     D*--------------------------------------------------------------*
001300940418     D*   SCHIERE                                                    *
001400940701     D*--------------------------------------------------------------*
001500111028     D ERR             S             70    DIM(11) CTDATA PERRCD(1)
001600940928     D COST            S             16    DIM(3) CTDATA PERRCD(1)
001700940928     D*--------------------------------------------------------------*
001800940929     D CODP            S              2    DIM(30)
001900041116     D C2A             S              3    DIM(250)
002000041116     D D2A             S             25    DIM(250)
002100110530     D D2Al            S             75    DIM(250)
002200041116     D T2Z             S              1    DIM(250)
002300940927     D C2D             S              1    DIM(50)
002400940928     D D2D             S             20    DIM(50)
002500021023     D C7O             S              1    DIM(50)
002600021023     D D7O             S             20    DIM(50)
002700940928     D*--------------------------------------------------------------*
002800940928     D VTI             S              1    DIM(5)
002900940928     D VDI             S              1    DIM(5)
003000940930     D VDD             S             12    DIM(5)
003100021023     D WTI             S              1    DIM(5)
003200021023     D WDI             S              1    DIM(5)
003300021023     D WDD             S             10    DIM(5)
003400021023     D WCA             S              1    DIM(5)
003500021023     D WCD             S             10    DIM(5)
003600031215     D WTI7            S              1    DIM(3)
003700031215     D WDI7            S              1    DIM(3)
003800031215     D WDD7            S             10    DIM(3)
003900031215     D WCA7            S              1    DIM(3)
004000031215     D WCD7            S             10    DIM(3)
004100110530     D Vmt             S              3    DIM(5)
004200110530     D Vmtd            S             74    DIM(5)
004300940928     D*--------------------------------------------------------------*
004400940929     D IN1             S              2  0 DIM(5) CTDATA PERRCD(5)
004500940929     D IN2             S              2  0 DIM(5) CTDATA PERRCD(5)
004600021024     D IN3             S              2  0 DIM(5) CTDATA PERRCD(5)
004700031215     D IN4             S              2  0 DIM(3) CTDATA PERRCD(3)
004800031215     D IN5             S              2  0 DIM(3) CTDATA PERRCD(3)
004900031215     D IN6             S              2  0 DIM(3) CTDATA PERRCD(3)
005000940928     D*--------------------------------------------------------------*
005100940928     D*  DS                                                          *
005200940928     D*--------------------------------------------------------------*
005300940929     D DSTB69        E DS                  EXTNAME(DS7PQRS)
005400940927     D DS7Q          E DS
005500940928     D  TIP                   78     82
005600940928     D                                     DIM(5)
005700940928     D  DSP                   83     87
005800940928     D                                     DIM(5)
005900021023     D DS7W          E DS
006000021023     D  TTI                    1      5
006100021023     D                                     DIM(5)
006200021023     D  TDI                    6     10
006300021023     D                                     DIM(5)
006400021023     D  TCA                   11     15
006500021023     D                                     DIM(5)
006600031215     D  TTI7                  16     18
006700031215     D                                     DIM(3)
006800031215     D  TDI7                  19     21
006900031215     D                                     DIM(3)
007000031215     D  TCA7                  22     24
007100031215     D                                     DIM(3)
007200110530     D  Tmt                   25     39
007300110530     D                                     DIM(5)
007400940927     D DS2A          E DS
007500940927     D DS2Z          E DS
007600900608      *
007700891030     D KPJBA         E DS
007800911118      *
007900911118     D UTEDSE        E DS                  EXTNAME(UTEDSE0F)
008000111028
008100111028     d tibs02ds      e ds                  inz
008200940228      *
008300940228     D                SDS
008400940927     D  V1CPGM                 1     10
008500891030     I*--------------------------------------------------------------*
008600891030     I*                        I N P U T                             *
008700891030     I*--------------------------------------------------------------*
008800940928     ITB69D04
008900940928     I              V4C4T1                      VTI(1)
009000940928     I              V4C4T2                      VTI(2)
009100940928     I              V4C4T3                      VTI(3)
009200940928     I              V4C4T4                      VTI(4)
009300940928     I              V4C4T5                      VTI(5)
009400940928     I              V4C4D1                      VDI(1)
009500940928     I              V4C4D2                      VDI(2)
009600940928     I              V4C4D3                      VDI(3)
009700940928     I              V4C4D4                      VDI(4)
009800940928     I              V4C4D5                      VDI(5)
009900940928     I              V4D4D1                      VDD(1)
010000940928     I              V4D4D2                      VDD(2)
010100940928     I              V4D4D3                      VDD(3)
010200940928     I              V4D4D4                      VDD(4)
010300940928     I              V4D4D5                      VDD(5)
010400021023     ITB69D07
010500021023     I              V7C6T1                      WTI(1)
010600021023     I              V7C6T2                      WTI(2)
010700021023     I              V7C6T3                      WTI(3)
010800021023     I              V7C6T4                      WTI(4)
010900021023     I              V7C6T5                      WTI(5)
011000021023     I              V7C6D1                      WDI(1)
011100021023     I              V7C6D2                      WDI(2)
011200021023     I              V7C6D3                      WDI(3)
011300021023     I              V7C6D4                      WDI(4)
011400021023     I              V7C6D5                      WDI(5)
011500021023     I              V7D6D1                      WDD(1)
011600021023     I              V7D6D2                      WDD(2)
011700021023     I              V7D6D3                      WDD(3)
011800021023     I              V7D6D4                      WDD(4)
011900021023     I              V7D6D5                      WDD(5)
012000021023     I              V7CAT1                      WCA(1)
012100021023     I              V7CAT2                      WCA(2)
012200021023     I              V7CAT3                      WCA(3)
012300021023     I              V7CAT4                      WCA(4)
012400021023     I              V7CAT5                      WCA(5)
012500021023     I              V7CAD1                      WCD(1)
012600021023     I              V7CAD2                      WCD(2)
012700021023     I              V7CAD3                      WCD(3)
012800021023     I              V7CAD4                      WCD(4)
012900021023     I              V7CAD5                      WCD(5)
013000031215      * tipologia 7
013100031215     I              V7C7T1                      WTI7(1)
013200031215     I              V7C7T2                      WTI7(2)
013300031215     I              V7C7T3                      WTI7(3)
013400031215     I              V7C7D1                      WDI7(1)
013500031215     I              V7C7D2                      WDI7(2)
013600031215     I              V7C7D3                      WDI7(3)
013700031215     I              V7D7D1                      WDD7(1)
013800031215     I              V7D7D2                      WDD7(2)
013900031215     I              V7D7D3                      WDD7(3)
014000031215     I              V7CAT71                     WCA7(1)
014100031215     I              V7CAT72                     WCA7(2)
014200031215     I              V7CAT73                     WCA7(3)
014300031215     I              V7CAD71                     WCD7(1)
014400031215     I              V7CAD72                     WCD7(2)
014500031215     I              V7CAD73                     WCD7(3)
014600110530     ITB69D08
014700110530     I              V8Cod1                      Vmt(1)
014800110530     I              V8Cod2                      Vmt(2)
014900110530     I              V8Cod3                      Vmt(3)
015000110530     I              V8Cod4                      Vmt(4)
015100110530     I              V8Cod5                      Vmt(5)
015200110530     I              V8Cod1d                     Vmtd(1)
015300110530     I              V8Cod2d                     Vmtd(2)
015400110530     I              V8Cod3d                     Vmtd(3)
015500110530     I              V8Cod4d                     Vmtd(4)
015600110530     I              V8Cod5d                     Vmtd(5)
015700931209     C*--------------------------------------------------------------*
015800931209     C*    C I C L O    P R I N C I P A L E                          *
015900931209     C*--------------------------------------------------------------*
016000940928     C     *ENTRY        PLIST
016100940928     C                   PARM                    KPJBA
016200940928     C                   MOVEL     KPJBU         DSTB69
016300940930     C                   MOVEL     ' '           DS7ERR
016400940930     C                   MOVEL     ' '           DS7F03
016500941104     C                   MOVEL     *BLANKS       DS7MSG
016600960110     C                   MOVEL     'N'           WFINE
016700940928     C*  Prima volta
016800940928     C     WPRIMA        IFNE      'N'
016900940929     C                   MOVEL     'N'           WPRIMA            1
017000940928     C                   EXSR      DEFCAM
017100940928     C                   END
017200940928     C*  Carico dati da tabella
017300940928     C                   EXSR      INZ01
017400940928     C*  Loop gestione videate
017500940927     C     WFINE         DOWNE     'S'
017600940928     C     WTPVID        CASEQ     '1'           GESD01
017700940928     C     WTPVID        CASEQ     '2'           GESD02
017800021023     C     WTPVID        CASEQ     '3'           GESD03
017900110530     C     WTPVID        CASEQ     '4'           GESD04
018000931209     C                   END
018100931209     C                   END
018200940929     C*
018300931209     C* Fine programma
018400931209     C     FINE          TAG
018500940929     C                   MOVEL     DSTB69        KPJBU
018600960110     C* Libero tabel
018700960110     C     *LOVAL        SETLL     TABEL00F
018800940928     C* Se il programma è richiamato da altre funzioni chiudo in LR
018900940928     C* altrimenti chiudo in RT
019000940929     C     DS7RIC        IFEQ      'S'
019100931209     C                   SETON                                        LR
019200940928     C                   ELSE
019300940928     C                   SETON                                        RT
019400940928     C                   END
019500940928     C*---------------------------------------------------*
019600940928     C*  GESD01: Gestione prima videata                   *
019700940928     C*---------------------------------------------------*
019800940928     C     GESD01        BEGSR
019900940928     C*---------------------------------------------------*
020000931209     C*  Emissione videata
020100940929     C                   WRITE     TB69T01
020200940928     C                   EXFMT     TB69D03
020300940928     C                   SETOFF                                       404128
020400940929     C                   SETOFF                                       424344
020500940929     C                   SETOFF                                       454647
020600940929     C                   SETOFF                                       4849
020700940418     C                   MOVEL     *BLANKS       $MSG
020800940928     C*
020900931209     C*  Fine elaborazione
021000931209     C     *INKC         IFEQ      '1'
021100940929     C                   MOVEL     'S'           WFINE
021200940929     C                   MOVEL     '1'           DS7F03
021300940928     C                   GOTO      FINVD1
021400931209     C                   END
021500931209     C*----------------------------------------------------*
021600931209     C*  Ritorno prima videata
021700940928     C     *INKL         IFEQ      '1'
021800940929     C                   MOVEL     'S'           WFINE
021900960110     C                   MOVEL     '2'           DS7F03
022000940928     C                   GOTO      FINVD1
022100931209     C                   END
022200960112     C*----------------------------------------------------*
022300960112     C     DS7GES        IFEQ      ' '
022400960112     C     DS7OPZ        ANDEQ     '1'
022500960112     C*----------------------------------------------------*
022600931209     C*  Annullamento
022700931209     C     *INKQ         IFEQ      '1'
022800931209     C                   EXSR      ANNUL
022900111028     C                   EXSR      ANNU2
023000940929     C                   MOVEL     'S'           WFINE
023100940928     C                   GOTO      FINVD1
023200931209     C                   END
023300940725     C*  controlli
023400940928     C                   EXSR      CTR01
023500940928     C   28              GOTO      FINVD1
023600931209     C*  Rirpristino
023700940928     C     *INKE         IFEQ      '1'
023800931209     C                   EXSR      RIPRIS
023900111028     C                   EXSR      RIPRI2
024000940929     C                   MOVEL     'S'           WFINE
024100940928     C                   GOTO      FINVD1
024200940928     C                   END
024300940928     C*----------------------------------------------------*
024400940928     C                   END
024500940928     C*----------------------------------------------------*
024600940929     C                   MOVEL     '2'           WTPVID
024700940928     C*
024800940928     C     FINVD1        ENDSR
024900940928     C*----------------------------------------------------*
025000940928     C*  GESD02: Gestione seconda videata                  *
025100940928     C*----------------------------------------------------*
025200940928     C     GESD02        BEGSR
025300940928     C*
025400940928     C*  Emissione videata
025500940929     C                   WRITE     TB69T01
025600940928     C                   EXFMT     TB69D04
025700940928     C                   SETOFF                                       404142
025800940928     C                   SETOFF                                       434445
025900940928     C                   SETOFF                                       464748
026000940928     C                   SETOFF                                       495051
026100940928     C                   SETOFF                                       28
026200940928     C                   MOVEL     *BLANKS       $MSG
026300940928     C*  Fine elaborazione
026400940928     C     *INKC         IFEQ      '1'
026500940929     C                   MOVEL     'S'           WFINE
026600940929     C                   MOVEL     '1'           DS7F03
026700940928     C                   GOTO      FINVD2
026800940928     C                   END
026900940928     C*  Ritorno prima videata
027000940928     C     *INKL         IFEQ      '1'
027100940929     C                   MOVEL     '1'           WTPVID            1
027200940928     C                   GOTO      FINVD2
027300940928     C                   END
027400940928     C*  Annullamento
027500021024     C*          *INKQ     IFEQ '1'
027600021024     C*                    EXSR ANNUL
027700021024     C*                    MOVEL'S'       WFINE
027800021024     C*                    GOTO FINVD2
027900021024     C*                    END
028000940928     C*  controlli
028100940928     C                   EXSR      CTR02
028200940929     C   28              GOTO      FINVD2
028300940928     C*  Ripristino
028400021024     C*          *INKE     IFEQ '1'
028500021024     C*                    EXSR RIPRIS
028600021024     C*                    MOVEL'S'       WFINE
028700021024     C*                    GOTO FINVD2
028800021024     C*                    END
028900940928     C*  Conferma
029000021024     C*          *INKF     IFEQ '1'
029100021024     C*                    EXSR AGGIOR
029200021024     C*                    MOVEL'S'       WFINE
029300021024     C*                    GOTO FINVD2
029400021024     C*                    END
029500021023     C                   MOVEL     '3'           WTPVID
029600940928     C*----------------------------------------------------*
029700940929     C     FINVD2        ENDSR
029800021023     C*----------------------------------------------------*
029900021023     C*  GESD03: Gestione TERZA VIDEATA 7W                 *
030000021023     C*----------------------------------------------------*
030100021023     C     GESD03        BEGSR
030200021023     C*
030300021023     C*  Emissione videata
030400021023     C                   WRITE     TB69T01
030500021023     C                   EXFMT     TB69D07
030600021024     C                   SETOFF                                       404142
030700021024     C                   SETOFF                                       434445
030800021024     C                   SETOFF                                       464748
030900021024     C                   SETOFF                                       495051
031000031215     C                   SETOFF                                       525354
031100031215     C                   SETOFF                                       575859
031200111028     C                   SETOFF                                       2870
031300021023     C                   MOVEL     *BLANKS       $MSG
031400021023     C*  Fine elaborazione
031500021023     C     *INKC         IFEQ      '1'
031600021023     C                   MOVEL     'S'           WFINE
031700021023     C                   MOVEL     '1'           DS7F03
031800021023     C                   GOTO      FINVD3
031900021023     C                   END
032000021023     C*  Ritorno prima videata
032100021023     C     *INKL         IFEQ      '1'
032200021023     C                   MOVEL     '2'           WTPVID            1
032300021023     C                   GOTO      FINVD3
032400021023     C                   END
032500021023     C*----------------------------------------------------*
032600021023     C     DS7GES        IFEQ      ' '
032700021023     C     DS7OPZ        ANDEQ     '1'
032800021023     C*----------------------------------------------------*
032900021023     C*  Annullamento
033000111028     C**!!!*INKQ         IFEQ      '1'
033100111028     C**!!!              EXSR      ANNUL
033200111028     C**!!!              EXSR      ANNU2
033300111028     C**!!!              MOVEL     'S'           WFINE
033400111028     C**!!!              GOTO      FINVD3
033500111028     C**!!!              END
033600021023     C*  controlli
033700021023     C                   EXSR      CTR03
033800021023     C   28              GOTO      FINVD3
033900021023     C*  Ripristino
034000110530     C*    *INKE         IFEQ      '1'
034100110530     C*                  EXSR      RIPRIS
034200110530     C*                  EXSR      RIPRI2
034300110530     C*                  MOVEL     'S'           WFINE
034400110530     C*                  GOTO      FINVD3
034500110530     C*                  END
034600021023     C*  Conferma
034700110530     C*    *INKF         IFEQ      '1'
034800110530     C*                  EXSR      AGGIOR
034900110530     C*                  EXSR      AGGIO2
035000110530     C*                  MOVEL     'S'           WFINE
035100110530     C*                  GOTO      FINVD3
035200110530     C*                  END
035300021023     C*----------------------------------------------------*
035400110615     C*mm                ELSE
035500110615     C*mm                MOVEL     'S'           WFINE
035600021023     C*----------------------------------------------------*
035700021023     C                   END
035800110530     C                   MOVEL     '4'           WTPVID
035900021023     C*----------------------------------------------------*
036000021023     C     FINVD3        ENDSR
036100940928     C*----------------------------------------------------*
036200110530     C*----------------------------------------------------*
036300110530     C*  GESD04: Gestione quarta VIDEATA 7W                *
036400110530     C*----------------------------------------------------*
036500110530     C     GESD04        BEGSR
036600110530     C*
036700110530     C*  Emissione videata
036800110530     C                   WRITE     TB69T01
036900110530     C                   EXFMT     TB69D08
037000110530     C                   SETOFF                                       404142
037100110530     C                   SETOFF                                       434445
037200110530     C                   SETOFF                                       464748
037300110530     C                   SETOFF                                       495051
037400110530     C                   SETOFF                                       525354
037500110530     C                   SETOFF                                       575859
037600110530     C                   SETOFF                                       28
037700110530     C                   MOVEL     *BLANKS       $MSG
037800110530     C*  Fine elaborazione
037900110530     C     *INKC         IFEQ      '1'
038000110530     C                   MOVEL     'S'           WFINE
038100110530     C                   MOVEL     '1'           DS7F03
038200110530     C                   GOTO      FINVD4
038300110530     C                   END
038400110530     C*  Ritorno prima videata
038500110530     C     *INKL         IFEQ      '1'
038600110530     C                   MOVEL     '3'           WTPVID            1
038700110530     C                   GOTO      FINVD4
038800110530     C                   END
038900110530     C*----------------------------------------------------*
039000110530     C     DS7GES        IFEQ      ' '
039100110530     C     DS7OPZ        ANDEQ     '1'
039200110530     C*----------------------------------------------------*
039300110530     C*  Annullamento
039400110530     C     *INKQ         IFEQ      '1'
039500110530     C                   EXSR      ANNUL
039600110530     C                   EXSR      ANNU2
039700110530     C                   MOVEL     'S'           WFINE
039800110530     C                   GOTO      FINVD4
039900110530     C                   END
040000110530     C*  controlli
040100110530     C                   EXSR      CTR04
040200110530     C   28              GOTO      FINVD4
040300110530     C*  Ripristino
040400110530     C     *INKE         IFEQ      '1'
040500110530     C                   EXSR      RIPRIS
040600110530     C                   EXSR      RIPRI2
040700110530     C                   MOVEL     'S'           WFINE
040800110530     C                   GOTO      FINVD4
040900110530     C                   END
041000110530     C*  Conferma
041100110530     C     *INKF         IFEQ      '1'
041200110530     C                   EXSR      AGGIOR
041300110530     C                   EXSR      AGGIO2
041400110530     C                   MOVEL     'S'           WFINE
041500110530     C                   GOTO      FINVD4
041600110530     C                   END
041700110530     C*----------------------------------------------------*
041800110530     C                   ELSE
041900110530     C                   MOVEL     'S'           WFINE
042000110530     C                   END
042100110530     C     FINVD4        ENDSR
042200110530     C*----------------------------------------------------*
042300940928     C*  INZ01   Gestione inizializzazione prima videata   *
042400940928     C*----------------------------------------------------*
042500940927     C     INZ01         BEGSR
042600940928     C*----------------------------------------------------*
042700940928     C                   SETOFF                                       010203
042800940928     C                   SETOFF                                       0405
042900940928     C                   SETOFF                                       284041
043000940929     C                   SETOFF                                       424344
043100940929     C                   SETOFF                                       454647
043200940929     C                   SETOFF                                       4849
043300940928     C                   SETOFF                                       929390
043400940928     C                   MOVEL     *BLANKS       $MSG
043500940928     C                   MOVEL     'N'           WFINE             1
043600940928     C                   MOVEL     '1'           WTPVID
043700960110     C     DS7KSC        IFNE      0
043800960110     C                   MOVEA     DS7CDP        CODP
043900960110     C                   END
044000940928     C*
044100940929     C                   MOVEL     '7Q'          KCOD
044200940928     C                   MOVEL     *BLANKS       KKEY
044300940929     C                   MOVEL     DS7COD        KKEY
044400940929     C                   MOVEL     DS7COD        V3CCOD
044500940928     C                   CLEAR                   DS7Q
044600940928     C     KTAB2         CHAIN     TABEL00F                           31
044700940930     C  N31              MOVEL     TBLUNI        DS7Q
044800021023      * TABELLA 7W CORRELATA ESTENSIONE 7Q
044900021119     C                   CLEAR                   DS7W
045000021119     C                   MOVEA     TTI           WTI
045100021119     C                   MOVEA     TDI           WDI
045200021119     C                   MOVEA     TCA           WCA
045300021120     C                   movea     *blank        Wdd
045400021120     C                   movea     *blank        Wcd
045500031215     C                   MOVEA     TTI7          WTI7
045600031215     C                   MOVEA     TDI7          WDI7
045700031215     C                   MOVEA     TCA7          WCA7
045800031215     C                   movea     *blank        Wdd7
045900031215     C                   movea     *blank        Wcd7
046000021119     C                   MOVEL     '7W'          KCOD
046100021119     C     KTAB2         CHAIN     TABEL00F                           81
046200021023     C     *IN81         IFEQ      '0'
046300021023     C                   MOVEL     TBLUNI        DS7W
046400021023     C                   MOVEA     TTI           WTI
046500021023     C                   MOVEA     TDI           WDI
046600021023     C                   MOVEA     TCA           WCA
046700031215     C                   MOVEA     TTI7          WTI7
046800031215     C                   MOVEA     TDI7          WDI7
046900031215     C                   MOVEA     TCA7          WCA7
047000110530     C                   MOVEA     tmt           vmt
047100110530     C                   MOVEA     *BLANKS       vmtd
047200111028     c                   eval      V4Caut = §7Waut
047300130419     c                   eval      V4Cdes = §7Wdes
047400021023     C*  Decodifico codici disposizioni apertura
047500021023     C                   DO        5             X
047600021023     C     WDI(X)        IFNE      *BLANKS
047700071008     C                   Z-ADD     1             Y                 3 0
047800021023     C     WDI(X)        LOOKUP    C2D(Y)                                 32
047900021023     C   32              MOVEL     D2D(Y)        WDD(X)
048000021023     C                   END
048100021023     C                   END
048200021023      * DECODIFICO CODICI CONSEGNE ANOMALE
048300021023     C                   DO        5             X
048400021023     C     WCA(X)        IFNE      *BLANKS
048500071008     C                   Z-ADD     1             Y
048600021023     C     WCA(X)        LOOKUP    C7O(Y)                                 32
048700021023     C   32              MOVEL     D7O(Y)        WCD(X)
048800021023     C                   END
048900021023     C                   END
049000031215     C*  Decodifico codici disposizioni apertura
049100031215     C                   DO        3             X
049200031215     C     WDI7(X)       IFNE      *BLANKS
049300071008     C                   Z-ADD     1             Y
049400031215     C     WDI7(X)       LOOKUP    C2D(Y)                                 32
049500031215     C   32              MOVEL     D2D(Y)        WDD7(X)
049600031215     C                   END
049700031215     C                   END
049800031215      * DECODIFICO CODICI CONSEGNE ANOMALE
049900031215     C                   DO        3             X
050000031215     C     WCA7(X)       IFNE      *BLANKS
050100071008     C                   Z-ADD     1             Y
050200031215     C     WCA7(X)       LOOKUP    C7O(Y)                                 32
050300031215     C   32              MOVEL     D7O(Y)        WCD7(X)
050400031215     C                   END
050500031215     C                   END
050600111028      * decodifico chi autorizza
050700111028     c                   clear                   tibs02ds
050800111028     c                   clear                   V4Daut
050900111028     c                   eval      T02mod = 'C'
051000111028     c                   eval      T02cod = 'RAC'
051100111028     c                   movel     V4Caut        t02ke1
051200111028     c                   call      'TIBS02R'
051300111028     c                   parm                    kpjba
051400111028     c                   parm                    tibs02ds
051500111028     c                   if        t02err = *blanks
051600111028     c                   movel     t02uni        V4Daut
051700111028     c                   endif
051800021023     C                   ELSE
051900021023     C                   CLEAR                   DS7W
052000141203     c                   movea     *blanks       vmt
052100141203     c                   movea     *blanks       vmtd
052200021023     C                   END
052300940928     C*----------------------------------------------------*
052400940928     C     *IN31         IFEQ      '0'
052500940929     C                   MOVEL     COST(2)       V1DFUN
052600940928     C     TBLFLG        IFNE      ' '
052700940928     C                   SETON                                        92
052800940928     C                   ELSE
052900940928     C                   SETON                                        93
053000940928     C                   END
053100940928     C                   ELSE
053200940929     C                   MOVEL     COST(1)       V1DFUN
053300940928     C                   CLEAR                   DS7Q
053400940928     C                   END
053500940928     C*
053600940929     C     DS7GES        IFEQ      'V'
053700940929     C     DS7OPZ        OREQ      '5'
053800940929     C                   MOVEL     COST(3)       V1DFUN
053900940928     C                   SETON                                        90
054000940928     C                   END
054100940928     C*
054200940928     C                   MOVE      §7QABC        V4CABC
054300940928     C                   MOVE      §7QELI        V4CELI
054400940928     C                   MOVE      §7QINS        V4CINS
054500940928     C                   MOVEA     TIP           VTI
054600940928     C                   MOVEA     DSP           VDI
054700940928     C                   MOVEA     *BLANKS       VDD
054800940928     C                   MOVE      §7Q5RP        V4C5RP
054900940928     C                   MOVE      §7Q5DI        V4C5DI
055000940928     C                   MOVEL     *BLANKS       V4D5DI
055100940929     C                   MOVEL     §7Q1SF        V3C1SF
055200940929     C                   MOVEL     §7Q1SA        V3C1SA
055300940929     C                   MOVEL     §7Q2MS        V3C2MS
055400940929     C                   MOVEL     §7Q2MC        V3C2MC
055500940929     C                   MOVEL     §7Q2MF        V3C2MF
055600940929     C                   MOVEL     §7Q2MQ        V3C2MQ
055700940929     C                   MOVEL     §7Q2EV        V3C2EV
055800940929     C                   MOVEL     §7Q3MS        V3C3MS
055900940929     C                   MOVEL     §7Q3MC        V3C3MC
056000940929     C                   MOVEL     §7Q3MF        V3C3MF
056100940929     C                   MOVEL     §7Q3MQ        V3C3MQ
056200940930     C                   MOVEL     §7Q3EV        V3C3EV
056300940928     C                   MOVEL     *BLANKS       V3D2EV
056400951130     C                   MOVEL     ' '           V4CFTR
056500940929     C     TBLFTT        IFEQ      ' '
056600940929     C     *IN31         ANDEQ     '0'
056700951130     C                   MOVEL     'N'           V4CFTR
056800940929     C                   END
056900940928     C*  Decodifico eventi
057000940928     C                   Z-ADD     1             Y
057100940928     C     V3C2EV        LOOKUP    C2A(Y)                                 32
057200940928     C   32              MOVEL     D2A(Y)        V3D2EV
057300940928     C                   Z-ADD     1             Y
057400940928     C     V3C3EV        LOOKUP    C2A(Y)                                 32
057500940928     C   32              MOVEL     D2A(Y)        V3D3EV
057600940928     C*  Decodifico codici disposizioni riapertura
057700071008     C                   Z-ADD     1             Y
057800940928     C     V4C5DI        LOOKUP    C2D(Y)                                 32
057900940928     C   32              MOVEL     D2D(Y)        V4D5DI
058000940928     C*  Decodifico codici disposizioni apertura
058100940928     C                   DO        5             X
058200940928     C     VDI(X)        IFNE      *BLANKS
058300071008     C                   Z-ADD     1             Y
058400940928     C     VDI(X)        LOOKUP    C2D(Y)                                 32
058500940928     C   32              MOVEL     D2D(Y)        VDD(X)
058600940928     C                   END
058700940928     C                   END
058800110530     C*  Decodifico codici non ammessa apertura giacenza
058900110530     C                   DO        5             X
059000110530     C     Vmt(X)        IFNE      *BLANKS
059100110530     C                   Z-ADD     1             Y
059200110530     C     Vmt(X)        LOOKUP    C2a(Y)                                 32
059300110530     C   32              MOVEL     D2al(Y)       Vmtd(X)
059400110530     C                   END
059500110530     C                   END
059600940928     C*  Accendo indicatori x posizionamento/REVERSE IMMAGE
059700940928     C*  su gruppo parametri da modificare
059800940928     C     §7Q5RP        IFNE      *ZEROS
059900940928     C     §7Q5DI        ORNE      *BLANKS
060000940928     C                   SETON                                        05
060100940928     C                   END
060200940929     C                   MOVEA     TIP           TIPOL             5
060300940929     C                   MOVEA     DSP           DISPO             5
060400940929     C     TIPOL         IFNE      *BLANKS
060500940929     C     DISPO         ORNE      *BLANKS
060600940928     C                   SETON                                        04
060700940928     C                   END
060800940928     C     §7Q3MS        IFNE      *BLANKS
060900940928     C                   SETON                                        03
061000940928     C                   END
061100940928     C     §7Q2MS        IFNE      *BLANKS
061200940928     C                   SETON                                        02
061300940928     C                   END
061400940928     C     §7Q1SF        IFNE      ' '
061500940928     C     §7Q1SA        ORNE      ' '
061600940928     C                   SETON                                        01
061700940928     C                   END
061800940928     C*
061900940929     C     FINCAR        ENDSR
062000940928     C*----------------------------------------------------*
062100940928     C*  CTR01 : Gestione inizial. prima   videata         *
062200940928     C*----------------------------------------------------*
062300940928     C     CTR01         BEGSR
062400940928     C*
062500940929     C*  Controllo dati messaggio in partenza
062600940929     C     V3C2MS        IFEQ      *BLANKS
062700940929     C     V3C2MC        IFNE      *BLANKS
062800940929     C     V3C2MF        ORNE      *BLANKS
062900940929     C     V3C2MQ        ORNE      *BLANKS
063000940929     C     V3C2EV        ORNE      *BLANKS
063100940929     C                   SETON                                        2842
063200960110     C                   MOVEL     ERR(7)        $MSG
063300940929     C                   GOTO      FINCT1
063400940929     C                   END
063500940929     C                   END
063600940929     C*
063700940929     C     V3C2MC        IFEQ      *BLANKS
063800940929     C     V3C2MS        IFNE      *BLANKS
063900940929     C     V3C2MF        ORNE      *BLANKS
064000940929     C     V3C2MQ        ORNE      *BLANKS
064100940929     C     V3C2EV        ORNE      *BLANKS
064200940929     C                   SETON                                        2843
064300960110     C                   MOVEL     ERR(8)        $MSG
064400940929     C                   GOTO      FINCT1
064500940929     C                   END
064600940929     C                   END
064700940929     C*
064800940929     C     V3C2MF        IFEQ      *BLANKS
064900940929     C     V3C2MS        IFNE      *BLANKS
065000940929     C     V3C2MC        ORNE      *BLANKS
065100940929     C     V3C2MQ        ORNE      *BLANKS
065200940929     C     V3C2EV        ORNE      *BLANKS
065300940929     C                   SETON                                        2844
065400960110     C                   MOVEL     ERR(8)        $MSG
065500940929     C                   GOTO      FINCT1
065600940929     C                   END
065700940929     C                   END
065800940929     C*
065900940929     C     V3C2MQ        IFEQ      *BLANKS
066000940929     C     V3C2MS        IFNE      *BLANKS
066100940929     C     V3C2MF        ORNE      *BLANKS
066200940929     C     V3C2MC        ORNE      *BLANKS
066300940929     C     V3C2EV        ORNE      *BLANKS
066400940929     C                   SETON                                        2845
066500960110     C                   MOVEL     ERR(8)        $MSG
066600940929     C                   GOTO      FINCT1
066700940929     C                   END
066800940929     C                   END
066900940929     C*
067000940929     C     V3C2EV        IFNE      *BLANKS
067100090402     C                   Z-ADD     1             XX
067200090402     C     V3C2EV        LOOKUP    C2A(XX)                                32
067300940928     C     *IN32         IFEQ      '0'
067400940928     C                   SETON                                        2840
067500940929     C                   MOVEL     ERR(1)        $MSG
067600940928     C                   GOTO      FINCT1
067700940928     C                   ELSE
067800090402     C                   MOVEL     D2A(XX)       V3D2EV
067900940928     C                   END
068000940929     C                   ELSE
068100940929     C                   MOVEL     *BLANKS       V3D2EV
068200940929     C                   END
068300940928     C*
068400940929     C*  Controllo dati messaggio in arrivo
068500940929     C     V3C3MS        IFEQ      *BLANKS
068600940929     C     V3C3MC        IFNE      *BLANKS
068700940929     C     V3C3MF        ORNE      *BLANKS
068800940929     C     V3C3MQ        ORNE      *BLANKS
068900940929     C     V3C3EV        ORNE      *BLANKS
069000940929     C                   SETON                                        2846
069100960110     C                   MOVEL     ERR(7)        $MSG
069200940929     C                   GOTO      FINCT1
069300940929     C                   END
069400940929     C                   END
069500940929     C*
069600940929     C     V3C3MC        IFEQ      *BLANKS
069700940929     C     V3C3MS        IFNE      *BLANKS
069800940929     C     V3C3MF        ORNE      *BLANKS
069900940929     C     V3C3MQ        ORNE      *BLANKS
070000940929     C     V3C3EV        ORNE      *BLANKS
070100940929     C                   SETON                                        2847
070200960110     C                   MOVEL     ERR(8)        $MSG
070300940929     C                   GOTO      FINCT1
070400940929     C                   END
070500940929     C                   END
070600940929     C*
070700940929     C     V3C3MF        IFEQ      *BLANKS
070800940929     C     V3C3MS        IFNE      *BLANKS
070900940929     C     V3C3MC        ORNE      *BLANKS
071000940929     C     V3C3MQ        ORNE      *BLANKS
071100940929     C     V3C3EV        ORNE      *BLANKS
071200940929     C                   SETON                                        2848
071300960110     C                   MOVEL     ERR(8)        $MSG
071400940929     C                   GOTO      FINCT1
071500940929     C                   END
071600940929     C                   END
071700940929     C*
071800940929     C     V3C3MQ        IFEQ      *BLANKS
071900940929     C     V3C3MS        IFNE      *BLANKS
072000940929     C     V3C3MF        ORNE      *BLANKS
072100940929     C     V3C3MC        ORNE      *BLANKS
072200940929     C     V3C3EV        ORNE      *BLANKS
072300940929     C                   SETON                                        2849
072400960110     C                   MOVEL     ERR(8)        $MSG
072500940929     C                   GOTO      FINCT1
072600940929     C                   END
072700940929     C                   END
072800940929     C*
072900940929     C     V3C3EV        IFNE      *BLANKS
073000090402     C                   Z-ADD     1             XX
073100090402     C     V3C3EV        LOOKUP    C2A(XX)                                32
073200940928     C     *IN32         IFEQ      '0'
073300940928     C                   SETON                                        2841
073400940929     C                   MOVEL     ERR(1)        $MSG
073500940928     C                   ELSE
073600090402     C                   MOVEL     D2A(XX)       V3D3EV
073700940928     C                   END
073800940929     C                   ELSE
073900940929     C                   MOVEL     *BLANKS       V3D3EV
074000940929     C                   END
074100940928     C*
074200940928     C     FINCT1        ENDSR
074300940928     C*----------------------------------------------------*
074400940928     C*  CTR02 : Controlli seconda videata                 *
074500940928     C*----------------------------------------------------*
074600940928     C     CTR02         BEGSR
074700940930     C*  Se ho un codice con tipologia 'T' non ne posso imm.altri
074800940930     C                   Z-ADD     1             X
074900940930     C                   Z-ADD     0             WX                2 0
075000940930     C     'T'           LOOKUP    VTI(X)                                 32
075100940930     C   32              Z-ADD     X             WX
075200940928     C*  Controllo Tipologie/codici disposizioni apertura
075300940928     C                   DO        5             X
075400940930     C     VTI(X)        IFNE      *BLANKS
075500940928     C     VTI(X)        ANDNE     'T'
075600940928     C     VTI(X)        LOOKUP    T2Z                                    32
075700940928     C     *IN32         IFEQ      '0'
075800940930     C                   SETON                                        28
075900940928     C                   MOVEL     IN1(X)        C                 2 0
076000940928     C                   MOVEL     '1'           *IN(C)
076100940928     C                   MOVEL     ERR(2)        $MSG
076200940928     C                   GOTO      FINCT2
076300940928     C                   END
076400940930     C     VTI(X)        IFNE      *BLANKS
076500940930     C     WX            ANDNE     X
076600940930     C     WX            ANDNE     0
076700940930     C                   SETON                                        28
076800940930     C                   MOVEL     IN1(X)        C                 2 0
076900940930     C                   MOVEL     '1'           *IN(C)
077000960110     C                   MOVEL     ERR(9)        $MSG
077100940930     C                   GOTO      FINCT2
077200940930     C                   END
077300940930     C*  Controllo se tipologia doppia
077400940930     C                   Z-ADD     1             Y
077500940930     C     VTI(X)        LOOKUP    VTI(Y)                                 97
077600940930     C     X             IFNE      Y
077700940930     C                   SETON                                        28
077800940930     C                   MOVEL     IN1(X)        C                 2 0
077900940930     C                   MOVEL     '1'           *IN(C)
078000960110     C                   MOVEL     ERR(9)        $MSG
078100940930     C                   GOTO      FINCT2
078200940930     C                   END
078300940929     C                   END
078400940929     C*
078500940930     C                   MOVEL     *BLANKS       VDD(X)
078600940929     C     VDI(X)        IFNE      *BLANKS
078700940930     C                   Z-ADD     1             Y
078800940930     C     VDI(X)        LOOKUP    C2D(Y)                                 32
078900940929     C     *IN32         IFEQ      '0'
079000940929     C                   SETON                                        28
079100940929     C                   MOVEL     IN2(X)        C                 2 0
079200940929     C                   MOVEL     '1'           *IN(C)
079300940929     C                   MOVEL     ERR(3)        $MSG
079400940929     C                   GOTO      FINCT2
079500940929     C                   ELSE
079600940930     C                   MOVEL     D2D(Y)        VDD(X)
079700940929     C                   END
079800940929     C                   END
079900940928     C*
080000940929     C     VTI(X)        IFEQ      *BLANKS
080100940929     C     VDI(X)        ANDNE     *BLANKS
080200940929     C                   SETON                                        28
080300940929     C                   MOVEL     IN1(X)        C                 2 0
080400940929     C                   MOVEL     '1'           *IN(C)
080500940929     C                   MOVEL     ERR(4)        $MSG
080600940929     C                   GOTO      FINCT2
080700940929     C                   END
080800940929     C*
080900940929     C     VTI(X)        IFNE      *BLANKS
081000940929     C     VDI(X)        ANDEQ     *BLANKS
081100940929     C                   SETON                                        28
081200940929     C                   MOVEL     IN2(X)        C                 2 0
081300940929     C                   MOVEL     '1'           *IN(C)
081400940929     C                   MOVEL     ERR(5)        $MSG
081500940929     C                   GOTO      FINCT2
081600940929     C                   END
081700940929     C*
081800940928     C                   END
081900940928     C*
082000940929     C*  Controllo numero aperture giacenze/disposiz.
082100940929     C     V4C5RP        IFEQ      *ZEROS
082200940929     C     V4C5DI        ANDNE     *BLANKS
082300940929     C                   SETON                                        2850
082400940929     C                   MOVEL     ERR(6)        $MSG
082500940929     C                   END
082600940929     C*
082700940929     C     V4C5RP        IFNE      *ZEROS
082800940929     C     V4C5DI        ANDEQ     *BLANKS
082900940929     C                   SETON                                        2851
083000940929     C                   MOVEL     ERR(5)        $MSG
083100940929     C                   END
083200940929     C*
083300940930     C                   MOVEL     *BLANKS       V4D5DI
083400940929     C     V4C5DI        IFNE      *BLANKS
083500940930     C                   Z-ADD     1             X
083600940930     C     V4C5DI        LOOKUP    C2D(X)                                 32
083700940929     C     *IN32         IFEQ      '0'
083800940929     C                   SETON                                        2851
083900940929     C                   MOVEL     ERR(3)        $MSG
084000940929     C                   ELSE
084100940929     C                   MOVEL     D2D(X)        V4D5DI
084200940929     C                   END
084300940929     C                   END
084400111028      * Chi autorizza
084500111028     c                   IF        V4Caut = *blanks
084600111028     c                   eval      *in70 = *on
084700111028     c                   eval      *in28 = *on
084800111028     c                   eval      $msg = err(11)
084900111028     c                   leavesr
085000111028     c                   ENDIF
085100111028     c                   IF        V4Caut = '?'
085200111028     c                   clear                   tibs02ds
085300111028     c                   eval      T02mod = 'R'
085400111028     c                   eval      T02cod = 'RAC'
085500111028     c                   call      'TIBS02R'
085600111028     c                   parm                    kpjba
085700111028     c                   parm                    tibs02ds
085800111028     c                   IF        T02err = *blanks
085900111028     c                   eval      V4Caut = T02ke1
086000111028     c                   eval      V4Daut = T02uni
086100111028     c                   ENDIF
086200111028     c                   ENDIF
086300111028     c                   IF        V4Caut <> *blanks
086400111028     c                   clear                   tibs02ds
086500111028     c                   eval      T02mod = 'C'
086600111028     c                   eval      T02cod = 'RAC'
086700111028     c                   eval      T02ke1 = V4Caut
086800111028     c                   call      'TIBS02R'
086900111028     c                   parm                    kpjba
087000111028     c                   parm                    tibs02ds
087100111028     c                   IF        T02err <> *blanks
087200111028     c                   eval      *in70 = *on
087300111028     c                   eval      *in28 = *on
087400111028     c                   eval      $msg = err(11)
087500111028     c                   leavesr
087600111028     c                   ENDIF
087700111028     c                   eval      V4Daut = T02uni
087800111028     c                   ENDIF
087900940929     C*
088000940928     C     FINCT2        ENDSR
088100021023     C*----------------------------------------------------*
088200021023     C*  CTR03 : Controlli TERZA  videata                  *
088300021023     C*----------------------------------------------------*
088400021023     C     CTR03         BEGSR
088500021023     C*  Se ho un codice con tipologia 'T' non ne posso imm.altri
088600021023     C                   Z-ADD     1             X
088700021023     C                   Z-ADD     0             WX                2 0
088800021023     C     'T'           LOOKUP    WTI(X)                                 32
088900021023     C   32              Z-ADD     X             WX
089000021023     C*  Controllo Tipologie/codici disposizioni apertura
089100031215     C                   DO        5             X
089200031215     C     WTI(X)        IFNE      *BLANKS
089300021023     C     WTI(X)        ANDNE     'T'
089400021023     C     WTI(X)        LOOKUP    T2Z                                    32
089500031215     C     *IN32         IFEQ      '0'
089600021023     C                   SETON                                        28
089700021023     C                   MOVEL     IN1(X)        C                 2 0
089800021023     C                   MOVEL     '1'           *IN(C)
089900021023     C                   MOVEL     ERR(2)        $MSG
090000021023     C                   GOTO      FINCT3
090100031215     C                   END
090200031215     C     WTI(X)        IFNE      *BLANKS
090300021023     C     WX            ANDNE     X
090400021023     C     WX            ANDNE     0
090500021023     C                   SETON                                        28
090600021023     C                   MOVEL     IN1(X)        C                 2 0
090700021023     C                   MOVEL     '1'           *IN(C)
090800021023     C                   MOVEL     ERR(9)        $MSG
090900021023     C                   GOTO      FINCT3
091000031215     C                   END
091100021023     C*  Controllo se tipologia doppia
091200021023     C                   Z-ADD     1             Y
091300021023     C     WTI(X)        LOOKUP    WTI(Y)                                 97
091400031215     C     X             IFNE      Y
091500021023     C                   SETON                                        28
091600021023     C                   MOVEL     IN1(X)        C                 2 0
091700021023     C                   MOVEL     '1'           *IN(C)
091800021023     C                   MOVEL     ERR(9)        $MSG
091900021023     C                   GOTO      FINCT3
092000031215     C                   END
092100031215     C                   END
092200021023     C*
092300021023     C                   MOVEL     *BLANKS       WDD(X)
092400031215     C     WDI(X)        IFNE      *BLANKS
092500021023     C                   Z-ADD     1             Y
092600021023     C     WDI(X)        LOOKUP    C2D(Y)                                 32
092700031215     C     *IN32         IFEQ      '0'
092800021023     C                   SETON                                        28
092900021023     C                   MOVEL     IN2(X)        C                 2 0
093000021023     C                   MOVEL     '1'           *IN(C)
093100021023     C                   MOVEL     ERR(3)        $MSG
093200021023     C                   GOTO      FINCT3
093300021023     C                   ELSE
093400021023     C                   MOVEL     D2D(Y)        WDD(X)
093500031215     C                   END
093600031215     C                   END
093700021023     C*
093800031215     C     WTI(X)        IFEQ      *BLANKS
093900021023     C     WDI(X)        ANDNE     *BLANKS
094000021023     C                   SETON                                        28
094100021024     C                   MOVEL     IN2(X)        C                 2 0
094200021023     C                   MOVEL     '1'           *IN(C)
094300021023     C                   MOVEL     ERR(4)        $MSG
094400021023     C                   GOTO      FINCT3
094500031215     C                   END
094600021023     C*
094700031215     C     WTI(X)        IFNE      *BLANKS
094800021023     C     WDI(X)        ANDEQ     *BLANKS
094900021023     C                   SETON                                        28
095000021023     C                   MOVEL     IN2(X)        C                 2 0
095100021023     C                   MOVEL     '1'           *IN(C)
095200021023     C                   MOVEL     ERR(5)        $MSG
095300021023     C                   GOTO      FINCT3
095400031215     C                   END
095500021023     C*
095600021023     C                   MOVEL     *BLANKS       WCD(X)
095700031215     C     WCA(X)        IFNE      *BLANKS
095800021023     C                   Z-ADD     1             Y
095900021023     C     WCA(X)        LOOKUP    C7O(Y)                                 32
096000031215     C     *IN32         IFEQ      '0'
096100021023     C                   SETON                                        28
096200021024     C                   MOVEL     IN3(X)        C                 2 0
096300021023     C                   MOVEL     '1'           *IN(C)
096400021023     C                   MOVEL     ERR(10)       $MSG
096500021023     C                   GOTO      FINCT3
096600021023     C                   ELSE
096700021023     C                   MOVEL     D7O(Y)        WCD(X)
096800031215     C                   END
096900031215     C                   END
097000021023     C*
097100031215     C                   END
097200031215      * TIPOLOGIA 7
097300031215     C*  Se ho un codice con tipologia 'T' non ne posso imm.altri
097400031215     C                   Z-ADD     1             X
097500031215     C                   Z-ADD     0             WX                2 0
097600031215     C     'T'           LOOKUP    WTI7(X)                                32
097700031215     C   32              Z-ADD     X             WX
097800031215     C*  Controllo Tipologie/codici disposizioni apertura
097900031215     C                   DO        3             X
098000031215     C     WTI7(X)       IFNE      *BLANKS
098100031215     C     WTI7(X)       ANDNE     'T'
098200031215     C     WTI7(X)       LOOKUP    T2Z                                    32
098300031215     C     *IN32         IFEQ      '0'
098400031215     C                   SETON                                        28
098500031215     C                   MOVEL     IN4(X)        C                 2 0
098600031215     C                   MOVEL     '1'           *IN(C)
098700031215     C                   MOVEL     ERR(2)        $MSG
098800031215     C                   GOTO      FINCT3
098900031215     C                   END
099000031215     C     WTI7(X)       IFNE      *BLANKS
099100031215     C     WX            ANDNE     X
099200031215     C     WX            ANDNE     0
099300031215     C                   SETON                                        28
099400031215     C                   MOVEL     IN4(X)        C                 2 0
099500031215     C                   MOVEL     '1'           *IN(C)
099600031215     C                   MOVEL     ERR(9)        $MSG
099700031215     C                   GOTO      FINCT3
099800031215     C                   END
099900031215     C*  Controllo se tipologia doppia
100000031215     C                   Z-ADD     1             Y
100100031215     C     WTI7(X)       LOOKUP    WTI7(Y)                                97
100200031215     C     X             IFNE      Y
100300031215     C                   SETON                                        28
100400031215     C                   MOVEL     IN4(X)        C                 2 0
100500031215     C                   MOVEL     '1'           *IN(C)
100600031215     C                   MOVEL     ERR(9)        $MSG
100700031215     C                   GOTO      FINCT3
100800031215     C                   END
100900031215     C                   END
101000031215     C*
101100031215     C                   MOVEL     *BLANKS       WDD7(X)
101200031215     C     WDI7(X)       IFNE      *BLANKS
101300031215     C                   Z-ADD     1             Y
101400031215     C     WDI7(X)       LOOKUP    C2D(Y)                                 32
101500031215     C     *IN32         IFEQ      '0'
101600031215     C                   SETON                                        28
101700031215     C                   MOVEL     IN5(X)        C                 2 0
101800031215     C                   MOVEL     '1'           *IN(C)
101900031215     C                   MOVEL     ERR(3)        $MSG
102000031215     C                   GOTO      FINCT3
102100031215     C                   ELSE
102200031215     C                   MOVEL     D2D(Y)        WDD7(X)
102300031215     C                   END
102400031215     C                   END
102500031215     C*
102600031215     C     WTI7(X)       IFEQ      *BLANKS
102700031215     C     WDI7(X)       ANDNE     *BLANKS
102800031215     C                   SETON                                        28
102900031215     C                   MOVEL     IN5(X)        C                 2 0
103000031215     C                   MOVEL     '1'           *IN(C)
103100031215     C                   MOVEL     ERR(4)        $MSG
103200031215     C                   GOTO      FINCT3
103300031215     C                   END
103400031215     C*
103500031215     C     WTI7(X)       IFNE      *BLANKS
103600031215     C     WDI7(X)       ANDEQ     *BLANKS
103700031215     C                   SETON                                        28
103800031215     C                   MOVEL     IN5(X)        C                 2 0
103900031215     C                   MOVEL     '1'           *IN(C)
104000031215     C                   MOVEL     ERR(5)        $MSG
104100031215     C                   GOTO      FINCT3
104200031215     C                   END
104300031215     C*
104400031215     C                   MOVEL     *BLANKS       WCD7(X)
104500031215     C     WCA7(X)       IFNE      *BLANKS
104600031215     C                   Z-ADD     1             Y
104700031215     C     WCA7(X)       LOOKUP    C7O(Y)                                 32
104800031215     C     *IN32         IFEQ      '0'
104900031215     C                   SETON                                        28
105000031215     C                   MOVEL     IN6(X)        C                 2 0
105100031215     C                   MOVEL     '1'           *IN(C)
105200031215     C                   MOVEL     ERR(10)       $MSG
105300031215     C                   GOTO      FINCT3
105400031215     C                   ELSE
105500031215     C                   MOVEL     D7O(Y)        WCD7(X)
105600031215     C                   END
105700031215     C                   END
105800031215     C*
105900031215     C                   END
106000021023     C*
106100021023     C     FINCT3        ENDSR
106200110530     C*----------------------------------------------------*
106300110530     C*  CTR04 : Gestione inizial. prima   videata         *
106400110530     C*----------------------------------------------------*
106500110530     C     CTR04         BEGSR
106600110530     C*
106700110531     C     Vmt(1)        IFNE      *BLANKS
106800110531     c                   z-add     1             xx
106900110531     C     vmt(1)        LOOKUP    C2A(xx)                                32
107000110531     C     *IN32         IFEQ      '0'
107100110531     C                   SETON                                        2841
107200110531     C                   MOVEL     ERR(1)        $MSG
107300110531     C                   ELSE
107400110531     C                   MOVEL     D2Al(XX)      vmtd(1)
107500110531     C                   END
107600110531     C                   ELSE
107700110531     C                   MOVEL     *BLANKS       vmtd(1)
107800110531     C                   END
107900110531     C     Vmt(2)        IFNE      *BLANKS
108000110531     c                   z-add     1             xx
108100110531     C     vmt(2)        LOOKUP    C2A(xx)                                32
108200110531     C     *IN32         IFEQ      '0'
108300110531     C                   SETON                                        2842
108400110531     C                   MOVEL     ERR(1)        $MSG
108500110531     C                   ELSE
108600110531     C                   MOVEL     D2Al(XX)      vmtd(2)
108700110531     C                   END
108800110531     C                   ELSE
108900110531     C                   MOVEL     *BLANKS       vmtd(2)
109000110531     C                   END
109100110531     C     Vmt(3)        IFNE      *BLANKS
109200110531     c                   z-add     1             xx
109300110531     C     vmt(3)        LOOKUP    C2A(xx)                                32
109400110531     C     *IN32         IFEQ      '0'
109500110531     C                   SETON                                        2843
109600110531     C                   MOVEL     ERR(1)        $MSG
109700110531     C                   ELSE
109800110531     C                   MOVEL     D2Al(XX)      vmtd(3)
109900110531     C                   END
110000110531     C                   ELSE
110100110531     C                   MOVEL     *BLANKS       vmtd(3)
110200110531     C                   END
110300110531     C     Vmt(4)        IFNE      *BLANKS
110400110531     c                   z-add     1             xx
110500110531     C     vmt(4)        LOOKUP    C2A(xx)                                32
110600110531     C     *IN32         IFEQ      '0'
110700110531     C                   SETON                                        2844
110800110531     C                   MOVEL     ERR(1)        $MSG
110900110531     C                   ELSE
111000110531     C                   MOVEL     D2Al(XX)      vmtd(4)
111100110531     C                   END
111200110531     C                   ELSE
111300110531     C                   MOVEL     *BLANKS       vmtd(4)
111400110531     C                   END
111500110531     C     Vmt(5)        IFNE      *BLANKS
111600110531     c                   z-add     1             xx
111700110531     C     vmt(5)        LOOKUP    C2A(xx)                                32
111800110531     C     *IN32         IFEQ      '0'
111900110531     C                   SETON                                        2845
112000110531     C                   MOVEL     ERR(1)        $MSG
112100110531     C                   ELSE
112200110531     C                   MOVEL     D2Al(XX)      vmtd(5)
112300110531     C                   END
112400110531     C                   ELSE
112500110531     C                   MOVEL     *BLANKS       vmtd(5)
112600110531     C                   END
112700110530     C                   ENDsr
112800940928     C*----------------------------------------------------*
112900940928     C*  AGGIOR: Aggiornamento paramteri                   *
113000940928     C*----------------------------------------------------*
113100931209     C     AGGIOR        BEGSR
113200940701     C*------------------------------------------------------*
113300940725     C*  Procedo all'aggiornamento della tabella
113400940929     C                   MOVEL     *BLANKS       KKEY
113500940929     C                   MOVE      '7Q'          KCOD
113600940929     C                   MOVEL     V3CCOD        KKEY
113700940929     C     KTAB2         CHAIN     TABEL00F                           31
113800940929     C                   MOVE      V4CABC        §7QABC
113900940929     C                   MOVE      V4CELI        §7QELI
114000940929     C                   MOVE      V4CINS        §7QINS
114100940929     C                   MOVEA     VTI           TIP
114200940929     C                   MOVEA     VDI           DSP
114300940929     C                   MOVE      V4C5RP        §7Q5RP
114400940929     C                   MOVE      V4C5DI        §7Q5DI
114500940929     C                   MOVEL     V3C1SF        §7Q1SF
114600940929     C                   MOVEL     V3C1SA        §7Q1SA
114700940929     C                   MOVEL     V3C2MS        §7Q2MS
114800940929     C                   MOVEL     V3C2MC        §7Q2MC
114900940929     C                   MOVEL     V3C2MF        §7Q2MF
115000940929     C                   MOVEL     V3C2MQ        §7Q2MQ
115100940929     C                   MOVEL     V3C2EV        §7Q2EV
115200940929     C                   MOVEL     V3C3MS        §7Q3MS
115300940929     C                   MOVEL     V3C3MC        §7Q3MC
115400940929     C                   MOVEL     V3C3MF        §7Q3MF
115500940929     C                   MOVEL     V3C3MQ        §7Q3MQ
115600940929     C                   MOVEL     V3C3EV        §7Q3EV
115700940929     C                   MOVEL     *BLANKS       TBLUNI
115800940929     C                   MOVEL     DS7Q          TBLUNI
115900940929     C     V4CFTR        IFEQ      ' '
116000940929     C                   MOVEL     '1'           TBLFTT
116100960110     C                   MOVEL     ' '           DS7TRA
116200940929     C                   ELSE
116300940929     C                   MOVEL     ' '           TBLFTT
116400960110     C                   MOVEL     'N'           DS7TRA
116500940929     C                   END
116600951130     C                   MOVEL     *BLANKS       TBLFTR
116700951130     C                   MOVEL     *ZEROS        TBLDTR
116800940929     C     *IN31         IFEQ      '1'
116900940418     C                   Z-ADD     1             TBLKUT
117000940929     C                   MOVEL     '7Q'          TBLCOD
117100940725     C                   MOVEL     *BLANKS       TBLKEY
117200940929     C                   MOVEL     V3CCOD        TBLKEY
117300931209     C                   WRITE     TABEL
117400940725     C                   ELSE
117500940725     C                   UPDATE    TABEL
117600940418     C                   END
117700021023      *
117800021023     C                   ENDSR
117900021023     C*----------------------------------------------------*
118000021023     C     AGGIO2        BEGSR
118100021023     C*------------------------------------------------------*
118200021023      * 7W CORRELATA
118300021023     C                   MOVE      '7W'          KCOD
118400021023     C                   MOVEL     V3CCOD        TBLKEY
118500021023     C                   MOVEA     WTI           TTI
118600021023     C                   MOVEA     WDI           TDI
118700021023     C                   MOVEA     WCA           TCA
118800031215     C                   MOVEA     WTI7          TTI7
118900031215     C                   MOVEA     WDI7          TDI7
119000031215     C                   MOVEA     WCA7          TCA7
119100110530     C                   MOVEA     Vmt           tmt
119200111028     c                   eval      §7Waut = V4Caut
119300130419     c                   eval      §7Wdes = V4Cdes
119400021023     C     KTAB2         CHAIN     TABEL00F                           81
119500021023     C     *IN81         IFEQ      '1'
119600021023     C                   Z-ADD     1             TBLKUT
119700021023     C                   MOVEL     '7W'          TBLCOD
119800021023     C                   MOVEL     *BLANKS       TBLKEY
119900021023     C                   MOVEL     V3CCOD        TBLKEY
120000031215     C                   MOVEL(p)  DS7W          TBLUNI
120100021023     C                   WRITE     TABEL
120200021023     C                   ELSE
120300031215     C                   MOVEL(p)  DS7W          TBLUNI
120400141128     C     V4CFTR        IFEQ      ' '
120500141128     C                   MOVEL     '1'           TBLFTT
120600141128     C                   ELSE
120700141128     C                   MOVEL     ' '           TBLFTT
120800141128     C                   END
120900141128     C                   MOVEL     *BLANKS       TBLFTR
121000141128     C                   MOVEL     *ZEROS        TBLDTR
121100021023     C                   UPDATE    TABEL
121200021023     C                   END
121300021023      *
121400931209     C                   ENDSR
121500931209     C*--------------------------------------------------------------*
121600931209     C*  ANNUL: Annullamento                                         *
121700931209     C*--------------------------------------------------------------*
121800931209     C     ANNUL         BEGSR
121900931209     C*----------------------------------------------------*
122000940929     C                   MOVEL     *BLANKS       KKEY
122100940929     C                   MOVE      '7Q'          KCOD
122200940929     C                   MOVEL     V3CCOD        KKEY
122300940929     C     KTAB2         CHAIN     TABEL00F                           80
122400940725     C     *IN80         IFEQ      '0'
122500960111     C                   MOVEL     'A'           DS7RIP
122600940418     C                   MOVEL     '*'           TBLFLG
122700141128     C     V4CFTR        IFEQ      ' '
122800141128     C                   MOVEL     '1'           TBLFTT
122900141128     C                   MOVEL     ' '           DS7TRA
123000141128     C                   ELSE
123100141128     C                   MOVEL     ' '           TBLFTT
123200141128     C                   MOVEL     'N'           DS7TRA
123300141128     C                   END
123400141128     C                   MOVEL     *BLANKS       TBLFTR
123500141128     C                   MOVEL     *ZEROS        TBLDTR
123600141128     C*******            EXCEPT    ANNTBL
123700141128     C                   UPDATE    TABEL
123800021023     C                   END
123900021023     C*
124000021023     C                   ENDSR
124100021023     C*--------------------------------------------------------------*
124200021023     C     ANNU2         BEGSR
124300021023     C*----------------------------------------------------*
124400021023     C                   MOVE      '7W'          KCOD
124500021023     C                   MOVEL     V3CCOD        KKEY
124600021023     C     KTAB2         CHAIN     TABEL00F                           80
124700021023     C     *IN80         IFEQ      '0'
124800021023     C                   MOVEL     '*'           TBLFLG
124900141128     C     V4CFTR        IFEQ      ' '
125000141128     C                   MOVEL     '1'           TBLFTT
125100141128     C                   ELSE
125200141128     C                   MOVEL     ' '           TBLFTT
125300141128     C                   END
125400141128     C                   MOVEL     *BLANKS       TBLFTR
125500141128     C                   MOVEL     *ZEROS        TBLDTR
125600141128     C******             EXCEPT    ANNTBL
125700141128     C                   UPDATE    TABEL
125800021023     C                   END
125900021023     C*
126000931209     C                   ENDSR
126100931209     C*--------------------------------------------------------------*
126200931209     C*  RIPRI: Ripristino                                           *
126300931209     C*--------------------------------------------------------------*
126400931209     C     RIPRIS        BEGSR
126500931209     C*----------------------------------------------------*
126600940929     C                   MOVEL     *BLANKS       KKEY
126700940929     C                   MOVE      '7Q'          KCOD
126800940929     C                   MOVEL     V3CCOD        KKEY
126900940929     C     KTAB2         CHAIN     TABEL00F                           80
127000940725     C     *IN80         IFEQ      '0'
127100960111     C                   MOVEL     'R'           DS7RIP
127200940418     C                   MOVEL     ' '           TBLFLG
127300141128     C     V4CFTR        IFEQ      ' '
127400141128     C                   MOVEL     '1'           TBLFTT
127500141128     C                   MOVEL     ' '           DS7TRA
127600141128     C                   ELSE
127700141128     C                   MOVEL     ' '           TBLFTT
127800141128     C                   MOVEL     'N'           DS7TRA
127900141128     C                   END
128000141128     C                   MOVEL     *BLANKS       TBLFTR
128100141128     C                   MOVEL     *ZEROS        TBLDTR
128200141128     C******             EXCEPT    ANNTBL
128300141128     C                   UPDATE    TABEL
128400021023     C                   END
128500021023     C*
128600021023     C                   ENDSR
128700021023     C*--------------------------------------------------------------*
128800021023     C     RIPRI2        BEGSR
128900021023     C*----------------------------------------------------*
129000021023     C                   MOVE      '7W'          KCOD
129100021023     C                   MOVEL     V3CCOD        KKEY
129200021023     C     KTAB2         CHAIN     TABEL00F                           80
129300021023     C     *IN80         IFEQ      '0'
129400021023     C                   MOVEL     ' '           TBLFLG
129500141128     C     V4CFTR        IFEQ      ' '
129600141128     C                   MOVEL     '1'           TBLFTT
129700141128     C                   ELSE
129800141128     C                   MOVEL     ' '           TBLFTT
129900141128     C                   END
130000141128     C                   MOVEL     *BLANKS       TBLFTR
130100141128     C                   MOVEL     *ZEROS        TBLDTR
130200141128     C******             EXCEPT    ANNTBL
130300141128     C                   UPDATE    TABEL
130400021023     C                   END
130500021023     C*
130600931209     C                   ENDSR
130700931209     C*--------------------------------------------------------------*
130800940928     C*  DEFCAM: Operazioni iniziali                                 *
130900931209     C*--------------------------------------------------------------*
131000940928     C     DEFCAM        BEGSR
131100931209     C*
131200931209     C*  Richiamo XPARUT
131300931209     C                   Z-ADD     1             CODUT
131400931209     C                   CALL      'XPARUT'
131500931209     C                   PARM                    UTEDSE
131600931209     C     CODUT         CABEQ     -1            FINE
131700940927     C                   MOVEL     RAGUT         V1CRSU
131800931209     C*----------------------------------------------------*
131900931209     C* Definizione chiavi                                 *
132000940927     C     KTAB1         KLIST
132100940927     C                   KFLD                    KKUT
132200940927     C                   KFLD                    KCOD
132300940927     C*
132400940927     C     KTAB2         KLIST
132500940927     C                   KFLD                    KKUT
132600940927     C                   KFLD                    KCOD
132700940927     C                   KFLD                    KKEY
132800931209     C*----------------------------------------------------*
132900931209     C* Definizione variabili                              *
133000940927     C     *LIKE         DEFINE    TBLKUT        KKUT
133100940927     C     *LIKE         DEFINE    TBLCOD        KCOD
133200940927     C     *LIKE         DEFINE    TBLKEY        KKEY
133300931209     C*----------------------------------------------------*
133400940927     C*  Caricamento tabella 2D
133500940927     C                   Z-ADD     1             KKUT
133600940927     C                   MOVE      '2D'          KCOD
133700940725     C                   Z-ADD     0             X                 2 0
133800940927     C     KTAB1         CHAIN     TABEL00F                           31
133900940927     C     *IN31         DOWEQ     '0'
134000940725     C     X             ANDLT     50
134100940725     C                   ADD       1             X
134200940930     C                   MOVEL     TBLKEY        C2D(X)
134300940930     C                   MOVEL     TBLUNI        D2D(X)
134400940929     C     KTAB1         READE     TABEL00F                               31
134500940725     C                   END
134600021023     C*  Caricamento tabella 7O
134700021023     C                   Z-ADD     1             KKUT
134800021023     C                   MOVE      '7O'          KCOD
134900021023     C                   Z-ADD     0             X                 2 0
135000021023     C     KTAB1         CHAIN     TABEL00F                           31
135100021023     C     *IN31         DOWEQ     '0'
135200021023     C     X             ANDLT     50
135300021023     C                   ADD       1             X
135400021023     C                   MOVEL     TBLKEY        C7O(X)
135500021023     C                   MOVEL     TBLUNI        D7O(X)
135600021023     C     KTAB1         READE     TABEL00F                               31
135700021023     C                   END
135800940927     C*----------------------------------------------------*
135900940927     C*  Caricamento tabella 2A E 2Z
136000090402     C                   Z-ADD     0             XX                3 0
136100940927     C                   MOVEL     '2A'          KCOD
136200940927     C     KTAB1         CHAIN     TABEL                              31
136300940927     C     *IN31         DOWEQ     '0'
136400090402     C     XX            ANDLT     250
136500940927     C                   MOVEL     TBLUNI        DS2A
136600940930     C     TBLFLG        IFEQ      ' '
136700090402     C                   ADD       1             XX
136800090402     C                   MOVEL     TBLKEY        C2A(XX)
136900090402     C                   MOVEL     §2ADES        D2A(XX)
137000110530     C                   MOVEL     §2ADES        D2Al(XX)
137100940927     C                   END
137200940927     C     KTAB1         READE     TABEL                                  31
137300940927     C                   END
137400940927     C*
137500940927     C                   MOVEL     '2Z'          KCOD
137600940929     C     KTAB1         CHAIN     TABEL                              31
137700940927     C     *IN31         DOWEQ     '0'
137800940927     C                   MOVEL     TBLUNI        DS2A
137900940927     C     TBLFLG        IFEQ      ' '
138000090402     C                   Z-ADD     1             XX
138100940927     C                   MOVEL     TBLKEY        COD2Z             3
138200090402     C     COD2Z         LOOKUP    C2A(XX)                                32
138300940930     C     *IN32         IFEQ      '1'
138400940927     C                   MOVEL     TBLUNI        DS2Z
138500090402     C                   MOVEL     §2ZTIG        T2Z(XX)
138600940927     C                   END
138700940927     C                   END
138800940929     C     KTAB1         READE     TABEL                                  31
138900940927     C                   END
139000940928     C*----------------------------------------------------*
139100931209     C                   ENDSR
139200931209     O*-----------------------------------------------------*
139300141128     O****TABEL     E            ANNTBL
139400141128     O****                       TBLFLG
139500021024**
139600940928Codice evento inesistente                                             01
139700940928Tipologia giacenza inesistente                                        02
139800940928Codice disposizione inesistente                                       03
139900940928Immettere tipologia giacenza                                          04
140000940928Immettere codice disposizione                                         05
140100940928Immettere numero riaperture consentite                                06
140200960110Testo del messaggio obbligatorio                                      07
140300960110Il valore immesso per il campo non è valido                           08
140400960110Tipologia già prevista                                                09
140500021023Codice anomalia inesistente                                           10
140600111028Codice "Chi Autorizza" errato                                         11
140700940419**
140800940419IMMISSIONE
140900940419VARIAZIONE
141000940927INTERROGAZIONE
141100940928**
1412009409284041424344
141300940928**
1414009409284546474849
141500021024**
1416000210246061626364
141700031215**
141800031215525354
141900031215**
142000031215575859
142100031215**
142200031215727374
