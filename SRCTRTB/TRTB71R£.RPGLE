000100891030     H DFTNAME(CN0500) DATEDIT(*YMD)
000200110811
000300110811      *?  ATTENZIONE!!  ?
000400110811      *?    Questa tabella è utilizzata anche dal pgm TNTA61R  ?
000500110811      *?    'Interrogazione abilitazioni clienti'              ?
000600110811      *?    In caso di aggiunta/modifica campi alla tabella    ?
000700110811      *?    verificare se sono validi per la visualizzazione   ?
000800110811      *?    da TA61                                            ?
000900110811
001000891030     F*----------------------* USO DEGLI INDICATORI *----------------*
001100931209     F*  KC        FINE                                              *
001200940228     F*  KE        RIPRISTINO                                        *
001300940228     F*  KF        CONFERMA                                          *
001400931209     F*  KL        RITORNO                                           *
001500931209     F*  KQ        ANNULLAMENTO                                      *
001600891030     F*--------------------------------------------------------------*
001700940929     FTRTB71D   CF   E             WORKSTN
001800940929     F*----------
001900940929     FTABEL00F  UF A E           K DISK
002000170120     fTABELx0F  Uf A e           k disk    rename(tabel:tabelx) prefix(x)
002100170120     F*----------
002200120302     ftntbe01l  if   e           k disk
002300940929     F*----------
002400980317     F**CNACO00FIF  E           K        DISK
002500940418     D*--------------------------------------------------------------*
002600940418     D*   SCHIERE                                                    *
002700940701     D*--------------------------------------------------------------*
002800940929     D*  Caricamento capoconto clienti
002900940929     D*--------------------------------------------------------------*
003000120302     D ERR             S             70    DIM(9) CTDATA PERRCD(1)
003100940929     D COST            S             16    DIM(3) CTDATA PERRCD(1)
003200940928     D*--------------------------------------------------------------*
003300940929     D VFLP            S              1    DIM(4)
003400940929     D DPAR            S             31    DIM(4) CTDATA PERRCD(1)
003500940929     D KPAR            S              1    DIM(4) CTDATA PERRCD(4)
003600940929     D PGM             S             10    DIM(4) CTDATA PERRCD(1)
003700940929     D*--------------------------------------------------------------*
003800940929     D CODP            S              2    DIM(30)
003900940929     D VCP             S              2    DIM(30)
004000940929     D IN1             S              2  0 DIM(30) CTDATA PERRCD(30)
004100940929     D*--------------------------------------------------------------*
004200090225     D C3K             S              7    DIM(9999)
004300090225     D D3K             S             89    DIM(9999)
004400090224     D*--------------------------------------------------------------*
004500110210     D C7Q             S              2    DIM(9999)
004600110210     D C7P             S              2    DIM(9999)
004700110210     D C7R             S              2    DIM(9999)
004800110210     D C7S             S              2    DIM(9999)
004900110210     D C7Rvag          S              1    DIM(9999)
005000120925     D C7Rbam          S              2    DIM(999)
005100940929     D*--------------------------------------------------------------*
005200940929     D CPA             S              2    DIM(50)
005300940929     D COMO            S            100    DIM(4)
005400940928     D*--------------------------------------------------------------*
005500940928     D*  DS                                                          *
005600940928     D*--------------------------------------------------------------*
005700940930     D DSTB71        E DS                  EXTNAME(DS7PQRS)
005800940929     D*
005900170120     d Tabelds       e ds                  extname(TABEL00F) inz
006000170120     d xTabelds      e ds                  extname(TABELX0F) inz
006100170120     d                                     prefix(x)
006200170120      *-
006300940929     D DS7U          E DS
006400090224     D DS7R          E DS
006500090224     D DS3K          E DS
006600900608      *
006700891030     D KPJBA         E DS
006800911118      *
006900971210     D TRUL12        E DS                  EXTNAME(TRUL12DS)
007000911118     D UTEDSE        E DS                  EXTNAME(UTEDSE0F)
007100940929     D  TCU                  398    697
007200940929     D                                     DIM(50)
007300940929     D  KCU                  698    847P 0
007400940929     D                                     DIM(50)
007500940929     D                                     PACKEVEN
007600980317     D TIBS69        E DS                  EXTNAME(TIBS69DS)
007700980317     D CNACO         E DS                  EXTNAME(CNACO00F)
007800980317     D CNIND         E DS                  EXTNAME(CNIND00F)
007900980317     D CNCLP         E DS                  EXTNAME(CNCLP00F)
008000980317     D FNCLS         E DS                  EXTNAME(FNCLS00F)
008100940929     D TCUDS           DS
008200940929     D  F3                     3      3
008300940929     D  F4                     4      4
008400940929     D  F56                    5      6
008500960212     D                 DS
008600960212     D  WNUM                   1      7  0
008700960212     D  WCOD                   8      8
008800960212     D  WKEY                   1      8
008900120302
009000120302       // -?Tabella OSR
009100120302     d dOSR          e ds
009200120302     d Ktbecod         s                   like(TBECOD)
009300120302     d                                     inz('OSR')
009400120302     d wokOSR          s               n   inz(*off)
009500120302
009600940228      *
009700940228     D                SDS
009800940927     D  V1CPGM                 1     10
009900891030     I*--------------------------------------------------------------*
010000891030     I*                        I N P U T                             *
010100891030     I*--------------------------------------------------------------*
010200940929     ITB71D01
010300940929     I              V1CCAS                      VFLP(1)
010400940929     I              V1CGIA                      VFLP(2)
010500940929     I              V1CCON                      VFLP(3)
010600940929     I              V1CARR                      VFLP(4)
010700940929     ITB71D02
010800940929     I              V2CC01                      VCP(1)
010900940929     I              V2CC02                      VCP(2)
011000940929     I              V2CC03                      VCP(3)
011100940929     I              V2CC04                      VCP(4)
011200940929     I              V2CC05                      VCP(5)
011300940929     I              V2CC06                      VCP(6)
011400940929     I              V2CC07                      VCP(7)
011500940929     I              V2CC08                      VCP(8)
011600940929     I              V2CC09                      VCP(9)
011700940929     I              V2CC10                      VCP(10)
011800940929     I              V2CC11                      VCP(11)
011900940929     I              V2CC12                      VCP(12)
012000940929     I              V2CC13                      VCP(13)
012100940929     I              V2CC14                      VCP(14)
012200940929     I              V2CC15                      VCP(15)
012300940929     I              V2CC16                      VCP(16)
012400940929     I              V2CC17                      VCP(17)
012500940929     I              V2CC18                      VCP(18)
012600940929     I              V2CC19                      VCP(19)
012700940929     I              V2CC20                      VCP(20)
012800940929     I              V2CC21                      VCP(21)
012900940929     I              V2CC22                      VCP(22)
013000940929     I              V2CC23                      VCP(23)
013100940929     I              V2CC24                      VCP(24)
013200940929     I              V2CC25                      VCP(25)
013300940929     I              V2CC26                      VCP(26)
013400940929     I              V2CC27                      VCP(27)
013500940929     I              V2CC28                      VCP(28)
013600940929     I              V2CC29                      VCP(29)
013700940929     I              V2CC30                      VCP(30)
013800931209     C*--------------------------------------------------------------*
013900931209     C*    C I C L O    P R I N C I P A L E                          *
014000931209     C*--------------------------------------------------------------*
014100940928     C                   EXSR      INZ01
014200940928     C*  Loop gestione videate
014300940927     C     WFINE         DOWNE     'S'
014400940929     C     WTPVID        CASEQ     '1'           GESD01
014500931209     C                   END
014600931209     C                   END
014700931209     C* Fine programma
014800931209     C     FINE          TAG
014900980317     C                   CLEAR                   TIBS69
015000980317     C                   MOVEL     'C'           I69TLA
015100980317     C                   CALL      'TIBS69R'
015200980317     C                   PARM                    TIBS69
015300980317     C                   PARM                    CNACO
015400980317     C                   PARM                    CNIND
015500980317     C                   PARM                    CNCLP
015600980317     C                   PARM                    FNCLS
015700980317     C*
015800931209     C                   SETON                                        LR
015900940929     C*---------------------------------------------------*
016000940929     C*  Gestione prima videata
016100940929     C*---------------------------------------------------*
016200940929     C     GESD01        BEGSR
016300940929     C*  Emissione videata
016400940929     C                   MOVEL     *BLANKS       V1DFUN
016500940929     C                   WRITE     TB71T01
016600940929     C                   EXFMT     TB71D01
016700940929     C                   SETOFF                                       404142
016800940929     C                   SETOFF                                       434428
016900940929     C                   MOVEL     *BLANKS       $MSG
017000940929     C*---------------------------------------------------*
017100940929     C*  Fine elaborazione
017200940929     C     *INKC         IFEQ      '1'
017300940929     C                   MOVEL     'S'           WFINE
017400940929     C                   GOTO      FINVD1
017500940929     C                   END
017600940929     C*---------------------------------------------------*
017700940929     C*  Richiamo Ricerca alfabetica
017800940929     C     *INKB         IFEQ      '1'
017900940929     C                   MOVEL     V1DCLI        ACORAG
018000940929     C                   Z-ADD     CLIITA        SAVKCI            4 0
018100940929     C                   MOVE      V1CCLI        SAVKSC            7 0
018200940929     C                   CALL      'XALFA1'
018300940929     C                   PARM                    V1CRSU
018400940929     C                   PARM                    KKUT
018500940929     C                   PARM                    ACORAG
018600940929     C                   PARM                    SAVKCI
018700940929     C                   PARM                    SAVKSC
018800940929     C                   PARM                    PARSTA            1 0
018900961017     C                   PARM                    PARFLR           90
019000940929     C     PARSTA        IFNE      -1
019100940929     C                   MOVE      SAVKSC        V1CCLI
019200940929     C                   MOVEL     ACORAG        V1DCLI
019300940929     C                   END
019400940929     C                   GOTO      FINVD1
019500940929     C                   END
019600940929     C*---------------------------------------------------*
019700940929     C* Controlli
019800940929     C                   EXSR      CTR01
019900940929     C* Se non ci sono errori emetto seconda videata
020000940929     C     *IN28         IFEQ      '0'
020100940929     C     *IN90         ANDEQ     '0'
020200940929     C                   EXSR      RICD02
020300940929     C                   END
020400940929     C*
020500940929     C     FINVD1        ENDSR
020600940929     C*---------------------------------------------------*
020700940929     C*  Gestione Seconda videata
020800940929     C*---------------------------------------------------*
020900940929     C     GESD02        BEGSR
021000940929     C*  Emissione videata
021100940929     C                   WRITE     TB71T01
021200940929     C                   EXFMT     TB71D02
021300940929     C                   SETOFF                                       404142
021400940929     C                   SETOFF                                       434445
021500940929     C                   SETOFF                                       464748
021600940929     C                   SETOFF                                       495051
021700940929     C                   SETOFF                                       525354
021800940929     C                   SETOFF                                       555657
021900940929     C                   SETOFF                                       585960
022000940929     C                   SETOFF                                       616263
022100940929     C                   SETOFF                                       646566
022200940929     C                   SETOFF                                       676869
022300060307     C                   SETOFF                                       28
022400940929     C                   MOVEL     *BLANKS       $MSG
022500940929     C*  Fine elaborazione
022600940929     C     *INKC         IFEQ      '1'
022700940929     C                   MOVEL     'S'           WFINE             1
022800940929     C                   GOTO      FINVD2
022900940929     C                   END
023000940929     C*  Ritorno prima videata
023100940929     C     *INKL         IFEQ      '1'
023200160901     c                   except    vuoto
023300940929     C                   GOTO      FINVD2
023400940929     C                   END
023500940929     C*  Annullamento
023600940929     C     *INKQ         IFEQ      '1'
023700940929     C                   EXSR      ANNUL
023800170120     C                   EXSR      AGGIORx
023900940929     C                   MOVEL     'S'           WFINE
024000940929     C                   GOTO      FINVD2
024100940929     C                   END
024200940929     C*  controlli
024300940929     C                   EXSR      CTR02
024400960209     C   28              MOVEL     '0'           *INKF
024500960209     C   28              GOTO      FINVD2
024600940929     C*  Ripristino
024700940929     C     *INKE         IFEQ      '1'
024800940929     C                   EXSR      RIPRIS
024900170120     C                   EXSR      AGGIORx
025000940929     C                   MOVEL     'S'           WFINE
025100120925     c* se c'e' una particolarità di restituizione doc mittente
025200120925     c* chiamo  tabella CLI
025300120925     c                   if        chiamacli='S'
025400120925     c                   clear                   kpjbu
025500120926     c                   movel     v2ccli        kpjbu
025600120925     c                   call      'TNTB53R'
025700120925     c                   parm                    kpjba
025800120925     c                   endif
025900120925     c
026000940929     C                   GOTO      FINVD2
026100940929     C                   END
026200940929     C*  Conferma
026300940929     C     *INKF         IFEQ      '1'
026400940929     C                   EXSR      AGGIOR
026500170120     C                   EXSR      AGGIORx
026600120925     c
026700120925     c* se c'e' una particolarità di restituizione doc mittente
026800120925     c* chiamo  tabella CLI solo la prima volta o quando la tolgo la partic
026900120925     c                   if        (chiamacli='S' and giadoc=' ') or
027000120925     c                             (chiamacli=' ' and giadoc='S')
027100120925     c                   clear                   kpjbu
027200120926     c                   movel     v2ccli        kpjbu
027300120925     c                   call      'TNTB53R'
027400120925     c                   parm                    kpjba
027500120925     c                   endif
027600120925     c
027700940929     C                   GOTO      FINVD2
027800940929     C                   END
027900940929     C*----------------------------------------------------*
028000940929     C     FINVD2        ENDSR
028100940929     C*----------------------------------------------------*
028200940929     C*  INZ01   Gestione inizializzazione prima videata   *
028300940929     C*----------------------------------------------------*
028400940929     C     INZ01         BEGSR
028500940929     C*  Inizializzo prima videata
028600940929     C                   MOVEL     '1'           WTPVID            1
028700940929     C                   MOVEL     *BLANKS       V1CCLI
028800940929     C                   MOVEL     *BLANKS       V1DCLI
028900940929     C                   MOVEA     *BLANKS       VFLP
029000940929     C                   MOVEL     *BLANKS       $MSG
029100940929     C                   SETOFF                                       404142
029200940929     C                   SETOFF                                       434445
029300940929     C*
029400940929     C                   ENDSR
029500940929     C*----------------------------------------------------*
029600940929     C*  RICD02  Richiamo gestione 2 videata               *
029700940929     C*----------------------------------------------------*
029800940929     C     RICD02        BEGSR
029900940929     C                   MOVE      V1CCLI        V2CCLI
030000940929     C                   MOVE      V1DCLI        V2DCLI
030100960212     C                   MOVEL     V1CCLI        WNUM
030200960212     C                   MOVEL     WNUM          KKEY
030300940929     C* Controllo particolarità selezionate
030400940929     C                   Z-ADD     0             X
030500940929     C     X             DOWLT     4
030600940929     C     *INKC         ANDNE     '1'
030700940929     C                   ADD       1             X
030800940929     C     VFLP(X)       IFEQ      '1'
030900940929     C                   MOVE      KPAR(X)       KKEY
031000940929     C                   MOVEL     DPAR(X)       V2DPAR
031100940929     C                   MOVEA     COMO(X)       CPA
031200940929     C                   MOVEL     PGM(X)        PGMPAR           10
031300940929     C                   EXSR      INZ02
031400940929     C     WFINE         DOWNE     'S'
031500940929     C     *INKL         ANDEQ     '0'
031600940930     C     *INKF         ANDEQ     '0'
031700940929     C                   EXSR      GESD02
031800940929     C                   END
031900940929     C                   END
032000941003     C                   MOVE      '0'           *INKL
032100941003     C                   MOVE      '0'           *INKF
032200940929     C                   END
032300940929     C*
032400940929     C     *INKC         IFNE      '1'
032500940929     C                   EXSR      INZ01
032600940929     C                   END
032700940929     C*
032800940929     C                   ENDSR
032900940929     C*----------------------------------------------------*
033000940929     C*  INZ02   Gestione inizializz. seconda videata      *
033100940929     C*----------------------------------------------------*
033200940929     C     INZ02         BEGSR
033300120925     c                   clear                   giadoc            1
033400940929     C*
033500960228     C                   SETOFF                                       03
033600960228     C     X             IFEQ      3
033700960228     C                   SETON                                        03
033800960228     C                   END
033900040806     C                   SETOFF                                       02
034000040806     C     X             IFEQ      2
034100040806     C                   SETON                                        02
034200040806     C                   END
034300940929     C                   MOVE      '7U'          KCOD
034400940929     C     KTAB2         CHAIN     TABEL00F                           31
034500940929     C     *IN31         IFEQ      '0'
034600940929     C                   MOVEL     TBLUNI        DS7U
034700940929     C     TBLFLG        IFNE      ' '
034800940929     C                   SETON                                        92
034900940929     C                   MOVEL     COST(3)       V1DFUN
035000940929     C                   ELSE
035100940929     C                   SETON                                        93
035200940929     C                   MOVEL     COST(2)       V1DFUN
035300940929     C                   END
035400940929     C                   ELSE
035500940929     C                   MOVEL     COST(1)       V1DFUN
035600940929     C                   CLEAR                   DS7U
035700940929     C                   END
035800940929     C*
035900960228     C                   MOVEL     §7USTB        V2CSTB
036000040806     C                   MOVEL     §7USRE        V2CSRS
036100060307     C                   MOVEL     §7Uldcr       V2Cldcr
036200940929     C                   MOVEA     §7UCPA        VCP
036300960223     C                   MOVEL     ' '           V2CTRA
036400940930     C     TBLFTT        IFEQ      '1'
036500940929     C     *IN31         ANDEQ     '0'
036600940929     C                   MOVEL     ' '           V2CTRA
036700940929     C                   END
036800120925     c* Verifico se già presente particolarità restituizione doc per non
036900120925     c*  richiedere la CLI
037000120925     c                   z-add     1             y
037100120925     c                   do        30            y
037200120925     c                   if        vcp(y)<>*blanks
037300120925     C     VCP(Y)        lookup    C7Rbam                                 32
037400120925     c                   if        *in32
037500120925     c                   eval      Giadoc='S'
037600120925     c                   leave
037700120925     c                   endif
037800120925     c                   endif
037900120925     c                   enddo
038000940929     C*
038100940929     C                   ENDSR
038200940929     C*----------------------------------------------------*
038300940929     C*  CTR01  Controlli prima videata                    *
038400940929     C*----------------------------------------------------*
038500940929     C     CTR01         BEGSR
038600940929     C*
038700940929     C                   SETOFF                                       90
038800150812     C                   MOVEA     VFLP          SELPAR
038900940929     C*   Se richiesta ricerca richiamo pgm.
039000940929     C     '?'           SCAN      V1CCLI                                 32
039100940929     C     *IN32         IFEQ      '1'
039200150812     C     v1ccli        orne      *blank
039300150812     C     *in32         andne     '1'
039400150812     C     selpar        andeq     *blank
039500150812     c                   if        not *in32
039600150812     c                   movel     v1ccli        pcli
039700150812     c                   endif
039800940929     C                   MOVEA     *BLANKS       VFLP
039900940929     C                   SETON                                        90
040000050112     C                   CALL      'TRTB71R1'
040100050112     C                   PARM                    KNSIF
040200050112     C                   PARM                    PCLI              8
040300050112     C                   PARM                    PRAG             25
040400170126     C                   PARM                    KNMUS            10
040500170126     C                   PARM                    V1CRSU           20
040600050112     C                   MOVEL     PCLI          V1CCLI
040700050112     C     PCLI          IFNE      *BLANKS
040800050112     C                   MOVE      PCLI          WTPPAR            1
040900940929     C                   Z-ADD     1             X
041000940929     C     WTPPAR        LOOKUP    KPAR(X)                                32
041100940929     C   32              MOVEL     '1'           VFLP(X)
041200940929     C                   END
041300940929     C                   MOVEL     RAGUT         V1CRSU
041400940929     C                   END
041500940929     C*  Controllo attendibilità codice cliente
041600940929     C     V1CCLI        IFEQ      *ZEROS
041700940929     C     V1CCLI        OREQ      *BLANKS
041800940929     C                   SETON                                        4028
041900940929     C                   MOVEL     ERR(1)        $MSG
042000940929     C                   GOTO      FINCT1
042100940929     C                   ELSE
042200940929     C                   MOVE      V1CCLI        KKSC
042300980317     C***        KACO      CHAINCNACO00F             31
042400980317     C                   CLEAR                   TIBS69
042500980317     C                   CLEAR                   CNACO
042600980317     C                   CLEAR                   CNIND
042700980317     C                   CLEAR                   CNCLP
042800980317     C                   CLEAR                   FNCLS
042900980317     C                   MOVEL     ' '           I69TLA
043000980317     C                   MOVEL     KNSIF         I69SIF
043100980317     C                   MOVE      V1CCLI        I69KAC
043200980317     C                   CALL      'TIBS69R'
043300980317     C                   PARM                    TIBS69
043400980317     C                   PARM                    CNACO
043500980317     C                   PARM                    CNIND
043600980317     C                   PARM                    CNCLP
043700980317     C                   PARM                    FNCLS
043800980317     C     O69ERR        IFNE      *BLANK
043900940929     C                   SETON                                        4028
044000980317     C                   MOVEL     O69MSG        $MSG
044100940929     C                   GOTO      FINCT1
044200940929     C                   ELSE
044300940929     C                   MOVEL     ACORAG        V1DCLI
044400940929     C                   END
044500940929     C                   END
044600940929     C*  Controllo sel.particolarità
044700940929     C                   MOVEA     VFLP          SELPAR            4
044800940929     C     SELPAR        IFEQ      *BLANKS
044900940929     C                   SETON                                        414243
045000060307     C                   SETON                                        4428
045100940929     C                   MOVEL     ERR(3)        $MSG
045200940929     C                   END
045300940929     C*
045400940929     C     FINCT1        ENDSR
045500940929     C*----------------------------------------------------*
045600940929     C*  CTR02 : Controlli seconda videata                 *
045700940929     C*----------------------------------------------------*
045800940929     C     CTR02         BEGSR
045900120925     c                   clear                   chiamacli         1
046000940929     C*
046100940929     C*  Controllo codici particolarità
046200940929     C                   MOVEA     VCP           COMO60           60
046300090224     C*
046400940929     C     COMO60        IFEQ      *BLANKS
046500090224     C*
046600050506     C     V2CSRS        ifne      'S'
046700060307     C     V2Cldcr       andle     '00'
046800940929     C                   SETON                                        28
046900940929     C                   DO        30            Y                 2 0
047000940929     C                   MOVEL     IN1(Y)        C                 2 0
047100940929     C                   MOVEL     '1'           *IN(C)
047200940929     C                   MOVEL     ERR(5)        $MSG
047300940929     C                   END
047400050506     C                   END
047500940929     C*
047600940929     C                   ELSE
047700090224     C*
047800940929     C                   DO        30            Y
047900090224     C*
048000940929     C     VCP(Y)        IFNE      *BLANKS
048100940929     C     '?'           SCAN      VCP(Y)                                 33
048200940930     C     *IN33         IFEQ      '1'
048300940930     C                   MOVEL     DSTB71        SAVDS           100
048400940930     C                   MOVEL     *BLANKS       KPJBU
048500940930     C                   MOVEL     DSTB71        KPJBU
048600940930     C                   CALL      PGMPAR
048700940930     C                   PARM                    KPJBA
048800940930     C                   MOVEL     KPJBU         DSTB71
048900940930     C                   MOVEL     DS7COD        VCP(Y)
049000940930     C                   MOVEL     SAVDS         DSTB71
049100940930     C                   END
049200090224     C*
049300090224     C     VCP(Y)        LOOKUP    CPA                                    32
049400940929     C     *IN32         IFEQ      '0'
049500940929     C                   SETON                                        28
049600940929     C                   MOVEL     IN1(Y)        C                 2 0
049700940929     C                   MOVEL     '1'           *IN(C)
049800940929     C                   MOVEL     ERR(4)        $MSG
049900090224     C                   Else
050000090224      *
050100090224      * Controlla se invierà il VAG
050200090224     c                   z-add     1             Xy                3 0
050300090224     C     VCP(Y)        lookup    C7R(Xy)                                32
050400090224      **
050500090224      * se abilitato invio del VAG
050600090224     C   32              if        c7rVAG(Xy) = 'S'
050700090224      **
050800090224      **  Controlla x il cliente sulla 3K se abilitato invio VAG
050900090224     c                   z-add     1             Xx
051000090224     C     v1cCLI        lookup    C3K(Xx)                                32
051100090224     C     *in32         ifeq      '1'
051200090224     c                   movel(p)  D3K(Xx)       ds3K
051300090224      *
051400090224      *  se manca il flag x invio VAG sulla 3K segnala l'errore
051500090224     c                   if        §3KNMC <> 'S'
051600090224     C                   SETON                                        28
051700090224     C                   MOVEL     IN1(Y)        C                 2 0
051800090224     C                   MOVEL     '1'           *IN(C)
051900090224     C                   MOVEL     ERR(7)        $MSG
052000090224     C                   End
052100090224     C                   Else
052200090224      **  Manca il cliente sulla 3K
052300090224     C                   SETON                                        28
052400090224     C                   MOVEL     IN1(Y)        C                 2 0
052500090224     C                   MOVEL     '1'           *IN(C)
052600090224     C                   MOVEL     ERR(8)        $MSG
052700090224     C                   End
052800090224      **
052900090224     C                   End
053000120925     c
053100120925     C     VCP(Y)        lookup    C7Rbam                                 32
053200120925     c                   if        *in32
053300120925     c                   eval      chiamacli='S'
053400120925     c                   endif
053500120302
053600120302       //?Controllo se cliente abilitato a tab OSR
053700120302     c                   IF        vcp(y) = 'RC'
053800120302     c                   exsr      ctrOSR
053900120302     c                   IF        not wokOSR
054000120302     c                   eval      *in28 = *on
054100120302     c                   eval      c = in1(y)
054200120302     c                   eval      *in(c) = '1'
054300120302     c                   eval      $msg = err(9)
054400120302     c                   ENDIF
054500120302     c                   ENDIF
054600090224      *
054700940929     C                   END
054800940929     C                   END
054900090224     C*
055000940929     C                   END
055100940929     C*
055200940929     C                   END
055300940929     C*
055400040806     C* CONTROLLO CHE SOLO PER LE PARTICOLARITÀ GIACENZA SI INDICHI
055500040806     C* SE IL CLIENTE VUOLE IL RESO ANCHE CON LE TRAZIONI RIDOTTE
055600040806     C     V2CSRS        IFEQ      'S'
055700040806     C     KPAR(X)       ANDNE     'Q'
055800040806     C                   SETON                                        28
055900040806     C                   MOVEL     ERR(6)        $MSG
056000040806     C                   ENDIF
056100060307      * giorni personalizzati per limite aperture con data consegna
056200060307      * richiesta validi solo per particolarità giacenze
056300060307     c                   if        kpar(x) <> 'Q' and
056400060615     c                              v2cldcr <> *blank and v2cldcr <> *zero
056500060307     C                   SETON                                        28
056600060307     C                   MOVEL     ERR(6)        $MSG
056700060307     C                   ENDIF
056800940929     C                   ENDSR
056900940929     C*----------------------------------------------------*
057000940929     C*  AGGIOR: Aggiornamento paramteri                   *
057100940929     C*----------------------------------------------------*
057200940929     C     AGGIOR        BEGSR
057300940929     C*------------------------------------------------------*
057400940929     C*  Procedo all'aggiornamento della tabella
057500940929     C     KTAB2         CHAIN     TABEL00F                           31
057600940929     C                   MOVEA     VCP           §7UCPA
057700960228     C                   MOVEL     V2CSTB        §7USTB
057800040806     C                   MOVEL     V2CSRS        §7USRE
057900060307     C                   MOVEL     V2Cldcr       §7Uldcr
058000960228     C                   MOVEL     *BLANKS       TBLUNI
058100960228     C                   MOVEL     DS7U          TBLUNI
058200940929     C     V2CTRA        IFEQ      ' '
058300940929     C                   MOVEL     '1'           TBLFTT
058400940929     C                   ELSE
058500940929     C                   MOVEL     ' '           TBLFTT
058600940929     C                   END
058700960307     C                   CLEAR                   TBLFTR
058800960307     C                   CLEAR                   TBLDTR
058900940929     C*
059000940929     C     *IN31         IFEQ      '1'
059100940929     C                   Z-ADD     1             TBLKUT
059200940929     C                   MOVEL     '7U'          TBLCOD
059300940929     C                   MOVEL     *BLANKS       TBLKEY
059400960212     C                   MOVEL     KKEY          TBLKEY
059500940929     C                   WRITE     TABEL
059600940929     C                   ELSE
059700940929     C                   UPDATE    TABEL
059800940929     C                   END
059900170120     C*----------------------------------------------------*
060000170120     C                   ENDSR
060100170120     C*--------------------------------------------------------------*
060200170120     C*  aggiorX
060300170120     C*--------------------------------------------------------------*
060400170120     C     AGGIORx       BEGSR
060500170120     C*--
060600170120     c     Ktab2         chain     tabelx0f
060700170120      *
060800170120     c                   movel     Tabelds       xTabelds
060900170120      *
061000170120     c                   if        %found(tabelx0f)
061100170120     c                   update    tabelx
061200170120     c                   else
061300170120     c                   write     tabelx
061400170120     c                   endif
061500170120     C*--
061600940929     C                   ENDSR
061700940929     C*--------------------------------------------------------------*
061800940929     C*  ANNUL: Annullamento                                         *
061900940929     C*--------------------------------------------------------------*
062000940929     C     ANNUL         BEGSR
062100940929     C*----------------------------------------------------*
062200940929     C     KTAB2         CHAIN     TABEL00F                           80
062300940929     C     *IN80         IFEQ      '0'
062400940929     C                   MOVEL     '*'           TBLFLG
062500970204     C     V2CTRA        IFEQ      ' '
062600970204     C                   MOVEL     '1'           TBLFTT
062700970204     C                   ELSE
062800970204     C                   MOVEL     ' '           TBLFTT
062900970204     C                   END
063000970204     C                   CLEAR                   TBLFTR
063100970204     C                   CLEAR                   TBLDTR
063200940929     C                   EXCEPT    ANNTBL
063300940929     C                   END
063400940929     C*----------------------------------------------------*
063500940929     C                   ENDSR
063600940929     C*--------------------------------------------------------------*
063700940929     C*  RIPRI: Ripristino                                           *
063800940929     C*--------------------------------------------------------------*
063900940929     C     RIPRIS        BEGSR
064000940929     C*----------------------------------------------------*
064100940929     C     KTAB2         CHAIN     TABEL00F                           80
064200940929     C     *IN80         IFEQ      '0'
064300940929     C                   MOVEL     ' '           TBLFLG
064400970204     C     V2CTRA        IFEQ      ' '
064500970204     C                   MOVEL     '1'           TBLFTT
064600970204     C                   ELSE
064700970204     C                   MOVEL     ' '           TBLFTT
064800970204     C                   END
064900970204     C                   CLEAR                   TBLFTR
065000970204     C                   CLEAR                   TBLDTR
065100940929     C                   EXCEPT    ANNTBL
065200940929     C                   END
065300940929     C*----------------------------------------------------*
065400940929     C                   ENDSR
065500940929     C*----------------------------------------------------*
065600940929     C*  *INZSR: Operazioni iniziali                       *
065700940929     C*----------------------------------------------------*
065800940929     C     *INZSR        BEGSR
065900940929     C*
066000940929     C     *ENTRY        PLIST
066100940929     C                   PARM                    KPJBA
066200931209     C*  Richiamo XPARUT
066300931209     C                   Z-ADD     1             CODUT
066400931209     C                   CALL      'XPARUT'
066500931209     C                   PARM                    UTEDSE
066600931209     C     CODUT         CABEQ     -1            FINE
066700940927     C                   MOVEL     RAGUT         V1CRSU
066800940929     C*--- RICERCA CAPOCONTI------------------------------------------*
066900120925     C                   DO        50            X
067000940929     C                   MOVE      TCU(X)        TCUDS
067100940929     C     F56           IFEQ      'CG'
067200940929     C     F4            ANDEQ     '1'
067300940929     C     F3            ANDEQ     '0'
067400940929     C                   Z-ADD     KCU(X)        CLIITA            4 0
067500940929     C                   END
067600940929     C                   END
067700931209     C*----------------------------------------------------*
067800931209     C* Definizione chiavi                                 *
067900940929     C     KACO          KLIST
068000940929     C                   KFLD                    KKUT
068100940929     C                   KFLD                    KKCC
068200940929     C                   KFLD                    KKSC
068300940929     C                   Z-ADD     CLIITA        KKCC
068400940929     C*                                                    *
068500940927     C     KTAB1         KLIST
068600940927     C                   KFLD                    KKUT
068700940927     C                   KFLD                    KCOD
068800940929     C*                                                    *
068900940927     C     KTAB2         KLIST
069000940927     C                   KFLD                    KKUT
069100940927     C                   KFLD                    KCOD
069200940927     C                   KFLD                    KKEY
069300931209     C*----------------------------------------------------*
069400931209     C* Definizione variabili                              *
069500940929     C     *LIKE         DEFINE    ACOKCC        KKCC
069600940929     C     *LIKE         DEFINE    ACOKSC        KKSC
069700940927     C     *LIKE         DEFINE    TBLKUT        KKUT
069800940927     C     *LIKE         DEFINE    TBLCOD        KCOD
069900940927     C     *LIKE         DEFINE    TBLKEY        KKEY
070000971210     C******************************************************
070100971210     C* Richiamo pgm per controllo modificabilità tabella
070200120302      * non faccio controllo per s.i. di prova
070300120302     c                   IF        %subst(knsif:7:1) <> 'P'
070400971210     C                   CLEAR                   TRUL12
070500971210     C                   MOVEL     'L'           I12TLA
070600971210     C                   MOVEL     KNSIF         I12NSI
070700971210     C                   MOVEL     '7U'          I12COD
070800971210     C                   CALL      'TRUL12R'
070900971210     C                   PARM                    TRUL12
071000971210     C     O12ERR        IFEQ      '1'
071100040806     C*****                GOTO FINE
071200971210     C                   END
071300120302     c                   ENDIF
071400931209     C*----------------------------------------------------*
071500940929     C*  Caricamento tabella 7P
071600940927     C                   Z-ADD     1             KKUT
071700940929     C                   MOVE      '7P'          KCOD
071800120925     C                   Z-ADD     0             X
071900160726     C     KTAB1         CHAIN(n)  TABEL00F                           31
072000940927     C     *IN31         DOWEQ     '0'
072100110210     C     X             ANDLT     9999
072200940929     C     TBLFLG        IFEQ      ' '
072300940725     C                   ADD       1             X
072400940929     C                   MOVE      TBLKEY        C7P(X)
072500940929     C                   END
072600160726     C     KTAB1         READE(n)  TABEL00F                               31
072700940725     C                   END
072800940927     C*----------------------------------------------------*
072900940929     C*  Caricamento tabella 7Q
073000120925     C                   Z-ADD     0             X
073100940929     C                   MOVEL     '7Q'          KCOD
073200160726     C     KTAB1         CHAIN(n)  TABEL                              31
073300940927     C     *IN31         DOWEQ     '0'
073400110210     C     X             ANDLT     9999
073500940929     C     TBLFLG        IFEQ      ' '
073600940927     C                   ADD       1             X
073700940929     C                   MOVEL     TBLKEY        C7Q(X)
073800940927     C                   END
073900160726     C     KTAB1         READE(n)  TABEL                                  31
074000940927     C                   END
074100940929     C*----------------------------------------------------*
074200940929     C*  Caricamento tabella 7R
074300120925     C                   Z-ADD     0             X                 5 0
074400120925     C                   Z-ADD     0             xx                5 0
074500940929     C                   MOVEL     '7R'          KCOD
074600160726     C     KTAB1         CHAIN(n)  TABEL                              31
074700940929     C     *IN31         DOWEQ     '0'
074800110210     C     X             ANDLT     9999
074900940929     C     TBLFLG        IFEQ      ' '
075000940929     C                   ADD       1             X
075100940929     C                   MOVEL     TBLKEY        C7R(X)
075200090224     c                   movel(p)  tblUNI        ds7R
075300090224     c                   if        §7R7T1 <> *blank or
075400090224     c                             §7R7T2 <> *blank or
075500090224     c                             §7R7T3 <> *blank or
075600090224     c                             §7R7T4 <> *blank or
075700090224     c                             §7R7T5 <> *blank
075800090224      * abilitato invio del VAG
075900090224     c                   move      'S'           C7Rvag(X)
076000940929     C                   END
076100120925     c
076200120925     c* controllo se ha la restituzione dei documenti
076300120925     c                   if        §7r1bf<>' '
076400120925     c                   add       1             xx
076500120925     c                   movel     tblkey        C7Rbam(xx)
076600120925     c                   endif
076700120925     c
076800090224     C                   END
076900160726     C     KTAB1         READE(n)  TABEL                                  31
077000940929     C                   END
077100940929     C*----------------------------------------------------*
077200940929     C*  Caricamento tabella 7S
077300120925     C                   Z-ADD     0             X
077400940929     C                   MOVEL     '7S'          KCOD
077500160726     C     KTAB1         CHAIN(n)  TABEL                              31
077600940929     C     *IN31         DOWEQ     '0'
077700110210     C     X             ANDLT     9999
077800940929     C     TBLFLG        IFEQ      ' '
077900940929     C                   ADD       1             X
078000940929     C                   MOVEL     TBLKEY        C7S(X)
078100940929     C                   END
078200160726     C     KTAB1         READE(n)  TABEL                                  31
078300940929     C                   END
078400940929     C*----------------------------------------------------*
078500090224     C*  Caricamento tabella 3K
078600090225     C                   Z-ADD     0             Xx                5 0
078700090224     C                   MOVEL     '3K'          KCOD
078800160726     C     KTAB1         CHAIN(n)  TABEL                              31
078900090224     C     *IN31         DOWEQ     '0'
079000090224     C     Xx            ANDLT     9999
079100090224     C     TBLFLG        IFEQ      ' '
079200090225     C                   ADD       1             Xx
079300090224     C                   MOVEL     TBLKEY        C3K(Xx)
079400090224     C                   MOVEL     TBLuni        D3K(Xx)
079500090224     C                   END
079600160726     C     KTAB1         READE(n)  TABEL                                  31
079700090224     C                   END
079800090224     C*----------------------------------------------------*
079900940929     C                   MOVEA     C7P           COMO(1)
080000940929     C                   MOVEA     C7Q           COMO(2)
080100940929     C                   MOVEA     C7R           COMO(3)
080200940929     C                   MOVEA     C7S           COMO(4)
080300940929     C*  Inizializzo DS per call pgm. particolarità
080400940930     C                   CLEAR                   DSTB71
080500940930     C                   MOVEL     'S'           DS7RIC
080600940930     C                   MOVEL     'V'           DS7GES
080700940928     C*----------------------------------------------------*
080800940929     C                   ENDSR
080900120302
081000120302      /free
081100120302       //--------------------------------------------------------------
081200120302       //?Controllo se cliente abilitato alla OSR
081300120302       //--------------------------------------------------------------
081400120302       BEGSR CtrOSR;
081500120302
081600120302       //?Controllo se cliente abilitato a tab OSR
081700120302         wokOSR = *off;
081800120302         setll kTBEcod TNTBE01L;
081900120302         reade kTBEcod TNTBE01L;
082000120302         DOW not %eof(TNTBE01L);
082100120302           dOSR = TBEuni;
082200120302           IF  d§OSRcli  = V2Ccli or
082300120302               d§OSRcl2  = V2Ccli or
082400120302               d§OSRcl3  = V2Ccli or
082500120302               d§OSRcl4  = V2Ccli or
082600120302               d§OSRcl5  = V2Ccli or
082700120302               d§OSRcl6  = V2Ccli or
082800120302               d§OSRcl7  = V2Ccli or
082900120302               d§OSRcl8  = V2Ccli or
083000120302               d§OSRcl9  = V2Ccli or
083100120302               d§OSRcl10 = V2Ccli or
083200120302               d§OSRcl11 = V2Ccli or
083300120302               d§OSRcl12 = V2Ccli or
083400120302               d§OSRcl13 = V2Ccli or
083500120302               d§OSRcl14 = V2Ccli or
083600120302               d§OSRcl15 = V2Ccli or
083700120302               d§OSRcl16 = V2Ccli;
083800120302             wokOSR = *on;
083900120302           ELSE;
084000120302             wokOSR = *off;
084100120302           ENDIF;
084200120302
084300120302           IF  wokOSR;
084400120302             leave;
084500120302           ENDIF;
084600120302           reade kTBEcod TNTBE01L;
084700120302         ENDDO;
084800120302
084900120302       ENDSR;
085000120302      /end-free
085100120302
085200931209     OTABEL     E            ANNTBL
085300931209     O                       TBLFLG
085400970204     O                       TBLFTT
085500970204     O                       TBLFTR
085600970204     O                       TBLDTR
085700160901     OTABEL     E            VUOTO
085800090224**  MESSAGGI DI ERRORE   (ERR)
085900940929Immettere codice cliente                                              01
086000940929Codice cliente inesistente                                            02
086100940929Effettuare almeno una scelta                                          03
086200940929Codice particolarità inesistente                                      04
086300940929Immettere almeno un codice particolarità                              05
086400040806Immettere SOLO PER PARTICOLARITA GIACENZE !                           06
086500090224Su tab.3K non è selezionato l'invio con VAG delle Mancate Consegne !  07
086600090224Manca la tab.3K codificata per il cliente x l'invio del VAG !         08
086700120302Cliente non abilitato in tabella OSR                                  09
086800940929**  MODALITA' PGM.
086900940419IMMISSIONE
087000940419VARIAZIONE
087100940929ANNULLATO
087200940929**  Descrizioni Particolarità gestite
087300940929 PARTICOLARITA' CONTRASSEGNI
087400940929   PARTICOLARITA' GIACENZE
087500940929   PARTICOLARITA' CONSEGNE
087600940929PARTICOLARITA' VARIE ALL'ARRIVO
087700940929**  Codice particolarità
087800940929PQRS
087900940929**  PGM. gestione particolarità
088000940929TRTB72R
088100940929TRTB69R
088200940929TRTB70R
088300940929TRTB73R
088400940929** Indicatori Particolarità
088500940929404142434445464748495051525354555657585960616263646566676869
