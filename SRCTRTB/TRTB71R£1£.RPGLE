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
002000120302     ftntbe01l  if   e           k disk
002100940929     F*----------
002200980317     F**CNACO00FIF  E           K        DISK
002300940418     D*--------------------------------------------------------------*
002400940418     D*   SCHIERE                                                    *
002500940701     D*--------------------------------------------------------------*
002600940929     D*  Caricamento capoconto clienti
002700940929     D*--------------------------------------------------------------*
002800120302     D ERR             S             70    DIM(9) CTDATA PERRCD(1)
002900940929     D COST            S             16    DIM(3) CTDATA PERRCD(1)
003000940928     D*--------------------------------------------------------------*
003100940929     D VFLP            S              1    DIM(4)
003200940929     D DPAR            S             31    DIM(4) CTDATA PERRCD(1)
003300940929     D KPAR            S              1    DIM(4) CTDATA PERRCD(4)
003400940929     D PGM             S             10    DIM(4) CTDATA PERRCD(1)
003500940929     D*--------------------------------------------------------------*
003600940929     D CODP            S              2    DIM(30)
003700940929     D VCP             S              2    DIM(30)
003800940929     D IN1             S              2  0 DIM(30) CTDATA PERRCD(30)
003900940929     D*--------------------------------------------------------------*
004000090225     D C3K             S              7    DIM(9999)
004100090225     D D3K             S             89    DIM(9999)
004200090224     D*--------------------------------------------------------------*
004300110210     D C7Q             S              2    DIM(9999)
004400110210     D C7P             S              2    DIM(9999)
004500110210     D C7R             S              2    DIM(9999)
004600110210     D C7S             S              2    DIM(9999)
004700110210     D C7Rvag          S              1    DIM(9999)
004800120925     D C7Rbam          S              2    DIM(999)
004900940929     D*--------------------------------------------------------------*
005000940929     D CPA             S              2    DIM(50)
005100940929     D COMO            S            100    DIM(4)
005200940928     D*--------------------------------------------------------------*
005300940928     D*  DS                                                          *
005400940928     D*--------------------------------------------------------------*
005500940930     D DSTB71        E DS                  EXTNAME(DS7PQRS)
005600940929     D*
005700940929     D DS7U          E DS
005800090224     D DS7R          E DS
005900090224     D DS3K          E DS
006000900608      *
006100891030     D KPJBA         E DS
006200911118      *
006300971210     D TRUL12        E DS                  EXTNAME(TRUL12DS)
006400911118     D UTEDSE        E DS                  EXTNAME(UTEDSE0F)
006500940929     D  TCU                  398    697
006600940929     D                                     DIM(50)
006700940929     D  KCU                  698    847P 0
006800940929     D                                     DIM(50)
006900940929     D                                     PACKEVEN
007000980317     D TIBS69        E DS                  EXTNAME(TIBS69DS)
007100980317     D CNACO         E DS                  EXTNAME(CNACO00F)
007200980317     D CNIND         E DS                  EXTNAME(CNIND00F)
007300980317     D CNCLP         E DS                  EXTNAME(CNCLP00F)
007400980317     D FNCLS         E DS                  EXTNAME(FNCLS00F)
007500940929     D TCUDS           DS
007600940929     D  F3                     3      3
007700940929     D  F4                     4      4
007800940929     D  F56                    5      6
007900960212     D                 DS
008000960212     D  WNUM                   1      7  0
008100960212     D  WCOD                   8      8
008200960212     D  WKEY                   1      8
008300120302
008400120302       // -?Tabella OSR
008500120302     d dOSR          e ds
008600120302     d Ktbecod         s                   like(TBECOD)
008700120302     d                                     inz('OSR')
008800120302     d wokOSR          s               n   inz(*off)
008900120302
009000940228      *
009100940228     D                SDS
009200940927     D  V1CPGM                 1     10
009300891030     I*--------------------------------------------------------------*
009400891030     I*                        I N P U T                             *
009500891030     I*--------------------------------------------------------------*
009600940929     ITB71D01
009700940929     I              V1CCAS                      VFLP(1)
009800940929     I              V1CGIA                      VFLP(2)
009900940929     I              V1CCON                      VFLP(3)
010000940929     I              V1CARR                      VFLP(4)
010100940929     ITB71D02
010200940929     I              V2CC01                      VCP(1)
010300940929     I              V2CC02                      VCP(2)
010400940929     I              V2CC03                      VCP(3)
010500940929     I              V2CC04                      VCP(4)
010600940929     I              V2CC05                      VCP(5)
010700940929     I              V2CC06                      VCP(6)
010800940929     I              V2CC07                      VCP(7)
010900940929     I              V2CC08                      VCP(8)
011000940929     I              V2CC09                      VCP(9)
011100940929     I              V2CC10                      VCP(10)
011200940929     I              V2CC11                      VCP(11)
011300940929     I              V2CC12                      VCP(12)
011400940929     I              V2CC13                      VCP(13)
011500940929     I              V2CC14                      VCP(14)
011600940929     I              V2CC15                      VCP(15)
011700940929     I              V2CC16                      VCP(16)
011800940929     I              V2CC17                      VCP(17)
011900940929     I              V2CC18                      VCP(18)
012000940929     I              V2CC19                      VCP(19)
012100940929     I              V2CC20                      VCP(20)
012200940929     I              V2CC21                      VCP(21)
012300940929     I              V2CC22                      VCP(22)
012400940929     I              V2CC23                      VCP(23)
012500940929     I              V2CC24                      VCP(24)
012600940929     I              V2CC25                      VCP(25)
012700940929     I              V2CC26                      VCP(26)
012800940929     I              V2CC27                      VCP(27)
012900940929     I              V2CC28                      VCP(28)
013000940929     I              V2CC29                      VCP(29)
013100940929     I              V2CC30                      VCP(30)
013200931209     C*--------------------------------------------------------------*
013300931209     C*    C I C L O    P R I N C I P A L E                          *
013400931209     C*--------------------------------------------------------------*
013500940928     C                   EXSR      INZ01
013600940928     C*  Loop gestione videate
013700940927     C     WFINE         DOWNE     'S'
013800940929     C     WTPVID        CASEQ     '1'           GESD01
013900931209     C                   END
014000931209     C                   END
014100931209     C* Fine programma
014200931209     C     FINE          TAG
014300980317     C                   CLEAR                   TIBS69
014400980317     C                   MOVEL     'C'           I69TLA
014500980317     C                   CALL      'TIBS69R'
014600980317     C                   PARM                    TIBS69
014700980317     C                   PARM                    CNACO
014800980317     C                   PARM                    CNIND
014900980317     C                   PARM                    CNCLP
015000980317     C                   PARM                    FNCLS
015100980317     C*
015200931209     C                   SETON                                        LR
015300940929     C*---------------------------------------------------*
015400940929     C*  Gestione prima videata
015500940929     C*---------------------------------------------------*
015600940929     C     GESD01        BEGSR
015700940929     C*  Emissione videata
015800940929     C                   MOVEL     *BLANKS       V1DFUN
015900940929     C                   WRITE     TB71T01
016000940929     C                   EXFMT     TB71D01
016100940929     C                   SETOFF                                       404142
016200940929     C                   SETOFF                                       434428
016300940929     C                   MOVEL     *BLANKS       $MSG
016400940929     C*---------------------------------------------------*
016500940929     C*  Fine elaborazione
016600940929     C     *INKC         IFEQ      '1'
016700940929     C                   MOVEL     'S'           WFINE
016800940929     C                   GOTO      FINVD1
016900940929     C                   END
017000940929     C*---------------------------------------------------*
017100940929     C*  Richiamo Ricerca alfabetica
017200940929     C     *INKB         IFEQ      '1'
017300940929     C                   MOVEL     V1DCLI        ACORAG
017400940929     C                   Z-ADD     CLIITA        SAVKCI            4 0
017500940929     C                   MOVE      V1CCLI        SAVKSC            7 0
017600940929     C                   CALL      'XALFA1'
017700940929     C                   PARM                    V1CRSU
017800940929     C                   PARM                    KKUT
017900940929     C                   PARM                    ACORAG
018000940929     C                   PARM                    SAVKCI
018100940929     C                   PARM                    SAVKSC
018200940929     C                   PARM                    PARSTA            1 0
018300961017     C                   PARM                    PARFLR           90
018400940929     C     PARSTA        IFNE      -1
018500940929     C                   MOVE      SAVKSC        V1CCLI
018600940929     C                   MOVEL     ACORAG        V1DCLI
018700940929     C                   END
018800940929     C                   GOTO      FINVD1
018900940929     C                   END
019000940929     C*---------------------------------------------------*
019100940929     C* Controlli
019200940929     C                   EXSR      CTR01
019300940929     C* Se non ci sono errori emetto seconda videata
019400940929     C     *IN28         IFEQ      '0'
019500940929     C     *IN90         ANDEQ     '0'
019600940929     C                   EXSR      RICD02
019700940929     C                   END
019800940929     C*
019900940929     C     FINVD1        ENDSR
020000940929     C*---------------------------------------------------*
020100940929     C*  Gestione Seconda videata
020200940929     C*---------------------------------------------------*
020300940929     C     GESD02        BEGSR
020400940929     C*  Emissione videata
020500940929     C                   WRITE     TB71T01
020600940929     C                   EXFMT     TB71D02
020700940929     C                   SETOFF                                       404142
020800940929     C                   SETOFF                                       434445
020900940929     C                   SETOFF                                       464748
021000940929     C                   SETOFF                                       495051
021100940929     C                   SETOFF                                       525354
021200940929     C                   SETOFF                                       555657
021300940929     C                   SETOFF                                       585960
021400940929     C                   SETOFF                                       616263
021500940929     C                   SETOFF                                       646566
021600940929     C                   SETOFF                                       676869
021700060307     C                   SETOFF                                       28
021800940929     C                   MOVEL     *BLANKS       $MSG
021900940929     C*  Fine elaborazione
022000940929     C     *INKC         IFEQ      '1'
022100940929     C                   MOVEL     'S'           WFINE             1
022200940929     C                   GOTO      FINVD2
022300940929     C                   END
022400940929     C*  Ritorno prima videata
022500940929     C     *INKL         IFEQ      '1'
022600160901     c                   except    vuoto
022700940929     C                   GOTO      FINVD2
022800940929     C                   END
022900940929     C*  Annullamento
023000940929     C     *INKQ         IFEQ      '1'
023100940929     C                   EXSR      ANNUL
023200940929     C                   MOVEL     'S'           WFINE
023300940929     C                   GOTO      FINVD2
023400940929     C                   END
023500940929     C*  controlli
023600940929     C                   EXSR      CTR02
023700960209     C   28              MOVEL     '0'           *INKF
023800960209     C   28              GOTO      FINVD2
023900940929     C*  Ripristino
024000940929     C     *INKE         IFEQ      '1'
024100940929     C                   EXSR      RIPRIS
024200940929     C                   MOVEL     'S'           WFINE
024300120925     c* se c'e' una particolarità di restituizione doc mittente
024400120925     c* chiamo  tabella CLI
024500120925     c                   if        chiamacli='S'
024600120925     c                   clear                   kpjbu
024700120926     c                   movel     v2ccli        kpjbu
024800120925     c                   call      'TNTB53R'
024900120925     c                   parm                    kpjba
025000120925     c                   endif
025100120925     c
025200940929     C                   GOTO      FINVD2
025300940929     C                   END
025400940929     C*  Conferma
025500940929     C     *INKF         IFEQ      '1'
025600940929     C                   EXSR      AGGIOR
025700120925     c
025800120925     c* se c'e' una particolarità di restituizione doc mittente
025900120925     c* chiamo  tabella CLI solo la prima volta o quando la tolgo la partic
026000120925     c                   if        (chiamacli='S' and giadoc=' ') or
026100120925     c                             (chiamacli=' ' and giadoc='S')
026200120925     c                   clear                   kpjbu
026300120926     c                   movel     v2ccli        kpjbu
026400120925     c                   call      'TNTB53R'
026500120925     c                   parm                    kpjba
026600120925     c                   endif
026700120925     c
026800940929     C                   GOTO      FINVD2
026900940929     C                   END
027000940929     C*----------------------------------------------------*
027100940929     C     FINVD2        ENDSR
027200940929     C*----------------------------------------------------*
027300940929     C*  INZ01   Gestione inizializzazione prima videata   *
027400940929     C*----------------------------------------------------*
027500940929     C     INZ01         BEGSR
027600940929     C*  Inizializzo prima videata
027700940929     C                   MOVEL     '1'           WTPVID            1
027800940929     C                   MOVEL     *BLANKS       V1CCLI
027900940929     C                   MOVEL     *BLANKS       V1DCLI
028000940929     C                   MOVEA     *BLANKS       VFLP
028100940929     C                   MOVEL     *BLANKS       $MSG
028200940929     C                   SETOFF                                       404142
028300940929     C                   SETOFF                                       434445
028400940929     C*
028500940929     C                   ENDSR
028600940929     C*----------------------------------------------------*
028700940929     C*  RICD02  Richiamo gestione 2 videata               *
028800940929     C*----------------------------------------------------*
028900940929     C     RICD02        BEGSR
029000940929     C                   MOVE      V1CCLI        V2CCLI
029100940929     C                   MOVE      V1DCLI        V2DCLI
029200960212     C                   MOVEL     V1CCLI        WNUM
029300960212     C                   MOVEL     WNUM          KKEY
029400940929     C* Controllo particolarità selezionate
029500940929     C                   Z-ADD     0             X
029600940929     C     X             DOWLT     4
029700940929     C     *INKC         ANDNE     '1'
029800940929     C                   ADD       1             X
029900940929     C     VFLP(X)       IFEQ      '1'
030000940929     C                   MOVE      KPAR(X)       KKEY
030100940929     C                   MOVEL     DPAR(X)       V2DPAR
030200940929     C                   MOVEA     COMO(X)       CPA
030300940929     C                   MOVEL     PGM(X)        PGMPAR           10
030400940929     C                   EXSR      INZ02
030500940929     C     WFINE         DOWNE     'S'
030600940929     C     *INKL         ANDEQ     '0'
030700940930     C     *INKF         ANDEQ     '0'
030800940929     C                   EXSR      GESD02
030900940929     C                   END
031000940929     C                   END
031100941003     C                   MOVE      '0'           *INKL
031200941003     C                   MOVE      '0'           *INKF
031300940929     C                   END
031400940929     C*
031500940929     C     *INKC         IFNE      '1'
031600940929     C                   EXSR      INZ01
031700940929     C                   END
031800940929     C*
031900940929     C                   ENDSR
032000940929     C*----------------------------------------------------*
032100940929     C*  INZ02   Gestione inizializz. seconda videata      *
032200940929     C*----------------------------------------------------*
032300940929     C     INZ02         BEGSR
032400120925     c                   clear                   giadoc            1
032500940929     C*
032600960228     C                   SETOFF                                       03
032700960228     C     X             IFEQ      3
032800960228     C                   SETON                                        03
032900960228     C                   END
033000040806     C                   SETOFF                                       02
033100040806     C     X             IFEQ      2
033200040806     C                   SETON                                        02
033300040806     C                   END
033400940929     C                   MOVE      '7U'          KCOD
033500940929     C     KTAB2         CHAIN     TABEL00F                           31
033600940929     C     *IN31         IFEQ      '0'
033700940929     C                   MOVEL     TBLUNI        DS7U
033800940929     C     TBLFLG        IFNE      ' '
033900940929     C                   SETON                                        92
034000940929     C                   MOVEL     COST(3)       V1DFUN
034100940929     C                   ELSE
034200940929     C                   SETON                                        93
034300940929     C                   MOVEL     COST(2)       V1DFUN
034400940929     C                   END
034500940929     C                   ELSE
034600940929     C                   MOVEL     COST(1)       V1DFUN
034700940929     C                   CLEAR                   DS7U
034800940929     C                   END
034900940929     C*
035000960228     C                   MOVEL     §7USTB        V2CSTB
035100040806     C                   MOVEL     §7USRE        V2CSRS
035200060307     C                   MOVEL     §7Uldcr       V2Cldcr
035300940929     C                   MOVEA     §7UCPA        VCP
035400960223     C                   MOVEL     ' '           V2CTRA
035500940930     C     TBLFTT        IFEQ      '1'
035600940929     C     *IN31         ANDEQ     '0'
035700940929     C                   MOVEL     ' '           V2CTRA
035800940929     C                   END
035900120925     c* Verifico se già presente particolarità restituizione doc per non
036000120925     c*  richiedere la CLI
036100120925     c                   z-add     1             y
036200120925     c                   do        30            y
036300120925     c                   if        vcp(y)<>*blanks
036400120925     C     VCP(Y)        lookup    C7Rbam                                 32
036500120925     c                   if        *in32
036600120925     c                   eval      Giadoc='S'
036700120925     c                   leave
036800120925     c                   endif
036900120925     c                   endif
037000120925     c                   enddo
037100940929     C*
037200940929     C                   ENDSR
037300940929     C*----------------------------------------------------*
037400940929     C*  CTR01  Controlli prima videata                    *
037500940929     C*----------------------------------------------------*
037600940929     C     CTR01         BEGSR
037700940929     C*
037800940929     C                   SETOFF                                       90
037900150812     C                   MOVEA     VFLP          SELPAR
038000940929     C*   Se richiesta ricerca richiamo pgm.
038100940929     C     '?'           SCAN      V1CCLI                                 32
038200940929     C     *IN32         IFEQ      '1'
038300150812     C     v1ccli        orne      *blank
038400150812     C     *in32         andne     '1'
038500150812     C     selpar        andeq     *blank
038600150812     c                   if        not *in32
038700150812     c                   movel     v1ccli        pcli
038800150812     c                   endif
038900940929     C                   MOVEA     *BLANKS       VFLP
039000940929     C                   SETON                                        90
039100050112     C                   CALL      'TRTB71R1'
039200050112     C                   PARM                    KNSIF
039300050112     C                   PARM                    PCLI              8
039400050112     C                   PARM                    PRAG             25
039500050112     C                   MOVEL     PCLI          V1CCLI
039600050112     C     PCLI          IFNE      *BLANKS
039700050112     C                   MOVE      PCLI          WTPPAR            1
039800940929     C                   Z-ADD     1             X
039900940929     C     WTPPAR        LOOKUP    KPAR(X)                                32
040000940929     C   32              MOVEL     '1'           VFLP(X)
040100940929     C                   END
040200940929     C                   MOVEL     RAGUT         V1CRSU
040300940929     C                   END
040400940929     C*  Controllo attendibilità codice cliente
040500940929     C     V1CCLI        IFEQ      *ZEROS
040600940929     C     V1CCLI        OREQ      *BLANKS
040700940929     C                   SETON                                        4028
040800940929     C                   MOVEL     ERR(1)        $MSG
040900940929     C                   GOTO      FINCT1
041000940929     C                   ELSE
041100940929     C                   MOVE      V1CCLI        KKSC
041200980317     C***        KACO      CHAINCNACO00F             31
041300980317     C                   CLEAR                   TIBS69
041400980317     C                   CLEAR                   CNACO
041500980317     C                   CLEAR                   CNIND
041600980317     C                   CLEAR                   CNCLP
041700980317     C                   CLEAR                   FNCLS
041800980317     C                   MOVEL     ' '           I69TLA
041900980317     C                   MOVEL     KNSIF         I69SIF
042000980317     C                   MOVE      V1CCLI        I69KAC
042100980317     C                   CALL      'TIBS69R'
042200980317     C                   PARM                    TIBS69
042300980317     C                   PARM                    CNACO
042400980317     C                   PARM                    CNIND
042500980317     C                   PARM                    CNCLP
042600980317     C                   PARM                    FNCLS
042700980317     C     O69ERR        IFNE      *BLANK
042800940929     C                   SETON                                        4028
042900980317     C                   MOVEL     O69MSG        $MSG
043000940929     C                   GOTO      FINCT1
043100940929     C                   ELSE
043200940929     C                   MOVEL     ACORAG        V1DCLI
043300940929     C                   END
043400940929     C                   END
043500940929     C*  Controllo sel.particolarità
043600940929     C                   MOVEA     VFLP          SELPAR            4
043700940929     C     SELPAR        IFEQ      *BLANKS
043800940929     C                   SETON                                        414243
043900060307     C                   SETON                                        4428
044000940929     C                   MOVEL     ERR(3)        $MSG
044100940929     C                   END
044200940929     C*
044300940929     C     FINCT1        ENDSR
044400940929     C*----------------------------------------------------*
044500940929     C*  CTR02 : Controlli seconda videata                 *
044600940929     C*----------------------------------------------------*
044700940929     C     CTR02         BEGSR
044800120925     c                   clear                   chiamacli         1
044900940929     C*
045000940929     C*  Controllo codici particolarità
045100940929     C                   MOVEA     VCP           COMO60           60
045200090224     C*
045300940929     C     COMO60        IFEQ      *BLANKS
045400090224     C*
045500050506     C     V2CSRS        ifne      'S'
045600060307     C     V2Cldcr       andle     '00'
045700940929     C                   SETON                                        28
045800940929     C                   DO        30            Y                 2 0
045900940929     C                   MOVEL     IN1(Y)        C                 2 0
046000940929     C                   MOVEL     '1'           *IN(C)
046100940929     C                   MOVEL     ERR(5)        $MSG
046200940929     C                   END
046300050506     C                   END
046400940929     C*
046500940929     C                   ELSE
046600090224     C*
046700940929     C                   DO        30            Y
046800090224     C*
046900940929     C     VCP(Y)        IFNE      *BLANKS
047000940929     C     '?'           SCAN      VCP(Y)                                 33
047100940930     C     *IN33         IFEQ      '1'
047200940930     C                   MOVEL     DSTB71        SAVDS           100
047300940930     C                   MOVEL     *BLANKS       KPJBU
047400940930     C                   MOVEL     DSTB71        KPJBU
047500940930     C                   CALL      PGMPAR
047600940930     C                   PARM                    KPJBA
047700940930     C                   MOVEL     KPJBU         DSTB71
047800940930     C                   MOVEL     DS7COD        VCP(Y)
047900940930     C                   MOVEL     SAVDS         DSTB71
048000940930     C                   END
048100090224     C*
048200090224     C     VCP(Y)        LOOKUP    CPA                                    32
048300940929     C     *IN32         IFEQ      '0'
048400940929     C                   SETON                                        28
048500940929     C                   MOVEL     IN1(Y)        C                 2 0
048600940929     C                   MOVEL     '1'           *IN(C)
048700940929     C                   MOVEL     ERR(4)        $MSG
048800090224     C                   Else
048900090224      *
049000090224      * Controlla se invierà il VAG
049100090224     c                   z-add     1             Xy                3 0
049200090224     C     VCP(Y)        lookup    C7R(Xy)                                32
049300090224      **
049400090224      * se abilitato invio del VAG
049500090224     C   32              if        c7rVAG(Xy) = 'S'
049600090224      **
049700090224      **  Controlla x il cliente sulla 3K se abilitato invio VAG
049800090224     c                   z-add     1             Xx
049900090224     C     v1cCLI        lookup    C3K(Xx)                                32
050000090224     C     *in32         ifeq      '1'
050100090224     c                   movel(p)  D3K(Xx)       ds3K
050200090224      *
050300090224      *  se manca il flag x invio VAG sulla 3K segnala l'errore
050400090224     c                   if        §3KNMC <> 'S'
050500090224     C                   SETON                                        28
050600090224     C                   MOVEL     IN1(Y)        C                 2 0
050700090224     C                   MOVEL     '1'           *IN(C)
050800090224     C                   MOVEL     ERR(7)        $MSG
050900090224     C                   End
051000090224     C                   Else
051100090224      **  Manca il cliente sulla 3K
051200090224     C                   SETON                                        28
051300090224     C                   MOVEL     IN1(Y)        C                 2 0
051400090224     C                   MOVEL     '1'           *IN(C)
051500090224     C                   MOVEL     ERR(8)        $MSG
051600090224     C                   End
051700090224      **
051800090224     C                   End
051900120925     c
052000120925     C     VCP(Y)        lookup    C7Rbam                                 32
052100120925     c                   if        *in32
052200120925     c                   eval      chiamacli='S'
052300120925     c                   endif
052400120302
052500120302       //?Controllo se cliente abilitato a tab OSR
052600120302     c                   IF        vcp(y) = 'RC'
052700120302     c                   exsr      ctrOSR
052800120302     c                   IF        not wokOSR
052900120302     c                   eval      *in28 = *on
053000120302     c                   eval      c = in1(y)
053100120302     c                   eval      *in(c) = '1'
053200120302     c                   eval      $msg = err(9)
053300120302     c                   ENDIF
053400120302     c                   ENDIF
053500090224      *
053600940929     C                   END
053700940929     C                   END
053800090224     C*
053900940929     C                   END
054000940929     C*
054100940929     C                   END
054200940929     C*
054300040806     C* CONTROLLO CHE SOLO PER LE PARTICOLARITÀ GIACENZA SI INDICHI
054400040806     C* SE IL CLIENTE VUOLE IL RESO ANCHE CON LE TRAZIONI RIDOTTE
054500040806     C     V2CSRS        IFEQ      'S'
054600040806     C     KPAR(X)       ANDNE     'Q'
054700040806     C                   SETON                                        28
054800040806     C                   MOVEL     ERR(6)        $MSG
054900040806     C                   ENDIF
055000060307      * giorni personalizzati per limite aperture con data consegna
055100060307      * richiesta validi solo per particolarità giacenze
055200060307     c                   if        kpar(x) <> 'Q' and
055300060615     c                              v2cldcr <> *blank and v2cldcr <> *zero
055400060307     C                   SETON                                        28
055500060307     C                   MOVEL     ERR(6)        $MSG
055600060307     C                   ENDIF
055700940929     C                   ENDSR
055800940929     C*----------------------------------------------------*
055900940929     C*  AGGIOR: Aggiornamento paramteri                   *
056000940929     C*----------------------------------------------------*
056100940929     C     AGGIOR        BEGSR
056200940929     C*------------------------------------------------------*
056300940929     C*  Procedo all'aggiornamento della tabella
056400940929     C     KTAB2         CHAIN     TABEL00F                           31
056500940929     C                   MOVEA     VCP           §7UCPA
056600960228     C                   MOVEL     V2CSTB        §7USTB
056700040806     C                   MOVEL     V2CSRS        §7USRE
056800060307     C                   MOVEL     V2Cldcr       §7Uldcr
056900960228     C                   MOVEL     *BLANKS       TBLUNI
057000960228     C                   MOVEL     DS7U          TBLUNI
057100940929     C     V2CTRA        IFEQ      ' '
057200940929     C                   MOVEL     '1'           TBLFTT
057300940929     C                   ELSE
057400940929     C                   MOVEL     ' '           TBLFTT
057500940929     C                   END
057600960307     C                   CLEAR                   TBLFTR
057700960307     C                   CLEAR                   TBLDTR
057800940929     C*
057900940929     C     *IN31         IFEQ      '1'
058000940929     C                   Z-ADD     1             TBLKUT
058100940929     C                   MOVEL     '7U'          TBLCOD
058200940929     C                   MOVEL     *BLANKS       TBLKEY
058300960212     C                   MOVEL     KKEY          TBLKEY
058400940929     C                   WRITE     TABEL
058500940929     C                   ELSE
058600940929     C                   UPDATE    TABEL
058700940929     C                   END
058800940929     C*----------------------------------------------------*
058900940929     C                   ENDSR
059000940929     C*--------------------------------------------------------------*
059100940929     C*  ANNUL: Annullamento                                         *
059200940929     C*--------------------------------------------------------------*
059300940929     C     ANNUL         BEGSR
059400940929     C*----------------------------------------------------*
059500940929     C     KTAB2         CHAIN     TABEL00F                           80
059600940929     C     *IN80         IFEQ      '0'
059700940929     C                   MOVEL     '*'           TBLFLG
059800970204     C     V2CTRA        IFEQ      ' '
059900970204     C                   MOVEL     '1'           TBLFTT
060000970204     C                   ELSE
060100970204     C                   MOVEL     ' '           TBLFTT
060200970204     C                   END
060300970204     C                   CLEAR                   TBLFTR
060400970204     C                   CLEAR                   TBLDTR
060500940929     C                   EXCEPT    ANNTBL
060600940929     C                   END
060700940929     C*----------------------------------------------------*
060800940929     C                   ENDSR
060900940929     C*--------------------------------------------------------------*
061000940929     C*  RIPRI: Ripristino                                           *
061100940929     C*--------------------------------------------------------------*
061200940929     C     RIPRIS        BEGSR
061300940929     C*----------------------------------------------------*
061400940929     C     KTAB2         CHAIN     TABEL00F                           80
061500940929     C     *IN80         IFEQ      '0'
061600940929     C                   MOVEL     ' '           TBLFLG
061700970204     C     V2CTRA        IFEQ      ' '
061800970204     C                   MOVEL     '1'           TBLFTT
061900970204     C                   ELSE
062000970204     C                   MOVEL     ' '           TBLFTT
062100970204     C                   END
062200970204     C                   CLEAR                   TBLFTR
062300970204     C                   CLEAR                   TBLDTR
062400940929     C                   EXCEPT    ANNTBL
062500940929     C                   END
062600940929     C*----------------------------------------------------*
062700940929     C                   ENDSR
062800940929     C*----------------------------------------------------*
062900940929     C*  *INZSR: Operazioni iniziali                       *
063000940929     C*----------------------------------------------------*
063100940929     C     *INZSR        BEGSR
063200940929     C*
063300940929     C     *ENTRY        PLIST
063400940929     C                   PARM                    KPJBA
063500931209     C*  Richiamo XPARUT
063600931209     C                   Z-ADD     1             CODUT
063700931209     C                   CALL      'XPARUT'
063800931209     C                   PARM                    UTEDSE
063900931209     C     CODUT         CABEQ     -1            FINE
064000940927     C                   MOVEL     RAGUT         V1CRSU
064100940929     C*--- RICERCA CAPOCONTI------------------------------------------*
064200120925     C                   DO        50            X
064300940929     C                   MOVE      TCU(X)        TCUDS
064400940929     C     F56           IFEQ      'CG'
064500940929     C     F4            ANDEQ     '1'
064600940929     C     F3            ANDEQ     '0'
064700940929     C                   Z-ADD     KCU(X)        CLIITA            4 0
064800940929     C                   END
064900940929     C                   END
065000931209     C*----------------------------------------------------*
065100931209     C* Definizione chiavi                                 *
065200940929     C     KACO          KLIST
065300940929     C                   KFLD                    KKUT
065400940929     C                   KFLD                    KKCC
065500940929     C                   KFLD                    KKSC
065600940929     C                   Z-ADD     CLIITA        KKCC
065700940929     C*                                                    *
065800940927     C     KTAB1         KLIST
065900940927     C                   KFLD                    KKUT
066000940927     C                   KFLD                    KCOD
066100940929     C*                                                    *
066200940927     C     KTAB2         KLIST
066300940927     C                   KFLD                    KKUT
066400940927     C                   KFLD                    KCOD
066500940927     C                   KFLD                    KKEY
066600931209     C*----------------------------------------------------*
066700931209     C* Definizione variabili                              *
066800940929     C     *LIKE         DEFINE    ACOKCC        KKCC
066900940929     C     *LIKE         DEFINE    ACOKSC        KKSC
067000940927     C     *LIKE         DEFINE    TBLKUT        KKUT
067100940927     C     *LIKE         DEFINE    TBLCOD        KCOD
067200940927     C     *LIKE         DEFINE    TBLKEY        KKEY
067300971210     C******************************************************
067400971210     C* Richiamo pgm per controllo modificabilità tabella
067500120302      * non faccio controllo per s.i. di prova
067600120302     c                   IF        %subst(knsif:7:1) <> 'P'
067700971210     C                   CLEAR                   TRUL12
067800971210     C                   MOVEL     'L'           I12TLA
067900971210     C                   MOVEL     KNSIF         I12NSI
068000971210     C                   MOVEL     '7U'          I12COD
068100971210     C                   CALL      'TRUL12R'
068200971210     C                   PARM                    TRUL12
068300971210     C     O12ERR        IFEQ      '1'
068400040806     C*****                GOTO FINE
068500971210     C                   END
068600120302     c                   ENDIF
068700931209     C*----------------------------------------------------*
068800940929     C*  Caricamento tabella 7P
068900940927     C                   Z-ADD     1             KKUT
069000940929     C                   MOVE      '7P'          KCOD
069100120925     C                   Z-ADD     0             X
069200160726     C     KTAB1         CHAIN(n)  TABEL00F                           31
069300940927     C     *IN31         DOWEQ     '0'
069400110210     C     X             ANDLT     9999
069500940929     C     TBLFLG        IFEQ      ' '
069600940725     C                   ADD       1             X
069700940929     C                   MOVE      TBLKEY        C7P(X)
069800940929     C                   END
069900160726     C     KTAB1         READE(n)  TABEL00F                               31
070000940725     C                   END
070100940927     C*----------------------------------------------------*
070200940929     C*  Caricamento tabella 7Q
070300120925     C                   Z-ADD     0             X
070400940929     C                   MOVEL     '7Q'          KCOD
070500160726     C     KTAB1         CHAIN(n)  TABEL                              31
070600940927     C     *IN31         DOWEQ     '0'
070700110210     C     X             ANDLT     9999
070800940929     C     TBLFLG        IFEQ      ' '
070900940927     C                   ADD       1             X
071000940929     C                   MOVEL     TBLKEY        C7Q(X)
071100940927     C                   END
071200160726     C     KTAB1         READE(n)  TABEL                                  31
071300940927     C                   END
071400940929     C*----------------------------------------------------*
071500940929     C*  Caricamento tabella 7R
071600120925     C                   Z-ADD     0             X                 5 0
071700120925     C                   Z-ADD     0             xx                5 0
071800940929     C                   MOVEL     '7R'          KCOD
071900160726     C     KTAB1         CHAIN(n)  TABEL                              31
072000940929     C     *IN31         DOWEQ     '0'
072100110210     C     X             ANDLT     9999
072200940929     C     TBLFLG        IFEQ      ' '
072300940929     C                   ADD       1             X
072400940929     C                   MOVEL     TBLKEY        C7R(X)
072500090224     c                   movel(p)  tblUNI        ds7R
072600090224     c                   if        §7R7T1 <> *blank or
072700090224     c                             §7R7T2 <> *blank or
072800090224     c                             §7R7T3 <> *blank or
072900090224     c                             §7R7T4 <> *blank or
073000090224     c                             §7R7T5 <> *blank
073100090224      * abilitato invio del VAG
073200090224     c                   move      'S'           C7Rvag(X)
073300940929     C                   END
073400120925     c
073500120925     c* controllo se ha la restituzione dei documenti
073600120925     c                   if        §7r1bf<>' '
073700120925     c                   add       1             xx
073800120925     c                   movel     tblkey        C7Rbam(xx)
073900120925     c                   endif
074000120925     c
074100090224     C                   END
074200160726     C     KTAB1         READE(n)  TABEL                                  31
074300940929     C                   END
074400940929     C*----------------------------------------------------*
074500940929     C*  Caricamento tabella 7S
074600120925     C                   Z-ADD     0             X
074700940929     C                   MOVEL     '7S'          KCOD
074800160726     C     KTAB1         CHAIN(n)  TABEL                              31
074900940929     C     *IN31         DOWEQ     '0'
075000110210     C     X             ANDLT     9999
075100940929     C     TBLFLG        IFEQ      ' '
075200940929     C                   ADD       1             X
075300940929     C                   MOVEL     TBLKEY        C7S(X)
075400940929     C                   END
075500160726     C     KTAB1         READE(n)  TABEL                                  31
075600940929     C                   END
075700940929     C*----------------------------------------------------*
075800090224     C*  Caricamento tabella 3K
075900090225     C                   Z-ADD     0             Xx                5 0
076000090224     C                   MOVEL     '3K'          KCOD
076100160726     C     KTAB1         CHAIN(n)  TABEL                              31
076200090224     C     *IN31         DOWEQ     '0'
076300090224     C     Xx            ANDLT     9999
076400090224     C     TBLFLG        IFEQ      ' '
076500090225     C                   ADD       1             Xx
076600090224     C                   MOVEL     TBLKEY        C3K(Xx)
076700090224     C                   MOVEL     TBLuni        D3K(Xx)
076800090224     C                   END
076900160726     C     KTAB1         READE(n)  TABEL                                  31
077000090224     C                   END
077100090224     C*----------------------------------------------------*
077200940929     C                   MOVEA     C7P           COMO(1)
077300940929     C                   MOVEA     C7Q           COMO(2)
077400940929     C                   MOVEA     C7R           COMO(3)
077500940929     C                   MOVEA     C7S           COMO(4)
077600940929     C*  Inizializzo DS per call pgm. particolarità
077700940930     C                   CLEAR                   DSTB71
077800940930     C                   MOVEL     'S'           DS7RIC
077900940930     C                   MOVEL     'V'           DS7GES
078000940928     C*----------------------------------------------------*
078100940929     C                   ENDSR
078200120302
078300120302      /free
078400120302       //--------------------------------------------------------------
078500120302       //?Controllo se cliente abilitato alla OSR
078600120302       //--------------------------------------------------------------
078700120302       BEGSR CtrOSR;
078800120302
078900120302       //?Controllo se cliente abilitato a tab OSR
079000120302         wokOSR = *off;
079100120302         setll kTBEcod TNTBE01L;
079200120302         reade kTBEcod TNTBE01L;
079300120302         DOW not %eof(TNTBE01L);
079400120302           dOSR = TBEuni;
079500120302           IF  d§OSRcli  = V2Ccli or
079600120302               d§OSRcl2  = V2Ccli or
079700120302               d§OSRcl3  = V2Ccli or
079800120302               d§OSRcl4  = V2Ccli or
079900120302               d§OSRcl5  = V2Ccli or
080000120302               d§OSRcl6  = V2Ccli or
080100120302               d§OSRcl7  = V2Ccli or
080200120302               d§OSRcl8  = V2Ccli or
080300120302               d§OSRcl9  = V2Ccli or
080400120302               d§OSRcl10 = V2Ccli or
080500120302               d§OSRcl11 = V2Ccli or
080600120302               d§OSRcl12 = V2Ccli or
080700120302               d§OSRcl13 = V2Ccli or
080800120302               d§OSRcl14 = V2Ccli or
080900120302               d§OSRcl15 = V2Ccli or
081000120302               d§OSRcl16 = V2Ccli;
081100120302             wokOSR = *on;
081200120302           ELSE;
081300120302             wokOSR = *off;
081400120302           ENDIF;
081500120302
081600120302           IF  wokOSR;
081700120302             leave;
081800120302           ENDIF;
081900120302           reade kTBEcod TNTBE01L;
082000120302         ENDDO;
082100120302
082200120302       ENDSR;
082300120302      /end-free
082400120302
082500931209     OTABEL     E            ANNTBL
082600931209     O                       TBLFLG
082700970204     O                       TBLFTT
082800970204     O                       TBLFTR
082900970204     O                       TBLDTR
083000160901     OTABEL     E            VUOTO
083100090224**  MESSAGGI DI ERRORE   (ERR)
083200940929Immettere codice cliente                                              01
083300940929Codice cliente inesistente                                            02
083400940929Effettuare almeno una scelta                                          03
083500940929Codice particolarità inesistente                                      04
083600940929Immettere almeno un codice particolarità                              05
083700040806Immettere SOLO PER PARTICOLARITA GIACENZE !                           06
083800090224Su tab.3K non è selezionato l'invio con VAG delle Mancate Consegne !  07
083900090224Manca la tab.3K codificata per il cliente x l'invio del VAG !         08
084000120302Cliente non abilitato in tabella OSR                                  09
084100940929**  MODALITA' PGM.
084200940419IMMISSIONE
084300940419VARIAZIONE
084400940929ANNULLATO
084500940929**  Descrizioni Particolarità gestite
084600940929 PARTICOLARITA' CONTRASSEGNI
084700940929   PARTICOLARITA' GIACENZE
084800940929   PARTICOLARITA' CONSEGNE
084900940929PARTICOLARITA' VARIE ALL'ARRIVO
085000940929**  Codice particolarità
085100940929PQRS
085200940929**  PGM. gestione particolarità
085300940929TRTB72R
085400940929TRTB69R
085500940929TRTB70R
085600940929TRTB73R
085700940929** Indicatori Particolarità
085800940929404142434445464748495051525354555657585960616263646566676869
