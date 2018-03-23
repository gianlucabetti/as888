000100891030     H DFTNAME(CN0500) DATEDIT(*YMD)
000200920723     F* TRTB37R *-----------------------------------------------------*
000300090617     F*              - GESTIONE TABELLA tipi incasso da abilitare     *
000400920723     F*---------------------------------------------------------------*
000500110811
000600110811      *?  ATTENZIONE!!  ?
000700110811      *?    Questa tabella è utilizzata anche dal pgm TNTA61R  ?
000800110811      *?    'Interrogazione abilitazioni clienti'              ?
000900110811      *?    In caso di aggiunta/modifica campi alla tabella    ?
001000110811      *?    verificare anche relativo pgm di interrogazione    ?
001100110811      *?    per le abilitazioni clienti --> TRTB371R.          ?
001200141007      *?  Il TRTB371R prevede 9 particolarità di tipo incasso  ?
001300141007      *?  in caso di aggiunta di nuove particolarità variare   ?
001400141007      *?  anche il TRTB371R.                                   ?
001500110811
001600920723     FTRTB37D   CF   E             WORKSTN
001700930610     F                                     SFILE(TB37DSF:NRR)
001800150813     F                                     SFILE(TB37DSF1:NRR1)
001900891030     FTABEL00F  UF A E           K DISK
002000980105     FCNACO00F  IF   E           K DISK
002100150813     FCNclp00F  IF   E           K DISK
002200920723     D SKI             S              1    DIM(3)
002300150520     D TES             S             38    DIM(2) CTDATA PERRCD(1)
002400930611     D CM1             S             13    DIM(1) CTDATA PERRCD(1)
002500080625     D ERR             S             70    DIM(13) CTDATA PERRCD(1)
002600930208     D PARAM           DS
002700930208     D  VIDFLG                 1      2
002800150813     D  parcli                 3      9
002900920723     D TCUDS           DS
003000920723     D  F1                     1      1
003100920723     D  F3                     3      3
003200920723     D  F2                     2      2
003300920723     D  F4                     4      4
003400920723     D  F56                    5      6
003500930322     D DSTM          E DS
003600930322     D KPJBA         E DS
003700920723     D  LIBSYS                92    100
003800980105     D TIBS69        E DS                  EXTNAME(TIBS69DS)
003900980105     D CNACO         E DS                  EXTNAME(CNACO00F)
004000980105     D CNIND         E DS                  EXTNAME(CNIND00F)
004100980105     D CNCLP         E DS                  EXTNAME(CNCLP00F)
004200980105     D FNCLS         E DS                  EXTNAME(FNCLS00F)
004300971210     D TRUL12        E DS                  EXTNAME(TRUL12DS)
004400150813     d datads          DS
004500150813     d  aa                            2  0
004600150813     d  mm                            2  0
004700150813     d  gg                            2  0
004800150813     d datads1         DS
004900150813     d  gg1                           2  0
005000150813     d  mm1                           2  0
005100150813     d  aa1                           2  0
005200920723     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
005300920723     D  TCU                  398    697
005400920723     D                                     DIM(50)
005500920723     D  KCU                  698    847P 0
005600920723     D                                     DIM(50)
005700920723     D                                     PACKEVEN
005800920723     D  DTU                  848    895P 0
005900920723     D                                     DIM(12)
006000920723     D                                     PACKEVEN
006100920723     I/SPACE 3
006200920731     C*****************************************************************
006300920731     C*  RIEPILOGO INDICATORI
006400920731     C*****************************************************************
006500920731     C* 01    - INSERIMENTO
006600920731     C* 02    - VARIAZIONE
006700930208     C* 10    - ON   GESTIONE TABELLA "BB"
006800951108     C* 11    - ON   GESTIONE TABELLA "BA"
006900080624     C* 10/11 - OFF  GESTIONE TABELLA "TM"   anche 17 on
007000080624     C* 12    - ON   GESTIONE TABELLA "CA"
007100080624     C* 13    - ON   GESTIONE TABELLA "BC"
007200050525     C* 14    - ON   GESTIONE TABELLA "SC"
007300060626     C* 15    - ON   GESTIONE TABELLA "TO"
007400080624     C* 16    - ON   GESTIONE TABELLA "S2"
007500080624     C* 17    - ON   GESTIONE TABELLA "TM"
007600090617     C* 18    - ON   GESTIONE TABELLA "BP"
007700150520     C* 19    - ON   GESTIONE TABELLA "TP"
007800920910     C* 20/21 - DI COMODO
007900930610     C* 35    - PULIZIA SUBFILE
008000920731     C* 80/98 - DI COMODO,RIUTILIZZABILI
008100920731     C* 99    - ERRORE GENERICO
008200920731     C*****************************************************************
008300920731     C*
008400891030     C     *ENTRY        PLIST
008500891030     C                   PARM                    KPJBA
008600891030     C                   Z-ADD     1             CODUT
008700920723     C                   CALL      'X§PARUT'
008800920723     C                   PARM                    UT§DSE
008900891030     C                   MOVEL     RAGUT         RSUT
009000920731     C                   MOVEL     RAGUT         PARDUT           30
009100920723     C*
009200930208     C                   MOVEL     KPJBU         PARAM
009300920723     C*--- RICERCA CAPOCONTI------------------------------------------*
009400920723     C                   DO        50            X                 2 0
009500920723     C                   MOVE      TCU(X)        TCUDS
009600920723     C     F56           CABNE     'CG'          END1
009700920723     C     F4            COMP      '1'                                    21
009800920723     C     F4            COMP      '2'                                    22
009900920723     C     F4            COMP      '3'                                    23
010000920723     C     F4            COMP      '6'                                    27
010100920723     C** 1 CLIENTI   21
010200920723     C** 2 FORNITORI 22
010300920723     C** 3 AGENTI    23
010400920723     C     F3            COMP      '0'                                242425
010500920723     C     F3            COMP      'I'                                    26
010600920723     C** 0 ITALIA   25
010700920723     C** 1 ESTERO   24
010800920723     ** I CAPO CONTO IVA
010900920723     C   21
011000920723     CAN 24              Z-ADD     KCU(X)        KCE               4 0
011100920723     C   21
011200920723     CAN 25              Z-ADD     KCU(X)        KCI               4 0
011300920723     C   22
011400920723     CAN 24              Z-ADD     KCU(X)        KFE               4 0
011500920723     C   22
011600920723     CAN 25              Z-ADD     KCU(X)        KFI               4 0
011700920723     C   23
011800920723     CAN 24              Z-ADD     KCU(X)        KAE               4 0
011900920723     C   23
012000920723     CAN 25              Z-ADD     KCU(X)        KAI               4 0
012100920723     C   26              Z-ADD     KCU(X)        KIVA              4 0
012200920723     C   27              Z-ADD     KCU(X)        KBNA              4 0
012300920723     C     END1          TAG
012400920723     C                   END
012500920723     C*---------------------------------------------------------------*
012600920731     C* ACCESSO   CNACO
012700920731     C     KACO          KLIST
012800920731     C                   KFLD                    CODUT
012900940725     C                   KFLD                    KCC               4 0
013000920731     C                   KFLD                    KSC               7 0
013100920731     C*
013200920723     C* CHIAVE X POSIZIONAMENTO TABELLE
013300920731     C     KTAB          KLIST
013400920731     C                   KFLD                    CODUT
013500920731     C                   KFLD                    COD               2
013600920731     C                   KFLD                    KEY               8
013700930610     C     KTAB2         KLIST
013800930610     C                   KFLD                    CODUT
013900930610     C                   KFLD                    COD
014000150520     C*---------------------------------------------------------------*
014100930322     C                   MOVEL     'TM'          COD
014200930611     C                   Z-ADD     13            LUNG             15 5
014300080624     c                   eval      vidtes2='**   '+%subst(vidflg:1:1)
014400080624     c                             +' '+%subst(vidflg:2:1)+'   **'
014500920204     C*
014600930208     C* 10 ON  - GESTIONE TABELLA "BB"
014700930208     C     VIDFLG        IFEQ      'BB'
014800930208     C                   SETON                                        10
014900930208     C                   END
015000951108     C* 11 ON  - GESTIONE TABELLA "BA"
015100951108     C     VIDFLG        IFEQ      'BA'
015200951108     C                   SETON                                        11
015300951108     C                   END
015400960826     C* 12 ON  - GESTIONE TABELLA "CA"
015500960826     C     VIDFLG        IFEQ      'CA'
015600960826     C                   SETON                                        12
015700960826     C                   END
015800010828     C* 13 ON  - GESTIONE TABELLA "BC"
015900010828     C     VIDFLG        IFEQ      'BC'
016000010828     C                   SETON                                        13
016100010828     C                   END
016200050525     C* 14 ON  - GESTIONE TABELLA "SC"
016300050525     C     VIDFLG        IFEQ      'SC'
016400050525     C                   SETON                                        14
016500050525     C                   END
016600060626     C* 15 ON  - GESTIONE TABELLA "TO"
016700060626     C     VIDFLG        IFEQ      'TO'
016800060626     C                   SETON                                        15
016900060626     C                   END
017000080624     C* 16 ON  - GESTIONE TABELLA "S2"
017100080624     C     VIDFLG        IFEQ      'S2'
017200080624     C                   SETON                                        16
017300080624     C                   END
017400080624     C* 17 ON  - GESTIONE TABELLA "TM"
017500080624     C     VIDFLG        IFEQ      'TM'
017600080624     C                   SETON                                        17
017700080624     C                   END
017800090617     C* 18 ON  - GESTIONE TABELLA "BP"
017900090617     C     VIDFLG        IFEQ      'BP'
018000090617     C                   SETON                                        18
018100090617     C                   END
018200150520     C* 19 ON  - GESTIONE TIC     "TP"
018300150520     C     VIDFLG        IFEQ      'TP'
018400150520     C                   SETON                                        19
018500150520     C                   END
018600930610     C*
018700930610     C                   MOVEL     *BLANKS       COMMAN
018800930610     C                   MOVEA     CM1(1)        COMMAN           80
018900930610     C                   CALL      'QCMDEXC'
019000930610     C                   PARM                    COMMAN
019100930610     C                   PARM                    LUNG
019200930610     C*
019300920204     C     STAR          TAG
019400920731     C                   MOVEL     *BLANKS       VIDLNP
019500920731     C                   Z-ADD     0             VIDCLI
019600940725     C                   Z-ADD     0             CODKSB
019700940725     C                   Z-ADD     0             GGPIU
019800940725     C                   Z-ADD     0             GGMEN
019900920731     C                   MOVEL     *BLANKS       DESCLI
020000920803     C                   MOVEL     *BLANKS       VIDTES
020100150520     C                   MOVEL     *BLANKS       VIDKSC            7 0
020200940406     C                   MOVEL     *ZEROS        PREKSC            7 0
020300150520     C                   MOVEL     'N'           VAUTVE
020400111220     C                   MOVEL     *BLANKS       vidforz
020500010830     C                   SETOFF                                       0102
020600150813     c                   if        parcli <> *zero and parcli <> *blank
020700150813     c                   movel     parcli        vidlnp
020800150813     c                   move      parcli        vidcli
020900150814     c                   endif
021000150814     c                   if        vidflg = *blank  or  vidcli =  0
021100150813     c                   exsr      gesnewsfl
021200150813     c                   endif
021300150813     C*
021400891030     C     INIZIO        TAG
021500920731     C                   EXFMT     TB37D01
021600920731     C*
021700080624     C** F3 - FINE LAVORO
021800920731     C   KC              GOTO      FINE
021900920731     C*
022000920731     C* CONTROLLI
022100920731     C                   EXSR      CTRFMT
022200920910     C   99              GOTO      INIZIO
022300150520     c
022400150520     c                   if        vidlnp<=*zeros and vidcli<=*zeros
022500150520     c                   GOTO      INIZIO
022600150520     c                   endif
022700920910     C*
022800150520     c
022900150520     c     for03         TAG
023000150520     C                   EXFMT     TB37D03
023100150520     c                   setoff                                       994142
023200150521     C   KC              GOTO      star
023300150520     c
023400150520     c                   exsr      contr
023500150520     c
023600150520     c                   if        *in99 or *in90 or (not *inkf and
023700150520     c                             not *inkq and not *inkb)
023800150520     c                   goto      for03
023900150520     c                   endif
024000150520     c*  F16 annullamento
024100150520     C                   if        *inkq and *in01
024200150520     C                   SETON                                        4199
024300150520     C                   MOVEL     ERR(1)        $MSG
024400150520     C                   GOTO      for03
024500150520     C                   ENDif
024600150520     C                   if        *inkq and *in02
024700150520     c                   exsr      Annull
024800920731     C                   GOTO      STAR
024900150520     C                   endif
025000920910     C*
025100150520     C* F6/F2 - AGGIORNAMENTO
025200920910     C                   EXSR      SCRIVI
025300891102     C                   GOTO      STAR
025400920731     C*
025500891030     C     FINE          TAG
025600980105     C                   CLEAR                   TIBS69
025700980105     C                   MOVEL     'C'           I69TLA
025800980105     C                   CALL      'TIBS69R'
025900980105     C                   PARM                    TIBS69
026000980105     C                   PARM                    CNACO
026100980105     C                   PARM                    CNIND
026200980105     C                   PARM                    CNCLP
026300980105     C                   PARM                    FNCLS
026400980105     C*
026500891030     C                   SETON                                        LR
026600920731     C*
026700930610     C**-------------------------------------------------------------**
026800930610     C** CONTROLLI  VIDEATA                                          **
026900930610     C**-------------------------------------------------------------**
027000920731     C     CTRFMT        BEGSR
027100940830     C                   SETOFF                                       9990
027200940830     C                   SETOFF                                       414243
027300940830     C                   SETOFF                                       4445
027400920910     C                   MOVEL     *BLANKS       KEY
027500920731     C*
027600920909     C*** LINEA DI PARTENZA
027700920909     C                   MOVEA     VIDLNP        SKI
027800920909     C     '?'           LOOKUP    SKI                                    20
027900930610     C* CARICO SUBFILE
028000930610     C   20              EXSR      CARSF
028100930610     C*
028200930610     C* C O N T R O L L I
028300920910     C     VIDLNP        IFNE      *BLANKS
028400920909     C                   TESTN                   VIDLNP               21
028500940725     C  N21              SETON                                        4199
028600940725     C  N21              MOVEL     ERR(4)        $MSG
028700150520     C  N21              GOTO      ENDCTR1
028800920910     C                   END
028900920909     C*
029000920731     C*** SE E' STATA DIGITATA UNA CHIAVE DI RICERCA E NON E' STATO
029100920731     C***    IMMESSO IL CODICE CLIENTE CALL PGM
029200920731     C     VIDCLI        IFEQ      0
029300920731     C     DESCLI        IFNE      *BLANKS
029400920731     C* PARSTA = 9  ESCLUDO ANNULLATI
029500920731     C                   Z-ADD     9             PARSTA
029600920731     C                   Z-ADD     0             ACOKSC
029700920910     C     VIDLNP        IFGT      *ZEROS
029800920731     C                   MOVEL     VIDLNP        ACOKSC
029900920910     C                   END
030000920910     C                   MOVEL     *BLANKS       DESCR            48
030100920910     C                   MOVEL     DESCLI        DESCR
030200920910     C                   MOVEL     KCI           CCC               4 0
030300920731     C                   CALL      'XALFA1'
030400920731     C                   PARM                    PARDUT
030500920731     C                   PARM                    CODUT
030600920910     C                   PARM                    DESCR
030700920910     C                   PARM                    CCC
030800920731     C                   PARM                    ACOKSC
030900920731     C                   PARM                    PARSTA            1 0
031000961017     C                   PARM                    PARFLR           90
031100920910     C     PARSTA        IFNE      -1
031200920731     C                   MOVE      ACOKSC        VIDCLI
031300920803     C                   MOVEL     ACOKSC        VIDLNP
031400920910     C                   MOVEL     DESCR         DESCLI
031500920909     C                   MOVEL     ACOKSC        VIDKSC
031600920910     C                   MOVEL     ACOKSC        KEY
031700920910     C                   END
031800920803     C*
031900920910     C                   SETON                                        90
032000150521     c                   if        parsta=-1
032100150521     c                   goto      endctr1
032200150521     c                   endif
032300150521     c
032400920731     C                   END
032500920731     C                   END
032600920731     C*
032700920910     C* LINEA DI PARTENZA OBBLIGATORIA
032800920910     C                   TESTN                   VIDLNP               21
032900940725     C  N21              SETON                                        4199
033000940725     C  N21              MOVEL     ERR(4)        $MSG
033100150520     C  N21              GOTO      ENDCTR1
033200920910     C*
033300920909     C* NON ACCETTARE CODICE CLIENTE = 0
033400920909     C     VIDLNP        IFEQ      *ZEROS
033500920909     C     VIDCLI        ANDEQ     *ZEROS
033600940725     C                   SETON                                        4199
033700940725     C                   MOVEL     ERR(2)        $MSG
033800150520     C                   GOTO      ENDCTR1
033900920909     C                   END
034000920909     C*
034100920731     C*** CODICE CLIENTE
034200920731     C                   MOVEL     *BLANKS       DESCLI
034300940725     C                   Z-ADD     KCI           KCC
034400940725     C                   MOVEL     VIDLNP        KSC
034500920731     C                   MOVE      VIDCLI        KSC
034600980105     C                   CLEAR                   TIBS69
034700980105     C                   CLEAR                   CNACO
034800980105     C                   CLEAR                   CNIND
034900980105     C                   CLEAR                   CNCLP
035000980105     C                   CLEAR                   FNCLS
035100980105     C                   MOVEL     ' '           I69TLA
035200980105     C                   MOVEL     KNSIF         I69SIF
035300980105     C                   Z-ADD     VIDCLI        I69KAC
035400980105     C                   MOVEL     VIDLNP        I69KAC
035500980105     C                   CALL      'TIBS69R'
035600980105     C                   PARM                    TIBS69
035700980105     C                   PARM                    CNACO
035800980105     C                   PARM                    CNIND
035900980105     C                   PARM                    CNCLP
036000980105     C                   PARM                    FNCLS
036100150520
036200080625  B3 C     O69ERR        IFNE      *BLANK
036300980105     C                   SETON                                        4199
036400080625     C                   MOVEL     err(4)        $MSG
036500150520     C                   GOTO      ENDCTR1
036600980105     C                   END
036700150520     c
036800920731     C* DECODIFICO CODICE CLIENTE
036900920731     C                   MOVEL     ACORAG        DESCLI
037000920731     C*
037100920731     C* NON ACCETTARE CLIENTI  8888  O  9999
037200920731     C     VIDCLI        IFEQ      8888
037300920731     C     VIDCLI        OREQ      9999
037400940725     C                   SETON                                        4199
037500940725     C                   MOVEL     ERR(4)        $MSG
037600150520     C                   GOTO      ENDCTR1
037700920731     C                   END
037800010830     C*
037900920910     C                   MOVEL     KSC           KEY
038000920910     C                   MOVEL     KSC           VIDKSC
038100010830     C*------>>>
038200010830     C*------>>>
038300951108     C*
038400080624    1C     VIDLNP        IFNE      *ZEROS
038500951108     C     VIDCLI        ANDNE     *ZEROS
038600951108     C*
038700150521     C                   SETOFF                                       010203
038800951108     C     KTAB          CHAIN     TABEL                              80
038900010830     C*
039000080624    2C     *IN80         IFEQ      '1'
039100951108     C                   MOVE      TES(1)        VIDTES
039200951108     C                   SETON                                        01
039300080625     c                   clear                   dstm
039400150520     c                   clear                   savfti            2
039500120629     c
039600080624   x2C                   ELSE
039700010830     C*
039800951108     C                   MOVEL     TBLUNI        DSTM
039900150520     c                   eval      savfti=§tmfti
040000050526     C*
040100080624    3c                   if        *in14 and §tmfsc='S'   or
040200080624     c                             *in15 and §tmfto='S'   or
040300080624     c                             *in16 and §tmfs2='S'   or
040400080624     c                             *in11 and §tmfba='S'   or
040500080624     c                             *in12 and §tmfca='S'   or
040600090617     c                             *in17 and §tmftm='S'   or
040700150520     c                             *in18 and §tmfbp='S'   or
040800150520     c                             *in19 and §tmftp='S'
040900080624     C                   MOVE      TES(2)        VIDTES
041000080624     C                   SETON                                        02
041100080624    3c                   endif
041200080624    3c                   if        *in14 and §tmfsc<>'S'   or
041300080624     c                             *in15 and §tmfto<>'S'   or
041400080624     c                             *in16 and §tmfs2<>'S'   or
041500080624     c                             *in11 and §tmfba<>'S'   or
041600080624     c                             *in12 and §tmfca<>'S'   or
041700090617     c                             *in17 and §tmftm<>'S'   or
041800150520     c                             *in18 and §tmfbp<>'S'   or
041900150520     c                             *in19 and §tmftp<>'S'
042000080624     C                   MOVE      TES(1)        VIDTES
042100080624     C                   SETON                                        01
042200080624    3c                   endif
042300080624     c
042400111220     c* campo per SC S2
042500120629    3c                   if        *in14 or *in16
042600150521     c                   seton                                        03
042700120629     c
042800120628     C                   MOVEL     *BLANKS       vidforz
042900120629    5c                   if        *in14 and §tmfsc='S'   or
043000120629     c                             *in16 and §tmfs2='S'
043100120629    6c                   if        §tmfti=*blanks
043200111220     c                   eval      vidforz ='N'
043300120629    6c                   endif
043400120629    5c                   endif
043500120629
043600120629    3c                   endif
043700120628     c
043800951108     C*
043900080624    3C   10§TMFBB        IFEQ      'S'
044000010830     C     §TMFBC        ANDEQ     *BLANK
044100951108     C                   MOVE      TES(2)        VIDTES
044200080624      *>>>>>>
044300080624    4C     VIDKSC        IFNE      PREKSC
044400951108     C                   MOVEL     §TMFVE        VAUTVE
044500951108     C                   MOVEL     §TMKSB        CODKSB
044600951108     C                   MOVEL     §TMGG1        GGPIU
044700951108     C                   MOVEL     §TMGG2        GGMEN
044800951108     C                   Z-ADD     VIDKSC        PREKSC
044900080624    4C                   ENDIF
045000150521     C                   SETON                                        0203
045100080624   x3C                   ELSE
045200951108     C                   MOVE      TES(1)        VIDTES
045300951108     C                   SETON                                        01
045400080624    3C                   END
045500010828     C*
045600080625    3C   13§TMFBC        IFEQ      'S'
045700010828     C                   MOVE      TES(2)        VIDTES
045800010829      *>>>>>>
045900010828     C     VIDKSC        IFNE      PREKSC
046000010828     C                   MOVEL     §TMFVE        VAUTVE
046100010828     C                   MOVEL     §TMKSB        CODKSB
046200010828     C                   MOVEL     §TMGG1        GGPIU
046300010828     C                   MOVEL     §TMGG2        GGMEN
046400010828     C                   Z-ADD     VIDKSC        PREKSC
046500010828     C                   ENDIF
046600150521     C                   SETON                                        0203
046700010828     C                   ELSE
046800010828     C                   MOVE      TES(1)        VIDTES
046900010828     C                   SETON                                        01
047000080624    3C                   END
047100010828     C*
047200080624    2C                   END
047300951108     C*
047400080624   x1C                   ELSE
047500951108     C                   MOVEL     *BLANKS       VIDTES
047600951108     C                   MOVEL     *ZEROS        VIDKSC
047700080624    1C                   END
047800150520     c     ENDCTR1       ENDSR
047900010830     C*
048000150520     C*-------------------------------------------------------------------------
048100150520     c     Contr         BEGSR
048200150520     C*------------------------------------------------------------------------
048300150520     C                   SETOFF                                       9990
048400150520     C                   SETOFF                                       414243
048500150520     C                   SETOFF                                       4445
048600150520     C* CODICE CLIENTE ANNULLATO
048700150520     C  NKQACOFLG        IFNE      ' '
048800150520     C                   SETON                                        4199
048900150520     C                   MOVEL     ERR(3)        $MSG
049000150520     C                   GOTO      ENDCTR
049100150520     c                   endif
049200150520     c
049300010830     C* CONTROLLI se il cliente era definito già come "BB"
049400010830     C*   E NON È STATO FORZATO CON F8
049500010830     C   13§TMFBB        IFEQ      'S'
049600010830     C     §TMFBC        ANDEQ     *BLANK
049700010830     C                   SETON                                        41  99
049800010830     C                   MOVEL     ERR(10)       $MSG
049900010830     C                   GOTO      ENDCTR
050000010830     C                   END
050100010830     C*
050200010830     C* CONTROLLI se il cliente era definito già come "BC"
050300010830     C*   E NON È STATO FORZATO CON F8
050400010830     C   10§TMFBC        IFEQ      'S'
050500010830     C                   SETON                                        41  99
050600010830     C                   MOVEL     ERR(11)       $MSG
050700010830     C                   GOTO      ENDCTR
050800010830     C                   END
050900080625     C* CONTROLLI se il cliente era definito già come "S2"
051000080625     C   14§TMFs2        IFEQ      'S'
051100080625     C                   SETON                                        41  99
051200080625     C                   MOVEL     ERR(12)       $MSG
051300080625     C                   GOTO      ENDCTR
051400080625     C                   END
051500080625     C* CONTROLLI se il cliente era definito già come "SC"
051600080625     C   16§TMFsc        IFEQ      'S'
051700080625     C                   SETON                                        41  99
051800080625     C                   MOVEL     ERR(13)       $MSG
051900080625     C                   GOTO      ENDCTR
052000080625     C                   END
052100010830     C*-------------------------------
052200010828      *
052300010828      * --  --  --  --  --  --  --  --  --  --  --
052400940830     C* SE CLIENTE "BB" CON AUTORIZZAZIONE AL VERSAMENTO
052500940830     C* BANCA OBBLIGATORIA
052600951108     C  N11*IN10         IFEQ      '1'
052700940830     C     VAUTVE        ANDEQ     *BLANKS
052800940830     C     CODKSB        ANDEQ     0
052900940830     C                   SETON                                        424399
053000940830     C                   MOVEL     ERR(7)        $MSG
053100940830     C                   GOTO      ENDCTR
053200940830     C                   END
053300010830      *
053400010829     C* SE CLIENTE "BC" CON AUTORIZZAZIONE AL VERSAMENTO
053500010829     C* BANCA OBBLIGATORIA
053600010830     C  N11*IN13         IFEQ      '1'
053700010830     C     VAUTVE        ANDEQ     *BLANKS
053800010830     C     CODKSB        ANDEQ     0
053900010830     C                   SETON                                        424399
054000010830     C                   MOVEL     ERR(9)        $MSG
054100010830     C                   GOTO      ENDCTR
054200010830     C                   END
054300010828      *
054400940830     C* SE immessi sia gg. in più che in meno segnalo errore
054500951108     C  N11*IN10         IFEQ      '1'
054600940830     C     GGPIU         ANDNE     0
054700940830     C     GGMEN         ANDNE     0
054800940830     C                   SETON                                        444599
054900940830     C                   MOVEL     ERR(8)        $MSG
055000940830     C                   GOTO      ENDCTR
055100940830     C                   END
055200010830      *
055300010829     C* SE immessi sia gg. in più che in meno segnalo errore
055400010830     C  N11*IN13         IFEQ      '1'
055500010830     C     GGPIU         ANDNE     0
055600010830     C     GGMEN         ANDNE     0
055700010830     C                   SETON                                        444599
055800010830     C                   MOVEL     ERR(8)        $MSG
055900010830     C                   GOTO      ENDCTR
056000010830     C                   END
056100010830     C*------>>>
056200010830     C*** CODICE BANCA SU CUI EFFETTUARE VERSAMENTO
056300010830     C                   Z-ADD     KBNA          KCC
056400010830     C                   Z-ADD     CODKSB        KSC
056500010830     C     CODKSB        IFNE      *ZEROS
056600010830     C     KACO          CHAIN     CNACO000                           30
056700010830     C     ACOFLG        IFNE      ' '
056800010830     C     *IN30         OREQ      '1'
056900010830     C* CODICE BANCA   ANNULLATO
057000010830     C                   SETON                                        4299
057100010830     C                   MOVEL     ERR(6)        $MSG
057200010830     C                   GOTO      ENDCTR
057300010830     C                   END
057400010830     C                   END
057500010830     C*------>>>
057600010830     C*
057700920731     C     ENDCTR        ENDSR
057800930610     C**-------------------------------------------------------------**
057900930610     C** CARICAMENTO DEL SUBFILE                                     **
058000930610     C**-------------------------------------------------------------**
058100930610     C     CARSF         BEGSR
058200930610     C                   Z-ADD     0             NRR               4 0
058300930610     C*
058400930610     C* PULIZIA SUBFILE
058500930610     C                   SETON                                        35
058600930610     C                   WRITE     TB37DCT
058700930610     C                   SETOFF                                       35
058800930610     C*
058900010830     C   10              MOVEL     'BC'          DESFLG
059000010830     C   13              MOVEL     'BB'          DESFLG
059100080625     C   14              MOVEL     'S2'          DESFLG
059200080625     C   16              MOVEL     'SC'          DESFLG
059300930610     C* CARICO SUBFILE
059400930610     C                   MOVEL     'TM'          COD
059500930611     C                   MOVEL     *BLANKS       KEY
059600930610     C     KTAB2         SETLL     TABEL
059700930610     C     KTAB2         READE     TABEL                                  30
059800930610     C*
059900930610     C     *IN30         DOWEQ     '0'
060000930617     C* ESCLUDO ANNULLATI
060100930617     C     TBLFLG        IFEQ      ' '
060200930610     C                   MOVEL     TBLUNI        DSTM
060300930610     C                   MOVEL     ' '           VI2SCE
060400010830     C                   MOVEL     ' '           VI2BBC
060500010830     C                   SETOFF                                       5556
060600930610     C*
060700150520     C* CARICO I CODICI CLIENTI CON TIPO INCASSO richiesto
060800080625     c                   if        *in15 and §tmfto='S'   or
060900080624     c                             *in11 and §tmfba='S'   or
061000080624     c                             *in12 and §tmfca='S'   or
061100090617     c                             *in17 and §tmftm='S'   or
061200150520     c                             *in18 and §tmfbp='S'   or
061300150520     c                             *in19 and §tmftp='S'
061400930610     C                   MOVEL     TBLKEY        VI2KSC
061500930610     C                   MOVEL     §TMRAG        VI2DES
061600930610     C*
061700150803     c                   if        nrr = 9999
061800150803     c                   leave
061900150803     C                   END
062000930610     C                   ADD       1             NRR
062100930610     C                   WRITE     TB37DSF
062200930610     C                   END
062300080625     c
062400080625     C* CARICO I CODICI CLIENTI CON TIPO INCASSO "SC"
062500150520     c                   if        *in14  and (§tmfsc='S' or §tmfs2='S')
062600080625     c                   if        §tmfsc='S'
062700080625     c                   seton                                        55
062800080625     c                   else
062900080625     c                   seton                                        56
063000080625     c                   movel     '*'           vi2bbc
063100080625     c                   endif
063200080625
063300080625     C                   MOVEL     TBLKEY        VI2KSC
063400080625     C                   MOVEL     §TMRAG        VI2DES
063500080625     C*
063600150803     c                   if        nrr = 9999
063700150803     c                   leave
063800150803     C                   END
063900080625     C                   ADD       1             NRR
064000080625     C                   WRITE     TB37DSF
064100080625     C                   END
064200080625     c*
064300080625     C* CARICO I CODICI CLIENTI CON TIPO INCASSO "S2"
064400080625     c                   if        *in16  and (§tmfsc='S' or §tmfs2='S')
064500080625     c                   if        §tmfs2='S'
064600080625     c                   seton                                        55
064700080625     c                   else
064800080625     c                   seton                                        56
064900080625     c                   movel     '*'           vi2bbc
065000080625     c                   endif
065100080625
065200080625     C                   MOVEL     TBLKEY        VI2KSC
065300080625     C                   MOVEL     §TMRAG        VI2DES
065400080625     C*
065500150803     c                   if        nrr = 9999
065600150803     c                   leave
065700150803     C                   END
065800080625     C                   ADD       1             NRR
065900080625     C                   WRITE     TB37DSF
066000080625     C                   END
066100080625     c
066200930610     C*
066300930610     C* CARICO I CODICI CLIENTI CON TIPO INCASSO "BB"
066400080624     C   10§TMFBB        IFEQ      'S'
066500080624     c
066600080625     C     §TMFBB        IFEQ      'S'
066700010830     C     §TMFBC        ANDEQ     *BLANK
066800010830     C                   SETON                                            55
066900010830     C                   ELSE
067000010830     C                   SETOFF                                           55
067100010830     C                   END
067200010830     C     §TMFBC        IFEQ      'S'
067300010830     C                   MOVEL     '*'           VI2BBC
067400010830     C                   ELSE
067500010830     C                   MOVEL     ' '           VI2BBC
067600010830     C                   END
067700010830     C     VI2BBC        COMP      '*'                                    56
067800010830     C                   MOVEL     TBLKEY        VI2KSC
067900930610     C                   MOVEL     §TMRAG        VI2DES
068000930610     C*
068100150803     c                   if        nrr = 9999
068200150803     c                   leave
068300150803     C                   END
068400930610     C                   ADD       1             NRR
068500930610     C                   WRITE     TB37DSF
068600930610     C                   END
068700010829     C*
068800010829     C* CARICO I CODICI CLIENTI CON TIPO INCASSO "BC"
068900080624     C   13§TMFBB        IFEQ      'S'
069000010830     C     §TMFBC        COMP      'S'                                    55
069100010830     C     §TMFBB        IFEQ      'S'
069200010830     C     §TMFBC        ANDEQ     *BLANK
069300010830     C                   MOVEL     '*'           VI2BBC
069400010830     C                   ELSE
069500010830     C                   MOVEL     ' '           VI2BBC
069600010830     C                   END
069700010830     C     VI2BBC        COMP      '*'                                    56
069800010829     C                   MOVEL     TBLKEY        VI2KSC
069900010829     C                   MOVEL     §TMRAG        VI2DES
070000010829     C*
070100150803     c                   if        nrr = 9999
070200150803     c                   leave
070300150803     C                   END
070400010829     C                   ADD       1             NRR
070500010829     C                   WRITE     TB37DSF
070600010829     C                   END
070700930610     C*
070800930617     C                   END
070900930610     C     KTAB2         READE     TABEL                                  30
071000930610     C                   END
071100930610     C*
071200930610     C* EMETTO SUBFILE
071300930610     C     NRR           IFGT      0
071400930610     C*
071500930610     C     SOPRA         TAG
071600930610     C                   Z-ADD     1             REC
071700930610     C                   WRITE     TB37D02
071800930610     C                   EXFMT     TB37DCT
071900930610     C*
072000930610     C* CMD12 - RITORNO
072100930610     C     *INKL         IFEQ      '0'
072200930610     C*
072300930610     C* CONTROLLO DATI SUBFILE
072400930610     C                   READC     TB37DSF                                31
072500930610     C   31              GOTO      SOPRA
072600930610     C  N31VI2SCE        IFEQ      'X'
072700930611     C                   MOVEL     VI2KSC        KEY
072800930610     C                   MOVEL     VI2KSC        VIDKSC
072900930610     C                   MOVEL     VI2KSC        VIDLNP
073000930610     C                   MOVE      VI2KSC        VIDCLI
073100930610     C                   MOVEL     *BLANKS       DESCLI
073200930610     C                   MOVEL     VI2DES        DESCLI
073300930610     C                   SETON                                        90
073400930610     C                   ELSE
073500930610     C                   GOTO      SOPRA
073600930610     C                   END
073700930610     C*
073800930610     C                   ELSE
073900930610     C                   MOVEL     *BLANKS       VIDLNP
074000930610     C                   MOVEL     *BLANKS       VIDCLI
074100930610     C                   MOVEL     *BLANKS       DESCLI
074200930610     C                   END
074300930610     C                   END
074400930610     C                   ENDSR
074500150813     C**-------------------------------------------------------------**
074600150813     C** Gestione nuovo  SUBFILE                                     **
074700150813     C**-------------------------------------------------------------**
074800150813     C     gesnewsfl     BEGSR
074900150813     c                   exsr      carsf2
075000150813     C                   ENDSR
075100150813     C**-------------------------------------------------------------**
075200150813     C** CARICAMENTO DEL SUBFILE                                     **
075300150813     C**-------------------------------------------------------------**
075400150813     C     carsf2        BEGSR
075500150813     C                   Z-ADD     0             NRR1              4 0
075600150813     C*
075700150813     C* PULIZIA SUBFILE
075800150813     C                   SETON                                        36
075900150813     C                   WRITE     TB37DCT1
076000150813     C                   SETOFF                                       36
076100150813     C* CARICO SUBFILE
076200150813     C                   MOVEL     'TM'          COD
076300150813     C                   MOVEL     *BLANKS       KEY
076400150813     C     KTAB2         SETLL     TABEL
076500150813     C     KTAB2         READE     TABEL                                  30
076600150813     C*
076700150813     C     *IN30         DOWEQ     '0'
076800150813     C* ESCLUDO ANNULLATI
076900150813     C     TBLFLG        IFEQ      ' '
077000150813     C                   MOVEL     TBLUNI        DSTM
077100150813     C                   MOVEL     ' '           VsSCE
077200150813     C                   SETOFF                                       5556
077300150813     C*
077400150813     C* CARICO I CODICI CLIENTI CON TIPO INCASSO richiesto
077500150813     C                   MOVEL     TBLKEY        VsKSC
077600150813     C                   MOVEL     §TMRAG        Vsdes
077700150813     C                   MOVEL     §tmfto        Vsto
077800150813     C                   MOVEL     §tmfba        Vsba
077900150813     C                   MOVEL     §tmfca        Vsca
078000150813     C                   MOVEL     §tmftm        Vstm
078100150813     C                   MOVEL     §tmfbp        Vsbc
078200150813     C                   MOVEL     §tmftp        Vssc
078300150813     C                   MOVEL     §TMfbb        Vsbb
078400150813     C                   MOVEL     §TMfs2        Vss2
078500150813     C*seconda riga
078600150813     C                   MOVEL     §TMFVE        VsFVE
078700150813     C                   MOVEL     §TMKSB        VsKSB
078800150813     C                   MOVEL     §TMgg1        Vsgg1
078900150813     C                   MOVEL     §TMgg2        Vsgg2
079000150813     C                   MOVEL     §TMFTI        VsFTI
079100150813      *ricerca data ultima bolla
079200150813     c                   clear                   vsdus
079300150813     c                   z-add     151           kcc
079400150813     c                   z-add     vsksc         ksc
079500150813     c     kaco          chain     cnclp00f
079600150813     c                   if        %found(cnclp00f) and
079700150813     c                             clpdus <> 0
079800150813     C                   MOVE      clpdus        datads
079900150813     C                   MOVE      aa            aa1
080000150813     C                   MOVE      mm            mm1
080100150813     C                   MOVE      gg            gg1
080200150813     C                   MOVE      datads1       vsdus
080300150813     c                   movel     20            vsdus
080400150813     c                   endif
080500150813     c                   if        nrr1= 9999
080600150813     c                   leave
080700150813     C                   END
080800150813     C                   ADD       1             NRR1
080900150813     C                   WRITE     TB37DSF1
081000150813     C                   END
081100150813     c
081200150813     C     KTAB2         READE     TABEL                                  30
081300150813     C                   END
081400150813     C*
081500150813     C* EMETTO SUBFILE
081600150813     C     NRR1          IFGT      0
081700150813     C*
081800150813     C     SOPRA2        TAG
081900150813     C                   Z-ADD     1             REC1
082000150813     C                   WRITE     TB37D021
082100150813     C                   EXFMT     TB37DCT1
082200150813     C*
082300150814     c   kf              leavesr
082400150813     C* CMD12 - RITORNO
082500150813     C     *INKL         IFEQ      '0'
082600150813     C*
082700150813     C* CONTROLLO DATI SUBFILE
082800150813     C                   READC     TB37DSF1                               31
082900150813     C   31              GOTO      SOPRA2
083000150813     C  N31Vssce         IFEQ      '2'
083100150813     C                   MOVEL     Vsksc         KEY
083200150813     C                   MOVEL     Vsksc         VIDKSC
083300150813     C                   MOVEL     Vsksc         VIDLNP
083400150813     C                   MOVE      Vsksc         VIDCLI
083500150813     C                   MOVEL     *BLANKS       DESCLI
083600150813     C                   MOVEL     Vsdes         DESCLI
083700150814      *aggiorna
083800150814     c     ktab          chain     tabel00f
083900150814     c                   if        %found(tabel00f)
084000150814     c                   setoff                                       80
084100150814     c                   exsr      scrivi
084200150814     c                   endif
084300150813     C                   SETON                                        90
084400150813     C                   ELSE
084500150813     C                   GOTO      SOPRA2
084600150813     C                   END
084700150814     C  N31Vssce         IFEQ      '4'
084800150814     C                   MOVEL     Vsksc         KEY
084900150814     C                   MOVEL     Vsksc         VIDKSC
085000150814     C                   MOVEL     Vsksc         VIDLNP
085100150814     C                   MOVE      Vsksc         VIDCLI
085200150814     C                   MOVEL     *BLANKS       DESCLI
085300150814     C                   MOVEL     Vsdes         DESCLI
085400150814      *aggiorna
085500150814     c     ktab          chain     tabel00f
085600150814     c                   if        %found(tabel00f)
085700150814     c                   move      '*'           tblflg
085800150814     c                   update    tabel
085900150814     c                   endif
086000150814     C                   SETON                                        90
086100150814     C                   ELSE
086200150814     C                   GOTO      SOPRA2
086300150814     C                   END
086400150813     C*
086500150813     C                   ELSE
086600150813     C                   MOVEL     *BLANKS       VIDLNP
086700150813     C                   MOVEL     *BLANKS       VIDCLI
086800150813     C                   MOVEL     *BLANKS       DESCLI
086900150813     C                   END
087000150813     C                   END
087100150813     C                   ENDSR
087200930610     C**-------------------------------------------------------------**
087300930610     C** AGGIORNAMENTO / INSERIMENTO                                 **
087400930610     C**-------------------------------------------------------------**
087500891030     C     SCRIVI        BEGSR
087600920731     C                   MOVEL     CODUT         TBLKUT
087700930322     C                   MOVEL     'TM'          TBLCOD
087800920902     C                   MOVEL     VIDLNP        ALF7              7
087900920902     C                   MOVE      VIDCLI        ALF7              7
088000920902     C                   MOVEL     *BLANKS       TBLKEY
088100920902     C                   MOVEL     ALF7          TBLKEY
088200930322     C                   MOVEL     DESCLI        §TMRAG
088300930324     C                   MOVEL     ' '           TBLFLG
088400150520     c                   eval      §tmfti=savfti
088500940725     C     *IN10         IFEQ      '0'
088600010829     C     *IN13         ANDEQ     '0'
088700010829      *
088800080624     c                   select
088900080624     C     *IN11         wheneq    '1'
089000951108     C                   MOVEL     'S'           §TMFBA
089100080624     C     *IN12         wheneq    '1'
089200960826     C                   MOVEL     'S'           §TMFCA
089300080624     C     *IN14         wheneq    '1'
089400050525     C                   MOVEL     'S'           §TMFsc
089500050525     C                   MOVEL     'SC'          §TMFTI
089600111220     c                   if        vidforz ='N'
089700111220     c                   clear                   §TMFTI
089800111220     c                   endif
089900111220     c
090000080624     C     *IN15         wheneq    '1'
090100060626     C                   MOVEL     'S'           §TMFTO
090200080624     C     *IN16         wheneq    '1'
090300080624     C                   MOVEL     'S'           §TMFs2
090400080624     C                   MOVEL     'S2'          §TMFti
090500111220     c                   if        vidforz ='N'
090600111220     c                   clear                   §TMFTI
090700111220     c                   endif
090800090617     C     *IN18         wheneq    '1'
090900090617     C                   MOVEL     'S'           §TMFbp
091000150520     C     *IN19         wheneq    '1'
091100150520     C                   MOVEL     'S'           §TMFTp
091200080624     C                   other
091300951108     C                   MOVEL     'S'           §TMFTM
091400080624     C                   ENDSL
091500010829      *
091600951114     C     §TMFBB        IFNE      'S'
091700940725     C                   MOVEL     *ZEROS        §TMKSB
091800940725     C                   MOVEL     *ZEROS        §TMGG1
091900940725     C                   MOVEL     *ZEROS        §TMGG2
092000951114     C                   END
092100010829      *
092200940725     C                   ELSE
092300010829      *
092400010830     C     *INKB         IFEQ      *OFF
092500010829     C   13              MOVEL     'S'           §TMFBC
092600010830     C                   MOVEL     'S'           §TMFBB
092700010830     C                   ELSE
092800010830      * CON F2 FORZATURA:
092900010830      *   SI VUOLE INVERTIRE DA "BB" A "BC" O VICEVERSA
093000010830     C   13              MOVEL     ' '           §TMFBC
093100010830     C   10              MOVEL     'S'           §TMFBC
093200010830     C                   MOVEL     'S'           §TMFBB
093300010830     C                   END
093400010830      *
093500010830     C                   MOVEL     VAUTVE        §TMFVE
093600010830     C                   MOVEL     CODKSB        §TMKSB
093700010830     C                   MOVEL     GGPIU         §TMGG1
093800010830     C                   MOVEL     GGMEN         §TMGG2
093900010829      *
094000940725     C                   END
094100010829      *
094200930322     C                   MOVEL     DSTM          TBLUNI
094300920731     C*
094400920731     C                   MOVEL     '1'           TBLFTT
094500920731     C                   MOVEL     *ZEROS        TBLFLT
094600920731     C                   MOVEL     ' '           TBLFTR
094700920731     C                   Z-ADD     0             TBLDTR
094800920731     C*
094900891102     C   80              WRITE     TABEL                                98
095000891102     C  N80              UPDATE    TABEL                                98
095100920731     C*
095200891030     C                   ENDSR
095300920803     C*---------------------------------------------------------------*
095400150520     c* Annullamento tipo incasso
095500150520     C*---------------------------------------------------------------*
095600150520     c     Annull        BEGSR
095700150520     c
095800150520     C     *IN10         IFEQ      '0'
095900150520     C     *IN13         ANDEQ     '0'
096000150520     C
096100150520     C   17              MOVEL     ' '           §TMFTM
096200150520     C   18              MOVEL     ' '           §TMFBP
096300150520     C   19              MOVEL     ' '           §TMFTP
096400150520     C
096500150520     C   11              MOVEL     ' '           §TMFBA
096600150520     C
096700150520     C   12              MOVEL     ' '           §TMFCA
096800150520     c*
096900150520     C   14              MOVEL     *BLANKS       §TMFsc
097000150520     C   14              MOVEL     *BLANKS       §TMFTI
097100150520     C   15              MOVEL     *BLANKS       §TMFto
097200150520     C   16              MOVEL     *BLANKS       §TMFs2
097300150520     C   16              MOVEL     *BLANKS       §TMFTI
097400150520     C                   ENDif
097500150520     C*
097600150520     C     *IN10         IFEQ      *ON
097700150520     C     *IN13         OREQ      *ON
097800150520     C                   MOVEL     ' '           §TMFBB
097900150520     C                   MOVEL     ' '           §TMFBC
098000150520     C                   MOVEL     ' '           §TMFVE
098100150520     C                   MOVEL     *ZEROS        §TMKSB
098200150520     C                   MOVEL     *ZEROS        §TMGG1
098300150520     C                   MOVEL     *ZEROS        §TMGG2
098400150520     C                   END
098500150520      *
098600150520     C     §TMFTM        IFNE      'S'
098700150520     C     §TMFBB        ANDNE     'S'
098800150520     C     §TMFBC        ANDNE     'S'
098900150520     C     §TMFBA        ANDNE     'S'
099000150520     C     §TMFCA        ANDNE     'S'
099100150520     C     §TMFsc        ANDNE     'S'
099200150520     C     §TMFTO        ANDNE     'S'
099300150520     C     §TMFs2        ANDNE     'S'
099400150520     C     §TMFBP        ANDNE     'S'
099500150520     C     §TMFTP        ANDNE     'S'
099600150520     C                   MOVEL     '*'           TBLFLG
099700150520     C                   END
099800150520     C*
099900150520     C                   MOVEL     DSTM          TBLUNI
100000150520     C                   MOVEL     ' '           TBLFTR
100100150520     C                   Z-ADD     0             TBLDTR
100200150520     C*
100300150520     C                   UPDATE    TABEL
100400150520     c                   ENDSR
100500010830**   TES
100600150520 CODICE  IN  I M M i S S I O N E
100700150520 CODICE  IN  A G G I O R N A M E N T O
100800930610**   CM1
100900010830DLYJOB DLY(1)
101000940725**   ERR
101100940725Tasto di funzione non ammesso: codice in inserimento                  01
101200940725Codice cliente obbligatorio                                           02
101300940725Codice cliente annullato                                              03
101400940725Codice cliente errato                                                 04
101500940725Codice cliente bloccato                                               05
101600940725Codice banca inesistente o annullato                                  06
101700010829Banca obbligatoria se cliente "BB" con autorizzazione al versamento   07
101800940830Impossibile inserire entrambi: immettere GG. in più o GG. in meno     08
101900010829Banca obbligatoria se cliente "BC" con autorizzazione al versamento   09
102000010830Il Cliente è già definito come "BB"                                   10
102100010830Il Cliente è già definito come "BC"                                   11
102200080625Il Cliente è già definito come "S2"                                   12
102300080625Il Cliente è già definito come "SC"                                   13
