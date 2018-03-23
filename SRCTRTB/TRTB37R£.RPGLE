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
001800891030     FTABEL00F  UF A E           K DISK
001900980105     FCNACO00F  IF   E           K DISK
002000920723     D SKI             S              1    DIM(3)
002100150520     D TES             S             38    DIM(2) CTDATA PERRCD(1)
002200930611     D CM1             S             13    DIM(1) CTDATA PERRCD(1)
002300080625     D ERR             S             70    DIM(13) CTDATA PERRCD(1)
002400930208     D PARAM           DS
002500150901     D  VIDFLG                        2
002600150901     D  parcli                        7
002700150901     D  paropz                        1
002800920723     D TCUDS           DS
002900920723     D  F1                     1      1
003000920723     D  F3                     3      3
003100920723     D  F2                     2      2
003200920723     D  F4                     4      4
003300920723     D  F56                    5      6
003400930322     D DSTM          E DS
003500930322     D KPJBA         E DS
003600920723     D  LIBSYS                92    100
003700980105     D TIBS69        E DS                  EXTNAME(TIBS69DS)
003800980105     D CNACO         E DS                  EXTNAME(CNACO00F)
003900980105     D CNIND         E DS                  EXTNAME(CNIND00F)
004000980105     D CNCLP         E DS                  EXTNAME(CNCLP00F)
004100980105     D FNCLS         E DS                  EXTNAME(FNCLS00F)
004200971210     D TRUL12        E DS                  EXTNAME(TRUL12DS)
004300920723     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
004400920723     D  TCU                  398    697
004500920723     D                                     DIM(50)
004600920723     D  KCU                  698    847P 0
004700920723     D                                     DIM(50)
004800920723     D                                     PACKEVEN
004900920723     D  DTU                  848    895P 0
005000920723     D                                     DIM(12)
005100920723     D                                     PACKEVEN
005200920723     I/SPACE 3
005300920731     C*****************************************************************
005400920731     C*  RIEPILOGO INDICATORI
005500920731     C*****************************************************************
005600920731     C* 01    - INSERIMENTO
005700920731     C* 02    - VARIAZIONE
005800930208     C* 10    - ON   GESTIONE TABELLA "BB"
005900951108     C* 11    - ON   GESTIONE TABELLA "BA"
006000080624     C* 10/11 - OFF  GESTIONE TABELLA "TM"   anche 17 on
006100080624     C* 12    - ON   GESTIONE TABELLA "CA"
006200080624     C* 13    - ON   GESTIONE TABELLA "BC"
006300050525     C* 14    - ON   GESTIONE TABELLA "SC"
006400060626     C* 15    - ON   GESTIONE TABELLA "TO"
006500080624     C* 16    - ON   GESTIONE TABELLA "S2"
006600080624     C* 17    - ON   GESTIONE TABELLA "TM"
006700090617     C* 18    - ON   GESTIONE TABELLA "BP"
006800150520     C* 19    - ON   GESTIONE TABELLA "TP"
006900920910     C* 20/21 - DI COMODO
007000930610     C* 35    - PULIZIA SUBFILE
007100920731     C* 80/98 - DI COMODO,RIUTILIZZABILI
007200920731     C* 99    - ERRORE GENERICO
007300920731     C*****************************************************************
007400920731     C*
007500891030     C     *ENTRY        PLIST
007600891030     C                   PARM                    KPJBA
007700891030     C                   Z-ADD     1             CODUT
007800920723     C                   CALL      'X§PARUT'
007900920723     C                   PARM                    UT§DSE
008000891030     C                   MOVEL     RAGUT         RSUT
008100920731     C                   MOVEL     RAGUT         PARDUT           30
008200920723     C*
008300930208     C                   MOVEL     KPJBU         PARAM
008400920723     C*--- RICERCA CAPOCONTI------------------------------------------*
008500920723     C                   DO        50            X                 2 0
008600920723     C                   MOVE      TCU(X)        TCUDS
008700920723     C     F56           CABNE     'CG'          END1
008800920723     C     F4            COMP      '1'                                    21
008900920723     C     F4            COMP      '2'                                    22
009000920723     C     F4            COMP      '3'                                    23
009100920723     C     F4            COMP      '6'                                    27
009200920723     C** 1 CLIENTI   21
009300920723     C** 2 FORNITORI 22
009400920723     C** 3 AGENTI    23
009500920723     C     F3            COMP      '0'                                242425
009600920723     C     F3            COMP      'I'                                    26
009700920723     C** 0 ITALIA   25
009800920723     C** 1 ESTERO   24
009900920723     ** I CAPO CONTO IVA
010000920723     C   21
010100920723     CAN 24              Z-ADD     KCU(X)        KCE               4 0
010200920723     C   21
010300920723     CAN 25              Z-ADD     KCU(X)        KCI               4 0
010400920723     C   22
010500920723     CAN 24              Z-ADD     KCU(X)        KFE               4 0
010600920723     C   22
010700920723     CAN 25              Z-ADD     KCU(X)        KFI               4 0
010800920723     C   23
010900920723     CAN 24              Z-ADD     KCU(X)        KAE               4 0
011000920723     C   23
011100920723     CAN 25              Z-ADD     KCU(X)        KAI               4 0
011200920723     C   26              Z-ADD     KCU(X)        KIVA              4 0
011300920723     C   27              Z-ADD     KCU(X)        KBNA              4 0
011400920723     C     END1          TAG
011500920723     C                   END
011600920723     C*---------------------------------------------------------------*
011700920731     C* ACCESSO   CNACO
011800920731     C     KACO          KLIST
011900920731     C                   KFLD                    CODUT
012000940725     C                   KFLD                    KCC               4 0
012100920731     C                   KFLD                    KSC               7 0
012200920731     C*
012300920723     C* CHIAVE X POSIZIONAMENTO TABELLE
012400920731     C     KTAB          KLIST
012500920731     C                   KFLD                    CODUT
012600920731     C                   KFLD                    COD               2
012700920731     C                   KFLD                    KEY               8
012800930610     C     KTAB2         KLIST
012900930610     C                   KFLD                    CODUT
013000930610     C                   KFLD                    COD
013100150520     C*---------------------------------------------------------------*
013200930322     C                   MOVEL     'TM'          COD
013300930611     C                   Z-ADD     13            LUNG             15 5
013400080624     c                   eval      vidtes2='**   '+%subst(vidflg:1:1)
013500080624     c                             +' '+%subst(vidflg:2:1)+'   **'
013600920204     C*
013700930208     C* 10 ON  - GESTIONE TABELLA "BB"
013800930208     C     VIDFLG        IFEQ      'BB'
013900930208     C                   SETON                                        10
014000930208     C                   END
014100951108     C* 11 ON  - GESTIONE TABELLA "BA"
014200951108     C     VIDFLG        IFEQ      'BA'
014300951108     C                   SETON                                        11
014400951108     C                   END
014500960826     C* 12 ON  - GESTIONE TABELLA "CA"
014600960826     C     VIDFLG        IFEQ      'CA'
014700960826     C                   SETON                                        12
014800960826     C                   END
014900010828     C* 13 ON  - GESTIONE TABELLA "BC"
015000010828     C     VIDFLG        IFEQ      'BC'
015100010828     C                   SETON                                        13
015200010828     C                   END
015300050525     C* 14 ON  - GESTIONE TABELLA "SC"
015400050525     C     VIDFLG        IFEQ      'SC'
015500050525     C                   SETON                                        14
015600050525     C                   END
015700060626     C* 15 ON  - GESTIONE TABELLA "TO"
015800060626     C     VIDFLG        IFEQ      'TO'
015900060626     C                   SETON                                        15
016000060626     C                   END
016100080624     C* 16 ON  - GESTIONE TABELLA "S2"
016200080624     C     VIDFLG        IFEQ      'S2'
016300080624     C                   SETON                                        16
016400080624     C                   END
016500080624     C* 17 ON  - GESTIONE TABELLA "TM"
016600080624     C     VIDFLG        IFEQ      'TM'
016700080624     C                   SETON                                        17
016800080624     C                   END
016900090617     C* 18 ON  - GESTIONE TABELLA "BP"
017000090617     C     VIDFLG        IFEQ      'BP'
017100090617     C                   SETON                                        18
017200090617     C                   END
017300150520     C* 19 ON  - GESTIONE TIC     "TP"
017400150520     C     VIDFLG        IFEQ      'TP'
017500150520     C                   SETON                                        19
017600150520     C                   END
017700930610     C*
017800930610     C                   MOVEL     *BLANKS       COMMAN
017900930610     C                   MOVEA     CM1(1)        COMMAN           80
018000930610     C                   CALL      'QCMDEXC'
018100930610     C                   PARM                    COMMAN
018200930610     C                   PARM                    LUNG
018300930610     C*
018400920204     C     STAR          TAG
018500150901     C                   MOVEL     parcli        VIDLNP
018600150901     C                   move      parcli        VIDCLI
018700940725     C                   Z-ADD     0             CODKSB
018800940725     C                   Z-ADD     0             GGPIU
018900940725     C                   Z-ADD     0             GGMEN
019000920731     C                   MOVEL     *BLANKS       DESCLI
019100920803     C                   MOVEL     *BLANKS       VIDTES
019200150902     C                   MOVEL     *blank        VIDKSC            7 0
019300150902     C                   MOVEL     *zero         PREKSC            7 0
019400150520     C                   MOVEL     'N'           VAUTVE
019500111220     C                   MOVEL     *BLANKS       vidforz
019600010830     C                   SETOFF                                       0102
019700920204     C*
019800891030     C     INIZIO        TAG
019900150901     c                   if        vidcli <=*zeros
020000920731     C                   EXFMT     TB37D01
020100150901     c                   endif
020200920731     C*
020300080624     C** F3 - FINE LAVORO
020400920731     C   KC              GOTO      FINE
020500920731     C*
020600920731     C* CONTROLLI
020700920731     C                   EXSR      CTRFMT
020800920910     C   99              GOTO      INIZIO
020900150520     c
021000150520     c                   if        vidlnp<=*zeros and vidcli<=*zeros
021100150520     c                   GOTO      INIZIO
021200150520     c                   endif
021300920910     C*
021400150520     c
021500150520     c     for03         TAG
021600150520     C                   EXFMT     TB37D03
021700150520     c                   setoff                                       994142
021800150521     C   KC              GOTO      star
021900150520     c
022000150520     c                   exsr      contr
022100150520     c
022200150520     c                   if        *in99 or *in90 or (not *inkf and
022300150520     c                             not *inkq and not *inkb)
022400150520     c                   goto      for03
022500150520     c                   endif
022600150520     c*  F16 annullamento
022700150520     C                   if        *inkq and *in01
022800150520     C                   SETON                                        4199
022900150520     C                   MOVEL     ERR(1)        $MSG
023000150520     C                   GOTO      for03
023100150520     C                   ENDif
023200150520     C                   if        *inkq and *in02
023300150520     c                   exsr      Annull
023400920731     C                   GOTO      STAR
023500150520     C                   endif
023600920910     C*
023700150520     C* F6/F2 - AGGIORNAMENTO
023800920910     C                   EXSR      SCRIVI
023900891102     C                   GOTO      STAR
024000920731     C*
024100891030     C     FINE          TAG
024200980105     C                   CLEAR                   TIBS69
024300980105     C                   MOVEL     'C'           I69TLA
024400980105     C                   CALL      'TIBS69R'
024500980105     C                   PARM                    TIBS69
024600980105     C                   PARM                    CNACO
024700980105     C                   PARM                    CNIND
024800980105     C                   PARM                    CNCLP
024900980105     C                   PARM                    FNCLS
025000980105     C*
025100891030     C                   SETON                                        LR
025200920731     C*
025300930610     C**-------------------------------------------------------------**
025400930610     C** CONTROLLI  VIDEATA                                          **
025500930610     C**-------------------------------------------------------------**
025600920731     C     CTRFMT        BEGSR
025700940830     C                   SETOFF                                       9990
025800940830     C                   SETOFF                                       414243
025900940830     C                   SETOFF                                       4445
026000920910     C                   MOVEL     *BLANKS       KEY
026100920731     C*
026200920909     C*** LINEA DI PARTENZA
026300920909     C                   MOVEA     VIDLNP        SKI
026400920909     C     '?'           LOOKUP    SKI                                    20
026500930610     C* CARICO SUBFILE
026600930610     C   20              EXSR      CARSF
026700930610     C*
026800930610     C* C O N T R O L L I
026900920910     C     VIDLNP        IFNE      *BLANKS
027000920909     C                   TESTN                   VIDLNP               21
027100940725     C  N21              SETON                                        4199
027200940725     C  N21              MOVEL     ERR(4)        $MSG
027300150520     C  N21              GOTO      ENDCTR1
027400920910     C                   END
027500920909     C*
027600920731     C*** SE E' STATA DIGITATA UNA CHIAVE DI RICERCA E NON E' STATO
027700920731     C***    IMMESSO IL CODICE CLIENTE CALL PGM
027800920731     C     VIDCLI        IFEQ      0
027900920731     C     DESCLI        IFNE      *BLANKS
028000920731     C* PARSTA = 9  ESCLUDO ANNULLATI
028100920731     C                   Z-ADD     9             PARSTA
028200920731     C                   Z-ADD     0             ACOKSC
028300920910     C     VIDLNP        IFGT      *ZEROS
028400920731     C                   MOVEL     VIDLNP        ACOKSC
028500920910     C                   END
028600920910     C                   MOVEL     *BLANKS       DESCR            48
028700920910     C                   MOVEL     DESCLI        DESCR
028800920910     C                   MOVEL     KCI           CCC               4 0
028900920731     C                   CALL      'XALFA1'
029000920731     C                   PARM                    PARDUT
029100920731     C                   PARM                    CODUT
029200920910     C                   PARM                    DESCR
029300920910     C                   PARM                    CCC
029400920731     C                   PARM                    ACOKSC
029500920731     C                   PARM                    PARSTA            1 0
029600961017     C                   PARM                    PARFLR           90
029700920910     C     PARSTA        IFNE      -1
029800920731     C                   MOVE      ACOKSC        VIDCLI
029900920803     C                   MOVEL     ACOKSC        VIDLNP
030000920910     C                   MOVEL     DESCR         DESCLI
030100920909     C                   MOVEL     ACOKSC        VIDKSC
030200920910     C                   MOVEL     ACOKSC        KEY
030300920910     C                   END
030400920803     C*
030500920910     C                   SETON                                        90
030600150521     c                   if        parsta=-1
030700150521     c                   goto      endctr1
030800150521     c                   endif
030900150521     c
031000920731     C                   END
031100920731     C                   END
031200920731     C*
031300920910     C* LINEA DI PARTENZA OBBLIGATORIA
031400920910     C                   TESTN                   VIDLNP               21
031500940725     C  N21              SETON                                        4199
031600940725     C  N21              MOVEL     ERR(4)        $MSG
031700150520     C  N21              GOTO      ENDCTR1
031800920910     C*
031900920909     C* NON ACCETTARE CODICE CLIENTE = 0
032000920909     C     VIDLNP        IFEQ      *ZEROS
032100920909     C     VIDCLI        ANDEQ     *ZEROS
032200940725     C                   SETON                                        4199
032300940725     C                   MOVEL     ERR(2)        $MSG
032400150520     C                   GOTO      ENDCTR1
032500920909     C                   END
032600920909     C*
032700920731     C*** CODICE CLIENTE
032800920731     C                   MOVEL     *BLANKS       DESCLI
032900940725     C                   Z-ADD     KCI           KCC
033000940725     C                   MOVEL     VIDLNP        KSC
033100920731     C                   MOVE      VIDCLI        KSC
033200980105     C                   CLEAR                   TIBS69
033300980105     C                   CLEAR                   CNACO
033400980105     C                   CLEAR                   CNIND
033500980105     C                   CLEAR                   CNCLP
033600980105     C                   CLEAR                   FNCLS
033700980105     C                   MOVEL     ' '           I69TLA
033800980105     C                   MOVEL     KNSIF         I69SIF
033900980105     C                   Z-ADD     VIDCLI        I69KAC
034000980105     C                   MOVEL     VIDLNP        I69KAC
034100980105     C                   CALL      'TIBS69R'
034200980105     C                   PARM                    TIBS69
034300980105     C                   PARM                    CNACO
034400980105     C                   PARM                    CNIND
034500980105     C                   PARM                    CNCLP
034600980105     C                   PARM                    FNCLS
034700150520
034800080625  B3 C     O69ERR        IFNE      *BLANK
034900980105     C                   SETON                                        4199
035000080625     C                   MOVEL     err(4)        $MSG
035100150520     C                   GOTO      ENDCTR1
035200980105     C                   END
035300150520     c
035400920731     C* DECODIFICO CODICE CLIENTE
035500920731     C                   MOVEL     ACORAG        DESCLI
035600920731     C*
035700920731     C* NON ACCETTARE CLIENTI  8888  O  9999
035800920731     C     VIDCLI        IFEQ      8888
035900920731     C     VIDCLI        OREQ      9999
036000940725     C                   SETON                                        4199
036100940725     C                   MOVEL     ERR(4)        $MSG
036200150520     C                   GOTO      ENDCTR1
036300920731     C                   END
036400010830     C*
036500920910     C                   MOVEL     KSC           KEY
036600920910     C                   MOVEL     KSC           VIDKSC
036700010830     C*------>>>
036800010830     C*------>>>
036900951108     C*
037000080624    1C     VIDLNP        IFNE      *ZEROS
037100951108     C     VIDCLI        ANDNE     *ZEROS
037200951108     C*
037300150521     C                   SETOFF                                       010203
037400951108     C     KTAB          CHAIN     TABEL                              80
037500010830     C*
037600080624    2C     *IN80         IFEQ      '1'
037700951108     C                   MOVE      TES(1)        VIDTES
037800951108     C                   SETON                                        01
037900080625     c                   clear                   dstm
038000150520     c                   clear                   savfti            2
038100120629     c
038200080624   x2C                   ELSE
038300010830     C*
038400951108     C                   MOVEL     TBLUNI        DSTM
038500150520     c                   eval      savfti=§tmfti
038600050526     C*
038700080624    3c                   if        *in14 and §tmfsc='S'   or
038800080624     c                             *in15 and §tmfto='S'   or
038900080624     c                             *in16 and §tmfs2='S'   or
039000080624     c                             *in11 and §tmfba='S'   or
039100080624     c                             *in12 and §tmfca='S'   or
039200090617     c                             *in17 and §tmftm='S'   or
039300150520     c                             *in18 and §tmfbp='S'   or
039400150520     c                             *in19 and §tmftp='S'
039500080624     C                   MOVE      TES(2)        VIDTES
039600080624     C                   SETON                                        02
039700080624    3c                   endif
039800080624    3c                   if        *in14 and §tmfsc<>'S'   or
039900080624     c                             *in15 and §tmfto<>'S'   or
040000080624     c                             *in16 and §tmfs2<>'S'   or
040100080624     c                             *in11 and §tmfba<>'S'   or
040200080624     c                             *in12 and §tmfca<>'S'   or
040300090617     c                             *in17 and §tmftm<>'S'   or
040400150520     c                             *in18 and §tmfbp<>'S'   or
040500150520     c                             *in19 and §tmftp<>'S'
040600080624     C                   MOVE      TES(1)        VIDTES
040700080624     C                   SETON                                        01
040800080624    3c                   endif
040900080624     c
041000111220     c* campo per SC S2
041100120629    3c                   if        *in14 or *in16
041200150521     c                   seton                                        03
041300120629     c
041400120628     C                   MOVEL     *BLANKS       vidforz
041500120629    5c                   if        *in14 and §tmfsc='S'   or
041600120629     c                             *in16 and §tmfs2='S'
041700120629    6c                   if        §tmfti=*blanks
041800111220     c                   eval      vidforz ='N'
041900120629    6c                   endif
042000120629    5c                   endif
042100120629
042200120629    3c                   endif
042300120628     c
042400951108     C*
042500080624    3C   10§TMFBB        IFEQ      'S'
042600010830     C     §TMFBC        ANDEQ     *BLANK
042700951108     C                   MOVE      TES(2)        VIDTES
042800080624      *>>>>>>
042900080624    4C     VIDKSC        IFNE      PREKSC
043000951108     C                   MOVEL     §TMFVE        VAUTVE
043100951108     C                   MOVEL     §TMKSB        CODKSB
043200951108     C                   MOVEL     §TMGG1        GGPIU
043300951108     C                   MOVEL     §TMGG2        GGMEN
043400951108     C                   Z-ADD     VIDKSC        PREKSC
043500080624    4C                   ENDIF
043600150521     C                   SETON                                        0203
043700080624   x3C                   ELSE
043800951108     C                   MOVE      TES(1)        VIDTES
043900951108     C                   SETON                                        01
044000080624    3C                   END
044100010828     C*
044200080625    3C   13§TMFBC        IFEQ      'S'
044300010828     C                   MOVE      TES(2)        VIDTES
044400010829      *>>>>>>
044500010828     C     VIDKSC        IFNE      PREKSC
044600010828     C                   MOVEL     §TMFVE        VAUTVE
044700010828     C                   MOVEL     §TMKSB        CODKSB
044800010828     C                   MOVEL     §TMGG1        GGPIU
044900010828     C                   MOVEL     §TMGG2        GGMEN
045000010828     C                   Z-ADD     VIDKSC        PREKSC
045100010828     C                   ENDIF
045200150521     C                   SETON                                        0203
045300010828     C                   ELSE
045400010828     C                   MOVE      TES(1)        VIDTES
045500010828     C                   SETON                                        01
045600080624    3C                   END
045700010828     C*
045800080624    2C                   END
045900951108     C*
046000080624   x1C                   ELSE
046100951108     C                   MOVEL     *BLANKS       VIDTES
046200951108     C                   MOVEL     *ZEROS        VIDKSC
046300080624    1C                   END
046400150520     c     ENDCTR1       ENDSR
046500010830     C*
046600150520     C*-------------------------------------------------------------------------
046700150520     c     Contr         BEGSR
046800150520     C*------------------------------------------------------------------------
046900150520     C                   SETOFF                                       9990
047000150520     C                   SETOFF                                       414243
047100150520     C                   SETOFF                                       4445
047200150520     C* CODICE CLIENTE ANNULLATO
047300150520     C  NKQACOFLG        IFNE      ' '
047400150520     C                   SETON                                        4199
047500150520     C                   MOVEL     ERR(3)        $MSG
047600150520     C                   GOTO      ENDCTR
047700150520     c                   endif
047800150520     c
047900010830     C* CONTROLLI se il cliente era definito già come "BB"
048000010830     C*   E NON È STATO FORZATO CON F8
048100010830     C   13§TMFBB        IFEQ      'S'
048200010830     C     §TMFBC        ANDEQ     *BLANK
048300010830     C                   SETON                                        41  99
048400010830     C                   MOVEL     ERR(10)       $MSG
048500010830     C                   GOTO      ENDCTR
048600010830     C                   END
048700010830     C*
048800010830     C* CONTROLLI se il cliente era definito già come "BC"
048900010830     C*   E NON È STATO FORZATO CON F8
049000010830     C   10§TMFBC        IFEQ      'S'
049100010830     C                   SETON                                        41  99
049200010830     C                   MOVEL     ERR(11)       $MSG
049300010830     C                   GOTO      ENDCTR
049400010830     C                   END
049500080625     C* CONTROLLI se il cliente era definito già come "S2"
049600080625     C   14§TMFs2        IFEQ      'S'
049700080625     C                   SETON                                        41  99
049800080625     C                   MOVEL     ERR(12)       $MSG
049900080625     C                   GOTO      ENDCTR
050000080625     C                   END
050100080625     C* CONTROLLI se il cliente era definito già come "SC"
050200080625     C   16§TMFsc        IFEQ      'S'
050300080625     C                   SETON                                        41  99
050400080625     C                   MOVEL     ERR(13)       $MSG
050500080625     C                   GOTO      ENDCTR
050600080625     C                   END
050700010830     C*-------------------------------
050800010828      *
050900010828      * --  --  --  --  --  --  --  --  --  --  --
051000940830     C* SE CLIENTE "BB" CON AUTORIZZAZIONE AL VERSAMENTO
051100940830     C* BANCA OBBLIGATORIA
051200951108     C  N11*IN10         IFEQ      '1'
051300940830     C     VAUTVE        ANDEQ     *BLANKS
051400940830     C     CODKSB        ANDEQ     0
051500940830     C                   SETON                                        424399
051600940830     C                   MOVEL     ERR(7)        $MSG
051700940830     C                   GOTO      ENDCTR
051800940830     C                   END
051900010830      *
052000010829     C* SE CLIENTE "BC" CON AUTORIZZAZIONE AL VERSAMENTO
052100010829     C* BANCA OBBLIGATORIA
052200010830     C  N11*IN13         IFEQ      '1'
052300010830     C     VAUTVE        ANDEQ     *BLANKS
052400010830     C     CODKSB        ANDEQ     0
052500010830     C                   SETON                                        424399
052600010830     C                   MOVEL     ERR(9)        $MSG
052700010830     C                   GOTO      ENDCTR
052800010830     C                   END
052900010828      *
053000940830     C* SE immessi sia gg. in più che in meno segnalo errore
053100951108     C  N11*IN10         IFEQ      '1'
053200940830     C     GGPIU         ANDNE     0
053300940830     C     GGMEN         ANDNE     0
053400940830     C                   SETON                                        444599
053500940830     C                   MOVEL     ERR(8)        $MSG
053600940830     C                   GOTO      ENDCTR
053700940830     C                   END
053800010830      *
053900010829     C* SE immessi sia gg. in più che in meno segnalo errore
054000010830     C  N11*IN13         IFEQ      '1'
054100010830     C     GGPIU         ANDNE     0
054200010830     C     GGMEN         ANDNE     0
054300010830     C                   SETON                                        444599
054400010830     C                   MOVEL     ERR(8)        $MSG
054500010830     C                   GOTO      ENDCTR
054600010830     C                   END
054700010830     C*------>>>
054800010830     C*** CODICE BANCA SU CUI EFFETTUARE VERSAMENTO
054900010830     C                   Z-ADD     KBNA          KCC
055000010830     C                   Z-ADD     CODKSB        KSC
055100010830     C     CODKSB        IFNE      *ZEROS
055200010830     C     KACO          CHAIN     CNACO000                           30
055300010830     C     ACOFLG        IFNE      ' '
055400010830     C     *IN30         OREQ      '1'
055500010830     C* CODICE BANCA   ANNULLATO
055600010830     C                   SETON                                        4299
055700010830     C                   MOVEL     ERR(6)        $MSG
055800010830     C                   GOTO      ENDCTR
055900010830     C                   END
056000010830     C                   END
056100010830     C*------>>>
056200010830     C*
056300920731     C     ENDCTR        ENDSR
056400930610     C**-------------------------------------------------------------**
056500930610     C** CARICAMENTO DEL SUBFILE                                     **
056600930610     C**-------------------------------------------------------------**
056700930610     C     CARSF         BEGSR
056800930610     C                   Z-ADD     0             NRR               4 0
056900930610     C*
057000930610     C* PULIZIA SUBFILE
057100930610     C                   SETON                                        35
057200930610     C                   WRITE     TB37DCT
057300930610     C                   SETOFF                                       35
057400930610     C*
057500010830     C   10              MOVEL     'BC'          DESFLG
057600010830     C   13              MOVEL     'BB'          DESFLG
057700080625     C   14              MOVEL     'S2'          DESFLG
057800080625     C   16              MOVEL     'SC'          DESFLG
057900930610     C* CARICO SUBFILE
058000930610     C                   MOVEL     'TM'          COD
058100930611     C                   MOVEL     *BLANKS       KEY
058200930610     C     KTAB2         SETLL     TABEL
058300930610     C     KTAB2         READE     TABEL                                  30
058400930610     C*
058500930610     C     *IN30         DOWEQ     '0'
058600930617     C* ESCLUDO ANNULLATI
058700930617     C     TBLFLG        IFEQ      ' '
058800930610     C                   MOVEL     TBLUNI        DSTM
058900930610     C                   MOVEL     ' '           VI2SCE
059000010830     C                   MOVEL     ' '           VI2BBC
059100010830     C                   SETOFF                                       5556
059200930610     C*
059300150520     C* CARICO I CODICI CLIENTI CON TIPO INCASSO richiesto
059400080625     c                   if        *in15 and §tmfto='S'   or
059500080624     c                             *in11 and §tmfba='S'   or
059600080624     c                             *in12 and §tmfca='S'   or
059700090617     c                             *in17 and §tmftm='S'   or
059800150520     c                             *in18 and §tmfbp='S'   or
059900150520     c                             *in19 and §tmftp='S'
060000930610     C                   MOVEL     TBLKEY        VI2KSC
060100930610     C                   MOVEL     §TMRAG        VI2DES
060200930610     C*
060300150803     c                   if        nrr = 9999
060400150803     c                   leave
060500150803     C                   END
060600930610     C                   ADD       1             NRR
060700930610     C                   WRITE     TB37DSF
060800930610     C                   END
060900080625     c
061000080625     C* CARICO I CODICI CLIENTI CON TIPO INCASSO "SC"
061100150520     c                   if        *in14  and (§tmfsc='S' or §tmfs2='S')
061200080625     c                   if        §tmfsc='S'
061300080625     c                   seton                                        55
061400080625     c                   else
061500080625     c                   seton                                        56
061600080625     c                   movel     '*'           vi2bbc
061700080625     c                   endif
061800080625
061900080625     C                   MOVEL     TBLKEY        VI2KSC
062000080625     C                   MOVEL     §TMRAG        VI2DES
062100080625     C*
062200150803     c                   if        nrr = 9999
062300150803     c                   leave
062400150803     C                   END
062500080625     C                   ADD       1             NRR
062600080625     C                   WRITE     TB37DSF
062700080625     C                   END
062800080625     c*
062900080625     C* CARICO I CODICI CLIENTI CON TIPO INCASSO "S2"
063000080625     c                   if        *in16  and (§tmfsc='S' or §tmfs2='S')
063100080625     c                   if        §tmfs2='S'
063200080625     c                   seton                                        55
063300080625     c                   else
063400080625     c                   seton                                        56
063500080625     c                   movel     '*'           vi2bbc
063600080625     c                   endif
063700080625
063800080625     C                   MOVEL     TBLKEY        VI2KSC
063900080625     C                   MOVEL     §TMRAG        VI2DES
064000080625     C*
064100150803     c                   if        nrr = 9999
064200150803     c                   leave
064300150803     C                   END
064400080625     C                   ADD       1             NRR
064500080625     C                   WRITE     TB37DSF
064600080625     C                   END
064700080625     c
064800930610     C*
064900930610     C* CARICO I CODICI CLIENTI CON TIPO INCASSO "BB"
065000080624     C   10§TMFBB        IFEQ      'S'
065100080624     c
065200080625     C     §TMFBB        IFEQ      'S'
065300010830     C     §TMFBC        ANDEQ     *BLANK
065400010830     C                   SETON                                            55
065500010830     C                   ELSE
065600010830     C                   SETOFF                                           55
065700010830     C                   END
065800010830     C     §TMFBC        IFEQ      'S'
065900010830     C                   MOVEL     '*'           VI2BBC
066000010830     C                   ELSE
066100010830     C                   MOVEL     ' '           VI2BBC
066200010830     C                   END
066300010830     C     VI2BBC        COMP      '*'                                    56
066400010830     C                   MOVEL     TBLKEY        VI2KSC
066500930610     C                   MOVEL     §TMRAG        VI2DES
066600930610     C*
066700150803     c                   if        nrr = 9999
066800150803     c                   leave
066900150803     C                   END
067000930610     C                   ADD       1             NRR
067100930610     C                   WRITE     TB37DSF
067200930610     C                   END
067300010829     C*
067400010829     C* CARICO I CODICI CLIENTI CON TIPO INCASSO "BC"
067500080624     C   13§TMFBB        IFEQ      'S'
067600010830     C     §TMFBC        COMP      'S'                                    55
067700010830     C     §TMFBB        IFEQ      'S'
067800010830     C     §TMFBC        ANDEQ     *BLANK
067900010830     C                   MOVEL     '*'           VI2BBC
068000010830     C                   ELSE
068100010830     C                   MOVEL     ' '           VI2BBC
068200010830     C                   END
068300010830     C     VI2BBC        COMP      '*'                                    56
068400010829     C                   MOVEL     TBLKEY        VI2KSC
068500010829     C                   MOVEL     §TMRAG        VI2DES
068600010829     C*
068700150803     c                   if        nrr = 9999
068800150803     c                   leave
068900150803     C                   END
069000010829     C                   ADD       1             NRR
069100010829     C                   WRITE     TB37DSF
069200010829     C                   END
069300930610     C*
069400930617     C                   END
069500930610     C     KTAB2         READE     TABEL                                  30
069600930610     C                   END
069700930610     C*
069800930610     C* EMETTO SUBFILE
069900930610     C     NRR           IFGT      0
070000930610     C*
070100930610     C     SOPRA         TAG
070200930610     C                   Z-ADD     1             REC
070300930610     C                   WRITE     TB37D02
070400930610     C                   EXFMT     TB37DCT
070500930610     C*
070600930610     C* CMD12 - RITORNO
070700930610     C     *INKL         IFEQ      '0'
070800930610     C*
070900930610     C* CONTROLLO DATI SUBFILE
071000930610     C                   READC     TB37DSF                                31
071100930610     C   31              GOTO      SOPRA
071200930610     C  N31VI2SCE        IFEQ      'X'
071300930611     C                   MOVEL     VI2KSC        KEY
071400930610     C                   MOVEL     VI2KSC        VIDKSC
071500930610     C                   MOVEL     VI2KSC        VIDLNP
071600930610     C                   MOVE      VI2KSC        VIDCLI
071700930610     C                   MOVEL     *BLANKS       DESCLI
071800930610     C                   MOVEL     VI2DES        DESCLI
071900930610     C                   SETON                                        90
072000930610     C                   ELSE
072100930610     C                   GOTO      SOPRA
072200930610     C                   END
072300930610     C*
072400930610     C                   ELSE
072500930610     C                   MOVEL     *BLANKS       VIDLNP
072600930610     C                   MOVEL     *BLANKS       VIDCLI
072700930610     C                   MOVEL     *BLANKS       DESCLI
072800930610     C                   END
072900930610     C                   END
073000930610     C                   ENDSR
073100930610     C**-------------------------------------------------------------**
073200930610     C** AGGIORNAMENTO / INSERIMENTO                                 **
073300930610     C**-------------------------------------------------------------**
073400891030     C     SCRIVI        BEGSR
073500920731     C                   MOVEL     CODUT         TBLKUT
073600930322     C                   MOVEL     'TM'          TBLCOD
073700920902     C                   MOVEL     VIDLNP        ALF7              7
073800920902     C                   MOVE      VIDCLI        ALF7              7
073900920902     C                   MOVEL     *BLANKS       TBLKEY
074000920902     C                   MOVEL     ALF7          TBLKEY
074100930322     C                   MOVEL     DESCLI        §TMRAG
074200930324     C                   MOVEL     ' '           TBLFLG
074300150520     c                   eval      §tmfti=savfti
074400940725     C     *IN10         IFEQ      '0'
074500010829     C     *IN13         ANDEQ     '0'
074600010829      *
074700080624     c                   select
074800080624     C     *IN11         wheneq    '1'
074900951108     C                   MOVEL     'S'           §TMFBA
075000080624     C     *IN12         wheneq    '1'
075100960826     C                   MOVEL     'S'           §TMFCA
075200080624     C     *IN14         wheneq    '1'
075300050525     C                   MOVEL     'S'           §TMFsc
075400050525     C                   MOVEL     'SC'          §TMFTI
075500111220     c                   if        vidforz ='N'
075600111220     c                   clear                   §TMFTI
075700111220     c                   endif
075800111220     c
075900080624     C     *IN15         wheneq    '1'
076000060626     C                   MOVEL     'S'           §TMFTO
076100080624     C     *IN16         wheneq    '1'
076200080624     C                   MOVEL     'S'           §TMFs2
076300080624     C                   MOVEL     'S2'          §TMFti
076400111220     c                   if        vidforz ='N'
076500111220     c                   clear                   §TMFTI
076600111220     c                   endif
076700090617     C     *IN18         wheneq    '1'
076800090617     C                   MOVEL     'S'           §TMFbp
076900150520     C     *IN19         wheneq    '1'
077000150520     C                   MOVEL     'S'           §TMFTp
077100080624     C                   other
077200951108     C                   MOVEL     'S'           §TMFTM
077300080624     C                   ENDSL
077400010829      *
077500951114     C     §TMFBB        IFNE      'S'
077600940725     C                   MOVEL     *ZEROS        §TMKSB
077700940725     C                   MOVEL     *ZEROS        §TMGG1
077800940725     C                   MOVEL     *ZEROS        §TMGG2
077900951114     C                   END
078000010829      *
078100940725     C                   ELSE
078200010829      *
078300010830     C     *INKB         IFEQ      *OFF
078400010829     C   13              MOVEL     'S'           §TMFBC
078500010830     C                   MOVEL     'S'           §TMFBB
078600010830     C                   ELSE
078700010830      * CON F2 FORZATURA:
078800010830      *   SI VUOLE INVERTIRE DA "BB" A "BC" O VICEVERSA
078900010830     C   13              MOVEL     ' '           §TMFBC
079000010830     C   10              MOVEL     'S'           §TMFBC
079100010830     C                   MOVEL     'S'           §TMFBB
079200010830     C                   END
079300010830      *
079400010830     C                   MOVEL     VAUTVE        §TMFVE
079500010830     C                   MOVEL     CODKSB        §TMKSB
079600010830     C                   MOVEL     GGPIU         §TMGG1
079700010830     C                   MOVEL     GGMEN         §TMGG2
079800010829      *
079900940725     C                   END
080000010829      *
080100930322     C                   MOVEL     DSTM          TBLUNI
080200920731     C*
080300920731     C                   MOVEL     '1'           TBLFTT
080400920731     C                   MOVEL     *ZEROS        TBLFLT
080500920731     C                   MOVEL     ' '           TBLFTR
080600920731     C                   Z-ADD     0             TBLDTR
080700920731     C*
080800891102     C   80              WRITE     TABEL                                98
080900891102     C  N80              UPDATE    TABEL                                98
081000920731     C*
081100891030     C                   ENDSR
081200920803     C*---------------------------------------------------------------*
081300150520     c* Annullamento tipo incasso
081400150520     C*---------------------------------------------------------------*
081500150520     c     Annull        BEGSR
081600150520     c
081700150520     C     *IN10         IFEQ      '0'
081800150520     C     *IN13         ANDEQ     '0'
081900150520     C
082000150520     C   17              MOVEL     ' '           §TMFTM
082100150520     C   18              MOVEL     ' '           §TMFBP
082200150520     C   19              MOVEL     ' '           §TMFTP
082300150520     C
082400150520     C   11              MOVEL     ' '           §TMFBA
082500150520     C
082600150520     C   12              MOVEL     ' '           §TMFCA
082700150520     c*
082800150520     C   14              MOVEL     *BLANKS       §TMFsc
082900150520     C   14              MOVEL     *BLANKS       §TMFTI
083000150520     C   15              MOVEL     *BLANKS       §TMFto
083100150520     C   16              MOVEL     *BLANKS       §TMFs2
083200150520     C   16              MOVEL     *BLANKS       §TMFTI
083300150520     C                   ENDif
083400150520     C*
083500150520     C     *IN10         IFEQ      *ON
083600150520     C     *IN13         OREQ      *ON
083700150520     C                   MOVEL     ' '           §TMFBB
083800150520     C                   MOVEL     ' '           §TMFBC
083900150520     C                   MOVEL     ' '           §TMFVE
084000150520     C                   MOVEL     *ZEROS        §TMKSB
084100150520     C                   MOVEL     *ZEROS        §TMGG1
084200150520     C                   MOVEL     *ZEROS        §TMGG2
084300150520     C                   END
084400150520      *
084500150520     C     §TMFTM        IFNE      'S'
084600150520     C     §TMFBB        ANDNE     'S'
084700150520     C     §TMFBC        ANDNE     'S'
084800150520     C     §TMFBA        ANDNE     'S'
084900150520     C     §TMFCA        ANDNE     'S'
085000150520     C     §TMFsc        ANDNE     'S'
085100150520     C     §TMFTO        ANDNE     'S'
085200150520     C     §TMFs2        ANDNE     'S'
085300150520     C     §TMFBP        ANDNE     'S'
085400150520     C     §TMFTP        ANDNE     'S'
085500150520     C                   MOVEL     '*'           TBLFLG
085600150520     C                   END
085700150520     C*
085800150520     C                   MOVEL     DSTM          TBLUNI
085900150520     C                   MOVEL     ' '           TBLFTR
086000150520     C                   Z-ADD     0             TBLDTR
086100150520     C*
086200150520     C                   UPDATE    TABEL
086300150520     c                   ENDSR
086400010830**   TES
086500150520 CODICE  IN  I M M i S S I O N E
086600150520 CODICE  IN  A G G I O R N A M E N T O
086700930610**   CM1
086800010830DLYJOB DLY(1)
086900940725**   ERR
087000940725Tasto di funzione non ammesso: codice in inserimento                  01
087100940725Codice cliente obbligatorio                                           02
087200940725Codice cliente annullato                                              03
087300940725Codice cliente errato                                                 04
087400940725Codice cliente bloccato                                               05
087500940725Codice banca inesistente o annullato                                  06
087600010829Banca obbligatoria se cliente "BB" con autorizzazione al versamento   07
087700940830Impossibile inserire entrambi: immettere GG. in più o GG. in meno     08
087800010829Banca obbligatoria se cliente "BC" con autorizzazione al versamento   09
087900010830Il Cliente è già definito come "BB"                                   10
088000010830Il Cliente è già definito come "BC"                                   11
088100080625Il Cliente è già definito come "S2"                                   12
088200080625Il Cliente è già definito come "SC"                                   13
