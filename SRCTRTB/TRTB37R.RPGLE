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
001900170309     f                                     extfile(wLibFile)  usropn
002000980105     FCNACO00F  IF   E           K DISK
002100170309      *
002200920723     D SKI             S              1    DIM(3)
002300150520     D TES             S             38    DIM(2) CTDATA PERRCD(1)
002400930611     D CM1             S             13    DIM(1) CTDATA PERRCD(1)
002500080625     D ERR             S             70    DIM(13) CTDATA PERRCD(1)
002600170309      *
002700930208     D PARAM           DS
002800150901     D  VIDFLG                        2
002900150901     D  parcli                        7
003000150901     D  paropz                        1
003100920723     D TCUDS           DS
003200920723     D  F1                     1      1
003300920723     D  F3                     3      3
003400920723     D  F2                     2      2
003500920723     D  F4                     4      4
003600920723     D  F56                    5      6
003700930322     D DSTM          E DS
003800930322     D KPJBA         E DS
003900920723     D  LIBSYS                92    100
004000170309     d TIBS55ds      e ds                  inz
004100980105     D TIBS69        E DS                  EXTNAME(TIBS69DS)
004200980105     D CNACO         E DS                  EXTNAME(CNACO00F)
004300980105     D CNIND         E DS                  EXTNAME(CNIND00F)
004400980105     D CNCLP         E DS                  EXTNAME(CNCLP00F)
004500980105     D FNCLS         E DS                  EXTNAME(FNCLS00F)
004600971210     D TRUL12        E DS                  EXTNAME(TRUL12DS)
004700920723     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
004800920723     D  TCU                  398    697
004900920723     D                                     DIM(50)
005000920723     D  KCU                  698    847P 0
005100920723     D                                     DIM(50)
005200920723     D                                     PACKEVEN
005300920723     D  DTU                  848    895P 0
005400920723     D                                     DIM(12)
005500920723     D                                     PACKEVEN
005600170309      *
005700170309      * -?Nome esteso Libreria/File del file tabella?
005800170309     d wLibFile        s             21a   inz
005900170309      *
006000170309      * -?Flag "Libreria" del file tabella: *off=Sede, *on=Filiale?
006100170309     d $Fil            s               n   inz
006200170309      *
006300920723     I/SPACE 3
006400920731     C*****************************************************************
006500920731     C*  RIEPILOGO INDICATORI
006600920731     C*****************************************************************
006700920731     C* 01    - INSERIMENTO
006800920731     C* 02    - VARIAZIONE
006900930208     C* 10    - ON   GESTIONE TABELLA "BB"
007000951108     C* 11    - ON   GESTIONE TABELLA "BA"
007100080624     C* 10/11 - OFF  GESTIONE TABELLA "TM"   anche 17 on
007200080624     C* 12    - ON   GESTIONE TABELLA "CA"
007300080624     C* 13    - ON   GESTIONE TABELLA "BC"
007400050525     C* 14    - ON   GESTIONE TABELLA "SC"
007500060626     C* 15    - ON   GESTIONE TABELLA "TO"
007600080624     C* 16    - ON   GESTIONE TABELLA "S2"
007700080624     C* 17    - ON   GESTIONE TABELLA "TM"
007800090617     C* 18    - ON   GESTIONE TABELLA "BP"
007900150520     C* 19    - ON   GESTIONE TABELLA "TP"
008000920910     C* 20/21 - DI COMODO
008100930610     C* 35    - PULIZIA SUBFILE
008200920731     C* 80/98 - DI COMODO,RIUTILIZZABILI
008300920731     C* 99    - ERRORE GENERICO
008400920731     C*****************************************************************
008500920731     C*
008600891030     C     *ENTRY        PLIST
008700891030     C                   PARM                    KPJBA
008800170309     C                   MOVEL     KPJBU         PARAM
008900170309     C*
009000891030     C                   Z-ADD     1             CODUT
009100920723     C                   CALL      'X§PARUT'
009200920723     C                   PARM                    UT§DSE
009300891030     C                   MOVEL     RAGUT         RSUT
009400920731     C                   MOVEL     RAGUT         PARDUT           30
009500920723     C*--- RICERCA CAPOCONTI------------------------------------------*
009600920723     C                   DO        50            X                 2 0
009700920723     C                   MOVE      TCU(X)        TCUDS
009800920723     C     F56           CABNE     'CG'          END1
009900920723     C     F4            COMP      '1'                                    21
010000920723     C     F4            COMP      '2'                                    22
010100920723     C     F4            COMP      '3'                                    23
010200920723     C     F4            COMP      '6'                                    27
010300920723     C** 1 CLIENTI   21
010400920723     C** 2 FORNITORI 22
010500920723     C** 3 AGENTI    23
010600920723     C     F3            COMP      '0'                                242425
010700920723     C     F3            COMP      'I'                                    26
010800920723     C** 0 ITALIA   25
010900920723     C** 1 ESTERO   24
011000920723     ** I CAPO CONTO IVA
011100920723     C   21
011200920723     CAN 24              Z-ADD     KCU(X)        KCE               4 0
011300920723     C   21
011400920723     CAN 25              Z-ADD     KCU(X)        KCI               4 0
011500920723     C   22
011600920723     CAN 24              Z-ADD     KCU(X)        KFE               4 0
011700920723     C   22
011800920723     CAN 25              Z-ADD     KCU(X)        KFI               4 0
011900920723     C   23
012000920723     CAN 24              Z-ADD     KCU(X)        KAE               4 0
012100920723     C   23
012200920723     CAN 25              Z-ADD     KCU(X)        KAI               4 0
012300920723     C   26              Z-ADD     KCU(X)        KIVA              4 0
012400920723     C   27              Z-ADD     KCU(X)        KBNA              4 0
012500920723     C     END1          TAG
012600920723     C                   END
012700920723     C*---------------------------------------------------------------*
012800920731     C* ACCESSO   CNACO
012900920731     C     KACO          KLIST
013000920731     C                   KFLD                    CODUT
013100940725     C                   KFLD                    KCC               4 0
013200920731     C                   KFLD                    KSC               7 0
013300920731     C*
013400920723     C* CHIAVE X POSIZIONAMENTO TABELLE
013500920731     C     KTAB          KLIST
013600920731     C                   KFLD                    CODUT
013700920731     C                   KFLD                    COD               2
013800920731     C                   KFLD                    KEY               8
013900930610     C     KTAB2         KLIST
014000930610     C                   KFLD                    CODUT
014100930610     C                   KFLD                    COD
014200150520     C*---------------------------------------------------------------*
014300170309      * -?Reperimento librerie (di Sede e di Filiale) con il *file?
014400170309      *  ?TABEL00F?
014500170309     c                   exsr      sr_ReperimLib
014600170309      * -?Apertura del file TABEL00F del s.i. di Sede?
014700170309     c                   exsr      sr_OpenFileTab
014800170309      *
014900930322     C                   MOVEL     'TM'          COD
015000930611     C                   Z-ADD     13            LUNG             15 5
015100080624     c                   eval      vidtes2='**   '+%subst(vidflg:1:1)
015200080624     c                             +' '+%subst(vidflg:2:1)+'   **'
015300920204     C*
015400930208     C* 10 ON  - GESTIONE TABELLA "BB"
015500930208     C     VIDFLG        IFEQ      'BB'
015600930208     C                   SETON                                        10
015700930208     C                   END
015800951108     C* 11 ON  - GESTIONE TABELLA "BA"
015900951108     C     VIDFLG        IFEQ      'BA'
016000951108     C                   SETON                                        11
016100951108     C                   END
016200960826     C* 12 ON  - GESTIONE TABELLA "CA"
016300960826     C     VIDFLG        IFEQ      'CA'
016400960826     C                   SETON                                        12
016500960826     C                   END
016600010828     C* 13 ON  - GESTIONE TABELLA "BC"
016700010828     C     VIDFLG        IFEQ      'BC'
016800010828     C                   SETON                                        13
016900010828     C                   END
017000050525     C* 14 ON  - GESTIONE TABELLA "SC"
017100050525     C     VIDFLG        IFEQ      'SC'
017200050525     C                   SETON                                        14
017300050525     C                   END
017400060626     C* 15 ON  - GESTIONE TABELLA "TO"
017500060626     C     VIDFLG        IFEQ      'TO'
017600060626     C                   SETON                                        15
017700060626     C                   END
017800080624     C* 16 ON  - GESTIONE TABELLA "S2"
017900080624     C     VIDFLG        IFEQ      'S2'
018000080624     C                   SETON                                        16
018100080624     C                   END
018200080624     C* 17 ON  - GESTIONE TABELLA "TM"
018300080624     C     VIDFLG        IFEQ      'TM'
018400080624     C                   SETON                                        17
018500080624     C                   END
018600090617     C* 18 ON  - GESTIONE TABELLA "BP"
018700090617     C     VIDFLG        IFEQ      'BP'
018800090617     C                   SETON                                        18
018900090617     C                   END
019000150520     C* 19 ON  - GESTIONE TIC     "TP"
019100150520     C     VIDFLG        IFEQ      'TP'
019200150520     C                   SETON                                        19
019300150520     C                   END
019400930610     C*
019500930610     C                   MOVEL     *BLANKS       COMMAN
019600930610     C                   MOVEA     CM1(1)        COMMAN           80
019700930610     C                   CALL      'QCMDEXC'
019800930610     C                   PARM                    COMMAN
019900930610     C                   PARM                    LUNG
020000930610     C*
020100920204     C     STAR          TAG
020200150901     C                   MOVEL     parcli        VIDLNP
020300150901     C                   move      parcli        VIDCLI
020400940725     C                   Z-ADD     0             CODKSB
020500940725     C                   Z-ADD     0             GGPIU
020600940725     C                   Z-ADD     0             GGMEN
020700920731     C                   MOVEL     *BLANKS       DESCLI
020800920803     C                   MOVEL     *BLANKS       VIDTES
020900150902     C                   MOVEL     *blank        VIDKSC            7 0
021000150902     C                   MOVEL     *zero         PREKSC            7 0
021100150520     C                   MOVEL     'N'           VAUTVE
021200111220     C                   MOVEL     *BLANKS       vidforz
021300010830     C                   SETOFF                                       0102
021400920204     C*
021500891030     C     INIZIO        TAG
021600150901     c                   if        vidcli <=*zeros
021700170309     c                             or     *in99
021800920731     C                   EXFMT     TB37D01
021900150901     c                   endif
022000920731     C*
022100080624     C** F3 - FINE LAVORO
022200920731     C   KC              GOTO      FINE
022300920731     C*
022400920731     C* CONTROLLI
022500920731     C                   EXSR      CTRFMT
022600920910     C   99              GOTO      INIZIO
022700150520     c
022800150520     c                   if        vidlnp<=*zeros and vidcli<=*zeros
022900150520     c                   GOTO      INIZIO
023000150520     c                   endif
023100920910     C*
023200150520     c
023300150520     c     for03         TAG
023400150520     C                   EXFMT     TB37D03
023500150520     c                   setoff                                       994142
023600150521     C   KC              GOTO      star
023700150520     c
023800150520     c                   exsr      contr
023900150520     c
024000150520     c                   if        *in99 or *in90 or (not *inkf and
024100150520     c                             not *inkq and not *inkb)
024200150520     c                   goto      for03
024300150520     c                   endif
024400150520     c*  F16 annullamento
024500150520     C                   if        *inkq and *in01
024600150520     C                   SETON                                        4199
024700150520     C                   MOVEL     ERR(1)        $MSG
024800150520     C                   GOTO      for03
024900150520     C                   ENDif
025000150520     C                   if        *inkq and *in02
025100150520     c                   exsr      Annull
025200170309     c                   eval      $Fil = *on
025300170309     c                   exsr      sr_OpenFileTab
025400170309     c     KTAB          chain     TABEL                              80
025500170309     c                   exsr      Annull
025600170309     c                   eval      $Fil = *off
025700170309     c                   exsr      sr_OpenFileTab
025800920731     C                   GOTO      STAR
025900150520     C                   endif
026000920910     C*
026100150520     C* F6/F2 - AGGIORNAMENTO
026200920910     C                   EXSR      SCRIVI
026300170309     c                   eval      $Fil = *on
026400170309     c                   exsr      sr_OpenFileTab
026500170309     c     KTAB          chain     TABEL                              80
026600170309     c                   exsr      Scrivi
026700170309     c                   eval      $Fil = *off
026800170309     c                   exsr      sr_OpenFileTab
026900891102     C                   GOTO      STAR
027000920731     C*
027100891030     C     FINE          TAG
027200980105     C                   CLEAR                   TIBS69
027300980105     C                   MOVEL     'C'           I69TLA
027400980105     C                   CALL      'TIBS69R'
027500980105     C                   PARM                    TIBS69
027600980105     C                   PARM                    CNACO
027700980105     C                   PARM                    CNIND
027800980105     C                   PARM                    CNCLP
027900980105     C                   PARM                    FNCLS
028000980105     C*
028100891030     C                   SETON                                        LR
028200920731     C*
028300170309
028400170309      *----------------------------------------------------------------
028500170309      *?Reperimento librerie con il *file TABEL00F per i due sistemi   ?
028600170309      *?  informativi (di Sede e di Filiale).                          ?
028700170309      *----------------------------------------------------------------
028800170309     c     sr_ReperimLib BEGSR
028900170309      *
029000170309      * -?Reperimento librerie con il *file TABEL00F?
029100170309     c                   clear                   TIBS55ds
029200170310     c                   eval      I50tla = 'L'
029300170310     c*//                eval      I50ppo = 046
029400170310     c*//                eval      I50apo = 001
029500170310     c                   Select
029600170310      * -?Dal S.I. di Sede?
029700170310     c                   When      %subst( kNSIf : 1 : 3 ) = 'GAI'
029800170310     c                   eval      I50psi = kNSIf
029900170310     c                   if        %subst( kNSIf : 7 : 1 ) = 'P'
030000170310     c                   eval      I50asi = 'FILTRAPFP '
030100170310     c                   else
030200170310     c                   eval      I50asi = 'FILTRA201 '
030300170310     c                   endif
030400170310      * -?Dal S.I. di Filiale?
030500170310     c                   When      %subst( kNSIf : 1 : 3 ) = 'FIL'
030600170310     c                   if        %subst( kNSIf : 7 : 1 ) = 'P'
030700170310     c                   eval      I50psi = 'GAITRAPSP '
030800170310     c                   else
030900170310     c                   eval      I50psi = 'GAITRA201 '
031000170310     c                   endif
031100170310     c                   eval      I50asi = kNSIf
031200170310     c                   EndSl
031300170310      *
031400170309     c                   call      'TIBS55R'
031500170309     c                   parm                    TIBS55ds
031600170309      *
031700170309     c                   eval      $Fil = *off
031800170309      *
031900170309     c                   ENDSR
032000170309
032100170309      *----------------------------------------------------------------
032200170309      *?Apertura dei files tabelle nel sistema informativo impostato.  ?
032300170309      *----------------------------------------------------------------
032400170309     c     sr_OpenFileTabBEGSR
032500170309      *
032600170309      * -?Chiusura (eventuale) del file TABEL00F già aperto?
032700170309     c                   if        %open(TABEL00F)
032800170309     c                   close     TABEL00F
032900170309     c                   endif
033000170309      *
033100170309     c                   if        $Fil = *off
033200170309      * -?Impostazione libreria di SEDE nel nome-file?
033300170310     c                   eval      wLibFile = %trimR( O50plg )
033400170309     c                                      + '/' + 'TABEL00F'
033500170309     c                   else
033600170309      * -?Impostazione libreria di FILIALE nel nome-file?
033700170310     c                   eval      wLibFile = %trimR( O50alg )
033800170309     c                                      + '/' + 'TABEL00F'
033900170309     c                   endif
034000170309      *
034100170309      * -?Apertura del file TABEL00F?
034200170309      *  ?(la cui libreria è appena stata impostata)?
034300170309     c                   open      TABEL00F
034400170309      *
034500170309     c                   ENDSR
034600170309
034700930610     C**-------------------------------------------------------------**
034800930610     C** CONTROLLI  VIDEATA                                          **
034900930610     C**-------------------------------------------------------------**
035000920731     C     CTRFMT        BEGSR
035100940830     C                   SETOFF                                       9990
035200940830     C                   SETOFF                                       414243
035300940830     C                   SETOFF                                       4445
035400920910     C                   MOVEL     *BLANKS       KEY
035500920731     C*
035600920909     C*** LINEA DI PARTENZA
035700920909     C                   MOVEA     VIDLNP        SKI
035800920909     C     '?'           LOOKUP    SKI                                    20
035900930610     C* CARICO SUBFILE
036000930610     C   20              EXSR      CARSF
036100930610     C*
036200930610     C* C O N T R O L L I
036300920910     C     VIDLNP        IFNE      *BLANKS
036400920909     C                   TESTN                   VIDLNP               21
036500940725     C  N21              SETON                                        4199
036600940725     C  N21              MOVEL     ERR(4)        $MSG
036700150520     C  N21              GOTO      ENDCTR1
036800920910     C                   END
036900920909     C*
037000920731     C*** SE E' STATA DIGITATA UNA CHIAVE DI RICERCA E NON E' STATO
037100920731     C***    IMMESSO IL CODICE CLIENTE CALL PGM
037200920731     C     VIDCLI        IFEQ      0
037300920731     C     DESCLI        IFNE      *BLANKS
037400920731     C* PARSTA = 9  ESCLUDO ANNULLATI
037500920731     C                   Z-ADD     9             PARSTA
037600920731     C                   Z-ADD     0             ACOKSC
037700920910     C     VIDLNP        IFGT      *ZEROS
037800920731     C                   MOVEL     VIDLNP        ACOKSC
037900920910     C                   END
038000920910     C                   MOVEL     *BLANKS       DESCR            48
038100920910     C                   MOVEL     DESCLI        DESCR
038200920910     C                   MOVEL     KCI           CCC               4 0
038300920731     C                   CALL      'XALFA1'
038400920731     C                   PARM                    PARDUT
038500920731     C                   PARM                    CODUT
038600920910     C                   PARM                    DESCR
038700920910     C                   PARM                    CCC
038800920731     C                   PARM                    ACOKSC
038900920731     C                   PARM                    PARSTA            1 0
039000961017     C                   PARM                    PARFLR           90
039100920910     C     PARSTA        IFNE      -1
039200920731     C                   MOVE      ACOKSC        VIDCLI
039300920803     C                   MOVEL     ACOKSC        VIDLNP
039400920910     C                   MOVEL     DESCR         DESCLI
039500920909     C                   MOVEL     ACOKSC        VIDKSC
039600920910     C                   MOVEL     ACOKSC        KEY
039700920910     C                   END
039800920803     C*
039900920910     C                   SETON                                        90
040000150521     c                   if        parsta=-1
040100150521     c                   goto      endctr1
040200150521     c                   endif
040300150521     c
040400920731     C                   END
040500920731     C                   END
040600920731     C*
040700920910     C* LINEA DI PARTENZA OBBLIGATORIA
040800920910     C                   TESTN                   VIDLNP               21
040900940725     C  N21              SETON                                        4199
041000940725     C  N21              MOVEL     ERR(4)        $MSG
041100150520     C  N21              GOTO      ENDCTR1
041200920910     C*
041300920909     C* NON ACCETTARE CODICE CLIENTE = 0
041400920909     C     VIDLNP        IFEQ      *ZEROS
041500920909     C     VIDCLI        ANDEQ     *ZEROS
041600940725     C                   SETON                                        4199
041700940725     C                   MOVEL     ERR(2)        $MSG
041800150520     C                   GOTO      ENDCTR1
041900920909     C                   END
042000920909     C*
042100920731     C*** CODICE CLIENTE
042200920731     C                   MOVEL     *BLANKS       DESCLI
042300940725     C                   Z-ADD     KCI           KCC
042400940725     C                   MOVEL     VIDLNP        KSC
042500920731     C                   MOVE      VIDCLI        KSC
042600980105     C                   CLEAR                   TIBS69
042700980105     C                   CLEAR                   CNACO
042800980105     C                   CLEAR                   CNIND
042900980105     C                   CLEAR                   CNCLP
043000980105     C                   CLEAR                   FNCLS
043100980105     C                   MOVEL     ' '           I69TLA
043200980105     C                   MOVEL     KNSIF         I69SIF
043300980105     C                   Z-ADD     VIDCLI        I69KAC
043400980105     C                   MOVEL     VIDLNP        I69KAC
043500980105     C                   CALL      'TIBS69R'
043600980105     C                   PARM                    TIBS69
043700980105     C                   PARM                    CNACO
043800980105     C                   PARM                    CNIND
043900980105     C                   PARM                    CNCLP
044000980105     C                   PARM                    FNCLS
044100150520
044200080625  B3 C     O69ERR        IFNE      *BLANK
044300980105     C                   SETON                                        4199
044400080625     C                   MOVEL     err(4)        $MSG
044500150520     C                   GOTO      ENDCTR1
044600980105     C                   END
044700150520     c
044800920731     C* DECODIFICO CODICE CLIENTE
044900920731     C                   MOVEL     ACORAG        DESCLI
045000920731     C*
045100920731     C* NON ACCETTARE CLIENTI  8888  O  9999
045200920731     C     VIDCLI        IFEQ      8888
045300920731     C     VIDCLI        OREQ      9999
045400940725     C                   SETON                                        4199
045500940725     C                   MOVEL     ERR(4)        $MSG
045600150520     C                   GOTO      ENDCTR1
045700920731     C                   END
045800010830     C*
045900920910     C                   MOVEL     KSC           KEY
046000920910     C                   MOVEL     KSC           VIDKSC
046100010830     C*------>>>
046200010830     C*------>>>
046300951108     C*
046400080624    1C     VIDLNP        IFNE      *ZEROS
046500951108     C     VIDCLI        ANDNE     *ZEROS
046600951108     C*
046700150521     C                   SETOFF                                       010203
046800951108     C     KTAB          CHAIN     TABEL                              80
046900010830     C*
047000080624    2C     *IN80         IFEQ      '1'
047100951108     C                   MOVE      TES(1)        VIDTES
047200951108     C                   SETON                                        01
047300080625     c                   clear                   dstm
047400150520     c                   clear                   savfti            2
047500120629     c
047600080624   x2C                   ELSE
047700010830     C*
047800951108     C                   MOVEL     TBLUNI        DSTM
047900150520     c                   eval      savfti=§tmfti
048000050526     C*
048100080624    3c                   if        *in14 and §tmfsc='S'   or
048200080624     c                             *in15 and §tmfto='S'   or
048300080624     c                             *in16 and §tmfs2='S'   or
048400080624     c                             *in11 and §tmfba='S'   or
048500080624     c                             *in12 and §tmfca='S'   or
048600090617     c                             *in17 and §tmftm='S'   or
048700150520     c                             *in18 and §tmfbp='S'   or
048800150520     c                             *in19 and §tmftp='S'
048900080624     C                   MOVE      TES(2)        VIDTES
049000080624     C                   SETON                                        02
049100080624    3c                   endif
049200080624    3c                   if        *in14 and §tmfsc<>'S'   or
049300080624     c                             *in15 and §tmfto<>'S'   or
049400080624     c                             *in16 and §tmfs2<>'S'   or
049500080624     c                             *in11 and §tmfba<>'S'   or
049600080624     c                             *in12 and §tmfca<>'S'   or
049700090617     c                             *in17 and §tmftm<>'S'   or
049800150520     c                             *in18 and §tmfbp<>'S'   or
049900150520     c                             *in19 and §tmftp<>'S'
050000080624     C                   MOVE      TES(1)        VIDTES
050100080624     C                   SETON                                        01
050200080624    3c                   endif
050300080624     c
050400111220     c* campo per SC S2
050500120629    3c                   if        *in14 or *in16
050600150521     c                   seton                                        03
050700120629     c
050800120628     C                   MOVEL     *BLANKS       vidforz
050900120629    5c                   if        *in14 and §tmfsc='S'   or
051000120629     c                             *in16 and §tmfs2='S'
051100120629    6c                   if        §tmfti=*blanks
051200111220     c                   eval      vidforz ='N'
051300120629    6c                   endif
051400120629    5c                   endif
051500120629
051600120629    3c                   endif
051700120628     c
051800951108     C*
051900080624    3C   10§TMFBB        IFEQ      'S'
052000010830     C     §TMFBC        ANDEQ     *BLANK
052100951108     C                   MOVE      TES(2)        VIDTES
052200080624      *>>>>>>
052300080624    4C     VIDKSC        IFNE      PREKSC
052400951108     C                   MOVEL     §TMFVE        VAUTVE
052500951108     C                   MOVEL     §TMKSB        CODKSB
052600951108     C                   MOVEL     §TMGG1        GGPIU
052700951108     C                   MOVEL     §TMGG2        GGMEN
052800951108     C                   Z-ADD     VIDKSC        PREKSC
052900080624    4C                   ENDIF
053000150521     C                   SETON                                        0203
053100080624   x3C                   ELSE
053200951108     C                   MOVE      TES(1)        VIDTES
053300951108     C                   SETON                                        01
053400080624    3C                   END
053500010828     C*
053600080625    3C   13§TMFBC        IFEQ      'S'
053700010828     C                   MOVE      TES(2)        VIDTES
053800010829      *>>>>>>
053900010828     C     VIDKSC        IFNE      PREKSC
054000010828     C                   MOVEL     §TMFVE        VAUTVE
054100010828     C                   MOVEL     §TMKSB        CODKSB
054200010828     C                   MOVEL     §TMGG1        GGPIU
054300010828     C                   MOVEL     §TMGG2        GGMEN
054400010828     C                   Z-ADD     VIDKSC        PREKSC
054500010828     C                   ENDIF
054600150521     C                   SETON                                        0203
054700010828     C                   ELSE
054800010828     C                   MOVE      TES(1)        VIDTES
054900010828     C                   SETON                                        01
055000080624    3C                   END
055100010828     C*
055200080624    2C                   END
055300951108     C*
055400080624   x1C                   ELSE
055500951108     C                   MOVEL     *BLANKS       VIDTES
055600951108     C                   MOVEL     *ZEROS        VIDKSC
055700080624    1C                   END
055800150520     c     ENDCTR1       ENDSR
055900010830     C*
056000150520     C*-------------------------------------------------------------------------
056100150520     c     Contr         BEGSR
056200150520     C*------------------------------------------------------------------------
056300150520     C                   SETOFF                                       9990
056400150520     C                   SETOFF                                       414243
056500150520     C                   SETOFF                                       4445
056600150520     C* CODICE CLIENTE ANNULLATO
056700150520     C  NKQACOFLG        IFNE      ' '
056800150520     C                   SETON                                        4199
056900150520     C                   MOVEL     ERR(3)        $MSG
057000150520     C                   GOTO      ENDCTR
057100150520     c                   endif
057200150520     c
057300010830     C* CONTROLLI se il cliente era definito già come "BB"
057400010830     C*   E NON È STATO FORZATO CON F8
057500010830     C   13§TMFBB        IFEQ      'S'
057600010830     C     §TMFBC        ANDEQ     *BLANK
057700010830     C                   SETON                                        41  99
057800010830     C                   MOVEL     ERR(10)       $MSG
057900010830     C                   GOTO      ENDCTR
058000010830     C                   END
058100010830     C*
058200010830     C* CONTROLLI se il cliente era definito già come "BC"
058300010830     C*   E NON È STATO FORZATO CON F8
058400010830     C   10§TMFBC        IFEQ      'S'
058500010830     C                   SETON                                        41  99
058600010830     C                   MOVEL     ERR(11)       $MSG
058700010830     C                   GOTO      ENDCTR
058800010830     C                   END
058900080625     C* CONTROLLI se il cliente era definito già come "S2"
059000080625     C   14§TMFs2        IFEQ      'S'
059100080625     C                   SETON                                        41  99
059200080625     C                   MOVEL     ERR(12)       $MSG
059300080625     C                   GOTO      ENDCTR
059400080625     C                   END
059500080625     C* CONTROLLI se il cliente era definito già come "SC"
059600080625     C   16§TMFsc        IFEQ      'S'
059700080625     C                   SETON                                        41  99
059800080625     C                   MOVEL     ERR(13)       $MSG
059900080625     C                   GOTO      ENDCTR
060000080625     C                   END
060100010830     C*-------------------------------
060200010828      *
060300010828      * --  --  --  --  --  --  --  --  --  --  --
060400940830     C* SE CLIENTE "BB" CON AUTORIZZAZIONE AL VERSAMENTO
060500940830     C* BANCA OBBLIGATORIA
060600951108     C  N11*IN10         IFEQ      '1'
060700940830     C     VAUTVE        ANDEQ     *BLANKS
060800940830     C     CODKSB        ANDEQ     0
060900940830     C                   SETON                                        424399
061000940830     C                   MOVEL     ERR(7)        $MSG
061100940830     C                   GOTO      ENDCTR
061200940830     C                   END
061300010830      *
061400010829     C* SE CLIENTE "BC" CON AUTORIZZAZIONE AL VERSAMENTO
061500010829     C* BANCA OBBLIGATORIA
061600010830     C  N11*IN13         IFEQ      '1'
061700010830     C     VAUTVE        ANDEQ     *BLANKS
061800010830     C     CODKSB        ANDEQ     0
061900010830     C                   SETON                                        424399
062000010830     C                   MOVEL     ERR(9)        $MSG
062100010830     C                   GOTO      ENDCTR
062200010830     C                   END
062300010828      *
062400940830     C* SE immessi sia gg. in più che in meno segnalo errore
062500951108     C  N11*IN10         IFEQ      '1'
062600940830     C     GGPIU         ANDNE     0
062700940830     C     GGMEN         ANDNE     0
062800940830     C                   SETON                                        444599
062900940830     C                   MOVEL     ERR(8)        $MSG
063000940830     C                   GOTO      ENDCTR
063100940830     C                   END
063200010830      *
063300010829     C* SE immessi sia gg. in più che in meno segnalo errore
063400010830     C  N11*IN13         IFEQ      '1'
063500010830     C     GGPIU         ANDNE     0
063600010830     C     GGMEN         ANDNE     0
063700010830     C                   SETON                                        444599
063800010830     C                   MOVEL     ERR(8)        $MSG
063900010830     C                   GOTO      ENDCTR
064000010830     C                   END
064100010830     C*------>>>
064200010830     C*** CODICE BANCA SU CUI EFFETTUARE VERSAMENTO
064300010830     C                   Z-ADD     KBNA          KCC
064400010830     C                   Z-ADD     CODKSB        KSC
064500010830     C     CODKSB        IFNE      *ZEROS
064600010830     C     KACO          CHAIN     CNACO000                           30
064700010830     C     ACOFLG        IFNE      ' '
064800010830     C     *IN30         OREQ      '1'
064900010830     C* CODICE BANCA   ANNULLATO
065000010830     C                   SETON                                        4299
065100010830     C                   MOVEL     ERR(6)        $MSG
065200010830     C                   GOTO      ENDCTR
065300010830     C                   END
065400010830     C                   END
065500010830     C*------>>>
065600010830     C*
065700920731     C     ENDCTR        ENDSR
065800930610     C**-------------------------------------------------------------**
065900930610     C** CARICAMENTO DEL SUBFILE                                     **
066000930610     C**-------------------------------------------------------------**
066100930610     C     CARSF         BEGSR
066200930610     C                   Z-ADD     0             NRR               4 0
066300930610     C*
066400930610     C* PULIZIA SUBFILE
066500930610     C                   SETON                                        35
066600930610     C                   WRITE     TB37DCT
066700930610     C                   SETOFF                                       35
066800930610     C*
066900010830     C   10              MOVEL     'BC'          DESFLG
067000010830     C   13              MOVEL     'BB'          DESFLG
067100080625     C   14              MOVEL     'S2'          DESFLG
067200080625     C   16              MOVEL     'SC'          DESFLG
067300930610     C* CARICO SUBFILE
067400930610     C                   MOVEL     'TM'          COD
067500930611     C                   MOVEL     *BLANKS       KEY
067600930610     C     KTAB2         SETLL     TABEL
067700930610     C     KTAB2         READE     TABEL                                  30
067800930610     C*
067900930610     C     *IN30         DOWEQ     '0'
068000930617     C* ESCLUDO ANNULLATI
068100930617     C     TBLFLG        IFEQ      ' '
068200930610     C                   MOVEL     TBLUNI        DSTM
068300930610     C                   MOVEL     ' '           VI2SCE
068400010830     C                   MOVEL     ' '           VI2BBC
068500010830     C                   SETOFF                                       5556
068600930610     C*
068700150520     C* CARICO I CODICI CLIENTI CON TIPO INCASSO richiesto
068800080625     c                   if        *in15 and §tmfto='S'   or
068900080624     c                             *in11 and §tmfba='S'   or
069000080624     c                             *in12 and §tmfca='S'   or
069100090617     c                             *in17 and §tmftm='S'   or
069200150520     c                             *in18 and §tmfbp='S'   or
069300150520     c                             *in19 and §tmftp='S'
069400930610     C                   MOVEL     TBLKEY        VI2KSC
069500930610     C                   MOVEL     §TMRAG        VI2DES
069600930610     C*
069700150803     c                   if        nrr = 9999
069800150803     c                   leave
069900150803     C                   END
070000930610     C                   ADD       1             NRR
070100930610     C                   WRITE     TB37DSF
070200930610     C                   END
070300080625     c
070400080625     C* CARICO I CODICI CLIENTI CON TIPO INCASSO "SC"
070500150520     c                   if        *in14  and (§tmfsc='S' or §tmfs2='S')
070600080625     c                   if        §tmfsc='S'
070700080625     c                   seton                                        55
070800080625     c                   else
070900080625     c                   seton                                        56
071000080625     c                   movel     '*'           vi2bbc
071100080625     c                   endif
071200080625
071300080625     C                   MOVEL     TBLKEY        VI2KSC
071400080625     C                   MOVEL     §TMRAG        VI2DES
071500080625     C*
071600150803     c                   if        nrr = 9999
071700150803     c                   leave
071800150803     C                   END
071900080625     C                   ADD       1             NRR
072000080625     C                   WRITE     TB37DSF
072100080625     C                   END
072200080625     c*
072300080625     C* CARICO I CODICI CLIENTI CON TIPO INCASSO "S2"
072400080625     c                   if        *in16  and (§tmfsc='S' or §tmfs2='S')
072500080625     c                   if        §tmfs2='S'
072600080625     c                   seton                                        55
072700080625     c                   else
072800080625     c                   seton                                        56
072900080625     c                   movel     '*'           vi2bbc
073000080625     c                   endif
073100080625
073200080625     C                   MOVEL     TBLKEY        VI2KSC
073300080625     C                   MOVEL     §TMRAG        VI2DES
073400080625     C*
073500150803     c                   if        nrr = 9999
073600150803     c                   leave
073700150803     C                   END
073800080625     C                   ADD       1             NRR
073900080625     C                   WRITE     TB37DSF
074000080625     C                   END
074100080625     c
074200930610     C*
074300930610     C* CARICO I CODICI CLIENTI CON TIPO INCASSO "BB"
074400080624     C   10§TMFBB        IFEQ      'S'
074500080624     c
074600080625     C     §TMFBB        IFEQ      'S'
074700010830     C     §TMFBC        ANDEQ     *BLANK
074800010830     C                   SETON                                            55
074900010830     C                   ELSE
075000010830     C                   SETOFF                                           55
075100010830     C                   END
075200010830     C     §TMFBC        IFEQ      'S'
075300010830     C                   MOVEL     '*'           VI2BBC
075400010830     C                   ELSE
075500010830     C                   MOVEL     ' '           VI2BBC
075600010830     C                   END
075700010830     C     VI2BBC        COMP      '*'                                    56
075800010830     C                   MOVEL     TBLKEY        VI2KSC
075900930610     C                   MOVEL     §TMRAG        VI2DES
076000930610     C*
076100150803     c                   if        nrr = 9999
076200150803     c                   leave
076300150803     C                   END
076400930610     C                   ADD       1             NRR
076500930610     C                   WRITE     TB37DSF
076600930610     C                   END
076700010829     C*
076800010829     C* CARICO I CODICI CLIENTI CON TIPO INCASSO "BC"
076900080624     C   13§TMFBB        IFEQ      'S'
077000010830     C     §TMFBC        COMP      'S'                                    55
077100010830     C     §TMFBB        IFEQ      'S'
077200010830     C     §TMFBC        ANDEQ     *BLANK
077300010830     C                   MOVEL     '*'           VI2BBC
077400010830     C                   ELSE
077500010830     C                   MOVEL     ' '           VI2BBC
077600010830     C                   END
077700010830     C     VI2BBC        COMP      '*'                                    56
077800010829     C                   MOVEL     TBLKEY        VI2KSC
077900010829     C                   MOVEL     §TMRAG        VI2DES
078000010829     C*
078100150803     c                   if        nrr = 9999
078200150803     c                   leave
078300150803     C                   END
078400010829     C                   ADD       1             NRR
078500010829     C                   WRITE     TB37DSF
078600010829     C                   END
078700930610     C*
078800930617     C                   END
078900930610     C     KTAB2         READE     TABEL                                  30
079000930610     C                   END
079100930610     C*
079200930610     C* EMETTO SUBFILE
079300930610     C     NRR           IFGT      0
079400930610     C*
079500930610     C     SOPRA         TAG
079600930610     C                   Z-ADD     1             REC
079700930610     C                   WRITE     TB37D02
079800930610     C                   EXFMT     TB37DCT
079900930610     C*
080000930610     C* CMD12 - RITORNO
080100930610     C     *INKL         IFEQ      '0'
080200930610     C*
080300930610     C* CONTROLLO DATI SUBFILE
080400930610     C                   READC     TB37DSF                                31
080500930610     C   31              GOTO      SOPRA
080600930610     C  N31VI2SCE        IFEQ      'X'
080700930611     C                   MOVEL     VI2KSC        KEY
080800930610     C                   MOVEL     VI2KSC        VIDKSC
080900930610     C                   MOVEL     VI2KSC        VIDLNP
081000930610     C                   MOVE      VI2KSC        VIDCLI
081100930610     C                   MOVEL     *BLANKS       DESCLI
081200930610     C                   MOVEL     VI2DES        DESCLI
081300930610     C                   SETON                                        90
081400930610     C                   ELSE
081500930610     C                   GOTO      SOPRA
081600930610     C                   END
081700930610     C*
081800930610     C                   ELSE
081900930610     C                   MOVEL     *BLANKS       VIDLNP
082000930610     C                   MOVEL     *BLANKS       VIDCLI
082100930610     C                   MOVEL     *BLANKS       DESCLI
082200930610     C                   END
082300930610     C                   END
082400930610     C                   ENDSR
082500930610     C**-------------------------------------------------------------**
082600930610     C** AGGIORNAMENTO / INSERIMENTO                                 **
082700930610     C**-------------------------------------------------------------**
082800891030     C     SCRIVI        BEGSR
082900920731     C                   MOVEL     CODUT         TBLKUT
083000930322     C                   MOVEL     'TM'          TBLCOD
083100920902     C                   MOVEL     VIDLNP        ALF7              7
083200920902     C                   MOVE      VIDCLI        ALF7              7
083300920902     C                   MOVEL     *BLANKS       TBLKEY
083400920902     C                   MOVEL     ALF7          TBLKEY
083500930322     C                   MOVEL     DESCLI        §TMRAG
083600930324     C                   MOVEL     ' '           TBLFLG
083700170310      *
083800170310      * -?I dati nella DS della tabella vengono impostati SOLO?
083900170310      *  ?nel primo aggiornamento (per Sede);?
084000170310      *  ?nel secondo (per Filiale) sono già impostati.?
084100170310if  1c                   If        $Fil = *off
084200170310      *
084300150520     c                   eval      §tmfti=savfti
084400940725     C     *IN10         IFEQ      '0'
084500010829     C     *IN13         ANDEQ     '0'
084600010829      *
084700080624     c                   select
084800080624     C     *IN11         wheneq    '1'
084900951108     C                   MOVEL     'S'           §TMFBA
085000080624     C     *IN12         wheneq    '1'
085100960826     C                   MOVEL     'S'           §TMFCA
085200080624     C     *IN14         wheneq    '1'
085300050525     C                   MOVEL     'S'           §TMFsc
085400050525     C                   MOVEL     'SC'          §TMFTI
085500111220     c                   if        vidforz ='N'
085600111220     c                   clear                   §TMFTI
085700111220     c                   endif
085800111220     c
085900080624     C     *IN15         wheneq    '1'
086000060626     C                   MOVEL     'S'           §TMFTO
086100080624     C     *IN16         wheneq    '1'
086200080624     C                   MOVEL     'S'           §TMFs2
086300080624     C                   MOVEL     'S2'          §TMFti
086400111220     c                   if        vidforz ='N'
086500111220     c                   clear                   §TMFTI
086600111220     c                   endif
086700090617     C     *IN18         wheneq    '1'
086800090617     C                   MOVEL     'S'           §TMFbp
086900150520     C     *IN19         wheneq    '1'
087000150520     C                   MOVEL     'S'           §TMFTp
087100080624     C                   other
087200951108     C                   MOVEL     'S'           §TMFTM
087300080624     C                   ENDSL
087400010829      *
087500951114     C     §TMFBB        IFNE      'S'
087600940725     C                   MOVEL     *ZEROS        §TMKSB
087700940725     C                   MOVEL     *ZEROS        §TMGG1
087800940725     C                   MOVEL     *ZEROS        §TMGG2
087900951114     C                   END
088000010829      *
088100940725     C                   ELSE
088200010829      *
088300010830     C     *INKB         IFEQ      *OFF
088400010829     C   13              MOVEL     'S'           §TMFBC
088500010830     C                   MOVEL     'S'           §TMFBB
088600010830     C                   ELSE
088700010830      * CON F2 FORZATURA:
088800010830      *   SI VUOLE INVERTIRE DA "BB" A "BC" O VICEVERSA
088900010830     C   13              MOVEL     ' '           §TMFBC
089000010830     C   10              MOVEL     'S'           §TMFBC
089100010830     C                   MOVEL     'S'           §TMFBB
089200010830     C                   END
089300010830      *
089400010830     C                   MOVEL     VAUTVE        §TMFVE
089500010830     C                   MOVEL     CODKSB        §TMKSB
089600010830     C                   MOVEL     GGPIU         §TMGG1
089700010830     C                   MOVEL     GGMEN         §TMGG2
089800010829      *
089900940725     C                   END
090000010829      *
090100170310      *
090200170310e   1c                   EndIf
090300170310      *
090400930322     C                   MOVEL     DSTM          TBLUNI
090500920731     C*
090600920731     C                   MOVEL     '1'           TBLFTT
090700920731     C                   MOVEL     *ZEROS        TBLFLT
090800170309     C*//                MOVEL     ' '           TBLFTR
090900170309     C*//                Z-ADD     0             TBLDTR
091000170309      * -?Impostazione del flag di trasmissione?
091100170309     c                   if        $Fil = *off
091200170309     c                   eval      TBLftr = 'T'
091300170309     c                   else
091400170309     c                   eval      TBLFtr = 'R'
091500170309     c                   endif
091600170309     c                   eval      TBLdtr = Udate
091700920731     C*
091800891102     C   80              WRITE     TABEL                                98
091900891102     C  N80              UPDATE    TABEL                                98
092000920731     C*
092100891030     C                   ENDSR
092200920803     C*---------------------------------------------------------------*
092300150520     c* Annullamento tipo incasso
092400150520     C*---------------------------------------------------------------*
092500150520     c     Annull        BEGSR
092600170310      *
092700170310      * -?I dati nella DS della tabella vengono impostati SOLO?
092800170310      *  ?nel primo aggiornamento (per Sede);?
092900170310      *  ?nel secondo (per Filiale) sono già impostati.?
093000170310if  1c                   If        $Fil = *off
093100170310      *
093200150520     C     *IN10         IFEQ      '0'
093300150520     C     *IN13         ANDEQ     '0'
093400150520     C
093500150520     C   17              MOVEL     ' '           §TMFTM
093600150520     C   18              MOVEL     ' '           §TMFBP
093700150520     C   19              MOVEL     ' '           §TMFTP
093800150520     C
093900150520     C   11              MOVEL     ' '           §TMFBA
094000150520     C
094100150520     C   12              MOVEL     ' '           §TMFCA
094200150520     c*
094300150520     C   14              MOVEL     *BLANKS       §TMFsc
094400150520     C   14              MOVEL     *BLANKS       §TMFTI
094500150520     C   15              MOVEL     *BLANKS       §TMFto
094600150520     C   16              MOVEL     *BLANKS       §TMFs2
094700150520     C   16              MOVEL     *BLANKS       §TMFTI
094800150520     C                   ENDif
094900150520     C*
095000150520     C     *IN10         IFEQ      *ON
095100150520     C     *IN13         OREQ      *ON
095200150520     C                   MOVEL     ' '           §TMFBB
095300150520     C                   MOVEL     ' '           §TMFBC
095400150520     C                   MOVEL     ' '           §TMFVE
095500150520     C                   MOVEL     *ZEROS        §TMKSB
095600150520     C                   MOVEL     *ZEROS        §TMGG1
095700150520     C                   MOVEL     *ZEROS        §TMGG2
095800150520     C                   END
095900170310      *
096000170310e   1c                   EndIf
096100150520      *
096200150520     C     §TMFTM        IFNE      'S'
096300150520     C     §TMFBB        ANDNE     'S'
096400150520     C     §TMFBC        ANDNE     'S'
096500150520     C     §TMFBA        ANDNE     'S'
096600150520     C     §TMFCA        ANDNE     'S'
096700150520     C     §TMFsc        ANDNE     'S'
096800150520     C     §TMFTO        ANDNE     'S'
096900150520     C     §TMFs2        ANDNE     'S'
097000150520     C     §TMFBP        ANDNE     'S'
097100150520     C     §TMFTP        ANDNE     'S'
097200150520     C                   MOVEL     '*'           TBLFLG
097300150520     C                   END
097400150520     C*
097500150520     C                   MOVEL     DSTM          TBLUNI
097600170309      * -?N.B. - L'annullamento  va lasciato in trasmissione?
097700170309      *         ?(vedi flag TBLFTR)?
097800150520     C                   MOVEL     ' '           TBLFTR
097900150520     C                   Z-ADD     0             TBLDTR
098000150520     C*
098100150520     C                   UPDATE    TABEL
098200150520     c                   ENDSR
098300010830**   TES
098400150520 CODICE  IN  I M M i S S I O N E
098500150520 CODICE  IN  A G G I O R N A M E N T O
098600930610**   CM1
098700010830DLYJOB DLY(1)
098800940725**   ERR
098900940725Tasto di funzione non ammesso: codice in inserimento                  01
099000940725Codice cliente obbligatorio                                           02
099100940725Codice cliente annullato                                              03
099200940725Codice cliente errato                                                 04
099300940725Codice cliente bloccato                                               05
099400940725Codice banca inesistente o annullato                                  06
099500010829Banca obbligatoria se cliente "BB" con autorizzazione al versamento   07
099600940830Impossibile inserire entrambi: immettere GG. in più o GG. in meno     08
099700010829Banca obbligatoria se cliente "BC" con autorizzazione al versamento   09
099800010830Il Cliente è già definito come "BB"                                   10
099900010830Il Cliente è già definito come "BC"                                   11
100000080625Il Cliente è già definito come "S2"                                   12
100100080625Il Cliente è già definito come "SC"                                   13
