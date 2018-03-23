000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200000529      * FNLS62R *-----------------------------------------------------*
000300000526      *          - PULIZIA BOLLE E STATISTICHE PARTENZE - MAIN        *
000400000526      *---------------------------------------------------------------*
000500920930     FTABEL00F  IF   E           K DISK
000600930804     D*---------------------------------------------------------------*
000700000526     D* PARAMETRI LANCIO PULIZIA                                      *
000800000526     D*---------------------------------------------------------------*
000900940930     D PARAM1          DS                  INZ
001000000526     D**
001100980721     D* DATA PULIZIA BOLLE PARTENZE (CON LNP NON ESTERA)
001200000526     D  VIDDBR                 1      8  0
001300980721     D**
001400961204     D* DATA PULIZIA BOLLE PARTENZE (CON LNP ESTERA - BOLLE IMPORT)
001500000526     D  VIDPBI               137    144  0
001600980721     D**
001700980721     D* DATA PULIZIA BOLLE CON RISERVA
001800000526     D  VIDBLR               161    168  0
001900990126     D**
002000990126     D* DATA PULIZIA BOLLE CON C.A.
002100000526     D  VIDBCA               169    176  0
002200000529     D**
002300000529     D* DATA PULIZIA BOLLE PARTENZE - POSTE
002400000529     D  POSTBR               177    184  0
002500000529     D**
002600000529     D* DATA PULIZIA BOLLE PARTENZE - BOLLE IMPORT - POSTE
002700000529     D  POSTBI               185    192  0
002800000529     D**
002900000529     D* DATA PULIZIA BOLLE CON RISERVA - POSTE
003000000529     D  POSTLR               193    200  0
003100000529     D**
003200000529     D* DATA PULIZIA BOLLE CON C.A. - POSTE
003300000529     D  POSTCA               201    208  0
003400000912     D**
003500000912     D* DATA PULIZIA BOLLE POSTE NON PARTITE
003600000912     D  DATGDP               217    224  0
003700080218     D**
003800040423     D* DATA PULIZIA BOLLE CON RECLAMI
003900040423     D  DATREC               233    240  0
004000000526     D*---------------------------------------------------------------*
004100000526     D* PARAMETRI CONTROLLO E INVERSIONE DATE                         *
004200000526     D*---------------------------------------------------------------*
004300910510     D WLBDAT          DS
004400940929     D  G02DAT                 1      8  0
004500940929     D  G02INV                 9     16  0
004600940929     D  G02ERR                17     17
004700940929     D  G02TGI                18     22  0
004800910510     D WGIDAT          DS
004900940929     D  GIODAT                 1      8  0
005000940929     D  GIOINV                 9     16  0
005100940929     D  GIOTGI                17     21  0
005200000526     D*---------------------------------------------------------------*
005300000526     D* PARAMETRI ESTERNI                                             *
005400000526     D*---------------------------------------------------------------*
005500930805     D KPJBA         E DS
005600000526     D*---------------------------------------------------------------*
005700000526     D* DS GIORNI DI PULIZIA                                          *
005800000526     D*---------------------------------------------------------------*
005900930614     D DS5A          E DS
006000970718     D DS5A2         E DS
006100000529     D DS5APT        E DS
006200100618     D dvpopuladd    E DS
006300000526     C*
006400000526     C*****************************************************************
006500000526     C*                                                               *
006600000526     C*        R O U T I N E   P R I N C I P A L E                    *
006700000526     C*                                                               *
006800000526     C*****************************************************************
006900930804     C*
007000930804     C* DEFINIZIONI VARIABILI, KLIST E PLIST
007100930804     C                   EXSR      DEFVAR
007200930614     C*
007300930804     C* CARICO FILIALI IN GESTIONE
007400920930     C                   EXSR      CARTAB
007500920930     C*
007600000526     C* INIZIALIZZO DATE
007700000526     C                   Z-ADD     0             VIDDBR            8 0
007800000526     C                   Z-ADD     0             VIDPBI            8 0
007900000526     C                   Z-ADD     0             VIDBLR            8 0
008000000526     C                   Z-ADD     0             VIDBCA            8 0
008100000529     C                   Z-ADD     0             POSTBR            8 0
008200000529     C                   Z-ADD     0             POSTBI            8 0
008300000529     C                   Z-ADD     0             POSTLR            8 0
008400000529     C                   Z-ADD     0             POSTCA            8 0
008500000912     C                   Z-ADD     0             DATGDP            8 0
008600040423     C                   Z-ADD     0             DATREC            8 0
008700940930     C*
008800000526     C*----------------------------------------------------------------
008900000526     C*
009000940930     C                   EXSR      IMPOST
009100910510     C*
009200000526     C*----------------------------------------------------------------
009300000526     C*
009400000529     C* GIRO TUTTE LE DATE DA GG/MM/AAAA  A AAAA/MM/GG
009500000529     C*
009600000526     C     VIDDBR        IFGT      0
009700000526     C                   MOVE      VIDDBR        G02DAT
009800000526     C                   MOVEL     *BLANK        G02ERR
009900000526     C                   CALL      'XSRDA8'
010000000526     C                   PARM                    WLBDAT
010100000526     C     G02ERR        IFEQ      '1'
010200000526     C                   Z-ADD     0             VIDDBR
010300000526     C                   ELSE
010400000526     C                   Z-ADD     G02INV        VIDDBR
010500000526     C                   ENDIF
010600000526     C                   ENDIF
010700000526     C*
010800000526     C     VIDBLR        IFGT      0
010900000526     C                   MOVE      VIDBLR        G02DAT
011000000526     C                   MOVEL     *BLANK        G02ERR
011100000526     C                   CALL      'XSRDA8'
011200000526     C                   PARM                    WLBDAT
011300000526     C     G02ERR        IFEQ      '1'
011400000526     C                   Z-ADD     0             VIDBLR
011500000526     C                   ELSE
011600000526     C                   Z-ADD     G02INV        VIDBLR
011700000526     C                   ENDIF
011800000526     C                   ENDIF
011900000526     C*
012000000526     C     VIDBCA        IFGT      0
012100000526     C                   MOVE      VIDBCA        G02DAT
012200000526     C                   MOVEL     *BLANK        G02ERR
012300000526     C                   CALL      'XSRDA8'
012400000526     C                   PARM                    WLBDAT
012500000526     C     G02ERR        IFEQ      '1'
012600000526     C                   Z-ADD     0             VIDBCA
012700000526     C                   ELSE
012800000526     C                   Z-ADD     G02INV        VIDBCA
012900000526     C                   ENDIF
013000000526     C                   ENDIF
013100000526     C*
013200000526     C     VIDPBI        IFGT      0
013300000526     C                   MOVE      VIDPBI        G02DAT
013400000526     C                   MOVEL     *BLANK        G02ERR
013500000526     C                   CALL      'XSRDA8'
013600000526     C                   PARM                    WLBDAT
013700000526     C     G02ERR        IFEQ      '1'
013800000526     C                   Z-ADD     0             VIDPBI
013900000526     C                   ELSE
014000000526     C                   Z-ADD     G02INV        VIDPBI
014100000526     C                   ENDIF
014200000526     C                   ENDIF
014300000529     C*
014400000529     C     POSTBR        IFGT      0
014500000529     C                   MOVE      POSTBR        G02DAT
014600000529     C                   MOVEL     *BLANK        G02ERR
014700000529     C                   CALL      'XSRDA8'
014800000529     C                   PARM                    WLBDAT
014900000529     C     G02ERR        IFEQ      '1'
015000000529     C                   Z-ADD     0             POSTBR
015100000529     C                   ELSE
015200000529     C                   Z-ADD     G02INV        POSTBR
015300000529     C                   ENDIF
015400000529     C                   ENDIF
015500000529     C*
015600000529     C     POSTLR        IFGT      0
015700000529     C                   MOVE      POSTLR        G02DAT
015800000529     C                   MOVEL     *BLANK        G02ERR
015900000529     C                   CALL      'XSRDA8'
016000000529     C                   PARM                    WLBDAT
016100000529     C     G02ERR        IFEQ      '1'
016200000529     C                   Z-ADD     0             POSTLR
016300000529     C                   ELSE
016400000529     C                   Z-ADD     G02INV        POSTLR
016500000529     C                   ENDIF
016600000529     C                   ENDIF
016700000529     C*
016800000529     C     POSTCA        IFGT      0
016900000529     C                   MOVE      POSTCA        G02DAT
017000000529     C                   MOVEL     *BLANK        G02ERR
017100000529     C                   CALL      'XSRDA8'
017200000529     C                   PARM                    WLBDAT
017300000529     C     G02ERR        IFEQ      '1'
017400000529     C                   Z-ADD     0             POSTCA
017500000529     C                   ELSE
017600000529     C                   Z-ADD     G02INV        POSTCA
017700000529     C                   ENDIF
017800000529     C                   ENDIF
017900000529     C*
018000000529     C     POSTBI        IFGT      0
018100000529     C                   MOVE      POSTBI        G02DAT
018200000529     C                   MOVEL     *BLANK        G02ERR
018300000529     C                   CALL      'XSRDA8'
018400000529     C                   PARM                    WLBDAT
018500000529     C     G02ERR        IFEQ      '1'
018600000529     C                   Z-ADD     0             POSTBI
018700000529     C                   ELSE
018800000529     C                   Z-ADD     G02INV        POSTBI
018900000529     C                   ENDIF
019000000529     C                   ENDIF
019100000912      *
019200000912     C     DATGDP        IFGT      0
019300000912     C                   MOVE      DATGDP        G02DAT
019400000912     C                   MOVEL     *BLANK        G02ERR
019500000912     C                   CALL      'XSRDA8'
019600000912     C                   PARM                    WLBDAT
019700000912     C     G02ERR        IFEQ      '1'
019800000912     C                   Z-ADD     0             DATGDP
019900000912     C                   ELSE
020000000912     C                   Z-ADD     G02INV        DATGDP
020100000912     C                   ENDIF
020200000912     C                   ENDIF
020300010514      *
020400040423      * DATA PULIZIA BOLLE PARTENZE CON RECLAMI
020500040423     C     DATREC        IFGT      0
020600040423     C                   MOVE      DATREC        G02DAT
020700040423     C                   MOVEL     *BLANK        G02ERR
020800040423     C                   CALL      'XSRDA8'
020900040423     C                   PARM                    WLBDAT
021000040423     C     G02ERR        IFEQ      '1'
021100040423     C                   Z-ADD     0             DATREC
021200040423     C                   ELSE
021300040423     C                   Z-ADD     G02INV        DATREC
021400040423     C                   ENDIF
021500040423     C                   ENDIF
021600930805     C*
021700000529     C* LANCIO RIORGANIZZAZIONE BOLLE PARTENZE
021800000529     C     VIDDBR        IFGT      0
021900000529     C     POSTBR        ANDGT     0
022000901218     C                   MOVEL     PARAM1        KPJBU
022100000629     C                   CALL      'FNLS61C'
022200080218      *                  -------------------
022300000629     C                   PARM                    KPJBA
022400010726      *
022500010726     C                   ENDIF
022600900406     C*
022700000000     C     FINE          TAG
022800000000     C                   SETON                                        LR
022900930804     C*---------------------------------------------------------------*
023000000526     C*                    CARTAB                                     *
023100000529     C* ROUTINE DI CARICAMENTO TABELLE PER GIORNI ARRETRAMENTO        *
023200930804     C*---------------------------------------------------------------*
023300920930     C     CARTAB        BEGSR
023400920930     C*
023500930804     C* LETTURA GIORNI DI PULIZIA
023600930614     C                   MOVEL     '5A'          COD
023700930614     C                   MOVEL     '1       '    KEY
023800930804     C     KTAB          CHAIN     TABEL00F                           30
023900930614     C  N30              MOVEL     TBLUNI        DS5A
024000930614     C   30              CLEAR                   DS5A
024100940202      *
024200940929      * LETTURA GIORNI DI PULIZIA (2° PARTE)
024300970718     C                   MOVEL     '5A'          COD
024400970718     C                   MOVEL     '2       '    KEY
024500940929     C     KTAB          CHAIN     TABEL00F                           30
024600040423     C  N30              MOVEL     TBLUNI        DS5A2
024700970718     C   30              CLEAR                   DS5A2
024800000529     C*
024900000529     C* LETTURA GIORNI DI PULIZIA (3° PARTE - POSTE)
025000000529     C                   MOVEL     '5A'          COD
025100000529     C                   MOVEL     'PT      '    KEY
025200000529     C     KTAB          CHAIN     TABEL00F                           30
025300000529     C  N30              MOVEL     TBLUNI        DS5APT
025400000529     C   30              CLEAR                   DS5APT
025500930614     C*
025600920930     C                   ENDSR
025700930804     C*---------------------------------------------------------------*
025800930804     C*                    DEFVAR                                     *
025900930804     C* ROUTINE DI DEFINIZIONE VARIABILI, KLIST E PLIST               *
026000930804     C*---------------------------------------------------------------*
026100930804     C     DEFVAR        BEGSR
026200930804     C*
026300930804     C     *ENTRY        PLIST
026400930804     C                   PARM                    KPJBA
026500000526     C                   Z-ADD     1             CODUT
026600930804     C*
026700930804     C* TABEL00F
026800930804     C     KTAB          KLIST
026900000526     C                   KFLD                    CODUT             1 0
027000940929     C                   KFLD                    COD
027100940929     C                   KFLD                    KEY
027200940929     C     *LIKE         DEFINE    TBLCOD        COD
027300940929     C     *LIKE         DEFINE    TBLKEY        KEY
027400940930     C*
027500930804     C                   ENDSR
027600940930     C*---------------------------------------------------------------*
027700940930     C*                    IMPOST                                     *
027800940930     C* IMPOSTAZIONE DELLE DATE CON CALCOLO DEI GIORNI                *
027900940930     C*---------------------------------------------------------------*
028000940930     C     IMPOST        BEGSR
028100940930     C*
028200940930     C*
028300050829     C* RICAVO IL NR. GIORNI DALLA DATA di riferimento
028400050829     C                   CALL      'FNLV61R'
028500050829     C                   PARM                    O61DRF            8 0
028600100618     C                   PARM      ' '           I61Rep            1
028700100618     C                   PARM                    dvpopuladd
028800100618
028900050829     C                   z-add     o61drf        G02inv
029000050829     C                   MOVEL     '3'           G02ERR
029100940930     C                   CALL      'XSRDA8'
029200940930     C                   PARM                    WLBDAT
029300100618     c* e ci sono aggiuntivi di pulizia da sottrarre --> tolto anche questi
029400940930     C*
029500980721     C*?FNBLP:  DATA PULIZIA BOLLE PARTENZA ITALIA ED EXPORT                   ?
029600940930     C     G02TGI        SUB       §5ABLP        GIOTGI
029700100618     C                   SUB       §vpogg        GIOTGI
029800940930     C                   CALL      'XSRGI8'
029900940930     C                   PARM                    WGIDAT
030000940930     C                   Z-ADD     GIODAT        VIDDBR
030100961204     C*
030200980721     C*?FNBLP:  DATA PULIZIA BOLLE PARTENZA IMPORT                             ?
030300970718     C     G02TGI        SUB       §5ABLI        GIOTGI
030400100618     C                   SUB       §vpogg        GIOTGI
030500961204     C                   CALL      'XSRGI8'
030600961204     C                   PARM                    WGIDAT
030700980721     C                   Z-ADD     GIODAT        VIDPBI
030800080218     C*
030900980721     C*?FNBLP:  DATA PULIZIA BOLLE PARTENZA CON RISERVA                        ?
031000980721     C     G02TGI        SUB       §5ABLR        GIOTGI
031100100618     C                   SUB       §vpogg        GIOTGI
031200980721     C                   CALL      'XSRGI8'
031300980721     C                   PARM                    WGIDAT
031400980721     C                   Z-ADD     GIODAT        VIDBLR
031500990126     C*
031600000615     C*?FNBLP:  PULIZIA BOLLA CON C.A.           (PER DTA CHIUSURA C.A)        ?
031700000615     C     G02TGI        SUB       §5ABCA        GIOTGI
031800990126     C                   CALL      'XSRGI8'
031900990126     C                   PARM                    WGIDAT
032000990126     C                   Z-ADD     GIODAT        VIDBCA
032100000529     C*
032200000529     C*?FNBLP:  DATA PULIZIA BOLLE PARTENZA - POSTE                            ?
032300000529     C     G02TGI        SUB       §5APT1        GIOTGI
032400000529     C                   CALL      'XSRGI8'
032500000529     C                   PARM                    WGIDAT
032600000529     C                   Z-ADD     GIODAT        POSTBR
032700000529     C*
032800000529     C*?FNBLP:  DATA PULIZIA BOLLE PARTENZA IMPORT - POSTE                     ?
032900000529     C     G02TGI        SUB       §5APT2        GIOTGI
033000000529     C                   CALL      'XSRGI8'
033100000529     C                   PARM                    WGIDAT
033200000529     C                   Z-ADD     GIODAT        POSTBI
033300080218     C*
033400000529     C*?FNBLP:  DATA PULIZIA BOLLE PARTENZA CON RISERVA - POSTE                ?
033500000529     C     G02TGI        SUB       §5APT3        GIOTGI
033600000529     C                   CALL      'XSRGI8'
033700000529     C                   PARM                    WGIDAT
033800000529     C                   Z-ADD     GIODAT        POSTLR
033900000529     C*
034000000529     C*? FNSTB: PULIZIA BOLLA CON C.A. - POSTE   (PER DTA CHIUSURA C.A)        ?
034100000529     C     G02TGI        SUB       §5APT4        GIOTGI
034200000529     C                   CALL      'XSRGI8'
034300000529     C                   PARM                    WGIDAT
034400000529     C                   Z-ADD     GIODAT        POSTCA
034500000912     C*
034600000912     C*? FNBLP: PULIZIA BOLLE POSTE NON PARTITE                                ?
034700000912     C     G02TGI        SUB       §5AGDP        GIOTGI
034800000912     C                   CALL      'XSRGI8'
034900000912     C                   PARM                    WGIDAT
035000000912     C                   Z-ADD     GIODAT        DATGDP
035100040423     C*
035200040423     C*? FNBLP: PULIZIA BOLLE CON RECLAMI CHIUSI (DATA CHIUSURA)
035300040423     C     G02TGI        SUB       §5AGPR        GIOTGI
035400100618     C                   SUB       §vpogg        GIOTGI
035500040423     C                   CALL      'XSRGI8'
035600040423     C                   PARM                    WGIDAT
035700040423     C                   Z-ADD     GIODAT        DATREC
035800961204     C*
035900940930     C                   ENDSR
