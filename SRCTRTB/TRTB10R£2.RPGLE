000100020508     H DFTACTGRP(*NO) ACTGRP(*CALLER) BNDDIR('PJXBND')
000200020508     H DATEDIT(*YMD)
000300891030     F*--------------------------------------------------------------*
000400891108     F* GESTIONE TABELLA ESENZIONI IVA
000500891030     F*----------------------* USO DEGLI INDICATORI *----------------*
000600891030     F*  01        INSERIMENTO                                       *
000700891030     F*  02        VARIAZIONE                                        *
000800060113      *  28        Visulizza messaggio di errore
000900891107     F*  29        CODICE OBBLIGATORIO                               *
001000891030     F*  30        DESCRIZIONE OBBLIGATORIA                          *
001100060113     F*  31        Data inizio validità errata                       *
001200060113     F*  32        Data fine validità errata                         *
001300891107     F*  79        COMANDO DI ANNULLAMENTO CONSENTITO SOLO DA EDP    *
001400891030     F*--------------------------------------------------------------*
001500891108     FTRTB10D   CF   E             WORKSTN
001600020508     FAnRiv01L  IF   E           K DISK
001700891030     FTABEL00F  UF A E           K DISK
001800891030     D*--------------------------------------------------------------*
001900891030     D*                        I N P U T                             *
002000891030     D*--------------------------------------------------------------*
002100060113
002200060113     d wdata           s              8  0
002300060113     d wdataini        s              8  0
002400060113     d wdatafin        s              8  0
002500060113
002600891108     D DSEI          E DS
002700891107     D IVDS            DS
002800891107     D  IVES                   4      4  0
002900971210     D TRUL12        E DS                  EXTNAME(TRUL12DS)
003000891107     D TCUDS           DS
003100891107     D  F1                     1      1
003200891107     D  F3                     3      3
003300891107     D  F2                     2      2
003400891107     D  F4                     4      4
003500891107     D  F56                    5      6
003600891030     D KPJBA         E DS
003700020508     D  MsgErr               453    502
003800891030     D UTEDSE        E DS                  EXTNAME(UTEDSE0F)
003900891107     D  TCU                  398    697
004000891107     D                                     DIM(50)
004100891107     D  KCU                  698    847P 0
004200891107     D                                     DIM(50)
004300891107     D                                     PACKEVEN
004400891107     D  DTU                  848    895P 0
004500891107     D                                     DIM(12)
004600891107     D                                     PACKEVEN
004700020508     D XSoc001DS     E DS
004800020508     D                                     INZ
004900060113
005000060113     d msg             s             78    dim(90) ctdata perrcd(1)
005100060113
005200060113     d wlbdat          ds                  inz
005300060113     d  g02dat                 1      8  0
005400060113     d  g02inv                 9     16  0
005500060113     d  g02err                17     17
005600060113     d  g02tgi                18     22  0
005700060113
005800020508     D Vendite         C                   '2'
005900891030     C******************************************************
006000891030     C     *ENTRY        PLIST
006100891030     C                   PARM                    KPJBA
006200891030     C                   Z-ADD     1             CODUT
006300891030     C                   CALL      'XPARUT'
006400891030     C                   PARM                    UTEDSE
006500891030     C     CODUT         CABEQ     -1            FINE
006600891030     C                   MOVEL     RAGUT         RSUT
006700020508
006800020508     ** Mi collego ad una società.
006900020508     C                   CALLB     'XSOC'
007000020508   >>C                   PARM      'SOC001'      TipXsc            6
007100020508   >>C                   PARM      *BLANK        SocXsc            3
007200020508   >>C                   PARM      *ZERO         CdsXsc            9 0
007300020508   >>C                   PARM      *BLANK        ModXsc            3
007400020508   >>C                   PARM      *BLANK        RtnXsc            1
007500020508     C                   PARM                    XSoc001DS
007600020508   >>C                   PARM                    Kpjba
007700020508
007800020508     ** Collegamento fallito.
007900020508     C                   IF        RtnXsc = '1'
008000020508     C                   EVAL      MsgErr =
008100020508     C                             'Non è possibile collegarsi alla società '
008200020508     C                             + XscSoc + '.'
008300020508     C                   EVAL      *INLR = *ON
008400020508     C                   RETURN
008500020508     C                   ENDIF
008600020508
008700891107     C*--- RICERCA CAPOCONTI------------------------------------------*
008800891107     C                   DO        50            X                 2 0
008900891107     C                   MOVE      TCU(X)        TCUDS
009000891107     C     F56           CABNE     'CG'          END0
009100891107     C     F4            COMP      '1'                                    21
009200891107     C     F4            COMP      '2'                                    22
009300891107     C     F4            COMP      '3'                                    23
009400891107     C     F4            COMP      '6'                                    27
009500891107     C** 1 CLIENTI   21
009600891107     C** 2 FORNITORI 22
009700891107     C** 3 AGENTI    23
009800891107     C     F3            COMP      '0'                                242425
009900891107     C     F3            COMP      'I'                                    26
010000891107     C** 0 ITALIA   25
010100891107     C** 1 ESTERO   24
010200891107     ** I CAPO CONTO IVA
010300891107     C   21
010400891107     CAN 24              Z-ADD     KCU(X)        KCE               4 0
010500891107     C   21
010600891107     CAN 25              Z-ADD     KCU(X)        KCI               4 0
010700891107     C   22
010800891107     CAN 24              Z-ADD     KCU(X)        KFE               4 0
010900891107     C   22
011000891107     CAN 25              Z-ADD     KCU(X)        KFI               4 0
011100891107     C   23
011200891107     CAN 24              Z-ADD     KCU(X)        KAE               4 0
011300891107     C   23
011400891107     CAN 25              Z-ADD     KCU(X)        KAI               4 0
011500891107     C   26              Z-ADD     KCU(X)        KIVA              4 0
011600891107     C   27              Z-ADD     KCU(X)        KBNA              4 0
011700891107     C     END0          TAG
011800891107     C                   END
011900891030     C******************************************************
012000891030     C* POSIZIONAMENTO TABELLE                             *
012100891107     C     KTAB          KLIST
012200891030     C                   KFLD                    §KUT
012300891030     C                   KFLD                    §COD
012400891030     C                   KFLD                    §KEY
012500020508     C     K04Riv01      KLIST
012600020508     C                   KFLD                    RIVSOCIETA
012700020508     C                   KFLD                    RIVTPREG
012800020508     C                   KFLD                    RIVALIQ
012900020508     C                   KFLD                    RIVRIFIVA
013000891030     C******************************************************
013100891030     C     *LIKE         DEFINE    TBLKUT        §KUT
013200891030     C     *LIKE         DEFINE    TBLCOD        §COD
013300891030     C     *LIKE         DEFINE    TBLKEY        §KEY
013400971210     C******************************************************
013500971210     C* Richiamo pgm per controllo modificabilità tabella
013600971210     C                   CLEAR                   TRUL12
013700971210     C                   MOVEL     'L'           I12TLA
013800971210     C                   MOVEL     KNSIF         I12NSI
013900971210     C                   MOVEL     'EI'          I12COD
014000971210     C                   CALL      'TRUL12R'
014100971210     C                   PARM                    TRUL12
014200971210     C     O12ERR        IFEQ      '1'
014300971210     C                   GOTO      FINE
014400971210     C                   END
014500971210     C******************************************************
014600891102     C     STAR          TAG
014700891102     C                   SETOFF                                       79
014800891102     C                   MOVEL     KNMUS         PROF              3
014900891102     C     PROF          IFNE      'EDP'
015000891102     C                   SETON                                        79
015100891102     C                   END
015200891108     C                   MOVEL     *BLANKS       §DES             20
015300891107     C                   MOVEL     *BLANKS       VCOD
015400891107     C                   MOVEL     '?'           VCOD
015500891030     C     INIZIO        TAG
015600891108     C                   EXFMT     TB10D1
015700891030     C   KL              GOTO      FINE
015800891107     C     VCOD          IFEQ      *BLANKS
015900891030     C                   SETON                                        29
016000891030     C                   GOTO      INIZIO
016100891030     C                   END
016200891030     C*§  RICERCA IN TABEL00F
016300891107     C     VCOD          IFEQ      '?'
016400891030     C                   MOVEL     CODUT         §KUT
016500891108     C                   MOVEL     'EI'          §COD
016600891107     C                   MOVE      *ZEROS        VCOD
016700891030     C                   CALL      'X§TABER'
016800891030     C                   PARM                    §KUT
016900891030     C                   PARM                    §COD
017000891030     C                   PARM                    §KEY
017100891030     C                   PARM                    §DES
017200891107     C                   MOVEL     §KEY          VCOD
017300891030     C                   GOTO      INIZIO
017400891102     C                   ELSE
017500891102     C                   MOVEL     CODUT         §KUT
017600891108     C                   MOVEL     'EI'          §COD
017700891107     C                   MOVEL     VCOD          §KEY
017800891030     C                   END
017900891030     C                   SETOFF                                       0102
018000891107     C                   EXSR      PULVID
018100900604     C     KTAB          CHAIN     TABEL                              80
018200891030     C     *IN80         IFEQ      '1'
018300891030     C                   SETON                                        01
018400891030     C                   ELSE
018500891108     C                   MOVEL     TBLUNI        DSEI
018600060113      * imposto le date
018700060113     c                   If        §eidti <> *Blanks
018800060113     c                   Move      §eidti        wdata
018900060113     c                   Clear                   wlbdat
019000060113     c                   Z-add     wdata         g02inv
019100060113     c                   Movel     '3'           g02err
019200060113     c                   Call      'XSRDA8'
019300060113     c                   Parm                    wlbdat
019400060113     c                   z-add     g02dat        dataini
019500060113     c                   EndIf
019600060113     c                   If        §eidti <> *Blanks
019700060113     c                   Move      §eidtf        wdata
019800060113     c                   Clear                   wlbdat
019900060113     c                   Z-add     wdata         g02inv
020000060113     c                   Movel     '3'           g02err
020100060113     c                   Call      'XSRDA8'
020200060113     c                   Parm                    wlbdat
020300060113     c                   z-add     g02dat        datafin
020400060113     c                   EndIf
020500060113
020600900614     C     TBLFLG        IFEQ      '*'
020700900614     C                   SETON                                        03
020800900614     C                   ELSE
020900891030     C                   SETON                                        02
021000891030     C                   END
021100900614     C                   END
021200891030     C     SU            TAG
021300891030     C                   SETOFF                                       99
021400891108     C                   EXFMT     TB10D2
021500060113     c                   Clear                   vidmsg
021600060113     c                   Setoff                                       28
021700891030     C   KL              GOTO      FINE
021800891030     C   KD              GOTO      INIZIO
021900891030     C**--- ANNULLAMENTO
022000900614     C   KQ              DO
022100900614     C                   MOVEL     '*'           TBLFLG
022200900614     C                   UPDATE    TABEL
022300900614     C                   GOTO      INIZIO
022400891030     C                   END
022500900614     C** RIPRISTINO
022600900614     C   KE              DO
022700900614     C                   MOVEL     *BLANKS       TBLFLG
022800900614     C                   UPDATE    TABEL
022900900614     C                   GOTO      INIZIO
023000900614     C                   END
023100900614     C****
023200891102     C                   EXSR      CTRFMT
023300891102     C   99              GOTO      SU
023400891030     C                   EXSR      SCRIVI
023500891102     C                   GOTO      STAR
023600891030     C     FINE          TAG
023700891030     C                   SETON                                        LR
023800891030     C**------------------------
023900891030     C** PULISCI CAMPI VIDEO
024000891030     C**------------------------
024100891030     C     PULVID        BEGSR
024200891108     C                   MOVEL     *BLANKS       §EIDES
024300891108     C                   Z-ADD     0             §EIKCC
024400891108     C                   Z-ADD     0             §EIKSC
024500060113     c                   Clear                   dataini
024600060113     c                   Clear                   datafin
024700891030     C                   ENDSR
024800891030     C**------------------------
024900891030     C** CONTROLLI 2 VIDEATA
025000891030     C**------------------------
025100891030     C     CTRFMT        BEGSR
025200060113
025300060113     c                   Setoff                                       3031
025400020508
025500020508     ** Controllo riferimento IVA.
025600020508     C                   IF        §EIJEI = *BLANK
025700020508     C                   EVAL      *IN32 = *ON
025800020508     C                   EVAL      *IN99 = *ON
025900020508     C                   ELSE
026000020508     C                   EVAL      RivSocieta = XscSoc
026100020508     C                   EVAL      RivTpReg   = Vendite
026200020508     C                   CLEAR                   RivAliq
026300020508     C                   EVAL      RivRifIva  = §EIJEI
026400020508     C     K04Riv01      CHAIN     AnRiv01L
026500020508     C                   IF        %FOUND
026600020508     C                   IF        §EIDes = *BLANK
026700020508     C                   EVAL      §EIDes = RivDesBrev
026800020508     C                   ENDIF
026900020508     C                   ELSE
027000020508     C                   EVAL      *IN32 = *ON
027100020508     C                   EVAL      *IN99 = *ON
027200020508     C                   ENDIF
027300020508     C                   ENDIF
027400020508
027500891107     C**--- DESCRIZIONE
027600891108     C     §EIDES        IFEQ      *BLANKS
027700891107     C                   SETON                                        3099
027800891030     C                   GOTO      END1
027900891030     C                   END
028000060113      **--- DATE
028100060113      * data inizio validità
028200060113      *  obbligatoria
028300060113     c                   If        dataini = *Zeros
028400060113     c                   Eval      *In28 = *On
028500060113     c                   Eval      *In31 = *On
028600060113     c                   Eval      *In99 = *On
028700060113     c                   Eval      vidmsg  = msg(01)
028800060113     c                   Leavesr
028900060113     c                   EndIf
029000060113      * controllo
029100060113     c                   Clear                   wlbdat
029200060113     c                   Z-add     dataini       g02dat
029300060113     c                   Call      'XSRDA8'
029400060113     c                   Parm                    wlbdat
029500060113     c                   If        g02err = '1'
029600060113     c                   Eval      *In28 = *On
029700060113     c                   Eval      *In31 = *On
029800060113     c                   Eval      *In99 = *On
029900060113     c                   Eval      vidmsg  = msg(02)
030000060113     c                   Leavesr
030100060113     c                   EndIf
030200060113     c                   Z-add     g02dat        dataini
030300060113     c                   Z-add     g02inv        wdataini
030400060113      * data fine validità
030500060113      *  obbligatoria
030600060113     c                   If        datafin = *Zeros
030700060113     c                   Eval      *In28 = *On
030800060113     c                   Eval      *In32 = *On
030900060113     c                   Eval      *In99 = *On
031000060113     c                   Eval      vidmsg  = msg(01)
031100060113     c                   Leavesr
031200060113     c                   EndIf
031300060113      * controllo
031400060113     c                   Clear                   wlbdat
031500060113     c                   Z-add     datafin       g02dat
031600060113     c                   Call      'XSRDA8'
031700060113     c                   Parm                    wlbdat
031800060113     c                   If        g02err = '1'
031900060113     c                   Eval      *In28 = *On
032000060113     c                   Eval      *In32 = *On
032100060113     c                   Eval      *In99 = *On
032200060113     c                   Eval      vidmsg  = msg(02)
032300060113     c                   Leavesr
032400060113     c                   EndIf
032500060113     c                   Z-add     g02dat        datafin
032600060113     c                   Z-add     g02inv        wdatafin
032700060113      * la data inizio non può essere maggiore della data fine
032800060113     c                   If        wdataini > wdatafin
032900060113     c                   Eval      *In28 = *On
033000060113     c                   Eval      *In31 = *On
033100060113     c                   Eval      *In99 = *On
033200060113     c                   Eval      vidmsg  = msg(03)
033300060113     c                   Leavesr
033400060113     c                   EndIf
033500060113
033600891030     C     END1          ENDSR
033700891030     C**----------------------------
033800891030     C** AGGIORNAMENTO / INSERIMENTO
033900891030     C**----------------------------
034000891030     C     SCRIVI        BEGSR
034100891107     C     KTAB          CHAIN     TABEL                              80
034200891030     C                   MOVEL     §KUT          TBLKUT
034300891030     C                   MOVEL     §COD          TBLCOD
034400891030     C                   MOVEL     §KEY          TBLKEY
034500060113      * passo le date
034600060113     c                   Move      wdataini      §eidti
034700060113     c                   Move      wdatafin      §eidtf
034800060113
034900891108     C                   MOVEL     DSEI          TBLUNI
035000900420     C*§
035100900420     C                   MOVE      '1'           TBLFTT
035200900420     C                   MOVE      *ZERO         TBLFLT
035300900420     C                   MOVE      *BLANK        TBLFTR
035400900420     C                   MOVE      *ZERO         TBLDTR
035500900420     C*§
035600891102     C   80              WRITE     TABEL                                98
035700891102     C  N80              UPDATE    TABEL                                98
035800891030     C                   ENDSR
035900060113      *-------------------------------------------------------------------------
036000060113** MSG  Lungh. 78                                                            *
036100060113Immettere la data                                                             01
036200060113Data errata                                                                   02
036300060113Data inizio supera la data fine                                               03
