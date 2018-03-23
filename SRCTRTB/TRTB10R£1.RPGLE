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
005900120614
006000120614      // ----------------------------------------------------
006100120614      // - Ds esterna + prototipo per ricerca tabella su TNTBE
006200120614     d Tibs02ds      e ds
006300120614      /copy gaitrasrc/srcprotopr,tibs02r
006400120614      // ----------------------------------------------------
006500120614
006600891030     C******************************************************
006700891030     C     *ENTRY        PLIST
006800891030     C                   PARM                    KPJBA
006900891030     C                   Z-ADD     1             CODUT
007000891030     C                   CALL      'XPARUT'
007100891030     C                   PARM                    UTEDSE
007200891030     C     CODUT         CABEQ     -1            FINE
007300891030     C                   MOVEL     RAGUT         RSUT
007400020508
007500020508     ** Mi collego ad una società.
007600020508     C                   CALLB     'XSOC'
007700020508   >>C                   PARM      'SOC001'      TipXsc            6
007800020508   >>C                   PARM      *BLANK        SocXsc            3
007900020508   >>C                   PARM      *ZERO         CdsXsc            9 0
008000020508   >>C                   PARM      *BLANK        ModXsc            3
008100020508   >>C                   PARM      *BLANK        RtnXsc            1
008200020508     C                   PARM                    XSoc001DS
008300020508   >>C                   PARM                    Kpjba
008400020508
008500020508     ** Collegamento fallito.
008600020508     C                   IF        RtnXsc = '1'
008700020508     C                   EVAL      MsgErr =
008800020508     C                             'Non è possibile collegarsi alla società '
008900020508     C                             + XscSoc + '.'
009000020508     C                   EVAL      *INLR = *ON
009100020508     C                   RETURN
009200020508     C                   ENDIF
009300020508
009400891107     C*--- RICERCA CAPOCONTI------------------------------------------*
009500891107     C                   DO        50            X                 2 0
009600891107     C                   MOVE      TCU(X)        TCUDS
009700891107     C     F56           CABNE     'CG'          END0
009800891107     C     F4            COMP      '1'                                    21
009900891107     C     F4            COMP      '2'                                    22
010000891107     C     F4            COMP      '3'                                    23
010100891107     C     F4            COMP      '6'                                    27
010200891107     C** 1 CLIENTI   21
010300891107     C** 2 FORNITORI 22
010400891107     C** 3 AGENTI    23
010500891107     C     F3            COMP      '0'                                242425
010600891107     C     F3            COMP      'I'                                    26
010700891107     C** 0 ITALIA   25
010800891107     C** 1 ESTERO   24
010900891107     ** I CAPO CONTO IVA
011000891107     C   21
011100891107     CAN 24              Z-ADD     KCU(X)        KCE               4 0
011200891107     C   21
011300891107     CAN 25              Z-ADD     KCU(X)        KCI               4 0
011400891107     C   22
011500891107     CAN 24              Z-ADD     KCU(X)        KFE               4 0
011600891107     C   22
011700891107     CAN 25              Z-ADD     KCU(X)        KFI               4 0
011800891107     C   23
011900891107     CAN 24              Z-ADD     KCU(X)        KAE               4 0
012000891107     C   23
012100891107     CAN 25              Z-ADD     KCU(X)        KAI               4 0
012200891107     C   26              Z-ADD     KCU(X)        KIVA              4 0
012300891107     C   27              Z-ADD     KCU(X)        KBNA              4 0
012400891107     C     END0          TAG
012500891107     C                   END
012600891030     C******************************************************
012700891030     C* POSIZIONAMENTO TABELLE                             *
012800891107     C     KTAB          KLIST
012900891030     C                   KFLD                    §KUT
013000891030     C                   KFLD                    §COD
013100891030     C                   KFLD                    §KEY
013200020508     C     K04Riv01      KLIST
013300020508     C                   KFLD                    RIVSOCIETA
013400020508     C                   KFLD                    RIVTPREG
013500020508     C                   KFLD                    RIVALIQ
013600020508     C                   KFLD                    RIVRIFIVA
013700891030     C******************************************************
013800891030     C     *LIKE         DEFINE    TBLKUT        §KUT
013900891030     C     *LIKE         DEFINE    TBLCOD        §COD
014000891030     C     *LIKE         DEFINE    TBLKEY        §KEY
014100971210     C******************************************************
014200971210     C* Richiamo pgm per controllo modificabilità tabella
014300120614     c                   if        %subst(knmus:1:3) <> 'EDP'
014400971210     C                   CLEAR                   TRUL12
014500971210     C                   MOVEL     'L'           I12TLA
014600971210     C                   MOVEL     KNSIF         I12NSI
014700971210     C                   MOVEL     'EI'          I12COD
014800971210     C                   CALL      'TRUL12R'
014900971210     C                   PARM                    TRUL12
015000971210     C     O12ERR        IFEQ      '1'
015100971210     C                   GOTO      FINE
015200971210     C                   END
015300120614     c                   ENDIF
015400971210     C******************************************************
015500891102     C     STAR          TAG
015600891102     C                   SETOFF                                       79
015700891102     C                   MOVEL     KNMUS         PROF              3
015800891102     C     PROF          IFNE      'EDP'
015900891102     C                   SETON                                        79
016000891102     C                   END
016100891108     C                   MOVEL     *BLANKS       §DES             20
016200891107     C                   MOVEL     *BLANKS       VCOD
016300891107     C                   MOVEL     '?'           VCOD
016400891030     C     INIZIO        TAG
016500891108     C                   EXFMT     TB10D1
016600891030     C   KL              GOTO      FINE
016700891107     C     VCOD          IFEQ      *BLANKS
016800891030     C                   SETON                                        29
016900891030     C                   GOTO      INIZIO
017000891030     C                   END
017100891030     C*§  RICERCA IN TABEL00F
017200891107     C     VCOD          IFEQ      '?'
017300891030     C                   MOVEL     CODUT         §KUT
017400891108     C                   MOVEL     'EI'          §COD
017500891107     C                   MOVE      *ZEROS        VCOD
017600891030     C                   CALL      'X§TABER'
017700891030     C                   PARM                    §KUT
017800891030     C                   PARM                    §COD
017900891030     C                   PARM                    §KEY
018000891030     C                   PARM                    §DES
018100891107     C                   MOVEL     §KEY          VCOD
018200891030     C                   GOTO      INIZIO
018300891102     C                   ELSE
018400891102     C                   MOVEL     CODUT         §KUT
018500891108     C                   MOVEL     'EI'          §COD
018600891107     C                   MOVEL     VCOD          §KEY
018700891030     C                   END
018800891030     C                   SETOFF                                       0102
018900891107     C                   EXSR      PULVID
019000900604     C     KTAB          CHAIN     TABEL                              80
019100891030     C     *IN80         IFEQ      '1'
019200891030     C                   SETON                                        01
019300891030     C                   ELSE
019400891108     C                   MOVEL     TBLUNI        DSEI
019500060113      * imposto le date
019600060113     c                   If        §eidti <> *Blanks
019700060113     c                   Move      §eidti        wdata
019800060113     c                   Clear                   wlbdat
019900060113     c                   Z-add     wdata         g02inv
020000060113     c                   Movel     '3'           g02err
020100060113     c                   Call      'XSRDA8'
020200060113     c                   Parm                    wlbdat
020300060113     c                   z-add     g02dat        dataini
020400060113     c                   EndIf
020500060113     c                   If        §eidti <> *Blanks
020600060113     c                   Move      §eidtf        wdata
020700060113     c                   Clear                   wlbdat
020800060113     c                   Z-add     wdata         g02inv
020900060113     c                   Movel     '3'           g02err
021000060113     c                   Call      'XSRDA8'
021100060113     c                   Parm                    wlbdat
021200060113     c                   z-add     g02dat        datafin
021300060113     c                   EndIf
021400120614      /free
021500120614       //?Se presente codice esenzione ProJ controllo se esiste in
021600120614       //?tabella YBO
021700120614         clear vokybo;
021800120614         IF  §EIjei <> *blanks;
021900120614           clear TIBS02DS;
022000120614           T02mod = 'C';
022100120614           T02cod = 'YBO';
022200120614           T02ke1 = §EIjei;
022300120614           TNTBE_RicercaControllo (kpjba : tibs02ds);
022400120614           IF  T02err = *blanks;
022500120614             vokybo = 'Presente in tabella YBO';
022600120614           ENDIF;
022700120614         ENDIF;
022800120614      /end-free
022900060113
023000900614     C     TBLFLG        IFEQ      '*'
023100900614     C                   SETON                                        03
023200900614     C                   ELSE
023300891030     C                   SETON                                        02
023400891030     C                   END
023500900614     C                   END
023600891030     C     SU            TAG
023700891030     C                   SETOFF                                       99
023800891108     C                   EXFMT     TB10D2
023900060113     c                   Clear                   vidmsg
024000060113     c                   Setoff                                       28
024100120614     C   KL              GOTO      inizio
024200891030     C   KD              GOTO      INIZIO
024300891030     C**--- ANNULLAMENTO
024400900614     C   KQ              DO
024500900614     C                   MOVEL     '*'           TBLFLG
024600900614     C                   UPDATE    TABEL
024700900614     C                   GOTO      INIZIO
024800891030     C                   END
024900900614     C** RIPRISTINO
025000900614     C   KE              DO
025100900614     C                   MOVEL     *BLANKS       TBLFLG
025200900614     C                   UPDATE    TABEL
025300900614     C                   GOTO      INIZIO
025400900614     C                   END
025500120614     C** Aggiorna/scrive
025600120614     C   KF              DO
025700120614     C                   EXSR      CTRFMT
025800120614     C   99              GOTO      SU
025900120614     C                   EXSR      SCRIVI
026000120614     C                   GOTO      STAR
026100120614     C                   END
026200900614     C****
026300891102     C                   EXSR      CTRFMT
026400120614     C                   GOTO      SU
026500120614     C** 99              GOTO      SU
026600120614     C***                EXSR      SCRIVI
026700120614     C***                GOTO      STAR
026800891030     C     FINE          TAG
026900891030     C                   SETON                                        LR
027000891030     C**------------------------
027100891030     C** PULISCI CAMPI VIDEO
027200891030     C**------------------------
027300891030     C     PULVID        BEGSR
027400891108     C                   MOVEL     *BLANKS       §EIDES
027500891108     C                   Z-ADD     0             §EIKCC
027600891108     C                   Z-ADD     0             §EIKSC
027700060113     c                   Clear                   dataini
027800060113     c                   Clear                   datafin
027900120614     c                   clear                   vokybo
028000891030     C                   ENDSR
028100891030     C**------------------------
028200891030     C** CONTROLLI 2 VIDEATA
028300891030     C**------------------------
028400891030     C     CTRFMT        BEGSR
028500060113
028600060113     c                   Setoff                                       3031
028700020508
028800020508     ** Controllo riferimento IVA.
028900020508     C                   IF        §EIJEI = *BLANK
029000020508     C                   EVAL      *IN32 = *ON
029100020508     C                   EVAL      *IN99 = *ON
029200020508     C                   ELSE
029300020508     C                   EVAL      RivSocieta = XscSoc
029400020508     C                   EVAL      RivTpReg   = Vendite
029500020508     C                   CLEAR                   RivAliq
029600020508     C                   EVAL      RivRifIva  = §EIJEI
029700020508     C     K04Riv01      CHAIN     AnRiv01L
029800020508     C                   IF        %FOUND
029900020508     C                   IF        §EIDes = *BLANK
030000020508     C                   EVAL      §EIDes = RivDesBrev
030100020508     C                   ENDIF
030200020508     C                   ELSE
030300020508     C                   EVAL      *IN32 = *ON
030400020508     C                   EVAL      *IN99 = *ON
030500020508     C                   ENDIF
030600020508     C                   ENDIF
030700120614      /free
030800120614       //?Se presente codice esenzione ProJ controllo se esiste in
030900120614       //?tabella YBO
031000120614         clear vokybo;
031100120614         IF  §EIjei <> *blanks and not *in99;
031200120614           clear TIBS02DS;
031300120614           T02mod = 'C';
031400120614           T02cod = 'YBO';
031500120614           T02ke1 = §EIjei;
031600120614           TNTBE_RicercaControllo (kpjba : tibs02ds);
031700120614           IF  T02err = *blanks;
031800120614             vokybo = 'Presente in tabella YBO';
031900120614           ENDIF;
032000120614         ENDIF;
032100120614      /end-free
032200020508
032300891107     C**--- DESCRIZIONE
032400891108     C     §EIDES        IFEQ      *BLANKS
032500891107     C                   SETON                                        3099
032600891030     C                   GOTO      END1
032700891030     C                   END
032800060113      **--- DATE
032900060113      * data inizio validità
033000060113      *  obbligatoria
033100060113     c                   If        dataini = *Zeros
033200060113     c                   Eval      *In28 = *On
033300060113     c                   Eval      *In31 = *On
033400060113     c                   Eval      *In99 = *On
033500060113     c                   Eval      vidmsg  = msg(01)
033600060113     c                   Leavesr
033700060113     c                   EndIf
033800060113      * controllo
033900060113     c                   Clear                   wlbdat
034000060113     c                   Z-add     dataini       g02dat
034100060113     c                   Call      'XSRDA8'
034200060113     c                   Parm                    wlbdat
034300060113     c                   If        g02err = '1'
034400060113     c                   Eval      *In28 = *On
034500060113     c                   Eval      *In31 = *On
034600060113     c                   Eval      *In99 = *On
034700060113     c                   Eval      vidmsg  = msg(02)
034800060113     c                   Leavesr
034900060113     c                   EndIf
035000060113     c                   Z-add     g02dat        dataini
035100060113     c                   Z-add     g02inv        wdataini
035200060113      * data fine validità
035300060113      *  obbligatoria
035400060113     c                   If        datafin = *Zeros
035500060113     c                   Eval      *In28 = *On
035600060113     c                   Eval      *In32 = *On
035700060113     c                   Eval      *In99 = *On
035800060113     c                   Eval      vidmsg  = msg(01)
035900060113     c                   Leavesr
036000060113     c                   EndIf
036100060113      * controllo
036200060113     c                   Clear                   wlbdat
036300060113     c                   Z-add     datafin       g02dat
036400060113     c                   Call      'XSRDA8'
036500060113     c                   Parm                    wlbdat
036600060113     c                   If        g02err = '1'
036700060113     c                   Eval      *In28 = *On
036800060113     c                   Eval      *In32 = *On
036900060113     c                   Eval      *In99 = *On
037000060113     c                   Eval      vidmsg  = msg(02)
037100060113     c                   Leavesr
037200060113     c                   EndIf
037300060113     c                   Z-add     g02dat        datafin
037400060113     c                   Z-add     g02inv        wdatafin
037500060113      * la data inizio non può essere maggiore della data fine
037600060113     c                   If        wdataini > wdatafin
037700060113     c                   Eval      *In28 = *On
037800060113     c                   Eval      *In31 = *On
037900060113     c                   Eval      *In99 = *On
038000060113     c                   Eval      vidmsg  = msg(03)
038100060113     c                   Leavesr
038200060113     c                   EndIf
038300060113
038400891030     C     END1          ENDSR
038500891030     C**----------------------------
038600891030     C** AGGIORNAMENTO / INSERIMENTO
038700891030     C**----------------------------
038800891030     C     SCRIVI        BEGSR
038900891107     C     KTAB          CHAIN     TABEL                              80
039000891030     C                   MOVEL     §KUT          TBLKUT
039100891030     C                   MOVEL     §COD          TBLCOD
039200891030     C                   MOVEL     §KEY          TBLKEY
039300060113      * passo le date
039400060113     c                   Move      wdataini      §eidti
039500060113     c                   Move      wdatafin      §eidtf
039600060113
039700891108     C                   MOVEL     DSEI          TBLUNI
039800900420     C*§
039900900420     C                   MOVE      '1'           TBLFTT
040000900420     C                   MOVE      *ZERO         TBLFLT
040100900420     C                   MOVE      *BLANK        TBLFTR
040200900420     C                   MOVE      *ZERO         TBLDTR
040300900420     C*§
040400891102     C   80              WRITE     TABEL                                98
040500891102     C  N80              UPDATE    TABEL                                98
040600891030     C                   ENDSR
040700060113      *-------------------------------------------------------------------------
040800060113** MSG  Lungh. 78                                                            *
040900060113Immettere la data                                                             01
041000060113Data errata                                                                   02
041100060113Data inizio supera la data fine                                               03
