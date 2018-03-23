000100921021     H DECEDIT('0,') DATEDIT(*DMY.)
000200990709     H* FNLV16R  *---------------------------------------------------*
000300941020     H*     CONTROLLO TASSAZIONE                                     *
000400990729     H*      INPUT:  FNLV16DS E DTASV
000500990729     H*      OUTPUT: I CAMPI PREVISIT NELLA FNLV16DS E I CODICI DI
000600990729     H*              ESENZIONE DELLE VARIE SE RICHIAMATO CON CONTROLLO
000700990729     H*              CAMPI TASSAZIONE-D17FIM='C' O 'E'
000800921021     H*--------------------------------------------------------------*
000900941019     FTABEL00F  IF   E           K DISK
001000990712     FTITA730C  IF   E           K DISK    USROPN
001100990712     FTITAS30C  IF   E           K DISK    USROPN
001200060113
001300060113     d wdata           s              8  0
001400060113
001500941021     D K78             S              1    DIM(78)
001600990730     D K10             S              1    DIM(10)
001700941021     D*
001800941021     D VRC             S              1    DIM(20)
001900941021     D*
002000150120     D SVA             S              1    DIM(100)
002100150120     D SVI             S              1    DIM(100)
002200150120     D SVE             S              1    DIM(100)
002300150120     D SVU             S              1    DIM(100)
002400150120     D SFC             S              1    DIM(100)
002500941021     D*
002600100118     D ERR             S             78    DIM(43) CTDATA PERRCD(1)
002700941021     D***
002800941021     D* PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
002900941021     D***
003000941021     D WLBDAT          DS                  INZ
003100941021     D  G02DAT                 1      8  0
003200941021     D  G02INV                 9     16  0
003300941021     D  G02ERR                17     17
003400941021     D  G02TGI                18     22  0
003500990914     D DATA            DS                  INZ
003600990914     D  WANNO                  1      4
003700990914     D  WMESE                  5      6
003800990914     D  WGIOR                  7      8
003900940218      *
004000990709     D* DS PER FNLV16R - DS DEI CAMPI DI TASSAZIONE
004100990709     D DSLV16        E DS                  EXTNAME(FNLV16DS)
004200990712     D DTASV         E DS
004300941108     D  SVR                    1     20
004400941021     D                                     DIM(20)
004500990712     D  VAR                   21    140P 3
004600990712     D                                     DIM(20)
004700990712     D  ESV                  141    160
004800941021     D                                     DIM(20)
004900941021     D*
005000020725     D DYIV          E DS
005100941021     D DSCC          E DS
005200990714     D DSCV          E DS
005300941117     D DS$2          E DS
005400100118     D DS4I          E DS
005500941021     D DSQT1         E DS
005600170717     D** DSQC2         E DS
005700020725     D TIBS02DS      E DS                  inz
005800941021     D KPJBA         E DS
005900020415     D DLV1601       E DS
006000020415     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
006100941021     D  DTU                  848    895P 0
006200941021     D                                     DIM(12)
006300941021     D                                     PACKEVEN
006400060113     d dsei          e ds
006500110623     d dstb          e ds
006600020725      *
006700020725      * Costanti
006800020726      * - Società chiave per tabella "YIV"
006900020725     D C_Soc           c                   const('201')
007000921021     C*---------------------------------------------------------------*
007100921021     C     *ENTRY        PLIST
007200990709     C                   PARM                    DSLV16
007300990714     C                   PARM                    DTASV
007400000000     C*---------------------------------------------------------------*
007500941124     C* TIPO LANCIO, D17TLA "C" -           CHIUSO CON LR
007600941124     C* TIPO LANCIO, D17TLA "L" - ELABORO E CHIUDO CON LR
007700941124     C* TIPO LANCIO, D17TLA " " - ELABORO E CHIUDO IN RETRN
007800990729    0C     D17TLA        IFNE      'C'
007900940921     C*
008000990709     C                   CLEAR                   O17MSG
008100990709     C                   CLEAR                   O17ERR
008200990709     C                   CLEAR                   O17ERV
008300990709     C                   CLEAR                   O17ERF
008400990709     C                   CLEAR                   O17DEI
008500990709     C                   CLEAR                   O17DFA
008600990709     C                   CLEAR                   O17VAI
008700990709     C                   CLEAR                   O17VAF
008800990714     C                   CLEAR                   O17IFT
008900990709     C                   CLEAR                   O17FDC
009000941021     C                   CLEAR                   ESV
009100990709     C                   CLEAR                   O17IMV
009200941111     C                   CLEAR                   VRC
009300950407     C                   SETOFF                                       900102
009400990914     C                   SETOFF                                       03
009500990729     C* SE NON E' STATO PASSATO IL TIPO DI ELABORAZIONE CHE SI VUOLE
009600990729     C*  ERRORE
009700990729    1C     D17FIM        IFEQ      ' '
009800990729     C                   MOVEL     ERR(28)       O17MSG
009900990729     C                   MOVEL     'E'           O17ERR
010000990729    1C                   ENDIF
010100990729     C**
010200990729    1C     O17ERR        IFEQ      ' '
010300990729     C**
010400990712     C* SE DEVO, CARICO I DATI TASSAZIONE
010500990729    2C     D17CRB        IFNE      ' '
010600990712     C                   EXSR      CARDAT
010700990729    2C                   ENDIF
010800941021     C*
010900990729    2C     O17ERR        IFEQ      ' '
011000941124     C                   MOVEL     D17TBL        FLGTB1            1
011100990714     C                   MOVE      D17KSC        TASCC4            4 0
011200110623
011300110623     C                   MOVEL     'TB'          COD
011400110623     C                   MOVEL(p)  D17TBL        key
011500110623     C     KTAB          CHAIN     TABEL                              30
011600110623     c                   if        %found(tabel00f)
011700110623     c                   movel     tbluni        dsTB
011800110623     c                   else
011900110623     c                   clear                   dstb
012000110623     c                   endif
012100990709     C** SOLO TOTALE IMPONIBILE
012200990729    3C     D17FIM        IFEQ      'T'
012300990712     C** SOLO TOTALE FATTURA
012400990729     C     D17FIM        OREQ      'F'
012500990709     C                   EXSR      CALIMV
012600990712     C                   ELSE
012700990928     C*
012800990928     C                   CLEAR                   WTOT
012900990928     C     D17FIM        IFEQ      'C'
013000990928     C     D17FIM        OREQ      'E'
013100990928     C                   MOVEL     'S'           WTOT              1
013200990928     C                   ENDIF
013300941021     C*
013400941021     C* CONTROLLI DATI TASSAZIONE BOLLA
013500941021     C                   EXSR      CTRTAS
013600990729    3C                   ENDIF
013700990729    2C                   ENDIF
013800990729    1C                   ENDIF
013900990729     C**
014000990729    0C                   ENDIF
014100941021     C* CHIUSURA PGM
014200990729    0C     D17TLA        IFEQ      ' '
014300941021     C                   RETURN
014400941021     C                   ELSE
014500941021     C                   SETON                                        LR
014600990729    0C                   ENDIF
014700941021     C*
014800990712     C* CARICAMENTO DATI TASSAZIONE ---------------------------------*
014900990712     C     CARDAT        BEGSR
015000990729     C* BOLLE DI SEDE
015100990712    1C     D17TBO        IFEQ      '0'
015200990729    3C     D17CRB        IFEQ      'S'
015300990729     C                   CLEAR                   DTASV
015400990729     C                   ENDIF
015500990729     C*
015600990729     C** CONTROLLO SE ESISTE LA SPEDIZIONE
015700990712     C                   OPEN      TITAS30C
015800990712     C     KBOL          CHAIN     TITAS30C                           30
015900990729     C*
016000990729    2C     *IN30         IFEQ      *ON
016100990714     C                   MOVEL     ERR(25)       O17MSG
016200990714     C                   MOVEL     'E'           O17ERR
016300990729   X2C                   ELSE
016400990729     C* SE DEVO CARICARE I DATI DALLA BOLLA --> OK
016500990729    3C     D17CRB        IFEQ      'S'
016600990712     C                   Z-ADD     TASPOR        D17POR
016700990714     C                   MOVEL     TASDIV        D17DIV
016800990712    3C                   ENDIF
016900990729     C*
017000990712     C                   CLOSE     TITAS30C
017100990719     C* PULISCO LE VARIE DALLA 4 IN POI
017200990825     C                   Z-ADD     4             YY                3 0
017300990719     C**
017400990825    3C     YY            DOWLE     20
017500990825     C                   MOVEL     ' '           SVR(YY)
017600990825     C                   Z-ADD     0             VAR(YY)
017700990825     C                   ADD       1             YY
017800990719    3C                   ENDDO
017900990712     C**
018000990729    3C     TASSV3        IFNE      ' '
018100990719     C**
018200990825     C                   Z-ADD     4             YY                3 0
018300990712     C                   OPEN      TITA730C
018400990712     C     KBOL          CHAIN     TITA730C                           30
018500990729    4C     *IN30         DOWEQ     *OFF
018600990825     C     YY            ANDLE     20
018700990825     C                   MOVEL     TA7SVN        SVR(YY)
018800990825     C                   Z-ADD     TA7VAN        VAR(YY)
018900990825     C                   ADD       1             YY
019000990712     C     KBOL          READE     TITA730C                               30
019100990729    4C                   ENDDO
019200990719     C                   CLOSE     TITA730C
019300990729    3C                   ENDIF
019400990712     C**
019500990729    2C                   ENDIF
019600990712    1C                   ENDIF
019700990714     C                   ENDSR
019800990712     C*
019900990712     C* CALCOLO SOLO IL TOTALE IMPONIBILE ---------------------------*
020000990712     C     CALIMV        BEGSR
020100990709     C*** VARIE ***
020200990709     C                   Z-ADD     1             X                 3 0
020300990709     C*
020400990709    1C                   DO        20            X
020500990709     C* SOLO SE RIEMPITA
020600990709    2C     SVR(X)        IFNE      *BLANKS
020700990709     C     VAR(X)        ORNE      0
020800990709     C*
020900990825     C                   Z-ADD     1             YY                3 0
021000990825     C     SVR(X)        LOOKUP    SVA(YY)                                30
021100990709    3C     *IN30         IFEQ      *OFF
021200990825     C     SVI(YY)       OREQ      'S'
021300990709     C                   ADD       VAR(X)        O17IMV
021400990709     C                   ADD       VAR(X)        O17VAI
021500990709     C                   ELSE
021600990709     C                   ADD       VAR(X)        O17VAF
021700990709    3C                   ENDIF
021800990712     C**
021900990825     C                   MOVEL     SVE(YY)       ESV(X)
022000990712     C* TOTALE FATTURA
022100990712     C     D17FIM        IFEQ      'F'
022200990712     C                   ADD       VAR(X)        O17IFT
022300990712     C                   ENDIF
022400990709     C*
022500990709     C                   ENDIF
022600990709    1C                   ENDDO
022700990709     C                   ADD       D17POR        O17IMV
022800990712     C* TOTALE FATTURA
022900990712     C     D17FIM        IFEQ      'F'
023000990712     C                   ADD       D17POR        O17IFT
023100990712     C                   ENDIF
023200990709     C                   ENDSR
023300990709     C*
023400990709     C* CONTROLLO TASSAZIONE BOLLA ----------------------------------*
023500990709     C     CTRTAS        BEGSR
023600020415     c                   movel     d17flo        dlv1601
023700941020     C***
023800941020     C*** ESENZIONE IVA ***
023900990928    0C     D17FIM        IFEQ      'I'
024000990928     C     WTOT          OREQ      'S'
024100990928     C*
024200941124    1C     D17CEI        IFNE      ' '
024300941020     C                   MOVEL     'EI'          COD
024400941020     C* RICERCA
024500941124    2C     D17CEI        IFEQ      '?'
024600941020     C                   CALL      'X§TABER'
024700941020     C                   PARM                    CODUT
024800941020     C                   PARM                    COD
024900941020     C                   PARM                    §KEY
025000941020     C                   PARM                    §DES             25
025100941124     C                   MOVEL     §KEY          D17CEI
025200990714     C                   MOVEL     §DES          O17DEI
025300990714     C                   MOVEL     '1'           O17ERR
025400941020     C*
025500941020   X2C                   ELSE
025600941020     C* CONTROLLO
025700941124     C                   MOVEL(P)  D17CEI        KEY
025800941020     C     KTAB          CHAIN     TABEL                              30
025900941020     C* ERRORE
026000941020    3C     *IN30         IFEQ      *ON
026100941020     C     *IN30         OREQ      *OFF
026200941020     C     TBLFLG        ANDNE     ' '
026300990714     C                   MOVEL     ERR(1)        O17MSG
026400990714     C                   MOVEL     '1'           O17ERR
026500941021     C                   GOTO      ENDCTA
026600941020    3C                   ENDIF
026700941020     C*
026800060113     C**!!!              MOVEL     TBLUNI        O17DEI
026900060113     c                   Movel     tbluni        dsei
027000060113     c                   Movel     §eides        o17dei
027100060113      * controllo se la data è nel range
027200060113     c                   If        §eidti <> *Blanks
027300060113     c                   Move      §eidti        wdata
027400060113     c                   If        d17dsp < wdata
027500060113     c                   Movea     err(38)       k78
027600060113     c                   Movel     §eidti        data
027700060113     c                   Movea     wanno         k78(46)
027800060113     c                   Movea     wmese         k78(43)
027900060113     c                   Movea     wgior         k78(40)
028000060113     c                   Movea     k78           o17msg
028100060113     c                   Eval      o17err = '1'
028200060113     c                   Leavesr
028300060113     c                   EndIf
028400060113     c                   EndIf
028500060113     c                   If        §eidtf <> *Blanks
028600060113     c                   Move      §eidtf        wdata
028700060113     c                   If        d17dsp > wdata
028800060113     c                   Movea     err(39)       k78
028900060113     c                   Movel     §eidtf        data
029000060113     c                   Movea     wanno         k78(50)
029100060113     c                   Movea     wmese         k78(47)
029200060113     c                   Movea     wgior         k78(44)
029300060113     c                   Movea     k78           o17msg
029400060113     c                   Eval      o17err = '1'
029500060113     c                   Leavesr
029600060113     c                   EndIf
029700060113     c                   EndIf
029800100115     c* se richiesto attivo controllo partita iva con codice esenzione
029900100115     c                   if        §d17ctriso='S'
030000100115     c                   exsr      CtrCEIISO
030100100115     c                   endif
030200100115     c
030300941020    2C                   ENDIF
030400941020    1C                   ENDIF
030500991006    0C                   ENDIF
030600990714     C***
030700990714     C*** DIVISA  ***
030800990928    0C     D17FIM        IFEQ      'D'
030900990928     C     WTOT          OREQ      'S'
031000990928     C*
031100990714     C                   MOVEL     'CV'          COD
031200990714     C* RICERCA
031300991006     C     '?'           SCAN      D17DIV                                 30
031400991006    1C     *IN30         IFEQ      *ON
031500990714     C                   CALL      'X§TABER'
031600990714     C                   PARM                    CODUT
031700990714     C                   PARM                    COD
031800990714     C                   PARM                    §KEY
031900990714     C                   PARM                    §DES             25
032000990714     C                   MOVEL     §KEY          D17DIV
032100990714     C                   MOVEL     §DES          O17DDI
032200990714     C                   MOVEL     '7'           O17ERR
032300990914    1C                   ENDIF
032400990714     C*
032500990714     C* CONTROLLO
032600990914    1C     D17DIV        IFNE      *BLANKS
032700990714     C                   MOVEL(P)  D17DIV        KEY
032800990714     C     KTAB          CHAIN     TABEL                              30
032900990714     C* ERRORE
033000990914    2C     *IN30         IFEQ      *ON
033100990714     C     *IN30         OREQ      *OFF
033200990714     C     TBLFLG        ANDNE     ' '
033300990714     C                   MOVEL     ERR(26)       O17MSG
033400990714     C                   MOVEL     '7'           O17ERR
033500990714     C                   GOTO      ENDCTA
033600990914    2C                   ENDIF
033700990714     C*
033800990714     C                   MOVEL     TBLUNI        DSCV
033900990714     C                   MOVEL     §CVDES        O17DDI
034000990714     C                   MOVEL     §CVFDC        O17FDC
034100990914     C* VEDO SE DIVISA VALIDA
034200990914    2C     §CVABI        IFEQ      ' '
034300990914     C                   MOVEL     ERR(26)       O17MSG
034400990914     C                   MOVEL     '7'           O17ERR
034500990914     C                   GOTO      ENDCTA
034600990914   X2C                   ELSE
034700990914     C* CONTROLLO CON LE DATE
034800990914    3C     D17DSP        IFGT      0
034900990914    4C     D17DSP        IFLT      §CVVDD
035000990914     C                   MOVEA     ERR(29)       K78
035100990914     C                   MOVEL     §CVVDD        DATA
035200990914     C                   MOVEA     WANNO         K78(38)
035300990914     C                   MOVEA     WMESE         K78(35)
035400990914     C                   MOVEA     WGIOR         K78(32)
035500990914     C                   MOVEA     K78           O17MSG
035600990914     C                   MOVEL     '7'           O17ERR
035700990914     C                   GOTO      ENDCTA
035800990914    4C                   ENDIF
035900990914     C*
036000990914    4C     D17DSP        IFGT      §CVVFD
036100990914     C                   MOVEA     ERR(30)       K78
036200990914     C                   MOVEL     §CVVFD        DATA
036300990914     C                   MOVEA     WANNO         K78(42)
036400990914     C                   MOVEA     WMESE         K78(39)
036500990914     C                   MOVEA     WGIOR         K78(36)
036600990914     C                   MOVEA     K78           O17MSG
036700990914     C                   MOVEL     '7'           O17ERR
036800990914     C                   GOTO      ENDCTA
036900990914    4C                   ENDIF
037000990914    3C                   ENDIF
037100990914    2C                   ENDIF
037200990914    1C                   ENDIF
037300990928    0C                   ENDIF
037400941124     c**
037500991006     C** DA QUI IN POI PROCEDO SOLTANTO SE CONTROLLO TASSAZIONE GLOBALE
037600991006    0C     WTOT          IFEQ      'S'
037700991006     C**
037800941124     C*** TASSAZIONE CON TARIFFA MITTENTE: BOLLA TASSATA FINO ALL'IMPON
037900950228     C* SOLO PER ASSEGNATI
038000941124     C     D17FTM        IFEQ      'S'
038100941124     C     D17IMV        ANDEQ     0
038200950228     C     FLGTB1        ANDEQ     'A'
038300990714     C                   MOVEL     ERR(22)       O17MSG
038400990714     C                   MOVEL     '5'           O17ERR
038500941124     C                   GOTO      ENDCTA
038600941124    3C                   ENDIF
038700941020     C***
038800941020     C*** VARIE ***
038900941021     C                   Z-ADD     1             X                 3 0
039000941020     C*
039100941021    1C                   DO        20            X
039200941020     C* SOLO SE RIEMPITA
039300941021    2C     SVR(X)        IFNE      *BLANKS
039400941021     C     VAR(X)        ORNE      0
039500941020     C*
039600941020     C* CONTROLLO SIGLA VARIA
039700941020     C                   MOVEL     'CC'          COD
039800941020     C* RICERCA
039900941021    3C     SVR(X)        IFEQ      '?'
040000941021     C                   CALL      'X§TABER'
040100941021     C                   PARM                    CODUT
040200941021     C                   PARM                    COD
040300941021     C                   PARM                    §KEY
040400941021     C                   PARM                    §DES             25
040500941021     C                   MOVE      §KEY          SVR(X)
040600990714     C                   MOVEL     '6'           O17ERR
040700990714     C                   Z-ADD     X             O17ERV
040800990831     C                   SETON                                        90
040900941021    3C                   ELSE
041000941020     C* CONTROLLO
041100990825     C                   Z-ADD     1             YY
041200990825     C     SVR(X)        LOOKUP    SVA(YY)                                30
041300990709     C* ERRORE: INESISTENTE
041400990709    4C     *IN30         IFEQ      *OFF
041500990709     C*         O NON UTILIZZABILE IN TASSAZIONE
041600990825     C     SVU(YY)       ORNE      'S'
041700990714     C                   MOVEL     ERR(13)       O17MSG
041800990714     C                   MOVEL     '6'           O17ERR
041900941215     C                   SETON                                        90
042000941021    4C                   ENDIF
042100941021    3C                   ENDIF
042200941020     C*
042300941020     C* CONTROLLO SE PER CASO NON L'HO GIA' MEMORIZZATA --> ERRORE
042400941021     C     SVR(X)        LOOKUP    VRC                                    30
042500941215     C  N90*IN30         IFEQ      *ON
042600950116     C     SVR(X)        ANDNE     *BLANKS
042700990714     C                   MOVEL     ERR(14)       O17MSG
042800990714     C                   MOVEL     '6'           O17ERR
042900941021     C                   SETON                                        90
043000941020    3C                   ENDIF
043100020415     C* SE TIPO BOLLA CHE PREVEDE UNA VARIA, ERRORE SE IMMESSA UNA VARIA
043200030603     C*  DIVERSA DA QUELLA PREVISTA SE FLAG DELLA TABELLA CC UGUALE A BLANK
043300030603     C*
043400041202     C  n90              IF        §D17SVA<>*BLANKS  AND SVR(X)<>§D17SVA
043500041202     C                             AND SFC(YY)=*BLANKS
043600020415     C                   MOVEL     ERR(35)       O17MSG
043700020415     C                   MOVEL     '6'           O17ERR
043800020415     C                   SETON                                        90
043900020415    3C                   ENDIF
044000941020     C*
044100941020     C* PER COD 8888 O 9999 CON VARIA "R" OBBLIGATORIO IMPORTO DA ASSIC
044200990416    3C  N90SVR(X)        IFEQ      §$2VRR
044300990714    4C     TASCC4        IFEQ      9999
044400990714     C     TASCC4        OREQ      8888
044500941020     C*
044600941020     C* SE = 0 E FORZO CON ENTER PRENDO VALORE DA TARIFFA
044700941124    5C     D17IAS        IFEQ      0
044800990714     C                   MOVEL     'S'           O17FAS
044900941020    5C                   ENDIF
045000941020    4C                   ENDIF
045100941020    3C                   ENDIF
045200941020     C*
045300941020     C* VARIA G NON AMMESSA SE LA BOLLA NON PREVEDE IL C/A
045400960102     C* AMMESSA SEMPRE PER LE BOLLE DI RECUPERO
045500110623     C*  non controllo se bolla storno o C/servizio
045600941124    3C  N90D17FCA        IFEQ      ' '
045700960102    3C     D17RBL        ANDNE     'R'
045800110623     c     §tbfcb        andeq     '1'
045900990714     C     SVR(X)        ANDEQ     §$2VRG
046000990714     C                   MOVEL     ERR(15)       O17MSG
046100990714     C                   MOVEL     '6'           O17ERR
046200941021     C                   SETON                                        90
046300941021    3C                   ENDIF
046400040115      * se sto controllando la seconda bolla posso inserire solo la varia 'G'
046500040115      * + le varie che hanno 'S' in §ccflo
046600040115    3c  n90              If        §d17tb2 <> *Blanks and
046700040115     c                             svr(x) <> §$2Vrg and Sfc(yy) = *Blanks
046800040129     c                   Eval      O17Msg = Err(35)
046900040115     c                   Eval      O17Err = '6'
047000040115     c                   Eval      *In90 = *On
047100040115    3c                   EndIf
047200040119      * se sto controllando la seconda bolla e c'è la varia 'G' questa non può
047300040119      * essere negata
047400040119    3c  n90              If        §d17tb2 <> *Blanks and
047500040119     c                             svr(x) =  §$2Vrg and var(x) = *Zeros
047600040129     c                   Eval      O17Msg = Err(37)
047700040119     c                   Eval      O17Err = '6'
047800040119     c                   Eval      *In90 = *On
047900040119    3c                   EndIf
048000941020     C*
048100941020     C* SE L'IMPORTO DELLA VARIA E' NEGATIVO ERRORE
048200990714    3C  N90VAR(X)        IFLT      0
048300990714     C                   MOVEL     ERR(16)       O17MSG
048400990714     C                   MOVEL     '6'           O17ERR
048500941021     C                   SETON                                        90
048600941021    3C                   ENDIF
048700990914     C* SE C'E' ALMENO UN IMPORTO DI UNA VARIA, CI DEVE ESSERE DIVISA
048800990914    3C  N90VAR(X)        IFGT      0
048900990914    4C     D17DIV        IFEQ      *BLANKS
049000990914     C                   MOVEL     ERR(31)       O17MSG
049100990914     C                   MOVEL     '7'           O17ERR
049200990914     C                   SETON                                        90
049300990914     C                   ELSE
049400990914     C* C'E' ALMENO UN IMPORTO DI TASSAZIONE  --> ACCENDO 03
049500990914     C                   SETON                                        03
049600990914    4C                   ENDIF
049700990914    3C                   ENDIF
049800990714     C* CONTROLLO I DECIMALI
049900990914    3C  N90D17DVP        IFEQ      D17DIV
050000990914     C     D17DVP        OREQ      *BLANKS
050100990914     C                   MOVE      VAR(X)        W003A             3
050200990914    4C     W003A         IFNE      '000'
050300990714     C     O17FDC        ANDEQ     ' '
050400990714     C                   MOVEL     ERR(27)       O17MSG
050500990714     C                   MOVEL     '6'           O17ERR
050600990714     C                   SETON                                        90
050700990914    4C                   ENDIF
050800991115     C*
050900991115     C* SE VARIA ESENTE, SOLO 2 DECIMALI
051000991115    4C     SVE(YY)       IFNE      *BLANKS
051100991115     C     O17FDC        ANDEQ     'S'
051200991115     C                   EXSR      CTRDEC
051300991115    4C                   ENDIF
051400991115    3C                   ENDIF
051500941020     C*
051600941020     C* MEMORIZZA L'EVENTUALE CODICE DI ESENZIONE DELLA VARIA
051700941020    3C     *IN90         IFEQ      *OFF
051800950116     C     SVR(X)        ANDNE     *BLANKS
051900990825     C                   MOVEL     SVE(YY)       ESV(X)
052000941021     C                   MOVEL     SVR(X)        VRC(X)
052100941124     C*
052200941124     C*
052300941020     C* SE IMMESSO IL TOTALE IMPONIBILE CONTROLLO SE E' CORRETTO
052400990709     C* SOMMO SOLO SE FA PARTE DEL TOT IMPONIBILE
052500990825    4C     SVI(YY)       IFEQ      'S'
052600990714     C* TROVATA UNA VARIA FACENTE PARTE DEL TOT.IMP --> ACCENDO LO 01
052700990714     C                   SETON                                        01
052800990714     C* TROVATO UN IMPORTO VARIA DIVERSO DA 0 --> ACCENDO LO 02
052900990914    5C     VAR(X)        IFNE      *ZEROS
053000990714     C                   SETON                                        02
053100990914    5C                   ENDIF
053200990714     C***
053300990709     C                   ADD       VAR(X)        O17IMV
053400990914     C                   ADD       VAR(X)        O17VAI
053500990914     C                   ELSE
053600990914     C                   ADD       VAR(X)        O17VAF
053700941020    4C                   ENDIF
053800990914    3C                   ENDIF
053900941020    2C                   ENDIF
054000941020     C*
054100991028     C   90O17ERR        IFEQ      '6'
054200991028     C                   Z-ADD     X             O17ERV
054300991028     C                   ENDIF
054400941215     C   90              Z-ADD     21            X
054500941020    1C                   ENDDO
054600941020     C* ERRORE
054700941021     C   90              GOTO      ENDCTA
054800990714     C* CONTROLLO PORTO
054900990914    1C     D17POR        IFGT      0
055000020415     c* Non ammesso se tipo bolla che prevede una sola varia
055100020415     c                   if        §d17sva<>*blanks
055200020415     C                   MOVEL     ERR(36)       O17MSG
055300020415     C                   MOVEL     '8'           O17ERR
055400020415     C                   GOTO      ENDCTA
055500020415     c                   endif
055600040129      * Non ammesso se estensione bolla
055700040129     c                   if        §d17tb2 <> *blanks
055800040129     C                   MOVEL     ERR(36)       O17MSG
055900040129     C                   MOVEL     '8'           O17ERR
056000040129     C                   GOTO      ENDCTA
056100040129     c                   endif
056200990914     C*
056300990914     C     D17DIV        IFEQ      *BLANKS
056400990914     C                   MOVEL     ERR(31)       O17MSG
056500990914     C                   MOVEL     '7'           O17ERR
056600990914     C                   GOTO      ENDCTA
056700990914     C                   ENDIF
056800990914     C*
056900990914    2C     D17DVP        IFEQ      D17DIV
057000990914     C     D17DVP        OREQ      *BLANKS
057100990914     C                   MOVE      D17POR        W003A             3
057200990914    3C     W003A         IFNE      '000'
057300990714     C     O17FDC        ANDEQ     ' '
057400990714     C                   MOVEL     ERR(27)       O17MSG
057500990714     C                   MOVEL     '8'           O17ERR
057600990914     C                   GOTO      ENDCTA
057700990914    3C                   ENDIF
057800990914    2C                   ENDIF
057900990914    1C                   ENDIF
058000990914     c**
058100990914     C* se non ci sono importi di varie o porto non si puo' immettere
058200990914     c*  la DIVISA
058300990914    1C     *IN03         IFEQ      *OFF
058400990914     C     D17POR        ANDEQ     0
058500990914     C     D17DIV        ANDNE     *BLANKS
058600990914     C                   MOVEL     ERR(32)       O17MSG
058700990914     C                   MOVEL     '7'           O17ERR
058800990914     C                   GOTO      ENDCTA
058900990914    1C                   ENDIF
059000990914     C**
059100941020     C*
059200941020     C* VARIA "G" OBBLIGATORIA SE C/ASSEGNO SU 1 BOLLA
059300990709     C* SE IMMESSO IL TOTALE IMPONIBILE O LO VOGLIO IN OUTPUT
059400991214     C* NON LO CHIEDO OBBLIGATORIO PER LE BOLLE DI RECUPERO
059500941124    1C     D17FCA        IFEQ      'S'
059600991214     C     D17RBL        ANDNE     'R'
059700110623     c     §tbfcb        andeq     '1'
059800991214     C*
059900990709    2C     D17IMV        IFNE      0
060000990709     C     D17FIM        OREQ      'E'
060100941117     C     §$2VRG        LOOKUP    VRC                                    30
060200941020     C*
060300990709    3C     *IN30         IFEQ      *OFF
060400990714     C                   MOVEL     ERR(18)       O17MSG
060500990714     C                   MOVEL     '6'           O17ERR
060600990714     C                   Z-ADD     1             O17ERV
060700941021     C                   GOTO      ENDCTA
060800990709    3C                   ENDIF
060900990709    2C                   ENDIF
061000941020    1C                   ENDIF
061100040127
061200040127      * Varia "G" obbligatoria se sto controllando una seconda bolla in sede
061300040127if  1c                   If        D17Tbo = '0' and §d17tb2 <> *Blanks
061400040127     c     §$2Vrg        Lookup    vrc                                    30
061500040127if  2c                   If        Not *In30
061600040127     c                   Eval      O17Msg = err(18)
061700040127     c                   Eval      O17Err = '6'
061800040127     c                   Z-add     1             O17Erv
061900040127     c                   GoTo      EndCta
062000040127    2c                   EndIf
062100040127    1c                   EndIf
062200941021     C**
062300941124     C*
062400941124     C* SE C'E' LA DATA FATTURA E NON E' UN C/S DEVO AVERE ALMENO UN
062500941124     C*  CAMPO DELLA TASSAZIONE
062600990825     C     D17PRP        IFNE      'S'
062700990701     C     D17DFG        ANDGT     0
062800941124     C     D17RBL        ANDNE     'C'
062900941124     C*
063000941124     C     D17POR        ANDEQ     0
063100950407     C     *IN02         ANDEQ     *OFF
063200990714     C                   MOVEL     ERR(21)       O17MSG
063300990714     C                   MOVEL     '5'           O17ERR
063400941124     C                   GOTO      ENDCTA
063500941124     C                   ENDIF
063600941021     c*
063700941021     C* CALCOLO IL TOTALE
063800990709     C                   ADD       D17POR        O17IMV
063900941117     C*
064000941124     C* SE IMPONIBILE DA CONTROLLARE, E C'E' DATA FATTURA
064100990709     C*  E O17IMV <> 0 --> TOTALE IMPONIBILE OBBLIGATORIO
064200990825     C*  SOLO IN SEDE  O PER PREPAGATO NON AUTOMATICI
064300990825     C     D17PRP        IFNE      'S'
064400990826     C     D17TBO        OREQ      '0'
064500990826     C     D17DFG        IFGT      0
064600990709     C     O17IMV        ANDGT     0
064700941124     C     D17IMV        ANDEQ     0
064800990714     C                   MOVEL     ERR(7)        O17MSG
064900990714     C                   MOVEL     '5'           O17ERR
065000941117     C                   GOTO      ENDCTA
065100941117    2C                   ENDIF
065200990826    2C                   ENDIF
065300941021     C* e' da controllare
065400990714    1C     D17FIM        IFNE      'E'
065500941124     C     D17IMV        ANDNE     0
065600990709     C     D17IMV        ANDNE     O17IMV
065700941020     C*
065800941020     C* SE ERRATO L'IMPONIBILE VISUALIZZO A VIDEO L'IMPORTO CORRETTO
065900941020     C* MUOVO LA SKIERA DELL ERRORE
066000941021     C                   MOVEA     ERR(20)       K78
066100941020     C*
066200941020     C* SOSTITUISCO CON " " GLI 0 NON SIGNIFICATIVI
066300990709    2C     O17IMV        IFGT      0
066400990730     C                   MOVEL     O17IMV        W010A            10
066500990730     C                   MOVE      O17IMV        W003A             3
066600990730     C                   MOVEA     W010A         K10
066700941020     C                   Z-ADD     1             X
066800941020     C*
066900991025     C     X             DOWLE     10
067000991025     C     K10(X)        ANDEQ     '0'
067100990730     C                   MOVEL     ' '           K10(X)
067200941020     C                   ADD       1             X
067300941020    3C                   ENDDO
067400941020     C*
067500941020   X2C                   ELSE
067600990730     C                   CLEAR                   K10
067700990730     C                   MOVE      '0'           K10(10)
067800941020    2C                   ENDIF
067900941021     C*
068000941020     C* IMPOSTO L'IMPONIBILE CORRETTO
068100990730     C                   MOVEA     K10           K78(55)
068200990730     C                   MOVEA     W003A         K78(66)
068300990714     C                   MOVEA     K78           O17MSG
068400990714     C                   MOVEL     '5'           O17ERR
068500941021     C                   GOTO      ENDCTA
068600941021    1C                   END
068700941201     C***
068800941201     C*** DATI FATTURA ***
068900941201     C* IN PARTENZA
069000941201     C* SE C/S O ASSEGNATO NON AMMESSO NUMERO E DATA FATTURA
069100941201    1C     D17TBO        IFEQ      'P'
069200941201     C*
069300991022     C     FLGTB1        IFEQ      'A'
069400941201     C*
069500941201    3C     D17NFT        IFNE      0
069600941201    3C     D17DFG        ORNE      0
069700941201    3C     D17FIV        ORNE      0
069800990714     C                   MOVEL     ERR(2)        O17MSG
069900990714     C                   MOVEL     '2'           O17ERR
070000941201     C                   GOTO      ENDCTA
070100941201    3C                   ENDIF
070200941201    2C                   ENDIF
070300941201    1C                   ENDIF
070400941201     C*
070500941201     C* IN ARRIVO SE C/S  NON AMMESSO NUMERO E DATA FATTURA
070600941201    1C     D17TBO        IFEQ      'A'
070700941201    1C     D17RBL        ANDEQ     'C'
070800941201     C*
070900941201    2C     D17NFT        IFNE      0
071000941201    2C     D17DFG        ORNE      0
071100941201    3C     D17FIV        ORNE      0
071200990714     C                   MOVEL     ERR(3)        O17MSG
071300990714     C                   MOVEL     '2'           O17ERR
071400941201     C                   GOTO      ENDCTA
071500941201    2C                   ENDIF
071600941201    1C                   ENDIF
071700941201     C*
071800941201     C* SE IMMESSA FIL IVA IMMETTERE ANCHE NUMERO E DATA FATTURA
071900941201    1C     D17FIV        IFNE      0
072000941201     C*
072100941201    2C     D17DFG        IFEQ      0
072200990714    2C     D17NFT        OREQ      0
072300990714     C                   MOVEL     ERR(5)        O17MSG
072400990714     C                   MOVEL     '2'           O17ERR
072500941201     C                   GOTO      ENDCTA
072600941201    2C                   END
072700941201    1C                   END
072800961112     C* SE IMMESSO NUM FATTURA IMMETTERE ANCHE REGISTRO IVA E DATA
072900961112    1C     D17NFT        IFGT      0
073000961112     C*
073100961112    2C     D17DFG        IFEQ      0
073200961112    2C     D17FIV        OREQ      0
073300990714     C                   MOVEL     ERR(23)       O17MSG
073400990714     C                   MOVEL     '3'           O17ERR
073500961112     C                   GOTO      ENDCTA
073600961112    2C                   END
073700961112    1C                   END
073800941201     C**
073900941201     C* DATA FATTURA
074000941201     C**
074100941201    1C     D17DFG        IFNE      0
074200941201     C*
074300941201     C* SE IMMESSA DATA IMMETTERE ANCHE IL NUMERO
074400941201     C* (SOLO SE NON E' UN C/S DI SEDE)
074500941201     C*   E SE NON E' SDI
074600941201    2C     D17NFT        IFEQ      0
074700941201     C*
074800941201     C     D17RBL        ANDNE     'C'
074900990714     C     D17TBO        ANDLT     '0'
075000990714     C                   MOVEL     ERR(9)        O17MSG
075100990714     C                   MOVEL     '4'           O17ERR
075200941201     C                   GOTO      ENDCTA
075300941201    2C                   ENDIF
075400941201     C* CONTROLLO DATA FATTURA
075500941201     C                   Z-ADD     D17DFG        G02DAT
075600941201     C                   MOVEL     *BLANKS       G02ERR
075700941201     C                   CALL      'XSRDA8'
075800941201     C                   PARM                    WLBDAT
075900941201    2C     G02ERR        IFEQ      '1'
076000990714     C                   MOVEL     ERR(6)        O17MSG
076100990714     C                   MOVEL     '4'           O17ERR
076200941201     C                   GOTO      ENDCTA
076300941201    2C                   END
076400941201     C*
076500990714     C                   Z-ADD     G02INV        O17DFA
076600941201     C                   Z-ADD     G02DAT        D17DFG
076700941201     C*
076800941201     C* DATA FATT NO < O UGUALE A 10° DATA PARUT
076900991029     C* CONTROLLO SOLO IN PARTENZA SE NON PREPAGATO AUTOMATICO
077000991029     C     D17TBO        IFEQ      'P'
077100991029     C     D17PRP        ANDNE     'S'
077200990714    2C     O17DFA        IFLE      WDTU10
077300990714     C                   MOVEL     ERR(10)       O17MSG
077400990714     C                   MOVEL     '4'           O17ERR
077500990714     C                   MOVEL     'S'           O17ERF
077600941201     C                   GOTO      ENDCTA
077700941201    2C                   ENDIF
077800990830    2C                   ENDIF
077900941201     C*
078000961112     C* DATA FATTURA NO > DATA DEL GIORNO se sono in P.O.
078100941201     C     D17TBO        IFLT      '0'
078200990714    2C     O17DFA        ANDGT     DATEU
078300990714     C                   MOVEL     ERR(11)       O17MSG
078400990714     C                   MOVEL     '4'           O17ERR
078500941201     C                   GOTO      ENDCTA
078600941201    2C                   END
078700941201    1C                   END
078800941201     C**
078900941201     C* NUMERO FATTURA
079000941201     C**
079100941201    1C     D17DFG        IFGT      0
079200941201     C     D17RBL        ANDNE     'C'
079300941201     C*
079400961112     C* CONTROLLO REGISTRO IVA
079500941201    2C     D17FIV        IFEQ      0
079600990714     C                   MOVEL     ERR(8)        O17MSG
079700990714     C                   MOVEL     '3'           O17ERR
079800941201     C                   GOTO      ENDCTA
079900941201    2C                   ENDIF
080000941201     C*
080100941201     C* SE SONO IN SEDE VEDO SE SONO LE FIL IVA IN AUTOMATICO
080200941201     C*       DA FATTURAZIONE AUTOMATICA
080300170717    2C***  D17FIV        IFEQ      §QCFIV
080400170717     C***  D17FIV        OREQ      §QCFI2
080500170717
080600170717     c                   if        D17FIV>=900
080700941201     C*
080800941201    3C     D17TBO        IFLT      '0'
080900941201     C     D17FFI        OREQ      'S'
081000990714     C                   MOVEL     ERR(8)        O17MSG
081100990714     C                   MOVEL     '3'           O17ERR
081200941201     C                   GOTO      ENDCTA
081300941201    3C                   ENDIF
081400941201     C*
081500941201   X2C                   ELSE
081600941201     C*
081700941201     C* SE FRANCO    PRENDO LA LINEA PARTENZA
081800941201    3C     FLGTB1        IFEQ      'F'
081900941201     C                   Z-ADD     D17LNP        COMFIV            3 0
082000941201     C                   ELSE
082100941201     C* SE ASSEGNATO PRENDO LA LINEA ARRIVO
082200941201     C                   Z-ADD     D17LNA        COMFIV            3 0
082300941201    3C                   ENDIF
082400941201     C*
082500020725     C                   clear                   TIBS02DS
082600020725     C                   clear                   DYIV
082700020725     C                   movel     'C'           T02mod
082800020725     C                   movel     KNSIF         T02sif
082900020725     C                   movel     'YIV'         T02cod
083000020725     C                   movel     C_Soc         T02ke1
083100020725     C                   movel     COMfiv        T02ke2
083200020725     C                   call      'TIBS02R'
083300020725     C                   parm                    KPJBA
083400020725     C                   parm                    TIBS02DS
083500020725    3C                   IF        T02err <> *blanks
083600941201     C*
083700941201     C* NON TROVATA TABELLA
083800990714     C                   MOVEL     ERR(8)        O17MSG
083900990714     C                   MOVEL     '3'           O17ERR
084000941201     C                   GOTO      ENDCTA
084100941201   X3C                   ELSE
084200020725     C                   movel     T02uni        DYIV
084300941201     C*
084400961112     C* registro IVA ERRATO
084500941201    4C     §IVLVF        IFNE      D17FIV
084600990714     C                   MOVEL     ERR(8)        O17MSG
084700990714     C                   MOVEL     '3'           O17ERR
084800941201     C                   GOTO      ENDCTA
084900941201    4C                   ENDIF
085000941201    3C                   ENDIF
085100941201    2C                   ENDIF
085200941201     C*
085300990701     C* PER GLI 8888 NUM FATTURA > DI 50000 (DA DSQT1)
085400990701     C*  NON PREPAGATI AUTOMATICI
085500990701     C*
085600990825   1AC     D17PRP        IFNE      'S'
085700990714    2C     TASCC4        IFEQ      8888
085800990701     C     D17TBO        OREQ      'P'
085900991105     C* NON SI POSSONO FARE PIU' PREPAGATI MANUALI
086000991105     C     D17NFT        IFGT      0
086100991105     C                   MOVEL     ERR(33)       O17MSG
086200990714     C                   MOVEL     '2'           O17ERR
086300990701     C                   GOTO      ENDCTA
086400990701    3C                   ENDIF
086500990701    2C                   ENDIF
086600990701   1AC                   ENDIF
086700941201     C*
086800941201    1C                   ENDIF
086900990416     C**
087000990416     C*
087100990416     C* PER COD 8888 O 9999 CON IMP DA ASSICUR OBBLIGATORIA VARIA "R"
087200990416     C* E' L'ULTIMO CONTROLLO PERCHE IN ARRIVO E' FORZABILE
087300120525     c* non segnalo errore se bolla già fatturata
087400990714    1C     TASCC4        IFEQ      8888
087500990714     C     TASCC4        OREQ      9999
087600990714    2C     D17IAS        IFNE      0
087700990416     C     D17IMV        ANDNE     0
087800120525     c     d17tbo        andne     '0'
087900990416     C*
088000990416     C     §$2VRR        LOOKUP    VRC                                    30
088100990416     c*
088200990416    3C     *IN30         IFEQ      *OFF
088300990714     C                   MOVEL     ERR(17)       O17MSG
088400990714     C                   MOVEL     '6'           O17ERR
088500990714     C                   Z-ADD     1             O17ERV
088600990416     C                   GOTO      ENDCTA
088700990416    3C                   ENDIF
088800990416    2C                   ENDIF
088900990416    1C                   ENDIF
089000991006     C** ENDIF DI TOT='S'
089100991006    0C                   ENDIF
089200941021     C*
089300941021     C     ENDCTA        ENDSR
089400941021     C*
089500991115     C* CONTROLLO DECIMALI DA CONTABILITA' ---------------------------*
089600991115     C     CTRDEC        BEGSR
089700991115     C     §CVDEC        IFEQ      1
089800991115     C                   MOVE      VAR(X)        W002A             2
089900991115     C     W002A         IFNE      '00'
090000991115     C                   MOVEA     ERR(34)       K78
090100991115     C                   MOVEL     §CVDEC        W002A
090200991115     C                   MOVEA     W002A         K78(36)
090300991115     C                   MOVEA     K78           O17MSG
090400991115     C                   MOVEL     '6'           O17ERR
090500991115     C                   SETON                                        90
090600991115     C                   ENDIF
090700991115     C                   ENDIF
090800991115     C**
090900991115     C     §CVDEC        IFEQ      2
091000991115     C                   MOVE      VAR(X)        W001A             1
091100991115     C     W001A         IFNE      '0'
091200991115     C                   MOVEA     ERR(34)       K78
091300991115     C                   MOVEL     §CVDEC        W002A
091400991115     C                   MOVEA     W002A         K78(36)
091500991115     C                   MOVEA     K78           O17MSG
091600991115     C                   MOVEL     '6'           O17ERR
091700991115     C                   SETON                                        90
091800991115     C                   ENDIF
091900991115     C                   ENDIF
092000991115     C                   ENDSR
092100100115     C*
092200100115     C* controllo codice esenzione IVA con partita IVA -----------------*
092300100115     C     CtrCEIISO     BEGSR
092400100118     c                   clear                   ds4i
092500100118
092600100118     c                   if        §d17iso<>' '
092700100118     C                   MOVEL     '4I'          COD
092800100118     C                   MOVEL(p)  §d17iso       key
092900100118     C     KTAB          CHAIN     TABEL                              30
093000100118     c                   if        %found(tabel00f)
093100100118     c                   movel     tbluni        ds4i
093200100118     c                   endif
093300100118     c                   endif
093400100118
093500100115     c* codice esenzine "T"
093600100115     c                   select
093700100115     c                   when      d17cei='T'
093800100119     c                   if        §d17iso<>'$$' and §d17iso<>'SM'
093900100115     C                   MOVEl     err(40)       O17MSG
094000100115     C                   MOVEL     '1'           O17ERR
094100100115     c                   endif
094200100118
094300100115     c                   when      d17cei='K'
094400100119     c                   if        §d17iso<>'$$' and §4iita<>'I'
094500100115     C                   MOVEl     err(41)       O17MSG
094600100115     C                   MOVEL     '1'           O17ERR
094700100115     c                   endif
094800100118
094900100115     c                   when      d17cei='S'
095000100118     c
095100100118     c                   if        §d17iso='  ' or §4iue<>'S'  or
095200100118     c                                             §4iita='I'
095300100115     C                   MOVEl     err(42)       O17MSG
095400100115     C                   MOVEL     '1'           O17ERR
095500100115     c                   endif
095600100118
095700100118     c                   when      d17cei='R'  and §d17iso<>' '
095800100118     c
095900100119     c                   if        §d17iso<>'  ' and
096000100119     c                             §d17iso<>'$$' and §4iita<>'I'
096100100118     C                   MOVEl     err(43)       O17MSG
096200100118     C                   MOVEL     '1'           O17ERR
096300100118     c                   endif
096400100115     c
096500100115     c                   ENDSL
096600100115     c                   ENDSR
096700991115     C*
096800991115     C* OPERAZIONI INIZIALI ------------------------------------------*
096900991115     C     *INZSR        BEGSR
097000941021     C**
097100941021     C* KLIST
097200941021     C**
097300941021     C     KTAB          KLIST
097400941021     C                   KFLD                    CODUT             1 0
097500941021     C                   KFLD                    COD               2
097600941021     C                   KFLD                    KEY               8
097700990709     C     KTAB2         KLIST
097800990709     C                   KFLD                    CODUT             1 0
097900990709     C                   KFLD                    COD               2
098000990712     C     KBOL          KLIST
098100990712     C                   KFLD                    D17AAS
098200990714     C                   KFLD                    D17LNS
098300990712     C                   KFLD                    D17NRS
098400990712     C                   KFLD                    D17NSP
098500990714     C                   KFLD                    D17TBS
098600941021     C*
098700941021     C                   Z-ADD     1             CODUT
098800941021     C**
098900941021     C* DEFINIZIONE CAMPI
099000941021     C**
099100941021     C     *LIKE         DEFINE    TBLKEY        §KEY
099200941124     C     *LIKE         DEFINE    TASSV1        WSV1
099300941124     C     *LIKE         DEFINE    TASVA1        WVA1
099400941021     C*
099500941021     C                   CALL      'X§PARUT'
099600941021     C                   PARM                    UT§DSE
099700941021     C*
099800941021     C                   TIME                    W0140            14 0
099900941021     C* UDATE IN GGMMAAAA
100000941021     C                   MOVE      W0140         WDTGIO            8 0
100100941021     C*
100200941021     C* UDATE IN AAAAMMGG
100300941021     C                   Z-ADD     WDTGIO        G02DAT
100400941021     C                   MOVEL     *BLANK        G02ERR
100500941021     C                   CALL      'XSRDA8'
100600941021     C                   PARM                    WLBDAT
100700941021     C                   MOVEL     G02INV        DATEU             8 0
100800941021     C***
100900941021     C* DTU,10 LA PORTO A 8 PER I CONTROLLO
101000941021     C***
101100941021     C                   CLEAR                   WLBDAT
101200941021     C                   Z-ADD     DTU(10)       G02INV
101300941021     C                   MOVEL     '3'           G02ERR
101400941021     C                   CALL      'XSRDA8'
101500941021     C                   PARM                    WLBDAT
101600941021     C                   MOVEL     G02INV        WDTU10            8 0
101700941021     C***
101800941021     C* CARICO CAMPI FISSI PER TASSAZIONE/BOLLETTAZIONE
101900941021     C***
102000941021     C                   MOVEL     'QT'          COD
102100941021     C                   MOVEL     '1       '    KEY
102200941021     C     KTAB          CHAIN     TABEL                              30
102300941021     C  N30              MOVEL     TBLUNI        DSQT1
102400941021     C   30              CLEAR                   DSQT1
102500941116     C***
102600170717     C***                   MOVEL     'QC'          COD
102700170717     C***                   MOVEL     '2       '    KEY
102800170717     C***     KTAB          CHAIN     TABEL                              30
102900170717     C***  N30              MOVEL     TBLUNI        DSQC2
103000170717     C***   30              CLEAR                   DSQC2
103100941117     C**
103200941117     C* CARICO TABELLE DEI CODICI VARIE
103300941117     C**
103400941117     C                   MOVEL     '$2'          COD
103500941117     C                   MOVEL     '1       '    KEY
103600941117     C     KTAB          CHAIN     TABEL                              30
103700941117     C  N30              MOVEL     TBLUNI        DS$2
103800941117     C   30              CLEAR                   DS$2
103900990709     C**
104000990709     C* CARICO TABELLE DELLE VARIE PER CONTROLLI
104100990709     C**
104200990709     C                   Z-ADD     0             X
104300990709     C                   MOVEL     'CC'          COD
104400990709     C     KTAB2         CHAIN     TABEL                              30
104500990709     C     *IN30         DOWEQ     *OFF
104600990709     C     TBLFLG        IFEQ      ' '
104700990730     C                   MOVEL     TBLKEY        W005A             5
104800990730     C     W005A         IFEQ      'VARIE'
104900990709     C                   MOVEL     TBLUNI        DSCC
105000990709     C                   ADD       1             X
105100990709     C* SIGLA VARIA
105200990709     C                   MOVE      TBLKEY        SVA(X)
105300990709     C* SIGLA VARIA CHE FA PARTE DEL TOTALE IMPONIBILE S/N
105400990709     C                   MOVEL     §CCFL6        SVI(X)
105500990709     C* COD ESENZIONE IVA COLLEGATO
105600990709     C                   MOVEL     §CCCEI        SVE(X)
105700990709     C* CODICE UTILIZZABILE IN TASSAZIONE
105800990714     C                   MOVEL     §CCUTI        SVU(X)
105900030603     C* FLAG PER IGNORARE I CONTROLLI NELLE BOLLE CHE ACCETTANO 1 SOLA VARIA
106000030603     C                   MOVEL     §CCFL0        SFC(X)
106100990714     C                   ENDIF
106200990730     C                   ENDIF
106300990714     C**
106400990709     C     KTAB2         READE     TABEL                                  30
106500990709     C                   ENDDO
106600941021     C*
106700941021     C                   ENDSR
106800991115**
106900941111Esenzione IVA errata                                                           1
107000941111Non ammesso numero e data fattura per bolla in Conto Servizio o ASSEGNATO      2
107100941111Non ammesso numero e data fattura per bolla in conto Servizio                  3
107200941111Per bolla in C/S ammessa soltanto la Data Fattura                              4
107300961112Se immesso registro IVA immettere anche numero e data fattura                  5
107400941111Data fattura errata                                                            6
107500941124Se c'e' la fattura immettere anche il totale imponibile                        7
107600961112Registro IVA errato                                                            8
107700941111Se immessa data immettere anche numero Fattura                                 9
107800941111Data fattura deve essere maggiore della data blocco fatturazione              10
107900941111Data fattura non puo' essere maggiore della data del giorno                   11
108000941111Per i Prepagati, Numero fattura deve essere maggiore di 50.000                12
108100941111Sigla Varia errata                                                            13
108200941111Varia gia' immessa                                                            14
108300941021Varia "G" non ammessa: la bolla non prevede il C/Assegno                      15
108400080123Importo della varia MINORE di ZERO !!!!!
108500080123Cli 8888/9999:se ImportoAssicurare e TotImponibile InserireVaria"R"
108600080123Varia "G" obbligatoria se bolla con C/Assegno o legata a bolla con C/A
108700941221Tassazione non ammessa per bolla in C/Servizio                                19
108800990730Imponibile non corretto. La somma dei dati immessi e' XXXXXXXXXX,XXX          20
108900080123Se c'e' la fattura, immettere almeno un importo di TASSAZIONE
109000080123TASSAZIONE CON TARIFFA MITT: la bolla deve essere tassata fino all'IMPONIBILE 22
109100961112Immettere anche data e registro iva se immesso numero fattura
109200080123Cli 8888/9999:se TotImponibile senza ImpAssicurare togliere varia"R"
109300990714Spedizione inesistente                                                        25
109400990714Divisa errata o non utilizzabile                                              26
109500991012Non sono ammessi importi con i decimali                                       27
109600990914Non e' stato indicato in tipo di elaborazione che si vuole (D17FIM=' ')       28
109700990914Divisa utilizzabile DALLA DATA xx/xx/xxxx                                     29
109800990914Divisa utilizzabile FINO ALLA DATA xx/xx/xxxx                                 30
109900990914Divisa obbligatoria se immesso almeno un importo di tassazione
110000080123Immettere la divisa soltanto se immesso un importo di tassazione
110100080123PREPAGATI MANUALI,con numero fattura > di 50.000 non più accettati
110200991115VARIA ESENTE IVA:ammessi soltanto  xX  decimali                               34
110300020415Varia non ammessa per il tipo bolla specificato                               35
110400020415Porto non ammesso per il tipo bolla specificato
110500040129Non è possibile negare la Varia "G" per il tipo bolla specificato             37
110600060113Cod. esenzione utilizzabile DALLA DATA xx/xx/xxxx                             38
110700060113Cod. esenzione utilizzabile FINO ALLA DATA xx/xx/xxxx                         39
110800100115Esenzione "T" per impresa Extra UE: inserire partitaIVA "$$" o "SM SanMarino"
110900100115Esenzione "K" per impresa Livigno o Camp.It:inserire partitaIVA italiana o "$$"
111000100118Esenzione "S" per impresa UE: inserire partitaIVA comunitaria non italiana
111100100118Esenzione "R" per impresa italiana: si accetta solo partitaIVA italiana o "$$"
