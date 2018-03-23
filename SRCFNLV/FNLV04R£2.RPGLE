000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200990727     F* FNLV04R  *---------------------------------------------------*
000300930505     F* - CALCOLO TOTALE FATTURA DI SPEDIZIONE                       *
000400990729     F*   INPUT: FNLV04DS DTASV - PIENA ANCHE CON I CODICI ESENZ.VARIE
000500990729     F*   OUTPUT: I CAMPI PREVISTI DALLA DS, IVA E BOLLO ANCHE
000600990729     F*           NELLA SCHIERA DELLE VARIE
000700000000     F*--------------------------------------------------------------*
000800930505     FTABEL00F  IF   E           K DISK
000900990728     D WSV             S              1    DIM(20)
001000990728     D WVA             S             11  3 DIM(20)
001100990728     D WES             S              1    DIM(20)
001200941104     D DSCV          E DS
001300990727     D DS$2          E DS
001400941104     D DSQT1         E DS
001500941104     D DS1A          E DS
001600050622     D DSEI          E DS
001700990928     D DGEI          E DS
001800990928     D KPJBA         E DS
001900941108     D* PASSAGGIO DATI NECESSARI PER CALCOLO TOTALE DA FATTURARE
002000990727     D DSLV04        E DS                  EXTNAME(FNLV04DS)
002100990727     D* PASSAGGIO DATI PER TOTALE IMPONIBILE - FNLV16R
002200990727     D DSLV16        E DS                  EXTNAME(FNLV16DS)
002300990928     D DSBS02        E DS                  EXTNAME(TIBS02DS)
002400991025     D YEUR          E DS                  EXTNAME(YEURCODS)
002500941108     D* DS DELLE VARIE
002600990727     D DTASV         E DS
002700990727     D  SVR                    1     20
002800990727     D                                     DIM(20)
002900990727     D  VAR                   21    140P 3
003000990727     D                                     DIM(20)
003100990727     D  ESE                  141    160
003200941104     D                                     DIM(20)
003300030918
003400030918     D var_impon       s              1
003500030918     D diffe           s             13  3
003600050622     D impbol          s             13  3
003700050622     D w0133           s             13  3
003800941104     I*----------------------------------------------------------------
003900941108     C     *ENTRY        PLIST
004000990727     C                   PARM                    DSLV04
004100990727     C                   PARM                    DTASV
004200941108     I*----------------------------------------------------------------
004300941104     C* D04TLA =' '  ELABORO E CHIUDO CON RETRN
004400941104     C* D04TLA ='L'  ELABORO E CHIUDO CON LR
004500941104     C* D04TLA ='C'            CHIUDO CON LR   SOLO PER CHIUSURA FILE
004600930505     C*
004700941104    1C     D04TLA        IFEQ      'C'
004800930505     C                   SETON                                        LR
004900990727     C** SE RICHIAMATO, CHIUDO IL FILES
005000990727     C     CALV16        IFEQ      'S'
005100990727     C                   CLEAR                   DSLV16
005200990727     C                   MOVEL     'C'           D17TLA
005300990727     C                   CALL      'FNLV16R'
005400990727     C                   PARM                    DSLV16
005500990727     C                   ENDIF
005600930505     C*
005700930505   X1C                   ELSE
005800941108     C*
005900941108     C* PULIZIA CAMPI DI OUTPUT
006000941108     C                   CLEAR                   D04IVA
006100941108     C                   CLEAR                   D04BOL
006200941108     C                   CLEAR                   D04IFT
006300990727     C                   CLEAR                   W0133
006400950221     C                   CLEAR                   WCAS
006500990727     C                   MOVEL     0             TRE               1 0
006600990727     C                   MOVEL     D04TBL        FLGTBL            1
006700990727     C* ELIMINO DALLE VARIE LA VARIA DELL'IVA E DEL BOLLO
006800990727     C                   Z-ADD     1             X
006900990727     C     §$2BOL        LOOKUP    SVR(X)                                 90
007000990727     C   90              CLEAR                   VAR(X)
007100990727     C   90              CLEAR                   ESE(X)
007200990804*    C   90              CLEAR                   SVR(X)
007300990727     C                   Z-ADD     1             X
007400990728     C     §$2IVA        LOOKUP    SVR(X)                                 91
007500990728     C   91              CLEAR                   VAR(X)
007600990728     C   91              CLEAR                   ESE(X)
007700990804*    C   91              CLEAR                   SVR(X)
007800990728     C* SE HO TROVATO UNA DELLE 2 SISTEMO LA DS DELLE VARIE RIEMPIENDO
007900990728     C*  I BUCHI VUOTI
008000990728     C     *IN90         IFEQ      *ON
008100990728     C     *IN91         OREQ      *ON
008200990728     C                   CLEAR                   WSV
008300990728     C                   CLEAR                   WVA
008400990728     C                   CLEAR                   WES
008500990804*    C*?*                  Z-ADD1         X
008600990804*    C                   CLEAR                   X
008700990728     C                   DO        20            Y                 2 0
008800990728     C     SVR(Y)        IFNE      ' '
008900990804*    C                   ADD       1             X
009000990728     C                   MOVEL     SVR(Y)        WSV(X)
009100990728     C                   Z-ADD     VAR(Y)        WVA(X)
009200990728     C                   MOVEL     ESE(Y)        WES(X)
009300990728     C                   ENDIF
009400990728     C                   ENDDO
009500990728     C**
009600990728     C                   MOVEA     WSV           SVR
009700990728     C                   MOVEA     WVA           VAR
009800990728     C                   MOVEA     WES           ESE
009900990728     C                   ENDIF
010000930505     C*
010100991025     C** DIVISA  FATTURA
010200990928     C                   MOVEL     'CV'          COD
010300991025     C                   MOVEL(P)  D04DIV        KEY
010400990928     C     KTAB          CHAIN     TABEL                              30
010500990928     C  N30              MOVEL     TBLUNI        DSCV
010600990928     C   30              CLEAR                   DSCV
010700990928     C                   MOVEL     §CVFDC        WFDC
010800991011     C                   MOVEL     §CVDEC        WDEC              2 0
010900991025     C* CONTROLLO DEL C/ASSEGNO
011000991025    2C     D04CAS        IFGT      0
011100991025     C                   EXSR      CTRCAS
011200991025    2C                   END
011300991025     C* SE DEVO CALCOLARE IL TOALE IMPONIBILE, LO CALCOLO
011400991025     C     D04FIM        IFEQ      'T'
011500991025     C                   EXSR      CALTOT
011600991025     C                   ENDIF
011700991012     C*
011800991012     C* MEMORIZZO QUAL E' L'UNITA'MINIMA PER LA DIVISA
011900991012     C                   SELECT
012000991012     C     WDEC          WHENEQ    0
012100991012     C                   Z-ADD     1             WUNITA            5 3
012200991012     C     WDEC          WHENEQ    1
012300991012     C                   Z-ADD     0,1           WUNITA
012400991012     C     WDEC          WHENEQ    2
012500991012     C                   Z-ADD     0,01          WUNITA
012600991012     C                   OTHER
012700991012     C                   Z-ADD     0,001         WUNITA
012800991012     C                   ENDSL
012900930505     C**
013000930505    2C     TRE           DOWLT     2
013100991123     C                   CLEAR                   ESVAR
013200991123     C                   CLEAR                   NUM133
013300990727     C                   CLEAR                   W0133
013400050622     C                   CLEAR                   IMPBOL
013500941108     C**
013600930506     C* P R I M A   B O L L A
013700040121      * E   S E C O N D A   B O L L A
013800941108     C**
013900040121    3C**!!!D04SPE        IFEQ      '1'
014000990727     C* FRANCO PREPAGATO
014100040121     C**!!!D04SPE        OREQ      '8'
014200991123     C**
014300991123     C* NUM133=  VARIE ESENTI
014400050622     C* impbol=  imponibile bollo
014500991123    4C                   DO        20            X                 2 0
014600991123     C                   CLEAR                   PP
014700991123    5C     ESE(X)        IFNE      ' '
014800991123     C                   ADD       VAR(X)        NUM133           13 3
014900991123     C                   ADD       VAR(X)        ESVAR            13 3
015000050622      * verifico se esenzione iva soggetta all'imposta di bollo
015100050622      ** tabella codici esenzione IVA
015200050622     c                   MOVEL     'EI'          COD
015300050622     c                   MOVEL(P)  ESE(X)        KEY
015400050622     c     KTAB          CHAIN     TABEL                              30
015500050622     c  N30              MOVEL     TBLUNI        DSEI
015600050622     c   30              CLEAR                   DSEI
015700050622      ** tabella controllo se esenzione iva soggetta al bollo
015800050622     c                   clear                   dsbs02
015900050622     c                   movel     'L'           T02tla
016000050622     c                   movel     'C'           T02mod
016100050622     c                   movel     knsif         t02sif
016200050622     c                   movel     'YBO'         t02cod
016300050622     c                   movel     §eijei        t02ke1
016400050622     c                   call      'TIBS02R'
016500050622     c                   parm                    KPJBA
016600050622     c                   parm                    dsbs02
016700050622      * se esenzione iva non presente in tabella calcolo imponibile per applicaz
016800050622     c                   if        t02err <> *blanks
016900050627     c                   add       var(X)        impbol
017000050622     c                   Endif
017100991123     C**
017200991123     C* MEMORIZZO LA PRIMA POSIZIONE DI VARIA ESENTE
017300991123     C     PP            IFEQ      0
017400991123     C                   Z-ADD     X             PP                2 0
017500991123     C                   ENDIF
017600991123    5C                   END
017700991123    4C                   ENDDO
017800941108     C**
017900941108     C** IMPONIBILE NON ESENTE IVA
018000941104    4C     D04CEI        IFEQ      ' '
018100990928     C                   Z-ADD     D04IMV        WIMV
018200991123     C                   SUB       NUM133        WIMV
018300990928     C                   EXSR      CALIVA
018400030918      * se ho variato l'imponibile modifico anche D04IMV con il nuovo WIMV
018500030918      * impostato nella routine CALIVA e la differenza la sommo al porto
018600030918      *
018700030918     c                   if        Var_Impon = 'S'
018800030918     c                   eval      diffe = wimv - d04imv
018900030918      *
019000030918     c                   add       diffe         d04por
019100030918      *
019200030918
019300030918     c                   eval      D04Imv  = Wimv + Num133
019400030918     c                   endif
019500991123     C** SOMMO TUTTI GLI IMPORTI ESENTI IVA
019600050622      * tolgo questa specifica perchè  fatto prima
019700050622     C*****              ADD       NUM133        W0133
019800990928     C*
019900941108   X4C                   ELSE
020000941108     C** IMPONIBILE ESENTE IVA
020100941108     C                   MOVEL     *ZEROS        D04ALI
020200050622      ** tabella codici esenzione IVA
020300050622     c                   MOVEL     'EI'          COD
020400050622     c                   MOVEL(P)  d04cei        KEY
020500050622     c     KTAB          CHAIN     TABEL                              30
020600050622     c  N30              MOVEL     TBLUNI        DSEI
020700050622     c   30              CLEAR                   DSEI
020800050622      ** tabella controllo se esenzione iva soggetta al bollo
020900050622     c                   clear                   dsbs02
021000050622     c                   movel     'L'           T02tla
021100050622     c                   movel     'C'           T02mod
021200050622     c                   movel     knsif         t02sif
021300050622     c                   movel     'YBO'         t02cod
021400050622     c                   movel     §eijei        t02ke1
021500050622     c                   call      'TIBS02R'
021600050622     c                   parm                    KPJBA
021700050622     c                   parm                    dsbs02
021800050622      * se esenzione iva non presente in tabella calcolo imponibile per applicaz
021900050622     c                   if        t02err <> *blanks
022000050622     c                   add       d04imv        impbol
022100050622     c                   Endif
022200050622      * tolgo questa specifica perchè  fatto in un altro campo
022300050622     C*****              ADD       D04IMV        W0133            13 3
022400941108    4C                   ENDIF
022500941108     C*
022600991123     C** BOLLO
022700990928     C                   EXSR      CALBOL
022800930505     C*
022900990727     C* D04IFT= VARIE ESENTI + IVA O BOLLO + TOT.IMPONIBILE
023000930505     C*
023100990727     C     D04IMV        ADD       D04IVA        D04IFT
023200941104     C                   ADD       D04BOL        D04IFT
023300991123     C* LE VARIE ESENTI NE FANNO GIA' PARTE
023400991123     C*********            ADD  NUM133    D04IFT
023500930505     C*
023600040121   X3C**!!!              ELSE
023700930506     C*
023800930506     C* S E C O N D A   B O L L A
023900040121    4C**!!!D04CEI        IFEQ      ' '
024000040121     C**!!!TASEV1        ANDEQ     ' '
024100040121     C**!!!              Z-ADD     TASVA1        WIMV
024200040121     C**!!!              EXSR      CALIVA
024300030918      * se ho variato l'imponibile modifico anche TASVA1 con il nuovo WIMV
024400030918      * impostato nella routine CALIVA
024500040121     c**!!!              if        Var_Impon = 'S'
024600030918      *
024700040121     c**!!!              eval      TASVA1  = Wimv
024800040121     c**!!!              endif
024900930505     C*
025000040121   X4C**!!!              ELSE
025100040121     C**!!!              MOVEL     *ZEROS        D04ALI
025200040121     C**!!!              Z-ADD     TASVA1        W0133
025300991012     C* APPLICO BOLLO
025400040121     C**!!!              EXSR      CALBOL
025500040121    4C**!!!              ENDIF
025600930505     C*
025700040121     C**!!!D04BOL        ADD       D04IVA        D04IFT
025800040121     C**!!!              ADD       TASVA1        D04IFT
025900040121    3C**!!!              ENDIF
026000930505     C**
026100991011     C* ESEGUO ARROTONDAMENTO SE C'E' VALORE O SE I DECIMALI DEL
026200991011     C*  CAMPO TOTALE IMPONIBILE SONO DI PIU' DI QUELLI PREVISTI
026300991011     C*  DALLA DIVISA
026400991011     C                   EXSR      CTRARR
026500930505     C*
026600930505     C*
026700930505    2C                   END
026800990728     C* SCRIVO IVA E BOLLO NELLE VARIE
026900990728     C     D04IVA        IFGT      0
027000990728     C                   Z-ADD     1             X
027100990728     C     ' '           LOOKUP    SVR(X)                                 90
027200990728     C   90              MOVEL     §$2IVA        SVR(X)
027300990728     C   90              Z-ADD     D04IVA        VAR(X)
027400990728     C                   ENDIF
027500990728     C     D04BOL        IFGT      0
027600990728     C                   Z-ADD     1             X
027700990728     C     ' '           LOOKUP    SVR(X)                                 90
027800990728     C   90              MOVEL     §$2BOL        SVR(X)
027900990728     C   90              Z-ADD     D04BOL        VAR(X)
028000990728     C                   ENDIF
028100930505     C*
028200941104     C     D04TLA        IFEQ      ' '
028300930505     C                   RETURN
028400930505     C                   ELSE
028500930505     C                   SETON                                        LR
028600930505     C                   END
028700930505    1C                   END
028800990727     C*
028900050622
029000050622
029100990727     C*--- CALCOLO TOTALE IMPONIBILE ---------------------------------*
029200990727     C     CALTOT        BEGSR
029300990727     C                   CLEAR                   DSLV16
029400990727     C                   Z-ADD     D04POR        D17POR
029500990727     C                   MOVEL     'T'           D17FIM
029600990727     C                   CALL      'FNLV16R'
029700990727     C                   PARM                    DSLV16
029800990727     C                   PARM                    DTASV
029900990727     C*
030000990727     C                   Z-ADD     O17IMV        D04IMV
030100990727     C                   MOVEL     'S'           CALV16            1
030200990727     C                   ENDSR
030300990928     C*
030400990928     C*--- CARICO LA DGEI --------------------------------------------*
030500990928     C     CARGEI        BEGSR
030600990928     C*
030700990928     C     D04DIV        IFNE      SAVDIV
030800990928     C                   CLEAR                   DSBS02
030900990928     C                   MOVEL     'C'           T02MOD
031000990928     C                   MOVEL     'GEI'         T02COD
031100990928     C                   MOVEL     D04DIV        T02KE1
031200990928     C                   CALL      'TIBS02R'
031300990928     C                   PARM                    KPJBA
031400990928     C                   PARM                    DSBS02
031500990928    2C     T02ERR        IFEQ      ' '
031600990928     C                   MOVEL     T02UNI        DGEI
031700990928     C                   ELSE
031800990928     C                   CLEAR                   DGEI
031900990928     C                   ENDIF
032000990928     C*
032100990928     C                   MOVEL     D04DIV        SAVDIV
032200990928     C                   ENDIF
032300990928     C                   ENDSR
032400990928     C*
032500990928     C*--- CALCOLO IVA IN BASE AI DECIMALI ---------------------------*
032600990928     C     CALIVA        BEGSR
032700030918      *
032800030918      * pulisco il flag che segnala se ho variato imponibile IVA
032900030918      *
033000030918     c                   clear                   var_impon
033100030918     c                   clear                   diffe
033200030918
033300991012     C* ALIQUOTA IVA
033400991012    1C     D04ALI        IFEQ      0
033500991012     C* VERIFICO LA DATA FATTURA SE C'E'
033600991012    2C     D04DFT        IFEQ      0
033700991012     C     D04DFT        ORGT      §QTDSA
033800991012     C                   Z-ADD     §QTALI        D04ALI
033900991012     C                   ELSE
034000991012     C                   Z-ADD     §QTALO        D04ALI
034100991012    2C                   END
034200991012    1C                   END
034300991012     C**
034400991011     C* IVA IN LIRE: ARROTONDATA PER ECCESSO E SENZA DECIMALI
034500991011    1C     WFDC          IFEQ      ' '
034600990928     C     WIMV          MULT      D04ALI        CO2IVA
034700990928     C                   DIV       100           CO2IVA
034800990928     C     CO2IVA        ADD       0,99          CO0IVA           11 0
034900990928     C                   Z-ADD     CO0IVA        D04IVA
035000991011   X1C                   ELSE
035100990928     C* IVA IN EURO: ARROTONDATA MATEMATICAMENTE E CON 2 DECIMALI
035200030918      * se imponibile iva inferiore a 0,03 imposto l'imponibile a 0,03
035300030918     c                   if        wimv < 0,03
035400030918     c                   eval      wimv = 0,03
035500030918      * valorizzo il flag che segnala la variazione dell'imponibile
035600030918     c                   eval      var_impon = 'S'
035700030918     c                   endif
035800030918
035900991011    2C                   SELECT
036000030918      *
036100991011     C     WDEC          WHENEQ    1
036200991012     C     WIMV          MULT      D04ALI        CO2IVA
036300991012     C     CO2IVA        DIV(H)    100           CO1IVA           13 1
036400991011     C                   Z-ADD     CO1IVA        D04IVA
036500991011     C*
036600991011     C     WDEC          WHENEQ    2
036700990928     C     WIMV          MULT      D04ALI        CO2IVA           13 2
036800990928     C                   DIV(H)    100           CO2IVA
036900990928     C                   Z-ADD     CO2IVA        D04IVA
037000991011     C*
037100991011     C                   OTHER
037200991011     C     WIMV          MULT      D04ALI        CO3IVA           13 3
037300991011     C                   DIV(H)    100           CO3IVA
037400991011     C                   Z-ADD     CO3IVA        D04IVA
037500991011     C*
037600991011    2C                   ENDSL
037700991011    1C                   ENDIF
037800990928     C                   ENDSR
037900990928     C*
038000990928     C*--- CALCOLO BOLLO ---------------------------------------------*
038100990928     C     CALBOL        BEGSR
038200050622    4C     impbol        IFGT      0
038300990928     C* IN BASE ALLA DIVISA, PRENDO LA TABELLA DGEI
038400990928     C                   EXSR      CARGEI
038500990928     C*
038600050622    5C     impbol        IFGT      §GEIMB
038700990928     C                   Z-ADD     §GEIBL        D04BOL
038800990928     C                   ELSE
038900990928     C                   Z-ADD     0             D04BOL
039000990928    5C                   END
039100990928    4C                   END
039200990928     C                   ENDSR
039300991011     C**
039400991011     C* CONTROLLO SE DEVO ESEGUIRE GLI ARROTONDAMENTI ----------------*
039500991011     C     CTRARR        BEGSR
039600991011     C                   ADD       1             TRE
039700991012     C* CONTROLLO SE L'ARROTONDAMENTO E' CONSONO AI DECIMALI PREVISTI
039800991012     C*  DALLA DIVISA
039900991012     C     D04VAR        IFGT      0
040000991012     C                   Z-ADD     D04VAR        W0133
040100991011     C                   EXSR      DECIM
040200991012     C                   ELSE
040300991012     C*
040400991012     C* SE NON C'E' ARROTONDAMENTO IMPOSTO MINIMO VALORE ACCETTABILE
040500991012     C*  CONTABILMENTE DALLA DIVISA
040600991012     C                   Z-ADD     WUNITA        D04VAR
040700991012     C                   ENDIF
040800991012     C*
040900991011     C*
041000991011    1C     TRE           IFNE      2
041100991011    2C     D04VAR        IFGT      0
041200991011     C*
041300991011     C                   Z-ADD     D04IFT        XVALUE
041400991011     C* NUM133= VARIE ESENTI + C/ASSEGNO SE IN CONT E SE BOLLA ASSEGNAT
041500991011     C*         PERCHE' IL C/A E' INCASSATO DAL DESTINATARIO
041600991011     C                   ADD       D04BOL        NUM133
041700991011     C*
041800991011     C* SE IL C/A E' DA SOMMARE AL TOTALE DA ESIGERE
041900991012     C*  SOLO SE C'E' UN VALORE DI ARROTONDAMENTO
042000050622     C*  SE DEVO SOLO SISTEMARE I DECIMALI, NON SOMMO
042100991011    3C     WCAS          IFGT      0
042200991011     C     §1ASTE        ANDEQ     'S'
042300991011     C     D04SPE        ANDNE     '8'
042400991011     C     FLGTBL        ANDEQ     'A'
042500991011     C                   ADD       WCAS          XVALUE
042600991011     C                   ADD       WCAS          NUM133
042700991011    3C                   END
042800991011     C*
042900991011     C                   EXSR      ARROT
043000991011     C*
043100991011   X2C                   ELSE
043200991011     C                   Z-ADD     2             TRE
043300991011    2C                   END
043400991011    1C                   END
043500991011     C                   ENDSR
043600930505     C*
043700991011     C*--- CONTROLLO NUMERO DECIMALI ---------------------------------*
043800991011     C     DECIM         BEGSR
043900991012     C*
044000991011     C                   SELECT
044100991012     C     WDEC          WHENEQ    0
044200991012     C                   MOVE      W0133         W003A             3
044300991012     C     W003A         IFNE      '000'
044400991012     C     D04VAR        ADD       WUNITA        COMOD0
044500991012     C                   Z-ADD     COMOD0        D04VAR
044600991012     C                   ENDIF
044700991012     C*
044800991012     C     WDEC          WHENEQ    1
044900991012     C                   MOVE      W0133         W002A             2
045000991011     C     W002A         IFNE      '00'
045100991012     C     D04VAR        ADD       WUNITA        COMOD1
045200991012     C                   Z-ADD     COMOD1        D04VAR
045300991011     C                   ENDIF
045400991011     C*
045500991011     C     WDEC          WHENEQ    2
045600991012     C                   MOVE      W0133         W001A             1
045700991011     C     W001A         IFNE      '0'
045800991012     C     D04VAR        ADD       WUNITA        COMOD2
045900991012     C                   Z-ADD     COMOD2        D04VAR
046000991011     C                   ENDIF
046100991011     C                   ENDSL
046200991011     C                   ENDSR
046300991011     C*
046400991011     C*--- ARROTONDO TOTALE DA INCASSARE RICALCOLO PORTO E IVA--------*
046500991011     C     ARROT         BEGSR
046600991012     C                   CLEAR                   XCOM02
046700930505     C*
046800941104     C* D04AED: "D" DIFETTO   "E" ECCESSO
046900930505     C* XVALUE: VALORE DA ARROTONDARE
047000941104     C* D04VAR: ARROTONDAMENTO
047100930505     C*
047200990727     C     XVALUE        DIV       D04VAR        INTERO           15 0
047300930505     C                   MVR                     XCOM02
047400991012    2C     D04AED        IFEQ      'E'
047500991012    3C     XCOM02        IFGT      0
047600990727     C                   ADD       1             INTERO
047700991012    3C                   END
047800991012    2C                   END
047900991012     C*
048000991012     C     INTERO        MULT      D04VAR        XVALUE
048100930505     C*
048200991012     C* SOLO SE C'ERA RESTO ED HO ARROTONDATO
048300991012     C*  O SE DEVO SISTEMARE I DECIMALI, RIFACCIO IL CALCOLO
048400990727    1C     XCOM02        IFNE      0
048500930505     C*
048600930505     C* XCOM02 = NUOVO TOTALE DA ESIGERE ARROTONDATO
048700930505     C                   Z-ADD     XVALUE        XCOM02
048800930505     C*
048900990727     C                   SUB       NUM133        XVALUE
049000930505     C*
049100930505     C* VEDO SE LA VARIA E' ESENTE
049200040121    2C**!!!D04SPE        IFEQ      '2'
049300040121     C**!!!D04CEI        ANDEQ     ' '
049400040121     C**!!!              MOVEL     TASEV1        CEI
049500040121   X2C**!!!              ELSE
049600040121     C**!!!              MOVEL     D04CEI        CEI
049700040121    2C**!!!              END
049800930505     C*
049900930505     C*
050000930505     C* XVALUE = NUOVO TOTALE IMPONIBILE PIU' IVA
050100040121    2C**!!!CEI           IFEQ      ' '
050200040121    2C     d04cei        IFEQ      ' '
050300930505     C* SCORPORO DELL'IVA
050400941104     C     D04ALI        ADD       100           NUM3              3 0
050500990727     C     100           MULT      XVALUE        XCOM04
050600990727     C     XCOM04        DIV       NUM3          XCOM04
050700991011     C*
050800991011     C* TENGO I DECIMALI PREVISIT DALLA CONTABILITA' DELLA DIVISA
050900991011     C*  DA 0 A 3
051000990928     C*
051100990928     C* SE NON CI SONO DECIMALI, ARROTONDO ALLA LIRA
051200990928     C     WFDC          IFEQ      ' '
051300991011     C                   Z-ADD(H)  XCOM04        COMOD0           13 0
051400991011     C                   Z-ADD     COMOD0        XCOM04
051500991011     C                   ELSE
051600991011     C                   SELECT
051700991011     C     WDEC          WHENEQ    1
051800991011     C                   Z-ADD(H)  XCOM04        COMOD1           14 1
051900991011     C                   Z-ADD     COMOD1        XCOM04
052000991011     C     WDEC          WHENEQ    2
052100991011     C                   Z-ADD(H)  XCOM04        COMOD2           15 2
052200991011     C                   Z-ADD     COMOD2        XCOM04
052300991011     C                   OTHER
052400991011     C                   Z-ADD(H)  XCOM04        COMOD3           16 3
052500991011     C                   Z-ADD     COMOD3        XCOM04
052600991011     C                   ENDSL
052700991011     C                   ENDIF
052800930505     C*
052900990727     C* XCOM04 = NUOVO TOTALE IMPONIBILE
053000930505     C* RICALCOLO IVA
053100930505     C* XCOM03 = NUOVO TOTALE DA ESIGERE RICALCOLATO
053200930505     C                   MOVEL     ' '           DUE               1
053300930505     C*
053400930617    3C     DUE           DOWNE     '1'
053500990928     C                   Z-ADD     XCOM04        WIMV
053600990928     C                   EXSR      CALIVA
053700030918      * se ho variato l'imponibile modifico anche XCOM04 con il nuovo WIMV
053800030918      * impostato nella routine CALIVA
053900030918     c                   if        Var_Impon = 'S'
054000030918     c                   eval      XCOM04  = Wimv
054100030918     c                   endif
054200030918     C*
054300990928     C*
054400990928     C                   Z-ADD     D04IVA        XCOM03
054500990727     C                   ADD       XCOM04        XCOM03
054600990727     C                   ADD       NUM133        XCOM03
054700990928     C*
054800991012     C* SE RICALCOLANDO L'IVA DIVENTA > RIPROVO TOGLIENDO UNA UNITA'
054900991012     C*  AL TOTALE IMPONIBILE
055000991012     C*
055100930617    4C     XCOM03        IFGT      XCOM02
055200991012     C* MI SALVO L'IVA E RITENTO
055300991012     C                   Z-ADD     D04IVA        SAVIVA
055400991012     C                   SUB       WUNITA        XCOM04
055500930505     C*
055600930617   X4C                   ELSE
055700930505     C*
055800930505     C                   MOVEL     '1'           DUE
055900930505     C* SE AL 2 TENTATIVO DIVENTA < REIMPOSTO IL VALORE MAGGIORE
056000930505     C     XCOM03        IFLT      XCOM02
056100991012     C                   ADD       WUNITA        XCOM04
056200991012     C                   Z-ADD     SAVIVA        D04IVA
056300991012     C                   ENDIF
056400930505     C*
056500991012    4C                   ENDIF
056600991012    3C                   ENDDO
056700930505     C*
056800930617   X2C                   ELSE
056900930505     C*
057000990727     C* XVALUE = XCOM04 = NUOVO TOTALE IMPONIBILE (GIA' SENZA BOLLO
057100990727     C*          PERCHE' SOTRATTO NUM133)
057200990727     C                   Z-ADD     XVALUE        XCOM04
057300930617    2C                   END
057400930505     C*
057500040121    2C**!!!D04SPE        IFEQ      '1'
057600040121     C**!!!D04SPE        OREQ      '8'
057700991123     C     XCOM04        ADD       ESVAR         XCOM04
057800990727     C     XCOM04        SUB       D04IMV        XVALUE
057900941104     C                   ADD       XVALUE        D04POR
058000991123     C                   ADD       XVALUE        D04IMV
058100040121     C**!!!              ELSE
058200040121     C**!!!              Z-ADD     XCOM04        TASVA1
058300040121    2C**!!!              END
058400930505     C*
058500930617   X1C                   ELSE
058600930505     C                   MOVEL     2             TRE
058700930617    1C                   END
058800930505     C                   ENDSR
058900941104     C*
059000941104     C* CONTROLLO SE C/ASSEGNI IN CONTANTI E IN DIVISA ESTERA---------*
059100941104     C     CTRCAS        BEGSR
059200941104     C                   CLEAR                   WCAS
059300941104     C* TIPO INCASSO C/A PER VEDERE SE E' IN CONTANTI
059400941104     C                   MOVEL     '1A'          COD
059500991025     C                   MOVEL(P)  D04TIC        KEY
059600941104     C     KTAB          CHAIN     TABEL                              30
059700941104     C  N30              MOVEL     TBLUNI        DS1A
059800941104     C   30              CLEAR                   DS1A
059900941104     C*
060000941108     C* SE IL C/A E' DA SOMMARE AL TOTALE DA ESIGERE,
060100991025     C*  --> TRASFORMO L'IMPORTO IN DIVISA FATURA
060200941108    1C     §1ASTE        IFEQ      'S'
060300991025     C* SE DIVISA UGUALE A DIVISA FATURA NON DEVO APPLICARE IL CAMBIO
060400991025    2C     D04VCA        IFEQ      D04DIV
060500991025     C                   Z-ADD     D04CAS        WCAS             13 3
060600991025     C                   ELSE
060700991025     C                   CLEAR                   YEUR
060800991025     C                   MOVEL     D04VCA        YECDVI
060900991025     C                   MOVEL     D04DIV        YECDVO
061000991025     C                   Z-ADD     D04CAS        YECIMI
061100991025    3C     WFDC          IFEQ      'S'
061200991123     C                   MOVE      WDEC          YECDCO
061300991025     C                   ELSE
061400991123     C                   MOVE      '0'           YECDCO
061500991025     C                   ENDIF
061600991025     C*
061700991025     C                   CALL      'YEURCO'
061800991025     C                   PARM                    YEUR
061900991025     C*
062000991025    3C     YECESI        IFEQ      ' '
062100991025     C                   Z-ADD     YECIMO        WCAS
062200991025     C                   ELSE
062300991025     C                   MOVEL     ' '           §1ASTE
062400991025    3C                   ENDIF
062500991025    2C                   ENDIF
062600941104    1C                   ENDIF
062700941104     C*
062800941104     C                   ENDSR
062900941104     C***
063000941108     C* ROUTINE INIZIALE---------------------------------------------*
063100941108     C* CARICO CAMPI FISSI PER TASSAZIONE/BOLLETTAZIONE -------------*
063200941108     C     *INZSR        BEGSR
063300941108     C**
063400941108     C* KLIST
063500941108     C**
063600941108     C     KTAB          KLIST
063700941108     C                   KFLD                    CODUT             1 0
063800941108     C                   KFLD                    COD               2
063900941108     C                   KFLD                    KEY               8
064000941108     C**
064100941108     C* DEFINIZIONE CAMPI
064200941108     C**
064300040121     C**!!!*LIKE         DEFINE    D04CEI        CEI
064400990727     C     *LIKE         DEFINE    D04VAR        XCOM04
064500941108     C     *LIKE         DEFINE    D04VAR        XCOM02
064600941108     C     *LIKE         DEFINE    D04VAR        XCOM03
064700941108     C     *LIKE         DEFINE    D04VAR        XVALUE
064800990928     C     *LIKE         DEFINE    §CVFDC        WFDC
064900990928     C     *LIKE         DEFINE    D04IVA        SAVIVA
065000990928     C     *LIKE         DEFINE    D04DIV        SAVDIV
065100990928     C     *LIKE         DEFINE    D04IMV        WIMV
065200941108     C**
065300941115     C*  CARICO LE COSTANTI
065400941108     C**
065500941115     C                   Z-ADD     1             CODUT
065600941104     C                   MOVEL     'QT'          COD
065700941104     C                   MOVEL     '1       '    KEY
065800941104     C     KTAB          CHAIN     TABEL                              30
065900941104     C  N30              MOVEL     TBLUNI        DSQT1
066000941104     C   30              CLEAR                   DSQT1
066100990727     C                   MOVEL     '$2'          COD
066200990727     C                   MOVEL     '1       '    KEY
066300990727     C     KTAB          CHAIN     TABEL                              30
066400990727     C  N30              MOVEL     TBLUNI        DS$2
066500990727     C   30              CLEAR                   DS$2
066600990928     C*
066700941104     C                   ENDSR
