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
001600990928     D DGEI          E DS
001700990928     D KPJBA         E DS
001800941108     D* PASSAGGIO DATI NECESSARI PER CALCOLO TOTALE DA FATTURARE
001900990727     D DSLV04        E DS                  EXTNAME(FNLV04DS)
002000990727     D* PASSAGGIO DATI PER TOTALE IMPONIBILE - FNLV16R
002100990727     D DSLV16        E DS                  EXTNAME(FNLV16DS)
002200990928     D DSBS02        E DS                  EXTNAME(TIBS02DS)
002300991025     D YEUR          E DS                  EXTNAME(YEURCODS)
002400941108     D* DS DELLE VARIE
002500990727     D DTASV         E DS
002600990727     D  SVR                    1     20
002700990727     D                                     DIM(20)
002800990727     D  VAR                   21    140P 3
002900990727     D                                     DIM(20)
003000990727     D  ESE                  141    160
003100941104     D                                     DIM(20)
003200030918
003300030918     D var_impon       s              1
003400030918     D diffe           s             13  3
003500941104     I*----------------------------------------------------------------
003600941108     C     *ENTRY        PLIST
003700990727     C                   PARM                    DSLV04
003800990727     C                   PARM                    DTASV
003900941108     I*----------------------------------------------------------------
004000941104     C* D04TLA =' '  ELABORO E CHIUDO CON RETRN
004100941104     C* D04TLA ='L'  ELABORO E CHIUDO CON LR
004200941104     C* D04TLA ='C'            CHIUDO CON LR   SOLO PER CHIUSURA FILE
004300930505     C*
004400941104    1C     D04TLA        IFEQ      'C'
004500930505     C                   SETON                                        LR
004600990727     C** SE RICHIAMATO, CHIUDO IL FILES
004700990727     C     CALV16        IFEQ      'S'
004800990727     C                   CLEAR                   DSLV16
004900990727     C                   MOVEL     'C'           D17TLA
005000990727     C                   CALL      'FNLV16R'
005100990727     C                   PARM                    DSLV16
005200990727     C                   ENDIF
005300930505     C*
005400930505   X1C                   ELSE
005500941108     C*
005600941108     C* PULIZIA CAMPI DI OUTPUT
005700941108     C                   CLEAR                   D04IVA
005800941108     C                   CLEAR                   D04BOL
005900941108     C                   CLEAR                   D04IFT
006000990727     C                   CLEAR                   W0133
006100950221     C                   CLEAR                   WCAS
006200990727     C                   MOVEL     0             TRE               1 0
006300990727     C                   MOVEL     D04TBL        FLGTBL            1
006400990727     C* ELIMINO DALLE VARIE LA VARIA DELL'IVA E DEL BOLLO
006500990727     C                   Z-ADD     1             X
006600990727     C     §$2BOL        LOOKUP    SVR(X)                                 90
006700990727     C   90              CLEAR                   VAR(X)
006800990727     C   90              CLEAR                   ESE(X)
006900990804*    C   90              CLEAR                   SVR(X)
007000990727     C                   Z-ADD     1             X
007100990728     C     §$2IVA        LOOKUP    SVR(X)                                 91
007200990728     C   91              CLEAR                   VAR(X)
007300990728     C   91              CLEAR                   ESE(X)
007400990804*    C   91              CLEAR                   SVR(X)
007500990728     C* SE HO TROVATO UNA DELLE 2 SISTEMO LA DS DELLE VARIE RIEMPIENDO
007600990728     C*  I BUCHI VUOTI
007700990728     C     *IN90         IFEQ      *ON
007800990728     C     *IN91         OREQ      *ON
007900990728     C                   CLEAR                   WSV
008000990728     C                   CLEAR                   WVA
008100990728     C                   CLEAR                   WES
008200990804*    C*?*                  Z-ADD1         X
008300990804*    C                   CLEAR                   X
008400990728     C                   DO        20            Y                 2 0
008500990728     C     SVR(Y)        IFNE      ' '
008600990804*    C                   ADD       1             X
008700990728     C                   MOVEL     SVR(Y)        WSV(X)
008800990728     C                   Z-ADD     VAR(Y)        WVA(X)
008900990728     C                   MOVEL     ESE(Y)        WES(X)
009000990728     C                   ENDIF
009100990728     C                   ENDDO
009200990728     C**
009300990728     C                   MOVEA     WSV           SVR
009400990728     C                   MOVEA     WVA           VAR
009500990728     C                   MOVEA     WES           ESE
009600990728     C                   ENDIF
009700930505     C*
009800991025     C** DIVISA  FATTURA
009900990928     C                   MOVEL     'CV'          COD
010000991025     C                   MOVEL(P)  D04DIV        KEY
010100990928     C     KTAB          CHAIN     TABEL                              30
010200990928     C  N30              MOVEL     TBLUNI        DSCV
010300990928     C   30              CLEAR                   DSCV
010400990928     C                   MOVEL     §CVFDC        WFDC
010500991011     C                   MOVEL     §CVDEC        WDEC              2 0
010600991025     C* CONTROLLO DEL C/ASSEGNO
010700991025    2C     D04CAS        IFGT      0
010800991025     C                   EXSR      CTRCAS
010900991025    2C                   END
011000991025     C* SE DEVO CALCOLARE IL TOALE IMPONIBILE, LO CALCOLO
011100991025     C     D04FIM        IFEQ      'T'
011200991025     C                   EXSR      CALTOT
011300991025     C                   ENDIF
011400991012     C*
011500991012     C* MEMORIZZO QUAL E' L'UNITA'MINIMA PER LA DIVISA
011600991012     C                   SELECT
011700991012     C     WDEC          WHENEQ    0
011800991012     C                   Z-ADD     1             WUNITA            5 3
011900991012     C     WDEC          WHENEQ    1
012000991012     C                   Z-ADD     0,1           WUNITA
012100991012     C     WDEC          WHENEQ    2
012200991012     C                   Z-ADD     0,01          WUNITA
012300991012     C                   OTHER
012400991012     C                   Z-ADD     0,001         WUNITA
012500991012     C                   ENDSL
012600930505     C**
012700930505    2C     TRE           DOWLT     2
012800991123     C                   CLEAR                   ESVAR
012900991123     C                   CLEAR                   NUM133
013000990727     C                   CLEAR                   W0133
013100941108     C**
013200930506     C* P R I M A   B O L L A
013300040121      * E   S E C O N D A   B O L L A
013400941108     C**
013500040121    3C**!!!D04SPE        IFEQ      '1'
013600990727     C* FRANCO PREPAGATO
013700040121     C**!!!D04SPE        OREQ      '8'
013800991123     C**
013900991123     C* NUM133=  VARIE ESENTI
014000991123    4C                   DO        20            X                 2 0
014100991123     C                   CLEAR                   PP
014200991123    5C     ESE(X)        IFNE      ' '
014300991123     C                   ADD       VAR(X)        NUM133           13 3
014400991123     C                   ADD       VAR(X)        ESVAR            13 3
014500991123     C**
014600991123     C* MEMORIZZO LA PRIMA POSIZIONE DI VARIA ESENTE
014700991123     C     PP            IFEQ      0
014800991123     C                   Z-ADD     X             PP                2 0
014900991123     C                   ENDIF
015000991123    5C                   END
015100991123    4C                   ENDDO
015200941108     C**
015300941108     C** IMPONIBILE NON ESENTE IVA
015400941104    4C     D04CEI        IFEQ      ' '
015500990928     C                   Z-ADD     D04IMV        WIMV
015600991123     C                   SUB       NUM133        WIMV
015700990928     C                   EXSR      CALIVA
015800030918      * se ho variato l'imponibile modifico anche D04IMV con il nuovo WIMV
015900030918      * impostato nella routine CALIVA e la differenza la sommo al porto
016000030918      *
016100030918     c                   if        Var_Impon = 'S'
016200030918     c                   eval      diffe = wimv - d04imv
016300030918      *
016400030918     c                   add       diffe         d04por
016500030918      *
016600030918
016700030918     c                   eval      D04Imv  = Wimv + Num133
016800030918     c                   endif
016900991123     C** SOMMO TUTTI GLI IMPORTI ESENTI IVA
017000991123     C                   ADD       NUM133        W0133
017100990928     C*
017200941108   X4C                   ELSE
017300941108     C** IMPONIBILE ESENTE IVA
017400941108     C                   MOVEL     *ZEROS        D04ALI
017500990727     C                   ADD       D04IMV        W0133            13 3
017600941108    4C                   ENDIF
017700941108     C*
017800991123     C** BOLLO
017900990928     C                   EXSR      CALBOL
018000930505     C*
018100990727     C* D04IFT= VARIE ESENTI + IVA O BOLLO + TOT.IMPONIBILE
018200930505     C*
018300990727     C     D04IMV        ADD       D04IVA        D04IFT
018400941104     C                   ADD       D04BOL        D04IFT
018500991123     C* LE VARIE ESENTI NE FANNO GIA' PARTE
018600991123     C*********            ADD  NUM133    D04IFT
018700930505     C*
018800040121   X3C**!!!              ELSE
018900930506     C*
019000930506     C* S E C O N D A   B O L L A
019100040121    4C**!!!D04CEI        IFEQ      ' '
019200040121     C**!!!TASEV1        ANDEQ     ' '
019300040121     C**!!!              Z-ADD     TASVA1        WIMV
019400040121     C**!!!              EXSR      CALIVA
019500030918      * se ho variato l'imponibile modifico anche TASVA1 con il nuovo WIMV
019600030918      * impostato nella routine CALIVA
019700040121     c**!!!              if        Var_Impon = 'S'
019800030918      *
019900040121     c**!!!              eval      TASVA1  = Wimv
020000040121     c**!!!              endif
020100930505     C*
020200040121   X4C**!!!              ELSE
020300040121     C**!!!              MOVEL     *ZEROS        D04ALI
020400040121     C**!!!              Z-ADD     TASVA1        W0133
020500991012     C* APPLICO BOLLO
020600040121     C**!!!              EXSR      CALBOL
020700040121    4C**!!!              ENDIF
020800930505     C*
020900040121     C**!!!D04BOL        ADD       D04IVA        D04IFT
021000040121     C**!!!              ADD       TASVA1        D04IFT
021100040121    3C**!!!              ENDIF
021200930505     C**
021300991011     C* ESEGUO ARROTONDAMENTO SE C'E' VALORE O SE I DECIMALI DEL
021400991011     C*  CAMPO TOTALE IMPONIBILE SONO DI PIU' DI QUELLI PREVISTI
021500991011     C*  DALLA DIVISA
021600991011     C                   EXSR      CTRARR
021700930505     C*
021800930505     C*
021900930505    2C                   END
022000990728     C* SCRIVO IVA E BOLLO NELLE VARIE
022100990728     C     D04IVA        IFGT      0
022200990728     C                   Z-ADD     1             X
022300990728     C     ' '           LOOKUP    SVR(X)                                 90
022400990728     C   90              MOVEL     §$2IVA        SVR(X)
022500990728     C   90              Z-ADD     D04IVA        VAR(X)
022600990728     C                   ENDIF
022700990728     C     D04BOL        IFGT      0
022800990728     C                   Z-ADD     1             X
022900990728     C     ' '           LOOKUP    SVR(X)                                 90
023000990728     C   90              MOVEL     §$2BOL        SVR(X)
023100990728     C   90              Z-ADD     D04BOL        VAR(X)
023200990728     C                   ENDIF
023300930505     C*
023400941104     C     D04TLA        IFEQ      ' '
023500930505     C                   RETURN
023600930505     C                   ELSE
023700930505     C                   SETON                                        LR
023800930505     C                   END
023900930505    1C                   END
024000990727     C*
024100990727     C*--- CALCOLO TOTALE IMPONIBILE ---------------------------------*
024200990727     C     CALTOT        BEGSR
024300990727     C                   CLEAR                   DSLV16
024400990727     C                   Z-ADD     D04POR        D17POR
024500990727     C                   MOVEL     'T'           D17FIM
024600990727     C                   CALL      'FNLV16R'
024700990727     C                   PARM                    DSLV16
024800990727     C                   PARM                    DTASV
024900990727     C*
025000990727     C                   Z-ADD     O17IMV        D04IMV
025100990727     C                   MOVEL     'S'           CALV16            1
025200990727     C                   ENDSR
025300990928     C*
025400990928     C*--- CARICO LA DGEI --------------------------------------------*
025500990928     C     CARGEI        BEGSR
025600990928     C*
025700990928     C     D04DIV        IFNE      SAVDIV
025800990928     C                   CLEAR                   DSBS02
025900990928     C                   MOVEL     'C'           T02MOD
026000990928     C                   MOVEL     'GEI'         T02COD
026100990928     C                   MOVEL     D04DIV        T02KE1
026200990928     C                   CALL      'TIBS02R'
026300990928     C                   PARM                    KPJBA
026400990928     C                   PARM                    DSBS02
026500990928    2C     T02ERR        IFEQ      ' '
026600990928     C                   MOVEL     T02UNI        DGEI
026700990928     C                   ELSE
026800990928     C                   CLEAR                   DGEI
026900990928     C                   ENDIF
027000990928     C*
027100990928     C                   MOVEL     D04DIV        SAVDIV
027200990928     C                   ENDIF
027300990928     C                   ENDSR
027400990928     C*
027500990928     C*--- CALCOLO IVA IN BASE AI DECIMALI ---------------------------*
027600990928     C     CALIVA        BEGSR
027700030918      *
027800030918      * pulisco il flag che segnala se ho variato imponibile IVA
027900030918      *
028000030918     c                   clear                   var_impon
028100030918     c                   clear                   diffe
028200030918
028300991012     C* ALIQUOTA IVA
028400991012    1C     D04ALI        IFEQ      0
028500991012     C* VERIFICO LA DATA FATTURA SE C'E'
028600991012    2C     D04DFT        IFEQ      0
028700991012     C     D04DFT        ORGT      §QTDSA
028800991012     C                   Z-ADD     §QTALI        D04ALI
028900991012     C                   ELSE
029000991012     C                   Z-ADD     §QTALO        D04ALI
029100991012    2C                   END
029200991012    1C                   END
029300991012     C**
029400991011     C* IVA IN LIRE: ARROTONDATA PER ECCESSO E SENZA DECIMALI
029500991011    1C     WFDC          IFEQ      ' '
029600990928     C     WIMV          MULT      D04ALI        CO2IVA
029700990928     C                   DIV       100           CO2IVA
029800990928     C     CO2IVA        ADD       0,99          CO0IVA           11 0
029900990928     C                   Z-ADD     CO0IVA        D04IVA
030000991011   X1C                   ELSE
030100990928     C* IVA IN EURO: ARROTONDATA MATEMATICAMENTE E CON 2 DECIMALI
030200030918      * se imponibile iva inferiore a 0,03 imposto l'imponibile a 0,03
030300030918     c                   if        wimv < 0,03
030400030918     c                   eval      wimv = 0,03
030500030918      * valorizzo il flag che segnala la variazione dell'imponibile
030600030918     c                   eval      var_impon = 'S'
030700030918     c                   endif
030800030918
030900991011    2C                   SELECT
031000030918      *
031100991011     C     WDEC          WHENEQ    1
031200991012     C     WIMV          MULT      D04ALI        CO2IVA
031300991012     C     CO2IVA        DIV(H)    100           CO1IVA           13 1
031400991011     C                   Z-ADD     CO1IVA        D04IVA
031500991011     C*
031600991011     C     WDEC          WHENEQ    2
031700990928     C     WIMV          MULT      D04ALI        CO2IVA           13 2
031800990928     C                   DIV(H)    100           CO2IVA
031900990928     C                   Z-ADD     CO2IVA        D04IVA
032000991011     C*
032100991011     C                   OTHER
032200991011     C     WIMV          MULT      D04ALI        CO3IVA           13 3
032300991011     C                   DIV(H)    100           CO3IVA
032400991011     C                   Z-ADD     CO3IVA        D04IVA
032500991011     C*
032600991011    2C                   ENDSL
032700991011    1C                   ENDIF
032800990928     C                   ENDSR
032900990928     C*
033000990928     C*--- CALCOLO BOLLO ---------------------------------------------*
033100990928     C     CALBOL        BEGSR
033200990928    4C     W0133         IFGT      0
033300990928     C* IN BASE ALLA DIVISA, PRENDO LA TABELLA DGEI
033400990928     C                   EXSR      CARGEI
033500990928     C*
033600990928    5C     W0133         IFGT      §GEIMB
033700990928     C                   Z-ADD     §GEIBL        D04BOL
033800990928     C                   ELSE
033900990928     C                   Z-ADD     0             D04BOL
034000990928    5C                   END
034100990928    4C                   END
034200990928     C                   ENDSR
034300991011     C**
034400991011     C* CONTROLLO SE DEVO ESEGUIRE GLI ARROTONDAMENTI ----------------*
034500991011     C     CTRARR        BEGSR
034600991011     C                   ADD       1             TRE
034700991012     C* CONTROLLO SE L'ARROTONDAMENTO E' CONSONO AI DECIMALI PREVISTI
034800991012     C*  DALLA DIVISA
034900991012     C     D04VAR        IFGT      0
035000991012     C                   Z-ADD     D04VAR        W0133
035100991011     C                   EXSR      DECIM
035200991012     C                   ELSE
035300991012     C*
035400991012     C* SE NON C'E' ARROTONDAMENTO IMPOSTO MINIMO VALORE ACCETTABILE
035500991012     C*  CONTABILMENTE DALLA DIVISA
035600991012     C                   Z-ADD     WUNITA        D04VAR
035700991012     C                   ENDIF
035800991012     C*
035900991011     C*
036000991011    1C     TRE           IFNE      2
036100991011    2C     D04VAR        IFGT      0
036200991011     C*
036300991011     C                   Z-ADD     D04IFT        XVALUE
036400991011     C* NUM133= VARIE ESENTI + C/ASSEGNO SE IN CONT E SE BOLLA ASSEGNAT
036500991011     C*         PERCHE' IL C/A E' INCASSATO DAL DESTINATARIO
036600991011     C                   ADD       D04BOL        NUM133
036700991011     C*
036800991011     C* SE IL C/A E' DA SOMMARE AL TOTALE DA ESIGERE
036900991012     C*  SOLO SE C'E' UN VALORE DI ARROTONDAMENTO
037000991012     C*  SE DEVO SOLO SISTEMARE I DECIMALI, NON SOMMO
037100991011    3C     WCAS          IFGT      0
037200991011     C     §1ASTE        ANDEQ     'S'
037300991011     C     D04SPE        ANDNE     '8'
037400991011     C     FLGTBL        ANDEQ     'A'
037500991011     C                   ADD       WCAS          XVALUE
037600991011     C                   ADD       WCAS          NUM133
037700991011    3C                   END
037800991011     C*
037900991011     C                   EXSR      ARROT
038000991011     C*
038100991011   X2C                   ELSE
038200991011     C                   Z-ADD     2             TRE
038300991011    2C                   END
038400991011    1C                   END
038500991011     C                   ENDSR
038600930505     C*
038700991011     C*--- CONTROLLO NUMERO DECIMALI ---------------------------------*
038800991011     C     DECIM         BEGSR
038900991012     C*
039000991011     C                   SELECT
039100991012     C     WDEC          WHENEQ    0
039200991012     C                   MOVE      W0133         W003A             3
039300991012     C     W003A         IFNE      '000'
039400991012     C     D04VAR        ADD       WUNITA        COMOD0
039500991012     C                   Z-ADD     COMOD0        D04VAR
039600991012     C                   ENDIF
039700991012     C*
039800991012     C     WDEC          WHENEQ    1
039900991012     C                   MOVE      W0133         W002A             2
040000991011     C     W002A         IFNE      '00'
040100991012     C     D04VAR        ADD       WUNITA        COMOD1
040200991012     C                   Z-ADD     COMOD1        D04VAR
040300991011     C                   ENDIF
040400991011     C*
040500991011     C     WDEC          WHENEQ    2
040600991012     C                   MOVE      W0133         W001A             1
040700991011     C     W001A         IFNE      '0'
040800991012     C     D04VAR        ADD       WUNITA        COMOD2
040900991012     C                   Z-ADD     COMOD2        D04VAR
041000991011     C                   ENDIF
041100991011     C                   ENDSL
041200991011     C                   ENDSR
041300991011     C*
041400991011     C*--- ARROTONDO TOTALE DA INCASSARE RICALCOLO PORTO E IVA--------*
041500991011     C     ARROT         BEGSR
041600991012     C                   CLEAR                   XCOM02
041700930505     C*
041800941104     C* D04AED: "D" DIFETTO   "E" ECCESSO
041900930505     C* XVALUE: VALORE DA ARROTONDARE
042000941104     C* D04VAR: ARROTONDAMENTO
042100930505     C*
042200990727     C     XVALUE        DIV       D04VAR        INTERO           15 0
042300930505     C                   MVR                     XCOM02
042400991012    2C     D04AED        IFEQ      'E'
042500991012    3C     XCOM02        IFGT      0
042600990727     C                   ADD       1             INTERO
042700991012    3C                   END
042800991012    2C                   END
042900991012     C*
043000991012     C     INTERO        MULT      D04VAR        XVALUE
043100930505     C*
043200991012     C* SOLO SE C'ERA RESTO ED HO ARROTONDATO
043300991012     C*  O SE DEVO SISTEMARE I DECIMALI, RIFACCIO IL CALCOLO
043400990727    1C     XCOM02        IFNE      0
043500930505     C*
043600930505     C* XCOM02 = NUOVO TOTALE DA ESIGERE ARROTONDATO
043700930505     C                   Z-ADD     XVALUE        XCOM02
043800930505     C*
043900990727     C                   SUB       NUM133        XVALUE
044000930505     C*
044100930505     C* VEDO SE LA VARIA E' ESENTE
044200040121    2C**!!!D04SPE        IFEQ      '2'
044300040121     C**!!!D04CEI        ANDEQ     ' '
044400040121     C**!!!              MOVEL     TASEV1        CEI
044500040121   X2C**!!!              ELSE
044600040121     C**!!!              MOVEL     D04CEI        CEI
044700040121    2C**!!!              END
044800930505     C*
044900930505     C*
045000930505     C* XVALUE = NUOVO TOTALE IMPONIBILE PIU' IVA
045100040121    2C**!!!CEI           IFEQ      ' '
045200040121    2C     d04cei        IFEQ      ' '
045300930505     C* SCORPORO DELL'IVA
045400941104     C     D04ALI        ADD       100           NUM3              3 0
045500990727     C     100           MULT      XVALUE        XCOM04
045600990727     C     XCOM04        DIV       NUM3          XCOM04
045700991011     C*
045800991011     C* TENGO I DECIMALI PREVISIT DALLA CONTABILITA' DELLA DIVISA
045900991011     C*  DA 0 A 3
046000990928     C*
046100990928     C* SE NON CI SONO DECIMALI, ARROTONDO ALLA LIRA
046200990928     C     WFDC          IFEQ      ' '
046300991011     C                   Z-ADD(H)  XCOM04        COMOD0           13 0
046400991011     C                   Z-ADD     COMOD0        XCOM04
046500991011     C                   ELSE
046600991011     C                   SELECT
046700991011     C     WDEC          WHENEQ    1
046800991011     C                   Z-ADD(H)  XCOM04        COMOD1           14 1
046900991011     C                   Z-ADD     COMOD1        XCOM04
047000991011     C     WDEC          WHENEQ    2
047100991011     C                   Z-ADD(H)  XCOM04        COMOD2           15 2
047200991011     C                   Z-ADD     COMOD2        XCOM04
047300991011     C                   OTHER
047400991011     C                   Z-ADD(H)  XCOM04        COMOD3           16 3
047500991011     C                   Z-ADD     COMOD3        XCOM04
047600991011     C                   ENDSL
047700991011     C                   ENDIF
047800930505     C*
047900990727     C* XCOM04 = NUOVO TOTALE IMPONIBILE
048000930505     C* RICALCOLO IVA
048100930505     C* XCOM03 = NUOVO TOTALE DA ESIGERE RICALCOLATO
048200930505     C                   MOVEL     ' '           DUE               1
048300930505     C*
048400930617    3C     DUE           DOWNE     '1'
048500990928     C                   Z-ADD     XCOM04        WIMV
048600990928     C                   EXSR      CALIVA
048700030918      * se ho variato l'imponibile modifico anche XCOM04 con il nuovo WIMV
048800030918      * impostato nella routine CALIVA
048900030918     c                   if        Var_Impon = 'S'
049000030918     c                   eval      XCOM04  = Wimv
049100030918     c                   endif
049200030918     C*
049300990928     C*
049400990928     C                   Z-ADD     D04IVA        XCOM03
049500990727     C                   ADD       XCOM04        XCOM03
049600990727     C                   ADD       NUM133        XCOM03
049700990928     C*
049800991012     C* SE RICALCOLANDO L'IVA DIVENTA > RIPROVO TOGLIENDO UNA UNITA'
049900991012     C*  AL TOTALE IMPONIBILE
050000991012     C*
050100930617    4C     XCOM03        IFGT      XCOM02
050200991012     C* MI SALVO L'IVA E RITENTO
050300991012     C                   Z-ADD     D04IVA        SAVIVA
050400991012     C                   SUB       WUNITA        XCOM04
050500930505     C*
050600930617   X4C                   ELSE
050700930505     C*
050800930505     C                   MOVEL     '1'           DUE
050900930505     C* SE AL 2 TENTATIVO DIVENTA < REIMPOSTO IL VALORE MAGGIORE
051000930505     C     XCOM03        IFLT      XCOM02
051100991012     C                   ADD       WUNITA        XCOM04
051200991012     C                   Z-ADD     SAVIVA        D04IVA
051300991012     C                   ENDIF
051400930505     C*
051500991012    4C                   ENDIF
051600991012    3C                   ENDDO
051700930505     C*
051800930617   X2C                   ELSE
051900930505     C*
052000990727     C* XVALUE = XCOM04 = NUOVO TOTALE IMPONIBILE (GIA' SENZA BOLLO
052100990727     C*          PERCHE' SOTRATTO NUM133)
052200990727     C                   Z-ADD     XVALUE        XCOM04
052300930617    2C                   END
052400930505     C*
052500040121    2C**!!!D04SPE        IFEQ      '1'
052600040121     C**!!!D04SPE        OREQ      '8'
052700991123     C     XCOM04        ADD       ESVAR         XCOM04
052800990727     C     XCOM04        SUB       D04IMV        XVALUE
052900941104     C                   ADD       XVALUE        D04POR
053000991123     C                   ADD       XVALUE        D04IMV
053100040121     C**!!!              ELSE
053200040121     C**!!!              Z-ADD     XCOM04        TASVA1
053300040121    2C**!!!              END
053400930505     C*
053500930617   X1C                   ELSE
053600930505     C                   MOVEL     2             TRE
053700930617    1C                   END
053800930505     C                   ENDSR
053900941104     C*
054000941104     C* CONTROLLO SE C/ASSEGNI IN CONTANTI E IN DIVISA ESTERA---------*
054100941104     C     CTRCAS        BEGSR
054200941104     C                   CLEAR                   WCAS
054300941104     C* TIPO INCASSO C/A PER VEDERE SE E' IN CONTANTI
054400941104     C                   MOVEL     '1A'          COD
054500991025     C                   MOVEL(P)  D04TIC        KEY
054600941104     C     KTAB          CHAIN     TABEL                              30
054700941104     C  N30              MOVEL     TBLUNI        DS1A
054800941104     C   30              CLEAR                   DS1A
054900941104     C*
055000941108     C* SE IL C/A E' DA SOMMARE AL TOTALE DA ESIGERE,
055100991025     C*  --> TRASFORMO L'IMPORTO IN DIVISA FATURA
055200941108    1C     §1ASTE        IFEQ      'S'
055300991025     C* SE DIVISA UGUALE A DIVISA FATURA NON DEVO APPLICARE IL CAMBIO
055400991025    2C     D04VCA        IFEQ      D04DIV
055500991025     C                   Z-ADD     D04CAS        WCAS             13 3
055600991025     C                   ELSE
055700991025     C                   CLEAR                   YEUR
055800991025     C                   MOVEL     D04VCA        YECDVI
055900991025     C                   MOVEL     D04DIV        YECDVO
056000991025     C                   Z-ADD     D04CAS        YECIMI
056100991025    3C     WFDC          IFEQ      'S'
056200991123     C                   MOVE      WDEC          YECDCO
056300991025     C                   ELSE
056400991123     C                   MOVE      '0'           YECDCO
056500991025     C                   ENDIF
056600991025     C*
056700991025     C                   CALL      'YEURCO'
056800991025     C                   PARM                    YEUR
056900991025     C*
057000991025    3C     YECESI        IFEQ      ' '
057100991025     C                   Z-ADD     YECIMO        WCAS
057200991025     C                   ELSE
057300991025     C                   MOVEL     ' '           §1ASTE
057400991025    3C                   ENDIF
057500991025    2C                   ENDIF
057600941104    1C                   ENDIF
057700941104     C*
057800941104     C                   ENDSR
057900941104     C***
058000941108     C* ROUTINE INIZIALE---------------------------------------------*
058100941108     C* CARICO CAMPI FISSI PER TASSAZIONE/BOLLETTAZIONE -------------*
058200941108     C     *INZSR        BEGSR
058300941108     C**
058400941108     C* KLIST
058500941108     C**
058600941108     C     KTAB          KLIST
058700941108     C                   KFLD                    CODUT             1 0
058800941108     C                   KFLD                    COD               2
058900941108     C                   KFLD                    KEY               8
059000941108     C**
059100941108     C* DEFINIZIONE CAMPI
059200941108     C**
059300040121     C**!!!*LIKE         DEFINE    D04CEI        CEI
059400990727     C     *LIKE         DEFINE    D04VAR        XCOM04
059500941108     C     *LIKE         DEFINE    D04VAR        XCOM02
059600941108     C     *LIKE         DEFINE    D04VAR        XCOM03
059700941108     C     *LIKE         DEFINE    D04VAR        XVALUE
059800990928     C     *LIKE         DEFINE    §CVFDC        WFDC
059900990928     C     *LIKE         DEFINE    D04IVA        SAVIVA
060000990928     C     *LIKE         DEFINE    D04DIV        SAVDIV
060100990928     C     *LIKE         DEFINE    D04IMV        WIMV
060200941108     C**
060300941115     C*  CARICO LE COSTANTI
060400941108     C**
060500941115     C                   Z-ADD     1             CODUT
060600941104     C                   MOVEL     'QT'          COD
060700941104     C                   MOVEL     '1       '    KEY
060800941104     C     KTAB          CHAIN     TABEL                              30
060900941104     C  N30              MOVEL     TBLUNI        DSQT1
061000941104     C   30              CLEAR                   DSQT1
061100990727     C                   MOVEL     '$2'          COD
061200990727     C                   MOVEL     '1       '    KEY
061300990727     C     KTAB          CHAIN     TABEL                              30
061400990727     C  N30              MOVEL     TBLUNI        DS$2
061500990727     C   30              CLEAR                   DS$2
061600990928     C*
061700941104     C                   ENDSR
