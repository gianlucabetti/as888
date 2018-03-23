000100000414
000200000414     H DECEDIT('0,') DATEDIT(*DMY.)
000300000414
000400001120     D*-----------------
000500001120     D* VARIABILI DI WRK
000600001120     D*-----------------
000700090319     D SimbolOK        C                   CONST(',;: ')
000800090319     D*SimbolOK        C                   CONST(',;.:')
000900090319     D ElencoNum       C                   CONST('1234567890')
001000010430     D WrkStr          S            200    varying
001100010430     D PosChar         S              5i 0 INZ(*zeros)
001200090319     D giri            S              3  0 INZ(*zeros)
001300010430     D LenTxtInput     S              5i 0 INZ(*zeros)
001400010430     D LenWrkStr       S              5i 0 INZ(*zeros)
001500090319     D wTxtInputNorm   S            200    varying
001600090319     D TotSimbols      S              3  0 INZ(*zeros)
001700090319     D TotSpaces       S              3  0 INZ(*zeros)
001800090319     D TotChars        S              3  0 INZ(*zeros)
001900010430     D SingleChar      S              1
002000090319     D SingleCharNext  S              1
002100010430     D flgOK           S              1    INZ('0')
002200001120
002300001120     D*-----------------
002400010430     D* PARAMETRI DI INGRESSO PROGRAMMA
002500001120     D*-----------------
002600010430     D TxtInput        S            200    varying
002700010430     D Lung            S              5i 0
002800010430     D TxtGiust        S            200    varying
002900010430     D TxtResto        S            200    varying
003000001120
003100000411
003200000414     C*------------------------------------------------------------------------*
003300000414     C* MAIN LINE
003400000414     C*------------------------------------------------------------------------*
003500090319     C*
003600090319     C                   eval      wTxtInputNorm = %trim(TxtInput)
003700090319     C*
003800090319     C* Conto gli spazi totali presenti nel testo da giustificare
003900090319     C                   exsr      cntSpaces
004000000414     C*
004100010430     C* Prima di tutto imposto le lunghezze stringhe iniziali
004200010430     C                   eval      %len(WrkStr) = 1
004300010430     C                   eval      %len(TxtResto) = 1
004400010430     C                   eval      %len(TxtGiust) = Lung
004500090319     C                   eval      LenTxtInput = %len(%trim(wTxtInputNorm))
004600010430     C*
004700090319     C* Eseguo routine di effettiva "giustificazione" testo passato solo se
004800090319     C* caratteri "spaziabili" sono meno dello spazio libero sulla riga / 4.
004900090319     C                   if        (Lung-TotChars) / 4 > TotSimbols
005000090319     C                   eval      TxtGiust = %trim(wTxtInputNorm)
005100090319     C                   else
005200010430     C                   exsr      exeGiust
005300090319     C                   endif
005400001120     C*
005500001120     C                   SETON                                        LR
005600090319     C*------------------------------------------------------------------------*
005700001120
005800001120
005900000414
006000001120     C*------------------------------------------------------------------------*
006100010430     C* EXEGIUST - OPERAZIONI DI "GIUSTIFICAZIONE" TESTO
006200001120     C*------------------------------------------------------------------------*
006300010430     C     EXEGIUST      BEGSR
006400001120     C*
006500010430     C* Inizio la scansione e la "giustificazione"
006600010430     C                   eval      flgOK = '0'
006700090319     C                   eval      WrkStr = %trim(wTxtInputNorm)
006800010430     C                   eval      LenWrkStr  = %len(%trim(WrkStr))
006900010430     C                   eval      PosChar = 1
007000010430     C                   eval      giri = 1
007100010430     C*
007200010430     C                   dow       flgOK = '0' and
007300010430     C                             PosChar <= LenWrkStr
007400090319     C                   setoff                                       55
007500010430     C                   eval      SingleChar=%subst(WrkStr:PosChar:1)
007600010430     C     SingleChar    scan      SimbolOK
007700010430     C                   if        %found
007800090319     C*
007900090319     C* Skippo se trattasi d caratteri numerici
008000090319     C                   eval      SingleCharNext=%subst(WrkStr:PosChar+1:1)
008100090319     C     SingleCharNextscan      ElencoNum
008200090319     C                   if        %found
008300090319     C                   seton                                        55
008400090319     C                   endif
008500090319     C*
008600090319     C                   if        *in55 = *off
008700010430     C                   eval      LenWrkStr = %len(%trim(WrkStr))
008800010430     C                   if        LenWrkStr < Lung
008900010430     C                   eval      %len(WrkStr) = LenWrkStr + 1
009000010430     C                   eval      WrkStr = %subst(WrkStr:1:PosChar) + ' ' +
009100010430     C                             %subst(WrkStr:PosChar+1:(LenWrkStr-PosChar))
009200010430     C                   eval      PosChar = PosChar + giri + 1
009300010430     C                   else
009400010430     C                   eval      flgOK = '1'
009500010430     C                   endif
009600090319     C*
009700090319     C                   else
009800090319     C                   eval      PosChar = PosChar + 1
009900090319     C                   endif
010000090319     C*
010100010430     C                   else
010200010430     C                   eval      PosChar = PosChar + 1
010300010430     C                   endif
010400010430     C                   if        PosChar >= LenWrkStr and
010500010430     C                             giri    <  LenWrkStr
010600010430     C                   eval      PosChar = 1
010700010430     C                   eval      giri = giri + 1
010800010430     C                   endif
010900010430     C                   enddo
011000010430     C*
011100010430     C* Una volta terminato il ciclo valorizzo i valori di ritorno.
011200010430     C                   eval      TxtGiust = WrkStr
011300010430     C*                  eval      TxtResto =
011400010430     C*                            %subst(WrkStr:PosChar:(LenWrkStr-PosChar))
011500001120     C*
011600001120     C                   ENDSR
011700001120     C*------------------------------------------------------------------------*
011800090319
011900090319
012000090319
012100090319
012200090319     C*------------------------------------------------------------------------*
012300090319     C* CNTSPACES - CONTEGGIO NUMERO SPAZI PRESENTI NEL TESTO DA GIUTIFICARE
012400090319     C*------------------------------------------------------------------------*
012500090319     C     CNTSPACES     BEGSR
012600090319     C*
012700090319     C                   z-add     *zeros        TotSimbols
012800090319     C                   z-add     *zeros        TotSpaces
012900090319     C                   z-add     *zeros        TotChars
013000090319     C                   z-add     1             i                 3 0
013100090319     C*
013200090319     C                   dow       i <= %len(wTxtInputNorm)
013300090319     C                   eval      SingleChar=%subst(wTxtInputNorm:i:1)
013400090319     C     SingleChar    scan      SimbolOK
013500090319     C                   if        %found
013600090319     C                   add       1             TotSimbols
013700090319     C                   endif
013800090319     C                   if        SingleChar = *blanks
013900090319     C                   add       1             TotSpaces
014000090319     C                   else
014100090319     C                   add       1             TotChars
014200090319     C                   endif
014300090319     C                   add       1             i
014400090319     C                   enddo
014500090319     C*
014600090319     C                   ENDSR
014700090319     C*------------------------------------------------------------------------*
014800000411
014900001120
015000001120
015100001120
015200000414     C*------------------------------------------------------------------------*
015300000414     C* *INZSR - OPERAZIONI INIZIALI
015400000414     C*------------------------------------------------------------------------*
015500000414     C     *INZSR        BEGSR
015600001120     C*
015700000414     C*--------------------
015800000414     C* RICEVIMENTO PARAMETRI
015900000414     C*--------------------
016000000414     C     *ENTRY        PLIST
016100010430     C                   PARM                    TxtInput
016200010430     C                   PARM                    Lung
016300010430     C                   PARM                    TxtGiust
016400010430     C                   PARM                    TxtResto
016500000414     C*
016600000414     C                   ENDSR
