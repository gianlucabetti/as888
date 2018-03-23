000100151125     H Option(*SrcStmt)
000200160324     H DFTACTGRP(*NO) ACTGRP(*CALLER) BNDDIR('UBBNDDIR')
000300151126
000400151125     **-----------------------------------------------------------------------**
000500160324     ** TRANSPALLET PESATORE - CONTROLLI FORMALI DATI RICEVUTI
000600151126     **-----------------------------------------------------------------------**
000700151126
000800160324
000900160324     D*------------------
001000160324     D* VARIABILI X LA CHIAMATA AL *PGM UTILITA' "XSPLIT"
001100160324     D*------------------
001200160324     D InStringa       S          65535A   VARYING                              (stringa input)
001300160324     D InSepara        S             10A                                        (separatore)
001400160324     D InTypeOut       S              1A                                        (tipo output)
001500160324     D wSkParam        S          65535A                                        (output)
001600160324     D OutErrore       S              1A                                        (flag errore)
001700160324     D OutMsg          S             80A                                        (messaggio errore)
001800160324     D SkSplitCSV_6    S            512    DIM(128)
001900160324
002000160324
002100160324     D*------------------
002200160324     D* DS CALCOLO CHECK-DIGITS MOD 10 (cod. 103)
002300160324     D*------------------
002400160324     D TRUL28DS      E DS
002500160324
002600160324
002700160324     D*------------------
002800160324     D* DS OUTPUT
002900160324     D*------------------
003000160324     D DSOUT_TPP       DS
003100160324     D  §TPP_SEGNBRT                 18A
003200160324     D  §TPP_PESO_GR                  9S 0
003300160427     D  §TPP_IDDEVICE                30A
003400160324
003500160324
003600160324     D*-----------
003700160324     D* VARIABILI D WRK
003800160324     D*-----------
003900160324     D pInMessage      S           1024A
004000160324     D pInTypCtrl      S              3A
004100160324     D pOutDati        S           1024A
004200160324     D pOutResponse    S           1024A
004300160324     D pOutEsito       S              1N
004400160324     D wSegnacollo     S            128A   INZ
004500160324     D wPesoAlfa       S            128A   INZ
004600160427     D wIDdevice       S            128A   INZ
004700160324
004800160324
004900160324     D*------------------
005000160324     D* LINKING A DEFINIZIONI ESTERNE
005100160324     D*------------------
005200160324     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
005300160324     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
005400160331
005500160331
005600160331     D*-------------------
005700160331     D* COSTANTI
005800160331     D*-------------------
005900160331     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
006000160331     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
006100160323
006200151012
006300090520     C*
006400160324     C*
006500160324     C     *ENTRY        PLIST
006600160324     C                   parm                    pInMessage
006700160324     C                   parm                    pInTypCtrl
006800160324     C                   parm                    pOutDati
006900160324     C                   parm                    pOutResponse
007000160324     C                   parm                    pOutEsito
007100160324     C*
007200160324     C* Inizializzo subito i paramteri di output
007300160324     C                   clear                   pOutDati
007400160330     C                   eval      pOutResponse = '*VOID'
007500160324     C                   clear                   pOutEsito
007600160324     C*
007700160324     C* Applico il tipo controllo richiesto in input
007800160324     C                   select
007900160324     C                   when      pInTypCtrl = 'TPP'                           * Transpallet Pesat.
008000160324     C*
008100160324     C* Splitto il buffer ricevuto dal transpallet pesatore con delimitarore campo ';'
008200160324     C                   clear                   SkSplitCSV_6
008300160324     C                   eval      InStringa = pInMessage
008400160324     C                   eval      InSepara = ';'
008500160324     C                   exsr      SR_SPLIT
008600160324     C                   eval      wSegnacollo = %trim(SkSplitCSV_6(1))
008700160324     C                   eval      wPesoAlfa   = %trim(SkSplitCSV_6(2))
008800160427     C                   eval      wIDdevice   = %trim(SkSplitCSV_6(3))
008900160324     C*
009000160324     C* Inizializzo flag di esito controllo e DS output per tipo controllo richiesto
009100160324     C                   setoff                                       30
009200160324     C                   clear                   DSOUT_TPP
009300160324     C*
009400160324     C* 1 - Verifica segnacollo "verosimilmente" BRT
009500160324     C*
009600160324     C* 1.1 => Lungo 18
009700160324     C                   if        not *in30 AND
009800160324     C                             %len(%trim(wSegnacollo)) = 18
009900160324     C                   else
010000160324     C                   seton                                        30
010100160324     C                   endif
010200160324     C*
010300160324     C* 1.2 => Che bytes da 1 a 3 siano <> *zeros
010400160324     C                   if        not *in30
010500160324     C                   if        %subst(%trim(wSegnacollo):1:3) <> *all'0'
010600160324     C                   else
010700160324     C                   seton                                        30
010800160324     C                   endif
010900160324     C                   endif
011000160324     C*
011100160324     C* 1.3 => Che bytes da 4 a 6 siano <> *zeros
011200160324     C                   if        not *in30
011300160324     C                   if        %subst(%trim(wSegnacollo):4:3) <> *all'0'
011400160324     C                   else
011500160324     C                   seton                                        30
011600160324     C                   endif
011700160324     C                   endif
011800160324     C*
011900160324     C* 1.4 => Numerico
012000160324     C                   if        not *in30
012100160324     C                   eval      PiStr = wSegnacollo
012200160324     C                   exsr      CHKNUM
012300160324     C                   if        PiInt = *on
012400160324     C                   else
012500160324     C                   seton                                        30
012600160324     C                   endif
012700160324     C                   endif
012800160324     C*
012900160324     C* 1.5 => Verifica check-digits
013000160324     C                   if        not *in30
013100160324     C                   clear                   TRUL28DS
013200160324     C                   eval      I28MOD = 'BAR'
013300160324     C                   eval      I28COD = %subst(%trim(wSegnacollo):1:17)
013400160324     C                   call      'TRUL28R1'
013500160324     C                   parm                    TRUL28DS
013600160324     C                   if        O28ERR = *blanks AND
013700160324     C                             O28CKD = %subst(%trim(wSegnacollo):18:1)
013800160324     C                   eval      §TPP_SEGNBRT = O28COD
013900160324     C                   else
014000160324     C                   seton                                        30
014100160324     C                   endif
014200160324     C                   endif
014300160324     C*
014400160324     C* 2 - Verifica e conversione peso (da Kg a Gr)
014500160324     C                   if        not *in30
014600160331     C     minu:maiu     xlate     wPesoAlfa     wPesoAlfa
014700160331     C     'KG':'  '     xlate     wPesoAlfa     wPesoAlfa
014800160331     C     '.':','       xlate     wPesoAlfa     wPesoAlfa
014900160324     C                   eval      PiStr = wPesoAlfa
015000160324     C                   exsr      CHKNUM
015100160324     C                   if        PiNum = *on
015200160324     C                   eval      §TPP_PESO_GR = PiVal * 1000
015300160324     C                   else
015400160324     C                   seton                                        30
015500160324     C                   endif
015600160324     C                   endif
015700160324     C*
015800160324     C* Se tutto ok => esito controllo 'TPP' positivo
015900160324     C                   if        not *in30
016000160406     C                   call      'UBSOKTP2C'
016100160427     C                   eval      §TPP_IDDEVICE = wIDdevice
016200160324     C                   eval      pOutDati     = DSOUT_TPP
016300160324     C                   eval      pOutResponse = 'OK'
016400160324     C                   eval      pOutEsito    = *on
016500160324     C                   else
016600160324     C                   eval      pOutResponse = 'KO'
016700160324     C                   endif
016800160324     C*
016900160324     C                   endsl
017000160324     C*
017100160324     C                   seton                                        LR
017200160324     C***
017300160324
017400160324
017500160324     C*----------------------------------------------------*
017600160324     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
017700160324     C*----------------------------------------------------*
017800160324     C     SR_SPLIT      BEGSR
017900160324     C*
018000160324     C* Chiamo il *pgm d utilità x lo "split" della riga delle intestazioni
018100160324     C                   CALL      'XSPLIT2'
018200160324     C                   PARM                    InStringa
018300160324     C                   PARM                    InSepara
018400160324     C                   PARM      '6'           InTypeOut
018500160324     C                   PARM      ''            wSkParam
018600160324     C                   PARM                    OutErrore
018700160324     C                   PARM                    OutMsg
018800160324     C                   MOVEA     wSkParam      SkSplitCSV_6
018900160324     C*
019000160324     C                   ENDSR
019100160324     C***
019200160324
019300160324
019400160324     C*----------------------------------------------------*
019500160324     C*  CONTROLLO NUMERICITA' CAMPI
019600160324     C*----------------------------------------------------*
019700160324     C     CHKNUM        BEGSR
019800160324     C*
019900160324     C                   eval      PiDecChr = ','
020000160324     C*
020100160324     C                   callp     UBISNUM_Check(PiStr
020200160324     C                                          :PiDecChr
020300160324     C                                          :PiVal
020400160324     C                                          :PiNum
020500160324     C                                          :PiInt)
020600160324     C*
020700160324     C                   ENDSR
020800160324     C***
020900160324
