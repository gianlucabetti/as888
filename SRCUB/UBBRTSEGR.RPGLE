000100151125     H Option(*SrcStmt)
000200160324     H DFTACTGRP(*NO) ACTGRP(*CALLER) BNDDIR('UBBNDDIR')
000300151126
000400160620     Fazorg01l  IF   E           K DISK
000500160324
000600160324
000700160324     D*------------------
000800160324     D* DS CALCOLO CHECK-DIGITS MOD 10 (cod. 103)
000900160324     D*------------------
001000160324     D TRUL28DS      E DS
001100160324
001200160324
001300160324     D*-----------
001400160324     D* VARIABILI D WRK
001500160324     D*-----------
001600160620     D pInBRT_SEGN     S             18A
001700161018     D pInLNP01        S              3S 0
001800161018     D pInLNP02        S              3S 0
001900161018     D pInLNP03        S              3S 0
002000161018     D pInLNP04        S              3S 0
002100161018     D pInLNP05        S              3S 0
002200161018     D pInNRS01        S              2S 0
002300161018     D pInNRS02        S              2S 0
002400161018     D pInNRS03        S              2S 0
002500161018     D pInNRS04        S              2S 0
002600161018     D pInNRS05        S              2S 0
002700161018     D pInNRS06        S              2S 0
002800161018     D pInNRS07        S              2S 0
002900161018     D pInNRS08        S              2S 0
003000161018     D pInNRS09        S              2S 0
003100161018     D pInNRS10        S              2S 0
003200170928     D pInNRS11        S              2S 0
003300170928     D pInNRS12        S              2S 0
003400170928     D pInNRS13        S              2S 0
003500170928     D pInNRS14        S              2S 0
003600170928     D pInNRS15        S              2S 0
003700170928     D pInNRS16        S              2S 0
003800170928     D pInNRS17        S              2S 0
003900170928     D pInNRS18        S              2S 0
004000170928     D pInNRS19        S              2S 0
004100170928     D pInNRS20        S              2S 0
004200160620     D pInChkFil       S              1A
004300160620     D pInChkDgt       S              1A
004400160620     D pOutEsitoOK     S              1N
004500160620
004600160620
004700160620     D*------------------
004800160620     D* DS RIDEFINIZIONE SEGNACOLLO BRT
004900160620     D*------------------
005000160620     D SEGNBRT         ds                  inz qualified
005100160620     D  LNP                           3  0
005200160620     D  LNA                           3  0
005300160620     D  NRS                           2  0
005400160620     D  NCD                           7  0
005500160620     D  ZNC                           2  0
005600160620     D  CHKDGT                        1
005700160324
005800160324
005900160324     D*------------------
006000160324     D* LINKING A DEFINIZIONI ESTERNE
006100160324     D*------------------
006200160324     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
006300160324     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
006400160323
006500151012
006600090520     C*
006700160324     C*
006800160324     C     *ENTRY        PLIST
006900160620     C                   parm                    pInBRT_SEGN
007000161018     C                   parm                    pInLNP01
007100161018     C                   parm                    pInLNP02
007200161018     C                   parm                    pInLNP03
007300161018     C                   parm                    pInLNP04
007400161018     C                   parm                    pInLNP05
007500161018     C                   parm                    pInNRS01
007600161018     C                   parm                    pInNRS02
007700161018     C                   parm                    pInNRS03
007800161018     C                   parm                    pInNRS04
007900161018     C                   parm                    pInNRS05
008000161018     C                   parm                    pInNRS06
008100161018     C                   parm                    pInNRS07
008200161018     C                   parm                    pInNRS08
008300161018     C                   parm                    pInNRS09
008400161018     C                   parm                    pInNRS10
008500170928     C                   parm                    pInNRS11
008600170928     C                   parm                    pInNRS12
008700170928     C                   parm                    pInNRS13
008800170928     C                   parm                    pInNRS14
008900170928     C                   parm                    pInNRS15
009000170928     C                   parm                    pInNRS16
009100170928     C                   parm                    pInNRS17
009200170928     C                   parm                    pInNRS18
009300170928     C                   parm                    pInNRS19
009400170928     C                   parm                    pInNRS20
009500160620     C                   parm                    pInChkFil
009600160620     C                   parm                    pInChkDgt
009700160620     C                   parm                    pOutEsitoOK
009800160620     C*
009900160324     C* Inizializzo subito i paramteri di output
010000160620     C                   clear                   pOutEsitoOK
010100160324     C*
010200160324     C* Inizializzo flag di esito controllo e DS output per tipo controllo richiesto
010300160324     C                   setoff                                       30
010400160620     C                   setoff                                       40
010500160621     C                   setoff                                       41
010600160324     C*
010700160620     C* Verifica se segnacollo è un BRT
010800160324     C*
010900160620     C* 1 => Lunghezza 18 (con check-digits) oppure 17 (senza check-digits)
011000160620     C                   if        not *in30
011100160620     C                   if        (pInChkDgt <> 'N' AND
011200160620     C                              %len(%trim(pInBRT_SEGN)) = 18) OR
011300160620     C                             (pInChkDgt  = 'N' AND
011400160620     C                              %len(%trim(pInBRT_SEGN)) = 17)
011500160324     C                   else
011600160324     C                   seton                                        30
011700160324     C                   endif
011800160620     C                   endif
011900160620     C*
012000160621     C* 2 => Filtro sulle 3 LNP disponibili
012100160620     C                   if        not *in30
012200161018     C                   if        (pInLNP01 <> *zeros AND
012300161018     C                             %subst(pInBRT_SEGN:1:3)=%editc(pInLNP01:'X'))
012400160621     C                             OR
012500161018     C                             (pInLNP02 <> *zeros AND
012600161018     C                             %subst(pInBRT_SEGN:1:3)=%editc(pInLNP02:'X'))
012700160621     C                             OR
012800161018     C                             (pInLNP03 <> *zeros AND
012900161018     C                             %subst(pInBRT_SEGN:1:3)=%editc(pInLNP03:'X'))
013000161018     C                             OR
013100161018     C                             (pInLNP04 <> *zeros AND
013200161018     C                             %subst(pInBRT_SEGN:1:3)=%editc(pInLNP04:'X'))
013300161018     C                             OR
013400161018     C                             (pInLNP05 <> *zeros AND
013500161018     C                             %subst(pInBRT_SEGN:1:3)=%editc(pInLNP05:'X'))
013600160620     C                   seton                                        40
013700160620     C                   else
013800160620     C                   seton                                        30
013900160620     C                   endif
014000160620     C                   endif
014100160620     C*
014200170928     C* 3 => Filtro sulle NRS disponibili
014300160620     C                   if        not *in30
014400161018     C                   if        (pInNRS01 <> *zeros AND
014500161018     C                             %subst(pInBRT_SEGN:7:2)=%editc(pInNRS01:'X'))
014600160621     C                             OR
014700161018     C                             (pInNRS02 <> *zeros AND
014800161018     C                             %subst(pInBRT_SEGN:7:2)=%editc(pInNRS02:'X'))
014900160621     C                             OR
015000161018     C                             (pInNRS03 <> *zeros AND
015100161018     C                             %subst(pInBRT_SEGN:7:2)=%editc(pInNRS03:'X'))
015200160621     C                             OR
015300161018     C                             (pInNRS04 <> *zeros AND
015400161018     C                             %subst(pInBRT_SEGN:7:2)=%editc(pInNRS04:'X'))
015500160621     C                             OR
015600161018     C                             (pInNRS05 <> *zeros AND
015700161018     C                             %subst(pInBRT_SEGN:7:2)=%editc(pInNRS05:'X'))
015800160621     C                             OR
015900161018     C                             (pInNRS06 <> *zeros AND
016000161018     C                             %subst(pInBRT_SEGN:7:2)=%editc(pInNRS06:'X'))
016100161018     C                             OR
016200161018     C                             (pInNRS07 <> *zeros AND
016300161018     C                             %subst(pInBRT_SEGN:7:2)=%editc(pInNRS07:'X'))
016400161018     C                             OR
016500161018     C                             (pInNRS08 <> *zeros AND
016600161018     C                             %subst(pInBRT_SEGN:7:2)=%editc(pInNRS08:'X'))
016700161018     C                             OR
016800161018     C                             (pInNRS09 <> *zeros AND
016900170928     C                             %subst(pInBRT_SEGN:7:2)=%editc(pInNRS09:'X'))
017000161018     C                             OR
017100161018     C                             (pInNRS10 <> *zeros AND
017200161018     C                             %subst(pInBRT_SEGN:7:2)=%editc(pInNRS10:'X'))
017300170928     C                             OR
017400170928     C                             (pInNRS11 <> *zeros AND
017500170928     C                             %subst(pInBRT_SEGN:7:2)=%editc(pInNRS11:'X'))
017600170928     C                             OR
017700170928     C                             (pInNRS12 <> *zeros AND
017800170928     C                             %subst(pInBRT_SEGN:7:2)=%editc(pInNRS12:'X'))
017900170928     C                             OR
018000170928     C                             (pInNRS13 <> *zeros AND
018100170928     C                             %subst(pInBRT_SEGN:7:2)=%editc(pInNRS13:'X'))
018200170928     C                             OR
018300170928     C                             (pInNRS14 <> *zeros AND
018400170928     C                             %subst(pInBRT_SEGN:7:2)=%editc(pInNRS14:'X'))
018500170928     C                             OR
018600170928     C                             (pInNRS15 <> *zeros AND
018700170928     C                             %subst(pInBRT_SEGN:7:2)=%editc(pInNRS15:'X'))
018800170928     C                             OR
018900170928     C                             (pInNRS16 <> *zeros AND
019000170928     C                             %subst(pInBRT_SEGN:7:2)=%editc(pInNRS16:'X'))
019100170928     C                             OR
019200170928     C                             (pInNRS17 <> *zeros AND
019300170928     C                             %subst(pInBRT_SEGN:7:2)=%editc(pInNRS17:'X'))
019400170928     C                             OR
019500170928     C                             (pInNRS18 <> *zeros AND
019600170928     C                             %subst(pInBRT_SEGN:7:2)=%editc(pInNRS18:'X'))
019700170928     C                             OR
019800170928     C                             (pInNRS19 <> *zeros AND
019900170928     C                             %subst(pInBRT_SEGN:7:2)=%editc(pInNRS19:'X'))
020000170928     C                             OR
020100170928     C                             (pInNRS20 <> *zeros AND
020200170928     C                             %subst(pInBRT_SEGN:7:2)=%editc(pInNRS20:'X'))
020300160620     C                   else
020400160620     C                   seton                                        30
020500160620     C                   endif
020600160620     C                   endif
020700160620     C*
020800160620     C* 4 => Valore numerico
020900160620     C                   if        not *in30
021000160620     C                   eval      PiStr = pInBRT_SEGN
021100160620     C                   exsr      CHKNUM
021200160620     C                   if        PiInt = *on
021300160620     C                   eval      SEGNBRT = pInBRT_SEGN
021400160620     C                   else
021500160620     C                   seton                                        30
021600160620     C                   endif
021700160620     C                   endif
021800160621     C*
021900160621     C* 5 => Verifica se "Cappario 789"
022000160621     C                   if        not *in30
022100160621     C                   if        SEGNBRT.LNA = 789
022200160621     C                   seton                                        41
022300160621     C                   endif
022400160621     C                   endif
022500160324     C*
022600160621     C* 6 => Se richiesto verifica LNP in AZORG
022700160620     C                   if        not *in30 AND not *in40 AND pInChkFil <> 'N'
022800160620     C     SEGNBRT.LNP   chain     azorg01l
022900160620     C                   if        %found(azorg01l)
023000160324     C                   else
023100160324     C                   seton                                        30
023200160324     C                   endif
023300160324     C                   endif
023400160620     C*
023500160621     C* 7 => Se richiesto verifica LNA in AZORG
023600160621     C                   if        not *in30 AND not *in41 AND pInChkFil <> 'N'
023700160620     C     SEGNBRT.LNA   chain     azorg01l
023800160620     C                   if        %found(azorg01l)
023900160620     C                   else
024000160620     C                   seton                                        30
024100160620     C                   endif
024200160620     C                   endif
024300160324     C*
024400160621     C* 8 => Se richiesto verifica check-digits
024500160620     C                   if        not *in30 AND pInChkDgt <> 'N'
024600160324     C                   clear                   TRUL28DS
024700160324     C                   eval      I28MOD = 'BAR'
024800160620     C                   eval      I28COD = %subst(%trim(pInBRT_SEGN):1:17)
024900160324     C                   call      'TRUL28R1'
025000160324     C                   parm                    TRUL28DS
025100160324     C                   if        O28ERR = *blanks AND
025200160620     C                             O28CKD = %subst(%trim(pInBRT_SEGN):18:1)
025300160324     C                   else
025400160324     C                   seton                                        30
025500160324     C                   endif
025600160324     C                   endif
025700160324     C*
025800160620     C  N30              eval      pOutEsitoOK = *on
025900160620     C*
026000160621     C                   seton                                        RT
026100160621     C                   return
026200160324     C***
026300160324
026400160324
026500160324     C*----------------------------------------------------*
026600160324     C*  CONTROLLO NUMERICITA' CAMPI
026700160324     C*----------------------------------------------------*
026800160324     C     CHKNUM        BEGSR
026900160324     C*
027000160324     C                   eval      PiDecChr = ','
027100160324     C*
027200160324     C                   callp     UBISNUM_Check(PiStr
027300160324     C                                          :PiDecChr
027400160324     C                                          :PiVal
027500160324     C                                          :PiNum
027600160324     C                                          :PiInt)
027700160324     C*
027800160324     C                   ENDSR
027900160324     C***
028000160324
