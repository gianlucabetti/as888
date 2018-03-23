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
003200160620     D pInChkFil       S              1A
003300160620     D pInChkDgt       S              1A
003400160620     D pOutEsitoOK     S              1N
003500160620
003600160620
003700160620     D*------------------
003800160620     D* DS RIDEFINIZIONE SEGNACOLLO BRT
003900160620     D*------------------
004000160620     D SEGNBRT         ds                  inz qualified
004100160620     D  LNP                           3  0
004200160620     D  LNA                           3  0
004300160620     D  NRS                           2  0
004400160620     D  NCD                           7  0
004500160620     D  ZNC                           2  0
004600160620     D  CHKDGT                        1
004700160324
004800160324
004900160324     D*------------------
005000160324     D* LINKING A DEFINIZIONI ESTERNE
005100160324     D*------------------
005200160324     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
005300160324     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
005400160323
005500151012
005600090520     C*
005700160324     C*
005800160324     C     *ENTRY        PLIST
005900160620     C                   parm                    pInBRT_SEGN
006000161018     C                   parm                    pInLNP01
006100161018     C                   parm                    pInLNP02
006200161018     C                   parm                    pInLNP03
006300161018     C                   parm                    pInLNP04
006400161018     C                   parm                    pInLNP05
006500161018     C                   parm                    pInNRS01
006600161018     C                   parm                    pInNRS02
006700161018     C                   parm                    pInNRS03
006800161018     C                   parm                    pInNRS04
006900161018     C                   parm                    pInNRS05
007000161018     C                   parm                    pInNRS06
007100161018     C                   parm                    pInNRS07
007200161018     C                   parm                    pInNRS08
007300161018     C                   parm                    pInNRS09
007400161018     C                   parm                    pInNRS10
007500160620     C                   parm                    pInChkFil
007600160620     C                   parm                    pInChkDgt
007700160620     C                   parm                    pOutEsitoOK
007800160620     C*
007900160324     C* Inizializzo subito i paramteri di output
008000160620     C                   clear                   pOutEsitoOK
008100160324     C*
008200160324     C* Inizializzo flag di esito controllo e DS output per tipo controllo richiesto
008300160324     C                   setoff                                       30
008400160620     C                   setoff                                       40
008500160621     C                   setoff                                       41
008600160324     C*
008700160620     C* Verifica se segnacollo è un BRT
008800160324     C*
008900160620     C* 1 => Lunghezza 18 (con check-digits) oppure 17 (senza check-digits)
009000160620     C                   if        not *in30
009100160620     C                   if        (pInChkDgt <> 'N' AND
009200160620     C                              %len(%trim(pInBRT_SEGN)) = 18) OR
009300160620     C                             (pInChkDgt  = 'N' AND
009400160620     C                              %len(%trim(pInBRT_SEGN)) = 17)
009500160324     C                   else
009600160324     C                   seton                                        30
009700160324     C                   endif
009800160620     C                   endif
009900160620     C*
010000160621     C* 2 => Filtro sulle 3 LNP disponibili
010100160620     C                   if        not *in30
010200161018     C                   if        (pInLNP01 <> *zeros AND
010300161018     C                             %subst(pInBRT_SEGN:1:3)=%editc(pInLNP01:'X'))
010400160621     C                             OR
010500161018     C                             (pInLNP02 <> *zeros AND
010600161018     C                             %subst(pInBRT_SEGN:1:3)=%editc(pInLNP02:'X'))
010700160621     C                             OR
010800161018     C                             (pInLNP03 <> *zeros AND
010900161018     C                             %subst(pInBRT_SEGN:1:3)=%editc(pInLNP03:'X'))
011000161018     C                             OR
011100161018     C                             (pInLNP04 <> *zeros AND
011200161018     C                             %subst(pInBRT_SEGN:1:3)=%editc(pInLNP04:'X'))
011300161018     C                             OR
011400161018     C                             (pInLNP05 <> *zeros AND
011500161018     C                             %subst(pInBRT_SEGN:1:3)=%editc(pInLNP05:'X'))
011600160620     C                   seton                                        40
011700160620     C                   else
011800160620     C                   seton                                        30
011900160620     C                   endif
012000160620     C                   endif
012100160620     C*
012200160621     C* 3 => Filtro sulle 6 NRS disponibili
012300160620     C                   if        not *in30
012400161018     C                   if        (pInNRS01 <> *zeros AND
012500161018     C                             %subst(pInBRT_SEGN:7:2)=%editc(pInNRS01:'X'))
012600160621     C                             OR
012700161018     C                             (pInNRS02 <> *zeros AND
012800161018     C                             %subst(pInBRT_SEGN:7:2)=%editc(pInNRS02:'X'))
012900160621     C                             OR
013000161018     C                             (pInNRS03 <> *zeros AND
013100161018     C                             %subst(pInBRT_SEGN:7:2)=%editc(pInNRS03:'X'))
013200160621     C                             OR
013300161018     C                             (pInNRS04 <> *zeros AND
013400161018     C                             %subst(pInBRT_SEGN:7:2)=%editc(pInNRS04:'X'))
013500160621     C                             OR
013600161018     C                             (pInNRS05 <> *zeros AND
013700161018     C                             %subst(pInBRT_SEGN:7:2)=%editc(pInNRS05:'X'))
013800160621     C                             OR
013900161018     C                             (pInNRS06 <> *zeros AND
014000161018     C                             %subst(pInBRT_SEGN:7:2)=%editc(pInNRS06:'X'))
014100161018     C                             OR
014200161018     C                             (pInNRS07 <> *zeros AND
014300161018     C                             %subst(pInBRT_SEGN:7:2)=%editc(pInNRS07:'X'))
014400161018     C                             OR
014500161018     C                             (pInNRS08 <> *zeros AND
014600161018     C                             %subst(pInBRT_SEGN:7:2)=%editc(pInNRS08:'X'))
014700161018     C                             OR
014800161018     C                             (pInNRS09 <> *zeros AND
014900161018     C                             %subst(pInBRT_SEGN:7:2)=%editc(pInNRS09:'X'))
015000161018     C                             OR
015100161018     C                             (pInNRS10 <> *zeros AND
015200161018     C                             %subst(pInBRT_SEGN:7:2)=%editc(pInNRS10:'X'))
015300160620     C                   else
015400160620     C                   seton                                        30
015500160620     C                   endif
015600160620     C                   endif
015700160620     C*
015800160620     C* 4 => Valore numerico
015900160620     C                   if        not *in30
016000160620     C                   eval      PiStr = pInBRT_SEGN
016100160620     C                   exsr      CHKNUM
016200160620     C                   if        PiInt = *on
016300160620     C                   eval      SEGNBRT = pInBRT_SEGN
016400160620     C                   else
016500160620     C                   seton                                        30
016600160620     C                   endif
016700160620     C                   endif
016800160621     C*
016900160621     C* 5 => Verifica se "Cappario 789"
017000160621     C                   if        not *in30
017100160621     C                   if        SEGNBRT.LNA = 789
017200160621     C                   seton                                        41
017300160621     C                   endif
017400160621     C                   endif
017500160324     C*
017600160621     C* 6 => Se richiesto verifica LNP in AZORG
017700160620     C                   if        not *in30 AND not *in40 AND pInChkFil <> 'N'
017800160620     C     SEGNBRT.LNP   chain     azorg01l
017900160620     C                   if        %found(azorg01l)
018000160324     C                   else
018100160324     C                   seton                                        30
018200160324     C                   endif
018300160324     C                   endif
018400160620     C*
018500160621     C* 7 => Se richiesto verifica LNA in AZORG
018600160621     C                   if        not *in30 AND not *in41 AND pInChkFil <> 'N'
018700160620     C     SEGNBRT.LNA   chain     azorg01l
018800160620     C                   if        %found(azorg01l)
018900160620     C                   else
019000160620     C                   seton                                        30
019100160620     C                   endif
019200160620     C                   endif
019300160324     C*
019400160621     C* 8 => Se richiesto verifica check-digits
019500160620     C                   if        not *in30 AND pInChkDgt <> 'N'
019600160324     C                   clear                   TRUL28DS
019700160324     C                   eval      I28MOD = 'BAR'
019800160620     C                   eval      I28COD = %subst(%trim(pInBRT_SEGN):1:17)
019900160324     C                   call      'TRUL28R1'
020000160324     C                   parm                    TRUL28DS
020100160324     C                   if        O28ERR = *blanks AND
020200160620     C                             O28CKD = %subst(%trim(pInBRT_SEGN):18:1)
020300160324     C                   else
020400160324     C                   seton                                        30
020500160324     C                   endif
020600160324     C                   endif
020700160324     C*
020800160620     C  N30              eval      pOutEsitoOK = *on
020900160620     C*
021000160621     C                   seton                                        RT
021100160621     C                   return
021200160324     C***
021300160324
021400160324
021500160324     C*----------------------------------------------------*
021600160324     C*  CONTROLLO NUMERICITA' CAMPI
021700160324     C*----------------------------------------------------*
021800160324     C     CHKNUM        BEGSR
021900160324     C*
022000160324     C                   eval      PiDecChr = ','
022100160324     C*
022200160324     C                   callp     UBISNUM_Check(PiStr
022300160324     C                                          :PiDecChr
022400160324     C                                          :PiVal
022500160324     C                                          :PiNum
022600160324     C                                          :PiInt)
022700160324     C*
022800160324     C                   ENDSR
022900160324     C***
023000160324
