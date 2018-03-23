000100040224      /TITLE Trasmissione in sede nelle note accredito automatiche.
000200040224
000300040224     H DFTACTGRP(*NO) ACTGRP(*NEW)
000400040224
000500040224     FFNNCR00F  UF   E             DISK
000600040224     F                                     USROPN
000700040224     F                                     COMMIT
000800040224     FFNNCR00T  O    E             DISK
000900040224     F                                     USROPN
001000040224     F                                     COMMIT
001100040224     F                                     RENAME(FNNCR000:FNNCR0T0)
001200040224
001300040224     D Errore          C                   '1'
001400040224     D LR              C                   'L'
001500040224     D Si              C                   'S'
001600040224     D nSede           C                   46
001700040224     D cSede           C                   '046'
001800040224     D Aggiunta        C                   'A'
001900040224     D AzioneRicezione...
002000040224     D                 C                   'YF99'
002100040224     D RecordGiaAllocato...
002200040224     D                 C                   1218
002300040224     D ControlloSincroniaGiaAttivo...
002400040224     D                 C                   'CPF8351'
002500040224     D IgnoreDBCSData...
002600040224     D                 C                   '0'
002700040224     D NeverPromptTheCommand...
002800040224     D                 C                   '0'
002900040224     D UseAS400Syntax...
003000040224     D                 C                   '0'
003100040224
003200040224     D KPJBA         E DS
003300040224     D UT§DSE0F      E DS
003400040224     D                                     INZ
003500040224     D  CodUt        E
003600040224     D                                     INZ(1)
003700040224     D CNCR80        E DS
003800040224     D                                     INZ
003900040224     D TIBS55DS      E DS
004000040224     D                                     INZ
004100040224     D  I50TLa       E
004200040224     D                                     INZ(LR)
004300040224     D  I50APO       E
004400040224     D                                     INZ(nSede)
004500040224     D DSCMZ         E DS
004600040224     D                                     INZ
004700040224     D  CMZDst       E
004800040224     D                                     INZ(cSede)
004900040224     D  CMZFlD       E
005000040224     D                                     INZ('FNNCR00T')
005100040224     D  CMZMbD       E
005200040506     D                                     INZ('FNNCR00T')
005300040224     D  CMZFlA       E
005400040224     D                                     INZ('FNNCR00R')
005500040224     D  CMZMbA       E
005600040506     D                                     INZ('FNNCR00R')
005700040512     D  CMZLbA       E
005800040512     D                                     INZ('GAITRA201')
005900040224     D  CMZOrI       E
006000040224     D                                     INZ(*ZERO)
006100040224     D  CMZDtI       E
006200040224     D                                     INZ(*ZERO)
006300040224     D  CMZAdd       E
006400040224     D                                     INZ(Aggiunta)
006500040224     D  CMZClr       E
006600040224     D                                     INZ(Si)
006700040224     D  CMZFTr       E
006800040224     D                                     INZ(Si)
006900040224     D  CMZAzn       E
007000040224     D                                     INZ(AzioneRicezione)
007100040512     D  CMZSyf       E
007200040512     D                                     INZ('GAITRA201')
007300040224
007400040224     D/COPY QSYSINC/QRPGLESRC,QUSEC
007500040224     D/COPY QSYSINC/QRPGLESRC,QCAPCMD
007600040224     D PCmd01          S            256                                         Source cmd string
007700040224     D PCmd02          S             10I 0                                      Length source string
007800040224     D                                     INZ(%SIZE(PCmd01))
007900040224     D PCmd04          S             10I 0                                      Length options ctl
008000040224     D                                     INZ(%SIZE(QCAP0100))
008100040224     D PCmd05          S              8                                         Opt. control format
008200040224     D                                     INZ('CPOP0100')
008300040224     D PCmd06          S                                                        Changed cmd string
008400040224     D                                     LIKE(PCmd01)
008500040224     D PCmd07          S             10I 0                                      Length changed strin
008600040224     D                                     INZ(%SIZE(PCmd06))
008700040224     D PCmd08          S             10I 0                                      Length changed retur
008800040224     D Wrk236          S            236
008900040224     D WrkNrR          S             10U 0
009000040224
009100040224     C     *ENTRY        PLIST
009200040224     C                   PARM                    KPJBA
009300040224
009400040224     ** Avvio il controllo sincronia.
009500040224     C                   EVAL      PCmd01 = 'STRCMTCTL LCKLVL(*CHG)'
009600040224     C                   EXSR      CalQCAPCMD
009700040224     C                   IF        QUSEI <> *BLANK AND
009800040224     C                             QUSEI <> ControlloSincroniaGiaAttivo
009900040224     C                   EVAL      *INH1 = *ON
010000040224     C                   EXSR      Uscita
010100040224     C                   ENDIF
010200040224
010300040224     C                   OPEN      FNNCR00F
010400040224     C                   OPEN      FNNCR00T
010500040224
010600040224     C                   DO        *HIVAL
010700040224     C
010800040224     C                   READ(E)   FNNCR00F
010900040224     C
011000040224     C                   SELECT
011100040224     C                   WHEN      (%ERROR AND %STATUS = RecordGiaAllocato)
011200040224     C                             OR %EOF
011300040224     C                   LEAVE
011400040224     C                   WHEN      %ERROR
011500040224     C                   EVAL      *INH3 = *ON
011600040224     C                   LEAVE
011700040224     C                   ENDSL
011800040224     C
011900040224     C                   EVAL      WrkNrR = WrkNrR + 1
012000040224     C                   WRITE     FNNCR0T0
012100040224     C                   DELETE    FNNCR000
012200040224     C                   COMMIT
012300040224     C
012400040224     C                   ENDDO
012500040224
012600040224     C                   CLOSE     FNNCR00F
012700040224     C                   CLOSE     FNNCR00T
012800040224
012900040224     C                   EXSR      Trasmissione
013000040224     C                   EXSR      Uscita
013100040224
013200040224     ***********************************************************************
013300040224     **
013400040224     ** Trasmissione in sede.
013500040224     **
013600040224     ***********************************************************************
013700040224     C     Trasmissione  BEGSR
013800040224
013900040224     C                   IF        WrkNrR <> *ZERO
014000040224
014100040224     C                   CALL      'X§PARUT'
014200040224     C                   PARM                    UT§DSE0F
014300040224     C                   EVAL      CNCR80 = Rec80
014400040224
014500040224     C                   EVAL      I50PSI = KNSIF
014600040224     C                   EVAL      I50PPO = SIMFEL
014700040224     C                   CALL      'TIBS55R'
014800040224     C                   PARM                    TIBS55DS
014900040224
015000040224     C                   IF        O50Err = Errore
015100040224     C                   EVAL      *INH2 = *ON
015200040224     C                   EXSR      Uscita
015300040224     C                   ENDIF
015400040224
015500040224     C                   MOVE      WrkNrR        CMZNrR
015600040224
015700040224     C                   CALL      'TIBS53C'
015800040224     C                   PARM                    O50LCL
015900040224     C                   PARM                    CMZDST
016000040224     C                   PARM                    CMZFLD
016100040224     C                   PARM                    CMZMBD
016200040224     C                   PARM                    O50PLA
016300040224     C                   PARM                    CMZFLA
016400040224     C                   PARM                    CMZMBA
016500040512     C                   PARM                    CMZLBA
016600040224     C                   PARM                    CMZORI
016700040224     C                   PARM                    CMZDTI
016800040224     C                   PARM                    CMZURG
016900040224     C                   PARM                    CMZADD
017000040224     C                   PARM                    CMZCLR
017100040224     C                   PARM                    CMZFTR
017200040224     C                   PARM                    CMZERR
017300040224     C                   PARM                    CMZAZN
017400040512     C                   PARM                    CMZSYF
017500040224     C                   PARM                    CMZNRR
017600040224     C                   PARM                    CMZPRR
017700040224     C                   PARM                    Wrk236
017800040224
017900040224     C                   ENDIF
018000040224
018100040224     C                   ENDSR
018200040224
018300040224     ***********************************************************************
018400040224     **
018500040224     ** Chiamata a QCAPCMD.
018600040224     **
018700040224     ***********************************************************************
018800040224     C     CalQCAPCMD    BEGSR
018900040224
019000040224     C                   CLEAR                   QCAP0100
019100040224     C                   EVAL      QCABCSDH = IgnoreDBCSData
019200040224     C                   EVAL      QCAPA = NeverPromptTheCommand
019300040224     C                   EVAL      QCACMDSS = UseAS400Syntax
019400040224     C                   EVAL      QCAERVED = X'000000000000000000'
019500040224     C                   CLEAR                   QUSEC
019600040224     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
019700040224
019800040224     C                   CALL      'QCAPCMD'
019900040224     C                   PARM                    PCmd01
020000040224     C                   PARM                    PCmd02
020100040224     C                   PARM                    Qcap0100
020200040224     C                   PARM                    PCmd04
020300040224     C                   PARM                    PCmd05
020400040224     C                   PARM      *BLANK        PCmd06
020500040224     C                   PARM                    PCmd07
020600040224     C                   PARM                    PCmd08
020700040224     C                   PARM                    QUSEC
020800040224
020900040224     C                   ENDSR
021000040224
021100040224     ***********************************************************************
021200040224     **
021300040224     ** Operazioni finali.
021400040224     **
021500040224     ***********************************************************************
021600040224     C     Uscita        BEGSR
021700040224
021800040224     ** Fine controllo sincronia.
021900040224     C                   EVAL      PCmd01 = 'ENDCMTCTL'
022000040224     C                   EXSR      CalQCAPCMD
022100040224
022200040224     C                   EVAL      *INLR = *ON
022300040224     C                   RETURN
022400040224
022500040224     C                   ENDSR
