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
003300040224     D UT�DSE0F      E DS
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
005700040224     D  CMZOrI       E
005800040224     D                                     INZ(*ZERO)
005900040224     D  CMZDtI       E
006000040224     D                                     INZ(*ZERO)
006100040224     D  CMZAdd       E
006200040224     D                                     INZ(Aggiunta)
006300040224     D  CMZClr       E
006400040224     D                                     INZ(Si)
006500040224     D  CMZFTr       E
006600040224     D                                     INZ(Si)
006700040224     D  CMZAzn       E
006800040224     D                                     INZ(AzioneRicezione)
006900040224
007000040224     D/COPY QSYSINC/QRPGLESRC,QUSEC
007100040224     D/COPY QSYSINC/QRPGLESRC,QCAPCMD
007200040224     D PCmd01          S            256                                         Source cmd string
007300040224     D PCmd02          S             10I 0                                      Length source string
007400040224     D                                     INZ(%SIZE(PCmd01))
007500040224     D PCmd04          S             10I 0                                      Length options ctl
007600040224     D                                     INZ(%SIZE(QCAP0100))
007700040224     D PCmd05          S              8                                         Opt. control format
007800040224     D                                     INZ('CPOP0100')
007900040224     D PCmd06          S                                                        Changed cmd string
008000040224     D                                     LIKE(PCmd01)
008100040224     D PCmd07          S             10I 0                                      Length changed strin
008200040224     D                                     INZ(%SIZE(PCmd06))
008300040224     D PCmd08          S             10I 0                                      Length changed retur
008400040224     D Wrk236          S            236
008500040224     D WrkNrR          S             10U 0
008600040224
008700040224     C     *ENTRY        PLIST
008800040224     C                   PARM                    KPJBA
008900040224
009000040224     ** Avvio il controllo sincronia.
009100040224     C                   EVAL      PCmd01 = 'STRCMTCTL LCKLVL(*CHG)'
009200040224     C                   EXSR      CalQCAPCMD
009300040224     C                   IF        QUSEI <> *BLANK AND
009400040224     C                             QUSEI <> ControlloSincroniaGiaAttivo
009500040224     C                   EVAL      *INH1 = *ON
009600040224     C                   EXSR      Uscita
009700040224     C                   ENDIF
009800040224
009900040224     C                   OPEN      FNNCR00F
010000040224     C                   OPEN      FNNCR00T
010100040224
010200040224     C                   DO        *HIVAL
010300040224     C
010400040224     C                   READ(E)   FNNCR00F
010500040224     C
010600040224     C                   SELECT
010700040224     C                   WHEN      (%ERROR AND %STATUS = RecordGiaAllocato)
010800040224     C                             OR %EOF
010900040224     C                   LEAVE
011000040224     C                   WHEN      %ERROR
011100040224     C                   EVAL      *INH3 = *ON
011200040224     C                   LEAVE
011300040224     C                   ENDSL
011400040224     C
011500040224     C                   EVAL      WrkNrR = WrkNrR + 1
011600040224     C                   WRITE     FNNCR0T0
011700040224     C                   DELETE    FNNCR000
011800040224     C                   COMMIT
011900040224     C
012000040224     C                   ENDDO
012100040224
012200040224     C                   CLOSE     FNNCR00F
012300040224     C                   CLOSE     FNNCR00T
012400040224
012500040224     C                   EXSR      Trasmissione
012600040224     C                   EXSR      Uscita
012700040224
012800040224     ***********************************************************************
012900040224     **
013000040224     ** Trasmissione in sede.
013100040224     **
013200040224     ***********************************************************************
013300040224     C     Trasmissione  BEGSR
013400040224
013500040224     C                   IF        WrkNrR <> *ZERO
013600040224
013700040224     C                   CALL      'X�PARUT'
013800040224     C                   PARM                    UT�DSE0F
013900040224     C                   EVAL      CNCR80 = Rec80
014000040224
014100040224     C                   EVAL      I50PSI = KNSIF
014200040224     C                   EVAL      I50PPO = SIMFEL
014300040224     C                   CALL      'TIBS55R'
014400040224     C                   PARM                    TIBS55DS
014500040224
014600040224     C                   IF        O50Err = Errore
014700040224     C                   EVAL      *INH2 = *ON
014800040224     C                   EXSR      Uscita
014900040224     C                   ENDIF
015000040224
015100040224     C                   MOVE      WrkNrR        CMZNrR
015200040224
015300040224     C                   CALL      'TIBS53C'
015400040224     C                   PARM                    O50LCL
015500040224     C                   PARM                    CMZDST
015600040224     C                   PARM                    CMZFLD
015700040224     C                   PARM                    CMZMBD
015800040224     C                   PARM                    O50PLA
015900040224     C                   PARM                    CMZFLA
016000040224     C                   PARM                    CMZMBA
016100040224     C                   PARM                    O50ALA
016200040224     C                   PARM                    CMZORI
016300040224     C                   PARM                    CMZDTI
016400040224     C                   PARM                    CMZURG
016500040224     C                   PARM                    CMZADD
016600040224     C                   PARM                    CMZCLR
016700040224     C                   PARM                    CMZFTR
016800040224     C                   PARM                    CMZERR
016900040224     C                   PARM                    CMZAZN
017000040224     C                   PARM                    O50ASI
017100040224     C                   PARM                    CMZNRR
017200040224     C                   PARM                    CMZPRR
017300040224     C                   PARM                    Wrk236
017400040224
017500040224     C                   ENDIF
017600040224
017700040224     C                   ENDSR
017800040224
017900040224     ***********************************************************************
018000040224     **
018100040224     ** Chiamata a QCAPCMD.
018200040224     **
018300040224     ***********************************************************************
018400040224     C     CalQCAPCMD    BEGSR
018500040224
018600040224     C                   CLEAR                   QCAP0100
018700040224     C                   EVAL      QCABCSDH = IgnoreDBCSData
018800040224     C                   EVAL      QCAPA = NeverPromptTheCommand
018900040224     C                   EVAL      QCACMDSS = UseAS400Syntax
019000040224     C                   EVAL      QCAERVED = X'000000000000000000'
019100040224     C                   CLEAR                   QUSEC
019200040224     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
019300040224
019400040224     C                   CALL      'QCAPCMD'
019500040224     C                   PARM                    PCmd01
019600040224     C                   PARM                    PCmd02
019700040224     C                   PARM                    Qcap0100
019800040224     C                   PARM                    PCmd04
019900040224     C                   PARM                    PCmd05
020000040224     C                   PARM      *BLANK        PCmd06
020100040224     C                   PARM                    PCmd07
020200040224     C                   PARM                    PCmd08
020300040224     C                   PARM                    QUSEC
020400040224
020500040224     C                   ENDSR
020600040224
020700040224     ***********************************************************************
020800040224     **
020900040224     ** Operazioni finali.
021000040224     **
021100040224     ***********************************************************************
021200040224     C     Uscita        BEGSR
021300040224
021400040224     ** Fine controllo sincronia.
021500040224     C                   EVAL      PCmd01 = 'ENDCMTCTL'
021600040224     C                   EXSR      CalQCAPCMD
021700040224
021800040224     C                   EVAL      *INLR = *ON
021900040224     C                   RETURN
022000040224
022100040224     C                   ENDSR
