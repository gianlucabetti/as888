000100040506     H*--------------------------------------------------------------------------------------------*
000200040506     H DECEDIT('0,') DATEDIT(*DMY.)
000300040506     F*--------------------------------------------------------------------------------------------*
000400040506     F* Data base
000500040506     F*--------------------------------------------------------------------------------------------*
000600050512     FTMPFDX0F  UF   E             DISK    rename(TMPFDX0F:TMPFDX00)
000700050512     FWADLY05L  IF   E           K DISK
000800040506     D*--------------------------------------------------------------------------------------------*
000900040506     D*----------------
001000050512     D* DS
001100040506     D*----------------
001200050512     D tisi95ds      e ds
001300050512     D*------------------
001400050512     D* DS "XSRDA8" - CONTROLLA DATA (8)
001500050512     D*------------------
001600050512     D WLBDA8          DS                  INZ
001700050512     D  G08DAT                 1      8  0
001800050512     D  G08INV                 9     16  0
001900050512     D  G08ERR                17     17
002000050512     D  G08TGI                18     22  0
002100040506     C*--------------------------------------------------------------------------------------------*
002200040506     C* Main lines
002300040506     C*--------------------------------------------------------------------------------------------*
002400050512     C*
002500050512     C* Eseguo elaborazione
002600040506     C                   EXSR      EXEELAB
002700050512     C*
002800050512     C* Effettuo lancio TISI95 solo x chiusura
002900050512     C                   CLEAR                   TISI95DS
003000050512     C                   EVAL      I95TLA = 'C'
003100050512     C                   CALL      'TISI95R'
003200050512     C                   PARM                    TISI95DS
003300040506     C*
003400040506     C                   SETON                                        LR
003500040506     C*--------------------------------------------------------------------------------------------*
003600040506     C* EXEELAB - ELaborazione file
003700040506     C*--------------------------------------------------------------------------------------------*
003800040506     C     EXEELAB       BEGSR
003900050512     C*
004000050512     C* Imposto i valori chiave costanti
004100050512     C                   EVAL      dlyLNP = 050
004200050512     C                   EVAL      dlyTSP = 'C'
004300050512     C                   EVAL      dlyNAR = *blanks
004400040506     C*
004500050512     C* Leggo tutto il file d input
004600050512     C     *start        SETLL     TMPFDX0F
004700050512     C                   READ      TMPFDX0F
004800050512     C                   DOW       not %eof(TMPFDX0F)
004900050512     C*
005000050512     C* 1° giro: richiamo del TISI95 x reperimento codici tipo tempo
005100050512     C                   CLEAR                   TISI95DS
005200050512     C                   EVAL      I95TCN = '3'
005300050512     C                   Z-ADD     datcor        I95DAT
005400050512     C                   EVAL      I95CAP = CAP
005500050512     C                   EVAL      I95LOC = LOCALITA
005600050512     C                   CALL      'TISI95R'
005700050512     C                   PARM                    TISI95DS
005800050512     C*
005900050512     C                   IF        O95GF2 = '01'
006000050512     C                   EVAL      CUT_OFF = '12:30'
006100050512     C                   ELSE
006200050512     C                   IF        O95GF2 = '02'
006300050512     C                   EVAL      CUT_OFF = '19:00'
006400050512     C                   ELSE
006500050512     C                   EVAL      CUT_OFF = '99:99'
006600050512     C                   ENDIF
006700050512     C                   ENDIF
006800050512     C*
006900050512     C* 2° giro: aggancio del file DELIVERY x reperimento tempi cliente
007000050512     C                   EVAL      dlyCAP = I95CAP
007100050512     C*
007200050512     C* Provo prima a reperire x LOCALITA
007300050512     C                   EVAL      dlyLOC = O95LOC
007400050512     C     KEYdly5_c     CHAIN     WADLY05L
007500050512     C                   IF        %found(WADLY05L)
007600050512     C                   IF        dlyISO = 'D'
007700050512     C                   EVAL      DLYTRC = DLYTRC + 48
007800050512     C                   ENDIF
007900050512     C                   MOVEL(P)  DLYTRC        IB_TIME
008000050512     C                   EVAL      OB_TIME = IB_TIME
008100050512     C*
008200050512     C* Se NN trovato x LOCALITA scendo a livello d CAP
008300050512     C                   ELSE
008400050512     C                   EVAL      dlyLOC = *blanks
008500050512     C     KEYdly5_c     CHAIN     WADLY05L
008600050512     C                   IF        %found(WADLY05L)
008700050512     C                   IF        dlyISO = 'D'
008800050512     C                   EVAL      DLYTRC = DLYTRC + 48
008900050512     C                   ENDIF
009000050512     C                   MOVEL(P)  DLYTRC        IB_TIME
009100050512     C                   EVAL      OB_TIME = IB_TIME
009200050512     C*
009300050512     C* Se NN trovato nemmeno x CAP marco a ### x riconoscere l'errore
009400050512     C                   ELSE
009500050512     C                   EVAL      IB_TIME = '###'
009600050512     C                   EVAL      OB_TIME = IB_TIME
009700050512     C                   ENDIF
009800050512     C                   ENDIF
009900050512     C*
010000050512     C* Quindi aggiorno il file d input
010100050512     C                   UPDATE    TMPFDX00
010200050512     C*
010300050512     C* Quindi proseguo con l'elaborazione del file d input
010400050512     C                   READ      TMPFDX0F
010500050512     C                   ENDDO
010600970826     C*
010700970826     C                   ENDSR
010800040506     C*--------------------------------------------------------------------------------------------*
010900970826     C* *inzsr - operazioni iniziali
011000040506     C*--------------------------------------------------------------------------------------------*
011100970826     C     *inzsr        BEGSR
011200050512     C*
011300050512     C* CALCOLA LA DATA CORRENTE
011400050512     C                   time                    wn14             14 0
011500050512     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
011600050512     C                   z-add     wn8           g08dat
011700050512     C                   z-add     *zeros        g08inv
011800050512     C                   movel     '0'           g08err
011900050512     C                   call      'XSRDA8'
012000050512     C                   parm                    wlbda8
012100050512     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
012200970826     C*
012300050512     C* CHIAVI DI LETTURA
012400050512     C     KEYdly5_c     KLIST
012500050512     C                   KFLD                    dlylnp
012600050512     C                   KFLD                    dlytsp
012700050512     C                   KFLD                    dlynar
012800050512     C                   KFLD                    dlycap
012900050512     C                   KFLD                    dlyloc
013000050512     C*
013100970826     C                   ENDSR
