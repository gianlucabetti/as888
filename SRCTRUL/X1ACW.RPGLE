000100010928      /TITLE Controllo tipo pagamento - fine validità divisa.
000200010928     ***********************************************************************
000300010928     **
000400010928     ** Controlla, per un tipo incasso, se è ammessa una divisa.
000500011002     ** Esempio: per il tipo incasso 'Bx' (assegno bancario)
000600011002     ** sarà possibile accettare 'ITL' fino al 31.12.2001.
000700010928     **
000800010928     ** Parametri di input:
000900010928     **
001000010928     ** Nome       Descrizione
001100011002     ** ---------- -----------------------------------------
001200010928     ** PRMTPINC   Tipo incasso.
001300010928     ** PRMDIVISA  Divisa incasso.
001400010928     ** PRMDATA    Data incasso.
001500010928     **            Se vuota si assume la data corrente.
001600011002     ** PRMDS1A    Dati tabella 1A.
001700011002     ** PRMDSCW    Dati tabella CW.
001800010928     **
001900011002     **
002000010928     ** Parametri di output:
002100010928     **
002200010928     ** Nome       Descrizione
002300011002     ** ---------- -----------------------------------------
002400011002     ** PRMESITO   '0' = eseguito senza errori.
002500011002     **            '1' = errore generico.
002600011002     **            '2' = ricevuti parametri formalmente sbagliati.
002700011002     **            'A' = errore tipo incasso - divisa.
002800011002     **            'B' = tabella '1A' inesistente.
002900011002     **            'C' = tabella 'CW' inesistente.
003000011002     ** PRMDS1A    Dati tabella 1A.
003100011002     ** PRMDSCW    Dati tabella CW.
003200010928     **
003300010928     ***********************************************************************
003400010928
003500010928     H DFTACTGRP(*NO) ACTGRP(*CALLER)
003600010928
003700010928     FTabel00F  IF   E           K DISK
003800011002     F                                     USROPN
003900010928
004000011002     D Eseguito        C                   '0'
004100011002     D Errore          C                   '1'
004200011002     D*
004300011002     D PrmTpInc        S              2
004400011002     D PrmDivisa       S              3
004500011002     D PrmData         S               D
004600011002     D PrmEsito        S              1
004700011002     D PrmDS1A         S
004800011002     D                                     LIKE(DS1A)
004900011002     D PrmDSCW         S
005000011002     D                                     LIKE(DSCW)
005100011107     D PrmData80       S              8  0
005200011002     D*
005300010928     D DS1A          E DS
005400011002     D                                     INZ
005500010928     D DSCW          E DS
005600011002     D                                     INZ
005700011002     D SavDS1A         S
005800011002     D                                     LIKE(DS1A)
005900011002     D SavDSCW         S
006000011002     D                                     LIKE(DSCW)
006100011002     D SavTpInc        S
006200011002     D                                     LIKE(PrmTpInc)
006300011002     D SavDivisa       S
006400011002     D                                     LIKE(PrmDivisa)
006500010928     D WrkCwDlc        S               D
006600010928
006700010928     C     *ENTRY        PLIST
006800010928     C                   PARM                    PrmTpInc
006900010928     C                   PARM                    PrmDivisa
007000010928     C                   PARM                    PrmData
007100010928     C                   PARM                    PrmEsito
007200011002     C                   PARM      DS1A          PrmDS1A
007300011002     C                   PARM      DSCW          PrmDSCW
007400011107     C                   PARM                    PrmData80
007500011002     C*
007600011002     C                   CLEAR                   DS1A
007700011002     C                   CLEAR                   DSCW
007800010928     C*
007900010928     C                   EXSR      ChkParm
008000010928     C*
008100011002     C                   IF        PrmDS1A = *BLANK
008200011002     C                   EXSR      RtvTab1A
008300011002     C                   ELSE
008400011002     C                   EVAL      DS1A = PrmDS1A
008500011002     C                   ENDIF
008600010928     C*
008700010928     C                   IF        §1ACdl = 'S'
008800010928     C*
008900011002     C                   IF        PrmDSCW = *BLANK
009000011002     C                   EXSR      RtvTabCW
009100011002     C                   ELSE
009200011002     C                   EVAL      DSCW = PrmDSCW
009300011002     C                   ENDIF
009400011002     C* Divisa non più valida per il tipo incasso specificato.
009500011002     C                   MOVE      §CWDlc        WrkCwDlc
009600010928     C                   IF        PrmData > WrkCwDlc
009700011002     C                   EVAL      PrmEsito = 'A'
009800010928     C                   ENDIF
009900010928     C*
010000010928     C                   ENDIF
010100010928     C*
010200010928     C                   EXSR      Uscita
010300011002
010400011002     ** Reperimento tabella '1A'.
010500011002     C     RtvTab1A      BEGSR
010600011002     C*
010700040107     C*                  IF        SavTpInc = PrmTpInc
010800040107     C*                  EVAL      DS1A = SavDS1A
010900040107     C*                  ELSE
011000011002     C                   EVAL      TblCod = '1A'
011100011002     C                   CLEAR                   TblKey
011200011002     C                   EVAL      TblKey = PrmTpInc
011300011002     C                   EXSR      ChainTabel
011400011002     C                   EVAL      DS1A = TblUni
011500011002     C                   EVAL      SavDS1A = DS1A
011600011002     C                   EVAL      SavTpInc = PrmTpInc
011700040107     C*                  ENDIF
011800011002     C*
011900011002     C                   ENDSR
012000011002
012100011002     ** Reperimento tabella 'CW'.
012200011002     C     RtvTabCW      BEGSR
012300011002     C*
012400011002     C                   IF        SavDivisa = PrmDivisa
012500011002     C                   EVAL      DSCW = SavDSCW
012600011002     C                   ELSE
012700011002     C                   EVAL      TblCod = 'CW'
012800011002     C                   CLEAR                   TblKey
012900011002     C                   EVAL      TblKey = PrmDivisa
013000011002     C                   EXSR      ChainTabel
013100011002     C                   EVAL      DSCW = TblUni
013200011002     C                   EVAL      SavDSCW = DSCW
013300011002     C                   EVAL      SavDivisa = PrmDivisa
013400011002     C                   ENDIF
013500011002     C*
013600011002     C                   ENDSR
013700010928
013800010928     ** Lettura TABEL00F.
013900010928     C     ChainTabel    BEGSR
014000011002     C*
014100011002     C                   IF        NOT %OPEN(Tabel00F)
014200011002     C                   OPEN      Tabel00F
014300011002     C                   ENDIF
014400010928     C*
014500010928     C     K03Tab00      CHAIN     Tabel00F
014600010928     C*
014700011002     C                   IF        NOT %FOUND(Tabel00F)
014800011002     C                   SELECT
014900011002     C                   WHEN      TblCod = '1A'
015000011002     C                   EVAL      PrmEsito = 'B'
015100011002     C                   WHEN      TblCod = 'CW'
015200011002     C                   EVAL      PrmEsito = 'C'
015300011002     C                   OTHER
015400011002     C                   EVAL      PrmEsito = Errore
015500011002     C                   ENDSL
015600010928     C                   EXSR      Uscita
015700010928     C                   ENDIF
015800010928     C*
015900010928     C                   ENDSR
016000010928
016100010928     ** Controllo parametri ricevuti.
016200010928     C     ChkParm       BEGSR
016300010928     C*
016400010928     C                   CLEAR                   PrmEsito
016500010928     C*
016600040102     C*                  IF        PrmTpInc = *BLANK
016700040102     C*                            OR
016800040102     C                   IF        PrmDivisa = *BLANK
016900011002     C                   EVAL      PrmEsito = '2'
017000010928     C                   EXSR      Uscita
017100010928     C                   ENDIF
017200040102     C*
017300050518     C**                 IF        PrmTpInc = 'SC'
017400050518     C**                 EVAL      PrmTpInc = *BLANK
017500050518     C**                 ENDIF
017600011107     C*
017700011107     C                   IF        %PARMS > 6
017800011107     C                             AND
017900011107     C                             PrmData80 <> 0
018000011107     C                   MOVE      PrmData80     PrmData
018100011107     C                   ENDIF
018200010928     C*
018300010928     C                   IF        PrmData = *LOVAL
018400010928     C                   MOVE      *DATE         PrmData
018500010928     C                   ENDIF
018600010928     C*
018700010928     C                   ENDSR
018800010928
018900010928     ** Operazioni finali.
019000010928     C     Uscita        BEGSR
019100010928     C*
019200010928     C                   IF        PrmEsito = *BLANK
019300011002     C                   EVAL      PrmEsito = '0'
019400010928     C                   ENDIF
019500010928     C*
019600010928     C                   RETURN
019700010928     C*
019800010928     C                   ENDSR
019900010928
020000010928     ** Operazioni iniziali.
020100010928     C     *INZSR        BEGSR
020200010928     C*
020300010928     C     K03Tab00      KLIST
020400010928     C                   KFLD                    TblKut
020500010928     C                   KFLD                    TblCod
020600010928     C                   KFLD                    TblKey
020700010928     C*
020800010928     C                   EVAL      TblKut = 1
020900010928     C*
021000010928     C                   ENDSR
