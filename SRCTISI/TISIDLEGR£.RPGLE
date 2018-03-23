000100081209     H DECEDIT('0,') DATEDIT(*YMD.)
000200170918      *--------------------------------------------------------------*
000300170918     FTABEL00F  IF   E           K DISK
000400170918     FDPLEG13l  uF A E           K DISK
000500170918      *--------------------------------------------------------------*
000600170918     D WLBDA8          DS                  INZ
000700170918     D  G08DAT                 1      8  0
000800170918     D  G08INV                 9     16  0
000900170918     D  G08ERR                17     17
001000170918     D  G08TGI                18     22  0
001100170918     D*----------------
001200170918     D* DS ESTERNE
001300170918     D*----------------
001400170918     D DS15          E DS                  INZ
001500170918      *---
001600170918     D skNaz           S              2    DIM(1000)
001700170918     D skNazKey        S              3    DIM(1000)
001800170918     D jNaz            S              4  0 INZ
001900170918      *------------------------------------------------------------------------*
002000170918      * RICEVIMENTO PARAMETRI
002100170918     C     *ENTRY        PLIST
002200170918     C                   PARM                    prmDEP7           7
002300170918     C                   PARM                    prmNAZISO2        2
002400170918     C*
002500170918     c                   exsr      CARTAB
002600170918     C* DPLEG13L - Completa
002700170918     C     KEYleg13      KLIST
002800170918     C                   KFLD                    prmDEP7
002900170918     C                   KFLD                    datcor
003000170918     C*
003100170918     C* TABEL00F - Parziale
003200170918     C     KEYtabel_P    KLIST
003300170918     C                   KFLD                    tblKUT
003400170918     C                   KFLD                    tblCOD
003500170918      **
003600170918     C* CALCOLA LA DATA CORRENTE
003700170918     C                   time                    wn14             14 0
003800170918     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
003900170918     C                   movel     wn14          oracor            6 0          *ORA
004000170918     C*
004100170918     C                   z-add     wn8           g08dat
004200170918     C                   z-add     *zeros        g08inv
004300170918     C                   movel     '0'           g08err
004400170918     C                   call      'XSRDA8'
004500170918     C                   parm                    wlbda8
004600170918     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
004700170918     C*
004800170918     C* Verifico se depot corrente già presente nel file d raccordo con P.d.C.
004900170918     C                   MOVEL     'S'           wFlgWri           1
005000170918     C     prmDEP7       SETLL     DPleg13L
005100170918     C                   IF        %equal(DPleg13L)
005200170918     C*
005300170918     C* Verifico se presente o cmq presente record valido x il depot corrente
005400170918     C     KEYleg13      SETLL     DPleg13L
005500170918     C                   IF        %found(DPleg13L)
005600170918     C*
005700170918     C     prmDEP7       reade     DPleg13L
005800170918     c                   dow       not %EoF(DPleg13L)
005900170918     C*
006000170918     C* Verifica validità record
006100170918     C                   IF        legDEP7  = prmDEP7 AND
006200170918     C                             legDDE  <= datcor  AND  datcor <= legDSC
006300170918     C                   MOVEL     'N'           wFlgWri
006400170918     C* Aggiorna la nazione passata da DPD sul file del legame.
006500170918     C                   EVAL      legNAR = prmNaz
006600170918     C                   update    DPLEG000
006700170918     C                   leave
006800170918     C                   ENDIF
006900170918     C*
007000170918     C     prmDEP7       reade     DPleg13L
007100170918     C                   ENDdo
007200170918     C*
007300170918     C                   ENDIF
007400170918     C                   ENDIF
007500170918     C*
007600170918     C                   IF        wFlgWri = 'S'
007700170918     C* ...altrimenti inserisco il depot corrente
007800170918     C                   CLEAR                   dpleg000
007900170918     c                   movel     prmDEP7       legBUN
008000170918     c                   move      prmDEP7       legDPC
008100170918     C                   EVAL         legDEP7 = prmDEP7
008200170918     C                   EVAL         legNAR  = prmNaz
008300170918     C                   EVAL         legKSC  = *zeros
008400170918     C                   EVAL         legDDE  = datcor
008500170918     C                   EVAL         legDSC  = 20391231
008600170918     C                   EVAL         legDTV  = datcor
008700170918     C                   EVAL         legORV  = oracor
008800170918     C                   EVAL         legPRU  = 'BATCH'
008900170918     C                   WRITE(e)  dpleg000
009000170918     C                   ENDIF
009100170918     C*
009200170918     C                   SETON                                        LR
009300170918     c                   return
009400170918     C*------------------------------------------------------------------------*
009500170918     C* CARTAB - CARICAMENTO DATI TABELLATI
009600170918     C*------------------------------------------------------------------------*
009700170918     C     cartab        BEGSR
009800170918     C*
009900170918     C* Reperisco tutte le nazioni (ISO2) servite da DPD
010000170918     C                   Z-ADD     *zeros        jNaz
010100170918     C                   CLEAR                   skNaz
010200170918     C                   CLEAR                   skNazKey
010300170918     C                   CLEAR                   DS15
010400170918     C                   CLEAR                   prmNAZ            2
010500170918     C*
010600170918     C                   EVAL      tblKUT = 1
010700170918     C                   EVAL      tblCOD = '15'
010800170918     C*
010900170918     C     KEYtabel_P    SETLL     TABEL00F
011000170918     C                   IF        %found(TABEL00F)
011100170918     C     KEYtabel_P    READE     TABEL00F
011200170918     C                   DOW       not %eof(TABEL00F)
011300170918      *-
011400170918     C                   IF        tblFLG = *blanks
011500170918     C                   EVAL      DS15 = tblUNI
011600170918     C                   IF        §15LAD > *zeros OR §15RED = 'S'
011700170918     C                   ADD       1             jNaz
011800170918     C                   MOVEL(P)  §15COD        skNaz(jNaz)
011900170918     C                   MOVEL(P)  tblKEY        skNazKey(jNaz)
012000170918      *-
012100170918      *- Nazione trascodificata
012200170918     c                   if        §15COD = prmNAZISO2
012300170918     C                   MOVEL(P)  tblKEY        prmNAZ
012400170918     c                   end
012500170918      *-
012600170918     C                   ENDIF
012700170918     C                   ENDIF
012800170918      *-
012900170918     C     KEYtabel_P    READE     TABEL00F
013000170918     C                   ENDDO
013100170918     C                   ENDIF
013200170918     C*
013300170918     C                   ENDSR
013400170918     C*------------------------------------------------------------------------*
