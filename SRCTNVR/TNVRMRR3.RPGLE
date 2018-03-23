000100040205     h Decedit('0,') Datedit(*dmy.) Option(*nodebugio)
000200040205
000300040205     fWFMRL00F  uP   E             DISK
000400040211     fTABEL00F  IF   E           K DISK
000500040211
000600040211     D Kcod            S                   LIKE(TBLcod)
000700040211     d wNtara          s                   Like(TasPkf)
000800040211     d wTara           s                   Like(§QtTpc)
000900040211
001000040211     D CODUT           S              1  0 INZ(1)
001100040205
001200040206     D DTAS          E DS
001300040206     D  dstasflo     E                     extfld(tasflo)
001400040209
001500040206     D DTASV         E DS
001600040206     D  SV                     1     20    DIM(20)                              SIGLE VARIE
001700040206     D  VA                    21    140P 3 DIM(20)                              IMPORTI VARIE
001800040209
001900040211     d dsqt1         e ds
002000040211
002100040209     D KPJBA         E DS
002200030205     C*---------------------------------------------------------------*
002300040209
002400040209     c                   Clear                   tasdsp
002500040209     c                   Clear                   tasban
002600040209     c                   Clear                   tascmb
002700040209     c                   Clear                   tascas
002800040209     c                   Clear                   tasdct
002900040209     c                   Clear                   tasnclb
003000040209     c                   Clear                   taspor
003100040209     c                   Clear                   taspkcn
003200040209     c                   Clear                   tasial
003300040207
003400040211     C                   IF        TASNSP <> 0 and Tastbl <> 'F7'
003500040207
003600040209      * Codice tariffa 010 fino al 30/04/03 compreso, poi 000
003700040207     C                   MOVEL     TASAAS        TASDSP
003800040207     C                   MOVE      TASMGS        TASDSP
003900040207     C                   If        TASDSP > 20030430
004000040207     C                   z-add     000           TASCTR
004100040207     C                   else
004200040207     C                   z-add     010           TASCTR
004300040207     C                   Endif
004400040207     C                   MOVEL     TASCTR        MRLCTR
004500040207
004600040207      * Peso
004700040211     c                   Z-add     EXMPTO        w0071             7 1
004800040211     c                   Z-add     EXMPTO        w0072             7 2
004900040211     c                   move      w0072         w0010             1 0
005000040211     c                   if        w0010 > 0
005100040211     c                   add       0,1           w0071
005200040211     c                   EndIF
005300040207     C                   SELECT
005400040211     C                   WHEN      TASPKc  > W0071
005500040211     c     §QtTpc        Mult      TasNcp        wTara
005600040211     c     TasPkc        Sub(h)    wTara         wNtara
005700040211     c                   Z-Add     wNtara        taspkc
005800040211     c                   z-add     TASPKc        TASPKF
005900040211     c                   z-add     TASPKc        MRLPKF
006000040207     c                   movel     'B'           MRLFPF
006100040211     C                   WHEN      TASPKc  < W0071
006200040207     c                   z-add     W0071         TASPKF
006300040207     c                   z-add     TASPKF        MRLPKF
006400040207     c                   movel     'M'           MRLFPF
006500040211     C                   WHEN      TASPKc  = W0071
006600040211     c                   z-add     w0071         TASPKF
006700040207     c                   z-add     TASPKF        MRLPKF
006800040207     c                   movel     ' '           MRLFPF
006900040207     c                   ENDSL
007000040211     c                   Clear                   Taspkc
007100040207
007200040207      * Volume
007300040206     c                   Z-add(H)  EXMVTO        w0053             5 3
007400040207     C                   SELECT
007500040211     C                   WHEN      TASVLc > W0053
007600040211     c                   z-add     Tasvlc        TASVLF
007700040211     c                   z-add     TASVLc        MRLVLF
007800040207     c                   movel     'B'           MRLFVF
007900040211     C                   WHEN      TASVLc < W0053
008000040210     c                   z-add     W0053         TASVLF
008100040207     c                   z-add     TASVLF        MRLVLF
008200040207     c                   movel     'M'           MRLFVF
008300040211     C                   WHEN      TASVLc = W0053
008400040211     c                   z-add     W0053         TASVLF
008500040211     c                   z-add     tasvlf        MRLVLF
008600040207     c                   movel     ' '           MRLFVF
008700040207     c                   ENDSL
008800030926      *
008900040207     C                   clear                   DTASV
009000040209     c                   Eval      Tasdct = tasdsp
009100040209     c                   Clear                   Tasimv
009200040210     c                   Eval      Tastc1 = tasftc
009300040207      *
009400040207     C                   CALL      'TNSF20R'
009500040207     C                   PARM                    KPJBA
009600040207     C                   PARM                    DTAS
009700040207     C                   PARM                    DTASV
009800040207      *
009900040209     C                   If        TasErr = *Blanks
010000040207     c                   z-add     TASIMV        MRLIMV
010100040207     c                   z-add     TASPVL        MRLPVL
010200040207     C                   except    wfmrl
010300040209     c                   EndIf
010400040207      *
010500040207     C                   ENDIF
010600040205      **-------------------------------------------------------------**
010700040204     C     *INZSR        BEGSR
010800040205
010900040207     C     *ENTRY        PLIST
011000040207     C                   PARM                    KPJBA
011100040211
011200040211     C     KTAB          KLIST
011300040211     C                   KFLD                    CODUT
011400040211     C                   KFLD                    KCOD
011500040211
011600040211     c                   Movel     'QT'          Kcod
011700040211     c     Ktab          Chain     Tabel00f
011800040211     c                   If        %Found(Tabel00f) and TblFlg = *Blanks
011900040211     c                   Movel     TblUni        dsQt1
012000040211     c                   EndIf
012100040211
012200040209     C                   ENDSR
012300040207      **-------------------------------------------------------------**
012400040209     OWFMRL000  E            WFMRL
012500040209     O                       MRLPKF
012600040209     O                       MRLFPF
012700040209     O                       MRLVLF
012800040209     O                       MRLFVF
012900040209     O                       MRLPVL
013000040209     O                       MRLIMV
013100040207     O                       MRLCTR
