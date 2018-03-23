000100020712     ***********************************************************************
000200020712     ** Dopo aver creato il programma, creare la User Defined Function
000300020712     ** con la seguente istruzione SQL:
000400020712     **
000500030701     ** CREATE FUNCTION [Libreria]/[Nome *PGM]
000600070126     **  (TIP_ANAG       CHAR(1 )
000700070126     ** , COD_PDR        DECIMAL(7, 0)
000800070126     ** , FLG_PRESTAZ    CHAR(1 )
000900070129     ** , FGS_CGI        DECIMAL(3, 0)
001000070126     ** , COD_CGI        CHAR(10 )
001100070223     ** , DATA_FVV       DECIMAL(8, 0)
001200070223     ** , NUM_FVV        DECIMAL(6, 0)
001300070226     ** , TIP_PIG        DECIMAL(1, 0))
001400070126     ** RETURNS DECIMAL(2, 0)
001500020712     ** RETURNS NULL ON NULL INPUT
001600020712     ** LANGUAGE RPGLE
001700020712     ** NO SQL
001800020712     ** PARAMETER STYLE SQL
001900030702     **
002000030702     ** Per rimuovere un UDF usare la seguente istruzione:
002100030702     ** DROP FUNCTION [Libreria]/[Nome *PGM]
002200020712     ***********************************************************************
002300020712
002400071008     FFIAPD54L  IF   E           K DISK
002500071008     FFIDST52L  IF   E           K DISK
002600030701     D*------------------
002700030701     D* VARIABILI DI WRK
002800030701     D*------------------
002900070126     D InTipAnag       S              1
003000070126     D InCodAutotr     S              7  0
003100070126     D InFlgPrestaz    S              1
003200070129     D InFgsGiro       S              3  0
003300070126     D InCodGiro       S             10
003400070223     D InDataFvv       S              8  0
003500070223     D InNumFvv        S              6  0
003600070226     D InTipPig        S              1  0
003700070126     D OutPriorit      S              2  0
003800070126     D NullIn_TANA     S              5I 0
003900070126     D NullIn_FLGP     S              5I 0
004000070126     D NullIn_CAUT     S              5I 0
004100070129     D NullIn_FGSG     S              5I 0
004200070126     D NullIn_CCGI     S              5I 0
004300070126     D NullIn_DATA     S              5I 0
004400070223     D NullIn_NFVV     S              5I 0
004500070223     D NullIn_TPIG     S              5I 0
004600070126     D NullOut_PRIO    S              5I 0
004700070126     D wSQLState       S              5
004800020705     D FunctionName    S            139
004900020705     D SpecificName    S            128
005000020705     D MsgText         S             70
005100020705     D                                     VARYING
005200020705
005300020705     C     *ENTRY        PLIST
005400020705     ** Parametri di input.
005500070126     C                   PARM                    InTipAnag
005600070126     C                   PARM                    InCodAutotr
005700070126     C                   PARM                    InFlgPrestaz
005800070129     C                   PARM                    InFgsGiro
005900070126     C                   PARM                    InCodGiro
006000070223     C                   PARM                    InDataFvv
006100070223     C                   PARM                    InNumFvv
006200070223     C                   PARM                    InTipPig
006300020705     ** Parametri di output.
006400070126     C                   PARM                    OutPriorit
006500020705     ** Parametri di indicatore nullo.
006600070126     C                   PARM                    NullIn_TANA
006700070126     C                   PARM                    NullIn_FLGP
006800070126     C                   PARM                    NullIn_CAUT
006900070129     C                   PARM                    NullIn_FGSG
007000070129     C                   PARM                    NullIn_CCGI
007100070126     C                   PARM                    NullIn_DATA
007200070223     C                   PARM                    NullIn_NFVV
007300070223     C                   PARM                    NullIn_TPIG
007400070126     C                   PARM                    NullOut_PRIO
007500020705     ** Parametri di funzione SQL.
007600070126     C                   PARM                    wSQLState
007700020705     C                   PARM                    FunctionName
007800020705     C                   PARM                    SpecificName
007900020705     C                   PARM                    MsgText
008000070129
008100070129     C                   CLEAR                   NullOut_PRIO
008200070129     C                   EVAL      wSQLState = *ZERO
008300070129
008400070129     C*
008500070129     C* DEFINIZIONE CHIAVI DI ACCESSO AI FILE
008600070129     C*
008700071008     C* FIAPD54L - Parziale
008800071008     C     KEYapd54_P    KLIST
008900071008     C                   KFLD                    apd5tipa
009000071008     C                   KFLD                    apd5pdr
009100071008     C                   KFLD                    apd5fpp
009200071008     C                   KFLD                    apd5fgsg
009300071008     C                   KFLD                    apd5cgi
009400071008     C                   KFLD                    apd5dde
009500070223     C*
009600071008     C* FIDST52L - Totale
009700071008     C     KEYdst52_C    KLIST
009800071008     C                   KFLD                    dst5fgs
009900071008     C                   KFLD                    dst5dfv
010000071008     C                   KFLD                    dst5fgsg
010100071008     C                   KFLD                    dst5cgi
010200071008     C                   KFLD                    dst5nfv
010300070129
010400070215     C                   if        InCodGiro = *blanks
010500070215     C                   eval      OutPriorit = 98
010600070215     C                   else
010700070223     C*
010800070226     C                   if        InTipPig = 1
010900071008     C                   eval      apd5tipa = InTipAnag
011000071008     C                   eval      apd5pdr  = InCodAutotr
011100071008     C                   eval      apd5fpp  = InFlgPrestaz
011200071008     C                   eval      apd5fgsg = InFgsGiro
011300071008     C                   eval      apd5cgi  = InCodGiro
011400071008     C                   eval      apd5dde  = InDataFvv
011500070129
011600070129     C                   eval      OutPriorit = 99
011700070129
011800071008     C     KEYapd54_P    setgt     fiapd54l
011900071008     C                   if        %found(fiapd54l)
012000071008     C                   readp     fiapd54l
012100071008     C                   if        not %eof(fiapd54l)
012200071008     C                   if        apd5tipa   = InTipAnag    AND
012300071008     C                             apd5pdr    = InCodAutotr  AND
012400071008     C                             apd5fpp    = InFlgPrestaz AND
012500071008     C                             apd5fgsg   = InFgsGiro    AND
012600071008     C                             apd5cgi    = InCodGiro    AND
012700071008     C                             apd5dde   <= InDataFvv    AND
012800071008     C                             apd5dscad >= InDataFvv
012900071008     C                   eval      OutPriorit = apd5prgg
013000070129     C                   endif
013100070129     C                   endif
013200070129     C                   endif
013300070223     C*
013400070223     C                   else
013500071008     C                   eval      dst5fgs  = InFgsGiro
013600071008     C                   eval      dst5dfv  = InDataFvv
013700071008     C                   eval      dst5fgsg = InFgsGiro
013800071008     C                   eval      dst5cgi  = InCodGiro
013900071008     C                   eval      dst5nfv  = InNumFvv
014000070223
014100070223     C                   eval      OutPriorit = 99
014200070223
014300071008     C     KEYdst52_C    chain     fidst52l
014400071008     C                   if        %found(fidst52l)
014500071008     C                   eval      OutPriorit = dst5prgg
014600070223     C                   endif
014700070223     C                   endif
014800070215     C                   endif
014900061221     C*
015000020705     C                   RETURN
015100070129
015200070129
015300070129     C     *pssr         BEGSR
015400070129     C*
015500070129     C                   eval      OutPriorit = *zeros
015600070129     C***                EVAL      NullOut_PRIO = -1
015700070129     C                   EVAL      wSQLState = '38I01'
015800070129     C                   EVAL      MsgText = 'Errore funzione GETCGIPTY'
015900070129     C*
016000070129     C                   ENDSR     '*CANCL'
016100070129     C***
