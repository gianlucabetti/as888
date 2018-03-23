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
001100070126     ** , DATA           DECIMAL(8, 0))
001200070126     ** RETURNS DECIMAL(2, 0)
001300020712     ** RETURNS NULL ON NULL INPUT
001400020712     ** LANGUAGE RPGLE
001500020712     ** NO SQL
001600020712     ** PARAMETER STYLE SQL
001700030702     **
001800030702     ** Per rimuovere un UDF usare la seguente istruzione:
001900030702     ** DROP FUNCTION [Libreria]/[Nome *PGM]
002000020712     ***********************************************************************
002100020712
002200070129     FFIAPD34L  IF   E           K DISK
002300030701     D*------------------
002400030701     D* VARIABILI DI WRK
002500030701     D*------------------
002600070126     D InTipAnag       S              1
002700070126     D InCodAutotr     S              7  0
002800070126     D InFlgPrestaz    S              1
002900070129     D InFgsGiro       S              3  0
003000070126     D InCodGiro       S             10
003100070126     D InData          S              8  0
003200070126     D OutPriorit      S              2  0
003300070126     D NullIn_TANA     S              5I 0
003400070126     D NullIn_FLGP     S              5I 0
003500070126     D NullIn_CAUT     S              5I 0
003600070129     D NullIn_FGSG     S              5I 0
003700070126     D NullIn_CCGI     S              5I 0
003800070126     D NullIn_DATA     S              5I 0
003900070126     D NullOut_PRIO    S              5I 0
004000070126     D wSQLState       S              5
004100020705     D FunctionName    S            139
004200020705     D SpecificName    S            128
004300020705     D MsgText         S             70
004400020705     D                                     VARYING
004500020705
004600020705     C     *ENTRY        PLIST
004700020705     ** Parametri di input.
004800070126     C                   PARM                    InTipAnag
004900070126     C                   PARM                    InCodAutotr
005000070126     C                   PARM                    InFlgPrestaz
005100070129     C                   PARM                    InFgsGiro
005200070126     C                   PARM                    InCodGiro
005300070126     C                   PARM                    InData
005400020705     ** Parametri di output.
005500070126     C                   PARM                    OutPriorit
005600020705     ** Parametri di indicatore nullo.
005700070126     C                   PARM                    NullIn_TANA
005800070126     C                   PARM                    NullIn_FLGP
005900070126     C                   PARM                    NullIn_CAUT
006000070129     C                   PARM                    NullIn_FGSG
006100070129     C                   PARM                    NullIn_CCGI
006200070126     C                   PARM                    NullIn_DATA
006300070126     C                   PARM                    NullOut_PRIO
006400020705     ** Parametri di funzione SQL.
006500070126     C                   PARM                    wSQLState
006600020705     C                   PARM                    FunctionName
006700020705     C                   PARM                    SpecificName
006800020705     C                   PARM                    MsgText
006900070129
007000070129     C                   CLEAR                   NullOut_PRIO
007100070129     C                   EVAL      wSQLState = *ZERO
007200070129
007300070129     C*
007400070129     C* DEFINIZIONE CHIAVI DI ACCESSO AI FILE
007500070129     C*
007600070129     C* FIAPD34L - Parziale
007700070129     C     KEYapd34_P    KLIST
007800070129     C                   KFLD                    apd3tipa
007900070129     C                   KFLD                    apd3pdr
008000070129     C                   KFLD                    apd3fpp
008100070129     C                   KFLD                    apd3fgsg
008200070129     C                   KFLD                    apd3cgi
008300070129     C                   KFLD                    apd3dde
008400070129
008500070215     C                   if        InCodGiro = *blanks
008600070215     C                   eval      OutPriorit = 98
008700070215     C                   else
008800070129     C                   eval      apd3tipa = InTipAnag
008900070129     C                   eval      apd3pdr  = InCodAutotr
009000070129     C                   eval      apd3fpp  = InFlgPrestaz
009100070129     C                   eval      apd3fgsg = InFgsGiro
009200070129     C                   eval      apd3cgi  = InCodGiro
009300070129     C                   eval      apd3dde  = InData
009400070129
009500070129     C                   eval      OutPriorit = 99
009600070129
009700070129     C     KEYapd34_P    setgt     fiapd34l
009800070129     C                   if        %found(fiapd34l)
009900070129     C                   readp     fiapd34l
010000070129     C                   if        not %eof(fiapd34l)
010100070206     C                   if        apd3tipa   = InTipAnag    AND
010200070206     C                             apd3pdr    = InCodAutotr  AND
010300070206     C                             apd3fpp    = InFlgPrestaz AND
010400070206     C                             apd3fgsg   = InFgsGiro    AND
010500070206     C                             apd3cgi    = InCodGiro    AND
010600070206     C                             apd3dde   <= InData       AND
010700070206     C                             apd3dscad >= InData
010800070129     C                   eval      OutPriorit = apd3prgg
010900070129     C                   endif
011000070129     C                   endif
011100070129     C                   endif
011200070215     C                   endif
011300061221     C*
011400020705     C                   RETURN
011500070129
011600070129
011700070129     C     *pssr         BEGSR
011800070129     C*
011900070129     C                   eval      OutPriorit = *zeros
012000070129     C***                EVAL      NullOut_PRIO = -1
012100070129     C                   EVAL      wSQLState = '38I01'
012200070129     C                   EVAL      MsgText = 'Errore funzione GETCGIPTY'
012300070129     C*
012400070129     C                   ENDSR     '*CANCL'
012500070129     C***
