000100160204     d pIn_Opz         s              1a
000200160204       // tipo lancio: 'C'=LR   *blank=RT
000300160204     d pIn_TLa         s              1a
000400160204     d pIn_COD         s              3a
000500160204     d pIn_KEY         s             15a
000600160204     d pIn_Data        s              8s 0
000700160204       // esito chiamata: 0=OK   1=ERR INPUT   2=KO
000800160204     d pOut_ERR        s              1s 0
000900160204     d pOut_DS         s             10a
001000160204     d pOut_Len        s              3s 0
001100160204     d pOut_Dati       s            256a
001200160204     C                   EVAL      pIn_Opz = *blank
001300160204     C                   EVAL      pIn_TLa = *blank
001400160204     C                   EVAL      pIn_COD = 'AOP'
001500160204     C                   EVAL      pIn_KEY = 'LOG-GG'
001600160204     C                   EVAL      pIn_Data=0
001700160204     C                   CALL      'UBTBE01R'
001800160204     C                   PARM                    pIn_Opz
001900160204     C                   PARM                    pIn_TLa
002000160204     C                   PARM                    pIn_COD
002100160204     c                   PARM                    pIn_KEY
002200160204     c                   PARM                    pIn_Data
002300160204     c                   parm                    pOut_ERR
002400160204     c                   parm                    pOut_DS
002500160204     c                   parm                    pOut_Len
002600160204     c                   parm                    pOut_Dati
002700121024
002800160204      // chiudo
002900160204     C                   EVAL      pIn_Opz = *blank
003000160204     C                   EVAL      pIn_TLa = 'C'
003100160204     C                   EVAL      pIn_COD = *blank
003200160204     C                   EVAL      pIn_KEY = *blank
003300160204     C                   EVAL      pIn_Data=0
003400160204     C                   CALL      'UBTBE01R'
003500160204     C                   PARM                    pIn_Opz
003600160204     C                   PARM                    pIn_TLa
003700160204     C                   PARM                    pIn_COD
003800160204     c                   PARM                    pIn_KEY
003900160204     c                   PARM                    pIn_Data
004000160204     c                   parm                    pOut_ERR
004100160204     c                   parm                    pOut_DS
004200160204     c                   parm                    pOut_Len
004300160204     c                   parm                    pOut_Dati
004400160204
004500111214     c                   seton                                        lr
