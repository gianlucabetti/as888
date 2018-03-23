000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200070316     d
000300080430     D DSlv13        E DS                  extname(fnlv13ds)
000400080430     d DSsi95        E DS                  extname(tisi95ds)
000500080430     d kpjba         E Ds
000600090615     c     *entry        plist
000700090615     c                   parm                    cap               9
000800090615     C                   parm                    loc              20
000900080430     C*
001000080430     C* CAP OBBLIGATORIO : CONTROLLO
001100080430     C                   CLEAR                   DSSI95
001200080430     C                   CLEAR                   DSLV13
001300080430     C                   MOVEL     '3'           I95TCN
001400090615     C                   MOVEL     cap           I95CAP
001500090615     C                   MOVEL     loc           I95LOC
001600080430     C* ERRORE AFFIDABILITA = 0
001700090615     C                   MOVEL     ' '           I13AF0
001800080430     C                   CALL      'FNLV13R'
001900080430     C                   PARM                    KPJBA
002000080430     C                   PARM                    DSLV13
002100080430     C                   PARM                    DSSI95
002200080430     C*
002300020503     C*
002400020502     C                   SETON                                        LR
002500070316     C*
