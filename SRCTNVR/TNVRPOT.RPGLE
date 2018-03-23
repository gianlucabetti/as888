000100000725     H DECEDIT('0,') DATEDIT(*YMD.) DATFMT(*ISO)
000200000725
000300000725      ******************************************************************
000400000725      *                                                                *
000500001110      *  TNVRPOT - Recupera ultimo stato e se si tratta di una visita  *
000600001110      *            scrive un file di work uguale a TNCPO00F            *
000700000725      *                                                                *
000800000725      ******************************************************************
000900991227
001000001110     FTNCPO00F  IP   E             DISK
001100001110     FTNCPOWWF  o    E             DISK    rename(TNCPO000:TNCPOwww)
001200001110     F                                     prefix(W)
001300000725
001400001110     D trmk25ds      E DS
001500001110     D DS_CPS        E DS                  extname(TNCPS00F)
001600001110     D KPJBA         E DS
001700001110     D DS_CPO        E DS                  extname(TNCPO00F)
001800001110     D DS_CPOW       E DS                  extname(TNCPO00F) prefix(W)
001900000725
002000000725      *----------------------------------------------------------------*
002100001110     C                   IF        CpoAtb = *blanks
002200001110      *
002300001110     C                   clear                   trmk25ds
002400001110     C                   eval      I25Dit = 'BAR'
002500001110     C                   eval      I25Cpo = CpoCpo
002600001110     c                   movel(p)  TRMK25DS      KPJBU
002700001110      *
002800001110     C                   call      'TRMK25R'
002900001110     c                   parm                    KPJBA
003000001110     c                   parm                    DS_CPS
003100001110      *
003200001115     C                   IF        O25Err <> 'S'  and  CpsKsc = *zeros  and
003300001115     c                             (CpsFst = '0 ' or CpsFst = '1 '
003400001115     c                                            or CpsFst = '2 ')
003500001110     c                   movel     DS_CPO        DS_CPOW
003600001110     c                   movel     CpsFst        wCpoFtr
003700001110     c                   clear                   wCpoDtr
003800001110     c                   write     tncpowww
003900001110     c                   ENDIF
004000001110      *
004100001110     c                   ENDIF
