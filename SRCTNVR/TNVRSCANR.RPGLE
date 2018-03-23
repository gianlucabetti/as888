000100000103     H DECEDIT('0,') DATEDIT(*DMY/)
000200000103      *                                                                     *****
000300160114     FFNBLP31L  IF   E           K DISK
000400160114     FFNEVB05L  uF   E           K DISK
000500000103      *-------------------
000600160114     D FIEU12DS      E DS
000700160114     D kpjba         E DS
000800000103      *----------------------------------------------------*
000900160114     c     *entry        plist
001000160114     c                   parm                    kpjba
001100000103
001200160114     c     kblp          klist
001300160114     c                   kfld                    klnp              3 0
001400160114     c                   kfld                    kaas              4 0
001500160114     c                   kfld                    kmgs              4 0
001600160114     c     kEVB          klist
001700160114     c                   kfld                    BLPAAS
001800160114     c                   kfld                    BLPLNP
001900160114     c                   kfld                    BLPNRS
002000160114     c                   kfld                    BLPNSP
002100160114
002200160114     c                   z-add     190           klnp
002300160114     c                   z-add     2015          kaas
002400160118     c                   z-add     0304          kmgs
002500160114     c     kblp          setll     fnblp31l
002600160114     c     kblp          reade     fnblp31l
002700160114     c                   dow       not %eof(fnblp31l)
002800160114     C* CHAIN
002900160114     c                   clear                   fiEU12ds
003000160114     c                   eval      i12aas=blpaas
003100160114     c                   eval      i12lnp=blplnp
003200160114     c                   eval      i12nrs=blpnrs
003300160114     c                   eval      i12nsp=blpnsp
003400160114     c                   eval      i12LNA=BLPLNA
003500160114     c                   eval      i12PARCEL=%subst(%editc(BLPRMN:'X'):2:14)
003600160114     C                   CALL      'FIEU12R'
003700160114     C                   PARM                    KPJBA
003800160114     C                   PARM                    FIEU12DS
003900160114     C
004000160114     C* SFLAGGO GLI EVENTI
004100160114     C     KEVB          SETLL     FNEVB05L
004200160114     C     KEVB          READE     FNEVB05L
004300160114     C                   DOW       NOT %EOF
004400160114     C                   CLEAR                   EVBDTC
004500160114     C                   UPDATE    FNEVB000
004600160114
004700160114     C     KEVB          READE     FNEVB05L
004800160114     C                   ENDDO
004900160114
005000160114     c     kblp          reade     fnblp31l
005100160114     C                   ENDDO
005200160114     C
005300160114     C                   SETON                                        LR
