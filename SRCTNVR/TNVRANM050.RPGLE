000100050118     H DECEDIT('0,') DATEDIT(*YMD.)
000200021214      *
000300160725     FFNART27L  iF   E           K DISK
000400160725     FFNARb01L  iF   E           K DISK
000500160725     FFnanm06L  iF   E           K DISK
000600160725     FFnanm02L  iF   E           K DISK    rename(fnanm000:fnanm002)
000700021214     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000800160725     D tfptfa          S              6  0 DIM(999)
000900160725     D num             S              6  0 DIM(999)
001000160725     D tfp             S              3  0 DIM(999)
001100160725     D nump            S              6  0 DIM(999)
001200021214      *--------------------------------------------------
001300050323     c*
001400160725     c                   z-add     50            kcaa              3 0
001500160725     C     kcaa          setll     fnanm06l
001600160725     C     kcaa          reade     fnanm06l
001700160725    1c                   DOW       not %EOF(FNanm06l)
001800160725    2c                   if        anmdch>=20160701
001900160725     c                   add       1             tot               7 0
002000160725
002100160725     c     kanm02        setll     fnanm02l
002200160725    3c                   if        %equal(fnanm02l)
002300160725     c                   add       1             mancabo           7 0
002400160725   x3c                   else
002500160725     c
002600160725     c     kanm06        chain     fnart27l
002700160725     c* non trovata --> verifico se c'è il manca record e conteggio
002800160725    4c                   if        not %found(fnart27l)
002900160725     c                   add       1             notrov            7 0
003000160725   x4c                   else
003100160725     c     kart          chain     fnarb01l
003200160725    5c                   if        not %found(fnarb01l)
003300160725     c                   add       1             notrov            7 0
003400160725   x5c                   else
003500160725    6c                   if        arbtfp=arbtfa
003600160725     c                   add       1             locali            7 0
003700160725   x6c                   else
003800160725     c                   z-add     arbtfa        w0060             6 0
003900160725     c                   movel     arbtfp        w0060
004000160725     c                   clear                   indx              7 0
004100160725     c                   eval      indx=%lookup(w0060:tfptfa)
004200160725    7c                   if        indx>0
004300160725     c                   add       1             num(indx)
004400160725   x7c                   else
004500160725     c                   eval      indx=%lookup(000000:tfptfa)
004600160725     c                   eval      tfptfa(indx)=w0060
004700160725     c                   eval      num(indx)=1
004800160725    7c                   endif
004900160725
005000160725     c                   movel     arbtfp        w0030             3 0
005100160725     c                   clear                   indx              7 0
005200160725     c                   eval      indx=%lookup(w0030:tfp)
005300160725    7c                   if        indx>0
005400160725     c                   add       1             nump(indx)
005500160725   x7c                   else
005600160725     c                   eval      indx=%lookup(000:tfp)
005700160725     c                   eval      tfp(indx)=w0030
005800160725     c                   eval      nump(indx)=1
005900160725    7c                   endif
006000160725    6c                   endif
006100160725    5c                   endif
006200160725    4c                   endif
006300160725
006400160725    3c                   endif
006500160725    2c                   endif
006600160725     C     kcaa          reade     fnanm06l
006700160725     c                   enddo
006800160725     c
006900160725     c                   eval      indx=1
007000160725     c                   dow       tfp(indx)>0
007100160725     c                   if        nump(indx)>500
007200160725     c                   except    terp
007300160725     c                   endif
007400160725     c                   add       1             indx
007500160725     c                   enddo
007600160725     c
007700160725     c                   eval      indx=1
007800160725     c                   dow       tfptfa(indx)>0
007900160725     c                   if        num(indx)>500
008000160725     c                   except    terpa
008100160725     c                   endif
008200160725     c                   add       1             indx
008300160725     c                   enddo
008400160725     c                   except    fin
008500021214     C                   eval       *inlr = *on
008600021214      *---------------------------------------------------------
008700021214     c     *inzsr        BEGSR
008800021214      *
008900160725     C     Kanm06        KLIST
009000160725     C                   KFLD                    anmfls
009100160725     C                   KFLD                    anmlna
009200160725     C                   KFLD                    anmnrs
009300160725     C                   KFLD                    anmscn
009400160725     C     Kanm02        KLIST
009500160725     C                   KFLD                    anmfls
009600160725     C                   KFLD                    anmlna
009700160725     C                   KFLD                    anmnrs
009800160725     C                   KFLD                    anmscn
009900160725     C                   KFLD                    kcaa10            3 0
010000160725     c                   movel     010           kcaa10
010100160725     C     Kart          KLIST
010200160725     C                   KFLD                    artaas
010300160725     C                   KFLD                    artlnp
010400160725     C                   KFLD                    artnrs
010500160725     C                   KFLD                    artnsp
010600021214      *
010700021216     C*
010800021214     C                   ENDSR
010900021214      *---------------------------------------------------------
011000160725     OQSYSPRT   E            fin         1
011100160725     O                       *DATE            +   1 '  /  /    '
011200160725     O                                        +   2 'TOTALE Anom. 50'
011300160725     o                       tot           2  +   1
011400160725     O                                        +   2 'di cui manca bo'
011500160725     o                       mancabo       2  +   1
011600160725     O                                        +   2 'di cui non trovata bo'
011700160725     o                       notrov        2  +   1
011800160725     O                                        +   2 'di cui locali'
011900160725     o                       locali        2  +   1
012000160725     OQSYSPRT   E            terp        1
012100160725     O                                        +   2 'term. par numerosi'
012200160725     o                       tfp(indx)        +   1
012300160725     o                       nump(indx)    2  +   1
012400160725     OQSYSPRT   E            terpa       1
012500160725     O                                        +   2 'term. par numerosi'
012600160725     o                       tfptfa(indx)     +   1 '   /   '
012700160725     o                       num(indx)     2  +   1
