000100990914     H DECEDIT('0,') DATEDIT(*DMY.)
000200170403     Ffnevb05l  IF   E           K DISK
000300061011     Ffnarb01l  IF   E           K DISK
000400170403     Ftigcp01l  IF   E           K DISK
000500170403     D
000600170403     D Seve            S              3    DIM(100)
000700170403     D CONT            S              5  0 DIM(100)
000800170403     D Smeve           S              3    DIM(100)
000900170403     D mCONT           S              5  0 DIM(100)
001000170403     D snoeve          S              3    DIM(100)
001100170403     D ncont           S              5  0 DIM(100)
001200170403     D XX              S              3  0
001300170403     D yy              S              3  0
001400170403     D zz              S              3  0
001500170403     D INDX            S              3  0
001600990914     C*--------------------------------------------------------------------*
001700170403     c     *entry        plist
001800170403     c                   parm                    flg               1
001900061011     C     Karb          KLIST
002000170403     C                   Kfld                    arbaas
002100170403     C                   Kfld                    arblnp
002200170403     C                   Kfld                    arbnrs
002300170403     C                   Kfld                    arbnsp
002400170403     c
002500170403     c                   z-add     2017          kaas              4 0
002600170403     c                   eval      xx=1
002700170403     c                   eval      yy=1
002800170403     c                   eval      zz=1
002900170403     c
003000170403     c     kaas          setll     fnarb01l
003100170403     c     kaas          reade     fnarb01l
003200061011     c
003300170403    1c                   dow       not %eof(fnarb01l)
003400170403    2c                   if        flg=arbffd     or
003500170403     c                             flg=arbtmc or  flg=' '
003600170403     c* solo con giacenza
003700170403     c     karb          chain     tigcp01l
003800170404    3c                   if        %found(tigcp01l) and gcpmgc=0404
003900170403     c                   clear                   wtrov             1
004000170403     c                   clear                   weve              3
004100170403     c* verifico se presente evento
004200170403     c     karb          setll     fnevb05l
004300170403     c     karb          reade     fnevb05l
004400170403    4c                   dow       not %eof(fnevb05l)
004500170403    5c                   if        evbcev<>'MIC' and evbcev<>'NIC' and
004600170403     c                             evbcev<>'PAT'
004700170403
004800170403    6c                   if        evbdtv=(gcpagc*10000)+gcpmgc
004900170403     c                   eval      wtrov='S'
005000170403     c                   eval      weve=evbcev
005100170403     c                   endif
005200170403
005300170403    6c                   if        evbdtv<(gcpagc*10000)+gcpmgc
005400170403     c                   eval      wtrov='Y'
005500170403     c                   eval      weve=evbcev
005600170403     c                   endif
005700170403     c
005800170403    5c                   endif
005900170403     c
006000170403     c     karb          reade     fnevb05l
006100170403    4c                   enddo
006200170403     c*
006300170403    4c                   select
006400170403     c                   when      wtrov=' '
006500170403     c                   EVAL      INDX=%lookup(gcpcmc:snoeve)
006600170403    5C                   IF        INDX>0
006700170403     C                   EVAL      nCONT(INDX)=nCONT(INDX)+1
006800170403     C                   ELSE
006900170403     C                   EVAL      nCONT(yy)=1
007000170403     c                   eval      snoeve(yy)=gcpcmc
007100170403     c                   add       1             yy
007200170403     C                   ENDIF
007300170403    5c
007400170403
007500170403    4c                   when      wtrov='S'
007600170403     c                   EVAL      INDX=%lookup(weve:seve)
007700170403    5C                   IF        INDX>0
007800170403     C                   EVAL      CONT(INDX)=CONT(INDX)+1
007900170403     C                   ELSE
008000170403     C                   EVAL      CONT(XX)=1
008100170403     c                   eval      seve(xx)=weve
008200170403     c                   add       1             xx
008300170403    5C                   ENDIF
008400170403     c
008500170403   x4c                   other
008600170403     c                   EVAL      INDX=%lookup(weve:smeve)
008700170403    5C                   IF        INDX>0
008800170403     C                   EVAL      mCONT(INDX)=mCONT(INDX)+1
008900170403     C                   ELSE
009000170403     C                   EVAL      mCONT(zz)=1
009100170403     c                   eval      smeve(zz)=weve
009200170403     c                   add       1             zz
009300170403    5C                   ENDIF
009400170403
009500170403    4C                   ENDsl
009600170403    3C                   ENDIF
009700170403    2C                   ENDIF
009800170403     c     kaas          reade     fnarb01l
009900170403    1c                   enddo
010000170403     c
010100040324     c                   seton                                        lr
