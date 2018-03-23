000100040225     h Decedit('0,') Datedit(*YMD.) Option(*nodebugio)
000200001006
000300040225      *------------------------------------------------------------------------*
000400171227     ftitas31c  uf   e           k Disk
000500171227     ftitas30c  uf   e           k Disk    rename(titas000:titas030)
000600171227     f                                     rename(titas010:titas130)
000700171227     f                                     rename(titasp00:titasp30)
000800171227     ffnlbl02l  uf   e           k Disk
000900171227     ffnblp01l  uf   e           k Disk    extfile(wfnblp) usropn
001000171227     ffnarb01l  uf   e           k Disk    extfile(wfnarb) usropn
001100150305     ftita430c  if a e           k Disk
001200171227     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001300040302
001400171227     d wfnblp          s             21    inz('FILTRA201 /FNBLP01L')
001500171227     d wfnARB          s             21    inz('FILTRA201 /FNARB01L')
001600001006      *------------------------------------------------------------------------*
001700150305     C     Kta4          KLIST
001800040423     C                   KFLD                    tasaas
001900040423     C                   KFLD                    taslnp
002000040423     C                   KFLD                    tasnrs
002100040423     C                   KFLD                    tasnsp
002200150305     C                   KFLD                    wtrd              1
002300171227     C     Ktas          KLIST
002400171227     C                   KFLD                    ksc
002500171227     C                   KFLD                    dft
002600171227     C     Kbolla        KLIST
002700171227     C                   KFLD                    tasaas
002800171227     C                   KFLD                    taslnp
002900171227     C                   KFLD                    tasnrs
003000171227     C                   KFLD                    tasnsp
003100171227     C     Klbln         KLIST
003200171227     C                   KFLD                    lblaan
003300171227     C                   KFLD                    lbllpn
003400171227     C                   KFLD                    lblnrn
003500171227     C                   KFLD                    lblnsn
003600171227
003700171227     c                   clear                   ksc               7 0
003800171227     c                   clear                   dft               8 0
003900171227
004000171227         open(e) fnblp01l ;
004100171227         if  not %open(fnblp01l)         ;
004200171227           %subst( wfnblp : 7 : 4 ) = 'PRD ';
004300171227           %subst( wfnarb : 7 : 4 ) = 'PRD ';
004400171227         open fnblp01l ;
004500171227         endif;
004600171227
004700171227         open fnarb01l ;
004800171227
004900171227     c                   eval      wtrd='c'
005000171227     c                   eval      ksc=0936429
005100040225
005200171227     c     ktas          setll     titas31c
005300171227     c     ktas          Reade     titas31c
005400040226
005500171227    1c                   dow       not %eof(titas31c)
005600171227     c
005700171227    2c                   if        tasksc=0936429 and tasias=0
005800171227     c                             and taslnp=093 and tasmgs>=1201
005900171227     c                             and tasmgs<=1221
006000171227
006100171227    3c                   if        tastbl <>'F3' and tastbl<>'A3'
006200171227     c                   exsr      Aggior
006300171227     c                   update    titas000
006400171227     c
006500171227     c
006600171227     c* se la bolla è dirottata, devo mettere ias anche sulla figlia e creare
006700171227     c* record "c" tita4
006800171227    4c                   if        tascca='1'
006900171227     c     kbolla        setll     fnlbl02l
007000171227     c     kbolla        reade     fnlbl02l
007100171227     c                   dow       not %eof(fnlbl02l)
007200171227     c     klbln         chain     titas30c
007300171227     c
007400171227     c                   exsr      Aggior
007500171227     c                   update    titas030
007600171227     c
007700171227     c     kbolla        reade     fnlbl02l
007800171227     c                   enddo
007900171227     c
008000171227    4c                   endif
008100171227     c
008200150305    3c                   endif
008300150305    2c                   endif
008400040415     c
008500040225
008600171227     c     ktas          Reade     titas31c
008700040415    1c                   EndDo
008800010202
008900171227     c                   except    fine
009000040225     c                   Eval      *Inlr = *On
009100171227     c*******************************************
009200171227     c     Aggior        BEGSR
009300171227     c                   eval      tasias=500
009400171227     c                   eval      tasvas='EUR'
009500171227     c
009600171227     c                   clear                   tita4000
009700171227     c                   eval      ta4aas=tasaas
009800171227     c                   eval      ta4lnp=taslnp
009900171227     c                   eval      ta4nrs=tasnrs
010000171227     c                   eval      ta4nsp=tasnsp
010100171227     c                   eval      ta4trc=wtrd
010200171227     c                   eval      ta4not='SN99999999'
010300171227     c                   write     tita4000
010400171227
010500171227
010600171227     c     kbolla        chain     fnblp01l
010700171227     c                   if        %found(fnblp01l)
010800171227     c                   eval      blpias=500
010900171227     c                   eval      blpvas='EUR'
011000171227     c                   update    fnblp000
011100171227     c                   endif
011200171227
011300171227     c     kbolla        chain     fnarb01l
011400171227     c                   if        %found(fnarb01l)
011500171227     c                   eval      arbias=500
011600171227     c                   eval      arbvas='EUR'
011700171227     c                   update    fnarb000
011800171227     c                   endif
011900171227     c
012000171227     c                   add       1             conta             7 0
012100171227     c                   ENDSR
012200171227     OQSYSPRT   E            FINE        1
012300171227     O                                         +  1 'TOT aggiornate'
012400171227     o                       conta         2   +  1
