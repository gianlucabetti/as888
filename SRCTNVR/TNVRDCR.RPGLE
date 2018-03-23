000100090319     FFNDCr01L  UF   E           K DISK
000200090319     FFNDCt01L  iF   E           K DISK
000300090319     Ftitas30c  iF   E           K DISK
000400090323     FFNDCK01L  IF   E           K DISK
000500090325     Fazorg01l  IF   E           K DISK
000600090319     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000700010620
000800090325     d og143         e ds
000900090325     d fnlv55ds      e ds
001000090319     d contanodct      s              9  0 inz
001100090319     d contanotas      s              9  0 inz
001200090325     D                 DS
001300090325     D  tasaas                 1      4  0
001400090325     D  tasmgs                 5      8  0
001500090325     D  tasdsp                 1      8  0
001600090319     d
001700090319     c     kdct          klist
001800090319     c                   kfld                    dcraac
001900090319     c                   kfld                    dcrfil
002000090319     c                   kfld                    dcrnca
002100090319     c     ktas          klist
002200090319     c                   kfld                    dctaas
002300090319     c                   kfld                    dctlnp
002400090319     c                   kfld                    dctnrs
002500090319     c                   kfld                    dctnsp
002600090323     C     kbolladck     KLIST                                                  *
002700090323     C                   KFLD                    dckaas                         *
002800090323     C                   KFLD                    dcklnp                         *
002900090323     C                   KFLD                    dcknrs                         *
003000090323     C                   KFLD                    dcknsp                         *
003100090319     c
003200090319     C     *LOVAL        SETLL     FNDCr01L
003300090319     C                   READ      FNDCr000
003400981125
003500090319    1C                   DOW       NOT %EOF(FNDCr01L)
003600090323     c                   eval      %subst(dcrflo:13:3)='000'
003700090319     c* chaino fndct00f
003800090325     c                   clear                   dctlnp
003900090319     c     kdct          chain     fndct01l
004000090319    2c                   if        %found(fndct01l)
004100090325     c
004200090325     c* se p.o. estero, terminal = se stesso
004300090325     c     dcrpor        chain     azorg01l
004400090325    3c                   if        %found(azorg01l)
004500090325     c                   movel     orgde3        og143
004600090325     c                   else
004700090325     c                   clear                   og143
004800090325    3c                   endif
004900090325   2ac                   if        §ogntw='EEX' or §ogntw='DPD' or
005000090325     c                             §ogntw='FED'
005100090325     c                   eval      %subst(dcrflo:13:3)=%editc(dcrpor:'X')
005200090325     c                   update    fndcr000
005300090325     c
005400090325  x2ac                   else
005500090323     c* Verifico se bolla legata
005600090323     c     Kdct          chain     fndck01l
005700090323     c
005800090325     c                   clear                   tasdsp
005900090325     c                   clear                   taslnp
006000090323     c*
006100090319     c     ktas          chain     titas30c
006200090319    3c                   if        %found(titas30c)
006300090319     c                   select
006400090319    4c                   when      dcrtcr='P'
006500090323     c                   if        dcrpor=taslnp or
006600090323     c                                                    dcrpor=tastfp
006700090319     c                   eval      %subst(dcrflo:13:3)=%editc(tastfp:'X')
006800090323     c                   endif
006900090319    4c                   when      dcrtcr='A'
007000090323     c                   if        dcrpor=taslna or
007100090323     c                                                    dcrpor=tastfa
007200090319     c                   eval      %subst(dcrflo:13:3)=%editc(tastfa:'X')
007300090323     c                   endif
007400090323     c
007500090325     c                   when      dcrtcr='T' and not %found(fndck01l)
007600090319     c                   exsr      CallLV55
007700090319    4c                   endsl
007800090323     c
007900090323     c* Se non ancora calcolato, cerco bolla legata
008000090323    4c                   if        %subst(dcrflo:13:3)<='000'   and
008100090323     c                             %found(fndck01l)
008200090323     c
008300090323     c     kbolladck     chain     titas30c
008400090323    5c                   if        %found(titas30c)
008500090326    6c                   if        dcrpor=taslnp or
008600090323     c                                                    dcrpor=tastfp
008700090323     c                   eval      %subst(dcrflo:13:3)=%editc(tastfp:'X')
008800090326   x6c                   else
008900090326    7c                   if        dcrpor=taslna or
009000090323     c                                                    dcrpor=tastfa
009100090323     c                   eval      %subst(dcrflo:13:3)=%editc(tastfa:'X')
009200090326    7c                   endif
009300090326    6c                   endif
009400090323    5c                   endif
009500090324    4c                   endif
009600090323     c
009700090323    5c                   if        %subst(dcrflo:13:3)<='000'
009800090323     c                   exsr      CallLV55
009900090323    5c                   endif
010000090319     c
010100090319     c                   update    fndcr000
010200090319     c
010300090319   x3c                   else
010400090326     c                   except    notas
010500090326     c
010600090319     c                   eval      contanotas=contanotas+1
010700090319     c                   exsr      CallLV55
010800090319     c                   update    fndcr000
010900090319    3c                   endif
011000090325   2ac                   endif
011100090319     c
011200090319   x2c                   else
011300090319     c                   eval      contanodct=contanodct+1
011400090319     c                   exsr      CallLV55
011500090319     c                   update    fndcr000
011600090325    2c                   endif
011700010620
011800090319     C                   READ      FNDCr000
011900010706
012000090319    1C                   ENDDO
012100010620
012200090319     c                   except    totali
012300090319     c
012400010801     C                   EVAL      *INLR = *ON
012500090319     c* c*-------------------------------------------
012600090319     c     CallLV55      BEGSR
012700090319     c                   clear                   fnlv55ds
012800090319     c                   if        dcrtcr='P'
012900090319     c                   eval      d55tpt='P'
013000090319     c                   else
013100090319     c                   eval      d55tpt='A'
013200090325     c                   if        taslnp>0
013300090325     c                   movel     taslnp        d55lnp
013400090325     c                   else
013500090325     c                   movel     dctlnp        d55lnp
013600090325     c                   endif
013700090325     c
013800090319     c                   endif
013900090319     c
014000090319     c                   eval      d55lin=dcrpor
014100090325     c                   if        tasdsp>0
014200090325     c                   z-add     tasdsp        d55drf
014300090325     c                   else
014400090319     c                   z-add     dctmgc        d55drf
014500090319     c                   movel     dctaac        d55drf
014600090325     c                   endif
014700090319     c                   call      'FNLV55R'
014800090319     c                   parm                    fnlv55ds
014900090323     c                   if        d55err<>' '
015000090323     c                   movel     20090401      d55drf
015100090323     c                   call      'FNLV55R'
015200090323     c                   parm                    fnlv55ds
015300090323     c                   endif
015400090319     c
015500090319     c                   if        dcrtcr='P'
015600090319     c                   eval      %subst(dcrflo:13:3)=%editc(d55tfp:'X')
015700090319     c                   else
015800090319     c                   eval      %subst(dcrflo:13:3)=%editc(d55tfa:'X')
015900090319     c                   endif
016000090319     c                   ENDSR
016100090319     c
016200090319     Oqsysprt   E            TOTALI      2
016300090319     O                                              'totale DCR senza DCT'
016400090319     o                       contanodct    2  +   2
016500090319     O                                        +   3 'totale DCT senza TAS'
016600090319     o                       contanotas    2  +   2
016700090325     Oqsysprt   E            NOTAS       2
016800090325     o                       dctaac        z  +   2
016900090325     o                       dctfil        2  +   1
017000090325     o                       dctnca        2  +   1
017100090325     o                       dctaas        z  +   2
017200090325     o                       dctlnp        2  +   1
017300090325     o                       dctnrs        2  +   1
017400090325     o                       dctnsp        2  +   1
