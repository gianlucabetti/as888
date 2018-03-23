000100060421     H DECEDIT('0,') DATEDIT(*YMD.)
000200050930     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300050930
000400090326     Ftrmz78d   CF   E             WORKSTN
000500090326     Faitrs02l  IF   E           K DISK
000600090403     Faitra01l  IF   E           K DISK
000700050930
000800060802     D kpjba         E DS
000900090415     D trmz70s1ds    E DS                  prefix(S1_)
001000090326     D anrcods       E DS                  extname(anrco98j)
001001120103     D dtrsflr       E DS
001100090326     d dataiso         s               d   datfmt(*iso)
001200090326     d dataeur         s               d   datfmt(*eur)
001300090415      *
001400090403     d  fil            s              3  0 dim(10)
001500060802      *
001600060302      *--------------------------------------------------------------------
001700060503     c                   do        *hival
001800060421     c                   exfmt     f01
001900060503     c   kc              leave
002000090326     c                   setoff                                       333435
002100090326     c                   if        vtrssocg = *blank
002200061106     c                   seton                                        33
002300061106     c                   iter
002400061106     c                   endif
002500090326     c                   if        vpiva    = *blank
002600090326     c                   seton                                        34
002700090326     c                   iter
002800090326     c                   endif
002900090326      *
003000090326     c     ktrs          chain     aitrs02l
003100090326     c                   if        %found(aitrs02l)
003101120104     c                   setoff                                       19
003102120104     c                   clear                   vtrsunodc
003103120104     c                   eval      dtrsflr = trsflr
003104120104     c                   if         §trsris = 'R'
003105120104     c                   setoN                                        19
003106120104     c                   end
003200090326     c                   z-add     trsnrc        vtrsnrc
003300090326     c                   z-add     trsfil        vtrsfil
003400090326     c                   if        trsdec > 0
003500090326     c                   move      trsdec        dataiso
003600090326     c                   move      dataiso       dataeur
003700090326     c                   move      dataeur       vtrsdec
003800090326     c                   endif
003900090326      *
004000090326     c                   if        trsdsc > 0
004100090326     c                   move      trsdsc        dataiso
004200090326     c                   move      dataiso       dataeur
004300090326     c                   move      dataeur       vtrsdsc
004301120104     c  n19              if        §trsunodc = *zeros or
004302120104     c                             §trsunodc =  *blank
004303120104     c                   move      dataeur       vtrsunodc
004304120104     c                   endif
004400090326     c                   endif
004401120103     c                   if        §trsunodc <> *zeros and
004402120103     c                             §trsunodc <> *blank
004403120103     c                   move      §trsunodc     §trsunodc0        8 0
004404120103     c                   move      §trsunodc0    dataiso
004405120103     c                   move      dataiso       dataeur
004406120103     c                   move      dataeur       vtrsunodc
004407120103     c                   endif
004408120103     c                   if        vtrsunodc = vtrsdsc
004409120103     c                   clear                   vtrsdsc
004410120103     c                   endif
004500090326      *
004600090326     c                   if        trsdrc > 0
004700090326     c                   move      trsdrc        dataiso
004800090326     c                   move      dataiso       dataeur
004900090326     c                   move      dataeur       vtrsdrc
005000090326     c                   endif
005100090326      **     x F=Fornitore/C=Cliente
005200090415      **  Routine x Reperire dati Fornitore:
005300090415      **    La routine in base alla sottonatura può funzionare
005400090415      **     x F=Fornitore/C=Cliente
005500090415     C                   clear                   trmz70s1ds                     Input
005600090415     C                   movel(p)  Vpiva         s1_PartitaIVA                  Input
005700090415     C                   movel(p)  'F'           s1_SottoNatur                  Input "F/C"
005800090415     C                   movel(p)  Vtrssocg      s1_Societa                     Input/Output
005900090415     C                   movel(p)  Vtrsfil       s1_Unita                       Input/Output
006000090415     c                   call      'TRMZ70SR1'
006100090415     C                   PARM                    trmz70s1ds                     Input
006200090415     c                   eval      VRAGsoc = s1_RagSocKSC
006300090415     c*
006400090403     c                   exsr      elencofil
006500090326     c*
006600090326     c                   else
006700090327     c                   clear                   vragsoc
006800090326     c                   clear                   vtrsdec
006900090326     c                   clear                   vtrsdsc
007000090326     c                   clear                   vtrsdrc
007100090326     c                   clear                   vtrsnrc
007200090326     c                   clear                   vtrsfil
007300090327     c                   write     f01
007400090326     c                   seton                                        35
007500090326     c                   endif
007600090326      *
007700090326     c                   enddo
007800060302     c                   seton                                        lr
007900050930
008000090403      *___________________________________________________                  ****
008100090403     C     elencofil     begsr
008200090403      *___________________________________________________                  ****
008300090403     c                   clear                   x                 2 0
008400090403     c                   clear                   fil
008500090403     C     trsnrc        setll     aitra01l
008600090403     C                   do        *hival
008700090403     C     trsnrc        reade     aitra01l
008800090403     C                   if        %eof(aitra01l)
008900090403     C                   leave
009000090403     C                   endif
009100090403     c                   if        tradfi > 0 or x = 10
009200090403     c                   iter
009300090403     c                   endif
009400090403     c     trafil        lookup    fil                                    66
009500090403     c                   if        not *in66
009600090403     c                   eval      x = x + 1
009700090403     c                   z-add     trafil        fil(x)
009800090403     c                   endif
009900090403     C                   enddo
010000090403     c                   eval      filv1 = fil(1)
010100090403     c                   eval      filv2 = fil(2)
010200090403     c                   eval      filv3 = fil(3)
010300090403     c                   eval      filv4 = fil(4)
010400090403     c                   eval      filv5 = fil(5)
010500090403     c                   eval      filv6 = fil(6)
010600090403     c                   eval      filv7 = fil(7)
010700090403     c                   eval      filv8 = fil(8)
010800090403     c                   eval      filv9 = fil(9)
010900090403     c                   eval      filv10 = fil(10)
011000090403     C                   endsr
011100060302      *___________________________________________________                  ****
011200060421     C     *inzsr        begsr
011300060302      *___________________________________________________                  ****
011400060421     C     *entry        plist
011500060906     C                   parm                    kpjba
011600090326     C     ktrs          klist
011700090326     C                   kfld                    vtrssocg
011800090326     C                   kfld                    vpiva
011900090326     C                   kfld                    data0             8 0
012000060421     C                   endsr
