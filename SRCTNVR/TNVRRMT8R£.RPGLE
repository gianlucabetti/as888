000100940224     H*--------------------------------------------------------------*
000200060614     Ftilao01l  UF   E           K DISK
000300060614     Ftitas30c  IF   E           K DISK
000400060614     d waas            s                   Like(tasaas)
000500060614     d wlnp            s                   Like(taslnp)
000600060614     d wnrs            s                   Like(tasnrs)
000700060614     d wnsp            s                   Like(tasnsp)
000800060614     d WtasRMN         s             15    inz(*zeros)
000900060614     d WtasDCM         s              8    inz(*zeros)
001000060614     d Wope            s                   like(laoope)
001100900131     C*---------------------------------------------------------------*
001200060614     c     ktas          klist
001300060614     c                   kfld                    waas
001400060614     c                   kfld                    wlnp
001500060614     c                   kfld                    wnrs
001600060614     c                   kfld                    wnsp
001700060512     c
001800060512
001900060614     c     *loval        setll     tilao01l
002000060517    1C                   DO        *hival
002100060614     c                   read      tilao01l
002200060614    2c                   if        %eof(tilao01l)
002300060517     c                   leave
002400060517    2c                   endif
002500060614     c                   if        %subst(laotim:1:8)< '20060609'
002600060614     c                   iter
002700060614     c                   endif
002800060614     c                   z-add     2000          waas
002900060614     c                   move      laoaas        waas
003000060614     c                   move      laolnp        wlnp
003100060614     c                   move      laonrs        wnrs
003200060614     c                   move      laonsp        wnsp
003300060614     c     ktas          chain     titas30c
003400060614     c                   if        %found(titas30c)
003500060614     c                   move      TASrmn        Wtasrmn
003600060614     c                   movel     TASdcm        WtasDCM
003700060614     c                   clear                   wope
003800060614     c                   eval      wope    = %subst(wTASrmn:4:11)
003900060614     c                                     + '_'
004000060614     c                                     + wTASdcm
004100060614     c                   if        wope <> laoope
004200060614     c                   eval      laoope=wope
004300060614     c                   eval      lagela = '00'
004400060614     c                   move      '99'          laotim
004500060614     c                   update    tilao000
004600060614     c                   endif
004700060614     c
004800060614     c                   endif
004900060512      *
005000060517    1C                   ENDDO
005100030205      *
005200000000     C                   SETON                                        LR
