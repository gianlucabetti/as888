000100060321     FTNTLN00f  Up   E             DISK
000200060321     FTNTLN01l  iF   E           K DISK    rename(tntln000:tntln01) prefix(L_)
000300060321     FTNTLt01l  iF   E           K DISK
000400060321     fqsysprt   o    f  132        printer
000500060321     d TIBS34DS      e ds
000600060321     d AZUTEDS       e ds                  extname(AZUTE00F)
000700060321     d DDATIUTE      e ds
000800060321     d*
000900060321     D WLBDAT          DS                  INZ
001000060321     D  G02DAT                 1      8  0
001100060321     D  G02INV                 9     16  0
001200060321     D  G02ERR                17     17
001300060321     D  G02TGI                18     22  0
001400060321     d wnfm            s                   Like(tlnnfm)
001500060321     d wpno            s                   Like(tlnpno)
001600060321     d wuno                           1
001700060321     d wprt                           1
001800060321     d*
001900060321     c     ktln          klist
002000060321     c                   kfld                    tlntrn
002100060321     c                   kfld                    tlndde
002200060321     c                   kfld                    wnfm
002300060321     c                   kfld                    wpno
002400060321     c*
002500060321     c     ktlt          klist
002600060321     c                   kfld                    tlntrn
002700060321     c                   kfld                    tlndde
002800060321     C                   TIME                    W0140            14 0
002900060321     C* UDATE IN GGMMAAAA
003000060321     C                   MOVE      W0140         WDTGIO            8 0
003100060321     C*
003200060321     C* UDATE IN AAAAMMGG
003300060321     C                   Z-ADD     WDTGIO        G02DAT
003400060321     C                   MOVEL     *BLANK        G02ERR
003500060321     C                   CALL      'XSRDA8'
003600060321     C                   PARM                    WLBDAT
003700060321     C                   MOVEL     G02INV        DATcor            8 0
003800060321     c*
003900060321     c     *dtaara       define    §azute        AZUTEDS
004000060321     c     *dtaara       define    §datiute      DDATIUTE
004100060321     c                   clear                   AZUTEDS
004200060321     c                   clear                   DDATIUTE
004300060321     c                   clear                   TIBS34DS
004400060321     c                   in(E)     *dtaara
004500060321if  1c                   IF        %Error  or  RSUT = *blanks
004600060321     c                   call      'TIBS34R'
004700060321     c                   parm                    TIBS34DS
004800060321     c                   in        *dtaara
004900060321e   1c                   ENDIF
005000060321     c*
005100060321     c                   except    testa
005200060321     c*
005300060321    1c                   if        tlnnfm > 0
005400060321     c                   z-add     0             wnfm
005500060321     c                   eval      wpno = tlnpno
005600060321     c                   eval      wuno = '0'
005700060321     c                   eval      wprt = '0'
005800060321     c*
005900060321     c     su            tag
006000060321     c     ktln          setll     tntln01l
006100060321    2c                   if        %equal(tntln01l)
006200060321     c                   eval      wpno = wpno + 1
006300060321    3c                   if        wuno = '0'
006400060321     c     ktlt          chain     tntlt01l
006500060321    4c                   if        %found(tntlt01l) and tltdsc >= datcor
006600060321     c                   eval      wprt = '1'
006700060321    4c                   endif
006800060321     c                   eval      wuno = '1'
006900060321    3c                   endif
007000060321     c                   goto      su
007100060321    2c                   endif
007200060321     c* aggiorno file fisico
007300060321     c                   z-add     0             tlnnfm
007400060321     c                   z-add     wpno          tlnpno
007500060321     c                   update    tntln000
007600060321     c* stampo segnalazione di controllare le note del traino
007700060321     c                   if        wprt = '1'
007800060321     c                   except    rigtrn
007900060321     c                   endif
008000060321     c*
008100060321    1c                   endif
008200060321     Oqsysprt   e            testa          2  2
008300060321     o                       rsut             +   1
008400060321     O                                        +  10 'ELENCO TRAINI CON NOTE DA '
008500060321     O                                        +   0 'CONTROLLARE'
008600060321     o                                           85 'TNVRNTTR'
008700060321     o                       *Date         y    110
008800060321     O          e            rigtrn         1
008900060321     o                                        +   1 'Traino n:'
009000060321     o                       tlntrn        2  +   2
009100060321     o                                        +   2 'Decorrenza'
009200060321     o                       w_tlndde         +   2 '  /  /    '
