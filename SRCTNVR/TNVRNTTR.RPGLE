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
001600060322     d w_tlntrn        s                   Like(tlntrn) inz
001700060322     d w_tlndde        s                   Like(tlndde) inz
001800060321     d wuno            s              1
001900060321     d wprt            s              1
002000060321     d*
002100060321     c*
002200060321    1c                   if        tlnnfm > 0
002300060321     c                   z-add     0             wnfm
002400060321     c                   eval      wpno = tlnpno
002500060322     c                   eval      wprt = '0'
002600060322     c                   if        tlntrn <> w_tlntrn  or tlndde <> w_tlndde
002700060321     c                   eval      wuno = '0'
002800060322     c                   eval      w_tlntrn = tlntrn
002900060322     c                   eval      w_tlndde = tlndde
003000060322     c                   endif
003100060321     c*
003200060321     c     su            tag
003300060321     c     ktln          setll     tntln01l
003400060321    2c                   if        %equal(tntln01l)
003500060321     c                   eval      wpno = wpno + 1
003600060321    3c                   if        wuno = '0'
003700060321     c     ktlt          chain     tntlt01l
003800060321    4c                   if        %found(tntlt01l) and tltdsc >= datcor
003900060321     c                   eval      wprt = '1'
004000060321    4c                   endif
004100060321     c                   eval      wuno = '1'
004200060321    3c                   endif
004300060321     c                   goto      su
004400060321    2c                   endif
004500060321     c* aggiorno file fisico
004600060321     c                   z-add     0             tlnnfm
004700060321     c                   z-add     wpno          tlnpno
004800060321     c                   update    tntln000
004900060321     c* stampo segnalazione di controllare le note del traino
005000060321     c                   if        wprt = '1'
005100060321     C* data decorrenza traino IN ggmmaaaa
005200060321     c                   clear                   wlbdat
005300060321     C                   Z-ADD     tlndde        G02inv
005400060321     C                   MOVEL     '3'           G02ERR
005500060321     C                   CALL      'XSRDA8'
005600060321     C                   PARM                    WLBDAT
005700060322     C                   MOVEL     G02dat        I_Tlndde          8 0
005800060321     c                   except    rigtrn
005900060321     c                   endif
006000060321     c*
006100060321    1c                   endif
006200060321     c     *inzsr        begsr
006300060321     c     ktln          klist
006400060321     c                   kfld                    tlntrn
006500060321     c                   kfld                    tlndde
006600060321     c                   kfld                    wnfm
006700060321     c                   kfld                    wpno
006800060321     c*
006900060321     c     ktlt          klist
007000060321     c                   kfld                    tlntrn
007100060321     c                   kfld                    tlndde
007200060321     C                   TIME                    W0140            14 0
007300060321     C* UDATE IN GGMMAAAA
007400060321     C                   MOVE      W0140         WDTGIO            8 0
007500060321     C*
007600060321     C* UDATE IN AAAAMMGG
007700060321     C                   Z-ADD     WDTGIO        G02DAT
007800060321     C                   MOVEL     *BLANK        G02ERR
007900060321     C                   CALL      'XSRDA8'
008000060321     C                   PARM                    WLBDAT
008100060321     C                   MOVEL     G02INV        DATcor            8 0
008200060321     c*
008300060321     c     *dtaara       define    §azute        AZUTEDS
008400060321     c     *dtaara       define    §datiute      DDATIUTE
008500060321     c                   clear                   AZUTEDS
008600060321     c                   clear                   DDATIUTE
008700060321     c                   clear                   TIBS34DS
008800060321     c                   in(E)     *dtaara
008900060321if  1c                   IF        %Error  or  RSUT = *blanks
009000060321     c                   call      'TIBS34R'
009100060321     c                   parm                    TIBS34DS
009200060321     c                   in        *dtaara
009300060321e   1c                   ENDIF
009400060321     c*
009500060321     c                   except    testa
009600060321     c                   endsr
009700060321     Oqsysprt   e            testa          2  2
009800060321     o                       rsut             +   1
009900060321     O                                        +  10 'ELENCO TRAINI CON NOTE DA '
010000060321     O                                        +   0 'CONTROLLARE'
010100060321     o                                           85 'TNVRNTTR'
010200060322     o                       *Date         8    110
010300060322     O          e            testa          2
010400060322     o                                        +   2 'N.Traino'
010500060322     o                                        +   2 'Decorrenza'
010600060322     O          e            rigtrn         1
010700060322     o                       tlntrn        2  +   1
010800060322     o                       I_tlndde         +   2 '  /  /    '
