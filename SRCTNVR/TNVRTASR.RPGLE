000100031105     H DECEDIT('0,') DATEDIT(*YMD.)
000200031105
000300031105      *****************************************************************
000400031105      *                                                               *
000500060519      *       AGGIORNA TITAI                                          *
000600031105      *                                                               *
000700031105      *****************************************************************
000800140312     FTITAS26c  IF   E           k DISK
000900140312     Ftntam01l  IF   E           k DISK
001000140313     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001100140312     D fnlv59ds      E DS
001200140313     D                 DS
001300140313     D  tasaas                 1      4  0
001400140313     D  taslnp                 5      7  0
001500140313     D  tasnrs                 8      9  0
001600140313     D  tasnsp                10     16  0
001700140313     D  sped                   1     16  0
001800140313     d
001900031105
002000050302      *
002100140312     c     ktas          klist
002200140312     c                   kfld                    dcm
002300140312     c                   kfld                    lna
002400140313
002500140313     C                   TIME                    WTIME            14 0
002600140313     C                   MOVE      WTIME         WDATE             8 0
002700140312     c
002800140312     c                   z-add     20140131      dcm               8 0
002900140312     c                   z-add     330           lna               3 0
003000140312     c                   setoff                                       01
003100140312     c                   dow       *in01=*off
003200140312     c     dcm           setgt     titas26c
003300140312     c                   read      titas26c
003400140312     c                   if        not %eof(titas26c)
003500140312     c                   eval      dcm=tasdcm
003600140312     c                   exsr      LeggiTAS
003700140312     c                   else
003800140312     c                   seton                                        01
003900140312     c                   endif
004000140312     c                   enddo
004100060228
004200140312     c* ota leggo le non consegnate
004300140312     c                   clear                   dcm
004400140312     c                   exsr      LeggiTAS
004500060228
004600140313     c                   except    totale
004700140313
004800050302     C                   EVAL      *inlr = *on
004900140312     c*************************************************************
005000140312     c     LEGGITAS      bEGSR
005100140312     c     ktas          setll     titas26c
005200140312     c     ktas          reade     titas26c
005300140312
005400140313    1c                   dow       not %eof(TITAS26c)
005500140312     c* escludo le bolle con la serie
005600140313    2c                   if        tasaas=2014 and tasmgs>=0201 and
005700140313     c                             taSmgs<=0228 and tasnrs=0 and
005800140313     c                             tastbl<>'A3'  and tastbl<>'F3'
005900140313   2ac                   if        tasncl=1 and taspkb<31,5 and tasfca<>'S'
006000140312     c                   add       1             tot330            9 0
006100140312     c* controllo se il cliente mittente ha una tariffa DPD
006200140312     c                   move      tasccm        alfaccm           4
006300140313    3c                   if        alfaccm<>'8888' and alfaccm<>'9999'
006400140312     c                   clear                   fnlv59ds
006500140312     C                   MOVEL     'P'           ilv59tbo
006600140313     C                   MOVEL     00151         ilv59kci
006700140313     C                   MOVEL     tasccm        ilv59Ksc
006800140312     c                   z-add     tasmgs        ilv59DSP
006900140312     C                   MOVEL     tasaas        ilv59DSP
007000140312     C                   MOVEL     tasTSP        ilv59TSP
007100140312     C                   MOVEL     'S'           ilv59RIC
007200140312     C* CONTROLLO TRA LE TARIFFE ESTERE E ITALIADPD
007300140312     C                   MOVEL     'D'           ilv59FIE
007400140312     C                   CALL      'FNLV59R'
007500140312     C                   PARM                    fnlv59ds
007600140312     C*
007700140312     C* SE NON HO TROVATO LA TARIFFA DEL CLIENTE PRENDOPRG
007800140313    4C     olv59err      IFEQ      ' '
007900140312     C     olv59tks      ANDEQ     'S'
008000140312     c                   add       1             sitar             9 0
008100140312     c                   else
008200140312     c                   add       1             notar             9 0
008300140313    4c                   endif
008400140313   x3c                   else
008500140313     c                   add       1             notar             9 0
008600140313     c*
008700140313    3c                   endif
008800140313     c
008900140313   2ac                   endif
009000140313    2c                   endif
009100140312     c     ktas          reade     titas26c
009200140313    1c                   enddo
009300140312     c
009400140312     c                   ENDSR
009500140313     OQSYSPRT   E            totale           02
009600140313     o                                              'Bolle linea arrivo'
009700140313     o                                              ' 330 1 collo peso fat.<='
009800140313     o                                              ' 31,5 no DISK B '
009900140313     o                                         + 10 'Data elaborazione'
010000140313     O                       wdate             +  2 '  /  /    '
010100140313     OQSYSPRT   E            totale      2
010200140313     o                                              'TOTALE :'
010300140313     O                       tot330        2   +  2
010400140313     o                                         +  2 'di cui mittente con '
010500140313     o                                              'tariffa DPD:'
010600140313     O                       sitar         2   +  2
010700140313     o                                         +  2 'di cui senza tariffa DPD:'
010800140313     O                       notar         2   +  2
