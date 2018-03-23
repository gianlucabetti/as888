000100951009     H DECEDIT('0,') DATEDIT(*DMY.)
000200980625      * ---------------------------------------------------------
000300141118     fFNARB25L  iF   E           K DISK
000400141118     fFNblp01L  IF   E           K DISK
000500141118     fFNlbl02L  IF   E           K DISK
000600990412     fPRTF198   O    F  132        PRINTER  oflind(*INOF)
000700980611      * ---------------------------------------------------------
000800041222     C     KARB          KLIST
000900141118     C                   KFLD                    KLNa              3 0
001000141118     C                   KFLD                    Kdcm              8 0
001100141118     C     Klbl          KLIST
001200041222     C                   KFLD                    ARBAAS
001300041222     C                   KFLD                    ARBLNP
001400041222     C                   KFLD                    ARBNRS
001500041222     C                   KFLD                    ARBNSP
001600141118     C     Kblp          KLIST
001700141118     C                   KFLD                    lblAAn
001800141118     C                   KFLD                    lbllpn
001900141118     C                   KFLD                    lblnrn
002000141118     C                   KFLD                    lblnsn
002100041222     C
002200141118     c                   eval      kdcm=20141117
002300141118     c                   eval      klna=001
002400141118
002500141118    0c                   dow       klna>0
002600141118     c     karb          setll     fnarb25l
002700141118     c     klna          reade     fnarb25l
002800041222     C
002900141118    1C                   DOW       NOT %EOF(FNARB25L)
003000141118    2c                   if        arbcca='1'
003100141118
003200141118     c     klbl          chain     fnlbl02l
003300141118    3c                   if        not %found(fnlbl02l)
003400141118     c                   except    err1
003500141118   x3c                   else
003600141118     C     Kblp          chain     FNblp01l
003700141118    4c                   if        not %found(fnblp01l) or blplnp<>arblna
003800141118     c                   except    err2
003900141118    4c                   endif
004000141118    3c                   endif
004100141118     c
004200141118    2c                   endif
004300141118     c     klna          reade     fnarb25l
004400141118    1c                   enddo
004500141118     c
004600041222     C
004700141118     c     klna          setgt     fnarb25l
004800141118     c                   read      fnarb25l
004900141118    1c                   if        not %eof(fnarb25l)
005000141118     c                   eval      klna=arblna
005100141118     c                   else
005200141118     c                   clear                   klna
005300141118    1c                   endif
005400141118     c
005500041222    1c                   enddo
005600041222     c
005700041222     c                   seton                                        lr
005800041222     c
005900041222     c
006000980611      *------------------------------------------------------------
006100141118     OPRTF198   E            err1        1
006200141118     O                                        +   0 'bolla DIR senza LBL'
006300041222     O                       arbaas           +   4
006400041222     O                       arblnp           +   2
006500041222     O                       arbnrs        z  +   2
006600041222     O                       arbnsp        z  +   2
006700141118     O                       arblna           +   2
006800141118     O                       arbdcm           +   2 '    /  /  '
006900141118     OPRTF198   E            err2        1
007000141118     O                                        +   0 'bolla DIR senza BLP'
007100141118     O                       arbaas           +   4
007200141118     O                       arblnp           +   2
007300141118     O                       arbnrs        z  +   2
007400141118     O                       arbnsp        z  +   2
007500141118     O                       arblna           +   2
007600141118     O                       arbdcm           +   2 '    /  /  '
