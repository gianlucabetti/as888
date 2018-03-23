000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200061106?     * FNLSA2R9 ----------------------------------------------------*
000300061106      *        ELABORAZIONE LSA2 F.VIAGGIO 99999                     *
000400971121?     *--------------------------------------------------------------*
000500061108     FAZORG01L  IF   E           K DISK
000600900514     D KPJBA         E DS
000700061106     d Azuteds       e ds                  Extname(Azute00f)
000800061106     d dDatiute      e ds
000900061106     d Tibs34ds      e ds                  Inz
001000061108     d og150         e ds                  Inz
001100061108     d og148         e ds                  Inz
001200020805
001300061108     D trul06ds      e ds
001400061108     D  LIN                    1     90  0    DIM(30)
001500041029     d tibs56ds      e ds
001600041029     d  ski                    7    756  0 dim(250)
001700061108      * ds richiamo PGM fnlsa2
001800061108     d PARAM2          DS
001900061108     d  pa2npg                 1      1  0
002000061108     d  pa2nfv                 2      4P 0 inz(00000)
002100061108     d  pa2dfv                 5      9P 0 inz(00000000)
002200061108     d  pa2ags                10     10
002300061108     d  pa2fvg                11     15
002400061108     d  pa2num                17     17  0
002500061108     d  pa2flg                19     19
002600061108     d  L1                    78    137P 0    dim(30)
002700061108     d  pa2sut               138    157
002800061108     d  pa2fel               158    160  0
002900061108     d  pa2fgs               164    166  0
003000061108     d  pa2cml               167    167
003100061108     d  pa2agp               168    168
003200061108     D  PA2spv               169    169
003300061108     D  PA2spp               170    170
003400061108
003500061108     d PARAM3          DS
003600061108     d  pa3nfv                 1      5
003700110325     d  pa3tfp                 6      8    inz('001')
003800061108
003900971121?     *--------------------------------------------------------------*
004000041029      * Carico tutti i terminal di partenza
004100041029     c                   Clear                   Tibs56ds
004200041029     c                   Clear                   Ski
004300110325     c                   eval      ski(1)=001
004400041029     c
004500041029     c
004600041029     c                   z-add     1             xx                3 0
004700061108     c                   movel     ski(xx)       pa2fel
004800061108     c     pa2fel        chain     azorg01l
004900061108     c                   if        %found(azorg01l) and orgfva = ' '
005000061108     c                   movel     orgdf0        og150
005100061108     c                   movel     orgde8        og148
005200061108     c                   else
005300061108     c                   clear                   og150
005400061108     c                   clear                   og148
005500061108     c                   endif
005600061108     c                   testn                   §ogvol               30
005700061108     c                   if        *in30 and §ogvol > *zeros
005800061108     c                   eval      pa2cml = 'S'
005900061108     c                   endif
006000061108     c                   eval      pa2spv=§ogspv
006100061108     c                   eval      pa2spp=§ogspp
006200061108     c                   if        §ogags = 'E'
006300061108     c                   eval      pa2ags = 'S'
006400061108     c                   eval      pa2agp = §ogagp
006500061108     c                   else
006600061108     c                   clear                   pa2ags
006700061108     c                   clear                   pa2agp
006800061108     c                   endif
006900061108      * carico la £1
007000061108     C                   CLEAR                   TRUL06DS
007100061108     C                   MOVE      '£1'          D06COD
007200061108     C                   MOVEL     pa2fel        D06KEY
007300061108     C                   MOVEL(P)  TRUL06DS      KPJBU
007400061108     C                   CALL      'TRUL06R'
007500061108     C                   PARM                    KPJBA
007600061108     C                   MOVEL     KPJBU         TRUL06DS
007700061108     C                   MOVEA     LIN           L1
007800061108     c                   eval      pa2sut=rsut
007900041029     c
008000061108     c                   eval      pa2fvg = '99999'
008100061108     c                   movel(p)  param2        kpjbu
008200080102     C                   CLEAR                   KBUFF
008300080102     C                   CLEAR                   Kqebi
008400110325     c                   call      'TNVRA2R8'
008500061108     c                   parm                    kpjba
008600061108     c*
008700061108     c* lancio la trasmissione delle bolle in sede
008800061109     C                   CLEAR                   KPJBU
008900061109     c                   movel     '99999'       pa3nfv
009000061109     c                   movel     param3        kpjbu
009100061109     c                   movel     'LSA3'        KCOAZ
009200080102     C                   CLEAR                   KBUFF
009300080102     C                   CLEAR                   Kqebi
009400061109     c                   call      'BCH10'
009500061109     c                   parm                    kpjba
009600041029     c
009700910521     C                   SETON                                        LR
009800041029      **********************************************************************
009900941018     C     *INZSR        BEGSR
010000941018     C     *ENTRY        PLIST
010100941018     C                   PARM                    KPJBA
010200941018      *
010300061106     c     *dtaara       define    §azute        azuteds
010400061106     c     *dtaara       define    §datiute      ddatiute
010500061106     c                   in(E)     *dtaara
010600061106     c                   If        %error  or RSUT = *blanks
010700061106     c                   CLEAR                   tibs34ds
010800061106     c                   CALL      'TIBS34R'
010900061106     c                   PARM                    tibs34ds
011000061106     c                   in        *dtaara
011100061106     c                   EndIf
011200941018     C                   ENDSR
