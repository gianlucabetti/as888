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
003700061108
003800971121?     *--------------------------------------------------------------*
003900041029      * Carico tutti i terminal di partenza
004000041029     c                   Clear                   Tibs56ds
004100041029     c                   Eval      I56Ppo = Simfel
004200041029     c                   Eval      I56Mod = 'TPS'
004300041029     c                   Call      'TIBS56R'
004400041029     c                   Parm                    Tibs56ds
004500041029     c                   If        O56Err <> *blanks
004600041029     c                   Clear                   Ski
004700041029     c                   EndIf
004800041029     c
004900041029     c
005000041029     c                   z-add     1             xx                3 0
005100041029     c                   dow       ski(xx)>0
005200061108     c                   movel     ski(xx)       pa2fel
005300061108     c     pa2fel        chain     azorg01l
005400061108     c                   if        %found(azorg01l) and orgfva = ' '
005500061108     c                   movel     orgdf0        og150
005600061108     c                   movel     orgde8        og148
005700061108     c                   else
005800061108     c                   clear                   og150
005900061108     c                   clear                   og148
006000061108     c                   endif
006100061108     c                   testn                   §ogvol               30
006200061108     c                   if        *in30 and §ogvol > *zeros
006300061108     c                   eval      pa2cml = 'S'
006301120917     c                   eval      pa2spv = 'S'
006400061108     c                   endif
006401120917     c                   testn                   §ogpes               30
006402120917     c                   if        *in30 and §ogpes > *zeros
006403120917     c                   eval      pa2cml = 'P'
006404120917     c                   eval      pa2spp = 'S'
006405120917     c                   endif
006500120917     c***                eval      pa2spv=§ogspv
006600120917     c***                eval      pa2spp=§ogspp
006700061108     c                   if        §ogags = 'E'
006800061108     c                   eval      pa2ags = 'S'
006900061108     c                   eval      pa2agp = §ogagp
007000061108     c                   else
007100061108     c                   clear                   pa2ags
007200061108     c                   clear                   pa2agp
007300061108     c                   endif
007400061108      * carico la £1
007500061108     C                   CLEAR                   TRUL06DS
007600061108     C                   MOVE      '£1'          D06COD
007700061108     C                   MOVEL     pa2fel        D06KEY
007800061108     C                   MOVEL(P)  TRUL06DS      KPJBU
007900061108     C                   CALL      'TRUL06R'
008000061108     C                   PARM                    KPJBA
008100061108     C                   MOVEL     KPJBU         TRUL06DS
008200061108     C                   MOVEA     LIN           L1
008300061108     c                   eval      pa2sut=rsut
008400041029     c
008500061108     c                   eval      pa2fvg = '99999'
008600061108     c                   movel(p)  param2        kpjbu
008700061108     c                   movel     'LSA2'        KCOAZ
008800080102     C                   CLEAR                   KBUFF
008900080102     C                   CLEAR                   Kqebi
009000061108     c                   call      'BCH10'
009100061108     c                   parm                    kpjba
009200041029     c                   add       1             xx
009300041029     c                   enddo
009400061108     c*
009500061108     c* lancio la trasmissione delle bolle in sede
009600061109     C                   CLEAR                   KPJBU
009700061109     c                   movel     '99999'       pa3nfv
009800061109     c                   movel     param3        kpjbu
009900061109     c                   movel     'LSA3'        KCOAZ
010000080102     C                   CLEAR                   KBUFF
010100080102     C                   CLEAR                   Kqebi
010200061109     c                   call      'BCH10'
010300061109     c                   parm                    kpjba
010400041029     c
010500910521     C                   SETON                                        LR
010600041029      **********************************************************************
010700941018     C     *INZSR        BEGSR
010800941018     C     *ENTRY        PLIST
010900941018     C                   PARM                    KPJBA
011000941018      *
011100061106     c     *dtaara       define    §azute        azuteds
011200061106     c     *dtaara       define    §datiute      ddatiute
011300061106     c                   in(E)     *dtaara
011400061106     c                   If        %error  or RSUT = *blanks
011500061106     c                   CLEAR                   tibs34ds
011600061106     c                   CALL      'TIBS34R'
011700061106     c                   PARM                    tibs34ds
011800061106     c                   in        *dtaara
011900061106     c                   EndIf
012000941018     C                   ENDSR
