000100081209     H DECEDIT('0,') DATEDIT(*YMD.)
000200091028     H* TRMZ68R  *---------------------------------------------------*
000300091028      *          AGGIORNA AITRA la rag.sociale (TRASOC)
000400000000     H*--------------------------------------------------------------*
000500140801     Ffidga1d   CF   E             WORKSTN
000600091028      *
000700140801     Ffidst01L  uF   E           K DISK
000800091028      *
000900140801     Fazorg01L  iF   E           K DISK
001000090410     Ffiapd01L  iF   E           K DISK
001100140801     Ffidsf01L  iF   E           K DISK
001200090102      *
001300090410     C*****************************************************************
001400091028     D kpjba         E DS
001500140806     D ddstflr       E DS
001600900521     C*****************************************************************
001700091023     c     *entry        plist
001800091023     c                   parm                    kpjba
001900091028     C                   SETON                                        LR
002000091023      *
002100140801     c     kdst          klist
002200140801     c                   kfld                    knpg
002300140801     c                   kfld                    v1cdis
002400140801     c                   kfld                    v1cfil
002500140801     c                   z-add     4             knpg              1 0
002600140801     c     kdsf          klist
002700140801     c                   kfld                    knpg
002800140801     c                   kfld                    v1cdis
002900140801     c                   kfld                    v1cfil
003000140801     c                   kfld                    ktrd
003100140801     c                   move      'CRI'         ktrd              3
003200140801     c     kapd          klist
003300140801     c                   kfld                    tipo
003400140801     c                   kfld                    dstpdr
003500140801     c                   move      'A'           tipo              1
003600091028     c     inizio        tag
003700140801     c                   clear                   v1cfil
003800140801     c                   clear                   v1cdis
003900091028      *
004000140801     c                   do        *hival
004100091028     c     video         tag
004200091028     c                   setoff                                       99
004300091028      *                 ------------------
004400091028     c                   exfmt     video1
004500091028      *                 ------------------
004600140801     c   KC              leave
004700081110      *
004800091028     c                   exsr      contr1
004900140801     c   99              goto      video
005000140806     c                   movel     dstflr        ddstflr
005100140806     c                   movel     §DSTTSTPDA    vflagt
005200091028      *
005300091028     c     finestra      tag
005400091028      *                 ------------------
005500140801     c                   do        *hival
005600140801     c                   setoff                                       99
005700091028     c                   exfmt     videoW
005800091028      *                 ------------------
005900140801     c                   exsr      contrw
006000140806     c   Kl              goto      video
006100140801     c   99              goto      finestra
006200140801     c
006300091028      *
006400140801     c   kf              exsr      esegui
006500140801     c   kf              leave
006600140801     c                   enddo
006700091028      *
006800140801     C                   enddo
006900091028     c                   return
007000091028     C*****************************************************************
007100091028      *  Controlli su nr.contratto o codice aut
007200091028     C*****************************************************************
007300091028     c     contr1        begsr
007400091028      *
007500091028     c                   setoff                                       414243
007600140801     c                   setoff                                       44
007700091028      *
007800091028      *  numero contratto
007900140801     c                   if        v1cfil = 0 or v1cdis = 0
008000091028     c                   seton                                        4199
008100091028     c                   else
008200091028      *
008300140801     c     kdst          chain     fidst01l
008400140801     c                   if        not %found(fidst01l)
008500140801     c                   seton                                        4299
008600091028     c                   else
008700140801     c                   if        dstfcf <> *blank
008800140801     c                   seton                                        4399
008900140801     c                   endif
009000140806     c                   movel     dstflr        ddstflr
009100140806     c                   if        §DSTTSTPDA = *blank or
009200140806     c                             §DSTTSTPDA = 'O'
009300140801     c                   seton                                        4499
009400140801     c                   endif
009500091028     c                   endIf
009600091028     c                   end
009700140801     c                   clear                   v1caut
009800140801     c                   clear                   v1daut
009900140801     c     kapd          chain     fiapd01l
010000140801     c                   if        %found(fiapd01l)
010100140801     c                   z-add     apdpdr        v1caut
010200140801     c                   movel     apdrsf        v1daut
010300140801     c                   endif
010400140801     c     v1cfil        chain     azorg01l
010500140801     c                   if        %found(azorg01l)
010600140801     c                   movel     orgdes        v1dfil
010700140801     c                   endif
010800140801      *
010900091028     c                   endsr
011000140801     C*****************************************************************
011100140801      *  Controlli x finestra flag test
011200140801     C*****************************************************************
011300140801     c     contrw        begsr
011400140801      *
011500140801     c                   setoff                                       30
011600140801     c                   seton                                        31
011700140801      *
011800140801      *  numero contratto
011900140801     c                   if        vflagt = 'E' and  vflagtn<> 'O' or
012000140801     c                             vflagt = 'C' and  vflagtn<> ' '
012100140801     c                   seton                                          99
012200140801     c                   endif
012300140801      *
012400140801     c     kdsf          chain     fidsf01l
012500140801     c                   if        %found(fidsf01l)
012600140801     c                   seton                                        30
012700140801     c                   endif
012800140801      *
012900140801     c                   endsr
013000091028     C*****************************************************************
013100091028      *    Esegue l'aggiornamento
013200091028     C*****************************************************************
013300091028     c     esegui        begsr
013400091028      *
013500140806     c                   movel     vflagtn       §DSTTSTPDA
013600140806     c                   movel     ddstflr       dstflr
013700140801     c                   update    fidst000
013800091023      *
013900091028     c                   ENDsr
