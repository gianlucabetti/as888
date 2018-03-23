000100160331    ?H* TnvrC1R *-----------------------------------------------------*
000200160331?    H*            CREAZIONE aut xxx9510                              *
000300011206?    H*---------------------------------------------------------------*
000400011206     H DECEDIT('0,') DATEDIT(*DMY.)
000600160331     FAZORG01L  IF   E           K DISK
000800071109     Ffiapd01l  IF a E           K DISK
000801160331     Ffigrg01l  o    E             DISK
000900011206?    D*---------------------------------------------------------------*
000901160331     D og143         e DS
000902160331     D dgrgflo       e DS
000903160331     D WLBDAT          DS                  INZ
000904160331     D  G02DAT                 1      8  0
000905160331     D  G02INV                 9     16  0
000906160331     D  G02ERR                17     17
000907160331     D  G02TGI                18     22  0
007500020206     C*
007600011206     C* DATA DEL GIORNO
007700011206     C                   TIME                    W0140            14 0
007800011206     C* UDATE IN GGMMAAAA
007900011206     C                   MOVE      W0140         WDTGIO            8 0
008000011206     C* UDATE IN AAAAMMGG
008100011206     C                   Z-ADD     WDTGIO        G02DAT
008200011206     C                   MOVEL     *BLANK        G02ERR
008300011206     C                   CALL      'XSRDA8'
008400011206     C                   PARM                    WLBDAT
008500011206     C                   MOVEL     G02INV        DATEU             8 0
008800071109     c*
008900071109     c     kapd          klist
009000160331     c                   kfld                    ktip              1
009100160331     c                   kfld                    kpdr              7 0
009101160331     c                   eval      ktip = 'A'
014600011206     C                   SETON                                        LR
014601160331     C*
037100160331     c     *loval        setll     azorg01l
037101160331     c                   do        *hival
037102160331     c                   read      azorg01l
037103160331     c*
037104160331     c                   if        %eof(azorg01l)
037105160331     c                   leave
037106160331     c                   end
037107160331     c*
037108160331     c                   movel     orgde3        og143
037900160331     c                   if        orgfag='A' or orgfva = '*'
037902160331     c                   iter
038500071108     c                   endif
038501160331     c                   if        §ogntw <> 'COR' and §ogntw <> 'MES'
038503160331     c                   iter
038504160331     c                   endif
038505160331     c* crea autista fittizio xxx9510
054600071109     c                   clear                   kpdr
054700160331     c                   movel     orgfil        kpdr
054800160331     c                   move      9510          kpdr
058700071109     c     kapd          chain     fiapd01l
058800071109     c                   if        not %found(fiapd01l)
058900160331     c                   movel     0700999       kpdr
059000071109     c     kapd          chain     fiapd01l
059100160331     c                   if        %found(fiapd01l)
059400160331     c                   movel     orgfil        apdpdr
059401160331     c                   move      9510          apdpdr
059500160331     c                   eval      apdrsc='VEDI PACCO             '
059600160331     c                   eval      apdrsf='VEDI PACCO             '
059700071109     c                   z-add     dateu         apddtr
059800071109     c                   clear                   apdftr
059900071109     c                   clear                   apdatb
060000071109     c                   write     fiapd000
060100071109     c                   endif
060200071109     c                   endif
060201160331     c* crea giro VP
060202160331     C                   clear                   figrg000
060203160331     C                   clear                   DGRGFLO
060204160331     C*
060205160331     C                   z-add     20070101      grgdde
060206160331     C                   z-add     20391231      grgdscad
060207160331     C                   move(p)   orgfil        grgfgs
060208160331     C                   movel     '='           grgain
060210160331     C                   movel     'S'           §GRGEDP
060211160331     C                   movel     'E'           §GRGTUG
060213160331     C                   z-add     dateu         grgduv
060214160331     C                   movel     'EDP001'      grgute
060215160331     C*
060216160331     C                   movel     'S'           §GRGFIT
060217160331     C                   movel     DGRGFLO       grgflo
060218160331     C                   movel     'VP'          grgcgi
060219160331     C                   eval      grgdes = 'VEDI PACCO    '
060220160331     C                   write(e)  figrg000
060300160331     c                   enddo
