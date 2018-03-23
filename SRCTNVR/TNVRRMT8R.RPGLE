000100940224     H*--------------------------------------------------------------*
000101170320     Ftnvrrmt8d CF   E             WORKSTN
000102170320
000103170320     d*dataiso         s               d   datfmt(*iso)
000104170320     d ICCM            s              7s 0
000105170320     d Idat            s              8s 0
000106170320     d INtw            s              3a
000107170320     d Ipkg            s              7s 1
000108170320     d Itsp            s              1a
000109170320     d Ilang           s              2a
000110170320     d oCtrPr          s              3a
000111170320     d oCtr            s             45
000112170320     d oDes            s            750
000113170320
000114170320     D RTNesito        S             10I 0
000115170320     D RTNopcode       S             10
000116170320     D RTNstatus       S             10I 0
001100900131     C*---------------------------------------------------------------*
001700060512     c
001800060512
001801170320     c                   clear                   iccm
001802170320     c                   clear                   idat
001803170320     c***    dataiso=%date();
001804170320     c***    idat =%dec(dataiso);
001805170320     c                   clear                   intw
001806170320     c                   clear                   ipkg
001807170320     c                   clear                   itsp
001808170320     c                   clear                   ilang
001809170320     c                   clear                   oCtrPr
001810170320     c                   clear                   oCtr
001811170320     c                   clear                   oDes
001812170320     c                   clear                   RTNesito
001813170320     c                   clear                   RTNopcode
001814170320     c                   clear                   RTNstatus
001815170320
001816170320     c                   do        *hival
001817170320     c                   exfmt     v01
001818170320     c                   if        *inkc
001819170320     c                   return
001820170320     c                   endif
001821170320     c                   call      'FISPE08R'
001822170320     C                   PARM                    ICCM
001823170320     C                   PARM                    Idat
001824170320     C                   PARM                    Intw
001825170320     C                   PARM                    Ipkg
001826170320     C                   PARM                    Itsp
001827170320     C                   PARM                    Ilang
001828170320     c                   parm                    oCtrPr
001829170320     c                   parm                    oCtr
001830170320     c                   parm                    oDes
001831170320     C                   PARM                    rtnesito
001832170320     C                   PARM                    rtnopcode
001833170320     C                   PARM                    rtnstatus
001835170320     c                   enddo
005100030205      *
005200000000     C                   SETON                                        LR
