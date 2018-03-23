000100040805     h decedit('0,') datedit(*ymd.)
000200021010      *
000300040913      * FITS20R  *----------------------------------------------------*
000400070207      *  autotrasportatori: FIAPD*
000500021010      *---------------------------------------------------------------* c
000600040913     fFIAPDNEW  If A e           k disk    rename(FIAPD000:APDNEW)
000700040913     fFIAPD00F  If   e             disk
000701070207     fFIAPD2NEW If A e           k disk    rename(FIAPD200:APD2NEW)
000702070207     fFIAPD20F  If   e             disk
000703070207     fFIAPD3NEW If A e           k disk    rename(FIAPD300:APD3NEW)
000704070207     fFIAPD30F  If   e             disk
000705070725     fFIAPD5NEW If A e           k disk    rename(FIAPD500:APD5NEW)
000706070725     fFIAPD50F  If   e             disk
000800040805     FqSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000900021008      *
001000021010     d KPJBA         E DS
001100040805     D* Schiera p.o. da trasferire
001200040805     D  SKPO           s              3    dim(500)
001300040805      *
001400021008      *****************************************************************
001500021010     c     *ENTRY        PLIST
001600021010     c                   PARM                    KPJBA
001700040805     C                   parm                    PARAM          1500
001800040913     C     KAPD          klist
001900040913     C                   kfld                    APDTIP
002000040913     C                   kfld                    APDPDR
002001070207     C     KAPD2         klist
002002070207     C                   kfld                    APD2TIPA
002003070207     C                   kfld                    APD2PDR
002004070207     C                   kfld                    APD2FPP
002005070207     C                   kfld                    APD2DDE
002006070207     C                   kfld                    APD2DSCAD
002007070207     C     KAPD3         klist
002008070207     C                   kfld                    APD3TIPA
002009070207     C                   kfld                    APD3PDR
002010070207     C                   kfld                    APD3FPP
002011070207     C                   kfld                    APD3DDE
002012070207     C                   kfld                    APD3DSCAD
002013070207     C                   kfld                    APD3PRGG
002014070207     C                   kfld                    APD3SQL
002015070725     C     KAPD5         klist
002016070725     C                   kfld                    APD5TIPA
002017070725     C                   kfld                    APD5PDR
002018070725     C                   kfld                    APD5FPP
002019070725     C                   kfld                    APD5DDE
002020070725     C                   kfld                    APD5DSCAD
002021070725     C                   kfld                    APD5PRGG
002022070725     C                   kfld                    APD5SQL
002100040805     c
002200040805     c     ' ':'0'       xlate     param         param
002300040805     c                   movea     param         skpo
002400021010      *---------------------------------------------------------------*
002500021008      ****
002600040910      ** LETTURA
002700021008      ****
002800040913     c                   READ      Fiapd00f                               22
002900040910    1c     *IN22         DOWEQ     *OFF
003000040805     c
003100040913     c                   movel     apdpdr        wlinea            3
003200040805     c     wlinea        lookup    skpo                                   35
003300040913     C
003400040910    2c                   if        *in35
003500040913     c     kapd          setll     fiaPDnew                               31
003600040910    3c                   if        not *in31
003700040913     c                   write     apdNEW                               61
003800040913     c                   add       1             Totapd            7 0
003900040910    3c                   endif
004000040910    2c                   endif
004100021008      *
004200040913     c                   READ      Fiapd00f                               22
004300040910    1c                   ENDDO
004301070207     c* fiapd20f
004302070207     c                   READ      Fiapd20f                               22
004303070207    1c     *IN22         DOWEQ     *OFF
004304070207     c
004305070207     c                   movel     apd2pdr       wlinea            3
004306070207     c     wlinea        lookup    skpo                                   35
004307070207     C
004308070207    2c                   if        *in35
004309070207     c     kapd2         setll     fiaPD2new                              31
004310070207    3c                   if        not *in31
004311070207     c                   write     apd2NEW                              61
004312070207     c                   add       1             Totapd2           7 0
004313070207    3c                   endif
004314070207    2c                   endif
004315070207      *
004316070207     c                   READ      Fiapd20f                               22
004317070207    1c                   ENDDO
004319070207     c* fiapd30f
004320070207     c                   READ      Fiapd30f                               22
004321070207    1c     *IN22         DOWEQ     *OFF
004322070207     c
004323070207     c                   movel     apd3pdr       wlinea            3
004324070207     c     wlinea        lookup    skpo                                   35
004325070207     C
004326070207    2c                   if        *in35
004327070207     c     kapd3         setll     fiaPD3new                              31
004328070207    3c                   if        not *in31
004329070207     c                   write     apd3NEW                              61
004330070207     c                   add       1             Totapd3           7 0
004331070207    3c                   endif
004332070207    2c                   endif
004333070207      *
004334070207     c                   READ      Fiapd30f                               22
004335070207    1c                   ENDDO
004336070725     c* fiapd50f
004337070725     c                   READ      Fiapd50f                               22
004338070725    1c     *IN22         DOWEQ     *OFF
004339070725     c
004340070725     c                   movel     apd5pdr       wlinea            3
004341070725     c     wlinea        lookup    skpo                                   35
004342070725     C
004343070725    2c                   if        *in35
004344070725     c     kapd5         setll     fiaPD5new                              31
004345070725    3c                   if        not *in31
004346070725     c                   write     apd5NEW                              61
004347070725     c                   add       1             Totapd5           7 0
004348070725    3c                   endif
004349070725    2c                   endif
004350070725      *
004351070725     c                   READ      Fiapd50f                               22
004352070725    1c                   ENDDO
004400021010      *
004500040805     c                   EXCEPT    RINUM1
004600021011     c                   seton                                        LR
004700021010      *
004800021010      *---------------------------------------------------------------*
004900040805     oQSYSPRT   E            RINUM1         2 02
005000040913     o                                         +  5 'AUTOTRASPORTATORI'
005100040913     o                                         +  5 'Fits20R'
005200021010     o                       UDATE             +  2 '  /  /  '
005300021010     o          E            RINUM1      1
005400040913     o                                         +  3 'TOTALE FIAPD00F:'
005500040913     o                       TOTAPD        2   +  2
005600070207     o                                         +  3 'TOTALE FIAPD20F:'
005700070207     o                       TOTAPD2       2   +  2
005800070207     o                                         +  3 'TOTALE FIAPD30F:'
005900070207     o                       TOTAPD3       2   +  2
006000070725     o                                         +  3 'TOTALE FIAPD50F:'
006100070725     o                       TOTAPD5       2   +  2
