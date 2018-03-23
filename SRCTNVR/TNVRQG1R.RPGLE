000001030402     h DEBUG DECEDIT('0,') DATEDIT(*ymd.)
000100020911      *--------------------------------------------------------------*
000700030402      *    crea il primo record sul file FNQIG00F                    *
000800020911      *--------------------------------------------------------------*
000900030402     Ffnqig00f  o    e             disk
000901030411     Fazorg01l  iF   E           K DISK
001100020911
001700030402     d kpjba         e ds
001701030411     D og148         E DS
001800020911     D trul06ds      e ds
001900020911     d  lin                    1     90  0 dim(30)
001901030402     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
001902030402     D CNCR80        E DS
002000020911
002500020702      *---------------------------------------------------------------*
002600020911     c                   do        30            i                 3 0
002700020911      *loop con le chiavi delle tabelle personalizzate per filiale
002800020911     c                   if        lin(i) = 0
002900020911     c                   leave
003000020911     c                   endif
003001030411     c* controllo che il po sia operativo
003002030411     c                   exsr      srorg
003003030411     c                   if        §oglpo = ' '
003004030411     c                   iter
003005030411     c                   end
003006030402     c                   clear                   fnqig000
003007030402     c                   eval      qigfle = lin(i)
003008030402     c                   eval      qigcuc = 999
003009030402     c                   eval      qigvca = 'EUR'
003010030402     c                   eval      qigdta = *date
003011030402     c                   write     fnqig000
005100020911     c                   ENDDO
005200020911
005300020911     C                   eval      *inlr = *on
005301030411     C*-----------------------------------------------------------*
005302030411     C*  aggancio AZORG01L
005303030411     C*-----------------------------------------------------------*
005304030411     C     srorg         BEGSR
005305030411     c                   clear                   og148
005306030411     c     lin(i)        chain     azorg01l
005307030411     c                   if        %found(azorg01l)
005308030411     c                   eval      og148 = orgde8
005309030411     c                   end
005310030411     C                   ENDSR
005400020911      *-----------------------------------------
005500020911     c     *inzsr        begsr
005600020911      *---------------------------------------------------------------*
005601030402     C                   Z-ADD     1             CODUT
005602030402     C                   CALL      'X§PARUT'
005603030402     C                   PARM                    UTEDSE
005604030402     C                   MOVEL     REC80         CNCR80
007400020911     C* CARICO I P.O. DI AREA
007500020911     C                   CLEAR                   trul06ds
007600020911     C                   MOVE      '£1'          D06COD
007700020911     C                   MOVEL     'L'           D06TLA
007800020911     C                   MOVEL     simfel        D06KEY
007900020911     C                   MOVEL     trul06ds      KPJBU
008000020911     C                   CALL      'TRUL06R'
008100020911     C                   PARM                    KPJBA
008200020911     C                   MOVEL     KPJBU         trul06ds
008400020911      *
008500020911     c                   endsr
