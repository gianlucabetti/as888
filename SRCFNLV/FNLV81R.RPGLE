000100030718     H DECEDIT('0,') DATEDIT(*DMY.)  OPTION(*NODEBUGIO)
000200140205     H* FNLV81R *----------------------------------------------------*
000300140205     H* - Driver di reperimento indirizzo email e/o numero telefono per sms
000301110610     h*
000400000000     H*--------------------------------------------------------------*
000500140205     FFiar501L  IF   E           K DISK
001200030722     F**
001600070313     d
001700140205      * ds parametri input/output
001800140205     D fnlv81ds      E DS
002000070313     d
002100110524     D Dar5emd       E DS
002400070313     d
003400110524     d Ktrd            s                   like(ar5trd) inz('EMD')
003800900518     C***************************************************************
003900000000     C     *ENTRY        PLIST
004000140205     C                   PARM                    fnlv81ds
004300940223     C*---------------------------------------------------------------*
004400140205     C* TIPO LANCIO, ilv81TLA "C" -           CHIUSO CON LR
004500140205     C* TIPO LANCIO, ilv81TLA "L" - ELABORO E CHIUDO CON LR
004600140205     C* TIPO LANCIO, ilv81TLA " " - ELABORO E CHIUDO IN RETRN
004700941216     C*
004800140205    1C     ilv81TLA      IFNE      'C'
004900140205     c                   clear                   olv81eml
005000140205     c                   clear                   olv81tel
005001140205     c                   clear                   olv81err
005003140206     c                   clear                   olv81flo2
005400070412     c
005500051123     c
005900070313     c
006000110524     c     kar5          chain     fiar501l
006100140205    2c                   if        not %found(fiar501l)
006200070313     c* restituisco errore al chiamante ed esco
006400140206     c                   eval      olv81err='N'
006600140205   x2c                   else
007000070326     c
008700110524     c*
008800110524     c                   movel     ar5uni        dar5emd
008801140205    3c                   select
008802140205     c* richiesti entrambi
008803140205     c                   when      ilv81trc='E'
008804140205     c                   eval      OLV81EML=§ar5eml
008805140205     c                   eval      OLV81TEL=§ar5tel
008806140205     c* richiesta mail
008807140205     c                   when      ilv81trc='M'
008808140205     c                   eval      OLV81EML=§ar5eml
008809140205     c* richiesto tel per sms
008810140205     c                   when      ilv81trc='S'
008811140205     c                   eval      OLV81TEL=§ar5tel
008812140205     c* richiesta tel Alert  (se presenti entrambi vince la mail)
008813140205     c                   when      ilv81trc='A'
008814140205     c                   if        §ar5eml<>*blanks
008815140205     c                   eval      OLV81EML=§ar5eml
008816140205     c                   else
008817140205     c                   eval      OLV81TEL=§ar5tel
008818140205     c                   endif
008819140205    3c                   endsl
008820140206     c                   if        olv81tel=*blanks and olv81eml=*blanks
008821140206     c                   eval      olv81err='N'
008822140206     c                   endif
012000140205    2c                   endif
012001140205    1c                   endif
012100051123     c
012200140205    1C     Ilv81tla      IFNE      ' '
012400051123     C                   SETON                                        LR
012500051123     C                   ELSE
012600051123     C                   SETON                                        RT
012700140205    1C                   ENDIF
012800991206     C**************************************************************************
012900051123     C     *INZSR        BEGSR
013000051123     c
013100110524     c     kAR5          Klist
013200140205     c                   Kfld                    ilv81Aas
013300140205     c                   Kfld                    ilv81Lnp
013400140205     c                   Kfld                    ilv81nrs
013500140205     c                   Kfld                    ilv81nsp
013501110524     c                   Kfld                    ktrd
013502110603     c
015300070313     c                   endsr
