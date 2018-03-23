000100030718     H DECEDIT('0,') DATEDIT(*DMY.)  OPTION(*NODEBUGIO)
000200140206     H* FNLV82R *----------------------------------------------------*
000300140206     H* - Driver di Verifica stato Alert
000301110610     h*
000400000000     H*--------------------------------------------------------------*
000500140206     FFitgd11l  IF   E           K DISK
000501140206     Ffisia01l  IF   E           K DISK
001200030722     F**
001600070313     d
001700140205      * ds parametri input/output
001800140206     D fnlv82ds      E DS
002000070313     d
002001140206     d ktor            s                   like(tgdtor) inz('S')
002002140206     d kogg            s                   like(tgdogg)
002003140206     d kmad            s                   like(tgdmad)
002004140206
003800900518     C***************************************************************
003900000000     C     *ENTRY        PLIST
004000140206     C                   PARM                    fnlv82ds
004300940223     C*---------------------------------------------------------------*
004400140205     C* TIPO LANCIO, ilv81TLA "C" -           CHIUSO CON LR
004500140205     C* TIPO LANCIO, ilv81TLA "L" - ELABORO E CHIUDO CON LR
004600140205     C* TIPO LANCIO, ilv81TLA " " - ELABORO E CHIUDO IN RETRN
004700941216     C*
004800140206    1C     ilv82TLA      IFNE      'C'
004900140206     c                   clear                   OLV82STS
005000140206     c                   clear                   OLV82DOIS
005001140206     c                   clear                   OLV82DOIA
005003140206     c                   clear                   OLV82FLO2
005400070412     c
005500051123     c
005900140206     c* Cerco la R.A.
005901140206     c                   exsr      sr_CerRA
005902140206     c* non trovata R.A. cerco nel file spia
005903140206    2c                   if        olv82sts<>'1'
007000140206     c                   exsr      sr_cerSPIA
012000140205    2c                   endif
012001140205    1c                   endif
012100051123     c
012200140206    1C     Ilv82tla      IFNE      ' '
012400051123     C                   SETON                                        LR
012500051123     C                   ELSE
012600051123     C                   SETON                                        RT
012700140205    1C                   ENDIF
012701140206     C**************************************************************************
012702140206     C     sr_cerRA      BEGSR
012703140206     c* Ricerca Richiesta Assistenza
012704140206     c* impostazinoe campi chiave:
012705140206     c* oggetto --> ky spedizione
012706140206     c                   eval      kogg=%editc(ilv82lnp:'X')
012707140206     c                                 +%editc(ilv82nrs:'X')
012708140206     c                                 +%editc(ilv82nsp:'X')
012709140206     c                                 +%editc(ilv82aas:'X')
012710140206     c* motivo apertura
012711140206     c                   select
012713140206     c* Fermo Deposito
012714140206     c                   when      ilv82tipa='FD'
012715140206     c                   eval      kmad='80'
012716140206     c* Lasciato Avviso
012717140206     c                   when      ilv82tipa='RIC'
012718140206     c                   eval      kmad='81'
012719140206     c                   endsl
012720140206     c     ktgd          chain     fitgd11l
012721140206     c                   if        %found(fitgd11l)
012722140206     c                   eval      olv82sts='1'
012723140206     c                   eval      olv82doia=%editc(tgddac:'X')
012724140207     c                             + %editc(tgdorc:'X')
012726140206     c                   endif
012727140206     c                   endsr
012728140206     C**************************************************************************
012729140206     C     sr_cerSPIA    BEGSR
012730140206     c     kfisia        chain     fisia01l
012731140207     c                   if        not %found(fisia01l) or siasts='A'
012732140207     c                             or siasts='E'
012733140207     c                   eval      olv82sts='9'
012734140207     c                   else
012735140207     c                   eval      olv82sts=siasts
012736140207     c                   if        olv82sts='1'
012737140207     c                   eval      olv82doia=%char(%dec(siadaore:*iso))
012738140207     c                   endif
012739140207     c                   eval      olv82dois=%char(%dec(siadaori:*iso))
012740140206     c                   endif
012741140206     c                   endsr
012800991206     C**************************************************************************
012900051123     C     *INZSR        BEGSR
013000051123     c
013100140206     c     ktgd          Klist
013200140206     c                   Kfld                    ktor
013300140206     c                   Kfld                    kogg
013400140206     c                   Kfld                    kmad
013401140206     c     kfisia        Klist
013402140206     c                   kfld                    ilv82aas
013403140206     c                   kfld                    ilv82lnp
013404140206     c                   kfld                    ilv82nrs
013405140206     c                   kfld                    ilv82nsp
013406140206     c                   kfld                    ilv82tipa
013407140206     c
015300070313     c                   endsr
