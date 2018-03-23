000100011003     H DECEDIT('0,') DATEDIT(*DMY.)
000200091118     FTITAS26c  uf   E           k DISK    extfile('GAITRAGRU/TITAS26C')
000201091117     ffnarb01l  if   e           k disk
000204091117     D KPJBA         E DS
000205091117     D TNSD99        E DS                  EXTNAME(TNSD99DS) INZ
000206091117     d param           ds
000207091117     d  dtai                          8S 0
000208091117     d  dtaf                          8s 0
000209091117     ITITAS000      01
000210091117     ITITAS010      02
000600011003     C*------------------------------------------------------------------------*
000601091117     c     *entry        plist
000602091117     c                   parm                    kpjba
000603091117     c                   movel(p)  kpjbu         param
000604091118     c                   if        param = ' ' or
000606091118     c                             dtai = 0
000607091117     c                   seton                                        lr
000608091117     c                   return
000609091117     c                   end
000610091117     C     KARB          KLIST
000611091117     C                   KFLD                    tasaas
000612091117     C                   KFLD                    taslnp
000613091117     C                   KFLD                    tasnrs
000614091117     C                   KFLD                    tasnsp
000700011003     C* MAIN LINE
000800011003     C*------------------------------------------------------------------------*
001000011003     C*
001100091117     C     dtai          SETLL     TITAS26c
001200011003     C                   DO        *HIVAL
001201091117     c                   setoff                                       0102
001300091117     C                   READ      TITAS26c
001301091117     c                   if        %eof(titas26c)
001400091117     C                   LEAVE
001401091117     c                   end
001500011003     C*
001600091117     C                   IF        tasdcm > dtaf
001601091117     c                   leave
001602091117     c                   end
001603091117     C*
001604091117     C                   IF        TASdcm = 0
001605091117     c                   iter
001606091117     c                   end
001607091117     c*
001608091117     c                   if        tascca =  '1' or
001609091118     c                             tascca =  '2' or
001610091118     c                             tascca =  '6'
001611091117     c                   iter
001612091117     c                   end
001613091117     c* solo se da calcolare
001614091117     c                   if        tasNCI <> 0 or
001615091117     c                             tasNPI <> 0 or
001616091117     c                             tasDCE <> 0 or
001617091117     c                             tasNRC <> 0
001618091117     c                   iter
001619091117     c                   end
001620091117     c* area di parma e filiale padova
001621091118     c                   if        (taslna = 6 and tasdcm <= 20091018) or
001622091118     c                             (tastfa = 20 and tasdcm <= 20091018) or
001623091118     c                             tasdcm >= 20091019
001700011003     C*
001701091117     c     karb          chain     fnarb01l
001702091117     c                   if        %found(fnarb01l)
001703091117     c                   eval      tasNCI = ARBNCI
001704091117     c                   eval      tasNPI = ARBNPI
001705091117     c                   eval      tasDCE = ARBDCE
001706091117     c                   eval      tasNRC = ARBNRC
001707091117     c                   else
001708091117     c* non trovo FNARB allora delivery dalla partenza
001709091117     c* devo agganciare FNBLP ???????????????????????
001710091117     c                   clear                   tnsd99
001711091117     C                   MOVEL     ' '           D98TLA
001712091117     C                   MOVEL     'P'           D98TBO
001713091117     C                   MOVE      tasAAS        D98AAS
001714091117     C                   MOVE      tasLNP        D98LNP
001715091117     C                   MOVE      tasNRS        D98NRS
001716091117     C                   MOVE      tasNSP        D98NSP
001717091117     C                   CALL      'TNSD99R'
001718091117     C                   PARM                    TNSD99
001719091117     C                   Z-ADD     D98NCI        tasNCI                         NON CONF.CONS. INT.
001720091117     C                   Z-ADD     D98NPI        tasNPI                         NON CONF.PART. INT.
001721091117     C                   Z-ADD     D98DCE        tasDCE                         DATA CONS. PREV.CLI.
001722091117     C                   Z-ADD     D98NRC        tasNRC                         NON CONF. RESA CLI.
001723091117     c                   end
001724091117     c*
001725091117     c                   if        tasNCI <> 0 or
001726091117     c                             tasNPI <> 0 or
001727091118     c*                            tasDCE <> 0 or
001728091117     c                             tasNRC <> 0
001729091117     c                   eval      tasll2 = 'd'
001730091117     c                   if        *in01
001731091117     C                   EXCEPT    AGGT0
001732091117     c                   end
001733091117     c                   if        *in02
001734091117     C                   EXCEPT    AGGT10
001735091117     c                   end
001736091117     c                   end
001737091118     c                   end
002300011003     C*
002400011003     C                   ENDDO
002600011003     C                   SETON                                        LR
002700091117     Otitas000  E            AGGT0
002800091117     O                       tasNCI
002900091117     O                       tasNPI
003000091117     O                       tasDCE
003100091117     O                       tasNRC
003101091117     O                       tasll2
003200091117     Otitas010  E            AGGT10
003300091117     O                       tasNCI
003400091117     O                       tasNPI
003500091117     O                       tasDCE
003600091117     O                       tasNRC
003700091117     O                       tasll2
