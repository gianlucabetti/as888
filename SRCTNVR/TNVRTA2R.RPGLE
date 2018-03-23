000100011003     H DECEDIT('0,') DATEDIT(*DMY.)
000200091118     FTITAS26c  uf   E           k DISK    extfile('GAITRAGRU/TITAS26C')
000201091117     ffnarb01l  if   e           k disk
000202091118     ffnlbl02l  if   e           k disk
000204091117     D KPJBA         E DS
000205091117     D TNSD99        E DS                  EXTNAME(TNSD99DS) INZ
000206091118     D fnarbds       E DS                  EXTNAME(fnarb00f)
000207091118     D fnlblds       E DS                  EXTNAME(fnlbl00f)
000208091117     d param           ds
000209091117     d  dtai                          8S 0
000210091117     d  dtaf                          8s 0
000212091118     D parb            s               *   inz(%addr(arbaas))
000213091118     d bollaarb        s             16  0 based(parb)
000214091118     D ptro            s               *   inz(%addr(lblaao))
000215091118     d originale       s             16  0 based(ptro)
000216091118     D ptrp            s               *   inz(%addr(lblaap))
000217091118     d precedente      s             16  0 based(ptrp)
000218091118     D ptrn            s               *   inz(%addr(lblaan))
000219091118     d seguente        s             16  0 based(ptrn)
000221091118     d savsegue        s             16  0 inz
000222091117     ITITAS000      01
000223091117     ITITAS010      02
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
000611091118     C                   KFLD                    arbaas
000612091118     C                   KFLD                    arblnp
000613091118     C                   KFLD                    arbnrs
000614091118     C                   KFLD                    arbnsp
000615091118     C     Ktas          KLIST
000616091118     C                   KFLD                    tasaas
000617091118     C                   KFLD                    taslnp
000618091118     C                   KFLD                    tasnrs
000619091118     C                   KFLD                    tasnsp
000620091118     C     Klbl          KLIST
000621091118     C                   KFLD                    LBLAAP
000622091118     C                   KFLD                    LBLLPP
000623091118     C                   KFLD                    LBLNRP
000624091118     C                   KFLD                    LBLNSP
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
001607091118     c* solo le legate
001608091118     c                   if        tascca <> '1' and
001609091118     c                             tascca <> '2' and
001610091118     c                             tascca <> '6'
001611091117     c                   iter
001612091117     c                   end
001613091117     c* solo se da calcolare
001614091118     c*                  if        tasNCI <> 0 or
001615091118     c*                            tasNPI <> 0 or
001616091118     c*                            tasDCE <> 0 or
001617091118     c*                            tasNRC <> 0
001618091118     c*                  iter
001619091118     c*                  end
001620091117     c* area di parma e filiale padova
001621091118     c                   if        (taslna = 6 and tasdcm <= 20091018) or
001622091118     c                             (tastfa = 20 and tasdcm <= 20091018) or
001623091118     c                             tasdcm >= 20091019
001700091118     C* verifico se la bolla è un originale
001701091118     c                   clear                   savsegue
001702091118     c     ktas          setll     fnlbl02l
001703091118     c                   do        *hival
001704091118     c     ktas          reade     fnlbl02l
001705091118     c                   if        %eof(fnlbl02l)
001706091118     c                   leave
001707091118     c                   end
001709091118     c                   if        originale = precedente and
001710091118     c                             lbllan <> lbllap
001711091118     c                   eval      savsegue = seguente
001712091118     c                   leave
001713091118     c                   end
001714091118     c                   enddo
001715091118     c* se bolla è originale mi metto in loop per cercare l'ultima figlia
001716091118     c* altrimenti leggo un altro record fdi titas
001717091118     c                   if        savsegue = 0
001718091118     c                   iter
001719091118     c                   end
001720091118     c*
001722091118     c                   eval      precedente = savsegue
001723091118     c     klbl          setll     fnlbl02l
001724091118     c                   do        *hival
001725091118     c     klbl          reade     fnlbl02l
001726091118     c                   if        %eof(fnlbl02l)
001727091118     c                   leave
001728091118     c                   end
001729091118     c                   if        lbllan <> lbllap
001730091118     c                   eval      savsegue = seguente
001731091118     c                   eval      precedente = seguente
001732091118     c     klbl          setll     fnlbl02l
001733091118     c                   iter
001734091118     c                   end
001735091118     c                   enddo
001738091118     c* calcolo delivery
001739091118     c                   eval      bollaarb = savsegue
001740091117     c     karb          chain     fnarb01l
001741091117     c                   if        %found(fnarb01l)
001742091117     c                   eval      tasNCI = ARBNCI
001743091117     c                   eval      tasNPI = ARBNPI
001744091117     c                   eval      tasDCE = ARBDCE
001745091117     c                   eval      tasNRC = ARBNRC
001746091117     c                   else
001747091117     c* non trovo FNARB allora delivery dalla partenza
001748091117     c* devo agganciare FNBLP ???????????????????????
001749091117     c                   clear                   tnsd99
001750091117     C                   MOVEL     ' '           D98TLA
001751091117     C                   MOVEL     'P'           D98TBO
001752091118     C                   MOVE      arbAAS        D98AAS
001753091118     C                   MOVE      arbLNP        D98LNP
001754091118     C                   MOVE      arbNRS        D98NRS
001755091118     C                   MOVE      arbNSP        D98NSP
001756091117     C                   CALL      'TNSD99R'
001757091117     C                   PARM                    TNSD99
001758091117     C                   Z-ADD     D98NCI        tasNCI                         NON CONF.CONS. INT.
001759091117     C                   Z-ADD     D98NPI        tasNPI                         NON CONF.PART. INT.
001760091117     C                   Z-ADD     D98DCE        tasDCE                         DATA CONS. PREV.CLI.
001761091117     C                   Z-ADD     D98NRC        tasNRC                         NON CONF. RESA CLI.
001762091117     c                   end
001763091117     c*
001764091117     c                   if        tasNCI <> 0 or
001765091117     c                             tasNPI <> 0 or
001766091118     c*                            tasDCE <> 0 or
001767091117     c                             tasNRC <> 0
001768091117     c                   eval      tasll2 = 'd'
001769091117     c                   if        *in01
001770091117     C                   EXCEPT    AGGT0
001771091117     c                   end
001772091117     c                   if        *in02
001773091117     C                   EXCEPT    AGGT10
001774091117     c                   end
001775091117     c                   end
001776091118     c                   end
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
