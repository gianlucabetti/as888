000200000000     H DECEDIT('0,') DATEDIT(*DMY.)
000300011127     H*--------------------------------------------------------------*
000400051128     H*             - lancio dettaglio attività -                    *
000500000000     H*--------------------------------------------------------------*
000900051128     Ftntu05d   CF   E             WORKSTN
001100051129     ftntbe01l  if   e           k disk
001400030116
001401051129     d dtut          e ds
001503051128     D tntu05ds      E DS
001504051129     D tntb38ds      E DS
001505051129     D tibs55ds      E DS
001700020103     D Psds           SDS
001800020103     D  PgmName          *PROC
001801051130     D savsifil        S                   like(O50ASI)
001802051130     D savsised        S                   like(O50ASI)
001900011228     D KPJBA         E DS
007200030116
007300000000     C                   EXSR      AZZER
007700011127     C*
007800011228     C  nlr              DO        *HIVAL
008300030117
008400011228     c                   if        *in99
008500011228     c                   setoff                                       99
008600051121     c                   write     v01
008700011228     c                   seton                                        99
008800011228     c                   end
008900030117
009600051121     C                   EXFMT     v01
009800000000     C                   SETOFF                                       99
009900011127     C* FINE LAVORO
010000011228     C                   IF        *INKC
010200011228     C                   LEAVE
010300011228     c                   end
010301051129     C* ricerca
010305051129     C   KD              EXSR      RIC01
011800011228     C* controlli
011900051121     C                   EXSR      CTR01
013300030117
013400011127     C   99              ITER
013500030117
013600011228     C* AGGIORNA
013700011228     C                   IF        *INKF
013800011228     C                   EXSR      SRAGGIO
014400011228     C                   LEAVE
014500011228     c                   end
014600011228     c                   enddo
014700011127     c*
014800000000     C                   SETON                                        LR
014801051129     C******************************************************
014802051129     C* RICERCA PRIMO VIDEO
014803051129     C******************************************************
014804051129     C     RIC01         BEGSR
014805051129     C                   clear                   Tntb38DS
014806051129     C                   eval      t38tla = 'L'
014807051129     C                   movel     'R'           t38mod
014808051129     C                   movel     *blanks       t38sif
014809051129     c                   select
014836051129     c* moltiplicatore
014837051129     C                   when      rtncsrfld = 'V01MULT'
014838051129     C                   movel     'TUT'         t38cod
014839051129     C                   CALL      'TNTB38R'
014840051129     C                   parm                    KPJBA
014841051129     C                   parm                    TNTB38DS
014842051129    2C                   IF        t38err = *blanks
014843051129     C                   movel     t38ke1        v01mult
014844051129     C                   movel     t38uni        dtut
014845051129     C                   movel     §tutdes       v01dmult
014846051129     C                   else
014847051129     C                   seton                                        0299
014848051129     C                   ENDIF
014849051129     C                   ENDsl
014851051129     C                   ENDSR
021000011127     C******************************************************
021100011127     C* CONTROLLI PRIMO VIDEO
021200011127     C******************************************************
021300011127     C     CTR01         BEGSR
021400011228     c                   setoff                                       99
021401051129     C* CONTROLLO DATA elaborazione
021403051129     C                   IF        v01dti <> 0
021404051129     C     *dmy          test(d)                 v01dti                 01
021405051129     c                   if        *in01
021406051129     C                   SETON                                        99
021410051129     C                   END
021413051129     C                   END
023501051129     c* moltiplicatore
023502051129     c     v01mult       ifne      *blanks
023503051129     c                   movel     v01mult       tbeke1
023504051129     c     keytbe        chain     tntbe01l
023505051129     c                   if        %found(tntbe01l) and tbeatb = ' '
023506051129     c                   movel     tbeuni        dtut
023507051129     c                   movel     §tutdes       v01dmult
023508051129     c                   if        §tutazn = ' '
023510051129     c                   seton                                        0299
023511051129     c                   end
023513051129     c                   else
023514051129     c                   seton                                        0299
023515051129     c                   end
023516051129     c                   else
023517051129     c                   clear                   v01dmult
023518051129     c                   end
039500030116
039600011127     C                   ENDSR
048500011228     c**********************************************************************
048600011228     c* AGGIORNA ARCHIVIO
048700011228     c**********************************************************************
048800011228     C     srAGGIO       BEGSR
050500051121     c                   if        v01dti <> 0
050600051129     c     *dmy          movel     v01dti        t05dti
050700020214     c                   else
050800051130     c                   movel     *loval        t05dti
050900020214     c                   end
050901051130     c* singolo pgm
050902051129     c                   if        v01mult<> ' '
050910051129     c                   movel     v01mult       t05mult
050911051130     c                   exsr      srkpjba
050914051129     c                   else
050915051129     c* lancio globale
050916051129     c     'TUT'         setll     tntbe01l
050917051129     c                   do        *hival
050918051129     c     'TUT'         reade     tntbe01l
050919051129     c                   if        %eof(tntbe01l)
050920051129     c                   leave
050921051129     c                   end
050922051129     c                   if        tbeatb = ' '
050923051129     c                   movel     tbeuni        dtut
050924051129     c                   if        §tutazn <> ' '
050925051130     c                   movel     tbeke1        t05mult
050926051130     c                   exsr      srkpjba
050931051129     c                   end
050932051129     c                   end
050933051129     c                   enddo
050934051129     c                   end
053400011228     C                   ENDSR
053500011127     C******************************************************
053600000000     C** PULIZIA CAMPI DS
053700011127     C******************************************************
053800011127     C     AZZER         BEGSR
053901051129     C                   MOVEL     *BLANKS       v01mult
055400051130     C                   z-add     0             v01dti
055600000000     C                   ENDSR
055601051130     C******************************************************
055602051130     C** imposta kpjba
055603051130     C******************************************************
055604051130     C     srkpjba       BEGSR
055607051130     c                   if        §tutsif ='F'
055608051130     c                   eval      knsif = savsifil
055609051130     c                   else
055610051130     c                   eval      knsif = savsised
055611051130     c                   end
055613051130     c                   movel     §tutazn       kcoaz
055614051130     c                   movel     tntu05ds      kpjbu
055615051130     c                   call      'BCH10'
055616051130     c                   parm                    kpjba
055617051130     C                   ENDSR
055700011127     C******************************************************
055800011127     C     *INZSR        BEGSR
055900011127     C******************************************************
056000011127     C*
056100011127     C     *ENTRY        PLIST
056200051121     C                   PARM                    KPJBA
056201051129     C     Keytbe        KLIST
056202051129     C                   KFLD                    tbecod
056203051129     C                   KFLD                    tbeke1
056204051129     c                   eval      tbecod='TUT'
056205051129     c* reperisco s.i. sede e filiale
056206051130     c                   clear                   tibs55ds
056207051129     c                   eval      I50tla='L'
056208051129     c                   eval      I50PPO=001
056209051129     c                   eval      I50aPO=001
056210051129     c                   call      'TIBS55R'
056211051130     c                   parm                    tibs55ds
056212051130     c                   movel     O50ASI        savsifil
056213051130     c                   clear                   tibs55ds
056214051130     c                   eval      I50tla='L'
056215051130     c                   eval      I50PPO=046
056216051130     c                   eval      I50aPO=046
056217051130     c                   call      'TIBS55R'
056218051130     c                   parm                    tibs55ds
056219051130     c                   movel     O50ASI        savsised
059500011127     C                   ENDSR
