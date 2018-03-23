000200000000     H DECEDIT('0,') DATEDIT(*DMY.)
000300011127     H*--------------------------------------------------------------*
000400051121     H*             - Anagrafico attività  -                         *
000500000000     H*--------------------------------------------------------------*
000700051202     Ftntbe01l  uF a E           k disk
000900051202     Ftntbtutd  CF   E             WORKSTN
001000051202     Fkfazn11l  iF   E           k DISK
001100030116
001101051129     D Eseguito        C                   '0'
001502051129     D dtut          E DS
001503051207     d comke1          s                   like(prmke1)
001600011127      *
001700020103     D Psds           SDS
001800020103     D  PgmName          *PROC
001900011228     D KPJBA         E DS
007200030116
007300000000     C                   EXSR      AZZER
007301051202     c                   exsr      srcarica
007700011127     C*
007800051202     C                   DO        *HIVAL
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
010100051202     C                   leave
010300011228     c                   end
011800011228     C* controlli
011900051121     C                   EXSR      CTR01
013300030117
013400011127     C   99              ITER
013500030117
013600011228     C* AGGIORNA
013700011228     C                   IF        *INKF
013800011228     C                   EXSR      SRAGGIO
013801051207     C                   IF        *IN41
013802051207     C                   SETON                                        99
013803051207     c                   iter
013804051207     c                   end
014400011228     C                   LEAVE
014500011228     c                   end
014600011228     c                   enddo
014700011127     c*
014800000000     C                   SETON                                        LR
014900011127     c**********************************************************************
015000011228     c* carica 1° formato
015100011127     c**********************************************************************
015200011228     C     srcarica      BEGSR
015500051202     C     keytbe        chain     tntbe01l
015600051202     C                   if        %found(tntbe01l)
015601051202     c                   movel     tbeuni        dtut
017700051202     c                   movel     tbeatb        v01atb
017701051202     c                   movel     tbeke1        v01ke1
017800051202     c                   move      §tutdes       v01des
017900051202     c                   movel     §tutper       v01per
017901051202     c                   movel     §tutazn       v01azn
017902051202     c                   if        v01azn <> ' '
017903051202     c                   exsr      srazn
017904051202     c                   end
018000051202     c                   movel     §tutsif       v01sif
018001051202     c                   movel     §tutfela      v01fela
018100051202     c                   movel     §tutpul       v01pul
018101051212     c                   movel     §tutril       v01ril
018102051212     c                   z-add     §tutqta       v01qta
019800011228     c                   exsr      ctr01
019900030116     C                   ENDIF
020100011228     C                   ENDSR
021000011127     C******************************************************
021100011127     C* CONTROLLI PRIMO VIDEO
021200011127     C******************************************************
021300011127     C     CTR01         BEGSR
021400011228     c                   setoff                                       99
021401051202     c                   if        v01atb = ' '
021600051202     c* codice tabella
021800051202     c     v01ke1        ifeq      *blanks
021900051121     c                   seton                                        0199
021901051207     c                   else
021902051207     c* inserimento
021903051207     c                   if        prmke1 = ' '
021904051207     c                   movel(p)  v01ke1        comke1
021905051207     c     ktbe          setll     tntbe01l                               41
021908051207     c                   end
023500030114     c                   end
023600051121     c* descrizione attività
023700051121     c     v01des        ifeq      *blanks
023800051121     c                   seton                                        0299
023900011228     c                   end
023901051202     c* codice azione
023902051202     c     v01azn        ifne      *blanks
023903051202     c                   exsr      srazn
023911051121     c                   end
023912051202     c                   end
023913051202     c* sistema informativo
023914051202     c     v01sif        comp      *blanks                                04
023915051202     c* frequenza elaborazione
023916051202     c     v01fela       comp      *blanks                                05
023917051202     c* pulizia archivi
023918051202     c     v01pul        comp      *blanks                                06
023919051212     c* rilevabile da pgm = no obbligatorio qta
023920051212     c                   if        v01ril = 'N' and v01qta = 0
023921051212     c                   seton                                        0899
023922051212     c                   end
023923051202     c* personalizzazioni
023924051202     c     v01per        comp      *blanks                                07
023925051207     c                   if        *in04 or *in05 or *in06 or *in07 or *in41
023926051202     c                   seton                                        99
023927051202     c                   end
039500030116
039600011127     C                   ENDSR
048500011228     c**********************************************************************
048600011228     c* AGGIORNA ARCHIVIO
048700011228     c**********************************************************************
048800011228     C     srAGGIO       BEGSR
048802051207     c                   movel(p)  v01ke1        comke1
048803051207     c     ktbe          chain     tntbe01l
048804051207     c                   clear                   dtut
049400051202     c                   movel     v01ke1        tbeke1
049500051202     c                   move      v01atb        tbeatb
049600051202     c                   movel     v01des        §tutdes
049601051202     c                   movel     v01azn        §tutazn
049602051202     c                   movel     v01sif        §tutsif
049700051202     c                   movel     v01fela       §tutfela
049701051202     c                   movel     v01pul        §tutpul
049702051202     c                   movel     v01per        §tutper
049703051212     c                   movel     v01ril        §tutril
049704051212     c                   z-add     v01qta        §tutqta
049705051202     c                   movel(p)  dtut          tbeuni
049706051202     c                   movel     'TUT'         tbecod
049707051202     c                   movel     'TU'          tbeapl
049708051202     c                   if        %found(tntbe01l)
052600051202     c                   update    tntbe000
052700020214     c                   else
053100051202     c                   write     tntbe000
053200020214     c                   end
053400011228     C                   ENDSR
053500011127     C******************************************************
053600000000     C** PULIZIA CAMPI DS
053700011127     C******************************************************
053800011127     C     AZZER         BEGSR
053900051202     C                   MOVEL     *BLANKS       v01atb
054300051202     C                   MOVEL     *blanks       v01ke1
054301051202     C                   MOVEL     *blanks       v01dazn
054400051202     C                   MOVEL     *blanks       v01azn
054401051202     C                   MOVEL     'G'           v01fela
054402051202     C                   MOVEL     'F'           v01sif
054403051202     C                   MOVEL     'P'           v01per
054404051202     C                   MOVEL     '2'           v01pul
054405051212     C                   MOVEL     'S'           v01ril
054406051212     c                   clear                   v01qta
055600000000     C                   ENDSR
055700011127     C******************************************************
055800011127     C     *INZSR        BEGSR
055900011127     C******************************************************
056000011127     C*
056100011127     C     *ENTRY        PLIST
056200051121     C                   PARM                    KPJBA
056201051202     C                   PARM                    prmcod            3
056202051202     C                   PARM                    prmke1           15
056600051202     C     Keytbe        KLIST
056700051202     C                   KFLD                    prmcod
056800051202     C                   KFLD                    prmke1
056801051207     C     Ktbe          KLIST
056802051207     C                   KFLD                    prmcod
056803051207     C                   KFLD                    comke1
059500011127     C                   ENDSR
061100030123
061200051121     C**************************************************************************
061300051202     C* azione
061400051121     C**************************************************************************
061500051202     C     srazn         BEGSR
061800051202     C     v01azn        chain     kfazn11l
061900051202     C                   if        %found(kfazn11l)
061901051202     c                   movel     kdsaz         v01dazn
061902051202     c                   else
061903051202     c                   seton                                        0399
061904051202     c                   clear                   v01dazn
062400051121     c                   end
062500051121     C                   ENDSR
