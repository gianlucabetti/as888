000200000000     H DECEDIT('0,') DATEDIT(*DMY.)
000300011127     H*--------------------------------------------------------------*
000400051121     H*             - Anagrafico attività  -                         *
000500000000     H*--------------------------------------------------------------*
000700051121     Ftntbe01l  iF   E           k disk
000900051121     Ftntu02d   CF   E             WORKSTN
001000051121     Ftntua00f  UF a E             DISK
001001051121     Ftntua01l  if   E           k DISK    rename(tntua000:tntua01)
001100030116
001101051129     D Eseguito        C                   '0'
001500051121     D dtuc          E DS
001501051121     D dtur          E DS
001502051129     D dtut          E DS
001503051121     D tntb38ds      E DS
001504051122     D tntu02ds      E DS
001505051125     D tntu03ds      E DS
001506051201     D tntuperds     E DS
001600011127      *
001700020103     D Psds           SDS
001800020103     D  PgmName          *PROC
001900011228     D KPJBA         E DS
004600011127     D DTAiso          S               D   INZ
004700011127     D DTAEUR          S               D   DATFMT(*EUR) INZ
004800011127     D DTADAL          S               D   INZ
004900011127     D DTAAL           S               D   INZ
005000011228     d data            ds            10
005100011228     d aa                      3      4
005200011228     d mm                      6      7
005300011228     d gg                      9     10
005400011228     D com6            S              6
005500030122     D Mod             S
005600051122     D                                     LIKE(t02Mod)
006500030120
006600030120     D Refresh         S              1
006700030120     D Esito           S              1
006800030120     D GesMsg          S              1
006900030120     D Output          S           4000
007000030120     D LenOut          S              9B 0
007100030120     D StrutturaO      S             10
007200030116
007300000000     C                   EXSR      AZZER
007400051122     c                   if        t02nrr <> 0
007500011228     c                   exsr      srcarica
007600011228     c                   end
007700011127     C*
007800011228     C  nlr              DO        *HIVAL
008300030117
008400011228     c                   if        *in99
008500011228     c                   setoff                                       99
008600051121     c                   write     v01
008700011228     c                   seton                                        99
008800011228     c                   end
008900030117
009000011228     C                   IF        mod = '4 '
009100011228     C                             OR
009200011228     C                             mod = '5 '
009300051121     C                   WRITE     v01
009400051121     C                   EXFMT     v01Protect
009500011228     C                   ELSE
009600051121     C                   EXFMT     v01
009700011228     C                   ENDIF
009800000000     C                   SETOFF                                       99
009900011127     C* FINE LAVORO
010000011228     C                   IF        *INKC
010100051122     C                   MOVE      'C'           t02CMD
010200011228     C                   LEAVE
010300011228     c                   end
010400011228     C* GUIDA
010500011228     C                   IF        *INKL
010600051122     C                   MOVE      'L'           t02CMD
010700011228     C                   LEAVE
010800011228     c                   end
010900020128     C* ripristino
011000020128     C                   IF        *INKe
011100020128     C                   exsr      srcarica
011200020128     C                   iter
011300020128     c                   end
011400030123     C*
011600011127     C* RICERCA
011700011127     C   KD              EXSR      RIC01
011800011228     C* controlli
011900051121     C                   EXSR      CTR01
013300030117
013400011127     C   99              ITER
013500030117
013600011228     C* AGGIORNA
013700011228     C                   IF        *INKF
013800011228     C                   EXSR      SRAGGIO
013900011228     C                   IF        *IN41
014000011228     C                   SETON                                        99
014100011228     c                   iter
014200011228     c                   end
014300051122     C                   MOVE      'F'           t02CMD
014400011228     C                   LEAVE
014500011228     c                   end
014600011228     c                   enddo
014700011127     c*
014701051124     c                   movel     tntu02ds      kpjbu
014800000000     C                   SETON                                        LR
014900011127     c**********************************************************************
015000011228     c* carica 1° formato
015100011127     c**********************************************************************
015200011228     C     srcarica      BEGSR
015300020218     c                   clear                   msgv
015400051122     C                   EVAL      Mod = t02Mod
015500051122     C     t02nrr        chain     tntua00f
015600051121     C                   if        %found(tntua00f)
015601051121     c* messaggio d'errore
015700051121     c                   if        (mod = '2 ' and tuaatb <> *blanks)
016500020218     c                   eval      msgv = 'Per poter modificare il record occor-
016600020218     c                             re prima ripristinarlo'
016800020218     c* forzo la visualizzazione
016900030122     c                   eval      modv = 'VISUALIZZAZIONE'
017000020214     c                   eval      mod = '5 '
017100020218     c                   seton                                        40
017101051122     c                   end
017300051125     c                   if        mod = '4 ' and
017302051125     c                             tuaatb <> *blanks
017400030122     c                   eval      modv = '   RIPRISTINO  '
017500030122     c                   eval      mod = '2 '
017603051125     c                   end
017604051122     c     mod           comp      '2 '                                   42
017700051121     c                   movel     tuaatt        v01att
017800051121     c                   move      tuaprg        v01prg
017900051121     c                   movel     tuades        v01des
018000051121     c                   movel     tuachi        v01chi
018001051129     c                   movel     tuamult       v01mult
018100051121     c                   movel     tuarep        v01rep
018500051121     c                   z-add     0             v01dti
018600051121     c                   if        tuadti <> *loval
018700051121     c                   move      tuadti        data
018800011228     c                   eval      com6 = gg + mm + aa
018900051121     c                   move      com6          v01dti
019000011228     c                   end
019100051121     c                   z-add     999999        v01dtf
019200051121     c                   if        tuadtf <> *hival
019300051121     c                   move      tuadtf        data
019400011228     c                   eval      com6 = gg + mm + aa
019500051121     c                   move      com6          v01dtf
019600011228     c                   end
019700030116     C                   IF        MsgV = *BLANK
019800011228     c                   exsr      ctr01
019900030116     C                   ENDIF
020000011228     C                   end
020100011228     C                   ENDSR
021000011127     C******************************************************
021100011127     C* CONTROLLI PRIMO VIDEO
021200011127     C******************************************************
021300011127     C     CTR01         BEGSR
021400011228     c                   setoff                                       99
021500020214     c                   if        mod <> '4 '
021600051121     c* codice attività obbligatorio
021800051121     c     v01att        ifeq      *blanks
021900051121     c                   seton                                        0199
023500030114     c                   end
023600051121     c* descrizione attività
023700051121     c     v01des        ifeq      *blanks
023800051121     c                   seton                                        0299
023900011228     c                   end
023901051121     c* po che esegue l'attività
023902051121     c     v01chi        ifeq      *blanks
023903051121     c                   seton                                        0399
023904051121     c                   else
023905051121     C                   movel(P)  v01chi        tbeke1
023906051121     c                   movel     'TUC'         tbecod
023907051121     c                   exsr      srtab
023908051129     c   10              seton                                        0399
023909051121     c                   movel     tbeuni        dtuc
023910051121     c                   eval      v01dchi = §tucdes
023911051121     c                   end
023912051121     c* reparto
023913051121     c     v01rep        ifeq      *blanks
023914051121     c                   seton                                        0499
023915051121     c                   else
023916051121     C                   movel(P)  v01rep        tbeke1
023917051121     c                   movel     'TUR'         tbecod
023918051121     c                   exsr      srtab
023919051129     c   10              seton                                        0499
023920051121     c                   movel     tbeuni        dtur
023921051121     c                   eval      v01drep = §turdes
023922051121     c                   end
023923051129     c* moltiplicatore
023924051129     c     v01mult       ifeq      *blanks
023925051129     c                   seton                                        0799
023926051129     c                   else
023927051129     C                   movel(P)  v01mult       tbeke1
023928051129     c                   movel     'TUT'         tbecod
023929051129     c                   exsr      srtab
023930051129     c   10              seton                                        0799
023931051129     c                   movel     tbeuni        dtut
023932051129     c                   eval      v01dmult= §tutdes
023933051129     c                   end
033400011127     C* CONTROLLO DATE
033500011127     C                   MOVEL     *LOVAL        DTADAL
033600051121     C                   IF        v01dti <> 0
033700051121     C     *dmy          test(d)                 v01dti                 05
033800051121     c                   if        *in05
033900020103     C                   SETON                                        99
034000020103     c                   else
034100051121     C     *dmy          MOVE      v01dti        DTAEUR
034200011127     C                   MOVEL     DTAEUR        DTADAL
034300020103     C                   END
034400011127     C                   END
034500011127     C* CONTROLLO AL
034600011228     C                   MOVEL     *hival        DTAAL
034700051121     C                   IF        v01dtf <> 999999
034800051121     C     *dmy          test(d)                 v01dtf                 06
034900051121     c                   if        *in06
035000011127     C                   SETON                                        99
035100020103     c                   else
035200051122     C     *dmy          MOVE      v01dtf        DTAEUR
035300011127     C                   MOVEL     DTAEUR        DTAAL
035400020103     C                   END
035500011127     C                   END
035600011127     C*
035700051121     C                   IF        (v01dti <> 0 OR v01dtf <> 999999) AND
035800011127     C                              DTADAL > DTAAL
035900051121     C                   SETON                                        050699
036000011127     C                   END
036100011228     C                   END
036200030116
036301060118     c* progressivo
036400060118     C                   IF        NOT *IN99  and
036600030116     C                             (Mod = '1' OR Mod = '3')
036602051122     c     keytua        chain     tntua01l
036603051122     c                   if        %found(tntua01l)
036604051122     c                   seton                                        9941
036605051121     c                   end
036606060118     c                   end
036607060118     ** Controllo sovrapposizione periodo validità e progressivo
036700030116
036701060118     C                   IF        NOT *IN99
036800051201     C                   CLEAR                   tntuperds
036801051201     c                   eval      §perfile = 'TNTUA00F'
036802051201     c                   eval      §peratt = v01att
036803060118     c*                  eval      §perprg = v01prg
036804060118     c                   eval      §perdti = dtadal
036805060118     c                   eval      §perdtf = dtaal
036806060118     c                   if        mod <> '1 ' and mod <> '3 '
036807060118     c                   eval      §pernrr = t02nrr
036808060118     c                   end
036900051121     C                   CALL      'TNTUPERR'
037000051201     C                   PARM                    tntuperds
037500030116
037600051201     C                   IF        §Peresi = Eseguito
037700030116     C                   EVAL      *IN99 = *ON
037800051121     C                   EVAL      *IN05 = *ON
037900030116     C                   ENDIF
038000030116
038001051201     C                   CLEAR                   tntuperds
038002051201     c                   eval      §perfile = 'TNTUA00F'
038003051201     c                   eval      §peratt = v01att
038004060118     c*                  eval      §perprg = v01prg
038005060118     c                   eval      §perdti = dtadal
038006060118     c                   eval      §perdtf = dtaal
038007060118     c                   if        mod <> '1 ' and mod <> '3 '
038008060118     c                   eval      §pernrr = t02nrr
038009060118     c                   end
038010051201     C                   CALL      'TNTUPERR'
038011051201     C                   PARM                    tntuperds
038800030116
038900051201     C                   IF        §Peresi = Eseguito
039000030116     C                   EVAL      *IN99 = *ON
039100051121     C                   EVAL      *IN06 = *ON
039200030116     C                   ENDIF
039300030116
039400030116     C                   ENDIF
039500030116
039600011127     C                   ENDSR
039700020107     C******************************************************
039800020107     C* RICERCA PRIMO VIDEO
039900020107     C******************************************************
040000020107     C     RIC01         BEGSR
040001051121     C                   clear                   Tntb38DS
040002051121     C                   eval      t38tla = 'L'
040003051121     C                   movel     'R'           t38mod
040004051121     C                   movel     *blanks       t38sif
040100011130     c                   select
040200051121     c* po che esegue l'attività
040301051122     C                   when      rtncsrfld = 'V01CHI'
040305051121     C                   movel     'TUC'         t38cod
040307051121     C                   CALL      'TNTB38R'
040308051121     C                   parm                    KPJBA
040309051121     C                   parm                    TNTB38DS
040310051121    2C                   IF        t38err = *blanks
040311051121     C                   movel     t38ke1        v01chi
040312051121     C                   movel     t38uni        dtuc
040313051121     C                   movel     §tucdes       v01dchi
040314051121     C                   else
040315051121     C                   seton                                        0399
040316051121     C                   ENDIF
041700051121     c* reparto
041701051122     C                   when      rtncsrfld = 'V01REP'
041804051121     C                   movel     'TUR'         t38cod
041806051121     C                   CALL      'TNTB38R'
041807051121     C                   parm                    KPJBA
041808051121     C                   parm                    TNTB38DS
041809051121    2C                   IF        t38err = *blanks
041810051121     C                   movel     t38ke1        v01rep
041811051121     C                   movel     t38uni        dtur
041812051121     C                   movel     §turdes       v01drep
041813051121     C                   else
041814051121     C                   seton                                        0499
041815051121     C                   ENDIF
041816051129     c* moltiplicatore
041817051129     C                   when      rtncsrfld = 'V01MULT'
041818051129     C                   movel     'TUT'         t38cod
041819051129     C                   CALL      'TNTB38R'
041820051129     C                   parm                    KPJBA
041821051129     C                   parm                    TNTB38DS
041822051129    2C                   IF        t38err = *blanks
041823051129     C                   movel     t38ke1        v01mult
041824051129     C                   movel     t38uni        dtut
041825051129     C                   movel     §tutdes       v01dmult
041826051129     C                   else
041827051129     C                   seton                                        0799
041828051129     C                   ENDIF
047900011130     C                   ENDsl
048000020214     c                   exsr      ctr01
048400011127     C                   ENDSR
048500011228     c**********************************************************************
048600011228     c* AGGIORNA ARCHIVIO
048700011228     c**********************************************************************
048800011228     C     srAGGIO       BEGSR
048900011228     C                   SETOFF                                       41
049000011228     c                   seton                                        90
049100011228     c                   if        mod <> '3 '
049200051122     C     t02nrr        chain     tntua00f                           90
049300011228     c                   end
049400051121     c                   movel     v01att        tuaatt
049500051121     c                   move      v01prg        tuaprg
049600051121     c                   movel     v01chi        tuachi
049601051129     c                   movel     v01mult       tuamult
049602051122     c                   movel     v01des        tuades
049700051121     c                   movel     v01rep        tuarep
050400020214     C* Date validità
050500051121     c                   if        v01dti <> 0
050600051121     c     *dmy          movel     v01dti        tuadti
050700020214     c                   else
050800051121     c                   movel     '0001-01-01'  tuadti
050900020214     c                   end
051000051121     c                   if        v01dtf <> 999999
051100051121     c     *dmy          movel     v01dtf        tuadtf
051200020214     c                   else
051300051121     c                   movel     '9999-12-31'  tuadtf
051400020214     c                   end
051411051125     c*
051500020214     c                   if        mod <> '4 '
051600051121     c                   movel     *blanks       tuaatb
051601051122     c                   else
051602051125     c* annullamento
051603051122     c                   movel     'A'           tuaatb
051604051125     c                   end
052200020214     c* aggiorno
052300020214     c                   if        not *in90
052301051125     c* annullamento/ripristino tempi
052302051125     c                   if        t02mod = '4'
052303051125     c                   exsr      sranrip
052312051125     c                   end
052313051125     c*
052400051122     c                   time                    tuaupd
052600051121     c                   update    tntua000                             41
052700020214     c                   else
052800020214     c* scrivo
052900051122     c                   time                    tuawrt
053100051121     c                   write     tntua000                             41
053200020214     c                   end
053400011228     C                   ENDSR
053401051125     C******************************************************
053402051125     C** window per annulla/ripristina tempi
053403051125     C******************************************************
053404051125     C     sranrip       BEGSR
053405051125     c                   do        *hival
053406051125     c                   exfmt     w02
053407051125     c                   if        *inkf
053408051125     c                   if        w02ant = 'S'
053409051125     c                   clear                   tntu03ds
053410051125     c                   movel     v01att        t03att
053411051125     c                   z-add     v01prg        t03prg
053412051125     c                   movel     tntu03ds      kpjbu
053413051125     c                   call      'TNTUANTR'
053414051125     c                   parm                    kpjba
053416051125     c                   end
053417051125     c                   leave
053418051125     c                   end
053419051125     c                   enddo
053420051125     C                   ENDSR
053500011127     C******************************************************
053600000000     C** PULIZIA CAMPI DS
053700011127     C******************************************************
053800011127     C     AZZER         BEGSR
053900051121     C                   MOVEL     *BLANKS       v01att
054000051121     C                   MOVE      *zeros        v01prg
054300051121     C                   MOVEL     *blanks       v01chi
054301051129     C                   MOVEL     *blanks       v01dchi
054400051121     C                   MOVEL     *blanks       v01rep
054401051129     C                   MOVEL     *blanks       v01drep
054402051129     C                   MOVEL     *blanks       v01mult
054403051129     C                   MOVEL     *blanks       v01dmult
055400051121     C                   Z-ADD     0             v01dti
055500051121     C                   Z-ADD     999999        v01dtf
055501051125     C                   MOVEL     'N'           w02ant
055600000000     C                   ENDSR
055700011127     C******************************************************
055800011127     C     *INZSR        BEGSR
055900011127     C******************************************************
056000011127     C*
056100011127     C     *ENTRY        PLIST
056200051121     C                   PARM                    KPJBA
056201051122     c                   movel     kpjbu         tntu02ds
056500051122     C                   EVAL      Mod = t02Mod
056600051121     C     Keytua        KLIST
056700051121     C                   KFLD                    v01att
056800051121     C                   KFLD                    v01prg
057000051121     C     Ktbe          KLIST
057100051121     C                   KFLD                    tbecod
057200051121     C                   KFLD                    tbeke1
057300011228     c     mod           comp      '5 '                                   40
057400020103     c                   select
057500020103     c                   when      mod = '1 '
057600030122     c                   eval      modv = '  INSERIMENTO  '
057700020103     c                   when      mod = '2 '
057800030122     c                   eval      modv = '   MODIFICA    '
057900020103     c                   when      mod = '3 '
058000030122     c                   eval      modv = '     COPIA     '
058100020103     c                   when      mod = '4 '
058200030122     c                   eval      modv = '    ANNULLA    '
058300020103     c                   when      mod = '5 '
058400020103     c                   eval      modv = 'VISUALIZZAZIONE'
058500020103     c                   endsl
059500011127     C                   ENDSR
061100030123
061200051121     C**************************************************************************
061300051121     C* tabella TUC/TUR
061400051121     C**************************************************************************
061500051121     C     srtab         BEGSR
061800051121     C     Ktbe          chain     tntbe01l                           10
061900051121     C     *in10         ifeq      *on
062000051121     C     tbeatb        orne      *blanks
062001051206     c                   seton                                        10
062100051121     c                   clear                   tbeuni
062400051121     c                   end
062500051121     C                   ENDSR
