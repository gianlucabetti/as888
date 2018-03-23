000200000000     H DECEDIT('0,') DATEDIT(*DMY.)
000300011127     H*--------------------------------------------------------------*
000400051122     H*             - Anagrafico tempi     -                         *
000500000000     H*--------------------------------------------------------------*
000900051124     Ftntu04d   CF   E             WORKSTN
001000051122     Ftntut00f  UF a E             DISK
001001051124     Fcnaco00f  iF   E           k DISK
001002051122     Fazorg01l  iF   E           k DISK
001003051124     Ftntua01l  if   E           k DISK
001004051129     Ftntbe01l  if   E           k DISK
001100030116
001200030116     D Errore          C                   '1'
001300030116     D Eseguito        C                   '0'
001400030116
001503051122     D tntu04ds      E DS
001504051201     D tntuperds     E DS
001505051129     D dtut          E DS
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
005600051122     D                                     LIKE(t04Mod)
006500030120
006600030120     D Refresh         S              1
006700030120     D Esito           S              1
006800030120     D GesMsg          S              1
006900030120     D Output          S           4000
007000030120     D LenOut          S              9B 0
007100030120     D StrutturaO      S             10
007200030116
007300000000     C                   EXSR      AZZER
007400051122     c                   if        t04nrr <> 0
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
010100051122     C                   MOVE      'C'           t04CMD
010200011228     C                   LEAVE
010300011228     c                   end
010400011228     C* GUIDA
010500011228     C                   IF        *INKL
010600051122     C                   MOVE      'L'           t04CMD
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
014300051122     C                   MOVE      'F'           t04CMD
014400011228     C                   LEAVE
014500011228     c                   end
014600011228     c                   enddo
014601051124     c                   movel     tntu04ds      kpjbu
014700011127     c*
014800000000     C                   SETON                                        LR
014900011127     c**********************************************************************
015000011228     c* carica 1° formato
015100011127     c**********************************************************************
015200011228     C     srcarica      BEGSR
015300020218     c                   clear                   msgv
015400051122     C                   EVAL      Mod = t04Mod
015500051122     C     t04nrr        chain     tntut00f
015600051122     C                   if        %found(tntut00f)
015602051121     c* messaggio d'errore
015700051122     c                   if        (mod = '2 ' and tutatb <> *blanks)
016500020218     c                   eval      msgv = 'Per poter modificare il record occor-
016600020218     c                             re prima ripristinarlo'
016800020218     c* forzo la visualizzazione
016900030122     c                   eval      modv = 'VISUALIZZAZIONE'
017000020214     c                   eval      mod = '5 '
017100020218     c                   seton                                        40
017101051122     c                   end
017300051122     c                   if        mod = '4 ' and tutatb <> *blanks
017400030122     c                   eval      modv = '   RIPRISTINO  '
017500030122     c                   eval      mod = '2 '
017600020214     c                   end
017601051122     c     mod           comp      '2 '                                   42
017603051124     c                   if        t04att <> *blanks
017700051124     c                   movel     t04att        v01att
017701051124     c                   move      t04prg        v01prg
017702051124     c                   else
017703051124     c                   movel     tutatt        v01att
017800051122     c                   move      tutprg        v01prg
017801051124     c                   end
017802051129     c                   exsr      srtua
017900051121     c                   movel     tuades        v01des
018000051122     c                   movel     tutfil        v01fil
018002051124     c                   exsr      srfil
018100051124     c                   movel     tutcli        v01cli
018101051124     c                   exsr      srcli
018500051121     c                   z-add     0             v01dti
018501051124     c                   z-add     tuttem        v01tem
018600051122     c                   if        tutdti <> *loval
018700051122     c                   move      tutdti        data
018800011228     c                   eval      com6 = gg + mm + aa
018900051121     c                   move      com6          v01dti
019000011228     c                   end
019100051121     c                   z-add     999999        v01dtf
019200051122     c                   if        tutdtf <> *hival
019300051122     c                   move      tutdtf        data
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
021600051122     c* personalizzaz. p.o.
021801051124     c                   exsr      srfil
023600051122     c* personalizzaz. cliente
023701051124     c                   exsr      srcli
023702060111     c* o il cliente o il p.o.
023703060111     c                   if        v01fil <> 0 and v01cli <> 0
023704060111     c                   seton                                        010299
023705060111     c                   end
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
036300051121     ** Controllo sovrapposizione periodo validità e progressivo
036400060117     C                   IF        NOT *IN99
036704051201     C                   CLEAR                   tntuperds
036705051201     c                   eval      §perfile = 'TNTUT00F'
036706051201     c                   eval      §peratt = v01att
036707051201     c                   eval      §perprg = v01prg
036708060118     c                   eval      §perdti = dtadal
036709060118     c                   eval      §perdtf = dtaal
036710051201     c                   eval      §perfil = v01fil
036711051201     c                   eval      §percli = v01cli
036712060118     c                   if        mod <> '1 ' and mod <> '3 '
036713060117     c                   eval      §pernrr = t04nrr
036714060118     c                   end
036715051201     C                   CALL      'TNTUPERR'
036716051201     C                   PARM                    tntuperds
037500060117     c*errore se sono in inserimento e trovo già un periodo
037602060117     C                   if        §Peresi = Eseguito
037700030116     C                   EVAL      *IN99 = *ON
037800051121     C                   EVAL      *IN05 = *ON
037901060117     C                   ENDIF
038000030116
038004051201     C                   CLEAR                   tntuperds
038005051201     c                   eval      §perfile = 'TNTUT00F'
038006051201     c                   eval      §peratt = v01att
038007051201     c                   eval      §perprg = v01prg
038008060118     c                   eval      §perdti = dtadal
038009060118     c                   eval      §perdtf = dtaal
038010051201     c                   eval      §perfil = v01fil
038011051201     c                   eval      §percli = v01cli
038012060118     c                   if        mod <> '1 ' and mod <> '3 '
038013060117     c                   eval      §pernrr = t04nrr
038014060118     c                   end
038015051201     C                   CALL      'TNTUPERR'
038016051201     C                   PARM                    tntuperds
038800030116
038904060117     C                   if        §Peresi = Eseguito
039000030116     C                   EVAL      *IN99 = *ON
039100051121     C                   EVAL      *IN06 = *ON
039200030116     C                   ENDIF
039300030116
039400030116     C                   ENDIF
039500030116
039600011127     C                   ENDSR
039700020107     C******************************************************
039800051124     C* cliente
039900020107     C******************************************************
040000051124     C     srcli         BEGSR
040001051124     c     v01cli        ifne      0
040002051124     c     keyaco        chain     cnaco00f
040003051124     c                   if        not %found(cnaco00f)
040004051124     c                   seton                                        0299
040005051124     c                   else
040006051124     c                   movel     acorag        v01dcli
040007051124     c                   end
040008051129     c                   if        §tutper <>'C' and
040009051129     c                             §tutper <>'E'
040010051129     c                   seton                                        0899
040011051129     c                   end
040012051124     c                   else
040013051124     c                   clear                   v01dcli
040014051124     c                   end
048400011127     C                   ENDSR
048406051124     C******************************************************
048407051124     C* p.o.
048408051124     C******************************************************
048409051124     C     srfil         BEGSR
048410051129     c     v01fil        ifne      0
048412051124     c     v01fil        chain     azorg01l
048413051124     c                   if        not %found(azorg01l)
048414051124     c                   seton                                        0199
048415051124     c                   else
048416051124     c                   movel     orgdes        v01dfil
048417051124     c                   end
048418051129     c                   if        §tutper <>'P' and
048419051129     c                             §tutper <>'E'
048420051129     c                   seton                                        0799
048421051129     c                   end
048422051124     c                   else
048423051124     c                   clear                   v01dfil
048424051124     c                   end
048425051124     C                   ENDSR
048426051129     C******************************************************
048427051129     C* RICERCA PRIMO VIDEO
048428051129     C******************************************************
048429051129     C     RIC01         BEGSR
048430051129     C                   ENDSR
048500011228     c**********************************************************************
048600011228     c* AGGIORNA ARCHIVIO
048700011228     c**********************************************************************
048800011228     C     srAGGIO       BEGSR
048900011228     C                   SETOFF                                       41
049000011228     c                   seton                                        90
049100011228     c                   if        mod <> '3 '
049200051122     C     t04nrr        chain     tntut00f                           90
049300011228     c                   end
049400051122     c                   movel     v01att        tutatt
049500051122     c                   move      v01prg        tutprg
049600051122     c                   move      v01fil        tutfil
049601051122     c                   move      v01cli        tutcli
049700051122     c                   move      v01tem        tuttem
050400020214     C* Date validità
050500051121     c                   if        v01dti <> 0
050600051122     c     *dmy          movel     v01dti        tutdti
050700020214     c                   else
050800051122     c                   movel     '0001-01-01'  tutdti
050900020214     c                   end
051000051121     c                   if        v01dtf <> 999999
051100051122     c     *dmy          movel     v01dtf        tutdtf
051200020214     c                   else
051300051122     c                   movel     '9999-12-31'  tutdtf
051400020214     c                   end
051500020214     c                   if        mod <> '4 '
051600051122     c                   movel     *blanks       tutatb
051601051122     c                   else
051602051122     c                   movel     'A'           tutatb
052100020214     c                   end
052200020214     c* aggiorno
052300020214     c                   if        not *in90
052400051122     c                   time                    tutupd
052600051122     c                   update    tntut000                             41
052700020214     c                   else
052800020214     c* scrivo
052900051124     c                   time                    tutwtr
053100051122     c                   write     tntut000                             41
053200020214     c                   end
053400011228     C                   ENDSR
053500011127     C******************************************************
053600000000     C** PULIZIA CAMPI DS
053700011127     C******************************************************
053800011127     C     AZZER         BEGSR
053801051124     c                   if        t04att <> *blanks
053802051124     C                   MOVEL     t04att        v01att
053803051124     C                   MOVE      t04prg        v01prg
053804051129     c                   exsr      srtua
053806051124     c                   MOVEL     tuades        v01des
053807051124     c                   else
053900051121     C                   MOVEL     *BLANKS       v01att
054000051124     C                   MOVE      *zeros        v01prg
054001051124     c                   MOVEL     *BLANKS       v01des
054002051124     c                   end
054300051122     C                   MOVE      *zeros        v01fil
054301051122     C                   MOVEL     *BLANKS       v01dfil
054400051122     C                   MOVE      *zeros        v01cli
054401051122     C                   MOVEL     *BLANKS       v01dcli
054402051122     C                   MOVE      *zeros        v01tem
055400051121     C                   Z-ADD     0             v01dti
055500051121     C                   Z-ADD     999999        v01dtf
055600000000     C                   ENDSR
055601051129     C******************************************************
055602051129     C     srtua         BEGSR
055603051129     C******************************************************
055604051129     C     keytua        chain     tntua01l
055605051129     c                   movel(p)  tuamult       tbeke1
055606051129     c     keytbe        chain     tntbe01l
055607051129     c                   if        %found(tntbe01l) and tbeatb = ' '
055608051129     c                   movel     tbeuni        dtut
055609051129     c                   else
055610051129     c                   clear                   dtut
055611051129     c                   end
055612051129     c                   ENDSR
055700011127     C******************************************************
055800011127     C     *INZSR        BEGSR
055900011127     C******************************************************
056000011127     C*
056100011127     C     *ENTRY        PLIST
056200051121     C                   PARM                    KPJBA
056201051122     c                   movel     kpjbu         tntu04ds
056500051122     C                   EVAL      Mod = t04Mod
056600051121     C     Keytua        KLIST
056700051121     C                   KFLD                    v01att
056800051121     C                   KFLD                    v01prg
056801051129     C     Keytbe        KLIST
056802051129     C                   KFLD                    tbecod
056803051129     C                   KFLD                    tbeke1
056804051129     c                   eval      tbecod='TUT'
056806051124     C     Keyaco        KLIST
056807051124     C                   KFLD                    ACOKUT
056808051124     C                   KFLD                    ACOKCC
056809051124     C                   KFLD                    v01cli
056810051124     C                   Z-ADD     1             ACOKUT
056811051124     C                   Z-ADD     151           ACOKCC
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
