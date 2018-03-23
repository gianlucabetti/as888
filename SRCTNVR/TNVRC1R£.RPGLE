000100071108    ?H* TIBSC1R *-----------------------------------------------------*
000200130715?    H*            CREAZIONE unica dati per Filiale - FILTRO -        *
000300011206?    H*---------------------------------------------------------------*
000400011206     H DECEDIT('0,') DATEDIT(*DMY.)
000500071108     FTIBSc1D   CF   E             WORKSTN
000600071108     FAZORG01L  IF a E           K DISK
000700071109     FAZCAE01L  IF a E           K DISK
000800071109     Ffiapd01l  IF a E           K DISK
000900011206?    D*---------------------------------------------------------------*
001000011206     D L1              S              3  0 DIM(30)                              P.O.GESTITE
001100071109     D msg             S             78    DIM(15) CTDATA PERRCD(1)
001200011206?    D*
001300061201     D* PASSAGGIO DATI PER CREAZIONE numeratori di gruppo  - AZNUM00F -
001400061201     d*                                       e di filiale - FLNUF00F -
001500011206     D PARAM           DS
001600011206     D* Codice P.O.
001700011206     D  PARFIL                 3      5  0
001800061201     D* terminal partenza di  PARFIL per numeratori partenza
001900061201     D  partfp                 6      8  0
002000011206     D* Codice Numeratore
002100061201     D  parCNU                 9     11
002200011206     D* Tipo P.O.
002300061201     D  parFTF                12     12
002400011206     D* Anno
002500061201     D  parANN                13     16  0
002600020206     D* Società
002700061201     D  parSOC                17     19
002800061201     D*
002900011206     D WLBDAT          DS                  INZ
003000011206     D  G02DAT                 1      8  0
003100011206     D  G02INV                 9     16  0
003200011206     D  G02ERR                17     17
003300011206     D  G02TGI                18     22  0
003400011206?    D*
003500011206     D* DS PER TRUL06R - CARICAMENTO £X
003600011206     D DSUL06        E DS                  EXTNAME(TRUL06DS)
003700011206     D  LIN                    1     90  0
003800011206     D                                     DIM(30)                              P.O. COMODO
003900011206     D*
004000011206     D TIBS02DS      E DS                  INZ
004100011206     D FNLV55DS      E DS                  INZ
004200071109     D og150         E DS                  INZ
004300071109     D og149         E DS                  INZ
004400071109     D og148         E DS                  INZ
004500071108     D og147         E DS                  INZ
004600071108     D og146         E DS                  INZ
004700071108     D og144         E DS                  INZ
004800071108     D og141         E DS                  INZ
004900011206?    D*
005000011206     D KPJBA         E DS
005100011206     D CNCR80        E DS
005200011206     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
005300071108     d SAvpod          s                   like(v1cpod)
005400071109     d Ktip            s                   like(apdtip) inz('A')
005500071109     d Kpdr            s                   like(apdpdr)
005600071108     d vidaan          s              2  0
005700071109     d WRSC            s             35
005800071109     d Werrcrea        s              1
005900011206     D*
006000011206     C*****************************************************************
006100011206     C* RIEPILOGO INDICATORI
006200011206     C*****************************************************************
006300011206     C* 30    - DI COMODO
006400011206     C* 35    - USATO PER TESTN
006500011206     C* 40/43 - INDICATORI PER MESSAGGI DI ERRORE
006600011206     C* 90    - SEGNALA LA PRESENZA DI ERRORI
006700011206     C*****************************************************************
006800011206     C     *ENTRY        PLIST
006900011206     C                   PARM                    KPJBA
007000011206     C                   Z-ADD     1             CODUT
007100011206     C                   CALL      'X§PARUT'
007200011206     C                   PARM                    UT§DSE
007300011206     C                   MOVEL     RAGUT         RSUT             20
007400011206     C                   MOVEL     REC80         CNCR80
007500020206     C*
007600011206     C* DATA DEL GIORNO
007700011206     C                   TIME                    W0140            14 0
007800011206     C* UDATE IN GGMMAAAA
007900011206     C                   MOVE      W0140         WDTGIO            8 0
008000011206     C* UDATE IN AAAAMMGG
008100011206     C                   Z-ADD     WDTGIO        G02DAT
008200011206     C                   MOVEL     *BLANK        G02ERR
008300011206     C                   CALL      'XSRDA8'
008400011206     C                   PARM                    WLBDAT
008500011206     C                   MOVEL     G02INV        DATEU             8 0
008600061201     c                   movel     dateu         annocor           4 0
008700071108     C                   MOVEL     G02dat        viddat            8 0
008800071109     c*
008900071109     c     kapd          klist
009000071109     c                   kfld                    ktip
009100071109     c                   kfld                    kpdr
009200011206     C*
009300011206     C* PULIZIA CAMPI FORMATO
009400071109     C                   CLEAR                   v1cpOD
009500071109     C                   CLEAR                   v1dpod
009600071109     c                   clear                   v1cape
009700011206     C*
009800011206?    C     INIZIO        TAG
009900071108     c                   clear                   v1ctfp
010000071108     c                   clear                   v1dtfp
010100071108     c                   clear                   v1ctfa
010200071108     c                   clear                   v1dtfa
010300071108     c                   clear                   v1corg
010400071109     c                   clear                   v1cpad
010500071108     c                   clear                   v1cpoc
010600071108     c                   clear                   v1dpoc
010700071108     c                   clear                   v1cter
010800071108     c                   clear                   wforzapod         1
010900011206     C*
011000011206     C     FOR01         TAG
011100071108     C                   EXFMT     BSc1D01
011200071108     c                   setoff                                       289040
011300071108     c                   setoff                                       414243
011400071109     c                   setoff                                       444546
011500011206     C*
011600061201?    C* F3 - FINE LAVORO
011700011206     C   KC              GOTO      FINE
011800011206     C*
011900011206?    C* CONTROLLI FORMATO
012000011206     C                   EXSR      CONTR
012100011206     C  NKF
012200011206     COR 90              GOTO      FOR01
012300011206     C*
012400061201?    C* F6 - ELABORAZIONE
012500071108     c*
012600071108     c                   if        v1corg='SI'
012700071108     c                   EXSR      CREAORG
012800071108     c                   endif
012900071108     c                   if        v1cTER='SI'
013000071108     c                   EXSR      CREATER
013100071108     c                   endif
013200071109     c                   if        v1cpad='SI'
013300071109     c                   EXSR      CREApadr
013400071109     c                   endif
013500071109     c
013600071109     C*  Se ci sono stati errori in creazione segnalo
013700071109     c                   if        werrcrea='S'
013800071109     c                   movel     msg(14)       vidmsg
013900071109     c                   else
014000071109     c                   movel     msg(15)       vidmsg
014100071109     c                   endif
014200071109     c                   seton                                        9028
014300061201     C*
014400011206     C                   GOTO      INIZIO
014500011206     C     FINE          TAG
014600011206     C                   SETON                                        LR
014700011206     C*
014800011206?    C*---------------------------------------------------------------*
014900011206?    C* CONTR * CONTROLLI FORMATO                                     *
015000011206?    C*---------------------------------------------------------------*
015100011206     C     CONTR         BEGSR
015200011206     C                   SETOFF                                       90
015300020206     C*
015400011206     C*
015500020206?    C****  P.O.  ****
015600071108IF  1C     V1cPOD        IFEQ      0
015700071108     c                   movel     msg(2)        vidmsg
015800071108     C                   SETON                                        289040
015900011206     C                   GOTO      ENDCTR
016000071108X  x1C                   ELSE
016100011206     C*
016200071108     C     V1cPOD        CHAIN     AZORG                              30
016300071108    2c                   if        not *in30 and (orgfag='V' or
016400071108     c                             orgfva<>' ')
016500071108     c                   movel     msg(2)        vidmsg
016600071108     C                   SETON                                        289040
016700071108     C                   GOTO      ENDCTR
016800071108    2c                   endif
016900071108     c
017000071108     c*Se cambia p.o. pulisco forzatura
017100071108    2c                   if        savpod<>v1cpod
017200071108     c                   clear                   wforzapod
017300071108     c                   eval      savpod=v1cpod
017400071108    2c                   endif
017500071108     c
017600071108     C* Se non esiste potrebbe essere creazione
017700071108    2c                   if        *in30  and wforzapod=' '
017800071108     c                   movel     msg(1)        vidmsg
017900071108     C                   SETON                                        289040
018000071108     c                   eval      wforzapod='S'
018100071108     C                   GOTO      ENDCTR
018200071108    2c                   endif
018300071108     c
018400071108     c* Se p.o. inesistente forzato, descrizione obbligatoria
018500071108    2c                   if        wforzapod='S'
018600071108    3c                   if        v1dpod=*blanks
018700071108     c                   movel     msg(3)        vidmsg
018800071108     C                   SETON                                        289045
018900071108     C                   GOTO      ENDCTR
019000071108    3c                   endif
019100071108     c
019200071108   x2c                   else
019300071108     c                   movel     orgdes        v1dpod
019400071108    2c                   endif
019500071108     c
019600011206E   1C                   ENDIF
019700071108     c*************************+
019800071108     c* Data apertura
019900071108     c*************************+
020000071108     c* Obbligatoria per nuovo p.o o per creazione terminal
020100071108    1c                   if        wforzapod='S' or v1cter='SI'
020200071108    2c                   if        v1cape=0
020300071108     c                   movel     msg(4)        vidmsg
020400071108     C                   SETON                                        289041
020500071108     C                   GOTO      ENDCTR
020600071108    2c                   endif
020700071108    1c                   endif
020800071108     c
020900071108    1c                   if        v1cape>0
021000071108     C                   Z-ADD     v1cape        G02DAT
021100071108     C                   MOVEL     *BLANK        G02ERR
021200071108     C                   CALL      'XSRDA8'
021300071108     C                   PARM                    WLBDAT
021400071108     c
021500071108    2c                   if        g02err='1'
021600071108     c                   movel     msg(6)        vidmsg
021700071108     C                   SETON                                        289041
021800071108     C                   GOTO      ENDCTR
021900071108E   2C                   ENDIF
022000071108     C                   MOVEL     G02INV        invape            8 0
022100071108     c                   movel     g02dat        v1cape
022200071108    1c                   endif
022300071108     C*
022400071108     c*************************+
022500071108     c* Crezione organigramma
022600071108     c*************************+
022700071108     c*  Obbligatoria se p.o. nuovo
022800071108    1c                   if        wforzapod='S' and v1corg='  '
022900071108     c                   movel     msg(5)        vidmsg
023000071108     C                   SETON                                        289046
023100071108     C                   GOTO      ENDCTR
023200071108    1c                   endif
023300071109     c
023400071108    1c                   if        v1corg='SI'
023500071109     c* Se esiste già non posso selezionarlo
023600071109     c     v1cpod        chain     azorg01l
023700071109     c                   if        %found(azorg01l)
023800071109     c                   movel     msg(13)       vidmsg
023900071109     C                   SETON                                        289046
024000071109     C                   GOTO      ENDCTR
024100071109    1c                   endif
024200071108     c
024300071108     c* Controllo p.o. di copia
024400071108     c                   clear                   v1dpoc
024500071108    2c                   if        v1cpoc=0
024600071108     c                   movel     msg(7)        vidmsg
024700071108     C                   SETON                                        289042
024800071108     C                   GOTO      ENDCTR
024900071108   x2c                   else
025000071108     C     V1cPOc        CHAIN     AZORG                              30
025100071108    3c                   if        *in30 or  orgfag='V' or
025200071108     c                             orgfva<>' '
025300071108     c                   movel     msg(7)        vidmsg
025400071108     C                   SETON                                        289042
025500071108     C                   GOTO      ENDCTR
025600071108    3c                   endif
025700071108     c                   movel     orgdes        v1dpoc
025800071108    2c                   endif
025900071108    1c                   endif
026000071108     c*************************+
026100071108     c* Crezione Terminal     non devono esistere
026200071108     c*************************+
026300071108    1c                   if        v1cter='SI'
026400071108    2c                   if        v1ctfp=0 and v1ctfa=0
026500071108     c                   movel     msg(8)        vidmsg
026600071108     C                   SETON                                        289043
026700071108     C                   GOTO      ENDCTR
026800071108    2c                   endif
026900071108     c
027000071108    2c                   if        v1ctfp>0
027100071108     C                   CLEAR                   FNLV55DS
027200071108     C                   MOVEL     'P'           D55TPT
027300071108     C                   MOVEL     V1cpod        D55LIN
027400071108     C                   MOVEL     invape        d55DRF
027500071108     C                   CALL      'FNLV55R'
027600071108     C                   PARM                    FNLV55DS
027700071108     c
027800071108     c* Se già presente errore
027900071108    3c                   if        d55err=' '
028000071108     c                   movel     msg(9)        vidmsg
028100130715     c                   eval      %subst(vidmsg:51:3)=%editc(d55tfp: 'X')
028200071108     C                   SETON                                        289043
028300071108     C                   GOTO      ENDCTR
028400071108    3c                   endif
028500071108     c
028600071108     c* il p.o. deve essere esistente ed un terminal di partenza
028700071108     c*  se <> dal p.o. stesso
028800071108    3c                   if        v1ctfp=v1cpod
028900071108     c                   movel     v1dpod        v1dtfp
029000071108     c                   else
029100071108     c     v1ctfp        chain     azorg01l
029200071108    4c                   if        *in30 or  orgfag='V' or
029300071108     c                             orgfva<>' '
029400071108     c                   movel     msg(10)       vidmsg
029500071108     C                   SETON                                        289043
029600071108     C                   GOTO      ENDCTR
029700071108    4c                   endif
029800071108     c
029900071108     c                   movel     orgdes        v1dtfp
030000071108     c
030100071108     c* Verifico se terminal di partenza
030200071108     C                   CLEAR                   FNLV55DS
030300071108     C                   MOVEL     'P'           D55TPT
030400071108     C                   MOVEL     V1ctfp        D55LIN
030500071108     C                   MOVEL     invape        d55DRF
030600071108     C                   CALL      'FNLV55R'
030700071108     C                   PARM                    FNLV55DS
030800071108     c
030900071108    4c                   if        d55err<>' ' or v1ctfp<>d55tfp
031000071108     c                   movel     msg(10)       vidmsg
031100071108     C                   SETON                                        289043
031200071108     C                   GOTO      ENDCTR
031300071108    4c                   endif
031400071108     c
031500071108    3c                   endif
031600071108    2c                   endif
031700071108     c
031800071108    2c                   if        v1ctfa>0
031900071108     C                   CLEAR                   FNLV55DS
032000071108     C                   MOVEL     'A'           D55TPT
032100071108     C                   MOVEL     V1cpod        D55LIN
032200071108     C                   MOVEL     invape        d55DRF
032300071108     C                   CALL      'FNLV55R'
032400071108     C                   PARM                    FNLV55DS
032500071108     c
032600071108    3c                   if        d55err=' '
032700071108     c                   movel     msg(11)       vidmsg
032800130715     c                   eval      %subst(vidmsg:51:3)=%editc(d55tfa: 'X')
032900071108     C                   SETON                                        289044
033000071108     C                   GOTO      ENDCTR
033100071108    3c                   endif
033200071108     c
033300071108     c* il p.o. deve essere esistente ed un terminal di arrivo
033400071108     c*  se <> dal p.o. stesso
033500071108    3c                   if        v1ctfa=v1cpod
033600071108     c                   movel     v1dpod        v1dtfa
033700071108     c                   else
033800071108     c     v1ctfa        chain     azorg01l
033900071108    4c                   if        *in30 or  orgfag='V' or
034000071108     c                             orgfva<>' '
034100071108     c                   movel     msg(12)       vidmsg
034200071108     C                   SETON                                        289044
034300071108     C                   GOTO      ENDCTR
034400071108    4c                   endif
034500071108     c
034600071109     c                   movel     orgdes        v1dtfa
034700071108     c
034800071108     c* Verifico se terminal di arrivo
034900071108     C                   CLEAR                   FNLV55DS
035000071108     C                   MOVEL     'A'           D55TPT
035100071108     C                   MOVEL     V1ctfa        D55LIN
035200071108     C                   MOVEL     invape        d55DRF
035300071108     C                   CALL      'FNLV55R'
035400071108     C                   PARM                    FNLV55DS
035500071108     c
035600071108    4c                   if        d55err<>' ' or v1ctfa<>d55tfa
035700071108     c                   movel     msg(12)       vidmsg
035800071108     C                   SETON                                        289044
035900071108     C                   GOTO      ENDCTR
036000071108    4c                   endif
036100071108     c
036200071108    3c                   endif
036300071108    2c                   endif
036400071108    1c                   endif
036500011206     C*
036600011206     C     ENDCTR        ENDSR
036700071108?    C*---------------------------------------------------------------*
036800071108?    C* CREAORG * creazione organigramma
036900071108?    C*---------------------------------------------------------------*
037000071108     C     CREAorg       BEGSR
037100071108     c     v1cpoc        chain     azorg01l
037200071108     c* campi da modificare
037300071108     c                   movel     v1cpod        orgfil
037400071108     c                   movel     v1dpod        orgdes
037500071108     c                   movel(P)  'V.'          orgind
037600071109     c                   movel(P)  '0'           orgtel
037700071109     c                   movel(P)  '0'           orgfax
037800071108     c                   clear                   orgtlx
037900071108     c                   if        orgfag='A'
038000071108     c                   movel     v1dpod        orgdea
038100071108     c                   movel(P)  'V.'          orgina
038200071109     c                   movel(P)  '0'           orgtea
038300071109     c                   movel(P)  '0'           orgfaa
038400071108     c                   clear                   orgtla
038500071108     c                   endif
038600071109     c* Descrizione 150
038700071109     c                   movel     orgdf0        og150
038800071109     c                   clear                   §ogpck
038900071109     c                   clear                   §ogpkm
039000080418     c                   clear                   §ogtis
039100071109     c                   movel     og150         orgdf0
039200071108     c* Descrizione 148
039300071108     c                   movel     orgde8        og148
039400071108     c                   clear                   §oglpo
039500071108     c                   clear                   §ogtlls
039600080418     c                   clear                   §oggeot
039700080418     c                   clear                   §ogbd1
039800080418     c                   clear                   §ogbd2
039900071108     c                   movel     'N'           §oglbl
040000071108     c                   movel     'N'           §oglfv
040100071108     c                   movel     'N'           §ognid
040200071109     c                   movel     'A'           §ogorm
040300080418     c                   movel     ' '           §ogpdaorm
040400080418     c                   movel     ' '           §ogpdacon
040500071108     c                   movel     og148         orgde8
040600071109     c* Descrizione 149
040700071109     c                   movel     orgde9        og149
040800071109     c                   movel(P)  '0'           §ogtld
040900071109     c                   movel     og149         orgde9
041000071108     c* Descrizione 146
041100071108     c                   movel     orgde6        og146
041200071108     c                   movel     invape        §ogdsd
041300071108     c                   movel     invape        §ogdse
041400071108     c                   clear                   §ogdda
041500071108     c                   movel     og146         orgde6
041600071108     c
041700071108     c                   movel     orgde1        og141
041800071109     c                   eval      §ogssc='RxxxSEDE   '
041900071108     c                   eval      %subst(§ogssc:2:3)=%editc(v1cpod:'X')
042000071108     c                   movel     og141         orgde1
042100071108     c
042200071108     c                   movel     orgde4        og144
042300071109     c                   eval      §ogssp='RxxxSEDE   '
042400071108     c                   eval      %subst(§ogssp:2:3)=%editc(v1cpod:'X')
042500071108     c                   movel     og144         orgde4
042600071108     c
042700080418     c* Descrizione 147
042800071108     c                   movel     orgde7        og147
042900071108     c                   clear                   §ogcgio
043000071108     c                   clear                   §ogddao
043100071108     c                   clear                   §ogfrtuo
043200071108     c                   clear                   §ogdoada
043300071108     c                   clear                   §ogfppda
043400071108     c                   movel     'N'           §ognoc
043500071108     c                   movel     og147         orgde7
043600071108     c* Pulisco le descrizione degli utenti
043700071108     c                   clear                   orgd10
043800071108     c                   clear                   orgf10
043900071108     c                   clear                   orgd13
044000071108     c                   clear                   orgf13
044100071108     c                   clear                   orgd15
044200071108     c                   clear                   orgf15
044300071108     c                   clear                   orgd18
044400071108     c                   clear                   orgf18
044500071108     c                   clear                   orgf25
044600071108     c                   clear                   orgd25
044700071108     c                   clear                   orgf28
044800071108     c                   clear                   orgd28
044900071108     c                   clear                   orgf31
045000071108     c                   clear                   orgd31
045100071108     c                   clear                   orgf33
045200071108     c                   clear                   orgd33
045300071108     c                   clear                   orgf36
045400071108     c                   clear                   orgd36
045500071108     c                   clear                   orgf41
045600071108     c                   clear                   orgd41
045700071108     c                   clear                   orgf46
045800071108     c                   clear                   orgd46
045900071108     c                   clear                   orgf47
046000071108     c                   clear                   orgd47
046100071108     c                   clear                   orgf49
046200071108     c                   clear                   orgd49
046300071108     c                   clear                   orgf56
046400071108     c                   clear                   orgd56
046500071108     c                   clear                   orgf58
046600071108     c                   clear                   orgd58
046700071108     c                   clear                   orgf61
046800071108     c                   clear                   orgd61
046900080418     c                   movel(P)  '.'           orgd71
047000080418     c                   movel(P)  '.'           orgd72
047100080418     c* Se primo livello se stesso, altrimenti il terminal di partenza
047200071108    1c                   if        v1ctfp>0
047300071108     c                   movel     v1dtfp        orgdc0
047400071109     c                   movel     v1ctfp        orgfc0
047500071108   x1c                   else
047600071108     c* Cerco il terminal di partenza
047700071108     C                   CLEAR                   FNLV55DS
047800071108     C                   MOVEL     'P'           D55TPT
047900071108     C                   MOVEL     V1cpod        D55LIN
048000071108     C                   MOVEL     invape        d55DRF
048100071108     C                   CALL      'FNLV55R'
048200071108     C                   PARM                    FNLV55DS
048300071108     c                   movel     d55tfp        orgfc0
048400071108     c     d55tfp        chain     azorg01l
048500071108    2c                   if        %found(azorg01l)
048600071108     c                   movel     orgdes        orgdc0
048700071108     c                   else
048800071108     c                   clear                   orgdc0
048900071108    2c                   endif
049000071108    1c                   endif
049100071108     c
049200071109     c                   movel     v1dpod        orgfc5
049300071109     c                   movel     v1cpod        orgfc5
049400071109     c                   movel     v1cpod        orgfe0
049500071109     c                   movel     v1cpod        orgfe1
049600071109     c                   movel     v1cpod        orgfe2
049700071109     c                   movel     v1cpod        orgfe3
049800071109     c                   movel     v1cpod        orgfe4
049900071109     c                   movel     v1cpod        orgfe5
050000071109     c                   movel     v1cpod        orgfe6
050100071109     c                   movel     v1cpod        orgfe7
050200071109     c                   movel     v1cpod        orgfe8
050300071109     c                   movel     v1cpod        orgfe9
050400071109     c                   movel     v1cpod        orgff0
050500080418     c                   movel     v1cpod        orgf71
050600080418     c                   movel     v1cpod        orgf72
050700071109     c                   eval      orgftr='N'
050800071108     c
050900071108     c                   write     azorg
051000071108     c* Richiamo la manutenzione organigramma
051100071108     c                   call      'TNSD15R'
051200071108     c                   parm                    kpjba
051300071108     c
051400071108     c                   ENDSR
051500071108?    C*---------------------------------------------------------------*
051600071108?    C* CREATER * creazione terminal
051700071108?    C*---------------------------------------------------------------*
051800071108     C     CREAter       BEGSR
051900071109     c* terminal di partenza
052000071109     c                   if        v1ctfp>0
052100071109     c                   clear                   azcae000
052200071109     c                   eval      caeepa='P'
052300071109     c                   eval      caetfp=v1cpod
052400071109     c                   eval      caetfe=v1ctfp
052500071109     c                   eval      caedde=dateu
052600071109     c                   eval      caedsc=20391231
052700071109     c                   write     azcae000
052800071109     c                   endif
052900071109     c
053000071109     c* terminal di Arrivo
053100071109     c                   if        v1ctfa>0
053200071109     c                   clear                   azcae000
053300071109     c                   eval      caeepa='A'
053400071109     c                   eval      caetfa=v1cpod
053500071109     c                   eval      caetfe=v1ctfa
053600071109     c                   eval      caedde=invape
053700071109     c                   eval      caedsc=20391231
053800071109     c                   write     azcae000
053900071109     c                   endif
054000071108     c                   ENDSR
054100071109?    C*---------------------------------------------------------------*
054200071109?    C* CREApadr * creazione cpdici padroncini standard
054300071109?    C*---------------------------------------------------------------*
054400071109     C     CREApadr      BEGSR
054500071109     c* Se non esistono già
054600071109     c                   clear                   kpdr
054700071109     c                   movel     v1cpod        kpdr
054800071109     c                   move      993           kpdr
054900071109     c                   eval      wrsC='C.A. CHIUSE            '
055000071109     C                   EXSR      SCRIVIAPD
055100071109     C
055200071109     c                   clear                   kpdr
055300071109     c                   movel     v1cpod        kpdr
055400071109     c                   move      996           kpdr
055500071109     c                   eval      wrsC='RESI                   '
055600071109     C                   EXSR      SCRIVIAPD
055700071109     C
055800071109     c                   clear                   kpdr
055900071109     c                   movel     v1cpod        kpdr
056000071109     c                   move      997           kpdr
056100071109     c                   eval      wrsC='C/SERVIZI              '
056200071109     C                   EXSR      SCRIVIAPD
056300071109     C
056400071109     c                   clear                   kpdr
056500071109     c                   movel     v1cpod        kpdr
056600071109     c                   move      998           kpdr
056700071109     c                   eval      wrsC='GIAC./DIROTT.          '
056800071109     C                   EXSR      SCRIVIAPD
056900071109     C
057000071109     c                   clear                   kpdr
057100071109     c                   movel     v1cpod        kpdr
057200071109     c                   move      999           kpdr
057300071109     c                   eval      wrsC='CONSEGNE A MAGAZZINO   '
057400071109     C                   EXSR      SCRIVIAPD
057500071109     C
057600071109     c                   clear                   kpdr
057700071109     c                   movel     v1cpod        kpdr
057800071109     c                   move      9876          kpdr
057900071109     c                   eval      wrsC='CHIUSURA PERIODICA     '
058000071109     C                   EXSR      SCRIVIAPD
058001160331     c
058002160331     c                   clear                   kpdr
058003160331     c                   movel     v1cpod        kpdr
058004160331     c                   move      9510          kpdr
058005160331     c                   eval      wrsC='VEDI PACCO             '
058006160331     C                   EXSR      SCRIVIAPD
058100071109     C
058200071109     C                   ENDSR
058300071109?    C*---------------------------------------------------------------*
058400071109?    C* SCRIVIAPD* scrittura file fiapd
058500071109?    C*---------------------------------------------------------------*
058600071109     C     SCRIVIAPD     BEGSR
058700071109     c     kapd          chain     fiapd01l
058800071109     c                   if        not %found(fiapd01l)
058900071109     c                   movel     070           kpdr
059000071109     c     kapd          chain     fiapd01l
059100071109     c                   if        not %found(fiapd01l)
059200071109     c                   eval      werrcrea='S'
059300071109     c                   else
059400071109     c                   movel     v1cpod        apdpdr
059500071109     c                   eval      apdrsc=wrsc
059600071109     c                   eval      apdrsf=wrsc
059700071109     c                   z-add     dateu         apddtr
059800071109     c                   clear                   apdftr
059900071109     c                   clear                   apdatb
060000071109     c                   write     fiapd000
060100071109     c                   endif
060200071109     c                   endif
060300071109     c
060400071109     C                   ENDSR
060500071109     c
060600071108**
060700090420Filiale inesistente: enter per forzare e creare la nuova filiale
060800130715Filiale inesistente, annullata o tipo "V"
060900130715Descrizione obbligatoria per nuova Filiale
061000130715Data apertura obbligatoria per Filiale nuova o creazione terminal
061100130715Creazione organigramma obbligatoria per nuova Filiale
061200071108Data errata
061300130715Filiale da cui copiare inesistente
061400071108Per creare i terminal immettere T.Partenza e/o T.Arrivo
061500130715Per la Filiale esiste già il terminal di partenza xxx alla data di apertura
061600071108Terminal di Partenza errato o non è un terminal di partenza                    10
061700130715Per la Filiale esiste già il terminal di Arrivo   xxx alla data di apertura
061800071109Terminal di Arrivo errato o non è un terminal di Arrivo                        12
061900130715Esiste già l'organigramma per la Filiale indicata
062000071109Errori in creazione dati: verificare!!!!
062100071109Dati creati ...
