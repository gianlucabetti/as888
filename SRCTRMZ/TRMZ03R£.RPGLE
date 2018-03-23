000100011212     H DFTACTGRP(*NO) BNDDIR('PJXBND') ACTGRP('QILE')
000200011212     H DECEDIT('0,') DATEDIT(*DMY.)
000300000000     F*          SELEZIONE GENERALIZZATA PER STAMPE
000400011204     D*----------------------------------------------------*
000500011204     FTRmz03D   CF   E             WORKSTN
000600011212     Fansif01l  iF   E           k disk
000700011212     Ftabel00f  iF   E           k disk
000800011212     Fanent01l  iF   E           k disk
000900011212     Fazorg01l  iF   E           k disk
001000011212     D* DS Interna per dati di output di XSOC
001100011212     D XSOCDS          DS          1000
001200011212     D Ana032ds      E DS                  INZ
001300011212     D ana007ds      e ds                  inz
001400011212    >D xsoc001ds     E DS
001500160517      *
001600160517     D tibs02ds      E DS
001700160517     D dmau          E DS
001800160517     D dcau          E DS
001900011204     D DTAiso          S               D   INZ
002000011204     D DTAEUR          S               D   DATFMT(*EUR) INZ
002100011204     D DTADAL          S               D   INZ
002200011204     D DTAAL           S               D   INZ
002300160809     D CMP             S              7    DIM(11) CTDATA PERRCD(1)             NOMI CAMPI
002400160809     D KEY             S              7    DIM(5)                               CAMPI PER KEYFLD
002500000000     D PARUT         E DS                  EXTNAME(UTEDSE0F)
002600000000     D KPJBA         E DS
002700000000     D PARAM           DS
002800011212     D  SCAT                          1
002900011212     D  SDIT                          1
003000011212     D  SFIL                          1
003100160809     D  keyfld                       35
003200011213     D  ANNOV                         4  0
003300011213     D  MESEV                         2  0
003400011213     D  VTGA                          2
003500011212     D  VTGN                          9
003600011212     D  VIMM                          4  0
003700011212     D  VCAT                          2
003800160517     D  Vcaut                         2
003900160517     D  Vmau                          3
004000011212     D  VDIT                          3
004100011212     D  VFIL                          3  0
004200011212     D  VCDC                          8
004300160808     D  Vdadug                        8  0
004400160808     D  Valdug                        8  0
004500160808     D  Vusodg                        8  0
004600160808     D  Vusoag                        8  0
004700000000     D                 DS
004800160808     D  SEL                    1     11  0 dim(11)
004900000000     D  VSEL01                 1      1  0
005000000000     D  VSEL02                 2      2  0
005100000000     D  VSEL03                 3      3  0
005200000000     D  VSEL04                 4      4  0
005300000000     D  VSEL05                 5      5  0
005400000000     D  VSEL06                 6      6  0
005500000000     D  VSEL07                 7      7  0
005600000000     D  VSEL08                 8      8  0
005700000000     D  VSEL09                 9      9  0
005800160808     D  VSEL10                10     10  0
005900160808     D  VSEL11                11     11  0
006000000000     D                 DS
006100160808     D  PAG                    1     11
006200160808     D                                     DIM(11)                              SALTO PAGINA
006300000000     D  VSCAT                  4      4
006400011204     D  VSDIT                  7      7
006500011204     D  VSFIL                  8      8
006600160808     D*-------------
006700160808     D WLBDA8          DS
006800160808     D  G02DAT                 1      8  0
006900160808     D  G02INV                 9     16  0
007000160808     D  G02ERR                17     17
007100160808     D  G02TGI                18     22  0
007200000000     C*----------------------------------------------------*
007300000000      * INDICATORI
007400000000      *
007500000000      * 73-87  ORDINAMENTO GIA SCELTO
007600000000      * 91     SALTO 'P' AMMESSO SOLO PER ORDINAMENTO 1
007700000000      * 21
007800000000     C*----------------------------------------------------*
007900011204     C  nlr              do        *hival                                       *
008000011214     C                   IF        KCDAZ = 'MZ18'
008100011213     C                   SETON                                            86
008200011213     C                   END
008300000000     C                   EXFMT     GA03D001                                      *
008400011204     c*--- FINE LAVORO                                     *
008500011204     c                   if        *inkc
008600011212     C                   MOVE      '1'           KPJBU
008700011204     C                   leave                                                  *
008800011204     c                   end
008900011204     c* ricerca
009000011204     c                   if        *inkd
009100011204     c                   exsr      ric01
009200011204     c                   iter
009300011204     c                   end
009400011204     c* controlli
009500011204     c                   exsr      ctr01
009600011204     c   99              iter
009700011204     c* IMPOSTA PARAMETRI                                  *
009800011204     c                   if        *inkf
009900011212     C                   move      VIMM          anno              2 0
010000011212     C                   movel     vimm          mese              2 0
010100011212     C                   movel     ANNO          comodo            4 0
010200011212     C                   move      mese          comodo
010300011212     C                   move      comodo        VIMM
010400011204     C                   MOVEA     KEY           KEYFLD                         *
010500011212     C                   MOVEl(p)  PARAM         KPJBU
010600011213     c                   select
010700011213     c                   when      kcdaz = 'MZ03'
010701170119     c                   eval      KJOST = 'H'
010800011213     C                   MOVEL     'MZ04'        KCOAZ
010900011213     c                   when      kcdaz = 'MZ33'
011000011213     C                   MOVEL     'MZ34'        KCOAZ
011100011213     c                   when      kcdaz = 'MZ18'
011200011213     C                   MOVEL     'MZ19'        KCOAZ
011300011213     c                   endsl
011400011213     C                   CALL      'BCH09'
011500011213     C                   PARM                    KPJBA
011600011213     C     KNMUS         IFEQ      *ALL'1'
011700011213     c                   select
011800011213     c                   when      kcdaz = 'MZ03'
011900011213     C                   CALL      'TRMZ04R1'
012000011213     C                   PARM                    KPJBA
012100011213     c                   when      kcdaz = 'MZ33'
012200011213     C                   CALL      'TRMZ34R'
012300011213     C                   PARM                    KPJBA
012400011213     c                   when      kcdaz = 'MZ18'
012500011213     C                   CALL      'TRMZ19R'
012600011213     C                   PARM                    KPJBA
012700011213     c                   endsl
012800011213     C                   ELSE
012900011213     C                   CALL      'BCH10'
013000011213     C                   PARM                    KPJBA
013100011213     C                   END
013200011204     c                   leave
013300011204     c                   end
013400011204     C                   enddo                                                  *
013500000000     C                   SETON                                          LR      *
013600011204     C*----------------------------------------------------*
013700011204      *  Ricerche                                          *
013800011204     C*----------------------------------------------------*
013900011204     c     ric01         begsr
014000011204     c                   movel     '1'           fld
014100011204     c*
014200011204     C                   SELECT
014300011204     C*CATEGORIA
014400011204     C                   when      §fld = 'VCAT'
014500011204     c                   exsr      srvcat
014600160517     C*marca
014700160517     C                   when      §fld = 'VCAUT'
014800160517     c                   clear                   tibs02ds
014900160517     c                   move      'R'           t02mod
015000160517     c                   exsr      srvcaut
015100160517     C*modello
015200160517     C                   when      §fld = 'VMAU'
015300160517     c                   clear                   tibs02ds
015400160517     c                   move      'R'           t02mod
015500160517     c                   exsr      srvmau
015600011204     C*CODICE FILIALE
015700011204     C                   when      §fld = 'VFIL'
015800011204     C                   MOVEL     *BLANKS       §COD1
015900011204     C                   MOVE      *BLANKS       §DES1
016000011204     C                   CALL      'TNSD19R'
016100011204     C                   PARM                    §COD1             3
016200011204     C                   PARM                    §TIP              1
016300011204     C                   PARM                    §DES1            25
016400011212     c                   if        §cod1 <> *blanks
016500011204     C                   MOVEL     §COD1         VFIL                           *CODICE
016600011204     C                   MOVEL     §DES1         VDFIL                          *DESCRIZIONE
016700011212     c                   end
016800011204     C*CODICE DITTA
016900011204     C                   when      §fld = 'VDIT'
017000011204     C                   reset                   ana007ds
017100011204     C                   eval      opz007 = '01'
017200011204     C                   eval      kpjbu = ana007ds
017300011204     C                   call      'ANA007R'
017400011204     C                   parm                    kpjba
017500011204     C                   eval      ana007ds = kpjbu
017600011204     C                   if        err007 <> '1' and soc007 <> *blanks
017700011204     C                   eval      vdit = soc007
017800011204     C                   endif
017900011204     C*CODICE CDC
018000011204     C                   when      §fld = 'VCDC'
018100011204    >C                   RESET                   ANA032DS
018200011204    >C                   MOVEL     '01'          OPZ032
018300011204     C                   MOVE      'S'           FSC032
018400011204     C                   MOVE      'CDC'         TPE032
018500011204    >C                   MOVEL     ANA032Ds      KPJBU
018600011204    >C                   CALL      'ANA032R'
018700011204     C                   PARM                    KPJBA
018800011204    >C                   MOVEL     KPJBU         ANA032DS
018900011212     c                   if        ent032 <> *blanks
019000011204     C                   MOVEL     ENT032        VCDC
019100011212     c                   end
019200011204     C                   ENDsl
019300011204     C* POSIZIONAMENTO CURSORE
019400011204     C                   Z-ADD     §RIGA         §RIG
019500011204     C                   Z-ADD     §COLON        §COL
019600011204     c*
019700011204     c                   endsr
019800011204     C*----------------------------------------------------*
019900011204      *  Controlli                                         *
020000011204     C*----------------------------------------------------*
020100011204     c     ctr01         begsr
020200011204     c*
020300011204     c                   movel     ' '           fld               1
020400011204     c                   setoff                                       99
020500011204     c                   do
020600011213     C*PERIODO
020700011213     c                   if        *in86
020800011214     c                   if        meseV  <> 0 and annov = 0
020900011214     c                   seton                                        5299
021000011214     c                   leave
021100011214     c                   end
021200011214     c                   if        meseV  > 12
021300011213     c                   seton                                        5399
021400011213     c                   leave
021500011213     c                   end
021600011204     c                   end
021700011204     C*MESE/ANNO IMMATRICOLAZIONE
021800011204     C     vimm          IFne      0
021900011212     c                   movel     vimm          mese
022000011212     C     MESe          IFEQ      0
022100011212     C     MESe          ORGT      12
022200011212     C                   SETON                                        6299
022300011204     C                   leave
022400011204     C                   END
022500011212     C                   END
022600011204     C*CATEGORIA
022700011204     c                   if        vcat <> *blanks
022800011204     c                   exsr      srvcat
022900011204     C   99              leave
023000011204     C                   END
023100160517      *marca
023200160517     c                   clear                   vcautd
023300160517     c                   if        vcaut <> *blank
023400160517     c                   clear                   tibs02ds
023500160517     c                   move      'C'           t02mod
023600160517     c                   exsr      srvcaut
023700160517     C   99              leave
023800160517     c                   endif
023900160517      *modello
024000160517     c                   clear                   vmaud
024100160517     c                   if        vmau <> *blank
024200160517     c                   clear                   tibs02ds
024300160517     c                   move      'C'           t02mod
024400160517     c                   exsr      srvmau
024500160517     C   99              leave
024600160517     c                   endif
024700011204     C*CODICE DITTA
024800011204     C     VDIT          IFne      *blanks
024900011204     C     vdit          CHAIN     ansif01L
025000011204     C                   if        not %found(ansif01l)
025100011204     C                   SETON                                        8799
025200011204     C                   leave
025300011204     C                   END
025400011204     C                   END
025500011204     C*CODICE FILIALE
025600011212     C     VFIL          IFne      0
025700011204     c                   exsr      srvfil
025800011204     C   99              leave
025900011204     C                   END
026000011204     C*CODICE CDC
026100011204     C     Vcdc          IFne      *BLANKS
026200011204     c                   exsr      srvcdc
026300011204     C   99              leave
026400011204     C                   END
026500160808      * controllo date periodo revisione
026600160808      * dal
026700160808     c                   clear                   vdadug
026800160808     c                   if        vdadur > 0
026900160808     C                   MOVE      Vdadur        G02DAT
027000160808     C                   MOVEL     *BLANK        G02ERR
027100160808     C                   CALL      'XSRDA8'
027200160808     C                   PARM                    WLBDA8
027300160808     C     G02ERR        IFEQ      '1'
027400160808     C                   SETON                                        6399
027500160808     c                   leavesr
027600160808     C                   END
027700160808     c                   move      g02dat        vdadur
027800160808     c                   move      g02inv        vdadug
027900160808     c                   endif
028000160808      * dal
028100160808     c                   clear                   valdug
028200160808     c                   if        valdur > 0
028300160808     C                   MOVE      Valdur        G02DAT
028400160808     C                   MOVEL     *BLANK        G02ERR
028500160808     C                   CALL      'XSRDA8'
028600160808     C                   PARM                    WLBDA8
028700160808     C     G02ERR        IFEQ      '1'
028800160808     C                   SETON                                        6499
028900160808     c                   leavesr
029000160808     C                   END
029100160808     c                   move      g02dat        valdur
029200160808     c                   move      g02inv        valdug
029300160808     c                   endif
029400160808     c                   if        valdug < vdadug
029500160808     c                   seton                                        6499
029600160808     c                   leavesr
029700160808     c                   endif
029800160808      * controllo date periodo in uso
029900160808      * dal
030000160808     c                   clear                   vusodg
030100160808     c                   if        vusodal > 0
030200160808     C                   MOVE      Vusodal       G02DAT
030300160808     C                   MOVEL     *BLANK        G02ERR
030400160808     C                   CALL      'XSRDA8'
030500160808     C                   PARM                    WLBDA8
030600160808     C     G02ERR        IFEQ      '1'
030700160808     C                   SETON                                        6399
030800160808     c                   leavesr
030900160808     C                   END
031000160808     c                   move      g02dat        vusodal
031100160808     c                   move      g02inv        vusodg
031200160808     c                   endif
031300160808      * dal
031400160808     c                   clear                   vusoag
031500160808     c                   if        vusoal > 0
031600160808     C                   MOVE      Vusoal        G02DAT
031700160808     C                   MOVEL     *BLANK        G02ERR
031800160808     C                   CALL      'XSRDA8'
031900160808     C                   PARM                    WLBDA8
032000160808     C     G02ERR        IFEQ      '1'
032100160808     C                   SETON                                        6499
032200160808     c                   leavesr
032300160808     C                   END
032400160808     c                   move      g02dat        vusoal
032500160808     c                   move      g02inv        vusoag
032600160808     c                   endif
032700160808     c                   if        vusoag < vusodg
032800160808     c                   seton                                        6499
032900160808     c                   leavesr
033000160808     c                   endif
033100160808     c* ORDINA SCHIERA KEY IN ORDINE DEI CAMPI VSE01-11 IMPOSTATI
033200011204     c* SE TROVA 2 NUMERI UGUALI ERRORE                    *
033300011204     c* SE GLI ORDINAMENTI SONO MENO DI 5 RIEMPIE CON AMENRF
033400011204     C                   Z-ADD     1             Y                 3 0          *
033500011204     C                   MOVE      'AMENRF'      KEY                            *
033600011204     C                   DO        5             I                 1 0          *
033700011204     C                   Z-ADD     1             S                 3 0          *
033800011204     C     I             LOOKUP    SEL(S)                                 21    *
033900011204     C   21              DO                                                     *
034000011204     C                   MOVE      CMP(S)        KEY(Y)                         *
034100011204     C                   ADD       1             Y                              *
034200011204     c* SE TARGA INSERISCO IL NUMERO                       *
034300011204     C     S             IFEQ      2                                            *
034400011204     C                   MOVE      'AMETGN'      KEY(Y)                         *
034500011204     C                   ADD       1             Y                              *
034600011204     C                   END                                                    *
034700011204     c* CONTROLLO NUMERO DOPPIO                            *
034800160808     C     S             IFLT      11                                           *
034900011204     C                   ADD       1             S                              *
035000011204     C     I             LOOKUP    SEL(S)                                 90    *
035100011204     c                   if        *in90
035200011204     C                   ADD       71            S                              *
035300011204     C                   MOVE      '1'           *IN(S)                         *
035400011204     c                   seton                                        99
035500011204     C                   leave                                                  *
035600011204     C                   END                                                    *
035700011204     C                   END                                                    *
035800011204     C                   ENDdo                                                  *
035900011204     c   99              leave
036000011204     C                   ENDdo                                                  *
036100011204     c* CONTROLLA CHE LA 'P' SIA SOLO NEL CAMPO CON ORDINAMENTO '1'
036200011204     C                   Z-ADD     1             S                 3 0          *
036300011204     C     'P'           LOOKUP    PAG(S)                                 21    *
036400011204     C   21SEL(S)        ifne      1                                            *
036500011204     c                   seton                                        9991
036600011204     c                   leave
036700011204     c                   end
036800160808     C   21S             IFNE      11
036900011204     C                   ADD       1             S
037000011204     C     'P'           LOOKUP    PAG(S)                                 21    *
037100011204     C   21SEL(S)        ifne      1                                            *
037200011204     c                   seton                                        9991
037300011204     c                   leave
037400011204     c                   end
037500011204     C                   END
037600011204     c* IMPOSTA IL SALTO AI LIVELLI SUPERIORI EVENTUALMENTE
037700011204     c* IN BIANCO
037800011204     C     -4            DO        -2            I                 1 0
037900011204     C                   Z-SUB     I             I
038000011204     C                   Z-ADD     1             S
038100011204     C     I             LOOKUP    SEL(S)                                 21
038200011204     C     *IN21         IFEQ      '1'
038300011204     C     PAG(S)        ANDNE     *BLANK
038400011204     C     I             SUB       1             Y
038500011204     C                   Z-SUB     Y             Y
038600011204     C     Y             DO        -1            I1                1 0
038700011204     C                   Z-SUB     I1            I1
038800011204     C                   Z-ADD     1             Z                 3 0
038900011204     C     I1            LOOKUP    SEL(Z)                                 22
039000011204     C   22PAG(Z)        IFEQ      *BLANK
039100011204     C                   MOVE      PAG(S)        PAG(Z)
039200011204     C                   ELSE
039300011204     C                   SETON                                            23
039400011204     C                   END
039500011204     C                   Z-SUB     I1            I1
039600011204     C  N23              END
039700011204     C                   END
039800011204     C                   Z-SUB     I             I
039900011204     C                   END
040000011204      *
040100011204     C     SEL(4)        IFNE      0
040200011204     C                   MOVE      PAG(4)        SCAT
040300011204     C                   END
040400011212     C     SEL(7)        IFNE      0
040500011212     C                   MOVE      PAG(7)        SDIT
040600011204     C                   END
040700011212     C     SEL(8)        IFNE      0
040800011212     C                   MOVE      PAG(8)        SFIL
040900011204     C                   END
041000011204     C                   enddo
041100011204     c                   endsr
041200160517     c**********************************************************************
041300160517     C** controllo marca
041400160517     c**********************************************************************
041500160517     C     srvcaut       BEGSR
041600160517     c*
041700160517     c                   if        vcaut <> *blank and t02mod <> 'R'
041800160517     c                   movel     vcaut         t02ke1
041900160517     c                   endif
042000160517     c                   movel     knsif         t02sif
042100160517     c                   movel     'CAU'         t02cod
042200160517     c                   call      'TIBS02R'
042300160517     c                   parm                    KPJBA
042400160517     c                   parm                    TIBS02DS
042500160517     c                   if        t02err =  *blanks
042600160517     c                   movel     t02uni        dcau
042700160517     c                   movel     t02ke1        vcaut
042800160517     c                   movel     §caudes       vcautd
042900160517     c                   else
043000160517     c                   seton                                        6799
043100160517     c                   endif
043200160517     C                   ENDSR
043300160517     c**********************************************************************
043400160517     C** controllo modello
043500160517     c**********************************************************************
043600160517     C     srvmau        BEGSR
043700160517     c*
043800160517     c                   if        vmau <> *blank and t02mod <> 'R'
043900160517     c                   movel     vmau          t02ke1
044000160517     c                   endif
044100160517     c                   movel     knsif         t02sif
044200160517     c                   movel     'MAU'         t02cod
044300160517     c                   call      'TIBS02R'
044400160517     c                   parm                    KPJBA
044500160517     c                   parm                    TIBS02DS
044600160517     c                   if        t02err =  *blanks
044700160517     c                   movel     t02uni        dmau
044800160517     c                   movel     t02ke1        vmau
044900160517     c                   movel     §maudes       vmaud
045000160517     c                   else
045100160517     c                   seton                                        6899
045200160517     c                   endif
045300160517     c*
045400160517     C                   ENDSR
045500011204     c**********************************************************************
045600011204     C** FILIALE
045700011204     c**********************************************************************
045800011204     C     srvfil        BEGSR
045900011204     c*
046000011212     C     vfil          CHAIN     AZORG
046100011204     C                   if        not %found(azorg01l)
046200011204     C                   SETON                                        8899
046300011204     c                   else
046400011204     C                   MOVEL     ORGDES        VDFIL
046500011204     C                   END
046600011204     c*
046700011204     C                   ENDSR
046800011204     c**********************************************************************
046900011204     C** centro di costo
047000011204     c**********************************************************************
047100011204     C     srvcdc        BEGSR
047200011204     c*
047300011204     C                   movel     *blanks       VDCDC
047400011204     C                   movel     VCDC          ententita
047500011204     C     kent          chain     anent01l
047600011204     C                   IF        NOT %FOUND(ANENT01L)
047700011204     C                   SETON                                        6199
047800011204     C                   ELSE
047900011204     C                   move      entdesbrev    VDCDC
048000011204     C                   END
048100011204     c*
048200011204     C                   ENDSR
048300011204     c**********************************************************************
048400011204     C** categoria
048500011204     c**********************************************************************
048600011204     C     srvcat        BEGSR
048700050526     c*  scelta multipla
048800050526     c                   if        %subst(vcat: 2: 1) <>'*'
048900011204     c                   movel     'XA'          parcod            2
049000011204     c                   movel(P)  VCAT          parkey
049100011204     c                   exsr      srtab
049200011204     c                   movel     parkey        vcat
049300011204     c                   if        *in99
049400011204     C                   MOVEL     *blanks       VDCAT
049500011204     C                   SETON                                        85
049600011204     C                   else
049700011204     C                   MOVEL     TBLUNI        VDCAT
049800011204     C                   END
049900050526     C                   END
050000011204     c*
050100011204     C                   ENDSR
050200011204     C**********************************************************************
050300011204     C** Controlli e decodifiche campi con tabel00f
050400011204     C**********************************************************************
050500011204     C     srtab         BEGSR
050600011204     c                   if        fld <> *blanks
050700011204     c                   movel     parkey        parkeys           8
050800011204     C                   CALL      'TRUL19R'
050900011204     c                   parm                    parcod            2
051000011204     c                   parm      '1'           parord            1
051100011204     c                   parm      *blanks       parkey            8
051200011204     c                   parm      *blanks       cmd               1
051300011204     c* ritorno senza scelta
051400011204     c                   if        cmd = 'L'
051500011204     c* in aggiornamento non è possibile cambiare l'utista
051600011204     c                             or (§fld = 'VCAU' and *in84)
051700011204     c                   movel     parkeys       parkey
051800011204     C                   END
051900011204     C                   END
052000011204     C                   MOVEL     *blanks       tbluni
052100011204     C                   MOVEL     parcod        COD
052200011212     C                   MOVEL(p)  parkey        KEYT
052300011204     C     KTAB          CHAIN     TABEL                              99
052400011204     c*
052500011204     C                   ENDSR
052600011204     c**********************************************************************
052700011204     c* pulisce 1° formato
052800011204     c**********************************************************************
052900011204     C     clr00         BEGSR
053000011204     C                   Z-ADD     0             §RIGA
053100011204     C                   Z-ADD     0             §COLON
053200011204     C                   CLEAR                   §FLD
053300011204     C                   ENDSR
053400011204     C*----------------------------------------------------*
053500011204      *  CARICO COSTANTI                                   *
053600011204     C*----------------------------------------------------*
053700011204     c     *inzsr        begsr
053800011204     C     *ENTRY        PLIST                                                  *
053900011204     C                   PARM                    KPJBA                          *
054000011204RE   C                   PARM                    TIPO             15            *
054100011204     C                   Z-ADD     1             CODUT                          *
054200011204     C                   MOVE      KPJBU         PARAM
054300011213     C                   Z-ADD     0             ANNOV
054400011213     C                   Z-ADD     0             MESEV
054500011204     C                   Z-ADD     0             SEL
054600011204     C                   Z-ADD     0             VIMM
054700011212     C                   z-add     0             Vfil
054800011212     C                   movel     *blanks       VCDC
054900011212     C                   movel     *blanks       Vdit
055000011212      * Reperimento dati società
055100011212     C                   clear                   SocXsc
055200011212     C                   eval      tipxsc = 'SOC001'
055300011212     C                   exsr      rtvsocieta
055400011212     C                   if        rtnxsc = '1'
055500011212     C                   seton                                        lr
055600011212     C                   else
055700011212     C                   MOVEL     XSOCDS        xsoc001ds
055800011212     C                   endif
055900160901      * accensione avviso per generazione file da scaricare su excel
056000160901     c                   if        kcdaz = 'MZ03'
056100160901     C                   seton                                        98
056200160901     c                   endif
056300011212     C* POSIZIONAMENTO SU ANENT                            *
056400011212     C     Kent          KLIST
056500011212     C                   KFLD                    xscsoc
056600011212     C                   KFLD                    enttprifan
056700011212     C                   KFLD                    ententita
056800011212     C                   move      'CDC'         enttprifan
056900011212     C* POSIZIONAMENTO TABELLE                             *
057000011212     C     KTAB          KLIST                                                  *
057100011212     C                   KFLD                    CODUT                          *
057200011212     C                   KFLD                    COD               2            *
057300011212     C                   KFLD                    KEYT              8            *
057400011204      *
057500011204     C                   CALL      'XPARUT'                                     *
057600011204     C                   PARM                    PARUT                          *
057700011204     C*--------- SE DITTA INESISTENTE FINE LAVORO          *
057800011204     C     CODUT         ifeq      -1                                           *
057900011204     c                   seton                                        lr
058000011204     c                   end
058100011204     C                   MOVEL     RAGUT         RSUT             20              *
058200011204     c                   endsr
058300011212     C*---------------------------------------------------------------------
058400011212     c*  Reperisco società.
058500011212     C*---------------------------------------------------------------------
058600011212     C     rtvsocieta    begsr
058700011212     C*
058800011212     C                   CALLb     'XSOC'
058900011212   >>C                   Parm                    TipXsc            6
059000011212   >>C                   Parm                    SocXsc            3
059100011212   >>C                   Parm                    CdsXsc            9 0
059200011212   >>C                   Parm                    ModXsc            3
059300011212   >>C                   Parm      *BLANK        RtnXsc            1
059400011212     C                   Parm                    XsocDS
059500011212   >>C                   Parm                    Kpjba
059600011212     C*
059700011212     C                   endsr
059800011204     C*----------------------------------------------------*
059900160809**   CMP
060000160809AMENRF
060100160809AMETGA
060200160809AMEIMM
060300160809AMECAT
060400160809AMECAUT
060500160809AMEMAU
060600160809AMEDIT
060700160809AMEFIL
060800160809AMECDC
060900160809AMEDUR
061000160809AMEDAL
