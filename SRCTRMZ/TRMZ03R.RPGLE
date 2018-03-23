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
004700170222     D  VfileSt                       1
004800170227     D  ParamSOC                      3
004900000000     D                 DS
005000160808     D  SEL                    1     11  0 dim(11)
005100000000     D  VSEL01                 1      1  0
005200000000     D  VSEL02                 2      2  0
005300000000     D  VSEL03                 3      3  0
005400000000     D  VSEL04                 4      4  0
005500000000     D  VSEL05                 5      5  0
005600000000     D  VSEL06                 6      6  0
005700000000     D  VSEL07                 7      7  0
005800000000     D  VSEL08                 8      8  0
005900000000     D  VSEL09                 9      9  0
006000160808     D  VSEL10                10     10  0
006100160808     D  VSEL11                11     11  0
006200000000     D                 DS
006300160808     D  PAG                    1     11
006400160808     D                                     DIM(11)                              SALTO PAGINA
006500000000     D  VSCAT                  4      4
006600011204     D  VSDIT                  7      7
006700011204     D  VSFIL                  8      8
006800160808     D*-------------
006900160808     D WLBDA8          DS
007000160808     D  G02DAT                 1      8  0
007100160808     D  G02INV                 9     16  0
007200160808     D  G02ERR                17     17
007300160808     D  G02TGI                18     22  0
007400170222      *
007500170222      *?V A R I A B I L I?--------------------------------------------*
007600170222      *
007700170222      * Parametri x QCMDEXC
007800170222     d Qcmd            s            100    inz
007900170222     d Qlen            s             15  5 inz(100)
008000000000     C*----------------------------------------------------*
008100000000      * INDICATORI
008200000000      *
008300000000      * 73-87  ORDINAMENTO GIA SCELTO
008400000000      * 91     SALTO 'P' AMMESSO SOLO PER ORDINAMENTO 1
008500000000      * 21
008600000000     C*----------------------------------------------------*
008700011204     C  nlr              do        *hival                                       *
008800011214     C                   IF        KCDAZ = 'MZ18'
008900011213     C                   SETON                                            86
009000011213     C                   END
009100000000     C                   EXFMT     GA03D001                                      *
009200011204     c*--- FINE LAVORO                                     *
009300011204     c                   if        *inkc
009400011212     C                   MOVE      '1'           KPJBU
009500011204     C                   leave                                                  *
009600011204     c                   end
009700011204     c* ricerca
009800011204     c                   if        *inkd
009900011204     c                   exsr      ric01
010000011204     c                   iter
010100011204     c                   end
010200011204     c* controlli
010300011204     c                   exsr      ctr01
010400011204     c   99              iter
010500011204     c* IMPOSTA PARAMETRI                                  *
010600011204     c                   if        *inkf
010700011212     C                   move      VIMM          anno              2 0
010800011212     C                   movel     vimm          mese              2 0
010900011212     C                   movel     ANNO          comodo            4 0
011000011212     C                   move      mese          comodo
011100011212     C                   move      comodo        VIMM
011200170227     c                   eval      ParamSOC = XSCsoc
011300011204     C                   MOVEA     KEY           KEYFLD                         *
011400011212     C                   MOVEl(p)  PARAM         KPJBU
011500011213     c                   select
011600011213     c                   when      kcdaz = 'MZ03'
011700170119     c                   eval      KJOST = 'H'
011800011213     C                   MOVEL     'MZ04'        KCOAZ
011900011213     c                   when      kcdaz = 'MZ33'
012000011213     C                   MOVEL     'MZ34'        KCOAZ
012100011213     c                   when      kcdaz = 'MZ18'
012200011213     C                   MOVEL     'MZ19'        KCOAZ
012300011213     c                   endsl
012400011213     C                   CALL      'BCH09'
012500011213     C                   PARM                    KPJBA
012600011213     C     KNMUS         IFEQ      *ALL'1'
012700011213     c                   select
012800011213     c                   when      kcdaz = 'MZ03'
012900011213     C                   CALL      'TRMZ04R1'
013000011213     C                   PARM                    KPJBA
013100011213     c                   when      kcdaz = 'MZ33'
013200011213     C                   CALL      'TRMZ34R'
013300011213     C                   PARM                    KPJBA
013400011213     c                   when      kcdaz = 'MZ18'
013500011213     C                   CALL      'TRMZ19R'
013600011213     C                   PARM                    KPJBA
013700011213     c                   endsl
013800011213     C                   ELSE
013900011213     C                   CALL      'BCH10'
014000011213     C                   PARM                    KPJBA
014100011213     C                   END
014200011204     c                   leave
014300011204     c                   end
014400011204     C                   enddo                                                  *
014500000000     C                   SETON                                          LR      *
014600011204     C*----------------------------------------------------*
014700011204      *  Ricerche                                          *
014800011204     C*----------------------------------------------------*
014900011204     c     ric01         begsr
015000011204     c                   movel     '1'           fld
015100011204     c*
015200011204     C                   SELECT
015300011204     C*CATEGORIA
015400011204     C                   when      §fld = 'VCAT'
015500011204     c                   exsr      srvcat
015600160517     C*marca
015700160517     C                   when      §fld = 'VCAUT'
015800160517     c                   clear                   tibs02ds
015900160517     c                   move      'R'           t02mod
016000160517     c                   exsr      srvcaut
016100160517     C*modello
016200160517     C                   when      §fld = 'VMAU'
016300160517     c                   clear                   tibs02ds
016400160517     c                   move      'R'           t02mod
016500160517     c                   exsr      srvmau
016600011204     C*CODICE FILIALE
016700011204     C                   when      §fld = 'VFIL'
016800011204     C                   MOVEL     *BLANKS       §COD1
016900011204     C                   MOVE      *BLANKS       §DES1
017000011204     C                   CALL      'TNSD19R'
017100011204     C                   PARM                    §COD1             3
017200011204     C                   PARM                    §TIP              1
017300011204     C                   PARM                    §DES1            25
017400011212     c                   if        §cod1 <> *blanks
017500011204     C                   MOVEL     §COD1         VFIL                           *CODICE
017600011204     C                   MOVEL     §DES1         VDFIL                          *DESCRIZIONE
017700011212     c                   end
017800011204     C*CODICE DITTA
017900011204     C                   when      §fld = 'VDIT'
018000011204     C                   reset                   ana007ds
018100011204     C                   eval      opz007 = '01'
018200011204     C                   eval      kpjbu = ana007ds
018300011204     C                   call      'ANA007R'
018400011204     C                   parm                    kpjba
018500011204     C                   eval      ana007ds = kpjbu
018600011204     C                   if        err007 <> '1' and soc007 <> *blanks
018700011204     C                   eval      vdit = soc007
018800011204     C                   endif
018900011204     C*CODICE CDC
019000011204     C                   when      §fld = 'VCDC'
019100011204    >C                   RESET                   ANA032DS
019200011204    >C                   MOVEL     '01'          OPZ032
019300011204     C                   MOVE      'S'           FSC032
019400011204     C                   MOVE      'CDC'         TPE032
019500011204    >C                   MOVEL     ANA032Ds      KPJBU
019600011204    >C                   CALL      'ANA032R'
019700011204     C                   PARM                    KPJBA
019800011204    >C                   MOVEL     KPJBU         ANA032DS
019900011212     c                   if        ent032 <> *blanks
020000011204     C                   MOVEL     ENT032        VCDC
020100011212     c                   end
020200011204     C                   ENDsl
020300011204     C* POSIZIONAMENTO CURSORE
020400011204     C                   Z-ADD     §RIGA         §RIG
020500011204     C                   Z-ADD     §COLON        §COL
020600011204     c*
020700011204     c                   endsr
020800011204     C*----------------------------------------------------*
020900011204      *  Controlli                                         *
021000011204     C*----------------------------------------------------*
021100011204     c     ctr01         begsr
021200011204     c*
021300011204     c                   movel     ' '           fld               1
021400011204     c                   setoff                                       99
021500011204     c                   do
021600011213     C*PERIODO
021700011213     c                   if        *in86
021800011214     c                   if        meseV  <> 0 and annov = 0
021900011214     c                   seton                                        5299
022000011214     c                   leave
022100011214     c                   end
022200011214     c                   if        meseV  > 12
022300011213     c                   seton                                        5399
022400011213     c                   leave
022500011213     c                   end
022600011204     c                   end
022700011204     C*MESE/ANNO IMMATRICOLAZIONE
022800011204     C     vimm          IFne      0
022900011212     c                   movel     vimm          mese
023000011212     C     MESe          IFEQ      0
023100011212     C     MESe          ORGT      12
023200011212     C                   SETON                                        6299
023300011204     C                   leave
023400011204     C                   END
023500011212     C                   END
023600011204     C*CATEGORIA
023700011204     c                   if        vcat <> *blanks
023800011204     c                   exsr      srvcat
023900011204     C   99              leave
024000011204     C                   END
024100160517      *marca
024200160517     c                   clear                   vcautd
024300160517     c                   if        vcaut <> *blank
024400160517     c                   clear                   tibs02ds
024500160517     c                   move      'C'           t02mod
024600160517     c                   exsr      srvcaut
024700160517     C   99              leave
024800160517     c                   endif
024900160517      *modello
025000160517     c                   clear                   vmaud
025100160517     c                   if        vmau <> *blank
025200160517     c                   clear                   tibs02ds
025300160517     c                   move      'C'           t02mod
025400160517     c                   exsr      srvmau
025500160517     C   99              leave
025600160517     c                   endif
025700011204     C*CODICE DITTA
025800011204     C     VDIT          IFne      *blanks
025900011204     C     vdit          CHAIN     ansif01L
026000011204     C                   if        not %found(ansif01l)
026100011204     C                   SETON                                        8799
026200011204     C                   leave
026300011204     C                   END
026400011204     C                   END
026500011204     C*CODICE FILIALE
026600011212     C     VFIL          IFne      0
026700011204     c                   exsr      srvfil
026800011204     C   99              leave
026900011204     C                   END
027000011204     C*CODICE CDC
027100011204     C     Vcdc          IFne      *BLANKS
027200011204     c                   exsr      srvcdc
027300011204     C   99              leave
027400011204     C                   END
027500160808      * controllo date periodo revisione
027600160808      * dal
027700160808     c                   clear                   vdadug
027800160808     c                   if        vdadur > 0
027900160808     C                   MOVE      Vdadur        G02DAT
028000160808     C                   MOVEL     *BLANK        G02ERR
028100160808     C                   CALL      'XSRDA8'
028200160808     C                   PARM                    WLBDA8
028300160808     C     G02ERR        IFEQ      '1'
028400160808     C                   SETON                                        6399
028500160808     c                   leavesr
028600160808     C                   END
028700160808     c                   move      g02dat        vdadur
028800160808     c                   move      g02inv        vdadug
028900160808     c                   endif
029000160808      * dal
029100160808     c                   clear                   valdug
029200160808     c                   if        valdur > 0
029300160808     C                   MOVE      Valdur        G02DAT
029400160808     C                   MOVEL     *BLANK        G02ERR
029500160808     C                   CALL      'XSRDA8'
029600160808     C                   PARM                    WLBDA8
029700160808     C     G02ERR        IFEQ      '1'
029800160808     C                   SETON                                        6499
029900160808     c                   leavesr
030000160808     C                   END
030100160808     c                   move      g02dat        valdur
030200160808     c                   move      g02inv        valdug
030300160808     c                   endif
030400160808     c                   if        valdug < vdadug
030500160808     c                   seton                                        6499
030600160808     c                   leavesr
030700160808     c                   endif
030800160808      * controllo date periodo in uso
030900160808      * dal
031000160808     c                   clear                   vusodg
031100160808     c                   if        vusodal > 0
031200160808     C                   MOVE      Vusodal       G02DAT
031300160808     C                   MOVEL     *BLANK        G02ERR
031400160808     C                   CALL      'XSRDA8'
031500160808     C                   PARM                    WLBDA8
031600160808     C     G02ERR        IFEQ      '1'
031700160808     C                   SETON                                        6399
031800160808     c                   leavesr
031900160808     C                   END
032000160808     c                   move      g02dat        vusodal
032100160808     c                   move      g02inv        vusodg
032200160808     c                   endif
032300160808      * dal
032400160808     c                   clear                   vusoag
032500160808     c                   if        vusoal > 0
032600160808     C                   MOVE      Vusoal        G02DAT
032700160808     C                   MOVEL     *BLANK        G02ERR
032800160808     C                   CALL      'XSRDA8'
032900160808     C                   PARM                    WLBDA8
033000160808     C     G02ERR        IFEQ      '1'
033100160808     C                   SETON                                        6499
033200160808     c                   leavesr
033300160808     C                   END
033400160808     c                   move      g02dat        vusoal
033500160808     c                   move      g02inv        vusoag
033600160808     c                   endif
033700160808     c                   if        vusoag < vusodg
033800160808     c                   seton                                        6499
033900160808     c                   leavesr
034000160808     c                   endif
034100160808     c* ORDINA SCHIERA KEY IN ORDINE DEI CAMPI VSE01-11 IMPOSTATI
034200011204     c* SE TROVA 2 NUMERI UGUALI ERRORE                    *
034300011204     c* SE GLI ORDINAMENTI SONO MENO DI 5 RIEMPIE CON AMENRF
034400011204     C                   Z-ADD     1             Y                 3 0          *
034500011204     C                   MOVE      'AMENRF'      KEY                            *
034600011204     C                   DO        5             I                 1 0          *
034700011204     C                   Z-ADD     1             S                 3 0          *
034800011204     C     I             LOOKUP    SEL(S)                                 21    *
034900011204     C   21              DO                                                     *
035000011204     C                   MOVE      CMP(S)        KEY(Y)                         *
035100011204     C                   ADD       1             Y                              *
035200011204     c* SE TARGA INSERISCO IL NUMERO                       *
035300011204     C     S             IFEQ      2                                            *
035400011204     C                   MOVE      'AMETGN'      KEY(Y)                         *
035500011204     C                   ADD       1             Y                              *
035600011204     C                   END                                                    *
035700011204     c* CONTROLLO NUMERO DOPPIO                            *
035800160808     C     S             IFLT      11                                           *
035900011204     C                   ADD       1             S                              *
036000011204     C     I             LOOKUP    SEL(S)                                 90    *
036100011204     c                   if        *in90
036200011204     C                   ADD       71            S                              *
036300011204     C                   MOVE      '1'           *IN(S)                         *
036400011204     c                   seton                                        99
036500011204     C                   leave                                                  *
036600011204     C                   END                                                    *
036700011204     C                   END                                                    *
036800011204     C                   ENDdo                                                  *
036900011204     c   99              leave
037000011204     C                   ENDdo                                                  *
037100011204     c* CONTROLLA CHE LA 'P' SIA SOLO NEL CAMPO CON ORDINAMENTO '1'
037200011204     C                   Z-ADD     1             S                 3 0          *
037300011204     C     'P'           LOOKUP    PAG(S)                                 21    *
037400011204     C   21SEL(S)        ifne      1                                            *
037500011204     c                   seton                                        9991
037600011204     c                   leave
037700011204     c                   end
037800160808     C   21S             IFNE      11
037900011204     C                   ADD       1             S
038000011204     C     'P'           LOOKUP    PAG(S)                                 21    *
038100011204     C   21SEL(S)        ifne      1                                            *
038200011204     c                   seton                                        9991
038300011204     c                   leave
038400011204     c                   end
038500011204     C                   END
038600011204     c* IMPOSTA IL SALTO AI LIVELLI SUPERIORI EVENTUALMENTE
038700011204     c* IN BIANCO
038800011204     C     -4            DO        -2            I                 1 0
038900011204     C                   Z-SUB     I             I
039000011204     C                   Z-ADD     1             S
039100011204     C     I             LOOKUP    SEL(S)                                 21
039200011204     C     *IN21         IFEQ      '1'
039300011204     C     PAG(S)        ANDNE     *BLANK
039400011204     C     I             SUB       1             Y
039500011204     C                   Z-SUB     Y             Y
039600011204     C     Y             DO        -1            I1                1 0
039700011204     C                   Z-SUB     I1            I1
039800011204     C                   Z-ADD     1             Z                 3 0
039900011204     C     I1            LOOKUP    SEL(Z)                                 22
040000011204     C   22PAG(Z)        IFEQ      *BLANK
040100011204     C                   MOVE      PAG(S)        PAG(Z)
040200011204     C                   ELSE
040300011204     C                   SETON                                            23
040400011204     C                   END
040500011204     C                   Z-SUB     I1            I1
040600011204     C  N23              END
040700011204     C                   END
040800011204     C                   Z-SUB     I             I
040900011204     C                   END
041000011204      *
041100011204     C     SEL(4)        IFNE      0
041200011204     C                   MOVE      PAG(4)        SCAT
041300011204     C                   END
041400011212     C     SEL(7)        IFNE      0
041500011212     C                   MOVE      PAG(7)        SDIT
041600011204     C                   END
041700011212     C     SEL(8)        IFNE      0
041800011212     C                   MOVE      PAG(8)        SFIL
041900011204     C                   END
042000011204     C                   enddo
042100011204     c                   endsr
042200160517     c**********************************************************************
042300160517     C** controllo marca
042400160517     c**********************************************************************
042500160517     C     srvcaut       BEGSR
042600160517     c*
042700160517     c                   if        vcaut <> *blank and t02mod <> 'R'
042800160517     c                   movel     vcaut         t02ke1
042900160517     c                   endif
043000160517     c                   movel     knsif         t02sif
043100160517     c                   movel     'CAU'         t02cod
043200160517     c                   call      'TIBS02R'
043300160517     c                   parm                    KPJBA
043400160517     c                   parm                    TIBS02DS
043500160517     c                   if        t02err =  *blanks
043600160517     c                   movel     t02uni        dcau
043700160517     c                   movel     t02ke1        vcaut
043800160517     c                   movel     §caudes       vcautd
043900160517     c                   else
044000160517     c                   seton                                        6799
044100160517     c                   endif
044200160517     C                   ENDSR
044300160517     c**********************************************************************
044400160517     C** controllo modello
044500160517     c**********************************************************************
044600160517     C     srvmau        BEGSR
044700160517     c*
044800160517     c                   if        vmau <> *blank and t02mod <> 'R'
044900160517     c                   movel     vmau          t02ke1
045000160517     c                   endif
045100160517     c                   movel     knsif         t02sif
045200160517     c                   movel     'MAU'         t02cod
045300160517     c                   call      'TIBS02R'
045400160517     c                   parm                    KPJBA
045500160517     c                   parm                    TIBS02DS
045600160517     c                   if        t02err =  *blanks
045700160517     c                   movel     t02uni        dmau
045800160517     c                   movel     t02ke1        vmau
045900160517     c                   movel     §maudes       vmaud
046000160517     c                   else
046100160517     c                   seton                                        6899
046200160517     c                   endif
046300160517     c*
046400160517     C                   ENDSR
046500011204     c**********************************************************************
046600011204     C** FILIALE
046700011204     c**********************************************************************
046800011204     C     srvfil        BEGSR
046900011204     c*
047000011212     C     vfil          CHAIN     AZORG
047100011204     C                   if        not %found(azorg01l)
047200011204     C                   SETON                                        8899
047300011204     c                   else
047400011204     C                   MOVEL     ORGDES        VDFIL
047500011204     C                   END
047600011204     c*
047700011204     C                   ENDSR
047800011204     c**********************************************************************
047900011204     C** centro di costo
048000011204     c**********************************************************************
048100011204     C     srvcdc        BEGSR
048200011204     c*
048300011204     C                   movel     *blanks       VDCDC
048400011204     C                   movel     VCDC          ententita
048500011204     C     kent          chain     anent01l
048600011204     C                   IF        NOT %FOUND(ANENT01L)
048700011204     C                   SETON                                        6199
048800011204     C                   ELSE
048900011204     C                   move      entdesbrev    VDCDC
049000011204     C                   END
049100011204     c*
049200011204     C                   ENDSR
049300011204     c**********************************************************************
049400011204     C** categoria
049500011204     c**********************************************************************
049600011204     C     srvcat        BEGSR
049700050526     c*  scelta multipla
049800050526     c                   if        %subst(vcat: 2: 1) <>'*'
049900011204     c                   movel     'XA'          parcod            2
050000011204     c                   movel(P)  VCAT          parkey
050100011204     c                   exsr      srtab
050200011204     c                   movel     parkey        vcat
050300011204     c                   if        *in99
050400011204     C                   MOVEL     *blanks       VDCAT
050500011204     C                   SETON                                        85
050600011204     C                   else
050700011204     C                   MOVEL     TBLUNI        VDCAT
050800011204     C                   END
050900050526     C                   END
051000011204     c*
051100011204     C                   ENDSR
051200011204     C**********************************************************************
051300011204     C** Controlli e decodifiche campi con tabel00f
051400011204     C**********************************************************************
051500011204     C     srtab         BEGSR
051600011204     c                   if        fld <> *blanks
051700011204     c                   movel     parkey        parkeys           8
051800011204     C                   CALL      'TRUL19R'
051900011204     c                   parm                    parcod            2
052000011204     c                   parm      '1'           parord            1
052100011204     c                   parm      *blanks       parkey            8
052200011204     c                   parm      *blanks       cmd               1
052300011204     c* ritorno senza scelta
052400011204     c                   if        cmd = 'L'
052500011204     c* in aggiornamento non è possibile cambiare l'utista
052600011204     c                             or (§fld = 'VCAU' and *in84)
052700011204     c                   movel     parkeys       parkey
052800011204     C                   END
052900011204     C                   END
053000011204     C                   MOVEL     *blanks       tbluni
053100011204     C                   MOVEL     parcod        COD
053200011212     C                   MOVEL(p)  parkey        KEYT
053300011204     C     KTAB          CHAIN     TABEL                              99
053400011204     c*
053500011204     C                   ENDSR
053600011204     c**********************************************************************
053700011204     c* pulisce 1° formato
053800011204     c**********************************************************************
053900011204     C     clr00         BEGSR
054000011204     C                   Z-ADD     0             §RIGA
054100011204     C                   Z-ADD     0             §COLON
054200011204     C                   CLEAR                   §FLD
054300011204     C                   ENDSR
054400011204     C*----------------------------------------------------*
054500011204      *  CARICO COSTANTI                                   *
054600011204     C*----------------------------------------------------*
054700011204     c     *inzsr        begsr
054800011204     C     *ENTRY        PLIST                                                  *
054900011204     C                   PARM                    KPJBA                          *
055000011204RE   C                   PARM                    TIPO             15            *
055100170222      *
055200011204     C                   Z-ADD     1             CODUT                          *
055300011204     C                   MOVE      KPJBU         PARAM
055400011213     C                   Z-ADD     0             ANNOV
055500011213     C                   Z-ADD     0             MESEV
055600011204     C                   Z-ADD     0             SEL
055700011204     C                   Z-ADD     0             VIMM
055800011212     C                   z-add     0             Vfil
055900011212     C                   movel     *blanks       VCDC
056000011212     C                   movel     *blanks       Vdit
056100170222     c                   clear                   VfileSt
056200170222     c                   if        KcdAz   = 'MZ03'
056300170222     c                   eval      VfileSt = 'F'
056400170222     c                   endif
056500011212      * Reperimento dati società
056600011212     C                   clear                   SocXsc
056700011212     C                   eval      tipxsc = 'SOC001'
056800011212     C                   exsr      rtvsocieta
056900011212     C                   if        rtnxsc = '1'
057000011212     C                   seton                                        lr
057100011212     C                   else
057200011212     C                   MOVEL     XSOCDS        xsoc001ds
057300011212     C                   endif
057400160901      * accensione avviso per generazione file da scaricare su excel
057500160901     c                   if        kcdaz = 'MZ03'
057600160901     C                   seton                                        98
057700160901     c                   endif
057800011212     C* POSIZIONAMENTO SU ANENT                            *
057900011212     C     Kent          KLIST
058000011212     C                   KFLD                    xscsoc
058100011212     C                   KFLD                    enttprifan
058200011212     C                   KFLD                    ententita
058300011212     C                   move      'CDC'         enttprifan
058400011212     C* POSIZIONAMENTO TABELLE                             *
058500011212     C     KTAB          KLIST                                                  *
058600011212     C                   KFLD                    CODUT                          *
058700011212     C                   KFLD                    COD               2            *
058800011212     C                   KFLD                    KEYT              8            *
058900011204      *
059000011204     C                   CALL      'XPARUT'                                     *
059100011204     C                   PARM                    PARUT                          *
059200011204     C*--------- SE DITTA INESISTENTE FINE LAVORO          *
059300011204     C     CODUT         ifeq      -1                                           *
059400011204     c                   seton                                        lr
059500011204     c                   end
059600011204     C                   MOVEL     RAGUT         RSUT             20              *
059700011204     c                   endsr
059800011212     C*---------------------------------------------------------------------
059900011212     c*  Reperisco società.
060000011212     C*---------------------------------------------------------------------
060100011212     C     rtvsocieta    begsr
060200011212     C*
060300011212     C                   CALLb     'XSOC'
060400011212   >>C                   Parm                    TipXsc            6
060500011212   >>C                   Parm                    SocXsc            3
060600011212   >>C                   Parm                    CdsXsc            9 0
060700011212   >>C                   Parm                    ModXsc            3
060800011212   >>C                   Parm      *BLANK        RtnXsc            1
060900011212     C                   Parm                    XsocDS
061000011212   >>C                   Parm                    Kpjba
061100011212     C*
061200011212     C                   endsr
061300011204     C*----------------------------------------------------*
061400160809**   CMP
061500160809AMENRF
061600160809AMETGA
061700160809AMEIMM
061800160809AMECAT
061900160809AMECAUT
062000160809AMEMAU
062100160809AMEDIT
062200160809AMEFIL
062300160809AMECDC
062400160809AMEDUR
062500160809AMEDAL
