000100090112     H DECEDIT('0,') DATEDIT(*DMY.) DFTACTGRP(*NO) ACTGRP(*CALLER)
000200011030     H* ficn84R *----------------------------------------------------*
000300000000     H*-----------------*                                            *
000400090109     H*              DUPLICAZIONE TARIFFA aut supertestata           *
000500000000     H*--------------------------------------------------------------*
000600090109     Fficn85D   CF   E             WORKSTN
000700090112     F                                     SFILE(CN10S02:NRR)
000800090112     FfiTGT01L  IF   E           K DISK
000900090112     Ffifgt01L  IF   E           K DISK
001000931018     FTABEL00F  IF   E           K DISK
001100021203     Ffiapd01L  IF   E           K DISK
001200011109     Ftntbe01L  IF   E           K DISK
001300931018     D TTR             S              1    DIM(50)                              COD. TARIFFA
001400931018     D DTR             S             15    DIM(50)                              DESCR.COD. TARIFFA
001500931104     D TSR             S              1    DIM(50)                              TIPO PRESTAZIONE
001600931104     D DSR             S             15    DIM(50)                              DESCR.TIPO PRESTAZIO
001700090112     D* SCHIERA DI COMODO
001800090112     D SER             S              1    DIM(12)                              COMODO X TIPO PRESTA
001900140117      *--
002000140117      ****-?
002100140117     D**** TRUL72DS      E DS
002200140117     D****  L72                           3s 0 DESCEND
002300140117     D****                                     DIM(60)
002400140117     D****                                     OVERLAY(O72Lia)
002500140117      ****-?Caricamento filiali contabili
002600140117     d TRUL31ds      e ds                  inz
002700140117     d $POg            s              3    inz   dim(250)
002800140117      ****-?
002900140117      *--
003000950113     D* SCHIERE PER CARICAMENTO L1 E L6
003100950113     D L6              S              3  0 DIM(30)                              FIL GESTITE
003200090109     D parana          S              1    inz('A')                             FIL GESTITE
003300090114     D comtsr          S              1                                         FIL GESTITE
003400940922     D*
003500090112     d ficn06ds      e ds
003600931018     D*
003700021203     D fnlv24ds      E DS
003800931018     D*
003900000000     D KPJBA         E DS
004000940922     D*
004100090112     D                 DS
004200090112     D  AA                     1      4  0
004300090112     D  MM                     5      6  0
004400090112     D  GG                     7      8  0
004500090112     D  DATA                   1      8  0
004600940922     D* DS PER TRUL06R - CARICAMENTO £1
004700940922     D DSUL06        E DS                  EXTNAME(TRUL06DS)
004800940922     D  LIN                    1     90  0
004900940922     D                                     DIM(30)                              SKI COMODO
005000940922     D*
005100931018     D DS1Z          E DS
005200940922     D DSTR          E DS
005300011109     D Dgedcn        E DS
005400931018     D CNCR80        E DS
005500921030     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
005600011109     D WLBDAT          DS
005700011109     D  G02DAT                 1      8  0
005800011109     D  G02INV                 9     16  0
005900011109     D  G02ERR                17     17
006000011109     D  G02TGI                18     22  0
006100000000     I/SPACE 3
006200931018     C*****************************************************************
006300931018     C* RIEPILOGO INDICATORI
006400931018     C*****************************************************************
006500931018     C* 30    - DI COMODO
006600931018     C* 35    - USATO PER LOKUP SU SCHIERE E TESTN
006700940923     C* 40/52 - INDICATORI PER MESSAGGI DI ERRORE
006800020717     C* 89    - SONO UN 2° liv.
006900931018     C* 90    - SEGNALA LA PRESENZA DI ERRORI
007000090112     C*****************************************************************
007100931018     C*
007200931018     C                   EXSR      PULIZ
007300931018     C*
007400090112     C                   do        *hival
007500011030     C                   EXFMT     cn84D01
007600931018     C** CMD3 - FINE LAVORO
007700090112     C   KC              leave
007800940922     C*
007900931018     C* CONTROLLI FORMATO1
008000931018     C                   EXSR      CONTR
008100090112     C   90              iter
008200090112     C* scelta tariffe
008300090112     c                   exsr      video2
008400090112     c   kl              iter
008500090112     c   kf              leave
008600090112     c                   enddo
008700931018     C*
008800931018     C                   SETON                                        LR
008900090112     C**********************************************************************
009000931018     C*--- PULIZIA CAMPI DEL FORMATO ---------------------------------*
009100090112     C**********************************************************************
009200931018     C     PULIZ         BEGSR
009300931018     C                   MOVEL     *BLANKS       DPDNEW
009400931018     C                   MOVEL     *BLANKS       DPDOLD
009500931018     C                   ENDSR
009600090112     C**********************************************************************
009700931018     C*--- CONTROLLI FORMATO -----------------------------------------*
009800090112     C**********************************************************************
009900931018     C     CONTR         BEGSR
010000931018     C                   SETOFF                                       90
010100940922     C*
010200940922     C****  CODICE FILIALE NUOVA  ****
010300950113     C* FILIALE OBBLIGATORIA
010400950113     C     FILNEW        IFEQ      0
010500950113     C                   SETON                                        5190
010600950113     C                   GOTO      ENDCTR
010700950113     C                   ENDIF
010800950113     C*
010900950113     C* 89 OFF - SONO SIMFEL QUINDI CONTROLLO LA FILIALE NELLA L1
011000950113     C     *IN89         IFEQ      *OFF
011100021216     C     FILNEW        IFNE      SIMpou
011200021216     C     simtpp        andeq     '1'
011300140117     C**** FILNEW        LOOKUP    L72                                    35
011400140117     C                   move      filNEW        Fil03A            3
011500140117     C     Fil03A        LOOKUP    $POg                                   35
011600940922     C  N35              SETON                                        5190
011700940922     C  N35              GOTO      ENDCTR
011800940922     C                   ENDIF
011900931018     C*
012000950113     C                   ELSE
012100020717     C* 89 ON  - SONO UN 2° liv.  QUINDI CONTROLLO LA FILIALE NELLA L6
012200020717     C     filnew        IFNE      simpou
012300950113     C     FILNEW        LOOKUP    L6                                     35
012400950113     C  N35              SETON                                        5190
012500950113     C  N35              GOTO      ENDCTR
012600950113     C                   ENDIF
012700950113     C                   ENDIF
012800950113     C*
012900931018     C****  CODICE PADRONCINO NUOVO  ****
013000931018     C     '?'           SCAN      PDRNEW                                 90
013100931018     C*
013200931018     C     *IN90         IFEQ      *ON
013300021203     c                   clear                   fnlv24ds
013400021203     C                   Z-ADD     FILNEW        d24FIL
013500021203     C                   MOVEL     'R'           d24FLG
013600030107     C                   MOVEL     parana        d24tip
013700021203     C                   MOVEL(p)  fnlv24ds      KPJBU
013800940926     C                   CALL      'FNLV24R'
013900931018     C                   PARM                    KPJBA
014000021203     C                   MOVEL     KPJBU         fnlv24ds
014100950113     C*
014200950113     C* CONTROLLO SE E' STATO SELEZIONATO UN CODICE PADRONCINO
014300021203     C     d24pdr        IFNE      0
014400021203     C                   MOVEL     d24pdr        FILNEW
014500931018     C                   MOVEL     *ZEROS        PDRNEW
014600021203     C                   MOVE      d24pdr        PDRNEW
014700021203     C                   MOVEL     d24rsc        DPDNEW
014800950113     C                   ENDIF
014900950113     C*
015000931018     C                   GOTO      ENDCTR
015100931018     C                   ENDIF
015200931018     C*
015300931018     C* CONTROLLO VALIDITA' DEL CODICE PADRONCINO
015400931018     C                   TESTN                   PDRNEW               35
015500931018     C  N35              SETON                                        4090
015600931018     C  N35              GOTO      ENDCTR
015700090504     C                   MOVEL     FILNEW        COMNEW            7 0
015800090504     C                   MOVE      PDRNEW        COMNEW
015900931018     C*
016000940922     C* CONTROLLO ESISTENZA DEL PADRONCINO IN ANAGRAFICA
016100021203     C     kapdn         CHAIN     fiapd000                           30
016200940922     C  N30APDATB        IFNE      ' '
016300940922     C                   SETON                                        30
016400940922     C                   ENDIF
016500940922     C* CODICE PADRONCINO ERRATO
016600940922     C   30              SETON                                        4090
016700940922     C   30              GOTO      ENDCTR
016800100223     C                   MOVEL     APDRSf        DPDNEW
016900931018     C*
017000940923     C****  CODICE FILIALE  ****
017100950113     C* FILIALE OBBLIGATORIA
017200950113     C     FILOLD        IFEQ      0
017300950113     C                   SETON                                        5290
017400950113     C                   GOTO      ENDCTR
017500950113     C                   ENDIF
017600950113     C*
017700070307     C* 89 OFF -  CONTROLLO LA FILIALE NELLA L72  filiali contabili
017800950113     C     *IN89         IFEQ      *OFF
017900021216     C     FILOLD        IFNE      SIMpou
018000021216     C     simtpp        andeq     '1'
018100140117     C**** FILOLD        LOOKUP    L72                                    35
018200140117     C                   move      filOLD        Fil03A            3
018300140117     C     Fil03A        LOOKUP    $POg                                   35
018400940923     C  N35              SETON                                        5290
018500940923     C  N35              GOTO      ENDCTR
018600940923     C                   ENDIF
018700950113     C*
018800950113     C                   ELSE
018900020717     C* 89 ON  - SONO UN 2° liv.  QUINDI CONTROLLO LA FILIALE NELLA L6
019000020717     C     filold        IFNE      simpou
019100950113     C     FILOLD        LOOKUP    L6                                     35
019200950113     C  N35              SETON                                        5290
019300950113     C  N35              GOTO      ENDCTR
019400950113     C                   ENDIF
019500950113     C                   ENDIF
019600940923     C*
019700931018     C****  CODICE PADRONCINO VECCHIO  ****
019800931018     C     '?'           SCAN      PDROLD                                 90
019900931018     C*
020000931018     C     *IN90         IFEQ      *ON
020100021203     c                   clear                   fnlv24ds
020200021203     C                   Z-ADD     FILOLD        d24FIL
020300021203     C                   MOVEL     'R'           d24FLG
020400030107     C                   MOVEL     parana        d24tip
020500021203     C                   MOVEL(p)  fnlv24ds      KPJBU
020600940926     C                   CALL      'FNLV24R'
020700931018     C                   PARM                    KPJBA
020800021203     C                   MOVEL     KPJBU         fnlv24ds
020900950113     C*
021000950113     C* CONTROLLO SE E' STATO SELEZIONATO UN CODICE PADRONCINO
021100021203     C     d24pdr        IFNE      0
021200021203     C                   MOVEL     d24pdr        FILOLD
021300931018     C                   MOVEL     *ZEROS        PDROLD
021400021203     C                   MOVE      d24pdr        PDROLD
021500021203     C                   MOVEL     d24rsc        DPDOLD
021600950113     C                   ENDIF
021700950113     C*
021800931018     C                   GOTO      ENDCTR
021900931018     C                   ENDIF
022000931018     C*
022100931018     C* CONTROLLO VALIDITA' DEL CODICE PADRONCINO
022200931018     C                   TESTN                   PDROLD               35
022300931018     C  N35              SETON                                        4490
022400931018     C  N35              GOTO      ENDCTR
022500090504     C                   MOVEL     FILOLD        COMOLD            7 0
022600090504     C                   MOVE      PDROLD        COMOLD
022700931018     C*
022800940922     C* CONTROLLO ESISTENZA DEL PADRONCINO IN ANAGRAFICA
022900021203     C     kapdo         CHAIN     fiapd000                           30
023000940922     C  N30APDATB        IFNE      ' '
023100940922     C                   SETON                                        30
023200940922     C                   ENDIF
023300940922     C* CODICE PADRONCINO ERRATO
023400940922     C   30              SETON                                        4490
023500940922     C   30              GOTO      ENDCTR
023600100223     C                   MOVEL     APDRSf        DPDOLD
023700931018     C*
023800090112     C****  CONTROLLO VALIDITA' DELLA TARIFFA old  ****
023900090112     c                   clear                   ok                1
024000090112     C     KFGTO         setll     fifgt000
024100090112     c                   do        *hival
024200090112     C     KFGTO         reade     fifgt000
024300090112     c                   if        %eof(fifgt01l)
024400090112     c                   leave
024500090112     c                   end
024600090112     C                   IF        fgtatb <> ' '
024700090112     c                   iter
024800090112     c                   end
024900090112     C                   IF        fgtprg <> prgold
025000090112     c                   iter
025100090112     c                   end
025200090112     c                   move      '1'           ok
025300090112     c                   leave
025400090112     C                   ENDdo
025500090112     c* tariffe inesistenti
025600090112     c                   if        ok = ' '
025700090112     c                   seton                                        5090
025800090112     c                   goto      endctr
025900011126     c                   end
026000090112     c* controllo validità tariffa new, deve esistere già la supertestata
026100090429     c* ma non deve essere convalidata
026200090112     C     KtgtN         chain     fitgt000
026300090112     c                   if        not %found(fitgt01l)
026400090112     c                             or (%found(fitgt01l) and tgtatb <> ' ')
026500090112     c                   seton                                        5390
026600090112     c                   goto      endctr
026700090112     c                   end
026800090114     c                   if        tgtdts <> 0
026900090114     c                   seton                                        5490
027000090114     c                   goto      endctr
027100090114     c                   end
027200931018     C*
027300931018     C     ENDCTR        ENDSR
027400090113     C*---------------------------------------------------------------*
027500090112     C*--- GESTIONE DELLA SECONDA VIDEATA ----------------------------*
027600090112     C     VIDEO2        BEGSR
027700090112     C*---------------------------------------------------------------*
027800090113     C                   SETON                                        87
027900090113     C                   WRITE     CN10C02
028000090113     C                   SETOFF                                       87
028100090113     C                   Z-ADD     0             NRR               4 0
028200090112     C* CARICAMENTO SUBFILE
028300090112     C                   EXSR      CARSF
028400090114     C                   Z-ADD     1             REC
028500090112     c*
028600090112     c                   do        *hival
028700090112     C*
028800090112     C                   WRITE     CN10Z02
028900090114     c                   EXFMT     CN10C02
029000090112     C*
029100090112     C** CMD12 - RITORNO
029200090112     C     *INKL         IFEQ      *ON
029300090112     C                   GOTO      ENDVD2
029400090112     C                   ENDIF
029500090112     C*
029600090113     c                   clear                   ok                1
029700090114     c                   setoff                                       55
029800090112     c                   do        *hival        nn                4 0
029900090112     C     nn            chain     CN10S02                            32
030000090112     c                   if        *in32
030100090112     c                   leave
030200090112     c                   end
030300090114     c* controllo che non ci sia già la tariffa inserita
030400090114     c                   if        vi2sce <> *blank
030500090114     c                   movel     '1'           ok
030600090114     c                   movel     vi2tsr        comtsr
030700090114     C     KFGTn1        chain     fifgt000
030800090114     c                   if        %found(fifgt01l)
030900090114     c                   seton                                        55
031000090114     c                   else
031100090114     C     *INKf         IFEQ      *ON
031200090112     C                   MOVEL     VI2TSR        TSRold
031300090112     C                   MOVEL     VI2CTR        CTRold
031400090112     c                   movel     ficn06ds      kpjbu
031500090112     c                   call      'FICN06R'
031600090112     c                   parm                    kpjba
031700090112     c                   end
031800090114     c                   end
031900090114     c                   end
032000090114     c                   update    cn10s02
032100090114     c   55              leave
032200090112     c                   enddo
032300090114     c                   if        ok = '1'
032400090114     c                   if        *inkf and not *in55
032500090112     c                   commit
032600090112     c                   leave
032700090114     c                   end
032800090113     c                   else
032900090113     c                   seton                                        50
033000090113     c                   end
033100090112     c                   enddo
033200090112     C     ENDVD2        ENDSR
033300090112     C*
033400090112     C*--- CARICAMENTO SUBFILE ---------------------------------------*
033500090112     C     CARSF         BEGSR
033600090112     C*---------------------------------------------------------------*
033700090112     C*
033800090112     c                   movel     filold        vidfil
033900090112     c                   movel     pdrold        vidpdr
034000090112     c                   movel     dpdold        despdr
034100090112     C     KFGTO         SETLL     fifgt01L
034200090112     C                   DO        *HIVAL
034300090112     C     KFGTO         READE     fifgt01L
034400090112     C                   IF        %EOF(FIFGT01L)
034500090112     C                   LEAVE
034600090112     C                   END
034700090112     C*
034800090112     c                   if        fgtprg <> prgOLD
034900090112     c                   ITER
035000090112     c                   end
035100090112     c                   if        fgtATB <> ' '
035200090112     c                   ITER
035300090112     c                   end
035400090112     C* CAMPO SELEZIONE
035500090112     C                   MOVEL     ' '           VI2SCE
035600090112     C* TIPO PRESTAZIONE
035700090112     C                   CLEAR                   SER
035800090112     C                   MOVEA     FGTTSR        SER(1)
035900090112     C                   MOVEA     '-'           SER(2)
036000090112     C                   Z-ADD     1             X
036100090112     C     FGTTSR        LOOKUP    Tsr(X)                                 35
036200090112     c  n35              ITER
036300090112     C   35              MOVEA     DSR(X)        SER(3)
036400090112     C   35              MOVEA     SER           VI2TSR
036500090112     C                   MOVEL     FGTCTR        VI2CTR
036600090112     C                   MOVEL     FGTPRG        VI2PRG
036700090112     C                   MOVEL     FGTdiv        VI2div
036800090112     C                   Z-ADD     FGTDDT        DATA
036900090112     C                   Z-ADD     MM            GGMM              4 0
037000090112     C                   MOVEL     GG            GGMM
037100090112     C                   Z-ADD     AA            VI2DDT
037200090112     C                   MOVEL     GGMM          VI2DDT
037300090112     C                   Z-ADD     FGTDST        DATA
037400090112     C                   Z-ADD     MM            GGMM
037500090112     C                   MOVEL     GG            GGMM
037600090112     C                   Z-ADD     AA            VI2DST
037700090112     C                   MOVEL     GGMM          VI2DST
037800090112     C                   Z-ADD     FGTDUV        DATA
037900090112     C                   Z-ADD     MM            GGMM
038000090112     C                   MOVEL     GG            GGMM
038100090112     C                   Z-ADD     AA            VI2DUV
038200090112     C                   MOVEL     GGMM          VI2DUV
038300090112     C                   MOVEL     FGTDCV        VI2DCV
038400090112     C                   ADD       1             NRR
038500090112     C                   WRITE     CN10S02
038600090112     C                   ENDDO
038700090112     C*
038800090112     C                   ENDSR
038900090112     C*****************************************************************
039000090112     c     *inzsr        begsr
039100090112     C*****************************************************************
039200090112     C     *ENTRY        PLIST
039300090112     C                   PARM                    KPJBA
039400090113     c*
039500090113     C                   Z-ADD     1             CODUT
039600090113     C                   CALL      'X§PARUT'
039700090113     C                   PARM                    UT§DSE
039800090113     C                   MOVEL     RAGUT         RSUT             20
039900090113     C                   MOVEL     REC80         CNCR80
040000090112     c* se richiamato da menu pulisco la ds altrimenti imposto i campi
040100090114     c                   if        kcdaz = 'FN85'
040200090112     c                   clear                   ficn06ds
040300090112     c                   else
040400090112     c                   movel     kpjbu         ficn06ds
040500090112     c                   end
040600090112     c     filnew        comp      0                                  0101
040700090112     c     filold        comp      0                                  0202
040800090113     c                   if        filnew <> 0
040900090113     c                   seton                                        0302
041000090113     c                   end
041100090113     c                   if        filold <> 0
041200090113     c                   seton                                        0401
041300090113     c                   end
041400090113     C*
041500090113     c                   if        filnew = 0 and filold = 0
041600090113     C                   MOVEL     simpou        FILNEW
041700090113     C                   MOVEL     simpou        FILOLD
041800090113     c                   else
041900090113     c                   if        filnew <> 0 and filold = 0
042000090113     c                   eval      filold = filnew
042100090113     c                   end
042200090113     c                   if        pdrnew <> ' ' and pdrold = ' '
042300090113     c                   eval      pdrold = pdrnew
042400090113     c                   end
042500090113     c                   if        filold <> 0 and filnew = 0
042600090113     c                   eval      filnew = filold
042700090113     c                   end
042800090113     c                   if        pdrold <> ' ' and pdrnew = ' '
042900090113     c                   eval      pdrnew = pdrold
043000090113     c                   end
043100090113     c                   if        smlold <> ' ' or smlnew <> ' '
043200090113     c                   eval      smlold ='S'
043300090113     c                   eval      smlnew ='S'
043400090113     c                   end
043500090113     c                   end
043600090112     C***
043700090112     C* GIRO DATA DEL GIORNO: LA PRENDO DA TIME
043800090112     C***
043900090112     C                   TIME                    W0120            14 0
044000090112     C                   MOVE      W0120         WDAT              8 0
044100090112     C*
044200090112     C                   Z-ADD     WDAT          G02DAT
044300090112     C                   MOVEL     *BLANK        G02ERR
044400090112     C                   CALL      'XSRDA8'
044500090112     C                   PARM                    WLBDAT
044600090112     C* UDATE A 8 IN AAAA/MM/GG
044700090112     C                   Z-ADD     G02INV        WUDA              8 0
044800090112     C*---------------------------------------------------------------*
044900090112     C* ACCESSO TABEL
045000090112     C     KTAB          KLIST
045100090112     C                   KFLD                    CODUT
045200090112     C                   KFLD                    COD
045300090112     C                   KFLD                    KEY
045400090112     C     KTAB2         KLIST
045500090112     C                   KFLD                    CODUT
045600090112     C                   KFLD                    COD
045700090112     C* ACCESSO fifgt
045800090112     C     KFGTO         KLIST
045900090112     C                   KFLD                    COMOLD
046000090112     C                   KFLD                    SMLOLD
046100090112     C     KFGTN         KLIST
046200090112     C                   KFLD                    COMNEW
046300090112     C                   KFLD                    SMLNEW
046400090114     C     KFGTN1        KLIST
046500090114     C                   KFLD                    COMNEW
046600090114     C                   KFLD                    SMLNEW
046700090114     C                   KFLD                    comtsr
046800090114     C                   KFLD                    vi2ctr
046900090114     C                   KFLD                    prgnew
047000090112     C* ACCESSO fiTgt
047100090112     C     KTGTN         KLIST
047200090112     C                   KFLD                    COMNEW
047300090112     C                   KFLD                    SMLNEW
047400090112     C                   KFLD                    PRGNEW
047500090112     C* ACCESSO Tntbe
047600090112     C     Ktbe          KLIST
047700090112     C                   KFLD                    kcod1
047800090112     C                   KFLD                    kke1
047900090112     C* ACCESSO fiapd
048000090112     C     Kapdn         KLIST
048100090112     C                   KFLD                    parana
048200090112     C                   KFLD                    comnew
048300090112     C     Kapdo         KLIST
048400090112     C                   KFLD                    parana
048500090112     C                   KFLD                    comold
048600090112     C                   MOVEL     'A'           PARANA
048700090112     C*
048800090112     C* DEFINIZIONE CAMPI
048900090112     C     *LIKE         DEFine    TBECOD        KCOD1
049000090112     C     *LIKE         DEFine    TBEKE1        KKE1
049100090112     C     *LIKE         DEFINE    TBLCOD        COD
049200090112     C     *LIKE         DEFINE    TBLKEY        KEY
049300090112     C*
049400090112     C*---------------------------------------------------------------*
049500090112     C* CARICAMENTO TABELLE
049600090112     C                   EXSR      CARTAB
049700090112     C                   ENDSR
049800090112     c**********************************************************************
049900090112     C*--- CARICAMENTO TABELLE ---------------------------------------*
050000090112     c**********************************************************************
050100090112     C     CARTAB        BEGSR
050200090112     C****  CODICI TARIFFA E DESCRIZIONI: TTR / DTR   ****
050300090112     C                   Z-ADD     1             X                 3 0
050400090112     C                   MOVEL     'TR'          COD
050500090112     C     KTAB2         SETLL     TABEL
050600090112     C     KTAB2         READE     TABEL                                  30
050700090112     C*
050800090112     C     *IN30         DOWEQ     *OFF
050900090112     C     TBLFLG        IFEQ      ' '
051000090112     C                   MOVEL     TBLUNI        DSTR
051100090112     C*
051200090112     C* CARICO SOLO I CODICI TARIFFA VALIDI PER LE TARIFFE aut
051300090112     c                   if        §trutp = 'S'
051400090112     C                   MOVEL     TBLKEY        TTR(X)
051500090112     C                   MOVEL     TBLUNI        DTR(X)
051600090112     C                   ADD       1             X
051700090112     C                   ENDIF
051800090112     C*
051900090112     C                   ENDIF
052000090112     C     KTAB2         READE     TABEL                                  30
052100090112     C                   ENDDO
052200090112     C*
052300090116     C****  TIPI PRESTAZIONE E DESCRIZIONI: TSR / DSR /   ****
052400090112     C                   Z-ADD     1             X
052500090112     C                   MOVEL     '1Z'          COD
052600090112     C     KTAB2         SETLL     TABEL
052700090112     C     KTAB2         READE     TABEL                                  30
052800090112     C*
052900090112     C     *IN30         DOWEQ     *OFF
053000090112     C     TBLFLG        IFEQ      ' '
053100090112     C                   MOVEL     TBLUNI        DS1Z
053200090112     c                   if        §1zfac ='A'
053300090112     C                   MOVEL     TBLKEY        TSR(X)
053400090112     C                   MOVEL     §1ZDES        DSR(X)
053500090112     C                   ADD       1             X
053600090112     C                   ENDIF
053700090112     C                   ENDIF
053800090112     C     KTAB2         READE     TABEL                                  30
053900090112     C                   ENDDO
054000090112     C*
054100090112     C***
054200090112     C* CARICO TABELLA FILIALI contabili GESTITE
054300090112     C***
054400140117     c***                clear                   trul72ds
054500140117     c***                eval      i72tla = 'L'
054600140117     c***                eval      I72POU = simpou
054700140117     c***                eval      kpjbu = trul72ds
054800140117     C***                CALL      'TRUL72R'
054900140117     C***                PARM                    KPJBA
055000140117     c***                movel     kpjbu         trul72ds
055100140117     C***
055200140117      * Reperimento dei P.O. di CONTABILITA' gestibili dalla filiale
055300140117     c                   clear                   TRUL31DS
055400140117     c                   eval      I31abi = 'AC'
055500140117     c                   eval      I31cpo = simpou
055600140117     c                   call      'TRUL31R'
055700140117     c                   parm                    KPJBA
055800140117     c                   parm                    TRUL31DS
055900140117     c                   if        o31pog > *zeros
056000140117     c                   movea     o31pog        $pog
056100140117     c                   endif
056200090112     C***
056300090112     C* VEDO SE SONO SIMFEL O REMOTO
056400090112     C***
056500090112     c                   if        simtpp = '2'
056600090112     C                   SETON                                        89
056700090112     C***
056800090112     C* CARICO TABELLA FILIALI GESTITE £6
056900090112     C***
057000090112     C                   CLEAR                   DSUL06
057100090112     C                   MOVE      '£6'          D06COD
057200090112     C                   MOVEL     simpou        D06KEY
057300090112     C                   MOVEL     DSUL06        KPJBU
057400090112     C*
057500090112     C                   CALL      'TRUL06R'
057600090112     C                   PARM                    KPJBA
057700090112     C                   MOVEL     KPJBU         DSUL06
057800090112     C                   MOVEA     LIN           L6
057900090112     C                   ENDIF
058000090112     C*
058100090112     C                   ENDSR
