000100081223     H DECEDIT('0,') DATEDIT(*DMY.) DFTACTGRP(*NO) ACTGRP(*CALLER)
000200020124     H* ficn10R *----------------------------------------------------*
000300000000     H*-----------------*                                            *
000400930824     H*               GESTIONE TARIFFE PADRONCINI                    *
000500000000     H*--------------------------------------------------------------*
000600020124     Fficn10D   CF   E             WORKSTN
000700030121     F                                     SFILE(CN10S02:NRR)
000800020124     F                                     SFILE(CN10S05:NR2)
000900090108     Ffifgt01L  UF A E           K DISK    commit(flg) usropn
001000081223     Ffitgt01L  if   E           K DISK
001100081223     Ffifpt01L  if   E           K DISK
001200020220     Ffifpd01L  if   E           K DISK
001300000000     FTABEL00F  IF   E           K DISK
001400021203     Ffiapd01L  IF   E           K DISK
001500011107     Ftntbe01L  IF   E           K DISK
001600020422     Ffiftt02L  IF   E           K DISK
001700030303     Ffictt01L  IF   E           K DISK    usropn
001800030122     Fazorg01l  IF   E           K DISK
001900050411     fazcae03l  if   e           k disk    usropn
002000930903     D TTR             S              1    DIM(50)                              COD. TARIFFA
002100930903     D DTR             S             15    DIM(50)                              DESCR.COD. TARIFFA
002200940920     D MIN             S             10  0 DIM(50)                              MINIMO TASSABILE X T
002300931104     D TPS             S              1    DIM(50)                              TIPO PRESTAZIONE
002400931104     D DSR             S             15    DIM(50)                              DESCR.TIPO PRESTAZIO
002500931104     D CDT             S              3    DIM(50)                              COD.TAR. DEL TIPO PR
002600930903     D TSM             S              1    DIM(50)                              TIPO SOMMA PESI
002700930903     D DSM             S             15    DIM(50)                              DESCR.TIPO SOMMA PES
002800931021     D TPE             S              1    DIM(50)                              TAR. PIK/ETICH.
002900931021     D DPE             S             15    DIM(50)                              DESCR.TAR. PIK/ETICH
003000931115     D* SCHIERA DI COMODO
003100931104     D SER             S              1    DIM(12)                              COMODO X TIPO PRESTA
003400930923     D*
003500081223     D ficn02ds      e DS
003600940920     D* PARAMETRO PASSATO PER ACCEDERE ALLE TARIFFE SIMULAZIONE
003700940920     D*   DIRETTAMENTE DA MENU'
003800940920     D PARAMX          DS
003900931115     D*
004000931115     D* RICHIAMO PGM DI DECODIFICA C.A.P.             - TRUL05R -
004100931115     D PARAM           DS
004200931115     D  PARTLA                 1      1
004300941221     D  PARCAP                 2     10
004400970718     D  PARDRI                11     18  0
004500970718     D  PARNAR                19     21    INZ
004600970718     D  PARNDE                22     23  0
004700970718     D  PARNDF                24     25  0
004800970718     D  PARDEC                26     60
004900970718     D  PARE01                61     95
005000970718     D  PARE02                96    130
005100970718     D  PARE03               131    165
005200970718     D  PARE04               166    200
005300970718     D  PARE05               201    235
005400970718     D  PARE06               236    270
005500970718     D  PARE07               271    305
005600970718     D  PARE08               306    340
005700970718     D  PARE09               341    375
005800970718     D  PARE10               376    410
005900970718     D  PARTCD               411    411    INZ
006000930825     D*
006100011029     D* RICHIAMO PGM DI DUPLICA TARIFFA               - ficn85R -
006200930825     D PARAM3          DS
006300930908     D  PA3PDR                 1      7  0
006400931011     D  PA3SML                 8      8
006500931011     D  PA3TSR                 9      9
006600931011     D  PA3CTR                10     12  0
006700931011     D  PA3PRG                13     15  0
006800931011     D  PA3PDN                16     22  0
006900931011     D  PA3SMN                23     23
007000931011     D  PA3TSN                24     24
007100931011     D  PA3CTN                25     27  0
007200011126     D  PA3div                28     30
007300930825     D*
007400011029     D* RICHIAMO PGM DI GESTIONE TARIFFE PARTICOLARI  - ficn82R -
007500930902     D PARAM4          DS
007600930927     D  PA4TES                 1     37
007700020130     d  modali                37     37
007800930927     D  PA4PDR                38     44  0
007900940922     D  PA4DPD                45     79
008000940922     D  PA4SML                80     80
008100940922     D  PA4TSR                81     81
008200940922     D  PA4DTS                82     96
008300940922     D  PA4CTR                97     99  0
008400021210     D  PA4DCT               100    113
008500021210     d  pa4anac              114    114
008600940922     D  PA4DCV               115    129
008700940922     D  PA4PRG               130    132  0
008800940922     D  PA4FLG               133    133
008900940922     D  PA4DUV               134    134
009000970718     D  PA4DDT               135    142  0
009100011029     D  PA4div               144    146
009200020128     D  PA4ctd               147    149
009300020128     D  PA4dtd               150    174
009400030121     D* ds parametri controllo esistenza altra tariffa valida coop
009500030121     D paramt          DS
009600030121     D  datax                         8s 0
009700030121     D  pogesx                        3s 0
009800030121     D  tipopre                       1
009900030121     d  error                         1
010500970718     D* DS PER TISI95R - CONTROLLO CAP
010600970718     D DSSI95        E DS                  EXTNAME(TISI95DS)
010700940919     D*
010800931115     D TCUDS           DS
010900931115     D  F1                     1      1
011000931115     D  F3                     3      3
011100931115     D  F2                     2      2
011200931115     D  F4                     4      4
011300931115     D  F56                    5      6
011400921102     D WLBDAT          DS
011500940906     D  G02DAT                 1      8  0
011600940906     D  G02INV                 9     16  0
011700940906     D  G02ERR                17     17
011800940906     D  G02TGI                18     22  0
011900940504     D                 DS
012000940906     D  AA                     1      4  0
012100940906     D  MM                     5      6  0
012200940906     D  GG                     7      8  0
012300940906     D  DATA                   1      8  0
012400940916     D                 DS
012500940916     D  VI4SGL                 1     10  0
012600011029     D  VI4ITR                11     19  3
012700011029     D  VI4MIN                20     28  3
012800011029     D  VI4MAX                29     37  3
012900011029     D  COMREC                 1     37
013000011029     D  CO2REC                11     37
013100940916     D                 DS
013200011029     D  SAVITR                 1      9  3
013300011029     D  SAVMIN                10     18  3
013400011029     D  SAVMAX                19     27  3
013500011029     D  SAVREC                 1     27
013600000000     D KPJBA         E DS
013700020701     D* REM                    1      3
013800020701     D* REMFIL                 4      6
013900930914     D*
014000930914     D TAR             DS
014100930914     D  FGDNRR               397    400B 0
014200011029     D FGTDS         E DS                  EXTNAME(fifgt00F)
014300090109     D vidpog          s                   like(vidfil)
014400090109     D comfgt          s                   like(fgtds)
014500020215     D savkpjbu        s                   like(kpjbu)
014600930928     D DSTR          E DS
014700930928     D DS1Z          E DS
014800900320     D CNCR80        E DS
014900131008     D tibs42ds      E DS
014901131008     D uteautds      E DS
015000021203     D fnlv24ds      E DS
015100021218     D fnlv55ds      E DS
015300030122     D og143         E DS
015400900320     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
015500931115     D  TCU                  398    697
015600931115     D                                     DIM(50)                              TAB FLAG CNT
015700931115     D  KCU                  698    847P 0
015800931115     D                                     DIM(50)                              TAB CAPO CONTO
015900931115     D                                     PACKEVEN
016000931115     D  DTU                  848    895P 0
016100931115     D                                     DIM(12)                              DATE PARAM
016200931115     D                                     PACKEVEN
016300020124     D ds8a          E DS
016400020124     d xx              s              3  0
016500020204     d carok           s              1
016600021210     d almeno_uno      s              1
016700020206     d ricar           s              1
016800020125     d wkctd           s              3
016900020129     d dataiso         s               d   datfmt(*iso)
017000020129     d dataeur         s               d   datfmt(*eur)
017100130614     d cdpar           s             30    dim(50)
017200130614     d cdpards         ds            30
017300020124     d  fba                           1    overlay(cdpards:1)
017400020124     d  ctd                           3    overlay(cdpards:2)
017500020124     d  dtd                          25    overlay(cdpards:5)
017501130614     d  esc                           1    overlay(cdpards:30)
017600941005     D*
017700020130     D XDTF            S              2  0 DIM(12) CTDATA PERRCD(12)            GG DEI MESI
017800930811     C*****************************************************************
017900930811     C*---------------------------------------------------------------*
018000930928     C* ESEGUO OPERAZIONI INIZIALI
018100930928     C                   EXSR      OPERAZ
018200930903     C*
018300930914     C* ALF1 = 'F' ===> VADO A FINE PGM
018400930811     C     ALF1          DOWNE     'F'
018500930824     C*
018600930914     C* ALF1 = '1' ===> GESTIONE VIDEATA DELLE SELEZIONI
018700930811     C     ALF1          IFEQ      '1'
018800930811     C                   EXSR      VIDEO1
018900930811     C                   ENDIF
019000930811     C*
019100930928     C* ALF1 = '2' ===> GESTIONE VIDEATA SUBFILE CODICI TARIFFA
019200930811     C     ALF1          IFEQ      '2'
019300930811     C                   EXSR      VIDEO2
019400930811     C                   ENDIF
019500930824     C*
019600020125     C     ALF1          IFEQ      '3'
019700020125     C                   EXSR      VIDEO3
019800020125     C                   ENDIF
019900020125     C*
020000930811     C                   ENDDO
020100940930     C*
020200930820     C                   SETON                                        LR
020300930928     C*
020400930811     C*--- GESTIONE DELLA PRIMA VIDEATA ------------------------------*
020500930811     C     VIDEO1        BEGSR
020600011109     C*---------------------------------------------------------------*
020700020206     C                   SETOFF                                         2910
020800930811     C*
020900081229     c     cn02mod       comp      ' '                                1010
021000081223     c                   if        cn02ana= 'A'
021100021210     c   10              eval      vidtes = 'Interrogazione tariffe Autotrasp. '
021200090424     c  n10              eval      vidtes = '  ** Gestione tariffe AUT città **'
021300021210     c                   else
021400021210     c   10              eval      vidtes = 'Interrogazione tariffe Cooperative'
021500021210     c  n10              eval      vidtes = '** Gestione tariffe Cooperative **'
021600021210     c                   endif
021700081229     C                   MOVEL     'EUR'         VIDdiv
021800020130     c
021900930910     C     *IN11         IFEQ      *OFF
022000930927     C                   MOVEL     *BLANKS       VIDTSR
022100930927     C                   MOVEL     *BLANKS       DESTSR
022200930907     C                   MOVEL     *BLANKS       VIDCTR
022300930910     C                   SETON                                        11
022400930909     C                   ENDIF
022500930930     C*
022600930930     C* P U L I Z I A   S U B F I L E
022700930930     C* LA FACCIO IN QUESTO PUNTO PERCHE' SE LA LASCIAVO PRIMA DEL
022800931012     C*   CARICAMENTO DEL SFL, NON VENIVANO EMESSI I MESSAGGI DI
022900931013     C*   ERRORE ASSOCIATI AGLI INDICATORI 51-72
023000020124     C                   EXSR      PULSF
023100930907     C*
023200930907     C     FOR01         TAG
023300090107     c                   if        *in90
023400090107     c                   setoff                                       90
023500090107     C                   write     CN10D01
023600090107     c                   seton                                        90
023700090107     c                   end
023800090107     C                   EXFMT     CN10D01
023900081223     C                   SETOFF                                       0486
024000930929     C*
024100930811     C** CMD3  - FINE LAVORO
024200931013     C   KC              MOVEL     'F'           ALF1
024300931013     C   KC              GOTO      ENDVD1
024400931013     C*
024500940928     C** CMD7  - GESTIONE ANAGRAFICA PADRONCINI
024600930907     C     *INKG         IFEQ      *ON
024700081223     c     cn02ana       andne     'A'
024900131022     C                   Z-ADD     VIDFIL        d24FIL
025000021203     C                   Z-ADD     0             d24pdr
025100030204     C                   MOVEL     'R'           d24FLG
025200081223     C                   MOVEL     cn02ana       d24tip
025300021203     C                   MOVEL     *BLANKS       d24rsc
025400021203     C                   MOVEL(p)  fnlv24ds      KPJBU
025500940926     C                   CALL      'FNLV24R'
025600930811     C                   PARM                    KPJBA
025700030204     C                   MOVEL     KPJBU         fnlv24ds
025800020228     C                   MOVEL     SAVKPJBU      KPJBU
025900030204     C* CONTROLLO SE E' STATO SELEZIONATO UN CODICE PADRONCINO
026000030204     C     d24pdr        IFNE      0
026100030204     C                   MOVEL     d24pdr        VIDFIL
026200030204     C                   MOVEL     *ZEROS        VIDPDR
026300030204     C                   MOVE      d24pdr        VIDPDR
026400030204     C                   MOVEL     d24rsc        DESPDR
026500030204     C                   ENDIF
026600930907     C*
026700930907     C                   GOTO      FOR01
026800930811     C                   ENDIF
026900930811     C*
027000930907     C** CMD14 - GESTIONE ANAGRAFICA FORNITORI
027100011126     c                   if        *inkn
027200011126     c                   movel(p)  'P'           kpjbu
027300011126     C                   CALL      'TRUL16R'
027400930907     C                   PARM                    KPJBA
027500020228     C                   MOVEL     savKPJBU      KPJBU
027600011126     C                   GOTO      FOR01
027700011126     c                   end
027800940919     C*
027900930811     C** CONTROLLI DELLA PRIMA VIDEATA
028000930907     C                   EXSR      CONTR1
028100930907     C   90              GOTO      FOR01
028200930811     C*
028300930907     C                   MOVEL     '2'           ALF1
028400930907     C     ENDVD1        ENDSR
028500930811     C*
028600930811     C*--- CONTROLLI DELLA PRIMA VIDEATA -----------------------------*
028700930811     C     CONTR1        BEGSR
028800011109     C*---------------------------------------------------------------*
028900930830     C                   SETOFF                                       030590
029000081229     C                   SETOFF                                       8562
029100090109      * controllo autorizzazione del profilo ad accedere alle tariffe
029200131008     c                   clear                   tibs42ds
029300131008     c                   movel     vidfil        i42pge
029500131008     c                   call      'TIBS42R'
029600131008     c                   parm                    tibs42ds
029700131008     c                   movel     o42uni        uteautds
029800090109     C* COOP
029900090109     c                   if        cn02ana= 'C'
030000100420     c                   if        §autvtcc <> 'S'
030100021211     c                   seton                                        6290
030200021211     c                   goto      endct1
030300021211     c                   endif
030400090109     C                   ELSE
030500090109     C* AUT
030600131022     c                   if        o42err <>' ' and §AUTMTC <> 'S'
030700090109     c                   seton                                        8390
030800090109     c                   goto      endct1
030900090109     c                   endif
031000021211     c                   endif
031100930811     C*
031200940920     C****  CODICE FILIALE  ****
031300950113     C* FILIALE OBBLIGATORIA
031400030529     C     VIDFIL        IFEQ      0
031500950113     C                   SETON                                        8390
031600950113     C                   GOTO      ENDCT1
031700950113     C                   ENDIF
031800950113     C*
032100020701     C                   MOVEL     VIDFIL        WFIL              3 0
032800940920     C*
032900930811     C****  CODICE PADRONCINO  ****
033000930908     C     '?'           SCAN      VIDPDR                                 90
033100930811     C*
033200930811     C     *IN90         IFEQ      *ON
033300021203     C                   Z-ADD     VIDFIL        d24FIL
033400021203     C                   Z-ADD     0             d24pdr
033500021203     C                   MOVEL     'R'           d24FLG
033600081223     C                   MOVEL     cn02ana       d24tip
033700021203     C                   MOVEL     *BLANKS       d24rsc
033800021203     C                   MOVEL(p)  fnlv24ds      KPJBU
033900940926     C                   CALL      'FNLV24R'
034000930811     C                   PARM                    KPJBA
034100021203     C                   MOVEL     KPJBU         fnlv24ds
034200020228     C                   MOVEL     savKPJBU      KPJBU
034300941006     C*
034400941006     C* CONTROLLO SE E' STATO SELEZIONATO UN CODICE PADRONCINO
034500021203     C     d24pdr        IFNE      0
034600021203     C                   MOVEL     d24pdr        VIDFIL
034700930921     C                   MOVEL     *ZEROS        VIDPDR
034800021203     C                   MOVE      d24pdr        VIDPDR
034900021203     C                   MOVEL     d24rsc        DESPDR
035000941006     C                   ENDIF
035100941006     C*
035200930811     C                   GOTO      ENDCT1
035300930811     C                   ENDIF
035400930811     C*
035500930921     C* CONTROLLO VALIDITA' DEL CODICE PADRONCINO
035600930921     C                   TESTN                   VIDPDR               35
035700930921     C  N35              SETON                                        4090
035800930921     C  N35              GOTO      ENDCT1
035900931111     C*
036000940915     C* CONTROLLO SE ESISTE NELL'ANAGRAFICA PADRONCINI
036100940920     C                   MOVEL     VIDFIL        COMPDR
036200940920     C                   MOVE      VIDPDR        COMPDR
036300021203     C     kapdc         CHAIN     fiapd000                           32
036400090610     c                   if        not *in32 and not *in10
036500090610     C     APDATB        IFNE      ' '
036600940914     C                   SETON                                        32
036700940914     C                   ENDIF
036800090610     C                   ENDIF
036900940914     C* CODICE PADRONCINO ERRATO
037000940914     C   32              SETON                                        4090
037100940914     C   32              GOTO      ENDCT1
037200100204      *
037300100204     c*  se escluso da autofatturazione dare msg di errore
037400100204     c                   if        apdpdd = 'S'  and cn02ana<>'C'
037500100204     C                   SETON                                        4090
037600100204     C                   GOTO      ENDCT1
037700100204     c                   end
037800100204      *
037900100224     C********           MOVEL     APDRSC        DESPDR
038000100224     C                   MOVEL     APDRSF        DESPDR
038100930811     C*
038200931104     C****  TIPO PRESTAZIONE  ****
038300930928     C                   MOVEL     *BLANKS       DESTSR
038400930927     C     VIDTSR        IFNE      ' '
038500930830     C*
038600930927     C     VIDTSR        IFEQ      '?'
038700930830     C                   MOVEL     '1Z'          COD
038800930927     C                   MOVEL     ' '           VIDTSR
038900930830     C                   MOVEL     *BLANKS       DES              25
039000930830     C                   CALL      'X§TABER'
039100930830     C                   PARM                    CODUT
039200930830     C                   PARM                    COD
039300930830     C                   PARM                    KEY
039400930830     C                   PARM                    DES
039500930927     C                   MOVEL     KEY           VIDTSR
039600930927     C                   MOVEL     DES           DESTSR
039700930830     C                   SETON                                        90
039800930830     C                   GOTO      ENDCT1
039900930903     C                   ENDIF
040000930830     C*
040100931104     C* CONTROLLO VALIDITA' TIPO PRESTAZIONE
040200930903     C                   Z-ADD     1             X
040300930927     C     VIDTSR        LOOKUP    TPS(X)                                 35
040400931104     C* TIPO PRESTAZIONE ERRATO
040500930903     C  N35              SETON                                        4190
040600930903     C  N35              GOTO      ENDCT1
040700930927     C                   MOVEL     DSR(X)        DESTSR
040800930928     C                   MOVEL     CDT(X)        COMCDT            3
040900930830     C*
041000931104     C* 03 ON  - E' STATO DIGITATO ANCHE IL TIPO PRESTAZIONE
041100930830     C                   SETON                                        03
041200930830     C                   ENDIF
041300930830     C*
041400930811     C****  CODICE TARIFFA  ****
041500930811     C     VIDCTR        IFNE      *BLANKS
041600931008     C*
041700931013     C  N03VIDCTR        IFNE      '000'
041800931104     C* SE DIGITATO IL CODICE TARIFFA OCCORRE INSERIRE IL TIPO PRESTAZ.
041900931013     C                   SETON                                        4290
042000931013     C                   GOTO      ENDCT1
042100931013     C                   ELSE
042200931013     C                   MOVEL     *BLANKS       VIDCTR
042300931013     C                   SETON                                        85
042400931013     C                   ENDIF
042500930928     C*
042600931013     C* 85 ON  - E' COME SE IL CODICE TARIFFA NON FOSSE STATO INSERITO
042700931013     C     *IN85         IFEQ      *OFF
042800930928     C*
042900931103     C     VIDTSR        IFEQ      'G'
043000931103     C*
043100931103     C* CONTROLLO SE IL CODICE TARIFFA DIGITATO E' UGUALE AL CODICE
043200931103     C*   TARIFFA PRESENTE IN TABELLA
043300930928     C     VIDCTR        IFNE      COMCDT
043400930928     C                   SETON                                        6190
043500930928     C                   GOTO      ENDCT1
043600930928     C                   ENDIF
043700930928     C*
043800011107     C*
043900931104     C                   Z-ADD     1             X
044000931104     C                   MOVEL     COMCDT        UNOCTR            1
044100931104     C     UNOCTR        LOOKUP    TTR(X)                                 35
044200931104     C* SALVO INDICE USATO PER CONTROLLARE L'APPLICAZIONE TARIF. FINITA
044300931104     C   35              Z-ADD     X             Y                 3 0
044400931104     C*
044500931103     C                   ELSE
044600931104     C* TIPO PRESTAZIONE >< "G"
044700931103     C*
044800931104     C                   Z-ADD     1             X
044900931104     C                   MOVEL     VIDCTR        UNOCTR
045000931104     C     UNOCTR        LOOKUP    TTR(X)                                 35
045100931104     C*
045200931103     C     VIDCTR        IFNE      COMCDT
045300931103     C* CODICE TARIFFA ERRATO
045400931103     C  N35              SETON                                        4390
045500931103     C  N35              GOTO      ENDCT1
045600931103     C* SALVO INDICE USATO PER CONTROLLARE L'APPLICAZIONE TARIF. FINITA
045700931104     C                   Z-ADD     X             Y
045800030304     c*
045900931104     C                   ELSE
046000931104     C   35              Z-ADD     X             Y
046100931103     C                   ENDIF
046200931103     C*
046300931103     C                   ENDIF
046400931103     C*
046500930928     C* 05 ON  - E' STATO DIGITATO ANCHE IL CODICE TARIFFA
046600930811     C                   SETON                                        05
046700931013     C                   ENDIF
046800931013     C*
046900931013     C                   MOVEL     VIDCTR        COMCTR
047000930811     C                   ENDIF
047100000620     C*
047200100201     c                   if        cn02ana  <>'C'
047300100127     C* Controlla che non vi sia presente già un'altra tariffa Consegna/Ritiro
047400100127     C*  di dettaglio sul progressivo
047500100127     C*
047600100127     c                   clear                   gia_dettaglio     1
047700100127     c                   if        vidctr <> '999'
047800100127     c     kfgt1         setll     fifgt01l
047900100127     c     kfgt1         reade     fifgt01l
048000100127     c                   dow       not %Eof(fifgt01l)
048100100127     C*
048200100127     c                   if        fgtprg = cn02prg and fgtctr <> 999
048300100127     c                   move      fgtctr        alfctr            3
048400100127     c                   if        alfctr <> vidctr
048500100127     c                   move      'S'           gia_dettaglio
048600100127     c                   leave
048700100127     c                   end
048800100127     c                   end
048900100127     C*
049000100127     c     kfgt1         reade     fifgt01l
049100100127     c                   enddo
049200100127     c                   end
049300100127     C*  se presente già un'altra tariffa di dettaglio
049400100127     C*     sullo stesso tipo servizio/progressivo
049500100127     c                   if        gia_dettaglio = 'S'
049600100127     C                   SETON                                        6590
049700100127     C                   GOTO      ENDCT1
049800100201     c                   end
049900100201     C*
050000100907     C* Controlla che non vi sia presente una tariffa a Giornata e si sta immettendo
050100100907     C*  una tariffa  di dettaglio sul progressivo
050200100907     C*
050300100907     c                   clear                   gia_giornata      1
050400100907     C*    quindi un dettaglio
050500100907     c                   if        vidctr <> '999' and vidctr <> *blank
050600100907     C*
050700100907     C                   movel     'G'           tipoGio           1
050800100907     C                   z-add     999           ctarGio           3 0
050900100907     c     kfpt1_gio     setll     fifpt01l
051000100907     c     kfpt1_gio     reade     fifpt01l
051100100907     c                   dow       not %Eof(fifpt01l)
051200100907     C*
051300100907     c                   if        fptprg = cn02prg
051400100907     c                   if        fptctd = 'TIG' or fptctd = 'TMG'
051500100907     c                   move      'S'           gia_giornata
051600100907     c                   leave
051700100907     c                   end
051800100907     c                   end
051900100907     C*
052000100907     c     kfpt1_gio     reade     fifpt01l
052100100907     c                   enddo
052200100907     c                   end
052300100907      *
052400100907     C*  se presente già un'altra tariffa di dettaglio
052500100907     C*     sullo stesso tipo servizio/progressivo
052600100907     c                   if        gia_giornata  = 'S'
052700100907     C                   SETON                                        6690
052800100907     C                   GOTO      ENDCT1
052900100907     c                   end
053000100907     C*
053100100127     c                   end
053200100127     C*
053300030206      *controllo immissione tariffa trattoristi e pulizie tariffe fisse
053400030206      * con codici 610 e 650
053500030529      * inserito controllo anche per le coop tariffa solo 600
053600030529     C     *IN05         IFEQ      *ON
053700030529     C     VIDtsr        ANDEQ     'X'
053800030529     C     vidctr        ANDNE     '600'
053900030529     C* CODICE TARIFFA ERRATO
054000030529     C                   SETON                                        6190
054100030529     C                   GOTO      ENDCT1
054200030529     C                   ENDIF
054300030206     C     *IN05         IFEQ      *ON
054400030206     C     VIDtsr        ANDEQ     'P'
054500030206     C     vidctr        ANDNE     '650'
054600030206     C* CODICE TARIFFA ERRATO
054700030206     C                   SETON                                        6190
054800030206     C                   GOTO      ENDCT1
054900030206     C                   ENDIF
055000030206     C     *IN05         IFEQ      *ON
055100030206     C     VIDtsr        ANDEQ     'T'
055200030206     C     vidctr        ANDNE     '610'
055300030206     C* CODICE TARIFFA ERRATO
055400030206     C                   SETON                                        6190
055500030206     C                   GOTO      ENDCT1
055600030206     C                   ENDIF
055700000620     C*
055800930928     C****  CONTROLLO SE ESISTONO TARIFFE PER QUEL PADRONCINO  ****
055900931012     C* IMPOSTO CAMPI DELLA CHIAVE
056000930928     C                   MOVEL     VIDFIL        COMPDR
056100930915     C                   MOVE      VIDPDR        COMPDR
056200931011     C  N19              MOVEL     ' '           COMSML
056300931011     C   19              MOVEL     'S'           COMSML
056400011107     C*
056500931006     C  N03
056600931006     CANN05              DO
056700011029     C     KFGT          SETLL     fifgt000                               30
056800931006     C  N30              SETON                                        5190
056900931006     C  N30              GOTO      ENDCT1
057000931006     C                   ENDDO
057100931005     C*
057200931006     C   03
057300931006     CANN05              DO
057400011029     C     KFGT1         SETLL     fifgt000                               30
057500931006     C  N30              SETON                                        7290
057600931006     C  N30              GOTO      ENDCT1
057700931006     C                   ENDDO
057800930909     C*
057900940905     C   03
058000011029     CAN 05KFGT2         SETLL     fifgt000                               30
058100090429     c* controllo che non esista il tgt convalidato
058200090429     c                   if        cn02ana = 'A' and cn02prg <> *hival
058300090511     c                             and *in03 and *in05
058400090511     C     KFGT4         SETLL     fifgt000                               30
058500090511     c                   if        not *in30
058600090429     C     Ktgt          CHAIN     fitgt000
058700090429     c                   if        %found(fitgt01l) and tgtatb = ' '
058800090429     c                             and tgtdts <> 0
058900090429     C                   SETON                                        7390
059000090429     C                   GOTO      ENDCT1
059100090429     c                   end
059200090429     c                   end
059300090511     c                   end
059400930908     C*
059500930811     C     ENDCT1        ENDSR
059600930811     C*
059700930811     C*--- GESTIONE DELLA SECONDA VIDEATA ----------------------------*
059800930811     C     VIDEO2        BEGSR
059900011109     C*---------------------------------------------------------------*
060000020206     C                   SETOFF                                       0102
060100930811     C*
060200930927     C                   MOVEL     ' '           TSR               1
060300930830     C                   Z-ADD     0             CTR               3 0
060400930830     C                   Z-ADD     0             PRG               3 0
060500930825     C*
060600930909     C* 30 OFF - IMMISSIONE
060700930909     C* 30 ON  - AGGIORNAMENTO
060800930909     C*
060900930825     C     *IN30         IFEQ      *OFF
061000931013     C     *IN29         ANDEQ     *OFF
061100930825     C* 01 ON  - IMMISSIONE
061200930825     C                   SETON                                        01
061300930927     C                   MOVEL     VIDTSR        TSR
061400930909     C                   MOVEL     VIDCTR        CTR
061500081223     c                   if        cn02ana  = 'A'
061600081223     c                   z-add     cn02prg       prg
061700081223     c                   else
061800081223     C                   Z-ADD     0             PRG
061900081223     c                   end
062000930812     C                   ELSE
062100020410     C* CARICAMENTO SUBFILE
062200931013     C  N29              EXSR      CARSF
062300090108     c* se sono in interrogazione e non ci sono record torno al 1° video
062400090108     c                   setoff                                       52
062500090108     C                   IF        nrr = 0 and *in10
062600090108     C                   SETON                                        5290
062700090108     C                   MOVEL     '1'           ALF1
062800090108     C                   GOTO      ENDVD2
062900090108     c                   end
063000930820     C*
063100020205     c                   do        *hival
063200090109     C* NESSUN PROGRESSIVO VALIDO CARICATO A VIDEO: VADO IN IMMISSIONE
063300930820     C     NRR           IFEQ      0
063400090109     C* 01 ON  - IMMISSIONE
063500930928     C                   SETON                                        01
063600090109     c*
063700011213     C     KFGT2         setgt     fifgt000
063800011213     c     kfgt2         readpe    fifgt000                               30
063900011213     c                   if        not *in30
064000020410     c     kfgt          chain     fifgt000
064100011213     c                   end
064200090109     c* se non inseriti prestazione o tariffa errore
064300930921     C     *IN05         IFEQ      *OFF
064400090107     C  N03              SETON                                        5190
064500090107     C   03              SETON                                        7290
064600930909     C                   MOVEL     '1'           ALF1
064700930909     C                   GOTO      ENDVD2
064800930909     C                   ENDIF
064900090109     C* per coop: ULTIMO LETTO + 1, per aut: invece mi viene già
065000090109     c* passato dalla supertestata
065100930930     C                   MOVEL     VIDTSR        TSR
065200930930     C                   MOVEL     VIDCTR        CTR
065300090108     c                   if        cn02ana = 'A' and cn02prg <> *hival
065400081223     c                   eval      prg = cn02prg
065500081223     c                   else
065600930830     C     LETPRG        ADD       1             PRG
065700081223     c                   end
065800020219     c                   leave
065900930820     C*
066000930820     C                   ELSE
066100930909     C* NRR <> 0
066200930928     C*
066300930928     C* 02 ON  - AGGIORNAMENTO
066400930928     C                   SETON                                        02
066500930928     C*
066600930910     C                   Z-ADD     1             REC
066700020125     C                   WRITE     CN10Z02
066800020124     C                   EXFMT     CN10C02
066900930820     C*
067000930820     C** CMD12 - RITORNO
067100930914     C     *INKL         IFEQ      *ON
067200930914     C                   SETOFF                                       11
067300930914     C                   MOVEL     '1'           ALF1
067400930914     C                   GOTO      ENDVD2
067500930914     C                   ENDIF
067600930820     C*
067700930820     C** CMD9  - AGGIUNTA TARIFFA
067800930914     C     *INKI         IFEQ      *ON
067900081229     C* SE NON IMMESSI IL TIPO PRESTAZIONE E IL CODICE TARIFFA ERRORE       A
068000930830     C     *IN03         IFEQ      *OFF
068100930830     C     *IN05         OREQ      *OFF
068200930909     C                   SETON                                        04
068300930820     C                   MOVEL     '1'           ALF1
068400930907     C                   GOTO      ENDVD2
068500081229     C                   End
068600081229
068700020205     C                   clear                   PARAM3
068800020206     c                   if        vi2prg > 0
068900020206     c                   z-add     vi2prg        pa3prg
069000020206     c                   end
069100930908     C                   Z-ADD     COMPDR        PA3PDR                         *PDR VECCHIO
069200931012     C                   MOVEL     COMSML        PA3SML                         *SML VECCHIO
069300930927     C                   MOVEL     VIDTSR        PA3TSR                         *TSR VECCHIO
069400930825     C                   MOVEL     VIDCTR        PA3CTR                         *CTR VECCHIO
069500931012     C                   Z-ADD     COMPDR        PA3PDN                         *PDR NUOVO
069600931012     C                   MOVEL     COMSML        PA3SMN                         *SML NUOVO
069700930927     C                   MOVEL     VIDTSR        PA3TSN                         *TSR NUOVO
069800930825     C                   MOVEL     VIDCTR        PA3CTN                         *CTR NUOVO
069900011126     C                   MOVEL     VIDdiv        PA3div                         *CTR NUOVO
070000930825     C                   MOVEL     PARAM3        KPJBU
070100020218     C                   CALL      'FICN13R'
070200930825     C                   PARM                    KPJBA
070300930825     C                   MOVEL     KPJBU         PARAM3
070400020215     C                   MOVEL     savKPJBU      kpjbu
070500930927     C                   MOVEL     PA3TSR        TSR
070600930921     C                   Z-ADD     PA3CTR        CTR
070700930830     C     PA3PRG        ADD       1             PRG
070800090107     c                   commit
070900020206     c                   leave
071000930823     C                   ELSE
071100930825     C* *INKI OFF - CONTROLLO SE E' STATA SCELTA UNA TARIFFA
071200020124     C                   READC     CN10S02                                32
071300930825     C*
071400020205     c                   if        vi2sce <> *blank
071500930927     C                   MOVEL     VI2TSR        TSR
071600930920     C                   MOVEL     VI2CTR        CTR
071700930920     C                   MOVEL     VI2PRG        PRG
071800020205     c                   leave
071900020205     c                   end
072000930820     C                   ENDIF
072100930812     C                   ENDIF
072200020205     c                   enddo
072300930825     C                   ENDIF
072400930825     C*
072500930907     C                   MOVEL     '3'           ALF1
072600930907     C     ENDVD2        ENDSR
072700930811     C*
072800930812     C*--- PULIZIA SUBFILE -------------------------------------------*
072900930812     C     PULSF         BEGSR
073000011109     C*---------------------------------------------------------------*
073100930915     C                   SETON                                        87
073200020124     C                   WRITE     CN10C02
073300930915     C                   SETOFF                                       87
073400930812     C                   ENDSR
073500930812     C*
073600930812     C*--- CARICAMENTO SUBFILE ---------------------------------------*
073700930812     C     CARSF         BEGSR
073800011109     C*---------------------------------------------------------------*
073900930812     C                   Z-ADD     0             NRR               4 0
074000930812     C*
074100940905     C  N03
074200011029     CANN05KFGT          READE(N)  fifgt000                               31
074300940905     C   03
074400011029     CANN05KFGT1         READE(N)  fifgt000                               31
074500940905     C   03
074600011029     CAN 05KFGT2         READE(N)  fifgt000                               31
074700930812     C*
074800930825     C     *IN31         DOWEQ     *OFF
074900081223     C* per aut il progressivo viene passato dalla supertestata
075000090109     c* quindi carico solo quello
075100081223     c                   if        cn02ana = 'A' and fgtprg <> cn02prg
075200090108     c                             and cn02prg <> *hival
075300081223     c                   goto      nocar
075400081223     c                   end
075500090428     C* per aut se interrogazione fino al 30/4/2009 ometto le tariffe
075600090428     c* con data >
075700090428     c                   if        cn02ana = 'A' and fgtddt > 20090430
075800090429     c                             and (kcdaz = 'FN11' or
075900090429     c                             kcdaz = 'FN1B') and
076000090429     c                             %subst(knmus: 1: 3) <> 'EDP'
076100090428     c                   goto      nocar
076200090428     c                   end
076300930819     C* ULTIMO PROGRESSIVO LETTO (COMPRESI QUELLI ANNULLATI)
076400930812     C                   Z-ADD     FGTPRG        LETPRG            3 0
076500930812     C     FGTATB        IFEQ      ' '
076600930820     C* CAMPO SELEZIONE
076700930819     C                   MOVEL     ' '           VI2SCE
076800931104     C* TIPO PRESTAZIONE
076900930830     C                   CLEAR                   SER
077000930927     C                   MOVEA     FGTTSR        SER(1)
077100930830     C                   MOVEA     '-'           SER(2)
077200930903     C                   Z-ADD     1             X
077300930927     C     FGTTSR        LOOKUP    TPS(X)                                 35
077400021210     c  n35              goto      nocar
077500930903     C   35              MOVEA     DSR(X)        SER(3)
077600930927     C   35              MOVEA     SER           VI2TSR
077700930819     C                   MOVEL     FGTCTR        VI2CTR
077800930819     C                   MOVEL     FGTPRG        VI2PRG
077900081223     C                   MOVEL     FGTdiv        VI2div
078000940906     C                   Z-ADD     FGTDDT        DATA
078100940906     C                   Z-ADD     MM            GGMM              4 0
078200940906     C                   MOVEL     GG            GGMM
078300940906     C                   Z-ADD     AA            VI2DDT
078400940906     C                   MOVEL     GGMM          VI2DDT
078500940906     C                   Z-ADD     FGTDST        DATA
078600940906     C                   Z-ADD     MM            GGMM
078700940906     C                   MOVEL     GG            GGMM
078800940906     C                   Z-ADD     AA            VI2DST
078900940906     C                   MOVEL     GGMM          VI2DST
079000940906     C                   Z-ADD     FGTDUV        DATA
079100940906     C                   Z-ADD     MM            GGMM
079200940906     C                   MOVEL     GG            GGMM
079300940906     C                   Z-ADD     AA            VI2DUV
079400940906     C                   MOVEL     GGMM          VI2DUV
079500081229     c* se confermato disabilito il cmd 9 per inserire nuove tariffe
079600081229     c                   if        fgtdts > 0 and cn02ana = 'A'
079700081229     c                   seton                                        17
079800081229     c                   end
079900930819     C                   MOVEL     FGTDCV        VI2DCV
080000930812     C                   ADD       1             NRR
080100020124     C                   WRITE     CN10S02
080200930812     C                   ENDIF
080300011126     c     nocar         tag
080400930812     C*
080500940905     C  N03
080600011029     CANN05KFGT          READE(N)  fifgt000                               31
080700940905     C   03
080800011029     CANN05KFGT1         READE(N)  fifgt000                               31
080900940905     C   03
081000011029     CAN 05KFGT2         READE(N)  fifgt000                               31
081100930812     C                   ENDDO
081200930820     C*
081300930812     C                   ENDSR
081400930826     C*
081500020125     C*--- GESTIONE DELLA TERZA VIDEATA ------------------------------*
081600020125     C     VIDEO3        BEGSR
081700020125     C*---------------------------------------------------------------*
081800020125     C*
081900020125     C* PULIZIA SFL DETTAGLIO
082000020125     C                   EXSR      PULVI3
082100090114     c                   setoff                                       131715
082200090114     c                   setoff                                       18
082300020125     C* CARICO DATI A VIDEO
082400081223     c                   clear                   v5km
082500090401     c                   clear                   v5pkl
082600090109     c* se aut carico i dati dalla supertestata
082700090108     c                   if        cn02ana = 'A' and cn02prg <> *hival
082800081223     C     Ktgt          CHAIN     fitgt000
082900081223     c                   if        %found(fitgt01l)
083000081223     c                   move      tgtprg        vi3prg
083100081223     c                   if        Tgtddt > 0
083200081223     c                   move      tgtddt        dataiso
083300081223     c                   move      dataiso       dataeur
083400081223     c                   move      dataeur       v5ddt
083500081223     c                   else
083600081223     c                   z-add     0             v5ddt
083700081223     c                   end
083800081223     c                   if        Tgtdst > 0
083900081223     c                   move      Tgtdst        dataiso
084000081223     c                   move      dataiso       dataeur
084100081223     c                   move      dataeur       v5dst
084200081223     c                   else
084300081223     c                   z-add     31122039      v5dst
084400081223     c                   end
084500081223     c                   if        Tgtduv > 0
084600081223     c                   move      Tgtduv        dataiso
084700081223     c                   move      dataiso       dataeur
084800081223     c                   move      dataeur       v5duv
084900081223     c                   else
085000081223     c                   z-add     0             v5duv
085100081223     c                   end
085200090325     c                   setoff                                       17
085300081223     c                   if        Tgtdts > 0
085400090324     c                   seton                                        17
085500081223     c                   move      tgtdts        dataiso
085600081223     c                   move      dataiso       dataeur
085700081223     c                   move      dataeur       v5dts
085800081223     c                   else
085900081223     c                   z-add     0             v5dts
086000081223     c                   end
086100090424     c                   if        Tgtdrc > 0
086200090424     c                   move      Tgtdrc        dataiso
086300081223     c                   move      dataiso       dataeur
086400081223     c                   move      dataeur       v5dcn
086500081223     c                   else
086600081223     c                   z-add     0             v5dcn
086700081223     c                   end
086800081223     c                   movel     tgtdcv        v5dcv
086900081223     c                   z-add     tgtkm         v5km
087000090401     c                   z-add     tgtpkl        v5pkl
087100081229     c                   seton                                        18
087200081223     c                   end
087300081223     c                   end
087400081223     c*
087500081223     C     KFGT3         CHAIN     fifgt000                           3236
087600020125     C*
087700020125     C* 36 ON  - RECORD ALLOCATO
087800020125     C     *IN36         IFEQ      *ON
087900020125     C                   SETON                                        5029
088000020125     C                   MOVEL     '2'           ALF1
088100020125     C                   GOTO      ENDVD3
088200020125     C                   ENDIF
088300020125     C*
088400020219     c                   if        *in32
088500020219     c                   clear                   fgtds
088600020219     c                   move      tsr           vi3tsr
088700020219     c                   move      ctr           vi3ctr
088800020219     c                   move      prg           vi3prg
088900081223     c                   move      viddiv        vi3div
089000020219     c                   else
089100090109     c* Controllo se abilitato alla manutenzione
089200020329     c* solo se è impostata la data di stampa
089300020509     c                   if        fgtdts > 0 and fgtsml = *blank
089400131008     c                   clear                   tibs42ds
089500131008     c                   movel     vidfil        i42pge
089700131008     c                   call      'TIBS42R'
089800131008     c                   parm                    tibs42ds
089900131008     c                   movel     o42uni        uteautds
090000080929      *
090100020329      * se abilitato con *in13 abilito la manutenzione della data scadenza
090200080926      * con *in15 proteggo gli altri campi
090300021211      * differisco i controlli autorizzazioni dal Flag dell'anagrafica
090400021211      * A / C
090500080929     c                   seton                                        15
090600090108     c                   if        cn02ana = 'A'
090700090108      * Non abilitato allora proteggo il campo
090800090108     c                   if        §autmtc <>'S'
090900090108     c                   seton                                        13
091000090108     c                   end
091100090108     c                   else
091200080929      * Non abilitato allora proteggo il campo
091300080929     c                   if        §autmtcc <>'S'
091400080929     c                   seton                                        13
091500090108     c                   end
091600021211     c                   end
091700081223     c                   end
091800020219      *record esistente
091900020219     c                   movel     fgtpdr        vidfil
092000020219     c                   move      fgtpdr        vidpdr
092100020219     c                   move      fgttsr        vi3tsr
092200020219     c                   move      fgtctr        vi3ctr
092300081223     c                   move      fgtdiv        vi3div
092400020219     c                   move      fgtprg        vi3prg
092500020219     c                   endif
092600020219
092700020219     c                   exsr      condiz
092800020205     C                   EXSR      CARVI3
092900020125     C*
093000020125     C*
093100020125     C     ENDVD3        ENDSR
093200930910     C*--- PULIZIA SUBFILE RIGHE DI DETTAGLIO ------------------------*
093300020125     C     PULVI3        BEGSR
093400011109     C*---------------------------------------------------------------*
093500930915     C                   SETON                                        88
093600020124     C                   WRITE     CN10C05
093700930915     C                   SETOFF                                       88
093800930917     C*
093900931008     C                   Z-ADD     0             NR2               4 0
094000931008     C                   Z-ADD     0             SAVNR2            4 0
094100930910     C                   ENDSR
094200930910     C*
094300930910     C*--- CARICAMENTO SUBFILE RIGHE DI DETTAGLIO --------------------*
094400020125     C     CARVI3        BEGSR
094500011109     C*---------------------------------------------------------------*
094600930914     C*
094700081223     c                   if        cn02ana= 'A'
094800020125     C                   exsr      car8a
094900021209     c                   else
095000021209     c                   exsr      car8ac
095100021209     c                   end
095200021210     c                   if        almeno_uno <> *blank
095300020125
095400020125     c                   do        50            xx
095500020125     c                   movel     cdpar(xx)     cdpards
095600020125     c                   if        ctd ='999'
095700020125     c                   leave
095800020125     c                   endif
095900020125     c                   movel     ctd           wkctd
096000020125     c
096100020125     c     kfpt          chain     fifpt01l                           91
096200020125     c                   move      ctd           v5ctd
096300020125     c                   movel     dtd           v5des
096301130619      *
096302130619     c                   if        fptatb = 'A'
096303130619     c                   iter
096304130619     c                   endif
096400020218     c
096500020220     c                   move      *blank        v5att
096600020218     c                   if        not *in91 and fptatb = *blank
096700020218     c                   move      'S'           v5att
096800020218     c                   end
096801130614     c                   setoff                                       76
096802130617     c                   if        esc = 'N' and *in01 or
096803130617     c                             esc = 'N' and *in91
096804130617     c                   iter
096805130617      *76 on protegge opzione
096806130617     c*                  move      *blank        v5att
096807130617     c*                  seton                                        76
096808130614     c                   endif
096900080926      *
097000020125     c                   if        fba = '1'
097100020125     c                   move      'B'           v5bas
097200020125     c                   else
097300020125     c                   move      *blank        v5bas
097400020125     c                   end
097500020125
097600020125     C                   ADD       1             NR2
097700020125     C                   WRITE     CN10S05
097800020125     C                   ENDdo
097900020125     C                   z-add     nr2           savNR2
098000090109     c* per coop o per aut prima della supertestata
098100090108     c                   if        cn02ana ='C' or
098200090108     c                             (cn02ana ='A' and cn02prg = *hival)
098300020129     c                   if        fgtddt > 0
098400020129     c                   move      fgtddt        dataiso
098500020129     c                   move      dataiso       dataeur
098600020129     c                   move      dataeur       v5ddt
098700020130     c                   else
098800020130     c                   z-add     0             v5ddt
098900020129     c                   end
099000020129     c                   if        fgtdst > 0
099100020129     c                   move      fgtdst        dataiso
099200020129     c                   move      dataiso       dataeur
099300020129     c                   move      dataeur       v5dst
099400020130     c                   else
099500030304     c                   z-add     31122039      v5dst
099600020129     c                   end
099700020129     c                   if        fgtduv > 0
099800020129     c                   move      fgtduv        dataiso
099900020129     c                   move      dataiso       dataeur
100000020129     c                   move      dataeur       v5duv
100100020130     c                   else
100200020130     c                   z-add     0             v5duv
100300020129     c                   end
100400020129     c                   if        fgtdts > 0
100500020129     c                   move      fgtdts        dataiso
100600020129     c                   move      dataiso       dataeur
100700020129     c                   move      dataeur       v5dts
100800020130     c                   else
100900020130     c                   z-add     0             v5dts
101000020129     c                   end
101100090109     c* se confermato disabilito l'aggiunta
101200090429     c                   if        cn02ana ='C'
101300090429     c                   setoff                                       17
101400090429     c                   end
101500020129     c                   if        fgtdcn > 0
101600090429     c                   seton                                        17
101700020129     c                   move      fgtdcn        dataiso
101800020129     c                   move      dataiso       dataeur
101900020129     c                   move      dataeur       v5dcn
102000020130     c                   else
102100020130     c                   z-add     0             v5dcn
102200020129     c                   end
102300020327     c                   movel     fgtdcv        v5dcv
102400090109     c* per aut con supertestata
102500081223     c                   else
102600081223     c                   move      tgtprg        vi3prg
102700081223     c                   if        Tgtddt > 0
102800081223     c                   move      tgtddt        dataiso
102900081223     c                   move      dataiso       dataeur
103000081223     c                   move      dataeur       v5ddt
103100081223     c                   else
103200081223     c                   z-add     0             v5ddt
103300081223     c                   end
103400081223     c                   if        Tgtdst > 0
103500081223     c                   move      Tgtdst        dataiso
103600081223     c                   move      dataiso       dataeur
103700081223     c                   move      dataeur       v5dst
103800081223     c                   else
103900081223     c                   z-add     31122039      v5dst
104000081223     c                   end
104100081223     c                   if        Tgtduv > 0
104200081223     c                   move      Tgtduv        dataiso
104300081223     c                   move      dataiso       dataeur
104400081223     c                   move      dataeur       v5duv
104500081223     c                   else
104600081223     c                   z-add     0             v5duv
104700081223     c                   end
104800090324     c                   setoff                                       17
104900090324     c* se confermato disabilito l'aggiunta
105000081223     c                   if        Tgtdts > 0
105100090324     c                   seton                                        17
105200081223     c                   move      tgtdts        dataiso
105300081223     c                   move      dataiso       dataeur
105400081223     c                   move      dataeur       v5dts
105500081223     c                   else
105600081223     c                   z-add     0             v5dts
105700081223     c                   end
105800090324     c*
105900090424     c                   if        Tgtdrc > 0
106000090424     c                   move      Tgtdrc        dataiso
106100090324     c                   move      dataiso       dataeur
106200090324     c                   move      dataeur       v5dcn
106300090324     c                   else
106400090324     c                   z-add     0             v5dcn
106500090324     c                   end
106600090324     c*
106700081223     c                   movel     tgtdcv        v5dcv
106800081223     c                   z-add     tgtkm         v5km
106900090401     c                   z-add     tgtpkl        v5pkl
107000081223     c                   end
107100021210
107200021210     c                   endif
107300021210
107400020125     C                   IF        NR2 > 0
107500020125     C                   EXSR      EMISF2
107600020125     C                   END
107700020125
107800020125     C                   ENDSR
107900020125     C*--- EMISSIONE SFL DETTAGLIO  --------------------*
108000020125     C     EMISF2        BEGSR
108100020125     C*-------------------------------------------------*
108200020205     c                   do        *hival
108300020206     c                   clear                   ricar
108400020125     C                   Z-ADD     1             REC2
108500020125     C                   WRITE     CN10Z05
108600020125     C     FORC2         TAG
108700020128     C                   EXFMT     CN10C05
108800030916     c                   setoff                                       44
108900020125     C*
109000020125     C** CMD12 - RITORNO
109100020125     C     *INKL         IFEQ      *ON
109200020125     C                   MOVEL     '1'           ALF1
109300020125     C                   GOTO      ENDVD4
109400020125     C                   ENDIF
109500080926      * in interrogazione non deve eseguire i controlli
109600080926     C  n10              exsr      contrfgt
109700030121
109800020130     c   90              goto      forc2
109900080929     c  n13
110000020411     Corn10*INKf         IFEQ      *ON
110100020130     C                   exsr      aggfgt
110200030303     c                   if        *in54 or *in60
110300030121     c                   goto      forc2
110400030121     c                   endif
110500090108     c                   commit
110600020130     C                   ENDIF
110700020327     c                   if        *in01
110800020327     c                   seton                                        56
110900020327     c                   goto      forc2
111000020327     c                   else
111100020327     c                   setoff                                       56
111200020327     c                   endif
111300020208     C** CMD16 - ANNULLAMENTO TOTALE TARIFFE
111400080929     c  n13
111500020411     Corn10*INKQ         IFEQ      *ON
111600101209     c                   if        %subst(knmus: 1: 3) = 'EDP'
111700101209     c                             and v5dts = 0
111800100907     c**
111900100907     c* Si è voluto permettere a TUTTI di cancellare le tariffe.
112000101209     c*                  IF            v5dts = 0
112100100907     c**
112200081229     c                   if        cn02ana='C'
112300020211     C                   call      'FICN12R'
112400020211     C                   parm                    COMPDR
112500020211     C                   parm                    COMSML
112600020211     C                   parm                    TSR
112700020211     C                   parm                    CTR
112800020211     C                   parm                    PRG
112900081229     c                   else
113000081229     C                   call      'FICN04R'
113100081229     C                   parm                    COMPDR
113200081229     C                   parm                    COMSML
113300081229     C                   parm                    PRG
113400090113     C                   parm                    TSR
113500090113     C                   parm                    CTR
113600081229     c                   end
113700090107     c                   commit
113800030916     c                   else
113900030916     c                   seton                                        44
114000030916     c                   goto      forc2
114100030916     c                   endif
114200020208     C*
114300090107     C                   SETOFF                                       11
114400020423     C                   MOVEL     '1'           ALF1
114500020208     C                   GOTO      ENDVD4
114600020208     C                   ENDIF
114700020125     c                   do        savnr2        nr2
114800020125     c     nr2           chain     cn10s05                            79
114900020125     c   79              leave
115000020424     c                   if        v5opz <> *blank
115100080926      **
115200090918      ** CHIODAZZO BRUTTO MA EFFICACE:
115300090918      **  Attenzione se immessa tariffa "XT" deve esserci la tar."X" altrimenti
115400090918      **     NON DEVE PERMETTERLO:
115500090918     c                   if        v5CTD = 'XT'
115600090918     c                   movel     'X  '         wkctd
115700090923     c     kfpt1         chain     fifpt01l
115800090923     C                   IF        NOT %FOUND(fifpt01l) or
115900090923     C                             (%FOUND(fifpt01l) and fptatb <> ' ')
116000090918     C                   SETON                                        9077
116100090918     c                   goto      forc2
116200090918     c                   end
116300090918     c                   end
116400090918      **
116500100907      ***
116600100907     c                   clear                   a_giornata        1
116700100907      *** Controlla se la selezione scelta è compatibile con le tariffe di
116800100907      ***  dettaglio. NON si possono inserire tariffe a GIORNATA con dettaglio
116900100907      ***  già presente!
117000100907     C*  VA FATTO CON IL CONTROLLO DELLA SCELTA DEL SFL!!!!!
117100100907     c                   if        cn02ana  <>'C' and
117200100907     c                              ( not *in10 or *in17)  and
117300100907     c                              ( v5CTD = 'TIG' or v5CTD = 'TMG' )
117400100907     C* vede se c'è una tariffa a dettaglio
117500100907     C                   movel     'C'           tipoDet           1
117600100907     c     ricontrolla   tag
117700100907     c     kfpt1_det     setll     fifpt01l
117800100907     c     kfpt1_det     reade     fifpt01l
117900100907     C*
118000100907     c                   dow       not %Eof(fifpt01l)
118100100907     c                   if        fptprg = vi3prg
118200100907     c                   move      'S'           a_giornata
118300100907     c                   leave
118400100907     c                   end
118500100907     c     kfpt1_det     reade     fifpt01l
118600100907     c                   enddo
118700100907     C*
118800100907     c                   if         a_giornata <> 'S' and tipoDET = 'C'
118900100907     C                   movel     'R'           tipoDet           1
119000100907     c                   goto      ricontrolla
119100100907     c                   end
119200100907      *
119300100907     C*  se presente già un'altra tariffa di dettaglio
119400100907     C*     sullo stesso tipo servizio/progressivo
119500100907     c                   if        a_giornata  = 'S'
119600100907     C                   SETON                                        7890
119700100907     c                   goto      forc2
119800100907     c                   end
119900100907     C*
120000100907     c                   end
120100090918      **
120200080926      **  se Interrogazione e non c'è "S" non deve entrare sulla tariffa specifica
120300090107      **  se già stampato e non c'è "S" non deve entrare sulla tariffa specifica
120400080926     c                   if        v5ATT <> 'S'
120500080926     c                   if        *in10 or
120600080926     c                             *in17
120700080926     c                   goto      nodettaglio
120800080926     c                   end
120900080926     c                   end
121000020125     c                   clear                   param4
121100020130     c                   move      vidtes        PA4TES
121200081223     c                   move      cn02mod       modali
121300080926     c   15
121400080926     cor 17
121500080926     cor 10              move      '*'           modali
121600020128     c                   movel     vidfil        PA4PDR
121700020125     c                   move      vidpdr        PA4PDR
121800020125     c                   move      despdr        PA4DPD
121900020213     c                   move      comsml        PA4SML
122000020125     c                   move      vi3tsr        PA4TSR
122100020128     c                   move      de3tsr        PA4DTS
122200020125     c                   move      vi3ctr        PA4CTR
122300020128     c                   move      de3ctr        PA4DCT
122400081223     c                   move      cn02ana       PA4anac
122500020130     c                   move      vi2dcv        PA4DCV
122600020125     c                   move      vi3prg        PA4PRG
122700020125     c                   move      *blank        PA4FLG
122800020125     c                   move      v5duv         PA4DUV
122900020219     c                   move      comddt        PA4DDT
123000081223     c                   move      viddiv        PA4div
123100020128     c                   move      v5ctd         PA4ctd
123200020128     c                   move      v5des         PA4dtd
123300020125
123400020125     c                   movel     param4        kpjbu
123500020128     c                   call      'FICN11R'
123600020125     c                   parm                    kpjba
123700020228     C                   MOVEL     savKPJBU      KPJBU
123800090114     c                   if        not *in15 and not *in17 and not *in10
123900090107     C                   COMMIT
124000090114     c                   end
124100080926      **
124200080926     c     nodettaglio   tag
124300020125     c                   move      *blank        v5opz
124400020125     c                   update    cn10s05
124500020206     c                   move      'X'           ricar
124600020424     c                   endif
124700020125     c                   enddo
124800020206      * se sono state effettuate delle scelte per abilitare dei codici di
124900020206      * dettaglio forzo a fine loop delle opzioni scelte l'uscita per ricaricare
125000020206      * il sfl dall'inizio
125100020206     c                   if        ricar <> *blank
125200020206     c                   leave
125300020206     c                   end
125400020205     c                   enddo
125500020125     C     endvd4        ENDSR
125600930910     C*
125700930914     C*--- CONTROLLO DATI SUBFILE RIGHE DI DETTAGLIO -----------------*
125800020125     C     CONSF2        BEGSR
125900011109     C*---------------------------------------------------------------*
126000020206     C                   SETOFF                                       90
126100930914     C                   Z-ADD     1             NR2
126200930914     C*
126300020124     C     NR2           CHAIN     CN10S05                            33
126400930914     C     *IN33         DOWEQ     *OFF
126500930914     C     ENDSF2        TAG
126600020124     c                   end
126700930914     C   90              Z-ADD     NR2           REC2
126800930914     C*
126900020124     C                   UPDATE    CN10S05
127000020124     c
127100931119     C*
127200011126     C     endrt2        ENDSR
127300930826     C*
127400020124     C*--- ----------------------------------------- -----------------*
127500020124     C     Car8a         BEGSR
127600020124     C*---------------------------------------------------------------*
127700020124     c
127800021210     c                   clear                   almeno_uno
127900020125     c                   move      *all'9'       cdpar
128000020124     C                   MOVEL     '8A'          COD
128100020124     c                   z-add     1             xx
128200020124     c     ktab2         setll     tabel00f
128300020124     c                   do        *hival
128400020130     c     ktab2         reade     tabel00f                               94
128500020130     c   94              leave
128600021210     c                   move      'X'           almeno_uno
128700020204     c                   clear                   carok
128800020124     c                   movel     tbluni        ds8a
128900020124     c
129000020204      *controllo il livello della tariffa per caricare i dettagli abilitati
129100020208     c                   if        vi3tsr = 'G' and §8afgi <> *blank
129200020204     c                   move      '1'           carok
129300020124     c                   endif
129400020124     c
129500020327     c                   if        vi3ctr = 999 and §8afpr <> *blank
129600020327     c                             and vi3tsr <> 'G'
129700020204     c                   move      '1'           carok
129800020124     c                   endif
129900020124     c
130000020208     c                   if        vi3ctr < 999 and §8afde <> *blank
130100020204     c                   move      '1'           carok
130200020124     c                   endif
130300020204
130400020204      *se il livello è buono controllo consegna e ritiro se specifici
130500020208     c                   if        carok ='1' and vi3tsr = 'C' and §8ausc <> 'S'
130600020204     c                   clear                   carok
130700020204     c                   endif
130800020204
130900020208     c                   if        carok ='1' and vi3tsr = 'R' and §8ausr <> 'S'
131000020204     c                   clear                   carok
131100020204     c                   endif
131200020204      * se non valido leggo il successivo
131300020204     c                   if        carok = *blank
131400020204     c                   iter
131500020204     c                   endif
131600020204
131700020125     c                   move      *all'9'       cdpards
131800020125     c     cdpards       lookup    cdpar(xx)                              87
131900020124     c                   movel     tblkey        ctd
132000020124     c                   movel     §8ades        dtd
132001130614     c                   move      §8aesccar     esc
132100020124     c                   if        §8atba = 'B'
132200020124     c                   move      '1'           fba
132300020124     c                   else
132400020124     c                   movel     '2'           fba
132500020124     c                   end
132700020124
132800020124     c                   movea     cdpards       cdpar(xx)
132900020124
133000020124     c                   enddo
133100020124
133200020124     c                   sorta     cdpar
133300020124
133400020124     c                   endsr
133500021209     C*--- ----------------------------------------- -----------------*
133600021209     C     Car8ac        BEGSR
133700021209     C*---------------------------------------------------------------*
133800050411      *    Controllo se la filiale è una HUB DPD
133900050411     c                   exsr      HUB_DPD
134000050411      *
134100021218      * prima di caricare determino se la filiale che sto gestendo è un primo
134200021218      * o secondo livello per gestire le voci specifiche
134300030122     c     vidfil        chain     azorg01l
134400030122     c                   movel     orgde3        og143
134500030122
134600021218     c                   clear                   fnlv55ds
134700021218     c                   move      'L'           d55tla
134800021218     c                   move      vidfil        d55lin
134900070131     c                   move      ' '           d55tpt
135000021218     c                   z-add     wuda          d55drf
135100021218     c                   call      'FNLV55R'
135200021218     c                   parm                    fnlv55ds
135300021209     c
135400021210     c                   clear                   almeno_uno
135500021209     c                   move      *all'9'       cdpar
135600021209     C                   MOVEL     '8A'          COD
135700021209     c                   z-add     1             xx
135800021209     c     ktab2         setll     tabel00f
135900021209     c                   do        *hival
136000021209     c     ktab2         reade     tabel00f                               94
136100021209     c   94              leave
136200030122     c                   if        tblflg <> ' '
136300030122     c                   iter
136400030122     c                   end
136500021210     c                   move      'X'           almeno_uno
136600021209     c                   clear                   carok
136700021209     c                   movel     tbluni        ds8a
136800021209     c
136900021209     c                   if        §8afac <> 'C'
137000021209     c                   iter
137100021209     c                   end
137200030123      *  se voce specifica per le poste la carica solo se il network è poste
137300030123     c                   if        §8afpt = 'P' and §ogntw <> 'PPT'
137400030123     c                   iter
137500030123     c                   endif
137600030123      *  se specifica NON POSTE la carica solo se il network NON è poste
137700030123     c                   if        §8afpt = 'N' and §ogntw = 'PPT'
137800030123     c                   iter
137900030123     c                   endif
138000021218      * controllo validità voce per 1° 2° livello o entrambe
138100021218      * se non è valido per entrambe
138200030123     c                   if        §8a12e <>'E'
138300021218      *  se primo liv. e se la voce non è specifica per il primo liv.
138400050411      *   e se non è una voce specifica x DPD di HUB primo livello
138500070131     c                   if        §8a12e <> '1'  and
138600070131     c                             (vidfil = d55tfp or vidfil = d55tfa)
138700050411      *** (1) livello
138800050411      * se è una tariffa DPD e non è una HUB che gestisce DPD
138900050411     c                   if        §8a12e = 'D' and Ntw_DPD_SN = 'N' or
139000050411     c                             §8a12e <>'D'
139100050411     c                   iter
139200050411     c                   endif
139300021218     c                   endif
139400021218      *  se secondo liv. e se la voce non è specifica x il secondo liv.
139500070131     c                   if        §8a12e <> '2' and
139600070131     c                             (vidfil <> d55tfp and vidfil <> d55tfa)
139700021218     c                   iter
139800021218     c                   endif
139900050411     c                   else
140000021218     c                   endif
140100021209      *controllo il tipo della tariffa per caricare i dettagli abilitati
140200021209     c                   if        vi3tsr = 'X' and §8ausc = 'X'
140300021209     c                   move      '1'           carok
140400021209     c                   endif
140500021209     c
140600021209     c                   if        vi3tsr = 'P' and §8ausc = 'P'
140700021209     c                   move      '1'           carok
140800021209     c                   endif
140900021209     c
141000021209     c                   if        vi3tsr = 'T' and §8ausc = 'T'
141100021209     c                   move      '1'           carok
141200021209     c                   endif
141300021209     c
141400021209      * se non valido leggo il successivo
141500021209     c                   if        carok = *blank
141600021209     c                   iter
141700021209     c                   endif
141800021209
141900021209     c                   move      *all'9'       cdpards
142000021209     c     cdpards       lookup    cdpar(xx)                              87
142100021209     c                   movel     tblkey        ctd
142200021209     c                   movel     §8ades        dtd
142201130614     c                   movel     §8aesccar     esc
142300021209     c                   if        §8atba = 'B'
142400021209     c                   move      '1'           fba
142500021209     c                   else
142600021209     c                   movel     '2'           fba
142700021209     c                   end
142800021209     c
142900021209
143000021209     c                   movea     cdpards       cdpar(xx)
143100021209
143200021209     c                   enddo
143300021209
143400021209     c                   sorta     cdpar
143500021209
143600021209     c                   endsr
143700020130     C*---------------------------------------------------------------*
143800020130     C     contrfgt      begsr
143900020130     C*---------------------------------------------------------------*
144000020130     C                   SETOFF                                       90
144100100907      ***
144200020130     C****  DATA DECORRENZA TARIFFA  ****
144300020130     C                   Z-ADD     V5ddt         G02DAT
144400020130     C                   MOVEL     *BLANKS       G02ERR
144500020130     C                   CALL      'XSRDA8'
144600020130     C                   PARM                    WLBDAT
144700020130     C     G02ERR        IFEQ      '1'
144800020130     C                   SETON                                        4590
144900020130     C                   GOTO      ENDCT3
145000020130     C                   ELSE
145100020130     C                   MOVE      G02INV        COMDDT
145200020130     C                   ENDIF
145300020130     C*
145400020130     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6
145500020130     C                   Z-ADD     G02DAT        V5ddt
145600020130     C*
145700020130     C****  DATA SCADENZA TARIFFA  ****
145800020130     C                   Z-ADD     V5dst         G02DAT
145900020130     C                   MOVEL     *BLANKS       G02ERR
146000020130     C                   CALL      'XSRDA8'
146100020130     C                   PARM                    WLBDAT
146200020130     C     G02ERR        IFEQ      '1'
146300020130     C                   SETON                                        4690
146400020130     C                   GOTO      ENDCT3
146500020130     C                   ELSE
146600020130     C                   MOVE      G02INV        COMDST
146700020130     C                   ENDIF
146800020130     C*
146900020130     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6
147000020130     C                   Z-ADD     G02DAT        V5DST
147100020130     C*
147200020130     C* LA DATA SCADENZA DEVE ESSERE MAGGIORE DELLA DATA DECORRENZA
147300020130     C     COMDST        IFLT      COMDDT
147400020130     C                   SETON                                        4790
147500020130     C                   GOTO      ENDCT3
147600020130     C                   ENDIF
147700020423      *verifica se esistono conteggi valorizzati o confermati x la data scadenza
147800040218      * solo per gli aut
147900081223     c                   if        cn02ana<> 'C'
148000080926      *
148100020423     c                   move      'C'           klvl
148200020423     c     kftt          setgt     fiftt02l
148300020423     c     kftt          readpe    fiftt02l                               59
148400020423     c                   if        not *in59
148500020607     c                   if        fttddc >  comdst
148600020403     c                   seton                                        6090
148700020403     C                   GOTO      ENDCT3
148800020422     c                   endif
148900020403     c                   endif
149000020423     c                   move      'V'           klvl
149100020423     c     kftt          setgt     fiftt02l
149200020423     c     kftt          readpe    fiftt02l                               59
149300020423     c                   if        not *in59
149400020607     c                   if        fttddc >  comdst
149500020423     c                   seton                                        6090
149600020423     C                   GOTO      ENDCT3
149700020423     c                   endif
149800020423     c                   endif
149900040218     c                   endif
150000020130      *
150100020130     C* LA DATA SCADENZA DEL PROGRESSIVO PRECEDENTE DEVE ESSERE MINORE
150200020130     C*   DELLA DATA DECORRENZA DEL PROGRESSIVO SEGUENTE
150300020130     C     PRG           ADD       1             COMPRG
150400020130     C     KFGT5         SETLL     fifgt000
150500020130     C     KFGT5         READE(N)  fifgt000                               32
150600020130     C*
150700020130     C     *IN32         DOWEQ     *OFF
150800020130     C     FGTATB        IFEQ      ' '
150900020130     C     COMDST        IFGE      FGTDDT
151000020130     C                   SETON                                        4890
151100020130     C                   GOTO      ENDCT3
151200020130     C                   ELSE
151300020130     C                   SETON                                        32
151400020130     C                   ENDIF
151500020130     C*
151600020130     C                   ELSE
151700020130     C* RECORD ANNULLATO: AGGIUNGO 1 E CERCO IL PROGRESSIVO SEGUENTE
151800020130     C                   ADD       1             COMPRG
151900020130     C                   ENDIF
152000020130     C*
152100020130     C  N32KFGT5         READE(N)  fifgt000                               32
152200020130     C                   ENDDO
152300020130     C*
152400020130     C* LA DATA DECORRENZA DEL PROGRESSIVO SEGUENTE DEVE ESSERE
152500020130     C*   MAGGIORE DELLA DATA SCADENZA DEL PROGRESSIVO PRECEDENTE
152600020130     C     PRG           SUB       1             COMPRG
152700020130     C     KFGT5         SETLL     fifgt000
152800020130     C     KFGT5         READE(N)  fifgt000                               32
152900020130     C*
153000020130     C     *IN32         DOWEQ     *OFF
153100020130     C     FGTATB        IFEQ      ' '
153200020130     C     COMDDT        IFLE      FGTDST
153300020130     C                   SETON                                        4990
153400020130     C                   GOTO      ENDCT3
153500020130     C                   ELSE
153600020130     C*
153700020130     C* LA DATA DECORRENZA PROGRESSIVO SEGUENTE DEVE ESSERE MAGGIORE
153800020130     C*   DI 1 GIORNO DELLA DATA SCADENZA PROGRESSIVO PRECEDENTE
153900020130     C                   ADD       1             FGTDST
154000020130     C                   Z-ADD     FGTDST        DATA
154100020130     C* CONTROLLO SE ANNO BISESTILE
154200020130     C     MM            IFEQ      2
154300020130     C     AA            DIV       4             G02QUA            2 0                        GBRM20
154400020130     C                   MVR                     G02RES            1 0                        GBRM20
154500020130     C     G02RES        IFEQ      0                                                        5 GBRM20
154600020130     C                   Z-ADD     29            XDTF(2)
154700020130     C                   ENDIF
154800020130     C                   ENDIF
154900020130     C*
155000020130     C     GG            IFGT      XDTF(MM)
155100020130     C     GG            SUB       XDTF(MM)      GG
155200020130     C                   ADD       1             MM
155300020130     C     MM            IFEQ      13
155400020130     C                   ADD       1             AA
155500020130     C                   Z-ADD     1             MM
155600020130     C                   ENDIF
155700020130     C                   ENDIF
155800020130     C                   Z-ADD     28            XDTF(2)
155900020130     C*
156000020130     C     COMDDT        IFNE      DATA
156100020130     C                   SETON                                        4990
156200020130     C                   GOTO      ENDCT3
156300020130     C                   ENDIF
156400020130     C*
156500020130     C                   SETON                                        32
156600020130     C                   ENDIF
156700020130     C*
156800020130     C                   ELSE
156900020130     C* RECORD ANNULLATO: SOTTRAGGO 1 E CERCO IL PROGRESSIVO PRECEDENTE
157000020130     C                   SUB       1             COMPRG
157100020130     C* RIESEGUO POSIZIONAMENTO CON PROGRESSIVO PRECEDENTE
157200020130     C     KFGT5         SETLL     fifgt000
157300020130     C                   ENDIF
157400020130     C*
157500020130     C  N32KFGT5         READE(N)  fifgt000                               32
157600020130     C                   ENDDO
157700040827     **
157800020807      *verifica se il codice autista di riferimento esiste
157900020807     c                   setoff                                       2526
158000020610     c                   setoff                                       37
158100020130     c     endct3        endsr
158200030121     C*---------------------------------------------------------------*
158300030121     C     contrfgt2     begsr
158400030121     C*---------------------------------------------------------------*
158500090116     c                   clear                   paramt
158600081223     c                   if        *in32 and cn02ana= 'C' and vi3tsr = 'X'
158700030121     c                   z-add     comddt        datax
158800030121     c                   movel     vidfil        pogesx
158900030121     c                   movel     vi3tsr        tipopre
159000030121     c                   call      'FICN36R2'
159100030121     c                   parm                    paramt
159200030303     c                   endif
159300030303      * controllo per le coop che non ci siano valorizzazioni confermate
159400030303      * con data superiore alla data scadenza della tariffa
159500030303     c                   movel     fgtpdr        cttpo             3 0
159600030303     c     cttpo         setgt     fictt01l
159700030303     c                   do        *hival
159800030303     c     cttpo         readpe    fictt01l                               59
159900030303     c   59              leave
160000030303      * cerco il primo confermato andando all'indietro
160100030303     c                   if        cttfvl <> 'C' or ctttsr <> fgttsr
160200030303     c                   iter
160300030303     c                   endif
160400030303      * se lo trovo lo confronto con la data scadenza se è maggiore segnalo
160500030303      * l'errore
160600030303     c                   if        cttddc >  comdst
160700030303     c                   seton                                        6090
160800030303     C                   GOTO      ENDCT4
160900030303     c                   endif
161000030303     c                   enddo
161100030121
161200030303     c     endct4        endsr
161300020130     C*---------------------------------------------------------------*
161400020130     C     aggfgt        begsr
161500020130     C*---------------------------------------------------------------*
161600020130     C*
161700090116     c                   setoff                                       54
161800020130     C* 32 ON  - IMMISSIONE
161900020130     C* 32 OFF - AGGIORNAMENTO
162000081223     C     KFGT3         CHAIN     fifgt000                           32
162100020130     C   32              clear                   fifgt000
162200090109     C   32              clear                   comfgt
162300090109     C  n32              eval      comfgt = fgtds
162400030121      *se non c'è errore verifico x le coop l'esistenza di un'altra tariffa
162500030121      * valida nel periodo NON POSSONO ESISTERE 2 TARIFFE VALIDE ANCHE SE I
162600030121      *CODICI DELLE COOPERATIVE SONO DIVERSI
162700081223     c                   if        cn02ana= 'C'
162800030121     c                   exsr      contrfgt2
162900030303     c   60              goto      endagg
163000030121     c                   if        error <> *blank
163100030121     c                   seton                                        54
163200030121     c                   goto      endagg
163300030121     c                   endif
163400030303     c                   endif
163500081229     c*
163600020130     C                   MOVEL     ' '           FGTATB
163700020130     c                   if        v5ddt > 0
163800020130     c                   move      v5ddt         dataeur
163900020130     c                   move      dataeur       dataiso
164000020130     c                   move      dataiso       fgtddt
164100020130     c                   end
164200020130     c                   if        v5dst > 0
164300020130     c                   move      v5dst         dataeur
164400020130     c                   move      dataeur       dataiso
164500020130     c                   move      dataiso       fgtdst
164600020130     c                   end
164700020130     C                   MOVEL     VI2DCV        FGTDCV
164800020529     c                   movel     v5dcv         fgtdcv
164900020130     C* SOLO PER IMMISSIONE
165000020130     C     *IN32         IFEQ      *ON
165100020130     C                   Z-ADD     COMPDR        FGTPDR
165200020130     C                   MOVEL     COMSML        FGTSML
165300020130     C                   MOVEL     VI3TSR        FGTTSR
165400020130     C                   Z-ADD     VI3CTR        FGTCTR
165500020130     C                   Z-ADD     VI3PRG        FGTPRG
165600081229     C                   MOVEL     VIddiv        FGTdiv
165700100908     c********           clear                   fgtDTS
165800100908     c********           clear                   fgtDCN
165900020130     C                   ENDIF
166000020130     C*
166100020130     C* ESEGUO WRITE/UPDAT DI fifgt00F SOLO SE SONO STATI INSERITI O
166200020130     C*   VARIATI DEI CAMPI
166300020130     C     COMFGT        IFNE      FGTDS
166400020130     C                   Z-ADD     UDATE8        V5duv
166500020130     C                   Z-ADD     WUDA          FGTDUV                         *DATA ULT.VARIAZIONE
166600020130     C*
166700020130     C   32              WRITE     fifgt000
166800020130     C  N32              UPDATE    fifgt000
166900020130     C                   ENDIF
167000020130     C*
167100020130     C                   SETOFF                                       01
167200020130     C                   SETON                                        02
167300030121     C     endagg        ENDSR
167400020125     C*--- OPERAZIONI INIZIALI ---------------------------------------*
167500020125     C     OPERAZ        BEGSR
167600020125     C*---------------------------------------------------------------*
167700020125     C* TESTATA PRG
167800020125     C*
167900020125     C* ALF1 = '1' ===> GESTIONE VIDEATA DELLE SELEZIONI
168000020125     C                   MOVEL     '1'           ALF1              1
168100020125     C*
168200020125     C* 19 ON  - UTILIZZARE TARIFFE     DI SIMULAZIONE
168300020125     C* 19 OFF - UTILIZZARE TARIFFE NON DI SIMULAZIONE
168400081223     C     cn02sml       IFEQ      'S'
168500020125     C                   SETON                                        19
168600020125     C                   ENDIF
168700020125     C*
168800020125     C***
168900020125     C* GIRO DATA DEL GIORNO: LA PRENDO DA TIME
169000020125     C***
169100020125     C                   TIME                    W0120            14 0
169200020125     C                   MOVE      W0120         WDAT              8 0
169300020125     C*
169400020125     C                   Z-ADD     WDAT          G02DAT
169500020125     C                   MOVEL     *BLANK        G02ERR
169600020125     C                   CALL      'XSRDA8'
169700020125     C                   PARM                    WLBDAT
169800020125     C* UDATE A 8 IN AAAA/MM/GG
169900020125     C                   Z-ADD     G02INV        WUDA
170000020125     C* DATA DEL GIORNO IN GG/MM/AAAA
170100020125     C                   Z-ADD     G02DAT        UDATE8
170200020125     C* CARICAMENTO TABELLE
170300020125     C                   EXSR      CARTAB
170400020125     C* PULISCO CODICE PADRONCINO E DESCRIZIONE SOLO A INIZIO PGM
170500020125     C*   IN MODO CHE NELLA PRIMA VIDEATA RESTI IMPOSTATO L'ULTIMO
170600020125     C                   MOVEL     *BLANKS       VIDPDR
170700020125     C                   MOVE      '?'           VIDPDR
170800020125     C                   MOVEL     *BLANKS       DESPDR
170900090108     c                   if        cn02ana = 'A' and cn02pdr <> 0
171000081223     c                   movel     cn02pdr       vidfil
171100081223     c                   move      cn02pdr       vidpdr
171200081223     C                   MOVEL     VIDFIL        COMPDR
171300081223     C                   MOVE      VIDPDR        COMPDR
171400081223     C     kapdc         CHAIN     fiapd000                           32
171500090610     c                   if        not *in32 and not *in10
171600090610     C     APDATB        IFNE      ' '
171700081223     C                   SETON                                        32
171800081223     C                   ENDIF
171900090610     C                   ENDIF
172000081223     C* CODICE PADRONCINO ERRATO
172100081223     C   32              SETON                                        4090
172200100204      *
172300100204     c*  se escluso da autofatturazione dare msg di errore
172400100204     c                   if        apdpdd = 'S' and cn02ana<>'C'
172500100204     C                   SETON                                        4090
172600100204     c                   end
172700100204      *
172800100224     C                   MOVEL     APDRSF        DESPDR
172900100224     C********           MOVEL     APDRSC        DESPDR
173000100224      *
173100081223     c                   end
173200100224      *
173300020125     C                   ENDSR
173400020125     C*
173500020125     C*--- CARICAMENTO TABELLE ---------------------------------------*
173600020125     C     CARTAB        BEGSR
173700020125     C*---------------------------------------------------------------*
173800020125     C****  CODICI TARIFFA E DESCRIZIONI: TTR / DTR / MIN  ****
173900020125     C                   Z-ADD     1             X                 3 0
174000020125     C                   MOVEL     'TR'          COD
174100020125     C     KTAB2         SETLL     TABEL
174200020125     C     KTAB2         READE     TABEL                                  32
174300020125     C*
174400020125     C     *IN32         DOWEQ     *OFF
174500020125     C     TBLFLG        IFEQ      ' '
174600020125     C                   MOVEL     TBLUNI        DSTR
174700020125     C* CARICO SOLO I CODICI TARIFFA VALIDI PER LE TARIFFE PADRONCINI
174800081223     c                   if        cn02ana= 'A' and §trutp = 'S'
174900020125     C                   MOVEL     TBLKEY        TTR(X)
175000020125     C                   MOVEL     TBLUNI        DSTR
175100020125     C                   MOVEL     §TRDES        DTR(X)
175200020125     C* MINIMO TASSABILE PER TIPO TARIFFA
175300020125     C                   Z-ADD     §TRATA        MIN(X)
175400020125     C                   ADD       1             X
175500020125     C                   ENDIF
175600020125     C*
175700021209     C* CARICO SOLO I CODICI TARIFFA VALIDI PER LE TARIFFE COOP
175800081223     c                   if        cn02ana= 'C' and §trutp = 'C'
175900021209     C                   MOVEL     TBLKEY        TTR(X)
176000021209     C                   MOVEL     TBLUNI        DSTR
176100021209     C                   MOVEL     §TRDES        DTR(X)
176200021209     C* MINIMO TASSABILE PER TIPO TARIFFA
176300021209     C                   Z-ADD     §TRATA        MIN(X)
176400021209     C                   ADD       1             X
176500021209     C                   ENDIF
176600021209     C*
176700020125     C                   ENDIF
176800020125     C     KTAB2         READE     TABEL                                  32
176900020125     C                   ENDDO
177000020125     C*
177100020125     C****  TIPI PRESTAZIONE E DESCRIZIONI: TPS / DSR / CDT  ****
177200020125     C                   Z-ADD     1             X
177300020125     C                   MOVEL     '1Z'          COD
177400020125     C     KTAB2         SETLL     TABEL
177500020125     C     KTAB2         READE     TABEL                                  32
177600020125     C*
177700020125     C     *IN32         DOWEQ     *OFF
177800020125     C     TBLFLG        IFEQ      ' '
177900021209     C                   MOVEL     TBLUNI        DS1Z
178000090109     c                   if        (cn02ana= 'A' and §1zfac ='A') or
178100090109     c                             (cn02ana= 'C' and §1zfac ='C')
178200020125     C                   MOVEL     TBLKEY        TPS(X)
178300020125     C                   MOVEL     §1ZDES        DSR(X)
178400020125     C* CODICE TARIFFA DEL TIPO PRESTAZIONE
178500020125     C                   MOVEL     §1ZCTR        CDT(X)
178600020125     C                   ADD       1             X
178700020125     C                   ENDIF
178800021210     C                   ENDIF
178900020125     C     KTAB2         READE     TABEL                                  32
179000020125     C                   ENDDO
179100020125     C****  TIPI SOMMA PESI E DESCRIZIONI: TSM / DSM  ****
179200020125     C                   Z-ADD     1             X
179300020125     C                   MOVEL     '1N'          COD
179400020125     C     KTAB2         SETLL     TABEL
179500020125     C     KTAB2         READE     TABEL                                  32
179600020125     C*
179700020125     C     *IN32         DOWEQ     *OFF
179800020125     C     TBLFLG        IFEQ      ' '
179900020125     C                   MOVEL     TBLKEY        TSM(X)
180000020125     C                   MOVEL     TBLUNI        DSM(X)
180100020125     C                   ADD       1             X
180200020125     C                   ENDIF
180300020125     C     KTAB2         READE     TABEL                                  32
180400020125     C                   ENDDO
180500020125     C****  TIPI TARIFFA PIKING/ETICHET. E DESCRIZIONI: TPE / DPE  ****
180600020125     C                   Z-ADD     1             X
180700020125     C                   MOVEL     '1K'          COD
180800020125     C     KTAB2         SETLL     TABEL
180900020125     C     KTAB2         READE     TABEL                                  32
181000020125     C*
181100020125     C     *IN32         DOWEQ     *OFF
181200020125     C     TBLFLG        IFEQ      ' '
181300020125     C                   MOVEL     TBLKEY        TPE(X)
181400020125     C                   MOVEL     TBLUNI        DPE(X)
181500020125     C                   ADD       1             X
181600020125     C                   ENDIF
181700020125     C     KTAB2         READE     TABEL                                  32
181800020125     C                   ENDDO
181900020125     C* VEDO SE SONO SIMFEL O REMOTO
182000020701     C                   EVAL      VidFil = SimPOU
182100020701     C                   EVAL      VidPOG = SimPOU
183900020125     C*
184000020125     C                   ENDSR
184100020125     C*
184200020125      *--------------------------------------------------
184300020125     c     *inzsr        begsr
184400020125      *--------------------------------------------------
184500020125
184600020125     C     *ENTRY        PLIST
184700020125     C                   PARM                    KPJBA
184800020125     C                   Z-ADD     1             CODUT
184900020125     C                   CALL      'X§PARUT'
185000020125     C                   PARM                    UT§DSE
185100020125     C                   MOVEL     RAGUT         RSUT             20
185200020125     C                   MOVEL     REC80         CNCR80
185300020125     C*
185400081223     C                   MOVEL     KPJBU         ficn02ds
185500090109     c* per aut: se richiamato da menù forzo l'interrogazione xchè
185600090109     c* bisogna passare dalla supertestata
185700090429     c                   if        kcdaz = 'FN1B' or
185800090108     c                             kcdaz = 'FN11'
185900090108     c                   seton                                        64
186000090108     c                   clear                   CN02PDR
186100090108     c                   eval      CN02PRG = *hival
186200090108     c                   end
186300020215     C                   MOVEL     KPJBU         savkpjbu
186400090109     c*
186500081223     c     cn02ana       comp      'C'                                    63
186600030303     c   63              open      fictt01l
186700090109     c* apro il file in commit solo se manutenzione
186800090108     c                   if        cn02mod <> ' '
186900090108     c                   eval      flg = *off
187000090108     c                   else
187100090108     c                   eval      flg = *on
187200090108     c                   end
187300090108     c                   if        not %open(fifgt01l)
187400090108     c                   open      fifgt01l
187500090108     c                   end
187600090324     c* decodifiche a video
187700090324     c                   if        cn02ana  = 'C'
187800090324     c                   eval      desdts = 'Data Stampa'
187900090324     c                   eval      desdcn = 'Data Ric.Stampa'
188000090324     c                   eval      decpdr = 'Cooperat.'
188100090324     c                   else
188200090324     c                   eval      decpdr = 'Autotrasp.'
188300090324     c                   eval      desdts = 'Conval.fil.'
188400090424     c                   eval      desdcn = 'Ric.Copia Firm.'
188500090324     c                   end
188600020125     C*
188700020125     C*---------------------------------------------------------------*
188800020125     C* ACCESSO TABEL
188900020125     C     KTAB          KLIST
189000020125     C                   KFLD                    CODUT
189100020125     C                   KFLD                    COD
189200020125     C                   KFLD                    KEY
189300020125     C     KTAB2         KLIST
189400020125     C                   KFLD                    CODUT
189500020125     C                   KFLD                    COD
189600020125     C* ACCESSO fifgt
189700081223     C     KTGT          KLIST
189800020125     C                   KFLD                    COMPDR
189900020125     C                   KFLD                    COMSML
190000081223     C                   KFLD                    cn02prg
190100081223     C     KFGT          KLIST
190200081223     C                   KFLD                    COMPDR
190300081223     C                   KFLD                    COMSML
190400020125     C     KFGT1         KLIST
190500020125     C                   KFLD                    COMPDR
190600020125     C                   KFLD                    COMSML
190700020125     C                   KFLD                    VIDTSR
190800020125     C     KFGT2         KLIST
190900020125     C                   KFLD                    COMPDR
191000020125     C                   KFLD                    COMSML
191100020125     C                   KFLD                    VIDTSR
191200020125     C                   KFLD                    COMCTR
191300020125     C     KFGT3         KLIST
191400020125     C                   KFLD                    COMPDR
191500020125     C                   KFLD                    COMSML
191600020125     C                   KFLD                    TSR
191700020125     C                   KFLD                    CTR
191800020125     C                   KFLD                    PRG
191900090511     C     KFGT4         KLIST
192000090511     C                   KFLD                    COMPDR
192100090511     C                   KFLD                    COMSML
192200090511     C                   KFLD                    VIDTSR
192300090511     C                   KFLD                    COMCTR
192400090511     C                   KFLD                    cn02prg
192500020125     C     KFGT5         KLIST
192600020125     C                   KFLD                    COMPDR
192700020125     C                   KFLD                    COMSML
192800020125     C                   KFLD                    TSR
192900020125     C                   KFLD                    CTR
193000020125     C                   KFLD                    COMPRG
193100020125     C     KFpt          KLIST
193200020125     C                   KFLD                    fgtpdr
193300020125     C                   KFLD                    fgtsml
193400020125     C                   KFLD                    fgttsr
193500020125     C                   KFLD                    fgtctr
193600020125     C                   KFLD                    fgtprg
193700020125     C                   KFLD                    wkctd
193800090923     C     KFpt1         KLIST
193900090923     c                   KFLD                    fgtpdr
194000090923     C                   KFLD                    fgtsml
194100090923     c                   KFLD                    vi3tsr
194200090923     c                   KFLD                    vi3ctr
194300090923     c                   KFLD                    vi3prg
194400090923     C                   KFLD                    wkctd
194500100907     C     KFpT1_gio     KLIST
194600100907     C                   KFLD                    COMPDR
194700100907     C                   KFLD                    COMSML
194800100907     C                   KFLD                    tipoGio
194900100907     C                   KFLD                    ctarGio
195000100907     C     KFpT1_det     KLIST
195100100907     C                   KFLD                    COMPDR
195200100907     C                   KFLD                    COMSML
195300100907     C                   KFLD                    tipoDet
195400020423     C     Kftt          KLIST
195500020423     C                   KFLD                    fgtpdr
195600020423     C                   KFLD                    klvl              1
195700020125     C* ACCESSO Tntbe
195800020125     C     Ktbe          KLIST
195900020125     C                   KFLD                    kcod1
196000020125     C                   KFLD                    kke1
196100021203     C* ACCESSO fiapd
196200021203     C     Kapdc         KLIST
196300081223     C                   KFLD                    cn02ana
196400021203     C                   KFLD                    compdr
196500020125     C* DEFINIZIONE CAMPI
196600020125     C     *LIKE         DEFine    TBECOD        KCOD1
196700020125     C     *LIKE         DEFine    TBEKE1        KKE1
196800020125     C     *LIKE         DEFINE    FGTDDT        WUDA                           *AAAA/MM/GG
196900020125     C     *LIKE         DEFINE    FGTDDT        UDATE8                         *GG/MM/AAAA
197000020125     C     *LIKE         DEFINE    FGTPDR        COMPDR
197100020125     C     *LIKE         DEFINE    FGTCTR        COMCTR
197200020125     C     *LIKE         DEFINE    FGTPRG        COMPRG
197300020125     C     *LIKE         DEFINE    FGTSML        COMSML
197400020125     C     *LIKE         DEFINE    FGTDDT        COMDDT
197500020125     C     *LIKE         DEFINE    FGTDST        COMDST
197600020125     C     *LIKE         DEFINE    TBLCOD        COD
197700020125     C     *LIKE         DEFINE    TBLKEY        KEY
197800020125     C     *LIKE         DEFINE    VI4SGL        DUPSGL
197900020125     C     *LIKE         DEFINE    VI4ITR        DUPITR
198000020125     C*
198100020125     c                   endsr
198200020128     C*--- CONDIZIONO EMISSIONE DATI A VIDEO -------------------------*
198300020128     C     CONDIZ        BEGSR
198400020128     C*---------------------------------------------------------------*
198500020128     C                   SETOFF                                       84
198600020128     C****  TIPO PRESTAZIONE  ****
198700020128     C                   MOVEL     TSR           VI3TSR
198800020128     C                   MOVEL     *BLANKS       DE3TSR
198900020128     C                   Z-ADD     1             X
199000020128     C     TSR           LOOKUP    TPS(X)                                 35
199100020128     C   35              MOVEA     DSR(X)        DE3TSR
199200020128     C****  CODICE TARIFFA  ****
199300020128     C                   MOVEL     CTR           VI3CTR
199400020128     C                   MOVEL     *BLANKS       DE3CTR
199500020128     C                   Z-ADD     1             X
199600020128     C                   MOVEL     CTR           UNOCTR
199700020128     C     UNOCTR        LOOKUP    TTR(X)                                 35
199800020128     C   35              MOVEA     DTR(X)        DE3CTR
199900020128     C* SALVO INDICE USATO PER CONTROLLARE L'APPLICAZIONE TARIF. FINITA
200000020128     C   35              Z-ADD     X             Y
200100020128     c                   endsr
200200050411     C*--------------------------------------------------------------------
200300050411      *  Controlla se è una HUB che gestisce DPD
200400050411     C*--------------------------------------------------------------------
200500050411     c     HUB_DPD       begSR
200600050411
200700050411     c                   move      'N'           Ntw_DPD_SN        1
200800050411      * Memorizza FGS a parte x controllo HUB DPD gestita dalla filiale
200900050411     c                   move      vidfil        fgskey            3 0
201000050411      **
201100050411     c     kcae          klist
201200050411     c                   kfld                    tipKey
201300050411     c                   kfld                    kfgs_zero
201400050411     c                   kfld                    fgskey
201500050411      **
201600050411     c                   move      'C'           tipkey            1
201700050411     c                   z-add     0             kfgs_zero         3 0
201800050411      **
201900050411     c                   open      azcae03l
202000050411      **
202100050411      *  Su AZCAE ci sono le linee gestite
202200050411     c     kcae          setll     azcae03l
202300050411     c     kcae          reade     azcae03l
202400050411      **
202500050411     c                   dow       not %eof(azcae03l)
202600050411      **
202700050411      **   se attivo nel range di date:
202800050411     c                   if        WUDA >= caedde and WUDA <= caedsc
202900050411      **
203000050411      **  test su AZORG se network DPD
203100050411     c     CAETFA        chain     azorg01l
203200050411     c                   if        %Found(azorg01l)
203300050411     c                   movel     orgde3        og143
203400130220     c*****    il test di DPD si può fare solo x Network senza coinvolgere l'HUB
203500130220     c************       if        §OGHb1 <> *blank and §OGNTW = 'DPD'
203600130220     c                   if         §OGNTW = 'DPD'
203700050411     c                   move      'S'           Ntw_DPD_SN
203800050411     c                   leave
203900050411     c                   end
204000050411     c                   end
204100050411      **
204200050411     c                   endif
204300050411      **
204400050411     c     kcae          reade     azcae03l
204500050411     c                   enddo
204600050411      *
204700050411     c                   close     azcae03l
204800050411      *
204900050411     c                   endSR
205000050411     C*--------------------------------------------------------------------
205100020130**   XDTF
205200020130312831303130313130313031
