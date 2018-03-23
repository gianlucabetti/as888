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
003200930923     D*
003300081223     D ficn02ds      e DS
003400940920     D* PARAMETRO PASSATO PER ACCEDERE ALLE TARIFFE SIMULAZIONE
003500940920     D*   DIRETTAMENTE DA MENU'
003600940920     D PARAMX          DS
003700931115     D*
003800931115     D* RICHIAMO PGM DI DECODIFICA C.A.P.             - TRUL05R -
003900931115     D PARAM           DS
004000931115     D  PARTLA                 1      1
004100941221     D  PARCAP                 2     10
004200970718     D  PARDRI                11     18  0
004300970718     D  PARNAR                19     21    INZ
004400970718     D  PARNDE                22     23  0
004500970718     D  PARNDF                24     25  0
004600970718     D  PARDEC                26     60
004700970718     D  PARE01                61     95
004800970718     D  PARE02                96    130
004900970718     D  PARE03               131    165
005000970718     D  PARE04               166    200
005100970718     D  PARE05               201    235
005200970718     D  PARE06               236    270
005300970718     D  PARE07               271    305
005400970718     D  PARE08               306    340
005500970718     D  PARE09               341    375
005600970718     D  PARE10               376    410
005700970718     D  PARTCD               411    411    INZ
005800930825     D*
005900011029     D* RICHIAMO PGM DI DUPLICA TARIFFA               - ficn85R -
006000930825     D PARAM3          DS
006100930908     D  PA3PDR                 1      7  0
006200931011     D  PA3SML                 8      8
006300931011     D  PA3TSR                 9      9
006400931011     D  PA3CTR                10     12  0
006500931011     D  PA3PRG                13     15  0
006600931011     D  PA3PDN                16     22  0
006700931011     D  PA3SMN                23     23
006800931011     D  PA3TSN                24     24
006900931011     D  PA3CTN                25     27  0
007000011126     D  PA3div                28     30
007100930825     D*
007200011029     D* RICHIAMO PGM DI GESTIONE TARIFFE PARTICOLARI  - ficn82R -
007300930902     D PARAM4          DS
007400930927     D  PA4TES                 1     37
007500020130     d  modali                37     37
007600930927     D  PA4PDR                38     44  0
007700940922     D  PA4DPD                45     79
007800940922     D  PA4SML                80     80
007900940922     D  PA4TSR                81     81
008000940922     D  PA4DTS                82     96
008100940922     D  PA4CTR                97     99  0
008200021210     D  PA4DCT               100    113
008300021210     d  pa4anac              114    114
008400940922     D  PA4DCV               115    129
008500940922     D  PA4PRG               130    132  0
008600940922     D  PA4FLG               133    133
008700940922     D  PA4DUV               134    134
008800970718     D  PA4DDT               135    142  0
008900011029     D  PA4div               144    146
009000020128     D  PA4ctd               147    149
009100020128     D  PA4dtd               150    174
009200030121     D* ds parametri controllo esistenza altra tariffa valida coop
009300030121     D paramt          DS
009400030121     D  datax                         8s 0
009500030121     D  pogesx                        3s 0
009600030121     D  tipopre                       1
009700030121     d  error                         1
009800970718     D* DS PER TISI95R - CONTROLLO CAP
009900970718     D DSSI95        E DS                  EXTNAME(TISI95DS)
010000940919     D*
010100931115     D TCUDS           DS
010200931115     D  F1                     1      1
010300931115     D  F3                     3      3
010400931115     D  F2                     2      2
010500931115     D  F4                     4      4
010600931115     D  F56                    5      6
010700921102     D WLBDAT          DS
010800940906     D  G02DAT                 1      8  0
010900940906     D  G02INV                 9     16  0
011000940906     D  G02ERR                17     17
011100940906     D  G02TGI                18     22  0
011200940504     D                 DS
011300940906     D  AA                     1      4  0
011400940906     D  MM                     5      6  0
011500940906     D  GG                     7      8  0
011600940906     D  DATA                   1      8  0
011700940916     D                 DS
011800940916     D  VI4SGL                 1     10  0
011900011029     D  VI4ITR                11     19  3
012000011029     D  VI4MIN                20     28  3
012100011029     D  VI4MAX                29     37  3
012200011029     D  COMREC                 1     37
012300011029     D  CO2REC                11     37
012400940916     D                 DS
012500011029     D  SAVITR                 1      9  3
012600011029     D  SAVMIN                10     18  3
012700011029     D  SAVMAX                19     27  3
012800011029     D  SAVREC                 1     27
012900000000     D KPJBA         E DS
013000020701     D* REM                    1      3
013100020701     D* REMFIL                 4      6
013200930914     D*
013300930914     D TAR             DS
013400930914     D  FGDNRR               397    400B 0
013500011029     D FGTDS         E DS                  EXTNAME(fifgt00F)
013600090109     D vidpog          s                   like(vidfil)
013700090109     D comfgt          s                   like(fgtds)
013800020215     D savkpjbu        s                   like(kpjbu)
013900930928     D DSTR          E DS
014000930928     D DS1Z          E DS
014100900320     D CNCR80        E DS
014200131008     D tibs42ds      E DS
014300131008     D uteautds      E DS
014400021203     D fnlv24ds      E DS
014500021218     D fnlv55ds      E DS
014600030122     D og143         E DS
014700900320     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
014800931115     D  TCU                  398    697
014900931115     D                                     DIM(50)                              TAB FLAG CNT
015000931115     D  KCU                  698    847P 0
015100931115     D                                     DIM(50)                              TAB CAPO CONTO
015200931115     D                                     PACKEVEN
015300931115     D  DTU                  848    895P 0
015400931115     D                                     DIM(12)                              DATE PARAM
015500931115     D                                     PACKEVEN
015600020124     D ds8a          E DS
015700020124     d xx              s              3  0
015800020204     d carok           s              1
015900021210     d almeno_uno      s              1
016000020206     d ricar           s              1
016100020125     d wkctd           s              3
016200020129     d dataiso         s               d   datfmt(*iso)
016300020129     d dataeur         s               d   datfmt(*eur)
016400130614     d cdpar           s             30    dim(50)
016500130614     d cdpards         ds            30
016600020124     d  fba                           1    overlay(cdpards:1)
016700020124     d  ctd                           3    overlay(cdpards:2)
016800020124     d  dtd                          25    overlay(cdpards:5)
016900130614     d  esc                           1    overlay(cdpards:30)
017000941005     D*
017100020130     D XDTF            S              2  0 DIM(12) CTDATA PERRCD(12)            GG DEI MESI
017200930811     C*****************************************************************
017300930811     C*---------------------------------------------------------------*
017400930928     C* ESEGUO OPERAZIONI INIZIALI
017500930928     C                   EXSR      OPERAZ
017600930903     C*
017700930914     C* ALF1 = 'F' ===> VADO A FINE PGM
017800930811     C     ALF1          DOWNE     'F'
017900930824     C*
018000930914     C* ALF1 = '1' ===> GESTIONE VIDEATA DELLE SELEZIONI
018100930811     C     ALF1          IFEQ      '1'
018200930811     C                   EXSR      VIDEO1
018300930811     C                   ENDIF
018400930811     C*
018500930928     C* ALF1 = '2' ===> GESTIONE VIDEATA SUBFILE CODICI TARIFFA
018600930811     C     ALF1          IFEQ      '2'
018700930811     C                   EXSR      VIDEO2
018800930811     C                   ENDIF
018900930824     C*
019000020125     C     ALF1          IFEQ      '3'
019100020125     C                   EXSR      VIDEO3
019200020125     C                   ENDIF
019300020125     C*
019400930811     C                   ENDDO
019500940930     C*
019600930820     C                   SETON                                        LR
019700930928     C*
019800930811     C*--- GESTIONE DELLA PRIMA VIDEATA ------------------------------*
019900930811     C     VIDEO1        BEGSR
020000011109     C*---------------------------------------------------------------*
020100020206     C                   SETOFF                                         2910
020200930811     C*
020300081229     c     cn02mod       comp      ' '                                1010
020400081223     c                   if        cn02ana= 'A'
020500021210     c   10              eval      vidtes = 'Interrogazione tariffe Autotrasp. '
020600090424     c  n10              eval      vidtes = '  ** Gestione tariffe AUT città **'
020700021210     c                   else
020800021210     c   10              eval      vidtes = 'Interrogazione tariffe Cooperative'
020900021210     c  n10              eval      vidtes = '** Gestione tariffe Cooperative **'
021000021210     c                   endif
021100081229     C                   MOVEL     'EUR'         VIDdiv
021200020130     c
021300930910     C     *IN11         IFEQ      *OFF
021400930927     C                   MOVEL     *BLANKS       VIDTSR
021500930927     C                   MOVEL     *BLANKS       DESTSR
021600930907     C                   MOVEL     *BLANKS       VIDCTR
021700930910     C                   SETON                                        11
021800930909     C                   ENDIF
021900930930     C*
022000930930     C* P U L I Z I A   S U B F I L E
022100930930     C* LA FACCIO IN QUESTO PUNTO PERCHE' SE LA LASCIAVO PRIMA DEL
022200931012     C*   CARICAMENTO DEL SFL, NON VENIVANO EMESSI I MESSAGGI DI
022300931013     C*   ERRORE ASSOCIATI AGLI INDICATORI 51-72
022400020124     C                   EXSR      PULSF
022500930907     C*
022600930907     C     FOR01         TAG
022700090107     c                   if        *in90
022800090107     c                   setoff                                       90
022900090107     C                   write     CN10D01
023000090107     c                   seton                                        90
023100090107     c                   end
023200090107     C                   EXFMT     CN10D01
023300081223     C                   SETOFF                                       0486
023400930929     C*
023500930811     C** CMD3  - FINE LAVORO
023600931013     C   KC              MOVEL     'F'           ALF1
023700931013     C   KC              GOTO      ENDVD1
023800931013     C*
023900940928     C** CMD7  - GESTIONE ANAGRAFICA PADRONCINI
024000930907     C     *INKG         IFEQ      *ON
024100081223     c     cn02ana       andne     'A'
024200131022     C                   Z-ADD     VIDFIL        d24FIL
024300021203     C                   Z-ADD     0             d24pdr
024400030204     C                   MOVEL     'R'           d24FLG
024500081223     C                   MOVEL     cn02ana       d24tip
024600021203     C                   MOVEL     *BLANKS       d24rsc
024700021203     C                   MOVEL(p)  fnlv24ds      KPJBU
024800940926     C                   CALL      'FNLV24R'
024900930811     C                   PARM                    KPJBA
025000030204     C                   MOVEL     KPJBU         fnlv24ds
025100020228     C                   MOVEL     SAVKPJBU      KPJBU
025200030204     C* CONTROLLO SE E' STATO SELEZIONATO UN CODICE PADRONCINO
025300030204     C     d24pdr        IFNE      0
025400030204     C                   MOVEL     d24pdr        VIDFIL
025500030204     C                   MOVEL     *ZEROS        VIDPDR
025600030204     C                   MOVE      d24pdr        VIDPDR
025700030204     C                   MOVEL     d24rsc        DESPDR
025800030204     C                   ENDIF
025900930907     C*
026000930907     C                   GOTO      FOR01
026100930811     C                   ENDIF
026200930811     C*
026300930907     C** CMD14 - GESTIONE ANAGRAFICA FORNITORI
026400011126     c                   if        *inkn
026500011126     c                   movel(p)  'P'           kpjbu
026600011126     C                   CALL      'TRUL16R'
026700930907     C                   PARM                    KPJBA
026800020228     C                   MOVEL     savKPJBU      KPJBU
026900011126     C                   GOTO      FOR01
027000011126     c                   end
027100940919     C*
027200930811     C** CONTROLLI DELLA PRIMA VIDEATA
027300930907     C                   EXSR      CONTR1
027400930907     C   90              GOTO      FOR01
027500930811     C*
027600930907     C                   MOVEL     '2'           ALF1
027700930907     C     ENDVD1        ENDSR
027800930811     C*
027900930811     C*--- CONTROLLI DELLA PRIMA VIDEATA -----------------------------*
028000930811     C     CONTR1        BEGSR
028100011109     C*---------------------------------------------------------------*
028200930830     C                   SETOFF                                       030590
028300081229     C                   SETOFF                                       8562
028400090109      * controllo autorizzazione del profilo ad accedere alle tariffe
028500131008     c                   clear                   tibs42ds
028600131008     c                   movel     vidfil        i42pge
028700131008     c                   call      'TIBS42R'
028800131008     c                   parm                    tibs42ds
028900131008     c                   movel     o42uni        uteautds
029000090109     C* COOP
029100090109     c                   if        cn02ana= 'C'
029200100420     c                   if        §autvtcc <> 'S'
029300021211     c                   seton                                        6290
029400021211     c                   goto      endct1
029500021211     c                   endif
029600090109     C                   ELSE
029700090109     C* AUT
029800131022     c                   if        o42err <>' ' and §AUTMTC <> 'S'
029900090109     c                   seton                                        8390
030000090109     c                   goto      endct1
030100090109     c                   endif
030200021211     c                   endif
030300930811     C*
030400940920     C****  CODICE FILIALE  ****
030500950113     C* FILIALE OBBLIGATORIA
030600030529     C     VIDFIL        IFEQ      0
030700950113     C                   SETON                                        8390
030800950113     C                   GOTO      ENDCT1
030900950113     C                   ENDIF
031000950113     C*
031100020701     C                   MOVEL     VIDFIL        WFIL              3 0
031200940920     C*
031300930811     C****  CODICE PADRONCINO  ****
031400930908     C     '?'           SCAN      VIDPDR                                 90
031500930811     C*
031600930811     C     *IN90         IFEQ      *ON
031700021203     C                   Z-ADD     VIDFIL        d24FIL
031800021203     C                   Z-ADD     0             d24pdr
031900021203     C                   MOVEL     'R'           d24FLG
032000081223     C                   MOVEL     cn02ana       d24tip
032100021203     C                   MOVEL     *BLANKS       d24rsc
032200021203     C                   MOVEL(p)  fnlv24ds      KPJBU
032300940926     C                   CALL      'FNLV24R'
032400930811     C                   PARM                    KPJBA
032500021203     C                   MOVEL     KPJBU         fnlv24ds
032600020228     C                   MOVEL     savKPJBU      KPJBU
032700941006     C*
032800941006     C* CONTROLLO SE E' STATO SELEZIONATO UN CODICE PADRONCINO
032900021203     C     d24pdr        IFNE      0
033000021203     C                   MOVEL     d24pdr        VIDFIL
033100930921     C                   MOVEL     *ZEROS        VIDPDR
033200021203     C                   MOVE      d24pdr        VIDPDR
033300021203     C                   MOVEL     d24rsc        DESPDR
033400941006     C                   ENDIF
033500941006     C*
033600930811     C                   GOTO      ENDCT1
033700930811     C                   ENDIF
033800930811     C*
033900930921     C* CONTROLLO VALIDITA' DEL CODICE PADRONCINO
034000930921     C                   TESTN                   VIDPDR               35
034100930921     C  N35              SETON                                        4090
034200930921     C  N35              GOTO      ENDCT1
034300931111     C*
034400940915     C* CONTROLLO SE ESISTE NELL'ANAGRAFICA PADRONCINI
034500940920     C                   MOVEL     VIDFIL        COMPDR
034600940920     C                   MOVE      VIDPDR        COMPDR
034700021203     C     kapdc         CHAIN     fiapd000                           32
034800090610     c                   if        not *in32 and not *in10
034900090610     C     APDATB        IFNE      ' '
035000940914     C                   SETON                                        32
035100940914     C                   ENDIF
035200090610     C                   ENDIF
035300940914     C* CODICE PADRONCINO ERRATO
035400940914     C   32              SETON                                        4090
035500940914     C   32              GOTO      ENDCT1
035600100204      *
035700100204     c*  se escluso da autofatturazione dare msg di errore
035800100204     c                   if        apdpdd = 'S'  and cn02ana<>'C'
035900100204     C                   SETON                                        4090
036000100204     C                   GOTO      ENDCT1
036100100204     c                   end
036200100204      *
036300100224     C********           MOVEL     APDRSC        DESPDR
036400100224     C                   MOVEL     APDRSF        DESPDR
036500930811     C*
036600931104     C****  TIPO PRESTAZIONE  ****
036700930928     C                   MOVEL     *BLANKS       DESTSR
036800930927     C     VIDTSR        IFNE      ' '
036900930830     C*
037000930927     C     VIDTSR        IFEQ      '?'
037100930830     C                   MOVEL     '1Z'          COD
037200930927     C                   MOVEL     ' '           VIDTSR
037300930830     C                   MOVEL     *BLANKS       DES              25
037400930830     C                   CALL      'X§TABER'
037500930830     C                   PARM                    CODUT
037600930830     C                   PARM                    COD
037700930830     C                   PARM                    KEY
037800930830     C                   PARM                    DES
037900930927     C                   MOVEL     KEY           VIDTSR
038000930927     C                   MOVEL     DES           DESTSR
038100930830     C                   SETON                                        90
038200930830     C                   GOTO      ENDCT1
038300930903     C                   ENDIF
038400930830     C*
038500931104     C* CONTROLLO VALIDITA' TIPO PRESTAZIONE
038600930903     C                   Z-ADD     1             X
038700930927     C     VIDTSR        LOOKUP    TPS(X)                                 35
038800931104     C* TIPO PRESTAZIONE ERRATO
038900930903     C  N35              SETON                                        4190
039000930903     C  N35              GOTO      ENDCT1
039100930927     C                   MOVEL     DSR(X)        DESTSR
039200930928     C                   MOVEL     CDT(X)        COMCDT            3
039300930830     C*
039400931104     C* 03 ON  - E' STATO DIGITATO ANCHE IL TIPO PRESTAZIONE
039500930830     C                   SETON                                        03
039600930830     C                   ENDIF
039700930830     C*
039800930811     C****  CODICE TARIFFA  ****
039900930811     C     VIDCTR        IFNE      *BLANKS
040000931008     C*
040100931013     C  N03VIDCTR        IFNE      '000'
040200931104     C* SE DIGITATO IL CODICE TARIFFA OCCORRE INSERIRE IL TIPO PRESTAZ.
040300931013     C                   SETON                                        4290
040400931013     C                   GOTO      ENDCT1
040500931013     C                   ELSE
040600931013     C                   MOVEL     *BLANKS       VIDCTR
040700931013     C                   SETON                                        85
040800931013     C                   ENDIF
040900930928     C*
041000931013     C* 85 ON  - E' COME SE IL CODICE TARIFFA NON FOSSE STATO INSERITO
041100931013     C     *IN85         IFEQ      *OFF
041200930928     C*
041300931103     C     VIDTSR        IFEQ      'G'
041400931103     C*
041500931103     C* CONTROLLO SE IL CODICE TARIFFA DIGITATO E' UGUALE AL CODICE
041600931103     C*   TARIFFA PRESENTE IN TABELLA
041700930928     C     VIDCTR        IFNE      COMCDT
041800930928     C                   SETON                                        6190
041900930928     C                   GOTO      ENDCT1
042000930928     C                   ENDIF
042100930928     C*
042200011107     C*
042300931104     C                   Z-ADD     1             X
042400931104     C                   MOVEL     COMCDT        UNOCTR            1
042500931104     C     UNOCTR        LOOKUP    TTR(X)                                 35
042600931104     C* SALVO INDICE USATO PER CONTROLLARE L'APPLICAZIONE TARIF. FINITA
042700931104     C   35              Z-ADD     X             Y                 3 0
042800931104     C*
042900931103     C                   ELSE
043000931104     C* TIPO PRESTAZIONE >< "G"
043100931103     C*
043200931104     C                   Z-ADD     1             X
043300931104     C                   MOVEL     VIDCTR        UNOCTR
043400931104     C     UNOCTR        LOOKUP    TTR(X)                                 35
043500931104     C*
043600931103     C     VIDCTR        IFNE      COMCDT
043700931103     C* CODICE TARIFFA ERRATO
043800931103     C  N35              SETON                                        4390
043900931103     C  N35              GOTO      ENDCT1
044000931103     C* SALVO INDICE USATO PER CONTROLLARE L'APPLICAZIONE TARIF. FINITA
044100931104     C                   Z-ADD     X             Y
044200030304     c*
044300931104     C                   ELSE
044400931104     C   35              Z-ADD     X             Y
044500931103     C                   ENDIF
044600931103     C*
044700931103     C                   ENDIF
044800931103     C*
044900930928     C* 05 ON  - E' STATO DIGITATO ANCHE IL CODICE TARIFFA
045000930811     C                   SETON                                        05
045100931013     C                   ENDIF
045200931013     C*
045300931013     C                   MOVEL     VIDCTR        COMCTR
045400930811     C                   ENDIF
045500000620     C*
045600100201     c                   if        cn02ana  <>'C'
045700100127     C* Controlla che non vi sia presente già un'altra tariffa Consegna/Ritiro
045800100127     C*  di dettaglio sul progressivo
045900100127     C*
046000100127     c                   clear                   gia_dettaglio     1
046100100127     c                   if        vidctr <> '999'
046200100127     c     kfgt1         setll     fifgt01l
046300100127     c     kfgt1         reade     fifgt01l
046400100127     c                   dow       not %Eof(fifgt01l)
046500100127     C*
046600100127     c                   if        fgtprg = cn02prg and fgtctr <> 999
046700100127     c                   move      fgtctr        alfctr            3
046800100127     c                   if        alfctr <> vidctr
046900100127     c                   move      'S'           gia_dettaglio
047000100127     c                   leave
047100100127     c                   end
047200100127     c                   end
047300100127     C*
047400100127     c     kfgt1         reade     fifgt01l
047500100127     c                   enddo
047600100127     c                   end
047700100127     C*  se presente già un'altra tariffa di dettaglio
047800100127     C*     sullo stesso tipo servizio/progressivo
047900100127     c                   if        gia_dettaglio = 'S'
048000100127     C                   SETON                                        6590
048100100127     C                   GOTO      ENDCT1
048200100201     c                   end
048300100201     C*
048400100907     C* Controlla che non vi sia presente una tariffa a Giornata e si sta immettendo
048500100907     C*  una tariffa  di dettaglio sul progressivo
048600100907     C*
048700100907     c                   clear                   gia_giornata      1
048800100907     C*    quindi un dettaglio
048900100907     c                   if        vidctr <> '999' and vidctr <> *blank
049000100907     C*
049100100907     C                   movel     'G'           tipoGio           1
049200100907     C                   z-add     999           ctarGio           3 0
049300100907     c     kfpt1_gio     setll     fifpt01l
049400100907     c     kfpt1_gio     reade     fifpt01l
049500100907     c                   dow       not %Eof(fifpt01l)
049600100907     C*
049700100907     c                   if        fptprg = cn02prg
049800100907     c                   if        fptctd = 'TIG' or fptctd = 'TMG'
049900100907     c                   move      'S'           gia_giornata
050000100907     c                   leave
050100100907     c                   end
050200100907     c                   end
050300100907     C*
050400100907     c     kfpt1_gio     reade     fifpt01l
050500100907     c                   enddo
050600100907     c                   end
050700100907      *
050800100907     C*  se presente già un'altra tariffa di dettaglio
050900100907     C*     sullo stesso tipo servizio/progressivo
051000100907     c                   if        gia_giornata  = 'S'
051100100907     C                   SETON                                        6690
051200100907     C                   GOTO      ENDCT1
051300100907     c                   end
051400100907     C*
051500100127     c                   end
051600100127     C*
051700030206      *controllo immissione tariffa trattoristi e pulizie tariffe fisse
051800030206      * con codici 610 e 650
051900030529      * inserito controllo anche per le coop tariffa solo 600
052000030529     C     *IN05         IFEQ      *ON
052100030529     C     VIDtsr        ANDEQ     'X'
052200030529     C     vidctr        ANDNE     '600'
052300030529     C* CODICE TARIFFA ERRATO
052400030529     C                   SETON                                        6190
052500030529     C                   GOTO      ENDCT1
052600030529     C                   ENDIF
052700030206     C     *IN05         IFEQ      *ON
052800030206     C     VIDtsr        ANDEQ     'P'
052900030206     C     vidctr        ANDNE     '650'
053000030206     C* CODICE TARIFFA ERRATO
053100030206     C                   SETON                                        6190
053200030206     C                   GOTO      ENDCT1
053300030206     C                   ENDIF
053400030206     C     *IN05         IFEQ      *ON
053500030206     C     VIDtsr        ANDEQ     'T'
053600030206     C     vidctr        ANDNE     '610'
053700030206     C* CODICE TARIFFA ERRATO
053800030206     C                   SETON                                        6190
053900030206     C                   GOTO      ENDCT1
054000030206     C                   ENDIF
054100000620     C*
054200930928     C****  CONTROLLO SE ESISTONO TARIFFE PER QUEL PADRONCINO  ****
054300931012     C* IMPOSTO CAMPI DELLA CHIAVE
054400930928     C                   MOVEL     VIDFIL        COMPDR
054500930915     C                   MOVE      VIDPDR        COMPDR
054600931011     C  N19              MOVEL     ' '           COMSML
054700931011     C   19              MOVEL     'S'           COMSML
054800011107     C*
054900931006     C  N03
055000931006     CANN05              DO
055100011029     C     KFGT          SETLL     fifgt000                               30
055200931006     C  N30              SETON                                        5190
055300931006     C  N30              GOTO      ENDCT1
055400931006     C                   ENDDO
055500931005     C*
055600931006     C   03
055700931006     CANN05              DO
055800011029     C     KFGT1         SETLL     fifgt000                               30
055900931006     C  N30              SETON                                        7290
056000931006     C  N30              GOTO      ENDCT1
056100931006     C                   ENDDO
056200930909     C*
056300940905     C   03
056400011029     CAN 05KFGT2         SETLL     fifgt000                               30
056500090429     c* controllo che non esista il tgt convalidato
056600090429     c                   if        cn02ana = 'A' and cn02prg <> *hival
056700090511     c                             and *in03 and *in05
056800090511     C     KFGT4         SETLL     fifgt000                               30
056900090511     c                   if        not *in30
057000090429     C     Ktgt          CHAIN     fitgt000
057100090429     c                   if        %found(fitgt01l) and tgtatb = ' '
057200090429     c                             and tgtdts <> 0
057300090429     C                   SETON                                        7390
057400090429     C                   GOTO      ENDCT1
057500090429     c                   end
057600090429     c                   end
057700090511     c                   end
057800930908     C*
057900930811     C     ENDCT1        ENDSR
058000930811     C*
058100930811     C*--- GESTIONE DELLA SECONDA VIDEATA ----------------------------*
058200930811     C     VIDEO2        BEGSR
058300011109     C*---------------------------------------------------------------*
058400020206     C                   SETOFF                                       0102
058500930811     C*
058600930927     C                   MOVEL     ' '           TSR               1
058700930830     C                   Z-ADD     0             CTR               3 0
058800930830     C                   Z-ADD     0             PRG               3 0
058900930825     C*
059000930909     C* 30 OFF - IMMISSIONE
059100930909     C* 30 ON  - AGGIORNAMENTO
059200930909     C*
059300930825     C     *IN30         IFEQ      *OFF
059400931013     C     *IN29         ANDEQ     *OFF
059500930825     C* 01 ON  - IMMISSIONE
059600930825     C                   SETON                                        01
059700930927     C                   MOVEL     VIDTSR        TSR
059800930909     C                   MOVEL     VIDCTR        CTR
059900081223     c                   if        cn02ana  = 'A'
060000081223     c                   z-add     cn02prg       prg
060100081223     c                   else
060200081223     C                   Z-ADD     0             PRG
060300081223     c                   end
060400930812     C                   ELSE
060500020410     C* CARICAMENTO SUBFILE
060600931013     C  N29              EXSR      CARSF
060700090108     c* se sono in interrogazione e non ci sono record torno al 1° video
060800090108     c                   setoff                                       52
060900090108     C                   IF        nrr = 0 and *in10
061000090108     C                   SETON                                        5290
061100090108     C                   MOVEL     '1'           ALF1
061200090108     C                   GOTO      ENDVD2
061300090108     c                   end
061400930820     C*
061500020205     c                   do        *hival
061600090109     C* NESSUN PROGRESSIVO VALIDO CARICATO A VIDEO: VADO IN IMMISSIONE
061700930820     C     NRR           IFEQ      0
061800090109     C* 01 ON  - IMMISSIONE
061900930928     C                   SETON                                        01
062000090109     c*
062100011213     C     KFGT2         setgt     fifgt000
062200011213     c     kfgt2         readpe    fifgt000                               30
062300011213     c                   if        not *in30
062400020410     c     kfgt          chain     fifgt000
062500011213     c                   end
062600090109     c* se non inseriti prestazione o tariffa errore
062700930921     C     *IN05         IFEQ      *OFF
062800090107     C  N03              SETON                                        5190
062900090107     C   03              SETON                                        7290
063000930909     C                   MOVEL     '1'           ALF1
063100930909     C                   GOTO      ENDVD2
063200930909     C                   ENDIF
063300090109     C* per coop: ULTIMO LETTO + 1, per aut: invece mi viene già
063400090109     c* passato dalla supertestata
063500930930     C                   MOVEL     VIDTSR        TSR
063600930930     C                   MOVEL     VIDCTR        CTR
063700090108     c                   if        cn02ana = 'A' and cn02prg <> *hival
063800081223     c                   eval      prg = cn02prg
063900081223     c                   else
064000930830     C     LETPRG        ADD       1             PRG
064100081223     c                   end
064200020219     c                   leave
064300930820     C*
064400930820     C                   ELSE
064500930909     C* NRR <> 0
064600930928     C*
064700930928     C* 02 ON  - AGGIORNAMENTO
064800930928     C                   SETON                                        02
064900930928     C*
065000930910     C                   Z-ADD     1             REC
065100020125     C                   WRITE     CN10Z02
065200020124     C                   EXFMT     CN10C02
065300930820     C*
065400930820     C** CMD12 - RITORNO
065500930914     C     *INKL         IFEQ      *ON
065600930914     C                   SETOFF                                       11
065700930914     C                   MOVEL     '1'           ALF1
065800930914     C                   GOTO      ENDVD2
065900930914     C                   ENDIF
066000930820     C*
066100930820     C** CMD9  - AGGIUNTA TARIFFA
066200930914     C     *INKI         IFEQ      *ON
066300081229     C* SE NON IMMESSI IL TIPO PRESTAZIONE E IL CODICE TARIFFA ERRORE       A
066400930830     C     *IN03         IFEQ      *OFF
066500930830     C     *IN05         OREQ      *OFF
066600930909     C                   SETON                                        04
066700930820     C                   MOVEL     '1'           ALF1
066800930907     C                   GOTO      ENDVD2
066900081229     C                   End
067000081229
067100020205     C                   clear                   PARAM3
067200020206     c                   if        vi2prg > 0
067300020206     c                   z-add     vi2prg        pa3prg
067400020206     c                   end
067500930908     C                   Z-ADD     COMPDR        PA3PDR                         *PDR VECCHIO
067600931012     C                   MOVEL     COMSML        PA3SML                         *SML VECCHIO
067700930927     C                   MOVEL     VIDTSR        PA3TSR                         *TSR VECCHIO
067800930825     C                   MOVEL     VIDCTR        PA3CTR                         *CTR VECCHIO
067900931012     C                   Z-ADD     COMPDR        PA3PDN                         *PDR NUOVO
068000931012     C                   MOVEL     COMSML        PA3SMN                         *SML NUOVO
068100930927     C                   MOVEL     VIDTSR        PA3TSN                         *TSR NUOVO
068200930825     C                   MOVEL     VIDCTR        PA3CTN                         *CTR NUOVO
068300011126     C                   MOVEL     VIDdiv        PA3div                         *CTR NUOVO
068400930825     C                   MOVEL     PARAM3        KPJBU
068500020218     C                   CALL      'FICN13R'
068600930825     C                   PARM                    KPJBA
068700930825     C                   MOVEL     KPJBU         PARAM3
068800020215     C                   MOVEL     savKPJBU      kpjbu
068900930927     C                   MOVEL     PA3TSR        TSR
069000930921     C                   Z-ADD     PA3CTR        CTR
069100930830     C     PA3PRG        ADD       1             PRG
069200090107     c                   commit
069300020206     c                   leave
069400930823     C                   ELSE
069500930825     C* *INKI OFF - CONTROLLO SE E' STATA SCELTA UNA TARIFFA
069600020124     C                   READC     CN10S02                                32
069700930825     C*
069800020205     c                   if        vi2sce <> *blank
069900930927     C                   MOVEL     VI2TSR        TSR
070000930920     C                   MOVEL     VI2CTR        CTR
070100930920     C                   MOVEL     VI2PRG        PRG
070200020205     c                   leave
070300020205     c                   end
070400930820     C                   ENDIF
070500930812     C                   ENDIF
070600020205     c                   enddo
070700930825     C                   ENDIF
070800930825     C*
070900930907     C                   MOVEL     '3'           ALF1
071000930907     C     ENDVD2        ENDSR
071100930811     C*
071200930812     C*--- PULIZIA SUBFILE -------------------------------------------*
071300930812     C     PULSF         BEGSR
071400011109     C*---------------------------------------------------------------*
071500930915     C                   SETON                                        87
071600020124     C                   WRITE     CN10C02
071700930915     C                   SETOFF                                       87
071800930812     C                   ENDSR
071900930812     C*
072000930812     C*--- CARICAMENTO SUBFILE ---------------------------------------*
072100930812     C     CARSF         BEGSR
072200011109     C*---------------------------------------------------------------*
072300930812     C                   Z-ADD     0             NRR               4 0
072400930812     C*
072500940905     C  N03
072600011029     CANN05KFGT          READE(N)  fifgt000                               31
072700940905     C   03
072800011029     CANN05KFGT1         READE(N)  fifgt000                               31
072900940905     C   03
073000011029     CAN 05KFGT2         READE(N)  fifgt000                               31
073100930812     C*
073200930825     C     *IN31         DOWEQ     *OFF
073300081223     C* per aut il progressivo viene passato dalla supertestata
073400090109     c* quindi carico solo quello
073500081223     c                   if        cn02ana = 'A' and fgtprg <> cn02prg
073600090108     c                             and cn02prg <> *hival
073700081223     c                   goto      nocar
073800081223     c                   end
073900090428     C* per aut se interrogazione fino al 30/4/2009 ometto le tariffe
074000090428     c* con data >
074100090428     c                   if        cn02ana = 'A' and fgtddt > 20090430
074200090429     c                             and (kcdaz = 'FN11' or
074300090429     c                             kcdaz = 'FN1B') and
074400090429     c                             %subst(knmus: 1: 3) <> 'EDP'
074500090428     c                   goto      nocar
074600090428     c                   end
074700930819     C* ULTIMO PROGRESSIVO LETTO (COMPRESI QUELLI ANNULLATI)
074800930812     C                   Z-ADD     FGTPRG        LETPRG            3 0
074900930812     C     FGTATB        IFEQ      ' '
075000930820     C* CAMPO SELEZIONE
075100930819     C                   MOVEL     ' '           VI2SCE
075200931104     C* TIPO PRESTAZIONE
075300930830     C                   CLEAR                   SER
075400930927     C                   MOVEA     FGTTSR        SER(1)
075500930830     C                   MOVEA     '-'           SER(2)
075600930903     C                   Z-ADD     1             X
075700930927     C     FGTTSR        LOOKUP    TPS(X)                                 35
075800021210     c  n35              goto      nocar
075900930903     C   35              MOVEA     DSR(X)        SER(3)
076000930927     C   35              MOVEA     SER           VI2TSR
076100930819     C                   MOVEL     FGTCTR        VI2CTR
076200930819     C                   MOVEL     FGTPRG        VI2PRG
076300081223     C                   MOVEL     FGTdiv        VI2div
076400940906     C                   Z-ADD     FGTDDT        DATA
076500940906     C                   Z-ADD     MM            GGMM              4 0
076600940906     C                   MOVEL     GG            GGMM
076700940906     C                   Z-ADD     AA            VI2DDT
076800940906     C                   MOVEL     GGMM          VI2DDT
076900940906     C                   Z-ADD     FGTDST        DATA
077000940906     C                   Z-ADD     MM            GGMM
077100940906     C                   MOVEL     GG            GGMM
077200940906     C                   Z-ADD     AA            VI2DST
077300940906     C                   MOVEL     GGMM          VI2DST
077400940906     C                   Z-ADD     FGTDUV        DATA
077500940906     C                   Z-ADD     MM            GGMM
077600940906     C                   MOVEL     GG            GGMM
077700940906     C                   Z-ADD     AA            VI2DUV
077800940906     C                   MOVEL     GGMM          VI2DUV
077900081229     c* se confermato disabilito il cmd 9 per inserire nuove tariffe
078000081229     c                   if        fgtdts > 0 and cn02ana = 'A'
078100081229     c                   seton                                        17
078200081229     c                   end
078300930819     C                   MOVEL     FGTDCV        VI2DCV
078400930812     C                   ADD       1             NRR
078500020124     C                   WRITE     CN10S02
078600930812     C                   ENDIF
078700011126     c     nocar         tag
078800930812     C*
078900940905     C  N03
079000011029     CANN05KFGT          READE(N)  fifgt000                               31
079100940905     C   03
079200011029     CANN05KFGT1         READE(N)  fifgt000                               31
079300940905     C   03
079400011029     CAN 05KFGT2         READE(N)  fifgt000                               31
079500930812     C                   ENDDO
079600930820     C*
079700930812     C                   ENDSR
079800930826     C*
079900020125     C*--- GESTIONE DELLA TERZA VIDEATA ------------------------------*
080000020125     C     VIDEO3        BEGSR
080100020125     C*---------------------------------------------------------------*
080200020125     C*
080300020125     C* PULIZIA SFL DETTAGLIO
080400020125     C                   EXSR      PULVI3
080500090114     c                   setoff                                       131715
080600090114     c                   setoff                                       18
080700020125     C* CARICO DATI A VIDEO
080800081223     c                   clear                   v5km
080900090401     c                   clear                   v5pkl
081000090109     c* se aut carico i dati dalla supertestata
081100090108     c                   if        cn02ana = 'A' and cn02prg <> *hival
081200081223     C     Ktgt          CHAIN     fitgt000
081300081223     c                   if        %found(fitgt01l)
081400081223     c                   move      tgtprg        vi3prg
081500081223     c                   if        Tgtddt > 0
081600081223     c                   move      tgtddt        dataiso
081700081223     c                   move      dataiso       dataeur
081800081223     c                   move      dataeur       v5ddt
081900081223     c                   else
082000081223     c                   z-add     0             v5ddt
082100081223     c                   end
082200081223     c                   if        Tgtdst > 0
082300081223     c                   move      Tgtdst        dataiso
082400081223     c                   move      dataiso       dataeur
082500081223     c                   move      dataeur       v5dst
082600081223     c                   else
082700081223     c                   z-add     31122039      v5dst
082800081223     c                   end
082900081223     c                   if        Tgtduv > 0
083000081223     c                   move      Tgtduv        dataiso
083100081223     c                   move      dataiso       dataeur
083200081223     c                   move      dataeur       v5duv
083300081223     c                   else
083400081223     c                   z-add     0             v5duv
083500081223     c                   end
083600090325     c                   setoff                                       17
083700081223     c                   if        Tgtdts > 0
083800090324     c                   seton                                        17
083900081223     c                   move      tgtdts        dataiso
084000081223     c                   move      dataiso       dataeur
084100081223     c                   move      dataeur       v5dts
084200081223     c                   else
084300081223     c                   z-add     0             v5dts
084400081223     c                   end
084500090424     c                   if        Tgtdrc > 0
084600090424     c                   move      Tgtdrc        dataiso
084700081223     c                   move      dataiso       dataeur
084800081223     c                   move      dataeur       v5dcn
084900081223     c                   else
085000081223     c                   z-add     0             v5dcn
085100081223     c                   end
085200081223     c                   movel     tgtdcv        v5dcv
085300081223     c                   z-add     tgtkm         v5km
085400090401     c                   z-add     tgtpkl        v5pkl
085500081229     c                   seton                                        18
085600081223     c                   end
085700081223     c                   end
085800081223     c*
085900081223     C     KFGT3         CHAIN     fifgt000                           3236
086000020125     C*
086100020125     C* 36 ON  - RECORD ALLOCATO
086200020125     C     *IN36         IFEQ      *ON
086300020125     C                   SETON                                        5029
086400020125     C                   MOVEL     '2'           ALF1
086500020125     C                   GOTO      ENDVD3
086600020125     C                   ENDIF
086700020125     C*
086800020219     c                   if        *in32
086900020219     c                   clear                   fgtds
087000020219     c                   move      tsr           vi3tsr
087100020219     c                   move      ctr           vi3ctr
087200020219     c                   move      prg           vi3prg
087300081223     c                   move      viddiv        vi3div
087400020219     c                   else
087500090109     c* Controllo se abilitato alla manutenzione
087600020329     c* solo se è impostata la data di stampa
087700020509     c                   if        fgtdts > 0 and fgtsml = *blank
087800131008     c                   clear                   tibs42ds
087900131008     c                   movel     vidfil        i42pge
088000131008     c                   call      'TIBS42R'
088100131008     c                   parm                    tibs42ds
088200131008     c                   movel     o42uni        uteautds
088300080929      *
088400020329      * se abilitato con *in13 abilito la manutenzione della data scadenza
088500080926      * con *in15 proteggo gli altri campi
088600021211      * differisco i controlli autorizzazioni dal Flag dell'anagrafica
088700021211      * A / C
088800080929     c                   seton                                        15
088900090108     c                   if        cn02ana = 'A'
089000090108      * Non abilitato allora proteggo il campo
089100090108     c                   if        §autmtc <>'S'
089200090108     c                   seton                                        13
089300090108     c                   end
089400090108     c                   else
089500080929      * Non abilitato allora proteggo il campo
089600080929     c                   if        §autmtcc <>'S'
089700080929     c                   seton                                        13
089800090108     c                   end
089900021211     c                   end
090000081223     c                   end
090100020219      *record esistente
090200020219     c                   movel     fgtpdr        vidfil
090300020219     c                   move      fgtpdr        vidpdr
090400020219     c                   move      fgttsr        vi3tsr
090500020219     c                   move      fgtctr        vi3ctr
090600081223     c                   move      fgtdiv        vi3div
090700020219     c                   move      fgtprg        vi3prg
090800020219     c                   endif
090900020219
091000020219     c                   exsr      condiz
091100020205     C                   EXSR      CARVI3
091200020125     C*
091300020125     C*
091400020125     C     ENDVD3        ENDSR
091500930910     C*--- PULIZIA SUBFILE RIGHE DI DETTAGLIO ------------------------*
091600020125     C     PULVI3        BEGSR
091700011109     C*---------------------------------------------------------------*
091800930915     C                   SETON                                        88
091900020124     C                   WRITE     CN10C05
092000930915     C                   SETOFF                                       88
092100930917     C*
092200931008     C                   Z-ADD     0             NR2               4 0
092300931008     C                   Z-ADD     0             SAVNR2            4 0
092400930910     C                   ENDSR
092500930910     C*
092600930910     C*--- CARICAMENTO SUBFILE RIGHE DI DETTAGLIO --------------------*
092700020125     C     CARVI3        BEGSR
092800011109     C*---------------------------------------------------------------*
092900930914     C*
093000081223     c                   if        cn02ana= 'A'
093100020125     C                   exsr      car8a
093200021209     c                   else
093300021209     c                   exsr      car8ac
093400021209     c                   end
093500021210     c                   if        almeno_uno <> *blank
093600020125
093700020125     c                   do        50            xx
093800020125     c                   movel     cdpar(xx)     cdpards
093900020125     c                   if        ctd ='999'
094000020125     c                   leave
094100020125     c                   endif
094200020125     c                   movel     ctd           wkctd
094300020125     c
094400020125     c     kfpt          chain     fifpt01l                           91
094500020125     c                   move      ctd           v5ctd
094600020125     c                   movel     dtd           v5des
094700130619      *
094800130619     c                   if        fptatb = 'A'
094900131126     c******             iter
095000130619     c                   endif
095100020218     c
095200020220     c                   move      *blank        v5att
095300020218     c                   if        not *in91 and fptatb = *blank
095400020218     c                   move      'S'           v5att
095500020218     c                   end
095600130614     c                   setoff                                       76
095700130617     c                   if        esc = 'N' and *in01 or
095800130617     c                             esc = 'N' and *in91
095900130617     c                   iter
096000130617      *76 on protegge opzione
096100130617     c*                  move      *blank        v5att
096200130617     c*                  seton                                        76
096300130614     c                   endif
096400080926      *
096500020125     c                   if        fba = '1'
096600020125     c                   move      'B'           v5bas
096700020125     c                   else
096800020125     c                   move      *blank        v5bas
096900020125     c                   end
097000020125
097100020125     C                   ADD       1             NR2
097200020125     C                   WRITE     CN10S05
097300020125     C                   ENDdo
097400020125     C                   z-add     nr2           savNR2
097500090109     c* per coop o per aut prima della supertestata
097600090108     c                   if        cn02ana ='C' or
097700090108     c                             (cn02ana ='A' and cn02prg = *hival)
097800020129     c                   if        fgtddt > 0
097900020129     c                   move      fgtddt        dataiso
098000020129     c                   move      dataiso       dataeur
098100020129     c                   move      dataeur       v5ddt
098200020130     c                   else
098300020130     c                   z-add     0             v5ddt
098400020129     c                   end
098500020129     c                   if        fgtdst > 0
098600020129     c                   move      fgtdst        dataiso
098700020129     c                   move      dataiso       dataeur
098800020129     c                   move      dataeur       v5dst
098900020130     c                   else
099000030304     c                   z-add     31122039      v5dst
099100020129     c                   end
099200020129     c                   if        fgtduv > 0
099300020129     c                   move      fgtduv        dataiso
099400020129     c                   move      dataiso       dataeur
099500020129     c                   move      dataeur       v5duv
099600020130     c                   else
099700020130     c                   z-add     0             v5duv
099800020129     c                   end
099900020129     c                   if        fgtdts > 0
100000020129     c                   move      fgtdts        dataiso
100100020129     c                   move      dataiso       dataeur
100200020129     c                   move      dataeur       v5dts
100300020130     c                   else
100400020130     c                   z-add     0             v5dts
100500020129     c                   end
100600090109     c* se confermato disabilito l'aggiunta
100700090429     c                   if        cn02ana ='C'
100800090429     c                   setoff                                       17
100900090429     c                   end
101000020129     c                   if        fgtdcn > 0
101100090429     c                   seton                                        17
101200020129     c                   move      fgtdcn        dataiso
101300020129     c                   move      dataiso       dataeur
101400020129     c                   move      dataeur       v5dcn
101500020130     c                   else
101600020130     c                   z-add     0             v5dcn
101700020129     c                   end
101800020327     c                   movel     fgtdcv        v5dcv
101900090109     c* per aut con supertestata
102000081223     c                   else
102100081223     c                   move      tgtprg        vi3prg
102200081223     c                   if        Tgtddt > 0
102300081223     c                   move      tgtddt        dataiso
102400081223     c                   move      dataiso       dataeur
102500081223     c                   move      dataeur       v5ddt
102600081223     c                   else
102700081223     c                   z-add     0             v5ddt
102800081223     c                   end
102900081223     c                   if        Tgtdst > 0
103000081223     c                   move      Tgtdst        dataiso
103100081223     c                   move      dataiso       dataeur
103200081223     c                   move      dataeur       v5dst
103300081223     c                   else
103400081223     c                   z-add     31122039      v5dst
103500081223     c                   end
103600081223     c                   if        Tgtduv > 0
103700081223     c                   move      Tgtduv        dataiso
103800081223     c                   move      dataiso       dataeur
103900081223     c                   move      dataeur       v5duv
104000081223     c                   else
104100081223     c                   z-add     0             v5duv
104200081223     c                   end
104300090324     c                   setoff                                       17
104400090324     c* se confermato disabilito l'aggiunta
104500081223     c                   if        Tgtdts > 0
104600090324     c                   seton                                        17
104700081223     c                   move      tgtdts        dataiso
104800081223     c                   move      dataiso       dataeur
104900081223     c                   move      dataeur       v5dts
105000081223     c                   else
105100081223     c                   z-add     0             v5dts
105200081223     c                   end
105300090324     c*
105400090424     c                   if        Tgtdrc > 0
105500090424     c                   move      Tgtdrc        dataiso
105600090324     c                   move      dataiso       dataeur
105700090324     c                   move      dataeur       v5dcn
105800090324     c                   else
105900090324     c                   z-add     0             v5dcn
106000090324     c                   end
106100090324     c*
106200081223     c                   movel     tgtdcv        v5dcv
106300081223     c                   z-add     tgtkm         v5km
106400090401     c                   z-add     tgtpkl        v5pkl
106500081223     c                   end
106600021210
106700021210     c                   endif
106800021210
106900020125     C                   IF        NR2 > 0
107000020125     C                   EXSR      EMISF2
107100020125     C                   END
107200020125
107300020125     C                   ENDSR
107400020125     C*--- EMISSIONE SFL DETTAGLIO  --------------------*
107500020125     C     EMISF2        BEGSR
107600020125     C*-------------------------------------------------*
107700020205     c                   do        *hival
107800020206     c                   clear                   ricar
107900020125     C                   Z-ADD     1             REC2
108000020125     C                   WRITE     CN10Z05
108100020125     C     FORC2         TAG
108200020128     C                   EXFMT     CN10C05
108300030916     c                   setoff                                       44
108400020125     C*
108500020125     C** CMD12 - RITORNO
108600020125     C     *INKL         IFEQ      *ON
108700020125     C                   MOVEL     '1'           ALF1
108800020125     C                   GOTO      ENDVD4
108900020125     C                   ENDIF
109000080926      * in interrogazione non deve eseguire i controlli
109100080926     C  n10              exsr      contrfgt
109200030121
109300020130     c   90              goto      forc2
109400080929     c  n13
109500020411     Corn10*INKf         IFEQ      *ON
109600020130     C                   exsr      aggfgt
109700030303     c                   if        *in54 or *in60
109800030121     c                   goto      forc2
109900030121     c                   endif
110000090108     c                   commit
110100020130     C                   ENDIF
110200020327     c                   if        *in01
110300020327     c                   seton                                        56
110400020327     c                   goto      forc2
110500020327     c                   else
110600020327     c                   setoff                                       56
110700020327     c                   endif
110800020208     C** CMD16 - ANNULLAMENTO TOTALE TARIFFE
110900080929     c  n13
111000020411     Corn10*INKQ         IFEQ      *ON
111100101209     c                   if        %subst(knmus: 1: 3) = 'EDP'
111200101209     c                             and v5dts = 0
111300100907     c**
111400100907     c* Si è voluto permettere a TUTTI di cancellare le tariffe.
111500101209     c*                  IF            v5dts = 0
111600100907     c**
111700081229     c                   if        cn02ana='C'
111800020211     C                   call      'FICN12R'
111900020211     C                   parm                    COMPDR
112000020211     C                   parm                    COMSML
112100020211     C                   parm                    TSR
112200020211     C                   parm                    CTR
112300020211     C                   parm                    PRG
112400081229     c                   else
112500081229     C                   call      'FICN04R'
112600081229     C                   parm                    COMPDR
112700081229     C                   parm                    COMSML
112800081229     C                   parm                    PRG
112900090113     C                   parm                    TSR
113000090113     C                   parm                    CTR
113100081229     c                   end
113200090107     c                   commit
113300030916     c                   else
113400030916     c                   seton                                        44
113500030916     c                   goto      forc2
113600030916     c                   endif
113700020208     C*
113800090107     C                   SETOFF                                       11
113900020423     C                   MOVEL     '1'           ALF1
114000020208     C                   GOTO      ENDVD4
114100020208     C                   ENDIF
114200020125     c                   do        savnr2        nr2
114300020125     c     nr2           chain     cn10s05                            79
114400020125     c   79              leave
114500020424     c                   if        v5opz <> *blank
114600080926      **
114700090918      ** CHIODAZZO BRUTTO MA EFFICACE:
114800090918      **  Attenzione se immessa tariffa "XT" deve esserci la tar."X" altrimenti
114900090918      **     NON DEVE PERMETTERLO:
115000090918     c                   if        v5CTD = 'XT'
115100090918     c                   movel     'X  '         wkctd
115200090923     c     kfpt1         chain     fifpt01l
115300090923     C                   IF        NOT %FOUND(fifpt01l) or
115400090923     C                             (%FOUND(fifpt01l) and fptatb <> ' ')
115500090918     C                   SETON                                        9077
115600090918     c                   goto      forc2
115700090918     c                   end
115800090918     c                   end
115900090918      **
116000100907      ***
116100100907     c                   clear                   a_giornata        1
116200100907      *** Controlla se la selezione scelta è compatibile con le tariffe di
116300100907      ***  dettaglio. NON si possono inserire tariffe a GIORNATA con dettaglio
116400100907      ***  già presente!
116500100907     C*  VA FATTO CON IL CONTROLLO DELLA SCELTA DEL SFL!!!!!
116600100907     c                   if        cn02ana  <>'C' and
116700100907     c                              ( not *in10 or *in17)  and
116800100907     c                              ( v5CTD = 'TIG' or v5CTD = 'TMG' )
116900100907     C* vede se c'è una tariffa a dettaglio
117000100907     C                   movel     'C'           tipoDet           1
117100100907     c     ricontrolla   tag
117200100907     c     kfpt1_det     setll     fifpt01l
117300100907     c     kfpt1_det     reade     fifpt01l
117400100907     C*
117500100907     c                   dow       not %Eof(fifpt01l)
117600100907     c                   if        fptprg = vi3prg
117700100907     c                   move      'S'           a_giornata
117800100907     c                   leave
117900100907     c                   end
118000100907     c     kfpt1_det     reade     fifpt01l
118100100907     c                   enddo
118200100907     C*
118300100907     c                   if         a_giornata <> 'S' and tipoDET = 'C'
118400100907     C                   movel     'R'           tipoDet           1
118500100907     c                   goto      ricontrolla
118600100907     c                   end
118700100907      *
118800100907     C*  se presente già un'altra tariffa di dettaglio
118900100907     C*     sullo stesso tipo servizio/progressivo
119000100907     c                   if        a_giornata  = 'S'
119100100907     C                   SETON                                        7890
119200100907     c                   goto      forc2
119300100907     c                   end
119400100907     C*
119500100907     c                   end
119600090918      **
119700080926      **  se Interrogazione e non c'è "S" non deve entrare sulla tariffa specifica
119800090107      **  se già stampato e non c'è "S" non deve entrare sulla tariffa specifica
119900080926     c                   if        v5ATT <> 'S'
120000080926     c                   if        *in10 or
120100080926     c                             *in17
120200080926     c                   goto      nodettaglio
120300080926     c                   end
120400080926     c                   end
120500020125     c                   clear                   param4
120600020130     c                   move      vidtes        PA4TES
120700081223     c                   move      cn02mod       modali
120800080926     c   15
120900080926     cor 17
121000080926     cor 10              move      '*'           modali
121100020128     c                   movel     vidfil        PA4PDR
121200020125     c                   move      vidpdr        PA4PDR
121300020125     c                   move      despdr        PA4DPD
121400020213     c                   move      comsml        PA4SML
121500020125     c                   move      vi3tsr        PA4TSR
121600020128     c                   move      de3tsr        PA4DTS
121700020125     c                   move      vi3ctr        PA4CTR
121800020128     c                   move      de3ctr        PA4DCT
121900081223     c                   move      cn02ana       PA4anac
122000020130     c                   move      vi2dcv        PA4DCV
122100020125     c                   move      vi3prg        PA4PRG
122200020125     c                   move      *blank        PA4FLG
122300020125     c                   move      v5duv         PA4DUV
122400020219     c                   move      comddt        PA4DDT
122500081223     c                   move      viddiv        PA4div
122600020128     c                   move      v5ctd         PA4ctd
122700020128     c                   move      v5des         PA4dtd
122800020125
122900020125     c                   movel     param4        kpjbu
123000020128     c                   call      'FICN11R'
123100020125     c                   parm                    kpjba
123200020228     C                   MOVEL     savKPJBU      KPJBU
123300090114     c                   if        not *in15 and not *in17 and not *in10
123400090107     C                   COMMIT
123500090114     c                   end
123600080926      **
123700080926     c     nodettaglio   tag
123800020125     c                   move      *blank        v5opz
123900020125     c                   update    cn10s05
124000020206     c                   move      'X'           ricar
124100020424     c                   endif
124200020125     c                   enddo
124300020206      * se sono state effettuate delle scelte per abilitare dei codici di
124400020206      * dettaglio forzo a fine loop delle opzioni scelte l'uscita per ricaricare
124500020206      * il sfl dall'inizio
124600020206     c                   if        ricar <> *blank
124700020206     c                   leave
124800020206     c                   end
124900020205     c                   enddo
125000020125     C     endvd4        ENDSR
125100930910     C*
125200930914     C*--- CONTROLLO DATI SUBFILE RIGHE DI DETTAGLIO -----------------*
125300020125     C     CONSF2        BEGSR
125400011109     C*---------------------------------------------------------------*
125500020206     C                   SETOFF                                       90
125600930914     C                   Z-ADD     1             NR2
125700930914     C*
125800020124     C     NR2           CHAIN     CN10S05                            33
125900930914     C     *IN33         DOWEQ     *OFF
126000930914     C     ENDSF2        TAG
126100020124     c                   end
126200930914     C   90              Z-ADD     NR2           REC2
126300930914     C*
126400020124     C                   UPDATE    CN10S05
126500020124     c
126600931119     C*
126700011126     C     endrt2        ENDSR
126800930826     C*
126900020124     C*--- ----------------------------------------- -----------------*
127000020124     C     Car8a         BEGSR
127100020124     C*---------------------------------------------------------------*
127200020124     c
127300021210     c                   clear                   almeno_uno
127400020125     c                   move      *all'9'       cdpar
127500020124     C                   MOVEL     '8A'          COD
127600020124     c                   z-add     1             xx
127700020124     c     ktab2         setll     tabel00f
127800020124     c                   do        *hival
127900020130     c     ktab2         reade     tabel00f                               94
128000020130     c   94              leave
128100021210     c                   move      'X'           almeno_uno
128200020204     c                   clear                   carok
128300020124     c                   movel     tbluni        ds8a
128400020124     c
128500020204      *controllo il livello della tariffa per caricare i dettagli abilitati
128600020208     c                   if        vi3tsr = 'G' and §8afgi <> *blank
128700020204     c                   move      '1'           carok
128800020124     c                   endif
128900020124     c
129000020327     c                   if        vi3ctr = 999 and §8afpr <> *blank
129100020327     c                             and vi3tsr <> 'G'
129200020204     c                   move      '1'           carok
129300020124     c                   endif
129400020124     c
129500020208     c                   if        vi3ctr < 999 and §8afde <> *blank
129600020204     c                   move      '1'           carok
129700020124     c                   endif
129800020204
129900020204      *se il livello è buono controllo consegna e ritiro se specifici
130000020208     c                   if        carok ='1' and vi3tsr = 'C' and §8ausc <> 'S'
130100020204     c                   clear                   carok
130200020204     c                   endif
130300020204
130400020208     c                   if        carok ='1' and vi3tsr = 'R' and §8ausr <> 'S'
130500020204     c                   clear                   carok
130600020204     c                   endif
130700020204      * se non valido leggo il successivo
130800020204     c                   if        carok = *blank
130900020204     c                   iter
131000020204     c                   endif
131100020204
131200020125     c                   move      *all'9'       cdpards
131300020125     c     cdpards       lookup    cdpar(xx)                              87
131400020124     c                   movel     tblkey        ctd
131500020124     c                   movel     §8ades        dtd
131600130614     c                   move      §8aesccar     esc
131700020124     c                   if        §8atba = 'B'
131800020124     c                   move      '1'           fba
131900020124     c                   else
132000020124     c                   movel     '2'           fba
132100020124     c                   end
132200020124
132300020124     c                   movea     cdpards       cdpar(xx)
132400020124
132500020124     c                   enddo
132600020124
132700020124     c                   sorta     cdpar
132800020124
132900020124     c                   endsr
133000021209     C*--- ----------------------------------------- -----------------*
133100021209     C     Car8ac        BEGSR
133200021209     C*---------------------------------------------------------------*
133300050411      *    Controllo se la filiale è una HUB DPD
133400050411     c                   exsr      HUB_DPD
133500050411      *
133600021218      * prima di caricare determino se la filiale che sto gestendo è un primo
133700021218      * o secondo livello per gestire le voci specifiche
133800030122     c     vidfil        chain     azorg01l
133900030122     c                   movel     orgde3        og143
134000030122
134100021218     c                   clear                   fnlv55ds
134200021218     c                   move      'L'           d55tla
134300021218     c                   move      vidfil        d55lin
134400070131     c                   move      ' '           d55tpt
134500021218     c                   z-add     wuda          d55drf
134600021218     c                   call      'FNLV55R'
134700021218     c                   parm                    fnlv55ds
134800021209     c
134900021210     c                   clear                   almeno_uno
135000021209     c                   move      *all'9'       cdpar
135100021209     C                   MOVEL     '8A'          COD
135200021209     c                   z-add     1             xx
135300021209     c     ktab2         setll     tabel00f
135400021209     c                   do        *hival
135500021209     c     ktab2         reade     tabel00f                               94
135600021209     c   94              leave
135700030122     c                   if        tblflg <> ' '
135800030122     c                   iter
135900030122     c                   end
136000021210     c                   move      'X'           almeno_uno
136100021209     c                   clear                   carok
136200021209     c                   movel     tbluni        ds8a
136300021209     c
136400021209     c                   if        §8afac <> 'C'
136500021209     c                   iter
136600021209     c                   end
136700030123      *  se voce specifica per le poste la carica solo se il network è poste
136800030123     c                   if        §8afpt = 'P' and §ogntw <> 'PPT'
136900030123     c                   iter
137000030123     c                   endif
137100030123      *  se specifica NON POSTE la carica solo se il network NON è poste
137200030123     c                   if        §8afpt = 'N' and §ogntw = 'PPT'
137300030123     c                   iter
137400030123     c                   endif
137500021218      * controllo validità voce per 1° 2° livello o entrambe
137600021218      * se non è valido per entrambe
137700030123     c                   if        §8a12e <>'E'
137800021218      *  se primo liv. e se la voce non è specifica per il primo liv.
137900050411      *   e se non è una voce specifica x DPD di HUB primo livello
138000070131     c                   if        §8a12e <> '1'  and
138100070131     c                             (vidfil = d55tfp or vidfil = d55tfa)
138200050411      *** (1) livello
138300050411      * se è una tariffa DPD e non è una HUB che gestisce DPD
138400050411     c                   if        §8a12e = 'D' and Ntw_DPD_SN = 'N' or
138500050411     c                             §8a12e <>'D'
138600050411     c                   iter
138700050411     c                   endif
138800021218     c                   endif
138900021218      *  se secondo liv. e se la voce non è specifica x il secondo liv.
139000070131     c                   if        §8a12e <> '2' and
139100070131     c                             (vidfil <> d55tfp and vidfil <> d55tfa)
139200021218     c                   iter
139300021218     c                   endif
139400050411     c                   else
139500021218     c                   endif
139600021209      *controllo il tipo della tariffa per caricare i dettagli abilitati
139700021209     c                   if        vi3tsr = 'X' and §8ausc = 'X'
139800021209     c                   move      '1'           carok
139900021209     c                   endif
140000021209     c
140100021209     c                   if        vi3tsr = 'P' and §8ausc = 'P'
140200021209     c                   move      '1'           carok
140300021209     c                   endif
140400021209     c
140500021209     c                   if        vi3tsr = 'T' and §8ausc = 'T'
140600021209     c                   move      '1'           carok
140700021209     c                   endif
140800021209     c
140900021209      * se non valido leggo il successivo
141000021209     c                   if        carok = *blank
141100021209     c                   iter
141200021209     c                   endif
141300021209
141400021209     c                   move      *all'9'       cdpards
141500021209     c     cdpards       lookup    cdpar(xx)                              87
141600021209     c                   movel     tblkey        ctd
141700021209     c                   movel     §8ades        dtd
141800130614     c                   movel     §8aesccar     esc
141900021209     c                   if        §8atba = 'B'
142000021209     c                   move      '1'           fba
142100021209     c                   else
142200021209     c                   movel     '2'           fba
142300021209     c                   end
142400021209     c
142500021209
142600021209     c                   movea     cdpards       cdpar(xx)
142700021209
142800021209     c                   enddo
142900021209
143000021209     c                   sorta     cdpar
143100021209
143200021209     c                   endsr
143300020130     C*---------------------------------------------------------------*
143400020130     C     contrfgt      begsr
143500020130     C*---------------------------------------------------------------*
143600020130     C                   SETOFF                                       90
143700100907      ***
143800020130     C****  DATA DECORRENZA TARIFFA  ****
143900020130     C                   Z-ADD     V5ddt         G02DAT
144000020130     C                   MOVEL     *BLANKS       G02ERR
144100020130     C                   CALL      'XSRDA8'
144200020130     C                   PARM                    WLBDAT
144300020130     C     G02ERR        IFEQ      '1'
144400020130     C                   SETON                                        4590
144500020130     C                   GOTO      ENDCT3
144600020130     C                   ELSE
144700020130     C                   MOVE      G02INV        COMDDT
144800020130     C                   ENDIF
144900020130     C*
145000020130     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6
145100020130     C                   Z-ADD     G02DAT        V5ddt
145200020130     C*
145300020130     C****  DATA SCADENZA TARIFFA  ****
145400020130     C                   Z-ADD     V5dst         G02DAT
145500020130     C                   MOVEL     *BLANKS       G02ERR
145600020130     C                   CALL      'XSRDA8'
145700020130     C                   PARM                    WLBDAT
145800020130     C     G02ERR        IFEQ      '1'
145900020130     C                   SETON                                        4690
146000020130     C                   GOTO      ENDCT3
146100020130     C                   ELSE
146200020130     C                   MOVE      G02INV        COMDST
146300020130     C                   ENDIF
146400020130     C*
146500020130     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6
146600020130     C                   Z-ADD     G02DAT        V5DST
146700020130     C*
146800020130     C* LA DATA SCADENZA DEVE ESSERE MAGGIORE DELLA DATA DECORRENZA
146900020130     C     COMDST        IFLT      COMDDT
147000020130     C                   SETON                                        4790
147100020130     C                   GOTO      ENDCT3
147200020130     C                   ENDIF
147300020423      *verifica se esistono conteggi valorizzati o confermati x la data scadenza
147400040218      * solo per gli aut
147500081223     c                   if        cn02ana<> 'C'
147600080926      *
147700020423     c                   move      'C'           klvl
147800020423     c     kftt          setgt     fiftt02l
147900020423     c     kftt          readpe    fiftt02l                               59
148000020423     c                   if        not *in59
148100020607     c                   if        fttddc >  comdst
148200020403     c                   seton                                        6090
148300020403     C                   GOTO      ENDCT3
148400020422     c                   endif
148500020403     c                   endif
148600020423     c                   move      'V'           klvl
148700020423     c     kftt          setgt     fiftt02l
148800020423     c     kftt          readpe    fiftt02l                               59
148900020423     c                   if        not *in59
149000020607     c                   if        fttddc >  comdst
149100020423     c                   seton                                        6090
149200020423     C                   GOTO      ENDCT3
149300020423     c                   endif
149400020423     c                   endif
149500040218     c                   endif
149600020130      *
149700020130     C* LA DATA SCADENZA DEL PROGRESSIVO PRECEDENTE DEVE ESSERE MINORE
149800020130     C*   DELLA DATA DECORRENZA DEL PROGRESSIVO SEGUENTE
149900020130     C     PRG           ADD       1             COMPRG
150000020130     C     KFGT5         SETLL     fifgt000
150100020130     C     KFGT5         READE(N)  fifgt000                               32
150200020130     C*
150300020130     C     *IN32         DOWEQ     *OFF
150400020130     C     FGTATB        IFEQ      ' '
150500020130     C     COMDST        IFGE      FGTDDT
150600020130     C                   SETON                                        4890
150700020130     C                   GOTO      ENDCT3
150800020130     C                   ELSE
150900020130     C                   SETON                                        32
151000020130     C                   ENDIF
151100020130     C*
151200020130     C                   ELSE
151300020130     C* RECORD ANNULLATO: AGGIUNGO 1 E CERCO IL PROGRESSIVO SEGUENTE
151400020130     C                   ADD       1             COMPRG
151500020130     C                   ENDIF
151600020130     C*
151700020130     C  N32KFGT5         READE(N)  fifgt000                               32
151800020130     C                   ENDDO
151900020130     C*
152000020130     C* LA DATA DECORRENZA DEL PROGRESSIVO SEGUENTE DEVE ESSERE
152100020130     C*   MAGGIORE DELLA DATA SCADENZA DEL PROGRESSIVO PRECEDENTE
152200020130     C     PRG           SUB       1             COMPRG
152300020130     C     KFGT5         SETLL     fifgt000
152400020130     C     KFGT5         READE(N)  fifgt000                               32
152500020130     C*
152600020130     C     *IN32         DOWEQ     *OFF
152700020130     C     FGTATB        IFEQ      ' '
152800020130     C     COMDDT        IFLE      FGTDST
152900020130     C                   SETON                                        4990
153000020130     C                   GOTO      ENDCT3
153100020130     C                   ELSE
153200020130     C*
153300020130     C* LA DATA DECORRENZA PROGRESSIVO SEGUENTE DEVE ESSERE MAGGIORE
153400020130     C*   DI 1 GIORNO DELLA DATA SCADENZA PROGRESSIVO PRECEDENTE
153500020130     C                   ADD       1             FGTDST
153600020130     C                   Z-ADD     FGTDST        DATA
153700020130     C* CONTROLLO SE ANNO BISESTILE
153800020130     C     MM            IFEQ      2
153900020130     C     AA            DIV       4             G02QUA            2 0                        GBRM20
154000020130     C                   MVR                     G02RES            1 0                        GBRM20
154100020130     C     G02RES        IFEQ      0                                                        5 GBRM20
154200020130     C                   Z-ADD     29            XDTF(2)
154300020130     C                   ENDIF
154400020130     C                   ENDIF
154500020130     C*
154600020130     C     GG            IFGT      XDTF(MM)
154700020130     C     GG            SUB       XDTF(MM)      GG
154800020130     C                   ADD       1             MM
154900020130     C     MM            IFEQ      13
155000020130     C                   ADD       1             AA
155100020130     C                   Z-ADD     1             MM
155200020130     C                   ENDIF
155300020130     C                   ENDIF
155400020130     C                   Z-ADD     28            XDTF(2)
155500020130     C*
155600020130     C     COMDDT        IFNE      DATA
155700020130     C                   SETON                                        4990
155800020130     C                   GOTO      ENDCT3
155900020130     C                   ENDIF
156000020130     C*
156100020130     C                   SETON                                        32
156200020130     C                   ENDIF
156300020130     C*
156400020130     C                   ELSE
156500020130     C* RECORD ANNULLATO: SOTTRAGGO 1 E CERCO IL PROGRESSIVO PRECEDENTE
156600020130     C                   SUB       1             COMPRG
156700020130     C* RIESEGUO POSIZIONAMENTO CON PROGRESSIVO PRECEDENTE
156800020130     C     KFGT5         SETLL     fifgt000
156900020130     C                   ENDIF
157000020130     C*
157100020130     C  N32KFGT5         READE(N)  fifgt000                               32
157200020130     C                   ENDDO
157300040827     **
157400020807      *verifica se il codice autista di riferimento esiste
157500020807     c                   setoff                                       2526
157600020610     c                   setoff                                       37
157700020130     c     endct3        endsr
157800030121     C*---------------------------------------------------------------*
157900030121     C     contrfgt2     begsr
158000030121     C*---------------------------------------------------------------*
158100090116     c                   clear                   paramt
158200081223     c                   if        *in32 and cn02ana= 'C' and vi3tsr = 'X'
158300030121     c                   z-add     comddt        datax
158400030121     c                   movel     vidfil        pogesx
158500030121     c                   movel     vi3tsr        tipopre
158600030121     c                   call      'FICN36R2'
158700030121     c                   parm                    paramt
158800030303     c                   endif
158900030303      * controllo per le coop che non ci siano valorizzazioni confermate
159000030303      * con data superiore alla data scadenza della tariffa
159100030303     c                   movel     fgtpdr        cttpo             3 0
159200030303     c     cttpo         setgt     fictt01l
159300030303     c                   do        *hival
159400030303     c     cttpo         readpe    fictt01l                               59
159500030303     c   59              leave
159600030303      * cerco il primo confermato andando all'indietro
159700030303     c                   if        cttfvl <> 'C' or ctttsr <> fgttsr
159800030303     c                   iter
159900030303     c                   endif
160000030303      * se lo trovo lo confronto con la data scadenza se è maggiore segnalo
160100030303      * l'errore
160200030303     c                   if        cttddc >  comdst
160300030303     c                   seton                                        6090
160400030303     C                   GOTO      ENDCT4
160500030303     c                   endif
160600030303     c                   enddo
160700030121
160800030303     c     endct4        endsr
160900020130     C*---------------------------------------------------------------*
161000020130     C     aggfgt        begsr
161100020130     C*---------------------------------------------------------------*
161200020130     C*
161300090116     c                   setoff                                       54
161400020130     C* 32 ON  - IMMISSIONE
161500020130     C* 32 OFF - AGGIORNAMENTO
161600081223     C     KFGT3         CHAIN     fifgt000                           32
161700020130     C   32              clear                   fifgt000
161800090109     C   32              clear                   comfgt
161900090109     C  n32              eval      comfgt = fgtds
162000030121      *se non c'è errore verifico x le coop l'esistenza di un'altra tariffa
162100030121      * valida nel periodo NON POSSONO ESISTERE 2 TARIFFE VALIDE ANCHE SE I
162200030121      *CODICI DELLE COOPERATIVE SONO DIVERSI
162300081223     c                   if        cn02ana= 'C'
162400030121     c                   exsr      contrfgt2
162500030303     c   60              goto      endagg
162600030121     c                   if        error <> *blank
162700030121     c                   seton                                        54
162800030121     c                   goto      endagg
162900030121     c                   endif
163000030303     c                   endif
163100081229     c*
163200020130     C                   MOVEL     ' '           FGTATB
163300020130     c                   if        v5ddt > 0
163400020130     c                   move      v5ddt         dataeur
163500020130     c                   move      dataeur       dataiso
163600020130     c                   move      dataiso       fgtddt
163700020130     c                   end
163800020130     c                   if        v5dst > 0
163900020130     c                   move      v5dst         dataeur
164000020130     c                   move      dataeur       dataiso
164100020130     c                   move      dataiso       fgtdst
164200020130     c                   end
164300020130     C                   MOVEL     VI2DCV        FGTDCV
164400020529     c                   movel     v5dcv         fgtdcv
164500020130     C* SOLO PER IMMISSIONE
164600020130     C     *IN32         IFEQ      *ON
164700020130     C                   Z-ADD     COMPDR        FGTPDR
164800020130     C                   MOVEL     COMSML        FGTSML
164900020130     C                   MOVEL     VI3TSR        FGTTSR
165000020130     C                   Z-ADD     VI3CTR        FGTCTR
165100020130     C                   Z-ADD     VI3PRG        FGTPRG
165200081229     C                   MOVEL     VIddiv        FGTdiv
165300100908     c********           clear                   fgtDTS
165400100908     c********           clear                   fgtDCN
165500020130     C                   ENDIF
165600020130     C*
165700020130     C* ESEGUO WRITE/UPDAT DI fifgt00F SOLO SE SONO STATI INSERITI O
165800020130     C*   VARIATI DEI CAMPI
165900020130     C     COMFGT        IFNE      FGTDS
166000020130     C                   Z-ADD     UDATE8        V5duv
166100020130     C                   Z-ADD     WUDA          FGTDUV                         *DATA ULT.VARIAZIONE
166200020130     C*
166300020130     C   32              WRITE     fifgt000
166400020130     C  N32              UPDATE    fifgt000
166500020130     C                   ENDIF
166600020130     C*
166700020130     C                   SETOFF                                       01
166800020130     C                   SETON                                        02
166900030121     C     endagg        ENDSR
167000020125     C*--- OPERAZIONI INIZIALI ---------------------------------------*
167100020125     C     OPERAZ        BEGSR
167200020125     C*---------------------------------------------------------------*
167300020125     C* TESTATA PRG
167400020125     C*
167500020125     C* ALF1 = '1' ===> GESTIONE VIDEATA DELLE SELEZIONI
167600020125     C                   MOVEL     '1'           ALF1              1
167700020125     C*
167800020125     C* 19 ON  - UTILIZZARE TARIFFE     DI SIMULAZIONE
167900020125     C* 19 OFF - UTILIZZARE TARIFFE NON DI SIMULAZIONE
168000081223     C     cn02sml       IFEQ      'S'
168100020125     C                   SETON                                        19
168200020125     C                   ENDIF
168300020125     C*
168400020125     C***
168500020125     C* GIRO DATA DEL GIORNO: LA PRENDO DA TIME
168600020125     C***
168700020125     C                   TIME                    W0120            14 0
168800020125     C                   MOVE      W0120         WDAT              8 0
168900020125     C*
169000020125     C                   Z-ADD     WDAT          G02DAT
169100020125     C                   MOVEL     *BLANK        G02ERR
169200020125     C                   CALL      'XSRDA8'
169300020125     C                   PARM                    WLBDAT
169400020125     C* UDATE A 8 IN AAAA/MM/GG
169500020125     C                   Z-ADD     G02INV        WUDA
169600020125     C* DATA DEL GIORNO IN GG/MM/AAAA
169700020125     C                   Z-ADD     G02DAT        UDATE8
169800020125     C* CARICAMENTO TABELLE
169900020125     C                   EXSR      CARTAB
170000020125     C* PULISCO CODICE PADRONCINO E DESCRIZIONE SOLO A INIZIO PGM
170100020125     C*   IN MODO CHE NELLA PRIMA VIDEATA RESTI IMPOSTATO L'ULTIMO
170200020125     C                   MOVEL     *BLANKS       VIDPDR
170300020125     C                   MOVE      '?'           VIDPDR
170400020125     C                   MOVEL     *BLANKS       DESPDR
170500090108     c                   if        cn02ana = 'A' and cn02pdr <> 0
170600081223     c                   movel     cn02pdr       vidfil
170700081223     c                   move      cn02pdr       vidpdr
170800081223     C                   MOVEL     VIDFIL        COMPDR
170900081223     C                   MOVE      VIDPDR        COMPDR
171000081223     C     kapdc         CHAIN     fiapd000                           32
171100090610     c                   if        not *in32 and not *in10
171200090610     C     APDATB        IFNE      ' '
171300081223     C                   SETON                                        32
171400081223     C                   ENDIF
171500090610     C                   ENDIF
171600081223     C* CODICE PADRONCINO ERRATO
171700081223     C   32              SETON                                        4090
171800100204      *
171900100204     c*  se escluso da autofatturazione dare msg di errore
172000100204     c                   if        apdpdd = 'S' and cn02ana<>'C'
172100100204     C                   SETON                                        4090
172200100204     c                   end
172300100204      *
172400100224     C                   MOVEL     APDRSF        DESPDR
172500100224     C********           MOVEL     APDRSC        DESPDR
172600100224      *
172700081223     c                   end
172800100224      *
172900020125     C                   ENDSR
173000020125     C*
173100020125     C*--- CARICAMENTO TABELLE ---------------------------------------*
173200020125     C     CARTAB        BEGSR
173300020125     C*---------------------------------------------------------------*
173400020125     C****  CODICI TARIFFA E DESCRIZIONI: TTR / DTR / MIN  ****
173500020125     C                   Z-ADD     1             X                 3 0
173600020125     C                   MOVEL     'TR'          COD
173700020125     C     KTAB2         SETLL     TABEL
173800020125     C     KTAB2         READE     TABEL                                  32
173900020125     C*
174000020125     C     *IN32         DOWEQ     *OFF
174100020125     C     TBLFLG        IFEQ      ' '
174200020125     C                   MOVEL     TBLUNI        DSTR
174300020125     C* CARICO SOLO I CODICI TARIFFA VALIDI PER LE TARIFFE PADRONCINI
174400081223     c                   if        cn02ana= 'A' and §trutp = 'S'
174500020125     C                   MOVEL     TBLKEY        TTR(X)
174600020125     C                   MOVEL     TBLUNI        DSTR
174700020125     C                   MOVEL     §TRDES        DTR(X)
174800020125     C* MINIMO TASSABILE PER TIPO TARIFFA
174900020125     C                   Z-ADD     §TRATA        MIN(X)
175000020125     C                   ADD       1             X
175100020125     C                   ENDIF
175200020125     C*
175300021209     C* CARICO SOLO I CODICI TARIFFA VALIDI PER LE TARIFFE COOP
175400081223     c                   if        cn02ana= 'C' and §trutp = 'C'
175500021209     C                   MOVEL     TBLKEY        TTR(X)
175600021209     C                   MOVEL     TBLUNI        DSTR
175700021209     C                   MOVEL     §TRDES        DTR(X)
175800021209     C* MINIMO TASSABILE PER TIPO TARIFFA
175900021209     C                   Z-ADD     §TRATA        MIN(X)
176000021209     C                   ADD       1             X
176100021209     C                   ENDIF
176200021209     C*
176300020125     C                   ENDIF
176400020125     C     KTAB2         READE     TABEL                                  32
176500020125     C                   ENDDO
176600020125     C*
176700020125     C****  TIPI PRESTAZIONE E DESCRIZIONI: TPS / DSR / CDT  ****
176800020125     C                   Z-ADD     1             X
176900020125     C                   MOVEL     '1Z'          COD
177000020125     C     KTAB2         SETLL     TABEL
177100020125     C     KTAB2         READE     TABEL                                  32
177200020125     C*
177300020125     C     *IN32         DOWEQ     *OFF
177400020125     C     TBLFLG        IFEQ      ' '
177500021209     C                   MOVEL     TBLUNI        DS1Z
177600090109     c                   if        (cn02ana= 'A' and §1zfac ='A') or
177700090109     c                             (cn02ana= 'C' and §1zfac ='C')
177800020125     C                   MOVEL     TBLKEY        TPS(X)
177900020125     C                   MOVEL     §1ZDES        DSR(X)
178000020125     C* CODICE TARIFFA DEL TIPO PRESTAZIONE
178100020125     C                   MOVEL     §1ZCTR        CDT(X)
178200020125     C                   ADD       1             X
178300020125     C                   ENDIF
178400021210     C                   ENDIF
178500020125     C     KTAB2         READE     TABEL                                  32
178600020125     C                   ENDDO
178700020125     C****  TIPI SOMMA PESI E DESCRIZIONI: TSM / DSM  ****
178800020125     C                   Z-ADD     1             X
178900020125     C                   MOVEL     '1N'          COD
179000020125     C     KTAB2         SETLL     TABEL
179100020125     C     KTAB2         READE     TABEL                                  32
179200020125     C*
179300020125     C     *IN32         DOWEQ     *OFF
179400020125     C     TBLFLG        IFEQ      ' '
179500020125     C                   MOVEL     TBLKEY        TSM(X)
179600020125     C                   MOVEL     TBLUNI        DSM(X)
179700020125     C                   ADD       1             X
179800020125     C                   ENDIF
179900020125     C     KTAB2         READE     TABEL                                  32
180000020125     C                   ENDDO
180100020125     C****  TIPI TARIFFA PIKING/ETICHET. E DESCRIZIONI: TPE / DPE  ****
180200020125     C                   Z-ADD     1             X
180300020125     C                   MOVEL     '1K'          COD
180400020125     C     KTAB2         SETLL     TABEL
180500020125     C     KTAB2         READE     TABEL                                  32
180600020125     C*
180700020125     C     *IN32         DOWEQ     *OFF
180800020125     C     TBLFLG        IFEQ      ' '
180900020125     C                   MOVEL     TBLKEY        TPE(X)
181000020125     C                   MOVEL     TBLUNI        DPE(X)
181100020125     C                   ADD       1             X
181200020125     C                   ENDIF
181300020125     C     KTAB2         READE     TABEL                                  32
181400020125     C                   ENDDO
181500020125     C* VEDO SE SONO SIMFEL O REMOTO
181600020701     C                   EVAL      VidFil = SimPOU
181700020701     C                   EVAL      VidPOG = SimPOU
181800020125     C*
181900020125     C                   ENDSR
182000020125     C*
182100020125      *--------------------------------------------------
182200020125     c     *inzsr        begsr
182300020125      *--------------------------------------------------
182400020125
182500020125     C     *ENTRY        PLIST
182600020125     C                   PARM                    KPJBA
182700020125     C                   Z-ADD     1             CODUT
182800020125     C                   CALL      'X§PARUT'
182900020125     C                   PARM                    UT§DSE
183000020125     C                   MOVEL     RAGUT         RSUT             20
183100020125     C                   MOVEL     REC80         CNCR80
183200020125     C*
183300081223     C                   MOVEL     KPJBU         ficn02ds
183400090109     c* per aut: se richiamato da menù forzo l'interrogazione xchè
183500090109     c* bisogna passare dalla supertestata
183600090429     c                   if        kcdaz = 'FN1B' or
183700090108     c                             kcdaz = 'FN11'
183800090108     c                   seton                                        64
183900090108     c                   clear                   CN02PDR
184000090108     c                   eval      CN02PRG = *hival
184100090108     c                   end
184200020215     C                   MOVEL     KPJBU         savkpjbu
184300090109     c*
184400081223     c     cn02ana       comp      'C'                                    63
184500030303     c   63              open      fictt01l
184600090109     c* apro il file in commit solo se manutenzione
184700090108     c                   if        cn02mod <> ' '
184800090108     c                   eval      flg = *off
184900090108     c                   else
185000090108     c                   eval      flg = *on
185100090108     c                   end
185200090108     c                   if        not %open(fifgt01l)
185300090108     c                   open      fifgt01l
185400090108     c                   end
185500090324     c* decodifiche a video
185600090324     c                   if        cn02ana  = 'C'
185700090324     c                   eval      desdts = 'Data Stampa'
185800090324     c                   eval      desdcn = 'Data Ric.Stampa'
185900090324     c                   eval      decpdr = 'Cooperat.'
186000090324     c                   else
186100090324     c                   eval      decpdr = 'Autotrasp.'
186200090324     c                   eval      desdts = 'Conval.fil.'
186300090424     c                   eval      desdcn = 'Ric.Copia Firm.'
186400090324     c                   end
186500020125     C*
186600020125     C*---------------------------------------------------------------*
186700020125     C* ACCESSO TABEL
186800020125     C     KTAB          KLIST
186900020125     C                   KFLD                    CODUT
187000020125     C                   KFLD                    COD
187100020125     C                   KFLD                    KEY
187200020125     C     KTAB2         KLIST
187300020125     C                   KFLD                    CODUT
187400020125     C                   KFLD                    COD
187500020125     C* ACCESSO fifgt
187600081223     C     KTGT          KLIST
187700020125     C                   KFLD                    COMPDR
187800020125     C                   KFLD                    COMSML
187900081223     C                   KFLD                    cn02prg
188000081223     C     KFGT          KLIST
188100081223     C                   KFLD                    COMPDR
188200081223     C                   KFLD                    COMSML
188300020125     C     KFGT1         KLIST
188400020125     C                   KFLD                    COMPDR
188500020125     C                   KFLD                    COMSML
188600020125     C                   KFLD                    VIDTSR
188700020125     C     KFGT2         KLIST
188800020125     C                   KFLD                    COMPDR
188900020125     C                   KFLD                    COMSML
189000020125     C                   KFLD                    VIDTSR
189100020125     C                   KFLD                    COMCTR
189200020125     C     KFGT3         KLIST
189300020125     C                   KFLD                    COMPDR
189400020125     C                   KFLD                    COMSML
189500020125     C                   KFLD                    TSR
189600020125     C                   KFLD                    CTR
189700020125     C                   KFLD                    PRG
189800090511     C     KFGT4         KLIST
189900090511     C                   KFLD                    COMPDR
190000090511     C                   KFLD                    COMSML
190100090511     C                   KFLD                    VIDTSR
190200090511     C                   KFLD                    COMCTR
190300090511     C                   KFLD                    cn02prg
190400020125     C     KFGT5         KLIST
190500020125     C                   KFLD                    COMPDR
190600020125     C                   KFLD                    COMSML
190700020125     C                   KFLD                    TSR
190800020125     C                   KFLD                    CTR
190900020125     C                   KFLD                    COMPRG
191000020125     C     KFpt          KLIST
191100020125     C                   KFLD                    fgtpdr
191200020125     C                   KFLD                    fgtsml
191300020125     C                   KFLD                    fgttsr
191400020125     C                   KFLD                    fgtctr
191500020125     C                   KFLD                    fgtprg
191600020125     C                   KFLD                    wkctd
191700090923     C     KFpt1         KLIST
191800090923     c                   KFLD                    fgtpdr
191900090923     C                   KFLD                    fgtsml
192000090923     c                   KFLD                    vi3tsr
192100090923     c                   KFLD                    vi3ctr
192200090923     c                   KFLD                    vi3prg
192300090923     C                   KFLD                    wkctd
192400100907     C     KFpT1_gio     KLIST
192500100907     C                   KFLD                    COMPDR
192600100907     C                   KFLD                    COMSML
192700100907     C                   KFLD                    tipoGio
192800100907     C                   KFLD                    ctarGio
192900100907     C     KFpT1_det     KLIST
193000100907     C                   KFLD                    COMPDR
193100100907     C                   KFLD                    COMSML
193200100907     C                   KFLD                    tipoDet
193300020423     C     Kftt          KLIST
193400020423     C                   KFLD                    fgtpdr
193500020423     C                   KFLD                    klvl              1
193600020125     C* ACCESSO Tntbe
193700020125     C     Ktbe          KLIST
193800020125     C                   KFLD                    kcod1
193900020125     C                   KFLD                    kke1
194000021203     C* ACCESSO fiapd
194100021203     C     Kapdc         KLIST
194200081223     C                   KFLD                    cn02ana
194300021203     C                   KFLD                    compdr
194400020125     C* DEFINIZIONE CAMPI
194500020125     C     *LIKE         DEFine    TBECOD        KCOD1
194600020125     C     *LIKE         DEFine    TBEKE1        KKE1
194700020125     C     *LIKE         DEFINE    FGTDDT        WUDA                           *AAAA/MM/GG
194800020125     C     *LIKE         DEFINE    FGTDDT        UDATE8                         *GG/MM/AAAA
194900020125     C     *LIKE         DEFINE    FGTPDR        COMPDR
195000020125     C     *LIKE         DEFINE    FGTCTR        COMCTR
195100020125     C     *LIKE         DEFINE    FGTPRG        COMPRG
195200020125     C     *LIKE         DEFINE    FGTSML        COMSML
195300020125     C     *LIKE         DEFINE    FGTDDT        COMDDT
195400020125     C     *LIKE         DEFINE    FGTDST        COMDST
195500020125     C     *LIKE         DEFINE    TBLCOD        COD
195600020125     C     *LIKE         DEFINE    TBLKEY        KEY
195700020125     C     *LIKE         DEFINE    VI4SGL        DUPSGL
195800020125     C     *LIKE         DEFINE    VI4ITR        DUPITR
195900020125     C*
196000020125     c                   endsr
196100020128     C*--- CONDIZIONO EMISSIONE DATI A VIDEO -------------------------*
196200020128     C     CONDIZ        BEGSR
196300020128     C*---------------------------------------------------------------*
196400020128     C                   SETOFF                                       84
196500020128     C****  TIPO PRESTAZIONE  ****
196600020128     C                   MOVEL     TSR           VI3TSR
196700020128     C                   MOVEL     *BLANKS       DE3TSR
196800020128     C                   Z-ADD     1             X
196900020128     C     TSR           LOOKUP    TPS(X)                                 35
197000020128     C   35              MOVEA     DSR(X)        DE3TSR
197100020128     C****  CODICE TARIFFA  ****
197200020128     C                   MOVEL     CTR           VI3CTR
197300020128     C                   MOVEL     *BLANKS       DE3CTR
197400020128     C                   Z-ADD     1             X
197500020128     C                   MOVEL     CTR           UNOCTR
197600020128     C     UNOCTR        LOOKUP    TTR(X)                                 35
197700020128     C   35              MOVEA     DTR(X)        DE3CTR
197800020128     C* SALVO INDICE USATO PER CONTROLLARE L'APPLICAZIONE TARIF. FINITA
197900020128     C   35              Z-ADD     X             Y
198000020128     c                   endsr
198100050411     C*--------------------------------------------------------------------
198200050411      *  Controlla se è una HUB che gestisce DPD
198300050411     C*--------------------------------------------------------------------
198400050411     c     HUB_DPD       begSR
198500050411
198600050411     c                   move      'N'           Ntw_DPD_SN        1
198700050411      * Memorizza FGS a parte x controllo HUB DPD gestita dalla filiale
198800050411     c                   move      vidfil        fgskey            3 0
198900050411      **
199000050411     c     kcae          klist
199100050411     c                   kfld                    tipKey
199200050411     c                   kfld                    kfgs_zero
199300050411     c                   kfld                    fgskey
199400050411      **
199500050411     c                   move      'C'           tipkey            1
199600050411     c                   z-add     0             kfgs_zero         3 0
199700050411      **
199800050411     c                   open      azcae03l
199900050411      **
200000050411      *  Su AZCAE ci sono le linee gestite
200100050411     c     kcae          setll     azcae03l
200200050411     c     kcae          reade     azcae03l
200300050411      **
200400050411     c                   dow       not %eof(azcae03l)
200500050411      **
200600050411      **   se attivo nel range di date:
200700050411     c                   if        WUDA >= caedde and WUDA <= caedsc
200800050411      **
200900050411      **  test su AZORG se network DPD
201000050411     c     CAETFA        chain     azorg01l
201100050411     c                   if        %Found(azorg01l)
201200050411     c                   movel     orgde3        og143
201300130220     c*****    il test di DPD si può fare solo x Network senza coinvolgere l'HUB
201400130220     c************       if        §OGHb1 <> *blank and §OGNTW = 'DPD'
201500130220     c                   if         §OGNTW = 'DPD'
201600050411     c                   move      'S'           Ntw_DPD_SN
201700050411     c                   leave
201800050411     c                   end
201900050411     c                   end
202000050411      **
202100050411     c                   endif
202200050411      **
202300050411     c     kcae          reade     azcae03l
202400050411     c                   enddo
202500050411      *
202600050411     c                   close     azcae03l
202700050411      *
202800050411     c                   endSR
202900050411     C*--------------------------------------------------------------------
203000020130**   XDTF
203100020130312831303130313130313031
