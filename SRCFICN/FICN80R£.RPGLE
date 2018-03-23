000100131007     H DECEDIT('0,') DATEDIT(*DMY.)
000200000808     H*         - STAMPA RIEPILOGO CONTEGGI PADRONCINI
000300000000     H*--------------------------------------------------------------*
000400030723     FFICN80D   CF   E             WORKSTN
000500021203     FFIAPD01L  IF   E           K DISK
000600930707     F*
000700950125     D* SCHIERE PER CARICAMENTO L1 E L6
000800920810     D KPJBA         E DS
000900950125     D*
001000011115     D* PASSAGGIO DATI A PGM DI STAMPA RIEPILOGO         - FICND6R -
001100931126     D PARAM           DS
001200030723     d  data1                         8  0
001300030723     d  data2                         8  0
001400030723     d  aut1                          7  0
001500030723     d  aut2                          7  0
001600031114     d  prosp                         1
001700031114     d  codf                          8
001800031114     d  note                          1
001900950125     D                                     DIM(30)                              SKI COMODO
002000910423     D WLBDAT          DS
002100950125     D  G02DAT                 1      8  0
002200950125     D  G02INV                 9     16  0
002300950125     D  G02ERR                17     17
002400950125     D  G02TGI                18     22  0
002500931015     D                 DS
002600931015     D  VI1PF1                 1      3  0
002700931116     D  VI1PD1                 4      7
002800931115     D  VI1P1                  1      7  0
002900931126     D                 DS
003000931126     D  VI1PF2                 1      3  0
003100931126     D  VI1PD2                 4      7
003200931126     D  VI1P2                  1      7  0
003300021203     D FNLV24DS      E DS
003400030723     D tibs36DS      E DS
003500061109     D fnlv55ds      E DS
003600991228     D CNCR80        E DS
003700900615     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
003800131007     D uteautds      E DS
003900131007     d tibs42ds      e ds
004000100617     d dvpocont      e ds
004100100617     d tibs02ds      e ds
004200030723     d w01dd1          s                   like(data1)
004300030723     d w01dd2          s                   like(data2)
004400061109     d dataiso         s               d   datfmt(*iso)
004500000000     I/SPACE 3
004600900521     C****************************************************************
004700900521     C*  RIEPILOGO INDICATORI
004800900521     C***************************************************************
004900931116     C* 30    - DI COMODO
005000010906     C* 40/56 - ERRORI
005100950125     C* 89    - SONO UN "REMXXX"
005200900521     C* 90    - ON INDICA CHE C'E' UN ERRORE NEI CAMPI
005300900521     C*****************************************************************
005400000000     C     *ENTRY        PLIST
005500000000     C                   PARM                    KPJBA
005600000000     C                   Z-ADD     1             CODUT
005700900615     C                   CALL      'X§PARUT'
005800900615     C                   PARM                    UT§DSE
005900000000     C                   MOVEL     RAGUT         RSUT             20
006000900615     C                   MOVEL     REC80         CNCR80
006100100617     c                   move      *date         dataiso
006200100617     c                   move      dataiso       wuda              8 0
006300030723      *ricerco terminal legato al profilo
006400030723     C                   CLEAR                   Tibs36Ds
006500030723     C                   EVAL      I36ute = knmus
006600030723     C                   CALL      'TIBS36R'
006700030723     C                   PARM                    tibs36ds
006800100617      * controllo se attivare o no controllo autorizzazioni e gestione terminal
006900100617     c                   clear                   tibs02ds
007000100617     c                   clear                   dvpocont
007100100617     c                   eval      t02mod = 'C'
007200100617     c                   eval      t02sif = knsif
007300100617     c                   eval      t02cod = 'VPO'
007400100617     c                   eval      t02ke1 = 'CONT'
007500100617     c                   call      'TIBS02R'
007600100617     c                   parm                    kpjba
007700100617     c                   parm                    tibs02ds
007800100617    2c                   if        t02err = *blanks
007900100617     c                   eval      dvpocont  = t02uni
008000100617    2c                   endif
008100100318
008200061109     c                   if        o36pou <> 046
008300140424     c                   seton                                        46
008400061109     c                   clear                   fnlv55ds
008500061109     c                   move      'L'           d55tla
008600061109     c                   move      o36pou        d55lin
008700100617     c                   if        wuda >= §vpoter
008800100617     c                   move      'E'           d55tpt
008900100617     c                   else
009000100617     c                   move      ' '           d55tpt
009100100617     c                   endif
009200061109     c                   move      dataiso       d55drf
009300061109     c                   call      'FNLV55R'
009400061109     c                   parm                    fnlv55ds
009500061109     c                   z-add     d55tfp        termp             3 0
009600061109     c                   z-add     d55tfa        terma             3 0
009700061109      *se profilo di filiale e i terminal P/A sono uguali proteggo il campo
009800061109     c                   if        terma <>termp
009900061109     c                   setoff                                       46
010000061109     c                   endif
010100061109     c                   endif
010200021203     C*
010300021203     C     KAPDV         KLIST
010400021203     C                   KFLD                    APDTIP
010500021203     C                   KFLD                    VI1P1
010600021203     C*
010700030723     C                   MOVEL     'D'           APDTIP
010800931116     C*---------------------------------------------------------------*
010900910419     C*
011000950411     C* OPERAZIONI INIZIALI
011100931111     C                   EXSR      PUL01
011200950411     C*
011300950411     C     INIZIO        TAG
011400950411     C                   MOVEL     '0000'        VI1PD1
011500950411     C                   MOVEL     '9999'        VI1PD2
011600950411     C                   CLEAR                   VI1DD1
011700950411     C                   CLEAR                   VI1DD2
011800020517     C                   MOVEL     '1'           VI1TSP
011900031114     C                   MOVEL     'N'           Vnote
012000031114     C                   CLEAR                   V1cdf
012100930426     C*
012200931015     C     FOR01         TAG
012300030723     C                   EXFMT     LR80D01
012400931116     C*
012500920323     C** CMD3 - FINE LAVORO
012600931112     C   KC              GOTO      FINE
012700931015     C* CONTROLLI FORMATO1
012800900528     C                   EXSR      CONTR1
012900950125     C  NKF
013000931116     COR 90              GOTO      FOR01
013100931116     C*
013200931202     C     VI1PD1        IFEQ      *BLANKS
013300931207     C                   MOVEL     *ZEROS        VI1PD1
013400931202     C                   ENDIF
013500931202     C     VI1PD2        IFEQ      *BLANKS
013600931207     C                   MOVEL     *ZEROS        VI1PD2
013700931202     C                   ENDIF
013800030723     C                   MOVE      VI1P1         aut1
013900030723     C                   MOVEL     VI1P2         aut2
014000030723     c                   move      w01dd1        data1
014100030723     c                   move      w01dd2        data2
014200030723     c                   move      vi1tsp        prosp
014300031114     c                   move      v1cdf         codf
014400031114     c                   move      vnote         note
014500931126     C                   MOVEL     PARAM         KPJBU
014600931202     C*
014700011115     c     knmus         ifeq      *all'1'
014800030723     C                   call      'FICN81R'
014900011115     C                   parm                    Kpjba
015000011115     c                   else
015100030723     c                   movel     'FN81'        kcoaz
015200931116     C                   CALL      'BCH10'
015300931116     C                   PARM                    KPJBA
015400011115     c                   end
015500900611     C*
015600950411     C                   GOTO      INIZIO
015700000000     C     FINE          TAG
015800931027     C                   SETON                                        LR
015900931111     C*
016000931111     C* PULIZIA CAMPI FORMATO 1 --------------------------------------*
016100931111     C     PUL01         BEGSR
016200950125     C***
016300950125     C* VEDO SE SONO SIMFEL O REMOTO
016400950125     C***
016500061109     C                   EVAL      Vi1PF1 = termp
016600061109     C                   EVAL      Vi1PF2 = vi1pf1
016700950125     C                   SETON                                        89
016800000203     C                   TIME                    W0140            14 0
016900000203     C* UDATE IN GGMMAAAA
017000000203     C                   MOVE      W0140         WDTGIO            8 0
017100000203     C                   MOVE      WDTGIO        ANNOCO            4 0
017200000203     C     ANNOCO        ADD       1             ANNOPI            4 0
017300000203     C     ANNOCO        SUB       1             ANNOME            4 0
017400140424     C                   SETOFF                                       904849
017500140424     c                   if        o36pou <> 046
017600140424     c                   if        vi1pf1 <> terma and vi1pf1 <> termp
017700140424     c                             or vi1pf2 <> vi1pf1
017800140424     c                   seton                                        4890
017900140424     C                   GOTO      ENDpul
018000140424     c                   endif
018100140424     c                   endif
018200140424      * controllo autorizzazione del profilo
018300140424     c                   if        wuda    >= §vpoaut
018400140424     c                   clear                   tibs42ds
018500140424     c                   movel     vi1pf1        i42pge
018600140424     c                   call      'TIBS42R'
018700140424     c                   parm                    TIBS42ds
018800140424     c                   movel     o42uni        uteautds
018900140424      *specifiche asteriscate le autorizzazioni per ora non sono attive
019000140424      * in quanto la conferma è presente solo nel menu di rf1
019100140424     c                   if        §affcon <> 'S'
019200140424     c                   return
019300140424     c                   endif
019400140424     c                   endif
019500950125     C*
019600140424     C     endpul        ENDSR
019700900528     C*
019800900608     C*--- CONTROLLI FORMATO1 ----------------------------------------*
019900900528     C     CONTR1        BEGSR
020000931126     C*
020100030723     C* D A T A   VIAGGI: DAL
020200931126     C                   MOVE      VI1DD1        G02DAT
020300931126     C                   MOVEL     *BLANK        G02ERR
020400950125     C                   CALL      'XSRDA8'
020500931126     C                   PARM                    WLBDAT
020600931126    1C     G02ERR        IFEQ      '1'
020700931126     C                   SETON                                        43  90
020800931126     C                   GOTO      ENDCT1
020900931126    1C                   END
021000931126     C                   Z-ADD     G02INV        W01DD1
021100950125     C                   Z-ADD     G02DAT        VI1DD1
021200011115     C                   z-add     vi1dd1        an1               4 0
021300000203     C* CONTROLLO L'ANNO
021400000203     C                   Z-ADD     G02DAT        W0040             4 0
021500000203     C     W0040         IFGT      ANNOPI
021600000203     C     W0040         ORLT      ANNOME
021700000203     C                   SETON                                        43  90
021800000203     C                   GOTO      ENDCT1
021900000203    1C                   END
022000931126     C*
022100021106     C* comunque imposta il 2 campo data
022200021106     C                   Z-ADD     w01dd1        w01dd2
022300931126    1C     VI1DD2        IFGT      0
022400931126     C                   MOVE      VI1DD2        G02DAT
022500931126     C                   MOVEL     *BLANK        G02ERR
022600950125     C                   CALL      'XSRDA8'
022700931126     C                   PARM                    WLBDAT
022800931126    2C     G02ERR        IFEQ      '1'
022900931126     C                   SETON                                        44  90
023000931126     C                   GOTO      ENDCT1
023100931126    2C                   END
023200931126     C                   Z-ADD     G02INV        W01DD2
023300950125     C                   Z-ADD     G02DAT        VI1DD2
023400011115     C                   z-add     vi1dd2        an2               4 0
023500000203     C* CONTROLLO L'ANNO
023600000203     C                   Z-ADD     G02DAT        W0040             4 0
023700011115     c     an1           ifne      an2
023800011115     C                   SETON                                        56  90
023900011115     C                   GOTO      ENDCT1
024000011115     c                   end
024100000203     C     W0040         IFGT      ANNOPI
024200000203     C     W0040         ORLT      ANNOME
024300000203     C                   SETON                                        44  90
024400000203     C                   GOTO      ENDCT1
024500000203    1C                   END
024600931126     C* DATA AL NO < DATA DAL
024700931126    2C     W01DD2        IFLT      W01DD1
024800931126     C                   SETON                                        45  90
024900931126     C                   GOTO      ENDCT1
025000931126    2C                   ENDIF
025100991228     C**
025200991228    1C                   ENDIF
025300950131     C*
025400931116     C* P A D R O N C I N O
025500931116     C* RICERCA
025600931116     C     '?'           SCAN      VI1PD1                                 90
025700931116     C*
025800931126    1C     *IN90         IFEQ      *ON
025900021203     C                   Z-ADD     VI1PF1        D24FIL
026000931126     C                   EXSR      RICPAD
026100931126     C*
026200021203     C     D24PDR        IFNE      0
026300021203     C                   MOVEL     D24PDR        VI1PF1
026400931116     C                   MOVEL     *ZEROS        VI1PD1
026500021203     C                   MOVE      D24PDR        VI1PD1
026600021203     C                   MOVEL     D24RSC        VI1DP1
026700950125     C                   ENDIF
026800931116     C                   GOTO      ENDCT1
026900931126    1C                   ENDIF
027000931126     C* PADRONCINO AL
027100931126     C* RICERCA
027200931126     C     '?'           SCAN      VI1PD2                                 90
027300931126     C*
027400931126    1C     *IN90         IFEQ      *ON
027500021203     C                   Z-ADD     VI1PF2        D24FIL
027600931126     C                   EXSR      RICPAD
027700931126     C*
027800021203     C     D24PDR        IFNE      0
027900021203     C                   MOVEL     D24PDR        VI1PF2
028000931126     C                   MOVEL     *ZEROS        VI1PD2
028100021203     C                   MOVE      D24PDR        VI1PD2
028200950125     C                   ENDIF
028300931126     C                   GOTO      ENDCT1
028400931126    1C                   ENDIF
028500931126     C*
028600931126     C*
028700931126    1C     VI1PD2        IFNE      *BLANKS
028800931126     C     VI1PD2        ANDNE     *ZEROS
028900931126     C                   TESTN                   VI1PD2               30
029000931126     C  N30              SETON                                        4790
029100931126     C                   ENDIF
029200931116     C*
029300931126    1C     VI1PD1        IFNE      *BLANKS
029400931126     C     VI1PD1        ANDNE     *ZEROS
029500931126     C                   TESTN                   VI1PD1               30
029600931126     C  N30              SETON                                        4190
029700931116     C*
029800931123     C* SE NE HO IMMESSO SOLTANTO UNO CONTROLLO L'ESISTENZ
029900931126    2C     VI1PD2        IFEQ      *BLANKS
030000931126     C     VI1PD2        OREQ      *ZEROS
030100931126     C*
030200021203     C     KAPDV         CHAIN     FIAPD000                           30
030300931015     C*
030400931126    3C     *IN30         IFEQ      *ON
030500020520     C* Tolto il controllo sul padroncino annullato
030600020520     C                   SETON                                        4190
030700931028     C                   GOTO      ENDCT1
030800931126    3C                   ENDIF
030900931129     C*
031000090507     C                   MOVEL     APDRSf        VI1DP1
031100931123     C*
031200931123   X2C                   ELSE
031300931126     C*
031400931123     C* PADRONCINO AL  <  MINORE PADRONCINO DAL
031500950125    3C     VI1P2         IFLT      VI1P1
031600931123     C                   SETON                                        4290
031700931123     C                   GOTO      ENDCT1
031800931126    3C                   ENDIF
031900931126    2C                   ENDIF
032000931123    1C                   ENDIF
032100010906     C*
032200010906     C* PADRONCINO AL E PADRONCINO DAL A ZERO
032300010906     C     VI1PD1        IFEQ      *ZEROS
032400010906     C     VI1PD2        ANDEQ     *ZEROS
032500010906     C                   SETON                                        4090
032600010906     C                   GOTO      ENDCT1
032700010906     C                   ENDIF
032800031114     c                   if        vi1pf1 = 0  and v1cdf <> *blank
032900031114     c                   movel     v1cdf         cod4              4 0
033000031114     c                   move      cod4          vi1pf1
033100031114     c                   move      cod4          vi1pf2
033200031114     c                   endif
033300910320     C*
033400900614     C     ENDCT1        ENDSR
033500931126     C*
033600931126     C* RICERCA CODICE PADRONCINO ------------------------------------*
033700931126     C     RICPAD        BEGSR
033800021203     C                   MOVEL     *BLANKS       D24RSC
033900021203     C                   Z-ADD     0             D24PDR
034000021203     C                   MOVEL     'R'           D24FLG
034100031113     C                   MOVEL     'D'           D24TIP
034200021203     C                   MOVEL(P)  FNLV24DS      KPJBU
034300950125     C                   CALL      'FNLV24R'
034400931126     C                   PARM                    KPJBA
034500021203     C                   MOVEL     KPJBU         FNLV24DS
034600931126     C                   ENDSR
