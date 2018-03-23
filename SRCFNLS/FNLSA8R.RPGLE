000100020422     H DECEDIT('0,') DATEDIT(*YMD.)
000200020418      * FNLSA8R *----------------------------------------------------*
000300020418      *        - AGGIORNAMENTI PER CANCELLAZIONE SPUNTE              *
000400020418      *--------------------------------------------------------------*
000500041018     FFIar501l  IF   E           K DISK
000600041015     Ffnfgv01l  IF   E           K DISK
000700041015     Ffnfgw01l  IF   E           K DISK
000800041015     FTABEL00F  IF   E           K DISK
000900050401     FTigcp21L  IF   E           K DISK
001000000825     FAZORG01L  IF   E           K DISK
001100990409     FFNDCD02L  UF   E           K DISK
001200060512     FFNFVV01L  IF   E           K DISK
001300060512     FFNFVV02L  IF   E           K DISK
001400960417     F                                     RENAME(FNFVV000:FNFVV2)
001500990518     FFNANM02L  UF A E           K DISK
001600150819     F                                     INFDS(FNANM2)
001700070124     FFNBRV07L  IF   E           K DISK
001800140618     FFNBRV05L  IF   E           K DISK
001900140618     F                                     RENAME(FNBRV000:FNBRV5)
002000960614     FFNAGB01L  IF A E           K DISK
002100941206     F* BOLLE ARRIVI ----------------------
002200941206     FFNART27L  UF   E           K DISK
002300941206     FFNART01L  IF   E           K DISK
002400941206     F                                     RENAME(FNART000:FNART001)
002500941206     FFNARB01L  UF   E           K DISK
002600941206     F* BOLLE TRANSITO --------------------
002700031114     FFNBTT37L  UF   E           K DISK
002800031114     FFNBTT11L  IF   E           K DISK
002900031114     F                                     RENAME(FNBTT000:FNBTT011)
003000031114     FFNBTP11L  UF   E           K DISK
003100941206     F* BOLLE PARTENZE --------------------
003200941206     FFNBLT27L  UF   E           K DISK
003300941206     FFNBLT01L  IF   E           K DISK
003400941206     F                                     RENAME(FNBLT000:FNBLT001)
003500941206     FFNBLP01L  UF   E           K DISK
003600030109     fFidta01l  uf a e           K Disk
003700050408     FFISGN03L  UF   E           K DISK
003800911014     D*
003900921020     D §SC             S              1    DIM(5)
004000921020     D §FS             S              1    DIM(5)
004100960417     D L6S             S              3  0 DIM(30)
004200921019     D PARAM           DS
004300941229     D  PARNFV                 2      6  0
004400941229     D  PARNPG                 7      7  0
004500921020     D  LNP                    8     10  0
004600921020     D  LNA                   11     13  0
004700921020     D  NRS                   14     15  0
004800921020     D  NSC                   16     22  0
004900921019     D  CAN                   23     23
005000941212     D  VLS                   24     30  6
005100941212     D  FLG                   31     31
005200941212     D  SIMFEL                92     94  0
005300941212     D  NPS                   95     96  0
005400941212     D  PARFGS                97     99  0
005500970530     D  PARFLF               100    102  0
005600031114     D  BPES                 103    105  0
005700000825     D DS7N          E DS
005800960416     D DS7C          E DS
005900970530     D DS7J          E DS
006000960510     D DS7A2         E DS
006100911016     D KPJBA         E DS
006200960416     D DSLR33        E DS                  EXTNAME(FNLR33DS)
006300970530     D* CALL A PGM CHE CARICA I DATI FOGLI  - FNLV53R
006400970530     D DSLV53        E DS                  EXTNAME(FNLV53DS)
006500970530     D* CALL A PGM CHE CERCA IL TERMINAL DI PARTENZA ARRIVO  - FNLV55R
006600970530     D DSLV55        E DS                  EXTNAME(FNLV55DS)
006700030320     d ds3a          e ds
006800041018     D DAR5GEN       E DS
006900161021     d fnlgi1ds      e ds
007000161021     D tigcp00ds     E DS                  extname(tigcp00f)
007100161021     d
007200990518     D FNANM2          DS
007300960416     D  ANMNR2               397    400B 0
007400960416     D WLBDA8          DS
007500960416     D  G08DAT                 1      8  0
007600960416     D  G08INV                 9     16  0
007700960416     D  G08ERR                17     17
007800960416     D  G08TGI                18     22  0
007900961120     D WGIDAT          DS                  INZ
008000961120     D  GIODAT                 1      8  0
008100961120     D  GIOINV                 9     16  0
008200961120     D  GIOTGI                17     21  0
008300041015     d
008400041015     D  FFV                    1    897
008500041015     D                                     DIM(299)
008600041015    +D  FGVFFV                 1    240
008700041015    +D  FGVFF2               241    447
008800041015    +D  fgwff3               448    687
008900041015    +D  fgwff4               688    897
009000041015     D                 DS
009100041015    +D  FLP                    1    897
009200041015     D                                     DIM(299)
009300041015    +D  FGVFLP                 1    240
009400041015    +D  FGVFL2               241    447
009500041015    +D  fgwfl3               448    687
009600041015    +D  fgwfl4               688    897
009700960417     D DSUL06        E DS                  EXTNAME(TRUL06DS)
009800960417     D  LIN                    1     90  0
009900960417     D                                     DIM(30)
010000960417     D DSFASE          DS
010100960417     D  D11TLA                 1      1
010200960417     D  D11NPG                 2      2
010300960417     D  D11SPG                 3      3
010400960417     D  D11FAS                 4      4
010500031114     D                 DS
010600031114     D  BLPAAS                 1      4  0
010700031114     D  BLPMGS                 5      8  0
010800031114     D  BLPDSP                 1      8  0
010900031114     D                 DS
011000031114     D  ARBAAS                 1      4  0
011100031114     D  ARBMGS                 5      8  0
011200031114     D  ARBDSP                 1      8  0
011300031114     D                 DS
011400031114     D  BTPAAS                 1      4  0
011500031114     D  BTPMGS                 5      8  0
011600031114     D  BTPDSP                 1      8  0
011700041014     D                 DS
011800041014     D  brvdcs
011900041014     D  brvhcs
012000070124     D  brvscar                1     14  0
012100041015     D                 DS
012200041015     D  brvdfs
012300041015     D  brvhms
012400070124     D  brvimm                 1     14  0
012500020418
012600041018     D ktrd            s                   like(ar5trd)  inz('GEN')
012700060512     d Knps            s                   like(brvnps)
012800030109
012900030109     d SavAas          s                   Like(DtaAas)
013000030109     d SavLnp1         s                   Like(DtaLnp)
013100030109     d SavNrs          s                   Like(DtaNrs)
013200030109     d SavNsp          s                   Like(DtaNsp)
013300030109     d SavPoc          s                   Like(DtaPoc)
013400030109     d SavTrd          s                   Like(DtaTrd)
013500030115     d Sav_ArbDam      s                   Like(ArbDam)
013600030115     d Sav_BtpDet      s                   Like(BtpDet)
013700030109     d wFidta          s              1    Inz('0')
013800031127     d wnfaf           s                   like(nfaf)
013900041015     d tradfv          s                   like(d53dfv)
014000041015     d tranps          s                   like(brvnps)
014100041015     d tranpg          s                   like(brvnpg)
014200041015     d trafgs          s                   like(brvfgs)
014300041015     d prtdfv          s                   like(d53dfv)
014400041015     d prtnps          s                   like(brvnps)
014500041015     d prtnpg          s                   like(brvnpg)
014600041015     d prtfgs          s                   like(brvfgs)
014700041015     d arrdfv          s                   like(d53dfv)
014800050509     d arrfl2          s                   like(artfl2)
014900041015     d arrnps          s                   like(brvnps)
015000041015     d arrnpg          s                   like(brvnpg)
015100041015     d arrfgs          s                   like(brvfgs)
015200041015     d dis1dfv         s                   like(d53dfv)
015300041015     d dis1nps         s                   like(brvnps)
015400041015     d dis1nfv         s                   like(d53nfv)
015500041015     d dis1fgs         s                   like(brvfgs)
015600041015     d dis1scar        s                   like(brvscar)
015700041015     d dis1atr         s                   like(brvatr)
015800041015     d dis2dfv         s                   like(d53dfv)
015900041015     d dis2nps         s                   like(brvnps)
016000041015     d dis2nfv         s                   like(d53nfv)
016100041015     d dis2fgs         s                   like(brvfgs)
016200041015     d dis2scar        s                   like(brvscar)
016300041015     d danfgs          s                   like(brvfgs)
016400041015     d dandfv          s                   like(d53dfv)
016500041015     d dannps          s                   like(brvnps)
016600041018     d dannpg          s                   like(brvnpg)
016700041018     d danspg          s                   like(fvvspg)
016800041015     d wnoarrivo       s              1    Inz(' ')
016900041018     d wokanm15        s              1    Inz(' ')
017000060512     d abbnsc          s                   like(artnsc)
017100060512     d abbdfv          s                   like(artdfv)
017200110908     d A_115dfv        s                   like(d53dfv)
017300110908     d A_115fgs        s                   like(brvfgs)
017400110908     d A_115npg        s                   like(brvnpg)
017500110908     d A_115spg        s                   like(fvvspg)
017600110908
017700070424     d DSRECANM      e DS                  extname(fnanm00F)
017800070424     d savRECANM     e DS                  extname(fnanm00F) prefix(S_)
017900030109
018000000000     C*---------------------------------------------------------------*
018100921014     C* RIEPILOGO INDICATORI
018200921014     C*---------------------------------------------------------------*
018300921014     C* 01    - LANCIO PGM ABILITAZIONE MERCE
018400921019     C* 06    - ELIMINO ANOMALIA
018500921019     C* 09    - ELIMINO DATA ENTRATA O DATA ARRIVO
018600041014     C* 16    - ESISTE altra spunta per arrivo
018700041014     C* 17    - ESISTE altra spunta per partenza
018800041014     C* 18    - ESISTE altra spunta per transito
018900041014     C* 19    - ESISTE IN ARCHIVIO UN'ALTRA SPUNTA DEL COLLO
019000041014     C* 20    -
019100921014     C* 32/34 - DI COMODO
019200921014     C* 37    - NON ESISTE BOLLA TRANSITO
019300921014     C* 39    - RECORD ALLOCATO
019400000825     C* 43    - CONTROLLO SU AZORG SE LINEA PARTENZA E' POSTE E SE
019500000825      *         POSSO CREARE ANOMALIE
019600921014     C*---------------------------------------------------------------*
019700020723     C     *ENTRY        PLIST
019800020723     C                   PARM                    KPJBA
019900020723     C                   MOVEL     KPJBU         PARAM
020000020723     c
020100020723     C***
020200020723     C* CARICO TUTTE LE  FILIALI GESTITE DA ALTRE (DA £6)
020300020723     c*   solo una volta
020400020723     C***
020500020723     c                   if        not *in95
020600020723     C                   CLEAR                   DSUL06
020700020723     C                   MOVE      '£6'          D06COD
020800020723     c                   eval      d06esc = 'G'
020900020723     c                   movel     simfel        d06key
021000020723     C                   MOVEL     DSUL06        KPJBU
021100020723     C                   CALL      'TRUL06R'
021200020723     C                   PARM                    KPJBA
021300020723     C                   MOVEL     KPJBU         DSUL06
021400020723     C                   MOVEA     LIN           L6S
021500020723     c                   seton                                        95
021600020723     c                   endif
021700900618     C*---------------------------------------------------------------*
021800921020     C* FLG = 1 PASSO IL VECCHIO RECORD DA ELIMINARE
021900921020     C* FLG = 3 ELIMINAZ.SOLO DEL CODICE ANOMALIA (NON DELETO ANOMALIE)
022000930609     C* FLG = 'C' CHIUDO PGM CON LR E BASTA
022100900619     C*
022200930609     C     FLG           IFEQ      'C'
022300970530     C                   MOVEL     'C'           D55TLA
022400970530     C                   CALL      'FNLV55R'
022500970530     C                   PARM                    DSLV55
022600970530     C                   MOVEL     'C'           D53TLA
022700970530     C                   CALL      'FNLV53R'
022800970530     C                   PARM                    DSLV53
022900970604     C                   MOVEL     'C'           SA2TLA
023000970604     C                   CALL      'FNLSA2R3'
023100970604     C                   PARM                    SA2TLA
023200970604     C                   PARM                    SA2TRC
023300970604     C                   PARM                    SA2DTS
023400970604     C                   PARM                    SA2LNA
023500161021
023600161021     c                   clear                   fnlgi1ds
023700161021     c                   eval      ilgi1tla = 'C'
023800161021     c                   movel     fnlgi1ds      kpjbu
023900161021     c                   call      'FNLGI1R'
024000161021     c                   parm                    kpjba
024100970530     C**
024200930609     C                   SETON                                        LR
024300930609     C*
024400930609     C                   ELSE
024500930609     C*
024600911016     C* IMPOSTAZIONI INIZIALI
024700911014     C                   EXSR      CARTAB
024800921019     C*
024900921020     C                   Z-ADD     1             B                 1 0
025000920424     C*
025100920424     C* SE TROVO RECORD ALLOCATI RITENTO 3 VOLTE
025200920424     C     B             DOWLE     3
025300031114     C                   EXSR      UPDbol
025400920424     C                   ADD       1             B
025500920424     C                   END
025600921019     C*
025700921019     C     B             IFEQ      3
025800921019     C                   MOVEL     'E'           FLG
025900921019     C                   END
026000911014     C*
026100921020     C                   MOVEL     PARAM         KPJBU
026200930609     C*
026300930609     C                   RETURN
026400930609     C                   END
026500941206     C**************************************************************************
026600941206     C*    CARICO TABELLE
026700941206     C**************************************************************************
026800911014     C     CARTAB        BEGSR
026900970530     C**
027000970529     C                   CLEAR                   DSLV53
027100970529     C                   MOVEL     PARNPG        D53NPG
027200970529     C                   MOVEL     PARNFV        D53NFV
027300970529     C                   MOVEL     PARFGS        D53FGS
027400970530     C                   MOVEL     PARFLF        D53FLF
027500970529     C                   MOVEL     SIMFEL        D53FEL
027600970529     C                   CALL      'FNLV53R'
027700970529     C                   PARM                    DSLV53
027800970529     C     D53ERR        IFEQ      ' '
027900970529     C                   MOVEL     D53SPG        WVVSPG
028000970529     C                   MOVE      D53DFV        WVVDFV
028100031114     C                   MOVE      D53DEF        WVVDEF
028200970529     C                   ELSE
028300970529     C                   MOVE      DATEU         WVVDFV
028400970616     C                   CLEAR                   WVVSPG
028500031114     C                   CLEAR                   WVVDEF
028600961120    1C                   END
028700911112     C*
028800921020     C* FASE DEL FOGLIO DELLA SPUNTA CHE ELABORO
028900960405     C                   MOVEL(P)  PARNPG        KEY
029000960405     C                   MOVEL     '7N'          COD
029100921019     C     KTAB          CHAIN     TABEL                              31
029200921019     C  N31              MOVEL     TBLUNI        DS7N
029300921021     C*
029400921021     C                   MOVEL     §7NFS1        §FS(1)
029500921021     C                   MOVEL     §7NFS2        §FS(2)
029600921021     C                   MOVEL     §7NFS3        §FS(3)
029700921021     C                   MOVEL     §7NFS4        §FS(4)
029800921021     C                   MOVEL     §7NFS5        §FS(5)
029900921021     C*
030000921021     C                   MOVEL     §7NSC1        §SC(1)
030100921021     C                   MOVEL     §7NSC2        §SC(2)
030200921021     C                   MOVEL     §7NSC3        §SC(3)
030300921021     C                   MOVEL     §7NSC4        §SC(4)
030400921021     C                   MOVEL     §7NSC5        §SC(5)
030500921020     C*
030600930604     C                   Z-ADD     1             I                 3 0
030700970616     C     WVVSPG        LOOKUP    §SC(I)                                 31
030800921020     C   31              MOVEL     §FS(I)        FASE              1
030900921020     C  N31              MOVEL     §FS(1)        FASE
031000060512     c*
031100060512     c* Vedo se la pistola è manuale
031200060512     c
031300060512     c                   MOVEL     nps           knps
031400060512     c                   exsr      TIPPIS
031500060512     c
031600060512     c                   movel     §7jrps        spurps            1
031700060512     c*
031800060512     c
031900921019     C*
032000911014     C                   ENDSR
032100941206     C**************************************************************************
032200911018     C*--- AGGIORNO RECORD BOLLE ARRIVI O CREO ANOMALIA --------------*
032300921020     C* ANNULLO DATA ARRIVO SULLE BOLLE SE:
032400921020     C*  1) NON CI SOLO ALTRE SPUNTE DEL COLLO
032500921020     C*  2) SE LA DATA ARRIVO CORRISPONDE ALLA DATA FOGLIO DELLA SPUNTA
032600921021     C*
032700921021     C* MODIFICO DATA DI ARRIVO SULLE BOLLE E COLLO :
032800921021     C*  1) SE ANNULLO LA SPUNTA CHE MI HA IMPOSTATO LA DATA DI ARRIVO
032900921021     C*     E CI METTO LA DATA FOGLIO DI UN'ALTRA SPUNTA DI QUEL COLLO
033000921020     C*
033100921020     C* ANNULLO ANOMALIA    SULLE BOLLE SE:
033200921020     C*  1) NON C'E' NESSUN'ALTRA SPUNTA CON UNA ANOMALIA
033300921020     C*  2) SE L'ANOMALIA SPUNTA E' = ALL'ANOMALIA SUL COLLO
033400941206     C**************************************************************************
033500031114     C     UPDBOL        BEGSR
033600941230     C*
033700070424     c                   clear                   AggioDTI          1
033800921019     C                   MOVEL     ' '           SAVCAN
033900941206     C                   MOVEL     *ZEROS        SPUDFV
034000941206     C                   MOVEL     *ZEROS        SPUNPS
034100960416     C                   MOVEL     *ZEROS        SPUNPG
034200960417     C                   MOVEL     *ZEROS        SPUSPG
034300960416     C                   MOVEL     *ZEROS        SPUNFV
034400960513     C                   MOVEL     *ZEROS        SPUFGS
034500961121     C                   MOVEL     *ZEROS        ENTDFV
034600961121     C                   MOVEL     *ZEROS        ENTNPS
034700990409     C                   MOVEL     *ZEROS        ENTFGS
034800990412     C                   MOVEL     *ZEROS        A56DFV
034900990412     C                   MOVEL     *ZEROS        A56NPS
035000990412     C                   MOVEL     *ZEROS        A56FGS
035100990412     C                   MOVEL     *ZEROS        A56NFV
035200990412     C                   MOVEL     *BLANKS       A56SPG
035300990412     C                   MOVEL     *BLANKS       A56NPG
035400960416     C                   CLEAR                   SAVCAA
035500960416     C                   CLEAR                   WAAS
035600960416     C                   CLEAR                   WLNP
035700960416     C                   CLEAR                   WNSP
035800960417     C                   CLEAR                   WDAM
035900960417     C                   CLEAR                   WDCM
036000961120     C                   CLEAR                   WDBR
036100961120     C                   CLEAR                   WFBC
036200101202     c*
036300960613     C                   CLEAR                   LNPB
036400031114     C                   CLEAR                   WESART
036500031114     C                   CLEAR                   WESBLT
036600031114     C                   CLEAR                   WESBTT
036700041015     C                   MOVEL     *ZEROS        tradfv
036800041015     C                   MOVEL     *ZEROS        tranps
036900041015     C                   MOVEL     *ZEROS        tranpg
037000041015     C                   MOVEL     *ZEROS        trafgs
037100041015     C                   MOVEL     *ZEROS        prtdfv
037200041015     C                   MOVEL     *ZEROS        prtnps
037300041015     C                   MOVEL     *ZEROS        prtnpg
037400041015     C                   MOVEL     *ZEROS        prtfgs
037500041015     C                   MOVEL     *ZEROS        arrdfv
037600050509     c                   clear                   arrfl2
037700041015     C                   MOVEL     *ZEROS        arrnps
037800041015     C                   MOVEL     *ZEROS        arrnpg
037900041015     C                   MOVEL     *ZEROS        arrfgs
038000041015     C                   MOVEL     *ZEROS        dis1nps
038100041015     C                   MOVEL     *ZEROS        dis1nfv
038200041015     C                   MOVEL     *ZEROS        dis1fgs
038300041015     C                   MOVEL     *ZEROS        dis1dfv
038400041015     C                   MOVEL     *ZEROS        dis1scar
038500041015     C                   MOVEL     *ZEROS        dis1atr
038600041015     C                   MOVEL     *ZEROS        dis2nps
038700041015     C                   MOVEL     *ZEROS        dis2nfv
038800041015     c                   MOVEL     *ZEROS        dis2fgs
038900041015     C                   MOVEL     *ZEROS        dis2dfv
039000041015     C                   MOVEL     *ZEROS        dis2scar
039100041015     C                   MOVEL     *ZEROS        dannps
039200041015     C                   MOVEL     *ZEROS        dandfv
039300041015     C                   MOVEL     *ZEROS        danfgs
039400041018     C                   MOVEL     *ZEROS        dannpg
039500041018     C                   MOVEL     '  '          danspg
039600110908     c
039700110908     c                   clear                   a_115dfv
039800110908     C                   MOVEL     *ZEROS        a_115fgs
039900110908     C                   MOVEL     *ZEROS        a_115npg
040000110908     C                   MOVEL     '  '          a_115spg
040100930604     C                   SETOFF                                       061920
040200041015     C                   SETOFF                                       161718
040300031114     c*
040400031114     C* TERMINAL partenza DI BPES
040500031114     C                   MOVEL     'P'           WTITER            1
040600031114     C                   MOVEL     wvvdfv        WDTRIF
040700031114     C                   EXSR      TERPES
040800031114     C                   Z-ADD     D55TFP        PESTFP
040900031114     c*
041000031114     c* Prima di tutto verifico chi sono come spunta e quindi cosa posso
041100031114     c*  aggiornare ...
041200031114     c*
041300031126     c* Se trovo la bolla transito la considero sempre buona e non
041400031114     c*  controllo i terminal (c'e' un problema a livello di eccezioni ter
041500031114     C     KBAR37        CHAIN     FNBTT37L                           3739
041600031114     C   39              GOTO      ENDUPD
041700031114     C  N37KBTT          CHAIN     FNBTP11L                           37
041800031114     C**
041900031114    1C     *IN37         IFEQ      *OFF
042000031114     C                   MOVEL     'S'           WESBTT            1
042100031114     C                   MOVEL     BTTLNP        LNPB
042200031114     C                   MOVEL     BTPDSP        DSPB
042300031114     C                   MOVEL     BTPDSP        WDTRIF
042400031114     C                   MOVEL     BTPTFP        LNPTFP
042500031114   X1C                   ELSE
042600031114     C                   MOVEL     'N'           WESBTT
042700031114     C**
042800031114     C** VEDO SE TROVO LA BOLLA IN BLT
042900041216     C     KBOL2         CHAIN(N)  FNBLT27L                           3239
043000031114     C   39              GOTO      ENDUPD
043100031114     C  N32KBLT          CHAIN(N)  FNBLP01L                           32
043200031114     C**
043300031114    2C     *IN32         IFEQ      *OFF
043400031114     C                   MOVEL     'S'           WESBLT            1
043500031114     C                   MOVEL     BLTLNP        LNPB
043600031114     C                   MOVEL     BLPTFP        LNPTFP
043700031114     C                   MOVEL     BLPDSP        WDTRIF
043800031114     C                   MOVEL     BLPDSP        DSPB
043900031114   X2C                   ELSE
044000031114     C                   MOVEL     'N'           WESBLT
044100031114     C** SE NON TROVO LA BOLLA PARTENZA CERCO NEGLI ARRIVI
044200031114     C     KBOL2         CHAIN     FNART27L                           3239
044300031114     C   39              GOTO      ENDUPD
044400031114     C  N32KARB          CHAIN     FNARB01L                           32
044500031114     C**
044600031114    3C     *IN32         IFEQ      *OFF
044700031114     C                   MOVEL     'S'           WESART            1
044800031114     C                   MOVEL     ARTLNP        LNPB
044900031114     C                   MOVEL     ARBDSP        DSPB
045000031114     C                   MOVEL     ARBDSP        WDTRIF
045100031114     C                   MOVEL     ARBTFP        LNPTFP
045200031114   X3C                   ELSE
045300031114     C                   MOVEL     'N'           WESART
045400031114     C** IMPOSTO LNPB = BARLNP
045500031114     C                   MOVEL     lnp           LNPB
045600031114     C                   Z-ADD     wvvdfv        DSPB
045700031114     C**
045800150819     C** SE NON TROVATA LA BOLLA, CERCO SE ESISTE L' EVENTUALE
045900031126     C*   SPUNTA PARTENZA
046000031114     C                   EXSR      RICLNP
046100150819     c
046200031114     c* prendo terminal di partenza della linea partenza calcolata
046300031114     C                   Z-ADD     wvvdfv        WDTRIF
046400031114     C                   EXSR      TERPAR
046500031114    3C                   ENDIF
046600031114    2C                   ENDIF
046700031114    1C                   ENDIF
046800031114     C**
046900031114     C** CONTROLLO TRA P.O. SPUNTA E LINEE SPUNTE CHE AGGIONAMENTI
047000031114     C**  POSSO FARE
047100031114     c*
047200150819     C* TERMINAL arrivo   DI BPES
047300031114     C                   MOVEL     'A'           WTITER
047400031114     C                   MOVEL     wvvdfv        WDTRIF
047500031114     C                   EXSR      TERPES
047600031114     C                   Z-ADD     D55TFA        PESTFA
047700031114     c
047800031114     C                   EXSR      CTRAGG
047900960416     C*
048000960416     C                   CLEAR                   WCHIUS
048100960416     C                   CLEAR                   WDFV
048200960416     C                   CLEAR                   WNPS
048300960416     C                   CLEAR                   WNFV
048400960416     C                   CLEAR                   WNPG
048500960417     C                   CLEAR                   WSPG
048600960416     C                   CLEAR                   WFGS
048700961120     C                   CLEAR                   WCDU
048800960416     C                   CLEAR                   WSPUN
048900031114     C                   CLEAR                   WLOCAL
049000960417     C                   CLEAR                   SAVDFV
049100960417     C                   CLEAR                   SAVLNP
049200970127     C                   CLEAR                   WGIAC
049300930604     C**
049400070124     C     KBOL2         SETLL     FNBRV07L
049500070124     C     KBOL2         READE     FNBRV07L                               32
049600930604     C*
049700970625    1C     *IN32         DOWEQ     '0'
049800961120     C* LA SPUNTA LETTA NON DEVE ESSERE QUELLA CHE STO ANNULLANDO
049900970625    2C     BRVNPG        IFNE      PARNPG
050000950118     C     BRVNFV        ORNE      PARNFV
050100950118     C     BRVFGS        ORNE      PARFGS
050200961120     C**
050300041013     C* CODICE ANOMALIA COLLO  tengo l'ultima che trovo
050400041013    3C     brvCAN        IFne      ' '
050500950118     C                   MOVEL     BRVCAN        SAVCAN            1
050600961118    3C                   END
050700970625     C**
050800970625     C* SOLO SE FLAG ='1' FACCIO GLI ALTRI CONTROLLI RELATIVI ALLE
050900970625     C* SPUNTE
051000970625    3C     FLG           IFEQ      '1'
051100961120     C**
051200961120     C* CHAINO I FOGLI A SECONDA DELLA CATEGORIA E FIL.GESTIONE
051300961120     C                   EXSR      CHAFV
051400970530     C**
051500970530     C* CONTROLLO IL TIPO PISTOLA DELLA SPUNTA LETTA
051600060512     c                   movel     brvnps        knps
051700970530     C                   EXSR      TIPPIS
051800110908     c
051900110908     c* Verifico se presente altra spunta reale sul sistema per non
052000110908     c*  riaprire  eventuale anomalia 115
052100110908    4C     §7JRPS        IFNE      'A'
052200110908     c                   eval      a_115dfv=dataf
052300110908     C                   MOVE      BRVFGS        a_115FGS
052400110908     C                   MOVE      BRVnpg        a_115npg
052500110908     C                   MOVE      spgf          a_115spg
052600110908     c                   endif
052700110908     c
052800041015     c* Per spunta transito o disguido memorizzo la spunta con data/ora cari
052900041015     c*  più alta, per aggiornare altro eventuale disguido/transito
053000041015     c                   if        d53err=' '  or d53err='6'
053100041015    4c                   if        wesbtt='S' or
053200041015     c                             wagpar=' ' and wagarr=' '
053300041015     c* Categoria partenza
053400041015    5c                   if        brvnpg=1 and deff=' '
053500041015    6c                   if        dis1dfv=0 or dataf>=dis1dfv
053600041015     C                   MOVEL     BRVNPS        dis1nPS
053700041015     C                   MOVE      BRVNFV        dis1NFV
053800041015     C                   MOVE      BRVFGS        dis1FGS
053900041015     C                   MOVE      dataf         dis1dfv
054000041015     c                   if        brvscar>0
054100041015     C                   MOVE      brvscar       dis1scar
054200041015     c                   else
054300041015     c                   move      brvimm        dis1scar
054400041015     c                   endif
054500041015     C                   MOVE      brvatr        dis1atr
054600041015    6c                   endif
054700041015    5c                   endif
054800041015    4c                   endif
054900041015    4c                   endif
055000101202     c*
055100041015     C* CONSIDERO SOLO SE NON C'E' ERRORE
055200041015    4C     D53ERR        IFEQ      ' '
055300960606     C*
055400961120     C*  ESCLUDO LE SPUNTE AUTOMATICHE CHE NON POSSONO AGGIORNARE LA
055500961120     C*  DATA DI ARRIVO
055600970625    5C     §7JRPS        IFNE      'A'
055700970530     C* CONSIDERO COMUNQUE LA PISTOLA AUTOMATICA DEI TRANSITI PERCHE'
055800970530     C*  AGGIORNA LA DATA DI ARRIVO TRANSITO
055900970530     C     BRVNPS        OREQ      89
056000961120     C*
056100961120     C* DETERMINO TERMINAL DI ARRIVO DELLA FILIALE GESTIONE SPUNTA LETT
056200961120     C                   EXSR      TERFGS
056300961120     C*
056400031126     C* SE DEVO AGGIORNARE L'ARRIVO: CONSIDERO ANCHE LE SPUNTE DEL TERMINAL
056500031126     C*   ALTRIMENTI  CONSIDERO SOLO LE SPUNTE DI SIMFEL
056600031126    6C                   IF        BRVFLE=SIMFEL  OR
056700031126     C                             WAGARR='S' AND WTFG  =LNATFA
056800031126     c* FOGLI VARI
056900970625    7C     WFVV          IFEQ      'S'
057000961120     C*
057100041014     C* SE TROVATA SPUNTA vedo se memorizzare i dati
057200970530     C                   MOVEL     DATAF         SPUDFV
057300970530     C                   EXSR      MEMDAT
057400990412     C**
057500990412     C** MEMORIZZO SE C'E' UN'ALTRA SPUNTA IMA/IMG/CONSEGNE
057600990412     C** PER ANOMALIA DI DISGUIDO
057700990412    8C     BRVNPG        IFEQ      3
057800990412     C     NFASPG        ANDEQ     'A'
057900990412     C     BRVNPG        OREQ      3
058000990412     C     NFASPG        ANDEQ     'G'
058100161021     C     BRVNPG        OREQ      3
058200161021     C     NFASPG        ANDEQ     'Z'
058300990412     C     BRVNPG        OREQ      4
058400990412     C                   MOVEL     DATAF         A56DFV
058500990412     C                   MOVE      BRVNPS        A56NPS
058600990412     C                   MOVE      BRVFGS        A56FGS
058700990412     C                   MOVE      BRVNPG        A56NPG
058800990412     C                   MOVE      BRVNFV        A56NFV
058900990412     C                   MOVE      NFASPG        A56SPG
059000990412    8C                   ENDIF
059100041015     c*
059200041015     c                   else
059300041015     c* La spunta partenza la consdiero per le partenze e transiti
059400041015     c* solo se non è autogenerata
059500041015     c                   if        brvscar>0 and wagarr<>'S'
059600041015     C                   MOVEL     DATAF         SPUDFV
059700041015     C                   EXSR      MEMDAT
059800990412    7C                   ENDIF
059900041015    7C                   ENDIF
060000961120     C**
060100961120     C* FOGLI PARTENZA LOCALE
060200970530     C* CONSIDERO SOLO SE SI TRATTA DI  DEFLUENZA
060300970625    7C     WFGV          IFEQ      'S'
060400970530     C     DEFF          ANDEQ     'S'
060500970530     C* SE TROVATA SPUNTA MEMORIZZO ANCHE GLI ALTRI DATI
060600970530     C                   MOVEL     DATAF         SPUDFV
060700970530     C                   EXSR      MEMDAT
060800970625    7C                   ENDIF
060900961120     C**
061000970625    6C                   END
061100970625    5C                   END
061200960417     C*
061300960417     C* SE SPUNTA PARTENZA MEMORIZZO QUELLA CON DATA PIù RECENTE
061400960417     C* E VERIFICO SE RELATIVA A FV ABBINATO A FOGLIO ARRIVI GIà CHIUSO
061500961120     C*  PERCHE' DEVO CREARE L'ANOMALIA 05-COLLO PARTITO NON ARRIVATO
061600041014    5C     *IN16         IFEQ      *OFF
061700031126     c     wagarr        andeq     'S'
061800961120     C     BRVNPG        ANDEQ     1
061900970530     C     DEFF          ANDNE     'S'
062000961118     C**
062100970625    6C     WFVA          IFEQ      'S'
062200970530     C     DATAF         ANDGT     SAVDFV
062300031127     c* rimemorizzo anche se trovo il foglio gisuto abbinato,
062400031127     C*  In caso di transito infatti su As unico leggo anche la spunta part
062500031127     c*  originale non diretta all'arrivo. Il foglio lo trova ma mi da non
062600031127     c*  abbinato (xchè abbinato al transito e non all'arrivo)
062700031127     c*  quindi se poi leggo il foglio giusto lo devo memorizzare
062800031127     c*  Se troviamo soluzione migliore...meglo!!
062900031127    6C     WFVA          oreq      'S'
063000031127     C     DATAF         ANDle     SAVDFV
063100031127     c     wnfaf         andeq     0
063200031127     c     nfaf          andgt     0
063300970530     C                   MOVE      DATAF         SAVDFV
063400000904     C                   MOVE      BRVFGS        SAVLNP
063500960417     C                   MOVE      BRVNPS        WNPS
063600961120     C                   MOVEL     ' '           WLOCAL            1
063700031127     C                   z-add     nfaf          Wnfaf
063800961120     C**
063900970530     C* SE FVP ABBINATO ED HO TROVATO FOGLIO ARRIVI, MEMORIZZO  I DATI
064000970625    7C     NFAF          IFGT      0
064100970530     C     NFAFCF        ANDNE     *BLANKS
064200960417     C                   MOVE      'S'           WCHIUS
064300970530     C                   MOVE      BRVNFV        WNFV
064400970530     C                   MOVE      BRVFGS        WCDU
064500970530     C                   MOVE      NFADFV        WDFV
064600970530     C                   MOVE      NFANPG        WNPG
064700970530     C                   MOVE      NFASPG        WSPG
064800970530     C                   MOVE      NFAFGS        WFGS
064900970625   X7C                   ELSE
065000960417     C                   CLEAR                   WCHIUS
065100970625    7C                   END
065200970625    6C                   END
065300961118     C**
065400961118     C** FOGLIO LOCALE: COME SE FOSSE SEMPRE ABBINATO E F.ARRIVI CHIUSO
065500031114    6C     wagarr        IFEQ      'S'
065600970530     C     WFGV          ANDEQ     'S'
065700970530     C     DATAF         ANDGT     SAVDFV
065800970530     C                   MOVE      DATAF         SAVDFV
065900970530     C                   MOVE      BRVFGS        SAVLNP
066000961120     C                   MOVE      BRVNPS        WNPS
066100970530     C                   MOVEL     'S'           WLOCAL            1
066200970530     C*
066300970530     C                   MOVE      'S'           WCHIUS
066400970530     C                   MOVE      BRVNFV        WNFV
066500961120     C                   MOVE      SIMFEL        WCDU
066600961120     C**
066700970625    6C                   ENDIF
066800970625    5C                   ENDIF
066900961118     C**
067000961118     C** CONTROLLO LE SPUNTE AI FINI IMA:
067100961118     C**  -SE TROVO UNA SPUNTA CON DATA> DELLA DATA SPUNTA CHE CANCELLO
067200961118     C**   SICURAMENTE NON DEVO CREARE ANOMALIA COLLO MANCANTE
067300961120     C**  -SE TROVO UNA SPUNTA CON DATA = ALLA DATA SPUNTA CHE CANCELLO
067400961118     C**   CONSIDERO VALIDE SOLO LE CATEGORIA 3  4 E 5
067500961118     C**  - ESCUDO SEMPRE LE SPUNTE CATEGORIA 1 PERCHE' NON SONO CONSID
067600961118     C**    NEMMENO DAL PGM DELL'IMA
067700031114    5C     wagarr        IFEQ      'S'
067800961120     C     WFVV          ANDEQ     'S'
067900961118     C*
068000970625    6C     DATAF         IFGT      WVVDFV
068100960417     C                   MOVE      '1'           WSPUN             1
068200970625    6C                   ENDIF
068300961118     C*
068400970625    6C     DATAF         IFEQ      WVVDFV
068500970625    7C     BRVNPG        IFEQ      3
068600961118     C     BRVNPG        OREQ      4
068700961118     C     BRVNPG        OREQ      5
068800041013     C     BRVNPG        OREQ      8
068900961118     C                   MOVE      '1'           WSPUN             1
069000970625    7C                   ENDIF
069100970625    6C                   ENDIF
069200961118     C*
069300970625    5C                   END
069400970625    4C                   END
069500970625    3C                   END
069600970625    2C                   END
069700930604     C*
069800070124     C     KBOL2         READE     FNBRV07L                               32
069900970625    1C                   ENDDO
070000031114     C**
070100031114     C** A G G I O R N A M E N T O   B O L L E   P A R T E N Z A
070200031114     C**
070300031114     c** elimino data su collo
070400031114    1C     wagpar        IFEQ      'S'
070500941230     C     KBOL2         CHAIN     FNBLT27L                           3239
070600990409     C*
070700031114    2C     *IN39         IFEQ      *OFF
070800990409     C     KBOL2         CHAIN     FNDCD02L                           3839
070900060512     c* La pistola è corrispondente se uguali, o entrambe manuali
071000060512   2ac                   if        not *in38 and not *in39
071100060512     c                   movel     dcdnps        knps
071200060512     c                   exsr      TIPPIS
071300101202     c*
071400060512     C     DCDNPS        IFEQ      NPS
071500990409     C     DCDDFV        ANDEQ     WVVDFV
071600060512     C     spurps        oreq      'M'
071700060512     C     §7jrps        andeq     spurps
071800990409     C                   MOVEL     'S'           WDCD              1
071900990409     C                   ELSE
072000990409     C                   UNLOCK    FNDCD02L
072100031114    3C                   ENDIF
072200060512   2aC                   ENDIF
072300031114    2C                   ENDIF
072400921013     C*
072500921013     C   39              GOTO      ENDUPD
072600101202     c*
072700031203     C* se c'e' la bolla aggiorno
072800031203   1ac                   if        not *in32
072900970530     C                   MOVE      BLTAAS        WAAS
073000970530     C                   MOVE      BLTLNP        WLNP
073100970530     C                   MOVE      BLTNSP        WNSP
073200920810     C*
073300970530    2C     SAVCAN        IFEQ      ' '
073400921020     C     CAN           ANDEQ     BLTCAN
073500921020     C                   MOVEL     ' '           BLTCAN
073600961121    2C                   END
073700921019     C*
073800101202     c*
073900970530     C* CANCELLO A SOSTITUISCO LA DATA ENTRATA SOLO SE CORRISPONDONO
074000970530     C*  PISTOLA E DATA
074100970530    2C     FLG           IFEQ      '1'
074200060512     c                   movel     bltnpe        knps
074300060512     c                   exsr      TIPPIS
074400060512   2aC     BLTNPE        ifeq      NPS
074500970530     C     BLTDSE        ANDEQ     WVVDFV
074600060512     c* oppure se sono entrambe manuali
074700060512     c     spurps        oreq      'M'
074800060512     c     §7jrps        andeq     spurps
074900970530     C*
075000961121     C* SE NON TROVATA NESSUNA SPUNTA, NEMMENO ENTRATA PULISCO LE DATE
075100041014     c                   select
075200041014    3C     *IN17         wheneq    *OFF
075300961121     C     ENTDFV        ANDEQ     0
075400041014     C     prtDFV        ANDEQ     0
075500970530     C                   MOVEL     *ZEROS        BLTDSE
075600970530     C                   MOVEL     *ZEROS        BLTNPE
075700060512     C                   MOVEL     ' '           BLTCAN
075800961121     C* SE COLLO LOCALE E
075900961121     C*  TROVATA SOLO SPUNTA ENTRATA O <= ALL'ALTRA SPUNTA TROVATA, USO
076000041014    3C     *IN17         wheneq    *Off
076100970530     C     ENTDFV        ORGT      0
076200041014     C     ENTDFV        ANDLE     prtDFV
076300961121     C                   MOVEL     ENTDFV        BLTDSE
076400961121     C                   MOVEL     ENTNPS        BLTNPE
076500041014   X3C                   other
076600961121     C*
076700961121     C* AGGIORNO SOLO CON L'ALTRA SPUNTA TROVATA O ENTRATA SE NO LOCALE
076800041014     C                   MOVEL     prtDFV        BLTDSE
076900041014     C                   MOVEL     prtNPS        BLTNPE
077000041014    3C                   ENDsl
077100060512   2aC                   ELSE
077200060512     c* 20 on - non aggiorno la data
077300060512     C                   SETON                                        20
077400060512   2aC                   ENDIF
077500961121     C**
077600961121   X2C                   ELSE
077700041014     c* 20 on - non aggiorno la data
077800930604     C                   SETON                                        20
077900961121    2C                   ENDIF
078000990409     C* DCD
078100990409    2C     FLG           IFEQ      '1'
078200990409     C     WDCD          ANDEQ     'S'
078300990409     C*
078400990409     C* SE NON TROVATA NESSUNA SPUNTA, NEMMENO ENTRATA PULISCO LE DATE
078500041014     C     danDFV        ifeq      0
078600990409     C                   MOVEL     *ZEROS        DCDDFV
078700990409     C                   MOVEL     *ZEROS        DCDFGS
078800990409     C                   MOVEL     *ZEROS        DCDNPS
078900990409     C                   CLEAR                   DCDFAR
079000990409   X3C                   ELSE
079100150819     C* Imposto la spunta con data più altra se>=alla data presente
079200041014     C     danDFV        ifge      dcddfv
079300041014     C                   MOVEL     danDFV        DCDDFV
079400041014     C                   MOVEL     danNPS        DCDNPS
079500041014     C                   MOVEL     danFGS        DCDFGS
079600041014     c                   else
079700041014     C                   MOVEL     *ZEROS        DCDDFV
079800041014     C                   MOVEL     *ZEROS        DCDFGS
079900041014     C                   MOVEL     *ZEROS        DCDNPS
080000041014     C                   CLEAR                   DCDFAR
080100041014   X4C                   endif
080200041014   X4C                   endif
080300101202     c*
080400990409     C                   UPDATE    FNDCD000
080500990409    2C                   ENDIF
080600921020     C*
080700970530     C                   UPDATE    FNBLT000
080800921019     C*
080900970530    2C     FLG           IFEQ      '1'
081000930604     C     *IN20         ANDEQ     '0'
081100930604     C*
081200031114     C     KBLT          CHAIN     FNBLP01L                           3339
081300150819     c* Visto che una bolla DPD eveva la data BLPDSE impostata ma non era
081400150819     c*  presente nel dettaglio colli, e visto che il pgm gestisce la eliminaz.
081500150819     c*  l'unico caso che mi viene in mente è che la bolla era allocata.
081600150819     c*  in sto caso scrivo FNAGB
081700150819     c                   if        *in39
081800150819     c                   clear                   fnagb000
081900150819     C                   MOVEL     'P'           AGBTBO
082000150819     C                   MOVEL     'DT'          AGBAGB
082100150819     C                   Z-ADD     blTAAS        AGBAAS
082200150819     C                   Z-ADD     blTLNP        AGBLNP
082300150819     C                   Z-ADD     blTNRS        AGBNRS
082400150819     C                   Z-ADD     blTNSP        AGBNSP
082500150819     C     KAGB          SETLL     FNAGB01L                               31
082600150819     C  N31              WRITE     FNAGB000
082700150819     c
082800150819     C                   GOTO      ENDUPD
082900150819     c                   endif
083000921019     C*
083100930604     C* CONTROLLO DETTAGLIO PER IMPOSTARE 1° DATA ENTRATA
083200001206     C  N20
083300001206     CANN33              EXSR      CTRBLT
083400921230     C*
083500941206     C  N33              UPDATE    FNBLP000
083600031203    2C                   END
083700031203   1aC                   END
083800921019     C*
083900970530    1C                   END
084000041216     c                   unlock    fnblt27l
084100031114     C**
084200031114     C* A G G I O R N O   B O L L E   A R R I V I
084300921013     C**
084400031114    1C     wagarr        IFEQ      'S'
084500941206     C                   SETOFF                                           20
084600941230     C     KBOL2         CHAIN     FNART27L                           3239
084700990409     C                   CLEAR                   WDCD
084800990409     C*
084900031114    2C     *IN39         IFEQ      *OFF
085000041014     c
085100060512     c* Solo se non ho già aggiornato da partenza
085200041015    3c                   if        wsploc=*blanks
085300990409     C     KBOL2         CHAIN     FNDCD02L                           3839
085400060710   3ac                   if        not *in38 and not *in39
085500060512     c                   movel     dcdnps        knps
085600060512     c                   exsr      TIPPIS
085700101202     C*
085800060512    4C     DCDNPS        IFEQ      NPS
085900990409     C     DCDDFV        ANDEQ     WVVDFV
086000060512     C     spurps        oreq      'M'
086100060512     C     §7jrps        andeq     spurps
086200990409     C                   MOVEL     'S'           WDCD              1
086300990409     C                   ELSE
086400990409     C                   UNLOCK    FNDCD02L
086500041015    4C                   ENDIF
086600060512   3aC                   ENDIF
086700041015    3C                   ENDIF
086800031114    2C                   ENDIF
086900920424     C*
087000921013     C   39              GOTO      ENDUPD
087100031203     c
087200041014     c* Solo se esiste la bolla
087300031203   1ac                   if        not *in32
087400990409     C* AGGIORNO FNDCD
087500990409    2C     FLG           IFEQ      '1'
087600990409     C     WDCD          ANDEQ     'S'
087700990409     C*
087800041014    4C                   IF        dandfv=0
087900990409     C                   CLEAR                   DCDFGS
088000990409     C                   CLEAR                   DCDNPS
088100990409     C                   CLEAR                   DCDDFV
088200990409     C                   CLEAR                   DCDFAR
088300990409   X4C                   ELSE
088400041014     C* ESISTE UN'ALTRA SPUNTA PER IL COLLO con data >= di quella annullata
088500041015    5c                   if        dandfv>=dcddfv
088600041014     C                   MOVEL     danDFV        DCDDFV
088700041014     C                   MOVEL     danNPS        DCDNPS
088800041015   x5C                   MOVEL     danFGS        DCDFGS
088900041014     c                   else
089000041014     C                   CLEAR                   DCDFGS
089100041014     C                   CLEAR                   DCDNPS
089200041014     C                   CLEAR                   DCDDFV
089300041014     C                   CLEAR                   DCDFAR
089400041015    5C                   ENDIF
089500041015    4C                   ENDIF
089600101202     c*
089700990409     C                   UPDATE    FNDCD000
089800990409    2C                   ENDIF
089900990409     C*
090000961129    2C     FLG           IFEQ      '1'
090100060512     c                   movel     artnap        knps
090200060512     c                   exsr      TIPPIS
090300101202     c*
090400060512   2aC     ARTNAP        ifeq      NPS
090500970530     C     ARTDAM        ANDEQ     WVVDFV
090600060512     c* Oppure se sono entrambe manuali
090700060512     c     spurps        oreq      'M'
090800060512     c     §7jrps        andeq     spurps
090900101202     c*
091000970604     C* MEMORIZZO LA DATA DI ARRIVO PRIMA DI MODIFICARLA
091100970604     C                   MOVE      ARTDAM        SAVDAM
091200970530     C**
091300960613     C* NON ESISTONO ALTRE SPUNTE PER IL COLLO
091400041014    3C     *IN16         IFEQ      *OFF
091500961129    4C     ARTDCM        IFGT      *ZEROS
091600960613     C                   MOVE      ARTDCM        ARTDAM
091700960613     C                   Z-ADD     85            ARTNAP
091800960613     C                   ELSE
091900960613     C                   Z-ADD     0             ARTDAM
092000960613     C                   Z-ADD     0             ARTNAP
092100050509     c                   clear                   artfl2
092200060512     c                   clear                   artcan
092300060512     C                   SETOff                                       06
092400961129    4C                   END
092500970530   X3C                   ELSE
092600960613     C* ESISTE UN'ALTRA SPUNTA PER IL COLLO
092700041014     C                   MOVEL     arrDFV        ARTDAM
092800041014     C                   MOVEL     arrNPS        ARTNAP
092900050509     C                   MOVEL     arrFL2        ARTFL2
093000970530    3C                   END
093100970530     C* SCRIVO LA STATISTICA PER RIELABORARE LA NUOVA DATA CHE HO
093200970604     C*  SCRITTO E LA DATA CHE C'ERA PRIMA
093300970604     C     ARTDAM        IFNE      SAVDAM
093400070424     c* Memorizzo che devo riaggiornare data arrivo ultimo collo
093500070424     c                   eval      AggioDTI='S'
093600970604     C* VECCHIA DATA
093700970604     C                   CLEAR                   SA2TLA
093800970604     C                   MOVEL     'A'           SA2TRC
093900970604     C                   Z-ADD     SAVDAM        SA2DTS
094000970604     C                   MOVE      *ZEROS        SA2LNA
094100970604     C                   CALL      'FNLSA2R3'
094200970604     C                   PARM                    SA2TLA            1
094300970604     C                   PARM                    SA2TRC            1
094400970604     C                   PARM                    SA2DTS            8 0
094500970604     C                   PARM                    SA2LNA            3 0
094600970604     C* NUOVA DATA
094700970604     C     ARTDAM        IFGT      *ZEROS
094800970604     C                   Z-ADD     ARTDAM        SA2DTS
094900970604     C                   CALL      'FNLSA2R3'
095000970604     C                   PARM                    SA2TLA            1
095100970604     C                   PARM                    SA2TRC            1
095200970604     C                   PARM                    SA2DTS            8 0
095300970604     C                   PARM                    SA2LNA            3 0
095400970604     C                   END
095500970604     C                   END
095600970625     C**
095700970625     C* LA SPUNTA ANNULLATA NON E' QUELLA CHE HA AGGIORNATO IL DETTAGL
095800060512   2aC                   ELSE
095900930604     C                   SETON                                        20
096000060512   2aC                   ENDIF
096100060512     C**
096200060512     C* LA SPUNTA ANNULLATA NON E' QUELLA CHE HA AGGIORNATO IL DETTAGL
096300060512   X2C                   ELSE
096400060512     C                   SETON                                        20
096500060512    2C                   ENDIF
096600970625     C*
096700961129    2C     SAVCAN        IFEQ      ' '
096800921020     C     ARTCAN        ANDEQ     CAN
096900921020     C                   MOVEL     ' '           ARTCAN
097000921019     C                   ELSE
097100921019     C                   SETON                                        06
097200961129    2C                   END
097300950118     C*
097400960417     C*  MEMORIZZAZIONI PER ANOMALIE
097500961129    2C     FLG           IFEQ      '1'
097600960416     C                   MOVE      ARTAAS        WAAS
097700960416     C                   MOVE      ARTLNP        WLNP
097800960416     C                   MOVE      ARTNSP        WNSP
097900960417     C                   MOVE      ARTDAM        WDAM
098000070424     C                   MOVE      ARTdet        WDET
098100070424     C                   MOVE      ARTFLP        WFLP
098200960417     C                   MOVE      ARTDCM        WDCM
098300961129    2C                   END
098400921020     C*
098500961129     C                   MOVEL     ARTDTR        WDTR
098600941206     C                   UPDATE    FNART000
098700921019     C*
098800911016     C* AGGANCIO BOLLA
098900930604     C*
099000941206     C     KARB          CHAIN     FNARB01L                           3339
099100920424     C* 39 ON - RECORD ALLOCATO
099200970625    2C     *IN39         IFEQ      *OFF
099300030115      * Mi salvo arbdam
099400030115     c                   Eval      Sav_ArbDam = ArbDam
099500970625     C**
099600921019     C* CONTROLLO TUTTO IL DETTAGLIO
099700970625    3C     *IN20         IFEQ      *OFF
099800970625     C     *IN06         OREQ      *OFF
099900970625     C*
100000921019     C  N33              EXSR      CTRART
100100970625     C* 06 OFF - NESSUN CODICE ANOMALIA: ELIMINO ANCHE IN TESTATA
100200970625     C  N06              MOVEL     ' '           ARBFAN
100300970625     C* FLAG ABILITAZIONE MERCE:
100400970625    4C  N20FLG           IFEQ      '1'
100500970625     C     ARBAMA        ANDNE     3
100600101202     c*
100700060512     c                   select
100800970625     C* ABILITATO IN BASE ALLA SPUNTA ARRIVO
100900060512    5C     NOAMA         wheneq    ' '
101000970625     C                   MOVEL     2             ARBAMA
101100970625     C                   MOVEL     1             ARBAMP
101200970625     C* ABILITATO IN BASE ALLA SPUNTA PARTENZA(ABBINATO)
101300060512    5C     NOAMP         wheneq    ' '
101400970625     C                   MOVEL     1             ARBAMA
101500970625     C                   MOVEL     1             ARBAMP
101600060512    5C     NOAMP         wheneq    '0'
101700060512     C                   MOVEL     0             ARBAMA
101800060512     C                   MOVEL     1             ARBAMP
101900060512   X5C                   other
102000970625     C* NON ABILITATO
102100970625     C                   MOVEL     0             ARBAMA
102200970625     C                   MOVEL     0             ARBAMP
102300060512    5C                   ENDsl
102400970625    4C                   END
102500970625    3C                   ENDIF
102600961120     C*
102700961120     C* MEMORIZZO LA DATA BORDERO' CHE MI SERVIRA' PER BOLLA LOCALE
102800961120     C*  SE DEVO CREARE L'ANOMALIA 5
102900961120     C                   Z-ADD     ARBDBR        WDBR
103000961120     C                   MOVEL     ARBFBC        WFBC
103100970127     C* TESTO SE SPEDIZIONE GIACENTE PER ANOMALIA 15
103200970625    3C     WFBC          IFEQ      'G'
103300050401     C     KGCA          CHAIN     Tigcp21L                           38
103400970721     C* AI FINI DELL'IMA DEVO TESTARE LA DATA DISPOSIZONI MITTENTE
103500970721    5C  N38GCPFAS        IFLT      35
103600050401    5C     GCPFAS        oreq      36
103700161021     c                   clear                   fnlgi1ds
103800161021     c                   eval      ilgi1aas=arbaas
103900161021     c                   eval      ilgi1lnp=arblnp
104000161021     c                   eval      ilgi1nrs=arbnrs
104100161021     c                   eval      ilgi1nsp=arbnsp
104200161021     c                   select
104300161021     c                   when      arbncp=arbncl
104400161021     c                   eval      ilgi1pkf=arbpkc
104500161021     c                   when      arbpkc>arbpkf
104600161021     c                   eval      ilgi1pkf=arbpkc
104700161021     c                   other
104800161021     c                   eval      ilgi1pkf=arbpkf
104900161021     c                   endsl
105000161021     c                   movel     fnlgi1ds      kpjbu
105100161021     c                   call      'FNLGI1R'
105200161021     c                   parm                    kpjba
105300161021     c                   parm                    tigcp00ds
105400161021     c                   movel     kpjbu         fnlgi1ds
105500161021
105600161021   5bc                   if        olgi1AMG='Y'  or olgi1inmg='S'
105700161021     C                   MOVEL     'S'           WGIAC             1
105800161021     c                   endif
105900161021     c
106000970625    3C                   ENDIF
106100970721    3C                   ENDIF
106200070424     c*
106300150819    3c                   if        Aggiodti='S'
106400070424     c                   clear                   arbdti
106500070424     c                   clear                   arbhti
106600070424     C     ARBFBS        IFEQ      ' '
106700070424     C                   MOVEL     'S'           ARBFBS
106800070424     C                   ELSE
106900070424     C     ARBFBS        IFEQ      'V'
107000070424     C     ARBFBS        oreq      'P'
107100070424     C                   MOVEL     'E'           ARBFBS
107200070424     C                   ENDIF
107300070424     C                   ENDIF
107400101202     c*
107500150819    3C                   ENDIF
107600921019     C*
107700941206     C  N33              UPDATE    FNARB000
107800101202      *
107900030320      * Se è un tipo bolla da trasmettere in sede
108000030320     c                   Clear                   ds3a
108100030320     c                   Eval      Cod = '3A'
108200030320     c                   Clear                   Key
108300030320     c                   Movel     ArbCbo        Key
108400030320     c     kTab          Chain     Tabel00f
108500030320     c                   If        %Found(Tabel00f)
108600030320     c                   Movel     TblUni        ds3a
108700030320     c                   EndIf
108800030320If  2c                   If        §3aBsd = *Blanks
108900030115      * Scrivo/Aggiorno Fidta se data variata
109000030115     c                   If        Not *In33 and Sav_ArbDam <> ArbDam
109100030115     c                   Eval      wFidta = '0'
109200030115     c                   Eval      SavAas = ArbAas
109300030115     c                   Eval      SavLnp1 = ArbLnp
109400030115     c                   Eval      SavNrs = ArbNrs
109500030115     c                   Eval      SavNsp = ArbNsp
109600030115     c                   Eval      SavPoc = ArbLna
109700030115     c                   Eval      SavTrd = 'DAM'
109800030115     c     kFidta        Setll     Fidta01l
109900030115     c                   Do        *Hival
110000030115     c     kFidta        Reade     Fidta01l
110100030115      * Fine file
110200030115     c                   If        %Eof(Fidta01l)
110300030115     c                   Leave
110400030115     c                   EndIf
110500030115      * Già trasmesso
110600030115     c                   If        DtaFtr <> *Blanks
110700030115     c                   Iter
110800030115     c                   EndIf
110900030115      * Aggiorno
111000030115     c                   Eval      DtaDta = ArbDam
111100030115     c                   Update    Fidta000
111200030115     c                   Eval      wFidta = '1'
111300030115     c                   EndDo
111400030115      * Scrivo
111500030115     c                   If        wFidta = '0'
111600030115     c                   Clear                   Fidta000
111700030115     c                   Eval      DtaAas = ArbAas
111800030115     c                   Eval      DtaLnp = ArbLnp
111900030115     c                   Eval      DtaNrs = ArbNrs
112000030115     c                   Eval      DtaNsp = ArbNsp
112100030115     c                   Eval      DtaTrd = SavTrd
112200030115     c                   Eval      DtaPoc = SavPoc
112300030115     c                   Eval      DtaDta = ArbDam
112400030115     c                   Write     Fidta000
112500030115     c                   EndIf
112600030320    2c                   EndIf
112700030115     c                   EndIf
112800101202      *
112900970625   X2C                   ELSE
113000960614     C* BOLLA ALLOCATA:
113100960614     C* SE NON E' L'ULTIMO TENTATIVO VADO A FINE ROUTINE
113200970625    3C     B             IFLT      3
113300960614     C                   GOTO      ENDUPD
113400970625    3C                   END
113500960614     C* SE E' L'ULTIMO TENTATIVO SCRIVO FNAGB
113600960614     C                   MOVEL     'A'           AGBTBO
113700960614     C                   MOVEL     'DT'          AGBAGB
113800960614     C                   Z-ADD     ARTAAS        AGBAAS
113900960614     C                   Z-ADD     ARTLNP        AGBLNP
114000960614     C                   Z-ADD     ARTNRS        AGBNRS
114100960614     C                   Z-ADD     ARTNSP        AGBNSP
114200960614     C                   MOVEL     'S'           AGBFBS
114300960614     C                   CLEAR                   AGBFVC
114400041014     C                   CLEAR                   AGBFpC
114500960614     C     KAGB          SETLL     FNAGB01L                               31
114600960614     C  N31              WRITE     FNAGB000
114700970625    2C                   END
114800031203   1aC                   END
114900921013     C*
115000031114    1C                   END
115100041216     c                   unlock    fnart27l
115200041216     c                   unlock    fnarb01l
115300031114     C**
115400031114     C** T R A N S I T I  /  D I S G U I D I   I N   A R E A   E  N O N
115500031114     C**
115600031114    1C     WESBTT        IFEQ      'S'
115700911112     C*
115800031114     C     KBAR37        CHAIN     FNBTT37L                           3739
115900920424     C* 39 ON  - RECORD ALLOCATO
116000920424     C   39              GOTO      ENDUPD
116100911016     C*
116200921019     C* 37 OFF - TROVATA BOLLA
116300941230    2C  N37              DO
116400941230     C*
116500941230    3C     SAVCAN        IFEQ      ' '
116600941206     C     BTTCAN        ANDEQ     CAN
116700941206     C                   MOVEL     ' '           BTTCAN
116800941230    3C                   END
116900921230     C*
117000941230    3C     FLG           IFEQ      '1'
117100060512     c                   movel     bttpet        knps
117200060512     c                   exsr      TIPPIS
117300101202     c*
117400060512   3aC     BTTPET        ifeq      NPS
117500970530     C     BTTDET        ANDEQ     WVVDFV
117600060512     c* Oppure se sono entrambe manuali
117700060512     c     spurps        oreq      'M'
117800060512     c     §7jrps        andeq     spurps
117900101202     c*
118000970604     C                   MOVE      BTTDET        SAVDET
118100041014     C  N18              MOVEL     *ZEROS        BTTDET
118200041014     C  N18              MOVEL     *ZEROS        BTTPET
118300060512     C  N18              clear                   BTTcan
118400041014     C   18              MOVEL     traDFV        BTTDET
118500041014     C   18              MOVEL     traNPS        BTTPET
118600970625     C*
118700970604     C     BTTDET        IFNE      SAVDET
118800970604     C                   CLEAR                   SA2TLA
118900970604     C                   MOVEL     'A'           SA2TRC
119000970604     C                   Z-ADD     SAVDET        SA2DTS
119100970604     C                   MOVE      *ZEROS        SA2LNA
119200970604     C                   CALL      'FNLSA2R3'
119300970604     C                   PARM                    SA2TLA            1
119400970604     C                   PARM                    SA2TRC            1
119500970604     C                   PARM                    SA2DTS            8 0
119600970604     C                   PARM                    SA2LNA            3 0
119700970604     C* NUOVA DATA
119800970604     C     BTTDET        IFGT      *ZEROS
119900970604     C                   Z-ADD     BTTDET        SA2DTS
120000970604     C                   CALL      'FNLSA2R3'
120100970604     C                   PARM                    SA2TLA            1
120200970604     C                   PARM                    SA2TRC            1
120300970604     C                   PARM                    SA2DTS            8 0
120400970604     C                   PARM                    SA2LNA            3 0
120500970604     C                   END
120600970604     C                   END
120700970625     C*
120800970625     C* NON SONO STATE MODIFICATE LE DATE SUL DETTAGLIO COLLI
120900060512   3aC                   ELSE
121000930604     C                   SETON                                        20
121100060512   3AC                   END
121200060512     C*
121300060512     C* NON SONO STATE MODIFICATE LE DATE SUL DETTAGLIO COLLI
121400060512   X3C                   ELSE
121500060512     C                   SETON                                        20
121600060512    3C                   END
121700960416     C*  MEMORIZZO KEY SPEDIZIONE PER ANOMALIE
121800960416     C     FLG           IFEQ      '1'
121900960416     C                   MOVE      BTTAAS        WAAS
122000960416     C                   MOVE      BTTLNP        WLNP
122100960416     C                   MOVE      BTTNSP        WNSP
122200960416     C                   END
122300921020     C*
122400941206     C                   UPDATE    FNBTT000
122500921020     C*
122600911016     C* AGGANCIO BOLLA
122700941230    3C     FLG           IFEQ      '1'
122800930604     C     *IN20         ANDEQ     '0'
122900031114     C     KBTT          CHAIN     FNBTP11L                           3339
123000920424     C* 39 ON  - RECORD ALLOCATO
123100920424     C   39              GOTO      ENDUPD
123200030115      * Mi salvo btpdet (btpdut non viene modificato)
123300030115     c                   Eval      Sav_BtpDet = BtpDet
123400930604     C*
123500921020     C  N33              EXSR      CTRBTT
123600921019     C*
123700941206     C  N33              UPDATE    FNBTP000
123800030115      * Scrivo/Aggiorno Fidta se la data è variata
123900030115     c                   If        Not *In33 and Sav_BtpDet <> BtpDet
124000030115     c                   Eval      wFidta = '0'
124100030115     c                   Eval      SavAas = BtpAas
124200030115     c                   Eval      SavLnp1 = BtpLnp
124300030115     c                   Eval      SavNrs = BtpNrs
124400030115     c                   Eval      SavNsp = BtpNsp
124500030115     c                   Eval      SavPoc = BtpFlp
124600030115     c                   Eval      SavTrd = 'DET'
124700030115     c     kFidta        Setll     Fidta01l
124800030115     c                   Do        *Hival
124900030115     c     kFidta        Reade     Fidta01l
125000030115      * Fine file
125100030115     c                   If        %Eof(Fidta01l)
125200030115     c                   Leave
125300030115     c                   EndIf
125400030115      * Già trasmesso
125500030115     c                   If        DtaFtr <> *Blanks
125600030115     c                   Iter
125700030115     c                   EndIf
125800030115      * Aggiorno
125900030115     c                   Eval      DtaDta = BtpDet
126000030115     c                   Update    Fidta000
126100030115     c                   Eval      wFidta = '1'
126200030115     c                   EndDo
126300030115      * Scrivo
126400030115     c                   If        wFidta = '0'
126500030115     c                   Clear                   Fidta000
126600030115     c                   Eval      DtaAas = BtpAas
126700030115     c                   Eval      DtaLnp = BtpLnp
126800030115     c                   Eval      DtaNrs = BtpNrs
126900030115     c                   Eval      DtaNsp = BtpNsp
127000030115     c                   Eval      DtaTrd = SavTrd
127100030115     c                   Eval      DtaPoc = SavPoc
127200030115     c                   Eval      DtaDta = BtpDet
127300030115     c                   Write     Fidta000
127400030115     c                   EndIf
127500030115     c                   EndIf
127600101202      *
127700941230    3C                   END
127800921014     C*
127900941230    2C                   END
128000941230    1C                   END
128100041216     c                   unlock    fnbtt37l
128200041216     c                   unlock    fnbtp11l
128300101202     c*
128400041013     c* Aggiornamento anche di FNART per spunta di transito/disguido
128500041013     c*  cancellata
128600041013     c                   if        (lnatfa<>bpes and lnatfa<>pestfa  and
128700041013     c                             wagpar=' ') or wesbtt='S'
128800041013     C     KBOL2         CHAIN     FNART27L
128900041013     c                   if        %found(fnart27l)
129000101202     c*
129100041013    3C     SAVCAN        IFEQ      ' '
129200041013     C     arTCAN        ANDEQ     CAN
129300041013     C                   MOVEL     ' '           arTCAN
129400041013    3C                   END
129500041013     C*
129600041013    3C     FLG           IFEQ      '1'
129700060512     c                   movel     artpet        knps
129800060512     c                   exsr      TIPPIS
129900101202     c*
130000060512   3aC     artPET        ifeq      NPS
130100041013     C     artDET        ANDEQ     WVVDFV
130200060512     c* Oppure se sono entrambe manuali
130300060512     c     spurps        oreq      'M'
130400060512     c     §7jrps        andeq     spurps
130500101202     c*
130600041015     C   18              MOVEL     traDFV        artDET
130700041015     C   18              MOVEL     traNPS        artPET
130800041014     C  N18              MOVEL     *ZEROS        artDET
130900041014     C  N18              MOVEL     *ZEROS        artPET
131000060512     C  N18              CLEAR                   artCAN
131100041014    4c  n18              if        wesbtt<>'S'
131200041014     C                   MOVEL     *ZEROS        artflp
131300041014     c* Verifico le spunte partenza per impostare eventuale altro p.o. di
131400041014     c*  transito/disguido
131500041014     c* Se p.o. ultima spunta partenza= p.o. ultima spunta no part.
131600041014     c* imposto questa
131700041014    5c                   select
131800041014     c                   when      dis1fgs=dis2fgs
131900041014     c                   z-add     dis2dfv       artdet
132000041014     c                   z-add     dis2nps       artpet
132100041014     c                   z-add     dis2fgs       artflp
132200041014     c                   z-add     dis1dfv       artdut
132300041014     c                   z-add     dis1nps       artput
132400101202     c*
132500041014     c* Elaboro spunta partenza se con data > o data/ora caricamento>
132600041014     c                   when      dis1dfv=dis2dfv and dis1scar>dis2scar or
132700041014     c                             dis1dfv>dis2dfv
132800041014     c* Se si tratta di uscita transito aggiorno subito altrimenti leggo
132900041014     c* le linee per vedere se c'e' transito o meno
133000041014    6c                   if        dis1atr='T' or dis1atr='D'
133100041014     c                   movel     dis1fgs       artflp
133200041014     c                   movel     dis1dfv       artdet
133300041014     c                   movel     dis1dfv       artdut
133400041014     c                   movel     dis1nps       artpet
133500041014     c                   movel     dis1nps       artput
133600041014   x6c                   else
133700041014     c* E si tratta della partenza o si tratta di un disguido che
133800041014     c* transita di nuovo per cui aggiorno solo flp
133900041014    7c                   if        dis1fgs<>lnptfp
134000041014     c                   movel     dis1fgs       artflp
134100041014   x7c                   else
134200041014     c* chain fnfgv
134300041014     c     kfgv          chain     fnfgv01l
134400041014    8c                   if        %found(fnfgv01l) and fgvlna<>lnatfa
134500041015     c     kfgv          chain     fnfgw01l
134600041015     c                   if        not %found(fnfgw01l)
134700041015     c                   clear                   fgwff3
134800041015     c                   clear                   fgwff4
134900041015     c                   clear                   fgwfl3
135000041015     c                   clear                   fgwfl4
135100041015     c                   endif
135200101202     c*
135300041015     c                   z-add     1             xx                3 0
135400041015     c                   movel     lna           wlna              3
135500041015     c                   movel     lnatfa        wlnatfa           3
135600041015     c     wlna          lookup    ffv(xx)                                31
135700041015    9c                   if        *in31 and flp(xx)<>wlnatfa
135800041014     c                   movel     flp(xx)       artflp
135900041014    9c                   endif
136000041014    8c                   endif
136100041014    7c                   endif
136200041014    6c                   endif
136300041014   x5c                   other
136400041014     c* Altrimenti elaboro spunta arrivi
136500041014     c                   movel     dis2fgs       artflp
136600041014     c                   movel     dis2dfv       artdet
136700041014     c                   movel     dis2nps       artpet
136800041014     c                   clear                   artdut
136900041014     c                   clear                   artput
137000041014    5c                   endsl
137100101202     c*
137200041014    4c                   endif
137300060512   3ac                   endif
137400060512    3c                   endif
137500101202     c*
137600041014     c                   update    fnart000
137700041014     c                   clear                   fnagb000
137800041014     C                   MOVEL     'A'           AGBTBO
137900041014     C                   MOVEL     'DT'          AGBAGB
138000041014     C                   Z-ADD     ARTAAS        AGBAAS
138100041014     C                   Z-ADD     ARTLNP        AGBLNP
138200041014     C                   Z-ADD     ARTNRS        AGBNRS
138300041014     C                   Z-ADD     ARTNSP        AGBNSP
138400041014     C     KAGB          SETLL     FNAGB01L                               31
138500041014     C  N31              WRITE     FNAGB000
138600041013     c                   endif
138700041014     c                   endif
138800921019     C**
138900921019     C** CHIUDO ANOMALIE
139000921019     C**
139100960416    1C     FLG           IFEQ      '1'
139200960416     C*
139300960416     C                   CLEAR                   W005
139400960416     C                   CLEAR                   W015
139500070424     c                   clear                   wcrea200          1
139600921021     C*
139700990518     C     KBOL2         SETLL     FNANM000
139800990518     C     KBOL2         READE     FNANM000                               32
139900921019     C*
140000961129    2C     *IN32         DOWEQ     *OFF
140100060512     c* Se anomalia 15 e non esiste più la data di arrivo del collo
140200060512     c*  devo cancellare l'anomalia
140300060512    3c                   if        anmcaa=015 and wdam=0  and wagarr='S'
140400060512    4c                   if        anmdch>0
140500060828     c                   unlock    fnanm02l
140600060512     c* Se già in idd3 la chiudo come annullata
140700060512     C                   MOVEL     FASE          D33FSC
140800060512     C                   MOVEL     'AN'          D33CCH
140900060828     C                   MOVEL     dateu         D33dCH
141000060512     C                   MOVE      ANMNR2        D33NRR
141100060512     C                   MOVEL     'S'           D33MAC
141200060512     C                   CALL      'FNLR33R'
141300060512     C                   PARM                    DSLR33
141400101202     c*
141500060512     C     KBOL2         READE     FNANM000                               32
141600060512     c                   iter
141700060512     c                   else
141800060512     c* la deleto
141900060512     c                   delete    fnanm000
142000101202     c*
142100060512     C     KBOL2         READE     FNANM000                               32
142200060512     c                   iter
142300060512    4c                   endif
142400060512    3c                   endif
142500101202     c*
142600070424     c* Se esiste anomalia 55 e artdam=0, vedo se ricreare la 200
142700130301    3c                   if        (anmcaa=57 or
142800130301    3c                              anmcaa=55 or anmcaa=56) and wdam=0
142900070424     c                             and anmdao=wdet and anmfle=wflp
143000070424     c                   eval      wcrea200='S'
143100070424     c                   eval      savrecanm=dsrecanm
143200070424    3c                   endif
143300101202     c*
143400070424     c* Se trovo già la 200 chiusa, la devo solo riaprire
143500070424     c                   if        wcrea200='S' and anmcaa=200 and
143600070424     c                             anmfle=wflp
143700070424     C                   Z-ADD     0             ANMDCH
143800070424     C                   MOVEL     ' '           ANMFSC
143900070424     C                   MOVEL     '  '          ANMCCH
144000070424     C                   CLEAR                   ANMFT1
144100070424     C                   CLEAR                   ANMFT2
144200070424     C                   UPDATE    FNANM000
144300101202     c*
144400070424     c* N= non creare la 200 perchè già trovata
144500070424     c                   eval      wcrea200='N'
144600110908     C     KBOL2         READE     FNANM000                               32
144700070424     c                   iter
144800070424     c                   endif
144900101202     c*
145000110908     c* Anomlia 115 --> se non esiste nessun'altra spunta sul sistema reale
145100110908     c*                 riapro l'anomalia anche non mia
145200110908    3c                   if        anmcaa=115
145300110908    4c                   if        a_115dfv=0
145400110908     C                   Z-ADD     0             ANMDCH
145500110908     C                   MOVEL     ' '           ANMFSC
145600110908     C                   MOVEL     '  '          ANMCCH
145700110908     C                   CLEAR                   ANMFT1
145800110908     C                   CLEAR                   ANMFT2
145900110908   x4c                   else
146000110908     c* altrimenti se è stata con questa spunta--> la riaggiorno
146100110908    5C     ANMDCH        IFNE      0
146200110908     C     ANMFSC        ANDEQ     FASE
146300110908     C     ANMdch        ANDEQ     wvvdfv
146400110908     C                   CLEAR                   DSFASE
146500110908     C                   MOVE      a_115NPG      D11NPG
146600110908     C                   MOVE      a_115SPG      D11SPG
146700110908     C                   CALL      'TRUL11R'
146800110908     C                   PARM                    DSFASE
146900110908     C                   MOVEL     D11FAS        ANMFSC
147000110908     C                   MOVEL     a_115dfv      ANMdch
147100110908    5c                   endif
147200110908    4c                   endif
147300110908     C                   UPDATE    FNANM000
147400110908     c
147500110908     C     KBOL2         READE     FNANM000                               32
147600110908     c                   iter
147700110908    3c                   endif
147800060512     C*
147900041018     c* Leggo solo le anomalie create da me
148000120109    3c*****              if        anmfle=bpes or anmfle=parfgs
148100921019     C*
148200921019     C* RIAPRO SE CHIUSE E NESSUN'ALTRA SPUNTA
148300980205     C* E SE LA SPEDIZIONE NON E' CONSEGNATA
148400120109    3C                   if        anmcaa=005
148500120109    4c                   if        anmfle=bpes or anmfle=parfgs
148600120109     c                             or  anmfle=lnatfa
148700041018    5c     wagarr        ifeq      'S'
148800961129     C                   MOVE      '1'           W005              1
148900921019     C*
149000041018    6C  N19WDCM          IFEQ      0
149100980205     C     WDAM          ANDEQ     0
149200980205     C**
149300041018    7C     ANMDCH        IFNE      0
149400041018    8C     ANMLID        IFGE      §7ALCI
149500960510     C                   MOVE      'PR'          ANMCCH
149600960510     C                   MOVE      'D'           ANMFSC
149700970410     C                   CLEAR                   ANMFT1
149800970410     C                   CLEAR                   ANMFT2
149900960510     C                   ELSE
150000921019     C                   Z-ADD     0             ANMDCH
150100921019     C                   MOVEL     ' '           ANMFSC
150200921019     C                   MOVEL     '  '          ANMCCH
150300970410     C                   CLEAR                   ANMFT1
150400970410     C                   CLEAR                   ANMFT2
150500041018    8C                   END
150600990518     C                   UPDATE    FNANM000
150700041018    7C                   END
150800041018    6C                   END
150900041018    5C                   END
151000120109    4C                   END
151100921021     C*
151200120109   x3c                   else
151300120109     c
151400120109    4c                   if        anmfle=bpes or anmfle=parfgs
151500101202     c*
151600921019     C* PER TUTTE LE ALTRE ANOMALIE CONTROLLO FASE APERTURA E FOGLIO
151700041018    5C     ANMNFV        IFEQ      PARNFV
151800921021     C     ANMFSA        ANDEQ     FASE
151900960513     C*
152000960417     C     ANMCAA        OREQ      050
152100960417     C     *IN19         ANDEQ     *OFF
152200041018     c     anmft1        andeq     *blanks
152300101202     c*
152400041018     c* chain in DS/C per controllo se autochiusa
152500041018     C     anmcaa        IFNE      SAVCAA
152600041018     C                   MOVEL     '7C'          COD
152700041018     C                   MOVEL(P)  ANMCAA        KEY
152800041018     C     KTAB          CHAIN     TABEL                              31
152900041018     C  N31              MOVEL     TBLUNI        DS7C
153000041018     C   31              MOVEL     *BLANKS       DS7C
153100041018     C                   MOVEL     anmcaa        SAVCAA
153200041018     C                   ENDIF
153300960513     C*
153400041018     C* PER alcune anomalie,
153500041018     C*  SE ESISTE UN'ALTRA SPUNTA PER IL COLLO MODIFICO
153600041018     C*  L'ANOMALIA IN BASE ALLA SPUNTA RIMASTA
153700041018    6c                   if        *in19 and (anmcaa=10 or anmcaa=160
153800041018     c                             or anmcaa=200 or anmcaa=60 or
153900041018     c                             anmcaa=61)
154000960417     C                   CLEAR                   DSFASE
154100960417     C                   MOVE      SPUNPG        D11NPG
154200960417     C                   MOVE      SPUSPG        D11SPG
154300960417     C                   CALL      'TRUL11R'
154400960417     C                   PARM                    DSFASE
154500960417     C                   MOVEL     D11FAS        ANMFSA
154600960513     C                   MOVE      SPUFGS        ANMCDU
154700041018     C                   Z-ADD     SPUNFV        ANMNFV
154800041018     C* NELL'ANOMALIA 10 NON MODIFICO LA DATA APERTURA PER NON FAR DIVE
154900041018     C*  NTARE INCONGRUENTE IL LIVELLO IDD CON LA DATA CREAZIoNE ANOM
155000041018    7c                   if        anmcaa<>10
155100041018     C                   Z-ADD     SPUDFV        ANMDAO
155200041018    7c                   endif
155300041018     C                   MOVE      SPUFGS        ANMFLE
155400041018     C                   MOVE      SPUNPS        ANMNPS
155500041018     c* se anomalia autochiusam, imposto anche i dati di chiusura
155600041018     c                   if        §7ccha='S'
155700041018     C                   MOVE      spudfv        ANMDCH
155800041018     C                   MOVEL     d11fas        ANMFSC
155900041018     c                   endif
156000970603     C*
156100041018    7C     ANMAIE        IFEQ      'E'
156200970603     C                   CLEAR                   ANMFT1
156300970603     C                   CLEAR                   ANMFT2
156400041018    7C                   ENDIF
156500990518     C                   UPDATE    FNANM000
156600041018   X6C                   ELSE
156700990412     C*
156800990412     C* SE SI TRATTA DI ANOMALIA 56 CON ALTRE SPUNTE (19 ON):
156900041018     C*  modifico anche se trasmessa essendo su As unico, non
157000041018     C*  modifico però la data visto che ha aggiornato bltdet
157100041018    7C                   IF        *in19 and (anmcaa=55 or anmcaa=56)
157200101202     C*
157300041018    8C                   if        ANMCAA=56 AND A56DFV>0
157400041018     c                   if        anmft1=' '
157500990412     C                   Z-ADD     A56DFV        ANMDAO
157600990412     C                   Z-ADD     A56DFV        ANMDCH
157700041018     c                   endif
157800990412     C                   CLEAR                   DSFASE
157900990412     C                   MOVE      A56NPG        D11NPG
158000990412     C                   MOVE      A56SPG        D11SPG
158100990412     C                   CALL      'TRUL11R'
158200990412     C                   PARM                    DSFASE
158300990412     C                   MOVEL     D11FAS        ANMFSA
158400990412     C                   MOVEL     D11FAS        ANMFSC
158500990412     C                   MOVE      A56FGS        ANMCDU
158600990412     C                   MOVE      A56FGS        ANMFLE
158700990412     C                   MOVE      A56NPS        ANMNPS
158800990518     C                   Z-ADD     A56NFV        ANMNFV
158900990518     C                   UPDATE    FNANM000
159000041018   X8C                   else
159100990412     C* LA TRASFORMO IN 55
159200990412     C                   Z-ADD     55            ANMCAA
159300041018    9c                   if        anmft1=' '
159400990412     C                   Z-ADD     SPUDFV        ANMDAO
159500990412     C                   Z-ADD     SPUDFV        ANMDCH
159600041018    9c                   endif
159700990412     C                   CLEAR                   DSFASE
159800990412     C                   MOVE      SPUNPG        D11NPG
159900990412     C                   MOVE      SPUSPG        D11SPG
160000990412     C                   CALL      'TRUL11R'
160100990412     C                   PARM                    DSFASE
160200990412     C                   MOVEL     D11FAS        ANMFSA
160300990412     C                   MOVEL     D11FAS        ANMFSC
160400990412     C                   MOVE      SPUFGS        ANMCDU
160500990412     C                   MOVE      SPUFGS        ANMFLE
160600990412     C                   MOVE      SPUNPS        ANMNPS
160700990518     C                   Z-ADD     SPUNFV        ANMNFV
160800990518     C                   UPDATE    FNANM000
160900041018    8C                   ENDif
161000990412   X7C                   ELSE
161100960416     C* Richiamo pgm di chiusura anomalia
161200990518     C                   UNLOCK    FNANM02L
161300960416     C                   CLEAR                   DSLR33
161400041018    8C     DATEU         IFGE      WVVDFV
161500970117     C                   MOVEL     DATEU         D33DCH
161600970117     C                   ELSE
161700041018    8C                   MOVEL     WVVDFV        D33DCH
161800970117     C                   ENDIF
161900960416     C                   MOVEL     FASE          D33FSC
162000960416     C                   MOVEL     'AN'          D33CCH
162100960416     C                   MOVE      ANMNR2        D33NRR
162200960510     C                   MOVEL     'S'           D33MAC
162300960416     C                   CALL      'FNLR33R'
162400960416     C                   PARM                    DSLR33
162500990412    7C                   ENDIF
162600041018    6C                   END
162700921021     C*
162800041018   X5C                   ELSE
162900041018    6C     ANMCAA        IFNE      50
163000041018    6C     ANMCAA        andne     56
163100041018    6C     ANMCAA        andne     55
163200130301    6C     ANMCAA        andne     57
163300921021     C* CONTROLLO SE E' STATA CHIUSA IN QUESTA FASE
163400041018    7C     ANMDCH        IFNE      0
163500921021     C     ANMFSC        ANDEQ     FASE
163600041018     C     ANMdch        ANDEQ     wvvdfv
163700041018     c* Per l'anomalia 15 controllo se esiste altra spunta con data >= a
163800041018     c*  quella annullata nel qual caso la chiudo con dati diversi
163900041018     c                   clear                   wokanm15
164000041018    9C                   if        anmcaa=015
164100041018     C                   MOVE      '1'           W015              1
164200041018    0c                   if        dandfv>wvvdfv  or
164300041018     c                             (dannpg<>2 and dandfv>wvvdfv)
164400110908     C                   CLEAR                   DSFASE
164500110908     C                   MOVE      danNPG        D11NPG
164600110908     C                   MOVE      danSPG        D11SPG
164700110908     C                   CALL      'TRUL11R'
164800110908     C                   PARM                    DSFASE
164900110908     C                   MOVEL     D11FAS        ANMFSC
165000110908     C                   MOVEL     dandfv        ANMdch
165100041018     c                   eval      wokanm15='S'
165200041018    9c                   endif
165300041018    8c                   endif
165400041018     c*
165500041018    8c                   if        wokanm15=' '
165600041018    9c     ANMLID        IFGE      §7ALCI
165700960510     C                   MOVE      'PR'          ANMCCH
165800960510     C                   MOVE      'D'           ANMFSC
165900960510     C                   ELSE
166000921021     C                   Z-ADD     0             ANMDCH
166100921021     C                   MOVEL     ' '           ANMFSC
166200921021     C                   MOVEL     '  '          ANMCCH
166300041018    9C                   END
166400961025     C                   MOVE      *BLANKS       ANMFT1
166500990518     C                   UPDATE    FNANM000
166600041018    8C                   END
166700041018    7C                   END
166800041018    6C                   END
166900041018    5C                   END
167000921021     C*
167100120109    4C                   ENDIF
167200120109    3C                   END
167300921019     C*
167400990518     C     KBOL2         READE     FNANM000                               32
167500960416    2C                   ENDDO
167600070424     c*
167700070424     c* Verifico se devo creare  l'anomalia 200
167800070424     c                   if        wcrea200='S'
167900070424     c                   eval      dsrecanm=savrecanm
168000070424     C                   Z-ADD     200           COMCAA
168100070424     c                   exsr      CRTANM
168200070424     c                   eval      wcrea200='N'
168300070424     c                   endif
168400960416     C*
168500960416     C* CREO ANOMALIA 005 A FRONTE DI CANCELLAZIONE SPUNTA
168600960416     C     W005          IFEQ      *BLANKS
168700031114     c     wagarr        andeq     'S'
168800960416     C     *IN19         ANDEQ     *OFF
168900960416     C     WCHIUS        ANDEQ     'S'
169000960530     C     WDAM          ANDEQ     *ZEROS
169100960530     C     WDCM          ANDEQ     *ZEROS
169200000904     C*
169300031114     c* creo sempre anche per 2 liv in arrivo su altro AS in quanto
169400031114     c*  se poi dal terminal riceco la spunta l'anomalia viene cancell
169500031114     c*  se riricevo l'anomalia, dovrebbe andare in update
169600031114     c* (prioma invece creava soltanto se terarr di lna in £1)
169700000904     C                   EXSR      CRTA05
169800000904     C                   ENDIF
169900960530     C**
170000960416     C* CREO ANOMALIA 015 A FRONTE DI CANCELLAZIONE SPUNTA
170100960416     C     W015          IFEQ      *BLANKS
170200031114     C     wagarr        andeq     'S'
170300960416     C* Collo non consegnato
170400960416     C     WDCM          ANDEQ     *ZEROS
170500960417     C* Collo arrivato
170600960417     C     WDAM          ANDGT     *ZEROS
170700960416     C* Data arrivo <= data spunta cancellata
170800960416     C     WDAM          ANDLE     WVVDFV
170900960416     C* Non esistono altre spunte inventario o consegna in data >=
171000960416     C* alla data di cancellazione della spunta
171100960416     C     WSPUN         ANDEQ     *BLANKS
171200970127     C* La bolla non deve essere bloccata
171300970127     C     WFBC          ANDNE     'B'
171400970127     C* La bolla non è in giacenza
171500970127     C     WGIAC         ANDNE     'S'
171600000904     C                   EXSR      CRTA15
171700000904     C                   END
171800960416     C*
171900960416    1C                   END
172000050408     c* se la pistola della spunta annullata non è automatica devo decrement
172100050408     c* are sgnst2 su fisgn
172200060512     c                   movel     nps           knps
172300050408     c                   exsr      tippis
172400050408     c     §7jrps        ifne      'A'
172500050408     C* AGGIORNO FILE  FISGN SE C'E'
172600050408     C     KBOL2         CHAIN     FISGN03L                           33
172700050408     C     *IN33         DOWEQ     *OFF
172800050408     c* testo > 0 per sicurezza ma in teoria non DEVE succedere che sia = 0
172900050408     C     SGNST2        IFgt      0
173000050408     C                   sub       1             SGNST2
173100050408     C                   CLEAR                   SGNT6A
173200050408     C                   CLEAR                   SGNT6B
173300050408     C                   CLEAR                   SGNT6C
173400050408     C                   CLEAR                   SGNT6D
173500050408     C                   CLEAR                   SGNT6E
173600050408     C                   CLEAR                   SGNT6F
173700080623     c                   eval      sgndatora = %char(%timestamp:*iso0)
173800050408     C                   UPDATE    FISGN000
173900050408     C                   ENDIF
174000050408     C     KBOL2         READE     FISGN03L                               33
174100050408     C                   ENDDO
174200050408     C                   ENDIF
174300921019     C*
174400920424     C                   Z-ADD     3             B
174500941206     C*
174600970530     C     ENDUPD        ENDSR
174700970530     C**************************************************************************
174800970530     C* CONTROLLO IL TIPO PISTOLA DELLA SPUNTA CHE LEGGO
174900970530     C**************************************************************************
175000970530     C     TIPPIS        BEGSR
175100970530     C                   MOVEL     '7J'          COD
175200060512     C                   MOVEL(P)  knps          KEY
175300970530     C     KTAB          CHAIN     TABEL                              31
175400970530     C  N31              MOVEL     TBLUNI        DS7J
175500970530     C   31              CLEAR                   DS7J
175600970530     C*
175700970530     C                   ENDSR
175800961120     C**************************************************************************
175900970530     C* REPERISCO I DATI DEL FOGLIO DELLA SPUNTA LETTA
176000961120     C**************************************************************************
176100961120     C     CHAFV         BEGSR
176200961120     C                   Z-ADD     DATEU         DATAF
176300961120     C                   MOVEL     ' '           WFVV              1
176400961120     C                   MOVEL     ' '           WFGV              1
176500961120     C                   MOVEL     ' '           WFVA              1
176600970530     C                   CLEAR                   DEFF
176700970530     C                   CLEAR                   SPGF
176800970530     C                   CLEAR                   NFAF
176900970530     C                   CLEAR                   AEDF
177000970530     C                   CLEAR                   NFAFCF
177100970530     C                   CLEAR                   NFASPG
177200970530     C                   CLEAR                   NFAFGS
177300970530     C                   CLEAR                   NFADFV
177400970530     C                   CLEAR                   NFANPG
177500970530     C**
177600970530     C                   CLEAR                   DSLV53
177700040203     c                   if        brvnpg=1 and brvfgs=simfel
177800040203     c                   eval      d53tfo='G'
177900040203     c                   endif
178000040203     c                   if        brvnpg=1 and brvfgs<>simfel
178100040203     c                   eval      d53tfo='A'
178200040203     c                   endif
178300970529     C                   MOVEL     BRVNPG        D53NPG
178400000203     C                   Z-ADD     BRVNFV        D53NFV
178500970529     C                   MOVEL     BRVFGS        D53FGS
178600970529     C                   MOVEL     'A'           D53ABB
178700970529     C                   MOVEL     LNA           D53LNA
178800031114     C                   MOVEL     lnatfa        D53TFA
178900970529     C                   CALL      'FNLV53R'
179000970529     C                   PARM                    DSLV53
179100101202     c*
179200041014     c* Se errore e categoria "1" cerco in fgv, se cercato in fva
179300041014     c                   if        d53err<>*blanks and brvnpg=1 and d53tfo
179400041014     c                             ='A'
179500041014     C                   CLEAR                   DSLV53
179600041014     c                   eval      d53tfo='G'
179700041014     C                   MOVEL     BRVNPG        D53NPG
179800041014     C                   Z-ADD     BRVNFV        D53NFV
179900041014     C                   MOVEL     BRVFGS        D53FGS
180000041014     C                   MOVEL     SIMFEL        D53FEL
180100041014     C                   MOVEL     'A'           D53ABB
180200041014     C                   MOVEL     LNA           D53LNA
180300041014     C                   MOVEL     lnatfa        D53TFA
180400041014     C                   CALL      'FNLV53R'
180500041014     C                   PARM                    DSLV53
180600041014     c                   endif
180700970530     C*
180800970530     C* NESSUN ERRORE --> FOGLIO TROVATO
180900970530    1C     D53ERR        IFEQ      ' '
181000970529     C                   MOVEL     D53DFV        DATAF
181100970530     C                   MOVEL     D53SPG        SPGF
181200970530     C                   MOVEL     D53DEF        DEFF              1
181300970530     C                   MOVEL     D53NFA        NFAF
181400970530     C                   MOVEL     D53AED        AEDF
181500970530     C**
181600970530    2C     D53TFO        IFEQ      'G'
181700970530     C                   MOVEL     'S'           WFGV
181800970529     C                   ELSE
181900970530    3C     D53TFO        IFEQ      'V'
182000970530     C                   MOVEL     'S'           WFVV
182100970530   X3C                   ELSE
182200970530     C*
182300970530     C                   MOVEL     'S'           WFVA
182400970530     C* SE ABBINATO CONTROLLO IL FOGLIO ARRIVI PER VEDERE
182500970530     C*  SE ESISTE CHIUSO
182600970530    4C     D53NFA        IFGT      0
182700970620     C                   MOVEL     D53NFA        SAVNFA
182800970620     C                   MOVEL     D53NPA        SAVNPA
182900020917     c                   Movel     D53Lai        SavLai
183000970530     C                   CLEAR                   DSLV53
183100970620     C                   MOVEL     SAVNFA        D53NFV
183200970620     C                   MOVEL     SAVNPA        D53NPG
183300970530     C                   MOVEL     'V'           D53TFO
183400970530     C                   MOVEL     SIMFEL        D53FEL
183500970530     C                   MOVEL     SIMFEL        D53FLF
183600020917     c                   Movel     SavLai        D53Fgs
183700970530     C                   CALL      'FNLV53R'
183800970530     C                   PARM                    DSLV53
183900970530     C* TROVATO FOGLIO DI ABBINAMENTO --> SE ERRORE COME SE NON CI FOSS
184000970530    5C     D53ERR        IFEQ      ' '
184100970530     C                   MOVEL     D53FCF        NFAFCF
184200970530     C                   MOVEL     D53SPG        NFASPG
184300970530     C                   MOVEL     D53FGS        NFAFGS
184400970530     C                   MOVEL     D53DFV        NFADFV
184500970530     C                   MOVEL     D53NPG        NFANPG
184600970530     C                   ELSE
184700970530     C                   CLEAR                   NFAF
184800970530     C                   CLEAR                   AEDF
184900970530    5C                   ENDIF
185000970530    4C                   ENDIF
185100970530    3C                   ENDIF
185200970530    2C                   ENDIF
185300970530    1C                   ENDIF
185400961120     C*
185500961120     C                   ENDSR
185600960606     C**************************************************************************
185700960606     C* DETERMINO TERMINAL DI ARRIVO DI BRVFGS
185800960606     C**************************************************************************
185900960606     C     TERFGS        BEGSR
186000970530     C                   CLEAR                   DSLV55
186100970530     C                   MOVEL     LNPB          D55LNP
186200970530     C                   MOVEL     BRVFGS        D55LIN
186300970530     C                   MOVEL     DATAF         D55DRF
186400970530     C                   MOVEL     'A'           D55TPT
186500970530     C                   CALL      'FNLV55R'
186600970530     C                   PARM                    DSLV55
186700970530     C     D55ERR        IFEQ      ' '
186800970530     C                   MOVEL     D55TFA        WTFG
186900970530     C                   ELSE
187000970530     C                   MOVEL     BRVFGS        WTFG
187100970530     C                   ENDIF
187200960606     C*
187300960606     C                   ENDSR
187400941206     C**************************************************************************
187500941206     C* CONTROLLO DETTAGLIO FNBLT00F
187600941206     C**************************************************************************
187700921019     C     CTRBLT        BEGSR
187800941206     C*
187900930604     C                   MOVEL     *ALL'9'       BLPDSE
188000941206     C     KBLP2         SETLL     FNBLT01L
188100941206     C     KBLP2         READE     FNBLT01L                               34
188200921019     C*
188300921019     C     *IN34         DOWEQ     '0'
188400921021     C*
188500930604     C     BLTDSE        IFNE      0
188600921021     C     BLTDSE        ANDLT     BLPDSE
188700921021     C                   MOVEL     BLTDSE        BLPDSE
188800921021     C                   END
188900921021     C*
189000941206     C     KBLP2         READE     FNBLT01L                               34
189100921019     C                   END
189200921021     C*
189300930604     C     BLPDSE        IFEQ      *ALL'9'
189400921021     C                   Z-ADD     0             BLPDSE
189500921021     C                   END
189600941206     C*
189700921019     C                   ENDSR
189800941206     C**************************************************************************
189900941206     C* CONTROLLO SE ALTRI COLLI HANNO DATA ARRIVO E/O COD.ANOM
190000941206     C**************************************************************************
190100921019     C     CTRART        BEGSR
190200941206     C*
190300930603     C* ABILITAZIONI
190400930603     C                   MOVEL     ' '           NOAMP             1
190500930603     C                   MOVEL     ' '           NOAMA             1
190600930604     C  N20              MOVEL     *ALL'9'       ARBDAM
190700060512     c                   clear                   abbnsc
190800930604     C*
190900941206     C     KARB2         SETLL     FNART01L
191000941206     C     KARB2         READE     FNART01L                               34
191100921019     C*
191200921019     C* CONTROLLO SE ALTRI COLLI HANNO LA DATA ARRIVO
191300921019     C     *IN34         DOWEQ     '0'
191400930604     C*
191500930604     C     *IN20         IFEQ      '0'
191600930528     C* SE ALMENO UN COLLO HA ARTDAM = 0  DEVO AZZERARE ARBDTI E ARBHTI
191700930603     C* NOAMA = S NON E' ABILITATO IN BASE ALLA SPUNTA ARRIVI
191800930528     C     ARTDAM        IFEQ      0
191900930528     C                   Z-ADD     0             ARBDTI
192000930528     C                   Z-ADD     0             ARBHTI
192100930603     C                   MOVEL     'S'           NOAMA
192200930528     C                   END
192300921021     C*
192400930603     C     ARTDAM        IFGT      0
192500921021     C     ARTDAM        ANDLT     ARBDAM
192600921021     C                   Z-ADD     ARTDAM        ARBDAM
192700921021     C                   END
192800930603     C*
192900930603     C* NOAMP = S NON E' ABILITATO IN BASE ALLA SPUNTA PART(NON ABBINAT
193000941209     C* ARTABN='S'   ===>  SPUNTA DI FOGLIO ABBINATO
193100941209     C     ARTABN        IFNE      'S'
193200930603     C                   MOVEL     'S'           NOAMP
193300060512     c* Mewmorizzo il segnacollo con la data più alta e verifico se
193400060512     c*  il relativo foglio IDA è stato chiuso per decidere come
193500060512     c*  impostare arbama
193600060512     c                   else
193700060512     c                   if        artdfv>abbdfv  and artflp=0
193800060512     c                   eval      abbnsc=artnsc
193900060512     c                   eval      abbdfv=artdfv
194000060512     c                   endif
194100060512     c                   if        artdut>abbdfv  and artflp>0
194200060512     c                   eval      abbnsc=artnsc
194300060512     c                   eval      abbdfv=artdut
194400060512     c                   endif
194500930603     C                   END
194600930604     C                   END
194700921019     C*
194800921019     C* CONTROLLO SE ALTRI COLLI HANNO L'ANOMALIA
194900921019     C  N06ARTCAN        IFNE      ' '
195000921019     C                   SETON                                        06
195100930604     C   20              SETON                                        34
195200921019     C                   END
195300921019     C*
195400941206     C  N34KARB2         READE     FNART01L                               34
195500921019     C                   END
195600921021     C*
195700930604     C  N20ARBDAM        IFEQ      *ALL'9'
195800921021     C                   Z-ADD     0             ARBDAM
195900921021     C                   END
196000101202     c*
196100060512     c* Se devo abilitare in base alla sola spunta partenza, verifico
196200060512     c*  foglio IDA
196300060512    1c                   if        noama='S' and noamp=' '  and
196400060512     c* Escluso i locali
196500060512     c                             arbtfp<>arbtfa
196600060512     c                   eval      knpg=1
196700140618     c     kspu          setll     fnbrv05l
196800140618     c     kspu          reade     fnbrv05l
196900140618    2c                   dow       not %eof(fnbrv05l)
197000060512     C                   CLEAR                   DSLV53
197100060512     C                   MOVEL     'A'           D53TFO
197200060512     C                   MOVEL     BRVNPG        D53NPG
197300060512     C                   Z-ADD     BRVNFV        D53NFV
197400060512     C                   MOVEL     BRVFGS        D53FGS
197500060512     C                   MOVEL     'A'           D53ABB
197600060512     C                   MOVEL     LNA           D53LNA
197700060512     C                   MOVEL     lnatfa        D53TFA
197800060512     C                   CALL      'FNLV53R'
197900060512     C                   PARM                    DSLV53
198000060512     c
198100060512     c* Escludo la defluenza
198200060512    3c                   if        d53err=' '  and d53def=' '
198300060512    4c                   if        d53nfa>0
198400060512     c* chain
198500060512     c     kfvv          chain     fnfvv01l
198600060512    5c                   if        %found(fnfvv01l) and fvvfcf='S'
198700060512     c                   eval      noamp='0'
198800060512   x5c                   else
198900060512     c                   clear                   noamp
199000060512     c                   leave
199100060512    5c                   endif
199200101202     c*
199300060512   x4c                   else
199400060512     c                   clear                   noamp
199500060512     c                   leave
199600060512    4c                   endif
199700060512    3c                   endif
199800101202     c*
199900140618     c     kspu          reade     fnbrv05l
200000060512    2c                   enddo
200100060512    1c                   endif
200200921019     C*
200300921019     C                   ENDSR
200400941206     C**************************************************************************
200500941206     C*    CONTROLLO SE ALMENO UN COLLO HA LA DATA
200600941206     C**************************************************************************
200700921019     C     CTRBTT        BEGSR
200800941206     C*
200900941206     C                   MOVEL     *ALL'9'       BTPDET
201000031114     C     KBTP2         SETLL     FNBTT11L
201100031114     C     KBTP2         READE     FNBTT11L                               34
201200921019     C*
201300921019     C     *IN34         DOWEQ     '0'
201400921021     C*
201500941206     C     BTTDET        IFNE      0
201600941206     C     BTTDET        ANDLT     BTPDET
201700941206     C                   MOVEL     BTTDET        BTPDET
201800921021     C                   END
201900921019     C*
202000031114     C     KBTP2         READE     FNBTT11L                               34
202100921019     C                   END
202200921021     C*
202300941206     C     BTPDET        IFEQ      *ALL'9'
202400941206     C                   Z-ADD     0             BTPDET
202500921021     C                   END
202600921019     C*
202700921019     C                   ENDSR
202800960417     C**************************************************************************
202900960417     C*    CREAZIONE ANOMALIA 005
203000960417     C**************************************************************************
203100960417     C     CRTA05        BEGSR
203200961120     C* SE SI TRATTA DI SPUNTA LOCALE, CERCO IL FOGLIO ARRIVI
203300961120     C*  CON DATA BORDERO' + 1
203400961120     C     WLOCAL        IFEQ      'S'
203500970127     C* SE NON C'E' LA DATA BORDERO' èRENDO LA DATA DEL GIORNO
203600970127     C     WDBR          IFGT      0
203700961120     C                   MOVE      WDBR          G08INV
203800970127     C                   ELSE
203900970127     C                   MOVE      DATEU         G08INV
204000970127     C                   ENDIF
204100970127     C*
204200961120     C                   MOVEL     '3'           G08ERR
204300961120     C                   CALL      'XSRDA8'
204400961120     C                   PARM                    WLBDA8
204500961120     C*
204600961120     C*  DATA BORDERO' + 1
204700961120     C     G08TGI        ADD       1             GIOTGI
204800961120     C                   CALL      'XSRGI8'
204900961120     C                   PARM                    WGIDAT
205000961120     C*
205100961120     C                   Z-ADD     GIOINV        WDBR1
205200961120     C                   Z-ADD     2             KNPG
205300961120     C     KFVV3         SETLL     FNFVV02L
205400961120     C     KFVV3         READE     FNFVV02L                               31
205500961120     C     *IN31         DOWEQ     *OFF
205600020919     c     fvvatb        andne     ' '
205700961120     C     KFVV3         READE     FNFVV02L                               31
205800961120     C                   ENDDO
205900961120     C  N31              Z-ADD     FVVDFV        W0060             6 0
206000961120     C   31              CLEAR                   W0060
206100961120     C**
206200961120     C** IMPOSTO 1 PRIMI CAMPI
206300031114     C                   Z-ADD     lnatfa        WFGS
206400961120     C                   MOVEL     2             WNPG
206500961120     C                   MOVEL     ' '           WSPG
206600961120     C**
206700101202     C                   SELECT
206800101202     C     W0060         WHENEQ    0
206900961120     C                   Z-ADD     WDBR1         WDFV
207000101202     C                   OTHER
207100961120     C                   Z-ADD     W0060         WDFV
207200101202     C                   ENDSL
207300961120     C*
207400961120     C                   ENDIF
207500961120     C**
207600990518     C                   CLEAR                   FNANM000
207700960417     C                   MOVE      LNP           ANMFLS
207800960417     C                   MOVE      LNA           ANMLNA
207900960417     C                   MOVE      NRS           ANMNRS
208000960417     C                   MOVE      NSC           ANMSCN
208100960417     C                   MOVE      WAAS          ANMAAS
208200960417     C                   MOVE      WLNP          ANMLNP
208300960417     C                   MOVE      WNSP          ANMNSP
208400960417     C* Ricerco fase
208500960417     C                   CLEAR                   DSFASE
208600960417     C                   MOVE      WNPG          D11NPG
208700960417     C                   MOVE      WSPG          D11SPG
208800960417     C                   CALL      'TRUL11R'
208900960417     C                   PARM                    DSFASE
209000960417     C                   MOVE      D11FAS        ANMFSA
209100960417     C                   Z-ADD     WDFV          ANMDAO
209200960417     C                   MOVE      WNPS          ANMNPS
209300961120     C                   MOVE      WCDU          ANMCDU
209400990518     C                   Z-ADD     WNFV          ANMNFV
209500960417     C                   MOVEL     WFGS          ANMFLE
209600960417     C                   MOVE      SAVLNP        WFL2
209700960417     C     ANMLNP        IFGT      0
209800960417     C                   MOVEL     ANMLNP        WFL1
209900960417     C                   ELSE
210000960417     C                   MOVEL     ANMFLS        WFL1
210100960417     C                   ENDIF
210200960417     C                   Z-ADD     5             COMCAA
210300960417     C                   EXSR      CRTANM
210400960417     C                   ENDSR
210500960417     C**************************************************************************
210600960417     C*    CREAZIONE ANOMALIA 015
210700960417     C**************************************************************************
210800960417     C     CRTA15        BEGSR
210900990518     C                   CLEAR                   FNANM000
211000960417     C* Cerco foglio ima della data della spunta cancellata
211100960417     C                   EXSR      RCRIMA
211200960417     C*
211300961118     C     WCREA         IFEQ      ' '
211400960417     C                   MOVE      LNP           ANMFLS
211500960417     C                   MOVE      LNA           ANMLNA
211600960417     C                   MOVE      NRS           ANMNRS
211700960417     C                   MOVE      NSC           ANMSCN
211800960417     C                   MOVE      WAAS          ANMAAS
211900960417     C                   MOVE      WLNP          ANMLNP
212000960417     C                   MOVE      WNSP          ANMNSP
212100960417     C* Ricerco fase
212200960417     C                   CLEAR                   DSFASE
212300960417     C                   MOVE      WNPG          D11NPG
212400960417     C                   MOVE      WSPG          D11SPG
212500960417     C                   CALL      'TRUL11R'
212600960417     C                   PARM                    DSFASE
212700960417     C                   MOVE      D11FAS        ANMFSA
212800960417     C                   MOVE      WVVDFV        ANMDAO
212900960417     C                   MOVE      WFGS          ANMCDU
213000990518     C                   Z-ADD     WNFV          ANMNFV
213100960417     C                   MOVE      WFGS          ANMFLE
213200960417     C                   CLEAR                   WFL2
213300960417     C     ANMLNP        IFGT      0
213400960417     C                   MOVEL     ANMLNP        WFL1
213500960417     C                   ELSE
213600960417     C                   MOVEL     ANMFLS        WFL1
213700960417     C                   ENDIF
213800960417     C*
213900960417     C                   Z-ADD     15            COMCAA
214000960417     C                   EXSR      CRTANM
214100961118     C                   ENDIF
214200960417     C                   ENDSR
214300960416     C**************************************************************************
214400960416     C*    CREAZIONE ANOMALIA
214500960416     C**************************************************************************
214600960416     C     CRTANM        BEGSR
214700960416     C                   Z-ADD     COMCAA        ANMCAA
214800960416     C     COMCAA        IFNE      SAVCAA
214900960416     C                   MOVEL     '7C'          COD
215000960416     C                   MOVEL(P)  ANMCAA        KEY
215100960416     C     KTAB          CHAIN     TABEL                              31
215200960416     C  N31              MOVEL     TBLUNI        DS7C
215300960416     C   31              MOVEL     *BLANKS       DS7C
215400960416     C                   MOVEL     COMCAA        SAVCAA
215500960416     C                   ENDIF
215600050707     c* imposto in anmflo data e ora creazione spunta
215700050707     C                   TIME                    W0140            14 0
215800050707     c
215900050707     C                   CLEAR                   WLBDA8
216000050707     C                   MOVE      w0140         G08DAT
216100050707     C                   CALL      'XSRDA8'
216200050707     C                   PARM                    WLBDA8
216300050707     C                   MOVE      G08INV        wdat6             6 0
216400050707     c                   movel     w0140         wora4             4 0
216500041018     c                   movel     wdat6         anmflo
216600041018     c                   move      wora4         anmflo
216700041018     c* Imposto la zona se trovo la bolla
216800041018     c                   z-add     999           anmfl4
216900041018     c                   select
217000041018     c                   when      wesblt='S'
217100041018     c                   z-add     blpznc        anmfl4
217200041018     c                   when      wesart='S'
217300041018     c                   z-add     arbznc        anmfl4
217400041018     c                   when      wesbtt='S'
217500041018     c                   z-add     btpznc        anmfl4
217600041018     c                   endsl
217700041018     c* Imposto se di valore
217800041018     c                   if        anmnsp>0
217900041018     c     kar5          chain     fiar501l
218000041018    3c                   if        %found(fiar501l)
218100041018     c                   movel     ar5uni        dar5gen
218200041018     c                   else
218300041018     c                   clear                   dar5gen
218400041018    3c                   endif
218500041018     c                   movel     §ar5bva       anmft4
218600041018     c                   endif
218700960416     C*
218800960416     C                   MOVEL     §7CTAN        ANMTAN
218900960416     C                   MOVEL     §7CSAN        ANMSAN
219000960416     C                   MOVEL     §7CAIE        ANMAIE
219100960416     C                   Z-ADD     §7CLID        ANMLID
219200960416     C                   Z-ADD     0             ANMDCH
219300960416     C                   MOVEL     ' '           ANMFSC
219400070424     C                   clear                   ANMcch
219500960416     C* CHIUSURA AUTOMATICA
219600960416     C     §7CCHA        IFEQ      'S'
219700960417     C                   Z-ADD     ANMDAO        ANMDCH
219800960417     C                   MOVE      ANMFSA        ANMFSC
219900960416     C                   END
220000960416     C                   Z-ADD     0             ANMFL1
220100960416     C                   Z-ADD     0             ANMFL2
220200960416     C* ESTERNA
220300000905    1C     §7CAIE        IFEQ      'E'
220400960416     C* VEDO A CHI TRASMETTERE
220500960416     C                   MOVEL     WFL1          ANMFL1
220600960416     C                   MOVEL     WFL2          ANMFL2
220700960416     C* SE UGUALI CANCELLO IL 2°
220800000905    2C     ANMFL1        IFEQ      ANMFL2
220900960416     C                   Z-ADD     0             ANMFL2
221000000905   X2C                   ELSE
221100000905     C                   CLEAR                   DSLV55
221200000905     C                   MOVEL     'P'           D55TPT
221300000905     C                   MOVE      ANMFL1        D55LIN
221400000905     C                   MOVE      DATEU         D55DRF
221500000905     C                   CALL      'FNLV55R'
221600000905     C                   PARM                    DSLV55
221700000905    3C     D55ERR        IFNE      *BLANKS
221800000905     C                   MOVE      ANMFL1        WTFP1
221900000905     C                   ELSE
222000000905     C                   MOVE      D55TFP        WTFP1
222100000905    3C                   END
222200000905     C                   CLEAR                   DSLV55
222300000905     C                   MOVEL     'P'           D55TPT
222400000905     C                   MOVE      ANMFL2        D55LIN
222500000905     C                   MOVE      DATEU         D55DRF
222600000905     C                   CALL      'FNLV55R'
222700000905     C                   PARM                    DSLV55
222800000905    3C     D55ERR        IFNE      *BLANKS
222900000905     C                   MOVE      ANMFL2        WTFP2
223000000905     C                   ELSE
223100000905     C                   MOVE      D55TFP        WTFP2
223200000905    3C                   END
223300000905    3C     WTFP1         IFEQ      WTFP2
223400000905     C                   CLEAR                   ANMFL2
223500000905    3C                   END
223600000905    2C                   ENDIF
223700961120     C                   ENDIF
223800960416     C*
223900961129     C* SE ANOMALIA INTERNA CHE USA SIA FLE CHE LNA LA FLAGGO PER
224000961129     C* NON TRASMETTERLA
224100961129    2C     §7CAIE        IFEQ      'I'
224200961129    2C     §7CUEA        ANDEQ     'E'
224300961129     C                   MOVEL     'T'           ANMFT1
224400961129    2C                   ENDIF
224500961129     C*
224600990518     C                   WRITE     FNANM000
224700960416     C                   ENDSR
224800960417     C**************************************************************************
224900960417     C*    RICERCA FOGLIO IMA
225000960417     C**************************************************************************
225100960417     C     RCRIMA        BEGSR
225200960417     C* DETERMINO FILIALE GESTIONE DELLA LNA DELLA SPUNTA
225300960417     C                   CLEAR                   WFGS
225400960417     C                   CLEAR                   WNFV
225500960417     C                   CLEAR                   WNPG
225600960417     C                   CLEAR                   WSPG
225700020418      * controllo su azcae
225800020422     C     LNA           LOOKUP    L6S                                    30
225900020422     C     *IN30         IFEQ      *OFF
226000020422     C                   MOVE      LNA           WFGS
226100020422     C                   ELSE
226200160419     c                   clear                   dslv55
226300160419     c                   eval      d55tpt='6'
226400160419     c                   eval      d55lin=lna
226500160419     c                   eval      d55drf= *date
226600160419     C                   exsr      FNLV55
226700160419     c                   movel     d55tfa        wfgs
226800160419     c                   endif
226900101202      *
227000960417     C                   MOVEL     '3'           WNPG
227100960417     C* CHAIN SU FOGLIO IMA
227200960417     C     KFVV2         CHAIN     FNFVV02L                           30
227300160419     c                   dow       not *in30 and fvvspg<>'A'
227400160419     C     KFVV2         reade     FNFVV02L                               30
227500160419     c                   enddo
227600000203     C  N30              Z-ADD     FVVNFV        WNFV
227700960417     C  N30              MOVE      FVVNPG        WNPG
227800960417     C  N30              MOVE      FVVSPG        WSPG
227900961118     C**
228000961118     C* SE IL FOGLIO NON C'E' O E' APERTO, NON CREO L'ANOMALIA
228100961118     C                   CLEAR                   WCREA
228200961118     C     *IN30         IFEQ      *ON
228300961118     C     FVVFCF        ORNE      'S'
228400961118     C                   MOVEL     'N'           WCREA             1
228500961118     C                   ENDIF
228600960417     C                   ENDSR
228700960605     C**************************************************************************
228800960605     C*    RICERCA LNP BOLLA
228900960605     C**************************************************************************
229000960605     C     RICLNP        BEGSR
229100031114     c* La tabella 5F qui non viene cercata perchè non serve: mi basta
229200031114     c*  soltanto sapere se c'e' spunta partenza da altro terminal per
229300031114     c*  la creazione della anomalia 05 altrimenti questo pgm non crea
229400031114     c*  nulla: al massimo cancella
229500961120     C                   MOVE      1             KNPG
229600140618     C     KBRV3         SETLL     FNBRV05L
229700140618     C     KBRV3         READE     FNBRV05L                               32
229800031114    1C     *IN32         DOWEQ     *OFF
229900031114    2C     BRVATR        IFEQ      *BLANKS
230000031114    3C     BRVpes        IFNE      LNP
230100061120     C     BRVfgs        andne     pestfp
230200960621     C                   MOVE      BRVFGS        LNPB
230300031114     c                   eval      *in32=*on
230400031114    3C                   END
230500031114    2C                   END
230600140618     C  N32KBRV3         READE     FNBRV05L                               32
230700031114    1C                   ENDDO
230800960605     C                   ENDSR
230900970530     C**************************************************************************
231000970530     C     *INZSR        BEGSR
231100970530     C                   MOVEL     KPJBU         SAVJBU
231200970530     C                   Z-ADD     1             CODUT             1 0
231300970530     C* GIRO DATA DEL GIORNO
231400970530     C                   TIME                    W0140            14 0
231500970530     C                   MOVE      W0140         DATEU             8 0
231600970530     C                   CLEAR                   WLBDA8
231700970530     C                   MOVE      DATEU         G08DAT
231800970530     C                   CALL      'XSRDA8'
231900970530     C                   PARM                    WLBDA8
232000970530     C                   MOVE      G08INV        DATEU
232100970530     C* DEFINIZIONE CAMPI
232200031114     C     *LIKE         DEFINE    d53def        wvvdef
232300031114     C     *LIKE         DEFINE    fvvdfv        wdtrif
232400031114     C     *LIKE         DEFINE    fvvdfv        dspb
232500031114     C     *LIKE         DEFINE    fvvdfv        SVVDFV
232600031114     C     *LIKE         DEFINE    blpLNA        SVVLNA
232700031114     C     *LIKE         DEFINE    blpLNP        SVVLNP
232800031114     C     *LIKE         DEFINE    blpLNA        LNATFA
232900031114     C     *LIKE         DEFINE    blpLNA        LNATFP
233000031114     C     *LIKE         DEFINE    blpLNA        LNPTFP
233100031114     C     *LIKE         DEFINE    blpLNA        PESTFA
233200031114     C     *LIKE         DEFINE    blpLNA        PESTFP
233300000905     C     *LIKE         DEFINE    ANMLNP        WTFP1
233400000905     C     *LIKE         DEFINE    ANMLNP        WTFP2
233500000904     C     *LIKE         DEFINE    FVVNFV        SAVNFA
233600970620     C     *LIKE         DEFINE    FVVNPG        SAVNPA
233700020917     c     *Like         Define    FvvFgs        SavLai
233800970620     C     *LIKE         DEFINE    KPJBU         SAVJBU
233900970530     C     *LIKE         DEFINE    FVVSPG        NFASPG
234000970530     C     *LIKE         DEFINE    FVVFGS        NFAFGS
234100970530     C     *LIKE         DEFINE    FVVFCF        NFAFCF
234200970530     C     *LIKE         DEFINE    FVVDFV        NFADFV
234300970530     C     *LIKE         DEFINE    FVVNPG        NFANPG
234400970530     C     *LIKE         DEFINE    FVVSPG        SPGF
234500970530     C     *LIKE         DEFINE    FVVNFV        NFAF
234600970530     C     *LIKE         DEFINE    FVVDFV        AEDF
234700970530     C     *LIKE         DEFINE    FVVDFV        WVVDFV
234800970530     C     *LIKE         DEFINE    FVVSPG        WVVSPG
234900970530     C     *LIKE         DEFINE    FVVDFV        SPUDFV
235000970530     C     *LIKE         DEFINE    BRVNPS        SPUNPS
235100970530     C     *LIKE         DEFINE    FVVDFV        ENTDFV
235200970530     C     *LIKE         DEFINE    BRVNPS        ENTNPS
235300990412     C     *LIKE         DEFINE    FVVDFV        A56DFV
235400990412     C     *LIKE         DEFINE    BRVNFV        A56NFV
235500990412     C     *LIKE         DEFINE    BRVFGS        A56FGS
235600990412     C     *LIKE         DEFINE    BRVNPS        A56NPS
235700990412     C     *LIKE         DEFINE    FVVSPG        A56SPG
235800990412     C     *LIKE         DEFINE    FVVNPG        A56NPG
235900970530     C     *LIKE         DEFINE    BRVFGS        WTFG
236000970530     C     *LIKE         DEFINE    FVVDFV        DATAF
236100970530     C     *LIKE         DEFINE    FVVSPG        SPUSPG
236200970530     C     *LIKE         DEFINE    BRVNPG        SPUNPG
236300970530     C     *LIKE         DEFINE    BRVFGS        SPUFGS
236400990409     C     *LIKE         DEFINE    BRVFGS        ENTFGS
236500970530     C     *LIKE         DEFINE    BRVNFV        SPUNFV
236600970530     C     *LIKE         DEFINE    FVVFCF        WCHIUS
236700970530     C     *LIKE         DEFINE    ANMCAA        COMCAA
236800970530     C     *LIKE         DEFINE    ANMCAA        SAVCAA
236900970530     C     *LIKE         DEFINE    ANMFL1        WFL1
237000970530     C     *LIKE         DEFINE    ANMFL2        WFL2
237100970530     C     *LIKE         DEFINE    ARBAAS        WAAS
237200970530     C     *LIKE         DEFINE    ANMLNP        WLNP
237300970530     C     *LIKE         DEFINE    ANMNSP        WNSP
237400970530     C     *LIKE         DEFINE    BRVNPS        WNPS
237500000203     C     *LIKE         DEFINE    BRVNFV        WNFV
237600970530     C     *LIKE         DEFINE    ANMCDU        WCDU
237700970530     C     *LIKE         DEFINE    FVVDFV        WDFV
237800970530     C     *LIKE         DEFINE    FVVNPG        WNPG
237900970530     C     *LIKE         DEFINE    FVVSPG        WSPG
238000970530     C     *LIKE         DEFINE    FVVFGS        WFGS
238100970530     C     *LIKE         DEFINE    ARTDAM        WDAM
238200070424     C     *LIKE         DEFINE    ARTDET        WDET
238300070424     C     *LIKE         DEFINE    ARTFLP        WFLP
238400970530     C     *LIKE         DEFINE    ARTDCM        WDCM
238500970530     C     *LIKE         DEFINE    ARTDTR        WDTR
238600970530     C     *LIKE         DEFINE    ARBDBR        WDBR
238700970530     C     *LIKE         DEFINE    ARBFBC        WFBC
238800970530     C     *LIKE         DEFINE    ARBDBR        WDBR1
238900970530     C     *LIKE         DEFINE    FVVDFV        SAVDFV
239000970530     C     *LIKE         DEFINE    FVVFGS        SAVLNP
239100970604     C     *LIKE         DEFINE    ARTDAM        SAVDAM
239200970604     C     *LIKE         DEFINE    BTTDET        SAVDET
239300970530     C     *LIKE         DEFINE    LNP           LNPB
239400970530     C     *LIKE         DEFINE    BRVNPG        KNPG
239500970721     C     *LIKE         DEFINE    GCPFRG        KFRG
239600970721     C                   MOVEL     '0'           KFRG
239700970530     C**
239800031114     C     Kbar37        KLIST
239900041014     C                   KFLD                    BPES
240000031114     C                   KFLD                    LNP
240100031114     C                   KFLD                    LNA
240200031114     C                   KFLD                    NRS
240300031114     C                   KFLD                    NSC
240400041015     C     KFGV          KLIST
240500041015     C                   KFLD                    dis1nfv
240600041015     C                   KFLD                    dis1fgs
240700031114     C     KBOL2         KLIST
240800970530     C                   KFLD                    LNP
240900970530     C                   KFLD                    LNA
241000970530     C                   KFLD                    NRS
241100970530     C                   KFLD                    NSC
241200060512     C     Kspu          KLIST
241300060512     C                   KFLD                    knpg
241400060512     c                   KFLD                    LNP
241500060512     C                   KFLD                    LNA
241600060512     C                   KFLD                    NRS
241700060512     C                   KFLD                    abbnsc
241800970530     C     KARB2         KLIST
241900970530     C                   KFLD                    ARBAAS
242000970530     C                   KFLD                    ARBLNP
242100970530     C                   KFLD                    ARBNRS
242200970530     C                   KFLD                    ARBNSP
242300970721     C     KGCA          KLIST
242400970721     C                   KFLD                    ARBAAS
242500970721     C                   KFLD                    ARBLNP
242600970721     C                   KFLD                    ARBNRS
242700970721     C                   KFLD                    ARBNSP
242800970721     C                   KFLD                    KFRG
242900970530     C     KARB          KLIST
243000970530     C                   KFLD                    ARTAAS
243100970530     C                   KFLD                    ARTLNP
243200970530     C                   KFLD                    ARTNRS
243300970530     C                   KFLD                    ARTNSP
243400970530     C     KBTP2         KLIST
243500031114     C                   KFLD                    BTPflp
243600031114     C                   KFLD                    BTPAAS
243700970530     C                   KFLD                    BTPLNP
243800970530     C                   KFLD                    BTPNRS
243900970530     C                   KFLD                    BTPNSP
244000031114     C     KBTT          KLIST
244100031114     C                   KFLD                    BTTflp
244200031114     C                   KFLD                    BTTAAS
244300970530     C                   KFLD                    BTTLNP
244400970530     C                   KFLD                    BTTNRS
244500970530     C                   KFLD                    BTTNSP
244600970530     C     KBLP2         KLIST
244700970530     C                   KFLD                    BLPAAS
244800970530     C                   KFLD                    BLPLNP
244900970530     C                   KFLD                    BLPNRS
245000970530     C                   KFLD                    BLPNSP
245100031114     C     KBLT          KLIST
245200970530     C                   KFLD                    BLTAAS
245300970530     C                   KFLD                    BLTLNP
245400970530     C                   KFLD                    BLTNRS
245500970530     C                   KFLD                    BLTNSP
245600970530     C     KFVV2         KLIST
245700970530     C                   KFLD                    WNPG
245800970530     C                   KFLD                    WVVDFV
245900970530     C                   KFLD                    WFGS
246000060512     C     KFVV          KLIST
246100060512     C                   KFLD                    d53npa
246200060512     C                   KFLD                    d53nfa
246300060512     C                   KFLD                    d53lai
246400970530     C     KFVV3         KLIST
246500970530     C                   KFLD                    KNPG
246600970530     C                   KFLD                    WDBR1
246700031114     c                   Kfld                    lnatfa
246800970530     C     KTAB          KLIST
246900970530     C                   KFLD                    CODUT
247000970530     C                   KFLD                    COD               2
247100970530     C                   KFLD                    KEY               8
247200970530     C     KBRV3         KLIST
247300970530     C                   KFLD                    KNPG
247400970530     C                   KFLD                    LNP
247500970530     C                   KFLD                    LNA
247600970530     C                   KFLD                    NRS
247700970530     C                   KFLD                    NSC
247800970530     C     KAGB          KLIST
247900970530     C                   KFLD                    AGBTBO
248000970530     C                   KFLD                    AGBAAS
248100970530     C                   KFLD                    AGBLNP
248200970530     C                   KFLD                    AGBNRS
248300970530     C                   KFLD                    AGBNSP
248400970530     C                   KFLD                    AGBAGB
248500001128     C*
248600101202      *
248700030109     c     kFidta        Klist
248800030109     c                   Kfld                    SavAas
248900030109     c                   Kfld                    SavLnp1
249000030109     c                   Kfld                    SavNrs
249100030109     c                   Kfld                    SavNsp
249200030109     c                   Kfld                    SavTrd
249300030109     c                   Kfld                    SavPoc
249400041018     C     Kar5          KLIST
249500041018     C                   KFLD                    anmaas
249600041018     C                   KFLD                    anmLNp
249700041018     C                   KFLD                    anmNRS
249800041018     C                   KFLD                    anmnsp
249900041018     C                   KFLD                    Ktrd
250000970530     C*---------------------------------------------------------------*
250100970530     C*
250200970530     C*  Reperisco livello di chiusura anomalia come IDD
250300970530     C                   MOVEL(P)  2             KEY
250400970530     C                   MOVEL     '7A'          COD
250500970530     C     KTAB          CHAIN     TABEL                              31
250600970530     C  N31              MOVEL     TBLUNI        DS7A2
250700970530     C   31              CLEAR                   DS7A2
250800000825     C*
250900970530     C                   MOVEL     SAVJBU        KPJBU
251000970530     C                   ENDSR
251100970530     C**************************************************************************
251200970530     C     MEMDAT        BEGSR
251300041015     c                   clear                   wnoarrivo
251400041014     c* Faccio 3 memorizzazioni:
251500041014     c* 1) Spunta transito: solo se chi spara è p.o. spunta
251600041014    1c                   if        wesbtt='S' and brvpes=BPES   or
251700041014     c*    oppure spunta di disguido: idem
251800041018     c                             wesbtt<>'S' and wagpar=' ' and wagarr=' '
251900041014     c                             and brvpes=bpes
252000041014    2c                   if        tradfv=0 or spudfv<tradfv
252100970530     C                   MOVEL     BRVNPS        SPUNPS
252200970530     C                   MOVE      BRVNPG        SPUNPG
252300970530     C                   MOVE      BRVNFV        SPUNFV
252400970530     C                   MOVE      BRVFGS        SPUFGS
252500970530     C                   MOVEL     SPGF          SPUSPG
252600041014     C                   MOVEL     spudfv        tradfv
252700041014     C                   MOVEL     BRVNPS        traNPS
252800041014     C                   MOVE      BRVNPG        traNPG
252900041014     C                   MOVE      BRVFGS        traFGS
253000041014     C                   SETON                                        1918
253100041014    2c                   endif
253200041014    1c                   endif
253300041014     c* Per spunta transito o disguido memorizzo la spunta con data/ora cari
253400041014     c*  più alta, per aggiornare altro eventuale disguido/transito
253500041014    1c                   if        wesbtt='S' or
253600041014     c                             wagpar=' ' and wagarr=' '
253700041014     c* Non è cat.partenza non è ne del term.partenza  nè del term.arrivo
253800041015    2c                   if        brvnpg<>1 or  deff='S'
253900041014    3c                   if        wtfg<>lnatfa and brvfle<>lnptfp
254000041014    4c                   if        dis2dfv=0 or spudfv>=dis2dfv
254100041014     C                   MOVEL     BRVNPS        dis2nPS
254200041014     C                   MOVE      BRVNFV        dis2NFV
254300041014     C                   MOVE      BRVFGS        dis2FGS
254400041014     C                   MOVE      spudfv        dis2dfv
254500041015     c                   if        brvscar>0
254600041014     C                   MOVE      brvscar       dis2scar
254700041015     c                   else
254800041015     c                   move      brvimm        dis2scar
254900041015     c                   endif
255000041014    4c                   endif
255100041014    3c                   endif
255200041014    2c                   endif
255300101202     c*
255400041014    1c                   endif
255500101202     c*
255600041014     c* 2) Spunta partenza: il terminal di partenza spunta deve essere
255700041014     c*                     simfel
255800041015    1c                   if        wagpar='S' and brvfle=simfel
255900041015    2c                   if        prtdfv=0 or spudfv<prtdfv
256000041014     C                   MOVEL     BRVNPS        SPUNPS
256100041014     C                   MOVE      BRVNPG        SPUNPG
256200041014     C                   MOVE      BRVNFV        SPUNFV
256300041014     C                   MOVE      BRVFGS        SPUFGS
256400041014     C                   MOVEL     SPGF          SPUSPG
256500041014     C                   MOVEL     spudfv        prtdfv
256600041014     C                   MOVEL     BRVNPS        prtNPS
256700041014     C                   MOVE      BRVNPG        prtNPG
256800041014     C                   MOVE      BRVFGS        prtFGS
256900041014     C                   SETON                                        1917
257000041014    2c                   endif
257100041014     c* Per i danni, cerco la spunta con data più alta
257200041018    2c                   if        dandfv=0 or dandfv<=spudfv
257300041014     C                   MOVEL     BRVNPS        danNPS
257400041014     C                   MOVE      spudfv        dandFV
257500041014     C                   MOVE      BRVFGS        danFGS
257600041018     C                   MOVE      BRVnpg        dannpg
257700041018     C                   MOVE      spgf          danspg
257800041014    2c                   endif
257900041014     c* Se bolla locale, memorizzo a parte dati spunta entrata
258000041014    2c                   if        wsploc<>*blanks and entdfv=0 or
258100041014     c                             wsploc<>*blanks and spudfv<entdfv
258200041014    3c                   if        brvnpg=5 or
258300041014     c                             (brvnpg=3 and spgf='I') or
258400041014     c                             (brvnpg=3 and spgf='P') or
258500041014     c                             (brvnpg=1 and deff<>'S')
258600041014     C                   MOVEL     spudfv        ENTDFV
258700041014     C                   MOVE      BRVNPS        ENTNPS
258800041014     C                   MOVE      BRVFGS        ENTFGS
258900041015     c* non devo consdierare la spunta per l'aggiornamento in arrivo
259000041015     c                   eval      wnoarrivo='N'
259100041014    3c                   endif
259200041014    2c                   endif
259300101202     c*
259400041014    1c                   endif
259500101202     c*
259600041014     c* 2) Spunta arrivo  : il terminal di arrivo spunta deve essere
259700041014     c*                     term.arrivo lna spunta annullata
259800041015    1c                   if        wagarr='S' and brvfle=simfel  and
259900041015     c                             wnoarrivo<>'N'
260000041014    2c                   if        arrdfv=0 or spudfv<arrdfv
260100041014     C                   MOVEL     BRVNPS        SPUNPS
260200041014     C                   MOVE      BRVNPG        SPUNPG
260300041014     C                   MOVE      BRVNFV        SPUNFV
260400041014     C                   MOVE      BRVFGS        SPUFGS
260500041014     C                   MOVEL     SPGF          SPUSPG
260600041014     C                   MOVEL     spudfv        arrdfv
260700041014     C                   MOVEL     BRVNPS        arrNPS
260800041014     C                   MOVE      BRVNPG        arrNPG
260900041014     C                   MOVE      BRVFGS        arrFGS
261000101202     c*
261100050509    3c                   if        brvnpg=5 or
261200050509     c                             (brvnpg=3 and spgf='I') or
261300050509     c                             (brvnpg=3 and spgf='P') or
261400050509     c                             (brvnpg=1 and deff<>'S')
261500050509     c                   eval      arrfl2='P'
261600050509     c                   else
261700050509     c                   eval      arrfl2=' '
261800050509     c                   endif
261900041014     C                   SETON                                        1916
262000041014    2c                   endif
262100041014     c* Per i danni, cerco la spunta con data più alta
262200041018    2c                   if        dandfv=0 or dandfv<=spudfv
262300041014     C                   MOVEL     BRVNPS        danNPS
262400041014     C                   MOVE      spudfv        dandFV
262500041014     C                   MOVE      BRVFGS        danFGS
262600041018     C                   MOVE      BRVnpg        dannpg
262700041018     C                   MOVE      spgf          danspg
262800041014    2c                   endif
262900041014    1c                   endif
263000101202     c*
263100110908     C                   ENDSR
263200031114     C**************************************************************************
263300031114     C* TERMINAL DI PARTENZA E ARRIVO DEL P.O. EFFETTUA SPUNTA
263400031114     C**************************************************************************
263500031114     C     TERPES        BEGSR
263600031114     C* IMPOSTO TERMINAL PARTENZA E ARRIVO
263700031114     C                   CLEAR                   DSLV55
263800031114     C                   MOVEL     WTITER        D55TPT
263900031114     C                   MOVEL     LNPB          D55LNP
264000031114     C                   MOVEL     WDTRIF        D55DRF
264100031114     C                   MOVE      bpes          D55LIN
264200031114     C                   EXSR      FNLV55
264300031114     C                   ENDSR
264400031114     C**************************************************************************
264500031114     C* CERCO TERMINAL PARTENZA LNP COLLO
264600031114     C**************************************************************************
264700031114     C     TERPAR        BEGSR
264800031114     C                   CLEAR                   DSLV55
264900031114     C                   MOVEL     'P'           D55TPT
265000031114     C                   MOVEL     LNPB          D55LIN
265100031114     C                   MOVEL     WDTRIF        D55DRF
265200031114     C                   EXSR      FNLV55
265300031114     C                   MOVE      D55TFP        LNPTFP
265400031114     C**
265500031114     C                   ENDSR
265600031114     C**************************************************************************
265700031114     C* CONTROLLO COSA POSSO AGGIORNARE IN BASE A CHI SONO
265800031114     C**************************************************************************
265900031114     C     CTRAGG        BEGSR
266000031114     C                   CLEAR                   WAGPAR
266100031114     C                   CLEAR                   WAGARR
266200031114     C**
266300031114     C     LNPB          CHAIN     AZORG01L                           34
266400031114     C**
266500031114     C** CERCO TERMINAL DELLA LINEA DI ARRIVO
266600031114     C                   EXSR      TERARR
266700031114     C* FACCIO PARTENZA DELLA STESSA AREA PARTENZA SE:
266800041013    1C***  BAS           IFEQ      LNPAS
266900031126     c* Se c'e'bolla transito x simfel è transito e non cerco altro
267000041013     c     wesbtt        ifne      'S'
267100031114     C*
267200031114     C*  1) P.O. EFFETTUA SPUNTA = LNP SPUNTA (DA BOLLA)
267300031114    2C     BPES          IFEQ      LNPB
267400031114     C*  2) P.O. EFFETTUA SPUNTA E' IL TER.PARTENZA DELLA LNP SPUNTA
267500031114     C*     IN BASE ALLA DATA SPEDIZIONE SE TROVATA LA BOLLA
267600031114     C     BPES          OREQ      LNPTFP
267700031114     C                   MOVEL     'S'           WAGPAR            1
267800031114   X2C                   ELSE
267900031114     C*  3) TER.PARTENZA P.O. EFFETTUA SPUNTA (IN BASE ALLA DATA SPED
268000031114     C*     SE TROVATA LA BOLLA) = TER.PARTENZA LNP SPUNTA SEMPRE IN
268100031114     C*     BASE ALLA DATA SPEDIZIONE SE TROVATA BOLLA
268200031114     C                   MOVEL     'P'           WTITER            1
268300031114     C                   EXSR      TERPES
268400031114    3C     D55TFP        IFEQ      LNPTFP
268500031114     C                   MOVEL     'S'           WAGPAR            1
268600031114    3C                   ENDIF
268700031114    2C                   ENDIF
268800041013    1C                   ENDIF
268900031114     C**
269000031114     C** NON AGGIORNO BOLLE ARRIVO SE SPUNTA DELL'AREA PARTENZA,
269100031114     C*   SPUNTA TIPICA PARTENZA E P.O. SPUNTA = LNA SPUNTA O TFA SPU
269200031114     c* controllo solo se spunta tipica partenza: l'arrivo non mi
269300031114     c*  interessa. non so perchè lo avevo preso in considerazione!!
269400031114    1C     WAGPAR        IFEQ      'S'
269500031114     C* PARTENZA NON DEFLUENZA
269600031114    3C     parnpg        IFEQ      1
269700031114     C     WVVDEF        ANDEQ     '0'
269800031114     C* ENTRATA
269900031114     C     parnpg        OREQ      5
270000031114     C* I.M.P.
270100031114     C     parnpg        OREQ      3
270200031114     C     wvvspg        ANDEQ     'P'
270300031114     C* I.M.I.
270400031114     C     parnpg        OREQ      3
270500031114     C     wvvspg        ANDEQ     'I'
270600031114     C                   GOTO      NOAGAR
270700031114    3C                   ENDIF
270800031114    2C**                 ENDIF
270900031114    1C                   ENDIF
271000031114     C**
271100031114     c     wesbtt        ifne      'S'
271200031114     C* AGGIORNO BOLLE ARRIVO SE :
271300031114     C*  1) P.O. EFFETTUA SPUNTA = LNA SPUNTA
271400031114    1C                   SELECT
271500031114     C     BPES          WHENEQ    lna
271600031114     C                   MOVEL     'S'           WAGARR            1
271700031114    2C     WAGPAR        IFEQ      'S'
271800031114     C                   MOVEL     'A'           WSPLOC            1
271900031114    2C                   ENDIF
272000031114     C*  2) P.O. EFFETTUA SPUNTA E' IL TER.ARRIVO   DELLA LNA SPUNTA
272100031114     C*     IN BASE ALLA DATA SPUNTA
272200031114     C     BPES          WHENEQ    LNATFA
272300031114     C                   MOVEL     'S'           WAGARR            1
272400031114    2C     WAGPAR        IFEQ      'S'
272500031114     C                   MOVEL     'T'           WSPLOC            1
272600031114    2C                   ENDIF
272700031114   X1C                   OTHER
272800031114     C*  3) TER.ARRIVO P.O. EFFETTUA SPUNTA
272900031114     C*     = TER.ARRIVO LNA SPUNTA IN BASE ALLA DATA SPUNTA
273000031114    2C     PESTFA        IFEQ      LNATFA
273100031114     C                   MOVEL     'S'           WAGARR            1
273200041014    2C     WAGPAR        IFEQ      'S'
273300031114     C                   MOVEL     'D'           WSPLOC
273400031114    2C                   ENDIF
273500041014    2C                   ENDIF
273600031114    1C                   ENDSL
273700031114     c*
273800031114    2C                   endif
273900031114     C     NOAGAR        TAG
274000031114     C* DEVO DETERMINARE SE LE BOLLE DELLA LNA SI TROVANO SULL'AS
274100031114     C*  IN CUI MI TROVO
274200041013     C**   LNAAS         IFEQ      BAS
274300041014     C**                 MOVEL     'S'           WSIBOL            1
274400041013     C**                 ENDIF
274500031114     C**
274600031114     C                   ENDSR
274700031114     C**************************************************************************
274800031114     C* CERCO TERMINAL ARRIVO LNA COLLO
274900031114     C**************************************************************************
275000031114     C     TERARR        BEGSR
275100031114     C**
275200031114     C* LINEA DI ARRIVO
275300031114     C     lna           IFNE      SVVLNA
275400031114     C     LNPB          ORNE      SVVLNP
275500031114     C     wvvdfv        ORNE      SVVDFV
275600031114     C*
275700031114     C* TEMINAL DI ARRIVO
275800031114     C                   CLEAR                   DSLV55
275900031114     C                   MOVEL     ' '           D55TPT
276000031114     C                   MOVEL     wvvdfv        D55DRF
276100031114     C                   MOVEL     lna           D55LIN
276200031114     C                   MOVEL     LNPB          D55LNP
276300031114     C                   EXSR      FNLV55
276400031114     C**
276500031114     C                   MOVEL     D55TFA        LNATFA
276600031114     C                   MOVEL     D55TFP        LNATFP
276700031114     C*
276800031114     C                   MOVEL     lna           SVVLNA
276900031114     C                   MOVEL     lnPB          SVVLNP
277000031114     C                   MOVEL     wvvdfv        SVVDFV
277100031114     C                   ENDIF
277200031114     C                   ENDSR
277300031114     C**************************************************************************
277400031114     C* CALL A PGM FNLV55R
277500031114     C**************************************************************************
277600031114     C     FNLV55        BEGSR
277700031114     C                   CALL      'FNLV55R'
277800031114     C                   PARM                    DSLV55
277900031114     C**
278000031114     C     D55ERR        IFNE      ' '
278100031114     C                   MOVE      D55LIN        D55TFA
278200031114     C                   MOVE      D55LIN        D55TFP
278300031114     C                   END
278400031114     C                   ENDSR
