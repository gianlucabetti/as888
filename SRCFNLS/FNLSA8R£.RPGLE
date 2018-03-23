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
006900990518     D FNANM2          DS
007000960416     D  ANMNR2               397    400B 0
007100960416     D WLBDA8          DS
007200960416     D  G08DAT                 1      8  0
007300960416     D  G08INV                 9     16  0
007400960416     D  G08ERR                17     17
007500960416     D  G08TGI                18     22  0
007600961120     D WGIDAT          DS                  INZ
007700961120     D  GIODAT                 1      8  0
007800961120     D  GIOINV                 9     16  0
007900961120     D  GIOTGI                17     21  0
008000041015     d
008100041015     D  FFV                    1    897
008200041015     D                                     DIM(299)
008300041015    +D  FGVFFV                 1    240
008400041015    +D  FGVFF2               241    447
008500041015    +D  fgwff3               448    687
008600041015    +D  fgwff4               688    897
008700041015     D                 DS
008800041015    +D  FLP                    1    897
008900041015     D                                     DIM(299)
009000041015    +D  FGVFLP                 1    240
009100041015    +D  FGVFL2               241    447
009200041015    +D  fgwfl3               448    687
009300041015    +D  fgwfl4               688    897
009400960417     D DSUL06        E DS                  EXTNAME(TRUL06DS)
009500960417     D  LIN                    1     90  0
009600960417     D                                     DIM(30)
009700960417     D DSFASE          DS
009800960417     D  D11TLA                 1      1
009900960417     D  D11NPG                 2      2
010000960417     D  D11SPG                 3      3
010100960417     D  D11FAS                 4      4
010200031114     D                 DS
010300031114     D  BLPAAS                 1      4  0
010400031114     D  BLPMGS                 5      8  0
010500031114     D  BLPDSP                 1      8  0
010600031114     D                 DS
010700031114     D  ARBAAS                 1      4  0
010800031114     D  ARBMGS                 5      8  0
010900031114     D  ARBDSP                 1      8  0
011000031114     D                 DS
011100031114     D  BTPAAS                 1      4  0
011200031114     D  BTPMGS                 5      8  0
011300031114     D  BTPDSP                 1      8  0
011400041014     D                 DS
011500041014     D  brvdcs
011600041014     D  brvhcs
011700070124     D  brvscar                1     14  0
011800041015     D                 DS
011900041015     D  brvdfs
012000041015     D  brvhms
012100070124     D  brvimm                 1     14  0
012200020418
012300041018     D ktrd            s                   like(ar5trd)  inz('GEN')
012400060512     d Knps            s                   like(brvnps)
012500030109
012600030109     d SavAas          s                   Like(DtaAas)
012700030109     d SavLnp1         s                   Like(DtaLnp)
012800030109     d SavNrs          s                   Like(DtaNrs)
012900030109     d SavNsp          s                   Like(DtaNsp)
013000030109     d SavPoc          s                   Like(DtaPoc)
013100030109     d SavTrd          s                   Like(DtaTrd)
013200030115     d Sav_ArbDam      s                   Like(ArbDam)
013300030115     d Sav_BtpDet      s                   Like(BtpDet)
013400030109     d wFidta          s              1    Inz('0')
013500031127     d wnfaf           s                   like(nfaf)
013600041015     d tradfv          s                   like(d53dfv)
013700041015     d tranps          s                   like(brvnps)
013800041015     d tranpg          s                   like(brvnpg)
013900041015     d trafgs          s                   like(brvfgs)
014000041015     d prtdfv          s                   like(d53dfv)
014100041015     d prtnps          s                   like(brvnps)
014200041015     d prtnpg          s                   like(brvnpg)
014300041015     d prtfgs          s                   like(brvfgs)
014400041015     d arrdfv          s                   like(d53dfv)
014500050509     d arrfl2          s                   like(artfl2)
014600041015     d arrnps          s                   like(brvnps)
014700041015     d arrnpg          s                   like(brvnpg)
014800041015     d arrfgs          s                   like(brvfgs)
014900041015     d dis1dfv         s                   like(d53dfv)
015000041015     d dis1nps         s                   like(brvnps)
015100041015     d dis1nfv         s                   like(d53nfv)
015200041015     d dis1fgs         s                   like(brvfgs)
015300041015     d dis1scar        s                   like(brvscar)
015400041015     d dis1atr         s                   like(brvatr)
015500041015     d dis2dfv         s                   like(d53dfv)
015600041015     d dis2nps         s                   like(brvnps)
015700041015     d dis2nfv         s                   like(d53nfv)
015800041015     d dis2fgs         s                   like(brvfgs)
015900041015     d dis2scar        s                   like(brvscar)
016000041015     d danfgs          s                   like(brvfgs)
016100041015     d dandfv          s                   like(d53dfv)
016200041015     d dannps          s                   like(brvnps)
016300041018     d dannpg          s                   like(brvnpg)
016400041018     d danspg          s                   like(fvvspg)
016500041015     d wnoarrivo       s              1    Inz(' ')
016600041018     d wokanm15        s              1    Inz(' ')
016700060512     d abbnsc          s                   like(artnsc)
016800060512     d abbdfv          s                   like(artdfv)
016900110908     d A_115dfv        s                   like(d53dfv)
017000110908     d A_115fgs        s                   like(brvfgs)
017100110908     d A_115npg        s                   like(brvnpg)
017200110908     d A_115spg        s                   like(fvvspg)
017300110908
017400070424     d DSRECANM      e DS                  extname(fnanm00F)
017500070424     d savRECANM     e DS                  extname(fnanm00F) prefix(S_)
017600030109
017700000000     C*---------------------------------------------------------------*
017800921014     C* RIEPILOGO INDICATORI
017900921014     C*---------------------------------------------------------------*
018000921014     C* 01    - LANCIO PGM ABILITAZIONE MERCE
018100921019     C* 06    - ELIMINO ANOMALIA
018200921019     C* 09    - ELIMINO DATA ENTRATA O DATA ARRIVO
018300041014     C* 16    - ESISTE altra spunta per arrivo
018400041014     C* 17    - ESISTE altra spunta per partenza
018500041014     C* 18    - ESISTE altra spunta per transito
018600041014     C* 19    - ESISTE IN ARCHIVIO UN'ALTRA SPUNTA DEL COLLO
018700041014     C* 20    -
018800921014     C* 32/34 - DI COMODO
018900921014     C* 37    - NON ESISTE BOLLA TRANSITO
019000921014     C* 39    - RECORD ALLOCATO
019100000825     C* 43    - CONTROLLO SU AZORG SE LINEA PARTENZA E' POSTE E SE
019200000825      *         POSSO CREARE ANOMALIE
019300921014     C*---------------------------------------------------------------*
019400020723     C     *ENTRY        PLIST
019500020723     C                   PARM                    KPJBA
019600020723     C                   MOVEL     KPJBU         PARAM
019700020723     c
019800020723     C***
019900020723     C* CARICO TUTTE LE  FILIALI GESTITE DA ALTRE (DA £6)
020000020723     c*   solo una volta
020100020723     C***
020200020723     c                   if        not *in95
020300020723     C                   CLEAR                   DSUL06
020400020723     C                   MOVE      '£6'          D06COD
020500020723     c                   eval      d06esc = 'G'
020600020723     c                   movel     simfel        d06key
020700020723     C                   MOVEL     DSUL06        KPJBU
020800020723     C                   CALL      'TRUL06R'
020900020723     C                   PARM                    KPJBA
021000020723     C                   MOVEL     KPJBU         DSUL06
021100020723     C                   MOVEA     LIN           L6S
021200020723     c                   seton                                        95
021300020723     c                   endif
021400900618     C*---------------------------------------------------------------*
021500921020     C* FLG = 1 PASSO IL VECCHIO RECORD DA ELIMINARE
021600921020     C* FLG = 3 ELIMINAZ.SOLO DEL CODICE ANOMALIA (NON DELETO ANOMALIE)
021700930609     C* FLG = 'C' CHIUDO PGM CON LR E BASTA
021800900619     C*
021900930609     C     FLG           IFEQ      'C'
022000970530     C                   MOVEL     'C'           D55TLA
022100970530     C                   CALL      'FNLV55R'
022200970530     C                   PARM                    DSLV55
022300970530     C                   MOVEL     'C'           D53TLA
022400970530     C                   CALL      'FNLV53R'
022500970530     C                   PARM                    DSLV53
022600970604     C                   MOVEL     'C'           SA2TLA
022700970604     C                   CALL      'FNLSA2R3'
022800970604     C                   PARM                    SA2TLA
022900970604     C                   PARM                    SA2TRC
023000970604     C                   PARM                    SA2DTS
023100970604     C                   PARM                    SA2LNA
023200970530     C**
023300930609     C                   SETON                                        LR
023400930609     C*
023500930609     C                   ELSE
023600930609     C*
023700911016     C* IMPOSTAZIONI INIZIALI
023800911014     C                   EXSR      CARTAB
023900921019     C*
024000921020     C                   Z-ADD     1             B                 1 0
024100920424     C*
024200920424     C* SE TROVO RECORD ALLOCATI RITENTO 3 VOLTE
024300920424     C     B             DOWLE     3
024400031114     C                   EXSR      UPDbol
024500920424     C                   ADD       1             B
024600920424     C                   END
024700921019     C*
024800921019     C     B             IFEQ      3
024900921019     C                   MOVEL     'E'           FLG
025000921019     C                   END
025100911014     C*
025200921020     C                   MOVEL     PARAM         KPJBU
025300930609     C*
025400930609     C                   RETURN
025500930609     C                   END
025600941206     C**************************************************************************
025700941206     C*    CARICO TABELLE
025800941206     C**************************************************************************
025900911014     C     CARTAB        BEGSR
026000970530     C**
026100970529     C                   CLEAR                   DSLV53
026200970529     C                   MOVEL     PARNPG        D53NPG
026300970529     C                   MOVEL     PARNFV        D53NFV
026400970529     C                   MOVEL     PARFGS        D53FGS
026500970530     C                   MOVEL     PARFLF        D53FLF
026600970529     C                   MOVEL     SIMFEL        D53FEL
026700970529     C                   CALL      'FNLV53R'
026800970529     C                   PARM                    DSLV53
026900970529     C     D53ERR        IFEQ      ' '
027000970529     C                   MOVEL     D53SPG        WVVSPG
027100970529     C                   MOVE      D53DFV        WVVDFV
027200031114     C                   MOVE      D53DEF        WVVDEF
027300970529     C                   ELSE
027400970529     C                   MOVE      DATEU         WVVDFV
027500970616     C                   CLEAR                   WVVSPG
027600031114     C                   CLEAR                   WVVDEF
027700961120    1C                   END
027800911112     C*
027900921020     C* FASE DEL FOGLIO DELLA SPUNTA CHE ELABORO
028000960405     C                   MOVEL(P)  PARNPG        KEY
028100960405     C                   MOVEL     '7N'          COD
028200921019     C     KTAB          CHAIN     TABEL                              31
028300921019     C  N31              MOVEL     TBLUNI        DS7N
028400921021     C*
028500921021     C                   MOVEL     §7NFS1        §FS(1)
028600921021     C                   MOVEL     §7NFS2        §FS(2)
028700921021     C                   MOVEL     §7NFS3        §FS(3)
028800921021     C                   MOVEL     §7NFS4        §FS(4)
028900921021     C                   MOVEL     §7NFS5        §FS(5)
029000921021     C*
029100921021     C                   MOVEL     §7NSC1        §SC(1)
029200921021     C                   MOVEL     §7NSC2        §SC(2)
029300921021     C                   MOVEL     §7NSC3        §SC(3)
029400921021     C                   MOVEL     §7NSC4        §SC(4)
029500921021     C                   MOVEL     §7NSC5        §SC(5)
029600921020     C*
029700930604     C                   Z-ADD     1             I                 3 0
029800970616     C     WVVSPG        LOOKUP    §SC(I)                                 31
029900921020     C   31              MOVEL     §FS(I)        FASE              1
030000921020     C  N31              MOVEL     §FS(1)        FASE
030100060512     c*
030200060512     c* Vedo se la pistola è manuale
030300060512     c
030400060512     c                   MOVEL     nps           knps
030500060512     c                   exsr      TIPPIS
030600060512     c
030700060512     c                   movel     §7jrps        spurps            1
030800060512     c*
030900060512     c
031000921019     C*
031100911014     C                   ENDSR
031200941206     C**************************************************************************
031300911018     C*--- AGGIORNO RECORD BOLLE ARRIVI O CREO ANOMALIA --------------*
031400921020     C* ANNULLO DATA ARRIVO SULLE BOLLE SE:
031500921020     C*  1) NON CI SOLO ALTRE SPUNTE DEL COLLO
031600921020     C*  2) SE LA DATA ARRIVO CORRISPONDE ALLA DATA FOGLIO DELLA SPUNTA
031700921021     C*
031800921021     C* MODIFICO DATA DI ARRIVO SULLE BOLLE E COLLO :
031900921021     C*  1) SE ANNULLO LA SPUNTA CHE MI HA IMPOSTATO LA DATA DI ARRIVO
032000921021     C*     E CI METTO LA DATA FOGLIO DI UN'ALTRA SPUNTA DI QUEL COLLO
032100921020     C*
032200921020     C* ANNULLO ANOMALIA    SULLE BOLLE SE:
032300921020     C*  1) NON C'E' NESSUN'ALTRA SPUNTA CON UNA ANOMALIA
032400921020     C*  2) SE L'ANOMALIA SPUNTA E' = ALL'ANOMALIA SUL COLLO
032500941206     C**************************************************************************
032600031114     C     UPDBOL        BEGSR
032700941230     C*
032800070424     c                   clear                   AggioDTI          1
032900921019     C                   MOVEL     ' '           SAVCAN
033000941206     C                   MOVEL     *ZEROS        SPUDFV
033100941206     C                   MOVEL     *ZEROS        SPUNPS
033200960416     C                   MOVEL     *ZEROS        SPUNPG
033300960417     C                   MOVEL     *ZEROS        SPUSPG
033400960416     C                   MOVEL     *ZEROS        SPUNFV
033500960513     C                   MOVEL     *ZEROS        SPUFGS
033600961121     C                   MOVEL     *ZEROS        ENTDFV
033700961121     C                   MOVEL     *ZEROS        ENTNPS
033800990409     C                   MOVEL     *ZEROS        ENTFGS
033900990412     C                   MOVEL     *ZEROS        A56DFV
034000990412     C                   MOVEL     *ZEROS        A56NPS
034100990412     C                   MOVEL     *ZEROS        A56FGS
034200990412     C                   MOVEL     *ZEROS        A56NFV
034300990412     C                   MOVEL     *BLANKS       A56SPG
034400990412     C                   MOVEL     *BLANKS       A56NPG
034500960416     C                   CLEAR                   SAVCAA
034600960416     C                   CLEAR                   WAAS
034700960416     C                   CLEAR                   WLNP
034800960416     C                   CLEAR                   WNSP
034900960417     C                   CLEAR                   WDAM
035000960417     C                   CLEAR                   WDCM
035100961120     C                   CLEAR                   WDBR
035200961120     C                   CLEAR                   WFBC
035300101202     c*
035400960613     C                   CLEAR                   LNPB
035500031114     C                   CLEAR                   WESART
035600031114     C                   CLEAR                   WESBLT
035700031114     C                   CLEAR                   WESBTT
035800041015     C                   MOVEL     *ZEROS        tradfv
035900041015     C                   MOVEL     *ZEROS        tranps
036000041015     C                   MOVEL     *ZEROS        tranpg
036100041015     C                   MOVEL     *ZEROS        trafgs
036200041015     C                   MOVEL     *ZEROS        prtdfv
036300041015     C                   MOVEL     *ZEROS        prtnps
036400041015     C                   MOVEL     *ZEROS        prtnpg
036500041015     C                   MOVEL     *ZEROS        prtfgs
036600041015     C                   MOVEL     *ZEROS        arrdfv
036700050509     c                   clear                   arrfl2
036800041015     C                   MOVEL     *ZEROS        arrnps
036900041015     C                   MOVEL     *ZEROS        arrnpg
037000041015     C                   MOVEL     *ZEROS        arrfgs
037100041015     C                   MOVEL     *ZEROS        dis1nps
037200041015     C                   MOVEL     *ZEROS        dis1nfv
037300041015     C                   MOVEL     *ZEROS        dis1fgs
037400041015     C                   MOVEL     *ZEROS        dis1dfv
037500041015     C                   MOVEL     *ZEROS        dis1scar
037600041015     C                   MOVEL     *ZEROS        dis1atr
037700041015     C                   MOVEL     *ZEROS        dis2nps
037800041015     C                   MOVEL     *ZEROS        dis2nfv
037900041015     c                   MOVEL     *ZEROS        dis2fgs
038000041015     C                   MOVEL     *ZEROS        dis2dfv
038100041015     C                   MOVEL     *ZEROS        dis2scar
038200041015     C                   MOVEL     *ZEROS        dannps
038300041015     C                   MOVEL     *ZEROS        dandfv
038400041015     C                   MOVEL     *ZEROS        danfgs
038500041018     C                   MOVEL     *ZEROS        dannpg
038600041018     C                   MOVEL     '  '          danspg
038700110908     c
038800110908     c                   clear                   a_115dfv
038900110908     C                   MOVEL     *ZEROS        a_115fgs
039000110908     C                   MOVEL     *ZEROS        a_115npg
039100110908     C                   MOVEL     '  '          a_115spg
039200930604     C                   SETOFF                                       061920
039300041015     C                   SETOFF                                       161718
039400031114     c*
039500031114     C* TERMINAL partenza DI BPES
039600031114     C                   MOVEL     'P'           WTITER            1
039700031114     C                   MOVEL     wvvdfv        WDTRIF
039800031114     C                   EXSR      TERPES
039900031114     C                   Z-ADD     D55TFP        PESTFP
040000031114     c*
040100031114     c* Prima di tutto verifico chi sono come spunta e quindi cosa posso
040200031114     c*  aggiornare ...
040300031114     c*
040400031126     c* Se trovo la bolla transito la considero sempre buona e non
040500031114     c*  controllo i terminal (c'e' un problema a livello di eccezioni ter
040600031114     C     KBAR37        CHAIN     FNBTT37L                           3739
040700031114     C   39              GOTO      ENDUPD
040800031114     C  N37KBTT          CHAIN     FNBTP11L                           37
040900031114     C**
041000031114    1C     *IN37         IFEQ      *OFF
041100031114     C                   MOVEL     'S'           WESBTT            1
041200031114     C                   MOVEL     BTTLNP        LNPB
041300031114     C                   MOVEL     BTPDSP        DSPB
041400031114     C                   MOVEL     BTPDSP        WDTRIF
041500031114     C                   MOVEL     BTPTFP        LNPTFP
041600031114   X1C                   ELSE
041700031114     C                   MOVEL     'N'           WESBTT
041800031114     C**
041900031114     C** VEDO SE TROVO LA BOLLA IN BLT
042000041216     C     KBOL2         CHAIN(N)  FNBLT27L                           3239
042100031114     C   39              GOTO      ENDUPD
042200031114     C  N32KBLT          CHAIN(N)  FNBLP01L                           32
042300031114     C**
042400031114    2C     *IN32         IFEQ      *OFF
042500031114     C                   MOVEL     'S'           WESBLT            1
042600031114     C                   MOVEL     BLTLNP        LNPB
042700031114     C                   MOVEL     BLPTFP        LNPTFP
042800031114     C                   MOVEL     BLPDSP        WDTRIF
042900031114     C                   MOVEL     BLPDSP        DSPB
043000031114   X2C                   ELSE
043100031114     C                   MOVEL     'N'           WESBLT
043200031114     C** SE NON TROVO LA BOLLA PARTENZA CERCO NEGLI ARRIVI
043300031114     C     KBOL2         CHAIN     FNART27L                           3239
043400031114     C   39              GOTO      ENDUPD
043500031114     C  N32KARB          CHAIN     FNARB01L                           32
043600031114     C**
043700031114    3C     *IN32         IFEQ      *OFF
043800031114     C                   MOVEL     'S'           WESART            1
043900031114     C                   MOVEL     ARTLNP        LNPB
044000031114     C                   MOVEL     ARBDSP        DSPB
044100031114     C                   MOVEL     ARBDSP        WDTRIF
044200031114     C                   MOVEL     ARBTFP        LNPTFP
044300031114   X3C                   ELSE
044400031114     C                   MOVEL     'N'           WESART
044500031114     C** IMPOSTO LNPB = BARLNP
044600031114     C                   MOVEL     lnp           LNPB
044700031114     C                   Z-ADD     wvvdfv        DSPB
044800031114     C**
044900150819     C** SE NON TROVATA LA BOLLA, CERCO SE ESISTE L' EVENTUALE
045000031126     C*   SPUNTA PARTENZA
045100031114     C                   EXSR      RICLNP
045200150819     c
045300031114     c* prendo terminal di partenza della linea partenza calcolata
045400031114     C                   Z-ADD     wvvdfv        WDTRIF
045500031114     C                   EXSR      TERPAR
045600031114    3C                   ENDIF
045700031114    2C                   ENDIF
045800031114    1C                   ENDIF
045900031114     C**
046000031114     C** CONTROLLO TRA P.O. SPUNTA E LINEE SPUNTE CHE AGGIONAMENTI
046100031114     C**  POSSO FARE
046200031114     c*
046300150819     C* TERMINAL arrivo   DI BPES
046400031114     C                   MOVEL     'A'           WTITER
046500031114     C                   MOVEL     wvvdfv        WDTRIF
046600031114     C                   EXSR      TERPES
046700031114     C                   Z-ADD     D55TFA        PESTFA
046800031114     c
046900031114     C                   EXSR      CTRAGG
047000960416     C*
047100960416     C                   CLEAR                   WCHIUS
047200960416     C                   CLEAR                   WDFV
047300960416     C                   CLEAR                   WNPS
047400960416     C                   CLEAR                   WNFV
047500960416     C                   CLEAR                   WNPG
047600960417     C                   CLEAR                   WSPG
047700960416     C                   CLEAR                   WFGS
047800961120     C                   CLEAR                   WCDU
047900960416     C                   CLEAR                   WSPUN
048000031114     C                   CLEAR                   WLOCAL
048100960417     C                   CLEAR                   SAVDFV
048200960417     C                   CLEAR                   SAVLNP
048300970127     C                   CLEAR                   WGIAC
048400930604     C**
048500070124     C     KBOL2         SETLL     FNBRV07L
048600070124     C     KBOL2         READE     FNBRV07L                               32
048700930604     C*
048800970625    1C     *IN32         DOWEQ     '0'
048900961120     C* LA SPUNTA LETTA NON DEVE ESSERE QUELLA CHE STO ANNULLANDO
049000970625    2C     BRVNPG        IFNE      PARNPG
049100950118     C     BRVNFV        ORNE      PARNFV
049200950118     C     BRVFGS        ORNE      PARFGS
049300961120     C**
049400041013     C* CODICE ANOMALIA COLLO  tengo l'ultima che trovo
049500041013    3C     brvCAN        IFne      ' '
049600950118     C                   MOVEL     BRVCAN        SAVCAN            1
049700961118    3C                   END
049800970625     C**
049900970625     C* SOLO SE FLAG ='1' FACCIO GLI ALTRI CONTROLLI RELATIVI ALLE
050000970625     C* SPUNTE
050100970625    3C     FLG           IFEQ      '1'
050200961120     C**
050300961120     C* CHAINO I FOGLI A SECONDA DELLA CATEGORIA E FIL.GESTIONE
050400961120     C                   EXSR      CHAFV
050500970530     C**
050600970530     C* CONTROLLO IL TIPO PISTOLA DELLA SPUNTA LETTA
050700060512     c                   movel     brvnps        knps
050800970530     C                   EXSR      TIPPIS
050900110908     c
051000110908     c* Verifico se presente altra spunta reale sul sistema per non
051100110908     c*  riaprire  eventuale anomalia 115
051200110908    4C     §7JRPS        IFNE      'A'
051300110908     c                   eval      a_115dfv=dataf
051400110908     C                   MOVE      BRVFGS        a_115FGS
051500110908     C                   MOVE      BRVnpg        a_115npg
051600110908     C                   MOVE      spgf          a_115spg
051700110908     c                   endif
051800110908     c
051900041015     c* Per spunta transito o disguido memorizzo la spunta con data/ora cari
052000041015     c*  più alta, per aggiornare altro eventuale disguido/transito
052100041015     c                   if        d53err=' '  or d53err='6'
052200041015    4c                   if        wesbtt='S' or
052300041015     c                             wagpar=' ' and wagarr=' '
052400041015     c* Categoria partenza
052500041015    5c                   if        brvnpg=1 and deff=' '
052600041015    6c                   if        dis1dfv=0 or dataf>=dis1dfv
052700041015     C                   MOVEL     BRVNPS        dis1nPS
052800041015     C                   MOVE      BRVNFV        dis1NFV
052900041015     C                   MOVE      BRVFGS        dis1FGS
053000041015     C                   MOVE      dataf         dis1dfv
053100041015     c                   if        brvscar>0
053200041015     C                   MOVE      brvscar       dis1scar
053300041015     c                   else
053400041015     c                   move      brvimm        dis1scar
053500041015     c                   endif
053600041015     C                   MOVE      brvatr        dis1atr
053700041015    6c                   endif
053800041015    5c                   endif
053900041015    4c                   endif
054000041015    4c                   endif
054100101202     c*
054200041015     C* CONSIDERO SOLO SE NON C'E' ERRORE
054300041015    4C     D53ERR        IFEQ      ' '
054400960606     C*
054500961120     C*  ESCLUDO LE SPUNTE AUTOMATICHE CHE NON POSSONO AGGIORNARE LA
054600961120     C*  DATA DI ARRIVO
054700970625    5C     §7JRPS        IFNE      'A'
054800970530     C* CONSIDERO COMUNQUE LA PISTOLA AUTOMATICA DEI TRANSITI PERCHE'
054900970530     C*  AGGIORNA LA DATA DI ARRIVO TRANSITO
055000970530     C     BRVNPS        OREQ      89
055100961120     C*
055200961120     C* DETERMINO TERMINAL DI ARRIVO DELLA FILIALE GESTIONE SPUNTA LETT
055300961120     C                   EXSR      TERFGS
055400961120     C*
055500031126     C* SE DEVO AGGIORNARE L'ARRIVO: CONSIDERO ANCHE LE SPUNTE DEL TERMINAL
055600031126     C*   ALTRIMENTI  CONSIDERO SOLO LE SPUNTE DI SIMFEL
055700031126    6C                   IF        BRVFLE=SIMFEL  OR
055800031126     C                             WAGARR='S' AND WTFG  =LNATFA
055900031126     c* FOGLI VARI
056000970625    7C     WFVV          IFEQ      'S'
056100961120     C*
056200041014     C* SE TROVATA SPUNTA vedo se memorizzare i dati
056300970530     C                   MOVEL     DATAF         SPUDFV
056400970530     C                   EXSR      MEMDAT
056500990412     C**
056600990412     C** MEMORIZZO SE C'E' UN'ALTRA SPUNTA IMA/IMG/CONSEGNE
056700990412     C** PER ANOMALIA DI DISGUIDO
056800990412    8C     BRVNPG        IFEQ      3
056900990412     C     NFASPG        ANDEQ     'A'
057000990412     C     BRVNPG        OREQ      3
057100990412     C     NFASPG        ANDEQ     'G'
057200990412     C     BRVNPG        OREQ      4
057300990412     C                   MOVEL     DATAF         A56DFV
057400990412     C                   MOVE      BRVNPS        A56NPS
057500990412     C                   MOVE      BRVFGS        A56FGS
057600990412     C                   MOVE      BRVNPG        A56NPG
057700990412     C                   MOVE      BRVNFV        A56NFV
057800990412     C                   MOVE      NFASPG        A56SPG
057900990412    8C                   ENDIF
058000041015     c*
058100041015     c                   else
058200041015     c* La spunta partenza la consdiero per le partenze e transiti
058300041015     c* solo se non è autogenerata
058400041015     c                   if        brvscar>0 and wagarr<>'S'
058500041015     C                   MOVEL     DATAF         SPUDFV
058600041015     C                   EXSR      MEMDAT
058700990412    7C                   ENDIF
058800041015    7C                   ENDIF
058900961120     C**
059000961120     C* FOGLI PARTENZA LOCALE
059100970530     C* CONSIDERO SOLO SE SI TRATTA DI  DEFLUENZA
059200970625    7C     WFGV          IFEQ      'S'
059300970530     C     DEFF          ANDEQ     'S'
059400970530     C* SE TROVATA SPUNTA MEMORIZZO ANCHE GLI ALTRI DATI
059500970530     C                   MOVEL     DATAF         SPUDFV
059600970530     C                   EXSR      MEMDAT
059700970625    7C                   ENDIF
059800961120     C**
059900970625    6C                   END
060000970625    5C                   END
060100960417     C*
060200960417     C* SE SPUNTA PARTENZA MEMORIZZO QUELLA CON DATA PIù RECENTE
060300960417     C* E VERIFICO SE RELATIVA A FV ABBINATO A FOGLIO ARRIVI GIà CHIUSO
060400961120     C*  PERCHE' DEVO CREARE L'ANOMALIA 05-COLLO PARTITO NON ARRIVATO
060500041014    5C     *IN16         IFEQ      *OFF
060600031126     c     wagarr        andeq     'S'
060700961120     C     BRVNPG        ANDEQ     1
060800970530     C     DEFF          ANDNE     'S'
060900961118     C**
061000970625    6C     WFVA          IFEQ      'S'
061100970530     C     DATAF         ANDGT     SAVDFV
061200031127     c* rimemorizzo anche se trovo il foglio gisuto abbinato,
061300031127     C*  In caso di transito infatti su As unico leggo anche la spunta part
061400031127     c*  originale non diretta all'arrivo. Il foglio lo trova ma mi da non
061500031127     c*  abbinato (xchè abbinato al transito e non all'arrivo)
061600031127     c*  quindi se poi leggo il foglio giusto lo devo memorizzare
061700031127     c*  Se troviamo soluzione migliore...meglo!!
061800031127    6C     WFVA          oreq      'S'
061900031127     C     DATAF         ANDle     SAVDFV
062000031127     c     wnfaf         andeq     0
062100031127     c     nfaf          andgt     0
062200970530     C                   MOVE      DATAF         SAVDFV
062300000904     C                   MOVE      BRVFGS        SAVLNP
062400960417     C                   MOVE      BRVNPS        WNPS
062500961120     C                   MOVEL     ' '           WLOCAL            1
062600031127     C                   z-add     nfaf          Wnfaf
062700961120     C**
062800970530     C* SE FVP ABBINATO ED HO TROVATO FOGLIO ARRIVI, MEMORIZZO  I DATI
062900970625    7C     NFAF          IFGT      0
063000970530     C     NFAFCF        ANDNE     *BLANKS
063100960417     C                   MOVE      'S'           WCHIUS
063200970530     C                   MOVE      BRVNFV        WNFV
063300970530     C                   MOVE      BRVFGS        WCDU
063400970530     C                   MOVE      NFADFV        WDFV
063500970530     C                   MOVE      NFANPG        WNPG
063600970530     C                   MOVE      NFASPG        WSPG
063700970530     C                   MOVE      NFAFGS        WFGS
063800970625   X7C                   ELSE
063900960417     C                   CLEAR                   WCHIUS
064000970625    7C                   END
064100970625    6C                   END
064200961118     C**
064300961118     C** FOGLIO LOCALE: COME SE FOSSE SEMPRE ABBINATO E F.ARRIVI CHIUSO
064400031114    6C     wagarr        IFEQ      'S'
064500970530     C     WFGV          ANDEQ     'S'
064600970530     C     DATAF         ANDGT     SAVDFV
064700970530     C                   MOVE      DATAF         SAVDFV
064800970530     C                   MOVE      BRVFGS        SAVLNP
064900961120     C                   MOVE      BRVNPS        WNPS
065000970530     C                   MOVEL     'S'           WLOCAL            1
065100970530     C*
065200970530     C                   MOVE      'S'           WCHIUS
065300970530     C                   MOVE      BRVNFV        WNFV
065400961120     C                   MOVE      SIMFEL        WCDU
065500961120     C**
065600970625    6C                   ENDIF
065700970625    5C                   ENDIF
065800961118     C**
065900961118     C** CONTROLLO LE SPUNTE AI FINI IMA:
066000961118     C**  -SE TROVO UNA SPUNTA CON DATA> DELLA DATA SPUNTA CHE CANCELLO
066100961118     C**   SICURAMENTE NON DEVO CREARE ANOMALIA COLLO MANCANTE
066200961120     C**  -SE TROVO UNA SPUNTA CON DATA = ALLA DATA SPUNTA CHE CANCELLO
066300961118     C**   CONSIDERO VALIDE SOLO LE CATEGORIA 3  4 E 5
066400961118     C**  - ESCUDO SEMPRE LE SPUNTE CATEGORIA 1 PERCHE' NON SONO CONSID
066500961118     C**    NEMMENO DAL PGM DELL'IMA
066600031114    5C     wagarr        IFEQ      'S'
066700961120     C     WFVV          ANDEQ     'S'
066800961118     C*
066900970625    6C     DATAF         IFGT      WVVDFV
067000960417     C                   MOVE      '1'           WSPUN             1
067100970625    6C                   ENDIF
067200961118     C*
067300970625    6C     DATAF         IFEQ      WVVDFV
067400970625    7C     BRVNPG        IFEQ      3
067500961118     C     BRVNPG        OREQ      4
067600961118     C     BRVNPG        OREQ      5
067700041013     C     BRVNPG        OREQ      8
067800961118     C                   MOVE      '1'           WSPUN             1
067900970625    7C                   ENDIF
068000970625    6C                   ENDIF
068100961118     C*
068200970625    5C                   END
068300970625    4C                   END
068400970625    3C                   END
068500970625    2C                   END
068600930604     C*
068700070124     C     KBOL2         READE     FNBRV07L                               32
068800970625    1C                   ENDDO
068900031114     C**
069000031114     C** A G G I O R N A M E N T O   B O L L E   P A R T E N Z A
069100031114     C**
069200031114     c** elimino data su collo
069300031114    1C     wagpar        IFEQ      'S'
069400941230     C     KBOL2         CHAIN     FNBLT27L                           3239
069500990409     C*
069600031114    2C     *IN39         IFEQ      *OFF
069700990409     C     KBOL2         CHAIN     FNDCD02L                           3839
069800060512     c* La pistola è corrispondente se uguali, o entrambe manuali
069900060512   2ac                   if        not *in38 and not *in39
070000060512     c                   movel     dcdnps        knps
070100060512     c                   exsr      TIPPIS
070200101202     c*
070300060512     C     DCDNPS        IFEQ      NPS
070400990409     C     DCDDFV        ANDEQ     WVVDFV
070500060512     C     spurps        oreq      'M'
070600060512     C     §7jrps        andeq     spurps
070700990409     C                   MOVEL     'S'           WDCD              1
070800990409     C                   ELSE
070900990409     C                   UNLOCK    FNDCD02L
071000031114    3C                   ENDIF
071100060512   2aC                   ENDIF
071200031114    2C                   ENDIF
071300921013     C*
071400921013     C   39              GOTO      ENDUPD
071500101202     c*
071600031203     C* se c'e' la bolla aggiorno
071700031203   1ac                   if        not *in32
071800970530     C                   MOVE      BLTAAS        WAAS
071900970530     C                   MOVE      BLTLNP        WLNP
072000970530     C                   MOVE      BLTNSP        WNSP
072100920810     C*
072200970530    2C     SAVCAN        IFEQ      ' '
072300921020     C     CAN           ANDEQ     BLTCAN
072400921020     C                   MOVEL     ' '           BLTCAN
072500961121    2C                   END
072600921019     C*
072700101202     c*
072800970530     C* CANCELLO A SOSTITUISCO LA DATA ENTRATA SOLO SE CORRISPONDONO
072900970530     C*  PISTOLA E DATA
073000970530    2C     FLG           IFEQ      '1'
073100060512     c                   movel     bltnpe        knps
073200060512     c                   exsr      TIPPIS
073300060512   2aC     BLTNPE        ifeq      NPS
073400970530     C     BLTDSE        ANDEQ     WVVDFV
073500060512     c* oppure se sono entrambe manuali
073600060512     c     spurps        oreq      'M'
073700060512     c     §7jrps        andeq     spurps
073800970530     C*
073900961121     C* SE NON TROVATA NESSUNA SPUNTA, NEMMENO ENTRATA PULISCO LE DATE
074000041014     c                   select
074100041014    3C     *IN17         wheneq    *OFF
074200961121     C     ENTDFV        ANDEQ     0
074300041014     C     prtDFV        ANDEQ     0
074400970530     C                   MOVEL     *ZEROS        BLTDSE
074500970530     C                   MOVEL     *ZEROS        BLTNPE
074600060512     C                   MOVEL     ' '           BLTCAN
074700961121     C* SE COLLO LOCALE E
074800961121     C*  TROVATA SOLO SPUNTA ENTRATA O <= ALL'ALTRA SPUNTA TROVATA, USO
074900041014    3C     *IN17         wheneq    *Off
075000970530     C     ENTDFV        ORGT      0
075100041014     C     ENTDFV        ANDLE     prtDFV
075200961121     C                   MOVEL     ENTDFV        BLTDSE
075300961121     C                   MOVEL     ENTNPS        BLTNPE
075400041014   X3C                   other
075500961121     C*
075600961121     C* AGGIORNO SOLO CON L'ALTRA SPUNTA TROVATA O ENTRATA SE NO LOCALE
075700041014     C                   MOVEL     prtDFV        BLTDSE
075800041014     C                   MOVEL     prtNPS        BLTNPE
075900041014    3C                   ENDsl
076000060512   2aC                   ELSE
076100060512     c* 20 on - non aggiorno la data
076200060512     C                   SETON                                        20
076300060512   2aC                   ENDIF
076400961121     C**
076500961121   X2C                   ELSE
076600041014     c* 20 on - non aggiorno la data
076700930604     C                   SETON                                        20
076800961121    2C                   ENDIF
076900990409     C* DCD
077000990409    2C     FLG           IFEQ      '1'
077100990409     C     WDCD          ANDEQ     'S'
077200990409     C*
077300990409     C* SE NON TROVATA NESSUNA SPUNTA, NEMMENO ENTRATA PULISCO LE DATE
077400041014     C     danDFV        ifeq      0
077500990409     C                   MOVEL     *ZEROS        DCDDFV
077600990409     C                   MOVEL     *ZEROS        DCDFGS
077700990409     C                   MOVEL     *ZEROS        DCDNPS
077800990409     C                   CLEAR                   DCDFAR
077900990409   X3C                   ELSE
078000150819     C* Imposto la spunta con data più altra se>=alla data presente
078100041014     C     danDFV        ifge      dcddfv
078200041014     C                   MOVEL     danDFV        DCDDFV
078300041014     C                   MOVEL     danNPS        DCDNPS
078400041014     C                   MOVEL     danFGS        DCDFGS
078500041014     c                   else
078600041014     C                   MOVEL     *ZEROS        DCDDFV
078700041014     C                   MOVEL     *ZEROS        DCDFGS
078800041014     C                   MOVEL     *ZEROS        DCDNPS
078900041014     C                   CLEAR                   DCDFAR
079000041014   X4C                   endif
079100041014   X4C                   endif
079200101202     c*
079300990409     C                   UPDATE    FNDCD000
079400990409    2C                   ENDIF
079500921020     C*
079600970530     C                   UPDATE    FNBLT000
079700921019     C*
079800970530    2C     FLG           IFEQ      '1'
079900930604     C     *IN20         ANDEQ     '0'
080000930604     C*
080100031114     C     KBLT          CHAIN     FNBLP01L                           3339
080200150819     c* Visto che una bolla DPD eveva la data BLPDSE impostata ma non era
080300150819     c*  presente nel dettaglio colli, e visto che il pgm gestisce la eliminaz.
080400150819     c*  l'unico caso che mi viene in mente è che la bolla era allocata.
080500150819     c*  in sto caso scrivo FNAGB
080600150819     c                   if        *in39
080700150819     c                   clear                   fnagb000
080800150819     C                   MOVEL     'P'           AGBTBO
080900150819     C                   MOVEL     'DT'          AGBAGB
081000150819     C                   Z-ADD     blTAAS        AGBAAS
081100150819     C                   Z-ADD     blTLNP        AGBLNP
081200150819     C                   Z-ADD     blTNRS        AGBNRS
081300150819     C                   Z-ADD     blTNSP        AGBNSP
081400150819     C     KAGB          SETLL     FNAGB01L                               31
081500150819     C  N31              WRITE     FNAGB000
081600150819     c
081700150819     C                   GOTO      ENDUPD
081800150819     c                   endif
081900921019     C*
082000930604     C* CONTROLLO DETTAGLIO PER IMPOSTARE 1° DATA ENTRATA
082100001206     C  N20
082200001206     CANN33              EXSR      CTRBLT
082300921230     C*
082400941206     C  N33              UPDATE    FNBLP000
082500031203    2C                   END
082600031203   1aC                   END
082700921019     C*
082800970530    1C                   END
082900041216     c                   unlock    fnblt27l
083000031114     C**
083100031114     C* A G G I O R N O   B O L L E   A R R I V I
083200921013     C**
083300031114    1C     wagarr        IFEQ      'S'
083400941206     C                   SETOFF                                           20
083500941230     C     KBOL2         CHAIN     FNART27L                           3239
083600990409     C                   CLEAR                   WDCD
083700990409     C*
083800031114    2C     *IN39         IFEQ      *OFF
083900041014     c
084000060512     c* Solo se non ho già aggiornato da partenza
084100041015    3c                   if        wsploc=*blanks
084200990409     C     KBOL2         CHAIN     FNDCD02L                           3839
084300060710   3ac                   if        not *in38 and not *in39
084400060512     c                   movel     dcdnps        knps
084500060512     c                   exsr      TIPPIS
084600101202     C*
084700060512    4C     DCDNPS        IFEQ      NPS
084800990409     C     DCDDFV        ANDEQ     WVVDFV
084900060512     C     spurps        oreq      'M'
085000060512     C     §7jrps        andeq     spurps
085100990409     C                   MOVEL     'S'           WDCD              1
085200990409     C                   ELSE
085300990409     C                   UNLOCK    FNDCD02L
085400041015    4C                   ENDIF
085500060512   3aC                   ENDIF
085600041015    3C                   ENDIF
085700031114    2C                   ENDIF
085800920424     C*
085900921013     C   39              GOTO      ENDUPD
086000031203     c
086100041014     c* Solo se esiste la bolla
086200031203   1ac                   if        not *in32
086300990409     C* AGGIORNO FNDCD
086400990409    2C     FLG           IFEQ      '1'
086500990409     C     WDCD          ANDEQ     'S'
086600990409     C*
086700041014    4C                   IF        dandfv=0
086800990409     C                   CLEAR                   DCDFGS
086900990409     C                   CLEAR                   DCDNPS
087000990409     C                   CLEAR                   DCDDFV
087100990409     C                   CLEAR                   DCDFAR
087200990409   X4C                   ELSE
087300041014     C* ESISTE UN'ALTRA SPUNTA PER IL COLLO con data >= di quella annullata
087400041015    5c                   if        dandfv>=dcddfv
087500041014     C                   MOVEL     danDFV        DCDDFV
087600041014     C                   MOVEL     danNPS        DCDNPS
087700041015   x5C                   MOVEL     danFGS        DCDFGS
087800041014     c                   else
087900041014     C                   CLEAR                   DCDFGS
088000041014     C                   CLEAR                   DCDNPS
088100041014     C                   CLEAR                   DCDDFV
088200041014     C                   CLEAR                   DCDFAR
088300041015    5C                   ENDIF
088400041015    4C                   ENDIF
088500101202     c*
088600990409     C                   UPDATE    FNDCD000
088700990409    2C                   ENDIF
088800990409     C*
088900961129    2C     FLG           IFEQ      '1'
089000060512     c                   movel     artnap        knps
089100060512     c                   exsr      TIPPIS
089200101202     c*
089300060512   2aC     ARTNAP        ifeq      NPS
089400970530     C     ARTDAM        ANDEQ     WVVDFV
089500060512     c* Oppure se sono entrambe manuali
089600060512     c     spurps        oreq      'M'
089700060512     c     §7jrps        andeq     spurps
089800101202     c*
089900970604     C* MEMORIZZO LA DATA DI ARRIVO PRIMA DI MODIFICARLA
090000970604     C                   MOVE      ARTDAM        SAVDAM
090100970530     C**
090200960613     C* NON ESISTONO ALTRE SPUNTE PER IL COLLO
090300041014    3C     *IN16         IFEQ      *OFF
090400961129    4C     ARTDCM        IFGT      *ZEROS
090500960613     C                   MOVE      ARTDCM        ARTDAM
090600960613     C                   Z-ADD     85            ARTNAP
090700960613     C                   ELSE
090800960613     C                   Z-ADD     0             ARTDAM
090900960613     C                   Z-ADD     0             ARTNAP
091000050509     c                   clear                   artfl2
091100060512     c                   clear                   artcan
091200060512     C                   SETOff                                       06
091300961129    4C                   END
091400970530   X3C                   ELSE
091500960613     C* ESISTE UN'ALTRA SPUNTA PER IL COLLO
091600041014     C                   MOVEL     arrDFV        ARTDAM
091700041014     C                   MOVEL     arrNPS        ARTNAP
091800050509     C                   MOVEL     arrFL2        ARTFL2
091900970530    3C                   END
092000970530     C* SCRIVO LA STATISTICA PER RIELABORARE LA NUOVA DATA CHE HO
092100970604     C*  SCRITTO E LA DATA CHE C'ERA PRIMA
092200970604     C     ARTDAM        IFNE      SAVDAM
092300070424     c* Memorizzo che devo riaggiornare data arrivo ultimo collo
092400070424     c                   eval      AggioDTI='S'
092500970604     C* VECCHIA DATA
092600970604     C                   CLEAR                   SA2TLA
092700970604     C                   MOVEL     'A'           SA2TRC
092800970604     C                   Z-ADD     SAVDAM        SA2DTS
092900970604     C                   MOVE      *ZEROS        SA2LNA
093000970604     C                   CALL      'FNLSA2R3'
093100970604     C                   PARM                    SA2TLA            1
093200970604     C                   PARM                    SA2TRC            1
093300970604     C                   PARM                    SA2DTS            8 0
093400970604     C                   PARM                    SA2LNA            3 0
093500970604     C* NUOVA DATA
093600970604     C     ARTDAM        IFGT      *ZEROS
093700970604     C                   Z-ADD     ARTDAM        SA2DTS
093800970604     C                   CALL      'FNLSA2R3'
093900970604     C                   PARM                    SA2TLA            1
094000970604     C                   PARM                    SA2TRC            1
094100970604     C                   PARM                    SA2DTS            8 0
094200970604     C                   PARM                    SA2LNA            3 0
094300970604     C                   END
094400970604     C                   END
094500970625     C**
094600970625     C* LA SPUNTA ANNULLATA NON E' QUELLA CHE HA AGGIORNATO IL DETTAGL
094700060512   2aC                   ELSE
094800930604     C                   SETON                                        20
094900060512   2aC                   ENDIF
095000060512     C**
095100060512     C* LA SPUNTA ANNULLATA NON E' QUELLA CHE HA AGGIORNATO IL DETTAGL
095200060512   X2C                   ELSE
095300060512     C                   SETON                                        20
095400060512    2C                   ENDIF
095500970625     C*
095600961129    2C     SAVCAN        IFEQ      ' '
095700921020     C     ARTCAN        ANDEQ     CAN
095800921020     C                   MOVEL     ' '           ARTCAN
095900921019     C                   ELSE
096000921019     C                   SETON                                        06
096100961129    2C                   END
096200950118     C*
096300960417     C*  MEMORIZZAZIONI PER ANOMALIE
096400961129    2C     FLG           IFEQ      '1'
096500960416     C                   MOVE      ARTAAS        WAAS
096600960416     C                   MOVE      ARTLNP        WLNP
096700960416     C                   MOVE      ARTNSP        WNSP
096800960417     C                   MOVE      ARTDAM        WDAM
096900070424     C                   MOVE      ARTdet        WDET
097000070424     C                   MOVE      ARTFLP        WFLP
097100960417     C                   MOVE      ARTDCM        WDCM
097200961129    2C                   END
097300921020     C*
097400961129     C                   MOVEL     ARTDTR        WDTR
097500941206     C                   UPDATE    FNART000
097600921019     C*
097700911016     C* AGGANCIO BOLLA
097800930604     C*
097900941206     C     KARB          CHAIN     FNARB01L                           3339
098000920424     C* 39 ON - RECORD ALLOCATO
098100970625    2C     *IN39         IFEQ      *OFF
098200030115      * Mi salvo arbdam
098300030115     c                   Eval      Sav_ArbDam = ArbDam
098400970625     C**
098500921019     C* CONTROLLO TUTTO IL DETTAGLIO
098600970625    3C     *IN20         IFEQ      *OFF
098700970625     C     *IN06         OREQ      *OFF
098800970625     C*
098900921019     C  N33              EXSR      CTRART
099000970625     C* 06 OFF - NESSUN CODICE ANOMALIA: ELIMINO ANCHE IN TESTATA
099100970625     C  N06              MOVEL     ' '           ARBFAN
099200970625     C* FLAG ABILITAZIONE MERCE:
099300970625    4C  N20FLG           IFEQ      '1'
099400970625     C     ARBAMA        ANDNE     3
099500101202     c*
099600060512     c                   select
099700970625     C* ABILITATO IN BASE ALLA SPUNTA ARRIVO
099800060512    5C     NOAMA         wheneq    ' '
099900970625     C                   MOVEL     2             ARBAMA
100000970625     C                   MOVEL     1             ARBAMP
100100970625     C* ABILITATO IN BASE ALLA SPUNTA PARTENZA(ABBINATO)
100200060512    5C     NOAMP         wheneq    ' '
100300970625     C                   MOVEL     1             ARBAMA
100400970625     C                   MOVEL     1             ARBAMP
100500060512    5C     NOAMP         wheneq    '0'
100600060512     C                   MOVEL     0             ARBAMA
100700060512     C                   MOVEL     1             ARBAMP
100800060512   X5C                   other
100900970625     C* NON ABILITATO
101000970625     C                   MOVEL     0             ARBAMA
101100970625     C                   MOVEL     0             ARBAMP
101200060512    5C                   ENDsl
101300970625    4C                   END
101400970625    3C                   ENDIF
101500961120     C*
101600961120     C* MEMORIZZO LA DATA BORDERO' CHE MI SERVIRA' PER BOLLA LOCALE
101700961120     C*  SE DEVO CREARE L'ANOMALIA 5
101800961120     C                   Z-ADD     ARBDBR        WDBR
101900961120     C                   MOVEL     ARBFBC        WFBC
102000970127     C* TESTO SE SPEDIZIONE GIACENTE PER ANOMALIA 15
102100970625    3C     WFBC          IFEQ      'G'
102200050401     C     KGCA          CHAIN     Tigcp21L                           38
102300970721     C* AI FINI DELL'IMA DEVO TESTARE LA DATA DISPOSIZONI MITTENTE
102400970721    5C  N38GCPFAS        IFLT      35
102500050401    5C     GCPFAS        oreq      36
102600970721     C                   MOVEL     'S'           WGIAC             1
102700970625    3C                   ENDIF
102800970721    3C                   ENDIF
102900070424     c*
103000150819    3c                   if        Aggiodti='S'
103100070424     c                   clear                   arbdti
103200070424     c                   clear                   arbhti
103300070424     C     ARBFBS        IFEQ      ' '
103400070424     C                   MOVEL     'S'           ARBFBS
103500070424     C                   ELSE
103600070424     C     ARBFBS        IFEQ      'V'
103700070424     C     ARBFBS        oreq      'P'
103800070424     C                   MOVEL     'E'           ARBFBS
103900070424     C                   ENDIF
104000070424     C                   ENDIF
104100101202     c*
104200150819    3C                   ENDIF
104300921019     C*
104400941206     C  N33              UPDATE    FNARB000
104500101202      *
104600030320      * Se è un tipo bolla da trasmettere in sede
104700030320     c                   Clear                   ds3a
104800030320     c                   Eval      Cod = '3A'
104900030320     c                   Clear                   Key
105000030320     c                   Movel     ArbCbo        Key
105100030320     c     kTab          Chain     Tabel00f
105200030320     c                   If        %Found(Tabel00f)
105300030320     c                   Movel     TblUni        ds3a
105400030320     c                   EndIf
105500030320If  2c                   If        §3aBsd = *Blanks
105600030115      * Scrivo/Aggiorno Fidta se data variata
105700030115     c                   If        Not *In33 and Sav_ArbDam <> ArbDam
105800030115     c                   Eval      wFidta = '0'
105900030115     c                   Eval      SavAas = ArbAas
106000030115     c                   Eval      SavLnp1 = ArbLnp
106100030115     c                   Eval      SavNrs = ArbNrs
106200030115     c                   Eval      SavNsp = ArbNsp
106300030115     c                   Eval      SavPoc = ArbLna
106400030115     c                   Eval      SavTrd = 'DAM'
106500030115     c     kFidta        Setll     Fidta01l
106600030115     c                   Do        *Hival
106700030115     c     kFidta        Reade     Fidta01l
106800030115      * Fine file
106900030115     c                   If        %Eof(Fidta01l)
107000030115     c                   Leave
107100030115     c                   EndIf
107200030115      * Già trasmesso
107300030115     c                   If        DtaFtr <> *Blanks
107400030115     c                   Iter
107500030115     c                   EndIf
107600030115      * Aggiorno
107700030115     c                   Eval      DtaDta = ArbDam
107800030115     c                   Update    Fidta000
107900030115     c                   Eval      wFidta = '1'
108000030115     c                   EndDo
108100030115      * Scrivo
108200030115     c                   If        wFidta = '0'
108300030115     c                   Clear                   Fidta000
108400030115     c                   Eval      DtaAas = ArbAas
108500030115     c                   Eval      DtaLnp = ArbLnp
108600030115     c                   Eval      DtaNrs = ArbNrs
108700030115     c                   Eval      DtaNsp = ArbNsp
108800030115     c                   Eval      DtaTrd = SavTrd
108900030115     c                   Eval      DtaPoc = SavPoc
109000030115     c                   Eval      DtaDta = ArbDam
109100030115     c                   Write     Fidta000
109200030115     c                   EndIf
109300030320    2c                   EndIf
109400030115     c                   EndIf
109500101202      *
109600970625   X2C                   ELSE
109700960614     C* BOLLA ALLOCATA:
109800960614     C* SE NON E' L'ULTIMO TENTATIVO VADO A FINE ROUTINE
109900970625    3C     B             IFLT      3
110000960614     C                   GOTO      ENDUPD
110100970625    3C                   END
110200960614     C* SE E' L'ULTIMO TENTATIVO SCRIVO FNAGB
110300960614     C                   MOVEL     'A'           AGBTBO
110400960614     C                   MOVEL     'DT'          AGBAGB
110500960614     C                   Z-ADD     ARTAAS        AGBAAS
110600960614     C                   Z-ADD     ARTLNP        AGBLNP
110700960614     C                   Z-ADD     ARTNRS        AGBNRS
110800960614     C                   Z-ADD     ARTNSP        AGBNSP
110900960614     C                   MOVEL     'S'           AGBFBS
111000960614     C                   CLEAR                   AGBFVC
111100041014     C                   CLEAR                   AGBFpC
111200960614     C     KAGB          SETLL     FNAGB01L                               31
111300960614     C  N31              WRITE     FNAGB000
111400970625    2C                   END
111500031203   1aC                   END
111600921013     C*
111700031114    1C                   END
111800041216     c                   unlock    fnart27l
111900041216     c                   unlock    fnarb01l
112000031114     C**
112100031114     C** T R A N S I T I  /  D I S G U I D I   I N   A R E A   E  N O N
112200031114     C**
112300031114    1C     WESBTT        IFEQ      'S'
112400911112     C*
112500031114     C     KBAR37        CHAIN     FNBTT37L                           3739
112600920424     C* 39 ON  - RECORD ALLOCATO
112700920424     C   39              GOTO      ENDUPD
112800911016     C*
112900921019     C* 37 OFF - TROVATA BOLLA
113000941230    2C  N37              DO
113100941230     C*
113200941230    3C     SAVCAN        IFEQ      ' '
113300941206     C     BTTCAN        ANDEQ     CAN
113400941206     C                   MOVEL     ' '           BTTCAN
113500941230    3C                   END
113600921230     C*
113700941230    3C     FLG           IFEQ      '1'
113800060512     c                   movel     bttpet        knps
113900060512     c                   exsr      TIPPIS
114000101202     c*
114100060512   3aC     BTTPET        ifeq      NPS
114200970530     C     BTTDET        ANDEQ     WVVDFV
114300060512     c* Oppure se sono entrambe manuali
114400060512     c     spurps        oreq      'M'
114500060512     c     §7jrps        andeq     spurps
114600101202     c*
114700970604     C                   MOVE      BTTDET        SAVDET
114800041014     C  N18              MOVEL     *ZEROS        BTTDET
114900041014     C  N18              MOVEL     *ZEROS        BTTPET
115000060512     C  N18              clear                   BTTcan
115100041014     C   18              MOVEL     traDFV        BTTDET
115200041014     C   18              MOVEL     traNPS        BTTPET
115300970625     C*
115400970604     C     BTTDET        IFNE      SAVDET
115500970604     C                   CLEAR                   SA2TLA
115600970604     C                   MOVEL     'A'           SA2TRC
115700970604     C                   Z-ADD     SAVDET        SA2DTS
115800970604     C                   MOVE      *ZEROS        SA2LNA
115900970604     C                   CALL      'FNLSA2R3'
116000970604     C                   PARM                    SA2TLA            1
116100970604     C                   PARM                    SA2TRC            1
116200970604     C                   PARM                    SA2DTS            8 0
116300970604     C                   PARM                    SA2LNA            3 0
116400970604     C* NUOVA DATA
116500970604     C     BTTDET        IFGT      *ZEROS
116600970604     C                   Z-ADD     BTTDET        SA2DTS
116700970604     C                   CALL      'FNLSA2R3'
116800970604     C                   PARM                    SA2TLA            1
116900970604     C                   PARM                    SA2TRC            1
117000970604     C                   PARM                    SA2DTS            8 0
117100970604     C                   PARM                    SA2LNA            3 0
117200970604     C                   END
117300970604     C                   END
117400970625     C*
117500970625     C* NON SONO STATE MODIFICATE LE DATE SUL DETTAGLIO COLLI
117600060512   3aC                   ELSE
117700930604     C                   SETON                                        20
117800060512   3AC                   END
117900060512     C*
118000060512     C* NON SONO STATE MODIFICATE LE DATE SUL DETTAGLIO COLLI
118100060512   X3C                   ELSE
118200060512     C                   SETON                                        20
118300060512    3C                   END
118400960416     C*  MEMORIZZO KEY SPEDIZIONE PER ANOMALIE
118500960416     C     FLG           IFEQ      '1'
118600960416     C                   MOVE      BTTAAS        WAAS
118700960416     C                   MOVE      BTTLNP        WLNP
118800960416     C                   MOVE      BTTNSP        WNSP
118900960416     C                   END
119000921020     C*
119100941206     C                   UPDATE    FNBTT000
119200921020     C*
119300911016     C* AGGANCIO BOLLA
119400941230    3C     FLG           IFEQ      '1'
119500930604     C     *IN20         ANDEQ     '0'
119600031114     C     KBTT          CHAIN     FNBTP11L                           3339
119700920424     C* 39 ON  - RECORD ALLOCATO
119800920424     C   39              GOTO      ENDUPD
119900030115      * Mi salvo btpdet (btpdut non viene modificato)
120000030115     c                   Eval      Sav_BtpDet = BtpDet
120100930604     C*
120200921020     C  N33              EXSR      CTRBTT
120300921019     C*
120400941206     C  N33              UPDATE    FNBTP000
120500030115      * Scrivo/Aggiorno Fidta se la data è variata
120600030115     c                   If        Not *In33 and Sav_BtpDet <> BtpDet
120700030115     c                   Eval      wFidta = '0'
120800030115     c                   Eval      SavAas = BtpAas
120900030115     c                   Eval      SavLnp1 = BtpLnp
121000030115     c                   Eval      SavNrs = BtpNrs
121100030115     c                   Eval      SavNsp = BtpNsp
121200030115     c                   Eval      SavPoc = BtpFlp
121300030115     c                   Eval      SavTrd = 'DET'
121400030115     c     kFidta        Setll     Fidta01l
121500030115     c                   Do        *Hival
121600030115     c     kFidta        Reade     Fidta01l
121700030115      * Fine file
121800030115     c                   If        %Eof(Fidta01l)
121900030115     c                   Leave
122000030115     c                   EndIf
122100030115      * Già trasmesso
122200030115     c                   If        DtaFtr <> *Blanks
122300030115     c                   Iter
122400030115     c                   EndIf
122500030115      * Aggiorno
122600030115     c                   Eval      DtaDta = BtpDet
122700030115     c                   Update    Fidta000
122800030115     c                   Eval      wFidta = '1'
122900030115     c                   EndDo
123000030115      * Scrivo
123100030115     c                   If        wFidta = '0'
123200030115     c                   Clear                   Fidta000
123300030115     c                   Eval      DtaAas = BtpAas
123400030115     c                   Eval      DtaLnp = BtpLnp
123500030115     c                   Eval      DtaNrs = BtpNrs
123600030115     c                   Eval      DtaNsp = BtpNsp
123700030115     c                   Eval      DtaTrd = SavTrd
123800030115     c                   Eval      DtaPoc = SavPoc
123900030115     c                   Eval      DtaDta = BtpDet
124000030115     c                   Write     Fidta000
124100030115     c                   EndIf
124200030115     c                   EndIf
124300101202      *
124400941230    3C                   END
124500921014     C*
124600941230    2C                   END
124700941230    1C                   END
124800041216     c                   unlock    fnbtt37l
124900041216     c                   unlock    fnbtp11l
125000101202     c*
125100041013     c* Aggiornamento anche di FNART per spunta di transito/disguido
125200041013     c*  cancellata
125300041013     c                   if        (lnatfa<>bpes and lnatfa<>pestfa  and
125400041013     c                             wagpar=' ') or wesbtt='S'
125500041013     C     KBOL2         CHAIN     FNART27L
125600041013     c                   if        %found(fnart27l)
125700101202     c*
125800041013    3C     SAVCAN        IFEQ      ' '
125900041013     C     arTCAN        ANDEQ     CAN
126000041013     C                   MOVEL     ' '           arTCAN
126100041013    3C                   END
126200041013     C*
126300041013    3C     FLG           IFEQ      '1'
126400060512     c                   movel     artpet        knps
126500060512     c                   exsr      TIPPIS
126600101202     c*
126700060512   3aC     artPET        ifeq      NPS
126800041013     C     artDET        ANDEQ     WVVDFV
126900060512     c* Oppure se sono entrambe manuali
127000060512     c     spurps        oreq      'M'
127100060512     c     §7jrps        andeq     spurps
127200101202     c*
127300041015     C   18              MOVEL     traDFV        artDET
127400041015     C   18              MOVEL     traNPS        artPET
127500041014     C  N18              MOVEL     *ZEROS        artDET
127600041014     C  N18              MOVEL     *ZEROS        artPET
127700060512     C  N18              CLEAR                   artCAN
127800041014    4c  n18              if        wesbtt<>'S'
127900041014     C                   MOVEL     *ZEROS        artflp
128000041014     c* Verifico le spunte partenza per impostare eventuale altro p.o. di
128100041014     c*  transito/disguido
128200041014     c* Se p.o. ultima spunta partenza= p.o. ultima spunta no part.
128300041014     c* imposto questa
128400041014    5c                   select
128500041014     c                   when      dis1fgs=dis2fgs
128600041014     c                   z-add     dis2dfv       artdet
128700041014     c                   z-add     dis2nps       artpet
128800041014     c                   z-add     dis2fgs       artflp
128900041014     c                   z-add     dis1dfv       artdut
129000041014     c                   z-add     dis1nps       artput
129100101202     c*
129200041014     c* Elaboro spunta partenza se con data > o data/ora caricamento>
129300041014     c                   when      dis1dfv=dis2dfv and dis1scar>dis2scar or
129400041014     c                             dis1dfv>dis2dfv
129500041014     c* Se si tratta di uscita transito aggiorno subito altrimenti leggo
129600041014     c* le linee per vedere se c'e' transito o meno
129700041014    6c                   if        dis1atr='T' or dis1atr='D'
129800041014     c                   movel     dis1fgs       artflp
129900041014     c                   movel     dis1dfv       artdet
130000041014     c                   movel     dis1dfv       artdut
130100041014     c                   movel     dis1nps       artpet
130200041014     c                   movel     dis1nps       artput
130300041014   x6c                   else
130400041014     c* E si tratta della partenza o si tratta di un disguido che
130500041014     c* transita di nuovo per cui aggiorno solo flp
130600041014    7c                   if        dis1fgs<>lnptfp
130700041014     c                   movel     dis1fgs       artflp
130800041014   x7c                   else
130900041014     c* chain fnfgv
131000041014     c     kfgv          chain     fnfgv01l
131100041014    8c                   if        %found(fnfgv01l) and fgvlna<>lnatfa
131200041015     c     kfgv          chain     fnfgw01l
131300041015     c                   if        not %found(fnfgw01l)
131400041015     c                   clear                   fgwff3
131500041015     c                   clear                   fgwff4
131600041015     c                   clear                   fgwfl3
131700041015     c                   clear                   fgwfl4
131800041015     c                   endif
131900101202     c*
132000041015     c                   z-add     1             xx                3 0
132100041015     c                   movel     lna           wlna              3
132200041015     c                   movel     lnatfa        wlnatfa           3
132300041015     c     wlna          lookup    ffv(xx)                                31
132400041015    9c                   if        *in31 and flp(xx)<>wlnatfa
132500041014     c                   movel     flp(xx)       artflp
132600041014    9c                   endif
132700041014    8c                   endif
132800041014    7c                   endif
132900041014    6c                   endif
133000041014   x5c                   other
133100041014     c* Altrimenti elaboro spunta arrivi
133200041014     c                   movel     dis2fgs       artflp
133300041014     c                   movel     dis2dfv       artdet
133400041014     c                   movel     dis2nps       artpet
133500041014     c                   clear                   artdut
133600041014     c                   clear                   artput
133700041014    5c                   endsl
133800101202     c*
133900041014    4c                   endif
134000060512   3ac                   endif
134100060512    3c                   endif
134200101202     c*
134300041014     c                   update    fnart000
134400041014     c                   clear                   fnagb000
134500041014     C                   MOVEL     'A'           AGBTBO
134600041014     C                   MOVEL     'DT'          AGBAGB
134700041014     C                   Z-ADD     ARTAAS        AGBAAS
134800041014     C                   Z-ADD     ARTLNP        AGBLNP
134900041014     C                   Z-ADD     ARTNRS        AGBNRS
135000041014     C                   Z-ADD     ARTNSP        AGBNSP
135100041014     C     KAGB          SETLL     FNAGB01L                               31
135200041014     C  N31              WRITE     FNAGB000
135300041013     c                   endif
135400041014     c                   endif
135500921019     C**
135600921019     C** CHIUDO ANOMALIE
135700921019     C**
135800960416    1C     FLG           IFEQ      '1'
135900960416     C*
136000960416     C                   CLEAR                   W005
136100960416     C                   CLEAR                   W015
136200070424     c                   clear                   wcrea200          1
136300921021     C*
136400990518     C     KBOL2         SETLL     FNANM000
136500990518     C     KBOL2         READE     FNANM000                               32
136600921019     C*
136700961129    2C     *IN32         DOWEQ     *OFF
136800060512     c* Se anomalia 15 e non esiste più la data di arrivo del collo
136900060512     c*  devo cancellare l'anomalia
137000060512    3c                   if        anmcaa=015 and wdam=0  and wagarr='S'
137100060512    4c                   if        anmdch>0
137200060828     c                   unlock    fnanm02l
137300060512     c* Se già in idd3 la chiudo come annullata
137400060512     C                   MOVEL     FASE          D33FSC
137500060512     C                   MOVEL     'AN'          D33CCH
137600060828     C                   MOVEL     dateu         D33dCH
137700060512     C                   MOVE      ANMNR2        D33NRR
137800060512     C                   MOVEL     'S'           D33MAC
137900060512     C                   CALL      'FNLR33R'
138000060512     C                   PARM                    DSLR33
138100101202     c*
138200060512     C     KBOL2         READE     FNANM000                               32
138300060512     c                   iter
138400060512     c                   else
138500060512     c* la deleto
138600060512     c                   delete    fnanm000
138700101202     c*
138800060512     C     KBOL2         READE     FNANM000                               32
138900060512     c                   iter
139000060512    4c                   endif
139100060512    3c                   endif
139200101202     c*
139300070424     c* Se esiste anomalia 55 e artdam=0, vedo se ricreare la 200
139400130301    3c                   if        (anmcaa=57 or
139500130301    3c                              anmcaa=55 or anmcaa=56) and wdam=0
139600070424     c                             and anmdao=wdet and anmfle=wflp
139700070424     c                   eval      wcrea200='S'
139800070424     c                   eval      savrecanm=dsrecanm
139900070424    3c                   endif
140000101202     c*
140100070424     c* Se trovo già la 200 chiusa, la devo solo riaprire
140200070424     c                   if        wcrea200='S' and anmcaa=200 and
140300070424     c                             anmfle=wflp
140400070424     C                   Z-ADD     0             ANMDCH
140500070424     C                   MOVEL     ' '           ANMFSC
140600070424     C                   MOVEL     '  '          ANMCCH
140700070424     C                   CLEAR                   ANMFT1
140800070424     C                   CLEAR                   ANMFT2
140900070424     C                   UPDATE    FNANM000
141000101202     c*
141100070424     c* N= non creare la 200 perchè già trovata
141200070424     c                   eval      wcrea200='N'
141300110908     C     KBOL2         READE     FNANM000                               32
141400070424     c                   iter
141500070424     c                   endif
141600101202     c*
141700110908     c* Anomlia 115 --> se non esiste nessun'altra spunta sul sistema reale
141800110908     c*                 riapro l'anomalia anche non mia
141900110908    3c                   if        anmcaa=115
142000110908    4c                   if        a_115dfv=0
142100110908     C                   Z-ADD     0             ANMDCH
142200110908     C                   MOVEL     ' '           ANMFSC
142300110908     C                   MOVEL     '  '          ANMCCH
142400110908     C                   CLEAR                   ANMFT1
142500110908     C                   CLEAR                   ANMFT2
142600110908   x4c                   else
142700110908     c* altrimenti se è stata con questa spunta--> la riaggiorno
142800110908    5C     ANMDCH        IFNE      0
142900110908     C     ANMFSC        ANDEQ     FASE
143000110908     C     ANMdch        ANDEQ     wvvdfv
143100110908     C                   CLEAR                   DSFASE
143200110908     C                   MOVE      a_115NPG      D11NPG
143300110908     C                   MOVE      a_115SPG      D11SPG
143400110908     C                   CALL      'TRUL11R'
143500110908     C                   PARM                    DSFASE
143600110908     C                   MOVEL     D11FAS        ANMFSC
143700110908     C                   MOVEL     a_115dfv      ANMdch
143800110908    5c                   endif
143900110908    4c                   endif
144000110908     C                   UPDATE    FNANM000
144100110908     c
144200110908     C     KBOL2         READE     FNANM000                               32
144300110908     c                   iter
144400110908    3c                   endif
144500060512     C*
144600041018     c* Leggo solo le anomalie create da me
144700120109    3c*****              if        anmfle=bpes or anmfle=parfgs
144800921019     C*
144900921019     C* RIAPRO SE CHIUSE E NESSUN'ALTRA SPUNTA
145000980205     C* E SE LA SPEDIZIONE NON E' CONSEGNATA
145100120109    3C                   if        anmcaa=005
145200120109    4c                   if        anmfle=bpes or anmfle=parfgs
145300120109     c                             or  anmfle=lnatfa
145400041018    5c     wagarr        ifeq      'S'
145500961129     C                   MOVE      '1'           W005              1
145600921019     C*
145700041018    6C  N19WDCM          IFEQ      0
145800980205     C     WDAM          ANDEQ     0
145900980205     C**
146000041018    7C     ANMDCH        IFNE      0
146100041018    8C     ANMLID        IFGE      §7ALCI
146200960510     C                   MOVE      'PR'          ANMCCH
146300960510     C                   MOVE      'D'           ANMFSC
146400970410     C                   CLEAR                   ANMFT1
146500970410     C                   CLEAR                   ANMFT2
146600960510     C                   ELSE
146700921019     C                   Z-ADD     0             ANMDCH
146800921019     C                   MOVEL     ' '           ANMFSC
146900921019     C                   MOVEL     '  '          ANMCCH
147000970410     C                   CLEAR                   ANMFT1
147100970410     C                   CLEAR                   ANMFT2
147200041018    8C                   END
147300990518     C                   UPDATE    FNANM000
147400041018    7C                   END
147500041018    6C                   END
147600041018    5C                   END
147700120109    4C                   END
147800921021     C*
147900120109   x3c                   else
148000120109     c
148100120109    4c                   if        anmfle=bpes or anmfle=parfgs
148200101202     c*
148300921019     C* PER TUTTE LE ALTRE ANOMALIE CONTROLLO FASE APERTURA E FOGLIO
148400041018    5C     ANMNFV        IFEQ      PARNFV
148500921021     C     ANMFSA        ANDEQ     FASE
148600960513     C*
148700960417     C     ANMCAA        OREQ      050
148800960417     C     *IN19         ANDEQ     *OFF
148900041018     c     anmft1        andeq     *blanks
149000101202     c*
149100041018     c* chain in DS/C per controllo se autochiusa
149200041018     C     anmcaa        IFNE      SAVCAA
149300041018     C                   MOVEL     '7C'          COD
149400041018     C                   MOVEL(P)  ANMCAA        KEY
149500041018     C     KTAB          CHAIN     TABEL                              31
149600041018     C  N31              MOVEL     TBLUNI        DS7C
149700041018     C   31              MOVEL     *BLANKS       DS7C
149800041018     C                   MOVEL     anmcaa        SAVCAA
149900041018     C                   ENDIF
150000960513     C*
150100041018     C* PER alcune anomalie,
150200041018     C*  SE ESISTE UN'ALTRA SPUNTA PER IL COLLO MODIFICO
150300041018     C*  L'ANOMALIA IN BASE ALLA SPUNTA RIMASTA
150400041018    6c                   if        *in19 and (anmcaa=10 or anmcaa=160
150500041018     c                             or anmcaa=200 or anmcaa=60 or
150600041018     c                             anmcaa=61)
150700960417     C                   CLEAR                   DSFASE
150800960417     C                   MOVE      SPUNPG        D11NPG
150900960417     C                   MOVE      SPUSPG        D11SPG
151000960417     C                   CALL      'TRUL11R'
151100960417     C                   PARM                    DSFASE
151200960417     C                   MOVEL     D11FAS        ANMFSA
151300960513     C                   MOVE      SPUFGS        ANMCDU
151400041018     C                   Z-ADD     SPUNFV        ANMNFV
151500041018     C* NELL'ANOMALIA 10 NON MODIFICO LA DATA APERTURA PER NON FAR DIVE
151600041018     C*  NTARE INCONGRUENTE IL LIVELLO IDD CON LA DATA CREAZIoNE ANOM
151700041018    7c                   if        anmcaa<>10
151800041018     C                   Z-ADD     SPUDFV        ANMDAO
151900041018    7c                   endif
152000041018     C                   MOVE      SPUFGS        ANMFLE
152100041018     C                   MOVE      SPUNPS        ANMNPS
152200041018     c* se anomalia autochiusam, imposto anche i dati di chiusura
152300041018     c                   if        §7ccha='S'
152400041018     C                   MOVE      spudfv        ANMDCH
152500041018     C                   MOVEL     d11fas        ANMFSC
152600041018     c                   endif
152700970603     C*
152800041018    7C     ANMAIE        IFEQ      'E'
152900970603     C                   CLEAR                   ANMFT1
153000970603     C                   CLEAR                   ANMFT2
153100041018    7C                   ENDIF
153200990518     C                   UPDATE    FNANM000
153300041018   X6C                   ELSE
153400990412     C*
153500990412     C* SE SI TRATTA DI ANOMALIA 56 CON ALTRE SPUNTE (19 ON):
153600041018     C*  modifico anche se trasmessa essendo su As unico, non
153700041018     C*  modifico però la data visto che ha aggiornato bltdet
153800041018    7C                   IF        *in19 and (anmcaa=55 or anmcaa=56)
153900101202     C*
154000041018    8C                   if        ANMCAA=56 AND A56DFV>0
154100041018     c                   if        anmft1=' '
154200990412     C                   Z-ADD     A56DFV        ANMDAO
154300990412     C                   Z-ADD     A56DFV        ANMDCH
154400041018     c                   endif
154500990412     C                   CLEAR                   DSFASE
154600990412     C                   MOVE      A56NPG        D11NPG
154700990412     C                   MOVE      A56SPG        D11SPG
154800990412     C                   CALL      'TRUL11R'
154900990412     C                   PARM                    DSFASE
155000990412     C                   MOVEL     D11FAS        ANMFSA
155100990412     C                   MOVEL     D11FAS        ANMFSC
155200990412     C                   MOVE      A56FGS        ANMCDU
155300990412     C                   MOVE      A56FGS        ANMFLE
155400990412     C                   MOVE      A56NPS        ANMNPS
155500990518     C                   Z-ADD     A56NFV        ANMNFV
155600990518     C                   UPDATE    FNANM000
155700041018   X8C                   else
155800990412     C* LA TRASFORMO IN 55
155900990412     C                   Z-ADD     55            ANMCAA
156000041018    9c                   if        anmft1=' '
156100990412     C                   Z-ADD     SPUDFV        ANMDAO
156200990412     C                   Z-ADD     SPUDFV        ANMDCH
156300041018    9c                   endif
156400990412     C                   CLEAR                   DSFASE
156500990412     C                   MOVE      SPUNPG        D11NPG
156600990412     C                   MOVE      SPUSPG        D11SPG
156700990412     C                   CALL      'TRUL11R'
156800990412     C                   PARM                    DSFASE
156900990412     C                   MOVEL     D11FAS        ANMFSA
157000990412     C                   MOVEL     D11FAS        ANMFSC
157100990412     C                   MOVE      SPUFGS        ANMCDU
157200990412     C                   MOVE      SPUFGS        ANMFLE
157300990412     C                   MOVE      SPUNPS        ANMNPS
157400990518     C                   Z-ADD     SPUNFV        ANMNFV
157500990518     C                   UPDATE    FNANM000
157600041018    8C                   ENDif
157700990412   X7C                   ELSE
157800960416     C* Richiamo pgm di chiusura anomalia
157900990518     C                   UNLOCK    FNANM02L
158000960416     C                   CLEAR                   DSLR33
158100041018    8C     DATEU         IFGE      WVVDFV
158200970117     C                   MOVEL     DATEU         D33DCH
158300970117     C                   ELSE
158400041018    8C                   MOVEL     WVVDFV        D33DCH
158500970117     C                   ENDIF
158600960416     C                   MOVEL     FASE          D33FSC
158700960416     C                   MOVEL     'AN'          D33CCH
158800960416     C                   MOVE      ANMNR2        D33NRR
158900960510     C                   MOVEL     'S'           D33MAC
159000960416     C                   CALL      'FNLR33R'
159100960416     C                   PARM                    DSLR33
159200990412    7C                   ENDIF
159300041018    6C                   END
159400921021     C*
159500041018   X5C                   ELSE
159600041018    6C     ANMCAA        IFNE      50
159700041018    6C     ANMCAA        andne     56
159800041018    6C     ANMCAA        andne     55
159900130301    6C     ANMCAA        andne     57
160000921021     C* CONTROLLO SE E' STATA CHIUSA IN QUESTA FASE
160100041018    7C     ANMDCH        IFNE      0
160200921021     C     ANMFSC        ANDEQ     FASE
160300041018     C     ANMdch        ANDEQ     wvvdfv
160400041018     c* Per l'anomalia 15 controllo se esiste altra spunta con data >= a
160500041018     c*  quella annullata nel qual caso la chiudo con dati diversi
160600041018     c                   clear                   wokanm15
160700041018    9C                   if        anmcaa=015
160800041018     C                   MOVE      '1'           W015              1
160900041018    0c                   if        dandfv>wvvdfv  or
161000041018     c                             (dannpg<>2 and dandfv>wvvdfv)
161100110908     C                   CLEAR                   DSFASE
161200110908     C                   MOVE      danNPG        D11NPG
161300110908     C                   MOVE      danSPG        D11SPG
161400110908     C                   CALL      'TRUL11R'
161500110908     C                   PARM                    DSFASE
161600110908     C                   MOVEL     D11FAS        ANMFSC
161700110908     C                   MOVEL     dandfv        ANMdch
161800041018     c                   eval      wokanm15='S'
161900041018    9c                   endif
162000041018    8c                   endif
162100041018     c*
162200041018    8c                   if        wokanm15=' '
162300041018    9c     ANMLID        IFGE      §7ALCI
162400960510     C                   MOVE      'PR'          ANMCCH
162500960510     C                   MOVE      'D'           ANMFSC
162600960510     C                   ELSE
162700921021     C                   Z-ADD     0             ANMDCH
162800921021     C                   MOVEL     ' '           ANMFSC
162900921021     C                   MOVEL     '  '          ANMCCH
163000041018    9C                   END
163100961025     C                   MOVE      *BLANKS       ANMFT1
163200990518     C                   UPDATE    FNANM000
163300041018    8C                   END
163400041018    7C                   END
163500041018    6C                   END
163600041018    5C                   END
163700921021     C*
163800120109    4C                   ENDIF
163900120109    3C                   END
164000921019     C*
164100990518     C     KBOL2         READE     FNANM000                               32
164200960416    2C                   ENDDO
164300070424     c*
164400070424     c* Verifico se devo creare  l'anomalia 200
164500070424     c                   if        wcrea200='S'
164600070424     c                   eval      dsrecanm=savrecanm
164700070424     C                   Z-ADD     200           COMCAA
164800070424     c                   exsr      CRTANM
164900070424     c                   eval      wcrea200='N'
165000070424     c                   endif
165100960416     C*
165200960416     C* CREO ANOMALIA 005 A FRONTE DI CANCELLAZIONE SPUNTA
165300960416     C     W005          IFEQ      *BLANKS
165400031114     c     wagarr        andeq     'S'
165500960416     C     *IN19         ANDEQ     *OFF
165600960416     C     WCHIUS        ANDEQ     'S'
165700960530     C     WDAM          ANDEQ     *ZEROS
165800960530     C     WDCM          ANDEQ     *ZEROS
165900000904     C*
166000031114     c* creo sempre anche per 2 liv in arrivo su altro AS in quanto
166100031114     c*  se poi dal terminal riceco la spunta l'anomalia viene cancell
166200031114     c*  se riricevo l'anomalia, dovrebbe andare in update
166300031114     c* (prioma invece creava soltanto se terarr di lna in £1)
166400000904     C                   EXSR      CRTA05
166500000904     C                   ENDIF
166600960530     C**
166700960416     C* CREO ANOMALIA 015 A FRONTE DI CANCELLAZIONE SPUNTA
166800960416     C     W015          IFEQ      *BLANKS
166900031114     C     wagarr        andeq     'S'
167000960416     C* Collo non consegnato
167100960416     C     WDCM          ANDEQ     *ZEROS
167200960417     C* Collo arrivato
167300960417     C     WDAM          ANDGT     *ZEROS
167400960416     C* Data arrivo <= data spunta cancellata
167500960416     C     WDAM          ANDLE     WVVDFV
167600960416     C* Non esistono altre spunte inventario o consegna in data >=
167700960416     C* alla data di cancellazione della spunta
167800960416     C     WSPUN         ANDEQ     *BLANKS
167900970127     C* La bolla non deve essere bloccata
168000970127     C     WFBC          ANDNE     'B'
168100970127     C* La bolla non è in giacenza
168200970127     C     WGIAC         ANDNE     'S'
168300000904     C                   EXSR      CRTA15
168400000904     C                   END
168500960416     C*
168600960416    1C                   END
168700050408     c* se la pistola della spunta annullata non è automatica devo decrement
168800050408     c* are sgnst2 su fisgn
168900060512     c                   movel     nps           knps
169000050408     c                   exsr      tippis
169100050408     c     §7jrps        ifne      'A'
169200050408     C* AGGIORNO FILE  FISGN SE C'E'
169300050408     C     KBOL2         CHAIN     FISGN03L                           33
169400050408     C     *IN33         DOWEQ     *OFF
169500050408     c* testo > 0 per sicurezza ma in teoria non DEVE succedere che sia = 0
169600050408     C     SGNST2        IFgt      0
169700050408     C                   sub       1             SGNST2
169800050408     C                   CLEAR                   SGNT6A
169900050408     C                   CLEAR                   SGNT6B
170000050408     C                   CLEAR                   SGNT6C
170100050408     C                   CLEAR                   SGNT6D
170200050408     C                   CLEAR                   SGNT6E
170300050408     C                   CLEAR                   SGNT6F
170400080623     c                   eval      sgndatora = %char(%timestamp:*iso0)
170500050408     C                   UPDATE    FISGN000
170600050408     C                   ENDIF
170700050408     C     KBOL2         READE     FISGN03L                               33
170800050408     C                   ENDDO
170900050408     C                   ENDIF
171000921019     C*
171100920424     C                   Z-ADD     3             B
171200941206     C*
171300970530     C     ENDUPD        ENDSR
171400970530     C**************************************************************************
171500970530     C* CONTROLLO IL TIPO PISTOLA DELLA SPUNTA CHE LEGGO
171600970530     C**************************************************************************
171700970530     C     TIPPIS        BEGSR
171800970530     C                   MOVEL     '7J'          COD
171900060512     C                   MOVEL(P)  knps          KEY
172000970530     C     KTAB          CHAIN     TABEL                              31
172100970530     C  N31              MOVEL     TBLUNI        DS7J
172200970530     C   31              CLEAR                   DS7J
172300970530     C*
172400970530     C                   ENDSR
172500961120     C**************************************************************************
172600970530     C* REPERISCO I DATI DEL FOGLIO DELLA SPUNTA LETTA
172700961120     C**************************************************************************
172800961120     C     CHAFV         BEGSR
172900961120     C                   Z-ADD     DATEU         DATAF
173000961120     C                   MOVEL     ' '           WFVV              1
173100961120     C                   MOVEL     ' '           WFGV              1
173200961120     C                   MOVEL     ' '           WFVA              1
173300970530     C                   CLEAR                   DEFF
173400970530     C                   CLEAR                   SPGF
173500970530     C                   CLEAR                   NFAF
173600970530     C                   CLEAR                   AEDF
173700970530     C                   CLEAR                   NFAFCF
173800970530     C                   CLEAR                   NFASPG
173900970530     C                   CLEAR                   NFAFGS
174000970530     C                   CLEAR                   NFADFV
174100970530     C                   CLEAR                   NFANPG
174200970530     C**
174300970530     C                   CLEAR                   DSLV53
174400040203     c                   if        brvnpg=1 and brvfgs=simfel
174500040203     c                   eval      d53tfo='G'
174600040203     c                   endif
174700040203     c                   if        brvnpg=1 and brvfgs<>simfel
174800040203     c                   eval      d53tfo='A'
174900040203     c                   endif
175000970529     C                   MOVEL     BRVNPG        D53NPG
175100000203     C                   Z-ADD     BRVNFV        D53NFV
175200970529     C                   MOVEL     BRVFGS        D53FGS
175300970529     C                   MOVEL     'A'           D53ABB
175400970529     C                   MOVEL     LNA           D53LNA
175500031114     C                   MOVEL     lnatfa        D53TFA
175600970529     C                   CALL      'FNLV53R'
175700970529     C                   PARM                    DSLV53
175800101202     c*
175900041014     c* Se errore e categoria "1" cerco in fgv, se cercato in fva
176000041014     c                   if        d53err<>*blanks and brvnpg=1 and d53tfo
176100041014     c                             ='A'
176200041014     C                   CLEAR                   DSLV53
176300041014     c                   eval      d53tfo='G'
176400041014     C                   MOVEL     BRVNPG        D53NPG
176500041014     C                   Z-ADD     BRVNFV        D53NFV
176600041014     C                   MOVEL     BRVFGS        D53FGS
176700041014     C                   MOVEL     SIMFEL        D53FEL
176800041014     C                   MOVEL     'A'           D53ABB
176900041014     C                   MOVEL     LNA           D53LNA
177000041014     C                   MOVEL     lnatfa        D53TFA
177100041014     C                   CALL      'FNLV53R'
177200041014     C                   PARM                    DSLV53
177300041014     c                   endif
177400970530     C*
177500970530     C* NESSUN ERRORE --> FOGLIO TROVATO
177600970530    1C     D53ERR        IFEQ      ' '
177700970529     C                   MOVEL     D53DFV        DATAF
177800970530     C                   MOVEL     D53SPG        SPGF
177900970530     C                   MOVEL     D53DEF        DEFF              1
178000970530     C                   MOVEL     D53NFA        NFAF
178100970530     C                   MOVEL     D53AED        AEDF
178200970530     C**
178300970530    2C     D53TFO        IFEQ      'G'
178400970530     C                   MOVEL     'S'           WFGV
178500970529     C                   ELSE
178600970530    3C     D53TFO        IFEQ      'V'
178700970530     C                   MOVEL     'S'           WFVV
178800970530   X3C                   ELSE
178900970530     C*
179000970530     C                   MOVEL     'S'           WFVA
179100970530     C* SE ABBINATO CONTROLLO IL FOGLIO ARRIVI PER VEDERE
179200970530     C*  SE ESISTE CHIUSO
179300970530    4C     D53NFA        IFGT      0
179400970620     C                   MOVEL     D53NFA        SAVNFA
179500970620     C                   MOVEL     D53NPA        SAVNPA
179600020917     c                   Movel     D53Lai        SavLai
179700970530     C                   CLEAR                   DSLV53
179800970620     C                   MOVEL     SAVNFA        D53NFV
179900970620     C                   MOVEL     SAVNPA        D53NPG
180000970530     C                   MOVEL     'V'           D53TFO
180100970530     C                   MOVEL     SIMFEL        D53FEL
180200970530     C                   MOVEL     SIMFEL        D53FLF
180300020917     c                   Movel     SavLai        D53Fgs
180400970530     C                   CALL      'FNLV53R'
180500970530     C                   PARM                    DSLV53
180600970530     C* TROVATO FOGLIO DI ABBINAMENTO --> SE ERRORE COME SE NON CI FOSS
180700970530    5C     D53ERR        IFEQ      ' '
180800970530     C                   MOVEL     D53FCF        NFAFCF
180900970530     C                   MOVEL     D53SPG        NFASPG
181000970530     C                   MOVEL     D53FGS        NFAFGS
181100970530     C                   MOVEL     D53DFV        NFADFV
181200970530     C                   MOVEL     D53NPG        NFANPG
181300970530     C                   ELSE
181400970530     C                   CLEAR                   NFAF
181500970530     C                   CLEAR                   AEDF
181600970530    5C                   ENDIF
181700970530    4C                   ENDIF
181800970530    3C                   ENDIF
181900970530    2C                   ENDIF
182000970530    1C                   ENDIF
182100961120     C*
182200961120     C                   ENDSR
182300960606     C**************************************************************************
182400960606     C* DETERMINO TERMINAL DI ARRIVO DI BRVFGS
182500960606     C**************************************************************************
182600960606     C     TERFGS        BEGSR
182700970530     C                   CLEAR                   DSLV55
182800970530     C                   MOVEL     LNPB          D55LNP
182900970530     C                   MOVEL     BRVFGS        D55LIN
183000970530     C                   MOVEL     DATAF         D55DRF
183100970530     C                   MOVEL     'A'           D55TPT
183200970530     C                   CALL      'FNLV55R'
183300970530     C                   PARM                    DSLV55
183400970530     C     D55ERR        IFEQ      ' '
183500970530     C                   MOVEL     D55TFA        WTFG
183600970530     C                   ELSE
183700970530     C                   MOVEL     BRVFGS        WTFG
183800970530     C                   ENDIF
183900960606     C*
184000960606     C                   ENDSR
184100941206     C**************************************************************************
184200941206     C* CONTROLLO DETTAGLIO FNBLT00F
184300941206     C**************************************************************************
184400921019     C     CTRBLT        BEGSR
184500941206     C*
184600930604     C                   MOVEL     *ALL'9'       BLPDSE
184700941206     C     KBLP2         SETLL     FNBLT01L
184800941206     C     KBLP2         READE     FNBLT01L                               34
184900921019     C*
185000921019     C     *IN34         DOWEQ     '0'
185100921021     C*
185200930604     C     BLTDSE        IFNE      0
185300921021     C     BLTDSE        ANDLT     BLPDSE
185400921021     C                   MOVEL     BLTDSE        BLPDSE
185500921021     C                   END
185600921021     C*
185700941206     C     KBLP2         READE     FNBLT01L                               34
185800921019     C                   END
185900921021     C*
186000930604     C     BLPDSE        IFEQ      *ALL'9'
186100921021     C                   Z-ADD     0             BLPDSE
186200921021     C                   END
186300941206     C*
186400921019     C                   ENDSR
186500941206     C**************************************************************************
186600941206     C* CONTROLLO SE ALTRI COLLI HANNO DATA ARRIVO E/O COD.ANOM
186700941206     C**************************************************************************
186800921019     C     CTRART        BEGSR
186900941206     C*
187000930603     C* ABILITAZIONI
187100930603     C                   MOVEL     ' '           NOAMP             1
187200930603     C                   MOVEL     ' '           NOAMA             1
187300930604     C  N20              MOVEL     *ALL'9'       ARBDAM
187400060512     c                   clear                   abbnsc
187500930604     C*
187600941206     C     KARB2         SETLL     FNART01L
187700941206     C     KARB2         READE     FNART01L                               34
187800921019     C*
187900921019     C* CONTROLLO SE ALTRI COLLI HANNO LA DATA ARRIVO
188000921019     C     *IN34         DOWEQ     '0'
188100930604     C*
188200930604     C     *IN20         IFEQ      '0'
188300930528     C* SE ALMENO UN COLLO HA ARTDAM = 0  DEVO AZZERARE ARBDTI E ARBHTI
188400930603     C* NOAMA = S NON E' ABILITATO IN BASE ALLA SPUNTA ARRIVI
188500930528     C     ARTDAM        IFEQ      0
188600930528     C                   Z-ADD     0             ARBDTI
188700930528     C                   Z-ADD     0             ARBHTI
188800930603     C                   MOVEL     'S'           NOAMA
188900930528     C                   END
189000921021     C*
189100930603     C     ARTDAM        IFGT      0
189200921021     C     ARTDAM        ANDLT     ARBDAM
189300921021     C                   Z-ADD     ARTDAM        ARBDAM
189400921021     C                   END
189500930603     C*
189600930603     C* NOAMP = S NON E' ABILITATO IN BASE ALLA SPUNTA PART(NON ABBINAT
189700941209     C* ARTABN='S'   ===>  SPUNTA DI FOGLIO ABBINATO
189800941209     C     ARTABN        IFNE      'S'
189900930603     C                   MOVEL     'S'           NOAMP
190000060512     c* Mewmorizzo il segnacollo con la data più alta e verifico se
190100060512     c*  il relativo foglio IDA è stato chiuso per decidere come
190200060512     c*  impostare arbama
190300060512     c                   else
190400060512     c                   if        artdfv>abbdfv  and artflp=0
190500060512     c                   eval      abbnsc=artnsc
190600060512     c                   eval      abbdfv=artdfv
190700060512     c                   endif
190800060512     c                   if        artdut>abbdfv  and artflp>0
190900060512     c                   eval      abbnsc=artnsc
191000060512     c                   eval      abbdfv=artdut
191100060512     c                   endif
191200930603     C                   END
191300930604     C                   END
191400921019     C*
191500921019     C* CONTROLLO SE ALTRI COLLI HANNO L'ANOMALIA
191600921019     C  N06ARTCAN        IFNE      ' '
191700921019     C                   SETON                                        06
191800930604     C   20              SETON                                        34
191900921019     C                   END
192000921019     C*
192100941206     C  N34KARB2         READE     FNART01L                               34
192200921019     C                   END
192300921021     C*
192400930604     C  N20ARBDAM        IFEQ      *ALL'9'
192500921021     C                   Z-ADD     0             ARBDAM
192600921021     C                   END
192700101202     c*
192800060512     c* Se devo abilitare in base alla sola spunta partenza, verifico
192900060512     c*  foglio IDA
193000060512    1c                   if        noama='S' and noamp=' '  and
193100060512     c* Escluso i locali
193200060512     c                             arbtfp<>arbtfa
193300060512     c                   eval      knpg=1
193400140618     c     kspu          setll     fnbrv05l
193500140618     c     kspu          reade     fnbrv05l
193600140618    2c                   dow       not %eof(fnbrv05l)
193700060512     C                   CLEAR                   DSLV53
193800060512     C                   MOVEL     'A'           D53TFO
193900060512     C                   MOVEL     BRVNPG        D53NPG
194000060512     C                   Z-ADD     BRVNFV        D53NFV
194100060512     C                   MOVEL     BRVFGS        D53FGS
194200060512     C                   MOVEL     'A'           D53ABB
194300060512     C                   MOVEL     LNA           D53LNA
194400060512     C                   MOVEL     lnatfa        D53TFA
194500060512     C                   CALL      'FNLV53R'
194600060512     C                   PARM                    DSLV53
194700060512     c
194800060512     c* Escludo la defluenza
194900060512    3c                   if        d53err=' '  and d53def=' '
195000060512    4c                   if        d53nfa>0
195100060512     c* chain
195200060512     c     kfvv          chain     fnfvv01l
195300060512    5c                   if        %found(fnfvv01l) and fvvfcf='S'
195400060512     c                   eval      noamp='0'
195500060512   x5c                   else
195600060512     c                   clear                   noamp
195700060512     c                   leave
195800060512    5c                   endif
195900101202     c*
196000060512   x4c                   else
196100060512     c                   clear                   noamp
196200060512     c                   leave
196300060512    4c                   endif
196400060512    3c                   endif
196500101202     c*
196600140618     c     kspu          reade     fnbrv05l
196700060512    2c                   enddo
196800060512    1c                   endif
196900921019     C*
197000921019     C                   ENDSR
197100941206     C**************************************************************************
197200941206     C*    CONTROLLO SE ALMENO UN COLLO HA LA DATA
197300941206     C**************************************************************************
197400921019     C     CTRBTT        BEGSR
197500941206     C*
197600941206     C                   MOVEL     *ALL'9'       BTPDET
197700031114     C     KBTP2         SETLL     FNBTT11L
197800031114     C     KBTP2         READE     FNBTT11L                               34
197900921019     C*
198000921019     C     *IN34         DOWEQ     '0'
198100921021     C*
198200941206     C     BTTDET        IFNE      0
198300941206     C     BTTDET        ANDLT     BTPDET
198400941206     C                   MOVEL     BTTDET        BTPDET
198500921021     C                   END
198600921019     C*
198700031114     C     KBTP2         READE     FNBTT11L                               34
198800921019     C                   END
198900921021     C*
199000941206     C     BTPDET        IFEQ      *ALL'9'
199100941206     C                   Z-ADD     0             BTPDET
199200921021     C                   END
199300921019     C*
199400921019     C                   ENDSR
199500960417     C**************************************************************************
199600960417     C*    CREAZIONE ANOMALIA 005
199700960417     C**************************************************************************
199800960417     C     CRTA05        BEGSR
199900961120     C* SE SI TRATTA DI SPUNTA LOCALE, CERCO IL FOGLIO ARRIVI
200000961120     C*  CON DATA BORDERO' + 1
200100961120     C     WLOCAL        IFEQ      'S'
200200970127     C* SE NON C'E' LA DATA BORDERO' èRENDO LA DATA DEL GIORNO
200300970127     C     WDBR          IFGT      0
200400961120     C                   MOVE      WDBR          G08INV
200500970127     C                   ELSE
200600970127     C                   MOVE      DATEU         G08INV
200700970127     C                   ENDIF
200800970127     C*
200900961120     C                   MOVEL     '3'           G08ERR
201000961120     C                   CALL      'XSRDA8'
201100961120     C                   PARM                    WLBDA8
201200961120     C*
201300961120     C*  DATA BORDERO' + 1
201400961120     C     G08TGI        ADD       1             GIOTGI
201500961120     C                   CALL      'XSRGI8'
201600961120     C                   PARM                    WGIDAT
201700961120     C*
201800961120     C                   Z-ADD     GIOINV        WDBR1
201900961120     C                   Z-ADD     2             KNPG
202000961120     C     KFVV3         SETLL     FNFVV02L
202100961120     C     KFVV3         READE     FNFVV02L                               31
202200961120     C     *IN31         DOWEQ     *OFF
202300020919     c     fvvatb        andne     ' '
202400961120     C     KFVV3         READE     FNFVV02L                               31
202500961120     C                   ENDDO
202600961120     C  N31              Z-ADD     FVVDFV        W0060             6 0
202700961120     C   31              CLEAR                   W0060
202800961120     C**
202900961120     C** IMPOSTO 1 PRIMI CAMPI
203000031114     C                   Z-ADD     lnatfa        WFGS
203100961120     C                   MOVEL     2             WNPG
203200961120     C                   MOVEL     ' '           WSPG
203300961120     C**
203400101202     C                   SELECT
203500101202     C     W0060         WHENEQ    0
203600961120     C                   Z-ADD     WDBR1         WDFV
203700101202     C                   OTHER
203800961120     C                   Z-ADD     W0060         WDFV
203900101202     C                   ENDSL
204000961120     C*
204100961120     C                   ENDIF
204200961120     C**
204300990518     C                   CLEAR                   FNANM000
204400960417     C                   MOVE      LNP           ANMFLS
204500960417     C                   MOVE      LNA           ANMLNA
204600960417     C                   MOVE      NRS           ANMNRS
204700960417     C                   MOVE      NSC           ANMSCN
204800960417     C                   MOVE      WAAS          ANMAAS
204900960417     C                   MOVE      WLNP          ANMLNP
205000960417     C                   MOVE      WNSP          ANMNSP
205100960417     C* Ricerco fase
205200960417     C                   CLEAR                   DSFASE
205300960417     C                   MOVE      WNPG          D11NPG
205400960417     C                   MOVE      WSPG          D11SPG
205500960417     C                   CALL      'TRUL11R'
205600960417     C                   PARM                    DSFASE
205700960417     C                   MOVE      D11FAS        ANMFSA
205800960417     C                   Z-ADD     WDFV          ANMDAO
205900960417     C                   MOVE      WNPS          ANMNPS
206000961120     C                   MOVE      WCDU          ANMCDU
206100990518     C                   Z-ADD     WNFV          ANMNFV
206200960417     C                   MOVEL     WFGS          ANMFLE
206300960417     C                   MOVE      SAVLNP        WFL2
206400960417     C     ANMLNP        IFGT      0
206500960417     C                   MOVEL     ANMLNP        WFL1
206600960417     C                   ELSE
206700960417     C                   MOVEL     ANMFLS        WFL1
206800960417     C                   ENDIF
206900960417     C                   Z-ADD     5             COMCAA
207000960417     C                   EXSR      CRTANM
207100960417     C                   ENDSR
207200960417     C**************************************************************************
207300960417     C*    CREAZIONE ANOMALIA 015
207400960417     C**************************************************************************
207500960417     C     CRTA15        BEGSR
207600990518     C                   CLEAR                   FNANM000
207700960417     C* Cerco foglio ima della data della spunta cancellata
207800960417     C                   EXSR      RCRIMA
207900960417     C*
208000961118     C     WCREA         IFEQ      ' '
208100960417     C                   MOVE      LNP           ANMFLS
208200960417     C                   MOVE      LNA           ANMLNA
208300960417     C                   MOVE      NRS           ANMNRS
208400960417     C                   MOVE      NSC           ANMSCN
208500960417     C                   MOVE      WAAS          ANMAAS
208600960417     C                   MOVE      WLNP          ANMLNP
208700960417     C                   MOVE      WNSP          ANMNSP
208800960417     C* Ricerco fase
208900960417     C                   CLEAR                   DSFASE
209000960417     C                   MOVE      WNPG          D11NPG
209100960417     C                   MOVE      WSPG          D11SPG
209200960417     C                   CALL      'TRUL11R'
209300960417     C                   PARM                    DSFASE
209400960417     C                   MOVE      D11FAS        ANMFSA
209500960417     C                   MOVE      WVVDFV        ANMDAO
209600960417     C                   MOVE      WFGS          ANMCDU
209700990518     C                   Z-ADD     WNFV          ANMNFV
209800960417     C                   MOVE      WFGS          ANMFLE
209900960417     C                   CLEAR                   WFL2
210000960417     C     ANMLNP        IFGT      0
210100960417     C                   MOVEL     ANMLNP        WFL1
210200960417     C                   ELSE
210300960417     C                   MOVEL     ANMFLS        WFL1
210400960417     C                   ENDIF
210500960417     C*
210600960417     C                   Z-ADD     15            COMCAA
210700960417     C                   EXSR      CRTANM
210800961118     C                   ENDIF
210900960417     C                   ENDSR
211000960416     C**************************************************************************
211100960416     C*    CREAZIONE ANOMALIA
211200960416     C**************************************************************************
211300960416     C     CRTANM        BEGSR
211400960416     C                   Z-ADD     COMCAA        ANMCAA
211500960416     C     COMCAA        IFNE      SAVCAA
211600960416     C                   MOVEL     '7C'          COD
211700960416     C                   MOVEL(P)  ANMCAA        KEY
211800960416     C     KTAB          CHAIN     TABEL                              31
211900960416     C  N31              MOVEL     TBLUNI        DS7C
212000960416     C   31              MOVEL     *BLANKS       DS7C
212100960416     C                   MOVEL     COMCAA        SAVCAA
212200960416     C                   ENDIF
212300050707     c* imposto in anmflo data e ora creazione spunta
212400050707     C                   TIME                    W0140            14 0
212500050707     c
212600050707     C                   CLEAR                   WLBDA8
212700050707     C                   MOVE      w0140         G08DAT
212800050707     C                   CALL      'XSRDA8'
212900050707     C                   PARM                    WLBDA8
213000050707     C                   MOVE      G08INV        wdat6             6 0
213100050707     c                   movel     w0140         wora4             4 0
213200041018     c                   movel     wdat6         anmflo
213300041018     c                   move      wora4         anmflo
213400041018     c* Imposto la zona se trovo la bolla
213500041018     c                   z-add     999           anmfl4
213600041018     c                   select
213700041018     c                   when      wesblt='S'
213800041018     c                   z-add     blpznc        anmfl4
213900041018     c                   when      wesart='S'
214000041018     c                   z-add     arbznc        anmfl4
214100041018     c                   when      wesbtt='S'
214200041018     c                   z-add     btpznc        anmfl4
214300041018     c                   endsl
214400041018     c* Imposto se di valore
214500041018     c                   if        anmnsp>0
214600041018     c     kar5          chain     fiar501l
214700041018    3c                   if        %found(fiar501l)
214800041018     c                   movel     ar5uni        dar5gen
214900041018     c                   else
215000041018     c                   clear                   dar5gen
215100041018    3c                   endif
215200041018     c                   movel     §ar5bva       anmft4
215300041018     c                   endif
215400960416     C*
215500960416     C                   MOVEL     §7CTAN        ANMTAN
215600960416     C                   MOVEL     §7CSAN        ANMSAN
215700960416     C                   MOVEL     §7CAIE        ANMAIE
215800960416     C                   Z-ADD     §7CLID        ANMLID
215900960416     C                   Z-ADD     0             ANMDCH
216000960416     C                   MOVEL     ' '           ANMFSC
216100070424     C                   clear                   ANMcch
216200960416     C* CHIUSURA AUTOMATICA
216300960416     C     §7CCHA        IFEQ      'S'
216400960417     C                   Z-ADD     ANMDAO        ANMDCH
216500960417     C                   MOVE      ANMFSA        ANMFSC
216600960416     C                   END
216700960416     C                   Z-ADD     0             ANMFL1
216800960416     C                   Z-ADD     0             ANMFL2
216900960416     C* ESTERNA
217000000905    1C     §7CAIE        IFEQ      'E'
217100960416     C* VEDO A CHI TRASMETTERE
217200960416     C                   MOVEL     WFL1          ANMFL1
217300960416     C                   MOVEL     WFL2          ANMFL2
217400960416     C* SE UGUALI CANCELLO IL 2°
217500000905    2C     ANMFL1        IFEQ      ANMFL2
217600960416     C                   Z-ADD     0             ANMFL2
217700000905   X2C                   ELSE
217800000905     C                   CLEAR                   DSLV55
217900000905     C                   MOVEL     'P'           D55TPT
218000000905     C                   MOVE      ANMFL1        D55LIN
218100000905     C                   MOVE      DATEU         D55DRF
218200000905     C                   CALL      'FNLV55R'
218300000905     C                   PARM                    DSLV55
218400000905    3C     D55ERR        IFNE      *BLANKS
218500000905     C                   MOVE      ANMFL1        WTFP1
218600000905     C                   ELSE
218700000905     C                   MOVE      D55TFP        WTFP1
218800000905    3C                   END
218900000905     C                   CLEAR                   DSLV55
219000000905     C                   MOVEL     'P'           D55TPT
219100000905     C                   MOVE      ANMFL2        D55LIN
219200000905     C                   MOVE      DATEU         D55DRF
219300000905     C                   CALL      'FNLV55R'
219400000905     C                   PARM                    DSLV55
219500000905    3C     D55ERR        IFNE      *BLANKS
219600000905     C                   MOVE      ANMFL2        WTFP2
219700000905     C                   ELSE
219800000905     C                   MOVE      D55TFP        WTFP2
219900000905    3C                   END
220000000905    3C     WTFP1         IFEQ      WTFP2
220100000905     C                   CLEAR                   ANMFL2
220200000905    3C                   END
220300000905    2C                   ENDIF
220400961120     C                   ENDIF
220500960416     C*
220600961129     C* SE ANOMALIA INTERNA CHE USA SIA FLE CHE LNA LA FLAGGO PER
220700961129     C* NON TRASMETTERLA
220800961129    2C     §7CAIE        IFEQ      'I'
220900961129    2C     §7CUEA        ANDEQ     'E'
221000961129     C                   MOVEL     'T'           ANMFT1
221100961129    2C                   ENDIF
221200961129     C*
221300990518     C                   WRITE     FNANM000
221400960416     C                   ENDSR
221500960417     C**************************************************************************
221600960417     C*    RICERCA FOGLIO IMA
221700960417     C**************************************************************************
221800960417     C     RCRIMA        BEGSR
221900960417     C* DETERMINO FILIALE GESTIONE DELLA LNA DELLA SPUNTA
222000960417     C                   CLEAR                   WFGS
222100960417     C                   CLEAR                   WNFV
222200960417     C                   CLEAR                   WNPG
222300960417     C                   CLEAR                   WSPG
222400020418      * controllo su azcae
222500020422     C     LNA           LOOKUP    L6S                                    30
222600020422     C     *IN30         IFEQ      *OFF
222700020422     C                   MOVE      LNA           WFGS
222800020422     C                   ELSE
222900160419     c                   clear                   dslv55
223000160419     c                   eval      d55tpt='6'
223100160419     c                   eval      d55lin=lna
223200160419     c                   eval      d55drf= *date
223300160419     C                   exsr      FNLV55
223400160419     c                   movel     d55tfa        wfgs
223500160419     c                   endif
223600101202      *
223700960417     C                   MOVEL     '3'           WNPG
223800960417     C* CHAIN SU FOGLIO IMA
223900960417     C     KFVV2         CHAIN     FNFVV02L                           30
224000160419     c                   dow       not *in30 and fvvspg<>'A'
224100160419     C     KFVV2         reade     FNFVV02L                               30
224200160419     c                   enddo
224300000203     C  N30              Z-ADD     FVVNFV        WNFV
224400960417     C  N30              MOVE      FVVNPG        WNPG
224500960417     C  N30              MOVE      FVVSPG        WSPG
224600961118     C**
224700961118     C* SE IL FOGLIO NON C'E' O E' APERTO, NON CREO L'ANOMALIA
224800961118     C                   CLEAR                   WCREA
224900961118     C     *IN30         IFEQ      *ON
225000961118     C     FVVFCF        ORNE      'S'
225100961118     C                   MOVEL     'N'           WCREA             1
225200961118     C                   ENDIF
225300960417     C                   ENDSR
225400960605     C**************************************************************************
225500960605     C*    RICERCA LNP BOLLA
225600960605     C**************************************************************************
225700960605     C     RICLNP        BEGSR
225800031114     c* La tabella 5F qui non viene cercata perchè non serve: mi basta
225900031114     c*  soltanto sapere se c'e' spunta partenza da altro terminal per
226000031114     c*  la creazione della anomalia 05 altrimenti questo pgm non crea
226100031114     c*  nulla: al massimo cancella
226200961120     C                   MOVE      1             KNPG
226300140618     C     KBRV3         SETLL     FNBRV05L
226400140618     C     KBRV3         READE     FNBRV05L                               32
226500031114    1C     *IN32         DOWEQ     *OFF
226600031114    2C     BRVATR        IFEQ      *BLANKS
226700031114    3C     BRVpes        IFNE      LNP
226800061120     C     BRVfgs        andne     pestfp
226900960621     C                   MOVE      BRVFGS        LNPB
227000031114     c                   eval      *in32=*on
227100031114    3C                   END
227200031114    2C                   END
227300140618     C  N32KBRV3         READE     FNBRV05L                               32
227400031114    1C                   ENDDO
227500960605     C                   ENDSR
227600970530     C**************************************************************************
227700970530     C     *INZSR        BEGSR
227800970530     C                   MOVEL     KPJBU         SAVJBU
227900970530     C                   Z-ADD     1             CODUT             1 0
228000970530     C* GIRO DATA DEL GIORNO
228100970530     C                   TIME                    W0140            14 0
228200970530     C                   MOVE      W0140         DATEU             8 0
228300970530     C                   CLEAR                   WLBDA8
228400970530     C                   MOVE      DATEU         G08DAT
228500970530     C                   CALL      'XSRDA8'
228600970530     C                   PARM                    WLBDA8
228700970530     C                   MOVE      G08INV        DATEU
228800970530     C* DEFINIZIONE CAMPI
228900031114     C     *LIKE         DEFINE    d53def        wvvdef
229000031114     C     *LIKE         DEFINE    fvvdfv        wdtrif
229100031114     C     *LIKE         DEFINE    fvvdfv        dspb
229200031114     C     *LIKE         DEFINE    fvvdfv        SVVDFV
229300031114     C     *LIKE         DEFINE    blpLNA        SVVLNA
229400031114     C     *LIKE         DEFINE    blpLNP        SVVLNP
229500031114     C     *LIKE         DEFINE    blpLNA        LNATFA
229600031114     C     *LIKE         DEFINE    blpLNA        LNATFP
229700031114     C     *LIKE         DEFINE    blpLNA        LNPTFP
229800031114     C     *LIKE         DEFINE    blpLNA        PESTFA
229900031114     C     *LIKE         DEFINE    blpLNA        PESTFP
230000000905     C     *LIKE         DEFINE    ANMLNP        WTFP1
230100000905     C     *LIKE         DEFINE    ANMLNP        WTFP2
230200000904     C     *LIKE         DEFINE    FVVNFV        SAVNFA
230300970620     C     *LIKE         DEFINE    FVVNPG        SAVNPA
230400020917     c     *Like         Define    FvvFgs        SavLai
230500970620     C     *LIKE         DEFINE    KPJBU         SAVJBU
230600970530     C     *LIKE         DEFINE    FVVSPG        NFASPG
230700970530     C     *LIKE         DEFINE    FVVFGS        NFAFGS
230800970530     C     *LIKE         DEFINE    FVVFCF        NFAFCF
230900970530     C     *LIKE         DEFINE    FVVDFV        NFADFV
231000970530     C     *LIKE         DEFINE    FVVNPG        NFANPG
231100970530     C     *LIKE         DEFINE    FVVSPG        SPGF
231200970530     C     *LIKE         DEFINE    FVVNFV        NFAF
231300970530     C     *LIKE         DEFINE    FVVDFV        AEDF
231400970530     C     *LIKE         DEFINE    FVVDFV        WVVDFV
231500970530     C     *LIKE         DEFINE    FVVSPG        WVVSPG
231600970530     C     *LIKE         DEFINE    FVVDFV        SPUDFV
231700970530     C     *LIKE         DEFINE    BRVNPS        SPUNPS
231800970530     C     *LIKE         DEFINE    FVVDFV        ENTDFV
231900970530     C     *LIKE         DEFINE    BRVNPS        ENTNPS
232000990412     C     *LIKE         DEFINE    FVVDFV        A56DFV
232100990412     C     *LIKE         DEFINE    BRVNFV        A56NFV
232200990412     C     *LIKE         DEFINE    BRVFGS        A56FGS
232300990412     C     *LIKE         DEFINE    BRVNPS        A56NPS
232400990412     C     *LIKE         DEFINE    FVVSPG        A56SPG
232500990412     C     *LIKE         DEFINE    FVVNPG        A56NPG
232600970530     C     *LIKE         DEFINE    BRVFGS        WTFG
232700970530     C     *LIKE         DEFINE    FVVDFV        DATAF
232800970530     C     *LIKE         DEFINE    FVVSPG        SPUSPG
232900970530     C     *LIKE         DEFINE    BRVNPG        SPUNPG
233000970530     C     *LIKE         DEFINE    BRVFGS        SPUFGS
233100990409     C     *LIKE         DEFINE    BRVFGS        ENTFGS
233200970530     C     *LIKE         DEFINE    BRVNFV        SPUNFV
233300970530     C     *LIKE         DEFINE    FVVFCF        WCHIUS
233400970530     C     *LIKE         DEFINE    ANMCAA        COMCAA
233500970530     C     *LIKE         DEFINE    ANMCAA        SAVCAA
233600970530     C     *LIKE         DEFINE    ANMFL1        WFL1
233700970530     C     *LIKE         DEFINE    ANMFL2        WFL2
233800970530     C     *LIKE         DEFINE    ARBAAS        WAAS
233900970530     C     *LIKE         DEFINE    ANMLNP        WLNP
234000970530     C     *LIKE         DEFINE    ANMNSP        WNSP
234100970530     C     *LIKE         DEFINE    BRVNPS        WNPS
234200000203     C     *LIKE         DEFINE    BRVNFV        WNFV
234300970530     C     *LIKE         DEFINE    ANMCDU        WCDU
234400970530     C     *LIKE         DEFINE    FVVDFV        WDFV
234500970530     C     *LIKE         DEFINE    FVVNPG        WNPG
234600970530     C     *LIKE         DEFINE    FVVSPG        WSPG
234700970530     C     *LIKE         DEFINE    FVVFGS        WFGS
234800970530     C     *LIKE         DEFINE    ARTDAM        WDAM
234900070424     C     *LIKE         DEFINE    ARTDET        WDET
235000070424     C     *LIKE         DEFINE    ARTFLP        WFLP
235100970530     C     *LIKE         DEFINE    ARTDCM        WDCM
235200970530     C     *LIKE         DEFINE    ARTDTR        WDTR
235300970530     C     *LIKE         DEFINE    ARBDBR        WDBR
235400970530     C     *LIKE         DEFINE    ARBFBC        WFBC
235500970530     C     *LIKE         DEFINE    ARBDBR        WDBR1
235600970530     C     *LIKE         DEFINE    FVVDFV        SAVDFV
235700970530     C     *LIKE         DEFINE    FVVFGS        SAVLNP
235800970604     C     *LIKE         DEFINE    ARTDAM        SAVDAM
235900970604     C     *LIKE         DEFINE    BTTDET        SAVDET
236000970530     C     *LIKE         DEFINE    LNP           LNPB
236100970530     C     *LIKE         DEFINE    BRVNPG        KNPG
236200970721     C     *LIKE         DEFINE    GCPFRG        KFRG
236300970721     C                   MOVEL     '0'           KFRG
236400970530     C**
236500031114     C     Kbar37        KLIST
236600041014     C                   KFLD                    BPES
236700031114     C                   KFLD                    LNP
236800031114     C                   KFLD                    LNA
236900031114     C                   KFLD                    NRS
237000031114     C                   KFLD                    NSC
237100041015     C     KFGV          KLIST
237200041015     C                   KFLD                    dis1nfv
237300041015     C                   KFLD                    dis1fgs
237400031114     C     KBOL2         KLIST
237500970530     C                   KFLD                    LNP
237600970530     C                   KFLD                    LNA
237700970530     C                   KFLD                    NRS
237800970530     C                   KFLD                    NSC
237900060512     C     Kspu          KLIST
238000060512     C                   KFLD                    knpg
238100060512     c                   KFLD                    LNP
238200060512     C                   KFLD                    LNA
238300060512     C                   KFLD                    NRS
238400060512     C                   KFLD                    abbnsc
238500970530     C     KARB2         KLIST
238600970530     C                   KFLD                    ARBAAS
238700970530     C                   KFLD                    ARBLNP
238800970530     C                   KFLD                    ARBNRS
238900970530     C                   KFLD                    ARBNSP
239000970721     C     KGCA          KLIST
239100970721     C                   KFLD                    ARBAAS
239200970721     C                   KFLD                    ARBLNP
239300970721     C                   KFLD                    ARBNRS
239400970721     C                   KFLD                    ARBNSP
239500970721     C                   KFLD                    KFRG
239600970530     C     KARB          KLIST
239700970530     C                   KFLD                    ARTAAS
239800970530     C                   KFLD                    ARTLNP
239900970530     C                   KFLD                    ARTNRS
240000970530     C                   KFLD                    ARTNSP
240100970530     C     KBTP2         KLIST
240200031114     C                   KFLD                    BTPflp
240300031114     C                   KFLD                    BTPAAS
240400970530     C                   KFLD                    BTPLNP
240500970530     C                   KFLD                    BTPNRS
240600970530     C                   KFLD                    BTPNSP
240700031114     C     KBTT          KLIST
240800031114     C                   KFLD                    BTTflp
240900031114     C                   KFLD                    BTTAAS
241000970530     C                   KFLD                    BTTLNP
241100970530     C                   KFLD                    BTTNRS
241200970530     C                   KFLD                    BTTNSP
241300970530     C     KBLP2         KLIST
241400970530     C                   KFLD                    BLPAAS
241500970530     C                   KFLD                    BLPLNP
241600970530     C                   KFLD                    BLPNRS
241700970530     C                   KFLD                    BLPNSP
241800031114     C     KBLT          KLIST
241900970530     C                   KFLD                    BLTAAS
242000970530     C                   KFLD                    BLTLNP
242100970530     C                   KFLD                    BLTNRS
242200970530     C                   KFLD                    BLTNSP
242300970530     C     KFVV2         KLIST
242400970530     C                   KFLD                    WNPG
242500970530     C                   KFLD                    WVVDFV
242600970530     C                   KFLD                    WFGS
242700060512     C     KFVV          KLIST
242800060512     C                   KFLD                    d53npa
242900060512     C                   KFLD                    d53nfa
243000060512     C                   KFLD                    d53lai
243100970530     C     KFVV3         KLIST
243200970530     C                   KFLD                    KNPG
243300970530     C                   KFLD                    WDBR1
243400031114     c                   Kfld                    lnatfa
243500970530     C     KTAB          KLIST
243600970530     C                   KFLD                    CODUT
243700970530     C                   KFLD                    COD               2
243800970530     C                   KFLD                    KEY               8
243900970530     C     KBRV3         KLIST
244000970530     C                   KFLD                    KNPG
244100970530     C                   KFLD                    LNP
244200970530     C                   KFLD                    LNA
244300970530     C                   KFLD                    NRS
244400970530     C                   KFLD                    NSC
244500970530     C     KAGB          KLIST
244600970530     C                   KFLD                    AGBTBO
244700970530     C                   KFLD                    AGBAAS
244800970530     C                   KFLD                    AGBLNP
244900970530     C                   KFLD                    AGBNRS
245000970530     C                   KFLD                    AGBNSP
245100970530     C                   KFLD                    AGBAGB
245200001128     C*
245300101202      *
245400030109     c     kFidta        Klist
245500030109     c                   Kfld                    SavAas
245600030109     c                   Kfld                    SavLnp1
245700030109     c                   Kfld                    SavNrs
245800030109     c                   Kfld                    SavNsp
245900030109     c                   Kfld                    SavTrd
246000030109     c                   Kfld                    SavPoc
246100041018     C     Kar5          KLIST
246200041018     C                   KFLD                    anmaas
246300041018     C                   KFLD                    anmLNp
246400041018     C                   KFLD                    anmNRS
246500041018     C                   KFLD                    anmnsp
246600041018     C                   KFLD                    Ktrd
246700970530     C*---------------------------------------------------------------*
246800970530     C*
246900970530     C*  Reperisco livello di chiusura anomalia come IDD
247000970530     C                   MOVEL(P)  2             KEY
247100970530     C                   MOVEL     '7A'          COD
247200970530     C     KTAB          CHAIN     TABEL                              31
247300970530     C  N31              MOVEL     TBLUNI        DS7A2
247400970530     C   31              CLEAR                   DS7A2
247500000825     C*
247600970530     C                   MOVEL     SAVJBU        KPJBU
247700970530     C                   ENDSR
247800970530     C**************************************************************************
247900970530     C     MEMDAT        BEGSR
248000041015     c                   clear                   wnoarrivo
248100041014     c* Faccio 3 memorizzazioni:
248200041014     c* 1) Spunta transito: solo se chi spara è p.o. spunta
248300041014    1c                   if        wesbtt='S' and brvpes=BPES   or
248400041014     c*    oppure spunta di disguido: idem
248500041018     c                             wesbtt<>'S' and wagpar=' ' and wagarr=' '
248600041014     c                             and brvpes=bpes
248700041014    2c                   if        tradfv=0 or spudfv<tradfv
248800970530     C                   MOVEL     BRVNPS        SPUNPS
248900970530     C                   MOVE      BRVNPG        SPUNPG
249000970530     C                   MOVE      BRVNFV        SPUNFV
249100970530     C                   MOVE      BRVFGS        SPUFGS
249200970530     C                   MOVEL     SPGF          SPUSPG
249300041014     C                   MOVEL     spudfv        tradfv
249400041014     C                   MOVEL     BRVNPS        traNPS
249500041014     C                   MOVE      BRVNPG        traNPG
249600041014     C                   MOVE      BRVFGS        traFGS
249700041014     C                   SETON                                        1918
249800041014    2c                   endif
249900041014    1c                   endif
250000041014     c* Per spunta transito o disguido memorizzo la spunta con data/ora cari
250100041014     c*  più alta, per aggiornare altro eventuale disguido/transito
250200041014    1c                   if        wesbtt='S' or
250300041014     c                             wagpar=' ' and wagarr=' '
250400041014     c* Non è cat.partenza non è ne del term.partenza  nè del term.arrivo
250500041015    2c                   if        brvnpg<>1 or  deff='S'
250600041014    3c                   if        wtfg<>lnatfa and brvfle<>lnptfp
250700041014    4c                   if        dis2dfv=0 or spudfv>=dis2dfv
250800041014     C                   MOVEL     BRVNPS        dis2nPS
250900041014     C                   MOVE      BRVNFV        dis2NFV
251000041014     C                   MOVE      BRVFGS        dis2FGS
251100041014     C                   MOVE      spudfv        dis2dfv
251200041015     c                   if        brvscar>0
251300041014     C                   MOVE      brvscar       dis2scar
251400041015     c                   else
251500041015     c                   move      brvimm        dis2scar
251600041015     c                   endif
251700041014    4c                   endif
251800041014    3c                   endif
251900041014    2c                   endif
252000101202     c*
252100041014    1c                   endif
252200101202     c*
252300041014     c* 2) Spunta partenza: il terminal di partenza spunta deve essere
252400041014     c*                     simfel
252500041015    1c                   if        wagpar='S' and brvfle=simfel
252600041015    2c                   if        prtdfv=0 or spudfv<prtdfv
252700041014     C                   MOVEL     BRVNPS        SPUNPS
252800041014     C                   MOVE      BRVNPG        SPUNPG
252900041014     C                   MOVE      BRVNFV        SPUNFV
253000041014     C                   MOVE      BRVFGS        SPUFGS
253100041014     C                   MOVEL     SPGF          SPUSPG
253200041014     C                   MOVEL     spudfv        prtdfv
253300041014     C                   MOVEL     BRVNPS        prtNPS
253400041014     C                   MOVE      BRVNPG        prtNPG
253500041014     C                   MOVE      BRVFGS        prtFGS
253600041014     C                   SETON                                        1917
253700041014    2c                   endif
253800041014     c* Per i danni, cerco la spunta con data più alta
253900041018    2c                   if        dandfv=0 or dandfv<=spudfv
254000041014     C                   MOVEL     BRVNPS        danNPS
254100041014     C                   MOVE      spudfv        dandFV
254200041014     C                   MOVE      BRVFGS        danFGS
254300041018     C                   MOVE      BRVnpg        dannpg
254400041018     C                   MOVE      spgf          danspg
254500041014    2c                   endif
254600041014     c* Se bolla locale, memorizzo a parte dati spunta entrata
254700041014    2c                   if        wsploc<>*blanks and entdfv=0 or
254800041014     c                             wsploc<>*blanks and spudfv<entdfv
254900041014    3c                   if        brvnpg=5 or
255000041014     c                             (brvnpg=3 and spgf='I') or
255100041014     c                             (brvnpg=3 and spgf='P') or
255200041014     c                             (brvnpg=1 and deff<>'S')
255300041014     C                   MOVEL     spudfv        ENTDFV
255400041014     C                   MOVE      BRVNPS        ENTNPS
255500041014     C                   MOVE      BRVFGS        ENTFGS
255600041015     c* non devo consdierare la spunta per l'aggiornamento in arrivo
255700041015     c                   eval      wnoarrivo='N'
255800041014    3c                   endif
255900041014    2c                   endif
256000101202     c*
256100041014    1c                   endif
256200101202     c*
256300041014     c* 2) Spunta arrivo  : il terminal di arrivo spunta deve essere
256400041014     c*                     term.arrivo lna spunta annullata
256500041015    1c                   if        wagarr='S' and brvfle=simfel  and
256600041015     c                             wnoarrivo<>'N'
256700041014    2c                   if        arrdfv=0 or spudfv<arrdfv
256800041014     C                   MOVEL     BRVNPS        SPUNPS
256900041014     C                   MOVE      BRVNPG        SPUNPG
257000041014     C                   MOVE      BRVNFV        SPUNFV
257100041014     C                   MOVE      BRVFGS        SPUFGS
257200041014     C                   MOVEL     SPGF          SPUSPG
257300041014     C                   MOVEL     spudfv        arrdfv
257400041014     C                   MOVEL     BRVNPS        arrNPS
257500041014     C                   MOVE      BRVNPG        arrNPG
257600041014     C                   MOVE      BRVFGS        arrFGS
257700101202     c*
257800050509    3c                   if        brvnpg=5 or
257900050509     c                             (brvnpg=3 and spgf='I') or
258000050509     c                             (brvnpg=3 and spgf='P') or
258100050509     c                             (brvnpg=1 and deff<>'S')
258200050509     c                   eval      arrfl2='P'
258300050509     c                   else
258400050509     c                   eval      arrfl2=' '
258500050509     c                   endif
258600041014     C                   SETON                                        1916
258700041014    2c                   endif
258800041014     c* Per i danni, cerco la spunta con data più alta
258900041018    2c                   if        dandfv=0 or dandfv<=spudfv
259000041014     C                   MOVEL     BRVNPS        danNPS
259100041014     C                   MOVE      spudfv        dandFV
259200041014     C                   MOVE      BRVFGS        danFGS
259300041018     C                   MOVE      BRVnpg        dannpg
259400041018     C                   MOVE      spgf          danspg
259500041014    2c                   endif
259600041014    1c                   endif
259700101202     c*
259800110908     C                   ENDSR
259900031114     C**************************************************************************
260000031114     C* TERMINAL DI PARTENZA E ARRIVO DEL P.O. EFFETTUA SPUNTA
260100031114     C**************************************************************************
260200031114     C     TERPES        BEGSR
260300031114     C* IMPOSTO TERMINAL PARTENZA E ARRIVO
260400031114     C                   CLEAR                   DSLV55
260500031114     C                   MOVEL     WTITER        D55TPT
260600031114     C                   MOVEL     LNPB          D55LNP
260700031114     C                   MOVEL     WDTRIF        D55DRF
260800031114     C                   MOVE      bpes          D55LIN
260900031114     C                   EXSR      FNLV55
261000031114     C                   ENDSR
261100031114     C**************************************************************************
261200031114     C* CERCO TERMINAL PARTENZA LNP COLLO
261300031114     C**************************************************************************
261400031114     C     TERPAR        BEGSR
261500031114     C                   CLEAR                   DSLV55
261600031114     C                   MOVEL     'P'           D55TPT
261700031114     C                   MOVEL     LNPB          D55LIN
261800031114     C                   MOVEL     WDTRIF        D55DRF
261900031114     C                   EXSR      FNLV55
262000031114     C                   MOVE      D55TFP        LNPTFP
262100031114     C**
262200031114     C                   ENDSR
262300031114     C**************************************************************************
262400031114     C* CONTROLLO COSA POSSO AGGIORNARE IN BASE A CHI SONO
262500031114     C**************************************************************************
262600031114     C     CTRAGG        BEGSR
262700031114     C                   CLEAR                   WAGPAR
262800031114     C                   CLEAR                   WAGARR
262900031114     C**
263000031114     C     LNPB          CHAIN     AZORG01L                           34
263100031114     C**
263200031114     C** CERCO TERMINAL DELLA LINEA DI ARRIVO
263300031114     C                   EXSR      TERARR
263400031114     C* FACCIO PARTENZA DELLA STESSA AREA PARTENZA SE:
263500041013    1C***  BAS           IFEQ      LNPAS
263600031126     c* Se c'e'bolla transito x simfel è transito e non cerco altro
263700041013     c     wesbtt        ifne      'S'
263800031114     C*
263900031114     C*  1) P.O. EFFETTUA SPUNTA = LNP SPUNTA (DA BOLLA)
264000031114    2C     BPES          IFEQ      LNPB
264100031114     C*  2) P.O. EFFETTUA SPUNTA E' IL TER.PARTENZA DELLA LNP SPUNTA
264200031114     C*     IN BASE ALLA DATA SPEDIZIONE SE TROVATA LA BOLLA
264300031114     C     BPES          OREQ      LNPTFP
264400031114     C                   MOVEL     'S'           WAGPAR            1
264500031114   X2C                   ELSE
264600031114     C*  3) TER.PARTENZA P.O. EFFETTUA SPUNTA (IN BASE ALLA DATA SPED
264700031114     C*     SE TROVATA LA BOLLA) = TER.PARTENZA LNP SPUNTA SEMPRE IN
264800031114     C*     BASE ALLA DATA SPEDIZIONE SE TROVATA BOLLA
264900031114     C                   MOVEL     'P'           WTITER            1
265000031114     C                   EXSR      TERPES
265100031114    3C     D55TFP        IFEQ      LNPTFP
265200031114     C                   MOVEL     'S'           WAGPAR            1
265300031114    3C                   ENDIF
265400031114    2C                   ENDIF
265500041013    1C                   ENDIF
265600031114     C**
265700031114     C** NON AGGIORNO BOLLE ARRIVO SE SPUNTA DELL'AREA PARTENZA,
265800031114     C*   SPUNTA TIPICA PARTENZA E P.O. SPUNTA = LNA SPUNTA O TFA SPU
265900031114     c* controllo solo se spunta tipica partenza: l'arrivo non mi
266000031114     c*  interessa. non so perchè lo avevo preso in considerazione!!
266100031114    1C     WAGPAR        IFEQ      'S'
266200031114     C* PARTENZA NON DEFLUENZA
266300031114    3C     parnpg        IFEQ      1
266400031114     C     WVVDEF        ANDEQ     '0'
266500031114     C* ENTRATA
266600031114     C     parnpg        OREQ      5
266700031114     C* I.M.P.
266800031114     C     parnpg        OREQ      3
266900031114     C     wvvspg        ANDEQ     'P'
267000031114     C* I.M.I.
267100031114     C     parnpg        OREQ      3
267200031114     C     wvvspg        ANDEQ     'I'
267300031114     C                   GOTO      NOAGAR
267400031114    3C                   ENDIF
267500031114    2C**                 ENDIF
267600031114    1C                   ENDIF
267700031114     C**
267800031114     c     wesbtt        ifne      'S'
267900031114     C* AGGIORNO BOLLE ARRIVO SE :
268000031114     C*  1) P.O. EFFETTUA SPUNTA = LNA SPUNTA
268100031114    1C                   SELECT
268200031114     C     BPES          WHENEQ    lna
268300031114     C                   MOVEL     'S'           WAGARR            1
268400031114    2C     WAGPAR        IFEQ      'S'
268500031114     C                   MOVEL     'A'           WSPLOC            1
268600031114    2C                   ENDIF
268700031114     C*  2) P.O. EFFETTUA SPUNTA E' IL TER.ARRIVO   DELLA LNA SPUNTA
268800031114     C*     IN BASE ALLA DATA SPUNTA
268900031114     C     BPES          WHENEQ    LNATFA
269000031114     C                   MOVEL     'S'           WAGARR            1
269100031114    2C     WAGPAR        IFEQ      'S'
269200031114     C                   MOVEL     'T'           WSPLOC            1
269300031114    2C                   ENDIF
269400031114   X1C                   OTHER
269500031114     C*  3) TER.ARRIVO P.O. EFFETTUA SPUNTA
269600031114     C*     = TER.ARRIVO LNA SPUNTA IN BASE ALLA DATA SPUNTA
269700031114    2C     PESTFA        IFEQ      LNATFA
269800031114     C                   MOVEL     'S'           WAGARR            1
269900041014    2C     WAGPAR        IFEQ      'S'
270000031114     C                   MOVEL     'D'           WSPLOC
270100031114    2C                   ENDIF
270200041014    2C                   ENDIF
270300031114    1C                   ENDSL
270400031114     c*
270500031114    2C                   endif
270600031114     C     NOAGAR        TAG
270700031114     C* DEVO DETERMINARE SE LE BOLLE DELLA LNA SI TROVANO SULL'AS
270800031114     C*  IN CUI MI TROVO
270900041013     C**   LNAAS         IFEQ      BAS
271000041014     C**                 MOVEL     'S'           WSIBOL            1
271100041013     C**                 ENDIF
271200031114     C**
271300031114     C                   ENDSR
271400031114     C**************************************************************************
271500031114     C* CERCO TERMINAL ARRIVO LNA COLLO
271600031114     C**************************************************************************
271700031114     C     TERARR        BEGSR
271800031114     C**
271900031114     C* LINEA DI ARRIVO
272000031114     C     lna           IFNE      SVVLNA
272100031114     C     LNPB          ORNE      SVVLNP
272200031114     C     wvvdfv        ORNE      SVVDFV
272300031114     C*
272400031114     C* TEMINAL DI ARRIVO
272500031114     C                   CLEAR                   DSLV55
272600031114     C                   MOVEL     ' '           D55TPT
272700031114     C                   MOVEL     wvvdfv        D55DRF
272800031114     C                   MOVEL     lna           D55LIN
272900031114     C                   MOVEL     LNPB          D55LNP
273000031114     C                   EXSR      FNLV55
273100031114     C**
273200031114     C                   MOVEL     D55TFA        LNATFA
273300031114     C                   MOVEL     D55TFP        LNATFP
273400031114     C*
273500031114     C                   MOVEL     lna           SVVLNA
273600031114     C                   MOVEL     lnPB          SVVLNP
273700031114     C                   MOVEL     wvvdfv        SVVDFV
273800031114     C                   ENDIF
273900031114     C                   ENDSR
274000031114     C**************************************************************************
274100031114     C* CALL A PGM FNLV55R
274200031114     C**************************************************************************
274300031114     C     FNLV55        BEGSR
274400031114     C                   CALL      'FNLV55R'
274500031114     C                   PARM                    DSLV55
274600031114     C**
274700031114     C     D55ERR        IFNE      ' '
274800031114     C                   MOVE      D55LIN        D55TFA
274900031114     C                   MOVE      D55LIN        D55TFP
275000031114     C                   END
275100031114     C                   ENDSR
