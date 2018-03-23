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
001600990518     F                                     INFDS(FNANM2)
001700070124     FFNBRV07L  IF   E           K DISK
001800070124     FFNBRV03L  IF   E           K DISK
001900070124     F                                     RENAME(FNBRV000:FNBRV3)
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
003600020418     fazcae02l  if   e           k disk
003700030109     fFidta01l  uf a e           K Disk
003800050408     FFISGN03L  UF   E           K DISK
003900911014     D*
004000921020     D §SC             S              1    DIM(5)
004100921020     D §FS             S              1    DIM(5)
004200960417     D L6S             S              3  0 DIM(30)
004300921019     D PARAM           DS
004400941229     D  PARNFV                 2      6  0
004500941229     D  PARNPG                 7      7  0
004600921020     D  LNP                    8     10  0
004700921020     D  LNA                   11     13  0
004800921020     D  NRS                   14     15  0
004900921020     D  NSC                   16     22  0
005000921019     D  CAN                   23     23
005100941212     D  VLS                   24     30  6
005200941212     D  FLG                   31     31
005300941212     D  SIMFEL                92     94  0
005400941212     D  NPS                   95     96  0
005500941212     D  PARFGS                97     99  0
005600970530     D  PARFLF               100    102  0
005700031114     D  BPES                 103    105  0
005800000825     D DS7N          E DS
005900960416     D DS7C          E DS
006000970530     D DS7J          E DS
006100960510     D DS7A2         E DS
006200911016     D KPJBA         E DS
006300960416     D DSLR33        E DS                  EXTNAME(FNLR33DS)
006400970530     D* CALL A PGM CHE CARICA I DATI FOGLI  - FNLV53R
006500970530     D DSLV53        E DS                  EXTNAME(FNLV53DS)
006600970530     D* CALL A PGM CHE CERCA IL TERMINAL DI PARTENZA ARRIVO  - FNLV55R
006700970530     D DSLV55        E DS                  EXTNAME(FNLV55DS)
006800030320     d ds3a          e ds
006900041018     D DAR5GEN       E DS
007000990518     D FNANM2          DS
007100960416     D  ANMNR2               397    400B 0
007200960416     D WLBDA8          DS
007300960416     D  G08DAT                 1      8  0
007400960416     D  G08INV                 9     16  0
007500960416     D  G08ERR                17     17
007600960416     D  G08TGI                18     22  0
007700961120     D WGIDAT          DS                  INZ
007800961120     D  GIODAT                 1      8  0
007900961120     D  GIOINV                 9     16  0
008000961120     D  GIOTGI                17     21  0
008100041015     d
008200041015     D  FFV                    1    897
008300041015     D                                     DIM(299)
008400041015    +D  FGVFFV                 1    240
008500041015    +D  FGVFF2               241    447
008600041015    +D  fgwff3               448    687
008700041015    +D  fgwff4               688    897
008800041015     D                 DS
008900041015    +D  FLP                    1    897
009000041015     D                                     DIM(299)
009100041015    +D  FGVFLP                 1    240
009200041015    +D  FGVFL2               241    447
009300041015    +D  fgwfl3               448    687
009400041015    +D  fgwfl4               688    897
009500960417     D DSUL06        E DS                  EXTNAME(TRUL06DS)
009600960417     D  LIN                    1     90  0
009700960417     D                                     DIM(30)
009800960417     D DSFASE          DS
009900960417     D  D11TLA                 1      1
010000960417     D  D11NPG                 2      2
010100960417     D  D11SPG                 3      3
010200960417     D  D11FAS                 4      4
010300031114     D                 DS
010400031114     D  BLPAAS                 1      4  0
010500031114     D  BLPMGS                 5      8  0
010600031114     D  BLPDSP                 1      8  0
010700031114     D                 DS
010800031114     D  ARBAAS                 1      4  0
010900031114     D  ARBMGS                 5      8  0
011000031114     D  ARBDSP                 1      8  0
011100031114     D                 DS
011200031114     D  BTPAAS                 1      4  0
011300031114     D  BTPMGS                 5      8  0
011400031114     D  BTPDSP                 1      8  0
011500041014     D                 DS
011600041014     D  brvdcs
011700041014     D  brvhcs
011800070124     D  brvscar                1     14  0
011900041015     D                 DS
012000041015     D  brvdfs
012100041015     D  brvhms
012200070124     D  brvimm                 1     14  0
012300020418
012400041018     D ktrd            s                   like(ar5trd)  inz('GEN')
012500020418     d kepa            s                   like(caeepa) inz('6')
012600020418     d ktfa            s                   like(caetfa)
012700020418     d ktfp            s                   like(caetfp) inz
012800020418     d kdde            s                   like(caedde)
012900060512     d Knps            s                   like(brvnps)
013000030109
013100030109     d SavAas          s                   Like(DtaAas)
013200030109     d SavLnp1         s                   Like(DtaLnp)
013300030109     d SavNrs          s                   Like(DtaNrs)
013400030109     d SavNsp          s                   Like(DtaNsp)
013500030109     d SavPoc          s                   Like(DtaPoc)
013600030109     d SavTrd          s                   Like(DtaTrd)
013700030115     d Sav_ArbDam      s                   Like(ArbDam)
013800030115     d Sav_BtpDet      s                   Like(BtpDet)
013900030109     d wFidta          s              1    Inz('0')
014000031127     d wnfaf           s                   like(nfaf)
014100041015     d tradfv          s                   like(d53dfv)
014200041015     d tranps          s                   like(brvnps)
014300041015     d tranpg          s                   like(brvnpg)
014400041015     d trafgs          s                   like(brvfgs)
014500041015     d prtdfv          s                   like(d53dfv)
014600041015     d prtnps          s                   like(brvnps)
014700041015     d prtnpg          s                   like(brvnpg)
014800041015     d prtfgs          s                   like(brvfgs)
014900041015     d arrdfv          s                   like(d53dfv)
015000050509     d arrfl2          s                   like(artfl2)
015100041015     d arrnps          s                   like(brvnps)
015200041015     d arrnpg          s                   like(brvnpg)
015300041015     d arrfgs          s                   like(brvfgs)
015400041015     d dis1dfv         s                   like(d53dfv)
015500041015     d dis1nps         s                   like(brvnps)
015600041015     d dis1nfv         s                   like(d53nfv)
015700041015     d dis1fgs         s                   like(brvfgs)
015800041015     d dis1scar        s                   like(brvscar)
015900041015     d dis1atr         s                   like(brvatr)
016000041015     d dis2dfv         s                   like(d53dfv)
016100041015     d dis2nps         s                   like(brvnps)
016200041015     d dis2nfv         s                   like(d53nfv)
016300041015     d dis2fgs         s                   like(brvfgs)
016400041015     d dis2scar        s                   like(brvscar)
016500041015     d danfgs          s                   like(brvfgs)
016600041015     d dandfv          s                   like(d53dfv)
016700041015     d dannps          s                   like(brvnps)
016800041018     d dannpg          s                   like(brvnpg)
016900041018     d danspg          s                   like(fvvspg)
017000041015     d wnoarrivo       s              1    Inz(' ')
017100041018     d wokanm15        s              1    Inz(' ')
017200060512     d abbnsc          s                   like(artnsc)
017300060512     d abbdfv          s                   like(artdfv)
017400110908     d A_115dfv        s                   like(d53dfv)
017500110908     d A_115fgs        s                   like(brvfgs)
017600110908     d A_115npg        s                   like(brvnpg)
017700110908     d A_115spg        s                   like(fvvspg)
017800110908
017900070424     d DSRECANM      e DS                  extname(fnanm00F)
018000070424     d savRECANM     e DS                  extname(fnanm00F) prefix(S_)
018100030109
018200000000     C*---------------------------------------------------------------*
018300921014     C* RIEPILOGO INDICATORI
018400921014     C*---------------------------------------------------------------*
018500921014     C* 01    - LANCIO PGM ABILITAZIONE MERCE
018600921019     C* 06    - ELIMINO ANOMALIA
018700921019     C* 09    - ELIMINO DATA ENTRATA O DATA ARRIVO
018800041014     C* 16    - ESISTE altra spunta per arrivo
018900041014     C* 17    - ESISTE altra spunta per partenza
019000041014     C* 18    - ESISTE altra spunta per transito
019100041014     C* 19    - ESISTE IN ARCHIVIO UN'ALTRA SPUNTA DEL COLLO
019200041014     C* 20    -
019300921014     C* 32/34 - DI COMODO
019400921014     C* 37    - NON ESISTE BOLLA TRANSITO
019500921014     C* 39    - RECORD ALLOCATO
019600000825     C* 43    - CONTROLLO SU AZORG SE LINEA PARTENZA E' POSTE E SE
019700000825      *         POSSO CREARE ANOMALIE
019800921014     C*---------------------------------------------------------------*
019900020723     C     *ENTRY        PLIST
020000020723     C                   PARM                    KPJBA
020100020723     C                   MOVEL     KPJBU         PARAM
020200020723     c
020300020723     C***
020400020723     C* CARICO TUTTE LE  FILIALI GESTITE DA ALTRE (DA £6)
020500020723     c*   solo una volta
020600020723     C***
020700020723     c                   if        not *in95
020800020723     C                   CLEAR                   DSUL06
020900020723     C                   MOVE      '£6'          D06COD
021000020723     c                   eval      d06esc = 'G'
021100020723     c                   movel     simfel        d06key
021200020723     C                   MOVEL     DSUL06        KPJBU
021300020723     C                   CALL      'TRUL06R'
021400020723     C                   PARM                    KPJBA
021500020723     C                   MOVEL     KPJBU         DSUL06
021600020723     C                   MOVEA     LIN           L6S
021700020723     c                   seton                                        95
021800020723     c                   endif
021900900618     C*---------------------------------------------------------------*
022000921020     C* FLG = 1 PASSO IL VECCHIO RECORD DA ELIMINARE
022100921020     C* FLG = 3 ELIMINAZ.SOLO DEL CODICE ANOMALIA (NON DELETO ANOMALIE)
022200930609     C* FLG = 'C' CHIUDO PGM CON LR E BASTA
022300900619     C*
022400930609     C     FLG           IFEQ      'C'
022500970530     C                   MOVEL     'C'           D55TLA
022600970530     C                   CALL      'FNLV55R'
022700970530     C                   PARM                    DSLV55
022800970530     C                   MOVEL     'C'           D53TLA
022900970530     C                   CALL      'FNLV53R'
023000970530     C                   PARM                    DSLV53
023100970604     C                   MOVEL     'C'           SA2TLA
023200970604     C                   CALL      'FNLSA2R3'
023300970604     C                   PARM                    SA2TLA
023400970604     C                   PARM                    SA2TRC
023500970604     C                   PARM                    SA2DTS
023600970604     C                   PARM                    SA2LNA
023700970530     C**
023800930609     C                   SETON                                        LR
023900930609     C*
024000930609     C                   ELSE
024100930609     C*
024200911016     C* IMPOSTAZIONI INIZIALI
024300911014     C                   EXSR      CARTAB
024400921019     C*
024500921020     C                   Z-ADD     1             B                 1 0
024600920424     C*
024700920424     C* SE TROVO RECORD ALLOCATI RITENTO 3 VOLTE
024800920424     C     B             DOWLE     3
024900031114     C                   EXSR      UPDbol
025000920424     C                   ADD       1             B
025100920424     C                   END
025200921019     C*
025300921019     C     B             IFEQ      3
025400921019     C                   MOVEL     'E'           FLG
025500921019     C                   END
025600911014     C*
025700921020     C                   MOVEL     PARAM         KPJBU
025800930609     C*
025900930609     C                   RETURN
026000930609     C                   END
026100941206     C**************************************************************************
026200941206     C*    CARICO TABELLE
026300941206     C**************************************************************************
026400911014     C     CARTAB        BEGSR
026500970530     C**
026600970529     C                   CLEAR                   DSLV53
026700970529     C                   MOVEL     PARNPG        D53NPG
026800970529     C                   MOVEL     PARNFV        D53NFV
026900970529     C                   MOVEL     PARFGS        D53FGS
027000970530     C                   MOVEL     PARFLF        D53FLF
027100970529     C                   MOVEL     SIMFEL        D53FEL
027200970529     C                   CALL      'FNLV53R'
027300970529     C                   PARM                    DSLV53
027400970529     C     D53ERR        IFEQ      ' '
027500970529     C                   MOVEL     D53SPG        WVVSPG
027600970529     C                   MOVE      D53DFV        WVVDFV
027700031114     C                   MOVE      D53DEF        WVVDEF
027800970529     C                   ELSE
027900970529     C                   MOVE      DATEU         WVVDFV
028000970616     C                   CLEAR                   WVVSPG
028100031114     C                   CLEAR                   WVVDEF
028200961120    1C                   END
028300911112     C*
028400921020     C* FASE DEL FOGLIO DELLA SPUNTA CHE ELABORO
028500960405     C                   MOVEL(P)  PARNPG        KEY
028600960405     C                   MOVEL     '7N'          COD
028700921019     C     KTAB          CHAIN     TABEL                              31
028800921019     C  N31              MOVEL     TBLUNI        DS7N
028900921021     C*
029000921021     C                   MOVEL     §7NFS1        §FS(1)
029100921021     C                   MOVEL     §7NFS2        §FS(2)
029200921021     C                   MOVEL     §7NFS3        §FS(3)
029300921021     C                   MOVEL     §7NFS4        §FS(4)
029400921021     C                   MOVEL     §7NFS5        §FS(5)
029500921021     C*
029600921021     C                   MOVEL     §7NSC1        §SC(1)
029700921021     C                   MOVEL     §7NSC2        §SC(2)
029800921021     C                   MOVEL     §7NSC3        §SC(3)
029900921021     C                   MOVEL     §7NSC4        §SC(4)
030000921021     C                   MOVEL     §7NSC5        §SC(5)
030100921020     C*
030200930604     C                   Z-ADD     1             I                 3 0
030300970616     C     WVVSPG        LOOKUP    §SC(I)                                 31
030400921020     C   31              MOVEL     §FS(I)        FASE              1
030500921020     C  N31              MOVEL     §FS(1)        FASE
030600060512     c*
030700060512     c* Vedo se la pistola è manuale
030800060512     c
030900060512     c                   MOVEL     nps           knps
031000060512     c                   exsr      TIPPIS
031100060512     c
031200060512     c                   movel     §7jrps        spurps            1
031300060512     c*
031400060512     c
031500921019     C*
031600911014     C                   ENDSR
031700941206     C**************************************************************************
031800911018     C*--- AGGIORNO RECORD BOLLE ARRIVI O CREO ANOMALIA --------------*
031900921020     C* ANNULLO DATA ARRIVO SULLE BOLLE SE:
032000921020     C*  1) NON CI SOLO ALTRE SPUNTE DEL COLLO
032100921020     C*  2) SE LA DATA ARRIVO CORRISPONDE ALLA DATA FOGLIO DELLA SPUNTA
032200921021     C*
032300921021     C* MODIFICO DATA DI ARRIVO SULLE BOLLE E COLLO :
032400921021     C*  1) SE ANNULLO LA SPUNTA CHE MI HA IMPOSTATO LA DATA DI ARRIVO
032500921021     C*     E CI METTO LA DATA FOGLIO DI UN'ALTRA SPUNTA DI QUEL COLLO
032600921020     C*
032700921020     C* ANNULLO ANOMALIA    SULLE BOLLE SE:
032800921020     C*  1) NON C'E' NESSUN'ALTRA SPUNTA CON UNA ANOMALIA
032900921020     C*  2) SE L'ANOMALIA SPUNTA E' = ALL'ANOMALIA SUL COLLO
033000941206     C**************************************************************************
033100031114     C     UPDBOL        BEGSR
033200941230     C*
033300070424     c                   clear                   AggioDTI          1
033400921019     C                   MOVEL     ' '           SAVCAN
033500941206     C                   MOVEL     *ZEROS        SPUDFV
033600941206     C                   MOVEL     *ZEROS        SPUNPS
033700960416     C                   MOVEL     *ZEROS        SPUNPG
033800960417     C                   MOVEL     *ZEROS        SPUSPG
033900960416     C                   MOVEL     *ZEROS        SPUNFV
034000960513     C                   MOVEL     *ZEROS        SPUFGS
034100961121     C                   MOVEL     *ZEROS        ENTDFV
034200961121     C                   MOVEL     *ZEROS        ENTNPS
034300990409     C                   MOVEL     *ZEROS        ENTFGS
034400990412     C                   MOVEL     *ZEROS        A56DFV
034500990412     C                   MOVEL     *ZEROS        A56NPS
034600990412     C                   MOVEL     *ZEROS        A56FGS
034700990412     C                   MOVEL     *ZEROS        A56NFV
034800990412     C                   MOVEL     *BLANKS       A56SPG
034900990412     C                   MOVEL     *BLANKS       A56NPG
035000960416     C                   CLEAR                   SAVCAA
035100960416     C                   CLEAR                   WAAS
035200960416     C                   CLEAR                   WLNP
035300960416     C                   CLEAR                   WNSP
035400960417     C                   CLEAR                   WDAM
035500960417     C                   CLEAR                   WDCM
035600961120     C                   CLEAR                   WDBR
035700961120     C                   CLEAR                   WFBC
035800101202     c*
035900960613     C                   CLEAR                   LNPB
036000031114     C                   CLEAR                   WESART
036100031114     C                   CLEAR                   WESBLT
036200031114     C                   CLEAR                   WESBTT
036300041015     C                   MOVEL     *ZEROS        tradfv
036400041015     C                   MOVEL     *ZEROS        tranps
036500041015     C                   MOVEL     *ZEROS        tranpg
036600041015     C                   MOVEL     *ZEROS        trafgs
036700041015     C                   MOVEL     *ZEROS        prtdfv
036800041015     C                   MOVEL     *ZEROS        prtnps
036900041015     C                   MOVEL     *ZEROS        prtnpg
037000041015     C                   MOVEL     *ZEROS        prtfgs
037100041015     C                   MOVEL     *ZEROS        arrdfv
037200050509     c                   clear                   arrfl2
037300041015     C                   MOVEL     *ZEROS        arrnps
037400041015     C                   MOVEL     *ZEROS        arrnpg
037500041015     C                   MOVEL     *ZEROS        arrfgs
037600041015     C                   MOVEL     *ZEROS        dis1nps
037700041015     C                   MOVEL     *ZEROS        dis1nfv
037800041015     C                   MOVEL     *ZEROS        dis1fgs
037900041015     C                   MOVEL     *ZEROS        dis1dfv
038000041015     C                   MOVEL     *ZEROS        dis1scar
038100041015     C                   MOVEL     *ZEROS        dis1atr
038200041015     C                   MOVEL     *ZEROS        dis2nps
038300041015     C                   MOVEL     *ZEROS        dis2nfv
038400041015     c                   MOVEL     *ZEROS        dis2fgs
038500041015     C                   MOVEL     *ZEROS        dis2dfv
038600041015     C                   MOVEL     *ZEROS        dis2scar
038700041015     C                   MOVEL     *ZEROS        dannps
038800041015     C                   MOVEL     *ZEROS        dandfv
038900041015     C                   MOVEL     *ZEROS        danfgs
039000041018     C                   MOVEL     *ZEROS        dannpg
039100041018     C                   MOVEL     '  '          danspg
039200110908     c
039300110908     c                   clear                   a_115dfv
039400110908     C                   MOVEL     *ZEROS        a_115fgs
039500110908     C                   MOVEL     *ZEROS        a_115npg
039600110908     C                   MOVEL     '  '          a_115spg
039700930604     C                   SETOFF                                       061920
039800041015     C                   SETOFF                                       161718
039900031114     c*
040000031114     C* TERMINAL partenza DI BPES
040100031114     C                   MOVEL     'P'           WTITER            1
040200031114     C                   MOVEL     wvvdfv        WDTRIF
040300031114     C                   EXSR      TERPES
040400031114     C                   Z-ADD     D55TFP        PESTFP
040500031114     c*
040600031114     c* Prima di tutto verifico chi sono come spunta e quindi cosa posso
040700031114     c*  aggiornare ...
040800031114     c*
040900031126     c* Se trovo la bolla transito la considero sempre buona e non
041000031114     c*  controllo i terminal (c'e' un problema a livello di eccezioni ter
041100031114     C     KBAR37        CHAIN     FNBTT37L                           3739
041200031114     C   39              GOTO      ENDUPD
041300031114     C  N37KBTT          CHAIN     FNBTP11L                           37
041400031114     C**
041500031114    1C     *IN37         IFEQ      *OFF
041600031114     C                   MOVEL     'S'           WESBTT            1
041700031114     C                   MOVEL     BTTLNP        LNPB
041800031114     C                   MOVEL     BTPDSP        DSPB
041900031114     C                   MOVEL     BTPDSP        WDTRIF
042000031114     C                   MOVEL     BTPTFP        LNPTFP
042100031114   X1C                   ELSE
042200031114     C                   MOVEL     'N'           WESBTT
042300031114     C**
042400031114     C** VEDO SE TROVO LA BOLLA IN BLT
042500041216     C     KBOL2         CHAIN(N)  FNBLT27L                           3239
042600031114     C   39              GOTO      ENDUPD
042700031114     C  N32KBLT          CHAIN(N)  FNBLP01L                           32
042800031114     C**
042900031114    2C     *IN32         IFEQ      *OFF
043000031114     C                   MOVEL     'S'           WESBLT            1
043100031114     C                   MOVEL     BLTLNP        LNPB
043200031114     C                   MOVEL     BLPTFP        LNPTFP
043300031114     C                   MOVEL     BLPDSP        WDTRIF
043400031114     C                   MOVEL     BLPDSP        DSPB
043500031114   X2C                   ELSE
043600031114     C                   MOVEL     'N'           WESBLT
043700031114     C** SE NON TROVO LA BOLLA PARTENZA CERCO NEGLI ARRIVI
043800031114     C     KBOL2         CHAIN     FNART27L                           3239
043900031114     C   39              GOTO      ENDUPD
044000031114     C  N32KARB          CHAIN     FNARB01L                           32
044100031114     C**
044200031114    3C     *IN32         IFEQ      *OFF
044300031114     C                   MOVEL     'S'           WESART            1
044400031114     C                   MOVEL     ARTLNP        LNPB
044500031114     C                   MOVEL     ARBDSP        DSPB
044600031114     C                   MOVEL     ARBDSP        WDTRIF
044700031114     C                   MOVEL     ARBTFP        LNPTFP
044800031114   X3C                   ELSE
044900031114     C                   MOVEL     'N'           WESART
045000031114     C** IMPOSTO LNPB = BARLNP
045100031114     C                   MOVEL     lnp           LNPB
045200031114     C                   Z-ADD     wvvdfv        DSPB
045300031114     C**
045400031114     C** SE NON TROVATA MAI LA BOLLA, CERCO SE ESISTE LA EVENTUALE
045500031126     C*   SPUNTA PARTENZA
045600031114    4C     nrs           IFGT      *ZEROS
045700031114     C                   EXSR      RICLNP
045800031114    4C                   ENDIF
045900031114     c* prendo terminal di partenza della linea partenza calcolata
046000031114     C                   Z-ADD     wvvdfv        WDTRIF
046100031114     C                   EXSR      TERPAR
046200031114    3C                   ENDIF
046300031114    2C                   ENDIF
046400031114    1C                   ENDIF
046500031114     C**
046600031114     C** CONTROLLO TRA P.O. SPUNTA E LINEE SPUNTE CHE AGGIONAMENTI
046700031114     C**  POSSO FARE
046800031114     c*
046900031114     C* TERMINAL partenza DI BPES
047000031114     C                   MOVEL     'A'           WTITER
047100031114     C                   MOVEL     wvvdfv        WDTRIF
047200031114     C                   EXSR      TERPES
047300031114     C                   Z-ADD     D55TFA        PESTFA
047400031114     c
047500031114     C                   EXSR      CTRAGG
047600960416     C*
047700960416     C                   CLEAR                   WCHIUS
047800960416     C                   CLEAR                   WDFV
047900960416     C                   CLEAR                   WNPS
048000960416     C                   CLEAR                   WNFV
048100960416     C                   CLEAR                   WNPG
048200960417     C                   CLEAR                   WSPG
048300960416     C                   CLEAR                   WFGS
048400961120     C                   CLEAR                   WCDU
048500960416     C                   CLEAR                   WSPUN
048600031114     C                   CLEAR                   WLOCAL
048700960417     C                   CLEAR                   SAVDFV
048800960417     C                   CLEAR                   SAVLNP
048900970127     C                   CLEAR                   WGIAC
049000930604     C**
049100070124     C     KBOL2         SETLL     FNBRV07L
049200070124     C     KBOL2         READE     FNBRV07L                               32
049300930604     C*
049400970625    1C     *IN32         DOWEQ     '0'
049500961120     C* LA SPUNTA LETTA NON DEVE ESSERE QUELLA CHE STO ANNULLANDO
049600970625    2C     BRVNPG        IFNE      PARNPG
049700950118     C     BRVNFV        ORNE      PARNFV
049800950118     C     BRVFGS        ORNE      PARFGS
049900961120     C**
050000041013     C* CODICE ANOMALIA COLLO  tengo l'ultima che trovo
050100041013    3C     brvCAN        IFne      ' '
050200950118     C                   MOVEL     BRVCAN        SAVCAN            1
050300961118    3C                   END
050400970625     C**
050500970625     C* SOLO SE FLAG ='1' FACCIO GLI ALTRI CONTROLLI RELATIVI ALLE
050600970625     C* SPUNTE
050700970625    3C     FLG           IFEQ      '1'
050800961120     C**
050900961120     C* CHAINO I FOGLI A SECONDA DELLA CATEGORIA E FIL.GESTIONE
051000961120     C                   EXSR      CHAFV
051100970530     C**
051200970530     C* CONTROLLO IL TIPO PISTOLA DELLA SPUNTA LETTA
051300060512     c                   movel     brvnps        knps
051400970530     C                   EXSR      TIPPIS
051500110908     c
051600110908     c* Verifico se presente altra spunta reale sul sistema per non
051700110908     c*  riaprire  eventuale anomalia 115
051800110908    4C     §7JRPS        IFNE      'A'
051900110908     c                   eval      a_115dfv=dataf
052000110908     C                   MOVE      BRVFGS        a_115FGS
052100110908     C                   MOVE      BRVnpg        a_115npg
052200110908     C                   MOVE      spgf          a_115spg
052300110908     c                   endif
052400110908     c
052500041015     c* Per spunta transito o disguido memorizzo la spunta con data/ora cari
052600041015     c*  più alta, per aggiornare altro eventuale disguido/transito
052700041015     c                   if        d53err=' '  or d53err='6'
052800041015    4c                   if        wesbtt='S' or
052900041015     c                             wagpar=' ' and wagarr=' '
053000041015     c* Categoria partenza
053100041015    5c                   if        brvnpg=1 and deff=' '
053200041015    6c                   if        dis1dfv=0 or dataf>=dis1dfv
053300041015     C                   MOVEL     BRVNPS        dis1nPS
053400041015     C                   MOVE      BRVNFV        dis1NFV
053500041015     C                   MOVE      BRVFGS        dis1FGS
053600041015     C                   MOVE      dataf         dis1dfv
053700041015     c                   if        brvscar>0
053800041015     C                   MOVE      brvscar       dis1scar
053900041015     c                   else
054000041015     c                   move      brvimm        dis1scar
054100041015     c                   endif
054200041015     C                   MOVE      brvatr        dis1atr
054300041015    6c                   endif
054400041015    5c                   endif
054500041015    4c                   endif
054600041015    4c                   endif
054700101202     c*
054800041015     C* CONSIDERO SOLO SE NON C'E' ERRORE
054900041015    4C     D53ERR        IFEQ      ' '
055000960606     C*
055100961120     C*  ESCLUDO LE SPUNTE AUTOMATICHE CHE NON POSSONO AGGIORNARE LA
055200961120     C*  DATA DI ARRIVO
055300970625    5C     §7JRPS        IFNE      'A'
055400970530     C* CONSIDERO COMUNQUE LA PISTOLA AUTOMATICA DEI TRANSITI PERCHE'
055500970530     C*  AGGIORNA LA DATA DI ARRIVO TRANSITO
055600970530     C     BRVNPS        OREQ      89
055700961120     C*
055800961120     C* DETERMINO TERMINAL DI ARRIVO DELLA FILIALE GESTIONE SPUNTA LETT
055900961120     C                   EXSR      TERFGS
056000961120     C*
056100031126     C* SE DEVO AGGIORNARE L'ARRIVO: CONSIDERO ANCHE LE SPUNTE DEL TERMINAL
056200031126     C*   ALTRIMENTI  CONSIDERO SOLO LE SPUNTE DI SIMFEL
056300031126    6C                   IF        BRVFLE=SIMFEL  OR
056400031126     C                             WAGARR='S' AND WTFG  =LNATFA
056500031126     c* FOGLI VARI
056600970625    7C     WFVV          IFEQ      'S'
056700961120     C*
056800041014     C* SE TROVATA SPUNTA vedo se memorizzare i dati
056900970530     C                   MOVEL     DATAF         SPUDFV
057000970530     C                   EXSR      MEMDAT
057100990412     C**
057200990412     C** MEMORIZZO SE C'E' UN'ALTRA SPUNTA IMA/IMG/CONSEGNE
057300990412     C** PER ANOMALIA DI DISGUIDO
057400990412    8C     BRVNPG        IFEQ      3
057500990412     C     NFASPG        ANDEQ     'A'
057600990412     C     BRVNPG        OREQ      3
057700990412     C     NFASPG        ANDEQ     'G'
057800990412     C     BRVNPG        OREQ      4
057900990412     C                   MOVEL     DATAF         A56DFV
058000990412     C                   MOVE      BRVNPS        A56NPS
058100990412     C                   MOVE      BRVFGS        A56FGS
058200990412     C                   MOVE      BRVNPG        A56NPG
058300990412     C                   MOVE      BRVNFV        A56NFV
058400990412     C                   MOVE      NFASPG        A56SPG
058500990412    8C                   ENDIF
058600041015     c*
058700041015     c                   else
058800041015     c* La spunta partenza la consdiero per le partenze e transiti
058900041015     c* solo se non è autogenerata
059000041015     c                   if        brvscar>0 and wagarr<>'S'
059100041015     C                   MOVEL     DATAF         SPUDFV
059200041015     C                   EXSR      MEMDAT
059300990412    7C                   ENDIF
059400041015    7C                   ENDIF
059500961120     C**
059600961120     C* FOGLI PARTENZA LOCALE
059700970530     C* CONSIDERO SOLO SE SI TRATTA DI  DEFLUENZA
059800970625    7C     WFGV          IFEQ      'S'
059900970530     C     DEFF          ANDEQ     'S'
060000970530     C* SE TROVATA SPUNTA MEMORIZZO ANCHE GLI ALTRI DATI
060100970530     C                   MOVEL     DATAF         SPUDFV
060200970530     C                   EXSR      MEMDAT
060300970625    7C                   ENDIF
060400961120     C**
060500970625    6C                   END
060600970625    5C                   END
060700960417     C*
060800960417     C* SE SPUNTA PARTENZA MEMORIZZO QUELLA CON DATA PIù RECENTE
060900960417     C* E VERIFICO SE RELATIVA A FV ABBINATO A FOGLIO ARRIVI GIà CHIUSO
061000961120     C*  PERCHE' DEVO CREARE L'ANOMALIA 05-COLLO PARTITO NON ARRIVATO
061100041014    5C     *IN16         IFEQ      *OFF
061200031126     c     wagarr        andeq     'S'
061300961120     C     BRVNPG        ANDEQ     1
061400970530     C     DEFF          ANDNE     'S'
061500961118     C**
061600970625    6C     WFVA          IFEQ      'S'
061700970530     C     DATAF         ANDGT     SAVDFV
061800031127     c* rimemorizzo anche se trovo il foglio gisuto abbinato,
061900031127     C*  In caso di transito infatti su As unico leggo anche la spunta part
062000031127     c*  originale non diretta all'arrivo. Il foglio lo trova ma mi da non
062100031127     c*  abbinato (xchè abbinato al transito e non all'arrivo)
062200031127     c*  quindi se poi leggo il foglio giusto lo devo memorizzare
062300031127     c*  Se troviamo soluzione migliore...meglo!!
062400031127    6C     WFVA          oreq      'S'
062500031127     C     DATAF         ANDle     SAVDFV
062600031127     c     wnfaf         andeq     0
062700031127     c     nfaf          andgt     0
062800970530     C                   MOVE      DATAF         SAVDFV
062900000904     C                   MOVE      BRVFGS        SAVLNP
063000960417     C                   MOVE      BRVNPS        WNPS
063100961120     C                   MOVEL     ' '           WLOCAL            1
063200031127     C                   z-add     nfaf          Wnfaf
063300961120     C**
063400970530     C* SE FVP ABBINATO ED HO TROVATO FOGLIO ARRIVI, MEMORIZZO  I DATI
063500970625    7C     NFAF          IFGT      0
063600970530     C     NFAFCF        ANDNE     *BLANKS
063700960417     C                   MOVE      'S'           WCHIUS
063800970530     C                   MOVE      BRVNFV        WNFV
063900970530     C                   MOVE      BRVFGS        WCDU
064000970530     C                   MOVE      NFADFV        WDFV
064100970530     C                   MOVE      NFANPG        WNPG
064200970530     C                   MOVE      NFASPG        WSPG
064300970530     C                   MOVE      NFAFGS        WFGS
064400970625   X7C                   ELSE
064500960417     C                   CLEAR                   WCHIUS
064600970625    7C                   END
064700970625    6C                   END
064800961118     C**
064900961118     C** FOGLIO LOCALE: COME SE FOSSE SEMPRE ABBINATO E F.ARRIVI CHIUSO
065000031114    6C     wagarr        IFEQ      'S'
065100970530     C     WFGV          ANDEQ     'S'
065200970530     C     DATAF         ANDGT     SAVDFV
065300970530     C                   MOVE      DATAF         SAVDFV
065400970530     C                   MOVE      BRVFGS        SAVLNP
065500961120     C                   MOVE      BRVNPS        WNPS
065600970530     C                   MOVEL     'S'           WLOCAL            1
065700970530     C*
065800970530     C                   MOVE      'S'           WCHIUS
065900970530     C                   MOVE      BRVNFV        WNFV
066000961120     C                   MOVE      SIMFEL        WCDU
066100961120     C**
066200970625    6C                   ENDIF
066300970625    5C                   ENDIF
066400961118     C**
066500961118     C** CONTROLLO LE SPUNTE AI FINI IMA:
066600961118     C**  -SE TROVO UNA SPUNTA CON DATA> DELLA DATA SPUNTA CHE CANCELLO
066700961118     C**   SICURAMENTE NON DEVO CREARE ANOMALIA COLLO MANCANTE
066800961120     C**  -SE TROVO UNA SPUNTA CON DATA = ALLA DATA SPUNTA CHE CANCELLO
066900961118     C**   CONSIDERO VALIDE SOLO LE CATEGORIA 3  4 E 5
067000961118     C**  - ESCUDO SEMPRE LE SPUNTE CATEGORIA 1 PERCHE' NON SONO CONSID
067100961118     C**    NEMMENO DAL PGM DELL'IMA
067200031114    5C     wagarr        IFEQ      'S'
067300961120     C     WFVV          ANDEQ     'S'
067400961118     C*
067500970625    6C     DATAF         IFGT      WVVDFV
067600960417     C                   MOVE      '1'           WSPUN             1
067700970625    6C                   ENDIF
067800961118     C*
067900970625    6C     DATAF         IFEQ      WVVDFV
068000970625    7C     BRVNPG        IFEQ      3
068100961118     C     BRVNPG        OREQ      4
068200961118     C     BRVNPG        OREQ      5
068300041013     C     BRVNPG        OREQ      8
068400961118     C                   MOVE      '1'           WSPUN             1
068500970625    7C                   ENDIF
068600970625    6C                   ENDIF
068700961118     C*
068800970625    5C                   END
068900970625    4C                   END
069000970625    3C                   END
069100970625    2C                   END
069200930604     C*
069300070124     C     KBOL2         READE     FNBRV07L                               32
069400970625    1C                   ENDDO
069500031114     C**
069600031114     C** A G G I O R N A M E N T O   B O L L E   P A R T E N Z A
069700031114     C**
069800031114     c** elimino data su collo
069900031114    1C     wagpar        IFEQ      'S'
070000941230     C     KBOL2         CHAIN     FNBLT27L                           3239
070100990409     C*
070200031114    2C     *IN39         IFEQ      *OFF
070300990409     C     KBOL2         CHAIN     FNDCD02L                           3839
070400060512     c* La pistola è corrispondente se uguali, o entrambe manuali
070500060512   2ac                   if        not *in38 and not *in39
070600060512     c                   movel     dcdnps        knps
070700060512     c                   exsr      TIPPIS
070800101202     c*
070900060512     C     DCDNPS        IFEQ      NPS
071000990409     C     DCDDFV        ANDEQ     WVVDFV
071100060512     C     spurps        oreq      'M'
071200060512     C     §7jrps        andeq     spurps
071300990409     C                   MOVEL     'S'           WDCD              1
071400990409     C                   ELSE
071500990409     C                   UNLOCK    FNDCD02L
071600031114    3C                   ENDIF
071700060512   2aC                   ENDIF
071800031114    2C                   ENDIF
071900921013     C*
072000921013     C   39              GOTO      ENDUPD
072100101202     c*
072200031203     C* se c'e' la bolla aggiorno
072300031203   1ac                   if        not *in32
072400970530     C                   MOVE      BLTAAS        WAAS
072500970530     C                   MOVE      BLTLNP        WLNP
072600970530     C                   MOVE      BLTNSP        WNSP
072700920810     C*
072800970530    2C     SAVCAN        IFEQ      ' '
072900921020     C     CAN           ANDEQ     BLTCAN
073000921020     C                   MOVEL     ' '           BLTCAN
073100961121    2C                   END
073200921019     C*
073300101202     c*
073400970530     C* CANCELLO A SOSTITUISCO LA DATA ENTRATA SOLO SE CORRISPONDONO
073500970530     C*  PISTOLA E DATA
073600970530    2C     FLG           IFEQ      '1'
073700060512     c                   movel     bltnpe        knps
073800060512     c                   exsr      TIPPIS
073900060512   2aC     BLTNPE        ifeq      NPS
074000970530     C     BLTDSE        ANDEQ     WVVDFV
074100060512     c* oppure se sono entrambe manuali
074200060512     c     spurps        oreq      'M'
074300060512     c     §7jrps        andeq     spurps
074400970530     C*
074500961121     C* SE NON TROVATA NESSUNA SPUNTA, NEMMENO ENTRATA PULISCO LE DATE
074600041014     c                   select
074700041014    3C     *IN17         wheneq    *OFF
074800961121     C     ENTDFV        ANDEQ     0
074900041014     C     prtDFV        ANDEQ     0
075000970530     C                   MOVEL     *ZEROS        BLTDSE
075100970530     C                   MOVEL     *ZEROS        BLTNPE
075200060512     C                   MOVEL     ' '           BLTCAN
075300961121     C* SE COLLO LOCALE E
075400961121     C*  TROVATA SOLO SPUNTA ENTRATA O <= ALL'ALTRA SPUNTA TROVATA, USO
075500041014    3C     *IN17         wheneq    *Off
075600970530     C     ENTDFV        ORGT      0
075700041014     C     ENTDFV        ANDLE     prtDFV
075800961121     C                   MOVEL     ENTDFV        BLTDSE
075900961121     C                   MOVEL     ENTNPS        BLTNPE
076000041014   X3C                   other
076100961121     C*
076200961121     C* AGGIORNO SOLO CON L'ALTRA SPUNTA TROVATA O ENTRATA SE NO LOCALE
076300041014     C                   MOVEL     prtDFV        BLTDSE
076400041014     C                   MOVEL     prtNPS        BLTNPE
076500041014    3C                   ENDsl
076600060512   2aC                   ELSE
076700060512     c* 20 on - non aggiorno la data
076800060512     C                   SETON                                        20
076900060512   2aC                   ENDIF
077000961121     C**
077100961121   X2C                   ELSE
077200041014     c* 20 on - non aggiorno la data
077300930604     C                   SETON                                        20
077400961121    2C                   ENDIF
077500990409     C* DCD
077600990409    2C     FLG           IFEQ      '1'
077700990409     C     WDCD          ANDEQ     'S'
077800990409     C*
077900990409     C* SE NON TROVATA NESSUNA SPUNTA, NEMMENO ENTRATA PULISCO LE DATE
078000041014     C     danDFV        ifeq      0
078100990409     C                   MOVEL     *ZEROS        DCDDFV
078200990409     C                   MOVEL     *ZEROS        DCDFGS
078300990409     C                   MOVEL     *ZEROS        DCDNPS
078400990409     C                   CLEAR                   DCDFAR
078500990409   X3C                   ELSE
078600041014     C* Imposto la spunta con data piuù altra se>=alla data presente
078700041014     C     danDFV        ifge      dcddfv
078800041014     C                   MOVEL     danDFV        DCDDFV
078900041014     C                   MOVEL     danNPS        DCDNPS
079000041014     C                   MOVEL     danFGS        DCDFGS
079100041014     c                   else
079200041014     C                   MOVEL     *ZEROS        DCDDFV
079300041014     C                   MOVEL     *ZEROS        DCDFGS
079400041014     C                   MOVEL     *ZEROS        DCDNPS
079500041014     C                   CLEAR                   DCDFAR
079600041014   X4C                   endif
079700041014   X4C                   endif
079800101202     c*
079900990409     C                   UPDATE    FNDCD000
080000990409    2C                   ENDIF
080100921020     C*
080200970530     C                   UPDATE    FNBLT000
080300921019     C*
080400970530    2C     FLG           IFEQ      '1'
080500930604     C     *IN20         ANDEQ     '0'
080600930604     C*
080700031114     C     KBLT          CHAIN     FNBLP01L                           3339
080800921020     C   39              GOTO      ENDUPD
080900921019     C*
081000930604     C* CONTROLLO DETTAGLIO PER IMPOSTARE 1° DATA ENTRATA
081100001206     C  N20
081200001206     CANN33              EXSR      CTRBLT
081300921230     C*
081400941206     C  N33              UPDATE    FNBLP000
081500031203    2C                   END
081600031203   1aC                   END
081700921019     C*
081800970530    1C                   END
081900041216     c                   unlock    fnblt27l
082000031114     C**
082100031114     C* A G G I O R N O   B O L L E   A R R I V I
082200921013     C**
082300031114    1C     wagarr        IFEQ      'S'
082400941206     C                   SETOFF                                           20
082500941230     C     KBOL2         CHAIN     FNART27L                           3239
082600990409     C                   CLEAR                   WDCD
082700990409     C*
082800031114    2C     *IN39         IFEQ      *OFF
082900041014     c
083000060512     c* Solo se non ho già aggiornato da partenza
083100041015    3c                   if        wsploc=*blanks
083200990409     C     KBOL2         CHAIN     FNDCD02L                           3839
083300060710   3ac                   if        not *in38 and not *in39
083400060512     c                   movel     dcdnps        knps
083500060512     c                   exsr      TIPPIS
083600101202     C*
083700060512    4C     DCDNPS        IFEQ      NPS
083800990409     C     DCDDFV        ANDEQ     WVVDFV
083900060512     C     spurps        oreq      'M'
084000060512     C     §7jrps        andeq     spurps
084100990409     C                   MOVEL     'S'           WDCD              1
084200990409     C                   ELSE
084300990409     C                   UNLOCK    FNDCD02L
084400041015    4C                   ENDIF
084500060512   3aC                   ENDIF
084600041015    3C                   ENDIF
084700031114    2C                   ENDIF
084800920424     C*
084900921013     C   39              GOTO      ENDUPD
085000031203     c
085100041014     c* Solo se esiste la bolla
085200031203   1ac                   if        not *in32
085300990409     C* AGGIORNO FNDCD
085400990409    2C     FLG           IFEQ      '1'
085500990409     C     WDCD          ANDEQ     'S'
085600990409     C*
085700041014    4C                   IF        dandfv=0
085800990409     C                   CLEAR                   DCDFGS
085900990409     C                   CLEAR                   DCDNPS
086000990409     C                   CLEAR                   DCDDFV
086100990409     C                   CLEAR                   DCDFAR
086200990409   X4C                   ELSE
086300041014     C* ESISTE UN'ALTRA SPUNTA PER IL COLLO con data >= di quella annullata
086400041015    5c                   if        dandfv>=dcddfv
086500041014     C                   MOVEL     danDFV        DCDDFV
086600041014     C                   MOVEL     danNPS        DCDNPS
086700041015   x5C                   MOVEL     danFGS        DCDFGS
086800041014     c                   else
086900041014     C                   CLEAR                   DCDFGS
087000041014     C                   CLEAR                   DCDNPS
087100041014     C                   CLEAR                   DCDDFV
087200041014     C                   CLEAR                   DCDFAR
087300041015    5C                   ENDIF
087400041015    4C                   ENDIF
087500101202     c*
087600990409     C                   UPDATE    FNDCD000
087700990409    2C                   ENDIF
087800990409     C*
087900961129    2C     FLG           IFEQ      '1'
088000060512     c                   movel     artnap        knps
088100060512     c                   exsr      TIPPIS
088200101202     c*
088300060512   2aC     ARTNAP        ifeq      NPS
088400970530     C     ARTDAM        ANDEQ     WVVDFV
088500060512     c* Oppure se sono entrambe manuali
088600060512     c     spurps        oreq      'M'
088700060512     c     §7jrps        andeq     spurps
088800101202     c*
088900970604     C* MEMORIZZO LA DATA DI ARRIVO PRIMA DI MODIFICARLA
089000970604     C                   MOVE      ARTDAM        SAVDAM
089100970530     C**
089200960613     C* NON ESISTONO ALTRE SPUNTE PER IL COLLO
089300041014    3C     *IN16         IFEQ      *OFF
089400961129    4C     ARTDCM        IFGT      *ZEROS
089500960613     C                   MOVE      ARTDCM        ARTDAM
089600960613     C                   Z-ADD     85            ARTNAP
089700960613     C                   ELSE
089800960613     C                   Z-ADD     0             ARTDAM
089900960613     C                   Z-ADD     0             ARTNAP
090000050509     c                   clear                   artfl2
090100060512     c                   clear                   artcan
090200060512     C                   SETOff                                       06
090300961129    4C                   END
090400970530   X3C                   ELSE
090500960613     C* ESISTE UN'ALTRA SPUNTA PER IL COLLO
090600041014     C                   MOVEL     arrDFV        ARTDAM
090700041014     C                   MOVEL     arrNPS        ARTNAP
090800050509     C                   MOVEL     arrFL2        ARTFL2
090900970530    3C                   END
091000970530     C* SCRIVO LA STATISTICA PER RIELABORARE LA NUOVA DATA CHE HO
091100970604     C*  SCRITTO E LA DATA CHE C'ERA PRIMA
091200970604     C     ARTDAM        IFNE      SAVDAM
091300070424     c* Memorizzo che devo riaggiornare data arrivo ultimo collo
091400070424     c                   eval      AggioDTI='S'
091500970604     C* VECCHIA DATA
091600970604     C                   CLEAR                   SA2TLA
091700970604     C                   MOVEL     'A'           SA2TRC
091800970604     C                   Z-ADD     SAVDAM        SA2DTS
091900970604     C                   MOVE      *ZEROS        SA2LNA
092000970604     C                   CALL      'FNLSA2R3'
092100970604     C                   PARM                    SA2TLA            1
092200970604     C                   PARM                    SA2TRC            1
092300970604     C                   PARM                    SA2DTS            8 0
092400970604     C                   PARM                    SA2LNA            3 0
092500970604     C* NUOVA DATA
092600970604     C     ARTDAM        IFGT      *ZEROS
092700970604     C                   Z-ADD     ARTDAM        SA2DTS
092800970604     C                   CALL      'FNLSA2R3'
092900970604     C                   PARM                    SA2TLA            1
093000970604     C                   PARM                    SA2TRC            1
093100970604     C                   PARM                    SA2DTS            8 0
093200970604     C                   PARM                    SA2LNA            3 0
093300970604     C                   END
093400970604     C                   END
093500970625     C**
093600970625     C* LA SPUNTA ANNULLATA NON E' QUELLA CHE HA AGGIORNATO IL DETTAGL
093700060512   2aC                   ELSE
093800930604     C                   SETON                                        20
093900060512   2aC                   ENDIF
094000060512     C**
094100060512     C* LA SPUNTA ANNULLATA NON E' QUELLA CHE HA AGGIORNATO IL DETTAGL
094200060512   X2C                   ELSE
094300060512     C                   SETON                                        20
094400060512    2C                   ENDIF
094500970625     C*
094600961129    2C     SAVCAN        IFEQ      ' '
094700921020     C     ARTCAN        ANDEQ     CAN
094800921020     C                   MOVEL     ' '           ARTCAN
094900921019     C                   ELSE
095000921019     C                   SETON                                        06
095100961129    2C                   END
095200950118     C*
095300960417     C*  MEMORIZZAZIONI PER ANOMALIE
095400961129    2C     FLG           IFEQ      '1'
095500960416     C                   MOVE      ARTAAS        WAAS
095600960416     C                   MOVE      ARTLNP        WLNP
095700960416     C                   MOVE      ARTNSP        WNSP
095800960417     C                   MOVE      ARTDAM        WDAM
095900070424     C                   MOVE      ARTdet        WDET
096000070424     C                   MOVE      ARTFLP        WFLP
096100960417     C                   MOVE      ARTDCM        WDCM
096200961129    2C                   END
096300921020     C*
096400961129     C                   MOVEL     ARTDTR        WDTR
096500941206     C                   UPDATE    FNART000
096600921019     C*
096700911016     C* AGGANCIO BOLLA
096800930604     C*
096900941206     C     KARB          CHAIN     FNARB01L                           3339
097000920424     C* 39 ON - RECORD ALLOCATO
097100970625    2C     *IN39         IFEQ      *OFF
097200030115      * Mi salvo arbdam
097300030115     c                   Eval      Sav_ArbDam = ArbDam
097400970625     C**
097500921019     C* CONTROLLO TUTTO IL DETTAGLIO
097600970625    3C     *IN20         IFEQ      *OFF
097700970625     C     *IN06         OREQ      *OFF
097800970625     C*
097900921019     C  N33              EXSR      CTRART
098000970625     C* 06 OFF - NESSUN CODICE ANOMALIA: ELIMINO ANCHE IN TESTATA
098100970625     C  N06              MOVEL     ' '           ARBFAN
098200970625     C* FLAG ABILITAZIONE MERCE:
098300970625    4C  N20FLG           IFEQ      '1'
098400970625     C     ARBAMA        ANDNE     3
098500101202     c*
098600060512     c                   select
098700970625     C* ABILITATO IN BASE ALLA SPUNTA ARRIVO
098800060512    5C     NOAMA         wheneq    ' '
098900970625     C                   MOVEL     2             ARBAMA
099000970625     C                   MOVEL     1             ARBAMP
099100970625     C* ABILITATO IN BASE ALLA SPUNTA PARTENZA(ABBINATO)
099200060512    5C     NOAMP         wheneq    ' '
099300970625     C                   MOVEL     1             ARBAMA
099400970625     C                   MOVEL     1             ARBAMP
099500060512    5C     NOAMP         wheneq    '0'
099600060512     C                   MOVEL     0             ARBAMA
099700060512     C                   MOVEL     1             ARBAMP
099800060512   X5C                   other
099900970625     C* NON ABILITATO
100000970625     C                   MOVEL     0             ARBAMA
100100970625     C                   MOVEL     0             ARBAMP
100200060512    5C                   ENDsl
100300970625    4C                   END
100400970625    3C                   ENDIF
100500961120     C*
100600961120     C* MEMORIZZO LA DATA BORDERO' CHE MI SERVIRA' PER BOLLA LOCALE
100700961120     C*  SE DEVO CREARE L'ANOMALIA 5
100800961120     C                   Z-ADD     ARBDBR        WDBR
100900961120     C                   MOVEL     ARBFBC        WFBC
101000970127     C* TESTO SE SPEDIZIONE GIACENTE PER ANOMALIA 15
101100970625    3C     WFBC          IFEQ      'G'
101200050401     C     KGCA          CHAIN     Tigcp21L                           38
101300970721     C* AI FINI DELL'IMA DEVO TESTARE LA DATA DISPOSIZONI MITTENTE
101400970721    5C  N38GCPFAS        IFLT      35
101500050401    5C     GCPFAS        oreq      36
101600970721     C                   MOVEL     'S'           WGIAC             1
101700970625    3C                   ENDIF
101800970721    3C                   ENDIF
101900070424     c*
102000070424     c                   if        Aggiodti='S'
102100070424     c                   clear                   arbdti
102200070424     c                   clear                   arbhti
102300070424     C     ARBFBS        IFEQ      ' '
102400070424     C                   MOVEL     'S'           ARBFBS
102500070424     C                   ELSE
102600070424     C     ARBFBS        IFEQ      'V'
102700070424     C     ARBFBS        oreq      'P'
102800070424     C                   MOVEL     'E'           ARBFBS
102900070424     C                   ENDIF
103000070424     C                   ENDIF
103100101202     c*
103200070424     C                   ENDIF
103300921019     C*
103400941206     C  N33              UPDATE    FNARB000
103500101202      *
103600030320      * Se è un tipo bolla da trasmettere in sede
103700030320     c                   Clear                   ds3a
103800030320     c                   Eval      Cod = '3A'
103900030320     c                   Clear                   Key
104000030320     c                   Movel     ArbCbo        Key
104100030320     c     kTab          Chain     Tabel00f
104200030320     c                   If        %Found(Tabel00f)
104300030320     c                   Movel     TblUni        ds3a
104400030320     c                   EndIf
104500030320If  2c                   If        §3aBsd = *Blanks
104600030115      * Scrivo/Aggiorno Fidta se data variata
104700030115     c                   If        Not *In33 and Sav_ArbDam <> ArbDam
104800030115     c                   Eval      wFidta = '0'
104900030115     c                   Eval      SavAas = ArbAas
105000030115     c                   Eval      SavLnp1 = ArbLnp
105100030115     c                   Eval      SavNrs = ArbNrs
105200030115     c                   Eval      SavNsp = ArbNsp
105300030115     c                   Eval      SavPoc = ArbLna
105400030115     c                   Eval      SavTrd = 'DAM'
105500030115     c     kFidta        Setll     Fidta01l
105600030115     c                   Do        *Hival
105700030115     c     kFidta        Reade     Fidta01l
105800030115      * Fine file
105900030115     c                   If        %Eof(Fidta01l)
106000030115     c                   Leave
106100030115     c                   EndIf
106200030115      * Già trasmesso
106300030115     c                   If        DtaFtr <> *Blanks
106400030115     c                   Iter
106500030115     c                   EndIf
106600030115      * Aggiorno
106700030115     c                   Eval      DtaDta = ArbDam
106800030115     c                   Update    Fidta000
106900030115     c                   Eval      wFidta = '1'
107000030115     c                   EndDo
107100030115      * Scrivo
107200030115     c                   If        wFidta = '0'
107300030115     c                   Clear                   Fidta000
107400030115     c                   Eval      DtaAas = ArbAas
107500030115     c                   Eval      DtaLnp = ArbLnp
107600030115     c                   Eval      DtaNrs = ArbNrs
107700030115     c                   Eval      DtaNsp = ArbNsp
107800030115     c                   Eval      DtaTrd = SavTrd
107900030115     c                   Eval      DtaPoc = SavPoc
108000030115     c                   Eval      DtaDta = ArbDam
108100030115     c                   Write     Fidta000
108200030115     c                   EndIf
108300030320    2c                   EndIf
108400030115     c                   EndIf
108500101202      *
108600970625   X2C                   ELSE
108700960614     C* BOLLA ALLOCATA:
108800960614     C* SE NON E' L'ULTIMO TENTATIVO VADO A FINE ROUTINE
108900970625    3C     B             IFLT      3
109000960614     C                   GOTO      ENDUPD
109100970625    3C                   END
109200960614     C* SE E' L'ULTIMO TENTATIVO SCRIVO FNAGB
109300960614     C                   MOVEL     'A'           AGBTBO
109400960614     C                   MOVEL     'DT'          AGBAGB
109500960614     C                   Z-ADD     ARTAAS        AGBAAS
109600960614     C                   Z-ADD     ARTLNP        AGBLNP
109700960614     C                   Z-ADD     ARTNRS        AGBNRS
109800960614     C                   Z-ADD     ARTNSP        AGBNSP
109900960614     C                   MOVEL     'S'           AGBFBS
110000960614     C                   CLEAR                   AGBFVC
110100041014     C                   CLEAR                   AGBFpC
110200960614     C     KAGB          SETLL     FNAGB01L                               31
110300960614     C  N31              WRITE     FNAGB000
110400970625    2C                   END
110500031203   1aC                   END
110600921013     C*
110700031114    1C                   END
110800041216     c                   unlock    fnart27l
110900041216     c                   unlock    fnarb01l
111000031114     C**
111100031114     C** T R A N S I T I  /  D I S G U I D I   I N   A R E A   E  N O N
111200031114     C**
111300031114    1C     WESBTT        IFEQ      'S'
111400911112     C*
111500031114     C     KBAR37        CHAIN     FNBTT37L                           3739
111600920424     C* 39 ON  - RECORD ALLOCATO
111700920424     C   39              GOTO      ENDUPD
111800911016     C*
111900921019     C* 37 OFF - TROVATA BOLLA
112000941230    2C  N37              DO
112100941230     C*
112200941230    3C     SAVCAN        IFEQ      ' '
112300941206     C     BTTCAN        ANDEQ     CAN
112400941206     C                   MOVEL     ' '           BTTCAN
112500941230    3C                   END
112600921230     C*
112700941230    3C     FLG           IFEQ      '1'
112800060512     c                   movel     bttpet        knps
112900060512     c                   exsr      TIPPIS
113000101202     c*
113100060512   3aC     BTTPET        ifeq      NPS
113200970530     C     BTTDET        ANDEQ     WVVDFV
113300060512     c* Oppure se sono entrambe manuali
113400060512     c     spurps        oreq      'M'
113500060512     c     §7jrps        andeq     spurps
113600101202     c*
113700970604     C                   MOVE      BTTDET        SAVDET
113800041014     C  N18              MOVEL     *ZEROS        BTTDET
113900041014     C  N18              MOVEL     *ZEROS        BTTPET
114000060512     C  N18              clear                   BTTcan
114100041014     C   18              MOVEL     traDFV        BTTDET
114200041014     C   18              MOVEL     traNPS        BTTPET
114300970625     C*
114400970604     C     BTTDET        IFNE      SAVDET
114500970604     C                   CLEAR                   SA2TLA
114600970604     C                   MOVEL     'A'           SA2TRC
114700970604     C                   Z-ADD     SAVDET        SA2DTS
114800970604     C                   MOVE      *ZEROS        SA2LNA
114900970604     C                   CALL      'FNLSA2R3'
115000970604     C                   PARM                    SA2TLA            1
115100970604     C                   PARM                    SA2TRC            1
115200970604     C                   PARM                    SA2DTS            8 0
115300970604     C                   PARM                    SA2LNA            3 0
115400970604     C* NUOVA DATA
115500970604     C     BTTDET        IFGT      *ZEROS
115600970604     C                   Z-ADD     BTTDET        SA2DTS
115700970604     C                   CALL      'FNLSA2R3'
115800970604     C                   PARM                    SA2TLA            1
115900970604     C                   PARM                    SA2TRC            1
116000970604     C                   PARM                    SA2DTS            8 0
116100970604     C                   PARM                    SA2LNA            3 0
116200970604     C                   END
116300970604     C                   END
116400970625     C*
116500970625     C* NON SONO STATE MODIFICATE LE DATE SUL DETTAGLIO COLLI
116600060512   3aC                   ELSE
116700930604     C                   SETON                                        20
116800060512   3AC                   END
116900060512     C*
117000060512     C* NON SONO STATE MODIFICATE LE DATE SUL DETTAGLIO COLLI
117100060512   X3C                   ELSE
117200060512     C                   SETON                                        20
117300060512    3C                   END
117400960416     C*  MEMORIZZO KEY SPEDIZIONE PER ANOMALIE
117500960416     C     FLG           IFEQ      '1'
117600960416     C                   MOVE      BTTAAS        WAAS
117700960416     C                   MOVE      BTTLNP        WLNP
117800960416     C                   MOVE      BTTNSP        WNSP
117900960416     C                   END
118000921020     C*
118100941206     C                   UPDATE    FNBTT000
118200921020     C*
118300911016     C* AGGANCIO BOLLA
118400941230    3C     FLG           IFEQ      '1'
118500930604     C     *IN20         ANDEQ     '0'
118600031114     C     KBTT          CHAIN     FNBTP11L                           3339
118700920424     C* 39 ON  - RECORD ALLOCATO
118800920424     C   39              GOTO      ENDUPD
118900030115      * Mi salvo btpdet (btpdut non viene modificato)
119000030115     c                   Eval      Sav_BtpDet = BtpDet
119100930604     C*
119200921020     C  N33              EXSR      CTRBTT
119300921019     C*
119400941206     C  N33              UPDATE    FNBTP000
119500030115      * Scrivo/Aggiorno Fidta se la data è variata
119600030115     c                   If        Not *In33 and Sav_BtpDet <> BtpDet
119700030115     c                   Eval      wFidta = '0'
119800030115     c                   Eval      SavAas = BtpAas
119900030115     c                   Eval      SavLnp1 = BtpLnp
120000030115     c                   Eval      SavNrs = BtpNrs
120100030115     c                   Eval      SavNsp = BtpNsp
120200030115     c                   Eval      SavPoc = BtpFlp
120300030115     c                   Eval      SavTrd = 'DET'
120400030115     c     kFidta        Setll     Fidta01l
120500030115     c                   Do        *Hival
120600030115     c     kFidta        Reade     Fidta01l
120700030115      * Fine file
120800030115     c                   If        %Eof(Fidta01l)
120900030115     c                   Leave
121000030115     c                   EndIf
121100030115      * Già trasmesso
121200030115     c                   If        DtaFtr <> *Blanks
121300030115     c                   Iter
121400030115     c                   EndIf
121500030115      * Aggiorno
121600030115     c                   Eval      DtaDta = BtpDet
121700030115     c                   Update    Fidta000
121800030115     c                   Eval      wFidta = '1'
121900030115     c                   EndDo
122000030115      * Scrivo
122100030115     c                   If        wFidta = '0'
122200030115     c                   Clear                   Fidta000
122300030115     c                   Eval      DtaAas = BtpAas
122400030115     c                   Eval      DtaLnp = BtpLnp
122500030115     c                   Eval      DtaNrs = BtpNrs
122600030115     c                   Eval      DtaNsp = BtpNsp
122700030115     c                   Eval      DtaTrd = SavTrd
122800030115     c                   Eval      DtaPoc = SavPoc
122900030115     c                   Eval      DtaDta = BtpDet
123000030115     c                   Write     Fidta000
123100030115     c                   EndIf
123200030115     c                   EndIf
123300101202      *
123400941230    3C                   END
123500921014     C*
123600941230    2C                   END
123700941230    1C                   END
123800041216     c                   unlock    fnbtt37l
123900041216     c                   unlock    fnbtp11l
124000101202     c*
124100041013     c* Aggiornamento anche di FNART per spunta di transito/disguido
124200041013     c*  cancellata
124300041013     c                   if        (lnatfa<>bpes and lnatfa<>pestfa  and
124400041013     c                             wagpar=' ') or wesbtt='S'
124500041013     C     KBOL2         CHAIN     FNART27L
124600041013     c                   if        %found(fnart27l)
124700101202     c*
124800041013    3C     SAVCAN        IFEQ      ' '
124900041013     C     arTCAN        ANDEQ     CAN
125000041013     C                   MOVEL     ' '           arTCAN
125100041013    3C                   END
125200041013     C*
125300041013    3C     FLG           IFEQ      '1'
125400060512     c                   movel     artpet        knps
125500060512     c                   exsr      TIPPIS
125600101202     c*
125700060512   3aC     artPET        ifeq      NPS
125800041013     C     artDET        ANDEQ     WVVDFV
125900060512     c* Oppure se sono entrambe manuali
126000060512     c     spurps        oreq      'M'
126100060512     c     §7jrps        andeq     spurps
126200101202     c*
126300041015     C   18              MOVEL     traDFV        artDET
126400041015     C   18              MOVEL     traNPS        artPET
126500041014     C  N18              MOVEL     *ZEROS        artDET
126600041014     C  N18              MOVEL     *ZEROS        artPET
126700060512     C  N18              CLEAR                   artCAN
126800041014    4c  n18              if        wesbtt<>'S'
126900041014     C                   MOVEL     *ZEROS        artflp
127000041014     c* Verifico le spunte partenza per impostare eventuale altro p.o. di
127100041014     c*  transito/disguido
127200041014     c* Se p.o. ultima spunta partenza= p.o. ultima spunta no part.
127300041014     c* imposto questa
127400041014    5c                   select
127500041014     c                   when      dis1fgs=dis2fgs
127600041014     c                   z-add     dis2dfv       artdet
127700041014     c                   z-add     dis2nps       artpet
127800041014     c                   z-add     dis2fgs       artflp
127900041014     c                   z-add     dis1dfv       artdut
128000041014     c                   z-add     dis1nps       artput
128100101202     c*
128200041014     c* Elaboro spunta partenza se con data > o data/ora caricamento>
128300041014     c                   when      dis1dfv=dis2dfv and dis1scar>dis2scar or
128400041014     c                             dis1dfv>dis2dfv
128500041014     c* Se si tratta di uscita transito aggiorno subito altrimenti leggo
128600041014     c* le linee per vedere se c'e' transito o meno
128700041014    6c                   if        dis1atr='T' or dis1atr='D'
128800041014     c                   movel     dis1fgs       artflp
128900041014     c                   movel     dis1dfv       artdet
129000041014     c                   movel     dis1dfv       artdut
129100041014     c                   movel     dis1nps       artpet
129200041014     c                   movel     dis1nps       artput
129300041014   x6c                   else
129400041014     c* E si tratta della partenza o si tratta di un disguido che
129500041014     c* transita di nuovo per cui aggiorno solo flp
129600041014    7c                   if        dis1fgs<>lnptfp
129700041014     c                   movel     dis1fgs       artflp
129800041014   x7c                   else
129900041014     c* chain fnfgv
130000041014     c     kfgv          chain     fnfgv01l
130100041014    8c                   if        %found(fnfgv01l) and fgvlna<>lnatfa
130200041015     c     kfgv          chain     fnfgw01l
130300041015     c                   if        not %found(fnfgw01l)
130400041015     c                   clear                   fgwff3
130500041015     c                   clear                   fgwff4
130600041015     c                   clear                   fgwfl3
130700041015     c                   clear                   fgwfl4
130800041015     c                   endif
130900101202     c*
131000041015     c                   z-add     1             xx                3 0
131100041015     c                   movel     lna           wlna              3
131200041015     c                   movel     lnatfa        wlnatfa           3
131300041015     c     wlna          lookup    ffv(xx)                                31
131400041015    9c                   if        *in31 and flp(xx)<>wlnatfa
131500041014     c                   movel     flp(xx)       artflp
131600041014    9c                   endif
131700041014    8c                   endif
131800041014    7c                   endif
131900041014    6c                   endif
132000041014   x5c                   other
132100041014     c* Altrimenti elaboro spunta arrivi
132200041014     c                   movel     dis2fgs       artflp
132300041014     c                   movel     dis2dfv       artdet
132400041014     c                   movel     dis2nps       artpet
132500041014     c                   clear                   artdut
132600041014     c                   clear                   artput
132700041014    5c                   endsl
132800101202     c*
132900041014    4c                   endif
133000060512   3ac                   endif
133100060512    3c                   endif
133200101202     c*
133300041014     c                   update    fnart000
133400041014     c                   clear                   fnagb000
133500041014     C                   MOVEL     'A'           AGBTBO
133600041014     C                   MOVEL     'DT'          AGBAGB
133700041014     C                   Z-ADD     ARTAAS        AGBAAS
133800041014     C                   Z-ADD     ARTLNP        AGBLNP
133900041014     C                   Z-ADD     ARTNRS        AGBNRS
134000041014     C                   Z-ADD     ARTNSP        AGBNSP
134100041014     C     KAGB          SETLL     FNAGB01L                               31
134200041014     C  N31              WRITE     FNAGB000
134300041013     c                   endif
134400041014     c                   endif
134500921019     C**
134600921019     C** CHIUDO ANOMALIE
134700921019     C**
134800960416    1C     FLG           IFEQ      '1'
134900960416     C*
135000960416     C                   CLEAR                   W005
135100960416     C                   CLEAR                   W015
135200070424     c                   clear                   wcrea200          1
135300921021     C*
135400990518     C     KBOL2         SETLL     FNANM000
135500990518     C     KBOL2         READE     FNANM000                               32
135600921019     C*
135700961129    2C     *IN32         DOWEQ     *OFF
135800060512     c* Se anomalia 15 e non esiste più la data di arrivo del collo
135900060512     c*  devo cancellare l'anomalia
136000060512    3c                   if        anmcaa=015 and wdam=0  and wagarr='S'
136100060512    4c                   if        anmdch>0
136200060828     c                   unlock    fnanm02l
136300060512     c* Se già in idd3 la chiudo come annullata
136400060512     C                   MOVEL     FASE          D33FSC
136500060512     C                   MOVEL     'AN'          D33CCH
136600060828     C                   MOVEL     dateu         D33dCH
136700060512     C                   MOVE      ANMNR2        D33NRR
136800060512     C                   MOVEL     'S'           D33MAC
136900060512     C                   CALL      'FNLR33R'
137000060512     C                   PARM                    DSLR33
137100101202     c*
137200060512     C     KBOL2         READE     FNANM000                               32
137300060512     c                   iter
137400060512     c                   else
137500060512     c* la deleto
137600060512     c                   delete    fnanm000
137700101202     c*
137800060512     C     KBOL2         READE     FNANM000                               32
137900060512     c                   iter
138000060512    4c                   endif
138100060512    3c                   endif
138200101202     c*
138300070424     c* Se esiste anomalia 55 e artdam=0, vedo se ricreare la 200
138400130301    3c                   if        (anmcaa=57 or
138500130301    3c                              anmcaa=55 or anmcaa=56) and wdam=0
138600070424     c                             and anmdao=wdet and anmfle=wflp
138700070424     c                   eval      wcrea200='S'
138800070424     c                   eval      savrecanm=dsrecanm
138900070424    3c                   endif
139000101202     c*
139100070424     c* Se trovo già la 200 chiusa, la devo solo riaprire
139200070424     c                   if        wcrea200='S' and anmcaa=200 and
139300070424     c                             anmfle=wflp
139400070424     C                   Z-ADD     0             ANMDCH
139500070424     C                   MOVEL     ' '           ANMFSC
139600070424     C                   MOVEL     '  '          ANMCCH
139700070424     C                   CLEAR                   ANMFT1
139800070424     C                   CLEAR                   ANMFT2
139900070424     C                   UPDATE    FNANM000
140000101202     c*
140100070424     c* N= non creare la 200 perchè già trovata
140200070424     c                   eval      wcrea200='N'
140300110908     C     KBOL2         READE     FNANM000                               32
140400070424     c                   iter
140500070424     c                   endif
140600101202     c*
140700110908     c* Anomlia 115 --> se non esiste nessun'altra spunta sul sistema reale
140800110908     c*                 riapro l'anomalia anche non mia
140900110908    3c                   if        anmcaa=115
141000110908    4c                   if        a_115dfv=0
141100110908     C                   Z-ADD     0             ANMDCH
141200110908     C                   MOVEL     ' '           ANMFSC
141300110908     C                   MOVEL     '  '          ANMCCH
141400110908     C                   CLEAR                   ANMFT1
141500110908     C                   CLEAR                   ANMFT2
141600110908   x4c                   else
141700110908     c* altrimenti se è stata con questa spunta--> la riaggiorno
141800110908    5C     ANMDCH        IFNE      0
141900110908     C     ANMFSC        ANDEQ     FASE
142000110908     C     ANMdch        ANDEQ     wvvdfv
142100110908     C                   CLEAR                   DSFASE
142200110908     C                   MOVE      a_115NPG      D11NPG
142300110908     C                   MOVE      a_115SPG      D11SPG
142400110908     C                   CALL      'TRUL11R'
142500110908     C                   PARM                    DSFASE
142600110908     C                   MOVEL     D11FAS        ANMFSC
142700110908     C                   MOVEL     a_115dfv      ANMdch
142800110908    5c                   endif
142900110908    4c                   endif
143000110908     C                   UPDATE    FNANM000
143100110908     c
143200110908     C     KBOL2         READE     FNANM000                               32
143300110908     c                   iter
143400110908    3c                   endif
143500060512     C*
143600041018     c* Leggo solo le anomalie create da me
143700120109    3c*****              if        anmfle=bpes or anmfle=parfgs
143800921019     C*
143900921019     C* RIAPRO SE CHIUSE E NESSUN'ALTRA SPUNTA
144000980205     C* E SE LA SPEDIZIONE NON E' CONSEGNATA
144100120109    3C                   if        anmcaa=005
144200120109    4c                   if        anmfle=bpes or anmfle=parfgs
144300120109     c                             or  anmfle=lnatfa
144400041018    5c     wagarr        ifeq      'S'
144500961129     C                   MOVE      '1'           W005              1
144600921019     C*
144700041018    6C  N19WDCM          IFEQ      0
144800980205     C     WDAM          ANDEQ     0
144900980205     C**
145000041018    7C     ANMDCH        IFNE      0
145100041018    8C     ANMLID        IFGE      §7ALCI
145200960510     C                   MOVE      'PR'          ANMCCH
145300960510     C                   MOVE      'D'           ANMFSC
145400970410     C                   CLEAR                   ANMFT1
145500970410     C                   CLEAR                   ANMFT2
145600960510     C                   ELSE
145700921019     C                   Z-ADD     0             ANMDCH
145800921019     C                   MOVEL     ' '           ANMFSC
145900921019     C                   MOVEL     '  '          ANMCCH
146000970410     C                   CLEAR                   ANMFT1
146100970410     C                   CLEAR                   ANMFT2
146200041018    8C                   END
146300990518     C                   UPDATE    FNANM000
146400041018    7C                   END
146500041018    6C                   END
146600041018    5C                   END
146700120109    4C                   END
146800921021     C*
146900120109   x3c                   else
147000120109     c
147100120109    4c                   if        anmfle=bpes or anmfle=parfgs
147200101202     c*
147300921019     C* PER TUTTE LE ALTRE ANOMALIE CONTROLLO FASE APERTURA E FOGLIO
147400041018    5C     ANMNFV        IFEQ      PARNFV
147500921021     C     ANMFSA        ANDEQ     FASE
147600960513     C*
147700960417     C     ANMCAA        OREQ      050
147800960417     C     *IN19         ANDEQ     *OFF
147900041018     c     anmft1        andeq     *blanks
148000101202     c*
148100041018     c* chain in DS/C per controllo se autochiusa
148200041018     C     anmcaa        IFNE      SAVCAA
148300041018     C                   MOVEL     '7C'          COD
148400041018     C                   MOVEL(P)  ANMCAA        KEY
148500041018     C     KTAB          CHAIN     TABEL                              31
148600041018     C  N31              MOVEL     TBLUNI        DS7C
148700041018     C   31              MOVEL     *BLANKS       DS7C
148800041018     C                   MOVEL     anmcaa        SAVCAA
148900041018     C                   ENDIF
149000960513     C*
149100041018     C* PER alcune anomalie,
149200041018     C*  SE ESISTE UN'ALTRA SPUNTA PER IL COLLO MODIFICO
149300041018     C*  L'ANOMALIA IN BASE ALLA SPUNTA RIMASTA
149400041018    6c                   if        *in19 and (anmcaa=10 or anmcaa=160
149500041018     c                             or anmcaa=200 or anmcaa=60 or
149600041018     c                             anmcaa=61)
149700960417     C                   CLEAR                   DSFASE
149800960417     C                   MOVE      SPUNPG        D11NPG
149900960417     C                   MOVE      SPUSPG        D11SPG
150000960417     C                   CALL      'TRUL11R'
150100960417     C                   PARM                    DSFASE
150200960417     C                   MOVEL     D11FAS        ANMFSA
150300960513     C                   MOVE      SPUFGS        ANMCDU
150400041018     C                   Z-ADD     SPUNFV        ANMNFV
150500041018     C* NELL'ANOMALIA 10 NON MODIFICO LA DATA APERTURA PER NON FAR DIVE
150600041018     C*  NTARE INCONGRUENTE IL LIVELLO IDD CON LA DATA CREAZIoNE ANOM
150700041018    7c                   if        anmcaa<>10
150800041018     C                   Z-ADD     SPUDFV        ANMDAO
150900041018    7c                   endif
151000041018     C                   MOVE      SPUFGS        ANMFLE
151100041018     C                   MOVE      SPUNPS        ANMNPS
151200041018     c* se anomalia autochiusam, imposto anche i dati di chiusura
151300041018     c                   if        §7ccha='S'
151400041018     C                   MOVE      spudfv        ANMDCH
151500041018     C                   MOVEL     d11fas        ANMFSC
151600041018     c                   endif
151700970603     C*
151800041018    7C     ANMAIE        IFEQ      'E'
151900970603     C                   CLEAR                   ANMFT1
152000970603     C                   CLEAR                   ANMFT2
152100041018    7C                   ENDIF
152200990518     C                   UPDATE    FNANM000
152300041018   X6C                   ELSE
152400990412     C*
152500990412     C* SE SI TRATTA DI ANOMALIA 56 CON ALTRE SPUNTE (19 ON):
152600041018     C*  modifico anche se trasmessa essendo su As unico, non
152700041018     C*  modifico però la data visto che ha aggiornato bltdet
152800041018    7C                   IF        *in19 and (anmcaa=55 or anmcaa=56)
152900101202     C*
153000041018    8C                   if        ANMCAA=56 AND A56DFV>0
153100041018     c                   if        anmft1=' '
153200990412     C                   Z-ADD     A56DFV        ANMDAO
153300990412     C                   Z-ADD     A56DFV        ANMDCH
153400041018     c                   endif
153500990412     C                   CLEAR                   DSFASE
153600990412     C                   MOVE      A56NPG        D11NPG
153700990412     C                   MOVE      A56SPG        D11SPG
153800990412     C                   CALL      'TRUL11R'
153900990412     C                   PARM                    DSFASE
154000990412     C                   MOVEL     D11FAS        ANMFSA
154100990412     C                   MOVEL     D11FAS        ANMFSC
154200990412     C                   MOVE      A56FGS        ANMCDU
154300990412     C                   MOVE      A56FGS        ANMFLE
154400990412     C                   MOVE      A56NPS        ANMNPS
154500990518     C                   Z-ADD     A56NFV        ANMNFV
154600990518     C                   UPDATE    FNANM000
154700041018   X8C                   else
154800990412     C* LA TRASFORMO IN 55
154900990412     C                   Z-ADD     55            ANMCAA
155000041018    9c                   if        anmft1=' '
155100990412     C                   Z-ADD     SPUDFV        ANMDAO
155200990412     C                   Z-ADD     SPUDFV        ANMDCH
155300041018    9c                   endif
155400990412     C                   CLEAR                   DSFASE
155500990412     C                   MOVE      SPUNPG        D11NPG
155600990412     C                   MOVE      SPUSPG        D11SPG
155700990412     C                   CALL      'TRUL11R'
155800990412     C                   PARM                    DSFASE
155900990412     C                   MOVEL     D11FAS        ANMFSA
156000990412     C                   MOVEL     D11FAS        ANMFSC
156100990412     C                   MOVE      SPUFGS        ANMCDU
156200990412     C                   MOVE      SPUFGS        ANMFLE
156300990412     C                   MOVE      SPUNPS        ANMNPS
156400990518     C                   Z-ADD     SPUNFV        ANMNFV
156500990518     C                   UPDATE    FNANM000
156600041018    8C                   ENDif
156700990412   X7C                   ELSE
156800960416     C* Richiamo pgm di chiusura anomalia
156900990518     C                   UNLOCK    FNANM02L
157000960416     C                   CLEAR                   DSLR33
157100041018    8C     DATEU         IFGE      WVVDFV
157200970117     C                   MOVEL     DATEU         D33DCH
157300970117     C                   ELSE
157400041018    8C                   MOVEL     WVVDFV        D33DCH
157500970117     C                   ENDIF
157600960416     C                   MOVEL     FASE          D33FSC
157700960416     C                   MOVEL     'AN'          D33CCH
157800960416     C                   MOVE      ANMNR2        D33NRR
157900960510     C                   MOVEL     'S'           D33MAC
158000960416     C                   CALL      'FNLR33R'
158100960416     C                   PARM                    DSLR33
158200990412    7C                   ENDIF
158300041018    6C                   END
158400921021     C*
158500041018   X5C                   ELSE
158600041018    6C     ANMCAA        IFNE      50
158700041018    6C     ANMCAA        andne     56
158800041018    6C     ANMCAA        andne     55
158900130301    6C     ANMCAA        andne     57
159000921021     C* CONTROLLO SE E' STATA CHIUSA IN QUESTA FASE
159100041018    7C     ANMDCH        IFNE      0
159200921021     C     ANMFSC        ANDEQ     FASE
159300041018     C     ANMdch        ANDEQ     wvvdfv
159400041018     c* Per l'anomalia 15 controllo se esiste altra spunta con data >= a
159500041018     c*  quella annullata nel qual caso la chiudo con dati diversi
159600041018     c                   clear                   wokanm15
159700041018    9C                   if        anmcaa=015
159800041018     C                   MOVE      '1'           W015              1
159900041018    0c                   if        dandfv>wvvdfv  or
160000041018     c                             (dannpg<>2 and dandfv>wvvdfv)
160100110908     C                   CLEAR                   DSFASE
160200110908     C                   MOVE      danNPG        D11NPG
160300110908     C                   MOVE      danSPG        D11SPG
160400110908     C                   CALL      'TRUL11R'
160500110908     C                   PARM                    DSFASE
160600110908     C                   MOVEL     D11FAS        ANMFSC
160700110908     C                   MOVEL     dandfv        ANMdch
160800041018     c                   eval      wokanm15='S'
160900041018    9c                   endif
161000041018    8c                   endif
161100041018     c*
161200041018    8c                   if        wokanm15=' '
161300041018    9c     ANMLID        IFGE      §7ALCI
161400960510     C                   MOVE      'PR'          ANMCCH
161500960510     C                   MOVE      'D'           ANMFSC
161600960510     C                   ELSE
161700921021     C                   Z-ADD     0             ANMDCH
161800921021     C                   MOVEL     ' '           ANMFSC
161900921021     C                   MOVEL     '  '          ANMCCH
162000041018    9C                   END
162100961025     C                   MOVE      *BLANKS       ANMFT1
162200990518     C                   UPDATE    FNANM000
162300041018    8C                   END
162400041018    7C                   END
162500041018    6C                   END
162600041018    5C                   END
162700921021     C*
162800120109    4C                   ENDIF
162900120109    3C                   END
163000921019     C*
163100990518     C     KBOL2         READE     FNANM000                               32
163200960416    2C                   ENDDO
163300070424     c*
163400070424     c* Verifico se devo creare  l'anomalia 200
163500070424     c                   if        wcrea200='S'
163600070424     c                   eval      dsrecanm=savrecanm
163700070424     C                   Z-ADD     200           COMCAA
163800070424     c                   exsr      CRTANM
163900070424     c                   eval      wcrea200='N'
164000070424     c                   endif
164100960416     C*
164200960416     C* CREO ANOMALIA 005 A FRONTE DI CANCELLAZIONE SPUNTA
164300960416     C     W005          IFEQ      *BLANKS
164400031114     c     wagarr        andeq     'S'
164500960416     C     *IN19         ANDEQ     *OFF
164600960416     C     WCHIUS        ANDEQ     'S'
164700960530     C     WDAM          ANDEQ     *ZEROS
164800960530     C     WDCM          ANDEQ     *ZEROS
164900000904     C*
165000031114     c* creo sempre anche per 2 liv in arrivo su altro AS in quanto
165100031114     c*  se poi dal terminal riceco la spunta l'anomalia viene cancell
165200031114     c*  se riricevo l'anomalia, dovrebbe andare in update
165300031114     c* (prioma invece creava soltanto se terarr di lna in £1)
165400000904     C                   EXSR      CRTA05
165500000904     C                   ENDIF
165600960530     C**
165700960416     C* CREO ANOMALIA 015 A FRONTE DI CANCELLAZIONE SPUNTA
165800960416     C     W015          IFEQ      *BLANKS
165900031114     C     wagarr        andeq     'S'
166000960416     C* Collo non consegnato
166100960416     C     WDCM          ANDEQ     *ZEROS
166200960417     C* Collo arrivato
166300960417     C     WDAM          ANDGT     *ZEROS
166400960416     C* Data arrivo <= data spunta cancellata
166500960416     C     WDAM          ANDLE     WVVDFV
166600960416     C* Non esistono altre spunte inventario o consegna in data >=
166700960416     C* alla data di cancellazione della spunta
166800960416     C     WSPUN         ANDEQ     *BLANKS
166900970127     C* La bolla non deve essere bloccata
167000970127     C     WFBC          ANDNE     'B'
167100970127     C* La bolla non è in giacenza
167200970127     C     WGIAC         ANDNE     'S'
167300000904     C                   EXSR      CRTA15
167400000904     C                   END
167500960416     C*
167600960416    1C                   END
167700050408     c* se la pistola della spunta annullata non è automatica devo decrement
167800050408     c* are sgnst2 su fisgn
167900060512     c                   movel     nps           knps
168000050408     c                   exsr      tippis
168100050408     c     §7jrps        ifne      'A'
168200050408     C* AGGIORNO FILE  FISGN SE C'E'
168300050408     C     KBOL2         CHAIN     FISGN03L                           33
168400050408     C     *IN33         DOWEQ     *OFF
168500050408     c* testo > 0 per sicurezza ma in teoria non DEVE succedere che sia = 0
168600050408     C     SGNST2        IFgt      0
168700050408     C                   sub       1             SGNST2
168800050408     C                   CLEAR                   SGNT6A
168900050408     C                   CLEAR                   SGNT6B
169000050408     C                   CLEAR                   SGNT6C
169100050408     C                   CLEAR                   SGNT6D
169200050408     C                   CLEAR                   SGNT6E
169300050408     C                   CLEAR                   SGNT6F
169400080623     c                   eval      sgndatora = %char(%timestamp:*iso0)
169500050408     C                   UPDATE    FISGN000
169600050408     C                   ENDIF
169700050408     C     KBOL2         READE     FISGN03L                               33
169800050408     C                   ENDDO
169900050408     C                   ENDIF
170000921019     C*
170100920424     C                   Z-ADD     3             B
170200941206     C*
170300970530     C     ENDUPD        ENDSR
170400970530     C**************************************************************************
170500970530     C* CONTROLLO IL TIPO PISTOLA DELLA SPUNTA CHE LEGGO
170600970530     C**************************************************************************
170700970530     C     TIPPIS        BEGSR
170800970530     C                   MOVEL     '7J'          COD
170900060512     C                   MOVEL(P)  knps          KEY
171000970530     C     KTAB          CHAIN     TABEL                              31
171100970530     C  N31              MOVEL     TBLUNI        DS7J
171200970530     C   31              CLEAR                   DS7J
171300970530     C*
171400970530     C                   ENDSR
171500961120     C**************************************************************************
171600970530     C* REPERISCO I DATI DEL FOGLIO DELLA SPUNTA LETTA
171700961120     C**************************************************************************
171800961120     C     CHAFV         BEGSR
171900961120     C                   Z-ADD     DATEU         DATAF
172000961120     C                   MOVEL     ' '           WFVV              1
172100961120     C                   MOVEL     ' '           WFGV              1
172200961120     C                   MOVEL     ' '           WFVA              1
172300970530     C                   CLEAR                   DEFF
172400970530     C                   CLEAR                   SPGF
172500970530     C                   CLEAR                   NFAF
172600970530     C                   CLEAR                   AEDF
172700970530     C                   CLEAR                   NFAFCF
172800970530     C                   CLEAR                   NFASPG
172900970530     C                   CLEAR                   NFAFGS
173000970530     C                   CLEAR                   NFADFV
173100970530     C                   CLEAR                   NFANPG
173200970530     C**
173300970530     C                   CLEAR                   DSLV53
173400040203     c                   if        brvnpg=1 and brvfgs=simfel
173500040203     c                   eval      d53tfo='G'
173600040203     c                   endif
173700040203     c                   if        brvnpg=1 and brvfgs<>simfel
173800040203     c                   eval      d53tfo='A'
173900040203     c                   endif
174000970529     C                   MOVEL     BRVNPG        D53NPG
174100000203     C                   Z-ADD     BRVNFV        D53NFV
174200970529     C                   MOVEL     BRVFGS        D53FGS
174300970529     C                   MOVEL     'A'           D53ABB
174400970529     C                   MOVEL     LNA           D53LNA
174500031114     C                   MOVEL     lnatfa        D53TFA
174600970529     C                   CALL      'FNLV53R'
174700970529     C                   PARM                    DSLV53
174800101202     c*
174900041014     c* Se errore e categoria "1" cerco in fgv, se cercato in fva
175000041014     c                   if        d53err<>*blanks and brvnpg=1 and d53tfo
175100041014     c                             ='A'
175200041014     C                   CLEAR                   DSLV53
175300041014     c                   eval      d53tfo='G'
175400041014     C                   MOVEL     BRVNPG        D53NPG
175500041014     C                   Z-ADD     BRVNFV        D53NFV
175600041014     C                   MOVEL     BRVFGS        D53FGS
175700041014     C                   MOVEL     SIMFEL        D53FEL
175800041014     C                   MOVEL     'A'           D53ABB
175900041014     C                   MOVEL     LNA           D53LNA
176000041014     C                   MOVEL     lnatfa        D53TFA
176100041014     C                   CALL      'FNLV53R'
176200041014     C                   PARM                    DSLV53
176300041014     c                   endif
176400970530     C*
176500970530     C* NESSUN ERRORE --> FOGLIO TROVATO
176600970530    1C     D53ERR        IFEQ      ' '
176700970529     C                   MOVEL     D53DFV        DATAF
176800970530     C                   MOVEL     D53SPG        SPGF
176900970530     C                   MOVEL     D53DEF        DEFF              1
177000970530     C                   MOVEL     D53NFA        NFAF
177100970530     C                   MOVEL     D53AED        AEDF
177200970530     C**
177300970530    2C     D53TFO        IFEQ      'G'
177400970530     C                   MOVEL     'S'           WFGV
177500970529     C                   ELSE
177600970530    3C     D53TFO        IFEQ      'V'
177700970530     C                   MOVEL     'S'           WFVV
177800970530   X3C                   ELSE
177900970530     C*
178000970530     C                   MOVEL     'S'           WFVA
178100970530     C* SE ABBINATO CONTROLLO IL FOGLIO ARRIVI PER VEDERE
178200970530     C*  SE ESISTE CHIUSO
178300970530    4C     D53NFA        IFGT      0
178400970620     C                   MOVEL     D53NFA        SAVNFA
178500970620     C                   MOVEL     D53NPA        SAVNPA
178600020917     c                   Movel     D53Lai        SavLai
178700970530     C                   CLEAR                   DSLV53
178800970620     C                   MOVEL     SAVNFA        D53NFV
178900970620     C                   MOVEL     SAVNPA        D53NPG
179000970530     C                   MOVEL     'V'           D53TFO
179100970530     C                   MOVEL     SIMFEL        D53FEL
179200970530     C                   MOVEL     SIMFEL        D53FLF
179300020917     c                   Movel     SavLai        D53Fgs
179400970530     C                   CALL      'FNLV53R'
179500970530     C                   PARM                    DSLV53
179600970530     C* TROVATO FOGLIO DI ABBINAMENTO --> SE ERRORE COME SE NON CI FOSS
179700970530    5C     D53ERR        IFEQ      ' '
179800970530     C                   MOVEL     D53FCF        NFAFCF
179900970530     C                   MOVEL     D53SPG        NFASPG
180000970530     C                   MOVEL     D53FGS        NFAFGS
180100970530     C                   MOVEL     D53DFV        NFADFV
180200970530     C                   MOVEL     D53NPG        NFANPG
180300970530     C                   ELSE
180400970530     C                   CLEAR                   NFAF
180500970530     C                   CLEAR                   AEDF
180600970530    5C                   ENDIF
180700970530    4C                   ENDIF
180800970530    3C                   ENDIF
180900970530    2C                   ENDIF
181000970530    1C                   ENDIF
181100961120     C*
181200961120     C                   ENDSR
181300960606     C**************************************************************************
181400960606     C* DETERMINO TERMINAL DI ARRIVO DI BRVFGS
181500960606     C**************************************************************************
181600960606     C     TERFGS        BEGSR
181700970530     C                   CLEAR                   DSLV55
181800970530     C                   MOVEL     LNPB          D55LNP
181900970530     C                   MOVEL     BRVFGS        D55LIN
182000970530     C                   MOVEL     DATAF         D55DRF
182100970530     C                   MOVEL     'A'           D55TPT
182200970530     C                   CALL      'FNLV55R'
182300970530     C                   PARM                    DSLV55
182400970530     C     D55ERR        IFEQ      ' '
182500970530     C                   MOVEL     D55TFA        WTFG
182600970530     C                   ELSE
182700970530     C                   MOVEL     BRVFGS        WTFG
182800970530     C                   ENDIF
182900960606     C*
183000960606     C                   ENDSR
183100941206     C**************************************************************************
183200941206     C* CONTROLLO DETTAGLIO FNBLT00F
183300941206     C**************************************************************************
183400921019     C     CTRBLT        BEGSR
183500941206     C*
183600930604     C                   MOVEL     *ALL'9'       BLPDSE
183700941206     C     KBLP2         SETLL     FNBLT01L
183800941206     C     KBLP2         READE     FNBLT01L                               34
183900921019     C*
184000921019     C     *IN34         DOWEQ     '0'
184100921021     C*
184200930604     C     BLTDSE        IFNE      0
184300921021     C     BLTDSE        ANDLT     BLPDSE
184400921021     C                   MOVEL     BLTDSE        BLPDSE
184500921021     C                   END
184600921021     C*
184700941206     C     KBLP2         READE     FNBLT01L                               34
184800921019     C                   END
184900921021     C*
185000930604     C     BLPDSE        IFEQ      *ALL'9'
185100921021     C                   Z-ADD     0             BLPDSE
185200921021     C                   END
185300941206     C*
185400921019     C                   ENDSR
185500941206     C**************************************************************************
185600941206     C* CONTROLLO SE ALTRI COLLI HANNO DATA ARRIVO E/O COD.ANOM
185700941206     C**************************************************************************
185800921019     C     CTRART        BEGSR
185900941206     C*
186000930603     C* ABILITAZIONI
186100930603     C                   MOVEL     ' '           NOAMP             1
186200930603     C                   MOVEL     ' '           NOAMA             1
186300930604     C  N20              MOVEL     *ALL'9'       ARBDAM
186400060512     c                   clear                   abbnsc
186500930604     C*
186600941206     C     KARB2         SETLL     FNART01L
186700941206     C     KARB2         READE     FNART01L                               34
186800921019     C*
186900921019     C* CONTROLLO SE ALTRI COLLI HANNO LA DATA ARRIVO
187000921019     C     *IN34         DOWEQ     '0'
187100930604     C*
187200930604     C     *IN20         IFEQ      '0'
187300930528     C* SE ALMENO UN COLLO HA ARTDAM = 0  DEVO AZZERARE ARBDTI E ARBHTI
187400930603     C* NOAMA = S NON E' ABILITATO IN BASE ALLA SPUNTA ARRIVI
187500930528     C     ARTDAM        IFEQ      0
187600930528     C                   Z-ADD     0             ARBDTI
187700930528     C                   Z-ADD     0             ARBHTI
187800930603     C                   MOVEL     'S'           NOAMA
187900930528     C                   END
188000921021     C*
188100930603     C     ARTDAM        IFGT      0
188200921021     C     ARTDAM        ANDLT     ARBDAM
188300921021     C                   Z-ADD     ARTDAM        ARBDAM
188400921021     C                   END
188500930603     C*
188600930603     C* NOAMP = S NON E' ABILITATO IN BASE ALLA SPUNTA PART(NON ABBINAT
188700941209     C* ARTABN='S'   ===>  SPUNTA DI FOGLIO ABBINATO
188800941209     C     ARTABN        IFNE      'S'
188900930603     C                   MOVEL     'S'           NOAMP
189000060512     c* Mewmorizzo il segnacollo con la data più alta e verifico se
189100060512     c*  il relativo foglio IDA è stato chiuso per decidere come
189200060512     c*  impostare arbama
189300060512     c                   else
189400060512     c                   if        artdfv>abbdfv  and artflp=0
189500060512     c                   eval      abbnsc=artnsc
189600060512     c                   eval      abbdfv=artdfv
189700060512     c                   endif
189800060512     c                   if        artdut>abbdfv  and artflp>0
189900060512     c                   eval      abbnsc=artnsc
190000060512     c                   eval      abbdfv=artdut
190100060512     c                   endif
190200930603     C                   END
190300930604     C                   END
190400921019     C*
190500921019     C* CONTROLLO SE ALTRI COLLI HANNO L'ANOMALIA
190600921019     C  N06ARTCAN        IFNE      ' '
190700921019     C                   SETON                                        06
190800930604     C   20              SETON                                        34
190900921019     C                   END
191000921019     C*
191100941206     C  N34KARB2         READE     FNART01L                               34
191200921019     C                   END
191300921021     C*
191400930604     C  N20ARBDAM        IFEQ      *ALL'9'
191500921021     C                   Z-ADD     0             ARBDAM
191600921021     C                   END
191700101202     c*
191800060512     c* Se devo abilitare in base alla sola spunta partenza, verifico
191900060512     c*  foglio IDA
192000060512    1c                   if        noama='S' and noamp=' '  and
192100060512     c* Escluso i locali
192200060512     c                             arbtfp<>arbtfa
192300060512     c                   eval      knpg=1
192400070124     c     kspu          setll     fnbrv03l
192500070124     c     kspu          reade     fnbrv03l
192600070124    2c                   dow       not %eof(fnbrv03l)
192700060512     C                   CLEAR                   DSLV53
192800060512     C                   MOVEL     'A'           D53TFO
192900060512     C                   MOVEL     BRVNPG        D53NPG
193000060512     C                   Z-ADD     BRVNFV        D53NFV
193100060512     C                   MOVEL     BRVFGS        D53FGS
193200060512     C                   MOVEL     'A'           D53ABB
193300060512     C                   MOVEL     LNA           D53LNA
193400060512     C                   MOVEL     lnatfa        D53TFA
193500060512     C                   CALL      'FNLV53R'
193600060512     C                   PARM                    DSLV53
193700060512     c
193800060512     c* Escludo la defluenza
193900060512    3c                   if        d53err=' '  and d53def=' '
194000060512    4c                   if        d53nfa>0
194100060512     c* chain
194200060512     c     kfvv          chain     fnfvv01l
194300060512    5c                   if        %found(fnfvv01l) and fvvfcf='S'
194400060512     c                   eval      noamp='0'
194500060512   x5c                   else
194600060512     c                   clear                   noamp
194700060512     c                   leave
194800060512    5c                   endif
194900101202     c*
195000060512   x4c                   else
195100060512     c                   clear                   noamp
195200060512     c                   leave
195300060512    4c                   endif
195400060512    3c                   endif
195500101202     c*
195600070124     c     kspu          reade     fnbrv03l
195700060512    2c                   enddo
195800060512    1c                   endif
195900921019     C*
196000921019     C                   ENDSR
196100941206     C**************************************************************************
196200941206     C*    CONTROLLO SE ALMENO UN COLLO HA LA DATA
196300941206     C**************************************************************************
196400921019     C     CTRBTT        BEGSR
196500941206     C*
196600941206     C                   MOVEL     *ALL'9'       BTPDET
196700031114     C     KBTP2         SETLL     FNBTT11L
196800031114     C     KBTP2         READE     FNBTT11L                               34
196900921019     C*
197000921019     C     *IN34         DOWEQ     '0'
197100921021     C*
197200941206     C     BTTDET        IFNE      0
197300941206     C     BTTDET        ANDLT     BTPDET
197400941206     C                   MOVEL     BTTDET        BTPDET
197500921021     C                   END
197600921019     C*
197700031114     C     KBTP2         READE     FNBTT11L                               34
197800921019     C                   END
197900921021     C*
198000941206     C     BTPDET        IFEQ      *ALL'9'
198100941206     C                   Z-ADD     0             BTPDET
198200921021     C                   END
198300921019     C*
198400921019     C                   ENDSR
198500960417     C**************************************************************************
198600960417     C*    CREAZIONE ANOMALIA 005
198700960417     C**************************************************************************
198800960417     C     CRTA05        BEGSR
198900961120     C* SE SI TRATTA DI SPUNTA LOCALE, CERCO IL FOGLIO ARRIVI
199000961120     C*  CON DATA BORDERO' + 1
199100961120     C     WLOCAL        IFEQ      'S'
199200970127     C* SE NON C'E' LA DATA BORDERO' èRENDO LA DATA DEL GIORNO
199300970127     C     WDBR          IFGT      0
199400961120     C                   MOVE      WDBR          G08INV
199500970127     C                   ELSE
199600970127     C                   MOVE      DATEU         G08INV
199700970127     C                   ENDIF
199800970127     C*
199900961120     C                   MOVEL     '3'           G08ERR
200000961120     C                   CALL      'XSRDA8'
200100961120     C                   PARM                    WLBDA8
200200961120     C*
200300961120     C*  DATA BORDERO' + 1
200400961120     C     G08TGI        ADD       1             GIOTGI
200500961120     C                   CALL      'XSRGI8'
200600961120     C                   PARM                    WGIDAT
200700961120     C*
200800961120     C                   Z-ADD     GIOINV        WDBR1
200900961120     C                   Z-ADD     2             KNPG
201000961120     C     KFVV3         SETLL     FNFVV02L
201100961120     C     KFVV3         READE     FNFVV02L                               31
201200961120     C     *IN31         DOWEQ     *OFF
201300020919     c     fvvatb        andne     ' '
201400961120     C     KFVV3         READE     FNFVV02L                               31
201500961120     C                   ENDDO
201600961120     C  N31              Z-ADD     FVVDFV        W0060             6 0
201700961120     C   31              CLEAR                   W0060
201800961120     C**
201900961120     C** IMPOSTO 1 PRIMI CAMPI
202000031114     C                   Z-ADD     lnatfa        WFGS
202100961120     C                   MOVEL     2             WNPG
202200961120     C                   MOVEL     ' '           WSPG
202300961120     C**
202400101202     C                   SELECT
202500101202     C     W0060         WHENEQ    0
202600961120     C                   Z-ADD     WDBR1         WDFV
202700101202     C                   OTHER
202800961120     C                   Z-ADD     W0060         WDFV
202900101202     C                   ENDSL
203000961120     C*
203100961120     C                   ENDIF
203200961120     C**
203300990518     C                   CLEAR                   FNANM000
203400960417     C                   MOVE      LNP           ANMFLS
203500960417     C                   MOVE      LNA           ANMLNA
203600960417     C                   MOVE      NRS           ANMNRS
203700960417     C                   MOVE      NSC           ANMSCN
203800960417     C                   MOVE      WAAS          ANMAAS
203900960417     C                   MOVE      WLNP          ANMLNP
204000960417     C                   MOVE      WNSP          ANMNSP
204100960417     C* Ricerco fase
204200960417     C                   CLEAR                   DSFASE
204300960417     C                   MOVE      WNPG          D11NPG
204400960417     C                   MOVE      WSPG          D11SPG
204500960417     C                   CALL      'TRUL11R'
204600960417     C                   PARM                    DSFASE
204700960417     C                   MOVE      D11FAS        ANMFSA
204800960417     C                   Z-ADD     WDFV          ANMDAO
204900960417     C                   MOVE      WNPS          ANMNPS
205000961120     C                   MOVE      WCDU          ANMCDU
205100990518     C                   Z-ADD     WNFV          ANMNFV
205200960417     C                   MOVEL     WFGS          ANMFLE
205300960417     C                   MOVE      SAVLNP        WFL2
205400960417     C     ANMLNP        IFGT      0
205500960417     C                   MOVEL     ANMLNP        WFL1
205600960417     C                   ELSE
205700960417     C                   MOVEL     ANMFLS        WFL1
205800960417     C                   ENDIF
205900960417     C                   Z-ADD     5             COMCAA
206000960417     C                   EXSR      CRTANM
206100960417     C                   ENDSR
206200960417     C**************************************************************************
206300960417     C*    CREAZIONE ANOMALIA 015
206400960417     C**************************************************************************
206500960417     C     CRTA15        BEGSR
206600990518     C                   CLEAR                   FNANM000
206700960417     C* Cerco foglio ima della data della spunta cancellata
206800960417     C                   EXSR      RCRIMA
206900960417     C*
207000961118     C     WCREA         IFEQ      ' '
207100960417     C                   MOVE      LNP           ANMFLS
207200960417     C                   MOVE      LNA           ANMLNA
207300960417     C                   MOVE      NRS           ANMNRS
207400960417     C                   MOVE      NSC           ANMSCN
207500960417     C                   MOVE      WAAS          ANMAAS
207600960417     C                   MOVE      WLNP          ANMLNP
207700960417     C                   MOVE      WNSP          ANMNSP
207800960417     C* Ricerco fase
207900960417     C                   CLEAR                   DSFASE
208000960417     C                   MOVE      WNPG          D11NPG
208100960417     C                   MOVE      WSPG          D11SPG
208200960417     C                   CALL      'TRUL11R'
208300960417     C                   PARM                    DSFASE
208400960417     C                   MOVE      D11FAS        ANMFSA
208500960417     C                   MOVE      WVVDFV        ANMDAO
208600960417     C                   MOVE      WFGS          ANMCDU
208700990518     C                   Z-ADD     WNFV          ANMNFV
208800960417     C                   MOVE      WFGS          ANMFLE
208900960417     C                   CLEAR                   WFL2
209000960417     C     ANMLNP        IFGT      0
209100960417     C                   MOVEL     ANMLNP        WFL1
209200960417     C                   ELSE
209300960417     C                   MOVEL     ANMFLS        WFL1
209400960417     C                   ENDIF
209500960417     C*
209600960417     C                   Z-ADD     15            COMCAA
209700960417     C                   EXSR      CRTANM
209800961118     C                   ENDIF
209900960417     C                   ENDSR
210000960416     C**************************************************************************
210100960416     C*    CREAZIONE ANOMALIA
210200960416     C**************************************************************************
210300960416     C     CRTANM        BEGSR
210400960416     C                   Z-ADD     COMCAA        ANMCAA
210500960416     C     COMCAA        IFNE      SAVCAA
210600960416     C                   MOVEL     '7C'          COD
210700960416     C                   MOVEL(P)  ANMCAA        KEY
210800960416     C     KTAB          CHAIN     TABEL                              31
210900960416     C  N31              MOVEL     TBLUNI        DS7C
211000960416     C   31              MOVEL     *BLANKS       DS7C
211100960416     C                   MOVEL     COMCAA        SAVCAA
211200960416     C                   ENDIF
211300050707     c* imposto in anmflo data e ora creazione spunta
211400050707     C                   TIME                    W0140            14 0
211500050707     c
211600050707     C                   CLEAR                   WLBDA8
211700050707     C                   MOVE      w0140         G08DAT
211800050707     C                   CALL      'XSRDA8'
211900050707     C                   PARM                    WLBDA8
212000050707     C                   MOVE      G08INV        wdat6             6 0
212100050707     c                   movel     w0140         wora4             4 0
212200041018     c                   movel     wdat6         anmflo
212300041018     c                   move      wora4         anmflo
212400041018     c* Imposto la zona se trovo la bolla
212500041018     c                   z-add     999           anmfl4
212600041018     c                   select
212700041018     c                   when      wesblt='S'
212800041018     c                   z-add     blpznc        anmfl4
212900041018     c                   when      wesart='S'
213000041018     c                   z-add     arbznc        anmfl4
213100041018     c                   when      wesbtt='S'
213200041018     c                   z-add     btpznc        anmfl4
213300041018     c                   endsl
213400041018     c* Imposto se di valore
213500041018     c                   if        anmnsp>0
213600041018     c     kar5          chain     fiar501l
213700041018    3c                   if        %found(fiar501l)
213800041018     c                   movel     ar5uni        dar5gen
213900041018     c                   else
214000041018     c                   clear                   dar5gen
214100041018    3c                   endif
214200041018     c                   movel     §ar5bva       anmft4
214300041018     c                   endif
214400960416     C*
214500960416     C                   MOVEL     §7CTAN        ANMTAN
214600960416     C                   MOVEL     §7CSAN        ANMSAN
214700960416     C                   MOVEL     §7CAIE        ANMAIE
214800960416     C                   Z-ADD     §7CLID        ANMLID
214900960416     C                   Z-ADD     0             ANMDCH
215000960416     C                   MOVEL     ' '           ANMFSC
215100070424     C                   clear                   ANMcch
215200960416     C* CHIUSURA AUTOMATICA
215300960416     C     §7CCHA        IFEQ      'S'
215400960417     C                   Z-ADD     ANMDAO        ANMDCH
215500960417     C                   MOVE      ANMFSA        ANMFSC
215600960416     C                   END
215700960416     C                   Z-ADD     0             ANMFL1
215800960416     C                   Z-ADD     0             ANMFL2
215900960416     C* ESTERNA
216000000905    1C     §7CAIE        IFEQ      'E'
216100960416     C* VEDO A CHI TRASMETTERE
216200960416     C                   MOVEL     WFL1          ANMFL1
216300960416     C                   MOVEL     WFL2          ANMFL2
216400960416     C* SE UGUALI CANCELLO IL 2°
216500000905    2C     ANMFL1        IFEQ      ANMFL2
216600960416     C                   Z-ADD     0             ANMFL2
216700000905   X2C                   ELSE
216800000905     C                   CLEAR                   DSLV55
216900000905     C                   MOVEL     'P'           D55TPT
217000000905     C                   MOVE      ANMFL1        D55LIN
217100000905     C                   MOVE      DATEU         D55DRF
217200000905     C                   CALL      'FNLV55R'
217300000905     C                   PARM                    DSLV55
217400000905    3C     D55ERR        IFNE      *BLANKS
217500000905     C                   MOVE      ANMFL1        WTFP1
217600000905     C                   ELSE
217700000905     C                   MOVE      D55TFP        WTFP1
217800000905    3C                   END
217900000905     C                   CLEAR                   DSLV55
218000000905     C                   MOVEL     'P'           D55TPT
218100000905     C                   MOVE      ANMFL2        D55LIN
218200000905     C                   MOVE      DATEU         D55DRF
218300000905     C                   CALL      'FNLV55R'
218400000905     C                   PARM                    DSLV55
218500000905    3C     D55ERR        IFNE      *BLANKS
218600000905     C                   MOVE      ANMFL2        WTFP2
218700000905     C                   ELSE
218800000905     C                   MOVE      D55TFP        WTFP2
218900000905    3C                   END
219000000905    3C     WTFP1         IFEQ      WTFP2
219100000905     C                   CLEAR                   ANMFL2
219200000905    3C                   END
219300000905    2C                   ENDIF
219400961120     C                   ENDIF
219500960416     C*
219600961129     C* SE ANOMALIA INTERNA CHE USA SIA FLE CHE LNA LA FLAGGO PER
219700961129     C* NON TRASMETTERLA
219800961129    2C     §7CAIE        IFEQ      'I'
219900961129    2C     §7CUEA        ANDEQ     'E'
220000961129     C                   MOVEL     'T'           ANMFT1
220100961129    2C                   ENDIF
220200961129     C*
220300990518     C                   WRITE     FNANM000
220400960416     C                   ENDSR
220500960417     C**************************************************************************
220600960417     C*    RICERCA FOGLIO IMA
220700960417     C**************************************************************************
220800960417     C     RCRIMA        BEGSR
220900960417     C* DETERMINO FILIALE GESTIONE DELLA LNA DELLA SPUNTA
221000960417     C                   CLEAR                   WFGS
221100960417     C                   CLEAR                   WNFV
221200960417     C                   CLEAR                   WNPG
221300960417     C                   CLEAR                   WSPG
221400020418      * controllo su azcae
221500020422     C     LNA           LOOKUP    L6S                                    30
221600020422     C     *IN30         IFEQ      *OFF
221700020422     C                   MOVE      LNA           WFGS
221800020422     C                   ELSE
221900020418     c                   eval      ktfa = lna
222000020418     c                   eval      kdde = *date
222100020418     c     kazcae        setll     azcae02l
222200020418     c                   do        *hival
222300020418     c     kazcae        reade     azcae02l
222400020418     c                   if        %eof(azcae02l)
222500020418     c                   leave
222600020418     c                   endif
222700020418     c                   if        caeatb <> *blanks
222800020418     c                   iter
222900020418     c                   endif
223000020418     c                   if        caedde > kdde or caedsc < kdde
223100020418     c                   iter
223200020418     c                   endif
223300020418     c                   move      caetfe        wfgs
223400020422     c                   leave
223500020418     c                   enddo
223600020422     C                   ENDIF
223700101202      *
223800960417     C                   MOVEL     '3'           WNPG
223900960417     C* CHAIN SU FOGLIO IMA
224000960417     C     KFVV2         CHAIN     FNFVV02L                           30
224100000203     C  N30              Z-ADD     FVVNFV        WNFV
224200960417     C  N30              MOVE      FVVNPG        WNPG
224300960417     C  N30              MOVE      FVVSPG        WSPG
224400961118     C**
224500961118     C* SE IL FOGLIO NON C'E' O E' APERTO, NON CREO L'ANOMALIA
224600961118     C                   CLEAR                   WCREA
224700961118     C     *IN30         IFEQ      *ON
224800961118     C     FVVFCF        ORNE      'S'
224900961118     C                   MOVEL     'N'           WCREA             1
225000961118     C                   ENDIF
225100960417     C                   ENDSR
225200960605     C**************************************************************************
225300960605     C*    RICERCA LNP BOLLA
225400960605     C**************************************************************************
225500960605     C     RICLNP        BEGSR
225600031114     c* La tabella 5F qui non viene cercata perchè non serve: mi basta
225700031114     c*  soltanto sapere se c'e' spunta partenza da altro terminal per
225800031114     c*  la creazione della anomalia 05 altrimenti questo pgm non crea
225900031114     c*  nulla: al massimo cancella
226000961120     C                   MOVE      1             KNPG
226100070124     C     KBRV3         SETLL     FNBRV03L
226200070124     C     KBRV3         READE     FNBRV03L                               32
226300031114    1C     *IN32         DOWEQ     *OFF
226400031114    2C     BRVATR        IFEQ      *BLANKS
226500031114    3C     BRVpes        IFNE      LNP
226600061120     C     BRVfgs        andne     pestfp
226700960621     C                   MOVE      BRVFGS        LNPB
226800031114     c                   eval      *in32=*on
226900031114    3C                   END
227000031114    2C                   END
227100070124     C  N32KBRV3         READE     FNBRV03L                               32
227200031114    1C                   ENDDO
227300960605     C                   ENDSR
227400970530     C**************************************************************************
227500970530     C     *INZSR        BEGSR
227600970530     C                   MOVEL     KPJBU         SAVJBU
227700970530     C                   Z-ADD     1             CODUT             1 0
227800970530     C* GIRO DATA DEL GIORNO
227900970530     C                   TIME                    W0140            14 0
228000970530     C                   MOVE      W0140         DATEU             8 0
228100970530     C                   CLEAR                   WLBDA8
228200970530     C                   MOVE      DATEU         G08DAT
228300970530     C                   CALL      'XSRDA8'
228400970530     C                   PARM                    WLBDA8
228500970530     C                   MOVE      G08INV        DATEU
228600970530     C* DEFINIZIONE CAMPI
228700031114     C     *LIKE         DEFINE    d53def        wvvdef
228800031114     C     *LIKE         DEFINE    fvvdfv        wdtrif
228900031114     C     *LIKE         DEFINE    fvvdfv        dspb
229000031114     C     *LIKE         DEFINE    fvvdfv        SVVDFV
229100031114     C     *LIKE         DEFINE    blpLNA        SVVLNA
229200031114     C     *LIKE         DEFINE    blpLNP        SVVLNP
229300031114     C     *LIKE         DEFINE    blpLNA        LNATFA
229400031114     C     *LIKE         DEFINE    blpLNA        LNATFP
229500031114     C     *LIKE         DEFINE    blpLNA        LNPTFP
229600031114     C     *LIKE         DEFINE    blpLNA        PESTFA
229700031114     C     *LIKE         DEFINE    blpLNA        PESTFP
229800000905     C     *LIKE         DEFINE    ANMLNP        WTFP1
229900000905     C     *LIKE         DEFINE    ANMLNP        WTFP2
230000000904     C     *LIKE         DEFINE    FVVNFV        SAVNFA
230100970620     C     *LIKE         DEFINE    FVVNPG        SAVNPA
230200020917     c     *Like         Define    FvvFgs        SavLai
230300970620     C     *LIKE         DEFINE    KPJBU         SAVJBU
230400970530     C     *LIKE         DEFINE    FVVSPG        NFASPG
230500970530     C     *LIKE         DEFINE    FVVFGS        NFAFGS
230600970530     C     *LIKE         DEFINE    FVVFCF        NFAFCF
230700970530     C     *LIKE         DEFINE    FVVDFV        NFADFV
230800970530     C     *LIKE         DEFINE    FVVNPG        NFANPG
230900970530     C     *LIKE         DEFINE    FVVSPG        SPGF
231000970530     C     *LIKE         DEFINE    FVVNFV        NFAF
231100970530     C     *LIKE         DEFINE    FVVDFV        AEDF
231200970530     C     *LIKE         DEFINE    FVVDFV        WVVDFV
231300970530     C     *LIKE         DEFINE    FVVSPG        WVVSPG
231400970530     C     *LIKE         DEFINE    FVVDFV        SPUDFV
231500970530     C     *LIKE         DEFINE    BRVNPS        SPUNPS
231600970530     C     *LIKE         DEFINE    FVVDFV        ENTDFV
231700970530     C     *LIKE         DEFINE    BRVNPS        ENTNPS
231800990412     C     *LIKE         DEFINE    FVVDFV        A56DFV
231900990412     C     *LIKE         DEFINE    BRVNFV        A56NFV
232000990412     C     *LIKE         DEFINE    BRVFGS        A56FGS
232100990412     C     *LIKE         DEFINE    BRVNPS        A56NPS
232200990412     C     *LIKE         DEFINE    FVVSPG        A56SPG
232300990412     C     *LIKE         DEFINE    FVVNPG        A56NPG
232400970530     C     *LIKE         DEFINE    BRVFGS        WTFG
232500970530     C     *LIKE         DEFINE    FVVDFV        DATAF
232600970530     C     *LIKE         DEFINE    FVVSPG        SPUSPG
232700970530     C     *LIKE         DEFINE    BRVNPG        SPUNPG
232800970530     C     *LIKE         DEFINE    BRVFGS        SPUFGS
232900990409     C     *LIKE         DEFINE    BRVFGS        ENTFGS
233000970530     C     *LIKE         DEFINE    BRVNFV        SPUNFV
233100970530     C     *LIKE         DEFINE    FVVFCF        WCHIUS
233200970530     C     *LIKE         DEFINE    ANMCAA        COMCAA
233300970530     C     *LIKE         DEFINE    ANMCAA        SAVCAA
233400970530     C     *LIKE         DEFINE    ANMFL1        WFL1
233500970530     C     *LIKE         DEFINE    ANMFL2        WFL2
233600970530     C     *LIKE         DEFINE    ARBAAS        WAAS
233700970530     C     *LIKE         DEFINE    ANMLNP        WLNP
233800970530     C     *LIKE         DEFINE    ANMNSP        WNSP
233900970530     C     *LIKE         DEFINE    BRVNPS        WNPS
234000000203     C     *LIKE         DEFINE    BRVNFV        WNFV
234100970530     C     *LIKE         DEFINE    ANMCDU        WCDU
234200970530     C     *LIKE         DEFINE    FVVDFV        WDFV
234300970530     C     *LIKE         DEFINE    FVVNPG        WNPG
234400970530     C     *LIKE         DEFINE    FVVSPG        WSPG
234500970530     C     *LIKE         DEFINE    FVVFGS        WFGS
234600970530     C     *LIKE         DEFINE    ARTDAM        WDAM
234700070424     C     *LIKE         DEFINE    ARTDET        WDET
234800070424     C     *LIKE         DEFINE    ARTFLP        WFLP
234900970530     C     *LIKE         DEFINE    ARTDCM        WDCM
235000970530     C     *LIKE         DEFINE    ARTDTR        WDTR
235100970530     C     *LIKE         DEFINE    ARBDBR        WDBR
235200970530     C     *LIKE         DEFINE    ARBFBC        WFBC
235300970530     C     *LIKE         DEFINE    ARBDBR        WDBR1
235400970530     C     *LIKE         DEFINE    FVVDFV        SAVDFV
235500970530     C     *LIKE         DEFINE    FVVFGS        SAVLNP
235600970604     C     *LIKE         DEFINE    ARTDAM        SAVDAM
235700970604     C     *LIKE         DEFINE    BTTDET        SAVDET
235800970530     C     *LIKE         DEFINE    LNP           LNPB
235900970530     C     *LIKE         DEFINE    BRVNPG        KNPG
236000970721     C     *LIKE         DEFINE    GCPFRG        KFRG
236100970721     C                   MOVEL     '0'           KFRG
236200970530     C**
236300031114     C     Kbar37        KLIST
236400041014     C                   KFLD                    BPES
236500031114     C                   KFLD                    LNP
236600031114     C                   KFLD                    LNA
236700031114     C                   KFLD                    NRS
236800031114     C                   KFLD                    NSC
236900041015     C     KFGV          KLIST
237000041015     C                   KFLD                    dis1nfv
237100041015     C                   KFLD                    dis1fgs
237200031114     C     KBOL2         KLIST
237300970530     C                   KFLD                    LNP
237400970530     C                   KFLD                    LNA
237500970530     C                   KFLD                    NRS
237600970530     C                   KFLD                    NSC
237700060512     C     Kspu          KLIST
237800060512     C                   KFLD                    knpg
237900060512     c                   KFLD                    LNP
238000060512     C                   KFLD                    LNA
238100060512     C                   KFLD                    NRS
238200060512     C                   KFLD                    abbnsc
238300970530     C     KARB2         KLIST
238400970530     C                   KFLD                    ARBAAS
238500970530     C                   KFLD                    ARBLNP
238600970530     C                   KFLD                    ARBNRS
238700970530     C                   KFLD                    ARBNSP
238800970721     C     KGCA          KLIST
238900970721     C                   KFLD                    ARBAAS
239000970721     C                   KFLD                    ARBLNP
239100970721     C                   KFLD                    ARBNRS
239200970721     C                   KFLD                    ARBNSP
239300970721     C                   KFLD                    KFRG
239400970530     C     KARB          KLIST
239500970530     C                   KFLD                    ARTAAS
239600970530     C                   KFLD                    ARTLNP
239700970530     C                   KFLD                    ARTNRS
239800970530     C                   KFLD                    ARTNSP
239900970530     C     KBTP2         KLIST
240000031114     C                   KFLD                    BTPflp
240100031114     C                   KFLD                    BTPAAS
240200970530     C                   KFLD                    BTPLNP
240300970530     C                   KFLD                    BTPNRS
240400970530     C                   KFLD                    BTPNSP
240500031114     C     KBTT          KLIST
240600031114     C                   KFLD                    BTTflp
240700031114     C                   KFLD                    BTTAAS
240800970530     C                   KFLD                    BTTLNP
240900970530     C                   KFLD                    BTTNRS
241000970530     C                   KFLD                    BTTNSP
241100970530     C     KBLP2         KLIST
241200970530     C                   KFLD                    BLPAAS
241300970530     C                   KFLD                    BLPLNP
241400970530     C                   KFLD                    BLPNRS
241500970530     C                   KFLD                    BLPNSP
241600031114     C     KBLT          KLIST
241700970530     C                   KFLD                    BLTAAS
241800970530     C                   KFLD                    BLTLNP
241900970530     C                   KFLD                    BLTNRS
242000970530     C                   KFLD                    BLTNSP
242100970530     C     KFVV2         KLIST
242200970530     C                   KFLD                    WNPG
242300970530     C                   KFLD                    WVVDFV
242400970530     C                   KFLD                    WFGS
242500060512     C     KFVV          KLIST
242600060512     C                   KFLD                    d53npa
242700060512     C                   KFLD                    d53nfa
242800060512     C                   KFLD                    d53lai
242900970530     C     KFVV3         KLIST
243000970530     C                   KFLD                    KNPG
243100970530     C                   KFLD                    WDBR1
243200031114     c                   Kfld                    lnatfa
243300970530     C     KTAB          KLIST
243400970530     C                   KFLD                    CODUT
243500970530     C                   KFLD                    COD               2
243600970530     C                   KFLD                    KEY               8
243700970530     C     KBRV3         KLIST
243800970530     C                   KFLD                    KNPG
243900970530     C                   KFLD                    LNP
244000970530     C                   KFLD                    LNA
244100970530     C                   KFLD                    NRS
244200970530     C                   KFLD                    NSC
244300970530     C     KAGB          KLIST
244400970530     C                   KFLD                    AGBTBO
244500970530     C                   KFLD                    AGBAAS
244600970530     C                   KFLD                    AGBLNP
244700970530     C                   KFLD                    AGBNRS
244800970530     C                   KFLD                    AGBNSP
244900970530     C                   KFLD                    AGBAGB
245000001128     C*
245100101202      *
245200020418     c     kazcae        klist
245300020418     c                   kfld                    kepa
245400020418     c                   kfld                    ktfa
245500020418     c                   kfld                    ktfp
245600101202      *
245700030109     c     kFidta        Klist
245800030109     c                   Kfld                    SavAas
245900030109     c                   Kfld                    SavLnp1
246000030109     c                   Kfld                    SavNrs
246100030109     c                   Kfld                    SavNsp
246200030109     c                   Kfld                    SavTrd
246300030109     c                   Kfld                    SavPoc
246400041018     C     Kar5          KLIST
246500041018     C                   KFLD                    anmaas
246600041018     C                   KFLD                    anmLNp
246700041018     C                   KFLD                    anmNRS
246800041018     C                   KFLD                    anmnsp
246900041018     C                   KFLD                    Ktrd
247000970530     C*---------------------------------------------------------------*
247100970530     C*
247200970530     C*  Reperisco livello di chiusura anomalia come IDD
247300970530     C                   MOVEL(P)  2             KEY
247400970530     C                   MOVEL     '7A'          COD
247500970530     C     KTAB          CHAIN     TABEL                              31
247600970530     C  N31              MOVEL     TBLUNI        DS7A2
247700970530     C   31              CLEAR                   DS7A2
247800000825     C*
247900970530     C                   MOVEL     SAVJBU        KPJBU
248000970530     C                   ENDSR
248100970530     C**************************************************************************
248200970530     C     MEMDAT        BEGSR
248300041015     c                   clear                   wnoarrivo
248400041014     c* Faccio 3 memorizzazioni:
248500041014     c* 1) Spunta transito: solo se chi spara è p.o. spunta
248600041014    1c                   if        wesbtt='S' and brvpes=BPES   or
248700041014     c*    oppure spunta di disguido: idem
248800041018     c                             wesbtt<>'S' and wagpar=' ' and wagarr=' '
248900041014     c                             and brvpes=bpes
249000041014    2c                   if        tradfv=0 or spudfv<tradfv
249100970530     C                   MOVEL     BRVNPS        SPUNPS
249200970530     C                   MOVE      BRVNPG        SPUNPG
249300970530     C                   MOVE      BRVNFV        SPUNFV
249400970530     C                   MOVE      BRVFGS        SPUFGS
249500970530     C                   MOVEL     SPGF          SPUSPG
249600041014     C                   MOVEL     spudfv        tradfv
249700041014     C                   MOVEL     BRVNPS        traNPS
249800041014     C                   MOVE      BRVNPG        traNPG
249900041014     C                   MOVE      BRVFGS        traFGS
250000041014     C                   SETON                                        1918
250100041014    2c                   endif
250200041014    1c                   endif
250300041014     c* Per spunta transito o disguido memorizzo la spunta con data/ora cari
250400041014     c*  più alta, per aggiornare altro eventuale disguido/transito
250500041014    1c                   if        wesbtt='S' or
250600041014     c                             wagpar=' ' and wagarr=' '
250700041014     c* Non è cat.partenza non è ne del term.partenza  nè del term.arrivo
250800041015    2c                   if        brvnpg<>1 or  deff='S'
250900041014    3c                   if        wtfg<>lnatfa and brvfle<>lnptfp
251000041014    4c                   if        dis2dfv=0 or spudfv>=dis2dfv
251100041014     C                   MOVEL     BRVNPS        dis2nPS
251200041014     C                   MOVE      BRVNFV        dis2NFV
251300041014     C                   MOVE      BRVFGS        dis2FGS
251400041014     C                   MOVE      spudfv        dis2dfv
251500041015     c                   if        brvscar>0
251600041014     C                   MOVE      brvscar       dis2scar
251700041015     c                   else
251800041015     c                   move      brvimm        dis2scar
251900041015     c                   endif
252000041014    4c                   endif
252100041014    3c                   endif
252200041014    2c                   endif
252300101202     c*
252400041014    1c                   endif
252500101202     c*
252600041014     c* 2) Spunta partenza: il terminal di partenza spunta deve essere
252700041014     c*                     simfel
252800041015    1c                   if        wagpar='S' and brvfle=simfel
252900041015    2c                   if        prtdfv=0 or spudfv<prtdfv
253000041014     C                   MOVEL     BRVNPS        SPUNPS
253100041014     C                   MOVE      BRVNPG        SPUNPG
253200041014     C                   MOVE      BRVNFV        SPUNFV
253300041014     C                   MOVE      BRVFGS        SPUFGS
253400041014     C                   MOVEL     SPGF          SPUSPG
253500041014     C                   MOVEL     spudfv        prtdfv
253600041014     C                   MOVEL     BRVNPS        prtNPS
253700041014     C                   MOVE      BRVNPG        prtNPG
253800041014     C                   MOVE      BRVFGS        prtFGS
253900041014     C                   SETON                                        1917
254000041014    2c                   endif
254100041014     c* Per i danni, cerco la spunta con data più alta
254200041018    2c                   if        dandfv=0 or dandfv<=spudfv
254300041014     C                   MOVEL     BRVNPS        danNPS
254400041014     C                   MOVE      spudfv        dandFV
254500041014     C                   MOVE      BRVFGS        danFGS
254600041018     C                   MOVE      BRVnpg        dannpg
254700041018     C                   MOVE      spgf          danspg
254800041014    2c                   endif
254900041014     c* Se bolla locale, memorizzo a parte dati spunta entrata
255000041014    2c                   if        wsploc<>*blanks and entdfv=0 or
255100041014     c                             wsploc<>*blanks and spudfv<entdfv
255200041014    3c                   if        brvnpg=5 or
255300041014     c                             (brvnpg=3 and spgf='I') or
255400041014     c                             (brvnpg=3 and spgf='P') or
255500041014     c                             (brvnpg=1 and deff<>'S')
255600041014     C                   MOVEL     spudfv        ENTDFV
255700041014     C                   MOVE      BRVNPS        ENTNPS
255800041014     C                   MOVE      BRVFGS        ENTFGS
255900041015     c* non devo consdierare la spunta per l'aggiornamento in arrivo
256000041015     c                   eval      wnoarrivo='N'
256100041014    3c                   endif
256200041014    2c                   endif
256300101202     c*
256400041014    1c                   endif
256500101202     c*
256600041014     c* 2) Spunta arrivo  : il terminal di arrivo spunta deve essere
256700041014     c*                     term.arrivo lna spunta annullata
256800041015    1c                   if        wagarr='S' and brvfle=simfel  and
256900041015     c                             wnoarrivo<>'N'
257000041014    2c                   if        arrdfv=0 or spudfv<arrdfv
257100041014     C                   MOVEL     BRVNPS        SPUNPS
257200041014     C                   MOVE      BRVNPG        SPUNPG
257300041014     C                   MOVE      BRVNFV        SPUNFV
257400041014     C                   MOVE      BRVFGS        SPUFGS
257500041014     C                   MOVEL     SPGF          SPUSPG
257600041014     C                   MOVEL     spudfv        arrdfv
257700041014     C                   MOVEL     BRVNPS        arrNPS
257800041014     C                   MOVE      BRVNPG        arrNPG
257900041014     C                   MOVE      BRVFGS        arrFGS
258000101202     c*
258100050509    3c                   if        brvnpg=5 or
258200050509     c                             (brvnpg=3 and spgf='I') or
258300050509     c                             (brvnpg=3 and spgf='P') or
258400050509     c                             (brvnpg=1 and deff<>'S')
258500050509     c                   eval      arrfl2='P'
258600050509     c                   else
258700050509     c                   eval      arrfl2=' '
258800050509     c                   endif
258900041014     C                   SETON                                        1916
259000041014    2c                   endif
259100041014     c* Per i danni, cerco la spunta con data più alta
259200041018    2c                   if        dandfv=0 or dandfv<=spudfv
259300041014     C                   MOVEL     BRVNPS        danNPS
259400041014     C                   MOVE      spudfv        dandFV
259500041014     C                   MOVE      BRVFGS        danFGS
259600041018     C                   MOVE      BRVnpg        dannpg
259700041018     C                   MOVE      spgf          danspg
259800041014    2c                   endif
259900041014    1c                   endif
260000101202     c*
260100110908     C                   ENDSR
260200031114     C**************************************************************************
260300031114     C* TERMINAL DI PARTENZA E ARRIVO DEL P.O. EFFETTUA SPUNTA
260400031114     C**************************************************************************
260500031114     C     TERPES        BEGSR
260600031114     C* IMPOSTO TERMINAL PARTENZA E ARRIVO
260700031114     C                   CLEAR                   DSLV55
260800031114     C                   MOVEL     WTITER        D55TPT
260900031114     C                   MOVEL     LNPB          D55LNP
261000031114     C                   MOVEL     WDTRIF        D55DRF
261100031114     C                   MOVE      bpes          D55LIN
261200031114     C                   EXSR      FNLV55
261300031114     C                   ENDSR
261400031114     C**************************************************************************
261500031114     C* CERCO TERMINAL PARTENZA LNP COLLO
261600031114     C**************************************************************************
261700031114     C     TERPAR        BEGSR
261800031114     C                   CLEAR                   DSLV55
261900031114     C                   MOVEL     'P'           D55TPT
262000031114     C                   MOVEL     LNPB          D55LIN
262100031114     C                   MOVEL     WDTRIF        D55DRF
262200031114     C                   EXSR      FNLV55
262300031114     C                   MOVE      D55TFP        LNPTFP
262400031114     C**
262500031114     C                   ENDSR
262600031114     C**************************************************************************
262700031114     C* CONTROLLO COSA POSSO AGGIORNARE IN BASE A CHI SONO
262800031114     C**************************************************************************
262900031114     C     CTRAGG        BEGSR
263000031114     C                   CLEAR                   WAGPAR
263100031114     C                   CLEAR                   WAGARR
263200031114     C**
263300031114     C     LNPB          CHAIN     AZORG01L                           34
263400031114     C**
263500031114     C** CERCO TERMINAL DELLA LINEA DI ARRIVO
263600031114     C                   EXSR      TERARR
263700031114     C* FACCIO PARTENZA DELLA STESSA AREA PARTENZA SE:
263800041013    1C***  BAS           IFEQ      LNPAS
263900031126     c* Se c'e'bolla transito x simfel è transito e non cerco altro
264000041013     c     wesbtt        ifne      'S'
264100031114     C*
264200031114     C*  1) P.O. EFFETTUA SPUNTA = LNP SPUNTA (DA BOLLA)
264300031114    2C     BPES          IFEQ      LNPB
264400031114     C*  2) P.O. EFFETTUA SPUNTA E' IL TER.PARTENZA DELLA LNP SPUNTA
264500031114     C*     IN BASE ALLA DATA SPEDIZIONE SE TROVATA LA BOLLA
264600031114     C     BPES          OREQ      LNPTFP
264700031114     C                   MOVEL     'S'           WAGPAR            1
264800031114   X2C                   ELSE
264900031114     C*  3) TER.PARTENZA P.O. EFFETTUA SPUNTA (IN BASE ALLA DATA SPED
265000031114     C*     SE TROVATA LA BOLLA) = TER.PARTENZA LNP SPUNTA SEMPRE IN
265100031114     C*     BASE ALLA DATA SPEDIZIONE SE TROVATA BOLLA
265200031114     C                   MOVEL     'P'           WTITER            1
265300031114     C                   EXSR      TERPES
265400031114    3C     D55TFP        IFEQ      LNPTFP
265500031114     C                   MOVEL     'S'           WAGPAR            1
265600031114    3C                   ENDIF
265700031114    2C                   ENDIF
265800041013    1C                   ENDIF
265900031114     C**
266000031114     C** NON AGGIORNO BOLLE ARRIVO SE SPUNTA DELL'AREA PARTENZA,
266100031114     C*   SPUNTA TIPICA PARTENZA E P.O. SPUNTA = LNA SPUNTA O TFA SPU
266200031114     c* controllo solo se spunta tipica partenza: l'arrivo non mi
266300031114     c*  interessa. non so perchè lo avevo preso in considerazione!!
266400031114    1C     WAGPAR        IFEQ      'S'
266500031114     C* PARTENZA NON DEFLUENZA
266600031114    3C     parnpg        IFEQ      1
266700031114     C     WVVDEF        ANDEQ     '0'
266800031114     C* ENTRATA
266900031114     C     parnpg        OREQ      5
267000031114     C* I.M.P.
267100031114     C     parnpg        OREQ      3
267200031114     C     wvvspg        ANDEQ     'P'
267300031114     C* I.M.I.
267400031114     C     parnpg        OREQ      3
267500031114     C     wvvspg        ANDEQ     'I'
267600031114     C                   GOTO      NOAGAR
267700031114    3C                   ENDIF
267800031114    2C**                 ENDIF
267900031114    1C                   ENDIF
268000031114     C**
268100031114     c     wesbtt        ifne      'S'
268200031114     C* AGGIORNO BOLLE ARRIVO SE :
268300031114     C*  1) P.O. EFFETTUA SPUNTA = LNA SPUNTA
268400031114    1C                   SELECT
268500031114     C     BPES          WHENEQ    lna
268600031114     C                   MOVEL     'S'           WAGARR            1
268700031114    2C     WAGPAR        IFEQ      'S'
268800031114     C                   MOVEL     'A'           WSPLOC            1
268900031114    2C                   ENDIF
269000031114     C*  2) P.O. EFFETTUA SPUNTA E' IL TER.ARRIVO   DELLA LNA SPUNTA
269100031114     C*     IN BASE ALLA DATA SPUNTA
269200031114     C     BPES          WHENEQ    LNATFA
269300031114     C                   MOVEL     'S'           WAGARR            1
269400031114    2C     WAGPAR        IFEQ      'S'
269500031114     C                   MOVEL     'T'           WSPLOC            1
269600031114    2C                   ENDIF
269700031114   X1C                   OTHER
269800031114     C*  3) TER.ARRIVO P.O. EFFETTUA SPUNTA
269900031114     C*     = TER.ARRIVO LNA SPUNTA IN BASE ALLA DATA SPUNTA
270000031114    2C     PESTFA        IFEQ      LNATFA
270100031114     C                   MOVEL     'S'           WAGARR            1
270200041014    2C     WAGPAR        IFEQ      'S'
270300031114     C                   MOVEL     'D'           WSPLOC
270400031114    2C                   ENDIF
270500041014    2C                   ENDIF
270600031114    1C                   ENDSL
270700031114     c*
270800031114    2C                   endif
270900031114     C     NOAGAR        TAG
271000031114     C* DEVO DETERMINARE SE LE BOLLE DELLA LNA SI TROVANO SULL'AS
271100031114     C*  IN CUI MI TROVO
271200041013     C**   LNAAS         IFEQ      BAS
271300041014     C**                 MOVEL     'S'           WSIBOL            1
271400041013     C**                 ENDIF
271500031114     C**
271600031114     C                   ENDSR
271700031114     C**************************************************************************
271800031114     C* CERCO TERMINAL ARRIVO LNA COLLO
271900031114     C**************************************************************************
272000031114     C     TERARR        BEGSR
272100031114     C**
272200031114     C* LINEA DI ARRIVO
272300031114     C     lna           IFNE      SVVLNA
272400031114     C     LNPB          ORNE      SVVLNP
272500031114     C     wvvdfv        ORNE      SVVDFV
272600031114     C*
272700031114     C* TEMINAL DI ARRIVO
272800031114     C                   CLEAR                   DSLV55
272900031114     C                   MOVEL     ' '           D55TPT
273000031114     C                   MOVEL     wvvdfv        D55DRF
273100031114     C                   MOVEL     lna           D55LIN
273200031114     C                   MOVEL     LNPB          D55LNP
273300031114     C                   EXSR      FNLV55
273400031114     C**
273500031114     C                   MOVEL     D55TFA        LNATFA
273600031114     C                   MOVEL     D55TFP        LNATFP
273700031114     C*
273800031114     C                   MOVEL     lna           SVVLNA
273900031114     C                   MOVEL     lnPB          SVVLNP
274000031114     C                   MOVEL     wvvdfv        SVVDFV
274100031114     C                   ENDIF
274200031114     C                   ENDSR
274300031114     C**************************************************************************
274400031114     C* CALL A PGM FNLV55R
274500031114     C**************************************************************************
274600031114     C     FNLV55        BEGSR
274700031114     C                   CALL      'FNLV55R'
274800031114     C                   PARM                    DSLV55
274900031114     C**
275000031114     C     D55ERR        IFNE      ' '
275100031114     C                   MOVE      D55LIN        D55TFA
275200031114     C                   MOVE      D55LIN        D55TFP
275300031114     C                   END
275400031114     C                   ENDSR
