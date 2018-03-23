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
045400150819     C** SE NON TROVATA LA BOLLA, CERCO SE ESISTE L' EVENTUALE
045500031126     C*   SPUNTA PARTENZA
045600031114     C                   EXSR      RICLNP
045700150819     c
045800031114     c* prendo terminal di partenza della linea partenza calcolata
045900031114     C                   Z-ADD     wvvdfv        WDTRIF
046000031114     C                   EXSR      TERPAR
046100031114    3C                   ENDIF
046200031114    2C                   ENDIF
046300031114    1C                   ENDIF
046400031114     C**
046500031114     C** CONTROLLO TRA P.O. SPUNTA E LINEE SPUNTE CHE AGGIONAMENTI
046600031114     C**  POSSO FARE
046700031114     c*
046800150819     C* TERMINAL arrivo   DI BPES
046900031114     C                   MOVEL     'A'           WTITER
047000031114     C                   MOVEL     wvvdfv        WDTRIF
047100031114     C                   EXSR      TERPES
047200031114     C                   Z-ADD     D55TFA        PESTFA
047300031114     c
047400031114     C                   EXSR      CTRAGG
047500960416     C*
047600960416     C                   CLEAR                   WCHIUS
047700960416     C                   CLEAR                   WDFV
047800960416     C                   CLEAR                   WNPS
047900960416     C                   CLEAR                   WNFV
048000960416     C                   CLEAR                   WNPG
048100960417     C                   CLEAR                   WSPG
048200960416     C                   CLEAR                   WFGS
048300961120     C                   CLEAR                   WCDU
048400960416     C                   CLEAR                   WSPUN
048500031114     C                   CLEAR                   WLOCAL
048600960417     C                   CLEAR                   SAVDFV
048700960417     C                   CLEAR                   SAVLNP
048800970127     C                   CLEAR                   WGIAC
048900930604     C**
049000070124     C     KBOL2         SETLL     FNBRV07L
049100070124     C     KBOL2         READE     FNBRV07L                               32
049200930604     C*
049300970625    1C     *IN32         DOWEQ     '0'
049400961120     C* LA SPUNTA LETTA NON DEVE ESSERE QUELLA CHE STO ANNULLANDO
049500970625    2C     BRVNPG        IFNE      PARNPG
049600950118     C     BRVNFV        ORNE      PARNFV
049700950118     C     BRVFGS        ORNE      PARFGS
049800961120     C**
049900041013     C* CODICE ANOMALIA COLLO  tengo l'ultima che trovo
050000041013    3C     brvCAN        IFne      ' '
050100950118     C                   MOVEL     BRVCAN        SAVCAN            1
050200961118    3C                   END
050300970625     C**
050400970625     C* SOLO SE FLAG ='1' FACCIO GLI ALTRI CONTROLLI RELATIVI ALLE
050500970625     C* SPUNTE
050600970625    3C     FLG           IFEQ      '1'
050700961120     C**
050800961120     C* CHAINO I FOGLI A SECONDA DELLA CATEGORIA E FIL.GESTIONE
050900961120     C                   EXSR      CHAFV
051000970530     C**
051100970530     C* CONTROLLO IL TIPO PISTOLA DELLA SPUNTA LETTA
051200060512     c                   movel     brvnps        knps
051300970530     C                   EXSR      TIPPIS
051400110908     c
051500110908     c* Verifico se presente altra spunta reale sul sistema per non
051600110908     c*  riaprire  eventuale anomalia 115
051700110908    4C     §7JRPS        IFNE      'A'
051800110908     c                   eval      a_115dfv=dataf
051900110908     C                   MOVE      BRVFGS        a_115FGS
052000110908     C                   MOVE      BRVnpg        a_115npg
052100110908     C                   MOVE      spgf          a_115spg
052200110908     c                   endif
052300110908     c
052400041015     c* Per spunta transito o disguido memorizzo la spunta con data/ora cari
052500041015     c*  più alta, per aggiornare altro eventuale disguido/transito
052600041015     c                   if        d53err=' '  or d53err='6'
052700041015    4c                   if        wesbtt='S' or
052800041015     c                             wagpar=' ' and wagarr=' '
052900041015     c* Categoria partenza
053000041015    5c                   if        brvnpg=1 and deff=' '
053100041015    6c                   if        dis1dfv=0 or dataf>=dis1dfv
053200041015     C                   MOVEL     BRVNPS        dis1nPS
053300041015     C                   MOVE      BRVNFV        dis1NFV
053400041015     C                   MOVE      BRVFGS        dis1FGS
053500041015     C                   MOVE      dataf         dis1dfv
053600041015     c                   if        brvscar>0
053700041015     C                   MOVE      brvscar       dis1scar
053800041015     c                   else
053900041015     c                   move      brvimm        dis1scar
054000041015     c                   endif
054100041015     C                   MOVE      brvatr        dis1atr
054200041015    6c                   endif
054300041015    5c                   endif
054400041015    4c                   endif
054500041015    4c                   endif
054600101202     c*
054700041015     C* CONSIDERO SOLO SE NON C'E' ERRORE
054800041015    4C     D53ERR        IFEQ      ' '
054900960606     C*
055000961120     C*  ESCLUDO LE SPUNTE AUTOMATICHE CHE NON POSSONO AGGIORNARE LA
055100961120     C*  DATA DI ARRIVO
055200970625    5C     §7JRPS        IFNE      'A'
055300970530     C* CONSIDERO COMUNQUE LA PISTOLA AUTOMATICA DEI TRANSITI PERCHE'
055400970530     C*  AGGIORNA LA DATA DI ARRIVO TRANSITO
055500970530     C     BRVNPS        OREQ      89
055600961120     C*
055700961120     C* DETERMINO TERMINAL DI ARRIVO DELLA FILIALE GESTIONE SPUNTA LETT
055800961120     C                   EXSR      TERFGS
055900961120     C*
056000031126     C* SE DEVO AGGIORNARE L'ARRIVO: CONSIDERO ANCHE LE SPUNTE DEL TERMINAL
056100031126     C*   ALTRIMENTI  CONSIDERO SOLO LE SPUNTE DI SIMFEL
056200031126    6C                   IF        BRVFLE=SIMFEL  OR
056300031126     C                             WAGARR='S' AND WTFG  =LNATFA
056400031126     c* FOGLI VARI
056500970625    7C     WFVV          IFEQ      'S'
056600961120     C*
056700041014     C* SE TROVATA SPUNTA vedo se memorizzare i dati
056800970530     C                   MOVEL     DATAF         SPUDFV
056900970530     C                   EXSR      MEMDAT
057000990412     C**
057100990412     C** MEMORIZZO SE C'E' UN'ALTRA SPUNTA IMA/IMG/CONSEGNE
057200990412     C** PER ANOMALIA DI DISGUIDO
057300990412    8C     BRVNPG        IFEQ      3
057400990412     C     NFASPG        ANDEQ     'A'
057500990412     C     BRVNPG        OREQ      3
057600990412     C     NFASPG        ANDEQ     'G'
057700990412     C     BRVNPG        OREQ      4
057800990412     C                   MOVEL     DATAF         A56DFV
057900990412     C                   MOVE      BRVNPS        A56NPS
058000990412     C                   MOVE      BRVFGS        A56FGS
058100990412     C                   MOVE      BRVNPG        A56NPG
058200990412     C                   MOVE      BRVNFV        A56NFV
058300990412     C                   MOVE      NFASPG        A56SPG
058400990412    8C                   ENDIF
058500041015     c*
058600041015     c                   else
058700041015     c* La spunta partenza la consdiero per le partenze e transiti
058800041015     c* solo se non è autogenerata
058900041015     c                   if        brvscar>0 and wagarr<>'S'
059000041015     C                   MOVEL     DATAF         SPUDFV
059100041015     C                   EXSR      MEMDAT
059200990412    7C                   ENDIF
059300041015    7C                   ENDIF
059400961120     C**
059500961120     C* FOGLI PARTENZA LOCALE
059600970530     C* CONSIDERO SOLO SE SI TRATTA DI  DEFLUENZA
059700970625    7C     WFGV          IFEQ      'S'
059800970530     C     DEFF          ANDEQ     'S'
059900970530     C* SE TROVATA SPUNTA MEMORIZZO ANCHE GLI ALTRI DATI
060000970530     C                   MOVEL     DATAF         SPUDFV
060100970530     C                   EXSR      MEMDAT
060200970625    7C                   ENDIF
060300961120     C**
060400970625    6C                   END
060500970625    5C                   END
060600960417     C*
060700960417     C* SE SPUNTA PARTENZA MEMORIZZO QUELLA CON DATA PIù RECENTE
060800960417     C* E VERIFICO SE RELATIVA A FV ABBINATO A FOGLIO ARRIVI GIà CHIUSO
060900961120     C*  PERCHE' DEVO CREARE L'ANOMALIA 05-COLLO PARTITO NON ARRIVATO
061000041014    5C     *IN16         IFEQ      *OFF
061100031126     c     wagarr        andeq     'S'
061200961120     C     BRVNPG        ANDEQ     1
061300970530     C     DEFF          ANDNE     'S'
061400961118     C**
061500970625    6C     WFVA          IFEQ      'S'
061600970530     C     DATAF         ANDGT     SAVDFV
061700031127     c* rimemorizzo anche se trovo il foglio gisuto abbinato,
061800031127     C*  In caso di transito infatti su As unico leggo anche la spunta part
061900031127     c*  originale non diretta all'arrivo. Il foglio lo trova ma mi da non
062000031127     c*  abbinato (xchè abbinato al transito e non all'arrivo)
062100031127     c*  quindi se poi leggo il foglio giusto lo devo memorizzare
062200031127     c*  Se troviamo soluzione migliore...meglo!!
062300031127    6C     WFVA          oreq      'S'
062400031127     C     DATAF         ANDle     SAVDFV
062500031127     c     wnfaf         andeq     0
062600031127     c     nfaf          andgt     0
062700970530     C                   MOVE      DATAF         SAVDFV
062800000904     C                   MOVE      BRVFGS        SAVLNP
062900960417     C                   MOVE      BRVNPS        WNPS
063000961120     C                   MOVEL     ' '           WLOCAL            1
063100031127     C                   z-add     nfaf          Wnfaf
063200961120     C**
063300970530     C* SE FVP ABBINATO ED HO TROVATO FOGLIO ARRIVI, MEMORIZZO  I DATI
063400970625    7C     NFAF          IFGT      0
063500970530     C     NFAFCF        ANDNE     *BLANKS
063600960417     C                   MOVE      'S'           WCHIUS
063700970530     C                   MOVE      BRVNFV        WNFV
063800970530     C                   MOVE      BRVFGS        WCDU
063900970530     C                   MOVE      NFADFV        WDFV
064000970530     C                   MOVE      NFANPG        WNPG
064100970530     C                   MOVE      NFASPG        WSPG
064200970530     C                   MOVE      NFAFGS        WFGS
064300970625   X7C                   ELSE
064400960417     C                   CLEAR                   WCHIUS
064500970625    7C                   END
064600970625    6C                   END
064700961118     C**
064800961118     C** FOGLIO LOCALE: COME SE FOSSE SEMPRE ABBINATO E F.ARRIVI CHIUSO
064900031114    6C     wagarr        IFEQ      'S'
065000970530     C     WFGV          ANDEQ     'S'
065100970530     C     DATAF         ANDGT     SAVDFV
065200970530     C                   MOVE      DATAF         SAVDFV
065300970530     C                   MOVE      BRVFGS        SAVLNP
065400961120     C                   MOVE      BRVNPS        WNPS
065500970530     C                   MOVEL     'S'           WLOCAL            1
065600970530     C*
065700970530     C                   MOVE      'S'           WCHIUS
065800970530     C                   MOVE      BRVNFV        WNFV
065900961120     C                   MOVE      SIMFEL        WCDU
066000961120     C**
066100970625    6C                   ENDIF
066200970625    5C                   ENDIF
066300961118     C**
066400961118     C** CONTROLLO LE SPUNTE AI FINI IMA:
066500961118     C**  -SE TROVO UNA SPUNTA CON DATA> DELLA DATA SPUNTA CHE CANCELLO
066600961118     C**   SICURAMENTE NON DEVO CREARE ANOMALIA COLLO MANCANTE
066700961120     C**  -SE TROVO UNA SPUNTA CON DATA = ALLA DATA SPUNTA CHE CANCELLO
066800961118     C**   CONSIDERO VALIDE SOLO LE CATEGORIA 3  4 E 5
066900961118     C**  - ESCUDO SEMPRE LE SPUNTE CATEGORIA 1 PERCHE' NON SONO CONSID
067000961118     C**    NEMMENO DAL PGM DELL'IMA
067100031114    5C     wagarr        IFEQ      'S'
067200961120     C     WFVV          ANDEQ     'S'
067300961118     C*
067400970625    6C     DATAF         IFGT      WVVDFV
067500960417     C                   MOVE      '1'           WSPUN             1
067600970625    6C                   ENDIF
067700961118     C*
067800970625    6C     DATAF         IFEQ      WVVDFV
067900970625    7C     BRVNPG        IFEQ      3
068000961118     C     BRVNPG        OREQ      4
068100961118     C     BRVNPG        OREQ      5
068200041013     C     BRVNPG        OREQ      8
068300961118     C                   MOVE      '1'           WSPUN             1
068400970625    7C                   ENDIF
068500970625    6C                   ENDIF
068600961118     C*
068700970625    5C                   END
068800970625    4C                   END
068900970625    3C                   END
069000970625    2C                   END
069100930604     C*
069200070124     C     KBOL2         READE     FNBRV07L                               32
069300970625    1C                   ENDDO
069400031114     C**
069500031114     C** A G G I O R N A M E N T O   B O L L E   P A R T E N Z A
069600031114     C**
069700031114     c** elimino data su collo
069800031114    1C     wagpar        IFEQ      'S'
069900941230     C     KBOL2         CHAIN     FNBLT27L                           3239
070000990409     C*
070100031114    2C     *IN39         IFEQ      *OFF
070200990409     C     KBOL2         CHAIN     FNDCD02L                           3839
070300060512     c* La pistola è corrispondente se uguali, o entrambe manuali
070400060512   2ac                   if        not *in38 and not *in39
070500060512     c                   movel     dcdnps        knps
070600060512     c                   exsr      TIPPIS
070700101202     c*
070800060512     C     DCDNPS        IFEQ      NPS
070900990409     C     DCDDFV        ANDEQ     WVVDFV
071000060512     C     spurps        oreq      'M'
071100060512     C     §7jrps        andeq     spurps
071200990409     C                   MOVEL     'S'           WDCD              1
071300990409     C                   ELSE
071400990409     C                   UNLOCK    FNDCD02L
071500031114    3C                   ENDIF
071600060512   2aC                   ENDIF
071700031114    2C                   ENDIF
071800921013     C*
071900921013     C   39              GOTO      ENDUPD
072000101202     c*
072100031203     C* se c'e' la bolla aggiorno
072200031203   1ac                   if        not *in32
072300970530     C                   MOVE      BLTAAS        WAAS
072400970530     C                   MOVE      BLTLNP        WLNP
072500970530     C                   MOVE      BLTNSP        WNSP
072600920810     C*
072700970530    2C     SAVCAN        IFEQ      ' '
072800921020     C     CAN           ANDEQ     BLTCAN
072900921020     C                   MOVEL     ' '           BLTCAN
073000961121    2C                   END
073100921019     C*
073200101202     c*
073300970530     C* CANCELLO A SOSTITUISCO LA DATA ENTRATA SOLO SE CORRISPONDONO
073400970530     C*  PISTOLA E DATA
073500970530    2C     FLG           IFEQ      '1'
073600060512     c                   movel     bltnpe        knps
073700060512     c                   exsr      TIPPIS
073800060512   2aC     BLTNPE        ifeq      NPS
073900970530     C     BLTDSE        ANDEQ     WVVDFV
074000060512     c* oppure se sono entrambe manuali
074100060512     c     spurps        oreq      'M'
074200060512     c     §7jrps        andeq     spurps
074300970530     C*
074400961121     C* SE NON TROVATA NESSUNA SPUNTA, NEMMENO ENTRATA PULISCO LE DATE
074500041014     c                   select
074600041014    3C     *IN17         wheneq    *OFF
074700961121     C     ENTDFV        ANDEQ     0
074800041014     C     prtDFV        ANDEQ     0
074900970530     C                   MOVEL     *ZEROS        BLTDSE
075000970530     C                   MOVEL     *ZEROS        BLTNPE
075100060512     C                   MOVEL     ' '           BLTCAN
075200961121     C* SE COLLO LOCALE E
075300961121     C*  TROVATA SOLO SPUNTA ENTRATA O <= ALL'ALTRA SPUNTA TROVATA, USO
075400041014    3C     *IN17         wheneq    *Off
075500970530     C     ENTDFV        ORGT      0
075600041014     C     ENTDFV        ANDLE     prtDFV
075700961121     C                   MOVEL     ENTDFV        BLTDSE
075800961121     C                   MOVEL     ENTNPS        BLTNPE
075900041014   X3C                   other
076000961121     C*
076100961121     C* AGGIORNO SOLO CON L'ALTRA SPUNTA TROVATA O ENTRATA SE NO LOCALE
076200041014     C                   MOVEL     prtDFV        BLTDSE
076300041014     C                   MOVEL     prtNPS        BLTNPE
076400041014    3C                   ENDsl
076500060512   2aC                   ELSE
076600060512     c* 20 on - non aggiorno la data
076700060512     C                   SETON                                        20
076800060512   2aC                   ENDIF
076900961121     C**
077000961121   X2C                   ELSE
077100041014     c* 20 on - non aggiorno la data
077200930604     C                   SETON                                        20
077300961121    2C                   ENDIF
077400990409     C* DCD
077500990409    2C     FLG           IFEQ      '1'
077600990409     C     WDCD          ANDEQ     'S'
077700990409     C*
077800990409     C* SE NON TROVATA NESSUNA SPUNTA, NEMMENO ENTRATA PULISCO LE DATE
077900041014     C     danDFV        ifeq      0
078000990409     C                   MOVEL     *ZEROS        DCDDFV
078100990409     C                   MOVEL     *ZEROS        DCDFGS
078200990409     C                   MOVEL     *ZEROS        DCDNPS
078300990409     C                   CLEAR                   DCDFAR
078400990409   X3C                   ELSE
078500150819     C* Imposto la spunta con data più altra se>=alla data presente
078600041014     C     danDFV        ifge      dcddfv
078700041014     C                   MOVEL     danDFV        DCDDFV
078800041014     C                   MOVEL     danNPS        DCDNPS
078900041014     C                   MOVEL     danFGS        DCDFGS
079000041014     c                   else
079100041014     C                   MOVEL     *ZEROS        DCDDFV
079200041014     C                   MOVEL     *ZEROS        DCDFGS
079300041014     C                   MOVEL     *ZEROS        DCDNPS
079400041014     C                   CLEAR                   DCDFAR
079500041014   X4C                   endif
079600041014   X4C                   endif
079700101202     c*
079800990409     C                   UPDATE    FNDCD000
079900990409    2C                   ENDIF
080000921020     C*
080100970530     C                   UPDATE    FNBLT000
080200921019     C*
080300970530    2C     FLG           IFEQ      '1'
080400930604     C     *IN20         ANDEQ     '0'
080500930604     C*
080600031114     C     KBLT          CHAIN     FNBLP01L                           3339
080700150819     c* Visto che una bolla DPD eveva la data BLPDSE impostata ma non era
080800150819     c*  presente nel dettaglio colli, e visto che il pgm gestisce la eliminaz.
080900150819     c*  l'unico caso che mi viene in mente è che la bolla era allocata.
081000150819     c*  in sto caso scrivo FNAGB
081100150819     c                   if        *in39
081200150819     c                   clear                   fnagb000
081300150819     C                   MOVEL     'P'           AGBTBO
081400150819     C                   MOVEL     'DT'          AGBAGB
081500150819     C                   Z-ADD     blTAAS        AGBAAS
081600150819     C                   Z-ADD     blTLNP        AGBLNP
081700150819     C                   Z-ADD     blTNRS        AGBNRS
081800150819     C                   Z-ADD     blTNSP        AGBNSP
081900150819     C     KAGB          SETLL     FNAGB01L                               31
082000150819     C  N31              WRITE     FNAGB000
082100150819     c
082200150819     C                   GOTO      ENDUPD
082300150819     c                   endif
082400921019     C*
082500930604     C* CONTROLLO DETTAGLIO PER IMPOSTARE 1° DATA ENTRATA
082600001206     C  N20
082700001206     CANN33              EXSR      CTRBLT
082800921230     C*
082900941206     C  N33              UPDATE    FNBLP000
083000031203    2C                   END
083100031203   1aC                   END
083200921019     C*
083300970530    1C                   END
083400041216     c                   unlock    fnblt27l
083500031114     C**
083600031114     C* A G G I O R N O   B O L L E   A R R I V I
083700921013     C**
083800031114    1C     wagarr        IFEQ      'S'
083900941206     C                   SETOFF                                           20
084000941230     C     KBOL2         CHAIN     FNART27L                           3239
084100990409     C                   CLEAR                   WDCD
084200990409     C*
084300031114    2C     *IN39         IFEQ      *OFF
084400041014     c
084500060512     c* Solo se non ho già aggiornato da partenza
084600041015    3c                   if        wsploc=*blanks
084700990409     C     KBOL2         CHAIN     FNDCD02L                           3839
084800060710   3ac                   if        not *in38 and not *in39
084900060512     c                   movel     dcdnps        knps
085000060512     c                   exsr      TIPPIS
085100101202     C*
085200060512    4C     DCDNPS        IFEQ      NPS
085300990409     C     DCDDFV        ANDEQ     WVVDFV
085400060512     C     spurps        oreq      'M'
085500060512     C     §7jrps        andeq     spurps
085600990409     C                   MOVEL     'S'           WDCD              1
085700990409     C                   ELSE
085800990409     C                   UNLOCK    FNDCD02L
085900041015    4C                   ENDIF
086000060512   3aC                   ENDIF
086100041015    3C                   ENDIF
086200031114    2C                   ENDIF
086300920424     C*
086400921013     C   39              GOTO      ENDUPD
086500031203     c
086600041014     c* Solo se esiste la bolla
086700031203   1ac                   if        not *in32
086800990409     C* AGGIORNO FNDCD
086900990409    2C     FLG           IFEQ      '1'
087000990409     C     WDCD          ANDEQ     'S'
087100990409     C*
087200041014    4C                   IF        dandfv=0
087300990409     C                   CLEAR                   DCDFGS
087400990409     C                   CLEAR                   DCDNPS
087500990409     C                   CLEAR                   DCDDFV
087600990409     C                   CLEAR                   DCDFAR
087700990409   X4C                   ELSE
087800041014     C* ESISTE UN'ALTRA SPUNTA PER IL COLLO con data >= di quella annullata
087900041015    5c                   if        dandfv>=dcddfv
088000041014     C                   MOVEL     danDFV        DCDDFV
088100041014     C                   MOVEL     danNPS        DCDNPS
088200041015   x5C                   MOVEL     danFGS        DCDFGS
088300041014     c                   else
088400041014     C                   CLEAR                   DCDFGS
088500041014     C                   CLEAR                   DCDNPS
088600041014     C                   CLEAR                   DCDDFV
088700041014     C                   CLEAR                   DCDFAR
088800041015    5C                   ENDIF
088900041015    4C                   ENDIF
089000101202     c*
089100990409     C                   UPDATE    FNDCD000
089200990409    2C                   ENDIF
089300990409     C*
089400961129    2C     FLG           IFEQ      '1'
089500060512     c                   movel     artnap        knps
089600060512     c                   exsr      TIPPIS
089700101202     c*
089800060512   2aC     ARTNAP        ifeq      NPS
089900970530     C     ARTDAM        ANDEQ     WVVDFV
090000060512     c* Oppure se sono entrambe manuali
090100060512     c     spurps        oreq      'M'
090200060512     c     §7jrps        andeq     spurps
090300101202     c*
090400970604     C* MEMORIZZO LA DATA DI ARRIVO PRIMA DI MODIFICARLA
090500970604     C                   MOVE      ARTDAM        SAVDAM
090600970530     C**
090700960613     C* NON ESISTONO ALTRE SPUNTE PER IL COLLO
090800041014    3C     *IN16         IFEQ      *OFF
090900961129    4C     ARTDCM        IFGT      *ZEROS
091000960613     C                   MOVE      ARTDCM        ARTDAM
091100960613     C                   Z-ADD     85            ARTNAP
091200960613     C                   ELSE
091300960613     C                   Z-ADD     0             ARTDAM
091400960613     C                   Z-ADD     0             ARTNAP
091500050509     c                   clear                   artfl2
091600060512     c                   clear                   artcan
091700060512     C                   SETOff                                       06
091800961129    4C                   END
091900970530   X3C                   ELSE
092000960613     C* ESISTE UN'ALTRA SPUNTA PER IL COLLO
092100041014     C                   MOVEL     arrDFV        ARTDAM
092200041014     C                   MOVEL     arrNPS        ARTNAP
092300050509     C                   MOVEL     arrFL2        ARTFL2
092400970530    3C                   END
092500970530     C* SCRIVO LA STATISTICA PER RIELABORARE LA NUOVA DATA CHE HO
092600970604     C*  SCRITTO E LA DATA CHE C'ERA PRIMA
092700970604     C     ARTDAM        IFNE      SAVDAM
092800070424     c* Memorizzo che devo riaggiornare data arrivo ultimo collo
092900070424     c                   eval      AggioDTI='S'
093000970604     C* VECCHIA DATA
093100970604     C                   CLEAR                   SA2TLA
093200970604     C                   MOVEL     'A'           SA2TRC
093300970604     C                   Z-ADD     SAVDAM        SA2DTS
093400970604     C                   MOVE      *ZEROS        SA2LNA
093500970604     C                   CALL      'FNLSA2R3'
093600970604     C                   PARM                    SA2TLA            1
093700970604     C                   PARM                    SA2TRC            1
093800970604     C                   PARM                    SA2DTS            8 0
093900970604     C                   PARM                    SA2LNA            3 0
094000970604     C* NUOVA DATA
094100970604     C     ARTDAM        IFGT      *ZEROS
094200970604     C                   Z-ADD     ARTDAM        SA2DTS
094300970604     C                   CALL      'FNLSA2R3'
094400970604     C                   PARM                    SA2TLA            1
094500970604     C                   PARM                    SA2TRC            1
094600970604     C                   PARM                    SA2DTS            8 0
094700970604     C                   PARM                    SA2LNA            3 0
094800970604     C                   END
094900970604     C                   END
095000970625     C**
095100970625     C* LA SPUNTA ANNULLATA NON E' QUELLA CHE HA AGGIORNATO IL DETTAGL
095200060512   2aC                   ELSE
095300930604     C                   SETON                                        20
095400060512   2aC                   ENDIF
095500060512     C**
095600060512     C* LA SPUNTA ANNULLATA NON E' QUELLA CHE HA AGGIORNATO IL DETTAGL
095700060512   X2C                   ELSE
095800060512     C                   SETON                                        20
095900060512    2C                   ENDIF
096000970625     C*
096100961129    2C     SAVCAN        IFEQ      ' '
096200921020     C     ARTCAN        ANDEQ     CAN
096300921020     C                   MOVEL     ' '           ARTCAN
096400921019     C                   ELSE
096500921019     C                   SETON                                        06
096600961129    2C                   END
096700950118     C*
096800960417     C*  MEMORIZZAZIONI PER ANOMALIE
096900961129    2C     FLG           IFEQ      '1'
097000960416     C                   MOVE      ARTAAS        WAAS
097100960416     C                   MOVE      ARTLNP        WLNP
097200960416     C                   MOVE      ARTNSP        WNSP
097300960417     C                   MOVE      ARTDAM        WDAM
097400070424     C                   MOVE      ARTdet        WDET
097500070424     C                   MOVE      ARTFLP        WFLP
097600960417     C                   MOVE      ARTDCM        WDCM
097700961129    2C                   END
097800921020     C*
097900961129     C                   MOVEL     ARTDTR        WDTR
098000941206     C                   UPDATE    FNART000
098100921019     C*
098200911016     C* AGGANCIO BOLLA
098300930604     C*
098400941206     C     KARB          CHAIN     FNARB01L                           3339
098500920424     C* 39 ON - RECORD ALLOCATO
098600970625    2C     *IN39         IFEQ      *OFF
098700030115      * Mi salvo arbdam
098800030115     c                   Eval      Sav_ArbDam = ArbDam
098900970625     C**
099000921019     C* CONTROLLO TUTTO IL DETTAGLIO
099100970625    3C     *IN20         IFEQ      *OFF
099200970625     C     *IN06         OREQ      *OFF
099300970625     C*
099400921019     C  N33              EXSR      CTRART
099500970625     C* 06 OFF - NESSUN CODICE ANOMALIA: ELIMINO ANCHE IN TESTATA
099600970625     C  N06              MOVEL     ' '           ARBFAN
099700970625     C* FLAG ABILITAZIONE MERCE:
099800970625    4C  N20FLG           IFEQ      '1'
099900970625     C     ARBAMA        ANDNE     3
100000101202     c*
100100060512     c                   select
100200970625     C* ABILITATO IN BASE ALLA SPUNTA ARRIVO
100300060512    5C     NOAMA         wheneq    ' '
100400970625     C                   MOVEL     2             ARBAMA
100500970625     C                   MOVEL     1             ARBAMP
100600970625     C* ABILITATO IN BASE ALLA SPUNTA PARTENZA(ABBINATO)
100700060512    5C     NOAMP         wheneq    ' '
100800970625     C                   MOVEL     1             ARBAMA
100900970625     C                   MOVEL     1             ARBAMP
101000060512    5C     NOAMP         wheneq    '0'
101100060512     C                   MOVEL     0             ARBAMA
101200060512     C                   MOVEL     1             ARBAMP
101300060512   X5C                   other
101400970625     C* NON ABILITATO
101500970625     C                   MOVEL     0             ARBAMA
101600970625     C                   MOVEL     0             ARBAMP
101700060512    5C                   ENDsl
101800970625    4C                   END
101900970625    3C                   ENDIF
102000961120     C*
102100961120     C* MEMORIZZO LA DATA BORDERO' CHE MI SERVIRA' PER BOLLA LOCALE
102200961120     C*  SE DEVO CREARE L'ANOMALIA 5
102300961120     C                   Z-ADD     ARBDBR        WDBR
102400961120     C                   MOVEL     ARBFBC        WFBC
102500970127     C* TESTO SE SPEDIZIONE GIACENTE PER ANOMALIA 15
102600970625    3C     WFBC          IFEQ      'G'
102700050401     C     KGCA          CHAIN     Tigcp21L                           38
102800970721     C* AI FINI DELL'IMA DEVO TESTARE LA DATA DISPOSIZONI MITTENTE
102900970721    5C  N38GCPFAS        IFLT      35
103000050401    5C     GCPFAS        oreq      36
103100970721     C                   MOVEL     'S'           WGIAC             1
103200970625    3C                   ENDIF
103300970721    3C                   ENDIF
103400070424     c*
103500150819    3c                   if        Aggiodti='S'
103600070424     c                   clear                   arbdti
103700070424     c                   clear                   arbhti
103800070424     C     ARBFBS        IFEQ      ' '
103900070424     C                   MOVEL     'S'           ARBFBS
104000070424     C                   ELSE
104100070424     C     ARBFBS        IFEQ      'V'
104200070424     C     ARBFBS        oreq      'P'
104300070424     C                   MOVEL     'E'           ARBFBS
104400070424     C                   ENDIF
104500070424     C                   ENDIF
104600101202     c*
104700150819    3C                   ENDIF
104800921019     C*
104900941206     C  N33              UPDATE    FNARB000
105000101202      *
105100030320      * Se è un tipo bolla da trasmettere in sede
105200030320     c                   Clear                   ds3a
105300030320     c                   Eval      Cod = '3A'
105400030320     c                   Clear                   Key
105500030320     c                   Movel     ArbCbo        Key
105600030320     c     kTab          Chain     Tabel00f
105700030320     c                   If        %Found(Tabel00f)
105800030320     c                   Movel     TblUni        ds3a
105900030320     c                   EndIf
106000030320If  2c                   If        §3aBsd = *Blanks
106100030115      * Scrivo/Aggiorno Fidta se data variata
106200030115     c                   If        Not *In33 and Sav_ArbDam <> ArbDam
106300030115     c                   Eval      wFidta = '0'
106400030115     c                   Eval      SavAas = ArbAas
106500030115     c                   Eval      SavLnp1 = ArbLnp
106600030115     c                   Eval      SavNrs = ArbNrs
106700030115     c                   Eval      SavNsp = ArbNsp
106800030115     c                   Eval      SavPoc = ArbLna
106900030115     c                   Eval      SavTrd = 'DAM'
107000030115     c     kFidta        Setll     Fidta01l
107100030115     c                   Do        *Hival
107200030115     c     kFidta        Reade     Fidta01l
107300030115      * Fine file
107400030115     c                   If        %Eof(Fidta01l)
107500030115     c                   Leave
107600030115     c                   EndIf
107700030115      * Già trasmesso
107800030115     c                   If        DtaFtr <> *Blanks
107900030115     c                   Iter
108000030115     c                   EndIf
108100030115      * Aggiorno
108200030115     c                   Eval      DtaDta = ArbDam
108300030115     c                   Update    Fidta000
108400030115     c                   Eval      wFidta = '1'
108500030115     c                   EndDo
108600030115      * Scrivo
108700030115     c                   If        wFidta = '0'
108800030115     c                   Clear                   Fidta000
108900030115     c                   Eval      DtaAas = ArbAas
109000030115     c                   Eval      DtaLnp = ArbLnp
109100030115     c                   Eval      DtaNrs = ArbNrs
109200030115     c                   Eval      DtaNsp = ArbNsp
109300030115     c                   Eval      DtaTrd = SavTrd
109400030115     c                   Eval      DtaPoc = SavPoc
109500030115     c                   Eval      DtaDta = ArbDam
109600030115     c                   Write     Fidta000
109700030115     c                   EndIf
109800030320    2c                   EndIf
109900030115     c                   EndIf
110000101202      *
110100970625   X2C                   ELSE
110200960614     C* BOLLA ALLOCATA:
110300960614     C* SE NON E' L'ULTIMO TENTATIVO VADO A FINE ROUTINE
110400970625    3C     B             IFLT      3
110500960614     C                   GOTO      ENDUPD
110600970625    3C                   END
110700960614     C* SE E' L'ULTIMO TENTATIVO SCRIVO FNAGB
110800960614     C                   MOVEL     'A'           AGBTBO
110900960614     C                   MOVEL     'DT'          AGBAGB
111000960614     C                   Z-ADD     ARTAAS        AGBAAS
111100960614     C                   Z-ADD     ARTLNP        AGBLNP
111200960614     C                   Z-ADD     ARTNRS        AGBNRS
111300960614     C                   Z-ADD     ARTNSP        AGBNSP
111400960614     C                   MOVEL     'S'           AGBFBS
111500960614     C                   CLEAR                   AGBFVC
111600041014     C                   CLEAR                   AGBFpC
111700960614     C     KAGB          SETLL     FNAGB01L                               31
111800960614     C  N31              WRITE     FNAGB000
111900970625    2C                   END
112000031203   1aC                   END
112100921013     C*
112200031114    1C                   END
112300041216     c                   unlock    fnart27l
112400041216     c                   unlock    fnarb01l
112500031114     C**
112600031114     C** T R A N S I T I  /  D I S G U I D I   I N   A R E A   E  N O N
112700031114     C**
112800031114    1C     WESBTT        IFEQ      'S'
112900911112     C*
113000031114     C     KBAR37        CHAIN     FNBTT37L                           3739
113100920424     C* 39 ON  - RECORD ALLOCATO
113200920424     C   39              GOTO      ENDUPD
113300911016     C*
113400921019     C* 37 OFF - TROVATA BOLLA
113500941230    2C  N37              DO
113600941230     C*
113700941230    3C     SAVCAN        IFEQ      ' '
113800941206     C     BTTCAN        ANDEQ     CAN
113900941206     C                   MOVEL     ' '           BTTCAN
114000941230    3C                   END
114100921230     C*
114200941230    3C     FLG           IFEQ      '1'
114300060512     c                   movel     bttpet        knps
114400060512     c                   exsr      TIPPIS
114500101202     c*
114600060512   3aC     BTTPET        ifeq      NPS
114700970530     C     BTTDET        ANDEQ     WVVDFV
114800060512     c* Oppure se sono entrambe manuali
114900060512     c     spurps        oreq      'M'
115000060512     c     §7jrps        andeq     spurps
115100101202     c*
115200970604     C                   MOVE      BTTDET        SAVDET
115300041014     C  N18              MOVEL     *ZEROS        BTTDET
115400041014     C  N18              MOVEL     *ZEROS        BTTPET
115500060512     C  N18              clear                   BTTcan
115600041014     C   18              MOVEL     traDFV        BTTDET
115700041014     C   18              MOVEL     traNPS        BTTPET
115800970625     C*
115900970604     C     BTTDET        IFNE      SAVDET
116000970604     C                   CLEAR                   SA2TLA
116100970604     C                   MOVEL     'A'           SA2TRC
116200970604     C                   Z-ADD     SAVDET        SA2DTS
116300970604     C                   MOVE      *ZEROS        SA2LNA
116400970604     C                   CALL      'FNLSA2R3'
116500970604     C                   PARM                    SA2TLA            1
116600970604     C                   PARM                    SA2TRC            1
116700970604     C                   PARM                    SA2DTS            8 0
116800970604     C                   PARM                    SA2LNA            3 0
116900970604     C* NUOVA DATA
117000970604     C     BTTDET        IFGT      *ZEROS
117100970604     C                   Z-ADD     BTTDET        SA2DTS
117200970604     C                   CALL      'FNLSA2R3'
117300970604     C                   PARM                    SA2TLA            1
117400970604     C                   PARM                    SA2TRC            1
117500970604     C                   PARM                    SA2DTS            8 0
117600970604     C                   PARM                    SA2LNA            3 0
117700970604     C                   END
117800970604     C                   END
117900970625     C*
118000970625     C* NON SONO STATE MODIFICATE LE DATE SUL DETTAGLIO COLLI
118100060512   3aC                   ELSE
118200930604     C                   SETON                                        20
118300060512   3AC                   END
118400060512     C*
118500060512     C* NON SONO STATE MODIFICATE LE DATE SUL DETTAGLIO COLLI
118600060512   X3C                   ELSE
118700060512     C                   SETON                                        20
118800060512    3C                   END
118900960416     C*  MEMORIZZO KEY SPEDIZIONE PER ANOMALIE
119000960416     C     FLG           IFEQ      '1'
119100960416     C                   MOVE      BTTAAS        WAAS
119200960416     C                   MOVE      BTTLNP        WLNP
119300960416     C                   MOVE      BTTNSP        WNSP
119400960416     C                   END
119500921020     C*
119600941206     C                   UPDATE    FNBTT000
119700921020     C*
119800911016     C* AGGANCIO BOLLA
119900941230    3C     FLG           IFEQ      '1'
120000930604     C     *IN20         ANDEQ     '0'
120100031114     C     KBTT          CHAIN     FNBTP11L                           3339
120200920424     C* 39 ON  - RECORD ALLOCATO
120300920424     C   39              GOTO      ENDUPD
120400030115      * Mi salvo btpdet (btpdut non viene modificato)
120500030115     c                   Eval      Sav_BtpDet = BtpDet
120600930604     C*
120700921020     C  N33              EXSR      CTRBTT
120800921019     C*
120900941206     C  N33              UPDATE    FNBTP000
121000030115      * Scrivo/Aggiorno Fidta se la data è variata
121100030115     c                   If        Not *In33 and Sav_BtpDet <> BtpDet
121200030115     c                   Eval      wFidta = '0'
121300030115     c                   Eval      SavAas = BtpAas
121400030115     c                   Eval      SavLnp1 = BtpLnp
121500030115     c                   Eval      SavNrs = BtpNrs
121600030115     c                   Eval      SavNsp = BtpNsp
121700030115     c                   Eval      SavPoc = BtpFlp
121800030115     c                   Eval      SavTrd = 'DET'
121900030115     c     kFidta        Setll     Fidta01l
122000030115     c                   Do        *Hival
122100030115     c     kFidta        Reade     Fidta01l
122200030115      * Fine file
122300030115     c                   If        %Eof(Fidta01l)
122400030115     c                   Leave
122500030115     c                   EndIf
122600030115      * Già trasmesso
122700030115     c                   If        DtaFtr <> *Blanks
122800030115     c                   Iter
122900030115     c                   EndIf
123000030115      * Aggiorno
123100030115     c                   Eval      DtaDta = BtpDet
123200030115     c                   Update    Fidta000
123300030115     c                   Eval      wFidta = '1'
123400030115     c                   EndDo
123500030115      * Scrivo
123600030115     c                   If        wFidta = '0'
123700030115     c                   Clear                   Fidta000
123800030115     c                   Eval      DtaAas = BtpAas
123900030115     c                   Eval      DtaLnp = BtpLnp
124000030115     c                   Eval      DtaNrs = BtpNrs
124100030115     c                   Eval      DtaNsp = BtpNsp
124200030115     c                   Eval      DtaTrd = SavTrd
124300030115     c                   Eval      DtaPoc = SavPoc
124400030115     c                   Eval      DtaDta = BtpDet
124500030115     c                   Write     Fidta000
124600030115     c                   EndIf
124700030115     c                   EndIf
124800101202      *
124900941230    3C                   END
125000921014     C*
125100941230    2C                   END
125200941230    1C                   END
125300041216     c                   unlock    fnbtt37l
125400041216     c                   unlock    fnbtp11l
125500101202     c*
125600041013     c* Aggiornamento anche di FNART per spunta di transito/disguido
125700041013     c*  cancellata
125800041013     c                   if        (lnatfa<>bpes and lnatfa<>pestfa  and
125900041013     c                             wagpar=' ') or wesbtt='S'
126000041013     C     KBOL2         CHAIN     FNART27L
126100041013     c                   if        %found(fnart27l)
126200101202     c*
126300041013    3C     SAVCAN        IFEQ      ' '
126400041013     C     arTCAN        ANDEQ     CAN
126500041013     C                   MOVEL     ' '           arTCAN
126600041013    3C                   END
126700041013     C*
126800041013    3C     FLG           IFEQ      '1'
126900060512     c                   movel     artpet        knps
127000060512     c                   exsr      TIPPIS
127100101202     c*
127200060512   3aC     artPET        ifeq      NPS
127300041013     C     artDET        ANDEQ     WVVDFV
127400060512     c* Oppure se sono entrambe manuali
127500060512     c     spurps        oreq      'M'
127600060512     c     §7jrps        andeq     spurps
127700101202     c*
127800041015     C   18              MOVEL     traDFV        artDET
127900041015     C   18              MOVEL     traNPS        artPET
128000041014     C  N18              MOVEL     *ZEROS        artDET
128100041014     C  N18              MOVEL     *ZEROS        artPET
128200060512     C  N18              CLEAR                   artCAN
128300041014    4c  n18              if        wesbtt<>'S'
128400041014     C                   MOVEL     *ZEROS        artflp
128500041014     c* Verifico le spunte partenza per impostare eventuale altro p.o. di
128600041014     c*  transito/disguido
128700041014     c* Se p.o. ultima spunta partenza= p.o. ultima spunta no part.
128800041014     c* imposto questa
128900041014    5c                   select
129000041014     c                   when      dis1fgs=dis2fgs
129100041014     c                   z-add     dis2dfv       artdet
129200041014     c                   z-add     dis2nps       artpet
129300041014     c                   z-add     dis2fgs       artflp
129400041014     c                   z-add     dis1dfv       artdut
129500041014     c                   z-add     dis1nps       artput
129600101202     c*
129700041014     c* Elaboro spunta partenza se con data > o data/ora caricamento>
129800041014     c                   when      dis1dfv=dis2dfv and dis1scar>dis2scar or
129900041014     c                             dis1dfv>dis2dfv
130000041014     c* Se si tratta di uscita transito aggiorno subito altrimenti leggo
130100041014     c* le linee per vedere se c'e' transito o meno
130200041014    6c                   if        dis1atr='T' or dis1atr='D'
130300041014     c                   movel     dis1fgs       artflp
130400041014     c                   movel     dis1dfv       artdet
130500041014     c                   movel     dis1dfv       artdut
130600041014     c                   movel     dis1nps       artpet
130700041014     c                   movel     dis1nps       artput
130800041014   x6c                   else
130900041014     c* E si tratta della partenza o si tratta di un disguido che
131000041014     c* transita di nuovo per cui aggiorno solo flp
131100041014    7c                   if        dis1fgs<>lnptfp
131200041014     c                   movel     dis1fgs       artflp
131300041014   x7c                   else
131400041014     c* chain fnfgv
131500041014     c     kfgv          chain     fnfgv01l
131600041014    8c                   if        %found(fnfgv01l) and fgvlna<>lnatfa
131700041015     c     kfgv          chain     fnfgw01l
131800041015     c                   if        not %found(fnfgw01l)
131900041015     c                   clear                   fgwff3
132000041015     c                   clear                   fgwff4
132100041015     c                   clear                   fgwfl3
132200041015     c                   clear                   fgwfl4
132300041015     c                   endif
132400101202     c*
132500041015     c                   z-add     1             xx                3 0
132600041015     c                   movel     lna           wlna              3
132700041015     c                   movel     lnatfa        wlnatfa           3
132800041015     c     wlna          lookup    ffv(xx)                                31
132900041015    9c                   if        *in31 and flp(xx)<>wlnatfa
133000041014     c                   movel     flp(xx)       artflp
133100041014    9c                   endif
133200041014    8c                   endif
133300041014    7c                   endif
133400041014    6c                   endif
133500041014   x5c                   other
133600041014     c* Altrimenti elaboro spunta arrivi
133700041014     c                   movel     dis2fgs       artflp
133800041014     c                   movel     dis2dfv       artdet
133900041014     c                   movel     dis2nps       artpet
134000041014     c                   clear                   artdut
134100041014     c                   clear                   artput
134200041014    5c                   endsl
134300101202     c*
134400041014    4c                   endif
134500060512   3ac                   endif
134600060512    3c                   endif
134700101202     c*
134800041014     c                   update    fnart000
134900041014     c                   clear                   fnagb000
135000041014     C                   MOVEL     'A'           AGBTBO
135100041014     C                   MOVEL     'DT'          AGBAGB
135200041014     C                   Z-ADD     ARTAAS        AGBAAS
135300041014     C                   Z-ADD     ARTLNP        AGBLNP
135400041014     C                   Z-ADD     ARTNRS        AGBNRS
135500041014     C                   Z-ADD     ARTNSP        AGBNSP
135600041014     C     KAGB          SETLL     FNAGB01L                               31
135700041014     C  N31              WRITE     FNAGB000
135800041013     c                   endif
135900041014     c                   endif
136000921019     C**
136100921019     C** CHIUDO ANOMALIE
136200921019     C**
136300960416    1C     FLG           IFEQ      '1'
136400960416     C*
136500960416     C                   CLEAR                   W005
136600960416     C                   CLEAR                   W015
136700070424     c                   clear                   wcrea200          1
136800921021     C*
136900990518     C     KBOL2         SETLL     FNANM000
137000990518     C     KBOL2         READE     FNANM000                               32
137100921019     C*
137200961129    2C     *IN32         DOWEQ     *OFF
137300060512     c* Se anomalia 15 e non esiste più la data di arrivo del collo
137400060512     c*  devo cancellare l'anomalia
137500060512    3c                   if        anmcaa=015 and wdam=0  and wagarr='S'
137600060512    4c                   if        anmdch>0
137700060828     c                   unlock    fnanm02l
137800060512     c* Se già in idd3 la chiudo come annullata
137900060512     C                   MOVEL     FASE          D33FSC
138000060512     C                   MOVEL     'AN'          D33CCH
138100060828     C                   MOVEL     dateu         D33dCH
138200060512     C                   MOVE      ANMNR2        D33NRR
138300060512     C                   MOVEL     'S'           D33MAC
138400060512     C                   CALL      'FNLR33R'
138500060512     C                   PARM                    DSLR33
138600101202     c*
138700060512     C     KBOL2         READE     FNANM000                               32
138800060512     c                   iter
138900060512     c                   else
139000060512     c* la deleto
139100060512     c                   delete    fnanm000
139200101202     c*
139300060512     C     KBOL2         READE     FNANM000                               32
139400060512     c                   iter
139500060512    4c                   endif
139600060512    3c                   endif
139700101202     c*
139800070424     c* Se esiste anomalia 55 e artdam=0, vedo se ricreare la 200
139900130301    3c                   if        (anmcaa=57 or
140000130301    3c                              anmcaa=55 or anmcaa=56) and wdam=0
140100070424     c                             and anmdao=wdet and anmfle=wflp
140200070424     c                   eval      wcrea200='S'
140300070424     c                   eval      savrecanm=dsrecanm
140400070424    3c                   endif
140500101202     c*
140600070424     c* Se trovo già la 200 chiusa, la devo solo riaprire
140700070424     c                   if        wcrea200='S' and anmcaa=200 and
140800070424     c                             anmfle=wflp
140900070424     C                   Z-ADD     0             ANMDCH
141000070424     C                   MOVEL     ' '           ANMFSC
141100070424     C                   MOVEL     '  '          ANMCCH
141200070424     C                   CLEAR                   ANMFT1
141300070424     C                   CLEAR                   ANMFT2
141400070424     C                   UPDATE    FNANM000
141500101202     c*
141600070424     c* N= non creare la 200 perchè già trovata
141700070424     c                   eval      wcrea200='N'
141800110908     C     KBOL2         READE     FNANM000                               32
141900070424     c                   iter
142000070424     c                   endif
142100101202     c*
142200110908     c* Anomlia 115 --> se non esiste nessun'altra spunta sul sistema reale
142300110908     c*                 riapro l'anomalia anche non mia
142400110908    3c                   if        anmcaa=115
142500110908    4c                   if        a_115dfv=0
142600110908     C                   Z-ADD     0             ANMDCH
142700110908     C                   MOVEL     ' '           ANMFSC
142800110908     C                   MOVEL     '  '          ANMCCH
142900110908     C                   CLEAR                   ANMFT1
143000110908     C                   CLEAR                   ANMFT2
143100110908   x4c                   else
143200110908     c* altrimenti se è stata con questa spunta--> la riaggiorno
143300110908    5C     ANMDCH        IFNE      0
143400110908     C     ANMFSC        ANDEQ     FASE
143500110908     C     ANMdch        ANDEQ     wvvdfv
143600110908     C                   CLEAR                   DSFASE
143700110908     C                   MOVE      a_115NPG      D11NPG
143800110908     C                   MOVE      a_115SPG      D11SPG
143900110908     C                   CALL      'TRUL11R'
144000110908     C                   PARM                    DSFASE
144100110908     C                   MOVEL     D11FAS        ANMFSC
144200110908     C                   MOVEL     a_115dfv      ANMdch
144300110908    5c                   endif
144400110908    4c                   endif
144500110908     C                   UPDATE    FNANM000
144600110908     c
144700110908     C     KBOL2         READE     FNANM000                               32
144800110908     c                   iter
144900110908    3c                   endif
145000060512     C*
145100041018     c* Leggo solo le anomalie create da me
145200120109    3c*****              if        anmfle=bpes or anmfle=parfgs
145300921019     C*
145400921019     C* RIAPRO SE CHIUSE E NESSUN'ALTRA SPUNTA
145500980205     C* E SE LA SPEDIZIONE NON E' CONSEGNATA
145600120109    3C                   if        anmcaa=005
145700120109    4c                   if        anmfle=bpes or anmfle=parfgs
145800120109     c                             or  anmfle=lnatfa
145900041018    5c     wagarr        ifeq      'S'
146000961129     C                   MOVE      '1'           W005              1
146100921019     C*
146200041018    6C  N19WDCM          IFEQ      0
146300980205     C     WDAM          ANDEQ     0
146400980205     C**
146500041018    7C     ANMDCH        IFNE      0
146600041018    8C     ANMLID        IFGE      §7ALCI
146700960510     C                   MOVE      'PR'          ANMCCH
146800960510     C                   MOVE      'D'           ANMFSC
146900970410     C                   CLEAR                   ANMFT1
147000970410     C                   CLEAR                   ANMFT2
147100960510     C                   ELSE
147200921019     C                   Z-ADD     0             ANMDCH
147300921019     C                   MOVEL     ' '           ANMFSC
147400921019     C                   MOVEL     '  '          ANMCCH
147500970410     C                   CLEAR                   ANMFT1
147600970410     C                   CLEAR                   ANMFT2
147700041018    8C                   END
147800990518     C                   UPDATE    FNANM000
147900041018    7C                   END
148000041018    6C                   END
148100041018    5C                   END
148200120109    4C                   END
148300921021     C*
148400120109   x3c                   else
148500120109     c
148600120109    4c                   if        anmfle=bpes or anmfle=parfgs
148700101202     c*
148800921019     C* PER TUTTE LE ALTRE ANOMALIE CONTROLLO FASE APERTURA E FOGLIO
148900041018    5C     ANMNFV        IFEQ      PARNFV
149000921021     C     ANMFSA        ANDEQ     FASE
149100960513     C*
149200960417     C     ANMCAA        OREQ      050
149300960417     C     *IN19         ANDEQ     *OFF
149400041018     c     anmft1        andeq     *blanks
149500101202     c*
149600041018     c* chain in DS/C per controllo se autochiusa
149700041018     C     anmcaa        IFNE      SAVCAA
149800041018     C                   MOVEL     '7C'          COD
149900041018     C                   MOVEL(P)  ANMCAA        KEY
150000041018     C     KTAB          CHAIN     TABEL                              31
150100041018     C  N31              MOVEL     TBLUNI        DS7C
150200041018     C   31              MOVEL     *BLANKS       DS7C
150300041018     C                   MOVEL     anmcaa        SAVCAA
150400041018     C                   ENDIF
150500960513     C*
150600041018     C* PER alcune anomalie,
150700041018     C*  SE ESISTE UN'ALTRA SPUNTA PER IL COLLO MODIFICO
150800041018     C*  L'ANOMALIA IN BASE ALLA SPUNTA RIMASTA
150900041018    6c                   if        *in19 and (anmcaa=10 or anmcaa=160
151000041018     c                             or anmcaa=200 or anmcaa=60 or
151100041018     c                             anmcaa=61)
151200960417     C                   CLEAR                   DSFASE
151300960417     C                   MOVE      SPUNPG        D11NPG
151400960417     C                   MOVE      SPUSPG        D11SPG
151500960417     C                   CALL      'TRUL11R'
151600960417     C                   PARM                    DSFASE
151700960417     C                   MOVEL     D11FAS        ANMFSA
151800960513     C                   MOVE      SPUFGS        ANMCDU
151900041018     C                   Z-ADD     SPUNFV        ANMNFV
152000041018     C* NELL'ANOMALIA 10 NON MODIFICO LA DATA APERTURA PER NON FAR DIVE
152100041018     C*  NTARE INCONGRUENTE IL LIVELLO IDD CON LA DATA CREAZIoNE ANOM
152200041018    7c                   if        anmcaa<>10
152300041018     C                   Z-ADD     SPUDFV        ANMDAO
152400041018    7c                   endif
152500041018     C                   MOVE      SPUFGS        ANMFLE
152600041018     C                   MOVE      SPUNPS        ANMNPS
152700041018     c* se anomalia autochiusam, imposto anche i dati di chiusura
152800041018     c                   if        §7ccha='S'
152900041018     C                   MOVE      spudfv        ANMDCH
153000041018     C                   MOVEL     d11fas        ANMFSC
153100041018     c                   endif
153200970603     C*
153300041018    7C     ANMAIE        IFEQ      'E'
153400970603     C                   CLEAR                   ANMFT1
153500970603     C                   CLEAR                   ANMFT2
153600041018    7C                   ENDIF
153700990518     C                   UPDATE    FNANM000
153800041018   X6C                   ELSE
153900990412     C*
154000990412     C* SE SI TRATTA DI ANOMALIA 56 CON ALTRE SPUNTE (19 ON):
154100041018     C*  modifico anche se trasmessa essendo su As unico, non
154200041018     C*  modifico però la data visto che ha aggiornato bltdet
154300041018    7C                   IF        *in19 and (anmcaa=55 or anmcaa=56)
154400101202     C*
154500041018    8C                   if        ANMCAA=56 AND A56DFV>0
154600041018     c                   if        anmft1=' '
154700990412     C                   Z-ADD     A56DFV        ANMDAO
154800990412     C                   Z-ADD     A56DFV        ANMDCH
154900041018     c                   endif
155000990412     C                   CLEAR                   DSFASE
155100990412     C                   MOVE      A56NPG        D11NPG
155200990412     C                   MOVE      A56SPG        D11SPG
155300990412     C                   CALL      'TRUL11R'
155400990412     C                   PARM                    DSFASE
155500990412     C                   MOVEL     D11FAS        ANMFSA
155600990412     C                   MOVEL     D11FAS        ANMFSC
155700990412     C                   MOVE      A56FGS        ANMCDU
155800990412     C                   MOVE      A56FGS        ANMFLE
155900990412     C                   MOVE      A56NPS        ANMNPS
156000990518     C                   Z-ADD     A56NFV        ANMNFV
156100990518     C                   UPDATE    FNANM000
156200041018   X8C                   else
156300990412     C* LA TRASFORMO IN 55
156400990412     C                   Z-ADD     55            ANMCAA
156500041018    9c                   if        anmft1=' '
156600990412     C                   Z-ADD     SPUDFV        ANMDAO
156700990412     C                   Z-ADD     SPUDFV        ANMDCH
156800041018    9c                   endif
156900990412     C                   CLEAR                   DSFASE
157000990412     C                   MOVE      SPUNPG        D11NPG
157100990412     C                   MOVE      SPUSPG        D11SPG
157200990412     C                   CALL      'TRUL11R'
157300990412     C                   PARM                    DSFASE
157400990412     C                   MOVEL     D11FAS        ANMFSA
157500990412     C                   MOVEL     D11FAS        ANMFSC
157600990412     C                   MOVE      SPUFGS        ANMCDU
157700990412     C                   MOVE      SPUFGS        ANMFLE
157800990412     C                   MOVE      SPUNPS        ANMNPS
157900990518     C                   Z-ADD     SPUNFV        ANMNFV
158000990518     C                   UPDATE    FNANM000
158100041018    8C                   ENDif
158200990412   X7C                   ELSE
158300960416     C* Richiamo pgm di chiusura anomalia
158400990518     C                   UNLOCK    FNANM02L
158500960416     C                   CLEAR                   DSLR33
158600041018    8C     DATEU         IFGE      WVVDFV
158700970117     C                   MOVEL     DATEU         D33DCH
158800970117     C                   ELSE
158900041018    8C                   MOVEL     WVVDFV        D33DCH
159000970117     C                   ENDIF
159100960416     C                   MOVEL     FASE          D33FSC
159200960416     C                   MOVEL     'AN'          D33CCH
159300960416     C                   MOVE      ANMNR2        D33NRR
159400960510     C                   MOVEL     'S'           D33MAC
159500960416     C                   CALL      'FNLR33R'
159600960416     C                   PARM                    DSLR33
159700990412    7C                   ENDIF
159800041018    6C                   END
159900921021     C*
160000041018   X5C                   ELSE
160100041018    6C     ANMCAA        IFNE      50
160200041018    6C     ANMCAA        andne     56
160300041018    6C     ANMCAA        andne     55
160400130301    6C     ANMCAA        andne     57
160500921021     C* CONTROLLO SE E' STATA CHIUSA IN QUESTA FASE
160600041018    7C     ANMDCH        IFNE      0
160700921021     C     ANMFSC        ANDEQ     FASE
160800041018     C     ANMdch        ANDEQ     wvvdfv
160900041018     c* Per l'anomalia 15 controllo se esiste altra spunta con data >= a
161000041018     c*  quella annullata nel qual caso la chiudo con dati diversi
161100041018     c                   clear                   wokanm15
161200041018    9C                   if        anmcaa=015
161300041018     C                   MOVE      '1'           W015              1
161400041018    0c                   if        dandfv>wvvdfv  or
161500041018     c                             (dannpg<>2 and dandfv>wvvdfv)
161600110908     C                   CLEAR                   DSFASE
161700110908     C                   MOVE      danNPG        D11NPG
161800110908     C                   MOVE      danSPG        D11SPG
161900110908     C                   CALL      'TRUL11R'
162000110908     C                   PARM                    DSFASE
162100110908     C                   MOVEL     D11FAS        ANMFSC
162200110908     C                   MOVEL     dandfv        ANMdch
162300041018     c                   eval      wokanm15='S'
162400041018    9c                   endif
162500041018    8c                   endif
162600041018     c*
162700041018    8c                   if        wokanm15=' '
162800041018    9c     ANMLID        IFGE      §7ALCI
162900960510     C                   MOVE      'PR'          ANMCCH
163000960510     C                   MOVE      'D'           ANMFSC
163100960510     C                   ELSE
163200921021     C                   Z-ADD     0             ANMDCH
163300921021     C                   MOVEL     ' '           ANMFSC
163400921021     C                   MOVEL     '  '          ANMCCH
163500041018    9C                   END
163600961025     C                   MOVE      *BLANKS       ANMFT1
163700990518     C                   UPDATE    FNANM000
163800041018    8C                   END
163900041018    7C                   END
164000041018    6C                   END
164100041018    5C                   END
164200921021     C*
164300120109    4C                   ENDIF
164400120109    3C                   END
164500921019     C*
164600990518     C     KBOL2         READE     FNANM000                               32
164700960416    2C                   ENDDO
164800070424     c*
164900070424     c* Verifico se devo creare  l'anomalia 200
165000070424     c                   if        wcrea200='S'
165100070424     c                   eval      dsrecanm=savrecanm
165200070424     C                   Z-ADD     200           COMCAA
165300070424     c                   exsr      CRTANM
165400070424     c                   eval      wcrea200='N'
165500070424     c                   endif
165600960416     C*
165700960416     C* CREO ANOMALIA 005 A FRONTE DI CANCELLAZIONE SPUNTA
165800960416     C     W005          IFEQ      *BLANKS
165900031114     c     wagarr        andeq     'S'
166000960416     C     *IN19         ANDEQ     *OFF
166100960416     C     WCHIUS        ANDEQ     'S'
166200960530     C     WDAM          ANDEQ     *ZEROS
166300960530     C     WDCM          ANDEQ     *ZEROS
166400000904     C*
166500031114     c* creo sempre anche per 2 liv in arrivo su altro AS in quanto
166600031114     c*  se poi dal terminal riceco la spunta l'anomalia viene cancell
166700031114     c*  se riricevo l'anomalia, dovrebbe andare in update
166800031114     c* (prioma invece creava soltanto se terarr di lna in £1)
166900000904     C                   EXSR      CRTA05
167000000904     C                   ENDIF
167100960530     C**
167200960416     C* CREO ANOMALIA 015 A FRONTE DI CANCELLAZIONE SPUNTA
167300960416     C     W015          IFEQ      *BLANKS
167400031114     C     wagarr        andeq     'S'
167500960416     C* Collo non consegnato
167600960416     C     WDCM          ANDEQ     *ZEROS
167700960417     C* Collo arrivato
167800960417     C     WDAM          ANDGT     *ZEROS
167900960416     C* Data arrivo <= data spunta cancellata
168000960416     C     WDAM          ANDLE     WVVDFV
168100960416     C* Non esistono altre spunte inventario o consegna in data >=
168200960416     C* alla data di cancellazione della spunta
168300960416     C     WSPUN         ANDEQ     *BLANKS
168400970127     C* La bolla non deve essere bloccata
168500970127     C     WFBC          ANDNE     'B'
168600970127     C* La bolla non è in giacenza
168700970127     C     WGIAC         ANDNE     'S'
168800000904     C                   EXSR      CRTA15
168900000904     C                   END
169000960416     C*
169100960416    1C                   END
169200050408     c* se la pistola della spunta annullata non è automatica devo decrement
169300050408     c* are sgnst2 su fisgn
169400060512     c                   movel     nps           knps
169500050408     c                   exsr      tippis
169600050408     c     §7jrps        ifne      'A'
169700050408     C* AGGIORNO FILE  FISGN SE C'E'
169800050408     C     KBOL2         CHAIN     FISGN03L                           33
169900050408     C     *IN33         DOWEQ     *OFF
170000050408     c* testo > 0 per sicurezza ma in teoria non DEVE succedere che sia = 0
170100050408     C     SGNST2        IFgt      0
170200050408     C                   sub       1             SGNST2
170300050408     C                   CLEAR                   SGNT6A
170400050408     C                   CLEAR                   SGNT6B
170500050408     C                   CLEAR                   SGNT6C
170600050408     C                   CLEAR                   SGNT6D
170700050408     C                   CLEAR                   SGNT6E
170800050408     C                   CLEAR                   SGNT6F
170900080623     c                   eval      sgndatora = %char(%timestamp:*iso0)
171000050408     C                   UPDATE    FISGN000
171100050408     C                   ENDIF
171200050408     C     KBOL2         READE     FISGN03L                               33
171300050408     C                   ENDDO
171400050408     C                   ENDIF
171500921019     C*
171600920424     C                   Z-ADD     3             B
171700941206     C*
171800970530     C     ENDUPD        ENDSR
171900970530     C**************************************************************************
172000970530     C* CONTROLLO IL TIPO PISTOLA DELLA SPUNTA CHE LEGGO
172100970530     C**************************************************************************
172200970530     C     TIPPIS        BEGSR
172300970530     C                   MOVEL     '7J'          COD
172400060512     C                   MOVEL(P)  knps          KEY
172500970530     C     KTAB          CHAIN     TABEL                              31
172600970530     C  N31              MOVEL     TBLUNI        DS7J
172700970530     C   31              CLEAR                   DS7J
172800970530     C*
172900970530     C                   ENDSR
173000961120     C**************************************************************************
173100970530     C* REPERISCO I DATI DEL FOGLIO DELLA SPUNTA LETTA
173200961120     C**************************************************************************
173300961120     C     CHAFV         BEGSR
173400961120     C                   Z-ADD     DATEU         DATAF
173500961120     C                   MOVEL     ' '           WFVV              1
173600961120     C                   MOVEL     ' '           WFGV              1
173700961120     C                   MOVEL     ' '           WFVA              1
173800970530     C                   CLEAR                   DEFF
173900970530     C                   CLEAR                   SPGF
174000970530     C                   CLEAR                   NFAF
174100970530     C                   CLEAR                   AEDF
174200970530     C                   CLEAR                   NFAFCF
174300970530     C                   CLEAR                   NFASPG
174400970530     C                   CLEAR                   NFAFGS
174500970530     C                   CLEAR                   NFADFV
174600970530     C                   CLEAR                   NFANPG
174700970530     C**
174800970530     C                   CLEAR                   DSLV53
174900040203     c                   if        brvnpg=1 and brvfgs=simfel
175000040203     c                   eval      d53tfo='G'
175100040203     c                   endif
175200040203     c                   if        brvnpg=1 and brvfgs<>simfel
175300040203     c                   eval      d53tfo='A'
175400040203     c                   endif
175500970529     C                   MOVEL     BRVNPG        D53NPG
175600000203     C                   Z-ADD     BRVNFV        D53NFV
175700970529     C                   MOVEL     BRVFGS        D53FGS
175800970529     C                   MOVEL     'A'           D53ABB
175900970529     C                   MOVEL     LNA           D53LNA
176000031114     C                   MOVEL     lnatfa        D53TFA
176100970529     C                   CALL      'FNLV53R'
176200970529     C                   PARM                    DSLV53
176300101202     c*
176400041014     c* Se errore e categoria "1" cerco in fgv, se cercato in fva
176500041014     c                   if        d53err<>*blanks and brvnpg=1 and d53tfo
176600041014     c                             ='A'
176700041014     C                   CLEAR                   DSLV53
176800041014     c                   eval      d53tfo='G'
176900041014     C                   MOVEL     BRVNPG        D53NPG
177000041014     C                   Z-ADD     BRVNFV        D53NFV
177100041014     C                   MOVEL     BRVFGS        D53FGS
177200041014     C                   MOVEL     SIMFEL        D53FEL
177300041014     C                   MOVEL     'A'           D53ABB
177400041014     C                   MOVEL     LNA           D53LNA
177500041014     C                   MOVEL     lnatfa        D53TFA
177600041014     C                   CALL      'FNLV53R'
177700041014     C                   PARM                    DSLV53
177800041014     c                   endif
177900970530     C*
178000970530     C* NESSUN ERRORE --> FOGLIO TROVATO
178100970530    1C     D53ERR        IFEQ      ' '
178200970529     C                   MOVEL     D53DFV        DATAF
178300970530     C                   MOVEL     D53SPG        SPGF
178400970530     C                   MOVEL     D53DEF        DEFF              1
178500970530     C                   MOVEL     D53NFA        NFAF
178600970530     C                   MOVEL     D53AED        AEDF
178700970530     C**
178800970530    2C     D53TFO        IFEQ      'G'
178900970530     C                   MOVEL     'S'           WFGV
179000970529     C                   ELSE
179100970530    3C     D53TFO        IFEQ      'V'
179200970530     C                   MOVEL     'S'           WFVV
179300970530   X3C                   ELSE
179400970530     C*
179500970530     C                   MOVEL     'S'           WFVA
179600970530     C* SE ABBINATO CONTROLLO IL FOGLIO ARRIVI PER VEDERE
179700970530     C*  SE ESISTE CHIUSO
179800970530    4C     D53NFA        IFGT      0
179900970620     C                   MOVEL     D53NFA        SAVNFA
180000970620     C                   MOVEL     D53NPA        SAVNPA
180100020917     c                   Movel     D53Lai        SavLai
180200970530     C                   CLEAR                   DSLV53
180300970620     C                   MOVEL     SAVNFA        D53NFV
180400970620     C                   MOVEL     SAVNPA        D53NPG
180500970530     C                   MOVEL     'V'           D53TFO
180600970530     C                   MOVEL     SIMFEL        D53FEL
180700970530     C                   MOVEL     SIMFEL        D53FLF
180800020917     c                   Movel     SavLai        D53Fgs
180900970530     C                   CALL      'FNLV53R'
181000970530     C                   PARM                    DSLV53
181100970530     C* TROVATO FOGLIO DI ABBINAMENTO --> SE ERRORE COME SE NON CI FOSS
181200970530    5C     D53ERR        IFEQ      ' '
181300970530     C                   MOVEL     D53FCF        NFAFCF
181400970530     C                   MOVEL     D53SPG        NFASPG
181500970530     C                   MOVEL     D53FGS        NFAFGS
181600970530     C                   MOVEL     D53DFV        NFADFV
181700970530     C                   MOVEL     D53NPG        NFANPG
181800970530     C                   ELSE
181900970530     C                   CLEAR                   NFAF
182000970530     C                   CLEAR                   AEDF
182100970530    5C                   ENDIF
182200970530    4C                   ENDIF
182300970530    3C                   ENDIF
182400970530    2C                   ENDIF
182500970530    1C                   ENDIF
182600961120     C*
182700961120     C                   ENDSR
182800960606     C**************************************************************************
182900960606     C* DETERMINO TERMINAL DI ARRIVO DI BRVFGS
183000960606     C**************************************************************************
183100960606     C     TERFGS        BEGSR
183200970530     C                   CLEAR                   DSLV55
183300970530     C                   MOVEL     LNPB          D55LNP
183400970530     C                   MOVEL     BRVFGS        D55LIN
183500970530     C                   MOVEL     DATAF         D55DRF
183600970530     C                   MOVEL     'A'           D55TPT
183700970530     C                   CALL      'FNLV55R'
183800970530     C                   PARM                    DSLV55
183900970530     C     D55ERR        IFEQ      ' '
184000970530     C                   MOVEL     D55TFA        WTFG
184100970530     C                   ELSE
184200970530     C                   MOVEL     BRVFGS        WTFG
184300970530     C                   ENDIF
184400960606     C*
184500960606     C                   ENDSR
184600941206     C**************************************************************************
184700941206     C* CONTROLLO DETTAGLIO FNBLT00F
184800941206     C**************************************************************************
184900921019     C     CTRBLT        BEGSR
185000941206     C*
185100930604     C                   MOVEL     *ALL'9'       BLPDSE
185200941206     C     KBLP2         SETLL     FNBLT01L
185300941206     C     KBLP2         READE     FNBLT01L                               34
185400921019     C*
185500921019     C     *IN34         DOWEQ     '0'
185600921021     C*
185700930604     C     BLTDSE        IFNE      0
185800921021     C     BLTDSE        ANDLT     BLPDSE
185900921021     C                   MOVEL     BLTDSE        BLPDSE
186000921021     C                   END
186100921021     C*
186200941206     C     KBLP2         READE     FNBLT01L                               34
186300921019     C                   END
186400921021     C*
186500930604     C     BLPDSE        IFEQ      *ALL'9'
186600921021     C                   Z-ADD     0             BLPDSE
186700921021     C                   END
186800941206     C*
186900921019     C                   ENDSR
187000941206     C**************************************************************************
187100941206     C* CONTROLLO SE ALTRI COLLI HANNO DATA ARRIVO E/O COD.ANOM
187200941206     C**************************************************************************
187300921019     C     CTRART        BEGSR
187400941206     C*
187500930603     C* ABILITAZIONI
187600930603     C                   MOVEL     ' '           NOAMP             1
187700930603     C                   MOVEL     ' '           NOAMA             1
187800930604     C  N20              MOVEL     *ALL'9'       ARBDAM
187900060512     c                   clear                   abbnsc
188000930604     C*
188100941206     C     KARB2         SETLL     FNART01L
188200941206     C     KARB2         READE     FNART01L                               34
188300921019     C*
188400921019     C* CONTROLLO SE ALTRI COLLI HANNO LA DATA ARRIVO
188500921019     C     *IN34         DOWEQ     '0'
188600930604     C*
188700930604     C     *IN20         IFEQ      '0'
188800930528     C* SE ALMENO UN COLLO HA ARTDAM = 0  DEVO AZZERARE ARBDTI E ARBHTI
188900930603     C* NOAMA = S NON E' ABILITATO IN BASE ALLA SPUNTA ARRIVI
189000930528     C     ARTDAM        IFEQ      0
189100930528     C                   Z-ADD     0             ARBDTI
189200930528     C                   Z-ADD     0             ARBHTI
189300930603     C                   MOVEL     'S'           NOAMA
189400930528     C                   END
189500921021     C*
189600930603     C     ARTDAM        IFGT      0
189700921021     C     ARTDAM        ANDLT     ARBDAM
189800921021     C                   Z-ADD     ARTDAM        ARBDAM
189900921021     C                   END
190000930603     C*
190100930603     C* NOAMP = S NON E' ABILITATO IN BASE ALLA SPUNTA PART(NON ABBINAT
190200941209     C* ARTABN='S'   ===>  SPUNTA DI FOGLIO ABBINATO
190300941209     C     ARTABN        IFNE      'S'
190400930603     C                   MOVEL     'S'           NOAMP
190500060512     c* Mewmorizzo il segnacollo con la data più alta e verifico se
190600060512     c*  il relativo foglio IDA è stato chiuso per decidere come
190700060512     c*  impostare arbama
190800060512     c                   else
190900060512     c                   if        artdfv>abbdfv  and artflp=0
191000060512     c                   eval      abbnsc=artnsc
191100060512     c                   eval      abbdfv=artdfv
191200060512     c                   endif
191300060512     c                   if        artdut>abbdfv  and artflp>0
191400060512     c                   eval      abbnsc=artnsc
191500060512     c                   eval      abbdfv=artdut
191600060512     c                   endif
191700930603     C                   END
191800930604     C                   END
191900921019     C*
192000921019     C* CONTROLLO SE ALTRI COLLI HANNO L'ANOMALIA
192100921019     C  N06ARTCAN        IFNE      ' '
192200921019     C                   SETON                                        06
192300930604     C   20              SETON                                        34
192400921019     C                   END
192500921019     C*
192600941206     C  N34KARB2         READE     FNART01L                               34
192700921019     C                   END
192800921021     C*
192900930604     C  N20ARBDAM        IFEQ      *ALL'9'
193000921021     C                   Z-ADD     0             ARBDAM
193100921021     C                   END
193200101202     c*
193300060512     c* Se devo abilitare in base alla sola spunta partenza, verifico
193400060512     c*  foglio IDA
193500060512    1c                   if        noama='S' and noamp=' '  and
193600060512     c* Escluso i locali
193700060512     c                             arbtfp<>arbtfa
193800060512     c                   eval      knpg=1
193900140618     c     kspu          setll     fnbrv05l
194000140618     c     kspu          reade     fnbrv05l
194100140618    2c                   dow       not %eof(fnbrv05l)
194200060512     C                   CLEAR                   DSLV53
194300060512     C                   MOVEL     'A'           D53TFO
194400060512     C                   MOVEL     BRVNPG        D53NPG
194500060512     C                   Z-ADD     BRVNFV        D53NFV
194600060512     C                   MOVEL     BRVFGS        D53FGS
194700060512     C                   MOVEL     'A'           D53ABB
194800060512     C                   MOVEL     LNA           D53LNA
194900060512     C                   MOVEL     lnatfa        D53TFA
195000060512     C                   CALL      'FNLV53R'
195100060512     C                   PARM                    DSLV53
195200060512     c
195300060512     c* Escludo la defluenza
195400060512    3c                   if        d53err=' '  and d53def=' '
195500060512    4c                   if        d53nfa>0
195600060512     c* chain
195700060512     c     kfvv          chain     fnfvv01l
195800060512    5c                   if        %found(fnfvv01l) and fvvfcf='S'
195900060512     c                   eval      noamp='0'
196000060512   x5c                   else
196100060512     c                   clear                   noamp
196200060512     c                   leave
196300060512    5c                   endif
196400101202     c*
196500060512   x4c                   else
196600060512     c                   clear                   noamp
196700060512     c                   leave
196800060512    4c                   endif
196900060512    3c                   endif
197000101202     c*
197100140618     c     kspu          reade     fnbrv05l
197200060512    2c                   enddo
197300060512    1c                   endif
197400921019     C*
197500921019     C                   ENDSR
197600941206     C**************************************************************************
197700941206     C*    CONTROLLO SE ALMENO UN COLLO HA LA DATA
197800941206     C**************************************************************************
197900921019     C     CTRBTT        BEGSR
198000941206     C*
198100941206     C                   MOVEL     *ALL'9'       BTPDET
198200031114     C     KBTP2         SETLL     FNBTT11L
198300031114     C     KBTP2         READE     FNBTT11L                               34
198400921019     C*
198500921019     C     *IN34         DOWEQ     '0'
198600921021     C*
198700941206     C     BTTDET        IFNE      0
198800941206     C     BTTDET        ANDLT     BTPDET
198900941206     C                   MOVEL     BTTDET        BTPDET
199000921021     C                   END
199100921019     C*
199200031114     C     KBTP2         READE     FNBTT11L                               34
199300921019     C                   END
199400921021     C*
199500941206     C     BTPDET        IFEQ      *ALL'9'
199600941206     C                   Z-ADD     0             BTPDET
199700921021     C                   END
199800921019     C*
199900921019     C                   ENDSR
200000960417     C**************************************************************************
200100960417     C*    CREAZIONE ANOMALIA 005
200200960417     C**************************************************************************
200300960417     C     CRTA05        BEGSR
200400961120     C* SE SI TRATTA DI SPUNTA LOCALE, CERCO IL FOGLIO ARRIVI
200500961120     C*  CON DATA BORDERO' + 1
200600961120     C     WLOCAL        IFEQ      'S'
200700970127     C* SE NON C'E' LA DATA BORDERO' èRENDO LA DATA DEL GIORNO
200800970127     C     WDBR          IFGT      0
200900961120     C                   MOVE      WDBR          G08INV
201000970127     C                   ELSE
201100970127     C                   MOVE      DATEU         G08INV
201200970127     C                   ENDIF
201300970127     C*
201400961120     C                   MOVEL     '3'           G08ERR
201500961120     C                   CALL      'XSRDA8'
201600961120     C                   PARM                    WLBDA8
201700961120     C*
201800961120     C*  DATA BORDERO' + 1
201900961120     C     G08TGI        ADD       1             GIOTGI
202000961120     C                   CALL      'XSRGI8'
202100961120     C                   PARM                    WGIDAT
202200961120     C*
202300961120     C                   Z-ADD     GIOINV        WDBR1
202400961120     C                   Z-ADD     2             KNPG
202500961120     C     KFVV3         SETLL     FNFVV02L
202600961120     C     KFVV3         READE     FNFVV02L                               31
202700961120     C     *IN31         DOWEQ     *OFF
202800020919     c     fvvatb        andne     ' '
202900961120     C     KFVV3         READE     FNFVV02L                               31
203000961120     C                   ENDDO
203100961120     C  N31              Z-ADD     FVVDFV        W0060             6 0
203200961120     C   31              CLEAR                   W0060
203300961120     C**
203400961120     C** IMPOSTO 1 PRIMI CAMPI
203500031114     C                   Z-ADD     lnatfa        WFGS
203600961120     C                   MOVEL     2             WNPG
203700961120     C                   MOVEL     ' '           WSPG
203800961120     C**
203900101202     C                   SELECT
204000101202     C     W0060         WHENEQ    0
204100961120     C                   Z-ADD     WDBR1         WDFV
204200101202     C                   OTHER
204300961120     C                   Z-ADD     W0060         WDFV
204400101202     C                   ENDSL
204500961120     C*
204600961120     C                   ENDIF
204700961120     C**
204800990518     C                   CLEAR                   FNANM000
204900960417     C                   MOVE      LNP           ANMFLS
205000960417     C                   MOVE      LNA           ANMLNA
205100960417     C                   MOVE      NRS           ANMNRS
205200960417     C                   MOVE      NSC           ANMSCN
205300960417     C                   MOVE      WAAS          ANMAAS
205400960417     C                   MOVE      WLNP          ANMLNP
205500960417     C                   MOVE      WNSP          ANMNSP
205600960417     C* Ricerco fase
205700960417     C                   CLEAR                   DSFASE
205800960417     C                   MOVE      WNPG          D11NPG
205900960417     C                   MOVE      WSPG          D11SPG
206000960417     C                   CALL      'TRUL11R'
206100960417     C                   PARM                    DSFASE
206200960417     C                   MOVE      D11FAS        ANMFSA
206300960417     C                   Z-ADD     WDFV          ANMDAO
206400960417     C                   MOVE      WNPS          ANMNPS
206500961120     C                   MOVE      WCDU          ANMCDU
206600990518     C                   Z-ADD     WNFV          ANMNFV
206700960417     C                   MOVEL     WFGS          ANMFLE
206800960417     C                   MOVE      SAVLNP        WFL2
206900960417     C     ANMLNP        IFGT      0
207000960417     C                   MOVEL     ANMLNP        WFL1
207100960417     C                   ELSE
207200960417     C                   MOVEL     ANMFLS        WFL1
207300960417     C                   ENDIF
207400960417     C                   Z-ADD     5             COMCAA
207500960417     C                   EXSR      CRTANM
207600960417     C                   ENDSR
207700960417     C**************************************************************************
207800960417     C*    CREAZIONE ANOMALIA 015
207900960417     C**************************************************************************
208000960417     C     CRTA15        BEGSR
208100990518     C                   CLEAR                   FNANM000
208200960417     C* Cerco foglio ima della data della spunta cancellata
208300960417     C                   EXSR      RCRIMA
208400960417     C*
208500961118     C     WCREA         IFEQ      ' '
208600960417     C                   MOVE      LNP           ANMFLS
208700960417     C                   MOVE      LNA           ANMLNA
208800960417     C                   MOVE      NRS           ANMNRS
208900960417     C                   MOVE      NSC           ANMSCN
209000960417     C                   MOVE      WAAS          ANMAAS
209100960417     C                   MOVE      WLNP          ANMLNP
209200960417     C                   MOVE      WNSP          ANMNSP
209300960417     C* Ricerco fase
209400960417     C                   CLEAR                   DSFASE
209500960417     C                   MOVE      WNPG          D11NPG
209600960417     C                   MOVE      WSPG          D11SPG
209700960417     C                   CALL      'TRUL11R'
209800960417     C                   PARM                    DSFASE
209900960417     C                   MOVE      D11FAS        ANMFSA
210000960417     C                   MOVE      WVVDFV        ANMDAO
210100960417     C                   MOVE      WFGS          ANMCDU
210200990518     C                   Z-ADD     WNFV          ANMNFV
210300960417     C                   MOVE      WFGS          ANMFLE
210400960417     C                   CLEAR                   WFL2
210500960417     C     ANMLNP        IFGT      0
210600960417     C                   MOVEL     ANMLNP        WFL1
210700960417     C                   ELSE
210800960417     C                   MOVEL     ANMFLS        WFL1
210900960417     C                   ENDIF
211000960417     C*
211100960417     C                   Z-ADD     15            COMCAA
211200960417     C                   EXSR      CRTANM
211300961118     C                   ENDIF
211400960417     C                   ENDSR
211500960416     C**************************************************************************
211600960416     C*    CREAZIONE ANOMALIA
211700960416     C**************************************************************************
211800960416     C     CRTANM        BEGSR
211900960416     C                   Z-ADD     COMCAA        ANMCAA
212000960416     C     COMCAA        IFNE      SAVCAA
212100960416     C                   MOVEL     '7C'          COD
212200960416     C                   MOVEL(P)  ANMCAA        KEY
212300960416     C     KTAB          CHAIN     TABEL                              31
212400960416     C  N31              MOVEL     TBLUNI        DS7C
212500960416     C   31              MOVEL     *BLANKS       DS7C
212600960416     C                   MOVEL     COMCAA        SAVCAA
212700960416     C                   ENDIF
212800050707     c* imposto in anmflo data e ora creazione spunta
212900050707     C                   TIME                    W0140            14 0
213000050707     c
213100050707     C                   CLEAR                   WLBDA8
213200050707     C                   MOVE      w0140         G08DAT
213300050707     C                   CALL      'XSRDA8'
213400050707     C                   PARM                    WLBDA8
213500050707     C                   MOVE      G08INV        wdat6             6 0
213600050707     c                   movel     w0140         wora4             4 0
213700041018     c                   movel     wdat6         anmflo
213800041018     c                   move      wora4         anmflo
213900041018     c* Imposto la zona se trovo la bolla
214000041018     c                   z-add     999           anmfl4
214100041018     c                   select
214200041018     c                   when      wesblt='S'
214300041018     c                   z-add     blpznc        anmfl4
214400041018     c                   when      wesart='S'
214500041018     c                   z-add     arbznc        anmfl4
214600041018     c                   when      wesbtt='S'
214700041018     c                   z-add     btpznc        anmfl4
214800041018     c                   endsl
214900041018     c* Imposto se di valore
215000041018     c                   if        anmnsp>0
215100041018     c     kar5          chain     fiar501l
215200041018    3c                   if        %found(fiar501l)
215300041018     c                   movel     ar5uni        dar5gen
215400041018     c                   else
215500041018     c                   clear                   dar5gen
215600041018    3c                   endif
215700041018     c                   movel     §ar5bva       anmft4
215800041018     c                   endif
215900960416     C*
216000960416     C                   MOVEL     §7CTAN        ANMTAN
216100960416     C                   MOVEL     §7CSAN        ANMSAN
216200960416     C                   MOVEL     §7CAIE        ANMAIE
216300960416     C                   Z-ADD     §7CLID        ANMLID
216400960416     C                   Z-ADD     0             ANMDCH
216500960416     C                   MOVEL     ' '           ANMFSC
216600070424     C                   clear                   ANMcch
216700960416     C* CHIUSURA AUTOMATICA
216800960416     C     §7CCHA        IFEQ      'S'
216900960417     C                   Z-ADD     ANMDAO        ANMDCH
217000960417     C                   MOVE      ANMFSA        ANMFSC
217100960416     C                   END
217200960416     C                   Z-ADD     0             ANMFL1
217300960416     C                   Z-ADD     0             ANMFL2
217400960416     C* ESTERNA
217500000905    1C     §7CAIE        IFEQ      'E'
217600960416     C* VEDO A CHI TRASMETTERE
217700960416     C                   MOVEL     WFL1          ANMFL1
217800960416     C                   MOVEL     WFL2          ANMFL2
217900960416     C* SE UGUALI CANCELLO IL 2°
218000000905    2C     ANMFL1        IFEQ      ANMFL2
218100960416     C                   Z-ADD     0             ANMFL2
218200000905   X2C                   ELSE
218300000905     C                   CLEAR                   DSLV55
218400000905     C                   MOVEL     'P'           D55TPT
218500000905     C                   MOVE      ANMFL1        D55LIN
218600000905     C                   MOVE      DATEU         D55DRF
218700000905     C                   CALL      'FNLV55R'
218800000905     C                   PARM                    DSLV55
218900000905    3C     D55ERR        IFNE      *BLANKS
219000000905     C                   MOVE      ANMFL1        WTFP1
219100000905     C                   ELSE
219200000905     C                   MOVE      D55TFP        WTFP1
219300000905    3C                   END
219400000905     C                   CLEAR                   DSLV55
219500000905     C                   MOVEL     'P'           D55TPT
219600000905     C                   MOVE      ANMFL2        D55LIN
219700000905     C                   MOVE      DATEU         D55DRF
219800000905     C                   CALL      'FNLV55R'
219900000905     C                   PARM                    DSLV55
220000000905    3C     D55ERR        IFNE      *BLANKS
220100000905     C                   MOVE      ANMFL2        WTFP2
220200000905     C                   ELSE
220300000905     C                   MOVE      D55TFP        WTFP2
220400000905    3C                   END
220500000905    3C     WTFP1         IFEQ      WTFP2
220600000905     C                   CLEAR                   ANMFL2
220700000905    3C                   END
220800000905    2C                   ENDIF
220900961120     C                   ENDIF
221000960416     C*
221100961129     C* SE ANOMALIA INTERNA CHE USA SIA FLE CHE LNA LA FLAGGO PER
221200961129     C* NON TRASMETTERLA
221300961129    2C     §7CAIE        IFEQ      'I'
221400961129    2C     §7CUEA        ANDEQ     'E'
221500961129     C                   MOVEL     'T'           ANMFT1
221600961129    2C                   ENDIF
221700961129     C*
221800990518     C                   WRITE     FNANM000
221900960416     C                   ENDSR
222000960417     C**************************************************************************
222100960417     C*    RICERCA FOGLIO IMA
222200960417     C**************************************************************************
222300960417     C     RCRIMA        BEGSR
222400960417     C* DETERMINO FILIALE GESTIONE DELLA LNA DELLA SPUNTA
222500960417     C                   CLEAR                   WFGS
222600960417     C                   CLEAR                   WNFV
222700960417     C                   CLEAR                   WNPG
222800960417     C                   CLEAR                   WSPG
222900020418      * controllo su azcae
223000020422     C     LNA           LOOKUP    L6S                                    30
223100020422     C     *IN30         IFEQ      *OFF
223200020422     C                   MOVE      LNA           WFGS
223300020422     C                   ELSE
223400020418     c                   eval      ktfa = lna
223500020418     c                   eval      kdde = *date
223600020418     c     kazcae        setll     azcae02l
223700020418     c                   do        *hival
223800020418     c     kazcae        reade     azcae02l
223900020418     c                   if        %eof(azcae02l)
224000020418     c                   leave
224100020418     c                   endif
224200020418     c                   if        caeatb <> *blanks
224300020418     c                   iter
224400020418     c                   endif
224500020418     c                   if        caedde > kdde or caedsc < kdde
224600020418     c                   iter
224700020418     c                   endif
224800020418     c                   move      caetfe        wfgs
224900020422     c                   leave
225000020418     c                   enddo
225100020422     C                   ENDIF
225200101202      *
225300960417     C                   MOVEL     '3'           WNPG
225400960417     C* CHAIN SU FOGLIO IMA
225500960417     C     KFVV2         CHAIN     FNFVV02L                           30
225600000203     C  N30              Z-ADD     FVVNFV        WNFV
225700960417     C  N30              MOVE      FVVNPG        WNPG
225800960417     C  N30              MOVE      FVVSPG        WSPG
225900961118     C**
226000961118     C* SE IL FOGLIO NON C'E' O E' APERTO, NON CREO L'ANOMALIA
226100961118     C                   CLEAR                   WCREA
226200961118     C     *IN30         IFEQ      *ON
226300961118     C     FVVFCF        ORNE      'S'
226400961118     C                   MOVEL     'N'           WCREA             1
226500961118     C                   ENDIF
226600960417     C                   ENDSR
226700960605     C**************************************************************************
226800960605     C*    RICERCA LNP BOLLA
226900960605     C**************************************************************************
227000960605     C     RICLNP        BEGSR
227100031114     c* La tabella 5F qui non viene cercata perchè non serve: mi basta
227200031114     c*  soltanto sapere se c'e' spunta partenza da altro terminal per
227300031114     c*  la creazione della anomalia 05 altrimenti questo pgm non crea
227400031114     c*  nulla: al massimo cancella
227500961120     C                   MOVE      1             KNPG
227600140618     C     KBRV3         SETLL     FNBRV05L
227700140618     C     KBRV3         READE     FNBRV05L                               32
227800031114    1C     *IN32         DOWEQ     *OFF
227900031114    2C     BRVATR        IFEQ      *BLANKS
228000031114    3C     BRVpes        IFNE      LNP
228100061120     C     BRVfgs        andne     pestfp
228200960621     C                   MOVE      BRVFGS        LNPB
228300031114     c                   eval      *in32=*on
228400031114    3C                   END
228500031114    2C                   END
228600140618     C  N32KBRV3         READE     FNBRV05L                               32
228700031114    1C                   ENDDO
228800960605     C                   ENDSR
228900970530     C**************************************************************************
229000970530     C     *INZSR        BEGSR
229100970530     C                   MOVEL     KPJBU         SAVJBU
229200970530     C                   Z-ADD     1             CODUT             1 0
229300970530     C* GIRO DATA DEL GIORNO
229400970530     C                   TIME                    W0140            14 0
229500970530     C                   MOVE      W0140         DATEU             8 0
229600970530     C                   CLEAR                   WLBDA8
229700970530     C                   MOVE      DATEU         G08DAT
229800970530     C                   CALL      'XSRDA8'
229900970530     C                   PARM                    WLBDA8
230000970530     C                   MOVE      G08INV        DATEU
230100970530     C* DEFINIZIONE CAMPI
230200031114     C     *LIKE         DEFINE    d53def        wvvdef
230300031114     C     *LIKE         DEFINE    fvvdfv        wdtrif
230400031114     C     *LIKE         DEFINE    fvvdfv        dspb
230500031114     C     *LIKE         DEFINE    fvvdfv        SVVDFV
230600031114     C     *LIKE         DEFINE    blpLNA        SVVLNA
230700031114     C     *LIKE         DEFINE    blpLNP        SVVLNP
230800031114     C     *LIKE         DEFINE    blpLNA        LNATFA
230900031114     C     *LIKE         DEFINE    blpLNA        LNATFP
231000031114     C     *LIKE         DEFINE    blpLNA        LNPTFP
231100031114     C     *LIKE         DEFINE    blpLNA        PESTFA
231200031114     C     *LIKE         DEFINE    blpLNA        PESTFP
231300000905     C     *LIKE         DEFINE    ANMLNP        WTFP1
231400000905     C     *LIKE         DEFINE    ANMLNP        WTFP2
231500000904     C     *LIKE         DEFINE    FVVNFV        SAVNFA
231600970620     C     *LIKE         DEFINE    FVVNPG        SAVNPA
231700020917     c     *Like         Define    FvvFgs        SavLai
231800970620     C     *LIKE         DEFINE    KPJBU         SAVJBU
231900970530     C     *LIKE         DEFINE    FVVSPG        NFASPG
232000970530     C     *LIKE         DEFINE    FVVFGS        NFAFGS
232100970530     C     *LIKE         DEFINE    FVVFCF        NFAFCF
232200970530     C     *LIKE         DEFINE    FVVDFV        NFADFV
232300970530     C     *LIKE         DEFINE    FVVNPG        NFANPG
232400970530     C     *LIKE         DEFINE    FVVSPG        SPGF
232500970530     C     *LIKE         DEFINE    FVVNFV        NFAF
232600970530     C     *LIKE         DEFINE    FVVDFV        AEDF
232700970530     C     *LIKE         DEFINE    FVVDFV        WVVDFV
232800970530     C     *LIKE         DEFINE    FVVSPG        WVVSPG
232900970530     C     *LIKE         DEFINE    FVVDFV        SPUDFV
233000970530     C     *LIKE         DEFINE    BRVNPS        SPUNPS
233100970530     C     *LIKE         DEFINE    FVVDFV        ENTDFV
233200970530     C     *LIKE         DEFINE    BRVNPS        ENTNPS
233300990412     C     *LIKE         DEFINE    FVVDFV        A56DFV
233400990412     C     *LIKE         DEFINE    BRVNFV        A56NFV
233500990412     C     *LIKE         DEFINE    BRVFGS        A56FGS
233600990412     C     *LIKE         DEFINE    BRVNPS        A56NPS
233700990412     C     *LIKE         DEFINE    FVVSPG        A56SPG
233800990412     C     *LIKE         DEFINE    FVVNPG        A56NPG
233900970530     C     *LIKE         DEFINE    BRVFGS        WTFG
234000970530     C     *LIKE         DEFINE    FVVDFV        DATAF
234100970530     C     *LIKE         DEFINE    FVVSPG        SPUSPG
234200970530     C     *LIKE         DEFINE    BRVNPG        SPUNPG
234300970530     C     *LIKE         DEFINE    BRVFGS        SPUFGS
234400990409     C     *LIKE         DEFINE    BRVFGS        ENTFGS
234500970530     C     *LIKE         DEFINE    BRVNFV        SPUNFV
234600970530     C     *LIKE         DEFINE    FVVFCF        WCHIUS
234700970530     C     *LIKE         DEFINE    ANMCAA        COMCAA
234800970530     C     *LIKE         DEFINE    ANMCAA        SAVCAA
234900970530     C     *LIKE         DEFINE    ANMFL1        WFL1
235000970530     C     *LIKE         DEFINE    ANMFL2        WFL2
235100970530     C     *LIKE         DEFINE    ARBAAS        WAAS
235200970530     C     *LIKE         DEFINE    ANMLNP        WLNP
235300970530     C     *LIKE         DEFINE    ANMNSP        WNSP
235400970530     C     *LIKE         DEFINE    BRVNPS        WNPS
235500000203     C     *LIKE         DEFINE    BRVNFV        WNFV
235600970530     C     *LIKE         DEFINE    ANMCDU        WCDU
235700970530     C     *LIKE         DEFINE    FVVDFV        WDFV
235800970530     C     *LIKE         DEFINE    FVVNPG        WNPG
235900970530     C     *LIKE         DEFINE    FVVSPG        WSPG
236000970530     C     *LIKE         DEFINE    FVVFGS        WFGS
236100970530     C     *LIKE         DEFINE    ARTDAM        WDAM
236200070424     C     *LIKE         DEFINE    ARTDET        WDET
236300070424     C     *LIKE         DEFINE    ARTFLP        WFLP
236400970530     C     *LIKE         DEFINE    ARTDCM        WDCM
236500970530     C     *LIKE         DEFINE    ARTDTR        WDTR
236600970530     C     *LIKE         DEFINE    ARBDBR        WDBR
236700970530     C     *LIKE         DEFINE    ARBFBC        WFBC
236800970530     C     *LIKE         DEFINE    ARBDBR        WDBR1
236900970530     C     *LIKE         DEFINE    FVVDFV        SAVDFV
237000970530     C     *LIKE         DEFINE    FVVFGS        SAVLNP
237100970604     C     *LIKE         DEFINE    ARTDAM        SAVDAM
237200970604     C     *LIKE         DEFINE    BTTDET        SAVDET
237300970530     C     *LIKE         DEFINE    LNP           LNPB
237400970530     C     *LIKE         DEFINE    BRVNPG        KNPG
237500970721     C     *LIKE         DEFINE    GCPFRG        KFRG
237600970721     C                   MOVEL     '0'           KFRG
237700970530     C**
237800031114     C     Kbar37        KLIST
237900041014     C                   KFLD                    BPES
238000031114     C                   KFLD                    LNP
238100031114     C                   KFLD                    LNA
238200031114     C                   KFLD                    NRS
238300031114     C                   KFLD                    NSC
238400041015     C     KFGV          KLIST
238500041015     C                   KFLD                    dis1nfv
238600041015     C                   KFLD                    dis1fgs
238700031114     C     KBOL2         KLIST
238800970530     C                   KFLD                    LNP
238900970530     C                   KFLD                    LNA
239000970530     C                   KFLD                    NRS
239100970530     C                   KFLD                    NSC
239200060512     C     Kspu          KLIST
239300060512     C                   KFLD                    knpg
239400060512     c                   KFLD                    LNP
239500060512     C                   KFLD                    LNA
239600060512     C                   KFLD                    NRS
239700060512     C                   KFLD                    abbnsc
239800970530     C     KARB2         KLIST
239900970530     C                   KFLD                    ARBAAS
240000970530     C                   KFLD                    ARBLNP
240100970530     C                   KFLD                    ARBNRS
240200970530     C                   KFLD                    ARBNSP
240300970721     C     KGCA          KLIST
240400970721     C                   KFLD                    ARBAAS
240500970721     C                   KFLD                    ARBLNP
240600970721     C                   KFLD                    ARBNRS
240700970721     C                   KFLD                    ARBNSP
240800970721     C                   KFLD                    KFRG
240900970530     C     KARB          KLIST
241000970530     C                   KFLD                    ARTAAS
241100970530     C                   KFLD                    ARTLNP
241200970530     C                   KFLD                    ARTNRS
241300970530     C                   KFLD                    ARTNSP
241400970530     C     KBTP2         KLIST
241500031114     C                   KFLD                    BTPflp
241600031114     C                   KFLD                    BTPAAS
241700970530     C                   KFLD                    BTPLNP
241800970530     C                   KFLD                    BTPNRS
241900970530     C                   KFLD                    BTPNSP
242000031114     C     KBTT          KLIST
242100031114     C                   KFLD                    BTTflp
242200031114     C                   KFLD                    BTTAAS
242300970530     C                   KFLD                    BTTLNP
242400970530     C                   KFLD                    BTTNRS
242500970530     C                   KFLD                    BTTNSP
242600970530     C     KBLP2         KLIST
242700970530     C                   KFLD                    BLPAAS
242800970530     C                   KFLD                    BLPLNP
242900970530     C                   KFLD                    BLPNRS
243000970530     C                   KFLD                    BLPNSP
243100031114     C     KBLT          KLIST
243200970530     C                   KFLD                    BLTAAS
243300970530     C                   KFLD                    BLTLNP
243400970530     C                   KFLD                    BLTNRS
243500970530     C                   KFLD                    BLTNSP
243600970530     C     KFVV2         KLIST
243700970530     C                   KFLD                    WNPG
243800970530     C                   KFLD                    WVVDFV
243900970530     C                   KFLD                    WFGS
244000060512     C     KFVV          KLIST
244100060512     C                   KFLD                    d53npa
244200060512     C                   KFLD                    d53nfa
244300060512     C                   KFLD                    d53lai
244400970530     C     KFVV3         KLIST
244500970530     C                   KFLD                    KNPG
244600970530     C                   KFLD                    WDBR1
244700031114     c                   Kfld                    lnatfa
244800970530     C     KTAB          KLIST
244900970530     C                   KFLD                    CODUT
245000970530     C                   KFLD                    COD               2
245100970530     C                   KFLD                    KEY               8
245200970530     C     KBRV3         KLIST
245300970530     C                   KFLD                    KNPG
245400970530     C                   KFLD                    LNP
245500970530     C                   KFLD                    LNA
245600970530     C                   KFLD                    NRS
245700970530     C                   KFLD                    NSC
245800970530     C     KAGB          KLIST
245900970530     C                   KFLD                    AGBTBO
246000970530     C                   KFLD                    AGBAAS
246100970530     C                   KFLD                    AGBLNP
246200970530     C                   KFLD                    AGBNRS
246300970530     C                   KFLD                    AGBNSP
246400970530     C                   KFLD                    AGBAGB
246500001128     C*
246600101202      *
246700020418     c     kazcae        klist
246800020418     c                   kfld                    kepa
246900020418     c                   kfld                    ktfa
247000020418     c                   kfld                    ktfp
247100101202      *
247200030109     c     kFidta        Klist
247300030109     c                   Kfld                    SavAas
247400030109     c                   Kfld                    SavLnp1
247500030109     c                   Kfld                    SavNrs
247600030109     c                   Kfld                    SavNsp
247700030109     c                   Kfld                    SavTrd
247800030109     c                   Kfld                    SavPoc
247900041018     C     Kar5          KLIST
248000041018     C                   KFLD                    anmaas
248100041018     C                   KFLD                    anmLNp
248200041018     C                   KFLD                    anmNRS
248300041018     C                   KFLD                    anmnsp
248400041018     C                   KFLD                    Ktrd
248500970530     C*---------------------------------------------------------------*
248600970530     C*
248700970530     C*  Reperisco livello di chiusura anomalia come IDD
248800970530     C                   MOVEL(P)  2             KEY
248900970530     C                   MOVEL     '7A'          COD
249000970530     C     KTAB          CHAIN     TABEL                              31
249100970530     C  N31              MOVEL     TBLUNI        DS7A2
249200970530     C   31              CLEAR                   DS7A2
249300000825     C*
249400970530     C                   MOVEL     SAVJBU        KPJBU
249500970530     C                   ENDSR
249600970530     C**************************************************************************
249700970530     C     MEMDAT        BEGSR
249800041015     c                   clear                   wnoarrivo
249900041014     c* Faccio 3 memorizzazioni:
250000041014     c* 1) Spunta transito: solo se chi spara è p.o. spunta
250100041014    1c                   if        wesbtt='S' and brvpes=BPES   or
250200041014     c*    oppure spunta di disguido: idem
250300041018     c                             wesbtt<>'S' and wagpar=' ' and wagarr=' '
250400041014     c                             and brvpes=bpes
250500041014    2c                   if        tradfv=0 or spudfv<tradfv
250600970530     C                   MOVEL     BRVNPS        SPUNPS
250700970530     C                   MOVE      BRVNPG        SPUNPG
250800970530     C                   MOVE      BRVNFV        SPUNFV
250900970530     C                   MOVE      BRVFGS        SPUFGS
251000970530     C                   MOVEL     SPGF          SPUSPG
251100041014     C                   MOVEL     spudfv        tradfv
251200041014     C                   MOVEL     BRVNPS        traNPS
251300041014     C                   MOVE      BRVNPG        traNPG
251400041014     C                   MOVE      BRVFGS        traFGS
251500041014     C                   SETON                                        1918
251600041014    2c                   endif
251700041014    1c                   endif
251800041014     c* Per spunta transito o disguido memorizzo la spunta con data/ora cari
251900041014     c*  più alta, per aggiornare altro eventuale disguido/transito
252000041014    1c                   if        wesbtt='S' or
252100041014     c                             wagpar=' ' and wagarr=' '
252200041014     c* Non è cat.partenza non è ne del term.partenza  nè del term.arrivo
252300041015    2c                   if        brvnpg<>1 or  deff='S'
252400041014    3c                   if        wtfg<>lnatfa and brvfle<>lnptfp
252500041014    4c                   if        dis2dfv=0 or spudfv>=dis2dfv
252600041014     C                   MOVEL     BRVNPS        dis2nPS
252700041014     C                   MOVE      BRVNFV        dis2NFV
252800041014     C                   MOVE      BRVFGS        dis2FGS
252900041014     C                   MOVE      spudfv        dis2dfv
253000041015     c                   if        brvscar>0
253100041014     C                   MOVE      brvscar       dis2scar
253200041015     c                   else
253300041015     c                   move      brvimm        dis2scar
253400041015     c                   endif
253500041014    4c                   endif
253600041014    3c                   endif
253700041014    2c                   endif
253800101202     c*
253900041014    1c                   endif
254000101202     c*
254100041014     c* 2) Spunta partenza: il terminal di partenza spunta deve essere
254200041014     c*                     simfel
254300041015    1c                   if        wagpar='S' and brvfle=simfel
254400041015    2c                   if        prtdfv=0 or spudfv<prtdfv
254500041014     C                   MOVEL     BRVNPS        SPUNPS
254600041014     C                   MOVE      BRVNPG        SPUNPG
254700041014     C                   MOVE      BRVNFV        SPUNFV
254800041014     C                   MOVE      BRVFGS        SPUFGS
254900041014     C                   MOVEL     SPGF          SPUSPG
255000041014     C                   MOVEL     spudfv        prtdfv
255100041014     C                   MOVEL     BRVNPS        prtNPS
255200041014     C                   MOVE      BRVNPG        prtNPG
255300041014     C                   MOVE      BRVFGS        prtFGS
255400041014     C                   SETON                                        1917
255500041014    2c                   endif
255600041014     c* Per i danni, cerco la spunta con data più alta
255700041018    2c                   if        dandfv=0 or dandfv<=spudfv
255800041014     C                   MOVEL     BRVNPS        danNPS
255900041014     C                   MOVE      spudfv        dandFV
256000041014     C                   MOVE      BRVFGS        danFGS
256100041018     C                   MOVE      BRVnpg        dannpg
256200041018     C                   MOVE      spgf          danspg
256300041014    2c                   endif
256400041014     c* Se bolla locale, memorizzo a parte dati spunta entrata
256500041014    2c                   if        wsploc<>*blanks and entdfv=0 or
256600041014     c                             wsploc<>*blanks and spudfv<entdfv
256700041014    3c                   if        brvnpg=5 or
256800041014     c                             (brvnpg=3 and spgf='I') or
256900041014     c                             (brvnpg=3 and spgf='P') or
257000041014     c                             (brvnpg=1 and deff<>'S')
257100041014     C                   MOVEL     spudfv        ENTDFV
257200041014     C                   MOVE      BRVNPS        ENTNPS
257300041014     C                   MOVE      BRVFGS        ENTFGS
257400041015     c* non devo consdierare la spunta per l'aggiornamento in arrivo
257500041015     c                   eval      wnoarrivo='N'
257600041014    3c                   endif
257700041014    2c                   endif
257800101202     c*
257900041014    1c                   endif
258000101202     c*
258100041014     c* 2) Spunta arrivo  : il terminal di arrivo spunta deve essere
258200041014     c*                     term.arrivo lna spunta annullata
258300041015    1c                   if        wagarr='S' and brvfle=simfel  and
258400041015     c                             wnoarrivo<>'N'
258500041014    2c                   if        arrdfv=0 or spudfv<arrdfv
258600041014     C                   MOVEL     BRVNPS        SPUNPS
258700041014     C                   MOVE      BRVNPG        SPUNPG
258800041014     C                   MOVE      BRVNFV        SPUNFV
258900041014     C                   MOVE      BRVFGS        SPUFGS
259000041014     C                   MOVEL     SPGF          SPUSPG
259100041014     C                   MOVEL     spudfv        arrdfv
259200041014     C                   MOVEL     BRVNPS        arrNPS
259300041014     C                   MOVE      BRVNPG        arrNPG
259400041014     C                   MOVE      BRVFGS        arrFGS
259500101202     c*
259600050509    3c                   if        brvnpg=5 or
259700050509     c                             (brvnpg=3 and spgf='I') or
259800050509     c                             (brvnpg=3 and spgf='P') or
259900050509     c                             (brvnpg=1 and deff<>'S')
260000050509     c                   eval      arrfl2='P'
260100050509     c                   else
260200050509     c                   eval      arrfl2=' '
260300050509     c                   endif
260400041014     C                   SETON                                        1916
260500041014    2c                   endif
260600041014     c* Per i danni, cerco la spunta con data più alta
260700041018    2c                   if        dandfv=0 or dandfv<=spudfv
260800041014     C                   MOVEL     BRVNPS        danNPS
260900041014     C                   MOVE      spudfv        dandFV
261000041014     C                   MOVE      BRVFGS        danFGS
261100041018     C                   MOVE      BRVnpg        dannpg
261200041018     C                   MOVE      spgf          danspg
261300041014    2c                   endif
261400041014    1c                   endif
261500101202     c*
261600110908     C                   ENDSR
261700031114     C**************************************************************************
261800031114     C* TERMINAL DI PARTENZA E ARRIVO DEL P.O. EFFETTUA SPUNTA
261900031114     C**************************************************************************
262000031114     C     TERPES        BEGSR
262100031114     C* IMPOSTO TERMINAL PARTENZA E ARRIVO
262200031114     C                   CLEAR                   DSLV55
262300031114     C                   MOVEL     WTITER        D55TPT
262400031114     C                   MOVEL     LNPB          D55LNP
262500031114     C                   MOVEL     WDTRIF        D55DRF
262600031114     C                   MOVE      bpes          D55LIN
262700031114     C                   EXSR      FNLV55
262800031114     C                   ENDSR
262900031114     C**************************************************************************
263000031114     C* CERCO TERMINAL PARTENZA LNP COLLO
263100031114     C**************************************************************************
263200031114     C     TERPAR        BEGSR
263300031114     C                   CLEAR                   DSLV55
263400031114     C                   MOVEL     'P'           D55TPT
263500031114     C                   MOVEL     LNPB          D55LIN
263600031114     C                   MOVEL     WDTRIF        D55DRF
263700031114     C                   EXSR      FNLV55
263800031114     C                   MOVE      D55TFP        LNPTFP
263900031114     C**
264000031114     C                   ENDSR
264100031114     C**************************************************************************
264200031114     C* CONTROLLO COSA POSSO AGGIORNARE IN BASE A CHI SONO
264300031114     C**************************************************************************
264400031114     C     CTRAGG        BEGSR
264500031114     C                   CLEAR                   WAGPAR
264600031114     C                   CLEAR                   WAGARR
264700031114     C**
264800031114     C     LNPB          CHAIN     AZORG01L                           34
264900031114     C**
265000031114     C** CERCO TERMINAL DELLA LINEA DI ARRIVO
265100031114     C                   EXSR      TERARR
265200031114     C* FACCIO PARTENZA DELLA STESSA AREA PARTENZA SE:
265300041013    1C***  BAS           IFEQ      LNPAS
265400031126     c* Se c'e'bolla transito x simfel è transito e non cerco altro
265500041013     c     wesbtt        ifne      'S'
265600031114     C*
265700031114     C*  1) P.O. EFFETTUA SPUNTA = LNP SPUNTA (DA BOLLA)
265800031114    2C     BPES          IFEQ      LNPB
265900031114     C*  2) P.O. EFFETTUA SPUNTA E' IL TER.PARTENZA DELLA LNP SPUNTA
266000031114     C*     IN BASE ALLA DATA SPEDIZIONE SE TROVATA LA BOLLA
266100031114     C     BPES          OREQ      LNPTFP
266200031114     C                   MOVEL     'S'           WAGPAR            1
266300031114   X2C                   ELSE
266400031114     C*  3) TER.PARTENZA P.O. EFFETTUA SPUNTA (IN BASE ALLA DATA SPED
266500031114     C*     SE TROVATA LA BOLLA) = TER.PARTENZA LNP SPUNTA SEMPRE IN
266600031114     C*     BASE ALLA DATA SPEDIZIONE SE TROVATA BOLLA
266700031114     C                   MOVEL     'P'           WTITER            1
266800031114     C                   EXSR      TERPES
266900031114    3C     D55TFP        IFEQ      LNPTFP
267000031114     C                   MOVEL     'S'           WAGPAR            1
267100031114    3C                   ENDIF
267200031114    2C                   ENDIF
267300041013    1C                   ENDIF
267400031114     C**
267500031114     C** NON AGGIORNO BOLLE ARRIVO SE SPUNTA DELL'AREA PARTENZA,
267600031114     C*   SPUNTA TIPICA PARTENZA E P.O. SPUNTA = LNA SPUNTA O TFA SPU
267700031114     c* controllo solo se spunta tipica partenza: l'arrivo non mi
267800031114     c*  interessa. non so perchè lo avevo preso in considerazione!!
267900031114    1C     WAGPAR        IFEQ      'S'
268000031114     C* PARTENZA NON DEFLUENZA
268100031114    3C     parnpg        IFEQ      1
268200031114     C     WVVDEF        ANDEQ     '0'
268300031114     C* ENTRATA
268400031114     C     parnpg        OREQ      5
268500031114     C* I.M.P.
268600031114     C     parnpg        OREQ      3
268700031114     C     wvvspg        ANDEQ     'P'
268800031114     C* I.M.I.
268900031114     C     parnpg        OREQ      3
269000031114     C     wvvspg        ANDEQ     'I'
269100031114     C                   GOTO      NOAGAR
269200031114    3C                   ENDIF
269300031114    2C**                 ENDIF
269400031114    1C                   ENDIF
269500031114     C**
269600031114     c     wesbtt        ifne      'S'
269700031114     C* AGGIORNO BOLLE ARRIVO SE :
269800031114     C*  1) P.O. EFFETTUA SPUNTA = LNA SPUNTA
269900031114    1C                   SELECT
270000031114     C     BPES          WHENEQ    lna
270100031114     C                   MOVEL     'S'           WAGARR            1
270200031114    2C     WAGPAR        IFEQ      'S'
270300031114     C                   MOVEL     'A'           WSPLOC            1
270400031114    2C                   ENDIF
270500031114     C*  2) P.O. EFFETTUA SPUNTA E' IL TER.ARRIVO   DELLA LNA SPUNTA
270600031114     C*     IN BASE ALLA DATA SPUNTA
270700031114     C     BPES          WHENEQ    LNATFA
270800031114     C                   MOVEL     'S'           WAGARR            1
270900031114    2C     WAGPAR        IFEQ      'S'
271000031114     C                   MOVEL     'T'           WSPLOC            1
271100031114    2C                   ENDIF
271200031114   X1C                   OTHER
271300031114     C*  3) TER.ARRIVO P.O. EFFETTUA SPUNTA
271400031114     C*     = TER.ARRIVO LNA SPUNTA IN BASE ALLA DATA SPUNTA
271500031114    2C     PESTFA        IFEQ      LNATFA
271600031114     C                   MOVEL     'S'           WAGARR            1
271700041014    2C     WAGPAR        IFEQ      'S'
271800031114     C                   MOVEL     'D'           WSPLOC
271900031114    2C                   ENDIF
272000041014    2C                   ENDIF
272100031114    1C                   ENDSL
272200031114     c*
272300031114    2C                   endif
272400031114     C     NOAGAR        TAG
272500031114     C* DEVO DETERMINARE SE LE BOLLE DELLA LNA SI TROVANO SULL'AS
272600031114     C*  IN CUI MI TROVO
272700041013     C**   LNAAS         IFEQ      BAS
272800041014     C**                 MOVEL     'S'           WSIBOL            1
272900041013     C**                 ENDIF
273000031114     C**
273100031114     C                   ENDSR
273200031114     C**************************************************************************
273300031114     C* CERCO TERMINAL ARRIVO LNA COLLO
273400031114     C**************************************************************************
273500031114     C     TERARR        BEGSR
273600031114     C**
273700031114     C* LINEA DI ARRIVO
273800031114     C     lna           IFNE      SVVLNA
273900031114     C     LNPB          ORNE      SVVLNP
274000031114     C     wvvdfv        ORNE      SVVDFV
274100031114     C*
274200031114     C* TEMINAL DI ARRIVO
274300031114     C                   CLEAR                   DSLV55
274400031114     C                   MOVEL     ' '           D55TPT
274500031114     C                   MOVEL     wvvdfv        D55DRF
274600031114     C                   MOVEL     lna           D55LIN
274700031114     C                   MOVEL     LNPB          D55LNP
274800031114     C                   EXSR      FNLV55
274900031114     C**
275000031114     C                   MOVEL     D55TFA        LNATFA
275100031114     C                   MOVEL     D55TFP        LNATFP
275200031114     C*
275300031114     C                   MOVEL     lna           SVVLNA
275400031114     C                   MOVEL     lnPB          SVVLNP
275500031114     C                   MOVEL     wvvdfv        SVVDFV
275600031114     C                   ENDIF
275700031114     C                   ENDSR
275800031114     C**************************************************************************
275900031114     C* CALL A PGM FNLV55R
276000031114     C**************************************************************************
276100031114     C     FNLV55        BEGSR
276200031114     C                   CALL      'FNLV55R'
276300031114     C                   PARM                    DSLV55
276400031114     C**
276500031114     C     D55ERR        IFNE      ' '
276600031114     C                   MOVE      D55LIN        D55TFA
276700031114     C                   MOVE      D55LIN        D55TFP
276800031114     C                   END
276900031114     C                   ENDSR
