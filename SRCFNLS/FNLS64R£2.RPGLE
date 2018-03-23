000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200940808     H* FNLS64R *----------------------------------------------------*
000300940808      *      VISUALIZZAZIONE DETTAGLIO SEGNACOLLI BOLLE PARTENZE
000400000000     H*--------------------------------------------------------------*
000500940808     FFNLS64D   CF   E             WORKSTN
000600940808     F                                     SFILE(LS64S02:NRR2)
000700910219     FAZORG01L  IF   E           K DISK
000800940808     FFNBLT01L  IF   E           K DISK
000900041028     FFNarT01L  IF   E           K DISK
001000940808     FFNBLP01L  IF   E           K DISK
001002140729     FFNlbl02L  IF   E           K DISK    rename(fnlbl000:fnlbl002)
001100940808     FTABEL00F  IF   E           K DISK
001200051013     FFIARS01L  IF   E           K DISK
001300940323      *
001400111020     D**!!!CLI             S              7  0 DIM(300)
001500970414     D* Schiera codici consegne anomale + relativi flag
001600970414     D C7O             S              1    DIM(200)
001700970414     D F7O             S              1    DIM(200)
001800020222     D C               S             35    DIM(10)
001900020222     D D               S             11    DIM(10)
002000120621     D chkart          S             35    DIM(1) CTDATA PERRCD(1)
002100940323      *
002200950116     D* PASSAGGIO DATI PER PGM
002300900523     D PARAM           DS
002400940808     D  PARAAS                 1      4  0
002500940808     D  PARLNP                 5      7  0
002600940808     D  PARNRS                 8      9  0
002700940808     D  PARNSP                10     16  0
002800940808     D  PARF03                17     17
002900940808     D  PARFLG                18     18
003000940808     D  PARPGM                19     28
003100940808     D  PARDSA                29     48
003200940808     D  PARCML                49     49
003300940808      * PARCLI indica se il pgm e' richiamato da un cliente (='S')
003400111020      * OBSOLETO --> non ci sono più clienti con DDM <-- 20/10/2011
003500111020     D**!!!PARCLI                50     50
003600950116     D* RICHIAMO PGM DI INTERROGAZIONE SPUNTE    -FNLR04R-
003700941230     D PARAM1          DS                  INZ
003800940808     D  PA1NFV                 1      5  0
003900940808     D  PA1LNP                 6      8  0
004000940808     D  PA1LNA                 9     11  0
004100940808     D  PA1NRS                12     13  0
004200940808     D  PA1NCD                14     20  0
004300940808     D  PA1NCA                21     27  0
004400940808     D  PA1NPS                28     29  0
004500940808     D  PA1NPG                30     30  0
004600940808     D  PA1FLG                31     31
004700940808     D  PA1PGM                32     38
004800941230     D  PA1FGS                39     41  0
004900101203     d* bolla
005000101203     d  pa1bolp               42     44  0
005100101203     d  pa1bors               45     46  0
005200101203     d  pa1bosp               47     53  0
005300101203     d  pa1boan               54     57  0
005400101203     D  Pa1ERR                58     58
005500101203     d
005600120621     D fnlr38ds        DS
005700120621     D  P38AAS                14     17  0
005800120621     D  P38LNP                18     20  0
005900120621     D  P38NRS                21     22  0
006000120621     D  P38NSP                23     29  0
006100120621     D  P38F03                30     30
006200120621     D  P38FLG                31     31
006300120621     D  P38PGM                32     41
006400120621     D  P38DSA                42     61
006500120621     D  P38DSP                63     70  0
006600120621     D  P38RMN                71     85  0
006700120621     D  P38LNA                86     88  0
006800120621     D  P38dlna               89    108
006900120621     D  P38RSc               109    143
007000120621     D  P38Tcli              153    153
007100120621     D  P38cod               154    160
007200120621     d
007300940808     D WLBDAT          DS
007400940808     D  G02DAT                 1      8  0
007500940808     D  G02INV                 9     16  0
007600940808     D  G02ERR                17     17
007700940808     D  G02TGI                18     22  0
007800910806     D                 DS
007900940808     D  BLPAAS                 1      4  0
008000940808     D  BLPMGS                 5      8  0
008100940808     D  BLPDSP                 1      8  0
008200900517     D KPJBA         E DS
008300111020     D**!!!DS3K          E DS
008400120711     D DS3E          E DS
008500120711     D DS7O          E DS
008600000911     D DS7H          E DS
008700020220     D OG143         E DS
008800111020
008900111020     d wshdeb          s                   like(VSCDEB)
009000111020     d wsheex          s                   like(VSCEEX)
009100111020
009200940808      * $FORFI e' il nome del file video
009300940808     D                 DS
009400940808     D  $FORFI                 1     10    INZ('FNLS64D   ')
009500041028     D  wcan                               like(bltcan)
009600041028     D  wdet                               like(bltdet)
009700041028     D  wdut                               like(bltdut)
009800041028     D  wdam                               like(bltdam)
009900041028     D  wdcm                               like(bltdcm)
010000041028     D  wfl1                               like(bltfl1)
010100000000     C*---------------------------------------------------------------*
010200000000     C     *ENTRY        PLIST
010300000000     C                   PARM                    KPJBA
010400940808     C                   MOVEL     KPJBU         PARAM
010500940808     C*
010600940808     C                   MOVEL     PARDSA        V1CDSA
010700930113     C                   Z-ADD     1             CODUT             1 0
010800900517     C*
010900930113     C* SE FILIALE HA CML VISUALIZZO VOLUME ALTRIMENTI NO
011000040121      * 21/01/04 VISUALIZZO SEMPRE IL VOLUME
011100040121     C**         PARCML    IFEQ 'S'
011200930113     C                   SETON                                        60
011300040121     C***                  END
011400940323      *
011500940808      * Testo il flag che mi dice se sono richiamato da un cliente
011600111020     C**!!!              SETOFF                                       32
011700111020     C**!!!PARCLI        IFEQ      'S'
011800940808      * Testo le abilitazioni del cliente
011900111020     C**!!!              EXSR      ABICLI
012000111020     C**!!!$3KINB        CABNE     'S'           FINE
012100111020     C**!!!              SETON                                        32
012200111020     C**!!!              ENDIF
012300930113     C*
012400921013     C* IMPOSTA CAMPI PER INT SPUNTE
012500940808     C                   Z-ADD     0             PA1NFV
012600950116     C                   MOVEL     *ZEROS        PA1LNP
012700940808     C                   MOVEL     *ZEROS        PA1LNA
012800940808     C                   MOVEL     PARNRS        PA1NRS
012900101203     C                   MOVEL     PARNRS        pa1bors
013000101203     C                   MOVEL     PARlnp        pa1bolp
013100101203     C                   MOVEL     PARnsp        pa1bosp
013200101203     C                   MOVEL     PARaas        pa1boan
013300941230     C                   MOVEL     9             PA1NPG
013400940808     C                   Z-ADD     0             PA1NCD
013500940808     C                   Z-ADD     0             PA1NCA
013600940808     C                   MOVEL     99            PA1NPS
013700940808     C                   MOVEL     'FNLS64R'     PA1PGM
013800940808     C                   MOVEL     '1'           PA1FLG
013900910806     C   30              GOTO      FINE
014000920422     C*
014100920422     C* CHAIN PER REPERIRE BOLLA
014200940808     C     KBLT1         CHAIN     FNBLP01L                           30
014300940808     C                   MOVEL     BLPKSC        VCCKSC
014400940808     C                   MOVEL     BLPRSM        VCCRSM
014500940808     C                   MOVEL     BLPLNA        VCCLNA
014600940808     C                   MOVEL     BLPRMN        VCCRMN
014601140729     c* Verifico se è bolla mamma:  in questo caso nonprendo i dati da ARB
014602140729     c                   clear                   wlegami           1
014607140729     c     kblt1         setll     fnlbl02l
014608140729     c                   if        %equal(fnlbl02l)
014609140729     c                   eval      wlegami='S'
014611140729     c                   endif
014700910219     C*
014800910220     C* DECODIFICA LINEA DI ARRIVO
014900910806     C     BLPLNA        CHAIN     AZORG                              30
015000940808     C  N30              MOVEL     ORGDES        VCDLNA
015100940808     C   30              MOVEL     *BLANKS       VCDLNA
015200020220     C  N30              MOVEL     ORGDE3        OG143
015300940808     C                   Z-ADD     BLPLNA        PA1LNA
015400910806     C* GIRO DATA SPEDIZIONE
015500940808     C                   Z-ADD     BLPDSP        G02INV
015600940808     C                   Z-ADD     *ZEROS        G02DAT
015700940808     C                   MOVEL     '3'           G02ERR
015800940808     C                   CALL      'XSRDA8'
015900940808     C                   PARM                    WLBDAT
016000940808     C                   Z-ADD     G02DAT        VCCDSP
016100020222     C                   MOVE      *OFF          WF11              1
016200910219     C*
016300910806     C* CARICO RIGHE DI DETTAGLIO SEGNACOLLI FLBLT
016400910806     C                   EXSR      CARBLT
016500910219     C*
016600940808     C                   MOVEL     'S'           WRIEM             1
016700921013     C     FOR01         TAG
016800940808     C     WRIEM         IFEQ      'S'
016900940808     C                   WRITE     LS64T01
017000990416     C**
017100020221      * SE HO CARICATO TRE RIGHE ATTIVO TASTO F11
017200020221      * CON DESCRIZIONE DIVERSA A SECONDA CHE ABBIA CARICATO
017300020222      * IN SECONDA RIGA TIPO RECORD 'F' O 'E'.
017400020222      * SOLO SE NON PROVENGO DA F11
017500020221     C     WRIGHE        IFEQ      3
017600020222     C     WF11          ANDEQ     *OFF
017700020221     C     §OGNTW        IFEQ      'FED'
017800020221     C                   SETON                                        35
017900020221     C                   SETOFF                                       36
018000020221     C                   ELSE
018100020221     C                   SETOFF                                       35
018200020221     C                   SETON                                        36
018300020221     C                   ENDIF
018400020221     C                   ENDIF
018500940808     C                   WRITE     LS64D02
018600990416     C                   MOVEL     *BLANKS       WRIEM
018700990416     C                   ENDIF
018800940808     C*
018900990416     C**         'LS64C02 'CAT  '  ':0    FORREC           FORMATO RECORD
019000990416     C**                   EXSR BYTE                       CONTA BYTE
019100990416     C**
019200940808     C                   EXFMT     LS64C02
019300020222     C                   SETOFF                                       11
019400940808     C*
019500940808     C   KC              MOVEL     '1'           PARF03
019600921013     C*
019700921013     C* CMD19 - INTERROGAZIONE SPUNTE
019800940808     C   KT              DO
019900940808     C                   MOVEL     PARAM1        KPJBU
020000940808     C                   CALL      'FNLR04R'
020100921013     C                   PARM                    KPJBA
020200940808     C                   MOVEL     'S'           WRIEM             1
020300940808     C                   GOTO      FOR01
020400940808     C                   ENDDO
020500020222     C*
020600020222     C* CMD11 - CAMBIO VISUALIZZAZIONE RECORD TIPO 'F'/'E'
020700020222     C   KK              DO
020800020222     C     WTRC          IFEQ      'E'
020900020222     C     WTRC          OREQ      'F'
021000020222     C                   SETON                                        11
021100020222     C                   EXSR      UPDSFL
021200020222     C                   GOTO      FOR01
021300020222     C                   ENDIF
021400020222     C                   ENDDO
021500120621     c*
021600120621     c* F21 - nuovo dett colli
021700120621     c                   if        *inkv
021800120621     c                   clear                   fnlr38ds
021900120621     c                   eval      p38aas=paraas
022000120621     c                   eval      p38lnp=parlnp
022100120621     c                   eval      p38nrs=parnrs
022200120621     c                   eval      p38nsp=parnsp
022300120621     c                   eval      p38pgm=parpgm
022400120621     c                   eval      p38dsa=pardsa
022500121106     c                   eval      p38dsp=vccdsp
022600120621     c                   eval      p38rmn=blprmn
022700120621     c                   eval      p38lna=blplna
022800120621     c                   eval      p38dlna=vcdlna
022900120621     c                   eval      p38rsc =blprsm
023000120621     c                   if        blpccm>0
023100120621     c                   eval      p38cod =%editc(blpccm:'X')
023200120621     c                   else
023300120621     c                   eval      p38cod =%editc(blpksc:'X')
023400120621     c                   endif
023500120621     c                   eval      p38tcli='P'
023600120621     c                   movel     fnlr38ds      kpjbu
023700120621     c                   call      'FNLR38R'
023800120621     c                   parm                    kpjba
023900120621     c
024000120621     C                   MOVE      'S'           WRIEM
024100120621     c                   goto      for01
024200120621     C                   END
024300910219     C*
024400000000     C     FINE          TAG
024500950126     C                   MOVEL     PARAM         KPJBU
024600000000     C                   SETON                                        LR
024700940808     C**************************************************************************
024800940808     C* CARICO DETTAGLIO SEGNACOLLI
024900940808     C**************************************************************************
025000910806     C     CARBLT        BEGSR
025100940808     C*
025200910205     C                   EXSR      PULSF
025300020222     C                   RESET                   C
025400020222     C                   RESET                   D
025500940808     C                   Z-ADD     0             NRR2              4 0
025600910806     C                   SETOFF                                       30
025700940808     C     KBLT1         SETLL     FNBLT01L
025800940808     C     KBLT1         READE     FNBLT01L                               30
025900940808     C  N30              MOVEL     BLTNSC        PA1NCD
026000950116     C  N30              MOVEL     BLTFLS        PA1LNP
026100910220     C*
026200910806     C     *IN30         DOWEQ     '0'
026300940808     C*
026400941219     C                   Z-ADD     BLTFLS        VSCFLS
026500941219     C                   Z-ADD     BLTNSC        VSCNSC
026600041028     c
026700041028     C* CHAIN ANCHE IN FNART PER PRENDERE I DATI DALL'ARRIVO SE C'E' FILE
026900041028     C                   CLEAR                   WESART            1
027000041028     C     KART1         chain     FNART01L
027100041028     C                   IF        %FOUND(FNART01L)
027200041028     C                   EVAL      WESART='S'
027300041028     C                   ELSE
027400041028     C                   EVAL      WESART='N'
027500041028     C                   ENDIF
027600920811     C* CODICE ANOMALIA
027700920811     C                   MOVEL     *BLANKS       DESCAN
027800041028     c                   clear                   wcan
027900041028     c                   if        bltcan<>*blanks
028000041028     c                   eval      wcan=bltcan
028100041028     c                   else
028200041028     c                   if        wesart='S' and artcan<>*blanks
028300041028     c                   eval      wcan=artcan
028400041028     c                   endif
028500041028     c                   endif
028600041028     c
028700041028     C     WCAN          IFNE      ' '
028800941219     C                   MOVEL     '3E'          KCOD
028900041028     C                   MOVEL     WCAN          KKEY
029000940808     C     KTAB1         CHAIN     TABEL00F                           31
029100120711     c  n31              movel     tbluni        ds3e
029200120711     c   31              clear                   ds3e
029300120711     C                   MOVEL     §3eDE12       ALF06             6
029400920811     C                   MOVEL     ' -'          DESCAN
029500041028     C                   MOVEL     WCAN          DESCAN
029600041028     C                   MOVE      ALF06         DESCAN
029700920811     C                   END
029800910912     C* GIRO DATE:
029900041028     C* SPUNTA ENTRATA
030000041028     C                   Z-ADD     BLTDSE        G02INV
030100041028     C                   Z-ADD     *ZEROS        G02DAT
030200041028     C                   MOVEL     '3'           G02ERR
030300041028     C                   CALL      'XSRDA8'
030400041028     C                   PARM                    WLBDAT
030500041028     C                   MOVEL     G02DAT        VSCDSE
030600041028     C     BLTDSE        IFGT      0
030700041028     C                   MOVEL     BLTNPE        VSCNPE
030800041028     C                   ELSE
030900041028     C                   MOVEL     '  '          VSCNPE
031000041028     C                   END
031100910912     C* PARTENZA MERCE
031200940808     C                   Z-ADD     BLTDFV        G02INV
031300940808     C                   Z-ADD     *ZEROS        G02DAT
031400940808     C                   MOVEL     '3'           G02ERR
031500940808     C                   CALL      'XSRDA8'
031600940808     C                   PARM                    WLBDAT
031700950116     C                   MOVEL     G02DAT        VSCDFV
031800950116     C                   MOVE      G02DAT        DUE               2 0
031900950116     C                   MOVE      DUE           VSCDFV
032000930108     C     BLTDFV        IFGT      0
032100940808     C                   MOVEL     BLTNPP        VSCNPP
032200930108     C                   ELSE
032300940808     C                   MOVEL     '  '          VSCNPP
032400930108     C                   END
032500041028     C* ARRIVO e partenza  TRANSITO
032600041028     c                   clear                   wdut
032700041028     c                   clear                   wdet
032800041028     c                   clear                   vscdut
032900041028     c                   clear                   vscdet
033000041028     c                   clear                   vscflp
033100041028     c
033200041028     c                   if        wesart='S'
033300041028     c                   eval      wdet=artdet
033400041028     c                   eval      wdut=artdut
033500041028     c                   if        artflp>0
033600041028     c                   movel     artflp        vscflp
033700041028     c                   endif
033800041028     c
033900041028     c                   else
034000041028     c                   eval      wdet=bltdet
034100041028     c                   eval      wdut=bltdut
034200041028     c                   endif
034300041028     c
034400041028     c                   if        wdet>0
034500041028     C                   Z-ADD     wdet          G02INV
034600940808     C                   Z-ADD     *ZEROS        G02DAT
034700940808     C                   MOVEL     '3'           G02ERR
034800940808     C                   CALL      'XSRDA8'
034900940808     C                   PARM                    WLBDAT
035000941219     C                   MOVEL     G02DAT        VSCDET
035100041028     c                   endif
035200041028     c
035300041028     c* Se non c'e' vero transito, visualizzo da blp il suo scarico
035400041028     c*  cioè il terminal di arrivo
035500041028     c                   if        vscflp=*blanks
035600041028     C     BLTFLP        IFNE      0
035700041028     C     BLTFLP        ANDNE     BLTLNA
035800041028     C                   MOVEL     BLTFLP        VSCFLP
035900041028     C                   END
036000041028     c                   endif
036100041028     c
036200041028     C*
036300041028     c                   if        wdut>0
036400041028     C                   Z-ADD     wdut          G02INV
036500940808     C                   Z-ADD     *ZEROS        G02DAT
036600940808     C                   MOVEL     '3'           G02ERR
036700940808     C                   CALL      'XSRDA8'
036800940808     C                   PARM                    WLBDAT
036900941219     C                   MOVEL     G02DAT        VSCDUT
037000041028     c                   endif
037100041028     c
037200910912     C* ARRIVO MERCE ALLA FIL ARRIVO
037300041028     c                   clear                   wdam
037400041028     c                   clear                   vscdam
037500041028     c                   clear                   vscnap
037600140729     c                   if        wesart='S' and wlegami=' '
037700041028     c                   eval      wdam=artdam
037800041103     c                   if        artdam>0
037900041028     c                   movel     artnap        vscnap
038000041103     c                   endif
038100041103     c
038200041028     c                   else
038300041028     c                   eval      wdam=bltdam
038400041028     c                   endif
038500041028     c
038600041028     c                   if        wdam>0
038700041028     C                   Z-ADD     wdam          G02INV
038800940808     C                   Z-ADD     *ZEROS        G02DAT
038900940808     C                   MOVEL     '3'           G02ERR
039000940808     C                   CALL      'XSRDA8'
039100940808     C                   PARM                    WLBDAT
039200950116     C                   MOVEL     G02DAT        VSCDAM
039300041028     c                   endif
039400910912     C* CONSEGNA MERCE REALE
039500041028     c                   clear                   wdcm
039600041028     c                   clear                   vscdcm
039700041028     C                   MOVEL     *BLANKS       V2CIDD
039800041028     c
039900140729     c                   if        wesart='S'  and wlegami=' '
040000041028     c                   eval      wdcm=artdcm
040100041028     c                   eval      wfl1=artfl1
040200041028     c                   else
040300041028     c                   eval      wdcm=bltdcm
040400041028     c                   eval      wfl1=bltfl1
040500041028     c                   endif
040600041028     c
040700041028     c                   if        wdcm>0
040800041028     C                   Z-ADD     wdcm          G02INV
040900940808     C                   Z-ADD     *ZEROS        G02DAT
041000940808     C                   MOVEL     '3'           G02ERR
041100940808     C                   CALL      'XSRDA8'
041200940808     C                   PARM                    WLBDAT
041300941219     C                   MOVEL     G02DAT        VSCDCM
041400941219     C                   MOVE      G02DAT        DUE
041500941219     C                   MOVE      DUE           VSCDCM
041600041028     c
041700041028     C* SEGNALO SE SEGNACOLLO IN IDD
041800041028     C                   Z-ADD     1             X
041900041028     C     wfl1          LOOKUP    C7O(X)                                 34
042000041028     C     *IN34         IFEQ      '1'
042100041028     C     F7O(X)        ANDNE     *BLANKS
042200041028     C                   MOVEL     'IDD'         V2CIDD
042300041028     C                   END
042400041028     c                   endif
042500930113     C*
042600930113     C* CONTROLLO SE VOLUME INSERITO MANUALMENTE
042700940808     C                   MOVEL     *BLANKS       VSDFVC
042800041028     c                   clear                   vscvuc
042900041028     c* Per peso e volume inverto:
043000041028     c*  se c'e' lo prendo dalla partenza, altrimenti dall'arrivo
043100990913     C     BLTVUC        IFGT      *ZERO
043200990913     C                   MOVE      BLTFVC        VSDFVC
043300041028     C                   Z-ADD     BLTVUC        VSCVUC
043400041028     c                   else
043500041028     c                   if        wesart='S' and artvuc>0
043600041028     C                   MOVE      artFVC        VSDFVC
043700041028     C                   z-add     artvuc        VScvuc
043800041028     c                   endif
043900990913     C                   END
044000041028     c
044100990913     C                   MOVEL     *BLANKS       VSDFPC
044200041028     c                   clear                   vscpuc
044300990913     C     BLTPUC        IFGT      *ZERO
044400990913     C                   MOVE      BLTFPC        VSDFPC
044500041028     C                   Z-ADD     BLTPUC        VSCPUC
044600041028     c                   else
044700041028     c                   if        wesart='S' and artpuc>0
044800041028     C                   MOVE      artFpC        VSDFpC
044900041028     C                   Z-ADD     arTPUC        VSCPUC
045000041028     c                   endif
045100930113     C                   END
045200020222      *
045300020222     C                   ADD       1             NRR2
045400000911     C* VISUALIZZO ESTENSIONE SEGNACOLLI (PER NON RIFARE IL SUBFILE
045500000911     C* SI CONSIDERA CHE AL MOMENTO VI SIANO SOLO I TIPI RECORD "C" E "E"
045600000911     C                   CLEAR                   VSCDEB
045700000911     C                   CLEAR                   VSCDE2
045800000911     C                   CLEAR                   VSCEEX
045900000911     C                   CLEAR                   VSCEE2
046000020226     C                   Z-ADD     0             WRIGHE            1 0
046100000911     C                   MOVE      'C'           WTRC
046200051013     C     KarS1         CHAIN     Fiars01L                           30
046300000911     C     *IN30         IFEQ      *OFF
046400020226     C                   Z-ADD     1             WRIGHE
046500000911     C                   MOVE      '7H'          KCOD
046600000911     C                   MOVEL(P)  WTRC          KKEY
046700000911     C     KTAB1         CHAIN     TABEL00F                           30
046800000911     C  N30              MOVEL     TBLUNI        DS7H
046900000911     C  N30              MOVEL     §7HDEB        VSCDEB
047000051013     C                   MOVEL     arSNOT        VSCEEX
047100000911     C                   ENDIF
047200020220      * A SECONDA DELLA LINEA DI ARRIVO SCELGO SE VISUALIZZARE
047300020220      * PRIMA EUREXPRES O FEDEX
047400020220     C     §OGNTW        IFEQ      'FED'
047500020220     C                   MOVE      'F'           WTRC
047600020220     C                   ELSE
047700000911     C                   MOVE      'E'           WTRC
047800020220     C                   ENDIF
047900051013     C     KarS1         CHAIN     FiarS01L                           30
048000000911     C     *IN30         IFEQ      *OFF
048100000911     C                   MOVE      '7H'          KCOD
048200000911     C                   MOVEL(P)  WTRC          KKEY
048300000911     C     KTAB1         CHAIN     TABEL00F                           30
048400000911     C  N30              MOVEL     TBLUNI        DS7H
048500020226     C     WRIGHE        IFEQ      0
048600000911     C  N30              MOVEL     §7HDEB        VSCDEB
048700051013     C                   MOVEL     arSNOT        VSCEEX
048800020226     C                   Z-ADD     1             WRIGHE
048900020226     C                   ELSE
049000020226     C     WRIGHE        IFEQ      1
049100020226     C                   Z-ADD     2             WRIGHE
049200000911     C  N30              MOVEL     §7HDEB        VSCDE2
049300051013     C                   MOVEL     arSNOT        VSCEE2
049400000911     C                   ENDIF
049500020226     C                   ENDIF
049600020226     C                   ENDIF
049700020220      *
049800020222     C     §OGNTW        IFEQ      'FED'
049900020220     C                   MOVE      'E'           WTRC
050000020220     C                   ELSE
050100020220     C                   MOVE      'F'           WTRC
050200020220     C                   ENDIF
050300051013     C     KarS1         CHAIN     FiarS01L                           30
050400020220     C     *IN30         IFEQ      *OFF
050500020220     C                   MOVE      '7H'          KCOD
050600020220     C                   MOVEL(P)  WTRC          KKEY
050700020220     C     KTAB1         CHAIN     TABEL00F                           30
050800020220     C  N30              MOVEL     TBLUNI        DS7H
050900020220     C                   SELECT
051000020226     C     WRIGHE        WHENEQ    0
051100020220     C  N30              MOVEL     §7HDEB        VSCDEB
051200051013     C                   MOVEL     arSNOT        VSCEEX
051300020226     C                   Z-ADD     1             WRIGHE
051400020226     C     WRIGHE        WHENEQ    1
051500020220     C  N30              MOVEL     §7HDEB        VSCDE2
051600051013     C                   MOVEL     arSNOT        VSCEE2
051700020226     C                   Z-ADD     2             WRIGHE
051800020226     C     WRIGHE        WHENEQ    2
051900020222      * MI SALVO SU CAMPI IN SCHIERA DELLA TERZA RIGA
052000020222     C  N30              MOVEL     §7HDEB        D(NRR2)
052100051013     C                   MOVEL     arSNOT        C(NRR2)
052200020226     C                   Z-ADD     3             WRIGHE
052300020220     C                   ENDSL
052400020220     C                   ENDIF
052500910912     C*
052600910205     C* SCRIVO SUBFILE
052700940905     C  N20              SETON                                        20
052800940808     C                   WRITE     LS64S02
052900910912     C*
053000940808     C     KBLT1         READE     FNBLT01L                               30
053100910205     C                   END
053200910205     C*
053300940808     C                   Z-ADD     NRR2          SAVNRR            4 0
053400921013     C* SE PIU' DI UN COLLO MEMORIZZO ANCHE AL
053500940808     C     BLTNSC        IFNE      PA1NCD
053600940808     C                   MOVEL     BLTNSC        PA1NCA
053700921013     C                   END
053800921104     C*
053900940808     C     NRR2          IFEQ      0
054000970321     C                   MOVEL     *BLANKS       V2CIDD
054100940808     C                   Z-ADD     0             VSCNSC
054200940808     C                   Z-ADD     0             VSCDSE
054300940808     C                   Z-ADD     0             VSCDFV
054400940808     C                   MOVEL     *BLANKS       VSCFLP
054500940808     C                   MOVEL     *BLANKS       VSCNPP
054600940808     C                   MOVEL     *BLANKS       VSCNPE
054700940808     C                   Z-ADD     0             VSCDET
054800940808     C                   Z-ADD     0             VSCDUT
054900940808     C                   Z-ADD     0             VSCDAM
055000940808     C                   Z-ADD     0             VSCDCM
055100940808     C                   Z-ADD     0             VSCVUC
055200990909     C                   Z-ADD     0             VSCPUC
055300921104     C                   MOVEL     *BLANKS       DESCAN
055400940808     C                   ADD       1             NRR2
055500940808     C                   WRITE     LS64S02
055600921104     C                   END
055700910205     C*
055800910205     C                   ENDSR
055900020222     C**************************************************************************
056000020222     C* AGGIORNO SFL PER ACMBIO VISUALIZZAZIONE TIPO RECORD F/E
056100020222     C**************************************************************************
056200020222     C     UPDSFL        BEGSR
056300020222      * RILEGGO IL SFL E SCAMBIO LE RIGHE
056400020222     C     WRIGHE        IFEQ      3
056500020222     C                   MOVE      *ON           WF11
056600020222     C                   Z-ADD     1             NRR2
056700020222     C     NRR2          CHAIN     LS64S02                            31
056800020222     C     *IN31         DOWEQ     *OFF
056900020222     C* VISUALIZZO VOLUME
057000020222     C                   MOVEL     VSCDE2        WSHDEB
057100020222     C                   MOVEL     VSCEE2        WSHEEX
057200020222     C                   MOVEL     D(NRR2)       VSCDE2
057300020222     C                   MOVEL     C(NRR2)       VSCEE2
057400020222     C                   MOVEL     WSHDEB        D(NRR2)
057500020222     C                   MOVEL     WSHEEX        C(NRR2)
057600020222     C                   UPDATE    LS64S02
057700020222     C                   ADD       1             NRR2
057800020222     C     NRR2          CHAIN     LS64S02                            31
057900020222     C                   ENDDO
058000020226      *
058100020222     C     *IN35         IFEQ      *ON
058200020222     C                   SETOFF                                       35
058300020222     C                   SETON                                        36
058400020226     C                   ELSE
058500020226     C     *IN36         IFEQ      *ON
058600020222     C                   SETOFF                                       36
058700020222     C                   SETON                                        35
058800020222     C                   ENDIF
058900020226     C                   ENDIF
059000020222     C                   MOVE      'S'           WRIEM
059100020222     C                   ENDIF
059200020222      *
059300020222     C                   ENDSR
059400940808     C**************************************************************************
059500940808     C* PULIZIA SUBFILE
059600940808     C**************************************************************************
059700910205     C     PULSF         BEGSR
059800940323      *
059900990416     C**         'LS64C02 'CAT  '  ':0    FORREC           FORMATO RECORD
060000990416     C**                   EXSR BYTE                       CONTA BYTE
060100940323      *
060200940808     C                   SETOFF                                       202123
060300940808     C                   WRITE     LS64C02
060400940808     C                   SETON                                        2123
060500940808     C*
060600910205     C                   ENDSR
060700940808      **************************************************************************
060800940808      * Nel caso di interrogazione da parte di un cliente, viene verificato se
060900940808      * egli puo' interrogare le bolle e se deve essere conteggiato il traffico
061000940808      * dei byte sulla linea di comunicazione.
061100940808      * Queste informazioni sono contenute nella tabella 3K.
061200940808      *
061300940808      * Inoltre, egli puo' interrogare solo alcuni clienti (quelli a lui
061400940808      * unificati.
061500940808      **************************************************************************
061600111020     C**!!!ABICLI        BEGSR
061700940808      *
061800111020     C**!!!*LIKE         DEFINE    §3KINB        $3KINB
061900090603     C**   *LIKE         DEFINE    §3KCTB        $3KCTB
062000111020     C**!!!*LIKE         DEFINE    VSCDEB        WSHDEB
062100111020     C**!!!*LIKE         DEFINE    VSCEEX        WSHEEX
062200111020     C**!!!              MOVE      *BLANKS       $3KINB
062300090603     C**                 MOVE      *BLANKS       $3KCTB
062400940808      *
062500940808      * Leggo tutta la tabella 3K riempendo una schiera con i cod.clienti
062600111020     C**!!!              Z-ADD     *ZERO         $PRIMA            1 0
062700111020     C**!!!              Z-ADD     *ZERO         $A                3 0
062800111020     C**!!!              MOVE      '3K'          KCOD
062900111020     C**!!!KTAB2         SETLL     TABEL00F
063000111020     C**!!!              DO        *HIVAL
063100111020     C**!!!KTAB2         READE     TABEL00F                               90
063200111020     C*!N90TBLFLG        IFEQ      *BLANKS
063300111020     C**!!!              MOVEL     TBLUNI        DS3K
063400111020     C**!!!KNMUS         IFEQ      §3KPRF
063500940808      *
063600940808      * Devo prendere il cod.cliente, che e' la chiave di TABEL
063700111020     C**!!!              MOVEL     TBLKEY        $CODIC            7 0
063800111020     C**!!!$CODIC        LOOKUP    CLI                                    99
063900111020     C*!N99$A            IFLT      300
064000111020     C**!!!              ADD       1             $A
064100111020     C**!!!              Z-ADD     $CODIC        CLI($A)
064200111020     C**!!!              ENDIF
064300940808      *
064400940808      * Gia' dalla prima lettura ho i due flag che abilitano il profilo
064500111020     C**!!!$PRIMA        IFEQ      *ZERO
064600111020     C**!!!              MOVE      §3KINB        $3KINB
064700090603     C**                 MOVE      §3KCTB        $3KCTB
064800111020     C**!!!§3KINB        COMP      'S'                                9090
064900111020     C**!!!              Z-ADD     1             $PRIMA
065000111020     C**!!!              ENDIF
065100940808      *
065200111020     C**!!!              ENDIF
065300111020     C**!!!              ENDIF
065400111020     C*!N90              ENDDO
065500940808      *
065600940808      *
065700111020     C**!!!              ENDSR
065800940808      **************************************************************************
065900940808      *    R O U T I N E     I N I Z I A L E
066000940808      **************************************************************************
066100940808     C     *INZSR        BEGSR
066200940808      *
066300940808     C* ACCESSO   FLBLT
066400940808     C     KBLT1         KLIST
066500940808     C                   KFLD                    PARAAS
066600940808     C                   KFLD                    PARLNP
066700940808     C                   KFLD                    PARNRS
066800940808     C                   KFLD                    PARNSP
066900041028     C     KART1         KLIST
067000041028     C                   KFLD                    PARAAS
067100041028     C                   KFLD                    PARLNP
067200041028     C                   KFLD                    PARNRS
067300041028     C                   KFLD                    PARNSP
067400041028     C                   KFLD                    BLTNSC
067500940808     C     KTAB1         KLIST
067600940808     C                   KFLD                    CODUT
067700940808     C                   KFLD                    KCOD
067800940808     C                   KFLD                    KKEY
067900940808     C     KTAB2         KLIST
068000940808     C                   KFLD                    CODUT
068100940808     C                   KFLD                    KCOD
068200051013     C     KarS1         KLIST
068300980316     C                   KFLD                    BLTFLS
068400980316     C                   KFLD                    BLTLNA
068500980316     C                   KFLD                    BLTNRS
068600980316     C                   KFLD                    BLTNSC
068700980316     C                   KFLD                    WTRC
068800940808      *
068900940808     C     *LIKE         DEFINE    TBLCOD        KCOD
069000940808     C     *LIKE         DEFINE    TBLKEY        KKEY
069100051013     C     *LIKE         DEFINE    arSTRC        WTRC
069200970414     C*
069300970414     C* Eseguo caricamento della tabella dei codici consegna anomala
069400970414     C                   Z-ADD     0             X                 3 0
069500970414     C                   Z-ADD     1             CODUT
069600970414     C                   MOVEL     '7O'          KCOD
069700970414     C     KTAB2         CHAIN     TABEL                              34
069800970414     C     *IN34         DOWEQ     '0'
069900970414     C     X             ANDLT     100
070000970414     C     TBLFLG        IFEQ      *BLANKS
070100970414     C                   ADD       1             X
070200970414     C                   MOVEL     TBLKEY        C7O(X)
070300970414     C                   MOVEL     TBLUNI        DS7O
070400970414     C                   MOVEL     §7OIDD        F7O(X)
070500970414     C                   END
070600970414     C     KTAB2         READE     TABEL                                  34
070700970414     C                   END
070800120621     c
070900120621     c* controllo se esiste nuovo file FIART
071000120621     c                   eval      comman=chkart(1)
071100120621     c                   eval      lengh=35
071200120621     c                   call      'QCMDEXC'                            99
071300120621     c                   parm                    comman           80
071400120621     c                   parm                    lengh            15 5
071500120621     c
071600120621     c                   if        not *in99  and %subst(knmus:1:3)='EDP'
071700120621     c                   seton                                        18
071800120621     c                   endif
071900120621     C*
072000940808     C                   ENDSR
072100120621**
072200120621CHKOBJ OBJ(FIART01L) OBJTYPE(*FILE)
