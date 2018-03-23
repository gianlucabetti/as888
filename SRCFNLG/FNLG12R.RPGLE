000100050930     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200060323
000300060302     Ffnlg12d   CF   E             WORKSTN
000400050930     F                                     SFILE(S01:S01RRN)
000500060302     Ftigcp02l  IF   E           K DISK
000600060801     Ftignp01l  IF   E           K DISK
000700060324     Ftitas30c  IF   E           K DISK    usropn
000800060324     Ftita730c  IF   E           K DISK    usropn
000900060302     Ftabel00f  IF   E           K DISK
001000050930
001100050930     D kpjba         E DS
001200060302     D fnlg12ds      E DS
001300060323     D fnlg13ds      E DS
001400060302     D ds2a          E DS
001500060324
001600060324     D spese           S                   like(gcpasg)
001700060324     D porto           S                   like(gcpnpc)
001800060324     D varia           S                   like(tassv1)
001900060324     D prima           S              1
002000050930
002100050930     D S01RRN          S              5I 0
002200050930     D S01RRNFirst     S              5I 0
002300050930     D S01RRNLast      S              5I 0
002400060324
002500060324     d cmd             s             50    dim(02) ctdata perrcd(1)
002600060324     d cmdp            s             50    dim(02) ctdata perrcd(1)
002700060324     d comman          s             80
002800060324     d lenght          s             15  5 inz(80)
002900060302      *--------------------------------------------------------------------
003000060302     c                   exsr      elabora
003100060330     c                   movel(p)  fnlg12ds      kpjbu
003200060302     c                   seton                                        lr
003300050930
003400060302      *___________________________________________________                  ****
003500060302     C     elabora       BEGSR
003600060302      *___________________________________________________                  ****
003700060302      * reperisco progressivi precedenti ed escludo il progressivo 0
003800060302     c     kgcp          setgt     tigcp02l
003900060302     c                   do        *hival
004000060302     c     kgcp          readpe    tigcp02l
004100060302     c                   if        %eof(tigcp02l)
004200060302     c                   leave
004300060302     c                   endif
004400060302     c                   if        gcpfrg = 0
004500060302     c                   leave
004600060302     c                   endif
004700060324     c                   if        prima = *blank
004800060324     c                   move      gcpasg        spese
004900060324     c                   move      gcpnpc        porto
005000060414     c                   exsr      confronta
005100060324     c                   else
005200060324     c                   exsr      confronta
005300060324     c                   endif
005400060302      * caricamento sfl
005500060323     c                   clear                   s01
005600060302     c                   exsr      carica
005700060302     c                   enddo
005800060331      * verifica se dati congruenti anche con progressivo 0 solo se ne sono
005900060331      * presenti altri
006000060802     c                   if        s01rrn > 0 and gcpasg <> *blank
006100060324     c                   exsr      confronta
006200060331     c                   endif
006300060324      * se i valori non sono congruenti emette la videata
006400060302      * emissione sfl
006500060302     c     s01rrn        comp      0                                  0101
006600060809     c                   if        i12forza = *blank
006700060327     c                   if        s01rrn > 0  and o12ok <> *blank or
006800060327     c                             s01rrn > 0  and i12nodsp = *blank
006900060302     c                   z-add     1             s01rrn
007000060302     c                   exsr      emissione
007100060302     c                   endif
007200060809     c                   endif
007300060302     c                   endsr
007400060302      *___________________________________________________                  ****
007500060302     C     carica        BEGSR
007600060302      *___________________________________________________                  ****
007700060302     c                   movel     gcpfrg        s01frg
007800060302     c                   movel     gcpcmc        s01cmc
007900060302     c                   movel     gcpdis        s01dis
008000060302     c                   movel     gcpasg        s01asg
008100060302     c                   movel     gcppsg        s01psg
008200060302     c                   movel     gcpnpc        s01npc
008300060801      *
008400060801     c                   clear                   s01poa
008500060801     c                   clear                   s01aas
008600060801     c                   clear                   s01lnp
008700060801     c                   clear                   s01nsr
008800060801     c                   clear                   s01nsp
008900060801      *
009000060801     c                   if        gcplpwa > 0
009100060302     c                   movel     'A'           s01poa
009200060302     c                   movel     gcpaswa       s01aas
009300060302     c                   movel     gcplpwa       s01lnp
009400060302     c                   movel     gcpnswa       s01nsr
009500060302     c                   movel     gcpnbwa       s01nsp
009600060801     c                   endif
009700060801     c                   if        gcplpw > 0
009800060302     c                   movel     'P'           s01poa
009900060302     c                   movel     gcpasw        s01aas
010000060302     c                   movel     gcplpw        s01lnp
010100060302     c                   movel     gcpnsw        s01nsr
010200060302     c                   movel     gcpnbw        s01nsp
010300060302     c                   endif
010400060302     c                   movel(p)  gcpcmc        tab2
010500060302     c     ktab          chain     tabel00f
010600060302     c                   if        %found(tabel00f)
010700060302     c                   movel     tbluni        ds2a
010800060302     c                   movel     §2ades        s01cmcd
010900060302     c                   else
011000060302     c                   clear                   s01cmcd
011100060302     c                   endif
011200060323     c     ktas          chain     titas30c
011300060323     c                   if        %found(titas30c)
011400060323     c                   z-add     tasnft        s01nft
011500060323     c                   z-add     tasimv        s01imv
011600060323     c                   move      tascbo        s01cbo
011700060323     c                   move      tasfa2        s01fa2
011800060323     c                   select
011900060323     c                   when      tassv1 = 'I'
012000060323     c                   z-add     tasva1        s01vari
012100060323     c                   when      tassv2 = 'I'
012200060323     c                   z-add     tasva2        s01vari
012300060323     c                   when      tassv3 = 'I'
012400060323     c                   z-add     tasva3        s01vari
012500060324     c                   other
012600060324     c     kta7          chain     tita730c
012700060324     c                   if        %found(tita730c)
012800060324     c                   z-add     ta7van        s01vari
012900060324     c                   endif
013000060323     c                   endsl
013100060323     c                   endif
013200060801     c     kgnp          setll     tignp01l                               66
013300060323
013400060323      *previste massimo 12 righe
013500060323     c                   if        s01rrn < 13
013600060302     c                   add       1             s01rrn
013700060302     c                   write     s01
013800060302     c                   endif
013900060302     c                   endsr
014000060302      *___________________________________________________                  ****
014100060302     C     emissione     BEGSR
014200060302      *___________________________________________________                  ****
014300051005
014400060323     c                   do        *hival
014500060302     C                   WRITE     f01
014600060302     C                   exfmt     c01
014700060302     c   kc              leave
014800060323      * note
014900060323     c                   do        *hival
015000060323     c                   readc     s01                                    99
015100060323     c   99              leave
015200060324     c                   if        s01not = 'N'
015300060323     c                   exsr      call13
015400060323     c                   endif
015500060323     c                   enddo
015600060302     c                   enddo
015700050930
015800060323     C                   endsr
015900060323      *___________________________________________________                  ****
016000060323     C     call13        BEGSR
016100060323      *___________________________________________________                  ****
016200060323     C* Imposto parametri
016300060323     C                   clear                   fnlg13ds
016400060323     C                   Z-ADD     gcpagc        i13Agc
016500060323     C                   Z-ADD     gcpfgc        i13Fgc
016600060323     C                   Z-ADD     gcpngc        i13Ngc
016700060323     C                   Z-ADD     s01frg        i13frg
016800060323     C                   CALL      'FNLG13R'
016900060323     C                   PARM                    fnlg13ds
017000060323     C*
017100060323     C                   endsr
017200060324      *___________________________________________________                  ****
017300060324     C     confronta     BEGSR
017400060324      *___________________________________________________                  ****
017500060324     c                   if        gcpasg <> spese
017600060324     c                   clear                   i12nodsp
017700060324     c                   move      '1'           o12ok
017800060801     c                   eval      motv = 'Incongruenza spese'
017900060324     c                   endif
018000060324
018100060324     c                   if        gcpnpc <> porto
018200060324     c                   clear                   i12nodsp
018300060324     c                   move      '1'           o12ok
018400060801     c                   eval      motv = 'Incongruenza porto'
018500060324     c                   endif
018600060324
018700060330     c                   if        i12motr <> *blank
018800060330     c                   movel(p)  gcpcmc        tab2
018900060330     c     ktab          chain     tabel00f
019000060330     c                   if        %found(tabel00f)
019100060330     c                   movel     tbluni        ds2a
019200060330     c                   if        §2avst = 'S'
019300060330     c                   clear                   i12nodsp
019400060330     c                   move      '1'           o12ok
019500060801     c                   eval      motv = 'Motivi a rischio  '
019600060330     c                   endif
019700060330     c                   endif
019800060330     c                   endif
019900060330
020000060324     C                   endsr
020100060302      *___________________________________________________                  ****
020200060302     C     *INZSR        BEGSR
020300060302      *___________________________________________________                  ****
020400060302     C     *ENTRY        PLIST
020500060302     C                   PARM                    kpjba
020600060302     c                   movel     kpjbu         fnlg12ds
020700060302     c                   movel     i12agc        c01agc
020800060302     c                   movel     i12fgc        c01fgc
020900060302     c                   movel     i12ngc        c01ngc
021000060324      * ?OVRDBF x TITAS30c                                                  */
021100060324      * ?sistema informativo filiale reale(SETRAS)                          */
021200060324     c                   if        knsif = 'FILTRA201'
021300060324     c                   Movel(p)  cmd(1)        comman
021400060324     c                   else
021500060324      * ?sistema informativo filiale prova(AS888)                           */
021600060324     c                   Movel(p)  cmdp(1)       comman
021700060324     c                   endif
021800060324     c                   eval      lenght = %len(%trim(comman))
021900060324     c                   Call      'QCMDEXC'                            99
022000060324     c                   Parm                    comman
022100060324     c                   Parm                    lenght
022200060324     c  n99              Open      Titas30c
022300060302
022400060324      * ?OVRDBF x TITA730c                                                  */
022500060324      * ?sistema informativo filiale reale(SETRAS)                          */
022600060324     c                   if        knsif = 'FILTRA201'
022700060324     c                   Movel(p)  cmd(2)        comman
022800060324     c                   else
022900060324      * ?sistema informativo filiale prova(AS888)                           */
023000060324     c                   Movel(p)  cmdp(2)       comman
023100060324     c                   endif
023200060324     c                   eval      lenght = %len(%trim(comman))
023300060324     c                   Call      'QCMDEXC'                            99
023400060324     c                   Parm                    comman
023500060324     c                   Parm                    lenght
023600060324     c  n99              Open      Tita730c
023700060324
023800060302     C     kgcp          KLIST
023900060302     C                   KFLD                    i12Agc
024000060302     C                   KFLD                    i12Fgc
024100060324     C                   KFLD                    i12Ngc
024200060302
024300060323     C     ktas          KLIST
024400060323     C                   KFLD                    s01aas
024500060323     C                   KFLD                    s01lnp
024600060323     C                   KFLD                    s01nsr
024700060323     C                   KFLD                    s01nsp
024800060323
024900060324     C     kta7          KLIST
025000060324     C                   KFLD                    s01aas
025100060324     C                   KFLD                    s01lnp
025200060324     C                   KFLD                    s01nsr
025300060324     C                   KFLD                    s01nsp
025400060324     C                   KFLD                    tastbl
025500060324     C                   KFLD                    varia
025600060324     C                   move      'I'           varia
025700060324
025800060302     C     ktab          KLIST
025900060302     C                   KFLD                    codut             1 0
026000060302     C                   KFLD                    tab1              2
026100060302     C                   KFLD                    tab2              8
026200060302     c                   z-add     1             codut
026300060302     c                   move      '2A'          tab1
026400060302
026500060801     C     kgnp          KLIST
026600060801     C                   KFLD                    gcpAGC
026700060801     C                   KFLD                    gcpFGC
026800060801     C                   KFLD                    gcpNGC
026900060801     C                   KFLD                    gcpFRG
027000060801     C                   KFLD                    fase              3 0
027100060801     C                   z-add     20            fase
027200060302      *inizializza sfl
027300060302     C                   SETOn                                        02
027400060302     C                   WRITE     C01
027500060302     C                   SETOff                                       02
027600060302     C                   CLEAR                   S01RRN
027700060302     C                   CLEAR                   S01RRNLast
027800060302
027900060302     C                   ENDSR
028000060302
028100060324** cmd
028200060324OVRDBF FILE(TITAS30C) TOFILE(GAITRAGRU/TITAS30C)
028300060324OVRDBF FILE(TITA730C) TOFILE(GAITRAGRU/TITA730C)
028400060324** cmdp
028500060324OVRDBF FILE(TITAS30C) TOFILE(GAITRAGRPS/TITAS30C)
028600060324OVRDBF FILE(TITA730C) TOFILE(GAITRAGRPS/TITA730C)
