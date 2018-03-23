000100930511     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000200941005     FTABEL00F  IF   E           K DISK
000300941005      *
000400941005     FFNBLP01L  IF   E           K DISK
000401070413     FAZORG01L  IF   E           K DISK
000500941005      *
000600941005     FFNARB00F  O    E           K DISK
000601070413     FFiAR401L  uf a E           K DISK
000700941005      *
000800941005     D WLBDA8          DS
000900941005     D  G08DAT                 1      8  0
001000941005     D  G08INV                 9     16  0
001100941005     D  G08ERR                17     17
001200941005     D  G08TGI                18     22  0
001300941005     D DS3A          E DS
001302070413     D DS7A          E DS
001303070413     d Og150         e ds
001304070413     D OG143         E DS
001400020603     D DSBL4A        E DS
001500941005     D DSUL41        E DS                  EXTNAME(TRUL41DS)
001501070413     D FNLV55DS      E DS
001502070413     D WTRC            S                   LIKE(AR4TRC)
001503070413     d lnpntw          s                   like(§ogntw)
001504070413     d lnapks          s              1
001600941005     C     KNWB          CHAIN     FNBLP01L                           21
001700941005     C  N21              DO
001800941005     C                   MOVEL     *BLANK        ARBATB
001900941005     C                   MOVEL     BLPAAS        ARBAAS
002000941005     C                   MOVEL     BLPLNP        ARBLNP
002100941005     C                   MOVEL     BLPNRS        ARBNRS
002200941005     C                   MOVEL     BLPNSP        ARBNSP
002300941005     C                   MOVEL     BLPMGS        ARBMGS
002400941005     C                   MOVEL     BLPCBO        ARBCBO
002500941005     C                   MOVEL     BLPFNS        ARBFNS
002600941005     C                   MOVEL     BLPSCL        ARBSCL
002700941005     C                   MOVEL     BLPFBR        ARBFBR
002800941005     C                   MOVEL     BLPFDN        ARBFDN
002900941005     C                   MOVEL     BLPLNA        ARBLNA
003000941005     C                   MOVEL     BLPZNC        ARBZNC
003100941005     C                   MOVEL     BLPKSC        ARBKSC
003200941005     C                   MOVEL     BLPRSM        ARBRSM
003300941005     C                   MOVEL     BLPINM        ARBINM
003400941005     C                   MOVEL     BLPCAM        ARBCAM
003500941005     C                   MOVEL     BLPLOM        ARBLOM
003600941005     C                   MOVEL     BLPPRM        ARBPRM
003700941005     C                   MOVEL     BLPFAP        ARBFAP
003800941005     C                   MOVEL     BLPNZM        ARBNZM
003900061031     C**!!!              MOVEL     BLPCPI        ARBCPI
004000941005     C                   MOVEL     BLPRSD        ARBRSD
004100941005     C                   MOVEL     BLPIND        ARBIND
004200941005     C                   MOVEL     BLPCAD        ARBCAD
004300941005     C                   MOVEL     BLPLOD        ARBLOD
004400941005     C                   MOVEL     BLPPRD        ARBPRD
004500941005     C                   MOVEL     BLPFIN        ARBFIN
004600941005     C                   MOVEL     BLPNZD        ARBNZD
004700941005     C                   MOVEL     BLPGC1        ARBGC1
004800941005     C                   MOVEL     BLPGC2        ARBGC2
004900941005     C                   MOVEL     BLPCTR        ARBCTR
005000941005     C                   MOVEL     BLPCTS        ARBCTS
005100941005     C                   MOVEL     BLPFTM        ARBFTM
005200941005     C                   MOVEL     BLPTSP        ARBTSP
005300941005     C                   MOVEL     BLPNAS        ARBNAS
005400941005     C                   MOVEL     BLPNCL        ARBNCL
005500941005     C                   MOVEL     BLPNCP        ARBNCP
005600941005     C                   MOVEL     BLPPKC        ARBPKC
005700941005     C                   MOVEL     BLPPKB        ARBPKB
005800941005     C                   MOVEL     BLPPKF        ARBPKF
005900941005     C                   MOVEL     BLPNCR        ARBNCR
006000941005     C                   MOVEL     BLPVLC        ARBVLC
006100941005     C                   MOVEL     BLPFVB        ARBFVB
006200941005     C                   MOVEL     BLPVLB        ARBVLB
006300941005     C                   MOVEL     BLPFVF        ARBFVF
006400941005     C                   MOVEL     BLPVLF        ARBVLF
006500941005     C                   MOVEL     BLPQFT        ARBQFT
006600941005     C                   MOVEL     BLPVMD        ARBVMD
006700941005     C                   MOVEL     BLPVAD        ARBVAD
006800941005     C                   MOVEL     BLPIAS        ARBIAS
006900941005     C                   MOVEL     BLPVAS        ARBVAS
007000941005     C                   MOVEL     BLPCCM        ARBCCM
007100941005     C                   MOVEL     BLPRMN        ARBRMN
007200941005     C                   MOVEL     BLPRMA        ARBRMA
007300941005     C                   MOVEL     BLPRMO        ARBRMO
007400941005     C                   MOVEL     BLPFLS        ARBFLS
007500941005     C                   MOVEL     BLPNCD        ARBNCD
007600941005     C                   MOVEL     BLPNCA        ARBNCA
007700941005     C                   MOVEL     BLPXCO        ARBXCO
007800941005     C                   MOVEL     BLPFFD        ARBFFD
007900941005     C                   MOVEL     BLPTCR        ARBTCR
008000941005     C                   MOVEL     BLPDCR        ARBDCR
008100941005     C                   MOVEL     BLPHCR        ARBHCR
008200941005     C                   Z-ADD     BLPDTI        ARBDTI
008300941005     C                   Z-ADD     BLPHTI        ARBHTI
008400941005     C                   Z-ADD     BLPDCE        ARBDCE
008500941005     C                   Z-ADD     BLPNPI        ARBNPI
008600941005     C                   Z-ADD     BLPNCI        ARBNCI
008700941005     C                   Z-ADD     BLPNRC        ARBNRC
008800941005     C                   MOVE      BLPDBR        ARBDBR
008900941005     C                   MOVE      BLPNFV        ARBNFV
009000941005     C                   MOVE      BLPDPC        ARBDPC
009100941005     C                   MOVE      BLPDUC        ARBDUC
009200941005     C                   MOVEL     BLPFLP        ARBFLP
009300941005     C                   MOVE      *ZEROS        ARBDET
009400941005     C                   MOVE      *ZEROS        ARBDUT
009500941005     C                   MOVE      *ZEROS        ARBDAM
009600941005     C                   MOVE      *ZEROS        ARBPDC
009700941005     C                   MOVE      *ZEROS        ARBNDC
009800941005     C                   MOVE      *ZEROS        ARBDDC
009900941005     C                   MOVE      *ZEROS        ARBNTC
010000941005     C                   MOVE      *ZEROS        ARBDCP
010100941005     C                   MOVE      *ZEROS        ARBDCM
010200941005     C                   MOVE      *ZEROS        ARBHMC
010300941005     C                   MOVEL     BLPTC1        ARBTC1
010400941005     C                   MOVEL     BLPTC2        ARBTC2
010500941005     C                   MOVEL     *BLANK        ARBFAN
010600941005     C                   MOVEL     *BLANKS       ARBCCA
010700941005     C                   MOVEL     *BLANK        ARBTMC
010800941005     C                   MOVEL     *BLANK        ARBCMC
010900941005     C                   MOVEL     BLPGGA        ARBGGA
011000941005     C                   MOVEL     BLPGMA        ARBGMA
011100941005     C                   MOVEL     BLPGVA        ARBGVA
011200941005     C                   MOVEL     *BLANK        ARBFBS
011300941005     C                   MOVEL     *ZEROS        ARBSTP
011400941005     C                   MOVEL     *ZEROS        ARBNGD
011500941005     C                   MOVEL     *ZEROS        ARBNSS
011600941005     C                   MOVE      *ZEROS        ARBAMP
011700941005     C                   MOVE      3             ARBAMA
011800941005     C                   MOVEL     'S'           ARBACC
011900941005     C                   MOVEL     *BLANKS       ARBACA
012000941005     C                   MOVEL     *BLANK        ARBICC
012100941005     C                   MOVEL     *BLANK        ARBICA
012200941005     C                   MOVEL     *BLANK        ARBMIF
012300941005     C                   MOVEL     *ZEROS        ARBIFP
012400941005     C                   MOVEL     *BLANK        ARBFIP
012500941005     C                   MOVEL     *ZEROS        ARBIPP
012600941005     C                   MOVEL     *BLANK        ARBFBC
012700941005     C                   MOVEL     *ZEROS        ARBCUC
012800941005     C                   MOVEL     *BLANK        ARBFDC
012900941005     C                   MOVEL     *BLANK        ARBFNC
013000941005     C                   MOVEL     *ZEROS        ARBNMI
013100941005     C                   MOVEL     *ZEROS        ARBDMI
013200941005     C                   MOVEL     BLPFLE        ARBFLE
013300941005     C                   MOVEL     BLPTFP        ARBTFP
013400941005     C                   MOVEL     BLPFEA        ARBFEA
013500941005     C                   MOVEL     BLPTFA        ARBTFA
013600941005     C                   MOVEL     'R'           ARBFT1
013700941005     C                   Z-ADD     AMGUDT        ARBDT1
013800941005     C                   MOVEL     *BLANKS       ARBFT2
013900941005     C                   Z-ADD     0             ARBDT2
014000941005     C                   MOVEL     *BLANKS       ARBFT3
014100941005     C                   Z-ADD     0             ARBDT3
014200941005     C* FLAG ABILITAZIONE CONSEGNA ASSEGNATO
014300941005     C                   MOVEL     *BLANKS       ARBACA
014400941005     C                   MOVEL     '3A'          TBLCOD
014500941005     C                   MOVEL     *BLANKS       TBLKEY
014600941005     C                   MOVEL     ARBCBO        TBLKEY
014700941005     C     KTAB          CHAIN     TABEL                              21
014800941005     C  N21              MOVEL     TBLUNI        DS3A
014900941005     C   21              MOVEL     'F1'          §3ATB1
015000941005     C                   MOVEL     §3ATB1        D41TPB            1
015100941005     C     D41TPB        IFEQ      'F'                                              FRANCO
015200941005     C     §3ATB2        IFEQ      '  '                                             2° BOLLA
015300941005     C                   MOVEL     'S'           ARBACA
015400941005     C                   END
015500941005     C                   ELSE
015600941005     C                   MOVEL     ' '           ARBACA
015700941005     C                   END
015800061031      * prendo la partita iva/codice fiscale solo se non è bolla doppia
015900061031     c                   if        §3atb2 = *blanks
016000061031     c                   eval      arbcpi = blpcpi
016100061031     c                   endif
016101070413     C* PRENDO TABELLA VARIABILI ARRIVI
016102070416     C*                  MOVEL     '7A'          tblCOD
016104070416     C*                  MOVEL(p)  '1'           tblkey
016105070416     C*    KTAB          CHAIN     TABEL                              33
016106070416     C* N33              MOVEL     TBLUNI        DS7A
016107070416     C*  33              CLEAR                   DS7A
016200941005     C*
016300941005     C                   WRITE     FNARB000
016301070413     c* scrive FIAR4 x ddt NO
016304070413     c                   exsr      Scriviar4A
016400060214     C                   ENDDO
016401070413     C     WCAL55        IFEQ      'S'
016402070413     C                   CLEAR                   FNLV55DS
016403070413     C                   MOVEL     'C'           D55TLA
016404070413     C                   CALL      'FNLV55R'
016405070413     C                   PARM                    FNLV55DS
016406070413     C                   ENDIF
016500060214     C*
016600060214     C                   SETON                                        LR
016601070413      *-----------------------------------------------------------------------
016602070413     c     Scriviar4A    BEGSR
016603070413     c                   eval      wtrc = 'A'
016604070413     c     kar4          chain     fiar401l
016606070413     c                   if        %found(fiar401l)
016607070413     c                   movel     ar4not        dsbl4a
016608070413     c                   else
016609070413     C                   MOVE      BLPAAS        AR4AAS
016610070413     C                   MOVEL     BLPLNP        AR4LNP
016611070413     C                   MOVEL     BLPNRS        AR4NRS
016612070413     C                   MOVEL     BLPNSP        AR4NSP
016613070413     C                   MOVEL     'A'           AR4TRC
016614070413     c                   clear                   dsbl4a
016615070413     c                   clear                   ar4not
016616070413     c                   movel     'R'           ar4ftr
016617070413     c                   movel     AMGUDT        ar4DTR
016619070413     c                   endif
016620070413     c*
016621070413     c                   movel     AMGUDT        ar4duv
016622070413     C                   MOVEL     BLpfst        §B4ABM
016623070413     c*
016624070413     c                   clear                   lnapks
016625070413     c* controllo se lna gestita da altri
016626070413     c                   clear                   fnlv55ds
016627070413     c                   movel     '6'           D55TPT
016628070413     c                   z-add     blplna        d55lin
016629070413     c                   z-add     AMGUDT        d55drf
016630070413     c                   movel     'S'           wcal55            1
016631070413     c                   call      'FNLV55R'
016632070413     c                   parm                    fnlv55ds
016633070413      * se è un p.o. gestito da altri controllo se la lna stessa ha il picking attivo
016634070413     c                   if        d55lin <> d55tfa
016635070413     c     d55lin        chain     azorg01l                           32
016636070413     c  n32              movel     orgdf0        og150
016638070413     c   32              clear                   og150
016640070413     c                   If        §OgPck = 'S' and §OgPkd <> 'S'
016641070413     c                             or §ogsda = 'S'
016642070413     c                   eval      lnapks = 'S'
016643070413     c                   endif
016644070413     c                   endif
016649070413      * controllo se c'è il picking attivo
016650070413     c                   if        lnapks = *blanks
016651070413     C     d55tfa        CHAIN     AZORG01L                           32
016652070413     C  N32              MOVEL     ORGDf0        OG150
016653070413     C   32              CLEAR                   OG150
016654070413if  5c                   If        §OgPck = 'S' and §OgPkd <> 'S'
016655070413     c                             or §ogsda = 'S'
016656070413     c                   eval      lnapks = 'S'
016657070413     c                   else
016658070413     c                   clear                   lnapks
016659070413     c                   endif
016660070413     c                   endif
016661070413      * se linea di arrivo con picking attivo o lna per cui stampare i
016662070413      * DDT SI in arrivo
016663070413if  1c                   If        lnapks = 'S'
016664070413 s  2c                   Select
016665070413      * per bolla di recupero
016666070413      * imposto che la bolla è già stata stampata
016667070413     c                   When         §3arbl = 'R'
016668070413     c                   If        Blpfst = 'S'
016669070413     c                   Eval      §B4Abm = 'P'
016670070413     c                   EndIf
016671070413     c                   If        Blpfst = 'C'
016672070413     c                   Eval      §B4Abm = 'Q'
016673070413     c                   EndIf
016674070413     c                   If        Blpfst = 'N' or Blpfst = 'Y'
016675070413     c                   Eval      §B4Abm = 'J'
016676070413     c                   Endif
016677070413     c                   If        Blpfst = 'K'
016678070413     c                   Eval      §B4Abm = 'W'
016679070413     c                   Endif
016680070413
016681070416      * se blpfst = 'Y'
016682070413      * imposto 'N' x bolla ancora da stampare
016683070416     c* lasciamo Y perchè non escano ne in stampa ne in ristampa NO DDT
016684070416     c* (Elisa)
016685070416     c*                  When      Blpfst = 'Y'
016686070416     c*                  Eval      §B4Abm = 'N'
016687070413
016688070413    2c                   Endsl
016689070413    1c                   Endif
016690070413     c* verifico il ntw
016691070416     c* lasciamo Y perchè non escano ne in stampa ne in ristampa NO DDT
016692070416     C*    BLPLNP        CHAIN     AZORG01L                           32
016693070416     C* N32              MOVEL     ORGDE3        OG143
016694070416     C*  32              CLEAR                   OG143
016695070416     c*                  eval      lnpntw = §ogntw
016696070413
016697070416     C*    §7AFSR        IFEQ      'Y'
016698070416     C*    BLpfst        ANDEQ     'Y'
016699070416     C*    lnpNTW        ANDNE     'DPD'
016700070416     C*                  MOVEL     'N'           §B4ABM
016701070416     C*                  ENDIF
016702070413     c* Sigla operatore video
016703070413     C                   MOVE      BLPSOP        §B4ASO
016704070413     C                   MOVEL     DSBL4A        AR4NOT
016705070413     c                   if        %found(fiar401l)
016706070413     c                   update    fiar4000
016707070413     c                   else
016708070413     C                   WRITE     FiAR4000                             99
016709070413     c                   endif
016710070413     c*
016711070413     c                   endsr
016712060214     C*------------------------------------------------------------
016800941005     C     *INZSR        BEGSR
016900060214     C*------------------------------------------------------------
017000941005     C     *ENTRY        PLIST
017100941005     C                   PARM                    DSUL41
017200941005     C* PASS. PARAMETRI DA PGM CHIAMANTE
017300941005      *
017400941005      * UDATE
017500941005     C                   TIME                    WHHUDT           14 0
017600941005     C                   MOVE      WHHUDT        G08DAT
017700941005     C                   MOVE      *ZEROS        G08INV
017800941005     C                   MOVEL     *BLANK        G08ERR
017900941005     C                   CALL      'XSRDA8'
018000941005     C                   PARM                    WLBDA8
018100941005     C                   Z-ADD     G08INV        AMGUDT            8 0
018200941005     C                   Z-ADD     G08DAT        GMAUDT            8 0
018300941005     C                   MOVEL     AMGUDT        ANNO              4 0
018400941005     C                   MOVEL     WHHUDT        ORAMIN            4 0
018500941005     C*
018600941005     C                   Z-ADD     1             CODUT             1 0
018700941005     C     KTAB          KLIST
018800941005     C                   KFLD                    CODUT
018900941005     C                   KFLD                    TBLCOD
019000941005     C                   KFLD                    TBLKEY
019100941005     C*
019200941005     C     KNWB          KLIST
019300941005     C                   KFLD                    D41AAS
019400941005     C                   KFLD                    D41LNP
019500941005     C                   KFLD                    D41NRS
019600941005     C                   KFLD                    D41NSP
019700941005     C*
019800941005     C     KNW4          KLIST
019900941005     C                   KFLD                    D41AAS
020000941005     C                   KFLD                    D41LNP
020100941005     C                   KFLD                    D41NRS
020200941005     C                   KFLD                    D41NSP
020201070413     C     KAR4          KLIST
020202070413     C                   KFLD                    BLPAAS
020203070413     C                   KFLD                    BLPLNP
020204070413     C                   KFLD                    BLPNRS
020205070413     C                   KFLD                    BLPNSP
020206070413     C                   KFLD                    WTRC
020300020603     C*
020400941005     C                   ENDSR
