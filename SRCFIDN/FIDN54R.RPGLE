000100990127      *----------------------------------------------------*
000200990218      *  Ricalcolo Responsabilità Danno per CA aperte dal P.O. che sto elaborando
000300990127      *----------------------------------------------------*
000400990127
000500990127      ****************************************************************
000600990127      *  RIEPILOGO INDICATORI
000700990127      ***************************************************************
000800990127      * 30 - COMODO
000900990209      * 31 - COMODO
001000990209      * 90 - prima volta che stampo
001100990127      ***************************************************************
001200990127
001300990127     H DECEDIT('0,') DATEDIT(*DMY.)
001400990127
001500990127     FFNDCT01L  IF   E           K DISK
001600040804     FFNDCK01L  IF   E           K DISK
001700990209     FFNARB01L  IF   E           K DISK
001800990218     FFNBLP01L  IF   E           K DISK
001900990310     FTABEL00f  IF   E           K DISK
002000990209     FQPRINT    O    F  132        PRINTER oflind(*INOF)
002100990127
002200990127      *------------------------------------------------------------------------*
002300990310     D Kcod            S                   LIKE(tblcod)
002400990310     D Kkey            S                   LIKE(tblkey)
002500990202     D dateu           s              8  0
002600990202     D wdtgio          S              8  0
002700990202     D w0140           S             14  0
002800990209     D ggspe           S              2  0
002900990209     D mmspe           S              2  0
003000990209     D TRAT            S            132    INZ(*all'-')
003100990310     D DS3A          E DS
003200040804     D DDCT01        E DS
003300990202      *
003400990209     D fidn50ds      E DS
003500990202      *
003600990209     D CNCR80        E DS
003700990209     D UT§DSE0F      E DS
003800990202     D KPJBA         E DS
003900990218      *
004000990208      *
004100990208     D WLBDAT          DS                  INZ
004200990208     D  G02DAT                 1      8  0
004300990208     D  G02INV                 9     16  0
004400990208     D  G02ERR                17     17
004500990208     D  G02TGI                18     22  0
004600990209
004700990209     D TES             S            100    DIM(3) CTDATA PERRCD(1)              TESTATA STAMPA
004800990218      ***************************************************************************
004900990209
005000990218     IFNBLP000
005100990218     I              BLPATB                      ARBATB
005200990218     I              BLPAAS                      ARBAAS
005300990218     I              BLPLNP                      ARBLNP
005400990218     I              BLPNRS                      ARBNRS
005500990218     I              BLPNSP                      ARBNSP
005600990218     I              BLPCBO                      ARBCBO
005700990218     I              BLPMGS                      ARBMGS
005800990218     I              BLPFNS                      ARBFNS
005900990218     I              BLPSCL                      ARBSCL
006000990218     I              BLPFBR                      ARBFBR
006100990218     I              BLPFST                      WDDT
006200990218     I              BLPFDN                      ARBFDN
006300990218     I              BLPLNA                      ARBLNA
006400990218     I              BLPZNC                      ARBZNC
006500990218     I              BLPKSC                      ARBKSC
006600990218     I              BLPRSM                      ARBRSM
006700990218     I              BLPINM                      ARBINM
006800990218     I              BLPCAM                      ARBCAM
006900990218     I              BLPLOM                      ARBLOM
007000990218     I              BLPNZM                      ARBNZM
007100990218     I              BLPPRM                      ARBPRM
007200990218     I              BLPFAP                      ARBFAP
007300990218     I              BLPCPI                      ARBCPI
007400990218     I              BLPRSD                      ARBRSD
007500990218     I              BLPIND                      ARBIND
007600990218     I              BLPCAD                      ARBCAD
007700990218     I              BLPLOD                      ARBLOD
007800990218     I              BLPNZD                      ARBNZD
007900990218     I              BLPPRD                      ARBPRD
008000990218     I              BLPFIN                      ARBFIN
008100990218     I              BLPGC1                      ARBGC1
008200990218     I              BLPGC2                      ARBGC2
008300990218     I              BLPCTR                      ARBCTR
008400990218     I              BLPCTS                      ARBCTS
008500990218     I              BLPFTM                      ARBFTM
008600990218     I              BLPTSP                      ARBTSP
008700990218     I              BLPNAS                      ARBNAS
008800990218     I              BLPNCL                      ARBNCL
008900990218     I              BLPNCP                      ARBNCP
009000990218     I              BLPPKC                      ARBPKC
009100990218     I              BLPPKB                      ARBPKB
009200990218     I              BLPPKF                      ARBPKF
009300990218     I              BLPNCR                      ARBNCR
009400990218     I              BLPVLC                      ARBVLC
009500990218     I              BLPFVB                      ARBFVB
009600990218     I              BLPVLB                      ARBVLB
009700990218     I              BLPFVF                      ARBFVF
009800990218     I              BLPVLF                      ARBVLF
009900990218     I              BLPQFT                      ARBQFT
010000990218     I              BLPVMD                      ARBVMD
010100990218     I              BLPVAD                      ARBVAD
010200990218     I              BLPIAS                      ARBIAS
010300990218     I              BLPVAS                      ARBVAS
010400990218     I              BLPCCM                      ARBCCM
010500990218     I              BLPRMN                      ARBRMN
010600990218     I              BLPRMA                      ARBRMA
010700990218     I              BLPRMO                      ARBRMO
010800990218     I              BLPFLS                      ARBFLS
010900990218     I              BLPNCD                      ARBNCD
011000990218     I              BLPNCA                      ARBNCA
011100990218     I              BLPXCO                      ARBXCO
011200990218     I              BLPCTM                      ARBCTM
011300990218     I              BLPFFD                      ARBFFD
011400990218     I              BLPTCR                      ARBTCR
011500990218     I              BLPDCR                      ARBDCR
011600990218     I              BLPHCR                      ARBHCR
011700990218     I              BLPDTI                      ARBDTI
011800990218     I              BLPHTI                      ARBHTI
011900990218     I              BLPDCE                      ARBDCE
012000990218     I              BLPNPI                      ARBNPI
012100990218     I              BLPNCI                      ARBNCI
012200990218     I              BLPNRC                      ARBNRC
012300990218     I              BLPDBR                      ARBDBR
012400990218     I              BLPNFV                      ARBNFV
012500990218     I              BLPDPC                      ARBDPC
012600990218     I              BLPDUC                      ARBDUC
012700990218     I              BLPFLP                      ARBFLP
012800990218     I              BLPDET                      ARBDET
012900990218     I              BLPDUT                      ARBDUT
013000990218     I              BLPDAM                      ARBDAM
013100990218     I              BLPNDC                      ARBNDC
013200990218     I              BLPDDC                      ARBDDC
013300990218     I              BLPDCP                      ARBDCP
013400990218     I              BLPDCM                      ARBDCM
013500990218     I              BLPHMC                      ARBHMC
013600990218     I              BLPTC1                      ARBTC1
013700990218     I              BLPTC2                      ARBTC2
013800990218     I              BLPCCA                      ARBCCA
013900990218     I              BLPTMC                      ARBTMC
014000990218     I              BLPGGA                      ARBGGA
014100990218     I              BLPGMA                      ARBGMA
014200990218     I              BLPGVA                      ARBGVA
014300990218     I              BLPFLE                      ARBFLE
014400990218     I              BLPTFP                      ARBTFP
014500990218     I              BLPFEA                      ARBFEA
014600990218     I              BLPTFA                      ARBTFA
014700990218     I              BLPFT1                      ARBFT1
014800990218     I              BLPDT1                      ARBDT1
014900990218     I              BLPFT2                      ARBFT2
015000990218     I              BLPDT2                      ARBDT2
015100990218     I              BLPFT3                      ARBFT3
015200990312     I              BLPDT3                      ARBDT3
015300990218
015400990202      *****************************************************************
015500990202      *
015600990202     c* Lettura di tutte le CA
015700990202      *
015800990202     c                   do        *hival
015900990202      *
016000990202     c                   read      fndct01l                               30
016100990209     c*
016200990218     c   30              leave
016300990308     c* escludo i pareggi e le manuali
016400990308     c     dctcch        ifne      '03'
016500990308     c     dctcar        andne     'M'
016600990312     c     dctatb        andeq     ' '
016700990312     c
016800990312     c     dctaac        ifeq      1999
016900990331     c     dctmgc        andge     0322
017000990312     c     dctaac        orgt      1999
017100990920     c     dctfil        ifeq      40
017200990920     c     dctnca        andeq     109
017300990308     c
017400040804      * se CA con legami recupero il numero di  spedizione da FNDCK
017500040804     c                   movel     dctflo        ddct01
017600040804     c                   If        §dctlega = 'S'
017700040804     c     Kdck          chain     Fndck01l
017800040804     c                   If        %found(fndck01l)
017900040804     c                   eval      dctaas = dckaas
018000040804     c                   eval      dctlnp = dcklnp
018100040804     c                   eval      dctnrs = dcknrs
018200040804     c                   eval      dctnsp = dcknsp
018300040804     c                   endif
018400040804     c                   endif
018500990209      * aggancio file bolle arrivi per recuperare i dati da passare al PGM calcolo responsabilità
018600990209     c     karb          chain     fnarb01l                           31
018700050914     c**n31              movel     'A'           wtpb              1
018800990310     c                   if        *in31
018900990218     c   31karb          chain     fnblp01l                           31
019000050914     c***                movel     'P'           wtpb              1
019100990310     c                   endif
019200990209      * se CA non regolare leggo la successiva
019300990202     c                   if        *in31
019400990202     c                   iter
019500990202      *
019600990202     c                   else
019700990218      * se CA regolare calcolo
019800990209     c                   exsr      SUB_RESP
019900990202      *
020000990202     c                   endif
020100990202      *
020200990419     c                   endif
020300990304     c                   endif
020400990312     c                   endif
020500990202      *
020600990202     c  N30              enddo
020700990202      *
020800990209     c* se stampata almeno una riga scrivo la riga finale di mandare i dati alla sede
020900990209     c   90              except    fine
021000990209     c*
021100990331     c                   movel     'DN94'        kcoaz
021200990331     c                   clear                   kpjbu
021300990920     c**                 Call      'BCH10'
021400990920     c**                 Parm                    KPJBA
021500990331     c*
021600990202     c                   eval      *inlr = *on
021700990202      *---------------------------------------------------------------*
021800990209      *   SUB_RESP  Routine richiamo PGM Calcolo
021900990202      *---------------------------------------------------------------*
022000990209     C     SUB_RESP      BEGSR
022100990202      *
022200990202      * preparo la ds
022300990209     c                   clear                   fidn50ds
022400990202     c*
022500990209     c                   movel     'C'           I50mod
022600990209     c                   z-add     DCTaac        I50aac
022700990209     c                   z-add     DCTfil        I50fil
022800990209     c                   z-add     DCTnca        I50nca
022900990209     c                   z-add     DCTaas        I50aas
023000990209     c                   z-add     DCTlnp        I50lnp
023100990209     c                   z-add     DCTnrs        I50nrs
023200990209     c                   z-add     DCTnsp        I50nsp
023300990209     c                   z-add     ARBlna        I50lna
023400990209     c                   z-add     ARBncl        I50ncl
023500990209     c                   z-add     ARBtfp        I50tfp
023600990209     c                   z-add     ARBtfa        I50tfa
023700050914     c****               movel     wtpb          I50tpb
023800050914     c                   movel     §dctlnpc      i50lnpmam
023900990310     c
024000990310     c* aggancio il tipo bolla
024100990310     C                   movel     '3A'          kCOD
024200990310     C                   movel(P)  ARBCBO        kKEY
024300990310     C     ktab          CHAIN     TABEL                              31
024400990310     C  n31              movel     TBLUNI        DS3A
024500990310     C  n31              movel     §3ATB1        WPORTO            1
024600990310     c   31              movel     'F'           WPORTO
024700990310     c                   if        wporto='F'
024800990310     c                   z-add     arbksc        i50ksm
024900990310     c                   else
025000990310     c                   z-add     arbksc        i50ksd
025100990310     c                   z-add     arbccm        i50ksm
025200990310     c                   endif
025300990209     c                   movel(P)  FIDN50DS      KPJBU
025400990209      *
025500990209     c                   Call      'FIDN50R'
025600990209     c                   Parm                    KPJBA
025700990202      *
025800990209      * se mi ritorna un errore stampo l'errore e la CA / Spedizione legata
025900990209      *
026000990209     c                   if        o50err = 'E'
026100990209     c*
026200990209     c                   if        not *in90 or *inof
026300990209     c*
026400990209     c                   except    testa
026500990209     c                   eval      *in90 = *on
026600990209     c                   eval      *inof = *off
026700990209     c                   endif
026800990209     c*
026900990209      *
027000990209     c                   clear                   mmspe
027100990209     c                   clear                   ggspe
027200990209      *
027300990209     c                   movel     arbmgs        mmspe
027400990209     c                   move      arbmgs        ggspe
027500990209      *
027600990209     c                   except    riga
027700990209     c*
027800990209     c                   endif
027900990202     c                   endsr
028000990127      *****************************************************************
028100990127      *   ROUTINE INIZIALE
028200990127      *****************************************************************
028300990127     C     *INZSR        BEGSR
028400990127      *
028500990127     C     *ENTRY        PLIST
028600990127     C                   PARM                    KPJBA
028700990209      *
028800990209     C                   Z-ADD     1             CODUT
028900990209     C                   CALL      'X§PARUT'
029000990209     C                   PARM                    UT§DSE0F
029100990209     C                   MOVEL     RAGUT         RSUT             20
029200990209     C                   MOVEL     REC80         CNCR80
029300990208      * ORA
029400990208     C                   TIME                    W0140
029500990208      * UDATE IN GGMMAAAA
029600990208     C                   MOVE      W0140         WDTGIO
029700990209      * accesso Bolle Arrivi
029800990209     C     karb          KLIST
029900990209     C                   KFLD                    dctaas
030000990209     C                   KFLD                    dctlnp
030100990209     C                   KFLD                    dctnrs
030200990209     C                   KFLD                    dctnsp
030300990310     C     ktab          KLIST                                                  *
030400990310     C                   KFLD                    codut                          *
030500990310     C                   KFLD                    kcod                           *
030600990310     C                   KFLD                    kkey                           *
030700040804      * accesso FNDCK01L
030800040804     C     kdck          KLIST
030900040804     C                   KFLD                    dctaac
031000040804     C                   KFLD                    dctfil
031100040804     C                   KFLD                    dctnca
031200990208      *
031300990127     C                   ENDSR
031400990209     OQPRINT    E            TESTA
031500990209     O                       RSUT              +  0
031600990209     O                                         +  0 '/'
031700990209     O                       SIMFEL            +  0
031800990209     O                                           56 'ELENCO CA / SPEDIZIONI CON'
031900990209     O                                         +  1 'ERRORI NEL CALCOLO RESPONS'
032000990209     O                                              'ABILITA'''
032100990209     O                                          100 'FIDN51R'
032200990209     O                       WDTGIO        Y    115
032300990209     O                                          125 'PAG.'
032400990209     O                       PAGE          Z    130
032500990209     O          E            TESTA             3
032600990209     O                       KNSIF             +  0
032700990209     O                       KNMUS             +  1
032800990209     O          E            TESTA             4
032900990209     O                       TRAT               132
033000990209     O          E            TESTA             5
033100990209     O                       tes(1)
033200990217     O          E            TESTA             6  1
033300990209     O                       TRAT               132
033400990217     O          E            riga              1
033500990209     O                       dctaac
033600990209     O                       dctfil           +   1
033700990209     O                       dctnca        Z  +   1
033800990209     O                       arbaas           +   1
033900990209     O                       arblnp           +   1
034000990209     O                       arbnrs        Z  +   1
034100990209     O                       arbnsp        Z  +   1
034200990209     O                       ggspe            +   1
034300990209     O                                              '/'
034400990209     O                       mmspe
034500990209     o                       o50err           +   5
034600990217     O          E            fine              1
034700990209     o                       Tes(3)           +  30
034800990217     O          E            fine              1
034900990209     o                       Tes(2)           +  30
035000990217     O          E            fine              1
035100990209     o                       Tes(3)           +  30
035200990209** TES  (Lungh. 100)   -Disegno stampa-
035300990209Anno P.O.  N°C.A.  Anno LNP Ser Numero  Data          E R R O R E
035400990209MANDARE LA LISTA IN SEDE ALL'UFF. CED
035500990209-------------------------------------
