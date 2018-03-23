000001060626     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000100920914     H DECEDIT('0,') DATEDIT(*DMY.)
000200950321?     *--------------------------------------------------------------*
000300061012?     *      richiesta codice fiscale e variazione bolla             *
000400950321?     *--------------------------------------------------------------*
000900061012     FFNLRz1D   CF   E             WORKSTN
000902061107     ffnarb01l  if   e           k disk    usropn
000903061012     ffnblp01l  if   e           k disk    usropn
000905071106     ffiar401l  if   e           k disk
000906080711     ffipci01l  uf   e           k disk    usropn
000907080711     ffipci02l  uf   e           k disk    usropn  rename(fipci000:fipci02)
000908061012     d kpjba         e ds
015500020822     D ddatiute      e ds
015600020822     D azuteds       e ds                  extname(AZUTE00F)
015700020822     D tibs34ds      E DS                  inz
015701080312     D dsbl4m        E DS                  inz
015702061108     D dta4q         E DS                  inz
015703061012     D fnlrz1ds      E DS
015704071106     D*dar5gen       E DS
015706071106      * DS PER TIBS02R - RICERCA TABELLA SU TNTBE
015707071106     D Tibs02DS      E DS
015708071106     D WLBDAT          DS                  INZ
015709071106     D  G02DAT                 1      8  0
015710071106     D  G02INV                 9     16  0
015711071106     D  G02ERR                17     17
015712071106     D  G02TGI                18     22  0
015800941115     D*
015802061012     D trul90ds      E DS
015803061012     D FNLR48ds      E DS                  inz
015900061012     D DSARBD        E DS                  inz
016000061012     D DSARBG        E DS                  inz
016100061012     D DARBT         E DS                  inz
016200061012     D DSARBK        E DS                  inz
016201061012     I*-------------_____------------_______-------------_______-----
016202061012     I* RIDENOMINO TUTTI I CAMPI DI BLP COME ARB
016203061012     IFNBLP000
016204061012     I              BLPATB                      ARBATB
016205061012     I              BLPAAS                      ARBAAS
016206061012     I              BLPLNP                      ARBLNP
016207061012     I              BLPNRS                      ARBNRS
016208061012     I              BLPNSP                      ARBNSP
016209061012     I              BLPCBO                      ARBCBO
016210061012     I              BLPMGS                      ARBMGS
016211061012     I              BLPFNS                      ARBFNS
016212061012     I              BLPSCL                      ARBSCL
016213061012     I              BLPFBR                      ARBFBR
016214061012     I              BLPFDN                      ARBFDN
016215061012     I              BLPLNA                      ARBLNA
016216061012     I              BLPZNC                      ARBZNC
016217061012     I              BLPKSC                      ARBKSC
016218061012     I              BLPRSM                      ARBRSM
016219061012     I              BLPINM                      ARBINM
016220061012     I              BLPCAM                      ARBCAM
016221061012     I              BLPLOM                      ARBLOM
016222061012     I              BLPNZM                      ARBNZM
016223061012     I              BLPPRM                      ARBPRM
016224061012     I              BLPFAP                      ARBFAP
016225061012     I              BLPCPI                      ARBCPI
016226061012     I              BLPRSD                      ARBRSD
016227061012     I              BLPIND                      ARBIND
016228061012     I              BLPCAD                      ARBCAD
016229061012     I              BLPLOD                      ARBLOD
016230061012     I              BLPNZD                      ARBNZD
016231061012     I              BLPPRD                      ARBPRD
016232061012     I              BLPFIN                      ARBFIN
016233061012     I              BLPGC1                      ARBGC1
016234061012     I              BLPGC2                      ARBGC2
016235061012     I              BLPCTR                      ARBCTR
016236061012     I              BLPCTS                      ARBCTS
016237061012     I              BLPFTM                      ARBFTM
016238061012     I              BLPTSP                      ARBTSP
016239061012     I              BLPNAS                      ARBNAS
016240061012     I              BLPNCL                      ARBNCL
016241061012     I              BLPNCP                      ARBNCP
016242061012     I              BLPPKC                      ARBPKC
016243061012     I              BLPPKB                      ARBPKB
016244061012     I              BLPPKF                      ARBPKF
016245061012     I              BLPNCR                      ARBNCR
016246061012     I              BLPVLC                      ARBVLC
016247061012     I              BLPFVB                      ARBFVB
016248061012     I              BLPVLB                      ARBVLB
016249061012     I              BLPFVF                      ARBFVF
016250061012     I              BLPVLF                      ARBVLF
016251061012     I              BLPQFT                      ARBQFT
016252061012     I              BLPVMD                      ARBVMD
016253061012     I              BLPVAD                      ARBVAD
016254061012     I              BLPIAS                      ARBIAS
016255061012     I              BLPVAS                      ARBVAS
016256061012     I              BLPCCM                      ARBCCM
016257061012     I              BLPRMN                      ARBRMN
016258061012     I              BLPRMA                      ARBRMA
016259061012     I              BLPRMO                      ARBRMO
016260061012     I              BLPFLS                      ARBFLS
016261061012     I              BLPNCD                      ARBNCD
016262061012     I              BLPNCA                      ARBNCA
016263061012     I              BLPXCO                      ARBXCO
016264061012     I              BLPCTM                      ARBCTM
016265061012     I              BLPFFD                      ARBFFD
016266061012     I              BLPTCR                      ARBTCR
016267061012     I              BLPDCR                      ARBDCR
016268061012     I              BLPHCR                      ARBHCR
016269061012     I              BLPDTI                      ARBDTI
016270061012     I              BLPHTI                      ARBHTI
016271061012     I              BLPDCE                      ARBDCE
016272061012     I              BLPNPI                      ARBNPI
016273061012     I              BLPNCI                      ARBNCI
016274061012     I              BLPNRC                      ARBNRC
016275061012     I              BLPDBR                      ARBDBR
016276061012     I              BLPNFV                      ARBNFV
016277061012     I              BLPDPC                      ARBDPC
016278061012     I              BLPDUC                      ARBDUC
016279061012     I              BLPFLP                      ARBFLP
016280061012     I              BLPDET                      ARBDET
016281061012     I              BLPDUT                      ARBDUT
016282061012     I              BLPDAM                      ARBDAM
016283061012     I              BLPNDC                      ARBNDC
016284061012     I              BLPDDC                      ARBDDC
016285061012     I              BLPDCP                      ARBDCP
016286061012     I              BLPDCM                      ARBDCM
016287061012     I              BLPHMC                      ARBHMC
016288061012     I              BLPTC1                      ARBTC1
016289061012     I              BLPTC2                      ARBTC2
016290061012     I              BLPCCA                      ARBCCA
016291061012     I              BLPTMC                      ARBTMC
016292061012     I              BLPGGA                      ARBGGA
016293061012     I              BLPGMA                      ARBGMA
016294061012     I              BLPGVA                      ARBGVA
016295061012     I              BLPFLE                      ARBFLE
016296061012     I              BLPTFP                      ARBTFP
016297061012     I              BLPFEA                      ARBFEA
016298061012     I              BLPTFA                      ARBTFA
016299061012     I              BLPFT1                      ARBFT1
016300061012     I              BLPDT1                      ARBDT1
016301061012     I              BLPFT2                      ARBFT2
016302061012     I              BLPDT2                      ARBDT2
016303061012     I              BLPFT3                      ARBFT3
016304061012     I              BLPDT3                      ARBDT3
024100061012     c*---------------------------------------------------------------*
024200061012     c* FLUSSO PRINCIPALE                                             *
024300061012     c*---------------------------------------------------------------*
024301080313     c                   clear                   w1pi
024302080313     c                   clear                   w1cf
024313080313     c* controllo se ho la partita iva sulla bolla
024314071105     c                   clear                   arbcpi
024315071105     c                   if        dz1tipo = 'A'
024316071105     c     karbblp       chain     fnarb01l                           01
024317071105     c                   else
024318071105     c     karbblp       chain     fnblp01l                           01
024319071105     c                   end
024320120502     c* se ho la partita iva esco
024321120502     c                   if        arbcpi <> ' '
024324120502     c                   eval      pda = *on
024325120502     c                   exsr      srpda
024326120502     c                   seton                                        lr
024327120502     c                   return
024328120502     c                   end
024329080313     c* controllo se ho il codice fiscle sull'estensione bolla
024330061107     C                   eval      ar4trc = 'Q'
024331061107     c     kar4          chain     fiar401l
024332061108     c                   if        %found(fiar401l)
024333061108     c                   eval      dta4q = ar4not
024334061108     c                   else
024335061108     c                   clear                   dta4q
024336061108     c                   end
024337080313     c* se uno dei due non è presente sulla bolla
024338061108     c                   if        (DZ1TIPO = 'P'
024339120502     c                             and %subst(§TA4QCFM: 12: 1)<>' ') or
024340061107     c                             (DZ1TIPO = 'A'
024341120502     c                             and %subst(§TA4QCFD: 12: 1)<>' ')
024342120502     c                   eval      pda = *on
024343120502     c                   exsr      srpda
024344120502     c                   seton                                        lr
024345120502     c                   return
024346120502     c                   end
024347080313     c* verifico se i dati sono stati inseriti nel pda
024348080711     c                   clear                   pda               1
024349080313     c                   exsr      srpda
024350080313     c* se ho entrambi i dati dal pda faccio variazione bolla ed esco
024351120427     c                   if        %subst(w1cf: 12: 1)<>' ' or w1pi<>' '
024352080313     c                   exsr      varbol
024353080313     c                   if        d48err = ' '
024354080711     c                   eval      pda = *on
024355080711     c                   exsr      srpda
024356080313     c                   seton                                        lr
024357080313     c                   return
024358080313     c                   end
024359080313     c                   end
024360080313     c* se non presenti nemmeno nel pda emetto la window di richiesta
024361061012     C                   DO        *hival
024362061107     c                   eval      w1lnp= DZ1LNP
024363061107     c                   eval      w1nrs= DZ1NRS
024364061107     c                   eval      w1nsp= DZ1NSP
024365080313     c                   if        arbcpi <> ' '
024366071105     c                   eval      w1pi=  arbcpi
024367080313     c                   end
024368080313     c                   if        DZ1TIPO = 'P' and §TA4QCFM <> ' '
024369071105     c                   eval      w1cf=  §TA4QCFM
024370080313     c                   end
024371080313     c                   if        DZ1TIPO = 'A' and §TA4QCFD <> ' '
024372071105     c                   eval      w1cf=  §TA4QCFD
024373071105     c                   end
024374080312     c*
024375061012     C                   EXFMT     LRZ1W01
024376061012     C   KL              leave
024377071105     c                   if        w1cf <> ' ' or w1pi <> ' '
024378061012     c                   exsr      varbol
024379061013     c* errore in manutenzione bolla
024380061013     c                   if        d48err = ' '
024381080711     c                   eval      pda = *on
024382080711     c                   exsr      srpda
024383061013     c                   leave
024384061013     c                   else
024385061013     c                   movel     d48msg        wmsg
024386061012     c                   end
024387061013     c                   end
024388061012     c                   enddo
024389061012     c*
024390061012     c                   movel     d48err        dz1err
024391061012     c                   movel     d48msg        dz1msg
024392061012     C                   MOVEL     FNLRz1ds      KPJBU
024393061012     C                   SETON                                        LR
162800941117     C*----------------------------------------------------*
162900061012     C*  RICHIAMO PGM VARIAZIONE BOLLE in modo occulto
163000941117     C*----------------------------------------------------*
163100061012     C     VARBOL        BEGSR
163101061013     c*
163102061013     c                   clear                   fnlr48ds
163103061013     c                   clear                   DSARBD
163104061013     c                   clear                   DSARBK
163105061013     c                   clear                   DARBT
163106061013     c                   clear                   DSARBG
163107061013     c                   clear                   wmsg
163108061013     c                   setoff                                       99
163109061128     c                   eval      D48F12='S'
163110061012     c*
163111061012     c                   if        dz1tipo = 'A'
163112061012     c     karbblp       chain     fnarb01l                           01
163114061012     C                   MOVEL     'A'           D48TBO
163115061012     c                   else
163116061012     c     karbblp       chain     fnblp01l                           01
163119061012     C                   MOVEL     'P'           D48TBO
163120061012     c                   end
163121061012     c                   if        not *in01
164101061106     C                   MOVEL     'FI'          D48CVB
164102061106     C                   MOVEL     'D'           D48TRC
164103061012     C                   MOVEL     'E'           D48TLA
164104061012     C                   MOVEL     arblna        D48FGS
164105061012     C                   MOVE      ARBAAS        D48AAS
164106061012     C                   MOVE      ARBLNP        D48LNP
164107061012     C                   MOVE      ARBNRS        D48NRS
164108061012     C                   MOVE      ARBNSP        D48NSP
164109061012     C                   MOVEL     ARBCBO        D48CBO
164110061012     C                   MOVEL     ' '           D48ERR
164111061012     C                   MOVEL     *BLANK        D48MSG
164118071106     C                   MOVEL     w1pi          §BDRSD
164119061012     C                   MOVEL     w1cf          §BDCPI
164139061012     C                   MOVEL(p)  fnlr48ds      KPJBU
164140061012     C                   CALL      'FNLR48R'
164141061012     C                   PARM                    KPJBA
164142061012     C                   PARM                    DSARBD
164143061012     C                   PARM                    DSARBK
164144061012     C                   PARM                    DARBT
164145061012     C                   PARM                    DSARBG
164146061012     C                   PARM                    trul90ds
164147061012     C                   MOVEL     KPJBU         FNLR48ds
164148061109     c* se non trovata la spedizione
164149061109     c                   else
164150061109     c                   eval      d48err= '1'
164151061109     c                   eval      d48msg= 'Spediz. inesistente'
164153061012     c                   end
175000941117     C*
175100941117     C                   ENDSR
175200061012     c*---------------------------------------------------------------*
175300080312     c     srpda         begsr
175400061012     c*---------------------------------------------------------------*
175401120510     c* se non ho il ritiro esco
175402120510     c                   if        dz1tipo = 'P' and
175403120510     c                             dz1POE = 0
175404120510     c                   leavesr
175405120510     c                   end
175406080312     c*
175407080711     c                   if        pda = ' '
175408080312     c                   clear                   pcicdf
175409080312     c                   clear                   pcicpi
175410080313     c                   clear                   pciatb
175411080312     c                   if        dz1tipo = 'A'
175412080711     c     kpci01        chain(n)  fipci01l
175413080312     c                   else
175418080711     c     kpci02        chain(n)  fipci02l
175419080312     c                   end
175420080313     c                   if        pcicdf <> ' ' and pciatb = ' '
175421080312     c                   eval      w1cf = pcicdf
175422080312     c                   end
175423080313     c                   if        pcicpi <> ' ' and pciatb = ' '
175424080312     c                   eval      w1pi = pcicpi
175425080312     c                   end
175426080711     c* delete il record
175427080711     c                   else
175428080711     c                   if        dz1tipo = 'A'
175429080711     c                   if        %open(fipci01l)
175430080711     c     kpci01        chain     fipci01l
175431080711     c                   if        %found(fipci01l)
175432080711     c                   delete    fipci000
175433080711     c                   end
175434080711     c                   end
175435080711     c                   else
175440080711     c                   if        %open(fipci02l)
175441080711     c     kpci02        chain     fipci02l
175442080711     c                   if        %found(fipci02l)
175443080711     c                   delete    fipci02
175444080711     c                   end
175445080711     c                   end
175446080711     c                   end
175447080711     c                   end
175448080312     C*
175449080312     C                   ENDSR
175450080312     c*---------------------------------------------------------------*
175451080312     c     *inzsr        begsr
175452080312     c*---------------------------------------------------------------*
175500061012     c*  Operazioni iniziali
175600061012     C     *ENTRY        PLIST
175700061012     C                   PARM                    KPJBA
175800061012     C*                  PARM                    trul90ds
175900061012     C                   MOVEL     KPJBU         FNLRz1ds
175901061107     c                   clear                   dz1err
175902061107     c                   clear                   dz1msg
175903061012     C* bolla arrivo
175904061012     c                   if        dz1tipo = 'A'
175905061012     c                   open      fnarb01l
175906080312     c                   open      fipci01l
175907080312     c                   setoff                                       10
175908061012     c                   else
175909120510     c* lo imposto perchè l'incasso prepagati lo imposta a blanks ...
175910120510     c                   eval      dz1tipo = 'P'
175911061012     c* prepagato
175912080312     c                   if        dz1poe = 0
175913080312     C                   eval      ar4trc = 'M'
175914080312     c     kar4          chain     fiar401l
175915080312     c                   if        %found(fiar401l)
175916080312     c                   eval      dsbl4m = ar4not
175917080312     c                   eval      dz1poe = §B4POE
175918080312     c                   eval      dz1nsr = §B4NSR
175919080312     c                   eval      dz1nor = §B4NOR
175920080312     c                   eval      dz1nrv = §B4NRV
175921080312     c                   else
175922080312     c                   clear                   dsbl4m
175923080312     c                   end
175924080312     c                   end
175925080312     c                   seton                                        10
175926080312     c                   eval      w1poe = dz1POE
175927080312     c                   eval      w1nsr = dz1NSR
175928080312     c                   eval      w1nor = dz1NOR
175929080312     c                   eval      w1nrv = dz1NRV
175930061012     c                   open      fnblp01l
175931080312     c                   open      fipci02l
175932061012     c                   end
176000061012     c*
176100061012     c     *dtaara       define    §azute        azuteds
176200061012     c     *dtaara       define    §datiute      ddatiute
176300061012     C                   in(E)     *dtaara
176400061012     C                   IF        %Error  or  RSUT = *blanks
176500061012     C                   call      'TIBS34R'
176600061012     C                   parm                    Tibs34Ds
176700061012     C                   in        *dtaara
176800061012     c                   ENDIF
176818071106     c*
176819071106     C                   TIME                    W0140            14 0
176820071106     C* UDATE IN GGMMAAAA
176821071106     C                   MOVE      W0140         WDTGIO            8 0
176822071106     C*
176823071106     C* UDATE IN AAAAMMGG
176824071106     C                   Z-ADD     WDTGIO        G02DAT
176825071106     C                   MOVEL     *BLANK        G02ERR
176826071106     C                   CALL      'XSRDA8'
176827071106     C                   PARM                    WLBDAT
176828071106     C                   MOVEL     G02INV        DATEU             8 0
176847061012     c*
176848061012     C     Karbblp       KLIST
176849061012     C                   KFLD                    dz1AAS
176850061012     C                   KFLD                    dz1LNP
176851061012     C                   KFLD                    dz1NRS
176852061012     C                   KFLD                    dz1NSP
176853061012     C     Kar4          KLIST
176854061012     C                   KFLD                    dz1AAS
176855061012     C                   KFLD                    dz1LNP
176856061012     C                   KFLD                    dz1NRS
176857061012     C                   KFLD                    dz1NSP
176858061012     C                   KFLD                    ar4trc
176859080312     C     Kpci01        KLIST
176860080312     C                   KFLD                    dz1AAS
176861080312     C                   KFLD                    dz1LNP
176862080312     C                   KFLD                    dz1NRS
176863080312     C                   KFLD                    dz1NSP
176864080312     C     Kpci02        KLIST
176865080312     C                   KFLD                    dz1POE
176866080312     C                   KFLD                    dz1NSR
176867080312     C                   KFLD                    dz1NOR
176868080312     C                   KFLD                    dz1NRV
176900061012     c                   endsr
