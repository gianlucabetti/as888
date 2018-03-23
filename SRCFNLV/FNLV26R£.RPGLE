000001060626     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000100920914     H DECEDIT('0,') DATEDIT(*DMY.)
000200950321?     *--------------------------------------------------------------*
000300061012?     *      richiesta codice fiscale e variazione bolla             *
000400950321?     *--------------------------------------------------------------*
000900061012     FFNLRz1D   CF   E             WORKSTN
000902061107     ffnarb01l  if   e           k disk    usropn
000903061012     ffnblp01l  if   e           k disk    usropn
000904071106     f*iar501l  if   e           k disk
000905071106     ffiar401l  if   e           k disk
000906061012     d kpjba         e ds
015500020822     D ddatiute      e ds
015600020822     D azuteds       e ds                  extname(AZUTE00F)
015700020822     D tibs34ds      E DS                  inz
015701061108     D dta4q         E DS                  inz
015702061012     D fnlrz1ds      E DS
015703071106     D*dar5gen       E DS
015704071106     d dvpoDECO      e ds
015705071106      * DS PER TIBS02R - RICERCA TABELLA SU TNTBE
015706071106     D Tibs02DS      E DS
015707071106     D WLBDAT          DS                  INZ
015708071106     D  G02DAT                 1      8  0
015709071106     D  G02INV                 9     16  0
015710071106     D  G02ERR                17     17
015711071106     D  G02TGI                18     22  0
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
024301071105     c* controllo se ho la partita iva
024302071105     c                   clear                   arbcpi
024303071105     c                   if        dz1tipo = 'A'
024304071105     c     karbblp       chain     fnarb01l                           01
024305071105     c                   else
024306071105     c     karbblp       chain     fnblp01l                           01
024307071105     c                   end
024309061107     c* controllo se ho il codice fiscle
024310061107     C                   eval      ar4trc = 'Q'
024311061107     c     kar4          chain     fiar401l
024312061108     c                   if        %found(fiar401l)
024313061108     c                   eval      dta4q = ar4not
024314061108     c                   else
024315061108     c                   clear                   dta4q
024316061108     c                   end
024317061108     c                   if        (DZ1TIPO = 'P'
024318061108     c                             and §TA4QCFM =' ') or
024319061107     c                             (DZ1TIPO = 'A'
024320071105     c                             and §TA4QCFD =' ') or
024321071106     c                             (arbcpi = ' ' and dateu>§vpopiv)
024322061012     C                   DO        *hival
024323061107     c                   eval      w1lnp= DZ1LNP
024324061107     c                   eval      w1nrs= DZ1NRS
024325061107     c                   eval      w1nsp= DZ1NSP
024326071105     c                   eval      w1pi=  arbcpi
024327071105     c                   if        DZ1TIPO = 'P'
024328071105     c                   eval      w1cf=  §TA4QCFM
024329071105     c                   else
024330071105     c                   eval      w1cf=  §TA4QCFD
024331071105     c                   end
024332061012     C                   EXFMT     LRZ1W01
024333061012     C   KL              leave
024334071105     c                   if        w1cf <> ' ' or w1pi <> ' '
024335061012     c                   exsr      varbol
024336061013     c* errore in manutenzione bolla
024337061013     c                   if        d48err = ' '
024338061013     c                   leave
024339061013     c                   else
024340061013     c                   movel     d48msg        wmsg
024341061012     c                   end
024342061013     c                   end
024343061012     c                   enddo
024344061107     c                   end
024345061012     c*
024346061012     c                   movel     d48err        dz1err
024347061012     c                   movel     d48msg        dz1msg
024348061012     C                   MOVEL     FNLRz1ds      KPJBU
024349061012     C                   SETON                                        LR
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
163122061012     C* PRENDO 2° RAGIONE SOCIALE DESTINATARIO
163123071106     C*                  eval      ar4trc = 'D'
163124071106     C*    KAR4          CHAIN     FiAR401L
163125071106     c*                  if        %found(fiar401l)
163126071106     C*                  MOVEL     ar4not        §BDRD2
163127071106     c*                  end
163128071106     C*                  clear                   dar5gen
163129071106     C*                  eval      ar5trd = 'GEN'
163130071106     C*    Kar5          CHAIN     FiAR501L
163131071106     c*                  if        %found(fiar501l)
163132071106     C*                  MOVEL     ar5uni        dar5gen
163133071106     C*                  MOVEL     §AR5REF       §BDref
163134071106     C*                  MOVEL     §AR5TELD      §BDtel
163135071106     C*                  endif
164100941117     C*
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
164120071106     c*                  move      arbffd        §bdffd
164121071106     C*                  MOVE      ARBPKF        §BDPKF
164122071106     C*                  MOVE      ARBVLF        §BDVLF
164123071106     C*                  MOVE      ARBFIN        §BDFIN
164125071106     C*                  MOVEL     arbind        §BDIND
164126071106     C*                  MOVEL     arbcad        §BDCAD
164127071106     C*                  MOVEL     arblod        §BDLOD
164128071106     C*                  MOVEL     arbprd        §BDPRD
164129071106     C*                  MOVEL     arbnzd        §BDNZD
164132071106     C*                  MOVE      ARBFIN        §BDFIN
164133071106     c*                  z-add     arbQFT        §BTQFT
164134071106     c*                  z-add     arbVMD        §BTVMD
164135071106     c*                  MOVE      arbVAD        §BTVAD
164136071106     c*                  z-add     arbIAS        §BTIAS
164137071106     c*                  MOVE      arbVAS        §BTVAS
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
175300061012     c     *inzsr        begsr
175400061012     c*---------------------------------------------------------------*
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
175906061012     c                   else
175907061012     c* prepagato
175908061012     c                   open      fnblp01l
175909061012     c                   end
176000061012     c*
176100061012     c     *dtaara       define    §azute        azuteds
176200061012     c     *dtaara       define    §datiute      ddatiute
176300061012     C                   in(E)     *dtaara
176400061012     C                   IF        %Error  or  RSUT = *blanks
176500061012     C                   call      'TIBS34R'
176600061012     C                   parm                    Tibs34Ds
176700061012     C                   in        *dtaara
176800061012     c                   ENDIF
176801071106      * Aggancio tabella VPO record DECO
176802071106     c                   Clear                   Dvpodeco
176803071106     c                   Clear                   Tibs02ds
176804071106     c                   Eval      T02Mod = 'C'
176805071106     c                   Eval      T02Sif = knsif
176806071106     c                   Eval      T02Cod = 'VPO'
176807071106     c                   Movel(p)  'DECO'        T02Ke1
176808071106     c                   Call      'TIBS02R'
176809071106     c                   Parm                    Kpjba
176810071106     c                   Parm                    Tibs02ds
176811071106     c                   If        T02Err = *Blanks
176812071106     c                   Movel     T02Uni        Dvpodeco
176813071106     c                   EndIf
176814071106     c*
176815071106     C                   TIME                    W0140            14 0
176816071106     C* UDATE IN GGMMAAAA
176817071106     C                   MOVE      W0140         WDTGIO            8 0
176818071106     C*
176819071106     C* UDATE IN AAAAMMGG
176820071106     C                   Z-ADD     WDTGIO        G02DAT
176821071106     C                   MOVEL     *BLANK        G02ERR
176822071106     C                   CALL      'XSRDA8'
176823071106     C                   PARM                    WLBDAT
176824071106     C                   MOVEL     G02INV        DATEU             8 0
176825071106     c                   if        dateu>§vpopiv
176826071106     c                   seton                                        10
176827071106     c                   end
176828061012     c*
176829061012     C     Karbblp       KLIST
176830061012     C                   KFLD                    dz1AAS
176831061012     C                   KFLD                    dz1LNP
176832061012     C                   KFLD                    dz1NRS
176833061012     C                   KFLD                    dz1NSP
176834071106     C*    Kar5          KLIST
176835071106     C*                  KFLD                    dz1AAS
176836071106     C*                  KFLD                    dz1LNP
176837071106     C*                  KFLD                    dz1NRS
176838071106     C*                  KFLD                    dz1NSP
176839071106     C*                  KFLD                    ar5trd
176840061012     C     Kar4          KLIST
176841061012     C                   KFLD                    dz1AAS
176842061012     C                   KFLD                    dz1LNP
176843061012     C                   KFLD                    dz1NRS
176844061012     C                   KFLD                    dz1NSP
176845061012     C                   KFLD                    ar4trc
176900061012     c                   endsr
