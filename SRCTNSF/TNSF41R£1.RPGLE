000100060504      *PARMS OPTION(*NOXREF) TGTRLS(*CURRENT)
000200060504      *===============================================================*
000300060518      *?TNSF41R * Ritassazione spedizioni fatturate                  ?*
000400060504      *===============================================================*
000500060504
000600060504     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000700060504
000800060504      *---------------------------------------------------------------*
000900060504
001000060801     fWFrft01l  uf   e           k disk    usropn
001100060801     fTitai01l  if   e           k disk    usropn
001200060801     fTitas30c  if   e           k disk    usropn
001300060801     Ffiar531c  if   e           k disk    usropn
001400060801     fTncsb03l  if   e           k disk    usropn
001500060801     Ffnevb04l  if   e           k disk
001600060519     fTabel00f  if   e           k disk
001700081006     fTntam01l  if   e           k disk
001800081006     fTitad04l  if   e           k disk
001900060504      *
002000060518     FTnsf41p   o    e             printer oflind(*in90)
002100060504
002200060504      *---------------------------------------------------------------*
002300060504
002400060504      *
002500060504      * Schiere  - - - - - - - - - - - - - - - - - - - - - - - - - - -*
002600060504      *
002700060522      * codici bolla che prevedono un imponibile =0
002800060522     d IM0             S              2    DIM(50)                              LEGENDA VARIE SIGLE
002900060522      * codici bolla che prevedono la tassazione di una singola varia
003000060522     d CBO             S              2    DIM(50)                              LEGENDA VARIE SIGLE
003100060522     d CBV             S              1    DIM(50)                              LEGENDA VARIE SIGLE
003200081006      * Tipi bolla non di recupero e conto servizio
003300081006     d TBO_OK          S              2    DIM(50)                              Tipi bolla normali
003400081006     d CBO_OK          S              2    DIM(50)                              Codici bolla normali
003500081006      * scaglioni tariffe
003600081006     d SGL             S             13  3 DIM(18)                              Scaglioni tariffe
003700081006      * importi totali legati allo scagione tariffa
003800081006     d IMF             S             15  3 DIM(18)                              Vecchio importo
003900081006     d IMR             S             15  3 DIM(18)                              Nuovo   importo
004000081006     d TKG             S              9  1 DIM(18)                              Totale  peso
004100081006     d TCL             S              7  0 DIM(18)                              Totale  colli
004200081006     d TVL             S              7  3 DIM(18)                              Totale  volume
004300081006     d NSP             S              7  0 DIM(18)                              Totale  spedizione
004400081006     d NMT             S              7  0 DIM(18)                              Totale  manca tariff
004500060801      * ovrdbf
004600060801     d cmdt            s             60    dim(06) ctdata perrcd(1)
004700060504      * Ds - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
004800060504      *
004900060504     d KPJBA         e ds
005000060518      * - Parametri x il lancio del pgm batch
005100060518     d TNSF40DS      e ds                  inz
005200060522      * - Parametri x Controllo profilo utenti
005300060522     d TIBS34DS      e ds                  inz
005400060522      * - Ds di riferimento al file esterno AZUTE00F
005500060522     d AZUTEds       e ds                  extname(AZUTE00F)
005600060522      * - Ds per dati organigramma
005700060522     d DDatiUte      e ds
005800081006      ** Tipi bolla
005900081006     D DSTB          E DS
006000060522      ** Codici bolla
006100060522     D DS3A          E DS
006200081006      ** Tipi tariffa
006300081006     D DSTR          E DS
006301110701      ** DS del tasflo del file TITAS00F
006302110701     D DTASFLO       E DS
006400060518      ** DS Calcolo tassazione  - Varie
006500060518     D DTAS          E DS
006600060518     D  dstasflo     E                     extfld(tasflo)
006700060926      ** DS Flag operativi DS DTAS
006800060926     d Dtas01        e ds
006900060926
007000060518     d Dtasv         e ds
007100060518     d  sv                     1     20
007200060518     d                                     Dim(20)                              Sigle varie
007300060518     d  va                    21    140P 3
007400060518     d                                     Dim(20)                              Importi varie
007500060522      * recupero bancali e colli originali
007600060522     d dAr5Ban       e ds
007700060522
007800060522     d dAr5Bnb       e ds
007900060518      * - Ds per scrittura file di lavoro
008000060518     d                 ds
008100060518     d  RFTsv1r                1      1
008200060518     d  RFTsv2r                2      2
008300060518     d  RFTsv3r                3      3
008400060518     d  RFTsv4r                4      4
008500060518     d  RFTsv5r                5      5
008600060518     d  RFTsv6r                6      6
008700060518     d  RFTsv7r                7      7
008800060518     d  RFTsv8r                8      8
008900060518     d  RFTsv9r                9      9
009000060518     d  RFTs10r               10     10
009100060518     d  RFTs11r               11     11
009200060518     d  RFTs12r               12     12
009300060518     d  RFTs13r               13     13
009400060518     d  RFTs14r               14     14
009500060518     d  RFTs15r               15     15
009600060518     d  RFTs16r               16     16
009700060518     d  RFTs17r               17     17
009800060518     d  RFTs18r               18     18
009900060518     d  RFTs19r               19     19
010000060518     d  RFTs20r               20     20
010100060518     d  RFT_svr                1     20    Dim(20)                              Sigle varie
010200060512
010300060518      * - Ds per scrittura file di lavoro
010400060518     d                 ds
010500060518     d  RFTva1r                1     11  3
010600060518     d  RFTva2r               12     22  3
010700060518     d  RFTva3r               23     33  3
010800060518     d  RFTva4r               34     44  3
010900060518     d  RFTva5r               45     55  3
011000060518     d  RFTva6r               56     66  3
011100060518     d  RFTva7r               67     77  3
011200060518     d  RFTva8r               78     88  3
011300060518     d  RFTva9r               89     99  3
011400060518     d  RFTv10r              100    110  3
011500060518     d  RFTv11r              111    121  3
011600060518     d  RFTv12r              122    132  3
011700060518     d  RFTv13r              133    143  3
011800060518     d  RFTv14r              144    154  3
011900060518     d  RFTv15r              155    165  3
012000060518     d  RFTv16r              166    176  3
012100060518     d  RFTv17r              177    187  3
012200060518     d  RFTv18r              188    198  3
012300060518     d  RFTv19r              199    209  3
012400060518     d  RFTv20r              210    220  3
012500060518     d  RFT_var                1    220  3 Dim(20)                              Importo varie
012600060504      *
012700060504      * - Controllo/Inversione date
012800060504     d WLBdat          ds                  inz
012900060504     d   G08dat                       8  0 inz
013000060504     d   G08inv                       8  0 inz
013100060504     d   G08err                       1    inz('3')
013200060504     d   G08tgi                       5  0 inz
013300060504      *
013400060504      * - Variabili definite a programma
013500060519     d codut           s              1  0 inz(1)
013600060504     d Wdate           s              8  0 inz
013700060518     d Wora            s              6  0 inz
013800060516     d W0140           s             14  0 inz
013900081006     d z               s              4  0
014000081006     d zz              s              4  0
014100060518     d k               s              4  0
014200060519     d kk              s              4  0
014300060522     d xx              s              4  0
014400081006     d yy              s              4  0
014500081006     d num_sgl         s              4  0
014600060522     d kscfil          s              3  0
014700081006     d tipo_tar        s              1  0
014800081006     d ok_scagl        s               n
014900060522     d cod             s                   like(tblcod)
015000081006     d key             s                   like(tblkey)
015100060522     d kAr5Trd         s                   Like(Ar5Trd)
015200060522     d antepo          s                   like(tasimv)
015300060522     d W_tasdiv        s                   like(tasdiv)
015400060522     d W_dtas          s                   like(dtas)
015500060522     d W_dtasv         s                   like(dtasv)
015600060522     D Wcev            s                   like(evbcev) inz('RIC')
015700081006     d prg2            s                   like(tamprg) inz
015800081006     d ctr2            s                   like(tamctr) inz
015900081006     d ksc2            s                   like(tamksc) inz
016000081006     d savprg          s                   like(tamprg)
016100081006     d savlnp          s                   like(tadlnp)
016200081006     d savorp          s                   like(tadorp)
016300081006     d savnaz          s                   like(tadnaz)
016400081006     d savcap          s                   like(tadcap)
016500081006     d misura          s                   like(tadsgl)
016600060801
016700060801     d wlib            s             10
016800060801     d wlib1           s             10
016900060801     d comman          s             80
017000060801     d lenght          s             15  5 inz(80)
017100060801
017200060522
017300060504      *
017400060504      * Key-List - - - - - - - - - - - - - - - - - - - - - - - - - - -*
017500060504      *
017600060518      * - WFRFT01L
017700060518     c     KRFT          Klist
017800060518     c                   Kfld                    I40noj
017900060518     c                   Kfld                    I40pru
018000060518     c                   Kfld                    I40dta
018100060518     c                   Kfld                    I40ora
018200060517
018300060518      * - TITAI01L / TITAS30C
018400060518     c     KSPE          Klist
018500060518     c                   Kfld                    RFTaas
018600060518     c                   Kfld                    RFTlnp
018700060518     c                   Kfld                    RFTnrs
018800060518     c                   Kfld                    RFTnsp
018900060518     c                   Kfld                    RFTtbl
019000060519      * Tncsb03l
019100060519     c     KCSB          Klist
019200060519     c                   Kfld                    TASaas
019300060519     c                   Kfld                    TASlnp
019400060519     c                   Kfld                    TASnrs
019500060519     c                   Kfld                    TASnsp
019600060519      * Tabel00f
019700060519     c     KTAB          Klist
019800060519     c                   Kfld                    codut
019900060519     c                   Kfld                    cod
020000081006     c     KTABkey       Klist
020100081006     c                   Kfld                    codut
020200081006     c                   Kfld                    cod
020300081006     c                   Kfld                    key
020400060522      * Fiar531c
020500060522     c     kFiar5        Klist
020600060522     c                   Kfld                    TasAas
020700060522     c                   Kfld                    TasLnp
020800060522     c                   Kfld                    TasNrs
020900060522     c                   Kfld                    TasNsp
021000060522     c                   Kfld                    kAr5Trd
021100060522      * Chiave file eventi FNEVB
021200060522     c     Kevb4         Klist
021300060522     c                   Kfld                    Tasaas
021400060522     c                   Kfld                    Taslnp
021500060522     c                   Kfld                    Tasnrs
021600060522     c                   Kfld                    Tasnsp
021700060522     c                   Kfld                    Wcev
021800081006      ** accesso tntam x cliente
021900081006     c     Ktam          Klist
022000081006     c                   Kfld                    rftkscr
022100081006     c                   Kfld                    rftctrr
022200081006      ** accesso titad
022300081006     c     Ktad          Klist
022400081006     c                   Kfld                    tamksc
022500081006     c                   Kfld                    tamctr
022600081006     c                   Kfld                    tamprg
022700081006
022800081006     c     Ktad2         Klist
022900081006     c                   Kfld                    tamksc
023000081006     c                   Kfld                    tamctr
023100081006     c                   Kfld                    tamprg
023200081006     c                   Kfld                    savlnp
023300081006     c                   Kfld                    savorp
023400081006     c                   Kfld                    savnaz
023500081006     c                   Kfld                    savcap
023600060504
023700060504      *---------------------------------------------------------------*
023800060504      *  RIEPILOGO INDICATORI                                         *
023900060504      *---------------------------------------------------------------*
024000060727      * 05/30 - Comodo
024100060727      * 50    - Data fattura antecedente 30-04-06 stampo scritta
024200060727      * 51    - Comodo
024300060727      * 90    - OF
024400060727      * 99    - Comodo
024500060504      *---------------------------------------------------------------*
024600060504
024700060504     c     *Entry        plist
024800060504     c                   parm                    KPJBA
024900060518     c                   movel     kpjbu         Tnsf40ds
025000060504      *
025100060504      * Operazioni Iniziali
025200060504     c                   exsr      RoutInz
025300060504      *
025400060523     c                   exsr      Carcbo
025500060523      *
025600060518      * Elaborazione del file
025700060518     c
025800060518do  1c                   exsr      Elabora
025900060504      * 1° videata
026000060518     c                   exsr      Stampa
026100060504      * Fine
026200060509      *
026300060512     c     fine          tag
026400060801
026500060801     c                   If        Dutlpo <> 'S'
026600060801     c                   Clear                   comman
026700060801     c                   Movel(p)  cmdt(6)       comman
026800060801     c                   Call      'QCMDEXC'                            99
026900060801     c                   Parm                    comman
027000060801     c                   Parm                    lenght
027100060801     c                   endif
027200060801
027300060504     c                   movel     *on           *inLR
027400060504      *
027500060504      *
027600060504      *---------------------------------------------------------------*
027700060518      * Elabora ritassazione                                          *
027800060504      *---------------------------------------------------------------*
027900060518     c     Elabora       BEGSR
028000060504      *
028100060518e   1c     Krft          setll     wfrft01l
028200060504
028300060504     c                   do        *hival
028400060522      *
028500060518     c     krft          reade     wfrft01l
028600060518
028700060522     c                   If        %eof(Wfrft01l)
028800060518     c                   leave
028900060518     c                   endif
029000060518      * verifico se record già elaborato
029100060518     c                   if        rftela <> ' '
029200060518     c                   iter
029300060518     c                   endif
029400060726      * imposto i campi della testata
029500060726     c                   if        prtfiv = *zeros
029600060726     c                   eval      prtfiv = rftfiv
029700060726     c                   eval      prtnft = rftnft
029800060726      *
029900060726     c                   clear                   WLBdat
030000060726     c                   eval      G08inv = rftdft
030100060726     c                   eval      G08err = '3'
030200060726     c                   call      'XSRDA8'
030300060726     c                   parm                    WLBdat
030400060726     c                   z-add     G08dat        prtdft
030500081006      * se si desidera tassazione scaglionata recupero gli scaglioni della tariffa
030600081006     c                   If        i40sgl = 'S'
030700081006     c                   exsr      car_scaglioni
030800081006     c                   endif
030900081006
031000060726     c                   endif
031100060727      * controllo data fattura
031200060727     c                   eval      *in50 = rftdft < 20060430
031300060518      * tasso
031400060518     c                   exsr      tassaz
031500060504      *
031600060504     c                   enddo
031700060504      *
031800060504     c                   ENDSR
031900060504      *
032000060522      *---------------------------------------------------------------*
032100060522      * Stampa Finale
032200060522      *---------------------------------------------------------------*
032300060522     c     Stampa        BEGSR
032400060522
032500060522      * testata
032600060530     c                   write     sf41t0
032700060522     c                   write     sf41ts
032800060522     c                   write     sf41t3
032900060522     c                   write     sf41t2
033000060522     c                   write     sf41t3
033100060522      * valorizzo i campi
033200060522     c                   eval      prtnoj = knmeb
033300060522     c                   eval      prtpru = rftpru
033400060522      *
033500060522     c                   clear                   WLBdat
033600060522     c                   eval      G08inv = rftdta
033700060522     c                   eval      G08err = '3'
033800060522     c                   call      'XSRDA8'
033900060522     c                   parm                    WLBdat
034000060522     c                   z-add     G08dat        prtdta
034100060522      *
034200060522     c                   eval      prtora = rftora
034300060614      * calcolo la differente
034400060614     c                   eval      prtdiffe = prtimvf - prtimvr
034500060522      *
034600060522     c                   write     sf41d3
034700060522      *
034800081006      * stampo la suddivisione per scaglione solo se richiesta
034900081006     c                   If        i40sgl = 'S'
035000081006     c                   eval      cod = 'TR'
035100081006     c                   movel(p)  tipo_tar      key
035200081006     c     ktabkey       chain     Tabel00f
035300081006     c                   if        %found(tabel00f)
035400081006     c                   movel     tbluni        dstr
035500081006      * valorizzo i campi descrittivi di stampa
035600081006     c                   movel     §trdst        prttar
035700081006     c                   endif
035800081006      * testata
035900081006     c                   write     sf41t04
036000081006     c                   write     sf41t3
036100081006     c                   write     sf41t4
036200081006     c                   write     sf41t3
036300081006
036400081006      * stampo dettaglio per ogni scaglione
036500081006     c                   z-add     1             yy
036600081006     c                   dow       yy <= num_sgl
036700081006
036800081006     c                   eval      prtsca = sgl(yy)
036900081006     c                   eval      prtimf = imf(yy)
037000081006     c                   eval      prtimr = imr(yy)
037100081006     c                   eval      prtdif = (prtimf - prtimr)
037200081006     c                   eval      prttkg = tkg(yy)
037300081006     c                   eval      prttcl = tcl(yy)
037400081006     c                   eval      prttvl = tvl(yy)
037500081006     c                   eval      prtnsp = nsp(yy)
037600081006     c                   eval      prtnmt = nmt(yy)
037700081006
037800081006     c                   write     sf41d4
037900081006      * pulisco la descrizione della tariffa
038000081006     c                   clear                   prttar
038100081006
038200081006     c                   add       1             yy
038300081006
038400081006     c                   enddo
038500081006      *
038600081006      * stampo se ci sono delle bolle di recupero
038700081006     c                   If        prtnspr > 0
038800081006     c                   write     sf41d4r
038900081006     c                   endif
039000081006      *
039100081006     c                   endif
039200081006      *
039300060522     c                   Endsr
039400060522      *
039500060515      *---------------------------------------------------------------*
039600060518      *  Routine di tassazione
039700060515      *---------------------------------------------------------------*
039800060518     c     Tassaz        Begsr
039900060518      *
040000060518     c                   clear                   dtas
040100060518     c                   clear                   dtasv
040200060518      * aggancio titas
040300060518     c     KSPE          chain     titas30c
040400060522   a1c                   if        %found(titas30c)
040500070411      * Pulisco i campi dell'assicurazione se calcolata in fattura
040600070411    2c                   IF        Tasfcm = 'F'
040700070411     c                   clear                   tasias
040800070411     c                   clear                   tasvas
040900070411    2c                   Endif
041000070411
041100060530      * valorizzo i campi del file printer
041200060530      *
041300060530     c                   clear                   WLBdat
041400060530     c                   movel     tasaas        g08inv
041500060530     c                   move      tasmgs        g08inv
041600060530     c                   eval      G08err = '3'
041700060530     c                   call      'XSRDA8'
041800060530     c                   parm                    WLBdat
041900060530     c                   z-add     G08dat        prtdsp
042000060530
042100060530     c                   z-add     tasksc        prtksc
042200060530     c                   z-add     tasctr        prtctr
042300060530
042400060530     c                   clear                   WLBdat
042500060530     c                   movel     rftdspr       g08inv
042600060530     c                   eval      G08err = '3'
042700060530     c                   call      'XSRDA8'
042800060530     c                   parm                    WLBdat
042900060530     c                   z-add     G08dat        prtdspr
043000060530
043100060530      *
043200060519      * valorizzo la DS DTAS e DTASV
043300060519     c                   clear                   dtasv
043400060519     c                   clear                   taspor
043500060519     c                   clear                   tasimv
043600060518      *
043700060519      * cerco in TITAI l'immagine della spedizione prima della tassazione
043800060519     c                   exsr      RIC_tai
043900060519      * se tassata fino all'imponibile non tasso
044000060522    1c                   if        tasimv = 0
044100060519      ** Ricerca contrassegno
044200060519    2c                   IF        Tasfca <> *blanks
044300060519     c                   Exsr      Carcsb
044400060519    2c                   Endif
044500060519
044600060519      *  se si tratta di codice bolla che prevede solo una varia, passo solo
044700060519      *  quella
044800060519     c                   z-add     1             kk
044900060519     c     tascbo        lookup    cbo(kk)                                05
045000060522    2c                   if        *in05
045100060519     c                   eval      tassva=cBV(kk)
045200060522    2c                   endif
045300060522
045400060522     c                   movel     Tasftc        tastc1
045500060522     c                   Z-add     rftdft        tasdct
045600060726
045700060726     c                   eval      Tasksc = rftkscr
045800060726     c                   eval      Tasctr = rftctrr
045900061221     c                   movel     rftaas        tasdsp
046000061221     c                   move      rftmgs        tasdsp
046100060726
046200060522      * Flag SI NO DDT
046300060522    2C                   If        tasll1 = 'C' or tasll1 = 'S'
046400060522     C                   movel     'S'           Tasddt
046500060522   x2C                   Else
046600060522     C                   Clear                   Tasddt
046700060522    2C                   Endif
046800060926     c****               movel     tasfbr        dstasflo
046900060926     c                   clear                   dtas01
047000060926     c                   movel     tasfbr        §asfbr
047100060926     c                   movel     tascca        §ascca
047101110701      * valorizzo flag email al destinatario
047102110701     c                   movel     tasflo        dtasflo
047103110701     c                   move      §floemd       §asemd
047104110701
047200061221      * se data spedizione differente da ritassazione passo nel flo anche la data ritassazione
047300061221     c                   If        tasdsp <> rftdspr
047400070108     c                   move      rftdspr       §asdrt
047500061221     c                   endif
047600061221
047700060926     c                   eval      dstasflo = dtas01
047800060522
047900060522      * Bancali
048000060522    2c                   If        %Subst(TasGva:1:1) = 'B'
048100060522
048200060522     c                   eval      kAr5Trd  = 'BAN'
048300060522     c     kFiar5        Chain     Fiar531c
048400060522If  3c                   If        %Found(Fiar531c)
048500060522     c                   Movel     Ar5Uni        dAr5Ban
048600060522    3c                   EndIf
048700060522      * Bancali
048800060522     c                   Eval      TasBan = §Ar5Nba + §Ar5Nb2
048900060522    2c                   EndIf
049000060522      **
049100060522      * Colli Originali
049200060522    2c                   If        %Subst(TasGva:1:1) = 'O'
049300060522
049400060522     c                   eval      kAr5Trd  = 'BNB'
049500060522     c     kFiar5        Chain     Fiar531c
049600060522If  3c                   If        %Found(Fiar531c)
049700060522     c                   Movel     Ar5Uni        dAr5Bnb
049800060522    3c                   EndIf
049900060522      * Colli
050000060522     c                   Eval      TasNcl = §Ar5bcor
050100060522    2c                   EndIf
050200060522      *
050300060522      * verifico se è una bolla non tassata fino l'imponibile , una bolla di reso ,
050400060522      * con VARIA 'N' valorizzata con 888888 chiamo il TNSF20R per il calcolo dell'anteporto
050500060522      *
050600060522    2c                   if        tasimv = 0 and tasfbr = 'S'
050700060522     c                   z-add     1             xx
050800060522     c     'N'           lookup    sv(xx)                                 30
050900060522    3c                   if        *in30 and va(xx) =  88888888
051000060522      * tasso l'anteporto
051100060522     c                   exsr      Varian
051200060522    3c                   endif
051300060522      *
051400060522    2c                   endif
051500060522      *
051600060522      * se ritorno dal calcolo della varia N e c'è errore non proseguo
051700060522      *
051800060522     c                   Setoff                                       05
051900060522    2c                   if        taserr = ' '
052000060522      * Se devo tassare per una varia e c'e' gia', non richiamo la tassazione  se importo
052100060522      * della varia uguale  a zero
052200060522    3c                   if        tassva<>*blanks and tasimv=0
052300060522     c                   z-add     1             kk
052400060522     c     tassva        lookup    sv(kk)                                 05
052500060522    4c                   if        *in05 and va(kk) > 0
052600060522     c                   setoff                                       05
052700060522    4c                   endif
052800060522    3c                   endif
052900060522      * verifico se esiste la varia & con importo uguale a 8888888 e se si passo al
053000060522      * TNSF20R il flag di calcolo varia &
053100060522      *
053200060522     c                   setoff                                       51
053300060522     c                   z-add     1             kk
053400060522     c     '&'           lookup    sv(kk)                                 51
053500060522    3c                   if        *in51 and va(kk) = 88888888
053600060522     c                   eval      tasfcaa = 'S'
053700060522     c                   clear                   sv(kk)
053800060522     c                   clear                   va(kk)
053900060522    3c                   endif
054000060522      * verifico se devo calcolare l'addebito di lasciato avviso
054100060522     c                   movel     rftkscr       kscfil
054200060522      *
054300060522      * verifico se c'è evento
054400060522     c     Kevb4         chain     Fnevb04l
054500060522
054600060522    4c                   If        %found(Fnevb04l)
054700060522     c                   eval      tasric = 'S'
054800060522    4c                   endif
054900060522
055000060522      **
055100060522      **
055200060522    3c                   if        tassva=*blanks or not *in05
055300060522     c                   call      'TNSF20R'
055400060522     c                   parm                    Kpjba
055500060522     c                   parm                    Dtas
055600060522     c                   parm                    Dtasv
055700060522    3c                   endif
055800060522      **
055900060522    2c                   endif
056000060522    1c                   endif
056100060519
056200060522     c                   If        taserr = ' '
056300060517      * tassazione
056400060518     c                   eval      RFTdivr = tasdiv
056500060518     c                   eval      RFTporr = taspor
056600060518      * carico eventuali varie presenti in TITA7
056700060518     c                   movea     sv            rft_svr
056800060518     c                   movea     va            rft_var
056900060518      *
057000060518     c                   eval      RFTimvr = tasimv
057100060522     c                   endif
057200060522   a1c                   endif
057300060522
057400060518     c                   eval      RFTela  = 'S'
057500060522     c                   eval      RFTnoj  = Knmeb
057600060517
057700060518     c                   UPDATE    WFRFT000
057800060518
057900060522      * faccio i totali imponibili e conto le spedizioni manca tariffa
058000060522
058100060522     c                   If        taserr <> ' '
058200060522     c                   add       1             prtnrmt
058300060522     c                   endif
058400060522
058500060522     c                   add       RFTimvf       prtimvf
058600060522     c                   add       RFTimvr       prtimvr
058700060518
058800060530      * differenza
058900060530     c                   eval      diffe = rftimvf - rftimvr
059000060530
059100060530      * STAMPO
059200060530     c                   If        not *in99 or *in90
059300060530      * testata
059400060530      * testata
059500060530     c                   write     sf41t0
059600060530     c                   write     sf41ts
059700060530     c                   write     sf41t3
059800060530     c                   write     sf41t1
059900060530     c                   write     sf41t3
060000060530
060100060530     c                   seton                                        99
060200060530     c                   setoff                                       90
060300060530
060400060530     c                   endif
060500060530
060600060530     c                   write     sf41d1
060700081006
060800081006      * suddivido i valori per scaglioni di !!! in base alla tipologia della tariffa
060900081006
061000081006     c                   If        i40sgl = 'S'
061100081006      * se tipo bolla fa parte delle bolle non di recupero le sommo scaglionate
061200081006      * altrimenti le metto nelle spedizioni di recupero
061300081006     c     tascbo        lookup    cbo_ok                                 30
061400081006     c                   if        %found
061500081006
061600081006     c                   z-add     1             yy
061700081006     c                   eval      ok_scagl = *off
061800081006     c                   select
061900081006      * quantità / valore
062000081006     c                   when      tipo_tar = 5 or tipo_tar = 4
062100081006     c                   z-add     tasqft        misura
062200081006     c                   exsr      ric_sgl
062300081006      * peso
062400081006     c                   when      tipo_tar = 0 or tipo_tar = 3
062500081006     c                   z-add     taspkf        misura
062600081006     c                   exsr      ric_sgl
062700081006      * colli
062800081006     c                   when      tipo_tar = 1
062900081006     c                   z-add     tasncl        misura
063000081006     c                   exsr      ric_sgl
063100081006      * spedizione
063200100302     c                   when      tipo_tar = 2
063300081006     c                   z-add     1             misura
063400081006     c                   exsr      ric_sgl
063500081006     c                   endsl
063600081006
063700081006      * se scaglione trovato valorizzo i campi delle schiere
063800081006     c                   If        ok_scagl
063900081006      * imponibile vecchio
064000081006     c                   add       rftimvf       imf(yy)
064100081006      * imponibile nuovo
064200081006     c                   add       rftimvr       imr(yy)
064300081006      * peso
064400081006     c                   add       taspkf        tkg(yy)
064500081006      * colli
064600081006     c                   add       tasncl        tcl(yy)
064700081006      * volume
064800081006     c                   add       tasvlf        tvl(yy)
064900081006      * spedizioni
065000081006     c                   add       1             nsp(yy)
065100081006      * manca tariffa
065200081006     c                   if        taserr <> ' '
065300081006     c                   add       1             nmt(yy)
065400081006     c                   endif
065500081006
065600081006     c                   endif
065700081006
065800081006      *** se bolla di recupero
065900081006     c                   else
066000081006      * imponibile vecchio
066100081006     c                   add       rftimvf       prtimfr
066200081006      * imponibile nuovo
066300081006     c                   add       rftimvr       prtimrr
066400081006      * spedizioni
066500081006     c                   add       1             prtnspr
066600081006      * manca tariffa
066700081006     c                   if        taserr <> ' '
066800081006     c                   add       1             prtnmtr
066900081006     c                   endif
067000081006      ***
067100081006     c                   endif
067200081006
067300081006     c                   endif
067400081006
067500060530
067600060518     c                   Endsr
067700060515
067800060519      *
067900060519      *---------------------------------------------------------------*
068000060519      *  Recupero importi di tassazione dal file titai
068100060519      *---------------------------------------------------------------*
068200060519     c     Ric_tai       Begsr
068300060519      *
068400060519     c                   z-add     0             k
068500060519
068600060519     c     kspe          setll     titai01l
068700060519
068800060519     c                   If        %equal(titai01l)
068900060519
069000060519     c                   do        *hival
069100060519
069200060522     c     kspe          reade     titai01l
069300060519
069400060519     c                   If        %eof(titai01l)
069500060519     c                   leave
069600060519     c                   endif
069700060519      * imponibile
069800060519     c                   if        taisvt = '£'
069900060519     c                   eval      tasimv = taivat
070000060519     c                   endif
070100060519      * porto
070200060519     c                   if        taisvt = ' '
070300060519     c                   eval      taspor = taivat
070400060519     c                   endif
070500060519      * varie
070600060519     c                   if        taisvt <> ' ' and taisvt <> '£'
070700060519     c                   add       1             k
070800060519     c                   eval      sv(k) = taisvt
070900060519     c                   eval      va(k) = taivat
071000060519     c                   endif
071100060519
071200060519     c                   enddo
071300060519
071400060519     c                   else
071500060519      * se non è pretassata pulisco la divisa
071600060519     c                   clear                   tasdiv
071700060519     c                   endif
071800070426      * se tassata fino l'imponibile valorizzo il campo tasman del prtf e il campo rfttmaf del file
071900070426     c                   if        tasimv = 0
072000070426     c                   clear                   tasman
072100070426     c                   else
072200070502     c                   eval      tasman = 'S'
072300070426      * faccio il totale dei tassati manualmente
072400070426     c                   add       1             prtnrtm
072500070426     c                   endif
072600070426
072700070426     c                   eval      rfttmaf = tasman
072800060519
072900060519     c                   endsr
073000060519      *---------------------------------------------------------------*
073100060519      ** Riverca Contrassegno
073200060519      *---------------------------------------------------------------*
073300060519     c     Carcsb        Begsr
073400060519
073500060519     c     Kcsb          chain     Tncsb03l
073600060519     c                   If        %found(tncsb03l) and
073700060519      * solo per stesso tipo bolla
073800060519     c                             tastbl = csbtbl
073900060519     c                   movel     csbsta        Tassta
074000060519     c                   z-add     csbcas        Tascas
074100061016     c*****              movel     csbtpa        Tastic
074200060519     c                   if        csbfus <> *blanks
074300060519     c                   move      csbfus        tastic
074400060519     c                   else
074500060519     c                   move      csbtpi        tastic
074600060519     c                   end
074700060519      * mittente
074800060519     c                   movel     csbvca        tasvca
074900060519     c                   z-add     csbcmb        tascmb
075000060519
075100060519     c                   endif
075200060519
075300060519     c                   endsr
075400060522      *---------------------------------------------------------------*
075500060522      ** Calcolo anteporto VARIA 'N'
075600060522      *---------------------------------------------------------------*
075700060522     c     Varian        Begsr
075800060522
075900060522      * richiamo il programma di tassazione
076000060522      * senza passare le varie e il porto
076100060522      *
076200060522      * mi salvo le ds di tassazione
076300060522     c                   eval      w_dtas = dtas
076400060522     c                   eval      w_dtasv = dtasv
076500060522      * mi salvo la divisa
076600060522     c                   eval      w_tasdiv = tasdiv
076700060522
076701110701      * tolgo il flag dell'invio mail al destinatario nell'eventualità ci fosse
076702110701     c                   eval      dtas01 = dstasflo
076703110701     c                   clear                   §asemd
076704110701     c                   eval      dstasflo = dtas01
076705110701
076800060522     c                   clear                   dtasv
076900060522     c                   clear                   tasdiv
077000060522     c                   clear                   taspor
077100060522
077200060522     c                   clear                   antepo
077300060522
077400060522     c                   call      'TNSF20R'
077500060522     c                   parm                    kpjba
077600060522     c                   parm                    dtas
077700060522     c                   parm                    dtasv
077800060522      * se non ci sono errori valorizzo la varia 'N'
077900060522     c                   if        taserr = ' '
078000060522     c                   z-add     tasimv        antepo
078100060522     c                   endif
078200060522      * ripristino le ds salvate prima
078300060522     c                   eval      dtas = w_dtas
078400060522     c                   eval      dtasv = w_dtasv
078500060522      * valorizzo l'anteporto nella varia N al posto di 88888888  se maggiore di zero
078600060522     c                   if        antepo > 0
078700060522     c                   z-add     antepo        va(xx)
078800060522     c                   endif
078900060522
079000060522     c                   endsr
079100060519      *---------------------------------------------------------------*
079200060519      ** CARICAMENTO codici bolla che prevedono di tassare solo 1 varia
079300060519      *   e che permettono imponibile =0
079400081006      ** CARICAMENTO codici bolla che non sono c/s o recuperi per
079500081006      *   scaglionare
079600060519      *---------------------------------------------------------------*
079700060519     c     Carcbo        Begsr
079800081006      **
079900081006     c                   Movel     'TB'          Cod
080000081006     c                   z-add     0             z
080100081006     c     Ktab          setll     tabel00f
080200081006     c                   do        *hival
080300081006     c     Ktab          reade     tabel00f
080400081006      * fine file
080500081006     c                   If        %eof(tabel00f)
080600081006     c                   leave
080700081006     c                   endif
080800081006    1
080900081006    2c                   If        tblkey <> *blanks
081000081006     c                   Movel     TBLuni        DStb
081100081006      * imponibile a zero
081200081006    3c                   If        §tbrbl  = 'N'
081300081006     c                   add       1             z
081400081006     c                   movel     TBLkey        tbo_oK(z)
081500081006    3c                   endif
081600081006      **
081700081006    2c                   endif
081800081006    1c                   enddo
081900081006
082000060519      **
082100060519     c                   Movel     '3A'          Cod
082200060519     c                   z-add     0             K
082300060519     c                   z-add     0             KK
082400060519     c     Ktab          setll     tabel00f
082500060519     c                   do        *hival
082600060519     c     Ktab          reade     tabel00f
082700060519      * fine file
082800060519     c                   If        %eof(tabel00f)
082900060519     c                   leave
083000060519     c                   endif
083100060519    1
083200060519    2c                   If        tblkey <> *blanks
083300060519     c                   Movel     TBLuni        DS3a
083400060519      * imponibile a zero
083500060519    3c                   If        §3aim0  = 'S'
083600060519     c                   add       1             K
083700060519     c                   movel     TBLkey        im0(k)
083800060519    3c                   endif
083900060519      * bolla con singola varia
084000060519     c                   If        §3asva  <> *blanks
084100060519     c                   add       1             Kk
084200060519     c                   movel     tblkey        cbo(kk)
084300060519     c                   movel     §3asva        cbv(kk)
084400060519    3c                   endif
084500081006      * bolla con tipo bolla normale
084600081006     c     §3atb1        lookup    tbo_ok                                 30
084700081006     c                   if        %found
084800081006     c                   add       1             zz
084900081006     c                   movel     tblkey        cbo_ok(zz)
085000081006     c                   endif
085100060519      **
085200060519    2c                   endif
085300060519    1c                   enddo
085400060519
085500060519     c                   Endsr
085600081006      *---------------------------------------------------------------*
085700081006      * Caricamento scaglioni
085800081006      *---------------------------------------------------------------*
085900081006     c     Car_scaglioni BEGSR
086000081006
086100081006     c                   eval      savprg = 999
086200081006     c                   clear                   yy
086300081006     c                   clear                   tipo_tar
086400081006     c                   clear                   num_sgl
086500081006      * cerco il giusto progressivo
086600081006
086700081006     c     ktam          setll     tntam01l
086800081006     c                   do        *hival
086900081006     c     ktam          reade     tntam01l
087000081006     c                   if        %eof(tntam01l)
087100081006     c                   leave
087200081006     c                   endif
087300081006      * verifico se progressivo annullato
087400081006     c                   if        tamatb <> ' '
087500081006     c                   iter
087600081006     c                   endif
087700081006      * controllo con la data di riferimento
087800081006     c                   If        rftdspr  < tamdst and rftdspr > tamddt
087900081006     c                   eval      savprg = tamprg
088000081006     c                   leave
088100081006     c                   endif
088200081006     c                   if        rftdspr > tamdst
088300081006     c                   eval      savprg = tamprg
088400081006     c                   iter
088500081006     c                   endif
088600081006     c                   enddo
088700081006
088800081006     c                   if        savprg <> 999
088900081006      * trovato progressivo carico gli scaglioni relativi leggendo da titad
089000081006     c     ktad          setll     titad04l
089001110713     c                   do        *hival
089100081006     c     ktad          reade     titad04l
089101110713     c                   if        %eof(titad04l)
089102110713     c                   leave
089103110713     c                   endif
089200110713     c                   if        tadatb = ' '
089300081006      * recupero la chiave
089400081006     c                   eval      savlnp = tadlnp
089500081006     c                   eval      savorp = tadorp
089600081006     c                   eval      savnaz = tadnaz
089700081006     c                   eval      savcap = tadcap
089701110713     c                   leave
089702110713     c                   endif
089703110713     c                   enddo
089800081006
090000081006      * leggo tutti gli scaglioni con la stessa chiave
090100081006     c     ktad2         setll     titad04l
090200081006
090300081006     c                   do        *hival
090400081006     c     ktad2         reade     titad04l
090500081006
090600081006     c                   if        %eof(titad04l)
090700081006     c                   leave
090800081006     c                   endif
090900081006
091000081006     c                   if        tadatb <> ' '
091100081006     c                   iter
091200081006     c                   endif
091300081006
091400081006     c     tadsgl        lookup    sgl                                    31
091500081006
091600081006     c                   if        %found
091700081006     c                   leave
091800081006     c                   endif
091900081006
092000081006     c                   add       1             yy
092100081006     c                   eval      sgl(yy) = tadsgl
092200081006     c                   enddo
092300081006      * valorizzo il numero totali di scaglioni
092400081006     c                   z-add     yy            num_sgl
092500081006
092600081006     c                   movel     rftctrr       tipo_tar
092700081006
092800081006     c                   endif
092900081006      *
093000081006     c                   endsr
093100081006      *---------------------------------------------------------------*
093200081006      *  Ricerca dello scaglione per spedizione tassata
093300081006      *---------------------------------------------------------------*
093400081006     c     Ric_sgl       Begsr
093500081006
093600081006     c                   z-add     1             yy
093700081006
093800081006     c                   dow       yy <= num_sgl
093900081006     c                   if        misura <= sgl(yy)
094000081006     c                   eval      ok_scagl = *on
094100081006     c                   leave
094200081006     c                   endif
094300081006     c                   add       1             yy
094400081006     c                   enddo
094500081006
094600081006     c                   endsr
094700081006      *
094800060519      *---------------------------------------------------------------*
094900060519      * Operazioni Iniziali                                           *
095000060519      *---------------------------------------------------------------*
095100060519     c     RoutInz       BEGSR
095200060519      *
095300060519      * Reperisco dati job
095400060519     c                   exsr      DatiJob
095500060519      * Reperisco data e ora
095600060519     c                   time                    w0140
095700060519     c                   movel     w0140         wora
095800060522     c                   move      w0140         wdate
095900060519      *
096000060801      * verifico se sono in filiale faccio le ovrdbf per i file di sede
096100060801     c                   If        dutlpo <> 'S'
096200060801      * librerie
096300060801
096400060801      * Se sono in ambiente buono - GAITRAGRU
096500060801     c                   If        knsif = 'FILTRA201 '
096600060801     c                   Eval      wlib = 'GAITRAGRU '
096700060801     c                   Eval      wlib1 = 'GAITRAAZM '
096800060801      *  Se sono in ambiente di prova - GAITRAGRPS
096900060801     c                   Else
097000060801     c                   Eval      wlib = 'GAITRAGRPS'
097100060801     c                   Eval      wlib1 = 'GAITRAAZM '
097200060801     c                   EndIf
097300060801      * WFRFT01L
097400060801     c                   Clear                   comman
097500060801     c                   Movel(p)  cmdt(1)       comman
097600060801     c                   Eval      %Subst(comman:30:10) = wlib1
097700060801     c                   Eval      comman =
097800060801     c                             %trim(comman) + '/WFRFT01L)'
097900060801     c                   Call      'QCMDEXC'                            99
098000060801     c                   Parm                    comman
098100060801     c                   Parm                    lenght
098200060801
098300060801      * TITAI01L
098400060801     c                   Clear                   comman
098500060801     c                   Movel(p)  cmdt(2)       comman
098600060801     c                   Eval      %Subst(comman:30:10) = wlib
098700060801     c                   Eval      comman =
098800060801     c                             %trim(comman) + '/TITAI01L)'
098900060801     c                   Call      'QCMDEXC'                            99
099000060801     c                   Parm                    comman
099100060801     c                   Parm                    lenght
099200060801
099300060801      * TITAS30C
099400060801     c                   Clear                   comman
099500060801     c                   Movel(p)  cmdt(3)       comman
099600060801     c                   Eval      %Subst(comman:30:10) = wlib
099700060801     c                   Eval      comman =
099800060801     c                             %trim(comman) + '/TITAS30C)'
099900060801     c                   Call      'QCMDEXC'                            99
100000060801     c                   Parm                    comman
100100060801     c                   Parm                    lenght
100200060801      * FIAR531C
100300060801     c                   Clear                   comman
100400060801     c                   Movel(p)  cmdt(4)       comman
100500060801     c                   Eval      %Subst(comman:30:10) = wlib
100600060801     c                   Eval      comman =
100700060801     c                             %trim(comman) + '/FIAR531C)'
100800060801     c                   Call      'QCMDEXC'                            99
100900060801     c                   Parm                    comman
101000060801     c                   Parm                    lenght
101100060801      * TNCSB03L
101200060801     c                   Clear                   comman
101300060801     c                   Movel(p)  cmdt(5)       comman
101400060801     c                   Eval      %Subst(comman:30:10) = wlib
101500060801     c                   Eval      comman =
101600060801     c                             %trim(comman) + '/TNCSB03L)'
101700060801     c                   Call      'QCMDEXC'                            99
101800060801     c                   Parm                    comman
101900060801     c                   Parm                    lenght
102000060801
102100060801     c                   endif
102200060801      *
102300060801     c                   open      wfrft01l
102400060801     c                   open      titai01l
102500060801     c                   open      titas30c
102600060801     c                   open      fiar531c
102700060801     c                   open      tncsb03l
102800060801
102900060519     c                   ENDSR
103000060519      *
103100060519      *---------------------------------------------------------------*
103200060519      * Reperimento Dati del job (Utente/Operativi)                   *
103300060519      *---------------------------------------------------------------*
103400060519     c     DatiJob       BEGSR
103500060519      *
103600060519     c     *dtaara       define    §azute        azuteds
103700060519     c     *dtaara       define    §datiute      ddatiute
103800060519      *
103900060519     c                   in(E)     *dtaara
104000060519     c                   IF        %ERROR or RSUT = *blanks
104100060519     c                   clear                   Tibs34Ds
104200060519     c                   call      'TIBS34R'
104300060519     c                   parm                    Tibs34Ds
104400060519     c                   in        *dtaara
104500060519     c                   Endif
104600060519
104700060519      *
104800060519     c                   ENDSR
104900060801** cmdt
105000060801OVRDBF FILE(WFRFT01L) TOFILE(
105100060801OVRDBF FILE(TITAI01L) TOFILE(
105200060801OVRDBF FILE(TITAS30C) TOFILE(
105300060801OVRDBF FILE(FIAR531C) TOFILE(
105400060801OVRDBF FILE(TNCSB03L) TOFILE(
105500060801DLTOVR FILE(*ALL)
