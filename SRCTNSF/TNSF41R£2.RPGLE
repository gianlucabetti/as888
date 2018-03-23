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
006400110701      ** DS del tasflo del file TITAS00F
006500110701     D DTASFLO       E DS
006600060518      ** DS Calcolo tassazione  - Varie
006700060518     D DTAS          E DS
006800060518     D  dstasflo     E                     extfld(tasflo)
006900060926      ** DS Flag operativi DS DTAS
007000060926     d Dtas01        e ds
007100060926
007200060518     d Dtasv         e ds
007300060518     d  sv                     1     20
007400060518     d                                     Dim(20)                              Sigle varie
007500060518     d  va                    21    140P 3
007600060518     d                                     Dim(20)                              Importi varie
007700060522      * recupero bancali e colli originali
007800060522     d dAr5Ban       e ds
007900060522
008000060522     d dAr5Bnb       e ds
008100060518      * - Ds per scrittura file di lavoro
008200060518     d                 ds
008300060518     d  RFTsv1r                1      1
008400060518     d  RFTsv2r                2      2
008500060518     d  RFTsv3r                3      3
008600060518     d  RFTsv4r                4      4
008700060518     d  RFTsv5r                5      5
008800060518     d  RFTsv6r                6      6
008900060518     d  RFTsv7r                7      7
009000060518     d  RFTsv8r                8      8
009100060518     d  RFTsv9r                9      9
009200060518     d  RFTs10r               10     10
009300060518     d  RFTs11r               11     11
009400060518     d  RFTs12r               12     12
009500060518     d  RFTs13r               13     13
009600060518     d  RFTs14r               14     14
009700060518     d  RFTs15r               15     15
009800060518     d  RFTs16r               16     16
009900060518     d  RFTs17r               17     17
010000060518     d  RFTs18r               18     18
010100060518     d  RFTs19r               19     19
010200060518     d  RFTs20r               20     20
010300060518     d  RFT_svr                1     20    Dim(20)                              Sigle varie
010400060512
010500060518      * - Ds per scrittura file di lavoro
010600060518     d                 ds
010700060518     d  RFTva1r                1     11  3
010800060518     d  RFTva2r               12     22  3
010900060518     d  RFTva3r               23     33  3
011000060518     d  RFTva4r               34     44  3
011100060518     d  RFTva5r               45     55  3
011200060518     d  RFTva6r               56     66  3
011300060518     d  RFTva7r               67     77  3
011400060518     d  RFTva8r               78     88  3
011500060518     d  RFTva9r               89     99  3
011600060518     d  RFTv10r              100    110  3
011700060518     d  RFTv11r              111    121  3
011800060518     d  RFTv12r              122    132  3
011900060518     d  RFTv13r              133    143  3
012000060518     d  RFTv14r              144    154  3
012100060518     d  RFTv15r              155    165  3
012200060518     d  RFTv16r              166    176  3
012300060518     d  RFTv17r              177    187  3
012400060518     d  RFTv18r              188    198  3
012500060518     d  RFTv19r              199    209  3
012600060518     d  RFTv20r              210    220  3
012700060518     d  RFT_var                1    220  3 Dim(20)                              Importo varie
012800060504      *
012900060504      * - Controllo/Inversione date
013000060504     d WLBdat          ds                  inz
013100060504     d   G08dat                       8  0 inz
013200060504     d   G08inv                       8  0 inz
013300060504     d   G08err                       1    inz('3')
013400060504     d   G08tgi                       5  0 inz
013500060504      *
013600060504      * - Variabili definite a programma
013700060519     d codut           s              1  0 inz(1)
013800060504     d Wdate           s              8  0 inz
013900060518     d Wora            s              6  0 inz
014000060516     d W0140           s             14  0 inz
014100081006     d z               s              4  0
014200081006     d zz              s              4  0
014300060518     d k               s              4  0
014400060519     d kk              s              4  0
014500060522     d xx              s              4  0
014600081006     d yy              s              4  0
014700081006     d num_sgl         s              4  0
014800060522     d kscfil          s              3  0
014900081006     d tipo_tar        s              1  0
015000081006     d ok_scagl        s               n
015100060522     d cod             s                   like(tblcod)
015200081006     d key             s                   like(tblkey)
015300060522     d kAr5Trd         s                   Like(Ar5Trd)
015400060522     d antepo          s                   like(tasimv)
015500060522     d W_tasdiv        s                   like(tasdiv)
015600060522     d W_dtas          s                   like(dtas)
015700060522     d W_dtasv         s                   like(dtasv)
015800060522     D Wcev            s                   like(evbcev) inz('RIC')
015900081006     d prg2            s                   like(tamprg) inz
016000081006     d ctr2            s                   like(tamctr) inz
016100081006     d ksc2            s                   like(tamksc) inz
016200081006     d savprg          s                   like(tamprg)
016300081006     d savlnp          s                   like(tadlnp)
016400081006     d savorp          s                   like(tadorp)
016500081006     d savnaz          s                   like(tadnaz)
016600081006     d savcap          s                   like(tadcap)
016700081006     d misura          s                   like(tadsgl)
016800060801
016900060801     d wlib            s             10
017000060801     d wlib1           s             10
017100060801     d comman          s             80
017200060801     d lenght          s             15  5 inz(80)
017300060801
017400060522
017500060504      *
017600060504      * Key-List - - - - - - - - - - - - - - - - - - - - - - - - - - -*
017700060504      *
017800060518      * - WFRFT01L
017900060518     c     KRFT          Klist
018000060518     c                   Kfld                    I40noj
018100060518     c                   Kfld                    I40pru
018200060518     c                   Kfld                    I40dta
018300060518     c                   Kfld                    I40ora
018400060517
018500060518      * - TITAI01L / TITAS30C
018600060518     c     KSPE          Klist
018700060518     c                   Kfld                    RFTaas
018800060518     c                   Kfld                    RFTlnp
018900060518     c                   Kfld                    RFTnrs
019000060518     c                   Kfld                    RFTnsp
019100060518     c                   Kfld                    RFTtbl
019200060519      * Tncsb03l
019300060519     c     KCSB          Klist
019400060519     c                   Kfld                    TASaas
019500060519     c                   Kfld                    TASlnp
019600060519     c                   Kfld                    TASnrs
019700060519     c                   Kfld                    TASnsp
019800060519      * Tabel00f
019900060519     c     KTAB          Klist
020000060519     c                   Kfld                    codut
020100060519     c                   Kfld                    cod
020200081006     c     KTABkey       Klist
020300081006     c                   Kfld                    codut
020400081006     c                   Kfld                    cod
020500081006     c                   Kfld                    key
020600060522      * Fiar531c
020700060522     c     kFiar5        Klist
020800060522     c                   Kfld                    TasAas
020900060522     c                   Kfld                    TasLnp
021000060522     c                   Kfld                    TasNrs
021100060522     c                   Kfld                    TasNsp
021200060522     c                   Kfld                    kAr5Trd
021300060522      * Chiave file eventi FNEVB
021400060522     c     Kevb4         Klist
021500060522     c                   Kfld                    Tasaas
021600060522     c                   Kfld                    Taslnp
021700060522     c                   Kfld                    Tasnrs
021800060522     c                   Kfld                    Tasnsp
021900060522     c                   Kfld                    Wcev
022000081006      ** accesso tntam x cliente
022100081006     c     Ktam          Klist
022200081006     c                   Kfld                    rftkscr
022300081006     c                   Kfld                    rftctrr
022400081006      ** accesso titad
022500081006     c     Ktad          Klist
022600081006     c                   Kfld                    tamksc
022700081006     c                   Kfld                    tamctr
022800081006     c                   Kfld                    tamprg
022900081006
023000081006     c     Ktad2         Klist
023100081006     c                   Kfld                    tamksc
023200081006     c                   Kfld                    tamctr
023300081006     c                   Kfld                    tamprg
023400081006     c                   Kfld                    savlnp
023500081006     c                   Kfld                    savorp
023600081006     c                   Kfld                    savnaz
023700081006     c                   Kfld                    savcap
023800060504
023900060504      *---------------------------------------------------------------*
024000060504      *  RIEPILOGO INDICATORI                                         *
024100060504      *---------------------------------------------------------------*
024200060727      * 05/30 - Comodo
024300060727      * 50    - Data fattura antecedente 30-04-06 stampo scritta
024400060727      * 51    - Comodo
024500060727      * 90    - OF
024600060727      * 99    - Comodo
024700060504      *---------------------------------------------------------------*
024800060504
024900060504     c     *Entry        plist
025000060504     c                   parm                    KPJBA
025100060518     c                   movel     kpjbu         Tnsf40ds
025200060504      *
025300060504      * Operazioni Iniziali
025400060504     c                   exsr      RoutInz
025500060504      *
025600060523     c                   exsr      Carcbo
025700060523      *
025800060518      * Elaborazione del file
025900060518     c
026000060518do  1c                   exsr      Elabora
026100060504      * 1° videata
026200060518     c                   exsr      Stampa
026300060504      * Fine
026400060509      *
026500060512     c     fine          tag
026600060801
026700060801     c                   If        Dutlpo <> 'S'
026800060801     c                   Clear                   comman
026900060801     c                   Movel(p)  cmdt(6)       comman
027000060801     c                   Call      'QCMDEXC'                            99
027100060801     c                   Parm                    comman
027200060801     c                   Parm                    lenght
027300060801     c                   endif
027400060801
027500060504     c                   movel     *on           *inLR
027600060504      *
027700060504      *
027800060504      *---------------------------------------------------------------*
027900060518      * Elabora ritassazione                                          *
028000060504      *---------------------------------------------------------------*
028100060518     c     Elabora       BEGSR
028200060504      *
028300060518e   1c     Krft          setll     wfrft01l
028400060504
028500060504     c                   do        *hival
028600060522      *
028700060518     c     krft          reade     wfrft01l
028800060518
028900060522     c                   If        %eof(Wfrft01l)
029000060518     c                   leave
029100060518     c                   endif
029200060518      * verifico se record già elaborato
029300060518     c                   if        rftela <> ' '
029400060518     c                   iter
029500060518     c                   endif
029600060726      * imposto i campi della testata
029700060726     c                   if        prtfiv = *zeros
029800060726     c                   eval      prtfiv = rftfiv
029900060726     c                   eval      prtnft = rftnft
030000060726      *
030100060726     c                   clear                   WLBdat
030200060726     c                   eval      G08inv = rftdft
030300060726     c                   eval      G08err = '3'
030400060726     c                   call      'XSRDA8'
030500060726     c                   parm                    WLBdat
030600060726     c                   z-add     G08dat        prtdft
030700081006      * se si desidera tassazione scaglionata recupero gli scaglioni della tariffa
030800081006     c                   If        i40sgl = 'S'
030900081006     c                   exsr      car_scaglioni
031000081006     c                   endif
031100081006
031200060726     c                   endif
031300060727      * controllo data fattura
031400060727     c                   eval      *in50 = rftdft < 20060430
031500060518      * tasso
031600060518     c                   exsr      tassaz
031700060504      *
031800060504     c                   enddo
031900060504      *
032000060504     c                   ENDSR
032100060504      *
032200060522      *---------------------------------------------------------------*
032300060522      * Stampa Finale
032400060522      *---------------------------------------------------------------*
032500060522     c     Stampa        BEGSR
032600060522
032700060522      * testata
032800060530     c                   write     sf41t0
032900060522     c                   write     sf41ts
033000060522     c                   write     sf41t3
033100060522     c                   write     sf41t2
033200060522     c                   write     sf41t3
033300060522      * valorizzo i campi
033400060522     c                   eval      prtnoj = knmeb
033500060522     c                   eval      prtpru = rftpru
033600060522      *
033700060522     c                   clear                   WLBdat
033800060522     c                   eval      G08inv = rftdta
033900060522     c                   eval      G08err = '3'
034000060522     c                   call      'XSRDA8'
034100060522     c                   parm                    WLBdat
034200060522     c                   z-add     G08dat        prtdta
034300060522      *
034400060522     c                   eval      prtora = rftora
034500060614      * calcolo la differente
034600060614     c                   eval      prtdiffe = prtimvf - prtimvr
034700060522      *
034800060522     c                   write     sf41d3
034900060522      *
035000081006      * stampo la suddivisione per scaglione solo se richiesta
035100081006     c                   If        i40sgl = 'S'
035200081006     c                   eval      cod = 'TR'
035300081006     c                   movel(p)  tipo_tar      key
035400081006     c     ktabkey       chain     Tabel00f
035500081006     c                   if        %found(tabel00f)
035600081006     c                   movel     tbluni        dstr
035700081006      * valorizzo i campi descrittivi di stampa
035800081006     c                   movel     §trdst        prttar
035900081006     c                   endif
036000081006      * testata
036100081006     c                   write     sf41t04
036200081006     c                   write     sf41t3
036300081006     c                   write     sf41t4
036400081006     c                   write     sf41t3
036500081006
036600081006      * stampo dettaglio per ogni scaglione
036700081006     c                   z-add     1             yy
036800081006     c                   dow       yy <= num_sgl
036900081006
037000081006     c                   eval      prtsca = sgl(yy)
037100081006     c                   eval      prtimf = imf(yy)
037200081006     c                   eval      prtimr = imr(yy)
037300081006     c                   eval      prtdif = (prtimf - prtimr)
037400081006     c                   eval      prttkg = tkg(yy)
037500081006     c                   eval      prttcl = tcl(yy)
037600081006     c                   eval      prttvl = tvl(yy)
037700081006     c                   eval      prtnsp = nsp(yy)
037800081006     c                   eval      prtnmt = nmt(yy)
037900081006
038000081006     c                   write     sf41d4
038100081006      * pulisco la descrizione della tariffa
038200081006     c                   clear                   prttar
038300081006
038400081006     c                   add       1             yy
038500081006
038600081006     c                   enddo
038700081006      *
038800081006      * stampo se ci sono delle bolle di recupero
038900081006     c                   If        prtnspr > 0
039000081006     c                   write     sf41d4r
039100081006     c                   endif
039200081006      *
039300081006     c                   endif
039400081006      *
039500060522     c                   Endsr
039600060522      *
039700060515      *---------------------------------------------------------------*
039800060518      *  Routine di tassazione
039900060515      *---------------------------------------------------------------*
040000060518     c     Tassaz        Begsr
040100060518      *
040200060518     c                   clear                   dtas
040300060518     c                   clear                   dtasv
040400060518      * aggancio titas
040500060518     c     KSPE          chain     titas30c
040600060522   a1c                   if        %found(titas30c)
040700070411      * Pulisco i campi dell'assicurazione se calcolata in fattura
040800070411    2c                   IF        Tasfcm = 'F'
040900070411     c                   clear                   tasias
041000070411     c                   clear                   tasvas
041100070411    2c                   Endif
041200070411
041300060530      * valorizzo i campi del file printer
041400060530      *
041500060530     c                   clear                   WLBdat
041600060530     c                   movel     tasaas        g08inv
041700060530     c                   move      tasmgs        g08inv
041800060530     c                   eval      G08err = '3'
041900060530     c                   call      'XSRDA8'
042000060530     c                   parm                    WLBdat
042100060530     c                   z-add     G08dat        prtdsp
042200060530
042300060530     c                   z-add     tasksc        prtksc
042400060530     c                   z-add     tasctr        prtctr
042500060530
042600060530     c                   clear                   WLBdat
042700060530     c                   movel     rftdspr       g08inv
042800060530     c                   eval      G08err = '3'
042900060530     c                   call      'XSRDA8'
043000060530     c                   parm                    WLBdat
043100060530     c                   z-add     G08dat        prtdspr
043200060530
043300060530      *
043400060519      * valorizzo la DS DTAS e DTASV
043500060519     c                   clear                   dtasv
043600060519     c                   clear                   taspor
043700060519     c                   clear                   tasimv
043800060518      *
043900060519      * cerco in TITAI l'immagine della spedizione prima della tassazione
044000060519     c                   exsr      RIC_tai
044100060519      * se tassata fino all'imponibile non tasso
044200060522    1c                   if        tasimv = 0
044300060519      ** Ricerca contrassegno
044400060519    2c                   IF        Tasfca <> *blanks
044500060519     c                   Exsr      Carcsb
044600060519    2c                   Endif
044700060519
044800060519      *  se si tratta di codice bolla che prevede solo una varia, passo solo
044900060519      *  quella
045000060519     c                   z-add     1             kk
045100060519     c     tascbo        lookup    cbo(kk)                                05
045200060522    2c                   if        *in05
045300060519     c                   eval      tassva=cBV(kk)
045400060522    2c                   endif
045500060522
045600060522     c                   movel     Tasftc        tastc1
045700060522     c                   Z-add     rftdft        tasdct
045800060726
045900060726     c                   eval      Tasksc = rftkscr
046000060726     c                   eval      Tasctr = rftctrr
046100061221     c                   movel     rftaas        tasdsp
046200061221     c                   move      rftmgs        tasdsp
046300060726
046400060522      * Flag SI NO DDT
046500060522    2C                   If        tasll1 = 'C' or tasll1 = 'S'
046600060522     C                   movel     'S'           Tasddt
046700060522   x2C                   Else
046800060522     C                   Clear                   Tasddt
046900060522    2C                   Endif
047000060926     c****               movel     tasfbr        dstasflo
047100060926     c                   clear                   dtas01
047200060926     c                   movel     tasfbr        §asfbr
047300060926     c                   movel     tascca        §ascca
047400110701      * valorizzo flag email al destinatario
047500110701     c                   movel     tasflo        dtasflo
047600110701     c                   move      §floemd       §asemd
047700110701
047800061221      * se data spedizione differente da ritassazione passo nel flo anche la data ritassazione
047900061221     c                   If        tasdsp <> rftdspr
048000070108     c                   move      rftdspr       §asdrt
048100061221     c                   endif
048200061221
048300060926     c                   eval      dstasflo = dtas01
048400060522
048500060522      * Bancali
048600060522    2c                   If        %Subst(TasGva:1:1) = 'B'
048700060522
048800060522     c                   eval      kAr5Trd  = 'BAN'
048900060522     c     kFiar5        Chain     Fiar531c
049000060522If  3c                   If        %Found(Fiar531c)
049100060522     c                   Movel     Ar5Uni        dAr5Ban
049200060522    3c                   EndIf
049300060522      * Bancali
049400060522     c                   Eval      TasBan = §Ar5Nba + §Ar5Nb2
049500060522    2c                   EndIf
049600060522      **
049700060522      * Colli Originali
049800060522    2c                   If        %Subst(TasGva:1:1) = 'O'
049900060522
050000060522     c                   eval      kAr5Trd  = 'BNB'
050100060522     c     kFiar5        Chain     Fiar531c
050200060522If  3c                   If        %Found(Fiar531c)
050300060522     c                   Movel     Ar5Uni        dAr5Bnb
050400060522    3c                   EndIf
050500060522      * Colli
050600060522     c                   Eval      TasNcl = §Ar5bcor
050700060522    2c                   EndIf
050800060522      *
050900060522      * verifico se è una bolla non tassata fino l'imponibile , una bolla di reso ,
051000060522      * con VARIA 'N' valorizzata con 888888 chiamo il TNSF20R per il calcolo dell'anteporto
051100060522      *
051200060522    2c                   if        tasimv = 0 and tasfbr = 'S'
051300060522     c                   z-add     1             xx
051400060522     c     'N'           lookup    sv(xx)                                 30
051500060522    3c                   if        *in30 and va(xx) =  88888888
051600060522      * tasso l'anteporto
051700060522     c                   exsr      Varian
051800060522    3c                   endif
051900060522      *
052000060522    2c                   endif
052100060522      *
052200060522      * se ritorno dal calcolo della varia N e c'è errore non proseguo
052300060522      *
052400060522     c                   Setoff                                       05
052500060522    2c                   if        taserr = ' '
052600060522      * Se devo tassare per una varia e c'e' gia', non richiamo la tassazione  se importo
052700060522      * della varia uguale  a zero
052800060522    3c                   if        tassva<>*blanks and tasimv=0
052900060522     c                   z-add     1             kk
053000060522     c     tassva        lookup    sv(kk)                                 05
053100060522    4c                   if        *in05 and va(kk) > 0
053200060522     c                   setoff                                       05
053300060522    4c                   endif
053400060522    3c                   endif
053500060522      * verifico se esiste la varia & con importo uguale a 8888888 e se si passo al
053600060522      * TNSF20R il flag di calcolo varia &
053700060522      *
053800060522     c                   setoff                                       51
053900060522     c                   z-add     1             kk
054000060522     c     '&'           lookup    sv(kk)                                 51
054100060522    3c                   if        *in51 and va(kk) = 88888888
054200060522     c                   eval      tasfcaa = 'S'
054300060522     c                   clear                   sv(kk)
054400060522     c                   clear                   va(kk)
054500060522    3c                   endif
054600060522      * verifico se devo calcolare l'addebito di lasciato avviso
054700060522     c                   movel     rftkscr       kscfil
054800060522      *
054900060522      * verifico se c'è evento
055000060522     c     Kevb4         chain     Fnevb04l
055100060522
055200060522    4c                   If        %found(Fnevb04l)
055300060522     c                   eval      tasric = 'S'
055400060522    4c                   endif
055500060522
055600060522      **
055700060522      **
055800060522    3c                   if        tassva=*blanks or not *in05
055900060522     c                   call      'TNSF20R'
056000060522     c                   parm                    Kpjba
056100060522     c                   parm                    Dtas
056200060522     c                   parm                    Dtasv
056300060522    3c                   endif
056400060522      **
056500060522    2c                   endif
056600060522    1c                   endif
056700060519
056800060522     c                   If        taserr = ' '
056900060517      * tassazione
057000060518     c                   eval      RFTdivr = tasdiv
057100060518     c                   eval      RFTporr = taspor
057200060518      * carico eventuali varie presenti in TITA7
057300060518     c                   movea     sv            rft_svr
057400060518     c                   movea     va            rft_var
057500060518      *
057600060518     c                   eval      RFTimvr = tasimv
057700060522     c                   endif
057800060522   a1c                   endif
057900060522
058000060518     c                   eval      RFTela  = 'S'
058100060522     c                   eval      RFTnoj  = Knmeb
058200060517
058300060518     c                   UPDATE    WFRFT000
058400060518
058500060522      * faccio i totali imponibili e conto le spedizioni manca tariffa
058600060522
058700060522     c                   If        taserr <> ' '
058800060522     c                   add       1             prtnrmt
058900060522     c                   endif
059000060522
059100060522     c                   add       RFTimvf       prtimvf
059200060522     c                   add       RFTimvr       prtimvr
059300060518
059400060530      * differenza
059500060530     c                   eval      diffe = rftimvf - rftimvr
059600060530
059700060530      * STAMPO
059800060530     c                   If        not *in99 or *in90
059900060530      * testata
060000060530      * testata
060100060530     c                   write     sf41t0
060200060530     c                   write     sf41ts
060300060530     c                   write     sf41t3
060400060530     c                   write     sf41t1
060500060530     c                   write     sf41t3
060600060530
060700060530     c                   seton                                        99
060800060530     c                   setoff                                       90
060900060530
061000060530     c                   endif
061100060530
061200060530     c                   write     sf41d1
061300081006
061400081006      * suddivido i valori per scaglioni di !!! in base alla tipologia della tariffa
061500081006
061600081006     c                   If        i40sgl = 'S'
061700081006      * se tipo bolla fa parte delle bolle non di recupero le sommo scaglionate
061800081006      * altrimenti le metto nelle spedizioni di recupero
061900081006     c     tascbo        lookup    cbo_ok                                 30
062000081006     c                   if        %found
062100081006
062200081006     c                   z-add     1             yy
062300081006     c                   eval      ok_scagl = *off
062400081006     c                   select
062500081006      * quantità / valore
062600081006     c                   when      tipo_tar = 5 or tipo_tar = 4
062700081006     c                   z-add     tasqft        misura
062800081006     c                   exsr      ric_sgl
062900081006      * peso
063000081006     c                   when      tipo_tar = 0 or tipo_tar = 3
063100081006     c                   z-add     taspkf        misura
063200081006     c                   exsr      ric_sgl
063300081006      * colli
063400081006     c                   when      tipo_tar = 1
063500081006     c                   z-add     tasncl        misura
063600081006     c                   exsr      ric_sgl
063700081006      * spedizione
063800100302     c                   when      tipo_tar = 2
063900081006     c                   z-add     1             misura
064000081006     c                   exsr      ric_sgl
064100081006     c                   endsl
064200081006
064300081006      * se scaglione trovato valorizzo i campi delle schiere
064400081006     c                   If        ok_scagl
064500081006      * imponibile vecchio
064600081006     c                   add       rftimvf       imf(yy)
064700081006      * imponibile nuovo
064800081006     c                   add       rftimvr       imr(yy)
064900081006      * peso
065000081006     c                   add       taspkf        tkg(yy)
065100081006      * colli
065200081006     c                   add       tasncl        tcl(yy)
065300081006      * volume
065400081006     c                   add       tasvlf        tvl(yy)
065500081006      * spedizioni
065600081006     c                   add       1             nsp(yy)
065700081006      * manca tariffa
065800081006     c                   if        taserr <> ' '
065900081006     c                   add       1             nmt(yy)
066000081006     c                   endif
066100081006
066200081006     c                   endif
066300081006
066400081006      *** se bolla di recupero
066500081006     c                   else
066600081006      * imponibile vecchio
066700081006     c                   add       rftimvf       prtimfr
066800081006      * imponibile nuovo
066900081006     c                   add       rftimvr       prtimrr
067000081006      * spedizioni
067100081006     c                   add       1             prtnspr
067200081006      * manca tariffa
067300081006     c                   if        taserr <> ' '
067400081006     c                   add       1             prtnmtr
067500081006     c                   endif
067600081006      ***
067700081006     c                   endif
067800081006
067900081006     c                   endif
068000081006
068100060530
068200060518     c                   Endsr
068300060515
068400060519      *
068500060519      *---------------------------------------------------------------*
068600060519      *  Recupero importi di tassazione dal file titai
068700060519      *---------------------------------------------------------------*
068800060519     c     Ric_tai       Begsr
068900060519      *
069000060519     c                   z-add     0             k
069100060519
069200060519     c     kspe          setll     titai01l
069300060519
069400060519     c                   If        %equal(titai01l)
069500060519
069600060519     c                   do        *hival
069700060519
069800060522     c     kspe          reade     titai01l
069900060519
070000060519     c                   If        %eof(titai01l)
070100060519     c                   leave
070200060519     c                   endif
070300060519      * imponibile
070400060519     c                   if        taisvt = '£'
070500060519     c                   eval      tasimv = taivat
070600060519     c                   endif
070700060519      * porto
070800060519     c                   if        taisvt = ' '
070900060519     c                   eval      taspor = taivat
071000060519     c                   endif
071100060519      * varie
071200060519     c                   if        taisvt <> ' ' and taisvt <> '£'
071300060519     c                   add       1             k
071400060519     c                   eval      sv(k) = taisvt
071500060519     c                   eval      va(k) = taivat
071600060519     c                   endif
071700060519
071800060519     c                   enddo
071900060519
072000060519     c                   else
072100060519      * se non è pretassata pulisco la divisa
072200060519     c                   clear                   tasdiv
072300060519     c                   endif
072400070426      * se tassata fino l'imponibile valorizzo il campo tasman del prtf e il campo rfttmaf del file
072500070426     c                   if        tasimv = 0
072600070426     c                   clear                   tasman
072700070426     c                   else
072800070502     c                   eval      tasman = 'S'
072900070426      * faccio il totale dei tassati manualmente
073000070426     c                   add       1             prtnrtm
073100070426     c                   endif
073200070426
073300070426     c                   eval      rfttmaf = tasman
073400060519
073500060519     c                   endsr
073600060519      *---------------------------------------------------------------*
073700060519      ** Riverca Contrassegno
073800060519      *---------------------------------------------------------------*
073900060519     c     Carcsb        Begsr
074000060519
074100060519     c     Kcsb          chain     Tncsb03l
074200060519     c                   If        %found(tncsb03l) and
074300060519      * solo per stesso tipo bolla
074400060519     c                             tastbl = csbtbl
074500060519     c                   movel     csbsta        Tassta
074600060519     c                   z-add     csbcas        Tascas
074700061016     c*****              movel     csbtpa        Tastic
074800060519     c                   if        csbfus <> *blanks
074900060519     c                   move      csbfus        tastic
075000060519     c                   else
075100060519     c                   move      csbtpi        tastic
075200060519     c                   end
075300060519      * mittente
075400060519     c                   movel     csbvca        tasvca
075500060519     c                   z-add     csbcmb        tascmb
075600060519
075700060519     c                   endif
075800060519
075900060519     c                   endsr
076000060522      *---------------------------------------------------------------*
076100060522      ** Calcolo anteporto VARIA 'N'
076200060522      *---------------------------------------------------------------*
076300060522     c     Varian        Begsr
076400060522
076500060522      * richiamo il programma di tassazione
076600060522      * senza passare le varie e il porto
076700060522      *
076800060522      * mi salvo le ds di tassazione
076900060522     c                   eval      w_dtas = dtas
077000060522     c                   eval      w_dtasv = dtasv
077100060522      * mi salvo la divisa
077200060522     c                   eval      w_tasdiv = tasdiv
077300060522
077400110701      * tolgo il flag dell'invio mail al destinatario nell'eventualità ci fosse
077500110701     c                   eval      dtas01 = dstasflo
077600110701     c                   clear                   §asemd
077700110701     c                   eval      dstasflo = dtas01
077800110701
077900060522     c                   clear                   dtasv
078000060522     c                   clear                   tasdiv
078100060522     c                   clear                   taspor
078200060522
078300060522     c                   clear                   antepo
078400060522
078500060522     c                   call      'TNSF20R'
078600060522     c                   parm                    kpjba
078700060522     c                   parm                    dtas
078800060522     c                   parm                    dtasv
078900060522      * se non ci sono errori valorizzo la varia 'N'
079000060522     c                   if        taserr = ' '
079100060522     c                   z-add     tasimv        antepo
079200060522     c                   endif
079300060522      * ripristino le ds salvate prima
079400060522     c                   eval      dtas = w_dtas
079500060522     c                   eval      dtasv = w_dtasv
079600060522      * valorizzo l'anteporto nella varia N al posto di 88888888  se maggiore di zero
079700060522     c                   if        antepo > 0
079800060522     c                   z-add     antepo        va(xx)
079900060522     c                   endif
080000060522
080100060522     c                   endsr
080200060519      *---------------------------------------------------------------*
080300060519      ** CARICAMENTO codici bolla che prevedono di tassare solo 1 varia
080400060519      *   e che permettono imponibile =0
080500081006      ** CARICAMENTO codici bolla che non sono c/s o recuperi per
080600081006      *   scaglionare
080700060519      *---------------------------------------------------------------*
080800060519     c     Carcbo        Begsr
080900081006      **
081000081006     c                   Movel     'TB'          Cod
081100081006     c                   z-add     0             z
081200081006     c     Ktab          setll     tabel00f
081300081006     c                   do        *hival
081400081006     c     Ktab          reade     tabel00f
081500081006      * fine file
081600081006     c                   If        %eof(tabel00f)
081700081006     c                   leave
081800081006     c                   endif
081900081006    1
082000081006    2c                   If        tblkey <> *blanks
082100081006     c                   Movel     TBLuni        DStb
082200081006      * imponibile a zero
082300081006    3c                   If        §tbrbl  = 'N'
082400081006     c                   add       1             z
082500081006     c                   movel     TBLkey        tbo_oK(z)
082600081006    3c                   endif
082700081006      **
082800081006    2c                   endif
082900081006    1c                   enddo
083000081006
083100060519      **
083200060519     c                   Movel     '3A'          Cod
083300060519     c                   z-add     0             K
083400060519     c                   z-add     0             KK
083500060519     c     Ktab          setll     tabel00f
083600060519     c                   do        *hival
083700060519     c     Ktab          reade     tabel00f
083800060519      * fine file
083900060519     c                   If        %eof(tabel00f)
084000060519     c                   leave
084100060519     c                   endif
084200060519    1
084300060519    2c                   If        tblkey <> *blanks
084400060519     c                   Movel     TBLuni        DS3a
084500060519      * imponibile a zero
084600060519    3c                   If        §3aim0  = 'S'
084700060519     c                   add       1             K
084800060519     c                   movel     TBLkey        im0(k)
084900060519    3c                   endif
085000060519      * bolla con singola varia
085100060519     c                   If        §3asva  <> *blanks
085200060519     c                   add       1             Kk
085300060519     c                   movel     tblkey        cbo(kk)
085400060519     c                   movel     §3asva        cbv(kk)
085500060519    3c                   endif
085600081006      * bolla con tipo bolla normale
085700081006     c     §3atb1        lookup    tbo_ok                                 30
085800081006     c                   if        %found
085900081006     c                   add       1             zz
086000081006     c                   movel     tblkey        cbo_ok(zz)
086100081006     c                   endif
086200060519      **
086300060519    2c                   endif
086400060519    1c                   enddo
086500060519
086600060519     c                   Endsr
086700081006      *---------------------------------------------------------------*
086800081006      * Caricamento scaglioni
086900081006      *---------------------------------------------------------------*
087000081006     c     Car_scaglioni BEGSR
087100081006
087200081006     c                   eval      savprg = 999
087300081006     c                   clear                   yy
087400081006     c                   clear                   tipo_tar
087500081006     c                   clear                   num_sgl
087600081006      * cerco il giusto progressivo
087700081006
087800081006     c     ktam          setll     tntam01l
087900081006     c                   do        *hival
088000081006     c     ktam          reade     tntam01l
088100081006     c                   if        %eof(tntam01l)
088200081006     c                   leave
088300081006     c                   endif
088400081006      * verifico se progressivo annullato
088500081006     c                   if        tamatb <> ' '
088600081006     c                   iter
088700081006     c                   endif
088800081006      * controllo con la data di riferimento
088900120216     c                   If        rftdspr  < tamdst and rftdspr >= tamddt
089000081006     c                   eval      savprg = tamprg
089100081006     c                   leave
089200081006     c                   endif
089300081006     c                   if        rftdspr > tamdst
089400081006     c                   eval      savprg = tamprg
089500081006     c                   iter
089600081006     c                   endif
089700081006     c                   enddo
089800081006
089900081006     c                   if        savprg <> 999
090000081006      * trovato progressivo carico gli scaglioni relativi leggendo da titad
090100081006     c     ktad          setll     titad04l
090200110713     c                   do        *hival
090300081006     c     ktad          reade     titad04l
090400110713     c                   if        %eof(titad04l)
090500110713     c                   leave
090600110713     c                   endif
090700110713     c                   if        tadatb = ' '
090800081006      * recupero la chiave
090900081006     c                   eval      savlnp = tadlnp
091000081006     c                   eval      savorp = tadorp
091100081006     c                   eval      savnaz = tadnaz
091200081006     c                   eval      savcap = tadcap
091300110713     c                   leave
091400110713     c                   endif
091500110713     c                   enddo
091600081006
091700081006      * leggo tutti gli scaglioni con la stessa chiave
091800081006     c     ktad2         setll     titad04l
091900081006
092000081006     c                   do        *hival
092100081006     c     ktad2         reade     titad04l
092200081006
092300081006     c                   if        %eof(titad04l)
092400081006     c                   leave
092500081006     c                   endif
092600081006
092700081006     c                   if        tadatb <> ' '
092800081006     c                   iter
092900081006     c                   endif
093000081006
093100081006     c     tadsgl        lookup    sgl                                    31
093200081006
093300081006     c                   if        %found
093400081006     c                   leave
093500081006     c                   endif
093600081006
093700081006     c                   add       1             yy
093800081006     c                   eval      sgl(yy) = tadsgl
093900081006     c                   enddo
094000081006      * valorizzo il numero totali di scaglioni
094100081006     c                   z-add     yy            num_sgl
094200081006
094300081006     c                   movel     rftctrr       tipo_tar
094400081006
094500081006     c                   endif
094600081006      *
094700081006     c                   endsr
094800081006      *---------------------------------------------------------------*
094900081006      *  Ricerca dello scaglione per spedizione tassata
095000081006      *---------------------------------------------------------------*
095100081006     c     Ric_sgl       Begsr
095200081006
095300081006     c                   z-add     1             yy
095400081006
095500081006     c                   dow       yy <= num_sgl
095600081006     c                   if        misura <= sgl(yy)
095700081006     c                   eval      ok_scagl = *on
095800081006     c                   leave
095900081006     c                   endif
096000081006     c                   add       1             yy
096100081006     c                   enddo
096200081006
096300081006     c                   endsr
096400081006      *
096500060519      *---------------------------------------------------------------*
096600060519      * Operazioni Iniziali                                           *
096700060519      *---------------------------------------------------------------*
096800060519     c     RoutInz       BEGSR
096900060519      *
097000060519      * Reperisco dati job
097100060519     c                   exsr      DatiJob
097200060519      * Reperisco data e ora
097300060519     c                   time                    w0140
097400060519     c                   movel     w0140         wora
097500060522     c                   move      w0140         wdate
097600060519      *
097700060801      * verifico se sono in filiale faccio le ovrdbf per i file di sede
097800060801     c                   If        dutlpo <> 'S'
097900060801      * librerie
098000060801
098100060801      * Se sono in ambiente buono - GAITRAGRU
098200060801     c                   If        knsif = 'FILTRA201 '
098300060801     c                   Eval      wlib = 'GAITRAGRU '
098400060801     c                   Eval      wlib1 = 'GAITRAAZM '
098500060801      *  Se sono in ambiente di prova - GAITRAGRPS
098600060801     c                   Else
098700060801     c                   Eval      wlib = 'GAITRAGRPS'
098800060801     c                   Eval      wlib1 = 'GAITRAAZM '
098900060801     c                   EndIf
099000060801      * WFRFT01L
099100060801     c                   Clear                   comman
099200060801     c                   Movel(p)  cmdt(1)       comman
099300060801     c                   Eval      %Subst(comman:30:10) = wlib1
099400060801     c                   Eval      comman =
099500060801     c                             %trim(comman) + '/WFRFT01L)'
099600060801     c                   Call      'QCMDEXC'                            99
099700060801     c                   Parm                    comman
099800060801     c                   Parm                    lenght
099900060801
100000060801      * TITAI01L
100100060801     c                   Clear                   comman
100200060801     c                   Movel(p)  cmdt(2)       comman
100300060801     c                   Eval      %Subst(comman:30:10) = wlib
100400060801     c                   Eval      comman =
100500060801     c                             %trim(comman) + '/TITAI01L)'
100600060801     c                   Call      'QCMDEXC'                            99
100700060801     c                   Parm                    comman
100800060801     c                   Parm                    lenght
100900060801
101000060801      * TITAS30C
101100060801     c                   Clear                   comman
101200060801     c                   Movel(p)  cmdt(3)       comman
101300060801     c                   Eval      %Subst(comman:30:10) = wlib
101400060801     c                   Eval      comman =
101500060801     c                             %trim(comman) + '/TITAS30C)'
101600060801     c                   Call      'QCMDEXC'                            99
101700060801     c                   Parm                    comman
101800060801     c                   Parm                    lenght
101900060801      * FIAR531C
102000060801     c                   Clear                   comman
102100060801     c                   Movel(p)  cmdt(4)       comman
102200060801     c                   Eval      %Subst(comman:30:10) = wlib
102300060801     c                   Eval      comman =
102400060801     c                             %trim(comman) + '/FIAR531C)'
102500060801     c                   Call      'QCMDEXC'                            99
102600060801     c                   Parm                    comman
102700060801     c                   Parm                    lenght
102800060801      * TNCSB03L
102900060801     c                   Clear                   comman
103000060801     c                   Movel(p)  cmdt(5)       comman
103100060801     c                   Eval      %Subst(comman:30:10) = wlib
103200060801     c                   Eval      comman =
103300060801     c                             %trim(comman) + '/TNCSB03L)'
103400060801     c                   Call      'QCMDEXC'                            99
103500060801     c                   Parm                    comman
103600060801     c                   Parm                    lenght
103700060801
103800060801     c                   endif
103900060801      *
104000060801     c                   open      wfrft01l
104100060801     c                   open      titai01l
104200060801     c                   open      titas30c
104300060801     c                   open      fiar531c
104400060801     c                   open      tncsb03l
104500060801
104600060519     c                   ENDSR
104700060519      *
104800060519      *---------------------------------------------------------------*
104900060519      * Reperimento Dati del job (Utente/Operativi)                   *
105000060519      *---------------------------------------------------------------*
105100060519     c     DatiJob       BEGSR
105200060519      *
105300060519     c     *dtaara       define    §azute        azuteds
105400060519     c     *dtaara       define    §datiute      ddatiute
105500060519      *
105600060519     c                   in(E)     *dtaara
105700060519     c                   IF        %ERROR or RSUT = *blanks
105800060519     c                   clear                   Tibs34Ds
105900060519     c                   call      'TIBS34R'
106000060519     c                   parm                    Tibs34Ds
106100060519     c                   in        *dtaara
106200060519     c                   Endif
106300060519
106400060519      *
106500060519     c                   ENDSR
106600060801** cmdt
106700060801OVRDBF FILE(WFRFT01L) TOFILE(
106800060801OVRDBF FILE(TITAI01L) TOFILE(
106900060801OVRDBF FILE(TITAS30C) TOFILE(
107000060801OVRDBF FILE(FIAR531C) TOFILE(
107100060801OVRDBF FILE(TNCSB03L) TOFILE(
107200060801DLTOVR FILE(*ALL)
