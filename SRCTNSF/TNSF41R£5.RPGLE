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
004500120921       // -?Varia e importo  fatturato e rifatturato
004600120921     d sk_Voce         s                   dim(99)  inz  like(TASsv1)
004700120921     d sk_ImpoF        s             12  3 dim(99)  inz
004800120921     d sk_ImpoR        s             12  3 dim(99)  inz
004900141217
005000141217       //?Sk part.consegna con abilitazione PIN Code
005100141217     d skGMA           s              2a   dim(10)
005200120921
005300060801      * ovrdbf
005400060801     d cmdt            s             60    dim(06) ctdata perrcd(1)
005500060504      * Ds - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
005600060504      *
005700060504     d KPJBA         e ds
005800060518      * - Parametri x il lancio del pgm batch
005900060518     d TNSF40DS      e ds                  inz
006000060522      * - Parametri x Controllo profilo utenti
006100060522     d TIBS34DS      e ds                  inz
006200060522      * - Ds di riferimento al file esterno AZUTE00F
006300060522     d AZUTEds       e ds                  extname(AZUTE00F)
006400060522      * - Ds per dati organigramma
006500060522     d DDatiUte      e ds
006600081006      ** Tipi bolla
006700081006     D DSTB          E DS
006800060522      ** Codici bolla
006900060522     D DS3A          E DS
007000081006      ** Tipi tariffa
007100081006     D DSTR          E DS
007200110701      ** DS del tasflo del file TITAS00F
007300110701     D DTASFLO       E DS
007400060518      ** DS Calcolo tassazione  - Varie
007500060518     D DTAS          E DS
007600060518     D  dstasflo     E                     extfld(tasflo)
007700060926      ** DS Flag operativi DS DTAS
007800060926     d Dtas01        e ds
007900060926
008000060518     d Dtasv         e ds
008100060518     d  sv                     1     20
008200060518     d                                     Dim(20)                              Sigle varie
008300060518     d  va                    21    140P 3
008400060518     d                                     Dim(20)                              Importi varie
008500151012      ** DS ds per valori tassazione peso desunto
008600151012     d Dtaspes       e ds
008700060522      * recupero bancali e colli originali
008800060522     d dAr5Ban       e ds
008900060522
009000060522     d dAr5Bnb       e ds
009100120921
009200120921       // -?Tabella "CC" = Codici Tassazione?
009300120921     d dsCC          e ds                  inz
009400141217
009500141217       //?Ds tabella 7R - part.consegna
009600141217     d ds7R          e ds
009700141217
009800120921
009900060518      * - Ds per scrittura file di lavoro
010000060518     d                 ds
010100060518     d  RFTsv1r                1      1
010200060518     d  RFTsv2r                2      2
010300060518     d  RFTsv3r                3      3
010400060518     d  RFTsv4r                4      4
010500060518     d  RFTsv5r                5      5
010600060518     d  RFTsv6r                6      6
010700060518     d  RFTsv7r                7      7
010800060518     d  RFTsv8r                8      8
010900060518     d  RFTsv9r                9      9
011000060518     d  RFTs10r               10     10
011100060518     d  RFTs11r               11     11
011200060518     d  RFTs12r               12     12
011300060518     d  RFTs13r               13     13
011400060518     d  RFTs14r               14     14
011500060518     d  RFTs15r               15     15
011600060518     d  RFTs16r               16     16
011700060518     d  RFTs17r               17     17
011800060518     d  RFTs18r               18     18
011900060518     d  RFTs19r               19     19
012000060518     d  RFTs20r               20     20
012100060518     d  RFT_svr                1     20    Dim(20)                              Sigle varie
012200060512
012300060518      * - Ds per scrittura file di lavoro
012400060518     d                 ds
012500060518     d  RFTva1r                1     11  3
012600060518     d  RFTva2r               12     22  3
012700060518     d  RFTva3r               23     33  3
012800060518     d  RFTva4r               34     44  3
012900060518     d  RFTva5r               45     55  3
013000060518     d  RFTva6r               56     66  3
013100060518     d  RFTva7r               67     77  3
013200060518     d  RFTva8r               78     88  3
013300060518     d  RFTva9r               89     99  3
013400060518     d  RFTv10r              100    110  3
013500060518     d  RFTv11r              111    121  3
013600060518     d  RFTv12r              122    132  3
013700060518     d  RFTv13r              133    143  3
013800060518     d  RFTv14r              144    154  3
013900060518     d  RFTv15r              155    165  3
014000060518     d  RFTv16r              166    176  3
014100060518     d  RFTv17r              177    187  3
014200060518     d  RFTv18r              188    198  3
014300060518     d  RFTv19r              199    209  3
014400060518     d  RFTv20r              210    220  3
014500060518     d  RFT_var                1    220  3 Dim(20)                              Importo varie
014600060504      *
014700060504      * - Controllo/Inversione date
014800060504     d WLBdat          ds                  inz
014900060504     d   G08dat                       8  0 inz
015000060504     d   G08inv                       8  0 inz
015100060504     d   G08err                       1    inz('3')
015200060504     d   G08tgi                       5  0 inz
015300060504      *
015400060504      * - Variabili definite a programma
015500060519     d codut           s              1  0 inz(1)
015600060504     d Wdate           s              8  0 inz
015700060518     d Wora            s              6  0 inz
015800060516     d W0140           s             14  0 inz
015900081006     d z               s              4  0
016000081006     d zz              s              4  0
016100060518     d k               s              4  0
016200060519     d kk              s              4  0
016300060522     d xx              s              4  0
016400081006     d yy              s              4  0
016500120921     d ww              s              4  0
016600081006     d num_sgl         s              4  0
016700060522     d kscfil          s              3  0
016800081006     d tipo_tar        s              1  0
016900081006     d ok_scagl        s               n
017000060522     d cod             s                   like(tblcod)
017100081006     d key             s                   like(tblkey)
017200060522     d kAr5Trd         s                   Like(Ar5Trd)
017300060522     d antepo          s                   like(tasimv)
017400060522     d W_tasdiv        s                   like(tasdiv)
017500060522     d W_dtas          s                   like(dtas)
017600060522     d W_dtasv         s                   like(dtasv)
017700060522     D Wcev            s                   like(evbcev) inz('RIC')
017800081006     d prg2            s                   like(tamprg) inz
017900081006     d ctr2            s                   like(tamctr) inz
018000081006     d ksc2            s                   like(tamksc) inz
018100081006     d savprg          s                   like(tamprg)
018200081006     d savlnp          s                   like(tadlnp)
018300081006     d savorp          s                   like(tadorp)
018400081006     d savnaz          s                   like(tadnaz)
018500081006     d savcap          s                   like(tadcap)
018600081006     d misura          s                   like(tadsgl)
018700060801
018800060801     d wlib            s             10
018900060801     d wlib1           s             10
019000060801     d comman          s             80
019100060801     d lenght          s             15  5 inz(80)
019200060801
019300120921       // -?Campi di comodo?
019400120921     d Wconta          s              1  0 inz
019500120921     d Wvaria          s                   like(TASsv1)   inz
019600120921     d WimpoF          s                   like(TASva1)   inz
019700120921     d WimpoR          s                   like(TASva1)   inz
019800060522
019900060504      *
020000060504      * Key-List - - - - - - - - - - - - - - - - - - - - - - - - - - -*
020100060504      *
020200060518      * - WFRFT01L
020300060518     c     KRFT          Klist
020400060518     c                   Kfld                    I40noj
020500060518     c                   Kfld                    I40pru
020600060518     c                   Kfld                    I40dta
020700060518     c                   Kfld                    I40ora
020800060517
020900060518      * - TITAI01L / TITAS30C
021000060518     c     KSPE          Klist
021100060518     c                   Kfld                    RFTaas
021200060518     c                   Kfld                    RFTlnp
021300060518     c                   Kfld                    RFTnrs
021400060518     c                   Kfld                    RFTnsp
021500060518     c                   Kfld                    RFTtbl
021600060519      * Tncsb03l
021700060519     c     KCSB          Klist
021800060519     c                   Kfld                    TASaas
021900060519     c                   Kfld                    TASlnp
022000060519     c                   Kfld                    TASnrs
022100060519     c                   Kfld                    TASnsp
022200060519      * Tabel00f
022300060519     c     KTAB          Klist
022400060519     c                   Kfld                    codut
022500060519     c                   Kfld                    cod
022600081006     c     KTABkey       Klist
022700081006     c                   Kfld                    codut
022800081006     c                   Kfld                    cod
022900081006     c                   Kfld                    key
023000060522      * Fiar531c
023100060522     c     kFiar5        Klist
023200060522     c                   Kfld                    TasAas
023300060522     c                   Kfld                    TasLnp
023400060522     c                   Kfld                    TasNrs
023500060522     c                   Kfld                    TasNsp
023600060522     c                   Kfld                    kAr5Trd
023700060522      * Chiave file eventi FNEVB
023800060522     c     Kevb4         Klist
023900060522     c                   Kfld                    Tasaas
024000060522     c                   Kfld                    Taslnp
024100060522     c                   Kfld                    Tasnrs
024200060522     c                   Kfld                    Tasnsp
024300060522     c                   Kfld                    Wcev
024400081006      ** accesso tntam x cliente
024500081006     c     Ktam          Klist
024600081006     c                   Kfld                    rftkscr
024700081006     c                   Kfld                    rftctrr
024800081006      ** accesso titad
024900081006     c     Ktad          Klist
025000081006     c                   Kfld                    tamksc
025100081006     c                   Kfld                    tamctr
025200081006     c                   Kfld                    tamprg
025300081006
025400081006     c     Ktad2         Klist
025500081006     c                   Kfld                    tamksc
025600081006     c                   Kfld                    tamctr
025700081006     c                   Kfld                    tamprg
025800081006     c                   Kfld                    savlnp
025900081006     c                   Kfld                    savorp
026000081006     c                   Kfld                    savnaz
026100081006     c                   Kfld                    savcap
026200060504
026300060504      *---------------------------------------------------------------*
026400060504      *  RIEPILOGO INDICATORI                                         *
026500060504      *---------------------------------------------------------------*
026600060727      * 05/30 - Comodo
026700060727      * 50    - Data fattura antecedente 30-04-06 stampo scritta
026800060727      * 51    - Comodo
026900060727      * 90    - OF
027000060727      * 99    - Comodo
027100060504      *---------------------------------------------------------------*
027200060504
027300060504     c     *Entry        plist
027400060504     c                   parm                    KPJBA
027500060518     c                   movel     kpjbu         Tnsf40ds
027600060504      *
027700060504      * Operazioni Iniziali
027800060504     c                   exsr      RoutInz
027900060504      *
028000060523     c                   exsr      Carcbo
028100141217      /free
028200141217       //?Rielaboro quello rimasto nel file in QTEMP
028300141217         exsr Car7RPinCode;
028400141217      /end-free
028500060523      *
028600060518      * Elaborazione del file
028700060518     c
028800060518do  1c                   exsr      Elabora
028900060504      * 1° videata
029000060518     c                   exsr      Stampa
029100060504      * Fine
029200060509      *
029300060512     c     fine          tag
029400060801
029500060801     c                   If        Dutlpo <> 'S'
029600060801     c                   Clear                   comman
029700060801     c                   Movel(p)  cmdt(6)       comman
029800060801     c                   Call      'QCMDEXC'                            99
029900060801     c                   Parm                    comman
030000060801     c                   Parm                    lenght
030100060801     c                   endif
030200060801
030300060504     c                   movel     *on           *inLR
030400060504      *
030500060504      *
030600060504      *---------------------------------------------------------------*
030700060518      * Elabora ritassazione                                          *
030800060504      *---------------------------------------------------------------*
030900060518     c     Elabora       BEGSR
031000060504      *
031100060518e   1c     Krft          setll     wfrft01l
031200060504
031300060504     c                   do        *hival
031400060522      *
031500060518     c     krft          reade     wfrft01l
031600060518
031700060522     c                   If        %eof(Wfrft01l)
031800060518     c                   leave
031900060518     c                   endif
032000060518      * verifico se record già elaborato
032100060518     c                   if        rftela <> ' '
032200060518     c                   iter
032300060518     c                   endif
032400060726      * imposto i campi della testata
032500060726     c                   if        prtfiv = *zeros
032600060726     c                   eval      prtfiv = rftfiv
032700060726     c                   eval      prtnft = rftnft
032800060726      *
032900060726     c                   clear                   WLBdat
033000060726     c                   eval      G08inv = rftdft
033100060726     c                   eval      G08err = '3'
033200060726     c                   call      'XSRDA8'
033300060726     c                   parm                    WLBdat
033400060726     c                   z-add     G08dat        prtdft
033500081006      * se si desidera tassazione scaglionata recupero gli scaglioni della tariffa
033600081006     c                   If        i40sgl = 'S'
033700081006     c                   exsr      car_scaglioni
033800081006     c                   endif
033900081006
034000060726     c                   endif
034100060727      * controllo data fattura
034200060727     c                   eval      *in50 = rftdft < 20060430
034300060518      * tasso
034400060518     c                   exsr      tassaz
034500060504      *
034600060504     c                   enddo
034700060504      *
034800060504     c                   ENDSR
034900060504      *
035000060522      *---------------------------------------------------------------*
035100060522      * Stampa Finale
035200060522      *---------------------------------------------------------------*
035300060522     c     Stampa        BEGSR
035400060522
035500060522      * testata
035600060530     c                   write     sf41t0
035700060522     c                   write     sf41ts
035800060522     c                   write     sf41t3
035900060522     c                   write     sf41t2
036000060522     c                   write     sf41t3
036100060522      * valorizzo i campi
036200060522     c                   eval      prtnoj = knmeb
036300060522     c                   eval      prtpru = rftpru
036400060522      *
036500060522     c                   clear                   WLBdat
036600060522     c                   eval      G08inv = rftdta
036700060522     c                   eval      G08err = '3'
036800060522     c                   call      'XSRDA8'
036900060522     c                   parm                    WLBdat
037000060522     c                   z-add     G08dat        prtdta
037100060522      *
037200060522     c                   eval      prtora = rftora
037300060614      * calcolo la differente
037400060614     c                   eval      prtdiffe = prtimvf - prtimvr
037500060522      *
037600060522     c                   write     sf41d3
037700060522      *
037800081006      * stampo la suddivisione per scaglione solo se richiesta
037900081006     c                   If        i40sgl = 'S'
038000081006     c                   eval      cod = 'TR'
038100081006     c                   movel(p)  tipo_tar      key
038200081006     c     ktabkey       chain     Tabel00f
038300081006     c                   if        %found(tabel00f)
038400081006     c                   movel     tbluni        dstr
038500081006      * valorizzo i campi descrittivi di stampa
038600081006     c                   movel     §trdst        prttar
038700081006     c                   endif
038800081006      * testata
038900081006     c                   write     sf41t04
039000081006     c                   write     sf41t3
039100081006     c                   write     sf41t4
039200081006     c                   write     sf41t3
039300081006
039400081006      * stampo dettaglio per ogni scaglione
039500081006     c                   z-add     1             yy
039600081006     c                   dow       yy <= num_sgl
039700081006
039800081006     c                   eval      prtsca = sgl(yy)
039900081006     c                   eval      prtimf = imf(yy)
040000081006     c                   eval      prtimr = imr(yy)
040100081006     c                   eval      prtdif = (prtimf - prtimr)
040200081006     c                   eval      prttkg = tkg(yy)
040300081006     c                   eval      prttcl = tcl(yy)
040400081006     c                   eval      prttvl = tvl(yy)
040500081006     c                   eval      prtnsp = nsp(yy)
040600081006     c                   eval      prtnmt = nmt(yy)
040700081006
040800081006     c                   write     sf41d4
040900081006      * pulisco la descrizione della tariffa
041000081006     c                   clear                   prttar
041100081006
041200081006     c                   add       1             yy
041300081006
041400081006     c                   enddo
041500081006      *
041600081006      * stampo se ci sono delle bolle di recupero
041700081006     c                   If        prtnspr > 0
041800081006     c                   write     sf41d4r
041900081006     c                   endif
042000081006      *
042100081006     c                   endif
042200120921      /free
042300120921       //?Stampa la suddivisione per voce se richiesta
042400120921        IF  I40sva = 'S';
042500120921          exsr  Stampa_TOT;
042600120921        ENDIF;
042700120921      /end-free
042800081006      *
042900060522     c                   Endsr
043000120921
043100120921      /free
043200120921       //--------------------------------------------------------------
043300120921       //?Stampa suddivisione Voci.
043400120921       //--------------------------------------------------------------
043500120921       BEGSR  Stampa_TOT;
043600120921
043700120921         write  SF41T05;
043800120921         write  SF41T5;
043900120921
044000120921         ww = 1;
044100120921
044200120921       //?SE non ci fosse la voce trasporto (che è sempre alla?
044300120921       //?prima posizione): si inizia a stampare dalla 2ª Varia?
044400120921         IF  sk_ImpoF(1) = *zero;
044500120921           ww = 2;
044600120921         ENDIF;
044700120921
044800120921       //?Stampa delle varie di 2 in 2?
044900120921         DoW  sk_Voce(ww) <> *blank;
045000120921
045100120921           Wconta = 1;
045200120921           clear  SF41D5;
045300120921
045400120921           DoW  Wconta <= 2  and  sk_Voce(ww) <> *blank;
045500120921
045600120921             Wvaria = sk_Voce(ww);
045700120921             if  Wconta = 1;
045800120921               //?Nella prima posizione: Trasporto?
045900120921               if  ww = 1;
046000120921                 P1Dsv1 = 'TRASPORTO      ';
046100120921               else;
046200120921                 exsr  sr_TabCC;
046300120921                 P1Csv1 = sk_Voce(ww) + ' -';
046400120921                 P1Dsv1 = §CCdes;
046500120921               endif;
046600120921               P1Cva1F = sk_ImpoF(ww);
046700120921               P1Cva1R = sk_ImpoR(ww);
046800120921             else;
046900120921               exsr  sr_TabCC;
047000120921               P1Csv2  = sk_Voce(ww) + ' -';
047100120921               P1Dsv2  = §CCdes;
047200120921               P1Cva2F = sk_ImpoF(ww);
047300120921               P1Cva2R = sk_ImpoR(ww);
047400120921             endif;
047500120921
047600120921             ww += 1;
047700120921             Wconta += 1;
047800120921
047900120921           EndDo;
048000120921
048100120921           write  SF41D5;
048200120921
048300120921         EndDo;
048400120921
048500120921         //?Totale Imponibile?
048600120925         //P1CimvF = %xfoot( sk_ImpoF);
048700120925         //P1CimvR = %xfoot( sk_ImpoR);
048800120921
048900120921         //?Stampa Totali per Fattura?
049000120921         write  SF41G5;
049100120921
049200120921       ENDSR;
049300120921
049400120921       //--------------------------------------------------------------
049500120921       //?Decodifica Sigla Varia.                                      ?
049600120921       //--------------------------------------------------------------
049700120921       BEGSR  sr_TabCC;
049800120921
049900120921         clear  dsCC;
050000120921         cod = 'CC';
050100120921         key = 'VARIE  ' + Wvaria;
050200120921
050300120921         chain  (codut:cod:key) TABEL00F;
050400120921
050500120921         IF  %found(TABEL00F);
050600120921           dsCC = TBLuni;
050700120921         ELSE;
050800120921           §CCdes = *all'?';
050900120921         ENDIF;
051000120921
051100120921       ENDSR;
051200120921      /end-free
051300060522      *
051400060515      *---------------------------------------------------------------*
051500060518      *  Routine di tassazione
051600060515      *---------------------------------------------------------------*
051700060518     c     Tassaz        Begsr
051800060518      *
051900060518     c                   clear                   dtas
052000060518     c                   clear                   dtasv
052100151012     c                   clear                   dtaspes
052200060518      * aggancio titas
052300060518     c     KSPE          chain     titas30c
052400060522   a1c                   if        %found(titas30c)
052500070411      * Pulisco i campi dell'assicurazione se calcolata in fattura
052600070411    2c                   IF        Tasfcm = 'F'
052700070411     c                   clear                   tasias
052800070411     c                   clear                   tasvas
052900070411    2c                   Endif
053000070411
053100060530      * valorizzo i campi del file printer
053200060530      *
053300060530     c                   clear                   WLBdat
053400060530     c                   movel     tasaas        g08inv
053500060530     c                   move      tasmgs        g08inv
053600060530     c                   eval      G08err = '3'
053700060530     c                   call      'XSRDA8'
053800060530     c                   parm                    WLBdat
053900060530     c                   z-add     G08dat        prtdsp
054000060530
054100060530     c                   z-add     tasksc        prtksc
054200060530     c                   z-add     tasctr        prtctr
054300060530
054400060530     c                   clear                   WLBdat
054500060530     c                   movel     rftdspr       g08inv
054600060530     c                   eval      G08err = '3'
054700060530     c                   call      'XSRDA8'
054800060530     c                   parm                    WLBdat
054900060530     c                   z-add     G08dat        prtdspr
055000060530
055100060530      *
055200060519      * valorizzo la DS DTAS e DTASV
055300060519     c                   clear                   dtasv
055400060519     c                   clear                   taspor
055500060519     c                   clear                   tasimv
055600151012     c                   clear                   dtaspes
055700060518      *
055800060519      * cerco in TITAI l'immagine della spedizione prima della tassazione
055900060519     c                   exsr      RIC_tai
056000060519      * se tassata fino all'imponibile non tasso
056100060522    1c                   if        tasimv = 0
056200060519      ** Ricerca contrassegno
056300060519    2c                   IF        Tasfca <> *blanks
056400060519     c                   Exsr      Carcsb
056500060519    2c                   Endif
056600060519
056700060519      *  se si tratta di codice bolla che prevede solo una varia, passo solo
056800060519      *  quella
056900060519     c                   z-add     1             kk
057000060519     c     tascbo        lookup    cbo(kk)                                05
057100060522    2c                   if        *in05
057200060519     c                   eval      tassva=cBV(kk)
057300060522    2c                   endif
057400060522
057500060522     c                   movel     Tasftc        tastc1
057600060522     c                   Z-add     rftdft        tasdct
057700060726
057800060726     c                   eval      Tasksc = rftkscr
057900060726     c                   eval      Tasctr = rftctrr
058000061221     c                   movel     rftaas        tasdsp
058100061221     c                   move      rftmgs        tasdsp
058200060726
058300060522      * Flag SI NO DDT
058400060522    2C                   If        tasll1 = 'C' or tasll1 = 'S'
058500060522     C                   movel     'S'           Tasddt
058600060522   x2C                   Else
058700060522     C                   Clear                   Tasddt
058800060522    2C                   Endif
058900060926     c****               movel     tasfbr        dstasflo
059000060926     c                   clear                   dtas01
059100060926     c                   movel     tasfbr        §asfbr
059200060926     c                   movel     tascca        §ascca
059300110701      * valorizzo flag email al destinatario
059400110701     c                   movel     tasflo        dtasflo
059500110701     c                   move      §floemd       §asemd
059600141217      /free
059700141217       //?Imposto se part.consegna con PinCode
059800141217         IF  TASgma <> *blanks and %lookup (TASgma:skGMA) > 0;
059900141217           §ASpin = 'S';
060000141217         ENDIF;
060100141217      /end-free
060200110701
060300061221      * se data spedizione differente da ritassazione passo nel flo anche la data ritassazione
060400061221     c                   If        tasdsp <> rftdspr
060500070108     c                   move      rftdspr       §asdrt
060600061221     c                   endif
060700061221
060800060926     c                   eval      dstasflo = dtas01
060900060522
061000060522      * Bancali
061100060522    2c                   If        %Subst(TasGva:1:1) = 'B'
061200060522
061300060522     c                   eval      kAr5Trd  = 'BAN'
061400060522     c     kFiar5        Chain     Fiar531c
061500060522If  3c                   If        %Found(Fiar531c)
061600060522     c                   Movel     Ar5Uni        dAr5Ban
061700060522    3c                   EndIf
061800060522      * Bancali
061900060522     c                   Eval      TasBan = §Ar5Nba + §Ar5Nb2
062000060522    2c                   EndIf
062100060522      **
062200060522      * Colli Originali
062300060522    2c                   If        %Subst(TasGva:1:1) = 'O'
062400060522
062500060522     c                   eval      kAr5Trd  = 'BNB'
062600060522     c     kFiar5        Chain     Fiar531c
062700060522If  3c                   If        %Found(Fiar531c)
062800060522     c                   Movel     Ar5Uni        dAr5Bnb
062900060522    3c                   EndIf
063000060522      * Colli
063100060522     c                   Eval      TasNcl = §Ar5bcor
063200060522    2c                   EndIf
063300060522      *
063400060522      * verifico se è una bolla non tassata fino l'imponibile , una bolla di reso ,
063500060522      * con VARIA 'N' valorizzata con 888888 chiamo il TNSF20R per il calcolo dell'anteporto
063600060522      *
063700060522    2c                   if        tasimv = 0 and tasfbr = 'S'
063800060522     c                   z-add     1             xx
063900060522     c     'N'           lookup    sv(xx)                                 30
064000060522    3c                   if        *in30 and va(xx) =  88888888
064100060522      * tasso l'anteporto
064200060522     c                   exsr      Varian
064300060522    3c                   endif
064400060522      *
064500060522    2c                   endif
064600060522      *
064700060522      * se ritorno dal calcolo della varia N e c'è errore non proseguo
064800060522      *
064900060522     c                   Setoff                                       05
065000060522    2c                   if        taserr = ' '
065100060522      * Se devo tassare per una varia e c'e' gia', non richiamo la tassazione  se importo
065200060522      * della varia uguale  a zero
065300060522    3c                   if        tassva<>*blanks and tasimv=0
065400060522     c                   z-add     1             kk
065500060522     c     tassva        lookup    sv(kk)                                 05
065600060522    4c                   if        *in05 and va(kk) > 0
065700060522     c                   setoff                                       05
065800060522    4c                   endif
065900060522    3c                   endif
066000060522      * verifico se esiste la varia & con importo uguale a 8888888 e se si passo al
066100060522      * TNSF20R il flag di calcolo varia &
066200060522      *
066300060522     c                   setoff                                       51
066400060522     c                   z-add     1             kk
066500060522     c     '&'           lookup    sv(kk)                                 51
066600060522    3c                   if        *in51 and va(kk) = 88888888
066700060522     c                   eval      tasfcaa = 'S'
066800060522     c                   clear                   sv(kk)
066900060522     c                   clear                   va(kk)
067000060522    3c                   endif
067100060522      * verifico se devo calcolare l'addebito di lasciato avviso
067200060522     c                   movel     rftkscr       kscfil
067300060522      *
067400060522      * verifico se c'è evento
067500060522     c     Kevb4         chain     Fnevb04l
067600060522
067700060522    4c                   If        %found(Fnevb04l)
067800060522     c                   eval      tasric = 'S'
067900060522    4c                   endif
068000060522
068100060522      **
068200060522      **
068300060522    3c                   if        tassva=*blanks or not *in05
068400151012      * valorizzo i dati nella ds DTASPES
068500151012     c                   eval      tasppkb = taspkb
068600151014      * se data tassazione impostata a video prendo quello altrimneti data fattura
068700151014     c                   If        tasdsp <> rftdspr
068800151014     c                   eval      taspdtt = rftdspr
068900151014     c                   else
069000151014     c                   eval      taspdtt = rftdft
069100151014     c                   endif
069200151012
069300151014     c                   call      'TNSF22R'
069400060522     c                   parm                    Kpjba
069500060522     c                   parm                    Dtas
069600060522     c                   parm                    Dtasv
069700151014     c                   parm                    Dtaspes
069800060522    3c                   endif
069900060522      **
070000060522    2c                   endif
070100060522    1c                   endif
070200060519
070300060522     c                   If        taserr = ' '
070400060517      * tassazione
070500060518     c                   eval      RFTdivr = tasdiv
070600060518     c                   eval      RFTporr = taspor
070700060518      * carico eventuali varie presenti in TITA7
070800060518     c                   movea     sv            rft_svr
070900060518     c                   movea     va            rft_var
071000060518      *
071100060518     c                   eval      RFTimvr = tasimv
071200060522     c                   endif
071300060522   a1c                   endif
071400060522
071500060518     c                   eval      RFTela  = 'S'
071600060522     c                   eval      RFTnoj  = Knmeb
071700060517
071800060518     c                   UPDATE    WFRFT000
071900060518
072000060522      * faccio i totali imponibili e conto le spedizioni manca tariffa
072100060522
072200060522     c                   If        taserr <> ' '
072300060522     c                   add       1             prtnrmt
072400060522     c                   endif
072500060522
072600060522     c                   add       RFTimvf       prtimvf
072700060522     c                   add       RFTimvr       prtimvr
072800060518
072900060530      * differenza
073000060530     c                   eval      diffe = rftimvf - rftimvr
073100120921
073200120921      /free
073300120921       //?Metto in SK il trasporto + le varie
073400120921         exsr  Mem_Varie;
073500120921      /end-free
073600060530
073700060530      * STAMPO
073800060530     c                   If        not *in99 or *in90
073900060530      * testata
074000060530      * testata
074100060530     c                   write     sf41t0
074200060530     c                   write     sf41ts
074300060530     c                   write     sf41t3
074400060530     c                   write     sf41t1
074500060530     c                   write     sf41t3
074600060530
074700060530     c                   seton                                        99
074800060530     c                   setoff                                       90
074900060530
075000060530     c                   endif
075100060530
075200060530     c                   write     sf41d1
075300081006
075400081006      * suddivido i valori per scaglioni di !!! in base alla tipologia della tariffa
075500081006
075600081006     c                   If        i40sgl = 'S'
075700081006      * se tipo bolla fa parte delle bolle non di recupero le sommo scaglionate
075800081006      * altrimenti le metto nelle spedizioni di recupero
075900081006     c     tascbo        lookup    cbo_ok                                 30
076000081006     c                   if        %found
076100081006
076200081006     c                   z-add     1             yy
076300081006     c                   eval      ok_scagl = *off
076400081006     c                   select
076500081006      * quantità / valore
076600081006     c                   when      tipo_tar = 5 or tipo_tar = 4
076700081006     c                   z-add     tasqft        misura
076800081006     c                   exsr      ric_sgl
076900081006      * peso
077000081006     c                   when      tipo_tar = 0 or tipo_tar = 3
077100081006     c                   z-add     taspkf        misura
077200081006     c                   exsr      ric_sgl
077300081006      * colli
077400081006     c                   when      tipo_tar = 1
077500081006     c                   z-add     tasncl        misura
077600081006     c                   exsr      ric_sgl
077700081006      * spedizione
077800100302     c                   when      tipo_tar = 2
077900081006     c                   z-add     1             misura
078000081006     c                   exsr      ric_sgl
078100081006     c                   endsl
078200081006
078300081006      * se scaglione trovato valorizzo i campi delle schiere
078400081006     c                   If        ok_scagl
078500081006      * imponibile vecchio
078600081006     c                   add       rftimvf       imf(yy)
078700081006      * imponibile nuovo
078800081006     c                   add       rftimvr       imr(yy)
078900081006      * peso
079000081006     c                   add       taspkf        tkg(yy)
079100081006      * colli
079200081006     c                   add       tasncl        tcl(yy)
079300081006      * volume
079400081006     c                   add       tasvlf        tvl(yy)
079500081006      * spedizioni
079600081006     c                   add       1             nsp(yy)
079700081006      * manca tariffa
079800081006     c                   if        taserr <> ' '
079900081006     c                   add       1             nmt(yy)
080000081006     c                   endif
080100081006
080200081006     c                   endif
080300081006
080400081006      *** se bolla di recupero
080500081006     c                   else
080600081006      * imponibile vecchio
080700081006     c                   add       rftimvf       prtimfr
080800081006      * imponibile nuovo
080900081006     c                   add       rftimvr       prtimrr
081000081006      * spedizioni
081100081006     c                   add       1             prtnspr
081200081006      * manca tariffa
081300081006     c                   if        taserr <> ' '
081400081006     c                   add       1             prtnmtr
081500081006     c                   endif
081600081006      ***
081700081006     c                   endif
081800081006
081900081006     c                   endif
082000081006
082100060530
082200060518     c                   Endsr
082300060515
082400060519      *
082500060519      *---------------------------------------------------------------*
082600060519      *  Recupero importi di tassazione dal file titai
082700060519      *---------------------------------------------------------------*
082800060519     c     Ric_tai       Begsr
082900060519      *
083000060519     c                   z-add     0             k
083100060519
083200060519     c     kspe          setll     titai01l
083300060519
083400060519     c                   If        %equal(titai01l)
083500060519
083600060519     c                   do        *hival
083700060519
083800060522     c     kspe          reade     titai01l
083900060519
084000060519     c                   If        %eof(titai01l)
084100060519     c                   leave
084200060519     c                   endif
084300060519      * imponibile
084400060519     c                   if        taisvt = '£'
084500060519     c                   eval      tasimv = taivat
084600060519     c                   endif
084700060519      * porto
084800060519     c                   if        taisvt = ' '
084900060519     c                   eval      taspor = taivat
085000060519     c                   endif
085100060519      * varie
085200060519     c                   if        taisvt <> ' ' and taisvt <> '£'
085300060519     c                   add       1             k
085400060519     c                   eval      sv(k) = taisvt
085500060519     c                   eval      va(k) = taivat
085600060519     c                   endif
085700060519
085800060519     c                   enddo
085900060519
086000060519     c                   else
086100060519      * se non è pretassata pulisco la divisa
086200060519     c                   clear                   tasdiv
086300060519     c                   endif
086400070426      * se tassata fino l'imponibile valorizzo il campo tasman del prtf e il campo rfttmaf del file
086500070426     c                   if        tasimv = 0
086600070426     c                   clear                   tasman
086700070426     c                   else
086800070502     c                   eval      tasman = 'S'
086900070426      * faccio il totale dei tassati manualmente
087000070426     c                   add       1             prtnrtm
087100070426     c                   endif
087200070426
087300070426     c                   eval      rfttmaf = tasman
087400060519
087500060519     c                   endsr
087600060519      *---------------------------------------------------------------*
087700060519      ** Riverca Contrassegno
087800060519      *---------------------------------------------------------------*
087900060519     c     Carcsb        Begsr
088000060519
088100060519     c     Kcsb          chain     Tncsb03l
088200060519     c                   If        %found(tncsb03l) and
088300060519      * solo per stesso tipo bolla
088400060519     c                             tastbl = csbtbl
088500060519     c                   movel     csbsta        Tassta
088600060519     c                   z-add     csbcas        Tascas
088700061016     c*****              movel     csbtpa        Tastic
088800060519     c                   if        csbfus <> *blanks
088900060519     c                   move      csbfus        tastic
089000060519     c                   else
089100060519     c                   move      csbtpi        tastic
089200060519     c                   end
089300060519      * mittente
089400060519     c                   movel     csbvca        tasvca
089500060519     c                   z-add     csbcmb        tascmb
089600060519
089700060519     c                   endif
089800060519
089900060519     c                   endsr
090000060522      *---------------------------------------------------------------*
090100060522      ** Calcolo anteporto VARIA 'N'
090200060522      *---------------------------------------------------------------*
090300060522     c     Varian        Begsr
090400060522
090500060522      * richiamo il programma di tassazione
090600060522      * senza passare le varie e il porto
090700060522      *
090800060522      * mi salvo le ds di tassazione
090900060522     c                   eval      w_dtas = dtas
091000060522     c                   eval      w_dtasv = dtasv
091100060522      * mi salvo la divisa
091200060522     c                   eval      w_tasdiv = tasdiv
091300060522
091400110701      * tolgo il flag dell'invio mail al destinatario nell'eventualità ci fosse
091500110701     c                   eval      dtas01 = dstasflo
091600110701     c                   clear                   §asemd
091700110701     c                   eval      dstasflo = dtas01
091800110701
091900060522     c                   clear                   dtasv
092000060522     c                   clear                   tasdiv
092100060522     c                   clear                   taspor
092200060522
092300060522     c                   clear                   antepo
092400060522
092500060522     c                   call      'TNSF20R'
092600060522     c                   parm                    kpjba
092700060522     c                   parm                    dtas
092800060522     c                   parm                    dtasv
092900060522      * se non ci sono errori valorizzo la varia 'N'
093000060522     c                   if        taserr = ' '
093100060522     c                   z-add     tasimv        antepo
093200060522     c                   endif
093300060522      * ripristino le ds salvate prima
093400060522     c                   eval      dtas = w_dtas
093500060522     c                   eval      dtasv = w_dtasv
093600060522      * valorizzo l'anteporto nella varia N al posto di 88888888  se maggiore di zero
093700060522     c                   if        antepo > 0
093800060522     c                   z-add     antepo        va(xx)
093900060522     c                   endif
094000060522
094100060522     c                   endsr
094200060519      *---------------------------------------------------------------*
094300060519      ** CARICAMENTO codici bolla che prevedono di tassare solo 1 varia
094400060519      *   e che permettono imponibile =0
094500081006      ** CARICAMENTO codici bolla che non sono c/s o recuperi per
094600081006      *   scaglionare
094700060519      *---------------------------------------------------------------*
094800060519     c     Carcbo        Begsr
094900081006      **
095000081006     c                   Movel     'TB'          Cod
095100081006     c                   z-add     0             z
095200081006     c     Ktab          setll     tabel00f
095300081006     c                   do        *hival
095400081006     c     Ktab          reade     tabel00f
095500081006      * fine file
095600081006     c                   If        %eof(tabel00f)
095700081006     c                   leave
095800081006     c                   endif
095900081006    1
096000081006    2c                   If        tblkey <> *blanks
096100081006     c                   Movel     TBLuni        DStb
096200081006      * imponibile a zero
096300081006    3c                   If        §tbrbl  = 'N'
096400081006     c                   add       1             z
096500081006     c                   movel     TBLkey        tbo_oK(z)
096600081006    3c                   endif
096700081006      **
096800081006    2c                   endif
096900081006    1c                   enddo
097000081006
097100060519      **
097200060519     c                   Movel     '3A'          Cod
097300060519     c                   z-add     0             K
097400060519     c                   z-add     0             KK
097500060519     c     Ktab          setll     tabel00f
097600060519     c                   do        *hival
097700060519     c     Ktab          reade     tabel00f
097800060519      * fine file
097900060519     c                   If        %eof(tabel00f)
098000060519     c                   leave
098100060519     c                   endif
098200060519    1
098300060519    2c                   If        tblkey <> *blanks
098400060519     c                   Movel     TBLuni        DS3a
098500060519      * imponibile a zero
098600060519    3c                   If        §3aim0  = 'S'
098700060519     c                   add       1             K
098800060519     c                   movel     TBLkey        im0(k)
098900060519    3c                   endif
099000060519      * bolla con singola varia
099100060519     c                   If        §3asva  <> *blanks
099200060519     c                   add       1             Kk
099300060519     c                   movel     tblkey        cbo(kk)
099400060519     c                   movel     §3asva        cbv(kk)
099500060519    3c                   endif
099600081006      * bolla con tipo bolla normale
099700081006     c     §3atb1        lookup    tbo_ok                                 30
099800081006     c                   if        %found
099900081006     c                   add       1             zz
100000081006     c                   movel     tblkey        cbo_ok(zz)
100100081006     c                   endif
100200060519      **
100300060519    2c                   endif
100400060519    1c                   enddo
100500060519
100600060519     c                   Endsr
100700141217
100800141217      /free
100900141217       //--------------------------------------------------------------
101000141217       //?Carico particolarità consegna con Pin Code.
101100141217       //--------------------------------------------------------------
101200141217       BEGSR Car7RPinCode;
101300141217
101400141217         cod = '7R';
101500141217         clear kk;
101600141217         setll (Codut:cod) TABEL00F;
101700141217         reade (Codut:cod) TABEL00F;
101800141217         DOW  not %eof(TABEL00F);
101900141217           IF  TBLflg = *blanks;
102000141217             ds7R = TBLuni;
102100141217             IF  §7Rpincode = 'S';
102200141217               kk += 1;
102300141217               skGMA(kk) = TBLkey;
102400141217             ENDIF;
102500141217           ENDIF;
102600141217           reade (Codut:cod) TABEL00F;
102700141217         ENDDO;
102800141217
102900141217       ENDSR;
103000141217      /end-free
103100141217
103200081006      *---------------------------------------------------------------*
103300081006      * Caricamento scaglioni
103400081006      *---------------------------------------------------------------*
103500081006     c     Car_scaglioni BEGSR
103600081006
103700081006     c                   eval      savprg = 999
103800081006     c                   clear                   yy
103900081006     c                   clear                   tipo_tar
104000081006     c                   clear                   num_sgl
104100081006      * cerco il giusto progressivo
104200081006
104300081006     c     ktam          setll     tntam01l
104400081006     c                   do        *hival
104500081006     c     ktam          reade     tntam01l
104600081006     c                   if        %eof(tntam01l)
104700081006     c                   leave
104800081006     c                   endif
104900081006      * verifico se progressivo annullato
105000081006     c                   if        tamatb <> ' '
105100081006     c                   iter
105200081006     c                   endif
105300081006      * controllo con la data di riferimento
105400120216     c                   If        rftdspr  < tamdst and rftdspr >= tamddt
105500081006     c                   eval      savprg = tamprg
105600081006     c                   leave
105700081006     c                   endif
105800081006     c                   if        rftdspr > tamdst
105900081006     c                   eval      savprg = tamprg
106000081006     c                   iter
106100081006     c                   endif
106200081006     c                   enddo
106300081006
106400081006     c                   if        savprg <> 999
106500081006      * trovato progressivo carico gli scaglioni relativi leggendo da titad
106600081006     c     ktad          setll     titad04l
106700110713     c                   do        *hival
106800081006     c     ktad          reade     titad04l
106900110713     c                   if        %eof(titad04l)
107000110713     c                   leave
107100110713     c                   endif
107200110713     c                   if        tadatb = ' '
107300081006      * recupero la chiave
107400081006     c                   eval      savlnp = tadlnp
107500081006     c                   eval      savorp = tadorp
107600081006     c                   eval      savnaz = tadnaz
107700081006     c                   eval      savcap = tadcap
107800110713     c                   leave
107900110713     c                   endif
108000110713     c                   enddo
108100081006
108200081006      * leggo tutti gli scaglioni con la stessa chiave
108300081006     c     ktad2         setll     titad04l
108400081006
108500081006     c                   do        *hival
108600081006     c     ktad2         reade     titad04l
108700081006
108800081006     c                   if        %eof(titad04l)
108900081006     c                   leave
109000081006     c                   endif
109100081006
109200081006     c                   if        tadatb <> ' '
109300081006     c                   iter
109400081006     c                   endif
109500081006
109600081006     c     tadsgl        lookup    sgl                                    31
109700081006
109800081006     c                   if        %found
109900081006     c                   leave
110000081006     c                   endif
110100081006
110200081006     c                   add       1             yy
110300081006     c                   eval      sgl(yy) = tadsgl
110400081006     c                   enddo
110500081006      * valorizzo il numero totali di scaglioni
110600081006     c                   z-add     yy            num_sgl
110700081006
110800081006     c                   movel     rftctrr       tipo_tar
110900081006
111000081006     c                   endif
111100081006      *
111200081006     c                   endsr
111300081006      *---------------------------------------------------------------*
111400081006      *  Ricerca dello scaglione per spedizione tassata
111500081006      *---------------------------------------------------------------*
111600081006     c     Ric_sgl       Begsr
111700081006
111800081006     c                   z-add     1             yy
111900081006
112000081006     c                   dow       yy <= num_sgl
112100081006     c                   if        misura <= sgl(yy)
112200081006     c                   eval      ok_scagl = *on
112300081006     c                   leave
112400081006     c                   endif
112500081006     c                   add       1             yy
112600081006     c                   enddo
112700081006
112800081006     c                   endsr
112900120921
113000120921      /free
113100120921       //--------------------------------------------------------------
113200120921       //?Memorizzo in SK il trasporto e le varie.
113300120921       //--------------------------------------------------------------
113400120921       BEGSR  Mem_Varie;
113500120921
113600120921       //?Il porto sempre al primo posto
113700120921         sk_Voce(1) = '.';
113800120921         IF  RFTporf > *zero;
113900120921           sk_ImpoF(1) += RFTporF;
114000120921           sk_ImpoR(1) += RFTporR;
114100120921         ENDIF;
114200120921
114300120921       //?Poi le varie
114400120924         IF  RFTva1F > *zero;
114500120921           Wvaria = RFTsv1F;
114600120921           WimpoF = RFTva1F;
114700120924           exsr  TotVariaF;
114800120924         ENDIF;
114900120924         IF  RFTva1R > *zero;
115000120924           Wvaria = RFTsv1R;
115100120921           WimpoR = RFTva1R;
115200120924           exsr  TotVariaR;
115300120921         ENDIF;
115400120921
115500120921         IF  RFTva2F > *zero;
115600120921           Wvaria = RFTsv2F;
115700120921           WimpoF = RFTva2F;
115800120924           exsr  TotVariaF;
115900120921         ENDIF;
116000120924         IF  RFTva2R > *zero;
116100120924           Wvaria = RFTsv2R;
116200120924           WimpoR = RFTva2R;
116300120924           exsr  TotVariaR;
116400120924         ENDIF;
116500120921
116600120921         IF  RFTva3F > *zero;
116700120921           Wvaria = RFTsv3F;
116800120921           WimpoF = RFTva3F;
116900120924           exsr  TotVariaF;
117000120921         ENDIF;
117100120924         IF  RFTva3R > *zero;
117200120924           Wvaria = RFTsv3R;
117300120924           WimpoR = RFTva3R;
117400120924           exsr  TotVariaR;
117500120924         ENDIF;
117600120921
117700120921         IF  RFTva4F > *zero;
117800120921           Wvaria = RFTsv4F;
117900120921           WimpoF = RFTva4F;
118000120924           exsr  TotVariaF;
118100120921         ENDIF;
118200120924         IF  RFTva4R > *zero;
118300120924           Wvaria = RFTsv4R;
118400120924           WimpoR = RFTva4R;
118500120924           exsr  TotVariaR;
118600120924         ENDIF;
118700120921
118800120921         IF  RFTva5F > *zero;
118900120921           Wvaria = RFTsv5F;
119000120921           WimpoF = RFTva5F;
119100120924           exsr  TotVariaF;
119200120921         ENDIF;
119300120924         IF  RFTva5R > *zero;
119400120924           Wvaria = RFTsv5R;
119500120924           WimpoR = RFTva5R;
119600120924           exsr  TotVariaR;
119700120924         ENDIF;
119800120921
119900120921         IF  RFTva6F > *zero;
120000120921           Wvaria = RFTsv6F;
120100120921           WimpoF = RFTva6F;
120200120924           exsr  TotVariaF;
120300120921         ENDIF;
120400120924         IF  RFTva6R > *zero;
120500120924           Wvaria = RFTsv6R;
120600120924           WimpoR = RFTva6R;
120700120924           exsr  TotVariaR;
120800120924         ENDIF;
120900120921
121000120921         IF  RFTva7F > *zero;
121100120921           Wvaria = RFTsv7F;
121200120921           WimpoF = RFTva7F;
121300120924           exsr  TotVariaF;
121400120921         ENDIF;
121500120924         IF  RFTva7R > *zero;
121600120924           Wvaria = RFTsv7R;
121700120924           WimpoR = RFTva7R;
121800120924           exsr  TotVariaR;
121900120924         ENDIF;
122000120921
122100120921         IF  RFTva8F > *zero;
122200120921           Wvaria = RFTsv8F;
122300120921           WimpoF = RFTva8F;
122400120924           exsr  TotVariaF;
122500120921         ENDIF;
122600120924         IF  RFTva8R > *zero;
122700120924           Wvaria = RFTsv8R;
122800120924           WimpoR = RFTva8R;
122900120924           exsr  TotVariaR;
123000120924         ENDIF;
123100120921
123200120921         IF  RFTva9F > *zero;
123300120921           Wvaria = RFTsv9F;
123400120921           WimpoF = RFTva9F;
123500120924           exsr  TotVariaF;
123600120921         ENDIF;
123700120924         IF  RFTva9R > *zero;
123800120924           Wvaria = RFTsv9R;
123900120924           WimpoR = RFTva9R;
124000120924           exsr  TotVariaR;
124100120924         ENDIF;
124200120921
124300120921         IF  RFTv10F > *zero;
124400120921           Wvaria = RFTs10F;
124500120921           WimpoF = RFTv10F;
124600120924           exsr  TotVariaF;
124700120921         ENDIF;
124800120924         IF  RFTv10R > *zero;
124900120924           Wvaria = RFTs10R;
125000120924           WimpoR = RFTv10R;
125100120924           exsr  TotVariaR;
125200120924         ENDIF;
125300120921
125400120921         IF  RFTv11F > *zero;
125500120921           Wvaria = RFTs11F;
125600120921           WimpoF = RFTv11F;
125700120924           exsr  TotVariaF;
125800120921         ENDIF;
125900120924         IF  RFTv11R > *zero;
126000120924           Wvaria = RFTs11R;
126100120924           WimpoR = RFTv11R;
126200120924           exsr  TotVariaR;
126300120924         ENDIF;
126400120921
126500120921         IF  RFTv12F > *zero;
126600120921           Wvaria = RFTs12F;
126700120921           WimpoF = RFTv12F;
126800120924           exsr  TotVariaF;
126900120921         ENDIF;
127000120924         IF  RFTv12R > *zero;
127100120924           Wvaria = RFTs12R;
127200120924           WimpoR = RFTv12R;
127300120924           exsr  TotVariaR;
127400120924         ENDIF;
127500120921
127600120921         IF  RFTv13F > *zero;
127700120921           Wvaria = RFTs13F;
127800120921           WimpoF = RFTv13F;
127900120924           exsr  TotVariaF;
128000120921         ENDIF;
128100120924         IF  RFTv13R > *zero;
128200120924           Wvaria = RFTs13R;
128300120924           WimpoF = RFTv13R;
128400120924           exsr  TotVariaR;
128500120924         ENDIF;
128600120921
128700120921         IF  RFTv14F > *zero;
128800120921           Wvaria = RFTs14F;
128900120921           WimpoF = RFTv14F;
129000120924           exsr  TotVariaF;
129100120921         ENDIF;
129200120924         IF  RFTv14R > *zero;
129300120924           Wvaria = RFTs14R;
129400120924           WimpoR = RFTv14R;
129500120924           exsr  TotVariaR;
129600120924         ENDIF;
129700120921
129800120921         IF  RFTv15F > *zero;
129900120921           Wvaria = RFTs15F;
130000120921           WimpoF = RFTv15F;
130100120924           exsr  TotVariaF;
130200120921         ENDIF;
130300120924         IF  RFTv15R > *zero;
130400120924           Wvaria = RFTs15R;
130500120924           WimpoR = RFTv15R;
130600120924           exsr  TotVariaR;
130700120924         ENDIF;
130800120921
130900120921         IF  RFTv16F > *zero;
131000120921           Wvaria = RFTs16F;
131100120921           WimpoF = RFTv16F;
131200120924           exsr  TotVariaF;
131300120921         ENDIF;
131400120924         IF  RFTv16R > *zero;
131500120924           Wvaria = RFTs16R;
131600120924           WimpoR = RFTv16R;
131700120924           exsr  TotVariaR;
131800120924         ENDIF;
131900120921
132000120921         IF  RFTv17F > *zero;
132100120921           Wvaria = RFTs17F;
132200120921           WimpoF = RFTv17F;
132300120924           exsr  TotVariaF;
132400120921         ENDIF;
132500120924         IF  RFTv17R > *zero;
132600120924           Wvaria = RFTs17R;
132700120924           WimpoR = RFTv17R;
132800120924           exsr  TotVariaR;
132900120924         ENDIF;
133000120921
133100120921         IF  RFTv18F > *zero;
133200120921           Wvaria = RFTs18F;
133300120921           WimpoF = RFTv18F;
133400120924           exsr  TotVariaF;
133500120921         ENDIF;
133600120924         IF  RFTv18R > *zero;
133700120924           Wvaria = RFTs18R;
133800120924           WimpoR = RFTv18R;
133900120924           exsr  TotVariaR;
134000120924         ENDIF;
134100120921
134200120921         IF  RFTv19F > *zero;
134300120921           Wvaria = RFTs19F;
134400120921           WimpoF = RFTv19F;
134500120924           exsr  TotVariaF;
134600120921         ENDIF;
134700120924         IF  RFTv19R > *zero;
134800120924           Wvaria = RFTs19R;
134900120924           WimpoR = RFTv19R;
135000120924           exsr  TotVariaR;
135100120924         ENDIF;
135200120921
135300120921         IF  RFTv20F > *zero;
135400120921           Wvaria = RFTs20F;
135500120921           WimpoF = RFTv20F;
135600120924           exsr  TotVariaF;
135700120921         ENDIF;
135800120924         IF  RFTv20R > *zero;
135900120924           Wvaria = RFTs20R;
136000120924           WimpoR = RFTv20R;
136100120924           exsr  TotVariaR;
136200120924         ENDIF;
136300120921
136400120921       ENDSR;
136500120921
136600120921       //--------------------------------------------------------------
136700120924       //?Incremento totali fatturato per voce in schiera.                       ?
136800120921       //--------------------------------------------------------------
136900120924       BEGSR  TotVariaF;
137000120921
137100120921         ww = %lookup( Wvaria : sk_Voce : 2 );
137200120921         IF  ww = *zero;
137300120921           ww = %lookup( *blank : sk_Voce : 2 );
137400120921           IF  ww > *zero;
137500120921             sk_Voce(ww) = Wvaria;
137600120921           ENDIF;
137700120921         ENDIF;
137800120921
137900120921         sk_ImpoF(ww) += WimpoF;
138000120921
138100120921       ENDSR;
138200120924
138300120924       //--------------------------------------------------------------
138400120924       //?Incremento totali per voce rifatturato in schiera.                       ?
138500120924       //--------------------------------------------------------------
138600120924       BEGSR  TotVariaR;
138700120924
138800120924         ww = %lookup( Wvaria : sk_Voce : 2 );
138900120924         IF  ww = *zero;
139000120924           ww = %lookup( *blank : sk_Voce : 2 );
139100120924           IF  ww > *zero;
139200120924             sk_Voce(ww) = Wvaria;
139300120924           ENDIF;
139400120924         ENDIF;
139500120924
139600120924         sk_ImpoR(ww) += WimpoR;
139700120924
139800120924       ENDSR;
139900120924
140000120924      /end-free
140100081006      *
140200060519      *---------------------------------------------------------------*
140300060519      * Operazioni Iniziali                                           *
140400060519      *---------------------------------------------------------------*
140500060519     c     RoutInz       BEGSR
140600060519      *
140700060519      * Reperisco dati job
140800060519     c                   exsr      DatiJob
140900060519      * Reperisco data e ora
141000060519     c                   time                    w0140
141100060519     c                   movel     w0140         wora
141200060522     c                   move      w0140         wdate
141300060519      *
141400060801      * verifico se sono in filiale faccio le ovrdbf per i file di sede
141500060801     c                   If        dutlpo <> 'S'
141600060801      * librerie
141700060801
141800060801      * Se sono in ambiente buono - GAITRAGRU
141900060801     c                   If        knsif = 'FILTRA201 '
142000060801     c                   Eval      wlib = 'GAITRAGRU '
142100060801     c                   Eval      wlib1 = 'GAITRAAZM '
142200060801      *  Se sono in ambiente di prova - GAITRAGRPS
142300060801     c                   Else
142400060801     c                   Eval      wlib = 'GAITRAGRPS'
142500060801     c                   Eval      wlib1 = 'GAITRAAZM '
142600060801     c                   EndIf
142700060801      * WFRFT01L
142800060801     c                   Clear                   comman
142900060801     c                   Movel(p)  cmdt(1)       comman
143000060801     c                   Eval      %Subst(comman:30:10) = wlib1
143100060801     c                   Eval      comman =
143200060801     c                             %trim(comman) + '/WFRFT01L)'
143300060801     c                   Call      'QCMDEXC'                            99
143400060801     c                   Parm                    comman
143500060801     c                   Parm                    lenght
143600060801
143700060801      * TITAI01L
143800060801     c                   Clear                   comman
143900060801     c                   Movel(p)  cmdt(2)       comman
144000060801     c                   Eval      %Subst(comman:30:10) = wlib
144100060801     c                   Eval      comman =
144200060801     c                             %trim(comman) + '/TITAI01L)'
144300060801     c                   Call      'QCMDEXC'                            99
144400060801     c                   Parm                    comman
144500060801     c                   Parm                    lenght
144600060801
144700060801      * TITAS30C
144800060801     c                   Clear                   comman
144900060801     c                   Movel(p)  cmdt(3)       comman
145000060801     c                   Eval      %Subst(comman:30:10) = wlib
145100060801     c                   Eval      comman =
145200060801     c                             %trim(comman) + '/TITAS30C)'
145300060801     c                   Call      'QCMDEXC'                            99
145400060801     c                   Parm                    comman
145500060801     c                   Parm                    lenght
145600060801      * FIAR531C
145700060801     c                   Clear                   comman
145800060801     c                   Movel(p)  cmdt(4)       comman
145900060801     c                   Eval      %Subst(comman:30:10) = wlib
146000060801     c                   Eval      comman =
146100060801     c                             %trim(comman) + '/FIAR531C)'
146200060801     c                   Call      'QCMDEXC'                            99
146300060801     c                   Parm                    comman
146400060801     c                   Parm                    lenght
146500060801      * TNCSB03L
146600060801     c                   Clear                   comman
146700060801     c                   Movel(p)  cmdt(5)       comman
146800060801     c                   Eval      %Subst(comman:30:10) = wlib
146900060801     c                   Eval      comman =
147000060801     c                             %trim(comman) + '/TNCSB03L)'
147100060801     c                   Call      'QCMDEXC'                            99
147200060801     c                   Parm                    comman
147300060801     c                   Parm                    lenght
147400060801
147500060801     c                   endif
147600060801      *
147700060801     c                   open      wfrft01l
147800060801     c                   open      titai01l
147900060801     c                   open      titas30c
148000060801     c                   open      fiar531c
148100060801     c                   open      tncsb03l
148200060801
148300060519     c                   ENDSR
148400060519      *
148500060519      *---------------------------------------------------------------*
148600060519      * Reperimento Dati del job (Utente/Operativi)                   *
148700060519      *---------------------------------------------------------------*
148800060519     c     DatiJob       BEGSR
148900060519      *
149000060519     c     *dtaara       define    §azute        azuteds
149100060519     c     *dtaara       define    §datiute      ddatiute
149200060519      *
149300060519     c                   in(E)     *dtaara
149400060519     c                   IF        %ERROR or RSUT = *blanks
149500060519     c                   clear                   Tibs34Ds
149600060519     c                   call      'TIBS34R'
149700060519     c                   parm                    Tibs34Ds
149800060519     c                   in        *dtaara
149900060519     c                   Endif
150000060519
150100060519      *
150200060519     c                   ENDSR
150300060801** cmdt
150400060801OVRDBF FILE(WFRFT01L) TOFILE(
150500060801OVRDBF FILE(TITAI01L) TOFILE(
150600060801OVRDBF FILE(TITAS30C) TOFILE(
150700060801OVRDBF FILE(FIAR531C) TOFILE(
150800060801OVRDBF FILE(TNCSB03L) TOFILE(
150900060801DLTOVR FILE(*ALL)
