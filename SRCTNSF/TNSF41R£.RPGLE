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
009100151127
009200151127      // - Rcd GEN file FIAR5
009300151127     d dAR5gen       e ds                  inz
009400120921
009500120921       // -?Tabella "CC" = Codici Tassazione?
009600120921     d dsCC          e ds                  inz
009700141217
009800141217       //?Ds tabella 7R - part.consegna
009900141217     d ds7R          e ds
010000141217
010100120921
010200060518      * - Ds per scrittura file di lavoro
010300060518     d                 ds
010400060518     d  RFTsv1r                1      1
010500060518     d  RFTsv2r                2      2
010600060518     d  RFTsv3r                3      3
010700060518     d  RFTsv4r                4      4
010800060518     d  RFTsv5r                5      5
010900060518     d  RFTsv6r                6      6
011000060518     d  RFTsv7r                7      7
011100060518     d  RFTsv8r                8      8
011200060518     d  RFTsv9r                9      9
011300060518     d  RFTs10r               10     10
011400060518     d  RFTs11r               11     11
011500060518     d  RFTs12r               12     12
011600060518     d  RFTs13r               13     13
011700060518     d  RFTs14r               14     14
011800060518     d  RFTs15r               15     15
011900060518     d  RFTs16r               16     16
012000060518     d  RFTs17r               17     17
012100060518     d  RFTs18r               18     18
012200060518     d  RFTs19r               19     19
012300060518     d  RFTs20r               20     20
012400060518     d  RFT_svr                1     20    Dim(20)                              Sigle varie
012500060512
012600060518      * - Ds per scrittura file di lavoro
012700060518     d                 ds
012800060518     d  RFTva1r                1     11  3
012900060518     d  RFTva2r               12     22  3
013000060518     d  RFTva3r               23     33  3
013100060518     d  RFTva4r               34     44  3
013200060518     d  RFTva5r               45     55  3
013300060518     d  RFTva6r               56     66  3
013400060518     d  RFTva7r               67     77  3
013500060518     d  RFTva8r               78     88  3
013600060518     d  RFTva9r               89     99  3
013700060518     d  RFTv10r              100    110  3
013800060518     d  RFTv11r              111    121  3
013900060518     d  RFTv12r              122    132  3
014000060518     d  RFTv13r              133    143  3
014100060518     d  RFTv14r              144    154  3
014200060518     d  RFTv15r              155    165  3
014300060518     d  RFTv16r              166    176  3
014400060518     d  RFTv17r              177    187  3
014500060518     d  RFTv18r              188    198  3
014600060518     d  RFTv19r              199    209  3
014700060518     d  RFTv20r              210    220  3
014800060518     d  RFT_var                1    220  3 Dim(20)                              Importo varie
014900060504      *
015000060504      * - Controllo/Inversione date
015100060504     d WLBdat          ds                  inz
015200060504     d   G08dat                       8  0 inz
015300060504     d   G08inv                       8  0 inz
015400060504     d   G08err                       1    inz('3')
015500060504     d   G08tgi                       5  0 inz
015600060504      *
015700060504      * - Variabili definite a programma
015800060519     d codut           s              1  0 inz(1)
015900060504     d Wdate           s              8  0 inz
016000060518     d Wora            s              6  0 inz
016100060516     d W0140           s             14  0 inz
016200081006     d z               s              4  0
016300081006     d zz              s              4  0
016400060518     d k               s              4  0
016500060519     d kk              s              4  0
016600060522     d xx              s              4  0
016700081006     d yy              s              4  0
016800120921     d ww              s              4  0
016900081006     d num_sgl         s              4  0
017000060522     d kscfil          s              3  0
017100081006     d tipo_tar        s              1  0
017200081006     d ok_scagl        s               n
017300060522     d cod             s                   like(tblcod)
017400081006     d key             s                   like(tblkey)
017500060522     d kAr5Trd         s                   Like(Ar5Trd)
017600060522     d antepo          s                   like(tasimv)
017700060522     d W_tasdiv        s                   like(tasdiv)
017800060522     d W_dtas          s                   like(dtas)
017900060522     d W_dtasv         s                   like(dtasv)
018000060522     D Wcev            s                   like(evbcev) inz('RIC')
018100081006     d prg2            s                   like(tamprg) inz
018200081006     d ctr2            s                   like(tamctr) inz
018300081006     d ksc2            s                   like(tamksc) inz
018400081006     d savprg          s                   like(tamprg)
018500081006     d savlnp          s                   like(tadlnp)
018600081006     d savorp          s                   like(tadorp)
018700081006     d savnaz          s                   like(tadnaz)
018800081006     d savcap          s                   like(tadcap)
018900081006     d misura          s                   like(tadsgl)
019000060801
019100060801     d wlib            s             10
019200060801     d wlib1           s             10
019300060801     d comman          s             80
019400060801     d lenght          s             15  5 inz(80)
019500060801
019600120921       // -?Campi di comodo?
019700120921     d Wconta          s              1  0 inz
019800120921     d Wvaria          s                   like(TASsv1)   inz
019900120921     d WimpoF          s                   like(TASva1)   inz
020000120921     d WimpoR          s                   like(TASva1)   inz
020100060522
020200060504      *
020300060504      * Key-List - - - - - - - - - - - - - - - - - - - - - - - - - - -*
020400060504      *
020500060518      * - WFRFT01L
020600060518     c     KRFT          Klist
020700060518     c                   Kfld                    I40noj
020800060518     c                   Kfld                    I40pru
020900060518     c                   Kfld                    I40dta
021000060518     c                   Kfld                    I40ora
021100060517
021200060518      * - TITAI01L / TITAS30C
021300060518     c     KSPE          Klist
021400060518     c                   Kfld                    RFTaas
021500060518     c                   Kfld                    RFTlnp
021600060518     c                   Kfld                    RFTnrs
021700060518     c                   Kfld                    RFTnsp
021800060518     c                   Kfld                    RFTtbl
021900060519      * Tncsb03l
022000060519     c     KCSB          Klist
022100060519     c                   Kfld                    TASaas
022200060519     c                   Kfld                    TASlnp
022300060519     c                   Kfld                    TASnrs
022400060519     c                   Kfld                    TASnsp
022500060519      * Tabel00f
022600060519     c     KTAB          Klist
022700060519     c                   Kfld                    codut
022800060519     c                   Kfld                    cod
022900081006     c     KTABkey       Klist
023000081006     c                   Kfld                    codut
023100081006     c                   Kfld                    cod
023200081006     c                   Kfld                    key
023300060522      * Fiar531c
023400060522     c     kFiar5        Klist
023500060522     c                   Kfld                    TasAas
023600060522     c                   Kfld                    TasLnp
023700060522     c                   Kfld                    TasNrs
023800060522     c                   Kfld                    TasNsp
023900060522     c                   Kfld                    kAr5Trd
024000060522      * Chiave file eventi FNEVB
024100060522     c     Kevb4         Klist
024200060522     c                   Kfld                    Tasaas
024300060522     c                   Kfld                    Taslnp
024400060522     c                   Kfld                    Tasnrs
024500060522     c                   Kfld                    Tasnsp
024600060522     c                   Kfld                    Wcev
024700081006      ** accesso tntam x cliente
024800081006     c     Ktam          Klist
024900081006     c                   Kfld                    rftkscr
025000081006     c                   Kfld                    rftctrr
025100081006      ** accesso titad
025200081006     c     Ktad          Klist
025300081006     c                   Kfld                    tamksc
025400081006     c                   Kfld                    tamctr
025500081006     c                   Kfld                    tamprg
025600081006
025700081006     c     Ktad2         Klist
025800081006     c                   Kfld                    tamksc
025900081006     c                   Kfld                    tamctr
026000081006     c                   Kfld                    tamprg
026100081006     c                   Kfld                    savlnp
026200081006     c                   Kfld                    savorp
026300081006     c                   Kfld                    savnaz
026400081006     c                   Kfld                    savcap
026500060504
026600060504      *---------------------------------------------------------------*
026700060504      *  RIEPILOGO INDICATORI                                         *
026800060504      *---------------------------------------------------------------*
026900060727      * 05/30 - Comodo
027000060727      * 50    - Data fattura antecedente 30-04-06 stampo scritta
027100060727      * 51    - Comodo
027200060727      * 90    - OF
027300060727      * 99    - Comodo
027400060504      *---------------------------------------------------------------*
027500060504
027600060504     c     *Entry        plist
027700060504     c                   parm                    KPJBA
027800060518     c                   movel     kpjbu         Tnsf40ds
027900060504      *
028000060504      * Operazioni Iniziali
028100060504     c                   exsr      RoutInz
028200060504      *
028300060523     c                   exsr      Carcbo
028400141217      /free
028500141217       //?Rielaboro quello rimasto nel file in QTEMP
028600141217         exsr Car7RPinCode;
028700141217      /end-free
028800060523      *
028900060518      * Elaborazione del file
029000060518     c
029100060518do  1c                   exsr      Elabora
029200060504      * 1° videata
029300060518     c                   exsr      Stampa
029400060504      * Fine
029500060509      *
029600060512     c     fine          tag
029700060801
029800060801     c                   If        Dutlpo <> 'S'
029900060801     c                   Clear                   comman
030000060801     c                   Movel(p)  cmdt(6)       comman
030100060801     c                   Call      'QCMDEXC'                            99
030200060801     c                   Parm                    comman
030300060801     c                   Parm                    lenght
030400060801     c                   endif
030500060801
030600060504     c                   movel     *on           *inLR
030700060504      *
030800060504      *
030900060504      *---------------------------------------------------------------*
031000060518      * Elabora ritassazione                                          *
031100060504      *---------------------------------------------------------------*
031200060518     c     Elabora       BEGSR
031300060504      *
031400060518e   1c     Krft          setll     wfrft01l
031500060504
031600060504     c                   do        *hival
031700060522      *
031800060518     c     krft          reade     wfrft01l
031900060518
032000060522     c                   If        %eof(Wfrft01l)
032100060518     c                   leave
032200060518     c                   endif
032300060518      * verifico se record già elaborato
032400060518     c                   if        rftela <> ' '
032500060518     c                   iter
032600060518     c                   endif
032700060726      * imposto i campi della testata
032800060726     c                   if        prtfiv = *zeros
032900060726     c                   eval      prtfiv = rftfiv
033000060726     c                   eval      prtnft = rftnft
033100060726      *
033200060726     c                   clear                   WLBdat
033300060726     c                   eval      G08inv = rftdft
033400060726     c                   eval      G08err = '3'
033500060726     c                   call      'XSRDA8'
033600060726     c                   parm                    WLBdat
033700060726     c                   z-add     G08dat        prtdft
033800081006      * se si desidera tassazione scaglionata recupero gli scaglioni della tariffa
033900081006     c                   If        i40sgl = 'S'
034000081006     c                   exsr      car_scaglioni
034100081006     c                   endif
034200081006
034300060726     c                   endif
034400060727      * controllo data fattura
034500060727     c                   eval      *in50 = rftdft < 20060430
034600060518      * tasso
034700060518     c                   exsr      tassaz
034800060504      *
034900060504     c                   enddo
035000060504      *
035100060504     c                   ENDSR
035200060504      *
035300060522      *---------------------------------------------------------------*
035400060522      * Stampa Finale
035500060522      *---------------------------------------------------------------*
035600060522     c     Stampa        BEGSR
035700060522
035800060522      * testata
035900060530     c                   write     sf41t0
036000060522     c                   write     sf41ts
036100060522     c                   write     sf41t3
036200060522     c                   write     sf41t2
036300060522     c                   write     sf41t3
036400060522      * valorizzo i campi
036500060522     c                   eval      prtnoj = knmeb
036600060522     c                   eval      prtpru = rftpru
036700060522      *
036800060522     c                   clear                   WLBdat
036900060522     c                   eval      G08inv = rftdta
037000060522     c                   eval      G08err = '3'
037100060522     c                   call      'XSRDA8'
037200060522     c                   parm                    WLBdat
037300060522     c                   z-add     G08dat        prtdta
037400060522      *
037500060522     c                   eval      prtora = rftora
037600060614      * calcolo la differente
037700060614     c                   eval      prtdiffe = prtimvf - prtimvr
037800060522      *
037900060522     c                   write     sf41d3
038000060522      *
038100081006      * stampo la suddivisione per scaglione solo se richiesta
038200081006     c                   If        i40sgl = 'S'
038300081006     c                   eval      cod = 'TR'
038400081006     c                   movel(p)  tipo_tar      key
038500081006     c     ktabkey       chain     Tabel00f
038600081006     c                   if        %found(tabel00f)
038700081006     c                   movel     tbluni        dstr
038800081006      * valorizzo i campi descrittivi di stampa
038900081006     c                   movel     §trdst        prttar
039000081006     c                   endif
039100081006      * testata
039200081006     c                   write     sf41t04
039300081006     c                   write     sf41t3
039400081006     c                   write     sf41t4
039500081006     c                   write     sf41t3
039600081006
039700081006      * stampo dettaglio per ogni scaglione
039800081006     c                   z-add     1             yy
039900081006     c                   dow       yy <= num_sgl
040000081006
040100081006     c                   eval      prtsca = sgl(yy)
040200081006     c                   eval      prtimf = imf(yy)
040300081006     c                   eval      prtimr = imr(yy)
040400081006     c                   eval      prtdif = (prtimf - prtimr)
040500081006     c                   eval      prttkg = tkg(yy)
040600081006     c                   eval      prttcl = tcl(yy)
040700081006     c                   eval      prttvl = tvl(yy)
040800081006     c                   eval      prtnsp = nsp(yy)
040900081006     c                   eval      prtnmt = nmt(yy)
041000081006
041100081006     c                   write     sf41d4
041200081006      * pulisco la descrizione della tariffa
041300081006     c                   clear                   prttar
041400081006
041500081006     c                   add       1             yy
041600081006
041700081006     c                   enddo
041800081006      *
041900081006      * stampo se ci sono delle bolle di recupero
042000081006     c                   If        prtnspr > 0
042100081006     c                   write     sf41d4r
042200081006     c                   endif
042300081006      *
042400081006     c                   endif
042500120921      /free
042600120921       //?Stampa la suddivisione per voce se richiesta
042700120921        IF  I40sva = 'S';
042800120921          exsr  Stampa_TOT;
042900120921        ENDIF;
043000120921      /end-free
043100081006      *
043200060522     c                   Endsr
043300120921
043400120921      /free
043500120921       //--------------------------------------------------------------
043600120921       //?Stampa suddivisione Voci.
043700120921       //--------------------------------------------------------------
043800120921       BEGSR  Stampa_TOT;
043900120921
044000120921         write  SF41T05;
044100120921         write  SF41T5;
044200120921
044300120921         ww = 1;
044400120921
044500120921       //?SE non ci fosse la voce trasporto (che è sempre alla?
044600120921       //?prima posizione): si inizia a stampare dalla 2ª Varia?
044700120921         IF  sk_ImpoF(1) = *zero;
044800120921           ww = 2;
044900120921         ENDIF;
045000120921
045100120921       //?Stampa delle varie di 2 in 2?
045200120921         DoW  sk_Voce(ww) <> *blank;
045300120921
045400120921           Wconta = 1;
045500120921           clear  SF41D5;
045600120921
045700120921           DoW  Wconta <= 2  and  sk_Voce(ww) <> *blank;
045800120921
045900120921             Wvaria = sk_Voce(ww);
046000120921             if  Wconta = 1;
046100120921               //?Nella prima posizione: Trasporto?
046200120921               if  ww = 1;
046300120921                 P1Dsv1 = 'TRASPORTO      ';
046400120921               else;
046500120921                 exsr  sr_TabCC;
046600120921                 P1Csv1 = sk_Voce(ww) + ' -';
046700120921                 P1Dsv1 = §CCdes;
046800120921               endif;
046900120921               P1Cva1F = sk_ImpoF(ww);
047000120921               P1Cva1R = sk_ImpoR(ww);
047100120921             else;
047200120921               exsr  sr_TabCC;
047300120921               P1Csv2  = sk_Voce(ww) + ' -';
047400120921               P1Dsv2  = §CCdes;
047500120921               P1Cva2F = sk_ImpoF(ww);
047600120921               P1Cva2R = sk_ImpoR(ww);
047700120921             endif;
047800120921
047900120921             ww += 1;
048000120921             Wconta += 1;
048100120921
048200120921           EndDo;
048300120921
048400120921           write  SF41D5;
048500120921
048600120921         EndDo;
048700120921
048800120921         //?Totale Imponibile?
048900120925         //P1CimvF = %xfoot( sk_ImpoF);
049000120925         //P1CimvR = %xfoot( sk_ImpoR);
049100120921
049200120921         //?Stampa Totali per Fattura?
049300120921         write  SF41G5;
049400120921
049500120921       ENDSR;
049600120921
049700120921       //--------------------------------------------------------------
049800120921       //?Decodifica Sigla Varia.                                      ?
049900120921       //--------------------------------------------------------------
050000120921       BEGSR  sr_TabCC;
050100120921
050200120921         clear  dsCC;
050300120921         cod = 'CC';
050400120921         key = 'VARIE  ' + Wvaria;
050500120921
050600120921         chain  (codut:cod:key) TABEL00F;
050700120921
050800120921         IF  %found(TABEL00F);
050900120921           dsCC = TBLuni;
051000120921         ELSE;
051100120921           §CCdes = *all'?';
051200120921         ENDIF;
051300120921
051400120921       ENDSR;
051500120921      /end-free
051600060522      *
051700060515      *---------------------------------------------------------------*
051800060518      *  Routine di tassazione
051900060515      *---------------------------------------------------------------*
052000060518     c     Tassaz        Begsr
052100060518      *
052200060518     c                   clear                   dtas
052300060518     c                   clear                   dtasv
052400151012     c                   clear                   dtaspes
052500060518      * aggancio titas
052600060518     c     KSPE          chain     titas30c
052700060522   a1c                   if        %found(titas30c)
052800070411      * Pulisco i campi dell'assicurazione se calcolata in fattura
052900070411    2c                   IF        Tasfcm = 'F'
053000070411     c                   clear                   tasias
053100070411     c                   clear                   tasvas
053200070411    2c                   Endif
053300070411
053400060530      * valorizzo i campi del file printer
053500060530      *
053600060530     c                   clear                   WLBdat
053700060530     c                   movel     tasaas        g08inv
053800060530     c                   move      tasmgs        g08inv
053900060530     c                   eval      G08err = '3'
054000060530     c                   call      'XSRDA8'
054100060530     c                   parm                    WLBdat
054200060530     c                   z-add     G08dat        prtdsp
054300060530
054400060530     c                   z-add     tasksc        prtksc
054500060530     c                   z-add     tasctr        prtctr
054600060530
054700060530     c                   clear                   WLBdat
054800060530     c                   movel     rftdspr       g08inv
054900060530     c                   eval      G08err = '3'
055000060530     c                   call      'XSRDA8'
055100060530     c                   parm                    WLBdat
055200060530     c                   z-add     G08dat        prtdspr
055300060530
055400060530      *
055500060519      * valorizzo la DS DTAS e DTASV
055600060519     c                   clear                   dtasv
055700060519     c                   clear                   taspor
055800060519     c                   clear                   tasimv
055900151012     c                   clear                   dtaspes
056000060518      *
056100060519      * cerco in TITAI l'immagine della spedizione prima della tassazione
056200060519     c                   exsr      RIC_tai
056300060519      * se tassata fino all'imponibile non tasso
056400060522    1c                   if        tasimv = 0
056500060519      ** Ricerca contrassegno
056600060519    2c                   IF        Tasfca <> *blanks
056700060519     c                   Exsr      Carcsb
056800060519    2c                   Endif
056900060519
057000060519      *  se si tratta di codice bolla che prevede solo una varia, passo solo
057100060519      *  quella
057200060519     c                   z-add     1             kk
057300060519     c     tascbo        lookup    cbo(kk)                                05
057400060522    2c                   if        *in05
057500060519     c                   eval      tassva=cBV(kk)
057600060522    2c                   endif
057700060522
057800060522     c                   movel     Tasftc        tastc1
057900060522     c                   Z-add     rftdft        tasdct
058000060726
058100060726     c                   eval      Tasksc = rftkscr
058200060726     c                   eval      Tasctr = rftctrr
058300061221     c                   movel     rftaas        tasdsp
058400061221     c                   move      rftmgs        tasdsp
058500060726
058600060522      * Flag SI NO DDT
058700060522    2C                   If        tasll1 = 'C' or tasll1 = 'S'
058800060522     C                   movel     'S'           Tasddt
058900060522   x2C                   Else
059000060522     C                   Clear                   Tasddt
059100060522    2C                   Endif
059200060926     c****               movel     tasfbr        dstasflo
059300060926     c                   clear                   dtas01
059400060926     c                   movel     tasfbr        §asfbr
059500060926     c                   movel     tascca        §ascca
059600110701      * valorizzo flag email al destinatario
059700110701     c                   movel     tasflo        dtasflo
059800110701     c                   move      §floemd       §asemd
059900141217      /free
060000151127       //?Imposto se Prenotazione Ritiro Telefonico
060100151127         TASprt = §FLOado;
060200151127
060300141217       //?Imposto se part.consegna con PinCode
060400141217         IF  TASgma <> *blanks and %lookup (TASgma:skGMA) > 0;
060500141217           §ASpin = 'S';
060600141217         ENDIF;
060700151127
060800151127       //?Imposto se Stampa Packing List
060900151127         clear dAR5gen;
061000151127         chain (TASaas:TASlnp:TASnrs:TASnsp:'GEN') FIAR531C;
061100151127         IF  %found(FIAR531C);
061200151127           dAR5gen = AR5uni;
061300151127         ENDIF;
061400151127         IF  §AR5als = 'S' or §AR5alx = 'S';
061500151127           TASspl = 'S';
061600151127         ENDIF;
061700141217      /end-free
061800110701
061900061221      * se data spedizione differente da ritassazione passo nel flo anche la data ritassazione
062000061221     c                   If        tasdsp <> rftdspr
062100070108     c                   move      rftdspr       §asdrt
062200061221     c                   endif
062300061221
062400060926     c                   eval      dstasflo = dtas01
062500060522
062600060522      * Bancali
062700060522    2c                   If        %Subst(TasGva:1:1) = 'B'
062800060522
062900060522     c                   eval      kAr5Trd  = 'BAN'
063000060522     c     kFiar5        Chain     Fiar531c
063100060522If  3c                   If        %Found(Fiar531c)
063200060522     c                   Movel     Ar5Uni        dAr5Ban
063300060522    3c                   EndIf
063400060522      * Bancali
063500060522     c                   Eval      TasBan = §Ar5Nba + §Ar5Nb2
063600060522    2c                   EndIf
063700060522      **
063800060522      * Colli Originali
063900060522    2c                   If        %Subst(TasGva:1:1) = 'O'
064000060522
064100060522     c                   eval      kAr5Trd  = 'BNB'
064200060522     c     kFiar5        Chain     Fiar531c
064300060522If  3c                   If        %Found(Fiar531c)
064400060522     c                   Movel     Ar5Uni        dAr5Bnb
064500060522    3c                   EndIf
064600060522      * Colli
064700060522     c                   Eval      TasNcl = §Ar5bcor
064800060522    2c                   EndIf
064900060522      *
065000060522      * verifico se è una bolla non tassata fino l'imponibile , una bolla di reso ,
065100060522      * con VARIA 'N' valorizzata con 888888 chiamo il TNSF20R per il calcolo dell'anteporto
065200060522      *
065300060522    2c                   if        tasimv = 0 and tasfbr = 'S'
065400060522     c                   z-add     1             xx
065500060522     c     'N'           lookup    sv(xx)                                 30
065600060522    3c                   if        *in30 and va(xx) =  88888888
065700060522      * tasso l'anteporto
065800060522     c                   exsr      Varian
065900060522    3c                   endif
066000060522      *
066100060522    2c                   endif
066200060522      *
066300060522      * se ritorno dal calcolo della varia N e c'è errore non proseguo
066400060522      *
066500060522     c                   Setoff                                       05
066600060522    2c                   if        taserr = ' '
066700060522      * Se devo tassare per una varia e c'e' gia', non richiamo la tassazione  se importo
066800060522      * della varia uguale  a zero
066900060522    3c                   if        tassva<>*blanks and tasimv=0
067000060522     c                   z-add     1             kk
067100060522     c     tassva        lookup    sv(kk)                                 05
067200060522    4c                   if        *in05 and va(kk) > 0
067300060522     c                   setoff                                       05
067400060522    4c                   endif
067500060522    3c                   endif
067600060522      * verifico se esiste la varia & con importo uguale a 8888888 e se si passo al
067700060522      * TNSF20R il flag di calcolo varia &
067800060522      *
067900060522     c                   setoff                                       51
068000060522     c                   z-add     1             kk
068100060522     c     '&'           lookup    sv(kk)                                 51
068200060522    3c                   if        *in51 and va(kk) = 88888888
068300060522     c                   eval      tasfcaa = 'S'
068400060522     c                   clear                   sv(kk)
068500060522     c                   clear                   va(kk)
068600060522    3c                   endif
068700060522      * verifico se devo calcolare l'addebito di lasciato avviso
068800060522     c                   movel     rftkscr       kscfil
068900060522      *
069000060522      * verifico se c'è evento
069100060522     c     Kevb4         chain     Fnevb04l
069200060522
069300060522    4c                   If        %found(Fnevb04l)
069400060522     c                   eval      tasric = 'S'
069500060522    4c                   endif
069600060522
069700060522      **
069800060522      **
069900060522    3c                   if        tassva=*blanks or not *in05
070000151012      * valorizzo i dati nella ds DTASPES
070100151012     c                   eval      tasppkb = taspkb
070101160503     c                   eval      taspfvf = tasfvf
070102160503     c                   eval      taspncr = tasncr
070200151014      * se data tassazione impostata a video prendo quello altrimneti data fattura
070300151014     c                   If        tasdsp <> rftdspr
070400151014     c                   eval      taspdtt = rftdspr
070500151014     c                   else
070600151014     c                   eval      taspdtt = rftdft
070700151014     c                   endif
070800151012
070900151014     c                   call      'TNSF22R'
071000060522     c                   parm                    Kpjba
071100060522     c                   parm                    Dtas
071200060522     c                   parm                    Dtasv
071300151014     c                   parm                    Dtaspes
071400060522    3c                   endif
071500060522      **
071600060522    2c                   endif
071700060522    1c                   endif
071800060519
071900060522     c                   If        taserr = ' '
072000060517      * tassazione
072100060518     c                   eval      RFTdivr = tasdiv
072200060518     c                   eval      RFTporr = taspor
072300060518      * carico eventuali varie presenti in TITA7
072400060518     c                   movea     sv            rft_svr
072500060518     c                   movea     va            rft_var
072600060518      *
072700060518     c                   eval      RFTimvr = tasimv
072800060522     c                   endif
072900060522   a1c                   endif
073000060522
073100060518     c                   eval      RFTela  = 'S'
073200060522     c                   eval      RFTnoj  = Knmeb
073300060517
073400060518     c                   UPDATE    WFRFT000
073500060518
073600060522      * faccio i totali imponibili e conto le spedizioni manca tariffa
073700060522
073800060522     c                   If        taserr <> ' '
073900060522     c                   add       1             prtnrmt
074000060522     c                   endif
074100060522
074200060522     c                   add       RFTimvf       prtimvf
074300060522     c                   add       RFTimvr       prtimvr
074400060518
074500060530      * differenza
074600060530     c                   eval      diffe = rftimvf - rftimvr
074700120921
074800120921      /free
074900120921       //?Metto in SK il trasporto + le varie
075000120921         exsr  Mem_Varie;
075100120921      /end-free
075200060530
075300060530      * STAMPO
075400060530     c                   If        not *in99 or *in90
075500060530      * testata
075600060530      * testata
075700060530     c                   write     sf41t0
075800060530     c                   write     sf41ts
075900060530     c                   write     sf41t3
076000060530     c                   write     sf41t1
076100060530     c                   write     sf41t3
076200060530
076300060530     c                   seton                                        99
076400060530     c                   setoff                                       90
076500060530
076600060530     c                   endif
076700060530
076800060530     c                   write     sf41d1
076900081006
077000081006      * suddivido i valori per scaglioni di !!! in base alla tipologia della tariffa
077100081006
077200081006     c                   If        i40sgl = 'S'
077300081006      * se tipo bolla fa parte delle bolle non di recupero le sommo scaglionate
077400081006      * altrimenti le metto nelle spedizioni di recupero
077500081006     c     tascbo        lookup    cbo_ok                                 30
077600081006     c                   if        %found
077700081006
077800081006     c                   z-add     1             yy
077900081006     c                   eval      ok_scagl = *off
078000081006     c                   select
078100081006      * quantità / valore
078200081006     c                   when      tipo_tar = 5 or tipo_tar = 4
078300081006     c                   z-add     tasqft        misura
078400081006     c                   exsr      ric_sgl
078500081006      * peso
078600081006     c                   when      tipo_tar = 0 or tipo_tar = 3
078700081006     c                   z-add     taspkf        misura
078800081006     c                   exsr      ric_sgl
078900081006      * colli
079000081006     c                   when      tipo_tar = 1
079100081006     c                   z-add     tasncl        misura
079200081006     c                   exsr      ric_sgl
079300081006      * spedizione
079400100302     c                   when      tipo_tar = 2
079500081006     c                   z-add     1             misura
079600081006     c                   exsr      ric_sgl
079700081006     c                   endsl
079800081006
079900081006      * se scaglione trovato valorizzo i campi delle schiere
080000081006     c                   If        ok_scagl
080100081006      * imponibile vecchio
080200081006     c                   add       rftimvf       imf(yy)
080300081006      * imponibile nuovo
080400081006     c                   add       rftimvr       imr(yy)
080500081006      * peso
080600081006     c                   add       taspkf        tkg(yy)
080700081006      * colli
080800081006     c                   add       tasncl        tcl(yy)
080900081006      * volume
081000081006     c                   add       tasvlf        tvl(yy)
081100081006      * spedizioni
081200081006     c                   add       1             nsp(yy)
081300081006      * manca tariffa
081400081006     c                   if        taserr <> ' '
081500081006     c                   add       1             nmt(yy)
081600081006     c                   endif
081700081006
081800081006     c                   endif
081900081006
082000081006      *** se bolla di recupero
082100081006     c                   else
082200081006      * imponibile vecchio
082300081006     c                   add       rftimvf       prtimfr
082400081006      * imponibile nuovo
082500081006     c                   add       rftimvr       prtimrr
082600081006      * spedizioni
082700081006     c                   add       1             prtnspr
082800081006      * manca tariffa
082900081006     c                   if        taserr <> ' '
083000081006     c                   add       1             prtnmtr
083100081006     c                   endif
083200081006      ***
083300081006     c                   endif
083400081006
083500081006     c                   endif
083600081006
083700060530
083800060518     c                   Endsr
083900060515
084000060519      *
084100060519      *---------------------------------------------------------------*
084200060519      *  Recupero importi di tassazione dal file titai
084300060519      *---------------------------------------------------------------*
084400060519     c     Ric_tai       Begsr
084500060519      *
084600060519     c                   z-add     0             k
084700060519
084800060519     c     kspe          setll     titai01l
084900060519
085000060519     c                   If        %equal(titai01l)
085100060519
085200060519     c                   do        *hival
085300060519
085400060522     c     kspe          reade     titai01l
085500060519
085600060519     c                   If        %eof(titai01l)
085700060519     c                   leave
085800060519     c                   endif
085900060519      * imponibile
086000060519     c                   if        taisvt = '£'
086100060519     c                   eval      tasimv = taivat
086200060519     c                   endif
086300060519      * porto
086400060519     c                   if        taisvt = ' '
086500060519     c                   eval      taspor = taivat
086600060519     c                   endif
086700060519      * varie
086800060519     c                   if        taisvt <> ' ' and taisvt <> '£'
086900060519     c                   add       1             k
087000060519     c                   eval      sv(k) = taisvt
087100060519     c                   eval      va(k) = taivat
087200060519     c                   endif
087300060519
087400060519     c                   enddo
087500060519
087600060519     c                   else
087700060519      * se non è pretassata pulisco la divisa
087800060519     c                   clear                   tasdiv
087900060519     c                   endif
088000070426      * se tassata fino l'imponibile valorizzo il campo tasman del prtf e il campo rfttmaf del file
088100070426     c                   if        tasimv = 0
088200070426     c                   clear                   tasman
088300070426     c                   else
088400070502     c                   eval      tasman = 'S'
088500070426      * faccio il totale dei tassati manualmente
088600070426     c                   add       1             prtnrtm
088700070426     c                   endif
088800070426
088900070426     c                   eval      rfttmaf = tasman
089000060519
089100060519     c                   endsr
089200060519      *---------------------------------------------------------------*
089300060519      ** Riverca Contrassegno
089400060519      *---------------------------------------------------------------*
089500060519     c     Carcsb        Begsr
089600060519
089700060519     c     Kcsb          chain     Tncsb03l
089800060519     c                   If        %found(tncsb03l) and
089900060519      * solo per stesso tipo bolla
090000060519     c                             tastbl = csbtbl
090100060519     c                   movel     csbsta        Tassta
090200060519     c                   z-add     csbcas        Tascas
090300061016     c*****              movel     csbtpa        Tastic
090400060519     c                   if        csbfus <> *blanks
090500060519     c                   move      csbfus        tastic
090600060519     c                   else
090700060519     c                   move      csbtpi        tastic
090800060519     c                   end
090900060519      * mittente
091000060519     c                   movel     csbvca        tasvca
091100060519     c                   z-add     csbcmb        tascmb
091200060519
091300060519     c                   endif
091400060519
091500060519     c                   endsr
091600060522      *---------------------------------------------------------------*
091700060522      ** Calcolo anteporto VARIA 'N'
091800060522      *---------------------------------------------------------------*
091900060522     c     Varian        Begsr
092000060522
092100060522      * richiamo il programma di tassazione
092200060522      * senza passare le varie e il porto
092300060522      *
092400060522      * mi salvo le ds di tassazione
092500060522     c                   eval      w_dtas = dtas
092600060522     c                   eval      w_dtasv = dtasv
092700060522      * mi salvo la divisa
092800060522     c                   eval      w_tasdiv = tasdiv
092900060522
093000110701      * tolgo il flag dell'invio mail al destinatario nell'eventualità ci fosse
093100110701     c                   eval      dtas01 = dstasflo
093200110701     c                   clear                   §asemd
093201160113      * tolgo il flag della consegna pin code  nell'eventualità ci fosse
093202160113     c                   clear                   §aspin
093203160113
093300110701     c                   eval      dstasflo = dtas01
093301160113
093302160113      * tolgo flag per calcolo  Diritto di chiamata prenotazione  ritiro e packing list
093303160113      *
093304160113     c                   clear                   TASPRT
093305160113     c                   clear                   TASSPL
093400110701
093500060522     c                   clear                   dtasv
093600060522     c                   clear                   tasdiv
093700060522     c                   clear                   taspor
093800060522
093900060522     c                   clear                   antepo
094000060522
094100060522     c                   call      'TNSF20R'
094200060522     c                   parm                    kpjba
094300060522     c                   parm                    dtas
094400060522     c                   parm                    dtasv
094500060522      * se non ci sono errori valorizzo la varia 'N'
094600060522     c                   if        taserr = ' '
094700060522     c                   z-add     tasimv        antepo
094800060522     c                   endif
094900060522      * ripristino le ds salvate prima
095000060522     c                   eval      dtas = w_dtas
095100060522     c                   eval      dtasv = w_dtasv
095200060522      * valorizzo l'anteporto nella varia N al posto di 88888888  se maggiore di zero
095300060522     c                   if        antepo > 0
095400060522     c                   z-add     antepo        va(xx)
095500060522     c                   endif
095600060522
095700060522     c                   endsr
095800060519      *---------------------------------------------------------------*
095900060519      ** CARICAMENTO codici bolla che prevedono di tassare solo 1 varia
096000060519      *   e che permettono imponibile =0
096100081006      ** CARICAMENTO codici bolla che non sono c/s o recuperi per
096200081006      *   scaglionare
096300060519      *---------------------------------------------------------------*
096400060519     c     Carcbo        Begsr
096500081006      **
096600081006     c                   Movel     'TB'          Cod
096700081006     c                   z-add     0             z
096800081006     c     Ktab          setll     tabel00f
096900081006     c                   do        *hival
097000081006     c     Ktab          reade     tabel00f
097100081006      * fine file
097200081006     c                   If        %eof(tabel00f)
097300081006     c                   leave
097400081006     c                   endif
097500081006    1
097600081006    2c                   If        tblkey <> *blanks
097700081006     c                   Movel     TBLuni        DStb
097800081006      * imponibile a zero
097900081006    3c                   If        §tbrbl  = 'N'
098000081006     c                   add       1             z
098100081006     c                   movel     TBLkey        tbo_oK(z)
098200081006    3c                   endif
098300081006      **
098400081006    2c                   endif
098500081006    1c                   enddo
098600081006
098700060519      **
098800060519     c                   Movel     '3A'          Cod
098900060519     c                   z-add     0             K
099000060519     c                   z-add     0             KK
099100060519     c     Ktab          setll     tabel00f
099200060519     c                   do        *hival
099300060519     c     Ktab          reade     tabel00f
099400060519      * fine file
099500060519     c                   If        %eof(tabel00f)
099600060519     c                   leave
099700060519     c                   endif
099800060519    1
099900060519    2c                   If        tblkey <> *blanks
100000060519     c                   Movel     TBLuni        DS3a
100100060519      * imponibile a zero
100200060519    3c                   If        §3aim0  = 'S'
100300060519     c                   add       1             K
100400060519     c                   movel     TBLkey        im0(k)
100500060519    3c                   endif
100600060519      * bolla con singola varia
100700060519     c                   If        §3asva  <> *blanks
100800060519     c                   add       1             Kk
100900060519     c                   movel     tblkey        cbo(kk)
101000060519     c                   movel     §3asva        cbv(kk)
101100060519    3c                   endif
101200081006      * bolla con tipo bolla normale
101300081006     c     §3atb1        lookup    tbo_ok                                 30
101400081006     c                   if        %found
101500081006     c                   add       1             zz
101600081006     c                   movel     tblkey        cbo_ok(zz)
101700081006     c                   endif
101800060519      **
101900060519    2c                   endif
102000060519    1c                   enddo
102100060519
102200060519     c                   Endsr
102300141217
102400141217      /free
102500141217       //--------------------------------------------------------------
102600141217       //?Carico particolarità consegna con Pin Code.
102700141217       //--------------------------------------------------------------
102800141217       BEGSR Car7RPinCode;
102900141217
103000141217         cod = '7R';
103100141217         clear kk;
103200141217         setll (Codut:cod) TABEL00F;
103300141217         reade (Codut:cod) TABEL00F;
103400141217         DOW  not %eof(TABEL00F);
103500141217           IF  TBLflg = *blanks;
103600141217             ds7R = TBLuni;
103700141217             IF  §7Rpincode = 'S';
103800141217               kk += 1;
103900141217               skGMA(kk) = TBLkey;
104000141217             ENDIF;
104100141217           ENDIF;
104200141217           reade (Codut:cod) TABEL00F;
104300141217         ENDDO;
104400141217
104500141217       ENDSR;
104600141217      /end-free
104700141217
104800081006      *---------------------------------------------------------------*
104900081006      * Caricamento scaglioni
105000081006      *---------------------------------------------------------------*
105100081006     c     Car_scaglioni BEGSR
105200081006
105300081006     c                   eval      savprg = 999
105400081006     c                   clear                   yy
105500081006     c                   clear                   tipo_tar
105600081006     c                   clear                   num_sgl
105700081006      * cerco il giusto progressivo
105800081006
105900081006     c     ktam          setll     tntam01l
106000081006     c                   do        *hival
106100081006     c     ktam          reade     tntam01l
106200081006     c                   if        %eof(tntam01l)
106300081006     c                   leave
106400081006     c                   endif
106500081006      * verifico se progressivo annullato
106600081006     c                   if        tamatb <> ' '
106700081006     c                   iter
106800081006     c                   endif
106900081006      * controllo con la data di riferimento
107000120216     c                   If        rftdspr  < tamdst and rftdspr >= tamddt
107100081006     c                   eval      savprg = tamprg
107200081006     c                   leave
107300081006     c                   endif
107400081006     c                   if        rftdspr > tamdst
107500081006     c                   eval      savprg = tamprg
107600081006     c                   iter
107700081006     c                   endif
107800081006     c                   enddo
107900081006
108000081006     c                   if        savprg <> 999
108100081006      * trovato progressivo carico gli scaglioni relativi leggendo da titad
108200081006     c     ktad          setll     titad04l
108300110713     c                   do        *hival
108400081006     c     ktad          reade     titad04l
108500110713     c                   if        %eof(titad04l)
108600110713     c                   leave
108700110713     c                   endif
108800110713     c                   if        tadatb = ' '
108900081006      * recupero la chiave
109000081006     c                   eval      savlnp = tadlnp
109100081006     c                   eval      savorp = tadorp
109200081006     c                   eval      savnaz = tadnaz
109300081006     c                   eval      savcap = tadcap
109400110713     c                   leave
109500110713     c                   endif
109600110713     c                   enddo
109700081006
109800081006      * leggo tutti gli scaglioni con la stessa chiave
109900081006     c     ktad2         setll     titad04l
110000081006
110100081006     c                   do        *hival
110200081006     c     ktad2         reade     titad04l
110300081006
110400081006     c                   if        %eof(titad04l)
110500081006     c                   leave
110600081006     c                   endif
110700081006
110800081006     c                   if        tadatb <> ' '
110900081006     c                   iter
111000081006     c                   endif
111100081006
111200081006     c     tadsgl        lookup    sgl                                    31
111300081006
111400081006     c                   if        %found
111500081006     c                   leave
111600081006     c                   endif
111700081006
111800081006     c                   add       1             yy
111900081006     c                   eval      sgl(yy) = tadsgl
112000081006     c                   enddo
112100081006      * valorizzo il numero totali di scaglioni
112200081006     c                   z-add     yy            num_sgl
112300081006
112400081006     c                   movel     rftctrr       tipo_tar
112500081006
112600081006     c                   endif
112700081006      *
112800081006     c                   endsr
112900081006      *---------------------------------------------------------------*
113000081006      *  Ricerca dello scaglione per spedizione tassata
113100081006      *---------------------------------------------------------------*
113200081006     c     Ric_sgl       Begsr
113300081006
113400081006     c                   z-add     1             yy
113500081006
113600081006     c                   dow       yy <= num_sgl
113700081006     c                   if        misura <= sgl(yy)
113800081006     c                   eval      ok_scagl = *on
113900081006     c                   leave
114000081006     c                   endif
114100081006     c                   add       1             yy
114200081006     c                   enddo
114300081006
114400081006     c                   endsr
114500120921
114600120921      /free
114700120921       //--------------------------------------------------------------
114800120921       //?Memorizzo in SK il trasporto e le varie.
114900120921       //--------------------------------------------------------------
115000120921       BEGSR  Mem_Varie;
115100120921
115200120921       //?Il porto sempre al primo posto
115300120921         sk_Voce(1) = '.';
115400120921         IF  RFTporf > *zero;
115500120921           sk_ImpoF(1) += RFTporF;
115600120921           sk_ImpoR(1) += RFTporR;
115700120921         ENDIF;
115800120921
115900120921       //?Poi le varie
116000120924         IF  RFTva1F > *zero;
116100120921           Wvaria = RFTsv1F;
116200120921           WimpoF = RFTva1F;
116300120924           exsr  TotVariaF;
116400120924         ENDIF;
116500120924         IF  RFTva1R > *zero;
116600120924           Wvaria = RFTsv1R;
116700120921           WimpoR = RFTva1R;
116800120924           exsr  TotVariaR;
116900120921         ENDIF;
117000120921
117100120921         IF  RFTva2F > *zero;
117200120921           Wvaria = RFTsv2F;
117300120921           WimpoF = RFTva2F;
117400120924           exsr  TotVariaF;
117500120921         ENDIF;
117600120924         IF  RFTva2R > *zero;
117700120924           Wvaria = RFTsv2R;
117800120924           WimpoR = RFTva2R;
117900120924           exsr  TotVariaR;
118000120924         ENDIF;
118100120921
118200120921         IF  RFTva3F > *zero;
118300120921           Wvaria = RFTsv3F;
118400120921           WimpoF = RFTva3F;
118500120924           exsr  TotVariaF;
118600120921         ENDIF;
118700120924         IF  RFTva3R > *zero;
118800120924           Wvaria = RFTsv3R;
118900120924           WimpoR = RFTva3R;
119000120924           exsr  TotVariaR;
119100120924         ENDIF;
119200120921
119300120921         IF  RFTva4F > *zero;
119400120921           Wvaria = RFTsv4F;
119500120921           WimpoF = RFTva4F;
119600120924           exsr  TotVariaF;
119700120921         ENDIF;
119800120924         IF  RFTva4R > *zero;
119900120924           Wvaria = RFTsv4R;
120000120924           WimpoR = RFTva4R;
120100120924           exsr  TotVariaR;
120200120924         ENDIF;
120300120921
120400120921         IF  RFTva5F > *zero;
120500120921           Wvaria = RFTsv5F;
120600120921           WimpoF = RFTva5F;
120700120924           exsr  TotVariaF;
120800120921         ENDIF;
120900120924         IF  RFTva5R > *zero;
121000120924           Wvaria = RFTsv5R;
121100120924           WimpoR = RFTva5R;
121200120924           exsr  TotVariaR;
121300120924         ENDIF;
121400120921
121500120921         IF  RFTva6F > *zero;
121600120921           Wvaria = RFTsv6F;
121700120921           WimpoF = RFTva6F;
121800120924           exsr  TotVariaF;
121900120921         ENDIF;
122000120924         IF  RFTva6R > *zero;
122100120924           Wvaria = RFTsv6R;
122200120924           WimpoR = RFTva6R;
122300120924           exsr  TotVariaR;
122400120924         ENDIF;
122500120921
122600120921         IF  RFTva7F > *zero;
122700120921           Wvaria = RFTsv7F;
122800120921           WimpoF = RFTva7F;
122900120924           exsr  TotVariaF;
123000120921         ENDIF;
123100120924         IF  RFTva7R > *zero;
123200120924           Wvaria = RFTsv7R;
123300120924           WimpoR = RFTva7R;
123400120924           exsr  TotVariaR;
123500120924         ENDIF;
123600120921
123700120921         IF  RFTva8F > *zero;
123800120921           Wvaria = RFTsv8F;
123900120921           WimpoF = RFTva8F;
124000120924           exsr  TotVariaF;
124100120921         ENDIF;
124200120924         IF  RFTva8R > *zero;
124300120924           Wvaria = RFTsv8R;
124400120924           WimpoR = RFTva8R;
124500120924           exsr  TotVariaR;
124600120924         ENDIF;
124700120921
124800120921         IF  RFTva9F > *zero;
124900120921           Wvaria = RFTsv9F;
125000120921           WimpoF = RFTva9F;
125100120924           exsr  TotVariaF;
125200120921         ENDIF;
125300120924         IF  RFTva9R > *zero;
125400120924           Wvaria = RFTsv9R;
125500120924           WimpoR = RFTva9R;
125600120924           exsr  TotVariaR;
125700120924         ENDIF;
125800120921
125900120921         IF  RFTv10F > *zero;
126000120921           Wvaria = RFTs10F;
126100120921           WimpoF = RFTv10F;
126200120924           exsr  TotVariaF;
126300120921         ENDIF;
126400120924         IF  RFTv10R > *zero;
126500120924           Wvaria = RFTs10R;
126600120924           WimpoR = RFTv10R;
126700120924           exsr  TotVariaR;
126800120924         ENDIF;
126900120921
127000120921         IF  RFTv11F > *zero;
127100120921           Wvaria = RFTs11F;
127200120921           WimpoF = RFTv11F;
127300120924           exsr  TotVariaF;
127400120921         ENDIF;
127500120924         IF  RFTv11R > *zero;
127600120924           Wvaria = RFTs11R;
127700120924           WimpoR = RFTv11R;
127800120924           exsr  TotVariaR;
127900120924         ENDIF;
128000120921
128100120921         IF  RFTv12F > *zero;
128200120921           Wvaria = RFTs12F;
128300120921           WimpoF = RFTv12F;
128400120924           exsr  TotVariaF;
128500120921         ENDIF;
128600120924         IF  RFTv12R > *zero;
128700120924           Wvaria = RFTs12R;
128800120924           WimpoR = RFTv12R;
128900120924           exsr  TotVariaR;
129000120924         ENDIF;
129100120921
129200120921         IF  RFTv13F > *zero;
129300120921           Wvaria = RFTs13F;
129400120921           WimpoF = RFTv13F;
129500120924           exsr  TotVariaF;
129600120921         ENDIF;
129700120924         IF  RFTv13R > *zero;
129800120924           Wvaria = RFTs13R;
129900120924           WimpoF = RFTv13R;
130000120924           exsr  TotVariaR;
130100120924         ENDIF;
130200120921
130300120921         IF  RFTv14F > *zero;
130400120921           Wvaria = RFTs14F;
130500120921           WimpoF = RFTv14F;
130600120924           exsr  TotVariaF;
130700120921         ENDIF;
130800120924         IF  RFTv14R > *zero;
130900120924           Wvaria = RFTs14R;
131000120924           WimpoR = RFTv14R;
131100120924           exsr  TotVariaR;
131200120924         ENDIF;
131300120921
131400120921         IF  RFTv15F > *zero;
131500120921           Wvaria = RFTs15F;
131600120921           WimpoF = RFTv15F;
131700120924           exsr  TotVariaF;
131800120921         ENDIF;
131900120924         IF  RFTv15R > *zero;
132000120924           Wvaria = RFTs15R;
132100120924           WimpoR = RFTv15R;
132200120924           exsr  TotVariaR;
132300120924         ENDIF;
132400120921
132500120921         IF  RFTv16F > *zero;
132600120921           Wvaria = RFTs16F;
132700120921           WimpoF = RFTv16F;
132800120924           exsr  TotVariaF;
132900120921         ENDIF;
133000120924         IF  RFTv16R > *zero;
133100120924           Wvaria = RFTs16R;
133200120924           WimpoR = RFTv16R;
133300120924           exsr  TotVariaR;
133400120924         ENDIF;
133500120921
133600120921         IF  RFTv17F > *zero;
133700120921           Wvaria = RFTs17F;
133800120921           WimpoF = RFTv17F;
133900120924           exsr  TotVariaF;
134000120921         ENDIF;
134100120924         IF  RFTv17R > *zero;
134200120924           Wvaria = RFTs17R;
134300120924           WimpoR = RFTv17R;
134400120924           exsr  TotVariaR;
134500120924         ENDIF;
134600120921
134700120921         IF  RFTv18F > *zero;
134800120921           Wvaria = RFTs18F;
134900120921           WimpoF = RFTv18F;
135000120924           exsr  TotVariaF;
135100120921         ENDIF;
135200120924         IF  RFTv18R > *zero;
135300120924           Wvaria = RFTs18R;
135400120924           WimpoR = RFTv18R;
135500120924           exsr  TotVariaR;
135600120924         ENDIF;
135700120921
135800120921         IF  RFTv19F > *zero;
135900120921           Wvaria = RFTs19F;
136000120921           WimpoF = RFTv19F;
136100120924           exsr  TotVariaF;
136200120921         ENDIF;
136300120924         IF  RFTv19R > *zero;
136400120924           Wvaria = RFTs19R;
136500120924           WimpoR = RFTv19R;
136600120924           exsr  TotVariaR;
136700120924         ENDIF;
136800120921
136900120921         IF  RFTv20F > *zero;
137000120921           Wvaria = RFTs20F;
137100120921           WimpoF = RFTv20F;
137200120924           exsr  TotVariaF;
137300120921         ENDIF;
137400120924         IF  RFTv20R > *zero;
137500120924           Wvaria = RFTs20R;
137600120924           WimpoR = RFTv20R;
137700120924           exsr  TotVariaR;
137800120924         ENDIF;
137900120921
138000120921       ENDSR;
138100120921
138200120921       //--------------------------------------------------------------
138300120924       //?Incremento totali fatturato per voce in schiera.                       ?
138400120921       //--------------------------------------------------------------
138500120924       BEGSR  TotVariaF;
138600120921
138700120921         ww = %lookup( Wvaria : sk_Voce : 2 );
138800120921         IF  ww = *zero;
138900120921           ww = %lookup( *blank : sk_Voce : 2 );
139000120921           IF  ww > *zero;
139100120921             sk_Voce(ww) = Wvaria;
139200120921           ENDIF;
139300120921         ENDIF;
139400120921
139500120921         sk_ImpoF(ww) += WimpoF;
139600120921
139700120921       ENDSR;
139800120924
139900120924       //--------------------------------------------------------------
140000120924       //?Incremento totali per voce rifatturato in schiera.                       ?
140100120924       //--------------------------------------------------------------
140200120924       BEGSR  TotVariaR;
140300120924
140400120924         ww = %lookup( Wvaria : sk_Voce : 2 );
140500120924         IF  ww = *zero;
140600120924           ww = %lookup( *blank : sk_Voce : 2 );
140700120924           IF  ww > *zero;
140800120924             sk_Voce(ww) = Wvaria;
140900120924           ENDIF;
141000120924         ENDIF;
141100120924
141200120924         sk_ImpoR(ww) += WimpoR;
141300120924
141400120924       ENDSR;
141500120924
141600120924      /end-free
141700081006      *
141800060519      *---------------------------------------------------------------*
141900060519      * Operazioni Iniziali                                           *
142000060519      *---------------------------------------------------------------*
142100060519     c     RoutInz       BEGSR
142200060519      *
142300060519      * Reperisco dati job
142400060519     c                   exsr      DatiJob
142500060519      * Reperisco data e ora
142600060519     c                   time                    w0140
142700060519     c                   movel     w0140         wora
142800060522     c                   move      w0140         wdate
142900060519      *
143000060801      * verifico se sono in filiale faccio le ovrdbf per i file di sede
143100060801     c                   If        dutlpo <> 'S'
143200060801      * librerie
143300060801
143400060801      * Se sono in ambiente buono - GAITRAGRU
143500060801     c                   If        knsif = 'FILTRA201 '
143600060801     c                   Eval      wlib = 'GAITRAGRU '
143700060801     c                   Eval      wlib1 = 'GAITRAAZM '
143800060801      *  Se sono in ambiente di prova - GAITRAGRPS
143900060801     c                   Else
144000060801     c                   Eval      wlib = 'GAITRAGRPS'
144100060801     c                   Eval      wlib1 = 'GAITRAAZM '
144200060801     c                   EndIf
144300060801      * WFRFT01L
144400060801     c                   Clear                   comman
144500060801     c                   Movel(p)  cmdt(1)       comman
144600060801     c                   Eval      %Subst(comman:30:10) = wlib1
144700060801     c                   Eval      comman =
144800060801     c                             %trim(comman) + '/WFRFT01L)'
144900060801     c                   Call      'QCMDEXC'                            99
145000060801     c                   Parm                    comman
145100060801     c                   Parm                    lenght
145200060801
145300060801      * TITAI01L
145400060801     c                   Clear                   comman
145500060801     c                   Movel(p)  cmdt(2)       comman
145600060801     c                   Eval      %Subst(comman:30:10) = wlib
145700060801     c                   Eval      comman =
145800060801     c                             %trim(comman) + '/TITAI01L)'
145900060801     c                   Call      'QCMDEXC'                            99
146000060801     c                   Parm                    comman
146100060801     c                   Parm                    lenght
146200060801
146300060801      * TITAS30C
146400060801     c                   Clear                   comman
146500060801     c                   Movel(p)  cmdt(3)       comman
146600060801     c                   Eval      %Subst(comman:30:10) = wlib
146700060801     c                   Eval      comman =
146800060801     c                             %trim(comman) + '/TITAS30C)'
146900060801     c                   Call      'QCMDEXC'                            99
147000060801     c                   Parm                    comman
147100060801     c                   Parm                    lenght
147200060801      * FIAR531C
147300060801     c                   Clear                   comman
147400060801     c                   Movel(p)  cmdt(4)       comman
147500060801     c                   Eval      %Subst(comman:30:10) = wlib
147600060801     c                   Eval      comman =
147700060801     c                             %trim(comman) + '/FIAR531C)'
147800060801     c                   Call      'QCMDEXC'                            99
147900060801     c                   Parm                    comman
148000060801     c                   Parm                    lenght
148100060801      * TNCSB03L
148200060801     c                   Clear                   comman
148300060801     c                   Movel(p)  cmdt(5)       comman
148400060801     c                   Eval      %Subst(comman:30:10) = wlib
148500060801     c                   Eval      comman =
148600060801     c                             %trim(comman) + '/TNCSB03L)'
148700060801     c                   Call      'QCMDEXC'                            99
148800060801     c                   Parm                    comman
148900060801     c                   Parm                    lenght
149000060801
149100060801     c                   endif
149200060801      *
149300060801     c                   open      wfrft01l
149400060801     c                   open      titai01l
149500060801     c                   open      titas30c
149600060801     c                   open      fiar531c
149700060801     c                   open      tncsb03l
149800060801
149900060519     c                   ENDSR
150000060519      *
150100060519      *---------------------------------------------------------------*
150200060519      * Reperimento Dati del job (Utente/Operativi)                   *
150300060519      *---------------------------------------------------------------*
150400060519     c     DatiJob       BEGSR
150500060519      *
150600060519     c     *dtaara       define    §azute        azuteds
150700060519     c     *dtaara       define    §datiute      ddatiute
150800060519      *
150900060519     c                   in(E)     *dtaara
151000060519     c                   IF        %ERROR or RSUT = *blanks
151100060519     c                   clear                   Tibs34Ds
151200060519     c                   call      'TIBS34R'
151300060519     c                   parm                    Tibs34Ds
151400060519     c                   in        *dtaara
151500060519     c                   Endif
151600060519
151700060519      *
151800060519     c                   ENDSR
151900060801** cmdt
152000060801OVRDBF FILE(WFRFT01L) TOFILE(
152100060801OVRDBF FILE(TITAI01L) TOFILE(
152200060801OVRDBF FILE(TITAS30C) TOFILE(
152300060801OVRDBF FILE(FIAR531C) TOFILE(
152400060801OVRDBF FILE(TNCSB03L) TOFILE(
152500060801DLTOVR FILE(*ALL)
