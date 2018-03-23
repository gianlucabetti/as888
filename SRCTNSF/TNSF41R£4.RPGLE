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
008500060522      * recupero bancali e colli originali
008600060522     d dAr5Ban       e ds
008700060522
008800060522     d dAr5Bnb       e ds
008900120921
009000120921       // -?Tabella "CC" = Codici Tassazione?
009100120921     d dsCC          e ds                  inz
009200141217
009300141217       //?Ds tabella 7R - part.consegna
009400141217     d ds7R          e ds
009500141217
009600120921
009700060518      * - Ds per scrittura file di lavoro
009800060518     d                 ds
009900060518     d  RFTsv1r                1      1
010000060518     d  RFTsv2r                2      2
010100060518     d  RFTsv3r                3      3
010200060518     d  RFTsv4r                4      4
010300060518     d  RFTsv5r                5      5
010400060518     d  RFTsv6r                6      6
010500060518     d  RFTsv7r                7      7
010600060518     d  RFTsv8r                8      8
010700060518     d  RFTsv9r                9      9
010800060518     d  RFTs10r               10     10
010900060518     d  RFTs11r               11     11
011000060518     d  RFTs12r               12     12
011100060518     d  RFTs13r               13     13
011200060518     d  RFTs14r               14     14
011300060518     d  RFTs15r               15     15
011400060518     d  RFTs16r               16     16
011500060518     d  RFTs17r               17     17
011600060518     d  RFTs18r               18     18
011700060518     d  RFTs19r               19     19
011800060518     d  RFTs20r               20     20
011900060518     d  RFT_svr                1     20    Dim(20)                              Sigle varie
012000060512
012100060518      * - Ds per scrittura file di lavoro
012200060518     d                 ds
012300060518     d  RFTva1r                1     11  3
012400060518     d  RFTva2r               12     22  3
012500060518     d  RFTva3r               23     33  3
012600060518     d  RFTva4r               34     44  3
012700060518     d  RFTva5r               45     55  3
012800060518     d  RFTva6r               56     66  3
012900060518     d  RFTva7r               67     77  3
013000060518     d  RFTva8r               78     88  3
013100060518     d  RFTva9r               89     99  3
013200060518     d  RFTv10r              100    110  3
013300060518     d  RFTv11r              111    121  3
013400060518     d  RFTv12r              122    132  3
013500060518     d  RFTv13r              133    143  3
013600060518     d  RFTv14r              144    154  3
013700060518     d  RFTv15r              155    165  3
013800060518     d  RFTv16r              166    176  3
013900060518     d  RFTv17r              177    187  3
014000060518     d  RFTv18r              188    198  3
014100060518     d  RFTv19r              199    209  3
014200060518     d  RFTv20r              210    220  3
014300060518     d  RFT_var                1    220  3 Dim(20)                              Importo varie
014400060504      *
014500060504      * - Controllo/Inversione date
014600060504     d WLBdat          ds                  inz
014700060504     d   G08dat                       8  0 inz
014800060504     d   G08inv                       8  0 inz
014900060504     d   G08err                       1    inz('3')
015000060504     d   G08tgi                       5  0 inz
015100060504      *
015200060504      * - Variabili definite a programma
015300060519     d codut           s              1  0 inz(1)
015400060504     d Wdate           s              8  0 inz
015500060518     d Wora            s              6  0 inz
015600060516     d W0140           s             14  0 inz
015700081006     d z               s              4  0
015800081006     d zz              s              4  0
015900060518     d k               s              4  0
016000060519     d kk              s              4  0
016100060522     d xx              s              4  0
016200081006     d yy              s              4  0
016300120921     d ww              s              4  0
016400081006     d num_sgl         s              4  0
016500060522     d kscfil          s              3  0
016600081006     d tipo_tar        s              1  0
016700081006     d ok_scagl        s               n
016800060522     d cod             s                   like(tblcod)
016900081006     d key             s                   like(tblkey)
017000060522     d kAr5Trd         s                   Like(Ar5Trd)
017100060522     d antepo          s                   like(tasimv)
017200060522     d W_tasdiv        s                   like(tasdiv)
017300060522     d W_dtas          s                   like(dtas)
017400060522     d W_dtasv         s                   like(dtasv)
017500060522     D Wcev            s                   like(evbcev) inz('RIC')
017600081006     d prg2            s                   like(tamprg) inz
017700081006     d ctr2            s                   like(tamctr) inz
017800081006     d ksc2            s                   like(tamksc) inz
017900081006     d savprg          s                   like(tamprg)
018000081006     d savlnp          s                   like(tadlnp)
018100081006     d savorp          s                   like(tadorp)
018200081006     d savnaz          s                   like(tadnaz)
018300081006     d savcap          s                   like(tadcap)
018400081006     d misura          s                   like(tadsgl)
018500060801
018600060801     d wlib            s             10
018700060801     d wlib1           s             10
018800060801     d comman          s             80
018900060801     d lenght          s             15  5 inz(80)
019000060801
019100120921       // -?Campi di comodo?
019200120921     d Wconta          s              1  0 inz
019300120921     d Wvaria          s                   like(TASsv1)   inz
019400120921     d WimpoF          s                   like(TASva1)   inz
019500120921     d WimpoR          s                   like(TASva1)   inz
019600060522
019700060504      *
019800060504      * Key-List - - - - - - - - - - - - - - - - - - - - - - - - - - -*
019900060504      *
020000060518      * - WFRFT01L
020100060518     c     KRFT          Klist
020200060518     c                   Kfld                    I40noj
020300060518     c                   Kfld                    I40pru
020400060518     c                   Kfld                    I40dta
020500060518     c                   Kfld                    I40ora
020600060517
020700060518      * - TITAI01L / TITAS30C
020800060518     c     KSPE          Klist
020900060518     c                   Kfld                    RFTaas
021000060518     c                   Kfld                    RFTlnp
021100060518     c                   Kfld                    RFTnrs
021200060518     c                   Kfld                    RFTnsp
021300060518     c                   Kfld                    RFTtbl
021400060519      * Tncsb03l
021500060519     c     KCSB          Klist
021600060519     c                   Kfld                    TASaas
021700060519     c                   Kfld                    TASlnp
021800060519     c                   Kfld                    TASnrs
021900060519     c                   Kfld                    TASnsp
022000060519      * Tabel00f
022100060519     c     KTAB          Klist
022200060519     c                   Kfld                    codut
022300060519     c                   Kfld                    cod
022400081006     c     KTABkey       Klist
022500081006     c                   Kfld                    codut
022600081006     c                   Kfld                    cod
022700081006     c                   Kfld                    key
022800060522      * Fiar531c
022900060522     c     kFiar5        Klist
023000060522     c                   Kfld                    TasAas
023100060522     c                   Kfld                    TasLnp
023200060522     c                   Kfld                    TasNrs
023300060522     c                   Kfld                    TasNsp
023400060522     c                   Kfld                    kAr5Trd
023500060522      * Chiave file eventi FNEVB
023600060522     c     Kevb4         Klist
023700060522     c                   Kfld                    Tasaas
023800060522     c                   Kfld                    Taslnp
023900060522     c                   Kfld                    Tasnrs
024000060522     c                   Kfld                    Tasnsp
024100060522     c                   Kfld                    Wcev
024200081006      ** accesso tntam x cliente
024300081006     c     Ktam          Klist
024400081006     c                   Kfld                    rftkscr
024500081006     c                   Kfld                    rftctrr
024600081006      ** accesso titad
024700081006     c     Ktad          Klist
024800081006     c                   Kfld                    tamksc
024900081006     c                   Kfld                    tamctr
025000081006     c                   Kfld                    tamprg
025100081006
025200081006     c     Ktad2         Klist
025300081006     c                   Kfld                    tamksc
025400081006     c                   Kfld                    tamctr
025500081006     c                   Kfld                    tamprg
025600081006     c                   Kfld                    savlnp
025700081006     c                   Kfld                    savorp
025800081006     c                   Kfld                    savnaz
025900081006     c                   Kfld                    savcap
026000060504
026100060504      *---------------------------------------------------------------*
026200060504      *  RIEPILOGO INDICATORI                                         *
026300060504      *---------------------------------------------------------------*
026400060727      * 05/30 - Comodo
026500060727      * 50    - Data fattura antecedente 30-04-06 stampo scritta
026600060727      * 51    - Comodo
026700060727      * 90    - OF
026800060727      * 99    - Comodo
026900060504      *---------------------------------------------------------------*
027000060504
027100060504     c     *Entry        plist
027200060504     c                   parm                    KPJBA
027300060518     c                   movel     kpjbu         Tnsf40ds
027400060504      *
027500060504      * Operazioni Iniziali
027600060504     c                   exsr      RoutInz
027700060504      *
027800060523     c                   exsr      Carcbo
027900141217      /free
028000141217       //?Rielaboro quello rimasto nel file in QTEMP
028100141217         exsr Car7RPinCode;
028200141217      /end-free
028300060523      *
028400060518      * Elaborazione del file
028500060518     c
028600060518do  1c                   exsr      Elabora
028700060504      * 1° videata
028800060518     c                   exsr      Stampa
028900060504      * Fine
029000060509      *
029100060512     c     fine          tag
029200060801
029300060801     c                   If        Dutlpo <> 'S'
029400060801     c                   Clear                   comman
029500060801     c                   Movel(p)  cmdt(6)       comman
029600060801     c                   Call      'QCMDEXC'                            99
029700060801     c                   Parm                    comman
029800060801     c                   Parm                    lenght
029900060801     c                   endif
030000060801
030100060504     c                   movel     *on           *inLR
030200060504      *
030300060504      *
030400060504      *---------------------------------------------------------------*
030500060518      * Elabora ritassazione                                          *
030600060504      *---------------------------------------------------------------*
030700060518     c     Elabora       BEGSR
030800060504      *
030900060518e   1c     Krft          setll     wfrft01l
031000060504
031100060504     c                   do        *hival
031200060522      *
031300060518     c     krft          reade     wfrft01l
031400060518
031500060522     c                   If        %eof(Wfrft01l)
031600060518     c                   leave
031700060518     c                   endif
031800060518      * verifico se record già elaborato
031900060518     c                   if        rftela <> ' '
032000060518     c                   iter
032100060518     c                   endif
032200060726      * imposto i campi della testata
032300060726     c                   if        prtfiv = *zeros
032400060726     c                   eval      prtfiv = rftfiv
032500060726     c                   eval      prtnft = rftnft
032600060726      *
032700060726     c                   clear                   WLBdat
032800060726     c                   eval      G08inv = rftdft
032900060726     c                   eval      G08err = '3'
033000060726     c                   call      'XSRDA8'
033100060726     c                   parm                    WLBdat
033200060726     c                   z-add     G08dat        prtdft
033300081006      * se si desidera tassazione scaglionata recupero gli scaglioni della tariffa
033400081006     c                   If        i40sgl = 'S'
033500081006     c                   exsr      car_scaglioni
033600081006     c                   endif
033700081006
033800060726     c                   endif
033900060727      * controllo data fattura
034000060727     c                   eval      *in50 = rftdft < 20060430
034100060518      * tasso
034200060518     c                   exsr      tassaz
034300060504      *
034400060504     c                   enddo
034500060504      *
034600060504     c                   ENDSR
034700060504      *
034800060522      *---------------------------------------------------------------*
034900060522      * Stampa Finale
035000060522      *---------------------------------------------------------------*
035100060522     c     Stampa        BEGSR
035200060522
035300060522      * testata
035400060530     c                   write     sf41t0
035500060522     c                   write     sf41ts
035600060522     c                   write     sf41t3
035700060522     c                   write     sf41t2
035800060522     c                   write     sf41t3
035900060522      * valorizzo i campi
036000060522     c                   eval      prtnoj = knmeb
036100060522     c                   eval      prtpru = rftpru
036200060522      *
036300060522     c                   clear                   WLBdat
036400060522     c                   eval      G08inv = rftdta
036500060522     c                   eval      G08err = '3'
036600060522     c                   call      'XSRDA8'
036700060522     c                   parm                    WLBdat
036800060522     c                   z-add     G08dat        prtdta
036900060522      *
037000060522     c                   eval      prtora = rftora
037100060614      * calcolo la differente
037200060614     c                   eval      prtdiffe = prtimvf - prtimvr
037300060522      *
037400060522     c                   write     sf41d3
037500060522      *
037600081006      * stampo la suddivisione per scaglione solo se richiesta
037700081006     c                   If        i40sgl = 'S'
037800081006     c                   eval      cod = 'TR'
037900081006     c                   movel(p)  tipo_tar      key
038000081006     c     ktabkey       chain     Tabel00f
038100081006     c                   if        %found(tabel00f)
038200081006     c                   movel     tbluni        dstr
038300081006      * valorizzo i campi descrittivi di stampa
038400081006     c                   movel     §trdst        prttar
038500081006     c                   endif
038600081006      * testata
038700081006     c                   write     sf41t04
038800081006     c                   write     sf41t3
038900081006     c                   write     sf41t4
039000081006     c                   write     sf41t3
039100081006
039200081006      * stampo dettaglio per ogni scaglione
039300081006     c                   z-add     1             yy
039400081006     c                   dow       yy <= num_sgl
039500081006
039600081006     c                   eval      prtsca = sgl(yy)
039700081006     c                   eval      prtimf = imf(yy)
039800081006     c                   eval      prtimr = imr(yy)
039900081006     c                   eval      prtdif = (prtimf - prtimr)
040000081006     c                   eval      prttkg = tkg(yy)
040100081006     c                   eval      prttcl = tcl(yy)
040200081006     c                   eval      prttvl = tvl(yy)
040300081006     c                   eval      prtnsp = nsp(yy)
040400081006     c                   eval      prtnmt = nmt(yy)
040500081006
040600081006     c                   write     sf41d4
040700081006      * pulisco la descrizione della tariffa
040800081006     c                   clear                   prttar
040900081006
041000081006     c                   add       1             yy
041100081006
041200081006     c                   enddo
041300081006      *
041400081006      * stampo se ci sono delle bolle di recupero
041500081006     c                   If        prtnspr > 0
041600081006     c                   write     sf41d4r
041700081006     c                   endif
041800081006      *
041900081006     c                   endif
042000120921      /free
042100120921       //?Stampa la suddivisione per voce se richiesta
042200120921        IF  I40sva = 'S';
042300120921          exsr  Stampa_TOT;
042400120921        ENDIF;
042500120921      /end-free
042600081006      *
042700060522     c                   Endsr
042800120921
042900120921      /free
043000120921       //--------------------------------------------------------------
043100120921       //?Stampa suddivisione Voci.
043200120921       //--------------------------------------------------------------
043300120921       BEGSR  Stampa_TOT;
043400120921
043500120921         write  SF41T05;
043600120921         write  SF41T5;
043700120921
043800120921         ww = 1;
043900120921
044000120921       //?SE non ci fosse la voce trasporto (che è sempre alla?
044100120921       //?prima posizione): si inizia a stampare dalla 2ª Varia?
044200120921         IF  sk_ImpoF(1) = *zero;
044300120921           ww = 2;
044400120921         ENDIF;
044500120921
044600120921       //?Stampa delle varie di 2 in 2?
044700120921         DoW  sk_Voce(ww) <> *blank;
044800120921
044900120921           Wconta = 1;
045000120921           clear  SF41D5;
045100120921
045200120921           DoW  Wconta <= 2  and  sk_Voce(ww) <> *blank;
045300120921
045400120921             Wvaria = sk_Voce(ww);
045500120921             if  Wconta = 1;
045600120921               //?Nella prima posizione: Trasporto?
045700120921               if  ww = 1;
045800120921                 P1Dsv1 = 'TRASPORTO      ';
045900120921               else;
046000120921                 exsr  sr_TabCC;
046100120921                 P1Csv1 = sk_Voce(ww) + ' -';
046200120921                 P1Dsv1 = §CCdes;
046300120921               endif;
046400120921               P1Cva1F = sk_ImpoF(ww);
046500120921               P1Cva1R = sk_ImpoR(ww);
046600120921             else;
046700120921               exsr  sr_TabCC;
046800120921               P1Csv2  = sk_Voce(ww) + ' -';
046900120921               P1Dsv2  = §CCdes;
047000120921               P1Cva2F = sk_ImpoF(ww);
047100120921               P1Cva2R = sk_ImpoR(ww);
047200120921             endif;
047300120921
047400120921             ww += 1;
047500120921             Wconta += 1;
047600120921
047700120921           EndDo;
047800120921
047900120921           write  SF41D5;
048000120921
048100120921         EndDo;
048200120921
048300120921         //?Totale Imponibile?
048400120925         //P1CimvF = %xfoot( sk_ImpoF);
048500120925         //P1CimvR = %xfoot( sk_ImpoR);
048600120921
048700120921         //?Stampa Totali per Fattura?
048800120921         write  SF41G5;
048900120921
049000120921       ENDSR;
049100120921
049200120921       //--------------------------------------------------------------
049300120921       //?Decodifica Sigla Varia.                                      ?
049400120921       //--------------------------------------------------------------
049500120921       BEGSR  sr_TabCC;
049600120921
049700120921         clear  dsCC;
049800120921         cod = 'CC';
049900120921         key = 'VARIE  ' + Wvaria;
050000120921
050100120921         chain  (codut:cod:key) TABEL00F;
050200120921
050300120921         IF  %found(TABEL00F);
050400120921           dsCC = TBLuni;
050500120921         ELSE;
050600120921           §CCdes = *all'?';
050700120921         ENDIF;
050800120921
050900120921       ENDSR;
051000120921      /end-free
051100060522      *
051200060515      *---------------------------------------------------------------*
051300060518      *  Routine di tassazione
051400060515      *---------------------------------------------------------------*
051500060518     c     Tassaz        Begsr
051600060518      *
051700060518     c                   clear                   dtas
051800060518     c                   clear                   dtasv
051900060518      * aggancio titas
052000060518     c     KSPE          chain     titas30c
052100060522   a1c                   if        %found(titas30c)
052200070411      * Pulisco i campi dell'assicurazione se calcolata in fattura
052300070411    2c                   IF        Tasfcm = 'F'
052400070411     c                   clear                   tasias
052500070411     c                   clear                   tasvas
052600070411    2c                   Endif
052700070411
052800060530      * valorizzo i campi del file printer
052900060530      *
053000060530     c                   clear                   WLBdat
053100060530     c                   movel     tasaas        g08inv
053200060530     c                   move      tasmgs        g08inv
053300060530     c                   eval      G08err = '3'
053400060530     c                   call      'XSRDA8'
053500060530     c                   parm                    WLBdat
053600060530     c                   z-add     G08dat        prtdsp
053700060530
053800060530     c                   z-add     tasksc        prtksc
053900060530     c                   z-add     tasctr        prtctr
054000060530
054100060530     c                   clear                   WLBdat
054200060530     c                   movel     rftdspr       g08inv
054300060530     c                   eval      G08err = '3'
054400060530     c                   call      'XSRDA8'
054500060530     c                   parm                    WLBdat
054600060530     c                   z-add     G08dat        prtdspr
054700060530
054800060530      *
054900060519      * valorizzo la DS DTAS e DTASV
055000060519     c                   clear                   dtasv
055100060519     c                   clear                   taspor
055200060519     c                   clear                   tasimv
055300060518      *
055400060519      * cerco in TITAI l'immagine della spedizione prima della tassazione
055500060519     c                   exsr      RIC_tai
055600060519      * se tassata fino all'imponibile non tasso
055700060522    1c                   if        tasimv = 0
055800060519      ** Ricerca contrassegno
055900060519    2c                   IF        Tasfca <> *blanks
056000060519     c                   Exsr      Carcsb
056100060519    2c                   Endif
056200060519
056300060519      *  se si tratta di codice bolla che prevede solo una varia, passo solo
056400060519      *  quella
056500060519     c                   z-add     1             kk
056600060519     c     tascbo        lookup    cbo(kk)                                05
056700060522    2c                   if        *in05
056800060519     c                   eval      tassva=cBV(kk)
056900060522    2c                   endif
057000060522
057100060522     c                   movel     Tasftc        tastc1
057200060522     c                   Z-add     rftdft        tasdct
057300060726
057400060726     c                   eval      Tasksc = rftkscr
057500060726     c                   eval      Tasctr = rftctrr
057600061221     c                   movel     rftaas        tasdsp
057700061221     c                   move      rftmgs        tasdsp
057800060726
057900060522      * Flag SI NO DDT
058000060522    2C                   If        tasll1 = 'C' or tasll1 = 'S'
058100060522     C                   movel     'S'           Tasddt
058200060522   x2C                   Else
058300060522     C                   Clear                   Tasddt
058400060522    2C                   Endif
058500060926     c****               movel     tasfbr        dstasflo
058600060926     c                   clear                   dtas01
058700060926     c                   movel     tasfbr        §asfbr
058800060926     c                   movel     tascca        §ascca
058900110701      * valorizzo flag email al destinatario
059000110701     c                   movel     tasflo        dtasflo
059100110701     c                   move      §floemd       §asemd
059200141217      /free
059300141217       //?Imposto se part.consegna con PinCode
059400141217         IF  TASgma <> *blanks and %lookup (TASgma:skGMA) > 0;
059500141217           §ASpin = 'S';
059600141217         ENDIF;
059700141217      /end-free
059800110701
059900061221      * se data spedizione differente da ritassazione passo nel flo anche la data ritassazione
060000061221     c                   If        tasdsp <> rftdspr
060100070108     c                   move      rftdspr       §asdrt
060200061221     c                   endif
060300061221
060400060926     c                   eval      dstasflo = dtas01
060500060522
060600060522      * Bancali
060700060522    2c                   If        %Subst(TasGva:1:1) = 'B'
060800060522
060900060522     c                   eval      kAr5Trd  = 'BAN'
061000060522     c     kFiar5        Chain     Fiar531c
061100060522If  3c                   If        %Found(Fiar531c)
061200060522     c                   Movel     Ar5Uni        dAr5Ban
061300060522    3c                   EndIf
061400060522      * Bancali
061500060522     c                   Eval      TasBan = §Ar5Nba + §Ar5Nb2
061600060522    2c                   EndIf
061700060522      **
061800060522      * Colli Originali
061900060522    2c                   If        %Subst(TasGva:1:1) = 'O'
062000060522
062100060522     c                   eval      kAr5Trd  = 'BNB'
062200060522     c     kFiar5        Chain     Fiar531c
062300060522If  3c                   If        %Found(Fiar531c)
062400060522     c                   Movel     Ar5Uni        dAr5Bnb
062500060522    3c                   EndIf
062600060522      * Colli
062700060522     c                   Eval      TasNcl = §Ar5bcor
062800060522    2c                   EndIf
062900060522      *
063000060522      * verifico se è una bolla non tassata fino l'imponibile , una bolla di reso ,
063100060522      * con VARIA 'N' valorizzata con 888888 chiamo il TNSF20R per il calcolo dell'anteporto
063200060522      *
063300060522    2c                   if        tasimv = 0 and tasfbr = 'S'
063400060522     c                   z-add     1             xx
063500060522     c     'N'           lookup    sv(xx)                                 30
063600060522    3c                   if        *in30 and va(xx) =  88888888
063700060522      * tasso l'anteporto
063800060522     c                   exsr      Varian
063900060522    3c                   endif
064000060522      *
064100060522    2c                   endif
064200060522      *
064300060522      * se ritorno dal calcolo della varia N e c'è errore non proseguo
064400060522      *
064500060522     c                   Setoff                                       05
064600060522    2c                   if        taserr = ' '
064700060522      * Se devo tassare per una varia e c'e' gia', non richiamo la tassazione  se importo
064800060522      * della varia uguale  a zero
064900060522    3c                   if        tassva<>*blanks and tasimv=0
065000060522     c                   z-add     1             kk
065100060522     c     tassva        lookup    sv(kk)                                 05
065200060522    4c                   if        *in05 and va(kk) > 0
065300060522     c                   setoff                                       05
065400060522    4c                   endif
065500060522    3c                   endif
065600060522      * verifico se esiste la varia & con importo uguale a 8888888 e se si passo al
065700060522      * TNSF20R il flag di calcolo varia &
065800060522      *
065900060522     c                   setoff                                       51
066000060522     c                   z-add     1             kk
066100060522     c     '&'           lookup    sv(kk)                                 51
066200060522    3c                   if        *in51 and va(kk) = 88888888
066300060522     c                   eval      tasfcaa = 'S'
066400060522     c                   clear                   sv(kk)
066500060522     c                   clear                   va(kk)
066600060522    3c                   endif
066700060522      * verifico se devo calcolare l'addebito di lasciato avviso
066800060522     c                   movel     rftkscr       kscfil
066900060522      *
067000060522      * verifico se c'è evento
067100060522     c     Kevb4         chain     Fnevb04l
067200060522
067300060522    4c                   If        %found(Fnevb04l)
067400060522     c                   eval      tasric = 'S'
067500060522    4c                   endif
067600060522
067700060522      **
067800060522      **
067900060522    3c                   if        tassva=*blanks or not *in05
068000060522     c                   call      'TNSF20R'
068100060522     c                   parm                    Kpjba
068200060522     c                   parm                    Dtas
068300060522     c                   parm                    Dtasv
068400060522    3c                   endif
068500060522      **
068600060522    2c                   endif
068700060522    1c                   endif
068800060519
068900060522     c                   If        taserr = ' '
069000060517      * tassazione
069100060518     c                   eval      RFTdivr = tasdiv
069200060518     c                   eval      RFTporr = taspor
069300060518      * carico eventuali varie presenti in TITA7
069400060518     c                   movea     sv            rft_svr
069500060518     c                   movea     va            rft_var
069600060518      *
069700060518     c                   eval      RFTimvr = tasimv
069800060522     c                   endif
069900060522   a1c                   endif
070000060522
070100060518     c                   eval      RFTela  = 'S'
070200060522     c                   eval      RFTnoj  = Knmeb
070300060517
070400060518     c                   UPDATE    WFRFT000
070500060518
070600060522      * faccio i totali imponibili e conto le spedizioni manca tariffa
070700060522
070800060522     c                   If        taserr <> ' '
070900060522     c                   add       1             prtnrmt
071000060522     c                   endif
071100060522
071200060522     c                   add       RFTimvf       prtimvf
071300060522     c                   add       RFTimvr       prtimvr
071400060518
071500060530      * differenza
071600060530     c                   eval      diffe = rftimvf - rftimvr
071700120921
071800120921      /free
071900120921       //?Metto in SK il trasporto + le varie
072000120921         exsr  Mem_Varie;
072100120921      /end-free
072200060530
072300060530      * STAMPO
072400060530     c                   If        not *in99 or *in90
072500060530      * testata
072600060530      * testata
072700060530     c                   write     sf41t0
072800060530     c                   write     sf41ts
072900060530     c                   write     sf41t3
073000060530     c                   write     sf41t1
073100060530     c                   write     sf41t3
073200060530
073300060530     c                   seton                                        99
073400060530     c                   setoff                                       90
073500060530
073600060530     c                   endif
073700060530
073800060530     c                   write     sf41d1
073900081006
074000081006      * suddivido i valori per scaglioni di !!! in base alla tipologia della tariffa
074100081006
074200081006     c                   If        i40sgl = 'S'
074300081006      * se tipo bolla fa parte delle bolle non di recupero le sommo scaglionate
074400081006      * altrimenti le metto nelle spedizioni di recupero
074500081006     c     tascbo        lookup    cbo_ok                                 30
074600081006     c                   if        %found
074700081006
074800081006     c                   z-add     1             yy
074900081006     c                   eval      ok_scagl = *off
075000081006     c                   select
075100081006      * quantità / valore
075200081006     c                   when      tipo_tar = 5 or tipo_tar = 4
075300081006     c                   z-add     tasqft        misura
075400081006     c                   exsr      ric_sgl
075500081006      * peso
075600081006     c                   when      tipo_tar = 0 or tipo_tar = 3
075700081006     c                   z-add     taspkf        misura
075800081006     c                   exsr      ric_sgl
075900081006      * colli
076000081006     c                   when      tipo_tar = 1
076100081006     c                   z-add     tasncl        misura
076200081006     c                   exsr      ric_sgl
076300081006      * spedizione
076400100302     c                   when      tipo_tar = 2
076500081006     c                   z-add     1             misura
076600081006     c                   exsr      ric_sgl
076700081006     c                   endsl
076800081006
076900081006      * se scaglione trovato valorizzo i campi delle schiere
077000081006     c                   If        ok_scagl
077100081006      * imponibile vecchio
077200081006     c                   add       rftimvf       imf(yy)
077300081006      * imponibile nuovo
077400081006     c                   add       rftimvr       imr(yy)
077500081006      * peso
077600081006     c                   add       taspkf        tkg(yy)
077700081006      * colli
077800081006     c                   add       tasncl        tcl(yy)
077900081006      * volume
078000081006     c                   add       tasvlf        tvl(yy)
078100081006      * spedizioni
078200081006     c                   add       1             nsp(yy)
078300081006      * manca tariffa
078400081006     c                   if        taserr <> ' '
078500081006     c                   add       1             nmt(yy)
078600081006     c                   endif
078700081006
078800081006     c                   endif
078900081006
079000081006      *** se bolla di recupero
079100081006     c                   else
079200081006      * imponibile vecchio
079300081006     c                   add       rftimvf       prtimfr
079400081006      * imponibile nuovo
079500081006     c                   add       rftimvr       prtimrr
079600081006      * spedizioni
079700081006     c                   add       1             prtnspr
079800081006      * manca tariffa
079900081006     c                   if        taserr <> ' '
080000081006     c                   add       1             prtnmtr
080100081006     c                   endif
080200081006      ***
080300081006     c                   endif
080400081006
080500081006     c                   endif
080600081006
080700060530
080800060518     c                   Endsr
080900060515
081000060519      *
081100060519      *---------------------------------------------------------------*
081200060519      *  Recupero importi di tassazione dal file titai
081300060519      *---------------------------------------------------------------*
081400060519     c     Ric_tai       Begsr
081500060519      *
081600060519     c                   z-add     0             k
081700060519
081800060519     c     kspe          setll     titai01l
081900060519
082000060519     c                   If        %equal(titai01l)
082100060519
082200060519     c                   do        *hival
082300060519
082400060522     c     kspe          reade     titai01l
082500060519
082600060519     c                   If        %eof(titai01l)
082700060519     c                   leave
082800060519     c                   endif
082900060519      * imponibile
083000060519     c                   if        taisvt = '£'
083100060519     c                   eval      tasimv = taivat
083200060519     c                   endif
083300060519      * porto
083400060519     c                   if        taisvt = ' '
083500060519     c                   eval      taspor = taivat
083600060519     c                   endif
083700060519      * varie
083800060519     c                   if        taisvt <> ' ' and taisvt <> '£'
083900060519     c                   add       1             k
084000060519     c                   eval      sv(k) = taisvt
084100060519     c                   eval      va(k) = taivat
084200060519     c                   endif
084300060519
084400060519     c                   enddo
084500060519
084600060519     c                   else
084700060519      * se non è pretassata pulisco la divisa
084800060519     c                   clear                   tasdiv
084900060519     c                   endif
085000070426      * se tassata fino l'imponibile valorizzo il campo tasman del prtf e il campo rfttmaf del file
085100070426     c                   if        tasimv = 0
085200070426     c                   clear                   tasman
085300070426     c                   else
085400070502     c                   eval      tasman = 'S'
085500070426      * faccio il totale dei tassati manualmente
085600070426     c                   add       1             prtnrtm
085700070426     c                   endif
085800070426
085900070426     c                   eval      rfttmaf = tasman
086000060519
086100060519     c                   endsr
086200060519      *---------------------------------------------------------------*
086300060519      ** Riverca Contrassegno
086400060519      *---------------------------------------------------------------*
086500060519     c     Carcsb        Begsr
086600060519
086700060519     c     Kcsb          chain     Tncsb03l
086800060519     c                   If        %found(tncsb03l) and
086900060519      * solo per stesso tipo bolla
087000060519     c                             tastbl = csbtbl
087100060519     c                   movel     csbsta        Tassta
087200060519     c                   z-add     csbcas        Tascas
087300061016     c*****              movel     csbtpa        Tastic
087400060519     c                   if        csbfus <> *blanks
087500060519     c                   move      csbfus        tastic
087600060519     c                   else
087700060519     c                   move      csbtpi        tastic
087800060519     c                   end
087900060519      * mittente
088000060519     c                   movel     csbvca        tasvca
088100060519     c                   z-add     csbcmb        tascmb
088200060519
088300060519     c                   endif
088400060519
088500060519     c                   endsr
088600060522      *---------------------------------------------------------------*
088700060522      ** Calcolo anteporto VARIA 'N'
088800060522      *---------------------------------------------------------------*
088900060522     c     Varian        Begsr
089000060522
089100060522      * richiamo il programma di tassazione
089200060522      * senza passare le varie e il porto
089300060522      *
089400060522      * mi salvo le ds di tassazione
089500060522     c                   eval      w_dtas = dtas
089600060522     c                   eval      w_dtasv = dtasv
089700060522      * mi salvo la divisa
089800060522     c                   eval      w_tasdiv = tasdiv
089900060522
090000110701      * tolgo il flag dell'invio mail al destinatario nell'eventualità ci fosse
090100110701     c                   eval      dtas01 = dstasflo
090200110701     c                   clear                   §asemd
090300110701     c                   eval      dstasflo = dtas01
090400110701
090500060522     c                   clear                   dtasv
090600060522     c                   clear                   tasdiv
090700060522     c                   clear                   taspor
090800060522
090900060522     c                   clear                   antepo
091000060522
091100060522     c                   call      'TNSF20R'
091200060522     c                   parm                    kpjba
091300060522     c                   parm                    dtas
091400060522     c                   parm                    dtasv
091500060522      * se non ci sono errori valorizzo la varia 'N'
091600060522     c                   if        taserr = ' '
091700060522     c                   z-add     tasimv        antepo
091800060522     c                   endif
091900060522      * ripristino le ds salvate prima
092000060522     c                   eval      dtas = w_dtas
092100060522     c                   eval      dtasv = w_dtasv
092200060522      * valorizzo l'anteporto nella varia N al posto di 88888888  se maggiore di zero
092300060522     c                   if        antepo > 0
092400060522     c                   z-add     antepo        va(xx)
092500060522     c                   endif
092600060522
092700060522     c                   endsr
092800060519      *---------------------------------------------------------------*
092900060519      ** CARICAMENTO codici bolla che prevedono di tassare solo 1 varia
093000060519      *   e che permettono imponibile =0
093100081006      ** CARICAMENTO codici bolla che non sono c/s o recuperi per
093200081006      *   scaglionare
093300060519      *---------------------------------------------------------------*
093400060519     c     Carcbo        Begsr
093500081006      **
093600081006     c                   Movel     'TB'          Cod
093700081006     c                   z-add     0             z
093800081006     c     Ktab          setll     tabel00f
093900081006     c                   do        *hival
094000081006     c     Ktab          reade     tabel00f
094100081006      * fine file
094200081006     c                   If        %eof(tabel00f)
094300081006     c                   leave
094400081006     c                   endif
094500081006    1
094600081006    2c                   If        tblkey <> *blanks
094700081006     c                   Movel     TBLuni        DStb
094800081006      * imponibile a zero
094900081006    3c                   If        §tbrbl  = 'N'
095000081006     c                   add       1             z
095100081006     c                   movel     TBLkey        tbo_oK(z)
095200081006    3c                   endif
095300081006      **
095400081006    2c                   endif
095500081006    1c                   enddo
095600081006
095700060519      **
095800060519     c                   Movel     '3A'          Cod
095900060519     c                   z-add     0             K
096000060519     c                   z-add     0             KK
096100060519     c     Ktab          setll     tabel00f
096200060519     c                   do        *hival
096300060519     c     Ktab          reade     tabel00f
096400060519      * fine file
096500060519     c                   If        %eof(tabel00f)
096600060519     c                   leave
096700060519     c                   endif
096800060519    1
096900060519    2c                   If        tblkey <> *blanks
097000060519     c                   Movel     TBLuni        DS3a
097100060519      * imponibile a zero
097200060519    3c                   If        §3aim0  = 'S'
097300060519     c                   add       1             K
097400060519     c                   movel     TBLkey        im0(k)
097500060519    3c                   endif
097600060519      * bolla con singola varia
097700060519     c                   If        §3asva  <> *blanks
097800060519     c                   add       1             Kk
097900060519     c                   movel     tblkey        cbo(kk)
098000060519     c                   movel     §3asva        cbv(kk)
098100060519    3c                   endif
098200081006      * bolla con tipo bolla normale
098300081006     c     §3atb1        lookup    tbo_ok                                 30
098400081006     c                   if        %found
098500081006     c                   add       1             zz
098600081006     c                   movel     tblkey        cbo_ok(zz)
098700081006     c                   endif
098800060519      **
098900060519    2c                   endif
099000060519    1c                   enddo
099100060519
099200060519     c                   Endsr
099300141217
099400141217      /free
099500141217       //--------------------------------------------------------------
099600141217       //?Carico particolarità consegna con Pin Code.
099700141217       //--------------------------------------------------------------
099800141217       BEGSR Car7RPinCode;
099900141217
100000141217         cod = '7R';
100100141217         clear kk;
100200141217         setll (Codut:cod) TABEL00F;
100300141217         reade (Codut:cod) TABEL00F;
100400141217         DOW  not %eof(TABEL00F);
100500141217           IF  TBLflg = *blanks;
100600141217             ds7R = TBLuni;
100700141217             IF  §7Rpincode = 'S';
100800141217               kk += 1;
100900141217               skGMA(kk) = TBLkey;
101000141217             ENDIF;
101100141217           ENDIF;
101200141217           reade (Codut:cod) TABEL00F;
101300141217         ENDDO;
101400141217
101500141217       ENDSR;
101600141217      /end-free
101700141217
101800081006      *---------------------------------------------------------------*
101900081006      * Caricamento scaglioni
102000081006      *---------------------------------------------------------------*
102100081006     c     Car_scaglioni BEGSR
102200081006
102300081006     c                   eval      savprg = 999
102400081006     c                   clear                   yy
102500081006     c                   clear                   tipo_tar
102600081006     c                   clear                   num_sgl
102700081006      * cerco il giusto progressivo
102800081006
102900081006     c     ktam          setll     tntam01l
103000081006     c                   do        *hival
103100081006     c     ktam          reade     tntam01l
103200081006     c                   if        %eof(tntam01l)
103300081006     c                   leave
103400081006     c                   endif
103500081006      * verifico se progressivo annullato
103600081006     c                   if        tamatb <> ' '
103700081006     c                   iter
103800081006     c                   endif
103900081006      * controllo con la data di riferimento
104000120216     c                   If        rftdspr  < tamdst and rftdspr >= tamddt
104100081006     c                   eval      savprg = tamprg
104200081006     c                   leave
104300081006     c                   endif
104400081006     c                   if        rftdspr > tamdst
104500081006     c                   eval      savprg = tamprg
104600081006     c                   iter
104700081006     c                   endif
104800081006     c                   enddo
104900081006
105000081006     c                   if        savprg <> 999
105100081006      * trovato progressivo carico gli scaglioni relativi leggendo da titad
105200081006     c     ktad          setll     titad04l
105300110713     c                   do        *hival
105400081006     c     ktad          reade     titad04l
105500110713     c                   if        %eof(titad04l)
105600110713     c                   leave
105700110713     c                   endif
105800110713     c                   if        tadatb = ' '
105900081006      * recupero la chiave
106000081006     c                   eval      savlnp = tadlnp
106100081006     c                   eval      savorp = tadorp
106200081006     c                   eval      savnaz = tadnaz
106300081006     c                   eval      savcap = tadcap
106400110713     c                   leave
106500110713     c                   endif
106600110713     c                   enddo
106700081006
106800081006      * leggo tutti gli scaglioni con la stessa chiave
106900081006     c     ktad2         setll     titad04l
107000081006
107100081006     c                   do        *hival
107200081006     c     ktad2         reade     titad04l
107300081006
107400081006     c                   if        %eof(titad04l)
107500081006     c                   leave
107600081006     c                   endif
107700081006
107800081006     c                   if        tadatb <> ' '
107900081006     c                   iter
108000081006     c                   endif
108100081006
108200081006     c     tadsgl        lookup    sgl                                    31
108300081006
108400081006     c                   if        %found
108500081006     c                   leave
108600081006     c                   endif
108700081006
108800081006     c                   add       1             yy
108900081006     c                   eval      sgl(yy) = tadsgl
109000081006     c                   enddo
109100081006      * valorizzo il numero totali di scaglioni
109200081006     c                   z-add     yy            num_sgl
109300081006
109400081006     c                   movel     rftctrr       tipo_tar
109500081006
109600081006     c                   endif
109700081006      *
109800081006     c                   endsr
109900081006      *---------------------------------------------------------------*
110000081006      *  Ricerca dello scaglione per spedizione tassata
110100081006      *---------------------------------------------------------------*
110200081006     c     Ric_sgl       Begsr
110300081006
110400081006     c                   z-add     1             yy
110500081006
110600081006     c                   dow       yy <= num_sgl
110700081006     c                   if        misura <= sgl(yy)
110800081006     c                   eval      ok_scagl = *on
110900081006     c                   leave
111000081006     c                   endif
111100081006     c                   add       1             yy
111200081006     c                   enddo
111300081006
111400081006     c                   endsr
111500120921
111600120921      /free
111700120921       //--------------------------------------------------------------
111800120921       //?Memorizzo in SK il trasporto e le varie.
111900120921       //--------------------------------------------------------------
112000120921       BEGSR  Mem_Varie;
112100120921
112200120921       //?Il porto sempre al primo posto
112300120921         sk_Voce(1) = '.';
112400120921         IF  RFTporf > *zero;
112500120921           sk_ImpoF(1) += RFTporF;
112600120921           sk_ImpoR(1) += RFTporR;
112700120921         ENDIF;
112800120921
112900120921       //?Poi le varie
113000120924         IF  RFTva1F > *zero;
113100120921           Wvaria = RFTsv1F;
113200120921           WimpoF = RFTva1F;
113300120924           exsr  TotVariaF;
113400120924         ENDIF;
113500120924         IF  RFTva1R > *zero;
113600120924           Wvaria = RFTsv1R;
113700120921           WimpoR = RFTva1R;
113800120924           exsr  TotVariaR;
113900120921         ENDIF;
114000120921
114100120921         IF  RFTva2F > *zero;
114200120921           Wvaria = RFTsv2F;
114300120921           WimpoF = RFTva2F;
114400120924           exsr  TotVariaF;
114500120921         ENDIF;
114600120924         IF  RFTva2R > *zero;
114700120924           Wvaria = RFTsv2R;
114800120924           WimpoR = RFTva2R;
114900120924           exsr  TotVariaR;
115000120924         ENDIF;
115100120921
115200120921         IF  RFTva3F > *zero;
115300120921           Wvaria = RFTsv3F;
115400120921           WimpoF = RFTva3F;
115500120924           exsr  TotVariaF;
115600120921         ENDIF;
115700120924         IF  RFTva3R > *zero;
115800120924           Wvaria = RFTsv3R;
115900120924           WimpoR = RFTva3R;
116000120924           exsr  TotVariaR;
116100120924         ENDIF;
116200120921
116300120921         IF  RFTva4F > *zero;
116400120921           Wvaria = RFTsv4F;
116500120921           WimpoF = RFTva4F;
116600120924           exsr  TotVariaF;
116700120921         ENDIF;
116800120924         IF  RFTva4R > *zero;
116900120924           Wvaria = RFTsv4R;
117000120924           WimpoR = RFTva4R;
117100120924           exsr  TotVariaR;
117200120924         ENDIF;
117300120921
117400120921         IF  RFTva5F > *zero;
117500120921           Wvaria = RFTsv5F;
117600120921           WimpoF = RFTva5F;
117700120924           exsr  TotVariaF;
117800120921         ENDIF;
117900120924         IF  RFTva5R > *zero;
118000120924           Wvaria = RFTsv5R;
118100120924           WimpoR = RFTva5R;
118200120924           exsr  TotVariaR;
118300120924         ENDIF;
118400120921
118500120921         IF  RFTva6F > *zero;
118600120921           Wvaria = RFTsv6F;
118700120921           WimpoF = RFTva6F;
118800120924           exsr  TotVariaF;
118900120921         ENDIF;
119000120924         IF  RFTva6R > *zero;
119100120924           Wvaria = RFTsv6R;
119200120924           WimpoR = RFTva6R;
119300120924           exsr  TotVariaR;
119400120924         ENDIF;
119500120921
119600120921         IF  RFTva7F > *zero;
119700120921           Wvaria = RFTsv7F;
119800120921           WimpoF = RFTva7F;
119900120924           exsr  TotVariaF;
120000120921         ENDIF;
120100120924         IF  RFTva7R > *zero;
120200120924           Wvaria = RFTsv7R;
120300120924           WimpoR = RFTva7R;
120400120924           exsr  TotVariaR;
120500120924         ENDIF;
120600120921
120700120921         IF  RFTva8F > *zero;
120800120921           Wvaria = RFTsv8F;
120900120921           WimpoF = RFTva8F;
121000120924           exsr  TotVariaF;
121100120921         ENDIF;
121200120924         IF  RFTva8R > *zero;
121300120924           Wvaria = RFTsv8R;
121400120924           WimpoR = RFTva8R;
121500120924           exsr  TotVariaR;
121600120924         ENDIF;
121700120921
121800120921         IF  RFTva9F > *zero;
121900120921           Wvaria = RFTsv9F;
122000120921           WimpoF = RFTva9F;
122100120924           exsr  TotVariaF;
122200120921         ENDIF;
122300120924         IF  RFTva9R > *zero;
122400120924           Wvaria = RFTsv9R;
122500120924           WimpoR = RFTva9R;
122600120924           exsr  TotVariaR;
122700120924         ENDIF;
122800120921
122900120921         IF  RFTv10F > *zero;
123000120921           Wvaria = RFTs10F;
123100120921           WimpoF = RFTv10F;
123200120924           exsr  TotVariaF;
123300120921         ENDIF;
123400120924         IF  RFTv10R > *zero;
123500120924           Wvaria = RFTs10R;
123600120924           WimpoR = RFTv10R;
123700120924           exsr  TotVariaR;
123800120924         ENDIF;
123900120921
124000120921         IF  RFTv11F > *zero;
124100120921           Wvaria = RFTs11F;
124200120921           WimpoF = RFTv11F;
124300120924           exsr  TotVariaF;
124400120921         ENDIF;
124500120924         IF  RFTv11R > *zero;
124600120924           Wvaria = RFTs11R;
124700120924           WimpoR = RFTv11R;
124800120924           exsr  TotVariaR;
124900120924         ENDIF;
125000120921
125100120921         IF  RFTv12F > *zero;
125200120921           Wvaria = RFTs12F;
125300120921           WimpoF = RFTv12F;
125400120924           exsr  TotVariaF;
125500120921         ENDIF;
125600120924         IF  RFTv12R > *zero;
125700120924           Wvaria = RFTs12R;
125800120924           WimpoR = RFTv12R;
125900120924           exsr  TotVariaR;
126000120924         ENDIF;
126100120921
126200120921         IF  RFTv13F > *zero;
126300120921           Wvaria = RFTs13F;
126400120921           WimpoF = RFTv13F;
126500120924           exsr  TotVariaF;
126600120921         ENDIF;
126700120924         IF  RFTv13R > *zero;
126800120924           Wvaria = RFTs13R;
126900120924           WimpoF = RFTv13R;
127000120924           exsr  TotVariaR;
127100120924         ENDIF;
127200120921
127300120921         IF  RFTv14F > *zero;
127400120921           Wvaria = RFTs14F;
127500120921           WimpoF = RFTv14F;
127600120924           exsr  TotVariaF;
127700120921         ENDIF;
127800120924         IF  RFTv14R > *zero;
127900120924           Wvaria = RFTs14R;
128000120924           WimpoR = RFTv14R;
128100120924           exsr  TotVariaR;
128200120924         ENDIF;
128300120921
128400120921         IF  RFTv15F > *zero;
128500120921           Wvaria = RFTs15F;
128600120921           WimpoF = RFTv15F;
128700120924           exsr  TotVariaF;
128800120921         ENDIF;
128900120924         IF  RFTv15R > *zero;
129000120924           Wvaria = RFTs15R;
129100120924           WimpoR = RFTv15R;
129200120924           exsr  TotVariaR;
129300120924         ENDIF;
129400120921
129500120921         IF  RFTv16F > *zero;
129600120921           Wvaria = RFTs16F;
129700120921           WimpoF = RFTv16F;
129800120924           exsr  TotVariaF;
129900120921         ENDIF;
130000120924         IF  RFTv16R > *zero;
130100120924           Wvaria = RFTs16R;
130200120924           WimpoR = RFTv16R;
130300120924           exsr  TotVariaR;
130400120924         ENDIF;
130500120921
130600120921         IF  RFTv17F > *zero;
130700120921           Wvaria = RFTs17F;
130800120921           WimpoF = RFTv17F;
130900120924           exsr  TotVariaF;
131000120921         ENDIF;
131100120924         IF  RFTv17R > *zero;
131200120924           Wvaria = RFTs17R;
131300120924           WimpoR = RFTv17R;
131400120924           exsr  TotVariaR;
131500120924         ENDIF;
131600120921
131700120921         IF  RFTv18F > *zero;
131800120921           Wvaria = RFTs18F;
131900120921           WimpoF = RFTv18F;
132000120924           exsr  TotVariaF;
132100120921         ENDIF;
132200120924         IF  RFTv18R > *zero;
132300120924           Wvaria = RFTs18R;
132400120924           WimpoR = RFTv18R;
132500120924           exsr  TotVariaR;
132600120924         ENDIF;
132700120921
132800120921         IF  RFTv19F > *zero;
132900120921           Wvaria = RFTs19F;
133000120921           WimpoF = RFTv19F;
133100120924           exsr  TotVariaF;
133200120921         ENDIF;
133300120924         IF  RFTv19R > *zero;
133400120924           Wvaria = RFTs19R;
133500120924           WimpoR = RFTv19R;
133600120924           exsr  TotVariaR;
133700120924         ENDIF;
133800120921
133900120921         IF  RFTv20F > *zero;
134000120921           Wvaria = RFTs20F;
134100120921           WimpoF = RFTv20F;
134200120924           exsr  TotVariaF;
134300120921         ENDIF;
134400120924         IF  RFTv20R > *zero;
134500120924           Wvaria = RFTs20R;
134600120924           WimpoR = RFTv20R;
134700120924           exsr  TotVariaR;
134800120924         ENDIF;
134900120921
135000120921       ENDSR;
135100120921
135200120921       //--------------------------------------------------------------
135300120924       //?Incremento totali fatturato per voce in schiera.                       ?
135400120921       //--------------------------------------------------------------
135500120924       BEGSR  TotVariaF;
135600120921
135700120921         ww = %lookup( Wvaria : sk_Voce : 2 );
135800120921         IF  ww = *zero;
135900120921           ww = %lookup( *blank : sk_Voce : 2 );
136000120921           IF  ww > *zero;
136100120921             sk_Voce(ww) = Wvaria;
136200120921           ENDIF;
136300120921         ENDIF;
136400120921
136500120921         sk_ImpoF(ww) += WimpoF;
136600120921
136700120921       ENDSR;
136800120924
136900120924       //--------------------------------------------------------------
137000120924       //?Incremento totali per voce rifatturato in schiera.                       ?
137100120924       //--------------------------------------------------------------
137200120924       BEGSR  TotVariaR;
137300120924
137400120924         ww = %lookup( Wvaria : sk_Voce : 2 );
137500120924         IF  ww = *zero;
137600120924           ww = %lookup( *blank : sk_Voce : 2 );
137700120924           IF  ww > *zero;
137800120924             sk_Voce(ww) = Wvaria;
137900120924           ENDIF;
138000120924         ENDIF;
138100120924
138200120924         sk_ImpoR(ww) += WimpoR;
138300120924
138400120924       ENDSR;
138500120924
138600120924      /end-free
138700081006      *
138800060519      *---------------------------------------------------------------*
138900060519      * Operazioni Iniziali                                           *
139000060519      *---------------------------------------------------------------*
139100060519     c     RoutInz       BEGSR
139200060519      *
139300060519      * Reperisco dati job
139400060519     c                   exsr      DatiJob
139500060519      * Reperisco data e ora
139600060519     c                   time                    w0140
139700060519     c                   movel     w0140         wora
139800060522     c                   move      w0140         wdate
139900060519      *
140000060801      * verifico se sono in filiale faccio le ovrdbf per i file di sede
140100060801     c                   If        dutlpo <> 'S'
140200060801      * librerie
140300060801
140400060801      * Se sono in ambiente buono - GAITRAGRU
140500060801     c                   If        knsif = 'FILTRA201 '
140600060801     c                   Eval      wlib = 'GAITRAGRU '
140700060801     c                   Eval      wlib1 = 'GAITRAAZM '
140800060801      *  Se sono in ambiente di prova - GAITRAGRPS
140900060801     c                   Else
141000060801     c                   Eval      wlib = 'GAITRAGRPS'
141100060801     c                   Eval      wlib1 = 'GAITRAAZM '
141200060801     c                   EndIf
141300060801      * WFRFT01L
141400060801     c                   Clear                   comman
141500060801     c                   Movel(p)  cmdt(1)       comman
141600060801     c                   Eval      %Subst(comman:30:10) = wlib1
141700060801     c                   Eval      comman =
141800060801     c                             %trim(comman) + '/WFRFT01L)'
141900060801     c                   Call      'QCMDEXC'                            99
142000060801     c                   Parm                    comman
142100060801     c                   Parm                    lenght
142200060801
142300060801      * TITAI01L
142400060801     c                   Clear                   comman
142500060801     c                   Movel(p)  cmdt(2)       comman
142600060801     c                   Eval      %Subst(comman:30:10) = wlib
142700060801     c                   Eval      comman =
142800060801     c                             %trim(comman) + '/TITAI01L)'
142900060801     c                   Call      'QCMDEXC'                            99
143000060801     c                   Parm                    comman
143100060801     c                   Parm                    lenght
143200060801
143300060801      * TITAS30C
143400060801     c                   Clear                   comman
143500060801     c                   Movel(p)  cmdt(3)       comman
143600060801     c                   Eval      %Subst(comman:30:10) = wlib
143700060801     c                   Eval      comman =
143800060801     c                             %trim(comman) + '/TITAS30C)'
143900060801     c                   Call      'QCMDEXC'                            99
144000060801     c                   Parm                    comman
144100060801     c                   Parm                    lenght
144200060801      * FIAR531C
144300060801     c                   Clear                   comman
144400060801     c                   Movel(p)  cmdt(4)       comman
144500060801     c                   Eval      %Subst(comman:30:10) = wlib
144600060801     c                   Eval      comman =
144700060801     c                             %trim(comman) + '/FIAR531C)'
144800060801     c                   Call      'QCMDEXC'                            99
144900060801     c                   Parm                    comman
145000060801     c                   Parm                    lenght
145100060801      * TNCSB03L
145200060801     c                   Clear                   comman
145300060801     c                   Movel(p)  cmdt(5)       comman
145400060801     c                   Eval      %Subst(comman:30:10) = wlib
145500060801     c                   Eval      comman =
145600060801     c                             %trim(comman) + '/TNCSB03L)'
145700060801     c                   Call      'QCMDEXC'                            99
145800060801     c                   Parm                    comman
145900060801     c                   Parm                    lenght
146000060801
146100060801     c                   endif
146200060801      *
146300060801     c                   open      wfrft01l
146400060801     c                   open      titai01l
146500060801     c                   open      titas30c
146600060801     c                   open      fiar531c
146700060801     c                   open      tncsb03l
146800060801
146900060519     c                   ENDSR
147000060519      *
147100060519      *---------------------------------------------------------------*
147200060519      * Reperimento Dati del job (Utente/Operativi)                   *
147300060519      *---------------------------------------------------------------*
147400060519     c     DatiJob       BEGSR
147500060519      *
147600060519     c     *dtaara       define    §azute        azuteds
147700060519     c     *dtaara       define    §datiute      ddatiute
147800060519      *
147900060519     c                   in(E)     *dtaara
148000060519     c                   IF        %ERROR or RSUT = *blanks
148100060519     c                   clear                   Tibs34Ds
148200060519     c                   call      'TIBS34R'
148300060519     c                   parm                    Tibs34Ds
148400060519     c                   in        *dtaara
148500060519     c                   Endif
148600060519
148700060519      *
148800060519     c                   ENDSR
148900060801** cmdt
149000060801OVRDBF FILE(WFRFT01L) TOFILE(
149100060801OVRDBF FILE(TITAI01L) TOFILE(
149200060801OVRDBF FILE(TITAS30C) TOFILE(
149300060801OVRDBF FILE(FIAR531C) TOFILE(
149400060801OVRDBF FILE(TNCSB03L) TOFILE(
149500060801DLTOVR FILE(*ALL)
