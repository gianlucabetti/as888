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
004900120921
005000060801      * ovrdbf
005100060801     d cmdt            s             60    dim(06) ctdata perrcd(1)
005200060504      * Ds - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
005300060504      *
005400060504     d KPJBA         e ds
005500060518      * - Parametri x il lancio del pgm batch
005600060518     d TNSF40DS      e ds                  inz
005700060522      * - Parametri x Controllo profilo utenti
005800060522     d TIBS34DS      e ds                  inz
005900060522      * - Ds di riferimento al file esterno AZUTE00F
006000060522     d AZUTEds       e ds                  extname(AZUTE00F)
006100060522      * - Ds per dati organigramma
006200060522     d DDatiUte      e ds
006300081006      ** Tipi bolla
006400081006     D DSTB          E DS
006500060522      ** Codici bolla
006600060522     D DS3A          E DS
006700081006      ** Tipi tariffa
006800081006     D DSTR          E DS
006900110701      ** DS del tasflo del file TITAS00F
007000110701     D DTASFLO       E DS
007100060518      ** DS Calcolo tassazione  - Varie
007200060518     D DTAS          E DS
007300060518     D  dstasflo     E                     extfld(tasflo)
007400060926      ** DS Flag operativi DS DTAS
007500060926     d Dtas01        e ds
007600060926
007700060518     d Dtasv         e ds
007800060518     d  sv                     1     20
007900060518     d                                     Dim(20)                              Sigle varie
008000060518     d  va                    21    140P 3
008100060518     d                                     Dim(20)                              Importi varie
008200060522      * recupero bancali e colli originali
008300060522     d dAr5Ban       e ds
008400060522
008500060522     d dAr5Bnb       e ds
008600120921
008700120921       // -?Tabella "CC" = Codici Tassazione?
008800120921     d dsCC          e ds                  inz
008900120921
009000060518      * - Ds per scrittura file di lavoro
009100060518     d                 ds
009200060518     d  RFTsv1r                1      1
009300060518     d  RFTsv2r                2      2
009400060518     d  RFTsv3r                3      3
009500060518     d  RFTsv4r                4      4
009600060518     d  RFTsv5r                5      5
009700060518     d  RFTsv6r                6      6
009800060518     d  RFTsv7r                7      7
009900060518     d  RFTsv8r                8      8
010000060518     d  RFTsv9r                9      9
010100060518     d  RFTs10r               10     10
010200060518     d  RFTs11r               11     11
010300060518     d  RFTs12r               12     12
010400060518     d  RFTs13r               13     13
010500060518     d  RFTs14r               14     14
010600060518     d  RFTs15r               15     15
010700060518     d  RFTs16r               16     16
010800060518     d  RFTs17r               17     17
010900060518     d  RFTs18r               18     18
011000060518     d  RFTs19r               19     19
011100060518     d  RFTs20r               20     20
011200060518     d  RFT_svr                1     20    Dim(20)                              Sigle varie
011300060512
011400060518      * - Ds per scrittura file di lavoro
011500060518     d                 ds
011600060518     d  RFTva1r                1     11  3
011700060518     d  RFTva2r               12     22  3
011800060518     d  RFTva3r               23     33  3
011900060518     d  RFTva4r               34     44  3
012000060518     d  RFTva5r               45     55  3
012100060518     d  RFTva6r               56     66  3
012200060518     d  RFTva7r               67     77  3
012300060518     d  RFTva8r               78     88  3
012400060518     d  RFTva9r               89     99  3
012500060518     d  RFTv10r              100    110  3
012600060518     d  RFTv11r              111    121  3
012700060518     d  RFTv12r              122    132  3
012800060518     d  RFTv13r              133    143  3
012900060518     d  RFTv14r              144    154  3
013000060518     d  RFTv15r              155    165  3
013100060518     d  RFTv16r              166    176  3
013200060518     d  RFTv17r              177    187  3
013300060518     d  RFTv18r              188    198  3
013400060518     d  RFTv19r              199    209  3
013500060518     d  RFTv20r              210    220  3
013600060518     d  RFT_var                1    220  3 Dim(20)                              Importo varie
013700060504      *
013800060504      * - Controllo/Inversione date
013900060504     d WLBdat          ds                  inz
014000060504     d   G08dat                       8  0 inz
014100060504     d   G08inv                       8  0 inz
014200060504     d   G08err                       1    inz('3')
014300060504     d   G08tgi                       5  0 inz
014400060504      *
014500060504      * - Variabili definite a programma
014600060519     d codut           s              1  0 inz(1)
014700060504     d Wdate           s              8  0 inz
014800060518     d Wora            s              6  0 inz
014900060516     d W0140           s             14  0 inz
015000081006     d z               s              4  0
015100081006     d zz              s              4  0
015200060518     d k               s              4  0
015300060519     d kk              s              4  0
015400060522     d xx              s              4  0
015500081006     d yy              s              4  0
015600120921     d ww              s              4  0
015700081006     d num_sgl         s              4  0
015800060522     d kscfil          s              3  0
015900081006     d tipo_tar        s              1  0
016000081006     d ok_scagl        s               n
016100060522     d cod             s                   like(tblcod)
016200081006     d key             s                   like(tblkey)
016300060522     d kAr5Trd         s                   Like(Ar5Trd)
016400060522     d antepo          s                   like(tasimv)
016500060522     d W_tasdiv        s                   like(tasdiv)
016600060522     d W_dtas          s                   like(dtas)
016700060522     d W_dtasv         s                   like(dtasv)
016800060522     D Wcev            s                   like(evbcev) inz('RIC')
016900081006     d prg2            s                   like(tamprg) inz
017000081006     d ctr2            s                   like(tamctr) inz
017100081006     d ksc2            s                   like(tamksc) inz
017200081006     d savprg          s                   like(tamprg)
017300081006     d savlnp          s                   like(tadlnp)
017400081006     d savorp          s                   like(tadorp)
017500081006     d savnaz          s                   like(tadnaz)
017600081006     d savcap          s                   like(tadcap)
017700081006     d misura          s                   like(tadsgl)
017800060801
017900060801     d wlib            s             10
018000060801     d wlib1           s             10
018100060801     d comman          s             80
018200060801     d lenght          s             15  5 inz(80)
018300060801
018400120921       // -?Campi di comodo?
018500120921     d Wconta          s              1  0 inz
018600120921     d Wvaria          s                   like(TASsv1)   inz
018700120921     d WimpoF          s                   like(TASva1)   inz
018800120921     d WimpoR          s                   like(TASva1)   inz
018900060522
019000060504      *
019100060504      * Key-List - - - - - - - - - - - - - - - - - - - - - - - - - - -*
019200060504      *
019300060518      * - WFRFT01L
019400060518     c     KRFT          Klist
019500060518     c                   Kfld                    I40noj
019600060518     c                   Kfld                    I40pru
019700060518     c                   Kfld                    I40dta
019800060518     c                   Kfld                    I40ora
019900060517
020000060518      * - TITAI01L / TITAS30C
020100060518     c     KSPE          Klist
020200060518     c                   Kfld                    RFTaas
020300060518     c                   Kfld                    RFTlnp
020400060518     c                   Kfld                    RFTnrs
020500060518     c                   Kfld                    RFTnsp
020600060518     c                   Kfld                    RFTtbl
020700060519      * Tncsb03l
020800060519     c     KCSB          Klist
020900060519     c                   Kfld                    TASaas
021000060519     c                   Kfld                    TASlnp
021100060519     c                   Kfld                    TASnrs
021200060519     c                   Kfld                    TASnsp
021300060519      * Tabel00f
021400060519     c     KTAB          Klist
021500060519     c                   Kfld                    codut
021600060519     c                   Kfld                    cod
021700081006     c     KTABkey       Klist
021800081006     c                   Kfld                    codut
021900081006     c                   Kfld                    cod
022000081006     c                   Kfld                    key
022100060522      * Fiar531c
022200060522     c     kFiar5        Klist
022300060522     c                   Kfld                    TasAas
022400060522     c                   Kfld                    TasLnp
022500060522     c                   Kfld                    TasNrs
022600060522     c                   Kfld                    TasNsp
022700060522     c                   Kfld                    kAr5Trd
022800060522      * Chiave file eventi FNEVB
022900060522     c     Kevb4         Klist
023000060522     c                   Kfld                    Tasaas
023100060522     c                   Kfld                    Taslnp
023200060522     c                   Kfld                    Tasnrs
023300060522     c                   Kfld                    Tasnsp
023400060522     c                   Kfld                    Wcev
023500081006      ** accesso tntam x cliente
023600081006     c     Ktam          Klist
023700081006     c                   Kfld                    rftkscr
023800081006     c                   Kfld                    rftctrr
023900081006      ** accesso titad
024000081006     c     Ktad          Klist
024100081006     c                   Kfld                    tamksc
024200081006     c                   Kfld                    tamctr
024300081006     c                   Kfld                    tamprg
024400081006
024500081006     c     Ktad2         Klist
024600081006     c                   Kfld                    tamksc
024700081006     c                   Kfld                    tamctr
024800081006     c                   Kfld                    tamprg
024900081006     c                   Kfld                    savlnp
025000081006     c                   Kfld                    savorp
025100081006     c                   Kfld                    savnaz
025200081006     c                   Kfld                    savcap
025300060504
025400060504      *---------------------------------------------------------------*
025500060504      *  RIEPILOGO INDICATORI                                         *
025600060504      *---------------------------------------------------------------*
025700060727      * 05/30 - Comodo
025800060727      * 50    - Data fattura antecedente 30-04-06 stampo scritta
025900060727      * 51    - Comodo
026000060727      * 90    - OF
026100060727      * 99    - Comodo
026200060504      *---------------------------------------------------------------*
026300060504
026400060504     c     *Entry        plist
026500060504     c                   parm                    KPJBA
026600060518     c                   movel     kpjbu         Tnsf40ds
026700060504      *
026800060504      * Operazioni Iniziali
026900060504     c                   exsr      RoutInz
027000060504      *
027100060523     c                   exsr      Carcbo
027200060523      *
027300060518      * Elaborazione del file
027400060518     c
027500060518do  1c                   exsr      Elabora
027600060504      * 1° videata
027700060518     c                   exsr      Stampa
027800060504      * Fine
027900060509      *
028000060512     c     fine          tag
028100060801
028200060801     c                   If        Dutlpo <> 'S'
028300060801     c                   Clear                   comman
028400060801     c                   Movel(p)  cmdt(6)       comman
028500060801     c                   Call      'QCMDEXC'                            99
028600060801     c                   Parm                    comman
028700060801     c                   Parm                    lenght
028800060801     c                   endif
028900060801
029000060504     c                   movel     *on           *inLR
029100060504      *
029200060504      *
029300060504      *---------------------------------------------------------------*
029400060518      * Elabora ritassazione                                          *
029500060504      *---------------------------------------------------------------*
029600060518     c     Elabora       BEGSR
029700060504      *
029800060518e   1c     Krft          setll     wfrft01l
029900060504
030000060504     c                   do        *hival
030100060522      *
030200060518     c     krft          reade     wfrft01l
030300060518
030400060522     c                   If        %eof(Wfrft01l)
030500060518     c                   leave
030600060518     c                   endif
030700060518      * verifico se record già elaborato
030800060518     c                   if        rftela <> ' '
030900060518     c                   iter
031000060518     c                   endif
031100060726      * imposto i campi della testata
031200060726     c                   if        prtfiv = *zeros
031300060726     c                   eval      prtfiv = rftfiv
031400060726     c                   eval      prtnft = rftnft
031500060726      *
031600060726     c                   clear                   WLBdat
031700060726     c                   eval      G08inv = rftdft
031800060726     c                   eval      G08err = '3'
031900060726     c                   call      'XSRDA8'
032000060726     c                   parm                    WLBdat
032100060726     c                   z-add     G08dat        prtdft
032200081006      * se si desidera tassazione scaglionata recupero gli scaglioni della tariffa
032300081006     c                   If        i40sgl = 'S'
032400081006     c                   exsr      car_scaglioni
032500081006     c                   endif
032600081006
032700060726     c                   endif
032800060727      * controllo data fattura
032900060727     c                   eval      *in50 = rftdft < 20060430
033000060518      * tasso
033100060518     c                   exsr      tassaz
033200060504      *
033300060504     c                   enddo
033400060504      *
033500060504     c                   ENDSR
033600060504      *
033700060522      *---------------------------------------------------------------*
033800060522      * Stampa Finale
033900060522      *---------------------------------------------------------------*
034000060522     c     Stampa        BEGSR
034100060522
034200060522      * testata
034300060530     c                   write     sf41t0
034400060522     c                   write     sf41ts
034500060522     c                   write     sf41t3
034600060522     c                   write     sf41t2
034700060522     c                   write     sf41t3
034800060522      * valorizzo i campi
034900060522     c                   eval      prtnoj = knmeb
035000060522     c                   eval      prtpru = rftpru
035100060522      *
035200060522     c                   clear                   WLBdat
035300060522     c                   eval      G08inv = rftdta
035400060522     c                   eval      G08err = '3'
035500060522     c                   call      'XSRDA8'
035600060522     c                   parm                    WLBdat
035700060522     c                   z-add     G08dat        prtdta
035800060522      *
035900060522     c                   eval      prtora = rftora
036000060614      * calcolo la differente
036100060614     c                   eval      prtdiffe = prtimvf - prtimvr
036200060522      *
036300060522     c                   write     sf41d3
036400060522      *
036500081006      * stampo la suddivisione per scaglione solo se richiesta
036600081006     c                   If        i40sgl = 'S'
036700081006     c                   eval      cod = 'TR'
036800081006     c                   movel(p)  tipo_tar      key
036900081006     c     ktabkey       chain     Tabel00f
037000081006     c                   if        %found(tabel00f)
037100081006     c                   movel     tbluni        dstr
037200081006      * valorizzo i campi descrittivi di stampa
037300081006     c                   movel     §trdst        prttar
037400081006     c                   endif
037500081006      * testata
037600081006     c                   write     sf41t04
037700081006     c                   write     sf41t3
037800081006     c                   write     sf41t4
037900081006     c                   write     sf41t3
038000081006
038100081006      * stampo dettaglio per ogni scaglione
038200081006     c                   z-add     1             yy
038300081006     c                   dow       yy <= num_sgl
038400081006
038500081006     c                   eval      prtsca = sgl(yy)
038600081006     c                   eval      prtimf = imf(yy)
038700081006     c                   eval      prtimr = imr(yy)
038800081006     c                   eval      prtdif = (prtimf - prtimr)
038900081006     c                   eval      prttkg = tkg(yy)
039000081006     c                   eval      prttcl = tcl(yy)
039100081006     c                   eval      prttvl = tvl(yy)
039200081006     c                   eval      prtnsp = nsp(yy)
039300081006     c                   eval      prtnmt = nmt(yy)
039400081006
039500081006     c                   write     sf41d4
039600081006      * pulisco la descrizione della tariffa
039700081006     c                   clear                   prttar
039800081006
039900081006     c                   add       1             yy
040000081006
040100081006     c                   enddo
040200081006      *
040300081006      * stampo se ci sono delle bolle di recupero
040400081006     c                   If        prtnspr > 0
040500081006     c                   write     sf41d4r
040600081006     c                   endif
040700081006      *
040800081006     c                   endif
040900120921      /free
041000120921       //?Stampa la suddivisione per voce se richiesta
041100120921        IF  I40sva = 'S';
041200120921          exsr  Stampa_TOT;
041300120921        ENDIF;
041400120921      /end-free
041500081006      *
041600060522     c                   Endsr
041700120921
041800120921      /free
041900120921       //--------------------------------------------------------------
042000120921       //?Stampa suddivisione Voci.
042100120921       //--------------------------------------------------------------
042200120921       BEGSR  Stampa_TOT;
042300120921
042400120921         write  SF41T05;
042500120921         write  SF41T5;
042600120921
042700120921         ww = 1;
042800120921
042900120921       //?SE non ci fosse la voce trasporto (che è sempre alla?
043000120921       //?prima posizione): si inizia a stampare dalla 2ª Varia?
043100120921         IF  sk_ImpoF(1) = *zero;
043200120921           ww = 2;
043300120921         ENDIF;
043400120921
043500120921       //?Stampa delle varie di 2 in 2?
043600120921         DoW  sk_Voce(ww) <> *blank;
043700120921
043800120921           Wconta = 1;
043900120921           clear  SF41D5;
044000120921
044100120921           DoW  Wconta <= 2  and  sk_Voce(ww) <> *blank;
044200120921
044300120921             Wvaria = sk_Voce(ww);
044400120921             if  Wconta = 1;
044500120921               //?Nella prima posizione: Trasporto?
044600120921               if  ww = 1;
044700120921                 P1Dsv1 = 'TRASPORTO      ';
044800120921               else;
044900120921                 exsr  sr_TabCC;
045000120921                 P1Csv1 = sk_Voce(ww) + ' -';
045100120921                 P1Dsv1 = §CCdes;
045200120921               endif;
045300120921               P1Cva1F = sk_ImpoF(ww);
045400120921               P1Cva1R = sk_ImpoR(ww);
045500120921             else;
045600120921               exsr  sr_TabCC;
045700120921               P1Csv2  = sk_Voce(ww) + ' -';
045800120921               P1Dsv2  = §CCdes;
045900120921               P1Cva2F = sk_ImpoF(ww);
046000120921               P1Cva2R = sk_ImpoR(ww);
046100120921             endif;
046200120921
046300120921             ww += 1;
046400120921             Wconta += 1;
046500120921
046600120921           EndDo;
046700120921
046800120921           write  SF41D5;
046900120921
047000120921         EndDo;
047100120921
047200120921         //?Totale Imponibile?
047300120925         //P1CimvF = %xfoot( sk_ImpoF);
047400120925         //P1CimvR = %xfoot( sk_ImpoR);
047500120921
047600120921         //?Stampa Totali per Fattura?
047700120921         write  SF41G5;
047800120921
047900120921       ENDSR;
048000120921
048100120921       //--------------------------------------------------------------
048200120921       //?Decodifica Sigla Varia.                                      ?
048300120921       //--------------------------------------------------------------
048400120921       BEGSR  sr_TabCC;
048500120921
048600120921         clear  dsCC;
048700120921         cod = 'CC';
048800120921         key = 'VARIE  ' + Wvaria;
048900120921
049000120921         chain  (codut:cod:key) TABEL00F;
049100120921
049200120921         IF  %found(TABEL00F);
049300120921           dsCC = TBLuni;
049400120921         ELSE;
049500120921           §CCdes = *all'?';
049600120921         ENDIF;
049700120921
049800120921       ENDSR;
049900120921      /end-free
050000060522      *
050100060515      *---------------------------------------------------------------*
050200060518      *  Routine di tassazione
050300060515      *---------------------------------------------------------------*
050400060518     c     Tassaz        Begsr
050500060518      *
050600060518     c                   clear                   dtas
050700060518     c                   clear                   dtasv
050800060518      * aggancio titas
050900060518     c     KSPE          chain     titas30c
051000060522   a1c                   if        %found(titas30c)
051100070411      * Pulisco i campi dell'assicurazione se calcolata in fattura
051200070411    2c                   IF        Tasfcm = 'F'
051300070411     c                   clear                   tasias
051400070411     c                   clear                   tasvas
051500070411    2c                   Endif
051600070411
051700060530      * valorizzo i campi del file printer
051800060530      *
051900060530     c                   clear                   WLBdat
052000060530     c                   movel     tasaas        g08inv
052100060530     c                   move      tasmgs        g08inv
052200060530     c                   eval      G08err = '3'
052300060530     c                   call      'XSRDA8'
052400060530     c                   parm                    WLBdat
052500060530     c                   z-add     G08dat        prtdsp
052600060530
052700060530     c                   z-add     tasksc        prtksc
052800060530     c                   z-add     tasctr        prtctr
052900060530
053000060530     c                   clear                   WLBdat
053100060530     c                   movel     rftdspr       g08inv
053200060530     c                   eval      G08err = '3'
053300060530     c                   call      'XSRDA8'
053400060530     c                   parm                    WLBdat
053500060530     c                   z-add     G08dat        prtdspr
053600060530
053700060530      *
053800060519      * valorizzo la DS DTAS e DTASV
053900060519     c                   clear                   dtasv
054000060519     c                   clear                   taspor
054100060519     c                   clear                   tasimv
054200060518      *
054300060519      * cerco in TITAI l'immagine della spedizione prima della tassazione
054400060519     c                   exsr      RIC_tai
054500060519      * se tassata fino all'imponibile non tasso
054600060522    1c                   if        tasimv = 0
054700060519      ** Ricerca contrassegno
054800060519    2c                   IF        Tasfca <> *blanks
054900060519     c                   Exsr      Carcsb
055000060519    2c                   Endif
055100060519
055200060519      *  se si tratta di codice bolla che prevede solo una varia, passo solo
055300060519      *  quella
055400060519     c                   z-add     1             kk
055500060519     c     tascbo        lookup    cbo(kk)                                05
055600060522    2c                   if        *in05
055700060519     c                   eval      tassva=cBV(kk)
055800060522    2c                   endif
055900060522
056000060522     c                   movel     Tasftc        tastc1
056100060522     c                   Z-add     rftdft        tasdct
056200060726
056300060726     c                   eval      Tasksc = rftkscr
056400060726     c                   eval      Tasctr = rftctrr
056500061221     c                   movel     rftaas        tasdsp
056600061221     c                   move      rftmgs        tasdsp
056700060726
056800060522      * Flag SI NO DDT
056900060522    2C                   If        tasll1 = 'C' or tasll1 = 'S'
057000060522     C                   movel     'S'           Tasddt
057100060522   x2C                   Else
057200060522     C                   Clear                   Tasddt
057300060522    2C                   Endif
057400060926     c****               movel     tasfbr        dstasflo
057500060926     c                   clear                   dtas01
057600060926     c                   movel     tasfbr        §asfbr
057700060926     c                   movel     tascca        §ascca
057800110701      * valorizzo flag email al destinatario
057900110701     c                   movel     tasflo        dtasflo
058000110701     c                   move      §floemd       §asemd
058100110701
058200061221      * se data spedizione differente da ritassazione passo nel flo anche la data ritassazione
058300061221     c                   If        tasdsp <> rftdspr
058400070108     c                   move      rftdspr       §asdrt
058500061221     c                   endif
058600061221
058700060926     c                   eval      dstasflo = dtas01
058800060522
058900060522      * Bancali
059000060522    2c                   If        %Subst(TasGva:1:1) = 'B'
059100060522
059200060522     c                   eval      kAr5Trd  = 'BAN'
059300060522     c     kFiar5        Chain     Fiar531c
059400060522If  3c                   If        %Found(Fiar531c)
059500060522     c                   Movel     Ar5Uni        dAr5Ban
059600060522    3c                   EndIf
059700060522      * Bancali
059800060522     c                   Eval      TasBan = §Ar5Nba + §Ar5Nb2
059900060522    2c                   EndIf
060000060522      **
060100060522      * Colli Originali
060200060522    2c                   If        %Subst(TasGva:1:1) = 'O'
060300060522
060400060522     c                   eval      kAr5Trd  = 'BNB'
060500060522     c     kFiar5        Chain     Fiar531c
060600060522If  3c                   If        %Found(Fiar531c)
060700060522     c                   Movel     Ar5Uni        dAr5Bnb
060800060522    3c                   EndIf
060900060522      * Colli
061000060522     c                   Eval      TasNcl = §Ar5bcor
061100060522    2c                   EndIf
061200060522      *
061300060522      * verifico se è una bolla non tassata fino l'imponibile , una bolla di reso ,
061400060522      * con VARIA 'N' valorizzata con 888888 chiamo il TNSF20R per il calcolo dell'anteporto
061500060522      *
061600060522    2c                   if        tasimv = 0 and tasfbr = 'S'
061700060522     c                   z-add     1             xx
061800060522     c     'N'           lookup    sv(xx)                                 30
061900060522    3c                   if        *in30 and va(xx) =  88888888
062000060522      * tasso l'anteporto
062100060522     c                   exsr      Varian
062200060522    3c                   endif
062300060522      *
062400060522    2c                   endif
062500060522      *
062600060522      * se ritorno dal calcolo della varia N e c'è errore non proseguo
062700060522      *
062800060522     c                   Setoff                                       05
062900060522    2c                   if        taserr = ' '
063000060522      * Se devo tassare per una varia e c'e' gia', non richiamo la tassazione  se importo
063100060522      * della varia uguale  a zero
063200060522    3c                   if        tassva<>*blanks and tasimv=0
063300060522     c                   z-add     1             kk
063400060522     c     tassva        lookup    sv(kk)                                 05
063500060522    4c                   if        *in05 and va(kk) > 0
063600060522     c                   setoff                                       05
063700060522    4c                   endif
063800060522    3c                   endif
063900060522      * verifico se esiste la varia & con importo uguale a 8888888 e se si passo al
064000060522      * TNSF20R il flag di calcolo varia &
064100060522      *
064200060522     c                   setoff                                       51
064300060522     c                   z-add     1             kk
064400060522     c     '&'           lookup    sv(kk)                                 51
064500060522    3c                   if        *in51 and va(kk) = 88888888
064600060522     c                   eval      tasfcaa = 'S'
064700060522     c                   clear                   sv(kk)
064800060522     c                   clear                   va(kk)
064900060522    3c                   endif
065000060522      * verifico se devo calcolare l'addebito di lasciato avviso
065100060522     c                   movel     rftkscr       kscfil
065200060522      *
065300060522      * verifico se c'è evento
065400060522     c     Kevb4         chain     Fnevb04l
065500060522
065600060522    4c                   If        %found(Fnevb04l)
065700060522     c                   eval      tasric = 'S'
065800060522    4c                   endif
065900060522
066000060522      **
066100060522      **
066200060522    3c                   if        tassva=*blanks or not *in05
066300060522     c                   call      'TNSF20R'
066400060522     c                   parm                    Kpjba
066500060522     c                   parm                    Dtas
066600060522     c                   parm                    Dtasv
066700060522    3c                   endif
066800060522      **
066900060522    2c                   endif
067000060522    1c                   endif
067100060519
067200060522     c                   If        taserr = ' '
067300060517      * tassazione
067400060518     c                   eval      RFTdivr = tasdiv
067500060518     c                   eval      RFTporr = taspor
067600060518      * carico eventuali varie presenti in TITA7
067700060518     c                   movea     sv            rft_svr
067800060518     c                   movea     va            rft_var
067900060518      *
068000060518     c                   eval      RFTimvr = tasimv
068100060522     c                   endif
068200060522   a1c                   endif
068300060522
068400060518     c                   eval      RFTela  = 'S'
068500060522     c                   eval      RFTnoj  = Knmeb
068600060517
068700060518     c                   UPDATE    WFRFT000
068800060518
068900060522      * faccio i totali imponibili e conto le spedizioni manca tariffa
069000060522
069100060522     c                   If        taserr <> ' '
069200060522     c                   add       1             prtnrmt
069300060522     c                   endif
069400060522
069500060522     c                   add       RFTimvf       prtimvf
069600060522     c                   add       RFTimvr       prtimvr
069700060518
069800060530      * differenza
069900060530     c                   eval      diffe = rftimvf - rftimvr
070000120921
070100120921      /free
070200120921       //?Metto in SK il trasporto + le varie
070300120921         exsr  Mem_Varie;
070400120921      /end-free
070500060530
070600060530      * STAMPO
070700060530     c                   If        not *in99 or *in90
070800060530      * testata
070900060530      * testata
071000060530     c                   write     sf41t0
071100060530     c                   write     sf41ts
071200060530     c                   write     sf41t3
071300060530     c                   write     sf41t1
071400060530     c                   write     sf41t3
071500060530
071600060530     c                   seton                                        99
071700060530     c                   setoff                                       90
071800060530
071900060530     c                   endif
072000060530
072100060530     c                   write     sf41d1
072200081006
072300081006      * suddivido i valori per scaglioni di !!! in base alla tipologia della tariffa
072400081006
072500081006     c                   If        i40sgl = 'S'
072600081006      * se tipo bolla fa parte delle bolle non di recupero le sommo scaglionate
072700081006      * altrimenti le metto nelle spedizioni di recupero
072800081006     c     tascbo        lookup    cbo_ok                                 30
072900081006     c                   if        %found
073000081006
073100081006     c                   z-add     1             yy
073200081006     c                   eval      ok_scagl = *off
073300081006     c                   select
073400081006      * quantità / valore
073500081006     c                   when      tipo_tar = 5 or tipo_tar = 4
073600081006     c                   z-add     tasqft        misura
073700081006     c                   exsr      ric_sgl
073800081006      * peso
073900081006     c                   when      tipo_tar = 0 or tipo_tar = 3
074000081006     c                   z-add     taspkf        misura
074100081006     c                   exsr      ric_sgl
074200081006      * colli
074300081006     c                   when      tipo_tar = 1
074400081006     c                   z-add     tasncl        misura
074500081006     c                   exsr      ric_sgl
074600081006      * spedizione
074700100302     c                   when      tipo_tar = 2
074800081006     c                   z-add     1             misura
074900081006     c                   exsr      ric_sgl
075000081006     c                   endsl
075100081006
075200081006      * se scaglione trovato valorizzo i campi delle schiere
075300081006     c                   If        ok_scagl
075400081006      * imponibile vecchio
075500081006     c                   add       rftimvf       imf(yy)
075600081006      * imponibile nuovo
075700081006     c                   add       rftimvr       imr(yy)
075800081006      * peso
075900081006     c                   add       taspkf        tkg(yy)
076000081006      * colli
076100081006     c                   add       tasncl        tcl(yy)
076200081006      * volume
076300081006     c                   add       tasvlf        tvl(yy)
076400081006      * spedizioni
076500081006     c                   add       1             nsp(yy)
076600081006      * manca tariffa
076700081006     c                   if        taserr <> ' '
076800081006     c                   add       1             nmt(yy)
076900081006     c                   endif
077000081006
077100081006     c                   endif
077200081006
077300081006      *** se bolla di recupero
077400081006     c                   else
077500081006      * imponibile vecchio
077600081006     c                   add       rftimvf       prtimfr
077700081006      * imponibile nuovo
077800081006     c                   add       rftimvr       prtimrr
077900081006      * spedizioni
078000081006     c                   add       1             prtnspr
078100081006      * manca tariffa
078200081006     c                   if        taserr <> ' '
078300081006     c                   add       1             prtnmtr
078400081006     c                   endif
078500081006      ***
078600081006     c                   endif
078700081006
078800081006     c                   endif
078900081006
079000060530
079100060518     c                   Endsr
079200060515
079300060519      *
079400060519      *---------------------------------------------------------------*
079500060519      *  Recupero importi di tassazione dal file titai
079600060519      *---------------------------------------------------------------*
079700060519     c     Ric_tai       Begsr
079800060519      *
079900060519     c                   z-add     0             k
080000060519
080100060519     c     kspe          setll     titai01l
080200060519
080300060519     c                   If        %equal(titai01l)
080400060519
080500060519     c                   do        *hival
080600060519
080700060522     c     kspe          reade     titai01l
080800060519
080900060519     c                   If        %eof(titai01l)
081000060519     c                   leave
081100060519     c                   endif
081200060519      * imponibile
081300060519     c                   if        taisvt = '£'
081400060519     c                   eval      tasimv = taivat
081500060519     c                   endif
081600060519      * porto
081700060519     c                   if        taisvt = ' '
081800060519     c                   eval      taspor = taivat
081900060519     c                   endif
082000060519      * varie
082100060519     c                   if        taisvt <> ' ' and taisvt <> '£'
082200060519     c                   add       1             k
082300060519     c                   eval      sv(k) = taisvt
082400060519     c                   eval      va(k) = taivat
082500060519     c                   endif
082600060519
082700060519     c                   enddo
082800060519
082900060519     c                   else
083000060519      * se non è pretassata pulisco la divisa
083100060519     c                   clear                   tasdiv
083200060519     c                   endif
083300070426      * se tassata fino l'imponibile valorizzo il campo tasman del prtf e il campo rfttmaf del file
083400070426     c                   if        tasimv = 0
083500070426     c                   clear                   tasman
083600070426     c                   else
083700070502     c                   eval      tasman = 'S'
083800070426      * faccio il totale dei tassati manualmente
083900070426     c                   add       1             prtnrtm
084000070426     c                   endif
084100070426
084200070426     c                   eval      rfttmaf = tasman
084300060519
084400060519     c                   endsr
084500060519      *---------------------------------------------------------------*
084600060519      ** Riverca Contrassegno
084700060519      *---------------------------------------------------------------*
084800060519     c     Carcsb        Begsr
084900060519
085000060519     c     Kcsb          chain     Tncsb03l
085100060519     c                   If        %found(tncsb03l) and
085200060519      * solo per stesso tipo bolla
085300060519     c                             tastbl = csbtbl
085400060519     c                   movel     csbsta        Tassta
085500060519     c                   z-add     csbcas        Tascas
085600061016     c*****              movel     csbtpa        Tastic
085700060519     c                   if        csbfus <> *blanks
085800060519     c                   move      csbfus        tastic
085900060519     c                   else
086000060519     c                   move      csbtpi        tastic
086100060519     c                   end
086200060519      * mittente
086300060519     c                   movel     csbvca        tasvca
086400060519     c                   z-add     csbcmb        tascmb
086500060519
086600060519     c                   endif
086700060519
086800060519     c                   endsr
086900060522      *---------------------------------------------------------------*
087000060522      ** Calcolo anteporto VARIA 'N'
087100060522      *---------------------------------------------------------------*
087200060522     c     Varian        Begsr
087300060522
087400060522      * richiamo il programma di tassazione
087500060522      * senza passare le varie e il porto
087600060522      *
087700060522      * mi salvo le ds di tassazione
087800060522     c                   eval      w_dtas = dtas
087900060522     c                   eval      w_dtasv = dtasv
088000060522      * mi salvo la divisa
088100060522     c                   eval      w_tasdiv = tasdiv
088200060522
088300110701      * tolgo il flag dell'invio mail al destinatario nell'eventualità ci fosse
088400110701     c                   eval      dtas01 = dstasflo
088500110701     c                   clear                   §asemd
088600110701     c                   eval      dstasflo = dtas01
088700110701
088800060522     c                   clear                   dtasv
088900060522     c                   clear                   tasdiv
089000060522     c                   clear                   taspor
089100060522
089200060522     c                   clear                   antepo
089300060522
089400060522     c                   call      'TNSF20R'
089500060522     c                   parm                    kpjba
089600060522     c                   parm                    dtas
089700060522     c                   parm                    dtasv
089800060522      * se non ci sono errori valorizzo la varia 'N'
089900060522     c                   if        taserr = ' '
090000060522     c                   z-add     tasimv        antepo
090100060522     c                   endif
090200060522      * ripristino le ds salvate prima
090300060522     c                   eval      dtas = w_dtas
090400060522     c                   eval      dtasv = w_dtasv
090500060522      * valorizzo l'anteporto nella varia N al posto di 88888888  se maggiore di zero
090600060522     c                   if        antepo > 0
090700060522     c                   z-add     antepo        va(xx)
090800060522     c                   endif
090900060522
091000060522     c                   endsr
091100060519      *---------------------------------------------------------------*
091200060519      ** CARICAMENTO codici bolla che prevedono di tassare solo 1 varia
091300060519      *   e che permettono imponibile =0
091400081006      ** CARICAMENTO codici bolla che non sono c/s o recuperi per
091500081006      *   scaglionare
091600060519      *---------------------------------------------------------------*
091700060519     c     Carcbo        Begsr
091800081006      **
091900081006     c                   Movel     'TB'          Cod
092000081006     c                   z-add     0             z
092100081006     c     Ktab          setll     tabel00f
092200081006     c                   do        *hival
092300081006     c     Ktab          reade     tabel00f
092400081006      * fine file
092500081006     c                   If        %eof(tabel00f)
092600081006     c                   leave
092700081006     c                   endif
092800081006    1
092900081006    2c                   If        tblkey <> *blanks
093000081006     c                   Movel     TBLuni        DStb
093100081006      * imponibile a zero
093200081006    3c                   If        §tbrbl  = 'N'
093300081006     c                   add       1             z
093400081006     c                   movel     TBLkey        tbo_oK(z)
093500081006    3c                   endif
093600081006      **
093700081006    2c                   endif
093800081006    1c                   enddo
093900081006
094000060519      **
094100060519     c                   Movel     '3A'          Cod
094200060519     c                   z-add     0             K
094300060519     c                   z-add     0             KK
094400060519     c     Ktab          setll     tabel00f
094500060519     c                   do        *hival
094600060519     c     Ktab          reade     tabel00f
094700060519      * fine file
094800060519     c                   If        %eof(tabel00f)
094900060519     c                   leave
095000060519     c                   endif
095100060519    1
095200060519    2c                   If        tblkey <> *blanks
095300060519     c                   Movel     TBLuni        DS3a
095400060519      * imponibile a zero
095500060519    3c                   If        §3aim0  = 'S'
095600060519     c                   add       1             K
095700060519     c                   movel     TBLkey        im0(k)
095800060519    3c                   endif
095900060519      * bolla con singola varia
096000060519     c                   If        §3asva  <> *blanks
096100060519     c                   add       1             Kk
096200060519     c                   movel     tblkey        cbo(kk)
096300060519     c                   movel     §3asva        cbv(kk)
096400060519    3c                   endif
096500081006      * bolla con tipo bolla normale
096600081006     c     §3atb1        lookup    tbo_ok                                 30
096700081006     c                   if        %found
096800081006     c                   add       1             zz
096900081006     c                   movel     tblkey        cbo_ok(zz)
097000081006     c                   endif
097100060519      **
097200060519    2c                   endif
097300060519    1c                   enddo
097400060519
097500060519     c                   Endsr
097600081006      *---------------------------------------------------------------*
097700081006      * Caricamento scaglioni
097800081006      *---------------------------------------------------------------*
097900081006     c     Car_scaglioni BEGSR
098000081006
098100081006     c                   eval      savprg = 999
098200081006     c                   clear                   yy
098300081006     c                   clear                   tipo_tar
098400081006     c                   clear                   num_sgl
098500081006      * cerco il giusto progressivo
098600081006
098700081006     c     ktam          setll     tntam01l
098800081006     c                   do        *hival
098900081006     c     ktam          reade     tntam01l
099000081006     c                   if        %eof(tntam01l)
099100081006     c                   leave
099200081006     c                   endif
099300081006      * verifico se progressivo annullato
099400081006     c                   if        tamatb <> ' '
099500081006     c                   iter
099600081006     c                   endif
099700081006      * controllo con la data di riferimento
099800120216     c                   If        rftdspr  < tamdst and rftdspr >= tamddt
099900081006     c                   eval      savprg = tamprg
100000081006     c                   leave
100100081006     c                   endif
100200081006     c                   if        rftdspr > tamdst
100300081006     c                   eval      savprg = tamprg
100400081006     c                   iter
100500081006     c                   endif
100600081006     c                   enddo
100700081006
100800081006     c                   if        savprg <> 999
100900081006      * trovato progressivo carico gli scaglioni relativi leggendo da titad
101000081006     c     ktad          setll     titad04l
101100110713     c                   do        *hival
101200081006     c     ktad          reade     titad04l
101300110713     c                   if        %eof(titad04l)
101400110713     c                   leave
101500110713     c                   endif
101600110713     c                   if        tadatb = ' '
101700081006      * recupero la chiave
101800081006     c                   eval      savlnp = tadlnp
101900081006     c                   eval      savorp = tadorp
102000081006     c                   eval      savnaz = tadnaz
102100081006     c                   eval      savcap = tadcap
102200110713     c                   leave
102300110713     c                   endif
102400110713     c                   enddo
102500081006
102600081006      * leggo tutti gli scaglioni con la stessa chiave
102700081006     c     ktad2         setll     titad04l
102800081006
102900081006     c                   do        *hival
103000081006     c     ktad2         reade     titad04l
103100081006
103200081006     c                   if        %eof(titad04l)
103300081006     c                   leave
103400081006     c                   endif
103500081006
103600081006     c                   if        tadatb <> ' '
103700081006     c                   iter
103800081006     c                   endif
103900081006
104000081006     c     tadsgl        lookup    sgl                                    31
104100081006
104200081006     c                   if        %found
104300081006     c                   leave
104400081006     c                   endif
104500081006
104600081006     c                   add       1             yy
104700081006     c                   eval      sgl(yy) = tadsgl
104800081006     c                   enddo
104900081006      * valorizzo il numero totali di scaglioni
105000081006     c                   z-add     yy            num_sgl
105100081006
105200081006     c                   movel     rftctrr       tipo_tar
105300081006
105400081006     c                   endif
105500081006      *
105600081006     c                   endsr
105700081006      *---------------------------------------------------------------*
105800081006      *  Ricerca dello scaglione per spedizione tassata
105900081006      *---------------------------------------------------------------*
106000081006     c     Ric_sgl       Begsr
106100081006
106200081006     c                   z-add     1             yy
106300081006
106400081006     c                   dow       yy <= num_sgl
106500081006     c                   if        misura <= sgl(yy)
106600081006     c                   eval      ok_scagl = *on
106700081006     c                   leave
106800081006     c                   endif
106900081006     c                   add       1             yy
107000081006     c                   enddo
107100081006
107200081006     c                   endsr
107300120921
107400120921      /free
107500120921       //--------------------------------------------------------------
107600120921       //?Memorizzo in SK il trasporto e le varie.
107700120921       //--------------------------------------------------------------
107800120921       BEGSR  Mem_Varie;
107900120921
108000120921       //?Il porto sempre al primo posto
108100120921         sk_Voce(1) = '.';
108200120921         IF  RFTporf > *zero;
108300120921           sk_ImpoF(1) += RFTporF;
108400120921           sk_ImpoR(1) += RFTporR;
108500120921         ENDIF;
108600120921
108700120921       //?Poi le varie
108800120924         IF  RFTva1F > *zero;
108900120921           Wvaria = RFTsv1F;
109000120921           WimpoF = RFTva1F;
109100120924           exsr  TotVariaF;
109200120924         ENDIF;
109300120924         IF  RFTva1R > *zero;
109400120924           Wvaria = RFTsv1R;
109500120921           WimpoR = RFTva1R;
109600120924           exsr  TotVariaR;
109700120921         ENDIF;
109800120921
109900120921         IF  RFTva2F > *zero;
110000120921           Wvaria = RFTsv2F;
110100120921           WimpoF = RFTva2F;
110200120924           exsr  TotVariaF;
110300120921         ENDIF;
110400120924         IF  RFTva2R > *zero;
110500120924           Wvaria = RFTsv2R;
110600120924           WimpoR = RFTva2R;
110700120924           exsr  TotVariaR;
110800120924         ENDIF;
110900120921
111000120921         IF  RFTva3F > *zero;
111100120921           Wvaria = RFTsv3F;
111200120921           WimpoF = RFTva3F;
111300120924           exsr  TotVariaF;
111400120921         ENDIF;
111500120924         IF  RFTva3R > *zero;
111600120924           Wvaria = RFTsv3R;
111700120924           WimpoR = RFTva3R;
111800120924           exsr  TotVariaR;
111900120924         ENDIF;
112000120921
112100120921         IF  RFTva4F > *zero;
112200120921           Wvaria = RFTsv4F;
112300120921           WimpoF = RFTva4F;
112400120924           exsr  TotVariaF;
112500120921         ENDIF;
112600120924         IF  RFTva4R > *zero;
112700120924           Wvaria = RFTsv4R;
112800120924           WimpoR = RFTva4R;
112900120924           exsr  TotVariaR;
113000120924         ENDIF;
113100120921
113200120921         IF  RFTva5F > *zero;
113300120921           Wvaria = RFTsv5F;
113400120921           WimpoF = RFTva5F;
113500120924           exsr  TotVariaF;
113600120921         ENDIF;
113700120924         IF  RFTva5R > *zero;
113800120924           Wvaria = RFTsv5R;
113900120924           WimpoR = RFTva5R;
114000120924           exsr  TotVariaR;
114100120924         ENDIF;
114200120921
114300120921         IF  RFTva6F > *zero;
114400120921           Wvaria = RFTsv6F;
114500120921           WimpoF = RFTva6F;
114600120924           exsr  TotVariaF;
114700120921         ENDIF;
114800120924         IF  RFTva6R > *zero;
114900120924           Wvaria = RFTsv6R;
115000120924           WimpoR = RFTva6R;
115100120924           exsr  TotVariaR;
115200120924         ENDIF;
115300120921
115400120921         IF  RFTva7F > *zero;
115500120921           Wvaria = RFTsv7F;
115600120921           WimpoF = RFTva7F;
115700120924           exsr  TotVariaF;
115800120921         ENDIF;
115900120924         IF  RFTva7R > *zero;
116000120924           Wvaria = RFTsv7R;
116100120924           WimpoR = RFTva7R;
116200120924           exsr  TotVariaR;
116300120924         ENDIF;
116400120921
116500120921         IF  RFTva8F > *zero;
116600120921           Wvaria = RFTsv8F;
116700120921           WimpoF = RFTva8F;
116800120924           exsr  TotVariaF;
116900120921         ENDIF;
117000120924         IF  RFTva8R > *zero;
117100120924           Wvaria = RFTsv8R;
117200120924           WimpoR = RFTva8R;
117300120924           exsr  TotVariaR;
117400120924         ENDIF;
117500120921
117600120921         IF  RFTva9F > *zero;
117700120921           Wvaria = RFTsv9F;
117800120921           WimpoF = RFTva9F;
117900120924           exsr  TotVariaF;
118000120921         ENDIF;
118100120924         IF  RFTva9R > *zero;
118200120924           Wvaria = RFTsv9R;
118300120924           WimpoR = RFTva9R;
118400120924           exsr  TotVariaR;
118500120924         ENDIF;
118600120921
118700120921         IF  RFTv10F > *zero;
118800120921           Wvaria = RFTs10F;
118900120921           WimpoF = RFTv10F;
119000120924           exsr  TotVariaF;
119100120921         ENDIF;
119200120924         IF  RFTv10R > *zero;
119300120924           Wvaria = RFTs10R;
119400120924           WimpoR = RFTv10R;
119500120924           exsr  TotVariaR;
119600120924         ENDIF;
119700120921
119800120921         IF  RFTv11F > *zero;
119900120921           Wvaria = RFTs11F;
120000120921           WimpoF = RFTv11F;
120100120924           exsr  TotVariaF;
120200120921         ENDIF;
120300120924         IF  RFTv11R > *zero;
120400120924           Wvaria = RFTs11R;
120500120924           WimpoR = RFTv11R;
120600120924           exsr  TotVariaR;
120700120924         ENDIF;
120800120921
120900120921         IF  RFTv12F > *zero;
121000120921           Wvaria = RFTs12F;
121100120921           WimpoF = RFTv12F;
121200120924           exsr  TotVariaF;
121300120921         ENDIF;
121400120924         IF  RFTv12R > *zero;
121500120924           Wvaria = RFTs12R;
121600120924           WimpoR = RFTv12R;
121700120924           exsr  TotVariaR;
121800120924         ENDIF;
121900120921
122000120921         IF  RFTv13F > *zero;
122100120921           Wvaria = RFTs13F;
122200120921           WimpoF = RFTv13F;
122300120924           exsr  TotVariaF;
122400120921         ENDIF;
122500120924         IF  RFTv13R > *zero;
122600120924           Wvaria = RFTs13R;
122700120924           WimpoF = RFTv13R;
122800120924           exsr  TotVariaR;
122900120924         ENDIF;
123000120921
123100120921         IF  RFTv14F > *zero;
123200120921           Wvaria = RFTs14F;
123300120921           WimpoF = RFTv14F;
123400120924           exsr  TotVariaF;
123500120921         ENDIF;
123600120924         IF  RFTv14R > *zero;
123700120924           Wvaria = RFTs14R;
123800120924           WimpoR = RFTv14R;
123900120924           exsr  TotVariaR;
124000120924         ENDIF;
124100120921
124200120921         IF  RFTv15F > *zero;
124300120921           Wvaria = RFTs15F;
124400120921           WimpoF = RFTv15F;
124500120924           exsr  TotVariaF;
124600120921         ENDIF;
124700120924         IF  RFTv15R > *zero;
124800120924           Wvaria = RFTs15R;
124900120924           WimpoR = RFTv15R;
125000120924           exsr  TotVariaR;
125100120924         ENDIF;
125200120921
125300120921         IF  RFTv16F > *zero;
125400120921           Wvaria = RFTs16F;
125500120921           WimpoF = RFTv16F;
125600120924           exsr  TotVariaF;
125700120921         ENDIF;
125800120924         IF  RFTv16R > *zero;
125900120924           Wvaria = RFTs16R;
126000120924           WimpoR = RFTv16R;
126100120924           exsr  TotVariaR;
126200120924         ENDIF;
126300120921
126400120921         IF  RFTv17F > *zero;
126500120921           Wvaria = RFTs17F;
126600120921           WimpoF = RFTv17F;
126700120924           exsr  TotVariaF;
126800120921         ENDIF;
126900120924         IF  RFTv17R > *zero;
127000120924           Wvaria = RFTs17R;
127100120924           WimpoR = RFTv17R;
127200120924           exsr  TotVariaR;
127300120924         ENDIF;
127400120921
127500120921         IF  RFTv18F > *zero;
127600120921           Wvaria = RFTs18F;
127700120921           WimpoF = RFTv18F;
127800120924           exsr  TotVariaF;
127900120921         ENDIF;
128000120924         IF  RFTv18R > *zero;
128100120924           Wvaria = RFTs18R;
128200120924           WimpoR = RFTv18R;
128300120924           exsr  TotVariaR;
128400120924         ENDIF;
128500120921
128600120921         IF  RFTv19F > *zero;
128700120921           Wvaria = RFTs19F;
128800120921           WimpoF = RFTv19F;
128900120924           exsr  TotVariaF;
129000120921         ENDIF;
129100120924         IF  RFTv19R > *zero;
129200120924           Wvaria = RFTs19R;
129300120924           WimpoR = RFTv19R;
129400120924           exsr  TotVariaR;
129500120924         ENDIF;
129600120921
129700120921         IF  RFTv20F > *zero;
129800120921           Wvaria = RFTs20F;
129900120921           WimpoF = RFTv20F;
130000120924           exsr  TotVariaF;
130100120921         ENDIF;
130200120924         IF  RFTv20R > *zero;
130300120924           Wvaria = RFTs20R;
130400120924           WimpoR = RFTv20R;
130500120924           exsr  TotVariaR;
130600120924         ENDIF;
130700120921
130800120921       ENDSR;
130900120921
131000120921       //--------------------------------------------------------------
131100120924       //?Incremento totali fatturato per voce in schiera.                       ?
131200120921       //--------------------------------------------------------------
131300120924       BEGSR  TotVariaF;
131400120921
131500120921         ww = %lookup( Wvaria : sk_Voce : 2 );
131600120921         IF  ww = *zero;
131700120921           ww = %lookup( *blank : sk_Voce : 2 );
131800120921           IF  ww > *zero;
131900120921             sk_Voce(ww) = Wvaria;
132000120921           ENDIF;
132100120921         ENDIF;
132200120921
132300120921         sk_ImpoF(ww) += WimpoF;
132400120921
132500120921       ENDSR;
132600120924
132700120924       //--------------------------------------------------------------
132800120924       //?Incremento totali per voce rifatturato in schiera.                       ?
132900120924       //--------------------------------------------------------------
133000120924       BEGSR  TotVariaR;
133100120924
133200120924         ww = %lookup( Wvaria : sk_Voce : 2 );
133300120924         IF  ww = *zero;
133400120924           ww = %lookup( *blank : sk_Voce : 2 );
133500120924           IF  ww > *zero;
133600120924             sk_Voce(ww) = Wvaria;
133700120924           ENDIF;
133800120924         ENDIF;
133900120924
134000120924         sk_ImpoR(ww) += WimpoR;
134100120924
134200120924       ENDSR;
134300120924
134400120924      /end-free
134500081006      *
134600060519      *---------------------------------------------------------------*
134700060519      * Operazioni Iniziali                                           *
134800060519      *---------------------------------------------------------------*
134900060519     c     RoutInz       BEGSR
135000060519      *
135100060519      * Reperisco dati job
135200060519     c                   exsr      DatiJob
135300060519      * Reperisco data e ora
135400060519     c                   time                    w0140
135500060519     c                   movel     w0140         wora
135600060522     c                   move      w0140         wdate
135700060519      *
135800060801      * verifico se sono in filiale faccio le ovrdbf per i file di sede
135900060801     c                   If        dutlpo <> 'S'
136000060801      * librerie
136100060801
136200060801      * Se sono in ambiente buono - GAITRAGRU
136300060801     c                   If        knsif = 'FILTRA201 '
136400060801     c                   Eval      wlib = 'GAITRAGRU '
136500060801     c                   Eval      wlib1 = 'GAITRAAZM '
136600060801      *  Se sono in ambiente di prova - GAITRAGRPS
136700060801     c                   Else
136800060801     c                   Eval      wlib = 'GAITRAGRPS'
136900060801     c                   Eval      wlib1 = 'GAITRAAZM '
137000060801     c                   EndIf
137100060801      * WFRFT01L
137200060801     c                   Clear                   comman
137300060801     c                   Movel(p)  cmdt(1)       comman
137400060801     c                   Eval      %Subst(comman:30:10) = wlib1
137500060801     c                   Eval      comman =
137600060801     c                             %trim(comman) + '/WFRFT01L)'
137700060801     c                   Call      'QCMDEXC'                            99
137800060801     c                   Parm                    comman
137900060801     c                   Parm                    lenght
138000060801
138100060801      * TITAI01L
138200060801     c                   Clear                   comman
138300060801     c                   Movel(p)  cmdt(2)       comman
138400060801     c                   Eval      %Subst(comman:30:10) = wlib
138500060801     c                   Eval      comman =
138600060801     c                             %trim(comman) + '/TITAI01L)'
138700060801     c                   Call      'QCMDEXC'                            99
138800060801     c                   Parm                    comman
138900060801     c                   Parm                    lenght
139000060801
139100060801      * TITAS30C
139200060801     c                   Clear                   comman
139300060801     c                   Movel(p)  cmdt(3)       comman
139400060801     c                   Eval      %Subst(comman:30:10) = wlib
139500060801     c                   Eval      comman =
139600060801     c                             %trim(comman) + '/TITAS30C)'
139700060801     c                   Call      'QCMDEXC'                            99
139800060801     c                   Parm                    comman
139900060801     c                   Parm                    lenght
140000060801      * FIAR531C
140100060801     c                   Clear                   comman
140200060801     c                   Movel(p)  cmdt(4)       comman
140300060801     c                   Eval      %Subst(comman:30:10) = wlib
140400060801     c                   Eval      comman =
140500060801     c                             %trim(comman) + '/FIAR531C)'
140600060801     c                   Call      'QCMDEXC'                            99
140700060801     c                   Parm                    comman
140800060801     c                   Parm                    lenght
140900060801      * TNCSB03L
141000060801     c                   Clear                   comman
141100060801     c                   Movel(p)  cmdt(5)       comman
141200060801     c                   Eval      %Subst(comman:30:10) = wlib
141300060801     c                   Eval      comman =
141400060801     c                             %trim(comman) + '/TNCSB03L)'
141500060801     c                   Call      'QCMDEXC'                            99
141600060801     c                   Parm                    comman
141700060801     c                   Parm                    lenght
141800060801
141900060801     c                   endif
142000060801      *
142100060801     c                   open      wfrft01l
142200060801     c                   open      titai01l
142300060801     c                   open      titas30c
142400060801     c                   open      fiar531c
142500060801     c                   open      tncsb03l
142600060801
142700060519     c                   ENDSR
142800060519      *
142900060519      *---------------------------------------------------------------*
143000060519      * Reperimento Dati del job (Utente/Operativi)                   *
143100060519      *---------------------------------------------------------------*
143200060519     c     DatiJob       BEGSR
143300060519      *
143400060519     c     *dtaara       define    §azute        azuteds
143500060519     c     *dtaara       define    §datiute      ddatiute
143600060519      *
143700060519     c                   in(E)     *dtaara
143800060519     c                   IF        %ERROR or RSUT = *blanks
143900060519     c                   clear                   Tibs34Ds
144000060519     c                   call      'TIBS34R'
144100060519     c                   parm                    Tibs34Ds
144200060519     c                   in        *dtaara
144300060519     c                   Endif
144400060519
144500060519      *
144600060519     c                   ENDSR
144700060801** cmdt
144800060801OVRDBF FILE(WFRFT01L) TOFILE(
144900060801OVRDBF FILE(TITAI01L) TOFILE(
145000060801OVRDBF FILE(TITAS30C) TOFILE(
145100060801OVRDBF FILE(FIAR531C) TOFILE(
145200060801OVRDBF FILE(TNCSB03L) TOFILE(
145300060801DLTOVR FILE(*ALL)
