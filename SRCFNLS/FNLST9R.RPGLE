000100110620     /*PRM dbgview(*source)
000200040107      *---------------------------------------------------------------*
000300130711      * Stampa Etichette documenti in partenza /Scheda di trasporto
000400040107      *---------------------------------------------------------------*
000500040107
000600040108     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000700090528     h dftactgrp(*no) actgrp(*caller)
000800040107
000900040107      *---------------------------------------------------------------*
001000040107
001100111202     ftabel00f  if   e           k disk
001200090922     fazorg01l  if   e           k disk
001300130529     ffnfgv01l  if   e           k disk    infds(fnfgvinf)
001400130529     ffnfgv02l  if   e           k disk    infds(fnfgvinf2)
001500130529     f                                     rename(fnfgv000:fnfgv2)
001600130529     ffnfgv00f  uf   e             disk    rename(fnfgv000:fnfgvf)
001700130612     ffnfgs01l  if   e           k disk
001800130524     ffnfv201l  if   e           k disk
001900130620     ffnfge01l  if   e           k disk
002000130524     ffnlst9d   cf   E             workstn SFILE(LST9S01:s01NRR)
002100040112      *
002200040107      *---------------------------------------------------------------*
002300040107      *
002400040107      *   S C H I E R E   - - - - - - - - - - - - - - - - - - - - - - *
002500040107      *
002600130620     d err             s             79    dim(10) ctdata perrcd(1)
002700130524     D sfv             S              5    DIM(199) inz
002800040107      *
002900040107      *   D S   - - - - - - - - - - - - - - - - - - - - - - - - - - - *
003000040107      *
003100130528     D*
003200130528     D PARAM1          DS
003300130711     d  pa1filtro              1      1
003400130528     D* MODULO STAMPA TABULATI
003500130528     D  PA1MDT               227    236
003600130528     D* PGM STAMPA ETICHETTE
003700130528     D  PA1PSE               237    246
003800130528     D* STAMPANTE TABULATI - X FV
003900130528     D  PA1PRT               247    256
004000130528
004100130612     D* PASSO DATI A STAMPA FV                           -FNLS94C1
004200130528     D PARAM2          DS
004300130528     D* VIDNFV                11     15
004400130528     D  wnfv                  11     15
004500130528     D* FNLS94R -->  ' 'STAMPA     'R'RSTAMPA
004600130528     D  PA2FLG                16     16
004700130528     D* SE PA2MSG PIENO --> C'E' STATO ERRORE
004800130528     D  PA2MSG                17     94
004900130528     D* PARF12= 'S'     --> PREMUTO F12
005000130528     D  PA2F12               102    102
005100130528     D  PA2FGS               103    105
005200130528     D* STAMPATE E MODULO PER STAMPA FV
005300130528     D  PA2PRT               164    173
005400130528     D  PA2MDT               174    183
005500130612     D  PA2cpy               184    184  0
005600130528
005700130527     D fnlst4ds        DS
005800130527     D* ilst4gES = M - MANUTENZIONE FV RICHIAMATA
005900130527     D*          B - DA BORDERO'
006000130527     D*          A - apertura foglio
006100130527     D*          C - DA CHIUSURA FV SINGOLA
006200130527     D*          I - DA CHIUSURA DA IMP
006300130527     D*          N - DA INTERROGAZIONE  FV
006400130527     D*          M - Manutenzione FGV
006500130527     D*          P - Manutenzione FGV con videata piombi
006600130527
006700130527     D* CAMPI INPUT OBBLIGATORI:
006800130527     D*  GES / FGS
006900130527     d
007000130527     D* CAMPI INPUT PER MANUTENZIONE :
007100130527     D*  NFV
007200130527     D*
007300130527     D* CAMPI INPUT SOLO PER APERTURA :
007400130527     d* DFV /OLD / TRN / NRRP / PDR
007500130527     D
007600130527     D  ilst4DFV               3     10
007700130527     D
007800130527     D  ilst4nfv              11     15
007900130527     D  IlsT4GES              16     16
008000130527     D* Se MSG pieno --> c'è stato ERRORE
008100130527     D  olst4msg              17     94
008200130527     d* se iolst4FOR è pieno --> premuto F6 per forzare foglio di traini doppio
008300130527     D  iolst4FOR             95     97
008400130527     D
008500130527     D  IlsT4OLD              98     98
008600130527     D* OLST4F12 = 'S'    --> premuto F12
008700130527     D  olst4F12             102    102
008800130527     d
008900130527     D  ilst4fgs             103    105
009000130527     D  ilst4TRN             106    112  0
009100130527     D  ilst4NRRP            113    125
009200130527     D* se non immessa il numero relativo record della proposta
009300130527     d*  si può immettere a modo vecchio il trazionista
009400130527     D  ilst4PDR             126    132
009500130527     D*
009600130524     d fnlst3ds        ds
009700130524     D  PA3FLG                 1      1    INZ('R')
009800130524     D  PA3FL2                 2      2    INZ
009900130524     D  PA3NFV                 3      5P 0 INZ
010000130524     D  PA3DIN                 6     10P 0 INZ
010100130524     D  PA3DFI                11     15P 0
010200130524     D  PA3TFV                16     16    INZ('2')
010300130524     D* CAMPO USATO PER PASSARE AL PGM CHIAMANTE TUTTI I FOGLI VIAGGIO
010400130524     D  FVA                   17    241P 0 DIM(45)
010500130524     D  PA3FGS               242    244  0
010600130524
010700130612     d dstv          e ds
010800130612     d TIBS02ds      e ds                  inz
011000130612     D dvpo          E DS
011100130530     d trul82ds      e ds
011200130529     d fnlst9ds      e ds
011300090922     d Tibs34ds      e ds                  inz
011400090922     d §azute        e ds                  Extname(Azute00f)
011500090922     d                                     dtaara
011600090922     d §Datiute      e ds                  EXTname(ddatiute)
011700130524     d                                     dtaara
011800040107     d KPJBA         e ds
011900130529
012000130529     d dfgvdcn       e ds
012100130529
012200130529     D fnfgvinf        ds
012300130529     D  fgvnrr               397    400i 0
012400130529
012500130529     D fnfgvinf2       ds
012600130529     D  fgvnrr2              397    400i 0
012700040123      *
012800040109      *
012900090409
013000040107      *
013100040107      *   V A R I A B I L I   - - - - - - - - - - - - - - - - - - - - *
013200040107      *
013300040213      * - Indici di schiera / Contatori
013400090827     d i               s              3  0 inz
013500130527     d Xi              s              3  0 inz
013600130527     d c_Digits        c                   const('0123456789')
013700040109      *
013800040109      * - Campi di comodo
013900111205     d dataiso         s               d   datfmt(*iso)
014000111205     d dataeur         s               d   datfmt(*eur)
014100111205     d datacur         s               d   datfmt(*iso)
014200130524     d wdate           s              8  0
014300130527     D NUM5            s              5  0 inz
014400130527     d comnfv          s                   like(fgvnfv)
014500130527     d wselfv          s              1
014600130528     d wprimo          s              1
014700130529     d wnrr            s                   like(vh1nrr)
014800130620     d wfge            s              1
014900130620     d wdfv            s                   like(fgvdfv)
015000111202      // - Campi associati al video
015100111206     d s01nrr          s              4  0 inz
015200130524     d savnrr          s              4  0 inz
015300130524     d vc1dfvinv       s                   like(vc1dfv)
015400130612     D alfaFGS         S              3    INZ
015500090828      *
015600090828      * - Flags booleani
015700090828     d $Finerec        s              1    inz(*off)
015800111202     d $Fine           s               n   inz(*off)
015900111207     d $End            s               n   inz(*off)
016000111202     d $InzS01         s               n   inz(*on)
016100130529
016200130529     D msgjob          C                   CONST('Si sta bloccando la stampa sc-
016300130529     D                                     heda di trasporto: USCIRE un momento-
016400130529     D                                      da questo lavoro')
016500130524
016600130524      // -
016700130524     d fnlst3r         pr                  extpgm('FNLST3R')
016800130524     d  kpjba                              likeds(kpjbA)
016900130527     d fnlst4r         pr                  extpgm('FNLST4R')
017000130527     d  kpjba                              likeds(kpjbA)
017100130612     d fnls94c1        pr                  extpgm('FNLS94C1')
017200130528     d  kpjba                              likeds(kpjbA)
017300130528     d fnlst9r1        pr                  extpgm('FNLST9R1')
017400130528     d  kpjba                              likeds(kpjbA)
017500130529     d trul82r         pr                  extpgm('TRUL82R')
017600130529     d  trul82ds                           likeds(trul82ds)
017700111205
017800081023
017900090402      // ? PROTOTIPI ?
018000090402      /copy gaitrasrc/srcprotopr,tibs34r
018100130612      /copy gaitrasrc/srcprotopr,tibs02r
018200040107
018300040107      *===============================================================*
018400040107      *  RIEPILOGO INDICATORI                                         *
018500040107      *---------------------------------------------------------------*
018600090827      *  XX    -
018700040107      *===============================================================*
018800040107
018900040107     c     *Entry        plist
019000040107     c                   parm                    KPJBA
019100130528     C                   MOVEL     KPJBU         PARAM1
019200130528     C* MUOVO STAMPANTI E MODULO ETICHETTA
019300130528     C                   MOVEL     PA1PSE        lst9pgmse
019400130711     c* memorizzo se si tratta di stampa etichetta o scheda trasporto
019500130711     c                   if        pa1filtro=' '
019600130711     c* scheda trasporto
019700130711     c                   setoff                                       10
019800130711     c                   else
019900130711     c* etichette
020000130711     c                   seton                                        10
020100130711     c                   endif
020200090827     c
020300040123      *
020400090331      /free
020500130524       // Inizializzazione video
020600130524         exsr inzs01;
020700130524       //?Gestione video
020800111202       DOW $Fine = *off;
020900130524         exsr GesC01;
021000111202       ENDDO;
021100090610
021200090827       eval *inlr=*on;
021300130524
021400130524       //-------------------------------------------------------------*
021500130524       Begsr GesC01;
021600130524
021700130524       //?Visualizzazione del SFL (se ci sono dati)
021800130524         *in20 = (S01nrr > *zeros);
021900130524          write lst9z01;
022000130524          exfmt lst9c01;
022100130524          *in28=*off;
022200130612          *in90=*off;
022300111206             select;
022400130902             // F3=Fine
022500130902                when *inkc;
022600111206                   $fine=*on;
022700130524             // Rollup
022800130524                when *in73;
022900130524                   s01nrr=savnrr;
023000130524                   chain s01nrr lst9s01;
023100130524                   if %found and (vs1nfv=*zeros or vs1nfv=*blanks);
023200130524                      *in28=*on;
023300130524                      vidmsg=err(3);
023400130524                   else;
023500130524                      exsr ries01;
023600130524                   endif;
023700130524                other;
023800130612                   exsr ctrc01;
023900130612                   exsr ctrsfl;
024000130612                   if *inkf and not *in90 and not *in91;
024100130528                      exsr sr_lancio;
024200130524                      $fine= *on;
024300111206                   endif;
024400111206             endsl;
024500111206       endsr;
024600111206       //--------------------------------------------------------------
024700111206       BEGSR InzS01;
024800111206
024900111206         *in21   = *off;
025000111206         *in23   = *off;
025100130524         write  LST9C01;
025200111206         *in21   = *on;
025300111206
025400130711         if *in10  ;
025500130711         vc1ste='S';
025600130711         vc1sts='N';
025700130711         else  ;
025800130711         vc1ste='N';
025900130711         vc1sts='S';
026000130711         endif  ;
026100130524         clear vc1dfv;
026200130524         clear vc1fog;
026300111206         clear S01nrr;
026400130524         *in41=*on;
026500111206
026600111206       //?Caricamento subfile
026700111206         exsr Ries01;
026800111206
026900111206         *in23 = *on;
027000111206
027100111206       ENDSR;
027200111206
027300111206       //--------------------------------------------------------------
027400111206       //?Riempimento S01.
027500111206       //--------------------------------------------------------------
027600111206       BEGSR RieS01;
027700130528
027800130528        *in42=*off;
027900130528        *in44=*off;
028000130528        *in04=*off;
028100130528
028200130527        for I=1 to 9;
028300111206           S01nrr += 1;
028400130528           exsr clr_f_sfl;
028500130524           write  lst9s01;
028600130524        endfor;
028700130524           rec=s01nrr;
028800130524           savnrr=s01nrr;
028900111206
029000111206       ENDSR ;
029100130524       //--------------------------------------------------------------
029200130524       //?Controlli control e sfl
029300130524       //--------------------------------------------------------------
029400130524       BEGSR CTRC01;
029500130524
029600130612          *in40=*off;
029700130612          *in43=*off;
029800130612          *in45=*off;
029900130612       // errore se non richiesta nè stampa etichette nè stampa scheda
030000130612       if vc1ste<>'S' and vc1sts<>'S';
030100130612          *in28=*on;
030200130612          *in45=*on;
030300130612          *in90=*on;
030400130612          vidmsg=err(9);
030500130612          leavesr;
030600130612       endif;
030700130524       // Verifico se selezionati fogli nel sfl
030800130524       exsr ctrsflA;
030900130524       // richiesta: O per data foglio O per numero foglio
031000130524       if (vc1dfv>0 and wselfv='S') or (vc1dfv<=0 and wselfv=' ');
031100130524          *in28=*on;
031200130612          *in40=*on;
031300130524          *in90=*on;
031400130524          vidmsg=err(2);
031500130524          leavesr;
031600130524       endif;
031700130524
031800130524       // Richiesta per data foglio: controllo la data
031900130524       if vc1dfv>0;
032000130527          *in41=*on;
032100130524       if %subst(%editc(vc1dfv:'X'):1:2)='00';
032200130524          test(DE) *dmy vc1dfv;
032300130524          if %error;
032400130524             *in90=*on;
032500130524             *in40=*on;
032600130524             *in28=*on;
032700130524             vidmsg=err(1);
032800130524             leavesr;
032900130524          else;
033000130524             dataiso=%date(vc1dfv:*dmy);
033100130524          endif;
033200130524       else;
033300130524          test(DE) *eur vc1dfv;
033400130524          if %error;
033500130524             *in90=*on;
033600130524             *in40=*on;
033700130524             *in28=*on;
033800130524             vidmsg=err(1);
033900130524             leavesr;
034000130524          else;
034100130524             dataiso=%date(vc1dfv:*eur);
034200130524          endif;
034300130524       endif;
034400130524          dataeur=dataiso;
034500130524          vc1dfv=%dec(dataeur);
034600130524          vc1dfvinv=%dec(dataiso);
034700130620
034800130620       // errore se per la data richiesta non sono presenti linee nel
034900130620       // file di appoggio
035000130620
035100130620          if vc1ste='S';
035200130620             wdfv=vc1dfvinv;
035300130620             exsr sr_fnfge;
035400130620             if wfge='N';
035500130620                *in90=*on;
035600130620                *in40=*on;
035700130620                *in28=*on;
035800130620                vidmsg=err(10);
035900130620                leavesr;
036000130620             endif;
036100130620          endif;
036200130527       // errore se non selezionato il tipo di fogli
036300130612       // if vc1fog <> 'T' and vc1fog<>'S';
036400130612       //    *in90=*on;
036500130612       //    *in43=*on;
036600130612       //    *in28=*on;
036700130612       //    vidmsg=err(5);
036800130612       //    leavesr;
036900130612       // endif;
037000130612       endif;
037100130524
037200130524       ENDSR ;
037300040108
037400130524       //--------------------------------------------------------------
037500130524       //?Controllo sfl se immesso almeno un foglio
037600130524       //--------------------------------------------------------------
037700130524       BEGSR ctrsflA;
037800130524       clear wselfv;
037900130524       for i=1 to savnrr;
038000130524          chain i lst9s01;
038100130524          if vs1nfv<>*zeros and vs1nfv<>*blanks;
038200130524             wselfv='S';
038300130527             leavesr;
038400130524          endif;
038500130524       endfor;
038600130524       endsr;
038700130524       //--------------------------------------------------------------
038800130524       //?Controllo sfl
038900130524       //--------------------------------------------------------------
039000130524       BEGSR ctrsfl;
039100130612       if vc1dfv=*zeros;
039200130612          *in41=*off;
039300130612       endif;
039400130612       *in91=*off;
039500130524       *in42=*off;
039600130524       *in44=*off;
039700130528       *in04=*off;
039800130528       wprimo=' ';
039900130524       clear sfv;
040000130524       s01nrr=1;
040100130524       chain s01nrr lst9s01;
040200130524       dow %found;
040300130524
040400130524       // Ricerca foglio
040500130612        if not*in90;
040600130524          if %scan('?':vs1nfv)>0;
040700130524             exsr ricnfv;
040800130612             *in91 = *on;
040900130524             leavesr;
041000130524          endif;
041100130524
041200130524       // Controllo numero foglio immesso
041300130524          if vs1nfv<> *zeros and vs1nfv<>*blanks;
041400130524             exsr ctrnfv;
041500130612             if *in91;
041600130524                *in42=*on;
041700130524                update lst9s01;
041800130524                leavesr;
041900130524             endif;
042000130527
042100130527             exsr decod;
042200130524
042300130524       // errore se numero foglio già presente nel sfl
042400130524             if %lookup(vs1nfv:sfv)>0;
042500130524                eval *in42=*on;
042600130612                eval *in91=*on;
042700130524                eval *in28=*on;
042800130524                vidmsg=err(4);
042900130524                update lst9s01;
043000130524                leavesr;
043100130524             endif;
043200130524             if s01nrr<=199;
043300130524                sfv(s01nrr)=vs1nfv;
043400130524             endif;
043500130524          // OPZIONE 1 --> Richiamo la manutenzione del foglio viaggio
043600130524             if vs1sce='1';
043700130524                vs1sce=' ' ;
043800130524                exsr fviaggio;
043900130524                update  lst9s01;
044000130527                if olst4msg<>*blanks;
044100130527                eval *in42=*on;
044200130612                eval *in91=*on;
044300130527                eval *in28=*on;
044400130527                vidmsg=olst4msg;
044500130527                update lst9s01;
044600130527                endif;
044700130524                leavesr;
044800130524             endif;
044900130620          // errore se nella data foglio non sono presenti linee nel
045000130620          // file di appoggio
045100130620             if vc1ste='S' and fgvttr<>'I' and vc1sts<>'S';
045200130620                wdfv=fgvdfv;
045300130620                exsr sr_fnfge;
045400130620                if wfge='N';
045500130620                   *in91=*on;
045600130620                   *in42=*on;
045700130620                   *in28=*on;
045800130620                   vidmsg=err(10);
045900130620                   update lst9s01;
046000130620                   leavesr;
046100130620                endif;
046200130620             endif;
046300130528          else;
046400130528             exsr clr_f_sfl;
046500130528             if wprimo=*blanks;
046600130612                if not *in45 and not *in41;
046700130612                   *in04=*on;
046800130612                endif;
046900130528                wprimo='1';
047000130528             endif;
047100130524          endif;
047200130612          endif;
047300130528          update lst9s01;
047400130524          s01nrr+=1;
047500130524          chain s01nrr lst9s01;
047600130524       enddo;
047700130524       endsr;
047800130528       //-------------------------------------------------------------*
047900130528       //pulizia campi sfl                                            *
048000130528       //-------------------------------------------------------------*
048100130528       begsr clr_f_sfl;
048200130528             clear vs1nfv;
048300130528             clear vs1sce;
048400130528             clear vs1lna;
048500130528             clear vs1dlna;
048600130528             clear vs1dfv;
048700130528             clear vs1hpa;
048800130529             clear vh1nrr;
048900130612             clear vh1ttr;
049000130528       endsr;
049100130527       //-------------------------------------------------------------*
049200130527       //decodifica campi sfl                                         *
049300130527       //-------------------------------------------------------------*
049400130527       begsr decod ;
049500130529       // Nrr
049600130529             vh1nrr=fgvnrr;
049700130527       // lna
049800130527             vs1lna=fgvlna;
049900130528             clear vs1dlna;
050000130527             chain (fgvlna) azorg01l;
050100130527             if %found(azorg01l);
050200130527                vs1dlna=orgdes;
050300130527             endif;
050400130527       // Data foglio
050500130527             dataeur=%date(fgvdfv:*iso);
050600130527             vs1dfv=%dec(%subst(%editc(%dec(dataeur:*eur):'X'):1:4):4:0);
050700130527       // ora partenza: quella reale se presente altrimenti quella teorica
050800130527       //               se presente
050900130528             clear vs1hpa;
051000130529             if fgvhmp>0;
051100130529                vs1hpa=fgvhmp;
051200130527             else;
051300130527                chain (fgvlnp: fgvnfv: 'P': 0: 'T') fnfv201l;
051400130527                if %found(fnfv201l);
051500130527                   vs1hpa=fv2hpa;
051600130527                endif;
051700130527             endif;
051800130612             vh1ttr=fgvttr;
051900130612             vh1def=§tvdef;
052000130527       endsr;
052100130524       //-------------------------------------------------------------*
052200130524       //Richiamo ricerca numero foglio                               *
052300130524       //-------------------------------------------------------------*
052400130524       begsr ricnfv;
052500130524       reset fnlst3ds;
052600130524       pa3fl2='F';
052700130524       pa3dfi=wdate;
052800130524       pa3fgs=simfel;
052900130524       kpjbu=fnlst3ds;
053000130524       fnlst3r(kpjba);
053100130524       fnlst3ds=kpjbu;
053200130524
053300130524       // se selezionati fv li scrivo nel sfl
053400130524       *in04=*off;
053500130524       clear vs1nfv;
053600130524       update lst9s01;
053700130524       s01nrr-=1;
053800130528       xi=1;
053900130528       dow XI<=45 and fva(xi)<>*zeros;
054000130524          s01nrr+=1;
054100130524          chain s01nrr lst9s01;
054200130527       // Se necessario creo nuova pagina di sfl
054300130524          if not %found;
054400130524             num5=s01nrr;
054500130524             s01nrr-=1;
054600130527             exsr ries01;
054700130524             s01nrr=num5;
054800130524             chain s01nrr lst9s01;
054900130524          endif;
055000130524          if vs1nfv=*zeros or vs1nfv=*blanks;
055100130527             vs1nfv=%subst(%editc(fva(Xi):'X'):1:5);
055200130527             comnfv=%dec(vs1nfv:5:0);
055300130527             vs1lna=%dec(%subst(%editc(fva(Xi):'X'):6:3):3:0);
055400130529             chain (comnfv: simfel) fnfgv01l;
055500130612             exsr sr_dstv;
055600130527             exsr decod;
055700130527             update lst9s01;
055800130528             xi+=1;
055900130524          endif;
056000130528       enddo;
056100130528
056200130528       // Posiziono il cursore sul primo record successivo a quello di
056300130528       // immissione
056400130528       select;
056500130528       when s01nrr<1;
056600130528          s01nrr=1;
056700130528       other;
056800130528          if (s01nrr+1)<=savnrr;
056900130528             s01nrr+=1;
057000130528          endif;
057100130528          chain s01nrr lst9s01;
057200130528          if %found;
057300130528             *in04=*on;
057400130528             update lst9s01;
057500130528          endif;
057600130528       endsl;
057700130528       *in04=*off;
057800130528       endsr;
057900130524       //-------------------------------------------------------------*
058000130524       //Richiamo manutenzione foglio viaggio                         *
058100130524       //-------------------------------------------------------------*
058200130524       begsr fviaggio;
058300130527       clear fnlst4ds;
058400130527       ilst4ges='B';
058500130527       ilst4fgs=%editc(simfel:'X');
058600130527       ilst4nfv=vs1nfv;
058700130527       kpjbu=fnlst4ds;
058800130527       fnlst4r(kpjba);
058900130527       fnlst4ds=kpjbu;
059000130524       endsr;
059100130527       //-------------------------------------------------------------*
059200130527       //Controllo foglio viaggio                                     *
059300130527       //-------------------------------------------------------------*
059400130527       begsr ctrnfv  ;
059500130527
059600130527       // deve contenere solo numeri
059700130527       if %check (c_Digits:vs1nfv)<>0;
059800130527          *in28=*on;
059900130612          *in91=*on;
060000130527          vidmsg=err(6);
060100130527          leavesr;
060200130527       endif;
060300130612
060400130527
060500130527       // Verifica esistenza del foglio su fnfgv
060600130527       comnfv=%dec(vs1nfv:5:0);
060700130529       chain (comnfv: simfel) fnfgv01l;
060800130612       if %found(fnfgv01l) and fgvatb=*blanks;
060900130612          exsr sr_dstv;
061000130612       endif;
061100130612       if not %found(fnfgv01l) or fgvatb<>*blanks or §tvdef='S';
061200130527          *in28=*on;
061300130612          *in91=*on;
061400130527          vidmsg=err(6);
061500130527          leavesr;
061600130527       endif;
061700130612       // Errore se foglio inoltro e richiesta solo la stampa etichette
061800130612       if fgvttr='I' and vc1sts<>'S' and vc1ste='S';
061900130612          *in28=*on;
062000130612          *in91=*on;
062100130612          vidmsg=err(8);
062200130612          leavesr;
062300130612       endif;
062400130527
062500130527       endsr;
062600130528       //-------------------------------------------------------------*
062700130528       //Routine Lancio pgm di stampa                                 *
062800130528       //-------------------------------------------------------------*
062900130528       begsr sr_lancio;
063000130528
063100130528       // LANCIO PER DATA FOGLIO
063200130528       if vc1dfv>0;
063300130528       // Stampa scheda di trasporto
063400130528          if vc1sts='S';
063500130528             exsr sr_schetra;
063600130528          endif;
063700130711       // Stampa etichette documenti in partenza
063800130528          if vc1ste='S';
063900130607             wnfv=*zeros;
064000130528             exsr sr_etich;
064100130528          endif;
064200130528       else;
064300130528       //LANCIO PER NUMERO FOGLIO
064400130528          exsr sr_elasfl;
064500130528       endif;
064600130528
064700130528       EndSr;
064800130528
064900130528       //-------------------------------------------------------------*
065000130528       //Routine Stampa scheda di trasporto                           *
065100130528       //-------------------------------------------------------------*
065200130528       begsr sr_Schetra;
065300130529       setll (simfel: vc1dfvinv) fnfgv02l;
065400130529       reade (simfel: vc1dfvinv) fnfgv02l;
065500130529       dow not %eof(fnfgv02l);
065600130612          // Escludo fogli annullati e le defluenze
065700130612          if fgvatb=*blanks;
065800130612             exsr sr_dstv;
065900130612            if §tvdef<>'S';
066000130529             wnfv=%editc(fgvnfv:'X');
066100130529             dfgvdcn=fgvdcn;
066200130529             // Scelta fogli: tutti
066300130529             // Scelta fogli: solo non stampati
066400130529             if vc1fog='T' or §dcnfst=' ';
066500130529                if §dcnfst=' ';
066600130529                   wnrr=fgvnrr2;
066700130529                   exsr sr_lck;
066800130529                endif;
066900130529             // Richiamo routine che provvede a far stampare scheda di trasp
067000130529             // e aggiorna flag di stampato su fnfgv
067100130529                exsr sr_pgmsche;
067200130529             endif;
067300130612            endif;
067400130529          endif;
067500130529          reade (simfel: vc1dfvinv) fnfgv02l;
067600130529       enddo;
067700130528       endsr;
067800130528       //-------------------------------------------------------------*
067900130711       //Routine Stampa etichette documenti in partenza               *
068000130528       //-------------------------------------------------------------*
068100130528       begsr sr_etich  ;
068200130528       lst9dfv=vc1dfvinv;
068300130528       lst9fog=vc1fog;
068400130528       lst9nfv=%dec(wnfv:5:0);
068500130607       kpjbu=fnlst9ds;
068600130528       fnlst9r1(kpjba);
068700130528       endsr;
068800130528       //-------------------------------------------------------------*
068900130528       //Routine elaborazione sfl per lancio stampe                   *
069000130528       //-------------------------------------------------------------*
069100130528       begsr sr_elasfl ;
069200130528       s01nrr=1;
069300130528       chain s01nrr lst9s01;
069400130528       dow %found;
069500130528          if vs1nfv>*zeros;
069600130528             wnfv=vs1nfv;
069700130528       // Scheda di trasporto
069800130612             if vc1sts='S' and vh1def<>'S';
069900130529              //chaino foglio viaggio per sapere se stampa o ristampa
070000130529              //e per poi aggiornare il flag di stampa
070100130529                wnrr=vh1nrr;
070200130529                exsr sr_lck;
070300130529              //richiamo pgm che stampa la scheda di trasporto
070400130529                dfgvdcn=fgvdcn;
070500130529                if §dcnfst='S';
070600130529                   unlock fnfgv00f;
070700130529                endif;
070800130528                exsr sr_pgmsche;
070900130528             endif;
071000130528       // Etichette
071100130612             if vc1ste='S' and vh1ttr<>'I' and vh1def<>'S';
071200130607                clear vc1dfvinv;
071300130528                exsr sr_etich;
071400130528             endif;
071500130528          endif;
071600130528          s01nrr+=1;
071700130528          chain s01nrr lst9s01;
071800130528       enddo;
071900130528       endsr;
072000130529       //-------------------------------------------------------------*
072100130529       //Allocazione foglio viaggio per il quale stampare la scheda   *
072200130529       //-------------------------------------------------------------*
072300130529       begsr sr_lck;
072400130529                chain(e) Wnrr fnfgv00f;
072500130529              // Gestione errore di allocazione
072600130529                if %error;
072700130529                   clear trul82ds;
072800130529                   ul82§rrn=wnrr;
072900130529                   ul82§fil='FNFGV00F';
073000130529                   UL82§WIN = 'N';
073100130529                   UL82§F7  = 'S';
073200130529                   UL82§num = 1  ;
073300130529                   UL82§att = 30 ;
073400130529                   UL82§mss = msgjob;
073500130529                   callp(e) trul82r(trul82ds);
073600130529                   chain wnrr fnfgv00f;
073700130529                endif;
073800130529       endsr;
073900130528       //-------------------------------------------------------------*
074000130528       //Richiamo pgm che stampa la scheda di trasporto               *
074100130528       //-------------------------------------------------------------*
074200130528       begsr sr_pgmsche;
074300130528
074400130612       //Numero di copie
074500130612       pa2cpy=§vpocpst;
074600130612       // chaino fnfgs per vedere se presente lo scambio
074700130612       chain (fgvnfv:fgvlnp) fnfgs01l;
074800130612       if %found(fnfgs01l) and fgsatb=*blanks;
074900130612          pa2cpy+=1;
075000130612       endif;
075100130528       // ' '=Stampa, 'R'=Ristampa
075200130529       if §dcnfst=*blanks;
075300130529          pa2flg=' ';
075400130529       else;
075500130529          pa2flg='R';
075600130529       endif;
075700130528
075800130528       clear pa2msg;
075900130528       clear pa2f12;
076000130528       pa2fgs=%editc(simfel:'X');
076100130528       pa2prt=pa1prt;
076200130528       pa2mdt=pa1mdt;
076300130528       kpjbu=param2;
076400130528
076500130612       fnls94c1(kpjba);
076600130529
076700130529       //aggiorno flag di stampa scheda di trasporto se è la prima
076800130529       //volta che viene stampata
076900130529       if §dcnfst=*blanks;
077000130529          §dcnfst='S';
077100130529          fgvdcn=dfgvdcn;
077200130529          update fnfgvf;
077300130529       endif;
077400130528       endsr;
077500130612       //-------------------------------------------------------------*
077600130612       //Reperimento tabella tipi traino                              *
077700130612       //-------------------------------------------------------------*
077800130612       begsr sr_dstv;
077900130612       clear dstv;
078000130612       chain (1:'TV':fgvttr) tabel00f;
078100130612       if %found(tabel00f);
078200130612          eval dstv=tbluni;
078300130612       endif;
078400130612       endsr;
078500130620       //-------------------------------------------------------------*
078600130620       //Controllo esistenza dati per stampa etichette                *
078700130620       //-------------------------------------------------------------*
078800130620       begsr sr_fnfge;
078900130620       clear wfge;
079000130620       setll wdfv fnfge01l;
079100130620       if not %equal(fnfge01l);
079200130620          wfge='N'   ;
079300130620       endif;
079400130620       endsr;
079500090402       //-------------------------------------------------------------*
079600090402       //Routine Iniziale                                             *
079700090402       //-------------------------------------------------------------*
079800090414       begsr *inzsr;
079900090402
080000090402       //Reperisco dati job
080100090917         exsr datijob;
082500130612
082600130612        // Carico numero copie scheda di trasporto
082700130612         clear TIBS02ds;
082800130612         T02Mod = 'C';
082900130612         T02sif = knsif;
083000130612         T02cod = 'VPO';
083100130612         T02ke1 ='VPO'   ;
083200130612         TNTBE_RicercaControllo  (kpjba : tibs02ds);
083300130612         if  T02err  = *blank;
083400130612           dvpo = T02uni;
083500130612         endif;
083600090402
083700090402       EndSr;
083800040108
083900090402       //-------------------------------------------------------------*
084000090402       //Reperimento Dati del job (Utente/Operativi)                  *
084100090402       //-------------------------------------------------------------*
084200090918       Begsr DatiJob;
084300090403
084400090917          in(e) §azute;
084500090917          if not %error;
084600090917           in(e) §datiute;
084700090917          endif;
084800090917          if %error or rsut = *blanks;
084900090917           tibs34r(tibs34ds);
085000090917           in §azute;
085100090917           in §datiute;
085200090917          endif;
085300111205
085400111205       // Reperisco la data corrente
085500111205          datacur=(%date());
085600130524          wdate=%dec(datacur :*eur);
085700130524       // utime= %dec(%time());
085800090917         endsr;
085900040108
086000111205**
086100111205Data errata o mancante                                                        01
086200130524Selezionare per Data OPPURE per Numero Foglio Viaggio                         02
086300130524Scorrimento non consentito                                                    03
086400130524Foglio viaggio già selezionato                                                04
086500130527Indicare il Tipo di Fogli da elaborare                                        05
086600130612Foglio Viaggio non valido o annullato                                         06
086700130902                                                                              07
086800130711Foglio Inoltro non valido per la stampa Etichette documenti in partenza       08
086900130612Effettuare almeno una richiesta di stampa fra Etichette e Scheda di Trasporto 09
087000130711Non è più possibile stampare le etichette documen.nella data foglio richiesta 10
