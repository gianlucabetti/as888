000100110620     /*PRM dbgview(*source)
001000040107      *---------------------------------------------------------------*
001100130524      * Stampa Etichette/Scheda di trasporto                          *
001200040107      *---------------------------------------------------------------*
001300040107
001400040108     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001500090528     h dftactgrp(*no) actgrp(*caller)
001600040107
001700040107      *---------------------------------------------------------------*
001800040107
001900111202     ftabel00f  if   e           k disk
002000090922     fazorg01l  if   e           k disk
002001130529     ffnfgv01l  if   e           k disk    infds(fnfgvinf)
002002130529     ffnfgv02l  if   e           k disk    infds(fnfgvinf2)
002003130529     f                                     rename(fnfgv000:fnfgv2)
002004130529     ffnfgv00f  uf   e             disk    rename(fnfgv000:fnfgvf)
002005130612     ffnfgs01l  if   e           k disk
002006130524     ffnfv201l  if   e           k disk
002007130524     ffnlst9d   cf   E             workstn SFILE(LST9S01:s01NRR)
002700040112      *
002800040107      *---------------------------------------------------------------*
003400040107      *
003500040107      *   S C H I E R E   - - - - - - - - - - - - - - - - - - - - - - *
003600040107      *
003703130612     d err             s             79    dim(9) ctdata perrcd(1)
003704130524     D sfv             S              5    DIM(199) inz
004700040107      *
004800040107      *   D S   - - - - - - - - - - - - - - - - - - - - - - - - - - - *
004900040107      *
004901130528     D*
004902130528     D PARAM1          DS
004903130528     D* MODULO STAMPA TABULATI
004904130528     D  PA1MDT               227    236
004905130528     D* PGM STAMPA ETICHETTE
004906130528     D  PA1PSE               237    246
004907130528     D* STAMPANTE TABULATI - X FV
004908130528     D  PA1PRT               247    256
004909130528
004910130612     D* PASSO DATI A STAMPA FV                           -FNLS94C1
004911130528     D PARAM2          DS
004912130528     D* VIDNFV                11     15
004913130528     D  wnfv                  11     15
004914130528     D* FNLS94R -->  ' 'STAMPA     'R'RSTAMPA
004915130528     D  PA2FLG                16     16
004916130528     D* SE PA2MSG PIENO --> C'E' STATO ERRORE
004917130528     D  PA2MSG                17     94
004918130528     D* PARF12= 'S'     --> PREMUTO F12
004919130528     D  PA2F12               102    102
004920130528     D  PA2FGS               103    105
004921130528     D* STAMPATE E MODULO PER STAMPA FV
004922130528     D  PA2PRT               164    173
004923130528     D  PA2MDT               174    183
004924130612     D  PA2cpy               184    184  0
004925130528
004926130527     D fnlst4ds        DS
004927130527     D* ilst4gES = M - MANUTENZIONE FV RICHIAMATA
004928130527     D*          B - DA BORDERO'
004929130527     D*          A - apertura foglio
004930130527     D*          C - DA CHIUSURA FV SINGOLA
004931130527     D*          I - DA CHIUSURA DA IMP
004932130527     D*          N - DA INTERROGAZIONE  FV
004933130527     D*          M - Manutenzione FGV
004934130527     D*          P - Manutenzione FGV con videata piombi
004935130527
004936130527     D* CAMPI INPUT OBBLIGATORI:
004937130527     D*  GES / FGS
004938130527     d
004939130527     D* CAMPI INPUT PER MANUTENZIONE :
004940130527     D*  NFV
004941130527     D*
004942130527     D* CAMPI INPUT SOLO PER APERTURA :
004943130527     d* DFV /OLD / TRN / NRRP / PDR
004944130527     D
004945130527     D  ilst4DFV               3     10
004946130527     D
004947130527     D  ilst4nfv              11     15
004948130527     D  IlsT4GES              16     16
004949130527     D* Se MSG pieno --> c'è stato ERRORE
004950130527     D  olst4msg              17     94
004951130527     d* se iolst4FOR è pieno --> premuto F6 per forzare foglio di traini doppio
004952130527     D  iolst4FOR             95     97
004953130527     D
004954130527     D  IlsT4OLD              98     98
004955130527     D* OLST4F12 = 'S'    --> premuto F12
004956130527     D  olst4F12             102    102
004957130527     d
004958130527     D  ilst4fgs             103    105
004959130527     D  ilst4TRN             106    112  0
004960130527     D  ilst4NRRP            113    125
004961130527     D* se non immessa il numero relativo record della proposta
004962130527     d*  si può immettere a modo vecchio il trazionista
004963130527     D  ilst4PDR             126    132
004964130527     D*
004965130524     d fnlst3ds        ds
004966130524     D  PA3FLG                 1      1    INZ('R')
004967130524     D  PA3FL2                 2      2    INZ
004968130524     D  PA3NFV                 3      5P 0 INZ
004969130524     D  PA3DIN                 6     10P 0 INZ
004970130524     D  PA3DFI                11     15P 0
004971130524     D  PA3TFV                16     16    INZ('2')
004972130524     D* CAMPO USATO PER PASSARE AL PGM CHIAMANTE TUTTI I FOGLI VIAGGIO
004973130524     D  FVA                   17    241P 0 DIM(45)
004974130524     D  PA3FGS               242    244  0
004975130524
004976130612     d dstv          e ds
004977130612     d TIBS02ds      e ds                  inz
004978130612     D dvpodecofi    E DS
004979130612     D dvpo          E DS
004980130530     d trul82ds      e ds
004981130529     d fnlst9ds      e ds
005000090922     d Tibs34ds      e ds                  inz
005100090922     d §azute        e ds                  Extname(Azute00f)
005200090922     d                                     dtaara
005300090922     d §Datiute      e ds                  EXTname(ddatiute)
005301130524     d                                     dtaara
006200040107     d KPJBA         e ds
006201130529
006202130529     d dfgvdcn       e ds
006203130529
006204130529     D fnfgvinf        ds
006205130529     D  fgvnrr               397    400i 0
006206130529
006207130529     D fnfgvinf2       ds
006208130529     D  fgvnrr2              397    400i 0
008200040123      *
008900040109      *
009200090409
009300040107      *
009400040107      *   V A R I A B I L I   - - - - - - - - - - - - - - - - - - - - *
009500040107      *
010700040213      * - Indici di schiera / Contatori
010800090827     d i               s              3  0 inz
010801130527     d Xi              s              3  0 inz
010802130527     d c_Digits        c                   const('0123456789')
011000040109      *
011100040109      * - Campi di comodo
011206111205     d dataiso         s               d   datfmt(*iso)
011207111205     d dataeur         s               d   datfmt(*eur)
011209111205     d datacur         s               d   datfmt(*iso)
011210130524     d wdate           s              8  0
011211130527     D NUM5            s              5  0 inz
011212130527     d comnfv          s                   like(fgvnfv)
011213130527     d wselfv          s              1
011214130528     d wprimo          s              1
011215130529     d wnrr            s                   like(vh1nrr)
011217111202      // - Campi associati al video
011218111206     d s01nrr          s              4  0 inz
011219130524     d savnrr          s              4  0 inz
011220130524     d vc1dfvinv       s                   like(vc1dfv)
011221130612     D alfaFGS         S              3    INZ
018500090828      *
018600090828      * - Flags booleani
018700090828     d $Finerec        s              1    inz(*off)
018701111202     d $Fine           s               n   inz(*off)
018702111207     d $End            s               n   inz(*off)
018704111202     d $InzS01         s               n   inz(*on)
018705130529
018706130529     D msgjob          C                   CONST('Si sta bloccando la stampa sc-
018707130529     D                                     heda di trasporto: USCIRE un momento-
018708130529     D                                      da questo lavoro')
018709130524
018710130524      // -
018711130524     d fnlst3r         pr                  extpgm('FNLST3R')
018712130524     d  kpjba                              likeds(kpjbA)
018713130527     d fnlst4r         pr                  extpgm('FNLST4R')
018714130527     d  kpjba                              likeds(kpjbA)
018715130612     d fnls94c1        pr                  extpgm('FNLS94C1')
018716130528     d  kpjba                              likeds(kpjbA)
018717130528     d fnlst9r1        pr                  extpgm('FNLST9R1')
018718130528     d  kpjba                              likeds(kpjbA)
018719130529     d trul82r         pr                  extpgm('TRUL82R')
018720130529     d  trul82ds                           likeds(trul82ds)
018723111205
018800081023
018900090402      // ? PROTOTIPI ?
019000090402      /copy gaitrasrc/srcprotopr,tibs34r
019001130612      /copy gaitrasrc/srcprotopr,tibs02r
019500040107
019600040107      *===============================================================*
019700040107      *  RIEPILOGO INDICATORI                                         *
019800040107      *---------------------------------------------------------------*
019900090827      *  XX    -
020000040107      *===============================================================*
020100040107
020200040107     c     *Entry        plist
020300040107     c                   parm                    KPJBA
020301130528     C                   MOVEL     KPJBU         PARAM1
020302130528     C* MUOVO STAMPANTI E MODULO ETICHETTA
020303130528     C                   MOVEL     PA1PSE        lst9pgmse
020400090827     c
020600040123      *
020700090331      /free
020701130524       // Inizializzazione video
020802130524         exsr inzs01;
020803130524       //?Gestione video
020804111202       DOW $Fine = *off;
020805130524         exsr GesC01;
020813111202       ENDDO;
020900090610
026100090827       eval *inlr=*on;
026103130524
026104130524       //-------------------------------------------------------------*
026105130524       Begsr GesC01;
026106130524
026107130524       //?Visualizzazione del SFL (se ci sono dati)
026108130524         *in20 = (S01nrr > *zeros);
026109130524          write lst9z01;
026110130524          exfmt lst9c01;
026111130524          *in28=*off;
026118111206             select;
026119130612             // F3=Fine o 99 on errore di filiale non abilitata
026120130612                when *inkc or *in99;
026121111206                   $fine=*on;
026124130524             // Rollup
026125130524                when *in73;
026126130524                   s01nrr=savnrr;
026127130524                   chain s01nrr lst9s01;
026128130524                   if %found and (vs1nfv=*zeros or vs1nfv=*blanks);
026129130524                      *in28=*on;
026130130524                      vidmsg=err(3);
026132130524                   else;
026133130524                      exsr ries01;
026135130524                   endif;
026136130524                other;
026137130524                   exsr ctrc01;
026141130524                   if *inkf;
026142130528                      exsr sr_lancio;
026143130524                      $fine= *on;
026144111206                   endif;
026145111206             endsl;
026146111206       endsr;
026244111206       //--------------------------------------------------------------
026245111206       BEGSR InzS01;
026246111206
026247111206         *in21   = *off;
026248111206         *in23   = *off;
026249130524         write  LST9C01;
026251111206         *in21   = *on;
026253111206
026254130524         vc1ste='N';
026255130524         vc1sts='S';
026256130524         clear vc1dfv;
026257130524         clear vc1fog;
026258111206         clear S01nrr;
026259130524         *in41=*on;
026260111206
026261111206       //?Caricamento subfile
026262111206         exsr Ries01;
026263111206
026264111206         *in23 = *on;
026265111206
026266111206       ENDSR;
026267111206
026268111206       //--------------------------------------------------------------
026269111206       //?Riempimento S01.
026270111206       //--------------------------------------------------------------
026271111206       BEGSR RieS01;
026272130528
026273130528        *in42=*off;
026274130528        *in44=*off;
026275130528        *in04=*off;
026276130528
026277130527        for I=1 to 9;
026283111206           S01nrr += 1;
026284130528           exsr clr_f_sfl;
026296130524           write  lst9s01;
026297130524        endfor;
026298130524           rec=s01nrr;
026299130524           savnrr=s01nrr;
026300111206
026301111206       ENDSR ;
026302130524       //--------------------------------------------------------------
026303130524       //?Controlli control e sfl
026304130524       //--------------------------------------------------------------
026305130524       BEGSR CTRC01;
026306130524
026307130612          *in40=*off;
026308130612          *in43=*off;
026309130612          *in45=*off;
026310130612       // errore se non richiesta nè stampa etichette nè stampa scheda
026311130612       if vc1ste<>'S' and vc1sts<>'S';
026312130612          *in28=*on;
026313130612          *in45=*on;
026315130612          vidmsg=err(9);
026316130612          leavesr;
026317130612       endif;
026318130524       // Verifico se selezionati fogli nel sfl
026319130524       exsr ctrsflA;
026320130524       // richiesta: O per data foglio O per numero foglio
026321130524       if (vc1dfv>0 and wselfv='S') or (vc1dfv<=0 and wselfv=' ');
026322130524          *in28=*on;
026323130612          *in40=*on;
026325130524          vidmsg=err(2);
026326130524          leavesr;
026327130524       endif;
026328130524
026329130524       // Richiesta per data foglio: controllo la data
026330130524       if vc1dfv>0;
026331130527          *in41=*on;
026332130524       if %subst(%editc(vc1dfv:'X'):1:2)='00';
026333130524          test(DE) *dmy vc1dfv;
026334130524          if %error;
026336130524             *in40=*on;
026337130524             *in28=*on;
026338130524             vidmsg=err(1);
026339130524             leavesr;
026340130524          else;
026341130524             dataiso=%date(vc1dfv:*dmy);
026342130524          endif;
026343130524       else;
026344130524          test(DE) *eur vc1dfv;
026345130524          if %error;
026347130524             *in40=*on;
026348130524             *in28=*on;
026349130524             vidmsg=err(1);
026350130524             leavesr;
026351130524          else;
026352130524             dataiso=%date(vc1dfv:*eur);
026353130524          endif;
026354130524       endif;
026355130524          dataeur=dataiso;
026356130524          vc1dfv=%dec(dataeur);
026357130524          vc1dfvinv=%dec(dataiso);
026358130527       // errore se non selezionato il tipo di fogli
026359130612       // if vc1fog <> 'T' and vc1fog<>'S';
026360130612       //    *in90=*on;
026361130612       //    *in43=*on;
026362130612       //    *in28=*on;
026363130612       //    vidmsg=err(5);
026364130612       //    leavesr;
026365130612       // endif;
026366130527       else;
026367130527       // Richiesta per numero foglio
026368130527          *in41=*off;
026369130527          exsr ctrsfl;
026370130524       endif;
026371130524
026372130524       ENDSR ;
026413040108
026414130524       //--------------------------------------------------------------
026415130524       //?Controllo sfl se immesso almeno un foglio
026416130524       //--------------------------------------------------------------
026417130524       BEGSR ctrsflA;
026418130524       clear wselfv;
026419130524       for i=1 to savnrr;
026420130524          chain i lst9s01;
026421130524          if vs1nfv<>*zeros and vs1nfv<>*blanks;
026422130524             wselfv='S';
026423130527             leavesr;
026424130524          endif;
026425130524       endfor;
026426130524       endsr;
026427130524       //--------------------------------------------------------------
026428130524       //?Controllo sfl
026429130524       //--------------------------------------------------------------
026430130524       BEGSR ctrsfl;
026432130524       *in42=*off;
026433130524       *in44=*off;
026434130528       *in04=*off;
026435130528       wprimo=' ';
026436130524       clear sfv;
026437130524       s01nrr=1;
026438130524       chain s01nrr lst9s01;
026439130524       dow %found;
026440130524
026441130524       // Ricerca foglio
026442130524          if %scan('?':vs1nfv)>0;
026443130524             exsr ricnfv;
026444130524             *in90 = *on;
026445130524             leavesr;
026446130524          endif;
026447130524
026448130524       // Controllo numero foglio immesso
026449130524          if vs1nfv<> *zeros and vs1nfv<>*blanks;
026450130524             exsr ctrnfv;
026451130524             if *in90;
026452130524                *in42=*on;
026453130524                update lst9s01;
026454130524                leavesr;
026455130524             endif;
026456130527
026457130527             exsr decod;
026472130524
026473130524       // errore se numero foglio già presente nel sfl
026474130524             if %lookup(vs1nfv:sfv)>0;
026475130524                eval *in42=*on;
026476130524                eval *in90=*on;
026477130524                eval *in28=*on;
026478130524                vidmsg=err(4);
026479130524                update lst9s01;
026480130524                leavesr;
026481130524             endif;
026482130524             if s01nrr<=199;
026483130524                sfv(s01nrr)=vs1nfv;
026484130524             endif;
026485130524          // OPZIONE 1 --> Richiamo la manutenzione del foglio viaggio
026486130524             if vs1sce='1';
026487130524                vs1sce=' ' ;
026488130524                exsr fviaggio;
026489130524                update  lst9s01;
026490130527                if olst4msg<>*blanks;
026491130527                eval *in42=*on;
026492130527                eval *in90=*on;
026493130527                eval *in28=*on;
026494130527                vidmsg=olst4msg;
026495130527                update lst9s01;
026496130527                endif;
026497130524                leavesr;
026498130524             endif;
026501130528          else;
026502130528             exsr clr_f_sfl;
026503130528             if wprimo=*blanks;
026504130612                if not *in45;
026505130612                   *in04=*on;
026506130612                endif;
026507130528                wprimo='1';
026508130528             endif;
026509130524          endif;
026510130528          update lst9s01;
026511130524          s01nrr+=1;
026512130524          chain s01nrr lst9s01;
026513130524       enddo;
026514130524       endsr;
026515130528       //-------------------------------------------------------------*
026516130528       //pulizia campi sfl                                            *
026517130528       //-------------------------------------------------------------*
026518130528       begsr clr_f_sfl;
026519130528             clear vs1nfv;
026520130528             clear vs1sce;
026521130528             clear vs1lna;
026522130528             clear vs1dlna;
026523130528             clear vs1dfv;
026529130528             clear vs1hpa;
026530130529             clear vh1nrr;
026531130612             clear vh1ttr;
026539130528       endsr;
026540130527       //-------------------------------------------------------------*
026541130527       //decodifica campi sfl                                         *
026542130527       //-------------------------------------------------------------*
026543130527       begsr decod ;
026544130529       // Nrr
026545130529             vh1nrr=fgvnrr;
026546130527       // lna
026547130527             vs1lna=fgvlna;
026548130528             clear vs1dlna;
026549130527             chain (fgvlna) azorg01l;
026550130527             if %found(azorg01l);
026551130527                vs1dlna=orgdes;
026552130527             endif;
026553130527       // Data foglio
026554130527             dataeur=%date(fgvdfv:*iso);
026555130527             vs1dfv=%dec(%subst(%editc(%dec(dataeur:*eur):'X'):1:4):4:0);
026556130527       // ora partenza: quella reale se presente altrimenti quella teorica
026557130527       //               se presente
026558130528             clear vs1hpa;
026559130529             if fgvhmp>0;
026560130529                vs1hpa=fgvhmp;
026561130527             else;
026562130527                chain (fgvlnp: fgvnfv: 'P': 0: 'T') fnfv201l;
026563130527                if %found(fnfv201l);
026564130527                   vs1hpa=fv2hpa;
026565130527                endif;
026566130527             endif;
026573130612             vh1ttr=fgvttr;
026574130612             vh1def=§tvdef;
026581130527       endsr;
026582130524       //-------------------------------------------------------------*
026583130524       //Richiamo ricerca numero foglio                               *
026584130524       //-------------------------------------------------------------*
026585130524       begsr ricnfv;
026586130524       reset fnlst3ds;
026587130524       pa3fl2='F';
026588130524       pa3dfi=wdate;
026589130524       pa3fgs=simfel;
026590130524       kpjbu=fnlst3ds;
026591130524       fnlst3r(kpjba);
026592130524       fnlst3ds=kpjbu;
026593130524
026594130524       // se selezionati fv li scrivo nel sfl
026595130524       *in04=*off;
026596130524       clear vs1nfv;
026597130524       update lst9s01;
026598130524       s01nrr-=1;
026599130528       xi=1;
026600130528       dow XI<=45 and fva(xi)<>*zeros;
026601130524          s01nrr+=1;
026602130524          chain s01nrr lst9s01;
026603130527       // Se necessario creo nuova pagina di sfl
026604130524          if not %found;
026605130524             num5=s01nrr;
026606130524             s01nrr-=1;
026607130527             exsr ries01;
026608130524             s01nrr=num5;
026609130524             chain s01nrr lst9s01;
026610130524          endif;
026611130524          if vs1nfv=*zeros or vs1nfv=*blanks;
026612130527             vs1nfv=%subst(%editc(fva(Xi):'X'):1:5);
026613130527             comnfv=%dec(vs1nfv:5:0);
026614130527             vs1lna=%dec(%subst(%editc(fva(Xi):'X'):6:3):3:0);
026615130529             chain (comnfv: simfel) fnfgv01l;
026616130612             exsr sr_dstv;
026617130527             exsr decod;
026618130527             update lst9s01;
026619130528             xi+=1;
026620130524          endif;
026621130528       enddo;
026622130528
026623130528       // Posiziono il cursore sul primo record successivo a quello di
026624130528       // immissione
026625130528       select;
026626130528       when s01nrr<1;
026627130528          s01nrr=1;
026628130528       other;
026629130528          if (s01nrr+1)<=savnrr;
026630130528             s01nrr+=1;
026631130528          endif;
026632130528          chain s01nrr lst9s01;
026633130528          if %found;
026634130528             *in04=*on;
026635130528             update lst9s01;
026636130528          endif;
026637130528       endsl;
026638130528       *in04=*off;
026639130528       endsr;
026640130524       //-------------------------------------------------------------*
026641130524       //Richiamo manutenzione foglio viaggio                         *
026642130524       //-------------------------------------------------------------*
026643130524       begsr fviaggio;
026644130527       clear fnlst4ds;
026645130527       ilst4ges='B';
026646130527       ilst4fgs=%editc(simfel:'X');
026647130527       ilst4nfv=vs1nfv;
026648130527       kpjbu=fnlst4ds;
026649130527       fnlst4r(kpjba);
026650130527       fnlst4ds=kpjbu;
026651130524       endsr;
026652130527       //-------------------------------------------------------------*
026653130527       //Controllo foglio viaggio                                     *
026654130527       //-------------------------------------------------------------*
026655130527       begsr ctrnfv  ;
026656130527
026657130612       *in90=*off;
026658130527       // deve contenere solo numeri
026659130527       if %check (c_Digits:vs1nfv)<>0;
026660130527          *in28=*on;
026661130527          *in90=*on;
026662130527          vidmsg=err(6);
026663130527          leavesr;
026664130527       endif;
026665130612
026666130527
026667130527       // Verifica esistenza del foglio su fnfgv
026668130527       comnfv=%dec(vs1nfv:5:0);
026669130529       chain (comnfv: simfel) fnfgv01l;
026670130612       if %found(fnfgv01l) and fgvatb=*blanks;
026671130612          exsr sr_dstv;
026672130612       endif;
026673130612       if not %found(fnfgv01l) or fgvatb<>*blanks or §tvdef='S';
026674130527          *in28=*on;
026675130527          *in90=*on;
026676130527          vidmsg=err(6);
026677130527          leavesr;
026678130527       endif;
026679130612       // Errore se foglio inoltro e richiesta solo la stampa etichette
026680130612       if fgvttr='I' and vc1sts<>'S' and vc1ste='S';
026681130612          *in28=*on;
026682130612          *in90=*on;
026683130612          vidmsg=err(8);
026684130612          leavesr;
026685130612       endif;
026686130527
026687130527       endsr;
026688130528       //-------------------------------------------------------------*
026689130528       //Routine Lancio pgm di stampa                                 *
026690130528       //-------------------------------------------------------------*
026691130528       begsr sr_lancio;
026692130528
026693130528       // LANCIO PER DATA FOGLIO
026694130528       if vc1dfv>0;
026695130528       // Stampa scheda di trasporto
026696130528          if vc1sts='S';
026697130528             exsr sr_schetra;
026698130528          endif;
026699130528       // Stampa etichette borderò
026700130528          if vc1ste='S';
026701130607             wnfv=*zeros;
026702130528             exsr sr_etich;
026703130528          endif;
026704130528       else;
026705130528       //LANCIO PER NUMERO FOGLIO
026706130528          exsr sr_elasfl;
026707130528       endif;
026708130528
026709130528       EndSr;
026710130528
026711130528       //-------------------------------------------------------------*
026712130528       //Routine Stampa scheda di trasporto                           *
026713130528       //-------------------------------------------------------------*
026714130528       begsr sr_Schetra;
026715130529       setll (simfel: vc1dfvinv) fnfgv02l;
026716130529       reade (simfel: vc1dfvinv) fnfgv02l;
026717130529       dow not %eof(fnfgv02l);
026718130612          // Escludo fogli annullati e le defluenze
026719130612          if fgvatb=*blanks;
026720130612             exsr sr_dstv;
026721130612            if §tvdef<>'S';
026722130529             wnfv=%editc(fgvnfv:'X');
026723130529             dfgvdcn=fgvdcn;
026724130529             // Scelta fogli: tutti
026725130529             // Scelta fogli: solo non stampati
026726130529             if vc1fog='T' or §dcnfst=' ';
026727130529                if §dcnfst=' ';
026728130529                   wnrr=fgvnrr2;
026729130529                   exsr sr_lck;
026730130529                endif;
026731130529             // Richiamo routine che provvede a far stampare scheda di trasp
026732130529             // e aggiorna flag di stampato su fnfgv
026733130529                exsr sr_pgmsche;
026734130529             endif;
026735130612            endif;
026736130529          endif;
026737130529          reade (simfel: vc1dfvinv) fnfgv02l;
026738130529       enddo;
026739130528       endsr;
026740130528       //-------------------------------------------------------------*
026741130528       //Routine Stampa etichette borderò                             *
026742130528       //-------------------------------------------------------------*
026743130528       begsr sr_etich  ;
026744130528       lst9dfv=vc1dfvinv;
026745130528       lst9fog=vc1fog;
026746130528       lst9nfv=%dec(wnfv:5:0);
026747130607       kpjbu=fnlst9ds;
026748130528       fnlst9r1(kpjba);
026749130528       endsr;
026750130528       //-------------------------------------------------------------*
026751130528       //Routine elaborazione sfl per lancio stampe                   *
026752130528       //-------------------------------------------------------------*
026753130528       begsr sr_elasfl ;
026754130528       s01nrr=1;
026755130528       chain s01nrr lst9s01;
026756130528       dow %found;
026757130528          if vs1nfv>*zeros;
026758130528             wnfv=vs1nfv;
026759130528       // Scheda di trasporto
026760130612             if vc1sts='S' and vh1def<>'S';
026761130529              //chaino foglio viaggio per sapere se stampa o ristampa
026762130529              //e per poi aggiornare il flag di stampa
026763130529                wnrr=vh1nrr;
026764130529                exsr sr_lck;
026765130529              //richiamo pgm che stampa la scheda di trasporto
026766130529                dfgvdcn=fgvdcn;
026767130529                if §dcnfst='S';
026768130529                   unlock fnfgv00f;
026769130529                endif;
026770130528                exsr sr_pgmsche;
026771130528             endif;
026772130528       // Etichette
026773130612             if vc1ste='S' and vh1ttr<>'I' and vh1def<>'S';
026774130607                clear vc1dfvinv;
026775130528                exsr sr_etich;
026776130528             endif;
026777130528          endif;
026778130528          s01nrr+=1;
026779130528          chain s01nrr lst9s01;
026780130528       enddo;
026781130528       endsr;
026782130529       //-------------------------------------------------------------*
026783130529       //Allocazione foglio viaggio per il quale stampare la scheda   *
026784130529       //-------------------------------------------------------------*
026785130529       begsr sr_lck;
026786130529                chain(e) Wnrr fnfgv00f;
026787130529              // Gestione errore di allocazione
026788130529                if %error;
026789130529                   clear trul82ds;
026790130529                   ul82§rrn=wnrr;
026791130529                   ul82§fil='FNFGV00F';
026792130529                   UL82§WIN = 'N';
026793130529                   UL82§F7  = 'S';
026794130529                   UL82§num = 1  ;
026795130529                   UL82§att = 30 ;
026796130529                   UL82§mss = msgjob;
026797130529                   callp(e) trul82r(trul82ds);
026798130529                   chain wnrr fnfgv00f;
026799130529                endif;
026800130529       endsr;
026801130528       //-------------------------------------------------------------*
026802130528       //Richiamo pgm che stampa la scheda di trasporto               *
026803130528       //-------------------------------------------------------------*
026804130528       begsr sr_pgmsche;
026805130528
026806130612       //Numero di copie
026807130612       pa2cpy=§vpocpst;
026808130612       // chaino fnfgs per vedere se presente lo scambio
026809130612       chain (fgvnfv:fgvlnp) fnfgs01l;
026810130612       if %found(fnfgs01l) and fgsatb=*blanks;
026811130612          pa2cpy+=1;
026812130612       endif;
026813130528       // ' '=Stampa, 'R'=Ristampa
026814130529       if §dcnfst=*blanks;
026815130529          pa2flg=' ';
026816130529       else;
026817130529          pa2flg='R';
026818130529       endif;
026819130528
026820130528       clear pa2msg;
026821130528       clear pa2f12;
026822130528       pa2fgs=%editc(simfel:'X');
026823130528       pa2prt=pa1prt;
026824130528       pa2mdt=pa1mdt;
026825130528       kpjbu=param2;
026826130528
026827130612       fnls94c1(kpjba);
026828130529
026829130529       //aggiorno flag di stampa scheda di trasporto se è la prima
026830130529       //volta che viene stampata
026831130529       if §dcnfst=*blanks;
026832130529          §dcnfst='S';
026833130529          fgvdcn=dfgvdcn;
026834130529          update fnfgvf;
026835130529       endif;
026836130528       endsr;
026837130612       //-------------------------------------------------------------*
026838130612       //Reperimento tabella tipi traino                              *
026839130612       //-------------------------------------------------------------*
026840130612       begsr sr_dstv;
026841130612       clear dstv;
026842130612       chain (1:'TV':fgvttr) tabel00f;
026843130612       if %found(tabel00f);
026844130612          eval dstv=tbluni;
026845130612       endif;
026846130612       endsr;
026847090402       //-------------------------------------------------------------*
026848090402       //Routine Iniziale                                             *
026849090402       //-------------------------------------------------------------*
026850090414       begsr *inzsr;
026851090402
026852090402       //Reperisco dati job
026900090917         exsr datijob;
026902130612
026903130612        // Carico filiali di test abilitate
026904130612         clear TIBS02ds;
026905130612         T02Mod = 'C';
026906130612         T02sif = knsif;
026907130612         T02cod = 'VPO';
026908130612         T02ke1 ='DECOFI'   ;
026909130612         TNTBE_RicercaControllo  (kpjba : tibs02ds);
026910130612         if  T02err  = *blank;
026911130612           dvpodecofi = T02uni;
026912130612         endif;
026913130612
026914130612       if  §vpofgvf1<>'999'  ;
026915130612           eval alfafgs=%editc(simfel:'X')  ;
026916130612       if  alfafgs<>§vpofgvf1 and alfafgs<>§vpofgvf2 and
026917130612           alfafgs<>§vpofgvf3 and alfafgs<>§vpofgvf4 and
026918130612           alfafgs<>§vpofgvf5 ;
026919130612          Vidmsg = err(7);
026921130612          *in28=*on  ;
026922130612          *in99=*on  ;
026923130612           leavesr;
026924130612         endif         ;
026925130612         endif         ;
026926130612
026927130612        // Carico numero copie scheda di trasporto
026928130612         clear TIBS02ds;
026929130612         T02Mod = 'C';
026930130612         T02sif = knsif;
026931130612         T02cod = 'VPO';
026932130612         T02ke1 ='VPO'   ;
026933130612         TNTBE_RicercaControllo  (kpjba : tibs02ds);
026934130612         if  T02err  = *blank;
026935130612           dvpo = T02uni;
026936130612         endif;
030300090402
030400090402       EndSr;
030500040108
034400090402       //-------------------------------------------------------------*
034500090402       //Reperimento Dati del job (Utente/Operativi)                  *
034600090402       //-------------------------------------------------------------*
034700090918       Begsr DatiJob;
034800090403
034900090917          in(e) §azute;
035000090917          if not %error;
035100090917           in(e) §datiute;
035200090917          endif;
035300090917          if %error or rsut = *blanks;
035400090917           tibs34r(tibs34ds);
035500090917           in §azute;
035600090917           in §datiute;
035700090917          endif;
035701111205
035702111205       // Reperisco la data corrente
035703111205          datacur=(%date());
035705130524          wdate=%dec(datacur :*eur);
035706130524       // utime= %dec(%time());
036000090917         endsr;
036100040108
155300111205**
155500111205Data errata o mancante                                                        01
155600130524Selezionare per Data OPPURE per Numero Foglio Viaggio                         02
155800130524Scorrimento non consentito                                                    03
155900130524Foglio viaggio già selezionato                                                04
156000130527Indicare il Tipo di Fogli da elaborare                                        05
156100130612Foglio Viaggio non valido o annullato                                         06
156200130612Filiale non abilitata all'uso di questa funzione.Enter per terminare          07
156300130612Foglio Inoltro non valido per la stampa etichette                             08
156400130612Effettuare almeno una richiesta di stampa fra Etichette e Scheda di Trasporto 09
