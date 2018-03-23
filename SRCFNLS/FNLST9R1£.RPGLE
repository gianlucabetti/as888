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
002001130524     ffnfgv01l  if   e           k disk
002002130524     ffnfv201l  if   e           k disk
002003130524     ffnlst9d   cf   E             workstn SFILE(LST9S01:s01NRR)
002700040112      *
002800040107      *---------------------------------------------------------------*
003400040107      *
003500040107      *   S C H I E R E   - - - - - - - - - - - - - - - - - - - - - - *
003600040107      *
003703130527     d err             s             79    dim(6) ctdata perrcd(1)
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
004910130528     D* PASSO DATI A STAMPA FV                           -FNLS94C
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
004924130528
004925130527     D fnlst4ds        DS
004926130527     D* ilst4gES = M - MANUTENZIONE FV RICHIAMATA
004927130527     D*          B - DA BORDERO'
004928130527     D*          A - apertura foglio
004929130527     D*          C - DA CHIUSURA FV SINGOLA
004930130527     D*          I - DA CHIUSURA DA IMP
004931130527     D*          N - DA INTERROGAZIONE  FV
004932130527     D*          M - Manutenzione FGV
004933130527     D*          P - Manutenzione FGV con videata piombi
004934130527
004935130527     D* CAMPI INPUT OBBLIGATORI:
004936130527     D*  GES / FGS
004937130527     d
004938130527     D* CAMPI INPUT PER MANUTENZIONE :
004939130527     D*  NFV
004940130527     D*
004941130527     D* CAMPI INPUT SOLO PER APERTURA :
004942130527     d* DFV /OLD / TRN / NRRP / PDR
004943130527     D
004944130527     D  ilst4DFV               3     10
004945130527     D
004946130527     D  ilst4nfv              11     15
004947130527     D  IlsT4GES              16     16
004948130527     D* Se MSG pieno --> c'è stato ERRORE
004949130527     D  olst4msg              17     94
004950130527     d* se iolst4FOR è pieno --> premuto F6 per forzare foglio di traini doppio
004951130527     D  iolst4FOR             95     97
004952130527     D
004953130527     D  IlsT4OLD              98     98
004954130527     D* OLST4F12 = 'S'    --> premuto F12
004955130527     D  olst4F12             102    102
004956130527     d
004957130527     D  ilst4fgs             103    105
004958130527     D  ilst4TRN             106    112  0
004959130527     D  ilst4NRRP            113    125
004960130527     D* se non immessa il numero relativo record della proposta
004961130527     d*  si può immettere a modo vecchio il trazionista
004962130527     D  ilst4PDR             126    132
004963130527     D*
004964130524     d fnlst3ds        ds
004965130524     D  PA3FLG                 1      1    INZ('R')
004966130524     D  PA3FL2                 2      2    INZ
004967130524     D  PA3NFV                 3      5P 0 INZ
004968130524     D  PA3DIN                 6     10P 0 INZ
004969130524     D  PA3DFI                11     15P 0
004970130524     D  PA3TFV                16     16    INZ('2')
004971130524     D* CAMPO USATO PER PASSARE AL PGM CHIAMANTE TUTTI I FOGLI VIAGGIO
004972130524     D  FVA                   17    241P 0 DIM(45)
004973130524     D  PA3FGS               242    244  0
004974130524
004975130528     d fnlst9ds      e ds
005000090922     d Tibs34ds      e ds                  inz
005100090922     d §azute        e ds                  Extname(Azute00f)
005200090922     d                                     dtaara
005300090922     d §Datiute      e ds                  EXTname(ddatiute)
005301130524     d                                     dtaara
006200040107     d KPJBA         e ds
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
011215111202      // - Campi associati al video
011216111206     d s01nrr          s              4  0 inz
011217130524     d savnrr          s              4  0 inz
011218130524     d vc1dfvinv       s                   like(vc1dfv)
018500090828      *
018600090828      * - Flags booleani
018700090828     d $Finerec        s              1    inz(*off)
018701111202     d $Fine           s               n   inz(*off)
018702111207     d $End            s               n   inz(*off)
018704111202     d $InzS01         s               n   inz(*on)
018705130524
018706130524      // -
018707130524     d fnlst3r         pr                  extpgm('FNLST3R')
018708130524     d  kpjba                              likeds(kpjbA)
018709130527     d fnlst4r         pr                  extpgm('FNLST4R')
018710130527     d  kpjba                              likeds(kpjbA)
018711130528     d fnls94c         pr                  extpgm('FNLS94C')
018712130528     d  kpjba                              likeds(kpjbA)
018713130528     d fnlst9r1        pr                  extpgm('FNLST9R1')
018714130528     d  kpjba                              likeds(kpjbA)
018715111205
018800081023
018900090402      // ? PROTOTIPI ?
019000090402      /copy gaitrasrc/srcprotopr,tibs34r
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
026119111206             // F3=Fine
026120111206                when *inkc;
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
026307130524       // Verifico se selezionati fogli nel sfl
026308130524       exsr ctrsflA;
026309130524       // richiesta: O per data foglio O per numero foglio
026311130524       if (vc1dfv>0 and wselfv='S') or (vc1dfv<=0 and wselfv=' ');
026312130524          *in28=*on;
026313130524          *in90=*on;
026314130524          vidmsg=err(2);
026315130524          leavesr;
026316130524       endif;
026325130524
026326130524       // Richiesta per data foglio: controllo la data
026327130524       if vc1dfv>0;
026328130527          *in41=*on;
026329130524       if %subst(%editc(vc1dfv:'X'):1:2)='00';
026330130524          test(DE) *dmy vc1dfv;
026331130524          if %error;
026332130524             *in90=*on;
026333130524             *in40=*on;
026334130524             *in28=*on;
026335130524             vidmsg=err(1);
026336130524             leavesr;
026337130524          else;
026338130524             dataiso=%date(vc1dfv:*dmy);
026339130524          endif;
026340130524       else;
026341130524          test(DE) *eur vc1dfv;
026342130524          if %error;
026343130524             *in90=*on;
026344130524             *in40=*on;
026345130524             *in28=*on;
026346130524             vidmsg=err(1);
026347130524             leavesr;
026348130524          else;
026349130524             dataiso=%date(vc1dfv:*eur);
026350130524          endif;
026351130524       endif;
026352130524          dataeur=dataiso;
026353130524          vc1dfv=%dec(dataeur);
026354130524          vc1dfvinv=%dec(dataiso);
026355130527       // errore se non selezionato il tipo di fogli
026356130527          if vc1fog <> 'T' and vc1fog<>'S';
026357130527             *in90=*on;
026358130527             *in43=*on;
026359130527             *in28=*on;
026360130527             vidmsg=err(5);
026361130527             leavesr;
026362130527          endif;
026363130527       else;
026364130527       // Richiesta per numero foglio
026365130527          *in41=*off;
026366130527          exsr ctrsfl;
026367130524       endif;
026368130524
026370130524       ENDSR ;
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
026431130524       *in90=*off;
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
026504130528                *in04=*on;
026505130528                wprimo='1';
026506130528             endif;
026507130524          endif;
026508130528          update lst9s01;
026509130524          s01nrr+=1;
026510130524          chain s01nrr lst9s01;
026511130524       enddo;
026512130524       endsr;
026513130528       //-------------------------------------------------------------*
026514130528       //pulizia campi sfl                                            *
026515130528       //-------------------------------------------------------------*
026516130528       begsr clr_f_sfl;
026518130528             clear vs1nfv;
026519130528             clear vs1sce;
026520130528             clear vs1lna;
026521130528             clear vs1dlna;
026522130528             clear vs1dfv;
026529130528             clear vs1hpa;
026539130528       endsr;
026540130527       //-------------------------------------------------------------*
026541130527       //decodifica campi sfl                                         *
026542130527       //-------------------------------------------------------------*
026543130527       begsr decod ;
026545130527       // lna
026546130527             vs1lna=fgvlna;
026547130528             clear vs1dlna;
026548130527             chain (fgvlna) azorg01l;
026549130527             if %found(azorg01l);
026550130527                vs1dlna=orgdes;
026551130527             endif;
026552130527       // Data foglio
026553130527             dataeur=%date(fgvdfv:*iso);
026554130527             vs1dfv=%dec(%subst(%editc(%dec(dataeur:*eur):'X'):1:4):4:0);
026555130527       // ora partenza: quella reale se presente altrimenti quella teorica
026556130527       //               se presente
026557130528             clear vs1hpa;
026558130527             chain (fgvlnp: fgvnfv: 'P': 0: 'R') fnfv201l;
026559130527             if %found(fnfv201l);
026560130527                vs1hpa=fv2hpa;
026561130527             else;
026562130527                chain (fgvlnp: fgvnfv: 'P': 0: 'T') fnfv201l;
026563130527                if %found(fnfv201l);
026564130527                   vs1hpa=fv2hpa;
026565130527                endif;
026566130527             endif;
026567130527       endsr;
026568130524       //-------------------------------------------------------------*
026569130524       //Richiamo ricerca numero foglio                               *
026570130524       //-------------------------------------------------------------*
026571130524       begsr ricnfv;
026572130524       reset fnlst3ds;
026573130524       pa3fl2='F';
026574130524       pa3dfi=wdate;
026575130524       pa3fgs=simfel;
026576130524       kpjbu=fnlst3ds;
026577130524       fnlst3r(kpjba);
026578130524       fnlst3ds=kpjbu;
026579130524
026580130524       // se selezionati fv li scrivo nel sfl
026581130524       *in04=*off;
026582130524       clear vs1nfv;
026583130524       update lst9s01;
026584130524       s01nrr-=1;
026585130528       xi=1;
026586130528       dow XI<=45 and fva(xi)<>*zeros;
026590130524          s01nrr+=1;
026591130524          chain s01nrr lst9s01;
026592130527       // Se necessario creo nuova pagina di sfl
026593130524          if not %found;
026594130524             num5=s01nrr;
026595130524             s01nrr-=1;
026596130527             exsr ries01;
026597130524             s01nrr=num5;
026598130524             chain s01nrr lst9s01;
026599130524          endif;
026600130524          if vs1nfv=*zeros or vs1nfv=*blanks;
026601130527             vs1nfv=%subst(%editc(fva(Xi):'X'):1:5);
026602130527             comnfv=%dec(vs1nfv:5:0);
026603130527             vs1lna=%dec(%subst(%editc(fva(Xi):'X'):6:3):3:0);
026604130527             chain (comnfv: simfel) fnfgv01l;
026605130527             exsr decod;
026606130527             update lst9s01;
026607130528             xi+=1;
026608130524          endif;
026609130528       enddo;
026610130528
026611130528       // Posiziono il cursore sul primo record successivo a quello di
026612130528       // immissione
026613130528       select;
026614130528       when s01nrr<1;
026615130528          s01nrr=1;
026616130528       other;
026617130528          if (s01nrr+1)<=savnrr;
026618130528             s01nrr+=1;
026619130528          endif;
026620130528          chain s01nrr lst9s01;
026621130528          if %found;
026622130528             *in04=*on;
026623130528             update lst9s01;
026624130528          endif;
026625130528       endsl;
026626130528       *in04=*off;
026627130528       endsr;
026628130524       //-------------------------------------------------------------*
026629130524       //Richiamo manutenzione foglio viaggio                         *
026630130524       //-------------------------------------------------------------*
026631130524       begsr fviaggio;
026632130527       clear fnlst4ds;
026633130527       ilst4ges='B';
026634130527       ilst4fgs=%editc(simfel:'X');
026635130527       ilst4nfv=vs1nfv;
026636130527       kpjbu=fnlst4ds;
026637130527       fnlst4r(kpjba);
026638130527       fnlst4ds=kpjbu;
026639130524       endsr;
026640130527       //-------------------------------------------------------------*
026641130527       //Controllo foglio viaggio                                     *
026642130527       //-------------------------------------------------------------*
026643130527       begsr ctrnfv  ;
026644130527
026645130527       // deve contenere solo numeri
026646130527       if %check (c_Digits:vs1nfv)<>0;
026647130527          *in28=*on;
026648130527          *in90=*on;
026649130527          vidmsg=err(6);
026650130527          leavesr;
026651130527       endif;
026652130527
026653130527       // Verifica esistenza del foglio su fnfgv
026654130527       comnfv=%dec(vs1nfv:5:0);
026655130527       chain (comnfv: simfel) fnfgv01l;
026656130527       if not %found(fnfgv01l) or fgvatb<>*blanks;
026657130527          *in28=*on;
026658130527          *in90=*on;
026659130527          vidmsg=err(6);
026660130527          leavesr;
026661130527       endif;
026662130527
026663130527       endsr;
026664130528       //-------------------------------------------------------------*
026665130528       //Routine Lancio pgm di stampa                                 *
026666130528       //-------------------------------------------------------------*
026667130528       begsr sr_lancio;
026668130528
026669130528       // LANCIO PER DATA FOGLIO
026670130528       if vc1dfv>0;
026671130528       // Stampa scheda di trasporto
026672130528          if vc1sts='S';
026673130528             exsr sr_schetra;
026674130528          endif;
026675130528       // Stampa etichette borderò
026676130528          if vc1ste='S';
026677130528             clear wnfv;
026678130528             exsr sr_etich;
026679130528          endif;
026680130528       else;
026681130528       //LANCIO PER NUMERO FOGLIO
026682130528          exsr sr_elasfl;
026683130528       endif;
026691130528
026692130528       EndSr;
026693130528
026694130528       //-------------------------------------------------------------*
026695130528       //Routine Stampa scheda di trasporto                           *
026696130528       //-------------------------------------------------------------*
026697130528       begsr sr_Schetra;
026705130528       endsr;
026706130528       //-------------------------------------------------------------*
026707130528       //Routine Stampa etichette borderò                             *
026708130528       //-------------------------------------------------------------*
026709130528       begsr sr_etich  ;
026711130528       lst9dfv=vc1dfvinv;
026712130528       lst9fog=vc1fog;
026713130528       lst9nfv=%dec(wnfv:5:0);
026714130528       fnlst9r1(kpjba);
026716130528       endsr;
026717130528       //-------------------------------------------------------------*
026718130528       //Routine elaborazione sfl per lancio stampe                   *
026719130528       //-------------------------------------------------------------*
026720130528       begsr sr_elasfl ;
026721130528       s01nrr=1;
026722130528       chain s01nrr lst9s01;
026723130528       dow %found;
026724130528          if vs1nfv>*zeros;
026725130528             wnfv=vs1nfv;
026726130528       // Scheda di trasporto
026727130528             if vc1sts='S';
026728130528       //       chaino foglio viaggio per sapere se stampato o da stampare
026729130528       //       e per poi aggiornare il flag di stampa
026730130528
026731130528       //       richiamo pgm che stampa la scheda di trasporto
026732130528                exsr sr_pgmsche;
026733130528             endif;
026734130528       // Etichette
026735130528             if vc1ste='S';
026736130528                exsr sr_etich;
026737130528             endif;
026738130528          endif;
026739130528          s01nrr+=1;
026740130528          chain s01nrr lst9s01;
026741130528       enddo;
026742130528       endsr;
026743130528       //-------------------------------------------------------------*
026744130528       //Richiamo pgm che stampa la scheda di trasporto               *
026745130528       //-------------------------------------------------------------*
026746130528       begsr sr_pgmsche;
026747130528
026748130528       // ' '=Stampa, 'R'=Ristampa
026749130528       pa2flg=' ';
026750130528
026751130528       clear pa2msg;
026752130528       clear pa2f12;
026753130528       pa2fgs=%editc(simfel:'X');
026754130528       pa2prt=pa1prt;
026755130528       pa2mdt=pa1mdt;
026756130528       kpjbu=param2;
026757130528
026758130528       fnls94c(kpjba);
026759130528       //       aggiorno flag di stampa scheda di trasporto se è la prima
026760130528       //       volta che viene stampata
026761130528       endsr;
026762090402       //-------------------------------------------------------------*
026763090402       //Routine Iniziale                                             *
026764090402       //-------------------------------------------------------------*
026765090414       begsr *inzsr;
026766090402
026800090402       //Reperisco dati job
026900090917         exsr datijob;
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
156100130527Immettere numero di foglio viaggio esistente e non annullato                  06
