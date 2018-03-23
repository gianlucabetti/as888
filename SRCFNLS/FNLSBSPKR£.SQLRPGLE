000100130723      *PARMS dfactgrp(*no) actgrp(*caller) dbgview(*source)
000200130723       //==============================================================
000300151210       //?Gestione File FIBSP10F - Gestione codice AUT per filiale/cli ?
000400130723       //==============================================================
001000130723     /*PRM  dbgview(*source)
001100130723     /*END
001200130723
001300130723       //--------------------------------------------------------------
001400130723       //?Specifiche di controllo.                                     ?
001500130723       //--------------------------------------------------------------
001600130723
001700130723     h decedit('0,') datedit(*dmy/) option(*nodebugio)
002000130723
002100130723       //--------------------------------------------------------------
002200130723       //?Dichiarazione file.                                          ?
002300130723       //--------------------------------------------------------------
002700130723
002701150518       // -?File clienti/filiali abilitati
002702151210     fFIBSP03L  if   e           k disk
002703151210       // -?File cod.aut per fil/cli
002705151210     fFIBSP11L  uf a e           k disk
002902150505
002903150505       // -?Anagrafica Aut
002904150505     fFIAPD01L  If   e           k disk
003100130723
003200130723       // -?Video?
003300151210     ffnlsBSPkd cf   e             workstn
003400130723     f                                     indds(IndDspF)
003500130723     f                                     infds(InfDspF)
003501151210     f                                     sfile(lsbsS02 : S02nrr)
003600130723
003700130723       //--------------------------------------------------------------
003800130723       //?Definizione costanti.                                        ?
003900130723       //--------------------------------------------------------------
004300130723
004301151210     D digits          c                   '0123456789'
005200130723       // -?Tasti funzionali a video?
005300130723     d c_F01           c                   const(x'31')
005400130723     d c_F02           c                   const(x'32')
005500130723     d c_F03           c                   const(x'33')
005600130723     d c_F04           c                   const(x'34')
005700130723     d c_F05           c                   const(x'35')
005800130723     d c_F06           c                   const(x'36')
005900130723     d c_F07           c                   const(x'37')
006000130723     d c_F08           c                   const(x'38')
006100130723     d c_F09           c                   const(x'39')
006200130723     d c_F10           c                   const(x'3A')
006300130723     d c_F11           c                   const(x'3B')
006400130723     d c_F12           c                   const(x'3C')
006500130723     d c_F13           c                   const(x'B1')
006600130723     d c_F14           c                   const(x'B2')
006700130723     d c_F15           c                   const(x'B3')
006800130723     d c_F16           c                   const(x'B4')
006900130723     d c_F17           c                   const(x'B5')
007000130723     d c_F18           c                   const(x'B6')
007100130723     d c_F19           c                   const(x'B7')
007200130723     d c_F20           c                   const(x'B8')
007300130723     d c_F21           c                   const(x'B9')
007400130723     d c_F22           c                   const(x'BA')
007500130723     d c_F23           c                   const(x'BB')
007600130723     d c_F24           c                   const(x'BC')
007700130723     d c_Enter         c                   const(x'F1')
007800130723     d c_RollDown      c                   const(x'F4')
007900130723     d c_RollUp        c                   const(x'F5')
008000130723
008100130723       //--------------------------------------------------------------
008200130723       //?Definizione schiere.                                         ?
008300130723       //--------------------------------------------------------------
009200130723
009300130723       // -?Messaggi di errore?
009400151210     d $Msg            s             78    dim(3)  ctdata  perrcd( 1)
009500130723
009600130723       //--------------------------------------------------------------
009700130723       //?Definizione aree dati.                                       ?
009800130723       //--------------------------------------------------------------
009900130723
010000130723       // -?Dati utente?
010100130723     d §AzUte        e ds                  extname(AZUTE00F)
010200130723     d                                     dtaara
010300130723     d §DatiUte      e ds                  extname(dDatiUte)
010400130723     d                                     dtaara
010500130723
010600130723       //--------------------------------------------------------------
010700130723       //?Definizione strutture dati.                                  ?
010800130723       //--------------------------------------------------------------
010900130723
011000130723       // -?Status ds?
011100130723     d Status         sds
011200130723     d   SDSpgm          *proc
011300130723
011400130723       // -?InfDS?
011500130723     d InfDspF         ds
011600130723     d   dsp_aid             369    369a
011601151210     d  cursor               370    371a
011602151210     d  sfl_rrn              376    377i 0
011603151210     d  min_nrr              378    379i 0
011604151210     d  num_rcds             380    381i 0
011700130723
011800130723       // -?Indicatori su DspF?
011900130723     d IndDspF         ds                  inz
012000130723         // -?Abilitazione tasti funzionali?
012200130723     d   F6Attivo                      n   overlay(IndDspF : 06)
012300130724     d   F12Attivo                     n   overlay(IndDspF : 12)
012401150520         // -?Indicatori di gestione del subfile?
012403151210     d  SflDsp_N                      1n   overlay(IndDspF : 30)
012404151210     d  SflDspCtl_N                   1n   overlay(IndDspF : 31)
012405151210     d  SflNxtChg                     1n   overlay(IndDspF : 32)
012406151210     d  SflEnd                        1n   overlay(IndDspF : 33)
012500130723         // -?Emissione messaggio di errore?
012600130723     d   ErrMessage                    n   overlay(IndDspF : 28)
012700130723         // -?Indicatori per Attibuti di visualizzazione?
012801151030        // - Indicatore di protezione campi
012900130723         // -?Posizionamento cursore & segnalazione errore?
013402151210     d   PosCurPDR                     n   overlay(IndDspF : 51)
013500130723         // -?Riemissione videata?
013600130723     d   ErrGenerico                   n   overlay(IndDspF : 99)
014100130723
014200130723       // -?Parametri ricevuti?
014300130723     d KPJBA         e ds
014301151210     d                 ds
014302151210     d bsplin
014303151210     d sbsp                           3  0 dim(30) overlay(bsplin)
014400130723
015600130723
015700130723       //--------------------------------------------------------------
015800130723       //?Definizione variabili globali.                               ?
015900130723       //--------------------------------------------------------------
016000130723
016100130723       // -?Flags booleani?
016200130723     d $Fine           s               n   inz
016201151210     d $InzS02         s               n   inz(*on)
017100130723
017200130723       // -?Variabili per la gestione del video?
017400131212
017500131212       // -?Nome esteso Libreria/File del file tabella?
017600131212     d wLibFile        s             21a   inz
017700130723
017800130723       // -?Campi di comodo?
017801151210     d S02nrr          s              4  0 inz
017802151210     d s02rcd          s                   like(c02rcd)
018100131212     d wDate           s              8  0 inz
018202151211     d wpdr            s                   like(vscpdr)
018204151210     d wpdrd           s                   like(apdrsc)
018207150526     d ix              s              2  0 inz
018210150520     d s01nrr          s              4  0
018211151211     d w0070           s              7  0
018300130723
018400130723       //--------------------------------------------------------------
018500130723       //?Definizione prototipi procedure.                             ?
018600130723       //--------------------------------------------------------------
019200130723
019300130723       // -?Reperimento dati utente?
019400130723     d TIBS34ds      e ds
019500130723      /copy gaitrasrc/srcProtoPR,TIBS34R
019501151210       // -?Selezione AUT?
019502151210     d FNLV24ds      e ds                  inz        qualified
019503151210     d   D24tip      e                     inz('A')
019504151210     d   D24flg      e                     inz('R')
019505151210      /copy gaitrasrc/srcProtoPr,FNLV24R
020200130724
021900130723
022000130723       //--------------------------------------------------------------
022100130723       //?Definizione key-list.                                        ?
022200130723       //--------------------------------------------------------------
022700130723
022800130723       //--------------------------------------------------------------
022900130723       //?Riepilogo indicatori utilizzati.                             ?
023000130723       //--------------------------------------------------------------
023100130723       //--------------------------------------------------------------
023200130723
023300130723       //--------------------------------------------------------------
023400130723       //?M A I N - L I N E                                            ?
023500130723       //--------------------------------------------------------------
023600130723
023700130723     c     *Entry        plist
023800130723     c                   parm                    KPJBA
023900130723
024000130723      /free
024001151210         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
024100130723
024200130723       // -?Operazioni iniziali?
024300130723       exsr  sr_RoutInz;
024400130723
024500130723       // -?Ciclo di gestione del file video?
024600130723       DOW  $Fine = *off;
025200151210             exsr  sr_Gess02;
026600130723       ENDDO;
026700130723
026800130723       // -?Operazioni finali?
026900130723       exsr  sr_RoutEnd;
027000130723
027100130723       //--------------------------------------------------------------
027200130723       //?Operazioni iniziali.                                         ?
027300130723       //--------------------------------------------------------------
027400130723       BEGSR  sr_RoutInz;
027500130723
027501151210          $inzs02=*on;
027600130723         *inLR = *on;
027700130723
027800130723         // -?Reperimento dati job?
027900130723         exsr  sr_DatiJob;
028000130723
028100130723         // -?Impostazione nome programma a video?
028200151210         Vscpgm = SDSpgm;
032600131212
032700131212         // -?Reperimento data odierna?
032800131212         wDate = %int( %subst( %char( %dec( %timestamp() ) )
032900131212                               : 1 : 8 ) );
033000130723
033100130723       ENDSR;
033200130723
033300130723       //--------------------------------------------------------------
033400130723       //?Reperimento Dati del job (Utente/Operativi).                 ?
033500130723       //--------------------------------------------------------------
033600130723       BEGSR  sr_DatiJob;
033700130723
033800130723         in(E) §AzUte;
033900130723         if NOT %error;
034000130723           in(E) §DatiUte;
034100130723         endif;
034200130723         if %error or RSut = *blanks;
034300130723           clear TIBS34ds;
034400130723           tibs34r ( tibs34ds );
034500130723           in §AzUte;
034600130723           in §DatiUte;
034700130723         endif;
034800130723
034900130723       ENDSR;
035000130723
035001151210       //--------------------------------------------------------------
035002151210       //?Gestione SFL 02
035003151210       //--------------------------------------------------------------
035004151210       BEGSR sr_GesS02;
035005151210
035006151210         // Inizializzazione videata
035007151210         if  $InzS02 = *on;
035008151210            exsr InzS02;
035009151210         // werrsfl = *off;
035010151210            c02csr=1     ;
035011151210            c02rcd=1     ;
035012151210            $InzS02  = *off;
035013151210
035014151210         // Non trovati record
035015151210            if s02nrr=0   ;
035016151210                SflDsp_N=*on ;
035021151210            endif  ;
035028151210         endif;
035029151210
035030151210       // c02csr   contiene il numero di riga del subfile su cui era posizionato
035031151210       // impostando c02rcd   visualizzo la pagina che vedeva l'utente quando ha
035032151210       // l'ultimo tasto
0350331512102         if c02csr   > 0;
035034151210           c02rcd   = c02csr  ;
035035151210          else;
035036151210           c02rcd   = s02rcd   ;
0350371512102         endif;
035038151210       // se non so quale pagina visualizzare forzo pagina 1
0350391512102         if c02rcd   < 1;
035040151210           eval c02rcd   = 1;
0350411512102         endif;
035042151210
035043151210       // salvo il record number del subfile
035044151210       // s02rcd    = c02rcd  ;
035045151210
035046151210         // Emissione Testata e Piede con tasti funzionali abilitati
035047151210       //if  errMessage  = *off;
035048151210           write  lsbsT01;
035049151210       //endif;
035050151210
035051151210         // Emissione videata
035052151210         write  lsbsz02;
035053151211         if SflDsp_N=*on ;
035054151211            write  lsbsd01;
035055151211         endif;
035056151210         exfmt  lsbsC02;
035057151210
035058151210         reset errMessage;
035059151210         reset errGenerico;
035060151210         clear V1cmsg;
035061151210         s02nrr=1  ;
035062151210
035063151210 1       SELECT;
035064151210
035065151210         // - F3=Fine
035066151210 1         WHEN  dsp_aid = c_F03;
035069151210               $Fine = *on;
035071151210
035081151210
035082151210         // - F5=Refresh
035083151210 1         WHEN  dsp_aid = c_F05;
035089151210               $inzs02 = *on ;
035091151210
035101151210         // - Roll-Up
035102151210           WHEN  dsp_aid = c_RollUp;
035103151210
035106151210 x1      // Invio / F6
035107151210           OTHER;
035108151210
035109151211             if SflDsp_N=*on ;
035110151211               $Fine = *on;
035111151211             else ;
035112151210               exsr ContrS02 ;
035119151210
035120151210 2             if  errGenerico = *on;
035121151210                 leavesr;
035122151210 2             endif;
035124151210
035125151210
035126151210         // F6=conferma Aggiornamento
035127151211               if   dsp_aid = c_F06 ;
035128151211                 exsr ConfermaS02;
035131151211                 $Fine = *on;
035132151211               endif  ;
035133151211             endif;
035134151210
035135151210 1       ENDSL;
035136151210
035137151210       ENDSR;
035138151210       //--------------------------------------------------------------
035139151210       //?Inizializzazione SFL02
035140151210       //--------------------------------------------------------------
035141151210       BEGSR InzS02;
035142151210
035143151210       // Pulizia subfile
035144151210         SflDsp_N    = *on;
035145151210         SflDspCtl_N = *on;
035146151210         write  lsbsC02;
035147151210         SflDspCtl_N = *off;
035148151210         SflEnd      = *off;
035149151210         SflDsp_N    = *off;
035150151210         Sflnxtchg   = *off;
035151151210
035157151210
035158151210         S02nrr=0 ;
035159151210         clear V1cmsg;
035160151210         errMessage  = *off;
035161151210         errGenerico = *off;
035162151210
035179151210       // Caricamento sfl
035180151210           setll 'S' fibsp03l;
035181151210           reade 'S' fibsp03l;
035182151210           dow not %eof;
035183151210       // elaboro solo se record di cliente
035184151210              if bspksc>0;
035185151210       // elaboro solo se la filiale utente è presente fra le
035186151210       // filiali abilitate
035187151210                 if %lookup(dutpou:sbsp)>0 ;
035188151210                  exsr carsf02;
035189151210                 endif;
035190151210              endif;
035191151210              reade 'S'   fibsp03l;
035192151210           enddo;
035193151210          s02rcd    = s02nrr  ;
035194151210
035195151210         ENDSR  ;
035196151210       //--------------------------------------------------------------
035197151210       //?eseguo caricamento SFL
035198151210       //--------------------------------------------------------------
035199151210          BEGSR   carSF02    ;
035201151210
035202151210          exsr CarRECORD  ;
035210151210          poscurpdr=*off;
035214151210          s02nrr=s02nrr+1   ;
035215151210          write  lsbsS02;
035216151210
035217151210          ENDSR         ;
035218151210       //--------------------------------------------------------------
035219151210       //?Caricamento record SFL
035220151210       //--------------------------------------------------------------
035221151210       BEGSR CarRecord;
035222151210
035223151210       clear  vscpdr;
035224151210       clear  vscpdrd;
035225151210       // Per la filiale/cliente che sto elaborando recupero eventuale
035226151210       // record già presente
035227151210       chain(n) (dutpou:bspksc) fibsp11l;
035228151210       if %found(fibsp11l);
035229151210          vscpdr=%editc(bs1pdr:'X');
035230151210          chain ('A': bs1pdr) fiapd01l;
035231151210          if %found(fiapd01l) ;
035232151210             vscpdrd=apdrsc ;
035233151210          endif;
035235151210       endif;
035236151210       vscksc=bspksc;
035237151210       vsckscd=bspdes;
035300151210       ENDSR         ;
035301151210       //--------------------------------------------------------------
035302151210       //?controlli SFL02
035303151210       //--------------------------------------------------------------
035304151210       BEGSR ContrS02;
035305151210
035306151210       s02nrr=1  ;
035307151210       readc       lsbss02    ;
035308151210
035309151210 0     dow not %eof                      ;
035310151210
035337151210                exsr ctrrecsfl;
035338151210                if errgenerico=*on;
035339151210                   leavesr;
035340151210                endif;
035343151210
035344151210       readc    lsbss02    ;
035345151210 0     enddo         ;
035346151210
035347151210       ENDSR  ;
035348151210       //--------------------------------------------------------------
035349151210       //?controllo riga di sfl
035350151210       //--------------------------------------------------------------
035351151210       BEGSR ctrrecsfl;
035352151210
035353151210          poscurpdr=*off;
035356151210
035357151210       // codice aut di ritiro
035360151210       // Interrogazione anagrafica aut
035361151210       if %scan('?':vscpdr)>0;
035362151210          clear vscpdr;
035363151210          clear vscpdrd;
035364151210          exsr sr_ricpdr;
035365151211          vscpdr=wpdr;
035366151210          vscpdrd=wpdrd;
035367151210          ErrGenerico=*on;
035368151210          poscurpdr  =*on;
035369151210          exsr aggiosfl;
035370151210          leavesr;
035371151210       endif;
035372151210       // Controllo validità del codice inserito
035373151211       if vscpdr<>*blanks and vscpdr<>*zeros;
035374151210       // deve contenere numeri
035375151210           if %check(digits:VSCPDR)>0;
035376151210             ErrGenerico=*on;
035377151210             ErrMessage =*on  ;
035378151210             poscurpdr  =*on;
035379151210             clear vscpdrd;
035380151210             v1cmsg=$msg(3);
035381151210             exsr aggiosfl;
035382151210             leavesr;
035383151210           ENDIF;
035384151210       // deve esistere in anagrafica aut
035385151211           w0070=%int(vscpdr);
035386151211           chain ('A':w0070) fiapd01l;
035387151210           if not %found(fiapd01l);
035388151210             ErrGenerico=*on;
035389151210             ErrMessage =*on  ;
035390151210             poscurpdr  =*on;
035391151210             clear vscpdrd;
035392151210             v1cmsg=$msg(3);
035393151210             exsr aggiosfl;
035394151210             leavesr;
035395151210           endif;
035396151210
035399151210            VscpdrD=apdrsc;
035400151210       endif;
035447151210
035448151210         exsr aggiosfl;
035449151210       endsr;
035450151210       //--------------------------------------------------------------
035451151210       //?Aggiornamento sfl
035452151210       //--------------------------------------------------------------
035453151210       BEGSR AGGIOSFL  ;
035454151210
035455151210       if v1cmsg<>*blanks   ;
035456151210       errMessage  = *on;
035457151210       errGenerico = *on;
035458151210       c02csr=s02nrr    ;
035459151210       endif    ;
035460151210
035461151210        Sflnxtchg=*on  ;
035463151210
035464151210       update  lsbss02  ;
035465151210
035466151210       ENDSR   ;
035467151210       //--------------------------------------------------------------
035468151210       //? Ricerca Aut di ritiro su anagrafica AUT
035469151210       //--------------------------------------------------------------
035470151210       BEGSR sr_ricpdr ;
035471151210         clear wpdr;
035472151210         clear wpdrd;
035473151210         reset fnlv24ds  ;
035475151210         FNLV24ds.D24fil = DUTpou;
035478151210         kpjbu=fnlv24ds;
035479151210         FNLV24R ( kpjbA );
035480151210         FNLV24ds = kpjbU;
035484151210           if FNLV24ds.D24pdr <> *zero;
035485151211            wpdr =%editc( FNLV24ds.D24pdr  : 'X' );
035486151210            wpdrd=FNLV24ds.D24rsc;
035487151210         endif;
035488151210       endsr ;
035489151210       //--------------------------------------------------------------
035490151210       //?Conferma dati sfl2
035491151210       //--------------------------------------------------------------
035492151210       BEGSR Confermas02;
035493151210
035494151210       // Prima di scrivere, cancello i record presenti di dutpou
035495151210       exec sql delete from fibsp10f where bs1fil=:dutpou;
035496151210       s02nrr=1  ;
035498151211       chain s02nrr lsbss02    ;
035499151210
035500151211 0     dow %found     ;
035501151210        if vscpdr>*zeros;
035502151210          bs1fil=dutpou;
035503151210          bs1ksc=vscksc;
035504151211          bs1pdr=%dec(vscpdr:7:0);
035505151210          bs1duv=%int( %subst( %char( %dec( %timestamp() ) )
035506151210                               : 1 : 8 ) );
035507151210          bs1huv=%dec( %time() );
035508151210          write(e) fibsp100;
035509151210        endif;
035510151211          s02nrr+=1;
035511151211          chain s02nrr lsbss02    ;
035512151210 0     enddo         ;
035513151210
035514151210       ENDSR  ;
111500130723       //--------------------------------------------------------------
111600130723       //?Operazioni finali.                                           ?
111700130723       //--------------------------------------------------------------
111800130723       BEGSR  sr_RoutEnd;
111900130723
112900130723         //  ?Uscita?
113000130723         return;
113100130723
113200130723       ENDSR;
113300130723
113400130723      /end-free
113500130723
113600130723       //--------------------------------------------------------------
113700130723       //?Definizione schiere a tempo di compilazione.                 ?
113800130723       //--------------------------------------------------------------
114300130723** -?$Msg:?Messaggi di Errore?-----------------------------------------------*
114400151210
114500151210
115300151210Codice Aut. inesistente o non valido
