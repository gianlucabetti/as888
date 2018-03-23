000100130723       //==============================================================
000200151210       //?Gestione File FIBSP10F - Gestione codice AUT per filiale/cli ?
000300130723       //==============================================================
000400130723     /*PRM  dbgview(*source)
000500130723     /*END
000600130723
000700130723       //--------------------------------------------------------------
000800130723       //?Specifiche di controllo.                                     ?
000900130723       //--------------------------------------------------------------
001000130723
001100130723     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001200130723
001300130723       //--------------------------------------------------------------
001400130723       //?Dichiarazione file.                                          ?
001500130723       //--------------------------------------------------------------
001600130723
001700150518       // -?File clienti/filiali abilitati
001800151210     fFIBSP03L  if   e           k disk
001900151210       // -?File cod.aut per fil/cli
002000151210     fFIBSP11L  uf a e           k disk
002100150505
002200150505       // -?Anagrafica Aut
002300150505     fFIAPD01L  If   e           k disk
002400130723
002500130723       // -?Video?
002600151210     ffnlsBSPkd cf   e             workstn
002700130723     f                                     indds(IndDspF)
002800130723     f                                     infds(InfDspF)
002900151210     f                                     sfile(lsbsS02 : S02nrr)
003000130723
003100130723       //--------------------------------------------------------------
003200130723       //?Definizione costanti.                                        ?
003300130723       //--------------------------------------------------------------
003400130723
003500151210     D digits          c                   '0123456789'
003600130723       // -?Tasti funzionali a video?
003700130723     d c_F01           c                   const(x'31')
003800130723     d c_F02           c                   const(x'32')
003900130723     d c_F03           c                   const(x'33')
004000130723     d c_F04           c                   const(x'34')
004100130723     d c_F05           c                   const(x'35')
004200130723     d c_F06           c                   const(x'36')
004300130723     d c_F07           c                   const(x'37')
004400130723     d c_F08           c                   const(x'38')
004500130723     d c_F09           c                   const(x'39')
004600130723     d c_F10           c                   const(x'3A')
004700130723     d c_F11           c                   const(x'3B')
004800130723     d c_F12           c                   const(x'3C')
004900130723     d c_F13           c                   const(x'B1')
005000130723     d c_F14           c                   const(x'B2')
005100130723     d c_F15           c                   const(x'B3')
005200130723     d c_F16           c                   const(x'B4')
005300130723     d c_F17           c                   const(x'B5')
005400130723     d c_F18           c                   const(x'B6')
005500130723     d c_F19           c                   const(x'B7')
005600130723     d c_F20           c                   const(x'B8')
005700130723     d c_F21           c                   const(x'B9')
005800130723     d c_F22           c                   const(x'BA')
005900130723     d c_F23           c                   const(x'BB')
006000130723     d c_F24           c                   const(x'BC')
006100130723     d c_Enter         c                   const(x'F1')
006200130723     d c_RollDown      c                   const(x'F4')
006300130723     d c_RollUp        c                   const(x'F5')
006400130723
006500130723       //--------------------------------------------------------------
006600130723       //?Definizione schiere.                                         ?
006700130723       //--------------------------------------------------------------
006800130723
006900130723       // -?Messaggi di errore?
007000151210     d $Msg            s             78    dim(3)  ctdata  perrcd( 1)
007100130723
007200130723       //--------------------------------------------------------------
007300130723       //?Definizione aree dati.                                       ?
007400130723       //--------------------------------------------------------------
007500130723
007600130723       // -?Dati utente?
007700130723     d §AzUte        e ds                  extname(AZUTE00F)
007800130723     d                                     dtaara
007900130723     d §DatiUte      e ds                  extname(dDatiUte)
008000130723     d                                     dtaara
008100130723
008200130723       //--------------------------------------------------------------
008300130723       //?Definizione strutture dati.                                  ?
008400130723       //--------------------------------------------------------------
008500130723
008600130723       // -?Status ds?
008700130723     d Status         sds
008800130723     d   SDSpgm          *proc
008900130723
009000130723       // -?InfDS?
009100130723     d InfDspF         ds
009200130723     d   dsp_aid             369    369a
009300151210     d  cursor               370    371a
009400151210     d  sfl_rrn              376    377i 0
009500151210     d  min_nrr              378    379i 0
009600151210     d  num_rcds             380    381i 0
009700130723
009800130723       // -?Indicatori su DspF?
009900130723     d IndDspF         ds                  inz
010000130723         // -?Abilitazione tasti funzionali?
010100130723     d   F6Attivo                      n   overlay(IndDspF : 06)
010200130724     d   F12Attivo                     n   overlay(IndDspF : 12)
010300150520         // -?Indicatori di gestione del subfile?
010400151210     d  SflDsp_N                      1n   overlay(IndDspF : 30)
010500151210     d  SflDspCtl_N                   1n   overlay(IndDspF : 31)
010600151210     d  SflNxtChg                     1n   overlay(IndDspF : 32)
010700151210     d  SflEnd                        1n   overlay(IndDspF : 33)
010800130723         // -?Emissione messaggio di errore?
010900130723     d   ErrMessage                    n   overlay(IndDspF : 28)
011000130723         // -?Indicatori per Attibuti di visualizzazione?
011100151030        // - Indicatore di protezione campi
011200130723         // -?Posizionamento cursore & segnalazione errore?
011300151210     d   PosCurPDR                     n   overlay(IndDspF : 51)
011400130723         // -?Riemissione videata?
011500130723     d   ErrGenerico                   n   overlay(IndDspF : 99)
011600130723
011700130723       // -?Parametri ricevuti?
011800130723     d KPJBA         e ds
011900151210     d                 ds
012000151210     d bsplin
012100151210     d sbsp                           3  0 dim(30) overlay(bsplin)
012200130723
012300130723
012400130723       //--------------------------------------------------------------
012500130723       //?Definizione variabili globali.                               ?
012600130723       //--------------------------------------------------------------
012700130723
012800130723       // -?Flags booleani?
012900130723     d $Fine           s               n   inz
013000151210     d $InzS02         s               n   inz(*on)
013100130723
013200130723       // -?Variabili per la gestione del video?
013300131212
013400131212       // -?Nome esteso Libreria/File del file tabella?
013500131212     d wLibFile        s             21a   inz
013600130723
013700130723       // -?Campi di comodo?
013800151210     d S02nrr          s              4  0 inz
013900151210     d s02rcd          s                   like(c02rcd)
014000131212     d wDate           s              8  0 inz
014100151211     d wpdr            s                   like(vscpdr)
014200151210     d wpdrd           s                   like(apdrsc)
014300150526     d ix              s              2  0 inz
014400150520     d s01nrr          s              4  0
014500151211     d w0070           s              7  0
014600130723
014700130723       //--------------------------------------------------------------
014800130723       //?Definizione prototipi procedure.                             ?
014900130723       //--------------------------------------------------------------
015000130723
015100130723       // -?Reperimento dati utente?
015200130723     d TIBS34ds      e ds
015300130723      /copy gaitrasrc/srcProtoPR,TIBS34R
015400151210       // -?Selezione AUT?
015500151210     d FNLV24ds      e ds                  inz        qualified
015600151210     d   D24tip      e                     inz('A')
015700151210     d   D24flg      e                     inz('R')
015800151210      /copy gaitrasrc/srcProtoPr,FNLV24R
015900130724
016000130723
016100130723       //--------------------------------------------------------------
016200130723       //?Definizione key-list.                                        ?
016300130723       //--------------------------------------------------------------
016400130723
016500130723       //--------------------------------------------------------------
016600130723       //?Riepilogo indicatori utilizzati.                             ?
016700130723       //--------------------------------------------------------------
016800130723       //--------------------------------------------------------------
016900130723
017000130723       //--------------------------------------------------------------
017100130723       //?M A I N - L I N E                                            ?
017200130723       //--------------------------------------------------------------
017300130723
017400130723     c     *Entry        plist
017500130723     c                   parm                    KPJBA
017600130723
017700130723      /free
017800151210         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
017900130723
018000130723       // -?Operazioni iniziali?
018100130723       exsr  sr_RoutInz;
018200130723
018300130723       // -?Ciclo di gestione del file video?
018400130723       DOW  $Fine = *off;
018500151210             exsr  sr_Gess02;
018600130723       ENDDO;
018700130723
018800130723       // -?Operazioni finali?
018900130723       exsr  sr_RoutEnd;
019000130723
019100130723       //--------------------------------------------------------------
019200130723       //?Operazioni iniziali.                                         ?
019300130723       //--------------------------------------------------------------
019400130723       BEGSR  sr_RoutInz;
019500130723
019600151210          $inzs02=*on;
019700130723         *inLR = *on;
019800130723
019900130723         // -?Reperimento dati job?
020000130723         exsr  sr_DatiJob;
020100130723
020200130723         // -?Impostazione nome programma a video?
020300151210         Vscpgm = SDSpgm;
020400131212
020500131212         // -?Reperimento data odierna?
020600131212         wDate = %int( %subst( %char( %dec( %timestamp() ) )
020700131212                               : 1 : 8 ) );
020800130723
020900130723       ENDSR;
021000130723
021100130723       //--------------------------------------------------------------
021200130723       //?Reperimento Dati del job (Utente/Operativi).                 ?
021300130723       //--------------------------------------------------------------
021400130723       BEGSR  sr_DatiJob;
021500130723
021600130723         in(E) §AzUte;
021700130723         if NOT %error;
021800130723           in(E) §DatiUte;
021900130723         endif;
022000130723         if %error or RSut = *blanks;
022100130723           clear TIBS34ds;
022200130723           tibs34r ( tibs34ds );
022300130723           in §AzUte;
022400130723           in §DatiUte;
022500130723         endif;
022600130723
022700130723       ENDSR;
022800130723
022900151210       //--------------------------------------------------------------
023000151210       //?Gestione SFL 02
023100151210       //--------------------------------------------------------------
023200151210       BEGSR sr_GesS02;
023300151210
023400151210         // Inizializzazione videata
023500151210         if  $InzS02 = *on;
023600151210            exsr InzS02;
023700151210         // werrsfl = *off;
023800151210            c02csr=1     ;
023900151210            c02rcd=1     ;
024000151210            $InzS02  = *off;
024100151210
024200151210         // Non trovati record
024300151210            if s02nrr=0   ;
024400151210                SflDsp_N=*on ;
024500151210            endif  ;
024600151210         endif;
024700151210
024800151210       // c02csr   contiene il numero di riga del subfile su cui era posizionato
024900151210       // impostando c02rcd   visualizzo la pagina che vedeva l'utente quando ha
025000151210       // l'ultimo tasto
0251001512102         if c02csr   > 0;
025200151210           c02rcd   = c02csr  ;
025300151210          else;
025400151210           c02rcd   = s02rcd   ;
0255001512102         endif;
025600151210       // se non so quale pagina visualizzare forzo pagina 1
0257001512102         if c02rcd   < 1;
025800151210           eval c02rcd   = 1;
0259001512102         endif;
026000151210
026100151210       // salvo il record number del subfile
026200151210       // s02rcd    = c02rcd  ;
026300151210
026400151210         // Emissione Testata e Piede con tasti funzionali abilitati
026500151210       //if  errMessage  = *off;
026600151210           write  lsbsT01;
026700151210       //endif;
026800151210
026900151210         // Emissione videata
027000151210         write  lsbsz02;
027100151211         if SflDsp_N=*on ;
027200151211            write  lsbsd01;
027300151211         endif;
027400151210         exfmt  lsbsC02;
027500151210
027600151210         reset errMessage;
027700151210         reset errGenerico;
027800151210         clear V1cmsg;
027900151210         s02nrr=1  ;
028000151210
028100151210 1       SELECT;
028200151210
028300151210         // - F3=Fine
028400151210 1         WHEN  dsp_aid = c_F03;
028500151210               $Fine = *on;
028600151210
028700151210
028800151210         // - F5=Refresh
028900151210 1         WHEN  dsp_aid = c_F05;
029000151210               $inzs02 = *on ;
029100151210
029200151210         // - Roll-Up
029300151210           WHEN  dsp_aid = c_RollUp;
029400151210
029500151210 x1      // Invio / F6
029600151210           OTHER;
029700151210
029800151211             if SflDsp_N=*on ;
029900151211               $Fine = *on;
030000151211             else ;
030100151210               exsr ContrS02 ;
030200151210
030300151210 2             if  errGenerico = *on;
030400151210                 leavesr;
030500151210 2             endif;
030600151210
030700151210
030800151210         // F6=conferma Aggiornamento
030900151211               if   dsp_aid = c_F06 ;
031000151211                 exsr ConfermaS02;
031100151211                 $Fine = *on;
031200151211               endif  ;
031300151211             endif;
031400151210
031500151210 1       ENDSL;
031600151210
031700151210       ENDSR;
031800151210       //--------------------------------------------------------------
031900151210       //?Inizializzazione SFL02
032000151210       //--------------------------------------------------------------
032100151210       BEGSR InzS02;
032200151210
032300151210       // Pulizia subfile
032400151210         SflDsp_N    = *on;
032500151210         SflDspCtl_N = *on;
032600151210         write  lsbsC02;
032700151210         SflDspCtl_N = *off;
032800151210         SflEnd      = *off;
032900151210         SflDsp_N    = *off;
033000151210         Sflnxtchg   = *off;
033100151210
033200151210
033300151210         S02nrr=0 ;
033400151210         clear V1cmsg;
033500151210         errMessage  = *off;
033600151210         errGenerico = *off;
033700151210
033800151210       // Caricamento sfl
033900151210           setll 'S' fibsp03l;
034000151210           reade 'S' fibsp03l;
034100151210           dow not %eof;
034200151210       // elaboro solo se record di cliente
034300151210              if bspksc>0;
034400151210       // elaboro solo se la filiale utente è presente fra le
034500151210       // filiali abilitate
034600151210                 if %lookup(dutpou:sbsp)>0 ;
034700151210                  exsr carsf02;
034800151210                 endif;
034900151210              endif;
035000151210              reade 'S'   fibsp03l;
035100151210           enddo;
035200151210          s02rcd    = s02nrr  ;
035300151210
035400151210         ENDSR  ;
035500151210       //--------------------------------------------------------------
035600151210       //?eseguo caricamento SFL
035700151210       //--------------------------------------------------------------
035800151210          BEGSR   carSF02    ;
035900151210
036000151210          exsr CarRECORD  ;
036100151210          poscurpdr=*off;
036200151210          s02nrr=s02nrr+1   ;
036300151210          write  lsbsS02;
036400151210
036500151210          ENDSR         ;
036600151210       //--------------------------------------------------------------
036700151210       //?Caricamento record SFL
036800151210       //--------------------------------------------------------------
036900151210       BEGSR CarRecord;
037000151210
037100151210       clear  vscpdr;
037200151210       clear  vscpdrd;
037300151210       // Per la filiale/cliente che sto elaborando recupero eventuale
037400151210       // record già presente
037500151210       chain(n) (dutpou:bspksc) fibsp11l;
037600151210       if %found(fibsp11l);
037700151210          vscpdr=%editc(bs1pdr:'X');
037800151210          chain ('A': bs1pdr) fiapd01l;
037900151210          if %found(fiapd01l) ;
038000151210             vscpdrd=apdrsc ;
038100151210          endif;
038200151210       endif;
038300151210       vscksc=bspksc;
038400151210       vsckscd=bspdes;
038500151210       ENDSR         ;
038600151210       //--------------------------------------------------------------
038700151210       //?controlli SFL02
038800151210       //--------------------------------------------------------------
038900151210       BEGSR ContrS02;
039000151210
039100151210       s02nrr=1  ;
039200151210       readc       lsbss02    ;
039300151210
039400151210 0     dow not %eof                      ;
039500151210
039600151210                exsr ctrrecsfl;
039700151210                if errgenerico=*on;
039800151210                   leavesr;
039900151210                endif;
040000151210
040100151210       readc    lsbss02    ;
040200151210 0     enddo         ;
040300151210
040400151210       ENDSR  ;
040500151210       //--------------------------------------------------------------
040600151210       //?controllo riga di sfl
040700151210       //--------------------------------------------------------------
040800151210       BEGSR ctrrecsfl;
040900151210
041000151210          poscurpdr=*off;
041100151210
041200151210       // codice aut di ritiro
041300151210       // Interrogazione anagrafica aut
041400151210       if %scan('?':vscpdr)>0;
041500151210          clear vscpdr;
041600151210          clear vscpdrd;
041700151210          exsr sr_ricpdr;
041800151211          vscpdr=wpdr;
041900151210          vscpdrd=wpdrd;
042000151210          ErrGenerico=*on;
042100151210          poscurpdr  =*on;
042200151210          exsr aggiosfl;
042300151210          leavesr;
042400151210       endif;
042500151210       // Controllo validità del codice inserito
042600151211       if vscpdr<>*blanks and vscpdr<>*zeros;
042700151210       // deve contenere numeri
042800151210           if %check(digits:VSCPDR)>0;
042900151210             ErrGenerico=*on;
043000151210             ErrMessage =*on  ;
043100151210             poscurpdr  =*on;
043200151210             clear vscpdrd;
043300151210             v1cmsg=$msg(3);
043400151210             exsr aggiosfl;
043500151210             leavesr;
043600151210           ENDIF;
043700151210       // deve esistere in anagrafica aut
043800151211           w0070=%int(vscpdr);
043900151211           chain ('A':w0070) fiapd01l;
044000151210           if not %found(fiapd01l);
044100151210             ErrGenerico=*on;
044200151210             ErrMessage =*on  ;
044300151210             poscurpdr  =*on;
044400151210             clear vscpdrd;
044500151210             v1cmsg=$msg(3);
044600151210             exsr aggiosfl;
044700151210             leavesr;
044800151210           endif;
044900151210
045000151210            VscpdrD=apdrsc;
045100151210       endif;
045200151210
045300151210         exsr aggiosfl;
045400151210       endsr;
045500151210       //--------------------------------------------------------------
045600151210       //?Aggiornamento sfl
045700151210       //--------------------------------------------------------------
045800151210       BEGSR AGGIOSFL  ;
045900151210
046000151210       if v1cmsg<>*blanks   ;
046100151210       errMessage  = *on;
046200151210       errGenerico = *on;
046300151210       c02csr=s02nrr    ;
046400151210       endif    ;
046500151210
046600151210        Sflnxtchg=*on  ;
046700151210
046800151210       update  lsbss02  ;
046900151210
047000151210       ENDSR   ;
047100151210       //--------------------------------------------------------------
047200151210       //? Ricerca Aut di ritiro su anagrafica AUT
047300151210       //--------------------------------------------------------------
047400151210       BEGSR sr_ricpdr ;
047500151210         clear wpdr;
047600151210         clear wpdrd;
047700151210         reset fnlv24ds  ;
047800151210         FNLV24ds.D24fil = DUTpou;
047900151210         kpjbu=fnlv24ds;
048000151210         FNLV24R ( kpjbA );
048100151210         FNLV24ds = kpjbU;
048200151210           if FNLV24ds.D24pdr <> *zero;
048300151211            wpdr =%editc( FNLV24ds.D24pdr  : 'X' );
048400151210            wpdrd=FNLV24ds.D24rsc;
048500151210         endif;
048600151210       endsr ;
048700151210       //--------------------------------------------------------------
048800151210       //?Conferma dati sfl2
048900151210       //--------------------------------------------------------------
049000151210       BEGSR Confermas02;
049100151210
049200151210       // Prima di scrivere, cancello i record presenti di dutpou
049300151210       exec sql delete from fibsp10f where bs1fil=:dutpou;
049400151210       s02nrr=1  ;
049500151211       chain s02nrr lsbss02    ;
049600151210
049700151211 0     dow %found     ;
049800151210        if vscpdr>*zeros;
049900151210          bs1fil=dutpou;
050000151210          bs1ksc=vscksc;
050100151211          bs1pdr=%dec(vscpdr:7:0);
050200151210          bs1duv=%int( %subst( %char( %dec( %timestamp() ) )
050300151210                               : 1 : 8 ) );
050400151210          bs1huv=%dec( %time() );
050500151210          write(e) fibsp100;
050600151210        endif;
050700151211          s02nrr+=1;
050800151211          chain s02nrr lsbss02    ;
050900151210 0     enddo         ;
051000151210
051100151210       ENDSR  ;
051200130723       //--------------------------------------------------------------
051300130723       //?Operazioni finali.                                           ?
051400130723       //--------------------------------------------------------------
051500130723       BEGSR  sr_RoutEnd;
051600130723
051700130723         //  ?Uscita?
051800130723         return;
051900130723
052000130723       ENDSR;
052100130723
052200130723      /end-free
052300130723
052400130723       //--------------------------------------------------------------
052500130723       //?Definizione schiere a tempo di compilazione.                 ?
052600130723       //--------------------------------------------------------------
052700130723** -?$Msg:?Messaggi di Errore?-----------------------------------------------*
052800151210
052900151210
053000151210Codice Aut. inesistente o non valido
