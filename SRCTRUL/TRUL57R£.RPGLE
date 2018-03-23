000100100803       //==============================================================
000200101027       //?Reperimento tipo invio fattura e-mail o etichetta            ?
000300100803       //==============================================================
000400100803
000500100803       //--------------------------------------------------------------
000600100803       //?Specifiche di controllo.                                     ?
000700100803       //--------------------------------------------------------------
000800100803
000900100803     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001000100809     h dftactgrp(*no)
001100101115       // - Deve essere specificato DFTACTGRP(*NO) per un prototipo che
001200101115       //   non ha la parola chiave EXTPGM.
001300101115       //  ?(vedi API QSNDRTVMOD - Retrieve Display Mode)?
001400101115
001500100803
001600100803       //--------------------------------------------------------------
001700100803       //?Dichiarazione file.
001800100803       //--------------------------------------------------------------
001900100803
002000101028     fFnspe03L  if   e           k disk
002100101027     fFnsp201L  if   e           k disk
002200101027     fTfntc01l  if   e           k disk
002300101118     fAncdp01l  if   e           k disk
002400101125     fCnabi00f  if   e           k disk
002500101116     fTABEL00F  if   e           k disk
002600100803
002700100803       // -?Video parametri di lancio?
002800101027     fTRUL57D   cf   e             workstn usropn
002900100803     f                                     indds(IndDspF)
003000100803     f                                     infds(InfDspF)
003100100803
003200100803       //--------------------------------------------------------------
003300100803       //?Definizione costanti.                                        ?
003400100803       //--------------------------------------------------------------
003500100803
003600100803       // -?Tasti funzionali a video?
003700100803     d c_F01           c                   const(x'31')
003800100803     d c_F02           c                   const(x'32')
003900100803     d c_F03           c                   const(x'33')
004000100803     d c_F04           c                   const(x'34')
004100100803     d c_F05           c                   const(x'35')
004200100803     d c_F06           c                   const(x'36')
004300100803     d c_F07           c                   const(x'37')
004400100803     d c_F08           c                   const(x'38')
004500100803     d c_F09           c                   const(x'39')
004600100803     d c_F10           c                   const(x'3A')
004700100803     d c_F11           c                   const(x'3B')
004800100803     d c_F12           c                   const(x'3C')
004900100803     d c_F13           c                   const(x'B1')
005000100803     d c_F14           c                   const(x'B2')
005100100803     d c_F15           c                   const(x'B3')
005200100803     d c_F16           c                   const(x'B4')
005300100803     d c_F17           c                   const(x'B5')
005400100803     d c_F18           c                   const(x'B6')
005500100803     d c_F19           c                   const(x'B7')
005600100803     d c_F20           c                   const(x'B8')
005700100803     d c_F21           c                   const(x'B9')
005800100803     d c_F22           c                   const(x'BA')
005900100803     d c_F23           c                   const(x'BB')
006000100803     d c_F24           c                   const(x'BC')
006100100803     d c_Enter         c                   const(x'F1')
006200100803     d c_RollDown      c                   const(x'F4')
006300100803     d c_RollUp        c                   const(x'F5')
006400100803
006500100803       //--------------------------------------------------------------
006600100803       //?Definizione schiere.                                         ?
006700100803       //--------------------------------------------------------------
006800100803
006900100803       //--------------------------------------------------------------
007000100803       //?Definizione aree dati.                                       ?
007100100803       //--------------------------------------------------------------
007200100803
007300100803       // -?Dati utente?
007400100803     d §AzUte        e ds                  extname(AZUTE00F)
007500100803     d                                     dtaara
007600100803     d §DatiUte      e ds                  extname(dDatiUte)
007700100803     d                                     dtaara
007800100803
007900100803       //--------------------------------------------------------------
008000100803       //?Definizione strutture dati.                                  ?
008100100803       //--------------------------------------------------------------
008200100803
008300100803       // -?Status ds?
008400100803     d Status         sds
008500100806     d   SDSpgm          *proc
008600100803
008700100803       // -?InfDS?
008800100803     d InfDspF         ds
008900100806     d   dsp_aid             369    369a
009000100803
009100101110      // - Indicatori su DspF
009200101110     d IndDspF         ds
009300101110         // -?Condzionamento tipo di emissione?
009400101110     d   xDs4                          n   overlay(IndDspF : 34)
009500101110        // - Indicatori di visualizzazione
009600101110     d  errMessage                    1n   overlay(IndDspF : 28)
009700101110     d  Etichetta                     1n   overlay(IndDspF : 10)
009800101110     d  Mail                          1n   overlay(IndDspF : 11)
009900120517     d  L136                          1n   overlay(IndDspF : 40)
010000101110
010100100803       // -?Parametri ricevuti?
010200100803     d KPJBA         e ds
010300101027     d TRUL57ds      e ds
010400100803
010500100803       // -?Parametri per Reperimento dati utente?
010600100803     d TIBS34ds      e ds                  inz
010700101116
010800101116      // - Ricerca/Controllo tabelle
010900101116     d TIBS02ds      e ds                  inz
011000101116
011100101116      // - Tabella TFT = Tipo Fattura
011200101116     d dtft          e ds                  inz
011300120516
011400120516      // - Tabella CLI = Abilitazioni clienti
011500120516     d dCLI          e ds                  inz
011600101116
011700101116      // - Tabella  FF = Frequenza fattura
011800101116     d dsff          e ds                  inz
011900101028       // - Reperimento dati anagrafici
012000101028     d TIBS69ds      e ds
012100101028     d DS_cnaco      e ds                  inz extname(CNACO00F)
012200101028     d DS_cnind      e ds                  inz extname(CNIND00F)
012300101028     d DS_cnclp      e ds                  inz extname(CNCLP00F)
012400101028     d DS_fncls      e ds                  inz extname(FNCLS00F)
012500101028
012600100803       //--------------------------------------------------------------
012700100803       //?Definizione variabili.                                       ?
012800100803       //--------------------------------------------------------------
012900100803
013000100803       // -?Flags booleani?
013100100803     d $Fine           s               n   inz
013200100803     d $InzD01         s               n   inz(*on)
013300100810     d ErrGenerico     s               n   inz
013400100803
013500100803       // -?Indici di schiera?
013600100803     d xx              s              3  0 inz
013700101028       // -?Campi di comodo
013800101028     d w_unificata     s              1    inz
013900101105     d w_Kscb          s                   like(d57ikscb)
014000101105     d w_Kscf          s                   like(d57ikscb)
014100101112     d W_eti           s              1
014200101112     d W_mail          s              1
014300101125     d wagenzia        s             16
014400101125     d wbanca          s             20
014500101125     d wdesbanca       s                   like(CLPbab)
014600101125     d wpos            s              2  0
014700101125     d wpos1           s              2  0
014800101125     d wpos2           s              2  0
014900101112
015000101112
015100100803       // -?Variabili per la gestione del video?
015200100803     d $Video          s              2    inz('D1')
015300100803
015400100803       //--------------------------------------------------------------
015500100908       //?Definizione prototipi & relativi parametri                   ?
015600100803       //--------------------------------------------------------------
015700100803
015800101105       // -?API QSNDRTVMOD (Retrieve Display Mode)?
015900101105     d QdspMode        s              1    inz
016000101105      /copy gaitrasrc/srcProtoPR,QSNRTVMOD
016100101105
016200101116      /copy gaitrasrc/srcprotopr,tibs02r
016300100803      /copy gaitrasrc/srcProtoPR,TIBS34R
016400101028      /copy gaitrasrc/srcprotopr,tibs69r
016500100803
016600100803       //--------------------------------------------------------------
016700100803       //?Definizione key-list.                                        ?
016800100803       //--------------------------------------------------------------
016900100803
017000101027       // -?File TFNTC00F?
017100101027     d k04tfntc00    e ds                  extname(TFNTC01L : *key)
017200100803     d                                     prefix(k_)   inz
017300101028       // -?File FNSPE03L?
017400101028     d k03fnspe03    e ds                  extname(FNSPE03L : *key)
017500100803     d                                     prefix(k_)   inz
017600101027       // -?File FNSP201L?
017700101027     d k03fnsp201    e ds                  extname(FNSP201L : *key)
017800101027     d                                     prefix(k_)   inz
017900100803
018000100803       //--------------------------------------------------------------
018100100803       //?M A I N - L I N E                                            ?
018200100803       //--------------------------------------------------------------
018300100803
018400100803     c     *Entry        plist
018500100803     c                   parm                    KPJBA
018600101105     c                   parm                    TRUL57DS
018700100803
018800100803      /free
018900100803
019000100803       // -?Operazioni iniziali?
019100100803       exsr  sr_RoutInz;
019200100803
019300100803       // -?Gestione window?
019400100803       DoW  Not $Fine;
019500100803         select;
019600101126           // -?F12=Ritorno?
019700101126           When  dsp_aid = c_F12;
019800101126             $Fine = *on;
019900101126
020000100803           when  $Video = 'D1';
020100100803             exsr  sr_GesD01;
020200101126
020300100803         endsl;
020400100803       EndDo;
020500100803
020600100803       // -?Operazioni finali?
020700100803       exsr  sr_RoutEnd;
020800100803
020900100803       //--------------------------------------------------------------
021000100803       //?Operazioni iniziali.                                         ?
021100100803       //--------------------------------------------------------------
021200100803       BEGSR  sr_RoutInz;
021300100803
021400100803         *inLR = *on;
021500100803
021600100803         // -?Impostazione campi "fissi"?
021700101027         k_SPEfls = 'L';
021800101027         k_SPEcod = '001';
021900101027         k_SP2cod = '001';
022000101027         k_SP2tpe = 'EM';
022100101027         k_NTCapl = 'C';
022200101027         k_NTCtnt = '84';
022300100803
022400100803         // -?Reperimento dati del lavoro?
022500100803         exsr  sr_DatiJob;
022600100809
022700100809         // -?Reperimento formato attuale display?
022800101028         clear  TR57P01;
022900100809         RetrieveDisplayMode ( QdspMode : *omit : *omit );
023000100809         xDs4 = (QdspMode = '4');
023100101028         open  TRUL57D;
023200100804
023300100804         // -?Posizionamento window?
023400100809           if  Not xDs4;
023500120517             W1rig = 09;
023600100809           else;
023700120517             W1rig = 12;
023800100809           endif;
023900100809           W1col = 2;
024000100803
024100100803       ENDSR;
024200100803
024300100803       //--------------------------------------------------------------
024400100803       //?Reperimento Dati del job (Utente/Operativi).                 ?
024500100803       //--------------------------------------------------------------
024600100803       BEGSR  sr_DatiJob;
024700100803
024800100803         in(E) §AzUte;
024900100803         if NOT %error;
025000100803           in(E) §DatiUte;
025100100803         endif;
025200100803         if %error or RSut = *blanks;
025300100803           clear TIBS34ds;
025400100803           tibs34r(tibs34ds);
025500100803           in §AzUte;
025600100803           in §DatiUte;
025700100803         endif;
025800100803
025900100803       ENDSR;
026000100803
026100100803       //--------------------------------------------------------------
026200100803       //?Gestione videata D01.                                        ?
026300100803       //--------------------------------------------------------------
026400100803       BEGSR  sr_GesD01;
026500100803
026600100803         // -?Inizializzazione videata?
026700100803         if  $InzD01;
026800101213           If D57Ipgm = 'TNTA60R' ;
026900101213              exsr  sr_InzD02;
027000101213           Else ;
027100101213              exsr  sr_InzD01;
027200101213           Endif ;
027300100803           $InzD01 = *off;
027400100803         endif;
027500100803
027600100809         // -?Emissione Piede (window) e Testata?
027700101118         write  TR57P01;
027800101118         IF D01TIT <> *BLANKS ;
027900101105         write  TR57T01;
028000101118         ENDIF ;
028100100804
028200100804         // -?Emissione videata?
028300101105         exfmt  TR57D01;
028400100804
028500100805         clear  D01msg;
028600101110         clear  errMessage;
028700100810         clear  errGenerico;
028800100803
028900100803
029000100803       ENDSR;
029100100803
029200100803       //--------------------------------------------------------------
029300100803       //?Inizializzazione videata D01.                                ?
029400100803       //--------------------------------------------------------------
029500100803       BEGSR  sr_InzD01;
029600100803
029700101027         clear  TR57D01;
029800100803
029900100805         // -?Impostazione campi a video con i parametri ricevuti?
030000101028         // ·?
030100101028         W_kscb = D57ikscb ;
030200101028         W_kscf = D57ikscf ;
030300101213         IF D57IKSCO = 0 ;
030400101213            D57Itfto  = D57ITFTB ;
030500101213            D57iffto  = D57IFFTB ;
030600101213            D57Iksco = D57IKSCB;
030700101213         ENDIF ;
030800101213
030900101213         // se codice di fatturazione uguale a zero  do errore
031000101213         If w_kscf = 0 ;
031100101213          d01tit='MANCANO LE INFORMAZIONI PER RECUPERARE I DATI FATTURAZIONE';
031200101213          leavesr ;
031300101213         endif;
031400101028
031500101112         // controllo il codice di bollettazione e recupero eventuali dati
031600101115         // se non passato il codice di fatturazione
031700101112
031800101028            clear  tibs69ds;
031900101105            I69kcp = d57ikscb;
032000101105            I69kcs = d57ikscb;
032100101117            I69kin = d57ikscb;
032200101028            TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
032300101117         If  o69err = *blanks ;
032400101117             If W_kscf =  0 ;
032500101115                D57iunib = %subst(clsflo: 2: 1);
032600101115                D57Itftb  = %editc(clptft: 'X') ;
032700101115                D57ifftb  = %editc(clpfft: 'X') ;
032800101116                d57idftb  = %subst(clpfun: 1: 1);
032900101115                D57Itfto  = %editc(clptft: 'X') ;
033000101115                D57iffto  = %editc(clpfft: 'X') ;
033100101115                D57Iksco = clpksc ;
033200101115                W_kscf   = clpscf ;
033300101115                D57Ikscf = clpscf ;
033400101117             Endif ;
033500101125         // VALORIZZO LE SPESE DI INVIO FATTURA se codice non unificato
033600101125            if d57iunib = ' ' ;
033700101117             d01sif = (indsin/1000) ;
033800101125            else ;
033900101125         // VALORIZZO LE SPESE DI INVIO FATTURA se codice non unificato
034000101125             clear  tibs69ds;
034100101125             I69kin = d57ikscf;
034200101125             TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
034300101125             If  o69err = *blanks ;
034400101125                 d01sif = (indsin/1000) ;
034500101125             endif ;
034600101125            endif ;
034700101117         endif ;
034800101115            // vado a cercare i dati del codice di fatturazione
034900101116            clear  tibs69ds;
035000101115            I69kcp = d57ikscf;
035100101115            I69kcs = d57ikscf;
035200101117            I69kin = d57ikscf;
035300101115            TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
035400101117          If  o69err = *blanks ;
035500101117         // i dati della bollettazione sono stati passati ma mancano quelli della fatturazione
035600101117            if d57itftf = ' '  ;
035700101115                D57iunif = %subst(clsflo: 2: 1);
035800101115                D57Itftf  = %editc(clptft: 'X') ;
035900101115                D57ifftf  = %editc(clpfft: 'X') ;
036000101116                d57idftf  = %subst(clpfun: 1: 1);
036100101115            Endif ;
036200101117         // codice pagamento
036300101118            d01cdp = %subst(indcdp:4:3) ;
036400101118            d01kscp = d57ikscf ;
036500101125            d01desp = 'Fatturazione'  ;
036600101125         // banca d'appoggio
036700101125            chain (indabi:indcab) CNABI00F;
036800101125            IF  %Found(CNABI00F) and ABIann <> '*';
036900101125                exsr sr_crebna;
037000101125                wDesbanca = wbanca + wagenzia;
037100101125            ENDIF;
037200101125
037300101118         // decodifica codice di pagamento
037400101118            chain ('201':d01cdp) ancdp01l ;
037500101118            If %found(ancdp01l) ;
037600101118               d01cdpd = cdpdesbrev ;
037700101118            else ;
037800101118               d01cdp = '?????????????' ;
037900101118            endif ;
038000101115         // verifico se si tratta di un'unificata / raggruppata con int.fattura diversa
038100101116            If D57Iunib = 'S' and D57iunif = 'S' ;
038200101115               clear  tibs69ds;
038300101115               I69kcp = d57ikscf;
038400101115               I69kcs = d57ikscf;
038500101115               TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
038600101115               If  o69err = *blanks and clpscf <> clpksc ;
038700101115            // imposto i dati di fatturazione in quelli di bollettazione e ricero i nuovi di fatturazione
038800101115                   D57iunib  = D57iunif ;
038900101115                   D57Itftb  = D57itftf ;
039000101115                   D57ifftb  = D57ifftf ;
039100101116                   d57idftb  = D57idftf ;
039200101115                   W_kscb   = clpksc ;
039300101115                   D57Ikscb = clpksc ;
039400101115                   D57iunif = %subst(clsflo: 2: 1);
039500101115                   D57Itftf  = %editc(clptft: 'X') ;
039600101115                   D57ifftf  = %editc(clpfft: 'X') ;
039700101116                   d57idftf  = %subst(clpfun: 1: 1);
039800101115                   W_kscf   = clpscf ;
039900101115                   D57Ikscf = clpscf ;
040000101115               Endif ;
040100101115            Endif;
040200101115          Endif;
040300101115         // ·?verifico la fattura
040400101115         // se come tipo fattura originale è uguale a 9 non viene fatturato
040500101115         If d57itfto = '9' ;
040600101116            d01tit='Codice ' + %editc(d57iksco: 'X') + ' NON VIENE FATTURATO ' ;
040700101116            leavesr ;
040800101125         else ;
040900101125            %SUBST(d01tit:31:12) = 'DATI FATTURA' ;
041000101115         Endif ;
041100101115
041200101115         // ·?Tipo fattura
041300101115
041400101116         If d57itfto = '9' ;
041500101115            d01tft = '9' ;
041600101115            d01ksct = d57iksco  ;
041700101115            d01dest = 'Bollettazione' ;
041800101115         else ;
041900101125            d01tft = d57itftf ;
042000101125            d01ksct = d57ikscf  ;
042100101125            d01dest = 'Fatturazione'  ;
042200101115         Endif ;
042300101115
042400101115         // ·?Frequenza fattura
042500101115
042600101116            d01fft = d57iffto;
042700101116            d01kscf = d57iksco  ;
042800101125            d01desf = 'Bollettazione' ;
042900101115
043000101116         // ·?Tipo Data fattura
043100101115
043200101116               d01dft = d57idftf ;
043300101116               d01kscd = d57ikscf  ;
043400101116               d01desd = 'Fatturazione'  ;
043500101117
043600101117         // ·?verifico SPESE INVIO FATTURA
043700101125                  d01kscs = d57ikscb ;
043800101125               If d57iunib = ' ' ;
043900101125                  d01dess = 'Bollettazione' ;
044000101125               else ;
044100101125                  If D57ikscb  = D57iksco  ;
044200101125                     d01kscs = d57ikscf ;
044300101125                  endif ;
044400101125                  d01dess = 'Raggruppamento';
044500101118               endif ;
044600101116
044700101028         // ·?verifico i codici luoghi e note
044800101028
044900101028         // aggancio il luogo 001 con il codice di bollettazione/unificante
045000101116            chain (K_spefls:d57ikscb:K_specod) fnspe03l ;
045100101028
045200101028    1       if %found(fnspe03l) ;
045300101116               D01reci= '001-LUOGO SPEDIZIONE FATTURA' ;
045400101112               w_eti   = 'S' ;
045500101125               d01ksc  = d57ikscb ;
045600101125               If d57iunib = ' ' ;
045700101125                  d01desK = 'Bollettazione' ;
045800101125               else ;
045900101125                  If D57ikscb  <> D57iksco  ;
046000101125                     d01desK = 'Raggruppamento';
046100101125                  else ;
046200101125                     d01desK = 'Bollettazione' ;
046300101125                  endif ;
046400101125               endif ;
046500101028
046600101028            //  verifico se c'è indirizzo mail
046700101116               chain (d57ikscb:K_sp2cod:K_sp2tpe) fnsp201l ;
046800101028
046900101028    2          if  %found(fnsp201l) and SP2est <> *blanks ;
047000101116                   W_mail  = 'S' ;
047100101109                   D01MAIL = %trim(sp2est) + ';' ;
047200101116                   D01ksc  = sp2cli ;
047300101116                   D01invio = 'VIA E-MAIL' ;
047400101028    2          else  ;
047500101028                   D01rag = sperag ;
047600101028                   D01ind = speind ;
047700101028                   D01loc = speloc ;
047800101028                   D01cap = specap ;
047900101028                   D01pro = spepro ;
048000101028                   D01naz = spenaz ;
048100101116                   D01invio = 'CARTACEO'   ;
048200101028    2          endif ;
048300101028    2       endif ;
048400101028
048500101028       // se non trovato FNSPE  cerco nella nota "84"
048600101105    1       if not %found(fnspe03l) ;
048700101116               K_ntcnk1 = '0151' + %editc(d57ikscb:'X') ;
048800101105               chain (K_ntcapl:K_ntcnk1:K_ntcnk2:K_ntctnt) Tfntc01l ;
048900101105    2          If  %found(tfntc01l) and ntcrnt <> *blanks ;
049000101116                   D01reci= '84 - E-MAIL INVIO FATTURA    ' ;
049100101116                   W_mail  = 'S' ;
049200101108                   D01MAIL = %trim(NTCRNT) + ';' ;
049300101116                   D01ksc  = D57ikscb ;
049400101116                   D01invio = 'VIA E-MAIL' ;
049500101125                   If d57iunib = ' ' ;
049600101125                       d01desK = 'Bollettazione' ;
049700101125                   else ;
049800101125                       If D57ikscb  <> D57iksco  ;
049900101125                          d01desK = 'Raggruppamento';
050000101125                       else ;
050100101125                          d01desK = 'Bollettazione' ;
050200101125                       endif ;
050300101125                   endif ;
050400101105    2          Endif;
050500101105    1       Endif;
050600101105
050700101105         // aggancio il luogo 001 con il codice di fatturazione se non trovato con il coidce di bollettazione
050800101116            If w_eti   <> 'S' and W_mail  <> 'S' ;
050900101116            chain (K_spefls:d57ikscf:K_specod) fnspe03l ;
051000101105
051100101105    1       if %found(fnspe03l) ;
051200101116               D01reci= '001-LUOGO SPEDIZIONE FATTURA' ;
051300101112               w_Eti   = 'S' ;
051400101116               D01ksc  = d57ikscf ;
051500101116               D01desk = 'Fatturazione' ;
051600101105
051700101105            //  verifico se c'è indirizzo mail
051800101116               chain (d57ikscf:K_sp2cod:K_sp2tpe) fnsp201l ;
051900101105
052000101105    2          if  %found(fnsp201l) and SP2est <> *blanks ;
052100101116                   W_mail  = 'S' ;
052200101108                   D01MAIL = %trim(sp2est) + ';' ;
052300101116                   D01invio = 'VIA E-MAIL' ;
052400101105    2          else  ;
052500101105                   D01rag = sperag ;
052600101105                   D01ind = speind ;
052700101105                   D01loc = speloc ;
052800101105                   D01cap = specap ;
052900101105                   D01pro = spepro ;
053000101105                   D01naz = spenaz ;
053100101116                   D01invio = 'CARTACEO'   ;
053200101105    2          endif ;
053300101105    2       endif ;
053400101105
053500101105       // se non trovato FNSPE  cerco nella nota "84"
053600101105    1       if not %found(fnspe03l) ;
053700101116               K_ntcnk1 = '0151' + %editc(d57ikscf:'X') ;
053800101105               chain (K_ntcapl:K_ntcnk1:K_ntcnk2:K_ntctnt) Tfntc01l ;
053900101105    2          If  %found(tfntc01l) and ntcrnt <> *blanks ;
054000101116                   D01reci= '84 - E-MAIL INVIO FATTURA    ' ;
054100101116                   D01ksc  = d57ikscf ;
054200101116                   W_mail  = 'S' ;
054300101108                   D01MAIL = %trim(ntcrnt) + ';' ;
054400101116                   D01invio = 'VIA E-MAIL' ;
054500101116                   D01desk = 'Fatturazione' ;
054600101105    2          Endif;
054700101105    1       Endif;
054800101105            Endif ;
054900101110
055000101110         // se non sono stati trovati indirizzi particolari
055100101110         // imposto come indirizzo di spedizione quello di bollettazione
055200101126         // ed invio cartaceo  (metto sempre il codice di fatturazione)
055300101112         If D01ksc  = 0 ;
055400101110            // se è il codice di fatturazione è diverso dal codice di bollettazione prendo il codica fatturazione
055500101126         // If  d57ikscf<> D57ikscb ;
055600101126                D01ksc  = d57ikscf ;
055700101126                D01desk = 'Fatturazione' ;
055800101126        //  else ;
055900101126        //      D01ksc  = d57ikscb ;
056000101126        //      If d57iunib = ' ' ;
056100101126        //         d01desK = 'Bollettazione' ;
056200101126        //      else ;
056300101126        //         If D57ikscb  <> D57iksco  ;
056400101126        //            d01desK = 'Raggruppamento';
056500101126        //         else ;
056600101126        //            d01desK = 'Bollettazione' ;
056700101126        //         endif ;
056800101126        //      endif ;
056900101126        //  endif ;
057000101116            D01invio = 'CARTACEO'   ;
057100101110         Endif ;
057200101116
057300101116         // Decodifico il tipo fattura
057400101116             clear dTFT;
057500101116             clear d01tftd ;
057600101116             clear TIBS02ds;
057700101116             T02mod = 'C';
057800101116             T02cod = 'TFT';
057900101116             T02ke1 = d01tft   ;
058000101116             T02sif = KNSIF;
058100101116             TNTBE_RicercaControllo  (kpjba : tibs02ds);
058200101116             IF  T02err = *blanks;
058300101116                 dTFT = t02uni ;
058400101116                 d01tftd = §tftdes;
058500101116             Endif ;
058600101116
058700101116
058800101116         //  Decodifico la frequenza fattura
058900101116             clear  dsff;
059000101116             clear  d01fftd ;
059100101116             TBLKut = 1;
059200101116             TBLcod = 'FF';
059300101116             TBLkey = d01fft ;
059400101116             chain (TBLkut : TBLcod : TBLkey) TABEL;
059500101116             If %found(TABEL00F);
059600101116                 If tblflg = ' '  ;
059700101116                    dsff    = TBLuni;
059800101116                    d01fftd = §ffdes;
059900101116                 Endif ;
060000101116             Endif ;
060100101116
060200101116
060300101116         //  Decodifico tipo data fattura
060400101116             clear  dsff;
060500101116             clear  d01dftd ;
060600101116             TBLkut = 1;
060700101116             TBLcod = '13';
060800101116             TBLkey = d01dft ;
060900101116             chain (TBLkut : TBLcod : TBLkey) TABEL;
061000101116             If %found(TABEL00F);
061100101116                 If tblflg = ' '  ;
061200101116                    dsff    = TBLuni;
061300101116                    d01dftd = tbluni;
061400101116                 Endif ;
061500101116             Endif ;
061600101116
061700101105
061800101110         // imposto indicatori di visualizzazione
061900101112         Etichetta = W_eti = 'S';
062000101116         Mail  = W_mail  = 'S';
062100120516
062200120516
062300120516         // -?Cerco su tabella CLI (con codice fatturazione) se fattura separta Legge 136?
062400120517           L136 = *off;
062500120516           clear dCLI;
062600120516           clear D01fts;
062700120516           clear TIBS02ds;
062800120516           T02mod = 'C';
062900120516           T02cod = 'CLI';
063000120516           T02ke1 = %editc(D57ikscf:'X');
063100120516           T02sif = KNSIF;
063200120516           TNTBE_RicercaControllo  (kpjba : tibs02ds);
063300120516           IF  T02err = *blanks;
063400120516             dCLI = T02uni;
063500120516           ENDIF;
063600120516           IF  §CLIfts = 'S';
063700120516             D01fts = 'SI';
063800120517             L136 = *on;
063900120516           ELSE;
064000120516             D01fts = 'NO';
064100120517             L136 = *off;
064200120516           ENDIF;
064300120516           D01kscfts = D57ikscf;
064400120516           D01desfts = 'Fatturazione';
064500120516
064600101110
064700100915         clear  D01msg;
064800100915         clear  errGenerico;
064900100803
065000100803       ENDSR;
065100100803
065200101213
065300101213       //--------------------------------------------------------------
065400101213       //?Inizializzazione videata D01 da TNTA60R                      ?
065500101213       //--------------------------------------------------------------
065600101213       BEGSR  sr_InzD02;
065700101213
065800101213         clear  TR57D01;
065900101213
066000101213         // -?Impostazione campi a video con i parametri ricevuti?
066100101213         // ·?
066200101213         W_kscb = D57ikscb ;
066300101213         W_kscf = D57ikscf ;
066400101213
066500101213         // imposto i codici originali con quelli di bollettazione
066600101213         IF D57IKSCO = 0 ;
066700101213            D57Itfto  = D57ITFTB ;
066800101213            D57iffto  = D57IFFTB ;
066900101213            D57Iksco = D57IKSCB;
067000101213         ENDIF ;
067100101213
067200101213         // se codice di fatturazione uguale a zero  do errore
067300101213         If w_kscf = 0 ;
067400101213          d01tit='MANCANO LE INFORMAZIONI PER RECUPERARE I DATI FATTURAZIONE';
067500101213          leavesr ;
067600101213         endif;
067700101213
067800101213         // VALORIZZO LE SPESE DI INVIO FATTURA se codice non unificato
067900101213            if d57iunib = ' ' ;
068000101213             d01sif = d57isifb ;
068100101213            else ;
068200101213         // VALORIZZO LE SPESE DI INVIO FATTURA se codice unificato
068300101213             clear  tibs69ds;
068400101213             I69kin = d57ikscf;
068500101213             TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
068600101213             If  o69err = *blanks ;
068700101213                 d01sif = (indsin/1000) ;
068800101213             endif ;
068900101213            endif ;
069000101213            // vado a cercare i dati del codice di fatturazione  se fatturazione diverso da bollettazione
069100101213          if  d57ikscb <> d57ikscf ;
069200101213            clear  tibs69ds;
069300101213            I69kcp = d57ikscf;
069400101213            I69kcs = d57ikscf;
069500101213            I69kin = d57ikscf;
069600101213            TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
069700101213          If  o69err <> *blanks ;
069800101213            // se non trovato codice di fatturazione  do errore
069900101213            d01tit='MANCANO LE INFORMAZIONI PER RECUPERARE I DATI FATTURAZIONE';
070000101213            leavesr ;
070100101213          endif ;
070200101213          If  o69err = *blanks ;
070300101213            D57iunif = %subst(clsflo: 2: 1);
070400101213            D57Itftf  = %editc(clptft: 'X') ;
070500101213            D57ifftf  = %editc(clpfft: 'X') ;
070600101213            d57idftf  = %subst(clpfun: 1: 1);
070700101213         // codice pagamento
070800101213            d01cdp = %subst(indcdp:4:3) ;
070900101213            d01kscp = d57ikscf ;
071000101213            d01desp = 'Fatturazione'  ;
071100101213         // banca d'appoggio
071200101213            chain (indabi:indcab) CNABI00F;
071300101213            IF  %Found(CNABI00F) and ABIann <> '*';
071400101213                exsr sr_crebna;
071500101213                wDesbanca = wbanca + wagenzia;
071600101213            ENDIF;
071700101213          endif ;
071800101214          endif ;
071900101213            //  se fatturazione =  a bollettazione
072000101213          if  d57ikscb =  d57ikscf ;
072100101213            D57iunif = ' ';
072200101213            D57Itftf  = D57Itftb ;
072300101213            D57ifftf  = D57ifftb ;
072400101213            d57idftf  = d57idftb;
072500101213         // codice pagamento
072600101213            d01cdp = d57icdpb ;
072700101213            d01kscp = d57ikscf ;
072800101213            d01desp = 'Fatturazione'  ;
072900101213         // banca d'appoggio
073000101213            chain (d57iabib:D57icabb) CNABI00F;
073100101213            IF  %Found(CNABI00F) and ABIann <> '*';
073200101213                exsr sr_crebna;
073300101213                wDesbanca = wbanca + wagenzia;
073400101213            ENDIF;
073500101213
073600101214          Endif;
073700101213         // decodifica codice di pagamento
073800101213            chain ('201':d01cdp) ancdp01l ;
073900101213            If %found(ancdp01l) ;
074000101213               d01cdpd = cdpdesbrev ;
074100101213            else ;
074200101213               d01cdp = '?????????????' ;
074300101213            endif ;
074400101213         // verifico se si tratta di un'unificata / raggruppata con int.fattura diversa
074500101213            If D57Iunib = 'S' and D57iunif = 'S' ;
074600101213               clear  tibs69ds;
074700101213               I69kcp = d57ikscf;
074800101213               I69kcs = d57ikscf;
074900101213               TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
075000101213               If  o69err = *blanks and clpscf <> clpksc ;
075100101213            // imposto i dati di fatturazione in quelli di bollettazione e ricero i nuovi di fatturazione
075200101213                   D57iunib  = D57iunif ;
075300101213                   D57Itftb  = D57itftf ;
075400101213                   D57ifftb  = D57ifftf ;
075500101213                   d57idftb  = D57idftf ;
075600101213                   d01sif = (indsin/1000) ;
075700101213                   W_kscb   = clpksc ;
075800101213                   D57Ikscb = clpksc ;
075900101213                   D57iunif = %subst(clsflo: 2: 1);
076000101213                   D57Itftf  = %editc(clptft: 'X') ;
076100101213                   D57ifftf  = %editc(clpfft: 'X') ;
076200101213                   d57idftf  = %subst(clpfun: 1: 1);
076300101213                   W_kscf   = clpscf ;
076400101213                   D57Ikscf = clpscf ;
076500101213               Endif ;
076600101213            Endif;
076700101213         // ·?verifico la fattura
076800101213         // se come tipo fattura originale è uguale a 9 non viene fatturato
076900101213         If d57itfto = '9' ;
077000101213            d01tit='Codice ' + %editc(d57iksco: 'X') + ' NON VIENE FATTURATO ' ;
077100101213            leavesr ;
077200101213         else ;
077300101213            %SUBST(d01tit:31:12) = 'DATI FATTURA' ;
077400101213         Endif ;
077500101213
077600101213         // ·?Tipo fattura
077700101213
077800101213         If d57itfto = '9' ;
077900101213            d01tft = '9' ;
078000101213            d01ksct = d57iksco  ;
078100101213            d01dest = 'Bollettazione' ;
078200101213         else ;
078300101213            d01tft = d57itftf ;
078400101213            d01ksct = d57ikscf  ;
078500101213            d01dest = 'Fatturazione'  ;
078600101213         Endif ;
078700101213
078800101213         // ·?Frequenza fattura
078900101213
079000101213            d01fft = d57iffto;
079100101213            d01kscf = d57iksco  ;
079200101213            d01desf = 'Bollettazione' ;
079300101213
079400101213         // ·?Tipo Data fattura
079500101213
079600101213               d01dft = d57idftf ;
079700101213               d01kscd = d57ikscf  ;
079800101213               d01desd = 'Fatturazione'  ;
079900101213
080000101213         // ·?verifico SPESE INVIO FATTURA
080100101213                  d01kscs = d57ikscb ;
080200101213               If d57iunib = ' ' ;
080300101213                  d01dess = 'Bollettazione' ;
080400101213               else ;
080500101213                  If D57ikscb  = D57iksco  ;
080600101213                     d01kscs = d57ikscf ;
080700101213                  endif ;
080800101213                  d01dess = 'Raggruppamento';
080900101213               endif ;
081000101213
081100101213         // ·?verifico i codici luoghi e note
081200101213
081300101213         // aggancio il luogo 001 con il codice di bollettazione/unificante
081400101213            chain (K_spefls:d57ikscb:K_specod) fnspe03l ;
081500101213
081600101213    1       if %found(fnspe03l) ;
081700101213               D01reci= '001-LUOGO SPEDIZIONE FATTURA' ;
081800101213               w_eti   = 'S' ;
081900101213               d01ksc  = d57ikscb ;
082000101213               If d57iunib = ' ' ;
082100101213                  d01desK = 'Bollettazione' ;
082200101213               else ;
082300101213                  If D57ikscb  <> D57iksco  ;
082400101213                     d01desK = 'Raggruppamento';
082500101213                  else ;
082600101213                     d01desK = 'Bollettazione' ;
082700101213                  endif ;
082800101213               endif ;
082900101213
083000101213            //  verifico se c'è indirizzo mail
083100101213               chain (d57ikscb:K_sp2cod:K_sp2tpe) fnsp201l ;
083200101213
083300101213    2          if  %found(fnsp201l) and SP2est <> *blanks ;
083400101213                   W_mail  = 'S' ;
083500101213                   D01MAIL = %trim(sp2est) + ';' ;
083600101213                   D01ksc  = sp2cli ;
083700101213                   D01invio = 'VIA E-MAIL' ;
083800101213    2          else  ;
083900101213                   D01rag = sperag ;
084000101213                   D01ind = speind ;
084100101213                   D01loc = speloc ;
084200101213                   D01cap = specap ;
084300101213                   D01pro = spepro ;
084400101213                   D01naz = spenaz ;
084500101213                   D01invio = 'CARTACEO'   ;
084600101213    2          endif ;
084700101213    2       endif ;
084800101213
084900101213       // se non trovato FNSPE  cerco nella nota "84"
085000101213    1       if not %found(fnspe03l) ;
085100101213               K_ntcnk1 = '0151' + %editc(d57ikscb:'X') ;
085200101213               chain (K_ntcapl:K_ntcnk1:K_ntcnk2:K_ntctnt) Tfntc01l ;
085300101213    2          If  %found(tfntc01l) and ntcrnt <> *blanks ;
085400101213                   D01reci= '84 - E-MAIL INVIO FATTURA    ' ;
085500101213                   W_mail  = 'S' ;
085600101213                   D01MAIL = %trim(NTCRNT) + ';' ;
085700101213                   D01ksc  = D57ikscb ;
085800101213                   D01invio = 'VIA E-MAIL' ;
085900101213                   If d57iunib = ' ' ;
086000101213                       d01desK = 'Bollettazione' ;
086100101213                   else ;
086200101213                       If D57ikscb  <> D57iksco  ;
086300101213                          d01desK = 'Raggruppamento';
086400101213                       else ;
086500101213                          d01desK = 'Bollettazione' ;
086600101213                       endif ;
086700101213                   endif ;
086800101213    2          Endif;
086900101213    1       Endif;
087000101213
087100101213         // aggancio il luogo 001 con il codice di fatturazione se non trovato con il coidce di bollettazione
087200101213            If w_eti   <> 'S' and W_mail  <> 'S' ;
087300101213            chain (K_spefls:d57ikscf:K_specod) fnspe03l ;
087400101213
087500101213    1       if %found(fnspe03l) ;
087600101213               D01reci= '001-LUOGO SPEDIZIONE FATTURA' ;
087700101213               w_Eti   = 'S' ;
087800101213               D01ksc  = d57ikscf ;
087900101213               D01desk = 'Fatturazione' ;
088000101213
088100101213            //  verifico se c'è indirizzo mail
088200101213               chain (d57ikscf:K_sp2cod:K_sp2tpe) fnsp201l ;
088300101213
088400101213    2          if  %found(fnsp201l) and SP2est <> *blanks ;
088500101213                   W_mail  = 'S' ;
088600101213                   D01MAIL = %trim(sp2est) + ';' ;
088700101213                   D01invio = 'VIA E-MAIL' ;
088800101213    2          else  ;
088900101213                   D01rag = sperag ;
089000101213                   D01ind = speind ;
089100101213                   D01loc = speloc ;
089200101213                   D01cap = specap ;
089300101213                   D01pro = spepro ;
089400101213                   D01naz = spenaz ;
089500101213                   D01invio = 'CARTACEO'   ;
089600101213    2          endif ;
089700101213    2       endif ;
089800101213
089900101213       // se non trovato FNSPE  cerco nella nota "84"
090000101213    1       if not %found(fnspe03l) ;
090100101213               K_ntcnk1 = '0151' + %editc(d57ikscf:'X') ;
090200101213               chain (K_ntcapl:K_ntcnk1:K_ntcnk2:K_ntctnt) Tfntc01l ;
090300101213    2          If  %found(tfntc01l) and ntcrnt <> *blanks ;
090400101213                   D01reci= '84 - E-MAIL INVIO FATTURA    ' ;
090500101213                   D01ksc  = d57ikscf ;
090600101213                   W_mail  = 'S' ;
090700101213                   D01MAIL = %trim(ntcrnt) + ';' ;
090800101213                   D01invio = 'VIA E-MAIL' ;
090900101213                   D01desk = 'Fatturazione' ;
091000101213    2          Endif;
091100101213    1       Endif;
091200101213            Endif ;
091300101213
091400101213         // se non sono stati trovati indirizzi particolari
091500101213         // imposto come indirizzo di spedizione quello di bollettazione
091600101213         // ed invio cartaceo  (metto sempre il codice di fatturazione)
091700101213         If D01ksc  = 0 ;
091800101213            // se è il codice di fatturazione è diverso dal codice di bollettazione prendo il codica fatturazione
091900101213         // If  d57ikscf<> D57ikscb ;
092000101213                D01ksc  = d57ikscf ;
092100101213                D01desk = 'Fatturazione' ;
092200101213        //  else ;
092300101213        //      D01ksc  = d57ikscb ;
092400101213        //      If d57iunib = ' ' ;
092500101213        //         d01desK = 'Bollettazione' ;
092600101213        //      else ;
092700101213        //         If D57ikscb  <> D57iksco  ;
092800101213        //            d01desK = 'Raggruppamento';
092900101213        //         else ;
093000101213        //            d01desK = 'Bollettazione' ;
093100101213        //         endif ;
093200101213        //      endif ;
093300101213        //  endif ;
093400101213            D01invio = 'CARTACEO'   ;
093500101213         Endif ;
093600101213
093700101213         // Decodifico il tipo fattura
093800101213             clear dTFT;
093900101213             clear d01tftd ;
094000101213             clear TIBS02ds;
094100101213             T02mod = 'C';
094200101213             T02cod = 'TFT';
094300101213             T02ke1 = d01tft   ;
094400101213             T02sif = KNSIF;
094500101213             TNTBE_RicercaControllo  (kpjba : tibs02ds);
094600101213             IF  T02err = *blanks;
094700101213                 dTFT = t02uni ;
094800101213                 d01tftd = §tftdes;
094900101213             Endif ;
095000101213
095100101213
095200101213         //  Decodifico la frequenza fattura
095300101213             clear  dsff;
095400101213             clear  d01fftd ;
095500101213             TBLKut = 1;
095600101213             TBLcod = 'FF';
095700101213             TBLkey = d01fft ;
095800101213             chain (TBLkut : TBLcod : TBLkey) TABEL;
095900101213             If %found(TABEL00F);
096000101213                 If tblflg = ' '  ;
096100101213                    dsff    = TBLuni;
096200101213                    d01fftd = §ffdes;
096300101213                 Endif ;
096400101213             Endif ;
096500101213
096600101213
096700101213         //  Decodifico tipo data fattura
096800101213             clear  dsff;
096900101213             clear  d01dftd ;
097000101213             TBLkut = 1;
097100101213             TBLcod = '13';
097200101213             TBLkey = d01dft ;
097300101213             chain (TBLkut : TBLcod : TBLkey) TABEL;
097400101213             If %found(TABEL00F);
097500101213                 If tblflg = ' '  ;
097600101213                    dsff    = TBLuni;
097700101213                    d01dftd = tbluni;
097800101213                 Endif ;
097900101213             Endif ;
098000101213
098100101213
098200101213         // imposto indicatori di visualizzazione
098300101213         Etichetta = W_eti = 'S';
098400101213         Mail  = W_mail  = 'S';
098500120516
098600120516
098700120516         // -?Cerco su tabella CLI (con codice fatturazione) se fattura separta Legge 136?
098800120517           L136 = *off;
098900120516           clear dCLI;
099000120516           clear D01fts;
099100120516           clear TIBS02ds;
099200120516           T02mod = 'C';
099300120516           T02cod = 'CLI';
099400120516           T02ke1 = %editc(D57ikscf:'X');
099500120516           T02sif = KNSIF;
099600120516           TNTBE_RicercaControllo  (kpjba : tibs02ds);
099700120516           IF  T02err = *blanks;
099800120516             dCLI = T02uni;
099900120516           ENDIF;
100000120516           IF  §CLIfts = 'S';
100100120516             D01fts = 'SI';
100200120517             L136 = *on;
100300120516           ELSE;
100400120516             D01fts = 'NO';
100500120517             L136 = *off;
100600120516           ENDIF;
100700120516           D01kscfts = D57ikscf;
100800120516           D01desfts = 'Fatturazione';
100900120516
101000101213
101100101213         clear  D01msg;
101200101213         clear  errGenerico;
101300101213
101400101213       ENDSR;
101500101213
101600101125       // -----------------------------------------------------------------------
101700101125       // Creo la descrizione della banca di appoggio in base ai codici ABI/CAB
101800101125       // -----------------------------------------------------------------------
101900101125       Begsr Sr_Crebna ;
102000101125
102100101125if  1       If abiabi <> 99999 or abicab <> 99999   ;
102200101125               Clear wagenzia ;
102300101125               Clear wbanca ;
102400101125               wbanca = abiist ;
102500101125           //  Compongo il nome della banca con località o comune
102600101125if  2          If abiage = *Blanks ;
102700101125                  Select ;
102800101125                  When abiloc <> *Blanks  ;
102900101125                       wagenzia = abiloc  ;
103000101125                  When abicom <> *Blanks  ;
103100101125                       wagenzia = abicom  ;
103200101125                  EndSl ;
103300101125           //  Compongo il nome della banca con agenzia + località o comune
103400101125   x2          Else ;
103500101125           //  Cerco il primo byte vuoto dell'agenzia
103600101125                  wpos = %checkr(' ':abiage) ;
103700101125
103800101125                  wagenzia = %subst(abiage:1:wpos) ;
103900101125                  wpos1 = wpos + 2 ;
104000101125                  wpos2 = 16 - wpos1 ;
104100101125           //  Se wpos2 inferiore a zero lo imposto a 0
104200101125                  If wpos2 < *Zeros ;
104300101125                     Clear wpos2 ;
104400101125                  EndIf ;
104500101125           //  Se posso aggiungere anche solo 1 carattere procedo
104600101125                  If  wpos > 1 and wpos < 16 ;
104700101125           //  Cerco di riempire quello che resta con la località o il comune
104800101125                      Select;
104900101125                      When abiloc <> *Blanks ;
105000101125                           wagenzia = %trim(wagenzia) + ' ' +
105100101125                                   %subst(abiloc:1:wpos2) ;
105200101125                      When      abicom <> *Blanks ;
105300101125                           wagenzia = %trim(wagenzia) + ' ' +
105400101125                                   %subst(abicom:1:wpos2) ;
105500101125                      EndSl;
105600101125                  EndIf ;
105700101125
105800101125    2          EndIf;
105900101125    1       EndIf;
106000101125
106100101125       EndSr ;
106200101125
106300100803       //--------------------------------------------------------------
106400100803       //?Operazioni finali.                                           ?
106500100803       //--------------------------------------------------------------
106600100803       BEGSR  sr_RoutEnd;
106700100803
106800100803
106900100803         // -?Uscita?
107000100803         return;
107100100803
107200100803       ENDSR;
107300100803
107400100803      /end-free
107500100804
107600100804       //--------------------------------------------------------------
107700100804       //?Messaggi di errore.                                          ?
107800100804       //--------------------------------------------------------------
107900100804
