000100100803       //==============================================================
000200101027       //?Reperimento tipo invio fattura e-mail o etichetta            ?
000300100803       //==============================================================
001000100803
001100100803       //--------------------------------------------------------------
001200100803       //?Specifiche di controllo.                                     ?
001300100803       //--------------------------------------------------------------
001400100803
001500100803     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001600100809     h dftactgrp(*no)
001601101115       // - Deve essere specificato DFTACTGRP(*NO) per un prototipo che
001602101115       //   non ha la parola chiave EXTPGM.
001603101115       //  ?(vedi API QSNDRTVMOD - Retrieve Display Mode)?
001604101115
002000100803
002100100803       //--------------------------------------------------------------
002200100803       //?Dichiarazione file.
002300100803       //--------------------------------------------------------------
002400100803
002600101028     fFnspe03L  if   e           k disk
002601101027     fFnsp201L  if   e           k disk
002602101027     fTfntc01l  if   e           k disk
002603101118     fAncdp01l  if   e           k disk
002604101125     fCnabi00f  if   e           k disk
002605101116     fTABEL00F  if   e           k disk
003400100803
003500100803       // -?Video parametri di lancio?
003600101027     fTRUL57D   cf   e             workstn usropn
003700100803     f                                     indds(IndDspF)
003800100803     f                                     infds(InfDspF)
003900100803
004400100803       //--------------------------------------------------------------
004500100803       //?Definizione costanti.                                        ?
004600100803       //--------------------------------------------------------------
006600100803
006700100803       // -?Tasti funzionali a video?
006800100803     d c_F01           c                   const(x'31')
006900100803     d c_F02           c                   const(x'32')
007000100803     d c_F03           c                   const(x'33')
007100100803     d c_F04           c                   const(x'34')
007200100803     d c_F05           c                   const(x'35')
007300100803     d c_F06           c                   const(x'36')
007400100803     d c_F07           c                   const(x'37')
007500100803     d c_F08           c                   const(x'38')
007600100803     d c_F09           c                   const(x'39')
007700100803     d c_F10           c                   const(x'3A')
007800100803     d c_F11           c                   const(x'3B')
007900100803     d c_F12           c                   const(x'3C')
008000100803     d c_F13           c                   const(x'B1')
008100100803     d c_F14           c                   const(x'B2')
008200100803     d c_F15           c                   const(x'B3')
008300100803     d c_F16           c                   const(x'B4')
008400100803     d c_F17           c                   const(x'B5')
008500100803     d c_F18           c                   const(x'B6')
008600100803     d c_F19           c                   const(x'B7')
008700100803     d c_F20           c                   const(x'B8')
008800100803     d c_F21           c                   const(x'B9')
008900100803     d c_F22           c                   const(x'BA')
009000100803     d c_F23           c                   const(x'BB')
009100100803     d c_F24           c                   const(x'BC')
009200100803     d c_Enter         c                   const(x'F1')
009300100803     d c_RollDown      c                   const(x'F4')
009400100803     d c_RollUp        c                   const(x'F5')
009800100803
009900100803       //--------------------------------------------------------------
010000100803       //?Definizione schiere.                                         ?
010100100803       //--------------------------------------------------------------
010200100803
010600100803       //--------------------------------------------------------------
010700100803       //?Definizione aree dati.                                       ?
010800100803       //--------------------------------------------------------------
010900100803
011000100803       // -?Dati utente?
011100100803     d §AzUte        e ds                  extname(AZUTE00F)
011200100803     d                                     dtaara
011300100803     d §DatiUte      e ds                  extname(dDatiUte)
011400100803     d                                     dtaara
011500100803
011600100803       //--------------------------------------------------------------
011700100803       //?Definizione strutture dati.                                  ?
011800100803       //--------------------------------------------------------------
011900100803
012000100803       // -?Status ds?
012100100803     d Status         sds
012200100806     d   SDSpgm          *proc
013900100803
014000100803       // -?InfDS?
014100100803     d InfDspF         ds
014200100806     d   dsp_aid             369    369a
014300100803
014301101110      // - Indicatori su DspF
014302101110     d IndDspF         ds
014303101110         // -?Condzionamento tipo di emissione?
014304101110     d   xDs4                          n   overlay(IndDspF : 34)
014305101110        // - Indicatori di visualizzazione
014306101110     d  errMessage                    1n   overlay(IndDspF : 28)
014307101110     d  Etichetta                     1n   overlay(IndDspF : 10)
014308101110     d  Mail                          1n   overlay(IndDspF : 11)
014309101110
014400100803       // -?Parametri ricevuti?
014500100803     d KPJBA         e ds
014600101027     d TRUL57ds      e ds
014700100803
014800100803       // -?Parametri per Reperimento dati utente?
014900100803     d TIBS34ds      e ds                  inz
014901101116
014902101116      // - Ricerca/Controllo tabelle
014903101116     d TIBS02ds      e ds                  inz
014904101116
014905101116      // - Tabella TFT = Tipo Fattura
014906101116     d dtft          e ds                  inz
014907101116
014908101116      // - Tabella  FF = Frequenza fattura
014909101116     d dsff          e ds                  inz
014910101028       // - Reperimento dati anagrafici
014911101028     d TIBS69ds      e ds
014912101028     d DS_cnaco      e ds                  inz extname(CNACO00F)
014913101028     d DS_cnind      e ds                  inz extname(CNIND00F)
014914101028     d DS_cnclp      e ds                  inz extname(CNCLP00F)
014915101028     d DS_fncls      e ds                  inz extname(FNCLS00F)
014916101028
019100100803       //--------------------------------------------------------------
019200100803       //?Definizione variabili.                                       ?
019300100803       //--------------------------------------------------------------
019400100803
019500100803       // -?Flags booleani?
019600100803     d $Fine           s               n   inz
019700100803     d $InzD01         s               n   inz(*on)
019800100810     d ErrGenerico     s               n   inz
019900100803
020000100803       // -?Indici di schiera?
020100100803     d xx              s              3  0 inz
020101101028       // -?Campi di comodo
020102101028     d w_unificata     s              1    inz
020103101105     d w_Kscb          s                   like(d57ikscb)
020104101105     d w_Kscf          s                   like(d57ikscb)
020105101112     d W_eti           s              1
020106101112     d W_mail          s              1
020107101125     d wagenzia        s             16
020108101125     d wbanca          s             20
020109101125     d wdesbanca       s                   like(CLPbab)
020110101125     d wpos            s              2  0
020111101125     d wpos1           s              2  0
020112101125     d wpos2           s              2  0
020113101112
020202101112
020300100803       // -?Variabili per la gestione del video?
020400100803     d $Video          s              2    inz('D1')
020500100803
021400100803       //--------------------------------------------------------------
021500100908       //?Definizione prototipi & relativi parametri                   ?
021600100803       //--------------------------------------------------------------
021700100803
021701101105       // -?API QSNDRTVMOD (Retrieve Display Mode)?
021702101105     d QdspMode        s              1    inz
021703101105      /copy gaitrasrc/srcProtoPR,QSNRTVMOD
021704101105
021705101116      /copy gaitrasrc/srcprotopr,tibs02r
021900100803      /copy gaitrasrc/srcProtoPR,TIBS34R
021902101028      /copy gaitrasrc/srcprotopr,tibs69r
022000100803
027100100803       //--------------------------------------------------------------
027200100803       //?Definizione key-list.                                        ?
027300100803       //--------------------------------------------------------------
027800100803
027900101027       // -?File TFNTC00F?
028000101027     d k04tfntc00    e ds                  extname(TFNTC01L : *key)
028100100803     d                                     prefix(k_)   inz
028200101028       // -?File FNSPE03L?
028300101028     d k03fnspe03    e ds                  extname(FNSPE03L : *key)
028400100803     d                                     prefix(k_)   inz
028401101027       // -?File FNSP201L?
028402101027     d k03fnsp201    e ds                  extname(FNSP201L : *key)
028403101027     d                                     prefix(k_)   inz
028500100803
028600100803       //--------------------------------------------------------------
028700100803       //?M A I N - L I N E                                            ?
028800100803       //--------------------------------------------------------------
028900100803
029000100803     c     *Entry        plist
029100100803     c                   parm                    KPJBA
029200101105     c                   parm                    TRUL57DS
029300100803
029400100803      /free
029500100803
029600100803       // -?Operazioni iniziali?
029700100803       exsr  sr_RoutInz;
029800100803
029900100803       // -?Gestione window?
030000100803       DoW  Not $Fine;
030100100803         select;
030101101126           // -?F12=Ritorno?
030102101126           When  dsp_aid = c_F12;
030103101126             $Fine = *on;
030104101126
030200100803           when  $Video = 'D1';
030300100803             exsr  sr_GesD01;
030401101126
030600100803         endsl;
030700100803       EndDo;
030800100803
030900100803       // -?Operazioni finali?
031000100803       exsr  sr_RoutEnd;
031100100803
031200100803       //--------------------------------------------------------------
031300100803       //?Operazioni iniziali.                                         ?
031400100803       //--------------------------------------------------------------
031500100803       BEGSR  sr_RoutInz;
031600100803
031700100803         *inLR = *on;
033300100803
033400100803         // -?Impostazione campi "fissi"?
033600101027         k_SPEfls = 'L';
033700101027         k_SPEcod = '001';
033701101027         k_SP2cod = '001';
033702101027         k_SP2tpe = 'EM';
033703101027         k_NTCapl = 'C';
033704101027         k_NTCtnt = '84';
033800100803
033900100803         // -?Reperimento dati del lavoro?
034000100803         exsr  sr_DatiJob;
034100100809
034200100809         // -?Reperimento formato attuale display?
034300101028         clear  TR57P01;
034400100809         RetrieveDisplayMode ( QdspMode : *omit : *omit );
034500100809         xDs4 = (QdspMode = '4');
034600101028         open  TRUL57D;
034700100804
034800100804         // -?Posizionamento window?
035900100809           if  Not xDs4;
036000100927             W1rig = 11;
036100100809           else;
036200100927             W1rig = 14;
036300100809           endif;
036400100809           W1col = 2;
036600100803
036700100803       ENDSR;
036800100803
036900100803       //--------------------------------------------------------------
037000100803       //?Reperimento Dati del job (Utente/Operativi).                 ?
037100100803       //--------------------------------------------------------------
037200100803       BEGSR  sr_DatiJob;
037300100803
037400100803         in(E) §AzUte;
037500100803         if NOT %error;
037600100803           in(E) §DatiUte;
037700100803         endif;
037800100803         if %error or RSut = *blanks;
037900100803           clear TIBS34ds;
038000100803           tibs34r(tibs34ds);
038100100803           in §AzUte;
038200100803           in §DatiUte;
038300100803         endif;
038400100803
038500100803       ENDSR;
038600100803
038700100803       //--------------------------------------------------------------
038800100803       //?Gestione videata D01.                                        ?
038900100803       //--------------------------------------------------------------
039000100803       BEGSR  sr_GesD01;
039100100803
039200100803         // -?Inizializzazione videata?
039300100803         if  $InzD01;
039301101213           If D57Ipgm = 'TNTA60R' ;
039302101213              exsr  sr_InzD02;
039303101213           Else ;
039400101213              exsr  sr_InzD01;
039401101213           Endif ;
039500100803           $InzD01 = *off;
039600100803         endif;
039700100803
039800100809         // -?Emissione Piede (window) e Testata?
039801101118         write  TR57P01;
039802101118         IF D01TIT <> *BLANKS ;
040000101105         write  TR57T01;
040001101118         ENDIF ;
040100100804
040200100804         // -?Emissione videata?
040300101105         exfmt  TR57D01;
040400100804
040500100805         clear  D01msg;
040501101110         clear  errMessage;
040700100810         clear  errGenerico;
041800100803
043000100803
043100100803       ENDSR;
043200100803
043300100803       //--------------------------------------------------------------
043400100803       //?Inizializzazione videata D01.                                ?
043500100803       //--------------------------------------------------------------
043600100803       BEGSR  sr_InzD01;
043700100803
043800101027         clear  TR57D01;
043900100803
044000100805         // -?Impostazione campi a video con i parametri ricevuti?
044100101028         // ·?
044101101028         W_kscb = D57ikscb ;
044102101028         W_kscf = D57ikscf ;
044103101213         IF D57IKSCO = 0 ;
044104101213            D57Itfto  = D57ITFTB ;
044105101213            D57iffto  = D57IFFTB ;
044106101213            D57Iksco = D57IKSCB;
044107101213         ENDIF ;
044108101213
044109101213         // se codice di fatturazione uguale a zero  do errore
044110101213         If w_kscf = 0 ;
044111101213          d01tit='MANCANO LE INFORMAZIONI PER RECUPERARE I DATI FATTURAZIONE';
044112101213          leavesr ;
044113101213         endif;
044114101028
044115101112         // controllo il codice di bollettazione e recupero eventuali dati
044116101115         // se non passato il codice di fatturazione
044117101112
044118101028            clear  tibs69ds;
044119101105            I69kcp = d57ikscb;
044120101105            I69kcs = d57ikscb;
044121101117            I69kin = d57ikscb;
044122101028            TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
044123101117         If  o69err = *blanks ;
044124101117             If W_kscf =  0 ;
044126101115                D57iunib = %subst(clsflo: 2: 1);
044127101115                D57Itftb  = %editc(clptft: 'X') ;
044128101115                D57ifftb  = %editc(clpfft: 'X') ;
044129101116                d57idftb  = %subst(clpfun: 1: 1);
044130101115                D57Itfto  = %editc(clptft: 'X') ;
044131101115                D57iffto  = %editc(clpfft: 'X') ;
044132101115                D57Iksco = clpksc ;
044133101115                W_kscf   = clpscf ;
044134101115                D57Ikscf = clpscf ;
044136101117             Endif ;
044137101125         // VALORIZZO LE SPESE DI INVIO FATTURA se codice non unificato
044138101125            if d57iunib = ' ' ;
044139101117             d01sif = (indsin/1000) ;
044140101125            else ;
044141101125         // VALORIZZO LE SPESE DI INVIO FATTURA se codice non unificato
044142101125             clear  tibs69ds;
044143101125             I69kin = d57ikscf;
044144101125             TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
044145101125             If  o69err = *blanks ;
044146101125                 d01sif = (indsin/1000) ;
044147101125             endif ;
044148101125            endif ;
044149101117         endif ;
044150101115            // vado a cercare i dati del codice di fatturazione
044151101116            clear  tibs69ds;
044152101115            I69kcp = d57ikscf;
044153101115            I69kcs = d57ikscf;
044154101117            I69kin = d57ikscf;
044155101115            TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
044156101117          If  o69err = *blanks ;
044157101117         // i dati della bollettazione sono stati passati ma mancano quelli della fatturazione
044158101117            if d57itftf = ' '  ;
044159101115                D57iunif = %subst(clsflo: 2: 1);
044160101115                D57Itftf  = %editc(clptft: 'X') ;
044161101115                D57ifftf  = %editc(clpfft: 'X') ;
044162101116                d57idftf  = %subst(clpfun: 1: 1);
044163101115            Endif ;
044164101117         // codice pagamento
044165101118            d01cdp = %subst(indcdp:4:3) ;
044166101118            d01kscp = d57ikscf ;
044167101125            d01desp = 'Fatturazione'  ;
044168101125         // banca d'appoggio
044169101125            chain (indabi:indcab) CNABI00F;
044170101125            IF  %Found(CNABI00F) and ABIann <> '*';
044171101125                exsr sr_crebna;
044214101125                wDesbanca = wbanca + wagenzia;
044215101125            ENDIF;
044216101125
044217101118         // decodifica codice di pagamento
044218101118            chain ('201':d01cdp) ancdp01l ;
044219101118            If %found(ancdp01l) ;
044220101118               d01cdpd = cdpdesbrev ;
044221101118            else ;
044222101118               d01cdp = '?????????????' ;
044223101118            endif ;
044224101115         // verifico se si tratta di un'unificata / raggruppata con int.fattura diversa
044225101116            If D57Iunib = 'S' and D57iunif = 'S' ;
044226101115               clear  tibs69ds;
044227101115               I69kcp = d57ikscf;
044228101115               I69kcs = d57ikscf;
044229101115               TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
044230101115               If  o69err = *blanks and clpscf <> clpksc ;
044231101115            // imposto i dati di fatturazione in quelli di bollettazione e ricero i nuovi di fatturazione
044232101115                   D57iunib  = D57iunif ;
044233101115                   D57Itftb  = D57itftf ;
044234101115                   D57ifftb  = D57ifftf ;
044235101116                   d57idftb  = D57idftf ;
044236101115                   W_kscb   = clpksc ;
044237101115                   D57Ikscb = clpksc ;
044238101115                   D57iunif = %subst(clsflo: 2: 1);
044239101115                   D57Itftf  = %editc(clptft: 'X') ;
044240101115                   D57ifftf  = %editc(clpfft: 'X') ;
044241101116                   d57idftf  = %subst(clpfun: 1: 1);
044242101115                   W_kscf   = clpscf ;
044243101115                   D57Ikscf = clpscf ;
044244101115               Endif ;
044245101115            Endif;
044246101115          Endif;
044247101115         // ·?verifico la fattura
044248101115         // se come tipo fattura originale è uguale a 9 non viene fatturato
044249101115         If d57itfto = '9' ;
044250101116            d01tit='Codice ' + %editc(d57iksco: 'X') + ' NON VIENE FATTURATO ' ;
044251101116            leavesr ;
044252101125         else ;
044253101125            %SUBST(d01tit:31:12) = 'DATI FATTURA' ;
044254101115         Endif ;
044255101115
044256101115         // ·?Tipo fattura
044257101115
044258101116         If d57itfto = '9' ;
044259101115            d01tft = '9' ;
044260101115            d01ksct = d57iksco  ;
044261101115            d01dest = 'Bollettazione' ;
044262101115         else ;
044263101125            d01tft = d57itftf ;
044264101125            d01ksct = d57ikscf  ;
044265101125            d01dest = 'Fatturazione'  ;
044266101115         Endif ;
044267101115
044268101115         // ·?Frequenza fattura
044269101115
044270101116            d01fft = d57iffto;
044271101116            d01kscf = d57iksco  ;
044275101125            d01desf = 'Bollettazione' ;
044277101115
044278101116         // ·?Tipo Data fattura
044279101115
044280101116               d01dft = d57idftf ;
044281101116               d01kscd = d57ikscf  ;
044282101116               d01desd = 'Fatturazione'  ;
044283101117
044284101117         // ·?verifico SPESE INVIO FATTURA
044286101125                  d01kscs = d57ikscb ;
044287101125               If d57iunib = ' ' ;
044288101125                  d01dess = 'Bollettazione' ;
044289101125               else ;
044290101125                  If D57ikscb  = D57iksco  ;
044291101125                     d01kscs = d57ikscf ;
044292101125                  endif ;
044294101125                  d01dess = 'Raggruppamento';
044298101118               endif ;
044299101116
044300101028         // ·?verifico i codici luoghi e note
044301101028
044302101028         // aggancio il luogo 001 con il codice di bollettazione/unificante
044303101116            chain (K_spefls:d57ikscb:K_specod) fnspe03l ;
044304101028
044305101028    1       if %found(fnspe03l) ;
044306101116               D01reci= '001-LUOGO SPEDIZIONE FATTURA' ;
044307101112               w_eti   = 'S' ;
044309101125               d01ksc  = d57ikscb ;
044310101125               If d57iunib = ' ' ;
044311101125                  d01desK = 'Bollettazione' ;
044312101125               else ;
044313101125                  If D57ikscb  <> D57iksco  ;
044317101125                     d01desK = 'Raggruppamento';
044318101125                  else ;
044319101125                     d01desK = 'Bollettazione' ;
044320101125                  endif ;
044321101125               endif ;
044325101028
044326101028            //  verifico se c'è indirizzo mail
044327101116               chain (d57ikscb:K_sp2cod:K_sp2tpe) fnsp201l ;
044328101028
044329101028    2          if  %found(fnsp201l) and SP2est <> *blanks ;
044330101116                   W_mail  = 'S' ;
044331101109                   D01MAIL = %trim(sp2est) + ';' ;
044332101116                   D01ksc  = sp2cli ;
044333101116                   D01invio = 'VIA E-MAIL' ;
044334101028    2          else  ;
044335101028                   D01rag = sperag ;
044336101028                   D01ind = speind ;
044337101028                   D01loc = speloc ;
044338101028                   D01cap = specap ;
044339101028                   D01pro = spepro ;
044340101028                   D01naz = spenaz ;
044341101116                   D01invio = 'CARTACEO'   ;
044342101028    2          endif ;
044343101028    2       endif ;
044344101028
044345101028       // se non trovato FNSPE  cerco nella nota "84"
044346101105    1       if not %found(fnspe03l) ;
044347101116               K_ntcnk1 = '0151' + %editc(d57ikscb:'X') ;
044348101105               chain (K_ntcapl:K_ntcnk1:K_ntcnk2:K_ntctnt) Tfntc01l ;
044349101105    2          If  %found(tfntc01l) and ntcrnt <> *blanks ;
044350101116                   D01reci= '84 - E-MAIL INVIO FATTURA    ' ;
044351101116                   W_mail  = 'S' ;
044352101108                   D01MAIL = %trim(NTCRNT) + ';' ;
044353101116                   D01ksc  = D57ikscb ;
044354101116                   D01invio = 'VIA E-MAIL' ;
044355101125                   If d57iunib = ' ' ;
044357101125                       d01desK = 'Bollettazione' ;
044358101125                   else ;
044359101125                       If D57ikscb  <> D57iksco  ;
044360101125                          d01desK = 'Raggruppamento';
044361101125                       else ;
044362101125                          d01desK = 'Bollettazione' ;
044363101125                       endif ;
044364101125                   endif ;
044368101105    2          Endif;
044369101105    1       Endif;
044370101105
044371101105         // aggancio il luogo 001 con il codice di fatturazione se non trovato con il coidce di bollettazione
044372101116            If w_eti   <> 'S' and W_mail  <> 'S' ;
044373101116            chain (K_spefls:d57ikscf:K_specod) fnspe03l ;
044374101105
044375101105    1       if %found(fnspe03l) ;
044376101116               D01reci= '001-LUOGO SPEDIZIONE FATTURA' ;
044377101112               w_Eti   = 'S' ;
044378101116               D01ksc  = d57ikscf ;
044379101116               D01desk = 'Fatturazione' ;
044386101105
044387101105            //  verifico se c'è indirizzo mail
044388101116               chain (d57ikscf:K_sp2cod:K_sp2tpe) fnsp201l ;
044389101105
044390101105    2          if  %found(fnsp201l) and SP2est <> *blanks ;
044391101116                   W_mail  = 'S' ;
044392101108                   D01MAIL = %trim(sp2est) + ';' ;
044394101116                   D01invio = 'VIA E-MAIL' ;
044395101105    2          else  ;
044396101105                   D01rag = sperag ;
044397101105                   D01ind = speind ;
044398101105                   D01loc = speloc ;
044399101105                   D01cap = specap ;
044400101105                   D01pro = spepro ;
044401101105                   D01naz = spenaz ;
044403101116                   D01invio = 'CARTACEO'   ;
044404101105    2          endif ;
044405101105    2       endif ;
044406101105
044407101105       // se non trovato FNSPE  cerco nella nota "84"
044408101105    1       if not %found(fnspe03l) ;
044409101116               K_ntcnk1 = '0151' + %editc(d57ikscf:'X') ;
044410101105               chain (K_ntcapl:K_ntcnk1:K_ntcnk2:K_ntctnt) Tfntc01l ;
044411101105    2          If  %found(tfntc01l) and ntcrnt <> *blanks ;
044412101116                   D01reci= '84 - E-MAIL INVIO FATTURA    ' ;
044413101116                   D01ksc  = d57ikscf ;
044414101116                   W_mail  = 'S' ;
044415101108                   D01MAIL = %trim(ntcrnt) + ';' ;
044417101116                   D01invio = 'VIA E-MAIL' ;
044418101116                   D01desk = 'Fatturazione' ;
044421101105    2          Endif;
044422101105    1       Endif;
044423101105            Endif ;
044424101110
044425101110         // se non sono stati trovati indirizzi particolari
044426101110         // imposto come indirizzo di spedizione quello di bollettazione
044427101126         // ed invio cartaceo  (metto sempre il codice di fatturazione)
044428101112         If D01ksc  = 0 ;
044429101110            // se è il codice di fatturazione è diverso dal codice di bollettazione prendo il codica fatturazione
044430101126         // If  d57ikscf<> D57ikscb ;
044431101126                D01ksc  = d57ikscf ;
044433101126                D01desk = 'Fatturazione' ;
044437101126        //  else ;
044438101126        //      D01ksc  = d57ikscb ;
044439101126        //      If d57iunib = ' ' ;
044440101126        //         d01desK = 'Bollettazione' ;
044441101126        //      else ;
044442101126        //         If D57ikscb  <> D57iksco  ;
044443101126        //            d01desK = 'Raggruppamento';
044444101126        //         else ;
044445101126        //            d01desK = 'Bollettazione' ;
044446101126        //         endif ;
044447101126        //      endif ;
044451101126        //  endif ;
044452101116            D01invio = 'CARTACEO'   ;
044453101110         Endif ;
044454101116
044455101116         // Decodifico il tipo fattura
044456101116             clear dTFT;
044457101116             clear d01tftd ;
044458101116             clear TIBS02ds;
044459101116             T02mod = 'C';
044460101116             T02cod = 'TFT';
044461101116             T02ke1 = d01tft   ;
044462101116             T02sif = KNSIF;
044463101116             TNTBE_RicercaControllo  (kpjba : tibs02ds);
044464101116             IF  T02err = *blanks;
044465101116                 dTFT = t02uni ;
044466101116                 d01tftd = §tftdes;
044467101116             Endif ;
044468101116
044472101116
044473101116         //  Decodifico la frequenza fattura
044474101116             clear  dsff;
044475101116             clear  d01fftd ;
044476101116             TBLKut = 1;
044477101116             TBLcod = 'FF';
044478101116             TBLkey = d01fft ;
044479101116             chain (TBLkut : TBLcod : TBLkey) TABEL;
044480101116             If %found(TABEL00F);
044481101116                 If tblflg = ' '  ;
044482101116                    dsff    = TBLuni;
044483101116                    d01fftd = §ffdes;
044484101116                 Endif ;
044485101116             Endif ;
044496101116
044497101116
044498101116         //  Decodifico tipo data fattura
044499101116             clear  dsff;
044500101116             clear  d01dftd ;
044501101116             TBLkut = 1;
044502101116             TBLcod = '13';
044503101116             TBLkey = d01dft ;
044504101116             chain (TBLkut : TBLcod : TBLkey) TABEL;
044505101116             If %found(TABEL00F);
044506101116                 If tblflg = ' '  ;
044507101116                    dsff    = TBLuni;
044508101116                    d01dftd = tbluni;
044509101116                 Endif ;
044510101116             Endif ;
044521101116
044522101105
044523101110         // imposto indicatori di visualizzazione
044524101112         Etichetta = W_eti = 'S';
044525101116         Mail  = W_mail  = 'S';
044526101110
048600100915         clear  D01msg;
048800100915         clear  errGenerico;
050200100803
050300100803       ENDSR;
050400100803
050401101213
050402101213       //--------------------------------------------------------------
050403101213       //?Inizializzazione videata D01 da TNTA60R                      ?
050404101213       //--------------------------------------------------------------
050405101213       BEGSR  sr_InzD02;
050406101213
050407101213         clear  TR57D01;
050408101213
050409101213         // -?Impostazione campi a video con i parametri ricevuti?
050410101213         // ·?
050411101213         W_kscb = D57ikscb ;
050412101213         W_kscf = D57ikscf ;
050413101213
050414101213         // imposto i codici originali con quelli di bollettazione
050415101213         IF D57IKSCO = 0 ;
050416101213            D57Itfto  = D57ITFTB ;
050417101213            D57iffto  = D57IFFTB ;
050418101213            D57Iksco = D57IKSCB;
050419101213         ENDIF ;
050420101213
050421101213         // se codice di fatturazione uguale a zero  do errore
050422101213         If w_kscf = 0 ;
050423101213          d01tit='MANCANO LE INFORMAZIONI PER RECUPERARE I DATI FATTURAZIONE';
050424101213          leavesr ;
050425101213         endif;
050426101213
050445101213         // VALORIZZO LE SPESE DI INVIO FATTURA se codice non unificato
050446101213            if d57iunib = ' ' ;
050447101213             d01sif = d57isifb ;
050448101213            else ;
050449101213         // VALORIZZO LE SPESE DI INVIO FATTURA se codice unificato
050450101213             clear  tibs69ds;
050451101213             I69kin = d57ikscf;
050452101213             TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
050453101213             If  o69err = *blanks ;
050454101213                 d01sif = (indsin/1000) ;
050455101213             endif ;
050456101213            endif ;
050458101213            // vado a cercare i dati del codice di fatturazione  se fatturazione diverso da bollettazione
050459101213          if  d57ikscb <> d57ikscf ;
050460101213            clear  tibs69ds;
050461101213            I69kcp = d57ikscf;
050462101213            I69kcs = d57ikscf;
050463101213            I69kin = d57ikscf;
050464101213            TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
050465101213          If  o69err <> *blanks ;
050466101213            // se non trovato codice di fatturazione  do errore
050468101213            d01tit='MANCANO LE INFORMAZIONI PER RECUPERARE I DATI FATTURAZIONE';
050469101213            leavesr ;
050471101213          endif ;
050472101213          If  o69err = *blanks ;
050473101213            D57iunif = %subst(clsflo: 2: 1);
050474101213            D57Itftf  = %editc(clptft: 'X') ;
050475101213            D57ifftf  = %editc(clpfft: 'X') ;
050476101213            d57idftf  = %subst(clpfun: 1: 1);
050477101213         // codice pagamento
050478101213            d01cdp = %subst(indcdp:4:3) ;
050479101213            d01kscp = d57ikscf ;
050480101213            d01desp = 'Fatturazione'  ;
050481101213         // banca d'appoggio
050482101213            chain (indabi:indcab) CNABI00F;
050483101213            IF  %Found(CNABI00F) and ABIann <> '*';
050484101213                exsr sr_crebna;
050485101213                wDesbanca = wbanca + wagenzia;
050486101213            ENDIF;
050487101213          endif ;
050488101214          endif ;
050489101213            //  se fatturazione =  a bollettazione
050490101213          if  d57ikscb =  d57ikscf ;
050491101213            D57iunif = ' ';
050492101213            D57Itftf  = D57Itftb ;
050493101213            D57ifftf  = D57ifftb ;
050494101213            d57idftf  = d57idftb;
050495101213         // codice pagamento
050496101213            d01cdp = d57icdpb ;
050497101213            d01kscp = d57ikscf ;
050498101213            d01desp = 'Fatturazione'  ;
050499101213         // banca d'appoggio
050500101213            chain (d57iabib:D57icabb) CNABI00F;
050501101213            IF  %Found(CNABI00F) and ABIann <> '*';
050502101213                exsr sr_crebna;
050503101213                wDesbanca = wbanca + wagenzia;
050504101213            ENDIF;
050506101213
050507101214          Endif;
050508101213         // decodifica codice di pagamento
050509101213            chain ('201':d01cdp) ancdp01l ;
050510101213            If %found(ancdp01l) ;
050511101213               d01cdpd = cdpdesbrev ;
050512101213            else ;
050513101213               d01cdp = '?????????????' ;
050514101213            endif ;
050515101213         // verifico se si tratta di un'unificata / raggruppata con int.fattura diversa
050516101213            If D57Iunib = 'S' and D57iunif = 'S' ;
050517101213               clear  tibs69ds;
050518101213               I69kcp = d57ikscf;
050519101213               I69kcs = d57ikscf;
050520101213               TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
050521101213               If  o69err = *blanks and clpscf <> clpksc ;
050522101213            // imposto i dati di fatturazione in quelli di bollettazione e ricero i nuovi di fatturazione
050523101213                   D57iunib  = D57iunif ;
050524101213                   D57Itftb  = D57itftf ;
050525101213                   D57ifftb  = D57ifftf ;
050526101213                   d57idftb  = D57idftf ;
050527101213                   d01sif = (indsin/1000) ;
050528101213                   W_kscb   = clpksc ;
050529101213                   D57Ikscb = clpksc ;
050530101213                   D57iunif = %subst(clsflo: 2: 1);
050531101213                   D57Itftf  = %editc(clptft: 'X') ;
050532101213                   D57ifftf  = %editc(clpfft: 'X') ;
050533101213                   d57idftf  = %subst(clpfun: 1: 1);
050534101213                   W_kscf   = clpscf ;
050535101213                   D57Ikscf = clpscf ;
050536101213               Endif ;
050537101213            Endif;
050538101213         // ·?verifico la fattura
050539101213         // se come tipo fattura originale è uguale a 9 non viene fatturato
050540101213         If d57itfto = '9' ;
050541101213            d01tit='Codice ' + %editc(d57iksco: 'X') + ' NON VIENE FATTURATO ' ;
050542101213            leavesr ;
050543101213         else ;
050544101213            %SUBST(d01tit:31:12) = 'DATI FATTURA' ;
050545101213         Endif ;
050546101213
050547101213         // ·?Tipo fattura
050548101213
050549101213         If d57itfto = '9' ;
050550101213            d01tft = '9' ;
050551101213            d01ksct = d57iksco  ;
050552101213            d01dest = 'Bollettazione' ;
050553101213         else ;
050554101213            d01tft = d57itftf ;
050555101213            d01ksct = d57ikscf  ;
050556101213            d01dest = 'Fatturazione'  ;
050557101213         Endif ;
050558101213
050559101213         // ·?Frequenza fattura
050560101213
050561101213            d01fft = d57iffto;
050562101213            d01kscf = d57iksco  ;
050563101213            d01desf = 'Bollettazione' ;
050564101213
050565101213         // ·?Tipo Data fattura
050566101213
050567101213               d01dft = d57idftf ;
050568101213               d01kscd = d57ikscf  ;
050569101213               d01desd = 'Fatturazione'  ;
050570101213
050571101213         // ·?verifico SPESE INVIO FATTURA
050572101213                  d01kscs = d57ikscb ;
050573101213               If d57iunib = ' ' ;
050574101213                  d01dess = 'Bollettazione' ;
050575101213               else ;
050576101213                  If D57ikscb  = D57iksco  ;
050577101213                     d01kscs = d57ikscf ;
050578101213                  endif ;
050579101213                  d01dess = 'Raggruppamento';
050580101213               endif ;
050581101213
050582101213         // ·?verifico i codici luoghi e note
050583101213
050584101213         // aggancio il luogo 001 con il codice di bollettazione/unificante
050585101213            chain (K_spefls:d57ikscb:K_specod) fnspe03l ;
050586101213
050587101213    1       if %found(fnspe03l) ;
050588101213               D01reci= '001-LUOGO SPEDIZIONE FATTURA' ;
050589101213               w_eti   = 'S' ;
050590101213               d01ksc  = d57ikscb ;
050591101213               If d57iunib = ' ' ;
050592101213                  d01desK = 'Bollettazione' ;
050593101213               else ;
050594101213                  If D57ikscb  <> D57iksco  ;
050595101213                     d01desK = 'Raggruppamento';
050596101213                  else ;
050597101213                     d01desK = 'Bollettazione' ;
050598101213                  endif ;
050599101213               endif ;
050600101213
050601101213            //  verifico se c'è indirizzo mail
050602101213               chain (d57ikscb:K_sp2cod:K_sp2tpe) fnsp201l ;
050603101213
050604101213    2          if  %found(fnsp201l) and SP2est <> *blanks ;
050605101213                   W_mail  = 'S' ;
050606101213                   D01MAIL = %trim(sp2est) + ';' ;
050607101213                   D01ksc  = sp2cli ;
050608101213                   D01invio = 'VIA E-MAIL' ;
050609101213    2          else  ;
050610101213                   D01rag = sperag ;
050611101213                   D01ind = speind ;
050612101213                   D01loc = speloc ;
050613101213                   D01cap = specap ;
050614101213                   D01pro = spepro ;
050615101213                   D01naz = spenaz ;
050616101213                   D01invio = 'CARTACEO'   ;
050617101213    2          endif ;
050618101213    2       endif ;
050619101213
050620101213       // se non trovato FNSPE  cerco nella nota "84"
050621101213    1       if not %found(fnspe03l) ;
050622101213               K_ntcnk1 = '0151' + %editc(d57ikscb:'X') ;
050623101213               chain (K_ntcapl:K_ntcnk1:K_ntcnk2:K_ntctnt) Tfntc01l ;
050624101213    2          If  %found(tfntc01l) and ntcrnt <> *blanks ;
050625101213                   D01reci= '84 - E-MAIL INVIO FATTURA    ' ;
050626101213                   W_mail  = 'S' ;
050627101213                   D01MAIL = %trim(NTCRNT) + ';' ;
050628101213                   D01ksc  = D57ikscb ;
050629101213                   D01invio = 'VIA E-MAIL' ;
050630101213                   If d57iunib = ' ' ;
050631101213                       d01desK = 'Bollettazione' ;
050632101213                   else ;
050633101213                       If D57ikscb  <> D57iksco  ;
050634101213                          d01desK = 'Raggruppamento';
050635101213                       else ;
050636101213                          d01desK = 'Bollettazione' ;
050637101213                       endif ;
050638101213                   endif ;
050639101213    2          Endif;
050640101213    1       Endif;
050641101213
050642101213         // aggancio il luogo 001 con il codice di fatturazione se non trovato con il coidce di bollettazione
050643101213            If w_eti   <> 'S' and W_mail  <> 'S' ;
050644101213            chain (K_spefls:d57ikscf:K_specod) fnspe03l ;
050645101213
050646101213    1       if %found(fnspe03l) ;
050647101213               D01reci= '001-LUOGO SPEDIZIONE FATTURA' ;
050648101213               w_Eti   = 'S' ;
050649101213               D01ksc  = d57ikscf ;
050650101213               D01desk = 'Fatturazione' ;
050651101213
050652101213            //  verifico se c'è indirizzo mail
050653101213               chain (d57ikscf:K_sp2cod:K_sp2tpe) fnsp201l ;
050654101213
050655101213    2          if  %found(fnsp201l) and SP2est <> *blanks ;
050656101213                   W_mail  = 'S' ;
050657101213                   D01MAIL = %trim(sp2est) + ';' ;
050658101213                   D01invio = 'VIA E-MAIL' ;
050659101213    2          else  ;
050660101213                   D01rag = sperag ;
050661101213                   D01ind = speind ;
050662101213                   D01loc = speloc ;
050663101213                   D01cap = specap ;
050664101213                   D01pro = spepro ;
050665101213                   D01naz = spenaz ;
050666101213                   D01invio = 'CARTACEO'   ;
050667101213    2          endif ;
050668101213    2       endif ;
050669101213
050670101213       // se non trovato FNSPE  cerco nella nota "84"
050671101213    1       if not %found(fnspe03l) ;
050672101213               K_ntcnk1 = '0151' + %editc(d57ikscf:'X') ;
050673101213               chain (K_ntcapl:K_ntcnk1:K_ntcnk2:K_ntctnt) Tfntc01l ;
050674101213    2          If  %found(tfntc01l) and ntcrnt <> *blanks ;
050675101213                   D01reci= '84 - E-MAIL INVIO FATTURA    ' ;
050676101213                   D01ksc  = d57ikscf ;
050677101213                   W_mail  = 'S' ;
050678101213                   D01MAIL = %trim(ntcrnt) + ';' ;
050679101213                   D01invio = 'VIA E-MAIL' ;
050680101213                   D01desk = 'Fatturazione' ;
050681101213    2          Endif;
050682101213    1       Endif;
050683101213            Endif ;
050684101213
050685101213         // se non sono stati trovati indirizzi particolari
050686101213         // imposto come indirizzo di spedizione quello di bollettazione
050687101213         // ed invio cartaceo  (metto sempre il codice di fatturazione)
050688101213         If D01ksc  = 0 ;
050689101213            // se è il codice di fatturazione è diverso dal codice di bollettazione prendo il codica fatturazione
050690101213         // If  d57ikscf<> D57ikscb ;
050691101213                D01ksc  = d57ikscf ;
050692101213                D01desk = 'Fatturazione' ;
050693101213        //  else ;
050694101213        //      D01ksc  = d57ikscb ;
050695101213        //      If d57iunib = ' ' ;
050696101213        //         d01desK = 'Bollettazione' ;
050697101213        //      else ;
050698101213        //         If D57ikscb  <> D57iksco  ;
050699101213        //            d01desK = 'Raggruppamento';
050700101213        //         else ;
050701101213        //            d01desK = 'Bollettazione' ;
050702101213        //         endif ;
050703101213        //      endif ;
050704101213        //  endif ;
050705101213            D01invio = 'CARTACEO'   ;
050706101213         Endif ;
050707101213
050708101213         // Decodifico il tipo fattura
050709101213             clear dTFT;
050710101213             clear d01tftd ;
050711101213             clear TIBS02ds;
050712101213             T02mod = 'C';
050713101213             T02cod = 'TFT';
050714101213             T02ke1 = d01tft   ;
050715101213             T02sif = KNSIF;
050716101213             TNTBE_RicercaControllo  (kpjba : tibs02ds);
050717101213             IF  T02err = *blanks;
050718101213                 dTFT = t02uni ;
050719101213                 d01tftd = §tftdes;
050720101213             Endif ;
050721101213
050722101213
050723101213         //  Decodifico la frequenza fattura
050724101213             clear  dsff;
050725101213             clear  d01fftd ;
050726101213             TBLKut = 1;
050727101213             TBLcod = 'FF';
050728101213             TBLkey = d01fft ;
050729101213             chain (TBLkut : TBLcod : TBLkey) TABEL;
050730101213             If %found(TABEL00F);
050731101213                 If tblflg = ' '  ;
050732101213                    dsff    = TBLuni;
050733101213                    d01fftd = §ffdes;
050734101213                 Endif ;
050735101213             Endif ;
050736101213
050737101213
050738101213         //  Decodifico tipo data fattura
050739101213             clear  dsff;
050740101213             clear  d01dftd ;
050741101213             TBLkut = 1;
050742101213             TBLcod = '13';
050743101213             TBLkey = d01dft ;
050744101213             chain (TBLkut : TBLcod : TBLkey) TABEL;
050745101213             If %found(TABEL00F);
050746101213                 If tblflg = ' '  ;
050747101213                    dsff    = TBLuni;
050748101213                    d01dftd = tbluni;
050749101213                 Endif ;
050750101213             Endif ;
050751101213
050752101213
050753101213         // imposto indicatori di visualizzazione
050754101213         Etichetta = W_eti = 'S';
050755101213         Mail  = W_mail  = 'S';
050756101213
050757101213         clear  D01msg;
050758101213         clear  errGenerico;
050759101213
050760101213       ENDSR;
050761101213
050762101125       // -----------------------------------------------------------------------
050763101125       // Creo la descrizione della banca di appoggio in base ai codici ABI/CAB
050764101125       // -----------------------------------------------------------------------
050765101125       Begsr Sr_Crebna ;
050766101125
050767101125if  1       If abiabi <> 99999 or abicab <> 99999   ;
050768101125               Clear wagenzia ;
050769101125               Clear wbanca ;
050770101125               wbanca = abiist ;
050771101125           //  Compongo il nome della banca con località o comune
050772101125if  2          If abiage = *Blanks ;
050773101125                  Select ;
050774101125                  When abiloc <> *Blanks  ;
050775101125                       wagenzia = abiloc  ;
050776101125                  When abicom <> *Blanks  ;
050777101125                       wagenzia = abicom  ;
050778101125                  EndSl ;
050779101125           //  Compongo il nome della banca con agenzia + località o comune
050780101125   x2          Else ;
050781101125           //  Cerco il primo byte vuoto dell'agenzia
050782101125                  wpos = %checkr(' ':abiage) ;
050783101125
050784101125                  wagenzia = %subst(abiage:1:wpos) ;
050785101125                  wpos1 = wpos + 2 ;
050786101125                  wpos2 = 16 - wpos1 ;
050787101125           //  Se wpos2 inferiore a zero lo imposto a 0
050788101125                  If wpos2 < *Zeros ;
050789101125                     Clear wpos2 ;
050790101125                  EndIf ;
050791101125           //  Se posso aggiungere anche solo 1 carattere procedo
050792101125                  If  wpos > 1 and wpos < 16 ;
050793101125           //  Cerco di riempire quello che resta con la località o il comune
050794101125                      Select;
050795101125                      When abiloc <> *Blanks ;
050796101125                           wagenzia = %trim(wagenzia) + ' ' +
050797101125                                   %subst(abiloc:1:wpos2) ;
050798101125                      When      abicom <> *Blanks ;
050799101125                           wagenzia = %trim(wagenzia) + ' ' +
050800101125                                   %subst(abicom:1:wpos2) ;
050801101125                      EndSl;
050802101125                  EndIf ;
050803101125
050804101125    2          EndIf;
050805101125    1       EndIf;
050806101125
050807101125       EndSr ;
050808101125
116200100803       //--------------------------------------------------------------
116300100803       //?Operazioni finali.                                           ?
116400100803       //--------------------------------------------------------------
116500100803       BEGSR  sr_RoutEnd;
116600100803
117900100803
118000100803         // -?Uscita?
118100100803         return;
118200100803
118300100803       ENDSR;
118400100803
118500100803      /end-free
119300100804
119400100804       //--------------------------------------------------------------
119500100804       //?Messaggi di errore.                                          ?
119600100804       //--------------------------------------------------------------
119700100804
