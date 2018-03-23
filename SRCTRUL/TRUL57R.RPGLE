000100100803       //==============================================================
000200101027       //?Reperimento tipo invio fattura e-mail o etichetta            ?
000300100803       //==============================================================
000400100803     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000500100803
000600100803       //--------------------------------------------------------------
000700100803       //?Dichiarazione file.
000800100803       //--------------------------------------------------------------
000900101028     fFnspe03L  if   e           k disk
001000101027     fFnsp201L  if   e           k disk
001100101027     fTfntc01l  if   e           k disk
001200101118     fAncdp01l  if   e           k disk
001300101125     fCnabi00f  if   e           k disk
001400101116     fTABEL00F  if   e           k disk
001500170725     fANSPI01L  if   e           k disk
001600100803
001700100803       // -?Video parametri di lancio?
001800170720     fTRUL57D   cf   e             workstn
001900100803     f                                     indds(IndDspF)
002000100803     f                                     infds(InfDspF)
002100100803
002200100803       //--------------------------------------------------------------
002300100803       //?Definizione costanti.                                        ?
002400100803       //--------------------------------------------------------------
002500100803       // -?Tasti funzionali a video?
002600100803     d c_F01           c                   const(x'31')
002700100803     d c_F02           c                   const(x'32')
002800100803     d c_F03           c                   const(x'33')
002900100803     d c_F04           c                   const(x'34')
003000100803     d c_F05           c                   const(x'35')
003100100803     d c_F06           c                   const(x'36')
003200100803     d c_F07           c                   const(x'37')
003300100803     d c_F08           c                   const(x'38')
003400100803     d c_F09           c                   const(x'39')
003500100803     d c_F10           c                   const(x'3A')
003600100803     d c_F11           c                   const(x'3B')
003700100803     d c_F12           c                   const(x'3C')
003800100803     d c_F13           c                   const(x'B1')
003900100803     d c_F14           c                   const(x'B2')
004000100803     d c_F15           c                   const(x'B3')
004100100803     d c_F16           c                   const(x'B4')
004200100803     d c_F17           c                   const(x'B5')
004300100803     d c_F18           c                   const(x'B6')
004400100803     d c_F19           c                   const(x'B7')
004500100803     d c_F20           c                   const(x'B8')
004600100803     d c_F21           c                   const(x'B9')
004700100803     d c_F22           c                   const(x'BA')
004800100803     d c_F23           c                   const(x'BB')
004900100803     d c_F24           c                   const(x'BC')
005000100803     d c_Enter         c                   const(x'F1')
005100100803     d c_RollDown      c                   const(x'F4')
005200100803     d c_RollUp        c                   const(x'F5')
005300100803
005400100803       //--------------------------------------------------------------
005500100803       //?Definizione schiere.                                         ?
005600100803       //--------------------------------------------------------------
005700100803
005800100803       //--------------------------------------------------------------
005900100803       //?Definizione aree dati.                                       ?
006000100803       //--------------------------------------------------------------
006100100803       // -?Dati utente?
006200100803     d §AzUte        e ds                  extname(AZUTE00F)
006300100803     d                                     dtaara
006400100803     d §DatiUte      e ds                  extname(dDatiUte)
006500100803     d                                     dtaara
006600100803
006700100803       //--------------------------------------------------------------
006800100803       //?Definizione strutture dati.                                  ?
006900100803       //--------------------------------------------------------------
007000100803       // -?Status ds?
007100100803     d Status         sds
007200100806     d   SDSpgm          *proc
007300100803
007400100803       // -?InfDS?
007500100803     d InfDspF         ds
007600100806     d   dsp_aid             369    369a
007700100803
007800101110      // - Indicatori su DspF
007900101110     d IndDspF         ds
008000101110        // - Indicatori di visualizzazione
008100101110     d  errMessage                    1n   overlay(IndDspF : 28)
008200101110     d  Etichetta                     1n   overlay(IndDspF : 10)
008300101110     d  Mail                          1n   overlay(IndDspF : 11)
008400120517     d  L136                          1n   overlay(IndDspF : 40)
008500170725     d  Split                         1n   overlay(IndDspF : 41)
008600101110
008700100803       // -?Parametri ricevuti?
008800100803     d KPJBA         e ds
008900101027     d TRUL57ds      e ds
009000100803
009100100803       // -?Parametri per Reperimento dati utente?
009200100803     d TIBS34ds      e ds                  inz
009300101116
009400101116      // - Ricerca/Controllo tabelle
009500101116     d TIBS02ds      e ds                  inz
009600101116
009700101116      // - Tabella TFT = Tipo Fattura
009800101116     d dtft          e ds                  inz
009900120516
010000120516      // - Tabella CLI = Abilitazioni clienti
010100120516     d dCLI          e ds                  inz
010200101116
010300101116      // - Tabella  FF = Frequenza fattura
010400101116     d dsff          e ds                  inz
010500101028       // - Reperimento dati anagrafici
010600101028     d TIBS69ds      e ds
010700101028     d DS_cnaco      e ds                  inz extname(CNACO00F)
010800101028     d DS_cnind      e ds                  inz extname(CNIND00F)
010900101028     d DS_cnclp      e ds                  inz extname(CNCLP00F)
011000101028     d DS_fncls      e ds                  inz extname(FNCLS00F)
011100101028
011200100803       //--------------------------------------------------------------
011300100803       //?Definizione variabili.                                       ?
011400100803       //--------------------------------------------------------------
011500100803       // -?Flags booleani?
011600100803     d $Fine           s               n   inz
011700100803     d $InzD01         s               n   inz(*on)
011800100810     d ErrGenerico     s               n   inz
011900100803
012000100803       // -?Indici di schiera?
012100100803     d xx              s              3  0 inz
012200101028       // -?Campi di comodo
012300101028     d w_unificata     s              1    inz
012400170901     d w_Ksc           s                   like(d57ikscb)
012500101112     d W_eti           s              1
012600101112     d W_mail          s              1
012700101125     d wagenzia        s             16
012800101125     d wbanca          s             20
012900101125     d wdesbanca       s                   like(CLPbab)
013000101125     d wpos            s              2  0
013100101125     d wpos1           s              2  0
013200101125     d wpos2           s              2  0
013300170727     d wdataalfa       s              8a
013400170901     d wcdp            s                   like(D57icdpb)
013500101112
013600100803       // -?Variabili per la gestione del video?
013700100803     d $Video          s              2    inz('D1')
013800100803
013900100803       //--------------------------------------------------------------
014000100908       //?Definizione prototipi & relativi parametri                   ?
014100100803       //--------------------------------------------------------------
014200101116      /copy gaitrasrc/srcprotopr,tibs02r
014300100803      /copy gaitrasrc/srcProtoPR,TIBS34R
014400101028      /copy gaitrasrc/srcprotopr,tibs69r
014500100803
014600100803       //--------------------------------------------------------------
014700100803       //?Definizione key-list.                                        ?
014800100803       //--------------------------------------------------------------
014900101027       // -?File TFNTC00F?
015000101027     d k04tfntc00    e ds                  extname(TFNTC01L : *key)
015100100803     d                                     prefix(k_)   inz
015200101028       // -?File FNSPE03L?
015300101028     d k03fnspe03    e ds                  extname(FNSPE03L : *key)
015400100803     d                                     prefix(k_)   inz
015500101027       // -?File FNSP201L?
015600101027     d k03fnsp201    e ds                  extname(FNSP201L : *key)
015700101027     d                                     prefix(k_)   inz
015800100803
015900100803       //--------------------------------------------------------------
016000100803       //?M A I N - L I N E                                            ?
016100100803       //--------------------------------------------------------------
016200100803     c     *Entry        plist
016300100803     c                   parm                    KPJBA
016400101105     c                   parm                    TRUL57DS
016500100803
016600100803      /free
016700100803
016800100803       // -?Operazioni iniziali?
016900100803       exsr  sr_RoutInz;
017000100803
017100100803       // -?Gestione window?
017200100803       DoW  Not $Fine;
017300100803         select;
017400101126           // -?F12=Ritorno?
017500101126           When  dsp_aid = c_F12;
017600101126             $Fine = *on;
017700101126
017800100803           when  $Video = 'D1';
017900100803             exsr  sr_GesD01;
018000101126
018100100803         endsl;
018200100803       EndDo;
018300100803
018400100803       // -?Operazioni finali?
018500100803       exsr  sr_RoutEnd;
018600100803
018700100803       //--------------------------------------------------------------
018800100803       //?Operazioni iniziali.                                         ?
018900100803       //--------------------------------------------------------------
019000100803       BEGSR  sr_RoutInz;
019100100803
019200100803         *inLR = *on;
019300100803
019400100803         // -?Impostazione campi "fissi"?
019500101027         k_SPEfls = 'L';
019600101027         k_SPEcod = '001';
019700101027         k_SP2cod = '001';
019800101027         k_SP2tpe = 'EM';
019900101027         k_NTCapl = 'C';
020000101027         k_NTCtnt = '84';
020100100803
020200100803         // -?Reperimento dati del lavoro?
020300100803         exsr  sr_DatiJob;
020400170725         clear D02tit;
020500170831
020600180205         //VTCpgm = D57Ipgm;
020700100803
020800100803       ENDSR;
020900100803
021000100803       //--------------------------------------------------------------
021100100803       //?Reperimento Dati del job (Utente/Operativi).                 ?
021200100803       //--------------------------------------------------------------
021300100803       BEGSR  sr_DatiJob;
021400100803
021500100803         in(E) §AzUte;
021600100803         if NOT %error;
021700100803           in(E) §DatiUte;
021800100803         endif;
021900100803         if %error or RSut = *blanks;
022000100803           clear TIBS34ds;
022100100803           tibs34r(tibs34ds);
022200100803           in §AzUte;
022300100803           in §DatiUte;
022400100803         endif;
022500100803
022600100803       ENDSR;
022700100803
022800100803       //--------------------------------------------------------------
022900100803       //?Gestione videata D01.                                        ?
023000100803       //--------------------------------------------------------------
023100100803       BEGSR  sr_GesD01;
023200100803
023300100803         // -?Inizializzazione videata?
023400100803         if  $InzD01;
023500170831           exsr sr_InzD01;
023600170911           exsr DatiSplitPayment;
023700100803           $InzD01 = *off;
023800100803         endif;
023900170724
024000170724         // -?Emissione videata vuota con errore?
024100170725         IF  D02tit <> *blanks;
024200180205         //write  TR57T01;
024300170724           exfmt  TR57D02;
024400100803
024500100804         // -?Emissione videata?
024600170724         ELSE;
024700180205         //write  TR57T01;
024800170724           exfmt  TR57D01;
024900170724         ENDIF;
025000100804
025100100805         clear  D01msg;
025200101110         clear  errMessage;
025300100810         clear  errGenerico;
025400100803
025500100803       ENDSR;
025600100803
025700100803       //--------------------------------------------------------------
025800100803       //?Inizializzazione videata D01.                                ?
025900100803       //--------------------------------------------------------------
026000170831       BEGSR  sr_InzD01;
026100101213
026200101213         clear  TR57D01;
026300170901         clear w_ksc;
026400170728
026500170728         // -?Decodifico il codice bollettazione passato?
026600170728         clear tibs69ds;
026700170728         clear ds_cnaco;
026800170728         I69kac = d57ikscb;
026900170728         TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
027000170728         IF  o69err = *blanks ;
027100170728           V01ksc = D57ikscb;
027200170728           V01rag = ACOrag;
027300170728         ENDIF;
027400170728         clear tibs69ds;
027500170728         clear ds_cnaco;
027600101213
027700101213         // -?Impostazione campi a video con i parametri ricevuti?
027800101213
027900101213         // imposto i codici originali con quelli di bollettazione
028000101213         IF D57IKSCO = 0 ;
028100101213            D57Itfto  = D57ITFTB ;
028200101213            D57iffto  = D57IFFTB ;
028300101213            D57Iksco = D57IKSCB;
028400101213         ENDIF ;
028500101213
028600101213         // se codice di fatturazione uguale a zero  do errore
028700170901         If d57ikscf = 0 ;
028800170725          d02tit='MANCANO LE INFORMAZIONI PER RECUPERARE I DATI FATTURAZIONE';
028900101213          leavesr ;
029000101213         endif;
029100101213
029200101213         // VALORIZZO LE SPESE DI INVIO FATTURA se codice non unificato
029300101213            if d57iunib = ' ' ;
029400101213             d01sif = d57isifb ;
029500170901         // salvo anche codice per ricerca invio fattura
029600170901             w_ksc = D57ikscb;
029700101213            else ;
029800101213         // VALORIZZO LE SPESE DI INVIO FATTURA se codice unificato
029900101213             clear  tibs69ds;
030000101213             I69kin = d57ikscf;
030100101213             TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
030200101213             If  o69err = *blanks ;
030300101213                 d01sif = (indsin/1000) ;
030400101213             endif ;
030500170901         // salvo anche codice per ricerca invio fattura
030600170901             w_ksc = D57ikscf;
030700101213            endif ;
030800170901
030900170901          // vado a cercare i dati del codice di fatturazione  se fatturazione diverso da bollettazione
031000101213          if  d57ikscb <> d57ikscf ;
031100101213            clear  tibs69ds;
031200101213            I69kcp = d57ikscf;
031300101213            I69kcs = d57ikscf;
031400101213            I69kin = d57ikscf;
031500101213            TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
031600101213          If  o69err <> *blanks ;
031700101213            // se non trovato codice di fatturazione  do errore
031800170725            d02tit='MANCANO LE INFORMAZIONI PER RECUPERARE I DATI FATTURAZIONE';
031900101213            leavesr ;
032000101213          endif ;
032100101213          If  o69err = *blanks ;
032200101213            D57iunif = %subst(clsflo: 2: 1);
032300101213            D57Itftf  = %editc(clptft: 'X') ;
032400101213            D57ifftf  = %editc(clpfft: 'X') ;
032500101213            d57idftf  = %subst(clpfun: 1: 1);
032600101213          endif ;
032700101214          endif ;
032800170901
032900170901          //  se fatturazione =  a bollettazione
033000101213          if  d57ikscb =  d57ikscf ;
033100101213            D57iunif = ' ';
033200101213            D57Itftf  = D57Itftb ;
033300101213            D57ifftf  = D57ifftb ;
033400101213            d57idftf  = d57idftb;
033500101214          Endif;
033600170901
033700101213         // verifico se si tratta di un'unificata / raggruppata con int.fattura diversa
033800101213            If D57Iunib = 'S' and D57iunif = 'S' ;
033900101213               clear  tibs69ds;
034000101213               I69kcp = d57ikscf;
034100101213               I69kcs = d57ikscf;
034200101213               TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
034300101213               If  o69err = *blanks and clpscf <> clpksc ;
034400101213            // imposto i dati di fatturazione in quelli di bollettazione e ricero i nuovi di fatturazione
034500101213                   D57iunib  = D57iunif ;
034600101213                   D57Itftb  = D57itftf ;
034700101213                   D57ifftb  = D57ifftf ;
034800101213                   d57idftb  = D57idftf ;
034900101213                   d01sif = (indsin/1000) ;
035000170901                   w_ksc = D57ikscf;
035100101213                   D57Ikscb = clpksc ;
035200101213                   D57iunif = %subst(clsflo: 2: 1);
035300101213                   D57Itftf  = %editc(clptft: 'X') ;
035400101213                   D57ifftf  = %editc(clpfft: 'X') ;
035500101213                   d57idftf  = %subst(clpfun: 1: 1);
035600101213                   D57Ikscf = clpscf ;
035700101213               Endif ;
035800101213            Endif;
035900170901
036000101213         // ·?verifico la fattura
036100101213         // se come tipo fattura originale è uguale a 9 non viene fatturato
036200101213         If d57itfto = '9' ;
036300101213            d01tit='Codice ' + %editc(d57iksco: 'X') + ' NON VIENE FATTURATO ' ;
036400101213            leavesr ;
036500101213         else ;
036600170725            %SUBST(d01tit:31:12) = 'DATI FATTURA' ;
036700101213         Endif ;
036800101213
036900101213         // ·?Tipo fattura
037000101213         If d57itfto = '9' ;
037100101213            d01tft = '9' ;
037200101213            d01ksct = d57iksco  ;
037300101213            d01dest = 'Bollettazione' ;
037400101213         else ;
037500101213            d01tft = d57itftf ;
037600101213            d01ksct = d57ikscf  ;
037700101213            d01dest = 'Fatturazione'  ;
037800101213         Endif ;
037900101213
038000101213         // ·?Frequenza fattura
038100101213            d01fft = d57iffto;
038200101213            d01kscf = d57iksco  ;
038300101213            d01desf = 'Bollettazione' ;
038400170901
038500170901         // ·?Codice Pagamento e Banca appoggio
038600170901            IF  D57iunib = *blanks and D57ikscb = D57ikscf;
038700170901              D01cdp = D57icdpb;
038800170901            ELSE;
038900170901              D01cdp = %subst(indcdp:4:3);
039000170901            ENDIF;
039100170901            D01kscp = d57ikscf;
039200170901            D01desp = 'Fatturazione';
039300101213
039400101213         // ·?Tipo Data fattura
039500101213               d01dft = d57idftf ;
039600101213               d01kscd = d57ikscf  ;
039700101213               d01desd = 'Fatturazione'  ;
039800101213
039900101213         // ·?verifico SPESE INVIO FATTURA
040000101213                  d01kscs = d57ikscb ;
040100101213               If d57iunib = ' ' ;
040200101213                  d01dess = 'Bollettazione' ;
040300101213               else ;
040400101213                  If D57ikscb  = D57iksco  ;
040500101213                     d01kscs = d57ikscf ;
040600101213                  endif ;
040700101213                  d01dess = 'Raggruppamento';
040800101213               endif ;
040900101213
041000101213         // ·?verifico i codici luoghi e note
041100170901              d01ksc = w_ksc;
041200170911              D01invio = 'CARTACEO';
041300170901              If d57iunib = ' ' ;
041400170901                 d01desk = 'Bollettazione' ;
041500170901               else ;
041600170901                 d01desk = 'Raggruppamento';
041700170901               endif ;
041800101213         // aggancio il luogo 001 con il codice di bollettazione/unificante
041900170901            chain (K_spefls:w_ksc:K_specod) fnspe03l ;
042000101213
042100101213    1       if %found(fnspe03l) ;
042200101213               D01reci= '001-LUOGO SPEDIZIONE FATTURA' ;
042300101213               w_eti   = 'S' ;
042400101213
042500101213            //  verifico se c'è indirizzo mail
042600170901               chain (w_ksc:K_sp2cod:K_sp2tpe) fnsp201l ;
042700101213
042800101213    2          if  %found(fnsp201l) and SP2est <> *blanks ;
042900101213                   W_mail  = 'S' ;
043000101213                   D01MAIL = %trim(sp2est) + ';' ;
043100101213                   D01invio = 'VIA E-MAIL' ;
043200101213    2          else  ;
043300101213                   D01rag = sperag ;
043400101213                   D01ind = speind ;
043500101213                   D01loc = speloc ;
043600101213                   D01cap = specap ;
043700101213                   D01pro = spepro ;
043800101213                   D01naz = spenaz ;
043900101213                   D01invio = 'CARTACEO'   ;
044000101213    2          endif ;
044100101213    2       endif ;
044200101213
044300101213       // se non trovato FNSPE  cerco nella nota "84"
044400101213    1       if not %found(fnspe03l) ;
044500170901               K_ntcnk1 = '0151' + %editc(w_ksc:'X') ;
044600101213               chain (K_ntcapl:K_ntcnk1:K_ntcnk2:K_ntctnt) Tfntc01l ;
044700101213    2          If  %found(tfntc01l) and ntcrnt <> *blanks ;
044800101213                   D01reci= '84 - E-MAIL INVIO FATTURA    ' ;
044900101213                   W_mail  = 'S' ;
045000101213                   D01MAIL = %trim(NTCRNT) + ';' ;
045100101213                   D01invio = 'VIA E-MAIL' ;
045200101213    2          Endif;
045300101213    1       Endif;
045400101213
045500101213         // aggancio il luogo 001 con il codice di fatturazione se non trovato con il coidce di bollettazione
045600101213            If w_eti   <> 'S' and W_mail  <> 'S' ;
045700101213            chain (K_spefls:d57ikscf:K_specod) fnspe03l ;
045800101213
045900101213    1       if %found(fnspe03l) ;
046000101213               D01reci= '001-LUOGO SPEDIZIONE FATTURA' ;
046100101213               w_Eti   = 'S' ;
046200101213               D01ksc  = d57ikscf ;
046300101213               D01desk = 'Fatturazione' ;
046400101213
046500101213            //  verifico se c'è indirizzo mail
046600101213               chain (d57ikscf:K_sp2cod:K_sp2tpe) fnsp201l ;
046700101213
046800101213    2          if  %found(fnsp201l) and SP2est <> *blanks ;
046900101213                   W_mail  = 'S' ;
047000101213                   D01MAIL = %trim(sp2est) + ';' ;
047100101213                   D01invio = 'VIA E-MAIL' ;
047200101213    2          else  ;
047300101213                   D01rag = sperag ;
047400101213                   D01ind = speind ;
047500101213                   D01loc = speloc ;
047600101213                   D01cap = specap ;
047700101213                   D01pro = spepro ;
047800101213                   D01naz = spenaz ;
047900101213                   D01invio = 'CARTACEO'   ;
048000101213    2          endif ;
048100101213    2       endif ;
048200101213
048300101213       // se non trovato FNSPE  cerco nella nota "84"
048400101213    1       if not %found(fnspe03l) ;
048500101213               K_ntcnk1 = '0151' + %editc(d57ikscf:'X') ;
048600101213               chain (K_ntcapl:K_ntcnk1:K_ntcnk2:K_ntctnt) Tfntc01l ;
048700101213    2          If  %found(tfntc01l) and ntcrnt <> *blanks ;
048800101213                   D01reci= '84 - E-MAIL INVIO FATTURA    ' ;
048900101213                   W_mail  = 'S' ;
049000101213                   D01MAIL = %trim(ntcrnt) + ';' ;
049100101213                   D01invio = 'VIA E-MAIL' ;
049200101213                   D01desk = 'Fatturazione' ;
049300101213    2          Endif;
049400101213    1       Endif;
049500101213            Endif ;
049600101213
049700101213         // se non sono stati trovati indirizzi particolari
049800101213         // imposto come indirizzo di spedizione quello di bollettazione
049900101213         // ed invio cartaceo  (metto sempre il codice di fatturazione)
050000101213         If D01ksc  = 0 ;
050100101213                D01ksc  = d57ikscf ;
050200101213                D01desk = 'Fatturazione' ;
050300101213            D01invio = 'CARTACEO'   ;
050400101213         Endif ;
050500101213
050600101213         // Decodifico il tipo fattura
050700101213             clear dTFT;
050800101213             clear d01tftd ;
050900101213             clear TIBS02ds;
051000101213             T02mod = 'C';
051100101213             T02cod = 'TFT';
051200101213             T02ke1 = d01tft   ;
051300101213             T02sif = KNSIF;
051400101213             TNTBE_RicercaControllo  (kpjba : tibs02ds);
051500101213             IF  T02err = *blanks;
051600101213                 dTFT = t02uni ;
051700101213                 d01tftd = §tftdes;
051800101213             Endif ;
051900101213
052000101213         //  Decodifico la frequenza fattura
052100101213             clear  dsff;
052200101213             clear  d01fftd ;
052300101213             TBLKut = 1;
052400101213             TBLcod = 'FF';
052500101213             TBLkey = d01fft ;
052600101213             chain (TBLkut : TBLcod : TBLkey) TABEL;
052700101213             If %found(TABEL00F);
052800101213                 If tblflg = ' '  ;
052900101213                    dsff    = TBLuni;
053000101213                    d01fftd = §ffdes;
053100101213                 Endif ;
053200101213             Endif ;
053300101213
053400101213         //  Decodifico tipo data fattura
053500101213             clear  dsff;
053600101213             clear  d01dftd ;
053700101213             TBLkut = 1;
053800101213             TBLcod = '13';
053900101213             TBLkey = d01dft ;
054000101213             chain (TBLkut : TBLcod : TBLkey) TABEL;
054100101213             If %found(TABEL00F);
054200101213                 If tblflg = ' '  ;
054300101213                    dsff    = TBLuni;
054400101213                    d01dftd = tbluni;
054500101213                 Endif ;
054600101213             Endif ;
054700170901
054800170901         // decodifica codice di pagamento
054900170901            chain ('201':d01cdp) ancdp01l;
055000170901            IF  %found(ancdp01l);
055100170901              D01cdpd = cdpdesbrev;
055200170901            ELSE;
055300170901              D01cdp = '?????????????';
055400170901            ENDIF;
055500170901
055600170901         // ricerco la banca d'appoggio
055700170901            chain (indabi:indcab) CNABI00F;
055800170901            IF  %Found(CNABI00F) and ABIann <> '*';
055900170901              exsr sr_crebna;
056000170901              wDesbanca = wbanca + wagenzia;
056100170901            ENDIF;
056200101213
056300101213         // imposto indicatori di visualizzazione
056400101213         Etichetta = W_eti = 'S';
056500101213         Mail  = W_mail  = 'S';
056600120516
056700120516         // -?Cerco su tabella CLI (con codice fatturazione) se fattura separta Legge 136?
056800120517           L136 = *off;
056900120516           clear dCLI;
057000120516           clear D01fts;
057100120516           clear TIBS02ds;
057200120516           T02mod = 'C';
057300120516           T02cod = 'CLI';
057400120516           T02ke1 = %editc(D57ikscf:'X');
057500120516           T02sif = KNSIF;
057600120516           TNTBE_RicercaControllo  (kpjba : tibs02ds);
057700120516           IF  T02err = *blanks;
057800120516             dCLI = T02uni;
057900120516           ENDIF;
058000120516           IF  §CLIfts = 'S';
058100120516             D01fts = 'SI';
058200120517             L136 = *on;
058300120516           ELSE;
058400120516             D01fts = 'NO';
058500120517             L136 = *off;
058600120516           ENDIF;
058700120516           D01kscfts = D57ikscf;
058800120516           D01desfts = 'Fatturazione';
058900101213
059000101213         clear  D01msg;
059100101213         clear  errGenerico;
059200101213
059300101213       ENDSR;
059400170725
059500170725       // -----------------------------------------------------------------------
059600170911       //?Cerco i dati relativi allo split payment.
059700170725       // -----------------------------------------------------------------------
059800170911       BEGSR DatiSplitPayment;
059900170725
060000170728         Split = *off;
060100170725         clear tibs69ds;
060200170725         I69kin = D57ikscf;
060300170831         TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
060400170725         IF  O69err = *blanks;
060500170726           chain (INDiva) ANSPI01L;
060600170726           IF  not %found(ANSPI01L);
060700170726             chain (INDcdf) ANSPI01L;
060800170726             IF  not %found(ANSPI01L);
060900170726               leavesr;
061000170726             ENDIF;
061100170726           ENDIF;
061200170726
061300170728           Split = *on;
061400170726           clear wdataalfa;
061500170726           %subst(wdataalfa:1:2) = %subst(%editc(SPIdde:'X'):7:2);
061600170726           %subst(wdataalfa:3:2) = %subst(%editc(SPIdde:'X'):5:2);
061700170726           %subst(wdataalfa:5:4) = %subst(%editc(SPIdde:'X'):1:4);
061800170726           D01dtdspli = %dec(wdataalfa:8:0);
061900170726           clear wdataalfa;
062000170726           %subst(wdataalfa:1:2) = %subst(%editc(SPIdsc:'X'):7:2);
062100170726           %subst(wdataalfa:3:2) = %subst(%editc(SPIdsc:'X'):5:2);
062200170726           %subst(wdataalfa:5:4) = %subst(%editc(SPIdsc:'X'):1:4);
062300170726           D01dtaspli = %dec(wdataalfa:8:0);
062400170726
062500170727           D01kscspli = D57ikscf;
062600170727           D01desspli = 'Fatturazione';
062700170727
062800170727           clear wdataalfa;
062900170728           %subst(wdataalfa:1:2) = %subst(%editc(SPIdim:'X'):7:2);
063000170728           %subst(wdataalfa:3:2) = %subst(%editc(SPIdim:'X'):5:2);
063100170728           %subst(wdataalfa:5:4) = %subst(%editc(SPIdim:'X'):1:4);
063200170727           D01dtispli = %dec(wdataalfa:8:0);
063300170727
063400170725         ENDIF;
063500170725
063600170725       ENDSR;
063700101213
063800101125       // -----------------------------------------------------------------------
063900101125       // Creo la descrizione della banca di appoggio in base ai codici ABI/CAB
064000101125       // -----------------------------------------------------------------------
064100101125       Begsr Sr_Crebna ;
064200101125
064300101125if  1       If abiabi <> 99999 or abicab <> 99999   ;
064400101125               Clear wagenzia ;
064500101125               Clear wbanca ;
064600101125               wbanca = abiist ;
064700101125           //  Compongo il nome della banca con località o comune
064800101125if  2          If abiage = *Blanks ;
064900101125                  Select ;
065000101125                  When abiloc <> *Blanks  ;
065100101125                       wagenzia = abiloc  ;
065200101125                  When abicom <> *Blanks  ;
065300101125                       wagenzia = abicom  ;
065400101125                  EndSl ;
065500101125           //  Compongo il nome della banca con agenzia + località o comune
065600101125   x2          Else ;
065700101125           //  Cerco il primo byte vuoto dell'agenzia
065800101125                  wpos = %checkr(' ':abiage) ;
065900101125
066000101125                  wagenzia = %subst(abiage:1:wpos) ;
066100101125                  wpos1 = wpos + 2 ;
066200101125                  wpos2 = 16 - wpos1 ;
066300101125           //  Se wpos2 inferiore a zero lo imposto a 0
066400101125                  If wpos2 < *Zeros ;
066500101125                     Clear wpos2 ;
066600101125                  EndIf ;
066700101125           //  Se posso aggiungere anche solo 1 carattere procedo
066800101125                  If  wpos > 1 and wpos < 16 ;
066900101125           //  Cerco di riempire quello che resta con la località o il comune
067000101125                      Select;
067100101125                      When abiloc <> *Blanks ;
067200101125                           wagenzia = %trim(wagenzia) + ' ' +
067300101125                                   %subst(abiloc:1:wpos2) ;
067400101125                      When      abicom <> *Blanks ;
067500101125                           wagenzia = %trim(wagenzia) + ' ' +
067600101125                                   %subst(abicom:1:wpos2) ;
067700101125                      EndSl;
067800101125                  EndIf ;
067900101125
068000101125    2          EndIf;
068100101125    1       EndIf;
068200101125
068300101125       EndSr ;
068400101125
068500100803       //--------------------------------------------------------------
068600100803       //?Operazioni finali.                                           ?
068700100803       //--------------------------------------------------------------
068800100803       BEGSR  sr_RoutEnd;
068900100803
069000100803         // -?Uscita?
069100100803         return;
069200100803
069300100803       ENDSR;
069400100803
069500100803      /end-free
069600100804
069700100804       //--------------------------------------------------------------
069800100804       //?Messaggi di errore.                                          ?
069900100804       //--------------------------------------------------------------
070000100804
