000100110722       //==============================================================
000200110722       //?Interrog. Tab. "LAC" - Laguna: clienti per ritorno immagini  ?
000300110722       //==============================================================
000400110722
000500110722       //--------------------------------------------------------------
000600110722       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700110722       //--------------------------------------------------------------
000800110722
000900110722     /*PRM dbgview(*source)
001000110722     /*END
001100110722
001200110722       //--------------------------------------------------------------
001300110722       //?Specifiche di controllo.                                     ?
001400110722       //--------------------------------------------------------------
001500110722
001600110722     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700110722     h dftactgrp(*no)
001800110801     h bnddir('UBRTVNETA')
001900110722
002000110722       //--------------------------------------------------------------
002100110722       //?Dichiarazione file.                                          ?
002200110722       //--------------------------------------------------------------
002300110801
002400110801       // -?File tabelle di sede?
002500110801     fTNTBE01L  if   e           k disk
002600110801     f                                     extfile(wLibTNTBE)
002700110801     f                                     usropn
002800110722
002900110722       // -?Video?
003000110722     fTNTB462D  cf   e             workstn
003100110722     f                                     indds(IndDspF)
003200110722     f                                     infds(InfDspF)
003300110722
003400110722       //--------------------------------------------------------------
003500110722       //?Definizione costanti.                                        ?
003600110722       //--------------------------------------------------------------
003700110722
003800110722       // -?Codice tabella in gestione?
003900110722     d c_Tab           c                   const('LAC')
004000110722
004100110722     d c_NotFound      c                   const('? ? ? ? ? ? ? ? ? ? +
004200110722     d                                            ? ? ? ? ? ? ? ? ? ?')
004300110722
004400110722       // -?Tasti funzionali a video?
004500110722     d c_F01           c                   const(x'31')
004600110722     d c_F02           c                   const(x'32')
004700110722     d c_F03           c                   const(x'33')
004800110722     d c_F04           c                   const(x'34')
004900110722     d c_F05           c                   const(x'35')
005000110722     d c_F06           c                   const(x'36')
005100110722     d c_F07           c                   const(x'37')
005200110722     d c_F08           c                   const(x'38')
005300110722     d c_F09           c                   const(x'39')
005400110722     d c_F10           c                   const(x'3A')
005500110913     d c_F11           c                   const(x'3B')
005600110722     d c_F12           c                   const(x'3C')
005700110722     d c_F13           c                   const(x'B1')
005800110722     d c_F14           c                   const(x'B2')
005900110722     d c_F15           c                   const(x'B3')
006000110722     d c_F16           c                   const(x'B4')
006100110722     d c_F17           c                   const(x'B5')
006200110722     d c_F18           c                   const(x'B6')
006300110722     d c_F19           c                   const(x'B7')
006400110722     d c_F20           c                   const(x'B8')
006500110722     d c_F21           c                   const(x'B9')
006600110722     d c_F22           c                   const(x'BA')
006700110722     d c_F23           c                   const(x'BB')
006800110722     d c_F24           c                   const(x'BC')
006900110722     d c_Enter         c                   const(x'F1')
007000110722     d c_RollDown      c                   const(x'F4')
007100110722     d c_RollUp        c                   const(x'F5')
007200110722
007300110722       //--------------------------------------------------------------
007400110722       //?Definizione schiere.                                         ?
007500110722       //--------------------------------------------------------------
007600110722
007700110722       // -?Tipi elaborazione previsti e relativa decodifica?
007800110722     d $LACaut         s                   like(§LACaut)  ctdata
007900110913     d                                     dim( 2)  perrcd(1)
008000110722     d $LACaut_d       s                   like(V1Daut)
008100110913     d                                     dim( 2)  alt($LACaut)
008200110913
008300110913       // -?Mesi schedulati e relativa decodifica?
008400110913     d $LACsch         s              2    ctdata
008500110913     d                                     dim(12)  perrcd(1)
008600110913     d $LACsch_d       s              3
008700110913     d                                     dim(12)  alt($LACsch)
008800110722
008900110722       // -?Flag "creazione TITAS"?
009000110722     d $LACtad         s                   like(§LACtad)  ctdata
009100110722     d                                     dim( 3)  perrcd(1)
009200110722     d $LACtad_d       s                   like(V1Dtad)
009300110722     d                                     dim( 3)  alt($LACtad)
009400110722
009500110722       // -?Tipi importo forzato?
009600110722     d $LACfimp        s                   like(§LACfimp) ctdata
009700110722     d                                     dim( 2)  perrcd(1)
009800110722     d $LACfimp_d      s                   like(V1Dfimp)
009900110722     d                                     dim( 2)  alt($LACfimp)
010000110722
010100110722       // -?Messaggi di errore?
010200110722     d $Msg            s             78    dim( 2)  ctdata  perrcd(1)
010300110722
010400110722       //--------------------------------------------------------------
010500110722       //?Definizione aree dati.                                       ?
010600110722       //--------------------------------------------------------------
010700110722
010800110722       // -?Dati utente?
010900110722     d §AzUte        e ds                  extname(AZUTE00F)
011000110722     d                                     dtaara
011100110722     d §DatiUte      e ds                  extname(dDatiUte)
011200110722     d                                     dtaara
011300110722
011400110722       //--------------------------------------------------------------
011500110722       //?Definizione strutture dati.                                  ?
011600110722       //--------------------------------------------------------------
011700110722
011800110722       // -?Status ds?
011900110722     d Psds           sds
012000110722     d   SDSpgm          *proc
012100110722
012200110722       // -?InfDS?
012300110722     d InfDspF         ds
012400110722     d   dsp_aid             369    369a                                        AID byte
012500110722
012600110722       // -?Indicatori su DspF?
012700110722     d IndDspF         ds
012800110722         // -?Abilitazione tasti funzionali?
012900110722         // -?Emissione messaggio di errore?
013000110722     d  ErrMessage                     n   overlay(IndDspF : 28)
013100110722         // -?Indicatori per Attibuti di visualizzazione?
013200110722     d   VisualMESI                    n   overlay(IndDspF : 42)
013300110722     d   VisualKSC                     n   overlay(IndDspF : 43)
013400110722     d   VisualIMP                     n   overlay(IndDspF : 44)
013500121016     d   VisualMerge                   n   overlay(IndDspF : 45)
013600110722         // -?Posizionamento cursore & Segnalazione errore?
013700110722         // -?Riemissione videata?
013800110722     d  ErrGenerico                    n   overlay(IndDspF : 99)
013900110722
014000110722       // -?Parametri ricevuti?
014100110722     d KPJBA         e ds
014200110804     d TNTA61ds      e ds
014300110722
014400110722       // -?Tabella "LAC" = Clienti per ritorno immagini?
014500110722     d dLAC          e ds                  inz
014600110913     d  $Sch                  71     82    dim(12)
014700110722
014800110722       //--------------------------------------------------------------
014900110722       //?Definizione variabili globali.                               ?
015000110722       //--------------------------------------------------------------
015100110722
015200110722       // -?Flags booleani?
015300110722     d $Fine           s               n   inz
015400110722     d $InzD01         s               n   inz(*on)
015500110722
015600110722       // -?Variabili per la gestione del video?
015700110722     d $Video          s              2    inz('D1')
015800110722
015900110722       // -?Indici di schiera?
016000110722     d xx              s              3  0 inz
016100110913     d yy              s              3  0 inz
016200110913     d zz              s              3  0 inz
016300110801
016400110801       // -?Nome del sistema?
016500110801     d currSysNeta     s              8a   inz
016600110801
016700110801       // -?Nome esteso Libreria/File del file tabelle?
016800110801     d wLibTNTBE       s             21a   inz
016900110722
017000110722       //--------------------------------------------------------------
017100110722       //?Definizione prototipi procedure.                             ?
017200110722       //--------------------------------------------------------------
017300110722
017400110722       // -?Reperimento dati utente?
017500110722     d TIBS34ds      e ds                  inz
017600110722      /copy gaitrasrc/srcProtoPR,TIBS34R
017700110801
017800110801       // -?Reperimento NETA sistema AS/400 corrente?
017900110801      /copy gaitrasrc/srcProtoPr,UBRTVNETA
018000110722
018100110722       // -?Parametri per pgm controllo/decodifica cliente?
018200110722      /copy gaitrasrc/srcProtoPI,TIBS69R
018300110722       // -?Controllo/Decodifica cliente?
018400110722      /copy gaitrasrc/srcProtoPR,TIBS69R
018500110722
018600110722       //--------------------------------------------------------------
018700110722       //?Definizione key-list.                                        ?
018800110722       //--------------------------------------------------------------
018900110722
019000110801       // -?File TNTBE01L?
019100110801     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
019200110801     d                                     prefix(k_)   inz
019300110722
019400110722       //--------------------------------------------------------------
019500110722       //?M A I N - L I N E                                            ?
019600110722       //--------------------------------------------------------------
019700110722
019800110722
019900110722     c     *Entry        plist
020000110722     c                   parm                    KPJBA
020100110804     c                   parm                    TNTA61DS
020200110722
020300110722      /free
020400110722
020500110722       // -?Operazioni iniziali?
020600110722       exsr sr_RoutInz;
020700110722
020800110722       // -?Gestione videate?
020900110722       DoW  $Fine = *off;
021000110722
021100110722         select;
021200110722           // -?Gestione videata D1 (unica)?
021300110722           when $Video = 'D1';
021400110722             exsr sr_GesD01;
021500110722           // -?? ? ? ? ??
021600110722           other;
021700110722             $Fine = *on;
021800110722         endsl;
021900110722
022000110722       enddo;
022100110722
022200110722       // -?Operazioni finali?
022300110722       exsr sr_RoutEnd;
022400110722
022500110722       //--------------------------------------------------------------
022600110722       //?Operazioni iniziali.                                         ?
022700110722       //--------------------------------------------------------------
022800110722       BEGSR  sr_RoutInz;
022900110722
023000110722         *inLR = *on;
023100110801
023200110801         // -?Verifica del sistema AS/400 corrente?
023300110801         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
023400110801           $Fine = *on;
023500110801           leavesr;
023600110801         endif;
023700110801
023800110801         // -?Impostazione libreria (di SEDE) in cui gestire il file?
023900110801         //  ?tabelle (a seconda del sistema in cui si stà lavorando)?
024000110801         if  %subst( currSysNetA : 1 : 6 ) = 'SETRAS';
024100110801           wLibTNTBE = 'GAITRAGRU/TNTBE01L';
024200110801         else;
024300110801           wLibTNTBE = 'GAITRAGRPS/TNTBE01L';
024400110801         endif;
024500110801
024600110801         // -?Apertura file TNTBE01L del S.I. di sede?
024700110801         open  TNTBE01L;
024800110722
024900110722         // -?Reperimento dati job?
025000110722         exsr  sr_DatiJob;
025100110722
025200110722         // -?Impostazione nome programma a video?
025300110722         V1Tpgm = SDSpgm;
025400110722
025500110722         // -?Verifica se ricevuto parametro?
025600110804         if  TA61ksc <= *zero;
025700110804           TA61err = *on;
025800110804           TA61msg = $Msg(01);
025900110722           $Fine  = *on;
026000110722           leavesr;
026100110722         endif;
026200110722
026300110722       ENDSR;
026400110722
026500110722       //--------------------------------------------------------------
026600110722       //?Reperimento Dati del job (Utente/Operativi).                 ?
026700110722       //--------------------------------------------------------------
026800110722       BEGSR  sr_DatiJob;
026900110722
027000110722         in(e) §AzUte;
027100110722         if NOT %error;
027200110722           in(e) §DatiUte;
027300110722         endif;
027400110722         if %error or RSut = *blank;
027500110722           tibs34r ( tibs34ds );
027600110722           in §AzUte;
027700110722           in §DatiUte;
027800110722         endif;
027900110722
028000110722       ENDSR;
028100110722
028200110722       //--------------------------------------------------------------
028300110722       //?Gestione videata D01.                                        ?
028400110722       //--------------------------------------------------------------
028500110722       BEGSR  sr_GesD01;
028600110722
028700110722         // -?Inizializzazione videata?
028800110722         if  $InzD01 = *on;
028900110722           exsr  sr_InzD01;
029000110722           $InzD01 = *off;
029100110722           if  ErrGenerico;
029200110722             $Fine = *on;
029300110722             leavesr;
029400110722           endif;
029500110722         endif;
029600110722
029700110722         // -?Emissione testata e videata?
029800110722         write  TN46T01;
029900110722         exfmt  TN46D01;
030000110722
030100110722         clear  V1Dmsg;
030200110722         reset  ErrMessage;
030300110722         reset  ErrGenerico;
030400110722
030500110722         select;
030600110722           // -?F12=Ritorno?
030700110722           when  dsp_aid = c_F12;
030800110722             $Fine = *on;
030900110722         endsl;
031000110722
031100110722       ENDSR;
031200110722
031300110722       //--------------------------------------------------------------
031400110722       //?Inizializzazione videata D01.                                ?
031500110722       //--------------------------------------------------------------
031600110722       BEGSR  sr_InzD01;
031700110722
031800110722         clear  TN46D01;
031900110722
032000110722         // -?Reperimento dati tabella "LAC"?
032100110801         clear  k05tntbe01;
032200110801         k_TBEcod = 'LAC';
032300110804         k_TBEke1 = %editc(TA61ksc:'X');
032400110801         chain  %kds( k05tntbe01 )  TNTBE000;
032500110801         if  Not %found(TNTBE01L)  or  TBEatb <> *blank;
032600110722           ErrGenerico = *on;
032700110722           ErrMessage  = *on;
032800110722           V1Dmsg = $Msg(02);
032900110804           TA61err = ErrMessage;
033000110804           TA61msg = V1Dmsg;
033100110722           $Fine  = *on;
033200110722           leavesr;
033300110722         endif;
033400110722
033500110801         dLAC = TBEuni;
033600110722
033700110722         // -?Impostazione dati a video:?
033800110722         //   ·?Codice cliente?
033900110804         V1Cccm  = TA61ksc;
034000110722         //   ·?Ragione sociale cliente?
034100110722         V1Dccm  = §LACrag;
034200110722         //   ·?Tipo elaborazione?
034300110722         if  §LACaut <> *blank;
034400110722           xx = %lookup( §LACaut : $LACaut );
034500110722           if  xx > *zero;
034600110722             V1Daut  = $LACaut_d(xx);
034700110722           else;
034800110722             V1Daut  = §LACaut + '   ' + c_NotFound;
034900110722           endif;
035000110722         endif;
035100110722         //   ·?Tipo immagine?
035200110722         V1Dtpi  = §LACtpi;
035300110722         //   ·?Nome immagine?
035400110722         if  §LACfmi <> *blank;
035500110801           clear  k05tntbe01;
035600110801           k_TBEcod = 'NIM';
035700110801           k_TBEke1 = §LACfmi;
035800110801           chain  %kds( k05tntbe01 )  TNTBE000;
035900110801           if  %found(TNTBE01L)  and  TBEatb = *blank;
036000110801             V1Dfmi = TBEuni;
036100110722           else;
036200110722             V1Dfmi  = §LACfmi + '   ' + c_NotFound;
036300110722           endif;
036400110722         endif;
036500110722         //   ·?Periodo estrazione in mesi?
036600110722         V1Dmesi = §LACmesi;
036700110913         //   ·?Mesi di estrazione?
036800110913         clear V1Dmesis;
036900110913         yy = 1;
037000110913         zz = 0;
037100110913         FOR yy by 1 to %elem($Sch);
037200110913           IF  $Sch(yy) = 'X' and zz > 0;
037300110913             V1Dmesis = %trim(V1Dmesis) + ' ' + $LACsch_d(yy);
037400110913           ENDIF;
037500110913           IF  $Sch(yy) = 'X' and zz = 0;
037600110913             V1Dmesis = $LACsch_d(yy);
037700110913             zz = 1;
037800110913           ENDIF;
037900110913         ENDFOR;
038000110722         //   ·?Creazione TITAS?
038100110722         if  §LACtad <> *blank;
038200110722           xx = %lookup( §LACtad : $LACtad );
038300110722           if  xx > *zero;
038400110722             V1Dtad  = $LACtad_d(xx);
038500110722           else;
038600110722             V1Dtad  = §LACtad + '   ' + c_NotFound;
038700110722           endif;
038800110722         endif;
038900110722         //   ·?Codice cliente forzato per tassazione?
039000110722         V1Dksc  = §LACksc;
039100110722         //   ·?Codice tariffa forzato per tassazione?
039200110722         V1Dctr  = §LACctr;
039300110722         //   ·?Importo forzato?
039400110722         V1Dimp  = §LACimp;
039500110722         //   ·?Tipo importo forzato?
039600110722         if  §LACfimp <> *blank;
039700110722           xx = %lookup( §LACfimp : $LACfimp );
039800110722           if  xx > *zero;
039900110722             V1Dfimp  = $LACfimp_d(xx);
040000110722           else;
040100110722             V1Dfimp = §LACfimp + '   ' + c_NotFound;
040200110722           endif;
040300110722         endif;
040400110722
040500110722         // -?Impostazione opzioni di visualizzazione?
040600110722         VisualMESI = (§LACmesi <> *zero);
040700110722         VisualKSC  = (§LACksc <> *zero);
040800110722         VisualIMP  = (§LACimp <> *zero);
040900121016         VisualMerge = (§LACmerge <> *blanks);
041000110722
041100110722       ENDSR;
041200110722
041300110722       //--------------------------------------------------------------
041400110722       //?Operazioni finali.                                           ?
041500110722       //--------------------------------------------------------------
041600110722       BEGSR  sr_RoutEnd;
041700110722
041800110722         reset  TIBS69ds;
041900110722         tibs69r( tibs69ds :
042000110722                  ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
042100110722
042200110722         return;
042300110722
042400110722       ENDSR;
042500110722
042600110722      /end-free
042700110722
042800110722       //--------------------------------------------------------------
042900110722       //?Schiere a tempo di compilazione.                             ?
043000110722       //--------------------------------------------------------------
043100110722
043200110722** -?$LACaut / _d:?Tipi Elaborazione previsti?
043300110913AFTP
043400110913SCD
043500110913** -?$LACsch / _d:?Mesi Estrazione
04360011091301Gen
04370011091302Feb
04380011091303Mar
04390011091304Apr
04400011091305Mag
04410011091306Giu
04420011091307Lug
04430011091308Ago
04440011091309Set
04450011091310Ott
04460011091311Nov
04470011091312Dic
044800110722** -?$LACtad / _d:?Creazione TITAS?
044900110913NNO
045000110913SSI
045100110913VSI con Varia Negata
045200110722** -?$LACfimp / _d:?Tipi Importo forzato?
045300110722Ta totale x Frequenza
045400110722Iper immagine
045500110722** -?$Msg:?Messaggi di Errore?-----------------------------------------------*
045600110722Codice cliente obbligatorio                                                    1
045700110722Cliente senza ritorno immagini                                                 2
