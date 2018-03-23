000100110722       //==============================================================
000200120604       //?Interrog. Tab. "JDC" - Laguna: clienti per ritorno documenti ?
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
003000120604     fTNTBJDC2D cf   e             workstn
003100110722     f                                     indds(IndDspF)
003200110722     f                                     infds(InfDspF)
003300110722
003400110722       //--------------------------------------------------------------
003500110722       //?Definizione costanti.                                        ?
003600110722       //--------------------------------------------------------------
003700110722
003800110722       // -?Codice tabella in gestione?
003900120604     d c_Tab           c                   const('JDC')
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
007800120604     d $JDCaut         s                   like(§JDCaut)  ctdata
007900110913     d                                     dim( 2)  perrcd(1)
008000120604     d $JDCaut_d       s                   like(V1Daut)
008100120604     d                                     dim( 2)  alt($JDCaut)
008200120606
008300120606       // -?Tipi invio previsti e relativa decodifica?
008400120606     d $JDCpag         s                   like(§JDCpag)  ctdata
008500120606     d                                     dim( 2)  perrcd(1)
008600120606     d $JDCpag_d       s                   like(V1Dpag)
008700120606     d                                     dim( 2)  alt($JDCpag)
008800110722
008900110722       // -?Messaggi di errore?
009000110722     d $Msg            s             78    dim( 2)  ctdata  perrcd(1)
009100110722
009200110722       //--------------------------------------------------------------
009300110722       //?Definizione aree dati.                                       ?
009400110722       //--------------------------------------------------------------
009500110722
009600110722       // -?Dati utente?
009700110722     d §AzUte        e ds                  extname(AZUTE00F)
009800110722     d                                     dtaara
009900110722     d §DatiUte      e ds                  extname(dDatiUte)
010000110722     d                                     dtaara
010100110722
010200110722       //--------------------------------------------------------------
010300110722       //?Definizione strutture dati.                                  ?
010400110722       //--------------------------------------------------------------
010500110722
010600110722       // -?Status ds?
010700110722     d Psds           sds
010800110722     d   SDSpgm          *proc
010900110722
011000110722       // -?InfDS?
011100110722     d InfDspF         ds
011200110722     d   dsp_aid             369    369a                                        AID byte
011300110722
011400110722       // -?Indicatori su DspF?
011500110722     d IndDspF         ds
011600110722         // -?Abilitazione tasti funzionali?
011700110722         // -?Emissione messaggio di errore?
011800110722     d  ErrMessage                     n   overlay(IndDspF : 28)
011900110722         // -?Posizionamento cursore & Segnalazione errore?
012000110722         // -?Riemissione videata?
012100110722     d  ErrGenerico                    n   overlay(IndDspF : 99)
012200110722
012300110722       // -?Parametri ricevuti?
012400110722     d KPJBA         e ds
012500110804     d TNTA61ds      e ds
012600110722
012700120604       // -?Tabella "JDC" = Clienti per ritorno immagini?
012800120604     d dJDC          e ds                  inz
012900110722
013000110722       //--------------------------------------------------------------
013100110722       //?Definizione variabili globali.                               ?
013200110722       //--------------------------------------------------------------
013300110722
013400110722       // -?Flags booleani?
013500110722     d $Fine           s               n   inz
013600110722     d $InzD01         s               n   inz(*on)
013700110722
013800110722       // -?Variabili per la gestione del video?
013900110722     d $Video          s              2    inz('D1')
014000110722
014100110722       // -?Indici di schiera?
014200110722     d xx              s              3  0 inz
014300110913     d yy              s              3  0 inz
014400110913     d zz              s              3  0 inz
014500120608
014600120608       // -?Campi data       ?
014700120608     d data_eur        s               d   datfmt(*eur)
014800120608     d data_iso        s               d   datfmt(*iso)
014900110801
015000110801       // -?Nome del sistema?
015100110801     d currSysNeta     s              8a   inz
015200110801
015300110801       // -?Nome esteso Libreria/File del file tabelle?
015400110801     d wLibTNTBE       s             21a   inz
015500110722
015600110722       //--------------------------------------------------------------
015700110722       //?Definizione prototipi procedure.                             ?
015800110722       //--------------------------------------------------------------
015900110722
016000110722       // -?Reperimento dati utente?
016100110722     d TIBS34ds      e ds                  inz
016200110722      /copy gaitrasrc/srcProtoPR,TIBS34R
016300110801
016400110801       // -?Reperimento NETA sistema AS/400 corrente?
016500110801      /copy gaitrasrc/srcProtoPr,UBRTVNETA
016600110722
016700110722       // -?Parametri per pgm controllo/decodifica cliente?
016800110722      /copy gaitrasrc/srcProtoPI,TIBS69R
016900110722       // -?Controllo/Decodifica cliente?
017000110722      /copy gaitrasrc/srcProtoPR,TIBS69R
017100110722
017200110722       //--------------------------------------------------------------
017300110722       //?Definizione key-list.                                        ?
017400110722       //--------------------------------------------------------------
017500110722
017600110801       // -?File TNTBE01L?
017700110801     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
017800110801     d                                     prefix(k_)   inz
017900110722
018000110722       //--------------------------------------------------------------
018100110722       //?M A I N - L I N E                                            ?
018200110722       //--------------------------------------------------------------
018300110722
018400110722
018500110722     c     *Entry        plist
018600110722     c                   parm                    KPJBA
018700110804     c                   parm                    TNTA61DS
018800110722
018900110722      /free
019000110722
019100110722       // -?Operazioni iniziali?
019200110722       exsr sr_RoutInz;
019300110722
019400110722       // -?Gestione videate?
019500110722       DoW  $Fine = *off;
019600110722
019700110722         select;
019800110722           // -?Gestione videata D1 (unica)?
019900110722           when $Video = 'D1';
020000110722             exsr sr_GesD01;
020100110722           // -?? ? ? ? ??
020200110722           other;
020300110722             $Fine = *on;
020400110722         endsl;
020500110722
020600110722       enddo;
020700110722
020800110722       // -?Operazioni finali?
020900110722       exsr sr_RoutEnd;
021000110722
021100110722       //--------------------------------------------------------------
021200110722       //?Operazioni iniziali.                                         ?
021300110722       //--------------------------------------------------------------
021400110722       BEGSR  sr_RoutInz;
021500110722
021600110722         *inLR = *on;
021700110801
021800110801         // -?Verifica del sistema AS/400 corrente?
021900110801         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
022000110801           $Fine = *on;
022100110801           leavesr;
022200110801         endif;
022300110801
022400110801         // -?Impostazione libreria (di SEDE) in cui gestire il file?
022500110801         //  ?tabelle (a seconda del sistema in cui si stà lavorando)?
022600110801         if  %subst( currSysNetA : 1 : 6 ) = 'SETRAS';
022700110801           wLibTNTBE = 'GAITRAGRU/TNTBE01L';
022800110801         else;
022900110801           wLibTNTBE = 'GAITRAGRPS/TNTBE01L';
023000110801         endif;
023100110801
023200110801         // -?Apertura file TNTBE01L del S.I. di sede?
023300110801         open  TNTBE01L;
023400110722
023500110722         // -?Reperimento dati job?
023600110722         exsr  sr_DatiJob;
023700110722
023800110722         // -?Impostazione nome programma a video?
023900110722         V1Tpgm = SDSpgm;
024000110722
024100110722         // -?Verifica se ricevuto parametro?
024200110804         if  TA61ksc <= *zero;
024300110804           TA61err = *on;
024400110804           TA61msg = $Msg(01);
024500110722           $Fine  = *on;
024600110722           leavesr;
024700110722         endif;
024800110722
024900110722       ENDSR;
025000110722
025100110722       //--------------------------------------------------------------
025200110722       //?Reperimento Dati del job (Utente/Operativi).                 ?
025300110722       //--------------------------------------------------------------
025400110722       BEGSR  sr_DatiJob;
025500110722
025600110722         in(e) §AzUte;
025700110722         if NOT %error;
025800110722           in(e) §DatiUte;
025900110722         endif;
026000110722         if %error or RSut = *blank;
026100110722           tibs34r ( tibs34ds );
026200110722           in §AzUte;
026300110722           in §DatiUte;
026400110722         endif;
026500110722
026600110722       ENDSR;
026700110722
026800110722       //--------------------------------------------------------------
026900110722       //?Gestione videata D01.                                        ?
027000110722       //--------------------------------------------------------------
027100110722       BEGSR  sr_GesD01;
027200110722
027300110722         // -?Inizializzazione videata?
027400110722         if  $InzD01 = *on;
027500110722           exsr  sr_InzD01;
027600110722           $InzD01 = *off;
027700110722           if  ErrGenerico;
027800110722             $Fine = *on;
027900110722             leavesr;
028000110722           endif;
028100110722         endif;
028200110722
028300110722         // -?Emissione testata e videata?
028400120604         write  TBJDCT01;
028500120604         exfmt  TBJDCD01;
028600110722
028700110722         clear  V1Dmsg;
028800110722         reset  ErrMessage;
028900110722         reset  ErrGenerico;
029000110722
029100110722         select;
029200110722           // -?F12=Ritorno?
029300110722           when  dsp_aid = c_F12;
029400110722             $Fine = *on;
029500110722         endsl;
029600110722
029700110722       ENDSR;
029800110722
029900110722       //--------------------------------------------------------------
030000110722       //?Inizializzazione videata D01.                                ?
030100110722       //--------------------------------------------------------------
030200110722       BEGSR  sr_InzD01;
030300110722
030400120604         clear  TBJDCD01;
030500110722
030600120604         // -?Reperimento dati tabella "JDC"?
030700110801         clear  k05tntbe01;
030800120604         k_TBEcod = 'JDC';
030900110804         k_TBEke1 = %editc(TA61ksc:'X');
031000110801         chain  %kds( k05tntbe01 )  TNTBE000;
031100110801         if  Not %found(TNTBE01L)  or  TBEatb <> *blank;
031200110722           ErrGenerico = *on;
031300110722           ErrMessage  = *on;
031400110722           V1Dmsg = $Msg(02);
031500110804           TA61err = ErrMessage;
031600110804           TA61msg = V1Dmsg;
031700110722           $Fine  = *on;
031800110722           leavesr;
031900110722         endif;
032000110722
032100120604         dJDC = TBEuni;
032200110722
032300110722         // -?Impostazione dati a video:?
032400110722         //   ·?Codice cliente?
032500110804         V1Cccm  = TA61ksc;
032600110722         //   ·?Ragione sociale cliente?
032700120604         V1Dccm  = §JDCrag;
032800110722         //   ·?Tipo elaborazione?
032900120604         if  §JDCaut <> *blank;
033000120604           xx = %lookup( §JDCaut : $JDCaut );
033100110722           if  xx > *zero;
033200120604             V1Daut  = $JDCaut_d(xx);
033300110722           else;
033400120604             V1Daut  = §JDCaut + '   ' + c_NotFound;
033500110722           endif;
033600110722         endif;
033700110722         //   ·?Tipo immagine?
033800120604         V1Dtpi  = §JDCtpi;
033900110722         //   ·?Nome immagine?
034000120604         if  §JDCfmi <> *blank;
034100110801           clear  k05tntbe01;
034200110801           k_TBEcod = 'NIM';
034300120604           k_TBEke1 = §JDCfmi;
034400110801           chain  %kds( k05tntbe01 )  TNTBE000;
034500110801           if  %found(TNTBE01L)  and  TBEatb = *blank;
034600110801             V1Dfmi = TBEuni;
034700110722           else;
034800120604             V1Dfmi  = §JDCfmi + '   ' + c_NotFound;
034900110722           endif;
035000110722         endif;
035100120606         //   ·?Tipo invio?
035200120606         if  §JDCpag <> *blank;
035300120607           xx = %lookup( §JDCpag : $JDCpag );
035400120606           if  xx > *zero;
035500120606             V1Dpag  = $JDCpag_d(xx);
035600120606           else;
035700120606             V1Dpag  = §JDCpag + '   ' + c_NotFound;
035800120606           endif;
035900120606         endif;
036000120608
036100120608         //   ·?Date scansione?
036200120608         data_eur = %date(§JDCdti:*iso);
036300120608         V1Ddti   = %dec(data_eur);
036400120608         data_eur = %date(§JDCdtf:*iso);
036500120608         V1Ddtf   = %dec(data_eur);
036600110722
036700110722       ENDSR;
036800110722
036900110722       //--------------------------------------------------------------
037000110722       //?Operazioni finali.                                           ?
037100110722       //--------------------------------------------------------------
037200110722       BEGSR  sr_RoutEnd;
037300110722
037400110722         reset  TIBS69ds;
037500110722         tibs69r( tibs69ds :
037600110722                  ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
037700110722
037800110722         return;
037900110722
038000110722       ENDSR;
038100110722
038200110722      /end-free
038300110722
038400110722       //--------------------------------------------------------------
038500110722       //?Schiere a tempo di compilazione.                             ?
038600110722       //--------------------------------------------------------------
038700110722
038800120604** -?$JDCaut / _d:?Tipi Elaborazione previsti?
038900110913AFTP
039000120604NNO
039100120606** -?$JDCpag / _d:?Tipi Invio previsti?
039200120606CLIDocumento Completo
039300120607CL1Documento senza la prima pagina
039400110722** -?$Msg:?Messaggi di Errore?-----------------------------------------------*
039500110722Codice cliente obbligatorio                                                    1
039600120604Cliente senza ritorno documenti                                                2
