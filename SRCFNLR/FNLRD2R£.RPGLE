000100170314       //==============================================================
000200170503       //?FNLRD2R * Utility per assistenza: Modifica FPP in Distinta.  ?
000300170314       //==============================================================
000400170314
000500170314       //--------------------------------------------------------------
000600170314       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700170314       //--------------------------------------------------------------
000800170314
000900170314     /*PRM  dbgview(*source)
001000170314     /*END
001100170314
001200170314       //--------------------------------------------------------------
001300170314       //?Specifiche di controllo.                                     ?
001400170314       //--------------------------------------------------------------
001500170314
001600170314     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700170314     h dftactgrp(*no) bnddir('TRUL')
001800170314
001900170314       //--------------------------------------------------------------
002000170314       //?Dichiarazione file.                                          ?
002100170314       //--------------------------------------------------------------
002200170316
002300170316       // -?Organigramma Filiale/Aziendale?
002400170316     fAZORG01L  if   e           k disk
002500170505
002600170509       // -?Estensione anagrafina AUT - Dati x distinta?
002700170505     fFIAPD22L  if   e           k disk    usropn
002800170509       // -?Estensione anagrafiba AUT - GIRI?
002900170505     fFIAPD31L  if   e           k disk    usropn
003000170509       // -?Estensione anagrafica AUT - GIRI ORM?
003100170505     fFIAPD51L  if   e           k disk    usropn
003200170314
003300170314       // -?Distinte Consegna?
003400170505     fFIDST01L  if   e           k disk    usropn
003500170505
003600170505     fFIDST08L  Uf   e           k disk    usropn
003700170505     f                                     rename( FIDST000 : FIDST008 )
003800170320
003900170320       // -?Fogli Vari?
004000170322     fFNFVV01L  Uf   e           k disk    usropn
004100170320     f                                     rename( FNFVV000 : FNFVV001 )
004200170504
004300170509       // -?ESTENSIONE distinte Consegna - Pianificazione GIRI?
004400170504     fFIDST30F  o    e           k disk    usropn
004500170509       // -?ESTENSIONE distinte O.R.M. - Pianificazione GIRI?
004600170504     fFIDST50F  o    e           k disk    usropn
004700170314
004800170314       // -?Video Gestione?
004900170314     fFNLRD2D   cf   e             workstn
005000170314     f                                     indds( IndDspF )
005100170314     f                                     infds( InfDspF )
005200170314
005300170314       //--------------------------------------------------------------
005400170314       //?Definizione costanti.                                        ?
005500170314       //--------------------------------------------------------------
005600170316
005700170316       // -?Categoria "Consegne"?
005800170316     d c_Cat_Consegne  c                   const(4)
005900170320       // -?Categoria "Rientro"?
006000170320     d c_Cat_Rientro   c                   const(8)
006100170314
006200170314       // -?Costante per controllo "caratteri solo numerici"?
006300170314     d c_Digits        c                   const('0123456789')
006400170314
006500170314       // -?Tasti funzionali a video?
006600170314     d c_F01           c                   const(x'31')
006700170314     d c_F02           c                   const(x'32')
006800170314     d c_F03           c                   const(x'33')
006900170314     d c_F04           c                   const(x'34')
007000170314     d c_F05           c                   const(x'35')
007100170314     d c_F06           c                   const(x'36')
007200170314     d c_F07           c                   const(x'37')
007300170314     d c_F08           c                   const(x'38')
007400170314     d c_F09           c                   const(x'39')
007500170314     d c_F10           c                   const(x'3A')
007600170314     d c_F11           c                   const(x'3B')
007700170314     d c_F12           c                   const(x'3C')
007800170314     d c_F13           c                   const(x'B1')
007900170314     d c_F14           c                   const(x'B2')
008000170314     d c_F15           c                   const(x'B3')
008100170314     d c_F16           c                   const(x'B4')
008200170314     d c_F17           c                   const(x'B5')
008300170314     d c_F18           c                   const(x'B6')
008400170314     d c_F19           c                   const(x'B7')
008500170314     d c_F20           c                   const(x'B8')
008600170314     d c_F21           c                   const(x'B9')
008700170314     d c_F22           c                   const(x'BA')
008800170314     d c_F23           c                   const(x'BB')
008900170314     d c_F24           c                   const(x'BC')
009000170314     d c_Enter         c                   const(x'F1')
009100170314     d c_RollDown      c                   const(x'F4')
009200170314     d c_RollUp        c                   const(x'F5')
009300170314
009400170314       //--------------------------------------------------------------
009500170314       //?Definizione schiere.                                         ?
009600170314       //--------------------------------------------------------------
009700170320
009800170320       // -?Categorie Foglio da elaborare?
009900170320     d sk_NPG          s                   like(DSTnpg)
010000170320     d                                     dim( 2)  ctdata  perrcd(1)
010100170314
010200170314       // -?Messaggi di errore?
010300170509     d sk_Msg          s             78    dim(12)  ctdata  perrcd(1)
010400170504
010500170504       // -?Consegne Particolari in FIAPD20F?
010600170504     d p_TC1a          s               *
010700170504     d                                     inz( %addr(APD2tc1) )
010800170504     d sk_TC1a         s                   like(APD2tc1)
010900170504     d                                     dim(5)
011000170504     d                                     based(p_TC1a)
011100170504       // -?Tipi Servizio in FIAPD20F?
011200170504     d p_TS1a          s               *
011300170504     d                                     inz( %addr(APD2ts1) )
011400170504     d sk_TS1a         s                   like(APD2ts1)
011500170504     d                                     dim(2)
011600170504     d                                     based(p_TS1a)
011700170504
011800170504       // -?Consegne Particolari in FIDST00F?
011900170504     d p_TC1d          s               *
012000170504     d                                     inz( %addr(DSTtc1) )
012100170504     d sk_TC1d         s                   like(DSTtc1)
012200170504     d                                     dim(5)
012300170504     d                                     based(p_TC1d)
012400170504       // -?Tipi Servizio in FIDST00F?
012500170504     d p_TS1d          s               *
012600170504     d                                     inz( %addr(DSTts1) )
012700170504     d sk_TS1d         s                   like(DSTts1)
012800170504     d                                     dim(2)
012900170504     d                                     based(p_TS1d)
013000170314
013100170314       //--------------------------------------------------------------
013200170314       //?Definizione aree dati.                                       ?
013300170314       //--------------------------------------------------------------
013400170314
013500170314       // -?Dati utente?
013600170314     d §AzUte        e ds                  extname(AZUTE00F)
013700170314     d                                     dtaara
013800170314     d §DatiUte      e ds                  extname(dDatiUte)
013900170314     d                                     dtaara
014000170314
014100170314       //--------------------------------------------------------------
014200170314       //?Definizione strutture dati.                                  ?
014300170314       //--------------------------------------------------------------
014400170314
014500170314       // -?Status ds?
014600170314     d Status         sds
014700170314     d   SDSpgm          *proc
014800170314     d*//SDSprm          *parms
014900170314     d*//SDSdta              191    198
015000170314     d   SDSjob              244    253
015100170314     d   SDSusr              254    263
015200170314
015300170314       // -?InfDS?
015400170314     d InfDspF         ds
015500170314     d   dsp_aid             369    369a
015600170314
015700170314       // -?Indicatori su DspF?
015800170314     d IndDspF         ds                  inz
015900170316         // ·?Abilitazione tasti funzionali?
016000170316     d   F6Attivo                      n   overlay(IndDspF : 06)
016100170316     d   F12Attivo                     n   overlay(IndDspF : 12)
016200170314         // ·?Emissione messaggio di errore?
016300170314     d   $ErrMessage                   n   overlay( IndDspF : 28 )
016400170505         // ·?Indicatori per Attibuti di visualizzazione?
016500170505     d   $Test                         n   overlay( IndDspF : 40 )
016600170314         // ·?Posizionamento cursore & Segnalazione errore?
016700170316     d   $PosCurFGS                    n   overlay( IndDspF : 50 )
016800170316     d   $PosCurDFV                    n   overlay( IndDspF : 51 )
016900170316     d   $PosCurNFV                    n   overlay( IndDspF : 52 )
017000170508     d   $PosCurUPG                    n   overlay( IndDspF : 53 )
017100170314         // ·?Riemissione videata?
017200170314     d   $ErrGenerico                  n   overlay( IndDspF : 90 )
017300170314
017400170314       // -?Parametri ricevuti?
017500170314     d KPJBA         e ds
017600170504
017700170504       // -?Parametri ricevuti?
017800170504     d Og147         e ds                  inz   qualified
017900170314
018000170314       //--------------------------------------------------------------
018100170314       //?Definizione variabili globali.                               ?
018200170314       //--------------------------------------------------------------
018300170314
018400170314       // -?Flags booleani?
018500170314     d $Fine           s               n   inz
018600170316     d $InzD01         s               n   inz(*on)
018700170316     d $InzD02         s               n   inz(*on)
018800170505     d $MemoErr        s               n   inz
018900170505     d $UpdFIDST       s               n   inz
019000170314
019100170314       // -?Variabili per la gestione del video?
019200170316     d $Video          s              2    inz('D1')
019300170320
019400170320       // -?Indici di schiera?
019500170320     d xx              s              3  0 inz
019600170314
019700170316       // -?Data Distinta da elaborare (in formato aaaa/mm/gg)?
019800170316     d WWWdfv          s                   like( DSTdfv )   inz
019900170316
020000170316       // -?Numero Distinte aggiornate nei 2 archivi?
020100170316     d wNrDist_FIDST   s                   like( DSTnfv )   inz
020200170320     d wNrDist_FNFVV   s                   like( DSTnfv )   inz
020300170321     d wNrDist_FNFVV8  s                   like( DSTnfv )   inz
020400170505     d wEstens_FIDST0  s                   like( DSTnfv )   inz
020500170505     d wEstens_FIDST3  s                   like( DSTnfv )   inz
020600170505     d wEstens_FIDST5  s                   like( DSTnfv )   inz
020700170508
020800170508       // -?Numero Distinte NON aggiornabili nei 2 archivi?
020900170508     d wNrDist_NOupd   s                   like( DSTnfv )   inz
021000170508     d wNrDist_SIupd   s                   like( DSTnfv )   inz
021100170316
021200170316       // -?Campi di comodo?
021300170316     d wDate_EUR       s               d   datfmt( *eur )   inz(*loval)
021400170508     d wSaveDFV        s                   like( D01dfv )   inz
021500170508     d wSaveNFV        s                   like( D01nfv )   inz
021600170314
021700170314       //--------------------------------------------------------------
021800170314       //?Definizione prototipi procedure usate.                       ?
021900170314       //--------------------------------------------------------------
022000170314
022100170314       // -?Reperimento dati utente?
022200170314     d TIBS34ds      e ds                  inz
022300170314      /copy gaitrasrc/srcProtoPR,TIBS34R
022400170322
022500170322       // -?Caricamento £x?
022600170322     d ds_L1           ds                  inz
022700170322     d   sk_L1                        3s 0 inz  dim(30)
022800170322     d TRUL06ds      e ds                  inz  qualified
022900170322     d   D06tla      e                     inz('L')
023000170322      /copy gaitrasrc/srcProtoPR,TRUL06R
023100170504
023200170504     d FIDG09ds      e ds                  inz  qualified
023300170504     d   D09iOp0     e                     inz('001')
023400170504     d fidg09r         pr                  extpgm('FIDG09R')
023500170504     d   kpjba                             likeds(KPJBA)
023600170314
023700170314       // -?Controllo/Inversione Date?
023800170314     d WLBdat          ds                  inz
023900170314     d   G08dat                       8  0 inz
024000170314     d   G08inv                       8  0 inz
024100170314     d   G08err                       1    inz('3')
024200170314     d   G08tgi                       5  0 inz
024300170314      /copy gaitrasrc/srcProtoPR,XSRDA8
024400170314
024500170314       // -?API QCAPCMD (Process Commands)?
024600170314     d Qcmd            s           2048    inz  varying
024700170314      /copy qSysInc/qRpgleSrc,QCAPCMD
024800170314      /copy gaitrasrc/srcProtoPR,QCAPCMD
024900170314       // -?Parametri gestione errori API.?
025000170314      /copy qsysinc/qRpgleSrc,QUSEC
025100170314
025200170314       //--------------------------------------------------------------
025300170314       //?Definizione key-list.                                        ?
025400170314       //--------------------------------------------------------------
025500170316
025600170316       // -?File FIDST01L?
025700170316     d keyFIDST01    e ds                  extname( FIDST01L : *key )
025800170316     d                                     prefix(k1_)  inz
025900170314
026000170505       // -?File FIDST08L?
026100170505     d keyFIDST08    e ds                  extname( FIDST08L : *key )
026200170314     d                                     prefix(k_)   inz
026300170320
026400170320       // -?File FNFVV01L?
026500170320     d keyFNFVV01    e ds                  extname( FNFVV01L : *key )
026600170320     d                                     prefix(k1_)  inz
026700170504
026800170504       // -?File FIAPD22L?
026900170504     d keyFIAPD22    e ds                  extname( FIAPD22L : *key )
027000170504     d                                     prefix(k_)   inz
027100170504
027200170504       // -?File FIAPD31L / FIAPD51L?
027300170504     d keyFIAPD31    e ds                  extname( FIAPD31L : *key )
027400170504     d                                     prefix(k_)   inz
027500170314
027600170314       //--------------------------------------------------------------
027700170314       //?Riepilogo indicatori utilizzati.                             ?
027800170314       //--------------------------------------------------------------
027900170314
028000170314
028100170314       //--------------------------------------------------------------
028200170314       //?M A I N - L I N E                                            ?
028300170314       //--------------------------------------------------------------
028400170314
028500170314     c     *Entry        plist
028600170314     c                   parm                    KPJBA
028700170314
028800170314      /free
028900170314
029000170314       // -?Operazioni iniziali?
029100170314       exsr  sr_RoutInz;
029200170314
029300170314       // -?Gestione videata?
029400170314       DoW  Not $Fine;
029500170314
029600170314         select;
029700170314
029800170314           // -?Filtro di lancio?
029900170314           when  $Video = 'D1';
030000170314             exsr  sr_GesD01;
030100170314
030200170314           // -?Aggiornamento eseguito?
030300170314           when  $Video = 'D2';
030400170314             exsr  sr_GesD02;
030500170314
030600170314           // -?? ? ??
030700170314           other;
030800170314             $Fine = *on;
030900170314
031000170314         endsl;
031100170314
031200170314       EndDo;
031300170314
031400170314       // -?Operazioni finali?
031500170314       exsr  sr_RoutEnd;
031600170314
031700170314       //--------------------------------------------------------------
031800170314       //?Operazioni iniziali.                                         ?
031900170314       //--------------------------------------------------------------
032000170314       BEGSR  sr_RoutInz;
032100170314
032200170314         // -?Impostazione chiusura?
032300170314         *inLR = *on;
032400170505
032500170505         // -?Verifica SE richiamo per TEST (senza aggiornamenti)?
032600170505         $Test = ( %subst( KPJBU : 1 : 5 ) = 'TEST ' );
032700170314
032800170314         // -?Reperimento dati job?
032900170314         exsr  sr_DatiJob;
033000170314
033100170314         // -?Impostazione nome programma a video?
033200170314         V1Tpgm = SDSpgm;
033300170322
033400170322         // -?Reperimento Filiali dell'Area ("£1")?
033500170322         clear  sk_L1;
033600170322         clear  KPJBU;
033700170322         clear  TRUL06ds;
033800170322         TRUL06ds.D06cod = '£1';
033900170322         TRUL06ds.D06key = %editc( DUTpou : 'X' );
034000170322         KPJBU  = TRUL06ds;
034100170322         trul06r ( KPJBA );
034200170322         TRUL06ds = KPJBU;
034300170322         ds_L1 = TRUL06ds.D06lia;
034400170314
034500170314       ENDSR;
034600170314
034700170314       //--------------------------------------------------------------
034800170314       //?Reperimento Dati del job (Utente/Operativi).                 ?
034900170314       //--------------------------------------------------------------
035000170314       BEGSR  sr_DatiJob;
035100170314
035200170314         in(e) §AzUte;
035300170314         if NOT %error;
035400170314           in(e) §DatiUte;
035500170314         endif;
035600170314         if %error or RSut = *blank;
035700170314           tibs34r ( tibs34ds );
035800170314           in §AzUte;
035900170314           in §DatiUte;
036000170314         endif;
036100170314
036200170314       ENDSR;
036300170314
036400170314       //--------------------------------------------------------------
036500170314       //?Gestione videata D01.                                        ?
036600170314       //--------------------------------------------------------------
036700170314       BEGSR  sr_GesD01;
036800170314
036900170314         // -?Inizializzazione videata?
037000170314         if  $InzD01 = *on;
037100170314           exsr  sr_InzD01;
037200170314           $InzD01 = *off;
037300170314         endif;
037400170316
037500170316         // -?(Dis)Abilitazione tasti funzionali a video?
037600170316         F6Attivo  = *on;
037700170316         F12Attivo = *off;
037800170314
037900170314         // -?Emissione Testata e Piede con tasti funzionali abilitati?
038000170314         write  LRD2T01;
038100170314         write  LRD2P01;
038200170314
038300170314         // -?Emissione videata?
038400170314         exfmt  LRD2D01;
038500170314
038600170314         clear  VIDmsg;
038700170314         reset  $ErrMessage;
038800170314         reset  $ErrGenerico;
038900170314
039000170314         Select;
039100170314
039200170314           // -?Errore: sistema informativo errato?
039300170314           When  %subst( KNSIF : 1 : 3 ) <> 'FIL';
039400170314             $Fine  = *on;
039500170314
039600170314           // -?F3=Fine?
039700170314           When  dsp_aid = c_F03;
039800170314             $Fine  = *on;
039900170314
040000170314           // -?Enter / F6=Conferma?
040100170314           Other;
040200170316             exsr  sr_CtrD01;
040300170316             select;
040400170316               when  $ErrGenerico;
040500170316                 leavesr;
040600170316               when  dsp_aid = c_F06;
040700170316                 $InzD02 = *on;
040800170316                 $Video  = 'D2';
040900170316             endsl;
041000170314
041100170314         EndSl;
041200170314
041300170314       ENDSR;
041400170314
041500170314       //--------------------------------------------------------------
041600170314       //?Inizializzazione videata D01.                                ?
041700170314       //--------------------------------------------------------------
041800170314       BEGSR  sr_InzD01;
041900170314
042000170314         // -?Spegnimento degli indicatori di errore?
042100170314         %subst( IndDspF : 50 ) = *off;
042200170314
042300170314         // -?Pulizia videata?
042400170314         clear  LRD2D01;
042500170314
042600170314         // -?Impostazione iniziale dei campi a video?
042700170316         D01fgs  = DUTpou;
042800170316         wDate_Eur = %date();
042900170316         D01dfv  = %dec( wDate_Eur );
043000170314         D01nfv  = *all'9';
043100170316
043200170316         // -?Decodifiche?
043300170316         exsr  sr_CtrD01;
043400170316         %subst( IndDspF : 50 ) = *off;
043500170314
043600170314         // -?Controllo del sistema informativo?
043700170314         if  %subst( KNSIF : 1 : 3 ) <> 'FIL';
043800170314           VIDmsg = sk_Msg(01);
043900170314           $ErrMessage  = *on;
044000170314           $ErrGenerico = *on;
044100170314           leavesr;
044200170314         endif;
044300170314
044400170314         // -?Apertura archivi del s.i. di Filiale?
044500170316         if  Not %open(FIDST01L);
044600170316           open  FIDST01L;
044700170316         endif;
044800170505         if  Not %open(FIDST08L);
044900170505           open  FIDST08L;
045000170316         endif;
045100170320         if  Not %open(FNFVV01L);
045200170320           open  FNFVV01L;
045300170320         endif;
045400170314
045500170314       ENDSR;
045600170314
045700170314       //--------------------------------------------------------------
045800170314       //?Inizializzazione videata D01.                                ?
045900170314       //--------------------------------------------------------------
046000170316       BEGSR  sr_CtrD01;
046100170314
046200170314         // -?Controllo/Decodifica Filiale Gestione?
046300170505         exsr  sr_Ctrl_FIL;
046400170314         if  $ErrGenerico  and
046500170314             Not  $InzD01;
046600170314           leavesr;
046700170314         endif;
046800170314
046900170314         // -?Data Foglio?
047000170316         Select;
047100170316           When  D01dfv = *zero  and  D01nfv = *all'9';
047200170316             VIDmsg = sk_Msg(03);
047300170316             $PosCurDFV   = *on;
047400170316             $ErrMessage  = *on;
047500170316             $ErrGenerico = *on;
047600170316             if  Not  $InzD01;
047700170316               leavesr;
047800170316             endif;
047900170316           When  D01dfv > *zero;
048000170316             clear  WLBdat;
048100170316             G08dat = D01dfv;
048200170316             XSRDA8 ( WLBdat );
048300170316             if  G08err = *on;
048400170316               VIDmsg = sk_Msg(04);
048500170316               $PosCurDFV   = *on;
048600170316               $ErrMessage  = *on;
048700170316               $ErrGenerico = *on;
048800170316               if  Not  $InzD01;
048900170316                 leavesr;
049000170316               endif;
049100170316             endif;
049200170316             D01dfv = G08dat;
049300170316             WWWdfv = G08inv;
049400170316         EndSl;
049500170509
049600170509         // -?» Nessun altro controllo in fase di inizializzazione «?
049700170509         if  $InzD01;
049800170509           leavesr;
049900170509         endif;
050000170314
050100170323         // -?Controllo Numero Distinta?
050200170323         clear  wNrDist_FIDST;
050300170323         clear  wNrDist_FNFVV;
050400170505         //*·clear  wNrDist_FNFVV8;      //?(qui non serve)?
050500170505         //*·clear  wEstens_FIDST0;      //?(qui non serve)?
050600170505         //*·clear  wEstens_FIDST3;      //?(qui non serve)?
050700170505         //*·clear  wEstens_FIDST5;      //?(qui non serve)?
050800170508         //*·clear  wNrDist_NOupd;       //?(NON qui!)?
050900170508         //*·clear  wNrDist_SIupd;       //?(NON qui!)?
051000170323
051100170508         Select;
051200170508           When  D01nfv <> *all'9';
051300170508             exsr  sr_Ctrl_NrDistinta;
051400170508           When  D01nfv <> wSaveNFV  or
051500170508                 D01dfv <> wSaveDFV;
051600170508             exsr  sr_Ctrl_Distinte;
051700170508         EndSl;
051800170508         if  $ErrGenerico  and  Not  $InzD01;
051900170508           leavesr;
052000170508         endif;
052100170504
052200170504         // -?Controlli per "AGGIORNAMENTO Pianificazione Giri"?
052300170508         If  D01upg <> *blank  and  D01upg <> 'S';
052400170509           VIDmsg = sk_Msg(08);
052500170504           $PosCurUPG   = *on;
052600170504           $ErrMessage  = *on;
052700170504           $ErrGenerico = *on;
052800170504           if  Not  $InzD01;
052900170504             leavesr;
053000170504           endif;
053100170504         EndIf;
053200170504
053300170504         If  D01upg = 'S';
053400170505
053500170505           IF  D01nfv = *all'9';
053600170505
053700170505             clear  $MemoErr;
053800170505             clear  keyFIDST08;
053900170505             k_DSTfgs = D01fgs;
054000170505             k_DSTnpg = sk_NPG(1);
054100170505             k_DSTdfv = WWWdfv;
054200170505             setll  %kds( keyFIDST08 )  FIDST008;
054300170505             reade(N)  %kds( keyFIDST08 : 3 )  FIDST008;
054400170505
054500170505             DoW  Not %eof( FIDST08L );
054600170505
054700170505               If  DSTatb = *blank  and
054800170505                   DSTfcf = *blank  and
054900170508                   DSTfpp = 'P'     and
055000170508                 ( DSTnbla  = *zero  or
055100170508                   DSTrnbla = *zero );
055200170505                 exsr  sr_Ctrl_PianificazGiri;
055300170505                 if  Not $ErrGenerico;
055400170505                   clear  $MemoErr;
055500170505                   leave;
055600170505                 else;
055700170505                   $MemoErr = *on;
055800170505                   clear  VIDmsg;
055900170505                   clear  $PosCurUPG;
056000170505                   clear  $ErrMessage;
056100170505                   clear  $ErrGenerico;
056200170505                 endif;
056300170505               EndIf;
056400170505
056500170505               reade(N)  %kds( keyFIDST08 : 3 )  FIDST008;
056600170505
056700170505             EndDo;
056800170505
056900170505             if  $MemoErr;
057000170509               VIDmsg = sk_Msg(10);
057100170505               $PosCurUPG   = *on;
057200170505               $ErrMessage  = *on;
057300170505               $ErrGenerico = *on;
057400170505             endif;
057500170505
057600170505           ELSE;
057700170505
057800170505             exsr  sr_Ctrl_PianificazGiri;
057900170505
058000170505           ENDIF;
058100170505
058200170504           if  $ErrGenerico  and  Not  $InzD01;
058300170504             leavesr;
058400170504           endif;
058500170505
058600170504         EndIf;
058700170314
058800170314       ENDSR;
058900170314
059000170314       //--------------------------------------------------------------
059100170314       //?Controllo della Filiale Gestione.                            ?
059200170314       //--------------------------------------------------------------
059300170505       BEGSR  sr_Ctrl_FIL;
059400170314
059500170314         clear  D01fgsD;
059600170504         clear  Og147;
059700170314
059800170322         If  D01fgs = *zero;
059900170322
060000170323           // -?Obbligatoria?
060100170322           VIDmsg = %replace( 'obbligatoria' : sk_Msg(02) :
060200170322                       %scan( 'errata      ' : sk_Msg(02) ) );
060300170322           $PosCurFGS   = *on;
060400170322           $ErrMessage  = *on;
060500170322           $ErrGenerico = *on;
060600170322           if  Not  $InzD01;
060700170322             leavesr;
060800170322           endif;
060900170322
061000170322         Else;
061100170322
061200170322           chain  ( D01fgs )  AZORG;
061300170505           if  %found( AZORG01L );
061400170322             D01fgsD = ORGdes;
061500170504             Og147   = ORGde7;
061600170322           else;
061700170323             // -?Errata?
061800170322             VIDmsg = sk_Msg(02);
061900170322             $PosCurFGS   = *on;
062000170322             $ErrMessage  = *on;
062100170322             $ErrGenerico = *on;
062200170322             if  Not  $InzD01;
062300170322               leavesr;
062400170322             endif;
062500170322           endif;
062600170322
062700170322         EndIf;
062800170314
062900170323         // -?Non in gestione all'utente?
063000170322         if  %lookup( D01fgs : sk_L1 ) = *zero;
063100170314           VIDmsg = %trimR( sk_Msg(02) ) +
063200170314                    ': non in gestione all''utente';
063300170314           $PosCurFGS   = *on;
063400170314           $ErrMessage  = *on;
063500170314           $ErrGenerico = *on;
063600170314           if  Not  $InzD01;
063700170314             leavesr;
063800170314           endif;
063900170314         endif;
064000170314
064100170314       ENDSR;
064200170504
064300170504       //--------------------------------------------------------------
064400170504       //?Controllo del Numero Distrinta.                              ?
064500170504       //--------------------------------------------------------------
064600170504       BEGSR  sr_Ctrl_NrDistinta;
064700170504
064800170504         // -?Numero Distinta Obbligatorio?
064900170504         If  D01nfv = *zero;
065000170508           VIDmsg = sk_Msg(05);
065100170504           $PosCurNFV   = *on;
065200170504           $ErrMessage  = *on;
065300170504           $ErrGenerico = *on;
065400170504           if  Not  $InzD01;
065500170504             leavesr;
065600170504           endif;
065700170504         EndIf;
065800170504
065900170504         // -?Numero Distinta errato?
066000170504         if  D01nfv > 99999;
066100170508           VIDmsg = sk_Msg(06);
066200170504           $PosCurNFV   = *on;
066300170504           $ErrMessage  = *on;
066400170504           $ErrGenerico = *on;
066500170504           if  Not  $InzD01;
066600170504             leavesr;
066700170504           endif;
066800170504         endif;
066900170504
067000170504         // -?Controllo Distinta in FIDST00F?
067100170504         clear  keyFIDST01;
067200170504         k1_DSTnpg = sk_NPG(1);
067300170504         k1_DSTnfv = D01nfv;
067400170504         k1_DSTfgs = D01fgs;
067500170505         chain  %kds( keyFIDST01 )  FIDST000;
067600170504
067700170504         //*·if  NOT %found( FIDST01L )  or
067800170504         //*·    DSTatb <> *blank        or
067900170504         //*·    DSTfcf <> *blank        or
068000170504         //*·    DSTfpp <> 'P';
068100170504         //*·  k1_DSTnpg = sk_NPG(2);
068200170505         //*·  chain  %kds( keyFIDST01 )  FIDST000;
068300170504         //*·endif;
068400170504
068500170504         //*·if  %found( FIDST01L );
068600170504         //*·  reset  WLBdat;
068700170504         //*·  G08inv = DSTdfv;
068800170504         //*·  XSRDA8 ( WLBdat );
068900170504         //*·  if  G08err = *on;
069000170504         //*·    VIDmsg = sk_Msg(04);
069100170504         //*·    $PosCurDFV   = *on;
069200170504         //*·    $ErrMessage  = *on;
069300170504         //*·    $ErrGenerico = *on;
069400170504         //*·    if  Not  $InzD01;
069500170504         //*·      leavesr;
069600170504         //*·    endif;
069700170504         //*·  //*·D01dfv = G08dat;
069800170504         //*·  //*·WWWdfv = G08inv;
069900170504         //*·  endif;
070000170504         //*·endif;
070100170504
070200170504         Select;
070300170504           // ·?Distinta inesistente o annullata?
070400170504           When  Not %found( FIDST01L )  or
070500170504                 DSTatb <> *blank;
070600170508             VIDmsg = sk_Msg(06);
070700170504             $PosCurNFV   = *on;
070800170504             $ErrMessage  = *on;
070900170504             $ErrGenerico = *on;
071000170504             if  Not  $InzD01;
071100170504               leavesr;
071200170504             endif;
071300170504           // ·?Distinta già chiusa?
071400170504           When  DSTfcf <> *blank;
071500170508             VIDmsg = %trimR( sk_Msg(07) ) + ' già chiusa';
071600170504             $PosCurNFV   = *on;
071700170504             $ErrMessage  = *on;
071800170504             $ErrGenerico = *on;
071900170504             if  Not  $InzD01;
072000170504               leavesr;
072100170504             endif;
072200170504           // ·?Distinta già del Mattino?
072300170504           When  DSTfpp <> 'P';
072400170508             VIDmsg = %trimR( sk_Msg(07) ) + ' già del Mattino';
072500170504             $PosCurNFV   = *on;
072600170504             $ErrMessage  = *on;
072700170504             $ErrGenerico = *on;
072800170504             if  Not  $InzD01;
072900170504               leavesr;
073000170504             endif;
073100170509           // ·?Consegne o ORM già assegnati?
073200170509           When  DSTnbla  > *zero  or
073300170509                 DSTrnbla > *zero;
073400170509             VIDmsg = %trimR( sk_Msg(07) ) + ' consegne o O.R.M. +
073500170509                                             risultano già assegnati';
073600170509             $PosCurNFV   = *on;
073700170509             $ErrMessage  = *on;
073800170509             $ErrGenerico = *on;
073900170509             if  Not  $InzD01;
074000170509               leavesr;
074100170509             endif;
074200170504           // ·?Distinta di data diversa?
074300170504           When  DSTdfv <> WWWdfv;
074400170504             reset  WLBdat;
074500170504             G08inv = DSTdfv;
074600170504             XSRDA8 ( WLBdat );
074700170508             VIDmsg = %trimR( sk_Msg(07) ) + ' trovata con data ' +
074800170504                      %editw( G08dat : '  /  /    ' );
074900170504             $PosCurNFV   = *on;
075000170504             $ErrMessage  = *on;
075100170504             $ErrGenerico = *on;
075200170504             if  Not  $InzD01;
075300170504               leavesr;
075400170504             endif;
075500170504           // ·?Distinta da modificare?
075600170504           Other;
075700170504             wNrDist_FIDST += 1;
075800170504         EndSl;
075900170504
076000170504         // -?Controllo Distinta in FNFVV00F?
076100170504         clear  keyFNFVV01;
076200170504         k1_FVVnpg = sk_NPG(1);
076300170504         k1_FVVnfv = D01nfv;
076400170504         k1_FVVfgs = D01fgs;
076500170504         chain(N)  %kds( keyFNFVV01 )  FNFVV001;
076600170504
076700170504         //*·if  NOT %found( FNFVV01L )  or
076800170504         //*·    FVVatb <> *blank        or
076900170504         //*·    FVVfcf <> *blank        or
077000170504         //*·    FVVfpp <> 'P';
077100170504         //*·  k1_FVVnpg = sk_NPG(2);
077200170505         //*·  chain(N)  %kds( keyFNFVV01 )  FNFVV001;
077300170504         //*·endif;
077400170504
077500170504         //*·if  %found( FNFVV01L )  and
077600170504         //*·    D01dfv = *zero;
077700170504         //*·  reset  WLBdat;
077800170504         //*·  G08inv = FVVdfv;
077900170504         //*·  XSRDA8 ( WLBdat );
078000170504         //*·  if  G08err = *on;
078100170504         //*·    VIDmsg = sk_Msg(04);
078200170504         //*·    $PosCurDFV   = *on;
078300170504         //*·    $ErrMessage  = *on;
078400170504         //*·    $ErrGenerico = *on;
078500170504         //*·    if  Not  $InzD01;
078600170504         //*·      leavesr;
078700170504         //*·    endif;
078800170504         //*·  endif;
078900170504         //*·  //*·D01dfv = G08dat;
079000170504         //*·  //*·WWWdfv = G08inv;
079100170504         //*·endif;
079200170504
079300170504         Select;
079400170504           // ·?Distinta inesistente o annullata?
079500170504           When  Not %found( FNFVV01L )  or
079600170504                 FVVatb <> *blank;
079700170508             VIDmsg = sk_Msg(06);
079800170504             $PosCurNFV   = *on;
079900170504             $ErrMessage  = *on;
080000170504             $ErrGenerico = *on;
080100170504             if  Not  $InzD01;
080200170504               leavesr;
080300170504             endif;
080400170504           // ·?Distinta già chiusa?
080500170504           When  FVVfcf <> *blank;
080600170508             VIDmsg = %trimR( sk_Msg(07) ) + ' già chiusa';
080700170504             $PosCurNFV   = *on;
080800170504             $ErrMessage  = *on;
080900170504             $ErrGenerico = *on;
081000170504             if  Not  $InzD01;
081100170504               leavesr;
081200170504             endif;
081300170504           // ·?Distinta già del Mattino?
081400170504           When  FVVfpp <> 'P';
081500170508             VIDmsg = %trimR( sk_Msg(07) ) + ' già del Mattino';
081600170504             $PosCurNFV   = *on;
081700170504             $ErrMessage  = *on;
081800170504             $ErrGenerico = *on;
081900170504             if  Not  $InzD01;
082000170504               leavesr;
082100170504             endif;
082200170504           // ·?Distinta di data diversa?
082300170504           When  FVVdfv <> WWWdfv;
082400170504             reset  WLBdat;
082500170504             G08inv = FVVdfv;
082600170504             XSRDA8 ( WLBdat );
082700170508             VIDmsg = %trimR( sk_Msg(07) ) + ' trovata con data ' +
082800170504                      %editw( G08dat : '  /  /    ' );
082900170504             $PosCurNFV   = *on;
083000170504             $ErrMessage  = *on;
083100170504             $ErrGenerico = *on;
083200170504             if  Not  $InzD01;
083300170504               leavesr;
083400170504             endif;
083500170504           // ·?Distinta da modificare?
083600170504           Other;
083700170504             wNrDist_FNFVV += 1;
083800170504         EndSl;
083900170504
084000170504         // -?Nessuna Distinta trovata (da modificare)?
084100170504         if  wNrDist_FIDST = *zero  and
084200170504             wNrDist_FNFVV = *zero;
084300170508           VIDmsg = sk_Msg(06);
084400170504           $PosCurNFV   = *on;
084500170504           $ErrMessage  = *on;
084600170504           $ErrGenerico = *on;
084700170504           if  Not  $InzD01;
084800170504             leavesr;
084900170504           endif;
085000170504         endif;
085100170504
085200170504       ENDSR;
085300170508
085400170508       //--------------------------------------------------------------
085500170508       //?Conteggio delle Distinte NON aggiornabili.                   ?
085600170508       //--------------------------------------------------------------
085700170508       BEGSR  sr_Ctrl_Distinte;
085800170508
085900170508         clear  wNrDist_NOupd;
086000170508         clear  wNrDist_SIupd;
086100170508         xx = 1;
086200170508         clear  keyFIDST08;
086300170508         k_DSTfgs = D01fgs;
086400170508         k_DSTnpg = sk_NPG(xx);
086500170508         k_DSTdfv = WWWdfv;
086600170508         setll  %kds( keyFIDST08 )  FIDST008;
086700170508         reade(N)  %kds( keyFIDST08 : 3 )  FIDST008;
086800170508
086900170508         DoW  Not %eof( FIDST08L );
087000170508           If  DSTatb = *blank  and
087100170508               DSTfcf = *blank  and
087200170508               DSTfpp = 'P';
087300170508             if  DSTnbla  <> *zero  or
087400170508                 DSTrnbla <> *zero;
087500170508               wNrDist_NOupd += 1;
087600170508             else;
087700170508               wNrDist_SIupd += 1;
087800170508             endif;
087900170508           EndIf;
088000170508           reade  %kds( keyFIDST08 : 3 )  FIDST008;
088100170508         EndDo;
088200170508
088300170509         // -?Trovate Distinte NON modificabili?
088400170508         If  wNrDist_NOupd > *zero;
088500170508
088600170508           if  wNrDist_SIupd = *zero;
088700170508             VIDmsg = %replace( ' SOLO ' :
088800170509                      sk_Msg(11) : %scan( '&NOupd' : sk_Msg(11) ) );
088900170508           else;
089000170509             wSaveNFV = D01nfv;
089100170509             wSaveDFV = D01dfv;
089200170508             VIDmsg = %replace( %editc( wNrDist_NOupd : '3' ) :
089300170509                      sk_Msg(11) : %scan( '&NOupd' : sk_Msg(11) ) );
089400170508             VIDmsg = %trimR( VIDmsg ) + ' - "Invio" x proseguire';
089500170508           endif;
089600170508           $PosCurNFV   = *on;
089700170508           $ErrMessage  = *on;
089800170508           $ErrGenerico = *on;
089900170508           leavesr;
090000170508
090100170508         EndIf;
090200170509
090300170509         // -?NON trovata nessuna Distinta da modificate?
090400170509         If  wNrDist_SIupd = *zero;
090500170509
090600170509           VIDmsg = sk_Msg(12);
090700170509           $PosCurNFV   = *on;
090800170509           $ErrMessage  = *on;
090900170509           $ErrGenerico = *on;
091000170509           leavesr;
091100170509
091200170509         EndIf;
091300170508
091400170508       ENDSR;
091500170314
091600170314       //--------------------------------------------------------------
091700170314       //?Gestione videata D02.                                        ?
091800170314       //--------------------------------------------------------------
091900170314       BEGSR  sr_GesD02;
092000170314
092100170314         // -?Inizializzazione videata?
092200170314         if  $InzD02 = *on;
092300170314           exsr  sr_InzD02;
092400170314           $InzD02 = *off;
092500170314         endif;
092600170316
092700170316         // -?(Dis)Abilitazione tasti funzionali a video?
092800170316         F6Attivo  = *off;
092900170316         F12Attivo = *on;
093000170314
093100170314         // -?Emissione Testata e Piede con tasti funzionali abilitati?
093200170316         write  LRD2T01;
093300170314         write  LRD2D01;
093400170316         write  LRD2P01;
093500170314
093600170314         // -?Emissione videata?
093700170314         write  Protect;
093800170314         exfmt  LRD2D02;
093900170314
094000170314         clear  VIDmsg;
094100170314         reset  $ErrMessage;
094200170314         reset  $ErrGenerico;
094300170314
094400170314         Select;
094500170314
094600170314           // -?F3=Fine?
094700170314           When  dsp_aid = c_F03;
094800170314             $Fine  = *on;
094900170314
095000170314           // -?F12=Ritorno?
095100170314           When  dsp_aid = c_F12;
095200170314             $Video = 'D1';
095300170316             //*·reset  $InzD01;
095400170314             reset  $InzD02;
095500170508             clear  wSaveNFV;
095600170508             clear  wSaveDFV;
095700170508             clear  wNrDist_NOupd;
095800170508             clear  wNrDist_SIupd;
095900170314
096000170314         EndSl;
096100170314
096200170314       ENDSR;
096300170314
096400170314       //--------------------------------------------------------------
096500170314       //?Inizializzazione videata D02.                                ?
096600170314       //--------------------------------------------------------------
096700170314       BEGSR  sr_InzD02;
096800170314
096900170314         // -?Spegnimento degli indicatori di errore?
097000170314         %subst( IndDspF : 50 ) = *off;
097100170314
097200170314         // -?Pulizia videata?
097300170314         clear  LRD2D02;
097400170314
097500170314         // -?Modifica Flag Prestazione Padroncino?
097600170320         //  ?(P=Pomeriggio => M=Mattina)?
097700170314         exsr  sr_Upd_FPP;
097800170314
097900170314         Select;
098000170314           // -?Nessuna Distinta aggiornata?
098100170321           When  wNrDist_FIDST  = *zero  and
098200170321                 wNrDist_FNFVV  = *zero  and
098300170321                 wNrDist_FNFVV8 = *zero;
098400170508             if  NOT $Test;
098500170316               D02txt1 = 'Nessuna distinta è stata aggiornata.';
098600170316             else;
098700170316               D02txt1 = 'Nessuna distinta risulta aggiornabile.';
098800170316             endif;
098900170314           // -?Aggiornata Distinta inserita?
099000170314           When  D01nfv        > *zero    and
099100170314                 D01nfv       <> *all'9'  and
099200170320                 wNrDist_FIDST > *zero    and
099300170320                 wNrDist_FIDST = wNrDist_FNFVV;
099400170508             if  NOT $test;
099500170316               D02txt1 = 'La distinta è stata aggiornata.';
099600170316             else;
099700170316               D02txt1 = 'La distinta è aggiornabile.';
099800170316             endif;
099900170316           When  D01nfv          > *zero    and
100000170316                 D01nfv         <> *all'9'  and
100100170320                 ( wNrDist_FIDST > *zero    or
100200170320                   wNrDist_FNFVV > *zero );
100300170508             if  NOT $Test;
100400170316               D02txt1 = 'La distinta è stata aggiornata';
100500170316             else;
100600170316               D02txt1 = 'La distinta è aggiornabile';
100700170316             endif;
100800170320             if  wNrDist_FIDST > *zero;
100900170320               D02txt1 = %trimR( D02txt1 ) + ' in FIDST00F.';
101000170316             else;
101100170320               D02txt1 = %trimR( D02txt1 ) + ' in FNFVV00F.';
101200170316             endif;
101300170314           // -?Aggiornate tutte le Distinte del giorno inserito?
101400170314           When  D01nfv        = *all'9'  and
101500170320                 wNrDist_FIDST = wNrDist_FNFVV;
101600170508             if  NOT $Test;
101700170316               D02txt1 = 'Aggiornate ' +
101800170320                         %trim( %editc( wNrDist_FIDST : '1' ) ) +
101900170316                         ' distinte.';
102000170316             else;
102100170316               D02txt1 = 'Reperite ' +
102200170320                         %trim( %editc( wNrDist_FIDST : '1' ) ) +
102300170316                         ' distinte aggiornabili.';
102400170316             endif;
102500170314           When  D01nfv         = *all'9'  and
102600170320                 wNrDist_FIDST <> wNrDist_FNFVV;
102700170508             if  NOT $Test;
102800170316               D02txt1 = 'Aggiornate ' +
102900170320                         %trim( %editc( wNrDist_FIDST : '1' ) ) +
103000170320                         ' distinte in FIDST00F +
103100170320                          (Distinte Uscita AUT Consegne/Ritiri).';
103200170316               D02txt2 = 'Aggiornate ' +
103300170320                         %trim( %editc( wNrDist_FNFVV : '1' ) ) +
103400170320                         ' distinte in FNFVV00F +
103500170321                          (Fogli Vari) - Cat. 4-Consegne.';
103600170321               D02txt3 = 'Aggiornate ' +
103700170321                         %trim( %editc( wNrDist_FNFVV8 : '1' ) ) +
103800170321                         ' distinte in FNFVV00F +
103900170321                          (Fogli Vari) - Cat. 8-Rientro.';
104000170316             else;
104100170320               D02txt1 = 'Reperite ' +
104200170316                         %trim( %editc( wNrDist_FIDST : '1' ) ) +
104300170316                         ' distinte aggiornabili in FIDST00F +
104400170316                          (Distinte Uscita AUT Consegne/Ritiri).';
104500170320               D02txt2 = 'Reperite ' +
104600170320                         %trim( %editc( wNrDist_FNFVV : '1' ) ) +
104700170320                         ' distinte aggiornabili in FNFVV00F +
104800170321                          (Fogli Vari) - Cat. 4-Consegne.';
104900170321               D02txt3 = 'Reperite ' +
105000170321                         %trim( %editc( wNrDist_FNFVV8 : '1' ) ) +
105100170321                         ' distinte aggiornabili in FNFVV00F +
105200170321                          (Fogli Vari) - Cat. 8-Rientro.';
105300170316             endif;
105400170314         EndSl;
105500170505
105600170505         If  ( wEstens_FIDST0 +
105700170505               wEstens_FIDST3 +
105800170505               wEstens_FIDST5 ) > *zero;
105900170505
106000170505           if  D02txt3 = *blank;
106100170508             if  NOT $Test;
106200170508               D02txt3 = 'Giri Pianificati: inseriti';
106300170508             else;
106400170508               D02txt3 = 'Giri Pianificati: inseribili';
106500170508             endif;
106600170508             D02txt3 = %trimR( D02txt3 ) + ' ' +
106700170508                       %trim( %editc( wEstens_FIDST3 : '3' ) ) +
106800170505                       ' per Consegne e ' +
106900170505                       %trim( %editc( wEstens_FIDST5 : '3' ) ) +
107000170505                       ' per O.R.M.';
107100170505           else;
107200170508             if  NOT $Test;
107300170508               D02txt4 = 'Giri Pianificati: inseriti';
107400170508             else;
107500170508               D02txt4 = 'Giri Pianificati: inseribili';
107600170508             endif;
107700170508             D02txt4 = %trimR( D02txt3 ) + ' ' +
107800170505                       %trim( %editc( wEstens_FIDST3 : '3' ) ) +
107900170505                       ' per Consegne e ' +
108000170505                       %trim( %editc( wEstens_FIDST5 : '3' ) ) +
108100170505                       ' per O.R.M.';
108200170505           endif;
108300170505
108400170505         EndIf;
108500170314
108600170314       ENDSR;
108700170314
108800170314       //--------------------------------------------------------------
108900170322       //?Modifica Flag Prestazione Padroncino                         ?
109000170322       //?da P=Pomeriggio in M=Mattina.                                ?
109100170314       //--------------------------------------------------------------
109200170314       BEGSR  sr_Upd_FPP;
109300170314
109400170314         clear  wNrDist_FIDST;
109500170320         clear  wNrDist_FNFVV;
109600170321         clear  wNrDist_FNFVV8;
109700170505         clear  wEstens_FIDST0;
109800170505         clear  wEstens_FIDST3;
109900170505         clear  wEstens_FIDST5;
110000170322
110100170322         if  D01nfv = *all'9';
110200170322           // -?Aggiornamento flag FPP in tutte le distinte della data?
110300170322           //  ?inserita?
110400170322           exsr  sr_Upd_FPP_Data;
110500170322         else;
110600170322           // -?Aggiornamento flag FPP nella singola distinta inserita?
110700170322           exsr  sr_Upd_FPP_Num;
110800170322         endif;
110900170322
111000170322       ENDSR;
111100170322
111200170322       //--------------------------------------------------------------
111300170322       //?Mod. Flag Prestazione Padroncino da P=Pomeriggio in M=Mattina?
111400170322       //?in TUTTE le distinte della data inserita.                    ?
111500170322       //--------------------------------------------------------------
111600170322       BEGSR  sr_Upd_FPP_Data;
111700170316
111800170316         // -?Aggiornamento flag FPP in FIDST00F (Distinte AUT)?
111900170321         //  ?dove le distinte sono solo di Cat. 4-Consegne?
112000170321         xx = 1;
112100170505         clear  keyFIDST08;
112200170505         k_DSTfgs = D01fgs;
112300170321         k_DSTnpg = sk_NPG(xx);
112400170321         k_DSTdfv = WWWdfv;
112500170505         setll  %kds( keyFIDST08 )  FIDST008;
112600170505         reade  %kds( keyFIDST08 : 3 )  FIDST008;
112700170321
112800170505         DoW  Not %eof( FIDST08L );
112900170505           exsr  sr_Upd_FIDST;
113000170505           reade  %kds( keyFIDST08 : 3 )  FIDST008;
113100170321         EndDo;
113200170316
113300170505         unlock  FIDST08L;
113400170314
113500170314       ENDSR;
113600170322
113700170322       //--------------------------------------------------------------
113800170322       //?Mod. Flag Prestazione Padroncino da P=Pomeriggio in M=Mattina?
113900170322       //?nella singola Distinta del numero inserito.                  ?
114000170322       //--------------------------------------------------------------
114100170322       BEGSR  sr_Upd_FPP_Num;
114200170322
114300170322         // -?Aggiornamento flag FPP in FIDST00F (Distinte AUT)?
114400170322         //  ?nella singola distinta di Cat. 4-Consegne?
114500170322         xx = 1;
114600170505         clear  keyFIDST08;
114700170505         k_DSTfgs = D01fgs;
114800170505         k_DSTnpg = sk_NPG(xx);
114900170505         k_DSTdfv = WWWdfv;
115000170505         k_DSTnfv = D01nfv;
115100170505         chain  %kds( keyFIDST08 )  FIDST008;
115200170322
115300170505         If  %found( FIDST08L );
115400170505           exsr  sr_Upd_FIDST;
115500170505         EndIf;
115600170505
115700170505         unlock  FIDST08L;
115800170322
115900170322       ENDSR;
116000170505
116100170505       //--------------------------------------------------------------
116200170505       //?Aggiornamento FIDST00F nella singola Distinta in elaboraz.   ?
116300170505       //--------------------------------------------------------------
116400170505       BEGSR  sr_Upd_FIDST;
116500170505
116600170509         If  DSTatb   = *blank  and
116700170509             DSTfcf   = *blank  and
116800170509             DSTfpp   = 'P'     and
116900170509             DSTnbla  = *zero   and
117000170509             DSTrnbla = *zero;
117100170505
117200170508           DSTfpp = 'M';
117300170508
117400170508           if  D01upg = 'S';
117500170508
117600170508             exsr  sr_Upd_PianificazGiri;
117700170508
117800170508           else;
117900170508
118000170508             if  NOT  $Test;        //? Test ? ?
118100170508               //_______________
118200170508               update  FIDST008;
118300170508               //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
118400170508             else;
118500170508               unlock  FIDST08L;
118600170508             endif;                 //? Test ? ?
118700170505
118800170508           endif;
118900170509
119000170509           wNrDist_FIDST += 1;
119100170509
119200170509           exsr  sr_Upd_FNFVV;
119300170505
119400170505         Else;
119500170505
119600170505           unlock  FIDST08L;
119700170505
119800170505         EndIf;
119900170505
120000170505       ENDSR;
120100170509
120200170509       //--------------------------------------------------------------
120300170509       //?Aggiornamento FNFVV00F nella singola Distinta in elaboraz.   ?
120400170509       //--------------------------------------------------------------
120500170509       BEGSR  sr_Upd_FNFVV;
120600170509
120700170509         // -?Aggiornamento flag FPP in FNFVV00F (Fogli Vari)?
120800170509         //  ?nel singolo foglio con Cat. 4-Consegne e 8-Rientro?
120900170509         For  xx = 1  To %elem( sk_NPG );
121000170509
121100170509           clear  keyFNFVV01;
121200170509           k1_FVVnpg = sk_NPG(xx);
121300170509           k1_FVVnfv = DSTnfv;
121400170509           k1_FVVfgs = DSTfgs;
121500170509           chain  %kds( keyFNFVV01 )  FNFVV001;
121600170509
121700170509           If  %found( FNFVV01L )  and
121800170509               FVVatb = *blank     and
121900170509               FVVfcf = *blank     and
122000170509               FVVfpp = 'P';
122100170509
122200170509             FVVfpp = 'M';
122300170509
122400170509             if  NOT  $Test;        //? Test ? ?
122500170509               //_______________
122600170509               update  FNFVV001;
122700170509               //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
122800170509             else;
122900170509               unlock  FNFVV01L;
123000170509             endif;                 //? Test ? ?
123100170509
123200170509             if  xx = 1;
123300170509               wNrDist_FNFVV  += 1;
123400170509             else;
123500170509               wNrDist_FNFVV8 += 1;
123600170509             endif;
123700170509
123800170509           Else;
123900170509
124000170509             if  %found( FNFVV01L );
124100170509               unlock  FNFVV01L;
124200170509             endif;
124300170509
124400170509           EndIf;
124500170509
124600170509         EndFor;
124700170509
124800170509       ENDSR;
124900170504
125000170504       //--------------------------------------------------------------
125100170504       //?Controlli per "AGGIORNAMENTO Pianificazione Giri".           ?
125200170504       //--------------------------------------------------------------
125300170504       BEGSR  sr_Ctrl_PianificazGiri;
125400170504
125500170504         // -?Presenza di giri pianificati nella distinta?
125600170504         if  Not  %open( FIAPD22L );
125700170504           open  FIAPD22L;
125800170504         endif;
125900170504         if  Not  %open( FIAPD31L );
126000170504           open  FIAPD31L;
126100170504         endif;
126200170504         if  Not  %open( FIAPD51L );
126300170504           open  FIAPD51L;
126400170504         endif;
126500170504
126600170504         // -?Lettura Estensione anagr. AUT - Dati x distinta?
126700170504         clear  KeyFIAPD22;
126800170504         k_APD2tipA = 'A';
126900170504         k_APD2pdr  = DSTpdr;
127000170504         k_APD2fpp  = DSTfpp;
127100170504         setll  %kds( KeyFIAPD22 )  FIAPD200;
127200170504         reade  %kds( KeyFIAPD22 : 3 )  FIAPD200;
127300170504
127400170504         DoW  Not %eof( FIAPD22L );
127500170504
127600170504           If  APD2atb   =  *blank  and
127700170504               APD2dde   <= wwwDFV  and
127800170504               APD2dscad >= wwwDFV;
127900170504
128000170504             clear  KeyFIAPD31;
128100170504             k_APD3nrAs = APD2nrAs;
128200170504
128300170504             // -?Controllo su Estensione anagr. AUT - GIRI?
128400170504             setll  %kds( KeyFIAPD31 )  FIAPD300;
128500170504             reade  %kds( KeyFIAPD31 : 1 )  FIAPD300;
128600170504             DoW  Not %eof( FIAPD31L );
128700170504               If  APD3atb   =  *blank  and
128800170504                   APD3dde   <= wwwDFV  and
128900170504                   APD3dscad >= wwwDFV;
129000170509                 VIDmsg = sk_Msg(09);
129100170504                 $PosCurUPG   = *on;
129200170504                 $ErrMessage  = *on;
129300170504                 $ErrGenerico = *on;
129400170504                 if  Not  $InzD01;
129500170504                   leavesr;
129600170504                 endif;
129700170504                 reade  %kds( KeyFIAPD31 : 1 )  FIAPD300;
129800170504               EndIf;
129900170504             EndDo;
130000170504
130100170504             // -?Controllo su Estensione anagr. AUT - GIRI ORM?
130200170504             setll  %kds( KeyFIAPD31 )  FIAPD500;
130300170504             reade  %kds( KeyFIAPD31 : 1 )  FIAPD500;
130400170504             DoW  Not %eof( FIAPD51L );
130500170504               If  APD5atb   =  *blank  and
130600170504                   APD5dde   <= wwwDFV  and
130700170504                   APD5dscad >= wwwDFV;
130800170509                 VIDmsg = sk_Msg(09);
130900170504                 $PosCurUPG   = *on;
131000170504                 $ErrMessage  = *on;
131100170504                 $ErrGenerico = *on;
131200170504                 if  Not  $InzD01;
131300170504                   leavesr;
131400170504                 endif;
131500170504                 reade  %kds( KeyFIAPD31 : 1 )  FIAPD500;
131600170504               EndIf;
131700170504             EndDo;
131800170504
131900170504           EndIf;
132000170504
132100170504           reade  %kds( KeyFIAPD22 : 3 )  FIAPD200;
132200170504
132300170504         EndDo;
132400170504
132500170504       ENDSR;
132600170504
132700170504       //--------------------------------------------------------------
132800170504       //?Aggiornamento Pianificazione Giri.                           ?
132900170504       //--------------------------------------------------------------
133000170504       BEGSR  sr_Upd_PianificazGiri;
133100170504
133200170504         if  Not  %open( FIAPD22L );
133300170504           open  FIAPD22L;
133400170504         endif;
133500170504         if  Not  %open( FIAPD31L );
133600170504           open  FIAPD31L;
133700170504         endif;
133800170504         if  Not  %open( FIAPD51L );
133900170504           open  FIAPD51L;
134000170504         endif;
134100170504         if  Not  %open( FIDST30F );
134200170504           open  FIDST30F;
134300170504         endif;
134400170504         if  Not  %open( FIDST50F );
134500170504           open  FIDST50F;
134600170504         endif;
134700170504
134800170504         // -?Lettura Estensione anagr. AUT - Dati x distinta?
134900170505         clear  $UpdFIDST;
135000170504         clear  KeyFIAPD22;
135100170504         k_APD2tipA = 'A';
135200170504         k_APD2pdr  = DSTpdr;
135300170508         //*·k_APD2fpp  = DSTfpp;
135400170508         k_APD2fpp  = 'P';
135500170504         setll  %kds( KeyFIAPD22 )  FIAPD200;
135600170504         reade  %kds( KeyFIAPD22 : 3 )  FIAPD200;
135700170504
135800170504         DoW  Not %eof( FIAPD22L );
135900170504
136000170504           if  APD2atb   =  *blank  and
136100170504               APD2dde   <= wwwDFV  and
136200170504               APD2dscad >= wwwDFV;
136300170504
136400170505             if  Not $UpdFIDST;
136500170505               exsr  sr_Upd_FIDST00;
136600170505               $UpdFIDST = *on;
136700170505             endif;
136800170504             exsr  sr_Wrt_FIDST30;
136900170504             exsr  sr_Wrt_FIDST50;
137000170504
137100170504           endif;
137200170504
137300170504           reade  %kds( KeyFIAPD22 : 3 )  FIAPD200;
137400170504
137500170504         EndDo;
137600170508
137700170508         // -?Aggiornamento flag in FIDST00F SE NON trovate estensioni?
137800170508         //  ?anagr. AUT - Dati x distinta?
137900170508         If  Not $UpdFIDST;
138000170508           if  NOT  $Test;       //? Test ? ?
138100170508             //_______________
138200170508             update  FIDST008;
138300170508             //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
138400170508           else;
138500170508             unlock  FIDST08L;
138600170508           endif;                //? Test ? ?
138700170508           $UpdFIDST = *on;
138800170508         EndIf;
138900170504
139000170504       ENDSR;
139100170504
139200170504       //--------------------------------------------------------------
139300170504       //?Aggiornamento FIDST00F (Distinte Uscita AUT Consegne/Ritiri).?
139400170504       //--------------------------------------------------------------
139500170504       BEGSR  sr_Upd_FIDST00;
139600170504
139700170504         // -?Aggiornamento Consegne in FIDST00F?
139800170504         sk_TC1d = sk_TC1a;
139900170504         sk_TS1d = sk_TS1a;
140000170504         if  APD2ftc = *blank;
140100170504           DSTftc = 'E';
140200170504         else;
140300170504           DSTftc = APD2ftc;
140400170504         endif;
140500170504         if  APD2fts = *blank;
140600170504           DSTfts = 'E';
140700170504         else;
140800170504           DSTfts = APD2fts;
140900170504         endif;
141000170504         DSTvlu = APD2vlu;
141100170504         DSTpkl = APD2pkl;
141200170504         DSTnbl = APD2nbl;
141300170504         DSTnftl= APD2nftl;
141400170504         DSTtol = APD2tol;
141500170504         if  APD2inc = *blank;
141600170504           DSTinc = 'S';
141700170504         else;
141800170504           DSTinc = APD2inc;
141900170504         endif;
142000170504         clear  DSTpklA;
142100170504         clear  DSTvluA;
142200170504         clear  DSTstpA;
142300170504         clear  DSTnblA;
142400170504
142500170504         // -?Aggiornamento ORM in FIDST00F?
142600170504         If  Og147.§OGdDao <> *blank;
142700170504           DSTrvlu  = APD2rvlu;
142800170504           DSTrpkl  = APD2rpkl;
142900170504           DSTrnbl  = APD2rnbl;
143000170504           DSTrnftl = APD2rnftl;
143100170504           DSTrtol  = APD2rtol;
143200170504           if  APD2rinc = *blank;
143300170504             DSTrinc = 'S';
143400170504           else;
143500170504             DSTrinc = APD2rinc;
143600170504           endif;
143700170504           if  APD2rspi = *blank;
143800170504             DSTrspi = 'N';
143900170504           else;
144000170504             DSTrspi = APD2rspi;
144100170504           endif;
144200170504         Else;
144300170504           clear  DSTrvlu;
144400170504           clear  DSTrpkl;
144500170504           clear  DSTrnbl;
144600170504           clear  DSTrnftl;
144700170504           clear  DSTrtol;
144800170504           clear  DSTrinc;
144900170504           clear  DSTrspi;
145000170504         EndIf;
145100170504         clear  DSTrpklA;
145200170504         clear  DSTrvluA;
145300170504         clear  DSTrstpA;
145400170504         clear  DSTrnblA;
145500170504         clear  DSTrnbaA;
145600170504
145700170508         if  NOT  $Test;       //? Test ? ?
145800170505           //_______________
145900170505           update  FIDST008;
146000170505           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
146100170508         else;
146200170508           unlock  FIDST08L;
146300170508         endif;                //? Test ? ?
146400170505
146500170505         wEstens_FIDST0 += 1;
146600170504
146700170504       ENDSR;
146800170504
146900170504       //--------------------------------------------------------------
147000170504       //?Scrittura FIDST30F (Pianificazione Giri Consegne).           ?
147100170504       //--------------------------------------------------------------
147200170504       BEGSR  sr_Wrt_FIDST30;
147300170504
147400170504         // -?Scrittura dei Giri Consegne?
147500170504         clear  FIDST300;
147600170504         DST3fgs = DSTfgs;
147700170504         DST3npg = c_Cat_Consegne;
147800170504         DST3dfv = DSTdfv;
147900170504         DST3nfv = DSTnfv;
148000170504
148100170504         clear  KeyFIAPD31;
148200170504         k_APD3nras = APD2nras;
148300170504         setll  %kds( keyFIAPD31 )  FIAPD300;
148400170504         reade  %kds( keyFIAPD31 : 1 )  FIAPD300;
148500170504
148600170504         DoW  Not %eof( FIAPD31L );
148700170504
148800170504           If  APD3atb = *blank;
148900170504             DST3fgsg = APD3fgsg;
149000170504             DST3cgi  = APD3cgi;
149100170504             DST3prgg = APD3prgg;
149200170504             DST3sql  = APD3sql;
149300170504             reset  FIDG09ds;
149400170504             FIDG09ds.D09ifgs  = APD3fgsg;
149500170504             FIDG09ds.D09icgi  = APD3cgi;
149600170504             FIDG09ds.D09itug  = 'C';
149700170504             exsr  sr_CtrlCGI;
149800170504             if  FIDG09ds.D09oerr <> *on;
149900170508               if  NOT  $Test;           //? Test ? ?
150000170505                 //______________
150100170505                 write  FIDST300;
150200170505                 //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
150300170508               endif;                    //? Test ? ?
150400170505               wEstens_FIDST3 += 1;
150500170504             endif;
150600170504           EndIf;
150700170504
150800170504           reade  %kds( keyFIAPD31 : 1 )  FIAPD300;
150900170504
151000170504         EndDo;
151100170504
151200170504       ENDSR;
151300170504
151400170504       //--------------------------------------------------------------
151500170504       //?Scrittura FIDST50F (Pianificazione Giri O.R.M.).             ?
151600170504       //--------------------------------------------------------------
151700170504       BEGSR  sr_Wrt_FIDST50;
151800170504
151900170504         if  Og147.§OGdDao = *blank;
152000170504           leavesr;
152100170504         endif;
152200170504
152300170504         // -?Scrittura dei Giri ORM?
152400170504         clear  FIDST500;
152500170504         DST5fgs  = DSTfgs;
152600170504         DST5npg  = c_Cat_Consegne;
152700170504         DST5dfv  = DSTdfv;
152800170504         DST5nfv  = DSTnfv;
152900170504
153000170504         clear  KeyFIAPD31;
153100170504         k_APD3nras = APD2nras;
153200170504         setll  %kds( keyFIAPD31 )  FIAPD500;
153300170504         reade  %kds( keyFIAPD31 : 1 )  FIAPD500;
153400170504
153500170504         DoW  Not %eof( FIAPD51L );
153600170504
153700170504           If  APD5atb = *blank;
153800170504             DST5fgsg = APD5fgsg;
153900170504             DST5cgi  = APD5cgi;
154000170504             DST5prgg = APD5prgg;
154100170504             DST5sql  = APD5sql;
154200170504             reset  FIDG09ds;
154300170504             FIDG09ds.D09ifgs  = APD5fgsg;
154400170504             FIDG09ds.D09icgi  = APD5cgi;
154500170504             FIDG09ds.D09itug  = 'R';
154600170504             exsr  sr_CtrlCGI;
154700170504             if  FIDG09ds.D09oerr <> *on;
154800170508               if  NOT  $Test;           //? Test ? ?
154900170505                 //______________
155000170505                 write  FIDST500;
155100170505                 //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
155200170508               endif;                    //? Test ? ?
155300170505               wEstens_FIDST5 += 1;
155400170504             endif;
155500170504           EndIf;
155600170504
155700170504           reade  %kds( keyFIAPD31 : 1 )  FIAPD500;
155800170504
155900170504         EndDo;
156000170504
156100170504       ENDSR;
156200170504
156300170504       //--------------------------------------------------------------
156400170504       //?Controllo Codice Giro.                                       ?
156500170504       //--------------------------------------------------------------
156600170504       BEGSR  sr_CtrlCGI;
156700170504
156800170504         //*·FIDG09ds.D09iOp0 = '001';  ?(già così)?
156900170504         FIDG09ds.D09iDat = DSTdfv;
157000170504         kpjbu    = FIDG09ds;
157100170504
157200170504         FIDG09R ( kpjba );
157300170504
157400170504         FIDG09ds = kpjbu;
157500170504
157600170504       ENDSR;
157700170314
157800170314       //--------------------------------------------------------------
157900170314       //?Operazioni finali.                                           ?
158000170314       //--------------------------------------------------------------
158100170314       BEGSR  sr_RoutEnd;
158200170314
158300170314         // -?Chiusura archivi?
158400170316         close  FIDST01L;
158500170505         close  FIDST08L;
158600170320         close  FNFVV01L;
158700170504         if  %open( FIAPD22L );
158800170504           close  FIAPD22L;
158900170504         endif;
159000170504         if  %open( FIAPD31L );
159100170504           close  FIAPD31L;
159200170504         endif;
159300170504         if  %open( FIAPD51L );
159400170504           close  FIAPD51L;
159500170504         endif;
159600170504         if  %open( FIDST30F );
159700170504           close  FIDST30F;
159800170504         endif;
159900170504         if  %open( FIDST50F );
160000170504           close  FIDST50F;
160100170504         endif;
160200170314
160300170314         // -?Uscita dal *pgm?
160400170314         return;
160500170314
160600170314       ENDSR;
160700170314
160800170314      /end-free
160900170314
161000170314       //--------------------------------------------------------------
161100170504       //?Definizione schiere a tempo di compilazione.                 ?
161200170314       //--------------------------------------------------------------
161300170320
161400170320** --?sk_NPG:?Categorie Foglio da elaborare?
1615001703234 - Consegne
1616001703238 - Rientri
161700170314** --?sk_Msg:?Messaggi di Errore?--------------------------------------------*
161800170314Funzione eseguibile SOLO in ambiente di Filiale: premere "Invio" per uscire     1
161900170322Filiale in gestione errata                                                      2
162000170316Data Distinta obbligatoria                                                      3
162100170316Data Distinta formalmente errata                                                4
162200170508Numero Distinta obbligatorio                                                    5
162300170508Distinta inesistente                                                            6
162400170508Distinta errata:                                                                7
162500170509Valore errato per il campo: "S" = Sì, " " = No                                  8
162600170509Risultano presenti dei Giri pianificati nella distinta                          9
162700170509Tutte le distinte hanno Consegne/ORM assegnati o giri pianificati              10
162800170509Reperite &NOupd distinte con Consegne/ORM già assegnate                        11
162900170509NON trovata alcuna distinta modificabile                                       12
