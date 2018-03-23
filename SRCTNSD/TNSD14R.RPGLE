000100110607       //==============================================================
000200110607       //?TNSD14R * Attivazione procedure PDA (ORM e Consegne)         ?
000300110607       //==============================================================
000400110607
000500110607       //--------------------------------------------------------------
000600110607       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700110607       //--------------------------------------------------------------
000800110607
000900110607     /*PRM dbgview(*source)
001000110607     /*END
001100110607
001200110607       //--------------------------------------------------------------
001300110607       //?Specifiche di controllo.                                     ?
001400110607       //--------------------------------------------------------------
001500110607
001600110607     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700110607     h dftactgrp(*no)
001800110607     h bnddir('UBRTVNETA')
001900110607
002000110607       //--------------------------------------------------------------
002100110607       //?Dichiarazione file.                                          ?
002200110607       //--------------------------------------------------------------
002300110607
002400110607       // -?Organigramma filiale/aziendale?
002500110607     fAZORG01L  Uf   e           k disk
002600110607     f                                     extfile(wLibFile)  usropn
002700110607
002800110607       // -?Video Gestione?
002900110607     fTNSD14D   cf   e             workstn
003000110607     f                                     indds(IndDspF)
003100110607     f                                     infds(InfDspF)
003200110607
003300110607       //--------------------------------------------------------------
003400110607       //?Definizione costanti.                                        ?
003500110607       //--------------------------------------------------------------
003600110607
003700110607       // -?Tasti funzionali a video?
003800110607     d c_F01           c                   const(x'31')
003900110607     d c_F02           c                   const(x'32')
004000110607     d c_F03           c                   const(x'33')
004100110607     d c_F04           c                   const(x'34')
004200110607     d c_F05           c                   const(x'35')
004300110607     d c_F06           c                   const(x'36')
004400110607     d c_F07           c                   const(x'37')
004500110607     d c_F08           c                   const(x'38')
004600110607     d c_F09           c                   const(x'39')
004700110607     d c_F10           c                   const(x'3A')
004800110607     d c_F11           c                   const(x'3B')
004900110607     d c_F12           c                   const(x'3C')
005000110607     d c_F13           c                   const(x'B1')
005100110607     d c_F14           c                   const(x'B2')
005200110607     d c_F15           c                   const(x'B3')
005300110607     d c_F16           c                   const(x'B4')
005400110607     d c_F17           c                   const(x'B5')
005500110607     d c_F18           c                   const(x'B6')
005600110607     d c_F19           c                   const(x'B7')
005700110607     d c_F20           c                   const(x'B8')
005800110607     d c_F21           c                   const(x'B9')
005900110607     d c_F22           c                   const(x'BA')
006000110607     d c_F23           c                   const(x'BB')
006100110607     d c_F24           c                   const(x'BC')
006200110607     d c_Enter         c                   const(x'F1')
006300110607     d c_RollDown      c                   const(x'F4')
006400110607     d c_RollUp        c                   const(x'F5')
006500110607
006600110607       // -?Costanti per la definizione delle schiere con i nomi?
006700110607       //  ?degli iSeries da elaborare e delle relative librerie?
006800110607     d c_NrSyst        c                   const(2)
006900110607     d c_NrLibr        c                   const(2)
007000110607
007100110607       // -?Costante per controllo "caratteri solo numerici"?
007200110607     d c_Digits        c                   const('0123456789')
007300110607
007400110607       //--------------------------------------------------------------
007500110607       //?Definizione schiere.                                         ?
007600110607       //--------------------------------------------------------------
007700110607
007800110607       // -?iSeries  &  Librerie con il file organigramma?
007900110607     d $iSystem        s                   like(currSysNetA)
008000110607     d                                     dim(c_NrSyst)
008100110607     d                                     ctdata   perrcd( 1)
008200110607     d $Libraries      s                   like(ds_Libr)
008300110607     d                                     dim(c_NrSyst)
008400110607     d                                     alt($iSystem)
008500110607
008600110607       // -?Messaggi di errore?
008700110609     d $Msg            s             78    dim( 8) ctdata perrcd(1)
008800110607
008900110607       //--------------------------------------------------------------
009000110607       //?Definizione aree dati.                                       ?
009100110607       //--------------------------------------------------------------
009200110607
009300110607       // -?Dati utente?
009400110607     d §AzUte        e ds                  extname(AZUTE00F)
009500110607     d                                     dtaara
009600110607     d §DatiUte      e ds                  extname(dDatiUte)
009700110607     d                                     dtaara
009800110607
009900110607       //--------------------------------------------------------------
010000110607       //?Definizione strutture dati.                                  ?
010100110607       //--------------------------------------------------------------
010200110607
010300110607       // -?Status ds?
010400110607     d Status         sds
010500110607     d  SDSpgm           *proc
010600110607
010700110607       // -?InfDS?
010800110607     d InfDspF         ds
010900110607     d   dsp_aid             369    369a
011000110607
011100110607       // -?Indicatori su DspF?
011200110607     d IndDspF         ds                  inz
011300110607         // ·?Abilitazione tasti funzionali?
011400110607     d   F6Attivo                      n   overlay(IndDspF : 06)
011500110607     d   F12Attivo                     n   overlay(IndDspF : 12)
011600110607         // ·?Emissione messaggio di errore?
011700110607     d   ErrMessage                    n   overlay(IndDspF : 28)
011800110607         // ·?Indicatori per Attibuti di visualizzazione?
011900110609     d   ProtectORM                    n   overlay(IndDspF : 41)
012000110609     d   ProtectCON                    n   overlay(IndDspF : 42)
012100110607         // ·?Posizionamento cursore & segnalazione errore?
012200110607     d   PosCurPWD                     n   overlay(IndDspF : 50)
012300110607     d   PosCurFIL                     n   overlay(IndDspF : 51)
012400110607     d   PosCurPDAORM                  n   overlay(IndDspF : 52)
012500110607     d   PosCurPDACON                  n   overlay(IndDspF : 53)
012600110607         // ·?Riemissione videata?
012700110607     d   ErrGenerico                   n   overlay(IndDspF : 99)
012800110607
012900110607       // -?Ridefinizione elenco librerie in elaborare l'organigramma?
013000110607     d ds_Libr         ds                  inz
013100110607     d  $Libr                        10    dim(c_NrLibr) inz
013200110607
013300110607       // -?Parametri ricevuti?
013400110607     d KPJBA         e ds
013500110607
013600110607       // -?148ª descrizione dell'organigramma?
013700110607     d Og148         e ds                  inz
013800110607
013900110607       //--------------------------------------------------------------
014000110607       //?Definizione variabili globali.                               ?
014100110607       //--------------------------------------------------------------
014200110607
014300110607       // -?Flags booleani?
014400110607     d $Fine           s               n   inz
014500110608     d $InzW01         s               n   inz(*on)
014600110607     d $InzD01         s               n   inz(*on)
014700110607     d $InzD02         s               n   inz(*on)
014800110607
014900110607       // -?Variabili per la gestione del video?
015000110608     d $Video          s              2    inz('W1')
015100110607
015200110607       // -?Nome del sistema?
015300110607     d currSysNeta     s              8a   inz
015400110607
015500110607       // -?Nome esteso Libreria/File del file AZORG00F?
015600110607     d wLibFile        s             21a   inz
015700110607
015800110607       // -?Campi di comodo?
015900110607     d w_iSystem       s              1  0 inz
016000110607     d w_SisInf        s              3  0 inz
016100110607     d wDate           s              8  0 inz
016200110607
016300110607       //--------------------------------------------------------------
016400110607       //?Definizione prototipi procedure.                             ?
016500110607       //--------------------------------------------------------------
016600110607
016700110607       // -?Reperimento NETA sistema AS/400 corrente?
016800110607      /copy gaitrasrc/srcProtoPr,UBRTVNETA
016900110607
017000110607       // -?Reperimento dati utente?
017100110607     d TIBS34ds      e ds                  inz
017200110607      /copy gaitrasrc/srcProtoPR,TIBS34R
017300110607
017400110607       // -?Ricerca filiale in organigramma?
017500110607      /copy gaitrasrc/srcProtoPI,TNSD24R
017600110607      /copy gaitrasrc/srcProtoPR,TNSD24R
017700110607
017800110607       //--------------------------------------------------------------
017900110607       //?Definizione key-list.                                        ?
018000110607       //--------------------------------------------------------------
018100110607
018200110607       // -?File AZORG01L?
018300110607     d k01azorg01    e ds                  extname(AZORG01L : *key)
018400110607     d                                     prefix(k_)   inz
018500110607
018600110607       //--------------------------------------------------------------
018700110607       //?Specifiche di calcolo.                                       ?
018800110607       //--------------------------------------------------------------
018900110607
019000110607     c     *Entry        plist
019100110607     c                   parm                    KPJBA
019200110607
019300110607      /free
019400110607
019500110607       // -?Operazioni iniziali?
019600110608       exsr  sr_RoutInz;
019700110607
019800110607       // -?Gestione videate?
019900110607       DoW  $Fine = *off;
020000110607
020100110607         select;
020200110607           // -?Gestione videata D1 (richiesta codice filiale)?
020300110607           when $Video = 'D1';
020400110608             exsr  sr_GesD01;
020500110607           // -?Gestione videata D2 (manutenzione dati filiale)?
020600110607           when $Video = 'D2';
020700110608             exsr  sr_GesD02;
020800110608           // -?Gestione videata W1 (richiesta password)?
020900110608           when $Video = 'W1';
021000110608             exsr  sr_GesW01;
021100110607           // -?? ? ? ? ??
021200110607           other;
021300110607             $Fine = *on;
021400110607         endsl;
021500110607
021600110607       enddo;
021700110607
021800110607       // -?Operazioni finali?
021900110608       exsr  sr_RoutEnd;
022000110607
022100110607       //--------------------------------------------------------------
022200110607       //?Operazioni iniziali.                                         ?
022300110607       //--------------------------------------------------------------
022400110607       BEGSR  sr_RoutInz;
022500110607
022600110607         *inLR = *on;
022700110607
022800110607         // -?Verifica del sistema AS/400 corrente?
022900110607         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
023000110607           $Fine = *on;
023100110607           leavesr;
023200110607         endif;
023300110607
023400110607         // -?Impostazione elenco librerie in cui gestire l'organigramma?
023500110607         //  ?(a seconda del sistema in cui si stà lavorando)?
023600110607         w_iSystem = %lookup ( %subst ( currSysNeta : 1 : 6 ) : $iSystem );
023700110607         if  w_iSystem = *zero;
023800110607           $Fine = *on;
023900110607           leavesr;
024000110607         endif;
024100110607
024200110607         // -?Reperimento dati job?
024300110607         exsr  sr_DatiJob;
024400110607
024500110607         // -?Reperimento data odierna?
024600110607         wDate = %int( %subst( %char( %dec( %timestamp() ) )
024700110607                               : 1 : 8 ) );
024800110607
024900110607         // -?Impostazione nome programma a video?
025000110607         V1Tpgm = SDSpgm;
025100110607
025200110607       ENDSR;
025300110607
025400110607       //--------------------------------------------------------------
025500110607       //?Reperimento Dati del job (Utente/Operativi).                 ?
025600110607       //--------------------------------------------------------------
025700110607       BEGSR  sr_DatiJob;
025800110607
025900110607         in(e) §AzUte;
026000110607         if NOT %error;
026100110607           in(e) §DatiUte;
026200110607         endif;
026300110607         if %error or RSut = *blank;
026400110607           tibs34r ( tibs34ds );
026500110607           in §AzUte;
026600110607           in §DatiUte;
026700110607         endif;
026800110607
026900110607       ENDSR;
027000110608
027100110608       //--------------------------------------------------------------
027200110608       //?Gestione window W01.                                         ?
027300110608       //--------------------------------------------------------------
027400110608       BEGSR  sr_GesW01;
027500110608
027600110608         // -?Inizializzazione window?
027700110608         if  $InzW01 = *on;
027800110608           clear  SD14W01;
027900110608           $InzD01 = *off;
028000110608           exsr  sr_InzD01;
028100110608           $InzW01 = *off;
028200110608           write  SD14T01;
028300110608           write  SD14D01;
028400110608           //write  SD14P01;       ?(coperto dalla window)?
028500110608         endif;
028600110608
028700110608         // -?Emissione window?
028800110608         exfmt  SD14W01;
028900110608
029000110608         clear  V1Dmsg;
029100110608         reset  ErrMessage;
029200110608         reset  ErrGenerico;
029300110608
029400110608         SELECT;
029500110608
029600110608           // -?F3=Fine?
029700110608           WHEN  dsp_aid = c_F03;
029800110608             $Fine = *on;
029900110608
030000110608           // -?Invio?
030100110608           OTHER;
030200110608             exsr  sr_CtrW01;
030300110608             if  ErrGenerico = *on;
030400110608               leavesr;
030500110608             endif;
030600110608             $Video  = 'D1';
030700110608             //$InzD01 = *on;      ?(videata già inizializzata)?
030800110608
030900110608         ENDSL;
031000110608
031100110608       ENDSR;
031200110608
031300110608       //--------------------------------------------------------------
031400110608       //?Controllo dati immessi nella window W01.                     ?
031500110608       //--------------------------------------------------------------
031600110608       BEGSR  sr_CtrW01;
031700110608
031800110608         // -?Spegnimento degli indicatori di errore?
031900110608         %subst(IndDspF : 50) = *off;
032000110608
032100110608         // -?Controllo password?
032200110608         Select;
032300110608           // ·?Obbligatoria?
032400110608           When  W1Cpwd = *blank;
032500110609             V1Dmsg = $Msg(01);
032600110608             PosCurPWD   = *on;
032700110608             ErrMessage  = *on;
032800110608             ErrGenerico = *on;
032900110608             leavesr;
033000110608           // ·?Errata?
033100110608           When  W1Cpwd <> 'ATTIVA';
033200110609             V1Dmsg = $Msg(02);
033300110608             PosCurPWD   = *on;
033400110608             ErrMessage  = *on;
033500110608             ErrGenerico = *on;
033600110608             leavesr;
033700110608         EndSl;
033800110608
033900110608       ENDSR;
034000110607
034100110607       //--------------------------------------------------------------
034200110607       //?Gestione videata D01.                                        ?
034300110607       //--------------------------------------------------------------
034400110607       BEGSR  sr_GesD01;
034500110607
034600110607         // -?Inizializzazione videata?
034700110607         if  $InzD01 = *on;
034800110607           exsr  sr_InzD01;
034900110607           $InzD01 = *off;
035000110607         endif;
035100110607
035200110607         // -?Emissione Testata e Piede?
035300110607         write  SD14T01;
035400110607         write  SD14P01;
035500110607
035600110607         // -?Emissione videata?
035700110607         exfmt  SD14D01;
035800110607
035900110607         clear  V1Dmsg;
036000110607         reset  ErrMessage;
036100110607         reset  ErrGenerico;
036200110607
036300110607         SELECT;
036400110607
036500110607           // -?F3=Fine?
036600110607           WHEN  dsp_aid = c_F03;
036700110607             $Fine = *on;
036800110607
036900110607           // -?Invio?
037000110607           OTHER;
037100110608             exsr  sr_CtrD01;
037200110607             if  ErrGenerico = *on;
037300110607               leavesr;
037400110607             endif;
037500110607             $Video  = 'D2';
037600110607             $InzD02 = *on;
037700110607
037800110607         ENDSL;
037900110607
038000110607       ENDSR;
038100110607
038200110607       //--------------------------------------------------------------
038300110607       //?Inizializzazione videata D01 - Richiesta codice filiale.     ?
038400110607       //--------------------------------------------------------------
038500110607       BEGSR  sr_InzD01;
038600110607
038700110608         clear  SD14D01;
038800110607         V1Cfil = '?';
038900110607
039000110607         // -?(Dis)Abilitazione tasti funzionali?
039100110607         F6Attivo  = *off;
039200110607         F12Attivo = *off;
039300110607
039400110607       ENDSR;
039500110607
039600110607       //--------------------------------------------------------------
039700110607       //?Controllo dati immessi nella videata D01.                    ?
039800110607       //--------------------------------------------------------------
039900110607       BEGSR  sr_CtrD01;
040000110607
040100110607         // -?Spegnimento degli indicatori di errore?
040200110607         %subst(IndDspF : 50) = *off;
040300110607
040400110607         // -?Controllo codice filiale?
040500110607         clear  V1Dfil;
040600110607         clear  V1Dfag;
040700110607         Select;
040800110607           // ·?Obbligatorio?
040900110607           When  V1Cfil = *blank  or  V1Cfil  = *zero;
041000110609             V1Dmsg = $Msg(03);
041100110607             PosCurFIL   = *on;
041200110607             ErrMessage  = *on;
041300110607             ErrGenerico = *on;
041400110607             leavesr;
041500110607           // ·?Richiesta ricerca?
041600110607           When  %scan( '?' : V1Cfil ) > *zeros;
041700110607             clear  p_Fil;
041800110607             clear  p_Des;
041900110607             p_Fag = 'F';
042000110607             tnsd24r ( p_FIL : p_FAG : p_DES : p_DIT );
042100110607             V1Cfil = p_FIL;
042200110607             V1Dfil = p_DES;
042300110607             ErrGenerico = *on;
042400110607             leavesr;
042500110607           // ·?Controllo immisione solo caratteri numerici?
042600110607           When  %check( c_Digits : V1Cfil ) > *zero;
042700110609             V1Dmsg = $Msg(04);
042800110607             PosCurFIL   = *on;
042900110607             ErrMessage  = *on;
043000110607             ErrGenerico = *on;
043100110607             leavesr;
043200110607         EndSl;
043300110607
043400110607
043500110607         // -?Apertura file AZORG01L del 1° S.I. (sede)?
043600110607         if  w_SisInf <> 1   or   Not %open(AZORG01L);
043700110607           w_SisInf = 1;
043800110607           exsr  sr_OpenFileORG;
043900110607         endif;
044000110607
044100110607
044200110607         // -?Aggancio record di AZORG:?
044300110607         //  ?verifica esistenza filiale in AZORG?
044400110607         k_ORGfil = %int( V1Cfil );
044500110607         chain  %kds( k01azorg01 )  AZORG;
044600110607
044700110607         if  NOT %found(AZORG01L)  or  ORGfva <> *blank;
044800110608           unlock  AZORG01L;
044900110609           V1Dmsg = $Msg(05);
045000110607           PosCurFIL   = *on;
045100110607           ErrMessage  = *on;
045200110607           ErrGenerico = *on;
045300110607           leavesr;
045400110607         endif;
045500110607
045600110607         V1Dfil = ORGdes;
045700110609         Og148  = ORGde8;
045800110607
045900110607         // -?Verifica se filiale?
046000110607         select;
046100110607           when  ORGfag = 'F';
046200110608             //V1Dfag = 'FILIALE';
046300110607           when  ORGfag = 'A';
046400110607             V1Dfag = 'AGENZIA';
046500110607           when  ORGfag = 'V'  and  ORGfl2 = 'S';
046600110607             V1Dfag = 'LINEA PER TARIFFE';
046700110607           when  ORGfag = 'V';
046800110607             V1Dfag = 'VARIE';
046900110607         endsl;
047000110607         if  ORGfag <> 'F';
047100110609           V1Dmsg = $Msg(06);
047200110607           PosCurFIL   = *on;
047300110607           ErrMessage  = *on;
047400110607           ErrGenerico = *on;
047500110607           leavesr;
047600110607         endif;
047700110607
047800110607       ENDSR;
047900110607
048000110607       //--------------------------------------------------------------
048100110607       //?Apertura dell'organigramma nel sistema informativo impostato.?
048200110607       //--------------------------------------------------------------
048300110607       BEGSR  sr_OpenFileORG;
048400110607
048500110607         // -?Chiusura (eventuale) archivio?
048600110607         if  %open(AZORG01L);
048700110607           close  AZORG01L;
048800110607         endif;
048900110607
049000110607         // -?Apertura archivio?
049100110607         ds_Libr  = $Libraries(w_iSystem);
049200110607         wLibFile = %trimr( $Libr(w_SisInf) ) + '/' + 'AZORG01L';
049300110607         open  AZORG01L;
049400110607
049500110607       ENDSR;
049600110607
049700110607       //--------------------------------------------------------------
049800110607       //?Gestione videata D02.                                        ?
049900110607       //--------------------------------------------------------------
050000110607       BEGSR  sr_GesD02;
050100110607
050200110607         // -?Inizializzazione videata?
050300110607         if  $InzD02 = *on;
050400110607           exsr  sr_InzD02;
050500110607           $InzD02 = *off;
050600110607         endif;
050700110607
050800110607         // -?Emissione Testata e Piede?
050900110607         if  NOT ErrMessage;
051000110607           write  SD14T01;
051100110607           write  SD14D01;
051200110607         endif;
051300110607         write  SD14P01;
051400110607         write  Protect;
051500110607
051600110607         // -?Emissione videata?
051700110607         exfmt  SD14D02;
051800110607
051900110607         clear  V1Dmsg;
052000110607         reset  ErrMessage;
052100110607         reset  ErrGenerico;
052200110607
052300110607         SELECT;
052400110607
052500110607           // -?F3=Fine?
052600110607           WHEN  dsp_aid = c_F03;
052700110607             unlock  AZORG01L;
052800110607             $Fine = *on;
052900110607
053000110607           // -?F12=Ritorno?
053100110607           WHEN  dsp_aid = c_F12;
053200110607             unlock  AZORG01L;
053300110607             $Video = 'D1';
053400110607             F6Attivo  = *off;
053500110607             F12Attivo = *off;
053600110607
053700110607           // -?Invio, F6=Aggiornamento?
053800110607           OTHER;
053900110608             exsr  sr_CtrD02;
054000110607             if  ErrGenerico = *on;
054100110607               leavesr;
054200110607             endif;
054300110607             // -?Aggiornamento?
054400110607             if  dsp_aid = c_F06;
054500110607               exsr  sr_UpdateORG;
054600110607               if  ErrGenerico = *on;
054700110607                 leavesr;
054800110607               endif;
054900110607               // ·?Ritorno alla videata iniziale?
055000110607               $Video  = 'D1';
055100110607               $InzD01 = *on;
055200110607             endif;
055300110607
055400110607         ENDSL;
055500110607
055600110607       ENDSR;
055700110607
055800110607       //--------------------------------------------------------------
055900110607       //?Inizializzazione dati nella videata D02.                     ?
056000110607       //--------------------------------------------------------------
056100110607       BEGSR  sr_InzD02;
056200110607
056300110607         clear  SD14D02;
056400110607
056500110607         // -?Caricamento dati a video?
056600110607         V2PdaOrm = §OGpdaOrm;
056700110607         V2PdaCon = §OGpdaCon;
056800110609
056900110609         // -?(Dis)Attivazione indicatori che condizionano attributi di?
057000110609         //  ?visualizzazione?
057100110609         ProtectORM = (§OGpdaOrm = 'S');
057200110609         ProtectCON = (§OGpdaCon = 'S');
057300110607
057400110607         // -?Impostazione indicatori per abilitazione tasti funzionali?
057500110607         exsr  sr_AbilitFxxD02;
057600110607
057700110607       ENDSR;
057800110607
057900110607       //--------------------------------------------------------------
058000110607       //?Abilitazione tasti funzionali in P02 (per D02 e D03).        ?
058100110607       //--------------------------------------------------------------
058200110607       BEGSR  sr_AbilitFxxD02;
058300110607
058400110607         // ->?F6 = Conferma?
058500110607         F6Attivo = ( §OGpdaORM = *blank  or  §OGpdaCON = *blank );
058600110607
058700110607         // ->?F12 = Ritorno?
058800110607         F12Attivo = *on;
058900110607
059000110607       ENDSR;
059100110607
059200110607       //--------------------------------------------------------------
059300110607       //?Controllo dati videata D02.                                  ?
059400110607       //--------------------------------------------------------------
059500110607       BEGSR  sr_CtrD02;
059600110607
059700110607         // -?Spegnimento degli indicatori di errore?
059800110607         %subst(IndDspF : 50) = *off;
059900110607
060000110609         // -?Attivare almeno una procedura PDA?
060100110609         if  V2PdaOrm = *blank  and  V2PdaCon = *blank;
060200110609           V1Dmsg = $Msg(07);
060300110609           PosCurPdaORM = *on;
060400110609           ErrMessage   = *on;
060500110609           ErrGenerico  = *on;
060600110609           leavesr;
060700110609         endif;
060800110607
060900110607       ENDSR;
061000110607
061100110607       //--------------------------------------------------------------
061200110607       //?Aggiornamento organigramma in entrambi i sistemi informativi ?
061300110607       //--------------------------------------------------------------
061400110607       BEGSR  sr_UpdateORG;
061500110607
061600110607         // -?Valorizzazione campi dell'organigramma di filiale Og148?
061700110607         §OGpdaORM = V2PdaOrm;
061800110607         §OGpdaCON = V2PdaCon;
061900110607
062000110607         // -?Ciclo di elaborazione per ogni sistema informativo?
062100110607         For  w_SisInf = 1  To  %elem($Libr);
062200110607
062300110607           // -?Apertura degli archivi?
062400110607           if  w_SisInf > 1;
062500110607             exsr  sr_OpenFileORG;
062600110607           endif;
062700110607
062800110607           // -?Aggiornamento organigramma?
062900110607           exsr  sr_UpdAZORG;
063000110607           if  ErrGenerico;
063100110607             leavesr;
063200110607           endif;
063300110607
063400110607         EndFor;
063500110607
063600110607       ENDSR;
063700110607
063800110607       //--------------------------------------------------------------
063900110607       //?Aggiornamento record AZORG00F.                               ?
064000110607       //--------------------------------------------------------------
064100110607       BEGSR  sr_UpdAZORG;
064200110607
064300110607         //? N.B.                                                      ?
064400110607         //L'aggiornamento  viene registrato subito nello stesso file ?
064500110607         //  di entrambi i S.I. - in due cicli diversi - ma NON viene ?
064600110607         //  messo in trasmissione.                                   ?
064700110607
064800110607         // -?RI-aggancio record da aggiornare?
064900110607         //  ?(dopo aver cambiato la libreria del file da aggiornare)?
065000110607         if  w_SisInf > 1;
065100110607           k_ORGfil = %int( V1Cfil );
065200110607           chain  %kds( k01azorg01 )  AZORG;
065300110607         endif;
065400110607
065500110607         // -?Valorizzazione 148° campo dell'organigramma di filiale?
065600110607         ORGde8 = Og148;
065700110607
065800110607         // -?Impostazione altri campi del record?
065900110607         if  w_SisInf = 1;
066000110607           ORGftr = 'T';
066100110607         else;
066200110607           ORGftr = 'R';
066300110607         endif;
066400110607         ORGntr = %int( %subst( %editc(wDate : 'X') :
066500110607                                %len(wDate) - %len(ORGntr) + 1 :
066600110607                                %len(ORGntr) ) );
066700110607         ORGfel = DUTfel;
066800110607
066900110607         // -?Aggiornamento record (F6 = Aggiornamento)?
067000110607         if  %found(AZORG01L);
067100110607           //___________
067200110607           UPDATE AZORG;
067300110607           //¯¯¯¯¯¯¯¯¯¯¯
067400110607         else;
067500110607           if  w_SisInf = 1;
067600110609             V1Dmsg = %trimr( $Msg(08) ) + ' SEDE';
067700110607           else;
067800110609             V1Dmsg = %trimr( $Msg(08) ) + ' FILIALE';
067900110607           endif;
068000110607           ErrMessage  = *on;
068100110607           ErrGenerico = *on;
068200110607           leavesr;
068300110607         endif;
068400110607
068500110607       ENDSR;
068600110607
068700110607       //--------------------------------------------------------------
068800110607       //?Operazioni finali.                                           ?
068900110607       //--------------------------------------------------------------
069000110609       BEGSR  sr_RoutEnd;
069100110607
069200110607         // -?Chiusura pgm?
069300110607         return;
069400110607
069500110607       ENDSR;
069600110607
069700110607      /end-free
069800110607
069900110607       //--------------------------------------------------------------
070000110607       //?Definizione schiere a tempo di compilazione                  ?
070100110607       //--------------------------------------------------------------
070200110607
070300110607** -?$iSystem / $Libraries:?Sistemi AS/400 & Librerie con entrambi i file?
070400110607SETRAS  GAITRAGRU FILTRAGRU
070500110607AS888   GAITRAGRPSFILTRAGRPF
070600110607** -?$Msg:?Messaggi di Errore?-----------------------------------------------*
070700110609Immettere la password                                                           1
070800110609Password errata                                                                 2
070900110609Immettere la filiale                                                            3
071000110609Ammessi SOLO caratteri numerici                                                 4
071100110609Filiale inesistente                                                             5
071200110609Codice filiale errato                                                           6
071300110609Attivare almeno una procedura PDA                                               7
071400110609Fallito aggiornamento: record NON trovato nell'ambiente di                      8
