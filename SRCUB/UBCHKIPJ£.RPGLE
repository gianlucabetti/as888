000100150305       //==============================================================
000200150305       //?UBCHKJIP - Controllo IP Address del lavoro.                  ?
000300150305       //==============================================================
000400150305
000500150305       //--------------------------------------------------------------
000600150305       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700150305       //--------------------------------------------------------------
000800150305
000900150305     /*PRM  dbgview(*source)
001000150305     /*END
001100150305
001200150305       //--------------------------------------------------------------
001300150305       //?Specifiche di controllo.                                     ?
001400150305       //--------------------------------------------------------------
001500150305
001600150305     h decedit('0,') datedit(*dmy/) debug(*yes) option(*nodebugio)
001700150309     h dftactgrp(*no)
001800150305
001900150305       //--------------------------------------------------------------
002000150305       //?Dichiarazione file.                                          ?
002100150305       //--------------------------------------------------------------
002200150505
002300150511       //// -?Utenti aziendali (Modulo Base)?
002400150511     f*//KPPRF01L  if   e           k disk
002500150305
002600150305       // -?Organigramma Filiale/Aziendale?
002700150511     fAZORG01L  if   e           k disk
002800150305
002900150305       // -?Controllo Accessi all'AS/400?
003000150511     fAZCAA00F  o    e             disk
003100150305
003200150305       //--------------------------------------------------------------
003300150305       //?Definizione costanti.                                        ?
003400150305       //--------------------------------------------------------------
003500150305
003600150305
003700150305       //--------------------------------------------------------------
003800150305       //?Definizione schiere.                                         ?
003900150305       //--------------------------------------------------------------
004000150305
004100150309       // -?Messaggi di errore?
004200150310     d sk_Msg          s             75    dim(10)  ctdata  perrcd( 1)
004300150305
004400150305       //--------------------------------------------------------------
004500150305       //?Definizione aree dati.                                       ?
004600150305       //--------------------------------------------------------------
004700150305
004800150305       // -?Dati utente?
004900150305     d §AzUte        e ds                  extname(AZUTE00F)
005000150305     d                                     dtaara
005100150305     d §DatiUte      e ds                  extname(dDatiUte)
005200150305     d                                     dtaara
005300150305
005400150305       //--------------------------------------------------------------
005500150305       //?Definizione strutture dati.                                  ?
005600150305       //--------------------------------------------------------------
005700150305
005800150305       // -?Status?
005900150305     d Status         sds
006000150305     d   SDSpgm          *proc
006100150305     d*//SDSprm          *parms
006200150305     d*//SDSdta              191    198
006300150305     d   SDSjobName          244    253
006400150305     d   SDSjobUser          254    263
006500150305     d   SDSjobNbr           264    269s 0
006600150305
006700150305       // -?Parametri ricevuti?
006800150305     d KPJBA         e ds
006900150306
007000150306       // -?Descrizione 143 dell'Organigramma?
007100150306     d Og143         e ds                  inz   qualified
007200150305
007300150305      //---------------------------------------------------------------
007400150305      //?Definizione variabili globali.
007500150305      //---------------------------------------------------------------
007600150306
007700150306       // -?Indici di schiera?
007800150306     d xP              s              3  0 inz
007900150305
008000150305       // -?Codice errore rilevato?
008100150306     d wErr            s                   inz   like(CAAcdm)
008200150306
008300150306       // -?Filiale reperita dall'IP address?
008400150306     d wIPfil          s                   inz   like(ORGfil)
008500150306
008600150306       // -?Funzione Aziendale (Abilitazioni Utente)?
008700150306     d wABI            s                   inz   like(uteAUT)
008800150305
008900150305       //--------------------------------------------------------------
009000150305       //?Definizione prototipi/procedure usate.                       ?
009100150305       //--------------------------------------------------------------
009200150305
009300150306       // -?Ricerca/Controllo tabelle (TNTBE)?
009400150306     d TIBS02ds      e ds                  inz
009500150306     d   T02mod      e                     inz('C')
009600150306     d   T02cod      e                     inz('LAT')
009700150306      /copy gaitrasrc/srcProtoPR,TIBS02R
009800150306       // -?Tabella "LAT" = Sigla Funzione Aziendale (Livello Autorità)?
009900150306     d dLAT          e ds                  inz
010000150306
010100150305       // -?Reperimento dati utente?
010200150305     d TIBS34ds      e ds                  inz
010300150305      /copy gaitrasrc/srcProtoPR,TIBS34R
010400150306       // -?Flags Abilitazioni utenti aziendali?
010500150306     d dUte01        e ds                  inz   qualified
010600150305
010700150305       // -?Caricamento filiali gestite dall'utente?
010800150305     d TRUL31ds      e ds                  inz
010900150305     d   sk_POg               10    759    inz   dim(250)
011000150305      /copy gaitrasrc/srcProtoPR,TRUL31R
011100150309
011200150309       // -?API Retrieve Device Description (QDCRDEVD)?
011300150309      /copy QSYSINC/QRPGLESRC,QDCRDEVD
011400150309      /copy QSYSINC/QRPGLESRC,QUSEC
011500150309     d RtvDevD         pr                  extpgm('QDCRDEVD')
011600150309     d   outReceiver                938a   const
011700150309     d   inLength                    10i 0 const
011800150309     d   inFormatName                 8a   const
011900150309     d   inDeviceName                10a   const
012000150309     d   ioErrorCode                 17a   const
012100150305
012200150305       //--------------------------------------------------------------
012300150305       //?Definizione key-list.                                        ?
012400150305       //--------------------------------------------------------------
012500150305
012600150305
012700150305       //--------------------------------------------------------------
012800150305       //?Riepilogo indicatori utilizzati.                             ?
012900150305       //--------------------------------------------------------------
013000150306
013100150305
013200150305       //--------------------------------------------------------------
013300150305       //?M A I N - L I N E                                            ?
013400150305       //--------------------------------------------------------------
013500150310       //?Evidenziata:                                                 ?
013600150310       // -? Uscita dal *pgm SENZA errori                       ?
013700150310       // -? Uscita dal *pgm CON errore registrato in AZCAA     ?
013800150310       // -? Uscita dal *pgm CON errore NON registrato in AZCAA ?
013900150310       //--------------------------------------------------------------
014000150305
014100150305      /free
014200150305
014300150305       // -?Operazioni iniziali?
014400150305       exsr  sr_RoutInz;
014500150305
014600150306       // -?Controllo indirizzo IP del terminale?
014700150305       exsr  sr_Chk_IPaddr;
014800150306
014900150306       // -?Controllo filiale utente?
015000150306       exsr  sr_Chk_dutPOU;
015100150305
015200150305       // -?Operazioni finali?
015300150305       exsr sr_RoutEnd;
015400150305
015500150305       //--------------------------------------------------------------
015600150305       //?Operazioni iniziali.                                         ?
015700150305       //--------------------------------------------------------------
015800150305       BEGSR  sr_RoutInz;
015900150305
016000150305         *inLR = *on;
016100150505
016200150511         // -?Impostazione dati nella KPJBA?
016300150511         exsr  sr_Set_KPJBA;
016400150305
016500150305         // -?Reperimento dati job?
016600150305         exsr  sr_DatiJob;
016700150310
016800150310         // -?SE rilevati errori nei dati utente => uscita dal *pgm?
016900150305         if  DUTerr = 'E';
017000150310           //wErr = 1;
017100150310           //exsr  sr_WriteLOG;
017200150306           exsr  sr_RoutEnd;
017300150305         endif;
017400150305
017500150305         // -?Caricamento filiali gestibili dall'utente?
017600150305         clear  TRUL31ds;
017700150305         I31abi = UTEaut;
017800150305         I31cdi = DUTdis;
017900150305         I31car = DUTare;
018000150305         I31cpo = DUTpou;
018100150305         trul31r ( KPJBA : TRUL31ds );
018200150305         //if  O31pog <= *zero;
018300150305         //  ...;
018400150305         //endif;
018500150305
018600150305       ENDSR;
018700150505
018800150505       //--------------------------------------------------------------
018900150511       //?Impostazione dati in KPJBA.                                  ?
019000150505       //--------------------------------------------------------------
019100150511       BEGSR  sr_Set_KPJBA;
019200150505
019300150511         //// -?Reperimento dati del profilo utente?
019400150511         //chain  ( SDSjobUser )  KPPRF000;
019500150511         //if  NOT  %found(KPPRF01L);
019600150511         //  exsr  sr_RoutEnd;
019700150511         //endif;
019800150505
019900150505         // -?Impostazione dati nella KPJBA?
020000150505         clear  KPJBA;
020100150506         kNmUs = SDSjobUser;   // -?NOME DEL PROFILO UTENTE?
020200150506         kNmTd = SDSjobName;   // -?NOME DEL TERMINALE?
020300150506         kNrJo = SDSjobNbr;    // -?NUMERO "JOB" ASSEGNATO DAL SISTEMA?
020400150506         kNmEb = SDSjobName;   // -?NOME JOB?
020500150511         //kNSIf = PRFnsi;       // -?NOME SISTEMA INFORMATIVO?
020600150511         kNSIf = 'GAITRA201 '; // -?NOME SISTEMA INFORMATIVO?
020700150506         kTpWs = 'N';          // -?FLAG DI TIPO TERMINALE?
020800150505
020900150505       ENDSR;
021000150305
021100150305       //--------------------------------------------------------------
021200150305       //?Reperimento Dati del job (Utente/Operativi).                 ?
021300150305       //--------------------------------------------------------------
021400150305       BEGSR  sr_DatiJob;
021500150305
021600150305         in(e) §AzUte;
021700150305         if NOT %error;
021800150305           in(e) §DatiUte;
021900150305         endif;
022000150305         if %error or RSut = *blank;
022100150305           tibs34r ( tibs34ds );
022200150305           in §AzUte;
022300150305           in §DatiUte;
022400150305         endif;
022500150305
022600150305       ENDSR;
022700150305
022800150305       //--------------------------------------------------------------
022900150305       //?Controllo Indirizzo IP del terminale.                        ?
023000150305       //--------------------------------------------------------------
023100150305       BEGSR  sr_Chk_IPaddr;
023200150305
023300150306         // -?Reperimento indirizzo IP del terminale?
023400150309         clear  QUSEC;
023500150309         callp(E)  RtvDevD ( QDCD060000 :
023600150309                             938 :
023700150309                             'DEVD0600' :
023800150309                             SDSjobName :
023900150309                             QUSEC );
024000150309
024100150309         // -?Verifica se IP errato:?
024200150309         clear  wErr;
024300150309         Select;
024400150309
024500150310           // -?Errore nell'esecuzione dell'API?
024600150309           When  %error();
024700150310             wErr = 2;
024800150309             exsr  sr_WriteLOG;
024900150309             exsr  sr_RoutEnd;
025000150309
025100150310           // -?Errore nel reperimento indirizzo IP del terminale?
025200150310           //  ?(vedi campo QUSEC.QUSBAVL)?
025300150309           When  QUSBAVL > *zero;
025400150310             wErr = 3;
025500150309             exsr  sr_WriteLOG;
025600150309             exsr  sr_RoutEnd;
025700150309
025800150310           // -?Network Protocol NON di tipo IP (terminale)?
025900150310           //  ?(vedi campo QDSD060000.QDCNP)?
026000150310           //  ?· x'02' = Internet Protocol            (IPv4)?
026100150310           //  ?· x'06' = Internetwork Packet Exchange (IPX)?
026200150310           //  ?· x'18' = Internet Protocol versione 6 (IPv6)?
026300150310           //When  QDCNP <> x'02';
026400150310           //  wErr = 4;
026500150310           //  exsr  sr_WriteLOG;
026600150310           //  exsr  sr_RoutEnd;
026700150309
026800150310           // ·?IP non inizia per "10."?
026900150310           //  ?(vedi campo QDSD060000.QDCIPADF)?
027000150309           When  %subst( QDCIPADF : 1 : 3 ) <> '10.';
027100150310             wErr = 5;
027200150309             exsr  sr_WriteLOG;
027300150309             exsr  sr_RoutEnd;
027400150309
027500150309         EndSl;
027600150309
027700150310         // ·?2° "." ("10.XXX.") inesistente o oltre la 7ª posizione?
027800150310         //  ?(vedi campo QDSD060000.QDCIPADF)?
027900150309         xP = %scan( '.' : QDCIPADF : 4 );
028000150306         if  xP = *zero  or  xP = 4  or  xP > 7;
028100150310           wErr = 6;
028200150306           exsr  sr_WriteLOG;
028300150306           exsr  sr_RoutEnd;
028400150306         endif;
028500150309
028600150310         // ·?Filiale ("10.XXX.") non numerica => uscita dal *pgm?
028700150310         //  ?(vedi campo QDSD060000.QDCIPADF)?
028800150306         Monitor;
028900150309           wIPfil = %int( %subst( QDCIPADF : 4 : ( xP - 4 ) ) );
029000150306           On-Error;
029100150310             //wErr = 7;
029200150306             //exsr  sr_WriteLOG;
029300150306             exsr  sr_RoutEnd;
029400150306         EndMon;
029500150306
029600150306       ENDSR;
029700150306
029800150306       //--------------------------------------------------------------
029900150306       //?Controllo Filiale dell'utente.                               ?
030000150306       //--------------------------------------------------------------
030100150306       BEGSR  sr_Chk_dutPOU;
030200150305
030300150306         // -?Controllo corrispondenza diretta IP Terminale / Fil. Utente?
030400150310         //  ?SE filiale dell'utente non trovata => uscita dal *pgm?
030500150306         chain  ( DUTpou )  AZORG;
030600150306         if  NOT  %found(AZORG01L);
030700150310           //wErr = 8;
030800150310           //exsr  sr_WriteLOG;
030900150306           exsr  sr_RoutEnd;
031000150306         endif;
031100150306
031200150310         // -?IP terminale ed IP filiale corrispondenti: OK?
031300150306         //  ?(se §OGIIP = *blank => si utilizza il codice filiale)?
031400150306         Og143 = orgDE3;
031500150306         if  wIPfil = dutPOU  or  %editc( wIPfil : 'X' ) = Og143.§ogIIP;
031600150306           exsr  sr_RoutEnd;
031700150306         endif;
031800150305
031900150306         // -?Verifica se Utente abilitato alla Filiale del Terminale?
032000150306         //  ?Verifica abilitazione in tabella "LAT"?
032100150306         dUte01 = uteFAF;
032200150306         if  dUte01.§uteIPT <> *blank;
032300150306           // -?Con AUTORITÀ SPECIFICA?
032400150306           wABI = dUte01.§uteIPT;
032500150306         else;
032600150306           // -?Con AUTORITÀ GENERICA?
032700150306           wABI = uteAUT;
032800150306         endif;
032900150305
033000150306         reset  TIBS02ds;
033100150306         //T02mod = 'C';       ? (già così)?
033200150306         //T02cod = 'LAT';      ?(già così)?
033300150306         T02ke1 = wABI;
033400150306         T02sif = knsif;
033500150306
033600150306         TNTBE_RicercaControllo ( kpjba : TIBS02ds );
033700150306
033800150306         if  T02err = *blank;
033900150306           dLAT = T02uni;
034000150306         endif;
034100150305
034200150310         // -?Utente SENZA abilitazione?
034300150306         if  T02err <> *blank  or  §LATabi = 'S';
034400150310           wErr = 9;
034500150306           exsr  sr_WriteLOG;
034600150306           exsr  sr_RoutEnd;
034700150306         endif;
034800150306
034900150310         // -?Utente con abilitazione "AZ" => nessun controllo (OK)?
035000150306         if  wABI = 'AZ';
035100150306           exsr  sr_RoutEnd;
035200150306         endif;
035300150306
035400150306         // -?Reperimento Filiali a cui l'utente è abilitato?
035500150306         clear  sk_POg;
035600150306         clear  TRUL31ds;
035700150306         I31abi = wABI;
035800150306         I31cdi = dutDIS;
035900150306         I31car = dutARE;
036000150306         I31cpo = dutPOU;
036100150306
036200150306         trul31r ( kpjba : TRUL31ds );
036300150305
036400150306         // -?Controllo se l'IP del device è tra quelli delle filiali?
036500150306         //  ?a cui è abilitato l'utente; per farlo reperisco in AZORG?
036600150306         //  ?le filiali con indirizzo IP uguale al device e verifico.?
036700150306         //  ?N.B.?Esistono filiali con IP "comune".?
036800150306         //  ?Al primo esco, se non trovo scrivo LOG.?
036900150306
037000150310         // -?L'indirizzo IP del device è di una delle filiali a cui è?
037100150310         //  ?abilitato l'utente => uscita (OK)?
037200150306         if  %lookup( %editc( wIPfil : 'X' ) : sk_POg ) > *zero;
037300150306           exsr  sr_RoutEnd;
037400150306         endif;
037500150306
037600150306         // -?Verifica se l'indirizzo IP del device è comunque previsto?
037700150306         //  ?in una delle filiali a cui è abilitato l'utente?
037800150306         clear  Og143;
037900150306         For  xP = 1  to  %elem( sk_POg );
038000150306
038100150306           if  sk_POg(xP) <= *zero;
038200150306             leave;
038300150306           endif;
038400150306
038500150306           // -?Per farlo: verifica in AZORG se almeno una delle fil.?
038600150306           //  ?a cui è abilitato l'utente  ha un indirizzo IP uguale?
038700150306           //  ?a quello del device?
038800150310           If  sk_POg(xP) <> %editc( DUTpou : 'X' );
038900150310             chain  ( %int( sk_POg(xP) ) )  AZORG;
039000150310             if  %found(AZORG01L);
039100150310               Og143 = orgDE3;
039200150310               // -?Al primo indirizzo IP uguale => uscita (OK)?
039300150310               if  Og143.§ogIIP = %editc( wIPfil : 'X' );
039400150310                 exsr  sr_RoutEnd;
039500150310               endif;
039600150310             endif;
039700150310           EndIf;
039800150306
039900150306         EndFor;
040000150306
040100150310         // -?Utente non abilitato alla filiale reperita dall'IP device?
040200150310         wErr = 10;
040300150306         exsr  sr_WriteLOG;
040400150306         exsr  sr_RoutEnd;
040500150305
040600150306       ENDSR;
040700150306
040800150306       //--------------------------------------------------------------
040900150306       //?Controllo Indirizzo IP del terminale.                        ?
041000150306       //--------------------------------------------------------------
041100150306       BEGSR  sr_WriteLOG;
041200150305
041300150306         clear  AZCAA000;
041400150306
041500150309         CAAute = kNmUs;
041600150306         CAAdev = SDSjobName;
041700150309         CAAip  = QDCIPADF;
041800150306         CAAtim = %int( %subst( %char( %dec( %timestamp() ) ) :1 :14 ) );
041900150306         CAAcdm = wErr;
042000150309
042100150310         evalR CAAtpm = sk_Msg( wErr );
042200150309
042300150309         CAAnot = sk_Msg( wErr );
042400150309
042500150310         if  wErr = 3;
042600150309           CAAnot = %trimR( CAAnot ) + ' ' + %trim( QUSEI );
042700150309         endif;
042800150306
042900150306         //______________
043000150306         write  AZCAA000;
043100150306         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
043200150306
043300150306       ENDSR;
043400150306
043500150306       //--------------------------------------------------------------
043600150306       //?Operazioni finali.                                         ?
043700150306       //--------------------------------------------------------------
043800150306       BEGSR  sr_RoutEnd;
043900150306
044000150306         // -?Chiusura applicazioni precedentemente aperte?
044100150306         if  T02ke1 <> *blank;
044200150306           clear  TIBS02ds;
044300150306           T02tla = 'C';
044400150306           TNTBE_RicercaControllo ( kpjba : TIBS02ds );
044500150306         endif;
044600150306
044700150306         // -?Chiusura *pgm?
044800150306         return;
044900150306
045000150306       ENDSR;
045100150309
045200150309      /end-free
045300150309
045400150309       //--------------------------------------------------------------
045500150309       //?Schiere a tempo di compilazione.                             ?
045600150309       //--------------------------------------------------------------
045700150310** --?sk_Msg:?Messaggi di Errore ------------------------------------* Tipo Progr?
045800150310Dati dell'utente non reperibili                                       |   0|-- 1 NON usato
045900150505Errore nell'esecuzione dell'API?QDCRDEVD (Retrieve Device Description)|   1|   2
046000150505Errore nel reperimento dell'indirizzo IP del dispositivo              |   1|   3
046100150310Dispositivo NON di tipo IP (terminale)                                |   1|-- 4 NON usato
046200150310Indirizzo IP errato: non inizia con "10."                             |   1|   5
046300150506Indirizzo IP errato: non trovato carattere "." nella 7ª posizione     |   1|   6
046400150505Codice Filiale non numerico                                           |   0|-- 7 NON usato
046500150505Filiale utente non trovata in anagrafica                              |   0|-- 8 NON usato
046600150505Utente non abilitato alla Filiale del dispositivo                     |   1|   9
046700150505Utente non abilitato alla Filiale reperita dall'IP del dispositivo    |   1|  10
