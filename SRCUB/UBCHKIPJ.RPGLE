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
017500150305       ENDSR;
017600150505
017700150505       //--------------------------------------------------------------
017800150511       //?Impostazione dati in KPJBA.                                  ?
017900150505       //--------------------------------------------------------------
018000150511       BEGSR  sr_Set_KPJBA;
018100150505
018200150511         //// -?Reperimento dati del profilo utente?
018300150511         //chain  ( SDSjobUser )  KPPRF000;
018400150511         //if  NOT  %found(KPPRF01L);
018500150511         //  exsr  sr_RoutEnd;
018600150511         //endif;
018700150505
018800150505         // -?Impostazione dati nella KPJBA?
018900150505         clear  KPJBA;
019000150506         kNmUs = SDSjobUser;   // -?NOME DEL PROFILO UTENTE?
019100150506         kNmTd = SDSjobName;   // -?NOME DEL TERMINALE?
019200150506         kNrJo = SDSjobNbr;    // -?NUMERO "JOB" ASSEGNATO DAL SISTEMA?
019300150506         kNmEb = SDSjobName;   // -?NOME JOB?
019400150511         //kNSIf = PRFnsi;       // -?NOME SISTEMA INFORMATIVO?
019500150511         kNSIf = 'GAITRA201 '; // -?NOME SISTEMA INFORMATIVO?
019600150506         kTpWs = 'N';          // -?FLAG DI TIPO TERMINALE?
019700150505
019800150505       ENDSR;
019900150305
020000150305       //--------------------------------------------------------------
020100150305       //?Reperimento Dati del job (Utente/Operativi).                 ?
020200150305       //--------------------------------------------------------------
020300150305       BEGSR  sr_DatiJob;
020400150305
020500150305         in(e) §AzUte;
020600150305         if NOT %error;
020700150305           in(e) §DatiUte;
020800150305         endif;
020900150305         if %error or RSut = *blank;
021000150305           tibs34r ( tibs34ds );
021100150305           in §AzUte;
021200150305           in §DatiUte;
021300150305         endif;
021400150305
021500150305       ENDSR;
021600150305
021700150305       //--------------------------------------------------------------
021800150305       //?Controllo Indirizzo IP del terminale.                        ?
021900150305       //--------------------------------------------------------------
022000150305       BEGSR  sr_Chk_IPaddr;
022100150305
022200150306         // -?Reperimento indirizzo IP del terminale?
022300150309         clear  QUSEC;
022400150309         callp(E)  RtvDevD ( QDCD060000 :
022500150309                             938 :
022600150309                             'DEVD0600' :
022700150309                             SDSjobName :
022800150309                             QUSEC );
022900150309
023000150309         // -?Verifica se IP errato:?
023100150309         clear  wErr;
023200150309         Select;
023300150309
023400150310           // -?Errore nell'esecuzione dell'API?
023500150309           When  %error();
023600150310             wErr = 2;
023700150309             exsr  sr_WriteLOG;
023800150309             exsr  sr_RoutEnd;
023900150309
024000150310           // -?Errore nel reperimento indirizzo IP del terminale?
024100150310           //  ?(vedi campo QUSEC.QUSBAVL)?
024200150309           When  QUSBAVL > *zero;
024300150310             wErr = 3;
024400150309             exsr  sr_WriteLOG;
024500150309             exsr  sr_RoutEnd;
024600150309
024700150310           // -?Network Protocol NON di tipo IP (terminale)?
024800150310           //  ?(vedi campo QDSD060000.QDCNP)?
024900150310           //  ?· x'02' = Internet Protocol            (IPv4)?
025000150310           //  ?· x'06' = Internetwork Packet Exchange (IPX)?
025100150310           //  ?· x'18' = Internet Protocol versione 6 (IPv6)?
025200150310           //When  QDCNP <> x'02';
025300150310           //  wErr = 4;
025400150310           //  exsr  sr_WriteLOG;
025500150310           //  exsr  sr_RoutEnd;
025600150309
025700150310           // ·?IP non inizia per "10."?
025800150310           //  ?(vedi campo QDSD060000.QDCIPADF)?
025900150309           When  %subst( QDCIPADF : 1 : 3 ) <> '10.';
026000150310             wErr = 5;
026100150309             exsr  sr_WriteLOG;
026200150309             exsr  sr_RoutEnd;
026300150309
026400150309         EndSl;
026500150309
026600150310         // ·?2° "." ("10.XXX.") inesistente o oltre la 7ª posizione?
026700150310         //  ?(vedi campo QDSD060000.QDCIPADF)?
026800150309         xP = %scan( '.' : QDCIPADF : 4 );
026900150306         if  xP = *zero  or  xP = 4  or  xP > 7;
027000150310           wErr = 6;
027100150306           exsr  sr_WriteLOG;
027200150306           exsr  sr_RoutEnd;
027300150306         endif;
027400150309
027500150310         // ·?Filiale ("10.XXX.") non numerica => uscita dal *pgm?
027600150310         //  ?(vedi campo QDSD060000.QDCIPADF)?
027700150306         Monitor;
027800150309           wIPfil = %int( %subst( QDCIPADF : 4 : ( xP - 4 ) ) );
027900150306           On-Error;
028000150310             //wErr = 7;
028100150306             //exsr  sr_WriteLOG;
028200150306             exsr  sr_RoutEnd;
028300150306         EndMon;
028400150306
028500150306       ENDSR;
028600150306
028700150306       //--------------------------------------------------------------
028800150306       //?Controllo Filiale dell'utente.                               ?
028900150306       //--------------------------------------------------------------
029000150306       BEGSR  sr_Chk_dutPOU;
029100150619
029200150619         // -?Valorizzazione "Flags Abilitazioni utenti aziendali"?
029300150619         dUte01 = uteFAF;
029400150305
029500150306         // -?Controllo corrispondenza diretta IP Terminale / Fil. Utente?
029600150310         //  ?SE filiale dell'utente non trovata => uscita dal *pgm?
029700150306         chain  ( DUTpou )  AZORG;
029800150306         if  NOT  %found(AZORG01L);
029900150310           //wErr = 8;
030000150310           //exsr  sr_WriteLOG;
030100150306           exsr  sr_RoutEnd;
030200150306         endif;
030300150306
030400150310         // -?IP terminale ed IP filiale corrispondenti: OK?
030500150306         //  ?(se §OGIIP = *blank => si utilizza il codice filiale)?
030600150306         Og143 = orgDE3;
030700150306         if  wIPfil = dutPOU  or  %editc( wIPfil : 'X' ) = Og143.§ogIIP;
030800150306           exsr  sr_RoutEnd;
030900150306         endif;
031000150619
031100150619         // -?Verifica se prevista "Filiale Ufficio" (in AZUTE00F)?
031200150623         if  %editc( wIPfil : 'X' ) = dUte01.§uteFUF;
031300150619           leavesr;
031400150619         endif;
031500150305
031600150306         // -?Verifica se Utente abilitato alla Filiale del Terminale?
031700150306         //  ?Verifica abilitazione in tabella "LAT"?
031800150306         if  dUte01.§uteIPT <> *blank;
031900150306           // -?Con AUTORITÀ SPECIFICA?
032000150306           wABI = dUte01.§uteIPT;
032100150306         else;
032200150306           // -?Con AUTORITÀ GENERICA?
032300150306           wABI = uteAUT;
032400150306         endif;
032500150305
032600150306         reset  TIBS02ds;
032700150306         //T02mod = 'C';       ? (già così)?
032800150306         //T02cod = 'LAT';      ?(già così)?
032900150306         T02ke1 = wABI;
033000150306         T02sif = knsif;
033100150306
033200150306         TNTBE_RicercaControllo ( kpjba : TIBS02ds );
033300150306
033400150306         if  T02err = *blank;
033500150306           dLAT = T02uni;
033600150306         endif;
033700150305
033800150310         // -?Utente SENZA abilitazione?
033900150306         if  T02err <> *blank  or  §LATabi = 'S';
034000150310           wErr = 9;
034100150306           exsr  sr_WriteLOG;
034200150306           exsr  sr_RoutEnd;
034300150306         endif;
034400150306
034500150310         // -?Utente con abilitazione "AZ" => nessun controllo (OK)?
034600150306         if  wABI = 'AZ';
034700150306           exsr  sr_RoutEnd;
034800150306         endif;
034900150306
035000150306         // -?Reperimento Filiali a cui l'utente è abilitato?
035100150306         clear  sk_POg;
035200150306         clear  TRUL31ds;
035300150306         I31abi = wABI;
035400150626         //I31cdi = dutDIS;
035500150626         //I31car = dutARE;
035600150626         if  dUte01.§uteFUF <= *zero;
035700150626           I31cpo = dutPOU;
035800150626         else;
035900150626           Monitor;
036000150626             I31cpo = %int( dUte01.§uteFUF );
036100150626             On-Error;
036200150626               I31cpo = dutPOU;
036300150626           EndMon;
036400150626         endif;
036500150306
036600150306         trul31r ( kpjba : TRUL31ds );
036700150305
036800150306         // -?Controllo se l'IP del device è tra quelli delle filiali?
036900150306         //  ?a cui è abilitato l'utente; per farlo reperisco in AZORG?
037000150306         //  ?le filiali con indirizzo IP uguale al device e verifico.?
037100150306         //  ?N.B.?Esistono filiali con IP "comune".?
037200150306         //  ?Al primo esco, se non trovo scrivo LOG.?
037300150306
037400150310         // -?L'indirizzo IP del device è di una delle filiali a cui è?
037500150310         //  ?abilitato l'utente => uscita (OK)?
037600150306         if  %lookup( %editc( wIPfil : 'X' ) : sk_POg ) > *zero;
037700150306           exsr  sr_RoutEnd;
037800150306         endif;
037900150306
038000150306         // -?Verifica se l'indirizzo IP del device è comunque previsto?
038100150306         //  ?in una delle filiali a cui è abilitato l'utente?
038200150306         clear  Og143;
038300150306         For  xP = 1  to  %elem( sk_POg );
038400150306
038500150306           if  sk_POg(xP) <= *zero;
038600150306             leave;
038700150306           endif;
038800150306
038900150306           // -?Per farlo: verifica in AZORG se almeno una delle fil.?
039000150306           //  ?a cui è abilitato l'utente  ha un indirizzo IP uguale?
039100150306           //  ?a quello del device?
039200150310           If  sk_POg(xP) <> %editc( DUTpou : 'X' );
039300150310             chain  ( %int( sk_POg(xP) ) )  AZORG;
039400150310             if  %found(AZORG01L);
039500150310               Og143 = orgDE3;
039600150310               // -?Al primo indirizzo IP uguale => uscita (OK)?
039700150310               if  Og143.§ogIIP = %editc( wIPfil : 'X' );
039800150310                 exsr  sr_RoutEnd;
039900150310               endif;
040000150310             endif;
040100150310           EndIf;
040200150306
040300150306         EndFor;
040400150306
040500150310         // -?Utente non abilitato alla filiale reperita dall'IP device?
040600150310         wErr = 10;
040700150306         exsr  sr_WriteLOG;
040800150306         exsr  sr_RoutEnd;
040900150305
041000150306       ENDSR;
041100150306
041200150306       //--------------------------------------------------------------
041300150306       //?Controllo Indirizzo IP del terminale.                        ?
041400150306       //--------------------------------------------------------------
041500150306       BEGSR  sr_WriteLOG;
041600150305
041700150306         clear  AZCAA000;
041800150306
041900150309         CAAute = kNmUs;
042000150306         CAAdev = SDSjobName;
042100150309         CAAip  = QDCIPADF;
042200150306         CAAtim = %int( %subst( %char( %dec( %timestamp() ) ) :1 :14 ) );
042300150306         CAAcdm = wErr;
042400150309
042500150310         evalR CAAtpm = sk_Msg( wErr );
042600150309
042700150309         CAAnot = sk_Msg( wErr );
042800150309
042900150310         if  wErr = 3;
043000150309           CAAnot = %trimR( CAAnot ) + ' ' + %trim( QUSEI );
043100150309         endif;
043200150306
043300150306         //______________
043400150306         write  AZCAA000;
043500150306         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
043600150306
043700150306       ENDSR;
043800150306
043900150306       //--------------------------------------------------------------
044000150306       //?Operazioni finali.                                         ?
044100150306       //--------------------------------------------------------------
044200150306       BEGSR  sr_RoutEnd;
044300150306
044400150306         // -?Chiusura applicazioni precedentemente aperte?
044500150306         if  T02ke1 <> *blank;
044600150306           clear  TIBS02ds;
044700150306           T02tla = 'C';
044800150306           TNTBE_RicercaControllo ( kpjba : TIBS02ds );
044900150306         endif;
045000150306
045100150306         // -?Chiusura *pgm?
045200150306         return;
045300150306
045400150306       ENDSR;
045500150309
045600150309      /end-free
045700150309
045800150309       //--------------------------------------------------------------
045900150309       //?Schiere a tempo di compilazione.                             ?
046000150309       //--------------------------------------------------------------
046100150310** --?sk_Msg:?Messaggi di Errore ------------------------------------* Tipo Progr?
046200150310Dati dell'utente non reperibili                                       |   0|-- 1 NON usato
046300150505Errore nell'esecuzione dell'API?QDCRDEVD (Retrieve Device Description)|   1|   2
046400150505Errore nel reperimento dell'indirizzo IP del dispositivo              |   1|   3
046500150310Dispositivo NON di tipo IP (terminale)                                |   1|-- 4 NON usato
046600150310Indirizzo IP errato: non inizia con "10."                             |   1|   5
046700150506Indirizzo IP errato: non trovato carattere "." nella 7ª posizione     |   1|   6
046800150505Codice Filiale non numerico                                           |   0|-- 7 NON usato
046900150505Filiale utente non trovata in anagrafica                              |   0|-- 8 NON usato
047000150505Utente non abilitato alla Filiale del dispositivo                     |   1|   9
047100150505Utente non abilitato alla Filiale reperita dall'IP del dispositivo    |   1|  10
