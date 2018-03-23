000100080206      //--------------------------------------------------------------
000200100503      //?TNTA55R - ESITAZIONE OFFERTE COMMERCIALI
000300080206      //--------------------------------------------------------------
000400080206
000500090407     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600090601     h dftactgrp(*no) actgrp(*caller)
000700080206
000800080206      //---------------------------------------------------------------
000900080206      //?Dichiarazione file.
001000080206      //---------------------------------------------------------------
001100050704
001200091207      // - Anagrafica Clienti
001300090810     fCNACO00F  uf a e           k disk
001400091207      // - Anagrafica Clienti
001500091207     fCNIND00F  uf a e           k disk
001600091207      // - Anagrafica Clienti
001700091207     fCNCLP00F  uf a e           k disk
001800091207      // - Anagrafica Clienti
001900091207     fFNCLS01L  uf a e           k disk
002000100915      // - Anagrafica Clienti
002100100915     fFNCBA01L  uf a e           k disk
002200100119      // - Anagrafica Provvisoria Trattative
002300091207     fTFACO00F  uf   e           k disk    rename(cnaco000:tfaco000)
002400100119      // - Anagrafica Provvisoria Trattative
002500091207     fTFIND00F  uf   e           k disk    rename(cnind000:tfind000)
002600100119      // - Anagrafica Provvisoria Trattative
002700091207     fTFCLP00F  uf   e           k disk    rename(cnclp000:tfclp000)
002800100119      // - Anagrafica Provvisoria Trattative
002900100119     fTFCLS01L  uf a e           k disk    rename(fncls000:tfcls000)
003000100915      // - Anagrafica Provvisoria Trattative
003100100915     fTFCBA01L  uf a e           k disk    rename(fncba000:tfcba000)
003200100503
003300100119      // - Note
003400100424     fTFNTC01L  uf a e           k disk
003500100916      // - Luoghi
003600100916     fFNSPE00F  o  a e             disk
003700100916      // - Luoghi 2
003800100916     fFNSP201L  if a e           k disk
003900100504      // - Tabelle
004000100504     fTABEL00F  if   e           k disk
004100080206
004200080206      //---------------------------------------------------------------
004300090406      //?Definizione costanti.
004400080206      //---------------------------------------------------------------
004500050519
004600080206      //---------------------------------------------------------------
004700080206      //?Definizione schiere.
004800080206      //---------------------------------------------------------------
004900100120
005000100504      // Schiera tipo Nota
005100100504     d tno             s              2    dim(50)
005200100504     d tdup            s              1    dim(50)
005300100504     d ttdp            s              1    dim(50)
005400100916
005500100916      // Schiera tipo luogo
005600100916     d tluo            s              3    dim(50)
005700100916     d tufi            s              1    dim(50)
005800100504     d
005900080206      //---------------------------------------------------------------
006000080206      //?Definizione aree dati.
006100080206      //---------------------------------------------------------------
006200080206
006300080206      // - Dati utente
006400080206     d §AzUte        e ds                  extname(AZUTE00F)
006500080206     d                                     dtaara
006600080206     d §DatiUte      e ds                  extname(dDatiUte)
006700080206     d                                     dtaara
006800080206
006900080206      //---------------------------------------------------------------
007000080206      //?Definizione strutture dati.
007100080206      //---------------------------------------------------------------
007200080206
007300080206      // - Status
007400080206     d Psds           sds
007500100122     d   SDSpgm          *proc
007600100122     d   SDSjob              244    253
007700100122     d   SDSuse              254    263
007800100122
007900080206      // - Parametri ricevuti
008000080206     d KPJBA         e ds
008100100503     d TNTA55DS      e ds
008200080206
008300080206      // - Reperimento dati utente
008400080206     d TIBS34ds      e ds
008500080206     d dUte01        e ds
008600081020     d TRUL31DS      e ds
008700081020     d POG                    10    759    DIM(250)
008800100312
008900100122      // - Tabella 1T = Tipi note
009000100122     d ds1t          e ds                  inz
009100090317
009200100916      // - Tabella 4L = Luoghi gestiti
009300100916     d ds4l          e ds
009400170307
009500170307      // - Tabella 4W  = Stato del credito
009600170307     d ds4W          e ds                  inz
009700100916
009800100119       // - Reperimento dati anagrafici
009900100119     d TIBS69ds      e ds
010000100504     d DS_cnaco      e ds                  inz extname(CNACO00F) prefix(w_)
010100100504     d DS_cnind      e ds                  inz extname(CNIND00F) prefix(w_)
010200100504     d DS_cnclp      e ds                  inz extname(CNCLP00F) prefix(w_)
010300100504     d DS_fncls      e ds                  inz extname(FNCLS00F) prefix(w_)
010400100119
010500100121       // - Creazione variazioni dati anagrafici
010600100121     d TIBS73ds      e ds
010700100121     d cnaco_73      e ds                  inz extname(CNACO00F)
010800100121     d cnind_73      e ds                  inz extname(CNIND00F)
010900100121     d cnclp_73      e ds                  inz extname(CNCLP00F)
011000100121     d fncls_73      e ds                  inz extname(FNCLS00F)
011100100121
011200100916      // - Per creazione o delle note trattative da copiare
011300100916     d TrulIbanI0    e ds                  qualified
011400100916     d TrulIbanO0    e ds                  qualified
011500100916
011600100425      // - Per il recupero delle note trattative da copiare
011700100425     d tfntcDs       e ds                  ExtName(tfntc00f)
011800100425
011900100916      // - Per il recupero dei luoghi da copiare
012000100916     d fnspeDs       e ds                  ExtName(fnspe00f)
012100080206      //---------------------------------------------------------------
012200080206      //?Definizione variabili globali.
012300080206      //---------------------------------------------------------------
012400080206
012500080206      // - Flags booleani
012600080208     d $Fine           s               n   inz(*off)
012700080207     d $EoF            s               n   inz(*off)
012800080208     d $RecOK          s               n   inz(*off)
012900080206
013000080206      // - Indici di schiera
013100080208     d xx              s              3  0 inz
013200080206
013300140605      // - Campi di salvataggio
013400140605     d sav_ACOabl      s                   like(ACOabl)
013500140605     d sav_INDopz      s                   like(INDopz)
013600140605     d sav_INDcdp      s                   like(INDcdp)
013700170307     d sav_INDnpr      s                   like(INDnpr)
013800170307     d sav_INDdpr      s                   like(INDdpr)
013900170307     d sav_INDnpn      s                   like(INDnpn)
014000140605     d sav_CLPnar      s                   like(CLPnar)
014100140605     d sav_CLPcon      s                   like(CLPcon)
014200140605     d sav_CLPconP     s                   like(CLPcon)
014300080206
014400090401      // - Campi di comodo data
014500090401     d  data_eur       s               d   Datfmt(*eur)
014600090401     d  data_iso       s               d   Datfmt(*iso)
014700090401     d  data_dmy       s               d   Datfmt(*dmy)
014800090811     d  data_ymd       s               d   datfmt(*ymd)
014900090401
015000080206      // - Campi di comodo
015100090406     d wAbi            s                   like(UTEaut)  inz
015200100122     d  datacor8       s              8  0
015300100122     d  datacor6       s              6  0
015400100504     d savnk1          s                   like(ntcnk1)
015500100504     d Wtibs73         s               n   inz(*off)
015600100916     d Woldtic         s                   like(clstic)
015700100916     d Wnewtic         s                   like(clstic)
015800100916     d rqsformatname...
015900100916     d                 S             10A
016000100916     d rqsdatasize...
016100100916     d                 S             10I 0
016200100916     d rqsopcode...
016300100916     d                 S             10A
016400100916     d rpyformatname...
016500100916     d                 S             10A
016600100916     d rpydatasize...
016700100916     d                 S             10I 0
016800100916     d rpyesito...
016900100916     d                 S             10I 0
017000090323     d
017100080208
017200090508      //---------------------------------------------------------------
017300090508      //?Definizione procedure usate.
017400090508      //---------------------------------------------------------------
017500080206
017600100420
017700100420      //---------------------------------------------------------------
017800100420      //?prototipi
017900100420      //---------------------------------------------------------------
018000100916
018100100916      // - Coordinate bancarie
018200100916     d TrulIbanR       pr                  extpgm('TRULIBANR')
018300100916     d  rqsOpCode                    10a   const
018400100916     d  rpyEsito                     10i 0
018500100916     d  rqsFormatName                10a   const
018600100916     d  rqsData                     256a   options(*varsize)
018700100916     d  rqsDataSize                  10i 0 const
018800100916     d  rpyFormatName                10a   const
018900100916     d  rpyData                     256a   options(*varsize)
019000100916     d  rpyDataSize                  10i 0 const
019100100916
019200100420
019300080626      /copy gaitrasrc/srcprotopr,tibs34r
019400100119      /copy gaitrasrc/srcprotopr,tibs69r
019500100503      /copy gaitrasrc/srcprotopr,tibs73r
019600090508
019700080206      //---------------------------------------------------------------
019800080206      //?Definizione key-list.
019900080206      //---------------------------------------------------------------
020000080206
020100080206
020200080206      //---------------------------------------------------------------
020300080206      //?Riepilogo indicatori.
020400080206      //---------------------------------------------------------------
020500100503
020600080206      //---------------------------------------------------------------
020700080206
020800080206      //---------------------------------------------------------------
020900080206      //?M A I N - L I N E
021000080206      //---------------------------------------------------------------
021100080206
021200080206     c     *Entry        plist
021300080206     c                   parm                    KPJBA
021400100503     c                   parm                    tnta55ds
021500080206
021600080206      /free
021700080206
021800080206       // Operazioni iniziali
021900080206       exsr RoutInz;
022000090601
022100100503       SELECT ;
022200100503
022300100503       // se prima convalida di trattativa nuova
022400100503       when ita55prima = 'SI' and Ita55tpv = 'N' ;
022500100503            exsr sr_daTFACO  ;
022600100503            exsr sr_daTFNTCT ;
022700100503
022800100503       // se non è prima convalida di trattativa nuova ksc nuovo OPPURE NO
022900100503       when ita55prima <> 'SI' and Ita55tpv = 'N' ;
023000100503         // verifico il nuovo cnaco
023100100504             acoksc = ita55nwksc ;
023200100503             setll  (acokut:acokcc:acoksc) Cnaco00f ;
023300100503             // esiste cnaco aggiorno solo CNIND
023400100503             If %equal ;
023500100503                exsr sr_updIND  ;
023600100503             else ;
023700100503                exsr sr_daTFACO;
023800100503                exsr sr_daTFNTCT;
023900100503             endif ;
024000100503
024100110114       // se prima convalida di trattativa non nuova con o senza tfaco
024200150707       when ita55prima = 'SI' and Ita55tpv <> 'N' ;
024300100519         // verifico il nuovo cnaco
024400100519             acokut = 1 ;
024500100519             acokcc = dutkci ;
024600100519             acoksc = ita55nwksc ;
024700100519             setll  (acokut:acokcc:acoksc) Cnaco00f ;
024800100916             // esiste cnaco aggiorno solo CNIND se non ho tfaco
024900100519             If %equal ;
025000100916                acokut = 1 ;
025100100916                acokcc = dutkci ;
025200100916                acoksc = ita55nrv ;
025300100916                setll  (acokut:acokcc:acoksc) Tfaco00f ;
025400100916             // non esiste tfaco aggiorno cnind
025500100916                If not %equal(tfaco00f)  ;
025600100916                   exsr sr_updIND  ;
025700100916                else ;
025800100916             // esiste tfaco aggiorno tutta l'anagrafica
025900100916                   exsr sr_daTFACO ;
026000100916              // non scrivo la rubrica in quanto già appoggiata al cliente
026100100916                endif ;
026200100519             else ;
026300100519                acokut = 1 ;
026400100519                acokcc = dutkci ;
026500100519                acoksc = ita55nrv ;
026600100519                setll  (acokut:acokcc:acoksc) Tfaco00f ;
026700100519                If %equal ;
026800100519                   exsr sr_daTFACO ;
026900100916              // scrivo la rubrica copiandolo dal cliente trattativa
027000100916                   exsr sr_daTFNTCC;
027100100917              // scrivo i luoghi copiandoli dal cliente trattativa
027200100917                   exsr sr_daFNSPEC;
027300100519                else ;
027400100519                   exsr sr_daCNACO ;
027500100519                   exsr sr_daTFNTCC;
027600100916                   exsr sr_daFNSPEC;
027700100519                   exsr sr_updIND ;
027800100519                endif ;
027900100916             endif ;
028000100916
028100100510
028200110114       // se non è prima convalida di trattativa non Nuova ksc nuovo OPPURE NO
028300150707       when ita55prima <> 'SI' and Ita55tpv <> 'N' ;
028400100503         // verifico il nuovo cnaco
028500100503             acokut = 1 ;
028600100503             acokcc = dutkci ;
028700100504             acoksc = ita55nwksc ;
028800100503             setll  (acokut:acokcc:acoksc) Cnaco00f ;
028900100503             // esiste cnaco aggiorno solo CNIND
029000100503             If %equal ;
029100100503                exsr sr_updIND  ;
029200100503             else ;
029300100503             // Verifico se esiste tfaco
029400100503                acoksc = ita55nrv ;
029500100503                setll  (acokut:acokcc:acoksc) Tfaco00f ;
029600100503                If %equal ;
029700100503                   exsr sr_daTFACO ;
029800100503              // scrivo la rubrica del cliente trattaiva nel nuovo cliente
029900100503                   exsr sr_daTFNTCC;
030000100916              // scrivo i luoghi del cliente trattativa nel nuovo cliente
030100100916                   exsr sr_daFNSPEC;
030200100503                else ;
030300100503              // copio da cnaco a cnaco e da tfntc a tfntc con codice cliente
030400100503                   exsr sr_daCNACO ;
030500100503                   exsr sr_daTFNTCC;
030600100916                   exsr sr_daFNSPEC;
030700100503                endif ;
030800100503             endif ;
030900100503
031000100503       ENDSL ;
031100080206       // Operazioni finali
031200080206       exsr RoutEnd;
031300080206
031400100121       //--------------------------------------------------------------
031500100503       //?Scrittura Anagrafiche da TFACO NUOVA
031600100121       //--------------------------------------------------------------
031700100503       BEGSR Sr_DaTFACO;
031800140605
031900140605         clear sav_ACOabl;
032000140605         clear sav_INDopz;
032100140605         clear sav_INDcdp;
032200170307         clear sav_INDnpr;
032300170307         clear sav_INDdpr;
032400170307         clear sav_INDnpn;
032500140605         clear sav_CLPnar;
032600140605         clear sav_CLPcon;
032700140605         clear sav_CLPconP;
032800100121
032900100504       acoksc = ita55nwksc ;
033000100503       // verifico se esiste anagrafica con il nuovo codice cliente
033100100503       chain (acokut:acokcc:acoksc) cnaco00f ;
033200100503       chain (acokut:acokcc:acoksc) cnind00f ;
033300100503       chain (acokut:acokcc:acoksc) cnclp00f ;
033400100503       chain (acoksc) fncls01l ;
033500170307
033600170307       //?Controllo subito se l'anagrafica del cliente (CNACO) è
033700170307       //?in contenzioso, se è così NON devo fare nessun aggiornamento
033800170307       //?quindi esco dalla routine
033900170307       IF  %found(CNCLP00F);
034000170307         clear ds4W;
034100170307         TBLkut = 1;
034200170307         TBLcod = '4W';
034300170307         TBLkey = ' ' + %subst(CLPcon:2:2);
034400170307         chain (TBLkut:TBLcod:TBLkey) TABEL00F;
034500170307         IF  %found(TABEL00F) and TBLflg = *blanks;
034600170307           ds4W = TBLuni;
034700170307         ENDIF;
034800170307         IF  §4Wtip <> *blanks;
034900170307           leavesr;
035000170307         ENDIF;
035100170307       ENDIF;
035200100121
035300100121       // se già trovato un'anagrafica esistente memorizzo l'immagine
035400100121       // precedente per storicizzare le variazioni
035500100121
035600100121       If %found(cnaco00f) ;
035700100121          clear tibs73ds ;
035800100121          ibs73immag = 'P' ;
035900100121          TIBS73R (tibs73ds:cnaco_73:cnind_73:cnclp_73:fncls_73);
036000100121          wtibs73 = *On  ;
036100140605         //?salvo il blocco servizio
036200140605          sav_ACOabl = ACOabl;
036300100121       endif;
036400140605
036500140605       //?salvo la causale del blocco servizio e
036600140605       //?      lo stato del credito
036700140605       IF  %found(CNCLP00F);
036800140605         sav_CLPnar = CLPnar;
036900140605         sav_CLPcon = CLPcon;
037000140605       ENDIF;
037100140605
037200140605       //?salvo il blocco pagamenti e
037300140605       //?      le condizioni di pagamento
037400170307       //?      e dati relativi alla dichiarazione di intenti
037500140605       IF  %found(CNIND00F);
037600140605         sav_INDopz = INDopz;
037700140605         sav_INDcdp = INDcdp;
037800170307         sav_INDnpr = INDnpr;
037900170307         sav_INDdpr = INDdpr;
038000170307         sav_INDnpn = INDnpn;
038100140605       ENDIF;
038200100121
038300100503       // aggancio anagrafica provvisoria   TFACO00F  con numero trattativa
038400100503       chain(n) (acokut:acokcc:ita55nrv) tfaco00f ;
038500100121
038600100121       If  %found(tfaco00f) ;
038700100504           acoksc = ita55nwksc ;
038800100121           acoftt = '3' ;
038900100504           acoflt = %dec(%subst(%EDITC(ita55nwKsc:'X'):1:3):3:0);
039000100121           clear acoftr ;
039100100121           clear acodtr ;
039200100121           acoduv = datacor6 ;
039300100121          if not %found(cnaco00f) ;
039400100121             nrjob = knraz ;
039500140605         //?il nuovo codice lo devo creare sbloccato
039600140605             clear ACOabl;
039700100121             write cnaco000 ;
039800100504             ota55new = '1';
039900100121          else ;
040000140605         //?devo mantenere il vecchio blocco servizio
040100140605             ACOabl = sav_ACOabl;
040200100121             update cnaco000 ;
040300100504             ota55new = '0';
040400100121          endif ;
040500140605
040600140605       //?Sposto qua l'aggiornamento del CNCLP in modo da poter
040700140605       //?verificare lo stato del credito, che serve poi per le
040800140605       //?condizioni di pagamento
040900140605       // aggancio anagrafica provvisoria   TFCLP00F con numero trattativa
041000140605       chain(n) (acokut:acokcc:ita55nrv) tfclp00f ;
041100140605
041200140605       If  %found(tfclp00f) ;
041300140605
041400140605       //?salvo lo stato del credito di TFACO
041500140605           sav_CLPconP = CLPcon;
041600140605
041700140605           clpksc = ita55nwksc ;
041800140605       //  se non c'è cnclp imposto il commerciale della visita e non + quello
041900140605       //  dell'anagrafica provvisoria xchè potrebbe essere incongruente con il
042000140605       //  p.o. del commerciale della visita
042100140605           if not %found(cnclp00f) ;
042200140605              clpage = ita55cmm ;
042300140605              clpnrj = knraz ;
042400140605         //?il nuovo codice lo devo creare senza stato del credito
042500140605         //?                             e senza causale blocco
042600140605              clear CLPcon;
042700140605              clear CLPnar;
042800140605              write cnclp000 ;
042900140605           else ;
043000140605         //?devo mantenere il vecchio stato del credito e
043100140605         //?               la vecchia causale del blocco servizio
043200140605              CLPcon = sav_CLPcon;
043300140605              CLPnar = sav_CLPnar;
043400140605              update cnclp000 ;
043500140605           endif ;
043600140605       endif ;
043700100121
043800100503       // aggancio anagrafica provvisoria   TFIND00F con numero trattativa
043900100503       chain (acokut:acokcc:ita55nrv) tfind00f ;
044000100121       // se modificati i dati del pagamento li registro prima nell'anagrafica
044100100121       // provvisoria
044200100121
044300100121       If  %found(tfind00f) ;
044400100520           If Ita55pag = '1' ;
044500100503              indabi = ita55abi ;
044600100503              indcab = ita55cab ;
044700100505              indcdp = ita55cdp ;
044800100122              update tfind000 ;
044900100503              chain(n) (acokut:acokcc:ita55nrv) tfind00f ;
045000100122           endif ;
045100100504           indksc = ita55nwksc ;
045200100122       // Partita Iva
045300100503           indiva = ita55iva ;
045400100122       // Codice Fiscale
045500100503           indcdf = ita55cdf;
045600100122
045700100122           if not %found(cnind00f) ;
045800100122              indnrj = knraz ;
045900140605         //?il nuovo codice lo devo creare senza blocco pagamenti
046000140605              %subst(INDopz:1:1) = '0';
046100140605         //?se in TFACO c'è uno stato del credito le condizioni di
046200140605         //?pagamento le imposto vuote
046300140605              IF  %subst(sav_CLPconP:2:2) <> *blanks;
046400140605                %subst(INDcdp:4:3) = *blanks;
046500140605              ENDIF;
046600170307            //?NON riporto i dati relativi alla dichiarazioni d'intenti
046700170307              clear INDnpr;
046800170307              clear INDdpr;
046900170307              clear INDnpn;
047000100122              write cnind000 ;
047100100122           else ;
047200140605         //?devo mantenere il vecchio blocco pagamenti
047300140605              %subst(INDopz:1:1) = %subst(sav_INDopz:1:1);
047400140605         //?se c'è anche solo 1 stato del credito devo mantenere le
047500140605         //?vecchie condizioni di pagamento
047600140605              IF  %subst(sav_CLPcon:2:2) <> *blanks or
047700140605                  %subst(sav_CLPconP:2:2) <> *blanks;
047800140605                %subst(INDcdp:4:3) = %subst(sav_INDcdp:4:3);
047900140605              ENDIF;
048000170307         //?devo mantenere i vecchi dati della dichiarazione d'intenti
048100170307              INDnpr = sav_INDnpr;
048200170307              INDdpr = sav_INDdpr;
048300170307              INDnpn = sav_INDnpn;
048400100122              update cnind000 ;
048500100122           endif;
048600100122       endif ;
048700100122
048800100503       // aggancio anagrafica provvisoria   TFCLS00F con numero trattativa
048900100916
049000100916       // mi memorizzo i codici pagamenti vecchi
049100100916          clear Woldtic ;
049200100916          clear Wnewtic ;
049300100916
049400100916       If %found(fncls01l);
049500100916          Woldtic = clstic ;
049600100916       endif ;
049700100916
049800100503       chain(n) (ita55nrv) tfcls01l ;
049900100122
050000100122       If  %found(tfcls01l) ;
050100100916           Wnewtic = clstic ;
050200100504           clsksc = ita55nwksc ;
050300100122           if not %found(fncls01l) ;
050400100122              write fncls000 ;
050500100122           else ;
050600100122              update fncls000 ;
050700100122           endif ;
050800100122
050900100122        Endif ;
051000100916
051100100916       // aggancio anagrafica provvisoria   TFCBA00F con numero trattativa
051200100916       // e per ogni tipo pagamento per verificare l'esistenza o meno dei
051300100916       // record. Se record non presente deleto FNCBA con TRULIBAN
051400100916       // mentre se c'è faccio update su FNCBA con TRULIBAN
051500100916
051600100916       // DANNI
051700100916
051800100916       chain(n) (ita55nrv:'DN') tfcba01l ;
051900100916
052000100916       If  not %found(tfcba01l) ;
052100100916           rqsOpCode = 'DELETE' ;
052200100916           CBAPAG = 'DN' ;
052300100916           CLEAR  CBAABI ;
052400100916           CLEAR  CBABIC ;
052500100916           exsr coordinate ;
052600100916       else ;
052700100916           If %found(cnaco00f) ;
052800100916              rqsOpCode = 'UPDATE' ;
052900100916           else ;
053000100916              rqsOpCode = 'INSERT' ;
053100100916           endif;
053200100916           exsr coordinate ;
053300100916       endif;
053400100916
053500100916       // NOTE ACCREDITO
053600100916
053700100916       chain(n) (ita55nrv:'NA') tfcba01l ;
053800100916
053900100916       If  not %found(tfcba01l) ;
054000100916           rqsOpCode = 'DELETE' ;
054100100916           CBAPAG = 'AN' ;
054200100916           CLEAR  CBAABI ;
054300100916           CLEAR  CBABIC ;
054400100916           exsr Coordinate ;
054500100916       else ;
054600100916           If %found(cnaco00f) ;
054700100916              rqsOpCode = 'UPDATE' ;
054800100916           else ;
054900100916              rqsOpCode = 'INSERT' ;
055000100916           endif;
055100100916           exsr Coordinate    ;
055200100916       endif;
055300100916
055400100122
055500100510       // Memorizzo l'immagine seguente per storicizzare le variazioni
055600100510           exsr ScriviACV ;
055700100122        Endif ;
055800100503
055900100503       ENDSR;
056000100503
056100100916       //--------------------------------------------------------------
056200100916       //?Operazioni relative alle coordinate bancarie.
056300100916       //--------------------------------------------------------------
056400100916       BEGSR Coordinate;
056500100916
056600100916         clear rpyEsito;
056700100916
056800100916           clear TrulIbanI0;
056900100916           clear TrulIbanO0;
057000100916           TrulIbanI0.KSC = ita55nwksc ;
057100100916           TrulIbanI0.PAG = cbapag ;
057200100916           TrulIbanI0.IBAN  = cbaiban ;
057300100916           TrulIbanI0.BIC   = cbabic ;
057400100916           If cbapag = 'DN' ;
057500100916              TrulIbanI0.CODPO = %subst(Woldtic:1:1);
057600100916              TrulIbanI0.CODPN = %subst(Wnewtic:1:1);
057700100916           endif ;
057800100916           If cbapag = 'NA' ;
057900100916              TrulIbanI0.CODPO = %subst(Woldtic:2:1);
058000100916              TrulIbanI0.CODPN = %subst(Wnewtic:2:1);
058100100916           endif ;
058200100916           TrulIbanI0.PGM   = 'TNTA56R' ;
058300100916           rqsFormatName = 'TRULIBANI0';
058400100916           rpyFormatName = 'TRULIBANO0';
058500100916           rqsDataSize   = %size(TrulIbanI0);
058600100916           rpyDataSize   = %size(TrulIbanO0);
058700100916           TrulIbanR (rqsOpCode:rpyEsito:
058800100916                      rqsFormatName:TrulIbanI0:rqsDataSize:
058900100916                      rpyFormatname:TrulIbanO0:rpyDataSize);
059000100916
059100100916       ENDSR;
059200100504       //--------------------------------------------------------------
059300100504       //?Scrittura Anagrafiche da CNACO NUOVA
059400100504       //--------------------------------------------------------------
059500100504       BEGSR Sr_DaCNACO;
059600100504
059700100504       // copio da cnaco cliente della trattativa a nuovo cliente
059800100504             chain(n) (acokut:acokcc:ita55ksc) cnaco00f ;
059900100504             If %found(cnaco00f) ;
060000100504                acoksc = ita55nwksc ;
060100100504                acoftt = '3' ;
060200100504                acoflt = %dec(%subst(%EDITC(ita55nwKsc:'X'):1:3):3:0);
060300100504                clear acoftr ;
060400100504                clear acodtr ;
060500100504                acoduv = datacor6 ;
060600100504                nrjob = knraz ;
060700100504                write cnaco000 ;
060800100504                ota55new = '1';
060900100504             endif ;
061000100504
061100100504       // copio da cnind cliente della trattativa a nuovo cliente
061200100504             chain(n) (acokut:acokcc:ita55ksc) cnind00f ;
061300100504             If %found(cnind00f) ;
061400100520                If Ita55pag = '1' ;
061500100504                   indabi = ita55abi ;
061600100504                   indcab = ita55cab ;
061700100505                   indcdp = ita55cdp ;
061800100504                endif ;
061900100504                indksc = ita55nwksc ;
062000100504       // Partita Iva
062100100504                indiva = ita55iva ;
062200100504       // Codice Fiscale
062300100504                indcdf = ita55cdf;
062400100504
062500100504                indnrj = knraz ;
062600170307              //?NON riporto i dati relativi alla dichiarazioni d'intenti
062700170307                clear INDnpr;
062800170307                clear INDdpr;
062900170307                clear INDnpn;
063000100504                write cnind000 ;
063100100504             endif ;
063200100504
063300100504       // copio da cnclp cliente della trattativa a nuovo cliente
063400100504       chain(n) (acokut:acokcc:ita55ksc) cnclp00f ;
063500100504
063600100504       If  %found(cnclp00f) ;
063700100504           clpksc = ita55nwksc ;
063800100504           clpage = ita55cmm ;
063900100504           clpnrj = knraz ;
064000100504           write cnclp000 ;
064100100504       endif ;
064200100504
064300100504       // copio da fncls cliente della trattativa a nuovo cliente
064400100504       chain(n) (ita55ksc) fncls01l ;
064500100504
064600100504       If  %found(fncls01l) ;
064700100504           clsksc = ita55nwksc ;
064800100504           write fncls000 ;
064900100504       endif ;
065000100504
065100100915       // copio da fncba cliente della trattativa a nuovo cliente
065200100915       setll    (ita55ksc) fncba01l ;
065300100915       reade(n) (ita55ksc) fncba01l ;
065400100915
065500100915       dow  not %eof(fncba01l) ;
065600100915           cbaksc = ita55nwksc ;
065700100916           write tfcba000 ;
065800100916           reade(n) (ita55ksc) fncba01l ;
065900100916       enddo ;
066000100915
066100100510       // Memorizzo l'immagine seguente per storicizzare le variazioni
066200100510           exsr ScriviACV ;
066300100504
066400100504       ENDSR;
066500100504
066600100503       //--------------------------------------------------------------
066700100503       //?Aggiorno CNIND da dati a video
066800100503       //--------------------------------------------------------------
066900100503       BEGSR Sr_UPDIND ;
067000170307
067100170307       //?Controllo subito se l'anagrafica del cliente (CNACO) è
067200170307       //?in contenzioso, se è così NON devo fare nessun aggiornamento
067300170307       //?quindi esco dalla routine
067400170307         IF  ITA55tpv <> 'N';
067500170307           chain (ACOkut:ACOkcc:ITA55nwksc) CNCLP00F;
067600170307           IF  %found(CNCLP00F);
067700170307             clear ds4W;
067800170307             TBLkut = 1;
067900170307             TBLcod = '4W';
068000170307             TBLkey = ' ' + %subst(CLPcon:2:2);
068100170307             chain (TBLkut:TBLcod:TBLkey) TABEL00F;
068200170307             IF  %found(TABEL00F) and TBLflg = *blanks;
068300170307               ds4W = TBLuni;
068400170307             ENDIF;
068500170307             IF  §4Wtip <> *blanks;
068600170307               leavesr;
068700170307             ENDIF;
068800170307           ENDIF;
068900170307         ENDIF;
069000100503
069100100504           chain (acokut:acokcc:ita55nwksc) cnind00f ;
069200100122           If %found(cnind00f) ;
069300100122       // se modificate le condizioni di pagamento si aggiornano
069400100520              If Ita55pag = '1' ;
069500100503                 indabi = ita55abi ;
069600100503                 indcab = ita55cab ;
069700100505                 indcdp = ita55cdp ;
069800100503              endif ;
069900100503       // Partita Iva
070000100503              indiva = ita55iva ;
070100100503       // Codice Fiscale
070200100503              indcdf = ita55cdf;
070300100503              update cnind000 ;
070400100503           endif;
070500100122
070600100121       ENDSR ;
070700100122
070800100122       //--------------------------------------------------------------
070900100122       //?Scrittura del file delle variazioni Anagrafiche
071000100122       //--------------------------------------------------------------
071100100122       BEGSR Scriviacv ;
071200100122
071300100122       Clear Tibs73ds ;
071400100122       ibs73pru=sdsuse ;
071500100122       ibs73noj=sdsjob ;
071600100506       ibs73pgm='TNTA56R';
071700100122       ibs73immag='D';
071800100122
071900100122       //  Immissione
072000100504       If  Ota55new    = '1' ;
072100100122           ibs73cta='O' ;
072200100122           TIBS73R (tibs73ds:cnaco_73:cnind_73:cnclp_73:fncls_73);
072300100122       //  Manutenzione
072400100122       else;
072500100122           ibs73cta='M' ;
072600100122           TIBS73R (tibs73ds:cnaco_73:cnind_73:cnclp_73:fncls_73);
072700100122       endif;
072800100122
072900100122       wtibs73 = *On ;
073000100122
073100100122       ENDSR ;
073200100424
073300100424       //--------------------------------------------------------------
073400100503       //?Scrittura Note Rubrica da trattativa
073500100424       //--------------------------------------------------------------
073600100503       BEGSR Sr_Datfntct;
073700100424
073800100503         savnk1 = %editc(dutkci: 'X') + %editc(ita55nrv:'X');
073900100425       // cerco le note relative alla trattativa da duplicare sul cliente
074000100504         exec sql declare c1 cursor for select *
074100100425         from tfntc01l where ntcapl = 'T' and ntcnk1 = :savnk1
074200100427         and ntctnt <> '10'
074300100425         order by ntcapl, ntcnk1, ntcnk2, ntctnt
074400100425         for read only  ;
074500100425
074600100504         exec sql open c1 ;
074700100425
074800100504         exec sql fetch c1 into :tfntcds ;
074900100425
075000100425         dow Sqlcod = 0;
075100100425
075200100425            If Ntcflt <> 'A' ;
075300100425    c          xx = 1 ;
075400100425               xx = %lookup(ntctnt:tno) ;
075500100425               If xx > 0  and tdup(xx) = 'S';
075600100425                   ntcapl = 'C' ;
075700100504                   ntcnk1 = %editc(dutkci: 'X') + %editc(ita55nwksc:'X');
075800100425                   ntcntr = datacor6;
075900100425                   write tfntc ;
076000100425    2          EndIf ;
076100100425    2       EndIf ;
076200100425
076300100504          exec sql fetch c1 into :tfntcds ;
076400100425
076500100425    1    enddo;
076600100425
076700100504         exec sql close c1 ;
076800100425
076900100424       ENDSR ;
077000100503
077100100503       //--------------------------------------------------------------
077200100503       //?Scrittura Note Rubrica da cliente
077300100503       //--------------------------------------------------------------
077400100503       BEGSR Sr_Datfntcc;
077500100503
077600100503         savnk1 = %editc(dutkci: 'X') + %editc(ita55ksc:'X');
077700100916       // cerco le note relative al cliente trattativa da duplicare sul cliente
077800100503         exec sql declare c2 cursor for select *
077900100506         from tfntc01l where ntcapl = 'C' and ntcnk1 = :savnk1
078000100503         and ntctnt <> '10'
078100100503         order by ntcapl, ntcnk1, ntcnk2, ntctnt
078200100503         for read only  ;
078300100503
078400100503         exec sql open c2 ;
078500100503
078600100503         exec sql fetch c2 into :tfntcds ;
078700100503
078800100503         dow Sqlcod = 0;
078900100503
079000100503            If Ntcflt <> 'A' ;
079100100503    c          xx = 1 ;
079200100503               xx = %lookup(ntctnt:tno) ;
079300100503               If xx > 0  and tdup(xx) = 'S';
079400100503                   ntcapl = 'C' ;
079500100504                   ntcnk1 = %editc(dutkci: 'X') + %editc(ita55nwksc:'X');
079600100503                   ntcntr = datacor6;
079700100503                   write tfntc ;
079800100503    2          EndIf ;
079900100503    2       EndIf ;
080000100503
080100100503          exec sql fetch c2 into :tfntcds ;
080200100503
080300100503    1    enddo;
080400100503
080500100503         exec sql close c2 ;
080600100503
080700100503       ENDSR ;
080800100916
080900100916       //--------------------------------------------------------------
081000100916       //?Scrittura luoghi  da cliente
081100100916       //--------------------------------------------------------------
081200100916       BEGSR Sr_Dafnspec;
081300100916
081400100916       // cerco i luoghi relative al cliente da duplicare sul cliente
081500100916           exec sql declare s1 cursor for select *
081600100916           from fnspe01l where spefls = 'L' and specli = :ita55ksc
081700100916           for read only  ;
081800100916
081900100916           exec sql open s1 ;
082000100916
082100100916           exec sql fetch s1 into :fnspeds ;
082200100916
082300100916           dow Sqlcod = 0;
082400100916
082500100916              If Speflg =  ' ' ;
082600100916
082700100916    c            xx = 1 ;
082800100916                 xx = %lookup(specod:tluo) ;
082900100916                 If xx > 0  and tufi(xx) = 'S';
083000100916
083100100916                     specli = ita55nwksc;
083200100916                     spedtr = datacor6;
083300100916                     speftr = ' ';
083400100916                     write fnspe000 ;
083500100916
083600100916           // cerco i suoi fnsp2
083700100916                     setll (ita55ksc:specod) fnsp201l;
083800100916                     reade (ita55ksc:specod) fnsp201l;
083900100916                     dow not %eof(fnsp201l) ;
084000100916                         if Sp2flg <> 'A' ;
084100100916                            sp2cli = ita55nwksc ;
084200100916                            write Fnsp2000 ;
084300100916                         endif ;
084400110222                       reade (ita55ksc:specod) fnsp201l;
084500100916                     Enddo ;
084600100916                 endif  ;
084700100916
084800100916    2         EndIf ;
084900100916
085000100916            exec sql fetch s1 into :fnspeds ;
085100100916
085200100916    1     enddo;
085300100916
085400100916            exec sql close s1 ;
085500100916
085600100916       ENDSR ;
085700100115       //--------------------------------------------------------------
085800100115       //?Operazioni iniziali.
085900100115       //--------------------------------------------------------------
086000100115       BEGSR RoutInz;
086100100115
086200100115         // Reperimento dati job
086300100115         exsr DatiJob;
086400100115
086500100115       // Reperisco data corrente
086600100115             data_iso = (%date());
086700100122             datacor8 = %dec(data_iso);
086800100125             data_ymd = (%date());
086900100121             datacor6 = %dec(data_ymd) ;
087000100122
087100100122        //  Carico schiera con tabella 1T (tipo nota)
087200100122             Clear   xx;
087300100505             TBLkut = 1 ;
087400100122             TBLcod = '1T';
087500100122             setll (TBLkut : TBLcod) TABEL;
087600100122             reade (TBLkut : TBLcod) TABEL;
087700100122             dow not %eof(TABEL00F);
087800100122                 If tblflg = ' ' and tblkey <> *blanks ;
087900100122                    xx = xx + 1 ;
088000100122                    ds1t = tbluni ;
088100100122                    tno(xx) = tblkey ;
088200100122                    tdup(xx) = §1tdup;
088300100122                    ttdp(xx) = §1ttdp;
088400100122                 endif ;
088500100122                 reade (TBLkut : TBLcod) TABEL;
088600100122             EndDo ;
088700100115
088800100916        //  Carico schiera con tabella 4L (Lughi)
088900100916             Clear   xx;
089000100916             TBLkut = 1 ;
089100100916             TBLcod = '4L';
089200100916             setll (TBLkut : TBLcod) TABEL;
089300100916             reade (TBLkut : TBLcod) TABEL;
089400100916             dow not %eof(TABEL00F);
089500100916                 If tblflg = ' ' and tblkey <> *blanks ;
089600100916                    xx = xx + 1 ;
089700100917                    ds4l = tbluni ;
089800100916                    tluo(xx) = tblkey ;
089900100916                    tufi(xx) = §4Lufi;
090000100916                 endif ;
090100100916                 reade (TBLkut : TBLcod) TABEL;
090200100916             EndDo ;
090300100916
090400100506             acokut = 1 ;
090500100506             acokcc = dutkci ;
090600100422
090700100115       ENDSR;
090800100115
090900100115       //--------------------------------------------------------------
091000100115       //?Reperimento Dati del job (Utente/Operativi).
091100100115       //--------------------------------------------------------------
091200100115       BEGSR DatiJob;
091300100115
091400100115         in(E) §AzUte;
091500100115         if NOT %error;
091600100115           in(E) §DatiUte;
091700100115         endif;
091800100115         if %error or RSut = *blanks;
091900100115           clear TIBS34ds;
092000100115           tibs34r(tibs34ds);
092100100115           in §AzUte;
092200100115           in §DatiUte;
092300100115         endif;
092400100115
092500100115       ENDSR;
092600100108
092700080206       //--------------------------------------------------------------
092800080206       //?Operazioni finali.
092900080206       //--------------------------------------------------------------
093000080206       BEGSR RoutEnd;
093100090806
093200100504
093300100504       If wtibs73 = *On  ;
093400100504          ibs73tla = 'C' ;
093500100504          TIBS73R (tibs73ds:cnaco_73:cnind_73:cnclp_73:fncls_73);
093600100504       EndIf ;
093700080206         *inLR = *on;
093800080206         return;
093900080206
094000080206       ENDSR;
094100080206
094200080206      /end-free
