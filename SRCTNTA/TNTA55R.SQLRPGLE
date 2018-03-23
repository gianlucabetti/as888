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
009400100916
009500100119       // - Reperimento dati anagrafici
009600100119     d TIBS69ds      e ds
009700100504     d DS_cnaco      e ds                  inz extname(CNACO00F) prefix(w_)
009800100504     d DS_cnind      e ds                  inz extname(CNIND00F) prefix(w_)
009900100504     d DS_cnclp      e ds                  inz extname(CNCLP00F) prefix(w_)
010000100504     d DS_fncls      e ds                  inz extname(FNCLS00F) prefix(w_)
010100100119
010200100121       // - Creazione variazioni dati anagrafici
010300100121     d TIBS73ds      e ds
010400100121     d cnaco_73      e ds                  inz extname(CNACO00F)
010500100121     d cnind_73      e ds                  inz extname(CNIND00F)
010600100121     d cnclp_73      e ds                  inz extname(CNCLP00F)
010700100121     d fncls_73      e ds                  inz extname(FNCLS00F)
010800100121
010900100916      // - Per creazione o delle note trattative da copiare
011000100916     d TrulIbanI0    e ds                  qualified
011100100916     d TrulIbanO0    e ds                  qualified
011200100916
011300100425      // - Per il recupero delle note trattative da copiare
011400100425     d tfntcDs       e ds                  ExtName(tfntc00f)
011500100425
011600100916      // - Per il recupero dei luoghi da copiare
011700100916     d fnspeDs       e ds                  ExtName(fnspe00f)
011800080206      //---------------------------------------------------------------
011900080206      //?Definizione variabili globali.
012000080206      //---------------------------------------------------------------
012100080206
012200080206      // - Flags booleani
012300080208     d $Fine           s               n   inz(*off)
012400080207     d $EoF            s               n   inz(*off)
012500080208     d $RecOK          s               n   inz(*off)
012600080206
012700080206      // - Indici di schiera
012800080208     d xx              s              3  0 inz
012900080206
013000140605      // - Campi di salvataggio
013100140605     d sav_ACOabl      s                   like(ACOabl)
013200140605     d sav_INDopz      s                   like(INDopz)
013300140605     d sav_INDcdp      s                   like(INDcdp)
013400170317     d sav_INDnpr      s                   like(INDnpr)
013500170317     d sav_INDdpr      s                   like(INDdpr)
013600170317     d sav_INDnpn      s                   like(INDnpn)
013700140605     d sav_CLPnar      s                   like(CLPnar)
013800140605     d sav_CLPcon      s                   like(CLPcon)
013900140605     d sav_CLPconP     s                   like(CLPcon)
014000170317     d sav_CLPfft      s                   like(CLPfft)
014100080206
014200090401      // - Campi di comodo data
014300090401     d  data_eur       s               d   Datfmt(*eur)
014400090401     d  data_iso       s               d   Datfmt(*iso)
014500090401     d  data_dmy       s               d   Datfmt(*dmy)
014600090811     d  data_ymd       s               d   datfmt(*ymd)
014700090401
014800080206      // - Campi di comodo
014900090406     d wAbi            s                   like(UTEaut)  inz
015000100122     d  datacor8       s              8  0
015100100122     d  datacor6       s              6  0
015200100504     d savnk1          s                   like(ntcnk1)
015300100504     d Wtibs73         s               n   inz(*off)
015400100916     d Woldtic         s                   like(clstic)
015500100916     d Wnewtic         s                   like(clstic)
015600100916     d rqsformatname...
015700100916     d                 S             10A
015800100916     d rqsdatasize...
015900100916     d                 S             10I 0
016000100916     d rqsopcode...
016100100916     d                 S             10A
016200100916     d rpyformatname...
016300100916     d                 S             10A
016400100916     d rpydatasize...
016500100916     d                 S             10I 0
016600100916     d rpyesito...
016700100916     d                 S             10I 0
016800090323     d
016900080208
017000090508      //---------------------------------------------------------------
017100090508      //?Definizione procedure usate.
017200090508      //---------------------------------------------------------------
017300080206
017400100420
017500100420      //---------------------------------------------------------------
017600100420      //?prototipi
017700100420      //---------------------------------------------------------------
017800100916
017900100916      // - Coordinate bancarie
018000100916     d TrulIbanR       pr                  extpgm('TRULIBANR')
018100100916     d  rqsOpCode                    10a   const
018200100916     d  rpyEsito                     10i 0
018300100916     d  rqsFormatName                10a   const
018400100916     d  rqsData                     256a   options(*varsize)
018500100916     d  rqsDataSize                  10i 0 const
018600100916     d  rpyFormatName                10a   const
018700100916     d  rpyData                     256a   options(*varsize)
018800100916     d  rpyDataSize                  10i 0 const
018900100916
019000100420
019100080626      /copy gaitrasrc/srcprotopr,tibs34r
019200100119      /copy gaitrasrc/srcprotopr,tibs69r
019300100503      /copy gaitrasrc/srcprotopr,tibs73r
019400090508
019500080206      //---------------------------------------------------------------
019600080206      //?Definizione key-list.
019700080206      //---------------------------------------------------------------
019800080206
019900080206
020000080206      //---------------------------------------------------------------
020100080206      //?Riepilogo indicatori.
020200080206      //---------------------------------------------------------------
020300100503
020400080206      //---------------------------------------------------------------
020500080206
020600080206      //---------------------------------------------------------------
020700080206      //?M A I N - L I N E
020800080206      //---------------------------------------------------------------
020900080206
021000080206     c     *Entry        plist
021100080206     c                   parm                    KPJBA
021200100503     c                   parm                    tnta55ds
021300080206
021400080206      /free
021500080206
021600080206       // Operazioni iniziali
021700080206       exsr RoutInz;
021800090601
021900100503       SELECT ;
022000100503
022100100503       // se prima convalida di trattativa nuova
022200100503       when ita55prima = 'SI' and Ita55tpv = 'N' ;
022300100503            exsr sr_daTFACO  ;
022400100503            exsr sr_daTFNTCT ;
022500100503
022600100503       // se non è prima convalida di trattativa nuova ksc nuovo OPPURE NO
022700100503       when ita55prima <> 'SI' and Ita55tpv = 'N' ;
022800100503         // verifico il nuovo cnaco
022900100504             acoksc = ita55nwksc ;
023000100503             setll  (acokut:acokcc:acoksc) Cnaco00f ;
023100100503             // esiste cnaco aggiorno solo CNIND
023200100503             If %equal ;
023300100503                exsr sr_updIND  ;
023400100503             else ;
023500100503                exsr sr_daTFACO;
023600100503                exsr sr_daTFNTCT;
023700100503             endif ;
023800100503
023900110114       // se prima convalida di trattativa non nuova con o senza tfaco
024000150707       when ita55prima = 'SI' and Ita55tpv <> 'N' ;
024100100519         // verifico il nuovo cnaco
024200100519             acokut = 1 ;
024300100519             acokcc = dutkci ;
024400100519             acoksc = ita55nwksc ;
024500100519             setll  (acokut:acokcc:acoksc) Cnaco00f ;
024600100916             // esiste cnaco aggiorno solo CNIND se non ho tfaco
024700100519             If %equal ;
024800100916                acokut = 1 ;
024900100916                acokcc = dutkci ;
025000100916                acoksc = ita55nrv ;
025100100916                setll  (acokut:acokcc:acoksc) Tfaco00f ;
025200100916             // non esiste tfaco aggiorno cnind
025300100916                If not %equal(tfaco00f)  ;
025400100916                   exsr sr_updIND  ;
025500100916                else ;
025600100916             // esiste tfaco aggiorno tutta l'anagrafica
025700100916                   exsr sr_daTFACO ;
025800100916              // non scrivo la rubrica in quanto già appoggiata al cliente
025900100916                endif ;
026000100519             else ;
026100100519                acokut = 1 ;
026200100519                acokcc = dutkci ;
026300100519                acoksc = ita55nrv ;
026400100519                setll  (acokut:acokcc:acoksc) Tfaco00f ;
026500100519                If %equal ;
026600100519                   exsr sr_daTFACO ;
026700100916              // scrivo la rubrica copiandolo dal cliente trattativa
026800100916                   exsr sr_daTFNTCC;
026900100917              // scrivo i luoghi copiandoli dal cliente trattativa
027000100917                   exsr sr_daFNSPEC;
027100100519                else ;
027200100519                   exsr sr_daCNACO ;
027300100519                   exsr sr_daTFNTCC;
027400100916                   exsr sr_daFNSPEC;
027500100519                   exsr sr_updIND ;
027600100519                endif ;
027700100916             endif ;
027800100916
027900100510
028000110114       // se non è prima convalida di trattativa non Nuova ksc nuovo OPPURE NO
028100150707       when ita55prima <> 'SI' and Ita55tpv <> 'N' ;
028200100503         // verifico il nuovo cnaco
028300100503             acokut = 1 ;
028400100503             acokcc = dutkci ;
028500100504             acoksc = ita55nwksc ;
028600100503             setll  (acokut:acokcc:acoksc) Cnaco00f ;
028700100503             // esiste cnaco aggiorno solo CNIND
028800100503             If %equal ;
028900100503                exsr sr_updIND  ;
029000100503             else ;
029100100503             // Verifico se esiste tfaco
029200100503                acoksc = ita55nrv ;
029300100503                setll  (acokut:acokcc:acoksc) Tfaco00f ;
029400100503                If %equal ;
029500100503                   exsr sr_daTFACO ;
029600100503              // scrivo la rubrica del cliente trattaiva nel nuovo cliente
029700100503                   exsr sr_daTFNTCC;
029800100916              // scrivo i luoghi del cliente trattativa nel nuovo cliente
029900100916                   exsr sr_daFNSPEC;
030000100503                else ;
030100100503              // copio da cnaco a cnaco e da tfntc a tfntc con codice cliente
030200100503                   exsr sr_daCNACO ;
030300100503                   exsr sr_daTFNTCC;
030400100916                   exsr sr_daFNSPEC;
030500100503                endif ;
030600100503             endif ;
030700100503
030800100503       ENDSL ;
030900080206       // Operazioni finali
031000080206       exsr RoutEnd;
031100080206
031200100121       //--------------------------------------------------------------
031300100503       //?Scrittura Anagrafiche da TFACO NUOVA
031400100121       //--------------------------------------------------------------
031500100503       BEGSR Sr_DaTFACO;
031600140605
031700140605         clear sav_ACOabl;
031800140605         clear sav_INDopz;
031900140605         clear sav_INDcdp;
032000170317         clear sav_INDnpr;
032100170317         clear sav_INDdpr;
032200170317         clear sav_INDnpn;
032300140605         clear sav_CLPnar;
032400140605         clear sav_CLPcon;
032500140605         clear sav_CLPconP;
032600170317         clear sav_CLPfft;
032700100121
032800100504       acoksc = ita55nwksc ;
032900100503       // verifico se esiste anagrafica con il nuovo codice cliente
033000100503       chain (acokut:acokcc:acoksc) cnaco00f ;
033100100503       chain (acokut:acokcc:acoksc) cnind00f ;
033200100503       chain (acokut:acokcc:acoksc) cnclp00f ;
033300100503       chain (acoksc) fncls01l ;
033400100121
033500100121       // se già trovato un'anagrafica esistente memorizzo l'immagine
033600100121       // precedente per storicizzare le variazioni
033700100121
033800100121       If %found(cnaco00f) ;
033900100121          clear tibs73ds ;
034000100121          ibs73immag = 'P' ;
034100100121          TIBS73R (tibs73ds:cnaco_73:cnind_73:cnclp_73:fncls_73);
034200100121          wtibs73 = *On  ;
034300140605         //?salvo il blocco servizio
034400140605          sav_ACOabl = ACOabl;
034500100121       endif;
034600140605
034700140605       //?salvo la causale del blocco servizio e
034800140605       //?      lo stato del credito
034900170317       //?      la frequenza fattura
035000140605       IF  %found(CNCLP00F);
035100140605         sav_CLPnar = CLPnar;
035200140605         sav_CLPcon = CLPcon;
035300170317         sav_CLPfft = CLPfft;
035400140605       ENDIF;
035500140605
035600140605       //?salvo il blocco pagamenti e
035700140605       //?      le condizioni di pagamento
035800170317       //?      i dati relativi all'esenzione IVA
035900140605       IF  %found(CNIND00F);
036000140605         sav_INDopz = INDopz;
036100140605         sav_INDcdp = INDcdp;
036200170317         sav_INDnpr = INDnpr;
036300170317         sav_INDdpr = INDdpr;
036400170317         sav_INDnpn = INDnpn;
036500140605       ENDIF;
036600100121
036700100503       // aggancio anagrafica provvisoria   TFACO00F  con numero trattativa
036800100503       chain(n) (acokut:acokcc:ita55nrv) tfaco00f ;
036900100121
037000100121       If  %found(tfaco00f) ;
037100100504           acoksc = ita55nwksc ;
037200100121           acoftt = '3' ;
037300100504           acoflt = %dec(%subst(%EDITC(ita55nwKsc:'X'):1:3):3:0);
037400100121           clear acoftr ;
037500100121           clear acodtr ;
037600100121           acoduv = datacor6 ;
037700100121          if not %found(cnaco00f) ;
037800100121             nrjob = knraz ;
037900140605         //?il nuovo codice lo devo creare sbloccato
038000140605             clear ACOabl;
038100100121             write cnaco000 ;
038200100504             ota55new = '1';
038300100121          else ;
038400140605         //?devo mantenere il vecchio blocco servizio
038500140605             ACOabl = sav_ACOabl;
038600100121             update cnaco000 ;
038700100504             ota55new = '0';
038800100121          endif ;
038900140605
039000140605       //?Sposto qua l'aggiornamento del CNCLP in modo da poter
039100140605       //?verificare lo stato del credito, che serve poi per le
039200140605       //?condizioni di pagamento
039300140605       // aggancio anagrafica provvisoria   TFCLP00F con numero trattativa
039400140605       chain(n) (acokut:acokcc:ita55nrv) tfclp00f ;
039500140605
039600140605       If  %found(tfclp00f) ;
039700140605
039800140605       //?salvo lo stato del credito di TFACO
039900140605           sav_CLPconP = CLPcon;
040000140605
040100140605           clpksc = ita55nwksc ;
040200140605       //  se non c'è cnclp imposto il commerciale della visita e non + quello
040300140605       //  dell'anagrafica provvisoria xchè potrebbe essere incongruente con il
040400140605       //  p.o. del commerciale della visita
040500140605           if not %found(cnclp00f) ;
040600140605              clpage = ita55cmm ;
040700140605              clpnrj = knraz ;
040800140605         //?il nuovo codice lo devo creare senza stato del credito
040900140605         //?                             e senza causale blocco
041000140605              clear CLPcon;
041100140605              clear CLPnar;
041200170317         //?se in TFACO c'è uno stato del credito
041300170317         //?e la frequenza fattura è settimanale
041400170317         //?quest'ultima la imposto vuota
041500170317              IF  %subst(sav_CLPconP:2:2) <> *blanks and CLPfft = 1;
041600170317                clear CLPfft;
041700170317              ENDIF;
041800140605              write cnclp000 ;
041900140605           else ;
042000140605         //?devo mantenere il vecchio stato del credito e
042100140605         //?               la vecchia causale del blocco servizio
042200170317         //?               la vecchia frequenza fattura
042300140605              CLPcon = sav_CLPcon;
042400140605              CLPnar = sav_CLPnar;
042500170317              CLPfft = sav_CLPfft;
042600140605              update cnclp000 ;
042700140605           endif ;
042800140605       endif ;
042900100121
043000100503       // aggancio anagrafica provvisoria   TFIND00F con numero trattativa
043100100503       chain (acokut:acokcc:ita55nrv) tfind00f ;
043200100121       // se modificati i dati del pagamento li registro prima nell'anagrafica
043300100121       // provvisoria
043400100121
043500100121       If  %found(tfind00f) ;
043600100520           If Ita55pag = '1' ;
043700100503              indabi = ita55abi ;
043800100503              indcab = ita55cab ;
043900100505              indcdp = ita55cdp ;
044000100122              update tfind000 ;
044100100503              chain(n) (acokut:acokcc:ita55nrv) tfind00f ;
044200100122           endif ;
044300100504           indksc = ita55nwksc ;
044400100122       // Partita Iva
044500100503           indiva = ita55iva ;
044600100122       // Codice Fiscale
044700100503           indcdf = ita55cdf;
044800100122
044900100122           if not %found(cnind00f) ;
045000100122              indnrj = knraz ;
045100140605         //?il nuovo codice lo devo creare senza blocco pagamenti
045200140605              %subst(INDopz:1:1) = '0';
045300170317         //?senza dati relativi all'esenzione IVA
045400170317              clear INDnpr;
045500170317              clear INDdpr;
045600170317              clear INDnpn;
045700140605         //?se in TFACO c'è uno stato del credito le condizioni di
045800140605         //?pagamento le imposto vuote
045900140605              IF  %subst(sav_CLPconP:2:2) <> *blanks;
046000140605                %subst(INDcdp:4:3) = *blanks;
046100140605              ENDIF;
046200100122              write cnind000 ;
046300100122           else ;
046400140605         //?devo mantenere il vecchio blocco pagamenti
046500140605              %subst(INDopz:1:1) = %subst(sav_INDopz:1:1);
046600140605         //?se c'è anche solo 1 stato del credito devo mantenere le
046700140605         //?vecchie condizioni di pagamento
046800140605              IF  %subst(sav_CLPcon:2:2) <> *blanks or
046900140605                  %subst(sav_CLPconP:2:2) <> *blanks;
047000140605                %subst(INDcdp:4:3) = %subst(sav_INDcdp:4:3);
047100140605              ENDIF;
047200170317         //?devo mantenere i vecchi dati relativi all'esensione IVA
047300170317              INDnpr = sav_INDnpr;
047400170317              INDdpr = sav_INDdpr;
047500170317              INDnpn = sav_INDnpn;
047600100122              update cnind000 ;
047700100122           endif;
047800100122       endif ;
047900100122
048000100503       // aggancio anagrafica provvisoria   TFCLS00F con numero trattativa
048100100916
048200100916       // mi memorizzo i codici pagamenti vecchi
048300100916          clear Woldtic ;
048400100916          clear Wnewtic ;
048500100916
048600100916       If %found(fncls01l);
048700100916          Woldtic = clstic ;
048800100916       endif ;
048900100916
049000100503       chain(n) (ita55nrv) tfcls01l ;
049100100122
049200100122       If  %found(tfcls01l) ;
049300100916           Wnewtic = clstic ;
049400100504           clsksc = ita55nwksc ;
049500100122           if not %found(fncls01l) ;
049600100122              write fncls000 ;
049700100122           else ;
049800100122              update fncls000 ;
049900100122           endif ;
050000100122
050100100122        Endif ;
050200100916
050300100916       // aggancio anagrafica provvisoria   TFCBA00F con numero trattativa
050400100916       // e per ogni tipo pagamento per verificare l'esistenza o meno dei
050500100916       // record. Se record non presente deleto FNCBA con TRULIBAN
050600100916       // mentre se c'è faccio update su FNCBA con TRULIBAN
050700100916
050800100916       // DANNI
050900100916
051000100916       chain(n) (ita55nrv:'DN') tfcba01l ;
051100100916
051200100916       If  not %found(tfcba01l) ;
051300100916           rqsOpCode = 'DELETE' ;
051400100916           CBAPAG = 'DN' ;
051500100916           CLEAR  CBAABI ;
051600100916           CLEAR  CBABIC ;
051700100916           exsr coordinate ;
051800100916       else ;
051900100916           If %found(cnaco00f) ;
052000100916              rqsOpCode = 'UPDATE' ;
052100100916           else ;
052200100916              rqsOpCode = 'INSERT' ;
052300100916           endif;
052400100916           exsr coordinate ;
052500100916       endif;
052600100916
052700100916       // NOTE ACCREDITO
052800100916
052900100916       chain(n) (ita55nrv:'NA') tfcba01l ;
053000100916
053100100916       If  not %found(tfcba01l) ;
053200100916           rqsOpCode = 'DELETE' ;
053300100916           CBAPAG = 'AN' ;
053400100916           CLEAR  CBAABI ;
053500100916           CLEAR  CBABIC ;
053600100916           exsr Coordinate ;
053700100916       else ;
053800100916           If %found(cnaco00f) ;
053900100916              rqsOpCode = 'UPDATE' ;
054000100916           else ;
054100100916              rqsOpCode = 'INSERT' ;
054200100916           endif;
054300100916           exsr Coordinate    ;
054400100916       endif;
054500100916
054600100122
054700100510       // Memorizzo l'immagine seguente per storicizzare le variazioni
054800100510           exsr ScriviACV ;
054900100122        Endif ;
055000100503
055100100503       ENDSR;
055200100503
055300100916       //--------------------------------------------------------------
055400100916       //?Operazioni relative alle coordinate bancarie.
055500100916       //--------------------------------------------------------------
055600100916       BEGSR Coordinate;
055700100916
055800100916         clear rpyEsito;
055900100916
056000100916           clear TrulIbanI0;
056100100916           clear TrulIbanO0;
056200100916           TrulIbanI0.KSC = ita55nwksc ;
056300100916           TrulIbanI0.PAG = cbapag ;
056400100916           TrulIbanI0.IBAN  = cbaiban ;
056500100916           TrulIbanI0.BIC   = cbabic ;
056600100916           If cbapag = 'DN' ;
056700100916              TrulIbanI0.CODPO = %subst(Woldtic:1:1);
056800100916              TrulIbanI0.CODPN = %subst(Wnewtic:1:1);
056900100916           endif ;
057000100916           If cbapag = 'NA' ;
057100100916              TrulIbanI0.CODPO = %subst(Woldtic:2:1);
057200100916              TrulIbanI0.CODPN = %subst(Wnewtic:2:1);
057300100916           endif ;
057400100916           TrulIbanI0.PGM   = 'TNTA56R' ;
057500100916           rqsFormatName = 'TRULIBANI0';
057600100916           rpyFormatName = 'TRULIBANO0';
057700100916           rqsDataSize   = %size(TrulIbanI0);
057800100916           rpyDataSize   = %size(TrulIbanO0);
057900100916           TrulIbanR (rqsOpCode:rpyEsito:
058000100916                      rqsFormatName:TrulIbanI0:rqsDataSize:
058100100916                      rpyFormatname:TrulIbanO0:rpyDataSize);
058200100916
058300100916       ENDSR;
058400100504       //--------------------------------------------------------------
058500100504       //?Scrittura Anagrafiche da CNACO NUOVA
058600100504       //--------------------------------------------------------------
058700100504       BEGSR Sr_DaCNACO;
058800100504
058900100504       // copio da cnaco cliente della trattativa a nuovo cliente
059000100504             chain(n) (acokut:acokcc:ita55ksc) cnaco00f ;
059100100504             If %found(cnaco00f) ;
059200100504                acoksc = ita55nwksc ;
059300100504                acoftt = '3' ;
059400100504                acoflt = %dec(%subst(%EDITC(ita55nwKsc:'X'):1:3):3:0);
059500100504                clear acoftr ;
059600100504                clear acodtr ;
059700100504                acoduv = datacor6 ;
059800100504                nrjob = knraz ;
059900100504                write cnaco000 ;
060000100504                ota55new = '1';
060100100504             endif ;
060200100504
060300100504       // copio da cnind cliente della trattativa a nuovo cliente
060400100504             chain(n) (acokut:acokcc:ita55ksc) cnind00f ;
060500100504             If %found(cnind00f) ;
060600100520                If Ita55pag = '1' ;
060700100504                   indabi = ita55abi ;
060800100504                   indcab = ita55cab ;
060900100505                   indcdp = ita55cdp ;
061000100504                endif ;
061100100504                indksc = ita55nwksc ;
061200100504       // Partita Iva
061300100504                indiva = ita55iva ;
061400100504       // Codice Fiscale
061500100504                indcdf = ita55cdf;
061600100504
061700100504                indnrj = knraz ;
061800170320         //?Non devo mai riportare i dati relativi all'esenzione IVA
061900170320              clear INDnpr;
062000170320              clear INDdpr;
062100170320              clear INDnpn;
062200100504                write cnind000 ;
062300100504             endif ;
062400100504
062500100504       // copio da cnclp cliente della trattativa a nuovo cliente
062600100504       chain(n) (acokut:acokcc:ita55ksc) cnclp00f ;
062700100504
062800100504       If  %found(cnclp00f) ;
062900100504           clpksc = ita55nwksc ;
063000100504           clpage = ita55cmm ;
063100100504           clpnrj = knraz ;
063200100504           write cnclp000 ;
063300100504       endif ;
063400100504
063500100504       // copio da fncls cliente della trattativa a nuovo cliente
063600100504       chain(n) (ita55ksc) fncls01l ;
063700100504
063800100504       If  %found(fncls01l) ;
063900100504           clsksc = ita55nwksc ;
064000100504           write fncls000 ;
064100100504       endif ;
064200100504
064300100915       // copio da fncba cliente della trattativa a nuovo cliente
064400100915       setll    (ita55ksc) fncba01l ;
064500100915       reade(n) (ita55ksc) fncba01l ;
064600100915
064700100915       dow  not %eof(fncba01l) ;
064800100915           cbaksc = ita55nwksc ;
064900100916           write tfcba000 ;
065000100916           reade(n) (ita55ksc) fncba01l ;
065100100916       enddo ;
065200100915
065300100510       // Memorizzo l'immagine seguente per storicizzare le variazioni
065400100510           exsr ScriviACV ;
065500100504
065600100504       ENDSR;
065700100504
065800100503       //--------------------------------------------------------------
065900100503       //?Aggiorno CNIND da dati a video
066000100503       //--------------------------------------------------------------
066100100503       BEGSR Sr_UPDIND ;
066200100503
066300100504           chain (acokut:acokcc:ita55nwksc) cnind00f ;
066400100122           If %found(cnind00f) ;
066500100122       // se modificate le condizioni di pagamento si aggiornano
066600100520              If Ita55pag = '1' ;
066700100503                 indabi = ita55abi ;
066800100503                 indcab = ita55cab ;
066900100505                 indcdp = ita55cdp ;
067000100503              endif ;
067100100503       // Partita Iva
067200100503              indiva = ita55iva ;
067300100503       // Codice Fiscale
067400100503              indcdf = ita55cdf;
067500100503              update cnind000 ;
067600100503           endif;
067700100122
067800100121       ENDSR ;
067900100122
068000100122       //--------------------------------------------------------------
068100100122       //?Scrittura del file delle variazioni Anagrafiche
068200100122       //--------------------------------------------------------------
068300100122       BEGSR Scriviacv ;
068400100122
068500100122       Clear Tibs73ds ;
068600100122       ibs73pru=sdsuse ;
068700100122       ibs73noj=sdsjob ;
068800100506       ibs73pgm='TNTA56R';
068900100122       ibs73immag='D';
069000100122
069100100122       //  Immissione
069200100504       If  Ota55new    = '1' ;
069300100122           ibs73cta='O' ;
069400100122           TIBS73R (tibs73ds:cnaco_73:cnind_73:cnclp_73:fncls_73);
069500100122       //  Manutenzione
069600100122       else;
069700100122           ibs73cta='M' ;
069800100122           TIBS73R (tibs73ds:cnaco_73:cnind_73:cnclp_73:fncls_73);
069900100122       endif;
070000100122
070100100122       wtibs73 = *On ;
070200100122
070300100122       ENDSR ;
070400100424
070500100424       //--------------------------------------------------------------
070600100503       //?Scrittura Note Rubrica da trattativa
070700100424       //--------------------------------------------------------------
070800100503       BEGSR Sr_Datfntct;
070900100424
071000100503         savnk1 = %editc(dutkci: 'X') + %editc(ita55nrv:'X');
071100100425       // cerco le note relative alla trattativa da duplicare sul cliente
071200100504         exec sql declare c1 cursor for select *
071300100425         from tfntc01l where ntcapl = 'T' and ntcnk1 = :savnk1
071400100427         and ntctnt <> '10'
071500100425         order by ntcapl, ntcnk1, ntcnk2, ntctnt
071600100425         for read only  ;
071700100425
071800100504         exec sql open c1 ;
071900100425
072000100504         exec sql fetch c1 into :tfntcds ;
072100100425
072200100425         dow Sqlcod = 0;
072300100425
072400100425            If Ntcflt <> 'A' ;
072500100425    c          xx = 1 ;
072600100425               xx = %lookup(ntctnt:tno) ;
072700100425               If xx > 0  and tdup(xx) = 'S';
072800100425                   ntcapl = 'C' ;
072900100504                   ntcnk1 = %editc(dutkci: 'X') + %editc(ita55nwksc:'X');
073000100425                   ntcntr = datacor6;
073100100425                   write tfntc ;
073200100425    2          EndIf ;
073300100425    2       EndIf ;
073400100425
073500100504          exec sql fetch c1 into :tfntcds ;
073600100425
073700100425    1    enddo;
073800100425
073900100504         exec sql close c1 ;
074000100425
074100100424       ENDSR ;
074200100503
074300100503       //--------------------------------------------------------------
074400100503       //?Scrittura Note Rubrica da cliente
074500100503       //--------------------------------------------------------------
074600100503       BEGSR Sr_Datfntcc;
074700100503
074800100503         savnk1 = %editc(dutkci: 'X') + %editc(ita55ksc:'X');
074900100916       // cerco le note relative al cliente trattativa da duplicare sul cliente
075000100503         exec sql declare c2 cursor for select *
075100100506         from tfntc01l where ntcapl = 'C' and ntcnk1 = :savnk1
075200100503         and ntctnt <> '10'
075300100503         order by ntcapl, ntcnk1, ntcnk2, ntctnt
075400100503         for read only  ;
075500100503
075600100503         exec sql open c2 ;
075700100503
075800100503         exec sql fetch c2 into :tfntcds ;
075900100503
076000100503         dow Sqlcod = 0;
076100100503
076200100503            If Ntcflt <> 'A' ;
076300100503    c          xx = 1 ;
076400100503               xx = %lookup(ntctnt:tno) ;
076500100503               If xx > 0  and tdup(xx) = 'S';
076600100503                   ntcapl = 'C' ;
076700100504                   ntcnk1 = %editc(dutkci: 'X') + %editc(ita55nwksc:'X');
076800100503                   ntcntr = datacor6;
076900100503                   write tfntc ;
077000100503    2          EndIf ;
077100100503    2       EndIf ;
077200100503
077300100503          exec sql fetch c2 into :tfntcds ;
077400100503
077500100503    1    enddo;
077600100503
077700100503         exec sql close c2 ;
077800100503
077900100503       ENDSR ;
078000100916
078100100916       //--------------------------------------------------------------
078200100916       //?Scrittura luoghi  da cliente
078300100916       //--------------------------------------------------------------
078400100916       BEGSR Sr_Dafnspec;
078500100916
078600100916       // cerco i luoghi relative al cliente da duplicare sul cliente
078700100916           exec sql declare s1 cursor for select *
078800100916           from fnspe01l where spefls = 'L' and specli = :ita55ksc
078900100916           for read only  ;
079000100916
079100100916           exec sql open s1 ;
079200100916
079300100916           exec sql fetch s1 into :fnspeds ;
079400100916
079500100916           dow Sqlcod = 0;
079600100916
079700100916              If Speflg =  ' ' ;
079800100916
079900100916    c            xx = 1 ;
080000100916                 xx = %lookup(specod:tluo) ;
080100100916                 If xx > 0  and tufi(xx) = 'S';
080200100916
080300100916                     specli = ita55nwksc;
080400100916                     spedtr = datacor6;
080500100916                     speftr = ' ';
080600100916                     write fnspe000 ;
080700100916
080800100916           // cerco i suoi fnsp2
080900100916                     setll (ita55ksc:specod) fnsp201l;
081000100916                     reade (ita55ksc:specod) fnsp201l;
081100100916                     dow not %eof(fnsp201l) ;
081200100916                         if Sp2flg <> 'A' ;
081300100916                            sp2cli = ita55nwksc ;
081400100916                            write Fnsp2000 ;
081500100916                         endif ;
081600110222                       reade (ita55ksc:specod) fnsp201l;
081700100916                     Enddo ;
081800100916                 endif  ;
081900100916
082000100916    2         EndIf ;
082100100916
082200100916            exec sql fetch s1 into :fnspeds ;
082300100916
082400100916    1     enddo;
082500100916
082600100916            exec sql close s1 ;
082700100916
082800100916       ENDSR ;
082900100115       //--------------------------------------------------------------
083000100115       //?Operazioni iniziali.
083100100115       //--------------------------------------------------------------
083200100115       BEGSR RoutInz;
083300100115
083400100115         // Reperimento dati job
083500100115         exsr DatiJob;
083600100115
083700100115       // Reperisco data corrente
083800100115             data_iso = (%date());
083900100122             datacor8 = %dec(data_iso);
084000100125             data_ymd = (%date());
084100100121             datacor6 = %dec(data_ymd) ;
084200100122
084300100122        //  Carico schiera con tabella 1T (tipo nota)
084400100122             Clear   xx;
084500100505             TBLkut = 1 ;
084600100122             TBLcod = '1T';
084700100122             setll (TBLkut : TBLcod) TABEL;
084800100122             reade (TBLkut : TBLcod) TABEL;
084900100122             dow not %eof(TABEL00F);
085000100122                 If tblflg = ' ' and tblkey <> *blanks ;
085100100122                    xx = xx + 1 ;
085200100122                    ds1t = tbluni ;
085300100122                    tno(xx) = tblkey ;
085400100122                    tdup(xx) = §1tdup;
085500100122                    ttdp(xx) = §1ttdp;
085600100122                 endif ;
085700100122                 reade (TBLkut : TBLcod) TABEL;
085800100122             EndDo ;
085900100115
086000100916        //  Carico schiera con tabella 4L (Lughi)
086100100916             Clear   xx;
086200100916             TBLkut = 1 ;
086300100916             TBLcod = '4L';
086400100916             setll (TBLkut : TBLcod) TABEL;
086500100916             reade (TBLkut : TBLcod) TABEL;
086600100916             dow not %eof(TABEL00F);
086700100916                 If tblflg = ' ' and tblkey <> *blanks ;
086800100916                    xx = xx + 1 ;
086900100917                    ds4l = tbluni ;
087000100916                    tluo(xx) = tblkey ;
087100100916                    tufi(xx) = §4Lufi;
087200100916                 endif ;
087300100916                 reade (TBLkut : TBLcod) TABEL;
087400100916             EndDo ;
087500100916
087600100506             acokut = 1 ;
087700100506             acokcc = dutkci ;
087800100422
087900100115       ENDSR;
088000100115
088100100115       //--------------------------------------------------------------
088200100115       //?Reperimento Dati del job (Utente/Operativi).
088300100115       //--------------------------------------------------------------
088400100115       BEGSR DatiJob;
088500100115
088600100115         in(E) §AzUte;
088700100115         if NOT %error;
088800100115           in(E) §DatiUte;
088900100115         endif;
089000100115         if %error or RSut = *blanks;
089100100115           clear TIBS34ds;
089200100115           tibs34r(tibs34ds);
089300100115           in §AzUte;
089400100115           in §DatiUte;
089500100115         endif;
089600100115
089700100115       ENDSR;
089800100108
089900080206       //--------------------------------------------------------------
090000080206       //?Operazioni finali.
090100080206       //--------------------------------------------------------------
090200080206       BEGSR RoutEnd;
090300090806
090400100504
090500100504       If wtibs73 = *On  ;
090600100504          ibs73tla = 'C' ;
090700100504          TIBS73R (tibs73ds:cnaco_73:cnind_73:cnclp_73:fncls_73);
090800100504       EndIf ;
090900080206         *inLR = *on;
091000080206         return;
091100080206
091200080206       ENDSR;
091300080206
091400080206      /end-free
