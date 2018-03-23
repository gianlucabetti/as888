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
013400140605     d sav_CLPnar      s                   like(CLPnar)
013500140605     d sav_CLPcon      s                   like(CLPcon)
013600140605     d sav_CLPconP     s                   like(CLPcon)
013700080206
013800090401      // - Campi di comodo data
013900090401     d  data_eur       s               d   Datfmt(*eur)
014000090401     d  data_iso       s               d   Datfmt(*iso)
014100090401     d  data_dmy       s               d   Datfmt(*dmy)
014200090811     d  data_ymd       s               d   datfmt(*ymd)
014300090401
014400080206      // - Campi di comodo
014500090406     d wAbi            s                   like(UTEaut)  inz
014600100122     d  datacor8       s              8  0
014700100122     d  datacor6       s              6  0
014800100504     d savnk1          s                   like(ntcnk1)
014900100504     d Wtibs73         s               n   inz(*off)
015000100916     d Woldtic         s                   like(clstic)
015100100916     d Wnewtic         s                   like(clstic)
015200100916     d rqsformatname...
015300100916     d                 S             10A
015400100916     d rqsdatasize...
015500100916     d                 S             10I 0
015600100916     d rqsopcode...
015700100916     d                 S             10A
015800100916     d rpyformatname...
015900100916     d                 S             10A
016000100916     d rpydatasize...
016100100916     d                 S             10I 0
016200100916     d rpyesito...
016300100916     d                 S             10I 0
016400090323     d
016500080208
016600090508      //---------------------------------------------------------------
016700090508      //?Definizione procedure usate.
016800090508      //---------------------------------------------------------------
016900080206
017000100420
017100100420      //---------------------------------------------------------------
017200100420      //?prototipi
017300100420      //---------------------------------------------------------------
017400100916
017500100916      // - Coordinate bancarie
017600100916     d TrulIbanR       pr                  extpgm('TRULIBANR')
017700100916     d  rqsOpCode                    10a   const
017800100916     d  rpyEsito                     10i 0
017900100916     d  rqsFormatName                10a   const
018000100916     d  rqsData                     256a   options(*varsize)
018100100916     d  rqsDataSize                  10i 0 const
018200100916     d  rpyFormatName                10a   const
018300100916     d  rpyData                     256a   options(*varsize)
018400100916     d  rpyDataSize                  10i 0 const
018500100916
018600100420
018700080626      /copy gaitrasrc/srcprotopr,tibs34r
018800100119      /copy gaitrasrc/srcprotopr,tibs69r
018900100503      /copy gaitrasrc/srcprotopr,tibs73r
019000090508
019100080206      //---------------------------------------------------------------
019200080206      //?Definizione key-list.
019300080206      //---------------------------------------------------------------
019400080206
019500080206
019600080206      //---------------------------------------------------------------
019700080206      //?Riepilogo indicatori.
019800080206      //---------------------------------------------------------------
019900100503
020000080206      //---------------------------------------------------------------
020100080206
020200080206      //---------------------------------------------------------------
020300080206      //?M A I N - L I N E
020400080206      //---------------------------------------------------------------
020500080206
020600080206     c     *Entry        plist
020700080206     c                   parm                    KPJBA
020800100503     c                   parm                    tnta55ds
020900080206
021000080206      /free
021100080206
021200080206       // Operazioni iniziali
021300080206       exsr RoutInz;
021400090601
021500100503       SELECT ;
021600100503
021700100503       // se prima convalida di trattativa nuova
021800100503       when ita55prima = 'SI' and Ita55tpv = 'N' ;
021900100503            exsr sr_daTFACO  ;
022000100503            exsr sr_daTFNTCT ;
022100100503
022200100503       // se non è prima convalida di trattativa nuova ksc nuovo OPPURE NO
022300100503       when ita55prima <> 'SI' and Ita55tpv = 'N' ;
022400100503         // verifico il nuovo cnaco
022500100504             acoksc = ita55nwksc ;
022600100503             setll  (acokut:acokcc:acoksc) Cnaco00f ;
022700100503             // esiste cnaco aggiorno solo CNIND
022800100503             If %equal ;
022900100503                exsr sr_updIND  ;
023000100503             else ;
023100100503                exsr sr_daTFACO;
023200100503                exsr sr_daTFNTCT;
023300100503             endif ;
023400100503
023500110114       // se prima convalida di trattativa non nuova con o senza tfaco
023600150707       when ita55prima = 'SI' and Ita55tpv <> 'N' ;
023700100519         // verifico il nuovo cnaco
023800100519             acokut = 1 ;
023900100519             acokcc = dutkci ;
024000100519             acoksc = ita55nwksc ;
024100100519             setll  (acokut:acokcc:acoksc) Cnaco00f ;
024200100916             // esiste cnaco aggiorno solo CNIND se non ho tfaco
024300100519             If %equal ;
024400100916                acokut = 1 ;
024500100916                acokcc = dutkci ;
024600100916                acoksc = ita55nrv ;
024700100916                setll  (acokut:acokcc:acoksc) Tfaco00f ;
024800100916             // non esiste tfaco aggiorno cnind
024900100916                If not %equal(tfaco00f)  ;
025000100916                   exsr sr_updIND  ;
025100100916                else ;
025200100916             // esiste tfaco aggiorno tutta l'anagrafica
025300100916                   exsr sr_daTFACO ;
025400100916              // non scrivo la rubrica in quanto già appoggiata al cliente
025500100916                endif ;
025600100519             else ;
025700100519                acokut = 1 ;
025800100519                acokcc = dutkci ;
025900100519                acoksc = ita55nrv ;
026000100519                setll  (acokut:acokcc:acoksc) Tfaco00f ;
026100100519                If %equal ;
026200100519                   exsr sr_daTFACO ;
026300100916              // scrivo la rubrica copiandolo dal cliente trattativa
026400100916                   exsr sr_daTFNTCC;
026500100917              // scrivo i luoghi copiandoli dal cliente trattativa
026600100917                   exsr sr_daFNSPEC;
026700100519                else ;
026800100519                   exsr sr_daCNACO ;
026900100519                   exsr sr_daTFNTCC;
027000100916                   exsr sr_daFNSPEC;
027100100519                   exsr sr_updIND ;
027200100519                endif ;
027300100916             endif ;
027400100916
027500100510
027600110114       // se non è prima convalida di trattativa non Nuova ksc nuovo OPPURE NO
027700150707       when ita55prima <> 'SI' and Ita55tpv <> 'N' ;
027800100503         // verifico il nuovo cnaco
027900100503             acokut = 1 ;
028000100503             acokcc = dutkci ;
028100100504             acoksc = ita55nwksc ;
028200100503             setll  (acokut:acokcc:acoksc) Cnaco00f ;
028300100503             // esiste cnaco aggiorno solo CNIND
028400100503             If %equal ;
028500100503                exsr sr_updIND  ;
028600100503             else ;
028700100503             // Verifico se esiste tfaco
028800100503                acoksc = ita55nrv ;
028900100503                setll  (acokut:acokcc:acoksc) Tfaco00f ;
029000100503                If %equal ;
029100100503                   exsr sr_daTFACO ;
029200100503              // scrivo la rubrica del cliente trattaiva nel nuovo cliente
029300100503                   exsr sr_daTFNTCC;
029400100916              // scrivo i luoghi del cliente trattativa nel nuovo cliente
029500100916                   exsr sr_daFNSPEC;
029600100503                else ;
029700100503              // copio da cnaco a cnaco e da tfntc a tfntc con codice cliente
029800100503                   exsr sr_daCNACO ;
029900100503                   exsr sr_daTFNTCC;
030000100916                   exsr sr_daFNSPEC;
030100100503                endif ;
030200100503             endif ;
030300100503
030400100503       ENDSL ;
030500080206       // Operazioni finali
030600080206       exsr RoutEnd;
030700080206
030800100121       //--------------------------------------------------------------
030900100503       //?Scrittura Anagrafiche da TFACO NUOVA
031000100121       //--------------------------------------------------------------
031100100503       BEGSR Sr_DaTFACO;
031200140605
031300140605         clear sav_ACOabl;
031400140605         clear sav_INDopz;
031500140605         clear sav_INDcdp;
031600140605         clear sav_CLPnar;
031700140605         clear sav_CLPcon;
031800140605         clear sav_CLPconP;
031900100121
032000100504       acoksc = ita55nwksc ;
032100100503       // verifico se esiste anagrafica con il nuovo codice cliente
032200100503       chain (acokut:acokcc:acoksc) cnaco00f ;
032300100503       chain (acokut:acokcc:acoksc) cnind00f ;
032400100503       chain (acokut:acokcc:acoksc) cnclp00f ;
032500100503       chain (acoksc) fncls01l ;
032600100121
032700100121       // se già trovato un'anagrafica esistente memorizzo l'immagine
032800100121       // precedente per storicizzare le variazioni
032900100121
033000100121       If %found(cnaco00f) ;
033100100121          clear tibs73ds ;
033200100121          ibs73immag = 'P' ;
033300100121          TIBS73R (tibs73ds:cnaco_73:cnind_73:cnclp_73:fncls_73);
033400100121          wtibs73 = *On  ;
033500140605         //?salvo il blocco servizio
033600140605          sav_ACOabl = ACOabl;
033700100121       endif;
033800140605
033900140605       //?salvo la causale del blocco servizio e
034000140605       //?      lo stato del credito
034100140605       IF  %found(CNCLP00F);
034200140605         sav_CLPnar = CLPnar;
034300140605         sav_CLPcon = CLPcon;
034400140605       ENDIF;
034500140605
034600140605       //?salvo il blocco pagamenti e
034700140605       //?      le condizioni di pagamento
034800140605       IF  %found(CNIND00F);
034900140605         sav_INDopz = INDopz;
035000140605         sav_INDcdp = INDcdp;
035100140605       ENDIF;
035200100121
035300100503       // aggancio anagrafica provvisoria   TFACO00F  con numero trattativa
035400100503       chain(n) (acokut:acokcc:ita55nrv) tfaco00f ;
035500100121
035600100121       If  %found(tfaco00f) ;
035700100504           acoksc = ita55nwksc ;
035800100121           acoftt = '3' ;
035900100504           acoflt = %dec(%subst(%EDITC(ita55nwKsc:'X'):1:3):3:0);
036000100121           clear acoftr ;
036100100121           clear acodtr ;
036200100121           acoduv = datacor6 ;
036300100121          if not %found(cnaco00f) ;
036400100121             nrjob = knraz ;
036500140605         //?il nuovo codice lo devo creare sbloccato
036600140605             clear ACOabl;
036700100121             write cnaco000 ;
036800100504             ota55new = '1';
036900100121          else ;
037000140605         //?devo mantenere il vecchio blocco servizio
037100140605             ACOabl = sav_ACOabl;
037200100121             update cnaco000 ;
037300100504             ota55new = '0';
037400100121          endif ;
037500140605
037600140605       //?Sposto qua l'aggiornamento del CNCLP in modo da poter
037700140605       //?verificare lo stato del credito, che serve poi per le
037800140605       //?condizioni di pagamento
037900140605       // aggancio anagrafica provvisoria   TFCLP00F con numero trattativa
038000140605       chain(n) (acokut:acokcc:ita55nrv) tfclp00f ;
038100140605
038200140605       If  %found(tfclp00f) ;
038300140605
038400140605       //?salvo lo stato del credito di TFACO
038500140605           sav_CLPconP = CLPcon;
038600140605
038700140605           clpksc = ita55nwksc ;
038800140605       //  se non c'è cnclp imposto il commerciale della visita e non + quello
038900140605       //  dell'anagrafica provvisoria xchè potrebbe essere incongruente con il
039000140605       //  p.o. del commerciale della visita
039100140605           if not %found(cnclp00f) ;
039200140605              clpage = ita55cmm ;
039300140605              clpnrj = knraz ;
039400140605         //?il nuovo codice lo devo creare senza stato del credito
039500140605         //?                             e senza causale blocco
039600140605              clear CLPcon;
039700140605              clear CLPnar;
039800140605              write cnclp000 ;
039900140605           else ;
040000140605         //?devo mantenere il vecchio stato del credito e
040100140605         //?               la vecchia causale del blocco servizio
040200140605              CLPcon = sav_CLPcon;
040300140605              CLPnar = sav_CLPnar;
040400140605              update cnclp000 ;
040500140605           endif ;
040600140605       endif ;
040700100121
040800100503       // aggancio anagrafica provvisoria   TFIND00F con numero trattativa
040900100503       chain (acokut:acokcc:ita55nrv) tfind00f ;
041000100121       // se modificati i dati del pagamento li registro prima nell'anagrafica
041100100121       // provvisoria
041200100121
041300100121       If  %found(tfind00f) ;
041400100520           If Ita55pag = '1' ;
041500100503              indabi = ita55abi ;
041600100503              indcab = ita55cab ;
041700100505              indcdp = ita55cdp ;
041800100122              update tfind000 ;
041900100503              chain(n) (acokut:acokcc:ita55nrv) tfind00f ;
042000100122           endif ;
042100100504           indksc = ita55nwksc ;
042200100122       // Partita Iva
042300100503           indiva = ita55iva ;
042400100122       // Codice Fiscale
042500100503           indcdf = ita55cdf;
042600100122
042700100122           if not %found(cnind00f) ;
042800100122              indnrj = knraz ;
042900140605         //?il nuovo codice lo devo creare senza blocco pagamenti
043000140605              %subst(INDopz:1:1) = '0';
043100140605         //?se in TFACO c'è uno stato del credito le condizioni di
043200140605         //?pagamento le imposto vuote
043300140605              IF  %subst(sav_CLPconP:2:2) <> *blanks;
043400140605                %subst(INDcdp:4:3) = *blanks;
043500140605              ENDIF;
043600100122              write cnind000 ;
043700100122           else ;
043800140605         //?devo mantenere il vecchio blocco pagamenti
043900140605              %subst(INDopz:1:1) = %subst(sav_INDopz:1:1);
044000140605         //?se c'è anche solo 1 stato del credito devo mantenere le
044100140605         //?vecchie condizioni di pagamento
044200140605              IF  %subst(sav_CLPcon:2:2) <> *blanks or
044300140605                  %subst(sav_CLPconP:2:2) <> *blanks;
044400140605                %subst(INDcdp:4:3) = %subst(sav_INDcdp:4:3);
044500140605              ENDIF;
044600100122              update cnind000 ;
044700100122           endif;
044800100122       endif ;
044900100122
045000100503       // aggancio anagrafica provvisoria   TFCLS00F con numero trattativa
045100100916
045200100916       // mi memorizzo i codici pagamenti vecchi
045300100916          clear Woldtic ;
045400100916          clear Wnewtic ;
045500100916
045600100916       If %found(fncls01l);
045700100916          Woldtic = clstic ;
045800100916       endif ;
045900100916
046000100503       chain(n) (ita55nrv) tfcls01l ;
046100100122
046200100122       If  %found(tfcls01l) ;
046300100916           Wnewtic = clstic ;
046400100504           clsksc = ita55nwksc ;
046500100122           if not %found(fncls01l) ;
046600100122              write fncls000 ;
046700100122           else ;
046800100122              update fncls000 ;
046900100122           endif ;
047000100122
047100100122        Endif ;
047200100916
047300100916       // aggancio anagrafica provvisoria   TFCBA00F con numero trattativa
047400100916       // e per ogni tipo pagamento per verificare l'esistenza o meno dei
047500100916       // record. Se record non presente deleto FNCBA con TRULIBAN
047600100916       // mentre se c'è faccio update su FNCBA con TRULIBAN
047700100916
047800100916       // DANNI
047900100916
048000100916       chain(n) (ita55nrv:'DN') tfcba01l ;
048100100916
048200100916       If  not %found(tfcba01l) ;
048300100916           rqsOpCode = 'DELETE' ;
048400100916           CBAPAG = 'DN' ;
048500100916           CLEAR  CBAABI ;
048600100916           CLEAR  CBABIC ;
048700100916           exsr coordinate ;
048800100916       else ;
048900100916           If %found(cnaco00f) ;
049000100916              rqsOpCode = 'UPDATE' ;
049100100916           else ;
049200100916              rqsOpCode = 'INSERT' ;
049300100916           endif;
049400100916           exsr coordinate ;
049500100916       endif;
049600100916
049700100916       // NOTE ACCREDITO
049800100916
049900100916       chain(n) (ita55nrv:'NA') tfcba01l ;
050000100916
050100100916       If  not %found(tfcba01l) ;
050200100916           rqsOpCode = 'DELETE' ;
050300100916           CBAPAG = 'AN' ;
050400100916           CLEAR  CBAABI ;
050500100916           CLEAR  CBABIC ;
050600100916           exsr Coordinate ;
050700100916       else ;
050800100916           If %found(cnaco00f) ;
050900100916              rqsOpCode = 'UPDATE' ;
051000100916           else ;
051100100916              rqsOpCode = 'INSERT' ;
051200100916           endif;
051300100916           exsr Coordinate    ;
051400100916       endif;
051500100916
051600100122
051700100510       // Memorizzo l'immagine seguente per storicizzare le variazioni
051800100510           exsr ScriviACV ;
051900100122        Endif ;
052000100503
052100100503       ENDSR;
052200100503
052300100916       //--------------------------------------------------------------
052400100916       //?Operazioni relative alle coordinate bancarie.
052500100916       //--------------------------------------------------------------
052600100916       BEGSR Coordinate;
052700100916
052800100916         clear rpyEsito;
052900100916
053000100916           clear TrulIbanI0;
053100100916           clear TrulIbanO0;
053200100916           TrulIbanI0.KSC = ita55nwksc ;
053300100916           TrulIbanI0.PAG = cbapag ;
053400100916           TrulIbanI0.IBAN  = cbaiban ;
053500100916           TrulIbanI0.BIC   = cbabic ;
053600100916           If cbapag = 'DN' ;
053700100916              TrulIbanI0.CODPO = %subst(Woldtic:1:1);
053800100916              TrulIbanI0.CODPN = %subst(Wnewtic:1:1);
053900100916           endif ;
054000100916           If cbapag = 'NA' ;
054100100916              TrulIbanI0.CODPO = %subst(Woldtic:2:1);
054200100916              TrulIbanI0.CODPN = %subst(Wnewtic:2:1);
054300100916           endif ;
054400100916           TrulIbanI0.PGM   = 'TNTA56R' ;
054500100916           rqsFormatName = 'TRULIBANI0';
054600100916           rpyFormatName = 'TRULIBANO0';
054700100916           rqsDataSize   = %size(TrulIbanI0);
054800100916           rpyDataSize   = %size(TrulIbanO0);
054900100916           TrulIbanR (rqsOpCode:rpyEsito:
055000100916                      rqsFormatName:TrulIbanI0:rqsDataSize:
055100100916                      rpyFormatname:TrulIbanO0:rpyDataSize);
055200100916
055300100916       ENDSR;
055400100504       //--------------------------------------------------------------
055500100504       //?Scrittura Anagrafiche da CNACO NUOVA
055600100504       //--------------------------------------------------------------
055700100504       BEGSR Sr_DaCNACO;
055800100504
055900100504       // copio da cnaco cliente della trattativa a nuovo cliente
056000100504             chain(n) (acokut:acokcc:ita55ksc) cnaco00f ;
056100100504             If %found(cnaco00f) ;
056200100504                acoksc = ita55nwksc ;
056300100504                acoftt = '3' ;
056400100504                acoflt = %dec(%subst(%EDITC(ita55nwKsc:'X'):1:3):3:0);
056500100504                clear acoftr ;
056600100504                clear acodtr ;
056700100504                acoduv = datacor6 ;
056800100504                nrjob = knraz ;
056900100504                write cnaco000 ;
057000100504                ota55new = '1';
057100100504             endif ;
057200100504
057300100504       // copio da cnind cliente della trattativa a nuovo cliente
057400100504             chain(n) (acokut:acokcc:ita55ksc) cnind00f ;
057500100504             If %found(cnind00f) ;
057600100520                If Ita55pag = '1' ;
057700100504                   indabi = ita55abi ;
057800100504                   indcab = ita55cab ;
057900100505                   indcdp = ita55cdp ;
058000100504                endif ;
058100100504                indksc = ita55nwksc ;
058200100504       // Partita Iva
058300100504                indiva = ita55iva ;
058400100504       // Codice Fiscale
058500100504                indcdf = ita55cdf;
058600100504
058700100504                indnrj = knraz ;
058800100504                write cnind000 ;
058900100504             endif ;
059000100504
059100100504       // copio da cnclp cliente della trattativa a nuovo cliente
059200100504       chain(n) (acokut:acokcc:ita55ksc) cnclp00f ;
059300100504
059400100504       If  %found(cnclp00f) ;
059500100504           clpksc = ita55nwksc ;
059600100504           clpage = ita55cmm ;
059700100504           clpnrj = knraz ;
059800100504           write cnclp000 ;
059900100504       endif ;
060000100504
060100100504       // copio da fncls cliente della trattativa a nuovo cliente
060200100504       chain(n) (ita55ksc) fncls01l ;
060300100504
060400100504       If  %found(fncls01l) ;
060500100504           clsksc = ita55nwksc ;
060600100504           write fncls000 ;
060700100504       endif ;
060800100504
060900100915       // copio da fncba cliente della trattativa a nuovo cliente
061000100915       setll    (ita55ksc) fncba01l ;
061100100915       reade(n) (ita55ksc) fncba01l ;
061200100915
061300100915       dow  not %eof(fncba01l) ;
061400100915           cbaksc = ita55nwksc ;
061500100916           write tfcba000 ;
061600100916           reade(n) (ita55ksc) fncba01l ;
061700100916       enddo ;
061800100915
061900100510       // Memorizzo l'immagine seguente per storicizzare le variazioni
062000100510           exsr ScriviACV ;
062100100504
062200100504       ENDSR;
062300100504
062400100503       //--------------------------------------------------------------
062500100503       //?Aggiorno CNIND da dati a video
062600100503       //--------------------------------------------------------------
062700100503       BEGSR Sr_UPDIND ;
062800100503
062900100504           chain (acokut:acokcc:ita55nwksc) cnind00f ;
063000100122           If %found(cnind00f) ;
063100100122       // se modificate le condizioni di pagamento si aggiornano
063200100520              If Ita55pag = '1' ;
063300100503                 indabi = ita55abi ;
063400100503                 indcab = ita55cab ;
063500100505                 indcdp = ita55cdp ;
063600100503              endif ;
063700100503       // Partita Iva
063800100503              indiva = ita55iva ;
063900100503       // Codice Fiscale
064000100503              indcdf = ita55cdf;
064100100503              update cnind000 ;
064200100503           endif;
064300100122
064400100121       ENDSR ;
064500100122
064600100122       //--------------------------------------------------------------
064700100122       //?Scrittura del file delle variazioni Anagrafiche
064800100122       //--------------------------------------------------------------
064900100122       BEGSR Scriviacv ;
065000100122
065100100122       Clear Tibs73ds ;
065200100122       ibs73pru=sdsuse ;
065300100122       ibs73noj=sdsjob ;
065400100506       ibs73pgm='TNTA56R';
065500100122       ibs73immag='D';
065600100122
065700100122       //  Immissione
065800100504       If  Ota55new    = '1' ;
065900100122           ibs73cta='O' ;
066000100122           TIBS73R (tibs73ds:cnaco_73:cnind_73:cnclp_73:fncls_73);
066100100122       //  Manutenzione
066200100122       else;
066300100122           ibs73cta='M' ;
066400100122           TIBS73R (tibs73ds:cnaco_73:cnind_73:cnclp_73:fncls_73);
066500100122       endif;
066600100122
066700100122       wtibs73 = *On ;
066800100122
066900100122       ENDSR ;
067000100424
067100100424       //--------------------------------------------------------------
067200100503       //?Scrittura Note Rubrica da trattativa
067300100424       //--------------------------------------------------------------
067400100503       BEGSR Sr_Datfntct;
067500100424
067600100503         savnk1 = %editc(dutkci: 'X') + %editc(ita55nrv:'X');
067700100425       // cerco le note relative alla trattativa da duplicare sul cliente
067800100504         exec sql declare c1 cursor for select *
067900100425         from tfntc01l where ntcapl = 'T' and ntcnk1 = :savnk1
068000100427         and ntctnt <> '10'
068100100425         order by ntcapl, ntcnk1, ntcnk2, ntctnt
068200100425         for read only  ;
068300100425
068400100504         exec sql open c1 ;
068500100425
068600100504         exec sql fetch c1 into :tfntcds ;
068700100425
068800100425         dow Sqlcod = 0;
068900100425
069000100425            If Ntcflt <> 'A' ;
069100100425    c          xx = 1 ;
069200100425               xx = %lookup(ntctnt:tno) ;
069300100425               If xx > 0  and tdup(xx) = 'S';
069400100425                   ntcapl = 'C' ;
069500100504                   ntcnk1 = %editc(dutkci: 'X') + %editc(ita55nwksc:'X');
069600100425                   ntcntr = datacor6;
069700100425                   write tfntc ;
069800100425    2          EndIf ;
069900100425    2       EndIf ;
070000100425
070100100504          exec sql fetch c1 into :tfntcds ;
070200100425
070300100425    1    enddo;
070400100425
070500100504         exec sql close c1 ;
070600100425
070700100424       ENDSR ;
070800100503
070900100503       //--------------------------------------------------------------
071000100503       //?Scrittura Note Rubrica da cliente
071100100503       //--------------------------------------------------------------
071200100503       BEGSR Sr_Datfntcc;
071300100503
071400100503         savnk1 = %editc(dutkci: 'X') + %editc(ita55ksc:'X');
071500100916       // cerco le note relative al cliente trattativa da duplicare sul cliente
071600100503         exec sql declare c2 cursor for select *
071700100506         from tfntc01l where ntcapl = 'C' and ntcnk1 = :savnk1
071800100503         and ntctnt <> '10'
071900100503         order by ntcapl, ntcnk1, ntcnk2, ntctnt
072000100503         for read only  ;
072100100503
072200100503         exec sql open c2 ;
072300100503
072400100503         exec sql fetch c2 into :tfntcds ;
072500100503
072600100503         dow Sqlcod = 0;
072700100503
072800100503            If Ntcflt <> 'A' ;
072900100503    c          xx = 1 ;
073000100503               xx = %lookup(ntctnt:tno) ;
073100100503               If xx > 0  and tdup(xx) = 'S';
073200100503                   ntcapl = 'C' ;
073300100504                   ntcnk1 = %editc(dutkci: 'X') + %editc(ita55nwksc:'X');
073400100503                   ntcntr = datacor6;
073500100503                   write tfntc ;
073600100503    2          EndIf ;
073700100503    2       EndIf ;
073800100503
073900100503          exec sql fetch c2 into :tfntcds ;
074000100503
074100100503    1    enddo;
074200100503
074300100503         exec sql close c2 ;
074400100503
074500100503       ENDSR ;
074600100916
074700100916       //--------------------------------------------------------------
074800100916       //?Scrittura luoghi  da cliente
074900100916       //--------------------------------------------------------------
075000100916       BEGSR Sr_Dafnspec;
075100100916
075200100916       // cerco i luoghi relative al cliente da duplicare sul cliente
075300100916           exec sql declare s1 cursor for select *
075400100916           from fnspe01l where spefls = 'L' and specli = :ita55ksc
075500100916           for read only  ;
075600100916
075700100916           exec sql open s1 ;
075800100916
075900100916           exec sql fetch s1 into :fnspeds ;
076000100916
076100100916           dow Sqlcod = 0;
076200100916
076300100916              If Speflg =  ' ' ;
076400100916
076500100916    c            xx = 1 ;
076600100916                 xx = %lookup(specod:tluo) ;
076700100916                 If xx > 0  and tufi(xx) = 'S';
076800100916
076900100916                     specli = ita55nwksc;
077000100916                     spedtr = datacor6;
077100100916                     speftr = ' ';
077200100916                     write fnspe000 ;
077300100916
077400100916           // cerco i suoi fnsp2
077500100916                     setll (ita55ksc:specod) fnsp201l;
077600100916                     reade (ita55ksc:specod) fnsp201l;
077700100916                     dow not %eof(fnsp201l) ;
077800100916                         if Sp2flg <> 'A' ;
077900100916                            sp2cli = ita55nwksc ;
078000100916                            write Fnsp2000 ;
078100100916                         endif ;
078200110222                       reade (ita55ksc:specod) fnsp201l;
078300100916                     Enddo ;
078400100916                 endif  ;
078500100916
078600100916    2         EndIf ;
078700100916
078800100916            exec sql fetch s1 into :fnspeds ;
078900100916
079000100916    1     enddo;
079100100916
079200100916            exec sql close s1 ;
079300100916
079400100916       ENDSR ;
079500100115       //--------------------------------------------------------------
079600100115       //?Operazioni iniziali.
079700100115       //--------------------------------------------------------------
079800100115       BEGSR RoutInz;
079900100115
080000100115         // Reperimento dati job
080100100115         exsr DatiJob;
080200100115
080300100115       // Reperisco data corrente
080400100115             data_iso = (%date());
080500100122             datacor8 = %dec(data_iso);
080600100125             data_ymd = (%date());
080700100121             datacor6 = %dec(data_ymd) ;
080800100122
080900100122        //  Carico schiera con tabella 1T (tipo nota)
081000100122             Clear   xx;
081100100505             TBLkut = 1 ;
081200100122             TBLcod = '1T';
081300100122             setll (TBLkut : TBLcod) TABEL;
081400100122             reade (TBLkut : TBLcod) TABEL;
081500100122             dow not %eof(TABEL00F);
081600100122                 If tblflg = ' ' and tblkey <> *blanks ;
081700100122                    xx = xx + 1 ;
081800100122                    ds1t = tbluni ;
081900100122                    tno(xx) = tblkey ;
082000100122                    tdup(xx) = §1tdup;
082100100122                    ttdp(xx) = §1ttdp;
082200100122                 endif ;
082300100122                 reade (TBLkut : TBLcod) TABEL;
082400100122             EndDo ;
082500100115
082600100916        //  Carico schiera con tabella 4L (Lughi)
082700100916             Clear   xx;
082800100916             TBLkut = 1 ;
082900100916             TBLcod = '4L';
083000100916             setll (TBLkut : TBLcod) TABEL;
083100100916             reade (TBLkut : TBLcod) TABEL;
083200100916             dow not %eof(TABEL00F);
083300100916                 If tblflg = ' ' and tblkey <> *blanks ;
083400100916                    xx = xx + 1 ;
083500100917                    ds4l = tbluni ;
083600100916                    tluo(xx) = tblkey ;
083700100916                    tufi(xx) = §4Lufi;
083800100916                 endif ;
083900100916                 reade (TBLkut : TBLcod) TABEL;
084000100916             EndDo ;
084100100916
084200100506             acokut = 1 ;
084300100506             acokcc = dutkci ;
084400100422
084500100115       ENDSR;
084600100115
084700100115       //--------------------------------------------------------------
084800100115       //?Reperimento Dati del job (Utente/Operativi).
084900100115       //--------------------------------------------------------------
085000100115       BEGSR DatiJob;
085100100115
085200100115         in(E) §AzUte;
085300100115         if NOT %error;
085400100115           in(E) §DatiUte;
085500100115         endif;
085600100115         if %error or RSut = *blanks;
085700100115           clear TIBS34ds;
085800100115           tibs34r(tibs34ds);
085900100115           in §AzUte;
086000100115           in §DatiUte;
086100100115         endif;
086200100115
086300100115       ENDSR;
086400100108
086500080206       //--------------------------------------------------------------
086600080206       //?Operazioni finali.
086700080206       //--------------------------------------------------------------
086800080206       BEGSR RoutEnd;
086900090806
087000100504
087100100504       If wtibs73 = *On  ;
087200100504          ibs73tla = 'C' ;
087300100504          TIBS73R (tibs73ds:cnaco_73:cnind_73:cnclp_73:fncls_73);
087400100504       EndIf ;
087500080206         *inLR = *on;
087600080206         return;
087700080206
087800080206       ENDSR;
087900080206
088000080206      /end-free
