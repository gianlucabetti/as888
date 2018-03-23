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
013000080207      // - Campi associati al video
013100080206
013200090401      // - Campi di comodo data
013300090401     d  data_eur       s               d   Datfmt(*eur)
013400090401     d  data_iso       s               d   Datfmt(*iso)
013500090401     d  data_dmy       s               d   Datfmt(*dmy)
013600090811     d  data_ymd       s               d   datfmt(*ymd)
013700090401
013800080206      // - Campi di comodo
013900090406     d wAbi            s                   like(UTEaut)  inz
014000100122     d  datacor8       s              8  0
014100100122     d  datacor6       s              6  0
014200100504     d savnk1          s                   like(ntcnk1)
014300100504     d Wtibs73         s               n   inz(*off)
014400100916     d Woldtic         s                   like(clstic)
014500100916     d Wnewtic         s                   like(clstic)
014600100916     d rqsformatname...
014700100916     d                 S             10A
014800100916     d rqsdatasize...
014900100916     d                 S             10I 0
015000100916     d rqsopcode...
015100100916     d                 S             10A
015200100916     d rpyformatname...
015300100916     d                 S             10A
015400100916     d rpydatasize...
015500100916     d                 S             10I 0
015600100916     d rpyesito...
015700100916     d                 S             10I 0
015800090323     d
015900080208
016000090508      //---------------------------------------------------------------
016100090508      //?Definizione procedure usate.
016200090508      //---------------------------------------------------------------
016300080206
016400100420
016500100420      //---------------------------------------------------------------
016600100420      //?prototipi
016700100420      //---------------------------------------------------------------
016800100916
016900100916      // - Coordinate bancarie
017000100916     d TrulIbanR       pr                  extpgm('TRULIBANR')
017100100916     d  rqsOpCode                    10a   const
017200100916     d  rpyEsito                     10i 0
017300100916     d  rqsFormatName                10a   const
017400100916     d  rqsData                     256a   options(*varsize)
017500100916     d  rqsDataSize                  10i 0 const
017600100916     d  rpyFormatName                10a   const
017700100916     d  rpyData                     256a   options(*varsize)
017800100916     d  rpyDataSize                  10i 0 const
017900100916
018000100420
018100080626      /copy gaitrasrc/srcprotopr,tibs34r
018200100119      /copy gaitrasrc/srcprotopr,tibs69r
018300100503      /copy gaitrasrc/srcprotopr,tibs73r
018400090508
018500080206      //---------------------------------------------------------------
018600080206      //?Definizione key-list.
018700080206      //---------------------------------------------------------------
018800080206
018900080206
019000080206      //---------------------------------------------------------------
019100080206      //?Riepilogo indicatori.
019200080206      //---------------------------------------------------------------
019300100503
019400080206      //---------------------------------------------------------------
019500080206
019600080206      //---------------------------------------------------------------
019700080206      //?M A I N - L I N E
019800080206      //---------------------------------------------------------------
019900080206
020000080206     c     *Entry        plist
020100080206     c                   parm                    KPJBA
020200100503     c                   parm                    tnta55ds
020300080206
020400080206      /free
020500080206
020600080206       // Operazioni iniziali
020700080206       exsr RoutInz;
020800090601
020900100503       SELECT ;
021000100503
021100100503       // se prima convalida di trattativa nuova
021200100503       when ita55prima = 'SI' and Ita55tpv = 'N' ;
021300100503            exsr sr_daTFACO  ;
021400100503            exsr sr_daTFNTCT ;
021500100503
021600100503       // se non è prima convalida di trattativa nuova ksc nuovo OPPURE NO
021700100503       when ita55prima <> 'SI' and Ita55tpv = 'N' ;
021800100503         // verifico il nuovo cnaco
021900100504             acoksc = ita55nwksc ;
022000100503             setll  (acokut:acokcc:acoksc) Cnaco00f ;
022100100503             // esiste cnaco aggiorno solo CNIND
022200100503             If %equal ;
022300100503                exsr sr_updIND  ;
022400100503             else ;
022500100503                exsr sr_daTFACO;
022600100503                exsr sr_daTFNTCT;
022700100503             endif ;
022800100503
022900110114       // se prima convalida di trattativa non nuova con o senza tfaco
023000110114       when ita55prima = 'SI' and Ita55tpv <> 'A' ;
023100100519         // verifico il nuovo cnaco
023200100519             acokut = 1 ;
023300100519             acokcc = dutkci ;
023400100519             acoksc = ita55nwksc ;
023500100519             setll  (acokut:acokcc:acoksc) Cnaco00f ;
023600100916             // esiste cnaco aggiorno solo CNIND se non ho tfaco
023700100519             If %equal ;
023800100916                acokut = 1 ;
023900100916                acokcc = dutkci ;
024000100916                acoksc = ita55nrv ;
024100100916                setll  (acokut:acokcc:acoksc) Tfaco00f ;
024200100916             // non esiste tfaco aggiorno cnind
024300100916                If not %equal(tfaco00f)  ;
024400100916                   exsr sr_updIND  ;
024500100916                else ;
024600100916             // esiste tfaco aggiorno tutta l'anagrafica
024700100916                   exsr sr_daTFACO ;
024800100916              // non scrivo la rubrica in quanto già appoggiata al cliente
024900100916                endif ;
025000100519             else ;
025100100519                acokut = 1 ;
025200100519                acokcc = dutkci ;
025300100519                acoksc = ita55nrv ;
025400100519                setll  (acokut:acokcc:acoksc) Tfaco00f ;
025500100519                If %equal ;
025600100519                   exsr sr_daTFACO ;
025700100916              // scrivo la rubrica copiandolo dal cliente trattativa
025800100916                   exsr sr_daTFNTCC;
025900100917              // scrivo i luoghi copiandoli dal cliente trattativa
026000100917                   exsr sr_daFNSPEC;
026100100519                else ;
026200100519                   exsr sr_daCNACO ;
026300100519                   exsr sr_daTFNTCC;
026400100916                   exsr sr_daFNSPEC;
026500100519                   exsr sr_updIND ;
026600100519                endif ;
026700100916             endif ;
026800100916
026900100510
027000110114       // se non è prima convalida di trattativa non Nuova ksc nuovo OPPURE NO
027100110114       when ita55prima <> 'SI' and Ita55tpv <> 'A' ;
027200100503         // verifico il nuovo cnaco
027300100503             acokut = 1 ;
027400100503             acokcc = dutkci ;
027500100504             acoksc = ita55nwksc ;
027600100503             setll  (acokut:acokcc:acoksc) Cnaco00f ;
027700100503             // esiste cnaco aggiorno solo CNIND
027800100503             If %equal ;
027900100503                exsr sr_updIND  ;
028000100503             else ;
028100100503             // Verifico se esiste tfaco
028200100503                acoksc = ita55nrv ;
028300100503                setll  (acokut:acokcc:acoksc) Tfaco00f ;
028400100503                If %equal ;
028500100503                   exsr sr_daTFACO ;
028600100503              // scrivo la rubrica del cliente trattaiva nel nuovo cliente
028700100503                   exsr sr_daTFNTCC;
028800100916              // scrivo i luoghi del cliente trattativa nel nuovo cliente
028900100916                   exsr sr_daFNSPEC;
029000100503                else ;
029100100503              // copio da cnaco a cnaco e da tfntc a tfntc con codice cliente
029200100503                   exsr sr_daCNACO ;
029300100503                   exsr sr_daTFNTCC;
029400100916                   exsr sr_daFNSPEC;
029500100503                endif ;
029600100503             endif ;
029700100503
029800100503       ENDSL ;
029900080206       // Operazioni finali
030000080206       exsr RoutEnd;
030100080206
030200100121       //--------------------------------------------------------------
030300100503       //?Scrittura Anagrafiche da TFACO NUOVA
030400100121       //--------------------------------------------------------------
030500100503       BEGSR Sr_DaTFACO;
030600100121
030700100504       acoksc = ita55nwksc ;
030800100503       // verifico se esiste anagrafica con il nuovo codice cliente
030900100503       chain (acokut:acokcc:acoksc) cnaco00f ;
031000100503       chain (acokut:acokcc:acoksc) cnind00f ;
031100100503       chain (acokut:acokcc:acoksc) cnclp00f ;
031200100503       chain (acoksc) fncls01l ;
031300100121
031400100121       // se già trovato un'anagrafica esistente memorizzo l'immagine
031500100121       // precedente per storicizzare le variazioni
031600100121
031700100121       If %found(cnaco00f) ;
031800100121          clear tibs73ds ;
031900100121          ibs73immag = 'P' ;
032000100121          TIBS73R (tibs73ds:cnaco_73:cnind_73:cnclp_73:fncls_73);
032100100121          wtibs73 = *On  ;
032200100121       endif;
032300100121
032400100503       // aggancio anagrafica provvisoria   TFACO00F  con numero trattativa
032500100503       chain(n) (acokut:acokcc:ita55nrv) tfaco00f ;
032600100121
032700100121       If  %found(tfaco00f) ;
032800100504           acoksc = ita55nwksc ;
032900100121           acoftt = '3' ;
033000100504           acoflt = %dec(%subst(%EDITC(ita55nwKsc:'X'):1:3):3:0);
033100100121           clear acoftr ;
033200100121           clear acodtr ;
033300100121           acoduv = datacor6 ;
033400100121          if not %found(cnaco00f) ;
033500100121             nrjob = knraz ;
033600100121             write cnaco000 ;
033700100504             ota55new = '1';
033800100121          else ;
033900100121             update cnaco000 ;
034000100504             ota55new = '0';
034100100121          endif ;
034200100121
034300100503       // aggancio anagrafica provvisoria   TFIND00F con numero trattativa
034400100503       chain (acokut:acokcc:ita55nrv) tfind00f ;
034500100121       // se modificati i dati del pagamento li registro prima nell'anagrafica
034600100121       // provvisoria
034700100121
034800100121       If  %found(tfind00f) ;
034900100520           If Ita55pag = '1' ;
035000100503              indabi = ita55abi ;
035100100503              indcab = ita55cab ;
035200100505              indcdp = ita55cdp ;
035300100122              update tfind000 ;
035400100503              chain(n) (acokut:acokcc:ita55nrv) tfind00f ;
035500100122           endif ;
035600100504           indksc = ita55nwksc ;
035700100122       // Partita Iva
035800100503           indiva = ita55iva ;
035900100122       // Codice Fiscale
036000100503           indcdf = ita55cdf;
036100100122
036200100122           if not %found(cnind00f) ;
036300100122              indnrj = knraz ;
036400100122              write cnind000 ;
036500100122           else ;
036600100122              update cnind000 ;
036700100122           endif;
036800100122       endif ;
036900100122
037000100503       // aggancio anagrafica provvisoria   TFCLP00F con numero trattativa
037100100503       chain(n) (acokut:acokcc:ita55nrv) tfclp00f ;
037200100122
037300100122       If  %found(tfclp00f) ;
037400100122
037500100504           clpksc = ita55nwksc ;
037600100122       //  se non c'è cnclp imposto il commerciale della visita e non + quello
037700100122       //  dell'anagrafica provvisoria xchè potrebbe essere incongruente con il
037800100122       //  p.o. del commerciale della visita
037900100122           if not %found(cnclp00f) ;
038000100503              clpage = ita55cmm ;
038100100122              clpnrj = knraz ;
038200100122              write cnclp000 ;
038300100122           else ;
038400100122              update cnclp000 ;
038500100122           endif ;
038600100122       endif ;
038700100122
038800100503       // aggancio anagrafica provvisoria   TFCLS00F con numero trattativa
038900100916
039000100916       // mi memorizzo i codici pagamenti vecchi
039100100916          clear Woldtic ;
039200100916          clear Wnewtic ;
039300100916
039400100916       If %found(fncls01l);
039500100916          Woldtic = clstic ;
039600100916       endif ;
039700100916
039800100503       chain(n) (ita55nrv) tfcls01l ;
039900100122
040000100122       If  %found(tfcls01l) ;
040100100916           Wnewtic = clstic ;
040200100504           clsksc = ita55nwksc ;
040300100122           if not %found(fncls01l) ;
040400100122              write fncls000 ;
040500100122           else ;
040600100122              update fncls000 ;
040700100122           endif ;
040800100122
040900100122        Endif ;
041000100916
041100100916       // aggancio anagrafica provvisoria   TFCBA00F con numero trattativa
041200100916       // e per ogni tipo pagamento per verificare l'esistenza o meno dei
041300100916       // record. Se record non presente deleto FNCBA con TRULIBAN
041400100916       // mentre se c'è faccio update su FNCBA con TRULIBAN
041500100916
041600100916       // DANNI
041700100916
041800100916       chain(n) (ita55nrv:'DN') tfcba01l ;
041900100916
042000100916       If  not %found(tfcba01l) ;
042100100916           rqsOpCode = 'DELETE' ;
042200100916           CBAPAG = 'DN' ;
042300100916           CLEAR  CBAABI ;
042400100916           CLEAR  CBABIC ;
042500100916           exsr coordinate ;
042600100916       else ;
042700100916           If %found(cnaco00f) ;
042800100916              rqsOpCode = 'UPDATE' ;
042900100916           else ;
043000100916              rqsOpCode = 'INSERT' ;
043100100916           endif;
043200100916           exsr coordinate ;
043300100916       endif;
043400100916
043500100916       // NOTE ACCREDITO
043600100916
043700100916       chain(n) (ita55nrv:'NA') tfcba01l ;
043800100916
043900100916       If  not %found(tfcba01l) ;
044000100916           rqsOpCode = 'DELETE' ;
044100100916           CBAPAG = 'AN' ;
044200100916           CLEAR  CBAABI ;
044300100916           CLEAR  CBABIC ;
044400100916           exsr Coordinate ;
044500100916       else ;
044600100916           If %found(cnaco00f) ;
044700100916              rqsOpCode = 'UPDATE' ;
044800100916           else ;
044900100916              rqsOpCode = 'INSERT' ;
045000100916           endif;
045100100916           exsr Coordinate    ;
045200100916       endif;
045300100916
045400100122
045500100510       // Memorizzo l'immagine seguente per storicizzare le variazioni
045600100510           exsr ScriviACV ;
045700100122        Endif ;
045800100503
045900100503       ENDSR;
046000100503
046100100916       //--------------------------------------------------------------
046200100916       //?Operazioni relative alle coordinate bancarie.
046300100916       //--------------------------------------------------------------
046400100916       BEGSR Coordinate;
046500100916
046600100916         clear rpyEsito;
046700100916
046800100916           clear TrulIbanI0;
046900100916           clear TrulIbanO0;
047000100916           TrulIbanI0.KSC = ita55nwksc ;
047100100916           TrulIbanI0.PAG = cbapag ;
047200100916           TrulIbanI0.IBAN  = cbaiban ;
047300100916           TrulIbanI0.BIC   = cbabic ;
047400100916           If cbapag = 'DN' ;
047500100916              TrulIbanI0.CODPO = %subst(Woldtic:1:1);
047600100916              TrulIbanI0.CODPN = %subst(Wnewtic:1:1);
047700100916           endif ;
047800100916           If cbapag = 'NA' ;
047900100916              TrulIbanI0.CODPO = %subst(Woldtic:2:1);
048000100916              TrulIbanI0.CODPN = %subst(Wnewtic:2:1);
048100100916           endif ;
048200100916           TrulIbanI0.PGM   = 'TNTA56R' ;
048300100916           rqsFormatName = 'TRULIBANI0';
048400100916           rpyFormatName = 'TRULIBANO0';
048500100916           rqsDataSize   = %size(TrulIbanI0);
048600100916           rpyDataSize   = %size(TrulIbanO0);
048700100916           TrulIbanR (rqsOpCode:rpyEsito:
048800100916                      rqsFormatName:TrulIbanI0:rqsDataSize:
048900100916                      rpyFormatname:TrulIbanO0:rpyDataSize);
049000100916
049100100916       ENDSR;
049200100504       //--------------------------------------------------------------
049300100504       //?Scrittura Anagrafiche da CNACO NUOVA
049400100504       //--------------------------------------------------------------
049500100504       BEGSR Sr_DaCNACO;
049600100504
049700100504       // copio da cnaco cliente della trattativa a nuovo cliente
049800100504             chain(n) (acokut:acokcc:ita55ksc) cnaco00f ;
049900100504             If %found(cnaco00f) ;
050000100504                acoksc = ita55nwksc ;
050100100504                acoftt = '3' ;
050200100504                acoflt = %dec(%subst(%EDITC(ita55nwKsc:'X'):1:3):3:0);
050300100504                clear acoftr ;
050400100504                clear acodtr ;
050500100504                acoduv = datacor6 ;
050600100504                nrjob = knraz ;
050700100504                write cnaco000 ;
050800100504                ota55new = '1';
050900100504             endif ;
051000100504
051100100504       // copio da cnind cliente della trattativa a nuovo cliente
051200100504             chain(n) (acokut:acokcc:ita55ksc) cnind00f ;
051300100504             If %found(cnind00f) ;
051400100520                If Ita55pag = '1' ;
051500100504                   indabi = ita55abi ;
051600100504                   indcab = ita55cab ;
051700100505                   indcdp = ita55cdp ;
051800100504                endif ;
051900100504                indksc = ita55nwksc ;
052000100504       // Partita Iva
052100100504                indiva = ita55iva ;
052200100504       // Codice Fiscale
052300100504                indcdf = ita55cdf;
052400100504
052500100504                indnrj = knraz ;
052600100504                write cnind000 ;
052700100504             endif ;
052800100504
052900100504       // copio da cnclp cliente della trattativa a nuovo cliente
053000100504       chain(n) (acokut:acokcc:ita55ksc) cnclp00f ;
053100100504
053200100504       If  %found(cnclp00f) ;
053300100504           clpksc = ita55nwksc ;
053400100504           clpage = ita55cmm ;
053500100504           clpnrj = knraz ;
053600100504           write cnclp000 ;
053700100504       endif ;
053800100504
053900100504       // copio da fncls cliente della trattativa a nuovo cliente
054000100504       chain(n) (ita55ksc) fncls01l ;
054100100504
054200100504       If  %found(fncls01l) ;
054300100504           clsksc = ita55nwksc ;
054400100504           write fncls000 ;
054500100504       endif ;
054600100504
054700100915       // copio da fncba cliente della trattativa a nuovo cliente
054800100915       setll    (ita55ksc) fncba01l ;
054900100915       reade(n) (ita55ksc) fncba01l ;
055000100915
055100100915       dow  not %eof(fncba01l) ;
055200100915           cbaksc = ita55nwksc ;
055300100916           write tfcba000 ;
055400100916           reade(n) (ita55ksc) fncba01l ;
055500100916       enddo ;
055600100915
055700100510       // Memorizzo l'immagine seguente per storicizzare le variazioni
055800100510           exsr ScriviACV ;
055900100504
056000100504       ENDSR;
056100100504
056200100503       //--------------------------------------------------------------
056300100503       //?Aggiorno CNIND da dati a video
056400100503       //--------------------------------------------------------------
056500100503       BEGSR Sr_UPDIND ;
056600100503
056700100504           chain (acokut:acokcc:ita55nwksc) cnind00f ;
056800100122           If %found(cnind00f) ;
056900100122       // se modificate le condizioni di pagamento si aggiornano
057000100520              If Ita55pag = '1' ;
057100100503                 indabi = ita55abi ;
057200100503                 indcab = ita55cab ;
057300100505                 indcdp = ita55cdp ;
057400100503              endif ;
057500100503       // Partita Iva
057600100503              indiva = ita55iva ;
057700100503       // Codice Fiscale
057800100503              indcdf = ita55cdf;
057900100503              update cnind000 ;
058000100503           endif;
058100100122
058200100121       ENDSR ;
058300100122
058400100122       //--------------------------------------------------------------
058500100122       //?Scrittura del file delle variazioni Anagrafiche
058600100122       //--------------------------------------------------------------
058700100122       BEGSR Scriviacv ;
058800100122
058900100122       Clear Tibs73ds ;
059000100122       ibs73pru=sdsuse ;
059100100122       ibs73noj=sdsjob ;
059200100506       ibs73pgm='TNTA56R';
059300100122       ibs73immag='D';
059400100122
059500100122       //  Immissione
059600100504       If  Ota55new    = '1' ;
059700100122           ibs73cta='O' ;
059800100122           TIBS73R (tibs73ds:cnaco_73:cnind_73:cnclp_73:fncls_73);
059900100122       //  Manutenzione
060000100122       else;
060100100122           ibs73cta='M' ;
060200100122           TIBS73R (tibs73ds:cnaco_73:cnind_73:cnclp_73:fncls_73);
060300100122       endif;
060400100122
060500100122       wtibs73 = *On ;
060600100122
060700100122       ENDSR ;
060800100424
060900100424       //--------------------------------------------------------------
061000100503       //?Scrittura Note Rubrica da trattativa
061100100424       //--------------------------------------------------------------
061200100503       BEGSR Sr_Datfntct;
061300100424
061400100503         savnk1 = %editc(dutkci: 'X') + %editc(ita55nrv:'X');
061500100425       // cerco le note relative alla trattativa da duplicare sul cliente
061600100504         exec sql declare c1 cursor for select *
061700100425         from tfntc01l where ntcapl = 'T' and ntcnk1 = :savnk1
061800100427         and ntctnt <> '10'
061900100425         order by ntcapl, ntcnk1, ntcnk2, ntctnt
062000100425         for read only  ;
062100100425
062200100504         exec sql open c1 ;
062300100425
062400100504         exec sql fetch c1 into :tfntcds ;
062500100425
062600100425         dow Sqlcod = 0;
062700100425
062800100425            If Ntcflt <> 'A' ;
062900100425    c          xx = 1 ;
063000100425               xx = %lookup(ntctnt:tno) ;
063100100425               If xx > 0  and tdup(xx) = 'S';
063200100425                   ntcapl = 'C' ;
063300100504                   ntcnk1 = %editc(dutkci: 'X') + %editc(ita55nwksc:'X');
063400100425                   ntcntr = datacor6;
063500100425                   write tfntc ;
063600100425    2          EndIf ;
063700100425    2       EndIf ;
063800100425
063900100504          exec sql fetch c1 into :tfntcds ;
064000100425
064100100425    1    enddo;
064200100425
064300100504         exec sql close c1 ;
064400100425
064500100424       ENDSR ;
064600100503
064700100503       //--------------------------------------------------------------
064800100503       //?Scrittura Note Rubrica da cliente
064900100503       //--------------------------------------------------------------
065000100503       BEGSR Sr_Datfntcc;
065100100503
065200100503         savnk1 = %editc(dutkci: 'X') + %editc(ita55ksc:'X');
065300100916       // cerco le note relative al cliente trattativa da duplicare sul cliente
065400100503         exec sql declare c2 cursor for select *
065500100506         from tfntc01l where ntcapl = 'C' and ntcnk1 = :savnk1
065600100503         and ntctnt <> '10'
065700100503         order by ntcapl, ntcnk1, ntcnk2, ntctnt
065800100503         for read only  ;
065900100503
066000100503         exec sql open c2 ;
066100100503
066200100503         exec sql fetch c2 into :tfntcds ;
066300100503
066400100503         dow Sqlcod = 0;
066500100503
066600100503            If Ntcflt <> 'A' ;
066700100503    c          xx = 1 ;
066800100503               xx = %lookup(ntctnt:tno) ;
066900100503               If xx > 0  and tdup(xx) = 'S';
067000100503                   ntcapl = 'C' ;
067100100504                   ntcnk1 = %editc(dutkci: 'X') + %editc(ita55nwksc:'X');
067200100503                   ntcntr = datacor6;
067300100503                   write tfntc ;
067400100503    2          EndIf ;
067500100503    2       EndIf ;
067600100503
067700100503          exec sql fetch c2 into :tfntcds ;
067800100503
067900100503    1    enddo;
068000100503
068100100503         exec sql close c2 ;
068200100503
068300100503       ENDSR ;
068400100916
068500100916       //--------------------------------------------------------------
068600100916       //?Scrittura luoghi  da cliente
068700100916       //--------------------------------------------------------------
068800100916       BEGSR Sr_Dafnspec;
068900100916
069000100916       // cerco i luoghi relative al cliente da duplicare sul cliente
069100100916           exec sql declare s1 cursor for select *
069200100916           from fnspe01l where spefls = 'L' and specli = :ita55ksc
069300100916           for read only  ;
069400100916
069500100916           exec sql open s1 ;
069600100916
069700100916           exec sql fetch s1 into :fnspeds ;
069800100916
069900100916           dow Sqlcod = 0;
070000100916
070100100916              If Speflg =  ' ' ;
070200100916
070300100916    c            xx = 1 ;
070400100916                 xx = %lookup(specod:tluo) ;
070500100916                 If xx > 0  and tufi(xx) = 'S';
070600100916
070700100916                     specli = ita55nwksc;
070800100916                     spedtr = datacor6;
070900100916                     speftr = ' ';
071000100916                     write fnspe000 ;
071100100916
071200100916           // cerco i suoi fnsp2
071300100916                     setll (ita55ksc:specod) fnsp201l;
071400100916                     reade (ita55ksc:specod) fnsp201l;
071500100916                     dow not %eof(fnsp201l) ;
071600100916                         if Sp2flg <> 'A' ;
071700100916                            sp2cli = ita55nwksc ;
071800100916                            write Fnsp2000 ;
071900100916                         endif ;
072000110222                       reade (ita55ksc:specod) fnsp201l;
072100100916                     Enddo ;
072200100916                 endif  ;
072300100916
072400100916    2         EndIf ;
072500100916
072600100916            exec sql fetch s1 into :fnspeds ;
072700100916
072800100916    1     enddo;
072900100916
073000100916            exec sql close s1 ;
073100100916
073200100916       ENDSR ;
073300100115       //--------------------------------------------------------------
073400100115       //?Operazioni iniziali.
073500100115       //--------------------------------------------------------------
073600100115       BEGSR RoutInz;
073700100115
073800100115         // Reperimento dati job
073900100115         exsr DatiJob;
074000100115
074100100115       // Reperisco data corrente
074200100115             data_iso = (%date());
074300100122             datacor8 = %dec(data_iso);
074400100125             data_ymd = (%date());
074500100121             datacor6 = %dec(data_ymd) ;
074600100122
074700100122        //  Carico schiera con tabella 1T (tipo nota)
074800100122             Clear   xx;
074900100505             TBLkut = 1 ;
075000100122             TBLcod = '1T';
075100100122             setll (TBLkut : TBLcod) TABEL;
075200100122             reade (TBLkut : TBLcod) TABEL;
075300100122             dow not %eof(TABEL00F);
075400100122                 If tblflg = ' ' and tblkey <> *blanks ;
075500100122                    xx = xx + 1 ;
075600100122                    ds1t = tbluni ;
075700100122                    tno(xx) = tblkey ;
075800100122                    tdup(xx) = §1tdup;
075900100122                    ttdp(xx) = §1ttdp;
076000100122                 endif ;
076100100122                 reade (TBLkut : TBLcod) TABEL;
076200100122             EndDo ;
076300100115
076400100916        //  Carico schiera con tabella 4L (Lughi)
076500100916             Clear   xx;
076600100916             TBLkut = 1 ;
076700100916             TBLcod = '4L';
076800100916             setll (TBLkut : TBLcod) TABEL;
076900100916             reade (TBLkut : TBLcod) TABEL;
077000100916             dow not %eof(TABEL00F);
077100100916                 If tblflg = ' ' and tblkey <> *blanks ;
077200100916                    xx = xx + 1 ;
077300100917                    ds4l = tbluni ;
077400100916                    tluo(xx) = tblkey ;
077500100916                    tufi(xx) = §4Lufi;
077600100916                 endif ;
077700100916                 reade (TBLkut : TBLcod) TABEL;
077800100916             EndDo ;
077900100916
078000100506             acokut = 1 ;
078100100506             acokcc = dutkci ;
078200100422
078300100115       ENDSR;
078400100115
078500100115       //--------------------------------------------------------------
078600100115       //?Reperimento Dati del job (Utente/Operativi).
078700100115       //--------------------------------------------------------------
078800100115       BEGSR DatiJob;
078900100115
079000100115         in(E) §AzUte;
079100100115         if NOT %error;
079200100115           in(E) §DatiUte;
079300100115         endif;
079400100115         if %error or RSut = *blanks;
079500100115           clear TIBS34ds;
079600100115           tibs34r(tibs34ds);
079700100115           in §AzUte;
079800100115           in §DatiUte;
079900100115         endif;
080000100115
080100100115       ENDSR;
080200100108
080300080206       //--------------------------------------------------------------
080400080206       //?Operazioni finali.
080500080206       //--------------------------------------------------------------
080600080206       BEGSR RoutEnd;
080700090806
080800100504
080900100504       If wtibs73 = *On  ;
081000100504          ibs73tla = 'C' ;
081100100504          TIBS73R (tibs73ds:cnaco_73:cnind_73:cnclp_73:fncls_73);
081200100504       EndIf ;
081300080206         *inLR = *on;
081400080206         return;
081500080206
081600080206       ENDSR;
081700080206
081800080206      /end-free
