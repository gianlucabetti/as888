000100130327       //==============================================================
000200130327       //?Ricerca beneficiario in C.A.                                 ?
000300130327       //==============================================================
000400130327
000500130327       //--------------------------------------------------------------
000600130327       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700130327       //--------------------------------------------------------------
000800130327
000900130327     /*PRM  dbgview(*source)
001000130328     /*CMD  ovrdbf file(TITAS30C) tofile(GAITRAGRPS/TITAS30C) +
001100130328     /*CMD         ovrscope(*calllvl)
001200130328     /*CMD  ovrdbf file(TITAA30C) tofile(GAITRAGRPS/TITAA30C) +
001300130328     /*CMD         ovrscope(*calllvl)
001400130328     /*CMD  ovrdbf file(FNBLP01L) tofile(FILTRAPRD/FNBLP01L) +
001500130327     /*CMD         ovrscope(*calllvl)
001600130328     /*CMD  ovrdbf file(FIAR601L) tofile(FILTRAPRD/FIAR601L) +
001700130328     /*CMD         ovrscope(*calllvl)
001800130328     /*END  dltovr file(*all) lvl(*)
001900150729
002000130327
002100130327       //--------------------------------------------------------------
002200130327       //?Specifiche di controllo.                                     ?
002300130327       //--------------------------------------------------------------
002400130327
002500081113     h decedit('0,') datedit(*dmy/) option(*nodebugio)
002600130327
002700130327       //--------------------------------------------------------------
002800130327       //?Dichiarazione file.                                          ?
002900130327       //--------------------------------------------------------------
003000081113
003100081113     fFNDKA01L  if   e           k disk
003200171102     fFIPND01L  if   e           k disk
003300130327
003400130327     fTITAS30C  if   e           k disk    usropn prefix(BLP:3)
003500130327     f                                     extfile(wLibSede)
003600130327     fTITAA30C  if   e           k disk    usropn
003700130327     f                                     extfile(wLibSede)
003800130327
003900130327     fFNBLP01L  if   e           k disk    usropn extfile(wLibFil)
004000130327     fFIAR601L  if   e           k disk    usropn extfile(wLibFil)
004100130327     fFNSPE01L  if   e           k disk
004200130327     fFNSP201L  if   e           k disk
004300081114
004400081114     fTFNTC01L  if   e           k disk
004500130327
004600081117
004700130327     fTABEL00F  if   e           k disk
004800130328     fAZORG01L  if   e           k disk
004900150701
005000150701       //--------------------------------------------------------------
005100150701       //?Definizione costanti.                                        ?
005200150701       //--------------------------------------------------------------
005300150701
005400150701       // -?Capoconto clienti Italia?
005500150701     d c_KCC_I         c                   const(0151)
005600130327
005700130328       //--------------------------------------------------------------
005800130328       //?Definizione strutture dati.                                  ?
005900130328       //--------------------------------------------------------------
006000130327
006100130328       // -?Parametri ricevuti?
006200130327     d FIDNBEds      e ds
006300171106     d FIDNBE2ds     e ds
006400081114
006500130328       // -?Flag operativi in FNDCT?
006600081118     d dDCT01        e ds                  inz
006700090107
006800171106       // -?Flag etichetta DPD ?
006900171106     d dPNDDP5       e ds                  inz
007000171106
007100130328       // -?Tabella tipo bolla?
007200130327     d dsTB          e ds                  inz
007300130327
007400130328       // -?Tabella nazioni?
007500130327     d ds15          e ds                  inz
007600081118
007700130328       // -?Dati intestatari Azienda?
007800130327     d dSTD2V        e ds                  extname (DSTD2)  Prefix(V)
007900130327
008000130328       // -?Dati da AZORG00F?
008100130327     d OG143ccm      e ds                  inz     extname(OG143)
008200130327     d                                             prefix(§CM:3)
008300081229
008400130328       // -?KPJBA?
008500130327     d KPJBA         e ds
008600130327
008700130327      //---------------------------------------------------------------
008800130327      //?Ridefinizione sottoconto?
008900130327      //---------------------------------------------------------------
009000130327
009100130327     d wDS_ksc         ds             7    inz
009200130327     d   w0030                        3  0 inz
009300130327     d   w0040                        4  0 inz
009400130328
009500130328       //--------------------------------------------------------------
009600130328       //?Definizione variabili globali.                               ?
009700130328       //--------------------------------------------------------------
009800130328
009900130328       // -?Flags booleani?
010000130328     d $FoundCap       s               n   inz
010100130328
010200130328       // -?Campi di comodo?
010300130328     d wLibSede        s             21    inz
010400130328     d wLibSedeP       s             21    inz('GAITRAGRPS/TITAS30C')
010500130328     d wLibSedeB       s             21    inz('GAITRAGRU /TITAS30C')
010600130328     d wLibSedePA      s             21    inz('GAITRAGRPS/TITAA30C')
010700130328     d wLibSedeBA      s             21    inz('GAITRAGRU /TITAA30C')
010800130328     d wLibFil         s             21    inz
010900130328     d wLibFilP        s             21    inz('FILTRAPRD /xxxxxxxx')
011000130328     d wLibFilB        s             21    inz('FILTRA201 /xxxxxxxx')
011100130328     d Kcli            s                   like(SPEcli)   inz
011200130328     d Kkey            s                   like(TBLkey)   inz
011300130328     d w§DCTport       s                   like(§dctport) inz
011400130328     d wFil            s                   like(ORGfil)   inz
011500130327
011600130327      //---------------------------------------------------------------
011700130328      //?Definizione prototipi procedure usate.                        ?
011800081114      //---------------------------------------------------------------
011900081114
012000130328       // -?Reperimento dati anagrafici?
012100130327      /copy gaitrasrc/srcProtoPI,TIBS69R
012200130327      /copy gaitrasrc/srcProtoPR,TIBS69R
012300081114
012400130328       // -?Ricerca/Controllo tabelle in TNTBE00F?
012500130328     d TIBS02ds      e ds                  inz
012600130327      /copy gaitrasrc/srcProtoPR,TIBS02R
012700130328
012800171102       // -?Ricerca/Controllo versione e anagrafico cappario DPD ?
012900171103     d O_DPCDP10Fds  E ds                  extname(DPCDP10F)
013000171103     d O_DPLEG10Fds  E ds                  extname(DPLEG10F)
013100171103     d TISIEdepDS    E ds                  inz
013200171102
013300171102     d TISIEDEPR       pr                  extpgm('TISIEDEPR')
013400171103     d  TISIEdepds                         likeds(tisiedepds)
013500171103     d  O_DPCDP10Fds                       likeds(O_DPCDP10Fds)
013600171103     d  O_DPLEG10Fds                       likeds(O_DPLEG10Fds)
013700130328
013800130328       // -?"Bonifica" stringa da caratteri indesiderati?
013900130328      /copy gaitrasrc/srcProtoPI,XCHKCHAR
014000130328      /copy gaitrasrc/srcProtoPR,XCHKCHAR
014100081229
014200081114      //---------------------------------------------------------------
014300130327      //?Definizione key-list.                                         ?
014400081114      //---------------------------------------------------------------
014500081118
014600130328       // -?File FNDKA01L?
014700130328     d k04dka01      e ds                  extname(FNDKA01L:*key)
014800130328     d                                     inz
014900130328
015000130328       // -?File TITAS30C?
015100081117     d k05tas        e ds                  extname(TITAS30C:*key)
015200081117     d                                     inz
015300130328       // -?File TITAA30C?
015400081223     d k05taa        e ds                  extname(TITAA30C:*key)
015500081223     d                                     inz
015600171102       // -?File FIPND01L?
015700171102     d k04PND        e ds                  extname(FIPND01L:*key)
015800130328
015900130328       // -?File FNBLP01L?
016000130328     d k04blp        e ds                  extname(FNBLP01L:*key)
016100130328     d                                     inz
016200130328       // -?File FIAR601L?
016300081118     d k05ar601      e ds                  extname(FIAR601L:*key)
016400081113     d                                     inz
016500130328
016600130328       // -?File FNSPE01L?
016700081114     d k03spe01      e ds                  extname(FNSPE01L:*key)
016800081114     d                                     inz
016900130328       // -?File FNSP201L?
017000081114     d k03sp201      e ds                  extname(FNSP201L:*key)
017100081114     d                                     inz
017200130328       // -?File TFNTC01L?
017300081114     d k04ntc01      e ds                  extname(TFNTC01L:*key)
017400081114     d                                     inz
017500130328       // -?File DPCDP01L?
017600130328     d k02dpcdp01    e ds                  extname(DPCDP01L:*key)
017700130328     d                                     inz   prefix(k_)
017800171102       // -Recupero versione cappario e anagrafico Cappario
017900171102       // -
018000171102
018100171102
018200130327       //-------------------------------------------------------------*
018300130327       //?Decodifica cliente, verifica se testo inglese e/o e-mail   ?*
018400130327       //-------------------------------------------------------------*
018500081113     c     *Entry        plist
018600130327     c                   parm                    FIDNBEds
018700171106     c                   parm                    FIDNBE2ds
018800130327
018900130328      /Free
019000130327
019100130327       *inLR = *on;
019200130327
019300130327       // -?Apertura file?
019400130328       if  iBEsif = 'P';
019500130328         wLibSede = wLibSedeP;
019600130328       else;
019700130328         wLibSede = wLibSedeB;
019800081212       endif;
019900130328       open  TITAS30C;
020000130327
020100130328       if  iBEsif = 'P';
020200130328         wLibSede = wLibSedePA;
020300130328       else;
020400130328         wLibSede = wLibSedeBA;
020500081223       endif;
020600130328       open  TITAA30C;
020700130328
020800130328       if  iBEsif = 'P';
020900130328         wLibFil  = wLibFilP;
021000130328       else;
021100130328         wLibFil  = wLibFilB;
021200130328       endif;
021300130328
021400130328       %subst(wLibFil:12) = 'FNBLP01L';
021500130328       open  FNBLP01L;
021600130328
021700130328       %subst(wLibFil:12) = 'FIAR601L';
021800130328       open  FIAR601L;
021900081118
022000130327
022100130410       dDCT01  = iBEflo;
022200130328       wDS_ksc = %editc(iBEksc:'X');
022300081118
022400130328
022500130410       // -?Verifico SE c'è rivalsa e aggancio FNDKA?
022600130328       DKAaac = iBEaac;
022700130328       DKAfil = iBEfil;
022800130328       DKAnca = iBEnca;
022900130327       DKAtrc = 'R';
023000130327       exsr  sr_DecodCli_FNDKA;
023100130328
023200130328
023300130328       SELECT;
023400130328
023500130328
023600130328         // ?Si tratta di rivalsa?
023700130328         WHEN  %found(FNDKA01L)  and  DKAatb = *blanks;
023800130328           oBEriv = 'S';
023900150710           // -?Restituisco il codice Beneficiario solo se richiesto?
024000150710           If  iBEmod = '*';
024100150710             oBEkcc = DKAkcc;
024200150710             oBEksc = DKAksc;
024300150710           EndIf;
024400130328
024500081118
024600130328         // ?NON si tratta di rivalsa      ?
024700130328         // ?· CON Beneficiario Alternativo?
024800130328         WHEN  iBEksc > *zeros;
024900130327
025000130327           // -?Se il Beneficiario alternativo è generico aggancio FNDKA,?
025100130327           IF  w0040 = 8888;
025200130327
025300150706             // -?Se il beneficiario è generico restituisco il codice?
025400150706             //  ?solo se richiesto?
025500150706             If  iBEmod = '*';
025600150706               oBEkcc = c_KCC_I;
025700150706               oBEksc = iBEksc;
025800150706             EndIf;
025900150706
026000130328             DKAaac = iBEaac;
026100130328             DKAfil = iBEfil;
026200130328             DKAnca = iBEnca;
026300130327             DKAtrc = 'C';
026400130327             exsr  sr_DecodCli_FNDKA;
026500130327
026600130328           // -?Altrimenti recupero l'anagrafica da CNACO?
026700081114           ELSE;
026800130327
026900130328             Kcli   = iBEksc ;
027000130328             oBEksc = Kcli;
027100081114             clear  tibs69ds;
027200130328             I69kac = iBEksc;
027300130328             I69kin = iBEksc;
027400130328             I69kcs = iBEksc;
027500130327             exsr  sr_DecodCli_CNACO;
027600130327
027700150729             // -?Controllo se esiste il luogo per il beneficiario?
027800150729             //  ?alternativo:?
027900150729             // ·?Se ricevuto codice luogo preferenziale "008"?
028000150729             //  ?(vedi *pgm FIDN29R): cerco SOLO lì?
028100150729             If  iBEpre = '008';
028200130327               SPEfls = 'L'   ;
028300130328               SPEcli = iBEksc;
028400130327               SPEcod = '008' ;
028500130327               chain %kds(k03spe01 : 3) FNSPE000;
028600150729             // ·?Altrimenti: cerco il luogo "006"?
028700150729             Else;
028800130327               SPEfls = 'L'   ;
028900130328               SPEcli = iBEksc;
029000130327               SPEcod = '006' ;
029100130327               chain %kds(k03spe01 : 3) FNSPE000;
029200150729             EndIf;
029300081114
029400130328             // ?Se esiste in FNSPE?
029500130327             IF  %found(FNSPE01L);
029600130327
029700130327               exsr  sr_DatiAna_FNSPE;
029800130327
029900130327               // -?Controllo se esiste nel luogo del beneficiario?
030000130327               //  ?alternativo l'indirizzo e-mail?
030100130328               SP2cli = iBEksc;
030200081219               SP2cod = SPEcod;
030300081114               SP2tpe = 'EM'  ;
030400130328               chain  %kds(k03sp201 : 3)  FNSP2000;
030500081114
030600130328               // -?Se esiste in FNSP2?
030700130328               if  %found(FNSP201L)  and  SP2est <> *blank;
030800130328                 oBEinv = 'M';
030900130328                 oBEima = SP2est;
031000130328                 oBEkma = iBEksc;
031100081114               endif;
031200130327
031300130328             // ?Se NON esiste in FNSPE?
031400081114             ELSE;
031500130327
031600130327               // -?Controllo se esiste nelle note con codice 087?
031700130327               //  ?l'indirizzo e-mail?
031800130328               if   oBEinv = *blanks ;
031900130327                 NTCapl = 'C' ;
032000150701                 NTCnk1 = %editc(c_KCC_I:'X') + %editc(iBEksc:'X');
032100130327                 clear  NTCnk2;
032200130327                 NTCtnt = '87';
032300081114                 chain %kds(k04ntc01 : 4) TFNTC;
032400081114
032500130328                 // -?Se esiste in TFNTC?
032600130328                 if  %found(TFNTC01L)  and  NTCrnt <> *blanks;
032700130328                   oBEinv = 'M';
032800130328                   oBEima = NTCrnt ;
032900130328                   oBEkma = iBEksc ;
033000081114                 endif;
033100081114               endif;
033200130327
033300130327             ENDIF;
033400081114
033500130327           ENDIF;
033600130328
033700081114
033800130328         // ?NON si tratta di rivalsa        ?
033900130328         // ?· SENZA Beneficiario Alternativo?
034000130328         OTHER;
034100130327
034200130328           // -»?se codice passato uguale a zero recupero il codice?
034300130327           exsr  Rep_CodCli;
034400130328           wDS_ksc = %editc( Kcli : 'X' );
034500130328
034600130328           // -?SE richiesto (cioè SE richiamato con 2° parametro),?
034700130328           //  ?SE non si tratta di rivalsa (oBEriv = "S")  e?
034800130328           //  ?SENZA beneficiario alternativo (iBEksc = *zero)?
034900130403           //  ?=> verifica SE C.A. con?beneficiario DPD?
035000130328           if  %parms() > 1;
035100130328             exsr  sr_Depot_DPD;
035200130328           endif;
035300130327
035400130328           SELECT;
035500130328
035600130328             // -?Beneficiario DPD?
035700130328             WHEN  %parms() > 1  and  oBErag <> *blanks;
035800171122
035900171213               oBEksc = LEGksc;
036000171213
036100171122               // - ?Imposto comunque il codice del cliente a cui mandare la?
036200171122               //   ?mail: se manca l'indirizzo mail sarà comunque da chiedere?
036300171122               oBEkma = LEGksc;
036400171122
036500171122               // - ?Controllo se esiste il luogo per il beneficiario?
036600171122               //   ?alternativo:?
036700171122               // · ?Se ricevuto codice luogo preferenziale "008"?
036800171122               //   ?(vedi *pgm FIDN29R): cerco SOLO lì?
036900171122               If  iBEpre = '008';
037000171122                 SPEfls = 'L'   ;
037100171122                 SPEcli = LEGksc;
037200171122                 SPEcod = '008' ;
037300171122                 chain  %kds(k03spe01 : 3)  FNSPE000;
037400171122               // · ?Altrimenti: cerco il luogo "006"?
037500171122               Else;
037600171122                 SPEfls = 'L'   ;
037700171122                 SPEcli = LEGksc;
037800171122                 SPEcod = '006' ;
037900171122                 chain  %kds(k03spe01 : 3)  FNSPE000;
038000171122               endif;
038100171122
038200171122               // - ?Se esiste in FNSPE?
038300171122               IF  %found(FNSPE01L);
038400171122
038500171122                 // - ?Controllo se esiste nel luogo del beneficiario?
038600171122                 //   ?alternativo l'indirizzo e-mail?
038700171122                 SP2cli = LEGksc;
038800171122                 SP2cod = SPEcod;
038900171122                 SP2tpe = 'EM'  ;
039000171122                 chain  %kds(k03sp201 : 3)  FNSP2000;
039100171122
039200171122                 // -?se esiste in FNSP2?
039300171122                 if  %found(FNSP201L)  and  SP2est <> *blank;
039400171122                   oBEinv = 'M';
039500171122                   oBEima = SP2est;
039600171122                   //*-oBEkma = LEGksc;     (già fatto)
039700171122                 endif;
039800171122
039900171122               // - ?Se NON esiste in FNSPE?
040000171122               ELSE;
040100171122
040200171122                 // - ?Controllo se esiste nelle note con codice 087?
040300171122                 //   ?l'indirizzo e-mail?
040400171122                 if   oBEinv = *blanks;
040500171122                   NTCapl = 'C' ;
040600171122                   NTCnk1 = %editc(c_KCC_I:'X') + %editc(LEGksc:'X');
040700171122                   clear  NTCnk2;
040800171122                   NTCtnt = '87';
040900171122                   chain  %kds(k04ntc01 : 4)  TFNTC;
041000171122
041100171122                   // -?se esiste in TFNTC?
041200171122                   if  %found(TFNTC01L)  and  NTCrnt <> *blanks;
041300171122                     oBEinv = 'M';
041400171122                     oBEima = NTCrnt ;
041500171122                     //*-oBEkma = LEGksc;   (già fatto)
041600171122                   endif;
041700171122                 endif;
041800171122
041900171122               ENDIF;
042000130328
042100130328             // -?Verifico se diverso da 8888 / 9999?
042200130328             WHEN  Kcli > *zeros  and  w0040 <> 8888
042300130328                                  and  w0040 <> 9999;
042400130327
042500130328               oBEksc = Kcli;
042600130328               clear TIBS69ds;
042700130328               I69kac = Kcli;
042800130328               I69kin = Kcli;
042900130328               I69kcs = Kcli;
043000130328               exsr  sr_DecodCli_CNACO;
043100130328
043200150729               // -?Controllo se esiste il luogo per il beneficiario?
043300150729               //  ?alternativo:?
043400150729               // ·?Se ricevuto codice luogo preferenziale "008"?
043500150729               //  ?(vedi *pgm FIDN29R): cerco SOLO lì?
043600150729               If  iBEpre = '008';
043700130328                 SPEfls = 'L'   ;
043800130328                 SPEcli = oBEksc;
043900130328                 SPEcod = '008' ;
044000130328                 chain  %kds(k03spe01 : 3)  FNSPE000;
044100150729               // ·?Altrimenti: cerco il luogo "006"?
044200150729               Else;
044300130328                 SPEfls = 'L'   ;
044400130328                 SPEcli = oBEksc;
044500130328                 SPEcod = '006' ;
044600130328                 chain  %kds(k03spe01 : 3)  FNSPE000;
044700130328               endif;
044800130328
044900130328               // ?Se esiste in FNSPE?
045000130328               IF  %found(FNSPE01L);
045100130328
045200130328                 exsr  sr_DatiAna_FNSPE;
045300130328
045400130328                 // -?Controllo se esiste nel luogo del beneficiario?
045500130328                 //  ?alternativo l'indirizzo e-mail?
045600130328                 SP2cli = oBEksc;
045700130328                 SP2cod = SPEcod;
045800130328                 SP2tpe = 'EM'  ;
045900130328                 chain  %kds(k03sp201 : 3)  FNSP2000;
046000130328
046100130328                 // -?se esiste in FNSP2?
046200130328                 if  %found(FNSP201L)  and  SP2est <> *blank;
046300130328                   oBEinv = 'M';
046400130328                   oBEima = SP2est;
046500130328                   oBEkma = oBEksc;
046600130328                 endif;
046700130328
046800130328               // ?Se NON esiste in FNSPE?
046900130328               ELSE;
047000130328
047100130328                 // -?Controllo se esiste nelle note con codice 087?
047200130328                 //  ?l'indirizzo e-mail?
047300130328                 if   oBEinv = *blanks;
047400130328                   NTCapl = 'C' ;
047500150701                   NTCnk1 = %editc(c_KCC_I:'X') + %editc(oBEksc:'X');
047600130328                   clear  NTCnk2;
047700130328                   NTCtnt = '87';
047800130328                   chain  %kds(k04ntc01 : 4)  TFNTC;
047900130328
048000130328                   // -?se esiste in TFNTC?
048100130328                   if  %found(TFNTC01L)  and  NTCrnt <> *blanks;
048200130328                     oBEinv = 'M';
048300130328                     oBEima = NTCrnt ;
048400130328                     oBEkma = oBEksc ;
048500130328                   endif;
048600130328                 endif;
048700130328
048800130328               ENDIF;
048900130327
049000130328             OTHER;
049100130327
049200150706               // -?Se invece il beneficiario non è codificato restituisco?
049300150706               //  ?il codice solo se richiesto?
049400150706               If  iBEmod = '*';
049500150706                 oBEkcc = c_KCC_I;
049600150706                 oBEksc = Kcli;
049700150706               EndIf;
049800150706
049900130328               // -?Se invece il beneficiario non è codificato recupero i?
050000130328               //  ?dati dalla spedizione?
050100130328               If  iBEptr = 'D';
050200130328
050300130328                 oBErag = BLPrsd;
050400130328                 oBEvia = BLPind;
050500130328                 oBEcap = BLPcad;
050600130328                 oBEloc = BLPlod;
050700130328                 oBEprv = BLPprd;
050800130328                 oBEnaz = BLPnzd;
050900130328
051000130328               Else;
051100130328
051200130328                 // -?Recupero la ragione sociale del mittente da FNBLP?
051300130328                 //  ?se non ho agganciato il TITAS altrimenti la recupero?
051400130328                 //  ?da TITAA?
051500130328                 if  %found(TITAS30C);
051600130328                   oBErag = TAArsc;
051700130328                   oBEvia = TAAind;
051800130328                   oBEcap = TAAcap;
051900130328                   oBEloc = TAAloc;
052000130328                   oBEprv = TAAprv;
052100130328                   oBEnaz = TAAnaz;
052200130328                 else;
052300130328                   oBErag = BLPrsm;
052400130328                   oBEvia = BLPinm;
052500130328                   oBEcap = BLPcam;
052600130328                   oBEloc = BLPlom;
052700130328                   oBEprv = BLPprm;
052800130328                   oBEnaz = BLPnzm;
052900130328                 endif;
053000130328
053100130328               EndIf;
053200130327
053300130328           ENDSL;
053400130328
053500130327
053600130328       ENDSL;
053700130410
053800130410
053900130410       // ?Se non c'è rivalsa ed il pagamento è a carico del vettore?
054000130410       // ?recupero i dati aziendali della BRT                      ?
054100130410       IF  oBEriv = *blanks  and  iBEpgd = 'V';
054200130410         clear TIBS02DS;
054300130410         T02mod = 'C';
054400130410         t02sif = knsif;
054500130410         T02cod = 'STD';
054600130410         T02ke1 = 'BAR';
054700130410         TNTBE_RicercaControllo (kpjba:tibs02ds);
054800130410         clear  dSTD2V;
054900130410         If  T02err = *blanks;
055000130410           dSTD2V = T02uni;
055100130410           oBErag = V§STD2rsc;
055200130410           oBEvia = V§STD2ind;
055300130410           oBEcap = V§STD2cap;
055400130410           oBEloc = V§STD2loc;
055500130410           oBEprv = V§STD2prv;
055600130410           oBEnaz = *blanks;
055700130410           clear  oBEksc;
055800150930           clear  oBEinv;
055900150930           clear  oBEima;
056000150930           clear  oBEkma;
056100130410         Endif;
056200130410       ENDIF;
056300081229
056400130328
056500130328       // -?Fine?
056600130328       return;
056700130327
056800081117       //--------------------------------------------------------------
056900130327       //?Reperimento Codice Cliente per luogo 006                     ?
057000081117       //--------------------------------------------------------------
057100130327       BEGSR  Rep_CodCli;
057200130327
057300130328         // -?Dalla spedizione determino il codice beneficiario?
057400130327         //*
057500130327         //* Per recuperare Mitt/Dest utilizzo il porto sped. che coincide con
057600130327         //*  quello C.A. tranne se la sped. è "Mamma di cambio porto" e mi trovo
057700130327         //*  in filiale, in questo caso inverto il porto C.A.
057800130328         TASaas = iBEaas;
057900130328         TASlnp = iBElnp;
058000130328         TASnrs = iBEnrs;
058100130328         TASnsp = iBEnsp;
058200130328         chain  %kds(k05tas : 4)  TITAS30C;
058300130328         Kkey = BLPtbl;
058400130328         chain  (1 : 'TB' : Kkey)  TABEL00F;
058500130328         if  %found(TABEL00F);
058600130327           dsTB = TBLuni;
058700130327         endif;
058800130328         if  §TBrbl = 'R'  or  BLPtbl = 'AP';
058900130328           reade  %kds(k05tas : 4)  TITAS30C;
059000130327         endif;
059100130327
059200130328         TAAaas = iBEaas;
059300130328         TAAlnp = iBElnp;
059400130328         TAAnrs = iBEnrs;
059500130328         TAAnsp = iBEnsp;
059600130327         TAAtrc = 'M';
059700130329         chain  %kds(k05taa : 5)  TITAA30C;
059800130329
059900130329         If  not  %found(TITAS30C);
060000130328           BLPaas = iBEaas;
060100130328           BLPlnp = iBElnp;
060200130328           BLPnrs = iBEnrs;
060300130328           BLPnsp = iBEnsp;
060400130328           chain  %kds(k04blp : 4)  FNBLP01L;
060500081117         Endif;
060600130327
060700130328         // -?Se "Mamma di cambio porto" per ottenere il tipo porto della?
060800130328         //  ?spedizione inverto quello della C.A.?
060900130327         select;
061000130327           when  §DCTcca  <> '9';
061100130327             w§DCTport = §DCTport;
061200130327           when  §DCTport =  'A';
061300130327             w§DCTport = 'F';
061400130327           when  §DCTport =  'F';
061500130327             w§DCTport = 'A';
061600130327         endsl;
061700081118
061800130328         // -?1ª impostazione del codice cliente da utilizzare?
061900130327         SELECT;
062000130328           WHEN  iBEptr = 'D'  and  W§dctport = 'F';
062100130328           WHEN  iBEptr = 'M'  and  W§DCTport = 'A';
062200130328             Kcli = BLPccm;
062300130327           OTHER;
062400130328             Kcli = BLPksc;
062500130327         ENDSL;
062600130328
062700130327
062800130328         // -?Se assegnato in partenza verifico il codice destinatario?
062900130327         clear   AR6ksc ;
063000130328         if  iBEtpb  = 'P'  and  w§DCTport = 'A' ;
063100130328           AR6aas = iBEaas;
063200130328           AR6lnp = iBElnp;
063300130328           AR6nrs = iBEnrs;
063400130328           AR6nsp = iBEnsp;
063500130328           AR6trc = '1';
063600130328           chain  %kds(k05ar601 : 5)  FIAR6000;
063700081117         endif;
063800081117
063900130328         // -?Determino il codice cliente da utilizzare per verifica?
064000130328         //  ?luogo 6 per danni?
064100081117         select;
064200130328           // -?Destinatario PA?
064300130328           when  iBEptr  = 'D' and  w§DCTport = 'A';
064400130327             if  AR6ksc <> *zeros ;
064500130327               Kcli = AR6ksc ;
064600130327             else ;
064700130327               Kcli = BLPksc ;
064800130327             endif;
064900130328           // -?mittente PF?
065000130328           when  iBEptr = 'M' and  w§DCTport = 'F';
065100130327             Kcli   = BLPksc ;
065200130328           // -?mittente PA?
065300130328           when  iBEptr = 'M' and  w§DCTport = 'A';
065400130327             Kcli   = BLPccm ;
065500081117         endsl;
065600130327
065700130327       ENDSR;
065800130327
065900130327       //--------------------------------------------------------------
066000130327       //?Reperimento DEPOT DPD.                                       ?
066100130327       //--------------------------------------------------------------
066200130327       BEGSR  sr_Depot_DPD;
066300130328
066400130328         clear  $FoundCAP;
066500130327
066600130327         // -?Verifica se spedizione DPD?
066700130328         //  ?(dal codice cliente Mittente)?
066800130419         wFil = Kcli / 10000;
066900130328         if  wFil <> ORGfil;
067000130328           chain  (wFil)  AZORG;
067100130328         endif;
067200130328         if  %found(AZORG01L);
067300130328           Og143ccm = ORGde3;
067400130328         else;
067500130328           clear  Og143ccm;
067600130328         endif;
067700130328
067800130328         if  §CMntw <> 'DPD';
067900130328           leavesr;
068000130328         endif;
068100130327
068200171103         // -?Verifica se beneficiario DPD?
068300171103         if  w0030<> ORGfil;
068400130327           chain  (w0030)  AZORG;
068500130327         endif;
068600130327         if  %found(AZORG01L);
068700130327           Og143ccm = ORGde3;
068800130327         else;
068900130327           clear  Og143ccm;
069000130327         endif;
069100130327
069200130327         if  §CMntw <> 'DPD';
069300130327           leavesr;
069400130327         endif;
069500130327
069600130419         // -?Forzo la lingua inglese per DPD?
069700130419         oBEing = *on;
069800130419
069900130327         // -?Reperimento DEPOT di competenza (dal parcel number)?
070000171102         //  ?- su FIPND?
070100171102         PNDaas = iBEaas;
070200171102         PNDlnp = iBElnp;
070300171102         PNDnrs = iBEnrs;
070400171102         PNDnsp = iBEnsp;
070500171102         chain  %kds(k04pnd)  FIPND01L;
070600130327
070700171106         if %found(FIPND01L) ;
070800171106          clear TISIEDEPDS;
070900171106          clear O_DPCDP10FDS ;
071000171106          CLEAR O_DPLEG10FDS ;
071100171102
071200171106          eval DpndDP5 = PNDetiu ;
071300171106
071400171106         // -Se spedizione IMPORT prendo il depot di partenza se spedizione export il depot
071500171106         //  di arrivo
071600171102         // -?Se Depot di 7 uguale a blank o zero imposto il depot di 4 ?
071700171106
071800171106         // IMPORT
071900171106          If  IBEtisp = 'I' ;
072000171106           if  PNDIDP7 = *blank or PNDidp7 = '0000000' ;
072100171106               I_depot4 = %int(%subst(PNDipn: 1: 4));
072200171106           else ;
072300171106               I_depot7 = %int(PNDidp7);
072400171106           endif;
072500171106          ENDIF;
072600171106
072700171106
072800171106          // EXPORT
072900171106          If  IBEtisp = 'E' ;
073000171106           if  PNDIDA7 = *blank or PNDida7 = '0000000' ;
073100171106               I_depot4 = %int(§PNDIDAST);
073200171106           else ;
073300171106               I_depot7 = %int(PNDida7);
073400171106           endif;
073500171106          ENDIF;
073600171106
073700171102         // -Chiamo il TISIEDEPR per recuperare l'anagrafica Depot
073800171106           TISIEDEPR (TISIEDEPds:O_DPCDP10FDS:O_DPLEG10FDS);
073900171102
074000171102         // -Se la versione del cappario o l'anagrafica non è stato trovato esco
074100171106          if O_FlgVers = '1' or O_FlgCapp = '1' ;
074200171106            leavesr;
074300171106          ENDIF;
074400171102
074500130327         // -?Devono essere TUTTI valorizzati?
074600171106          if  CDPname1 <= *blank  or
074700171115              CDPstret <= *blank  or
074800171106              CDPptc  <= *blank  or
074900171106              CDPcity  <= *blank  or
075000171106              CDPiso2 <= *blank;
075100171106            leavesr;
075200171106          endif;
075300130327
075400171106          // -?Conversione CAP?
075500171106          setll  ( 1 : '15' )  TABEL;
075600171106          reade  ( 1 : '15' )  TABEL;
075700171106          DoW  Not  %eof(TABEL00F);
075800171106            if  tblFLG = *blank;
075900171106              ds15 = tblUNI;
076000171106              if  §15cod = CDPiso2;
076100171106                $FoundCAP = *on;
076200171106                leave;
076300171106              endif;
076400171106            endif;
076500171106            reade  ( 1 : '15' )  TABEL;
076600171106          EndDo;
076700171106          if  Not  $FoundCAP;
076800171106            leavesr;
076900171106          endif;
077000130328
077100171106          // -?Conversione in maiuscolo (Uppercase = "1")  ed?
077200171106          //  ?eliminazione di caratteri NON ammessi (CharSost = " ")?
077300171115          TxtInOut  = %subst( CDPstret : 1 : 30 );
077400171106          Uppercase = '1';
077500171106          ChkNull   = '1';
077600171106          xChkChar ( TxtInOut :
077700171106                     ElencoChar :
077800171106                     TipoElenco :
077900171106                     CharSost :
078000171106                     Uppercase :
078100171106                     ChkNull :
078200171106                     CharNull :
078300171106                     Esito);
078400171106          if  Esito < '0';      //"I" / "E" = ERRORE?
078500171106            leavesr;
078600171106          endif;
078700130327
078800130328         // -?Impostazione dati anagrafici per DPD?
078900171106          oBErag = CDPname1 + %subst( CDPname2 : 1 : 13 );
079000130328         //oBEvia = CDPadr1;
079100171106          oBEvia = TxtInOut;
079200171106          oBEcap = CDPptc;
079300171106          oBEloc = %subst( CDPcity : 1 : 35 );
079400171106          clear  oBEprv;
079500171106          oBEnaz = TBLkey;
079600130328
079700130328         //if  CDPeml <> *blank; ......................................
079800130328         //  oBEinv = 'M';       ......................................
079900130328         //  oBEima = CDPeml;    ......................................
080000130328         //  oBEkma = oBEksc;    ......................................
080100130328         //endif;                ......................................
080200130327
080300171106         Endif;
080400130327       ENDSR;
080500130327
080600130327       //--------------------------------------------------------------
080700130327       //?Impostazione dati anagrafici da FNSPE01L.                    ?
080800130327       //--------------------------------------------------------------
080900130327       BEGSR  sr_DecodCli_FNDKA;
081000130327
081100130328         chain  %kds( k04dka01 : 4 )  FNDKA000;
081200130328
081300130328         If  %found(fndka01l) and DKAatb = *blanks;
081400150701           oBEkcc = DKAkcc;
081500130328           oBErag = DKArag;
081600130328           oBEvia = DKAvia;
081700130328           oBEcap = DKAcap;
081800130328           oBEloc = DKAloc;
081900130328           oBEprv = DKAprv;
082000130328           oBEnaz = DKAnaz;
082100130328           oBEfax = DKAfax;
082200130328           oBEtel = DKAtel;
082300130328         EndIf;
082400130327
082500130327       ENDSR;
082600130327
082700130327       //--------------------------------------------------------------
082800130327       //?Reperimento dati anagrafici del cliente.                     ?
082900130327       //--------------------------------------------------------------
083000130410       BEGSR  sr_DecodCli_CNACO;
083100130327
083200130327         TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
083300130327
083400150701         oBEkcc = ACOkcc;
083500130328         oBErag = ACOrag;
083600130328         oBEvia = INDvia;
083700130328         oBEcap = INDcae;
083800130328         oBEloc = INDcit;
083900130328         oBEprv = INDprv;
084000130328         oBEnaz = INDsta;
084100130328         oBEfax = INDtlf;
084200130328         oBEtel = INDtel;
084300130327
084400130327         if  %subst(CLSflo:7:1) <> *blanks;
084500130328           oBEing = *on;
084600130327         endif;
084700130327
084800130327       ENDSR;
084900130327
085000130327       //--------------------------------------------------------------
085100130327       //?Impostazione dati anagrafici da FNSPE01L.                    ?
085200130327       //--------------------------------------------------------------
085300130327       BEGSR  sr_DatiAna_FNSPE;
085400130327
085500150701         oBEkcc = c_KCC_I;
085600130328         oBErag = SPErag;
085700130328         oBEvia = SPEind;
085800130328         oBEcap = SPEcap;
085900130328         oBEloc = SPEloc;
086000130328         oBEprv = SPEpro;
086100130328         oBEnaz = SPEnaz;
086200130328         oBEfax = SPEfax;
086300130328         oBEtel = SPEtel;
086400130328         oBEper = SPEper;
086500130327
086600130327       ENDSR;
086700130327
086800130328      /End-Free
