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
001600130328     /*CMD  ovrdbf file(FNBLP01L) tofile(FILTRAPRD/FNBLP01L) +
001700130327     /*CMD         ovrscope(*calllvl)
002000130328     /*CMD  ovrdbf file(FIAR601L) tofile(FILTRAPRD/FIAR601L) +
002100130328     /*CMD         ovrscope(*calllvl)
002200130328     /*END  dltovr file(*all) lvl(*)
002300150729
002400130327
002500130327       //--------------------------------------------------------------
002600130327       //?Specifiche di controllo.                                     ?
002700130327       //--------------------------------------------------------------
002800130327
002900081113     h decedit('0,') datedit(*dmy/) option(*nodebugio)
003000130327
003100130327       //--------------------------------------------------------------
003200130327       //?Dichiarazione file.                                          ?
003300130327       //--------------------------------------------------------------
003400081113
003500081113     fFNDKA01L  if   e           k disk
003501171102     fFIPND01L  if   e           k disk
003600130327
003700130327     fTITAS30C  if   e           k disk    usropn prefix(BLP:3)
003800130327     f                                     extfile(wLibSede)
003900130327     fTITAA30C  if   e           k disk    usropn
004000130327     f                                     extfile(wLibSede)
004300130327
004400130327     fFNBLP01L  if   e           k disk    usropn extfile(wLibFil)
004500130327     fFIAR601L  if   e           k disk    usropn extfile(wLibFil)
004800130327     fFNSPE01L  if   e           k disk
004900130327     fFNSP201L  if   e           k disk
005000081114
005100081114     fTFNTC01L  if   e           k disk
005200130327
005400081117
005500130327     fTABEL00F  if   e           k disk
005600130328     fAZORG01L  if   e           k disk
005700150701
005800150701       //--------------------------------------------------------------
005900150701       //?Definizione costanti.                                        ?
006000150701       //--------------------------------------------------------------
006100150701
006200150701       // -?Capoconto clienti Italia?
006300150701     d c_KCC_I         c                   const(0151)
006400130327
006500130328       //--------------------------------------------------------------
006600130328       //?Definizione strutture dati.                                  ?
006700130328       //--------------------------------------------------------------
006800130327
006900130328       // -?Parametri ricevuti?
007000130327     d FIDNBEds      e ds
007100171106     d FIDNBE2ds     e ds
007300081114
007400130328       // -?Flag operativi in FNDCT?
007500081118     d dDCT01        e ds                  inz
007600090107
007601171106       // -?Flag etichetta DPD ?
007602171106     d dPNDDP5       e ds                  inz
007603171106
007700130328       // -?Tabella tipo bolla?
007800130327     d dsTB          e ds                  inz
007900130327
008000130328       // -?Tabella nazioni?
008100130327     d ds15          e ds                  inz
008200081118
008300130328       // -?Dati intestatari Azienda?
008400130327     d dSTD2V        e ds                  extname (DSTD2)  Prefix(V)
008500130327
008600130328       // -?Dati da AZORG00F?
008700130327     d OG143ccm      e ds                  inz     extname(OG143)
008800130327     d                                             prefix(§CM:3)
008900081229
009000130328       // -?KPJBA?
009100130327     d KPJBA         e ds
009200130327
009300130327      //---------------------------------------------------------------
009400130327      //?Ridefinizione sottoconto?
009500130327      //---------------------------------------------------------------
009600130327
009700130327     d wDS_ksc         ds             7    inz
009800130327     d   w0030                        3  0 inz
009900130327     d   w0040                        4  0 inz
010000130328
010100130328       //--------------------------------------------------------------
010200130328       //?Definizione variabili globali.                               ?
010300130328       //--------------------------------------------------------------
010400130328
010500130328       // -?Flags booleani?
010600130328     d $FoundCap       s               n   inz
010700130328
010800130328       // -?Campi di comodo?
010900130328     d wLibSede        s             21    inz
011000130328     d wLibSedeP       s             21    inz('GAITRAGRPS/TITAS30C')
011100130328     d wLibSedeB       s             21    inz('GAITRAGRU /TITAS30C')
011200130328     d wLibSedePA      s             21    inz('GAITRAGRPS/TITAA30C')
011300130328     d wLibSedeBA      s             21    inz('GAITRAGRU /TITAA30C')
011600130328     d wLibFil         s             21    inz
011700130328     d wLibFilP        s             21    inz('FILTRAPRD /xxxxxxxx')
011800130328     d wLibFilB        s             21    inz('FILTRA201 /xxxxxxxx')
011900130328     d Kcli            s                   like(SPEcli)   inz
012000130328     d Kkey            s                   like(TBLkey)   inz
012100130328     d w§DCTport       s                   like(§dctport) inz
012200130328     d wFil            s                   like(ORGfil)   inz
012300130327
012400130327      //---------------------------------------------------------------
012500130328      //?Definizione prototipi procedure usate.                        ?
012600081114      //---------------------------------------------------------------
012700081114
012800130328       // -?Reperimento dati anagrafici?
012900130327      /copy gaitrasrc/srcProtoPI,TIBS69R
013000130327      /copy gaitrasrc/srcProtoPR,TIBS69R
013100081114
013200130328       // -?Ricerca/Controllo tabelle in TNTBE00F?
013300130328     d TIBS02ds      e ds                  inz
013400130327      /copy gaitrasrc/srcProtoPR,TIBS02R
013500130328
013501171102       // -?Ricerca/Controllo versione e anagrafico cappario DPD ?
013502171103     d O_DPCDP10Fds  E ds                  extname(DPCDP10F)
013503171103     d O_DPLEG10Fds  E ds                  extname(DPLEG10F)
013504171103     d TISIEdepDS    E ds                  inz
013505171102
013507171102     d TISIEDEPR       pr                  extpgm('TISIEDEPR')
013508171103     d  TISIEdepds                         likeds(tisiedepds)
013509171103     d  O_DPCDP10Fds                       likeds(O_DPCDP10Fds)
013510171103     d  O_DPLEG10Fds                       likeds(O_DPLEG10Fds)
014000130328
014100130328       // -?"Bonifica" stringa da caratteri indesiderati?
014200130328      /copy gaitrasrc/srcProtoPI,XCHKCHAR
014300130328      /copy gaitrasrc/srcProtoPR,XCHKCHAR
014400081229
014500081114      //---------------------------------------------------------------
014600130327      //?Definizione key-list.                                         ?
014700081114      //---------------------------------------------------------------
014800081118
014900130328       // -?File FNDKA01L?
015000130328     d k04dka01      e ds                  extname(FNDKA01L:*key)
015100130328     d                                     inz
015200130328
015300130328       // -?File TITAS30C?
015400081117     d k05tas        e ds                  extname(TITAS30C:*key)
015500081117     d                                     inz
015600130328       // -?File TITAA30C?
015700081223     d k05taa        e ds                  extname(TITAA30C:*key)
015800081223     d                                     inz
015801171102       // -?File FIPND01L?
015802171102     d k04PND        e ds                  extname(FIPND01L:*key)
016200130328
016300130328       // -?File FNBLP01L?
016400130328     d k04blp        e ds                  extname(FNBLP01L:*key)
016500130328     d                                     inz
016900130328       // -?File FIAR601L?
017000081118     d k05ar601      e ds                  extname(FIAR601L:*key)
017100081113     d                                     inz
017200130328
017300130328       // -?File FNSPE01L?
017400081114     d k03spe01      e ds                  extname(FNSPE01L:*key)
017500081114     d                                     inz
017600130328       // -?File FNSP201L?
017700081114     d k03sp201      e ds                  extname(FNSP201L:*key)
017800081114     d                                     inz
017900130328       // -?File TFNTC01L?
018000081114     d k04ntc01      e ds                  extname(TFNTC01L:*key)
018100081114     d                                     inz
018200130328       // -?File DPCDP01L?
018300130328     d k02dpcdp01    e ds                  extname(DPCDP01L:*key)
018400130328     d                                     inz   prefix(k_)
018401171102       // -Recupero versione cappario e anagrafico Cappario
018506171102       // -
018507171102
018509171102
018600130327       //-------------------------------------------------------------*
018700130327       //?Decodifica cliente, verifica se testo inglese e/o e-mail   ?*
018800130327       //-------------------------------------------------------------*
018900081113     c     *Entry        plist
019000130327     c                   parm                    FIDNBEds
019100171106     c                   parm                    FIDNBE2ds
019200130327
019300130328      /Free
019400130327
019500130327       *inLR = *on;
019600130327
019700130327       // -?Apertura file?
019800130328       if  iBEsif = 'P';
019900130328         wLibSede = wLibSedeP;
020000130328       else;
020100130328         wLibSede = wLibSedeB;
020200081212       endif;
020300130328       open  TITAS30C;
020400130327
020500130328       if  iBEsif = 'P';
020600130328         wLibSede = wLibSedePA;
020700130328       else;
020800130328         wLibSede = wLibSedeBA;
020900081223       endif;
021000130328       open  TITAA30C;
021100130328
021200130328       if  iBEsif = 'P';
021300130328         wLibFil  = wLibFilP;
021400130328       else;
021500130328         wLibFil  = wLibFilB;
021600130328       endif;
021700130328
021800130328       %subst(wLibFil:12) = 'FNBLP01L';
021900130328       open  FNBLP01L;
022000130328
022100130328       %subst(wLibFil:12) = 'FIAR601L';
022200130328       open  FIAR601L;
022300081118
022400130327
022500130410       dDCT01  = iBEflo;
022600130328       wDS_ksc = %editc(iBEksc:'X');
022700081118
022800130328
022900130410       // -?Verifico SE c'è rivalsa e aggancio FNDKA?
023000130328       DKAaac = iBEaac;
023100130328       DKAfil = iBEfil;
023200130328       DKAnca = iBEnca;
023300130327       DKAtrc = 'R';
023400130327       exsr  sr_DecodCli_FNDKA;
023500130328
023600130328
023700130328       SELECT;
023800130328
023900130328
024000130328         // ?Si tratta di rivalsa?
024100130328         WHEN  %found(FNDKA01L)  and  DKAatb = *blanks;
024200130328           oBEriv = 'S';
024300150710           // -?Restituisco il codice Beneficiario solo se richiesto?
024400150710           If  iBEmod = '*';
024500150710             oBEkcc = DKAkcc;
024600150710             oBEksc = DKAksc;
024700150710           EndIf;
024800130328
024900081118
025000130328         // ?NON si tratta di rivalsa      ?
025100130328         // ?· CON Beneficiario Alternativo?
025200130328         WHEN  iBEksc > *zeros;
025300130327
025400130327           // -?Se il Beneficiario alternativo è generico aggancio FNDKA,?
025500130327           IF  w0040 = 8888;
025600130327
025700150706             // -?Se il beneficiario è generico restituisco il codice?
025800150706             //  ?solo se richiesto?
025900150706             If  iBEmod = '*';
026000150706               oBEkcc = c_KCC_I;
026100150706               oBEksc = iBEksc;
026200150706             EndIf;
026300150706
026400130328             DKAaac = iBEaac;
026500130328             DKAfil = iBEfil;
026600130328             DKAnca = iBEnca;
026700130327             DKAtrc = 'C';
026800130327             exsr  sr_DecodCli_FNDKA;
026900130327
027000130328           // -?Altrimenti recupero l'anagrafica da CNACO?
027100081114           ELSE;
027200130327
027300130328             Kcli   = iBEksc ;
027400130328             oBEksc = Kcli;
027500081114             clear  tibs69ds;
027600130328             I69kac = iBEksc;
027700130328             I69kin = iBEksc;
027800130328             I69kcs = iBEksc;
027900130327             exsr  sr_DecodCli_CNACO;
028000130327
028100150729             // -?Controllo se esiste il luogo per il beneficiario?
028200150729             //  ?alternativo:?
028300150729             // ·?Se ricevuto codice luogo preferenziale "008"?
028400150729             //  ?(vedi *pgm FIDN29R): cerco SOLO lì?
028500150729             If  iBEpre = '008';
028600130327               SPEfls = 'L'   ;
028700130328               SPEcli = iBEksc;
028800130327               SPEcod = '008' ;
028900130327               chain %kds(k03spe01 : 3) FNSPE000;
029000150729             // ·?Altrimenti: cerco il luogo "006"?
029100150729             Else;
029200130327               SPEfls = 'L'   ;
029300130328               SPEcli = iBEksc;
029400130327               SPEcod = '006' ;
029500130327               chain %kds(k03spe01 : 3) FNSPE000;
029600150729             EndIf;
029700081114
029800130328             // ?Se esiste in FNSPE?
029900130327             IF  %found(FNSPE01L);
030000130327
030100130327               exsr  sr_DatiAna_FNSPE;
030200130327
030300130327               // -?Controllo se esiste nel luogo del beneficiario?
030400130327               //  ?alternativo l'indirizzo e-mail?
030500130328               SP2cli = iBEksc;
030600081219               SP2cod = SPEcod;
030700081114               SP2tpe = 'EM'  ;
030800130328               chain  %kds(k03sp201 : 3)  FNSP2000;
030900081114
031000130328               // -?Se esiste in FNSP2?
031100130328               if  %found(FNSP201L)  and  SP2est <> *blank;
031200130328                 oBEinv = 'M';
031300130328                 oBEima = SP2est;
031400130328                 oBEkma = iBEksc;
031500081114               endif;
031600130327
031700130328             // ?Se NON esiste in FNSPE?
031800081114             ELSE;
031900130327
032000130327               // -?Controllo se esiste nelle note con codice 087?
032100130327               //  ?l'indirizzo e-mail?
032200130328               if   oBEinv = *blanks ;
032300130327                 NTCapl = 'C' ;
032400150701                 NTCnk1 = %editc(c_KCC_I:'X') + %editc(iBEksc:'X');
032500130327                 clear  NTCnk2;
032600130327                 NTCtnt = '87';
032700081114                 chain %kds(k04ntc01 : 4) TFNTC;
032800081114
032900130328                 // -?Se esiste in TFNTC?
033000130328                 if  %found(TFNTC01L)  and  NTCrnt <> *blanks;
033100130328                   oBEinv = 'M';
033200130328                   oBEima = NTCrnt ;
033300130328                   oBEkma = iBEksc ;
033400081114                 endif;
033500081114               endif;
033600130327
033700130327             ENDIF;
033800081114
033900130327           ENDIF;
034000130328
034100081114
034200130328         // ?NON si tratta di rivalsa        ?
034300130328         // ?· SENZA Beneficiario Alternativo?
034400130328         OTHER;
034500130327
034600130328           // -»?se codice passato uguale a zero recupero il codice?
034700130327           exsr  Rep_CodCli;
034800130328           wDS_ksc = %editc( Kcli : 'X' );
034900130328
035000130328           // -?SE richiesto (cioè SE richiamato con 2° parametro),?
035100130328           //  ?SE non si tratta di rivalsa (oBEriv = "S")  e?
035200130328           //  ?SENZA beneficiario alternativo (iBEksc = *zero)?
035300130403           //  ?=> verifica SE C.A. con?beneficiario DPD?
035400130328           if  %parms() > 1;
035500130328             exsr  sr_Depot_DPD;
035600130328           endif;
035700130327
035800130328           SELECT;
035900130328
036000130328             // -?Beneficiario DPD?
036100130328             WHEN  %parms() > 1  and  oBErag <> *blanks;
036200130328
036300130328             // -?Verifico se diverso da 8888 / 9999?
036400130328             WHEN  Kcli > *zeros  and  w0040 <> 8888
036500130328                                  and  w0040 <> 9999;
036600130327
036700130328               oBEksc = Kcli;
036800130328               clear TIBS69ds;
036900130328               I69kac = Kcli;
037000130328               I69kin = Kcli;
037100130328               I69kcs = Kcli;
037200130328               exsr  sr_DecodCli_CNACO;
037300130328
037400150729               // -?Controllo se esiste il luogo per il beneficiario?
037500150729               //  ?alternativo:?
037600150729               // ·?Se ricevuto codice luogo preferenziale "008"?
037700150729               //  ?(vedi *pgm FIDN29R): cerco SOLO lì?
037800150729               If  iBEpre = '008';
037900130328                 SPEfls = 'L'   ;
038000130328                 SPEcli = oBEksc;
038100130328                 SPEcod = '008' ;
038200130328                 chain  %kds(k03spe01 : 3)  FNSPE000;
038300150729               // ·?Altrimenti: cerco il luogo "006"?
038400150729               Else;
038500130328                 SPEfls = 'L'   ;
038600130328                 SPEcli = oBEksc;
038700130328                 SPEcod = '006' ;
038800130328                 chain  %kds(k03spe01 : 3)  FNSPE000;
038900130328               endif;
039000130328
039100130328               // ?Se esiste in FNSPE?
039200130328               IF  %found(FNSPE01L);
039300130328
039400130328                 exsr  sr_DatiAna_FNSPE;
039500130328
039600130328                 // -?Controllo se esiste nel luogo del beneficiario?
039700130328                 //  ?alternativo l'indirizzo e-mail?
039800130328                 SP2cli = oBEksc;
039900130328                 SP2cod = SPEcod;
040000130328                 SP2tpe = 'EM'  ;
040100130328                 chain  %kds(k03sp201 : 3)  FNSP2000;
040200130328
040300130328                 // -?se esiste in FNSP2?
040400130328                 if  %found(FNSP201L)  and  SP2est <> *blank;
040500130328                   oBEinv = 'M';
040600130328                   oBEima = SP2est;
040700130328                   oBEkma = oBEksc;
040800130328                 endif;
040900130328
041000130328               // ?Se NON esiste in FNSPE?
041100130328               ELSE;
041200130328
041300130328                 // -?Controllo se esiste nelle note con codice 087?
041400130328                 //  ?l'indirizzo e-mail?
041500130328                 if   oBEinv = *blanks;
041600130328                   NTCapl = 'C' ;
041700150701                   NTCnk1 = %editc(c_KCC_I:'X') + %editc(oBEksc:'X');
041800130328                   clear  NTCnk2;
041900130328                   NTCtnt = '87';
042000130328                   chain  %kds(k04ntc01 : 4)  TFNTC;
042100130328
042200130328                   // -?se esiste in TFNTC?
042300130328                   if  %found(TFNTC01L)  and  NTCrnt <> *blanks;
042400130328                     oBEinv = 'M';
042500130328                     oBEima = NTCrnt ;
042600130328                     oBEkma = oBEksc ;
042700130328                   endif;
042800130328                 endif;
042900130328
043000130328               ENDIF;
043100130327
043200130328             OTHER;
043300130327
043400150706               // -?Se invece il beneficiario non è codificato restituisco?
043500150706               //  ?il codice solo se richiesto?
043600150706               If  iBEmod = '*';
043700150706                 oBEkcc = c_KCC_I;
043800150706                 oBEksc = Kcli;
043900150706               EndIf;
044000150706
044100130328               // -?Se invece il beneficiario non è codificato recupero i?
044200130328               //  ?dati dalla spedizione?
044300130328               If  iBEptr = 'D';
044400130328
044500130328                 oBErag = BLPrsd;
044600130328                 oBEvia = BLPind;
044700130328                 oBEcap = BLPcad;
044800130328                 oBEloc = BLPlod;
044900130328                 oBEprv = BLPprd;
045000130328                 oBEnaz = BLPnzd;
045100130328
045200130328               Else;
045300130328
045400130328                 // -?Recupero la ragione sociale del mittente da FNBLP?
045500130328                 //  ?se non ho agganciato il TITAS altrimenti la recupero?
045600130328                 //  ?da TITAA?
045700130328                 if  %found(TITAS30C);
045800130328                   oBErag = TAArsc;
045900130328                   oBEvia = TAAind;
046000130328                   oBEcap = TAAcap;
046100130328                   oBEloc = TAAloc;
046200130328                   oBEprv = TAAprv;
046300130328                   oBEnaz = TAAnaz;
046400130328                 else;
046500130328                   oBErag = BLPrsm;
046600130328                   oBEvia = BLPinm;
046700130328                   oBEcap = BLPcam;
046800130328                   oBEloc = BLPlom;
046900130328                   oBEprv = BLPprm;
047000130328                   oBEnaz = BLPnzm;
047100130328                 endif;
047200130328
047300130328               EndIf;
047400130327
047500130328           ENDSL;
047600130328
047700130327
047800130328       ENDSL;
047900130410
048000130410
048100130410       // ?Se non c'è rivalsa ed il pagamento è a carico del vettore?
048200130410       // ?recupero i dati aziendali della BRT                      ?
048300130410       IF  oBEriv = *blanks  and  iBEpgd = 'V';
048400130410         clear TIBS02DS;
048500130410         T02mod = 'C';
048600130410         t02sif = knsif;
048700130410         T02cod = 'STD';
048800130410         T02ke1 = 'BAR';
048900130410         TNTBE_RicercaControllo (kpjba:tibs02ds);
049000130410         clear  dSTD2V;
049100130410         If  T02err = *blanks;
049200130410           dSTD2V = T02uni;
049300130410           oBErag = V§STD2rsc;
049400130410           oBEvia = V§STD2ind;
049500130410           oBEcap = V§STD2cap;
049600130410           oBEloc = V§STD2loc;
049700130410           oBEprv = V§STD2prv;
049800130410           oBEnaz = *blanks;
049900130410           clear  oBEksc;
050000150930           clear  oBEinv;
050100150930           clear  oBEima;
050200150930           clear  oBEkma;
050300130410         Endif;
050400130410       ENDIF;
050500081229
050600130328
050700130328       // -?Fine?
050800130328       return;
050900130327
051000081117       //--------------------------------------------------------------
051100130327       //?Reperimento Codice Cliente per luogo 006                     ?
051200081117       //--------------------------------------------------------------
051300130327       BEGSR  Rep_CodCli;
051400130327
051500130328         // -?Dalla spedizione determino il codice beneficiario?
051600130327         //*
051700130327         //* Per recuperare Mitt/Dest utilizzo il porto sped. che coincide con
051800130327         //*  quello C.A. tranne se la sped. è "Mamma di cambio porto" e mi trovo
051900130327         //*  in filiale, in questo caso inverto il porto C.A.
052000130328         TASaas = iBEaas;
052100130328         TASlnp = iBElnp;
052200130328         TASnrs = iBEnrs;
052300130328         TASnsp = iBEnsp;
052400130328         chain  %kds(k05tas : 4)  TITAS30C;
052500130328         Kkey = BLPtbl;
052600130328         chain  (1 : 'TB' : Kkey)  TABEL00F;
052700130328         if  %found(TABEL00F);
052800130327           dsTB = TBLuni;
052900130327         endif;
053000130328         if  §TBrbl = 'R'  or  BLPtbl = 'AP';
053100130328           reade  %kds(k05tas : 4)  TITAS30C;
053200130327         endif;
053300130327
053400130328         TAAaas = iBEaas;
053500130328         TAAlnp = iBElnp;
053600130328         TAAnrs = iBEnrs;
053700130328         TAAnsp = iBEnsp;
053800130327         TAAtrc = 'M';
053900130329         chain  %kds(k05taa : 5)  TITAA30C;
054000130329
054100130329         If  not  %found(TITAS30C);
054200130328           BLPaas = iBEaas;
054300130328           BLPlnp = iBElnp;
054400130328           BLPnrs = iBEnrs;
054500130328           BLPnsp = iBEnsp;
054600130328           chain  %kds(k04blp : 4)  FNBLP01L;
054700081117         Endif;
054800130327
054900130328         // -?Se "Mamma di cambio porto" per ottenere il tipo porto della?
055000130328         //  ?spedizione inverto quello della C.A.?
055100130327         select;
055200130327           when  §DCTcca  <> '9';
055300130327             w§DCTport = §DCTport;
055400130327           when  §DCTport =  'A';
055500130327             w§DCTport = 'F';
055600130327           when  §DCTport =  'F';
055700130327             w§DCTport = 'A';
055800130327         endsl;
055900081118
056000130328         // -?1ª impostazione del codice cliente da utilizzare?
056100130327         SELECT;
056200130328           WHEN  iBEptr = 'D'  and  W§dctport = 'F';
056300130328           WHEN  iBEptr = 'M'  and  W§DCTport = 'A';
056400130328             Kcli = BLPccm;
056500130327           OTHER;
056600130328             Kcli = BLPksc;
056700130327         ENDSL;
056800130328
056900130327
057000130328         // -?Se assegnato in partenza verifico il codice destinatario?
057100130327         clear   AR6ksc ;
057200130328         if  iBEtpb  = 'P'  and  w§DCTport = 'A' ;
057300130328           AR6aas = iBEaas;
057400130328           AR6lnp = iBElnp;
057500130328           AR6nrs = iBEnrs;
057600130328           AR6nsp = iBEnsp;
057700130328           AR6trc = '1';
057800130328           chain  %kds(k05ar601 : 5)  FIAR6000;
057900081117         endif;
058000081117
058100130328         // -?Determino il codice cliente da utilizzare per verifica?
058200130328         //  ?luogo 6 per danni?
058300081117         select;
058400130328           // -?Destinatario PA?
058500130328           when  iBEptr  = 'D' and  w§DCTport = 'A';
058600130327             if  AR6ksc <> *zeros ;
058700130327               Kcli = AR6ksc ;
058800130327             else ;
058900130327               Kcli = BLPksc ;
059000130327             endif;
059100130328           // -?mittente PF?
059200130328           when  iBEptr = 'M' and  w§DCTport = 'F';
059300130327             Kcli   = BLPksc ;
059400130328           // -?mittente PA?
059500130328           when  iBEptr = 'M' and  w§DCTport = 'A';
059600130327             Kcli   = BLPccm ;
059700081117         endsl;
059800130327
059900130327       ENDSR;
060000130327
060100130327       //--------------------------------------------------------------
060200130327       //?Reperimento DEPOT DPD.                                       ?
060300130327       //--------------------------------------------------------------
060400130327       BEGSR  sr_Depot_DPD;
060500130328
060600130328         clear  $FoundCAP;
060700130327
060800130327         // -?Verifica se spedizione DPD?
060900130328         //  ?(dal codice cliente Mittente)?
061000130419         wFil = Kcli / 10000;
061100130328         if  wFil <> ORGfil;
061200130328           chain  (wFil)  AZORG;
061300130328         endif;
061400130328         if  %found(AZORG01L);
061500130328           Og143ccm = ORGde3;
061600130328         else;
061700130328           clear  Og143ccm;
061800130328         endif;
061900130328
062000130328         if  §CMntw <> 'DPD';
062100130328           leavesr;
062200130328         endif;
062300130327
062400171103         // -?Verifica se beneficiario DPD?
062500171103         if  w0030<> ORGfil;
062600130327           chain  (w0030)  AZORG;
062700130327         endif;
062800130327         if  %found(AZORG01L);
062900130327           Og143ccm = ORGde3;
063000130327         else;
063100130327           clear  Og143ccm;
063200130327         endif;
063300130327
063400130327         if  §CMntw <> 'DPD';
063500130327           leavesr;
063600130327         endif;
063700130327
063800130419         // -?Forzo la lingua inglese per DPD?
063900130419         oBEing = *on;
064000130419
064100130327         // -?Reperimento DEPOT di competenza (dal parcel number)?
064200171102         //  ?- su FIPND?
064201171102         PNDaas = iBEaas;
064202171102         PNDlnp = iBElnp;
064203171102         PNDnrs = iBEnrs;
064204171102         PNDnsp = iBEnsp;
064205171102         chain  %kds(k04pnd)  FIPND01L;
067000130327
067001171106         if %found(FIPND01L) ;
067002171106          clear TISIEDEPDS;
067003171106          clear O_DPCDP10FDS ;
067004171106          CLEAR O_DPLEG10FDS ;
067005171102
067006171106          eval DpndDP5 = PNDetiu ;
067007171106
067008171106         // -Se spedizione IMPORT prendo il depot di partenza se spedizione export il depot
067009171106         //  di arrivo
067010171102         // -?Se Depot di 7 uguale a blank o zero imposto il depot di 4 ?
067011171106
067012171106         // IMPORT
067013171106          If  IBEtisp = 'I' ;
067015171106           if  PNDIDP7 = *blank or PNDidp7 = '0000000' ;
067016171106               I_depot4 = %int(%subst(PNDipn: 1: 4));
067017171106           else ;
067018171106               I_depot7 = %int(PNDidp7);
067019171106           endif;
068301171106          ENDIF;
068302171106
068303171106
068304171106          // EXPORT
068307171106          If  IBEtisp = 'E' ;
068308171106           if  PNDIDA7 = *blank or PNDida7 = '0000000' ;
068309171106               I_depot4 = %int(§PNDIDAST);
068310171106           else ;
068311171106               I_depot7 = %int(PNDida7);
068312171106           endif;
068313171106          ENDIF;
068314171106
068315171102         // -Chiamo il TISIEDEPR per recuperare l'anagrafica Depot
068316171106           TISIEDEPR (TISIEDEPds:O_DPCDP10FDS:O_DPLEG10FDS);
068317171102
068318171102         // -Se la versione del cappario o l'anagrafica non è stato trovato esco
068319171106          if O_FlgVers = '1' or O_FlgCapp = '1' ;
068320171106            leavesr;
068321171106          ENDIF;
068322171102
068400130327         // -?Devono essere TUTTI valorizzati?
068500171106          if  CDPname1 <= *blank  or
068600171115              CDPstret <= *blank  or
068700171106              CDPptc  <= *blank  or
068800171106              CDPcity  <= *blank  or
068900171106              CDPiso2 <= *blank;
069000171106            leavesr;
069100171106          endif;
069200130327
069300171106          // -?Conversione CAP?
069400171106          setll  ( 1 : '15' )  TABEL;
069500171106          reade  ( 1 : '15' )  TABEL;
069600171106          DoW  Not  %eof(TABEL00F);
069700171106            if  tblFLG = *blank;
069800171106              ds15 = tblUNI;
069900171106              if  §15cod = CDPiso2;
070000171106                $FoundCAP = *on;
070100171106                leave;
070200171106              endif;
070300171106            endif;
070400171106            reade  ( 1 : '15' )  TABEL;
070500171106          EndDo;
070600171106          if  Not  $FoundCAP;
070700171106            leavesr;
070800171106          endif;
070900130328
071000171106          // -?Conversione in maiuscolo (Uppercase = "1")  ed?
071100171106          //  ?eliminazione di caratteri NON ammessi (CharSost = " ")?
071200171115          TxtInOut  = %subst( CDPstret : 1 : 30 );
071300171106          Uppercase = '1';
071400171106          ChkNull   = '1';
071500171106          xChkChar ( TxtInOut :
071600171106                     ElencoChar :
071700171106                     TipoElenco :
071800171106                     CharSost :
071900171106                     Uppercase :
072000171106                     ChkNull :
072100171106                     CharNull :
072200171106                     Esito);
072300171106          if  Esito < '0';      //"I" / "E" = ERRORE?
072400171106            leavesr;
072500171106          endif;
072600130327
072700130328         // -?Impostazione dati anagrafici per DPD?
072800171106          oBErag = CDPname1 + %subst( CDPname2 : 1 : 13 );
072900130328         //oBEvia = CDPadr1;
073000171106          oBEvia = TxtInOut;
073100171106          oBEcap = CDPptc;
073200171106          oBEloc = %subst( CDPcity : 1 : 35 );
073300171106          clear  oBEprv;
073400171106          oBEnaz = TBLkey;
073500130328
073600130328         //if  CDPeml <> *blank; ......................................
073700130328         //  oBEinv = 'M';       ......................................
073800130328         //  oBEima = CDPeml;    ......................................
073900130328         //  oBEkma = oBEksc;    ......................................
074000130328         //endif;                ......................................
074100130327
074101171106         Endif;
074200130327       ENDSR;
074300130327
074400130327       //--------------------------------------------------------------
074500130327       //?Impostazione dati anagrafici da FNSPE01L.                    ?
074600130327       //--------------------------------------------------------------
074700130327       BEGSR  sr_DecodCli_FNDKA;
074800130327
074900130328         chain  %kds( k04dka01 : 4 )  FNDKA000;
075000130328
075100130328         If  %found(fndka01l) and DKAatb = *blanks;
075200150701           oBEkcc = DKAkcc;
075300130328           oBErag = DKArag;
075400130328           oBEvia = DKAvia;
075500130328           oBEcap = DKAcap;
075600130328           oBEloc = DKAloc;
075700130328           oBEprv = DKAprv;
075800130328           oBEnaz = DKAnaz;
075900130328           oBEfax = DKAfax;
076000130328           oBEtel = DKAtel;
076100130328         EndIf;
076200130327
076300130327       ENDSR;
076400130327
076500130327       //--------------------------------------------------------------
076600130327       //?Reperimento dati anagrafici del cliente.                     ?
076700130327       //--------------------------------------------------------------
076800130410       BEGSR  sr_DecodCli_CNACO;
076900130327
077000130327         TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
077100130327
077200150701         oBEkcc = ACOkcc;
077300130328         oBErag = ACOrag;
077400130328         oBEvia = INDvia;
077500130328         oBEcap = INDcae;
077600130328         oBEloc = INDcit;
077700130328         oBEprv = INDprv;
077800130328         oBEnaz = INDsta;
077900130328         oBEfax = INDtlf;
078000130328         oBEtel = INDtel;
078100130327
078200130327         if  %subst(CLSflo:7:1) <> *blanks;
078300130328           oBEing = *on;
078400130327         endif;
078500130327
078600130327       ENDSR;
078700130327
078800130327       //--------------------------------------------------------------
078900130327       //?Impostazione dati anagrafici da FNSPE01L.                    ?
079000130327       //--------------------------------------------------------------
079100130327       BEGSR  sr_DatiAna_FNSPE;
079200130327
079300150701         oBEkcc = c_KCC_I;
079400130328         oBErag = SPErag;
079500130328         oBEvia = SPEind;
079600130328         oBEcap = SPEcap;
079700130328         oBEloc = SPEloc;
079800130328         oBEprv = SPEpro;
079900130328         oBEnaz = SPEnaz;
080000130328         oBEfax = SPEfax;
080100130328         oBEtel = SPEtel;
080200130328         oBEper = SPEper;
080300130327
080400130327       ENDSR;
080500130327
080600130328      /End-Free
