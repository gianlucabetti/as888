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
001400130328     /*CMD  ovrdbf file(TITA430C) tofile(GAITRAGRPS/TITA430C) +
001500130328     /*CMD         ovrscope(*calllvl)
001600130328     /*CMD  ovrdbf file(FNBLP01L) tofile(FILTRAPRD/FNBLP01L) +
001700130327     /*CMD         ovrscope(*calllvl)
001800130329     /*CMD  ovrdbf file(FIAR401L) tofile(FILTRAPRD/FIAR401L) +
001900130329     /*CMD         ovrscope(*calllvl)
002000130328     /*CMD  ovrdbf file(FIAR601L) tofile(FILTRAPRD/FIAR601L) +
002100130328     /*CMD         ovrscope(*calllvl)
002200130328     /*END  dltovr file(*all) lvl(*)
002300130327     /*END
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
003600130327
003700130327     fTITAS30C  if   e           k disk    usropn prefix(BLP:3)
003800130327     f                                     extfile(wLibSede)
003900130327     fTITAA30C  if   e           k disk    usropn
004000130327     f                                     extfile(wLibSede)
004100130327     fTITA430C  if   e           k disk    usropn
004200130327     f                                     extfile(wLibSede)
004300130327
004400130327     fFNBLP01L  if   e           k disk    usropn extfile(wLibFil)
004500130327     fFIAR601L  if   e           k disk    usropn extfile(wLibFil)
004600130329     fFIAR401L  if   e           k disk    usropn extfile(wLibFil)
004700130327
004800130327     fFNSPE01L  if   e           k disk
004900130327     fFNSP201L  if   e           k disk
005000081114
005100081114     fTFNTC01L  if   e           k disk
005200130327
005300130327     fDPCDP01L  if   e           k disk
005400081117
005500130327     fTABEL00F  if   e           k disk
005600130328     fAZORG01L  if   e           k disk
005700130327
005800130328       //--------------------------------------------------------------
005900130328       //?Definizione strutture dati.                                  ?
006000130328       //--------------------------------------------------------------
006100130327
006200130328       // -?Parametri ricevuti?
006300130327     d FIDNBEds      e ds
006400130327     d FIDNBEds2       ds
006500130327     d   iBEdpd                       1
006600081114
006700130328       // -?Flag operativi in FNDCT?
006800081118     d dDCT01        e ds                  inz
006900090107
007000130328       // -?Tabella tipo bolla?
007100130327     d dsTB          e ds                  inz
007200130327
007300130328       // -?Tabella nazioni?
007400130327     d ds15          e ds                  inz
007500081118
007600130328       // -?Dati intestatari Azienda?
007700130327     d dSTD2V        e ds                  extname (DSTD2)  Prefix(V)
007800130327
007900130328       // -?Dati da AZORG00F?
008000130327     d OG143ccm      e ds                  inz     extname(OG143)
008100130327     d                                             prefix(§CM:3)
008200081229
008300130328       // -?KPJBA?
008400130327     d KPJBA         e ds
008500130327
008600130327      //---------------------------------------------------------------
008700130327      //?Ridefinizione sottoconto?
008800130327      //---------------------------------------------------------------
008900130327
009000130327     d wDS_ksc         ds             7    inz
009100130327     d   w0030                        3  0 inz
009200130327     d   w0040                        4  0 inz
009300130328
009400130328       //--------------------------------------------------------------
009500130328       //?Definizione variabili globali.                               ?
009600130328       //--------------------------------------------------------------
009700130328
009800130328       // -?Flags booleani?
009900130328     d $FoundCap       s               n   inz
010000130328
010100130328       // -?Campi di comodo?
010200130328     d wLibSede        s             21    inz
010300130328     d wLibSedeP       s             21    inz('GAITRAGRPS/TITAS30C')
010400130328     d wLibSedeB       s             21    inz('GAITRAGRU /TITAS30C')
010500130328     d wLibSedePA      s             21    inz('GAITRAGRPS/TITAA30C')
010600130328     d wLibSedeBA      s             21    inz('GAITRAGRU /TITAA30C')
010700130328     d wLibSedeP4      s             21    inz('GAITRAGRPS/TITA430C')
010800130328     d wLibSedeB4      s             21    inz('GAITRAGRU /TITA430C')
010900130328     d wLibFil         s             21    inz
011000130328     d wLibFilP        s             21    inz('FILTRAPRD /xxxxxxxx')
011100130328     d wLibFilB        s             21    inz('FILTRA201 /xxxxxxxx')
011200130328     d Kcli            s                   like(SPEcli)   inz
011300130328     d Kkey            s                   like(TBLkey)   inz
011400130328     d w§DCTport       s                   like(§dctport) inz
011500130328     d wFil            s                   like(ORGfil)   inz
011600130327
011700130327      //---------------------------------------------------------------
011800130328      //?Definizione prototipi procedure usate.                        ?
011900081114      //---------------------------------------------------------------
012000081114
012100130328       // -?Reperimento dati anagrafici?
012200130327      /copy gaitrasrc/srcProtoPI,TIBS69R
012300130327      /copy gaitrasrc/srcProtoPR,TIBS69R
012400081114
012500130328       // -?Ricerca/Controllo tabelle in TNTBE00F?
012600130328     d TIBS02ds      e ds                  inz
012700130327      /copy gaitrasrc/srcProtoPR,TIBS02R
012800130328
012900130328       // -?Reperimento versione cappario DPD GeoPost?
013000130328     d TISIE2ds      e ds                  inz
013100130328     d tisiE2r         pr                  extpgm('TISIE2R')
013200130328     d   tisiE2ds                          likeds(TISIE2ds)
013300130328
013400130328       // -?"Bonifica" stringa da caratteri indesiderati?
013500130328      /copy gaitrasrc/srcProtoPI,XCHKCHAR
013600130328      /copy gaitrasrc/srcProtoPR,XCHKCHAR
013700081229
013800081114      //---------------------------------------------------------------
013900130327      //?Definizione key-list.                                         ?
014000081114      //---------------------------------------------------------------
014100081118
014200130328       // -?File FNDKA01L?
014300130328     d k04dka01      e ds                  extname(FNDKA01L:*key)
014400130328     d                                     inz
014500130328
014600130328       // -?File TITAS30C?
014700081117     d k05tas        e ds                  extname(TITAS30C:*key)
014800081117     d                                     inz
014900130328       // -?File TITAA30C?
015000081223     d k05taa        e ds                  extname(TITAA30C:*key)
015100081223     d                                     inz
015200130328       // -?File TITA430C?
015300130328     d k05tita430    e ds                  extname(TITA430C:*key)
015400130328     d                                     inz   prefix(k_)
015500130328
015600130328       // -?File FNBLP01L?
015700130328     d k04blp        e ds                  extname(FNBLP01L:*key)
015800130328     d                                     inz
015900130329       // -?File FIAR401L?
016000130329     d k05fiar401    e ds                  extname(FIAR401L:*key)
016100130329     d                                     inz   prefix(k_)
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
017800130327
017900130327       //-------------------------------------------------------------*
018000130327       //?Decodifica cliente, verifica se testo inglese e/o e-mail   ?*
018100130327       //-------------------------------------------------------------*
018200081113     c     *Entry        plist
018300130327     c                   parm                    FIDNBEds
018400130327     c                   parm                    FIDNBEds2
018500130327
018600130328      /Free
018700130327
018800130327       *inLR = *on;
018900130327
019000130327       // -?Apertura file?
019100130328       if  iBEsif = 'P';
019200130328         wLibSede = wLibSedeP;
019300130328       else;
019400130328         wLibSede = wLibSedeB;
019500081212       endif;
019600130328       open  TITAS30C;
019700130327
019800130328       if  iBEsif = 'P';
019900130328         wLibSede = wLibSedePA;
020000130328       else;
020100130328         wLibSede = wLibSedeBA;
020200081223       endif;
020300130328       open  TITAA30C;
020400130328
020500130328       if  iBEsif = 'P';
020600130328         wLibFil  = wLibFilP;
020700130328       else;
020800130328         wLibFil  = wLibFilB;
020900130328       endif;
021000130328
021100130328       %subst(wLibFil:12) = 'FNBLP01L';
021200130328       open  FNBLP01L;
021300130328
021400130328       %subst(wLibFil:12) = 'FIAR601L';
021500130328       open  FIAR601L;
021600081118
021700130327
021800130410       dDCT01  = iBEflo;
021900130328       wDS_ksc = %editc(iBEksc:'X');
022000081118
022100130328
022200130410       // -?Verifico SE c'è rivalsa e aggancio FNDKA?
022300130328       DKAaac = iBEaac;
022400130328       DKAfil = iBEfil;
022500130328       DKAnca = iBEnca;
022600130327       DKAtrc = 'R';
022700130327       exsr  sr_DecodCli_FNDKA;
022800130328
022900130328
023000130328       SELECT;
023100130328
023200130328
023300130328         // ?Si tratta di rivalsa?
023400130328         WHEN  %found(FNDKA01L)  and  DKAatb = *blanks;
023500130328           oBEriv = 'S';
023600130328
023700081118
023800130328         // ?NON si tratta di rivalsa      ?
023900130328         // ?· CON Beneficiario Alternativo?
024000130328         WHEN  iBEksc > *zeros;
024100130327
024200130327           // -?Se il Beneficiario alternativo è generico aggancio FNDKA,?
024300130327           IF  w0040 = 8888;
024400130327
024500130328             DKAaac = iBEaac;
024600130328             DKAfil = iBEfil;
024700130328             DKAnca = iBEnca;
024800130327             DKAtrc = 'C';
024900130327             exsr  sr_DecodCli_FNDKA;
025000130327
025100130328           // -?Altrimenti recupero l'anagrafica da CNACO?
025200081114           ELSE;
025300130327
025400130328             Kcli   = iBEksc ;
025500130328             oBEksc = Kcli;
025600081114             clear  tibs69ds;
025700130328             I69kac = iBEksc;
025800130328             I69kin = iBEksc;
025900130328             I69kcs = iBEksc;
026000130327             exsr  sr_DecodCli_CNACO;
026100130327
026200130328             // -?Se non c'è nessuna preferenza prendo da 006 mentre se?
026300130327             //  ?c'è luogo 008 nelle preferenze cerco prima lì?
026400130328             if  iBEpre = '008';
026500130327               SPEfls = 'L'   ;
026600130328               SPEcli = iBEksc;
026700130327               SPEcod = '008' ;
026800130327               chain %kds(k03spe01 : 3) FNSPE000;
026900130327             endif;
027000081219
027100130327             // -?Se non esiste oppure non ci sono preferenze vado su 006?
027200130328             if  Not  %found(FNSPE01L)  or  iBEpre <> '008';
027300130328               // -?Controllo se esiste il luogo per il beneficiario?
027400130328               //  ?alternativo?
027500130327               SPEfls = 'L'   ;
027600130328               SPEcli = iBEksc;
027700130327               SPEcod = '006' ;
027800130327               chain %kds(k03spe01 : 3) FNSPE000;
027900081219             endif;
028000081114
028100130328             // ?Se esiste in FNSPE?
028200130327             IF  %found(FNSPE01L);
028300130327
028400130327               exsr  sr_DatiAna_FNSPE;
028500130327
028600130327               // -?Controllo se esiste nel luogo del beneficiario?
028700130327               //  ?alternativo l'indirizzo e-mail?
028800130328               SP2cli = iBEksc;
028900081219               SP2cod = SPEcod;
029000081114               SP2tpe = 'EM'  ;
029100130328               chain  %kds(k03sp201 : 3)  FNSP2000;
029200081114
029300130328               // -?Se esiste in FNSP2?
029400130328               if  %found(FNSP201L)  and  SP2est <> *blank;
029500130328                 oBEinv = 'M';
029600130328                 oBEima = SP2est;
029700130328                 oBEkma = iBEksc;
029800081114               endif;
029900130327
030000130328             // ?Se NON esiste in FNSPE?
030100081114             ELSE;
030200130327
030300130327               // -?Controllo se esiste nelle note con codice 087?
030400130327               //  ?l'indirizzo e-mail?
030500130328               if   oBEinv = *blanks ;
030600130327                 NTCapl = 'C' ;
030700130328                 NTCnk1 = '0151' + %editc(iBEksc:'X');
030800130327                 clear  NTCnk2;
030900130327                 NTCtnt = '87';
031000081114                 chain %kds(k04ntc01 : 4) TFNTC;
031100081114
031200130328                 // -?Se esiste in TFNTC?
031300130328                 if  %found(TFNTC01L)  and  NTCrnt <> *blanks;
031400130328                   oBEinv = 'M';
031500130328                   oBEima = NTCrnt ;
031600130328                   oBEkma = iBEksc ;
031700081114                 endif;
031800081114               endif;
031900130327
032000130327             ENDIF;
032100081114
032200130327           ENDIF;
032300130328
032400081114
032500130328         // ?NON si tratta di rivalsa        ?
032600130328         // ?· SENZA Beneficiario Alternativo?
032700130328         OTHER;
032800130327
032900130328           // -»?se codice passato uguale a zero recupero il codice?
033000130327           exsr  Rep_CodCli;
033100130328           wDS_ksc = %editc( Kcli : 'X' );
033200130328
033300130328           // -?SE richiesto (cioè SE richiamato con 2° parametro),?
033400130328           //  ?SE non si tratta di rivalsa (oBEriv = "S")  e?
033500130328           //  ?SENZA beneficiario alternativo (iBEksc = *zero)?
033600130403           //  ?=> verifica SE C.A. con?beneficiario DPD?
033700130328           if  %parms() > 1;
033800130328             exsr  sr_Depot_DPD;
033900130328           endif;
034000130327
034100130328           SELECT;
034200130328
034300130328             // -?Beneficiario DPD?
034400130328             WHEN  %parms() > 1  and  oBErag <> *blanks;
034500130328
034600130328             // -?Verifico se diverso da 8888 / 9999?
034700130328             WHEN  Kcli > *zeros  and  w0040 <> 8888
034800130328                                  and  w0040 <> 9999;
034900130327
035000130328               oBEksc = Kcli;
035100130328               clear TIBS69ds;
035200130328               I69kac = Kcli;
035300130328               I69kin = Kcli;
035400130328               I69kcs = Kcli;
035500130328               exsr  sr_DecodCli_CNACO;
035600130328
035700130328               // -?Se non c'è nessuna preferenza prendo da 006 mentre se?
035800130328               //  ?c'è luogo 008 nelle preferenze cerco prima lì?
035900130328               if  iBEpre = '008';
036000130328                 SPEfls = 'L'   ;
036100130328                 SPEcli = oBEksc;
036200130328                 SPEcod = '008' ;
036300130328                 chain  %kds(k03spe01 : 3)  FNSPE000;
036400130328               endif;
036500130328
036600130328               // -?Se non esiste oppure non ci sono preferenze vado su 006?
036700130328               if  Not  %found(FNSPE01L)  or  iBEpre <> '008';
036800130328                 // -?Controllo se esiste il luogo per il beneficiario?
036900130328                 //  ?alternativo?
037000130328                 SPEfls = 'L'   ;
037100130328                 SPEcli = oBEksc;
037200130328                 SPEcod = '006' ;
037300130328                 chain  %kds(k03spe01 : 3)  FNSPE000;
037400130328               endif;
037500130328
037600130328               // ?Se esiste in FNSPE?
037700130328               IF  %found(FNSPE01L);
037800130328
037900130328                 exsr  sr_DatiAna_FNSPE;
038000130328
038100130328                 // -?Controllo se esiste nel luogo del beneficiario?
038200130328                 //  ?alternativo l'indirizzo e-mail?
038300130328                 SP2cli = oBEksc;
038400130328                 SP2cod = SPEcod;
038500130328                 SP2tpe = 'EM'  ;
038600130328                 chain  %kds(k03sp201 : 3)  FNSP2000;
038700130328
038800130328                 // -?se esiste in FNSP2?
038900130328                 if  %found(FNSP201L)  and  SP2est <> *blank;
039000130328                   oBEinv = 'M';
039100130328                   oBEima = SP2est;
039200130328                   oBEkma = oBEksc;
039300130328                 endif;
039400130328
039500130328               // ?Se NON esiste in FNSPE?
039600130328               ELSE;
039700130328
039800130328                 // -?Controllo se esiste nelle note con codice 087?
039900130328                 //  ?l'indirizzo e-mail?
040000130328                 if   oBEinv = *blanks;
040100130328                   NTCapl = 'C' ;
040200130328                   NTCnk1 = '0151' + %editc(oBEksc:'X');
040300130328                   clear  NTCnk2;
040400130328                   NTCtnt = '87';
040500130328                   chain  %kds(k04ntc01 : 4)  TFNTC;
040600130328
040700130328                   // -?se esiste in TFNTC?
040800130328                   if  %found(TFNTC01L)  and  NTCrnt <> *blanks;
040900130328                     oBEinv = 'M';
041000130328                     oBEima = NTCrnt ;
041100130328                     oBEkma = oBEksc ;
041200130328                   endif;
041300130328                 endif;
041400130328
041500130328               ENDIF;
041600130327
041700130328             OTHER;
041800130327
041900130328               // -?Se invece il beneficiario non è codificato recupero i?
042000130328               //  ?dati dalla spedizione?
042100130328               If  iBEptr = 'D';
042200130328
042300130328                 oBErag = BLPrsd;
042400130328                 oBEvia = BLPind;
042500130328                 oBEcap = BLPcad;
042600130328                 oBEloc = BLPlod;
042700130328                 oBEprv = BLPprd;
042800130328                 oBEnaz = BLPnzd;
042900130328
043000130328               Else;
043100130328
043200130328                 // -?Recupero la ragione sociale del mittente da FNBLP?
043300130328                 //  ?se non ho agganciato il TITAS altrimenti la recupero?
043400130328                 //  ?da TITAA?
043500130328                 if  %found(TITAS30C);
043600130328                   oBErag = TAArsc;
043700130328                   oBEvia = TAAind;
043800130328                   oBEcap = TAAcap;
043900130328                   oBEloc = TAAloc;
044000130328                   oBEprv = TAAprv;
044100130328                   oBEnaz = TAAnaz;
044200130328                 else;
044300130328                   oBErag = BLPrsm;
044400130328                   oBEvia = BLPinm;
044500130328                   oBEcap = BLPcam;
044600130328                   oBEloc = BLPlom;
044700130328                   oBEprv = BLPprm;
044800130328                   oBEnaz = BLPnzm;
044900130328                 endif;
045000130328
045100130328               EndIf;
045200130327
045300130328           ENDSL;
045400130328
045500130327
045600130328       ENDSL;
045700130410
045800130410
045900130410       // ?Se non c'è rivalsa ed il pagamento è a carico del vettore?
046000130410       // ?recupero i dati aziendali della BRT                      ?
046100130410       IF  oBEriv = *blanks  and  iBEpgd = 'V';
046200130410         clear TIBS02DS;
046300130410         T02mod = 'C';
046400130410         t02sif = knsif;
046500130410         T02cod = 'STD';
046600130410         T02ke1 = 'BAR';
046700130410         TNTBE_RicercaControllo (kpjba:tibs02ds);
046800130410         clear  dSTD2V;
046900130410         If  T02err = *blanks;
047000130410           dSTD2V = T02uni;
047100130410           oBErag = V§STD2rsc;
047200130410           oBEvia = V§STD2ind;
047300130410           oBEcap = V§STD2cap;
047400130410           oBEloc = V§STD2loc;
047500130410           oBEprv = V§STD2prv;
047600130410           oBEnaz = *blanks;
047700130410           clear  oBEksc;
047800130410         Endif;
047900130410       ENDIF;
048000081229
048100130328
048200130328       // -?Fine?
048300130328       return;
048400130327
048500081117       //--------------------------------------------------------------
048600130327       //?Reperimento Codice Cliente per luogo 006                     ?
048700081117       //--------------------------------------------------------------
048800130327       BEGSR  Rep_CodCli;
048900130327
049000130328         // -?Dalla spedizione determino il codice beneficiario?
049100130327         //*
049200130327         //* Per recuperare Mitt/Dest utilizzo il porto sped. che coincide con
049300130327         //*  quello C.A. tranne se la sped. è "Mamma di cambio porto" e mi trovo
049400130327         //*  in filiale, in questo caso inverto il porto C.A.
049500130328         TASaas = iBEaas;
049600130328         TASlnp = iBElnp;
049700130328         TASnrs = iBEnrs;
049800130328         TASnsp = iBEnsp;
049900130328         chain  %kds(k05tas : 4)  TITAS30C;
050000130328         Kkey = BLPtbl;
050100130328         chain  (1 : 'TB' : Kkey)  TABEL00F;
050200130328         if  %found(TABEL00F);
050300130327           dsTB = TBLuni;
050400130327         endif;
050500130328         if  §TBrbl = 'R'  or  BLPtbl = 'AP';
050600130328           reade  %kds(k05tas : 4)  TITAS30C;
050700130327         endif;
050800130327
050900130328         TAAaas = iBEaas;
051000130328         TAAlnp = iBElnp;
051100130328         TAAnrs = iBEnrs;
051200130328         TAAnsp = iBEnsp;
051300130327         TAAtrc = 'M';
051400130329         chain  %kds(k05taa : 5)  TITAA30C;
051500130329
051600130329         If  not  %found(TITAS30C);
051700130328           BLPaas = iBEaas;
051800130328           BLPlnp = iBElnp;
051900130328           BLPnrs = iBEnrs;
052000130328           BLPnsp = iBEnsp;
052100130328           chain  %kds(k04blp : 4)  FNBLP01L;
052200081117         Endif;
052300130327
052400130328         // -?Se "Mamma di cambio porto" per ottenere il tipo porto della?
052500130328         //  ?spedizione inverto quello della C.A.?
052600130327         select;
052700130327           when  §DCTcca  <> '9';
052800130327             w§DCTport = §DCTport;
052900130327           when  §DCTport =  'A';
053000130327             w§DCTport = 'F';
053100130327           when  §DCTport =  'F';
053200130327             w§DCTport = 'A';
053300130327         endsl;
053400081118
053500130328         // -?1ª impostazione del codice cliente da utilizzare?
053600130327         SELECT;
053700130328           WHEN  iBEptr = 'D'  and  W§dctport = 'F';
053800130328           WHEN  iBEptr = 'M'  and  W§DCTport = 'A';
053900130328             Kcli = BLPccm;
054000130327           OTHER;
054100130328             Kcli = BLPksc;
054200130327         ENDSL;
054300130328
054400130327
054500130328         // -?Se assegnato in partenza verifico il codice destinatario?
054600130327         clear   AR6ksc ;
054700130328         if  iBEtpb  = 'P'  and  w§DCTport = 'A' ;
054800130328           AR6aas = iBEaas;
054900130328           AR6lnp = iBElnp;
055000130328           AR6nrs = iBEnrs;
055100130328           AR6nsp = iBEnsp;
055200130328           AR6trc = '1';
055300130328           chain  %kds(k05ar601 : 5)  FIAR6000;
055400081117         endif;
055500081117
055600130328         // -?Determino il codice cliente da utilizzare per verifica?
055700130328         //  ?luogo 6 per danni?
055800081117         select;
055900130328           // -?Destinatario PA?
056000130328           when  iBEptr  = 'D' and  w§DCTport = 'A';
056100130327             if  AR6ksc <> *zeros ;
056200130327               Kcli = AR6ksc ;
056300130327             else ;
056400130327               Kcli = BLPksc ;
056500130327             endif;
056600130328           // -?mittente PF?
056700130328           when  iBEptr = 'M' and  w§DCTport = 'F';
056800130327             Kcli   = BLPksc ;
056900130328           // -?mittente PA?
057000130328           when  iBEptr = 'M' and  w§DCTport = 'A';
057100130327             Kcli   = BLPccm ;
057200081117         endsl;
057300130327
057400130327       ENDSR;
057500130327
057600130327       //--------------------------------------------------------------
057700130327       //?Reperimento DEPOT DPD.                                       ?
057800130327       //--------------------------------------------------------------
057900130327       BEGSR  sr_Depot_DPD;
058000130328
058100130328         clear  $FoundCAP;
058200130327
058300130327         // -?Verifica se spedizione DPD?
058400130328         //  ?(dal codice cliente Mittente)?
058500130419         wFil = Kcli / 10000;
058600130328         if  wFil <> ORGfil;
058700130328           chain  (wFil)  AZORG;
058800130328         endif;
058900130328         if  %found(AZORG01L);
059000130328           Og143ccm = ORGde3;
059100130328         else;
059200130328           clear  Og143ccm;
059300130328         endif;
059400130328
059500130328         if  §CMntw <> 'DPD';
059600130328           leavesr;
059700130328         endif;
059800130327
059900130327         // -?Verifica se beneficiario DPD?
060000130327         if  w0030 <> ORGfil;
060100130327           chain  (w0030)  AZORG;
060200130327         endif;
060300130327         if  %found(AZORG01L);
060400130327           Og143ccm = ORGde3;
060500130327         else;
060600130327           clear  Og143ccm;
060700130327         endif;
060800130327
060900130327         if  §CMntw <> 'DPD';
061000130327           leavesr;
061100130327         endif;
061200130327
061300130419         // -?Forzo la lingua inglese per DPD?
061400130419         oBEing = *on;
061500130419
061600130327         // -?Reperimento DEPOT di competenza (dal parcel number)?
061700130329         //  ?- prima su TITA4 (sede),?
061800130329         //    ?poi (se non trovato) su FIAR4 (fil.) -?
061900130328         if  iBEsif  = 'P';
062000130327           wLibSede = wLibSedeP4;
062100130327         else;
062200130327           wLibSede = wLibSedeB4;
062300130327         endif;
062400130327         open  TITA430C;
062500130327         k_TA4aas = iBEaas;
062600130327         k_TA4lnp = iBElnp;
062700130327         k_TA4nrs = iBEnrs;
062800130327         k_TA4nsp = iBEnsp;
062900130327         k_TA4trc = 'I';
063000130327         chain  %kds(k05tita430)  TITA430C;
063100130328         if  Not  %found(TITA430C);
063200130329           // -?Se NON trovato su TITA4 (sede), si prova su FIAR4 (fil.)?
063300130329           %subst(wLibFil:12) = 'FIAR401L';
063400130329           open  FIAR401L;
063500130329           k_AR4aas = k_TA4aas;
063600130329           k_AR4lnp = k_TA4lnp;
063700130329           k_AR4nrs = k_TA4nrs;
063800130329           k_AR4nsp = k_TA4nsp;
063900130329           k_AR4trc = k_TA4trc;
064000130329           chain  %kds(k05fiar401)  FIAR4000;
064100130329           if  Not  %found(FIAR401L);
064200130329             leavesr;
064300130329           endif;
064400130327         endif;
064500130327
064600130327         // -?Reperimento ultima versione BRT del capario DPD?
064700130328         clear  TISIE2ds;
064800130327         SIE2dri = (BLPaas * 10000) + BLPmgs;
064900130328         TISIE2R ( TISIE2ds );
065000130327
065100130327         // -?Reperimento dati anagrafici da DPCDP01L?
065200130327         k_CDPver = SIE2ver;
065300130327         k_CDPdpc = %subst( TA4not : 1 : 4 );
065400130327         chain  %kds( k02dpcdp01 )  DPCDP000;
065500130327         if  Not  %found(DPCDP01L);
065600130327           leavesr;
065700130327         endif;
065800130327
065900130327         // -?Devono essere TUTTI valorizzati?
066000130327         if  CDPnam1 <= *blank  or
066100130327             CDPadr1 <= *blank  or
066200130327             CDPptc  <= *blank  or
066300130327             CDPcty  <= *blank  or
066400130327             CDPiso2 <= *blank;
066500130327           leavesr;
066600130327         endif;
066700130327
066800130327         // -?Conversione CAP?
066900130327         setll  ( 1 : '15' )  TABEL;
067000130327         reade  ( 1 : '15' )  TABEL;
067100130327         DoW  Not  %eof(TABEL00F);
067200130327           if  tblFLG = *blank;
067300130327             ds15 = tblUNI;
067400130327             if  §15cod = CDPiso2;
067500130327               $FoundCAP = *on;
067600130327               leave;
067700130327             endif;
067800130327           endif;
067900130327           reade  ( 1 : '15' )  TABEL;
068000130327         EndDo;
068100130327         if  Not  $FoundCAP;
068200130327           leavesr;
068300130327         endif;
068400130328
068500130328         // -?Conversione in maiuscolo (Uppercase = "1")  ed?
068600130328         //  ?eliminazione di caratteri NON ammessi (CharSost = " ")?
068700130412         TxtInOut  = %subst( CDPadr1 : 1 : 30 );
068800130328         Uppercase = '1';
068900130328         ChkNull   = '1';
069000130328         xChkChar ( TxtInOut :
069100130328                    ElencoChar :
069200130328                    TipoElenco :
069300130328                    CharSost :
069400130328                    Uppercase :
069500130328                    ChkNull :
069600130328                    CharNull :
069700130328                    Esito);
069800140605         if  Esito < '0';      //"I" / "E" = ERRORE?
069900130328           leavesr;
070000130328         endif;
070100130327
070200130328         // -?Impostazione dati anagrafici per DPD?
070300130412         oBErag = CDPnam1 + %subst( CDPnam2 : 1 : 13 );
070400130328         //oBEvia = CDPadr1;
070500130328         oBEvia = TxtInOut;
070600130328         oBEcap = CDPptc;
070700130412         oBEloc = %subst( CDPcty : 1 : 35 );
070800130328         clear  oBEprv;
070900130328         oBEnaz = TBLkey;
071000130328
071100130328         //if  CDPeml <> *blank; ......................................
071200130328         //  oBEinv = 'M';       ......................................
071300130328         //  oBEima = CDPeml;    ......................................
071400130328         //  oBEkma = oBEksc;    ......................................
071500130328         //endif;                ......................................
071600130327
071700130327       ENDSR;
071800130327
071900130327       //--------------------------------------------------------------
072000130327       //?Impostazione dati anagrafici da FNSPE01L.                    ?
072100130327       //--------------------------------------------------------------
072200130327       BEGSR  sr_DecodCli_FNDKA;
072300130327
072400130328         chain  %kds( k04dka01 : 4 )  FNDKA000;
072500130328
072600130328         If  %found(fndka01l) and DKAatb = *blanks;
072700130328           oBErag = DKArag;
072800130328           oBEvia = DKAvia;
072900130328           oBEcap = DKAcap;
073000130328           oBEloc = DKAloc;
073100130328           oBEprv = DKAprv;
073200130328           oBEnaz = DKAnaz;
073300130328           oBEfax = DKAfax;
073400130328           oBEtel = DKAtel;
073500130328         EndIf;
073600130327
073700130327       ENDSR;
073800130327
073900130327       //--------------------------------------------------------------
074000130327       //?Reperimento dati anagrafici del cliente.                     ?
074100130327       //--------------------------------------------------------------
074200130410       BEGSR  sr_DecodCli_CNACO;
074300130327
074400130327         TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
074500130327
074600130328         oBErag = ACOrag;
074700130328         oBEvia = INDvia;
074800130328         oBEcap = INDcae;
074900130328         oBEloc = INDcit;
075000130328         oBEprv = INDprv;
075100130328         oBEnaz = INDsta;
075200130328         oBEfax = INDtlf;
075300130328         oBEtel = INDtel;
075400130327
075500130327         if  %subst(CLSflo:7:1) <> *blanks;
075600130328           oBEing = *on;
075700130327         endif;
075800130327
075900130327       ENDSR;
076000130327
076100130327       //--------------------------------------------------------------
076200130327       //?Impostazione dati anagrafici da FNSPE01L.                    ?
076300130327       //--------------------------------------------------------------
076400130327       BEGSR  sr_DatiAna_FNSPE;
076500130327
076600130328         oBErag = SPErag;
076700130328         oBEvia = SPEind;
076800130328         oBEcap = SPEcap;
076900130328         oBEloc = SPEloc;
077000130328         oBEprv = SPEpro;
077100130328         oBEnaz = SPEnaz;
077200130328         oBEfax = SPEfax;
077300130328         oBEtel = SPEtel;
077400130328         oBEper = SPEper;
077500130327
077600130327       ENDSR;
077700130327
077800130328      /End-Free
