000100130408       //==============================================================
000200130408       //?Stampa Riferimenti & Contatti per cliente                    ?
000300130408       //==============================================================
000400130408
000500130408       //--------------------------------------------------------------
000600130408       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700130408       //--------------------------------------------------------------
000800130408
000900130408     /*PRM  dbgview(*source)
001000130408     /*END
001100130408
001200130408       //--------------------------------------------------------------
001300130408       //?Specifiche di controllo.                                     ?
001400130408       //--------------------------------------------------------------
001500130408
001600130517     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700130408     h dftactgrp(*no)
001800130408     h bnddir('TRUL')
001900130408
002000130408       //--------------------------------------------------------------
002100130408       //?Dichiarazione file.                                          ?
002200130408       //--------------------------------------------------------------
002300130408
002400130408       // -?Note Clienti (per "alla C.A.")?
002500130926     fTFNTC01L  if   e           k disk    prefix(TF)
002600130829
002700130829       // -?Anagrafica Commerciali?
002800130829     fAZCMM01L  if   e           k disk
002900130926     fAZNTC01L  if   e           k disk    prefix(AZ)
003000130408
003100130408       // -?Organigramma di filiale?
003200130408     fAZORG01L  if   e           k disk
003300130408
003400130408       // -?Stampa?
003500130411     fTNSD30P   o    e             printer usropn
003600130408
003700130408       //--------------------------------------------------------------
003800130408       //?Definizione costanti.                                        ?
003900130408       //--------------------------------------------------------------
004000130411
004100130411       // -?Comando di override al file di stampa?
004200130411     d c_CmdOvrPrtf_1  c                   const('OVRPRTF file(TNSD30P) +
004300130411     d                                            formtype(LET1) +
004400130531     d                                            usrdta(''Contatti'')')
004500130531     d c_CmdOvrPrtf_2  c                   const(' tofile(TNSD30P2)')
004600130531     d c_CmdOvrPrtf_3  c                   const(' tofile(TNSD30P3)')
004700130531     d c_CmdOvrPrtf_4  c                   const(' tofile(TNSD30P4)')
004800130531     d c_CmdOvrPrtf_9  c                   const(' copies(')
004900130411     d c_CmdDltOvr     c                   const('DLTOVR file(TNSD30P)')
005000130408
005100130409       // -?Dominio e-mail?
005200130408     d c_atBRT         c                   const('@brt.it')
005300130409
005400130409       // -?Caratteri Maiuscoli?
005500130409     d c_Maiusc        c                   const('ABCDEFGHIJKLM+
005600130409     d                                            NOPQRSTUVWXYZ+
005700130409     d                                            ÀÁÂÃÄÅ+
005800130409     d                                            ÆÇ+
005900130409     d                                            ÈÉÊË+
006000130409     d                                            ÌÍÎÏ+
006100130409     d                                            ÒÓÔÕÖ+
006200130409     d                                            ÙÚÛÜ')
006300130409
006400130409       // -?Caratteri Minuscoli?
006500130409     d c_Minusc        c                   const('abcdefghijklm+
006600130409     d                                            nopqrstuvwxyz+
006700130409     d                                            àáâãäå+
006800130409     d                                            æç+
006900130409     d                                            èéêë+
007000130409     d                                            ìíîï+
007100130409     d                                            òóôõö+
007200130409     d                                            ùúûü')
007300130408
007400130408       //--------------------------------------------------------------
007500130408       //?Definizione schiere.                                         ?
007600130408       //--------------------------------------------------------------
007700130408
007800130408
007900130408       //--------------------------------------------------------------
008000130408       //?Definizione aree dati.                                       ?
008100130408       //--------------------------------------------------------------
008200130408
008300130408       // -?Dati utente?
008400130408     d §AzUte        e ds                  extname(AZUTE00F)
008500130408     d                                     dtaara
008600130408     d §DatiUte      e ds                  extname(dDatiUte)
008700130408     d                                     dtaara
008800130408
008900130408       //--------------------------------------------------------------
009000130408       //?Definizione strutture dati.                                  ?
009100130408       //--------------------------------------------------------------
009200130408
009300130408       // -?Status ds?
009400130408     d Status         sds
009500130408     d   Parms           *parms
009600130408
009700130516       // -?Tabella "15" = Nazioni?
009800130516     d ds15          e ds                  inz
009900130408
010000130408       // -?Dati Organigramma?
010100130423     d Og130         e ds                  inz   qualified
010200130408
010300130408       // -?Parametri ricevuti?
010400130408     d KPJBA         e ds
010500130408     d FNLV45ds      e ds
010600130408     d p_TNSD17ds      s                         like(TNSD17ds)
010700130408       // -?Parametri per stampa organigramma di filiale?
010800130408     d TNSD17ds      e ds                  inz
010900130408
011000130408       //--------------------------------------------------------------
011100130408       //?Definizione variabili globali.                               ?
011200130408       //--------------------------------------------------------------
011300130409
011400130409       // -?Flags booleani?
011500130409     d $FilLiv1        s               n   inz
011600130408
011700130408       // -?Codice lingua per stampa?
011800130517     d*// wCodLin         s              1  0 inz(1)
011900130517     d wCodLinA        s              1    inz('1')
012000130409
012100130409       // -?Campi di comodo?
012200130829     d wCMMfil         s                   inz  like(CMMfil)
012300130926     d wNTCname        s                   inz  like(AZNTCrnt)
012400130926     d*// wNTCmail        s                   inz  like(AZNTCrnt)
012500130516     d wTFA_£6         s                   inz  like(D55tfa)
012600130517     d wDate_Eur       s               d   inz  datfmt(*EUR)
012700130409     d ds_Time14       ds            14    inz
012800130409     d   wDate                        8  0 inz
012900130409     d   wTime                        6  0 inz
013000130408
013100130408       //--------------------------------------------------------------
013200130408       //?Definizione prototipi procedure.                             ?
013300130408       //--------------------------------------------------------------
013400130408
013500130408       // -?Reperimento dati utente?
013600130408     d TIBS34ds      e ds
013700130408      /copy gaitrasrc/srcProtoPR,TIBS34R
013800130408
013900130408       // -?Controllo/Decodifica cliente?
014000130408      /copy gaitrasrc/srcProtoPI,TIBS69R
014100130408      /copy gaitrasrc/srcProtoPR,TIBS69R
014200130408
014300130408       // -?Reperimento dati tabelle?
014400130408      /copy gaitrasrc/srcProtoPI,TRULTAB
014500130408      /copy gaitrasrc/srcProtoPR,TRULTAB
014600130409
014700130409       // -?Reperimento capofila £6 / terminal filiale?
014800130409     d FNLV55ds      e ds                  inz
014900130409     d   D55tla      e                     inz('C')
015000130409      /copy gaitrasrc/srcProtoPR,FNLV55R
015100130411
015200130411       // -?Parametri API QCAPCMD (Process Commands)?
015300130411     d Qcmd            s           2048    inz  varying
015400130411      /copy qSysInc/qRpgleSrc,QCAPCMD
015500130411       // -?API QCAPCMD (Process Commands)?
015600130411      /copy gaitrasrc/srcProtoPR,QCAPCMD
015700130411
015800130411       // -?Parametri gestione errori API.?
015900130411      /copy qsysinc/qrpglesrc,QUSEC
016000130408
016100130408       //--------------------------------------------------------------
016200130408       //?Definizione key-list.                                        ?
016300130408       //--------------------------------------------------------------
016400130408
016500130408       // -?File AZORG01L?
016600130408     d k01azorg01    e ds                  extname(AZORG01L : *key)
016700130408     d                                     prefix(k_)   inz
016800130829
016900130829       // -?File AZCMM01L?
017000130829     d k_azcmm01     e ds                  extname(AZCMM01L : *key)
017100130829     d                                     prefix(k_)   inz
017200130829
017300130829       // -?File AZNTC01L?
017400130829     d k_azntc01     e ds                  extname(AZNTC01L : *key)
017500130829     d                                     prefix(kAZ)  inz
017600130408
017700130408       // -?File TFNTC01L?
017800130408     d k04tfntc01    e ds                  extname(TFNTC01L : *key)
017900130408     d                                     prefix(k_)   inz
018000130408
018100130408       //--------------------------------------------------------------
018200130408       //?M A I N - L I N E                                            ?
018300130408       //--------------------------------------------------------------
018400130408
018500130408     c     *Entry        plist
018600130408     c                   parm                    KPJBA
018700130408     c                   parm                    FNLV45ds
018800130408     c                   parm                    p_TNSD17ds
018900130408
019000130408      /free
019100130408
019200130408       // -?Operazioni iniziali?
019300130408       exsr  sr_RoutInz;
019400130408
019500130408       // -?Elaborazione singola filiale per singolo cliente?
019600130408       exsr  sr_Elab_Cli;
019700130408
019800130408       // -?Operazioni finali?
019900130408       exsr  sr_RoutEnd;
020000130408
020100130408       //--------------------------------------------------------------
020200130408       //?Operazioni iniziali.                                         ?
020300130408       //--------------------------------------------------------------
020400130408       BEGSR  sr_RoutInz;
020500130408
020600130408         *inLR = *on;
020700130408
020800130408         // -?Verifica se sono stati passati 2 parametri?
020900130408         //  ?(caso di richiesta stampa organigramma da tariffe)?
021000130408         //                          ?oppure 3 parametri?
021100130408         //  ?(caso si richiesta stampa organigramma da menù)?
021200130408         If  %parms() < 3;
021300130408           // -?Impostazione campi della TNDS17ds necessari per la stampa?
021400130408           D17pos = I45app;
021500130408           D17pgi = I45npg;
021600130408           D17tps = I45ast;
021700130408         Else;
021800130408           TNSD17ds = p_TNSD17ds;
021900130408         EndIf;
022000130531
022100130531         // -?Impostazione lingua?
022200130531         if  I45lin = 'I';
022300130531           wCodLinA = '1';
022400130531         else;
022500130531           wCodLinA = I45lin;
022600130531         endif;
022700130408
022800130408         // -?Reperimento dati job?
022900130408         exsr  sr_DatiJob;
023000130411
023100130411         // -?Override al file di stampa e apertura dello stesso?
023200130411         Qcmd = c_CmdOvrPrtf_1;
023300130531         select;
023400130531           when  I45lin = '2';
023500130531             Qcmd = Qcmd + c_CmdOvrPrtf_2;
023600130531           when  I45lin = '3';
023700130531             Qcmd = Qcmd + c_CmdOvrPrtf_3;
023800130531           when  I45lin = '4';
023900130531             Qcmd = Qcmd + c_CmdOvrPrtf_4;
024000130531         endsl;
024100130411         if  I45nco > *zero;
024200130531           Qcmd = Qcmd + c_CmdOvrPrtf_9 +
024300130411                  %trim( %editc( I45nco : '1' ) ) + ')';
024400130411         endif;
024500130411         exsr  sr_ExecCmd;
024600130411         open  TNSD30P;
024700130409
024800130409         // -?Reperimento Data & Ora correnti?
024900130409         ds_Time14 = %subst( %char( %dec( %timestamp() ) )
025000130409                             : 1 : 14 );
025100130408
025200130408       ENDSR;
025300130408
025400130408       //--------------------------------------------------------------
025500130408       //?Reperimento Dati del job (Utente/Operativi).                 ?
025600130408       //--------------------------------------------------------------
025700130408       BEGSR  sr_DatiJob;
025800130408
025900130408         in(E) §AzUte;
026000130408         if NOT %error;
026100130408           in(E) §DatiUte;
026200130408         endif;
026300130408         if %error or RSut = *blanks;
026400130408           clear TIBS34ds;
026500130408           tibs34r ( tibs34ds );
026600130408           in §AzUte;
026700130408           in §DatiUte;
026800130408         endif;
026900130408
027000130408       ENDSR;
027100130408
027200130408       //--------------------------------------------------------------
027300130408       //?Elaborazione dati singola filiale per singolo cliente.       ?
027400130408       //--------------------------------------------------------------
027500130408       BEGSR  sr_Elab_Cli;
027600130408
027700130409         // -?REPERIMENTO DATI IN STAMPA?
027800130409
027900130409         clear  k_ORGfil;
028000130409
028100130409         // -?Reperimento capofila £6 della filiale del cliente?
028200130516         clear  wTFA_£6;
028300130409         clear  FNLV55ds;
028400130409         D55tpt = '6';
028500130409         if  I45ksc <> *zero;
028600130409           D55lin = I45ksc / 10000;
028700130409         else;
028800130409           D55lin = I45app;
028900130409         endif;
029000130409         D55drf = wDate;
029100130409         fnlv55r ( FNLV55ds );
029200130516         if  D55err = *blank  and  D55tfa <> D55lin;
029300130516           wTFA_£6 = D55tfa;
029400130516         endif;
029500130516
029600130516         // -?Reperimento terminal di partenza?
029700130516         //  ?(per verificare se filiale di 1° o 2° livello)?
029800130516         clear  FNLV55ds;
029900130516         D55tpt = 'P';
030000130516         select;
030100130516           when  wTFA_£6 <> *zero;
030200130516             D55lin = wTFA_£6;
030300130516           when  I45ksc <> *zero;
030400130516             D55lin = I45ksc / 10000;
030500130516           other;
030600130516             D55lin = I45app;
030700130516         endsl;
030800130516         D55drf = wDate;
030900130516         fnlv55r ( FNLV55ds );
031000130516         if  D55err = *blank;
031100130516           k_ORGfil = D55lin;
031200130517           //$FilLiv1 = (D55tfp = D55lin  and  wTFA_£6 = *zero);
031300130517           $FilLiv1 = (D55tfp = D55lin  or  D55tfp = wTFA_£6);
031400130516         else;
031500130516           leavesr;
031600130516         endif;
031700130409
031800130408         // -?Reperimento dati anagrafici della filiale del cliente?
031900130408         chain  %kds( k01azorg01 )  AZORG;
032000130409
032100130409         select;
032200130409           // -?Esclusione delle filiali inesistenti?
032300130409           when  Not  %found(AZORG01L);
032400130409             leavesr;
032500130409           // -?Esclusione delle filiali annullate?
032600130409           when  ORGfva <> *blank;
032700130409             leavesr;
032800130409           // -?Esclusione dei tipi filiale = "V"/"T"?
032900130409           when  ORGfag = 'V';
033000130409             leavesr;
033100130409         endsl;
033200130408
033300130408         // -?Parzializzazioni su tipo filiale?
033400130408         If  D17tpo <> *blank;
033500130408           select;
033600130408             // - Filiale
033700130408             when  D17tpo = 'F'  and  ORGfag <> 'F';
033800130408               leavesr;
033900130408             // - Agenzia
034000130408             when  D17tpo = 'A'  and  ORGfag <> 'A';
034100130408               leavesr;
034200130408             // - Filiale+Agenzia
034300130408             when  D17tpo = 'E'  and  ORGfag <> 'F'  and  ORGfag <> 'A';
034400130408               leavesr;
034500130408             // - Varie
034600130408             when  D17tpo = 'V'  and  ORGfag <> 'V'  or
034700130408                   D17tpo = 'V'  and  ORGfag =  'V'  and  ORGfl2 = 'S';
034800130408               leavesr;
034900130408             // - Tariffe
035000130408             when  D17tpo = 'T'  and  ORGfag <> 'V'  or
035100130408                   D17tpo = 'T'  and  ORGfag =  'V'  and  ORGfl2 = ' ';
035200130408               leavesr;
035300130408           endsl;
035400130408         EndIf;
035500130408
035600130408         // -?Impostazione DS per descrizioni organigramma fil. cliente?
035700130423         Og130 = ORGdd0;
035800130408
035900130408
036000130408         // -?Reperimento dati anagrafici del cliente?
036100130408         //  ?(se NON ricevuti)?
036200130408         If  I45ksc > *zero;
036300130408
036400130408           // -?Reperimento dati anagrafici del cliente?
036500130408           clear  TIBS69ds;
036600130408           I69sif = knsif;
036700130408           I69kcc = DUTkci;
036800130408           I69kac = I45ksc;
036900130516           I69kin = I45ksc;
037000130409           I69kcp = I45ksc;
037100130408           tibs69r( tibs69ds :
037200130408                    ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
037300130409           //if  O69err <> *on;
037400130409           //  I45rag = ACOrag;
037500130516           //  I45via = INDvia;
037600130516           //  I45cap = INDcae;
037700130516           //  I45pro = INDprv;
037800130516           //  I45nar = ...INDsta;
037900130409           //  I45cmm = CLPage;
038000130409           //endif;
038100130408
038200130408           // -?Reperimento riferimento per "c.a."?
038300130409           clear  k04tfntc01;
038400130409           k_NTCapl = 'C';
038500130409           k_NTCnk1 = %editc( DUTkci : 'X' ) + %editc( I45ksc : 'X' );
038600130409           k_NTCtnt = '05';
038700130409           chain  %kds( k04tfntc01 )  TFNTC;
038800130409           //if  %found(TFNTC01L);
038900130926           //  I45aca = TFNTCrnt;
039000130409           //endif;
039100130408
039200130408         EndIf;
039300130408
039400130408         // -?Reperimento dati dell'agente commerciale?
039500130829         clear  wCMMfil;
039600130829         clear  wNTCname;
039700130829         //clear  wNTCmail;
039800130829
039900130409         If  CLPage <> *zero  or  I45cmm <> *zero;
040000130409
040100130409           if  CLPage <> *zero;
040200130829             k_CMMcod = CLPage;
040300130409           else;
040400130829             k_CMMcod = I45cmm;
040500130409           endif;
040600130409
040700130829           // ·?Filiale di appartenenza?
040800130829           chain  %kds( k_azcmm01 )  AZCMM000;
040900130829           if  %found(AZCMM01L);
041000130829             wCMMfil = CMMfil;
041100130409           endif;
041200130409
041300130829           // ·?Nominativo?
041400130829           kAZNTCcmm = k_CMMcod;
041500130829           kAZNTCtnt = '01';
041600130829           chain  %kds( k_azntc01 )  AZNTC000;
041700130829           if  %found(AZNTC01L);
041800130926             wNTCname = AZNTCrnt;
041900130409           endif;
042000130829
042100130829           //// ·?e-Mail?
042200130829           //kAZNTCtnt = '02';
042300130829           //chain  %kds( k_azntc01 )  AZNTC000;
042400130829           //if  %found(AZNTC01L);
042500130926           //  wNTCmail = AZNTCrnt;
042600130829           //endif;
042700130409
042800130409         EndIf;
042900130409
043000130409         // -?IMPOSTAZIONE CAMPI DI STAMPA E STAMPA DOCUMENTO?
043100130409         exsr  sr_Stampa;
043200130408
043300130408       ENDSR;
043400130408
043500130408       //--------------------------------------------------------------
043600130408       //?Stampa dati per singolo cliente.                             ?
043700130408       //--------------------------------------------------------------
043800130408       BEGSR  sr_Stampa;
043900130409
044000130409         // -?IMPOSTAZIONE CAMPI DI STAMPA:?
044100130409
044200130516         // -?Dati anagrafici del cliente?
044300130409         if  ACOrag <> *blank;
044400130409           O30rsc = ACOrag;
044500130516           O30ind = INDvia;
044600130517           //O30cap = INDcae;
044700130517           //O30loc = INDcit;
044800130517           //O30pro = INDprv;
044900130517           O30calopr = %trimr(INDcae) + '  ' + %trimr(INDcit);
045000130517           %subst(O30calopr : %size(O30calopr) - 2) = ' ' + INDprv;
045100130409         else;
045200130409           O30rsc = I45rag;
045300130516           O30ind = I45via;
045400130517           //O30cap = I45cap;
045500130517           //O30loc = I45cit;
045600130517           //O30pro = I45pro;
045700130517           O30calopr = %trimr(I45cap) + '  ' + %trimr(I45cit);
045800130517           %subst(O30calopr : %size(O30calopr) - 2) = ' ' + I45pro;
045900130409         endif;
046000130516         // ·?Nazione?
046100130516         if  (ACOrag <> *blank  and  INDsta <> *blank)
046200130516                                or   I45nar <> *blank;
046300130516           clear  ds15;
046400130516           if  INDsta <> *blank;
046500130516             ds_TNTBE.TBEke1 = INDsta;
046600130516           else;
046700130516             ds_TNTBE.TBEke1 = I45nar;
046800130516           endif;
046900130517           //*wCodLinA = .....;
047000130516           if  getTabella ( c_Tabel : '15'  : ds_TNTBE.TBEke1 :
047100130517                            *omit : wCodLinA : *omit :
047200130516                            *omit : *omit :
047300130516                            *omit : *omit : *omit : *omit :
047400130516                            *omit : *omit : *omit : *omit :
047500130516                            ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
047600130516                          = *zero      AND
047700130516               ds_TNTBE.TBEatb = *blank;
047800130516             ds15 = ds_TNTBE.TBEuni;
047900130516             O30naz = §15des;
048000130516           endif;
048100130516         endif;
048200130409         select;
048300130926           when  TFNTCrnt <> *blank;
048400130531             select;
048500130531               when  I45lin = '2';
048600130926                 O30aca = 'Attn.: ' + TFNTCrnt;
048700130531               when  I45lin = '3';
048800130926                 O30aca = 'à l''attention de ' + TFNTCrnt;
048900130531               when  I45lin = '4';
049000130926                 O30aca = 'z.H. ' + TFNTCrnt;
049100130531               other;
049200130926                 O30aca = 'c.a. ' + TFNTCrnt;
049300130531             endsl;
049400130409           when  I45aca <> *blank;
049500130531             select;
049600130531               when  I45lin = '2';
049700130531                 O30aca = 'Attn.: ' + I45aca;
049800130531               when  I45lin = '3';
049900130531                 O30aca = 'à l''attention de ' + I45aca;
050000130531               when  I45lin = '4';
050100130531                 O30aca = 'z.H. ' + I45aca;
050200130531               other;
050300130531                 O30aca = 'c.a. ' + I45aca;
050400130531             endsl;
050500130409         endsl;
050600130409
050700130516         // -?Dati anagrafici dellla filiale?
050800130409         O30filD = ORGdes;
050900130409         O30filI = ORGind;
051000130409         O30filA = ORGcpf;
051100130409         //O30filL = ORGloc;
051200130409         //O30filP = ORGpro;
051300130409         O30filLP  = %trimr( ORGloc ) + '   ' + ORGpro;
051400130517
051500130517         // -?Luogo & Data del documento?
051600130517         wDate_Eur = %date(wDate : *iso);
051700130521         //O30dat  = %trim(ORGloc) + ', li ' +
051800130521         O30dat  = %trim(ORGloc) + ', ' +
051900130517                   %editc( %dec(wDate_Eur) : 'Y' );
052000130409
052100130516         // -?Dati Servizio Clienti?
052200130517         O30aclT = ORGtel;
052300130531         select;
052400130531           when  I45lin = '2';
052500130531             O30acl  = 'Monday   to Friday';
052600130531           when  I45lin = '3';
052700130531             O30acl  = 'Du lundi   au vendredi';
052800130531           when  I45lin = '4';
052900130531             O30acl  = 'Von Montag   bis Freitag';
053000130531           other;
053100130531             O30acl  = 'Dal lunedì   al venerdì';
053200130531         endsl;
053300130531         O30acl  = %trim( O30acl ) + '   ' +
053400130423                   %subst(Og130.§OGoraMd :  1 : 2) + '.' +
053500130517                   %subst(Og130.§OGoraMd :  3 : 2) + ' - ' +
053600130423                   %subst(Og130.§OGoraMa :  1 : 2) + '.' +
053700130524                   %subst(Og130.§OgoraMa :  3 : 2);
053800130524         if  Og130.§OGoraPd > *zero  or  Og130.§OGoraPa > *zero;
053900130531           select;
054000130531             when  I45lin = '2';
054100130531               O30acl  = %trim( O30acl ) + '   and';
054200130531             when  I45lin = '3';
054300130531               O30acl  = %trim( O30acl ) + '   et';
054400130531             when  I45lin = '4';
054500130531               O30acl  = %trim( O30acl ) + ' Uhr   und';
054600130531             other;
054700130531               O30acl  = %trim( O30acl ) + '   e';
054800130531           endsl;
054900130531           O30acl  = %trim( O30acl ) + '   ' +
055000130524                     %subst(Og130.§OGoraPd :  1 : 2) + '.' +
055100130524                     %subst(Og130.§OGoraPd :  3 : 2) + ' - ' +
055200130524                     %subst(Og130.§OGoraPa :  1 : 2) + '.' +
055300130524                     %subst(Og130.§OgoraPa :  3 : 2);
055400130531           if  I45lin = '4';
055500130531             O30acl  = %trim( O30acl ) + ' Uhr';
055600130531           endif;
055700130524         endif;
055800130515         //O30aclT = ORGdd1;
055900130515         O30aclF = ORGfax;
056000130409         if  ORGdd2 <> *blank;
056100130409           O30aclE = %xlate( c_Maiusc : c_Minusc :
056200130409                     %trimr( ORGdd2 ) ) + c_atBRT;
056300130409         endif;
056400130516
056500130516
056600130516         // -?Dati del referente commerciale?
056700130829         O30comD = wNTCname;
056800130516
056900130516         // -?Reperimento dati anagrafici della filiale di appartenenza?
057000130829         //  ?del referente commerciale (CMMfil)?
057100130829         IF  wCMMfil > *zero;
057200130516
057300130516           // ·?Reperimento capofila £6 della filiale del cliente?
057400130516           clear  wTFA_£6;
057500130516           clear  FNLV55ds;
057600130516           D55tpt = '6';
057700130829           D55lin = wCMMfil;
057800130516           D55drf = wDate;
057900130516           fnlv55r ( FNLV55ds );
058000130516           if  D55err = *blank  and  D55tfa <> D55lin;
058100130516             wTFA_£6 = D55tfa;
058200130516           endif;
058300130516
058400130516           // ·?Reperimento terminal di partenza?
058500130516           //  ?(per verificare se filiale di 1° o 2° livello)?
058600130516           clear  FNLV55ds;
058700130516           D55tpt = 'P';
058800130516           if  wTFA_£6 <> *zero;
058900130516             D55lin = wTFA_£6;
059000130516           else;
059100130829             D55lin = wCMMfil;
059200130516           endif;
059300130516           D55drf = wDate;
059400130516           fnlv55r ( FNLV55ds );
059500130516           if  D55err = *blank;
059600130516             k_ORGfil = D55lin;
059700130517             $FilLiv1 = (D55tfp = D55lin  or  D55tfp = wTFA_£6);
059800130516           else;
059900130516             leavesr;
060000130516           endif;
060100130516
060200130516           // ·?Aggancio organigramma filiale agente?
060300130516           chain  %kds( k01azorg01 )  AZORG;
060400130516
060500130516         ENDIF;
060600130516
060700130516         // ·?Impostazione dati in stampa?
060800130516         //if  Og148.§OgLPO = '1';
060900130516         if  $FilLiv1;
061000130516           O30comT = ORGdd3;
061100130516           O30comE = %xlate( c_Maiusc : c_Minusc :
061200130516                             %trimr( ORGdd4 ) : 1 ) + c_atBRT;
061300130516         else;
061400130516           O30comT = ORGtel;
061500130516           O30comE = %xlate( c_Maiusc : c_Minusc :
061600130516                             %trimr( ORGdd2 ) : 1 ) + c_atBRT;
061700130516         endif;
061800130517
061900130517
062000130517         // -?Firma?
062100130517         O30rsuo = RSUtOld;
062200130409
062300130409
062400130409         // -?STAMPA DOCUMENTO?
062500130409
062600130409         // -?Stampa dati comuni a fil. di 1° e di 2° livello?
062700130409         write  SD30txt;
062800130408
062900130408       ENDSR;
063000130411
063100130411       //--------------------------------------------------------------
063200130411       //?Esecuzione del comando (già impostato).                      ?
063300130411       //--------------------------------------------------------------
063400130411       BEGSR  sr_ExecCmd;
063500130411
063600130411         clear Qcap0100;
063700130411         Qcabcsdh = *off;
063800130411         Qcapa    = *off;
063900130411         Qcacmdss = *off;
064000130411         Qcaerved = *allX'00';
064100130411
064200130411         clear Qusec;
064300130411         Qusbprv  = %size(Qusec);
064400130411
064500130411         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
064600130411                           %size(Qcap0100) : 'CPOP0100' : *OMIT :
064700130411                           0 : 0 : Qusec);
064800130411
064900130411         //if  Qusei <> *blank;
065000130411         //  ...
065100130411         //endif;
065200130411
065300130411       ENDSR;
065400130408
065500130408       //--------------------------------------------------------------
065600130408       //?Operazioni finali.                                           ?
065700130408       //--------------------------------------------------------------
065800130408       BEGSR  sr_RoutEnd;
065900130408
066000130408         // -?Chiusura funzioni precedentemente aperte?
066100130408         reset  TIBS69ds;
066200130408         //I69tla = 'C';         ?(già così)?
066300130408         tibs69r( tibs69ds :
066400130408                  ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
066500130409
066600130409         reset  FNLV55ds;
066700130409         //D55rla = 'C';         ?(già così)?
066800130409         fnlv55r ( FNLV55ds );
066900130408
067000130408         cloTabella ();
067100130411
067200130411         // -?Chiusura file di stampa?
067300130411         close  TNSD30P;
067400130411         Qcmd = c_CmdDltOvr;
067500130411         exsr  sr_ExecCmd;
067600130408
067700130408         // -?Chiusura *pgm?
067800130408         return;
067900130408
068000130408       ENDSR;
068100130408
068200130408      /end-free
