000100120705       //==============================================================
000200120705       //?TNSF44R - Stampa Fattura: totali suddivisi per voce (batch)  ?
000300120705       //==============================================================
000400120705
000500120705       //--------------------------------------------------------------
000600120705       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700120705       //--------------------------------------------------------------
000800120705
000900120705     /*PRM  dbgview(*source)
001000120706     /*CMD  ovrdbf file(TITAS40C) tofile(GAITRAGRPS/TITAS40C) +
001100120706     /*CMD         ovrscope(*calllvl)
001200120706     /*CMD  ovrdbf file(TITA730C) tofile(GAITRAGRPS/TITA730C) +
001300120706     /*CMD         ovrscope(*calllvl)
001400120706     /*END  dltovr file(TITAS40C TITA730C) lvl(*)
001500120705     /*END
001600120705
001700120705       //--------------------------------------------------------------
001800120705       //?Specifiche di controllo.                                     ?
001900120705       //--------------------------------------------------------------
002000120705
002100120705     h decedit('0,') datedit(*ymd.) option(*nodebugio)
002200120705     h dftactgrp(*no)
002300120705     h bnddir('UBRTVNETA')
002400120705
002500120705       //--------------------------------------------------------------
002600120705       //?Dichiarazione file.                                          ?
002700120705       //--------------------------------------------------------------
002800120705
002900120705       // -?Organigramma filiale/aziendale?
003000120705     fAZORG01L  if   e           k disk
003100120705
003200120705       // -?Tabelle?
003300120705     fTABEL00F  if   e           k disk
003400120705
003500120705       // -?Dati di Bolle SEDE?
003600120705     fTITAS40C  if   e           k disk    extfile(wLibTITAS)
003700120705     f                                     usropn
003800120705     fTITA730C  if   e           k disk    extfile(wLibTITA7)
003900120705     f                                     usropn
004000120705
004100120705       // -?File di stampa?
004200120705     fTNSF44P   o    e             printer
004300120705
004400120705       //--------------------------------------------------------------
004500120705       //?Definizione costanti.                                        ?
004600120705       //--------------------------------------------------------------
004700120705
004800120705       // -?Codice utente x TABEL00F?
004900120705     d c_Kut           c                   const(1)
005000120705
005100120705       //--------------------------------------------------------------
005200120705       //?Definizione schiere.                                         ?
005300120705       //--------------------------------------------------------------
005400120705
005500120705       // -?Tipi Bolla (tab. "TB") con Raggruppamento Bolle = "R"?
005600120706     d sk_TBrbl_R      s                   dim(40)  inz  like(TAStbl)
005700120705
005800120705       // -?Varia, importo e n° spedizioni x Singolo Cliente?
005900120706     d sk_Voce         s                   dim(99)  inz  like(TASsv1)
006000120706     d sk_Impo         s             12  3 dim(99)  inz
006100120706     d sk_Sped         s              9  0 dim(99)  inz
006200120705       // -?Varia, importo e n° spedizioni x Totale Fattura?
006300120706     d sk_T_Voce       s                   dim(99)  inz  like(TASsv1)
006400120706     d sk_T_Impo       s             12  3 dim(99)  inz
006500120706     d sk_T_Sped       s              9  0 dim(99)  inz
006600120705
006700120705       //--------------------------------------------------------------
006800120705       //?Definizione aree dati.                                       ?
006900120705       //--------------------------------------------------------------
007000120705
007100120705       // -?Dati utente?
007200120705     d §AzUte        e ds                  extname(AZUTE00F)
007300120705     d                                     dtaara
007400120705     d §DatiUte      e ds                  extname(dDatiUte)
007500120705     d                                     dtaara
007600120705
007700120705       //--------------------------------------------------------------
007800120705       //?Definizione strutture dati.                                  ?
007900120705       //--------------------------------------------------------------
008000120705
008100120705       // -?Parametri ricevuti?
008200120705     d KPJBA         e ds
008300120705     d TNSF44ds      e ds                  inz
008400120705
008500120705       // -?Tabella "CC" = Codici Tassazione?
008600120705     d dsCC          e ds                  inz
008700120705       // -?Tabella "TB" = Tipi Bolla?
008800120705     d dsTB          e ds                  inz
008900120705
009000120705       // -?Status ds?
009100120705     d Status         sds
009200120705     d   SDSpgm          *proc
009300120705
009400120705       //--------------------------------------------------------------
009500120705       //?Definizione variabili globali.                               ?
009600120705       //--------------------------------------------------------------
009700120705
009800120705       // -?Indici di schiera?
009900120705     d xx              s              3  0 inz
010000120705     d yy              s              3  0 inz
010100120705
010200120705       // -?Nome del sistema?
010300120705     d currSysNeta     s              8a   inz
010400120705
010500120705       // -?Nomi estesi Libreria/File dei files?
010600120705     d wLibTITAS       s             21a   inz
010700120705     d wLibTITA7       s             21a   inz
010800120705
010900120705       // -?Campi di Totale per Cliente?
011000120706     d SaveKSC         s                   inz  like(TASksc)
011100120705     d wTotSpe         s             11  0 inz
011200120705     d wTotRecupero    s             11  0 inz
011300120705     d wTotLivigno     s              9  0 inz
011400120705       // -?Campi di Totale Generale?
011500120705     d wCountKSC       s              5  0 inz
011600120705     d w_T_TotSpe      s             11  0 inz
011700120705     d w_T_TotRec      s             11  0 inz
011800120705     d w_T_TotLivigno  s              9  0 inz
011900120705
012000120705       // -?Campi di comodo?
012100120706     d Wconta          s              1  0 inz
012200120705     d Wvaria          s                   like(TASsv1)   inz
012300120705     d Wimpo           s                   like(TASva1)   inz
012400120705
012500120705       //--------------------------------------------------------------
012600120705       //?Definizione prototipi procedure.                             ?
012700120705       //--------------------------------------------------------------
012800120705
012900120705       // -?Reperimento NETA sistema AS/400 corrente?
013000120705      /copy gaitrasrc/srcProtoPR,UBRTVNETA
013100120705
013200120705       // -?Reperimento dati utente?
013300120705     d TIBS34ds      e ds                  inz
013400120705      /copy gaitrasrc/srcProtoPR,TIBS34R
013500120705
013600120705       // -?Controllo/Decodifica cliente?
013700120705      /copy gaitrasrc/srcProtoPI,TIBS69R
013800120705      /copy gaitrasrc/srcProtoPR,TIBS69R
013900120705
014000120705       // -?Controllo formale / Inversione data?
014100120705     d WLBdat          ds                  inz
014200120705     d   G08dat                1      8  0 inz
014300120705     d   G08inv                9     16  0 inz
014400120705     d   G08err               17     17    inz('3')
014500120705     d   G08tgi               18     22  0 inz
014600120705      /copy gaitrasrc/srcProtoPR,XSRDA8
014700120705
014800120705       //--------------------------------------------------------------
014900120705       //?Definizione key-list.                                        ?
015000120705       //--------------------------------------------------------------
015100120705
015200120705       // -?File TABEL00F?
015300120705     d k03tabel00    e ds                  extname(TABEL00F : *key)
015400120705     d                                     prefix(k_)   inz
015500120705
015600120705       // -?File TITAS40C?
015700120705     d k05titas40    e ds                  extname(TITAS40C : *key)
015800120705     d                                     prefix(k_)   inz
015900120705
016000120705       // -?File TITA730C?
016100120705     d k06tita730    e ds                  extname(TITA730C : *key)
016200120705     d                                     prefix(k_)   inz
016300120705
016400120705       //--------------------------------------------------------------
016500120705       //?M A I N - L I N E                                            ?
016600120705       //--------------------------------------------------------------
016700120705
016800120705     c     *Entry        plist
016900120705     c                   parm                    KPJBA
017000120705
017100120705      /free
017200120705
017300120705       // -?Operazioni iniziali?
017400120705       exsr  sr_RoutInz;
017500120705
017600120705       // -?Elaborazione bolle & Stampa riepilogo per Cliente?
017700120705       exsr  sr_LeggiBolle;
017800120705
017900120705       // -?Stampa Totali per Numero Fattura?
018000120706       if  wCountKSC > 1;
018100120706         exsr  sr_Stampa_TOT;
018200120706       endif;
018300120705
018400120705       // -?Operazioni finali?
018500120705       exsr  sr_RoutEnd;
018600120705
018700120705       //--------------------------------------------------------------
018800120705       //?Operazioni iniziali.                                         ?
018900120705       //--------------------------------------------------------------
019000120705       BEGSR  sr_RoutInz;
019100120705
019200120705         *inLR = *on;
019300120705
019400120705         // -?Impostazione parametri ricevuti?
019500120705         TNSF44ds = kpjbu;
019600120705
019700120705         // -?Verifica del sistema AS/400 corrente?
019800120705         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
019900120705           exsr  sr_RoutEnd;
020000120705         endif;
020100120705
020200120705         // -?Impostazione libreria per file TITAS30C?
020300120705         //  ?(che verrà aperto SOLO se richiesto dal chiamante)?
020400120705         if  %subst ( currSysNeta : 1 : 6 ) = 'SETRAS';
020500120705           wLibTITAS = 'GAITRAGRU/TITAS40C';
020600120705           wLibTITA7 = 'GAITRAGRU/TITA730C';
020700120705         else;
020800120705           wLibTITAS = 'GAITRAGRPS/TITAS40C';
020900120705           wLibTITA7 = 'GAITRAGRPS/TITA730C';
021000120705         endif;
021100120705
021200120705         // -?Apertura files?
021300120705         open  TITAS40C;
021400120705         open  TITA730C;
021500120705
021600120705         // -?Reperimento dati job?
021700120705         exsr  sr_DatiJob;
021800120705
021900120705         // -?Impostazione campi fissi?
022000120705         clear  k03tabel00;
022100120705         k_TBLkut = c_Kut;
022200120705
022300120705         clear  k05titas40;
022400120705         k_TASfiv = SF44fiv;
022500120705         k_TASnft = SF44nft;
022600120705         k_TASdft = SF44dft;
022700120706         k_TASksc = SF44ksc;
022800120705
022900120705         clear  k06tita730;
023000120705
023100120705         // -?Intabellamento Tipi Bolla (tab. "TB") con Raggruppam.Bolle = "R"?
023200120705         clear  xx;
023300120705         clear  sk_TBrbl_R;
023400120705         k_TBLcod = 'TB';
023500120705         setll  %kds( k03tabel00 : 2 )  TABEL;
023600120705         reade  %kds( k03tabel00 : 2 )  TABEL;
023700120705         DoW  Not %eof(TABEL00F);
023800120705           dsTB = TBLuni;
023900120705           if  §TBrbl = 'R';
024000120705             xx += 1;
024100120706             sk_TBrbl_R(xx) = TBLkey;
024200120705           endif;
024300120705           reade  %kds( k03tabel00 : 2 )  TABEL;
024400120705         EndDo;
024500120705
024600120705         // -?Impostazione campi in testata?
024700120705         clear  SF44PT1;
024800120705         clear  SF44PT2;
024900120705         clear  SF44PT3;
025000120705         clear  SF44VAR;
025100120705         clear  SF44TOT;
025200120705
025300120705         P1Trsu = RsUt;
025400120705         P1Tpgm = SDSpgm;
025500120705         P1Tnft = SF44nft;
025600120705         reset  WLBdat;
025700120705         G08dat = SF44dft;
025800120705         xsrda8 ( WLBdat );
025900120705         if  G08err = *off;
026000120705           P1Tdft  = G08dat;
026100120705         endif;
026200120705         P1Tfiv = SF44fiv;
026300120705         if  SF44ksc > *zero;
026400120705           P1Tksc = SF44ksc;
026500120705           P1Trsc = SF44rsc;
026600120705         endif;
026700120705
026800120705       ENDSR;
026900120705
027000120705       //--------------------------------------------------------------
027100120705       //?Reperimento Dati del job (Utente/Operativi).                 ?
027200120705       //--------------------------------------------------------------
027300120705       BEGSR  sr_DatiJob;
027400120705
027500120705         in(E) §AzUte;
027600120705         if NOT %error;
027700120705           in(E) §DatiUte;
027800120705         endif;
027900120705         if %error or RSut = *blanks;
028000120705           clear TIBS34ds;
028100120705           tibs34r ( tibs34ds );
028200120705           in §AzUte;
028300120705           in §DatiUte;
028400120705         endif;
028500120705
028600120705       ENDSR;
028700120705
028800120705       //--------------------------------------------------------------
028900120705       //?Elaborazione spedizioni della fattura   &                    ?
029000120705       //?Stampa riepilogo x Cliente / Varia (a "rottura" di Cliente). ?
029100120705       //--------------------------------------------------------------
029200120705       BEGSR  sr_LeggiBolle;
029300120705
029400120705         // -?Ciclo di lettura spedizioni della fattura?
029500120705         if  SF44ksc <> *zero;
029600120705           setll  %kds( k05titas40 : 4 ) TITAS40C;
029700120705           reade  %kds( k05titas40 : 4 ) TITAS40C;
029800120705         else;
029900120705           setll  %kds( k05titas40 : 3 ) TITAS40C;
030000120705           reade  %kds( k05titas40 : 3 ) TITAS40C;
030100120705         endif;
030200120705
030300120705         if  Not %eof(TITAS40C);
030400120705           SaveKSC = TASksc;
030500120705         endif;
030600120705
030700120705         DoW  Not %eof(TITAS40C);
030800120705
030900120705           // -?Stampa riepilogo per Cliente / Varia (a "rottura")?
031000120705           if  SaveKSC <> TASksc;
031100120705             exsr  sr_Stampa_KSC;
031200120705             SaveKSC = TASksc;
031300120705           endif;
031400120705
031500120705           // -?Incremento totali Varia per il singolo cliente?
031600120705           exsr  sr_Varie_TITAS;
031700120705
031800120705           // -?Lettura spedizione successiva?
031900120705           if  SF44ksc <> *zero;
032000120705             reade  %kds( k05titas40 : 4 ) TITAS40C;
032100120705           else;
032200120705             reade  %kds( k05titas40 : 3 ) TITAS40C;
032300120705           endif;
032400120705
032500120705         EndDo;
032600120705
032700120705         // -?Stampa ultimo riepilogo per cliente (l'ultimo)?
032800120706         exsr  sr_Stampa_KSC;
032900120706         SaveKSC = TASksc;
033000120705
033100120705       ENDSR;
033200120705
033300120705       //--------------------------------------------------------------
033400120705       //?Incremento totali per Varia del singolo cliente.             ?
033500120705       //--------------------------------------------------------------
033600120705       BEGSR  sr_Varie_TITAS;
033700120705
033800120705         // -?Il porto sempre nella prima posizione?
033900120705         sk_Voce(1) = '.';
034000120705         if  TASpor > *zero;
034100120705           sk_Impo(1) += TASpor;
034200120705           sk_sped(1) += 1;
034300120705         endif;
034400120705
034500120705         // -?Le prime 3 varie?
034600120705         if  TASva1 > *zero;
034700120705           Wvaria = TASsv1;
034800120705           Wimpo  = TASva1;
034900120705           exsr  sr_TotVaria;
035000120705         endif;
035100120705         if  TASva2 > *zero;
035200120705           Wvaria = TASsv2;
035300120705           Wimpo  = TASva2;
035400120705           exsr  sr_TotVaria;
035500120705         endif;
035600120705         if  TASva3 > *zero;
035700120705           Wvaria = TASsv3;
035800120705           Wimpo  = TASva3;
035900120705           exsr  sr_TotVaria;
036000120705           // -?Eventuale reperimento di altre Varie da TITA730C?
036100120705           exsr  sr_Varie_TITA7;
036200120705         endif;
036300120705
036400120705         // -?Incremento num. spedizioni?
036500120705         wTotSpe += 1;
036600120705
036700120705         // -?Totalizzazione num. spedizioni di recupero?
036800120705         if  %lookup( TAStbl : sk_TBrbl_R ) > *zero;
036900120705           wTotRecupero += 1;
037000120705         endif;
037100120705
037200120705       ENDSR;
037300120705
037400120705       //--------------------------------------------------------------
037500120705       //?Reperimento eventuali altre Varie da TITA7.                  ?
037600120705       //--------------------------------------------------------------
037700120705       BEGSR  sr_Varie_TITA7;
037800120705
037900120705         k_TA7aas = TASaas;
038000120705         k_TA7lnp = TASlnp;
038100120705         k_TA7nrs = TASnrs;
038200120705         k_TA7nsp = TASnsp;
038300120705         k_TA7tbl = TAStbl;
038400120706         setll  %kds( k06tita730 : 5 )  TITA730C;
038500120706         reade  %kds( k06tita730 : 5 )  TITA730C;
038600120705
038700120705         DoW  Not %eof(TITA730C);
038800120705
038900120705           if  TA7van > *zero;
039000120705             Wvaria = TA7svn;
039100120705             Wimpo  = TA7van;
039200120705             exsr  sr_TotVaria;
039300120705           endif;
039400120705
039500120706           reade  %kds( k06tita730 : 5 )  TITA730C;
039600120705
039700120705         EndDo;
039800120705
039900120705       ENDSR;
040000120705
040100120705       //--------------------------------------------------------------
040200120705       //?Incremento totali per voce in schiera.                       ?
040300120705       //--------------------------------------------------------------
040400120705       BEGSR  sr_TotVaria;
040500120705
040600120705         //xx = %lookup( Wvaria : sk_Voce : 2 : %elem(sk_Voce)-1 );
040700120705         xx = %lookup( Wvaria : sk_Voce : 2 );
040800120705         if  xx = *zero;
040900120705           xx = %lookup( *blank : sk_Voce : 2 );
041000120705           if  xx > *zero;
041100120705             sk_Voce(xx) = Wvaria;
041200120705           endif;
041300120705         endif;
041400120705
041500120705         sk_Impo(xx) += Wimpo;
041600120705         sk_Sped(xx) += 1;
041700120705
041800120705         // -?SE si tratta di isola per Livigno, isola Sondrio:?
041900120705         //  ?si memorizza a parte (per Lorena)?
042000120705         if  sk_Voce(xx) = 'J'  and  TAScts = '23'  and  TASfin = 'I';
042100120705           wTotLivigno += 1;
042200120705         endif;
042300120705
042400120705       ENDSR;
042500120705
042600120705       //--------------------------------------------------------------
042700120705       //?Stampa totali pr Cliente.                                    ?
042800120705       //--------------------------------------------------------------
042900120705       BEGSR  sr_Stampa_KSC;
043000120705
043100120705         // -?Decodifica cliente (se NON ricevuto)?
043200120705         If  SF44ksc = *zero;
043300120706           P1Tksc = Saveksc;
043400120705           clear  TIBS69ds;
043500120705           I69sif = knsif;
043600120705           I69kcc = DUTkci;
043700120705           I69kac = TASksc;
043800120705           tibs69r( tibs69ds :
043900120705                    ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
044000120706           if  O69err <> *on;
044100120705             P1Trsc = ACOrag;
044200120705           endif;
044300120705         EndIf;
044400120705
044500120705         // -?Stampa testate?
044600120705         write  SF44PT1;
044700120705         write  SF44PT2;
044800120705         write  SF44PT3;
044900120705
045000120705         // -?Stampa delle varie di 2 in 2?
045100120705         xx = 1;
045200120705
045300120705         // -?SE non ci fosse la voce trasporto (che è sempre alla?
045400120705         //  ?prima posizione): si inizia a stampare dalla 2ª Varia?
045500120705         if  sk_Impo(1) = *zero;
045600120705           xx = 2;
045700120705         endif;
045800120705
045900120705         DoW  sk_Voce(xx) <> *blank;
046000120705
046100120706           Wconta = 1;
046200120705           clear  SF44VAR;
046300120705
046400120706           DoW  Wconta <= 2  and  sk_Voce(xx) <> *blank;
046500120705
046600120706             Wvaria = sk_Voce(xx);
046700120706             if  Wconta = 1;
046800120705               // -?Nella prima posizione: Trasporto?
046900120705               if  xx = 1;
047000120705                 P1Dsv1 = 'TRASPORTO      ';
047100120705               else;
047200120705                 exsr  sr_TabCC;
047300120705                 P1Csv1 = sk_Voce(xx) + ' -';
047400120705                 P1Dsv1 = §CCdes;
047500120705               endif;
047600120705               P1Cva1  = sk_Impo(xx);
047700120705               P1Cspe1 = sk_Sped(xx);
047800120705             else;
047900120705               exsr  sr_TabCC;
048000120705               P1Csv2  = sk_Voce(xx) + ' -';
048100120705               P1Dsv2  = §CCdes;
048200120705               P1Cva2  = sk_Impo(xx);
048300120705               P1Cspe2 = sk_Sped(xx);
048400120705             endif;
048500120705
048600120705             xx += 1;
048700120706             Wconta += 1;
048800120705
048900120705           EndDo;
049000120705
049100120705           write  SF44VAR;
049200120705
049300120705         EndDo;
049400120705
049500120705         // -?Totale Imponibile?
049600120705         P1Cimv = %xfoot( sk_Impo );
049700120705
049800120705         // -?Totale Spedizioni?
049900120705         P1Csped = wTotSpe;
050000120705
050100120705         // -?Totale Spedizioni di Recupero?
050200120705         P1Crec = wTotRecupero;
050300120705
050400120705         // -?Totale Spedizioni Livigno?
050500120705         P1Cliv = wTotLivigno;
050600120705         // -?Da stampare solo se ce ne sono?
050700120705         *in01 = (P1Cliv > *zero);
050800120705
050900120705         // -?Stampa Totali per Cliente?
051000120705         write  SF44TOT;
051100120705
051200120706         // -?Incremento dei totali per Fattura?
051300120706         //  ?(SE non ricevuto il codice cliente)?
051400120706         IF  SF44ksc = *zero;
051500120706
051600120706           wCountKSC += 1;
051700120706           xx = 1;
051800120706           DoW  sk_Voce(xx) <> *blank;
051900120706
052000120706             yy = %lookup( sk_Voce(xx) : sk_T_Voce );
052100120706             if  yy = *zero;
052200120706               yy = %lookup( *blank : sk_T_Voce );
052300120706               sk_T_Voce(yy) = sk_Voce(xx);
052400120706             endif;
052500120706
052600120706             // -?Totale Imponibile x Varia?
052700120706             sk_T_Impo(yy) += sk_Impo(xx);
052800120706             // -?Totale Spedizioni x Varia?
052900120706             sk_T_Sped(yy) += sk_Sped(xx);
053000120706
053100120706             xx += 1;
053200120706
053300120706           EndDo;
053400120706
053500120706           // -?Totale Spedizioni (anche senza Varia)?
053600120706           w_T_TotSpe += wTotSpe;
053700120706           // -?Totale Spedizioni di Recupero?
053800120706           w_T_TotRec += wTotRecupero;
053900120706           // -?Totale Spedizioni Livigno?
054000120706           w_T_TotLivigno += wTotLivigno;
054100120706
054200120706           // -?Pulizia delle schiere e dei campi di totale x Cliente?
054300120706           clear  xx;
054400120706           clear  sk_Voce;
054500120706           clear  sk_Impo;
054600120706           clear  sk_Sped;
054700120706           clear  wTotSpe;
054800120706           clear  wTotRecupero;
054900120706           clear  wTotLivigno;
055000120706
055100120706         ELSE;
055200120706
055300120706           // -?Stampa "*** FINE STAMPA ***"?
055400120706           write  SF44END;
055500120706
055600120706         ENDIF;
055700120705
055800120705       ENDSR;
055900120705
056000120705       //--------------------------------------------------------------
056100120705       //?Stampa totali per Numero Fattura.                            ?
056200120705       //--------------------------------------------------------------
056300120705       BEGSR  sr_Stampa_TOT;
056400120705
056500120705         write  SF44PT1;
056600120705         write  SF44PT3;
056700120705
056800120705         xx = 1;
056900120705
057000120705         // -?SE non ci fosse la voce trasporto (che è sempre alla?
057100120705         //  ?prima posizione): si inizia a stampare dalla 2ª Varia?
057200120705         if  sk_T_Impo(1) = *zero;
057300120705           xx = 2;
057400120705         endif;
057500120706
057600120706         // -?Stampa delle varie di 2 in 2?
057700120705         DoW  sk_T_Voce(xx) <> *blank;
057800120705
057900120706           Wconta = 1;
058000120705           clear  SF44VAR;
058100120705
058200120706           DoW  Wconta <= 2  and  sk_T_Voce(xx) <> *blank;
058300120705
058400120706             Wvaria = sk_T_Voce(xx);
058500120706             if  Wconta = 1;
058600120705               // -?Nella prima posizione: Trasporto?
058700120705               if  xx = 1;
058800120705                 P1Dsv1 = 'TRASPORTO      ';
058900120705               else;
059000120705                 exsr  sr_TabCC;
059100120705                 P1Csv1 = sk_T_Voce(xx) + ' -';
059200120705                 P1Dsv1 = §CCdes;
059300120705               endif;
059400120705               P1Cva1  = sk_T_Impo(xx);
059500120705               P1Cspe1 = sk_T_Sped(xx);
059600120705             else;
059700120705               exsr  sr_TabCC;
059800120705               P1Csv2  = sk_T_Voce(xx) + ' -';
059900120705               P1Dsv2  = §CCdes;
060000120705               P1Cva2  = sk_T_Impo(xx);
060100120705               P1Cspe2 = sk_T_Sped(xx);
060200120705             endif;
060300120705
060400120705             xx += 1;
060500120706             Wconta += 1;
060600120705
060700120705           EndDo;
060800120705
060900120705           write  SF44VAR;
061000120705
061100120705         EndDo;
061200120705
061300120705         // -?Totale Imponibile?
061400120705         P1Cimv = %xfoot( sk_T_Impo );
061500120705
061600120705         // -?Totale Spedizioni?
061700120705         P1Csped = w_T_TotSpe;
061800120705
061900120705         // -?Totale Spedizioni di Recupero?
062000120705         P1Crec = w_T_TotRec;
062100120705
062200120705         // -?Totale Spedizioni Livigno?
062300120705         P1Cliv = w_T_TotLivigno;
062400120705         // -?Da stampare solo se ce ne sono?
062500120705         *in01 = (P1Cliv > *zero);
062600120705
062700120705         // -?Stampa Totali per Fattura?
062800120705         write  SF44TOT;
062900120706
063000120706         // -?Stampa "*** FINE STAMPA ***"?
063100120706         write  SF44END;
063200120705
063300120705       ENDSR;
063400120705
063500120705       //--------------------------------------------------------------
063600120705       //?Decodifica Sigla Varia.                                      ?
063700120705       //--------------------------------------------------------------
063800120705       BEGSR  sr_TabCC;
063900120705
064000120705         clear  dsCC;
064100120705         k_TBLcod = 'CC';
064200120706         k_TBLkey = 'VARIE  ' + Wvaria;
064300120705
064400120705         chain  %kds( k03tabel00 )  TABEL;
064500120705
064600120705         if  %found(TABEL00F);
064700120705           dsCC = TBLuni;
064800120705         else;
064900120705           §CCdes = *all'?';
065000120705         endif;
065100120705
065200120705       ENDSR;
065300120705
065400120705       //--------------------------------------------------------------
065500120705       //?Decodifica Sigla Varia.                                      ?
065600120705       //--------------------------------------------------------------
065700120705       BEGSR  sr_RoutEnd;
065800120705
065900120705         reset  TIBS69ds;
066000120705         tibs69r( tibs69ds :
066100120705                  ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
066200120705
066300120705         return;
066400120705
066500120705       ENDSR;
