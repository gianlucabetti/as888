000100071212      *PARMS OPTION(*NOXREF) DFTACTGRP(*NO) ACTGRP(QILE)
000200120302     /*PRM option(*noxref) dftactgrp(*no) actgrp(QILE)
000300120302     /*END
000400071212      //---------------------------------------------------------------
000500080508      //?FIMS53R - Controllo e segnalazione anomalie spunte da PDA
000600080519      //?          Controllo e segnalazione anomalie dati ORM da PDA
000700080520      //?          Controllo e segnalazione anomalie dati CONSEGNE da PDA
000800090224      //?          Controllo e segnalazione anomalie spunte da Positrova
000900071212      //---------------------------------------------------------------
001000071213     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001100071218     h dftactgrp(*no)
001200071212
001300071212      //---------------------------------------------------------------
001400071212      //?Dichiarazione file.
001500071212      //---------------------------------------------------------------
001600071219
001700080508     fFIPSP01L  If   e           k disk    infds(fipspinf)
001800080519     f                                     rename(fipsp000:fipsp01)
001900080519     fFIPSP00F  uf   e             disk
002000080519
002100080519     ffiprc00r  uf   e             disk
002200080520     ffiprd00r  uf   e             disk
002300080519
002400080508     fAZORG01L  if   e           k disk
002500071219
002600080508     fFIPSL00F  IF A e             disk
002700080519     ffiplg00f  o    e             disk
002800090224
002900090224     fFITR401L  If   e           k disk
003000080519
003100080508     fPrtEMAIL  O    f  132        printer  oflind(*inOF)  usropn
003200071212
003300071212      //---------------------------------------------------------------
003400071212      //?Definizione costanti.
003500071212      //---------------------------------------------------------------
003600071219
003700080508     d Digits          c                   const('0123456789')
003800080519
003900071219      // - Dati di default
004000071227     d C_Sede          c                   const('046')
004100071219
004200071219      // - Comando di override al PrtF
004300071221     d C_CmdOvrPrtF    c                   const('OVRPRTF +
004400071221     d                                           file(PRTEMAIL) +
004500071221     d                                           pagesize(66 132) +
004600071221     d                                           lpi(6) cpi(10) +
004700071221     d                                           ovrscope(*actgrpdfn) +
004800071221     d                                           ')
004900071221     d C_CmdDltOvr     c                   const('DLTOVR +
005000071221     d                                            file(PRTEMAIL) +
005100071221     d                                            lvl(*actgrpdfn)')
005200071212
005300071212      //---------------------------------------------------------------
005400080519      //?Definizione strutture dati.
005500071212      //---------------------------------------------------------------
005600071219
005700080508     D fipspinf        ds
005800080508     D  fipspnrri            397    400i 0
005900080519
006000071212      // - Status
006100071212     d Psds           sds
006200071213     d   SDSpgm          *proc
006300071213     d   JobUser             254    263                                         User name
006400071221
006500071221      // Parametri
006600071221     d kpjba         e ds
006700071221
006800080519      // - Tabella "MRA" = Bart-Mailing - BartMag
006900071221     d dMRAdan       e ds                  inz
007000080519      // - Tabella "MRA" = Bart-Mailing - BartAut RITIRI
007100080519     d dMRAdanO      e ds                  extname(dmradan) inz
007200080519     d                                     prefix(o_)
007300080519      // - Tabella "MRA" = Bart-Mailing - BartAut CONSEGNE
007400080519     d dMRAdanC      e ds                  extname(dmradan) inz
007500080519     d                                     prefix(c_)
007600090224      // - Tabella "MRA" = Bart-Mailing - Positrova
007700090224     d dMRAdanP      e ds                  extname(dmradan) inz
007800090224     d                                     prefix(p_)
007900071221
008000071221      // - Reperimento dati utente
008100071221     d TIBS34ds      e ds
008200071221
008300071221      // - Gestione tabelle: controllo e ricerca
008400071221     d TIBS02ds      e ds                  inz
008500071221     d   T02tla      e                     inz('L')
008600071221     d   T02mod      e                     inz('C')
008700071221     d   T02cod      e                     inz('MRA')
008800071219
008900071219      // - Parametri x Ridefinizione dati utente estesi per mailing PDF
009000071219     d TRTCM1ds      e ds                  inz
009100071219      //   ·§CM1mitt = Indirizzo e-mail del mittente
009200120302     d   §CM1mitt    e                     inz('ced@brt.it')
009300071219      //   ·§CM1dst  = Indirizzo e-mail del destinatario
009400120302     d   §CM1dst     e                     inz('rita.mazza@brt.it')
009500071219      //   ·§CM1tips = Tipo lettera via e-mail:
009600071219      //    "LET" = testo allegato in corpo con logo
009700071219      //            (richiede righe libere iniziali per il logo)
009800071219      //    "COR" = testo integrato senza logo
009900071219      //            (non consente né UNDERLINE né HIGHLIGHT)
010000071219     d   §CM1tips    e                     inz('COR')
010100071219      //   ·§CM1po   = Filiale
010200071219     d   §CM1po      e                     inz('046')
010300071219      //   ·§CM1var  = Oggetto e-mail
010400071219     d   §CM1var     e                     inz('*OBJM*+
010500080508     d                                     Anomalie spunte da pistole +
010600080508     d                                     radio')
010700071219      //   ·§CM1sts  = Stato
010800071219     d   §CM1sts     e                     inz(*off)
010900080508      //   ·§CM1idp  = Id processo
011000080523     d   §CM1idp     e                     inz('2')
011100080508      * -
011200080508     d FIpslds         ds                  inz
011300080508     d  pslnpg                 1      1
011400080508     d  pslnfv                 2     10
011500080508     d  pslbarco              11     28
011600080508     d  pslcan                29     29
011700080508     d  psldup                30     30
011800080508     d  pslman                31     31
011900080508     d  pslusr                32     41
012000080508     d  psldtorar             42     55
012100080508     d  pslmilles             56     58
012200080508     d  pslindip              59     70
012300080508     d  pslfiller             71    100
012400140916     d anomcds         ds                  inz
012500140916     d  anmtiporcd                    3
012600140916     d  anmprfc                      10
012700140916     d  anmiddisp                    35
012800140916     d  anmdatora                    14
012900140916     d sk              s              2  0 inz
013000140916     d zz              s              2  0 inz
013100140916     d skanom          s                   dim(20) inz
013200140916     d                                     like(anomcds)
013300071218
013400071212      //---------------------------------------------------------------
013500071212      //?Definizione variabili globali.
013600071212      //---------------------------------------------------------------
013700071219
013800071212      // - Flags booleani
013900071213     d W_Esito         s              1n   inz(*off)
014000071219     d $Invio          s              1n   inz(*off)
014100080512     d W_anom          s              1n   inz(*off)
014200080519     d w_anomo         s              1n   inz(*off)
014300080519     d w_anomc         s              1n   inz(*off)
014400090225     d w_anomp         s              1n   inz(*off)
014500071219
014600071219      // - Campi per QCMDEXC
014700071219     d Qcmd            s            512    inz
014800071219
014900080508      // - Campi di comodo
015000080508     d N_psppo         s              3  0 inz
015100080519     d s_psppo         s                   like(psppo)
015200080519     d prcfil          s              3
015300080519     d num_prcfil      s                   like(orgfil)
015400080520     d prdfil          s              3
015500080520     d num_prdfil      s                   like(orgfil)
015600090225     d N_tr4fil        s              3  0 inz
015700090225     d s_tr4fil        s                   like(tr4fil)
015800071219
015900071218      // - Campi di stampa
016000071219     d O_testo         s            132    inz
016100071212
016200071212      //---------------------------------------------------------------
016300071213      //?Definizione procedure esterne.
016400071212      //---------------------------------------------------------------
016500071219
016600071219      // - Esecuzione comando di sistema
016700071219     d qCmdExc         pr                  extpgm('QCMDEXC')
016800071219     d  Qcmd                        512    const  options(*varsize)
016900071219     d  Qlen                         15  5 const
017000071221
017100071221      // - Gestione tabelle
017200071221     d tibs02r         pr                  extpgm('TIBS02R')
017300071221     d  kpjba                              likeds(KPJBA)    const
017400071221     d  tibs02ds                           likeds(TIBS02ds)
017500071212
017600071212      //---------------------------------------------------------------
017700071212      //?Definizione key-list.
017800071212      //---------------------------------------------------------------
017900071212
018000071212      //---------------------------------------------------------------
018100071212      //?Main.
018200071212      //---------------------------------------------------------------
018300071212     c     *Entry        plist
018400071221     c                   parm                    KPJBA
018500071212
018600071212      /free
018700071212
018800071212       // Operazioni iniziali
018900080519       exsr RoutInz;
019000071212
019100080508       //
019200080508       exsr Elab_FIPSP;
019300080519       exsr Elab_FIPRC;
019400080520       exsr Elab_FIPRD;
019500090224       exsr Elab_FITR4;
019600071212
019700071212       //--------------------------------------------------------------
019800071212       //?Operazioni iniziali.
019900071212       //--------------------------------------------------------------
020000071212       BEGSR RoutInz;
020100071219
020200071219         *inLR = *on;
020300071221
020400071221       // Reperimento tab. "MRA"
020500071221         clear dMRAdan;
020600071221         clear kpjbu;
020700071221         reset tibs02ds;
020800071221         T02ke1 = SDSpgm;
020900071221         tibs02r(kpjba:tibs02ds);
021000071221         if T02err = *blank;
021100071221           dMRAdan = T02uni;
021200071221         endif;
021300080519
021400080519       // Reperimento tab. "MRA" --> RITIRI
021500080519         clear dMRAdanO;
021600080519         clear kpjbu;
021700080519         reset tibs02ds;
021800080519         T02ke1 = SDSpgm;
021900080519         T02ke2 = 'RITIRI';
022000080519         tibs02r(kpjba:tibs02ds);
022100080519         if T02err = *blank;
022200080519           dMRAdanO = T02uni;
022300080519         endif;
022400080519
022500080519       // Reperimento tab. "MRA" --> CONSEGNE
022600080519         clear dMRAdanC;
022700080519         clear kpjbu;
022800080519         reset tibs02ds;
022900080519         T02ke1 = SDSpgm;
023000080519         T02ke2 = 'CONSEGNE';
023100080519         tibs02r(kpjba:tibs02ds);
023200080519         if T02err = *blank;
023300080519           dMRAdanC = T02uni;
023400080519         endif;
023500090224
023600090224       // Reperimento tab. "MRA" --> Positrova
023700090224         clear dMRAdanP;
023800090224         clear kpjbu;
023900090224         reset tibs02ds;
024000090224         T02ke1 = SDSpgm;
024100090224         T02ke2 = 'POSITROVA';
024200090224         tibs02r(kpjba:tibs02ds);
024300090224         if T02err = *blank;
024400090224           dMRAdanP = T02uni;
024500090224         endif;
024600071212
024700071212       ENDSR;
024800071212
024900071212       //--------------------------------------------------------------
025000080519       //?Elaborazione file ritorno dati da BartMag
025100071212       //--------------------------------------------------------------
025200080508       BEGSR Elab_FIPSP;
025300071212
025400080508         s_psppo='   ';
025500080512         w_anom=*off;
025600080519
025700080508       //?Ciclo di lettura tab. FIPSP?
025800071212
025900080519         setll *loval fipsp01l;
026000080519         read         fipsp01l;
026100071212
026200080508         DOW not %eof(FIPSP01L);
026300080519
026400080519          // se po a blank imposto il valore massimo nel campo di confronto
026500080519         if psppo = *blanks;
026600080519         s_psppo = *hival;
026700080519         endif;
026800071212
026900080508         // Operazioni a rottura di p.o.
027000080508         if S_psppo<>psppo;
027100080508         w_esito=*off;
027200080508            // se codice p.o. non è tutto numerico è sicuramente un'anomalia
027300080508            if %check(digits:psppo)>0;
027400080508            w_esito=*on;
027500080508            else;
027600080508            // se codice tutto numerico verifico esistenza su azorg
027700080519            n_psppo = %dec(psppo:3:0);
027800080508            chain (n_psppo) azorg01l;
027900080508            if not %found or (orgfag<>'A' and orgfag<>'F' or (orgfva<>' '));
028000080508            w_esito=*on;
028100080508            else;
028200080508            // se trovato p.o. mi posiziono sul prossimo
028300080508            setgt (psppo) fipsp01l;
028400080519         read         fipsp01l;
028500080508            iter;
028600080508            endif;
028700080508            endif;
028800080508         S_psppo=psppo;
028900080508         endif;
029000080508
029100080508         // Elaborazione singolo record
029200080508         if w_esito=*on;
029300080519         chain(e) fipspnrri fipsp00f;
029400080508         if not(%error);
029500080512         w_anom=*on;
029600080508         pslprfc=pspprfc;
029700080508         psliddisp=pspiddisp;
029800080508         fipslds=pspdati;
029900080508         psldatora=pspdatora;
030000080508         pslerr='E';
030100080508         pslfil=psppo;
030200080508         write FIPSL000;
030300080519         delete fipsp000;
030400080508         endif;
030500080508         endif;
030600071212
030700080519           Read    FIPSP01l;
030800071212
030900071212         ENDDO;
031000071212
031100080512         // Se riscontrata almeno un'anomalia preparo e_mail
031200080519         if w_anom;
031300080512         exsr sr_Prep_eMail;
031400080519         // Operazioni finali di chiusura file
031500080519         exsr RoutEnd;
031600080512         endif;
031700080519
031800080519         w_anom=*off;
031900080519
032000071212       ENDSR;
032100080519
032200080519       //--------------------------------------------------------------
032300080519       //?Elaborazione file ritorno dati da BartAut RITIRI
032400080519       //--------------------------------------------------------------
032500080519       begsr Elab_FIPRC;
032600080519
032700080519         clear prcfil;
032800080519         w_anomo = *off;
032900080519
033000080519       //?Ciclo di lettura file FIPRC?
033100080519
033200080519         setll *start fiprc00r;
033300080519         read         fiprc00r;
033400080519
033500080519         dow not %eof(fiprc00r);
033600080519
033700080519          // Imposto la filiale dall'utente
033800080519          prcfil = %subst(prcprfc:3:3);
033900080519
034000080519            // se la filiale non è tutta numerica è sicuramente un'anomalia
034100080519            if %check(digits:prcfil) > 0;
034200080519             // scrivo il record su fiplg00f
034300080519             clear fiplg000;
034400080519             plgerr = 'E';
034500080519             plgmsg = 'FILIALE ERRATA';
034600080519             plgtiporcd = prctiporcd;
034700080519             plgprfc = prcprfc;
034800080519             plgiddisp = prciddisp;
034900080519             plgdatora = prcdatora;
035000080519             plgdati = prcdati;
035100080519             write fiplg000;
035200080519             // cancello il record letto su fiprc
035300080519             delete fiprc000;
035400080519             // imposto flag anomalia
035500080519             w_anomo = *on;
035600080519
035700080519            else;
035800080519
035900080519            // se la filiale è tutta numerica verifico esistenza su azorg
036000080519            num_prcfil = %dec(prcfil:3:0);
036100080519            chain (num_prcfil) azorg01l;
036200080519            // filiale non valida
036300080519             if not %found or (orgfag<>'A' and orgfag<>'F' or (orgfva<>' '));
036400080519              // scrivo il record su fiplg00f
036500080519              clear fiplg000;
036600080519              plgerr = 'E';
036700080519              plgmsg = 'FILIALE ERRATA';
036800080519              plgtiporcd = prctiporcd;
036900080519              plgprfc = prcprfc;
037000080519              plgiddisp = prciddisp;
037100080519              plgdatora = prcdatora;
037200080519              plgdati = prcdati;
037300080519              write fiplg000;
037400080519              // cancello il record letto su fiprc
037500080519              delete fiprc000;
037600080519              // imposto flag anomalia
037700080519              w_anomo = *on;
037800080519             endif;
037900080519
038000080519            endif;
038100080519
038200080519         read fiprc00r;
038300080519
038400080519         enddo;
038500080519
038600080519         // Se riscontrata almeno un'anomalia preparo e_mail
038700080519         if w_anomo;
038800080519         exsr sr_Prep_eMail;
038900080519         // Operazioni finali di chiusura file
039000080519         exsr RoutEnd;
039100080519         endif;
039200080519
039300080519         w_anomo = *off;
039400080519
039500080519       endsr;
039600080520
039700080520       //--------------------------------------------------------------
039800080520       //?Elaborazione file ritorno dati da BartAut CONSEGNE
039900080520       //--------------------------------------------------------------
040000080520       begsr Elab_FIPRd;
040100080520
040200080520         clear prdfil;
040300140916         clear anomcds;
040400140916         clear skanom;
040500140916         clear sk;
040600080520         w_anomc = *off;
040700080520
040800080520       //?Ciclo di lettura file FIPRD?
040900080520
041000080520         setll *start fiprd00r;
041100080520         read         fiprd00r;
041200080520
041300080520         dow not %eof(fiprd00r);
041400080520
041500080520          // Imposto la filiale dall'utente
041600080520          prdfil = %subst(prdprfc:3:3);
041700080520
041800080520            // se la filiale non è tutta numerica è sicuramente un'anomalia
041900080520            if %check(digits:prdfil) > 0;
042000080520             // scrivo il record su fiplg00f
042100080520             clear fiplg000;
042200080520             plgerr = 'E';
042300080520             plgmsg = 'FILIALE ERRATA';
042400080520             plgtiporcd = prdtiporcd;
042500080520             plgprfc = prdprfc;
042600080520             plgiddisp = prdiddisp;
042700080520             plgdatora = prddatora;
042800080520             plgdati = prddati;
042900080520             write fiplg000;
043000140916             // metto in skiera i riferimenti dell'errore ne memorizza fino a 20
043100140916            if sk < 20;
043200140916             anmtiporcd = prdtiporcd;
043300140916             anmprfc = prdprfc;
043400140916             anmiddisp = prdiddisp;
043500140916             anmdatora = prddatora;
043600140916             sk = sk + 1 ;
043700140916             skanom(sk) = anomcds;
043800140916            endif;
043900080520             // cancello il record letto su fiprd
044000080520             delete fiprd000;
044100080520             // imposto flag anomalia
044200080520             w_anomc = *on;
044300080520
044400080520            else;
044500080520
044600080520            // se la filiale è tutta numerica verifico esistenza su azorg
044700080520            num_prdfil = %dec(prdfil:3:0);
044800080520            chain (num_prdfil) azorg01l;
044900080520            // filiale non valida
045000080520             if not %found or (orgfag<>'A' and orgfag<>'F' or (orgfva<>' '));
045100080520              // scrivo il record su fiplg00f
045200080520              clear fiplg000;
045300080520              plgerr = 'E';
045400080520              plgmsg = 'FILIALE ERRATA';
045500080520              plgtiporcd = prdtiporcd;
045600080520              plgprfc = prdprfc;
045700080520              plgiddisp = prdiddisp;
045800080520              plgdatora = prddatora;
045900080520              plgdati = prddati;
046000140916            if sk < 20;
046100140916             anmtiporcd = prdtiporcd;
046200140916             anmprfc = prdprfc;
046300140916             anmiddisp = prdiddisp;
046400140916             anmdatora = prddatora;
046500140916             sk = sk + 1 ;
046600140916             skanom(sk) = anomcds;
046700140916            endif;
046800080520              write fiplg000;
046900080520              // cancello il record letto su fiprd
047000080520              delete fiprd000;
047100080520              // imposto flag anomalia
047200080520              w_anomc = *on;
047300080520             endif;
047400080520
047500080520            endif;
047600080520
047700080520         read fiprd00r;
047800080520
047900080520         enddo;
048000080520
048100080520         // Se riscontrata almeno un'anomalia preparo e_mail
048200080520         if w_anomc;
048300080520         exsr sr_Prep_eMail;
048400080520         // Operazioni finali di chiusura file
048500080520         exsr RoutEnd;
048600080520         endif;
048700080520
048800080520         w_anomc = *off;
048900080520
049000080520       endsr;
049100090224
049200090224       //--------------------------------------------------------------
049300090224       //?Elaborazione file ritorno dati da Positrova
049400090224       //--------------------------------------------------------------
049500090224       BEGSR Elab_FITR4;
049600090224
049700090225         s_tr4fil='   ';
049800090224         w_anomp=*off;
049900090224
050000090224       //?Ciclo di lettura tab. FITR4?
050100090224
050200090224         setll *loval fitr401l;
050300090224         read         fitr401l;
050400090224
050500090224         DOW not %eof(FItr401L);
050600090224
050700090224          // se po a blank imposto il valore massimo nel campo di confronto
050800090225         if tr4fil = *blanks;
050900090225         s_tr4fil= *hival;
051000090224         endif;
051100090224
051200090224         // Operazioni a rottura di p.o.
051300090225         if S_tr4fil<>tr4fil;
051400090224         w_esito=*off;
051500090224            // se codice p.o. non è tutto numerico è sicuramente un'anomalia
051600090225            if %check(digits:tr4fil)>0;
051700090224            w_esito=*on;
051800090224            else;
051900090224            // se codice tutto numerico verifico esistenza su azorg
052000090225            n_tr4fil = %dec(tr4fil:3:0);
052100090225            chain (n_tr4fil) azorg01l;
052200090224            if not %found or (orgfag<>'A' and orgfag<>'F' or (orgfva<>' '));
052300090224            w_esito=*on;
052400090224            else;
052500090224            // se trovato p.o. mi posiziono sul prossimo
052600090225            setgt (tr4fil) fitr401l;
052700090224         read         fitr401l;
052800090224            iter;
052900090224            endif;
053000090224            endif;
053100090225         S_tr4fil=tr4fil;
053200090224         endif;
053300090224
053400090224         // Elaborazione singolo record
053500090224         if w_esito=*on;
053600090224         w_anomp=*on;
053700090224         endif;
053800090224
053900090224           Read    FItr401l;
054000090224
054100090224         ENDDO;
054200090224
054300090224         // Se riscontrata almeno un'anomalia preparo e_mail
054400090224         if w_anomp;
054500090224         exsr sr_Prep_eMail;
054600090224         // Operazioni finali di chiusura file
054700090224         exsr RoutEnd;
054800090224         endif;
054900090224
055000090224         w_anomp=*off;
055100090224
055200090224       ENDSR;
055300071212
055400071212       //--------------------------------------------------------------
055500080512       //?Preparazione e-mail
055600071212       //--------------------------------------------------------------
055700071219       BEGSR sr_Prep_eMail;
055800071212
055900071221       // Override al file di stampa ed apertura dello stesso
056000071221         if not %open(PRTEMAIL);
056100071221           exsr sr_OpenPrtF;
056200071221         endif;
056300071212
056400071212       ENDSR;
056500071221
056600071221       //--------------------------------------------------------------
056700071221       //?Override al file di stampa per impostarvi i dati per
056800071221       //?  l'invio via e-mail   +   stampa inizio e-mail
056900071221       //--------------------------------------------------------------
057000071221       BEGSR sr_OpenPrtF;
057100071221
057200071221         // Override al file di stampa per impostarvi i dati per
057300071221         // l'invio via e-mail
057400071221           exsr sr_Override;
057500071221
057600071221           open PrtEMAIL;
057700080519
057800080519        // distinguo le mail 1 per ogni file che legge
057900080519           select;
058000080519
058100080519        // Mail per BartMag
058200080519         when w_anom;
058300071221         // Stampa una testata se NON è richiesta la e-mail
058400071221           IF  §MRAdreg =  *blank;
058500071221             O_testo = JobUser + ' - ' + SDSpgm
058600071221                     + ' - ' + %editc( *date : 'Y' )
058700071221                     + ' - *REM* ' + %subst(§CM1var : 7 : 70);
058800071221             except PrtDet;
058900071221             clear O_testo;
059000071221             except PrtDet;
059100071221             except PrtDet;
059200071221           ENDIF;
059300071221         // Stampa testo iniziale
059400080519           O_testo = 'Riscontrate anomalie in ricezione spunte da +
059500080508                      pistole radio. Per visualizzarle eseguire "Interro+
059600080508                     gazione Log spunte da PDA"';
059700080519
059800080519        // Mail per BartAut Ritiri
059900080519         when w_anomo;
060000080519         // Stampa una testata se NON è richiesta la e-mail
060100080519           IF  o_§MRAdreg =  *blank;
060200080519             O_testo = JobUser + ' - ' + SDSpgm
060300080519                     + ' - ' + %editc( *date : 'Y' )
060400080519                     + ' - *REM* ' + %subst(§CM1var : 7 : 70);
060500080519             except PrtDet;
060600080519             clear O_testo;
060700080519             except PrtDet;
060800080519             except PrtDet;
060900080519           ENDIF;
061000080519           O_testo = 'Riscontrate anomalie in ricezione dati da +
061100080519                      PDA Ritiri. Per visualizzarle eseguire "Interro+
061200080519                      gazione Log PDA"';
061300080519
061400080519        // Mail per BartAut Consegne
061500080519         when w_anomc;
061600080519         // Stampa una testata se NON è richiesta la e-mail
061700080519           IF  c_§MRAdreg =  *blank;
061800080519             O_testo = JobUser + ' - ' + SDSpgm
061900080519                     + ' - ' + %editc( *date : 'Y' )
062000080519                     + ' - *REM* ' + %subst(§CM1var : 7 : 70);
062100080519             except PrtDet;
062200080519             clear O_testo;
062300080519           ENDIF;
062400140916             clear O_testo;
062500140916             except PrtDet;
062600140916             zz = 1;
062700140916              FOR zz by 1 to sk;
062800140916               IF  skanom(zz) <> *blanks;
062900140916                 O_testo = skanom(zz);
063000140916                 except PrtDet;
063100140916               ENDIF;
063200140916              ENDFOR;
063300140916             clear O_testo;
063400140916             except PrtDet;
063500140916             except PrtDet;
063600080519           O_testo = 'Riscontrate anomalie in ricezione dati da +
063700140916                      PDA Consegne. Filiale assente o errata "Interro+
063800080519                      gazione Log PDA"';
063900090225
064000090225        // Mail per Positrova
064100090225         when w_anomp;
064200090225         // Stampa una testata se NON è richiesta la e-mail
064300090225           IF  §MRAdreg =  *blank;
064400090225             O_testo = JobUser + ' - ' + SDSpgm
064500090225                     + ' - ' + %editc( *date : 'Y' )
064600090225                     + ' - *REM* ' + %subst(§CM1var : 7 : 70);
064700090225             except PrtDet;
064800090225             clear O_testo;
064900090225             except PrtDet;
065000090225             except PrtDet;
065100090225           ENDIF;
065200090225         // Stampa testo iniziale
065300090225           O_testo = 'Riscontrate anomalie in ricezione spunte da +
065400090225                      Positrova. Verificare contenuto del file FITR4+
065500090225                     00F';
065600080519
065700080519           endsl;
065800080519
065900071221           except PrtDet;
066000071221
066100071221         // Stampa una riga vuota
066200071221           clear O_testo;
066300071221           except PrtDet;
066400071221
066500071221       ENDSR;
066600071219
066700071219       //--------------------------------------------------------------
066800071219       //?Override al file di stampa per impostarvi i dati per
066900071219       //?  l'invio via e-mail   +   stampa inizio e-mail
067000071219       //--------------------------------------------------------------
067100071219       BEGSR sr_Override;
067200071219
067300071220         reset $Invio;
067400071219         reset TRTCM1ds;
067500080519
067600080519        // distinguo le mail 1 per ogni file che legge
067700080519        select;
067800080519
067900080519        // Mail per BartMag
068000080519        when w_anom;
068100071221         IF  §MRAdreg <> *blank;
068200080317           §CM1mitt = %trim(§MRAdmitt);
068300071221           §CM1dst  = %trim(§MRAddest);
068400071221           §CM1tips = §MRAdreg;
068500071227           §CM1po   = C_Sede;
068600071221           §CM1var  = '*OBJM*' + §MRAddes;
068700071221           §CM1idp  = §MRAdidpro;
068800071221           Qcmd = C_CmdOvrPrtF
068900071221                + ' outq(' + %trim(§MRAdoutqi) + ')'
069000071221                + ' usrdfndta(''' + TRTCM1ds + ''')';
069100071221         ELSE;
069200071221           Qcmd = C_CmdOvrPrtF;
069300071221         ENDIF;
069400071220
069500071219         callp(e) qCmdExc(Qcmd : %size(Qcmd));
069600071221         IF  §MRAdreg <> *blanks  and  not %error;
069700071221           $Invio = *on;
069800071221         ENDIF;
069900080519
070000080519        // Mail per BartAut Ritiri
070100080519        when w_anomo;
070200080519         IF  o_§MRAdreg <> *blank;
070300080519           §CM1mitt = %trim(o_§MRAdmitt);
070400080519           §CM1dst  = %trim(o_§MRAddest);
070500080519           §CM1tips = o_§MRAdreg;
070600080519           §CM1po   = C_Sede;
070700080519           §CM1var  = '*OBJM*' + o_§MRAddes;
070800080519           §CM1idp  = o_§MRAdidpro;
070900080519           Qcmd = C_CmdOvrPrtF
071000080519                + ' outq(' + %trim(o_§MRAdoutqi) + ')'
071100080519                + ' usrdfndta(''' + TRTCM1ds + ''')';
071200080519         ELSE;
071300080519           Qcmd = C_CmdOvrPrtF;
071400080519         ENDIF;
071500080519
071600080519         callp(e) qCmdExc(Qcmd : %size(Qcmd));
071700080519         IF  o_§MRAdreg <> *blanks  and  not %error;
071800080519           $Invio = *on;
071900080519         ENDIF;
072000080519
072100080519        // Mail per BartAut Consegne
072200080519        when w_anomc;
072300080519         IF  c_§MRAdreg <> *blank;
072400080519           §CM1mitt = %trim(c_§MRAdmitt);
072500080519           §CM1dst  = %trim(c_§MRAddest);
072600080519           §CM1tips = c_§MRAdreg;
072700080519           §CM1po   = C_Sede;
072800080519           §CM1var  = '*OBJM*' + c_§MRAddes;
072900080519           §CM1idp  = c_§MRAdidpro;
073000080519           Qcmd = C_CmdOvrPrtF
073100080519                + ' outq(' + %trim(c_§MRAdoutqi) + ')'
073200080519                + ' usrdfndta(''' + TRTCM1ds + ''')';
073300080519         ELSE;
073400080519           Qcmd = C_CmdOvrPrtF;
073500080519         ENDIF;
073600080519
073700080519         callp(e) qCmdExc(Qcmd : %size(Qcmd));
073800080519         IF  c_§MRAdreg <> *blanks  and  not %error;
073900080519           $Invio = *on;
074000080519         ENDIF;
074100090225
074200090225        // Mail per Positrova
074300090225        when w_anomp;
074400090225         IF  p_§MRAdreg <> *blank;
074500090225           §CM1mitt = %trim(p_§MRAdmitt);
074600090225           §CM1dst  = %trim(p_§MRAddest);
074700090225           §CM1tips = p_§MRAdreg;
074800090225           §CM1po   = C_Sede;
074900090225           §CM1var  = '*OBJM*' + p_§MRAddes;
075000090225           §CM1idp  = p_§MRAdidpro;
075100090225           Qcmd = C_CmdOvrPrtF
075200090225                + ' outq(' + %trim(p_§MRAdoutqi) + ')'
075300090225                + ' usrdfndta(''' + TRTCM1ds + ''')';
075400090225         ELSE;
075500090225           Qcmd = C_CmdOvrPrtF;
075600090225         ENDIF;
075700090225
075800090225         callp(e) qCmdExc(Qcmd : %size(Qcmd));
075900090225         IF  p_§MRAdreg <> *blanks  and  not %error;
076000090225           $Invio = *on;
076100090225         ENDIF;
076200080519
076300080519        endsl;
076400071219
076500071219       ENDSR;
076600071221
076700071221       //--------------------------------------------------------------
076800071221       //?Operazioni finali.
076900071221       //--------------------------------------------------------------
077000071221       BEGSR RoutEnd;
077100071221
077200071221         if %open(PrtEMAIL);
077300071221
077400071221         //?Chiusura dello spool?
077500080522           O_testo = sdspgm;
077600080522           except PrtDet;
077700071221           clear O_testo;
077800071221           except PrtDet;
077900071221           O_testo = '***   Fine Lista   ***';
078000071221           except PrtDet;
078100071221
078200071221           close PrtEMAIL;
078300071221
078400071221         //?Eliminazione overflow?
078500071221           Qcmd = C_CmdDltOvr;
078600071221           callp(e) qCmdExc(Qcmd : %size(Qcmd));
078700071221
078800071221         endif;
078900071221
079000071221       ENDSR;
079100071212
079200071212      /end-free
079300071219
079400071219      //---------------------------------------------------------------
079500071219      //?Spool di stampa (per e-mail).
079600071219      //---------------------------------------------------------------
079700071219     oPrtEMAIL  e            PRTdet      1
079800071219     o                       O_testo
079900071213
