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
012400071218
012500071212      //---------------------------------------------------------------
012600071212      //?Definizione variabili globali.
012700071212      //---------------------------------------------------------------
012800071219
012900071212      // - Flags booleani
013000071213     d W_Esito         s              1n   inz(*off)
013100071219     d $Invio          s              1n   inz(*off)
013200080512     d W_anom          s              1n   inz(*off)
013300080519     d w_anomo         s              1n   inz(*off)
013400080519     d w_anomc         s              1n   inz(*off)
013500090225     d w_anomp         s              1n   inz(*off)
013600071219
013700071219      // - Campi per QCMDEXC
013800071219     d Qcmd            s            512    inz
013900071219
014000080508      // - Campi di comodo
014100080508     d N_psppo         s              3  0 inz
014200080519     d s_psppo         s                   like(psppo)
014300080519     d prcfil          s              3
014400080519     d num_prcfil      s                   like(orgfil)
014500080520     d prdfil          s              3
014600080520     d num_prdfil      s                   like(orgfil)
014700090225     d N_tr4fil        s              3  0 inz
014800090225     d s_tr4fil        s                   like(tr4fil)
014900071219
015000071218      // - Campi di stampa
015100071219     d O_testo         s            132    inz
015200071212
015300071212      //---------------------------------------------------------------
015400071213      //?Definizione procedure esterne.
015500071212      //---------------------------------------------------------------
015600071219
015700071219      // - Esecuzione comando di sistema
015800071219     d qCmdExc         pr                  extpgm('QCMDEXC')
015900071219     d  Qcmd                        512    const  options(*varsize)
016000071219     d  Qlen                         15  5 const
016100071221
016200071221      // - Gestione tabelle
016300071221     d tibs02r         pr                  extpgm('TIBS02R')
016400071221     d  kpjba                              likeds(KPJBA)    const
016500071221     d  tibs02ds                           likeds(TIBS02ds)
016600071212
016700071212      //---------------------------------------------------------------
016800071212      //?Definizione key-list.
016900071212      //---------------------------------------------------------------
017000071212
017100071212      //---------------------------------------------------------------
017200071212      //?Main.
017300071212      //---------------------------------------------------------------
017400071212     c     *Entry        plist
017500071221     c                   parm                    KPJBA
017600071212
017700071212      /free
017800071212
017900071212       // Operazioni iniziali
018000080519       exsr RoutInz;
018100071212
018200080508       //
018300080508       exsr Elab_FIPSP;
018400080519       exsr Elab_FIPRC;
018500080520       exsr Elab_FIPRD;
018600090224       exsr Elab_FITR4;
018700071212
018800071212       //--------------------------------------------------------------
018900071212       //?Operazioni iniziali.
019000071212       //--------------------------------------------------------------
019100071212       BEGSR RoutInz;
019200071219
019300071219         *inLR = *on;
019400071221
019500071221       // Reperimento tab. "MRA"
019600071221         clear dMRAdan;
019700071221         clear kpjbu;
019800071221         reset tibs02ds;
019900071221         T02ke1 = SDSpgm;
020000071221         tibs02r(kpjba:tibs02ds);
020100071221         if T02err = *blank;
020200071221           dMRAdan = T02uni;
020300071221         endif;
020400080519
020500080519       // Reperimento tab. "MRA" --> RITIRI
020600080519         clear dMRAdanO;
020700080519         clear kpjbu;
020800080519         reset tibs02ds;
020900080519         T02ke1 = SDSpgm;
021000080519         T02ke2 = 'RITIRI';
021100080519         tibs02r(kpjba:tibs02ds);
021200080519         if T02err = *blank;
021300080519           dMRAdanO = T02uni;
021400080519         endif;
021500080519
021600080519       // Reperimento tab. "MRA" --> CONSEGNE
021700080519         clear dMRAdanC;
021800080519         clear kpjbu;
021900080519         reset tibs02ds;
022000080519         T02ke1 = SDSpgm;
022100080519         T02ke2 = 'CONSEGNE';
022200080519         tibs02r(kpjba:tibs02ds);
022300080519         if T02err = *blank;
022400080519           dMRAdanC = T02uni;
022500080519         endif;
022600090224
022700090224       // Reperimento tab. "MRA" --> Positrova
022800090224         clear dMRAdanP;
022900090224         clear kpjbu;
023000090224         reset tibs02ds;
023100090224         T02ke1 = SDSpgm;
023200090224         T02ke2 = 'POSITROVA';
023300090224         tibs02r(kpjba:tibs02ds);
023400090224         if T02err = *blank;
023500090224           dMRAdanP = T02uni;
023600090224         endif;
023700071212
023800071212       ENDSR;
023900071212
024000071212       //--------------------------------------------------------------
024100080519       //?Elaborazione file ritorno dati da BartMag
024200071212       //--------------------------------------------------------------
024300080508       BEGSR Elab_FIPSP;
024400071212
024500080508         s_psppo='   ';
024600080512         w_anom=*off;
024700080519
024800080508       //?Ciclo di lettura tab. FIPSP?
024900071212
025000080519         setll *loval fipsp01l;
025100080519         read         fipsp01l;
025200071212
025300080508         DOW not %eof(FIPSP01L);
025400080519
025500080519          // se po a blank imposto il valore massimo nel campo di confronto
025600080519         if psppo = *blanks;
025700080519         s_psppo = *hival;
025800080519         endif;
025900071212
026000080508         // Operazioni a rottura di p.o.
026100080508         if S_psppo<>psppo;
026200080508         w_esito=*off;
026300080508            // se codice p.o. non è tutto numerico è sicuramente un'anomalia
026400080508            if %check(digits:psppo)>0;
026500080508            w_esito=*on;
026600080508            else;
026700080508            // se codice tutto numerico verifico esistenza su azorg
026800080519            n_psppo = %dec(psppo:3:0);
026900080508            chain (n_psppo) azorg01l;
027000080508            if not %found or (orgfag<>'A' and orgfag<>'F' or (orgfva<>' '));
027100080508            w_esito=*on;
027200080508            else;
027300080508            // se trovato p.o. mi posiziono sul prossimo
027400080508            setgt (psppo) fipsp01l;
027500080519         read         fipsp01l;
027600080508            iter;
027700080508            endif;
027800080508            endif;
027900080508         S_psppo=psppo;
028000080508         endif;
028100080508
028200080508         // Elaborazione singolo record
028300080508         if w_esito=*on;
028400080519         chain(e) fipspnrri fipsp00f;
028500080508         if not(%error);
028600080512         w_anom=*on;
028700080508         pslprfc=pspprfc;
028800080508         psliddisp=pspiddisp;
028900080508         fipslds=pspdati;
029000080508         psldatora=pspdatora;
029100080508         pslerr='E';
029200080508         pslfil=psppo;
029300080508         write FIPSL000;
029400080519         delete fipsp000;
029500080508         endif;
029600080508         endif;
029700071212
029800080519           Read    FIPSP01l;
029900071212
030000071212         ENDDO;
030100071212
030200080512         // Se riscontrata almeno un'anomalia preparo e_mail
030300080519         if w_anom;
030400080512         exsr sr_Prep_eMail;
030500080519         // Operazioni finali di chiusura file
030600080519         exsr RoutEnd;
030700080512         endif;
030800080519
030900080519         w_anom=*off;
031000080519
031100071212       ENDSR;
031200080519
031300080519       //--------------------------------------------------------------
031400080519       //?Elaborazione file ritorno dati da BartAut RITIRI
031500080519       //--------------------------------------------------------------
031600080519       begsr Elab_FIPRC;
031700080519
031800080519         clear prcfil;
031900080519         w_anomo = *off;
032000080519
032100080519       //?Ciclo di lettura file FIPRC?
032200080519
032300080519         setll *start fiprc00r;
032400080519         read         fiprc00r;
032500080519
032600080519         dow not %eof(fiprc00r);
032700080519
032800080519          // Imposto la filiale dall'utente
032900080519          prcfil = %subst(prcprfc:3:3);
033000080519
033100080519            // se la filiale non è tutta numerica è sicuramente un'anomalia
033200080519            if %check(digits:prcfil) > 0;
033300080519             // scrivo il record su fiplg00f
033400080519             clear fiplg000;
033500080519             plgerr = 'E';
033600080519             plgmsg = 'FILIALE ERRATA';
033700080519             plgtiporcd = prctiporcd;
033800080519             plgprfc = prcprfc;
033900080519             plgiddisp = prciddisp;
034000080519             plgdatora = prcdatora;
034100080519             plgdati = prcdati;
034200080519             write fiplg000;
034300080519             // cancello il record letto su fiprc
034400080519             delete fiprc000;
034500080519             // imposto flag anomalia
034600080519             w_anomo = *on;
034700080519
034800080519            else;
034900080519
035000080519            // se la filiale è tutta numerica verifico esistenza su azorg
035100080519            num_prcfil = %dec(prcfil:3:0);
035200080519            chain (num_prcfil) azorg01l;
035300080519            // filiale non valida
035400080519             if not %found or (orgfag<>'A' and orgfag<>'F' or (orgfva<>' '));
035500080519              // scrivo il record su fiplg00f
035600080519              clear fiplg000;
035700080519              plgerr = 'E';
035800080519              plgmsg = 'FILIALE ERRATA';
035900080519              plgtiporcd = prctiporcd;
036000080519              plgprfc = prcprfc;
036100080519              plgiddisp = prciddisp;
036200080519              plgdatora = prcdatora;
036300080519              plgdati = prcdati;
036400080519              write fiplg000;
036500080519              // cancello il record letto su fiprc
036600080519              delete fiprc000;
036700080519              // imposto flag anomalia
036800080519              w_anomo = *on;
036900080519             endif;
037000080519
037100080519            endif;
037200080519
037300080519         read fiprc00r;
037400080519
037500080519         enddo;
037600080519
037700080519         // Se riscontrata almeno un'anomalia preparo e_mail
037800080519         if w_anomo;
037900080519         exsr sr_Prep_eMail;
038000080519         // Operazioni finali di chiusura file
038100080519         exsr RoutEnd;
038200080519         endif;
038300080519
038400080519         w_anomo = *off;
038500080519
038600080519       endsr;
038700080520
038800080520       //--------------------------------------------------------------
038900080520       //?Elaborazione file ritorno dati da BartAut CONSEGNE
039000080520       //--------------------------------------------------------------
039100080520       begsr Elab_FIPRd;
039200080520
039300080520         clear prdfil;
039400080520         w_anomc = *off;
039500080520
039600080520       //?Ciclo di lettura file FIPRD?
039700080520
039800080520         setll *start fiprd00r;
039900080520         read         fiprd00r;
040000080520
040100080520         dow not %eof(fiprd00r);
040200080520
040300080520          // Imposto la filiale dall'utente
040400080520          prdfil = %subst(prdprfc:3:3);
040500080520
040600080520            // se la filiale non è tutta numerica è sicuramente un'anomalia
040700080520            if %check(digits:prdfil) > 0;
040800080520             // scrivo il record su fiplg00f
040900080520             clear fiplg000;
041000080520             plgerr = 'E';
041100080520             plgmsg = 'FILIALE ERRATA';
041200080520             plgtiporcd = prdtiporcd;
041300080520             plgprfc = prdprfc;
041400080520             plgiddisp = prdiddisp;
041500080520             plgdatora = prddatora;
041600080520             plgdati = prddati;
041700080520             write fiplg000;
041800080520             // cancello il record letto su fiprd
041900080520             delete fiprd000;
042000080520             // imposto flag anomalia
042100080520             w_anomc = *on;
042200080520
042300080520            else;
042400080520
042500080520            // se la filiale è tutta numerica verifico esistenza su azorg
042600080520            num_prdfil = %dec(prdfil:3:0);
042700080520            chain (num_prdfil) azorg01l;
042800080520            // filiale non valida
042900080520             if not %found or (orgfag<>'A' and orgfag<>'F' or (orgfva<>' '));
043000080520              // scrivo il record su fiplg00f
043100080520              clear fiplg000;
043200080520              plgerr = 'E';
043300080520              plgmsg = 'FILIALE ERRATA';
043400080520              plgtiporcd = prdtiporcd;
043500080520              plgprfc = prdprfc;
043600080520              plgiddisp = prdiddisp;
043700080520              plgdatora = prddatora;
043800080520              plgdati = prddati;
043900080520              write fiplg000;
044000080520              // cancello il record letto su fiprd
044100080520              delete fiprd000;
044200080520              // imposto flag anomalia
044300080520              w_anomc = *on;
044400080520             endif;
044500080520
044600080520            endif;
044700080520
044800080520         read fiprd00r;
044900080520
045000080520         enddo;
045100080520
045200080520         // Se riscontrata almeno un'anomalia preparo e_mail
045300080520         if w_anomc;
045400080520         exsr sr_Prep_eMail;
045500080520         // Operazioni finali di chiusura file
045600080520         exsr RoutEnd;
045700080520         endif;
045800080520
045900080520         w_anomc = *off;
046000080520
046100080520       endsr;
046200090224
046300090224       //--------------------------------------------------------------
046400090224       //?Elaborazione file ritorno dati da Positrova
046500090224       //--------------------------------------------------------------
046600090224       BEGSR Elab_FITR4;
046700090224
046800090225         s_tr4fil='   ';
046900090224         w_anomp=*off;
047000090224
047100090224       //?Ciclo di lettura tab. FITR4?
047200090224
047300090224         setll *loval fitr401l;
047400090224         read         fitr401l;
047500090224
047600090224         DOW not %eof(FItr401L);
047700090224
047800090224          // se po a blank imposto il valore massimo nel campo di confronto
047900090225         if tr4fil = *blanks;
048000090225         s_tr4fil= *hival;
048100090224         endif;
048200090224
048300090224         // Operazioni a rottura di p.o.
048400090225         if S_tr4fil<>tr4fil;
048500090224         w_esito=*off;
048600090224            // se codice p.o. non è tutto numerico è sicuramente un'anomalia
048700090225            if %check(digits:tr4fil)>0;
048800090224            w_esito=*on;
048900090224            else;
049000090224            // se codice tutto numerico verifico esistenza su azorg
049100090225            n_tr4fil = %dec(tr4fil:3:0);
049200090225            chain (n_tr4fil) azorg01l;
049300090224            if not %found or (orgfag<>'A' and orgfag<>'F' or (orgfva<>' '));
049400090224            w_esito=*on;
049500090224            else;
049600090224            // se trovato p.o. mi posiziono sul prossimo
049700090225            setgt (tr4fil) fitr401l;
049800090224         read         fitr401l;
049900090224            iter;
050000090224            endif;
050100090224            endif;
050200090225         S_tr4fil=tr4fil;
050300090224         endif;
050400090224
050500090224         // Elaborazione singolo record
050600090224         if w_esito=*on;
050700090224         w_anomp=*on;
050800090224         endif;
050900090224
051000090224           Read    FItr401l;
051100090224
051200090224         ENDDO;
051300090224
051400090224         // Se riscontrata almeno un'anomalia preparo e_mail
051500090224         if w_anomp;
051600090224         exsr sr_Prep_eMail;
051700090224         // Operazioni finali di chiusura file
051800090224         exsr RoutEnd;
051900090224         endif;
052000090224
052100090224         w_anomp=*off;
052200090224
052300090224       ENDSR;
052400071212
052500071212       //--------------------------------------------------------------
052600080512       //?Preparazione e-mail
052700071212       //--------------------------------------------------------------
052800071219       BEGSR sr_Prep_eMail;
052900071212
053000071221       // Override al file di stampa ed apertura dello stesso
053100071221         if not %open(PRTEMAIL);
053200071221           exsr sr_OpenPrtF;
053300071221         endif;
053400071212
053500071212       ENDSR;
053600071221
053700071221       //--------------------------------------------------------------
053800071221       //?Override al file di stampa per impostarvi i dati per
053900071221       //?  l'invio via e-mail   +   stampa inizio e-mail
054000071221       //--------------------------------------------------------------
054100071221       BEGSR sr_OpenPrtF;
054200071221
054300071221         // Override al file di stampa per impostarvi i dati per
054400071221         // l'invio via e-mail
054500071221           exsr sr_Override;
054600071221
054700071221           open PrtEMAIL;
054800080519
054900080519        // distinguo le mail 1 per ogni file che legge
055000080519           select;
055100080519
055200080519        // Mail per BartMag
055300080519         when w_anom;
055400071221         // Stampa una testata se NON è richiesta la e-mail
055500071221           IF  §MRAdreg =  *blank;
055600071221             O_testo = JobUser + ' - ' + SDSpgm
055700071221                     + ' - ' + %editc( *date : 'Y' )
055800071221                     + ' - *REM* ' + %subst(§CM1var : 7 : 70);
055900071221             except PrtDet;
056000071221             clear O_testo;
056100071221             except PrtDet;
056200071221             except PrtDet;
056300071221           ENDIF;
056400071221         // Stampa testo iniziale
056500080519           O_testo = 'Riscontrate anomalie in ricezione spunte da +
056600080508                      pistole radio. Per visualizzarle eseguire "Interro+
056700080508                     gazione Log spunte da PDA"';
056800080519
056900080519        // Mail per BartAut Ritiri
057000080519         when w_anomo;
057100080519         // Stampa una testata se NON è richiesta la e-mail
057200080519           IF  o_§MRAdreg =  *blank;
057300080519             O_testo = JobUser + ' - ' + SDSpgm
057400080519                     + ' - ' + %editc( *date : 'Y' )
057500080519                     + ' - *REM* ' + %subst(§CM1var : 7 : 70);
057600080519             except PrtDet;
057700080519             clear O_testo;
057800080519             except PrtDet;
057900080519             except PrtDet;
058000080519           ENDIF;
058100080519           O_testo = 'Riscontrate anomalie in ricezione dati da +
058200080519                      PDA Ritiri. Per visualizzarle eseguire "Interro+
058300080519                      gazione Log PDA"';
058400080519
058500080519        // Mail per BartAut Consegne
058600080519         when w_anomc;
058700080519         // Stampa una testata se NON è richiesta la e-mail
058800080519           IF  c_§MRAdreg =  *blank;
058900080519             O_testo = JobUser + ' - ' + SDSpgm
059000080519                     + ' - ' + %editc( *date : 'Y' )
059100080519                     + ' - *REM* ' + %subst(§CM1var : 7 : 70);
059200080519             except PrtDet;
059300080519             clear O_testo;
059400080519             except PrtDet;
059500080519             except PrtDet;
059600080519           ENDIF;
059700080519           O_testo = 'Riscontrate anomalie in ricezione dati da +
059800080519                      PDA Consegne. Per visualizzarle eseguire "Interro+
059900080519                      gazione Log PDA"';
060000090225
060100090225        // Mail per Positrova
060200090225         when w_anomp;
060300090225         // Stampa una testata se NON è richiesta la e-mail
060400090225           IF  §MRAdreg =  *blank;
060500090225             O_testo = JobUser + ' - ' + SDSpgm
060600090225                     + ' - ' + %editc( *date : 'Y' )
060700090225                     + ' - *REM* ' + %subst(§CM1var : 7 : 70);
060800090225             except PrtDet;
060900090225             clear O_testo;
061000090225             except PrtDet;
061100090225             except PrtDet;
061200090225           ENDIF;
061300090225         // Stampa testo iniziale
061400090225           O_testo = 'Riscontrate anomalie in ricezione spunte da +
061500090225                      Positrova. Verificare contenuto del file FITR4+
061600090225                     00F';
061700080519
061800080519           endsl;
061900080519
062000071221           except PrtDet;
062100071221
062200071221         // Stampa una riga vuota
062300071221           clear O_testo;
062400071221           except PrtDet;
062500071221
062600071221       ENDSR;
062700071219
062800071219       //--------------------------------------------------------------
062900071219       //?Override al file di stampa per impostarvi i dati per
063000071219       //?  l'invio via e-mail   +   stampa inizio e-mail
063100071219       //--------------------------------------------------------------
063200071219       BEGSR sr_Override;
063300071219
063400071220         reset $Invio;
063500071219         reset TRTCM1ds;
063600080519
063700080519        // distinguo le mail 1 per ogni file che legge
063800080519        select;
063900080519
064000080519        // Mail per BartMag
064100080519        when w_anom;
064200071221         IF  §MRAdreg <> *blank;
064300080317           §CM1mitt = %trim(§MRAdmitt);
064400071221           §CM1dst  = %trim(§MRAddest);
064500071221           §CM1tips = §MRAdreg;
064600071227           §CM1po   = C_Sede;
064700071221           §CM1var  = '*OBJM*' + §MRAddes;
064800071221           §CM1idp  = §MRAdidpro;
064900071221           Qcmd = C_CmdOvrPrtF
065000071221                + ' outq(' + %trim(§MRAdoutqi) + ')'
065100071221                + ' usrdfndta(''' + TRTCM1ds + ''')';
065200071221         ELSE;
065300071221           Qcmd = C_CmdOvrPrtF;
065400071221         ENDIF;
065500071220
065600071219         callp(e) qCmdExc(Qcmd : %size(Qcmd));
065700071221         IF  §MRAdreg <> *blanks  and  not %error;
065800071221           $Invio = *on;
065900071221         ENDIF;
066000080519
066100080519        // Mail per BartAut Ritiri
066200080519        when w_anomo;
066300080519         IF  o_§MRAdreg <> *blank;
066400080519           §CM1mitt = %trim(o_§MRAdmitt);
066500080519           §CM1dst  = %trim(o_§MRAddest);
066600080519           §CM1tips = o_§MRAdreg;
066700080519           §CM1po   = C_Sede;
066800080519           §CM1var  = '*OBJM*' + o_§MRAddes;
066900080519           §CM1idp  = o_§MRAdidpro;
067000080519           Qcmd = C_CmdOvrPrtF
067100080519                + ' outq(' + %trim(o_§MRAdoutqi) + ')'
067200080519                + ' usrdfndta(''' + TRTCM1ds + ''')';
067300080519         ELSE;
067400080519           Qcmd = C_CmdOvrPrtF;
067500080519         ENDIF;
067600080519
067700080519         callp(e) qCmdExc(Qcmd : %size(Qcmd));
067800080519         IF  o_§MRAdreg <> *blanks  and  not %error;
067900080519           $Invio = *on;
068000080519         ENDIF;
068100080519
068200080519        // Mail per BartAut Consegne
068300080519        when w_anomc;
068400080519         IF  c_§MRAdreg <> *blank;
068500080519           §CM1mitt = %trim(c_§MRAdmitt);
068600080519           §CM1dst  = %trim(c_§MRAddest);
068700080519           §CM1tips = c_§MRAdreg;
068800080519           §CM1po   = C_Sede;
068900080519           §CM1var  = '*OBJM*' + c_§MRAddes;
069000080519           §CM1idp  = c_§MRAdidpro;
069100080519           Qcmd = C_CmdOvrPrtF
069200080519                + ' outq(' + %trim(c_§MRAdoutqi) + ')'
069300080519                + ' usrdfndta(''' + TRTCM1ds + ''')';
069400080519         ELSE;
069500080519           Qcmd = C_CmdOvrPrtF;
069600080519         ENDIF;
069700080519
069800080519         callp(e) qCmdExc(Qcmd : %size(Qcmd));
069900080519         IF  c_§MRAdreg <> *blanks  and  not %error;
070000080519           $Invio = *on;
070100080519         ENDIF;
070200090225
070300090225        // Mail per Positrova
070400090225        when w_anomp;
070500090225         IF  p_§MRAdreg <> *blank;
070600090225           §CM1mitt = %trim(p_§MRAdmitt);
070700090225           §CM1dst  = %trim(p_§MRAddest);
070800090225           §CM1tips = p_§MRAdreg;
070900090225           §CM1po   = C_Sede;
071000090225           §CM1var  = '*OBJM*' + p_§MRAddes;
071100090225           §CM1idp  = p_§MRAdidpro;
071200090225           Qcmd = C_CmdOvrPrtF
071300090225                + ' outq(' + %trim(p_§MRAdoutqi) + ')'
071400090225                + ' usrdfndta(''' + TRTCM1ds + ''')';
071500090225         ELSE;
071600090225           Qcmd = C_CmdOvrPrtF;
071700090225         ENDIF;
071800090225
071900090225         callp(e) qCmdExc(Qcmd : %size(Qcmd));
072000090225         IF  p_§MRAdreg <> *blanks  and  not %error;
072100090225           $Invio = *on;
072200090225         ENDIF;
072300080519
072400080519        endsl;
072500071219
072600071219       ENDSR;
072700071221
072800071221       //--------------------------------------------------------------
072900071221       //?Operazioni finali.
073000071221       //--------------------------------------------------------------
073100071221       BEGSR RoutEnd;
073200071221
073300071221         if %open(PrtEMAIL);
073400071221
073500071221         //?Chiusura dello spool?
073600080522           O_testo = sdspgm;
073700080522           except PrtDet;
073800071221           clear O_testo;
073900071221           except PrtDet;
074000071221           O_testo = '***   Fine Lista   ***';
074100071221           except PrtDet;
074200071221
074300071221           close PrtEMAIL;
074400071221
074500071221         //?Eliminazione overflow?
074600071221           Qcmd = C_CmdDltOvr;
074700071221           callp(e) qCmdExc(Qcmd : %size(Qcmd));
074800071221
074900071221         endif;
075000071221
075100071221       ENDSR;
075200071212
075300071212      /end-free
075400071219
075500071219      //---------------------------------------------------------------
075600071219      //?Spool di stampa (per e-mail).
075700071219      //---------------------------------------------------------------
075800071219     oPrtEMAIL  e            PRTdet      1
075900071219     o                       O_testo
076000071213
